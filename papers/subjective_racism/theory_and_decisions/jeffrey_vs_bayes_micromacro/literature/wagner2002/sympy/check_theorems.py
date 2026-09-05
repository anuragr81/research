import sympy as sp
R = sp.Rational

# Prior on E x F, 2x2, kept fully generic (full support => Wagner's (4.3)-(4.4) hold)
a,b,c = sp.symbols('alpha beta c', positive=True)
p = {(1,1):a*b+c, (1,0):a*(1-b)-c, (0,1):(1-a)*b-c, (0,0):(1-a)*(1-b)+c}

def kin_E(P, e1):
    pE1 = P[(1,1)]+P[(1,0)]; pE0 = P[(0,1)]+P[(0,0)]
    return {(1,1):P[(1,1)]/pE1*e1, (1,0):P[(1,0)]/pE1*e1,
            (0,1):P[(0,1)]/pE0*(1-e1), (0,0):P[(0,0)]/pE0*(1-e1)}

def kin_F(P, f1):
    pF1 = P[(1,1)]+P[(0,1)]; pF0 = P[(1,0)]+P[(0,0)]
    return {(1,1):P[(1,1)]/pF1*f1, (0,1):P[(0,1)]/pF1*f1,
            (1,0):P[(1,0)]/pF0*(1-f1), (0,0):P[(0,0)]/pF0*(1-f1)}

def margE(P): return P[(1,1)]+P[(1,0)], P[(0,1)]+P[(0,0)]
def margF(P): return P[(1,1)]+P[(0,1)], P[(1,0)]+P[(0,0)]

e1, f1, g1, h1 = sp.symbols('e1 f1 g1 h1', positive=True)

# route 1 (top-then-right of schema 3.1): p -E(e1)-> q -F(f1)-> r
q  = kin_E(p, e1)
r  = kin_F(q, f1)
# route 2 (left-then-bottom): p -F(g1)-> q' -E(h1)-> r'
qprime = kin_F(p, g1)
rprime = kin_E(qprime, h1)

print("=== Theorem 3.1 (sufficiency): Bayes-factor identities (3.2),(3.3) imply r=r' ===")
pE1,pE0 = margE(p); pF1,pF0 = margF(p)
qF1,qF0 = margF(q)          # q's own (derived) F-marginal
qpE1,qpE0 = margE(qprime)   # q''s own (derived) E-marginal

# (3.2): beta_{r',q'}(E1:E0) = beta_{q,p}(E1:E0)  =>  solve for h1
eq32 = sp.Eq((h1/(1-h1))/(qpE1/qpE0), (e1/(1-e1))/(pE1/pE0))
h1_bfc = sp.solve(eq32, h1)[0]
# (3.3): beta_{q',p}(F1:F0) = beta_{r,q}(F1:F0)  =>  solve for f1
eq33 = sp.Eq((g1/(1-g1))/(pF1/pF0), (f1/(1-f1))/(qF1/qF0))
f1_bfc = sp.solve(eq33, f1)[0]

r_bfc  = kin_F(q, f1_bfc)
rp_bfc = kin_E(qprime.copy() if hasattr(qprime,'copy') else {k:qprime[k] for k in qprime}, h1_bfc)
print("r == r' under Bayes-factor consistency, all cells:",
      all(sp.simplify(sp.together(r_bfc[k]-rp_bfc[k]))==0 for k in r_bfc))

print()
print("=== Theorem 4.1 (necessity): r=r' implies (3.2) and (3.3), full-support prior ===")
subs_prior = {a:R(3,10), b:R(11,20), c:R(1,20)}
e1_num = R(2,5)
system = [sp.Eq(r[k].subs(subs_prior).subs(e1,e1_num), rprime[k].subs(subs_prior).subs(e1,e1_num))
          for k in [(1,1),(1,0)]]
sol = sp.solve(system, [f1,h1], dict=True)[0]
f1_expr, h1_expr = sol[f1], sol[h1]

pE1n,pE0n = [x.subs(subs_prior) for x in margE(p)]
pF1n,pF0n = [x.subs(subs_prior) for x in margF(p)]
qF1n,qF0n = [x.subs(subs_prior).subs(e1,e1_num) for x in margF(q)]
qpE1n,qpE0n = [x.subs(subs_prior) for x in margE(qprime)]  # derived, function of free g1

beta_qp   = (e1_num/(1-e1_num)) / (pE1n/pE0n)
beta_rpqp = (h1_expr/(1-h1_expr)) / (qpE1n/qpE0n)
print("(3.2) residual, function of free g1 (should be 0 identically):",
      sp.simplify(sp.together(beta_rpqp-beta_qp)))

beta_qppF = (g1/(1-g1)) / (pF1n/pF0n)
beta_rqF  = (f1_expr/(1-f1_expr)) / (qF1n/qF0n)
print("(3.3) residual, function of free g1 (should be 0 identically):",
      sp.simplify(sp.together(beta_qppF-beta_rqF)))
