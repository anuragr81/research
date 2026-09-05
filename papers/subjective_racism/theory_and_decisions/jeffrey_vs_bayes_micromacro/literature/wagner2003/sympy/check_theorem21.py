import sympy as sp
R = sp.Rational

# --- Theorem 2.1: matched Bayes-factor reweighting on a general atomic algebra ---
atoms = ['w1','w2','w3','w4']
p = dict(zip(atoms, [R(1,5), R(3,10), R(1,4), R(1,4)]))
assert sum(p.values()) == 1

w_q, w_Q = sp.symbols('w_q1:5'), sp.symbols('w_Q1:5')
w_q = dict(zip(atoms, w_q)); w_Q = dict(zip(atoms, w_Q))

def reweight(P, w):
    unnorm = {k: P[k]*w[k] for k in P}
    Z = sum(unnorm.values())
    return {k: unnorm[k]/Z for k in unnorm}

q = reweight(p, w_q)
Qm = reweight(p, w_Q)
r_second = reweight(Qm, w_q)   # route 2's second step, weighted like p->q
R_second = reweight(q, w_Q)    # route 1's second step, weighted like p->Q

print("=== Theorem 2.1: matched Bayes-factor reweighting on a general 4-atom algebra")
print("    (no 2x2 product structure) => r == R ===")
diffs_sym = [sp.simplify(sp.together(r_second[k]-R_second[k])) for k in atoms]
print("symbolic, arbitrary weights, all cells zero:", all(d == 0 for d in diffs_sym))

subs_num = {w_q[k]: v for k,v in zip(atoms, [R(2,1), R(3,1), R(1,2), R(5,4)])}
subs_num.update({w_Q[k]: v for k,v in zip(atoms, [R(1,3), R(4,1), R(2,1), R(1,1)])})
print("numeric witness, all cells zero:",
      all(sp.simplify(sp.together((r_second[k]-R_second[k]).subs(subs_num))) == 0 for k in atoms))

print()
print("=== Remark 2.2's explicit formula (2.7): r(A) = pi_qp(A) pi_Qp(A) p(A) / Z ===")
pi_qp = {k: q[k]/p[k] for k in atoms}
pi_Qp = {k: Qm[k]/p[k] for k in atoms}
direct = {k: pi_qp[k]*pi_Qp[k]*p[k] for k in atoms}
Zd = sum(direct.values())
direct = {k: direct[k]/Zd for k in direct}
print("formula (2.7) matches the schema-built r:",
      all(sp.simplify(sp.together((direct[k]-r_second[k]).subs(subs_num))) == 0 for k in atoms))

print()
print("=== Section 5: the probability-factor index pi 'reaches the point of absurdity'")
print("    at 2 atoms -- unless Q=p exactly, no valid r satisfies the pi-index identity ===")
p1, q1, Q1 = sp.symbols('p1 q1 Q1', positive=True)
p2, q2 = 1-p1, 1-q1
Q1_sol = sp.solve(sp.Eq(Q1*q1/p1 + (1-Q1)*q2/p2, 1), Q1)
print("unique Q1 making r a valid probability:", Q1_sol, " (equals p1 exactly:",
      sp.simplify(Q1_sol[0]-p1) == 0, ")")
