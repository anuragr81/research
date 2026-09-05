import sympy as sp
R = sp.Rational

p, q, a = sp.symbols('p q alpha', positive=True)   # p=prior P(E), q=delivered credence, a=alpha placeholder

# PW eq(1): upco pools P(E) with Q(E)
def upco(P_E, Q_E):
    return P_E*Q_E / (P_E*Q_E + (1-P_E)*(1-Q_E))

# --- 1. My ORIGINAL comparison: does Paper B's single-cue PB (= q, full deference) ---
#        equal upco(p, q) directly (treating the delivered credence AS PW's Q(E))?
naive = sp.simplify(upco(p, q) - q)
print("upco(p,q) - q  [treating delivered credence q as PW's Q(E) directly]:", naive)
print("  zero only at p=1/2:", sp.solve(sp.Eq(upco(p,q), q), p))

# --- 2. PW's own identification: Field's beta (odds-scale) reproduces q exactly when
#        beta is DERIVED from (q,p) via Paper B's own ell = q/p, ell0 = (1-q)/(1-p) ---
beta = (q/p) / ((1-q)/(1-p))   # = ell_1/ell_0, Paper B's own Bayes-factor RATIO
field_eq2 = beta*p / (beta*p + (1-p))   # PW's eq (2), Field updating on (E,beta)
print()
print("Field-updating-on-beta(p, q-derived-beta) - q  [should be 0, tautology confirming Prop IMM]:",
      sp.simplify(field_eq2 - q))

# --- 3. PW's OWN claimed identity: Field's eq(2) IS upco's eq(1) when Q(E):=beta/(beta+1) ---
Q_E_from_beta = beta/(beta+1)
pw_identity = sp.simplify(field_eq2 - upco(p, Q_E_from_beta))
print("Field eq(2) - upco(p, Q(E):=beta/(beta+1))  [PW's own claimed identity]:", pw_identity)

# --- 4. So what IS Q(E):=beta/(beta+1) in terms of p,q?  (the 'naive recommendation') ---
Q_E_simplified = sp.simplify(Q_E_from_beta)
print()
print("Q(E) := beta/(beta+1), simplified in terms of (p,q):", Q_E_simplified)
print("Q(E) equals q itself only when:", sp.solve(sp.Eq(Q_E_simplified, q), p))
