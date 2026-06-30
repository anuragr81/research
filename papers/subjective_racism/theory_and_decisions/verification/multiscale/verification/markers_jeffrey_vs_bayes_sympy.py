#!/usr/bin/env python3
r"""
Multiple soft markers: Jeffrey vs Bayes updating.

Decision-level model (no dynamics). An observer reads two soft cues before an
engagement decision. State = two binary attributes A, B (e.g. what the accent
suggests, what the name suggests); joint prior p[i][j] = P(A=i, B=j).

  * A Jeffrey updater registers each cue as the IMPRESSION it produces -- a target
    marginal over a partition (q over A, r over B) -- and updates by Jeffrey's rule,
    holding the orthogonal conditionals rigid:  P'(.) = sum_i q_i P(. | A=i).
  * A Bayesian updater registers each cue as a LIKELIHOOD (l^A over A, l^B over B)
    and updates by Bayes' rule.

Claims, each checked below with normalisation q0+q1 = r0+r1 = 1 imposed:
  (1) Jeffrey is NON-COMMUTATIVE: cue-order changes the posterior. The last cue's
      partition keeps its target marginal; the earlier one is disturbed.
  (2) Bayes is COMMUTATIVE: cue-order is irrelevant.
  (3) The Jeffrey order-effect can FLIP an engagement decision across a threshold.
  (4) SCOPE: Jeffrey commutes iff the two markers are independent under the prior;
      the effect requires correlated markers. (And specifying cues by likelihood/
      Bayes-factor instead of impression also removes it -- the Bayesian case.)

Run:  python3 markers_jeffrey_vs_bayes_sympy.py    (exits non-zero on any failure)
"""
import sympy as sp

PASS = []
def ok(name, cond):
    assert cond, f"FAILED: {name}"
    PASS.append(name); print(f"  [ok] {name}")

# ---- prior and cue parameters, with marginal normalisation imposed -------------
p00, p01, p10, p11 = sp.symbols('p00 p01 p10 p11', positive=True)
q0, r0 = sp.symbols('q0 r0', positive=True)
q = {0: q0, 1: 1 - q0}          # target A-marginal from cue 1 (normalised)
r = {0: r0, 1: 1 - r0}          # target B-marginal from cue 2 (normalised)
P = {(0,0): p00, (0,1): p01, (1,0): p10, (1,1): p11}

Amarg = lambda D, i: D[(i,0)] + D[(i,1)]
Bmarg = lambda D, j: D[(0,j)] + D[(1,j)]
jA = lambda D, q: {(i,j): q[i]*D[(i,j)]/Amarg(D,i) for i in (0,1) for j in (0,1)}
jB = lambda D, r: {(i,j): r[j]*D[(i,j)]/Bmarg(D,j) for i in (0,1) for j in (0,1)}

P12 = jB(jA(P, q), r)   # cue order: A then B
P21 = jA(jB(P, r), q)   # cue order: B then A

print("=" * 70); print("(1) JEFFREY is non-commutative"); print("=" * 70)
# last-updated partition retains its target marginal:
ok("A-then-B keeps B-marginal = r", all(sp.simplify(Bmarg(P12,j)-r[j])==0 for j in (0,1)))
ok("B-then-A keeps A-marginal = q", all(sp.simplify(Amarg(P21,i)-q[i])==0 for i in (0,1)))
# earlier partition is disturbed -> the two posteriors differ.  Show on a correlated prior:
corr = {p00: sp.Rational(9,20), p01: sp.Rational(1,20), p10: sp.Rational(1,4), p11: sp.Rational(1,4),
        q0: sp.Rational(4,5), r0: sp.Rational(1,2)}
cell_gap = sp.nsimplify((P12[(0,0)] - P21[(0,0)]).subs(corr))
ok("posteriors differ at a correlated prior (P12 != P21)", cell_gap != 0)
print(f"       P12(0,0)-P21(0,0) = {cell_gap}  ({float(cell_gap):+.4f})")

print("=" * 70); print("(2) BAYES is commutative"); print("=" * 70)
lA0, lA1, lB0, lB1 = sp.symbols('lA0 lA1 lB0 lB1', positive=True)
lA = {0: lA0, 1: lA1}; lB = {0: lB0, 1: lB1}
def bstep(D, l, axis):
    raw = {(i,j): D[(i,j)]*(l[i] if axis=='A' else l[j]) for i in (0,1) for j in (0,1)}
    Z = sum(raw.values()); return {k: v/Z for k, v in raw.items()}
BAB = bstep(bstep(P, lA, 'A'), lB, 'B')   # likelihood A then B
BBA = bstep(bstep(P, lB, 'B'), lA, 'A')   # likelihood B then A
ok("Bayes posterior identical in both orders", all(sp.simplify(BAB[k]-BBA[k])==0 for k in P))

print("=" * 70); print("(3) the order-effect flips an engagement decision"); print("=" * 70)
# desirability c[state]; engage iff E[c] >= tau
c = {(0,0): sp.Integer(1), (0,1): sp.Rational(3,10), (1,0): sp.Rational(3,10), (1,1): sp.Integer(0)}
Ec = lambda D: sum(D[k]*c[k] for k in D)
E12 = sp.nsimplify(Ec(P12).subs(corr)); E21 = sp.nsimplify(Ec(P21).subs(corr))
tau = (E12 + E21) / 2
print(f"       E[c | A then B] = {float(E12):.4f}    E[c | B then A] = {float(E21):.4f}")
print(f"       threshold tau = {float(tau):.4f}")
ok("same cues, opposite decision across tau",
   (E12 >= tau) != (E21 >= tau) and E12 != E21)
print(f"       A-then-B -> {'ENGAGE' if E12>=tau else 'reject'} ; "
      f"B-then-A -> {'ENGAGE' if E21>=tau else 'reject'}")

print("=" * 70); print("(4) SCOPE: order-effect requires correlated markers"); print("=" * 70)
a, b = sp.symbols('a b', positive=True)
indep = {p00: a*b, p01: a*(1-b), p10: (1-a)*b, p11: (1-a)*(1-b)}  # independent prior
# under independence both orders collapse to the product q⊗r (check via cancel):
ok("independent prior => Jeffrey commutes (P12 = P21)",
   all(sp.cancel((P12[k]-P21[k]).subs(indep)) == 0 for k in P))
ok("independent prior => P12 = q⊗r",
   all(sp.cancel(P12[(i,j)].subs(indep) - q[i]*r[j]) == 0 for i in (0,1) for j in (0,1)))

print("\n" + "=" * 70); print(f"ALL {len(PASS)} CHECKS PASSED"); print("=" * 70)
for i, n in enumerate(PASS, 1): print(f"  {i}. {n}")
print()
print("Reading: a Jeffrey updater who registers soft cues as impressions is order-")
print("dependent whenever the cues are correlated -- a primacy/first-impression effect")
print("in the engagement decision that a likelihood-based Bayesian cannot produce. The")
print("effect is transient (about the path of belief formation), not a steady state.")
