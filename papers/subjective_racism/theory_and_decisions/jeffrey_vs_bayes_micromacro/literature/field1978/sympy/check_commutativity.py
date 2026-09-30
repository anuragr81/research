"""
Field (1978), "A Note on Jeffrey Conditionalization" -- checks of the paper's
own claims. Each check prints PASS/FAIL; the script exits 0 iff all pass.

  (1) eq. (4) inverts eq. (5), and eq. (5) inverts eq. (4);
  (2) e^{2 alpha} is the odds ratio (q/p)/((1-q)/(1-p)), i.e. the likelihood
      ratio of a Bayes update (audit L2): e^{alpha} is its square root;
  (3) eq. (7): the two alpha-tilts commute and both equal the closed form
      with weights e^{+-alpha +- alpha'} (audit L1: tilt_step1 used to ignore
      its argument, so the E'-then-E route was never computed);
  (4) the same two updates in Jeffrey's credence parameters q, q' do not
      commute (Field p. 365); witness 651/28120.

Lean: lean/Literature/Field.lean (symlinked in ../lean/).
"""
import sys

import sympy as sp

R = sp.Rational
FAILS = []


def check(name, ok, detail=""):
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}" + (f"\n         {detail}" if detail and not ok else ""))
    if not ok:
        FAILS.append(name)


def is_zero(expr):
    return sp.simplify(sp.together(expr)) == 0


# --- eqs. (4), (5) --------------------------------------------------------------
p, q = sp.symbols('p q', positive=True)
alpha = sp.symbols('alpha', real=True)


def q_of_alpha(p, alpha):          # eq. (5)
    return p * sp.exp(alpha) / (p * sp.exp(alpha) + (1 - p) * sp.exp(-alpha))


def alpha_of(p, q):                # eq. (4)
    return R(1, 2) * sp.log((q / p) / ((1 - q) / (1 - p)))


print("eqs. (4)-(5)")
a_rec = alpha_of(p, q_of_alpha(p, alpha))
check("eq. (4) after eq. (5) is the identity: alpha(p, q(alpha, p)) = alpha",
      sp.simplify(sp.expand_log(sp.simplify(a_rec), force=True) - alpha) == 0,
      f"difference = {sp.simplify(a_rec - alpha)}")
# (5) after (4), at exact rational points in (0,1)
pts = [(R(3, 10), R(4, 10)), (R(1, 7), R(5, 6)), (R(9, 10), R(1, 20))]
check("eq. (5) after eq. (4) is the identity at three rational points",
      all(sp.simplify(q_of_alpha(pp, alpha_of(pp, qq)) - qq) == 0 for pp, qq in pts))

print("e^{2 alpha} is the likelihood ratio (audit L2)")
odds_ratio = (q / p) / ((1 - q) / (1 - p))
check("e^{2 alpha(p,q)} = (q/p)/((1-q)/(1-p))",
      sp.simplify(sp.exp(2 * alpha_of(p, q)) - odds_ratio) == 0)
l1, l0 = sp.symbols('ell1 ell0', positive=True)
q_bayes = p * l1 / (p * l1 + (1 - p) * l0)
check("for a Bayes update with likelihoods l1, l0: e^{2 alpha} = l1/l0",
      is_zero(sp.exp(2 * alpha_of(p, q_bayes)) - l1 / l0))
check("so e^{alpha} is the square root of the likelihood ratio, not e^{2 alpha} its square",
      is_zero(sp.exp(alpha_of(p, q_bayes)) ** 2 - l1 / l0))

# --- eq. (7) --------------------------------------------------------------------
# cells (a, b): a = 1 iff E, b = 1 iff E'
F11, F10, F01, F00 = sp.symbols('F11 F10 F01 F00', positive=True)
al, ap = sp.symbols('alpha alpha_prime', real=True)
prior = {(1, 1): F11, (1, 0): F10, (0, 1): F01, (0, 0): F00}


def normalize(w):
    Z = sum(w.values())
    return {k: w[k] / Z for k in w}


def tilt_E(F, a):
    """Field's (6) on E = {(1,1),(1,0)}: weight e^{a} on E, e^{-a} off it."""
    return normalize({k: F[k] * sp.exp(a if k[0] == 1 else -a) for k in F})


def tilt_Ep(F, a):
    """Field's (6) on E' = {(1,1),(0,1)}."""
    return normalize({k: F[k] * sp.exp(a if k[1] == 1 else -a) for k in F})


route_EEp = tilt_Ep(tilt_E(prior, al), ap)
route_EpE = tilt_E(tilt_Ep(prior, ap), al)
closed = normalize({k: prior[k] * sp.exp((al if k[0] == 1 else -al) + (ap if k[1] == 1 else -ap))
                    for k in prior})

print("eq. (7)")
check("tilt_E actually reweights its argument (regression test for audit L1)",
      is_zero(tilt_E({k: 1 for k in prior}, al)[(1, 1)] - sp.exp(al) / (2 * sp.exp(al) + 2 * sp.exp(-al))))
check("E-then-E' equals E'-then-E in all four cells",
      all(is_zero(route_EEp[k] - route_EpE[k]) for k in prior))
check("E-then-E' equals the closed form (7) in all four cells",
      all(is_zero(route_EEp[k] - closed[k]) for k in prior))
check("E'-then-E equals the closed form (7) in all four cells",
      all(is_zero(route_EpE[k] - closed[k]) for k in prior))
# eq. (6) = eq. (3) with q from eq. (5), on a normalized prior (F00 = 1 - F11 - F10 - F01)
prior_n = {k: v.subs(F00, 1 - F11 - F10 - F01) for k, v in prior.items()}
q5 = q_of_alpha(F11 + F10, al)

# --- credence-input (Jeffrey) updates ------------------------------------------
q1, q2 = sp.symbols('q1 q2', positive=True)


def jeffrey_E(F, t):
    mE = F[(1, 1)] + F[(1, 0)]
    mnE = F[(0, 1)] + F[(0, 0)]
    return {k: (F[k] / mE * t if k[0] == 1 else F[k] / mnE * (1 - t)) for k in F}


def jeffrey_Ep(F, t):
    mE = F[(1, 1)] + F[(0, 1)]
    mnE = F[(1, 0)] + F[(0, 0)]
    return {k: (F[k] / mE * t if k[1] == 1 else F[k] / mnE * (1 - t)) for k in F}


check("eq. (6) is Jeffrey's (3) with q from (5), in all four cells",
      all(is_zero(tilt_E(prior_n, al)[k] - jeffrey_E(prior_n, q5)[k]) for k in prior))

jr1 = jeffrey_Ep(jeffrey_E(prior, q1), q2)
jr2 = jeffrey_E(jeffrey_Ep(prior, q2), q1)
gap = sp.simplify(sp.together(jr1[(1, 1)] - jr2[(1, 1)]))
pt = {F11: R(1, 10), F10: R(2, 10), F01: R(3, 10), F00: R(4, 10), q1: R(7, 10), q2: R(3, 10)}
print("credence-input updates (p. 365)")
check("the credence-input gap in cell (1,1) is not identically zero", gap != 0)
check("E-then-E' value 147/760 at F=(.1,.2,.3,.4), q=.7, q'=.3", jr1[(1, 1)].subs(pt) == R(147, 760))
check("E'-then-E value 63/370 at the same point", jr2[(1, 1)].subs(pt) == R(63, 370))
check("gap = 651/28120 at the same point", gap.subs(pt) == R(651, 28120))

print(f"\n  -> {'all checks passed' if not FAILS else 'FAILURES: ' + ', '.join(FAILS)}")
sys.exit(0 if not FAILS else 1)
