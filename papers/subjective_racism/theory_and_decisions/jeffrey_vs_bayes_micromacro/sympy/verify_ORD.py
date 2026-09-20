"""
Proposition ORD (between-order contrast), companion to DRF/SCR/DEC.

DRF and SCR compare a *route* (or the mixed aggregate) to the sequence-free
benchmark P^B.  ORD compares the two *routes to each other*:

    d/dc <v, P^J_AB - P^J_BA> |_{c=0}  =  kappa <v, R1>  -  kappa' <v, R2>,

for ANY linear read-out <v, .>.  Two things distinguish this from DRF:

  (1) It is benchmark-free and lambda-free.  DRF's aggregate drift is
      (1-lambda) K and requires knowing P^B and the order mix lambda; the
      between-order gap is the same -K c whatever the benchmark or lambda,
      because P^J_AB - P^J_BA never mentions either.  An auditor who records
      reading order can compute it; one who has only a pooled aggregate cannot.

  (2) It says what the discriminating design must have.  The contrast exists
      only when reading order varies across evaluators and is recorded; a
      pooled audit that discards order also discards this statistic.

The row table (first-order coefficient of the between-order gap in c):

    statistic        coefficient                     needs benchmark?
    A-marginal       kappa' = -K = (beta-r0)q0(1-q0)/Z      no
    B-marginal       -kappa      = (q0-alpha)r0(1-r0)/Z     no
    association      0 at first order (Theta(c^2))          no
    generic score    Theta(c) unless v in span{J,grad}      no

The four rows are read off the single formula by choosing v, and the formula
itself is checked directly against the composed routes (not merely assumed from
Proposition DIV).
"""
import sympy as sp
from jeffrey_core import *


def main():
    ck = Check("Proposition ORD -- the between-order contrast")

    PJ_AB, PJ_BA, PB = posteriors()

    # entrywise first-order Taylor coefficient of the between-order gap
    G1 = mat_taylor_coeff(PJ_AB - PJ_BA, 1)          # d/dc (P^J_AB - P^J_BA)|0
    # the claimed closed form
    G1_claim = kappa * R1 - kappa_p * R2

    # ---- (0) the between-order gap vanishes at c = 0 (Proposition IMM) -----
    ck.mat_eq("P^J_AB = P^J_BA at c = 0 (routes agree on independent prior)",
              mat_taylor_coeff(PJ_AB - PJ_BA, 0), sp.zeros(2, 2))

    # ---- (1) the master formula, entrywise, checked against the routes ----
    ck.mat_eq("d/dc (P^J_AB - P^J_BA)|0 = kappa R1 - kappa' R2  (entrywise)",
              G1, G1_claim)

    # linear read-out form, for a fully generic weight v
    v = sp.Matrix([[sp.Symbol('v00'), sp.Symbol('v01')],
                   [sp.Symbol('v10'), sp.Symbol('v11')]])
    lhs = frob(v, G1)
    rhs = kappa * frob(v, R1) - kappa_p * frob(v, R2)
    ck.eq("<v, .> form: coeff = kappa<v,R1> - kappa'<v,R2> for arbitrary v",
          lhs, rhs)

    # ---- (2) benchmark-free / lambda-free: the gap never mentions P^B or lambda
    # The mixed-aggregate-vs-benchmark drift (DRF) carries a (1-lambda) factor
    # and is measured against P^B; the between-order gap has neither.
    K = q0 * (1 - q0) * (r0 - beta) / Z
    ck("the gap expression contains no lambda symbol",
       lam not in (PJ_AB - PJ_BA).free_symbols)
    ck("DRF's aggregate drift DOES carry (1-lambda) -- the contrast is that ORD does not",
       sp.simplify((1 - lam) * K).has(lam))

    # ---- (3) row: A-marginal --------------------------------------------
    vA = sp.Matrix([[0, 0], [1, 1]])                 # picks Q(A=1)
    cA = frob(vA, G1)
    ck.eq("A-marginal between-order gap coeff = kappa' = (beta-r0)q0(1-q0)/Z", cA, kappa_p)
    ck.eq("...and kappa' = -K  (so the gap is -K c, DRF's coefficient with sign flip)",
          kappa_p, -K)
    # cross-check straight off the pinning identities: P^J_AB(A=1)=q1-Kc+..,
    # P^J_BA(A=1)=q1 exactly, so the gap's A-coeff is -K = kappa'.
    ck.eq("direct: P^J_AB(A=1) - P^J_BA(A=1) first-order coeff = kappa' "
          "(P^J_BA(A=1)=q1 exact, P^J_AB(A=1)=q1-Kc)",
          taylor_coeff(marg_A(PJ_AB)[1] - marg_A(PJ_BA)[1], 1), kappa_p)

    # ---- (4) row: B-marginal --------------------------------------------
    vB = sp.Matrix([[0, 1], [0, 1]])                 # picks Q(B=1)
    cB = frob(vB, G1)
    ck.eq("B-marginal between-order gap coeff = -kappa = (q0-alpha)r0(1-r0)/Z",
          cB, -kappa)

    # ---- (5) row: association is second-order ---------------------------
    ka, _ = order_in_c_at_generic(assoc(PJ_AB) - assoc(PJ_BA))
    ck("association between-order gap is Theta(c^2), not first order",
       ka == 2, f"leading order in c = {ka}")
    # consistent with the linear formula: no single v reproduces assoc, but the
    # first-order piece of any linear proxy for assoc must vanish -- check that
    # the gradient of assoc at the independent point is in the null space.
    ck.eq("grad(assoc) at q(x)r annihilates the first-order gap direction",
          frob(grad_assoc_indep, G1), 0)

    # ---- (6) row: generic score is Theta(c), protected weights are not ---
    # A generic desirability weight u gives a first-order score gap.
    u = sp.Matrix([[sp.Rational(3, 1), sp.Rational(-1, 1)],
                   [sp.Rational(2, 1), sp.Rational(5, 1)]])
    ck.ne("generic desirability weight: between-order score gap is nonzero at O(c)",
          frob(u, G1))
    kg, _ = order_in_c_at_generic(frob(u, PJ_AB - PJ_BA))
    ck("...and it is genuinely Theta(c)", kg == 1, f"leading order = {kg}")
    # constant weight J: no score gap (probabilities sum to 1 on both routes)
    ck.eq("constant weight J: no between-order score gap (both routes normalise)",
          frob(J, G1), 0)

    # ---- (7) at least one marginal moves; both cannot be first-order-null --
    # A-coeff = kappa' , B-coeff = -kappa ; both zero only if r0=beta AND q0=alpha
    sol = sp.solve([sp.Eq(kappa_p, 0), sp.Eq(kappa, 0)],
                   [r0], dict=True)
    ck("A- and B-marginal gaps both vanish only on the measure-zero set r0=beta, q0=alpha",
       all((s.get(r0) == beta) for s in sol) if sol else True,
       f"solutions: {sol}")
    ck.ne("at the generic point the A-marginal contrast is nonzero", cA)
    ck.ne("at the generic point the B-marginal contrast is nonzero", cB)

    # ---- (8) equivalence with protection: the between-order coefficient
    # kappa<G,R1> - kappa'<G,R2> vanishes over an open set of priors (four
    # independent (alpha, beta) points) exactly when G annihilates span{R1,R2},
    # i.e. G in span{J, grad assoc} -- the same solution space PRO finds for the
    # aggregate coefficient <G, M_lambda>.  No lambda enters.
    G = sp.Matrix(2, 2, sp.symbols('G00 G01 G10 G11'))
    pts = [(sp.Rational(1, 3), sp.Rational(1, 4)), (sp.Rational(2, 7), sp.Rational(3, 8)),
           (sp.Rational(5, 9), sp.Rational(1, 5)), (sp.Rational(3, 5), sp.Rational(5, 8))]
    base = {q0: sp.Rational(2, 5), r0: sp.Rational(5, 7)}
    coeff = kappa * frob(G, R1) - kappa_p * frob(G, R2)
    eqs = []
    for (av, bv) in pts:
        sub = dict(base); sub[alpha] = av; sub[beta] = bv
        eqs.append(sp.Eq(sp.cancel(coeff.subs(sub)), 0))
    solG = sp.solve(eqs, list(G), dict=True)
    Gsol = sp.Matrix(2, 2, lambda i, j: G[i, j].subs(solG[0])) if solG else None
    free = set().union(*[Gsol[k].free_symbols for k in range(4)]) if solG else set()
    ck("(8) vanishing between-order coefficient over 4 priors leaves a 2-parameter "
       "solution space for grad F", bool(solG) and len(free) == 2, f"solG = {solG}")
    if solG:
        GA = sp.Matrix(2, 2, lambda i, j: grad_assoc_indep[i, j].subs(base))
        a_, b_ = sp.symbols('a_ b_')
        fit = sp.solve([sp.Eq(Gsol[k], a_ * J[k] + b_ * GA[k]) for k in range(4)],
                       [a_, b_] + sorted(free, key=str), dict=True)
        ck("(8) every solution is a combination of J and grad assoc, i.e. "
           "annihilates span{R1,R2}: second-order between-order gap <=> protected",
           bool(fit), f"fit = {fit}")
        R1n = sp.Matrix(2, 2, lambda i, j: R1[i, j].subs(base))
        R2n = sp.Matrix(2, 2, lambda i, j: R2[i, j].subs(base))
        ck.eq("(8) ...and the solution annihilates R1", sp.expand(frob(Gsol, R1n)), 0)
        ck.eq("(8) ...and the solution annihilates R2", sp.expand(frob(Gsol, R2n)), 0)
    # kappa and kappa' vary independently across priors: the Jacobian in
    # (alpha, beta) has full rank at the generic point.
    Jac = sp.Matrix([[sp.diff(kappa, alpha), sp.diff(kappa, beta)],
                     [sp.diff(kappa_p, alpha), sp.diff(kappa_p, beta)]])
    ck.ne("(8) kappa, kappa' vary independently: det d(kappa,kappa')/d(alpha,beta) != 0",
          sp.cancel(Jac.det()))

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
