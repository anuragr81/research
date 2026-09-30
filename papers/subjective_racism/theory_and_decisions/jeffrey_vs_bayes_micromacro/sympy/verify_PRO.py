"""
Proposition PRO (uniqueness of the protected statistic), Section 5.4,
proof in Appendix A.4.  This is the paper's central characterisation.

  (i)  Protection.  If dF at q(x)r, restricted to the simplex tangent space, is
       a scalar multiple of d assoc, then F(Pbar_lambda) - F(P^B) = O(c^2) for
       every prior and every lambda.
  (ii) Uniqueness.  Fix lambda in (0,1).  If F(Pbar_lambda) - F(P^B) = o(c) for
       all (alpha,beta) in some open set, then dF at q(x)r restricted to the
       simplex is a multiple of d assoc.

The four steps of the appendix proof are checked in turn, then the resulting
classification is exercised on the statistics the paper names: the association,
the odds ratio, its logarithm, Yule's Q (all protected), against the marginal
probability, the conditional probability Pbar(B=1|A=1) (unprotected), and the
conditional-probability DIFFERENCE Pbar(B=1|A=1) - Pbar(B=1|A=0) (protected) --
the boundary case discussed at the end of Section 5.4.
"""
import sympy as sp
from jeffrey_core import *

X = sp.Matrix(2, 2, sp.symbols('X00 X01 X10 X11', positive=True))
SUBS_INDEP = list(zip(list(X), list(indep)))


def grad_at_indep(F):
    """grad F evaluated at the independent posterior q (x) r."""
    return sp.Matrix(2, 2, lambda i, j: sp.cancel(sp.diff(F(X), X[i, j]).subs(SUBS_INDEP)))


def in_span_J_gradassoc(G):
    """Is G in span{J, grad assoc(q(x)r)}?  Returns (bool, x, y)."""
    x_, y_ = sp.symbols('x_ y_')
    sol = sp.solve([sp.Eq(sp.cancel((x_ * J + y_ * grad_assoc_indep - G)[i, j]), 0)
                    for i in range(2) for j in range(2)], [x_, y_], dict=True)
    if not sol:
        return False, None, None
    s = sol[0]
    return True, sp.cancel(s.get(x_, x_)), sp.cancel(s.get(y_, y_))


def main():
    ck = Check("Proposition PRO -- uniqueness of the protected statistic")

    PJ_AB, PJ_BA, PB = posteriors()
    Pbar = mean_belief(PJ_AB, PJ_BA)
    M_lam = lam * kappa * R1 + (1 - lam) * kappa_p * R2

    # ================= Step 1: the aggregate moves in a plane ==============
    ck.mat_eq("Step 1: d/dc (Pbar_lambda - P^B)|_0 = M_lambda "
              "= lambda kappa R1 + (1-lambda) kappa' R2",
              mat_taylor_coeff(Pbar - PB, 1), M_lam)
    ck.mat_eq("Step 1: (Pbar_lambda - P^B)|_{c=0} = 0",
              sp.Matrix(2, 2, lambda i, j: sp.cancel((Pbar - PB)[i, j].subs(c, 0))),
              sp.zeros(2, 2))

    # ============ Step 2: the annihilator is span{J, grad assoc} ===========
    # R1, R2 independent (the appendix's row-sum argument)
    a_, b_ = sp.symbols('a_ b_')
    ck.eq("Step 2: adding the top-row entries of a R1 + b R2 gives b(r0+r1) = b",
          sp.expand((a_ * R1 + b_ * R2)[0, 0] + (a_ * R1 + b_ * R2)[0, 1]), b_)
    sol = sp.solve([sp.Eq((a_ * R1 + b_ * R2)[i, j], 0) for i in range(2) for j in range(2)],
                   [a_, b_], dict=True)
    ck("Step 2: R1, R2 are linearly independent",
       sol and all(sp.simplify(s.get(a_, 0)) == 0 and sp.simplify(s.get(b_, 0)) == 0
                   for s in sol), f"sol = {sol}")

    # the annihilator has dimension 4 - 2 = 2 (computed as a nullspace)
    def flat(Mx):
        return [Mx[i, j] for i in range(2) for j in range(2)]

    Arows = sp.Matrix([flat(R1), flat(R2)])          # <V, R_k> = 0 as a linear system
    ck("Step 2: the constraint matrix [R1; R2] has rank 2",
       Arows.rank() == 2, f"rank = {Arows.rank()}")
    ck("Step 2: dim{V : <V,R1> = <V,R2> = 0} = 4 - 2 = 2",
       len(Arows.nullspace()) == 2, f"nullity = {len(Arows.nullspace())}")

    ck.eq("Step 2: <J, R1> = 0", frob(J, R1), 0)
    ck.eq("Step 2: <J, R2> = 0", frob(J, R2), 0)
    ck.eq("Step 2: <grad assoc, R1> = 0", frob(grad_assoc_indep, R1), 0)
    ck.eq("Step 2: <grad assoc, R2> = 0", frob(grad_assoc_indep, R2), 0)

    x_, y_ = sp.symbols('x_ y_')
    dep = sp.solve([sp.Eq((x_ * J + y_ * grad_assoc_indep)[i, j], 0)
                    for i in range(2) for j in range(2)], [x_, y_], dict=True)
    ck("Step 2: J and grad assoc are linearly independent, hence span the annihilator",
       dep and all(sp.simplify(s.get(x_, 0)) == 0 and sp.simplify(s.get(y_, 0)) == 0
                   for s in dep), f"sol = {dep}")

    # ==================== Step 3: part (i), protection =====================
    ck.eq("Step 3 / part (i): grad F = xJ + y grad assoc  =>  <grad F, M_lambda> = 0 "
          "identically in (alpha, beta, lambda)",
          frob(x_ * J + y_ * grad_assoc_indep, M_lam), 0)

    # ==================== Step 4: part (ii), uniqueness ====================
    ratio = sp.cancel(kappa / kappa_p)
    ck.eq("Step 4: kappa/kappa' = (alpha-q0) r0(1-r0) / [(beta-r0) q0(1-q0)]",
          ratio, (alpha - q0) * r0 * (1 - r0) / ((beta - r0) * q0 * (1 - q0)))
    ck.eq("Step 4: d/dalpha (kappa/kappa') = r0(1-r0) / [(beta-r0) q0(1-q0)]",
          sp.cancel(sp.diff(ratio, alpha)),
          r0 * (1 - r0) / ((beta - r0) * q0 * (1 - q0)))
    ck.ne("Step 4: d/dalpha (kappa/kappa') != 0 wherever beta != r0, so kappa/kappa' "
          "is non-constant on any open set of priors", sp.diff(ratio, alpha))
    ck.eq("Step 4: kappa' vanishes only on {beta = r0}", kappa_p.subs(beta, r0), 0)

    # Uniqueness, checked constructively: impose vanishing of the aggregate
    # first-order coefficient at an interior lambda over an open set of
    # (alpha, beta) -- realised as several independent prior points -- and solve
    # for grad F.  The solution space must be exactly span{J, grad assoc}.
    G = sp.Matrix(2, 2, sp.symbols('G00 G01 G10 G11'))
    LAM = sp.Rational(2, 5)
    pts = [(sp.Rational(1, 3), sp.Rational(1, 4)), (sp.Rational(2, 7), sp.Rational(3, 8)),
           (sp.Rational(5, 9), sp.Rational(1, 5)), (sp.Rational(3, 5), sp.Rational(5, 8))]
    base = {q0: sp.Rational(2, 5), r0: sp.Rational(5, 7)}
    eqs = []
    for (av, bv) in pts:
        sub = dict(base); sub[alpha] = av; sub[beta] = bv; sub[lam] = LAM
        eqs.append(sp.Eq(sp.cancel(frob(G, M_lam).subs(sub)), 0))
    solG = sp.solve(eqs, list(G), dict=True)
    ck("Step 4 / part (ii): vanishing of the aggregate O(c) coefficient over an open "
       "set of priors (4 independent points, lambda = 2/5) has a 2-parameter "
       "solution space for grad F",
       bool(solG) and len(set().union(*[set(sp.Matrix(2, 2, lambda i, j:
            G[i, j].subs(solG[0]))[k].free_symbols) for k in range(4)])) == 2,
       f"solG = {solG}")
    if solG:
        Gsol = sp.Matrix(2, 2, lambda i, j: G[i, j].subs(solG[0]))
        Jn = J
        GA = sp.Matrix(2, 2, lambda i, j: grad_assoc_indep[i, j].subs(base))
        # every solution is a combination of J and grad assoc
        frees = sorted(set().union(*[set(Gsol[k].free_symbols) for k in range(4)]),
                       key=str)
        ok = True
        for f in frees:
            Gf = sp.Matrix(2, 2, lambda i, j: sp.cancel(sp.diff(Gsol[i, j], f)))
            s2 = sp.solve([sp.Eq(sp.cancel((x_ * Jn + y_ * GA - Gf)[i, j]), 0)
                           for i in range(2) for j in range(2)], [x_, y_], dict=True)
            ok = ok and bool(s2)
        ck("Step 4 / part (ii): that solution space IS span{J, grad assoc} "
           "-- no other statistic is protected", ok)

    # the interior-lambda hypothesis is essential (remark after the proof)
    n1 = len(sp.Matrix([flat(R1)]).nullspace())
    ck("remark: at lambda = 0 or 1 only one route direction enters, and the "
       "annihilator of a single direction is 3-dimensional, not 2", n1 == 3,
       f"dimension = {n1}")
    # concretely: at lambda = 1 there is a protected statistic outside
    # span{J, d assoc}, namely the A-marginal (Prop. DRF at lambda = 1)
    G_margA = grad_at_indep(lambda Q: Q[1, 0] + Q[1, 1])
    ck("remark: at lambda = 1 the A-marginal is protected although its "
       "differential is NOT in span{J, d assoc}",
       sp.cancel(frob(G_margA, M_lam.subs(lam, 1))) == 0
       and not in_span_J_gradassoc(G_margA)[0])

    # =============== the classification, exercised on real statistics ======
    print("\n  -- the protected class, statistic by statistic --")
    stats = [
        ("assoc",                      lambda Q: assoc(Q),                                   True),
        ("odds ratio",                 lambda Q: (Q[0, 0] * Q[1, 1]) / (Q[0, 1] * Q[1, 0]),  True),
        ("log odds ratio",             lambda Q: sp.log(Q[0, 0]) + sp.log(Q[1, 1])
                                                 - sp.log(Q[0, 1]) - sp.log(Q[1, 0]),        True),
        ("Yule's Q",                   lambda Q: assoc(Q) / (Q[0, 0] * Q[1, 1]
                                                             + Q[0, 1] * Q[1, 0]),           True),
        ("marginal P(A=1)",            lambda Q: Q[1, 0] + Q[1, 1],                          False),
        ("marginal P(B=1)",            lambda Q: Q[0, 1] + Q[1, 1],                          False),
        ("conditional P(B=1|A=1)",     lambda Q: Q[1, 1] / (Q[1, 0] + Q[1, 1]),              False),
        ("conditional difference "
         "P(B=1|A=1) - P(B=1|A=0)",    lambda Q: Q[1, 1] / (Q[1, 0] + Q[1, 1])
                                                 - Q[0, 1] / (Q[0, 0] + Q[0, 1]),            True),
        ("cell probability Q11",       lambda Q: Q[1, 1],                                    False),
    ]
    for name, F, expected in stats:
        G = grad_at_indep(F)
        inspan, _, _ = in_span_J_gradassoc(G)
        coeff = sp.cancel(frob(G, M_lam))
        firstorder_zero = (coeff == 0)
        ck(f"{name}: dF in span{{J, d assoc}} = {inspan}, aggregate O(c) coefficient "
           f"{'vanishes' if firstorder_zero else 'is nonzero'} "
           f"-> {'PROTECTED' if inspan else 'UNPROTECTED'} (paper: "
           f"{'protected' if expected else 'unprotected'})",
           inspan == expected and firstorder_zero == expected,
           f"grad = {list(G)}, coefficient = {coeff}")

    # ---- and the criterion agrees with the actual expansion ---------------
    print("\n  -- cross-check against the actual expansion in c --")
    for name, F, expected in stats:
        val = sp.cancel(sp.together(F(Pbar) - F(PB))) if 'log' not in name else None
        if val is None:
            continue
        k, _ = order_in_c_at_generic(val.subs(lam, sp.Rational(2, 5)))
        ck(f"{name}: actual leading order in c is {k} "
           f"({'>= 2 as protected' if expected else '= 1 as unprotected'})",
           (k is None or k >= 2) if expected else k == 1)

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
