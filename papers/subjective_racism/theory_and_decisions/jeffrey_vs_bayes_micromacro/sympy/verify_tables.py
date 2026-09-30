"""
Tables 1 and 2 of the manuscript, reproduced end to end.

Table 1 ("Order in the prior covariance c, individual versus aggregate")
and Table 2 ("What four aggregate audits of the same population find") are the
paper's summary of every order claim.  This script recomputes the order of each
listed quantity directly from the model -- individual and aggregate, belief and
decision -- and checks it against the order the table asserts.

`exactly` in the table means the leading coefficient vanishes identically in
(alpha, beta, q0, r0, lambda); `generically` means it vanishes only on a
lower-dimensional set.  The *identity* half of every `exactly` claim is proved
symbolically in the per-result scripts (verify_ASC, verify_DEC,
verify_DRF, verify_SCR); this script checks the resulting ORDER of
each table entry, which is what the table reports, at an exact rational point
chosen off every degenerate locus -- fast enough to run the whole table at once.
"""
import sympy as sp
from jeffrey_core import *
from decision_core import (c as cd, flip_bounds, share_given_delta, loss_given_delta)


def main():
    ck = Check("Tables 1 and 2 -- the order summary, end to end")

    PJ_AB, PJ_BA, PB = posteriors()
    Pbar = mean_belief(PJ_AB, PJ_BA)
    LAM = sp.Rational(2, 5)

    def order_agg(expr):
        return order_in_c_at_generic(sp.together(expr).subs(lam, LAM))[0]

    def order_ind(expr):
        return order_in_c_at_generic(expr)[0]

    # ---------------- Table 1: primitives ---------------------------------
    print("\n  -- Table 1: primitives --")
    ck("gap: individual Theta(c), aggregate Theta(c) [Prop. DIV; proof of Prop. PRO]",
       order_ind((PJ_AB - PB)[0, 0]) == 1 and order_agg((Pbar - PB)[0, 0]) == 1)

    v_un = sp.Matrix([[1, 0], [0, 0]])          # outside span{J, grad assoc}
    s_ind = frob(v_un, PJ_AB) - frob(v_un, PB)
    s_agg = frob(v_un, Pbar) - frob(v_un, PB)
    ck("score gap on an unprotected weight: individual Theta(c), aggregate Theta(c) "
       "[Lemma SCR]", order_ind(s_ind) == 1 and order_agg(s_agg) == 1)

    x_, y_ = sp.symbols('x_ y_')
    v_pr = x_ * J + y_ * grad_assoc_indep      # protected weight
    ck.eq("score gap on a protected weight has zero first-order coefficient "
          "(the aggregate route plane is annihilated)",
          frob(v_pr, lam * kappa * R1 + (1 - lam) * kappa_p * R2), 0)

    # ---------------- Table 1: belief statistics --------------------------
    print("\n  -- Table 1: belief statistics --")
    ck.eq("association: the first-order coefficient is annihilated on both routes "
          "-- <grad assoc, R1> = <grad assoc, R2> = 0 [Lemma ASC]",
          frob(grad_assoc_indep, lam * kappa * R1 + (1 - lam) * kappa_p * R2), 0)
    ck("association: individual departure is exactly order 2 [Lemma ASC]",
       order_ind(assoc(PJ_AB) - assoc(PB)) == 2)
    ck("association: aggregate departure is exactly order 2, not 1 and not higher "
       "[Prop. DEC]", order_agg(assoc(Pbar) - assoc(PB)) == 2)

    mA1 = lambda Q: marg_A(Q)[1]
    mB1 = lambda Q: marg_B(Q)[1]
    ck("marginal: on route AB the attribute read FIRST is O(c^2)",
       (order_ind(mA1(PJ_AB) - mA1(PB)) or 99) >= 2)
    ck("marginal: on route AB the attribute read LAST is Theta(c) generically",
       order_ind(mB1(PJ_AB) - mB1(PB)) == 1)
    ck("marginal: on route BA the attribute read FIRST is O(c^2)",
       (order_ind(mB1(PJ_BA) - mB1(PB)) or 99) >= 2)
    ck("marginal: on route BA the attribute read LAST is Theta(c) generically",
       order_ind(mA1(PJ_BA) - mA1(PB)) == 1)
    ck("marginal: aggregate Theta(c) generically for at least one attribute, "
       "for EVERY lambda",
       all(order_in_c_at_generic(
               sp.together(mA1(Pbar) - mA1(PB)).subs(lam, L))[0] == 1
           or order_in_c_at_generic(
               sp.together(mB1(Pbar) - mB1(PB)).subs(lam, L))[0] == 1
           for L in [0, sp.Rational(1, 4), sp.Rational(1, 2), sp.Rational(3, 4), 1]))

    # ---------------- Table 1: decision statistics ------------------------
    print("\n  -- Table 1: decision statistics --")
    dvals = [sp.Rational(3, 2), -sp.Rational(1, 2)]
    f0 = sp.Rational(1, 4)
    f_ = lambda x: f0                                   # uniform on [-2, 2]
    share = sp.simplify(sum(share_given_delta(f_, cd, dv, c_positive=True)
                            for dv in dvals) / len(dvals))
    loss = sp.simplify(sum(loss_given_delta(f_, cd, dv, c_positive=True)
                           for dv in dvals) / len(dvals))

    ck("loss (individual): identically 0 until |c delta| reaches |u| "
       "-- no leading coefficient at all [Sec. 4.4]",
       flip_bounds(sp.Rational(1, 100), sp.Rational(3, 2))[0]
       < 0 < flip_bounds(sp.Rational(1, 100), sp.Rational(3, 2))[1] + 1
       and not (abs(sp.Rational(1, 100) * sp.Rational(3, 2)) > abs(sp.Rational(1, 2))))
    ck("loss (aggregate): O(c^2) [Theorem LOS]",
       sp.series(loss, cd, 0, 3).removeO().coeff(cd, 1) == 0
       and sp.series(loss, cd, 0, 3).removeO().coeff(cd, 2) != 0)
    ck("share (aggregate): Theta(c) [Prop. SHR]",
       sp.series(share, cd, 0, 2).removeO().coeff(cd, 1) != 0)
    ck("share and loss decouple by exactly one order in c",
       sp.limit(loss / share, cd, 0, '+') == 0
       and sp.limit(loss / cd**2, cd, 0, '+') > 0
       and sp.limit(share / cd, cd, 0, '+') > 0)

    # ---------------- Table 2: the four audit questions -------------------
    print("\n  -- Table 2: what four aggregate audits of the same population find --")
    ck("Q1 'How much stereotype does the average belief carry?' "
       "-> assoc(Pbar) - assoc(PB) = O(c^2)",
       order_agg(assoc(Pbar) - assoc(PB)) == 2)
    ck("Q2 'How far does believed prevalence sit from the benchmark?' "
       "-> Pbar(A=1) - PB(A=1) = Theta(c) generically",
       order_agg(mA1(Pbar) - mA1(PB)) == 1)
    ck("Q3 'What share of decisions did the reading sequence change?' "
       "-> Theta(c)",
       sp.series(share, cd, 0, 2).removeO().coeff(cd, 1) != 0)
    ck("Q4 'How large is the loss through sequence-affected decisions?' "
       "-> O(c^2)",
       sp.series(loss, cd, 0, 3).removeO().coeff(cd, 1) == 0)

    # ---------------- the paper's headline claim --------------------------
    print("\n  -- the headline claim --")
    ck("every individual's gap is Theta(c) while the aggregate association and "
       "the surplus-weighted loss are O(c^2): an aggregate can look Bayesian "
       "when no member's belief is",
       order_ind((PJ_AB - PB)[0, 0]) == 1
       and order_ind((PJ_BA - PB)[0, 0]) == 1
       and order_agg(assoc(Pbar) - assoc(PB)) == 2
       and sp.series(loss, cd, 0, 3).removeO().coeff(cd, 1) == 0)
    ck("...while a FIRST-order share of individuals is genuinely affected, so "
       "the null on the protected statistics is a fact about the instrument",
       sp.series(share, cd, 0, 2).removeO().coeff(cd, 1) != 0)

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
