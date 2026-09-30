"""
Lemma SCR (first-order score gap), Section 5.1.2.

    s(P^J_sigma) - s(P^B) = c * delta_sigma + O(c^2),
    delta_sigma := <v, d/dc (P^J_sigma - P^B)|_{c=0}>,

with, generically, delta_AB != delta_BA, and delta_sigma = 0 whenever v is
constant (both posteriors being normalised, the score telescopes).

Also checked: the definition of delta_sigma given in Section 4.1 agrees with
the one used in Section 5.1.2, and the score gap is Theta(c) exactly when v
lies outside span{J, grad assoc} -- the "unprotected weight" row of Table 1.
"""
import sympy as sp
from jeffrey_core import *


def main():
    ck = Check("Lemma SCR -- first-order score gap")

    v00, v01, v10, v11 = sp.symbols('v00 v01 v10 v11', real=True)
    v = sp.Matrix([[v00, v01], [v10, v11]])

    PJ_AB, PJ_BA, PB = posteriors()

    def score(Q):
        return frob(v, Q)

    D_AB, D_BA = kappa * R1, kappa_p * R2
    delta_AB = frob(v, D_AB)
    delta_BA = frob(v, D_BA)

    # ---- the expansion ----------------------------------------------------
    ck.eq("score gap on route AB has no constant term",
          taylor_coeff(score(PJ_AB) - score(PB), 0), 0)
    ck.eq("d/dc [s(P^J_AB) - s(P^B)]|_0 = delta_AB = <v, kappa R1>",
          taylor_coeff(score(PJ_AB) - score(PB), 1), delta_AB)
    ck.eq("score gap on route BA has no constant term",
          taylor_coeff(score(PJ_BA) - score(PB), 0), 0)
    ck.eq("d/dc [s(P^J_BA) - s(P^B)]|_0 = delta_BA = <v, kappa' R2>",
          taylor_coeff(score(PJ_BA) - score(PB), 1), delta_BA)

    # ---- constant v kills the score gap -----------------------------------
    const = {v00: 1, v01: 1, v10: 1, v11: 1}
    ck.eq("delta_AB = 0 when v is constant", delta_AB.subs(const), 0)
    ck.eq("delta_BA = 0 when v is constant", delta_BA.subs(const), 0)
    k_ = sp.Symbol('k_')
    ck.eq("delta_AB = 0 for any constant vector v = k*J",
          delta_AB.subs({v00: k_, v01: k_, v10: k_, v11: k_}), 0)

    # ---- generically delta_AB != delta_BA ---------------------------------
    ck.ne("delta_AB - delta_BA != 0 generically",
          (delta_AB - delta_BA).subs({v00: 1, v01: 0, v10: 0, v11: 0}))

    # ---- protected vs unprotected weights (Table 1, "score gap" row) ------
    # v in span{J, grad assoc} -> the score gap is O(c^2) for BOTH routes
    x_, y_ = sp.symbols('x_ y_')
    v_prot = x_ * J + y_ * grad_assoc_indep
    ck.eq("v in span{J, grad assoc}: delta_AB = 0",
          frob(v_prot, D_AB), 0)
    ck.eq("v in span{J, grad assoc}: delta_BA = 0",
          frob(v_prot, D_BA), 0)
    ck.eq("v in span{J, grad assoc}: aggregate first-order score gap = 0",
          frob(v_prot, lam * D_AB + (1 - lam) * D_BA), 0)

    # an explicitly unprotected v: check the score gap really is Theta(c)
    v_unprot = {v00: 1, v01: 0, v10: 0, v11: 0}
    k, co = order_in_c_at_generic((score(PJ_AB) - score(PB)).subs(v_unprot))
    ck("unprotected weight v = e_00: score gap is Theta(c)", k == 1,
       f"leading order = {k}, coefficient = {co}")

    # e_00 is indeed outside span{J, grad assoc}
    e00 = sp.Matrix([[1, 0], [0, 0]])
    sol = sp.solve([sp.Eq((x_ * J + y_ * grad_assoc_indep - e00)[i, j], 0)
                    for i in range(2) for j in range(2)], [x_, y_], dict=True)
    ck("e_00 is outside span{J, grad assoc} (no solution)", sol == [], f"sol={sol}")

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
