"""
Lemma ASC (individual-level association immunity), Section 4.2.

    assoc(P^J_sigma) - assoc(P^B) = O(c^2)  identically in (alpha,beta,q0,r0),
    for each reading sequence sigma in {AB, BA}.

The manuscript's proof runs through the chain rule plus the two vanishing
inner products <grad assoc(q(x)r), R1> = <grad assoc(q(x)r), R2> = 0.  Both the
conclusion and the two inner products are checked here, and the gradient
formula printed in the proof is checked against the actual gradient.
"""
import sympy as sp
from jeffrey_core import *


def main():
    ck = Check("Lemma ASC -- individual-level association immunity")

    PJ_AB, PJ_BA, PB = posteriors()

    # ---- the gradient of assoc at the independent posterior q (x) r --------
    X = sp.Matrix(2, 2, sp.symbols('x00 x01 x10 x11'))
    grad = sp.Matrix(2, 2, lambda i, j: sp.diff(assoc(X), X[i, j])).subs(
        list(zip(list(X), list(indep))))
    ck.mat_eq("grad assoc|_{q(x)r} = [[q1 r1, -q1 r0], [-q0 r1, q0 r0]]",
              grad, grad_assoc_indep)

    # ---- the two inner products vanish termwise ---------------------------
    ck.eq("<grad assoc(q(x)r), R1> = 0", frob(grad_assoc_indep, R1), 0)
    ck.eq("<grad assoc(q(x)r), R2> = 0", frob(grad_assoc_indep, R2), 0)

    # ---- hence the first-order term of the association gap vanishes -------
    for nm, PJ in [("AB", PJ_AB), ("BA", PJ_BA)]:
        d = assoc(PJ) - assoc(PB)
        ck.eq(f"assoc(P^J_{nm}) - assoc(P^B) has no constant term", taylor_coeff(d, 0), 0)
        ck.eq(f"assoc(P^J_{nm}) - assoc(P^B) has no O(c) term  ->  = O(c^2)",
              taylor_coeff(d, 1), 0)

    # ---- and it is genuinely second order, not higher (route AB) ----------
    k, co = order_in_c_at_generic(assoc(PJ_AB) - assoc(PB))
    ck("assoc(P^J_AB) - assoc(P^B) is exactly Theta(c^2) generically",
       k == 2, f"leading order = {k}, coefficient = {co}")

    # ---- the direct chain-rule identity used in the proof -----------------
    for nm, PJ, D in [("AB", PJ_AB, kappa * R1), ("BA", PJ_BA, kappa_p * R2)]:
        ck.eq(f"chain rule on route {nm}: d/dc[assoc(P^J)-assoc(P^B)]|_0 "
              f"= <grad assoc, D_{nm}>",
              taylor_coeff(assoc(PJ) - assoc(PB), 1), frob(grad_assoc_indep, D))

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
