"""
Proposition DIV (micro divergence), Section 4.1, with the deferred proof of
Appendix A.1.

  (i)  P^J_AB - P^B = c * kappa * R1 + O(c^2),  R1 = q (x) (1,-1),
                                                kappa  = (alpha-q0) r0(1-r0)/Z
       P^J_BA - P^B = c * kappa'* R2 + O(c^2),  R2 = (1,-1) (x) r,
                                                kappa' = (beta-r0) q0(1-q0)/Z
       kappa = 0 <=> q0 = alpha or r0 in {0,1};  D_sigma not identically 0.

  (ii) P^J_AB - P^J_BA = c * Delta_seq + O(c^2),
       Delta_seq = kappa R1 - kappa' R2, not identically 0 since R1, R2 are
       linearly independent.

Appendix A.1 additionally asserts closed forms for D and Delta_seq that are
checked here entry by entry.
"""
import sympy as sp
from jeffrey_core import *


def main():
    ck = Check("Proposition DIV -- micro divergence (with Appendix A.1)")

    PJ_AB, PJ_BA, PB = posteriors()

    gap_AB = PJ_AB - PB
    gap_BA = PJ_BA - PB
    seq = PJ_AB - PJ_BA

    # ---- Step 1 of the appendix proof: everything vanishes at c = 0 --------
    ck.mat_eq("Step 1: (P^J_AB - P^B)|_{c=0} = 0", gap_AB.subs(c, 0), sp.zeros(2, 2))
    ck.mat_eq("Step 1: (P^J_AB - P^J_BA)|_{c=0} = 0", seq.subs(c, 0), sp.zeros(2, 2))

    # ---- Step 2/3: the leading coefficients --------------------------------
    D_AB = mat_taylor_coeff(gap_AB, 1)
    D_BA = mat_taylor_coeff(gap_BA, 1)
    D_seq = mat_taylor_coeff(seq, 1)

    ck.mat_eq("(i) d/dc (P^J_AB - P^B)|_0 = kappa * R1", D_AB, kappa * R1)
    ck.mat_eq("(i) d/dc (P^J_BA - P^B)|_0 = kappa' * R2", D_BA, kappa_p * R2)
    ck.mat_eq("(ii) d/dc (P^J_AB - P^J_BA)|_0 = kappa R1 - kappa' R2",
              D_seq, kappa * R1 - kappa_p * R2)

    # ---- Appendix Step 3: the explicit entries of D ------------------------
    ck.eq("App. Step 3: D_00 = q0 r0 (q0-alpha)(r0-1)/Z",
          D_AB[0, 0], q0 * r0 * (q0 - alpha) * (r0 - 1) / Z)
    ck.eq("App. Step 3: D_01 = -D_00", D_AB[0, 1], -D_AB[0, 0])
    ck.eq("App. Step 3: D_10 = -r0 (q0-alpha)(q0-1)(r0-1)/Z",
          D_AB[1, 0], -r0 * (q0 - alpha) * (q0 - 1) * (r0 - 1) / Z)
    ck.eq("App. Step 3: D_11 = -D_10", D_AB[1, 1], -D_AB[1, 0])
    ck.eq("App. Step 3: D = kappa q (x) (1,-1)  [A-marginal preserved: D_i0 = -D_i1]",
          sum(D_AB[i, j] for i in range(2) for j in range(2)), 0)
    for i in range(2):
        ck.eq(f"A-marginal of D vanishes in row {i}: D_{i}0 + D_{i}1 = 0",
              D_AB[i, 0] + D_AB[i, 1], 0)

    # mirror: on route BA the B-marginal of the leading gap vanishes
    for j in range(2):
        ck.eq(f"B-marginal of D' vanishes in column {j}: D'_0{j} + D'_1{j} = 0",
              D_BA[0, j] + D_BA[1, j], 0)

    # ---- Step 4: kappa vanishes exactly on the stated locus ----------------
    ck.eq("App. Step 4: kappa|_{q0=alpha} = 0", kappa.subs(q0, alpha), 0)
    ck.eq("App. Step 4: kappa|_{r0=0} = 0", kappa.subs(r0, 0), 0)
    ck.eq("App. Step 4: kappa|_{r0=1} = 0", kappa.subs(r0, 1), 0)
    ck.ne("App. Step 4: kappa != 0 generically (so D not identically 0)", kappa)
    ck.eq("kappa' |_{r0=beta} = 0", kappa_p.subs(r0, beta), 0)
    ck.ne("kappa' != 0 generically", kappa_p)
    # kappa factors as (surprise in the A-impression) x (spread of the B-impression)
    ck.eq("kappa = (alpha-q0) * r0(1-r0) / Z exactly",
          kappa, (alpha - q0) * r0 * (1 - r0) / Z)

    # ---- Step 5: the sequence effect --------------------------------------
    ck.eq("App. Step 5: Delta_seq,00 = q0 r0 [(alpha-beta) + r0(1-alpha) - q0(1-beta)]/Z",
          D_seq[0, 0],
          q0 * r0 * ((alpha - beta) + r0 * (1 - alpha) - q0 * (1 - beta)) / Z)
    ck.eq("App. Step 5: sum of Delta_seq entries = 0",
          sum(D_seq[i, j] for i in range(2) for j in range(2)), 0)

    # on the symmetry locus {alpha=beta, q0=r0} the diagonal coefficient dies ...
    sym = [(beta, alpha), (r0, q0)]
    ck.eq("App. Step 5: Delta_seq,00 = 0 on {alpha=beta, q0=r0}",
          D_seq[0, 0].subs(sym), 0)
    # ... and the effect survives in the antisymmetric off-diagonal part
    ck.eq("App. Step 5: Delta_seq,01 = q0(1-q0)(q0-alpha)/(alpha^2 (1-alpha)^2) on the locus",
          sp.cancel(D_seq[0, 1].subs(sym)),
          q0 * (1 - q0) * (q0 - alpha) / (alpha**2 * (1 - alpha)**2))
    ck.eq("App. Step 5: Delta_seq,01 = -Delta_seq,10 on the locus",
          D_seq[0, 1].subs(sym), -D_seq[1, 0].subs(sym))
    ck.ne("App. Step 5: Delta_seq,01 != 0 on the locus unless q0 = alpha",
          D_seq[0, 1].subs(sym))
    ck.eq("App. Step 5: Delta_seq,01 = 0 on the locus when q0 = alpha",
          D_seq[0, 1].subs(sym).subs(q0, alpha), 0)

    # ---- (ii) R1 and R2 are linearly independent ---------------------------
    # a R1 + b R2 = 0  =>  a = b = 0 for q0, r0 in (0,1)
    aa, bb = sp.symbols('a_ b_')
    sol = sp.solve([sp.Eq((aa * R1 + bb * R2)[i, j], 0)
                    for i in range(2) for j in range(2)], [aa, bb], dict=True)
    ck("(ii) R1, R2 linearly independent (only trivial combination vanishes)",
       sol in ([{aa: 0, bb: 0}], [{bb: 0, aa: 0}]), f"solutions: {sol}")

    # hence Delta_seq == 0 iff kappa = kappa' = 0
    ck.ne("(ii) Delta_seq not identically zero", D_seq[0, 0])

    # ---- the gap is genuinely Theta(c): leading order exactly 1 -----------
    for nm, G in [("P^J_AB - P^B", gap_AB), ("P^J_BA - P^B", gap_BA),
                  ("P^J_AB - P^J_BA", seq)]:
        k, co = order_in_c_at_generic(G[0, 0])
        ck(f"{nm} is Theta(c) generically (entry (0,0) has leading order 1)",
           k == 1, f"leading order = {k}, coefficient = {co}")

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
