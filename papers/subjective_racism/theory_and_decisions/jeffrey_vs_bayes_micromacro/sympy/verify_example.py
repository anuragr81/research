"""
Worked example for the Setup section (plan Change 19): every number printed in
the manuscript's example is recomputed here in exact rationals and compared
with the rounded value the text states.
"""
import sympy as sp
from jeffrey_core import *

R = sp.Rational
VAL = {alpha: R(1, 2), beta: R(1, 2), c: R(1, 20), q0: R(1, 5), r0: R(7, 10)}


def num(M):
    return M.subs(VAL).applyfunc(sp.nsimplify)


def main():
    ck = Check("Worked example (Setup) -- alpha=beta=1/2, c=1/20, q0=1/5, r0=7/10")
    P = num(prior())
    ck.mat_eq("prior table", P, sp.Matrix([[R(3, 10), R(1, 5)], [R(1, 5), R(3, 10)]]))

    PA = num(jeffrey_A(prior()))
    ck.mat_eq("after the credential alone", PA,
              sp.Matrix([[R(3, 25), R(2, 25)], [R(8, 25), R(12, 25)]]))
    ck.eq("credential alone moves the B-marginal: P^A(B=0) = 11/25 = 0.44",
          marg_B(PA)[0], R(11, 25))

    PJ_AB, PJ_BA, PBm = [num(X) for X in posteriors()]
    a_ab, a_ba, a_b = marg_A(PJ_AB)[0], marg_A(PJ_BA)[0], marg_A(PBm)[0]
    b_ab, b_ba, b_b = marg_B(PJ_AB)[0], marg_B(PJ_BA)[0], marg_B(PBm)[0]
    ck.eq("AB: last-read marginal pinned, P_AB(B=0) = 7/10", b_ab, R(7, 10))
    ck.eq("BA: last-read marginal pinned, P_BA(A=0) = 1/5", a_ba, R(1, 5))
    stated = {"P_AB(A=0)": (a_ab, "0.234"), "P_BA(B=0)": (b_ba, "0.643"),
              "P_B(A=0)": (a_b, "0.227"), "P_B(B=0)": (b_b, "0.647"),
              "assoc AB": (assoc(PJ_AB), "0.0273"), "assoc BA": (assoc(PJ_BA), "0.0271"),
              "assoc B": (assoc(PBm), "0.0297")}
    for name, (val, txt) in stated.items():
        ck(f"{name} = {val} rounds to {txt}",
           abs(float(val) - float(txt)) <= 0.5 * 10 ** (-len(txt.split('.')[1])),
           f"exact {val} = {float(val):.6f}")
    ck("sequence effect: 0.034 on the A-marginal, 0.057 on the B-marginal, 0.0002 on the association",
       abs(float(a_ab - a_ba) - 0.034) < 5e-4 and abs(float(b_ab - b_ba) - 0.057) < 5e-4
       and abs(float(assoc(PJ_AB) - assoc(PJ_BA)) - 0.0002) < 5e-5,
       f"{float(a_ab-a_ba):.5f}, {float(b_ab-b_ba):.5f}, {float(assoc(PJ_AB)-assoc(PJ_BA)):.6f}")
    ck("gap from the benchmark in sequence AB: 0.053 on the last-read B-marginal, "
       "0.007 on the first-read A-marginal, 0.002 on the association",
       abs(float(b_ab - b_b) - 0.053) < 5e-4 and abs(float(a_ab - a_b) - 0.007) < 5e-4
       and abs(float(assoc(PBm) - assoc(PJ_AB)) - 0.002) < 5e-4,
       f"{float(b_ab-b_b):.5f}, {float(a_ab-a_b):.5f}, {float(assoc(PBm)-assoc(PJ_AB)):.5f}")
    ck("the marginal sequence effects exceed the association's by a factor above 150",
       float(a_ab - a_ba) / float(assoc(PJ_AB) - assoc(PJ_BA)) > 150,
       f"ratio = {float(a_ab-a_ba)/float(assoc(PJ_AB)-assoc(PJ_BA)):.0f}")
    orr = [sp.nsimplify(odds_ratio(X)) for X in (P, PJ_AB, PJ_BA, PBm)]
    ck("odds ratio is 9/4 for the prior, both sequences and the benchmark",
       all(o == R(9, 4) for o in orr), f"{orr}")
    # the adoption weight read back from three ratings (omega = 1/2)
    from check_zero_slope_identification import damped_route_AB
    Qh = num(damped_route_AB(R(1, 2)))
    mid, fin, r1v = marg_B(PA)[1], marg_B(Qh)[1], 1 - VAL[r0]
    ck("omega = 1/2: final trustworthiness rating is 0.43 (between 0.56 and 0.30)",
       (mid, fin, r1v) == (R(14, 25), R(43, 100), R(3, 10)), f"{mid}, {fin}, {r1v}")
    ck.eq("...and (0.56 - 0.43)/(0.56 - 0.30) returns omega = 1/2", (mid - fin) / (mid - r1v), R(1, 2))
    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
