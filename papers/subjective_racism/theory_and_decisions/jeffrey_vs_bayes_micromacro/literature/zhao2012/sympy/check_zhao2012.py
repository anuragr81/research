"""
Zhao, Crupi, Tentori, Fitelson & Osherson (2012), "Updating: Learning versus
supposing", Cognition 124, 373-378.

Checks, in exact arithmetic:
  (1) Eq. (1) (p.373): conditioning on a learned B gives a distribution with
      Pr2(B) = 1 and Pr2(A) = Pr1(A|B); footnote 1: once Pr2(B) = 1,
      Pr2(A|B) = Pr2(A), so violating (1) is failing invariance of Pr(A|B).
  (2) Design: five judgments per participant (100 estimates per condition from
      20 participants, Fig. 1); Exp 3's three disjoint groups of 20 (60 in all);
      the 20 swing states of fn 4.
  (3) Decks (Tables 1 and 3): 20 cards each; Experiment 2's objective
      conditionals are more extreme (p.375).
  (4) Experiment 3 (Table 5 and p.377): learn 0.64, suppose 0.53, control 0.51,
      and the comparisons the project states; consistent/inconsistent pairs;
      quadratic penalties; 0.5 guarantees 0.25.
  (5) Binomial tests (pp.374, 377), recomputed exactly (two-sided, null 1/2).
  (6) A consistency note (printed, assumption stated): whether 0.64 and 0.53
      are compatible with 60% consistent pairs at 0.72/0.50 and 0.55/0.48.
Exit status 0 iff every check passes.
"""
from fractions import Fraction as F
from math import comb

import sympy as sp

OK = True


def check(name, cond):
    global OK
    print(("  PASS  " if cond else "  FAIL  ") + name)
    if not cond:
        OK = False


def binom_two_sided(k, n):
    pk = F(comb(n, k), 2 ** n)
    return sum(F(comb(n, j), 2 ** n) for j in range(n + 1) if F(comb(n, j), 2 ** n) <= pk)


def main():
    print("(1) Eq. (1) and footnote 1")
    a, b, c, d = sp.symbols("a b c d", positive=True)   # cells (B,A), (B,~A), (~B,A), (~B,~A)
    pB = a + b
    L = {"BA": a / pB, "BnA": b / pB, "nBA": 0, "nBnA": 0}
    check("Pr2 = Pr1(.|B) sums to 1", sp.simplify(sum(L.values()) - 1) == 0)
    check("Pr2(B) = 1", sp.simplify(L["BA"] + L["BnA"] - 1) == 0)
    check("eq. (1): Pr2(A) = Pr1(A|B)", sp.simplify((L["BA"] + L["nBA"]) - a / pB) == 0)
    check("updating by (1) keeps Pr(A|B)", sp.simplify(L["BA"] / (L["BA"] + L["BnA"]) - a / pB) == 0)
    # footnote 1: any Q with Q(B) = 1 has Q(~B, .) = 0, so Q(A|B) = Q(A)
    x, y = sp.symbols("x y", nonnegative=True)
    Q = {"BA": x, "BnA": 1 - x, "nBA": 0, "nBnA": 0}
    check("fn 1: Q(B) = 1 implies Q(A|B) = Q(A)",
          sp.simplify(Q["BA"] / (Q["BA"] + Q["BnA"]) - (Q["BA"] + Q["nBA"])) == 0)

    print("(2) Design")
    check("five judgments per participant: 100 estimates / 20 participants (Fig. 1)", F(100, 20) == 5)
    check("Exp 1 and Exp 2: 40 = 20 learn + 20 suppose", 20 + 20 == 40)
    check("Exp 3: 60 = 20 learn + 20 suppose + 20 control (three disjoint groups)",
          20 + 20 + 20 == 60 and 20 + 20 != 60)
    swing = "AL AZ GA ID IN KS KY LA MI MN MO MS MT NC ND NM OH TN VA WV".split()
    check("fn 4: 20 distinct swing states", len(swing) == 20 == len(set(swing)))

    print("(3) Decks")
    T1 = [(5, 4, 6, 5), (9, 2, 6, 3), (4, 8, 2, 6), (7, 8, 2, 3), (3, 3, 6, 8)]
    T3 = [(9, 2, 1, 8), (2, 8, 9, 1), (7, 1, 3, 9), (3, 8, 7, 2), (8, 3, 2, 7)]
    check("Table 1: five decks of 20", len(T1) == 5 and all(sum(t) == 20 for t in T1))
    check("Table 3: five decks of 20", len(T3) == 5 and all(sum(t) == 20 for t in T3))

    def devs(t):
        gd, gk, yd, yk = t
        return [abs(F(gd, gd + gk) - F(1, 2)), abs(F(yd, yd + yk) - F(1, 2)),
                abs(F(gd, gd + yd) - F(1, 2)), abs(F(gk, gk + yk) - F(1, 2))]
    m1 = sum(sum(devs(t)) for t in T1) / 20
    m3 = sum(sum(devs(t)) for t in T3) / 20
    check(f"Exp 2 objective conditionals more extreme: mean |Pr(A|B) - .5| {float(m1):.3f} vs "
          f"{float(m3):.3f}", m1 < F("0.14") and m3 > F("0.30"))
    check("Tables 2/4 row (b): suppose more extreme in Exp 1 (.19 > .14), learn in Exp 2 (.23 > .18)",
          F("0.19") > F("0.14") and F("0.23") > F("0.18"))

    print("(4) Experiment 3")
    learn, suppose, control = F("0.64"), F("0.53"), F("0.51")
    check("control 0.51 < suppose 0.53 < learn 0.64", control < suppose < learn)
    check("learn - suppose = 0.11, suppose - control = 0.02, learn - control = 0.13",
          (learn - suppose, suppose - control, learn - control) == (F("0.11"), F("0.02"), F("0.13")))
    check("interaction direction: (0.72 - 0.50) - (0.55 - 0.48) = 0.15 > 0",
          (F("0.72") - F("0.50")) - (F("0.55") - F("0.48")) == F("0.15"))
    check("issuing 0.5 guarantees quadratic penalty 0.25 for either outcome",
          (1 - F(1, 2)) ** 2 == (0 - F(1, 2)) ** 2 == F("0.25"))
    check("penalties: learn 0.18 < suppose 0.25 < control 0.29",
          F("0.18") < F("0.25") < F("0.29"))

    print("(5) Binomial tests (two-sided, exact)")
    p16 = binom_two_sided(16, 20)
    check(f"Exp 1: 16 of 20 pairs, printed p = .01: exact {float(p16):.4f}",
          abs(p16 - F(1, 100)) < F(5, 1000))
    p17 = binom_two_sided(17, 20)
    check(f"Exp 3: 17 of 20 pairs, printed p = .01: exact {float(p17):.4f} (<= .01; "
          "would round to .00)", p17 <= F(1, 100))

    print("(6) Consistency note (assumes the 60% share holds per participant, so that the "
          "group means are 0.6 x consistent + 0.4 x inconsistent)")
    for tag, mean, cons, inc in (("learn", learn, F("0.72"), F("0.50")),
                                 ("suppose", suppose, F("0.55"), F("0.48"))):
        lo = F(6, 10) * (cons - F(5, 1000)) + F(4, 10) * (inc - F(5, 1000))
        hi = F(6, 10) * (cons + F(5, 1000)) + F(4, 10) * (inc + F(5, 1000))
        overlap = max(lo, mean - F(5, 1000)) <= min(hi, mean + F(5, 1000))
        print(f"    {tag}: mixture in [{float(lo):.4f}, {float(hi):.4f}], printed {float(mean)} "
              f"-> {'compatible (at the edge)' if overlap else 'INCOMPATIBLE'}")

    print("\nAll checks passed." if OK else "\nSOME CHECKS FAILED.")
    return OK


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
