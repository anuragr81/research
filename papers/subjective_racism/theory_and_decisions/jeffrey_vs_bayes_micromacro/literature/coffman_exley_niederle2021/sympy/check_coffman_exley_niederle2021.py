"""
Coffman, Exley & Niederle (2021), "The Role of Beliefs in Driving Gender
Discrimination", Management Science 67(6), 3551-3569 (read as the HBS working
paper of 28 February 2020; p.N is its printed page).

Checks, in exact arithmetic:
  (1) Coding (fn 17, p.16): with no discrimination either way the expected
      code is 1/2 for every share of chance choices.
  (2) Hiring rates (p.16) below 1/2; their difference against Table 1 col 1.
  (3) Fig. 1 (p.9) easy-quiz gaps against fn 13's actual easy gaps; fn 13's
      believed gaps have the right sign and exceed the actual ones.
  (4) In-group beliefs (p.23) and hiring (p.22); Information Stage (p.25).
  (5) Design counts (pp.5, 8, 10-12, 27).
  (6) Paper B's reading, not the paper's claim: with a 0/1 score the believed
      gap between groups equals the conditional difference P(B=1|A=1) - P(B=1|A=0).
Exit status 0 iff every check passes.
"""
from fractions import Fraction as F

import sympy as sp

OK = True


def check(name, cond):
    global OK
    print(("  PASS  " if cond else "  FAIL  ") + name)
    if not cond:
        OK = False


def main():
    print("(1) Coding of decisions")
    p = sp.Symbol("p")
    code = {"female": 1, "male": 0, "chance": sp.Rational(1, 2)}
    check("symmetric choice gives expected code 1/2 for every p",
          sp.simplify(p * code["female"] + p * code["male"] + (1 - 2 * p) * code["chance"] - sp.Rational(1, 2)) == 0)

    print("(2) Hiring rates")
    g, b = F(43, 100), F(37, 100)
    check("43% and 37% are both below 1/2", g < F(1, 2) and b < F(1, 2))
    check("43% - 37% is within 0.01 of Table 1's 0.061", abs(g - b - F(61, 1000)) <= F(1, 100))

    print("(3) Information shown and beliefs held")
    check("math: 3.96 - 3.27 within 0.01 of fn 13's 0.692", abs(F(396, 100) - F(327, 100) - F(692, 1000)) <= F(1, 100))
    check("sports: 5.50 - 4.50 = 1.0 (fn 13)", F(550, 100) - F(450, 100) == 1)
    pairs = {"sports hard": (F(276, 100), F(11, 10)), "sports easy": (F(262, 100), F(1)),
             "math hard": (F(154, 100), F(451, 1000)), "math easy": (F(107, 100), F(692, 1000))}
    for k, (belief, actual) in pairs.items():
        check(f"fn 13 {k}: actual gap > 0 and believed gap {belief} > actual {actual}", 0 < actual < belief)

    print("(4) In-group and Information Stage")
    check("in-group employers believe in a smaller gap (1.16 < 2.39, 1.91 < 2.48)",
          F(116, 100) < F(239, 100) and F(191, 100) < F(248, 100))
    check("in-group hiring 41% > out-group 32%", F(41, 100) > F(32, 100))
    check("Information Stage: 29% < 33% < 50%", F(29, 100) < F(33, 100) < F(1, 2))

    print("(5) Design counts")
    check("54 decisions = 6 screens x 9; 9 = 3 equal + 6 female-better", 6 * 9 == 54 and 3 + 6 == 9)
    risk = [99, 95, 90, 75, 50]
    check("five strictly falling risk levels on screens 2-6", len(risk) == 5 and all(x > y for x, y in zip(risk, risk[1:])))
    check("12 information screens = 11 subsets + full", 11 + 1 == 12)
    check("pools 25 + 25; 2 x 400 employers; 2000 - 5 = 1995", 25 + 25 == 50 and 2 * 400 == 800 and 2000 - 5 == 1995)

    print("(6) Paper B's reading")
    q00, q01, q10, q11 = sp.symbols("q00 q01 q10 q11", positive=True)   # cells (group, high score)
    mean1 = (q11 * 1 + q10 * 0) / (q11 + q10)
    mean0 = (q01 * 1 + q00 * 0) / (q01 + q00)
    cond_diff = q11 / (q10 + q11) - q01 / (q00 + q01)
    check("believed gap of 0/1 means = P(B=1|A=1) - P(B=1|A=0)", sp.simplify(mean1 - mean0 - cond_diff) == 0)
    return OK


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
