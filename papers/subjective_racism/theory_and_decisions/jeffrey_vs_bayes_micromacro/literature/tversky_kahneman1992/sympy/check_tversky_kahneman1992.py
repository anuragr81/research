"""
Tversky & Kahneman (1992), "Advances in prospect theory: Cumulative
representation of uncertainty", J. Risk Uncertainty 5, 297-323 (journal pages).

Checks:
  (1) Decision weights (p.301), symbolic in an arbitrary weighting function w:
      for a positive prospect they telescope to w(1) - w(0) = 1; with w the
      identity they are the probabilities; the die example's gain weights.
  (2) Rank dependence: with w(p) = p^2 and three equiprobable gains the top
      gain gets 1/9 and the middle one 1/3.
  (3) Mixed prospects: w+(1/2) + w-(1/2) below 1 for w = p^2, above 1 for
      w = 2p - p^2 (p.301).
  (4) The weighting function (6) (p.309): w(0) = 0, w(1) = 1; at the median
      estimates (p.312) w+(.5) and w-(.5) are below .5, and gamma < delta.
  (5) Table 6 (p.312): the tabulated theta is (x - b)/(a - c) to two decimals
      in all eight rows; the printed definition (x - b)/(c - a) gives the
      negatives.
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


def gain_weights(w, ps):
    """pi_i = w(p_i + ... + p_n) - w(p_{i+1} + ... + p_n), ranks 0..n."""
    n = len(ps) - 1
    tail = [sum(ps[i:], sp.Integer(0)) for i in range(n + 2)]
    return [w(tail[i]) - w(tail[i + 1]) for i in range(n + 1)]


def main():
    print("(1) Decision weights")
    W = sp.Function("w")
    p = sp.symbols("p0:4", positive=True)
    pis = gain_weights(W, list(p))
    total = sp.expand(sum(pis))
    check("weights telescope to w(p0+...+p3) - w(0)", sp.simplify(total - (W(sum(p)) - W(0))) == 0)
    pis_id = gain_weights(lambda x: x, list(p))
    check("additive w: pi_i = p_i", all(sp.simplify(a - b) == 0 for a, b in zip(pis_id, p)))
    die = gain_weights(W, [sp.Rational(1, 2), sp.Rational(1, 6), sp.Rational(1, 6), sp.Rational(1, 6)])
    check("die example: weights of 2, 4, 6 are w(1/2)-w(1/3), w(1/3)-w(1/6), w(1/6)-w(0)",
          [sp.simplify(d) for d in die[1:]] ==
          [W(sp.Rational(1, 2)) - W(sp.Rational(1, 3)), W(sp.Rational(1, 3)) - W(sp.Rational(1, 6)),
           W(sp.Rational(1, 6)) - W(0)])

    print("(2) Rank dependence")
    sq = gain_weights(lambda x: x**2, [sp.Rational(1, 3)] * 3)
    check("w = p^2, three equiprobable gains: top gets 1/9, middle 1/3, bottom 5/9",
          sq[2] == sp.Rational(1, 9) and sq[1] == sp.Rational(1, 3) and sq[0] == sp.Rational(5, 9))

    print("(3) Mixed prospects")
    h = sp.Rational(1, 2)
    check("w = p^2: w+(1/2) + w-(1/2) = 1/2 < 1", h**2 + h**2 == sp.Rational(1, 2))
    check("w = 2p - p^2: w+(1/2) + w-(1/2) = 3/2 > 1", 2 * (2 * h - h**2) == sp.Rational(3, 2))

    print("(4) The weighting function (6) and the median estimates")
    g = sp.Symbol("gamma", positive=True)
    wTK = lambda x, gg: x**gg / (x**gg + (1 - x)**gg)**(1 / gg)
    check("w(0) = 0 for gamma > 0", sp.simplify(wTK(sp.Integer(0), g)) == 0)
    check("w(1) = 1 for gamma > 0", sp.simplify(wTK(sp.Integer(1), g)) == 1)
    gam, dlt = sp.Rational(61, 100), sp.Rational(69, 100)
    check("median gamma = .61 < delta = .69", gam < dlt)
    wp, wm = sp.N(wTK(h, gam), 30), sp.N(wTK(h, dlt), 30)
    check(f"w+(.5) = {sp.N(wp, 6)} < .5 and w-(.5) = {sp.N(wm, 6)} < .5 at the medians", wp < h and wm < h)

    print("(5) Table 6")
    rows = [(0, 0, -25, 61, "2.44"), (0, 0, -50, 101, "2.02"), (0, 0, -100, 202, "2.02"),
            (0, 0, -150, 280, "1.87"), (-20, 50, -50, 112, "2.07"), (-50, 150, -125, 301, "2.01"),
            (50, 120, 20, 149, "0.97"), (100, 300, 25, 401, "1.35")]
    tab = [abs(F(x - b, a - c) - F(th)) <= F(1, 200) for a, b, c, x, th in rows]
    neg = [abs(F(x - b, c - a) + F(th)) <= F(1, 200) for a, b, c, x, th in rows]
    check("tabulated theta = (x - b)/(a - c) to two decimals, all eight rows", all(tab))
    check("printed theta = (x - b)/(c - a) is the negative in all eight rows", all(neg))
    return OK


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
