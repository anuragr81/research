"""
Becker (1962), "Irrational Behavior and Economic Theory", JPE 70(1), 1-13.
Source: web PDF of the JSTOR scan (not Drive). Pages are journal pages.

Checks, exact / symbolic unless stated. Exit status 0 iff all pass.
  (1) Impulsive households (pp. 5-6, fn. 10, fn. 15): uniform choice on the
      budget line has mean (I/2p1, I/2p2); the compensated rotation through
      that point (income fixed, p1 -> (1+t) p1) forces p2 -> (1-t) p2; the new
      midpoint is to the left and above; demand I/(2 p1) is unit-elastic
      (fn. 15), and fn. 15's compensated form X = k' P/Px.
  (2) Inefficient impulsive households (p. 9, fn. 16): uniform on the
      triangle OAB, centre of gravity (I/3p1, I/3p2); the compensated change
      moves it left and up.
  (3) Market averaging (pp. 5-6): a seeded Monte Carlo of 200000 uniform
      households lands within 0.5% of I/(2 p1) at two prices, and the market
      demand falls. (Floating point; the exact statement is the strong law in
      Becker.lean.)
  (4) Inert households (pp. 6-8, fn. 12, fn. 14):
      - under a uniform initial distribution, half the households are on Ap
        with mean x0/2 = I/(4 p1), and half on pB with mean 3 x0/2;
      - fn. 14: X1 = 31/88 I/Px, change -13/44, elasticity -65/22 ~ -2.95;
      - general t: elasticity -(1+3t)/(4t(1+t)), decreasing in size with t;
        it diverges as t -> 0+ (p. 8: "A smaller price change ... would yield a
        still higher elasticity");
      - p. 6's sufficient condition ("more than OD") FAILS in fn. 14's own
        example (mean in pB = 0.75 I/Px < OD = 0.909 I/Px): the -30% there
        comes from the assumed adjustment to the new midpoint, not from
        arithmetic necessity;
      - fn. 12: the mean in pB rises with the dispersion around p.
  (5) Weighted average (p. 7): a mixture of the impulsive and inert market
      demands is decreasing in t.
"""
import random
import sys

import sympy as sp

R = sp.Rational
OK = True


def check(name, cond):
    global OK
    print(("  PASS  " if cond else "  FAIL  ") + name)
    if not cond:
        OK = False


def main():
    I, p1, p2, t, x, y, d = sp.symbols("I p1 p2 t x y d", positive=True)

    # ------------------------------------------------------------- (1)
    print("(1) impulsive households on the budget line")
    a = I / p1
    mean_x = sp.integrate(x, (x, 0, a)) / a
    yline = (I - p1 * x) / p2
    mean_y = sp.integrate(yline, (x, 0, a)) / a
    check("uniform on the line: E[x] = I/(2 p1), E[y] = I/(2 p2) (fn. 10)",
          sp.simplify(mean_x - I / (2 * p1)) == 0 and sp.simplify(mean_y - I / (2 * p2)) == 0)
    speed = sp.sqrt(1 + sp.diff(yline, x)**2)
    check("arc length is proportional to x (constant speed), so 'uniform on the line' = uniform in x",
          sp.diff(speed, x) == 0)
    q1, q2 = (1 + t) * p1, sp.symbols("q2", positive=True)
    q2sol = sp.solve(sp.Eq(q1 * I / (2 * p1) + q2 * I / (2 * p2), I), q2)[0]
    check("compensated rotation through the midpoint, income fixed: p2 -> (1 - t) p2",
          sp.simplify(q2sol - (1 - t) * p2) == 0)
    new_x, new_y = I / (2 * q1), I / (2 * q2sol)
    tv = R(1, 10)
    check("new midpoint is left and above (p. 6): x falls, y rises (t = 1/10, all I, p)",
          sp.simplify((new_x - I / (2 * p1)).subs(t, tv)) == -I / (22 * p1)
          and sp.simplify((new_y - I / (2 * p2)).subs(t, tv)) == I / (18 * p2))
    elas = sp.simplify(sp.diff(I / (2 * p1), p1) * p1 / (I / (2 * p1)))
    check("unit elasticity of I/(2 p1) (fn. 15)", elas == -1)
    P = sp.symbols("P", positive=True)
    check("fn. 15, compensated: with I/P = c, X = k I/Px = (k c) P/Px",
          sp.simplify((R(1, 2) * I / p1).subs(I, sp.Symbol("c") * P) - R(1, 2) * sp.Symbol("c") * P / p1) == 0)

    # ------------------------------------------------------------- (2)
    print("(2) inefficient impulsive households: uniform on the triangle OAB (p. 9)")
    area = sp.integrate(yline, (x, 0, a))
    cx = sp.integrate(x * yline, (x, 0, a)) / area
    cy = sp.integrate(yline**2 / 2, (x, 0, a)) / area
    check("centre of gravity (I/(3 p1), I/(3 p2))",
          sp.simplify(cx - I / (3 * p1)) == 0 and sp.simplify(cy - I / (3 * p2)) == 0)
    ncx, ncy = I / (3 * q1), I / (3 * q2sol)
    check("compensated change moves the centre left and above (fn. 16), t = 1/10",
          sp.simplify((ncx - I / (3 * p1)).subs(t, tv)) == -I / (33 * p1)
          and sp.simplify((ncy - I / (3 * p2)).subs(t, tv)) == I / (27 * p2))

    # ------------------------------------------------------------- (3)
    print("(3) market averaging (seeded Monte Carlo)")
    rng = random.Random(1962)
    Iv, pv = 1.0, 1.0
    N = 200000
    m0 = sum(rng.uniform(0, Iv / pv) for _ in range(N)) / N
    m1 = sum(rng.uniform(0, Iv / (1.1 * pv)) for _ in range(N)) / N
    print(f"    market mean x at p1 = 1: {m0:.5f} (theory 0.5); at p1 = 1.1: {m1:.5f} (theory {1/2.2:.5f})")
    check("market means within 0.5% of I/(2 p1) at both prices, and demand falls",
          abs(m0 / 0.5 - 1) < 0.005 and abs(m1 / (1 / 2.2) - 1) < 0.005 and m1 < m0)

    # ------------------------------------------------------------- (4)
    print("(4) inert households (pp. 6-8)")
    x0 = I / (2 * p1)
    frac_Ap = sp.integrate(1, (x, 0, x0)) / a
    mean_Ap = sp.integrate(x, (x, 0, x0)) / (x0)
    mean_pB = sp.integrate(x, (x, x0, a)) / (a - x0)
    check("uniform initial distribution: half on Ap, mean x0/2 = I/(4 p1); mean on pB = 3 x0/2",
          sp.simplify(frac_Ap - R(1, 2)) == 0 and sp.simplify(mean_Ap - I / (4 * p1)) == 0
          and sp.simplify(mean_pB - 3 * I / (4 * p1)) == 0)
    X1 = frac_Ap * mean_Ap + (1 - frac_Ap) * I / (2 * q1)
    check("fn. 14: X1 = 1/2 (I/4Px) + 1/2 (I/2.2Px) = 31/88 I/Px",
          sp.simplify(X1.subs(t, tv) - R(31, 88) * I / p1) == 0)
    chg = sp.simplify((X1 - x0) / x0)
    check("fn. 14: (X1 - X0)/X0 = -13/44 (Becker: '-.3', 'about 30 per cent')",
          sp.simplify(chg.subs(t, tv)) == R(-13, 44))
    el = sp.simplify(chg / t)
    check("elasticity -65/22 = -2.95... (Becker: 'a high elasticity of -3')",
          sp.simplify(el.subs(t, tv)) == R(-65, 22))
    check("general t: elasticity = -(1+3t)/(4t(1+t))",
          sp.simplify(el + (1 + 3 * t) / (4 * t * (1 + t))) == 0)
    check("its size falls with t (derivative of (1+3t)/(4t(1+t)) is -(1+2t+3t^2)/(4 t^2 (1+t)^2) < 0) "
          "and diverges as t -> 0+",
          sp.simplify(sp.diff((1 + 3 * t) / (4 * t * (1 + t)), t)
                      + (1 + 2 * t + 3 * t**2) / (4 * t**2 * (1 + t)**2)) == 0
          and sp.limit(el, t, 0, "+") == -sp.oo)
    OD = I / q1
    check("p. 6's sufficient condition fails in fn. 14's own example: mean on pB = 3/4 I/Px "
          "< OD = 10/11 I/Px", sp.simplify((OD - mean_pB).subs(t, tv)) == R(7, 44) * I / p1)
    tstar = sp.solve(sp.Eq(OD, mean_pB), t)
    check("the condition 'mean on pB > OD' needs a rise of more than 1/3 (t > 1/3)",
          tstar == [R(1, 3)])
    # fn. 12: dispersion. Households uniform on [x0 - d, x0 + d] around p
    mean_pB_d = sp.integrate(x, (x, x0, x0 + d)) / d
    check("fn. 12: with households uniform on [x0 - d, x0 + d], the mean on pB is x0 + d/2, "
          "increasing in d", sp.simplify(mean_pB_d - (x0 + d / 2)) == 0
          and sp.simplify(sp.diff(mean_pB_d, d)) == R(1, 2))

    # ------------------------------------------------------------- (5)
    print("(5) weighted average (p. 7)")
    w = sp.symbols("w", nonnegative=True)
    X_imp = I / (2 * q1)
    X_mix = w * X_imp + (1 - w) * X1
    dmix = sp.simplify(sp.diff(X_mix, t))
    check("d/dt of the mixture = -(I/(2 p1 (1+t)^2)) (w + (1-w)/2) < 0 for w in [0, 1]",
          sp.simplify(dmix + I / (2 * p1 * (1 + t)**2) * (w + (1 - w) / 2)) == 0)

    print("\nAll checks passed." if OK else "\nSOME CHECKS FAILED.")
    return OK


if __name__ == "__main__":
    sys.exit(0 if main() else 1)
