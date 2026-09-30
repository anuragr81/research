"""
Tao (2011), An Introduction to Measure Theory -- the two results Paper B's
proof of Theorem LOS (Appendix A.3) takes from it, checked on explicit
distributions. Exact / symbolic arithmetic; exit status 0 iff all pass.

  (1) Exercise 1.4.23(iii), "continuity from above", as Step 4 uses it:
      for the band B_c = {u : 0 < |u| <= cM}, mu(B_c) -> 0 as c -> 0+ for
        (a) u ~ N(0,1);
        (b) u with an atom of mass 1/2 at 0 plus 1/2 N(0,1) (the atom is in no
            band, as the manuscript says);
        (c) u ~ Uniform[-1, 1].
      And the finiteness hypothesis is needed: E_n = [n, oo) under Lebesgue
      measure has infinite measure for every n and empty intersection.
  (2) Tonelli (Thm 1.7.15): both iterated integrals of a nonnegative joint
      density agree, and integrating 1_B(u) g(u, delta) first in u then in
      delta gives the u-marginal mass of B (Step 3's identity).
  (3) The manuscript's Steps 3-5 on a concrete population: u ~ N(0,1) and
      delta ~ Uniform[-1,1] independent (M = 1). The flip set is u strictly
      between 0 and -c delta. Exact L(c), the bound L(c) <= c M mu(B_c),
      L(c)/c -> 0, and Step 5's constant: L(c) = f(0)/2 E[delta^2] c^2 + O(c^3)
      (in fact + O(c^4)).
"""
import sys

import sympy as sp

OK = True


def check(name, cond):
    global OK
    print(("  PASS  " if cond else "  FAIL  ") + name)
    if not cond:
        OK = False


def main():
    c, u, d, t = sp.symbols("c u delta t", real=True)
    cp = sp.symbols("c", positive=True)
    phi = sp.exp(-u**2 / 2) / sp.sqrt(2 * sp.pi)
    M = 1

    print("(1) Exercise 1.4.23(iii) for the band B_c = {0 < |u| <= cM}")
    band_normal = 2 * sp.integrate(phi, (u, 0, cp * M))
    check("N(0,1): mu(B_c) = erf(c/sqrt 2) -> 0 as c -> 0+",
          sp.simplify(band_normal - sp.erf(cp / sp.sqrt(2))) == 0
          and sp.limit(band_normal, cp, 0, "+") == 0)
    band_atom = sp.Rational(1, 2) * band_normal  # the atom at 0 is excluded from every band
    check("atom 1/2 at 0 + 1/2 N(0,1): mu(B_c) -> 0 (atom in no band), although mu({|u| <= c}) -> 1/2",
          sp.limit(band_atom, cp, 0, "+") == 0
          and sp.limit(sp.Rational(1, 2) + band_atom, cp, 0, "+") == sp.Rational(1, 2))
    band_unif = sp.Piecewise((cp * M, cp * M <= 1), (1, True))
    check("Uniform[-1,1]: mu(B_c) = min(cM, 1) -> 0",
          band_unif.subs(cp, sp.Rational(1, 10**6)) == sp.Rational(1, 10**6)
          and band_unif.subs(cp, 3) == 1 and sp.limit(cp * M, cp, 0, "+") == 0)
    n = sp.symbols("n", positive=True, integer=True)
    check("E_n = [n, oo): Lebesgue measure oo for every n (finiteness is needed)",
          sp.integrate(1, (u, n, sp.oo)) == sp.oo)

    print("(2) Tonelli (Thm 1.7.15) and Step 3's marginal identity")
    # a correlated joint density on R x [-1, 1]; nonnegative since |u e^{-u^2/2}| <= e^{-1/2} < 1
    g = phi * (1 + d * u * sp.exp(-u**2 / 2)) / 2
    I_ud = sp.integrate(sp.integrate(g, (d, -1, 1)), (u, -sp.oo, sp.oo))
    I_du = sp.integrate(sp.integrate(g, (u, -sp.oo, sp.oo)), (d, -1, 1))
    check("both iterated integrals of a nonnegative joint density equal 1",
          sp.simplify(I_ud - 1) == 0 and sp.simplify(I_du - 1) == 0)
    a, b = sp.Rational(1, 5), sp.Rational(7, 10)
    inner_u_first = sp.integrate(sp.integrate(g, (u, a, b)), (d, -1, 1))
    marginal = sp.integrate(g, (d, -1, 1))
    check("int_delta int_u 1_B(u) g du d delta = int_B f_u(u) du  (B = (1/5, 7/10])",
          sp.simplify(inner_u_first - sp.integrate(marginal, (u, a, b))) == 0)

    print("(3) Theorem LOS Steps 3-5, u ~ N(0,1), delta ~ U[-1,1] independent, M = 1")
    # flip: u strictly between 0 and -c delta; weight |u|; joint density phi(u)/2
    # by symmetry of phi, the inner integral is int_0^{c|delta|} t phi(t) dt
    inner = sp.integrate(t * phi.subs(u, t), (t, 0, cp * sp.Abs(d)))
    L = sp.integrate(sp.simplify(inner.subs(sp.Abs(d), d)), (d, 0, 1))  # (1/2) * 2 * int_0^1
    L = sp.simplify(L)
    print("    L(c) =", L)
    f0 = phi.subs(u, 0)
    Ed2 = sp.integrate(d**2 / 2, (d, -1, 1))
    ser = sp.series(L, cp, 0, 5).removeO()
    check("Step 5: L(c) = f(0)/2 E[delta^2] c^2 + O(c^4), f(0)/2 E[delta^2] = 1/(6 sqrt(2 pi))",
          sp.simplify(ser.coeff(cp, 2) - f0 / 2 * Ed2) == 0
          and sp.simplify(f0 / 2 * Ed2 - 1 / (6 * sp.sqrt(2 * sp.pi))) == 0
          and ser.coeff(cp, 3) == 0)
    check("Step 4: L(c)/c -> 0", sp.limit(L / cp, cp, 0, "+") == 0)
    ok_bound = True
    for cv in [sp.Rational(1, 100), sp.Rational(1, 10), sp.Rational(1, 2), 1, 2]:
        lv = sp.N(L.subs(cp, cv), 30)
        bv = sp.N(cv * M * band_normal.subs(cp, cv), 30)
        if not (0 <= lv <= bv):
            ok_bound = False
        print(f"    c = {str(cv):5s}  L(c) = {float(lv):.3e}   c M mu(B_c) = {float(bv):.3e}")
    check("Step 3 bound 0 <= L(c) <= c M mu(B_c) at c in {1/100, 1/10, 1/2, 1, 2}", ok_bound)
    share = sp.integrate(sp.integrate(phi, (u, 0, cp * d)), (d, 0, 1))  # P(flip)
    check("the flip share is first order: P(flip) = f(0) E|delta| c + O(c^3)",
          sp.simplify(sp.series(share, cp, 0, 3).removeO().coeff(cp, 1) - f0 / 2) == 0)

    print("\nAll checks passed." if OK else "\nSOME CHECKS FAILED.")
    return OK


if __name__ == "__main__":
    sys.exit(0 if main() else 1)
