"""
Zhao & Osherson (2010), "Updating beliefs in light of uncertain evidence:
Descriptive assessment of Jeffrey's rule", Thinking & Reasoning 16(4), 288-307.

Checks, in exact arithmetic (sympy / Fraction):
  (1) Jeffrey's rule (their eq. 4) on the partition {B, not-B}: it sets
      Pr2(B) = q, satisfies invariance (eq. 3) for Pr(G|B) and Pr(G|~B), and
      is the unique distribution with Pr2(B) = q that does ("substituting (3)
      into (2) yields (4)", p.290); q = 1 is simple updating (eq. 1).
  (2) The converse Pr(B|G) is not invariant: its odds move by the odds factor
      of B, so it is invariant iff Pr2(B) = Pr1(B) (given positive
      likelihoods); at independence it equals q.
  (3) Pearl's criterion (eq. 5, p.291): conditioning on an experience e with
      G independent of e given B (and given not-B) is the Jeffrey update with
      q = Pr1(B|e).
  (4) The explicit instance on ZO's deck (Table 1, p.292): the Objective row of
      Table 2 (p.294), and the update Pr(blue) 0.24 -> 0.75.
  (5) The rain example (p.290).
  (6) The reported counts and averages (pp.294-305), including one printed
      average (18.45%, p.304) that does not reproduce from the reported means.
  (7) The binomial tests (p.295, p.296, p.303), recomputed exactly (two-sided,
      null 1/2).
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


def jeffrey(P, q):
    """P: dict (b, g) -> weight; Jeffrey update of {B, ~B} to Pr2(B) = q."""
    pB = P[(1, 1)] + P[(1, 0)]
    pN = P[(0, 1)] + P[(0, 0)]
    return {(b, g): (q * P[(b, g)] / pB if b else (1 - q) * P[(b, g)] / pN)
            for b in (0, 1) for g in (0, 1)}


def prB(P): return P[(1, 1)] + P[(1, 0)]
def prNB(P): return P[(0, 1)] + P[(0, 0)]
def prG(P): return P[(1, 1)] + P[(0, 1)]
def prNG(P): return P[(1, 0)] + P[(0, 0)]
def gB(P): return P[(1, 1)] / prB(P)
def gNB(P): return P[(0, 1)] / prNB(P)
def bG(P): return P[(1, 1)] / prG(P)
def bNG(P): return P[(1, 0)] / prNG(P)


def binom_two_sided(k, n):
    """Exact two-sided binomial test, null 1/2 (sum of outcomes no more likely
    than k)."""
    pk = F(comb(n, k), 2 ** n)
    return sum(F(comb(n, j), 2 ** n) for j in range(n + 1) if F(comb(n, j), 2 ** n) <= pk)


def rnd2(x):
    """Round half up to two decimals."""
    return F(int(F(x) * 100 + F(1, 2)), 100)


def main():
    a, b, c, d, q = sp.symbols("a b c d q", positive=True)
    P = {(1, 1): a, (1, 0): b, (0, 1): c, (0, 0): d}
    J = jeffrey(P, q)

    print("(1) Jeffrey's rule, invariance, uniqueness, simple updating")
    check("Pr2(B) = q", sp.simplify(prB(J) - q) == 0)
    check("Pr2(~B) = 1 - q", sp.simplify(prNB(J) - (1 - q)) == 0)
    check("invariance: Pr2(G|B) = Pr1(G|B)", sp.simplify(gB(J) - gB(P)) == 0)
    check("invariance: Pr2(G|~B) = Pr1(G|~B)", sp.simplify(gNB(J) - gNB(P)) == 0)
    check("eq. (4): Pr2(G) = Pr1(G|B) q + Pr1(G|~B) (1 - q)",
          sp.simplify(prG(J) - (gB(P) * q + gNB(P) * (1 - q))) == 0)
    # uniqueness: Q with Q(B) = q, Q(~B) = 1 - q and invariance is J
    x11, x10, x01, x00 = sp.symbols("x11 x10 x01 x00")
    sol = sp.solve([x11 + x10 - q, x01 + x00 - (1 - q),
                    x11 * (a + b) - a * q, x01 * (c + d) - c * (1 - q)],
                   [x11, x10, x01, x00], dict=True)
    check("invariance + Pr2(B) = q determine Pr2 uniquely, and it is (4)",
          len(sol) == 1 and all(sp.simplify(sol[0][v] - J[k]) == 0 for v, k in
                                ((x11, (1, 1)), (x10, (1, 0)), (x01, (0, 1)), (x00, (0, 0)))))
    J1 = jeffrey(P, 1)
    check("q = 1: simple updating, Pr2(G) = Pr1(G|B), Pr2(~B) = 0",
          sp.simplify(prG(J1) - gB(P)) == 0 and J1[(0, 1)] == 0 and J1[(0, 0)] == 0)

    print("(2) The converse Pr(B|G)")
    odds2 = J[(1, 1)] / J[(0, 1)]
    check("odds of B given G after = q/(1-q) * Pr1(G|B)/Pr1(G|~B)",
          sp.simplify(odds2 - q / (1 - q) * gB(P) / gNB(P)) == 0)
    p = prB(P)
    diff = sp.factor(sp.together(bG(J) - bG(P)))
    num = sp.numer(diff)
    # numerator vanishes iff q = Pr1(B) (for positive cells)
    check("Pr2(B|G) = Pr1(B|G) iff q = Pr1(B): numerator is a multiple of (q(a+b+c+d) - (a+b))",
          sp.simplify(sp.rem(sp.expand(num), sp.expand(q * (a + b + c + d) - (a + b)), q)) == 0)
    g = sp.symbols("g", positive=True)
    Pind = {(1, 1): g * b, (1, 0): (1 - g) * b, (0, 1): g * c, (0, 0): (1 - g) * c}
    check("at independence Pr2(B|G) = q", sp.simplify(bG(jeffrey(Pind, q)) - q) == 0)

    print("(3) Pearl's criterion (eq. 5)")
    # R(b, g, e): e depends on b only (G independent of e given B)
    r11, r10, r01, r00, lb, ln = sp.symbols("r11 r10 r01 r00 lb ln", positive=True)
    Pm = {(1, 1): r11, (1, 0): r10, (0, 1): r01, (0, 0): r00}
    lik = {1: lb, 0: ln}   # Pr(e | B = b)
    Re = {k: Pm[k] * lik[k[0]] for k in Pm}   # joint with e = true
    E = sum(Re.values())
    post = {k: Re[k] / E for k in Re}
    Jp = jeffrey(Pm, prB(post))
    check("conditioning on e (G indep. of e given B) = Jeffrey with q = Pr1(B|e)",
          all(sp.simplify(post[k] - Jp[k]) == 0 for k in Pm))
    check("but Pr(B|G) moves: Pr1(B|G,e) != Pr1(B|G) when lb != ln",
          sp.simplify(bG(post) - bG(Pm)) != 0)

    print("(4) ZO's deck (Table 1, p.292) and Table 2's Objective row (p.294)")
    deck = {(1, 1): F(8, 50), (1, 0): F(4, 50), (0, 1): F(10, 50), (0, 0): F(28, 50)}
    check("50 cards: 12 blue, 38 purple",
          (8 + 4 + 10 + 28, 8 + 4, 10 + 28) == (50, 12, 38))
    exact = [prG(deck), prB(deck), gB(deck), gNB(deck), bG(deck), bNG(deck)]
    printed = [F("0.36"), F("0.24"), F("0.67"), F("0.26"), F("0.44"), F("0.13")]
    check(f"exact {[str(x) for x in exact]} round to the printed Objective row",
          [rnd2(x) for x in exact] == printed)
    Jd = jeffrey(deck, F(3, 4))
    check("update Pr(blue) 0.24 -> 0.75: Pr(G|B) = 2/3 and Pr(G|~B) = 5/19 unchanged",
          gB(Jd) == F(2, 3) == gB(deck) and gNB(Jd) == F(5, 19) == gNB(deck))
    check(f"... Pr(B|G) moves 4/9 -> {bG(Jd)} (= 38/43)",
          bG(deck) == F(4, 9) and bG(Jd) == F(38, 43))

    print("(5) Rain example (p.290)")
    check(".6 x .8 + .1 x .2 = .5", F("0.6") * F("0.8") + F("0.1") * F("0.2") == F("0.5"))

    print("(6) Reported counts and averages")
    check("Exp 1: 70 = 40 experimental + 30 control; 40 = 20 Blue + 20 Purple",
          40 + 30 == 70 and 20 + 20 == 40)
    check("22 of 40 changed Pr(G|B) = 11 Blue (p.294) + 11 Purple (p.296); a majority",
          11 + 11 == 22 and 2 * 22 > 40)
    check("18 of 40 changed Pr(G|~B) = 8 Blue + 10 Purple; 'almost half'",
          8 + 10 == 18 and 2 * 18 < 40)
    check("40 of 80 conditional judgments moved (p.304)", 22 + 18 == 40)
    check("converse Pr(B|G): 15 Blue + 17 Purple = 32 of 40 changed", 15 + 17 == 32)
    check("33.0% = mean(23.5, 42.5); 73.0% = mean(82.3, 63.7) (p.298)",
          (F("23.5") + F("42.5")) / 2 == F("33.0") and (F("82.3") + F("63.7")) / 2 == F("73.0"))
    m4 = (F("23.5") + F("15.7") + F("42.5") + F("29.2")) / 4
    check(f"27.73% (p.304) = mean of the four Exp 1 violation means = {float(m4)}",
          abs(m4 - F("27.73")) <= F("0.005"))
    check("outliers: N = 38 and 37 in Fig. 1, t(34) on 35", (40 - 2, 40 - 3, 40 - 5 - 1) == (38, 37, 34))
    check("0.18 < 0.45 (p.297)", F("0.18") < F("0.45"))
    check("lottery 1.42% and 1.33% 'less than 2%' (p.305)", F("1.42") < 2 and F("1.33") < 2)
    check("ultimatum 19 + 14 = 33, 14 + 24 = 38, 33 + 38 = 71 of 100 (pp.303-304)",
          (19 + 14, 14 + 24, 33 + 38) == (33, 38, 71))
    mu = (F("18.71") + F("17.38")) / 2
    check(f"DISCREPANCY (documented, not a failure): printed 18.45% (p.304) != mean of 18.71 and "
          f"17.38 = {float(mu)}", mu == F("18.045") and abs(mu - F("18.45")) > F("0.4"))
    w = (F("18.45") - F("17.38")) / (F("18.71") - F("18.45"))
    print(f"    18.45 needs weight {w} = {float(w):.2f} : 1 on the Pr(A|O) mean; the Exp 1 figure "
          "27.73 uses equal weights")

    print("(7) Binomial tests (two-sided, exact)")
    for k, n, bound, where in ((15, 20, F(5, 100), "p.295 Blue converse, p < .05"),
                               (17, 20, F(1, 100), "p.296 Purple converse, p < .01"),
                               (33, 50, F(5, 100), "p.303 Pr(A|O), p < .05"),
                               (38, 50, F(1, 100), "p.303 Pr(A|~O), p < .01")):
        pv = binom_two_sided(k, n)
        check(f"{k}/{n}: p = {float(pv):.4f}  ({where})", pv < bound)

    print("\nAll checks passed." if OK else "\nSOME CHECKS FAILED.")
    return OK


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
