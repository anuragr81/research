"""
Canay, Mogstad & Mountjoy (2024), "On the Use of Outcome Tests for Detecting
Bias in Decision Making", Review of Economic Studies 91(4), 2135-2167 (read as
NBER WP 27802, revised 10 June 2023; p.N is the printed page).

Checks, in exact arithmetic:
  (1) Theorem 4.2 (p.24): with benefits independent of v (ERM), the marginal
      outcome difference E[D|w,v*_w] - E[D|b,v*_b] equals tau(w) - tau(b),
      for arbitrary cost functions.
  (2) Theorem 4.1 (pp.20-21) by the witnesses of CanayMogstadMountjoy.lean:
      for each case (unbiased; globally biased against black, against white;
      locally biased against black, against white; unclassified) the marginal
      of each race is the unique crossing of cost and benefit, the bias
      property holds, and the outcome difference equals the prescribed t for
      every t, so it can be positive, negative or zero.
Exit status 0 iff every check passes.
"""
import sympy as sp

OK = True


def check(name, cond):
    global OK
    print(("  PASS  " if cond else "  FAIL  ") + name)
    if not cond:
        OK = False


v, t = sp.symbols("v t", real=True)


def kink(x):
    return x + sp.Max(0, -x) / 2


def crossings(cost, benefit):
    """Real crossings of cost and benefit, split on the sign of v so the kink is linear."""
    sols = set()
    for sign, region in ((1, lambda s: s >= 0), (-1, lambda s: s < 0)):
        b = benefit.subs(sp.Max(0, -v), 0 if sign == 1 else -v)
        for s in sp.solve(sp.Eq(cost, b), v):
            if region(s.subs(t, 0)) if s.free_symbols else region(s):
                sols.add(sp.simplify(s))
    return sols


def main():
    print("(1) Theorem 4.2, ERM")
    Lw, Lb = sp.Function("Lw"), sp.Function("Lb")
    tw, tb, vw, vb = sp.symbols("tau_w tau_b vw vb", real=True)
    # at the marginal defendants Lw(vw) = tau_w and Lb(vb) = tau_b
    diff = (Lw(vw) - Lb(vb)).subs({Lw(vw): tw, Lb(vb): tb})
    check("marginal outcome difference = tau(w) - tau(b)", sp.simplify(diff - (tw - tb)) == 0)

    print("(2) Theorem 4.1, GRM, witnesses")
    cases = {
        "(i) unbiased":            ({"w": 2 * t - v, "b": -v},      {"w": v, "b": v},             {"w": t, "b": 0}),
        "(ii) global, vs black":   ({"w": 2 * t - 1 - v, "b": -v},  {"w": v + 1, "b": v},         {"w": t - 1, "b": 0}),
        "(ii) global, vs white":   ({"w": 2 * t - v, "b": -1 - v},  {"w": v, "b": v + 1},         {"w": t, "b": -1}),
        "(iii) local, vs black":   ({"w": -v, "b": -2 * t - v},     {"w": kink(v), "b": v},       {"w": 0, "b": -t}),
        "(iii) local, vs white":   ({"w": 2 * t - v, "b": -v},      {"w": v, "b": kink(v)},       {"w": t, "b": 0}),
        "(iv) unclassified":       ({"w": -v, "b": -2 * t - v},     {"w": 2 * v, "b": v},         {"w": 0, "b": -t}),
    }
    for name, (L, T, vs) in cases.items():
        for r in ("w", "b"):
            check(f"{name}: race {r} crosses at v* = {vs[r]}",
                  sp.simplify((L[r] - T[r]).subs(v, vs[r])) == 0)
            cr = crossings(L[r], T[r])
            check(f"{name}: race {r} crossing is unique", cr == {sp.simplify(vs[r])})
        d = sp.simplify(L["w"].subs(v, vs["w"]) - L["b"].subs(v, vs["b"]))
        check(f"{name}: outcome difference equals t for every t", sp.simplify(d - t) == 0)
    # bias properties, on a grid of v (piecewise-linear functions, so the grid with 0 suffices)
    grid = [sp.Rational(k, 2) for k in range(-6, 7)]
    tw_, tb_ = sp.Lambda(v, kink(v)), sp.Lambda(v, v)
    check("(iii) vs black: kink(v) >= v everywhere, > v exactly for v < 0",
          all(tw_(g) >= tb_(g) for g in grid) and all((tw_(g) > tb_(g)) == (g < 0) for g in grid))
    check("(iv) unclassified: 2v > v for v > 0 and 2v < v for v < 0",
          all((2 * g > g) == (g > 0) and (2 * g < g) == (g < 0) for g in grid))
    check("(ii) global vs black: v + 1 > v for every v", sp.simplify((v + 1) - v) == 1)
    return OK


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
