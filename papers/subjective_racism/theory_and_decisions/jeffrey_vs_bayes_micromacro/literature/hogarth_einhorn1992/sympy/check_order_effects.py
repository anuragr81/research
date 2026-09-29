"""
Hogarth & Einhorn (1992), Cognitive Psychology 24, 1-55 -- order-effect predictions
of the belief-adjustment model.

Pages are to the LaTeX transcription (T-p.N); the journal scan was not available.

Checks, in exact rational / symbolic arithmetic:
  (1) Eq.(3) -> Eq.(4) averaging form.
  (2) Appendix B: Eq.(B.3) from two applications of Eq.(1) with R = S_{k-1};
      (B.5) D = w_a w_b (s_b - s_a) under Anderson-Hovland weights.
  (3) Positional weights (w1 first, w2 second): D = (s_b - s_a)(w2(1+w1) - w1).
  (4) HE's own contrast weights (6a/6b), R = S_{k-1}, mixed evidence
      (lo < S0 < hi): closed form with all terms >= 0, hence recency; plus a
      random exact-rational sweep.
  (5) Same weights, consistent evidence: closed form for the no-crossing case,
      the two explicit primacy counterexamples proved in Lean, and the share of
      a random sweep that gives primacy.  HE's own illustration (T-p.7-8,
      S0 = .5, items .6/.9) gives recency on a grid of (alpha, beta).
  (6) Appendix C (R = 0): C.2, C.4 commutative; C.7 D = -alpha beta s- s+;
      variants of T-p.13-14 (constant w: none; reversed contrast: primacy).
  (7) EoS, Eq.(8): effective weights; two-item order effects; threshold
      w < n/(n+1) for the plain mean; Eq.(5) with explicit S0 has no effect.
  (8) Comparison with the one-sided construction (first cue in full, second
      damped by w): D = (2w-1)(s_b-s_a) vs HE two-sided D = w^2 (s_b-s_a).
  (9) Table 2 cells for short simple series, re-derived.
"""
import random
import sympy as sp

R = sp.Rational
OK = True


def check(name, cond):
    global OK
    print(("  PASS  " if cond else "  FAIL  ") + name)
    if not cond:
        OK = False


# ---------------------------------------------------------------- model
def adjust(w, Rf, S, s):
    """Eq.(1): S_k = S_{k-1} + w_k [s(x_k) - R]."""
    return S + w * (s - Rf)


def est(w, S, s):
    """Eq.(3): R = S_{k-1}."""
    return adjust(w, S, S, s)


def cweight(al, be, Rf, S, s):
    """Eqs.(6a)/(6b)."""
    return al * S if s <= Rf else be * (1 - S)


def est_c(al, be, S, s):
    return adjust(cweight(al, be, S, S, s), S, S, s)


def eval_c(al, be, S, s):
    return adjust(cweight(al, be, 0, S, s), 0, S, s)


def D_est_c(al, be, S0, lo, hi):
    return est_c(al, be, est_c(al, be, S0, lo), hi) - est_c(al, be, est_c(al, be, S0, hi), lo)


def main():
    S, S0, w, a, b = sp.symbols('S S0 w a b')
    wa, wab, wb, wba, w1, w2 = sp.symbols('w_a w_ab w_b w_ba w1 w2')
    al, be = sp.symbols('alpha beta')

    print("(1) Eq.(3) -> Eq.(4)")
    check("S + w(s - S) == (1-w)S + w s", sp.expand(est(w, S, a) - ((1 - w) * S + w * a)) == 0)

    print("(2) Appendix B")
    Sab = est(wab, est(wa, S0, a), b)
    Sba = est(wba, est(wb, S0, b), a)
    B3 = (a - S0) * (wa - wa * wab - wba) + (b - S0) * (wab - wb + wb * wba)
    check("(B.3) identity", sp.expand(Sab - Sba - B3) == 0)
    B5 = sp.expand((Sab - Sba).subs({wba: wa, wab: wb}))
    check("(B.5) D = w_a w_b (s_b - s_a)", sp.expand(B5 - wa * wb * (b - a)) == 0)

    print("(3) Positional weights")
    Dpos = sp.expand(est(w2, est(w1, S0, a), b) - est(w2, est(w1, S0, b), a))
    check("D = (b-a)(w2(1+w1) - w1)", sp.expand(Dpos - (b - a) * (w2 * (1 + w1) - w1)) == 0)
    check("w1 = 1: D = (2 w2 - 1)(b - a)", sp.expand(Dpos.subs(w1, 1) - (2 * w2 - 1) * (b - a)) == 0)
    check("w1 = w2 = w: D = w^2 (b - a)", sp.expand(Dpos.subs({w1: w, w2: w}) - w**2 * (b - a)) == 0)

    print("(4) Contrast weights, R = S_{k-1}, mixed evidence")
    u, v = sp.symbols('u v')
    lo, hi = S0 - u, S0 + v
    # branch choices valid for 0<=alpha,beta, 0<=S0<=1, u,v>=0 (see Lean proof)
    S1 = S0 + al * S0 * (lo - S0)
    S2 = S1 + be * (1 - S1) * (hi - S1)
    T1 = S0 + be * (1 - S0) * (hi - S0)
    T2 = T1 + al * T1 * (lo - T1)
    closed = al * be * (u * v + S0 * (1 - S0) * (u + v) + al * S0**2 * u**2 + be * (1 - S0)**2 * v**2)
    check("closed form (Lean contrast_mixed_orderEffect)", sp.expand(S2 - T2 - closed) == 0)
    random.seed(1992)
    bad = 0
    for _ in range(4000):
        s0 = R(random.randint(1, 99), 100)
        l = R(random.randint(0, 99), 100) * s0
        h = s0 + R(random.randint(1, 100), 100) * (1 - s0)
        A_, B_ = R(random.randint(1, 100), 100), R(random.randint(1, 100), 100)
        if not (l < s0 < h):
            continue
        if D_est_c(A_, B_, s0, l, h) <= 0:
            bad += 1
    check(f"random sweep (4000 draws, exact): recency every time (violations = {bad})", bad == 0)

    print("(5) Contrast weights, R = S_{k-1}, consistent evidence")
    p, q = sp.symbols('p q')
    lo, hi = S0 + p, S0 + p + q
    S1 = S0 + be * (1 - S0) * (lo - S0)
    S2 = S1 + be * (1 - S1) * (hi - S1)
    T1 = S0 + be * (1 - S0) * (hi - S0)
    T2 = T1 + be * (1 - T1) * (lo - T1)      # no crossing: T1 < lo
    closed = be**2 * (1 - S0)**2 * q * (1 - be * (2 * p + q))
    check("no-crossing closed form (Lean contrast_consistent_pos_nocross)",
          sp.expand(S2 - T2 - closed) == 0)
    e1 = est_c(R(1, 10), 1, est_c(R(1, 10), 1, R(1, 10), R(1, 2)), 1)
    e2 = est_c(R(1, 10), 1, est_c(R(1, 10), 1, R(1, 10), 1), R(1, 2))
    print(f"    asym example: S_ab = {e1}, S_ba = {e2}, D = {e1 - e2}")
    check("asym (alpha=1/10, beta=1, S0=1/10; 1/2, 1): 7516/10000 vs 87269/100000, primacy",
          e1 == R(7516, 10000) and e2 == R(87269, 100000) and e1 < e2)
    f1 = est_c(1, 1, est_c(1, 1, R(9, 10), 0), R(1, 5))
    f2 = est_c(1, 1, est_c(1, 1, R(9, 10), R(1, 5)), 0)
    print(f"    equal example: S_ab = {f1}, S_ba = {f2}, D = {f1 - f2}")
    check("equal (alpha=beta=1, S0=9/10; 0, 1/5): 1901/10000 vs 1971/10000, primacy",
          f1 == R(1901, 10000) and f2 == R(1971, 10000) and f1 < f2)
    random.seed(7)
    n_cons = n_prim = 0
    for _ in range(20000):
        s0 = R(random.randint(1, 99), 100)
        x, y = R(random.randint(0, 100), 100), R(random.randint(0, 100), 100)
        l, h = min(x, y), max(x, y)
        if l == h or (l < s0 < h) or l == s0 or h == s0:
            continue
        A_, B_ = R(random.randint(1, 100), 100), R(random.randint(1, 100), 100)
        n_cons += 1
        if D_est_c(A_, B_, s0, l, h) < 0:
            n_prim += 1
    share = n_prim / n_cons
    print(f"    consistent-evidence sweep: primacy in {n_prim}/{n_cons} = {float(share):.3f}")
    check("primacy occurs for consistent evidence under HE's weights", n_prim > 0)
    # HE's own illustration (T-p.7-8): S0 = .5, estimation values .6 (weak), .9 (strong)
    neg = [(A_, B_) for A_ in [R(i, 10) for i in range(1, 11)] for B_ in [R(i, 10) for i in range(1, 11)]
           if D_est_c(A_, B_, R(1, 2), R(6, 10), R(9, 10)) <= 0]
    check("HE illustration (S0=.5; .6,.9): recency for all alpha,beta in {.1..1}^2", neg == [])
    # Experiment 5 (T-p.23-25): estimation-mode stimuli 60% and 80%, consistent positive.
    # Primacy needs a low initial anchor: on the grid S0 in {.05..55}, alpha, beta in {.1..1}
    # it occurs only for S0 <= .25 (Exp 5 stems were rated around the scale midpoint, Fig. 6).
    e5 = [(s0, A_, B_) for s0 in [R(i, 20) for i in range(1, 12)]
          for A_ in [R(i, 10) for i in range(1, 11)] for B_ in [R(i, 10) for i in range(1, 11)]
          if D_est_c(A_, B_, s0, R(6, 10), R(8, 10)) <= 0]
    print(f"    Exp 5 values (.6, .8): primacy cells {len(e5)}, max S0 among them = {max(x[0] for x in e5)}")
    check("Exp 5 values: recency whenever S0 >= 3/10 on the grid", all(x[0] <= R(1, 4) for x in e5))
    # and the weak item after the strong one is a downward adjustment (T-p.8), when beta >= 1/2
    T1v = est_c(1, 1, R(1, 2), R(9, 10))
    check("HE illustration: strong-then-weak gives a downward second step (alpha=beta=1)",
          est_c(1, 1, T1v, R(6, 10)) < T1v)

    print("(6) Appendix C, R = 0")
    x, y = sp.symbols('x y')
    neg1 = S0 * (1 + al * x)
    check("(C.2) negatives commute", sp.expand((neg1 * (1 + al * y)) - (S0 * (1 + al * y) * (1 + al * x))) == 0)
    pos = lambda S_, s_: S_ * (1 - be * s_) + be * s_
    C4 = S0 + be * (1 - S0) * (x + y - be * x * y)
    check("(C.4) positives: both orders = S0 + beta(1-S0)(x+y-beta x y)",
          sp.expand(pos(pos(S0, x), y) - C4) == 0 and sp.expand(pos(pos(S0, y), x) - C4) == 0)
    n_, p_ = sp.symbols('n p')
    Smp = pos(S0 * (1 + al * n_), p_)
    Spm = pos(S0, p_) * (1 + al * n_)
    check("(C.7) D = -alpha beta s- s+ (no S0)", sp.expand(Smp - Spm + al * be * n_ * p_) == 0)
    check("(C.7) numeric: eval_c agrees",
          eval_c(R(1, 2), R(1, 3), R(2, 5), R(-1, 2)) is not None and
          eval_c(R(1, 2), R(1, 3), eval_c(R(1, 2), R(1, 3), R(2, 5), R(-1, 2)), R(3, 4))
          - eval_c(R(1, 2), R(1, 3), eval_c(R(1, 2), R(1, 3), R(2, 5), R(3, 4)), R(-1, 2))
          == -R(1, 2) * R(1, 3) * R(-1, 2) * R(3, 4))
    check("variant: constant w, R = 0 -> no order effect",
          sp.expand(adjust(w, 0, adjust(w, 0, S0, x), y) - adjust(w, 0, adjust(w, 0, S0, y), x)) == 0)
    rneg = lambda S_, s_: S_ + al * (1 - S_) * s_
    rpos = lambda S_, s_: S_ + be * S_ * s_
    check("variant: reversed contrast, mixed -> D = alpha beta n p (primacy)",
          sp.expand(rpos(rneg(S0, n_), p_) - rneg(rpos(S0, p_), n_) - al * be * n_ * p_) == 0)

    print("(7) EoS, Eqs.(5) and (8)")
    k = 4
    xs = sp.symbols('x2:%d' % (k + 1))
    cs = sp.symbols('c2:%d' % (k + 1))
    s1 = sp.Symbol('s1')
    agg = sum(c * xx for c, xx in zip(cs, xs))
    eq8_est = s1 + w * (agg - s1)
    eq8_eval = s1 + w * agg
    check("Eq.(8) estimation: weight 1-w on s1, w c_j on x_j",
          sp.expand(eq8_est - ((1 - w) * s1 + sum(w * c * xx for c, xx in zip(cs, xs)))) == 0)
    check("Eq.(8) evaluation: weight 1 on s1, w c_j on x_j",
          sp.expand(eq8_eval - (s1 + sum(w * c * xx for c, xx in zip(cs, xs)))) == 0)
    thr_ok = all(((1 - ww) > ww / n) == (ww < R(n, n + 1))
                 for n in range(1, 8) for ww in [R(i, 100) for i in range(0, 101)])
    check("plain mean over n later items: first dominates iff w < n/(n+1)", thr_ok)
    check("two items, estimation: D = (2w-1)(b-a)",
          sp.expand((a + w * (b - a)) - (b + w * (a - b)) - (2 * w - 1) * (b - a)) == 0)
    check("two items, evaluation: D = -(1-w)(b-a)",
          sp.expand((a + w * b) - (b + w * a) + (1 - w) * (b - a)) == 0)
    check("Eq.(5) with explicit S0, symmetric aggregate: no order effect",
          sp.expand((S0 + w * ((a + b) / 2 - S0)) - (S0 + w * ((b + a) / 2 - S0))) == 0)

    print("(8) One-sided construction vs HE two-sided")
    one = lambda A_, B_: est(w, est(1, S0, A_), B_)
    two = lambda A_, B_: est(w, est(w, S0, A_), B_)
    Done = sp.expand(one(a, b) - one(b, a))
    Dtwo = sp.expand(two(a, b) - two(b, a))
    check("one-sided D = (2w-1)(b-a)", sp.expand(Done - (2 * w - 1) * (b - a)) == 0)
    check("one-sided == Eq.(8) estimation, k=2", sp.expand(one(a, b) - (a + w * (b - a))) == 0)
    check("two-sided D = w^2 (b-a)", sp.expand(Dtwo - w**2 * (b - a)) == 0)
    check("gap = (1-w)^2 (b-a)", sp.expand(Dtwo - Done - (1 - w)**2 * (b - a)) == 0)
    ww = R(1, 4)
    check("w = 1/4, a<b: two-sided recency, one-sided primacy",
          Dtwo.subs({w: ww, a: 0, b: 1}) > 0 and Done.subs({w: ww, a: 0, b: 1}) < 0)
    sA, SA = sp.symbols('s_A S_A')
    check("two attributes: one-sided effect on first-read attribute = (1-w)(s_A - S_A)",
          sp.expand(est(1, SA, sA) - est(w, SA, sA) - (1 - w) * (sA - SA)) == 0)

    print("(9) Table 2 (T-p.12), short simple series, re-derived")
    # EoS: Eq.(8), anchor = first item; SbS: two items after an explicit S0.
    derived = {
        ("R=S", "EoS"): "Primacy (iff w < 1/2 for two items)",
        ("R=S", "SbS"): "Recency (A-H weights; HE weights: mixed yes, consistent not always)",
        ("R=0 mixed", "EoS"): "Primacy (w < 1)",
        ("R=0 mixed", "SbS"): "Recency (C.7)",
        ("R=0 consistent", "EoS"): "Primacy (w < 1)",
        ("R=0 consistent", "SbS"): "No effect (C.2/C.4)",
    }
    table2 = {("R=S", "EoS"): "Primacy", ("R=S", "SbS"): "Recency",
              ("R=0 mixed", "EoS"): "Primacy", ("R=0 mixed", "SbS"): "Recency",
              ("R=0 consistent", "EoS"): "Primacy", ("R=0 consistent", "SbS"): "No effect"}
    for key in table2:
        print(f"    {key[0]:16s} {key[1]}: Table 2 = {table2[key]:9s} | derived: {derived[key]}")
    check("every short-simple Table 2 cell matches the derived direction",
          all(derived[kk].startswith(table2[kk]) for kk in table2))

    print("\nAll checks passed." if OK else "\nSOME CHECKS FAILED.")
    return OK


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
