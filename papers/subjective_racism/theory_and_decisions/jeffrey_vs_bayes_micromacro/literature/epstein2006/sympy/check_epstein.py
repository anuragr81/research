"""
Epstein (2006), "An Axiomatic Model of Non-Bayesian Updating",
Review of Economic Studies 73, 413-436 -- the belief algebra of the
representation, the Section 2.3 examples, the Section 4 remark on the law of
iterated expectations, and the Paper B correspondence with the partial-adoption
step.  Journal pages are printed pages (PDF page + 412).

The paper prints no numerical illustrations; every number below is constructed
here to instantiate one of its displayed claims, and is labelled as such.

Checks, in exact rational / symbolic arithmetic:
  (1) (9) -> (10): the S1-marginal of p* is p1, and p*(.|s1) is the
      s1-dependent mixture [p(.|s1) + a q(.|s1)]/(1+a) (p. 419); (8) equals
      (1+a) E_{p*(.|s1)} u (p. 419).
  (2) (23)/(11) -> (12): p*(.|s1) = (1-g) p(.|s1) + g p2, g = a l/(1+a)
      (pp. 420, 429), with s1-dependent a, l; g < 1 whenever a >= 0, l <= 1.
  (3) Bayesian cases (p. 429): l = 0, a = 0, product p (neutral signals,
      p(.|s1) = p2, p. 428), S1 a singleton.
  (4) Section 2.3: the overreaction rewrite (p. 420), sensitivity scaled by
      1-g, (13) and the confirmatory-bias claim (p. 421), representativeness
      (p. 421), footnote 10 (p. 421).
  (5) Section 4 (pp. 429-430): the explicit LIE violation proved in Lean;
      under (23) with constant g the interim posteriors average to p2; under
      (23) no act is reversed at every signal (exact random sweep); with
      s1-dependent g the average can differ from p2 (constructed example).
  (6) Reg2 under (23): l != 0 forces supp p2 within supp p(.|s1).
  (7) Paper B: (23) and (12) are the damped target (1-w) current + w delivered
      with current = p2(s2), delivered = p(s2|s1), w = 1-l (for q) and
      w = 1 - a l/(1+a) (for p*); ranges of w; every w > 0 attained, w = 0 not;
      the damped Jeffrey step is a (23)-type mixture on the joint.

Exit status 0 iff every check passes.
"""
import random
import sys

import sympy as sp

R = sp.Rational
OK = True
N_PASS = 0
N_FAIL = 0


def check(name, cond):
    global OK, N_PASS, N_FAIL
    cond = bool(cond)
    print(("  PASS  " if cond else "  FAIL  ") + name, flush=True)
    if cond:
        N_PASS += 1
    else:
        N_FAIL += 1
        OK = False


def zero(expr):
    """Exact test that a rational function vanishes identically."""
    return sp.cancel(sp.together(expr)) == 0


# ---------------------------------------------------------------- helpers
def marg1(p):
    return [sum(row) for row in p]


def marg2(p):
    return [sum(p[i][j] for i in range(len(p))) for j in range(len(p[0]))]


def cond(p):
    m = marg1(p)
    return [[p[i][j] / m[i] for j in range(len(p[0]))] for i in range(len(p))]


def pstar(p, qc, a):
    """Eq. (9)."""
    c = cond(p)
    m = marg1(p)
    return [[(c[i][j] + a[i] * qc[i][j]) / (1 + a[i]) * m[i]
             for j in range(len(p[0]))] for i in range(len(p))]


def prior_bias(p, lam):
    """Eq. (23) = (11)."""
    c = cond(p)
    p2 = marg2(p)
    return [[(1 - lam[i]) * c[i][j] + lam[i] * p2[j]
             for j in range(len(p[0]))] for i in range(len(p))]


def expect(mu, v):
    return sum(m * x for m, x in zip(mu, v))


def damped(current, delivered, w):
    """The project's partial-adoption target (JeffreyOrder.dampedTarget)."""
    return (1 - w) * current + w * delivered


def main():
    # symbolic 2x3 joint with free positive entries, normalized
    x = sp.symbols('x0:6', positive=True)
    tot = sum(x)
    p = [[x[0] / tot, x[1] / tot, x[2] / tot], [x[3] / tot, x[4] / tot, x[5] / tot]]
    a0, a1 = sp.symbols('a0 a1', nonnegative=True)
    l0, l1 = sp.symbols('l0 l1', real=True)
    a = [a0, a1]
    lam = [l0, l1]
    y = sp.symbols('y0:6', positive=True)
    qc = [[y[0] / (y[0] + y[1] + y[2]), y[1] / (y[0] + y[1] + y[2]), y[2] / (y[0] + y[1] + y[2])],
          [y[3] / (y[3] + y[4] + y[5]), y[4] / (y[3] + y[4] + y[5]), y[5] / (y[3] + y[4] + y[5])]]
    u = sp.symbols('u0:3', real=True)

    print("(1) Compromise prior, eqs. (8)-(10), p. 419")
    ps = pstar(p, qc, a)
    check("marg1(p*) = p1", all(zero(m1 - m2) for m1, m2 in zip(marg1(ps), marg1(p))))
    cps, cp = cond(ps), cond(p)
    check("(10): p*(.|s1) = [p(.|s1) + a q(.|s1)]/(1+a)",
          all(zero(cps[i][j] - (cp[i][j] + a[i] * qc[i][j]) / (1 + a[i]))
              for i in range(2) for j in range(3)))
    check("(8) = (1+a) E_{p*(.|s1)} u",
          all(zero(expect(cp[i], u) + a[i] * expect(qc[i], u) - (1 + a[i]) * expect(cps[i], u))
              for i in range(2)))

    print("(2) Prior-Bias, (23) -> (12), pp. 420, 429")
    q = prior_bias(p, lam)
    check("q(.|s1) sums to 1", all(zero(sum(q[i]) - 1) for i in range(2)))
    psb = cond(pstar(p, q, a))
    p2 = marg2(p)
    g = [a[i] * lam[i] / (1 + a[i]) for i in range(2)]
    check("(12) with s1-dependent a, l: p* = (1-g) p(.|s1) + g p2",
          all(zero(psb[i][j] - ((1 - g[i]) * cp[i][j] + g[i] * p2[j]))
              for i in range(2) for j in range(3)))
    # g < 1: 1 - g = (1 + a(1-l))/(1+a) > 0 for a >= 0, l <= 1
    aa, ll = sp.symbols('aa ll', real=True)
    check("1 - g = (1 + a(1-l))/(1+a)",
          zero((1 - aa * ll / (1 + aa)) - (1 + aa * (1 - ll)) / (1 + aa)))
    ok = True
    for aval in [R(0), R(1, 3), R(1), R(5), R(100)]:
        for lval in [R(-3), R(-1, 2), R(0), R(1, 2), R(1)]:
            ok &= aval * lval / (1 + aval) < 1
    check("g < 1 on a grid of a >= 0, l <= 1", ok)

    print("(3) Bayesian cases, p. 429")
    psb0 = cond(pstar(p, prior_bias(p, [0, 0]), a))
    check("l = 0: p* = p(.|s1)", all(zero(psb0[i][j] - cp[i][j]) for i in range(2) for j in range(3)))
    psa0 = cond(pstar(p, qc, [0, 0]))
    check("a = 0: p* = p(.|s1) for any q", all(zero(psa0[i][j] - cp[i][j]) for i in range(2) for j in range(3)))
    A = sp.symbols('A0:2', positive=True)
    B = sp.symbols('B0:3', positive=True)
    sA, sB = sum(A), sum(B)
    pp = [[A[i] * B[j] / (sA * sB) for j in range(3)] for i in range(2)]
    cpp, pp2 = cond(pp), marg2(pp)
    check("product p: p(.|s1) = p2 (every signal neutral)",
          all(zero(cpp[i][j] - pp2[j]) for i in range(2) for j in range(3)))
    psp = cond(pstar(pp, prior_bias(pp, lam), a))
    check("product p: p* = p(.|s1) for every a, l",
          all(zero(psp[i][j] - cpp[i][j]) for i in range(2) for j in range(3)))
    p1s = [[x[0] / (x[0] + x[1] + x[2]), x[1] / (x[0] + x[1] + x[2]), x[2] / (x[0] + x[1] + x[2])]]
    check("S1 singleton: p(.|s1) = p2", all(zero(cond(p1s)[0][j] - marg2(p1s)[j]) for j in range(3)))

    print("(4) Section 2.3 examples, pp. 420-421")
    check("overreaction rewrite q = p - l (p - p2)",
          all(zero(q[i][j] - (cp[i][j] - lam[i] * (cp[i][j] - p2[j]))) for i in range(2) for j in range(3)))
    gg = sp.symbols('g', real=True)
    # sensitivity: same g at both signals
    lam_same = [gg * (1 + a0) / a0, gg * (1 + a1) / a1]
    psg = cond(pstar(p, prior_bias(p, lam_same), a))
    check("constant g: p*(.|s1) - p*(.|s1') = (1-g)(p(.|s1) - p(.|s1'))",
          all(zero((psg[0][j] - psg[1][j]) - (1 - gg) * (cp[0][j] - cp[1][j])) for j in range(3)))
    th, pB = sp.symbols('theta pB', real=True)
    # (13): S1 = {a,b}, S2 = {A,B}, p(a|A) = p(b|B) = theta
    p1b = (1 - pB) * (1 - th) + pB * th
    check("(13): p1(b) - 1/2 = (2 theta - 1)(p2(B) - 1/2)", zero(p1b - R(1, 2) - (2 * th - 1) * (pB - R(1, 2))))
    # constructed confirmatory-bias instance: theta = 3/4, p2(B) = 3/5
    thv, pBv = R(3, 4), R(3, 5)
    pc = [[(1 - pBv) * thv, pBv * (1 - thv)],          # row a: (A, B)
          [(1 - pBv) * (1 - thv), pBv * thv]]          # row b
    m1 = marg1(pc)
    check("constructed (13) instance: p2(B) = 3/5 > 1/2 and p1(b) = 11/20 > 1/2",
          marg2(pc)[1] == pBv and m1[1] == R(11, 20))

    def gstar(t):
        return (1 - t) / 2                               # decreasing [0,1] -> [0,1/2]
    gam_c = [gstar(m1[i] / max(m1)) for i in range(2)]
    check("gamma(a) > gamma(b) = 0: the conflicting signal a gets more prior weight",
          gam_c[0] > gam_c[1] == 0)
    cpc, p2c = cond(pc), marg2(pc)
    pst_c = [[(1 - gam_c[i]) * cpc[i][j] + gam_c[i] * p2c[j] for j in range(2)] for i in range(2)]
    check("signal a (points to A) is underweighted: p(A|a) > p*(A|a) > p2(A)",
          cpc[0][0] > pst_c[0][0] > p2c[0])
    check("signal b (confirms B) is processed as Bayes: p*(B|b) = p(B|b)", pst_c[1][1] == cpc[1][1])
    avg = [sum(m1[i] * pst_c[i][j] for i in range(2)) for j in range(2)]
    check("s1-dependent gamma: E_{p1}[p*(.|s1)] = %s != p2 = %s (LIE fails)" % (avg, p2c), avg != p2c)
    gamma_max = max(sp.Rational(1, 1) * 0, *[gam_c[i] for i in range(2)])
    check("gamma* values lie in [0,1) (gamma = al/(1+a) < 1 under the representation)",
          0 <= gamma_max < 1)
    # representativeness: q(A|a) = q(B|b) = 1
    alph = sp.symbols('alpha', positive=True)
    tq = sp.symbols('t', positive=True)   # p(A|a) in (0,1)
    pstar_Aa = (tq + alph * 1) / (1 + alph)
    check("representativeness: p*(A|a) - p(A|a) = alpha(1 - p(A|a))/(1+alpha) > 0",
          zero(pstar_Aa - tq - alph * (1 - tq) / (1 + alph)))
    pAa = cpc[0][0]
    check("  on the (13) instance: p*(A|a) = %s > p(A|a) = %s at alpha = 1"
          % ((pAa + 1) / 2, pAa), (pAa + 1) / 2 > pAa)
    # footnote 10
    ok = True
    for lv in [R(-3), R(-1), R(-1, 2), R(-1, 10)]:
        thr = -lv / (1 - lv)
        for xv in [thr - R(1, 100), thr, thr + R(1, 100)]:
            ok &= ((1 - lv) * xv + lv >= 0) == (xv >= thr)
    check("fn 10: (1-l) p(s|s) + l >= 0 iff p(s|s) >= -l/(1-l)", ok)

    print("(5) Section 4, law of iterated expectations, pp. 429-430")
    P = [[R(2, 5), R(1, 10)], [R(1, 5), R(3, 10)]]
    Q = [[0, 1], [0, 1]]
    Al = [1, 1]
    f = [1, -1]
    cP = cond(P)
    check("example: p1 = (1/2,1/2), p2 = (3/5,2/5), p(.|a) = (4/5,1/5), p(.|b) = (2/5,3/5)",
          marg1(P) == [R(1, 2)] * 2 and marg2(P) == [R(3, 5), R(2, 5)]
          and cP == [[R(4, 5), R(1, 5)], [R(2, 5), R(3, 5)]])
    check("example: q << p", all(not (cP[i][j] == 0 and Q[i][j] != 0) for i in range(2) for j in range(2)))
    check("example: E_{p2} f = 1/5 > 0, so {f} > {-f} at t = 0", expect(marg2(P), f) == R(1, 5))
    cPs = cond(pstar(P, Q, Al))
    vals = [expect(cPs[i], f) for i in range(2)]
    check("example: E_{p*(.|a)} f = -1/5, E_{p*(.|b)} f = -3/5 (-f chosen at each s1)",
          vals == [R(-1, 5), R(-3, 5)])
    check("example: (8) prefers -f to f at each s1",
          all(expect(cP[i], f) + Al[i] * expect(Q[i], f)
              < expect(cP[i], [-v for v in f]) + Al[i] * expect(Q[i], [-v for v in f])
              for i in range(2)))
    # constant g under (23): average of p* equals p2 (symbolic)
    avg_s = [sum(marg1(p)[i] * psg[i][j] for i in range(2)) for j in range(3)]
    check("(23), constant g: E_{p1}[p*(.|s1)] = p2 (LIE holds)", all(zero(avg_s[j] - p2[j]) for j in range(3)))
    # exact random sweep: no uniform reversal under (23)
    rng = random.Random(20060413)
    bad = 0
    trials = 3000
    for _ in range(trials):
        n1, n2 = rng.choice([2, 3, 4]), rng.choice([2, 3, 4])
        w = [[R(rng.randint(1, 20)) for _ in range(n2)] for _ in range(n1)]
        t = sum(sum(r) for r in w)
        pr = [[e / t for e in r] for r in w]
        al = [R(rng.randint(0, 40), rng.randint(1, 4)) for _ in range(n1)]
        lm = [R(rng.randint(-40, 10), 10) for _ in range(n1)]   # l <= 1
        qq = prior_bias(pr, lm)
        if any(e < 0 for r in qq for e in r):
            continue   # not a probability measure; outside the representation
        v = [R(rng.randint(-10, 10)) for _ in range(n2)]
        c0 = expect(marg2(pr), v)
        cps_r = cond(pstar(pr, qq, al))
        if max(expect(cps_r[i], v) for i in range(n1)) < c0:
            bad += 1
    check("(23): some s1 has E_{p*(.|s1)} v >= E_{p2} v, %d exact random draws (violations: %d)"
          % (trials, bad), bad == 0)

    print("(6) Reg2 (q << p) under (23)")
    Pz = [[R(1, 2), 0], [R(1, 4), R(1, 4)]]    # signal a rules out B, p2(B) = 1/4
    qz = prior_bias(Pz, [R(1, 2), 0])
    check("p(B|a) = 0 < p2(B) and l(a) = 1/2 give q(B|a) = 1/8 > 0: q not << p",
          cond(Pz)[0][1] == 0 and qz[0][1] == R(1, 8))

    print("(7) Paper B: (23) and the partial-adoption target")
    check("q(s2|s1) = damped(current = p2(s2), delivered = p(s2|s1), w = 1 - l(s1))",
          all(zero(q[i][j] - damped(p2[j], cp[i][j], 1 - lam[i])) for i in range(2) for j in range(3)))
    weff = [(1 + a[i] * (1 - lam[i])) / (1 + a[i]) for i in range(2)]
    check("p*(s2|s1) = damped(p2(s2), p(s2|s1), w = (1 + a(1-l))/(1+a))",
          all(zero(psb[i][j] - damped(p2[j], cp[i][j], weff[i])) for i in range(2) for j in range(3)))
    ok_pos, ok_neg, ok_gt0 = True, True, True
    for aval in [R(1, 10), R(1), R(7)]:
        for lval in [R(1, 10), R(1, 2), R(1)]:
            wv = (1 + aval * (1 - lval)) / (1 + aval)
            ok_pos &= (1 / (1 + aval) <= wv < 1)
        for lval in [R(-2), R(-1, 3), R(0)]:
            ok_neg &= (1 + aval * (1 - lval)) / (1 + aval) >= 1
        for lval in [R(-2), R(0), R(1)]:
            ok_gt0 &= (1 + aval * (1 - lval)) / (1 + aval) > 0
    check("Positive Prior-Bias (0<l<=1, a>0): 1/(1+a) <= w < 1", ok_pos)
    check("Negative Prior-Bias (l<=0): w >= 1 (overshoot)", ok_neg)
    check("w > 0 always (w = 0 unattainable)", ok_gt0)
    om = sp.symbols('omega', positive=True)
    check("every w in (0,1]: a = (1-w)/w, l = 1", zero(1 - ((1 - om) / om) * 1 / (1 + (1 - om) / om) - om))
    check("every w > 1: a = 1, l = 2(1-w)", zero(1 - 1 * 2 * (1 - om) / 2 - om))
    cur, dlv = sp.symbols('cur dlv', real=True)
    check("deviation: damped - delivered = (1-w)(current - delivered)",
          zero(damped(cur, dlv, om) - dlv - (1 - om) * (cur - dlv)))
    # damped Jeffrey step on a 2x2 joint = (1-w) Q + w J
    z = sp.symbols('z0:4', positive=True)
    Qj = [[z[0], z[1]], [z[2], z[3]]]   # rows A, cols B
    QB = [z[0] + z[2], z[1] + z[3]]
    d = sp.symbols('d0:2', positive=True)
    J = [[Qj[i][j] / QB[j] * d[j] for j in range(2)] for i in range(2)]
    D = [[Qj[i][j] / QB[j] * damped(QB[j], d[j], om) for j in range(2)] for i in range(2)]
    check("damped Jeffrey step = (1-w) Q + w (full Jeffrey update)",
          all(zero(D[i][j] - ((1 - om) * Qj[i][j] + om * J[i][j])) for i in range(2) for j in range(2)))

    print("\n%d PASS, %d FAIL" % (N_PASS, N_FAIL))
    print("All checks passed." if OK else "SOME CHECKS FAILED.")
    return OK


if __name__ == "__main__":
    sys.exit(0 if main() else 1)
