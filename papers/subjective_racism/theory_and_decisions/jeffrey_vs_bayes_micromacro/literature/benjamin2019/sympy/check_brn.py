"""
Benjamin, Bodoh-Creed & Rabin (2019), "Base-Rate Neglect: Foundations and
Implications", working paper, July 19, 2019.

Checks, exact (sympy rationals / symbols) unless stated:

  Part I, the paper's model and its numbers
    (1)  the one-shot rule and the dynamic rule: the t-step closed form
         (eq. 4 p.20, eq. 5-6 p.21) symbolically for t = 1..5, two hypotheses;
         the weight on signal tau is alpha^(t-tau);
    (2)  recency: reversing two signals shifts the log odds by
         (1-alpha)(l2 - l1); eq. (7) p.22 geometric bound L/(1-alpha);
    (3)  eq. (17) / Proposition 1 (p.14, p.52): the thresholds
         z = O^(1-alpha) and O > z^(1/(1-alpha));
    (4)  Proposition 2 (subadditivity, p.16) and Proposition 3 (conjunction,
         p.17) on an exact instance;
    (5)  the paper's worked numbers: Kahneman-Tversky 5.4 and 1.2 (p.8), the
         Cab problem 41% (p.8), Eddy's test 32%, >99%, 86%, and the belief
         movements (p.13-14), Heidi/Tarso 1/32, 15/31, 1/16, 31/46 = .674 and
         footnote 10's .270 (p.17), eq. (9)'s 7/12 and 5/12 (p.25), Table 2
         (p.27), Proposition 7's threshold (p.32), eq. (14) (p.29);
    (6)  Table 1 (p.11) re-derived from its own Median column;
    (7)  the five-employee example of Section 8.2 (p.39) by enumeration.

  Part II, the Paper B question (not the paper's claim)
    (8)  2x2 joint, one cue on A (likelihood a_i), one on B (likelihood b_j),
         BRN on the four cells: log odds ratio after two steps is alpha^2 times
         the prior's in either order; the Bayes benchmark P^B keeps it;
    (9)  at independence (c = 0) the A-marginal differs between orders:
         odds (a0/a1)^alpha (u0/u1)^(alpha^2) vs (a0/a1)(u0/u1)^(alpha^2);
         exact witness alpha = 1/2, uniform prior, a = b = (49/50, 1/50):
         7/8 vs 49/50; the same in Paper B's parametrisation (prior(alpha,
         beta, c), matched likelihoods q_i/P(A=i));
    (10) framing (the paper's Section 2.3): BRN applied to the A-partition
         only, with the B-given-A conditionals carried over, gives NO order
         effect at c = 0 and keeps the odds ratio.

Exit status 0 iff every check passes.
"""
import itertools
import sys

import sympy as sp

RESULTS = []


def check(name, cond):
    ok = bool(cond)
    RESULTS.append((name, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}")
    return ok


def brn(prior, lik, al):
    """One BRN step on a dict of cells: p(s|th) p(th)^al / sum."""
    w = {k: lik[k] * prior[k] ** al for k in prior}
    Z = sum(w.values())
    return {k: w[k] / Z for k in w}


# ---------------------------------------------------------------------------
# Part I
# ---------------------------------------------------------------------------
def part1_closed_form():
    al = sp.symbols("alpha", positive=True)
    L0 = sp.symbols("l0", real=True)
    ls = sp.symbols("l1:6", real=True)
    # two hypotheses, log-odds recursion L_t = l_t + al * L_{t-1}
    L = L0
    for t in range(1, 6):
        L = ls[t - 1] + al * L
        closed = al ** t * L0 + sum(al ** (t - tau) * ls[tau - 1] for tau in range(1, t + 1))
        check(f"(1) eq.(6) closed form, t={t}: L = alpha^t l0 + sum alpha^(t-tau) l_tau",
              sp.expand(L - closed) == 0)
    # eq. (4) as a ratio with actual probabilities, two hypotheses
    p, s1, s1b, s2, s2b = sp.symbols("p x1 y1 x2 y2", positive=True)
    prior = {"th": p, "tb": 1 - p}
    post1 = brn(prior, {"th": s1, "tb": s1b}, al)
    post2 = brn(post1, {"th": s2, "tb": s2b}, al)
    ratio = post2["th"] / post2["tb"]
    eq4 = (s2 / s2b) * (s1 / s1b) ** al * (p / (1 - p)) ** (al ** 2)
    d = (sp.expand_log(sp.log(sp.powdenest(ratio, force=True)), force=True)
         - sp.expand_log(sp.log(eq4), force=True))
    check("(1) eq.(4) p.20: two-step ratio = (x2/y2)(x1/y1)^alpha (p/(1-p))^(alpha^2)",
          sp.simplify(d) == 0)


def part1_recency_bound():
    al, L0, l1, l2 = sp.symbols("alpha l0 l1 l2", real=True)
    ab = al ** 2 * L0 + al * l1 + l2
    ba = al ** 2 * L0 + al * l2 + l1
    check("(2) order effect: L(1 then 2) - L(2 then 1) = (1-alpha)(l2-l1)",
          sp.expand(ab - ba - (1 - al) * (l2 - l1)) == 0)
    a = sp.Rational(3, 5)
    check("(2) recency: with alpha=3/5, l2>l1 the later signal wins",
          (ab - ba).subs({al: a, l1: 0, l2: 1}) > 0)
    x, Lb = sp.symbols("x Lbar", positive=True)
    j = sp.symbols("j", integer=True, nonnegative=True)
    s = sp.summation(x ** j, (j, 0, sp.oo))
    check("(2) eq.(7) p.22: sum_{j>=0} alpha^j = 1/(1-alpha) for 0<alpha<1",
          sp.simplify(sp.piecewise_fold(s).args[0][0] - 1 / (1 - x)) == 0)
    # eq. (8): stationary mean E[l]/(1-alpha)
    El = sp.symbols("El", real=True)
    check("(2) eq.(8) p.22: sum_j alpha^j E[l] = E[l]/(1-alpha)",
          sp.simplify(sp.piecewise_fold(s).args[0][0] * El - El / (1 - x)) == 0)


def part1_prop1():
    ok = True
    for al in [sp.Rational(k, 10) for k in range(0, 10)]:
        for O in [sp.Rational(n, 1) for n in (2, 5, 9, 20)]:
            thr = O ** (1 - al)          # z threshold of part (2)
            for z in [sp.Rational(11, 10), sp.Rational(3, 2), 2, 4, 25]:
                post = z * O ** al
                if (post < O) != (z < thr):
                    ok = False
    check("(3) Prop.1 / eq.(17): z O^alpha < O  iff  z < O^(1-alpha) (grid)", ok)
    # part (1): prior odds above z^(1/(1-alpha)) gives moderation
    al = sp.Rational(1, 2)
    z = sp.Integer(3)
    O = z ** (1 / (1 - al)) + 1          # 10 > 9
    check("(3) Prop.1(1): alpha=1/2, z=3, O=10 > 3^2 gives posterior odds < prior odds",
          z * sp.sqrt(O) < O)


def part1_props23():
    al = sp.Rational(1, 2)
    pA, pB, pC = sp.Rational(1, 2), sp.Rational(1, 10), sp.Rational(2, 5)
    sA, sB, sC = sp.Rational(1, 5), sp.Rational(1, 2), sp.Rational(1, 3)
    # Theta2 = {A, B, C}: fine; Theta1 = {A, B u C}: coarse (the paper's
    # labelling in the proposition is swapped relative to the text; the
    # inequality is fine > coarse)
    fine = brn({"A": pA, "B": pB, "C": pC}, {"A": sA, "B": sB, "C": sC}, al)
    pBC = pB + pC
    sBC = (sB * pB + sC * pC) / pBC
    coarse = brn({"A": pA, "BC": pBC}, {"A": sA, "BC": sBC}, al)
    check("(4) Prop.2 p.16: p(B|s)+p(C|s) (fine) > p(B u C|s) (coarse), alpha=1/2",
          sp.nsimplify(fine["B"] + fine["C"] - coarse["BC"]) > 0)
    # Prop 3: A in B in Omega; states w1 (A), w2 (B\A), w3 (Omega\B)
    p1, p2, p3 = sp.Rational(1, 20), sp.Rational(3, 10), sp.Rational(13, 20)
    l1, l2, l3 = sp.Rational(9, 10), sp.Rational(1, 10), sp.Rational(1, 10)
    sA_ = l1
    sB_ = (l1 * p1 + l2 * p2) / (p1 + p2)
    snA = (l2 * p2 + l3 * p3) / (p2 + p3)
    snB = l3
    cond = sA_ > sB_ and snB >= snA
    al0 = sp.Integer(0)
    a1 = brn({"A": p1, "nA": p2 + p3}, {"A": sA_, "nA": snA}, al0)["A"]
    b2 = brn({"B": p1 + p2, "nB": p3}, {"B": sB_, "nB": snB}, al0)["B"]
    bay_a = brn({"A": p1, "nA": p2 + p3}, {"A": sA_, "nA": snA}, 1)["A"]
    bay_b = brn({"B": p1 + p2, "nB": p3}, {"B": sB_, "nB": snB}, 1)["B"]
    check("(4) Prop.3 p.17: conjunction violation at alpha=0 (and none for Bayes)",
          cond and a1 > b2 and bay_a <= bay_b)


def part1_numbers():
    R = sp.Rational
    # Kahneman-Tversky lawyer/engineer, p.8
    kt = (R(70, 30)) / (R(30, 70))
    check("(5) p.8: Bayes ratio (70/30)/(30/70) = 5.44 ~ 5.4", abs(kt - R(54, 10)) < R(5, 100))
    obs = (R(55, 45)) / (R(50, 50))
    check("(5) p.8: observed ratio (55/45)/(50/50) = 1.22 ~ 1.2", abs(obs - R(12, 10)) < R(5, 100))
    # Cab problem, p.8
    cab = R(8, 10) * R(15, 100) / (R(8, 10) * R(15, 100) + R(2, 10) * R(85, 100))
    check("(5) p.8: Cab problem (.8)(.15)/((.8)(.15)+(.2)(.85)) = 0.414 ~ 41%",
          abs(cab - R(41, 100)) < R(1, 200))
    # Eddy, p.13-14
    pos = R(5, 100) * R(9, 10) / (R(5, 100) * R(9, 10) + R(95, 100) * R(1, 10))
    check("(5) p.13: P(disease|+) = 0.3214 ~ 32%", abs(pos - R(32, 100)) < R(1, 200))
    neg_rate = R(5, 100) * R(1, 10) + R(95, 100) * R(9, 10)
    check("(5) p.13: P(negative) = 0.86", neg_rate == R(86, 100))
    free = R(95, 100) * R(9, 10) / neg_rate
    check("(5) p.14: P(no disease|-) = 0.9942 > 99%", free > R(99, 100))
    saki_neg = brn({"D": R(5, 100), "N": R(95, 100)}, {"D": R(1, 10), "N": R(9, 10)}, 0)
    check("(5) p.14: extreme Saki after a negative test: P(disease) = 10%, P(free) = 90%",
          saki_neg["D"] == R(1, 10))
    check("(5) p.14: Tommy moves up 0.27 after + (0.3214 - 0.05 = 0.2714)",
          abs(pos - R(5, 100) - R(27, 100)) < R(1, 200))
    down = R(5, 100) - (1 - free)
    print(f"      Tommy's move down after -: {sp.nsimplify(down)} = {float(down):.4f} "
          "(paper: 0.045)")
    check("(5) p.14: Tommy moves down ~0.045 after - (actual 0.0442; paper rounds up)",
          abs(down - R(45, 1000)) < R(1, 1000))
    check("(5) p.14: extreme Saki moves up 0.85 (to 90%) and up 0.05 (to 10%)",
          R(9, 10) - R(5, 100) == R(85, 100) and R(1, 10) - R(5, 100) == R(5, 100))
    # Heidi / Tarso, p.17
    prior = R(1, 32)
    lik_not = R(15, 31)
    check("(5) p.17: P(sweep) = 1/32 and P(h | no sweep) = 15/31",
          prior == R(1, 2) ** 5 and lik_not == (R(1, 2) - prior) / (1 - prior))
    tommy = brn({"S": prior, "N": 1 - prior}, {"S": 1, "N": lik_not}, 1)["S"]
    check("(5) p.17: Tommy's P(sweep | h) = 1/16", tommy == R(1, 16))
    saki0 = brn({"S": prior, "N": 1 - prior}, {"S": 1, "N": lik_not}, 0)["S"]
    check("(5) p.17: extreme Saki's P(sweep | h) = 31/46 = 0.674",
          saki0 == R(31, 46) and abs(float(saki0) - 0.674) < 5e-4)
    saki_half = brn({"S": prior, "N": 1 - prior}, {"S": 1, "N": lik_not}, R(1, 2))["S"]
    v = float(saki_half)
    print(f"      footnote 10, alpha = 1/2: {sp.simplify(saki_half)} = {v:.5f} (paper: 0.270)")
    check("(5) p.17 fn.10: alpha=1/2 gives 0.2707 (paper prints 0.270; 0.271 to 3 d.p.)",
          abs(v - 0.270) < 1e-3)
    # eq. (9), p.25
    Th = [R(3, 4), R(1, 2), R(1, 4)]
    phh = sum(t ** 2 for t in Th) / sum(Th)
    pht = sum(t * (1 - t) for t in Th) / sum(1 - t for t in Th)
    check("(5) eq.(9) p.25: p(h|h) = 7/12 and p(h|t) = 5/12", phh == R(7, 12) and pht == R(5, 12))
    # Table 2, p.27 (alpha = 0, eqs 10-13)
    th = [(R(9, 10), R(9, 10)), (R(1, 2), R(1, 2)), (R(1, 10), R(1, 10))]
    rows = {
        "(h,h)": [r for r, s in th],
        "(t,h)": [1 - r for r, s in th],
        "(h,t)": [1 - s for r, s in th],
        "(t,t)": [s for r, s in th],
    }
    published = {"(h,h)": (0.60, 0.33, 0.07), "(h,t)": (0.07, 0.33, 0.60),
                 "(t,h)": (0.07, 0.33, 0.60), "(t,t)": (0.60, 0.33, 0.07)}
    ok = True
    for key, w in rows.items():
        post = [x / sum(w) for x in w]
        ok &= all(abs(float(a) - b) < 0.005 for a, b in zip(post, published[key]))
    check("(5) Table 2 p.27: 0.60/0.33/0.07 rows (the third row is printed '(h,h)', "
          "a typo for '(t,h)')", ok)
    # Proposition 7, p.32
    ok = True
    for al in [R(k, 10) for k in range(0, 10)]:
        for m, n in [(R(1, 2), R(1, 4)), (R(3, 5), R(1, 5)), (R(4, 10), R(3, 10))]:
            thr = m ** (1 / (1 - al)) / (m ** (1 / (1 - al)) + n ** (1 / (1 - al)))
            for pG in [R(k, 20) for k in range(1, 20)]:
                silent_ok = (m / n) * (pG / (1 - pG)) ** al <= pG / (1 - pG)
                if silent_ok != (pG >= thr):
                    ok = False
    check("(5) Prop.7 p.32: revealing g does not raise the odds iff p(G) >= "
          "m^(1/(1-a))/(m^(1/(1-a))+n^(1/(1-a))) (grid)", ok)
    # eq. (14), p.29: long-run recursion of the normal-normal mean
    al = sp.symbols("alpha", positive=True)
    s_ = sp.symbols("s1:9")
    T = 8
    def mean(t, rho0=0):
        num = al ** t * rho0 + sum(al ** (t - i) * s_[i - 1] for i in range(1, t + 1))
        den = al ** t * rho0 + sum(al ** (t - i) for i in range(1, t + 1))
        return num, den
    # with rho0 -> 0 and the infinite-past limit, den -> 1/(1-alpha): E_t = (1-a) sum a^(t-i) s_i
    Et = (1 - al) * sum(al ** (T - i) * s_[i - 1] for i in range(1, T + 1))
    Et1 = (1 - al) * sum(al ** (T - 1 - i) * s_[i - 1] for i in range(1, T))
    check("(5) eq.(14) p.29: E_t - E_{t-1} = (1-alpha)(s_t - E_{t-1}) in the stationary limit",
          sp.expand(Et - Et1 - (1 - al) * (s_[T - 1] - Et1)) == 0)


def part1_table1():
    """Table 1 p.11, from Griffin & Tversky (1992) Study 2.  Recomputes every
    derived column from the printed Median column.  Priors printed .33/.67 are
    1/3, 2/3.  The medians are printed to two decimals, so a derived value is
    accepted if it is attained by some median within +-0.005 of the printed
    one (and, for the 'Actual alpha' column, of the printed 0.50-prior median)."""
    R = sp.Rational
    txt = """0.10 5 0.10 0.23 0.56 0.56 1.00 2.61
0.10 6 0.20 0.45 0.28 0.46 2.25 7.36
0.10 7 0.36 0.60 0.20 0.55 5.06 13.5
0.10 8 0.55 0.80 0.00 0.48 11.4 36.0
0.10 9 0.74 0.85 0.21 0.69 25.6 51.0
0.33 5 0.33 0.33 1.02 1.02 1.00 0.99
0.33 6 0.53 0.50 0.58 1.17 2.25 2.00
0.33 7 0.72 0.57 0.82 1.93 5.06 2.65
0.33 8 0.85 0.77 0.26 1.77 11.4 6.70
0.33 9 0.93 0.90 0.00 1.51 25.6 18.0
0.50 5 0.50 0.50 x x 1.00 1.00
0.50 6 0.69 0.60 x x 2.25 1.5
0.50 7 0.84 0.70 x x 5.06 2.33
0.50 8 0.92 0.80 x x 11.4 4.00
0.50 9 0.96 0.90 x x 25.6 9.00
0.67 5 0.67 0.55 0.29 0.29 1.00 0.61
0.67 6 0.82 0.65 0.31 -0.28 2.25 0.93
0.67 7 0.91 0.71 0.07 -1.05 5.06 1.22
0.67 8 0.96 0.83 0.24 -1.27 11.4 2.36
0.67 9 0.98 0.90 0.00 -1.51 25.6 4.50
0.90 5 0.90 0.60 0.18 0.18 1.00 0.17
0.90 6 0.95 0.70 0.20 0.02 2.25 0.26
0.90 7 0.98 0.85 0.40 0.05 5.06 0.63
0.90 8 0.99 0.93 0.51 0.04 11.4 1.37
0.90 9 0.996 0.99 0.90 0.43 25.6 7.30"""
    import math
    rows = [l.split() for l in txt.splitlines()]
    prior_of = {"0.10": R(1, 10), "0.33": R(1, 3), "0.50": R(1, 2), "0.67": R(2, 3), "0.90": R(9, 10)}
    med05 = {int(r[1]): float(r[3]) for r in rows if r[0] == "0.50"}
    lr_ok = bayes_bad = []
    lr_ok, bayes_bad, derived_bad = True, [], []
    for r in rows:
        p = prior_of[r[0]]
        k = int(r[1])
        LR = R(3, 2) ** (2 * k - 10)
        # LR column is printed to 3 significant figures
        if abs(float(LR) - float(r[6])) > 0.051 * max(1, float(r[6]) / 10):
            lr_ok = False
        bayes = p * LR / (p * LR + 1 - p)
        dp = 3 if r[2] == "0.996" else 2
        if round(float(bayes), dp) != float(r[2]):
            bayes_bad.append((r[0], k, float(bayes), r[2]))
        if r[0] == "0.50":
            continue
        pr = float(p / (1 - p))
        m = float(r[3])
        meds = [m + d for d in (-0.005, 0, 0.005) if 0 < m + d < 1]
        m5 = med05[k]
        meds5 = [m5 + d for d in (-0.005, 0, 0.005)] if k != 5 else [0.5]
        def odds(x):
            return x / (1 - x)
        acts = [math.log(odds(a) / odds(b)) / math.log(pr) for a in meds for b in meds5]
        miss = [math.log(odds(a) / float(LR)) / math.log(pr) for a in meds]
        pus = [odds(a) / pr for a in meds]
        def within(vals, printed, tol):
            return min(vals) - tol <= printed <= max(vals) + tol
        tol_pu = 0.005 if float(r[7]) < 10 else 0.05
        if not (within(acts, float(r[4]), 0.005) and within(miss, float(r[5]), 0.005)
                and within(pus, float(r[7]), tol_pu)):
            derived_bad.append((r[0], k))
    check("(6) Table 1 p.11: LR column = 1.5^(2s-10) (3 s.f.)", lr_ok)
    for b in bayes_bad:
        print(f"      Bayes column: prior {b[0]}, {b[1]} heads: exact {b[2]:.4f}, printed {b[3]}")
    check("(6) Table 1: Bayes column reproduced to printed precision except prior .10 / 8 heads "
          "(0.5586 printed 0.55)", [(b[0], b[1]) for b in bayes_bad] == [("0.10", 8)])
    for d in derived_bad:
        print(f"      derived columns not attainable from the printed median: prior {d[0]}, {d[1]} heads")
    check("(6) Table 1: 'Actual alpha', '(Mis)est alpha' and P_U columns attainable from "
          "the printed Median (+-0.005) in all 20 rows", derived_bad == [])


def part1_employees():
    """Section 3 (p.20) and Section 8.2 (p.39): five employees, each agrees
    w.p. 1/2, H = at least three agree.  Enumerate the 32 profiles."""
    R = sp.Rational
    profiles = list(itertools.product([0, 1], repeat=5))
    def P(event):
        return R(sum(1 for x in profiles if event(x)), 32)
    H = lambda x: sum(x) >= 3
    def lik(k, hyp):
        # p(s_k = a | hyp, s_1..s_{k-1} = a)
        num = P(lambda x: hyp(x) and all(x[:k]))
        den = P(lambda x: hyp(x) and all(x[:k - 1]))
        return num / den
    notH = lambda x: not H(x)
    l1 = (lik(1, H), lik(1, notH))
    l2 = (lik(2, H), lik(2, notH))
    l3 = (lik(3, H), lik(3, notH))
    check("(7) p.39: p(s1=a|H) = 0.6875, p(s1=a|not H) = 0.3125 (as printed)",
          l1 == (R(11, 16), R(5, 16)))
    post1 = brn({"H": R(1, 2), "N": R(1, 2)}, {"H": l1[0], "N": l1[1]}, 0)
    check("(7) p.39: extreme Saki after one 'agree': 0.6875 (as printed)", post1["H"] == R(11, 16))
    print(f"      correct likelihoods of the 2nd 'agree': p(.|H,s1) = {l2[0]}, "
          f"p(.|not H,s1) = {l2[1]}  (paper uses 0.875 and 0.125)")
    post2 = brn(post1, {"H": l2[0], "N": l2[1]}, 0)
    tommy2 = P(lambda x: H(x) and all(x[:2])) / P(lambda x: all(x[:2]))
    print(f"      extreme Saki after two 'agree': {post2['H']} = {float(post2['H']):.4f} "
          f"(paper: 0.875);  Tommy: {tommy2} = {float(tommy2):.4f} (paper: 0.939)")
    check("(7) p.39 AUDIT: correct extreme-Saki value after two 'agree' is 35/46, not 0.875",
          post2["H"] == R(35, 46))
    check("(7) p.39 AUDIT: Tommy's value after two 'agree' is 7/8 = 0.875, not 0.939",
          tommy2 == R(7, 8))
    paper_tommy = R(7, 8) * R(11, 16) / (R(7, 8) * R(11, 16) + R(1, 8) * R(5, 16))
    check("(7) p.39 AUDIT: the printed 0.939 is Bayes with the posteriors 0.875/0.125 used "
          "as likelihoods (double counting s1)", abs(float(paper_tommy) - 0.939) < 5e-4)
    post3 = brn(post2, {"H": l3[0], "N": l3[1]}, 0)
    check("(7) p.39: after three 'agree' extreme Saki is certain of H (as printed)",
          post3["H"] == 1)


# ---------------------------------------------------------------------------
# Part II: the Paper B question
# ---------------------------------------------------------------------------
CELLS = [(0, 0), (0, 1), (1, 0), (1, 1)]


def cueA(a):
    return {(i, j): a[i] for (i, j) in CELLS}


def cueB(b):
    return {(i, j): b[j] for (i, j) in CELLS}


def log_or(P):
    return sp.log(P[(0, 0)]) + sp.log(P[(1, 1)]) - sp.log(P[(0, 1)]) - sp.log(P[(1, 0)])


def or_(P):
    return P[(0, 0)] * P[(1, 1)] / (P[(0, 1)] * P[(1, 0)])


def margA0(P):
    return P[(0, 0)] + P[(0, 1)]


def simp(e):
    return sp.simplify(sp.powsimp(sp.expand_power_base(sp.powdenest(e, force=True), force=True),
                                  force=True))


def part2_assoc():
    al = sp.symbols("alpha", positive=True)
    P = dict(zip(CELLS, sp.symbols("P00 P01 P10 P11", positive=True)))
    a = sp.symbols("a0 a1", positive=True)
    b = sp.symbols("b0 b1", positive=True)
    AB = brn(brn(P, cueA(a), al), cueB(b), al)
    BA = brn(brn(P, cueB(b), al), cueA(a), al)
    PB = brn(P, {k: cueA(a)[k] * cueB(b)[k] for k in CELLS}, 1)
    check("(8) Paper B: odds ratio after A then B = OR(P)^(alpha^2)",
          simp(or_(AB) / or_(P) ** (al ** 2)) == 1)
    check("(8) Paper B: odds ratio after B then A = OR(P)^(alpha^2)",
          simp(or_(BA) / or_(P) ** (al ** 2)) == 1)
    check("(8) Paper B: identical across orders", simp(or_(AB) / or_(BA)) == 1)
    check("(8) Paper B: Bayes benchmark P^B keeps OR(P)", simp(or_(PB) / or_(P)) == 1)
    lg = sp.expand_log(log_or(AB), force=True)
    target = al ** 2 * sp.expand_log(log_or(P), force=True)
    check("(8) Paper B: log OR(BRN) = alpha^2 log OR(P^B), symbolic",
          sp.simplify(sp.expand(sp.expand_log(sp.powdenest(lg, force=True), force=True)) - target) == 0)


def part2_marginal():
    al = sp.symbols("alpha", positive=True)
    a = sp.symbols("a0 a1", positive=True)
    b = sp.symbols("b0 b1", positive=True)
    u = sp.symbols("u0 u1", positive=True)
    v = sp.symbols("v0 v1", positive=True)
    P = {(i, j): u[i] * v[j] for (i, j) in CELLS}
    AB = brn(brn(P, cueA(a), al), cueB(b), al)
    BA = brn(brn(P, cueB(b), al), cueA(a), al)
    oddsAB = margA0(AB) / (1 - margA0(AB))
    oddsBA = margA0(BA) / (1 - margA0(BA))
    fAB = (a[0] / a[1]) ** al * (u[0] / u[1]) ** (al ** 2)
    fBA = (a[0] / a[1]) * (u[0] / u[1]) ** (al ** 2)
    check("(9) c=0: A-marginal odds, A first = (a0/a1)^alpha (u0/u1)^(alpha^2)",
          simp(oddsAB / fAB) == 1)
    check("(9) c=0: A-marginal odds, A last = (a0/a1) (u0/u1)^(alpha^2)",
          simp(oddsBA / fBA) == 1)
    check("(9) c=0: ratio of the two = (a0/a1)^(alpha-1): the EARLIER cue is down-weighted",
          simp(oddsAB / oddsBA / (a[0] / a[1]) ** (al - 1)) == 1)
    # exact witness
    R = sp.Rational
    Pu = {k: R(1, 4) for k in CELLS}
    c = (R(49, 50), R(1, 50))
    ab = brn(brn(Pu, cueA(c), R(1, 2)), cueB(c), R(1, 2))
    ba = brn(brn(Pu, cueB(c), R(1, 2)), cueA(c), R(1, 2))
    pb = brn(Pu, {k: cueA(c)[k] * cueB(c)[k] for k in CELLS}, 1)
    mab, mba, mpb = (sp.nsimplify(margA0(x)) for x in (ab, ba, pb))
    print(f"      witness: A-marginal A-first {mab}, A-last {mba}, Bayes {mpb}")
    check("(9) witness alpha=1/2, uniform prior, a=b=(49/50,1/50): 7/8 vs 49/50 (Bayes 49/50)",
          mab == R(7, 8) and mba == R(49, 50) and mpb == R(49, 50))
    check("(9) witness: log odds ratio is 0 in both orders (prior independent)",
          sp.nsimplify(or_(ab)) == 1 and sp.nsimplify(or_(ba)) == 1)
    # Paper B's own parametrisation: prior(mA, mB, c), matched likelihoods
    mA, mB, q0, r0 = R(3, 5), R(1, 2), R(4, 5), R(3, 10)
    def priorB(cc):
        return {(0, 0): mA * mB + cc, (0, 1): mA * (1 - mB) - cc,
                (1, 0): (1 - mA) * mB - cc, (1, 1): (1 - mA) * (1 - mB) + cc}
    aB = (q0 / mA, (1 - q0) / (1 - mA))
    bB = (r0 / mB, (1 - r0) / (1 - mB))
    lam = R(1, 4)
    Pc = priorB(0)
    ab = brn(brn(Pc, cueA(aB), lam), cueB(bB), lam)
    ba = brn(brn(Pc, cueB(bB), lam), cueA(aB), lam)
    pb = brn(Pc, {k: cueA(aB)[k] * cueB(bB)[k] for k in CELLS}, 1)
    fab, fba, fpb = (float(margA0(x)) for x in (ab, ba, pb))
    print(f"      Paper B prior(3/5,1/2,0), q0=4/5, r0=3/10, BRN alpha=1/4: "
          f"P(A=0) A-first {fab:.4f}, A-last {fba:.4f}, P^B {fpb:.4f}")
    check("(9) Paper B parametrisation at c=0: A-marginal differs between orders; P^B gives q0",
          abs(fab - fba) > 1e-3 and sp.nsimplify(margA0(pb)) == q0)
    cc = sp.symbols("c")
    Pcc = priorB(cc)
    al_s = sp.Rational(1, 2)
    abc = brn(brn(Pcc, cueA(aB), al_s), cueB(bB), al_s)
    bac = brn(brn(Pcc, cueB(bB), al_s), cueA(aB), al_s)
    ok = True
    for cv in [R(-1, 20), R(-1, 50), R(1, 50), R(1, 20)]:
        o1 = or_({k: abc[k].subs(cc, cv) for k in CELLS})
        o2 = or_({k: bac[k].subs(cc, cv) for k in CELLS})
        o0 = or_(Pcc).subs(cc, cv)
        ok &= abs(float(o1) - float(o2)) < 1e-12 and abs(float(o1) - float(o0) ** 0.25) < 1e-12
    check("(9) Paper B parametrisation, c != 0: OR equal across orders and = OR(P)^(1/4) "
          "at BRN alpha=1/2", ok)


def part2_framing():
    """BRN applied to the A-partition only (hypotheses A=0, A=1), with the
    B-given-A conditionals carried over rigidly, and likewise for B.  At c = 0
    this gives no order effect on any cell."""
    al = sp.symbols("alpha", positive=True)
    a = sp.symbols("a0 a1", positive=True)
    b = sp.symbols("b0 b1", positive=True)
    x, y = sp.symbols("x y", positive=True)
    u, v = (x, 1 - x), (y, 1 - y)
    P = {(i, j): u[i] * v[j] for (i, j) in CELLS}
    def coarseA(Q):
        m = [sp.factor(sp.together(Q[(0, 0)] + Q[(0, 1)])), sp.factor(sp.together(Q[(1, 0)] + Q[(1, 1)]))]
        post = brn({0: m[0], 1: m[1]}, {0: a[0], 1: a[1]}, al)
        return {(i, j): post[i] * Q[(i, j)] / m[i] for (i, j) in CELLS}
    def coarseB(Q):
        m = [sp.factor(sp.together(Q[(0, 0)] + Q[(1, 0)])), sp.factor(sp.together(Q[(0, 1)] + Q[(1, 1)]))]
        post = brn({0: m[0], 1: m[1]}, {0: b[0], 1: b[1]}, al)
        return {(i, j): post[j] * Q[(i, j)] / m[j] for (i, j) in CELLS}
    ab = coarseB(coarseA(P))
    ba = coarseA(coarseB(P))
    check("(10) framing: BRN on each attribute's partition (rigid conditionals) has no "
          "order effect at c=0", all(sp.simplify(ab[k] - ba[k]) == 0 for k in CELLS))
    Pg = dict(zip(CELLS, sp.symbols("P00 P01 P10 P11", positive=True)))
    check("(10) framing: and it keeps the odds ratio at every prior (no alpha^2 shrinkage)",
          sp.simplify(or_(coarseB(coarseA(Pg))) / or_(Pg)) == 1)


def main():
    part1_closed_form()
    part1_recency_bound()
    part1_prop1()
    part1_props23()
    part1_numbers()
    part1_table1()
    part1_employees()
    part2_assoc()
    part2_marginal()
    part2_framing()
    n_ok = sum(ok for _, ok in RESULTS)
    print(f"\n{n_ok}/{len(RESULTS)} PASS")
    return 0 if n_ok == len(RESULTS) else 1


if __name__ == "__main__":
    sys.exit(main())
