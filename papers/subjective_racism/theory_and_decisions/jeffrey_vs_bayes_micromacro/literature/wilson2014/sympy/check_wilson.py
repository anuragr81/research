"""
Wilson, "Bounded Memory and Biases in Information Processing" (Econometrica
82(6), 2014).  Checked against the April 29, 2003 draft; all numbering and pages
are the draft's.

Checks, symbolic or in exact rationals unless marked numeric:
  (1) Section 2 / Lemma 1: for the N = 3 Theorem 3 rule with leaving
      probability gamma, the closed form f = (u q A, A B, u p B)/Delta solves
      f = eta g0 + (1-eta) f T and sums to 1; it agrees with the series (1)
      and the matrix inverse eta g0 (I - (1-eta)T)^{-1}.
  (2) Eq. (4) (p.15) for a general 3-state rule without 1<->3 jumps.
  (3) Payoff Pi3, the exact gap to (1 + ((1-rho)/rho)^2)^{-1}, the payoff
      difference formula, and the optimal gamma*: gamma*^2 = eta((1-gamma*)^2+1),
      eta <= gamma*^2 <= 2 eta; a grid search agrees (numeric).
  (4) Corollary (i)-(ii) at N = 3 along gamma*(eta) -> numeric limits; and, as
      an independent numeric check beyond the Lean proofs, at N = 5 and N = 7
      the Theorem 3 rule with gamma = sqrt(eta) approaches the Corollary's
      payoff and the beliefs (rho/(1-rho))^(i-1) ((1-rho)/rho)^(N-i).
  (5) Absorbing extremes: limit payoff rho = gambler's ruin at N = 3; and the
      gambler's-ruin value (1 + ((1-rho)/rho)^((N-1)/2))^{-1} at N = 5 (numeric).
  (6) Lemma 4 identity and alpha* = 1 (p.16).
  (7) Theorem 6: one memory step multiplies the limiting odds by (rho/(1-rho))^2;
      overconfidence (i), underconfidence (iii).
  (8) Skeleton: Theorem 4 (i) pathwise inequality, exhaustive for N in {3,5,7};
      the Section 4 example (p.21) and the p.6 example; Theorem 5 (i):
      thresholds at N = 5, 7, 9 against the draft's N-1-(k-j) and the proof's
      2j-2+N-k.
  (9) Project question: Cov = pi(1-pi)(kH-kL); a likelihood depending on S only
      leaves Pr(Y|S) unchanged; a Y-dependent signal gives Cov = +-1/12.
Exit status 0 iff every check passes.
"""
import math
import sys
from itertools import product

import sympy as sp

R = sp.Rational
OK = True


def check(name, cond):
    global OK
    print(("  PASS  " if cond else "  FAIL  ") + name)
    if not cond:
        OK = False


eta, gam, rho, p = sp.symbols("eta gamma rho p", positive=True)
u = 1 - eta


def D3(e, g, pp):
    return e + g * (1 - e) - g * (2 - g) * (1 - e) ** 2 * pp * (1 - pp)


def fcl(e, g, pp):
    A = e + (1 - e) * g * (1 - pp)
    B = e + (1 - e) * g * pp
    D = D3(e, g, pp)
    return [(1 - e) * (1 - pp) * A / D, A * B / D, (1 - e) * pp * B / D]


def T3(g, pp):
    """Transition matrix of the N = 3 rule when h has probability pp."""
    q = 1 - pp
    return sp.Matrix([[1 - pp * g, pp * g, 0], [q, 0, pp], [0, q * g, 1 - q * g]])


def trans_matrix(N, g, pp):
    """Theorem 3 rule on N states (0-indexed), leaving extremes w.p. g."""
    q = 1 - pp
    T = [[0.0] * N for _ in range(N)]
    for i in range(N):
        if i == 0:
            T[0][1] += pp * g
            T[0][0] += 1 - pp * g
        elif i == N - 1:
            T[i][i - 1] += q * g
            T[i][i] += 1 - q * g
        else:
            T[i][i + 1] += pp
            T[i][i - 1] += q
    return T


def end_dist_numeric(N, e, g, pp):
    import numpy as np
    T = np.array(trans_matrix(N, g, pp))
    g0 = np.zeros(N)
    g0[N // 2] = 1.0
    return e * g0 @ np.linalg.inv(np.eye(N) - (1 - e) * T)


def main():
    print("(1) N = 3 closed form")
    for pp in (rho, 1 - rho):
        f = fcl(eta, gam, pp)
        T = T3(gam, pp)
        g0 = [0, 1, 0]
        for j in range(3):
            rhs = eta * g0[j] + u * sum(f[i] * T[i, j] for i in range(3))
            check(f"stationary eq, state {j + 1}, p = {pp}", sp.simplify(f[j] - rhs) == 0)
        check(f"sum f = 1, p = {pp}", sp.simplify(sum(f) - 1) == 0)
        check(f"T rows sum to 1, p = {pp}", all(sp.simplify(sum(T.row(i)) - 1) == 0 for i in range(3)))
    # series and matrix inverse at exact rational parameters
    ev, gv, rv = R(1, 10), R(1, 3), R(7, 10)
    T = T3(gv, rv)
    g0 = sp.Matrix([[0, 1, 0]])
    inv = ev * g0 * (sp.eye(3) - (1 - ev) * T).inv()
    f = [x.subs({eta: ev, gam: gv, rho: rv}) for x in fcl(eta, gam, rho)]
    check("matrix inverse = closed form", all(sp.simplify(inv[j] - f[j]) == 0 for j in range(3)))
    S = sp.zeros(1, 3)
    M = g0
    for t in range(400):
        S += ev * (1 - ev) ** t * M
        M = M * T
    check("series (1), 400 terms, = closed form to 1e-12 (numeric)",
          all(abs(float(S[j]) - float(f[j])) < 1e-12 for j in range(3)))

    print("(2) Eq. (4)")
    # general 3-state rule, no 1<->3 jumps, g0(3) = 0: only t23 and t32 enter
    tau = sp.symbols("t23 t32", nonnegative=True)
    f2, f3 = sp.symbols("f2 f3", positive=True)
    # balance at state 3: f3 = (1-eta)(f2 t23 + f3 (1 - t32))  =>  f3 (eta + (1-eta) t32) = (1-eta) f2 t23
    sol = sp.solve(sp.Eq(f3, u * (f2 * tau[0] + f3 * (1 - tau[1]))), f3)[0]
    check("balance at state 3 gives eq. (4)'s factor",
          sp.simplify(sol - u * f2 * tau[0] / (eta + u * tau[1])) == 0)
    xl, xh = sp.symbols("xl xh", nonnegative=True)
    tH = (1 - rho) * xl + rho * xh
    tL = rho * xl + (1 - rho) * xh
    check("(1-rho) tauH - rho tauL = -(2rho-1) xl  (up-move bound)",
          sp.simplify((1 - rho) * tH - rho * tL + (2 * rho - 1) * xl) == 0)
    check("(1-rho) tauL - rho tauH = -(2rho-1) xh  (down-move bound)",
          sp.simplify((1 - rho) * tL - rho * tH + (2 * rho - 1) * xh) == 0)

    print("(3) payoff, gap, optimal gamma")
    fH, fL = fcl(eta, gam, rho), fcl(eta, gam, 1 - rho)
    Pi = sp.Rational(1, 2) * (fL[0] + fH[1] + fH[2])
    Pi3 = (eta + u * gam * rho) * (eta + u * gam * (1 - rho) + 2 * u * rho) / (2 * D3(eta, gam, rho))
    check("payoff = Pi3", sp.simplify(Pi - Pi3) == 0)
    beta = rho ** 2 / (rho ** 2 + (1 - rho) ** 2)
    check("beta = (1 + ((1-rho)/rho)^2)^-1", sp.simplify(beta - 1 / (1 + ((1 - rho) / rho) ** 2)) == 0)
    gap = (2 * rho - 1) * (rho * (1 - rho) * u * (gam ** 2 * u + 2 * eta) + eta * (eta + gam * u)) / (
        2 * (rho ** 2 + (1 - rho) ** 2) * D3(eta, gam, rho))
    check("exact gap beta - Pi3", sp.simplify(beta - Pi3 - gap) == 0)
    g2 = sp.symbols("g2", positive=True)
    diff = rho * (1 - rho) * u ** 3 * (2 * rho - 1) * (gam - g2) * (eta * ((1 - gam) * (1 - g2) + 1) - gam * g2) / (
        2 * D3(eta, gam, rho) * D3(eta, g2, rho))
    check("payoff difference formula", sp.simplify(Pi3 - Pi3.subs(gam, g2) - diff) == 0)
    check("absorbing: Pi3(gamma=0) = rho - eta(rho - 1/2)",
          sp.simplify(Pi3.subs(gam, 0) - (rho - eta * (rho - R(1, 2)))) == 0)
    gstar = (sp.sqrt(2 * eta - eta ** 2) - eta) / (1 - eta)
    check("gamma*^2 = eta((1-gamma*)^2+1)", sp.simplify(sp.expand(gstar ** 2 - eta * ((1 - gstar) ** 2 + 1))) == 0)
    for ev in (0.3, 0.1, 0.01, 1e-4):
        gs = (math.sqrt(2 * ev - ev * ev) - ev) / (1 - ev)
        check(f"eta={ev}: eta <= gamma*^2 <= 2 eta (numeric)", ev <= gs * gs + 1e-15 and gs * gs <= 2 * ev + 1e-15)
        F = sp.lambdify(gam, Pi3.subs({eta: ev, rho: 0.7}))
        grid = [k / 20000 for k in range(20001)]
        best = max(grid, key=F)
        check(f"eta={ev}: grid argmax {best:.4f} ~ gamma* {gs:.4f} (numeric)", abs(best - gs) < 1e-3)

    print("(4) Corollary at N = 3 (gamma*) and N = 5, 7 (gamma = sqrt(eta)), numeric")
    rv = 0.7
    b3 = rv ** 2 / (rv ** 2 + (1 - rv) ** 2)
    for ev in (1e-2, 1e-4, 1e-6):
        gs = (math.sqrt(2 * ev - ev * ev) - ev) / (1 - ev)
        pv = float(Pi3.subs({eta: ev, gam: gs, rho: rv}))
        lr = float((fH[2] / fL[2]).subs({eta: ev, gam: gs, rho: rv}))
        print(f"      N=3 eta={ev:g}: payoff {pv:.6f} (bound {b3:.6f}), f3H/f3L {lr:.4f} (limit {(rv / (1 - rv)) ** 2:.4f})")
    ev = 1e-8
    gs = (math.sqrt(2 * ev - ev * ev) - ev) / (1 - ev)
    check("N=3: payoff within 1e-3 of bound at eta=1e-8",
          abs(float(Pi3.subs({eta: ev, gam: gs, rho: rv})) - b3) < 1e-3)
    try:
        import numpy  # noqa: F401
        have_np = True
    except ImportError:
        have_np = False
    if have_np:
        for N in (5, 7):
            ev = 1e-9
            gv = math.sqrt(ev)
            fh = end_dist_numeric(N, ev, gv, rv)
            fl = end_dist_numeric(N, ev, gv, 1 - rv)
            a = [0] * (N // 2) + [1] * (N - N // 2)
            pay = 0.5 * sum(fh[i] * a[i] + fl[i] * (1 - a[i]) for i in range(N))
            bound = 1 / (1 + ((1 - rv) / rv) ** (N - 1))
            check(f"N={N}: Theorem 3 rule, gamma=sqrt(eta), payoff {pay:.5f} ~ Corollary (i) {bound:.5f}",
                  abs(pay - bound) < 2e-3)
            ok = True
            for i in range(N):
                lim = (rv / (1 - rv)) ** (i) * ((1 - rv) / rv) ** (N - 1 - i)
                if abs(math.log(fh[i] / fl[i]) - math.log(lim)) > 5e-3:
                    ok = False
            check(f"N={N}: beliefs ~ Corollary (ii) (log-odds within 5e-3)", ok)
            fh0 = end_dist_numeric(N, ev, 0.0, rv)
            fl0 = end_dist_numeric(N, ev, 0.0, 1 - rv)
            pay0 = 0.5 * sum(fh0[i] * a[i] + fl0[i] * (1 - a[i]) for i in range(N))
            ruin = 1 / (1 + ((1 - rv) / rv) ** ((N - 1) / 2))
            check(f"N={N}: absorbing extremes -> gambler's ruin {ruin:.5f} (got {pay0:.5f})", abs(pay0 - ruin) < 1e-5)
    else:
        print("      (numpy not available: N = 5, 7 numeric checks skipped)")

    print("(5) absorbing at N = 3")
    check("lim Pi3(eta, 0) = rho", sp.limit(Pi3.subs(gam, 0), eta, 0) == rho)
    check("rho < beta on (1/2, 1)", all(r < b for r, b in
          ((rv_, rv_ ** 2 / (rv_ ** 2 + (1 - rv_) ** 2)) for rv_ in (0.51, 0.6, 0.7, 0.9, 0.99))))

    print("(6) Lemma 4 and alpha* = 1")
    r, xx, al, q = sp.symbols("r x alpha q", positive=True)
    lhs = (1 + r) * (1 + xx) * (1 + r ** 2 / xx) * (2 / (1 + r) - (1 / (1 + xx) + 1 / (1 + r ** 2 / xx)))
    check("Lemma 4 identity", sp.simplify(lhs - (1 - r) / xx * (xx - r) ** 2) == 0)
    val = R(1, 2) * (1 / (1 + q / al) + 1 / (1 + al * q))
    check("alpha* = 1 attains 1/(1+q)", sp.simplify(val.subs(al, 1) - 1 / (1 + q)) == 0)
    check("1/(1+q) - value = q(1-q)(alpha-1)^2 / (2(1+q)(alpha+q)(1+alpha q)) >= 0",
          sp.simplify(1 / (1 + q) - val - q * (1 - q) * (al - 1) ** 2 / (2 * (1 + q) * (al + q) * (1 + al * q))) == 0)

    print("(7) Theorem 6")
    N_, i_ = 7, 3
    def lrl(rv_, N, i):
        return (rv_ / (1 - rv_)) ** (i - 1) * ((1 - rv_) / rv_) ** (N - i)
    rr = R(7, 10)
    check("lrLimit(i+1) = (rho/(1-rho))^2 lrLimit(i), N=7, all i",
          all(lrl(rr, 7, i + 1) == (rr / (1 - rr)) ** 2 * lrl(rr, 7, i) for i in range(1, 7)))
    t = rr / (1 - rr)
    op = lambda k: t ** k / (1 + t ** k)
    check("Theorem 6 (i): odds^(2D) more confident than odds^D, D=1..5",
          all(op(2 * D) > op(D) for D in range(1, 6)))
    check("Theorem 6 (iii): delta > N-1 beats every memory state (N=7)",
          all(op(d) > op(k) for d in range(7, 12) for k in range(0, 7)))

    print("(8) skeleton")
    def skel(N, i, s):
        if i <= 1 or i >= N:
            return i
        return i + 1 if s == "h" else i - 1

    def run(N, i, w):
        for s in w:
            i = skel(N, i, s)
        return i

    viol = 0
    strict = 0
    for N in (3, 5, 7):
        for c in range(2, N):
            for tau in range(0, 4):
                for L in range(0, 8):
                    for w in product("lh", repeat=L):
                        a = run(N, c, w + ("h",) * tau)
                        b = run(N, c, ("h",) * tau + w)
                        viol += a > b
                        strict += a < b
    check(f"Theorem 4 (i) pathwise: 0 violations, {strict} strict cases", viol == 0 and strict > 0)
    ords = ["lhhl", "lhlh", "llhh", "hllh", "hlhl", "hhll"]
    got = [(run(5, 2, w), run(5, 4, w)) for w in ords]
    check(f"Section 4 example (p.21): {got}", got == [(1, 5), (1, 4), (1, 4), (1, 5), (2, 5), (2, 5)])
    check("C(4,2) = 6 orderings means tau = 2 reports of each type (text: tau = 4)",
          math.comb(4, 2) == 6 and math.comb(8, 4) == 70)
    check("p.6 example: N=4, (2,3), l h h -> (1,4)", (run(4, 2, "lhh"), run(4, 3, "lhh")) == (1, 4))

    def nxt(N, i, s):
        return {skel(N, i, s)} if 1 < i < N else set(range(1, N + 1))

    def reach(N, i, w):
        S = {i}
        for s in w:
            S = set().union(*[nxt(N, x, s) for x in S])
        return S

    fails = []
    thr_ok = True
    for N in (5, 7, 9):
        for j in range(2, N):
            for k in range(j + 1, N):
                thr = min(2 * j - 1, 2 * (N - k) + 1)
                for t in range(0, 2 * N + 1):
                    ws = list(product("lh", repeat=t))
                    sk = any(run(N, j, w) < j and run(N, k, w) > k for w in ws)
                    if sk != (t >= thr):
                        thr_ok = False
                draft = N - 1 - (k - j)
                if draft < thr:
                    ws = list(product("lh", repeat=draft))
                    su = any(min(reach(N, j, w)) < j and max(reach(N, k, w)) > k for w in ws)
                    if not su:
                        fails.append((N, j, k, draft))
                if 2 * j - 2 + N - k < thr:
                    thr_ok = False
    check("skeleton thresholds = min(2j-1, 2(N-k)+1), and the proof's 2j-2+N-k is above them, N = 5, 7, 9",
          thr_ok)
    print(f"      draft bound N-1-(k-j) insufficient even on the superset at: {fails}")
    check("draft Theorem 5 (i) fails at (N,j,k,t) = (5,2,4,2) and (7,3,5,4)",
          (5, 2, 4, 2) in fails and (7, 3, 5, 4) in fails)

    print("(9) project question")
    pi_, kH, kL = sp.symbols("pi kH kL")
    cov = pi_ * kH - pi_ * (pi_ * kH + (1 - pi_) * kL)
    check("Cov = pi(1-pi)(kH-kL)", sp.expand(cov - pi_ * (1 - pi_) * (kH - kL)) == 0)
    qs = sp.symbols("q00 q01 q10 q11", positive=True)
    l0, l1, Z = sp.symbols("l0 l1 Z", positive=True)
    qm = {(0, 0): qs[0], (0, 1): qs[1], (1, 0): qs[2], (1, 1): qs[3]}
    post = {k: (l1 if k[0] else l0) * v / Z for k, v in qm.items()}
    check("S-only likelihood keeps Pr(Y=1|S) for S = 0, 1",
          all(sp.simplify(post[(s, 1)] / (post[(s, 0)] + post[(s, 1)]) - qm[(s, 1)] / (qm[(s, 0)] + qm[(s, 1)])) == 0
              for s in (0, 1)))
    for sign, (a, b) in ((1, (2, 1)), (-1, (1, 2))):
        qq = {(s, y): R(a if s == y else b, 6) for s in (0, 1) for y in (0, 1)}
        c = qq[(1, 1)] - (qq[(1, 1)] + qq[(1, 0)]) * (qq[(1, 1)] + qq[(0, 1)])
        check(f"Y-dependent signal: Cov = {c}", c == sign * R(1, 12))

    print("\nALL PASS" if OK else "\nSOME CHECKS FAILED")
    return 0 if OK else 1


if __name__ == "__main__":
    sys.exit(main())
