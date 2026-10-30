"""Writes results_q3_regimes.md: Question 3, the four regimes of Bayraktar et al. (2026),
Section 4.2 and Figure 4, in the theory paper's model.

Regimes, all under a common ceiling pi <= PI_MAX on the risky exposure, so the admissible
sets are nested (their device):
    none        pi <= PI_MAX
    solvency    pi <= min((1 - 1/x)/a1, PI_MAX)
    liquidity   pi <= min((1 - a2/x)/a3, PI_MAX)
    both        pi <= min((1 - 1/x)/a1, (1 - a2/x)/a3, PI_MAX)   (A2 with the ceiling)
PI_MAX = 4. Unregulated, the value is finite only if the long-run exposure stays below
(rho - r)/(mu - r) = 5 at these parameters (their Assumption 3.1); the solvency branch
alone tends to 1/a1 = 22, so it needs the ceiling too. With PI_MAX = 4 above the
liquidity branch's limit 1/a3 = 3.33, "both" is A2 itself.

1. Leveraged case, 1201 nodes, (lambda_S, K) = (1.2, 0.05) and lambda_S = 1, 1.5: the
   thresholds, the value function at 1.2, the value loss against "none", the share of
   the inaction region at the cap, buffer variance at the trigger and at the barrier
   against the deficit limit and the surplus limit, and the determinant of M3 by its
   formula and by finite differences (h = 0.1 in lambda_S, 0.01 in K), with the
   hypotheses of M1, M2 and M4.
2. No borrowing, PI_MAX = 1, 601 nodes: the largest difference in V between "both" and
   "solvency" and between "liquidity" and "none", which C3 says are zero once
   a2 <= (1 - a3) x.

Under "liquidity" and "none" the exposure stays positive at the distress boundary, so
the bank holds risk at zero equity there and the state reaches x = 1. There the bank
must issue or close (solve_howard_closed); the reflection of solve_qvi.solve_howard
would be a free injection. These regimes are devices for the decomposition, not
regulations in force.

No row of the manuscript rests on these numbers.

    python3 run_q3_regimes.py
"""
import hashlib
import json
import os
import sys
from multiprocessing import Pool

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import numpy as np  # noqa: E402
import solve_qvi as s  # noqa: E402

PI_MAX = 4.0
REGIMES = ("none", "solvency", "liquidity", "both")
LAMS = (1.0, 1.2, 1.5)
N = 1201


def cap(x, p):
    reg, top = p["regime"], p["pi_max"]
    t1 = (1 / p["a1"]) * (1 - 1 / x)
    t3 = (1 / p["a3"]) * (1 - p["a2"] / x)
    if reg == "none":
        c = np.full_like(np.asarray(x, float), top)
    elif reg == "solvency":
        c = np.minimum(t1, top)
    elif reg == "liquidity":
        c = np.minimum(t3, top)
    else:
        c = np.minimum(np.minimum(t1, t3), top)
    return np.maximum(0.0, c)


s.pi_bar = cap
_howard_reflecting = s.solve_howard


def solve_howard_closed(p, g=s.GRID, v0=None, max_iter=300):
    """solve_qvi.solve_howard with one change at the distress boundary x = 1. There the
    bank must issue or close (value 0) and cannot continue. solve_howard lets the bank
    continue at x = 1 with the lower neighbour folded into the diagonal, a reflection.
    Under A2 the exposure is zero at x = 1, the variance vanishes and the drift
    r - mu_L + gamma is positive, so the state does not reach x = 1 and the two agree.
    Where a regime leaves the exposure positive at x = 1 the reflection is a free
    injection, and the bank pays dividends at every state."""
    x = np.linspace(1.0, g["x_max"], g["n"])
    dx = x[1] - x[0]
    n = len(x)
    if v0 is None:
        _, v0, _, _ = s.solve(p, dict(g, tol=1e-9))
    v = v0.copy()
    calm = 0
    rows = np.arange(n)
    for it in range(max_iter):
        G, I, D, pi, lo, di, up, targ, lam_pen = s.qvi_terms(x, v, p)
        G[0] = -np.inf
        C = np.full(n, -np.inf)
        C[0] = -v[0]
        act = np.argmax(np.stack([G * dx ** 2, I, D, C]), axis=0)
        act[-1] = 2
        A = np.zeros((n, n))
        r = np.zeros(n)
        ci = rows[act == 0]
        A[ci, ci] = di[ci]
        A[ci, ci - 1] = lo[ci]
        A[ci, ci + 1] = up[ci]
        r[ci] = lam_pen[ci]
        ii = rows[act == 1]
        A[ii, ii] = 1.0
        A[ii, targ[ii]] -= 1.0
        r[ii] = -p["K"] - (1 + p["kappa"]) * (x[targ[ii]] - x[ii])
        di_ = rows[act == 2]
        A[di_, di_] = 1.0
        A[di_, di_ - 1] = -1.0
        r[di_] = dx
        if act[0] == 3:
            A[0, 0] = 1.0
            r[0] = 0.0
        v_new = np.linalg.solve(A, r)
        change = np.max(np.abs(v_new - v))
        v = v_new
        calm = calm + 1 if change < 1e-12 else 0
        if calm >= 3:
            break
    G, I, D, *_ = s.qvi_terms(x, v, p)
    G[0] = -np.inf
    C = np.full(n, -np.inf)
    C[0] = -v[0]
    resid = np.max(np.abs(np.maximum.reduce([G[:-1] * dx ** 2, I[:-1], D[:-1], C[:-1]])))
    return x, v, it, resid


s.solve_howard = solve_howard_closed


def sha(path):
    return hashlib.sha256(open(path, "rb").read()).hexdigest()


def params(reg, lam, top=PI_MAX):
    return dict(s.BASE, r=0.02, kappa=0.01, lam=lam, K=0.05, regime=reg, pi_max=top)


def limits(p):
    q = lambda w: w * w * p["sigma"] ** 2 - 2 * w * p["c"] * p["sigma"] * p["sigma_L"] + p["sigma_L"] ** 2
    return dict(nu1=q(1 / p["a1"]), nu3=q(1 / p["a3"]))


def levels(args):
    reg, lam = args
    p = params(reg, lam)
    x, v, it, resid = s.solve_howard(p, dict(s.GRID, n=N))
    th = s.thresholds(x, v, p)
    q = th["q"]
    iL, ist = q["iL"], q["istar"]
    xs, pis = x[iL:ist + 1], q["pi"][iL:ist + 1]
    zeta = s.sig2(xs, pis, p) / (xs - 1) ** 2
    at_cap = np.isclose(pis, cap(xs, p), rtol=1e-9)
    return reg, lam, dict(y_star=th["y_star"], x_L=th["x_L"], y_post=th["y_post"], v12=float(np.interp(1.2, x, v)),
                          resid=resid, cap_share=float(at_cap.mean()), zeta_L=float(zeta[0]), zeta_star=float(zeta[-1]),
                          kind="distress boundary" if iL <= 1 else "interior")


def determinant(reg):
    o = s.check_point(params(reg, 1.2), N, 0.1, 0.01)
    return reg, dict(formula=o["formula"], numeric=o["numeric"], det_formula=o["det_formula"],
                     det_numeric=o["det_numeric"], W=o["W_y_star"],
                     failed=[k for k, v in o["hypotheses"].items() if not v])


def noborrow(reg):
    p = params(reg, 1.2, top=1.0)
    x, v, it, resid = s.solve_howard(p, dict(s.GRID, n=601))
    return reg, v


def main():
    p0 = dict(s.BASE, r=0.02, kappa=0.01, lam=1.2, K=0.05, regime="both", pi_max=PI_MAX)
    xa, va, _, _ = solve_howard_closed(p0, dict(s.GRID, n=601))
    xb, vb, _, _ = _howard_reflecting(p0, dict(s.GRID, n=601))
    agree = float(np.max(np.abs(va - vb)))
    print(f"closed against reflecting under both: {agree:.2e}", flush=True)
    with Pool(8) as pool:
        lev = pool.map(levels, [(r, l) for r in REGIMES for l in LAMS])
        print("levels done", flush=True)
        dets = pool.map(determinant, REGIMES)
        print("determinants done", flush=True)
        nb = dict(pool.map(noborrow, REGIMES))
        print("no borrowing done", flush=True)
    m = {(r, l): o for r, l, o in lev}
    d = dict(dets)
    L0 = limits(params("both", 1.2))
    pins = ", ".join(f"`{f}` sha256 {sha(os.path.join(HERE, f))}" for f in ("solve_qvi.py", "run_q3_regimes.py"))
    L = ["# Results: Question 3, the four regimes (generated)", "",
         f"Generated by `run_q3_regimes.py`. Pinned: {pins}.", "",
         f"Parameters of Bayraktar et al. (2026) with r = 0.02, kappa = 0.01, K = 0.05, R = 1.15, common ceiling "
         f"pi <= {PI_MAX}, {N} nodes on [1, 2.5]. Deficit limit nu_1^2 = {L0['nu1']:.4f}, surplus limit "
         f"nu_3^2 = {L0['nu3']:.4f}. Buffer variance zeta = sigma^2(x, pi)/(x - 1)^2.", "",
         "## 0. The boundary rule", "",
         "At x = 1 the bank must issue or close; it cannot continue (`solve_howard_closed`). Under the cap of A2 with "
         f"the ceiling, (lambda_S, K) = (1.2, 0.05), 601 nodes, V differs from that of `solve_qvi.solve_howard` by at most "
         f"{agree:.2e}.", "",
         "## 1. Leveraged case", "",
         "### Thresholds and value", "",
         "| regime | lambda_S | x_L | y* | y_post | v(1.2) | value loss against none | share of inaction at the cap | "
         "zeta at x_L | zeta at y* | trigger | QVI residual |", "|---|---|---|---|---|---|---|---|---|---|---|---|"]
    for r in REGIMES:
        for l in LAMS:
            o = m[(r, l)]
            L.append(f"| {r} | {l} | {o['x_L']:.4f} | {o['y_star']:.4f} | {o['y_post']:.4f} | {o['v12']:.4f} | "
                     f"{m[('none', l)]['v12'] - o['v12']:.4f} | {o['cap_share']:.2f} | {o['zeta_L']:.4f} | "
                     f"{o['zeta_star']:.4f} | {o['kind']} | {o['resid']:.1e} |")
    L += ["", "### Spread across lambda_S in {1, 1.2, 1.5}", "",
          "| regime | x_L | y* | y_post | v(1.2) |", "|---|---|---|---|---|"]
    for r in REGIMES:
        sp = lambda k: max(m[(r, l)][k] for l in LAMS) - min(m[(r, l)][k] for l in LAMS)
        L.append(f"| {r} | {sp('x_L'):.4f} | {sp('y_star'):.4f} | {sp('y_post'):.4f} | {sp('v12'):.4f} |")
    L += ["", "### Determinant of M3 at (lambda_S, K) = (1.2, 0.05)", "",
          "| regime | dy*/dlam formula, FD | dx_L/dlam formula, FD | dy*/dK formula, FD | dx_L/dK formula, FD | "
          "J formula | J FD | W(y*) | hypotheses failing |", "|---|---|---|---|---|---|---|---|---|"]
    for r in REGIMES:
        o = d[r]
        f, n = o["formula"], o["numeric"]
        L.append(f"| {r} | {f['dystar_dlam']:.4f}, {n['dystar_dlam']:.4f} | {f['dxL_dlam']:.4f}, {n['dxL_dlam']:.4f} | "
                 f"{f['dystar_dK']:.3f}, {n['dystar_dK']:.3f} | {f['dxL_dK']:.3f}, {n['dxL_dK']:.3f} | "
                 f"{o['det_formula']:.4f} | {o['det_numeric']:.4f} | {o['W']:.4f} | "
                 f"{', '.join(o['failed']) if o['failed'] else 'none'} |")
    L += ["", "## 2. No borrowing, pi <= 1, 601 nodes, (lambda_S, K) = (1.2, 0.05)", "",
          f"max |V_both - V_solvency| = {np.max(np.abs(nb['both'] - nb['solvency'])):.2e}, "
          f"max |V_liquidity - V_none| = {np.max(np.abs(nb['liquidity'] - nb['none'])):.2e}. "
          "C3 (`BCVW.ceiling_tighter_than_lcr`) says both are zero, since a2 = 0.05 <= (1 - a3) x for every x >= 1.", ""]
    open(os.path.join(HERE, "results_q3_regimes.md"), "w").write("\n".join(L))
    json.dump(dict(levels=[(r, l, o) for r, l, o in lev], determinants=d), open(os.path.join(HERE, "results_q3_regimes.json"), "w"),
              indent=1, default=float)
    print("written results_q3_regimes.md")


if __name__ == "__main__":
    main()
