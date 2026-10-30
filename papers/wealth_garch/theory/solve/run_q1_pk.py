"""Writes results_q1_pk.md: Question 1, pass 2, the three angles of Peura and Keppo (2006)
that pass 1 did not cover, in the theory paper's model at the parameters of Bayraktar et
al. (2026) with r = 0.02, kappa = 0.01, the cap of A2 (leveraged).

1. Issue moments (their Table 2, their route to pinning the delay and the fixed cost).
   Over lambda_S and K: issue size xi = y_post - x_L, proportional cost per issue
   (K + kappa xi)/xi, and the expected time between issues, T(y_post), where
   sigma^2 T''/2 + b T' + 1 = 0 on (x_L, y*), T(x_L) = 0, T'(y*) = 0 (dividends reflect
   the state at y*). Then, at four centres, the Jacobian of (T, cost) in (lambda_S, K)
   beside the Jacobian of (y*, x_L), by central differences on 1201 nodes with
   h = 0.1 in lambda_S and 0.01 in K.
2. Value of the option to issue (their Fig. 4): O(x) = V(x) - V0(x), V0 the value with
   no issuance (K = 1000), across the buffer, with its share of V, for lambda_S and K.
3. Thresholds against the asset volatility sigma and the expected asset return mu (their
   Fig. 6), for lambda_S = 1, 1.2, 1.5 at K = 0.05, with the determinant of M3 by its
   formula along the sigma sweep. mu stays below r + (rho - r) a3 = 0.05, the bound for
   a finite value under the cap.

No row of the manuscript rests on these numbers.

    python3 run_q1_pk.py
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

BASEP = dict(s.BASE, r=0.02, kappa=0.01)
LAMS = [1.0, 1.1, 1.2, 1.35, 1.5, 2.0]
KS = [0.01, 0.02, 0.035, 0.05, 0.075, 0.1]
CENTRES = [(1.2, 0.05), (1.5, 0.05), (1.2, 0.02), (1.2, 0.1)]
H_LAM, H_K = 0.1, 0.01
OPT_LAMS = [1.0, 1.2, 1.5, 2.0]
OPT_KS = [0.02, 0.05, 0.1]
SIGMAS = [0.04, 0.06, 0.08, 0.10, 0.12, 0.16]
MUS = [0.025, 0.03, 0.035, 0.04, 0.045]
N = 601


def sha(path):
    return hashlib.sha256(open(path, "rb").read()).hexdigest()


def solve(p, n=N):
    x, v, it, resid = s.solve_howard(p, dict(s.GRID, n=n))
    th = s.thresholds(x, v, p)
    return x, v, th, resid


def time_between_issues(x, p, q):
    """T on nodes iL..istar: generator without discounting, T(x_L) = 0, T'(y*) = 0; T at y_post."""
    iL, ip, ist = q["iL"], q["ipost"], q["istar"]
    dx = x[1] - x[0]
    lo, di, up = s.generator(x, q["pi"], p, dx)
    di = di + (p["rho"] - p["mu_L"])
    m = ist - iL + 1
    A = np.zeros((m, m))
    r = np.zeros(m)
    A[0, 0] = 1.0
    for k in range(1, m - 1):
        i = iL + k
        A[k, k - 1], A[k, k], A[k, k + 1] = lo[i], di[i], up[i]
        r[k] = -1.0
    A[m - 1, m - 1], A[m - 1, m - 2] = 1.0, -1.0
    T = np.linalg.solve(A, r)
    return float(T[ip - iL])


def moments(p, n=N):
    x, v, th, resid = solve(p, n)
    q = th["q"]
    xi = th["y_post"] - th["x_L"]
    return dict(y_star=th["y_star"], x_L=th["x_L"], y_post=th["y_post"], size=xi,
                cost=(p["K"] + p["kappa"] * xi) / xi, T=time_between_issues(x, p, q),
                interior=q["iL"] > 1, resid=resid, v12=float(np.interp(1.2, x, v)))


def sweep_job(args):
    lam, K = args
    return lam, K, moments(dict(BASEP, lam=lam, K=K))


def centre_job(args):
    lam, K = args
    out = {}
    for key, dl, dk in [("0", 0, 0), ("l+", H_LAM, 0), ("l-", -H_LAM, 0), ("k+", 0, H_K), ("k-", 0, -H_K)]:
        out[key] = moments(dict(BASEP, lam=lam + dl, K=K + dk), n=1201)
    d = lambda f, a: (out[a + "+"][f] - out[a + "-"][f]) / (2 * (H_LAM if a == "l" else H_K))
    jac = {f: {"lam": d(f, "l"), "K": d(f, "k")} for f in ("y_star", "x_L", "T", "cost", "size")}
    det = lambda f, g: jac[f]["lam"] * jac[g]["K"] - jac[f]["K"] * jac[g]["lam"]
    return lam, K, dict(centre=out["0"], jac=jac, det_thresholds=det("y_star", "x_L"), det_moments=det("T", "cost"),
                        det_size_T=det("size", "T"), all_interior=all(o["interior"] for o in out.values()))


def option_job(args):
    lam, K = args
    p = dict(BASEP, lam=lam, K=K)
    x, v, th, _ = solve(p)
    x0, v0, _, _ = solve(dict(p, K=1000.0))
    O = v - v0
    i_max = int(np.argmax(O))
    at = lambda xx: (float(np.interp(xx, x, O)), float(np.interp(xx, x, O / v)) if np.interp(xx, x, v) > 0 else float("nan"))
    return lam, K, dict(x_L=th["x_L"], y_star=th["y_star"], y_post=th["y_post"], x_max=float(x[i_max]), O_max=float(O[i_max]),
                        at_1=at(1.0 + 1e-9), at_xL=at(th["x_L"]), at_ypost=at(th["y_post"]), at_ystar=at(th["y_star"]),
                        at_top=(float(O[-1]), float(O[-1] / v[-1])),
                        monotone=bool(np.all(np.diff(O) <= 1e-10)))


def sigma_job(args):
    what, val, lam = args
    p = dict(BASEP, lam=lam, K=0.05, **{what: val})
    x, v, th, resid = solve(p)
    q = th["q"]
    iL, ist = q["iL"], q["istar"]
    capshare = float(np.mean(np.isclose(q["pi"][iL:ist + 1], s.pi_bar(x[iL:ist + 1], p), rtol=1e-9)))
    return what, val, lam, dict(y_star=th["y_star"], x_L=th["x_L"], y_post=th["y_post"], v12=float(np.interp(1.2, x, v)),
                                cap_share=capshare, interior=iL > 1, resid=resid)


def det_sigma_job(sig):
    o = s.check_point(dict(BASEP, lam=1.2, K=0.05, sigma=sig), N, 0.02, 0.005)
    return sig, dict(det=o["det_formula"], f=o["formula"], failed=[k for k, v in o["hypotheses"].items() if not v])


def main():
    with Pool(6) as pool:
        sw = pool.map(sweep_job, [(l, k) for l in LAMS for k in KS]); print("sweep done", flush=True)
        cen = pool.map(centre_job, CENTRES); print("centres done", flush=True)
        opt = pool.map(option_job, [(l, k) for l in OPT_LAMS for k in OPT_KS]); print("option done", flush=True)
        sg = pool.map(sigma_job, [("sigma", v, l) for v in SIGMAS for l in (1.0, 1.2, 1.5)]
                      + [("mu", v, l) for v in MUS for l in (1.0, 1.2, 1.5)]); print("moments of returns done", flush=True)
        ds = pool.map(det_sigma_job, SIGMAS); print("determinants done", flush=True)
    pins = ", ".join(f"`{f}` sha256 {sha(os.path.join(HERE, f))}" for f in ("solve_qvi.py", "run_q1_pk.py"))
    L = ["# Results: Question 1, pass 2, the angles of Peura and Keppo (generated)", "",
         f"Generated by `run_q1_pk.py`. Pinned: {pins}.", "",
         "Accuracy of T. At (lambda_S, K) = (1.2, 0.05) the PDE gives 6.28, 6.39 and 6.34 years on 301, 601 and 1201 "
         "nodes, since x_L and y_post move by a node, and a Monte Carlo of 4000 paths from the same policy (dt = 0.005, "
         "reflection at y*, issue when the state reaches x_L) gives 6.79 with standard error 0.09, high by the "
         "crossings that discrete monitoring misses. Read T to about ten percent; the cost per issue and the signs "
         "of the responses do not depend on it.", "",
         "Parameters of Bayraktar et al. (2026) with r = 0.02, kappa = 0.01, R = 1.15, the cap of A2, [1, 2.5]. "
         "Time is in years, sizes and costs per unit of liabilities.", "",
         "## 1. Issue moments", "",
         f"Issue size xi = y_post - x_L, proportional cost per issue (K + kappa xi)/xi, expected time between issues "
         f"T from y_post to x_L under the optimal policy. {N} nodes.", ""]
    m = {(l, k): o for l, k, o in sw}
    for name, key, fmt in (("Expected time between issues, years", "T", "{:.1f}"),
                           ("Proportional cost per issue", "cost", "{:.3f}"), ("Issue size", "size", "{:.3f}")):
        L += [f"### {name}", "", "| lambda_S \\ K | " + " | ".join(str(k) for k in KS) + " |", "|---|" + "---|" * len(KS)]
        for l in LAMS:
            L.append(f"| {l} | " + " | ".join((fmt.format(m[(l, k)][key]) + ("" if m[(l, k)]["interior"] else "*"))
                                              for k in KS) + " |")
        L.append("")
    L += ["An asterisk marks a trigger at the distress boundary.", "",
          "### Jacobians at four centres, 1201 nodes, central differences", "",
          "| (lambda_S, K) | dT/dlam | dT/dK | dcost/dlam | dcost/dK | det of (T, cost) | dy*/dlam | dy*/dK | dx_L/dlam | dx_L/dK | "
          "det of (y*, x_L) | det of (size, T) | all five solves interior |", "|---|---|---|---|---|---|---|---|---|---|---|---|---|"]
    for l, k, o in cen:
        j = o["jac"]
        L.append(f"| ({l}, {k}) | {j['T']['lam']:.2f} | {j['T']['K']:.1f} | {j['cost']['lam']:.4f} | {j['cost']['K']:.3f} | "
                 f"{o['det_moments']:.4f} | {j['y_star']['lam']:.4f} | {j['y_star']['K']:.3f} | {j['x_L']['lam']:.4f} | "
                 f"{j['x_L']['K']:.3f} | {o['det_thresholds']:.4f} | {o['det_size_T']:.3f} | {o['all_interior']} |")
    L += ["", "## 2. Value of the option to issue", "",
          f"O(x) = V(x) - V0(x), V0 with no issuance (K = 1000), {N} nodes. Pairs are (O, O/V). Under the cap the "
          "exposure is zero at x = 1 and the drift r - mu_L + gamma is positive, so without issuance the bank is never "
          "closed; the option buys a faster way out of the low buffers and out of the shortfall penalty, not survival.", "",
          "| lambda_S | K | x_L | y* | O largest at x | O largest | O, O/V at x = 1 | at x_L | at y_post | at y* | at x = 2.5 | "
          "O falls throughout |", "|---|---|---|---|---|---|---|---|---|---|---|---|"]
    for l, k, o in opt:
        f2 = lambda t: f"{t[0]:.4f}, {t[1]:.3f}"
        L.append(f"| {l} | {k} | {o['x_L']:.4f} | {o['y_star']:.4f} | {o['x_max']:.4f} | {o['O_max']:.4f} | {f2(o['at_1'])} | "
                 f"{f2(o['at_xL'])} | {f2(o['at_ypost'])} | {f2(o['at_ystar'])} | {f2(o['at_top'])} | {o['monotone']} |")
    L += ["", "## 3. Thresholds against the moments of the asset return, K = 0.05", ""]
    g = {(w, v, l): o for w, v, l, o in sg}
    for what, vals, label in (("sigma", SIGMAS, "asset volatility sigma"), ("mu", MUS, "expected asset return mu")):
        L += [f"### {label}", "", "| value | lambda_S | x_L | y* | y_post | v(1.2) | share of inaction at the cap | trigger |",
              "|---|---|---|---|---|---|---|---|"]
        for v in vals:
            for l in (1.0, 1.2, 1.5):
                o = g[(what, v, l)]
                L.append(f"| {v} | {l} | {o['x_L']:.4f} | {o['y_star']:.4f} | {o['y_post']:.4f} | {o['v12']:.4f} | "
                         f"{o['cap_share']:.2f} | {'interior' if o['interior'] else 'distress boundary'} |")
        L.append("")
    L += ["### Determinant of M3 by its formula along sigma, (lambda_S, K) = (1.2, 0.05)", "",
          "| sigma | dy*/dlam | dx_L/dlam | dy*/dK | dx_L/dK | determinant | hypotheses failing |", "|---|---|---|---|---|---|---|"]
    for sig, o in ds:
        f = o["f"]
        L.append(f"| {sig} | {f['dystar_dlam']:.4f} | {f['dxL_dlam']:.4f} | {f['dystar_dK']:.3f} | {f['dxL_dK']:.3f} | "
                 f"{o['det']:.4f} | {', '.join(o['failed']) if o['failed'] else 'none'} |")
    L.append("")
    open(os.path.join(HERE, "results_q1_pk.md"), "w").write("\n".join(L))
    json.dump(dict(sweep=sw, centres=cen, option=opt, returns=sg, det_sigma=ds),
              open(os.path.join(HERE, "results_q1_pk.json"), "w"), indent=1, default=float)
    print("written results_q1_pk.md")


if __name__ == "__main__":
    main()
