"""Writes results_q7.md: Question 7, safety against value. The theory paper's model at
the parameters of Bayraktar et al. (2026) (r = 0.02, kappa = 0.01, the cap of A2,
leveraged), solved on 1201 nodes and simulated by q7_paths.py.

Value is v(1.2) and the shareholders' cash value C(1.2) = v(1.2) + (lambda_S - 1) W(1.2),
the expected discounted dividends less issue costs with the shortfall penalty added
back, since v contains the penalty and falls in lambda_S by construction. Both are under
the measure of the value equation, W and N by Feynman-Kac (solve_qvi.feynman_kac).
Safety is over five years from x0 = 1.2, the horizon and starting state of Bayraktar
et al.'s frontier, under the physical measure: the probability of no deficit (never
below the reference R), the expected time in deficit, the expected undiscounted
shortfall, the expected number of issues and the capital raised, the probability of
never reaching 1.02 (their survival probability), and the average risky exposure and
its ratio to the cap along the path.

1. Validation, at (lambda_S, K) = (1.2, 0.05): the simulator under the measure of the
   value equation, 60 years, against v(1.2), W(1.2) and N(1.2).
2. lambda_S = 1, 1.1, 1.2, 1.35, 1.5, 2, 3 at K = 0.02 and 0.05, the solvency floor at
   0.045.
3. The solvency floor a1 = 0.045, 0.06, 0.08, 0.10, 0.12 at lambda_S = 1 and 1.2,
   K = 0.05: Bayraktar et al.'s lever, and the question of Koehn and Santomero as
   reported by Milne and Whalley (L67), whether a higher floor raises the exposure the
   bank chooses below the cap.
4. K = 0.01 to 0.1 at lambda_S = 1.2.
5. The two levers at equal safety: for each point of the a1 path at lambda_S = 1, the
   lambda_S on the lambda_S path at K = 0.05 that gives the same expected time in
   deficit, and the cash value of each.

Common random numbers (one seed) across points. No row of the manuscript rests on
these numbers.

    python3 run_q7_safety.py
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
import q7_paths as q  # noqa: E402

BASEP = dict(s.BASE, r=0.02, kappa=0.01, K=0.05)
N = 1201
LAMS = [1.0, 1.1, 1.2, 1.35, 1.5, 2.0, 3.0]
A1S = [0.045, 0.06, 0.08, 0.10, 0.12]
KS = [0.01, 0.02, 0.035, 0.05, 0.075, 0.1]
PATHS, DT, T, X0, SEED = 20000, 0.001, 5.0, 1.2, 7
SAFETY = ("p_no_deficit", "time_deficit", "shortfall", "issues", "raised", "p_no_issue", "p_above_1_02",
          "min_x_q05", "pi_mean", "pi_over_cap")


def sha(path):
    return hashlib.sha256(open(path, "rb").read()).hexdigest()


def point(args):
    lam, K, a1 = args
    p = dict(BASEP, lam=lam, K=K, a1=a1)
    pol = q.policy(p, N)
    v12 = float(np.interp(X0, pol["x"], pol["v"]))
    if pol["issues"]:
        W, Nn = q.feynman_kac_at(p, pol, X0)
        how = "Feynman-Kac"
    else:                                   # no issue is ever made: W by simulation
        o = q.simulate(p, pol, x0=X0, T=60, dt=DT, paths=4000, seed=SEED, measure="Pstar", discount=True)
        W, Nn, how = o["W"], 0.0, "simulation"
    saf = q.simulate(p, pol, x0=X0, T=T, dt=DT, paths=PATHS, seed=SEED, measure="P")
    return args, dict(y_star=pol["y_star"], x_L=pol["x_L"], y_post=pol["y_post"], interior=pol["interior"],
                      issuer=pol["issues"], v12=v12, W=W, N=Nn, W_how=how, C=v12 + (lam - 1) * W,
                      resid=pol["resid"], **{k: saf[k] for k in SAFETY}, se_p=saf["se_p"])


def validation(_):
    p = dict(BASEP, lam=1.2)
    pol = q.policy(p, N)
    W, Nn = q.feynman_kac_at(p, pol, X0)
    out = dict(v12=float(np.interp(X0, pol["x"], pol["v"])), W=W, N=Nn)
    for dt in (0.001, 0.0005):
        o = q.simulate(p, pol, x0=X0, T=60, dt=dt, paths=4000, seed=1, measure="Pstar", discount=True)
        out[dt] = {k: o[k] for k in ("V", "V_se", "W", "W_se", "N", "N_se")}
    return out


def main():
    jobs = sorted({(l, k, 0.045) for l in LAMS for k in (0.02, 0.05)}
                  | {(l, 0.05, a) for l in (1.0, 1.2) for a in A1S}
                  | {(1.2, k, 0.045) for k in KS})
    with Pool(6) as pool:
        val = pool.apply_async(validation, (None,))
        res = dict(pool.map(point, jobs))
        val = val.get()
    print("solves and paths done", flush=True)

    pins = ", ".join(f"`{f}` sha256 {sha(os.path.join(HERE, f))}"
                     for f in ("solve_qvi.py", "q7_paths.py", "run_q7_safety.py"))
    head = ("| lambda_S | K | a1 | y* | x_L | y_post | v(1.2) | C(1.2) | P(no deficit) | time in deficit | shortfall | "
            "issues | raised | P(no issue) | P(above 1.02) | 5% min | mean pi | pi / cap |")
    rule = "|" + "---|" * 18

    def row(a):
        o = res[a]
        f = lambda v, d=4: "none" if v is None else f"{v:.{d}f}"
        return (f"| {a[0]} | {a[1]} | {a[2]} | {f(o['y_star'])} | {f(o['x_L'])}{'' if o['interior'] else '*'} | "
                f"{f(o['y_post'])} | {o['v12']:.4f} | {o['C']:.4f} | {o['p_no_deficit']:.3f} | {o['time_deficit']:.3f} | "
                f"{o['shortfall']:.4f} | {o['issues']:.2f} | {o['raised']:.3f} | {o['p_no_issue']:.3f} | "
                f"{o['p_above_1_02']:.3f} | {o['min_x_q05']:.3f} | {o['pi_mean']:.3f} | {o['pi_over_cap']:.3f} |")

    se = res[jobs[0]]["se_p"]
    L = ["# Results: Question 7, safety against value (generated)", "",
         f"Generated by `run_q7_safety.py`. Pinned: {pins}.", "",
         f"Parameters of Bayraktar et al. (2026) with r = 0.02, kappa = 0.01, R = 1.15, the cap of A2, [1, 2.5], {N} "
         f"nodes. Safety over {T:g} years from x0 = {X0}, {PATHS} paths, Euler-Maruyama step {DT}, the issue trigger and "
         f"the dividend barrier corrected for discrete monitoring (Broadie, Glasserman and Kou), under the physical "
         f"measure. Standard error of a probability at most {se:.4f}. Values under the measure of the value equation. "
         "C(1.2) is v(1.2) with the shortfall penalty added back. An asterisk on x_L marks a trigger at the distress "
         "boundary; 'none' marks a bank that never issues.", "",
         "## 1. Validation at (lambda_S, K) = (1.2, 0.05)", "",
         f"Value equation on {N} nodes: v(1.2) = {val['v12']:.4f}, W(1.2) = {val['W']:.5f}, N(1.2) = {val['N']:.4f}. "
         "Simulated over 60 years under its measure, 4000 paths:", "",
         "| step | v(1.2) | W(1.2) | N(1.2) |", "|---|---|---|---|"]
    for dt in (0.001, 0.0005):
        o = val[dt]
        L.append(f"| {dt} | {o['V']:.4f} ({o['V_se']:.4f}) | {o['W']:.5f} ({o['W_se']:.5f}) | {o['N']:.4f} ({o['N_se']:.4f}) |")
    L += ["", "Standard errors in brackets. The value equation's own grid error is of the same size: W(1.2) is 0.0511 "
          "on 601 nodes and 0.0488 on 1201.", "",
          "## 2. The asymmetry parameter", "", head, rule]
    L += [row((l, k, 0.045)) for k in (0.02, 0.05) for l in LAMS]
    L += ["", "## 3. The solvency floor", "", head, rule]
    L += [row((l, 0.05, a)) for l in (1.0, 1.2) for a in A1S]
    L += ["", "## 4. The fixed issuance cost", "", head, rule]
    L += [row((1.2, k, 0.045)) for k in KS]
    # 5. two levers at equal expected time in deficit
    lam_path = [(res[(l, 0.05, 0.045)]["time_deficit"], res[(l, 0.05, 0.045)]["C"], l) for l in LAMS]
    td, Cl, ll = map(np.array, zip(*lam_path))
    L += ["", "## 5. The two levers at equal safety", "",
          "Along the solvency-floor path at lambda_S = 1, K = 0.05, each point's expected time in deficit is matched on "
          "the lambda_S path at a1 = 0.045, K = 0.05 by linear interpolation in the time in deficit, which falls in "
          "lambda_S on that path if the column 'monotone' says so. The difference is the cash value of reaching that "
          "safety by the asymmetry parameter rather than by the floor.", "",
          f"monotone: {bool(np.all(np.diff(td) < 0))}", "",
          "| a1 | time in deficit | C by the floor | lambda_S giving it | C by lambda_S | difference |", "|---|---|---|---|---|---|"]
    order = np.argsort(td)
    for a in A1S:
        o = res[(1.0, 0.05, a)]
        t0 = o["time_deficit"]
        if td.min() <= t0 <= td.max() and np.all(np.diff(td) < 0):
            lam_eq = float(np.interp(t0, td[order], ll[order]))
            C_eq = float(np.interp(t0, td[order], Cl[order]))
            L.append(f"| {a} | {t0:.3f} | {o['C']:.4f} | {lam_eq:.3f} | {C_eq:.4f} | {C_eq - o['C']:+.4f} |")
        else:
            L.append(f"| {a} | {t0:.3f} | {o['C']:.4f} | outside the lambda_S path | | |")
    L.append("")
    open(os.path.join(HERE, "results_q7.md"), "w").write("\n".join(L))
    json.dump(dict(validation={str(k): v for k, v in val.items()}, points=[[list(k), v] for k, v in res.items()]),
              open(os.path.join(HERE, "results_q7.json"), "w"), indent=1, default=float)
    print("written results_q7.md")


if __name__ == "__main__":
    main()
