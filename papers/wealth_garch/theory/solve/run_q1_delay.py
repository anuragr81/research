"""Writes results_q1_delay.md: Question 1, pass 3, whether a delay on issues, the
friction of Peura and Keppo (2006), is a rival to K1. The model of delay_model.py at
the parameters of run_q1_pk.py (Bayraktar et al. 2026 with r = 0.02, kappa = 0.01, the
cap of A2, leveraged), 1201 nodes.

1. The three thresholds (y*, x_L, y_post) and v(1.2) over the delay Delta = 0, 0.05,
   0.1, 0.15, 0.2, 0.25 years at the four centres of run_q1_pk.py.
2. The Jacobian of (y*, x_L, y_post) in (lambda_S, K, Delta) at Delta = 0 (lambda_S and
   K by central differences, h = 0.1 and 0.01; Delta by the slope at zero of a quadratic
   through the sweep) and at Delta = 0.1 (all three by central differences, h = 0.05 in
   Delta). Its determinant, and the omitted-delay test in linear form: the change in
   (lambda_S, K) that mimics the delay in (y*, x_L), and the part of the delay's effect
   on y_post that this change leaves.
3. The same test without linearising. For Delta = 0.05, 0.1, 0.25 the thresholds of the
   model with the delay are taken as data, the model without it is fitted to (y*, x_L) by
   Newton steps with the Jacobian of part 2, and its y_post is set against the data's.

The accuracy of the thresholds is about 3e-4 (1201 against 2401 nodes, in the header).
No row of the manuscript rests on these numbers.

    python3 run_q1_delay.py
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
import delay_model as d  # noqa: E402

BASEP = dict(s.BASE, r=0.02, kappa=0.01, delay=0.0)
CENTRES = [(1.2, 0.05), (1.5, 0.05), (1.2, 0.02), (1.2, 0.1)]
DELAYS = [0.0, 0.05, 0.1, 0.15, 0.2, 0.25]
H_LAM, H_K, H_D = 0.1, 0.01, 0.05
D_LOCAL = 0.1
D_FIT = [0.05, 0.1, 0.25]
N = 1201
KEYS = ("y_star", "x_L", "y_post")
# 1201 against 2401 nodes at (1.2, 0.05), from the convergence run of 30 October 2026
GRID_CHECK = {0.0: ((1.836, 1.03843, 1.61027), (1.83593, 1.03857, 1.61018)),
              0.1: ((1.85183, 1.04018, 1.62404), (1.85166, 1.04028, 1.62394)),
              0.25: ((1.87224, 1.04374, 1.64194), (1.87213, 1.04378, 1.64182))}


def sha(path):
    return hashlib.sha256(open(path, "rb").read()).hexdigest()


def point(args):
    lam, K, D = args
    p = dict(BASEP, lam=lam, K=K, delay=D)
    x, v, U, it, resid = d.solve(p, N)
    th = d.thresholds(x, v, U, p)
    return args, dict({k: float(th[k]) for k in KEYS}, interior=bool(th["interior"]),
                      v12=float(np.interp(1.2, x, v)), resid=float(resid), it=it)


def fit_job(args):
    """Fit the model without delay to (y*, x_L) of the model with delay."""
    lam0, K0, D, target, J = args
    th = np.array([lam0, K0])
    Jinv = np.linalg.inv(np.array(J))
    hist = []
    for _ in range(8):
        _, o = point((th[0], th[1], 0.0))
        r = np.array([o["y_star"] - target[0], o["x_L"] - target[1]])
        hist.append((float(th[0]), float(th[1]), float(np.max(np.abs(r)))))
        if np.max(np.abs(r)) < 2e-5:
            break
        th = th - Jinv @ r
    return lam0, K0, D, dict(lam_hat=float(th[0]), K_hat=float(th[1]), fit=o, steps=hist)


def main():
    jobs = set()
    for lam, K in CENTRES:
        jobs |= {(lam, K, D) for D in DELAYS}
        jobs |= {(lam + H_LAM, K, 0.0), (lam - H_LAM, K, 0.0), (lam, K + H_K, 0.0), (lam, K - H_K, 0.0)}
        jobs |= {(lam + H_LAM, K, D_LOCAL), (lam - H_LAM, K, D_LOCAL), (lam, K + H_K, D_LOCAL),
                 (lam, K - H_K, D_LOCAL), (lam, K, D_LOCAL + H_D), (lam, K, D_LOCAL - H_D)}
    jobs = sorted(jobs, key=lambda a: -a[2])          # the slow solves first
    with Pool(6) as pool:
        res = dict(pool.map(point, jobs))
        print("solves done", flush=True)
        jac = {}
        fits_in = []
        for lam, K in CENTRES:
            g = lambda dl, dk, D: np.array([res[(lam + dl, K + dk, D)][k] for k in KEYS])
            sw = np.array([[res[(lam, K, D)][k] for k in KEYS] for D in DELAYS])
            slope0 = np.array([np.polyfit(DELAYS, sw[:, i], 2)[1] for i in range(3)])
            J0 = np.column_stack([(g(H_LAM, 0, 0.0) - g(-H_LAM, 0, 0.0)) / (2 * H_LAM),
                                  (g(0, H_K, 0.0) - g(0, -H_K, 0.0)) / (2 * H_K), slope0])
            J1 = np.column_stack([(g(H_LAM, 0, D_LOCAL) - g(-H_LAM, 0, D_LOCAL)) / (2 * H_LAM),
                                  (g(0, H_K, D_LOCAL) - g(0, -H_K, D_LOCAL)) / (2 * H_K),
                                  (g(0, 0, D_LOCAL + H_D) - g(0, 0, D_LOCAL - H_D)) / (2 * H_D)])
            out = {}
            for name, J in (("0", J0), ("0.1", J1)):
                A = J[:2, :2]
                mimic = np.linalg.solve(A, J[:2, 2])          # d(lambda, K) per unit of Delta
                left = J[2, 2] - J[2, :2] @ mimic             # y_post per unit of Delta, not mimicked
                out[name] = dict(J=J.tolist(), det3=float(np.linalg.det(J)), det2=float(np.linalg.det(A)),
                                 det_lam_delay=float(J[0, 0] * J[1, 2] - J[0, 2] * J[1, 0]),
                                 mimic_lam=float(mimic[0]), mimic_K=float(mimic[1]),
                                 ypost_left=float(left), ypost_delay=float(J[2, 2]))
            jac[(lam, K)] = out
            fits_in += [(lam, K, D, [res[(lam, K, D)]["y_star"], res[(lam, K, D)]["x_L"]], J0[:2, :2].tolist())
                        for D in D_FIT]
        fits = pool.map(fit_job, fits_in)
        print("fits done", flush=True)

    pins = ", ".join(f"`{f}` sha256 {sha(os.path.join(HERE, f))}"
                     for f in ("solve_qvi.py", "delay_model.py", "run_q1_delay.py"))
    L = ["# Results: Question 1, pass 3, a delay on issues against K1 (generated)", "",
         f"Generated by `run_q1_delay.py`. Pinned: {pins}.", "",
         "Parameters of Bayraktar et al. (2026) with r = 0.02, kappa = 0.01, R = 1.15, the cap of A2, [1, 2.5], "
         f"{N} nodes. The delay Delta is in years. While an issue is pending the bank pays no dividend and bears "
         "the shortfall penalty; the state cannot fall below 1, so the delay carries no risk of failure, unlike "
         "Peura and Keppo's.", "",
         "Accuracy. (y*, x_L, y_post) at (lambda_S, K) = (1.2, 0.05), 1201 against 2401 nodes: "
         + "; ".join(f"Delta {D}: {a} against {b}" for D, (a, b) in GRID_CHECK.items())
         + ". Read the thresholds to about 3e-4.", "",
         "## 1. The thresholds over the delay", "",
         "| (lambda_S, K) | Delta | y* | x_L | y_post | v(1.2) | trigger |", "|---|---|---|---|---|---|---|"]
    for lam, K in CENTRES:
        for D in DELAYS:
            o = res[(lam, K, D)]
            L.append(f"| ({lam}, {K}) | {D} | {o['y_star']:.5f} | {o['x_L']:.5f} | {o['y_post']:.5f} | "
                     f"{o['v12']:.5f} | {'interior' if o['interior'] else 'distress boundary'} |")
    L += ["", "## 2. The Jacobian of (y*, x_L, y_post) in (lambda_S, K, Delta)", "",
          "Columns lambda_S, K, Delta; rows y*, x_L, y_post. The mimicking change is the (d lambda_S, d K) per unit "
          "of Delta that moves (y*, x_L) as the delay does; the part left is the delay's effect on y_post minus "
          "that of the mimicking change, per unit of Delta. A part left near zero means y_post cannot tell the "
          "delay from more asymmetry and a higher fixed cost.", "",
          "| (lambda_S, K) | at Delta | J (rows y*, x_L, y_post) | det of (y*, x_L) in (lambda_S, K) | "
          "det of (y*, x_L) in (lambda_S, Delta) | det of the 3x3 | mimicking d lambda_S, d K | "
          "y_post per unit Delta | part left |", "|---|---|---|---|---|---|---|---|---|"]
    for lam, K in CENTRES:
        for name in ("0", "0.1"):
            o = jac[(lam, K)][name]
            Js = "; ".join("(" + ", ".join(f"{v:.4f}" for v in row) + ")" for row in o["J"])
            L.append(f"| ({lam}, {K}) | {name} | {Js} | {o['det2']:.4f} | {o['det_lam_delay']:.5f} | {o['det3']:.6f} | "
                     f"{o['mimic_lam']:.3f}, {o['mimic_K']:.4f} | {o['ypost_delay']:.4f} | {o['ypost_left']:.5f} |")
    L += ["", "## 3. The model without delay fitted to the thresholds of the model with it", "",
          "| (lambda_S, K) | Delta | fitted lambda_S | fitted K | fit error in (y*, x_L) | y_post, data | "
          "y_post, fitted model | gap | Newton steps |", "|---|---|---|---|---|---|---|---|---|"]
    for lam, K, D, o in fits:
        data = res[(lam, K, D)]["y_post"]
        L.append(f"| ({lam}, {K}) | {D} | {o['lam_hat']:.4f} | {o['K_hat']:.5f} | {o['steps'][-1][2]:.1e} | "
                 f"{data:.5f} | {o['fit']['y_post']:.5f} | {o['fit']['y_post'] - data:+.5f} | {len(o['steps'])} |")
    L += ["", "A gap below the accuracy of about 3e-4 is one the injection target cannot detect.", ""]
    open(os.path.join(HERE, "results_q1_delay.md"), "w").write("\n".join(L))
    json.dump(dict(points=[[list(k), v] for k, v in res.items()],
                   jacobians=[[list(k), v] for k, v in jac.items()], fits=fits),
              open(os.path.join(HERE, "results_q1_delay.json"), "w"), indent=1, default=float)
    print("written results_q1_delay.md")


if __name__ == "__main__":
    main()
