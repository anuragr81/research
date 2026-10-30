"""The model of Bayraktar, Chevalier, Ly Vath and Wang (2026) solved by exact policy iteration.

State y on [y_low, y_max], y_low = 1.02 the supervisory distress threshold. Generator,
drift, diffusion and cap as in solve_qvi.py (the same as theirs), exposure in closed form.
Their issuance operator, in post-issuance coordinates,
    H v(y) = sup_{z > y} v(z) - (z - y)/(1 - kappa') - kappa kappa' (y - 1)/(1 - kappa'),
and liquidation for y - 1. The discrete problem at each node is the max over
continuation, issuance, dividend (V' >= 1) and liquidation, solved exactly at each
policy step, so there is no time-step error.

issue = "anywhere": issuance allowed at every node, the problem their MATLAB solver poses
            (it takes max(v, Hv) at every node).
issue = "boundary": issuance only at y_low, the model as stated in their Section 2.
issue = "none":     no issuance, liquidation at y_low, their v^HJB benchmark.
"""
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import numpy as np  # noqa: E402
import solve_qvi as s  # noqa: E402

BASE = dict(s.BASE, r=0.02, lam=1.0, K=0.0, kappa=0.01, kappa_p=0.02)


def solve(p, issue="anywhere", y_low=1.02, y_max=2.5, n=601, max_iter=500):
    x = np.linspace(y_low, y_max, n)
    dx = x[1] - x[0]
    N = n
    k, kp = p["kappa"], p["kappa_p"]
    v = np.maximum(x - 1, 0) + 0.3
    v[0] = y_low - 1
    calm = 0
    act = np.zeros(N, int)
    tg = np.full(N, N - 1)
    for it in range(max_iter):
        v1, v2 = s.derivs(v, dx)
        pi = s.best_pi(x, v1, v2, p)
        lo, di, up = s.generator(x, pi, p, dx)
        G = np.full(N, -np.inf)
        G[1:-1] = lo[1:-1] * v[:-2] + di[1:-1] * v[1:-1] + up[1:-1] * v[2:]
        base = v - x / (1 - kp)
        suf = np.maximum.accumulate(base[::-1])[::-1]
        arg = np.empty(N, int)
        best = N - 1
        for j in range(N - 1, -1, -1):
            if base[j] >= base[best]:
                best = j
            arg[j] = best
        Hv = np.full(N, -np.inf)
        tg = np.full(N, N - 1)
        Hv[:-1] = suf[1:] + x[:-1] / (1 - kp) - k * kp * (x[:-1] - 1) / (1 - kp)
        tg[:-1] = arg[1:]
        if issue == "boundary":
            Hv[1:] = -np.inf
        if issue == "none" or it == 0:
            Hv[:] = -np.inf
        D = np.full(N, -np.inf)
        D[1:] = v[:-1] + dx - v[1:]
        Lq = (x - 1) - v
        act = np.argmax(np.stack([G * dx ** 2, Hv - v, D, Lq]), axis=0)
        act[0] = 1 if Hv[0] - v[0] > Lq[0] else 3
        act[-1] = 2
        A = np.zeros((N, N))
        r = np.zeros(N)
        for i in range(N):
            a = act[i]
            if a == 0:
                A[i, i - 1], A[i, i], A[i, i + 1] = lo[i], di[i], up[i]
            elif a == 1:
                A[i, i] = 1
                A[i, tg[i]] -= 1
                r[i] = -(x[tg[i]] - x[i]) / (1 - kp) - k * kp * (x[i] - 1) / (1 - kp)
            elif a == 2:
                A[i, i], A[i, i - 1], r[i] = 1, -1, dx
            else:
                A[i, i] = 1
                r[i] = x[i] - 1
        vn = np.linalg.solve(A, r)
        ch = np.max(np.abs(vn - v))
        v = vn
        calm = calm + 1 if ch < 1e-12 else 0
        if calm >= 3:
            break
    cont = np.where(act == 0)[0]
    iss = np.where(act == 1)[0]
    return dict(x=x, v=v, act=act, it=it,
                y_star=x[cont.max() + 1] if len(cont) else np.nan,
                issue_top=x[iss.max()] if len(iss) else np.nan,
                target=x[tg[iss.max()]] if len(iss) else np.nan,
                v12=float(np.interp(1.2, x, v)))


def table1_point(p, n=601):
    q = solve(p, "anywhere", n=n)
    b = solve(p, "boundary", n=n)
    h = solve(p, "none", n=n)
    return dict(anywhere=q, boundary=b, none=h,
                G_anywhere=(q["v12"] - h["v12"]) / h["v12"], G_boundary=(b["v12"] - h["v12"]) / h["v12"])
