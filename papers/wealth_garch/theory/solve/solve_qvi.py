"""Numerical solve of the theory paper's model at one parameter point.

The model is that of wealth_garch_theory.tex, A1 to A6: the state x on
[1, x_max], drift b(x, pi) = x((1 - pi) r + pi mu_s - mu_L) + gamma and
diffusion sigma^2(x, pi) of A1, exposure 0 <= pi <= u(x)/x under the
regulatory cap of A2, running shortfall penalty Lambda(x) =
(lambda_S - 1)(R - x)^+ (A4), and recapitalisation at fixed issuance cost K
plus (1 + kappa) per unit raised (A5). The value V solves the quasi-variational
inequality

    max{ max_pi [ sigma^2 V''/2 + b V' ] - rho_L V - Lambda,  M V - V,  1 - V' } = 0,

with M V(x) = sup_{xi > 0} V(x + xi) - K - (1 + kappa) xi. The scheme is that
of the parent bundle's 02_numerical/solve_bank_qvi_lambda_M.m (implicit time
stepping with upwind drift, the impulse obstacle, a slope floor of one), ported
to numpy. The optimal exposure is taken in closed form at each node, the
interior maximiser of the concave quadratic in pi where V'' < 0 and the better
of the two ends of [0, u(x)/x] otherwise.

Under the optimal policy the expected discounted shortfall W and the discounted
injection count N solve sigma^2 F''/2 + b F' - rho_L F + f = 0 on (x_L, y*),
f = (R - x)^+ for W and 0 for N, with F'(y*) = 0 at the reflecting barrier and
W(x_L) = W(y_post), N(x_L) = 1 + N(y_post) at the trigger.

The solve is an illustration. No row of the manuscript rests on it; it shows
that the hypotheses of M1 to M4 hold at one parameter point.
"""
import json
import sys

import numpy as np

BASE = dict(r=0.025, mu=0.04, mu_L=0.03, rho=0.12, sigma=0.08, sigma_L=0.03, c=0.20, gamma=0.02,
            a1=0.045, a2=0.05, a3=0.30, kappa=0.01, R=1.15, K=0.02, lam=1.5)
GRID = dict(x_max=2.5, n=1201, dt=0.5, tol=1e-11, max_iter=200000)


def pi_bar(x, p):
    t1 = (1 / p["a1"]) * (1 - 1 / x)
    t3 = (1 / p["a3"]) * (1 - p["a2"] / x)
    return np.maximum(0.0, np.minimum(t1, t3))


def drift(x, pi, p):
    return x * ((1 - pi) * p["r"] + pi * p["mu"] - p["mu_L"]) + p["gamma"]


def sig2(x, pi, p):
    return np.maximum(0.0, pi ** 2 * p["sigma"] ** 2 * x ** 2
                      + 2 * pi * p["c"] * p["sigma"] * p["sigma_L"] * x * (1 - x)
                      + p["sigma_L"] ** 2 * (1 - x) ** 2)


def best_pi(x, v1, v2, p):
    """Maximise sigma^2(x,pi) v2/2 + b(x,pi) v1 over pi in [0, pi_bar(x)]."""
    pb = pi_bar(x, p)
    def h(pi):
        return 0.5 * sig2(x, pi, p) * v2 + drift(x, pi, p) * v1
    cands = [np.zeros_like(x), pb]
    with np.errstate(divide="ignore", invalid="ignore"):
        # d/dpi = v2 (pi sigma^2 x^2 + c sigma sigma_L x (1 - x)) + x (mu - r) v1 = 0
        pint = -(x * (p["mu"] - p["r"]) * v1 + v2 * p["c"] * p["sigma"] * p["sigma_L"] * x * (1 - x)) \
            / (v2 * p["sigma"] ** 2 * x ** 2)
    pint = np.where((v2 < 0) & np.isfinite(pint), np.clip(pint, 0.0, pb), 0.0)
    cands.append(pint)
    vals = np.stack([h(cc) for cc in cands])
    return np.stack(cands)[np.argmax(vals, axis=0), np.arange(len(x))]


def thomas(a, b, c, d):
    n = len(b)
    cp, dp = np.empty(n), np.empty(n)
    cp[0], dp[0] = c[0] / b[0], d[0] / b[0]
    for i in range(1, n):
        m = b[i] - a[i] * cp[i - 1]
        cp[i] = c[i] / m if i < n - 1 else 0.0
        dp[i] = (d[i] - a[i] * dp[i - 1]) / m
    out = np.empty(n)
    out[-1] = dp[-1]
    for i in range(n - 2, -1, -1):
        out[i] = dp[i] - cp[i] * out[i + 1]
    return out


def impulse(x, v, p):
    """M v(x_i) = max_{j > i} v_j - K - (1 + kappa)(x_j - x_i), and the argmax."""
    base = v - (1 + p["kappa"]) * x
    suf = np.maximum.accumulate(base[::-1])[::-1]
    arg = np.empty(len(x), dtype=int)
    best = len(x) - 1
    for j in range(len(x) - 1, -1, -1):
        if base[j] >= base[best]:
            best = j
        arg[j] = best
    mv = np.full(len(x), -1e100)
    mv[:-1] = suf[1:] + (1 + p["kappa"]) * x[:-1] - p["K"]
    targ = np.full(len(x), len(x) - 1)
    targ[:-1] = arg[1:]
    return mv, targ


def derivs(v, dx):
    v1, v2 = np.empty_like(v), np.empty_like(v)
    v1[1:-1] = (v[2:] - v[:-2]) / (2 * dx)
    v2[1:-1] = (v[2:] - 2 * v[1:-1] + v[:-2]) / dx ** 2
    v1[0], v1[-1] = (v[1] - v[0]) / dx, (v[-1] - v[-2]) / dx
    v2[0], v2[-1] = v2[1], v2[-2]
    return v1, v2


def generator(x, pi, p, dx):
    """Upwind tridiagonal coefficients of sigma^2/2 d2 + b d1 - rho_L."""
    rhoL = p["rho"] - p["mu_L"]
    b, s2 = drift(x, pi, p), sig2(x, pi, p)
    A = 0.5 * s2 / dx ** 2
    lo = A + np.maximum(-b, 0) / dx
    up = A + np.maximum(b, 0) / dx
    di = -(lo + up) - rhoL
    return lo, di, up


def solve(p, g=GRID, v0=None):
    x = np.linspace(1.0, g["x_max"], g["n"])
    dx = x[1] - x[0]
    lam_pen = (p["lam"] - 1) * np.maximum(p["R"] - x, 0)
    v = np.maximum(x - 1, 0) + 1.0 if v0 is None else v0.copy()
    for it in range(g["max_iter"]):
        v1, v2 = derivs(v, dx)
        pi = best_pi(x, v1, v2, p)
        lo, di, up = generator(x, pi, p, dx)
        n = len(x) - 2
        a = -g["dt"] * lo[1:-1]; bb = 1 - g["dt"] * di[1:-1]; c = -g["dt"] * up[1:-1]
        rhs = v[1:-1] - g["dt"] * lam_pen[1:-1]
        rhs[0] -= a[0] * v[0]; rhs[-1] -= c[-1] * v[-1]
        a = a.copy(); a[0] = 0.0; c = c.copy(); c[-1] = 0.0
        vn = v.copy()
        vn[1:-1] = thomas(a, bb, c, rhs)
        vn[-1] = vn[-2] + dx
        mv, _ = impulse(x, vn, p)
        vn = np.maximum(vn, mv)
        vn[0] = max(0.0, mv[0])
        for i in range(1, len(x)):          # slope floor V' >= 1, dividends
            if vn[i] < vn[i - 1] + dx:
                vn[i] = vn[i - 1] + dx
        err = np.max(np.abs(vn - v))
        v = vn
        if err < g["tol"]:
            break
    return x, v, it, err


def policy(x, v, p):
    dx = x[1] - x[0]
    v1, v2 = derivs(v, dx)
    pi = best_pi(x, v1, v2, p)
    mv, targ = impulse(x, v, p)
    active = np.abs(v - mv) < 1e-9
    iL = int(np.max(np.where(active)[0])) if active.any() else 0
    ipost = int(targ[iL])
    rhoL = p["rho"] - p["mu_L"]
    lam_pen = (p["lam"] - 1) * np.maximum(p["R"] - x, 0)
    # the barrier: first node above the target where V' falls to one
    above = np.where((x > x[ipost]) & (v1 <= 1 + 1e-7))[0]
    istar = int(above[0]) if len(above) else len(x) - 1
    return dict(dx=dx, v1=v1, v2=v2, pi=pi, iL=iL, ipost=ipost, istar=istar, lam_pen=lam_pen, rhoL=rhoL)


def feynman_kac(x, pi, p, iL, ipost, istar, src, jump):
    """F on nodes iL..istar: generator F + src = 0, F'(y*) = 0, F(x_L) = F(y_post) + jump."""
    dx = x[1] - x[0]
    lo, di, up = generator(x, pi, p, dx)
    idx = np.arange(iL, istar + 1)
    m = len(idx)
    Amat = np.zeros((m, m)); rhs = np.zeros(m)
    Amat[0, 0] = 1.0; Amat[0, ipost - iL] -= 1.0; rhs[0] = jump
    for k in range(1, m - 1):
        i = idx[k]
        Amat[k, k - 1], Amat[k, k], Amat[k, k + 1] = lo[i], di[i], up[i]
        rhs[k] = -src[i]
    Amat[m - 1, m - 1], Amat[m - 1, m - 2] = 1.0, -1.0        # F'(y*) = 0
    F = np.linalg.solve(Amat, rhs)
    return idx, F


def report(p):
    x, v, it, err = solve(p)
    q = policy(x, v, p)
    iL, ip, ist, dx = q["iL"], q["ipost"], q["istar"], q["dx"]
    rhoL = q["rhoL"]
    b = drift(x, q["pi"], p); s2 = sig2(x, q["pi"], p)
    W_idx, W = feynman_kac(x, q["pi"], p, iL, ip, ist, np.maximum(p["R"] - x, 0), 0.0)
    _, N = feynman_kac(x, q["pi"], p, iL, ip, ist, np.zeros_like(x), 1.0)
    Wd = np.gradient(W, dx); Nd = np.gradient(N, dx)
    # V'' just above the trigger and at the target from the interior equation
    def vpp(i):
        return 2 * (rhoL * v[i] + q["lam_pen"][i] - b[i] * q["v1"][i]) / s2[i]
    psi_L = rhoL * v[iL] + q["lam_pen"][iL] - (1 + p["kappa"]) * b[iL]
    mu_prime = (b[ist + 1] - b[ist - 1]) / (2 * dx)
    out = dict(params=p, grid=GRID, iterations=it, final_change=err,
               y_star=x[ist], x_L=x[iL], y_post=x[ip],
               V_prime_at_y_star=q["v1"][ist], V_second_at_y_star=q["v2"][ist],
               W_y_star=W[-1], N_y_star=N[-1], W_prime_x_L=Wd[0], N_prime_x_L=Nd[0],
               N_prime_y_post=Nd[ip - iL], N_prime_max_inaction=float(np.max(Nd[1:-1])),
               rhoL_W_y_post_minus_R_minus_x_L=rhoL * W[ip - iL] - (p["R"] - x[iL]),
               V_second_x_L_plus=vpp(iL + 1), V_second_y_post=vpp(ip),
               Psi_x_L=psi_L, mu_prime_y_star=mu_prime, rhoL=rhoL,
               sigma2_y_star=s2[ist], sigma2_x_L=s2[iL],
               pi_over_cap_at_x_L=q["pi"][iL] / pi_bar(x[iL], p), pi_over_cap_at_y_star=q["pi"][ist] / pi_bar(x[ist], p),
               share_of_inaction_at_cap=float(np.mean(np.isclose(q["pi"][iL:ist + 1], pi_bar(x[iL:ist + 1], p), rtol=1e-9))))
    return out, (x, v, q, W, N)


if __name__ == "__main__":
    p = dict(BASE)
    for a in sys.argv[1:]:
        k, val = a.split("=")
        p[k] = float(val)
    out, _ = report(p)
    print(json.dumps({k: (float(v) if isinstance(v, (np.floating, float, int, np.integer)) else v) for k, v in out.items()}, indent=1))


def thresholds(x, v, p):
    """Thresholds between grid nodes. y* where V' falls to one, y_post where V'
    falls through 1 + kappa above the trigger, x_L from the quadratic contact of
    V - MV with zero (smooth fit), fitted on the four nodes above the trigger."""
    q = policy(x, v, p)
    v1, iL, ip, ist = q["v1"], q["iL"], q["ipost"], q["istar"]
    def cross(i0, level):
        for i in range(i0, len(x) - 1):
            if v1[i] > level >= v1[i + 1]:
                return x[i] + (v1[i] - level) / (v1[i] - v1[i + 1]) * (x[i + 1] - x[i])
        return float("nan")
    y_star = cross(max(ip, iL + 1), 1.0 + 1e-12)
    y_post = cross(iL + 1, 1.0 + p["kappa"])
    mv, _ = impulse(x, v, p)
    d = np.sqrt(np.maximum(v - mv, 0.0))
    js = np.arange(iL + 1, iL + 5)
    slope, icpt = np.polyfit(x[js], d[js], 1)
    x_L = -icpt / slope if slope > 0 else x[iL]
    return dict(y_star=y_star, y_post=y_post, x_L=max(x_L, x[iL]), q=q)


def check_point(p, n, h_lam, h_K):
    """Solve at p and at p +- h, return thresholds, W and N, the envelope errors and
    the finite-difference responses against the formulas of M1 and M2."""
    g = dict(GRID, n=n)
    sol = {}
    for key, dl, dk in [("0", 0, 0), ("l+", h_lam, 0), ("l-", -h_lam, 0), ("k+", 0, h_K), ("k-", 0, -h_K)]:
        pp = dict(p, lam=p["lam"] + dl, K=p["K"] + dk)
        x, v, it, err = solve_howard(pp, g)
        sol[key] = (x, v, thresholds(x, v, pp), it, err)
    x, v, th, it, err = sol["0"]
    q = th["q"]
    iL, ip, ist, dx = q["iL"], q["ipost"], q["istar"], q["dx"]
    rhoL = q["rhoL"]
    b = drift(x, q["pi"], p); s2 = sig2(x, q["pi"], p)
    idx, W = feynman_kac(x, q["pi"], p, iL, ip, ist, np.maximum(p["R"] - x, 0), 0.0)
    _, N = feynman_kac(x, q["pi"], p, iL, ip, ist, np.zeros_like(x), 1.0)
    Wd, Nd = np.gradient(W, dx), np.gradient(N, dx)
    dVl = (sol["l+"][1] - sol["l-"][1]) / (2 * h_lam)
    dVk = (sol["k+"][1] - sol["k-"][1]) / (2 * h_K)
    inner = idx[2:-2]
    env_W = float(np.max(np.abs(dVl[inner] + W[2:-2])) / np.max(np.abs(W)))
    env_N = float(np.max(np.abs(dVk[inner] + N[2:-2])) / np.max(np.abs(N)))
    psi = rhoL * v[iL] + q["lam_pen"][iL] - (1 + p["kappa"]) * b[iL]
    vpp_L = 2 * psi / s2[iL]                     # M4: sigma^2 V''(x_L+)/2 = Psi(x_L)
    i_post = ip - iL
    vpp_post = 2 * (rhoL * v[ip] + q["lam_pen"][ip] - b[ip] * (1 + p["kappa"])) / s2[ip]
    mu1 = (b[ist + 1] - b[ist - 1]) / (2 * dx)
    fd = lambda k, a: (sol[k + "+"][2][a] - sol[k + "-"][2][a]) / (2 * (h_lam if k == "l" else h_K))
    formula = dict(dystar_dlam=rhoL * W[-1] / (rhoL - mu1), dystar_dK=rhoL * N[-1] / (rhoL - mu1),
                   dxL_dlam=Wd[0] / vpp_L, dxL_dK=Nd[0] / vpp_L)
    numeric = dict(dystar_dlam=fd("l", "y_star"), dystar_dK=fd("k", "y_star"),
                   dxL_dlam=fd("l", "x_L"), dxL_dK=fd("k", "x_L"))
    det_formula = formula["dystar_dlam"] * formula["dxL_dK"] - formula["dystar_dK"] * formula["dxL_dlam"]
    det_numeric = numeric["dystar_dlam"] * numeric["dxL_dK"] - numeric["dystar_dK"] * numeric["dxL_dlam"]
    hyp = {"V'(y*) = 1": abs(q["v1"][ist] - 1) < 1e-3, "y* > R (Lambda'(y*) = 0)": th["y_star"] > p["R"],
           "sigma^2(y*) > 0": s2[ist] > 0, "mu'(y*) < rho_L": mu1 < rhoL,
           "W(y*) > 0": W[-1] > 0, "N(y*) > 0": N[-1] > 0, "W'(x_L) > 0": Wd[0] > 0,
           "N' < 0 on the inaction region": bool(np.max(Nd[1:-1]) < 0), "N'(y_post) < 0": Nd[i_post] < 0,
           "Psi(x_L) > 0": psi > 0, "V''(x_L+) > 0": vpp_L > 0, "V''(y_post) < 0": vpp_post < 0,
           "sigma^2(x_L) > 0": s2[iL] > 0, "x_L interior, 1 < x_L < R": 1 + 2 * dx < th["x_L"] < p["R"] - 2 * dx}
    return dict(n=n, iterations=it, final_change=err, y_star=th["y_star"], x_L=th["x_L"], y_post=th["y_post"],
                W_y_star=W[-1], N_y_star=N[-1], W_prime_x_L=Wd[0], N_prime_x_L=Nd[0], Psi_x_L=psi,
                V_second_x_L_plus=vpp_L, V_second_y_post=vpp_post, mu_prime_y_star=mu1, rhoL=rhoL,
                envelope_rel_err_W=env_W, envelope_rel_err_N=env_N, formula=formula, numeric=numeric,
                det_formula=det_formula, det_numeric=det_numeric,
                cap_share_inaction=float(np.mean(np.isclose(q["pi"][iL:ist + 1], pi_bar(x[iL:ist + 1], p), rtol=1e-9))),
                hypotheses=hyp)


def qvi_terms(x, v, p):
    """The three terms of the discrete QVI at every node, with the exposure and the targets."""
    dx = x[1] - x[0]
    n = len(x)
    lam_pen = (p["lam"] - 1) * np.maximum(p["R"] - x, 0)
    v1, v2 = derivs(v, dx)
    pi = best_pi(x, v1, v2, p)
    lo, di, up = generator(x, pi, p, dx)
    G = np.full(n, -np.inf)
    G[1:-1] = lo[1:-1] * v[:-2] + di[1:-1] * v[1:-1] + up[1:-1] * v[2:] - lam_pen[1:-1]
    G[0] = (di[0] + lo[0]) * v[0] + up[0] * v[1] - lam_pen[0]
    mv, targ = impulse(x, v, p)
    I = mv - v
    D = np.full(n, -np.inf); D[1:] = v[:-1] + dx - v[1:]
    return G, I, D, pi, lo, di, up, targ, lam_pen


def solve_howard(p, g=GRID, v0=None, max_iter=300):
    """Policy iteration on the discrete quasi-variational inequality
        max{ G_i(V), MV_i - V_i, V_{i-1} + dx - V_i } = 0,
    G the upwind generator minus the penalty. Each step fixes at every node the
    action that attains the max (continue with the best exposure, issue to the
    best target, or pay a dividend) and solves the linear system exactly, so the
    fixed point is the discrete QVI with no projection error. Warm-started from
    the time-stepping scheme. Stops when V changes by less than 1e-12 on three
    successive steps; ties between actions can make the labels flip without
    moving V."""
    x = np.linspace(1.0, g["x_max"], g["n"])
    dx = x[1] - x[0]
    n = len(x)
    if v0 is None:
        _, v0, _, _ = solve(p, dict(g, tol=1e-9))
    v = v0.copy()
    calm = 0
    rows = np.arange(n)
    for it in range(max_iter):
        G, I, D, pi, lo, di, up, targ, lam_pen = qvi_terms(x, v, p)
        act = np.argmax(np.stack([G * dx ** 2, I, D]), axis=0)
        act[-1] = 2
        A = np.zeros((n, n)); r = np.zeros(n)
        c = act == 0
        ci = rows[c]
        A[ci, ci] = np.where(ci > 0, di[ci], di[0] + lo[0])
        cm = ci[ci > 0]; A[cm, cm - 1] = lo[cm]
        A[ci, ci + 1] = up[ci]
        r[ci] = lam_pen[ci]
        ii = rows[act == 1]
        A[ii, ii] = 1.0; A[ii, targ[ii]] -= 1.0
        r[ii] = -p["K"] - (1 + p["kappa"]) * (x[targ[ii]] - x[ii])
        di_ = rows[act == 2]
        A[di_, di_] = 1.0; A[di_, di_ - 1] = -1.0; r[di_] = dx
        v_new = np.linalg.solve(A, r)
        change = np.max(np.abs(v_new - v))
        v = v_new
        calm = calm + 1 if change < 1e-12 else 0
        if calm >= 3:
            break
    G, I, D, *_ = qvi_terms(x, v, p)
    resid = np.max(np.abs(np.maximum.reduce([G[:-1] * dx ** 2, I[:-1], D[:-1]])))
    return x, v, it, resid
