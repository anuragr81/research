"""The theory paper's model (solve_qvi.py) with a delay on issues, after Peura and
Keppo (2006): the bank decides to issue, and the capital arrives Delta years later.

While the issue is pending the bank pays no dividend, decides no other issue, keeps
choosing its exposure under the cap of A2, and bears the shortfall penalty. On arrival
it pays K and raises xi >= 0 at (1 + kappa) per unit. The value of deciding at x is

    U(x) = sup_pi E_x[ -int_0^Delta e^{-rho_L t} Lambda(X_t) dt + e^{-rho_L Delta} A(X_Delta) ],
    A(y) = max_{z >= y} [ V(z) - (1 + kappa)(z - y) ] - K,

and V solves max{ max_pi [ sigma^2 V''/2 + b V' ] - rho_L V - Lambda, U - V, 1 - V' } = 0.
With Delta = 0, U = MV on the nodes where an issue is worth making, and the problem
is that of solve_qvi.py. Peura and Keppo liquidate at the minimum, so their delay
carries a risk of failure while the issue is pending. Here the state cannot fall
below 1 (the cap is zero there and the drift positive), so the delay costs only the
penalty, the discounting and the drift over the lag.

The lag is solved backward from A by implicit Euler steps with the upwind generator
of solve_qvi.py, the exposure at each step the maximiser for the current iterate.
Given the exposures and the targets, U is linear in V, U = S (P V + a) + c, so policy
iteration solves each step exactly, as solve_qvi.solve_howard does.
"""
import numpy as np
from scipy.linalg import solve_banded

import solve_qvi as s


def arrival(x, v, p):
    """A = P v + a: the best target z >= y for each node y, and its cost."""
    base = v - (1 + p["kappa"]) * x
    n = len(x)
    t = np.empty(n, dtype=int)
    best = n - 1
    for j in range(n - 1, -1, -1):
        if base[j] >= base[best]:
            best = j
        t[j] = best
    a = -(1 + p["kappa"]) * (x[t] - x) - p["K"]
    return t, a


def lag_map(x, v, p, steps):
    """S, c, t, a with U = S (v[t] + a) + c, and U itself, for the current v."""
    n = len(x)
    dx = x[1] - x[0]
    t, a = arrival(x, v, p)
    if p["delay"] <= 0:
        return np.eye(n), np.zeros(n), t, a, v[t] + a
    h = p["delay"] / steps
    pen = (p["lam"] - 1) * np.maximum(p["R"] - x, 0)
    S = np.eye(n)
    c = np.zeros(n)
    u = v[t] + a
    for _ in range(steps):
        v1, v2 = s.derivs(u, dx)
        pi = s.best_pi(x, v1, v2, p)
        lo, di, up = s.generator(x, pi, p, dx)
        d = di.copy()
        d[0] += lo[0]
        d[-1] += up[-1]                      # reflection at both ends of the grid
        ab = np.zeros((3, n))
        ab[0, 1:] = -h * up[:-1]
        ab[1] = 1 - h * d
        ab[2, :-1] = -h * lo[1:]
        S = solve_banded((1, 1), ab, S)
        c = solve_banded((1, 1), ab, c - h * pen)
        u = solve_banded((1, 1), ab, u - h * pen)
    return S, c, t, a, u


def steps_for(delay):
    return max(20, int(np.ceil(delay / 0.004)))


def solve(p, n=601, v0=None, max_iter=300, steps=None):
    """Policy iteration on the discrete delayed QVI. Returns x, v, U, iterations, residual."""
    g = dict(s.GRID, n=n)
    x = np.linspace(1.0, g["x_max"], n)
    dx = x[1] - x[0]
    steps = steps_for(p["delay"]) if steps is None else steps
    if v0 is None:
        _, v0, _, _ = s.solve_howard(p, g)
    v = v0.copy()
    rows = np.arange(n)
    calm = 0
    for it in range(max_iter):
        G, _, D, pi, lo, di, up, _, pen = s.qvi_terms(x, v, p)
        S, c, t, a, U = lag_map(x, v, p, steps)
        C = U - v
        act = np.argmax(np.stack([G * dx ** 2, C, D]), axis=0)
        act[-1] = 2
        A = np.zeros((n, n))
        r = np.zeros(n)
        ci = rows[act == 0]
        A[ci, ci] = np.where(ci > 0, di[ci], di[0] + lo[0])
        cm = ci[ci > 0]
        A[cm, cm - 1] = lo[cm]
        A[ci, ci + 1] = up[ci]
        r[ci] = pen[ci]
        ii = rows[act == 1]
        if len(ii):
            # v_i - sum_j S_ij v_{t(j)} = sum_j S_ij a_j + c_i
            SP = np.zeros((len(ii), n))
            np.add.at(SP, (np.repeat(np.arange(len(ii)), n), np.tile(t, len(ii))), S[ii].ravel())
            A[ii] = -SP
            A[ii, ii] += 1.0
            r[ii] = S[ii] @ a + c[ii]
        dv = rows[act == 2]
        A[dv, dv] = 1.0
        A[dv, dv - 1] = -1.0
        r[dv] = dx
        v_new = np.linalg.solve(A, r)
        change = np.max(np.abs(v_new - v))
        v = v_new
        calm = calm + 1 if change < 1e-12 else 0
        if calm >= 3:
            break
    G, _, D, *_ = s.qvi_terms(x, v, p)
    U = lag_map(x, v, p, steps)[4]
    resid = np.max(np.abs(np.maximum.reduce([G[:-1] * dx ** 2, (U - v)[:-1], D[:-1]])))
    return x, v, U, it, resid


def thresholds(x, v, U, p):
    """x_L: the highest node where deciding to issue is active, refined by the
    quadratic contact of V - U with zero on the four nodes above it, as
    solve_qvi.thresholds. y_post: where V' falls through 1 + kappa at the best
    target. y*: where V' falls to one above the target."""
    dx = x[1] - x[0]
    v1, _ = s.derivs(v, dx)
    active = np.abs(v - U) < 1e-9
    iL = int(np.max(np.where(active)[0])) if active.any() else 0
    k = iL + int(np.argmax((v - (1 + p["kappa"]) * x)[iL:]))

    def cross(i0, i1, level):
        for i in range(max(i0, 0), min(i1, len(x) - 1)):
            if v1[i] > level >= v1[i + 1]:
                return x[i] + (v1[i] - level) / (v1[i] - v1[i + 1]) * dx
        return float("nan")
    y_post = cross(k - 2, k + 2, 1 + p["kappa"])
    # High contact at y*: V' - 1 is quadratic in (y* - x) below the barrier, so the
    # root of a line through sqrt(slope - 1) on the four intervals below the first
    # interval of unit slope. Node-quantized crossing kept as y_star_node.
    sl = np.diff(v) / dx
    xm = 0.5 * (x[1:] + x[:-1])
    flat = np.where((xm > x[k]) & (sl <= 1 + 1e-10))[0]
    y_star_node = cross(k + 1, len(x) - 1, 1.0 + 1e-12)
    if len(flat) and flat[0] - 4 > k:
        js = np.arange(flat[0] - 4, flat[0])
        b1, b0 = np.polyfit(xm[js], np.sqrt(sl[js] - 1), 1)
        y_star = -b0 / b1 if b1 < 0 else y_star_node
    else:
        y_star = y_star_node
    # Smooth fit at x_L: V - U is quadratic in (x - x_L) above the trigger, and
    # sqrt(V - U) bends slightly, so the root of a quadratic through it on the
    # five nodes above the last active node, not clipped to that node.
    d = np.sqrt(np.maximum(v - U, 0.0))
    js = np.arange(iL + 1, iL + 6)
    c2, c1, c0 = np.polyfit(x[js] - x[iL], d[js], 2)
    roots = np.roots([c2, c1, c0]) if abs(c2) > 0 else np.array([-c0 / c1])
    roots = roots[np.isreal(roots)].real
    x_L = x[iL] + roots[np.argmin(np.abs(roots))] if len(roots) else x[iL]
    if not (iL > 1 and abs(x_L - x[iL]) < 2 * dx):
        x_L = x[iL]
    return dict(y_star=y_star, y_star_node=y_star_node, y_post=y_post, x_L=x_L, x_L_node=x[iL],
                iL=iL, interior=iL > 1)
