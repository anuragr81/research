import numpy as np
from scipy import integrate


def score_FG(mu, dist_r, dist_s, n=300000, ngrid=4001):
    r = dist_r.rvs(size=n)
    s = dist_s.rvs(size=n)
    Y = mu * r
    X = mu * r + (1 - mu) * s
    hi = max(X.max(), Y.max())
    grid = np.linspace(0.0, hi, ngrid)
    F = np.searchsorted(np.sort(X), grid, side="right") / n
    G = np.searchsorted(np.sort(Y), grid, side="right") / n
    return F, G, grid


def random_pair(grid, rng):
    k = rng.integers(2, 7)
    xs = np.sort(rng.random(k))
    ys = np.sort(rng.random(k))
    G = np.interp(grid, np.r_[0, xs, 1], np.r_[0, ys, 1])
    gap = rng.random() * np.interp(
        grid, np.r_[0, np.sort(rng.random(3)), 1], np.r_[0, rng.random(3), 0]
    )
    F = np.clip(G - gap, 0, 1)
    F = np.maximum.accumulate(F)
    G = np.maximum.accumulate(G)
    F[0] = G[0] = 0.0
    F[-1] = G[-1] = 1.0
    return F, G


def delta(F, G, grid, m, Q, inc_invests=True, V=1.0):
    C = F if inc_invests else G
    H = F**m * G ** (Q - 1 - m) * C
    dF = np.gradient(F, grid)
    dG = np.gradient(G, grid)
    return V * (
        integrate.simpson(H * dF, x=grid) - integrate.simpson(H * dG, x=grid)
    )


def delta_via_phi(F, G, grid, m, Q, inc_invests=True, V=1.0):
    C = F if inc_invests else G
    H = F**m * G ** (Q - 1 - m) * C
    return V * integrate.simpson((G - F) * np.gradient(H, grid), x=grid)


def delta_step_identity(F, G, grid, m, Q, inc_invests=True, V=1.0):
    C = F if inc_invests else G
    K = F**m * G ** (Q - 2 - m) * C
    phi = G - F
    return -0.5 * V * integrate.simpson(phi**2 * np.gradient(K, grid), x=grid)


def kappa(w, c, gamma=2.0):
    w = np.asarray(w, dtype=float)
    if abs(gamma - 1.0) < 1e-9:
        return np.log(w) - np.log(np.maximum(w - c, 1e-12))
    f = lambda x: x ** (1 - gamma) / (1 - gamma)
    return f(w) - f(np.maximum(w - c, 1e-12))


def equilibrium_k(wealths, c, V, Q, delta_fn, gamma=2.0):
    s = np.sort(wealths)[::-1]
    k = 0
    for j in range(min(Q, len(s))):
        if s[j] - c <= 1e-9:
            break
        if kappa(s[j], c, gamma) <= delta_fn(j, Q, V):
            k = j + 1
        else:
            break
    return k


def mean_preserving_spread(w, lam):
    m = w.mean()
    return m + (w - m) * lam
