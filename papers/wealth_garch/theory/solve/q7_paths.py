"""Path simulator for Question 7: the theory paper's model (solve_qvi.py) under its
optimal policy, simulated by Euler-Maruyama.

The policy comes from solve_qvi.solve_howard: the exposure pi(x) on the grid,
interpolated between nodes, a dividend barrier y* that reflects the state, and a
recapitalisation trigger x_L at which the bank issues up to the injection target
y_post. When no issue is ever worth making (V > MV at every node) the bank never
issues, and the state is held at the distress boundary by reflection, which the
boundary does locally in the model since the cap and the diffusion vanish there and
the drift r - mu_L + gamma is positive.

Two measures. The value equation is written under the measure that weights each path
by its liabilities (Bayraktar et al. 2026, Proposition 2.1), under which the drift is
mu(x, pi) = x((1 - pi) r + pi mu_s - mu_L) + gamma. Under the physical measure the
drift is mu(x, pi) + sigma_L^2 (x - 1) - c pi sigma sigma_L x (their appendix, the
Ito step before Girsanov). Values are computed under the first, safety under the second.

Path statistics over [0, T] from x0:
  p_no_deficit   probability the state never falls below the reference R
  p_no_issue     probability of no issue
  p_above_1_02   probability the state never falls to 1.02, the distress threshold of
                 Bayraktar et al., whose survival probability this is
  issues         expected number of issues
  raised         expected capital raised, per unit of initial liabilities
  time_deficit   expected time below R, years
  shortfall      expected integral of (R - x)^+ dt, undiscounted
  min_x_q05      5th percentile of the path minimum
  pi_mean        time average of the risky exposure along the path
  pi_over_cap    time average of the exposure over the cap pi_bar(x) = u(x)/x
and, with discounting at rho_L over a long horizon, the discounted dividends, issue
costs, shortfall and issue count, for the check against the value function and the
Feynman-Kac solutions W and N.
"""
import numpy as np

import solve_qvi as s

BGK = 0.5826


def policy(p, n=601):
    """Solve and return the grid, the value, the exposure and the thresholds."""
    x, v, it, resid = s.solve_howard(p, dict(s.GRID, n=n))
    th = s.thresholds(x, v, p)
    q = th["q"]
    mv, _ = s.impulse(x, v, p)
    issues = bool(np.any(np.abs(v - mv) < 1e-9))
    return dict(x=x, v=v, pi=q["pi"], y_star=float(th["y_star"]), x_L=float(th["x_L"]) if issues else None,
                y_post=float(th["y_post"]) if issues else None, interior=bool(issues and q["iL"] > 1),
                issues=issues, q=q, resid=float(resid))


def simulate(p, pol, x0=1.2, T=5.0, dt=0.001, paths=20000, seed=0, measure="P", discount=False):
    rng = np.random.default_rng(seed)
    x = np.full(paths, float(x0))
    grid, pig = pol["x"], pol["pi"]
    ys, xL, yp = pol["y_star"], pol["x_L"], pol["y_post"]
    R, rhoL = p["R"], p["rho"] - p["mu_L"]
    c, sig, sL = p["c"], p["sigma"], p["sigma_L"]
    steps = int(round(T / dt))
    sq = np.sqrt(dt)
    deficit_ever = np.zeros(paths, bool)
    below_102 = np.zeros(paths, bool)
    n_iss = np.zeros(paths)
    raised = np.zeros(paths)
    t_def = np.zeros(paths)
    short = np.zeros(paths)
    xmin = x.copy()
    d_div = np.zeros(paths)
    d_cost = np.zeros(paths)
    d_short = np.zeros(paths)
    d_n = np.zeros(paths)
    pi_sum = np.zeros(paths)
    capr_sum = np.zeros(paths)

    def vol(x, pi):
        return np.sqrt(np.maximum(s.sig2(x, pi, p), 0.0))

    def issue(x, disc):
        # Broadie, Glasserman and Kou (1997): monitoring at discrete dates misses
        # crossings between them, corrected by moving the barrier by 0.5826 sigma sqrt(dt).
        hit = x <= xL + BGK * vol(x, np.interp(x, grid, pig)) * sq
        if hit.any():
            xi = yp - x[hit]
            n_iss[hit] += 1
            raised[hit] += xi
            if discount:
                d_cost[hit] += disc * (p["K"] + (1 + p["kappa"]) * xi)
                d_n[hit] += disc
            x[hit] = yp
        return x

    if pol["issues"]:
        x = issue(x, 1.0)
    for k in range(steps):
        disc = np.exp(-rhoL * k * dt)
        pi = np.interp(x, grid, pig)
        cap = s.pi_bar(x, p)
        pi_sum += pi
        capr_sum += np.where(cap > 1e-12, pi / np.maximum(cap, 1e-12), 1.0)
        drift = x * ((1 - pi) * p["r"] + pi * p["mu"] - p["mu_L"]) + p["gamma"]
        if measure == "P":
            drift = drift + sL ** 2 * (x - 1) - c * pi * sig * sL * x
        z1 = rng.standard_normal(paths)
        z2 = c * z1 + np.sqrt(1 - c * c) * rng.standard_normal(paths)
        gap = np.maximum(R - x, 0.0)
        t_def += (gap > 0) * dt
        short += gap * dt
        if discount:
            d_short += disc * gap * dt
        x = x + drift * dt + pi * x * sig * sq * z1 + sL * (1 - x) * sq * z2
        top = ys - BGK * vol(np.full(1, ys), np.interp(ys, grid, pig))[0] * sq
        over = x > top
        if over.any():
            if discount:
                d_div[over] += disc * (x[over] - top)
            x[over] = top
        if pol["issues"]:
            x = issue(x, np.exp(-rhoL * (k + 1) * dt))
        x = np.maximum(x, 1.0)
        deficit_ever |= x < R
        below_102 |= x <= 1.02
        np.minimum(xmin, x, out=xmin)
    out = dict(p_no_deficit=1 - deficit_ever.mean(), p_no_issue=float((n_iss == 0).mean()),
               p_above_1_02=1 - below_102.mean(), issues=n_iss.mean(), raised=raised.mean(),
               time_deficit=t_def.mean(), shortfall=short.mean(), min_x_q05=float(np.quantile(xmin, 0.05)),
               se_p=float(np.sqrt(0.25 / paths)), pi_mean=float(pi_sum.mean() / steps),
               pi_over_cap=float(capr_sum.mean() / steps))
    if discount:
        lam1 = p["lam"] - 1
        total = d_div - d_cost - lam1 * d_short
        out.update(div=d_div.mean(), cost=d_cost.mean(), W=d_short.mean(), N=d_n.mean(),
                   V=total.mean(), V_se=float(total.std() / np.sqrt(paths)),
                   W_se=float(d_short.std() / np.sqrt(paths)), N_se=float(d_n.std() / np.sqrt(paths)))
    return {k: float(v) for k, v in out.items()}


def feynman_kac_at(p, pol, x0):
    """W and N at x0 under the optimal policy, by solve_qvi.feynman_kac (needs an issue)."""
    q, x = pol["q"], pol["x"]
    idx, W = s.feynman_kac(x, q["pi"], p, q["iL"], q["ipost"], q["istar"], np.maximum(p["R"] - x, 0), 0.0)
    _, N = s.feynman_kac(x, q["pi"], p, q["iL"], q["ipost"], q["istar"], np.zeros_like(x), 1.0)
    return float(np.interp(x0, x[idx], W)), float(np.interp(x0, x[idx], N))
