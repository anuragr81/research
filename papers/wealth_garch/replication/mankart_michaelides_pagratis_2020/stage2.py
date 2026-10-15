import hashlib
import math
import os
import sys
import time

import numpy as np
from numba import njit, prange

from mmp_config import PB, TABLE2, model
from mmp_a1 import solve_a1
from mmp_model import NEG, interp2, nelder_mead, value_of

THEIRS = {"large": {"failure rate": 0.092, "loans/assets": 0.704, "deposits/assets": 0.638, "equity/assets": 0.065,
                    "profit/equity": 0.055, "dividends/equity": 0.029, "sd loans/assets": 0.100,
                    "sd deposits/assets": 0.062, "sd equity/assets": 0.013, "sd profit/equity": 0.035,
                    "sd dividends/equity": 0.011},
          "small": {"failure rate": 0.051, "loans/assets": 0.626, "deposits/assets": 0.891, "equity/assets": 0.067,
                    "profit/equity": 0.029, "dividends/equity": 0.012, "sd loans/assets": 0.050,
                    "sd deposits/assets": 0.021, "sd equity/assets": 0.015, "sd profit/equity": 0.024,
                    "sd dividends/equity": 0.007}}
RATIOS = ["loans/assets", "deposits/assets", "equity/assets", "profit/equity", "dividends/equity"]
N_BANKS, T, KEEP, SEED = 10_000, 2_000, 500, 20261014


@njit(cache=True)
def choose(e, l, iw, ib, V, pol, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par, qx, qn, qf, nmax, fmax):
    x = interp2(pol[:, :, iw, ib, 0], eg, lg, e, l)
    n = interp2(pol[:, :, iw, ib, 1], eg, lg, e, l)
    f = interp2(pol[:, :, iw, ib, 2], eg, lg, e, l)
    v = value_of(x, n, f, e, l, iw, ib, V, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par)
    if v > NEG:
        return x, n, f, v
    best, bx, bn, bf = NEG, 0.0, 0.0, 0.0
    for a in range(qx.shape[0]):
        for b in range(qn.shape[0]):
            for d in range(qf.shape[0]):
                xx, nn, ff = qx[a] * e, -l + qn[b] * (l + nmax), qf[d] * fmax
                vv = value_of(xx, nn, ff, e, l, iw, ib, V, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par)
                if vv > best:
                    best, bx, bn, bf = vv, xx, nn, ff
    if best > NEG:
        p, vv = nelder_mead(np.array([bx, bn, bf]), np.array([max(e * 0.05, 1e-4), 0.02 * (l + nmax), 0.02 * fmax + 1e-6]),
                            e, l, iw, ib, V, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par, False, 60)
        if vv > best:
            return p[0], p[1], p[2], vv
    return bx, bn, bf, best


@njit(parallel=True, cache=True)
def step(e, l, iw, alive, ib, ib2, u_w, z_g, V, pol, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par, qx, qn, qf, nmax, fmax,
         mu_g, sd_g, out):
    tau, theta, phiN, psi, phiF, phiE, c, vd = par[2], par[3], par[4], par[5], par[6], par[7], par[8], par[13]
    nb = e.shape[0]
    for i in prange(nb):
        out[i, :] = np.nan
        if not alive[i]:
            continue
        vhere = interp2(V[:, :, iw[i], ib], eg, lg, e[i], l[i]) if e[i] > 0 else vd
        if e[i] <= 0 or vhere <= vd:
            alive[i] = False
            out[i, 5] = 1.0
            continue
        x, n, f, v = choose(e[i], l[i], iw[i], ib, V, pol, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par, qx, qn, qf, nmax, fmax)
        if v <= vd:
            alive[i] = False
            out[i, 5] = 1.0
            continue
        eq = e[i] - x
        s = 1.0 + f + e[i] - x - l[i] - n
        assets = l[i] + n + s
        cum = np.cumsum(Pw[ib2, iw[i]])
        j = 0
        while j < cum.shape[0] - 1 and u_w[i] > cum[j]:
            j += 1
        w2 = wg[j]
        gN = phiN * n * n if n > 0 else psi * phiN * n * n
        pi = (rL[ib2] - w2) * (l[i] + n) + rS[ib2] * s - rD[ib2] - (rD[ib2] * f + phiF * f * f - phiE * eq * eq) - gN - c
        after = (1.0 - tau) * pi if pi > 0 else pi
        G = math.exp(mu_g[ib2] + sd_g[ib2] * z_g[i])
        out[i, 0] = (l[i] + n) / assets
        out[i, 1] = 1.0 / assets
        out[i, 2] = eq / assets
        out[i, 3] = pi / eq
        out[i, 4] = x / eq
        out[i, 5] = 0.0
        e[i] = (eq + after) / G
        l[i] = (1.0 - theta - w2) * (l[i] + n) / G
        iw[i] = j


def simulate(m, V, pol, size):
    rng = np.random.default_rng(SEED)
    t2 = TABLE2[size]
    mu_g = np.array(t2["g_mean"]) / 100
    sd_g = np.array(t2["g_sd"]) / 100
    e = np.full(N_BANKS, 0.10)
    l = np.full(N_BANKS, 0.6)
    iw = np.full(N_BANKS, len(m["wg"]) // 2)
    alive = np.ones(N_BANKS, dtype=np.bool_)
    b = 0
    out = np.zeros((N_BANKS, 6))
    rec = np.full((KEEP, N_BANKS, 6), np.nan)
    args = (V, pol, m["eg"], m["lg"], m["wg"], m["Pw"], m["Pb"], m["gn"], m["gw"], m["rD"], m["rL"], m["rS"], m["par"],
            m["qx"], m["qn"], m["qf"], m["nmax"], m["fmax"], mu_g, sd_g)
    for t in range(T):
        b2 = int(rng.random() < PB[b, 1])
        u_w = rng.random(N_BANKS)
        z_g = rng.standard_normal(N_BANKS)
        dead = ~alive
        if dead.any():
            e[dead] = e[alive].mean()
            l[dead] = l[alive].mean()
            iw[dead] = 0
            alive[dead] = True
        step(e, l, iw, alive, b, b2, u_w, z_g, *args, out)
        if t >= T - KEEP:
            rec[t - (T - KEEP)] = out
        b = b2
    return rec


def moments(rec):
    res = {"failure rate": 100 * np.nanmean(rec[:, :, 5])}
    live = ~np.all(np.isnan(rec[:, :, 0]), axis=0)
    res["banks with no live quarter"] = int((~live).sum())
    for k, name in enumerate(RATIOS):
        res[name] = float(np.mean(np.nanmean(rec[:, live, k], axis=0)))
        res["sd " + name] = float(np.mean(np.nanstd(rec[:, live, k], axis=0)))
    return res


def verdict(ours, theirs, name):
    if name == "failure rate":
        return abs(ours - theirs) <= 0.05
    return abs(ours - theirs) <= (0.005 if abs(theirs) < 0.05 else 0.20 * abs(theirs))


def run(size, npts, log):
    m = model(size, ne=npts, nl=npts)
    t0 = time.time()
    V, pol, outer, diff, ok, fall = solve_a1(m, rel=1e-6, howard=500, max_outer=400, log=log)
    log(f"{size} {npts}x{npts}: {'converged' if ok else 'NOT converged'} after {outer} sweeps, last change {diff:.1e}, "
        f"largest fall in any cell {fall:.1e} of the value range, {time.time() - t0:.0f}s")
    mom = moments(simulate(m, V, pol, size))
    print(f"{size} {npts}x{npts}: {'converged' if ok else 'not converged'} after {outer} sweeps; moments {mom}", flush=True)
    return mom, outer, diff, ok


def main():
    for f in ("mmp_model.py", "mmp_config.py", "mmp_a1.py", "stage2.py"):
        print(f"{f} sha256 {hashlib.sha256(open(f, 'rb').read()).hexdigest()}")
    results = {}
    for npts in (41, 61):
        for size in ("large", "small"):
            results[(size, npts)] = run(size, npts, lambda s: print(s, file=sys.stderr, flush=True))
    for size in ("large", "small"):
        mom41, outer41, diff41, ok41 = results[(size, 41)]
        mom61, outer61, diff61, ok61 = results[(size, 61)]
        print(f"\n===== {size}: 41x41 grid ({'converged' if ok41 else 'NOT converged'}, {outer41} sweeps, last change "
              f"{diff41:.1e}, {mom41['banks with no live quarter']} banks with no live quarter); 61x61 grid "
              f"({'converged' if ok61 else 'NOT converged'}, {outer61} sweeps, last change {diff61:.1e}, "
              f"{mom61['banks with no live quarter']} banks with no live quarter)")
        holds = 0
        for name, theirs in THEIRS[size].items():
            a, b = mom41[name], mom61[name]
            ok = verdict(a, theirs, name) and ok41
            holds += ok
            moved = abs(b - a) > 0.10 * abs(a) if a != 0 else b != 0
            print(f"  {name:20s} ours {a:8.4f} [{theirs}] -> {'reproduces' if ok else 'differs'}; "
                  f"61x61 {b:8.4f}{' (moves more than 10%: not converged)' if moved else ''}")
        print(f"  {size}: {holds} of {len(THEIRS[size])} moments reproduce")


if __name__ == "__main__":
    main()
