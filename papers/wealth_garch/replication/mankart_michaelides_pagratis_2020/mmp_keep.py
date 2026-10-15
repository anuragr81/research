import numpy as np
from numba import njit, prange

from mmp_model import evaluate, nelder_mead, optimise, value_of


@njit(parallel=True, cache=True)
def keep_better(Vn, pol, prev, V, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par, nmax, fmax, nm_iters):
    ne, nl, nw, nb = V.shape
    vd = par[13]
    for idx in prange(ne * nl * nw * nb):
        ie = idx // (nl * nw * nb)
        il = (idx // (nw * nb)) % nl
        iw = (idx // nb) % nw
        ib = idx % nb
        e, l = eg[ie], lg[il]
        p0 = prev[ie, il, iw, ib].copy()
        best = value_of(p0[0], p0[1], p0[2], e, l, iw, ib, V, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par)
        bp = p0
        steps = np.array([max(e * 0.05, 1e-4), 0.02 * (l + nmax), 0.02 * fmax + 1e-6])
        p, v = nelder_mead(p0, steps, e, l, iw, ib, V, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par, False, nm_iters)
        if v > best:
            best, bp = v, p
        if best > Vn[ie, il, iw, ib]:
            Vn[ie, il, iw, ib] = best if best > vd else vd
            pol[ie, il, iw, ib, 0] = bp[0]
            pol[ie, il, iw, ib, 1] = bp[1]
            pol[ie, il, iw, ib, 2] = bp[2]
    return Vn, pol


def sweep(model, V, prev, howard):
    args = (model["eg"], model["lg"], model["wg"], model["Pw"], model["Pb"], model["gn"], model["gw"],
            model["rD"], model["rL"], model["rS"], model["par"])
    Vn, pol = optimise(V, *args, model["qx"], model["qn"], model["qf"], model["nmax"], model["fmax"],
                       model["fix_nf"], model["nm_iters"])
    if prev is not None:
        Vn, pol = keep_better(Vn, pol, prev, V, *args, model["nmax"], model["fmax"], model["nm_iters"])
    for _ in range(howard):
        Vn = evaluate(Vn, pol, *args)
    return Vn, pol


def solve_keep(model, rel=1e-6, howard=500, max_outer=400, log=print):
    vd = model["par"][13]
    V = np.full((len(model["eg"]), len(model["lg"]), len(model["wg"]), model["Pb"].shape[0]), vd)
    prev = None
    fall = 0.0
    for outer in range(max_outer):
        Vn, pol = sweep(model, V, prev, howard)
        diff = float(np.max(np.abs(Vn - V)))
        tol = rel * float(Vn.max() - vd)
        if outer > 0:
            fall = max(fall, float(np.max(V - Vn)) / float(Vn.max() - vd))
        V, prev = Vn, pol
        log(f"outer {outer}: max change {diff:.3e} (tolerance {tol:.3e})")
        if diff <= tol:
            return V, pol, outer, diff, True, fall
    return V, pol, max_outer, diff, False, fall
