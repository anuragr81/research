import math

import numpy as np
from numba import njit, prange

NEG = -1e30


def tauchen_common(means, rhos, sds, n, width=3.0):
    lo = min(m - width * s for m, s in zip(means, sds))
    hi = max(m + width * s for m, s in zip(means, sds))
    grid = np.linspace(lo, hi, n)
    step = grid[1] - grid[0]
    trans = []
    for m, r, s in zip(means, rhos, sds):
        mu, sig = m * (1 - r), s * math.sqrt(1 - r * r)
        P = np.zeros((n, n))
        for i in range(n):
            cond = mu + r * grid[i]
            for j in range(n):
                upper = 1.0 if j == n - 1 else _ncdf((grid[j] + step / 2 - cond) / sig)
                lower = 0.0 if j == 0 else _ncdf((grid[j] - step / 2 - cond) / sig)
                P[i, j] = upper - lower
        trans.append(P)
    return grid, np.array(trans)


def _ncdf(z):
    return 0.5 * (1 + math.erf(z / math.sqrt(2)))


def gauss_hermite_lognormal(means, sds, n):
    x, wts = np.polynomial.hermite.hermgauss(n)
    nodes = np.array([[math.exp(m + math.sqrt(2) * s * xi) for xi in x] for m, s in zip(means, sds)])
    return nodes, wts / math.sqrt(math.pi)


@njit(cache=True)
def interp2(V, eg, lg, e, l):
    ne, nl = eg.shape[0], lg.shape[0]
    if e <= eg[0]:
        ie, te = 0, 0.0
    elif e >= eg[ne - 1]:
        ie, te = ne - 2, 1.0
    else:
        ie = np.searchsorted(eg, e) - 1
        te = (e - eg[ie]) / (eg[ie + 1] - eg[ie])
    if l <= lg[0]:
        il, tl = 0, 0.0
    elif l >= lg[nl - 1]:
        il, tl = nl - 2, 1.0
    else:
        il = np.searchsorted(lg, l) - 1
        tl = (l - lg[il]) / (lg[il + 1] - lg[il])
    return ((1 - te) * (1 - tl) * V[ie, il] + te * (1 - tl) * V[ie + 1, il]
            + (1 - te) * tl * V[ie, il + 1] + te * tl * V[ie + 1, il + 1])


@njit(cache=True)
def value_of(x, n, f, e, l, iw, ib, V, eg, lg, wg, Pw, Pb, gam_nodes, gam_wts, rD, rL, rS, par):
    beta, gamma, tau, theta, phiN, psi, phiF, phiE, c, lw, lu, omL, omS, vd = (
        par[0], par[1], par[2], par[3], par[4], par[5], par[6], par[7], par[8], par[9], par[10], par[11], par[12], par[13])
    eq = e - x
    s = 1.0 + f + e - x - l - n
    loans = l + n
    if x <= 0.0 or eq <= 0.0 or s < 0.0 or f < 0.0 or loans < 0.0:
        return NEG
    if omL * loans + omS * s > lw * eq or loans + s > lu * eq:
        return NEG
    gN = phiN * n * n if n > 0 else psi * phiN * n * n
    cont = 0.0
    for b2 in range(Pb.shape[1]):
        pb = Pb[ib, b2]
        if pb == 0.0:
            continue
        for j in range(wg.shape[0]):
            pw = Pw[b2, iw, j]
            if pw == 0.0:
                continue
            w2 = wg[j]
            pi = (rL[b2] - w2) * loans + rS[b2] * s - rD[b2] - (rD[b2] * f + phiF * f * f - phiE * eq * eq) - gN - c
            after = (1.0 - tau) * pi if pi > 0 else pi
            for k in range(gam_nodes.shape[1]):
                G = gam_nodes[b2, k]
                e2 = (eq + after) / G
                l2 = (1.0 - theta - w2) * loans / G
                if e2 <= 0.0:
                    v2 = vd
                else:
                    v2 = interp2(V[:, :, j, b2], eg, lg, e2, l2)
                    if v2 < vd:
                        v2 = vd
                cont += pb * pw * gam_wts[k] * G ** (1.0 - gamma) * v2
    return x ** (1.0 - gamma) / (1.0 - gamma) + beta * cont


@njit(cache=True)
def nelder_mead(p0, steps, e, l, iw, ib, V, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par, fix_nf, iters):
    dim = 1 if fix_nf else 3
    pts = np.zeros((dim + 1, 3))
    vals = np.zeros(dim + 1)
    for k in range(dim + 1):
        pts[k] = p0
        if k > 0:
            pts[k, k - 1] += steps[k - 1]
        vals[k] = -value_of(pts[k, 0], pts[k, 1], pts[k, 2], e, l, iw, ib, V, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par)
    for _ in range(iters):
        order = np.argsort(vals)
        pts, vals = pts[order], vals[order]
        cen = np.zeros(3)
        for k in range(dim):
            cen += pts[k]
        cen /= dim
        xr = cen + (cen - pts[dim])
        fr = -value_of(xr[0], xr[1], xr[2], e, l, iw, ib, V, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par)
        if fr < vals[0]:
            xe = cen + 2.0 * (cen - pts[dim])
            fe = -value_of(xe[0], xe[1], xe[2], e, l, iw, ib, V, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par)
            if fe < fr:
                pts[dim], vals[dim] = xe, fe
            else:
                pts[dim], vals[dim] = xr, fr
        elif fr < vals[dim - 1]:
            pts[dim], vals[dim] = xr, fr
        else:
            xc = cen + 0.5 * (pts[dim] - cen)
            fc = -value_of(xc[0], xc[1], xc[2], e, l, iw, ib, V, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par)
            if fc < vals[dim]:
                pts[dim], vals[dim] = xc, fc
            else:
                for k in range(1, dim + 1):
                    pts[k] = pts[0] + 0.5 * (pts[k] - pts[0])
                    vals[k] = -value_of(pts[k, 0], pts[k, 1], pts[k, 2], e, l, iw, ib, V, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par)
    best = np.argmin(vals)
    return pts[best], -vals[best]


@njit(parallel=True, cache=True)
def optimise(V, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par, qx, qn, qf, nmax, fmax, fix_nf, nm_iters):
    ne, nl, nw, nb = V.shape
    Vn = np.empty_like(V)
    pol = np.zeros((ne, nl, nw, nb, 3))
    vd = par[13]
    for idx in prange(ne * nl * nw * nb):
        ie = idx // (nl * nw * nb)
        il = (idx // (nw * nb)) % nl
        iw = (idx // nb) % nw
        ib = idx % nb
        e, l = eg[ie], lg[il]
        best, bx, bn, bf = NEG, 0.0, 0.0, 0.0
        for a in range(qx.shape[0]):
            x = qx[a] * e
            for b in range(1 if fix_nf else qn.shape[0]):
                n = 0.0 if fix_nf else -l + qn[b] * (l + nmax)
                for d in range(1 if fix_nf else qf.shape[0]):
                    f = 0.0 if fix_nf else qf[d] * fmax
                    v = value_of(x, n, f, e, l, iw, ib, V, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par)
                    if v > best:
                        best, bx, bn, bf = v, x, n, f
        if best > NEG and nm_iters > 0:
            p0 = np.array([bx, bn, bf])
            steps = np.array([max(e * 0.05, 1e-4), 0.02 * (l + nmax), 0.02 * fmax + 1e-6])
            p, v = nelder_mead(p0, steps, e, l, iw, ib, V, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par, fix_nf, nm_iters)
            if v > best:
                best, bx, bn, bf = v, p[0], p[1], p[2]
        Vn[ie, il, iw, ib] = best if best > vd else vd
        pol[ie, il, iw, ib, 0] = bx
        pol[ie, il, iw, ib, 1] = bn
        pol[ie, il, iw, ib, 2] = bf
    return Vn, pol


@njit(parallel=True, cache=True)
def evaluate(V, pol, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par):
    ne, nl, nw, nb = V.shape
    Vn = np.empty_like(V)
    vd = par[13]
    for idx in prange(ne * nl * nw * nb):
        ie = idx // (nl * nw * nb)
        il = (idx // (nw * nb)) % nl
        iw = (idx // nb) % nw
        ib = idx % nb
        v = value_of(pol[ie, il, iw, ib, 0], pol[ie, il, iw, ib, 1], pol[ie, il, iw, ib, 2], eg[ie], lg[il], iw, ib,
                     V, eg, lg, wg, Pw, Pb, gn, gw, rD, rL, rS, par)
        Vn[ie, il, iw, ib] = v if v > vd else vd
    return Vn


def solve(model, tol=1e-6, howard=50, max_outer=400, log=print):
    V = np.full((len(model["eg"]), len(model["lg"]), len(model["wg"]), model["Pb"].shape[0]), 0.0)
    V[:] = model["par"][13] if model["par"][13] > NEG / 10 else 0.0
    args = (model["eg"], model["lg"], model["wg"], model["Pw"], model["Pb"], model["gn"], model["gw"],
            model["rD"], model["rL"], model["rS"], model["par"])
    for outer in range(max_outer):
        Vn, pol = optimise(V, *args, model["qx"], model["qn"], model["qf"], model["nmax"], model["fmax"],
                           model["fix_nf"], model["nm_iters"])
        for _ in range(howard):
            Vn = evaluate(Vn, pol, *args)
        diff = float(np.max(np.abs(Vn - V)))
        V = Vn
        log(f"outer {outer}: max change {diff:.3e}")
        if diff < tol:
            return V, pol, outer, diff
    return V, pol, max_outer, diff
