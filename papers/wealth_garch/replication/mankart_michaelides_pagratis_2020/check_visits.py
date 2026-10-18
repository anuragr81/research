import sys

import numpy as np

import stage2
from mmp_config import PB, TABLE2, model
from mmp_fast import solve_fast
from validate_fast import perturbed


def log(s):
    print(s, file=sys.stderr, flush=True)


def solve(m, name, start=None):
    V, pol, outer, diff, ok, fall, full = solve_fast(m, rel=1e-6, howard=50, max_outer=400, start=start, log=log)
    np.savez(name, V=V, pol=pol)
    print(f"{name}: {'converged' if ok else 'NOT converged'} after {outer} sweeps ({full} full), last change {diff:.1e}")
    return V, pol


def visits(m, V, pol):
    rng = np.random.default_rng(stage2.SEED)
    t2 = TABLE2["small"]
    mu_g, sd_g = np.array(t2["g_mean"]) / 100, np.array(t2["g_sd"]) / 100
    N, T = stage2.N_BANKS, stage2.T
    eg, lg = m["eg"], m["lg"]
    seen = np.zeros(V.shape, dtype=np.int64)
    e = np.full(N, 0.10)
    l = np.full(N, 0.6)
    iw = np.full(N, len(m["wg"]) // 2)
    alive = np.ones(N, dtype=np.bool_)
    b = 0
    out = np.zeros((N, 6))
    args = (V, pol, eg, lg, m["wg"], m["Pw"], m["Pb"], m["gn"], m["gw"], m["rD"], m["rL"], m["rS"], m["par"],
            m["qx"], m["qn"], m["qf"], m["nmax"], m["fmax"], mu_g, sd_g)
    for t in range(T):
        b2 = int(rng.random() < PB[b, 1])
        u_w = rng.random(N)
        z_g = rng.standard_normal(N)
        dead = ~alive
        if dead.any():
            e[dead] = e[alive].mean()
            l[dead] = l[alive].mean()
            iw[dead] = 0
            alive[dead] = True
        ie = np.clip(np.searchsorted(eg, e) - 1, 0, len(eg) - 2)
        il = np.clip(np.searchsorted(lg, l) - 1, 0, len(lg) - 2)
        for de in (0, 1):
            for dl in (0, 1):
                np.add.at(seen, (ie + de, il + dl, iw, np.full(N, b)), 1)
        stage2.step(e, l, iw, alive, b, b2, u_w, z_g, *args, out)
        b = b2
    return seen


def main():
    m = model("small", ne=41, nl=41)
    V, pol = solve(m, "solved_fast_small_41.npz")
    mp = perturbed()
    Vw, pw = solve(mp, "solved_fast_small_41_p_warm.npz", start=(V, pol))
    Vc, pc = solve(mp, "solved_fast_small_41_p_cold.npz")
    seen = visits(mp, Vw, pw) + visits(mp, Vc, pc)
    span = float(Vc.max() - mp["par"][13])
    d = np.abs(Vw - Vc) / span
    vis = seen > 0
    print(f"cells {V.size}, visited {int(vis.sum())} ({100 * vis.mean():.2f}%)")
    print(f"largest difference over visited cells {d[vis].max():.3e}, over cells not visited {d[~vis].max():.3e} of the value range")
    big = d > 1e-4
    print(f"visits to cells with a difference above 1e-4: {int(seen[big].sum())} of {int(seen.sum())} corner-visits "
          f"({100 * seen[big].sum() / seen.sum():.4f}%)")
    print(f"visited cells with a difference above 1e-4: {int((vis & big).sum())}")
    print(f"rule: {'adopted' if d[vis].max() <= 1e-4 else 'not adopted'}")


if __name__ == "__main__":
    main()
