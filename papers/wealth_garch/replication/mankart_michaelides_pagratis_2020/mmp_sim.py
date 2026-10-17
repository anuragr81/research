import numpy as np

import stage2
from mmp_config import PB, TABLE2


def simulate_counts(m, V, pol, size):
    rng = np.random.default_rng(stage2.SEED)
    t2 = TABLE2[size]
    mu_g, sd_g = np.array(t2["g_mean"]) / 100, np.array(t2["g_sd"]) / 100
    N, T, KEEP = stage2.N_BANKS, stage2.T, stage2.KEEP
    etop, ltop = m["eg"][-1], m["lg"][-1]
    e = np.full(N, 0.10)
    l = np.full(N, 0.6)
    iw = np.full(N, len(m["wg"]) // 2)
    alive = np.ones(N, dtype=np.bool_)
    b = 0
    out = np.zeros((N, 6))
    rec = np.full((KEEP, N, 6), np.nan)
    keys = ("not finite", "e above 0.4", "e above top", "l above top", "failures")
    counts = {"all": dict.fromkeys(keys, 0), "last 500": dict.fromkeys(keys, 0)}
    emax = {"all": 0.0, "last 500": 0.0}
    args = (V, pol, m["eg"], m["lg"], m["wg"], m["Pw"], m["Pb"], m["gn"], m["gw"], m["rD"], m["rL"], m["rS"], m["par"],
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
        fin = np.isfinite(e) & np.isfinite(l)
        row = {"not finite": int((~fin).sum()), "e above 0.4": int((e[fin] > 0.4).sum()),
               "e above top": int((e[fin] > etop).sum()), "l above top": int((l[fin] > ltop).sum())}
        stage2.step(e, l, iw, alive, b, b2, u_w, z_g, *args, out)
        row["failures"] = int((out[:, 5] == 1.0).sum())
        for span in ("all", "last 500") if t >= T - KEEP else ("all",):
            for k in keys:
                counts[span][k] += row[k]
            if fin.any():
                emax[span] = max(emax[span], float(e[fin].max()))
        if t >= T - KEEP:
            rec[t - (T - KEEP)] = out
        b = b2
    return rec, counts, emax
