import sys

import numpy as np

import stage2
from mmp_config import PB, TABLE2, model

size, npts = sys.argv[1], int(sys.argv[2])
data = np.load(f"solved_{size}_{npts}.npz")
V, pol = data["V"], data["pol"]
m = model(size, ne=npts, nl=npts)
rng = np.random.default_rng(stage2.SEED)
t2 = TABLE2[size]
mu_g, sd_g = np.array(t2["g_mean"]) / 100, np.array(t2["g_sd"]) / 100
N, T, KEEP = stage2.N_BANKS, stage2.T, stage2.KEEP
e = np.full(N, 0.10)
l = np.full(N, 0.6)
iw = np.full(N, len(m["wg"]) // 2)
alive = np.ones(N, dtype=np.bool_)
b = 0
out = np.zeros((N, 6))
rec = np.full((KEEP, N, 6), np.nan)
st = np.full((KEEP, N, 4), np.nan)
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
    if t >= T - KEEP:
        st[t - (T - KEEP), :, 0] = e
        st[t - (T - KEEP), :, 1] = l
        st[t - (T - KEEP), :, 2] = iw
        st[t - (T - KEEP), :, 3] = b
    stage2.step(e, l, iw, alive, b, b2, u_w, z_g, *args, out)
    if t >= T - KEEP:
        rec[t - (T - KEEP)] = out
    b = b2
print(f"{size} {npts}x{npts} moments from the saved arrays: {stage2.moments(rec)}")
failed = rec[:, :, 5] == 1.0
print(f"bank-quarters {rec.shape[0] * rec.shape[1]}, failures {int(failed.sum())}")
names = ["loans/assets", "deposits/assets", "equity/assets", "profit/equity", "dividends/equity"]
for k, name in enumerate(names):
    col = rec[:, :, k]
    bad = ~failed & ~np.isfinite(col)
    print(f"{name}: NaN {int((~failed & np.isnan(col)).sum())}, +inf {int(np.isposinf(col).sum())}, "
          f"-inf {int(np.isneginf(col).sum())} (outside failure quarters); banks with no finite value "
          f"{int((~np.isfinite(col).any(axis=0)).sum())}")
    if bad.any():
        idx = np.argwhere(bad)[:10]
        for q, i in idx:
            s = st[q, i]
            x, n, f, v = stage2.choose(s[0], s[1], int(s[2]), int(s[3]), V, pol, m["eg"], m["lg"], m["wg"], m["Pw"], m["Pb"],
                                       m["gn"], m["gw"], m["rD"], m["rL"], m["rS"], m["par"], m["qx"], m["qn"], m["qf"],
                                       m["nmax"], m["fmax"])
            print(f"   quarter {q} bank {i}: e {s[0]:.6g} l {s[1]:.6g} w node {int(s[2])} b {int(s[3])}; "
                  f"policy x {x:.6g} n {n:.6g} f {f:.6g}, value {v:.6g}; row {rec[q, i]}")
