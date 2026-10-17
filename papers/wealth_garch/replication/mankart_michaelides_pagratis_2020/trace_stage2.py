import math

import numpy as np

import stage2
from mmp_config import PB, TABLE2, model

data = np.load("solved_large_41.npz")
V, pol = data["V"], data["pol"]
m = model("large", ne=41, nl=41)
rng = np.random.default_rng(stage2.SEED)
t2 = TABLE2["large"]
mu_g, sd_g = np.array(t2["g_mean"]) / 100, np.array(t2["g_sd"]) / 100
N, T = stage2.N_BANKS, stage2.T
par = m["par"]
tau, theta, phiN, psi, phiF, phiE, c = par[2], par[3], par[4], par[5], par[6], par[7], par[8]
e = np.full(N, 0.10)
l = np.full(N, 0.6)
iw = np.full(N, len(m["wg"]) // 2)
alive = np.ones(N, dtype=np.bool_)
b = 0
out = np.zeros((N, 6))
args = (V, pol, m["eg"], m["lg"], m["wg"], m["Pw"], m["Pb"], m["gn"], m["gw"], m["rD"], m["rL"], m["rS"], par,
        m["qx"], m["qn"], m["qf"], m["nmax"], m["fmax"], mu_g, sd_g)
print(f"top of the equity grid {m['eg'][-1]}, of the loan grid {m['lg'][-1]}")


def summary(t, replaced, mean_e, mean_l, label):
    fin = np.isfinite(e)
    print(f"quarter {t} {label}: e not finite {int((~fin).sum())}, l not finite {int((~np.isfinite(l)).sum())}, "
          f"max finite e {e[fin].max():.4g}, e above grid {int((e[fin] > m['eg'][-1]).sum())}, "
          f"l above grid {int((l[np.isfinite(l)] > m['lg'][-1]).sum())}, replaced {replaced}, "
          f"replacement mean e {mean_e:.4g} l {mean_l:.4g}")


def report(t, idx, replaced_mask, pe, pl, piw, b, b2, u_w, z_g, label):
    print(f"FIRST non-finite state at quarter {t}, {label}: {len(idx)} banks, {int(replaced_mask[idx].sum())} of them replaced this quarter")
    for i in idx[:10]:
        x, n, f, v = stage2.choose(pe[i], pl[i], int(piw[i]), b, V, pol, m["eg"], m["lg"], m["wg"], m["Pw"], m["Pb"], m["gn"],
                                   m["gw"], m["rD"], m["rL"], m["rS"], par, m["qx"], m["qn"], m["qf"], m["nmax"], m["fmax"])
        eq = pe[i] - x
        s = 1.0 + f + pe[i] - x - pl[i] - n
        cum = np.cumsum(m["Pw"][b2, int(piw[i])])
        j = 0
        while j < cum.shape[0] - 1 and u_w[i] > cum[j]:
            j += 1
        w2 = m["wg"][j]
        gN = phiN * n * n if n > 0 else psi * phiN * n * n
        pi = (m["rL"][b2] - w2) * (pl[i] + n) + m["rS"][b2] * s - m["rD"][b2] - (m["rD"][b2] * f + phiF * f * f - phiE * eq * eq) - gN - c
        after = (1.0 - tau) * pi if pi > 0 else pi
        G = math.exp(mu_g[b2] + sd_g[b2] * z_g[i])
        print(f"   bank {i}: replaced {bool(replaced_mask[i])}; start e {pe[i]:.6g} l {pl[i]:.6g} w node {int(piw[i])}; "
              f"policy x {x:.6g} n {n:.6g} f {f:.6g} value {v:.6g}; eq {eq:.6g} s {s:.6g} w' {w2:.4g} pi {pi:.6g} "
              f"after {after:.6g} Gamma {G:.6g}; now e {e[i]:.6g} l {l[i]:.6g}")


for t in range(T):
    b2 = int(rng.random() < PB[b, 1])
    u_w = rng.random(N)
    z_g = rng.standard_normal(N)
    dead = ~alive
    replaced = int(dead.sum())
    mean_e = float(e[alive].mean()) if replaced else float("nan")
    mean_l = float(l[alive].mean()) if replaced else float("nan")
    if replaced:
        e[dead] = e[alive].mean()
        l[dead] = l[alive].mean()
        iw[dead] = 0
        alive[dead] = True
    if t % 100 == 0:
        summary(t, replaced, mean_e, mean_l, "start")
    bad = np.where(~np.isfinite(e) | ~np.isfinite(l))[0]
    if len(bad):
        report(t, bad, dead, e.copy(), l.copy(), iw.copy(), b, b2, u_w, z_g, "after replacement, before the step")
        break
    pe, pl, piw = e.copy(), l.copy(), iw.copy()
    stage2.step(e, l, iw, alive, b, b2, u_w, z_g, *args, out)
    bad = np.where((~np.isfinite(e) | ~np.isfinite(l)) & alive)[0]
    if len(bad):
        report(t, bad, dead, pe, pl, piw, b, b2, u_w, z_g, "after the step")
        break
    b = b2
else:
    print("no non-finite state in any of the 2,000 quarters")
