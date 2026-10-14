import numpy as np

from mmp_config import PB, TABLE2, TABLE4, cake, model
from mmp_model import solve


def v1():
    ok_all = True
    for size in ("large", "small"):
        gamma, beta, r = TABLE4[size]["gamma"], TABLE4[size]["beta"], 0.01
        kappa = 1 - beta ** (1 / gamma) * (1 + r) ** ((1 - gamma) / gamma)
        for npts in (161, 321, 641, 1281):
            m = cake(gamma, beta, r)
            m["eg"] = np.geomspace(1e-6, 0.4, npts)
            m["qx"] = np.linspace(0.001, 0.2, 400)
            V, pol, outer, diff = solve(m, tol=1e-8, howard=200, max_outer=400, log=lambda s: None)
            eg = m["eg"]
            inner = (eg >= 0.05) & (eg <= 0.3)
            err = float(np.max(np.abs(pol[inner, 0, 0, 0, 0] / (kappa * eg[inner]) - 1)))
            verr = float(np.max(np.abs(V[inner, 0, 0, 0] / (kappa ** (-gamma) * eg[inner] ** (1 - gamma) / (1 - gamma)) - 1)))
            print(f"V1 {size} (gamma {gamma}, beta {beta}, kappa {kappa:.5f}), {npts} geometric equity points: "
                  f"policy error {err:.2e}, value error {verr:.2e}, {outer} sweeps, last change {diff:.1e}")
        ok = err < 0.01
        ok_all &= ok
        print(f"V1 {size} at the finest grid -> {'passes' if ok else 'fails'}")
    return ok_all


def v2():
    rng = np.random.default_rng(20261014)
    ok_all = True
    for size in ("large", "small"):
        m = model(size)
        wg, Pw, t2 = m["wg"], m["Pw"], TABLE2[size]
        T = 400_000
        b = np.zeros(T, dtype=int)
        iw = np.zeros(T, dtype=int)
        iw[0] = len(wg) // 2
        for t in range(1, T):
            b[t] = rng.random() < PB[b[t - 1], 1]
            iw[t] = min(np.searchsorted(np.cumsum(Pw[b[t], iw[t - 1]]), rng.random()), len(wg) - 1)
        w = wg[iw] * 100
        for state, name in ((0, "expansion"), (1, "recession")):
            sel = b == state
            pairs = sel[1:] & sel[:-1]
            stats = (w[sel].mean(), w[sel].std(), np.corrcoef(w[:-1][pairs], w[1:][pairs])[0, 1])
            target = (t2["w_mean"][state], t2["w_sd"][state], t2["w_ar"][state])
            oks = [abs(s - g) <= 0.10 * abs(g) for s, g in zip(stats, target)]
            ok_all &= all(oks)
            print(f"V2 {size} {name} write-offs: mean {stats[0]:.3f} [{target[0]}], s.d. {stats[1]:.3f} [{target[1]}], "
                  f"AR(1) {stats[2]:.3f} [{target[2]}] -> {'passes' if all(oks) else 'fails'}")
        for state, name in ((0, "expansion"), (1, "recession")):
            logs = np.log(m["gn"][state])
            mean = float(np.sum(m["gw"] * logs)) * 100
            sd = float(np.sqrt(np.sum(m["gw"] * (logs - mean / 100) ** 2))) * 100
            ok = abs(mean - t2["g_mean"][state]) < 1e-9 and abs(sd - t2["g_sd"][state]) < 1e-9
            ok_all &= ok
            print(f"V2 {size} {name} deposit growth quadrature: mean {mean:.4f} [{t2['g_mean'][state]}], "
                  f"s.d. {sd:.4f} [{t2['g_sd'][state]}] -> {'passes' if ok else 'fails'}")
    return ok_all


if __name__ == "__main__":
    a, b = v1(), v2()
    print(f"V1 {'passes' if a else 'fails'}; V2 {'passes' if b else 'fails'}")
