import numpy as np

from mmp_a1 import solve_a1
from mmp_config import TABLE4, cake
from validate_stage2 import v2


def v1():
    ok_all = True
    for size in ("large", "small"):
        gamma, beta, r = TABLE4[size]["gamma"], TABLE4[size]["beta"], 0.01
        kappa = 1 - beta ** (1 / gamma) * (1 + r) ** ((1 - gamma) / gamma)
        for npts in (161, 321, 641, 1281):
            m = cake(gamma, beta, r)
            m["eg"] = np.geomspace(1e-6, 0.4, npts)
            m["qx"] = np.linspace(0.001, 0.2, 400)
            V, pol, outer, diff, ok, fall = solve_a1(m, rel=1e-6, howard=200, max_outer=400, log=lambda s: None)
            eg = m["eg"]
            inner = (eg >= 0.05) & (eg <= 0.3)
            err = float(np.max(np.abs(pol[inner, 0, 0, 0, 0] / (kappa * eg[inner]) - 1)))
            verr = float(np.max(np.abs(V[inner, 0, 0, 0] / (kappa ** (-gamma) * eg[inner] ** (1 - gamma) / (1 - gamma)) - 1)))
            print(f"V1 {size} (gamma {gamma}, beta {beta}, kappa {kappa:.5f}), {npts} geometric equity points: "
                  f"policy error {err:.2e}, value error {verr:.2e}, {outer} sweeps, "
                  f"{'converged' if ok else 'not converged'}, last change {diff:.1e}, largest fall {fall:.1e}")
        passed = err < 0.01
        ok_all &= passed
        print(f"V1 {size} at the finest grid -> {'passes' if passed else 'fails'}")
    return ok_all


if __name__ == "__main__":
    a, b = v1(), v2()
    print(f"V1 {'passes' if a else 'fails'}; V2 {'passes' if b else 'fails'}")
