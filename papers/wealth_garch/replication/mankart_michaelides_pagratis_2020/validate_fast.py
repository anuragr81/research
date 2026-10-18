import ast
import re
import sys
import time

import numpy as np

from mmp_config import TABLE4, cake, model
from mmp_fast import solve_fast
from mmp_sim import simulate_counts
from stage2 import THEIRS, moments


def log(s):
    print(s, file=sys.stderr, flush=True)


def timed(label, m, **kw):
    t0 = time.time()
    r = solve_fast(m, rel=1e-6, howard=50, max_outer=400, log=log, **kw)
    log(f"{label}: {r[2]} sweeps ({r[6]} full), {time.time() - t0:.0f}s")
    print(f"{label}: {'converged' if r[4] else 'NOT converged'} after {r[2]} sweeps ({r[6]} full), "
          f"last change {r[3]:.1e}, largest fall {r[5]:.1e}")
    return r


def close(a, b, name):
    if name == "failure rate":
        return abs(a - b) <= 0.005
    return abs(a - b) <= 0.01 * abs(b)


def compare(mine, ref, label):
    ok = True
    for name in THEIRS["small"]:
        good = close(mine[name], ref[name], name)
        ok &= good
        print(f"  {label} {name:20s} {mine[name]:.6f} against {ref[name]:.6f} -> {'agrees' if good else 'differs'}")
    return ok


def f1():
    ok_all = True
    for size in ("large", "small"):
        gamma, beta, r = TABLE4[size]["gamma"], TABLE4[size]["beta"], 0.01
        kappa = 1 - beta ** (1 / gamma) * (1 + r) ** ((1 - gamma) / gamma)
        m = cake(gamma, beta, r)
        m["eg"] = np.geomspace(1e-6, 0.4, 1281)
        m["qx"] = np.linspace(0.001, 0.2, 400)
        V, pol, *_ = timed(f"F1 {size} cake, 1281 points", m)
        inner = (m["eg"] >= 0.05) & (m["eg"] <= 0.3)
        err = float(np.max(np.abs(pol[inner, 0, 0, 0, 0] / (kappa * m["eg"][inner]) - 1)))
        ok = err < 0.01
        ok_all &= ok
        print(f"F1 {size}: policy error {err:.2e} -> {'passes' if ok else 'fails'}")
    return ok_all


def f2():
    m = model("large", ne=41, nl=41)
    ref = np.load("solved_large_41.npz")["V"]
    V, *_ = timed("F2 large 41 cold", m)
    gap = float(np.max(np.abs(V - ref)) / (ref.max() - m["par"][13]))
    ok = gap <= 1e-4
    print(f"F2: largest value difference {gap:.2e} of the value range -> {'passes' if ok else 'fails'}; "
          f"cells where the changed solver is higher {float(np.mean(V > ref + 1e-12)):.3f}, lower {float(np.mean(V < ref - 1e-12)):.3f}")
    return ok


def stage2_small_41():
    text = open("results_stage2.md").read()
    line = re.search(r"^small 41x41: converged.*moments (\{.*\})$", text, re.M).group(1)
    return ast.literal_eval(re.sub(r"np\.float64\(([^)]*)\)", r"\1", line))


def small_moments(m, V, pol):
    rec, counts, emax = simulate_counts(m, V, pol, "small")
    return moments(rec), counts["all"]["not finite"]


def perturbed():
    m = model("small", ne=41, nl=41)
    par = m["par"].copy()
    beta, gamma = par[0], par[1] * 1.02
    for i in (4, 6, 7, 8):
        par[i] *= 1.05
    cD = TABLE4["small"]["cD"] * 1.05
    par[1] = gamma
    par[13] = cD ** (1 - gamma) / (1 - gamma) / (1 - beta)
    m["par"] = par
    return m


def main():
    a = f1()
    b = f2()
    m = model("small", ne=41, nl=41)
    V, pol, *_ = timed("F3 small 41 cold", m)
    mom, bad = small_moments(m, V, pol)
    c = compare(mom, stage2_small_41(), "F3") and bad == 0
    print(f"F3: {bad} non-finite bank-quarters -> {'passes' if c else 'fails'}")
    mp = perturbed()
    Vw, pw, *_ = timed("F4 perturbed, warm", mp, start=(V, pol))
    Vc, pc, *_ = timed("F4 perturbed, cold", mp)
    gap = float(np.max(np.abs(Vw - Vc)) / (Vc.max() - mp["par"][13]))
    mw, _ = small_moments(mp, Vw, pw)
    mc, _ = small_moments(mp, Vc, pc)
    d = compare(mw, mc, "F4") and gap <= 1e-4
    print(f"F4: largest value difference {gap:.2e} of the value range -> {'passes' if d else 'fails'}")
    print(f"F1 {'passes' if a else 'fails'}; F2 {'passes' if b else 'fails'}; F3 {'passes' if c else 'fails'}; "
          f"F4 {'passes' if d else 'fails'}")


if __name__ == "__main__":
    main()
