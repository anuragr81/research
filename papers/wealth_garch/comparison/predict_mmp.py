import hashlib
import importlib.util
import os
import sys

import numpy as np

from sample import ROOT, R_REF, QUARTERS, samples
from window import FAR, THIN, load_years, observed, show

MMP = os.path.join(ROOT, "replication", "mankart_michaelides_pagratis_2020")
sys.path.insert(0, MMP)
SOLUTION = os.path.join(MMP, "solved_fast_small_41.npz")


def small_class(years):
    q = load_years(years, keep_class=True)
    by_d = {}
    for cert, s in q.items():
        for d, rec in s.items():
            if rec["BKCLASS"] in ("N", "NM", "SM") and rec["ASSET"] is not None:
                by_d.setdefault(d, []).append(rec["ASSET"])
    cut = {d: np.percentile(v, 95) for d, v in by_d.items()}
    return lambda cert, d: d in cut and q.get(cert, {}).get(d, {}).get("ASSET") is not None and q[cert][d]["ASSET"] < cut[d]


def mmp_stage2():
    spec = importlib.util.spec_from_file_location("mmp_stage2", os.path.join(MMP, "stage2.py"))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def model_facts():
    stage2 = mmp_stage2()
    from mmp_config import model
    data = np.load(SOLUTION)
    m = model("small", ne=41, nl=41)
    rec = stage2.simulate(m, data["V"], data["pol"], "small")
    assets = 1.0 / rec[:, :, 1]
    loans = rec[:, :, 0] * assets
    eq = rec[:, :, 2] * assets
    rwa = loans + 0.2 * (assets - loans)
    b = eq / rwa - R_REF
    pi = rec[:, :, 3] * eq
    x = rec[:, :, 4] * eq
    live = (rec[:, :, 5] == 0) & np.isfinite(b)
    T, N = b.shape
    yrs = T // 4
    roa = (pi / rwa)[: 4 * yrs].reshape(yrs, 4, N)
    full = live[: 4 * yrs].reshape(yrs, 4, N).all(axis=1)
    annual = np.where(full, roa.sum(axis=1), np.nan)
    count = full.sum(axis=0)
    sigma = np.nanstd(annual, axis=0, ddof=1)
    use = count >= 8
    f2 = live & use & (b > FAR * sigma)
    n2 = live & use
    d1 = live[1:] & live[:-1] & use & (x[1:] > 0)
    f1 = d1 & (b[:-1] < THIN * sigma)
    return {"F1": (int(f1.sum()), int(d1.sum())), "F2": (int(f2.sum()), int(n2.sum())),
            "F3": (0, 0), "F4": (0, 0)}, int((~use).sum()), N


def main():
    print(f"solved_fast_small_41.npz sha256 {hashlib.sha256(open(SOLUTION, 'rb').read()).hexdigest()}")
    samp, q = samples()
    keep = small_class(range(2014, 2025))
    obs = observed(samp["A"], q, QUARTERS, R_REF, keep)
    print(f"\nobserved, sample A small bank-quarters, 2015-2024")
    show(obs)
    pred, dropped, n = model_facts()
    print(f"\nMMP small banks, simulated ({n} banks, last 500 quarters; {dropped} with fewer than 8 complete years)")
    show(pred)
    print("\nverdicts (agree within 10 points; F3, F4: no episodes predicted)")
    for k in ("F1", "F2"):
        (ph, pn), (oh, on) = pred[k], obs[k]
        ps, os_ = 100 * ph / pn, 100 * oh / on
        v = "not measured" if on < 20 else ("agrees" if abs(ps - os_) <= 10 else "disagrees")
        print(f"  {k}: {ps:.1f}% vs {os_:.1f}% -> {v}")
    for k in ("F3", "F4"):
        on = obs[k][1]
        print(f"  {k}: no episodes vs {on} observed -> {'not measured' if on < 20 else 'fails'}")


if __name__ == "__main__":
    main()
