import csv
import gzip
import hashlib
import math
import os
import statistics
from collections import defaultdict
from functools import partial
from multiprocessing import Pool

import numpy as np

import predict_pk
from judged_risk import DT, PATHS, RHO, SEED, STEPS_Q
from sample import ANNUAL, RATES, R_REF, ROOT, TEXT, load_panel, load_rates, num, samples, steady
from stage1_barriers import u0
from stage2 import DELTA, solve
from window import FAR, THIN, observed, ratio

HOLDOUT = os.path.join(ROOT, "common_data", "holdout", "fdic_q_2025.csv.gz")
QUARTERLY = [os.path.join(ROOT, "common_data", f"fdic_q_{y}.csv.gz") for y in range(2014, 2025)] + [HOLDOUT]
FLOOR = 0.105
NQ = 4
Y0, Y1 = 2014, 2024
START = "20241231"
QS = ["20250331", "20250630", "20250930", "20251231"]
MIN_CHANGES = 8
HOLD_TAG = 2025


def load(paths):
    panel = defaultdict(dict)
    for path in paths:
        with gzip.open(path, "rt", newline="") as fh:
            for r in csv.DictReader(fh):
                panel[r["CERT"]][r["REPDTE"]] = {k: num(v) for k, v in r.items() if k not in TEXT}
    return panel


def inputs_recent(cert, s, rates):
    s = {y: r for y, r in s.items() if Y0 <= y <= Y1}
    if not s or any(rec["BKCLASS"] != "N" for rec in s.values()):
        return None
    R, X = {}, {}
    for y, rec in s.items():
        if rec["RWAJT"] and rec["RBCT1J"] is not None and rec["RBCT2"] is not None:
            R[y] = rec["RWAJT"]
            X[y] = (rec["RBCT1J"] + rec["RBCT2"]) / R[y]
    roa = [s[y]["NETINC"] / R[y - 1] - rates[y] * X[y - 1] for y in range(Y0 + 1, Y1 + 1)
           if y in s and y - 1 in R and s[y]["NETINC"] is not None and steady(s, y)]
    if len(roa) < 8:
        return None
    mu, sigma = statistics.fmean(roa), statistics.stdev(roa)
    if mu <= 0:
        return None
    return {"mu": mu, "sigma": sigma}


def sigma_d_recent(q, cert):
    s = q.get(cert, {})
    changes = []
    for y in range(Y0 + 1, Y1 + 1):
        a, b = f"{y - 1}1231", f"{y}1231"
        ra, rb = ratio(q, cert, a), ratio(q, cert, b)
        if ra is None or rb is None or not 0.5 <= s[b]["ASSET"] / s[a]["ASSET"] <= 2:
            continue
        changes.append(rb - ra)
    return statistics.stdev(changes) if len(changes) >= MIN_CHANGES else None


def frozen_sim(b):
    s = b["sD"]
    rng = np.random.default_rng([SEED, int(b["cert"]), HOLD_TAG, int(round(s * 1e7))])
    mu, sig = b["mu"], b["sigma"]
    top = u0(mu, s, RHO)
    x = np.full(PATHS, b["x0"] - FLOOR)
    alive = x > 0
    x = np.minimum(x, top)
    f1 = f2 = n1 = n2 = 0
    pre_b = x + FLOOR - R_REF
    sq = math.sqrt(DT)
    for _ in range(NQ):
        divq = np.zeros(PATHS, dtype=bool)
        for _ in range(STEPS_Q):
            x = x + mu * DT + s * sq * rng.standard_normal(PATHS)
            alive &= ~(alive & (x <= 0))
            held = alive & (x >= top)
            divq |= held
            x = np.where(held, top, x)
        now_b = x + FLOOR - R_REF
        f2 += int(np.sum(alive & (now_b > FAR * sig)))
        n2 += int(np.sum(alive))
        d = alive & divq
        f1 += int(np.sum(d & (pre_b < THIN * sig)))
        n1 += int(np.sum(d))
        pre_b = now_b
    return f1, n1, f2, n2, bool(b["x0"] - FLOOR <= 0)


def pk_sim(b):
    rng = np.random.default_rng([predict_pk.SEED, int(b["cert"])])
    mu, sig = b["mu"], b["sigma"]
    issuer = b["u2"] is not None
    top = b["u2"] if issuer else b["u0"]
    x = np.full(PATHS, b["x0"] - 0.08)
    alive = x > 0
    x = np.minimum(x, top)
    wait = np.full(PATHS, -1.0)
    f1 = f2 = n1 = n2 = 0
    pre_b = x + 0.08 - R_REF
    sq = math.sqrt(DT)
    for _ in range(NQ):
        divq = np.zeros(PATHS, dtype=bool)
        for _ in range(STEPS_Q):
            x = x + mu * DT + sig * sq * rng.standard_normal(PATHS)
            alive &= ~(alive & (x <= 0))
            held = alive & (x >= top)
            divq |= held
            x = np.where(held, top, x)
            if issuer:
                start = alive & (wait < 0) & (x <= b["u1"])
                wait = np.where(start, DELTA, wait)
                ticking = alive & (wait >= 0)
                wait = np.where(ticking, wait - DT, wait)
                land = alive & ticking & (wait <= 1e-12)
                x = np.where(land, b["u2"], x)
                wait = np.where(land, -1.0, wait)
        now_b = x + 0.08 - R_REF
        f2 += int(np.sum(alive & (now_b > FAR * sig)))
        n2 += int(np.sum(alive))
        d = alive & divq
        f1 += int(np.sum(d & (pre_b < THIN * sig)))
        n1 += int(np.sum(d))
        pre_b = now_b
    return f1, n1, f2, n2


def show(label, res):
    print(label)
    for k, (hit, n) in res.items():
        print(f"  {k}: {hit}/{n} = {100 * hit / n:.1f}%" if n else f"  {k}: 0/0 = n/a")


def main():
    for p in [ANNUAL, RATES] + QUARTERLY:
        print(f"{p.split('/')[-1]} sha256 {hashlib.sha256(open(p, 'rb').read()).hexdigest()}")
    annual = load_panel(ANNUAL)
    rates = load_rates(RATES)
    q = load(QUARTERLY)
    samp, _ = samples()
    with Pool() as pool:
        for name in ("B", "A"):
            banks, dropped = [], defaultdict(int)
            for b0 in samp[name]:
                cert = b0["cert"]
                x0 = ratio(q, cert, START)
                if x0 is None:
                    dropped["no 2024Q4 buffer"] += 1
                    continue
                par = inputs_recent(cert, annual.get(cert, {}), rates)
                if par is None:
                    dropped["no 2015-2024 parameters"] += 1
                    continue
                sd = sigma_d_recent(q, cert)
                if sd is None:
                    dropped["fewer than 8 yearly changes"] += 1
                    continue
                rec = q[cert][START]
                banks.append({"cert": cert, "mu": par["mu"], "sigma": par["sigma"], "sD": sd, "x0": x0,
                              "asset_rwa": rec["ASSET"] / rec["RWAJT"]})
            print(f"\n===== sample {name}: {len(banks)} of {len(samp[name])} banks; left out: {dict(dropped)}")
            print(f"  medians, 2015-2024: sigma_i {100 * statistics.median(b['sigma'] for b in banks):.2f} points, "
                  f"sigma_D {100 * statistics.median(b['sD'] for b in banks):.2f} points, 2024Q4 buffer over R "
                  f"{100 * statistics.median(b['x0'] - R_REF for b in banks):.2f} points")
            obs_raw = observed(banks, q, QS, R_REF)
            obs_raw = {k: obs_raw[k] for k in ("F1", "F2")}
            show("observed, 2025", obs_raw)
            obs = {k: 100 * h / n for k, (h, n) in obs_raw.items()}
            se = {k: 100 * math.sqrt((h / n) * (1 - h / n) / n) for k, (h, n) in obs_raw.items()}
            print(f"  standard errors of the observed shares: F1 {se['F1']:.1f}, F2 {se['F2']:.1f} points")
            out = pool.map(frozen_sim, banks)
            res = {"F1": (sum(o[0] for o in out), sum(o[1] for o in out)), "F2": (sum(o[2] for o in out), sum(o[3] for o in out))}
            show(f"(P) frozen model, floor 10.5%: {sum(o[4] for o in out)} start at or below the floor", res)
            bout = pool.map(pk_sim, pool.map(solve, banks))
            base = {"F1": (sum(o[0] for o in bout), sum(o[1] for o in bout)), "F2": (sum(o[2] for o in bout), sum(o[3] for o in bout))}
            show("baseline (Peura and Keppo), minimum 8%", base)
            misses = {}
            for label, r in (("(P)", res), ("baseline", base)):
                p = {k: 100 * h / n for k, (h, n) in r.items() if n}
                d1, d2 = abs(p["F1"] - obs["F1"]), abs(p["F2"] - obs["F2"])
                misses[label] = d1 + d2
                print(f"  {label}: |dF1| {d1:.1f} + |dF2| {d2:.1f} = {d1 + d2:.1f} points; F1 within 10: {d1 <= 10}; F2 within 10: {d2 <= 10}")
            if name == "B":
                print(f"  rule (sample B): (P) {'beats' if misses['(P)'] < misses['baseline'] else 'does not beat'} the baseline "
                      f"({misses['(P)']:.1f} against {misses['baseline']:.1f} points)")



if __name__ == "__main__":
    main()
