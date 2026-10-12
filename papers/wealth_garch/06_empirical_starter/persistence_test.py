import csv
import gzip
import hashlib
import math
import sys
from collections import defaultdict

import numpy as np

QE = ("0331", "0630", "0930", "1231")
MIN_ASSET = 100000
PRIMARY = 0.085
SECONDARY = 0.06
DRAWS = 2000
SEED = 20261012
MIN_PAIRS = 100
MIN_BANKS = 30
HALF_WIDTH = 0.05


def num(v):
    return None if v is None or v == "" else float(v)


def shift(d, k):
    y, q = int(d[:4]), QE.index(d[4:])
    t = y * 4 + q + k
    return f"{t // 4}{QE[t % 4]}"


def load(path):
    panel = defaultdict(dict)
    with gzip.open(path, "rt", newline="") as fh:
        for r in csv.DictReader(fh):
            panel[r["CERT"]][r["REPDTE"]] = {k: num(v) for k, v in r.items() if k not in ("CERT", "REPDTE")}
    return panel


def qflow(s, d, field):
    rec = s.get(d)
    if rec is None or rec[field] is None:
        return None
    if d[4:] == "0331":
        return rec[field]
    prev = s.get(shift(d, -1))
    if prev is None or prev[field] is None:
        return None
    return rec[field] - prev[field]


def pairs(panel):
    out = []
    for cert, s in panel.items():
        for d, rec in s.items():
            pd = shift(d, -1)
            prev = s.get(pd)
            if prev is None:
                continue
            if any(v is None for v in (rec["ASSET"], prev["ASSET"], rec["RBCT1J"], prev["RBCT1J"],
                                       rec["RWAJT"], prev["RWAJT"])):
                continue
            if rec["ASSET"] < MIN_ASSET or prev["ASSET"] < MIN_ASSET:
                continue
            if min(rec["RBCT1J"], prev["RBCT1J"], rec["RWAJT"], prev["RWAJT"]) <= 0:
                continue
            if qflow(s, d, "EQCMRG") != 0:
                continue
            ni = [qflow(s, shift(pd, -k), "NETINC") for k in range(4)]
            dv = [qflow(s, shift(pd, -k), "EQCDIV") for k in range(4)]
            window = None if any(v is None for v in ni + dv) else (sum(ni), sum(dv))
            out.append({"bank": cert, "e0": prev["RBCT1J"], "e1": rec["RBCT1J"], "r0": prev["RWAJT"],
                        "r1": rec["RWAJT"], "window": window, "div": qflow(s, d, "EQCDIV")})
    return out


def samples(prs, req):
    deficit, surplus = [], []
    for p in prs:
        z0 = math.log(p["e0"] / (req * p["r0"]))
        z1 = math.log(p["e1"] / (req * p["r1"]))
        if z0 < 0 and p["window"] is not None and p["window"][0] > 0:
            ni, dv = p["window"]
            b = 1 - dv / ni
            if 0 < b <= 1:
                deficit.append((p["bank"], z0, z1, p["r1"] / p["r0"] - 1, ni, dv))
        elif z0 > 0 and p["div"] is not None and 0 <= p["div"] < p["e0"]:
            surplus.append((p["bank"], z0, z1, math.log(p["e0"] / (p["e0"] - p["div"]))))
    return deficit, surplus


def banks_of(rows):
    ids = sorted({r[0] for r in rows})
    index = {b: i for i, b in enumerate(ids)}
    return np.array([index[r[0]] for r in rows]), len(ids)


def t1_stat(a):
    z0, z1, g, ni, dv = a
    phi = (z0 * z1).sum() / (z0 * z0).sum()
    gbar, bbar = g.mean(), (ni - dv).sum() / ni.sum()
    return phi - (1 - gbar * (1 - bbar) / bbar), phi, 1 - gbar * (1 - bbar) / bbar


def t2_stat(a):
    z0, z1, g = a[0], a[1], a[2]
    lo = g <= np.median(g)
    phi = lambda m: (z0[m] * z1[m]).sum() / (z0[m] * z0[m]).sum()
    return phi(lo) - phi(~lo), phi(lo), phi(~lo)


def t3_stat(a):
    z0, z1, lp = a
    phi = (z0 * z1).sum() / (z0 * z0).sum()
    delta = lp.sum() / z0.sum()
    return phi - (1 - delta), phi, 1 - delta


def bootstrap(rows, cols, stat, rng):
    bank, nb = banks_of(rows)
    arrays = [np.array([r[c] for r in rows], dtype=float) for c in cols]
    members = [np.flatnonzero(bank == i) for i in range(nb)]
    point = stat(arrays)
    draws = []
    for _ in range(DRAWS):
        pick = np.concatenate([members[i] for i in rng.integers(0, nb, nb)])
        draws.append(stat([a[pick] for a in arrays])[0])
    lo, hi = np.percentile(draws, [2.5, 97.5])
    return point, lo, hi, len(rows), nb


def level_verdict(lo, hi):
    if lo > 0 or hi < 0:
        return "fails"
    return "holds" if (hi - lo) / 2 <= HALF_WIDTH else "not measured"


def sign_verdict(lo, hi):
    if lo > 0:
        return "holds"
    return "fails" if hi < 0 else "not measured"


def report(name, res, verdict_fn, labels, use_verdict):
    point, lo, hi, n, nb = res
    enough = n >= MIN_PAIRS and nb >= MIN_BANKS
    v = verdict_fn(lo, hi) if enough else "not measured (too few pairs or banks)"
    print(f"{name}: {n} pairs, {nb} banks")
    print(f"    {labels[0]} {point[1]:+.4f}   {labels[1]} {point[2]:+.4f}")
    print(f"    difference {point[0]:+.4f}, 95% interval [{lo:+.4f}, {hi:+.4f}]")
    print(f"    verdict: {v if use_verdict else 'secondary, no verdict'}")


def main(path):
    digest = hashlib.sha256(open(path, "rb").read()).hexdigest()
    prs = pairs(load(path))
    print(f"panel sha256 {digest}")
    print(f"pairs passing the filters: {len(prs)}")
    rng = np.random.default_rng(SEED)
    for req, primary in ((PRIMARY, True), (SECONDARY, False)):
        deficit, surplus = samples(prs, req)
        print()
        print(f"=== requirement {req} x RWAJT ({'primary' if primary else 'secondary'})")
        n_def = sum(1 for p in prs if p["e0"] < req * p["r0"])
        print(f"pairs starting in deficit: {n_def} of {len(prs)}; with 0 < b <= 1: {len(deficit)}")
        if deficit:
            g = np.array([r[3] for r in deficit])
            ni, dv = sum(r[4] for r in deficit), sum(r[5] for r in deficit)
            print(f"deficit pairs: mean g {g.mean():+.4f}, retention b {(ni - dv) / ni:.4f}")
        lp, z0 = sum(r[3] for r in surplus), sum(r[1] for r in surplus)
        print(f"surplus pairs: payout rate delta_p {lp / z0:.4f} per quarter")
        if deficit:
            report("T1 deficit level", bootstrap(deficit, (1, 2, 3, 4, 5), t1_stat, rng), level_verdict,
                   ("phi- estimated", "phi- predicted"), primary)
            report("T2 deficit falls in g", bootstrap(deficit, (1, 2, 3), t2_stat, rng), sign_verdict,
                   ("phi- low g", "phi- high g"), primary)
        report("T3 surplus level", bootstrap(surplus, (1, 2, 3), t3_stat, rng), level_verdict,
               ("phi+ estimated", "phi+ predicted"), primary)


if __name__ == "__main__":
    main(sys.argv[1])
