import csv
import gzip
import math
import os
from collections import defaultdict

import numpy as np

from sample import MIN_ASSET, ROOT, TEXT, num, qflow, shift

THIN, FAR = 1.0, 4.0


def load_years(years, keep_class=False):
    panel = defaultdict(dict)
    for y in years:
        with gzip.open(os.path.join(ROOT, "common_data", f"fdic_q_{y}.csv.gz"), "rt", newline="") as fh:
            for r in csv.DictReader(fh):
                rec = {k: num(v) for k, v in r.items() if k not in TEXT}
                if keep_class:
                    rec["BKCLASS"] = r["BKCLASS"]
                panel[r["CERT"]][r["REPDTE"]] = rec
    return panel


def quarters(first, last):
    return [f"{y}{md}" for y in range(first, last + 1) for md in ("0331", "0630", "0930", "1231")]


def ratio(q, cert, d):
    rec = q.get(cert, {}).get(d)
    if rec is None or rec["ASSET"] is None or rec["ASSET"] < MIN_ASSET:
        return None
    if rec["RBCT1J"] is None or rec["RBCT2"] is None or not rec["RWAJT"] or rec["RWAJT"] <= 0:
        return None
    return (rec["RBCT1J"] + rec["RBCT2"]) / rec["RWAJT"]


def observed(banks, q, qs, r_ref, keep=lambda cert, d: True):
    f1, f2, eps = [], [], []
    for b in banks:
        s, sig = q.get(b["cert"], {}), b["sigma"]
        for d in qs:
            if not keep(b["cert"], d):
                continue
            r_now, r_pre = ratio(q, b["cert"], d), ratio(q, b["cert"], shift(d, -1))
            now = None if r_now is None else r_now - r_ref
            pre = None if r_pre is None else r_pre - r_ref
            if now is not None:
                f2.append(now > FAR * sig)
            div = qflow(s, d, "EQCDIV")
            if pre is not None and now is not None and div is not None and div > 0:
                f1.append(pre < THIN * sig)
            if pre is not None and now is not None:
                stock, parent, merger = s[d]["EQCSXQ"], qflow(s, d, "EQCBHCTR"), qflow(s, d, "EQCMRG")
                if stock is not None and parent is not None and merger is not None and merger == 0 \
                        and stock + parent >= 0.005 * s[d]["ASSET"]:
                    eps.append((pre < THIN * sig, THIN * sig <= now <= FAR * sig))
    return {"F1": (sum(f1), len(f1)), "F2": (sum(f2), len(f2)),
            "F3": (sum(e[0] for e in eps), len(eps)), "F4": (sum(e[1] for e in eps), len(eps))}


def barrier_paths(b, minimum, r_ref, x0, asset_rwa, delta, seed, paths=200, years=10, dt=1 / 360, steps_q=90):
    rng = np.random.default_rng([seed, int(b["cert"])])
    mu, sig = b["mu"], b["sigma"]
    issuer = b["u2"] is not None
    top = b["u2"] if issuer else b["u0"]
    min_issue = 0.005 * asset_rwa
    x = np.full(paths, x0 - minimum)
    alive = x > 0
    x = np.minimum(x, top)
    wait = np.full(paths, -1.0)
    f1 = f2 = n1 = n2 = 0
    eps = []
    pre_b = x + minimum - r_ref
    sq = math.sqrt(dt)
    for _ in range(4 * years):
        divq = np.zeros(paths, dtype=bool)
        arrived = np.zeros(paths, dtype=bool)
        for _ in range(steps_q):
            x = x + mu * dt + sig * sq * rng.standard_normal(paths)
            alive &= ~(alive & (x <= 0))
            held = alive & (x >= top)
            divq |= held
            x = np.where(held, top, x)
            if issuer:
                start = alive & (wait < 0) & (x <= b["u1"])
                wait = np.where(start, delta, wait)
                ticking = alive & (wait >= 0)
                wait = np.where(ticking, wait - dt, wait)
                land = alive & ticking & (wait <= 1e-12)
                arrived |= land & (b["u2"] - x >= min_issue)
                x = np.where(land, b["u2"], x)
                wait = np.where(land, -1.0, wait)
        now_b = x + minimum - r_ref
        f2 += int(np.sum(alive & (now_b > FAR * sig)))
        n2 += int(np.sum(alive))
        d = alive & divq
        f1 += int(np.sum(d & (pre_b < THIN * sig)))
        n1 += int(np.sum(d))
        for i in np.where(alive & arrived)[0]:
            eps.append((bool(pre_b[i] < THIN * sig), bool(THIN * sig <= now_b[i] <= FAR * sig)))
        pre_b = now_b
    return f1, n1, f2, n2, eps, bool(x0 - minimum <= 0)


def pooled(out):
    eps = [e for o in out for e in o[4]]
    return {"F1": (sum(o[0] for o in out), sum(o[1] for o in out)), "F2": (sum(o[2] for o in out), sum(o[3] for o in out)),
            "F3": (sum(e[0] for e in eps), len(eps)), "F4": (sum(e[1] for e in eps), len(eps))}


def show(res):
    for k, (hit, n) in res.items():
        print(f"  {k}: {hit}/{n} = {100 * hit / n:.1f}%" if n else f"  {k}: 0/0 = n/a")
