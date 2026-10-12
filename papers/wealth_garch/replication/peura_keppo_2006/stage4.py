import csv
import gzip
import hashlib
import os
import statistics
import sys
from multiprocessing import Pool

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "06_empirical_starter"))

from distress_targeting import MIN_ASSET, load, qflow, shift
from stage2 import load_panel, load_rates, solve
from stage3 import bank_record

MIN_OBS = 20


def load_rbct2(path):
    with gzip.open(path, "rt", newline="") as fh:
        return {(r["CERT"], r["REPDTE"]): (float(r["RBCT2"]) if r["RBCT2"] not in ("", "None") else None)
                for r in csv.DictReader(fh)}


def buffer(q, t2, cert, d):
    rec = q.get(cert, {}).get(d)
    if rec is None or rec["ASSET"] is None or rec["ASSET"] < MIN_ASSET:
        return None
    r2 = t2.get((cert, d))
    if rec["RBCT1J"] is None or r2 is None or not rec["RWAJT"] or rec["RWAJT"] <= 0:
        return None
    return (rec["RBCT1J"] + r2) / rec["RWAJT"] - 0.08


def barrier(b):
    return b["u2"] if b["u2"] is not None else b["u0"]


def verdict(share, n, holds):
    if n < MIN_OBS:
        return "not measured"
    return "holds" if holds(share) else "fails"


def run(banks, q, t2, label):
    issuers = [b for b in banks if b["u2"] is not None]
    print()
    print(f"===== SAMPLE {label}: {len(banks)} banks, {len(issuers)} issuer, {len(banks) - len(issuers)} non-issuer")
    t1 = {"issuer": [], "non-issuer": []}
    t2s = {"issuer": [], "non-issuer": []}
    gaps1, eps = [], []
    for b in banks:
        s, kind, bar = q.get(b["cert"], {}), ("issuer" if b["u2"] is not None else "non-issuer"), barrier(b)
        for d in sorted(s):
            now = buffer(q, t2, b["cert"], d)
            if now is not None:
                t2s[kind].append(now > bar + b["mu"] + b["sigma"])
            pre = buffer(q, t2, b["cert"], shift(d, -1))
            div = qflow(s, d, "EQCDIV")
            if pre is not None and now is not None and div is not None and div > 0:
                t1[kind].append(pre < bar - b["sigma"] / 2)
                gaps1.append(pre - bar)
            if kind == "issuer" and pre is not None and now is not None:
                stock, parent, merger = s[d]["EQCSXQ"], qflow(s, d, "EQCBHCTR"), qflow(s, d, "EQCMRG")
                if stock is not None and parent is not None and merger is not None and merger == 0 \
                        and stock + parent >= 0.005 * s[d]["ASSET"]:
                    eps.append((pre <= b["u1"] + b["sigma"], abs(now - b["u2"]) <= b["sigma"], now - b["u2"]))
    out = {}
    for name, groups, rule, pred in (("T1 dividends below the barrier", t1, lambda x: x <= 0.05, "0%"),
                                     ("T2 capital far above the barrier", t2s, lambda x: x <= 0.05, "0%")):
        allv = groups["issuer"] + groups["non-issuer"]
        share = sum(allv) / len(allv) if allv else float("nan")
        v = verdict(share, len(allv), rule)
        out[name] = v
        parts = "; ".join(f"{k} {sum(g)}/{len(g)} = {100 * sum(g) / len(g):.1f}%" for k, g in groups.items() if g)
        print(f"{name}: {sum(allv)}/{len(allv)} = {100 * share:.1f}% (model {pred}) -> {v}   [{parts}]")
    for name, idx in (("T3 issue from near u1", 0), ("T4 issue lands near u2", 1)):
        n = len(eps)
        share = sum(e[idx] for e in eps) / n if n else float("nan")
        v = verdict(share, n, lambda x: x >= 0.80)
        out[name] = v
        print(f"{name}: {sum(e[idx] for e in eps)}/{n} = {100 * share:.1f}% (model 100%) -> {v}")
    if gaps1:
        print(f"median b_(q-1) - barrier over dividend quarters: {100 * statistics.median(gaps1):+.2f} pp")
    if eps:
        print(f"median b_q - u2 over episodes: {100 * statistics.median(e[2] for e in eps):+.2f} pp")
    return out


def main(annual_path, rates_path, quarterly_path, rbct2_path):
    for p in (annual_path, rates_path, quarterly_path, rbct2_path):
        print(f"{os.path.basename(p)} sha256 {hashlib.sha256(open(p, 'rb').read()).hexdigest()}")
    panel = load_panel(annual_path)
    rates = load_rates(rates_path)
    banks = [b for b in (bank_record(c, s, rates, 2015, 8) for c, s in panel.items()) if b]
    with Pool() as pool:
        solved = pool.map(solve, sorted(banks, key=lambda b: int(b["cert"])))
    q = load(quarterly_path)
    t2 = load_rbct2(rbct2_path)
    a = run(solved, q, t2, "A (all national banks)")
    b = run(sorted(solved, key=lambda x: -x["size"])[:61], q, t2, "B (61 largest)")
    print()
    print("VERDICTS")
    for k in a:
        print(f"  {k}: A {a[k]}, B {b[k]}")


if __name__ == "__main__":
    main(*sys.argv[1:5])
