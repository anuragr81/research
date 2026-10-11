import csv
import gzip
import hashlib
import math
import statistics
import sys
from collections import defaultdict

QE = ("0331", "0630", "0930", "1231")
YTD = ("NETINC", "EQCBHCTR", "EQCMRG", "EQCDIV")
MIN_ASSET = 100000
THRESHOLD = 0.005
DECILE = 0.10
MIN_EPISODES = 30
MIN_BANKS = 15
Z95 = 1.959963984540054


def num(v):
    if v is None or v == "":
        return None
    return float(v)


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


def xval(rec):
    if rec is None or rec["ASSET"] is None or rec["LIAB"] is None or rec["LIAB"] <= 0:
        return None
    return rec["ASSET"] / rec["LIAB"]


def tier1_ratio(rec):
    if rec is None or rec["RBCT1J"] is None or rec["RWAJT"] is None or rec["RWAJT"] <= 0:
        return None
    return rec["RBCT1J"] / rec["RWAJT"]


def p3_cutoff(panel):
    vals = [tier1_ratio(rec) for s in panel.values() for rec in s.values()
            if rec["ASSET"] is not None and rec["ASSET"] >= MIN_ASSET]
    vals = sorted(v for v in vals if v is not None)
    return vals[int(DECILE * len(vals))]


def p1(s, pre):
    flows = [qflow(s, shift(pre, -k), "NETINC") for k in range(4)]
    return None if any(v is None for v in flows) else sum(flows) < 0


def p2(s, pre):
    a, b = xval(s.get(pre)), xval(s.get(shift(pre, -4)))
    return None if a is None or b is None else (1 - 1 / a) <= (1 - 1 / b) - 0.01


def p3(s, pre, cut):
    v = tier1_ratio(s.get(pre))
    return None if v is None else v < cut


def p4(s, pre):
    a, b = s.get(pre), s.get(shift(pre, -4))
    if a is None or b is None or a["ASSET"] is None or b["ASSET"] is None:
        return None
    return a["ASSET"] <= b["ASSET"]


def no_dividend(s, d, pre):
    a, b = qflow(s, d, "EQCDIV"), qflow(s, pre, "EQCDIV")
    return None if a is None or b is None else (a == 0 and b == 0)


def episodes(panel, cut):
    out = []
    for cert, s in panel.items():
        for d, rec in s.items():
            pre = shift(d, -1)
            prec = s.get(pre)
            if prec is None or rec["ASSET"] is None or prec["ASSET"] is None:
                continue
            if rec["ASSET"] < MIN_ASSET or prec["ASSET"] < MIN_ASSET:
                continue
            stock, parent, merger = rec["EQCSXQ"], qflow(s, d, "EQCBHCTR"), qflow(s, d, "EQCMRG")
            if stock is None or parent is None or merger is None or merger != 0:
                continue
            recap = stock + parent
            if recap < THRESHOLD * rec["ASSET"]:
                continue
            xpre, xpost = xval(prec), xval(rec)
            if xpre is None or xpost is None or not (1 < xpre < 2 and 1 < xpost < 2):
                continue
            out.append({"cert": cert, "d": d, "pre": xpre, "post": xpost, "f": recap / rec["ASSET"],
                        "P1": p1(s, pre), "P2": p2(s, pre), "P3": p3(s, pre, cut), "P4": p4(s, pre),
                        "nodiv": no_dividend(s, d, pre)})
    return out


def validity(eps, key):
    known = [e for e in eps if e[key] is not None and e["nodiv"] is not None]
    a = [e["nodiv"] for e in known if e[key]]
    b = [e["nodiv"] for e in known if not e[key]]
    if not a or not b:
        return None
    pa, pb = sum(a) / len(a), sum(b) / len(b)
    pool = (sum(a) + sum(b)) / (len(a) + len(b))
    se = math.sqrt(pool * (1 - pool) * (1 / len(a) + 1 / len(b)))
    z = (pa - pb) / se if se > 0 else 0.0
    p = 0.5 * math.erfc(z / math.sqrt(2))
    return {"pa": pa, "pb": pb, "na": len(a), "nb": len(b), "z": z, "p": p, "valid": p < 0.05}


def within(eps):
    by = defaultdict(list)
    for e in eps:
        by[e["cert"]].append(e)
    by = {k: v for k, v in by.items() if len(v) >= 2}
    rows = [e for v in by.values() for e in v]
    if len(by) < 2:
        return {"n": len(rows), "banks": len(by)}
    dm = {}
    for k, v in by.items():
        m = {c: statistics.fmean(e[c] for e in v) for c in ("pre", "post", "f")}
        dm[k] = [{c: e[c] - m[c] for c in ("pre", "post", "f")} for e in v]
    sxx = sum(r["f"] ** 2 for v in dm.values() for r in v)
    g = len(dm)
    res = {"n": len(rows), "banks": g, "xbar": statistics.fmean(e["pre"] for e in rows)}
    for y in ("pre", "post"):
        b = sum(r["f"] * r[y] for v in dm.values() for r in v) / sxx
        meat = sum(sum(r["f"] * (r[y] - b * r["f"]) for r in v) ** 2 for v in dm.values())
        res[f"b_{y}"] = b
        res[f"se_{y}"] = math.sqrt(g / (g - 1) * meat) / sxx
    return res


def verdict(r):
    lo, hi = r["b_post"] - Z95 * r["se_post"], r["b_post"] + Z95 * r["se_post"]
    if lo > 0 or hi < 0:
        return "fails"
    if not (lo <= r["xbar"] / 2 <= hi):
        return "holds"
    return "not measured"


def table(eps, label):
    print(f"--- {label}: {len(eps)} episodes")
    print(f"{'group':6s} {'eps':>5s} {'w-eps':>5s} {'banks':>5s} {'b_pre':>8s} {'se':>7s} {'b_post':>8s} {'se':>7s} {'xbar':>6s}  verdict")
    groups = {k: [e for e in eps if e[k]] for k in ("P1", "P2", "P3", "P4")}
    groups["none"] = [e for e in eps if not any(e[k] for k in ("P1", "P2", "P3", "P4"))]
    groups["all"] = eps
    out = {}
    for k, g in groups.items():
        r = within(g)
        out[k] = r
        if "b_post" not in r:
            print(f"{k:6s} {len(g):5d} {r['n']:5d} {r['banks']:5d}  too few banks")
            continue
        enough = r["n"] >= MIN_EPISODES and r["banks"] >= MIN_BANKS
        v = verdict(r) if enough else "not enough data"
        r["verdict"] = v
        print(f"{k:6s} {len(g):5d} {r['n']:5d} {r['banks']:5d} {r['b_pre']:+8.4f} {r['se_pre']:7.4f} "
              f"{r['b_post']:+8.4f} {r['se_post']:7.4f} {r['xbar']:6.3f}  {v}")
    return out


def main(path):
    digest = hashlib.sha256(open(path, "rb").read()).hexdigest()
    panel = load(path)
    cut = p3_cutoff(panel)
    eps = episodes(panel, cut)
    print(f"panel sha256 {digest}")
    print(f"banks {len(panel)}; P3 cutoff Tier1/RWA {cut:.4f}")
    print()
    print("VALIDITY  no dividend in the issuing and preceding quarter, classified vs not")
    valid = {}
    for k in ("P1", "P2", "P3", "P4"):
        missing = sum(e[k] is None for e in eps)
        v = validity(eps, k)
        valid[k] = bool(v and v["valid"])
        print(f"{k}: classified {v['na']} share {v['pa']:.3f}; others {v['nb']} share {v['pb']:.3f}; "
              f"z {v['z']:+.2f} p {v['p']:.4f} -> {'valid' if v['valid'] else 'not valid'}; inputs missing {missing}")
    print()
    full = table(eps, "2015-2024")
    print()
    table([e for e in eps if e["d"] < "2020"], "2015-2019 (reported, not used for the verdict)")
    print()
    table([e for e in eps if e["d"] >= "2020"], "2020-2024 (reported, not used for the verdict)")
    print()
    counted = [k for k in ("P1", "P2", "P3", "P4") if valid[k] and full[k].get("verdict") not in (None, "not enough data")]
    verdicts = {full[k]["verdict"] for k in counted}
    overall = verdicts.pop() if len(verdicts) == 1 and verdicts <= {"holds", "fails"} else "not measured"
    print(f"proxies valid with enough data: {counted or 'none'}")
    print(f"OVERALL VERDICT (exact targeting, b_post = 0): {overall}")


if __name__ == "__main__":
    main(sys.argv[1])
