import csv
import gzip
import hashlib
import sys
from collections import defaultdict

from distress_targeting import (MIN_ASSET, MIN_BANKS, MIN_EPISODES, episodes, load, p3_cutoff, qflow, shift,
                                validity, verdict, within)
from dividend_location import EDGES, Y_STAR

GROUPS = ("S", "M")
SHARE_LIMIT = 0.05


def load_ownership(path):
    own = {}
    with gzip.open(path, "rt", newline="") as fh:
        for r in csv.DictReader(fh):
            own[(r["CERT"], r["REPDTE"])] = (r["RSSDHCR"].strip(), r["HCTMULT"].strip())
    return own


def group(own, cert, d):
    row = own.get((cert, d))
    if row is None:
        return "U"
    return {"1": "M", "0": "S"}.get(row[1], "U")


def no_holder(own, cert, d):
    return own[(cert, d)][0] in ("", "0")


def dividend_counts(panel, own):
    counts = defaultdict(lambda: [0, 0])
    for cert, s in panel.items():
        for d, rec in s.items():
            if rec["ASSET"] is None or rec["ASSET"] < MIN_ASSET or not rec["LIAB"] or rec["EQCDIV"] is None:
                continue
            flow = qflow(s, d, "EQCDIV")
            if flow is None:
                continue
            g = group(own, cert, d)
            x = rec["ASSET"] / rec["LIAB"]
            for lo, hi in zip(EDGES, EDGES[1:]):
                if lo <= x < hi:
                    for key in (g, g + "0" if g == "S" and no_holder(own, cert, d) else None):
                        if key is not None:
                            counts[(key, lo, hi)][0] += 1
                            counts[(key, lo, hi)][1] += flow > 0
    return counts


def e3(counts):
    print("E3  dividend paid in the quarter, by x = ASSET/LIAB, banks with assets >= $100m; "
          f"y* = {Y_STAR}; holds if the share below y* is at most {SHARE_LIMIT:.0%}")
    out = {}
    for key, label in (("S", "S stand-alone"), ("M", "M multibank subsidiary"), ("U", "U unclassified"),
                       ("S0", "S0 no holding company (within S, reported only)")):
        print(f"--- {label}")
        for lo, hi in zip(EDGES, EDGES[1:]):
            n, k = counts[(key, lo, hi)]
            share = f"{100 * k / n:5.1f}%" if n else "    -"
            print(f"  x in [{lo:.4f}, {hi:.4f}): {n:7d} bank-quarters, paid {share}")
        n = sum(counts[(key, lo, hi)][0] for lo, hi in zip(EDGES, EDGES[1:]) if hi <= Y_STAR)
        k = sum(counts[(key, lo, hi)][1] for lo, hi in zip(EDGES, EDGES[1:]) if hi <= Y_STAR)
        if n:
            v = "holds" if k / n <= SHARE_LIMIT else "fails"
            out[key] = v
            print(f"  below y*: {n} bank-quarters, paid {100 * k / n:.1f}%  ->  {v if key in GROUPS else 'reported'}")
    return out


def composition(panel, eps):
    stock = parent = 0.0
    for e in eps:
        s = panel[e["cert"]]
        stock += s[e["d"]]["EQCSXQ"]
        parent += qflow(s, e["d"], "EQCBHCTR")
    total = stock + parent
    return stock / total, parent / total


def e2(panel, eps, own):
    print("E2  within-bank regression of distress_targeting.py, proxies P1 and P2, by group of the pre-issue quarter")
    out = {}
    for g, label in (("S", "S stand-alone"), ("M", "M multibank subsidiary"), ("U", "U unclassified")):
        geps = [e for e in eps if group(own, e["cert"], shift(e["d"], -1)) == g]
        print(f"--- {label}: {len(geps)} episodes")
        if not geps:
            continue
        cs, cp = composition(panel, geps)
        print(f"  recapitalisation from stock sales {cs:.3f}, from transactions with the parent {cp:.3f}")
        print(f"  {'proxy':5s} {'valid?':>16s} {'eps':>5s} {'w-eps':>5s} {'banks':>5s} {'b_pre':>8s} {'se':>7s} "
              f"{'b_post':>8s} {'se':>7s} {'xbar':>6s}  verdict")
        verdicts = []
        for k in ("P1", "P2"):
            v = validity(geps, k)
            vtxt = f"z {v['z']:+.2f} p {v['p']:.3f}" if v else "untestable"
            sel = [e for e in geps if e[k]]
            r = within(sel)
            if "b_post" not in r:
                print(f"  {k:5s} {vtxt:>16s} {len(sel):5d} {r['n']:5d} {r['banks']:5d}  too few banks")
                continue
            enough = r["n"] >= MIN_EPISODES and r["banks"] >= MIN_BANKS
            vd = verdict(r) if enough else "not enough data"
            if enough:
                verdicts.append(vd)
            print(f"  {k:5s} {vtxt:>16s} {len(sel):5d} {r['n']:5d} {r['banks']:5d} {r['b_pre']:+8.4f} "
                  f"{r['se_pre']:7.4f} {r['b_post']:+8.4f} {r['se_post']:7.4f} {r['xbar']:6.3f}  {vd}")
        if g in GROUPS:
            out[g] = verdicts[0] if verdicts and set(verdicts) == {verdicts[0]} and verdicts[0] in (
                "holds", "fails") else "not measured"
            print(f"  group verdict: {out[g]}")
    return out


def report(panel_path, own_path, own):
    panel = load(panel_path)
    print(f"panel sha256 {hashlib.sha256(open(panel_path, 'rb').read()).hexdigest()}")
    print(f"ownership sha256 {hashlib.sha256(open(own_path, 'rb').read()).hexdigest()}")
    tally = defaultdict(int)
    changers = 0
    for cert, s in panel.items():
        gs = set()
        for d in s:
            g = group(own, cert, d)
            tally[g] += 1
            if g in GROUPS:
                gs.add(g)
        changers += len(gs) > 1
    print(f"bank-quarters in the panel by group: S {tally['S']}, M {tally['M']}, unclassified {tally['U']}")
    print(f"banks whose group changes within 2015-2024: {changers} of {len(panel)}")
    print()
    v3 = e3(dividend_counts(panel, own))
    print()
    v2 = e2(panel, episodes(panel, p3_cutoff(panel)), own)
    print()
    print(f"VERDICTS  E3: S {v3.get('S')}, M {v3.get('M')}   E2: S {v2.get('S')}, M {v2.get('M')}")


def main(panel_path, own_path):
    report(panel_path, own_path, load_ownership(own_path))


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
