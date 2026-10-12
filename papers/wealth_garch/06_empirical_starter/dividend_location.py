import csv
import gzip
import hashlib
import sys
from collections import defaultdict

QE = ("0331", "0630", "0930", "1231")
MIN_ASSET = 100000
Y_STAR = 1.571518
EDGES = (1.0, 1.03, 1.10, 1.20, 1.39, Y_STAR, 2.0)


def shift(d, k):
    y, q = int(d[:4]), QE.index(d[4:])
    t = y * 4 + q + k
    return f"{t // 4}{QE[t % 4]}"


def main(path):
    digest = hashlib.sha256(open(path, "rb").read()).hexdigest()
    panel = defaultdict(dict)
    with gzip.open(path, "rt", newline="") as fh:
        for r in csv.DictReader(fh):
            panel[r["CERT"]][r["REPDTE"]] = {k: (float(v) if v != "" else None) for k, v in r.items()
                                             if k not in ("CERT", "REPDTE")}
    counts = defaultdict(lambda: [0, 0])
    for s in panel.values():
        for d, rec in s.items():
            if rec["ASSET"] is None or rec["ASSET"] < MIN_ASSET or not rec["LIAB"] or rec["EQCDIV"] is None:
                continue
            if d[4:] == "0331":
                flow = rec["EQCDIV"]
            else:
                prev = s.get(shift(d, -1))
                if prev is None or prev["EQCDIV"] is None:
                    continue
                flow = rec["EQCDIV"] - prev["EQCDIV"]
            x = rec["ASSET"] / rec["LIAB"]
            for lo, hi in zip(EDGES, EDGES[1:]):
                if lo <= x < hi:
                    counts[(lo, hi)][0] += 1
                    counts[(lo, hi)][1] += flow > 0
    print(f"panel sha256 {digest}")
    print(f"dividend paid in the quarter, by x = ASSET/LIAB, banks with assets >= $100m; y* = {Y_STAR}")
    for lo, hi in zip(EDGES, EDGES[1:]):
        n, k = counts[(lo, hi)]
        print(f"  x in [{lo:.4f}, {hi:.4f}): {n:7d} bank-quarters, paid {100 * k / n:5.1f}%")
    n = sum(counts[b][0] for b in counts if b[1] <= Y_STAR)
    k = sum(counts[b][1] for b in counts if b[1] <= Y_STAR)
    print(f"below y*: {n} bank-quarters, paid {100 * k / n:.1f}%; the model predicts 0%")


if __name__ == "__main__":
    main(sys.argv[1])
