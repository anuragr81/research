import csv
import gzip
import hashlib
import os
import sys
from collections import defaultdict

COMMON = {"N", "NM", "SM"}
SHOWN = ["ASSET", "EQ", "NETINC", "EQCDIV", "RBCT1J", "RWAJT", "EQCSXQ", "EQCBHCTR", "EQCMRG"]
FLOWS = ["EQCSXQ", "EQCBHCTR", "EQCMRG"]
BASIC = ["ASSET", "EQ", "NETINC", "EQCDIV"]


def tally(paths):
    n_all, n_common, present, banks = defaultdict(int), defaultdict(int), defaultdict(lambda: defaultdict(int)), defaultdict(set)
    fields = None
    for path in paths:
        with gzip.open(path, "rt", newline="") as fh:
            reader = csv.DictReader(fh)
            fields = reader.fieldnames
            for r in reader:
                q = r["REPDTE"]
                n_all[q] += 1
                if r["BKCLASS"] not in COMMON:
                    continue
                n_common[q] += 1
                banks[q[:4]].add(r["CERT"])
                for f in fields:
                    if r[f] != "":
                        present[q][f] += 1
    return n_all, n_common, present, banks, fields


def share(present, n_common, q, f):
    return present[q][f] / n_common[q] if n_common[q] else 0.0


def main(paths):
    for p in paths:
        print(f"{os.path.basename(p)} sha256 {hashlib.sha256(open(p, 'rb').read()).hexdigest()}")
    n_all, n_common, present, banks, fields = tally(paths)
    quarters = sorted(n_all)
    print()
    print("quarter   all  common  " + "  ".join(f"{f:>8s}" for f in SHOWN))
    for q in quarters:
        print(f"{q} {n_all[q]:5d} {n_common[q]:6d}  " + "  ".join(f"{share(present, n_common, q, f):8.3f}" for f in SHOWN))
    print()
    print("year  distinct common-sample banks  lowest quarterly share over all fields (field)")
    for y in sorted(banks):
        qs = [q for q in quarters if q[:4] == y]
        low = min(((share(present, n_common, q, f), f) for q in qs for f in fields), key=lambda t: t[0])
        print(f"{y}  {len(banks[y]):6d}  {low[0]:.3f} ({low[1]})")
    print()
    e1 = [(q, f) for q in quarters for f in BASIC if share(present, n_common, q, f) < 0.95]
    e2a = [q for q in quarters if "19901231" <= q <= "20191231" and share(present, n_common, q, "RWAJT") < 0.95]
    e2b = [q for q in quarters if q < "19900101" and share(present, n_common, q, "RWAJT") > 0]
    e3a = [(q, f) for q in quarters for f in FLOWS
           if (q.endswith("1231") or q >= "20010101") and share(present, n_common, q, f) < 0.95]
    e3b = [(q, f) for q in quarters for f in FLOWS
           if "19850101" <= q <= "20001231" and not q.endswith("1231") and share(present, n_common, q, f) >= 0.5]
    print(f"E1 assets, equity, income, dividends >= 95% every quarter: {'holds' if not e1 else 'fails'} {e1[:6]}")
    print(f"E2 RWAJT >= 95% 1990Q4-2019Q4: {'holds' if not e2a else 'fails'} {e2a[:6]}")
    print(f"E2 RWAJT empty before 1990: {'holds' if not e2b else 'fails'} {e2b[:6]}")
    print(f"E3 equity flows >= 95% every December and every quarter from 2001: {'holds' if not e3a else 'fails'} {e3a[:6]}")
    print(f"E3 equity flows < 50% in non-December quarters 1985-2000: {'holds' if not e3b else 'fails'} {e3b[:6]}")
    late = [q for q in quarters if q > "20241231"]
    print(f"hold-out: report dates after 2024: {len(late)}")


if __name__ == "__main__":
    main(sys.argv[1:])
