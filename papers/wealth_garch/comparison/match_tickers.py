import csv
import io
import os
import re
import zipfile

from judged_risk import FIRMS, LISTS, holders, sha, LIST_SHA
from sample import ROOT, samples

NIC = [os.path.join(ROOT, "replication", "gordon_hirtle_2026", f"CSV_ATTRIBUTES_{s}.zip") for s in ("ACTIVE", "CLOSED")]
NIC_SHA = {"CSV_ATTRIBUTES_ACTIVE.zip": "a5b2f2b7ec3a2282cf71a6c858838b4a100ce958752ddf118d280c33212aa0f4",
           "CSV_ATTRIBUTES_CLOSED.zip": "adcedf003026cf001c606466ea420bf49ebfd086e39c01ab1381d21ca04d818c"}
NOT_COMMON = re.compile(r"\b(warrants?|units?|rights?|notes?|preferred|depositary)\b", re.I)


def norm(name):
    words = re.sub(r"[^a-z0-9]+", " ", name.lower()).split()
    text = " ".join(words)
    text = re.sub(r"\bcommon (stock|shares)\b", " ", text).split()
    if text and text[0] == "the":
        text = text[1:]
    if text and text[-1] == "the":
        text = text[:-1]
    return "".join(text)


def nic():
    recs = {}
    by_cert = {}
    for path in NIC:
        assert sha(path) == NIC_SHA[os.path.basename(path)], path
        active = "ACTIVE" in path
        z = zipfile.ZipFile(path)
        with z.open(z.namelist()[0]) as fh:
            for r in csv.DictReader(io.TextIOWrapper(fh, encoding="latin-1")):
                rssd = r["#ID_RSSD"].strip()
                end = r["DT_END"].strip() if "DT_END" in r else ""
                country = r["CNTRY_INC_NM"].strip() or r["CNTRY_NM"].strip()
                recs.setdefault(rssd, []).append((active, end, r["NM_LGL"].strip(), country))
                cert = r["ID_FDIC_CERT"].strip()
                if cert and cert != "0":
                    by_cert.setdefault(cert, set()).add(rssd)
    return recs, by_cert


def listed_common():
    out = {}
    for path in LISTS:
        assert sha(path) == LIST_SHA[os.path.basename(path)], path
        with open(path) as fh:
            rows = list(csv.reader(fh, delimiter="|"))
        head = rows[0]
        for r in rows[1:]:
            if len(r) != len(head):
                continue
            d = dict(zip(head, r))
            name = d["Security Name"]
            if d.get("Test Issue") == "Y" or d.get("ETF") == "Y" or NOT_COMMON.search(name):
                continue
            if not re.search(r"common (stock|shares)", name, re.I):
                continue
            out.setdefault(norm(name), set()).add((r[0], name))
    return out


def main():
    recs, by_cert = nic()
    common = listed_common()
    known = {f[0]: f[2] for f in FIRMS}
    samp, _ = samples()
    banks = sorted(samp["B"], key=lambda b: -b["size"])
    hc = holders({b["cert"] for b in banks})
    print(f"sample B: {len(banks)} banks")
    parents = {}
    for b in banks:
        h = hc.get(b["cert"], "")
        if h in ("", "0"):
            own = by_cert.get(b["cert"], set())
            h = sorted(own)[0] if len(own) == 1 else ""
            how = "itself" if h else f"itself, {len(own)} RSSD records"
        else:
            how = "RSSDHCR"
        parents.setdefault(h, []).append((b["cert"], how))
    covered, notcov = [], []
    for h, bs in parents.items():
        rs = recs.get(h, [])
        act = [r for r in rs if r[0]]
        main_rec = act[0] if act else max(rs, key=lambda r: r[1]) if rs else None
        country = main_rec[3] if main_rec else "?"
        names = sorted({r[2] for r in rs})
        certs = ", ".join(f"{c} ({w})" for c, w in bs)
        if h in known:
            covered.append((h, names, known[h], "kept from PREREG_comparison_9.md", certs))
            continue
        if country != "UNITED STATES":
            notcov.append((h, names, f"country {country}", certs))
            continue
        cands = set()
        for n in names:
            cands |= common.get(norm(n), set())
        if len(cands) == 1:
            t, lname = next(iter(cands))
            covered.append((h, names, t, lname, certs))
        else:
            notcov.append((h, names, f"{len(cands)} candidates {sorted(cands)}", certs))
    print(f"\ncovered parents: {len(covered)}, banks {sum(len(parents[c[0]]) for c in covered)}")
    for h, names, t, lname, certs in sorted(covered, key=lambda c: c[2]):
        print(f"  {h:>8s} {t:6s} NIC {names} | listed {lname!r} | banks {certs}")
    print(f"\nnot covered parents: {len(notcov)}, banks {sum(len(parents[c[0]]) for c in notcov)}")
    for h, names, why, certs in notcov:
        print(f"  {h:>8s} NIC {names} | {why} | banks {certs}")
    print("\nStooq pages for the new tickers")
    for h, names, t, lname, certs in sorted(covered, key=lambda c: c[2]):
        if h not in known:
            print(f"  https://stooq.com/q/d/?s={t.lower().replace('.', '-')}.us&i=d")


if __name__ == "__main__":
    main()
