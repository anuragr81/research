"""Suite for Laurison and Friedman (2016). The claims are about what the paper
reports, so the suite checks the quotations against the extracted text of the
accepted version, compiles the core-Lean arithmetic that links the printed
numbers, audits its axioms, and runs a fabricated quotation as a control.
"""

import difflib
import os
import pathlib
import re
import shutil
import subprocess
import sys
import tempfile

HERE = pathlib.Path(__file__).resolve().parent
SRC = HERE / "LaurisonFriedman2016.lean"
NS = "LaurisonFriedman2016"
CACHE = pathlib.Path.home() / ".cache" / "entry_contest" / "laurison_friedman_2016.txt"
THRESHOLD = 0.97
results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def norm(s):
    s = s.lower().replace("“", '"').replace("”", '"').replace("’", "'")
    s = re.sub(r"[^a-z0-9%£.]+", " ", s)
    return re.sub(r"\s+", " ", s).strip()


def best_ratio(quote, text_norm):
    q = norm(quote)
    if q in text_norm:
        return 1.0
    qw = q.split()
    best = 0.0
    for k in range(max(1, len(qw) - 2)):
        anchor = " ".join(qw[k:k + 3])
        offset = len(" ".join(qw[:k])) + (1 if k else 0)
        for m in re.finditer(re.escape(anchor), text_norm):
            start = max(0, m.start() - offset)
            window = text_norm[start:start + len(q)]
            best = max(best, difflib.SequenceMatcher(None, q, window).ratio())
        if best >= THRESHOLD:
            return best
    return best


print("=" * 72)
print("LAURISON-FRIEDMAN 2016 (quotations, arithmetic in core Lean, controls)")
print("=" * 72)

claims = (HERE / "CLAIMS.md").read_text()
rows = re.findall(r"^\|\s*(LF-\d+)\s*\|\s*(\d+)\s*\|\s*\"(.+?)\"\s*\|", claims, re.M)
check("LF-L0 every quotation row has an ID, a page and a quotation", len(rows) >= 10, f"{len(rows)} rows")

if not CACHE.exists():
    check("LF-L1 the extracted text is available", False,
          f"export the Drive PDF's text to {CACHE}")
    sys.exit(1)
text = CACHE.read_text()
check("LF-L1 the text is the accepted version of the paper",
      "American Sociological Review" in text[:3000] and "eprints.lse.ac.uk" in text[:3000])
tn = norm(text)
for rid, page, quote in rows:
    r = best_ratio(quote, tn)
    check(f"{rid} (p.{page}) is in the text", r >= THRESHOLD, f"match {r:.2f}")

fake = "the class pay gap is largest where the rule weights talent most and vanishes in the traditional professions"
r = best_ratio(fake, tn)
check("LF-C1 control: a fabricated quotation is not found", r < 0.9, f"best match {r:.2f}")

lean = shutil.which("lean")
if lean is None:
    check("LF-L2 lean on PATH", False, "lean not on PATH")
else:
    src = SRC.read_text()
    p = subprocess.run([lean, str(SRC)], capture_output=True, text=True)
    check("LF-L2 LaurisonFriedman2016.lean compiles with no errors", p.returncode == 0,
          (p.stdout + p.stderr).strip()[:300])
    check("LF-L3 no sorry in the source", re.search(r"\bsorry\b", src) is None)
    names = re.findall(r"^theorem\s+(\w+)", src, re.M)
    with tempfile.TemporaryDirectory() as td:
        audit = os.path.join(td, "audit.lean")
        with open(audit, "w") as fh:
            fh.write(src + "\n")
            for n in names:
                fh.write(f"#print axioms {NS}.{n}\n")
        q = subprocess.run([lean, audit], capture_output=True, text=True)
        out = q.stdout + q.stderr
    allowed = {"propext", "Quot.sound"}
    bad = []
    for n in names:
        m = re.search(rf"'{NS}\.{re.escape(n)}' (does not depend on any axioms|depends on axioms: \[([^\]]*)\])", out)
        if m is None:
            bad.append((n, "no audit line"))
        elif m.group(2):
            used = {a.strip() for a in m.group(2).split(",")}
            if not used <= allowed:
                bad.append((n, ", ".join(sorted(used - allowed))))
    check(f"LF-L4 every theorem audited within {sorted(allowed)}", not bad, f"{len(names)} theorems; {bad}")
    check("LF-L5 the controls are present",
          "control_firm_size_text_ne_table" in names and "control_doctors_rounding" in names)

fails = results.count(False)
print("=" * 72)
print(f"LAURISON-FRIEDMAN SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
