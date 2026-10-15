"""Suite for Liu, Garg and Borgs, "Strategic Ranking" (AISTATS 2022), read from
arXiv:2109.08240v2. The suite pins the PDF by sha256 and checks each quotation verbatim on the page
named, against the text layer read in layout and in reading order (pdftotext -layout and -raw),
ignoring case, punctuation and spaces. It runs a reversed-quote control. It then measures
lean/mathlib/LiuGargBorgs.lean: a build root, it builds, no sorry, every Lean name cited in CLAIMS.md
declared, every theorem within the Mathlib axioms, and the controls present.
"""

import hashlib
import os
import pathlib
import re
import shutil
import subprocess
import sys
import tempfile

HERE = pathlib.Path(__file__).resolve().parent
CACHE = pathlib.Path.home() / ".cache" / "entry_contest"
PDF = CACHE / "liu_garg_borgs_2022.pdf"
SHA256 = "5a0238a533ac91100a27d7ab80ea09cfa2a514cfd207476d716a6d43ba8b918e"
results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def squash(s):
    s = re.sub(r"-\s*\n\s*", "", s).lower()
    return re.sub(r"[^a-z0-9]+", "", s)


print("=" * 72)
print("LIU-GARG-BORGS 2022 (AISTATS; quotations; lean/mathlib/LiuGargBorgs.lean)")
print("=" * 72)

claims = (HERE / "CLAIMS.md").read_text()
rows = re.findall(r"^\|\s*(LGB-\d+)\s*\|\s*(\d+)\s*\|\s*\"(.+?)\"\s*\|", claims, re.M)
check("LGB-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 15, f"{len(rows)} rows")

if not PDF.exists():
    check("LGB-Q1 the cached PDF is available", False, "download https://arxiv.org/pdf/2109.08240v2 to " + str(PDF))
    sys.exit(1)
d = hashlib.sha256(PDF.read_bytes()).hexdigest()
check("LGB-Q1 the cached PDF is the one read", d == SHA256, d[:16])
pt = shutil.which("pdftotext")
lay = subprocess.run([pt, "-layout", str(PDF), "-"], capture_output=True, text=True).stdout.split("\f")
raw = subprocess.run([pt, "-raw", str(PDF), "-"], capture_output=True, text=True).stdout.split("\f")
pages = [a + "\n" + b for a, b in zip(lay, raw)]
check("LGB-Q2 the text is the paper read",
      "strategicranking" in squash(pages[0]) and len(pages) >= 30, f"{len(pages) - 1} pages")
for rid, page, quote in rows:
    idx = int(page) - 1
    check(f"{rid} (p.{page}) is on the page verbatim",
          0 <= idx < len(pages) and squash(quote) in squash(pages[idx]))
flip = squash("Pure randomization has lower A than any two-level policy.")
check("LGB-C1 control: LGB-12 with higher replaced by lower is not in the text", all(flip not in squash(p) for p in pages))

ROOT = HERE.parent.parent
PROJ = ROOT / "lean" / "mathlib"
LSRC = PROJ / "LiuGargBorgs.lean"
LNS = "LiuGargBorgs2022"
ALLOWED = {"propext", "Classical.choice", "Quot.sound"}


def parse_axioms(out, names):
    used = {}
    for n in names:
        m = re.search(rf"'{re.escape(n)}' (does not depend on any axioms|depends on axioms: \[([^\]]*)\])", out)
        if m:
            used[n] = set() if m.group(2) is None else {a.strip() for a in m.group(2).split(",")}
    return used


lsrc = LSRC.read_text()
lakefile = (PROJ / "lakefile.toml").read_text()
check("LGB-L2 LiuGargBorgs is a build root", '"LiuGargBorgs"' in lakefile)
check("LGB-L3 no sorry and no native_decide in the source",
      re.search(r"\bsorry\b", lsrc) is None and "native_decide" not in lsrc)
declared = re.findall(r"^(?:theorem|lemma)\s+([^\s:(\[{]+)", lsrc, re.M)
defs = re.findall(r"^(?:noncomputable\s+)?def\s+([^\s:(\[{]+)", lsrc, re.M)
cited = sorted(set(re.findall(r"`([a-z][A-Za-z0-9_']*)`", claims)))
missing = [c for c in cited if c not in declared and c not in defs]
check("LGB-L4 every Lean name cited in CLAIMS.md is declared", not missing,
      f"{len(cited)} cited, {len(declared)} declared" + (f", missing {missing}" if missing else ""))
lake = shutil.which("lake") or os.path.expanduser("~/.elan/bin/lake")
b = subprocess.run([lake, "build", "LiuGargBorgs"], cwd=PROJ, capture_output=True, text=True)
check("LGB-L5 lake build LiuGargBorgs succeeds", b.returncode == 0, b.stderr.strip()[-200:])
names = [f"{LNS}.{d}" for d in declared]
with tempfile.NamedTemporaryFile("w", suffix=".lean", dir=PROJ, delete=False) as fh:
    fh.write("import LiuGargBorgs\n")
    for n in names:
        fh.write(f"#print axioms {n}\n")
    tmp = fh.name
pr = subprocess.run([lake, "env", "lean", tmp], cwd=PROJ, capture_output=True, text=True)
os.unlink(tmp)
used = parse_axioms(pr.stdout + pr.stderr, names)
check("LGB-L6 the axiom audit covers every declared theorem", len(used) == len(names), f"{len(used)} of {len(names)}")
bad = {n: sorted(a - ALLOWED) for n, a in used.items() if a - ALLOWED}
check("LGB-L7 no axiom outside propext, Classical.choice and Quot.sound", not bad, str(bad) if bad else "")
check("LGB-L8 the controls are present", all(c in declared for c in ("access_needs_floor", "access_pure")))

fails = results.count(False)
print("=" * 72)
print(f"LIU-GARG-BORGS 2022 SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
