"""Suite for Fryer and Loury, "Categorical Redistribution in Winner-Take-All Markets"
(NBER Working Paper 10104, 2003). It pins the
cached PDF by sha256, extracts its text with pdftotext -layout, checks each
quotation verbatim (after normalising case, spacing, punctuation and
ligatures) on the page named, runs a reversed-quote control, and measures
lean/mathlib/FryerLouryTournament.lean: a build root, it builds, no sorry, every Lean
name cited in CLAIMS.md declared, every theorem within the Mathlib axioms, and
the controls present.
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
PDF = pathlib.Path.home() / ".cache" / "entry_contest" / "w10104.pdf"
TXT = PDF.with_suffix(".txt")
SHA256 = "11ce87a918c5b395630d5cf73bfa7ad4eb08bb3ceccbff982e33789ada2799b4"
OFFSET = 1  # PDF page minus printed page
LIGATURES = {"ﬀ": "ff", "ﬁ": "fi", "ﬂ": "fl", "ﬃ": "ffi", "ﬄ": "ffl"}
results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def norm(s):
    for k, v in LIGATURES.items():
        s = s.replace(k, v)
    s = re.sub(r"-\s*\n\s*", "", s).lower()
    s = re.sub(r"[^a-z0-9]+", " ", s)
    return re.sub(r"\s+", " ", s).strip()


print("=" * 72)
print("FRYER-LOURY 2003 (NBER WP 10104; quotations; lean/mathlib/FryerLouryTournament.lean)")
print("=" * 72)

claims = (HERE / "CLAIMS.md").read_text()
rows = re.findall(r"^\|\s*(FT-D?\d+)\s*\|\s*(\d+)\s*\|\s*\"(.+?)\"\s*\|", claims, re.M)
check("FT-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 14, f"{len(rows)} rows")

pages = None
if not PDF.exists():
    check("FT-Q1 the cached PDF is available", False, f"download the paper from Drive to {PDF}")
else:
    d = hashlib.sha256(PDF.read_bytes()).hexdigest()
    check("FT-Q1 the cached PDF is the one read", d == SHA256, d[:16])
    pt = shutil.which("pdftotext")
    if pt:
        pages = subprocess.run([pt, "-layout", str(PDF), "-"], capture_output=True, text=True).stdout.split("\f")
    elif TXT.exists():
        pages = TXT.read_text().split("\f")
if pages is None:
    check("FT-Q2 the text is available", False, "install pdftotext or provide the cached text")
    sys.exit(1)
check("FT-Q2 the text is the working paper", "WINNER-TAKE-ALL MARKETS" in pages[0] and len(pages) >= 27,
      f"{len(pages)} pages")
for rid, page, quote in rows:
    p = int(page) + OFFSET
    check(f"{rid} (p.{page}) is on the page verbatim", 1 <= p <= len(pages) and norm(quote) in norm(pages[p - 1]))
flip = "and this pool decreases with the aggressiveness of the redistributive goal"
check("FT-C1 control: FT-10 with increases replaced by decreases is not in the text", flip not in norm(" ".join(pages)))
check("FT-C2 control: FT-10 is not on the first page", norm(rows[9][2]) not in norm(pages[0]))

ROOT = HERE.parent.parent
PROJ = ROOT / "lean" / "mathlib"
LSRC = PROJ / "FryerLouryTournament.lean"
LNS = "FryerLoury2003"
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
check("FT-L2 FryerLouryTournament is a build root", '"FryerLouryTournament"' in lakefile)
check("FT-L3 no sorry in the source", re.search(r"\bsorry\b", lsrc) is None)
declared = re.findall(r"^(?:theorem|lemma)\s+([^\s:(\[{]+)", lsrc, re.M)
cited = sorted(set(re.findall(r"`([a-z][A-Za-z0-9_']*)`", claims)))
missing = [c for c in cited if c not in declared]
check("FT-L4 every Lean name cited in CLAIMS.md is declared", not missing,
      f"{len(cited)} cited, {len(declared)} declared" + (f", missing {missing}" if missing else ""))
lake = shutil.which("lake") or os.path.expanduser("~/.elan/bin/lake")
b = subprocess.run([lake, "build", "FryerLouryTournament"], cwd=PROJ, capture_output=True, text=True)
check("FT-L5 lake build FryerLouryTournament succeeds", b.returncode == 0, b.stderr.strip()[-200:])
names = [f"{LNS}.{d}" for d in declared]
with tempfile.NamedTemporaryFile("w", suffix=".lean", dir=PROJ, delete=False) as fh:
    fh.write("import FryerLouryTournament\n")
    for n in names:
        fh.write(f"#print axioms {n}\n")
    tmp = fh.name
pr = subprocess.run([lake, "env", "lean", tmp], cwd=PROJ, capture_output=True, text=True)
os.unlink(tmp)
used = parse_axioms(pr.stdout + pr.stderr, names)
check("FT-L6 the axiom audit covers every declared theorem", len(used) == len(names), f"{len(used)} of {len(names)}")
bad = {n: sorted(a - ALLOWED) for n, a in used.items() if a - ALLOWED}
check("FT-L7 no axiom outside propext, Classical.choice and Quot.sound", not bad, str(bad) if bad else "")
check("FT-L8 the controls are present", "pool_grows" in declared and "prop1_best_reply" in declared)

fails = results.count(False)
print("=" * 72)
print(f"FRYER-LOURY 2003 SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
