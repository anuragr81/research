"""Suite for Hassin and Haviv, "Equilibrium Threshold Strategies: The Case of Queues with
Priorities" (Operations Research 45, 1997), read from the authors' preprint. The suite pins the
PDF by sha256 and checks each quotation verbatim on the page named, against the text layer
(pdftotext -layout), ignoring case, punctuation and spaces. It runs a reversed-quote control. It
then measures lean/mathlib/HassinHaviv.lean: a build root, it builds, no sorry, every Lean name
cited in CLAIMS.md declared, every theorem within the Mathlib axioms, and the controls present.
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
PDF = CACHE / "hassin_haviv_1997.pdf"
SHA256 = "4ffd3593bc5909db9a717c94870dd0de5c7008a7ad07abf7c2e74db7bbe3fb2a"
results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def squash(s):
    s = re.sub(r"-\s*\n\s*", "", s).lower()
    return re.sub(r"[^a-z0-9]+", "", s)


print("=" * 72)
print("HASSIN-HAVIV 1997 (Operations Research; quotations; lean/mathlib/HassinHaviv.lean)")
print("=" * 72)

claims = (HERE / "CLAIMS.md").read_text()
rows = re.findall(r"^\|\s*(HH-\d+)\s*\|\s*(\d+)\s*\|\s*\"(.+?)\"\s*\|", claims, re.M)
check("HH-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 12, f"{len(rows)} rows")

if not PDF.exists():
    check("HH-Q1 the cached PDF is available", False, f"download ay.pdf from the Drive folder to {PDF}")
    sys.exit(1)
d = hashlib.sha256(PDF.read_bytes()).hexdigest()
check("HH-Q1 the cached PDF is the one read", d == SHA256, d[:16])
pt = shutil.which("pdftotext")
pages = subprocess.run([pt, "-layout", str(PDF), "-"], capture_output=True, text=True).stdout.split("\f")
check("HH-Q2 the text is Hassin and Haviv's paper",
      "equilibriumthresholdstrategiesthecaseofqueueswith" in squash(pages[0]) and len(pages) >= 17,
      f"{len(pages) - 1} pages")
for rid, page, quote in rows:
    idx = int(page) - 1
    check(f"{rid} (p.{page}) is on the page verbatim",
          0 <= idx < len(pages) and squash(quote) in squash(pages[idx]))
flip = squash("In FTC case there is a single fixed point.")
check("HH-C1 control: HH-5 with ATC replaced by FTC is not in the text",
      all(flip not in squash(p) for p in pages))

ROOT = HERE.parent.parent
PROJ = ROOT / "lean" / "mathlib"
LSRC = PROJ / "HassinHaviv.lean"
LNS = "HassinHaviv1997"
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
check("HH-L2 HassinHaviv is a build root", '"HassinHaviv"' in lakefile)
check("HH-L3 no sorry in the source", re.search(r"\bsorry\b", lsrc) is None)
check("HH-L3b no native_decide in the source", "native_decide" not in lsrc)
declared = re.findall(r"^(?:theorem|lemma)\s+([^\s:(\[{]+)", lsrc, re.M)
defs = re.findall(r"^def\s+([^\s:(\[{]+)", lsrc, re.M)
cited = sorted(set(re.findall(r"`([a-z][A-Za-z0-9_']*)`", claims)))
missing = [c for c in cited if c not in declared and c not in defs]
check("HH-L4 every Lean name cited in CLAIMS.md is declared", not missing,
      f"{len(cited)} cited, {len(declared)} declared" + (f", missing {missing}" if missing else ""))
lake = shutil.which("lake") or os.path.expanduser("~/.elan/bin/lake")
b = subprocess.run([lake, "build", "HassinHaviv"], cwd=PROJ, capture_output=True, text=True)
check("HH-L5 lake build HassinHaviv succeeds", b.returncode == 0, b.stderr.strip()[-200:])
names = [f"{LNS}.{d}" for d in declared]
with tempfile.NamedTemporaryFile("w", suffix=".lean", dir=PROJ, delete=False) as fh:
    fh.write("import HassinHaviv\n")
    for n in names:
        fh.write(f"#print axioms {n}\n")
    tmp = fh.name
pr = subprocess.run([lake, "env", "lean", tmp], cwd=PROJ, capture_output=True, text=True)
os.unlink(tmp)
used = parse_axioms(pr.stdout + pr.stderr, names)
check("HH-L6 the axiom audit covers every declared theorem", len(used) == len(names), f"{len(used)} of {len(names)}")
bad = {n: sorted(a - ALLOWED) for n, a in used.items() if a - ALLOWED}
check("HH-L7 no axiom outside propext, Classical.choice and Quot.sound", not bad, str(bad) if bad else "")
check("HH-L8 the controls are present",
      all(c in declared for c in ("ftc_many", "E35_one", "E35_increments", "four_eight_not_equilibria")))

fails = results.count(False)
print("=" * 72)
print(f"HASSIN-HAVIV 1997 SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
