"""Suite for Hu, Immorlica and Vaughan, "The Disparate Effects of Strategic Manipulation"
(FAT* 2019), read from arXiv:1808.08646v4. The suite pins the PDF by sha256 and checks each quotation verbatim on the page
named, against the text layer read in layout and in reading order (pdftotext -layout and -raw),
ignoring case, punctuation and spaces. It runs a reversed-quote control. It then measures
lean/mathlib/HuImmorlicaVaughan.lean: a build root, it builds, no sorry, every Lean name cited in CLAIMS.md
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
PDF = CACHE / "hu_immorlica_vaughan_2019.pdf"
SHA256 = "95e0d127bea7d0a6fbc8b294bf662a68bf0bb3e26e0b5ff5e9d1734cfcb5e8d8"
results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def squash(s):
    s = re.sub(r"-\s*\n\s*", "", s).lower()
    return re.sub(r"[^a-z0-9]+", "", s)


print("=" * 72)
print("HU-IMMORLICA-VAUGHAN 2019 (FAT*; quotations; lean/mathlib/HuImmorlicaVaughan.lean)")
print("=" * 72)

claims = (HERE / "CLAIMS.md").read_text()
rows = re.findall(r"^\|\s*(HIV-\d+)\s*\|\s*(\d+)\s*\|\s*\"(.+?)\"\s*\|", claims, re.M)
check("HIV-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 13, f"{len(rows)} rows")

if not PDF.exists():
    check("HIV-Q1 the cached PDF is available", False, "download https://arxiv.org/pdf/1808.08646v4 to " + str(PDF))
    sys.exit(1)
d = hashlib.sha256(PDF.read_bytes()).hexdigest()
check("HIV-Q1 the cached PDF is the one read", d == SHA256, d[:16])
pt = shutil.which("pdftotext")
lay = subprocess.run([pt, "-layout", str(PDF), "-"], capture_output=True, text=True).stdout.split("\f")
raw = subprocess.run([pt, "-raw", str(PDF), "-"], capture_output=True, text=True).stdout.split("\f")
pages = [a + "\n" + b for a, b in zip(lay, raw)]
check("HIV-Q2 the text is the paper read",
      "thedisparateeffectsofstrategicmanipulation" in squash(pages[0]) and len(pages) >= 29, f"{len(pages) - 1} pages")
for rid, page, quote in rows:
    idx = int(page) - 1
    check(f"{rid} (p.{page}) is on the page verbatim",
          0 <= idx < len(pages) and squash(quote) in squash(pages[idx]))
flip = squash("Manipulating from a feature vector x to y is always at least as costly for a member of group A as it is for a member of group B.")
check("HIV-C1 control: HIV-2 with the groups swapped is not in the text", all(flip not in squash(p) for p in pages))

ROOT = HERE.parent.parent
PROJ = ROOT / "lean" / "mathlib"
LSRC = PROJ / "HuImmorlicaVaughan.lean"
LNS = "HuImmorlicaVaughan2019"
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
check("HIV-L2 HuImmorlicaVaughan is a build root", '"HuImmorlicaVaughan"' in lakefile)
check("HIV-L3 no sorry and no native_decide in the source",
      re.search(r"\bsorry\b", lsrc) is None and "native_decide" not in lsrc)
declared = re.findall(r"^(?:theorem|lemma)\s+([^\s:(\[{]+)", lsrc, re.M)
defs = re.findall(r"^(?:noncomputable\s+)?def\s+([^\s:(\[{]+)", lsrc, re.M)
cited = sorted(set(re.findall(r"`([a-z][A-Za-z0-9_']*)`", claims)))
missing = [c for c in cited if c not in declared and c not in defs]
check("HIV-L4 every Lean name cited in CLAIMS.md is declared", not missing,
      f"{len(cited)} cited, {len(declared)} declared" + (f", missing {missing}" if missing else ""))
lake = shutil.which("lake") or os.path.expanduser("~/.elan/bin/lake")
b = subprocess.run([lake, "build", "HuImmorlicaVaughan"], cwd=PROJ, capture_output=True, text=True)
check("HIV-L5 lake build HuImmorlicaVaughan succeeds", b.returncode == 0, b.stderr.strip()[-200:])
names = [f"{LNS}.{d}" for d in declared]
with tempfile.NamedTemporaryFile("w", suffix=".lean", dir=PROJ, delete=False) as fh:
    fh.write("import HuImmorlicaVaughan\n")
    for n in names:
        fh.write(f"#print axioms {n}\n")
    tmp = fh.name
pr = subprocess.run([lake, "env", "lean", tmp], cwd=PROJ, capture_output=True, text=True)
os.unlink(tmp)
used = parse_axioms(pr.stdout + pr.stderr, names)
check("HIV-L6 the axiom audit covers every declared theorem", len(used) == len(names), f"{len(used)} of {len(names)}")
bad = {n: sorted(a - ALLOWED) for n, a in used.items() if a - ALLOWED}
check("HIV-L7 no axiom outside propext, Classical.choice and Quot.sound", not bad, str(bad) if bad else "")
check("HIV-L8 the controls are present", all(c in declared for c in ("sigma_order_needs_condition", "ell_def_sign")))

fails = results.count(False)
print("=" * 72)
print(f"HU-IMMORLICA-VAUGHAN 2019 SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
