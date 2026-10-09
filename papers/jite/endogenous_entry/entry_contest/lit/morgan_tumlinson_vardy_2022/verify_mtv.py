"""Suite for Morgan, Tumlinson and Vardy (IMF WP/18/231, 2018). It checks each
quotation verbatim (after normalising case, spacing, punctuation and
ligatures) against the cached text of the working paper, runs a reversed-quote
control, and measures lean/mathlib/MeritocracyLogistic.lean, the exact
instance of their Proposition 1: a build root, it builds, no sorry, every Lean
name cited in CLAIMS.md declared, every theorem within the Mathlib axioms.
"""

import os
import pathlib
import re
import shutil
import subprocess
import sys
import tempfile

HERE = pathlib.Path(__file__).resolve().parent
SRC = HERE / "MorganTumlinsonVardy.lean"
NS = "MorganTumlinsonVardy"
CACHE = pathlib.Path.home() / ".cache" / "entry_contest" / "morgan_tumlinson_vardy_2018.txt"
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
print("MORGAN-TUMLINSON-VARDY (working paper 2018; quotations; lean/mathlib/MeritocracyLogistic.lean)")
print("=" * 72)

claims = (HERE / "CLAIMS.md").read_text()
rows = re.findall(r"^\|\s*(MTV-\d+)\s*\|\s*(\d+)\s*\|\s*\"(.+?)\"\s*\|", claims, re.M)
check("MTV-L0 every quotation row has an ID, a page and a quotation", len(rows) >= 5, f"{len(rows)} rows")

if not CACHE.exists():
    check("MTV-L1 the cached text is available", False, f"extract wp18231.pdf from Drive to {CACHE}")
    sys.exit(1)
text = CACHE.read_text()
check("MTV-L1 the text is the IMF working paper WP/18/231", "WP/18/231" in text[:2000] and "The Limits of Meritocracy" in text[:2000])
tn = norm(text)
for rid, page, quote in rows:
    check(f"{rid} (p.{page}) is in the text verbatim", norm(quote) in tn)
flip = "once it kicks in, attrition proceeds from the top of the ability distribution"
check("MTV-C1 control: MTV-5 with bottom replaced by top is not in the text", norm(flip) not in tn)

ROOT = HERE.parent.parent
PROJ = ROOT / "lean" / "mathlib"
LSRC = PROJ / "MeritocracyLogistic.lean"
LNS = "MeritocracyLogistic"
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
check("MTV-L2 MeritocracyLogistic is a build root", '"MeritocracyLogistic"' in lakefile)
check("MTV-L3 no sorry in the source", re.search(r"\bsorry\b", lsrc) is None)
declared = re.findall(r"^theorem\s+([^\s:(\[{]+)", lsrc, re.M)
cited = sorted(set(re.findall(r"`([a-z][A-Za-z0-9_']*)`", claims)))
missing = [c for c in cited if c not in declared]
check("MTV-L4 every Lean name cited in CLAIMS.md is declared", not missing,
      f"{len(cited)} cited, {len(declared)} declared" + (f", missing {missing}" if missing else ""))
lake = shutil.which("lake") or os.path.expanduser("~/.elan/bin/lake")
b = subprocess.run([lake, "build", LNS], cwd=PROJ, capture_output=True, text=True)
check("MTV-L5 lake build MeritocracyLogistic succeeds", b.returncode == 0, b.stderr.strip()[-200:])
names = [f"{LNS}.{d}" for d in declared]
with tempfile.NamedTemporaryFile("w", suffix=".lean", dir=PROJ, delete=False) as fh:
    fh.write(f"import {LNS}\n")
    for n in names:
        fh.write(f"#print axioms {n}\n")
    tmp = fh.name
pr = subprocess.run([lake, "env", "lean", tmp], cwd=PROJ, capture_output=True, text=True)
os.unlink(tmp)
used = parse_axioms(pr.stdout + pr.stderr, names)
check("MTV-L6 the axiom audit covers every declared theorem", len(used) == len(names), f"{len(used)} of {len(names)}")
bad = {n: sorted(a - ALLOWED) for n, a in used.items() if a - ALLOWED}
check("MTV-L7 no axiom outside propext, Classical.choice and Quot.sound", not bad, str(bad) if bad else "")
check("MTV-L8 the controls are present",
      "control_constant_hazard_no_solution" in declared and "control_soc_fails" in declared)

fails = results.count(False)
print("=" * 72)
print(f"MORGAN-TUMLINSON-VARDY SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
