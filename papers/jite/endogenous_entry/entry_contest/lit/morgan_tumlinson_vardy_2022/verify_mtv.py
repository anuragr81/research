"""Suite for Morgan, Tumlinson and Vardy (IMF WP/18/231, 2018). The claims
are about what the paper reports, so the suite checks each quotation verbatim
(after normalising case, spacing, punctuation and ligatures) against the
cached text of the working paper, runs a reversed-quote control, compiles the
core-Lean record of the sign comparison with M28, and audits its axioms.
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
print("MORGAN-TUMLINSON-VARDY (working paper 2018; quotations, sign record in core Lean)")
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

lean = shutil.which("lean")
if lean is None:
    check("MTV-L2 lean on PATH", False, "lean not on PATH")
else:
    src = SRC.read_text()
    p = subprocess.run([lean, str(SRC)], capture_output=True, text=True)
    check("MTV-L2 MorganTumlinsonVardy.lean compiles with no errors", p.returncode == 0,
          (p.stdout + p.stderr).strip()[:300])
    check("MTV-L3 no sorry in the source", re.search(r"\bsorry\b", src) is None)
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
    check(f"MTV-L4 every theorem audited within {sorted(allowed)}", not bad, f"{len(names)} theorems; {bad}")
    check("MTV-L5 the control is present", "control_same_margin_same_sign" in names)

fails = results.count(False)
print("=" * 72)
print(f"MORGAN-TUMLINSON-VARDY SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
