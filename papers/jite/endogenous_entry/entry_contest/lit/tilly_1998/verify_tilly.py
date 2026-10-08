"""Suite for Tilly (1998), one page read from the author's photograph. No OCR
is available, so the suite pins the image by hash, checks the quotation rows,
compiles the core-Lean record of the definition's logical form, and audits
its axioms. The manuscript checker separately confirms that row L19 quotes
this record verbatim.
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
SRC = HERE / "Tilly1998.lean"
NS = "Tilly1998"
IMAGE = pathlib.Path.home() / ".cache" / "entry_contest" / "tilly_opportunity_hoarding.png"
SHA256 = "0419ff008b9829f4c6e437e356f4ef39b8531164613153e9280093332b8d446a"
results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


print("=" * 72)
print("TILLY 1998 (p. 154 from the author's photograph; logical form in core Lean)")
print("=" * 72)

claims = (HERE / "CLAIMS.md").read_text()
rows = re.findall(r"^\|\s*(TL-\d+)\s*\|\s*(\d+)\s*\|\s*\"(.+?)\"\s*\|", claims, re.M)
check("TL-L0 every quotation row has an ID, a page and a quotation", len(rows) >= 2, f"{len(rows)} rows")
check("TL-L1 every quotation is from p. 154, the page read", all(p == "154" for _, p, _ in rows))
check("TL-L2 the definition is recorded in full",
      any(q.startswith("If members of a network acquire access") and q.endswith("sustain their control.")
          for _, _, q in rows))

if not IMAGE.exists():
    check("TL-L3 the page image is available", False, f"download opportunity_hoarding.png from Drive to {IMAGE}")
else:
    digest = hashlib.sha256(IMAGE.read_bytes()).hexdigest()
    check("TL-L3 the cached image is the page recorded", digest == SHA256, digest[:16])

lean = shutil.which("lean")
if lean is None:
    check("TL-L4 lean on PATH", False, "lean not on PATH")
else:
    src = SRC.read_text()
    p = subprocess.run([lean, str(SRC)], capture_output=True, text=True)
    check("TL-L4 Tilly1998.lean compiles with no errors", p.returncode == 0,
          (p.stdout + p.stderr).strip()[:300])
    check("TL-L5 no sorry in the source", re.search(r"\bsorry\b", src) is None)
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
    check(f"TL-L6 every theorem audited within {sorted(allowed)}", not bad, f"{len(names)} theorems; {bad}")
    check("TL-L7 the controls are present",
          "control_not_necessary" in names and "control_categorical_needs_bound" in names)

fails = results.count(False)
print("=" * 72)
print(f"TILLY SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
