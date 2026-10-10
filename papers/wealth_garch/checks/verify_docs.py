import pathlib
import re
import sys

results = []


def check(name, ok, detail=""):
    results.append((name, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


HERE = pathlib.Path(__file__).resolve().parent.parent
ver_path = pathlib.Path(sys.argv[1]) if len(sys.argv) > 1 else HERE / "VERIFICATION.md"
ver = ver_path.read_text()
tex = (HERE / "MANUSCRIPT.tex").read_text()

print("=" * 72)
print("D1  LEAN COUNTS STATED IN MANUSCRIPT.tex vs THE GENERATED AUDIT")
print("=" * 72)

COUNT_WORDS = r"(?:twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety|hundred)"


def stated_counts(text):
    digits = [int(x) for x in re.findall(r"\b(\d+)\s+(?:Lean\s+)?theorems\b", text)]
    words = re.findall(COUNT_WORDS + r"[- a-z]*\s+(?:Lean\s+)?theorems\b", text, re.I)
    return digits, words


m_tot = re.search(r"theorems:\s*(\d+);\s*audited:\s*(\d+)", ver)
if not m_tot:
    check("D1 the audit line is present in the generated file", False,
          "cannot compare; did the Mathlib suite run?")
else:
    allowed = {int(m_tot.group(1))}
    print(f"      generated: {m_tot.group(1)} theorems declared in lean/mathlib/")
    digits, words = stated_counts(tex)
    check("D1a every theorem count the manuscript states matches the audit",
          all(d in allowed for d in digits), f"stated {digits}, allowed {sorted(allowed)}")
    check("D1b no theorem count is spelled in words", not words, f"{words}")
    bad_d, _ = stated_counts("We prove 12345 theorems.")
    _, bad_w = stated_counts("We prove thirty-one theorems.")
    check("D1-control a wrong count and a count in words are both caught",
          any(d not in allowed for d in bad_d) and bool(bad_w))

print()
print("=" * 72)
print("D2  NO FILE ASSERTS AN ENVIRONMENT FACT THAT verify.sh MEASURES")
print("=" * 72)

pat = re.compile(r"mathlib|toolchain", re.I)
bad = re.compile(r"unavailable|infeasible|not available|not installed|no such toolchain|without access|cannot be built|not (yet )?compile(d|-checked)|yet compiled", re.I)
offenders = []
scanned = 0
for path in sorted(HERE.rglob("*")):
    if path.suffix not in {".tex", ".lean", ".md", ".py", ".sh"}:
        continue
    if path.name in {"verify_docs.py", "VERIFICATION.md"} or path.name.startswith("HANDOVER"):
        continue
    if ".lake" in path.parts or "__pycache__" in path.parts:
        continue
    if path.relative_to(HERE).parts[0] not in {"MANUSCRIPT.tex", "lean", "checks", "lit", "notes", "TODO.md", "verify.sh"}:
        continue
    scanned += 1
    lines = path.read_text(errors="replace").splitlines()
    for n, line in enumerate(lines, 1):
        window = " ".join(lines[max(0, n - 2):n])
        if pat.search(window) and bad.search(line):
            offenders.append(f"{path.relative_to(HERE)}:{n}: {line.strip()[:70]}")
for o in offenders:
    print(f"      OFFENDER {o}")
check("D2 no skeleton file asserts the Lean toolchain or Mathlib is unavailable", not offenders,
      f"{scanned} files scanned; assertions of a measured fact: {len(offenders)}")
probe = ["Not compile-checked against a live Lean/Mathlib", "toolchain -- no such toolchain is available here."]
hit = any(pat.search(" ".join(probe[max(0, n - 1):n + 1])) and bad.search(l) for n, l in enumerate(probe))
check("D2-control an uncorrected assertion spread over two lines is caught", hit)

print()
print("=" * 72)
print("D3  EVERY CHECK IN checks/ IS ACTUALLY INVOKED BY verify.sh")
print("=" * 72)

def orphaned(scripts, vsh):
    return [s for s in scripts if s not in vsh]


vsh = (HERE / "verify.sh").read_text()
scripts = sorted(p.name for p in (HERE / "checks").glob("verify_*.py"))
orphans = orphaned(scripts, vsh)
print(f"      {len(scripts)} check scripts: {', '.join(scripts)}")
for o in orphans:
    print(f"      ORPHAN {o} is never run")
check("D3 no orphaned check scripts", not orphans,
      f"{len(scripts)} scripts, {len(orphans)} never invoked")
check("D3-control a script absent from verify.sh is reported",
      orphaned(scripts + ["verify_absent.py"], vsh) == orphans + ["verify_absent.py"])

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"DOCS SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
sys.exit(1 if nf else 0)
