"""D: does the paper's prose agree with the bundle's own generated evidence?

Two defect classes have now recurred and neither was catchable by any existing
suite, because both live in English rather than in a number a check compares.

  F1 (2 Sep, morning)  PROOFS.tex cited 3000 randomised spreads where the
                       suite ran 758.  Caught by hand.
  Recurrence (2 Sep)   PROOFS.tex cited "thirty-one theorems, twenty ...
                       eleven" where the audit reported 52 / 36 / 16.  A
                       numeric audit missed it because the counts were spelled
                       out as words.
  Environment note     PROOFS.tex and EntryContest.lean asserted Mathlib was
                       unreachable.  verify.sh now measures that, and the
                       measurement contradicted the prose in two files while a
                       third paragraph of the same document corrected it.

This suite runs LAST, after VERIFICATION.md has been written, and compares the
paper against it.  It takes the generated file as argv[1].
"""
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
print("      PROOFS.tex, retired at pass 6, stated the core counts in a sentence")
print("      this check compared. The manuscript states no counts. Any count it")
print("      comes to state, in digits, must match the audit, and a count in")
print("      words is refused because words are how the 2 Sep error survived.")

COUNT_WORDS = r"(?:twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety|hundred)"


def stated_counts(text):
    digits = [int(x) for x in re.findall(r"\b(\d+)\s+(?:Lean\s+)?theorems\b", text)]
    words = re.findall(COUNT_WORDS + r"[- a-z]*\s+(?:Lean\s+)?theorems\b", text, re.I)
    return digits, words


m_tot = re.search(r"theorems:\s*(\d+);\s*audited:\s*(\d+)", ver)
m_thm = re.search(r"theorems declared:\s*(\d+)", ver)
if not m_tot or not m_thm:
    check("D1 the audit lines are present in the generated file", False,
          "cannot compare; did the Lean suites run?")
else:
    allowed = {int(m_tot.group(1)), int(m_thm.group(1)), int(m_tot.group(1)) + int(m_thm.group(1))}
    print(f"      generated: Mathlib {m_tot.group(1)}, core {m_thm.group(1)}")
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
print("      Mathlib's presence is probed on every run. Any file that instead")
print("      ASSERTS it is unavailable can contradict the measurement, which")
print("      is what happened on 2 Sep in two files at once. VERIFICATION.md")
print("      is excluded: it is generated, and the target here is authored prose.")
print("      A +/-2 line window is allowed, so a file may QUOTE the old claim")
print("      while correcting it nearby; only an uncorrected assertion fails.")

pat = re.compile(r"mathlib", re.I)
bad = re.compile(r"unreachable|unavailable|infeasible|not available|cannot be (built|reached)", re.I)
allow = re.compile(r"was false|used to record|previously|no longer|probe|corrected|defect|OFFENDER", re.I)
offenders = []
scanned = 0
for path in sorted(HERE.rglob("*")):
    if path.suffix not in {".tex", ".lean", ".md", ".py", ".sh"}:
        continue
    if path.name in {"verify.sh", "verify_docs.py", "VERIFICATION.md"}:
        continue
    if ".lake" in path.parts or "__pycache__" in path.parts:
        continue
    scanned += 1
    lines = path.read_text(errors="replace").splitlines()
    for n, line in enumerate(lines, 1):
        if not (pat.search(line) and bad.search(line)):
            continue
        window = " ".join(lines[max(0, n - 3):n + 2])
        if allow.search(window):
            continue
        offenders.append(f"{path.relative_to(HERE)}:{n}: {line.strip()[:70]}")
for o in offenders:
    print(f"      OFFENDER {o}")
check("D2 no file asserts Mathlib is unavailable", not offenders,
      f"{scanned} files scanned; assertions of a measured fact: {len(offenders)}")

print()
print("=" * 72)
print("D3  EVERY CHECK IN checks/ IS ACTUALLY INVOKED BY verify.sh")
print("=" * 72)

vsh = (HERE / "verify.sh").read_text()
scripts = sorted(p.name for p in (HERE / "checks").glob("verify_*.py"))
orphans = [s for s in scripts if s not in vsh]
print(f"      {len(scripts)} check scripts: {', '.join(scripts)}")
for o in orphans:
    print(f"      ORPHAN {o} is never run")
check("D3 no orphaned check scripts", not orphans,
      f"{len(scripts)} scripts, {len(orphans)} never invoked")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"DOCS SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
sys.exit(1 if nf else 0)
