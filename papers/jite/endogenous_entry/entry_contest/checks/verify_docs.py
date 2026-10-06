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
tex = (HERE / "PROOFS.tex").read_text()

print("=" * 72)
print("D1  LEAN COUNTS IN PROOFS.tex vs THE GENERATED AUDIT")
print("=" * 72)

m_aud = re.search(r"audited:\s*(\d+)\s*of\s*(\d+)\s*declared;\s*axiom-free:\s*(\d+)", ver)
m_thm = re.search(r"theorems declared:\s*(\d+)", ver)
if not m_aud or not m_thm:
    check("D1 the audit lines are present in the generated file", False,
          "cannot compare; did the Lean suite run?")
else:
    naud, ndecl, nfree = (int(x) for x in m_aud.groups())
    ncore = ndecl - nfree
    print(f"      generated: {ndecl} declared, {naud} audited, {nfree} axiom-free, "
          f"{ncore} on core axioms")
    m_tex = re.search(
        r"Of the (\d+) theorems, (\d+) depend on no axioms at all and (\d+) depend", tex)
    if not m_tex:
        check("D1a PROOFS.tex states the counts in digits, not words", False,
              "pattern not found -- if the sentence was reworded, update this check; "
              "words are not machine-checkable and are how the last error survived")
    else:
        t_all, t_free, t_core = (int(x) for x in m_tex.groups())
        print(f"      PROOFS.tex: {t_all} theorems, {t_free} axiom-free, {t_core} on core")
        check("D1a total theorem count matches the audit", t_all == ndecl,
              f"PROOFS {t_all} vs generated {ndecl}")
        check("D1b axiom-free count matches the audit", t_free == nfree,
              f"PROOFS {t_free} vs generated {nfree}")
        check("D1c core-axiom count matches, and the three are consistent",
              t_core == ncore and t_free + t_core == t_all,
              f"PROOFS {t_core} vs generated {ncore}")
    check("D1d the audit covered every declared theorem", naud == ndecl,
          f"{naud} of {ndecl}")

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
