"""Does the manuscript's prose agree with the bundle's generated evidence?

Runs last in verify.sh, after VERIFICATION.md is written, and takes that file as
argv[1]. A theorem count stated in MANUSCRIPT.tex must be written in digits and
must match the Mathlib audit of this run. A count spelled in words is refused,
because a count in words once escaped a numeric check in the reference bundle.
"""
import pathlib
import re
import sys

results = []


def check(name, ok, detail=""):
    results.append((name, bool(ok)))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


HERE = pathlib.Path(__file__).resolve().parent.parent
ver_path = pathlib.Path(sys.argv[1]) if len(sys.argv) > 1 else HERE / "VERIFICATION.md"
ver = ver_path.read_text()
tex = (HERE / "MANUSCRIPT.tex").read_text()

COUNT_WORDS = (r"(?:one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|"
               r"twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety|hundred)")


def stated_counts(text):
    digits = [int(x) for x in re.findall(r"\b(\d+)\s+(?:Lean\s+)?theorems\b", text)]
    words = re.findall(r"\b" + COUNT_WORDS + r"[- a-z]*\s+(?:Lean\s+)?theorems\b", text, re.I)
    return digits, words


print("=" * 72)
print("D1  THEOREM COUNTS STATED IN MANUSCRIPT.tex vs THE GENERATED AUDIT")
print("=" * 72)
m = re.search(r"theorems:\s*(\d+);\s*audited:\s*(\d+)", ver)
if not m:
    check("D1 the Mathlib audit line is present in the generated file", False,
          "cannot compare; did the Mathlib suite run?")
    allowed = set()
else:
    allowed = {int(m.group(1))}
    print(f"      generated: {m.group(1)} theorems declared, {m.group(2)} audited")
    digits, words = stated_counts(tex)
    check("D1a every theorem count the manuscript states matches the audit",
          all(d in allowed for d in digits), f"stated {digits}, allowed {sorted(allowed)}")
    check("D1b no theorem count is spelled in words", not words, f"{words}")

bad_d, _ = stated_counts("We prove 12345 theorems.")
_, bad_w = stated_counts("We prove thirty-one theorems.")
check("D1-control a wrong count and a count in words are both caught",
      any(d not in allowed for d in bad_d) and bool(bad_w))

nf = sum(1 for _, ok in results if not ok)
print("=" * 72)
print(f"DOCS SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
sys.exit(1 if nf else 0)
