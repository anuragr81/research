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

print("=" * 72)
print("D2  THE TABLE OF LEAN FILES IN APPENDIX B")
print("=" * 72)
sys.path.insert(0, str(HERE / "checks"))
from lean_files_table import current, table  # noqa: E402

want, files, cites = table(tex)
have = current(tex)
check("D2a the table matches the rows that cite each file", have == want,
      "run: python checks/lean_files_table.py --write" if have != want else "")
uncited = [f for f in files if f not in cites]
check("D2b every Lean file is cited by a row, R1, a literature record or the measurement map",
      not uncited, f"{uncited}")
fake = tex.replace(r"\mrow{M1}{", r"\mrow{M99}{", 1)
check("D2-control a renamed row makes the table stale", current(fake) != table(fake)[0])

print("=" * 72)
print("D3  CONCLUSIONS STAND ON VERIFIED GROUND")
print("=" * 72)


def split_tables(text):
    text = re.sub(r"(?<!\\)%.*", "", text)
    i = text.find(r"\label{tab:conclusions}")
    j = text.find(r"\label{tab:pending}")
    k = text.find(r"\end{longtable}", j) if j >= 0 else -1
    if min(i, j, k) < 0:
        return None, None
    return text[i:j], text[j:k]


def crows(block):
    return re.findall(r"\\crow\{(C\d+)\}\{((?:[^{}]|\{(?:[^{}]|\{[^{}]*\})*\})*)\}", block)


t4, t5 = split_tables(tex)
if t4 is None:
    check("D3 the two conclusion tables are found", False)
else:
    marked = [r for r, body in crows(t4) if "\\pend{" in body or "\\unv{" in body]
    unnamed = [r for r, body in crows(t5) if "\\pend{" not in body]
    print(f"      conclusions table: {len(crows(t4))}; pending table: {len(crows(t5))}")
    check("D3a no row of the conclusions table carries a pending or unverified mark", not marked, f"{marked}")
    check("D3b every row of the pending table names what it waits on", not unnamed, f"{unnamed}")
    sample = (r"\label{tab:conclusions} \crow{C1}{x \pend{a paper}}{M1} "
              r"\label{tab:pending} \crow{C2}{y}{M1} \end{longtable}")
    s4, s5 = split_tables(sample)
    check("D3-control a pending mark in the conclusions table and an unnamed claim in the pending table are both caught",
          any("\\pend{" in b for _, b in crows(s4)) and any("\\pend{" not in b for _, b in crows(s5)))

nf = sum(1 for _, ok in results if not ok)
print("=" * 72)
print(f"DOCS SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
sys.exit(1 if nf else 0)
