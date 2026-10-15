"""R: the predecessor manuscript was retired at pass 6. Is everything in it
accounted for?

Reads the retired file from git at the commit recorded in RETIREMENT.md, takes
an inventory (theorem-like environments by line, section titles, figures,
Lean names, numbers), and requires each item to be present in the documents
that replace it or listed in RETIREMENT.md with a reason. Controls check that
an invented item of each kind is caught.
"""
import pathlib
import re
import subprocess
import sys

HERE = pathlib.Path(__file__).resolve().parent.parent
COMMIT = "91a74cb1"
GITPATH = "papers/subjective_racism/theory_and_decisions/manuscript.tex"
RETIRED = HERE.parent / "theory_and_decisions" / "manuscript.tex"
ENVS = "theorem|proposition|lemma|corollary|claim|assumption|definition|remark|example"
results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def inventory(src):
    src = re.sub(r"(?<!\\)%.*", "", src)
    lines = src.split("\n")
    envs = [i + 1 for i, ln in enumerate(lines) if re.search(rf"\\begin\{{({ENVS})\}}", ln)]
    secs = re.findall(r"\\(?:section|subsection|subsubsection)\*?\{((?:[^{}]|\{[^{}]*\})*)\}", src)
    figs = re.findall(r"\\includegraphics(?:\[[^\]]*\])?\{([^}]*)\}", src)
    names = set(re.findall(r"\\texttt\{([A-Za-z0-9_\\']+)\}", src))
    names = {n.replace("\\_", "_") for n in names}
    names = {n for n in names if "_" in n and not n.endswith((".py", ".lean"))}
    body = re.sub(r"\\cite[a-z]*\*?(\[[^\]]*\])*\{[^}]*\}", " ", src)
    body = re.sub(r"\\(label|ref|eqref|includegraphics|bibliography|bibliographystyle)"
                  r"(\[[^\]]*\])?\{[^}]*\}", " ", body)
    nums = re.findall(r"(?<![A-Za-z0-9\\{.])(\d+(?:[.,]\d+)*(?:/\d+)?)(?![0-9])", body)
    nums = {n for n in nums if ("." in n or "/" in n or "," in n) or int(n) >= 10}
    return envs, secs, figs, names, nums


def prose_quotes(tex):
    """Quotations in prose, written ``...''. The L-row macro adds its own quote
    marks, so L-row quotations do not appear here; MS-5 checks those."""
    tex = re.sub(r"(?<!\\)%.*", "", tex)
    return [q for q in re.findall(r"``(.*?)''", tex, re.S) if "#" not in q]


def squash(s):
    return re.sub(r"\s+", "", s)


def misquoted(quotes, src):
    flat = squash(re.sub(r"(?<!\\)%.*", "", src))
    return [q for q in quotes if squash(q) not in flat]


def unaccounted(inv, dest, record):
    envs, secs, figs, names, nums = inv
    rows = set(re.findall(r"^\|\s*([^|]+?)\s*\|", record, re.M))
    ticks = set(re.findall(r"`([^`]+)`", record))
    return ([e for e in envs if str(e) not in rows],
            [t for t in secs if t not in rows],
            [f for f in figs if f not in rows],
            sorted(n for n in names if n not in dest and n not in ticks),
            sorted(n for n in nums if n not in dest and n not in rows))


print("=" * 72)
print("R  RETIREMENT OF THE PREDECESSOR MANUSCRIPT: EVERY PART ACCOUNTED FOR")
print("=" * 72)
check("R0 the predecessor is no longer in the working tree", not RETIRED.exists())
record = (HERE / "RETIREMENT.md").read_text()
check("R1 RETIREMENT.md names the commit it reads", COMMIT in record and GITPATH in record)
p = subprocess.run(["git", "show", f"{COMMIT}:{GITPATH}"], cwd=HERE, capture_output=True, text=True)
if p.returncode != 0:
    check("R2 the retired file can be read from git", False, p.stderr.strip()[:200])
    sys.exit(1)
src = p.stdout
check("R2 the retired file can be read from git", True, f"{len(src.splitlines())} lines")

files = ["MANUSCRIPT.tex", "MEASUREMENT_MAP.tex", "TODO.md", "lean/mathlib/README.md"]
dest = "\n".join((HERE / f).read_text() for f in files)

inv = inventory(src)
be, bs, bf, bn, bu = unaccounted(inv, dest, record)
print(f"      inventory: {len(inv[0])} environments, {len(inv[1])} sections, {len(inv[2])} figures, "
      f"{len(inv[3])} Lean names, {len(inv[4])} numbers")
check("R3 every theorem-like environment has a destination", not be, f"{be}")
check("R4 every section has a destination", not bs, f"{bs}")
check("R5 every figure has a destination", not bf, f"{bf}")
check("R6 every Lean name is carried or recorded", not bn, f"{bn}")
check("R7 every number is carried or recorded with a reason", not bu, f"{bu}")

quotes = []
for f in ["MANUSCRIPT.tex", "MEASUREMENT_MAP.tex"]:
    quotes += prose_quotes((HERE / f).read_text())
bad_q = misquoted(quotes, src)
print(f"      prose quotations of the predecessor: {len(quotes)}")
check("R8 every prose quotation is verbatim in the retired file", quotes and not bad_q, f"{bad_q}")

print()
print("CONTROLS  an invented item of each kind must be caught")
fake = src + "\n\\begin{proposition}x\\end{proposition}\n\\section{An invented section}\n" \
             "\\includegraphics{figures/invented}\n\\texttt{invented\\_theorem\\_name} 98765\n"
fe, fs, ff, fn, fu = unaccounted(inventory(fake), dest, record)
check("R-C1 an invented environment is caught", len(fe) == 1)
check("R-C2 an invented section is caught", "An invented section" in fs)
check("R-C3 an invented figure is caught", "figures/invented" in ff)
check("R-C4 an invented Lean name is caught", "invented_theorem_name" in fn)
check("R-C5 an invented number is caught", "98765" in fu)
check("R-C6 a quotation with altered case is caught",
      misquoted(prose_quotes("``under base parameters''"), src) == ["under base parameters"])
check("R-C7 a quotation broken across lines in the source is accepted",
      not misquoted(prose_quotes("``sufficiently large $\\delta$''"), src))

fails = results.count(False)
print("=" * 72)
print(f"RETIREMENT SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
