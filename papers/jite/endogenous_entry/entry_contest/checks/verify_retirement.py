"""R: PROOFS.tex was retired at pass 6. Is everything in it accounted for?

Reads the retired file from git at the commit recorded in RETIREMENT.md, takes
an inventory (theorem-like environments by line, section titles, Lean names,
numbers), and requires each item to be present in the documents that replace
PROOFS.tex or listed in RETIREMENT.md with a reason. Controls check that an
invented item of each kind is caught.
"""
import glob
import pathlib
import re
import subprocess
import sys

HERE = pathlib.Path(__file__).resolve().parent.parent
COMMIT = "7132717f"
results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def inventory(src):
    src = re.sub(r"(?<!\\)%.*", "", src)
    lines = src.split("\n")
    envs = [i + 1 for i, ln in enumerate(lines)
            if re.search(r"\\begin\{(theorem|proposition|lemma|corollary|claim)\}", ln)]
    secs = re.findall(r"\\(?:section|subsection|subsubsection)\{([^}]*)\}", src)
    names = set(re.findall(r"\\texttt\{([A-Za-z0-9_\\']+)\}", src))
    names |= set(re.findall(r"\\obj\{[^}]*\}\{([^}]*)\}", src))
    names = {n.replace("\\_", "_") for n in names}
    names = {n for n in names if "_" in n}
    nums = re.findall(r"(?<![A-Za-z\\{])(\d+(?:[.,]\d+)*(?:/\d+)?)", src)
    nums = {n for n in nums if ("." in n or "/" in n or "," in n) or int(n) >= 10}
    return envs, secs, names, nums


def unaccounted(inv, dest, record):
    envs, secs, names, nums = inv
    dn = dest.replace("\\_", "_")
    rec_rows = set(re.findall(r"^\|\s*([^|]+?)\s*\|", record, re.M))
    rec_ticks = set(re.findall(r"`([^`]+)`", record))
    bad_env = [e for e in envs if str(e) not in rec_rows]
    bad_sec = [t for t in secs if t not in rec_rows]
    bad_name = sorted(n for n in names if n not in dn and n not in rec_ticks)
    bad_num = sorted(n for n in nums
                     if n not in dest and n.replace(",", "{,}") not in dest and n not in rec_rows)
    return bad_env, bad_sec, bad_name, bad_num


print("=" * 72)
print("R  RETIREMENT OF PROOFS.tex: EVERY PART ACCOUNTED FOR")
print("=" * 72)

check("R0 PROOFS.tex is no longer in the working tree", not (HERE / "PROOFS.tex").exists())
record = (HERE / "RETIREMENT.md").read_text()
check("R1 RETIREMENT.md names the commit it reads", COMMIT in record)
p = subprocess.run(["git", "show", f"{COMMIT}:./PROOFS.tex"], cwd=HERE, capture_output=True, text=True)
if p.returncode != 0:
    check("R2 the retired file can be read from git", False, p.stderr.strip()[:200])
    sys.exit(1)
src = p.stdout
check("R2 the retired file can be read from git", True, f"{len(src.splitlines())} lines")

files = ["MANUSCRIPT.tex", "PROOFS_ADDENDUM.tex", "LITERATURE.tex", "MEASUREMENT_MAP.tex",
         "TODO.md", "lean/mathlib/README.md"]
files += [str(pathlib.Path(f).relative_to(HERE)) for f in glob.glob(str(HERE / "lit" / "*" / "*.md"))]
dest = "\n".join((HERE / f).read_text() for f in files)

inv = inventory(src)
bad_env, bad_sec, bad_name, bad_num = unaccounted(inv, dest, record)
print(f"      inventory: {len(inv[0])} environments, {len(inv[1])} sections, "
      f"{len(inv[2])} Lean names, {len(inv[3])} numbers")
check("R3 every theorem-like environment has a destination", not bad_env, f"{bad_env}")
check("R4 every section has a destination", not bad_sec, f"{bad_sec}")
check("R5 every Lean name is carried or recorded", not bad_name, f"{bad_name}")
check("R6 every number is carried or recorded with a reason", not bad_num, f"{bad_num}")

print()
print("CONTROLS  an invented item of each kind must be caught")
fake = src + "\n\\begin{theorem}x\\end{theorem}\n\\section{An invented section}\n" \
             "\\texttt{invented\\_theorem\\_name} 98765\n"
fe, fs, fn, fu = unaccounted(inventory(fake), dest, record)
check("R-C1 an invented environment is caught", len(fe) == 1)
check("R-C2 an invented section is caught", "An invented section" in fs)
check("R-C3 an invented Lean name is caught", "invented_theorem_name" in fn)
check("R-C4 an invented number is caught", "98765" in fu)

fails = results.count(False)
print("=" * 72)
print(f"RETIREMENT SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
