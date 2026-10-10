import os
import re
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
COMMIT = "1d1a56f5"
RETIRED = ["00_document/PROOFS_v2.tex", "00_document/PROOFS_v2.pdf",
           "00_reader/proof_registry.py", "00_reader/check_citations.py"]
results = []


def check(name, ok, detail=""):
    results.append((name, bool(ok)))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def git_source():
    prefix = subprocess.run(["git", "rev-parse", "--show-prefix"], cwd=ROOT,
                            capture_output=True, text=True).stdout.strip()
    p = subprocess.run(["git", "show", f"{COMMIT}:{prefix}00_document/PROOFS_v2.tex"], cwd=ROOT,
                       capture_output=True, text=True)
    return p.returncode == 0, p.stdout


def extract(src):
    labels = re.findall(r"\\label\{([^}]*)\}", src)
    sections = re.findall(r"^\\(?:sub)?section\*?\{(.*)\}", src, re.M)
    numbers = sorted(set(re.findall(r"(?<![\w.])(\d+\.\d+)(?![\w.])", src)))
    lean = sorted({x.replace("\\_", "_") for x in re.findall(r"\\texttt\{([^}]*)\}", src)
                   if re.fullmatch(r"[A-Za-z0-9_\\]+\.lean|[a-z][a-z0-9\\_]*_[a-z0-9\\_]+", x)})
    return labels, sections, numbers, lean


def table(md, heading):
    m = re.search(rf"^## {re.escape(heading)}\s*$(.*?)(?=^## |\Z)", md, re.M | re.S)
    rows = []
    for ln in (m.group(1).splitlines() if m else []):
        if ln.startswith("|") and not re.match(r"^\|\s*-", ln):
            rows.append([c.strip() for c in ln.strip().strip("|").split("|")])
    return rows[1:]


def homes_exist(home, row_ids):
    bad = []
    for part in [h.strip() for h in home.split(",") if h.strip()]:
        if re.fullmatch(r"[KMLC][1-9][0-9]*", part):
            if part not in row_ids:
                bad.append(part)
        elif re.fullmatch(r"App\. [A-E]", part) or part in {"illustration", "TODO.md", "VERIFICATION.md"}:
            continue
        elif not os.path.exists(os.path.join(ROOT, part)):
            bad.append(part)
    return bad


def audit(src, md, row_ids):
    labels, sections, numbers, lean = extract(src)
    out = {}
    lab = {r[0].strip("`"): r[1] for r in table(md, "Labels")}
    sec = {r[0]: r[1] for r in table(md, "Sections")}
    num = {r[0]: r[3] for r in table(md, "Numbers")}
    lea = {r[0].strip("`"): r[1] for r in table(md, "Lean names")}
    out["R1 every label has a home"] = [l for l in labels if not lab.get(l)]
    out["R2 every section has a home"] = [s for s in sections if not sec.get(s.replace("|", "/"))]
    out["R3 every decimal number has a home"] = [n for n in numbers if not num.get(n)]
    out["R4 every Lean name has a home"] = [x for x in lean if not lea.get(x)]
    bad = []
    for home in list(lab.values()) + list(sec.values()) + list(num.values()):
        bad += homes_exist(home, row_ids)
    for target in lea.values():
        m = re.search(r"\((M[0-9]+)\)", target)
        if m and m.group(1) not in row_ids:
            bad.append(m.group(1))
        if target.startswith("lean/") and not os.path.exists(os.path.join(ROOT, target)):
            bad.append(target)
    out["R5 every home named exists"] = sorted(set(bad))
    return out, (len(labels), len(sections), len(numbers), len(lean))


print("=" * 72)
print(f"RETIREMENT OF PROOFS_v2.tex (read from git at {COMMIT})")
print("=" * 72)
ok, src = git_source()
check("R0 the retired file is readable from git at the pinned commit", ok and src)
md = open(os.path.join(ROOT, "RETIREMENT.md")).read()
tex = open(os.path.join(ROOT, "MANUSCRIPT.tex")).read()
row_ids = set(re.findall(r"\\[kmlc]row\{([KMLC][0-9]+)\}", tex))
if ok:
    res, counts = audit(src, md, row_ids)
    print(f"      labels {counts[0]}, sections {counts[1]}, numbers {counts[2]}, Lean names {counts[3]}")
    for k, missing in res.items():
        check(k, not missing, f"{missing}" if missing else "")
present = [p for p in RETIRED if os.path.exists(os.path.join(ROOT, p))]
check("R6 the retired files are gone from the tree", not present, f"{present}" if present else "")

print("-" * 72)
print("CONTROLS")
fake = src + "\n\\label{prop:fabricated}\n\\section{A fabricated section}\nworth 9.8765 \\texttt{fake\\_lemma}\n"
res, _ = audit(fake, md, row_ids)
check("R-control a fabricated label, section, number and Lean name are all caught",
      "prop:fabricated" in res["R1 every label has a home"]
      and "A fabricated section" in res["R2 every section has a home"]
      and "9.8765" in res["R3 every decimal number has a home"]
      and "fake_lemma" in res["R4 every Lean name has a home"])
bad_md = md.replace("| `thm:lambda4` | M1 |", "| `thm:lambda4` | M99 |")
res, _ = audit(src, bad_md, row_ids)
check("R-control a home naming a row that does not exist is caught",
      "M99" in res["R5 every home named exists"])

nf = sum(1 for _, ok in results if not ok)
print("=" * 72)
print(f"RETIREMENT SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
sys.exit(1 if nf else 0)
