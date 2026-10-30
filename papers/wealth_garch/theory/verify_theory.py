import glob
import hashlib
import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
PROJ = os.path.join(HERE, "lean")
ORIGIN = os.path.join(PROJ, "ORIGIN.md")
results = []


def check(name, ok, detail=""):
    results.append((name, bool(ok)))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def declarations(path):
    names, ns = [], []
    for ln in open(path):
        m = re.match(r"\s*namespace\s+([A-Za-z0-9_.']+)", ln)
        if m:
            ns.append(m.group(1))
            continue
        m = re.match(r"\s*end\s+([A-Za-z0-9_.']+)\s*$", ln)
        if m and ns and ns[-1] == m.group(1):
            ns.pop()
            continue
        m = re.match(r"\s*(?:theorem|lemma)\s+([^\s:(\[{]+)", ln)
        if m:
            names.append(".".join(ns + [m.group(1)]))
    return names


def cited(tex):
    names = []
    for ln in tex.splitlines():
        if ln.startswith(r"\mrow{"):
            m = re.search(r"\{([^{}]*)\}\s*$", ln)
            if m:
                names += [n.strip() for n in m.group(1).split(",") if n.strip()]
    names += [n for n in re.findall(r"\\lean\{([A-Za-z0-9_.']+)\}", tex)]
    return names


def origin_pins(origin_md):
    return dict(re.findall(r"\|\s*`([A-Za-z0-9_]+\.lean)`\s*\|\s*`([0-9a-f]{64})`\s*\|", origin_md))


def drifted(proj, pins):
    """Files whose sha256 is not their pin in ORIGIN.md, files without a pin, and pins without a file."""
    bad = []
    present = {os.path.basename(f) for f in glob.glob(os.path.join(proj, "*.lean"))}
    for name in sorted(present | set(pins)):
        f = os.path.join(proj, name)
        now = hashlib.sha256(open(f, "rb").read()).hexdigest() if os.path.exists(f) else None
        if now != pins.get(name):
            bad.append(name)
    return bad


print("=" * 72)
print("THEORY PAPER (theory/lean/)")
print("=" * 72)

files = sorted(glob.glob(os.path.join(PROJ, "*.lean")))
declared = {n for f in files for n in declarations(f)}
pins = origin_pins(open(ORIGIN).read()) if os.path.exists(ORIGIN) else {}
print(f"      files {[os.path.basename(f) for f in files]}")
print(f"      theorems declared {len(declared)}; files pinned in ORIGIN.md {len(pins)}")
check("TH-1 every theory file matches its sha256 pin in theory/lean/ORIGIN.md, and every pin has a file",
      files and pins and not drifted(PROJ, pins), f"{drifted(PROJ, pins)}")

THEORY_TEX = os.path.join(HERE, "wealth_garch_theory.tex")
LIT = os.path.join(HERE, "lit")


def lit_lean(lit):
    """Map each bib key in lit/*/KEYS to the Lean files in that record's LEAN."""
    out = {}
    for d in sorted(glob.glob(os.path.join(lit, "*", "KEYS"))):
        rec = os.path.dirname(d)
        lean = os.path.join(rec, "LEAN")
        files = open(lean).read().split() if os.path.exists(lean) else []
        for k in open(d).read().split():
            out[k] = files
    return out


def cited_keys(tex):
    return sorted({k.strip() for g in re.findall(r"\\cite[pt]?\{([^}]*)\}", tex)
                   for k in g.split(",") if k.strip() and not k.strip().startswith("#")}
                  | set(re.findall(r"^\\lrow\{[^{}]*\}\{([^{}]*)\}", tex, re.M)))


keys = cited_keys(open(THEORY_TEX).read())
lmap = lit_lean(LIT)
present = {os.path.basename(f) for f in files}
gaps = [k for k in keys if not lmap.get(k) or any(f not in present for f in lmap[k])]
print(f"      papers cited by theory/wealth_garch_theory.tex {keys}")
check("TH-4 every paper cited by theory/wealth_garch_theory.tex has a record in theory/lit whose Lean file is in theory/lean",
      keys and not gaps, f"gaps: {gaps}" if gaps else "")

tnames = [n for n in cited(open(THEORY_TEX).read()) if "#" not in n]
tmissing = sorted(set(tnames) - declared)
print(f"      Lean names cited by theory/wealth_garch_theory.tex {len(set(tnames))}")
check("TH-5 every Lean name cited by theory/wealth_garch_theory.tex is declared in theory/lean",
      tnames and not tmissing, f"missing: {tmissing}" if tmissing else "")

print("-" * 72)
print("CONTROLS")
check("TH-C1 a fabricated citation is detected as missing",
      "RateBased.no_such_theorem" not in declared)
check("TH-C2 a row's last brace group is read as its Lean names",
      cited(r"\mrow{M0}{a {b} c}{$x^{2}$}{A.one, B.two}") == ["A.one", "B.two"])
check("TH-C5 a file whose sha256 is not its pin, and a pin without a file, are both drift",
      drifted(PROJ, dict(pins, **{"Envelope.lean": "0" * 64, "NoSuchFile.lean": "1" * 64})) == ["Envelope.lean", "NoSuchFile.lean"])

SOLVE = os.path.join(HERE, "solve")


def pin_problems(results_text, folder):
    """Every `name` sha256 <hex> pin of a results file must match the file now."""
    bad = []
    pins = re.findall(r"`([^`]+)` sha256 ([0-9a-f]{64})", results_text)
    for name, digest in pins:
        f = os.path.join(folder, name)
        now = hashlib.sha256(open(f, "rb").read()).hexdigest() if os.path.exists(f) else None
        if now != digest:
            bad.append(name)
    return pins, bad


spins, badpins = [], []
for res in ("results_solve.md", "results_noborrow.md"):
    res_md = os.path.join(SOLVE, res)
    p_, b_ = pin_problems(open(res_md).read(), SOLVE) if os.path.exists(res_md) else ([], [res])
    spins += p_
    badpins += b_ if p_ or b_ else [res]
check("TH-6 the solve's results files are pinned to the scripts as they now are (theory/solve/)",
      spins and not badpins, f"stale or missing: {badpins}")

check("TH-C3 a cited paper without a lit record is a gap",
      not lmap.get("nosuchpaper1999") and cited_keys("\\citet{a1999, b2000}\\citep{c2001}\n\\lrow{L1}{d2002}{x}") == ["a1999", "b2000", "c2001", "d2002"])

check("TH-C4 a pin that does not match its file is caught",
      pin_problems("`solve_qvi.py` sha256 " + "0" * 64, SOLVE)[1] == ["solve_qvi.py"])

print("-" * 72)
p = subprocess.run([sys.executable, os.path.join(HERE, "checks", "verify_mathlib.py"), PROJ])
check("TH-3 build, sorry and axiom audit of theory/lean (checks/verify_mathlib.py)", p.returncode == 0)

nf = sum(1 for _, ok in results if not ok)
print("=" * 72)
print(f"THEORY SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
sys.exit(1 if nf else 0)
