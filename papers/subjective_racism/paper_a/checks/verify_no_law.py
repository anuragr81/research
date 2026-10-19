"""NL: no law of motion creeps into Paper A1.

Paper A1 keeps every row that needs no law of motion for a group's standing.
This suite fails if any object of the law of motion appears in the Lean
build roots, in a Lean source, in skeleton_ClassMarkers.tex, in ledger_picture.tex (the figure it includes)
or in MEASUREMENT_MAP.tex, outside TeX comments. It also fails if either TeX file, comments included,
names the companion paper or uses a scope-statement macro, since Paper A1
does not point to other papers. Controls check that an injected object of
each kind is caught.
"""
import glob
import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
LEAN_DIR = os.path.join(ROOT, "lean", "mathlib")

# Lean modules and declarations of the law of motion, as whole words.
LEAN_FORBIDDEN = ["ClosureMap", "FrozenMap", "Bifurcation", "Outsider", "GeneralF", "Intensity",
                  "Slowing", "MeasurementMap", "Figures", "CoateLoury1993", "Arrow1971",
                  "Hf", "Hk", "gammaStar", "bStar", "rhoStar", "cEff", "etaCrit",
                  "tokenism_not_implied", "law"]
# Strings of the manuscript and the map, matched after comments are removed. Case-insensitive for words, exact for TeX.
TEX_FORBIDDEN = ["law of motion", "composed map", r"\mathcal{H}", "^{t+1}", r"\phi^t",
                 "closure trap", "tipping point", "mobility threshold", "exclusion threshold",
                 "intensity threshold", "fixed point", "basin", "converge", "escape", "frozen",
                 "re-learn", "intergenerational", "generations", "mobility", "exclusion pressure",
                 "cost of exclusion", "convergence rate", "merged root", "spillover",
                 "selection--mutation", r"\mu", r"\tilde\gamma", r"\delta", r"\rho", r"\beta_0",
                 "c_M", "c_L", r"\varepsilon", r"c_{\mathrm{eff}}", r"\eta", r"\theta", "Q("]
# Strings that point to the companion paper, matched in the whole file.
POINTERS = ["Paper A2", "paper_a2", r"\outside"]
results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def strip_comments(tex):
    return re.sub(r"(?<!\\)%.*", "", tex)


def pointer_hits(tex):
    flat = re.sub(r"\s+", " ", tex)
    return [s for s in POINTERS if s in flat]


def tex_hits(body):
    hits = []
    for s in TEX_FORBIDDEN:
        if s.startswith("\\") and s[1:].isalpha():
            # a whole control word, so \mu does not match \multicolumn
            found = re.search(re.escape(s) + r"(?![A-Za-z])", body) is not None
        elif s.startswith("\\") or s.endswith("(") or s.startswith("^") or "_" in s:
            found = s in body
        else:
            found = re.search(r"(?i)\b" + re.escape(s), body) is not None
        if found:
            hits.append(s)
    return hits


def lean_hits(src):
    src = re.sub(r"/-.*?-/", "", src, flags=re.S)
    src = re.sub(r"--.*", "", src)
    return [w for w in LEAN_FORBIDDEN if re.search(r"(?<![A-Za-z0-9_.'])" + re.escape(w) + r"(?![A-Za-z0-9_'])", src)]


def analyse(lakefile, sources, manuscript, mmap):
    out = {}
    roots = re.findall(r"roots\s*=\s*\[([^\]]*)\]", lakefile)
    names = re.findall(r'"([^"]+)"', " ".join(roots))
    bad = [n for n in names if n in LEAN_FORBIDDEN]
    out["NL-1"] = (bool(names) and not bad, f"forbidden build roots {bad}")
    badsrc = {f: h for f, h in ((f, lean_hits(s)) for f, s in sources.items()) if h}
    out["NL-2"] = (not badsrc, f"Lean sources naming the law of motion {badsrc}")
    h = tex_hits(strip_comments(manuscript))
    out["NL-3"] = (not h, f"manuscript names {h}")
    h = tex_hits(strip_comments(mmap))
    out["NL-4"] = (not h, f"map names {h}")
    p = pointer_hits(manuscript) + pointer_hits(mmap)
    out["NL-5"] = (not p, f"pointers to the companion paper {p}")
    return out


print("=" * 72)
print("NL  NO LAW OF MOTION IN PAPER A1")
print("=" * 72)
lakefile = open(os.path.join(LEAN_DIR, "lakefile.toml")).read()
sources = {os.path.basename(f): open(f).read() for f in glob.glob(os.path.join(LEAN_DIR, "*.lean"))}
manuscript = open(os.path.join(ROOT, "skeleton_ClassMarkers.tex")).read() + "\n" + open(os.path.join(ROOT, "ledger_picture.tex")).read()
mmap = open(os.path.join(ROOT, "MEASUREMENT_MAP.tex")).read()
real = analyse(lakefile, sources, manuscript, mmap)
print(f"      Lean sources: {len(sources)}")
labels = {"NL-1": "no build root is a module of the law of motion",
          "NL-2": "no Lean source names a module or declaration of the law of motion",
          "NL-3": "the manuscript names no object of the law of motion",
          "NL-4": "the measurement map names no object of the law of motion",
          "NL-5": "neither TeX file names the companion paper, even in a comment"}
for k in sorted(real):
    ok, d = real[k]
    check(f"{k} {labels[k]}", ok, "" if ok else d)

print("-" * 72)
print("CONTROLS  an injected object of each kind must be caught")
G_LAKE = 'roots = ["Sufficiency", "Passing"]'
B_LAKE = 'roots = ["Sufficiency", "ClosureMap"]'
G_SRC = {"A.lean": "theorem t : 1 = 1 := rfl -- Hf in a comment is fine\n/- ClosureMap in a block comment -/"}
B_SRC = {"A.lean": "import ClosureMap\ntheorem t : Hf = Hf := rfl"}
G_TEX = r"The prior moves within one generation. % law of motion in a comment"
B_TEX = "The composed map $\\mathcal{H}$ has a fixed point, as in Paper\nA2."
good = analyse(G_LAKE, G_SRC, G_TEX, G_TEX)
bad = analyse(B_LAKE, B_SRC, B_TEX, B_TEX)
for k in sorted(good):
    check(f"{k}-control passes on the well-formed sample", good[k][0], "" if good[k][0] else good[k][1])
    check(f"{k}-control fails on the faulty sample", not bad[k][0])
check("NL-C1 a pointer in a comment is caught", pointer_hits("% see paper_a2") == ["paper_a2"])

fails = results.count(False)
print("=" * 72)
print(f"NO LAW OF MOTION SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
