"""NL: no law of motion creeps into Paper A1.

Paper A1 keeps every row that needs no law of motion for a group's standing.
This suite fails if any object of the law of motion appears in the Lean
build roots, in a Lean source, in MANUSCRIPT.tex or in MEASUREMENT_MAP.tex.
Scope statements written as \\outside{...} are exempt, counted, and each
must name Paper A2, where the objects live. Controls check that an injected
object of each kind is caught and that an \\outside statement is exempt.
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
# Strings of the manuscript and the map, matched after comments and
# \outside{...} are removed. Case-insensitive for words, exact for TeX.
TEX_FORBIDDEN = ["law of motion", "composed map", r"\mathcal{H}", "^{t+1}", r"\phi^t",
                 "closure trap", "tipping point", "mobility threshold", "exclusion threshold",
                 "intensity threshold", "fixed point", "basin", "converge", "escape", "frozen",
                 "re-learn", "intergenerational", "generations", "mobility", "exclusion pressure",
                 "cost of exclusion", "convergence rate", "merged root", "spillover",
                 "selection--mutation", r"\mu", r"\tilde\gamma", r"\delta", r"\rho", r"\beta_0",
                 "c_M", "c_L", r"\varepsilon", r"c_{\mathrm{eff}}", r"\eta", r"\theta", "Q("]
results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def strip_outside(tex):
    """Remove comments and every \\outside{...}, returning the rest and the
    list of exempted statements."""
    tex = re.sub(r"(?<!\\)%.*", "", tex)
    stmts = []
    out, i = [], 0
    for m in re.finditer(r"\\outside\{", tex):
        if m.start() < i:
            continue
        depth, j = 1, m.end()
        while j < len(tex) and depth:
            if tex[j] == "\\":
                j += 2
                continue
            depth += tex[j] == "{"
            depth -= tex[j] == "}"
            j += 1
        stmts.append(tex[m.end():j - 1])
        out.append(tex[i:m.start()])
        i = j
    out.append(tex[i:])
    return "".join(out), stmts


def tex_hits(body):
    hits = []
    for s in TEX_FORBIDDEN:
        if s.startswith("\\") or s.endswith("(") or s.startswith("^") or "_" in s:
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
    body, stmts = strip_outside(manuscript)
    h = tex_hits(body)
    out["NL-3"] = (not h, f"manuscript names {h}")
    body_m, stmts_m = strip_outside(mmap)
    h = tex_hits(body_m)
    out["NL-4"] = (not h, f"map names {h}")
    allst = stmts + stmts_m
    unnamed = [s[:50] for s in allst if "Paper A2" not in re.sub(r"\s+", " ", s)]
    out["NL-5"] = (not unnamed, f"scope statements not naming Paper A2 {unnamed}")
    return out, len(allst)


print("=" * 72)
print("NL  NO LAW OF MOTION IN PAPER A1")
print("=" * 72)
lakefile = open(os.path.join(LEAN_DIR, "lakefile.toml")).read()
sources = {os.path.basename(f): open(f).read() for f in glob.glob(os.path.join(LEAN_DIR, "*.lean"))}
manuscript = open(os.path.join(ROOT, "MANUSCRIPT.tex")).read()
mmap = open(os.path.join(ROOT, "MEASUREMENT_MAP.tex")).read()
real, n = analyse(lakefile, sources, manuscript, mmap)
print(f"      Lean sources: {len(sources)}; scope statements exempted: {n}")
labels = {"NL-1": "no build root is a module of the law of motion",
          "NL-2": "no Lean source names a module or declaration of the law of motion",
          "NL-3": "the manuscript names no object of the law of motion outside a scope statement",
          "NL-4": "the measurement map names no object of the law of motion outside a scope statement",
          "NL-5": "every scope statement names Paper A2"}
for k in sorted(real):
    ok, d = real[k]
    check(f"{k} {labels[k]}", ok, "" if ok else d)

print("-" * 72)
print("CONTROLS  an injected object of each kind must be caught, and a scope statement exempted")
G_LAKE = 'roots = ["Sufficiency", "Passing"]'
B_LAKE = 'roots = ["Sufficiency", "ClosureMap"]'
G_SRC = {"A.lean": "theorem t : 1 = 1 := rfl -- Hf in a comment is fine\n/- ClosureMap in a block comment -/"}
B_SRC = {"A.lean": "import ClosureMap\ntheorem t : Hf = Hf := rfl"}
G_TEX = r"The prior moves. \outside{The composed map and the tipping point are in Paper A2.} % law of motion in a comment"
B_TEX = r"The composed map $\mathcal{H}$ has a fixed point. \outside{It converges.}"
good, ng = analyse(G_LAKE, G_SRC, G_TEX, G_TEX)
bad, nb = analyse(B_LAKE, B_SRC, B_TEX, B_TEX)
for k in sorted(good):
    check(f"{k}-control passes on the well-formed sample", good[k][0], "" if good[k][0] else good[k][1])
    check(f"{k}-control fails on the faulty sample", not bad[k][0])
check("NL-C1 scope statements are counted", ng == 2)

fails = results.count(False)
print("=" * 72)
print(f"NO LAW OF MOTION SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
