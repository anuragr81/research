"""Checks MEASUREMENT_MAP.tex, the companion note that names every object of
the model. Rules MM-1 to MM-9, each run on the note and on a well-formed and
a faulty sample.

MM-1  every \\mm row has six non-empty fields and a plain-English name
MM-2  names are unique, one name per object
MM-3  every term of the manuscript's Terminology appendix is a name in the map
MM-4  every "Obtained how" entry begins with one of the six status words
MM-5  no colon in the prose of the note outside mathematics
MM-6  every Lean theorem the note names is an audited declaration
MM-7  every row says Yes or No under Directly observed, Yes exactly when its
      last column begins with "Recorded"
MM-8  no symbol names two rows
MM-9  every symbol in the manuscript's table of assumptions is a symbol of
      the map, and every letter in an assumption's mathematics belongs to a
      symbol the assumption lists
"""
import glob
import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
STATUS = ("Recorded", "Elicited", "Computed", "Assumed", "Never observed", "A device")
results = []


def check(name, ok, detail=""):
    results.append((name, bool(ok)))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def strip_comments(tex):
    return re.sub(r"(?<!\\)%.*", "", tex)


def brace_group(s, i):
    while i < len(s) and s[i].isspace():
        i += 1
    if i >= len(s) or s[i] != "{":
        return None, i
    depth, j = 0, i
    while j < len(s):
        if s[j] == "\\":
            j += 2
            continue
        if s[j] == "{":
            depth += 1
        elif s[j] == "}":
            depth -= 1
            if depth == 0:
                return s[i + 1:j], j + 1
        j += 1
    return None, i


def macro_calls(tex, name, nargs):
    calls = []
    for m in re.finditer(rf"\\{name}(?![A-Za-z])", tex):
        i, args = m.end(), []
        for _ in range(nargs):
            a, i = brace_group(tex, i)
            if a is None:
                break
            args.append(a)
        if len(args) == nargs:
            calls.append(args)
    return calls


def lean_declarations():
    names = set()
    files = glob.glob(os.path.join(ROOT, "lean", "*.lean"))
    files += glob.glob(os.path.join(ROOT, "lean", "mathlib", "*.lean"))
    for f in files:
        ns = []
        for ln in open(f):
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
                names.add(".".join(ns + [m.group(1)]))
    return names


def manuscript_terms(tex):
    tex = strip_comments(tex)
    m = re.search(r"\\section\{Terminology\}(.*?)\\section\{", tex, re.S)
    body = m.group(1) if m else ""
    terms = []
    for ln in body.splitlines():
        m = re.match(r"^([A-Z][A-Za-z\- ]*?) & ", ln)
        if m:
            terms.append(m.group(1).strip())
    return terms


GREEK = ("alpha beta gamma delta varepsilon epsilon zeta eta theta kappa lambda mu nu xi pi "
         "rho sigma tau phi varphi chi psi omega Gamma Delta Theta Lambda Pi Sigma Phi Psi Omega").split()
# The generation index of a sequence, used inside assumptions.
INDICES = {"t"}


def symbol_keys(cell):
    """The symbols a map row names. A segment `symbol = formula` names its left
    side. Any other segment, such as the value $\\bar c=1$, is its own key."""
    keys = []
    for seg in re.findall(r"\$([^$]*)\$", cell):
        if "=" in seg:
            lhs, rhs = seg.split("=", 1)
            if re.search(r"[A-Za-z]", rhs):
                seg = lhs
        keys.append(re.sub(r"\s+", "", seg))
    return keys


def letters(math):
    """The identifiers in a stretch of mathematics, with decorations dropped."""
    toks = re.findall(r"\\math(?:rm|cal)\{[^}]*\}|\\[A-Za-z]+|[A-Za-z]", math)
    return {t for t in toks if not t.startswith("\\") or t.startswith("\\math") or t[1:] in GREEK}


def assumption_rows(tex):
    return macro_calls(strip_comments(tex), "arow", 5)


def assumption_problems(arows, keys):
    unknown, stray = [], []
    for r in arows:
        listed = re.findall(r"\$([^$]*)\$", r[2])
        unknown += [(r[0], x) for x in listed if re.sub(r"\s+", "", x) not in keys]
        allowed = set().union(*[letters(x) for x in listed]) if listed else set()
        used = set().union(*[letters(x) for x in re.findall(r"\$([^$]*)\$", r[1])] or [set()])
        stray += [(r[0], t) for t in sorted(used - allowed - INDICES)]
    return unknown, stray


def prose_only(body):
    body = re.sub(r"\\lean\{[^{}]*\}", " ", body)
    body = re.sub(r"\\\[.*?\\\]", " ", body, flags=re.S)
    body = re.sub(r"\$[^$]*\$", " ", body)
    return body


def analyse(tex, terms, lean, arows):
    tex = strip_comments(tex)
    out = {}
    rows = macro_calls(tex, "mm", 6)
    bad1 = [r[1] for r in rows if any(not x.strip() for x in r)
            or "$" in r[1] or "\\" in r[1]]
    out["MM-1"] = (bool(rows) and not bad1, f"faulty rows {bad1}")
    names = [r[1].strip() for r in rows]
    dup = sorted({n for n in names if names.count(n) > 1})
    out["MM-2"] = (not dup, f"names used twice {dup}")
    low = {n.lower() for n in names}
    missing = [t for t in terms if t.lower() not in low]
    out["MM-3"] = (not missing, f"manuscript terms absent from the map {missing}")
    bad4 = [r[1] for r in rows if not r[5].strip().startswith(STATUS)]
    out["MM-4"] = (not bad4, f"rows whose last column has no status word {bad4}")
    doc = tex.split(r"\begin{document}", 1)[-1]
    colons = [ln.strip() for ln in prose_only(doc).splitlines() if ":" in ln]
    out["MM-5"] = (not colons, f"lines with a colon {colons[:3]}")
    cited = {c[0].replace("\\_", "_").strip() for c in macro_calls(tex, "lean", 1)}
    unknown = sorted(c for c in cited if c not in lean)
    out["MM-6"] = (not unknown, f"Lean names not declared {unknown}")
    bad7 = [r[1] for r in rows if r[4].strip() not in ("Yes", "No")
            or (r[4].strip() == "Yes") != r[5].strip().startswith("Recorded")]
    out["MM-7"] = (not bad7, f"rows whose Directly observed does not match {bad7}")
    owner = {}
    for r in rows:
        for k in symbol_keys(r[0]):
            owner.setdefault(k, []).append(r[1].strip())
    clash = {k: v for k, v in owner.items() if len(v) > 1}
    out["MM-8"] = (not clash, f"symbols naming two rows {clash}")
    unknown_a, stray = assumption_problems(arows, set(owner))
    out["MM-9"] = (bool(arows) and not unknown_a and not stray,
                   f"assumption symbols absent from the map {unknown_a}; letters not listed {stray}")
    return out, len(rows)


FIX_LEAN = {"Fixture.flat_posterior"}
FIX_TERMS_GOOD = ["Prior"]
FIX_TERMS_BAD = ["Ghost term"]

GOOD = r"""
\begin{document}
\mm{$\pi_g(c)$}{Prior}{A pmf.}{The belief before the marker.}{No}{Elicited from the observer.}
\mm{$Z_g=\sum_c\ell_g(c)\pi_g(c)$}{Marginal}{A real number.}{The marker's frequency.}{No}{Computed from $\ell_g$ and $\pi_g$.}
\mm{$m$}{Marker}{A signal.}{A visible trait.}{Yes}{Recorded at the encounter.}
The formula is a theorem (\lean{Fixture.flat\_posterior}) and $a:b$ is a ratio.
\end{document}
"""

BAD = r"""
\begin{document}
\mm{$\pi_g(c)$}{Prior}{A pmf.}{The belief before the marker.}{Yes}{Maybe elicited.}
\mm{$\pi_g(c)$}{Prior}{}{The marker's frequency.}{No}{Computed from records.}
The formula is a theorem (\lean{Fixture.no\_such}) and here is a colon: in prose.
\end{document}
"""

FIX_A_GOOD = [["A1", r"The marker $m$ moves the prior $\pi_g(c)$ through $Z_g$.",
               r"$m$, $\pi_g(c)$, $Z_g$", "M1", "r"]]
FIX_A_BAD = [["A1", r"The prior $\pi_g(c)$ is scaled by $\delta$.",
              r"$\pi_g(c)$, $\theta$", "M1", "r"]]

lean = lean_declarations()
print("=" * 72)
print("MEASUREMENT MAP (MEASUREMENT_MAP.tex)")
print("=" * 72)
tex = open(os.path.join(ROOT, "MEASUREMENT_MAP.tex")).read()
mtex = open(os.path.join(ROOT, "MANUSCRIPT.tex")).read()
terms = manuscript_terms(mtex)
arows = assumption_rows(mtex)
real, nrows = analyse(tex, terms, lean, arows)
print(f"      rows in the map: {nrows}; terms in the manuscript's Terminology appendix: {len(terms)}; "
      f"assumptions: {len(arows)}")
names = {"MM-1": "every row has six non-empty fields and a plain-English name",
         "MM-2": "one name per object",
         "MM-3": "every manuscript term is a name in the map",
         "MM-4": "every last column begins with a status word",
         "MM-5": "no colon in the prose",
         "MM-6": "every Lean name the note cites is declared",
         "MM-7": "Directly observed is Yes exactly for recorded objects",
         "MM-8": "no symbol names two rows",
         "MM-9": "every symbol of every assumption is a symbol of the map"}
for k in sorted(real):
    ok, detail = real[k]
    check(f"{k} {names[k]}", ok, "" if ok else detail)

print("-" * 72)
print("CONTROLS  a well-formed sample must pass every rule; a faulty one must fail every rule")
good, _ = analyse(GOOD, FIX_TERMS_GOOD, FIX_LEAN, FIX_A_GOOD)
bad, _ = analyse(BAD, FIX_TERMS_BAD, FIX_LEAN, FIX_A_BAD)
for k in sorted(good):
    check(f"{k}-control passes on the well-formed sample", good[k][0], "" if good[k][0] else good[k][1])
    check(f"{k}-control fails on the faulty sample", not bad[k][0])

nf = sum(1 for _, ok in results if not ok)
print("=" * 72)
print(f"MEASUREMENT MAP SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
sys.exit(1 if nf else 0)
