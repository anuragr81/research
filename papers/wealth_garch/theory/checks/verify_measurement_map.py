"""Checks the measurement map, Appendix app:terms of theory/MANUSCRIPT.tex,
which names every object of the theory paper's model and is its terminology.
Rules MM-1 to MM-11, each run on the map and on a well-formed and a faulty
sample. MM-1 to MM-10 are Paper A's (papers/subjective_racism/paper_a/checks/
verify_measurement_map.py), with the Lean declarations read from theory/lean.
MM-11 is this bundle's, since the three papers share the parent's terminology
(MEASUREMENT_MAP.tex, the Terminology table).

MM-1  every \\mm row has six non-empty fields and a plain-English name
MM-2  names are unique, one name per object
MM-3  the map is the manuscript's only terminology, with no second table of terms
MM-4  every "Obtained how" entry begins with one of the six status words
MM-5  no colon in the prose of the map outside mathematics
MM-6  every Lean theorem the map names is an audited declaration
MM-7  every row says Yes or No under Directly observed, Yes exactly when its
      last column begins with "Recorded"
MM-8  no symbol names two rows
MM-9  every symbol in the manuscript's table of assumptions is a symbol of
      the map, and every letter in an assumption's mathematics belongs to a
      symbol the assumption lists
MM-10 every assumption's plain-English reading has no mathematics beyond the
      Lean theorems it cites, names the map entry of every symbol the
      assumption lists, and cites only declared theorems
MM-11 every name of the map is an Agreed or In use name of the parent
      MEASUREMENT_MAP.tex, or one of the names PROPOSED below to the author
"""
import glob
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
PARENT = os.path.dirname(ROOT)
STATUS = ("Recorded", "Elicited", "Computed", "Assumed", "Never observed", "A device")
# Names the theory paper needs that the parent's terminology table does not
# yet carry, offered to the author (TODO.md). Each is one parameter of the cap
# geometry, or the value function.
PROPOSED = {"Solvency floor", "Run-off rate", "Liquidity haircut", "Value function"}
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
    for f in glob.glob(os.path.join(ROOT, "lean", "*.lean")):
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


def parent_terms():
    """The Agreed and In use names of the parent map's Terminology table."""
    tex = strip_comments(open(os.path.join(PARENT, "MEASUREMENT_MAP.tex")).read())
    return {r[0].strip().lower() for r in macro_calls(tex, "nrow", 5) if r[3].strip() in ("Agreed", "In use")}


MAP_HEAD = r"\section{Terminology and measurement map}"


def map_section(tex):
    i = tex.find(MAP_HEAD)
    if i < 0:
        return ""
    j = tex.find(r"\section{", i + len(MAP_HEAD))
    return "\\begin{document}\n" + tex[i:j if j >= 0 else len(tex)] + "\n\\end{document}\n"


def other_glossaries(tex):
    tex = strip_comments(tex)
    i = tex.find(MAP_HEAD)
    j = tex.find(r"\section{", i + len(MAP_HEAD)) if i >= 0 else -1
    rest = tex if i < 0 else tex[:i] + (tex[j:] if j >= 0 else "")
    return re.findall(r"\\textbf\{Term\}|^Term & Symbol", rest, re.M)


GREEK = ("alpha beta gamma delta varepsilon epsilon zeta eta theta kappa lambda mu nu xi pi "
         "rho sigma tau phi varphi chi psi omega Gamma Delta Theta Lambda Pi Sigma Phi Psi Omega").split()
INDICES = set()


def symbol_keys(cell):
    keys = []
    for seg in re.findall(r"\$([^$]*)\$", cell):
        if "=" in seg:
            lhs, rhs = seg.split("=", 1)
            if re.search(r"[A-Za-z]", rhs):
                seg = lhs
        keys.append(re.sub(r"\s+", "", seg))
    return keys


def letters(math):
    toks = re.findall(r"\\math(?:rm|cal)\{[^}]*\}|\\[A-Za-z]+|[A-Za-z]", math)
    return {t for t in toks if not t.startswith("\\") or t.startswith("\\math") or t[1:] in GREEK}


def assumption_rows(tex):
    return macro_calls(strip_comments(tex), "arow", 6)


def assumption_problems(arows, keys):
    unknown, stray = [], []
    for r in arows:
        listed = re.findall(r"\$([^$]*)\$", r[3])
        unknown += [(r[0], x) for x in listed if re.sub(r"\s+", "", x) not in keys]
        allowed = set().union(*[letters(x) for x in listed]) if listed else set()
        used = set().union(*[letters(x) for x in re.findall(r"\$([^$]*)\$", r[1])] or [set()])
        stray += [(r[0], t) for t in sorted(used - allowed - INDICES)]
    return unknown, stray


def plain_problems(arows, owner, lean=frozenset()):
    maths, unnamed, undeclared = [], [], []
    for r in arows:
        cited = [c[0].replace("\\_", "_").strip() for c in macro_calls(r[2], "lean", 1)]
        undeclared += [(r[0], c) for c in cited if c not in lean]
        plain = re.sub(r"\s+", " ", re.sub(r"\\lean\{[^{}]*\}", " ", r[2]))
        if "$" in plain or re.search(r"\\[A-Za-z]", plain):
            maths.append(r[0])
        for x in re.findall(r"\$([^$]*)\$", r[3]):
            for name in owner.get(re.sub(r"\s+", "", x), []):
                if name.lower() not in plain.lower():
                    unnamed.append((r[0], name))
    return maths, unnamed, undeclared


def prose_only(body):
    body = re.sub(r"\\lean\{[^{}]*\}", " ", body)
    body = re.sub(r"\\(?:label|ref|pageref|eqref|path|cite[tp]?)\{[^{}]*\}", " ", body)
    body = re.sub(r"\\\[.*?\\\]", " ", body, flags=re.S)
    body = re.sub(r"\$[^$]*\$", " ", body)
    return body


def analyse(tex, terms, lean, arows, agreed):
    tex = strip_comments(tex)
    out = {}
    rows = macro_calls(tex, "mm", 6)
    bad1 = [r[1] for r in rows if any(not x.strip() for x in r) or "$" in r[1] or "\\" in r[1]]
    out["MM-1"] = (bool(rows) and not bad1, f"faulty rows {bad1}")
    names = [r[1].strip() for r in rows]
    dup = sorted({n for n in names if names.count(n) > 1})
    out["MM-2"] = (not dup, f"names used twice {dup}")
    out["MM-3"] = (bool(rows) and not terms, f"second tables of terms {terms}")
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
    maths, unnamed, undeclared = plain_problems(arows, owner, lean)
    out["MM-10"] = (bool(arows) and not maths and not unnamed and not undeclared,
                    f"readings with mathematics {maths}; map names missing {unnamed}; "
                    f"Lean names not declared {undeclared}")
    foreign = sorted(n for n in names if n.lower() not in agreed and n not in PROPOSED)
    out["MM-11"] = (bool(rows) and not foreign, f"names neither agreed in the parent map nor proposed {foreign}")
    return out, len(rows)


FIX_LEAN = {"Fixture.flat_posterior"}
FIX_AGREED = {"prior", "marginal", "marker"}
FIX_TERMS_GOOD = []
FIX_TERMS_BAD = [r"\textbf{Term}"]

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
\mm{$w$}{Weight}{A number.}{A name agreed nowhere.}{No}{Assumed.}
The formula is a theorem (\lean{Fixture.no\_such}) and here is a colon: in prose.
\end{document}
"""

FIX_A_GOOD = [["A1", r"The marker $m$ moves the prior $\pi_g(c)$ through $Z_g$.",
               r"The marker moves the prior through the marginal. Lean, \lean{Fixture.flat\_posterior}.",
               r"$m$, $\pi_g(c)$, $Z_g$", "M1", "r"]]
FIX_A_BAD = [["A1", r"The prior $\pi_g(c)$ is scaled by $\delta$.",
              r"The belief is scaled by $\delta$. Lean, \lean{Fixture.no\_such}.",
              r"$\pi_g(c)$, $\theta$", "M1", "r"]]

lean = lean_declarations()
agreed = parent_terms()
print("=" * 72)
print("MEASUREMENT MAP (Appendix app:terms of theory/MANUSCRIPT.tex)")
print("=" * 72)
mtex = open(os.path.join(ROOT, "MANUSCRIPT.tex")).read()
tex = map_section(mtex)
terms = other_glossaries(mtex)
arows = assumption_rows(mtex)
real, nrows = analyse(tex, terms, lean, arows, agreed)
print(f"      rows in the map: {nrows}; tables of terms outside the map: {len(terms)}; "
      f"assumptions: {len(arows)}; agreed parent names: {len(agreed)}; proposed: {sorted(PROPOSED)}")
names = {"MM-1": "every row has six non-empty fields and a plain-English name",
         "MM-2": "one name per object",
         "MM-3": "the map is the only terminology",
         "MM-4": "every last column begins with a status word",
         "MM-5": "no colon in the prose",
         "MM-6": "every Lean name the map cites is declared in theory/lean",
         "MM-7": "Directly observed is Yes exactly for recorded objects",
         "MM-8": "no symbol names two rows",
         "MM-9": "every symbol of every assumption is a symbol of the map",
         "MM-10": "every plain-English reading is free of mathematics, uses the map's names and cites declared theorems",
         "MM-11": "every name is agreed in the parent map or proposed to the author"}
for k in sorted(real, key=lambda s: int(s.split('-')[1])):
    ok, detail = real[k]
    check(f"{k} {names[k]}", ok, "" if ok else detail)

print("-" * 72)
print("CONTROLS  a well-formed sample must pass every rule; a faulty one must fail every rule")
good, _ = analyse(GOOD, FIX_TERMS_GOOD, FIX_LEAN, FIX_A_GOOD, FIX_AGREED)
bad, _ = analyse(BAD, FIX_TERMS_BAD, FIX_LEAN, FIX_A_BAD, FIX_AGREED)
for k in sorted(good, key=lambda s: int(s.split('-')[1])):
    check(f"{k}-control passes on the well-formed sample", good[k][0], "" if good[k][0] else good[k][1])
    check(f"{k}-control fails on the faulty sample", not bad[k][0])

nf = sum(1 for _, ok in results if not ok)
print("=" * 72)
print(f"MEASUREMENT MAP SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
sys.exit(1 if nf else 0)
