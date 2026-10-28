"""Checks the measurement map of the enhancement paper, enhancement/MEASUREMENT_MAP.tex,
whose tables (map_tables.tex) Appendix C of enhancement/ENHANCEMENT.tex includes. The rules
are those of the theory paper's checker (theory/checks/verify_measurement_map.py), MM-1 to
MM-12, with the Lean declarations read from lean/mathlib and with MM-11 holding every name
either to the parent terminology table (Agreed or In use), to AGREED_HERE, the names the author
agreed for this paper on 28 October 2026, or to PROPOSED, names offered and not yet agreed.
"""
import glob
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
PARENT = os.path.dirname(ROOT)
LEAN_DIR = os.path.join(PARENT, "lean", "mathlib")
STATUS = ("Recorded", "Elicited", "Computed", "Assumed", "Never observed", "A device")
AGREED_HERE = {"capital ratio", "regulatory floor", "mean earnings", "earnings volatility", "judged risk",
            "book-capital volatility", "equity volatility", "mean capital ratio", "market judged risk",
            "stress-test decline", "normal quantile", "stress-test judged risk", "own-loss judged risk",
            "do-nothing volatility", "bank size", "buffer over the floor", "brownian motion", "positive root",
            "negative root", "barrier coefficients", "thin-buffer dividend share", "far-above share"}
PROPOSED = set()
ALTERNATIVES = {
    r"\breference ratio\b": "reference",
    r"\bdeposit insurer\b": "insurer",
    r"\bpayout barrier\b": "dividend barrier",
}
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
    for f in glob.glob(os.path.join(LEAN_DIR, "*.lean")):
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


def parent_retired():
    """Retired names of the parent table (its Replaces column) and the parent
    checker's banned list."""
    tex = strip_comments(open(os.path.join(PARENT, "MEASUREMENT_MAP.tex")).read())
    out = {"loss aversion", "payout barrier", "injection trigger", "recapitalisation boundary", "volatility ratio"}
    for r in macro_calls(tex, "nrow", 5):
        out |= {x.strip().lower() for x in r[4].split(",") if x.strip() and x.strip() != "none"}
    return sorted(out)


def term_problems(manuscript, retired):
    """Retired names outside literature rows and citing sentences; alternative names anywhere."""
    body = strip_comments(expand_inputs(manuscript, ROOT))
    body = body.split(r"\begin{document}", 1)[-1]
    lines = [ln for ln in body.splitlines() if not ln.lstrip().startswith("\\lrow{")]
    sents = re.split(r"(?<=[.;])\s+", " ".join(lines))
    found = []
    # The "what we rely on" field of a literature row is the paper's own text,
    # held to the alternative names; its quote is the source's and is not.
    for r in macro_calls(body, "lrow", 6):
        low = " ".join(r[2].replace("$", "").lower().split())
        found += [("alternative in " + r[0], pat) for pat in ALTERNATIVES if re.search(pat, low)]
    for snt in sents:
        low = " ".join(snt.replace("$", "").lower().split())
        cites = "\\cite" in snt or re.search(r"\bL[0-9]+\b", snt)
        if not cites:
            found += [("retired", n) for n in retired if n in low]
        found += [("alternative", pat) for pat in ALTERNATIVES if re.search(pat, low)]
    return found


def parent_terms():
    """The Agreed and In use names of the parent map's Terminology table."""
    tex = strip_comments(open(os.path.join(PARENT, "MEASUREMENT_MAP.tex")).read())
    return {r[0].strip().lower() for r in macro_calls(tex, "nrow", 5) if r[3].strip() in ("Agreed", "In use")}


MAP_HEAD = r"\section{Terminology and measurement map}"


def expand_inputs(tex, folder):
    """Replace each \\input{name} by the file's text, so the rows are read where they live."""
    def sub(m):
        name = m.group(1)
        f = os.path.join(folder, name if name.endswith(".tex") else name + ".tex")
        return open(f).read() if os.path.exists(f) else ""
    return re.sub(r"\\input\{([^{}]*)\}", sub, tex)


def map_section(tex):
    """The standalone map's document body, inputs expanded."""
    i = tex.find(r"\begin{document}")
    return expand_inputs(tex[i:] if i >= 0 else tex, ROOT)


def other_glossaries(tex):
    tex = strip_comments(expand_inputs(tex, ROOT))
    i = tex.find(MAP_HEAD)
    j = tex.find(r"\section{", i + len(MAP_HEAD)) if i >= 0 else -1
    rest = tex if i < 0 else tex[:i] + (tex[j:] if j >= 0 else "")
    return re.findall(r"\\textbf\{Term\}|^Term & Symbol", rest, re.M)


GREEK = ("alpha beta gamma delta varepsilon epsilon zeta eta theta kappa lambda mu nu xi pi "
         "rho sigma tau phi varphi chi psi omega Gamma Delta Theta Lambda Pi Sigma Phi Psi Omega").split()
INDICES = {"d", "t", "i"}


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
    foreign = sorted(n for n in names if n.lower() not in agreed | AGREED_HERE and n.lower() not in PROPOSED)
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
print("MEASUREMENT MAP (enhancement/MEASUREMENT_MAP.tex, included by Appendix C of ENHANCEMENT.tex)")
print("=" * 72)
mtex = open(os.path.join(ROOT, "ENHANCEMENT.tex")).read()
tex = map_section(open(os.path.join(ROOT, "MEASUREMENT_MAP.tex")).read())
included = r"\input{map_tables}" in strip_comments(mtex)
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
         "MM-6": "every Lean name the map cites is declared in lean/mathlib",
         "MM-7": "Directly observed is Yes exactly for recorded objects",
         "MM-8": "no symbol names two rows",
         "MM-9": "every symbol of every assumption is a symbol of the map",
         "MM-10": "every plain-English reading is free of mathematics, uses the map's names and cites declared theorems",
         "MM-11": "every name is agreed in the parent map or for this paper, or proposed to the author"}
for k in sorted(real, key=lambda s: int(s.split('-')[1])):
    ok, detail = real[k]
    check(f"{k} {names[k]}", ok, "" if ok else detail)
check("MM-0 the manuscript's Appendix C includes map_tables.tex, so manuscript and map cannot differ", included)
retired = parent_retired()
bad12 = term_problems(mtex, retired)
check("MM-12 no retired name outside literature rows and citing sentences, and no alternative name, in the manuscript or its figures",
      not bad12, f"{bad12[:6]}")

print("-" * 72)
print("CONTROLS  a well-formed sample must pass every rule; a faulty one must fail every rule")
good, _ = analyse(GOOD, FIX_TERMS_GOOD, FIX_LEAN, FIX_A_GOOD, FIX_AGREED)
bad, _ = analyse(BAD, FIX_TERMS_BAD, FIX_LEAN, FIX_A_BAD, FIX_AGREED)
for k in sorted(good, key=lambda s: int(s.split('-')[1])):
    check(f"{k}-control passes on the well-formed sample", good[k][0], "" if good[k][0] else good[k][1])
    check(f"{k}-control fails on the faulty sample", not bad[k][0])

check("MM-12-control a paper's own retired term in a sentence citing its literature row passes",
      term_problems("\\begin{document} With constant loss aversion the volatility is constant (L12).", retired) == [])
check("MM-12-control a retired name in the paper's own sentence is caught",
      term_problems("\\begin{document} Dividends are paid at the payout barrier.", retired) != [])
check("MM-12-control an alternative name is caught even in a citing sentence",
      term_problems("\\begin{document} The bank guards the reference ratio (L20).", retired) != [])

nf = sum(1 for _, ok in results if not ok)
print("=" * 72)
print(f"MEASUREMENT MAP SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
sys.exit(1 if nf else 0)
