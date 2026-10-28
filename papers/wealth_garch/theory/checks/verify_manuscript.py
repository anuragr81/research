import glob
import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
# The literature records of papers/wealth_garch, shared by its papers.
LIT = os.path.join(os.path.dirname(ROOT), "lit")
# Every section and table of the skeleton, by label.
REQUIRED = ["sec:intro", "tab:headlines", "sec:model", "tab:players", "tab:assumptions",
            "tab:results", "sec:literature", "tab:literature", "tab:citations", "sec:conclusions",
            "tab:concl", "tab:pending", "app:proofs", "app:evidence", "app:terms",
            "tab:terms", "app:scope", "app:refuted"]
PROSE_LIMIT = 0.30
# The table of citations (MS-10): counts fetched by checks/fetch_citations.py,
# ratings read from the ABDC list the author supplied.
CITATIONS = os.path.join(ROOT, "citations.json")
ABDC = os.path.join(ROOT, "..", "..", "journal_rankings", "ABDC-JQL-2025-v3-210926.xlsx")
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


def proof_blocks(tex):
    blocks = []
    for m in re.finditer(r"\\begin\{mproof\}", tex):
        i, args = m.end(), []
        for _ in range(2):
            a, i = brace_group(tex, i)
            args.append(a)
        end = tex.find(r"\end{mproof}", i)
        blocks.append((args[0], args[1], tex[i:end if end >= 0 else len(tex)]))
    return blocks


def split_list(s):
    return [x.strip() for x in (s or "").split(",") if x.strip()]


def lean_declarations():
    names = set()
    files = glob.glob(os.path.join(ROOT, "lean", "*.lean"))
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


def bib_keys():
    return set(re.findall(r"^@\w+\{([^,]+),", open(os.path.join(ROOT, "refs.bib")).read(), re.M))


def lit_records():
    rec = {}
    for kf in glob.glob(os.path.join(LIT, "*", "KEYS")):
        d = os.path.dirname(kf)
        text = ""
        for name in ("CLAIMS.md", "NOTES.md"):
            p = os.path.join(d, name)
            if os.path.exists(p):
                text += open(p).read() + "\n"
        for k in open(kf).read().split():
            rec[k] = rec.get(k, "") + text
    return rec


def normalise(s):
    s = re.sub(r"\\[A-Za-z]+\*?\{([^{}]*)\}", r"\1", s)
    s = s.replace("$", "").replace("``", "").replace("''", "").replace("\\", "")
    return re.sub(r"\s+", " ", s).strip()


MATH_ENV = r"(?:equation|align|gather|multline)\*?"


def prose_ratio(body):
    body = strip_comments(body)
    body = re.sub(r"\\(?:lean|ref|eqref|label|cite[a-z]*)\*?(?:\[[^\]]*\])?\{[^{}]*\}", " ", body)
    math = []
    pats = [rf"\\begin\{{({MATH_ENV})\}}(.*?)\\end\{{\1\}}", r"\\\[(.*?)\\\]",
            r"\$\$(.*?)\$\$", r"\\\((.*?)\\\)", r"\$(.*?)\$"]
    for p in pats:
        def keep(m):
            math.append(m.group(m.lastindex))
            return " "
        body = re.sub(p, keep, body, flags=re.S)
    mtok = 0
    for seg in math:
        seg = re.sub(r"\\\\|&|[{}]", " ", seg)
        mtok += len(re.findall(r"\\[A-Za-z]+|[A-Za-z0-9]+|[^\sA-Za-z0-9\\]", seg))
    prose = re.sub(r"\\[A-Za-z]+\*?", " ", body)
    ptok = len(re.findall(r"[A-Za-z]+(?:'[A-Za-z]+)?", prose))
    total = ptok + mtok
    return (ptok / total if total else 0.0), ptok, mtok


def abdc_index(path):
    """ISSN -> 2025 rating, from the 2025 sheet of the ABDC list (cells carry trailing whitespace)."""
    import openpyxl
    ws = openpyxl.load_workbook(path, read_only=True)["2025 JQL"]
    idx = {}
    for r in ws.iter_rows(min_row=9, values_only=True):
        if r[1]:
            for i in (3, 4):
                if r[i]:
                    idx[str(r[i]).strip()] = str(r[7]).strip()
    return idx


def cited_keys(tex):
    keys = set()
    for m in re.finditer(r"\\cite(?:t|p|alt|author|year)?\*?(?:\[[^\]]*\])*\{([^{}]*)\}", tex):
        keys |= {k.strip() for k in m.group(1).split(",") if k.strip() and not k.strip().startswith("#")}
    return keys


def citation_problems(tex, cit, abdc):
    """MS-10. Every cited key has exactly one row in the table of citations and no other key has one; each
    row's count is the OpenAlex count of the record it names, with the other versions' counts in brackets
    when there are any; each row's ABDC rating is the 2025 rating of the journal's ISSN, or -- or
    'not listed ...' when the ISSN is not in the 2025 list."""
    rows = macro_calls(tex, "cirow", 6)
    cited = cited_keys(tex) | {r[1].strip() for r in macro_calls(tex, "lrow", 6)}
    keys = [r[0].strip() for r in rows]
    prob = []
    if not rows:
        prob.append("no table of citations")
    prob += [f"{k} cited, no row" for k in sorted(cited - set(keys))]
    prob += [f"{k} has a row, not cited" for k in sorted(set(keys) - cited)]
    prob += [f"{k} has {keys.count(k)} rows" for k in sorted(set(keys)) if keys.count(k) > 1]
    for r in rows:
        key, count, rating = r[0].strip(), r[4].strip(), r[5].strip()
        d = cit.get("papers", {}).get(key)
        if d is None:
            prob.append(f"{key} not in citations.json")
            continue
        main, others = d["main"]["cites"], d["others_cites"]
        want = f"{main} (+{others})" if others else f"{main}"
        if count != want:
            prob.append(f"{key} count {count!r}, citations.json gives {want!r}")
        hit = next((abdc[i] for i in d["main"].get("issn") or [] if i in abdc), None)
        if hit is not None and rating != hit:
            prob.append(f"{key} ABDC {rating!r}, the 2025 list gives {hit!r}")
        if hit is None and not (rating == "--" or rating.startswith("not listed")):
            prob.append(f"{key} ABDC {rating!r}, but its ISSN is not in the 2025 list")
        if (d.get("abdc_2025") or None) != hit:
            prob.append(f"{key} citations.json rating {d.get('abdc_2025')!r} against the list {hit!r}")
    return prob


def analyse(tex, lean, bibs, lit, cit=None, abdc=None):
    tex = strip_comments(tex)
    krows = macro_calls(tex, "krow", 4)
    prows = macro_calls(tex, "prow", 6)
    arows = macro_calls(tex, "arow", 6)
    mrows = macro_calls(tex, "mrow", 4)
    lrows = macro_calls(tex, "lrow", 6)
    crows = macro_calls(tex, "crow", 3)
    proofs = proof_blocks(tex)
    out = {}

    ids = [(r[0].strip(), "K") for r in krows] + [(r[0].strip(), "M") for r in mrows]
    ids += [(r[0].strip(), "P") for r in prows]
    ids += [(r[0].strip(), "A") for r in arows]
    ids += [(r[0].strip(), "L") for r in lrows] + [(r[0].strip(), "C") for r in crows]
    bad_form = [i for i, p in ids if not re.fullmatch(p + r"[1-9][0-9]*", i)]
    seen, dup = set(), []
    for i, _ in ids:
        if i in seen:
            dup.append(i)
        seen.add(i)
    out["MS-1"] = (not bad_form and not dup, f"bad ids {bad_form}; duplicates {dup}")

    mids = {r[0].strip() for r in mrows}
    lids = {r[0].strip() for r in lrows}
    cids = {r[0].strip() for r in crows}
    bad_k = [(r[0], l) for r in krows for l in (split_list(r[2]) or ["(none)"]) if l not in mids]
    out["MS-2"] = (not bad_k, f"introduction links not to a model row {bad_k}")
    bad_c = [(r[0], l) for r in crows for l in (split_list(r[2]) or ["(none)"]) if l not in mids | lids]
    out["MS-3"] = (not bad_c, f"conclusion links not to a model or literature row {bad_c}")

    bad_a = [(r[0], l) for r in arows for l in (split_list(r[4]) or ["(none)"]) if l not in mids]
    covered = {l for r in arows for l in split_list(r[4])}
    bare = sorted(mids - covered)
    out["MS-7"] = ((bool(arows) or not mrows) and not bad_a and not bare,
                   f"assumption links not to a model row {bad_a}; model rows under no assumption {bare}")

    bad_p = [(r[0], l) for r in prows for l in (split_list(r[5]) or ["(none)"]) if l not in mids | lids]
    out["MS-8"] = ((bool(prows) or not mrows) and not bad_p, f"player links not to a model or literature row {bad_p}")

    problems = []
    proof_ids = [p[0].strip() for p in proofs]
    for r in mrows:
        names = split_list(r[3])
        if not names:
            problems.append((r[0], "no Lean theorem"))
        problems += [(r[0], f"unknown {n}") for n in names if n not in lean]
        n_proofs = proof_ids.count(r[0].strip())
        if n_proofs != 1:
            problems.append((r[0], f"{n_proofs} proofs"))
    for pid, pnames, _ in proofs:
        if pid.strip() not in mids:
            problems.append((pid, "proof without a model row"))
            continue
        row = next(r for r in mrows if r[0].strip() == pid.strip())
        if set(split_list(pnames)) != set(split_list(row[3])):
            problems.append((pid, "proof and row name different Lean theorems"))
    out["MS-4"] = (not problems, f"{problems}")

    lprob = []
    for r in lrows:
        key, quote, links = r[1].strip(), normalise(r[3]), split_list(r[5])
        if key not in bibs:
            lprob.append((r[0], f"{key} not in refs.bib"))
        elif key not in lit:
            lprob.append((r[0], f"{key} has no lit/ directory"))
        elif not quote or quote not in normalise(lit[key]):
            lprob.append((r[0], "quote not found verbatim in the lit/ record"))
        bad = [l for l in (links or ["(none)"]) if l not in mids | cids]
        if bad:
            lprob.append((r[0], f"links {bad}"))
    out["MS-5"] = (not lprob, f"{lprob}")

    over = []
    for pid, _, body in proofs:
        ratio, p, m = prose_ratio(body)
        if ratio > PROSE_LIMIT:
            over.append((pid, round(ratio, 3), p, m))
    out["MS-6"] = (not over, f"proofs above {PROSE_LIMIT:.0%} prose {over}")

    cprob = citation_problems(tex, cit or {}, abdc or {})
    out["MS-10"] = (not cprob, f"{cprob}")

    labels = set(re.findall(r"\\label\{([^{}]*)\}", tex))
    absent = [l for l in REQUIRED if l not in labels]
    out["MS-9"] = (not absent, f"sections or tables missing {absent}")

    counts = f"K{len(krows)} P{len(prows)} A{len(arows)} M{len(mrows)} L{len(lrows)} C{len(crows)}, proofs {len(proofs)}"
    return out, counts


# The controls run on fixtures of their own, so they do not depend on what the
# bundle happens to contain yet. The well-formed sample cites a Lean name, a bib
# key and a quotation that the fixtures declare; the faulty sample breaks every
# rule once.
FIX_LEAN = {"Fixture.flat_posterior"}
FIX_BIBS = {"Fixture2000"}
FIX_LIT = {"Fixture2000": "The marker carries no information about class within a group."}
FIX_CIT = {"papers": {"Fixture2000": {"main": {"cites": 7, "issn": ["0000-0001"]}, "others_cites": 2, "abdc_2025": "A"}}}
FIX_ABDC = {"0000-0001": "A"}

GOOD = r"""
\krow{K1}{h}{M1}{s}
\prow{P1}{p}{c}{h}{l}{M1, L1}
\arow{A1}{a}{p}{$x$}{M1}{r}
\mrow{M1}{c}{$x$}{Fixture.flat_posterior}
\lrow{L1}{Fixture2000}{w}{The marker carries no information about class within a group.}{12}{M1}
\crow{C1}{s}{M1, L1}
\cirow{Fixture2000}{r}{p}{2000}{7 (+2)}{A}
\begin{mproof}{M1}{Fixture.flat_posterior}
Then \[ \frac{\ell(c)\pi(c)}{\sum_{c'} \ell(c')\pi(c')} = \pi(c) \] for every $c \in \mathcal{C}$.
\end{mproof}
""" + "".join(f"\\label{{{l}}}\n" for l in REQUIRED)

BAD = r"""
\krow{K1}{h}{M9}{s}
\prow{P1}{p}{c}{h}{l}{X5}
\arow{A1}{a}{p}{$x$}{M9}{r}
\krow{K1}{h}{M1}{s}
\mrow{M1}{c}{$x$}{Fixture.no_such_theorem}
\lrow{L1}{Fixture2000}{w}{A sentence the paper never printed.}{12}{M1}
\crow{C1}{s}{X5}
\cirow{Fixture2000}{r}{p}{2000}{9}{B}
\cirow{Fixture1999}{r}{p}{1999}{0}{--}
\begin{mproof}{M1}{Fixture.no_such_theorem}
This proof talks through every step in words and uses almost no symbols at all, so
a reader learns the argument from sentences rather than from expressions, which is
exactly what the rule forbids; it mentions $x$ once.
\end{mproof}
"""

lean = lean_declarations()
bibs = bib_keys()
lit = lit_records()

print("=" * 72)
print("MANUSCRIPT SKELETON (theory/wealth_garch_theory.tex)")
print("=" * 72)
print(f"      Lean declarations found: {len(lean)}; bib keys: {len(bibs)}; lit keys: {len(lit)}")

tex = open(os.path.join(ROOT, "wealth_garch_theory.tex")).read()
# Appendix C includes map_tables.tex, so the labels and rows it holds count.
tex = re.sub(r"\\input\{([^{}]*)\}",
             lambda m: open(os.path.join(ROOT, m.group(1) + ("" if m.group(1).endswith(".tex") else ".tex"))).read()
             if os.path.exists(os.path.join(ROOT, m.group(1) + ("" if m.group(1).endswith(".tex") else ".tex"))) else "",
             tex)
import json  # noqa: E402
cit = json.load(open(CITATIONS)) if os.path.exists(CITATIONS) else {}
abdc = abdc_index(ABDC) if os.path.exists(ABDC) else {}
print(f"      citations.json: {len(cit.get('papers', {}))} papers, retrieved {cit.get('retrieved')}; ABDC 2025 ISSNs: {len(abdc)}")
real, counts = analyse(tex, lean, bibs, lit, cit, abdc)
print(f"      rows in wealth_garch_theory.tex: {counts}")
names = {"MS-1": "row IDs well formed and unique",
         "MS-2": "introduction rows point to model rows only",
         "MS-3": "conclusion rows point to model or literature rows only",
         "MS-4": "every model row names audited Lean theorems and has one matching proof",
         "MS-5": "every literature row quotes its lit/ record verbatim",
         "MS-6": f"plain English at most {PROSE_LIMIT:.0%} of every proof",
         "MS-7": "assumption rows point to model rows, and every model row rests on one",
         "MS-8": "player rows point to model or literature rows only",
         "MS-9": "every section and table of the skeleton is present",
         "MS-10": "the table of citations covers every cited paper, with counts and ABDC ratings from their sources"}
for k in sorted(real):
    ok, detail = real[k]
    check(f"{k} {names[k]}", ok, "" if ok else detail)

print("-" * 72)
print("CONTROLS  a well-formed sample must pass every rule; a faulty one must fail every rule")
good, _ = analyse(GOOD, FIX_LEAN, FIX_BIBS, FIX_LIT, FIX_CIT, FIX_ABDC)
bad, _ = analyse(BAD, FIX_LEAN, FIX_BIBS, FIX_LIT, FIX_CIT, FIX_ABDC)
for k in sorted(good):
    check(f"{k}-control passes on the well-formed sample", good[k][0], "" if good[k][0] else good[k][1])
    check(f"{k}-control fails on the faulty sample", not bad[k][0])
r_good = prose_ratio(proof_blocks(GOOD)[0][2])
r_bad = prose_ratio(proof_blocks(BAD)[0][2])
print(f"      prose share, well-formed sample {r_good[0]:.2f}; faulty sample {r_bad[0]:.2f}")

nf = sum(1 for _, ok in results if not ok)
print("=" * 72)
print(f"MANUSCRIPT SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
sys.exit(1 if nf else 0)
