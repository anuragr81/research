import glob
import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROSE_LIMIT = 0.30
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
    for f in glob.glob(os.path.join(ROOT, "lean", "mathlib", "*.lean")):
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
    for kf in glob.glob(os.path.join(ROOT, "lit", "*", "KEYS")):
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


def analyse(tex, lean, bibs, lit):
    tex = strip_comments(tex)
    krows = macro_calls(tex, "krow", 4)
    mrows = macro_calls(tex, "mrow", 4)
    lrows = macro_calls(tex, "lrow", 6)
    crows = macro_calls(tex, "crow", 3)
    proofs = proof_blocks(tex)
    out = {}

    ids = [(r[0].strip(), "K") for r in krows] + [(r[0].strip(), "M") for r in mrows]
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

    counts = f"K{len(krows)} M{len(mrows)} L{len(lrows)} C{len(crows)}, proofs {len(proofs)}"
    return out, counts


GOOD = r"""
\krow{K1}{h}{M1}{s}
\mrow{M1}{c}{$x$}{SmoothFit.smooth_fit}
\lrow{L1}{bayraktar2026}{w}{the parameter a3 scales the entire liquidity constraint and determines its asymptotic upper bound}{17}{M1}
\crow{C1}{s}{M1, L1}
\begin{mproof}{M1}{SmoothFit.smooth_fit}
Then \[ V'(s^-) = V'(s^+), \qquad \mathcal{M}V(s) = V(s) \] at the trigger $s$.
\end{mproof}
"""

BAD = r"""
\krow{K1}{h}{M9}{s}
\krow{K1}{h}{M1}{s}
\mrow{M1}{c}{$x$}{SmoothFit.no_such_theorem}
\lrow{L1}{bayraktar2026}{w}{A sentence the paper never printed.}{17}{M1}
\crow{C1}{s}{X5}
\begin{mproof}{M1}{SmoothFit.no_such_theorem}
This proof talks through every step in words and uses almost no symbols at all, so
a reader learns the argument from sentences rather than from expressions, which is
exactly what the rule forbids; it mentions $x$ once.
\end{mproof}
"""

lean = lean_declarations()
bibs = bib_keys()
lit = lit_records()

print("=" * 72)
print("MANUSCRIPT SKELETON (MANUSCRIPT.tex)")
print("=" * 72)
print(f"      Lean declarations found: {len(lean)}; bib keys: {len(bibs)}; lit keys: {len(lit)}")

tex = open(os.path.join(ROOT, "MANUSCRIPT.tex")).read()
real, counts = analyse(tex, lean, bibs, lit)
print(f"      rows in MANUSCRIPT.tex: {counts}")
names = {"MS-1": "row IDs well formed and unique",
         "MS-2": "introduction rows point to model rows only",
         "MS-3": "conclusion rows point to model or literature rows only",
         "MS-4": "every model row names audited Lean theorems and has one matching proof",
         "MS-5": "every literature row quotes its lit/ record verbatim",
         "MS-6": f"plain English at most {PROSE_LIMIT:.0%} of every proof"}
for k in sorted(real):
    ok, detail = real[k]
    check(f"{k} {names[k]}", ok, "" if ok else detail)

print("-" * 72)
print("CONTROLS  a well-formed sample must pass every rule; a faulty one must fail every rule")
print("      the samples resolve Lean names, bib keys and the quote against the real bundle")
check("MS-C0 the control's Lean theorem, bib key and lit record exist",
      "SmoothFit.smooth_fit" in lean and "bayraktar2026" in bibs and "bayraktar2026" in lit)
good, _ = analyse(GOOD, lean, bibs, lit)
bad, _ = analyse(BAD, lean, bibs, lit)
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
