#!/usr/bin/env python3
"""Render the corrections (entries E in corrections.py) as a LaTeX master plan:
BEFORE and AFTER typeset side by side, then Why and Evidence."""
import re, sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import corrections_entries as C
from plan_entries_E import E2
C.discipline_check()


def esc(t):
    t = t.replace("\\", "\\textbackslash{}")
    for a, b in [("&", "\\&"), ("%", "\\%"), ("#", "\\#"), ("_", "\\_"), ("{", "\\{"), ("}", "\\}"),
                 ("^", "\\^{}"), ("~", "\\~{}")]:
        t = t.replace(a, b)
    return t.replace("\\textbackslash\\{\\}", "\\textbackslash{}")


def prose(t):
    """Markdown-ish explanation text to LaTeX: keep $...$, `code` -> texttt, "q" -> ``q''."""
    out = []
    for k, part in enumerate(re.split(r"(\$[^$]*\$|`[^`]*`)", t)):
        if part.startswith("$") and part.endswith("$") and len(part) > 1:
            out.append(part)
        elif part.startswith("`") and part.endswith("`") and len(part) > 1:
            out.append("\\texttt{" + esc(part[1:-1]) + "}")
        else:
            p = esc(part).replace("->", "$\\to$")
            p = re.sub(r'"([^"]*)"', r"``\1''", p)
            p = re.sub(r"\*\*([^*]*)\*\*", r"\\textbf{\1}", p)
            p = re.sub(r"\*([^*]*)\*", r"\\emph{\1}", p)
            out.append(p)
    return "".join(out)


def latex_cell(t):
    t = " ".join(t.split())
    if t.startswith("%"):
        return "\\textit{(sentence deleted)}"
    # a floating table cannot sit inside a longtable cell: unwrap it
    t = re.sub(r"\\begin\{table\}(\[[^\]]*\])?\s*\\centering\s*", "", t)
    t = re.sub(r"\\caption\{(.*?)\}\s*\\label\{[^}]*\}\s*\\end\{table\}", r" \\par\\textit{Table caption: \1}", t)
    # a sectioning command cannot sit inside a table cell: show a heading stand-in
    t = re.sub(r"\\subsection\{((?:[^{}]|\{[^{}]*\})*)\}(\\label\{[^}]*\})?",
               r"\\textsc{subsection heading}\\par\\textbf{\\large \1}\\par ", t)
    t = t.replace("p{2.6cm}p{5.4cm}p{5.4cm}", "p{2.2cm}p{4.6cm}p{4.6cm}")
    t = t.replace("p{3.0cm}p{5.6cm}p{5.6cm}", "p{2.0cm}p{3.4cm}p{3.4cm}")   # the four-case table (6.9)
    t = t.replace("p{3.4cm}cccc", "p{2.0cm}cccc").replace("p{5.2cm}cc@", "p{4.2cm}cc@")
    t = re.sub(r"\\paragraph\{([^}]*)\}", r"\\textbf{\1} ", t)
    t = re.sub(r"\\section\{([^}]*)\}", r"\\textit{Section title:} \\textbf{\1}", t)
    t = t.replace("}%", "}")
    t = t.replace("\\begin{align*}", "\\[\\begin{aligned}").replace("\\end{align*}", "\\end{aligned}\\]")
    return t


import glob, importlib.util

def load_grounds():
    g = {}
    for f in sorted(glob.glob(os.path.join(os.path.dirname(os.path.abspath(__file__)), "grounds", "grounds_*.py"))):
        spec = importlib.util.spec_from_file_location(os.path.basename(f)[:-3], f)
        m = importlib.util.module_from_spec(spec); spec.loader.exec_module(m)
        for k, v in m.GROUNDS.items():
            g.setdefault(k, []).extend(v)
    return g

GROUNDS = load_grounds()
KIND = {"quote": "Quote", "theorem": "Theorem", "computation": "Computation"}


def grounds_rows(eid):
    """One table row per item, so the table can break between items."""
    items = GROUNDS.get(eid, [])
    rows = []
    for n, it in enumerate(items):
        src = re.sub(r"(?<!\\)_", r"\\_", it['source'])
        head = f"\\textbf{{{KIND.get(it['kind'], it['kind'])}.}} \\textit{{{src}}}"
        line = f"{head} \\newline {it['text']}"
        if it.get("note"):
            line += f" \\newline \\textit{{{it['note']}}}"
        label = "\\textbf{Grounds.}\\par " if n == 0 else ""
        rows.append(f"\\multicolumn{{3}}{{|p{{\\fullw}}|}}{{{label}{line}}} \\\\")
    if rows:
        rows[-1] = rows[-1] + " \\hline"
    return rows


# ---------------------------------------------------------------------------
# One table in manuscript order, numbered by the section each entry changes.
# ---------------------------------------------------------------------------
RAW = C.MS


def _rx(anchor):
    return re.compile(r"\s+".join(re.escape(w) for w in anchor.split()))


def landing(part):
    """Raw-text position where the part's AFTER text lands."""
    kind, s, e, after = C.norm_part(part)
    m = _rx(s).search(RAW)
    assert m, s[:60]
    if kind == "insert_sentence":
        return m.start()
    if kind in ("insert_para", "insert_cont"):
        end = _rx(e).search(RAW, m.start())
        brk = re.compile(r"\n[ \t]*\n").search(RAW, end.end())
        return brk.start() if brk else len(RAW)
    return m.start()


def section_marks():
    """(position, label): 0 abstract, 1.. numbered sections, A.. appendix, B back matter."""
    marks, names = [(0, "0")], {"0": "Abstract", "B": "Back matter and bibliography"}
    n, app, in_app = 0, 0, False
    for m in re.finditer(r"\\appendix|\\section\*?\{([^}]*)\}|\\bibliography\{", RAW):
        tok = m.group(0)
        if tok == "\\appendix":
            in_app = True
        elif tok.startswith("\\section*") or tok.startswith("\\bibliography"):
            marks.append((m.start(), "B"))
        elif in_app:
            app += 1
            lab = chr(ord("A") + app - 1)
            marks.append((m.start(), lab))
            names[lab] = "Appendix, " + m.group(1)
        else:
            n += 1
            marks.append((m.start(), str(n)))
            names[str(n)] = m.group(1)
    return marks, names


MARKS, SEC_NAME = section_marks()


def sec_of(pos):
    lab = "0"
    for p0, l in MARKS:
        if p0 <= pos:
            lab = l
    return lab


# Grounds follow the entries that were split.
def _move(frm, to, pred):
    keep, moved = [], []
    for it in GROUNDS.get(frm, []):
        (moved if pred(it) else keep).append(it)
    GROUNDS[frm] = keep
    GROUNDS.setdefault(to, []).extend(moved)


_move("C.9", "C.9w", lambda it: it["source"].startswith("Hawthorne"))
_c11 = GROUNDS.get("C.11", [])
_cut = next((i for i, it in enumerate(_c11) if it["source"].startswith("Phelps")), len(_c11))
GROUNDS["C.11b"] = _c11[_cut:] + GROUNDS.get("C.11b", [])
GROUNDS["C.11"] = _c11[:_cut]
_ADJ = ("Anchoring.lean", "sympy/check\\_zero", "Hogarth and Einhorn", "HogarthEinhorn.lean", "Epstein.lean")
_move("E.10", "E.15", lambda it: it["source"].startswith(_ADJ))
_move("E.12", "E.15", lambda it: True)

# Entry numbers are frozen here so that applying entries never renumbers the rest.
NUMBERS = {
    "C.1": "0.1", "C.2": "1.1", "C.3": "1.2", "C.4": "1.3", "C.5": "1.4", "E.1": "1.5",
    "C.6": "1.6", "C.7": "1.7", "C.8": "1.8", "E.15r": "1.9",
    "C.9": "2.1", "E.2": "2.2", "C.9w": "2.3", "C.10": "2.4", "E.3": "2.5", "E.4": "2.6",
    "E.5": "3.1", "C.11": "3.2", "E.6": "3.3", "C.11b": "3.4", "E.7": "3.5",
    "E.8": "4.1", "C.13": "4.2", "E.9": "5.1", "E.10": "5.2",
    "E.15h": "6.1", "E.11": "6.2", "E.15": "6.3",
    "C.14": "A.1", "E.13": "B.1", "E.14": "B.2", "C.16": "5.3", "C.17": "7.1", "C.18": "6.4", "C.19": "7.2", "C.20": "3.6", "C.21": "6.5", "C.22": "6.6", "C.23": "3.7", "C.24": "6.7", "C.25": "2.7", "C.26": "6.8", "C.27": "6.9", "C.29": "6.10", "C.30": "1.10", "C.31": "6.11", "C.32": "6.12",
}
ALL = list(C.E) + list(E2)
assert set(e[0] for e in ALL) == set(NUMBERS), set(e[0] for e in ALL) ^ set(NUMBERS)
APPLIED = C.APPLIED
placed = []
for seq, ent in enumerate(ALL):
    if ent[0] in APPLIED:
        continue
    live = [p for k, p in enumerate(ent[2], 1) if (ent[0], k) not in C.APPLIED_PARTS]
    pos = min(landing(p) for p in live) if live else len(RAW) + seq
    placed.append((pos, seq, ent))
placed.sort(key=lambda t: (t[0], t[1]))
NEW = dict(NUMBERS)
ALIAS = {"C.12": NEW["E.14"], "E.12": NEW["E.15"], "C.15": NEW["C.11b"]}


def _numkey(n):
    a, b = n.split(".")
    return ({"0": 0, "A": 8, "B": 9}.get(a, int(a) if a.isdigit() else 7), int(b))


APPLIED_NUMS = sorted((NEW[k] for k in APPLIED), key=_numkey)
PENDING_NUMS = sorted((NEW[k] for k in NEW if k not in APPLIED), key=_numkey)
APPLIED_COMMITS = sorted(set(APPLIED.values()))
_pp = {}
for (eid, k), cm in C.APPLIED_PARTS.items():
    _pp.setdefault(eid, []).append(k)
APPLIED_PARTS_NOTE = "".join(f"; {NEW[e]} parts {', '.join(str(k) for k in sorted(ks))} at {C.APPLIED_PARTS[(e, ks[0])]}" for e, ks in sorted(_pp.items()))
APPLIED_COMMITS = sorted(set(APPLIED.values()) | set(C.APPLIED_PARTS.values()))


def renum(t):
    """Rewrite old entry numbers (C.4, E.15, ...) as the new section numbers."""
    t = re.sub(r"\bC\.11\.9\b|\bC\.11 part 9\b", NEW["C.11b"], t)
    return re.sub(r"\b[CE]\.\d+[a-z]?\b", lambda m: NEW.get(m.group(0)) or ALIAS.get(m.group(0)) or m.group(0), t)


def rows_for(placed, keep=None, note=None, why=None):
    """keep(eid, k): include part k; note(eid, k): status line under the part;
    why(eid): None for the full Why/Evidence/Grounds, else a line that replaces them."""
    rows, current = [], None
    for pos, seq, (eid, title, pairs, purpose, verif) in placed:
        ks = [k for k in range(1, len(pairs) + 1) if keep is None or keep(eid, k)]
        if pairs and not ks:
            continue
        lab = NEW[eid].split(".")[0]
        if lab != current:
            current = lab
            rows.append(f"\\multicolumn{{3}}{{|l|}}{{\\rule{{0pt}}{{3.2ex}}\\Large\\textbf{{{lab}\\quad {prose(SEC_NAME.get(lab, lab))}}}}} \\\\ \\hline\\hline")
        rows.append(f"\\entryhead{{{NEW[eid]}}}{{{prose(renum(title))}\\quad{{\\footnotesize\\textit{{(formerly {eid})}}}}}}")
        for k, part in enumerate(pairs, 1):
            if k not in ks:
                continue
            kind, s, e, after = C.norm_part(part)
            before = C.cut(s, e, eid, k)
            n = f"{k}/{len(pairs)}"
            if (eid, k) in C.APPLIED_PARTS:
                n += " \\par{\\tiny\\textit{applied}}" if len(pairs) > 1 else ""
            if kind == "insert_para":
                before = "\\textit{New paragraph(s) after the paragraph containing} ``" + before + "''"
            elif kind == "insert_cont":
                before = "\\textit{Continues the insertion of the previous part.}"
            elif kind == "insert_sentence":
                before = "\\textit{New sentence after} ``" + before + "''"
            rows.append(f"{n} & {latex_cell(before)} & {latex_cell(after)} \\\\ \\hline")
            line = note(eid, k) if note else None
            if line:
                rows.append(f"\\multicolumn{{3}}{{|p{{\\fullw}}|}}{{\\textit{{Status.}} {line}}} \\\\ \\hline")
        if not pairs:
            rows.append(f" & \\multicolumn{{2}}{{p{{24.6cm}}|}}{{\\textit{{No manuscript text; see Why.}}}} \\\\ \\hline")
        w = why(eid) if why else None
        if w:
            rows.append(f"\\multicolumn{{3}}{{|p{{\\fullw}}|}}{{{w}}} \\\\ \\hline")
        else:
            rows.append(f"\\multicolumn{{3}}{{|p{{\\fullw}}|}}{{\\textbf{{Why.}} {prose(renum(purpose))}}} \\\\")
            rows.append(f"\\multicolumn{{3}}{{|p{{\\fullw}}|}}{{\\textbf{{Evidence.}} {prose(renum(verif))}}} \\\\ \\hline")
            rows.extend(renum(r) for r in grounds_rows(eid))
        rows.append("\\multicolumn{3}{|l|}{} \\\\ \\hline")
    return rows


rows = rows_for(placed)


# ---------------------------------------------------------------------------
# Applied parts: still in the manuscript, or history.  A part's AFTER text is
# split into sentences; the share still present (whitespace-normalised) in the
# current manuscript decides.  At least half present: in force, with a note of
# what changed it since.  Less than half: history.  A deletion stands while the
# deleted text stays out.  The commits at which sentences vanished are named,
# with the applied entry responsible when its BEFORE text held them.
# ---------------------------------------------------------------------------
import subprocess

def _norm(t):
    return re.sub(r"\s+", " ", t)


_HEADING = re.compile(r"(\\(?:sub)*section\*?\{(?:[^{}]|\{[^{}]*\})*\}(?:\\label\{[^}]*\})?)\s*(?:\\par\b)?\s*")


def _sentences(after):
    """Sentences of an AFTER text; a sectioning heading (with its label) counts as a unit of its own."""
    t = _norm("\n".join(l for l in after.splitlines() if not l.lstrip().startswith("%"))).strip()
    units = [u.strip() for u in _HEADING.split(t) if u.strip()]
    out = []
    for u in units:
        out += [u] if _HEADING.fullmatch(u + " ") else re.split(r"(?<=[.?!])\s+(?=[A-Z\\])", u)
    return [x for x in out if len(x) >= 25]


_COMMITS = subprocess.run(["git", "-C", C.REPO, "rev-list", "--reverse", "HEAD", "--", "PAPER_B_MANUSCRIPT.tex"],
                          capture_output=True, text=True, check=True).stdout.split()
_TEXT = {}


def _text_at(full):
    if full not in _TEXT:
        _TEXT[full] = _norm(subprocess.run(["git", "-C", C.REPO, "show", f"{full}:./PAPER_B_MANUSCRIPT.tex"],
                                           capture_output=True, text=True, check=True).stdout)
    return _TEXT[full]


def _applied_at(eid, k):
    return APPLIED.get(eid) or C.APPLIED_PARTS.get((eid, k))


def _who(commit8, gone):
    """Name the applied entry whose BEFORE text held, or sat inside, a vanished sentence; else the commit."""
    for eid2, title2, parts2, *_ in ALL:
        for k2, part2 in enumerate(parts2, 1):
            if _applied_at(eid2, k2) != commit8:
                continue
            _, s2, e2, _ = C.norm_part(part2)
            before = _norm(C.cut(s2, e2, eid2, k2)).strip()
            # the BEFORE text held the sentence, or (a short replacement) sat inside it
            if any(x in before or (len(before) >= 20 and before in x) for x in gone):
                return f"entry {NEW[eid2]} (part {k2}) at {commit8}"
    subj = subprocess.run(["git", "-C", C.REPO, "log", "-1", "--format=%s", commit8],
                          capture_output=True, text=True, check=True).stdout.strip()
    subj = subj if len(subj) <= 70 else subj[:67].rsplit(" ", 1)[0] + "..."
    return f"the edit at {commit8} (``{esc(subj)}'')"


STATUS = {}
for _eid, _t, _parts, *_ in ALL:
    for _k, _part in enumerate(_parts, 1):
        _c8 = _applied_at(_eid, _k)
        if not _c8:
            continue
        _kind, _s, _e, _after = C.norm_part(_part)
        if _after.lstrip().startswith("%"):
            STATUS[(_eid, _k)] = ("force", "") if C.cut(_s, _e, _eid, _k) not in C.NORM else \
                ("history", "The deleted text is back in the manuscript.")
            continue
        _ss = _sentences(_after)
        _full = next(c for c in _COMMITS if c.startswith(_c8))
        _changes, _prev = [], set(x for x in _ss if x in _text_at(_full))
        for _cm in _COMMITS[_COMMITS.index(_full) + 1:]:
            _here = set(x for x in _ss if x in _text_at(_cm))
            if _here < _prev:
                _changes.append(_who(_cm[:8], _prev - _here))
            _prev = _here
        _n, _m = len([x for x in _ss if x in C.NORM]), len(_ss)
        _by = "; ".join(_changes)
        if _n == _m:
            STATUS[(_eid, _k)] = ("force", "")
        elif 2 * _n >= _m:
            STATUS[(_eid, _k)] = ("force", f"In force with changes: {_n} of {_m} sentences stand as applied; changed by {_by}.")
        else:
            STATUS[(_eid, _k)] = ("history", (f"Removed by {_by}." if _n == 0 else
                                             f"Largely superseded: {_n} of {_m} sentences stand as applied; changed by {_by}."))

_in_force = lambda e, k: STATUS.get((e, k), ("", ""))[0] == "force"
_in_history = lambda e, k: STATUS.get((e, k), ("", ""))[0] == "history"
_note = lambda e, k: STATUS.get((e, k), ("", ""))[1] or None
HIST_ENTRIES = {e for (e, k), (cls, _) in STATUS.items() if cls == "history"}
SPLIT = {e for e in HIST_ENTRIES if any(cls == "force" for (e2, k), (cls, _) in STATUS.items() if e2 == e)}

applied_placed = sorted(((0, seq, ent) for seq, ent in enumerate(ALL)
                         if ent[0] in APPLIED or any((ent[0], k) in C.APPLIED_PARTS for k in range(1, len(ent[2]) + 1))),
                        key=lambda t: _numkey(NEW[t[2][0]]))
rows_applied = rows_for(applied_placed, keep=_in_force, note=_note)
rows_history = rows_for(applied_placed, keep=_in_history, note=_note,
                        why=lambda e: ("\\textit{Why, Evidence and Grounds are with this entry under Applied entries in force.}"
                                       if e in SPLIT else None))
HIST_NUMS = sorted({NEW[e] for e in HIST_ENTRIES}, key=_numkey)

def _order_note():
    """Only constraints among pending entries; applied ones need no ordering."""
    pend = lambda *ks: [renum(k) for k in ks if k not in APPLIED]
    parts = []
    if "E.14" not in APPLIED:
        parts.append(renum("E.14") + " (bibliography) goes in before every pending entry that cites a reference not yet in the .bib")
    citers = pend("C.4", "C.5", "C.9w", "E.15r")
    if citers and ("E.10" not in APPLIED or "E.15" not in APPLIED):
        parts.append(", ".join(pend("E.10", "E.15")) + " go in before " + ", ".join(citers) + ", which cite them")
    if "E.11" not in APPLIED and "E.15" not in APPLIED:
        parts.append(renum("E.11") + " goes in before " + renum("E.15") + ", which inserts after it")
    if "C.4" not in APPLIED and "C.27" not in APPLIED:
        parts.append(renum("C.27") + " goes in with or before " + renum("C.4") + ", whose introduction points to the varieties it lists")
    if "E.13" not in APPLIED:
        parts.append(renum("E.13") + " goes in last")
    if not parts:
        return "The pending entries can be applied in any order."
    return "Order of application, where it differs from manuscript order. " + ". ".join(p[0].upper() + p[1:] for p in parts) + "."

ORDER_NOTE = _order_note()


def _key(k):
    m = re.match(r"([CE])\.(\d+)(\w*)", k)
    return (m.group(1), int(m.group(2)), m.group(3))


MAP_ROWS = "\n".join(
    f"{eid} & {NEW[eid]} & {prose(SEC_NAME.get(NEW[eid].split('.')[0], ''))} & "
    f"{('applied at ' + APPLIED[eid] + (', part in History' if eid in SPLIT else (', in History' if eid in HIST_ENTRIES else ''))) if eid in APPLIED else 'pending'} \\\\ \\hline"
    for eid in sorted(NEW, key=_key))
MAP_ROWS += "\n" + "\n".join(f"{a} & {b} & folded in & \\\\ \\hline" for a, b in sorted(ALIAS.items()))

D = [
    ("0.A", "Abstract", "Superseded by the author's rewrite and C.1."),
    ("1.A, 1.B, 1.D, 1.E", "Introduction", "Superseded by the author's rewrite; what survives is corrected in C.2, C.3, C.4, C.5, C.6, C.7 and C.8. The ORD sentence of 1.E is still unapplied and presupposes E.10."),
    ("1.A2", "Two-channel prelude", "Applied by the author; corrected by C.4."),
    ("1.C", "Why sequence matters beyond one judgement", "Re-issued as E.1."),
    ("2.A, 2.B, 2.C", "Impression; worked example; soft cues", "Re-issued as E.2, E.3 (extended with the soft-versus-hard passage), E.4."),
    ("3.A, 3.B", "Commutativity literature; Asch", "Re-issued as E.5, E.6."),
    ("3.C", "Heckman", "Superseded by the identification paragraph appended in C.11b, with P17a applied."),
    ("3.D", "Bohren-Imas-Rosenberg", "Re-issued as E.7."),
    ("4.A, 5.A", "Section titles and openings", "Re-issued as E.8, E.9."),
    ("5.B", "Propositions ORD and ADJ", "ORD re-issued as E.10; ADJ moved, with LAD and FAC, to E.15."),
    ("6.A, 6.B", "Rival mechanisms; two channels", "Re-issued as E.11 and E.15 (6.B's text pulled in from interior\\_omega.tex)."),
    ("B.A, B.B", "Bibliography; AI declaration", "Re-issued as E.14, E.13."),
    ("W.A", "Input/output tables", "Withdrawn by the author 2026-09-21; kept in notes/empirical\\_analytics.tex."),
]
drows = "\n".join(f"{a} & {b} & {prose(renum(c))} \\\\ \\hline" for a, b, c in D)

BIB = NEW["E.14"]
BIB_USERS = renum("C.6, C.10, C.11b, E.1, E.5 and E.7")
CHAPTER_USERS = renum("C.6 and C.10")
IDENT = NEW["C.11b"]

tex = r"""%% manuscript_corrections.tex -- master plan of corrections to PAPER_B_MANUSCRIPT.tex.
%% GENERATED by notes/tools/make_corrections_tex.py from notes/tools/corrections_entries.py
%% and notes/tools/plan_entries_E.py (BEFORE text cut from the committed manuscript by
%% script). Edit the entries files, then run:
%%   python3 notes/tools/make_corrections_tex.py notes/manuscript_corrections.tex
\documentclass[10pt]{article}
\usepackage[a4paper,landscape,margin=1.4cm]{geometry}
\usepackage{amsmath,amssymb,dsfont}
\usepackage{amsthm}
\newtheorem{proposition}{Proposition}
\newtheorem{lemma}{Lemma}
\newtheorem{theorem}{Theorem}
\theoremstyle{definition}
\newtheorem{definition}{Definition}
\usepackage{longtable,array,booktabs}
\usepackage[authoryear,round]{natbib}
\usepackage[T1]{fontenc}
\newcommand{\PB}{P^{\mathrm{B}}}
\newcommand{\PJ}{P^{\mathrm{J}}}
\newcommand{\Pbar}{\overline{P}}
\newcommand{\assoc}{\operatorname{assoc}}
\newcommand{\sgn}{\operatorname{sgn}}
\newcommand{\E}{\mathbb{E}}
\newcommand{\bigO}{\mathcal{O}}
\newcommand{\vv}{v}
\newcommand{\anchor}[1]{}
\renewcommand{\ref}[1]{\textsc{\detokenize{#1}}}
\renewcommand{\footnote}[1]{ \textit{[Footnote: #1]}}
\newlength{\fullw}\setlength{\fullw}{25.9cm}
\newcommand{\entryhead}[2]{\multicolumn{3}{|l|}{\rule{0pt}{2.6ex}\large\textbf{#1}\quad #2} \\ \hline}
\setlength{\parindent}{0pt}
\title{Paper B: corrections to the manuscript (master plan)}
\author{}
\date{Against the manuscript at """ + APPLIED_COMMITS[-1] + r""", 2 October 2026}
\begin{document}
\maketitle
\vspace{-1.5em}
\noindent Each row sets the current manuscript text (left) against the proposed replacement (right),
both typeset as they would appear. BEFORE text is cut from the committed manuscript by script, so it
matches the manuscript exactly. Under each entry, \textbf{Why} gives the error and \textbf{Evidence} the
Lean or sympy record or page that settles it (audit items refer to \texttt{notes/citation\_audit.md}).
The identification paragraph appended in """ + IDENT + r""" reconciles Section 3 with
\texttt{notes/positioning\_economics.tex}. Cross-references print as their label (e.g.\ \textsc{prop:IMM});
footnotes print inline. The document has three parts. \textbf{Pending entries} are proposals, none
applied; the author approves them by number, and approved entries are applied to the manuscript,
compiled and committed. \textbf{Applied entries in force} are those whose text is in the manuscript.
\textbf{History} keeps applied text that a later entry or the author's own edit has since removed or
largely rewritten, together with the archived plan and the old entry numbers. Every AFTER text passes the
mechanical checks of \texttt{notes/writing\_discipline.md} (no colons, no dashes doing a sentence's
work, ``sequence'' for reading order, the fixed adoption terminology, length within 80--120\% of the
draft except where the entry says why); the generator refuses to build otherwise.

\section*{Pending entries}
Entries are numbered by the manuscript section they change and listed in the order in which their
text would appear. \textbf{0} is the abstract, \textbf{1} to \textbf{7} are the numbered sections,
\textbf{A} is the appendix and \textbf{B} is the back matter with the bibliography. Each entry also
shows its former number, which earlier notes and commits use, and the table at the end maps old
numbers to new. """ + ORDER_NOTE + r"""

\paragraph*{Applied to the manuscript} at """ + ", ".join(APPLIED_COMMITS) + r""": """ + ", ".join(APPLIED_NUMS) + APPLIED_PARTS_NOTE + r""".
These entries have left the table of changes. What of them stands in the manuscript is under
``Applied entries in force''; what has since been removed or largely rewritten (""" + ", ".join(HIST_NUMS) + r""") is under ``History''.

\paragraph*{Pending} (""" + str(len(PENDING_NUMS)) + r""" entries, in manuscript order): """ + ", ".join(PENDING_NUMS) + r""".

{\small
\begin{longtable}{|p{0.9cm}|p{12.2cm}|p{12.2cm}|}
\hline
\textbf{Part} & \textbf{BEFORE (current manuscript, or the anchor for an insert)} & \textbf{AFTER (proposed)} \\ \hline\hline
\endhead
""" + "\n".join(rows) + r"""
\end{longtable}
}

\section*{Bibliography entries the AFTER texts need (""" + BIB + r""")}
\texttt{Hawthorne2004} and \texttt{ZhaoOsherson2010} are already in \texttt{bibliography.bib}.
Six more are needed by """ + BIB_USERS + r""" (Jeffrey 2004, Benjamin et al.\ 2019, Heckman 1998,
Bohren et al.\ 2019, D\"oring 1999, Garber 1980); they are in
\texttt{notes/manuscript\_corrections\_extra.bib} so that this document renders them, and move to
\texttt{bibliography.bib} on approval. The Drive copy of Jeffrey (2004) is the November 2002 draft,
so """ + CHAPTER_USERS + r""" cite the chapter only.
\citet{CoffmanExleyNiederle2021} (\emph{Management Science} 67(6), 3551--3569) and
\citet{TverskyKahneman1992} (\emph{Journal of Risk and Uncertainty} 5, 297--323), needed by
""" + renum("C.19") + r""" and """ + renum("C.20") + r""", moved into \texttt{bibliography.bib} when those entries were applied
on 3 October 2026.

\section*{Applied entries in force}
These entries were applied to the manuscript at """ + ", ".join(APPLIED_COMMITS) + r""", and their text stands in
it. Each part shows its BEFORE text as it stood just before the applying commit and the AFTER text as
applied, with the Why, Evidence and Grounds that justified it. A part at least half of whose sentences
stand as applied is kept here with a \textit{Status} line naming what has changed it since; a part of
which less remains is under History.
{\small
\begin{longtable}{|p{0.9cm}|p{12.2cm}|p{12.2cm}|}
\hline
\textbf{Part} & \textbf{BEFORE (manuscript before the applying commit)} & \textbf{AFTER (as applied)} \\ \hline\hline
\endhead
""" + "\n".join(rows_applied) + r"""
\end{longtable}
}

\section*{History}
\subsection*{Applied text since removed or largely rewritten}
Parts of applied entries whose text no longer stands in the manuscript, each with a \textit{Status} line
naming the entry or the author's edit that removed it. They are kept as the record of what was
proposed and applied, not as proposals.
{\small
\begin{longtable}{|p{0.9cm}|p{12.2cm}|p{12.2cm}|}
\hline
\textbf{Part} & \textbf{BEFORE (manuscript before the applying commit)} & \textbf{AFTER (as applied)} \\ \hline\hline
\endhead
""" + "\n".join(rows_history) + r"""
\end{longtable}
}

\subsection*{Where each entry of the archived plan (notes/manuscript\_change\_plan\_asof\_2026-09-30.md) now lives}
{\small
\begin{longtable}{|p{2.6cm}|p{5.2cm}|p{17.4cm}|}
\hline
\textbf{Entry} & \textbf{Subject} & \textbf{Status and what it needs} \\ \hline\endhead
""" + drows + r"""
\end{longtable}
}

\subsection*{Old and new entry numbers}
{\small
\begin{longtable}{|p{2.2cm}|p{2.2cm}|p{9cm}|p{4cm}|}
\hline
\textbf{Former} & \textbf{Now} & \textbf{Section} & \textbf{Status} \\ \hline\endhead
""" + MAP_ROWS + r"""
\end{longtable}
}

\bibliographystyle{plainnat}
\bibliography{../bibliography,manuscript_corrections_extra}
\end{document}
"""
open(sys.argv[1] if len(sys.argv) > 1 else os.path.join(C.REPO, "notes", "manuscript_corrections.tex"), "w").write(tex)
print("rows:", len(rows), "entries:", len(NEW))
print("order:", " ".join(f"{NEW[e[0]]}={e[0]}" for _, _, e in placed))
