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
    t = re.sub(r"\\subsection\{([^}]*)\}(\\label\{[^}]*\})?",
               r"\\textsc{subsection heading}\\par\\textbf{\\large \1}\\par ", t)
    t = t.replace("p{2.6cm}p{5.4cm}p{5.4cm}", "p{2.2cm}p{4.6cm}p{4.6cm}")
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

ALL = list(C.E) + list(E2)
placed = []
for seq, ent in enumerate(ALL):
    pos = min(landing(p) for p in ent[2]) if ent[2] else len(RAW) + seq
    placed.append((pos, seq, ent))
placed.sort(key=lambda t: (t[0], t[1]))

NEW, _count = {}, {}
for pos, seq, ent in placed:
    lab = sec_of(pos) if ent[2] else "B"
    _count[lab] = _count.get(lab, 0) + 1
    NEW[ent[0]] = f"{lab}.{_count[lab]}"
ALIAS = {"C.12": NEW["E.14"], "E.12": NEW["E.15"], "C.15": NEW["C.11b"]}


def renum(t):
    """Rewrite old entry numbers (C.4, E.15, ...) as the new section numbers."""
    t = re.sub(r"\bC\.11\.9\b|\bC\.11 part 9\b", NEW["C.11b"], t)
    return re.sub(r"\b[CE]\.\d+[a-z]?\b", lambda m: NEW.get(m.group(0)) or ALIAS.get(m.group(0)) or m.group(0), t)


def rows_for(placed):
    rows, current = [], None
    for pos, seq, (eid, title, pairs, purpose, verif) in placed:
        lab = NEW[eid].split(".")[0]
        if lab != current:
            current = lab
            rows.append(f"\\multicolumn{{3}}{{|l|}}{{\\rule{{0pt}}{{3.2ex}}\\Large\\textbf{{{lab}\\quad {prose(SEC_NAME.get(lab, lab))}}}}} \\\\ \\hline\\hline")
        rows.append(f"\\entryhead{{{NEW[eid]}}}{{{prose(renum(title))}\\quad{{\\footnotesize\\textit{{(formerly {eid})}}}}}}")
        for k, part in enumerate(pairs, 1):
            kind, s, e, after = C.norm_part(part)
            before = C.cut(s, e)
            n = f"{k}/{len(pairs)}" if len(pairs) > 1 else ""
            if kind == "insert_para":
                before = "\\textit{New paragraph(s) after the paragraph containing} ``" + before + "''"
            elif kind == "insert_cont":
                before = "\\textit{Continues the insertion of the previous part.}"
            rows.append(f"{n} & {latex_cell(before)} & {latex_cell(after)} \\\\ \\hline")
        if not pairs:
            rows.append(f" & \\multicolumn{{2}}{{p{{24.6cm}}|}}{{\\textit{{No manuscript text; see Why.}}}} \\\\ \\hline")
        rows.append(f"\\multicolumn{{3}}{{|p{{\\fullw}}|}}{{\\textbf{{Why.}} {prose(renum(purpose))}}} \\\\")
        rows.append(f"\\multicolumn{{3}}{{|p{{\\fullw}}|}}{{\\textbf{{Evidence.}} {prose(renum(verif))}}} \\\\ \\hline")
        rows.extend(renum(r) for r in grounds_rows(eid))
        rows.append("\\multicolumn{3}{|l|}{} \\\\ \\hline")
    return rows


rows = rows_for(placed)

ORDER_NOTE = renum(
    "Order of application, where it differs from manuscript order. E.14 (bibliography) goes in "
    "before every entry that cites a new reference. E.10 (Proposition ORD) and E.15 (Propositions "
    "ADJ, LAD and FAC) go in before C.4, C.5, C.9w and E.15r, which cite them. E.11 goes in before "
    "E.15, which inserts after it. E.13 goes in last.")


def _key(k):
    m = re.match(r"([CE])\.(\d+)(\w*)", k)
    return (m.group(1), int(m.group(2)), m.group(3))


MAP_ROWS = "\n".join(
    f"{eid} & {NEW[eid]} & {prose(SEC_NAME.get(NEW[eid].split('.')[0], ''))} \\\\ \\hline"
    for eid in sorted(NEW, key=_key))
MAP_ROWS += "\n" + "\n".join(f"{a} & {b} & folded in \\\\ \\hline" for a, b in sorted(ALIAS.items()))

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
\date{Against the author's draft committed at c2ae782f, 1 October 2026}
\begin{document}
\maketitle
\vspace{-1.5em}
\noindent Each row sets the current manuscript text (left) against the proposed replacement (right),
both typeset as they would appear. BEFORE text is cut from the committed manuscript by script, so it
matches the manuscript exactly. Under each entry, \textbf{Why} gives the error and \textbf{Evidence} the
Lean or sympy record or page that settles it (audit items refer to \texttt{notes/citation\_audit.md}).
The identification paragraph appended in """ + IDENT + r""" reconciles Section 3 with
\texttt{notes/positioning\_economics.tex}. Cross-references print as their label (e.g.\ \textsc{prop:IMM});
footnotes print inline. \textbf{Nothing here is applied.} The author approves entries by number;
approved entries are applied to the manuscript, compiled and committed. Every AFTER text passes the
mechanical checks of \texttt{notes/writing\_discipline.md} (no colons, no dashes doing a sentence's
work, ``sequence'' for reading order, the fixed adoption terminology, length within 80--120\% of the
draft except where the entry says why); the generator refuses to build otherwise.

\section*{Entries in manuscript order}
Entries are numbered by the manuscript section they change and listed in the order in which their
text would appear. \textbf{0} is the abstract, \textbf{1} to \textbf{7} are the numbered sections,
\textbf{A} is the appendix and \textbf{B} is the back matter with the bibliography. Each entry also
shows its former number, which earlier notes and commits use, and the table at the end maps old
numbers to new. """ + ORDER_NOTE + r"""

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

\section*{Where each entry of the archived plan (notes/manuscript\_change\_plan\_asof\_2026-09-30.md) now lives}
{\small
\begin{longtable}{|p{2.6cm}|p{5.2cm}|p{17.4cm}|}
\hline
\textbf{Entry} & \textbf{Subject} & \textbf{Status and what it needs} \\ \hline\endhead
""" + drows + r"""
\end{longtable}
}

\section*{Old and new entry numbers}
{\small
\begin{longtable}{|p{2.2cm}|p{2.2cm}|p{12cm}|}
\hline
\textbf{Former} & \textbf{Now} & \textbf{Section} \\ \hline\endhead
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
