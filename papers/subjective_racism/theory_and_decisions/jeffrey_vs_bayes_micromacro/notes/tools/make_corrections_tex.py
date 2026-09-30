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


def rows_for(entries):
    rows = []
    for eid, title, pairs, purpose, verif in entries:
        rows.append(f"\\entryhead{{{eid}}}{{{prose(title)}}}")
        for k, part in enumerate(pairs, 1):
            kind, s, e, after = C.norm_part(part)
            before = C.cut(s, e)
            n = f"{k}/{len(pairs)}" if len(pairs) > 1 else ""
            if kind == "insert_para":
                before = "\\textit{New paragraph(s) after the paragraph containing} ``" + before + "''"
            rows.append(f"{n} & {latex_cell(before)} & {latex_cell(after)} \\\\ \\hline")
        if not pairs:
            rows.append(f" & \\multicolumn{{2}}{{p{{24.6cm}}|}}{{\\textit{{No manuscript text; see Why.}}}} \\\\ \\hline")
        rows.append(f"\\multicolumn{{3}}{{|p{{\\fullw}}|}}{{\\textbf{{Why.}} {prose(purpose)}}} \\\\")
        rows.append(f"\\multicolumn{{3}}{{|p{{\\fullw}}|}}{{\\textbf{{Evidence.}} {prose(verif)}}} \\\\ \\hline")
        rows.extend(grounds_rows(eid))
        rows.append("\\multicolumn{3}{|l|}{} \\\\ \\hline")
    return rows


rows = rows_for(C.E)
rows_e = rows_for(E2)

D = [
    ("0.A", "Abstract", "Superseded by the author's rewrite and C.1."),
    ("1.A, 1.B, 1.D, 1.E", "Introduction", "Superseded by the author's rewrite; what survives is corrected in C.2-C.8. The ORD sentence of 1.E is still unapplied and presupposes E.10."),
    ("1.A2", "Two-channel prelude", "Applied by the author; corrected by C.4."),
    ("1.C", "Why sequence matters beyond one judgement", "Re-issued as E.1."),
    ("2.A, 2.B, 2.C", "Impression; worked example; soft cues", "Re-issued as E.2, E.3 (extended with the soft-versus-hard passage), E.4."),
    ("3.A, 3.B", "Commutativity literature; Asch", "Re-issued as E.5, E.6."),
    ("3.C", "Heckman", "Superseded by C.15, which carries the identification precedent with P17a applied."),
    ("3.D", "Bohren-Imas-Rosenberg", "Re-issued as E.7."),
    ("4.A, 5.A", "Section titles and openings", "Re-issued as E.8, E.9."),
    ("5.B", "Propositions ORD and ADJ", "Re-issued as E.10 (mathematics unchanged; prose corrected per P10/P11)."),
    ("6.A, 6.B", "Rival mechanisms; two channels", "Re-issued as E.11, E.12 (6.B's text pulled in from interior\\_omega.tex)."),
    ("B.A, B.B", "Bibliography; AI declaration", "Re-issued as E.14, E.13."),
    ("W.A", "Input/output tables", "Withdrawn by the author 2026-09-21; kept in notes/empirical\\_analytics.tex."),
]
drows = "\n".join(f"{a} & {b} & {prose(c) if '$' not in c and chr(92) not in c else c} \\\\ \\hline" for a, b, c in D)

tex = r"""%% manuscript_corrections.tex -- master plan of corrections to PAPER_B_MANUSCRIPT.tex.
%% GENERATED by notes/tools/make_corrections_tex.py from notes/tools/corrections_entries.py
%% (BEFORE text cut from the committed manuscript by script). Edit the entries file,
%% then run: python3 notes/tools/make_corrections_tex.py notes/manuscript_corrections.tex
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
\date{Against the author's draft committed at e7b997e3, 30 September 2026}
\begin{document}
\maketitle
\vspace{-1.5em}
\noindent Each row sets the current manuscript text (left) against the proposed replacement (right),
both typeset as they would appear. BEFORE text is cut from the committed manuscript by script, so it
matches the manuscript exactly. Under each entry, \textbf{Why} gives the error and \textbf{Evidence} the
Lean or sympy record or page that settles it (audit items refer to \texttt{notes/citation\_audit.md}).
Entry C.15 (inside C.11.9) reconciles Section 3 with \texttt{notes/positioning\_economics.tex}; the abstract (C.1) leads with the identification point for the same reason. Cross-references print as their label (e.g.\ \textsc{prop:IMM}); footnotes print inline. \textbf{Nothing
here is applied.} The author approves entries by number; approved entries are applied to the
manuscript, compiled and committed. Every AFTER text passes the mechanical checks of \texttt{notes/writing\_discipline.md} (no colons, no dashes doing a sentence's work, ``sequence'' for reading order, the fixed adoption terminology, length within 80--120\% of the draft except for deletions, citation fixes and the removed duplicate); the generator refuses to build otherwise.

\section*{Section C: corrections to the current draft}
{\small
\begin{longtable}{|p{0.9cm}|p{12.2cm}|p{12.2cm}|}
\hline
\textbf{Part} & \textbf{BEFORE (current manuscript)} & \textbf{AFTER (proposed)} \\ \hline\hline
\endhead
""" + "\n".join(rows) + r"""
\end{longtable}
}

\section*{Section E: the archived plan's entries, re-issued and corrected}
The entries of \texttt{notes/manuscript\_change\_plan\_asof\_2026-09-30.md} that the author's draft did
not already apply, each corrected against the audit and passed through the discipline gate.
Order of application: E.10 (Propositions ORD and ADJ) before anything that cites them; E.14
(bibliography) before E.1, E.5, E.7 and C.15; E.11 before E.12.
{\small
\begin{longtable}{|p{0.9cm}|p{12.2cm}|p{12.2cm}|}
\hline
\textbf{Part} & \textbf{BEFORE (current manuscript, or the anchor for an insert)} & \textbf{AFTER (proposed)} \\ \hline\hline
\endhead
""" + "\n".join(rows_e) + r"""
\end{longtable}
}

\section*{C.12 and E.14: bibliography entries the AFTER texts need}
\texttt{Hawthorne2004} and \texttt{ZhaoOsherson2010} are already in \texttt{bibliography.bib}.
Six more are needed by C.4's optional sentence, C.6, C.10, C.15, E.1, E.5 and E.7 (Jeffrey 2004, Benjamin et al.\ 2019, Heckman 1998, Bohren et al.\ 2019, D\"oring 1999, Garber 1980); they are in
\texttt{notes/manuscript\_corrections\_extra.bib} so that this document renders them, and move to
\texttt{bibliography.bib} on approval. The Drive copy of Jeffrey (2004) is the November 2002 draft,
so C.6 and C.10 cite the chapter only.

\section*{Section D: where each entry of the archived plan (notes/manuscript\_change\_plan\_asof\_2026-09-30.md) now lives}
{\small
\begin{longtable}{|p{2.6cm}|p{5.2cm}|p{17.4cm}|}
\hline
\textbf{Entry} & \textbf{Subject} & \textbf{Status and what it needs} \\ \hline\endhead
""" + drows + r"""
\end{longtable}
}

\bibliographystyle{plainnat}
\bibliography{../bibliography,manuscript_corrections_extra}
\end{document}
"""
open(sys.argv[1] if len(sys.argv) > 1 else os.path.join(C.REPO, "notes", "manuscript_corrections.tex"), "w").write(tex)
print("rows:", len(rows))
