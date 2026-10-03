#!/usr/bin/env python3
"""Generate second_order_sequence_stats_numbered_eqs.tex from PAPER_B_MANUSCRIPT.tex.

The copy differs from the manuscript only in how results are named: lemmas,
propositions and the theorem share one counter numbered within sections
(Lemma 4.1, Proposition 4.2, ...), instead of the manuscript's mnemonics (DIV,
PRO, ...).  Every editorial change is made to PAPER_B_MANUSCRIPT.tex; this copy
is regenerated, never edited:

    python3 notes/tools/make_numbered_copy.py          # write the .tex
    python3 notes/tools/make_numbered_copy.py --build  # and compile the .pdf

The transform asserts what it expects to find, so a change in the manuscript's
structure stops it rather than producing a silently wrong copy.
"""
import os
import re
import subprocess
import sys

REPO = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", ".."))
SRC = os.path.join(REPO, "PAPER_B_MANUSCRIPT.tex")
NAME = "second_order_sequence_stats_numbered_eqs"
DST = os.path.join(REPO, NAME + ".tex")

LEMMAS = {"ASC", "SCR", "SEP"}
THEOREMS = {"LOS"}
MNEMONICS = "IMM|DIV|ASC|ORD|SCR|SEP|DEC|DRF|LOS|SHR|PRO|ADJ|LAD"


def label(m):
    return ("lem:" if m in LEMMAS else "thm:" if m in THEOREMS else "prop:") + m


def transform(s):
    # 1. one counter for theorem, proposition and lemma, numbered within sections
    old = "\\newtheorem{proposition}{Proposition}\n\\newtheorem{lemma}{Lemma}\n\\newtheorem{theorem}{Theorem}\n"
    assert s.count(old) == 1, "theorem declarations not as expected"
    s = s.replace(old, "\\newtheorem{theorem}{Theorem}[section]\n"
                       "\\newtheorem{proposition}[theorem]{Proposition}\n"
                       "\\newtheorem{lemma}[theorem]{Lemma}\n")
    # 2. drop the mnemonic renaming before each result
    s, n = re.subn(r"^\\renewcommand\{\\the(?:proposition|lemma|theorem)\}\{[A-Z]+\}%?\n", "", s, flags=re.M)
    assert n == 13, f"expected 13 mnemonic renamings, found {n}"
    # 3. mnemonics typed as text become references (\anchor{...} prints nothing and is left alone)
    pat = re.compile(r"\b(Proposition|Prop\.|Lemma|Theorem)(~| )(" + MNEMONICS + r")\b")
    out, pos, count = [], 0, 0
    for m in pat.finditer(s):
        if s.rfind("\\anchor{", 0, m.start()) > s.rfind("}", 0, m.start()):
            continue
        out.append(s[pos:m.start()] + f"{m.group(1)}~\\ref{{{label(m.group(3))}}}")
        pos, count = m.end(), count + 1
    s = "".join(out) + s[pos:]
    # nothing named by mnemonic may remain outside labels and \anchor
    left = [m.group(0) for m in pat.finditer(s) if s.rfind("\\anchor{", 0, m.start()) <= s.rfind("}", 0, m.start())]
    assert not left, f"mnemonics left as text: {left}"
    head = ("% GENERATED from PAPER_B_MANUSCRIPT.tex by notes/tools/make_numbered_copy.py.\n"
            "% Do not edit: make every change in PAPER_B_MANUSCRIPT.tex and regenerate.\n"
            "% Differs only in naming: lemmas, propositions and the theorem are numbered\n"
            "% within sections on one counter instead of by mnemonic.\n")
    return head + s, count


def build():
    for cmd in (["pdflatex", "-interaction=nonstopmode", NAME + ".tex"], ["bibtex", NAME],
                ["pdflatex", "-interaction=nonstopmode", NAME + ".tex"],
                ["pdflatex", "-interaction=nonstopmode", NAME + ".tex"]):
        subprocess.run(cmd, cwd=REPO, capture_output=True)
    log = open(os.path.join(REPO, NAME + ".log"), errors="replace").read()
    errors = log.count("\n! ")
    undefined = [l for l in log.splitlines() if "undefined" in l.lower() and "Font shape" not in l]
    print(f"built {NAME}.pdf: {errors} errors, {len(undefined)} undefined-reference warnings")
    return errors == 0 and not undefined


if __name__ == "__main__":
    text, n = transform(open(SRC).read())
    open(DST, "w").write(text)
    print(f"wrote {NAME}.tex ({n} textual mnemonics turned into references)")
    if "--build" in sys.argv:
        sys.exit(0 if build() else 1)
