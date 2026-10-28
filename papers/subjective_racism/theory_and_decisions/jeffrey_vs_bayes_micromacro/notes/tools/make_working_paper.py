#!/usr/bin/env python3
"""Generate PAPER_B_WORKING_PAPER.tex, the public working-paper version.

The working paper is the numbered copy (second_order_sequence_stats_numbered_eqs.tex,
itself generated from PAPER_B_MANUSCRIPT.tex) in a plain article layout, with no
Elsevier class or styling, so that as a preprint it does not look like the journal
version (Elsevier article sharing policy).  Only the preamble, the title block and
the bibliography style change; the body is copied unchanged.  Every editorial change
is made to PAPER_B_MANUSCRIPT.tex and both copies are regenerated:

    python3 notes/tools/make_working_paper.py          # write the .tex
    python3 notes/tools/make_working_paper.py --build  # and compile the .pdf

The transform asserts what it expects to find, so a change in the manuscript's
front matter stops it rather than producing a silently wrong copy.
"""
import os
import re
import subprocess
import sys

REPO = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", ".."))
NUMBERED = os.path.join(REPO, "second_order_sequence_stats_numbered_eqs.tex")
NAME = "PAPER_B_WORKING_PAPER"
DST = os.path.join(REPO, NAME + ".tex")
DATE_LINE = "Working paper, October 2026"

PREAMBLE_PACKAGES = r"""\documentclass[11pt,a4paper]{article}

\usepackage[T1]{fontenc}
\usepackage[utf8]{inputenc}
\usepackage{lmodern}
\usepackage{microtype}
\usepackage[a4paper,margin=2.5cm]{geometry}
\usepackage{amsmath,amssymb}
\usepackage{graphicx}
\usepackage[authoryear,longnamesfirst]{natbib}
"""


def one(s, old, new):
    assert s.count(old) == 1, (old[:60], s.count(old))
    return s.replace(old, new)


def transform(s):
    header = (
        "% GENERATED from PAPER_B_MANUSCRIPT.tex (through the numbered copy) by\n"
        "% notes/tools/make_working_paper.py.  Do not edit: make every change in\n"
        "% PAPER_B_MANUSCRIPT.tex and regenerate.  Plain article layout for the\n"
        "% public working paper; the body is the numbered copy's, unchanged.\n"
    )
    start = s.index("\\documentclass")
    s = header + s[start:]
    s = one(s, "\\documentclass[a4paper,fleqn]{cas-sc}\n\n\\usepackage[authoryear,longnamesfirst]{natbib}\n",
            PREAMBLE_PACKAGES)
    s = one(s, "\\emergencystretch=3em\n",
            "\\emergencystretch=3em\n\n\\usepackage[colorlinks,allcolors=blue]{hyperref}\n")
    s = one(s, "\\let\\WriteBookmarks\\relax\n\\def\\floatpagepagefraction{1}\n\\def\\textpagefraction{.001}\n", "")

    i = s.index("\\shorttitle{")
    j = s.index("\\maketitle", i) + len("\\maketitle")
    front = s[i:j]
    title = re.search(r"\\title\[mode=title\]\{(.*?)\}\n", front).group(1)
    abstract = re.search(r"\\begin\{abstract\}\n(.*?)\n\\end\{abstract\}", front, re.S).group(1)
    kw = re.search(r"\\begin\{keywords\}\n(.*?)\n\\end\{keywords\}", front, re.S).group(1)
    orcid = re.search(r"orcid=([0-9X-]+)", front).group(1)
    email = re.search(r"\\ead\{([^}]*)\}", front).group(1)
    org = re.search(r"organization=\{([^}]*)\}", front).group(1)
    addr = re.search(r"addressline=\{([^}]*)\}", front).group(1)
    city = re.search(r"city=\{([^}]*)\}", front).group(1)
    country = re.search(r"country=\{([^}]*)\}", front).group(1)
    words, jel = kw.split("\\sep \\textit{JEL:}")
    keywords = "; ".join(w.strip() for w in words.split("\\sep") if w.strip())
    new_front = (
        f"\\title{{{title}}}\n"
        f"\\author{{Anurag Srivastava\\thanks{{{org}, {addr}, {city}, {country}. "
        f"\\texttt{{{email}}}. ORCID {orcid}.}}}}\n"
        f"\\date{{{DATE_LINE}}}\n\n"
        "\\maketitle\n\n"
        f"\\begin{{abstract}}\n{abstract}\n\n"
        f"\\medskip\\noindent\\textbf{{Keywords}}\\quad {keywords}\\par\n"
        f"\\noindent\\textbf{{JEL}}\\quad {jel.strip()}\n"
        "\\end{abstract}"
    )
    s = s[:i] + new_front + s[j:]

    # an empty heading prints in the working paper
    s = re.sub(r"\\section\*\{Acknowledgements\}\s*\n(?=\\bibliographystyle)", "", s)
    s = one(s, "\\bibliographystyle{cas-model2-names}", "\\bibliographystyle{plainnat}")
    return s


def main(argv):
    subprocess.run([sys.executable, os.path.join(REPO, "notes", "tools", "make_numbered_copy.py")],
                   check=True, cwd=REPO, stdout=subprocess.DEVNULL)
    s = transform(open(NUMBERED).read())
    open(DST, "w").write(s)
    print(f"wrote {NAME}.tex")
    if "--build" in argv:
        out = os.path.join(REPO, "notes", "tools", "build_working_paper")
        os.makedirs(out, exist_ok=True)
        subprocess.run(["latexmk", "-pdf", "-interaction=nonstopmode", f"-outdir={out}", DST],
                       cwd=REPO, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        log = open(os.path.join(out, NAME + ".log"), errors="ignore").read()
        errors = log.count("\n! ")
        undef = len(re.findall(r"undefined", log))
        pdf = os.path.join(out, NAME + ".pdf")
        if os.path.exists(pdf):
            os.replace(pdf, os.path.join(REPO, NAME + ".pdf"))
        print(f"built {NAME}.pdf: {errors} errors, {undef} undefined-reference warnings")
        return 1 if errors else 0
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
