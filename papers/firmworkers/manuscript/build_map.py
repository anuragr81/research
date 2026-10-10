import os
import sys
import yaml

ROOT = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, ROOT)
from build_manuscript import mixed, escape_text, col, vocab
from check import load_map


def used_in(claims):
    out = {}
    for c in claims['model']:
        for x in c.get('symbols', []) or []:
            out.setdefault(x, []).append(c['id'])
    return out


def table(rows, used, role):
    head = r'ID & Symbol & Model meaning & Real-world referent & Support & Direction & Used in \\ \midrule \endhead'
    body = []
    for r in rows:
        if r['role'] != role:
            continue
        prod = f"\\newline produced by {', '.join(r['produced_by'])}" if r.get('produced_by') else ''
        body.append(f"{r['id']} & {r['symbol']}{prod} & {mixed(r['meaning'])} & {mixed(r.get('referent')) or '---'} & "
                    f"\\texttt{{{escape_text(r['support'])}}} & {mixed(r.get('direction')) or '---'} & "
                    f"{', '.join(used.get(r['id'], []))} \\\\ \\midrule")
    return f"\\begin{{longtable}}{{{col([0.9, 2.6, 5.6, 5.2, 1.9, 5.0, 2.6])}}}\n\\toprule {head}\n" + '\n'.join(body) + "\n\\end{longtable}"


def main():
    mmap = load_map()
    with open(os.path.join(ROOT, 'claims.yaml'), encoding='utf8') as fh:
        claims = yaml.safe_load(fh)
    used = used_in(claims)
    meta = mmap['meta']
    tex = rf"""\documentclass[10pt]{{article}}
\usepackage[a4paper,landscape,margin=1.5cm]{{geometry}}
\usepackage{{fontspec}}
\setmainfont{{DejaVu Serif}}
\setmonofont{{DejaVu Sans Mono}}[Scale=0.9]
\usepackage{{amsmath,amssymb,longtable,booktabs,array,xurl}}
\usepackage[hidelinks]{{hyperref}}
\renewcommand{{\arraystretch}}{{1.25}}
\setlength{{\tabcolsep}}{{4pt}}
\setlength{{\parindent}}{{0pt}}
\setlength{{\parskip}}{{4pt}}
\title{{{escape_text(meta['title'])}}}
\date{{Pass {meta['pass']}}}
\begin{{document}}
\maketitle
{mixed(meta['scope'])} Model claims M1--M23 are those of the manuscript skeleton of the same pass. No data source is assigned in this pass.

\section*{{Support vocabulary}}
\begin{{description}}
{vocab(mmap['support_status'])}
\end{{description}}

\section{{Inputs}}
{{\small
{table(mmap['rows'], used, 'input')}}}

\section{{Outputs}}
{{\small
{table(mmap['rows'], used, 'output')}}}
\end{{document}}
"""
    with open(os.path.join(ROOT, 'measurement_map.tex'), 'w', encoding='utf8') as fh:
        fh.write(tex)


if __name__ == '__main__':
    main()
