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


def observation_cell(r):
    if not str(r.get('observation', '')).strip():
        return '---'
    pend = f"\\newline pending on {', '.join(r['pending'])}" if r.get('pending') else ''
    sup = r.get('observation_support', '')
    lean = f" ({', '.join('\\nolinkurl{' + n + '}' for n in r['observation_lean'])})" if r.get('observation_lean') else ''
    return (f"{mixed(r['observation'])} \\newline \\texttt{{{escape_text(r['observation_status'])}}}{pend}"
            f" \\newline \\texttt{{{escape_text(sup)}}}{lean}. {mixed(r.get('observation_note', ''))}")


def table(rows, used, role):
    head = r'ID & Symbol & Model meaning & Real-world referent & Support & Direction & How an observer could obtain it & Used in \\ \midrule \endhead'
    body = []
    for r in rows:
        if r['role'] != role:
            continue
        prod = f"\\newline produced by {', '.join(r['produced_by'])}" if r.get('produced_by') else ''
        body.append(f"{r['id']} & {r['symbol']} \\newline \\textit{{{escape_text(r.get('term', ''))}}}{prod} & {mixed(r['meaning'])} & {mixed(r.get('referent')) or '---'} & "
                    f"\\texttt{{{escape_text(r['support'])}}} & {mixed(r.get('direction')) or '---'} & {observation_cell(r)} & "
                    f"{', '.join(used.get(r['id'], []))} \\\\ \\midrule")
    return f"\\begin{{longtable}}{{{col([0.8, 2.2, 4.0, 3.6, 1.6, 4.0, 4.6, 1.8])}}}\n\\toprule {head}\n" + '\n'.join(body) + "\n\\end{longtable}"


def terminology(mmap):
    rows = [(r['term'], r['symbol'], mixed(r['meaning']), r['id'], r.get('avoid') or [], r.get('term_status', ''))
            for r in mmap['rows']]
    rows += [(t['term'], '---', mixed(t['meaning']), ', '.join(t.get('defined_in') or []), t.get('avoid') or [],
              t.get('status', '')) for t in mmap.get('terms') or []]
    head = r'Term & Symbol & Meaning & Defined in & Not to be called & Status \\ \midrule \endhead'
    body = '\n'.join(f"{escape_text(term)} & {sym} & {mean} & {where} & {escape_text(', '.join(avoid)) or '---'} & "
                     f"\\texttt{{{escape_text(st)}}} \\\\ \\midrule"
                     for term, sym, mean, where, avoid, st in sorted(rows, key=lambda r: r[0].lower()))
    return f"\\begin{{longtable}}{{{col([3.4, 2.6, 7.4, 2.2, 5.0, 2.0])}}}\n\\toprule {head}\n{body}\n\\end{{longtable}}"


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
{mixed(meta['scope'])} Model claims M1--M23 are those of the manuscript skeleton of the same pass. Each row says how an observer could obtain the object. Each observation also carries its support. PROVEN means a Lean theorem shows the observed quantity determines the object within the model; DEFINITIONAL means the observation is the object once the referent is accepted; CONSTRUCT means the object is not measured; UNVERIFIED means neither, and no headline may rest on it. Entries marked PROPOSED await the author's confirmation; a headline resting on one is flagged, and a headline resting on a row pending on an undecided model claim is an error.

\section*{{Support vocabulary}}
\begin{{description}}
{vocab(mmap['support_status'])}

\section*{{Observation support}}

{vocab(mmap['observation_support'])}

\section*{{Observation status}}

{vocab(mmap['observation_status'])}
\end{{description}}

\section{{Terminology}}
One plain-English name per object. The model summary in the manuscript and the prose of this map use only these names; \texttt{{check.py}} rejects the names in the column Not to be called and lists the ledger rows that still use them. Terms marked PROPOSED await the author's agreement.

{{\small
{terminology(mmap)}}}

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
