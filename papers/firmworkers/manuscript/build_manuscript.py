import os
import re
import sys
import yaml

ROOT = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, ROOT)
from check import load_map, reference_citations, cited_map_rows

LEAN_FILES = ['lean/IscLean.lean', 'lean/IscLean/Lottery.lean', 'lean/IscLean/Ladder.lean', 'lean/IscLean/Gap.lean']
BUILD_FILES = ['lean/lean-toolchain', 'lean/lakefile.toml', 'lean/lake-manifest.json']
SYMPY_FILE = 'sympy/micro_checks.py'
SPECIAL = {'&': r'\&', '%': r'\%', '#': r'\#', '_': r'\_', '{': r'\{', '}': r'\}',
           '~': r'\textasciitilde{}', '^': r'\textasciicircum{}', '\\': r'\textbackslash{}',
           '<': r'\textless{}', '>': r'\textgreater{}'}
PATH_TOKEN = re.compile(r'([\w./-]+::[\w.]+)')
TOP_DECL = re.compile(r'^(theorem|lemma|def|noncomputable def|end|namespace|open)\b')


def escape_text(s):
    return ''.join(SPECIAL.get(ch, ch) for ch in s)


def text_part(p):
    pieces = PATH_TOKEN.split(p)
    return ''.join(f'\\nolinkurl{{{t}}}' if PATH_TOKEN.fullmatch(t) else escape_text(t) for t in pieces)


def mixed(s):
    s = str(s or '')
    parts = re.split(r'(\$[^$]*\$)', s)
    return ''.join(p if p.startswith('$') and p.endswith('$') and len(p) > 1 else text_part(p) for p in parts)


def read(rel):
    with open(os.path.join(ROOT, rel), encoding='utf8') as fh:
        return fh.read().splitlines()


def block_range(lines, start_pattern, stop_pattern):
    start = next((i for i, l in enumerate(lines) if re.match(start_pattern, l)), None)
    if start is None:
        return None
    end = len(lines) - 1
    for j in range(start + 1, len(lines)):
        if re.match(stop_pattern, lines[j]):
            end = j - 1
            break
    while end > start and not lines[end].strip():
        end -= 1
    return start + 1, end + 1


def lean_location(name):
    short = name.rpartition('.')[2]
    for idx, rel in enumerate(LEAN_FILES):
        rng = block_range(read(rel), r'^theorem\s+' + re.escape(short) + r'\b', TOP_DECL.pattern)
        if rng:
            return idx, rel, rng
    raise SystemExit(f'lean theorem {name} not found in listings')


def sympy_location(fn):
    rng = block_range(read(SYMPY_FILE), r'^def\s+' + re.escape(fn) + r'\b', r'^(def|CHECKS|if __name__)\b')
    if not rng:
        raise SystemExit(f'sympy function {fn} not found')
    return rng


def proof_cell(c):
    parts = []
    if c.get('lean'):
        idx, rel, (a, b) = lean_location(c['lean'])
        parts.append(f"Lean \\nolinkurl{{{c['lean']}}}, Listing A.{idx + 1} (\\nolinkurl{{{os.path.basename(rel)}}}), lines {a}--{b}")
    for fn in re.findall(r'micro_checks\.py::(\w+)', c.get('evidence', '')):
        a, b = sympy_location(fn)
        parts.append(f"SymPy \\nolinkurl{{{fn}}}, Listing C.1, lines {a}--{b}")
    return '; '.join(parts) if parts else '---'


def evidence_text(c):
    text = re.sub(r'(lean|sympy)/[\w/]+\.(lean|py)::[\w.]+\.?\s*', '', str(c.get('evidence', ''))).strip()
    text = re.sub(r'^Cross-check\s*', '', text).strip()
    return mixed(text) if text else '---'


def col(widths):
    return ' '.join(f'>{{\\raggedright\\arraybackslash}}p{{{w}cm}}' for w in widths)


def model_table(items):
    head = r'ID & Claim & Assumptions & Manuscript anchor & Evidence & Proof location & Status \\ \midrule \endhead'
    rows = '\n'.join(
        f"{c['id']} & {c['claim']} & {mixed(c['assumptions'])} & {mixed(c['anchor'])} & {evidence_text(c)} & "
        f"{proof_cell(c)} & \\texttt{{{escape_text(c['status'])}}} \\\\ \\midrule" for c in items)
    return f"\\begin{{longtable}}{{{col([0.8, 5.0, 2.9, 3.9, 4.4, 3.6, 2.5])}}}\n\\toprule {head}\n{rows}\n\\end{{longtable}}"


def literature_table(items):
    head = r'ID & Source & Claim & Evidence seen & Verbatim quote & Status \\ \midrule \endhead'
    rows = []
    for c in items:
        copy = f"\\newline \\url{{{c['open_copy']}}}" if c.get('open_copy') else ''
        quote = mixed(c.get('quote')) or '---'
        page = f" (p.~{escape_text(str(c['page']))})" if c.get('page') else ''
        rows.append(f"{c['id']} & {c['source']} & {c['claim']} & {mixed(c['evidence_seen'])}{copy} & {quote}{page} & "
                    f"\\texttt{{{escape_text(c['status'])}}} \\\\ \\midrule")
    return f"\\begin{{longtable}}{{{col([0.8, 4.6, 6.6, 5.2, 3.4, 2.5])}}}\n\\toprule {head}\n" + '\n'.join(rows) + "\n\\end{longtable}"


def derived_table(items, empty):
    if not items:
        return empty
    head = r'ID & Claim & Rests on \\ \midrule \endhead'
    rows = '\n'.join(f"{c['id']} & {c['claim']} & {', '.join(c['rests_on'])} \\\\ \\midrule" for c in items)
    return f"\\begin{{longtable}}{{{col([1.0, 17.0, 5.0])}}}\n\\toprule {head}\n{rows}\n\\end{{longtable}}"


def references_table(data, cited):
    source = {}
    for c in data['literature']:
        source.setdefault(c['ref'], c['source'])
    head = r'Key & Full citation & Why the manuscript depends on it & Impact if not cited & Cited by & Maths / Lean \\ \midrule \endhead'
    rows = []
    for r in data['references']:
        k = r['key']
        by = ', '.join(cited[k]) if cited[k] else 'none'
        rows.append(f"\\nolinkurl{{{k}}} & {source[k]} & {mixed(r['reason'])} & {mixed(r['impact_if_removed'])} & {by} & "
                    f"{escape_text(r['math_content'])} / \\texttt{{{escape_text(r['lean'])}}} \\\\ \\midrule")
    return f"\\begin{{longtable}}{{{col([2.7, 5.0, 5.4, 5.4, 1.8, 2.6])}}}\n\\toprule {head}\n" + '\n'.join(rows) + "\n\\end{longtable}"


def vocab(d):
    return '\n'.join(f"\\item[\\texttt{{{escape_text(k)}}}] {mixed(v)}" for k, v in d.items())


def listing(rel, label, size='footnotesize'):
    return (f"\\subsection*{{Listing {label}: \\nolinkurl{{{rel}}}}}\n"
            f"\\VerbatimInput[numbers=left,numbersep=4pt,fontsize=\\{size},frame=single]{{{rel}}}\n")


def cited_rows_appendix(data, mmap):
    ids = cited_map_rows(data)
    if not ids:
        return 'No headline or concluding claim cites a measurement-map row in this pass.'
    rows = {r['id']: r for r in mmap['rows']}
    body = '\n'.join(f"{i} & {rows[i]['symbol']} & {mixed(rows[i]['meaning'])} & {mixed(rows[i]['referent'])} & "
                     f"\\texttt{{{rows[i]['support']}}} \\\\ \\midrule" for i in ids)
    return (f"\\begin{{longtable}}{{{col([1.0, 2.6, 7.0, 7.0, 2.4])}}}\n\\toprule ID & Symbol & Model meaning & Real-world referent & Support "
            f"\\\\ \\midrule \\endhead\n{body}\n\\end{{longtable}}")


def main():
    with open(os.path.join(ROOT, 'claims.yaml'), encoding='utf8') as fh:
        d = yaml.safe_load(fh)
    mmap = load_map()
    rows = {r['id']: r for r in mmap['rows']}
    cited = reference_citations(d, rows)
    meta = d['meta']
    lean_listings = '\n'.join(listing(rel, f'A.{i + 1}') for i, rel in enumerate(LEAN_FILES))
    build_listings = '\n'.join(listing(rel, f'B.{i + 1}', 'scriptsize') for i, rel in enumerate(BUILD_FILES))
    tex = rf"""\documentclass[10pt]{{article}}
\usepackage[a4paper,landscape,margin=1.5cm]{{geometry}}
\usepackage{{fontspec}}
\setmainfont{{DejaVu Serif}}
\setmonofont{{DejaVu Sans Mono}}[Scale=0.9]
\usepackage{{amsmath,amssymb,longtable,booktabs,array,xurl,fancyvrb}}
\usepackage[hidelinks]{{hyperref}}
\renewcommand{{\arraystretch}}{{1.25}}
\setlength{{\tabcolsep}}{{4pt}}
\setlength{{\parindent}}{{0pt}}
\setlength{{\parskip}}{{4pt}}
\title{{{escape_text(meta['title'])}}}
\date{{Pass {meta['pass']}}}
\begin{{document}}
\maketitle
Source manuscript for the model claims: {mixed(meta['manuscript'])}.

Headline and concluding claims may rest only on model claims, literature claims and measurement-map rows. Model claims are proved in Lean 4 against Mathlib. The full Lean source is in Appendix~A and the build files in Appendix~B.

\section*{{Status vocabulary}}
\begin{{description}}
{vocab(d['model_status'])}
{vocab(d['literature_status'])}
\end{{description}}

\section{{Headline claims}}
{derived_table(d.get('headline') or [], 'No headline claims are admitted in this pass.')}

\section{{Model claims}}
{{\small
{model_table(d['model'])}}}

\section{{Literature claims}}
{{\small
{literature_table(d['literature'])}}}

\section{{Concluding remarks}}
{derived_table(d.get('concluding') or [], 'No concluding claims are admitted in this pass.')}

\section{{References}}
{{\small
{references_table(d, cited)}}}

\appendix
\section{{Lean source}}
{lean_listings}
\section{{Lean build files}}
{build_listings}
\section{{SymPy evidence}}
{listing(SYMPY_FILE, 'C.1')}
\section{{Measurement-map rows cited by headline and concluding claims}}
{cited_rows_appendix(d, mmap)}
\end{{document}}
"""
    with open(os.path.join(ROOT, 'manuscript.tex'), 'w', encoding='utf8') as fh:
        fh.write(tex)


if __name__ == '__main__':
    main()
