import os
import re
import sys
import yaml

ROOT = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, ROOT)
from check import load_map, reference_citations, cited_map_rows, lean_names

LEAN_FILES = ['lean/IscLean.lean', 'lean/IscLean/Lottery.lean', 'lean/IscLean/Ladder.lean', 'lean/IscLean/Gap.lean', 'lean/IscLean/Moments.lean']
BUILD_FILES = ['lean/lean-toolchain', 'lean/lakefile.toml', 'lean/lake-manifest.json']
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


def proof_cell(c):
    parts = []
    for name in lean_names(c):
        idx, rel, (a, b) = lean_location(name)
        parts.append(f"Lean \\nolinkurl{{{name}}}, Listing A.{idx + 1} (\\nolinkurl{{{os.path.basename(rel)}}}), lines {a}--{b}")
    return '; '.join(parts) if parts else '---'


def by_status(d, *statuses):
    return [c for c in d['model'] if c['status'] in statuses]


def evidence_text(c):
    text = re.sub(r'lean/[\w/]+\.lean::[\w.]+\.?\s*', '', str(c.get('evidence', ''))).strip()
    return mixed(text) if text else '---'


def col(widths):
    return ' '.join(f'>{{\\raggedright\\arraybackslash}}p{{{w}cm}}' for w in widths)


def model_table(items):
    head = r'ID & Claim & Assumptions & Manuscript anchor & Evidence & Proof location & Status \\ \midrule \endhead'
    rows = '\n'.join(
        f"{c['id']} & {c['claim']} & {mixed(c['assumptions'])} & {mixed(c['anchor'])} & {evidence_text(c)} & "
        f"{proof_cell(c)} & \\texttt{{{escape_text(c['status'])}}} \\\\ \\midrule" for c in items)
    return f"\\begin{{longtable}}{{{col([0.8, 5.0, 2.9, 3.9, 4.4, 3.6, 2.5])}}}\n\\toprule {head}\n{rows}\n\\end{{longtable}}"


def pending_table(items):
    head = r'ID & Claim stated in the source & Manuscript anchor & Related rows \\ \midrule \endhead'
    rows = '\n'.join(
        f"{c['id']} & {c['claim']}. \\pend{{{mixed(c['waits_on'])}}} & {mixed(c['anchor'])} & "
        f"{', '.join(c.get('rows') or []) or '---'} \\\\ \\midrule" for c in items)
    return f"\\begin{{longtable}}{{{col([0.8, 10.6, 7.4, 4.2])}}}\n\\toprule {head}\n{rows}\n\\end{{longtable}}"


def primitives_table(items):
    head = r'ID & Claim the source needs & Manuscript anchor & What the source leaves undefined & Rows that depend on it & Decision \\ \midrule \endhead'
    rows = '\n'.join(
        f"{c['id']} & {c['claim']} & {mixed(c['anchor'])} & {evidence_text(c)} & {', '.join(c['affects'])} & "
        f"pending, see \\nolinkurl{{TODO.md}} \\\\ \\midrule" for c in items)
    return f"\\begin{{longtable}}{{{col([0.8, 4.6, 5.0, 6.6, 2.6, 2.8])}}}\n\\toprule {head}\n{rows}\n\\end{{longtable}}"


def refuted_table(items):
    head = r'ID & Conjecture as printed & Manuscript anchor & Why it fails & Lean counterexample & Repair or refuting rows & Status \\ \midrule \endhead'
    rows = []
    for c in items:
        answer = c.get('repaired_by') or re.findall(r'\bM\d+\b', c['evidence'])
        rows.append(f"{c['id']} & {c['claim']} & {mixed(c['anchor'])} & {evidence_text(c)} & {proof_cell(c)} & "
                    f"{', '.join(answer)} & \\texttt{{{escape_text(c['status'])}}} \\\\ \\midrule")
    return f"\\begin{{longtable}}{{{col([0.8, 3.8, 3.9, 5.2, 4.2, 2.2, 2.3])}}}\n\\toprule {head}\n" + '\n'.join(rows) + "\n\\end{longtable}"


def short_cite(c):
    m = re.match(r"(.+?\(\d{4}\))", c['source'])
    return m.group(1) if m else c['source']


def literature_table(d, rows_by_lit):
    read = [c for c in d['literature'] if c['status'] == 'VERBATIM']
    unread = [c for c in d['literature'] if c['status'] != 'VERBATIM']
    head = r'ID & Paper & What we rely on & Quote & Page & Rows \\ \midrule \endhead'
    body = '\n'.join(
        f"{c['id']} & {c['source']} & {c['claim']} & ``{mixed(c['quote'])}'' & {escape_text(str(c['page']))} & "
        f"{', '.join(rows_by_lit.get(c['id'], [])) or '---'} \\\\ \\midrule" for c in read)
    table = (f"\\begin{{longtable}}{{{col([0.8, 5.4, 5.6, 7.0, 1.2, 2.0])}}}\n\\toprule {head}\n{body}\n\\end{{longtable}}"
             if read else 'No paper has been read in full in this pass.')
    names = ', '.join(f"{c['id']} {short_cite(c)}" for c in unread)
    tail = (f"Papers cited but not yet read in full are {names}. Every claim made about them is unverified."
            if unread else '')
    return f"{table}\n\n{tail}"


def literature_rows(d):
    out = {}
    for c in d['model']:
        for lid in re.findall(r'\bL\d+\b', str(c.get('evidence', '')) + ' ' + str(c.get('assumptions', ''))):
            out.setdefault(lid, []).append(c['id'])
    for section in ('headline', 'concluding'):
        for c in d.get(section) or []:
            for r in c.get('rests_on') or []:
                if r.startswith('L'):
                    out.setdefault(r, []).append(c['id'])
    return out


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
\newcommand{{\pend}}[1]{{\textit{{Pending:}} #1}}
\renewcommand{{\arraystretch}}{{1.25}}
\setlength{{\tabcolsep}}{{4pt}}
\setlength{{\parindent}}{{0pt}}
\setlength{{\parskip}}{{4pt}}
\title{{{escape_text(meta['title'])}}}
\date{{Pass {meta['pass']}}}
\begin{{document}}
\maketitle
Source manuscript for the model claims: {mixed(meta['manuscript'])}.

Headline and concluding claims may rest only on model claims, literature claims and measurement-map rows. Model claims are proved in Lean 4 against Mathlib. The full Lean source is in Appendix~A and the build files in Appendix~B. Claims the source manuscript needs but leaves undefined are in Appendix~D. Claims it prints that fail as printed are in Appendix~E, each with its Lean counterexample and the rows that repair or refute it.

\section*{{Status vocabulary}}
\begin{{description}}
{vocab(d['model_status'])}
{vocab(d['literature_status'])}
\end{{description}}

\section{{Headline claims}}
{derived_table(d.get('headline') or [], 'No headline claims are admitted in this pass.')}

\section{{Model claims}}
{{\small
{model_table(by_status(d, 'LEAN_PROVED', 'LEAN_WRITTEN'))}}}

\section{{Literature claims}}
{{\small
{literature_table(d, literature_rows(d))}}}

\section{{Concluding remarks}}
\subsection*{{Conclusions resting on verified rows}}
{derived_table(d.get('concluding') or [], 'No concluding claims are admitted in this pass.')}
\subsection*{{Pending}}
{{\small
{pending_table(by_status(d, 'OPEN'))}}}

\section{{References}}
{{\small
{references_table(d, cited)}}}

\appendix
\section{{Lean source}}
{lean_listings}
\section{{Lean build files}}
{build_listings}
\section{{Measurement-map rows cited by headline and concluding claims}}
{cited_rows_appendix(d, mmap)}
\section{{Primitives and scope}}
{{\small
{primitives_table(by_status(d, 'UNDERSPECIFIED'))}}}
\section{{Refuted conjectures}}
{{\small
{refuted_table(by_status(d, 'ILL_POSED', 'REFUTED'))}}}
\end{{document}}
"""
    with open(os.path.join(ROOT, 'manuscript.tex'), 'w', encoding='utf8') as fh:
        fh.write(tex)


if __name__ == '__main__':
    main()
