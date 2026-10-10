# Summaries bundle

## Contents
- `SHORT_SUMMARY.tex` — under-300-word summary, 5 result codes cited.
- `EXECUTIVE_SUMMARY.tex` — full summary, all 18 result codes cited.
- `references.bib` — shared bibliography for both (Kahneman-Tversky,
  Armstrong-Brigo, Chen-Park-Wong, Cao et al., Angelis, Altinkilic-Hansen,
  Buhner-Kaserer).
- `PROOFS_v2.tex` / `PROOFS_v2.pdf` — the theory document the codes refer
  to (index of codes near the top, after the table of contents).
- `TERMINOLOGY.md` — the term/symbol table both summaries were built from.
- `THEORY_PITCH.md` — the ~150-word elevator pitch (plain markdown, not
  part of the LaTeX build).

## To compile locally
Each summary needs the standard three-pass LaTeX + BibTeX cycle, since
both cite `references.bib`:

```
pdflatex SHORT_SUMMARY.tex
bibtex SHORT_SUMMARY
pdflatex SHORT_SUMMARY.tex
pdflatex SHORT_SUMMARY.tex

pdflatex EXECUTIVE_SUMMARY.tex
bibtex EXECUTIVE_SUMMARY
pdflatex EXECUTIVE_SUMMARY.tex
pdflatex EXECUTIVE_SUMMARY.tex
```

Requires `natbib` (round-bracket citation style) and `hyperref`, both in
any standard TeX distribution. `PROOFS_v2.tex` additionally needs
`longtable` and `booktabs` for its result-code index table.

## Known open items on this content
- The Kahneman-Tversky page range (263-291 vs 263-292 depending on
  source) is unresolved — see the comment at the top of `references.bib`.
- `cao2026`'s volume/page numbers are not yet confirmed against the
  journal (arXiv confirms acceptance only).
- Both bolding passes (via `\textbf{}`) bold each term on its first
  eligible occurrence per document, not on every occurrence.
