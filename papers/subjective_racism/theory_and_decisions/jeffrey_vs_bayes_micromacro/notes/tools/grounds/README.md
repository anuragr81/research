# Grounds for the corrections

One Python file per group, each defining `GROUNDS = {entry_id: [item, ...]}`.
The generator (`notes/tools/make_corrections_tex.py`) merges every
`grounds_*.py` here and renders a **Grounds** block under the Why of each
entry of `notes/manuscript_corrections.tex`.

An item is a dict with

- `kind`: `"quote"` (verbatim excerpt from a source), `"theorem"` (a Lean
  theorem, its statement rendered in mathematical notation) or
  `"computation"` (a sympy identity or numbers).
- `source`: for a quote, author, year, page (and section/theorem/footnote
  where useful); for a theorem, `File.lean, theorem_name`; for a
  computation, the script and check.
- `text`: LaTeX. For a quote, the excerpt verbatim inside ``...'' with
  `\ldots` marking any elision; keep it to the sentence(s) that carry the
  point. For a theorem, the statement in math notation, faithful to the Lean
  (hypotheses included). For a computation, the identity or the numbers.
- `note` (optional): one sentence saying what the item establishes for the
  entry.

Every quote is checked against the paper before it enters; a quote that
cannot be found verbatim does not enter.
