# Literature records

One directory per paper read, named `<author_year>`. Each record checks **our
reading of the paper**, not the paper. The permitted conclusion is "our reading
is consistent with the source", never "the paper is verified". The layout
follows `papers/MANUSCRIPT_SKELETON.md` §6 and the reference bundle
`papers/jite/endogenous_entry/entry_contest/lit/`.

## What a record contains

| File | Role |
|---|---|
| `RECONSTRUCTION.md` | Written first. The paper rebuilt from its own primitives, the derivation chain, what each result is derived from, and what could not be reconstructed. Records the source read: version, pages, upload date, Drive id, cache path, sha256. |
| `CLAIMS.md` | Quotation table (ID, Page, Quotation, What we rely on), the Lean results with fully qualified names, and the readings recorded (`XX-D<n>`) for every discrepancy in the source. |
| `NOTES.md` | What was verified, what could not be, every discrepancy, the findings from the formalisation, and the control of each check. |
| `KEYS` | The paper's `refs.bib` keys, whitespace separated. `check.py` uses it to find the record of a literature row. |
| `LEAN` | The path of the paper's Lean file under `lean/Lit/`, a module of the `Lit` library in `lean/lakefile.toml`, built against the pinned Mathlib. Use the paper's notation. |
| `verify_<slug>.py` | The paper's suite, built on `litcheck.py`. |

## Rules

1. Every cited paper gets a Lean file (author's instruction, 10 Oct 2026),
   empirical papers included, for whatever structure we attribute to them. Any
   exemption is recorded here with its date.
2. Sources are cached outside the repository in `~/.cache/firmworkers/`, the PDF
   pinned by sha256 in the suite. Pages are the printed pages of the version
   read. A working paper read in place of the published version is said so in
   `RECONSTRUCTION.md` and marks the ledger row unverified.
3. A quotation is verbatim. The suite finds it on its own printed page by exact
   containment after normalising case, spacing, punctuation, ligatures and
   line-end hyphens. A fabricated quotation is the control and must fail.
4. The Lean block of every suite measures what it reports: the module is a
   build root and builds, the source has no `sorry`, every name cited in
   `CLAIMS.md` is declared, every declared theorem is audited with
   `#print axioms`, no axiom lies outside `propext`, `Classical.choice` and
   `Quot.sound`, and the named `control_*` theorems are present.
5. Every check must be able to fail. Each suite is mutation-tested when written,
   and `NOTES.md` says how.

## Records

| Record | Key | Version read | Status |
|---|---|---|---|
| `borjas_1992` | `Borjas1992` | QJE article, pp.123–150 | read in full, 19 quotations, 47 Lean theorems |
| `cunha_heckman_2007` | `CunhaHeckman2007` | NBER WP 12840 (Jan 2007), cited as such | read in full, 20 quotations, 23 Lean theorems |
| `gibbons_waldman_1998` | `GibbonsWaldman1999` | NBER WP 6454 (Mar 1998), scan read through Drive's OCR, cited as such | read in full, 17 quotations, 33 Lean theorems |

## Running

`./verify_lit.sh` runs every paper suite and fails if any suite fails or if a
record has no `verify_*.py`. `../verify.sh` runs it together with
`check.py --lean`.

## Exemptions

None.
