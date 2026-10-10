# Literature records

One directory `lit/<author_year>/` per paper read. Each record checks our
reading of the paper and does not reprove it. The permitted conclusion is
"our reading is consistent with the source", never "the paper is verified".

## What a record contains

| File | Role |
|---|---|
| `RECONSTRUCTION.md` | Written first. The paper rebuilt from its own primitives, the derivation chain, and what each result is derived from. Ends with what could not be reconstructed. Records the source read (version, pages, upload date, cache path). |
| `CLAIMS.md` | Quotation table (ID, Page, Quotation, What we rely on); table of the Lean results; "Readings recorded" table giving an ID such as `XX-D1` to every discrepancy or typo in the source, with the reading adopted and its reason. |
| `NOTES.md` | What was verified, what could not be, every discrepancy, and the control of each check. |
| `KEYS` | The paper's bib keys, whitespace-separated. `checks/verify_manuscript.py` finds the record through this file. |
| `verify_<slug>.py` | The paper's suite. |
| Lean | `<Slug>.lean` in the directory (core Lean), or a file `LEAN` naming a file in `lean/mathlib/`. The paper's own notation. |

## Evidence tags

| Tag | Meaning |
|---|---|
| [F] | Full text read. |
| [P] | Page images from the author's own copy read, pinned by sha256. Nothing beyond the pages read is attributed. |
| [A] | Abstract or publisher summary only. Cannot support a positioning claim. |
| [C] | Known only through a citing paper's description. Cannot support a positioning claim. |

## What each suite checks

1. The quotation rows parse, and their pages lie in the paper's range.
2. Each quotation is found in the cached text by exact normalised
   containment. A fabricated quotation is the control and must fail.
3. The cached file matches its pinned sha256.
4. The Lean file is a build root, builds, has no `sorry`, declares every Lean
   name cited in `CLAIMS.md`, and every theorem passes the axiom audit.
5. The named controls are present.

Source PDFs and their extracted text are cached outside the repository under
`~/.cache/wealth_garch/`.

## Exemptions from Lean

None.

## Running

`./verify_lit.sh` runs every paper suite and fails if a suite fails, if a
record has no `verify_*.py`, or if no suite ran.
