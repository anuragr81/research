# Literature records

One directory per paper read, `lit/<author_year>/`, laid out as in
`papers/MANUSCRIPT_SKELETON.md` section 6 (`RECONSTRUCTION.md`, `CLAIMS.md`,
`NOTES.md`, `KEYS`, `verify_<slug>.py`, and a Lean file or a `LEAN` pointer).
Each record checks our reading of a source, never the source itself. Write
"our reading is consistent with the source", never "the paper is verified".

`verify_lit.sh` runs every record's suite and fails when a record has no suite.

Shared checks are in `litcheck.py` (LC-1 to LC-5). Each record's suite calls
it with the paper's pages, pinned sha256 and named controls. Sources are cached
under `~/.cache/paper_a/`, downloaded from the author's Drive.

| Record | Read | Lean | Rows |
|---|---|---|---|
| `coate_loury_1993/` | [F] 15 Oct 2026 | `lean/mathlib/CoateLoury1993.lean` | L1 to L3, C12, C13, C18 |
| `phelps_1972/` | [P] 15 Oct 2026, page images, quotations checked against OCR | `lean/mathlib/Phelps1972.lean` | L4, C14, C19 |
| `arrow_1971/` | [P] 15 Oct 2026, the 1971 Princeton working paper, cited as such | `lean/mathlib/Arrow1971.lean` | L5, C14 |
| `jeffrey_1983/` | [P] 15 Oct 2026, chapter 11 and the preface of the second edition, cited as such | `lean/mathlib/Jeffrey1983.lean` | L6, L7, C1 |

Scanned sources without a text layer are read from page images [P], and put
through `ocr_pages.py` (RapidOCR, not installed in the repository) so that the
suite has a text to check quotations against. Quotations are then compared
with whitespace removed, which is weaker than a check against a publisher's
text layer.
