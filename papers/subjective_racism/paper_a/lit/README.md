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
