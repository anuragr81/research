# Literature records of the theory paper

One folder per paper the theory paper cites, 20 in all, each with:

- `KEYS`, the bib key or keys of `theory/refs.bib`;
- `CLAIMS.md`, the quotations the manuscript's literature rows rely on, with pages;
- `NOTES.md`, the reading notes;
- `LEAN`, the file of `theory/lean/` that formalises the record;
- `verify_*.py`, the record's suite.

`litcheck.py` runs a suite. Each suite checks the cached source PDF in
`~/.cache/wealth_garch/` against its pinned sha256, finds each quotation on its
stated page, rejects a fabricated sentence and a wrong page, and builds and
audits the record's Lean file in `theory/lean/`. `ocr_pages.py` reads scanned
pages. `verify_lit.sh` runs every suite.

## Origin

The records, `litcheck.py` and `ocr_pages.py` are copies of `lit/` of the
`wealth_garch` bundle at commit `740823e4` (28 October 2026), the last commit
before work for another paper changed any of them. The one edit is the Lean
project `litcheck.py` builds against, `theory/lean/` in place of
`lean/mathlib/`. From here on the records are the theory paper's own, and
changes to the bundle's `lit/` do not reach them.
