# Literature records

One directory per paper read, `lit/<author_year>/`, laid out as in
`papers/MANUSCRIPT_SKELETON.md` section 6 (`RECONSTRUCTION.md`, `CLAIMS.md`,
`NOTES.md`, `KEYS`, `verify_<slug>.py`, and a Lean file or a `LEAN` pointer).
Each record checks our reading of a source, never the source itself. Write
"our reading is consistent with the source", never "the paper is verified".

`verify_lit.sh` runs every record's suite and fails when a record has no suite.

Shared checks are in `litcheck.py` (LC-1 to LC-5). Each record's suite calls
it with the paper's pages, pinned sha256 and named controls. Sources are cached
under `~/.cache/paper_a/`, downloaded from the author's Drive or, for the
records of 16 October 2026, from the open sources listed in
`notes/source_inventory.md`. A record with no rows yet bears on the
positioning of Paper A1. What each one shows for the candidate contributions
is in `notes/motivation_scan.md`.

| Record | Read | Lean | Rows |
|---|---|---|---|
| `phelps_1972/` | [P] 15 Oct 2026, page images, quotations checked against OCR | `lean/mathlib/Phelps1972.lean` | L4, C14, C19 |
| `jeffrey_1983/` | [P] 15 Oct 2026, chapter 11 and the preface of the second edition, cited as such | `lean/mathlib/Jeffrey1983.lean` | L6, L7, C1 |
| `cole_mailath_postlewaite_1991/` | [F] 16 Oct 2026, CARESS working paper 91-14, cited as such | `lean/mathlib/ColeEtAl1991.lean` | none yet |
| `pesendorfer_1995/` | [F] 16 Oct 2026 | `lean/mathlib/Pesendorfer1995.lean` | none yet |
| `eeckhout_2006/` | [F] 16 Oct 2026 | `lean/mathlib/Eeckhout2006.lean` | none yet |
| `mailath_postlewaite_2006/` | [F] 16 Oct 2026, PIER working paper 06-003, with 04-025 and 02-003 read for differences | `lean/mathlib/MailathPostlewaite2006.lean` | none yet |
| `fryer_jackson_2008/` | [F] 16 Oct 2026 | `lean/mathlib/FryerJackson2008.lean` | none yet |
| `austen_smith_fryer_2003/` | [F] 16 Oct 2026, NBER working paper 9904, cited as such | `lean/mathlib/AustenSmithFryer2003.lean` | none yet |
| `charles_hurst_roussanov_2007/` | [F] 16 Oct 2026, NBER working paper 13392, cited as such | `lean/mathlib/CharlesEtAl2007.lean` | none yet |
| `corneo_jeanne_1994/` | [F] 16 Oct 2026, Bonn discussion paper A-461, cited as such | `lean/mathlib/CorneoJeanne1994.lean` | none yet |
| `moav_neeman_2012/` | [F] 16 Oct 2026 | `lean/mathlib/MoavNeeman2012.lean` | none yet |
| `kim_loury_2009/` | [F] 16 Oct 2026, MPRA paper 18765, cited as such | `lean/mathlib/KimLoury2009.lean` | none yet |
| `kim_loury_2012/` | [F] 16 Oct 2026, the KDI article, pp. 1 to 15 of the issue | `lean/mathlib/KimLoury2012.lean` | none yet |
| `kim_loury_2014/` | [F] 16 Oct 2026, MPRA paper 54950, cited as such | `lean/mathlib/KimLoury2014.lean` | none yet |
| `peski_szentes_2012/` | [F] 16 Oct 2026, BFI working paper 2012-005, cited as such | `lean/mathlib/PeskiSzentes2012.lean` | none yet |
| `eguia_2013/` | [F] 16 Oct 2026, the NYU working paper of 10 September 2013, cited as such | `lean/mathlib/Eguia2013.lean` | none yet |
| `craig_fryer_2017/` | [F] 16 Oct 2026, NBER working paper 23811 | `lean/mathlib/CraigFryer2017.lean` | none yet |

Scanned sources without a text layer are read from page images [P], and put
through `ocr_pages.py` (RapidOCR, not installed in the repository) so that the
suite has a text to check quotations against. Quotations are then compared
with whitespace removed, which is weaker than a check against a publisher's
text layer.
