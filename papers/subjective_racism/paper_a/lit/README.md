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
positioning of Paper A1. In a second pass on 16 October 2026 each record's
Lean file was extended to implement the source's model, and each `CLAIMS.md`
gained a table, "Coverage of the source's results", that lists every numbered
result as proved in general, proved in a special case, or not formalised with a
reason. What each one shows for the candidate contributions
is in `notes/motivation_scan.md`.

| Record | Read | Lean | Rows |
|---|---|---|---|
| `phelps_1972/` | [P] 15 Oct 2026, page images, quotations checked against OCR | `lean/mathlib/Phelps1972.lean` | L4, C14, C19 |
| `jeffrey_1983/` | [P] 15 Oct 2026, chapter 11 and the preface of the second edition, cited as such | `lean/mathlib/Jeffrey1983.lean` | Record only. Rows L6, L7 and the Jeffrey clause of C1 removed 17 Oct 2026 (D8) |
| `cole_mailath_postlewaite_1991/` | [F] 16 Oct 2026, CARESS working paper 91-14, cited as such | `lean/mathlib/ColeEtAl1991.lean` | none yet |
| `pesendorfer_1995/` | [F] 16 Oct 2026 | `lean/mathlib/Pesendorfer1995.lean` | L19, C22, L24 |
| `eeckhout_2006/` | [F] 16 Oct 2026 | `lean/mathlib/Eeckhout2006.lean` | none yet |
| `mailath_postlewaite_2006/` | [F] 16 Oct 2026, PIER working paper 06-003, with 04-025 and 02-003 read for differences | `lean/mathlib/MailathPostlewaite2006.lean` | L21 |
| `fryer_jackson_2008/` | [F] 16 Oct 2026 | `lean/mathlib/FryerJackson2008.lean` | L13 |
| `austen_smith_fryer_2003/` | [F] 16 Oct 2026, NBER working paper 9904, cited as such | `lean/mathlib/AustenSmithFryer2003.lean` | L18, C22 |
| `charles_hurst_roussanov_2007/` | [F] 16 Oct 2026, NBER working paper 13392, cited as such | `lean/mathlib/CharlesEtAl2007.lean` | L11, L37 |
| `corneo_jeanne_1994/` | [F] 16 Oct 2026, Bonn discussion paper A-461, cited as such | `lean/mathlib/CorneoJeanne1994.lean` | L17 |
| `moav_neeman_2012/` | [F] 16 Oct 2026 | `lean/mathlib/MoavNeeman2012.lean` | none yet |
| `kim_loury_2009/` | [F] 16 Oct 2026, MPRA paper 18765, cited as such | `lean/mathlib/KimLoury2009.lean` | none yet |
| `kim_loury_2012/` | [F] 16 Oct 2026, the KDI article, pp. 1 to 15 of the issue | `lean/mathlib/KimLoury2012.lean` | L29 |
| `kim_loury_2014/` | [F] 16 Oct 2026, MPRA paper 54950, cited as such | `lean/mathlib/KimLoury2014.lean` | L23 |
| `peski_szentes_2012/` | [F] 16 Oct 2026, BFI working paper 2012-005, cited as such | `lean/mathlib/PeskiSzentes2012.lean` | none yet |
| `eguia_2013/` | [F] 16 Oct 2026, the NYU working paper of 10 September 2013, cited as such | `lean/mathlib/Eguia2013.lean` | L22, L36, L38 |
| `craig_fryer_2017/` | [F] 16 Oct 2026, NBER working paper 23811 | `lean/mathlib/CraigFryer2017.lean` | L16 |
| `labov_1964/` | [F] 16 Oct 2026, the Columbia dissertation, all 646 pages from OCR, cited as such | `lean/mathlib/Labov1964.lean` | none yet |
| `aigner_cain_1977/` | [F] 16 Oct 2026 | `lean/mathlib/AignerCain1977.lean` | none yet |
| `mcpherson_smith_lovin_cook_2001/` | [F] 16 Oct 2026 | `lean/mathlib/McPhersonEtAl2001.lean` | none yet |
| `bertrand_mullainathan_2004/` | [F] 16 Oct 2026, the article only, not the cited-by list that follows it in the PDF | `lean/mathlib/BertrandMullainathan2004.lean` | none yet |
| `hitsch_hortacsu_ariely_2006/` | [F] 16 Oct 2026, MIT Sloan working paper 4603-06, cited as such | `lean/mathlib/HitschEtAl2006.lean` | L12, L35 |
| `currarini_jackson_pin_2008/` | [F] 16 Oct 2026, the working paper of 29 July 2008, cited as such | `lean/mathlib/CurrariniEtAl2008.lean` | L20 |
| `fang_moro_2010/` | [F] 16 Oct 2026, NBER working paper 15860, cited as such | `lean/mathlib/FangMoro2010.lean` | L14, L15 |
| `peng_zhang_pan_2010/` | [F] 16 Oct 2026 | `lean/mathlib/PengEtAl2010.lean` | none yet |
| `mill_stein_2012/` | [F] 16 Oct 2026, the working paper of 30 December 2012, cited as such | `lean/mathlib/MillStein2012.lean` | none yet |
| `kraus_park_tan_2017/` | [F] 16 Oct 2026, the accepted manuscript | `lean/mathlib/KrausEtAl2017.lean` | none yet |
| `edelman_luca_svirsky_2017/` | [F] 16 Oct 2026, the accepted version | `lean/mathlib/EdelmanEtAl2017.lean` | L30 |
| `rothschild_white_1993/` | [F] 16 Oct 2026, the NBER chapter, not the 1995 article | `lean/mathlib/RothschildWhite1993.lean` | L39 |
| `grogger_2018/` | [F] 16 Oct 2026, the working paper Speech and Wages, version 8, not the 2011 article | `lean/mathlib/Grogger2018.lean` | none yet |
| `bohren_imas_rosenberg_2019/` | [F] 16 Oct 2026, the working paper of January 2019, cited as such | `lean/mathlib/BohrenEtAl2019.lean` | none yet |
| `dahis_nix_qian_2019/` | [F] 16 Oct 2026, NBER working paper 26465 as revised in June 2020 | `lean/mathlib/DahisEtAl2019.lean` | L28 |
| `kraus_et_al_2019/` | [F] 16 Oct 2026, the PubMed Central text without the SI appendix | `lean/mathlib/KrausEtAl2019.lean` | none yet |
| `levon_et_al_2020/` | [F] 16 Oct 2026, the Accent Bias Britain report | `lean/mathlib/LevonEtAl2020.lean` | none yet |
| `chetty_et_al_2022a/` | [F] 16 Oct 2026, Social Capital I in Nature | `lean/mathlib/ChettyEtAl2022a.lean` | none yet |
| `chetty_et_al_2022b/` | [F] 16 Oct 2026, Social Capital II in Nature, with the NBER version read for differences | `lean/mathlib/ChettyEtAl2022b.lean` | L25, L26, L27 |
| `crabtree_et_al_2022/` | [F] 16 Oct 2026 | `lean/mathlib/CrabtreeEtAl2022.lean` | none yet |
| `laouenan_rathelot_2022/` | [F] 16 Oct 2026, with the WRAP manuscript read for differences | `lean/mathlib/LaouenanRathelot2022.lean` | L31 |
| `levon_sharma_ilbury_2022/` | [F] 16 Oct 2026, the Sutton Trust report Speaking Up | `lean/mathlib/LevonEtAl2022.lean` | none yet |
| `bohren_hull_imas_2024/` | [F] 16 Oct 2026, NBER working paper 29820 as revised in December 2024 | `lean/mathlib/BohrenEtAl2024.lean` | none yet |
| `goffman_1951_es/` | [F] 16 Oct 2026, the authorised Spanish translation only, set aside at the author's direction on 17 Oct 2026 and not cited | `lean/mathlib/Goffman1951es.lean` | none yet |
| `hanson_hawley_2011/` | [F] 17 Oct 2026, Experimental Economics Center working paper 2011-05, cited as such | `lean/mathlib/HansonHawley2011.lean` | none yet |
| `akerlof_1997/` | [F] 17 Oct 2026, the JSTOR copy of the published article, supplied by the author | `lean/mathlib/Akerlof1997.lean` | L32, L33, L34, C22 |

Where a cached text interleaves two columns, the record checks quotations
against a reading-order extraction made with `pdftotext` from the same pinned
PDF, as the Chetty records do. The suite pins the PDF and that extraction.

Scanned sources without a text layer are read from page images [P], and put
through `ocr_pages.py` (RapidOCR, not installed in the repository) so that the
suite has a text to check quotations against. Quotations are then compared
with whitespace removed, which is weaker than a check against a publisher's
text layer.
