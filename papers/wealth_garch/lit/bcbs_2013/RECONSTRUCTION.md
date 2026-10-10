# Reconstruction: Basel Committee on Banking Supervision (2013)

## Source read

- "Basel III: The Liquidity Coverage Ratio and liquidity risk monitoring
  tools", Basel Committee on Banking Supervision, Bank for International
  Settlements, January 2013. 75 PDF pages.
- Pages read: printed pp. 4 (paragraph 22), 7 (paragraph 23), 12 to 15
  (paragraphs 49 to 54, the definition of HQLA) and 20 to 22 (paragraphs
  69 and 73 to 79, net outflows and retail run-off). Evidence tag [F] for
  these pages only. Nothing beyond them is attributed.
- Supplied by the author on Google Drive (folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`, file
  `201301-standards-basel-iii-liquidity-coverage-ratio-and-liquidity-risk-monitoring-tools.pdf`),
  uploaded 2026-10-10. Cached at `~/.cache/wealth_garch/bcbs_lcr_2013.pdf`,
  sha256 `cb2d45abd8a04243c750d6eea841129897ad1a2f72296574ea18a394719f9c33`.
- The text layer carries no printed page numbers. Printed page = PDF page
  − 6, fixed from the table of contents ("Stock of HQLA", p. 7, opens on
  PDF p. 13; "Total net cash outflows", p. 20, on PDF p. 26; Annex 4, p. 66,
  on PDF p. 72).

## Primitives

- The stock of HQLA, in Level 1 (no haircut, no cap), Level 2A (15%
  haircut) and Level 2B (25% or 50% haircut). Level 2 assets are capped at
  40% of the stock after haircuts.
- Total net cash outflows over 30 days, outflows by run-off rates on
  liability categories, minus inflows capped at 75% of outflows.

## Derivation chain

1. Paragraph 22. LCR = HQLA / total net cash outflows over 30 days, at least
   100% absent stress.
2. Paragraphs 49 to 54. HQLA is the haircut sum of the levels, subject to
   the Level 2 cap.
3. Paragraphs 69, 73 to 79. Outflows are balances times run-off rates.
   Stable retail deposits usually receive 5% (3% under conditions), less
   stable at least 10%.

## Roles

- BCVW's liquidity constraint (BCVW-Q10 to BCVW-Q12) keeps the ratio and
  replaces each schedule by one number: a single haircut $a_3$ and a single
  run-off rate $a_2$ on all liabilities.

## What could not be reconstructed

- The method for the Level 2 cap (Annex 1, not read).
- The run-off rates for wholesale funding (paragraphs 80 onward, not read).
