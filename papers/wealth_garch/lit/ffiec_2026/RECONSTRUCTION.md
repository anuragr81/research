# Reconstruction: FFIEC (2026)

## Source read

- "FFIEC 031 and 041, Schedule RC-R, Regulatory Capital", the Call Report
  instructions for Schedule RC-R Part I (Regulatory Capital Components and
  Ratios), Federal Financial Institutions Examination Council. Page stamps
  run to (6-26). PDF metadata created 4 June 2026. 73 PDF pages.
- Pages read: PDF pp. 38 (items 18 and 19, printed RC-R-30), 47 (item 26,
  RC-R-39), 51 (items 30 and 31, RC-R-43) and 67 to 68 (items 48 to 51,
  RC-R-55 to RC-R-56). Evidence tag [F] for these pages only. Nothing
  beyond them is attributed.
- Supplied by the author on Google Drive (folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`, file
  `ffiec-031-041-schedule-rc-r-part-i-june-2026.pdf`), uploaded 2026-10-10.
  Cached at `~/.cache/wealth_garch/ffiec_rcr_2026.pdf`, sha256
  `6c56f2fb5a51d788835da01cfa1bfd9956c0a91595f618946d98b5a9c5f7d944`.
- Printed labels are not a fixed offset from PDF pages (RC-R-39 is PDF p. 47,
  RC-R-55 is PDF p. 67), so `CLAIMS.md` cites PDF pages.

## Primitives

- Item 19, CET1 capital (item 12 less item 18). Item 26, Tier 1 capital
  (items 19 and 25). Item 30, total assets for the leverage ratio. Item 48,
  total risk-weighted assets.

## Derivation chain

1. Item 31. Leverage ratio = item 26 / item 30.
2. Items 49 and 50. On the FFIEC 041, CET1 ratio = item 19 / item 48 and
   Tier 1 ratio = item 26 / item 48.
3. Item 19. For a bank electing the community bank leverage ratio, item 19
   is not the numerator of a reported risk-based ratio.

## Roles

- The reporting counterparts of the Basel ratios for US banks. The FDIC
  field names used by the empirical scripts (RWAJT, RBCT1J, ASSET, LIAB) do
  not appear in this document. Their link to these items needs the FDIC
  data dictionary, which was not supplied.

## What could not be reconstructed

- Items 27 to 29 (the adjustments to total assets for the leverage ratio)
  and Part II (the risk weights). Not read.
