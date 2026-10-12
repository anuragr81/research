# Reconstruction: Hirtle (2016)

## Source read

- Beverly Hirtle, "Bank Holding Company Dividends and Repurchases during
  the Financial Crisis", Federal Reserve Bank of New York Staff Report 666,
  March 2014, revised April 2016. 37 PDF pages: a cover, an abstract page,
  then pages 1 to 35 (printed page = PDF page - 2). Read in full. Evidence
  tag [F].
- Supplied by the author on Google Drive (folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`, file `sr666.pdf`, id
  `1NYK20bEeSnDkvmHb7hc8L8sqzIdG0gwb`), uploaded 2026-10-12. Cached at
  `~/.cache/wealth_garch/hirtle_2016.pdf`, sha256
  `a053fd02846c45b1f3889762fc384346845861634440526257a872825d4efa5e`.

## Primitives

- Unit: top-tier U.S. bank holding companies with assets above $5 billion
  in Q1 2005, FR Y-9C, quarterly 2005-2009; 66 in the estimation sample,
  42 smaller and 24 larger (split at $25 billion) (pp. 9-10).
- Repurchases: treasury stock purchases plus net retirements over
  conversions, if positive (p. 8).
- A dividend cut: a fall in dividends per share and dividends over assets
  beyond the 5th percentile of 2003-2006 changes; elimination: dividends
  per share at most $0.011 (p. 9, nn. 10-11).

## Derivation chain

1. Aggregates (Figures 1-3): dividends held near pre-crisis levels into
   late 2008; repurchases fell to de minimis levels by mid-2008.
2. Counts (Tables 1-3): 48 of 66 cut dividends, 30 eliminated them; larger
   holding companies cut more often.
3. Cox hazards (Tables 5-11): higher pre-crisis repurchases delay cuts and
   eliminations at smaller holding companies; higher capital ratios delay
   both; pre-crisis total payout speeds the first cut.
4. Size of cuts (Tables 12-13): first cuts average about 65%, total cuts
   above 90%; higher repurchases mean smaller total cuts at smaller
   holding companies.

## Roles in this bundle

- Holding companies kept paying dividends while losses accumulated (H-Q1):
  payouts well inside the region where a barrier model pays nothing.
- Cuts arrive sooner at lower capital (H-Q7) and are large once made
  (H-Q8): a graded hazard of cutting, then a lumpy cut.
- The author's reading is precautionary capital management against a
  signalling cost of cutting dividends (H-Q4), a cost absent from the
  baseline models.

## What could not be reconstructed

- The hazard and selection estimates; the figures.
