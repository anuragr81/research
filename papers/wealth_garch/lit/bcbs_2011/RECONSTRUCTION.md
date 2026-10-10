# Reconstruction: Basel Committee on Banking Supervision (2011)

## Source read

- "Basel III: A global regulatory framework for more resilient banks and
  banking systems", Basel Committee on Banking Supervision, Bank for
  International Settlements, December 2010 (rev June 2011). 77 PDF pages.
  Each page carries a banner saying that the December 2017 revisions affect
  parts of the publication. Those revisions were not read.
- Pages read: printed pp. 12 (paragraphs 48 to 50, the minimum
  requirements), 54 to 56 (paragraphs 122 to 132, the capital conservation
  buffer) and 61 (paragraph 153, the leverage ratio). Evidence tag [F] for
  these pages only. Nothing beyond them is attributed.
- Supplied by the author on Google Drive (folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`, file
  `201106-standards-basel-iii-global-regulatory-framework-more-resilient-banks-and-banking-systems-revised-version-june.pdf`),
  uploaded 2026-10-10. Cached at `~/.cache/wealth_garch/bcbs_basel3_2011.pdf`,
  sha256 `cbb1d8ea33595d817b18dc5a40c437335c750518d21bb3d063a6e77196eff96e`.
  Printed page = PDF page − 8 (printed p. 12 is PDF p. 20, printed p. 55 is
  PDF p. 63).

## Primitives

- Common Equity Tier 1 (CET1), Additional Tier 1 and Tier 2 capital, each net
  of regulatory adjustments (paragraph 50).
- Risk-weighted assets, the denominator of the three minimum ratios.

## Derivation chain

1. Paragraph 50. CET1, Tier 1 and Total Capital must be at least 4.5%, 6.0%
   and 8.0% of risk-weighted assets at all times.
2. Paragraphs 129 to 131. A conservation buffer of 2.5% of CET1 sits above
   the minimum. Inside it, a schedule restricts distributions, from 100% of
   earnings retained at a CET1 ratio of 4.5% to 5.125%, down to 0% above
   7.0%.
3. Paragraph 153. A minimum Tier 1 leverage ratio of 3%, capital over an
   exposure measure, tested in a parallel run from 2013 to 2017.

## Roles

- The minimum ratio is a constraint on the bank's exposure given its
  capital, which is how BCVW's solvency constraint uses it (BCVW-Q9).
- The buffer and its distribution schedule constrain payouts, which the
  model leaves to the bank's choice of barrier.

## What could not be reconstructed

- The calculation of risk-weighted assets, which the document defers to the
  Basel II framework. The identification of risk-weighted assets with
  $\pi X$ is therefore a modelling choice
  (`MeasurementMap.rwa_two_assets` and its control).
- The definition of the leverage exposure measure (paragraphs 154 to 164),
  not read.
