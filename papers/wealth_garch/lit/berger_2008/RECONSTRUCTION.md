# Reconstruction: Berger, DeYoung, Flannery, Lee and Öztekin (2008)

## Source read

- Allen N. Berger, Robert DeYoung, Mark J. Flannery, David Lee and Özde
  Öztekin, "How Do Large Banking Organizations Manage Their Capital
  Ratios?", Federal Reserve Bank of Kansas City Research Working Paper
  08-01. 39 PDF pages: a cover, an abstract page, then pages 1 to 37
  (printed page = PDF page - 2). Read in full. Evidence tag [F] for this
  version. The published article, *Journal of Financial Services Research*
  34(2-3):123-149, 2008, was not read.
- Supplied by the author on Google Drive (folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`, file `pdf-rwp08-01.pdf`, id
  `1zNek_pxvvFBbrhBpbP7x2hlXPGB_8wgw`), uploaded 2026-10-12. Cached at
  `~/.cache/wealth_garch/berger_2008.pdf`, sha256
  `741123eec7a14635d41678537bff7843305d506763c28233694ba4a1b251114a`.

## Primitives

- Unit: publicly traded top-tier U.S. bank holding companies, FR Y-9C,
  1992-2006: 4,563 observations on 666 holding companies (p. 3).
- Capital ratios: Tier 1 leverage (CAPLEV), Tier 1 risk-based (CAPTIER1)
  and total risk-based (CAPTOTAL) (p. 15).
- Do-nothing ratio DNK: last year's capital plus this year's net income
  less last year's dividends, over this year's assets (pp. 12, 15-16).
- Target $k^*=\beta X$ (1); partial adjustment
  $k-\text{DNK}=\lambda(k^*-\text{DNK})$ (2); speed
  $\lambda_{i,t}=\Lambda Z_{i,t-1}$ (4), with $Z$ the position relative to
  the regulatory thresholds, the supervisory rating and the bond rating.

## Derivation chain

1. Capital ratios exceed minima throughout 1992-2006 (Figures 1-4, pp.
   5-6).
2. Hypotheses: earnings retention, economic capital, acquisition plans
   (pp. 6-8).
3. Decomposition of equity changes (Table 1): retained earnings and
   merger-related issuance exceed the change in capital; other issuance is
   negative in twelve of fifteen years (pp. 9-11).
4. Three-step estimation (pp. 19-20): Blundell-Bond GMM with a constant
   speed; a regression of the speed on $Z$; GMM again for the targets with
   the fitted speeds.
5. Results (Table 3): speeds 28%-41% a year on average; faster below the
   well-capitalized and adequate thresholds, slower under bad supervisory
   ratings; targets above actual ratios, rising with risk and falling with
   size (pp. 21-24).

## Roles in this bundle

- The standard empirical description of bank capital dynamics: partial
  adjustment toward a firm-specific target, with speed depending on the
  distance to the regulatory thresholds. This is the comparison class for
  the persistence test (E4, M12).
- The authors place Peura and Keppo as the dynamic model (BDF-Q3) and
  conjecture asymmetric management: gradual above, lumpy near the minimum
  (BDF-Q15), the shape our model gives.

## What could not be reconstructed

- The GMM estimates and the figures.
- The published version's changes.
