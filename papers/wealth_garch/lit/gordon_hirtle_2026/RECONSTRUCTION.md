# Reconstruction: Gordon and Hirtle (2026)

## Source read

- Lily Gordon and Beverly Hirtle, "How Do Banks Build Equity Capital?",
  Federal Reserve Bank of New York *Economic Policy Review* 32(1),
  September 2026, doi 10.59576/epr.32.1.1-48. Published version, 49 PDF
  pages with no printed page numbers (GH-D1). Read in full. Evidence tag
  [F]. The charts are images and were not read; nothing is attributed to
  them beyond what the text states.
- Supplied by the author on Google Drive (folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`, file `gordon_hirtle_epr_2026.pdf`,
  id `1XQ8ecxcU33UNP2P6WZxknKuHAaBTY8e8`), uploaded 2026-10-12. Cached at
  `~/.cache/wealth_garch/gordon_hirtle_2026.pdf`, sha256
  `f1fe4e9c87a9ee8b00248446555a41ed7c339d2c3ff9404382fd408c5510e64e`.

## Primitives

- Unit: U.S.-owned top-tier bank holding companies with assets above
  $3 billion in 2024 dollars, 1990-2024, FR Y-9C (p. 11). Stand-alone banks
  and smaller holding companies are excluded (p. 3, n. 1).
- Equation 1 (p. 7): equity capital this year equals last year's plus net
  income, OCI, common and preferred issuance, less common and preferred
  dividends and repurchases, plus other changes.
- Issuance and repurchases are imputed from the equity schedule (pp. 9-10,
  nn. 6-7).
- EMP: earnings (net income plus OCI) minus common dividends and
  repurchases (Chart 10 notes, p. 22).
- Tier 1 buffer: the Tier 1 risk-based ratio less the applicable
  requirement, with Basel I and III phase-ins (n. 13, p. 27).

## Derivation chain

1. Aggregates (Charts 3 to 6): earnings are 11.6% of prior equity in a
   typical year, common issuance 2.5%, dividends 4.7%, repurchases 4.2%
   (p. 16).
2. Bank-level facts (Charts 7 to 14): dividends are paid in more than 85%
   of bank-years; payouts exceed earnings in about one bank-year in five;
   issuance is frequent and small (pp. 4, 19-24).
3. Negative EMP regressions (Tables 2 to 6): persistent, more likely with
   higher equity (capital management) and after negative earnings shocks,
   OCI shocks above net income shocks; for large banks, lagged dividends
   matter (pp. 26-33).
4. Issuance and repurchases (Exhibit 2, Tables 7-8): small issuance and
   small repurchases rise with stock-based compensation; large ones do not
   (pp. 35-40).

## Roles in this bundle

- The holding company, which pays outside shareholders, is the unit the
  payout models describe. At that unit dividends are paid in more than 85%
  of bank-years and payouts exceed earnings in about a fifth: payouts are
  continuous, not confined to an upper barrier.
- The mean Tier 1 buffer of 6.74 points (Table 1) sits far above the
  requirement, as E1 finds at bank level.
- Most measured issuance is compensation-related, not capital raising
  (GH-Q7), which bears on any issuance measure built from stock sales.

## What could not be reconstructed

- The charts, Exhibits 1 and 2, and the regressions.
