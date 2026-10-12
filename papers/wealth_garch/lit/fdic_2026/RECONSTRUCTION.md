# Reconstruction: FDIC (2026)

## Source read

- "All Financial Reports.xlsx", the variable workbook of the FDIC BankFind
  API, linked from https://api.fdic.gov/banks/docs. Sheet
  "Reference-Variables&Definitions" (2,333 variables). Evidence tag [F]
  for the variables quoted in `CLAIMS.md`. Nothing beyond them is
  attributed.
- Supplied by the author on Google Drive (folder
  `1QTH_CjoS9athYKFq_dbhnA1q_ePVLjEM`), uploaded 2026-10-11, and identical
  to the copy fetched from the API the same day. Cached at
  `~/.cache/wealth_garch/fdic_all_financial_reports.xlsx`, sha256
  `6413c93064c16c63d4850221b25181f03ea51d086cd8f68d365cb070f383ab65`.
- Quotations are located by variable, not by page.

## Primitives

- Balances: `ASSET`, `LIAB`, `RWAJT`, `RBCT1J`. Ratio: `RBCT1CER`.
- Flows, year to date unless marked quarterly: `NETINC`, `EQCDIV`,
  `EQCSTKRX`, `EQCBHCTR`, `EQCMRG`. Quarterly: `EQCSXQ`.

## Roles

- The panel fields of `06_empirical_starter/` map to rows G1 to G4, G14
  and G15 of `MEASUREMENT_MAP.tex`.

## What could not be reconstructed

- The mapping from each variable to a Call Report item number. The
  workbook does not give it.
