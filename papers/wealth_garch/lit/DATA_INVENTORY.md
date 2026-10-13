# Data inventory of the background papers

Made 13 October 2026 for the direction that we implement the background
papers ourselves and score them on one common dataset (TODO.md). Each row
of the summary rests on the quotations below, which `verify_inventory.py`
checks against the cached PDFs with `litcheck`'s extraction (PDF pages).
Availability is our assessment, not a quotation.

## Summary

| Paper | Model | Data | Unit, period | Public core | Licensed or confidential |
|---|---|---|---|---|---|
| Peura and Keppo (2006) | Structural, calibrated | Compustat (PK-Q11; we used Call Reports) | Banks, 1983-2002 | Replicated, stages 1-4 | none needed |
| Mankart, Michaelides and Pagratis (2016 draft) | Structural, MSM on 11 moments (DI-6) | Call Reports, FDIC failures (DI-5) | Banks, quarterly, 1990-2010 | All | none |
| Berger et al. (2008) | Partial adjustment, Blundell-Bond (DI-13, DI-14) | FR Y-9C (DI-10) | Top-tier BHCs, annual, 1992-2006 (DI-12) | Capital ratios, earnings, dividends, issuance residual | CRSP, Compustat ratings, confidential BOPEC ratings (DI-11) |
| Gordon and Hirtle (2026) | None, payout regressions | FR Y-9C (DI-15) | BHCs over $3bn, annual, 1990-2024 (DI-16) | All but two tables | Compustat stock compensation (DI-17) |
| Hirtle (2016) | None, Cox hazard (DI-20) | FR Y-9C (DI-18) | 66 BHCs, quarterly, 2007-2009 (DI-19) | Core | CRSP, I/B/E/S in robustness |
| Holod and Peek (2006 WP) | None, panel regressions | Call Reports (DI-23) | Banks, annual, 1987-2004 (DI-24) | Core | publicly traded flag, source not stated |
| Jokipii and Milne (2008) | Partial adjustment, Arellano-Bond (DI-22) | Bankscope (DI-21) | European banks, annual, 1997-2004 | none | Bankscope |
| Houston, James and Marcus (1997) | None, cash-flow regressions | Y-9 1981-89, CRSP, IDD (DI-25 to DI-27) | BHCs and subsidiaries | Call Reports 1986-89 | CRSP, IDD; Y-9 before 1986 |
| Baron (2020) | Stylised, assumed parameters (DI-4) | Compustat, CRSP (DI-1 to DI-3) | Listed banks, quarterly, 1980-2012 | none for the core | Compustat, CRSP, Capital IQ, Bankscope |
| Bayraktar et al. (2026) | Stochastic control, illustrative parameters (DI-7) | none; code on GitHub (DI-8) | none | n/a | none |

## Quotations

| ID | Source | Layout | PDF page | Quotation |
|---|---|---|---|---|
| DI-1 | baron_2020.pdf | yes | 14 | Commercial banks (which I also take to also include savings and trust institutions) are selected from Compustat by SIC codes 6020-6036 |
| DI-2 | baron_2020.pdf | yes | 14 | for U.S. publicly traded banks over the sample period 1980–2012 |
| DI-3 | baron_2020.pdf | yes | 15 | this decomposition is first done on the monthly level for each bank using monthly CRSP data |
| DI-4 | baron_2020.pdf | yes | 11 | Let bank risk be given by s = 0.4, the initial benefit of debt at t = 0 to be d = 0.02, the recapitalization cost c = 20 |
| DI-5 | mankart_michaelides_pagratis_2016.pdf | yes | 8 | We consider a sample of individual bank data from the Reports of Condition and Income (Call Reports) for the period 1990:Q1-2010:Q4. |
| DI-6 | mankart_michaelides_pagratis_2016.pdf | yes | 25 | We estimate the model separately for small and large banks by the Method of Simulated Moments using 11 moment conditions. |
| DI-7 | bcvw_2603.14557v2.pdf | yes | 13 | These values constitute an illustrative parameterization rather than an empirical calibration |
| DI-8 | bcvw_2603.14557v2.pdf | yes | 13 | github.com/yuqiongwang/bank_capital_structure |
| DI-10 | berger_2008.pdf | yes | 3 | Our data are for U.S. bank holding companies with total assets exceeding $150 million, and are obtained from the year-end FR-Y9C reports. |
| DI-11 | berger_2008.pdf | yes | 17 | we collect information on stock prices from the Center for Research on Stock Prices (CRSP) database, information on bond ratings from Standard and Poor's Compustat, and information on BHC examination ratings from the Federal Reserve. |
| DI-12 | berger_2008.pdf | yes | 17 | The resulting data set is an unbalanced panel containing 4,563 annual observations over 15 years |
| DI-13 | berger_2008.pdf | yes | 21 | we estimate (3) using the Blundell-Bond (1998) GMM method. |
| DI-14 | berger_2008.pdf | yes | 24 | On average, these variable adjustment speeds averaged between 28% and 41% |
| DI-15 | gordon_hirtle_2026.pdf | yes | 7 | Our primary data source is the FR Y-9C report, which contains consolidated balance sheet and income statement data for bank holding companies |
| DI-16 | gordon_hirtle_2026.pdf | yes | 11 | Our sample consists of all U.S.-owned, top-tier bank holding companies with assets greater than $3 billion in 2024 dollars from 1990 to 2024. |
| DI-17 | gordon_hirtle_2026.pdf | yes | 7 | Finally, we obtain data on stock-based compensation from Compustat. |
| DI-18 | hirtle_2016.pdf | yes | 9 | The data used in the estimation come primarily from the Federal Reserve Y‐9C regulatory reports filed by BHCs with assets greater than $500 million. |
| DI-19 | hirtle_2016.pdf | yes | 12 | resulting in a final estimation sample of 66 BHCs. |
| DI-20 | hirtle_2016.pdf | yes | 13 | These results are based on estimation of a Cox proportional hazard model: |
| DI-21 | jokipii_milne_2008.pdf | no | 4 | We build an unbalanced panel data set with eight years of annual bank balance sheet data obtained from the Bureau Van Dyck Bankscope database. |
| DI-22 | jokipii_milne_2008.pdf | no | 6 | we use a partial adjustment framework with quadratic costs of adjusting capital. |
| DI-23 | holod_peek_2006.pdf | yes | 14 | The individual bank-level data are taken from the Consolidated Report of Condition and Income database (Call Reports). |
| DI-24 | holod_peek_2006.pdf | yes | 14 | Our annual sample covers the period from 1987 to 2004. |
| DI-25 | houston_james_marcus_1997.pdf | yes | 7 | We collected bank holding company data from the Federal Reserve Y-9 tapes from 1981-1989, using annual observations. |
| DI-26 | houston_james_marcus_1997.pdf | yes | 7 | All stock data come from the Center for Research in Security Prices (CRSP) master tapes. |
| DI-27 | houston_james_marcus_1997.pdf | yes | 9 | from the Investment Dealer's Digest ODD). |
