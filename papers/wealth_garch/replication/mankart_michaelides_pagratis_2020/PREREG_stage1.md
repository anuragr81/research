# Pre-registration: Mankart, Michaelides and Pagratis replication, stage 1

Written and committed 14 October 2026, before the data below were fetched
and before any computation on the common bank dataset for this paper.
Nothing in this file may change after the first run. A change is a new
file with a new date, and both are reported.

## Aim

Reproduce, on Call Reports, the data side of Mankart, Michaelides and
Pagratis (NBER draft May 2016; `lit/mankart_michaelides_pagratis_2020`,
pages are PDF pages): their Table 1 (balance sheets by size, 2010Q4),
Table 2 (shock processes, rates, failure rates) and the data columns of
Table 5 (the 11 moments their estimation matches). Stage 2 solves the
model; stage 3 estimates it.

## What was seen before this file

Their pp. 8-28 and 51-56 read in full (data, model, estimation, data
appendix); the common dataset's coverage (results_common_data.md); the
field names of the FDIC failures endpoint (one record). No statistic of
theirs has been computed on our data.

## Data

- The pinned common bank dataset, quarters 1990Q1-2010Q4 (and 1989Q4 for
  lags), commercial banks (`BKCLASS` N, NM, SM).
- A supplement fetched after this commit from the same FDIC `financials`
  endpoint, 1989Q4-2010Q4, joined on `CERT` and `REPDTE`: `NETINCQ`,
  `ITAXQ`, `EXTRAQ`, `DRLNLSQ`, `ILNDOMQ`, `ILNFORQ`, `IFREPOQ`, `IGLSECQ`,
  `EDEPDOMQ`, `EDEPFORQ`, `EQCDIVQ`, `LNRE`, `LNCI`, `LNCON`, `LNAG`,
  `TRN`, `NTR`. Every field is defined in the pinned FDIC workbook.
- FDIC failures endpoint, all events with `FAILDATE` in 1990-2010 (both
  failures and assistance, `RESTYPE`), their data appendix p. 53.
- FRED `USRECQ` (NBER recession indicator, quarterly), 1989-2011.
- CPI: the pinned `cpiaucsl_1989_2024.csv.gz`, quarterly averages, for
  real rates and real deposit growth.
Pinned by sha256 in the results file. Nothing from 2025.

## Definitions (their pp. 6-12, 51-53)

- Size class each quarter by total assets `ASSET`: small below the 95th
  percentile, medium 95th-98th, large above the 98th.
- Recession quarters: `USRECQ` = 1, extended by the two quarters before
  each recession's start and the six after its end (their n. 14).
- Tangible assets `ASSET` - `INTAN`; tangible equity `ASSET` - `LIAB` -
  `INTAN`; only bank-quarters with positive tangible equity (p. 52).
- Outliers: bank-quarters below the 1st or above the 99th percentile of
  that quarter's growth in loans (`LNLSNET`) or in tangible assets are
  dropped (p. 51).
- Loan write-off w: `DRLNLSQ` / lagged (`LNLSGR` + `DRLNLSQ`) (p. 51).
- Real deposit growth: log difference of `DEP` less the log change in the
  CPI.
- Deposit rate: (`EDEPDOMQ` + `EDEPFORQ`) / lagged `DEP`; loan return
  (`ILNDOMQ` + `ILNFORQ`) / lagged `LNLSNET`; liquid-asset return
  (`IFREPOQ` + `IGLSECQ`) / lagged (`CHBAL` + `FREPO` + `SC`), all less
  the quarter's CPI inflation; spreads are returns less the deposit rate.
- Ratios: loans `LNLSNET`, deposits `DEP`, equity tangible equity, all
  over tangible assets; profit (`NETINCQ` + `ITAXQ` - `EXTRAQ`, pre-tax
  and before discontinued operations) over tangible equity; dividends
  `EQCDIVQ` over tangible equity.
- Failure rate: events in the quarter over banks in the class at the
  start of the quarter, a bank's class read from its last report before
  the event; events without a matching report are counted and reported.
- Winsorising at the 1st and 99th percentiles per quarter by size, of the
  ratios (p. 52) and of w and deposit growth (p. 52).
- Shock processes: per bank, the mean, standard deviation and AR(1) of w
  and of deposit growth (deposit growth with quarter dummies), overall and
  within each state, for banks with at least 35 quarters in expansions
  and 35 in recessions; averaged across banks after winsorising the bank
  estimates at the 1st and 99th percentiles (p. 52).
- Table 5 moments: per bank, mean and standard deviation of each ratio
  over its quarters, for banks with at least 20 quarters in each state;
  averaged across banks. A bank's quarters are assigned to the size class
  of that quarter, and its moments are computed within each class.

## Targets and rules (theirs on pp. 8, 12, 24, 51)

- S. Final sample within 10% of 799,437 bank-quarters and 16,564 banks;
  failure events within 10% of 1,292.
- T1. Table 1, small and large, 2010Q4: bank counts within 10%; the
  shares of cash, securities, loans, deposits, transaction deposits,
  wholesale funding (Fed funds and repos borrowed `FREPP` plus other
  liabilities `LIAB` - `DEP` - `FREPP`) and tangible equity within 2
  percentage points.
- T2. Table 2, small and large, each of 30 statistics: within 20% of
  theirs, or within 0.05 percentage points when theirs is below 0.25 in
  absolute value.
- T5. Table 5 data columns, small and large, each of 11 moments: within
  10% of theirs, or within 0.005 when theirs is below 0.05.

Each statistic holds or differs; the count is reported per table.

## Known limits

- Deposit growth's quarter dummies and the CPI deflator are our readings
  of "deseasonalized" and "real"; the paper does not name a deflator.
- `TRN` and `NTR` are domestic-office deposits; `DEP` includes foreign
  offices.
- The FDIC's current processing of past reports may differ from theirs.
