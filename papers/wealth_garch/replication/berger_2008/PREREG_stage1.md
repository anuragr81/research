# Pre-registration: Berger et al. replication, stage 1

Written and committed 14 October 2026, before the data below were fetched
or read. Nothing in this file may change after the first run. A change is a
new file with a new date, and both are reported.

## Aim

Reproduce, on public data, the partial-adjustment estimates of Berger,
DeYoung, Flannery, Lee and Oztekin (2008) (`lit/berger_2008`; pages are PDF
pages): their capital means, the Step 1 constant adjustment speed, the
Step 2 adjustment speeds, and the Step 3 targets.

## What was seen before this file

Their text pp. 14-25 and Tables 1-3 (pp. 30-35), read in full; the MDRM
dictionary (pinned in `replication/gordon_hirtle_2026/`); the NY Fed
CRSP-FRB link documentation; the header and transformation-code counts of
the NIC transformations file (pinned here, received from the author); the
NIC data dictionary (pinned here). Y-9C December files 1990-2020 were used
in the Gordon and Hirtle replication. No Berger variable has been
computed.

## Scope, agreed with the author

- Years: dependent variable 1997-2006. The Y-9C reports Tier 1 capital and
  risk-weighted assets from March 1996 (MDRM `BHCK8274`, `BHCKA223`), and
  do-nothing capital needs the previous year's capital. Their sample is
  1992-2006.
- Dropped, unavailable: MKTBOOK (CRSP prices, licensed), bond ratings BBB,
  JUNK, MISSING (Compustat, licensed), BADBOPEC (confidential supervisory
  ratings).

## Data, fetched after this commit

- Y-9C quarterly files from the Federal Reserve Bank of Chicago, March,
  June and September of 1994-2006 (scripted); December files 1994-2006 are
  those pinned in `replication/gordon_hirtle_2026/`.
- NY Fed CRSP-FRB link `crsp_20240930v2.csv` (scripted).
- Already pinned: CPI, NIC relationships (`replication/gordon_hirtle_2026/`),
  NIC transformations and data dictionary (here).
All files pinned by sha256 in the results file. Nothing from 2025 is used.

## Sample

A holding company enters year t if, in the December Y-9C of year t, it is
top-tier (`RSSD9348` zero, empty or its own ID), in the United States
(`RSSD9101` "Y"), a bank holding company (`RSSD9346` BHC or FHD), and
publicly traded: a CRSP-FRB link with its RSSD ID covers 31 December of t
(`dt_start` <= that date <= `dt_end`). It needs every variable below.
Observations with any of the three capital ratios above the 99th
percentile of the sample are deleted (their p. 17).

## Variables (MDRM items; their Table 2 definitions where text and table differ)

- Capital: Tier 1 `BHCK8274`; total `BHCK3792`; risk-weighted assets
  `BHCKA223`; assets `BHCK2170`. CAPLEV = Tier 1/assets, CAPTIER1 = Tier
  1/RWA, CAPTOTAL = total/RWA.
- Do-nothing capital: DNK_t = (K_{t-1} + NI_t - DIV_{t-1})/A_t, with NI
  `BHCK4340` and DIV `BHCK4460` + `BHCK4598`, K and A as in each ratio.
  Their "small adjustments" (n. 22) are not specified and not made.
- X at t-1:
  - STDROA: standard deviation of quarterly `BHCK4300` (income before
    discontinued operations; year-to-date items differenced within the
    year) over end-of-quarter `BHCK2170`, the twelve quarters ending in
    December of t-1, all twelve required.
  - NMERGER: acquisitions in calendar year t. A transformation counts if
    its code is 1 or 9 (NIC data dictionary p. 44: charter discontinued or
    retained, merger or purchase and assumption, no failure), its date is
    in year t, its successor is the holding company or in its
    organisation on that date, and its predecessor was not in the
    organisation the day before. Organisation: entities reached through
    NIC relationships with `RELN_LVL` 1 and `CTRL_IND` 1 in force on the
    date.
  - LNASSETS = ln `BHCK2170`; D50B = assets above $50 billion in 2001
    dollars (CPI); D50B x LNASSETS.
  - RETAILDEP = (`BHCB3187` + `BHOD3187` + `BHCB6648` + `BHOD6648`) /
    `BHCK2948`: transaction accounts other than demand deposits plus time
    deposits under $100,000, over total liabilities. Brokered deposits are
    not subtracted (the Y-9C reports them only to 2000).
  - BUSINESSLOAN = (`BHCK1766` + `BHDM1480` + `BHDM1415`) / `BHCK2122`.
  - OFFBSHEET = 1 if the gross notional of derivatives (`BHCKA126`,
    `BHCKA127`, `BHCK8723` to `BHCK8732`, those reported) exceeds ten times
    `BHCK2170`.
  - BUSINESSLOAN and OFFBSHEET enter the CAPLEV model only (their p. 17).
- Z at t-1, per measure: WELLCAP_OVER800 (omitted), 600TO800, 400TO600,
  200TO400, 0TO200, ADEQCAP, UNDERCAP, SIG-UNDERCAP, against the thresholds
  of their Table 2 (leverage 5% and 4%, Tier 1 6% and 4%, total 10% and 8%).

## Estimation (gmm.py, validated in results_validation.md)

- Step 1, each measure: k_t = a DNK_t + b'X_{t-1} + year effects + constant
  + firm effect + error, two-step system GMM; DNK_t GMM-style with lags 2
  to 3; X_{t-1} and the year dummies as standard instruments. lambda = 1 - a;
  target = (constant + b'X_{t-1})/lambda.
- Step 2: (k_t - DNK_t) on DEV and DEV times each Z dummy, with year
  dummies, by OLS, standard errors clustered by holding company; DEV =
  Step 1 target - DNK_t; lambda_it = the fitted speed.
- Step 3: k_t - DNK_t(1 - lambda_it) on lambda_it, lambda_it X_{t-1}, year
  dummies and a constant, two-step system GMM with the instruments of
  Step 1. Target = coefficient on lambda_it + coefficients on lambda_it X
  times X_{t-1}.

## Targets and rules, per measure (theirs in brackets)

T1. Mean capital ratio within 0.010 [.0846, .1208, .1381].
T2. Step 1 lambda within 0.10 [.6024, .4864, .5632].
T3. Step 2 mean lambda_it within 0.10 [.4113, .2785, .3604].
T4. Step 2 coefficients on DEV x WELLCAP_400TO600, 200TO400 and 0TO200
    positive at 5% [all positive at 1%].
T5. Step 3 mean target within 0.010 [.0864, .1440, .1694]; for CAPTIER1 and
    CAPTOTAL, mean target above mean capital ratio [their p. 23].
T6. Step 3 signs: LNASSETS negative in all three [theirs negative];
    STDROA positive for CAPTIER1 and CAPTOTAL [theirs positive].

Each target holds or differs; there is no overall verdict.

## Reported, not used for a verdict

Bank-years and holding companies by year (theirs about 304 a year); means
of STDROA [.0034], NMERGER [.066], RETAILDEP [.605], BUSINESSLOAN [.281],
OFFBSHEET [.009], D50B [.054]; capital-position shares (their Table 2);
Hansen and Arellano-Bond tests; all Step 2 coefficients.

## Known limits

- Ten years against fifteen; four regressors fewer; instruments and
  first-step weighting are our choices (they state Blundell-Bond only).
- RETAILDEP uses transaction accounts other than demand deposits, as the
  Y-9C reports them, for their "non-business transactions deposits".
