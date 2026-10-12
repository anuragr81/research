# Pre-registration: Peura and Keppo (2006) replication, stage 2

Written and committed 12 October 2026, before any data below were fetched.
Nothing in this file may change after the first run. A change is a new file
with a new date, and both are reported.

## Aim

Reproduce the data side of Peura and Keppo, section V (pp. 2181-2190),
on their period, with FDIC Call Reports in place of Compustat: Table 1,
Figure 5 and Table 3, and the statements of pp. 2182-2190 built on them.
Their method is followed step by step; where the data force a choice, the
choice is fixed here. Every difference from their data is recorded.

## Their method (lit/peura_keppo_2006, pp. 2181-2189)

- Sample: U.S. national commercial banks on Compustat with at least 15
  years of annual data, 1983-2002; one bank with negative μ dropped; 61
  banks.
- ROA_t = NI_t / R_{t-1} - r_t X_{t-1} (28), with R risk-weighted assets, X
  the capital ratio and r the federal funds rate. μ is the bank's time-series
  mean of ROA, σ its standard deviation (n. 14).
- Before 1993, R is the bank's total assets times its average post-1993
  ratio of risk-weighted to total assets (n. 14).
- Capital buffer: the bank's average over 1993-2002 of the capital ratio
  less 8% (Table 1 note).
- Groups: above- and below-average loan loss provisions over the sample
  (29 and 32 banks).
- Model barriers per bank with Δ = 0.5 years, K = 0.25%, ρ = 4%: u1, u2 and
  u0 (pp. 2184-2185, Table 3).

## Data

- FDIC BankFind API, endpoint `financials`, December reports 1984 to 2002,
  every institution, fields `CERT`, `REPDTE`, `BKCLASS`, `ASSET`, `NETINC`,
  `EQ`, `ELNATR`, `RWAJT`, `RBCT1J`, `RBCT2`, `RBCRWAJ`. December values of
  year-to-date items are annual. Amounts in thousands of dollars.
  Definitions: lit record `fdic_2026`.
- Federal funds rate: FRED series `FEDFUNDS` (monthly, percent), fetched
  from https://fred.stlouisfed.org/graph/fredgraph.csv?id=FEDFUNDS. r_t is
  the mean of the twelve monthly values of year t.
- Both files are saved and pinned by sha256 in the results file.

## Choices forced by the data

1. Period: Call Reports start in 1984, so the data run 1984-2002 and ROA
   runs 1985-2002 (they had 1983-2002).
2. Banks: national banks, `BKCLASS` = N in every year the bank reports.
   Call Reports cover banks, not the holding companies on Compustat.
3. Data length: at least 15 annual ROA values in 1985-2002.
4. Risk-weighted assets: as their n. 14, R_t = RWAJT from 1993; before 1993,
   R_t = ASSET_t times the bank's mean of RWAJT/ASSET over 1993-2002. A bank
   with no RWAJT in 1993-2002 is dropped.
5. Capital ratio X_t: total risk-based capital (RBCT1J + RBCT2) over R_t from
   1993; before 1993, equity EQ over R_t (total risk-based capital is not
   reported before 1990).
6. Capital buffer: the mean over 1993-2002 of RBCRWAJ/100 - 0.08, over the
   years reported; at least 5 such years.
7. Loan loss provisions: the bank's mean of ELNATR/ASSET over 1985-2002;
   above-average means above the sample mean.
8. Banks with μ ≤ 0 are dropped, as theirs.
9. Two samples, both reported:
   - A: every national bank passing 2-8;
   - B: the 61 banks of A with the largest mean total assets over
     1985-2002, matching their sample size and the size of listed banks.
10. Barriers: u0 from (17) as in stage 1; u1 and u2 from (14)-(18) with the
    stage 1 functions, Δ = 0.5, K = 0.0025, ρ = 0.04, starting points scaled
    to u0 (u1 in {0.05, 0.15, 0.3, 0.45, 0.6} × u0, u2 in {0.5, 0.7, 0.85,
    0.95, 0.99} × u0) and stage 1's admissibility test. A bank without a
    unique admissible solution is counted and left out of the statistics
    that use u1 or u2.

## Findings to reproduce

Theirs, with the reproduction rule fixed now (5% two-sided tests on
Pearson correlations; OLS with conventional standard errors):

- F1 (Table 1): the μ-σ correlation is negative and significant (theirs
  -0.37, p 0.003).
- F2 (Table 1): the above-average-provision group has a lower mean μ and a
  higher mean σ than the other group.
- F3 (Table 1): in the above-average-provision group, the buffer-σ
  correlation is positive and significant (theirs 0.58).
- F4 (Fig. 6, Table 3): the u2-σ correlation is at least 0.9 (theirs 0.99).
- F5 (Fig. 5): in the above-average-provision group, the buffer-u2
  correlation is positive and significant (theirs 0.57), with OLS slope
  significantly below one and intercept significantly above zero.
- F6 (pp. 2188): the mean model barrier is below the mean buffer, for u2
  (theirs 2.4% against 5.0%) and for u0 (2.7%).
- F7 (p. 2188): no bank has a mean buffer below 2%.

Each is "reproduced" when its sign and significance (or its inequality)
hold in our data, and "not reproduced" otherwise, separately for samples A
and B. Every number of their Table 1 and Table 3 is printed beside ours.

## Known limits

- Banks, not holding companies; Call Report, not Compustat, definitions of
  net income and capital.
- 1984 is missing, so one ROA year fewer.
- Sample B's size rule approximates the listing filter of Compustat.
