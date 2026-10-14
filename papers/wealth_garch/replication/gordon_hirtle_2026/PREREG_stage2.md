# Pre-registration: Gordon and Hirtle replication, stage 2

Written and committed 14 October 2026, before any computation on the data
below. Nothing in this file may change after the first run. A change is a
new file with a new date, and both are reported.

## Aim

Test whether Gordon and Hirtle's findings on bank holding companies hold
for banks, on the common bank dataset (`common_data/`, the testing
thread's data). Stage 1 (results_stage1.md, results_stage1_3.md)
reproduced the findings on their own data.

## What was seen before this file

The common dataset's coverage (results_common_data.md); stage 1 and its
sample investigation; earlier results on the FDIC data: the ownership
split of E2 and E3 (06_empirical_starter/results_ownership_split_2.md) and
Peura and Keppo stages 2 to 4. No payout-above-earnings measure has been
computed for banks.

## Data

The pinned common dataset `common_data/fdic_q_1990.csv.gz` to
`fdic_q_2024.csv.gz` (sha256 in results_common_data.md), December reports
only, so year-to-date items are full-year values; the pinned CPI
`cpiaucsl_1989_2024.csv.gz` of stage 1. Nothing new is fetched.

## Samples

- P (primary): commercial banks (`BKCLASS` N, NM or SM) with total assets
  above $3 billion in 2024 dollars (CPI, as stage 1), 1990-2024. The size
  threshold is theirs, so the population is of comparable size.
- A: all commercial banks, no size threshold.
- Within P and A, reported by ownership in that December: no holding
  company (`RSSDHCR` zero or empty); the only bank of its holding company
  in that December's file; a subsidiary of a holding company with two or
  more banks in that file. Inside a holding company, a bank's dividends
  and stock transactions are largely transfers with its parent (lit
  records of Holod and Peek, and of Houston, James and Marcus).

## Variables, bank analogues of stage 1

- Earnings: `NETINC`. Banks' reports carry no OCI in our fields, so
  earnings are net income, the measure of their Table 3 column (3).
- Common dividends: `EQCDIVC`.
- Net stock transactions: `EQCSTKRX` (sales, conversions, retirements and
  treasury stock, net, per the FDIC definition). Repurchases are
  max(-`EQCSTKRX`, 0) and issuance max(`EQCSTKRX`, 0): net, not gross as
  theirs, a known difference.
- Prior year-end equity: `EQCPREV` + `EQCREST` + `EQCMRG`.
- Negative EMP: earnings minus common dividends minus repurchases below
  zero.
- Equity `EQ`, assets `ASSET`; Tier 1 buffer 100 `RBCT1J`/`RWAJT` minus
  the requirement schedule of stage 1, missing when `RWAJT` <= 0 (banks
  under the community bank leverage ratio, results_common_data.md).
- A bank-year needs `ASSET`, `NETINC` and `EQCPREV` present.
- Winsorising as stage 1, at the 1st and 99th percentiles within each
  sample, of ln real assets, equity/assets, the Tier 1 buffer, net income
  over assets and its change.

## Hypotheses and rules, primary sample P

H1. Dividends are positive in more than 85% of bank-years (GH-Q1). Holds
    if the share exceeds 85%.
H2. Payouts exceed earnings in more than 20% of bank-years (GH-Q2). Holds
    if the negative-EMP share exceeds 20%.
H3. Negative EMP is persistent: in their Table 2 specification, columns
    (3) and (4), the coefficient on its first lag is positive at 5%.
    Holds if positive and significant in both.
H4. Capital-management channel (GH-Q10): in the same columns the
    coefficient on equity/assets at t-1 is positive at 5%. Holds if
    positive and significant in both.
H5. Earnings-shock channel (GH-Q17): in their Table 3 column (3)
    specification (change in net income over assets, with ln assets,
    equity/assets and the Tier 1 buffer at t-1, year and bank effects,
    two-way clustering), the coefficient on the change is negative at 5%.
    Holds if so.

Specifications, lags (the bank in the sample at t-1 and t-2), the linear
probability model, the dropping of singleton banks with bank effects, and
the two-way clustering are as stage 1. Their coefficients are printed
beside ours; magnitudes are reported, not tested, since the populations
differ.

## Reported, not used for a verdict

- H1 to H5 on sample A and on each ownership group of P and A.
- The typical-year components (their Chart 4) and the median issue among
  issuers (GH-Q5), with net issuance.
- The number of banks and bank-years in each sample and group.

## Known limits

- Net, not gross, stock transactions; no OCI.
- Subsidiary banks' payouts go mostly to their parent, so for them EMP
  measures upstreaming, not payout to outside shareholders.
- `EQCDIVC` and `EQCSTKRX` are filed by banks under $100 million only in
  December, which the annual design uses.
