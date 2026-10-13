# Pre-registration: Gordon and Hirtle replication, stage 1

Written and committed 13 October 2026, before the data below were fetched.
Nothing in this file may change after the first run. A change is a new file
with a new date, and both are reported.

## Aim

Reproduce, on their own data source, the stylised facts and the first
regression table of Gordon and Hirtle (2026), FRBNY Economic Policy Review
32(1) (`lit/gordon_hirtle_2026`, pages are PDF pages). Stage 1 is their
analysis on their data; later stages take their findings to the common
bank dataset (`common_data/`).

## What was seen before this file

Their data section (pp. 6-12) and results through Table 2 (pp. 13-29), read
in full on 13 October 2026; the column header of one Y-9C file
(`bhcf1012.csv`, no data rows read); and the Federal Reserve's MDRM data
dictionary, pinned here: `MDRM.zip` sha256
`32cdcda3c22d5535b39601e0dbf48db617ce45b3376eb9e0ee22598fdf8203ea`,
downloaded 13 October 2026. No Y-9C values have been read.

## Data

- FR Y-9C, December reports 1990-2024. 1990-2020 from the Federal Reserve
  Bank of Chicago (`bhcfYY12.csv`, scripted). 2021-2024 from the FFIEC
  National Information Center, which refuses scripted access; the author
  downloads those four December files by browser. Every file is pinned by
  sha256 in the results file. Nothing from 2025.
- Price index for "2024 dollars": not stated by the authors. Ours is the
  CPI for all urban consumers (FRED `CPIAUCSL`), annual average of the
  twelve months, fetched after this commit and pinned.

## Sample (their p. 11 and n. 2)

A holding company enters year t if its December report of year t is in the
file (their full-year rule, p. 10), and:

1. Top-tier: `RSSD9348` (regulatory high holder, MDRM) is zero, empty or
   equal to its own `RSSD9001`.
2. U.S.-owned: `RSSD9101` (domestic indicator, MDRM) is "Y". With rule 1
   this drops U.S. holding companies of foreign banks, whose regulatory
   high holder is the foreign parent.
3. Assets: `BHCK2170` times CPI(2024)/CPI(t) above $3 billion (Y-9C values
   are in thousands, so above 3,000,000).
4. Equity-change schedule reported: `BHCK4340` present.

Reported, not used: the counts under the alternative top-tier rule on the
financial high holder `RSSD9364`.

## Variables (their pp. 7-10, Exhibit 1 items as defined in MDRM)

- Earnings: net income `BHCK4340` plus OCI `BHCKB511`. OCI is on the
  report from 2001 (MDRM). Their pre-2001 OCI proxy is described but not
  itemised (n. 4); ours is none, so earnings 1990-2000 are net income only.
  A known difference.
- Common issuance: `BHCK3579` + `BHCK4782` + max(`BHCK3580`, 0).
- Common repurchases: `BHCK4783` + max(-`BHCK3580`, 0).
- Common dividends: `BHCK4460`. Preferred: issuance `BHCK3577` +
  max(`BHCK3578`, 0), dividends `BHCK4598`, repurchases max(-`BHCK3578`, 0).
- Prior year-end equity, adjusted as on p. 10: `BHCKB508` (restated) from
  2001, `BHCK3217` before 2001, plus business combinations `BHCK4356`.
- Equity: `BHCK3210`. Assets: `BHCK2170`.
- Negative EMP: earnings minus common dividends minus common repurchases
  below zero (p. 22).
- Tier 1 buffer: Tier 1 capital over risk-weighted assets minus the
  requirement. 1996-2014 `BHCK8274`/`BHCKA223`; 2015-2024
  `BHCA8274`/`BHCAA223`. Before 1996 the authors reconstruct both from
  components they do not list (n. 13); ours is missing, a known
  difference. Requirement: 4% through 2014; 6% from 2015 (B3-Q2); plus
  the conservation buffer (B3-Q4) at 0.625, 1.25, 1.875 and 2.5 points in
  2016, 2017, 2018 and from 2019. The phase-in dates are our choice, not
  from a pinned source. Bank-specific surcharges (G-SIB, stress capital
  buffer) are not included, a known difference.
- Winsorising (Table 1 notes): assets, equity/assets, Tier 1 buffer,
  earnings, net income, OCI and their changes at the 1st and 99th
  percentiles of the pooled sample.

## Targets and reproduction rule

R1. Chart 4 medians over 1990-2024 of the annual aggregate component as a
share of aggregate adjusted prior year-end equity (pp. 14-16): earnings
11.6%, common issuance 2.5%, common dividends 4.7%, common repurchases
4.2% (GH-Q6 for earnings).
R2. Dividends positive in more than 85% of bank-years (GH-Q1).
R3. Median common issuance among banks that issue, 0.9% of equity
(GH-Q5); measured pooled over issuing bank-years, as a share of adjusted
prior year-end equity.
R4. Table 1 (p. 27): N 6,448; equity/assets mean 9.10; negative EMP mean
0.21; dividends/assets 0.37, repurchases/assets 0.22, issuance/assets
0.20 (percent of assets); Tier 1 buffer mean 6.74, N 6,394.
R5. Table 2 (p. 28), linear probability model of negative EMP in t on its
lags at t-1 and t-2, ln(assets), equity/assets and Tier 1 buffer at t-1;
columns (1) to (4) as their notes (no fixed effects and clustering by
year; then by year and bank; year effects; year and bank effects). Their
coefficient on the first lag: 0.201, 0.201, 0.208, 0.097; N 5,148 (5,091
in column 4). Two-way clustering by the Cameron-Gelbach-Miller formula.

Rule. A share, mean or median reproduces if ours is within 10% of theirs
(relative), or within 0.5 percentage points for values below 5%. A count
reproduces within 10%. A coefficient reproduces if it has their sign and
significance at 5%, and lies within two of their standard errors of their
value. Every target is reported with theirs beside it; there is no overall
verdict.

## Known limits

- Pre-1996 Tier 1 buffer and pre-2001 OCI are missing (above), so R4's
  Tier 1 buffer and R5 run on fewer bank-years than theirs.
- The FFIEC files of 2021-2024 come from a manual download.
- The API-era Y-9C files are the Fed's current processing, which may
  differ from what the authors used.
