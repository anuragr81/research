# Pre-registration: the common bank dataset

Written and committed 13 October 2026, before the data below were fetched.
Nothing in this file may change after the fetch. A change is a new file
with a new date, and both are reported.

## Aim

One quarterly dataset of US banks on which the testing thread scores every
implemented paper (TODO.md, 13 October 2026). The unit is the bank, as the
author decided. This file fixes the source, fields, period and storage, and
states what we expect the coverage to be. It fixes no analysis: each
replication or test that uses the data has its own pre-registration.

## Source

FDIC BankFind API, endpoint `financials` (https://api.fdic.gov/banks/),
no registration. Every field below is defined in the pinned FDIC
definitions workbook (`lit/fdic_2026`, sheet
"Reference-Variables&Definitions"); the fetch script refuses a field that
is not defined there.

## Period and hold-out

Report dates 1984Q1 to 2024Q4, every quarter end. Nothing from 2025 or
later is requested, and the fetch script stops if any returned row has a
report date after 31 December 2024.

## Fields

- Identity and structure: `CERT`, `REPDTE`, `BKCLASS`, `RSSDHCR`.
- Balance sheet: `ASSET`, `LIAB`, `EQ`, `EQTOT`, `DEP`, `LNLSGR`,
  `LNLSNET`, `SC`, `CHBAL`, `FREPO`, `FREPP`, `INTAN`.
- Income and expense (year-to-date): `NETINC`, `ELNATR`, `INTINC`,
  `EINTEXP`, `EDEPDOM`, `EDEPFOR`, `ILNDOM`, `NTLNLS`, `DRLNLS`.
- Regulatory capital: `RBCT1J`, `RBCT2`, `RWAJT`, `RBCRWAJ`, `RBC1AAJ`.
- Changes in equity capital: `EQCDIV`, `EQCDIVC`, `EQCSXQ` (quarterly),
  `EQCSTKRX`, `EQCTRSTX`, `EQCBHCTR`, `EQCMRG`, `EQCPREV`, `EQCREST`.

Values are stored exactly as returned, with no imputation, rescaling or
filtering. A field the API returns empty is stored empty.

## Common sample

The stored file holds every institution the endpoint returns. The common
sample used by the testing thread is commercial banks, `BKCLASS` in
{N, NM, SM} in that quarter. Savings institutions and other classes are
kept in the file and are outside the common sample. Further filters
(size, mergers, ownership) belong to each test's pre-registration.

## Storage and pinning

One gzip CSV per year, `fdic_q_<year>.csv.gz`, columns in the order
above, rows sorted by `REPDTE` then `CERT`. Each file is pinned by sha256
in `results_common_data.md` and committed if under 50 MB; a larger file is
kept outside the repository and refetched by the committed script. The
federal funds rate stays the pinned `replication/peura_keppo_2006/fedfunds.csv`.

## Expected coverage (from the probe of 13 October 2026, TODO.md)

Stated before the fetch and checked after it, over the common sample:

1. Capital, assets, income and `EQCDIV` are present for at least 95% of
   bank-quarters in every quarter from 1984Q1.
2. `RWAJT` is present for at least 95% of bank-quarters in every quarter
   from 1990Q4 to 2019Q4, and empty before 1990. From 2020 banks that
   elect the community bank leverage ratio report no risk-weighted assets,
   so the share falls; the share is reported, not tested.
3. `EQCSXQ`, `EQCBHCTR` and `EQCMRG` are present for at least 95% of
   bank-quarters in every December from 1984 and in every quarter from
   2001Q1, and for less than half in the March, June and September
   quarters of 1985 to 2000.

A failed expectation is reported as found; nothing is refetched to meet
it.

## Reported

For each quarter: the number of institutions, of common-sample banks, and
the share of common-sample banks with each field present. The number of
distinct banks per year.

## Known limits

- The API serves the FDIC's current processing of past reports, which may
  differ from what a paper's authors downloaded at the time.
- Before 2001, quarterly equity-change flows exist only for the banks that
  filed the full schedule each quarter (HP-A3).
- `NETINC` and the other year-to-date items need differencing within the
  year for quarterly flows; the tests do that, not the stored file.
