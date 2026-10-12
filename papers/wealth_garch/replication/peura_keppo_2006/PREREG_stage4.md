# Pre-registration: Peura and Keppo replication, stage 4

Written and committed 12 October 2026, before the supplement below was
fetched and before any computation. Nothing in this file may change after
the first run.

## Aim

Peura and Keppo test their model on capital levels only (section V). Their
model also predicts when dividends are paid, how far capital may rise above
the dividend barrier, when capital is issued and where an issue lands.
Stage 4 tests these predictions bank by bank, with each bank's own
barriers from stage 3, on 2015-2024. The results are the baseline's
scorecard for the comparison with the enhanced model.

## Banks and parameters

The banks of stage 3, primary window (ROA 2015-2024), samples A and B, with
μ_i, σ_i, u1_i, u2_i and u0_i computed exactly as there (stage3.py on the
pinned fdic_dec_2004_2024.csv.gz and fedfunds.csv; Δ = 0.5, K = 0.25%,
ρ = 4%). A bank with a unique admissible (u1, u2) is an issuer bank; any
other bank is a non-issuer bank, for which the model's optimum is never to
issue, with dividend barrier u0_i.

## Data

- Quarterly: the pinned panel fdic_distress_panel_2015_2024.csv.gz (sha256
  `3cb78c0aa33691037f275eaf7bea5a75d02958161e0f8f7f2318fae4b0cc879b`), and a
  supplement fetched after this commit: FDIC `financials`, report dates
  2015Q1-2024Q4, fields `CERT`, `REPDTE`, `RBCT2`, joined on (`CERT`,
  `REPDTE`). Pinned by sha256 in the results file.
- Quarterly buffer b_q = (RBCT1J + RBCT2)/RWAJT - 0.08 at the quarter end;
  missing if any input is missing or RWAJT ≤ 0.
- Quarterly flows as in E2: year-to-date items differenced within the
  year, a missing previous quarter giving a missing flow. Bank-quarters
  with ASSET below 100,000 are dropped, as in E2.
- σ_i is annual; one quarter's noise is σ_i/2.

## Tests

T1, dividend location. Over dividend-paying quarters (EQCDIV flow > 0) with
b_{q-1} observed: the share with b_{q-1} below the bank's dividend barrier
(u2_i, or u0_i for non-issuer banks) by more than one quarter's noise,
b_{q-1} < barrier - σ_i/2. The model predicts 0%.
Holds if the share is at most 5%; fails otherwise.

T2, capital above the barrier. Over all bank-quarters with b_q observed:
the share with b_q above the barrier by more than one year of expected
earnings and two quarters of noise, b_q > barrier + μ_i + σ_i. The model
pays out capital above the barrier, so it predicts 0% up to discreteness.
Holds if the share is at most 5%; fails otherwise.

T3, issue timing (issuer banks). Recapitalisation episodes as E2: in
quarter q, EQCSXQ_q + ΔEQCBHCTR_q ≥ 0.005 ASSET_q, ΔEQCMRG_q = 0, and
b_{q-1}, b_q observed. Share of episodes with b_{q-1} ≤ u1_i + σ_i (the
issue barrier plus the noise of the six-month delay, σ_i√0.5, rounded up
to σ_i). The model predicts all.
Holds if the share is at least 80%; fails otherwise.

T4, issue landing (issuer banks). Over the same episodes, the share with
|b_q - u2_i| ≤ σ_i (two quarters of noise). The model lands every issue at
u2_i.
Holds if the share is at least 80%; fails otherwise.

Each test is applied to samples A and B separately. A test with fewer than
20 observations in a sample is "not measured" there.

## Reported, not used for a verdict

- The number of issuer and non-issuer banks; for each test, the
  observations and the share; for T1 and T2, the shares by issuer and
  non-issuer banks.
- The median of b_{q-1} - barrier over dividend-paying quarters (T1) and of
  b_q - u2_i over episodes (T4), in percentage points.

## Known limits

- Quarterly reports see the capital ratio once a quarter; the model's
  barriers act continuously. The tolerances above allow one quarter's
  noise (T1, T2 allow more).
- The 8% minimum is the model's; the conservation buffer raised effective
  minima after 2016 (stage 3).
- Issuance includes capital from the parent (EQCBHCTR), as in E2.
