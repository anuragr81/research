# Pre-registration: Peura and Keppo replication, stage 3

Written and committed 12 October 2026, before the data below were fetched.
Nothing in this file may change after the first run. A change is a new file
with a new date, and both are reported.

## Aim

Apply the method of stage 2, run 2 (PREREG_stage2.md with the screen of
PREREG_stage2_2.md) to recent data, and report findings F1 to F7 under the
same rule. Nothing from 2025 or later is fetched (the hold-out).

## Windows

- Primary: ROA years 2015-2024 (December reports 2014-2024). At least 8
  ROA values (their 15 of 20, as a share). Buffer over 2015-2024.
- Secondary: ROA years 2005-2024 (December reports 2004-2024), their
  length. At least 15 ROA values. Buffer over the last ten years,
  2015-2024, as theirs over 1993-2002.

Both are reported in full; neither is preferred in the reading.

## Data

- FDIC BankFind API, endpoint `financials`, December reports 2004-2024,
  the fields of stage 2: `CERT`, `REPDTE`, `BKCLASS`, `ASSET`, `NETINC`,
  `EQ`, `ELNATR`, `RWAJT`, `RBCT1J`, `RBCT2`, `RBCRWAJ`.
- FRED `FEDFUNDS`, as in stage 2, r_t the mean of year t's twelve months.
- Both pinned by sha256 in the results file.

## Rules, as stage 2 run 2 except where stated

1. National banks: `BKCLASS` = N in every year the bank reports in the
   window.
2. R_t = RWAJT; X_t = (RBCT1J + RBCT2)/R_t. No imputation: risk-weighted
   assets are reported in every year of both windows. A year without them
   (from 2020, banks that elect the community bank leverage ratio) has no
   R_t or X_t.
3. ROA_t = NETINC_t/R_{t-1} - r_t X_{t-1}, entering only if
   0.5 ≤ ASSET_t/ASSET_{t-1} ≤ 2.
4. Buffer: the mean over 2015-2024 of RBCRWAJ/100 - 0.08, at least 5 years.
   The model's minimum stays 8% for comparability with stages 1 and 2,
   although the Basel III conservation buffer adds 2.5 points from 2019.
5. Provisions: the mean of ELNATR/ASSET over the window's ROA years;
   above-average means above the sample mean.
6. μ > 0; samples A (all) and B (61 largest by mean total assets over the
   window's ROA years).
7. Barriers as stage 2 (Δ = 0.5, K = 0.25%, ρ = 4%, starting points scaled
   to u0, search below 1.5 u0, stage 1's admissibility test).

## Findings and rule

F1 to F7 of PREREG_stage2.md, with the same reproduction rule, for each
window and sample. Their numbers are printed beside ours. Spearman rank
correlations for F1 and F3 are reported and not used.

## Known limits

- Ten return years in the primary window; standard errors of μ and σ are
  wider than theirs.
- The conservation buffer raises effective minima after 2016; the 8% buffer
  overstates the distance to the binding requirement.
