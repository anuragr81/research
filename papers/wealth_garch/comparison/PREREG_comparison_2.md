# Pre-registration: the testing thread's comparison, second file

Written and committed 18 October 2026, after PREREG_comparison.md (commit
26f44a15) and before any share was computed. It fixes the details that
file leaves to implementation, for the observed facts (facts.py) and for
the baseline's predictions (predict_pk.py). Nothing else changes.

## What was seen before this file

As PREREG_comparison.md. Nothing has been computed with these rules.

## Banks and inputs (training window)

- Annual inputs from the pinned December panel of Peura and Keppo stage 3
  (replication/peura_keppo_2006/fdic_dec_2004_2024.csv.gz) and the pinned
  fedfunds.csv, reading December reports 2004-2014 only.
- mu_i and sigma_i: the mean and standard deviation of Peura and Keppo's
  annual excess return on risk-weighted assets, ROA_t = NETINC_t / R_{t-1}
  - r_t X_{t-1} (stage 3, rule 3, with its asset-growth screen), over ROA
  years 2005-2014, at least 8 values. This return is in the units of the
  buffer (capital over risk-weighted assets), so sigma_i measures the
  buffer's own earnings risk. sigma_i is the unit of the facts for every
  paper.
- Sample A: national banks (BKCLASS N in every December report 2004-2014
  they file), mu_i > 0, at least 8 ROA values, and the buffer observed in
  the quarterly data at 2014Q4. Sample B: the 61 largest of A by mean
  total assets over 2005-2014. The same banks are used for every paper.

## Observed facts (facts.py)

- Quarterly data: common_data/fdic_q_2014.csv.gz to fdic_q_2024.csv.gz;
  year-to-date items (EQCDIV, EQCBHCTR, EQCMRG) differenced within the
  year as in stage 4; EQCSXQ taken as reported, as in stage 4.
- Buffer b_q = (RBCT1J + RBCT2) / RWAJT - 0.105, missing if an input is
  missing or RWAJT <= 0, and the bank-quarter dropped if ASSET < 100,000.
- F1 to F4 exactly as PREREG_comparison.md, over 2015Q1-2024Q4, pooled
  over the banks of each sample, with counts.

## Baseline predictions (predict_pk.py)

- Each bank's barriers as Peura and Keppo stage 3 (stage2.solve: Delta =
  0.5, K = 0.25%, rho = 4%) from its mu_i, sigma_i. Issuer bank: a unique
  admissible (u1, u2); otherwise the bank never issues and pays dividends
  at u0.
- The model's state x is the buffer over the model's own 8% minimum,
  starting at the bank's observed 2014Q4 value. Paths: dx = mu_i dt +
  sigma_i dW, Euler steps of 1/360 year, 200 paths per bank, seed
  20261018. Dividends: x is held at the dividend barrier (u2, or u0), and
  any quarter in which it is held is a dividend quarter; a start above the
  barrier pays the excess at once. Issue (issuer banks): when x reaches u1
  an issue starts; after Delta years, if x has stayed above 0, x jumps to
  u2. Failure: x reaching 0 ends the path, as in the model.
- Quarterly records: b_q = x_q + 0.08 - 0.105 at each quarter end; an
  episode is an issue arriving in quarter q whose size, u2 - x before the
  jump, is at least 0.005 ASSET / RWAJT at the bank's 2014Q4 values (the
  data's 0.5%-of-assets rule in buffer units), with pre = b_{q-1} and
  now = b_q.
- sigma for a simulated bank: its earnings volatility is sigma_i by
  construction, so sigma_i is used, as PREREG_comparison.md asks.
- Predicted shares: pooled over all paths and banks of each sample, in the
  same way as the observed facts. Verdict per fact as PREREG_comparison.md
  (agrees if within 10 percentage points; fewer than 20 observed events:
  not measured).

## Known limits

- The observed start (2014Q4) is used for each path; banks whose 2014Q4
  buffer is at or below the model's 8% minimum start failed and are
  counted and left out of the predictions.
- Euler steps of 1/360 year approximate the continuous barrier; the
  number of barrier touches within a quarter is not used, only whether
  one occurs.
