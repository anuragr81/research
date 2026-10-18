# Pre-registration: the testing thread's comparison

Written and committed 18 October 2026, before any share below was
computed. The author took the five open decisions on 18 October 2026 by
accepting the recommendations; they are recorded where they apply.
Nothing in this file changes after the first run; a change is a new
file with a new date, and both are reported.

## Aim

Score each implemented background paper, and later each enhanced model, on
one question asked of all of them: how do banks manage their capital
buffer? The question is put as four facts measured on the common data
(option B, agreed 18 October 2026): each paper predicts the four facts
from its own mechanism, and no paper's construct (such as Peura and
Keppo's barriers) defines the test. Peura and Keppo are rescored on the
same four facts.

## What was seen before this file

- Peura and Keppo stage 4 (results_stage4.md): with each bank's own
  barriers, on 2015-2024, dividends are rarely paid below the dividend
  barrier (0.6% and 0.3%), capital sits far above it in 94.3% and 92.7% of
  bank-quarters, and recapitalisation episodes (201 in A, 35 in B) seldom
  start near the issue barrier (7.0%, 5.7%) or land near the upper one
  (8.0%, 11.4%). The median dividend is paid 4 to 6 points above the
  barrier.
- The replications of Gordon and Hirtle (stages 1 and 2), Berger et al.
  (stage 1) and Mankart, Michaelides and Pagratis (stages 1 and 2).
- No share of the four facts below has been computed with the reference
  level R.

## Common data, window, samples

- Data: common_data/ (FDIC Call Reports, pinned), quarterly.
- Test window 2015Q1-2024Q4; the 2025 hold-out stays sealed.
- Inputs that a paper takes from data (a bank's earnings mean and
  volatility, a target capital ratio, an adjustment speed) come from the
  training window 2005Q1-2014Q4 only, never from the test window.
  (Peura and Keppo's stage 3 and 4 took mu_i, sigma_i from 2015-2024;
  their rescoring here uses 2005-2014, so their numbers here can differ
  from stage 4's.)
- Samples A and B as in Peura and Keppo stage 4; B primary, A reported.

## The buffer and the events

- Buffer b_q = (RBCT1J + RBCT2) / RWAJT - R at the quarter end, missing if
  an input is missing or RWAJT <= 0; bank-quarters with ASSET below
  100,000 dropped (as stage 4).
- R = 10.5% (author's decision): the 8% total-capital minimum plus the
  2.5% capital conservation buffer of Basel III (B3-Q4), below which
  distributions are restricted. The alternative, 8%, is Peura and Keppo's
  own level. The US phase-in of the buffer over 2016-2019 is a stated
  limit.
- Dividend quarters: EQCDIV flow > 0 (as stage 4). Recapitalisation
  episodes: as stage 4 (EQCSXQ_q + change in EQCBHCTR_q >= 0.005 ASSET_q,
  no merger change, b_{q-1} and b_q observed).
- sigma_i: the bank's annual ROA volatility over 2005-2014, the unit in
  which "thin" and "far" are measured, so that a volatile bank's buffer is
  judged against its own risk. For a simulated bank, sigma is its own
  annual ROA volatility over its simulated test-window path, so that model
  and data are measured the same way.

## The four facts (shares, on the test window)

- F1, payouts from a thin buffer: over dividend quarters, the share with
  b_{q-1} < sigma_i (less than a year's earnings noise above R).
- F2, capital far above R: over bank-quarters, the share with b_q > 4
  sigma_i.
- F3, recapitalisations start thin: over episodes, the share with b_{q-1}
  < sigma_i.
- F4, recapitalisations land moderately: over episodes, the share with
  sigma_i <= b_q <= 4 sigma_i.
- The multiples 1 and 4 (author's decision) are set here before any
  share is computed and are not chosen by any paper's fit. The
  alternative, fixed points above R, ignores how risky the bank is.
- A fact with fewer than 20 observations in a sample is "not measured"
  there.

## What each paper predicts, and how

A paper is scored on a fact only if its mechanism produces that fact; it
is otherwise "not scoreable", stated with the reason. A prediction is a
share computed on simulated or fitted paths for the same banks and window.

- Peura and Keppo (baseline): each bank's barrier policy with mu_i,
  sigma_i from 2005-2014, Delta = 0.5, K = 0.25%, rho = 4% as stage 3;
  each bank's buffer simulated from its 2014Q4 value over the 40 test
  quarters, many paths. Scoreable: F1-F4.
- Enhanced models (enhancement thread): as Peura and Keppo, with their own
  policy. Scoreable: F1-F4.
- Mankart, Michaelides and Pagratis: the stage 2 small-bank solution at
  the published parameters, simulated; buffer = E / (L + 0.2 S) - R in the
  model's units (their risk weights). Scoreable: F1 and F2. F3 and F4: the
  model has no equity issue, so it predicts no recapitalisation episodes;
  scored as a prediction of "no episodes": a fail on F3 and F4 in a
  sample where episodes are measured (20 or more), since the data
  contradict it (author's decision). Large banks: not scoreable
  (PREREG_stage2_7.md).
- Berger et al.: partial adjustment of the capital ratio towards a target
  at a constant speed, both estimated on 2005-2014 banks with their
  Step 1 specification; simulated forward from 2014Q4 with the fitted
  residual spread. Scoreable: F2. F1 (dividends) and F3, F4 (episodes):
  the model does not say through which channel capital moves, so not
  scoreable.
- Gordon and Hirtle: regressions of payout behaviour, not a model of the
  buffer's path. Not scoreable on F1-F4 (author's decision): scoring F1
  through their linear probability model of payouts would need a mapping
  their paper does not give.
- Not yet implemented (Bayraktar et al., Holod and Peek, Hirtle): each
  gets its row here before it is scored.

## Verdicts

- For each paper and fact: the predicted share against the observed
  share; agrees if within 10 percentage points (author, 13 October 2026).
- Reported as a table, paper by fact, samples A and B, with the counts.
  No overall ranking across papers that are scoreable on different facts.
- The positive claim of the paper is about the enhanced model against
  the baseline: on the facts both can be scored on, sample B, the count
  of facts that agree.
- Across an enhancement's pre-fixed parameter grid (author's decision):
  the verdict is taken at the grid's central point, and every grid point
  is reported. The alternatives, every point or the majority of points,
  were not taken.

## Known limits

- Quarterly reports see the buffer once a quarter.
- A model's own regulatory floor can differ from R (MMP's is a 6%
  risk-weighted ratio); the facts measure the buffer above the common R
  whatever floor the model assumes.
- Recapitalisations include capital from the parent (as stage 4).
