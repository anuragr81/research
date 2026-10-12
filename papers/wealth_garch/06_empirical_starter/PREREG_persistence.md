# Pre-registration: deficit and surplus persistence (row M12)

Written and committed 12 October 2026, before any persistence was computed.
The only figure seen beforehand is a count of deficit quarters, used to
check feasibility: 774 consecutive pairs starting in deficit across 129
banks at the primary requirement, over all bank sizes. Nothing in this file
may change after the first run. A change is a new file with a new date, and
both are reported.

## Question

Does the discrete-time model of `MANUSCRIPT.tex` describe the persistence
of US banks' capital buffers? The state is $z_t=\log(E_t/Q_t)$, equity over
required capital. The model has $z_t=\phi z_{t-1}+\varepsilon_t$ with

- deficit persistence $\phi^-=1-g(1-b)/b$ when $z_{t-1}<0$, with $g$ the
  growth rate of required capital and $b$ the retention ratio;
- surplus persistence $\phi^+=1-\delta_p$ when $z_{t-1}>0$, with $\delta_p$
  the surplus payout rate.

`MANUSCRIPT.tex` App. D records these as a maintained model, not a
derivation. Row M12 proves their algebra in Lean, including that $\phi^-$
falls in $g$ for $0<b<1$.

## Data

- `fdic_distress_panel_2015_2024.csv.gz`, sha256
  `3cb78c0aa33691037f275eaf7bea5a75d02958161e0f8f7f2318fae4b0cc879b`,
  fields defined in `lit/fdic_2026`.
- Flows (`NETINC`, `EQCDIV`, `EQCMRG`) are year to date and decumulated by
  quarter as in `PREREG_distress_targeting.md`. A missing previous quarter
  makes the flow missing, never zero.

## Measurement

- $E_t$ is Tier 1 capital (`RBCT1J`).
- Primary requirement $Q_t=0.085\,\texttt{RWAJT}_t$, the Tier 1 minimum of
  6.0% (B3-Q2) plus the conservation buffer of 2.5% (B3-Q4). Secondary,
  reported and not used for any verdict: $Q_t=0.06\,\texttt{RWAJT}_t$.
- $z_t=\log(E_t/Q_t)$, defined when $E_t>0$ and `RWAJT` $>0$.
- A pair is two consecutive quarters of one bank, both with `ASSET` at
  least 100,000 (thousands of dollars), with no merger-driven equity change
  in the second quarter ($\Delta$`EQCMRG` $=0$).
- $g_t=\texttt{RWAJT}_t/\texttt{RWAJT}_{t-1}-1$.
- $b$ over the four quarters ending at $t-1$: $1-\sum\text{div}/\sum\text{NI}$,
  defined when $\sum\text{NI}>0$. Deficit pairs enter T1 and T2 only with
  $0<b\le1$.
- $\delta_p$ over surplus pairs:
  $\sum\log\big(E_{t-1}/(E_{t-1}-\text{div}_t)\big)\big/\sum z_{t-1}$, the
  share of the log-surplus paid out per quarter, from pairs with
  $0\le\text{div}_t<E_{t-1}$.

## Tests

The persistence estimate is pooled least squares of $z_t$ on $z_{t-1}$
through the origin, as in the model. Uncertainty is a bank-cluster
bootstrap, 2,000 draws, seed 20261012, resampling banks and recomputing
every estimate and prediction in each draw.

- T1, deficit level. $\hat\phi^--\phi^{-*}$, with
  $\phi^{-*}=1-\bar g(1-\bar b)/\bar b$, $\bar g$ the mean of $g$ and
  $\bar b=\sum(\text{NI}-\text{div})/\sum\text{NI}$, both over the T1 pairs.
- T2, deficit sign. $\hat\phi^-$ on pairs with $g$ at or below the median
  of the T1 pairs, minus $\hat\phi^-$ above it. The model predicts a
  positive difference.
- T3, surplus level. $\hat\phi^+-(1-\hat\delta_p)$ over surplus pairs.

A test runs only with at least 100 pairs from at least 30 banks. Otherwise
its verdict is not measured.

## Verdict rule

- T1 and T3: holds if the 95% bootstrap interval of the difference
  contains 0 and its half-width is at most 0.05; fails if it excludes 0;
  otherwise not measured.
- T2: holds if the 95% interval lies above 0; fails if it lies below 0;
  otherwise not measured.

## Known limits

- The model takes $g$, $b$ and $\delta_p$ as constants. The test uses
  their sample averages.
- Tier 1 capital is net of regulatory adjustments (B3-D2), so $E_t$ is not
  book equity.
- Earnings and risk-weight changes move $z$ outside the model's two
  mechanisms.
