# Pre-registration: Mankart, Michaelides and Pagratis replication, stage 2

Written and committed 14 October 2026, before any solver code was written.
Nothing in this file may change after the first run of the full model
(steps 2d below). A change is a new file with a new date, and both are
reported.

## Aim

Solve the model of MODEL.md (the August 2018 version) at its Table 4
parameters, for large and small banks, simulate it, and compare the model
moments with the model columns of their Table 5.

## What was seen before this file

MODEL.md (both versions of the paper read in full: the May 2016 draft and
the August 2018 manuscript); stage 1 results (results_stage1.md). No part
of the model has been solved or simulated.

## Choices for what the paper leaves open (MODEL.md R1-R6)

- Decision state. At the start of quarter t, after profits and write-offs:
  e = (E_t-1 + after-tax Pi_t)/D_t (equity before dividends), l =
  (1 - theta - w_t) L_t-1 / D_t (loans after repayment and write-offs),
  the write-off node w_t and the cycle state b_t. The bank chooses x, n, f;
  s = 1 + f + e - x - l - n (step 7a); it needs s >= 0 and both capital
  constraints on post-dividend equity e - x: omega_L (l + n) + omega_S s
  <= lambda_w (e - x) and (l + n + s) <= lambda_u (e - x).
- Next quarter, per unit of today's deposits: profit pi' = (r_L' - w')(l + n)
  + r_S' s - r_D' - [r_D' f + phi_F f^2 - phi_E (e - x)^2] - g_N(n) - c, with
  g_N(n) = phi_N n^2 (n > 0) or psi phi_N n^2 (n <= 0); e' = [(e - x) +
  pi' (1 - tau if pi' > 0 else 1)] / Gamma'; l' = (1 - theta - w')(l + n) /
  Gamma'. This reads eq. (3)'s g_N(N_t-1, D_t-1) and g_F(F_t-1, D_t-1,
  E_t-1) as charged on last quarter's choices, E_t-1 being post-dividend.
- R1, failure value. Homogeneity: V = D^(1-gamma) v. A failed banker
  consumes c_D times deposits forever: v^d = u(c_D)/(1 - beta), u(x) =
  x^(1-gamma)/(1-gamma). Bellman: v(e, l, w, b) = max[v^d, max over x > 0,
  n, f >= 0 of u(x) + beta E[Gamma'^(1-gamma) v(e', l', w', b')]]; any
  choice set violating a constraint for every x > 0 leaves v^d.
- R2, discretisation.
  - Cycle: P(expansion to recession) = 1/20, P(recession to expansion)
    = 1/8 (durations of 20 and 8 quarters, p. 21).
  - Write-offs: per state, mu_b = m_b (1 - rho_b) and sigma_b = s_b
    sqrt(1 - rho_b^2) from Table 2's state mean m_b, AR(1) rho_b and s.d.
    s_b; Tauchen (1986) with 9 nodes on one grid spanning both states'
    means plus and minus three standard deviations, floored at zero (R4);
    w' given (w, b') from b''s AR(1).
  - Deposit growth: log Gamma' ~ N(mu_b'D, sigma_b'D^2) from Table 2's
    state mean and s.d. of real deposit growth, by 5-point Gauss-Hermite
    quadrature.
  - Grids: e on [0, 0.4] and l on [0, 2.5], 41 points each, with linear
    interpolation in two dimensions (n. 26). Choices: grid search over x,
    n, f followed by a bounded local refinement (Nelder-Mead from the best
    grid point); the grid sizes are fixed in the code before 2d and
    reported.
  - Value function iteration until the largest change is below 1e-6.
- R3, returns: fixed at their Table 2 state means: r_D the real deposit
  rate, r_L = r_D + loan spread, r_S = r_D + liquid-asset spread, by the
  state b' in which profits are realised; percentages per quarter / 100.
- R4, write-offs: grid floored at zero.
- R5, moments, per simulated bank over the last 500 quarters, then
  averaged across banks: loans/assets (l + n)/(l + n + s); deposits/assets
  1/(l + n + s); equity/assets (e - x)/(l + n + s); profit/equity pi /
  (e - x) of the quarter the profit is earned against the equity it ends
  in; dividends/equity x / (e - x); means and standard deviations over
  time; failure rate = failures per bank-quarter, in percent.
- R6, simulation: 10,000 banks, 2,000 quarters, last 500 used (step 13);
  every bank starts at e = 0.10, l = 0.6, the middle write-off node, in an
  expansion; a failed bank is replaced next quarter by one at the cross-
  sectional mean of e and l, the lowest write-off node (step 12). Seed
  20261014.

## Validation before the full model (step 2c)

V1. Known answer: with only a liquid asset at return r = 0.01, no loans,
    no wholesale funding, no costs, no tax, Gamma = 1, no constraints and
    no failure, the policy is x = kappa (e + 1) with kappa = 1 -
    beta^(1/gamma) (1 + r)^((1 - gamma)/gamma) (deposits are a perpetual
    income of r). Passes if the solved x is within 1% of this at every
    interior grid point, for gamma = 1.3023 and 1.9054.
V2. The discretised write-off chain reproduces each state's mean, s.d. and
    AR(1) within 10% (relative) by simulation; the quadrature reproduces
    the mean and s.d. of log Gamma.
V3. Grid check: at their parameters, the simulated Table 5 moments with
    the e and l grids at 41 and at 61 points; a moment that moves by more
    than 10% between them is reported as not converged.
The full model runs only after V1 and V2 pass.

## Targets and rule (step 2d)

Their Table 5 model columns, large and small, 11 moments each (failure
rate 0.092/0.051, loans/assets 0.704/0.626, deposits/assets 0.638/0.891,
equity/assets 0.065/0.067, profit/equity 0.055/0.029, dividends/equity
0.029/0.012, s.d.s 0.100/0.050, 0.062/0.021, 0.013/0.015, 0.035/0.024,
0.011/0.007). A moment reproduces if within 20% of theirs, or within
0.005 when theirs is below 0.05; failure rates within 0.05 percentage
points. The count is reported per size class; no overall verdict.

## Known limits

The choices above are ours; a miss may come from them rather than from the
model. Their solution used nested grids for (x, n, f) of unstated size.
