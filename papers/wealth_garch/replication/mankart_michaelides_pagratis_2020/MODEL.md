# The model of Mankart, Michaelides and Pagratis, as stated

Stage 2a of the replication. Version used: the August 2018 manuscript
(`MMP_2018Aug21.pdf`, Drive file 1ISiwGhVJUiwr9fbhDd0c3a4FvWfuM3Sz), which
matches Deutsche Bundesbank Discussion Paper 51/2018 (`1046840088.pdf`,
Drive file 1XQdu5vos1zRWuOgutx-bLYqMv0lhUUR2) in model, parameters and
appendix, and precedes the Financial Management article (49(2), 473-502,
2020). Read on 14 October 2026 through the Drive connector's text
extraction, which keeps Greek letters; page numbers are the manuscript's
printed pages. Not yet pinned locally (to do: local copies for quote
checks). The May 2016 NBER draft read first (lit record) differs in timing,
costs and parameters, and leaves more unstated; its gaps G1-G6 are listed
at the end with how the 2018 version settles them.

## Shocks (p. 14)

- (1) w_t+1 = mu_b + rho_b w_t + sigma_b eps_t+1, eps ~ N(0,1), by
  business-cycle state b (two-state Markov chain).
- (2) log(D_t+1/D_t) ~ N(mu_bD, sigma_bD^2), i.i.d. given b.
- Recessions last 8 quarters and expansions 20 on average (p. 21).
- Returns: the real deposit rate and the loan and liquid-asset spreads of
  Table 2, by state (p. 22); returns "vary with the business cycle" (p. 13).

## Timing, profits and equity (pp. 14-16)

- The bank enters t with L_t-1, S_t-1, D_t-1, F_t-1, E_t-1; shocks are
  realised; it decides to continue or fail; it liquidates S_t-1, repays
  F_t-1, realises loan losses w_t.
- (3) Pi_t = (r_L,t - w_t) L_t-1 + r_S,t S_t-1 - r_D,t D_t-1
  - g_F(F_t-1, D_t-1, E_t-1) - g_N(N_t-1, D_t-1) - c D_t-1.
- Taxes tau on positive profits only.
- (4) E_t = E_t-1 - X_t + (1 - tau) Pi_t 1{Pi_t > 0} + Pi_t 1{Pi_t <= 0}.
- (5) L_t = (1 - theta - w_t) L_t-1 + N_t; N_t < 0 is liquidation.
- (6) L_t + S_t = D_t + F_t + E_t.
- (7) (omega_L L_t + omega_S S_t) / E_t <= lambda_w, E_t after dividends
  and retained profits.
- (8) (L_t + S_t) / E_t <= lambda_u.

## Objective (pp. 17-18)

- (9) V = E_0 sum beta^t X_t^(1-gamma)/(1-gamma).
- (10) V^C(L_t-1, D_t-1, E_t-1; Omega_t) = max over X_t, S_t, F_t, N_t of
  X_t^(1-gamma)/(1-gamma) + beta E_t[V(...; Omega_t+1)].
- (11) V = max[V^D, V^C]: limited liability, outside option V^D. A bank
  that violates a constraint even at zero dividends fails; slightly above,
  it may prefer the outside option (p. 18).

## Costs (pp. 18-19)

- (12) g_N(N, D) = 1{N>0} phi_N N^2/D + 1{N<=0} psi phi_N N^2/D.
- (13) g_F(F_t-1, D_t-1, E_t-1) = r_D,t F_t-1 + phi_F F_t-1^2/D_t-1
  - phi_E E_t-1^2/D_t-1: the base rate is the risk-free deposit rate.

## Normalisation and algorithm (pp. 55-58)

- Divide by D_t; Gamma_t = D_t/D_t-1.
- (15) (omega_L (l_t + n_t) + omega_S s_t) / e_t <= lambda_w.
- (16) e_t = (e_t-1 - x_t)/Gamma_t + (1 - tau) pi_t 1{Pi>0} + pi_t 1{Pi<=0}.
- s = 1 + f + e - x - l - n (step 7a).
- Two continuous states (e, l); write-offs discretised "using the
  procedures described by Adda and Cooper (2003)"; nested grids for
  (x, n, f) at each node; two-dimensional linear interpolation; value
  function iteration to a tolerance; a constraint violation gets v^d; the
  continuation value is v or v^d (steps 5-8).
- Simulation: 10,000 banks, 2,000 periods, last 500 used; a failed bank is
  replaced by one with mean equity, mean loans and low loan losses (steps
  10-13).
- Estimation weights: standard deviations of the data moments, with "a high
  weight" on the failure rate (n. 19, p. 23).

## Parameters

| Parameter | Large | Small | Source |
|---|---|---|---|
| phi_F | 0.0092 | 0.0803 | Table 4, p. 24 |
| phi_E | 0.0705 | 0.0072 | Table 4 |
| beta | 0.9753 | 0.9859 | Table 4 |
| gamma | 1.3023 | 1.9054 | Table 4 |
| c | 0.0110 | 0.0096 | Table 4 |
| phi_N | 0.4352 | 0.9005 | Table 4 |
| c_D | 2e-5 | 4e-5 | Table 4 |
| theta | 0.06 | 0.08 | p. 22 |
| psi | 1.3 | 1.3 | p. 22 |
| tau | 0.15 | 0.15 | p. 22 |
| lambda_w, lambda_u | 16.66, 33.33 | same | p. 22 |
| omega_L, omega_S | 1, 0.2 | same | p. 22 |
| shocks and rates by state | Table 2 | Table 2 | p. 22 |

Table 5 (model and data moments) is identical to the 2016 draft's.

## Gaps of the 2016 draft and the 2018 version

| Gap (2016) | 2018 |
|---|---|
| G1 wholesale base rate r_F | settled: r_D, eq. (13) |
| G2 liquidation cost psi | settled: 1.3, p. 22 |
| G3 failure value | partly: V = max[V^D, V^C] with an outside option, but v^d's formula from c_D is not given |
| G4 g_F rate or total | settled: total cost in profits (3) |
| G5 timing of g_N | settled: in profits (3), on last period's new loans |
| G6 normalisation dates | settled: (16) |

## Remaining gaps (2018)

- R1. v^d as a function of c_D (step 5) is not given.
- R2. Discretisation: number of write-off nodes; how the state-dependent
  AR(1) is discretised across regime switches; nodes for deposit growth;
  grid sizes and ranges for e, l, x, n, f; the tolerance.
- R3. Whether returns are fixed at their state means or random.
- R4. Bounds on simulated write-offs (a normal AR(1) can go negative).
- R5. The model counterparts of the Table 5 moments: definitions of the
  ratios, profit as Pi before or after tax, standard deviations per bank
  over time or across the panel, failure rate per quarter.
- R6. Initial states of the simulated banks; the shock seed.

R1 changes the model; R2 to R6 are numerics and measurement. Each needs a
choice fixed in the stage 2 pre-registration and reported as ours.
