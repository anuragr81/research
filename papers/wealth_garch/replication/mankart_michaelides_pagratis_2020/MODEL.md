# The model of Mankart, Michaelides and Pagratis (May 2016 draft), as stated

Stage 2a of the replication: the model written out from the paper, every
item with its PDF page, and the gaps the paper leaves. The text layer of
the PDF drops Greek letters; the equations below were transcribed from
the pages rendered as images (110 dpi) on 14 October 2026, and checked by
eye against them. Printed page = PDF page - 2.

## State, shocks and timing (pp. 16-18)

- Balance sheet: loans L (return r_L), liquid assets S (r_S); deposits D
  (r_D), wholesale funding F (cost g_F), equity E (Table 3, p. 16).
- Aggregate state b: two-state Markov chain, expansion or recession;
  recessions last 8 quarters and expansions 20 on average (pp. 23-24; the
  appendix says 2 and 5 years, p. 56).
- Loan write-offs (1), p. 16: w_{t+1} = mu_b + rho_b w_t + sigma_b eps_{t+1},
  eps ~ N(0,1), parameters by state b.
- Deposit growth (2), p. 16: log(D_{t+1}/D_t) ~ N(mu_bD, sigma_bD^2), i.i.d.
  given b.
- Returns: "the various returns r ... stochastic and vary with the
  business cycle" (p. 15); their values are the state-conditional means of
  Table 2 (p. 14): real deposit rate, loan spread, liquid-asset spread.
- Timing (p. 17): at t+ the bank chooses whether to continue; if it does,
  it liquidates S_t, repays F_t, makes new loans N_t (L_{t+} = L_t + N_t;
  N_t < 0 is liquidation at a cost), chooses S_{t+} >= 0, F_{t+} and
  dividend X_t, with E_{t+} = E_t - X_t.
- Balance sheet (3), p. 17: L_{t+} + S_{t+} = D_t + F_{t+} + E_{t+}.
- Capital adequacy (4), p. 17: (omega_L L_{t+} + omega_S S_{t+}) / E_{t+}
  <= lambda_w.
- Leverage (5), p. 18: (L_{t+} + S_{t+}) / E_{t+} <= lambda_u.
- Loans (6), p. 18: L_{t+1} = (1 - theta - w_{t+1}) L_{t+}, theta the
  repaid fraction.
- Pre-tax profit (7), p. 18: Pi_{t+1} = (r_{L,t+1} - w_{t+1}) L_{t+}
  + r_{S,t+1} S_{t+} - r_{D,t+1} D_t - g_{F,t+1} F_{t+} - g_N - c D_t.
- Equity (8), p. 19: E_{t+1} = E_{t+} + (1 - tau) Pi_{t+1} 1{Pi>0}
  + Pi_{t+1} 1{Pi<=0}.

## Objective (pp. 19-20)

- (9): V = E_0 sum_t beta^t X_t^{1-gamma}/(1-gamma).
- (10): V^C(L, D, E; w, r) = max over X, S_{t+}, F_{t+}, N of
  { X^{1-gamma}/(1-gamma) + E_t[beta V(L', D', E'; w', r')] }.
- (11): V = max[0, V^C], "limited liability".

## Costs (p. 21)

- Screening (12): g_N(N, D) = 1{N>0} phi_N N^2/D + 1{N<=0} psi phi_N N^2/D,
  psi > 1.
- Wholesale funding (13): g_F(F, D, E) = r_{F,t+1} F + phi_F F^2/D
  - phi_E E^2/D.

## Normalisation and algorithm (pp. 55-59)

- Variables divided by D_t; Gamma_{t+1} = D_{t+1}/D_t (p. 55).
- (14), p. 56: (omega_L (l + n) + omega_S s) / (e - x - g_N(n)) <= lambda_w.
- (15), p. 56: e_{t+1} = (e_t - x_t)/Gamma_{t+1} + (1 - tau) pi_{t+1}
  1{Pi>0} + pi_{t+1} 1{Pi<=0} - g_N(n_t).
- Liquid assets from the balance sheet: s = 1 + f + e - x - l - n
  (p. 57, step 7a).
- Failure value v^d implied by "consumption after failure" c-bar
  (p. 57, step 5); a candidate that violates a constraint gets v^d
  (step 7c); the continuation value is v or v^d (step 7b).
- Grids for e and l; three nested grids for (x, n, f) at each node;
  two-dimensional linear interpolation; value function iteration to a
  tolerance (pp. 56-58).
- Write-offs discretised "using the procedures described by Adda and
  Cooper (2003)" (p. 56).
- Simulation: 10,000 banks, 2,000 periods, the last 500 used; a failed
  bank is replaced by one with mean equity, mean loans and a low loss
  state (pp. 58-59).

## Parameter values

| Parameter | Large | Small | Source |
|---|---|---|---|
| phi_F | 0.0092 | 0.081 | Table 4, p. 25 |
| phi_E | 0.07 | 0.007 | Table 4 |
| beta | 0.975 | 0.986 | Table 4 |
| gamma (CRRA) | 1.31 | 1.89 | Table 4 |
| c_D (consumption after failure) | 2e-5 | 4e-5 | Table 4 |
| c (operating cost) | 0.011 | 0.0096 | Table 4 |
| phi_N | 0.63 | 0.90 | Table 4 |
| theta | 0.06 | 0.08 | p. 24, p. 55 |
| tau | 0.15 | 0.15 | p. 24 |
| lambda_w, lambda_u | 16.66, 33.33 | same | p. 24 |
| omega_L, omega_S | 1, 0.2 | same | p. 24 |
| w, deposit growth, rates, by state | Table 2 | Table 2 | p. 14 |

## Gaps: items the paper does not state or states inconsistently

- G1. r_F, the base rate on wholesale funding in (13): not given.
- G2. psi, the liquidation cost multiplier in (12): only "psi > 1".
- G3. v^d: the formula from c-bar is not given; (11) bounds the value at 0,
  which with gamma > 1 (negative utility) would make exit always optimal,
  while the appendix uses v^d.
- G4. g_F is a rate times F_{t+} in (7) but a total cost in (13); (13)
  uses E_t without saying whether before or after the dividend.
- G5. g_N is paid out of next period's profit in (7) but out of current
  equity in (14) and (15).
- G6. (15) divides the profit by D_{t+1} but g_N(n_t) by D_t.
- G7. The discretisation: number of write-off nodes, how the
  state-dependent AR(1) is discretised across regime switches, nodes for
  deposit growth; grid sizes and ranges for e, l, x, n, f.
- G8. Whether rates are real or nominal in the model (Table 2's deposit
  rate is real; its spreads are differences).
- G9. Whether simulated write-offs are bounded (a normal AR(1) can go
  negative).
- G10. The model counterparts of the Table 5 moments: loans/assets as
  l/(l + s), deposits/assets as 1/(l + s), and whether standard deviations
  are per bank over time (as the data) or across the panel.
- G11. Initial states of the simulated banks.

## Consequence for stage 2

G1 to G6 change the model, not only its numerics. A reproduction of the
Table 5 model columns therefore needs either these values from the
authors' published version or code, or choices fixed in a pre-registration
and reported as ours.
