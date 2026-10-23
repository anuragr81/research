# Claims: Bayraktar, Chevalier, Ly Vath and Wang (2026)

Bib key `bayraktar2026`. Version read: arXiv:2603.14557v2. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| BCVW-Q1 | 17 | the parameter a3 scales the entire liquidity constraint and determines its asymptotic upper bound | The cap of M2 and M3 is this paper's eq. (4). Its surplus-side limit is governed by $a_3$ (L1). |
| BCVW-Q2 | 11 | At the hypothetical zero-equity level y = 1, the solvency constraint forces π = 0 and the diffusion coefficient of the ratio process vanishes. | The degeneracy at $x=1$ is in print (H2 in `TODO.md`). |
| BCVW-Q3 | 11 | Thus, y = 1 is locally repelling when r > rL and locally attracting when r < rL | The condition $r>r_L$ of PROOFS_v2's `prop:rrL` is in print (H2b). |
| BCVW-Q4 | 9 | Fixed issuance costs, nonlinear issuance costs, taxes, deposit insurance premia, endogenous deposit rates, balance-sheet size effects, multiple asset classes, or regulatory constraints depending on dollar levels rather than ratios would generally break the one-dimensional reduction. | A fixed issuance cost $K$ keeps the reduction only if it is charged per unit of liabilities. Appendix D must say so when $K$ enters a row. |
| BCVW-Q5 | 13 | If one introduced fixed issuance costs, endogenous market access, or adverse-selection costs that rise near distress, earlier recapitalization or inaction at the boundary could become optimal. | The paper has no fixed issuance cost. Its issuance occurs only at the boundary (H3, H4). |
| BCVW-Q6 | 2 | Rather than using the zero-equity level y = 1 as the intervention boundary, we introduce an internal distress threshold | The recapitalisation threshold is exogenous here, unlike the trigger $x_L$ of PROOFS_v2 (H4). |
| BCVW-Q7 | 8 | This kinked investment cap is the main channel through which Basel-style regulation affects the bank's payout and recapitalization decisions. | The cap is the paper's mechanism. |
| BCVW-Q8 | 15 | The key structure is the minimum operator in the regulatory constraint, with a switching point | The crossing point $\bar x$ of M2 is their switching point (BCVW-D1). |
| BCVW-Q9 | 7 | the solvency ratio is the ratio of shareholders' equity to risky asset holdings | $a_1$ is a floor on equity over risky assets. The measurement map reads risky assets as risk-weighted assets (G4 in `MEASUREMENT_MAP.tex`). |
| BCVW-Q10 | 8 | model the 30-day net outflow as a fixed fraction a2 ∈ (0, 1) of liabilities | $a_2$ is a run-off rate on all liabilities (G5). |
| BCVW-Q11 | 8 | We assume that risk-free assets qualify fully as HQLA. | The riskless asset enters HQLA without haircut, as Level 1 assets do in the LCR standard (G6). |
| BCVW-Q12 | 8 | a fraction a3 ∈ (0, 1) is excluded from HQLA | $a_3$ is a single haircut on the risky asset (G6). |
| BCVW-Q13 | 13 | The baseline regulatory parameters are (a1 , a2 , a3 ) = (0.045, 0.05, 0.30) | The baseline values compared with the Basel numbers in the measurement map (G4 to G6). |
| BCVW-Q14 | 13 | r = 0.02, µ = 0.04, µL = 0.03, ρ = 0.12, γ = 0.02, together with σ = 0.08, σL = 0.03, c = 0.20, κ = 0.01, κ′ = 0.02 | The market, liability and cost parameters have no regulatory counterpart and are calibrated at this baseline (G8 to G11). |
| BCVW-Q15 | 19 | we treat the π ≤ 1 case as the main banking interpretation and the unrestricted case as a mathematical benchmark | The authors' main banking case forbids borrowing to hold the risky asset. Under it the liquidity branch of the cap does not bind (BCVW-D8). |
| BCVW-Q16 | 20 | the direct ceiling π ≤ 1 is tighter than the LCR constraint | In their parameter range the no-borrowing ceiling lies below the liquidity branch, `BCVW.ceiling_tighter_than_lcr` for every $a_2\le(1-a_3)y$. |
| BCVW-Q17 | 20 | the LCR constraint is nonbinding within the parameter range considered | M3's surplus limit, which needs the liquidity branch to bind deep in surplus, belongs to their unrestricted benchmark. |
| BCVW-Q18 | 17 | the LCR is everywhere weaker than the direct leverage ceiling and is therefore redundant | The same point for the value comparison of their Table 1. |
| BCVW-Q19 | 12 | dividends are paid at an upper barrier, while recapitalization, when optimal, is triggered at the distress boundary and jumps to a post-issuance target | The thresholds of Theorem 3.1. The trigger is the exogenous distress boundary, not an interior threshold. |
| BCVW-Q20 | 13 | These values constitute an illustrative parameterization rather than an empirical calibration | No parameter is set from data or read off the policy. |
| BCVW-Q21 | 13 | c is their shock correlation | The liability shock is correlated with the risky return, as in the theory paper's A1. |
| BCVW-Q22 | 13 | The regulatory constraints do not create the threshold structure from scratch | The cap moves the thresholds and does not make them; the thresholds come from the costs. |

## Lean results

| Name | Statement |
|---|---|
| `BCVW.piBar_mul_y` | $\bar\pi(y)\,y=u(y)$, the capped exposure of `CapGeometry`, for $a_1,a_3,y>0$. |
| `BCVW.piBar_at_one` | $\bar\pi(1)=0$ when $a_3>0$ and $a_2<1$. |
| `BCVW.diffusion_vanishes_at_one` | $\sigma^2(1,0)=0$. |
| `BCVW.drift_at_one` | With $\mu_L=\gamma+r_L$, the drift at $y=1$, $\pi=0$ is $r-r_L$. |
| `BCVW.sig2_decomp` | $\sigma^2(y,\pi)=(\pi\sigma y+c\sigma_L(1-y))^2+(1-c^2)\sigma_L^2(1-y)^2$. |
| `BCVW.sig2_nondegenerate` | $c^2<1$, $\sigma_L\ne0$, $y\ne1$ imply $\sigma^2(y,\pi)>0$. |
| `BCVW.switching_point_baseline` | $\lvert\hat y-1.168\rvert<0.0005$ at $(a_1,a_2,a_3)=(0.045,0.05,0.30)$. |
| `BCVW.piBar_tendsto` | $\bar\pi(y)\to1/\max(a_1,a_3)$ as $y\to\infty$, for $a_1,a_3>0$. |
| `BCVW.u_concave` | $u$ is concave on $\mathbb R$. |
| `BCVW.lcr_branch_ge_one` | $0<a_3$, $0<y$, $a_2\le(1-a_3)y$ imply $1\le\frac1{a_3}(1-a_2/y)$: the liquidity branch of $\bar\pi$ is at least one. |
| `BCVW.ceiling_tighter_than_lcr` | Under the same conditions $\min\{\frac1{a_3}(1-a_2/y),1\}=1$: with no borrowing the ceiling, not the liquidity branch, binds. |
| `BCVW.control_ceiling_needs_small_runoff` | At $a_3=1/2$, $a_2=y=1$ the liquidity branch is $0<1$. |
| `BCVW.control_nondegenerate_needs_abs_c_lt_one` | At $c=1$, $\sigma=\sigma_L=1$, $y=2$, $\pi=1/2$ the diffusion is $0$. |
| `BCVW.control_limit_needs_a1_le_a3` | At $a_1=1/2>a_3=1/4$ the limit $1/\max(a_1,a_3)=2$ is not $1/a_3=4$. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| BCVW-D1 | p. 15 | Switching point $\hat y=(a_3-a_1a_2)/(a_3-a_1)$. | The fraction is garbled in the text layer. The reading gives the printed value 1.168 at the baseline (`BCVW.switching_point_baseline`). |
| BCVW-D2 | p. 10 | $\lim_{y\to\infty}\bar\pi(y)=1/\max(a_1,a_3)=:1/\bar a$. | Garbled in the text layer. Assumption 3.1 defines $\bar a=\max(a_1,a_3)$, and p. 17 gives $1/a_3$ at the baseline, where $a_3>a_1$ (`BCVW.piBar_tendsto`). |
| BCVW-D3 | p. 10 | Diffusion coefficient $\pi^2\sigma^2y^2+2\pi c\sigma\sigma_Ly(1-y)+\sigma_L^2(1-y)^2$. | Exponents garbled in the text layer. The reading is the quadratic variation of the SDE of Prop. 2.1. |
| BCVW-D4 | p. 32 | $\sigma_Y^2=(\pi\sigma y+c\sigma_L(1-y))^2+(1-c^2)\sigma_L^2(1-y)^2\ge(1-c^2)\sigma_L^2(y-1)^2$. | The bound is an identity plus a dropped square. Strict positivity needs $c^2<1$, $\sigma_L\ne0$ and $y\ne1$ (`BCVW.sig2_nondegenerate` and its control). |
| BCVW-D5 | pp. 6, 10 | The paper's $K$ (Prop. 3.1) is a constant in an upper bound, its $\kappa$ a dilution cost and its $\kappa'$ the proportional cost. | PROOFS_v2 uses $K$ for a fixed issuance cost and $\kappa$ for the proportional cost. No symbol is carried over from the paper. |
| BCVW-D6 | p. 17 | $\lim\bar\pi=1/a_3$ holds when $a_3\ge a_1$. | p. 17 states it without the condition. It holds at the baseline. The general limit is BCVW-D2. |
| BCVW-D7 | p. 34 | $u(y)=\min\{(y-1)/a_1,(y-a_2)/a_3\}$ is concave in $y$. | The formula is garbled in the text layer. The reading is $\bar\pi(y)\,y$ (`BCVW.piBar_mul_y`, `BCVW.u_concave`). |
| BCVW-D8 | pp. 17, 19-20 | With no borrowing ($\pi\le1$) the liquidity branch never binds once $a_2\le(1-a_3)y$, which holds on $y\ge1$ for every $a_2\le1-a_3$, so for all Basel-like parameters. | BCVW-Q15 to BCVW-Q18 state it for their grid; `BCVW.ceiling_tighter_than_lcr` proves it in general. |
