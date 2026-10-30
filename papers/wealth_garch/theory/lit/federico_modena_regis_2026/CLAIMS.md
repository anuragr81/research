# Claims: Federico, Modena and Regis (2026)

Bib key `federico2026`. Version read: SAFE Working Paper No. 487, July 2026 ('This version: 14th July 2026'), EconStor copy; 67 PDF pages, printed page = PDF page - 2. Read in full. The supplementary material the paper mentions is not in the file. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| FMR-Q1 | 3 | the bank decides whether to default or, at a cost, to issue equity (and in what amount) when capital requirements become binding | The choice at the requirement is binary (default or costly issue), not a graduated penalty. |
| FMR-Q2 | 4 | In the presence of dividend restrictions, the optimal recapitalisation target falls below the payout threshold. | x*2 < x̃2 under the tax; the target separates from the barrier. |
| FMR-Q3 | 9 | The regulator requires the bank to hold sufficient equity to repay depositors in full if loans are liquidated at a fire-sale price | The form of the capital requirement (eq. 3). |
| FMR-Q4 | 10 | When this occurs, the manager must either liquidate the bank or recapitalize it by issuing new equity at a cost. | A hard floor: at the requirement, liquidation or impulse issuance. |
| FMR-Q5 | 14 | If the manager liquidates the firm, the shareholders receive the maximum between the capital buffer and zero | The liquidation payoff (Γ+), borne by shareholders. |
| FMR-Q6 | 19 | Remark 1 clarifies that, when taxes are excessively high in the bad state, the bank finds it optimal to postpone dividend payments until the good state materialises. | A ban is the limit case of the state-contingent tax. |
| FMR-Q7 | 24 | This is the value observed across US commercial banks, according to the NYU Stern database. | The calibration target for κ (price-to-book ≈ 1.04). |
| FMR-Q8 | 27 | Hence, dividend taxes reduce the bank’s willingness to issue equity when reserves hit the regulatory threshold | The tax lowers the bad-state injection target x*2. |
| FMR-Q9 | 29 | Second, higher dividend taxes slightly reduce the dispersion of reserves in the good state but significantly increase it in the bad state. | Effect on the variance of capital (S1, S2). |
| FMR-Q10 | 31 | We nevertheless interpret this tightening of the incentive-compatibility constraint as evidence that dividend restrictions may endogenously increase default risk. | The interaction of tax and issuance cost through κ̄. |
| FMR-Q11 | 33 | As intuition suggests, tighter (looser) capital requirements are associated with higher (lower) recapitalisation thresholds and lower (higher) bank valuations. | The comparative static of thresholds in Γ (numerical). |
| FMR-Q12 | 38 | One important limitation of our model is that loan exposure is fixed, preventing us from examining how dividend restrictions affect bank risk-taking. | Stated limitation; no exposure choice in the baseline. |
| FMR-Q13 | 42 | Higher recapitalisation costs in the bad state generate a similar effect, due to the precautionary motive to avoid costlier equity issuance | A higher κ2 shifts all thresholds and the target up (Table 8). |
| FMR-Q14 | 44 | Another limitation of our model is that the bank’s optimal responses perfectly anticipate the policy the regulator will enact in each possible state. | Stated limitation and future research. |
| FMR-Q15 | 57 | We only sketch the proof, as a rigorous argument would be unnecessarily technical. | Theorem 1 (verification) is not fully proved. |

## Lean results

| Name | Statement |
|---|---|
| `FedericoEtAl.banCutoff_rises_in_l` | The payout share $\lambda_2/(\lambda_2+\delta)$ at or below which the bad-state dividend is a ban rises with the probability of leaving the bad state. |
| `FedericoEtAl.banCutoff_falls_in_delta` | It falls with the manager's impatience. |
| `FedericoEtAl.liquidationPayoff_nonneg` | The shareholders' payoff on liquidation, the buffer floored at zero, is non-negative. |
| `FedericoEtAl.control_no_impatience_cutoff_is_one` | With no impatience the cutoff is one. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| FMR-D1 | pp. 9, 19 | The dividend restriction is a proportional payout tax, a ban in the limit, that depends on the macro state and not on reserves. It acts at the dividend barrier and is not a cost of being below a requirement; it reaches recapitalisation only through lower value. | FMR-Q6; eqs. (13), (15), (18). |
| FMR-D2 | pp. 10, 14 | The capital requirement is a hard floor where the bank issues at a fixed cost or is liquidated, and reserves never go below it. There is no cost that grows with a shortfall. | FMR-Q1, FMR-Q4, FMR-Q5. |
| FMR-D3 | pp. 12, 17-18 | The issuance trigger is fixed at the floor; only the target and the dividend barriers are chosen. The fixed cost $\kappa$ is per issue and there is no proportional cost. | FMR-Q2; eqs. (13), (29). |
| FMR-D4 | pp. 27, 30, 32, 36, 42 | Every comparative static of the thresholds, in the tax, the buffer, a counter-cyclical requirement and the bad-state issuance cost, is numerical at one calibration. The baseline fixed cost is never varied. | FMR-Q8, FMR-Q11, FMR-Q13. |
| FMR-D5 | pp. 27, 42 | The bad-state tax and the bad-state issuance cost both raise the bad-state dividend barrier and move the bad-state target in opposite directions. The paper draws no identification conclusion from this and the second is a single discrete change; the reading is mine. | Tables 2 and 8. |
| FMR-D6 | p. 24 | The only parameter matched to a moment is the fixed cost, to a price-to-book ratio of about 1.04; nothing is estimated and identification is not asked. | FMR-Q7. |
| FMR-D7 | pp. 29-30, 37 | The stationary variance of reserves is computed and changes with the tax and a counter-cyclical requirement. There is no exposure cap and no penalty parameter enters it. | FMR-Q9, FMR-Q12. |
