# Claims: Décamps, Mariotti, Rochet and Villeneuve (2011)

Bib key `decamps2011`. Version read: Journal of Finance 66(5), October 2011, pp. 1501-1544; 44 PDF pages (PDF page = printed page - 1500). Read in full; the Internet Appendix is not in the file and was not read. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| DMRV-Q1 | 1503 | Because issuing shares involves a fixed cost, equity adjustments take place in lumpy and infrequent issues | The fixed cost gives lumpy issuance |
| DMRV-Q2 | 1504 | If the former were positive but the latter were zero, the firm would maintain the level of its cash reserves at zero | Issuance costs create the cash buffer; interaction with agency costs |
| DMRV-Q3 | 1504 | Its solutions are almost explicit, and its parameters can be easily estimated, as in the case of issuance costs and of the volatility of cash | Parameters come from outside, not identified from policy |
| DMRV-Q4 | 1507 | The firm is held by a diffuse basis of risk-neutral shareholders and holds no debt | Owners are risk-neutral; all-equity firm |
| DMRV-Q5 | 1508 | In practice, this cost reflects taxes and expert or registration fees that are largely independent of the size of the issue. | Fixed cost per issue, independent of issue size |
| DMRV-Q6 | 1509 | assume that they effectively reduce the rate of return on internally held liquid assets | The agency cost is a running cost on cash held (surplus side) |
| DMRV-Q7 | 1510 | The firm is liquidated if it cannot meet its short-term operating costs by drawing cash from its reserves or by issuing new shares. | The only consequence at the threshold is liquidation, not a graduated cost |
| DMRV-Q8 | 1514 | Because of the costs associated with new share issues, it is natural to expect that these should be delayed as much as possible. | Issuance trigger at zero cash |
| DMRV-Q9 | 1520 | as issuance costs increase, the firm must accumulate more liquidity before distributing dividends | Corollary 1: dividend barrier rises with issuance costs |
| DMRV-Q10 | 1521 | of an increase in the proportional issuance cost is a priori ambiguous | Effect of p on the issue target is ambiguous |
| DMRV-Q11 | 1522 | As for issuance costs, we follow Gomes (2001) in setting | Issuance costs taken from prior estimates |
| DMRV-Q12 | 1529 | The volatility of stock returns is decreasing in the stock price. | Corollary 3: asymmetric return volatility |
| DMRV-Q13 | 1530 | the asymmetric volatility phenomenon can be attributed neither to the volatility feedback effect, as investors are risk-neutral, nor to financial leverage, as our firm is 100% equity financed. | Source of the asymmetry; risk neutrality |
| DMRV-Q14 | 1531 | The volatility of stock returns is increasing in issuance costs. | Corollary 5: issuance costs raise volatility |
| DMRV-Q15 | 1533 | One promising extension would be to allow the profitability of the firm to evolve randomly. | Stated future research |

## Lean results

| Name | Statement |
|---|---|
| `DecampsEtAl.unitCost_falls` | With $f>0$ the cost per unit raised, $p(m+f)/m$, falls with the issue size $m$, which is why issues are lumpy. |
| `DecampsEtAl.control_no_fixed_cost_flat` | With $f=0$ the cost per unit is the same at every size. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| DMRV-D1 | pp. 1514-1516 | The policy has a trigger, a target and a barrier, but the trigger is pinned at zero cash because nothing penalises low cash short of running out. The theory paper's trigger is interior and moves with $\lambda_S$. | DMRV-Q7, DMRV-Q8. |
| DMRV-D2 | pp. 1520-1521, fn. 21 | The fixed cost $f$ raises both the barrier and the target; the proportional cost $p$ raises the barrier and lowers the target near $p=1$. The opposite signs on the target are my inference, not the paper's; the paper never asks about identification. | DMRV-Q9, DMRV-Q10. |
| DMRV-D3 | pp. 1528-1531 | The asymmetric volatility is the volatility of stock returns, $\sigma V'/V$, from the concavity of value in cash. It is not the variance of the cash buffer, and there is no cap and no asymmetry parameter. | DMRV-Q12 to DMRV-Q14. |
| DMRV-D4 | p. 1509 | The running cost $\lambda$ is a cost of carry on cash held, the surplus side; its mirror image below a level is absent. | DMRV-Q6. |
