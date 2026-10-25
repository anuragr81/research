# Claims: Glover, Johnson, Evatt and Cheng (2023)

Bib key `glover2023`. Version read: the preprint submitted to The European Journal of Finance, dated 1 December 2022; 41 PDF pages, printed page = PDF page. Read in full. The published version was not read. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| GJEC-Q1 | 6 | Should this condition be violated, but the bank remain solvent, the bank will be forced by the regulator to raise the appropriate amount of new equity | A breach of the requirement triggers forced recapitalisation, not a running penalty |
| GJEC-Q2 | 6 | the regulator will place the bank into resolution and the shareholder’s control will be removed. | At insolvency the consequence is closure (resolution) |
| GJEC-Q3 | 7 | In what follows we choose to remain agnostic on the relative importance of the regulator’s objectives since we will consider each banking metric separately | The regulator's problem is not solved; ω is swept exogenously |
| GJEC-Q4 | 10 | whenever the bank decides (or is forced) to raise new equity it will incur a cost | One issuance cost covers voluntary and forced issues |
| GJEC-Q5 | 10 | while a fixed cost can also be included in our modelling, it would only serve to increase the (already large) parameter space and computation time | No fixed issuance cost in the model |
| GJEC-Q6 | 11 | We assume that all agents are risk-neutral and discount cash flows at a constant rate | Risk-neutral valuation |
| GJEC-Q7 | 17 | it is well known that these optimal controls will be of bang-bang (all or nothing) type | Solution structure: bang-bang controls |
| GJEC-Q8 | 18 | which is consistent with the average underwriter spread for bank seasoned equity offerings between 1996 and 2007 | The issuance cost is calibrated externally, not estimated from policy |
| GJEC-Q9 | 19 | we try to match two key bank performance ratios, the bank’s net interest margin | Calibration moments are accounting ratios |
| GJEC-Q10 | 20 | Indeed, under minimum capital requirements, the central choice of a bank is how much to hold in excess of the minimum amount required (if any). | The buffer above the requirement is the focus |
| GJEC-Q11 | 22 | A higher capital adequacy ratio results in a higher optimal capital ratio, but a lower equity ‘buffer’ over that required by regulation. | Result 4: comparative static of the buffer in ω |
| GJEC-Q12 | 22 | still chooses to keep a small amount of additional equity, over and above that required, in order to avoid the dead-weight loss from the costly external financing | The buffer persists because forced issuance is costly |
| GJEC-Q13 | 31 | it is optimal for the bank to raise new equity the moment internal equity drops below the optimal financing line | Proportional cost gives barrier-type, non-lumpy issuance |
| GJEC-Q14 | 27 | Our model is not without limitations however, and the relaxation of some of our modelling assumptions would be the direction of interesting future research. | Stated limitations |
| GJEC-Q15 | 28 | This could be relaxed to consider possible risk-adjustment of these values in response to a bank’s leverage and lending decisions. | Future research: prices, including ψ, are fixed |

## Lean results

| Name | Statement |
|---|---|
| `GloverEtAl.forcedCost_below` | Below the requirement the forced issue costs $\psi_0(\omega L-E)$. |
| `GloverEtAl.forcedCost_above` | At or above it nothing is forced. |
| `GloverEtAl.breach_slope_is_issuance_cost` | The breach is priced at slope $\psi_0$, the same parameter as voluntary issuance. |
| `GloverEtAl.control_free_issuance_no_breach_cost` | With $\psi_0=0$ a breach costs nothing. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| GJEC-D1 | pp. 6, 10 | A breach forces immediate recapitalisation to $\omega L$, so the cost of being below is $\psi_0(\omega L-E)$, one-sided and linear but paid once, not a flow while below. | GJEC-Q1, GJEC-Q4. |
| GJEC-D2 | p. 10 | One parameter $\psi_0$ prices the breach and voluntary issuance, so the model cannot separate a cost of being below from a cost of issuing even in principle (`breach_slope_is_issuance_cost`). | GJEC-Q4, Lean. |
| GJEC-D3 | p. 10 | There is no fixed issuance cost; the authors say one would add only parameters and computation, without showing it. | GJEC-Q5. |
| GJEC-D4 | pp. 16-27 | The comparative statics are in regulatory ratios, loss frequency and deposit size, never in $\psi_0$, and nothing is reported on how the dividend or issuance thresholds move with a friction. | GJEC-Q3, GJEC-Q11. |
