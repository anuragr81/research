# Claims: Egami and Oryu (version of 8 November 2014)

Bib key `egami2014`. Version read: working paper, 'This version: November 8, 2014', a later version of arXiv 1311.3019; 24 PDF pages, printed page = PDF page. It prints no journal or arXiv identifier. Read in full. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| EO-Q1 | 1 | and finds an optimal leverage ratio that triggers a PCA | The object solved is a leverage-ratio trigger for prompt corrective action. |
| EO-Q2 | 1 | In other words, one can see the cost of rescuing a bank which is | Bank size ("too big to fail") is central to the cost side. |
| EO-Q3 | 2 | and compute the optimal threshold level that triggers | The trigger is chosen to minimise total cost. |
| EO-Q4 | 3 | the bank does not have enough creditworthiness to raise capital and hence taxpayers | No private equity issuance; recapitalisation is publicly funded (answer to (b)). |
| EO-Q5 | 4 | should be determined by the regulator and we call it the PCA trigger level | It is the regulator's problem; the regulator chooses the trigger. |
| EO-Q6 | 5 | Let us emphasize that this value is large when the size of the bank is large | The cash-infusion cost scales with bank size; it is not a fixed cost. |
| EO-Q7 | 5 | the running cost will be incurred in proportion to the duration of PCA being in place | The running cost is duration-based at constant rate α, not graded in the shortfall. |
| EO-Q8 | 5 | becomes greater as the size of the bank becomes larger | The failure (social) cost is proportional to bank size. |
| EO-Q9 | 5 | It should be noted that the computation is numerical and is for a specific model. | The optimal trigger is found numerically; results are not proved in general. |
| EO-Q10 | 12 | the PCA may be applied too early so that the running cost to be incurred would become large | The trade-off: an early trigger raises running cost; a late one raises the infusion and penalty. |
| EO-Q11 | 12 | With greater asset volatility, the total cost becomes smaller | Comparative statics in PCA-regime volatility (Figure 2 caption). |
| EO-Q12 | 13 | while the conclusion may become different with different sets of parameters | Comparative statics are example-specific (limitation). |
| EO-Q13 | 14 | Hence the existence of jumps has significant impact on the total cost | Jump versus no-jump comparison (b∗ 0.441 versus 0.459). |
| EO-Q14 | 16 | This assumption seems to be natural since the administration cost of banks under the PCA provisions depends on the size of the bank. | Running cost is scaled by asset size in the calibration; it is administrative. |
| EO-Q15 | 17 | Hence a PCA should be (or should have been) implemented very early before the bank becomes too large. | 2006-07 high-volatility calibration gives the earliest possible trigger (b∗ = a). |

## Lean results

| Name | Statement |
|---|---|
| `EgamiOryu.argmin_free_of_size` | With a cost proportional to bank size, $C(x,b')=c\,G(b')$ with $c>0$, the minimiser in the trigger does not depend on size (their Remark 4.1). |
| `EgamiOryu.control_zero_scale_ties` | At zero scale every trigger ties, so size is what the minimiser is free of, not what selects it. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| EO-D1 | pp. 2, 4-5, 16 | The bank is a passive balance sheet. The regulator chooses the trigger and bears the cost: the injection, an administration cost and a social cost of failure. There is no bank objective, no dividend choice and no issuance choice. | EO-Q4, EO-Q5, EO-Q14. |
| EO-D2 | pp. 4-5 | The running cost is a constant rate over the whole period of the intervention, including time after the injection, so it is tied to being under supervision, not to the size of a shortfall below a requirement. | EO-Q7; eq. (2.3). |
| EO-D3 | p. 4 | In the theory paper's units a larger optimal trigger in this paper means a later intervention and a lower assets-to-liabilities trigger. | Leverage at the trigger is $e^{b'-b}$. |
| EO-D4 | pp. 5, 11, 16 | There is no fixed cost of intervention and no proportional issuance cost, the recapitalisation target is exogenous, and the cost parameters are set once, so the trigger is never varied in any cost. | EO-Q4, EO-Q9. |
| EO-D5 | pp. 11-12 | A trade-off in the text suggests the running cost would raise the trigger and the social cost of failure would lower it. The paper never varies either; this reading is mine. | EO-Q10. |
| EO-D6 | p. 16 | Only the asset drift, volatility and initial value are estimated, by likelihood on equity prices; the trigger is an output and is never matched to an observed intervention. | Section 4.2. |
| EO-D7 | pp. 18-19 | The trigger is independent of initial size when the running cost scales with assets (`argmin_free_of_size`). | Remark 4.1. |
