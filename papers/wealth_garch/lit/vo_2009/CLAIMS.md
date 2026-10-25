# Claims: Vo (2009)

Bib key `vo2009`. Version read: Norges Bank Working Paper 2009/28, December 2009, EconStor copy; 27 PDF pages, printed pages 1 to 24 (PDF page = printed page + 3). Read in full; Greek letters checked against page images. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| VO-Q1 | 1 | regulatory supervision should be realized in the spirit of gradual intervention and the book-value of capital is used as information to trigger intervention. | The graduated (PCA-style) intervention result, triggered by book capital. |
| VO-Q2 | 1 | Banks with high capital are not subject to any restrictions. Dividend distribution is prohibited in banks with intermediate level of capital. | Dividend restriction in the intermediate region; no restriction above E\*. |
| VO-Q3 | 2 | The banker has to bear a positive cost for each additional unit of capital injected into the bank. | Issuance cost is proportional, per unit, borne by the banker. |
| VO-Q4 | 2 | there exists some exogenous bound for the amount of capital per unit of time the banker | Injection-rate cap K (a flow bound, not a fixed cost). |
| VO-Q5 | 3 | we relax the limited liability of the agent (the banker) and so, allow the principal (the DIC) to require the banker to inject money during their relationship. | Recapitalisation as the paper's departure from the dynamic moral-hazard literature. |
| VO-Q6 | 6 | Positive transfer corresponds to some dividend paid to the banker while negative transfer is consistent with a capital injection by the banker. | Dividends and recapitalisation are the two signs of one transfer process. |
| VO-Q7 | 6 | To capture the fact that in general, recapitalization is costly, we assume that the banker has to bear a cost | Introduction of the per-unit recapitalisation cost α. |
| VO-Q8 | 13 | region is reduced but also, for prudent reason, it takes more time for the bank to start distributing dividend. | Higher α shrinks the recapitalisation region and raises the dividend threshold. |
| VO-Q9 | 13 | The intuition is that since recapitalization is costless, it is optimal for the DIC to use this punishment device as soon as possible. | Limit α → 0: recapitalisation and dividend thresholds coincide. |
| VO-Q10 | 15 | restrictions on banks become more stringent the less capitalized banks are | Graduated, monotone intervention in capital. |
| VO-Q11 | 15 | dividend distributions are suspended but banks are still allowed to continue in normal operation, no capital restoration is required. | Middle region: a buffer with a dividend ban and no recapitalisation. |
| VO-Q12 | 15 | This premium reduction can be seen as a subsidy from the DIC to the undercapitalized banks in replenishing capital. | In the implementation the DIC refunds the recapitalisation cost via the premium. |
| VO-Q13 | 16 | Our liquidation policy exhibits the same property in the sense that it claims to liquidate the banks as soon as their capital is wiped out. | The terminal cost is closure at zero capital, not a graduated cost. |
| VO-Q14 | 16 | A rigorous analyse of the choice between two measures requires a richer setting than ours. | Stated limitation (book vs market value). |

## Lean results

| Name | Statement |
|---|---|
| `Vo.slopes_ordered` | For $\alpha>0$, $-1<-1/(1+\alpha)$: the recapitalisation slope lies above the dividend slope. |
| `Vo.control_slopes_merge_at_zero` | At $\alpha=0$ the two slopes coincide, so the thresholds merge (Corollary 1). |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| VO-D1 | pp. 10-11 | The graduated intervention comes from the curvature of the insurer's value under moral hazard, at $F'=-1$ and $F'=-1/(1+\alpha)$, not from a running cost of the shortfall. | No penalty term in eqs. (1) to (5), p. 7. |
| VO-D2 | pp. 6, 11 | The recapitalisation friction is a proportional cost $\alpha$ plus a cap $K$ on the injection rate. With no fixed cost, injection is a flow at the cap over a region, not an impulse from a trigger to a target. Vo's $K$ is the rate cap, not a fixed cost. | VO-Q3, VO-Q4, VO-Q7. |
| VO-D3 | p. 13 | The recapitalisation threshold falls and the dividend threshold rises in $\alpha$, the directions the theory paper reports for $K$; the monotonicity is asserted without a displayed proof. | VO-Q8, VO-Q9. |
| VO-D4 | pp. 14-15 | Capital is a relabelling of continuation utility, so the thresholds are contract thresholds rather than exogenous requirements. | VO-Q10, VO-Q11. |
| VO-D5 | whole paper | No parameter plays the role of $\lambda_S$, and nothing separates two frictions from the thresholds. | Only $\alpha$ has comparative statics. |
