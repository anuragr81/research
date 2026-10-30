# Claims: Behn, Daminato and Salleo (2019)

Bib key `behn2019`. Version read: ECB Working Paper 2233, January 2019; 63 PDF pages, printed page = PDF page - 1. Read in full, figure pages as images. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| BDS-Q1 | 9 | shareholders incur a cost in case the bank violates either type of requirement | Breach cost is borne by shareholders (a) |
| BDS-Q2 | 10 | These costs are reminiscent of the bankruptcy cost in classical trade-off theory models | Breach cost framed as a bankruptcy-type cost, not a graduated one (a) |
| BDS-Q3 | 10 | can be motivated by the risk for shareholders to be bailed in, possible restrictions on dividend payments, or other supervisory measures | Interpretation of the breach cost: bail-in, dividend restrictions, supervisory action (a) |
| BDS-Q4 | 10 | These costs are meant to reflect the existence of direct transactional costs and indirect costs of raising equity | Motivation for the issuance cost (b) |
| BDS-Q5 | 10 | In line with pecking order theory, the cost does not apply if banks decide to accumulate capital from retained earnings. | Issuance cost applies to external equity only; retained earnings are free (b) |
| BDS-Q6 | 10 | both the costs of breaching minimum capital and liquidity requirements and the cost of raising fresh equity are not imposed ex ante but estimated within our model structure. | Both frictions are estimated jointly (c) |
| BDS-Q7 | 18 | Generally, these buffers can be expected to be larger the larger costs of breaching capital and liquidity requirements. | The only directional statement on breach cost and buffers; verbal, not derived (c) |
| BDS-Q8 | 23 | The costs of breaching the minimum capital and liquidity requirements are mainly identified by the extent of the observed voluntary buffers | Moment used to identify Ω and Ψ (c) |
| BDS-Q9 | 24 | lower (higher) costs of raising fresh equity in the market will induce a higher (lower) covariance between capital and asset growth | Moment used to identify the issuance cost (c) |
| BDS-Q10 | 25 | The cost of breaching regulatory capital or liquidity requirements are 46 and 28 percent of total capital, respectively. | Estimated size of the breach costs (a) |
| BDS-Q11 | 25 | The magnitude of the parameters is such that raising equity is relatively costly, so that banks make use of this measure only in exceptional circumstances | Estimated issuance cost is large; issuance is rare (b) |
| BDS-Q12 | 26 | less than one percent of the banks in our sample raise fresh equity in each period | Simulated issuance frequency (b) |
| BDS-Q13 | 30 | the amount by which they would have to cut back lending to achieve their target capital ratios is so large that it becomes preferable to raise external equity | Issuance chosen by banks nearest the minimum after a rise in requirements (c) |
| BDS-Q14 | 31 | An interpretation of this result is that a bank’s lending reaction likely depends on the extent to which it is willing or forced to recapitalize | Link between recapitalisation choice and lending (c) |
| BDS-Q15 | 31 | The change is slightly smaller than proportional, which can be explained by the lower cost that is associated with breaching the liquidity requirement | The one place a breach-cost level is used to explain a buffer response (c) |
| BDS-Q16 | 35 | Future research should focus on investigating the importance of additional sources of heterogeneity among banks that may affect the response to policy changes. | Stated future research |

## Lean results

| Name | Statement |
|---|---|
| `BehnEtAl.breach_flat_below` | The breach cost $\Omega\,1(CR<\theta)$ is the same at every shortfall. |
| `BehnEtAl.issueCost_no_fixed_part` | With $\Phi_2>0$ the issuance cost $\Phi_1a^{\Phi_2}$ is zero on a zero issue: no fixed part. |
| `BehnEtAl.control_zero_exponent_is_fixed` | At $\Phi_2=0$ the same form would be a pure fixed cost. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| BDS-D1 | pp. 10, 25, 57 | The breach cost is a step, $\Omega$ times an indicator, the same for a breach of 0.1 or 5 points; the text and Table 5 state it in percent of capital, the equation shows no scaling. | BDS-Q1, BDS-Q2, BDS-Q10. |
| BDS-D2 | pp. 10, 25 | The issuance cost $\Phi_1(-div)^{\Phi_2}$ has no constant term and $\Phi_2=1.04$, so it is nearly proportional with no fixed part. | BDS-Q4, BDS-Q5, BDS-Q11. |
| BDS-D3 | pp. 23-24 | The two frictions are separated by assigning moments, buffer size to the breach cost and capital-asset co-movement to the issuance cost, with no standard errors and no formal identification. | BDS-Q8, BDS-Q9. |
| BDS-D4 | pp. 18, 27 | No threshold policy and no comparative statics in either friction; the counterfactuals vary only the requirements and impairments. | BDS-Q7. |
| BDS-D5 | p. 57 | The only second moment targeted is the standard deviation of profits over assets; nothing on the variance of capital or the buffer. | Table 5, Panel B. |
