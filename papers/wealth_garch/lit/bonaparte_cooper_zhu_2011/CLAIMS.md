# Claims: Bonaparte, Cooper and Zhu (2011)

Bib key `bonaparte2011`. Version read: NBER Working Paper 16957, April 2011; 27 PDF pages, printed page = PDF page - 2. Read in full, with Appendix 6. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| BCZ-Q1 | 1 | The model includes the choice of two assets: one riskless without adjustment costs and a second risky asset with adjustment costs. | Model structure: the cost applies only to stocks. |
| BCZ-Q2 | 2 | We consider the implications of an opportunity cost of asset trading and model it as a fraction of (stochastic) labor income. | The form of ψ: an income-proportional cost per adjustment. |
| BCZ-Q3 | 2 | However, in the presence of non-convex adjustment costs, as in our model, the standard Euler equation does not hold even for stockholders. | Why Euler-equation estimates of the EIS are misread. |
| BCZ-Q4 | 4 | is meant to capture the total time cost of making an optimal choice about stock holdings | ψ is a time cost of deciding. |
| BCZ-Q5 | 4 | This cost occurs whenever a household make additions or reductions to stock accounts | ψ is paid per adjustment, in either direction. |
| BCZ-Q6 | 6 | This moment is particularly informative about the opportunity cost of stock trading, one of the key elements in our model. | The adjustment rate is the moment for ψ. |
| BCZ-Q7 | 7 | The existing literature has found that a large coefficient of relative risk aversion is required in order to induce households to hold bonds due to the equity premium. | Context for the stock-share moment, which links γ and ψ. |
| BCZ-Q8 | 8 | This table is also informative about identification as it indicates which moments are particularly sensitive to variations in each of the parameters. | Table 7, the elasticity matrix, is the only identification device. |
| BCZ-Q9 | 8 | In the model with adjustment costs, there is no direct link between the inverse of the EIS and preference parameters. | The bias or misreading claim. |
| BCZ-Q10 | 8 | As noted earlier, the median ratio of wealth to income is particularly sensitive to the discount factor. | The wealth/income ratio identifies β. |
| BCZ-Q11 | 9 | There is no additional estimation involved here, just simulations with different parameterizations. | The robustness rows of Table 2 are not re-estimations. |
| BCZ-Q12 | 9 | But, keep in mind that the weighting matrix assumes that covariances are all zero. | Stated limitation of the SMM. |
| BCZ-Q13 | 12 | there is a sizable inaction region for stock adjustment for all three income states | The policy has an inaction region. |
| BCZ-Q14 | 12 | in the low income state, the inaction range is narrower and the critical values of current stockholdings defining the inaction region are lower. | The thresholds exist and shift with income. Described only qualitatively. |
| BCZ-Q15 | 12 | the opportunity cost of trading is lower when income is low. | Why the band narrows: ψ is scaled by income. |

## Lean results

| Name | Statement |
|---|---|
| `BonaparteCooper.timeCost_fixed_in_size` | The time cost of an adjustment, income times $(1-\psi)$, is the same at every trade size. |
| `BonaparteCooper.timeCost_rises_in_income` | It rises with income for $\psi<1$, which is why the inaction band narrows when income is low. |
| `BonaparteCooper.control_no_cost_at_one` | At $\psi=1$ the time cost is zero. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| BCZ-D1 | p. 4, eq. (3) | The adjustment cost is a fixed cost per adjustment scaled by current income, not by trade size, paid in either direction (`timeCost_fixed_in_size`). | BCZ-Q2, BCZ-Q5. |
| BCZ-D2 | pp. 6-9 | Two preference parameters and the adjustment cost are estimated jointly by over-identified simulated method of moments on four moments with a diagonal weighting matrix. | BCZ-Q6, BCZ-Q10, BCZ-Q12. |
| BCZ-D3 | p. 8, Table 7 | The only identification device is a table of elasticities of the moments to the parameters, which the paper calls informative about identification and does not analyse for rank or locality. | BCZ-Q8. |
| BCZ-D4 | p. 24, Table 7 | Read off the table, the adjustment cost and risk aversion move three of the four moments the same way and only the wealth-to-income ratio in opposite ways. The comparison and the full column rank of the matrix are my computation from the printed entries, not the paper's. | Table 7. |
| BCZ-D5 | pp. 11-12 | The policy has an explicit inaction band in stockholdings with income-dependent critical values, described qualitatively after estimation; the thresholds are not used for identification and carry no values. | BCZ-Q13, BCZ-Q14. |
| BCZ-D6 | p. 8 | With adjustment costs there is no direct link between the inverse of the elasticity of intertemporal substitution and preference parameters, which is the paper's version of the bias finding. | BCZ-Q9. |
