# Claims: Bonaparte and Cooper (2009)

Bib key `bonaparte2009`. Version read: NBER Working Paper 15227, August 2009 (the cached file is named 2010 and is the August 2009 version); 20 PDF pages, printed page = PDF page - 2. Read in full; it has no appendix. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| BC-Q1 | 1 | These adjustment costs imply that inferences drawn about household risk aversion and the elasticity of intertemporal substitution are biased | The headline bias claim. |
| BC-Q2 | 1 | household risk aversion is lower compared to other estimates and it is not equal to the inverse of the elasticity of intertemporal substitution | The direction of the bias: risk aversion is lower and is not 1/EIS. |
| BC-Q3 | 3 | The nature and extent of the bias introduced on estimates of the intertemporal elasticity of substitution is not clear a priori. | The direction of the EIS bias is not signed theoretically. |
| BC-Q4 | 3 | Though the inaction seems to naturally pull the estimated EIS lower, when adjustment occurs it can be more sensitive to interest rate movements. | Two opposing effects of inaction on the measured EIS. |
| BC-Q5 | 3 | We obtain some trading costs directly from the data and provide estimates of both a preference parameter and a direct opportunity cost of trading. | Joint estimation of one preference parameter and one adjustment cost. |
| BC-Q6 | 5 | We call this a time cost of trade to distinguish it from the trading costs paid to a broker. | ψ is a time cost, separate from the broker's C(·). |
| BC-Q7 | 5 | One interpretation of this cost is lost income due to time spent on portfolio adjustment. | The form of ψ: an opportunity cost of time, paid out of labour income. |
| BC-Q8 | 6 | This allows us to identify the utility costs of trade separately from actual trading costs. | The separation of the two costs comes from estimating C(·) outside the model. |
| BC-Q9 | 10 | This moment is very informative about the cost of trade | The adjustment rate is the moment for ψ. |
| BC-Q10 | 10 | In simulation, we see that this regression coefficient is sensitive to variations in | α₁ is the moment for γ (footnote 17). No direction is given. |
| BC-Q11 | 11 | This is a just-identified exercise and an identity matrix was used as a weighting matrix. | Two moments, two parameters, identity weighting matrix. |
| BC-Q12 | 12 | As a consequence the adjustment rate is much lower. | Lowering ψ (a higher time cost) lowers the adjustment rate. |
| BC-Q13 | 12 | Importantly, one cannot infer the EIS nor the degree of risk aversion directly from the inverse | The bias claim: 1/α₁ is not risk aversion. |
| BC-Q14 | 12 | Rather, it reflects the inaction of households in their portfolio adjustment as well as the presence of non-participants in aggregated data. | Why α₁ is near zero: inaction plus non-participation. |
| BC-Q15 | 17 | Adding these additional assets, as well as durable goods such as housing, remains for future work. | Stated future work. |

## Lean results

| Name | Statement |
|---|---|
| `BonaparteCooper.timeCost_fixed_in_size` | The time cost of an adjustment, income times $(1-\psi)$, is the same at every trade size. |
| `BonaparteCooper.timeCost_rises_in_income` | It rises with income for $\psi<1$. |
| `BonaparteCooper.control_no_cost_at_one` | At $\psi=1$ the time cost is zero. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| BC-D1 | p. 5, eq. (4) | The adjustment cost is fixed per adjustment and scaled by labour income, not by trade size (`timeCost_fixed_in_size`). | BC-Q6, BC-Q7. |
| BC-D2 | pp. 5-6, 15-16 | The policy is an inaction rule in principle but the paper never computes or plots its thresholds; its only description is a probit. | BC-Q9. |
| BC-D3 | pp. 10-11 | One preference parameter and one adjustment cost are estimated jointly from two moments by just-identified simulated method of moments with an identity weighting matrix; the adjustment rate is the moment for the cost. | BC-Q9, BC-Q10, BC-Q11. |
| BC-D4 | p. 12 | A higher time cost lowers the adjustment rate; the paper gives no sign for the preference parameter, and I infer that both lower the second moment. | BC-Q12. |
| BC-D5 | pp. 1, 3, 12 | The bias finding is that ignoring the adjustment cost biases the inference of risk aversion and the elasticity of intertemporal substitution from the inverse of the regression coefficient; it is a bias claim, not an identification from thresholds. | BC-Q1, BC-Q2, BC-Q13. |
| BC-D6 | pp. 13-14 | The robustness text and Table 3 disagree on some values, among them 0.8348 and 0.901. | Text and Table 3. |
