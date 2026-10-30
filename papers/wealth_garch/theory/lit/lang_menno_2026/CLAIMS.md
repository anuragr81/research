# Claims: Lang and Menno (2026)

Bib key `langmenno2026`. Version read: ECB Working Paper Series No 3188, 2026 (no month printed); 51 PDF pages, printed pages 1 to 49 on PDF pages 2 to 50 (PDF page = printed page + 1). Read in full twice on 25 October 2026, for the comparison (LM-Q1 to LM-Q12, LM-D1) and for the theory paper's Section 3.1 (LM-Q13 to LM-Q24, LM-D2 to LM-D7). Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| LM-Q1 | 1 | We prove that even very low stigma costs induce large buffer usability constraints | A breachable buffer requirement binds in practice. |
| LM-Q2 | 8 | For tractability, we restrict the model to 2 periods. | Their model is two-period, not a continuous-time barrier model. |
| LM-Q3 | 12 | There is just a single stochastic process for loan impairments that drives the dynamics of the model, i.e. there are no idiosyncratic bank shocks. | One common shock: no cross-section of banks. |
| LM-Q4 | 9 | Second, we add a CBR to the model, which is greater or equal to the minimum capital requirement | The buffer requirement sits above the minimum. |
| LM-Q5 | 12 | Rcbr = 0.125 so that it equals the minimum capital requirement plus the capital conservation buffer (CCoB). | Their floor is minimum plus conservation buffer, the construction of our 10.5% (euro-area minimum 10%). |
| LM-Q6 | 12 | To illustrate the analytical insights that we derive with concrete examples, we calibrate our model to match key features of euro area bank data. | Euro-area calibration for illustration; not fitted to observed buffers. |
| LM-Q7 | 18 | Banks do not maintain voluntary capital buffers above the CBR | In their equilibrium the capital ratio sits at the buffer requirement: it acts as the floor, and nothing lies far above it. |
| LM-Q8 | 17 | For our benchmark model calibration (1% default cost) banks maintain voluntary capital buffers of 1pp to 2pp, and these voluntary capital buffers are pro-cyclical, | Without the buffer requirement, voluntary buffers of 1 to 2 points above the minimum. |
| LM-Q9 | 14 | Without an equity issuance constraint and costly bank liquidation, there are no reasons for banks to maintain voluntary capital buffers. | Buffers come from costly failure and costly issuance, as in Milne and Whalley. |
| LM-Q10 | 12 | In principle bank heterogeneity or mixed strategy equilibria could also be studied, but we leave this for future research. | Heterogeneity across banks is left open. |
| LM-Q11 | 30 | We showed that very low aggregate stigma costs of 0.5-3 bps are sufficient to induce banks to fulfill the capital buffer requirement in normal times. | Their conclusion. |
| LM-Q12 | 7 | we are the first to explicitly model capital buffer requirements that banks are allowed to breach | Their novelty claim. |
| LM-Q13 | 5 | We restrict the model to two periods, similar to an overlapping generations banking sector model | Two-period structure, no infinite-horizon barrier policy |
| LM-Q14 | 9 | This feature resembles the Basel III minimum capital requirement framework, where a bank is put into resolution when it fails to meet the minimum capital requirement | Below R^min: liquidation/closure, not a graduated cost |
| LM-Q15 | 9 | These reduced form stigma costs are a convenient way to model that under the Basel III regulatory framework breaching the CBR entails consequences | The stigma cost is reduced form and stands in for the consequences of a breach |
| LM-Q16 | 10 | This assumption seems reasonable, as the signal that a bank sends when breaching the CBR would be worse if it was the only bank breaching buffer requirements. | Individual stigma above aggregate stigma |
| LM-Q17 | 11 | this comes at the expense of needing to maintain a higher capital ratio which is costly due to the assumption that equity funding is more expensive than debt funding | The cost of capital is an equity funding premium, not an issuance cost |
| LM-Q18 | 12 | stigma costs at 3 bps and 25 bps respectively, which ensures that banks fulfil the CBR and deleverage when hit with negative shocks | Calibrated stigma values and how they were chosen |
| LM-Q19 | 13 | it should be noted that the specific calibration of stigma costs has no impact on the results that are derived in the remainder of the paper. | The results are threshold conditions and do not depend on the ξ values |
| LM-Q20 | 13 | Ad-hoc: low but sufficient to induce banks to fulfill the CBR | Table 1 source for ξ: ad hoc, not estimated |
| LM-Q21 | 14 | Banks can always go to the market and raise new equity from shareholders in case of need, e.g. when losses are incurred. | Frictionless issuance in the benchmark (no issuance cost) |
| LM-Q22 | 19 | banks will fulfill the CBR whenever aggregate stigma costs are higher than the net cost of fulfilling the CBR. | Stigma works as a threshold/switch, not a smooth comparative static |
| LM-Q23 | 26 | the higher the deleveraging pressure for non-using banks is, the higher the individual stigma cost needs to be | Comparative statics of the required stigma in shock size |
| LM-Q24 | 31 | structural capital buffers and releasable capital buffers could be a promising avenue for future research. | Stated future research (capital stack composition) |

## Lean results

| Name | Statement |
|---|---|
| `LangMenno.cbr_is_min_plus_ccob` | $0.10+0.025=0.125$ (LM-Q5). |
| `LangMenno.us_floor_same_construction` | $0.08+0.025=0.105$: the same construction on the US 8% minimum gives our R. |
| `LangMenno.control_cbr_needs_buffer` | Without the buffer the floor stays at the minimum. |
| `LangMenno.control_us_floor_not_ecb` | The US and euro-area floors differ. |
| `LangMenno.stigma_flat_below` | The stigma charge $\xi\,1\{CR<R\}$ is the same at every shortfall. |
| `LangMenno.stigma_zero_at_or_above` | It is zero at or above the requirement. |
| `LangMenno.graduated_grows_below` | For $\lambda_S>1$ the theory paper's penalty $(\lambda_S-1)(R-x)^+$ grows with the shortfall. |
| `LangMenno.control_graduated_flat_at_one` | At $\lambda_S=1$ the graduated penalty is flat (zero). |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| LM-D1 | pp. 8, 12, 18 | A two-period representative-bank model with a common shock; the buffer requirement binds in equilibrium and no bank holds capital far above it; calibrated for illustration, never compared with observed buffers. | LM-Q2, LM-Q3, LM-Q6, LM-Q7, LM-Q10. |
| LM-D2 | p. 10, eq. (1) | The stigma cost is $\xi\,1\{CR<R^{cbr}\}L$, a step in the capital ratio scaled by loans; it does not depend on the shortfall. | LM-Q15; every equation with $\xi$ multiplies it by an indicator and $L'$. |
| LM-D3 | p. 8, fn. 4 | Equity issuance is either free or ruled out by $d\ge0$; there is no fixed or proportional issuance cost, and the paper's $\kappa$ is an operating cost. | LM-Q17, LM-Q21. |
| LM-D4 | pp. 18, 28 | The response to $\xi$ is a switch: below the threshold of eq. (8) the bank uses the buffer, above it the bank sits exactly at the CBR with no buffer above it. | LM-Q7, LM-Q22. |
| LM-D5 | pp. 12-13 | $\xi$ is set ad hoc to make the CBR bind and is not matched to any moment. | LM-Q18 to LM-Q20. |
| LM-D6 | pp. 16-18 | Voluntary buffers above the minimum come only from the liquidation cost $\Delta L'$, a one-off cost proportional to loans rather than a running cost of the shortfall. | LM-Q8. |
| LM-D7 | pp. 5, 8 | Two periods and one aggregate shock rule out a dividend barrier, a recapitalisation trigger and any statement on the variance of capital. | LM-Q13, LM-Q10. |
