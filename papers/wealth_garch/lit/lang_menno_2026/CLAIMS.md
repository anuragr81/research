# Claims: Lang and Menno (2026)

Bib key `langmenno2026`. Version read: ECB Working Paper 3188, 2026; 51 PDF pages, printed page = PDF page - 1. Read in full; figures for captions only. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| LM-Q1 | 5 | We restrict the model to two periods, similar to an overlapping generations banking sector model | Two-period structure, no infinite-horizon barrier policy |
| LM-Q2 | 9 | This feature resembles the Basel III minimum capital requirement framework, where a bank is put into resolution when it fails to meet the minimum capital requirement | Below R^min: liquidation/closure, not a graduated cost |
| LM-Q3 | 9 | These reduced form stigma costs are a convenient way to model that under the Basel III regulatory framework breaching the CBR entails consequences | The stigma cost is reduced form and stands in for the consequences of a breach |
| LM-Q4 | 10 | This assumption seems reasonable, as the signal that a bank sends when breaching the CBR would be worse if it was the only bank breaching buffer requirements. | Individual stigma above aggregate stigma |
| LM-Q5 | 11 | this comes at the expense of needing to maintain a higher capital ratio which is costly due to the assumption that equity funding is more expensive than debt funding | The cost of capital is an equity funding premium, not an issuance cost |
| LM-Q6 | 12 | In principle bank heterogeneity or mixed strategy equilibria could also be studied, but we leave this for future research. | Stated limitation |
| LM-Q7 | 12 | stigma costs at 3 bps and 25 bps respectively, which ensures that banks fulfil the CBR and deleverage when hit with negative shocks | Calibrated stigma values and how they were chosen |
| LM-Q8 | 13 | it should be noted that the specific calibration of stigma costs has no impact on the results that are derived in the remainder of the paper. | The results are threshold conditions and do not depend on the ξ values |
| LM-Q9 | 13 | Ad-hoc: low but sufficient to induce banks to fulfill the CBR | Table 1 source for ξ: ad hoc, not estimated |
| LM-Q10 | 14 | Banks can always go to the market and raise new equity from shareholders in case of need, e.g. when losses are incurred. | Frictionless issuance in the benchmark (no issuance cost) |
| LM-Q11 | 17 | For our benchmark model calibration (1% default cost) banks maintain voluntary capital buffers of 1pp to 2pp, and these voluntary capital buffers are pro-cyclical | Voluntary buffers above R^min and their size |
| LM-Q12 | 18 | Banks do not maintain voluntary capital buffers above the CBR | Prop. 3/6: the bank sits exactly at the CBR, with no buffer above it |
| LM-Q13 | 19 | banks will fulfill the CBR whenever aggregate stigma costs are higher than the net cost of fulfilling the CBR. | Stigma works as a threshold/switch, not a smooth comparative static |
| LM-Q14 | 26 | the higher the deleveraging pressure for non-using banks is, the higher the individual stigma cost needs to be | Comparative statics of the required stigma in shock size |
| LM-Q15 | 31 | structural capital buffers and releasable capital buffers could be a promising avenue for future research. | Stated future research (capital stack composition) |

## Lean results

| Name | Statement |
|---|---|
| `LangMenno.stigma_flat_below` | The stigma charge $\xi\,1\{CR<R\}$ is the same at every shortfall. |
| `LangMenno.stigma_zero_at_or_above` | It is zero at or above the requirement. |
| `LangMenno.graduated_grows_below` | For $\lambda_S>1$ the theory paper's penalty $(\lambda_S-1)(R-x)^+$ grows with the shortfall. |
| `LangMenno.control_graduated_flat_at_one` | At $\lambda_S=1$ the graduated penalty is flat (zero). |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| LM-D1 | p. 10, eq. (1) | The stigma cost is $\xi\,1\{CR<R^{cbr}\}L$, a step in the capital ratio scaled by loans; it does not depend on the shortfall. | LM-Q3; every equation with $\xi$ multiplies it by an indicator and $L'$. |
| LM-D2 | p. 8, fn. 4 | Equity issuance is either free or ruled out by $d\ge0$; there is no fixed or proportional issuance cost, and the paper's $\kappa$ is an operating cost. | LM-Q5, LM-Q10. |
| LM-D3 | pp. 18, 28 | The response to $\xi$ is a switch: below the threshold of eq. (8) the bank uses the buffer, above it the bank sits exactly at the CBR with no buffer above it. | LM-Q12, LM-Q13. |
| LM-D4 | pp. 12-13 | $\xi$ is set ad hoc to make the CBR bind and is not matched to any moment. | LM-Q7 to LM-Q9. |
| LM-D5 | pp. 16-18 | Voluntary buffers above the minimum come only from the liquidation cost $\Delta L'$, a one-off cost proportional to loans rather than a running cost of the shortfall. | LM-Q11. |
| LM-D6 | pp. 5, 8 | Two periods and one aggregate shock rule out a dividend barrier, a recapitalisation trigger and any statement on the variance of capital. | LM-Q1, LM-Q6. |
