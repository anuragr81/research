# Claims: Engle and Siriwardane (2018)

Bib key `engle2018`. Version read: the published RFS article. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| ES-Q1 | 449 | Volatility asymmetry is mostly explained by exposure to the aggregate market, not a mechanical leverage effect. | The observed asymmetry is assigned to market exposure rather than to a structural mechanism (L3, C2). |
| ES-Q2 | 452 | Our main finding is that mechanical leverage drives almost none of the observed equity volatility asymmetry for the firms in our sample. | As ES-Q1. |
| ES-Q3 | 473 | we say that a firm displays volatility asymmetry when negative equity returns predict higher future volatility, relative to positive equity returns of the same magnitude | Their asymmetry is a news response, not a ratio of state-dependent variances (ES-D4). |
| ES-Q4 | 474 | which we interpret to mean that leverage explains only 3% of volatility asymmetry | The correlation-measure share. |
| ES-Q5 | 478 | only about 14% of volatility asymmetry in equity returns comes from leverage | The GJR-measure share. |
| ES-Q6 | 476 | This finding supports the risk premium explanation of French, Schwert, and Stambaugh (1987) for volatility asymmetry. | The residual is assigned to risk premia, not to preference. |
| ES-Q7 | 459 | which is the exact relationship implied by structural models of credit | Equity variance is the squared multiplier times asset variance, eq. (8). |

## Lean results

| Name | Statement |
|---|---|
| `EngleSiriwardane.gjr_news_asymmetry` | For $r>0$, the GJR variance after news $-r$ exceeds that after $+r$ by $\gamma r^2$ (eq. 16). |
| `EngleSiriwardane.equity_variance` | $(LM\cdot r_A)^2=LM^2r_A^2$, the algebra of eq. (8). |
| `EngleSiriwardane.phi_zero_nests_gjr` | $(LM^{BSM})^0=1$. |
| `EngleSiriwardane.phi_zero_equity_is_asset` | At $\phi=0$, $LM^2h_A=h_A$. |
| `EngleSiriwardane.share_corr` | $1-0.97=0.03$. |
| `EngleSiriwardane.share_gjr` | $1-0.86=0.14$. |
| `EngleSiriwardane.share_market` | $1-0.17=0.83$. |
| `EngleSiriwardane.control_symmetric_without_gamma` | At $\gamma=0$ the news response is symmetric. |
| `EngleSiriwardane.control_market_share_is_not_eighty` | $1-0.17\ne0.80$, so the printed "≈ 80%" is a rounding (ES-D1). |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| ES-D1 | p. 476 | Market exposure accounts for $1-0.17=0.83$ of the asymmetry. The paper writes "around 100−17 ≈ 80%". | The arithmetic gives 83%. Only the paper's rounded phrase is attributed, and the computed share is 0.83 (`EngleSiriwardane.share_market` and its control). |
| ES-D2 | p. 486, C.2.4 | The asset variance recursion is eq. (15) of p. 476, $h_{A,t+1}=\omega+\alpha r_{A,t}^2+\gamma r_{A,t}^2\mathbf 1_{r_{A,t}<0}+\beta h_{A,t}$. | The C.2.4 display prints $\gamma\alpha r^2_{A,t-1}$ and the lag $t-1$. Eqs. (7), (9) and (15) agree with the reading. |
| ES-D3 | p. 459, eq. 9 | The indicator is $\mathbf 1_{r_{A,t-1}<0}$. | The text layer drops "< 0". Eqs. (7) and (15) carry it. |
| ES-D4 | pp. 473 to 478 | Their two asymmetry measures are responses of variance to the sign of news. Neither is a ratio of variances across regions of a state variable. | No equivalence with the variance ratio $\lambda_V^4$ of this bundle is attributed to the paper. |
