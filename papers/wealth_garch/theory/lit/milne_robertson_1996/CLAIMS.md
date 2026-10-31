# Claims: Milne and Robertson (1996)

Bib key `milnerobertson1996`. Version read: the authors' draft of February 1994, "Firm Behaviour under the Threat of Liquidation: Implications for Output, Investment, and Business Cycle Transmission", marked "Not for quotation"; 42 PDF pages, printed page = PDF page - 2. Read in full, the appendix included. The quotations below are internal notes and are not for publication (author, 30 October 2026); the manuscript cites the JEDC article, which was not read. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| MR-Q1 | 6 | This implies that the firm cannot raise new equity finance. | No issuance: the base model of Peura and Keppo and of Milne and Whalley. |
| MR-Q2 | 6 | allows it to make a one-off issue of new equity capital | Their footnote 4: the alternative they set aside, an issue after costly monitoring at the trigger. |
| MR-Q3 | 7 | Instead of invoking liquidation it would be possible to work with a model without liquidation | Their footnote 5: a penalty rate on debt below the threshold in place of liquidation. |
| MR-Q4 | 7 | This penalty interest rate would induce a similar aversion to indebtedness as appears in our model. | A running charge on the shortfall would do the work of liquidation, by their account. They do not solve that model. |
| MR-Q5 | 8 | Since (3) is linear in the control variable d, the optimal control is bang-bang. | Dividends at a barrier, from linearity in the dividend rate. |
| MR-Q6 | 15 | the degree of local risk aversion increases as the firm moves closer to liquidation | With liquidation and no issuance the value is concave below the barrier and aversion to risk rises as cash falls (Proposition 3). |
| MR-Q7 | 16 | Any increase in the variance of the disturbance to the cash balances of the firm is therefore costly | In continuous time with liquidation alone, more variance always costs the firm. |
| MR-Q8 | 18 | operating to the left of our liquidation trigger | Convexity appears only past the liquidation threshold, their savings and loans example. |

## Lean results

| Name | Statement |
|---|---|
| `MilneRobertson.no_dividend_below_barrier` | Where $V'>1$, $d(1-V')\le0$ for $d\ge0$: no dividend below the barrier. |
| `MilneRobertson.prudence_sign` | With $\rho>r$, $V'/V''<0$ and $V/V'>0$, $W'/W<0$ (Theorem 2, sign corrected, MR-D3). |
| `MilneRobertson.control_sign_needs_positive_ratio` | With $V/V'<0$ as their text writes, the expression can be positive. |
| `MilneRobertson.control_dividend_above_barrier` | Where $V'<1$ a positive dividend beats none. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| MR-D1 | title page | The draft is read in place of the JEDC article (vol. 20, 1996, pp. 1427-1449), whose title drops the subtitle. Results are attributed to the draft; that the article states them is not checked. | Only the draft was supplied. |
| MR-D2 | pp. 10 to 11, 32 | The barrier conditions are $V'(x^*)=1$ and $V''(x^*)=0$, those M1 uses at the dividend barrier. With $r=0$, $V=A e^{m_1x}+Be^{m_2x}$ and $V(0)=0$ give $x^*=\ln(m_2^2/m_1^2)/(m_1-m_2)$, the buffer of Milne and Whalley's (A12) and Peura and Keppo's $u_0$. At their printed $\mu=0.14$, $\sigma^2=0.005$, $\rho=0.05$ this is $0.178721$, against $0.17872$ printed (computed 30 October 2026). | Eqs. (5) to (7), boundary conditions (i) to (iii), notes to the figures. |
| MR-D3 | p. 38 | Theorem 2's proof says $V/V_x<0$; with $V>0$ and $V_x>0$ it is positive, which is what the inequality $W_x/W<0$ needs. | `MilneRobertson.prudence_sign` and its control. |
