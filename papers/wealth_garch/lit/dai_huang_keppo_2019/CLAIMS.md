# Claims: Dai, Huang and Keppo (2019)

Bib key `daihuangkeppo2019`. Version read: chapter 2 and appendix of Huang's thesis, arXiv:2107.02242v1, not the journal's typeset article. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| DHK-Q1 | 130 | has been published on Journal of Economic Dynamics and Control | Chapter 2 is the JEDC article (DHK-D1). |
| DHK-Q2 | 27 | with the capital issuances, there is a delay and a fixed cost | The issue costs of Peura and Keppo, a delay and a fixed cost. |
| DHK-Q3 | 26 | We ignore debt volatility and debt financing | No liability shock, unlike the theory paper's bank. |
| DHK-Q4 | 45 | Parameter u2 is the dividend barrier | Column $u_2$ of Table 2.1. |
| DHK-Q5 | 76 | Table 2.1: Elasticities. | The table of threshold responses. |
| DHK-Q6 | 76 | ∆ 0.0808 −0.1444 0 −0.0146 | The delay raises $u_2$ and lowers $u_1$ (`DaiHuangKeppo.table21_delay_signs`). |
| DHK-Q7 | 76 | K 0.0017 −0.0543 0 −0.0004 | The fixed cost raises $u_2$ and lowers $u_1$, the signs of the theory paper's M1 and M2 in $K$ (`DaiHuangKeppo.table21_K_signs`). |
| DHK-Q8 | 76 | the average is over the values in Figure 2.7(iii) | The elasticities are averages; the grid is stated for $\sigma$ only (DHK-D2). |
| DHK-Q9 | 35 | c1 and c2 are the cost parameters for no liquidation and liquidation | The regulator's asymmetry $a=c_2/(c_1+c_2)$, a preference of the regulator, not of the shareholders. |
| DHK-Q10 | 71 | when the liquidation parameter a rises, then the liquidation barrier I falls | The regulator's asymmetry moves the barriers through the liquidation barrier, both the same way. |
| DHK-Q11 | 84 | are calibrated by fitting the partially observed value function | Parameters are fitted to equity values, not identified from the thresholds. |
| DHK-Q12 | 77 | These correspond to our model's cost parameter K = 0.20% | $K$ is set from the literature. |
| DHK-Q13 | 30 | take the minimum equity-to-debt ratio κ as a constraint | The shareholders choose the barriers given the regulator's ratio. |
| DHK-Q14 | 40 | Thus we skip it | Theorem 2.2.1(i) is not proved in the text. |

## Lean results

| Name | Statement |
|---|---|
| `DaiHuangKeppo.elasticity_det` | The 2×2 elasticity determinant is the Jacobian determinant times $t_1t_2/(y_1y_2)$. |
| `DaiHuangKeppo.jacobian_ne_zero_of_elasticity` | For non-zero barriers, a non-zero elasticity determinant gives a non-zero Jacobian determinant. |
| `DaiHuangKeppo.table21_K_signs` | $0<0.0017$ and $-0.0543<0$: $K$ raises $u_2$ and lowers $u_1$. |
| `DaiHuangKeppo.table21_delay_signs` | $0<0.0808$ and $-0.1444<0$: $\Delta$ raises $u_2$ and lowers $u_1$. |
| `DaiHuangKeppo.table21_det_neg` | The printed $(\Delta,K)$ block has determinant $0.0808\cdot(-0.0543)-0.0017\cdot(-0.1444)<0$. |
| `DaiHuangKeppo.control_proportional_columns_singular` | Columns proportional to each other give a zero determinant. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| DHK-D1 | p. 130 | The thesis chapter is read as the JEDC article. Only its pages are attributed. | DHK-Q1; the published version was not read. |
| DHK-D2 | p. 76 | The rows $\Delta$ and $K$ of Table 2.1 are averages over an unstated grid. Their signs are used, not their magnitudes, except for the determinant, which is evaluated as printed. | The averaging is stated for $\sigma$ only (DHK-Q8), and Figure 2.7 has no panel for $\Delta$ or $K$. |
| DHK-D3 | p. 67 | Prop. 2.2.9(i) for $a\le50\%$ reads as stated, $I(S)$ rises in $S$. | The proof says "decreases", contradicting the statement and the sign it derives. |
| DHK-D4 | pp. 46, 84, 85 | Full-information $\sigma$ is 3.45% (Table 2.3). | The text and the caption of Figure 2.3 say 3.11%. Not used by the theory paper. |
| DHK-D5 | p. 76 | Both $\Delta$ and $K$ move $u_1$ down, so the determinant is non-zero by magnitude, not by the opposite-direction pattern of the theory paper's M3. | DHK-Q6, DHK-Q7. |
