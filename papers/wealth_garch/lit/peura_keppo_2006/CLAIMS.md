# Claims: Peura and Keppo (2006)

Bib key `peura2006`. Version read: the published article, *Journal of Business* 79(4), pp. 2163-2201, pinned to the JSTOR copy. Evidence [F]: read in full; each quotation is found verbatim in the text layer of its page.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| PK-Q1 | 2165 | We are not aware of a similar calibration exercise performed in the existing banking literature. | A bank capital model calibrated to bank data and compared with observed capital ratios, claimed first here. |
| PK-Q2 | 2165 | Our model builds on the basic continuous-time model of a capital-constrained firm presented in Milne and Robertson (1996). | The paper's own lineage: an extension of a standard model. |
| PK-Q3 | 2165 | Our main modeling innovation is to impose a delay on capital issues. | Its novelty is the issue delay, not the threshold structure. |
| PK-Q4 | 2166 | We imagine a bank whose portfolio size, measured by its regulatory risk-weighted assets | Size is risk-weighted assets, the denominator of the state. |
| PK-Q5 | 2167 | with a cost that is a fixed proportion K of the size of the bank (as measured by R). | The fixed issue cost is proportional to size, as $K$ per unit of liabilities in M4 to M6. |
| PK-Q6 | 2168 | We assume that the corrective action from violation of the minimum capital requirement will be liquidation | The penalty at the requirement is extreme. |
| PK-Q7 | 2176 | As in the general case, the dividend barrier is also the level up to which the capital ratio is replenished each time a new capital issue is implemented. | Every issue ends at the dividend barrier: a single target (PK-D2). |
| PK-Q8 | 2177 | The sole presence of the fixed cost does not generate a positive capital issue barrier and will not result in a positive probability of liquidation. | The delay, not the fixed cost, creates the issue barrier. |
| PK-Q9 | 2179 | The owners optimally issue new equity in larger quantities but less frequently when the cost of issuance is high | Issue size rises with the fixed cost, as the gap of M5. |
| PK-Q10 | 2166 | Our model can replicate observed capital ratios exactly if volatility is made an implied parameter, analogous to how the Black-Scholes model is used in practice. | The implied-volatility exercise, close to the sigma backtest of `TODO.md`. |
| PK-Q11 | 2182 | We use annual Compustat data on actual capital and accounting returns for a sample of U.S. national commercial banks | The calibration data (PK-D3). |
| PK-Q12 | 2188 | The model falls somewhat short of explaining the level of capital held by banks. | The baseline underpredicts the capital level, as our corner test (G4). |
| PK-Q13 | 2188 | It is possible that our model underestimates actual capital because we have | Their diagnosis begins: a misspecified minimum requirement. |
| PK-Q14 | 2191 | Finally, we have calibrated our model to bank-level accounting return data and tested its ability to replicate actual bank capital ratios. | The test design. |
| PK-Q15 | 2191 | the model explains some 40% of the variation in capital ratios across banks and 70% of the average level of the capital buffer. | Their best fit, on the subsample with above-average loan loss provisions. |
| PK-Q16 | 2190 | The banking literature so far lacks good attempts to explain observed bank capital holding behavior with optimizing models of capital choice. | The state of the literature in 2006, by their account. |
| PK-Q17 | 2192 | Our calibration exercise has been somewhat arbitrary. | Their own limit. |
| PK-Q18 | 2192 | A model in which the penalty gradually increases as the capital ratio deteriorates would therefore be more realistic. | The extension they call for, a graduated penalty below the requirement. |
| PK-Q19 | 2193 | bank capital requirements apply to an accounting measure of capital; hence, the dynamics of accounting measures could be all that matter. | Accounting volatility is the relevant sigma for a capital model (PK-D3). |
| PK-Q20 | 2174 | We have done the optimization with the Solver add-in in Microsoft Excel. | The source of small numerical differences in the replication (`replication/peura_keppo_2006/results_stage1.md`). |
| PK-Q21 | 2179 | the expected size of a new issue (which is well approximated by the difference between the dividend barrier and the capital issue barrier) increases with the capital raising cost K. | The approximation behind Table 2's issue-size column (PK-D5). |
| PK-Q22 | 2184 | For D and K, we identify common estimates across banks such that their implied capital raising cost and frequency are of plausible magnitude. | The authors pin the delay and the fixed cost from two moments derived from the thresholds, issue cost and frequency. A precedent for C4's logic, by calibration rather than a local identification theorem, and with no preference parameter. |
| PK-Q23 | 2184 | is driven by K, and hence we may choose K rather independently from D, so as to yield a desired proportional cost of capital issuance | The trigger-to-target gap is driven by the fixed cost, which is how they separate $K$ from $D$. |
| PK-Q24 | 2184 | K is the fixed cost of capital issue, u1 is the optimal capital issue barrier, u2 is the optimal dividend barrier | The columns of Table 2. |
| PK-Q25 | 2184 | .08 .10 .9 2.4 1.5 6 8 | Table 2 row $(\Delta,K)=(0.08,0.10)$: $u_1=0.9$, $u_2=2.4$. |
| PK-Q26 | 2184 | .08 .25 .8 2.7 1.8 12 18 | Row $(0.08,0.25)$: $u_1=0.8$, $u_2=2.7$. |
| PK-Q27 | 2184 | .5 .10 1.6 3.2 1.6 6 11 | Row $(0.5,0.10)$: $u_1=1.6$, $u_2=3.2$. |
| PK-Q28 | 2192 | imposed common values for other parameters (mainly guided by high level economic intuition) | The parameters other than the return moments are imposed, not identified from the bank's policy. The theory paper's M3 identifies two of them from the thresholds. |
| PK-Q29 | 2192 | It is the task of future research | The sentence continues "to test alternative model specifications", across a line-end hyphen in the text layer. The authors leave the specification open. |
| PK-Q30 | 2192 | the penalty from regulatory capital violation is extreme | Liquidation at the minimum (PK-Q6) is, by their account, too severe a penalty. The sentence opens "In light of the FDICIA stipulations, our assumption concerning", across a line-end hyphen in the text layer. |
| PK-Q31 | 2192 | Whether these omissions matter reduces to the issue of which constituency provides the binding constraint on bank capitalization. | The question they leave: whose cost binds the bank's capital, the regulator's or the market's. The theory paper identifies the weight of the shortfall penalty and not its constituency (Appendix D of the theory paper). |
| PK-Q32 | 2184 | .5 1.00 .9 3.7 2.8 27 122 | Table 2 row $(\Delta,K)=(0.5,1.00)$: $u_1=0.9$, $u_2=3.7$, the base of the high-cost finite differences. |
| PK-Q33 | 2184 | 1 1.00 .6 3.8 3.2 24 291 | Row $(1,1.00)$: $u_1=0.6$, $u_2=3.8$. The delay lowers the issue barrier at high $K$. |
| PK-Q34 | 2184 | .5 .50 1.1 3.5 2.4 17 57 | Row $(0.5,0.50)$: $u_1=1.1$, $u_2=3.5$. |
| PK-Q35 | 2190 | For large banks (average assets over 40 billion US$), the average capital buffer of 3.8% implies a volatility of 1.06%. | Their implied volatility for large banks, against an estimate of 0.82% (p. 2191): a multiple of about 1.3 (`PeuraKeppo.implied_multiple_large`). The idea of an implied volatility is theirs; the enhancement paper's E2 measures it again on common data. |
| PK-Q36 | 2190 | For small banks, the average capital buffer of 5.3% implies a volatility of 1.19%. | Against an estimate of 1.06%, a multiple of about 1.1 (`PeuraKeppo.implied_multiple_small`). |
| PK-Q37 | 2190 | suggesting that larger banks hold less capital than small banks, even after controlling for possible differences in their | The size gradient beyond volatility; they assign it to capital-market imperfections, letting only large banks recapitalise. |
| PK-Q38 | 2190 | implying that the required volatility adjustment is, in fact, higher for large banks | Once only large banks may recapitalise. |
| PK-Q39 | 2170 | Now the following characterization of optimum can be established using standard arguments | Proposition 2: the value function satisfies the variational inequalities (13i) to (13v), the master equation the enhancement paper states (its M6). |
| PK-Q40 | 2175 | the optimal policy and the value function reduce to those of the Milne and Robertson (1996) model without the capital issue option. | Case 1, no issuance: the value function is their (20) with the barrier $u_0$ of (17), the enhanced model's case. |
| PK-Q41 | 2195 | coincides with the value function (8iv). This completes the proof of proposition 3. | Their verification that the constructed function is the value function; the enhancement paper cites it for optimality rather than proving it. |

## Lean results

| Name | Statement |
|---|---|
| `PeuraKeppo.char_eq` | $d_{1\pm}=\big(-\mu\pm\sqrt{\mu^2+2\rho\sigma^2}\big)/\sigma^2$ solve $\tfrac12\sigma^2d^2+\mu d-\rho=0$, the characteristic equation of (A1). |
| `PeuraKeppo.dplus_pos` | $d_{1+}>0$ for $\sigma\ne0$, $\rho>0$. |
| `PeuraKeppo.dminus_neg` | $d_{1-}<0$ for $\sigma\ne0$, $\rho>0$. |
| `PeuraKeppo.smooth_paste` | $a_1,a_2$ of (A4) give $f_1'(u_2)=1$ and $f_1''(u_2)=0$. |
| `PeuraKeppo.coeff_signs` | $a_1>0>a_2$ when $d_{1+}>0>d_{1-}$, as (A4) states. |
| `PeuraKeppo.value_at_barrier` | Smooth pasting and the characteristic equation give $f_1(u_2)=a_1+a_2=\mu/\rho$, (A5). |
| `PeuraKeppo.no_issue_barrier` | $u_0=\frac{2}{d_{1+}-d_{1-}}\log(-d_{1-}/d_{1+})$ solves $f_1(0;u_0)=0$, (17) and (A11). |
| `PeuraKeppo.control_dplus_needs_positive_rho` | At $\rho=0$, $\mu=\sigma=1$, $d_{1+}=0$. |
| `PeuraKeppo.control_paste_needs_distinct_roots` | With $d_{1+}=d_{1-}$ the coefficients fail smooth pasting. |
| `PeuraKeppo.table2_signs` | From the printed Table 2 at $(\Delta,K)=(0.08,0.10)$: the dividend barrier rises in $\Delta$ and in $K$, the issue barrier rises in $\Delta$ and falls in $K$. |
| `PeuraKeppo.table2_det_neg` | The finite-difference Jacobian of $(u_2,u_1)$ in $(\Delta,K)$ there is negative, by `Identification.det_neg_of_opposite`, the sign pattern of M6. |
| `PeuraKeppo.control_delay_lowers_trigger_at_high_K` | At $K=1.00$ a longer delay lowers $u_1$ (0.9 to 0.6), so the pattern is local. |
| `PeuraKeppo.table2_det_value` | The determinant of $(u_2,u_1)$ in $(\Delta,K)$ from Table 2's printed differences at $(0.08,0.10)$ is $-29/100$. |
| `PeuraKeppo.table2_det_high_K` | The same determinant at $(0.5,1.00)$ is $+1/25$; the sign changes across the table. |
| `PeuraKeppo.implied_multiple_large` | $1.29<1.06/0.82<1.30$. |
| `PeuraKeppo.implied_multiple_small` | $1.12<1.19/1.06<1.13$. |
| `PeuraKeppo.control_implied_not_three` | $1.06/0.82<3$: their multiple is far below the enhancement paper's three. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| PK-D1 | the copy pinned | Since 2026-10-12 the JSTOR copy with a text layer (offset 2161). The earlier page-image copy, in which PDF page 24 held pp. 2186 and 2187, was checked through OCR. | Every quotation matches the JSTOR text layer on the same printed page. |
| PK-D2 | pp. 2176, 2194 | Each capital issue brings the buffer exactly to the dividend barrier $u_2$. | PK-Q7, and the supremum of (11) is attained at $s^*=u_2-X_\Delta$ (p. 2194). The model predicts exact targeting, as M15's premise does here. |
| PK-D3 | pp. 2181 to 2182 | $\mu$ and $\sigma$ are the mean and standard deviation of accounting return on risk-weighted assets, net of the risk-free return on capital. | The paper defines them through the accounting identity (24) to (28). In this baseline $\sigma$ is a book quantity and is measurable from Call Report data. |
| PK-D4 | references | The copy read ends at p. 2201. | BCVW cites the article as pp. 2163-2202. A final page, if any, was not supplied. |
| PK-D5 | p. 2184, Table 2 | The column E[s] is $u_2-u_1$, the approximation of PK-Q21, not the conditional expectation its note describes. | Replication stage 1: the column matches $u_2-u_1$ in 10 of 12 rows and the conditional expectation in 1. |
| PK-D6 | p. 2184, Table 2 | The column $E(t_i-t_{i-1})$ is the expected time from $u_2$ down to $u_1$, without the delay its note includes. | Replication stage 1: 9 of 12 rows match without the delay, 5 of 12 with it. |
| PK-D7 | p. 2184, Table 2 | Signs of the threshold responses are read from finite differences of printed values at one decimal; the opposite-direction pattern holds at $K=0.10$ and $0.25$ and fails at $K=1.00$. | PK-Q25 to PK-Q27; `PeuraKeppo.control_delay_lowers_trigger_at_high_K`. The replication reproduces the table (`replication/peura_keppo_2006/results_stage1.md`). |
