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

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| PK-D1 | the copy pinned | Since 2026-10-12 the JSTOR copy with a text layer (offset 2161). The earlier page-image copy, in which PDF page 24 held pp. 2186 and 2187, was checked through OCR. | Every quotation matches the JSTOR text layer on the same printed page. |
| PK-D2 | pp. 2176, 2194 | Each capital issue brings the buffer exactly to the dividend barrier $u_2$. | PK-Q7, and the supremum of (11) is attained at $s^*=u_2-X_\Delta$ (p. 2194). The model predicts exact targeting, as M15's premise does here. |
| PK-D3 | pp. 2181 to 2182 | $\mu$ and $\sigma$ are the mean and standard deviation of accounting return on risk-weighted assets, net of the risk-free return on capital. | The paper defines them through the accounting identity (24) to (28). In this baseline $\sigma$ is a book quantity and is measurable from Call Report data. |
| PK-D4 | references | The copy read ends at p. 2201. | BCVW cites the article as pp. 2163-2202. A final page, if any, was not supplied. |
| PK-D5 | p. 2184, Table 2 | The column E[s] is $u_2-u_1$, the approximation of PK-Q21, not the conditional expectation its note describes. | Replication stage 1: the column matches $u_2-u_1$ in 10 of 12 rows and the conditional expectation in 1. |
| PK-D6 | p. 2184, Table 2 | The column $E(t_i-t_{i-1})$ is the expected time from $u_2$ down to $u_1$, without the delay its note includes. | Replication stage 1: 9 of 12 rows match without the delay, 5 of 12 with it. |
