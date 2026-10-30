# Claims: Milne and Whalley (2001)

Bib key `milnewhalley2001`. Version read: "Bank Capital Regulation and Incentives for Risk-Taking", December 2001, a revision of Bank of England Working Paper 90 (1999); 52 PDF pages, printed pages 1 to 46 (PDF page = printed page + 1). Read in full. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| MW-Q1 | 1 | Forward looking banks with sufficient franchise value maintain a buffer of capital in excess of the regulatory minimum. | A buffer above the minimum is optimal when issuance is costly and audits are random (mechanisms 2 and 4). |
| MW-Q2 | 3 | What matters instead is the possibility of future capitalisation falling below minimum required levels and inducing a costly regulatory intervention or market response. | The buffer guards against the supervisor's and the market's response (ledger A9, A7). |
| MW-Q3 | 9 | There are fixed costs to the issue of new equity capital implying that banks do not recapitalise following each disturbance to net worth. | Issuance costs, mechanism 2. |
| MW-Q4 | 23 | always results in an increase in desired capital | Result 12: a higher fixed issue cost or audit frequency raises the target. |
| MW-Q5 | 23 | an alteration of the minimum capital requirement has no long run impact on either the buffer of free capital | Result 13: raising the minimum shifts the target one for one, as in results_mechanisms.md, mechanism 4. |
| MW-Q6 | 42 | so the closed form solution represents an upper bound on the capital buffer. | The buffer when the bank never recapitalises bounds the buffer when it sometimes does (A11). |
| MW-Q7 | 42 | which is the special case analysed by Milne and Robertson (1996). | Continuous audit with no recapitalisation, (A12), whose buffer is Peura and Keppo's u0 (MW-D1). |
| MW-Q8 | 25 | Lowering ρ increases the maximum level of C | A lower cost of equity relative to deposits raises the target. |
| MW-Q9 | 24 | will have negative cash flow one year in twenty, approximately the average observed performance of OECD banks. | Their calibration of earnings risk, in units of mean earnings. |
| MW-Q10 | 9 | The bank capital decision is then akin to an (s, S) inventory problem. | The two frictions in one bank, the audit below the minimum and the fixed cost of issue, give an issue point and a target. |
| MW-Q11 | 9 | desired bank capital holdings are a function of cash-flow uncertainty, franchise value, audit frequency, recapitalisation costs, and cost of equity relative to deposits | The cost of falling below the minimum (audit frequency) and the fixed cost of issue are joint determinants of the buffer. |
| MW-Q12 | 23 | A shift in the same direction would be produced by an increase of q. | Result 12: more frequent audit moves the policy the same way as a higher fixed cost of issue, raising desired capital and the state below which an audited bank liquidates rather than recapitalises. |
| MW-Q13 | 18 | optimal policy is always to extremise the degree of cash flow uncertainty | Result 6: the bank picks the smaller of two assumed volatilities where $V_{CC}<0$ and the larger where $V_{CC}>0$, since bearing risk earns it nothing. The exposure rule of the theory paper's cap term reduces to this when the risk premium is zero (MW-D3). |
| MW-Q14 | 18 | a bank with less than the minimum required capital and hence under immediate threat of regulatory intervention is risk loving | Result 5: below the minimum, which the bank reaches only because the audit is random, $V_{CC}>0$. The theory paper's $V''(x_L^+)>0$ of M4 is the same convexity, just above the trigger rather than below the minimum (MW-D3). |
| MW-Q15 | 19 | A bank never chooses voluntary recapitalisation or voluntary liquidation. | Result 8: the bank issues or liquidates only after an audit finds it below the minimum. The (s, S) structure of MW-Q10 has a target and no chosen issue point, so the model has no recapitalisation trigger in the sense of the theory paper's M2 (MW-D4). |
| MW-Q16 | 9 | that regulators engage in a Poisson audit | The audit arrives at a constant rate, q in their equation (3). Below the minimum their equation (3), p. 15, adds q times the value of being forced to issue or close less the value of waiting (MW-D4). |
| MW-Q17 | 11 | The alternative to assuming a linear bank objective is a risk-averse bank utility function | The two ways the bank literature writes the objective, by their account. They take the first. |
| MW-Q18 | 11 | Such a risk-averse utility specification is usually justified as reflecting divergence of interest between managers and shareholders. | The reason they report for a concave bank utility. |
| MW-Q19 | 11 | that the introduction of regulatory capital requirements may induce banks to shift their portfolio composition towards riskier assets | Their account of Koehn and Santomero (1980) and Kim and Santomero (1988), papers the theory paper has not read. |
| MW-Q20 | 11 | we do find induced risk-aversion when capital is less than the desired level | With a linear objective the frictions induce the aversion to risk. |

## Lean results

| Name | Statement |
|---|---|
| `MilneWhalley.buffer_eq_u0` | For roots $p>0>m$: $\frac{1}{p-m}\ln(m^2/p^2)=\frac{2}{p-m}\ln(-m/p)$, so (A12) is Peura and Keppo's $u_0$. |
| `MilneWhalley.buffer_shift_invariant` | Raising target and minimum by the same $d$ leaves the buffer $C^*-\tilde C$ unchanged (Result 13). |
| `MilneWhalley.control_buffer_needs_square` | Dropping the square ($\ln(-m/p)$ for $\ln(m^2/p^2)$) changes the number: $2\ln2\neq\ln2$. |
| `MilneWhalley.control_shift_needs_same_d` | Shifting the target alone changes the buffer. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| MW-D1 | pp. 13 to 23, 41 to 42 | With continuous audit and no recapitalisation their model is the non-issuer case of Peura and Keppo: the same dynamics of capital, dividends at a barrier and failure at the minimum, and the buffer (A12) equals $u_0$ (`buffer_eq_u0`). Random audit and costly issuance can only lower the buffer from there (MW-Q6, Result 12 with $q=\infty$ as the limit). So the mechanisms of this paper cannot produce buffers larger than the non-issuer rows of results_reward.md and results_mechanisms.md. | MW-Q4, MW-Q6, MW-Q7. |
| MW-D2 | p. 25 | The target rises as the cost of equity relative to deposits ρ falls; they use ρ = 5% and report experimenting with 3% to 10%. | MW-Q8. |
| MW-D3 | pp. 17 to 18, 41 | Their variance control is bang-bang in the sign of $V_{CC}$ between two assumed values $\sigma_1<\sigma_2$, with no return to risk (Result 6, proof on p. 41, "The variance of bank earnings effects shareholder value only through its impact on the terms in $V_{CC}$"). In the theory paper the exposure $\pi$ sets drift and variance together under the regulatory cap of A2, with premium $\mu-r>0$, so the maximiser is interior where $V''<0$ and at an end of $[0,u(x)/x]$ where $V''\ge0$. With $\mu=r$ the first-order term vanishes and the rule is theirs, with $\pi=0$ for $\sigma_1$ and the cap for $\sigma_2$. The nesting is of the exposure rule only. Their state has no liability shock, their bank sits below the minimum because the audit is random, and the theory paper's bank issues at the trigger instead, so its convex region (M4) is just above the trigger and bounded by the cap. | MW-Q13, MW-Q14. |
| MW-D4 | pp. 14 to 15, 19 | Their equation (3) holds the theory paper's waiting and dividend terms above the minimum, with the dividend rate entering linearly and the volatility chosen from two values. Below the minimum it adds $q[\max(\max_{\Delta C}\{V(C+\Delta C)-\Delta C-\chi\},0)-V(C)]$, a forced issue or closure at the audit's rate. In the theory paper's equation that is a payoff term depending on $V$, not a choice of $\Lambda$, and it differs from the voluntary impulse term, which the bank never uses in their model (MW-Q15). If an audit instead charged the shareholders a fine $f$ per unit of shortfall, the compensator of the audit's Poisson process gives an expected cost per unit of time of $qf(R-x)^+$, which is $\Lambda$ with $\lambda_S-1=qf$. The thresholds would then identify the product $qf$, not the audit rate and the fine apart. | MW-Q15, MW-Q16. |
