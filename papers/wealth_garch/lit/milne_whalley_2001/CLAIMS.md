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
