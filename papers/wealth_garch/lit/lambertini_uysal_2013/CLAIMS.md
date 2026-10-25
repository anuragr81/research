# Claims: Lambertini and Uysal (2013)

Bib key `lambertini2013`. Version read: EPFL working paper, October 2013, "Preliminary and Incomplete"; 47 PDF pages, printed page = PDF page. Read in full; the earlier draft of Lambertini and Uysal (2014). Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| LU13-Q1 | 3 | The constraint is modeled as a penalty in terms of the bank’s net worth. | Penalty in net worth, same as 2014 |
| LU13-Q2 | 25 | modeling capital requirements for the bank as a penalty | Soft penalty instead of a hard constraint |
| LU13-Q3 | 25 | Due to the log term, the bank is penalized when its capital goes below the required 8 percent of total assets | Log-term penalty below γ (eq. 19) |
| LU13-Q4 | 25 | The second component of the penalty function penalizes capita-to-asset ratios above the steady state. | Second (ζ) term; typo "capita" is verbatim |
| LU13-Q5 | 30 | rewards banks for capital ratios above the required value (except at the steady state) | ζ < 1 makes the function a reward above γ, so not one-sided |
| LU13-Q6 | 30 | penalizes banks for capital ratios above its required value. | The 2013 description of ζ = 0, reversed in 2014 (D1) |
| LU13-Q7 | 24 | It is typically assumed in the banking literature that equity is more expensive than deposits | Proportional equity cost τ |
| LU13-Q8 | 35 | This finding suggests that issuing stocks may be costly. | Weaker 2013 wording; no adjustment cost yet |
| LU13-Q9 | 28 | Regarding banks, we choose the parameters to come as close as possible to the aggregate summary statistics for banks reported in section 4. | Calibration, not estimation |
| LU13-Q10 | 31 | We approximate the model without capital constraints by assigning a very low value to the penalty parameter | No-requirement benchmark via φ near 0 |
| LU13-Q11 | 33 | bank capital requirements lead to a smaller fall in output right after a capital quality shock | 2013 de-amplification result (reversed in 2014) |
| LU13-Q12 | 33 | It is clear that the presence of capital constraints greatly reduces the volatility of bank equity by requiring recapitalization when the balance sheet of banks is under stress. | Recapitalisation under the penalty |
| LU13-Q13 | 36 | Banks hold more equity when they operate in the high-risk environment, both in level and as percentage of their assets. | Model buffer rises (slightly) with risk |
| LU13-Q14 | 9 | indicate that the majority of the banks were well-capitalized, and they hold more than the regulatory requirement. | Empirical capital buffer |
| LU13-Q15 | 3 | We show that reducing the minimum capital requirement increases the frequency of bankruptcy. | Claim made in intro but not delivered in body (D2) |

## Lean results

| Name | Statement |
|---|---|
| `LambertiniUysal.logTerm_neg_below` | The log term is negative below the requirement. |
| `LambertiniUysal.logTerm_pos_above` | It is positive above it. |
| `LambertiniUysal.control_logTerm_zero_at_requirement` | It is zero at the requirement. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| LU13-D1 | p. 30 | The statement that $\zeta=0$ penalises ratios above the requirement is a slip; with $\zeta=0$ the penalty is the pure log term, negative only below, as p. 25 says and the 2014 version corrects. | LU13-Q3, LU13-Q6. |
| LU13-D2 | pp. 24, 29-30 | The only equity friction is a proportional per-period cost $\tau$, with inconsistent values; no fixed and no quadratic issuance cost. | LU13-Q7. |
| LU13-D3 | p. 37 | The model capital ratio sits at about the requirement, so the 2013 model has practically no buffer. | LU13-Q13. |
| LU13-D4 | whole paper | It adds nothing to the 2014 version for the theory paper beyond an earlier date for the requirement modelled as a net-worth penalty. | LU13-Q1, LU13-Q2. |
