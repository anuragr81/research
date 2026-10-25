# Claims: Lambertini and Uysal (2014)

Bib key `lambertini2014`. Version read: EPFL working paper, February 2014; 47 PDF pages, printed page = PDF page. Read in full; figures for captions only. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| LU14-Q1 | 22 | We interpret this evidence as suggesting that mandated capital requirements overall bind for financial institutions but not strictly and not at all times. | Motivation for a soft penalty instead of a hard constraint |
| LU14-Q2 | 23 | Due to the log term, the bank is penalized when its capital goes below the required 8 percent of total assets | Penalty bites below γ through a log term (eq. 20) |
| LU14-Q3 | 23 | The second component of the penalty function penalizes capital-to-asset ratios above the steady state. | Second (ζ) term charges capital above steady state |
| LU14-Q4 | 23 | First, it allows for variations in the capital-to-asset ratio that a fixed constraint would instead rule out. | Purpose of the penalty: variation in the ratio |
| LU14-Q5 | 27 | penalizes banks for capital ratios different from the required value, both larger and smaller | With ζ = 1 the penalty is two-sided, so not asymmetric by design |
| LU14-Q6 | 21 | It is typically assumed in the banking literature that equity is more expensive than deposits | Justification for proportional equity cost τ > ι |
| LU14-Q7 | 25 | This cost is present only outside the steady state and it captures the fact that issuing and retiring equity is costly. | Quadratic equity adjustment cost (λ_0); no fixed cost |
| LU14-Q8 | 35 | This finding suggests that issuing stocks may be more costly than assumed in our paper. | Authors concede issuance cost is too low or misspecified |
| LU14-Q9 | 26 | Regarding banks, we choose the parameters to come as close as possible to the aggregate summary statistics for banks reported in section 3. | Bank parameters calibrated informally, not estimated |
| LU14-Q10 | 27 | We choose these values to roughly match the standard deviation of bank assets as reported in table 9. | Capital-quality shock calibrated to asset volatility |
| LU14-Q11 | 32 | Banks hold more capital when they operate in the high-risk environment, both in level and as percentage of their assets. | Model buffer rises with risk |
| LU14-Q12 | 36 | Intuitively, the cost equity is such that banks trade off capital for penalty. | Interaction of equity cost and penalty; steady state below requirement |
| LU14-Q13 | 37 | unless the penalty for being under-capitalized is higher, banks will not raise sufficiently their capital ratios under Basel III | Smooth penalty allows persistent shortfall |
| LU14-Q14 | 40 | we document that banks actively manage their capital ratios and hold capital that are well above the regulatory minimum requirement. | Empirical capital buffer above the minimum |
| LU14-Q15 | 9 | Tables 3 and 4 indicate that the majority of the banks were well-capitalized, and they hold more than the regulatory requirement. | Empirical buffer, by size class |

## Lean results

| Name | Statement |
|---|---|
| `LambertiniUysal.logTerm_neg_below` | The log term $\log(k/\gamma a)$ is negative below the requirement. |
| `LambertiniUysal.logTerm_pos_above` | It is positive above it, a reward rather than zero. |
| `LambertiniUysal.control_logTerm_zero_at_requirement` | It is zero at the requirement: no kink and no one-sided charge. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| LU14-D1 | pp. 23, 27 | The penalty $\phi[\log(k/\gamma a)-\zeta(k-\bar k)/\bar k]$ is smooth across the requirement and rewards capital above it; asymmetry comes only from the concavity of the log. | LU14-Q2, LU14-Q3, LU14-Q5. |
| LU14-D2 | pp. 21-25 | The equity costs are a proportional per-period cost $\tau$ on outstanding equity and a quadratic adjustment cost in the share count; there is no fixed issuance cost. | LU14-Q6, LU14-Q7. |
| LU14-D3 | pp. 22-24 | Banks have no dividend policy and pay out only on random exit, so nothing corresponds to a dividend barrier or a trigger. | Bellman equation (21). |
| LU14-D4 | pp. 26, 41 | All bank parameters are calibrated by assertion to aggregate statistics; estimation is left to future work. | LU14-Q9, LU14-Q10. |
| LU14-D5 | pp. 32, 36 | The model capital ratio sits just above 8 percent and, under Basel III, below the requirement, because the smooth penalty lets the bank trade capital against penalty. | LU14-Q11 to LU14-Q13. |
