# Claims: Alvarez, Le Bihan and Lippi (2014)

Bib key `alvarez2014`. Version read: NBER Working Paper 20155, May 2014; 49 PDF pages, printed page = PDF page - 2. Read in full; it contains Appendix A only, and the online Appendices B to S were not read. The notes listed Banque de France WP 492; that version was not read, and whether it differs is not known. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| ALL-Q1 | 2 | In those models additional information is needed to identify the parameters | When the price-change distribution is not informative (e.g. no menu cost), identification needs extra data. |
| ALL-Q2 | 4 | are pinned down by the shape of the distribution of price changes, e.g. by its kurtosis, and by the data on the costs of price adjustment. | (n, ℓ) are identified from shape/kurtosis plus menu-cost evidence, after ψ/B and λ are fixed by N and Std. |
| ALL-Q3 | 4 | In our set-up a given kurtosis may be obtained by | Opening of fn. 3; it continues "different combinations of n and ℓ", the explicit non-identification of (n, ℓ) by kurtosis alone. |
| ALL-Q4 | 4 | models with high n yield a better representation of the cross-sectional data because it eliminates the predominant mass of large price changes | Extra information (shape) is used to choose among (n, ℓ) pairs with equal kurtosis. |
| ALL-Q5 | 16 | We show below that in order to generate a shape of the size distribution that is comparable to the one in the data one needs | The shape of the distribution disciplines n (continues "n ≥ 6"). |
| ALL-Q6 | 17 | as opposed to each of them separately. Intuitively this is because when a free adjustment opportunity occurs the price gap is adjusted | The decision rule depends on λ + r only: the threshold cannot tell λ from r. |
| ALL-Q7 | 20 | We underline that equation (3) holds for any stopping rule, not just for the optimal one. | σ² = N·Var is identified independently of the optimal policy (Prop. 1). |
| ALL-Q8 | 21 | for large values of the adjustment cost the rule becomes a square root and that the optimal threshold does not depends on | Prop. 2: for large ψ the threshold does not respond to σ (a parameter that does not move a threshold). |
| ALL-Q9 | 21 | For a given n there is a one to one and onto mapping between | Prop. 3: global bijection between φ and the fraction of free adjustments ℓ. |
| ALL-Q10 | 23 | Every two economies sharing these parameters will have the same size distribution of price changes once the scale is adjusted. | Prop. 4: the shape depends only on (n, ℓ). |
| ALL-Q11 | 24 | the kurtosis of the price changes would not change. Moreover, kurtosis is increasing in both n and | Kurtosis is invariant to ψ/B, λ, σ² given (n, ℓ), and increases in both n and ℓ (same direction). |
| ALL-Q12 | 25 | Kurtosis and the costs of price adjustments can thus be used to discipline the parameterization of the model | The two moments used to pin down the residual parameters (n, ℓ). |
| ALL-Q13 | 27 | is not monotone in the number of products n. | The implied per-adjustment menu cost is not monotone in n; for larger ℓ the order across n reverses. |
| ALL-Q14 | 28 | the model reproduces the yearly cost of 0.7% of revenues when the fraction of free adjustments | Calibration: n = 10 and ℓ ≈ 60% from the observed adjustment-cost share. |
| ALL-Q15 | 37 | Compared to our model, these models require more observations to be identified, such as direct information on the shape of the cost shocks faced by firms. | Limitation: with fat-tailed shocks, kurtosis is no longer sufficient. |

## Lean results

| Name | Statement |
|---|---|
| `AlvarezLeBihanLippi.sigma_sq_from_moments` | Prop. 1: $\sigma^2=N\cdot\mathrm{Var}$ from the frequency and the variance of price changes. |
| `AlvarezLeBihanLippi.lambda_from_share` | Prop. 3 with $r\to0$: $\lambda=\ell N$ from the share of free adjustments. |
| `AlvarezLeBihanLippi.control_same_direction_not_identified` | A moment that rises in two parameters takes the same value at two distinct pairs. |
| `AlvarezLeBihanLippi.control_threshold_sees_sum_only` | A threshold that depends on $\lambda+r$ only is the same at two distinct pairs. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| ALL-D1 | pp. 20, 21, 26 | Given the number of products $n$ and the share of free adjustments $\ell$, the variance of shocks, the free-adjustment rate and the menu cost are identified globally and analytically, by bijections and a unique triplet, not by a local argument. | ALL-Q7, ALL-Q9; Props. 1, 3, 5. |
| ALL-D2 | pp. 4, 24 | Kurtosis rises in both $n$ and $\ell$, shown by Figure 5 rather than a proposition, so kurtosis alone leaves a curve of pairs. | ALL-Q3, ALL-Q11. |
| ALL-D3 | pp. 4, 27-28 | A second moment, the implied cost of adjustment, rising in $\ell$ and not monotone in $n$, together with the shape requirement $n\ge6$, selects $n=10$ and $\ell\approx0.6$. The paper states no Jacobian, rank or opposite-sign condition; this reading of the procedure is mine. | ALL-Q2, ALL-Q12 to ALL-Q14. |
| ALL-D4 | pp. 17, 20 | The threshold depends on $\lambda$ and $r$ only through $\lambda+r$, two parameters the threshold cannot tell apart; they set $r\to0$ and identify $\lambda$ from the share of free adjustments. | ALL-Q6. |
| ALL-D5 | pp. 20, 26 | Identification is from moments of the distribution of adjustments and outside cost data, not from the threshold, which is never observed. The theory paper identifies its two frictions from two thresholds. | ALL-Q7, ALL-Q12. |
| ALL-D6 | pp. 16, 20 | The loss is symmetric and quadratic with a fixed menu cost as the only adjustment cost, so no asymmetry parameter or proportional cost is separated from the fixed cost. | Model of Section 2. |
