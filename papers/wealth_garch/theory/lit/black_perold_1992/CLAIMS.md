# Claims: Black and Perold (1992)

Bib key `blackperold1992`. Version read: the published article, journal pages 403-426. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| BP-Q1 | 403 | maintains the portfolio's risk exposure a constant multiple of the excess of wealth over a floor, up to a borrowing limit | The rule has two branches, a multiple of the cushion and a borrowing limit, the geometry of the regulatory cap with $a_2=0$ (`BlackPerold.cap_is_cppi`). |
| BP-Q2 | 404 | It invests a constant multiple of the cushion in risky assets up to the borrowing limit, where the cushion is the difference between wealth and a specified floor. | The cushion is wealth over a floor, our equity $x-1$ with the floor at the liabilities. |
| BP-Q3 | 404 | 'safe' may mean that it closely tracks a liability stream | The reserve asset can carry liability risk, so floor and safe holdings move together and the liability is hedged (BP-D1). |
| BP-Q4 | 406 | The CPPI decision rule calls for keeping the exposure a constant multiple, m, of the cushion. | On the multiplier branch the exposure-to-cushion ratio is $m$ (`BlackPerold.multiplier_branch`). |
| BP-Q5 | 410 | Let the index ratio follow geometric Brownian motion. | Prop. 2, whose proof (A.1) gives log-cushion volatility $m\sigma$ away from the limit (`BlackPerold.multiplier_branch_vol`). |
| BP-Q6 | 411 | But the cushion grows only linearly in S as opposed to geometrically in S when the limit is not in effect. | At the limit the exposure is $bW$, and the ratio to the cushion tends to $b$ as wealth grows (`BlackPerold.limit_ratio_tendsto`, `BlackPerold.limit_vol_tendsto`). Not stated as a limit in the paper. |
| BP-Q7 | 404 | One of our results is that the borrowing-constrained rule too is utility-maximizing. | Prop. 11. |
| BP-Q8 | 406 | the size of the multiple is related to the investor's impatience to consume | The multiple is a preference. |
| BP-Q9 | 421 | The multiple depends on the utility function only through | Prop. 11: through the impatience $\rho$ (the symbol prints as "p", BP-D2). |
| BP-Q10 | 421 | an investor with sufficiently large risk tolerance | The leverage limit $b$ exceeds one for small risk aversion, so $b$ is a preference too. |
| BP-Q11 | 421 | the level of wealth at which the borrowing limit is reached | $W^*=F\,m/(m-b)$ (`BlackPerold.binding_wealth`). |
| BP-Q12 | 406 | There is little lost in generality, therefore, by assuming b = 1, i.e., no borrowing permitted. | The paper's main case is no borrowing, as in Bayraktar et al. (BCVW-Q15). |

## Lean results

| Name | Statement |
|---|---|
| `BlackPerold.multiplier_branch` | If $m(W-F)\le bW$ and $W>F$, then $E/C=m$. |
| `BlackPerold.multiplier_branch_vol` | Under the same conditions the log-cushion variance $(E/C)^2\sigma^2$ is $m^2\sigma^2$. |
| `BlackPerold.limit_branch` | If $bW\le m(W-F)$, then $E=bW$. |
| `BlackPerold.limit_ratio_tendsto` | $bW/(W-F)\to b$ as $W\to\infty$. |
| `BlackPerold.limit_vol_tendsto` | At the limit the log-cushion variance tends to $b^2\sigma^2$ as $W\to\infty$. |
| `BlackPerold.binding_wealth` | For $m\ne b$, $m(W-F)=bW\iff W=Fm/(m-b)$, the $W^*$ of p. 421. |
| `BlackPerold.cap_is_cppi` | `CapGeometry.capU` with $a_2=0$ equals the rule with $m=1/a_1$, $b=1/a_3$, $F=1$. |
| `BlackPerold.control_ratio_needs_multiplier_branch` | At $m=4$, $b=1$, $W=2$, $F=1$ the limit binds and $E/C=2\ne4$. |
| `BlackPerold.control_limit_needs_fixed_floor` | With the floor growing as $W/2$ the ratio is $2b$, not $b$. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| BP-D1 | pp. 404, 407-408 | Liability risk enters through the index ratio $S=A/R$; floor and reserve holdings move together, so the liability is hedged. | The floor is a fixed number of reserve-asset shares (p. 406). In the theory paper's bank the riskless asset does not track the liabilities, which is why its deficit limit is $\sigma^2/a_1^2-2c\sigma\sigma_L/a_1+\sigma_L^2$ and not $m^2\sigma_S^2$. |
| BP-D2 | p. 421 | "through p" is "through ρ", the impatience to consume; "small A" is "small λ", the relative risk aversion. | The text layer is OCR of a scan; the symbols are those of (7.1) and the definition of $U$. |
| BP-D3 | p. 411 | The limit of the log-cushion volatility at the borrowing limit for large wealth is ours, from the linear growth the paper states. | The paper states no limit of the volatility and no ratio of the two branch volatilities. |
