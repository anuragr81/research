# Claims: Basel Committee on Banking Supervision (2011)

Bib key `bcbs2011`. Version read: December 2010, revised June 2011. Evidence [F] for the pages read (see `RECONSTRUCTION.md`).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| B3-Q1 | 12 | Common Equity Tier 1 must be at least 4.5% of risk-weighted assets at all times. | The solvency floor $a_1$ is a minimum of capital over risk-weighted assets. BCVW's baseline $a_1=0.045$ is this number (G4). |
| B3-Q2 | 12 | Tier 1 Capital must be at least 6.0% of risk-weighted assets at all times. | The alternative reading $a_1=0.06$ (G4). |
| B3-Q3 | 12 | All elements above are net of the associated regulatory adjustments | Regulatory capital is not book equity $X-L$ (G3, B3-D2). |
| B3-Q4 | 55 | A capital conservation buffer of 2.5%, comprised of Common Equity Tier 1, is established above the regulatory minimum capital requirement. | A buffer above $a_1$ that the model does not have (B3-D3). |
| B3-Q5 | 55 | Capital distribution constraints will be imposed on a bank when capital levels fall within this range. | Payouts are restricted inside the buffer, while the model restricts them only through the barrier $y^*$ (B3-D3). |
| B3-Q6 | 56 | Items considered to be distributions include dividends and share buybacks | The restricted payouts are the model's dividends (G14). |
| B3-Q7 | 61 | The Committee will test a minimum Tier 1 leverage ratio of 3% during the parallel run period from 1 January 2013 to 1 January 2017. | A floor on capital over exposure, the candidate counterpart of the requirement $q$ (G7). |

## Lean results

| Name | Statement |
|---|---|
| `MeasurementMap.solvency_iff` | For $a_1,\pi,X,L>0$: $a_1\le(X-L)/(\pi X)\iff\pi x\le(x-1)/a_1$ with $x=X/L$. |
| `MeasurementMap.rwa_two_assets` | With risk weight 0 on the riskless and 1 on the risky asset, risk-weighted assets are $\pi X$. |
| `MeasurementMap.leverage_of_state` | $(X-L)/X=(x-1)/x$ for $X,L>0$. |
| `MeasurementMap.leverage_req_iff` | For $\ell_0<1$, $X,L>0$: $\ell_0\le(X-L)/X\iff\ell_0/(1-\ell_0)\le x-1$. |
| `MeasurementMap.control_solvency_needs_positive_exposure` | At $\pi=0$ the ratio form fails while the cap form holds. |
| `MeasurementMap.control_rwa_needs_zero_weight_on_riskless` | With weight $1/5$ on the riskless asset, risk-weighted assets differ from $\pi X$. |
| `MeasurementMap.control_leverage_needs_l0_lt_one` | At $\ell_0=1$ the equivalence fails. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| B3-D1 | p. 12 against BCVW p. 2 | $a_1$ is read as the CET1 minimum 4.5%, with Tier 1 (6.0%) the alternative. | BCVW p. 2 names "Basel III's Tier 1 capital ratio", but its baseline $a_1=0.045$ (BCVW-Q13) is the CET1 minimum. The tier is not fixed by the source pair. |
| B3-D2 | p. 12 | Model equity $X-L$ stands for regulatory capital only under the assumption that regulatory adjustments vanish. | B3-Q3. The capital in the ratio is net of regulatory adjustments. |
| B3-D3 | pp. 55 to 56 | The model has no conservation buffer and no distribution constraint. Its payout restriction is the barrier $y^*$, chosen by the bank. | B3-Q4 to B3-Q6. A ratio of CET1 between 4.5% and 7.0% restricts distributions by schedule (table on p. 56). |
