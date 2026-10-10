# Claims: FFIEC (2026)

Bib key `ffiec2026`. Version read: Schedule RC-R Part I instructions, June 2026. Evidence [F] for the pages read (see `RECONSTRUCTION.md`).

## Quotations

Pages are PDF pages. The printed labels (RC-R-n) are given in `RECONSTRUCTION.md`.

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| RCR-Q1 | 38 | Common equity tier 1 capital. Report Schedule RC-R, Part I, item 12 less item 18. | The reported CET1 capital, net of adjustments (G3). |
| RCR-Q2 | 38 | Except for a CBLR electing institution under the community bank leverage ratio framework, the amount reported in this item is the numerator of the institution's common equity tier 1 risk-based capital ratio. | Banks electing the community bank leverage ratio do not report the risk-based ratio (RCR-D1). |
| RCR-Q3 | 47 | Tier 1 capital. Report the sum of Schedule RC-R, Part I, items 19 and 25. | The reported Tier 1 capital (G3). |
| RCR-Q4 | 51 | Leverage ratio. Report the institution's leverage ratio as a percentage, rounded to four decimal places. Divide Schedule RC-R, Part I, item 26 by item 30. | The reported leverage ratio, Tier 1 over total assets for the leverage ratio (G1, G7). |
| RCR-Q5 | 67 | Total risk-weighted assets. Report the amount of total risk-weighted assets | The reported denominator of the risk-based ratios (G2). |
| RCR-Q6 | 67 | On the FFIEC 041: Divide Schedule RC-R, Part I, item 19 by item 48. | The reported CET1 ratio (G4). |
| RCR-Q7 | 68 | On the FFIEC 041: Divide Schedule RC-R, Part I, item 26 by item 48. | The reported Tier 1 ratio (G4). |

## Lean results

| Name | Statement |
|---|---|
| `MeasurementMap.rwa_two_assets` | With risk weight 0 on the riskless and 1 on the risky asset, risk-weighted assets are $\pi X$. |
| `MeasurementMap.solvency_iff` | For $a_1,\pi,X,L>0$: $a_1\le(X-L)/(\pi X)\iff\pi x\le(x-1)/a_1$. |
| `MeasurementMap.leverage_of_state` | $(X-L)/X=(x-1)/x$ for $X,L>0$. |
| `MeasurementMap.control_rwa_needs_zero_weight_on_riskless` | With weight $1/5$ on the riskless asset, risk-weighted assets differ from $\pi X$. |
| `MeasurementMap.control_leverage_needs_l0_lt_one` | At $\ell_0=1$ the leverage rearrangement fails. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| RCR-D1 | p. 38 | For a bank electing the community bank leverage ratio, the risk-based ratios and risk-weighted assets are not observed. Only the leverage ratio is. | RCR-Q2. |
| RCR-D2 | pp. 51 and 67 | $\pi$ is observed as item 48 over total assets only under the risk-weight identification of `MeasurementMap.rwa_two_assets`. | The document reports risk-weighted assets, not a risky share. The control shows the identification is needed. |
| RCR-D3 | p. 51 | The state $x$ is recovered from the leverage ratio $\ell$ as $1/(1-\ell)$ only if Tier 1 capital equals book equity and item 30 equals total assets. | `MeasurementMap.leverage_of_state`. Item 30 is item 27 less items 28 and 29, which were not read. |
