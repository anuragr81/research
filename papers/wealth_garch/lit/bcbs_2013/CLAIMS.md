# Claims: Basel Committee on Banking Supervision (2013)

Bib key `bcbs2013`. Version read: January 2013. Evidence [F] for the pages read (see `RECONSTRUCTION.md`).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| LCR-Q1 | 4 | The standard requires that, absent a situation of financial stress, the value of the ratio be no lower than 100% | The liquidity constraint is HQLA over net outflows at least one (G5, G6). |
| LCR-Q2 | 7 | The numerator of the LCR is the "stock of HQLA". | The numerator of BCVW's liquidity constraint (G6). |
| LCR-Q3 | 12 | Level 1 assets can comprise an unlimited share of the pool and are not subject to a haircut under the LCR. | The riskless asset entering HQLA in full (BCVW-Q11) matches Level 1 (G6). |
| LCR-Q4 | 13 | subject to the requirement that they comprise no more than 40% of the overall stock after haircuts have been applied. | A cap on Level 2 assets that the model does not have (LCR-D3). |
| LCR-Q5 | 13 | A 15% haircut is applied to the current market value of each Level 2A asset held in the stock of HQLA. | One of the haircuts $a_3$ could stand for (LCR-D2). |
| LCR-Q6 | 14 | may be included in Level 2B, subject to a 25% haircut | As LCR-Q5. |
| LCR-Q7 | 15 | Common equity shares that satisfy all of the following conditions may be included in Level 2B, subject to a 50% haircut | As LCR-Q5. |
| LCR-Q8 | 20 | up to an aggregate cap of 75% of total expected cash outflows | Inflows are capped, while the model has no inflows (LCR-D3). |
| LCR-Q9 | 21 | Stable deposits, which usually receive a run-off factor of 5% | BCVW's baseline $a_2=0.05$ is this run-off factor (G5, LCR-D1). |
| LCR-Q10 | 22 | with a minimum run-off rate of 10%. | Less stable retail deposits run off faster (LCR-D1). |

## Lean results

| Name | Statement |
|---|---|
| `MeasurementMap.hqla_haircut` | With haircut $a_3$ on the risky share, HQLA $=(1-\pi)X+(1-a_3)\pi X=X-a_3\pi X$. |
| `MeasurementMap.lcr_iff` | For $a_2,a_3,L>0$: $1\le(X-a_3\pi X)/(a_2L)\iff\pi x\le(x-a_2)/a_3$. |
| `MeasurementMap.cap_iff` | Both ratio constraints hold iff $\pi x\le u(x)$, the capped exposure of `CapGeometry`. |
| `MeasurementMap.control_lcr_needs_positive_runoff` | At $a_2=0$ the ratio form fails while the cap form holds. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| LCR-D1 | pp. 21 to 22 | $a_2$ is a blended run-off rate on all liabilities. Its baseline 0.05 is the factor for stable insured retail deposits, the lowest bucket apart from the 3% option. | LCR-Q9, LCR-Q10. The standard applies different rates to different deposit categories, and the model has one liability. |
| LCR-D2 | pp. 12 to 15 | $a_3$ is a blended haircut on the risky asset. Its baseline 0.30 matches no single haircut of the standard (0%, 15%, 25%, 50%). | LCR-Q3, LCR-Q5 to LCR-Q7 against BCVW-Q13. |
| LCR-D3 | pp. 13 and 20 | The model's HQLA is linear in $\pi$ and it has no inflows. The 40% cap on Level 2 assets and the 75% cap on inflows are not represented. | LCR-Q4, LCR-Q8. |
