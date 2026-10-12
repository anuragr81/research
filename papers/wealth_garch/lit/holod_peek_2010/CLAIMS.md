# Claims: Holod and Peek (2010)

Bib key `holod2010`. Version read: the working paper dated 5 December 2006 (SSRN 891563), 44 numbered pages after a title page. Evidence [F] for that version.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| HP-Q1 | 14 | CapTrans is measured as the sum of dividends (with a negative sign), bank capital sale and acquisition, and other transactions with the parent MBHC not included in the first two items. | The definition of capital transfers with the parent. Checked against Tables 1 and 2 in Lean. |
| HP-Q2 | 13 | They may have their undercapitalized subsidiaries (or those most in need of additional capital) pay smaller dividends, while having subsidiaries with higher capital ratios | Dividends of a subsidiary are a tool of the parent's capital allocation. |
| HP-Q3 | 14 | Alternatively, they may have the same dividend requirements for all of their subsidiaries, but downstream capital to those subsidiaries most in need of additional capital and upstream capital from those subsidiaries with excess capital | The second strategy: injections from the parent. |
| HP-Q4 | 15 | In particular, higher capital subsidiaries pay considerably more dividends to their parent MBHC, and lower capital subsidiaries receive relatively more capital through capital sale and acquisition and other transactions with their parent holding company. | The pattern of Table 1 (HP-D2 for Table 2). |
| HP-Q5 | 4 | We find that bank subsidiaries receive more capital transfers the lower is their capital ratio and the higher are the capital ratios of the other banks in their MBHC, consistent with the regulatory incentive to reallocate funds within the MBHC. | Equation (1), Table 6. |
| HP-Q6 | 29 | the effect of the capital ratio on the net loan sales of banks affiliated with an MBHC is almost three times as large (in absolute value) as that of stand-alone banks | Checked in Lean (`HolodPeek.table8_almost_three_times`). |
| HP-Q7 | 16 | loan sales and purchases reported by banks do not distinguish between loan sales and purchases made between affiliates of the same MBHC and those conducted with counterparties external to the MBHC. | A limit of the Call Report data. |
| HP-Q8 | 43 | In fact, most banks typically hold levels of capital above the minimum required by bank regulators. | Buffers above the minimum (n. 2). |
| HP-Q9 | 43 | this should be interpreted more broadly as target capital ratios set by bank management | Their regulatory motive is read as a management target (n. 2). |
| HP-Q10 | 43 | As a result, currently all banks report Schedule RI-A quarterly. | From 2001:Q1 (n. 6); our 2015-2024 panel is quarterly for every bank. |
| HP-Q11 | 44 | Stand-alone banks include banks that are not affiliated with a bank holding company, as well as banks that are affiliated with a one-bank holding company. | The split adopted for the ownership test (n. 8). |
| HP-Q12 | 44 | We use the (unweighted) leverage capital ratio rather than the risk-based capital ratio. | Their capital ratio (n. 10). |
| HP-Q13 | 13 | Our annual sample covers the period from 1987 to 2004. | Sample period of equation (1). |

## Lean results

| Name | Statement |
|---|---|
| `HolodPeek.capTrans_upstream_iff` | CapTrans $<0$ exactly when capital sales plus other transactions are below dividends: a net transfer to the parent. |
| `HolodPeek.table1_captrans` | In each capital quintile of Table 1, $-\text{Div}+\text{CapSale}+\text{Other}$ equals the reported total exactly. |
| `HolodPeek.table1_net_loan_sales` | In Table 1, loan sales less purchases equal the reported net loan sales exactly. |
| `HolodPeek.table1_dividends_rise` | Table 1 dividends rise across the quintiles, 0.51 to 0.93 percent of assets. |
| `HolodPeek.table2_captrans_within_rounding` | In Table 2 every total is within 0.01 of the sum of its components. |
| `HolodPeek.table2_two_cells_rounded` | Two Table 2 totals are not exactly the sum (HP-D2). |
| `HolodPeek.p17_differences` | The differences 0.65 and 0.53 quoted on p. 17. |
| `HolodPeek.table8_almost_three_times` | $2.8<0.0440/0.0153<3$ and the difference $-0.0287$ of Table 8. |
| `HolodPeek.table10_more_than_twice` | $0.0295/0.0136>2$ and the difference $-0.0159$ of Table 10. |
| `HolodPeek.table9_slightly_more_than_half` | $1/2<0.0246/0.0440<0.6$ (p. 30). |
| `HolodPeek.control_dividends_enter_negatively` | With dividends entered positively, Table 1's first total is not reproduced. |
| `HolodPeek.control_not_three_times` | The Table 8 ratio is not 3 or more. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| HP-D1 | the copy read | Working paper of 5 December 2006; the JMCB 2010 article was not read. | Only this version's pages are attributed. |
| HP-D2 | Table 2, p. 33 | The totals $-0.45$ and $-0.87$ are the components' sums $-0.46$ and $-0.88$ after rounding. | Each component is itself rounded; every other total is exact (Lean). |
| HP-D3 | n. 7, p. 44 | "quartiles" means the quintiles of Tables 1 and 5. | The tables group banks into five capital-ratio groups. |
| HP-D4 | equations (1) and (2), p. 20 | The coefficient index 8 appears twice in each equation; the equations have ten slope coefficients. | Table 6 and Table 7 report ten coefficients. |
| HP-D5 | p. 8 | "Markus" is Marcus. | The reference list, p. 41. |
