# Claims: Houston, James and Marcus (1997)

Bib key `houston1997`. Version read: the published article, JFE 46:135-164, with an OCR text layer. Evidence [F] for the text; the tables were not read (HJM-D2).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| HJM-Q1 | 139 | Some companies tend to keep the minimum capital levels in their subsidiary banks by transferring excess capital to the parent in the form of dividends. | The supervision manual, quoted: subsidiaries held at the minimum, excess paid up as dividends. Checked against the page image. |
| HJM-Q2 | 139 | Other companies calculate dividends based strictly on the parent's cash needs and keep any excess at the bank level | The other practice: dividends set by the parent's cash needs. |
| HJM-Q3 | 139 | The source of strength doctrine gives rise to a perceived obligation on the part of the holding company to downstream capital | Capital injections from the parent into weak subsidiaries. |
| HJM-Q4 | 137 | However, most banks are subsidiaries of multiple bank holding companies. | The unit of decision is often not the bank. |
| HJM-Q5 | 148 | will have an estimated sensitivity of 3.83 | Checked in Lean (`HoustonJamesMarcus.median_sensitivity`). Checked against the page image. |
| HJM-Q6 | 148 | reach nearly 13 percentage points before its estimated cash-flow sensitivity would equal zero | Checked in Lean (`HoustonJamesMarcus.zero_near_13_points`). |
| HJM-Q7 | 143 | about 3% of the bank holding companies (31 out of 954) were below the regulatory minimum | Few holding companies below the minimum. Checked in Lean (`HoustonJamesMarcus.below_minimum_share`). |
| HJM-Q8 | 156 | we find that what matters is whether the holding company, and not the bank itself, faces binding capital requirements. | The binding constraint is at the holding company. |
| HJM-Q9 | 141 | Our sample contains 281 bank holding companies, 237 of which are multi-bank holding companies. | Sample. |
| HJM-Q10 | 142 | Required ratios were 5.5% from 1981-1984, and were 6% from 1985-1989. | The leverage requirement of the period. |

## Lean results

| Name | Statement |
|---|---|
| `HoustonJamesMarcus.sensitivity_anti` | With a negative interaction coefficient, the cash-flow sensitivity $a+bs$ falls as surplus $s$ rises. |
| `HoustonJamesMarcus.sensitivity_zero` | It is zero at $s=-a/b$. |
| `HoustonJamesMarcus.median_sensitivity` | $4.53+0.02(-34.95)=3.831$, which rounds to 3.83. |
| `HoustonJamesMarcus.zero_near_13_points` | The zero is at $4.53/34.95\in(0.129,0.13)$. |
| `HoustonJamesMarcus.below_minimum_share` | $31/954\in(0.03,0.035)$. |
| `HoustonJamesMarcus.control_sensitivity_rises_if_b_positive` | With $b>0$ the sensitivity rises with surplus. |
| `HoustonJamesMarcus.control_zero_not_at_13_with_other_sign` | With $b>0$ it is not zero at $4.53/34.95$. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| HJM-D1 | the copy read | Published version, OCR text layer. The quotations HJM-Q1 and HJM-Q5 were checked against the page images on 2026-10-12. | Quotations are matched against the text layer. |
| HJM-D2 | the tables | Not read; garbled in the text layer. Nothing from them is attributed beyond what the text states. | Evidence [F] covers the text only. |
| HJM-D3 | pp. 141, 144, 148 | Holding-company regressions use 1982-1989. | The text says Y-9 data from 1981-1989 (p. 141); Table 1's caption and p. 148 say 1982-1989. |
