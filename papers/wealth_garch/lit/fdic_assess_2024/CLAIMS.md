# Claims: Federal Deposit Insurance Corporation (2024), 12 CFR part 327

Bib key `fdic2024assess`. Source: Code of Federal Regulations, title 12, part 327, Assessments, edition of 1 January 2024, GovInfo. Read: section 327.4(a) to (c) (printed p. 476) and section 327.16(a)(1) with its pricing multipliers (printed pp. 509 to 510). Evidence [F] for those pages only.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| ASMT-Q1 | 476 | each insured depository institution will be provided an assessment risk assignment. | The insurer prices each bank's insurance by its risk (A11). |
| ASMT-Q2 | 509 | each of seven financial ratios and a weighted average of CAMELS component ratings will be multiplied by a corresponding pricing multiplier. | A small bank's assessment rate is linear in its ratios (A11). |
| ASMT-Q3 | 509 | The sum of these products will be added to a uniform amount. The resulting sum shall equal the institution's initial base assessment rate | The rate is the uniform amount plus the weighted ratios (A11). |
| ASMT-Q4 | 510 | financial ratios are: Leverage Ratio (%) | The leverage ratio is the first of the seven (A11). |

## Lean results

| Name | Statement |
|---|---|
| `MeasurementMap.assessment_falls_in_leverage` | With a negative multiplier on the leverage ratio, a higher ratio gives a strictly lower rate. |
| `MeasurementMap.control_assessment_needs_negative_multiplier` | With a positive multiplier the rate does not fall. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| ASMT-D1 | p. 510 | The pricing multiplier on the leverage ratio is negative in all three schedules shown (-1.264 when the reserve ratio is below 2%; the minus sign is printed in a font glyph that text extraction reads as a yen sign, so the number is not quoted). A bank with more capital pays a lower rate for its insurance. | ASMT-Q2 to ASMT-Q4. |
