# Claims: Office of the Comptroller of the Currency (2024), 12 CFR part 6

Bib key `occ2024pca`. Source: Code of Federal Regulations, title 12, part 6, Prompt Corrective Action, edition of 1 January 2024, GovInfo. Printed pages 392 to 402, read in full. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| OCC-Q1 | 393 | Section 38 of the FDI Act establishes a framework of supervisory actions for insured depository institutions that are not adequately capitalized. | The supervisor's actions depend on the capital category (A9). |
| OCC-Q2 | 395 | A Consolidated Reports of Condition and Income (Call Report) is required to be filed with the OCC | A bank is deemed notified of its capital category when it files its Call Report, so the category is read from the report (A9). |
| OCC-Q3 | 395 | has a total risk-based capital ratio of 10.0 percent or greater | Well capitalized needs a total risk-based ratio of at least 10% (A9). |
| OCC-Q4 | 396 | has a total risk-based capital ratio of 8.0 percent or greater | Adequately capitalized needs at least 8% (A9). |
| OCC-Q5 | 397 | total assets that is equal to or less than 2.0 percent. | Critically undercapitalized: tangible equity to total assets of 2% or less (A10). |
| OCC-Q6 | 400 | All national banks and Federal savings associations are subject to the restrictions contained in section 38(d) of the FDI Act on payment of distributions and management fees. | The dividend restriction applies to every national bank (A9). |

## Lean results

| Name | Statement |
|---|---|
| `MeasurementMap.distribution_allowed_iff` | The largest distribution that keeps the total risk-based ratio at $k$, for $k=8\%$ (OCC-Q4). |
| `MeasurementMap.control_distribution_needs_positive_rwa` | At $R=0$ the ratio form fails while the level form holds. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| OCC-D1 | pp. 395 to 397 | Our reference R of 10.5% (comparison/PREREG_comparison.md) sits above the well-capitalized total ratio of 10.0% and the adequately capitalized 8.0%; a bank at R is well capitalized on that measure. | OCC-Q3, OCC-Q4. |
