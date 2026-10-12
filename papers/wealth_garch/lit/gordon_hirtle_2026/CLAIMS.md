# Claims: Gordon and Hirtle (2026)

Bib key `gordon2026`. Version read: the published article, FRBNY *Economic Policy Review* 32(1), September 2026, 49 PDF pages. Evidence [F]. Pages are PDF pages (GH-D1).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| GH-Q1 | 4 | nearly all banks pay dividends most of the time: dividend payments are positive in more than 85 percent of bank-year observations in our sample. | Holding companies pay dividends almost always: the outside-shareholder counterpart of E3. |
| GH-Q2 | 4 | Common stock dividends and repurchases exceed earnings for more than 20 percent of the bank-years in the sample | Payouts above earnings are common. |
| GH-Q3 | 4 | This behavior is more likely at banks with higher levels of common equity, suggesting that capital planning—for instance, movement toward a target capital ratio—could play a role. | The authors' reading: capital management toward a target. |
| GH-Q4 | 4 | banks are also more likely to make payouts that exceed earnings when they experience a negative earnings shock | The second channel: sticky payouts against earnings shocks. |
| GH-Q5 | 3 | Common stock issuance is quite frequent, but amounts tend to be small, with median issuance amounts of 0.9 percent of equity capital among banks that issue in a given year. | Most issuance is small. |
| GH-Q6 | 16 | In a typical year, the combination of net income and OCI equals 11.6 percent of prior-year equity capital. | Earnings dominate equity growth. |
| GH-Q7 | 40 | Together, these findings indicate that much routine issuance reflects compensation and share-maintenance practices rather than deliberate efforts to raise capital. | Measured issuance is mostly not capital raising. |
| GH-Q8 | 3 | our sample excludes bank holding companies with assets less than $3 billion in 2024 dollars and stand-alone commercial banks. | Their population (n. 1). |
| GH-Q9 | 40 | Examples include examining how payout behavior interacts with internal capital targets versus supervisory requirements | Named by the authors as open. |
| GH-Q10 | 29 | This pattern is consistent with the capital-management channel where banks distribute more capital when equity levels are high. | Table 2, column 4. |
| GH-Q11 | 27 | The Tier 1 buffer equals each bank's Tier 1 risk-based capital ratio (Tier 1 capital divided by risk-weighted assets) minus the applicable regulatory Tier 1 requirement at year-end. | Their buffer (n. 13). |
| GH-Q12 | 27 | Tier 1 Buffer 6.74 3.69 6.13 0.52 22.30 6394 | Table 1: the mean Tier 1 buffer is 6.74 percentage points above the requirement. |
| GH-Q13 | 27 | Equity / Assets 9.10 2.65 8.79 4.18 17.68 6448 | Table 1: equity to assets, mean 9.10%, SD 2.65. |
| GH-Q14 | 27 | Negative EMP 0.21 0.41 0.00 0.00 1.00 6448 | Table 1: one bank-year in five. |
| GH-Q15 | 28 | a bank with negative EMP in one year is 50 to 100 percent more likely to have negative EMP in the following year (10 to 20 percentage points relative to the overall average of 21 percent) | Checked in Lean against Table 2 (GH-D2). |
| GH-Q16 | 29 | a one standard deviation increase in equity capital results in a 10 to 30 percent (2 to 6 percentage point) increase in the likelihood of negative EMP relative to average | Checked in Lean against Tables 1 and 2. |
| GH-Q17 | 29 | a one standard deviation decrease in an earnings shock raises the probability of negative EMP by 60 to 100 percent (12 to 20 percentage points) | Checked in Lean against Tables 1 and 3. |
| GH-Q18 | 33 | Because large banks rarely cut dividends even during periods of weaker earnings, maintaining those payouts can increase the likelihood that distributions exceed contemporaneous earnings. | Dividend rigidity at large banks. |

## Lean results

| Name | Statement |
|---|---|
| `GordonHirtle.negEMP_iff_payout_ratio` | With positive earnings, EMP $<0$ exactly when the payout ratio exceeds one. |
| `GordonHirtle.negEMP_of_nonpos_earnings` | With earnings not positive, any positive payout gives EMP $<0$. |
| `GordonHirtle.equity_falls_of_negEMP` | Under Equation 1, with no issuance, no preferred actions and no other change, EMP $<0$ lowers equity capital. |
| `GordonHirtle.persistence_magnitudes` | Table 2's lag coefficients 0.097 and 0.208 are 46% and 99% of 0.21. |
| `GordonHirtle.equity_sd_magnitudes` | $0.0265\times0.910$ and $0.0265\times2.331$ lie in $(0.02,0.065)$, and are 11% and 29% of 0.21. |
| `GordonHirtle.shock_sd_magnitudes` | $0.0092\times13.313$ and $0.0092\times22.158$ lie in $(0.12,0.21)$, and are 58% and 97% of 0.21. |
| `GordonHirtle.control_ratio_needs_positive_earnings` | With negative earnings, EMP $<0$ while the payout ratio is not above one. |
| `GordonHirtle.control_lower_persistence_below_half` | $0.097/0.21<0.5$. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| GH-D1 | the copy read | No page numbers are printed; pages cited are PDF pages, the cover being page 1. | The DOI range 1-48 does not fix which PDF page is page 1. |
| GH-D2 | p. 28 | "50 to 100 percent" covers coefficients from 0.097 (46% of 0.21) to 0.208 (99%). | Table 2; Lean `persistence_magnitudes`. |
| GH-D3 | pp. 26, 35 | The share of banks both issuing and repurchasing is read off Chart 14 as 35 to 70 percent on p. 26 and 35 to 75 percent on p. 35. Neither figure is relied on. | The text gives two ranges for the same chart. |
| GH-D4 | p. 26 | "micro-founders" means micro-foundations. | Context. |
| GH-D5 | pp. 6, 46 | "Dittman et al. (2025)" is Dittmann et al. (2025). | The reference list. |
