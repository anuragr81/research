# Claims: Baron (2020)

Bib key `baron2020`. Version read: the accepted manuscript dated 13 January 2020, "Forthcoming at Review of Financial Studies", with its Online Appendix, 127 PDF pages. Evidence [F] for the text; appendix tables read through their captions. Pages are PDF pages (BAR-D1).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| BAR-Q1 | 2 | This paper finds that for large U.S. commercial banks over the period 1980–2012, banks raise and retain less common equity during credit expansions, even though more equity might help banks to better absorb subsequent shocks. | The main finding: countercyclical issuance and retention at large banks. |
| BAR-Q2 | 8 | the bank is forced to engage in a costly recapitalization, forcing it to increase its equity to this minimum level Φ. | In the model, a forced recapitalization tops up to the minimum, not to a target above it. |
| BAR-Q3 | 65 | up to minimal capital requirements and pay cost c(Φ*A – E), which is proportional to the new equity raised as part of the forced recapitalization. | The recapitalization cost is proportional, with no fixed part. |
| BAR-Q4 | 9 | Note that the countercyclical pattern is, in part, driven by required recapitalization during the bust to meet minimum regulatory capital requirements – this feature is built into the model by assumption. | The author's own limit on the model. |
| BAR-Q5 | 4 | I also provide evidence that smaller banks raise equity often and in relatively large amounts | Issuance is frequent and large at smaller banks. |
| BAR-Q6 | 24 | According to Table 4, banks of size #1-5 by assets issue equity infrequently (1.9% of bank-quarters). However, smaller banks do so more frequently, with the rate monotonically increasing in bank | Table 4 (BAR-D2). |
| BAR-Q7 | 25 | size, up to a maximum of 10.4% of bank-quarters for banks of size #101–200. | Table 4. |
| BAR-Q8 | 57 | #101-200 by assets 10.4% 23.3% 5,527 10.6% 22.5% 3,943 | Table 4 row: frequency and average size of issuances above 5% of book equity. |
| BAR-Q9 | 22 | net income minus payouts (i.e., changes in retained earnings) is countercyclical for the largest U.S. commercial banks, meaning that banks increase their payouts faster than their income | Payouts outpace income in expansions. |
| BAR-Q10 | 6 | Specifically, I find that when the fraction of guaranteed debt decreases by 10 percentage points, cumulative equity issuance over the subsequent credit cycle increases by 23.36 percentage points of book equity, and retained earnings increase by 13.57 percentage | Checked in Lean against Table 8. |
| BAR-Q11 | 20 | an indicator variable that equals 1 (and 0 otherwise) when the previous year's common equity to assets ratio is less than 5% | Their undercapitalisation control. |
| BAR-Q12 | 27 | Column 2 shows that a 1-standard-deviation increase in credit expansion is associated with a statistically significant 1.7-percentage-point decrease in net equity issuance minus dividends (t = -6.694) and an additional -3.8-percentage-point decrease (t = -3.384) when there is a high expectation of government support | Table 5: expected government support deepens the countercyclicality. |
| BAR-Q13 | 5 | I find a 28-basis-point (bp) drop in marginal debt funding costs subsequent to each equity issuance | Creditor market discipline, absent guarantees. |

## Lean results

| Name | Statement |
|---|---|
| `Baron.a8_factor` | The first-order condition (A8), with $k=\Phi/(1-\Phi)$, equals $\bar A/(2\sigma)$ times $\gamma(1-\sigma)+2\sigma\delta-V/\bar A-\bar L(2ck^2+\gamma)$. |
| `Baron.a8_iff_a9` | With a positive denominator, (A8) holds exactly at (A9). |
| `Baron.a9_increasing_in_delta` | The debt ratio (A9) rises with the funding benefit $\delta$ (p. 69). |
| `Baron.a9_decreasing_in_V` | It falls with franchise value $V$ (p. 69). |
| `Baron.landesbank_magnitudes` | Table 8 coefficients 2.336 and 1.357 times a 10-point fall in guaranteed debt give 23.36 and 13.57 points (p. 6). |
| `Baron.table4_size_pattern` | Table 4's frequencies peak at #101-200 and amounts at #201-500, and neither rises monotonically with rank (BAR-D2). |
| `Baron.control_zero_denominator_leaves_a8_unsolved` | With $2ck^2+\gamma=0$, (A9) does not solve (A8). |
| `Baron.control_landesbank_coefficients_not_swapped` | The retained-earnings coefficient does not give the issuance magnitude. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| BAR-D1 | the copy read | Accepted manuscript of 13 January 2020 (SSRN 2902505); the published RFS 33(9):4186-4230 was not read. Pages are PDF pages: the paper (pp. 1-62) and the Online Appendix (pp. 63-127) number separately. | Only this version's pages are attributed. |
| BAR-D2 | pp. 24-25 | Issuance frequency rises from #1-5 to #21-50, dips at #51-100 (7.1% against 7.6%), peaks at #101-200 and then falls. | Table 4, p. 57; the text says "monotonically increasing". |
| BAR-D3 | Table 4, p. 57 | The printed N is the number of bank-quarters in each size group. | The caption defines N as the number of issuance quarters; the text layer also carries unprinted counts equal to frequency times N. Neither count is relied on. |
| BAR-D4 | Table 1, p. 52 | Panel A is the 2005 cross-section and Panel B the 1980-2012 time series. | The panel headings and pp. 17-18; the caption interchanges them. |
