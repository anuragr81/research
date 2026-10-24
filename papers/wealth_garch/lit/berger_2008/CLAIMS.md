# Claims: Berger, DeYoung, Flannery, Lee and Öztekin (2008)

Bib key `berger2008`. Version read: Federal Reserve Bank of Kansas City Research Working Paper 08-01, 37 numbered pages after a cover and an abstract page; the abstract page is cited as p. 0. Evidence [F] for that version.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| BDF-Q1 | 1 | Since the early 1990s, however, the largest U.S. banking organizations have maintained ratios well in excess of regulatory minimums. | Buffers far above the minimum, as E1 finds. |
| BDF-Q2 | 2 | Since dividend payments tend to be sticky, capital ratios may rise almost automatically with high earnings. | Sticky dividends. |
| BDF-Q3 | 7 | Peura and Keppo (2006) model the bank capital decision as a dynamic tradeoff between the opportunity cost of equity and the loss of franchise value. | The empirical literature names Peura and Keppo as the dynamic model. |
| BDF-Q4 | 12 | We can thus combine (1) with a partial adjustment model specifying that the firm closes a constant proportion of the gap between its current capital ratio and its target capital ratio | Partial adjustment toward a target, equation (2). Checked in Lean. |
| BDF-Q5 | 16 | Thus, DNK corresponds to what the capital ratio would be if the bank "did nothing": kept dividend payments constant and let the remaining cash flow accrue to capital. | The do-nothing benchmark. |
| BDF-Q6 | 13 | Low estimated values for λ will indicate that BHCs are passive managers of their capital ratios | Reading of λ. |
| BDF-Q7 | 13 | The speed of capital adjustment may be slower for banks that find themselves above their target capital ratios, but faster for banks that find themselves below their targets. | Asymmetric adjustment, hypothesised. |
| BDF-Q8 | 21 | Firms below the ADEQUATE capital level adjust particularly fast, roughly 33 and 45 percentage points faster for the UNDERCAPITALIZED and SIG-UNDERCAP BHCs, respectively. | Adjustment speed rises below the regulatory thresholds (Table 3, Panel A). |
| BDF-Q9 | 21 | BHCs with BADBOPEC ratings adjust toward their capital targets more slowly than BHCs with better regulatory safety and soundness ratings, presumably because they are unable to raise new capital effectively. | Troubled banks adjust slowly. |
| BDF-Q10 | 19 | Between 94 and 98 percent of the sample observations fall into the various well-capitalized categories. | Few observations near the minimum. |
| BDF-Q11 | 22 | On average, these variable adjustment speeds averaged between 28% and 41%, substantially slower than the constrained adjustment speeds of 50% to 61% (i.e., 1-(1-λ)) estimated in Step 1. | Annual speeds (BDF-D2). |
| BDF-Q12 | 23 | In other words, BHCs held less capital than they desired on average during our sample period | Targets above actual ratios. |
| BDF-Q13 | 24 | In twelve of the fifteen sample years banks either partially or fully offset new share issuance with existing share repurchases | Active capital management by repurchases. |
| BDF-Q14 | 25 | actively offset losses remains an open question, and cannot be evaluated using data from our regularly profitable sample period. | The loss side was not observed. |
| BDF-Q15 | 25 | suggest that active capital management may be asymmetric. While sustained periods of profitability allow managers to steer toward capital targets with gradual adjustments, large unexpected losses that plunge banks toward their regulatory capital minimums can require lumpy, one-time adjustments. | The authors' conjecture: gradual above, lumpy near the minimum (n. 30). |
| BDF-Q16 | 28 | Mean: 98% 69% -67% 143% 82% -125% 169% 88% -157% | Table 1 means; checked in Lean. |
| BDF-Q17 | 31 | Mean fitted target .0830 .0864 | Table 3, Panel A. |
| BDF-Q18 | 32 | Mean fitted target .1302 .1440 | Table 3, Panel B. |
| BDF-Q19 | 33 | Mean fitted target .1500 .1694 | Table 3, Panel C. |
| BDF-Q20 | 0 | choose target capital levels substantially above well-capitalized regulatory minima | Targets, not accidents: the buffer is chosen (abstract page, printed 0). |
| BDF-Q21 | 0 | these targets increase with BHC risk but decrease with BHC size | The buffer scales with risk (mechanism 1, and the scale of the gap in results_mechanisms.md). |
| BDF-Q22 | 0 | based on their risk exposures and their counterparties' sensitivities to default risk | Counterparties price default risk (ledger A7). |
| BDF-Q23 | 0 | slower (ceteris paribus) for BHCs under severe regulatory pressure | The supervisor's pressure slows adjustment (ledger A9). |
| BDF-Q24 | 7 | Finally, bank capital ratios tend to be inversely related to asset size. This might reflect greater diversification, scale economies in risk management, and/or lower expected cost of raising new equity on short notice. | The size gradient and the explanations they offer, among them a lower cost of raising equity for larger banks; offered, not measured. |
| BDF-Q25 | 16 | Larger banking organizations are likely to hold relatively less capital due to greater diversification, scale economies in risk management, greater ability to raise equity on short notice | The same list as the hypothesis behind their size variables. |
| BDF-Q26 | 23 | As expected, larger BHCs hold significantly lower capital ratios. | The size gradient in their Step 3 targets. |
| BDF-Q27 | 23 | Because the effects of BHC size are like to be incompletely captured by our explanatory variables, this finding could also indicate the existence of economies of scale at the very largest BHCs | They do not separate the explanations (n. 27). |
| BDF-Q28 | 23 | we interpret this either as weak evidence that BHCs with access to inexpensive external capital can afford to hold less capital | Cheap external capital lowers the target: the issue-cost channel, weakly supported (MKTBOOK). |
| BDF-Q29 | 21 | Banks with poor capital market access (MISSING = 1) might be unable to adjust quickly to their target capital levels. | Market access proxied by having no bond rating; it enters the speed of adjustment, not the target. |

## Lean results

| Name | Statement |
|---|---|
| `BergerEtAl.partial_adjustment_solution` | From (2), $k=\lambda k^*+(1-\lambda)\,\text{DNK}$. |
| `BergerEtAl.gap_closes` | The gap after adjustment is $(1-\lambda)$ times the gap before. |
| `BergerEtAl.between` | For $\lambda\in[0,1]$ and DNK $\le k^*$, $k$ lies between DNK and $k^*$. |
| `BergerEtAl.table1_rows_within_rounding` | Every row of Table 1 sums to 100% within one point. |
| `BergerEtAl.table1_means` | The mean rows sum to 100%, and $98+69=167$ (p. 24). |
| `BergerEtAl.footnote28` | $-0.1985+0.0109\times16.87=-0.014617$, the 146 basis points of p. 23. |
| `BergerEtAl.footnote29` | The 129 and 3 basis point calculations of n. 29. |
| `BergerEtAl.step3_minus_step1` | Step 3 targets exceed Step 1 targets by 34, 138 and 194 basis points (p. 24). |
| `BergerEtAl.step1_speed_range` | The Step 1 speeds 0.4864 and 0.6024 (BDF-D2). |
| `BergerEtAl.control_overshoot_without_speed_bound` | With $\lambda>1$ the ratio overshoots the target. |
| `BergerEtAl.control_step1_lower_below_half` | $0.4864<0.5$. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| BDF-D1 | the copy read | Kansas City Fed RWP 08-01; the JFSR 34(2-3):123-149 article was not read. The author accepted the working paper on 2026-10-12. | Only this version's pages are attributed. |
| BDF-D2 | p. 22 | The Step 1 speeds of Table 3 are 0.4864 to 0.6024, so "50% to 61%" is 49% to 60%. | Table 3, Panels A to C; Lean `step1_speed_range`. |
| BDF-D3 | Table 1, p. 28 | Rows sum to 100% within one point. | Rounding of each share; Lean `table1_rows_within_rounding`. |
| BDF-D4 | Table 2, p. 29 | BBB is a BBB rating and JUNK a rating below BBB, as on pp. 17-18. | Table 2's definitions of the two dummies are interchanged. |
