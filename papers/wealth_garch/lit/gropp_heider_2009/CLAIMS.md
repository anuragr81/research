# Claims: Gropp and Heider (2009)

Bib key `groppheider2009`. Version read: ECB Working Paper Series No 1096, September 2009 (published as Gropp and Heider 2010, Review of Finance 14, 587-622, not read); PDF pages 1 to 51 are the paper (PDF page 52 is another working paper's cover), printed pages 3 to 50 on PDF pages 4 to 51 (PDF page = printed page + 1). Read in full. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| GH09-Q1 | 4 | The paper shows that mispriced deposit insurance and capital regulation were of second order importance in determining the capital structure of large U.S. and European banks during 1991 to 2004. | Their headline: regulation second order. |
| GH09-Q2 | 4 | except for banks whose capital ratio is close to the regulatory minimum. | The exception near the minimum. |
| GH09-Q3 | 4 | banks’ leverage converges to bank specific, time invariant targets. | Bank-specific targets. |
| GH09-Q4 | 11 | We focus only on the 100 largest publicly traded commercial banks and bank-holding companies in the United States | Large listed banks only: no unlisted or small banks. |
| GH09-Q5 | 7 | Banks’ regulatory capital ratios are not uniformly close to the minimum of 4% specified in the Basel Capital Accord (Basel I). | Their minimum is 4% Tier 1. |
| GH09-Q6 | 25 | We use two definition of the Close dummy: less than 5% or less than 6% of Tier 1 capital in the previous year. | "Close to the minimum" is Tier 1 below 6%, the well-capitalised Tier 1 line of the period (OR-Q14); the 10% total line is never used. |
| GH09-Q7 | 25 | There are no banks that fell below the regulatory minimum in any year during our sample period. | No observations at the floor. |
| GH09-Q8 | 26 | Overall, we conclude that capital requirements do introduce a non-linearity in the behaviour of banks when capital falls to levels very close to the regulatory minimum. | Regulation matters near the line. |
| GH09-Q9 | 15 | Banks’ leverage depends positively on size and collateral, and negatively on the market-to-book ratio, profits and dividends. | Larger banks hold less capital, as in our data. |
| GH09-Q10 | 16 | Only the coefficient on bank size is in line with the regulatory view if one argues that larger banks are better known to the market and find it easier to issue equity. | Their reading of size: access to capital, which results_access.md does not support. |
| GH09-Q11 | 44 | The loss of significance on size is linked to US banks being much smaller than EU banks. | In their US subsample size is not significant for market leverage. |
| GH09-Q12 | 21 | The fixed effect accounts for 92% of book leverage and for 76% of market leverage. | Bank fixed effects dominate. |
| GH09-Q13 | 29 | These targets do not seem to be determined by standard corporate finance variables or by regulation, | Targets unexplained. |
| GH09-Q14 | 22 | Adding the standard determinants of leverage increases the speed of adjustment by 1.8 percentage points to 46.8%. | Speed with bank fixed effects. |
| GH09-Q15 | 8 | the high levels of banks’ discretionary capital observed do not appear to be explained by buffers that banks hold to insure against falling below the minimum capital requirement. | Their rejection of the buffer view, tested against the Basel minimum. |
| GH09-Q16 | 29 | Our evidence does not support the view that buffers in excess of the regulatory minimum are an explanation for the variation of bank capital. | Their conclusion. |

## Lean results

| Name | Statement |
|---|---|
| `GroppHeider.speed_with_determinants` | $45+1.8=46.8$ (GH09-Q14, with the 45% fixed-effects speed of p. 22). |
| `GroppHeider.close_is_wellcap_tier1` | Their 6% Tier 1 cutoff equals the well-capitalised Tier 1 line (OR-Q14) and exceeds the 4% minimum by 2 points. |
| `GroppHeider.control_speed_needs_fe` | The pooled speed 9% plus 1.8 is not 46.8. |
| `GroppHeider.control_close_not_total` | The 6% Tier 1 cutoff is not the 10% total line. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| GH09-D1 | pp. 7, 25 | Their test of the buffer view sets the line at the Basel I Tier 1 minimum (4%) or just above (5%, 6%); the US 10% total capital line of PCA is never used, which is Orozco and Rubio's reading (OR-Q3). | GH09-Q5, GH09-Q6, GH09-Q15. |
| GH09-D2 | p. 22 | The pooled speed is 9% and the fixed-effects speed 45%; GH09-Q14 is the latter plus 1.8. | Text of p. 22. |
