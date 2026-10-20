# Claims: Federal Reserve (2013), Dodd-Frank Act Stress Test 2013

Bib key `fed2013dfast`. Source: "Dodd-Frank Act Stress Test 2013: Supervisory Stress Test Methodology and Results", Board of Governors of the Federal Reserve System, March 2013, the corrected version of 14 March 2013 (federalreserve.gov, dfast_2013_results_20130314.pdf); 74 PDF pages, printed page = PDF page - 6. Read: Table 2 and its notes (printed p. 17). Evidence [F] for that page.

## Quotations

Each row is quoted from the table's first three columns: the tier 1 common ratio actual, projected at the end of the horizon, and projected minimum, in percent.

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| DF13-Q1 | 17 | JPMorgan Chase & Co. 10.4 6.8 6.3 | JPMorgan Chase: tier 1 common ratio 10.4% actual, 6.8% ending, 6.3% minimum, severely adverse (Q3 2012 actual, minimum over Q4 2012 to Q4 2014); decline 4.1 points. |
| DF13-Q2 | 17 | Bank of America Corporation 11.4 6.9 6.8 | Bank of America: tier 1 common ratio 11.4% actual, 6.9% ending, 6.8% minimum, severely adverse (Q3 2012 actual, minimum over Q4 2012 to Q4 2014); decline 4.6 points. |
| DF13-Q3 | 17 | Citigroup Inc. 12.7 8.9 8.3 | Citigroup: tier 1 common ratio 12.7% actual, 8.9% ending, 8.3% minimum, severely adverse (Q3 2012 actual, minimum over Q4 2012 to Q4 2014); decline 4.4 points. |
| DF13-Q4 | 17 | Wells Fargo & Company 9.9 7.0 7.0 | Wells Fargo: tier 1 common ratio 9.9% actual, 7.0% ending, 7.0% minimum, severely adverse (Q3 2012 actual, minimum over Q4 2012 to Q4 2014); decline 2.9 points. |
| DF13-Q5 | 17 | U.S. Bancorp 9.0 8.3 8.3 | U.S. Bancorp: tier 1 common ratio 9.0% actual, 8.3% ending, 8.3% minimum, severely adverse (Q3 2012 actual, minimum over Q4 2012 to Q4 2014); decline 0.7 points. |
| DF13-Q6 | 17 | The PNC Financial Services Group, Inc. 9.5 8.7 8.7 | PNC: tier 1 common ratio 9.5% actual, 8.7% ending, 8.7% minimum, severely adverse (Q3 2012 actual, minimum over Q4 2012 to Q4 2014); decline 0.8 points. |
| DF13-Q7 | 17 | Capital One Financial Corporation 10.7 7.4 7.4 | Capital One: tier 1 common ratio 10.7% actual, 7.4% ending, 7.4% minimum, severely adverse (Q3 2012 actual, minimum over Q4 2012 to Q4 2014); decline 3.3 points. |
| DF13-Q8 | 17 | KeyCorp 11.3 8.0 8.0 | KeyCorp: tier 1 common ratio 11.3% actual, 8.0% ending, 8.0% minimum, severely adverse (Q3 2012 actual, minimum over Q4 2012 to Q4 2014); decline 3.3 points. |
| DF13-Q9 | 17 | The Bank of New York Mellon Corporation 13.3 15.9 13.2 | BNY Mellon: tier 1 common ratio 13.3% actual, 15.9% ending, 13.2% minimum, severely adverse (Q3 2012 actual, minimum over Q4 2012 to Q4 2014); decline 0.1 points. |
| DF13-Q10 | 17 | The minimum stressed ratios (%) are the lowest quarterly ratios from Q4 2012 to Q4 2014 under the severely adverse scenario. | The third number of each row is the minimum over the horizon. |

## Lean results

| Name | Statement |
|---|---|
| `StressTests.judged_sigma` | $D/(z\sqrt{9/4})=D/(1.5z)$: the volatility under which a 2.25-year decline $D$ is a $z$-quantile. |
| `StressTests.decline_of_rows` | When the minimum is at most the actual ratio the decline is non-negative. |
| `StressTests.control_horizon_not_quarters` | $\sqrt{9/4}\neq9/4$: the horizon enters through its square root. |
| `StressTests.control_decline_needs_order` | A minimum above the actual ratio would give a negative decline (BNY Mellon's ending ratio exceeds its actual one, so the minimum, not the ending ratio, is used). |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| DF13-D1 | p. 17 | The decline D is the actual ratio minus the projected minimum, in points of risk-weighted assets, under the Federal Reserve's own estimates with the capital actions the stress-testing rule prescribes. | The rows and the note. |
