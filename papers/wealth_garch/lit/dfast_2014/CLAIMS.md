# Claims: Federal Reserve (2014), Dodd-Frank Act Stress Test 2014

Bib key `fed2014dfast`. Source: "Dodd-Frank Act Stress Test 2014: Supervisory Stress Test Methodology and Results", Board of Governors of the Federal Reserve System, March 2014 (federalreserve.gov, bcreg20140320a1.pdf); 168 PDF pages, printed page = PDF page - 6. Read: Table 3 and its notes (printed p. 28). Evidence [F] for that page.

## Quotations

Each row is quoted from the table's first three columns: the tier 1 common ratio actual, projected at the end of the horizon, and projected minimum, in percent.

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| DF14-Q1 | 28 | JPMorgan Chase & Co. 10.5 6.7 6.3 | JPMorgan Chase: tier 1 common ratio 10.5% actual, 6.7% ending, 6.3% minimum, severely adverse (Q3 2013 actual, minimum over Q4 2013 to Q4 2015); decline 4.2 points. |
| DF14-Q2 | 28 | Bank of America Corporation 11.1 6.0 5.9 | Bank of America: tier 1 common ratio 11.1% actual, 6.0% ending, 5.9% minimum, severely adverse (Q3 2013 actual, minimum over Q4 2013 to Q4 2015); decline 5.2 points. |
| DF14-Q3 | 28 | Citigroup Inc. 12.7 7.2 7.2 | Citigroup: tier 1 common ratio 12.7% actual, 7.2% ending, 7.2% minimum, severely adverse (Q3 2013 actual, minimum over Q4 2013 to Q4 2015); decline 5.5 points. |
| DF14-Q4 | 28 | Wells Fargo & Company 10.6 8.2 8.2 | Wells Fargo: tier 1 common ratio 10.6% actual, 8.2% ending, 8.2% minimum, severely adverse (Q3 2013 actual, minimum over Q4 2013 to Q4 2015); decline 2.4 points. |
| DF14-Q5 | 28 | U.S. Bancorp 9.3 8.3 8.2 | U.S. Bancorp: tier 1 common ratio 9.3% actual, 8.3% ending, 8.2% minimum, severely adverse (Q3 2013 actual, minimum over Q4 2013 to Q4 2015); decline 1.1 points. |
| DF14-Q6 | 28 | The PNC Financial Services Group, Inc. 10.3 9.0 9.0 | PNC: tier 1 common ratio 10.3% actual, 9.0% ending, 9.0% minimum, severely adverse (Q3 2013 actual, minimum over Q4 2013 to Q4 2015); decline 1.3 points. |
| DF14-Q7 | 28 | Capital One Financial Corporation 12.7 7.8 7.8 | Capital One: tier 1 common ratio 12.7% actual, 7.8% ending, 7.8% minimum, severely adverse (Q3 2013 actual, minimum over Q4 2013 to Q4 2015); decline 4.9 points. |
| DF14-Q8 | 28 | KeyCorp 11.2 9.3 9.2 | KeyCorp: tier 1 common ratio 11.2% actual, 9.3% ending, 9.2% minimum, severely adverse (Q3 2013 actual, minimum over Q4 2013 to Q4 2015); decline 2.0 points. |
| DF14-Q9 | 28 | The Bank of New York Mellon Corporation 14.1 16.1 13.1 | BNY Mellon: tier 1 common ratio 14.1% actual, 16.1% ending, 13.1% minimum, severely adverse (Q3 2013 actual, minimum over Q4 2013 to Q4 2015); decline 1.0 points. |
| DF14-Q10 | 28 | Huntington Bancshares Incorporated 10.9 7.4 7.4 | Huntington: tier 1 common ratio 10.9% actual, 7.4% ending, 7.4% minimum, severely adverse (Q3 2013 actual, minimum over Q4 2013 to Q4 2015); decline 3.5 points. |
| DF14-Q11 | 28 | Zions Bancorporation 10.5 3.6 3.6 | Zions: tier 1 common ratio 10.5% actual, 3.6% ending, 3.6% minimum, severely adverse (Q3 2013 actual, minimum over Q4 2013 to Q4 2015); decline 6.9 points. |
| DF14-Q12 | 28 | The minimum capital ratio presented is for the period Q4 2013 to Q4 2015. | The third number of each row is the minimum over the horizon. |

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
| DF14-D1 | p. 28 | The decline D is the actual ratio minus the projected minimum, in points of risk-weighted assets, under the Federal Reserve's own estimates with the capital actions the stress-testing rule prescribes. | The rows and the note. |
