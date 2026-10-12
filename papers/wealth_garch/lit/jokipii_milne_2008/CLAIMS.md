# Claims: Jokipii and Milne (2008)

Bib key `jokipii2008`. Version read: the published article, *Journal of Banking & Finance* 32:1440-1451. Evidence [F]. Pages are checked in reading order (JM-D1).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| JM-Q1 | 1445 | We do not investigate the possibility of asymmetries in these costs, since it may be easier to reduce capital, e.g. by paying dividends or buying back equity, than it is to increase capital. | The asymmetry of adjustment costs, named and left untested (n. 10). |
| JM-Q2 | 1441 | Banks may also hold a buffer of capital as a protection against the violation of the regulatory minimum requirements (Marcus, 1984; Milne and Whalley, 2001; Milne, 2004). | The buffer motive our model formalises. |
| JM-Q3 | 1440 | In practice, well-functioning banks hold capital well in excess of the minimum requirements | Buffers above the minimum. |
| JM-Q4 | 1447 | corresponding to a rate of adjustment towards desired capital of around 66% per annum | The estimated speed (JM-D8). |
| JM-Q5 | 1441 | In almost all cases we find a fairly slow speed of adjustment towards desired capital buffers. | The authors' wording of the speed (JM-D8). |
| JM-Q6 | 1441 | Total capital buffers (Tier 1 plus Tier 2) of banks within the EU15 vary from 1.87% of riskweighted assets in Portugal to 4.79% in Finland with an average across the EU15 of 2.93%. | Table 1 (JM-D2). |
| JM-Q7 | 1441 | ranging between 2.64% in Cyprus and 6.99% in Malta | Table 1, accession countries (JM-D3). |
| JM-Q8 | 1450 | For co-operative banks and smaller commercial banks the most likely reason is capital market frictions. Small banks find it relatively costly to raise new equity capital while co-operative banks are unable to make such issues at all. | Issuance cost shapes buffer dynamics. |
| JM-Q9 | 1450 | Larger banks, notably the commercial and savings banks, have in our sample period increased capital buffers in the economic downturn. | Negative co-movement for large banks. |
| JM-Q10 | 1441 | H1(a) Capital buffers co-move positively with the business cycle | The hypothesis labels (JM-D7). |
| JM-Q11 | 1446 | we find a negative significant relationship between the capital buffer and each of the cycle variables, consistent with our H1(a) hypothesis. | The labels as used (JM-D7). |
| JM-Q12 | 1443 | In total, 468 banks are included in the sample | Sample (JM-D5). |
| JM-Q13 | 1450 | We build an unbalanced panel of 486 banks | Sample, as restated (JM-D5). |

## Lean results

| Name | Statement |
|---|---|
| `JokipiiMilne.partial_adjustment` | From (1a), $\text{BUF}_t=d\,\text{BUF}^*+(1-d)\text{BUF}_{t-1}$: the lag coefficient is $1-d$. |
| `JokipiiMilne.rate_66_is_lag_034` | An adjustment rate of 66% is a lag coefficient of 0.34. |
| `JokipiiMilne.eu15_average` | 2.93 is the mean of the fifteen country averages and of the eight yearly EU15 values, to rounding. |
| `JokipiiMilne.ram10_average_is_over_years` | 5.14 is the mean of the yearly RAM10 values, not of the ten country averages (JM-D4). |
| `JokipiiMilne.eu15_extremes` | The EU15 country averages lie in $[1.46,4.79]$, and the UK's 1.46 is below Portugal's 1.87 (JM-D2). |
| `JokipiiMilne.ram10_extremes` | The RAM10 country averages lie in $[2.64,9.18]$, and Slovakia's 9.18 exceeds Malta's 6.99 (JM-D3). |
| `JokipiiMilne.control_rate_is_not_lag` | A 66% rate is not a lag coefficient of 0.66. |
| `JokipiiMilne.control_eu15_average_not_303` | The fifteen country averages do not average 3.03. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| JM-D1 | the copy read | Published version from academia.edu with a text layer; two columns, so quotations are checked in reading order (`layout` off). The layer prints "riskweighted" without a hyphen. Table 1 was read on the page image. | The column layout. |
| JM-D2 | p. 1441 | The smallest EU15 average in Table 1 is the UK's (1.46), not Portugal's (1.87). | Table 1, p. 1442; Lean `eu15_extremes`. |
| JM-D3 | p. 1441 | The largest RAM10 average in Table 1 is Slovakia's (9.18, from 2002-2004 only), not Malta's (6.99). | Table 1; Lean `ram10_extremes`. |
| JM-D4 | Table 1 | Composite averages are means over years of the yearly composite values. | Lean `eu15_average`, `ram10_average_is_over_years`. |
| JM-D5 | pp. 1443, 1450 | The sample is 468 banks. | Table 3 totals 468; p. 1450 says 486. |
| JM-D6 | pp. 1441, 1443 | The accession countries joined the EU in May 2004. | pp. 1441 and 1450; p. 1443 says 2005. |
| JM-D7 | pp. 1446-1447 | The labels H1(a) and H1(b) are interchanged in the results; the results are read from their words (negative co-movement for EU25, EU15, EA and DK-SE-UK, positive for RAM10). | H1(a) is defined as positive co-movement, p. 1441. |
| JM-D8 | pp. 1441, 1447 | The estimated speed is around 66% a year. | p. 1441 calls adjustment "fairly slow"; p. 1447 gives the number. |
