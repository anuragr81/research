# Claims: Craine and Martin (2017)

Bib key `crainemartin2017`. Version read: "Safe Capital Ratios for Bank Holding Companies", Roger Craine and Vance L. Martin, revised draft, July 2017 (Berkeley page of the first author); 22 PDF pages, printed page = PDF page. Read in full. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CM-Q1 | 1 | Book value capital ratios hardly move in response to severe economic distress. | Book capital barely moves; market capital does. |
| CM-Q2 | 2 | The safe capital ratios implied by FED stress tests are much less sensitive to a severe downturn than CM | Stress-test implied capital moves little; market-implied a lot. |
| CM-Q3 | 13 | We used 18 bank holding companies where the VLab data and our data overlap with the FED Stress tests (2015). | Their sample: 18 holding companies, DFAST 2015. |
| CM-Q4 | 14 | The standard deviations of the market based measures almost are four times as large as the book value measures. Book values don | About four times, as a dispersion across banks. |
| CM-Q5 | 14 | The first two columns give the standard deviations of the actual capital ratios around the sample mean | The four times is across banks at one date, not over time. |
| CM-Q6 | 14 | is close to the average deviation for the market value measures (columns 5 and 6). | The average required change is similar for book and market measures. |
| CM-Q7 | 15 | But none of the 18 bank holding companies book capital (leverage) ratios fall below 6% after 2008. | Book ratios held through the crisis. |
| CM-Q8 | 16 | Market value equity measures respond (and maybe overreact) to current and anticipated economic events. | Their conclusion on market measures. |
| CM-Q9 | 11 | The median SCR for Vlab is 15% and for CM is 13%. | Market-based safe capital ratios. |
| CM-Q10 | 12 | the crisis used by Vlab and CM which only specifies a 40% decline in equity values over two quarters. | Their market crisis differs from the stress scenario. |
| CM-Q11 | 14 | Precisely identifying the sources of the different results for the calculated Safe Capital Ratios is nearly impossible. | They do not reconcile the measures. |

## Lean results

| Name | Statement |
|---|---|
| `CraineMartin.scr_ge_k` | For $0<k<1$ and $0\le L<1$, $k\le k/(1-(1-k)L)$: the safe capital ratio is at least the buffer $k$. |
| `CraineMartin.ratio_above_four` | $0.0739/0.0175>4$: their Table III.2 dispersions. |
| `CraineMartin.control_scr_needs_loss_below_one` | With $L=1$ and $k=0$ the denominator vanishes. |
| `CraineMartin.control_ratio_not_eight` | The dispersion ratio is below 8. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| CM-D1 | pp. 13-14 | Their "four times" compares dispersions across 18 banks at one date (Table III.2); it is not a volatility over time, and the average required change agrees across measures. | CM-Q4, CM-Q5, CM-Q6. |
