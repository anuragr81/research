# Claims: Fries, Mella-Barral and Perraudin (1997)

Bib key `fries1997`. Version read: the published article, Journal of Banking & Finance 21(4), pp. 441-468; 28 PDF pages, printed page = PDF page + 440. Read in full; the country table on p. 443 and the figure pages are garbled in the text layer and are read for orientation only. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| FMP-Q1 | 444 | the willingness of equity-holders to inject capital constrains banking policy | Equity holders inject capital to keep the bank open and stop when it no longer pays, so closure is theirs to choose. |
| FMP-Q2 | 446 | we allow cash disbursements to shareholders to become negative in some states of the world | Injections are continuous, a negative payout, with no fixed cost. |
| FMP-Q3 | 446 | the important assumption that regulators cannot directly subsidize banks to keep them operating | Section 2 holds without subsidies; Section 3 adds them. |
| FMP-Q4 | 448 | one may exploit the homogeneity of the differential equation and the boundary conditions | Values per unit of deposits, the reduction the theory paper keeps by charging the fixed issuance cost per unit of liabilities (L4). |
| FMP-Q5 | 453 | smooth pasting is a necessary condition for an optimal stopping rule | The equity holders' closure point satisfies smooth pasting. |
| FMP-Q6 | 459 | There exists a moral hazard problem if the bank equity value is | Their Definition 1: moral hazard is equity value rising in volatility. |
| FMP-Q7 | 460 | Late closure creates an option value for equity-holders which makes their claims convex in the state-variable. | The convexity near closure comes from limited liability. |

## Lean results

| Name | Statement |
|---|---|
| `FriesEtAl.closure_factor_in_unit` | For $A<0$, $0<-A/(1-A)<1$: the equity holders close the bank while its net assets are still positive (p. 453). |
| `FriesEtAl.linear_no_gain_from_noise` | A symmetric shock leaves a value linear in the state unchanged. |
| `FriesEtAl.convex_gains_from_noise` | A symmetric shock $\pm e$ raises the average of $k^2$ by $e^2$. |
| `FriesEtAl.control_factor_needs_negative_root` | With $A=1/2$ the factor is $-1$. |
| `FriesEtAl.control_gain_needs_noise` | With no shock the convex value does not gain. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| FMP-D1 | pp. 446 to 453 | The state is net cash flow over deposits. Payouts may be negative, so equity holders inject capital continuously, and they close the bank at the point that maximises equity, below zero net assets, by smooth pasting. There is no fixed cost of injecting and no choice of risk. | FMP-Q1, FMP-Q2, FMP-Q5, `closure_factor_in_unit`. |
| FMP-D2 | pp. 459 to 460 | Their moral hazard is the convexity of equity near a late closure. Risk is not chosen in their model; Definition 1 asks whether equity would gain from more volatility. | FMP-Q6, FMP-Q7. |
| FMP-D3 | throughout | The text layer is an OCR of the scan with errors in symbols (for example 3' for gamma). Quotations are taken only from clean text. | Checked page by page. |
