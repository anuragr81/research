# Claims: Peura and Jokivuolle (2003)

Bib key `peurajokivuolle2003`. Version read: Bank of Finland Discussion Papers 4/2003, "Simulation-based stress testing of banks' regulatory capital adequacy" (published as Journal of Banking and Finance 28(8), 2004, 1801-1824, not read; the author chose the working paper on 29 October 2026). 45 PDF pages; printed pages 3 to 41 on PDF pages 6 to 44 (PDF page = printed page + 3). Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| PJ-Q1 | 16 | We simulate shocks to book capital and to book capital requirements, rather than to the market value of bank equity. | Book capital, credit risk only. |
| PJ-Q2 | 20 | Given a bank’s portfolio and its capital ratio, there is an implied value of a which makes the capital ratio solved from the model equal to the observed capital ratio. | They invert a buffer model on observed ratios for a confidence level; the enhancement paper inverts for a risk scale with the decision rule fixed. |
| PJ-Q3 | 31 | We take the observed median capital ratio of US banks, 11.9%, from Table 1 as the ratio against which we match our model ratios. | One target, the median of 33 large US banks, 1997-2001. |
| PJ-Q4 | 32 | If the average US bank used a ‘recession’ scenario in its capital adequacy analysis, the average (or median) bank capital ratio of 11.9% were consistent with a confidence level of approximately 99.3%. | Implied confidence, one per scenario (`PeuraJokivuolle.breach_probabilities`). |
| PJ-Q5 | 32 | and if the average bank used a ‘prolonged recession’ scenario, the implied confidence level would be 97.2%. | The implied confidence falls as the assumed scenario worsens. |
| PJ-Q6 | 32 | If eg the underlying asset correlations were increased, the value of a which would produce the empirically observed capital ratios would decline. | Risk and confidence are not separately identified from one ratio. |
| PJ-Q7 | 20 | In our framework a is an exogenous parameter. | No optimisation; a one-rule simulation. |
| PJ-Q8 | 39 | Moreover, we have assumed away dividend payments and the (perhaps costly) option to issue new capital. | No dividend or issuance control. |
| PJ-Q9 | 8 | This is not an easy task, however, because most of the existing theories are so stylised that it is not at all clear how to eg represent different banking portfolios in these theories. | They decline to test the dynamic buffer theories. |
| PJ-Q10 | 36 | The obvious shortcoming of such an analysis is the absence of any likelihoods attached to the chosen rating transition scenario itself. | A deterministic stress scenario carries no probability; any quantile given to it is assumed (the z grid of M4). |
| PJ-Q11 | 26 | Average Unconditional                 0.00     0.69   2.41 | Table 7, the average portfolio's one-year net loss, mean, SD and 99th percentile: the 99th percentile is about 3.5 SD out, not 2.33 (`PeuraJokivuolle.fat_tail`). |
| PJ-Q12 | 27 | Third, as for the IRB regime, the distribution of credit losses appears more ‘long-tailed’ than the distribution of minimum capital requirements. | Credit losses are long-tailed. |

## Lean results

| Name | Statement |
|---|---|
| `PeuraJokivuolle.breach_probabilities` | $1-0.993=0.007$, $1-0.984=0.016$, $1-0.972=0.028$. |
| `PeuraJokivuolle.fat_tail` | $3.4<2.41/0.69<3.5$ and $1.16/0.32>3.6$. |
| `PeuraJokivuolle.control_not_normal_tail` | $2.326<2.41/0.69$: the tail is wider than a normal one. |
| `PeuraJokivuolle.control_breach_order` | $0.007<0.028$. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| PJ-D1 | throughout | The letter printed as "a" in the text layer is the confidence level alpha. | Extraction artefact. |
| PJ-D2 | p. 32 | The breach probabilities over 2.5 years with quarterly monitoring are one less the confidence levels. | Their definition, p. 18. |
