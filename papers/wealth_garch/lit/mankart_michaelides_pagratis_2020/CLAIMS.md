# Claims: Mankart, Michaelides and Pagratis (2020)

Bib key `mankart2020`. Version read: the NBER Summer Institute 2016 draft dated May 2016, 57 numbered pages after a title and an abstract page. Evidence [F] for that version.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| MMP-Q1 | 1 | Banks raise equity capital only internally through retained earnings | No external equity in the model. |
| MMP-Q2 | 2 | we abstract from seasoned equity issuance. | The same, completed on p. 2. |
| MMP-Q3 | 1 | One key departure from Modigliani-Miller (MM) arises because banks are run by managers maximizing bank charter value | The objective. |
| MMP-Q4 | 18 | smooth dividends over time, as suggested by empirical evidence in Acharya, Le and Shin (2013). | The concave objective (9) makes dividends smooth. |
| MMP-Q5 | 18 | However, limited liability implies that if equity is too low the bank may choose not to issue new equity (i.e. negative dividend), therefore to violate its regulatory capital constraint and fail. | Failure instead of recapitalisation. |
| MMP-Q6 | 22 | Given the breadth and complexity of possible enforcement actions, we make the simplifying assumption that a bank fails if it is deemed | Breach means failure. |
| MMP-Q7 | 24 | underpredicts mean equity holdings | The model holds less equity than banks do. |
| MMP-Q8 | 24 | Mean equity/assets 0:065 0:072 0:067 0:099 | Table 5: model and data, large then small banks (MMP-D1). |
| MMP-Q9 | 24 | Mean dividends/equity 0:029 0:028 0:012 0:013 | Table 5: mean dividends are matched. |
| MMP-Q10 | 23 | CRRA 1:31 (0:0042) 1:89 (0:0056) | Table 4: curvature, large then small banks. |
| MMP-Q11 | 23 | Discount factor 0:975 (0:0041) 0:986 (0:0115) | Table 4. |
| MMP-Q12 | 31 | Since banks want to smooth dividends, they do not lower dividend payouts as much as | Dividends fall less than profits in recessions. |
| MMP-Q13 | 43 | tighter risk-weighted capital requirements reduce loan supply and increase bank failures because endogenous equity holdings increase by less than capital requirements. | The main counterfactual. |

## Lean results

| Name | Statement |
|---|---|
| `MankartMichaelidesPagratis.min_equity_iff` | With deposits one and no wholesale funding, $(1+E)/E\le\lambda_u$ exactly when $E\ge 1/(\lambda_u-1)$, the minimum equity of Figure 4. |
| `MankartMichaelidesPagratis.limits_to_ratios` | $1/0.06\approx16.66$, $1/0.03\approx33.33$, and the ranges $\lambda_u\in[25,41]$, $\lambda_w\in[13,20]$ are ratios of 4% to 2.44% and 7.7% to 5%. |
| `MankartMichaelidesPagratis.smoothing_gain` | For a strictly concave objective, two unequal dividends are worth less than their average paid twice. |
| `MankartMichaelidesPagratis.equity_underpredicted` | Table 5: model mean equity/assets is below the data for large (0.065 against 0.072) and small banks (0.067 against 0.099). |
| `MankartMichaelidesPagratis.control_min_equity_needs_limit_above_one` | With $\lambda_u=1$ no positive equity meets the limit. |
| `MankartMichaelidesPagratis.control_linear_objective_not_strictly_concave` | A linear objective is not strictly concave, so it gives no smoothing gain. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| MMP-D1 | the copy read | The text layer renders "fi" as "…", "ff" as "¤", "fl" as "‡", and the decimal point in Tables 2, 4 and 5 as ":". Quotations reproduce the text layer; table entries are read with ":" as ".". | The font encoding of the PDF. |
| MMP-D2 | the copy read | NBER Summer Institute draft of May 2016; the *Financial Management* 49(2):473-502 article was not read. The author accepted the working paper on 2026-10-12. | Only this version's pages are attributed. |
| MMP-D3 | Table 1 note, p. 8 | Medium banks are the 95th to 98th percentiles. | The text, pp. 6-7, and the Figure notes; the Table 1 note says 96th. |
| MMP-D4 | (15), p. 54 | Equity evolves as (7)-(8): the screening cost enters once, inside profits. | (15) subtracts $g_N$ outside profits as well, and refers to an unresolved equation "(??)". |
| MMP-D5 | references, p. 49 | Holod and Peek (2010) is JMCB 42(5):879-906. | The paper cites pp. 779-905; see lit/holod_peek_2010. |
