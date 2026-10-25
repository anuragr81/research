# Claims: Lang and Menno (2026)

Bib key `langmenno2026`. Version read: ECB Working Paper Series No 3188, 2026 (no month printed); 51 PDF pages, printed pages 1 to 49 on PDF pages 2 to 50 (PDF page = printed page + 1). Read in full. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| LM-Q1 | 1 | We prove that even very low stigma costs induce large buffer usability constraints | A breachable buffer requirement binds in practice. |
| LM-Q2 | 8 | For tractability, we restrict the model to 2 periods. | Their model is two-period, not a continuous-time barrier model. |
| LM-Q3 | 12 | There is just a single stochastic process for loan impairments that drives the dynamics of the model, i.e. there are no idiosyncratic bank shocks. | One common shock: no cross-section of banks. |
| LM-Q4 | 9 | Second, we add a CBR to the model, which is greater or equal to the minimum capital requirement | The buffer requirement sits above the minimum. |
| LM-Q5 | 12 | Rcbr = 0.125 so that it equals the minimum capital requirement plus the capital conservation buffer (CCoB). | Their floor is minimum plus conservation buffer, the construction of our 10.5% (euro-area minimum 10%). |
| LM-Q6 | 12 | To illustrate the analytical insights that we derive with concrete examples, we calibrate our model to match key features of euro area bank data. | Euro-area calibration for illustration; not fitted to observed buffers. |
| LM-Q7 | 18 | Banks do not maintain voluntary capital buffers above the CBR | In their equilibrium the capital ratio sits at the buffer requirement: it acts as the floor, and nothing lies far above it. |
| LM-Q8 | 17 | For our benchmark model calibration (1% default cost) banks maintain voluntary capital buffers of 1pp to 2pp, and these voluntary capital buffers are pro-cyclical, | Without the buffer requirement, voluntary buffers of 1 to 2 points above the minimum. |
| LM-Q9 | 14 | Without an equity issuance constraint and costly bank liquidation, there are no reasons for banks to maintain voluntary capital buffers. | Buffers come from costly failure and costly issuance, as in Milne and Whalley. |
| LM-Q10 | 12 | In principle bank heterogeneity or mixed strategy equilibria could also be studied, but we leave this for future research. | Heterogeneity across banks is left open. |
| LM-Q11 | 30 | We showed that very low aggregate stigma costs of 0.5-3 bps are sufficient to induce banks to fulfill the capital buffer requirement in normal times. | Their conclusion. |
| LM-Q12 | 7 | we are the first to explicitly model capital buffer requirements that banks are allowed to breach | Their novelty claim. |

## Lean results

| Name | Statement |
|---|---|
| `LangMenno.cbr_is_min_plus_ccob` | $0.10+0.025=0.125$ (LM-Q5). |
| `LangMenno.us_floor_same_construction` | $0.08+0.025=0.105$: the same construction on the US 8% minimum gives our R. |
| `LangMenno.control_cbr_needs_buffer` | Without the buffer the floor stays at the minimum. |
| `LangMenno.control_us_floor_not_ecb` | The US and euro-area floors differ. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| LM-D1 | pp. 8, 12, 18 | A two-period representative-bank model with a common shock; the buffer requirement binds in equilibrium and no bank holds capital far above it; calibrated for illustration, never compared with observed buffers. | LM-Q2, LM-Q3, LM-Q6, LM-Q7, LM-Q10. |
