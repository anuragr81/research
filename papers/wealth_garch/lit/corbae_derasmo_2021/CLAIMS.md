# Claims: Corbae and D'Erasmo (2021)

Bib key `corbaederasmo2021`. Version read: Federal Reserve Bank of Philadelphia Working Paper 21-24, June 2021; the Econometrica article (2021, 89(6)) was not read. 74 PDF pages; printed pages 1 to 50 on PDF pages 3 to 52 (PDF page = printed page + 2), an online appendix after. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CD-Q1 | 2 | Similar to the consumer's problem, banks with a higher variance of funding shocks (i.e. the small banks) will endogenously hold a larger buffer of equity than more diversified big banks, again consistent with U.S. data. | Their size mechanism, funding volatility, a channel distinct from earnings volatility. |
| CD-Q2 | 22 | Our calibration strategy involves a set of parameters taken directly from the data and a second set using Simulated Method of Moments (SMM). | A dynamic model taken to US data by estimation. |
| CD-Q3 | 27 | One of the main counterfactuals in the paper evaluates changes in capital regulation, so we target the risk-weighted Tier 1 capital ratio by bank size (equity to loans in the model). | Buffer levels by size are targeted moments, fitted in sample. |
| CD-Q4 | 29 | Capital ratio (risk-weighted) (b, f ) (8.46, 12.10) (10.51, 11.28) (·, 10.42) (·, 11.81) | Data against model for big and fringe banks: the targeted size gap of 3.64 points is reproduced as 0.77 (`CorbaeDErasmo.size_gap`). |
| CD-Q5 | 31 | After estimating parameters to match observed levels of buffers in both the imperfect and perfect competition economies | Their own description: fitted to buffer levels. |
| CD-Q6 | 26 | equity issuance costs are significantly lower for big than for small banks | A second size channel, access to capital, estimated not tested. |
| CD-Q7 | 24 | big banks have less volatile funding inflows, which is one important factor explaining why they hold a smaller capital buffer in our model. | Funding volatility. |
| CD-Q8 | 8 | The fact that capital ratios are above what regulation defines as well capitalized suggests a buffer stock motive which we model. | Buffers sit above the well-capitalised line; their model's floor is the 4% minimum. |
| CD-Q9 | 33 | Our model fails to generate the significant heterogeneity and long right tail of the distribution of risk-weighted capital of fringe banks. | The far-above tail, our F2, is missed. |
| CD-Q10 | 23 | and end in 2007 (due to the unconventional government intervention since 2008 which is not part of our model). | Moments from 1984-2007; no out-of-sample test of buffers. |
| CD-Q11 | 8 | The figure makes clear that large institutions have consistently lower levels of capital than other banks. | The size fact, Top 10 against the rest. |

## Lean results

| Name | Statement |
|---|---|
| `CorbaeDErasmo.size_gap` | $12.10-8.46=3.64$, $11.28-10.51=0.77$, and $0.77/3.64<0.22$. |
| `CorbaeDErasmo.big_overpredicted` | $10.51-8.46>2$: the big-bank ratio is over-predicted by about two points. |
| `CorbaeDErasmo.control_gap_not_closed` | $0.77<3.64$. |
| `CorbaeDErasmo.control_big_overpredicted` | $8.46<10.51$. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| CD-D1 | Tables 4 and 6 | The big-bank capital ratio is 8.48 in Table 4 and 8.46 in Table 6; Table 6 is used. Big-bank equity issuance is 0.02 in the text and 0.03 in Table 6. | Internal inconsistencies; neither changes a reading. |
