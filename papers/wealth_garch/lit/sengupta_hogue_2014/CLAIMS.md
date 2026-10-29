# Claims: Sengupta and Hogue (2014)

Bib key `senguptahogue2014`. Version read: the forthcoming version of the article for the Federal Reserve Bank of Kansas City Economic Review, 2014 Q4, which states that its page numbering will change in the issue; 18 PDF pages, printed pages 5 to 22 (PDF page = printed page - 4). The issue version was not read. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| SH-Q1 | 6 | It finds that banks with higher risk and banks that are not owned by a BHC have higher capital ratios | Their two findings. |
| SH-Q2 | 10 | The analysis uses quarterly bank-level data on U.S. commercial banks from the Consolidated Reports of Condition and Income for a Bank (FFIEC 041), popularly known as the Call Reports. | Call Report data, 1996-2006. |
| SH-Q3 | 12 | The regression analysis is restricted to community banks, as most S-corp banks and non-BHC banks are community banks (Table 1). | Community banks only, under 10 billion dollars. |
| SH-Q4 | 17 | Dummy variables. Non-BHC bank is equal to 1 if the bank is not a holding company and 0 otherwise. | Their stand-alone bank has no holding company; a shell holding company's bank counts as held (SH-D1). |
| SH-Q5 | 9 | The second is the shell BHC, a company established primarily to hold the stock of its subsidiary bank. | One-bank holding companies are holding-company banks in their coding. |
| SH-Q6 | 13 | Bank-specific, time-invariant dummies (fixed effects) are used to control for a bank’s idiosyncratic risk. | Their estimates are within banks, not across banks of the same size. |
| SH-Q7 | 16 | Non-BHC banks are associated with a 1.06-percentage-point higher Tier 1 leverage ratio and a 2.19-percentage-point higher Tier 1 risk-based capital ratio, after controlling for other determinants of capital ratios. | Their stand-alone effect (`SenguptaHogue.gap_leverage`). |
| SH-Q8 | 16 | These results lend support to the precautionary motive of non-BHC banks to hold more capital because they have less access to external sources of capital. | Their access reading. |
| SH-Q9 | 11 | The capital ratios for community banks are significantly higher than for large and regional banks. | The size fact, descriptive. |

## Lean results

| Name | Statement |
|---|---|
| `SenguptaHogue.gap_leverage` | $10.70-9.65=1.05$ (Table 3), against 1.06 in the text (SH-D2). |
| `SenguptaHogue.gap_riskbased` | $17.00-14.80=2.20$, against 2.19 in the text (SH-D2). |
| `SenguptaHogue.control_rounding` | $10.70-9.65\ne1.06$. |
| `SenguptaHogue.control_riskbased_not_leverage` | $2.20\ne1.05$. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| SH-D1 | pp. 9, 17 | Stand-alone means no holding company; banks of one-bank holding companies are held. results_access.md pools both as stand-alone (after Holod and Peek), so the two designs differ in the category. | SH-Q4, SH-Q5. |
| SH-D2 | p. 16, Table 3 | The text's 1.06 and 2.19 are the Table 3 differences 1.05 and 2.20 after rounding of the table's entries. | Lean. |
| SH-D3 | Table A1 | The legend reads "Significant at .1 percent level" for one star; read as 10, 5 and 1 percent. | Garbled legend. |
