# Claims: FDIC (2026)

Bib key `fdic2026`. Source: the FDIC BankFind workbook `All Financial Reports.xlsx`, sheet "Reference-Variables&Definitions". Evidence [F] for the variables listed.

## Quotations

The Page column holds the variable whose entry (title and definition) contains the quotation.

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| FDIC-Q1 | ASSET | The sum of all assets owned by the institution including cash, loans, securities, bank premises and other assets. | `ASSET` is total assets $X$ (G1). |
| FDIC-Q2 | LIAB | Deposits and other borrowings, subordinated notes and debentures, limited-life preferred stock and related surplus, trading account liabilities and mortgage indebtedness. | `LIAB` is total liabilities $L$ (G1). |
| FDIC-Q3 | RWAJT | Total risk weighted assets are assets adjusted for risk-based capital definitions which include on-balance-sheet as well as off-balance-sheet items multiplied by specified risk-weights. | `RWAJT` is risk-weighted assets (G2). |
| FDIC-Q4 | RBCT1J | Tier 1 (core) capital includes: common equity plus noncumulative perpetual preferred stock plus minority interests in consolidated subsidiaries less goodwill and other ineligible intangible assets. | `RBCT1J` is Tier 1 capital, not CET1 (G3, G4). |
| FDIC-Q5 | RBCT1CER | The ratio of common equity tier 1 capital to risk-weighted assets. | `RBCT1CER` is the CET1 ratio, the counterpart of $a_1$ (G4). |
| FDIC-Q6 | RBCT1CER | Beginning March 2020, this ratio in not required nor available for an institution that has elected the Community Bank leverage Ratio (CBLR) framework. | The CET1 ratio is missing for banks electing the community bank leverage ratio (FDIC-D3). |
| FDIC-Q7 | NETINC | ( YTD, $ ) Net interest income plus total noninterest income plus realized gains (losses) on securities and extraordinary items, less total noninterest expense, loan loss provisions and income taxes. | Net income is year to date and is decumulated by quarter. |
| FDIC-Q8 | EQCDIV | ( YTD, $ ) Cash dividends paid on common and preferred stock. | Dividends, the model's singular control (G14). Year to date. |
| FDIC-Q9 | EQCSXQ | ( Qtly, $ ) Sale, conversion, acquisition, or retirement of capital stock, net. | Quarterly net stock issuance, one part of the recapitalisation measure (G15). |
| FDIC-Q10 | EQCSTKRX | treasury stock transactions; and capital related transactions involving the institution's employee stock ownership plan. | The stock measure is net of buybacks and includes ESOP transactions (G15). |
| FDIC-Q11 | EQCSTKRX | Institutions with total assets of less than $100 million file these data only on the December Call Report. | Equity flows are quarterly only for banks of at least $100 million. |
| FDIC-Q12 | EQCBHCTR | Other transactions involving the institution's parent holding company which affect equity capital directly such as capital contributions other than those for stock or dividends to the holding company in the form of property rather than cash. | Capital from the parent, the other part of the recapitalisation measure (G15). |
| FDIC-Q13 | EQCMRG | Changes that occur as a result of pooling of interest, purchase acquisition, or push down accounting. | Merger-driven equity changes, excluded from recapitalisations. |
| FDIC-Q14 | CERT | A unique number assigned by the FDIC used to identify institutions and for the issuance of insurance certificates. | `CERT` identifies the bank, the reporting institution (N1). |
| FDIC-Q15 | DEP | The sum of all deposits including demand deposits, money market deposits, other savings deposits, time deposits and deposits in foreign offices. | `DEP` records depositors' funds at the bank in aggregate (N4). |
| FDIC-Q16 | RSSDHCR | The unique number assigned by the Federal Reserve Board to the regulatory high holding company of the institution. | `RSSDHCR` identifies the parent holding company among the investors (N3). |
| FDIC-Q17 | NAMEHCR | Regulatory top holder is any company that directly or indirectly owns, controls or has power to vote 25 percent or more of a bank's or direct holding company's shares | The parent holding company is an investor that controls the bank (N3). |
| FDIC-Q18 | REGAGNT | There are now three federal regulators of banks and savings and loan institutions. | The regulator is the bank's primary federal regulator (N7). |
| FDIC-Q19 | BKCLASS | N - Commercial bank, national (federal) charter, Fed member, and supervised by the Office of the Comptroller of the Currency (OCC) | A national bank's regulator is the OCC (N7). |
| FDIC-Q20 | DEPINS | The estimated amount of FDIC Insured deposits in domestic offices and in Insured branches of Puerto Rico and US territories and possessions. | The insured deposits, held by the insured creditors and insured by the insurer (N4, N7). |
| FDIC-Q21 | IDLNCORR | core deposits held in domestic offices now includes: total domestic office deposits minus time deposits of more than $250,000 held in domestic offices and brokered deposits of $250,000 or less held in domestic offices. | The FDIC's own split of stable from rate-sensitive deposits (N4, N5). |
| FDIC-Q22 | VOLIAB | Federal funds purchased and securities sold under agreements to repurchase | The first item of volatile liabilities, funding from uninsured creditors (N5). |
| FDIC-Q23 | VOLIAB | Time deposits over $250,000 held in domestic offices | Large time deposits count as volatile liabilities, so uninsured depositors are uninsured creditors (N5). |
| FDIC-Q24 | DEPUNA | Estimated amount of uninsured deposits in domestic offices and in insured branches in Puerto Rico and U.S. territories and possessions reported by the institution. | The uninsured deposits (N5). |
| FDIC-Q25 | DEPUNA | Beginning September 2019 this item is required to be reported by FFIEC CALL form 051 filers with $1 billion or more in total assets. | Uninsured deposits are observed only for banks of $1 billion or more (N5). |

## Lean results

| Name | Statement |
|---|---|
| `MeasurementMap.rwa_two_assets` | With risk weight 0 on the riskless and 1 on the risky asset, risk-weighted assets are $\pi X$. |
| `MeasurementMap.leverage_of_state` | $(X-L)/X=(x-1)/x$ for $X,L>0$. |
| `MeasurementMap.control_rwa_needs_zero_weight_on_riskless` | With weight $1/5$ on the riskless asset, risk-weighted assets differ from $\pi X$. |
| `MeasurementMap.control_leverage_needs_l0_lt_one` | At $\ell_0=1$ the leverage rearrangement fails. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| FDIC-D1 | API, not the workbook | Amounts are in thousands of dollars. | The workbook states no unit. JPMorgan Chase Bank (CERT 628) reports `ASSET` 3,395,126,000 at 31 Dec 2023, which is $3.4 trillion only in thousands. |
| FDIC-D2 | ASSET, LIAB | `ASSET` and `LIAB` are quarter-end balances. | The workbook labels them "YTD", which for a balance has no flow meaning. |
| FDIC-D3 | RBCT1CER | "in not required" reads "is not required". | A typo in the source. |
