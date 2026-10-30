# Claims: Calem and Rob (1996)

Bib key `calem1999`. Version read: Federal Reserve Board, Finance and Economics Discussion Series 96-12, dated March 1996; 47 PDF pages, printed page = PDF page - 1. Read in full, the tables and the reference list included. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CR-Q1 | 7 | Bank size (total assets) is exogenously set at 1. | The state is capital per unit of assets, in discrete time. |
| CR-Q2 | 9 | stockholders earn zero, and the entire net return that period goes toward next period's capital | Below the requirement the bank rebuilds capital from earnings alone. It has no way to issue (CR-D1). |
| CR-Q3 | 18 | In general, a severely undercapitalized bank takes maximal risk in an effort to improve its capital position. | The gamble for resurrection near insolvency, in a bank that cannot issue. |
| CR-Q4 | 23 | the bank generally takes on more portfolio risk | A higher flat requirement raises the risk a well-capitalized bank takes. |
| CR-Q5 | 25 | The underlying intuition is that the premium surcharge cuts into bank earnings, hampering the effort to recapitalize. | A premium surcharge below the requirement, the same at every shortfall, raises the risk undercapitalized banks take because it slows recapitalisation from earnings. |
| CR-Q6 | 26 | has no appreciable incentive effect on the behavior of well capitalized banks | The surcharge does not change a well-capitalized bank's risk. |
| CR-Q7 | 34 | be sufficiently stringent for it to be effective | Their risk-based standard moderates risk only when stringent enough. |
| CR-Q8 | 6 | Previous contributions focused entirely on the impact of a flat capital standard. | Their own positioning against the static portfolio models. |

## Lean results

| Name | Statement |
|---|---|
| `CalemRob.riskbased_is_linear_bound` | For $n>0$, $0.06+n(R-R_0)\le C\iff R\le R_0+(C-0.06)/n$: their rule (13) bounds the risky share linearly in capital. |
| `CalemRob.bound_slope` | The bound rises in capital with slope $1/n$. |
| `CalemRob.table3a_u_shape` | Table 3a, column (i): $0.05<1.00$ and $0.05<0.55$, so the risky share falls and then rises in capital. |
| `CalemRob.higher_requirement_more_risk` | Table 4 against Table 3a, column (i): $0.55<0.60$ and $0.55<0.65$ at the requirement. |
| `CalemRob.control_bound_needs_positive_slope` | With $n=0$ the standard does not depend on the risky share. |
| `CalemRob.control_monotone_is_not_u` | A schedule that only falls has no interior minimum. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| CR-D1 | pp. 7 to 10 | The bank never issues. Capital above the requirement is paid out each period and capital below it is rebuilt from the period's net return, with failure when the net return is not positive. The requirement is therefore also the bank's target and its dividend barrier, except under the risk-based standard of Section 6, where the bank chooses a capital target above it. | CR-Q2, eq. (5). |
| CR-D2 | pp. 28 to 29 | Rule (13) is a bound on the risky share linear in capital with slope $1/n$ and an intercept, the form of the solvency branch of the theory paper's regulatory cap (A2), where the slope is $1/a_1$ and the intercept zero. | `CalemRob.riskbased_is_linear_bound`, `CalemRob.bound_slope`. |
| CR-D3 | p. 38 | Table 1 prints $x=0.08$ and $\#_0=0.048$, the net rates, where the text (p. 12) gives the gross $1.08$ and $1.048$. | The two agree as net and gross returns. |
