# Claims: Andries and Haddad (2020)

Bib key `andrieshaddad2020`. Version read: the published article,
*Journal of Political Economy* 128(5):1901-1939, as page images without a
text layer (39 PDF pages, printed page 1923 twice, printed page 1939
absent, AH-D1). Quotations are checked against the OCR text (AH-D2).
Evidence [F] for the main text; the appendices are online and unread.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| AH-Q1 | 1903 | Disappointment aversion captures a sharp asymmetry between the extent to which people dislike bad outcomes and appreciate good outcomes. | The preference is a kink at a reference point, as in the bundle's shortfall penalty and in BHS. |
| AH-Q2 | 1909 | Disappointing outcomes thus create discontinuously more disutility to our agent than comparable gains, an asymmetry reminiscent of loss aversion | The kink in eq. (2); `AndriesHaddad.ce2_solves` is the two-point case. |
| AH-Q3 | 1910 | adding risk to a certain lottery is more hurtful than adding the same risk to an uncertain lottery of the same value. | The mechanism of Proposition 1. |
| AH-Q4 | 1910 | Therefore, our agent would rather not receive this intermediate information and just have the final payoff revealed at once. | Information aversion; `AndriesHaddad.information_aversion_example` for the uniform four-point payoff. |
| AH-Q5 | 1911 | our agent fears the flow rather than the quantity | The cost is in the number of revelations, not the information content (Proposition 2). |
| AH-Q6 | 1911 | the endogenous information costs have a roughly hump-shaped structure, where more spread out information flows tend to yield more information costs. | Why a Bayesian agent would avoid frequent signals. |
| AH-Q7 | 1911 | This contrasts fundamentally with both exogenous information costs and rational inattention models | The paper's own positioning against capacity-based accounts. |
| AH-Q8 | 1915 | With too frequent observations, the risk becomes so glaring to the information-averse investor that she finds the risky asset less appealing | Proposition 3, the non-participation region $v(T)\le r$. |
| AH-Q9 | 1916 | as long as the expected return on the risky asset g is larger than the risk-free rate r, that is, when there is a positive risk premium. | Proposition 4, $T^*>0$ whenever $g>r$. |
| AH-Q10 | 1917 | increasing in volatility | Proposition 5, item 3: inattention rises with $\sigma$. The channel runs from volatility to attention. |
| AH-Q11 | 1918 | while variations in expected returns leave them unchanged | The marginal cost of attention depends on $\sigma$ and $\theta$ and not on $g$. |
| AH-Q12 | 1920 | Third, the marginal cost of attention does not depend at all on the characteristics of the assets or on the agent's preferences. | The fixed-cost benchmark, eq. (12), against which information aversion is identified. |
| AH-Q13 | 1923 | this results in an optimal observation interval of 1.1 years | The calibration. |
| AH-Q14 | 1925 | both theories rationalize that inattention increases when volatility is larger. | Fact 3 does not separate the two accounts; Fact 4 does (Propositions 6 and 7). |
| AH-Q15 | 1931 | Our results show that she would consequently delay observing her portfolio. | The ostrich effect of Proposition 9, from bad unavoidable news. |
| AH-Q16 | 1918 | are available online | The proofs were not supplied (AH-D3). |

## Lean results

| Name | Statement |
|---|---|
| `AndriesHaddad.ce2_mem` | For $\theta\ge0$, $0<q<1$, $L<H$, the two-point certainty equivalent $(q(1+\theta)L+(1-q)H)/(1+\theta q)$ lies strictly between $L$ and $H$. |
| `AndriesHaddad.ce2_solves` | It solves eq. (2) for the lottery $(L,q;H,1-q)$, with $L$ the only disappointing outcome. |
| `AndriesHaddad.ce2_unique` | Any solution of eq. (2) for that lottery equals it. |
| `AndriesHaddad.compound_eq` | Revealing the uniform payoff on $\{0,1,2,3\}$ by halves and valuing recursively gives $3/(2+\theta)$. |
| `AndriesHaddad.oneShot_solves` | For $0\le\theta<2$, $(6+\theta)/(4+2\theta)$ solves eq. (2) for the uniform payoff revealed at once. |
| `AndriesHaddad.oneShot_unique` | For $0\le\theta<2$, it is the only solution. |
| `AndriesHaddad.oneShot_sub_compound` | The one-shot value exceeds the two-step value by $\theta/(4+2\theta)$. |
| `AndriesHaddad.information_aversion_example` | For $\theta>0$ the two-step value is strictly below the one-shot value (Propositions 1 and 2 for this example, AH-D4). |
| `AndriesHaddad.consumption_increasing_in_T` | $1-e^{kT}$ is strictly increasing in $T$ when $k<0$ (the form of eq. 7 with the rate held fixed, AH-D5). |
| `AndriesHaddad.control_no_aversion_at_theta_zero` | At $\theta=0$ the two values coincide. |
| `AndriesHaddad.control_ce2_degenerate` | At $q=1$ the two-point value is $L$, so `ce2_mem` needs $q<1$. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| AH-D1 | PDF pages 23 and 24 | Both carry printed page 1923. Printed $n\le1923$ is PDF page $n-1900$, printed $1924\le n\le1938$ is PDF page $n-1899$, and printed 1939 is absent. | The headers and first lines of PDF pages 23 and 24 are the same text. The suite uses this map. |
| AH-D2 | throughout | The OCR text replaces f by l or I in many words ("inlormation", "relerence", "Irom") and reads some symbols as letters ("T* > o", "volatility o"). | Quotations are taken from spans free of such words and symbols, and from spans that do not cross a line-end hyphen, which the suite's normaliser keeps. |
| AH-D3 | p. 1918 | All appendices are treated as online and unread. | The text says "appendix B" holds the extensions and "apps. A to C are available online". The supplied PDF ends with the references and has no appendix. |
| AH-D4 | eq. (1), two-date example | For a payoff at date 2 with no consumption before it, the recursion values an intermediate signal as $\mu_\theta(\mu_\theta(X\mid s))$, and discounting is common to the two filtrations. | The paper gives Proposition 1 in general with its proof online. Our example is a construction from eq. (1) and eq. (2), not the paper's display. |
| AH-D5 | p. 1915 | `AndriesHaddad.consumption_increasing_in_T` holds the certainty-equivalent rate fixed. | In eq. (7) the exponent's coefficient involves $v(T)$, which Lemma 2 says rises in $T$. The paper's statement "increasing in T" includes that dependence, and its proof is online. |
