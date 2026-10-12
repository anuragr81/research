# Claims: Løkka and Zervos (2008)

Bib key `lokka2008`. Version read: the authors' preprint dated 14 July 2005, 14 pages, not the journal's typeset version. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| LZ-Q1 | 1 | Assuming proportional issuance of equity costs, we derive closed form solutions and we completely characterise the optimal strategies. | Costs are proportional only; no fixed cost. |
| LZ-Q2 | 2 | Indeed, banks and insurance companies are restricted by regulations to maintain positive reserves at all times in order to operate. | The authors motivate the reflected problem by bank regulation. |
| LZ-Q3 | 2 | In this problem, it is optimal to issue minimal new equity so as to reflect the reserves process at 0 and make minimal dividend payments so as to reflect the reserves process at a certain level x∗ (see Section 4). | Issuance is singular, minimal top-ups at zero, with no lump and no target (LZ-D1). |
| LZ-Q4 | 2 | Thus, under the optimal strategy, 0 acts either as an absorbing or as a reflecting boundary point in the reserves process' state space. | The firm either never issues or always issues at zero. |
| LZ-Q5 | 3 | Note that β = 1 corresponds to no costs, while high values of β correspond to high costs associated with raising capital in the market. | The cost parameter. |
| LZ-Q6 | 7 | Now, considering the time value of money can lead us to the conclusion that it is optimal to postpone the issuance of new equity for as long as possible. | Issuance waits for zero reserves. |
| LZ-Q7 | 1 | Date: July 14, 2005. | The version read. |

## Lean results

| Name | Statement |
|---|---|
| `LokkaZervos.roots_sum` | $\alpha_1+\alpha_2=-2\mu/\sigma^2$ for the roots (3.5)-(3.6), which are `PeuraKeppo.dplus`, `PeuraKeppo.dminus` with $r$ for $\rho$. |
| `LokkaZervos.xss_exp` | $e^{(\alpha_1-\alpha_2)x^{**}}=\alpha_2^2/\alpha_1^2$ for $x^{**}$ of (3.10). |
| `LokkaZervos.barrier_paste` | $\alpha_1^2e^{\alpha_1x^{**}}=\alpha_2^2e^{\alpha_2x^{**}}$, so (3.9) holds at $x^{**}$. |
| `LokkaZervos.xss_pos_iff` | $x^{**}>0\iff\mu>0$, (3.12). |
| `LokkaZervos.coeffs_paste` | $c_1,c_2$ of (4.10) satisfy (4.7) and (4.8). |
| `LokkaZervos.eq_49` | Under (4.10), $c_1\alpha_1+c_2\alpha_2=(\alpha_1e^{-\alpha_2x^*}-\alpha_2e^{-\alpha_1x^*})/(\alpha_1-\alpha_2)$, so (4.6) is (4.9). |
| `LokkaZervos.v_concave` | The second derivative (4.14) is negative for $x<x^*$. |
| `LokkaZervos.control_concave_needs_below_barrier` | Above the barrier the expression of (4.14) is positive. |
| `LokkaZervos.control_coeffs_need_distinct_roots` | With $\alpha_1=\alpha_2$ the coefficients fail (4.7). |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| LZ-D1 | pp. 2, 7 | Issuance in this model is a minimal reflection at zero: many infinitesimal top-ups, no lump-sum issue and no target above zero. | LZ-Q3, Theorem 5.1 (iii). With proportional costs only, nothing pays for a lump. |
| LZ-D2 | the copy read | The preprint is dated 2005; the published article (BCVW cites *Insurance: Mathematics and Economics* 42(3):954-961, 2008) was not read. | Its text may differ. Only the preprint's pages are attributed. |
