# Claims: Juselius, Ponte Marques and Tarashev (2025)

Bib key `juseliusetal2025`. Version read: "Banks' regulatory risk tolerance", dated 20 October 2025, a conference version (file SBFC2025_7B1_P170.pdf); the BIS Working Paper 1287 version was not read. 30 PDF pages; printed pages 1 to 29 on PDF pages 2 to 30 (PDF page = printed page + 1); the abstract page is cited as p. 0. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| JPT-Q1 | 0 | We employ 68 quarters of data | 17 US and 17 euro-area banks, 2000-2008 and 2016-2024, the crisis years dropped. |
| JPT-Q2 | 2 | Through several regulatory regimes, banks have consistently operated with management buffers (MBs) – defined as the headroom in regulatory capital ratios above the corresponding requirements. | Their buffer sits above minimum plus every regulatory buffer, the floor construction of the enhanced model. |
| JPT-Q3 | 3 | we express the MB in terms of the highest quality regulatory capital relative to risk-weighted assets (RWA) and robustly estimate a partial adjustment model of the MB. | A reduced-form partial adjustment, not a dynamic optimisation with a barrier. |
| JPT-Q4 | 10 | This measure targets simplicity and relies on the standard deviation of shocks as a proxy to the | Their regulatory risk tolerance is shock volatility over the buffer target. |
| JPT-Q5 | 12 | The implication is that an eight-standard-deviation shock is needed to wipe out the MB target on impact in the former regime, whereas a four-standard-deviation shock would have a similar impact in the latter regime. | A buffer-to-volatility multiple, about 7.7 before the crisis and 3.8 after (`JuseliusEtAl.post_multiple`), measured against the volatility of buffer changes, not against earnings volatility. |
| JPT-Q6 | 3 | conflating exogenous effects of the macro-financial environment and any counteracting measures that the bank takes within a quarter. | Their shocks include the bank's own actions, so they do not measure the damping of E8. |
| JPT-Q7 | 4 | For instance, US banks have significantly lower RRT than euro-area banks, mainly because of lower volatility of MB shocks. | US banks, a multiple of about 4.7 after the crisis (`JuseliusEtAl.us_multiple`). |
| JPT-Q8 | 16 | small banks experience 13% higher volatility of MB shocks and revert more slowly to the MB target, but operate with a 50% higher MB target than big banks. | Within large banks, smaller ones hold more beyond volatility. |
| JPT-Q9 | 10 | Employing similar reasoning, Peura and Jokivuolle (2004) calibrate the probability of breaching regulatory requirements to match data on banks | A further antecedent they cite, not read. |
| JPT-Q10 | 3 | Since we abstract from the GFC and its aftermath, our aim is not to assess banks | Normal times only. |

## Lean results

| Name | Statement |
|---|---|
| `JuseliusEtAl.pre_multiple` | $7.6<4.59/0.60<7.7$ (Table 2, regime 1). |
| `JuseliusEtAl.post_multiple` | $3.8<2.59/0.68<3.82$ (Table 2, regime 2). |
| `JuseliusEtAl.us_multiple` | $4.68<2.39/0.51<4.7$ (Table 5, US). |
| `JuseliusEtAl.control_post_multiple_not_three` | $2.59/0.68>3$. |
| `JuseliusEtAl.control_us_lower_volatility` | $0.51<0.82$, US against euro-area shock volatility. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| JPT-D1 | pp. 12, 13 | The multiple is the buffer target over the standard deviation of one-quarter buffer changes net of the bank's own actions, from Tables 2 and 5; it is not a volatility that makes a model reproduce buffers. | JPT-Q3 to JPT-Q6. |
| JPT-D2 | Section 7 | A "one standard deviation negative shock" there (about 1.3 and 1.7 points) does not match the shock volatility of Table 2 (0.68); it appears to be the dispersion of the buffer level. Not relied on. | Internal comparison. |
