# Claims: Barberis, Huang and Santos (1999)

Bib key `barberis1999`. Version read and cited: NBER Working Paper 7220,
July 1999. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| BHS-Q1 | 16 | the volatility of log returns in this model is equal to the volatility of log consumption growth, namely 3.79%. | With a constant level of loss aversion, return volatility does not depend on that level (L2, C2). |
| BHS-Q2 | 32 | The investor's risk-aversion will therefore change more slowly over time, generating lower volatility and hence a lower equity premium. | Volatility in their full model comes from time variation in risk aversion, not from its level. |
| BHS-Q3 | 2 | The key to our results is that the agent's risk-aversion changes over time as a function of his investment performance. | As BHS-Q2, from the abstract. |
| BHS-Q4 | 10 | This is a piecewise linear function, shown in Figure 1. It is kinked at the origin, where the gain equals zero. | Their loss-aversion term is kinked-linear, as is the shortfall penalty $\Lambda$ (H5 in `TODO.md`). |
| BHS-Q5 | 15 | loss-aversion by itself cannot explain the equity premium. | The constant-level model fails on volatility and on the premium. |

## Lean results

| Name | Statement |
|---|---|
| `BarberisHuangSantos.v_homogeneous` | $v(SX)=S\,v(X)$ for $S>0$ (eq. 5). |
| `BarberisHuangSantos.v_eq_min` | For $\lambda\ge1$, $v(X)=\min\{X,\lambda X\}$. |
| `BarberisHuangSantos.v_concave` | For $\lambda\ge1$, $v$ is concave. |
| `BarberisHuangSantos.logReturn_eq` | For $f>0$, $\log\big(\frac{1+f}{f}e^{g+\sigma\varepsilon}\big)=\log\frac{1+f}{f}+g+\sigma\varepsilon$ (eq. 15). |
| `BarberisHuangSantos.dispersion_free_of_f` | For $f>0$, $\log R(\varepsilon_1)-\log R(\varepsilon_2)=\sigma(\varepsilon_1-\varepsilon_2)$, whatever $f$. |
| `BarberisHuangSantos.lam_after_ten_percent_fall` | $\lambda(z)=2.25+50(z-1)=7.25$ at $z=1.1$ (p. 28). |
| `BarberisHuangSantos.control_dispersion_depends_on_state` | With a state-dependent ratio, $f(z_t)=1$ and $f(z_{t+1})\in\{1,3\}$, the log return moves by $\log4-\log2\ne0$ at the same shock. |
| `BarberisHuangSantos.control_concavity_needs_lam_ge_one` | At $\lambda=1/2$, $v$ is not concave. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| BHS-D1 | title page | The version read and cited is NBER WP 7220 (July 1999). | Author's decision of 2026-10-10. The later journal version was not read, and nothing is attributed to it. |
| BHS-D2 | throughout | The text layer drops the "fi" and "fl" ligatures ("nancial", "specications"). | Quotations are taken from passages without them, and each is checked on its page. |
| BHS-D3 | p. 16, eq. 15 | $R_{t+1}=\frac{1+f}{f}e^{g+\sigma\varepsilon_{t+1}}$. | Garbled in the text layer. The reading follows from eq. (10) with constant $f$, and gives the stated log-return volatility $\sigma=3.79\%$. |
| BHS-D4 | p. 10, eq. 4 | $v(X)=X$ for $X\ge0$, $\lambda X$ for $X<0$. | Garbled in the text layer. The reading matches "piecewise linear ... kinked at the origin" and the caption of Figure 1. |
| BHS-D5 | p. 28 | $2.25+5=7.25$. | The text layer prints the decimal point as a colon ("2:25"). |
