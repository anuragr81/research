# Claims: Barberis, Huang and Santos (2001)

Bib key `barberis2001`. Version read: *Quarterly Journal of Economics*
116(1):1-53. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| QJE-Q1 | 47 | the volatility of log returns in this model is equal to the volatility of log dividend growth, namely 12 percent. | With constant loss aversion, return volatility does not depend on its level (C2). |
| QJE-Q2 | 47 | Standard deviation 12.0 12.0 12.0 12.0 20.02 | Table XIII. The same volatility at $b_0=0$, 0.7, 2 and 100, against the empirical 20.02. |
| QJE-Q3 | 4 | The investor's risk aversion is then constant over time, and stock prices lose an important source of volatility. | Volatility comes from time variation in loss aversion. |
| QJE-Q4 | 10 | This is a piecewise linear function, shown as the solid line in Figure I. It is kinked at the origin, where the gain equals zero. | The loss-aversion term is kinked-linear (H5). |
| QJE-Q5 | 17 | For simplicity then, we make v linear over both gains and losses. | As QJE-Q4. |
| QJE-Q6 | 20 | We calculate the price P t of a dividend claim—in other words, the stock price—in two different economies. | Economy II, absent from the working paper. |

## Lean results

| Name | Statement |
|---|---|
| `BarberisHuangSantos.v_homogeneous` | $v(SX)=S\,v(X)$ for $S>0$. |
| `BarberisHuangSantos.v_concave` | For $\lambda\ge1$, $v$ is concave. |
| `BarberisHuangSantos.logReturn_eq` | Eq. (46) in logs: $\log\frac{1+f}{f}+g+\sigma\varepsilon$. |
| `BarberisHuangSantos.dispersion_free_of_f` | Log-return dispersion is $\sigma(\varepsilon_1-\varepsilon_2)$, whatever $f$. |
| `BarberisHuangSantos.control_dispersion_depends_on_state` | With a state-dependent $f$ the dispersion moves. |
| `BarberisHuangSantos.control_concavity_needs_lam_ge_one` | At $\lambda=1/2$, $v$ is not concave. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| QJE-D1 | p. 47, eq. 46 | $R_{t+1}=\frac{1+f}{f}e^{g_D+\sigma_D\varepsilon_{t+1}}$. | The display is broken across lines in the text layer. The reading follows from eq. (45) with constant $f$ and gives the stated 12 percent. |
| QJE-D2 | throughout | Greek letters and operators are replaced by private-use glyphs in the text layer. | Quotations are taken from passages without mathematics. |
| QJE-D3 | p. 30 | The illustration "2.25 + 3(0.1) = 2.55" uses $k=3$. | The working paper's illustration used $k=50$ and 7.25. The two are different calibrations, not an error. |
