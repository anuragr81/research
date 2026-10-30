# Claims: Choulli, Taksar and Zhou (2004)

Bib key `choulli2004`. Version read: the arXiv reprint math/0503541v1 of the article in *The Annals of Applied Probability* 14(4), 1810-1837, 29 pages, which "differs from the original in pagination and typographic detail". Pages below are the reprint's. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CTZ-Q1 | 1 | The objective is to maximize the expected present value of the total dividend distributions. | Dividends only. There is no shortfall penalty and no asymmetry parameter. |
| CTZ-Q2 | 1 | some business activities might be redundant, that is, under the optimal policy they will never be used in any scenario. | Which exposures the optimal policy uses is the paper's main result. |
| CTZ-Q3 | 2 | the risk it can assume cannot exceed a certain level or that its business activities cannot be reduced to zero unless the company goes bankrupt. | The exposure bounds α and β are constants motivated by statute, not a cap that moves with the state. |
| CTZ-Q4 | 2 | The dividend rate is bounded by a constant M given a priori. | Dividends are paid at a bounded rate, not at a barrier. |
| CTZ-Q5 | 2 | In our model we also assume the existence of a constant rate liability payment, such as a mortgage payment on a property or amortization of bonds. | The liability is a deterministic outflow δ, with no liability shock. |
| CTZ-Q6 | 2 | The results of this model can be viewed as an extension of the results of Choulli, Taksar and Zhou [4]. | Reference [4] is the SIAM J. Control Optim. 41 paper of 2003, the one C5 names, with unbounded (singular) dividends. |
| CTZ-Q7 | 3 | irrespective of what business activities it chooses and x is the initial reserve. | The state is the reserve level, not a ratio to liabilities. |
| CTZ-Q8 | 23 | in view of the fact that no Skorohod problem has to be involved in this case. | Bounded-rate dividends need no reflection, unlike [4] and unlike the paper's barrier. |
| CTZ-Q9 | 24 | The function a∗ (x) is the optimal feedback risk control function | The optimal exposure is a feedback of the reserve, the object of M11's hypothesis. |
| CTZ-Q10 | 27 | The risk control policy is characterized by two critical reserve levels: | Two levels, x_α and x_β, separate the regions where each bound binds. |
| CTZ-Q11 | 27 | take the minimum risk α) when the reserve is below level | The lower bound binds near ruin when debt is small relative to profit. |
| CTZ-Q12 | 27 | then gradually increase business activity when the reserve is between | Between the levels the exposure is interior and increasing. |
| CTZ-Q13 | 27 | when the reserve reaches or goes beyond level | The upper bound binds at large reserves. |
| CTZ-Q14 | 27 | This policy is the same as that obtained in [4] for the case of unbounded dividend rate. | The same three-region policy holds in [4]. |
| CTZ-Q15 | 28 | It is interesting to note that in the case of unbounded dividend rate, the maximum business activity is always taken before dividend distributions ever take place; see [4]. | With barrier dividends, as in the paper, the upper bound binds below the dividend barrier. This is the precedent for the hypothesis of M11 at the surplus end. |
| CTZ-Q16 | 28 | the dividend distribution takes place only when the reserve exceeds the critical level | The dividend policy is of threshold type. |
| CTZ-Q17 | 10 | is a concave, twice differentiable solution of the HJB equation (3.3) on | Theorem 3.4, the closed form above the dividend threshold. |

## Lean results

| Name | Statement |
|---|---|
| `ChoulliTaksarZhou.ctilde_pos` | $\tilde c=2\mu(\delta+M)/(\mu^2+2\gamma\sigma^2)>0$ of (3.15). |
| `ChoulliTaksarZhou.disc_at_ctilde` | The discriminant of (3.10) at $z=\tilde c$ is $(\delta+M)^2$. |
| `ChoulliTaksarZhou.rtm_at_ctilde` | $\tilde r_-(\tilde c)=-\mu/(\sigma^2\tilde c)$, the exponent of Theorem 3.4(ii). |
| `ChoulliTaksarZhou.charpoly_at_ctilde` | That exponent solves the characteristic equation of (3.3) at $a=\tilde c$. |
| `ChoulliTaksarZhou.exposure_at_ctilde` | Along that exponential the feedback (3.5) equals $\tilde c$. |
| `ChoulliTaksarZhou.ktilde_smooth_fit` | Smooth fit $V'(x_1)=1$ gives $\tilde K=-\sigma^2\tilde c/\mu=1/\tilde r_-(\tilde c)$. |
| `ChoulliTaksarZhou.binds_upper_iff` | $\beta\le\tilde c\iff\beta(\mu^2+2\gamma\sigma^2)\le2\mu(\delta+M)$, the case of Theorem 3.4(i). |
| `ChoulliTaksarZhou.drift_neg` | If $\beta\mu<\delta$ the drift $a\mu-\delta$ is negative at every $a\le\beta$, the step of Proposition 3.1. |
| `ChoulliTaksarZhou.control_printed_exponent_fails` | The exponent $-\sigma^2\tilde c/\mu$ printed on p. 9 is not a root at $\mu=\sigma=\gamma=1$, $\delta+M=3$ (CTZ-D1). |
| `ChoulliTaksarZhou.control_printed_ktilde_fails` | The constant $\tilde K=-\mu/(\sigma^2\tilde c)$ printed on pp. 9 and 10 fails smooth fit at the same point (CTZ-D2). |

## Readings recorded

| ID | Reading |
|---|---|
| CTZ-D1 | Page 9 prints $\tilde r_-(\tilde c)=-\sigma^2\tilde c/\mu$ and the matching exponent. Theorem 3.4(ii) on p. 10 and Theorem 3.19(ii) on p. 23 print $-\mu/(\sigma^2\tilde c)$, which is correct by (3.5), by dimension and by `rtm_at_ctilde`. A typographical slip with no effect on the results. |
| CTZ-D2 | The constant $\tilde K$ for $x_1>0$ is printed as $-\mu/(\sigma^2\tilde c)$ on pp. 9 and 10. Smooth fit with the correct exponent gives $-\sigma^2\tilde c/\mu$, the same form $1/\tilde r_-$ as $K_\beta$ and $K_\alpha$. A slip with no effect on the policy. |
| CTZ-D3 | The bounds on exposure are constants. In the paper the cap $u(x)/x$ moves with the buffer and has two branches, so where it binds the buffer variance changes with the state and has limits (M11). |
| CTZ-D4 | The liability is a constant outflow, so a binding bound fixes the reserve variance at $\alpha^2\sigma^2$ or $\beta^2\sigma^2$. In the paper the liability shock is unhedged and enters the limits $\nu_1^2,\nu_3^2$ with the correlation $c$. |
| CTZ-D5 | There is no shortfall penalty, no issuance and no asymmetry parameter, so the paper's statement that the limits are free of $\lambda_S$ has no counterpart here. |
| CTZ-D6 | This is not the paper that C5 names. That is reference [4], SIAM J. Control Optim. 41 (2003) 1946-1979, which has barrier dividends like the paper's, and whose result is reported here on pp. 27 and 28 (CTZ-Q14, CTZ-Q15) but was not read. |
