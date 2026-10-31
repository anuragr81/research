# Claims: Pagel (2016)

Bib key `pagel2016`. Version read: the author's accepted manuscript
(51 pages, PDF dated 27 October 2014). The journal version, *Journal of the
European Economic Association* 14(2), was not read (PG-D1). Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| PG-Q1 | 4 | as the preferences feature full belief updating in every period, which is not true in the data. | The basic model updates the reference point fully each period. |
| PG-Q2 | 11 | Thus, the agent wants to delay the consumption response to shocks, which makes the consumption-wealth ratio vary. | The mechanism of Proposition 3, which `Pagel.rho_strictAnti_in_F` formalises in differences. |
| PG-Q3 | 12 | the variation in the consumption-wealth ratio is bounded. | The weight lies in $[\eta,\eta\lambda]$ (`Pagel.weight_mem`). |
| PG-Q4 | 14 | Thus, despite the i.i.d. environment, the expected risky return varies to make the agent willing to hold the entire tree each period. | Variation in expected returns comes from the preference, with i.i.d. fundamentals. |
| PG-Q5 | 15 | In the event of a bad shock, the consumption-wealth ratio is high, which increases not only expected returns but also the standard deviation of expected returns. | The arithmetic return's dispersion scales with the state, and the log return's does not (PG-D8). |
| PG-Q6 | 20 | The conditional variance of the stock market is countercyclical as in the data (Brandt and Kang (2004)), however, there is no empirical evidence that a high consumption-wealth ratio predicts high conditional variance. | The paper's own claim on conditional variance, for the arithmetic return. |
| PG-Q7 | 21 | Unfortunately, with full belief updating, there is no reason to expect persistence in the consumption-wealth ratio in the basic model. | The basic model has no persistence, so no persistent volatility. |
| PG-Q8 | 26 | Thus, a positive shock today implies on average more positive gain-loss utility in the future. | Persistence enters with the sluggish belief rule. |
| PG-Q9 | 35 | In the following, I quickly guess and verify the model's equilibrium. | Proposition 1 rests on a guess and its verification. |
| PG-Q10 | 41 | The existence of gain-loss utility generates negative values of the stochastic discount factor in particularly good states of the world. | The equilibrium's discount factor is negative in the far right tail, for $\eta(\lambda-1)>1$, which holds in every calibration of Table 2. |
| PG-Q11 | 43 | To keep the model tractable, I simply assume that the agent's beliefs do not correspond to actual consumption growth any more | The sluggish belief rule is assumed, not derived. |
| PG-Q12 | 44 | Unfortunately, this model can no longer be solved analytically. | The extension with persistence is numerical only. |

## Lean results

| Name | Statement |
|---|---|
| `Pagel.weight_mem` | For $\eta\ge0$, $\lambda\ge1$, $F\in[0,1]$, $\eta\le\eta F+\eta\lambda(1-F)\le\eta\lambda$. |
| `Pagel.weight_strictAnti` | For $\eta>0$, $\lambda>1$, $F_1<F_2$, the weight at $F_2$ is below the weight at $F_1$. |
| `Pagel.foc_iff_rho` | For $1+G>0$, $\psi+\gamma QG>0$, $(1+G)(1-\rho)=\rho(\psi+\gamma QG)$ iff $\rho=1/(1+\frac{\psi+\gamma QG}{1+G})$ (eq. 7). |
| `Pagel.rho_mem` | Under the same positivity, $0<\rho<1$. |
| `Pagel.pc_sub` | $\frac{\psi+\gamma QG_1}{1+G_1}-\frac{\psi+\gamma QG_2}{1+G_2}=\frac{(\psi-\gamma Q)(G_2-G_1)}{(1+G_1)(1+G_2)}$. |
| `Pagel.rho_strictAnti_in_F` | For $\eta>0$, $\lambda>1$, $0\le F_1<F_2\le1$, $0\le\gamma Q<\psi$, $\rho$ at $F_2$ is below $\rho$ at $F_1$ (Proposition 3, $\theta>1$). |
| `Pagel.gammaBar_solves` | $Q+\Omega+\bar\gamma Q\Omega+\bar\gamma Q\eta\lambda=Q(1+\eta\lambda)$, the crossing condition at $G=\eta\lambda$ (Proposition 2, PG-D4). |
| `Pagel.gammaBar_lt_one` | For $Q>0$, $\Omega>0$, $\eta\lambda\ge0$, $\bar\gamma<1$. |
| `Pagel.logReturn_eq` | For $0<\rho_0<1$, $\rho_1>0$, $\log R=\log\frac{\rho_0}{1-\rho_0}+(\mu+\varepsilon)-\log\rho_1$ (eq. 10 in logs). |
| `Pagel.log_dispersion_free_of_state` | The difference of log returns across two next-period outcomes is $(\varepsilon_a-\log\rho_a)-(\varepsilon_b-\log\rho_b)$, whatever $\rho_0$. |
| `Pagel.control_rho_flat_at_psi_eq_gammaQ` | At $\psi=\gamma Q$, $\rho$ is the same at every weight. |
| `Pagel.control_weight_flat_at_lam_one` | At $\lambda=1$ the weight is $\eta$ whatever $F$. |
| `Pagel.control_gross_dispersion_depends_on_state` | The gross-return dispersion between growth 1 and 2 is 2 at $\rho_0=1/2$ and 4 at $\rho_0=2/3$. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| PG-D1 | title page | This record is of the author's accepted manuscript, not the typeset JEEA article. | It thanks the editor and three referees, and its PDF is dated 27 October 2014. An L row citing the journal must be marked `\unv{...}`. |
| PG-D2 | throughout | The text layer drops the "fi", "fl" and "ff" ligatures ("nd", "uctuations", "oset"). | Quotations are taken from passages without them, and each is checked on its page. |
| PG-D3 | p. 14 | "as the risk-free rate is constant" means that $R^f_{t+1}$ is known at $t$. | Eq. (11) makes $R^f_{t+1}$ vary with $\rho_t$, and Proposition 4 signs that variation. Only the conditional reading is consistent with both. |
| PG-D4 | p. 42, B.3 | $\bar\gamma$ is obtained at $F(\varepsilon_t)=0$, where $G=\eta\lambda$. | The text says "I chose F(εt) = 1", but the formula $(\eta\lambda-\Omega/Q)/(\Omega+\eta\lambda)$ solves the crossing condition at $G=\eta\lambda$ (`Pagel.gammaBar_solves`), and for $\theta>1$ $\rho_t$ is largest at low shocks, where $F\to0$ (Proposition 3). Footnote 11's lower bound, with $\eta$ in place of $\eta\lambda$, is the case $F=1$. |
| PG-D5 | p. 36, B.1 | The tree's price is $P_t=W_t-C_t$. | B.1 prints "Pt = Ct − Wt". Eq. (6), p. 9 and $P_t/C_t=(1-\rho_t)/\rho_t$ all give $W_t-C_t$. |
| PG-D6 | eq. (7) and B.1 | $\psi=Q+\Omega+\gamma Q\Omega$. | Eq. (7) and eq. (9) write $Q+\Omega+\gamma Q\Omega$ where B.1 and B.5 write $\psi$, in the same position of the same ratio. B.3 calls both the marginal value of savings. |
| PG-D7 | pp. 26 and 43 | The sluggish rule's loading $\tilde\mu$ is the Appendix C one, $\bar\mu(E[R^{of}_{t+1}]-R^{of}_{t+1})$. | p. 26 says it "should reflect the variation in the basic model's consumption-wealth ratio", and Appendix C defines it from the basic model's risk-free rate. The two agree in shape, since by eq. (11) the risk-free rate varies through $\rho_t/(1-\rho_t)$. |
| PG-D8 | pp. 14 and 15 | "The quantity of risk $\sigma_t(R_{t+1})$ varies" concerns the gross return. | By eq. (10) the state is a predetermined scale on an i.i.d. factor, so the conditional dispersion of the log return is constant (`Pagel.log_dispersion_free_of_state`), while that of the gross return scales with the state (`Pagel.control_gross_dispersion_depends_on_state`). Not an error. |
