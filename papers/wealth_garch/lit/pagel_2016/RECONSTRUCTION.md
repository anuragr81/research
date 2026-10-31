# Reconstruction: Pagel (2016)

## Source read

- "Expectations-Based Reference-Dependent Preferences and Asset Pricing",
  Michaela Pagel, Columbia Graduate School of Business. Published in the
  *Journal of the European Economic Association* 14(2), 2016. The version
  read is the author's accepted manuscript (`PaperMP.pdf`). It thanks "my
  editor and three anonymous referees", its PDF creation date is
  27 October 2014, and it has 51 pages. These are the body (pp. 1 to 28),
  references, Appendix A (figures), Appendix B (proofs, pp. 35 to 43),
  Appendix C (sluggish belief updating, pp. 43 to 44) and an Online
  Appendix D. Evidence tag [F]: the full text was read, including the
  appendices. The typeset journal version was not read, and its pages are
  not attributed here (PG-D1).
- Supplied by the author on Google Drive (folder
  `13YJlLFKPSU7yPWMLjfsHwcbvEDoTWBVj`), uploaded 2026-10-31. Cached at
  `~/.cache/wealth_garch/pagel_2016.pdf`, sha256
  `288146428a7f2dfe52a1d1d010905e990f6c7622e5cef5a200d2dea162ddc248`.
  Printed page $n$ is PDF page $n$.

## Primitives

- A Lucas tree. Log consumption growth is i.i.d. normal,
  $\log(C_{t+1}/C_t)=\mu_c+\varepsilon_{t+1}$ with
  $\varepsilon_{t+1}\sim N(0,\sigma_c^2)$ (eq. 1). Consumption equals
  dividends, and there is no separate dividend process.
- Instantaneous utility is consumption utility plus gain-loss utility over
  news (eq. 2), $U_t=u(C_t)+n(C_t,F^{t-1}_{C_t})+\gamma\sum_{\tau\ge1}\beta^\tau
  n(F^{t,t-1}_{C_{t+\tau}})$, with power utility
  $u(c)=c^{1-\theta}/(1-\theta)$.
- Gain-loss utility compares outcomes in utility units through a
  piecewise-linear $\mu(x)=\eta x$ for $x>0$ and $\eta\lambda x$ for
  $x\le0$ (eqs. 3 and 4). The reference point is the agent's rational
  belief from the previous period, a full distribution. Contemporaneous
  gain-loss utility is over news about current consumption, and
  prospective gain-loss utility is over revisions of beliefs about future
  consumption, discounted by $\gamma$.
- The equilibrium concept is a Markovian rational-expectations equilibrium
  (Definition 3). It is the preferred personal equilibrium of Koszegi and
  Rabin (2006). The price-consumption ratio and the risk-free return depend
  only on the current shock, $P_t/C_t=p(\varepsilon_t)$ and
  $R^f_{t+1}=r(\varepsilon_t)$ (Definition 2).

## Derivation chain

1. Proposition 1 and Appendix B.1. Guess $C_t=\rho_tW_t$ with $\rho_t$
   i.i.d. Then $R_{t+1}=\frac{\rho_t}{1-\rho_t}\frac{C_{t+1}}{C_t}\frac1{\rho_{t+1}}$,
   and future values are powers of $W_t-C_t$ times
   $(\rho_t/(1-\rho_t))^{1-\theta}$, which the price-taking agent treats as
   exogenous. In equilibrium the prior belief evaluated at realised
   consumption is $F^{t-1}_{C_t}(C_t)=F(\varepsilon_t)$. The first-order
   condition is then
   $C_t^{-\theta}(1+G_t)=(W_t-C_t)^{-\theta}(\frac{\rho_t}{1-\rho_t})^{1-\theta}(\psi+\gamma QG_t)$
   with $G_t=\eta F(\varepsilon_t)+\eta\lambda(1-F(\varepsilon_t))$.
   Substituting $C_t=\rho_tW_t$ cancels the powers and leaves
   $(1+G_t)(1-\rho_t)=\rho_t(\psi+\gamma QG_t)$, so
   $\rho_t=1/\big(1+\frac{\psi+\gamma QG_t}{1+G_t}\big)$ (eq. 7). The
   cancellation of powers is done by hand. `Pagel.foc_iff_rho` starts after
   it, and `Pagel.rho_mem` gives $0<\rho_t<1$. The proof verifies the
   first-order condition. It does not check the second-order condition,
   the convergence of $Q$ (which needs
   $\beta e^{\mu_c(1-\theta)+\frac12(1-\theta)^2\sigma_c^2}<1$), the
   existence of the fixed point $\psi$, or transversality. Proposition 1's
   "exists" therefore rests on a guess and its verification.
2. The constants. $Q$ is the discounted stream of consumption utility,
   a geometric sum (p. 35). $\Omega>0$ for $\theta>1$ is expected gain-loss
   disutility (p. 41). $\psi=Q+\Omega+\gamma Q\Omega$ is the marginal value
   of savings (PG-D6). With $\eta=0$ the model is Mehra-Prescott, $\psi=Q$
   and $\rho^s=1/(1+Q)$ is constant (p. 41).
3. Proposition 2 and Appendix B.3. The ratio is below the traditional one
   for every shock when $\theta>1$ and $\gamma>\bar\gamma$, where
   $\bar\gamma=(\eta\lambda-\Omega/Q)/(\Omega+\eta\lambda)<1$. The
   threshold solves the crossing condition at $G=\eta\lambda$, which is
   $F=0$ (`Pagel.gammaBar_solves`, `Pagel.gammaBar_lt_one`, PG-D4).
4. Proposition 3 and Appendix B.4. The weight $G$ lies in
   $[\eta,\eta\lambda]$ (`Pagel.weight_mem`) and falls in $F$ when
   $\lambda>1$ (`Pagel.weight_strictAnti`). The price-consumption ratio
   moves with the weight by the factor $\psi-\gamma Q$ (`Pagel.pc_sub`). So
   when $\psi>\gamma Q$, a higher shock lowers $\rho_t$
   (`Pagel.rho_strictAnti_in_F`), and when $\psi=\gamma Q$ the ratio is
   flat (`Pagel.control_rho_flat_at_psi_eq_gammaQ`, the case $\theta=1$,
   $\gamma=1$). This is the difference form of the derivative in B.4.
5. Proposition 4 and Appendix B.5. Expected return, risk-free return and
   equity premium are each a constant times $\rho_t/(1-\rho_t)$ (eqs. 10,
   11 and 13), so for $\theta>1$ all three fall in $\varepsilon_t$. The
   price of risk and the conditional Sharpe ratio are constant (p. 14).
6. Calibration (Section 3.1, Table 2). The frequency is biannual, with
   $\theta=4$, $\beta=0.999$, $\eta=1$, $\eta(\lambda-1)\in\{1.6,2,2.4\}$
   and $\gamma=0.7$. The model matches the mean and volatility of the
   equity premium. It predicts a risk-free rate volatility of 11 to 17
   percent against 0.97 in the data, return autocorrelation of $-0.47$
   against 0.01, and $AR(c_t-w_t)\approx0.01$ against 0.6.
7. Section 4.1. Environment-based extensions put heteroskedasticity, a
   time-varying mean or time-varying disaster risk into the consumption
   process, each an AR(1) whose shock loading is reverse-engineered to
   offset the risk-free rate (pp. 23 to 25). Results are "available on
   request" or in the Online Appendix.
8. Section 4.2 and Appendix C. The preference-based extension is sluggish
   belief updating, with
   $\mu_{t+1}=\nu\mu_t+(1-\nu)(\mu_c+\tilde\mu(\varepsilon_{t+1}))$ and
   $\tilde\mu$ defined from the basic model's risk-free rate (p. 43,
   PG-D7). It is assumed, "To keep the model tractable" (p. 43), with
   $\nu=0.5$, and is solved numerically (p. 44). It gives
   $AR(c_t-w_t)=0.14$ (Table 3).

## What the paper implies for the volatility of returns

By eq. (10), $R_{t+1}=s(\varepsilon_t)\,X(\varepsilon_{t+1})$ with
$s=\rho_t/(1-\rho_t)$ and $X=e^{\mu_c+\varepsilon_{t+1}}/\rho_{t+1}$. The
state enters only as a predetermined scale. In logs this is
$\log R_{t+1}=\log s(\varepsilon_t)+\mu_c+\varepsilon_{t+1}-\log\rho_{t+1}$
(`Pagel.logReturn_eq`). The dispersion of the log return across any two
next-period outcomes is free of the current state
(`Pagel.log_dispersion_free_of_state`), while the dispersion of the gross
return scales with it (`Pagel.control_gross_dispersion_depends_on_state`).

The paper's "the quantity of risk $\sigma_t(R_{t+1})$ varies with the
consumption-wealth ratio" (p. 14) is therefore a statement about the
arithmetic return (PG-D8). Two consequences follow from the derivation, and
neither is stated by the paper.

- The conditional variance of the log return is constant in the basic
  model.
- The scale $s(\varepsilon_t)$ is i.i.d. Squared returns are therefore
  dependent at lag one, since $R_t$ and $R_{t+1}$ share $\varepsilon_t$.
  They are independent at lag two and beyond, since $R_{t+1}$ and $R_{t-1}$
  share no shock. The basic model has no persistent volatility.

The paper reports the same absence of persistence for the
consumption-wealth ratio (PG-Q7, PG-Q10). The lag structure of squared
returns is our derivation from eq. (10) and Definition 2, and is not
claimed in Lean beyond the factorisation and the dispersion identity.

## Roles

- The reference point is forward-looking, a rational belief. It moves
  every period with full updating, and that is what makes the ratio state
  dependent. In Barberis, Huang and Santos (2001) the ratio is constant
  when loss aversion is constant (BHS records). Here the kink parameter
  $\lambda$ reaches the ratio through $G$, because the reference
  distribution is evaluated at the realised shock.
- Persistence in the ratio, and so in any volatility structure, enters
  only through an assumed law of motion. In Section 4.1 it is an exogenous
  AR(1) in the consumption process. In Section 4.2 and Appendix C it is an
  AR(1) belief rule with a reverse-engineered loading. Neither rule is
  derived from a model of how information is processed.

## What could not be reconstructed

- The Online Appendix derivations of the heteroskedastic and disaster-risk
  extensions, and the simulations "available on request" (pp. 25, 26).
- The numerical solution of Appendix C and the moments of Tables 2 and 3.
- Whether the guess of Appendix B.1 is the only Markovian equilibrium. The
  paper does not claim uniqueness.
- The typeset journal version and its pagination.
