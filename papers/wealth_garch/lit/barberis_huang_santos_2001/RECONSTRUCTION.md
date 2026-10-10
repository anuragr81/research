# Reconstruction: Barberis, Huang and Santos (2001)

## Source read

- "Prospect Theory and Asset Prices", Nicholas Barberis, Ming Huang, Tano
  Santos. *Quarterly Journal of Economics* 116(1):1-53, February 2001.
  53 pages. Evidence tag [F]: the full text was read, including the appendix
  and the references.
- Supplied by the author on Google Drive (folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`, file `bhs_jnl.pdf`), uploaded
  2026-10-10. Cached at `~/.cache/wealth_garch/bhs_qje2001.pdf`, sha256
  `271295d5aacdedccda5547042089d62c29a5d0ec8dab45d120b27b58d121ed3f`. Printed
  page numbers equal PDF pages.
- The working paper of 1999 has its own record,
  `lit/barberis_huang_santos_1999/`. `NOTES.md` here lists what differs.

## Primitives

- A continuum of identical agents, a risk-free asset in zero net supply and
  one unit of a risky asset, a claim to dividends with i.i.d. log growth
  $g_D+\sigma_D\varepsilon$ (eq. 1).
- Preferences (eq. 2): power utility over consumption plus
  $b_t\rho^{t+1}v(X_{t+1},S_t,z_t)$, with gain or loss
  $X_{t+1}=S_tR_{t+1}-S_tR_{f,t}$ (eq. 3) and $z_t=Z_t/S_t$ the benchmark state.
- At $z_t=1$, $v=X$ for gains and $\lambda X$ for losses, $\lambda>1$, piecewise
  linear and kinked at the origin (eq. 4). Prior gains cushion losses
  (eqs. 5 to 6). Prior losses raise the penalty to
  $\lambda(z_t)=\lambda+k(z_t-1)$ (eqs. 7 to 8). The benchmark is sluggish,
  $z_{t+1}=\eta z_t\bar R/R_{t+1}+(1-\eta)$ (eq. 10).

## Derivation chain

1. Section IV. One-factor Markov equilibria with $f_t=f(z_t)$ (eq. 19) and
   constant $R_f$, in Economy I (dividends equal consumption, Prop. 1) and
   Economy II (dividends and consumption separate, correlation $\omega$,
   Prop. 2).
2. Section V. Numerical solution. Economy I gives return volatility of 4.77%
   to 5.62% at $k=3$ (Table II). Economy II with $\sigma_D=12\%$ gives 17.39%
   to 20.87% (Table IV), from changing risk aversion.
3. Section VI, Prop. 3. With constant loss aversion, $v=v(X_{t+1})$ alone
   (eq. 41), the price-dividend ratio is constant and returns are i.i.d.
   By eq. (46), $R_{t+1}=\frac{1+f}{f}e^{g_D+\sigma_D\varepsilon_{t+1}}$, so log
   return volatility equals $\sigma_D=12\%$ for every $b_0$ and $\lambda$.
   Table XIII reports a standard deviation of 12.0 at $b_0=0$, 0.7, 2 and 100.
4. Appendix. Sufficiency of the Euler equations (Duffie and Skiadas method),
   and the measure of average loss aversion.

## Roles

- As in the working paper, the silence of the level of loss aversion in
  return volatility (step 3) comes from a constant price-dividend ratio in an
  endowment economy. Volatility in excess of dividends comes from changing
  loss aversion (Sections IV.C and V).

## What could not be reconstructed

- The numerical solutions and simulated moments (Section V). Not rerun.
- The limiting step of the sufficiency proof, which the paper leaves
  "available upon request" (footnote 29, p. 50).
