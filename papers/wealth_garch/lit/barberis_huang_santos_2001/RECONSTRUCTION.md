# Reconstruction: Barberis, Huang and Santos (2001)

## Source read

- "Prospect Theory and Asset Prices", Nicholas Barberis, Ming Huang, Tano
  Santos. NBER Working Paper 7220, July 1999. 50 pages. Evidence tag [F] for
  this version: the full text was read, including the appendix, tables and
  figure captions.
- The citation is the published article, *Quarterly Journal of Economics*
  116(1):1-53, 2001. The published version was not read. Every page number
  below is the working paper's, and nothing is attributed to the published
  version (BHS-D1).
- Supplied by the author on Google Drive (folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`, file `w7220.pdf`), uploaded
  2026-10-10. Cached at `~/.cache/wealth_garch/bhs_w7220.pdf`, sha256
  `dca360482ae82da568a5b366ad699910258fa3cb3e940f5a4f4d6f0bf3cd6c4b`. Printed
  page numbers equal PDF pages. The abstract is on PDF page 2.

## Primitives

- A Lucas (1978) endowment economy. Log consumption growth is i.i.d. normal,
  $\log(\bar C_{t+1}/\bar C_t)=g+\sigma\varepsilon_{t+1}$ (eq. 1), and the
  dividend equals aggregate consumption.
- Preferences (eq. 2): power utility over consumption plus a term
  $b_t\,v(X_{t+1})$ over the gain or loss $X_{t+1}=S_tR_{t+1}-S_tR_{f,t}$ on
  the risky holding (eq. 3).
- $v(X)=X$ for $X\ge0$ and $\lambda X$ for $X<0$ (eq. 4), piecewise linear and
  kinked at the origin. $\lambda=2.25$. The paper makes $v$ linear on both
  sides "for simplicity" (p. 11).
- Linearity gives $v(S_tR_{t+1}-S_tR_{f,t})=S_t\hat v(R_{t+1})$ (eq. 5), so the
  objective is (7) with $\hat v$ of (8). The scaling $b_t=b_0\bar C_t^{-\gamma}$
  (eq. 9) keeps ratios stationary.

## Derivation chain

1. Section 2, Prop. 1 (p. 13). There is an equilibrium with a constant riskless
   rate (eq. 11) and a constant price-dividend ratio $f$ (eq. 12). A constant
   $f$ with i.i.d. dividend growth makes returns i.i.d. (eq. 10).
2. Eq. (15), p. 16. $R_{t+1}=\frac{1+f}{f}e^{g+\sigma\varepsilon_{t+1}}$, so
   $\log R_{t+1}=\log\frac{1+f}{f}+g+\sigma\varepsilon_{t+1}$. The volatility
   of log returns equals $\sigma$ for every $f$, so for every $\lambda$ and
   $b_0$, which enter only through $f$. The paper states this as "the
   volatility of log returns in this model is equal to the volatility of log
   consumption growth, namely 3.79%."
3. Consequence (p. 15, Table 2). With $b_0=2$ the equity premium is 0.91% and
   return volatility 3.79%. As $b_0\to\infty$ the premium is bounded by 1.2%.
   "loss-aversion by itself cannot explain the equity premium."
4. Section 3. A benchmark $Z_t$ and $z_t=Z_t/S_t$ enter $\hat v(R_{t+1},z_t)$:
   prior gains ($z_t\le1$) cushion losses (eq. 18), prior losses ($z_t>1$)
   raise the penalty to $\lambda(z_t)=\lambda+k(z_t-1)$ (eqs. 19 to 20). The
   benchmark moves sluggishly (eqs. 21 to 22, memory $\eta$).
5. Prop. 2 (p. 26). A one-factor equilibrium with $f=f(z_t)$. Returns
   $R_{t+1}=\frac{1+f(z_{t+1})}{f(z_t)}e^{g+\sigma\varepsilon_{t+1}}$ (eq. 30).
   Because $f$ now moves with $z$, risk aversion changes over time and returns
   become more volatile than dividends (p. 27). Table 3: volatility 13.3%,
   premium 4.1%. Table 6: lower $\eta$ lowers volatility.

## Roles

- The silence of the level of $\lambda$ in return volatility (step 2) is a
  property of the Section 2 equilibrium, where $f$ is constant because growth
  is i.i.d. and preferences are homogeneous. It is not a statement about a
  firm's control problem.
- Volatility in Section 3 comes from time variation in effective loss
  aversion, through $f(z)$, not from its level.

## What could not be reconstructed

- The numerical solution $f(z)$ of eq. (32) and the simulated moments
  (Section 3.3). The iteration on p. 29 was not rerun.
- The claim that results "do not depend crucially" on the functional form of
  $\lambda(\cdot)$ (p. 23). No robustness table is given.
