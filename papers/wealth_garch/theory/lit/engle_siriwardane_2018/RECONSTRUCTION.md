# Reconstruction: Engle and Siriwardane (2018)

## Source read

- "Structural GARCH: The Volatility-Leverage Connection", Robert F. Engle and
  Emil N. Siriwardane. *Review of Financial Studies* 31(2):449-492, 2018,
  doi 10.1093/rfs/hhx099. The published version, downloaded from
  academic.oup.com, with a cover page dated September 2017. 46 PDF pages,
  printed pages 449 to 492. Evidence tag [F]: the full text was read,
  including Appendices A to D. The Online Appendix was not supplied and was
  not read.
- Supplied by the author on Google Drive (folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`), uploaded 2026-10-10. Cached at
  `~/.cache/wealth_garch/engle_siriwardane_2018.pdf`, sha256
  `0e7aae3d7256bd4630f478ec56f3eff698ee6a23ecfd7e4c4488503f853af090`. Printed
  page $n$ is PDF page $n-447$.

## Primitives

- Equity is a call option on the firm's assets (Merton 1974),
  $E_t=f(A_t,D_t,\sigma_{A,t},\tau_t,r_t;\theta_p,\theta_r)$ (eq. 4).
- Asset returns follow a GJR process. Equity returns are asset returns
  amplified by a leverage multiplier,
  $r_{E,t}=LM_{t-1}\,r_{A,t}$ with
  $LM=(LM^{BSM}(D/E,\sigma_A^\tau,\tau,r))^\phi$ (eqs. 6 and 7).

## Derivation chain

1. Appendix A. Itô's lemma on the option formula gives equity returns
   $dE/E=LM\cdot dA/A+$ (vega, convexity and jump terms) (A5 to A9). Dropping
   all but the first term, justified in the Online Appendix, gives
   $\text{vol}(dE/E)\approx LM\cdot\sigma_A$ (A10), the basis of eq. (5).
2. Eq. (6). The multiplier is modelled as $(LM^{BSM})^\phi$, a transformation
   of the Black-Scholes-Merton multiplier and not an assumption that BSM
   holds. $\phi=0$ nests a plain GJR model.
3. Eq. (8). $h_{E,t}=LM_{t-1}^2h_{A,t}$, the variance version of
   $r_E=LM\,r_A$.
4. Section 2. QMLE on 91 financial firms, 1998 to June 2016. Median
   $\phi=0.68$. The median leverage multiplier is around 3.
5. Section 3.1. Precautionary capital, the equity a firm must raise today to
   meet a capital ratio $k$ in a crisis with confidence $c$ (eqs. 10 to 13).
6. Section 3.2. Volatility asymmetry, "negative equity returns predict higher
   future volatility" (p. 473), is measured by $\rho(|x_t|,x_{t-1})$ and by
   the GJR $\gamma$, for equity (E), asset (A) and idiosyncratic asset (I)
   returns. Median $\rho_A/\rho_E=0.97$, so leverage explains about 3%.
   Median $\gamma_A/\gamma_E=0.86$, so about 14%. Median $\rho_I/\rho_E=0.17$,
   so market exposure accounts for "around ... 80%" (ES-D1). The conclusion
   favours the risk-premium explanation (French, Schwert and Stambaugh 1987)
   over mechanical leverage (Black 1976, Christie 1982).

## Roles

- The decomposition in step 6 is of an empirical return-volatility
  asymmetry, separating a structural part (mechanical leverage) from
  exposure to priced risk. The residual is assigned to risk premia, not to
  preference.
- Their asymmetry is the response of next-period variance to the sign of
  news. It is not a ratio of variances in two regions of a state variable
  (ES-D4).

## What could not be reconstructed

- The estimation itself (data from Datastream, OptionMetrics, TAQ).
- The Online Appendix arguments for dropping the higher-order terms in (A9),
  and its robustness checks.
- The precautionary-capital simulation of Appendix D.
