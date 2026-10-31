# Reconstruction: Andries and Haddad (2020)

## Source read

- "Information Aversion", Marianne Andries (Toulouse School of Economics)
  and Valentin Haddad (UCLA and NBER). *Journal of Political Economy*
  128(5), 2020, pp. 1901 to 1939, electronically published 13 April 2020.
  The version read is the published article as page images, 39 PDF pages
  with no text layer (produced by LibreOffice 7.5 on 31 October 2026 from
  the author's copy). Evidence tag [F] for the main text, Sections I to VI
  and the references. The appendices, which hold every proof, are online
  (p. 1918, "apps. A to C are available online") and were not supplied.
- Supplied by the author through the session's upload folder on
  2026-10-31. Cached at `~/.cache/wealth_garch/andries_haddad_2020.pdf`,
  sha256
  `871deba550e4fc84a1d5db25fcf0c311917d1cd4d40d093b5fba63a99fc1f015`.
  Printed page $n$ is PDF page $n-1900$. The OCR text, made by
  `lit/ocr_pages.py` at 200 DPI, is in
  `~/.cache/wealth_garch/andries_haddad_2020_ocr/`, whose `SOURCE` file
  carries the PDF's sha256.

## Primitives

- Preferences (Definition 1, p. 1908). A recursive utility
  $\mathcal V_t=\{C_t^{1-\alpha}dt+e^{-\rho dt}[\mu_\theta(\mathcal V_{t+dt}\mid\mathcal F_t)]^{1-\alpha}\}^{1/(1-\alpha)}$
  (eq. 1), with $\rho>0$ the rate of time discount and $1/\alpha>0$ the
  elasticity of intertemporal substitution. The certainty equivalent
  $\mu_\theta$ is Gul's (1991) disappointment aversion (eq. 2): the unique
  solution of
  $\mu=\big(\int x\,dG+\theta\int_{x<\mu}x\,dG\big)/\big(1+\theta\int_{x<\mu}dG\big)$,
  with $\theta\ge0$ the coefficient of disappointment aversion. The
  reference point is $\mu$ itself (p. 1909). The piecewise-linear form
  makes the kink the only source of risk aversion (p. 1909).
- Information (Section II.A). The agent chooses her filtration
  $\{\mathcal F_t\}$, learns in a Bayesian way and does not forget. In
  Section III the choice is restricted to observation times,
  $\mathcal F_t=\bar{\mathcal F}_{\tau(t)}$ with $\tau(t)\le t$ the last
  observation (eq. 4).
- Environment (Section III.A). Merton's (1969) problem with a risk-free
  rate $r$ and a geometric Brownian motion with drift $g>r$ and volatility
  $\sigma$. Budget $dW_t=-C_tdt+S_tdX_t+r(W_t-S_tX_t)dt$, $W_t\ge0$
  (eq. 3). Between observations consumption is deterministic and financed
  from the risk-free account. No leverage, $S_tX_t\le W_t$.

## Derivation chain

1. Proposition 1 (p. 1910). Across all filtrations for a final payoff $X$
   with the same initial information, the agent prefers those revealing
   $X$ at a single date, and is indifferent to which date. The mechanism
   (p. 1910) is Gul's resolution of the Allais paradox, that adding risk to
   a certain lottery hurts more than adding the same risk to an uncertain
   one, so an intermediate signal that moves risk to a date without prior
   uncertainty lowers value. The proof is in the online appendix.
   `AndriesHaddad.information_aversion_example` gives the inequality for
   one example, the uniform payoff on $\{0,1,2,3\}$ revealed at once
   against revealed by halves, where the gap is $\theta/(4+2\theta)$
   (`AndriesHaddad.oneShot_sub_compound`). At $\theta=0$ the gap is zero
   (`AndriesHaddad.control_no_aversion_at_theta_zero`).
2. Proposition 2 (p. 1911). For one intermediate signal of any
   nondegenerate informativeness, there is a filtration with no cost and
   one with strictly positive cost. "Our agent fears the flow rather than
   the quantity of information" (p. 1911). Rankings do not respect the
   Blackwell order (fn. 12).
3. Lemma 1 (p. 1914, eq. 5). Homogeneity of degree one and i.i.d. returns
   make the value proportional to wealth and the observation interval
   constant. The problem reduces to choosing $T$, the wealth $\mathcal C$
   set aside for consumption and the risky share $\mathcal S$, with
   $\mathcal S+\mathcal C\le1$.
4. Definition 2 and Lemma 2 (p. 1914). The certainty-equivalent rate
   $v(T)$, $\exp(v(T)T)=\mu_\theta(X_{t+T}/X_t\mid\mathcal F_t)$ (eq. 6),
   decomposes as $g+\bar v(T)$, with $\bar v$ negative, decreasing in
   $\sigma$ and $\theta$ and increasing in $T$. Its expansion is
   $v(T)=g-\kappa(\theta)\sigma/\sqrt T+o(1/\sqrt T)$ (p. 1915).
5. Proposition 3 (p. 1915, eq. 7). Given $T$, $\mathcal S=1-\mathcal C$ if
   $v(T)>r$ and $\mathcal S=0$ otherwise, and
   $\mathcal C=1-\exp[(-\rho/\alpha+\frac{1-\alpha}{\alpha}v(T))T]$,
   increasing in $T$. `AndriesHaddad.consumption_increasing_in_T` is the
   monotonicity when the exponent's coefficient is negative, which the
   paper's finite-utility condition $g-r<\rho/(1-\alpha)-g$ (p. 1913,
   for $\alpha<1$) is meant to secure. We did not derive that implication.
6. Proposition 4 (p. 1916, eq. 8). $T^*$ is the unique solution of
   $dv/d\log T=(\rho/(1-\alpha)-v)[1-f(\rho/(1-\alpha)-r,T)/f(\rho/(1-\alpha)-v,T)]$
   with $f(x,T)=x/(e^{(1-\alpha)xT/\alpha}-1)$; $T^*>0$ and $v(T^*)>r$.
   The left side is the marginal cost of information, the elasticity of
   $v$ in $T$, and the right side the opportunity cost of saving at $r$
   rather than $v$. Expansion in $\theta$ (eq. 9):
   $T^*\sqrt{T^*}=\frac{\frac12\sqrt{2\pi}\,\theta\sigma}{\frac12(g-r)\{[\rho+(\alpha-1)g]/\alpha\}}+o(\theta)$.
7. Proposition 5 (p. 1917). $T^*$ rises in $\theta$ and $\sigma$ and
   falls in $g$. Proposition 6 (p. 1918). After a rise in $\sigma$ with a
   compensating rise in $g$, inattention rises iff $v(T^*)$ rises by less
   than a multiplier $\lambda>1$; with $v(T^*)$ held constant it rises.
8. Section III.B.3, the fixed-cost benchmark. Definition 3 (eq. 10) uses
   CRRA with risk aversion $\gamma$ and a cost $\phi_o$ of wealth per
   observation. Then $dv/d\log T=-\frac1T\log(1-\phi_o)$ (eq. 12), so the
   marginal cost of attention is independent of the asset and of
   preferences. Proposition 7 (p. 1920): inattention rises iff the Sharpe
   ratio $(g-r)/\sigma$ falls.
9. Calibration (p. 1923). $r=2\%$, $g-r=9.6\%$, $\sigma=14.6\%$,
   $\alpha=0.4$, $\rho=0.22$, $\theta=0.8$ give $T^*\approx1.1$ years. The
   fixed-cost model with $\gamma=4.5$ and $\phi_o=118$ basis points gives
   the same interval. Elasticities of $T$ to $g-r$ and $\sigma$ are
   $-0.63$ and $0.79$ under information aversion against $-0.95$ and
   $0.95$ under fixed costs (p. 1925).
10. Section IV, Proposition 8 (p. 1927, eq. 13). Alerts after large falls
    give first-order gains $AX_l$; alerts after rises have an effect of
    order $\exp(-B\log(X_h)^2)$. The gain comes from relaxing
    $\mathcal S+\mathcal C\le1$ (p. 1928). Calibrated $X_l^*=0.77$
    (p. 1929).
11. Section V, Proposition 9 (p. 1930). After an unavoidable signal at
    $\xi$, the next observation time falls in the level $w(T\mid\mathcal F_\xi)$
    and rises in its slope $dw/dT$, given by eq. (15). Bad news raises the
    probability of disappointment in the denominator of (15) and so
    delays observation, the ostrich effect. Section V.B: with one
    unwanted signal per asset the undiversified portfolio is preferred
    above a correlation of 37.5 percent, with two per asset above 22.5
    percent (pp. 1934 to 1935).

## What the paper implies for the volatility of prices

Nothing directly. The price $X_t$ is an exogenous geometric Brownian
motion and $\sigma$ is an input. The economy is not closed, so the
preference reaches attention ($T^*$), the risky share ($\mathcal S$) and
welfare, and not the price process. The channel the paper establishes runs
from volatility to information processing, with $T^*$ rising in $\sigma$
(Proposition 5), which is the reverse of the direction this bundle asks
about.

## Roles

- The paper is a preference-based account of why updating is infrequent.
  Disappointment aversion makes every intermediate signal costly, so the
  agent chooses to observe rarely, and more rarely when volatility is high
  or after bad news. That is one answer to the question Pagel (2016) leaves
  open in her Appendix C, where sluggish updating is assumed. The other
  candidate answer, a capacity constraint on perception (Woodford 2012,
  Frydman and Jin), is the one this bundle is exploring. The two differ in
  what they change. Here the agent's update rule is Bayesian and the
  filtration is coarse by choice. Under efficient coding the agent attends
  continuously and the perceived signal is distorted.
- The paper's results on information structure (Propositions 1, 2 and 8)
  bear on any model in which the agent receives frequent signals. An
  information-averse agent would pay to avoid the signals an adaptive
  encoder needs. If both features were put in one model they would pull
  against each other, which is a reason to keep them in separate papers.
- The kink here is at the certainty equivalent, as in Routledge and Zin
  (2010), which the paper cites for equilibrium prices (p. 1906). The
  paper does not study prices.

## What could not be reconstructed

- Every proof. Appendices A to C are online and were not supplied. The
  Lean file therefore proves only what follows from eq. (2) for finite
  lotteries and from the sign of eq. (7)'s exponent.
- Lemma 2's decomposition and the expansion of $v(T)$, Proposition 4's
  uniqueness, Propositions 5 to 9, the fixed-cost derivations of eqs. (11)
  and (12), and the diversification example of Figure 3.
- The calibrated numbers (1.1 years, 118 basis points, $X_l^*=0.77$, the
  elasticities, the correlation thresholds).
