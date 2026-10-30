# Reconstruction: Dai, Huang and Keppo (2019)

## Source read

- Min Dai, Shan Huang and Jussi Keppo, "Opaque bank assets and optimal equity
  capital", *Journal of Economic Dynamics and Control* 100:369-394, 2019,
  read as chapter 2 of Shan Huang, "Two stochastic control problems in
  capital structure and portfolio choice", NUS PhD thesis, arXiv:2107.02242v1
  (p. 130: "has been published on Journal of Economic Dynamics and Control").
  Read in full: chapter 2, PDF pp. 24-89, every proof in the text, and the
  whole appendix A.1-A.6, PDF pp. 130-150. Evidence tag [F] for the thesis
  chapter. The published article was not read (DHK-D1).
- Fetched from arxiv.org on 23 October 2026. Cached at
  `~/.cache/wealth_garch/dai_huang_keppo_2019_thesis.pdf`, sha256
  `985a281d96915a841b984e137c1ecdbed0cdf2dd9358c2a2ac84d50d5c73ae50`, 167
  pages. Page numbers below are PDF pages.

## Primitives

- Debt $dD=\mu D\,dt$ (2.2), assets $dY=\alpha Y\,dt+\sigma Y\,dW$ (2.3),
  book equity $E$ with dividends $L$ and issues $s_i$ (2.4), p. 26-27. No
  liability shock: "We ignore debt volatility and debt financing" (p. 26).
- Issuance "with the capital issuances, there is a delay and a fixed cost"
  (p. 27): issues arrive $\Delta$ after the decision and cost $K D$ (2.5),
  p. 29. The issue size is chosen at arrival, "the optimal issuance s equals
  $(u_2-X_\Delta)^+$" (p. 41).
- Shareholders maximise dividends net of issues until liquidation (2.5),
  risk-neutral, discount $\delta>\max(\mu,\alpha)$.
- Liquidation by the regulator below the equity-to-debt ratio $\kappa$
  (p. 28). With noisy accounting $dZ=M\,dt+m\,dB$, $dW\,dB=\rho\,dt$ (2.7),
  the regulator liquidates when the probability of insolvency exceeds
  $a=c_2/(c_1+c_2)$ (2.17), p. 35, giving the liquidation barrier
  $I(S)$ (2.18), p. 36.

## Derivation chain

1. Prop. 2.1.1 (pp. 32-33, A.3): the filter; the posterior variance $S$ is
   deterministic and tends to $m\sigma(1-\rho)$.
2. Full information: HJB (2.20) reduced by homogeneity (2.23). Theorem 2.2.1
   (pp. 39-45): (i) uniqueness of the viscosity solution, proof skipped
   ("Thus we skip it", p. 40); (ii) the barrier solution with dividend
   barrier $u_2$ (smooth fit) and recapitalisation barrier $u_1$, under
   conditions (2.32) checked only numerically (p. 40); existence in A.1,
   verification in A.2. Lemma 2.2.2: the liquidation-value root.
3. Prop. 2.2.3 (p. 46, A.5): value bounds.
4. Partial information: HJB (2.41); weak dynamic programming (Prop. 2.2.4,
   A.6); Theorem 2.2.5, unique viscosity solution, via Lemmas 2.2.6 to 2.2.8
   (supersolution, subsolution, comparison principle), pp. 50-66.
5. Prop. 2.2.9 (pp. 66-68): the shape of $I(S)$ in $S$, and with
   $\Delta=0$ no issue above $I(S)$.
6. Numerics by a penalty method (p. 70); comparative statics in figures and
   Table 2.1 (p. 76); calibration (pp. 77-85), $K$ set from the literature,
   $\delta,\omega,\hat\kappa$ fitted to market equity values, $a=79.93\%$.

## Roles

- Shareholders choose dividends, the timing and size of issues, and treat
  $\kappa$ as a constraint (p. 30). The regulator follows the fixed rule
  (2.17). Bankers who choose the noise are not modelled (p. 31, n. 8).
- The recapitalisation barrier $u_1$ is chosen by the shareholders, not set
  by regulation, which is what makes Table 2.1's column $u_1$ a response of
  an optimal trigger.

## What could not be reconstructed

- Theorem 2.2.1(i), proof skipped; conditions (2.32), numerical only.
- Theorem 2.2.5 is proved on $\Omega_1$ only and $\bar S$ is not defined.
- The averaging grid of Table 2.1 for the rows $\Delta$ and $K$ (DHK-D2).
- Inconsistencies in the source (DHK-D3, DHK-D4).
