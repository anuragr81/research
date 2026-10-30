# Reconstruction: Li, Yu and Zhang (2023)

## Source read

- "Optimal Consumption with Loss Aversion and Reference to Past Spending
  Maximum", Xun Li, Xiang Yu, Qinyi Zhang. arXiv:2108.02648v4 [math.OC],
  28 Feb 2023. 38 pages. Evidence tag [F]: the full text was read,
  including Section 5 (proofs) and the references.
- Supplied by the author on Google Drive (folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`, file `2108.02648v4.pdf`), uploaded
  2026-10-10. An older copy in the same folder (`2108.02648.pdf`, uploaded
  2026-07-29) was not used. Cached at
  `~/.cache/wealth_garch/lyz_2108.02648v4.pdf`, sha256
  `5a02fd5d1af9ca926586e0b3e0af9d5a6540fb73841ee324cead686e754d1390`. Printed
  page numbers equal PDF pages. Whether a journal version exists was not
  checked, and none is cited.

## Primitives

- A Black-Scholes market, riskless rate $r$, risky drift $\mu>r$, volatility
  $\sigma$, Sharpe ratio $\kappa=(\mu-r)/\sigma$. Wealth
  $dX=(rX+\pi(\mu-r)-c)\,dt+\pi\sigma\,dW$, with no bankruptcy.
- An individual agent maximises $E\int_0^\infty e^{-\rho t}U(c_t-\lambda H_t)\,dt$
  with $H_t=\max\{h,\sup_{s\le t}c_s\}$ the running maximum of consumption
  and $\lambda\in(0,1)$ the reference degree (eq. 2.1).
- $U$ is the two-part power utility (eq. 1.1), $x^{\beta_1}/\beta_1$ on gains
  and $-k(-x)^{\beta_2}/\beta_2$ on losses, $0<\beta_1,\beta_2<1$, $k>0$. "The
  utility is an S-shaped function on R" (p. 3).
- From Section 3 on, $\rho=r$.

## Derivation chain

1. Section 2.2. For fixed $h$, the concave envelope $\tilde U(\cdot,h)$ of
   $U(c-\lambda h)$ on $c\in[0,h]$ is linear on $[0,z(h)]$ and equal to $U$ on
   $[z(h),h]$, with $z(h)$ the tangent point of eq. (2.2).
2. Prop. 2.1 (concavification). The problem with $\tilde U$ has the same
   optimum, proved in Section 5.4 after the verification.
3. The auxiliary HJB variational inequality (2.6), reduced by the first-order
   condition in $\pi$ to (2.7), splits into regions by the marginal value
   $\tilde u_x$ against the thresholds $y_1(h)\ge y_2(h)>y_3(h)$ (eq. 3.1).
4. The dual transform in $x$ linearises the equation to (3.10). Smooth fit at
   $y_1(h)$ and $y_2(h)$, the free boundary $v_h=0$ at $y_3(h)$, and the
   boundary conditions (3.11) to (3.13) give the solution (3.15), with
   coefficients $C_2(h),\dots,C_6(h)$ implicit in $h$ (3.16). Assumption (A1)
   is used for convexity and verification.
5. Theorem 3.1 (verification, proof in Section 5.2) and Corollary 3.1 give the
   feedback controls in the primal variables, with wealth thresholds
   $x_{\text{zero}}(h)\le x_{\text{aggr}}(h)\le x_{\text{lavs}}(h)$ (eq. 3.19).
   Optimal consumption jumps from zero to above $\lambda H$, because $U$ is
   risk-loving on losses (p. 4, p. 15).
6. Corollary 4.1 (proof in Section 5.6). As wealth grows along
   $x_{\text{lavs}}(h)$, $c^*/x\to L_1$ and $\pi^*/x\to L_2$. As $\lambda\to0$
   they recover Merton's $\pi^*/x=(\mu-r)/(\sigma^2(1-\beta_1))$ (p. 34).
7. Remark 4.1 (p. 13). The limits "sensitively" depend on $\lambda$,
   $\beta_1$, $\beta_2$ and $k$.
8. Remark 4.2. When $\beta_1=\beta_2$, $\tilde u(x,h)=h^{\beta_1}\tilde u(x/h,1)$
   and the problem reduces to one dimension.

## Roles

- An individual's consumption and investment, not a firm's capital. No
  regulatory constraint on the risky position, no impulse or singular
  control of the state, and no volatility of a capital ratio.
- The concave envelope is needed because $U$ is convex on losses. The
  shortfall penalty of PROOFS_v2 is kinked-linear and concave, so the
  bundle's model needs no envelope.

## What could not be reconstructed

- The asymptotic orders of Lemma 5.1 in full detail, and Lemmas 5.2 and 5.3,
  which the paper proves by reference to Deng, Li, Pham and Yu (2022).
- The numerical figures (Section 4).
- The exact grouping of the limit formulas on pp. 33 to 34, which is garbled
  in the text layer (LYZ-D1 records only what the reading needs).
