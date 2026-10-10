# Reconstruction: Bayraktar, Chevalier, Ly Vath and Wang (2026)

## Source read

- "Tractable bank capital structure: optimal control under Basel III
  constraints", Erhan Bayraktar, Etienne Chevalier, Vathana Ly Vath, Yuqiong
  Wang. arXiv:2603.14557v2 [math.OC], stamped 30 Aug 2026, title page dated
  September 1, 2026. 39 pages. Evidence tag [F]: the full text was read,
  pages 1 to 39 including the appendix proofs and the references.
- Supplied by the author on Google Drive (folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`, file `2603.14557v2.pdf`), uploaded
  2026-10-10. Cached at `~/.cache/wealth_garch/bcvw_2603.14557v2.pdf`, sha256
  `e080555878991988c1978a51b788be16145e8082f59a2be19682441d721cd935`, text
  extracted with `pdftotext -layout`. Printed page numbers equal PDF pages.

## Primitives

- Deposits $dL_t=L_t(\mu_L\,dt+\sigma_L\,dW_t)$ with $\mu_L=\gamma+r_L$
  (eq. 1). A risky asset $dS_t=S_t(\mu\,dt+\sigma\,dB_t)$ (eq. 2).
  $d[W,B]_t=c\,dt$ with $|c|<1$. A risk-free rate $r>0$.
- Assets $X_t=F_t+L_t$, with $F$ shareholders' equity. Controls: the risky
  fraction $\pi_t$ (classical), cumulative dividends $Z$ (singular), issuance
  times and sizes $(\tau_n,\xi_n)$ (impulse). Issuance costs are a dilution
  cost $\kappa F_{\tau-}$ and a proportional cost $\kappa'\xi$. There is no
  fixed issuance cost.
- Solvency $F/(\pi X)\ge a_1$, equivalently $1-1/Y\ge a_1\pi$ with $Y=X/L$.
  Liquidity coverage $(X-a_3\pi X)/(a_2L)\ge1$, equivalently
  $1-a_2/Y\ge a_3\pi$. Together they give the cap
  $\bar\pi(y)=\min\{(1-1/y)/a_1,(1-a_2/y)/a_3\}$ (eq. 4).
- A supervisory distress threshold $\underline y>1$, fixed exogenously. At
  $\underline y$ the bank must recapitalise or liquidate. The trigger is not
  chosen optimally in the main problem. Section 4.4 lets the bank choose it
  from a grid, numerically.
- Discount rate $\rho$, liability-net rate $\rho_L=\rho-\mu_L$.

## Derivation chain

1. Prop. 2.1 (p. 9). Homogeneity of balance sheet and objective, plus the
   change of measure $dP^*/dP=M_t=\exp(\sigma_LW_t-\sigma_L^2t/2)$, reduce the
   problem to one dimension in $Y$, with
   $dY=(Y[\mu(\pi)-\mu_L]+\gamma)dt+\pi Y\sigma\,dB+\sigma_L(1-Y)dW-dZ$ and
   value $\hat v(\ell,x)=\ell\,v(x/\ell)$. The paper states that fixed issuance
   costs "would generally break the one-dimensional reduction" (p. 9).
2. The generator (p. 10) has diffusion coefficient
   $\pi^2\sigma^2y^2+2\pi c\sigma\sigma_Ly(1-y)+\sigma_L^2(1-y)^2$, the
   quadratic variation of the SDE in step 1.
3. Assumption 3.1: $\rho>\max(\mu_L,r+(\mu-r)^+/\bar a)$ with
   $\bar a=\max(a_1,a_3)$, using $\lim_{y\to\infty}\bar\pi(y)=1/\bar a$
   (p. 10).
4. Prop. 3.1: $y-1\le v(y)\le y+K$, where $K$ is a constant built from the
   parameters. It is not an issuance cost.
5. Remark 3.1 (p. 11). At $y=1$ the cap forces $\pi=0$, the diffusion
   vanishes and the drift is $r-r_L$. The point is locally repelling when
   $r>r_L$ and attracting when $r<r_L$. The paper avoids it by intervening at
   $\underline y>1$.
6. Prop. 3.2: $v$ is the unique continuous viscosity solution of the
   variational inequality (6) with linear growth. Lemma 5.2's proof (p. 32)
   uses $\sigma_Y^2=(\pi\sigma y+c\sigma_L(1-y))^2+(1-c^2)\sigma_L^2(1-y)^2$,
   bounded below by $(1-c^2)\sigma_L^2(y-1)^2$.
7. Prop. 3.3: $v$ is concave. Proof (p. 34) writes the control as
   $U=\pi Y$ with $U\le u(y)=\min\{(y-1)/a_1,(y-a_2)/a_3\}$, concave in $y$, so
   the state-control set is convex and the coefficients are affine. The
   impulse operator preserves concavity, and an iteration gives concavity of
   $v$.
8. Cor. 3.1 and Thm 3.1. Dividends are paid at a barrier $y^*$ with smooth
   fit there and $\rho_Lv(y^*)=\gamma-(\mu_L-\mu^*(y^*))y^*$. Issuance occurs
   only at $\underline y$, jumping to $y^*_{\text{post}}$ with
   $v'(y^*_{\text{post}})=1/(1-\kappa')$.
9. Numerics (Section 4). Baseline $r=0.02$, $\mu=0.04$, $\mu_L=0.03$,
   $\rho=0.12$, $\gamma=0.02$, $\sigma=0.08$, $\sigma_L=0.03$, $c=0.20$,
   $\kappa=0.01$, $\kappa'=0.02$, $(a_1,a_2,a_3)=(0.045,0.05,0.30)$,
   $\underline y=1.02$. Switching point $\hat y=(a_3-a_1a_2)/(a_3-a_1)=1.168$
   (p. 15). $y^*=1.4282$, $y^*_{\text{post}}=1.1828>\hat y$.

## Roles

- $\underline y$ is a supervisory threshold, not an optimally chosen
  trigger. Smooth fit is proved at the dividend barrier $y^*$, not at a
  recapitalisation trigger.
- The cap enters only through $\bar\pi(y)$ (p. 15). The regulatory
  parameters are not preference parameters. The objective is risk neutral,
  with no asymmetric penalty.
- $u(y)=\bar\pi(y)\,y$ is the object `CapGeometry.capU` of this bundle.

## What could not be reconstructed

- The numerical tables and the Monte Carlo frontier (Sections 4.2 to 4.5).
  The solver was not rerun.
- The step in Lemma 5.2 that $P(T_\varepsilon\ge T_y)\to0$ "by continuity of
  the scale functions of $Y^*$" (p. 31). The scale functions are not given.
- The constants in the uniqueness argument of Prop. 3.2 beyond the outline on
  pp. 33 to 34.
