# Reconstruction: Peura and Keppo (2006)

## Source read

- Samu Peura and Jussi Keppo, "Optimal Bank Capital with Costly
  Recapitalization", *Journal of Business* 79(4), 2006, pp. 2163-2201 of
  the copy supplied. Evidence tag [P]: 38 page images, read in full,
  including the appendix and the references.
- Supplied by the author on Google Drive (folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`, file `optimal_risk_control.pdf`),
  uploaded 2026-10-12. Cached at `~/.cache/wealth_garch/peura_keppo_2006.pdf`,
  sha256 `112de752eccbf5a81642ea1af52484ebe97daf6e6bbad2d4ebcc566f714fe5ab`.
- No text layer. OCR text of each page, made by `lit/ocr_pages.py`
  (rapidocr_onnxruntime 1.2.3, 200 dpi), is cached at
  `~/.cache/wealth_garch/peura_keppo_2006.ocr/`. Printed page = PDF page +
  2162 up to p. 2186; PDF page 24 holds pp. 2186 and 2187; from p. 2188,
  printed page = PDF page + 2163.

## Primitives

- Risk-weighted assets $R_t=R_0e^{rt}$ grow at the risk-free rate $r$
  (eq. 1). Cumulative profit $dY_t=\mu R_tdt+\sigma R_tdW_t$.
- Capital $\hat X_t$ earns $r$, receives profits and issues, and pays
  dividends (eq. 3). Dividends are singular. An issue is ordered at a
  stopping time, arrives after a delay $\Delta$, and costs $KR$ (eq. 5).
- Violation of the 8% minimum means liquidation (eq. 4). Owners discount at
  $r+\rho$, with $\rho$ the wedge between debt and equity finance.

## Derivation chain

1. Prop. 1. The ratio $X_t=\hat X_t/R_t$ follows
   $dX=\mu\,dt+\sigma dW-dL+\text{issues}$, so the problem is
   one-dimensional and the default point is normalised to $X=0$ (excess
   capital over 8%).
2. Prop. 2. The value function satisfies the quasi-variational inequalities
   (13i)-(13v), with the impulse operator $M$ of (11) carrying the delay.
3. Section III. Three regions: order capital on $(0,u_1]$, wait on
   $(u_1,u_2)$, pay dividends on $[u_2,\infty)$. On the waiting region
   $f_1=a_1e^{-d_{1+}(u_2-x)}+a_2e^{-d_{1-}(u_2-x)}$; above $u_2$,
   $f_2=\mu/\rho+(x-u_2)$. Smooth pasting at $u_2$ to second order gives
   $a_1,a_2$ (A4) and $f_1(u_2)=\mu/\rho$ (A5).
4. Prop. 3. $(u_1,u_2)$ solve value matching and smooth pasting at $u_1$
   (18i)-(18ii), with $M$ in closed form (14) by the reflection principle
   (A8)-(A9). Existence by Lemma 4, concavity and verification by Lemma 5.
5. Limiting cases. Above critical $K$ or $\Delta$, no issuance and the
   Milne and Robertson (1996) barrier $u_0$ (17). With $\Delta=0$, Prop. 4:
   the issue barrier is at 0 and the dividend barrier $\hat u<u_0$ solves
   (23). With $K=0$, the delay alone keeps $u_1>0$.
6. Section IV (numerical). $u_2$ is nondecreasing in $K$ and $\Delta$,
   $u_1$ nonincreasing in $K$ and non-monotone in $\Delta$, and issue size
   $\approx u_2-u_1$ rises with $K$.
7. Section V. $\mu,\sigma$ estimated per bank from annual Compustat
   accounting returns, 61 US commercial banks, 1983-2002; $\rho=4\%$,
   $K=0.25\%$, $\Delta=0.5$ years chosen by plausibility. Model barrier
   against actual buffer: 2.4% against 5.0% overall; explanatory power only
   in the subsample with above-average loan loss provisions (R^2 about 0.3,
   rising to 0.40 when only large banks keep the issue option). Implied
   volatilities replicate the average buffers of large and small banks.

## Roles

- A bank capital model with singular dividends and delayed, costly
  recapitalisation, with no risk choice and no regulatory cap on exposure.
  The exposure is fixed by risk-weighted assets, so the corner question of
  our G2 does not arise.
- The closest published baseline for a backtest: it is calibrated to bank
  data and judged by its fit to observed capital ratios (PK-Q1, PK-Q14).

## What could not be reconstructed

- The proof of Prop. 1, which the paper states is available from the
  authors (p. 2170).
- The sufficiency of the first-order conditions, also available from the
  authors (n. 11).
- Lemma 2's sign claims on the partial derivatives of the first-passage
  probability, asserted without proof.
- The calibration numbers themselves; the bank-level data are not supplied.
