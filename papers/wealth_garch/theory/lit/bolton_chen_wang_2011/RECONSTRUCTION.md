# Reconstruction: Bolton, Chen and Wang (2011)

## Source read

- Patrick Bolton, Hui Chen and Neng Wang, "A Unified Theory of Tobin's q,
  Corporate Investment, Financing, and Risk Management", accepted
  manuscript marked "Forthcoming, Journal of Finance". 45 PDF pages: a
  title page, an abstract page, then pages 1 to 43 (printed page = PDF page
  - 2). Read in full. Evidence tag [F] for this version. The published
  version (*Journal of Finance* 66(5):1545-1578, 2011, as BCVW cites it)
  was not read.
- Supplied by the author on Google Drive (folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`, file `bcw_jf_forthcoming.pdf`),
  uploaded 2026-10-12. Cached at `~/.cache/wealth_garch/bcw_2011.pdf`,
  sha256 `8f70e72e42be16b8d0906e53a28d0e4aabb1f7eef94f256cb08a667153ac9600`.

## Primitives

- AK technology with quadratic adjustment cost $g(i)=\theta i^2/2$, capital
  $dK=(I-\delta K)dt$, productivity $dA=\mu dt+\sigma dZ$.
- Cash $dW=dY+(r-\lambda)Wdt+dH-dU$, with carry cost $\lambda$.
- Equity issuance at fixed cost $\phi K$ and marginal cost $\gamma$;
  liquidation value $lK$.

## Derivation chain

1. Homogeneity: $P(K,W)=Kp(w)$, $w=W/K$, ODE (13), investment (14).
2. Payout at the upper barrier: $p'=1$, $p''=0$ (16)-(17).
3. At $w=0$: liquidate ($p(0)=l$) or issue $mK$, with $p(0)=p(m)-\phi-(1+\gamma)m$
   and $p'(m)=1+\gamma$ (19)-(20).
4. Section III: calibrated to firm-level parameters; stationary
   distributions (Table II).
5. Sections IV-V: hedging with margin requirements, and a credit line.

## Roles

- A non-financial firm. Not a bank model: no regulatory capital, no
  deposits. It shares with Peura and Keppo and with M4 to M6 the fixed
  issuance cost proportional to size, and the lump issue to a single
  target.

## What could not be reconstructed

- Appendices B and D (not in the copy).
- The numerical solutions (Figures 2 to 8, Table II).
