# Reconstruction: Black and Perold (1992)

## Source read

- Fischer Black and André F. Perold, "Theory of constant proportion portfolio
  insurance", *Journal of Economic Dynamics and Control* 16(3-4):403-426,
  1992, PII 0165-1889(92)90043-E. The published article, journal pages
  403-426, read in full including the appendix. Evidence tag [F].
- Supplied by the author on Google Drive on 23 October 2026 (file
  `12Az94OgOB92_mfmZCEkrh15gP2swn718`, "28665427.pdf"). Cached at
  `~/.cache/wealth_garch/black_perold_1992.pdf`, sha256
  `c25eb94e974e09fabecdf1aefa2d9d9d43a39be70dd07d1de4f51d1a697b97dd`.
  A scan with an OCR text layer. Printed page = PDF page + 402.

## Primitives

- Two assets, an active (risky) asset $A$ and a reserve asset $R$, which may
  be risky and "closely tracks a liability stream" (p. 404). The numeraire
  is shares of the reserve asset, so the active asset becomes the index ratio
  $S=A/R$ (pp. 407-408).
- Wealth $W$, a floor $F$ fixed in reserve-asset shares, the cushion
  $C=W-F$, the exposure $E$ invested in the active asset.
- The rule (2.1), $E=\min\{mC,bW\}$, with multiple $m>1$ and maximum
  leverage ratio $1\le b<m$ (p. 406). Section 2.1 reduces $b$ to one by a
  change of active asset.

## Derivation chain

1. Section 3, discrete rebalancing away from the limit. Prop. 1, the cushion
   after $n$ trades. Prop. 2, under geometric Brownian motion and continuous
   rebalancing $C(t)=C_0\exp(-\tfrac12(m^2-m)\sigma^2t)(S(t)/S_0)^m$, proved
   in A.1 from $d\ln C=m\,d\ln S-\tfrac12(m^2-m)\sigma^2dt$, so the
   log-cushion volatility is $m\sigma$.
2. The borrowing limit. At the limit "the cushion grows only linearly in S"
   (p. 411); leaving the limit resets the cushion to $F/(m-1)$. Prop. 3, the
   only effect of time at the limit is to reduce the volatility cost.
3. Section 4, proportional transaction costs, Prop. 4.
4. Section 5, large multiples. Prop. 5, CPPI tends to stop-loss; Props. 6-8,
   expected return is not monotonic in the multiple.
5. Section 6, perpetual American calls, Props. 9-10.
6. Section 7, Prop. 11. With a utility linear below $c^*$ and of constant
   relative risk aversion $\lambda$ above, and consumption bounded below,
   the optimal investment is CPPI with floor $F$, multiple $m$ and leverage
   limit $b$; the limit binds above $W^*=F\,m/(m-b)$, the multiple depends on
   the utility only through the impatience $\rho$, and $b$ exceeds one for
   small $\lambda$ (p. 421).

## Roles

- A portfolio rule for an investor, not a bank. The floor is a liability the
  reserve asset tracks, so floor and safe holdings move together.
- The two slopes of the rule, $m$ and $b$, are the investor's, set by
  preferences in Prop. 11.

## What could not be reconstructed

- The proof of Prop. 11 is sketched (A.7); the constants $P'$, $Q'$ and the
  root $\lambda'$ are garbled in the text layer and were not rederived.
- The Laplace-transform proof of Prop. 8 (A.5) was read, not rederived.
