# Reconstruction: Mankart, Michaelides and Pagratis (2020)

## Source read

- Jochen Mankart, Alexander Michaelides and Spyros Pagratis, "Bank Capital
  Buffers in a Dynamic Model", draft of May 2016 presented at the NBER
  Summer Institute 2016. 59 PDF pages: a title page, an abstract page, then
  pages 1 to 57 (printed page = PDF page - 2). Read in full. Evidence tag
  [F] for this version. The published article, *Financial Management*
  49(2):473-502, 2020, was not read.
- Downloaded from
  https://conference.nber.org/confer/2016/SI2016/EFEL/Mankart_Michaelides_Pagratis.pdf
  on 2026-10-12; not yet in the Drive folder. Cached at
  `~/.cache/wealth_garch/mankart_michaelides_pagratis_2016.pdf`, sha256
  `3a0e6caf7edbfe7a53b4a0fb2a14b0893b3d4f8d8d20a60e77008bbbd5cd8cc0`.

## Primitives

- Discrete time, quarterly. A bank holds loans $L$ and liquid assets $S$,
  funded by insured deposits $D$, wholesale funding $F$ and equity $E$
  (Table 3, (3)).
- Shocks: loan write-offs AR(1) with regime-dependent moments (1); i.i.d.
  log deposit growth by regime (2); a two-state business cycle.
- Two requirements: risk-weighted assets over equity at most $\lambda_w$
  (4) and assets over equity at most $\lambda_u$ (5), with
  $\lambda_w=16.66$, $\lambda_u=33.33$, $\omega_L=1$, $\omega_S=0.2$
  (p. 22). Breach means failure (p. 22).
- No equity issuance (pp. 1-2). Equity grows only by retained after-tax
  profits (8).
- Objective (9): discounted CRRA utility of dividends, with limited
  liability (10)-(11).
- Convex screening cost of new loans (12) and of wholesale funding (13).

## Derivation chain

1. Data facts from Call Reports 1990-2010 by size (Tables 1-2, Figures
   1-3).
2. Value function iteration on normalised loans and equity (Solution
   Appendix).
3. Seven parameters by the Method of Simulated Moments on 11 moments,
   separately for small and large banks (Tables 4-5).
4. Policy functions: dividends rise with equity; precautionary equity
   buffers above the requirements; failure follows long recessions
   (Figures 5-10).
5. Counterfactuals: a tighter risk-weighted limit lowers lending and
   buffers and raises failures; a tighter leverage limit raises lending
   (Figures 13-17).

## Roles in this bundle

- An estimated structural model of US banks in which dividends are paid
  every quarter and smoothed, from the curvature of the objective (MMP-Q4,
  MMP-Q12), not from a barrier. A rival to the barrier models on E3.
- Its buffers are precautionary, from uninsurable risk without issuance;
  it still underpredicts mean equity (MMP-Q7, MMP-Q8).
- No recapitalisation exists in the model (MMP-Q5), so it makes no
  prediction for E2.

## What could not be reconstructed

- The value function solution and the simulated moments.
- The published version's changes.
