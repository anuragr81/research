# Reconstruction of Fryer and Loury, "Categorical Redistribution in Winner-Take-All Markets"

**Source read.** NBER Working Paper 10104 (November 2003), read in full [F];
text cached at `~/.cache/entry_contest/w10104.txt`. The published version,
*Journal of Economic Inequality* 3(3), 263-280 (2005), is not checked.

**Why this paper.** The binary-investment search of 13 October 2026 found it as
a candidate for C12: a blind rule that creates a pool of contestants who exert no
effort.

## Model (Section 2)

There is a continuum of workers with private constant marginal costs `c ~ F`.
Each chooses an effort before being paired at random with another worker. The
employer hires the higher effort at wage `ω`. Two categories have cost laws
`F_1` and `F_2`, and Definition 1 makes `f_1/f_2` decreasing, so category 1 is
advantaged.

## Results

1. **Proposition 1.** The unique equilibrium has `e*(c) = ω ∫_c^{c_max} dF(y)/y`.
   Under laissez-faire category 1 wins mixed contests at the rate `γ_0 > 1/2`,
   and category 2's share of hires is below its population share.
2. **Sighted redistribution (Proposition 2).** A constant handicap `λ*(γ)` that
   grows as the target `γ` becomes more aggressive.
3. **Blind redistribution (Proposition 3).** The unique D1 equilibrium has the
   workers of cost above a cut-off `ĉ` pooled at zero effort. The cut-off
   solves eq. (10) and falls, so the pool grows, as the target becomes more
   aggressive. At parity everyone pools.
4. **Section 4.** The centralized problem is Myerson's optimal auction with a
   representation constraint.

## What could not be reconstructed

Nothing material. Two slips are recorded in `CLAIMS.md` (FT-D1, FT-D3), and the
first-order condition on p.6 is printed with the wrong function (FT-D2).
