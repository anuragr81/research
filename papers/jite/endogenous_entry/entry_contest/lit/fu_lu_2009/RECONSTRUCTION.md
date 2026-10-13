# Reconstruction of Fu and Lu, "Contest with pre-contest investment"

**Source read.** Qiang Fu and Jingfeng Lu, manuscript of February 2009, 8
pages, read in full [F] and checked on page images [P]. The published version
is *Economics Letters* 103(3), 142-145 (2009), not checked. The text was
extracted with `pdftotext -layout` and cached at
`~/.cache/entry_contest/fu_lu_2009.txt`; page images are in
`~/.cache/entry_contest/fulu_pages/`.

**Why this paper.** The binary-investment search of 13 October 2026 found it as
the only candidate bearing on C7: investment before a contest whose response to
the number of contestants is not monotonic.

## Model (Sections 2-3)

`K ≥ 2` identical contestants compete for a prize of one in a Tullock contest.
Before the contest each invests `I_k ≥ 0`, publicly observed, which sets the
marginal cost of effort to `c(I_k) = λ/(λ + I_k)`. Investment costs `I_k`. The
paper studies symmetric pure-strategy subgame-perfect equilibria.

## Results as stated

1. **Proposition 1.**
   - (i) A symmetric equilibrium needs `λ ≥ 2(K-1)²/K³ - 1/K²`.
   - (ii) Zero investment is an equilibrium if and only if `λ ≥ 2(K-1)²/K³`.
   - (iii) For `K ≥ 4` there is no symmetric equilibrium with positive
     investment.
   - (iv) For `K = 2, 3` and `λ` in the interval below `2(K-1)²/K³`, the
     candidate `I*(K) = 2(K-1)²/K³ - λ` is an equilibrium iff an effort
     comparison holds. Effort is then `2(K-1)³/(λK⁵)`.
2. **Corollaries 1-2.** For `λ` near 0.23, investment and effort rise from
   `K = 2` to `K = 3` and investment is zero for `K ≥ 7`, so neither is
   monotonic in `K`.

## What could not be reconstructed

Proposition 1(ii) for `K ≥ 4`, the condition in Proposition 1(iv), and the
corollaries (see `CLAIMS.md`, Reading recorded).
