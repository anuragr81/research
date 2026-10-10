# Reconstruction of Drugov and Ryvkin, "How noise affects effort in tournaments"

**Source read.** Mikhail Drugov and Dmitry Ryvkin, "How noise affects effort
in tournaments", NES Working Paper 256, version of 17 February 2020, 32 pages,
uploaded by the author on 10 Oct 2026 as `WP256.pdf` on Drive. Read in full
[F]. The published version is *Journal of Economic Theory* 188 (2020),
105065, not checked. The text was extracted with `pdftotext` and cached at
`~/.cache/entry_contest/drugov_ryvkin_2020_wp.txt`; pages are the working
paper's printed page numbers.

**Why this paper.** C12 claimed the sign of M28 on the entry margin and
waited on this paper to confirm it has no entry margin. It has one
(Section 5.3), with the opposite sign.

## Model (Section 2)

`n` identical players choose effort `e` at cost `c(e)` (strictly increasing,
strictly convex); output `e + X_i` with i.i.d. noise; prizes
`V_1 ≥ … ≥ V_n`, summing to 1. Symmetric FOC (4): `c'(e*) = Σ B_{r,n} D_r`
with `D_r = V_r − V_{r+1} ≥ 0`. Representation (6): `B_{r,n}` is a
non-negative weighted integral of the inverse quantile density
`m(z) = f(F⁻¹(z))`.

## Results

1. **Proposition 1 (p.10).** Effort is lower under noise `X` than under `Y`
   for all admissible prize schedules and sizes iff `X` is more dispersed
   than `Y`. Sufficiency: more dispersed means `m_X ≤ m_Y`, so every
   `B_{r,n}` is lower.
2. **Scaling (p.11).** `X = σY`, `σ > 1`, is a stretching, `m_X = m_Y/σ`,
   and effort falls in `σ`.
3. **Winner-take-all (Section 4).** Weaker orders (quantile stochastic
   dominance) and Rényi entropy of order statistics; not used here.
4. **Section 5.3, endogenous number of players (pp.20-22).** `N` potential
   players first choose entry or an outside option `ω > 0`; entrants then
   choose effort. Entrants' payoff `1/n − c(e*_{X,n})`, assumed decreasing in
   `n`. If `e*_{X,n} ≥ e*_{Y,n}` for all `n` (`X` the less dispersed noise),
   then `n_X ≤ n_Y`: the less noisy tournament has weakly fewer entrants.
   Effort comparisons then carry a correction for the discreteness of `n`.
5. **Footnote 8 (p.6).** With too little noise the pure-strategy
   equilibrium fails and effort falls through dropping out (they cite Morgan,
   Tumlinson and Vardy); the paper studies adding noise under full
   participation.
6. **Literature (p.5).** Fu, Jiao and Lu (2015): with endogenous entry, a
   higher discriminatory power may reduce the number of entrants.

## What could not be reconstructed

Nothing material for our use. The existence conditions are referred to
Drugov and Ryvkin (2018).
