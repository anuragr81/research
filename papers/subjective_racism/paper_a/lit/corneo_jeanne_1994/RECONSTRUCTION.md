# Corneo and Jeanne (1994), reconstruction

Giacomo Corneo and Olivier Jeanne, "Conspicuous Consumption and the Existence
of Upward Sloping Demand Curves", Discussion Paper A-461, University of Bonn,
December 1994. A version was published as "Conspicuous consumption,
snobbism and conformism", *Journal of Public Economics* 66 (1997), pp. 55–71,
which is not read (CJ-D1).

## Source read

- The discussion paper, 24 PDF pages, with the title page, abstract, text pp. 3–18,
  Appendix pp. 19–20, Figures 1 and 2 (p. 21, blank in this copy, CJ-D6),
  references pp. 22–24. Cached at
  `~/.cache/paper_a/sources/corneo_jeanne_1997_dp.pdf`, sha256
  `2f315ec019f7f38f17f3a545f6a94892a25509edd3c31da41af26b2145680c57`, and its
  text layer at `~/.cache/paper_a/sources/corneo_jeanne_1997_dp.txt`, sha256
  `fe244470b27547c19369e717c11e49a0d50bfd294437bf82c8c0d35c96d38c8d`. The
  file name says 1997, but the title page is the December 1994 discussion
  paper.
- Every page was read from the text layer on 16 October 2026 [F]. PDF pages
  1, 8 to 14 and 19 to 21 were also rendered at 110 dpi and read as images,
  because the text layer drops the Greek symbols, the ligatures fi, ff and fl
  and the accent of "numéraire" (CJ-D3).

## The model, from its own primitives

1. **Population** (p. 5). A continuum of consumers `r ∈ [0, 1]`, ranked so
   that `r < r'` exactly when `y_r > y_{r'}`. Rank 0 is the richest.
2. **Goods and observation** (p. 6). An indivisible conspicuous good, bought
   or not (`δ_r ∈ {0, 1}`), and a numéraire. Income and numéraire consumption
   are hidden (CJ-Q1). Utility `U_r = u(c_r) + v(δ_r)` with `u` increasing and
   strictly concave (eq. 1).
3. **Status** (p. 6). `v(δ_r) = E(a(r) | δ_r)`, the public's expectation of
   the rank utility `a` given the purchase (eq. 2). The good's only purpose
   is the message (CJ-Q2), and every observer reads it the same way (CJ-Q3).
4. **Budget** (p. 7). `c_r + p δ_r ≤ y_r` (eq. 3).
5. **Purchase rule** (p. 8). Buy when `σ = v(1) − v(0) > u(y_r) − u(y_r − p)`
   (eq. 4), so buyers are those above a threshold income `ȳ(σ, p)`, the
   indifferent income (eq. 5) or the price itself when the budget binds
   (eq. 6). The number of buyers is `n = N(ȳ(σ, p))` (eq. 7).
6. **Beliefs** (pp. 8–9). In a semi-separating equilibrium, buying says the
   buyer is in the richest `n` (CJ-Q5), and by Bayes' rule
   `σ = (∫_0^n a)/n − (∫_n^1 a)/(1 − n) = (1/(n(1 − n))) ∫_0^n (a − ā)` (eq. 8).
   Each group, buyers and non-buyers, is valued at its own mean rank utility,
   given the cutoff that the equilibrium produces.
7. **Conformism and snobbism** (pp. 9–10). Behaviour is conformist when
   `σ(n)` rises and snobbish when it falls. Any `σ` comes from the rank
   utility of eq. (9). The family `σ = α + β n^γ` (eq. 10) comes from eq. (11).
   With `γ = 1`, conformism is concavity of `a` in rank, the fear of being
   pooled with the poor (CJ-Q8).
8. **Proposition 1** (p. 11). The price function `p(n)`, from eq. (12)
   `σ(n) = u(y_n) − u(y_n − p)`, is unique and continuous, and decreasing under
   snobbism (CJ-Q9, CJ-D2).
9. **Example** (pp. 12–13). `σ = n^γ`, `u(c) = ū − e^{−μc}`,
   `N(y) = e^{−λy}`, giving `p = (1/μ) log(1 + n^{γ − μ/λ})` while the buyer
   can afford the good (eq. 16) and `p = −(1/λ) log n` when the budget binds
   (eq. 17). For `γ > μ/λ` and small `n` the price rises in `n`.
10. **Policy** (pp. 13–17). The planner wants no conspicuous good (eq. 18); a
    tax may raise consumption under conformism; under snobbism and passive
    conjectures a cheap good is not bought (Proposition 2); voluntary
    contributions to a public good priced above market signal income, as at
    Rotary and Lions dinners (CJ-Q10, CJ-Q11).
11. **Pooling** (pp. 19–20). Propositions 3 and 4 give the pooling equilibria
    at `n = 0` and `n = 1` and the beliefs that support them.

## Derivation chain of what Paper A uses

- Eq. (8) is the identity of the difference of group means with the integral
  form (`eq8`). Differentiating it gives eq. (9) (`eq9`), and eq. (10) in eq.
  (9) gives eq. (11) (`eq11`).
- Concavity of `u` makes the utility cost of the price fall in income
  (`concave_increment`), which with snobbism makes the price function fall
  (`prop1_snob`). Without snobbism the price can rise, as in the Section 3
  example (`example_eq16`, `control_conformist_price_rises`).
- A rank utility that rises in income gives a negative signaling value
  (`control_rising_rank_utility`).

## Second pass, 16 October 2026

Every numbered equation and proposition now has a row in the coverage table
of `CLAIMS.md`. Added in this pass are the purchase rule and the upper set of
buyers, the limits of eq. (8), the case `γ = 1` with its concavity and
footnote 8's counterexample, existence and uniqueness of the price in
Proposition 1, eqs. (13) to (17) in general, eq. (18), and Propositions 2 to 4
with eq. (24).

## What could not be reconstructed

- The continuity of `p(n)` in Proposition 1 (CJ-D5).
- That the stationary rank `γ/(2 + γ)` is the maximum of the rank utility.
- The text's necessity reading of the upward slope ("only if `γ` and `λ` are
  sufficiently large and `μ` is sufficiently small"). Sufficiency is formalised.
- The monopoly discussion of Section 4.2 and eq. (19), which the paper argues
  in words or reduces to Proposition 1.
- Figures 1 and 2 are blank in the copy read (CJ-D6).
