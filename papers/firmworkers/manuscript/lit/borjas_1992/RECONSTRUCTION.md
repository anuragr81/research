# Reconstruction of Borjas, "Ethnic Capital and Intergenerational Mobility"

**Source read.** George J. Borjas, "Ethnic Capital and Intergenerational
Mobility", *The Quarterly Journal of Economics*, February 1992, printed pages
123–150 (28 pages, the journal's typeset article). Uploaded by the author on
10 Oct 2026 as `ethnic_capital_1992.pdf` on Drive (two identical copies, Drive
ids `19frG7EpOVv1j-hkYWm8eQY-KTcrVJxF1` and `1PlPcTKa3Nk5_9p7nJHIfgbDptXG5omZe`).
Read in full [F]. Cached at `~/.cache/firmworkers/borjas_1992.pdf`, sha256
`5f90cc1cbed44a26cb646e7d83ad5e0207c076c10087fba01e253537be1a1433`; text
extracted with `pdftotext -layout` to `~/.cache/firmworkers/borjas_1992.txt`,
one form feed per page, so page `p` of the journal is block `p − 123`. The
text layer garbles every displayed equation; equations (1)–(18) were read from
page images rendered at 110 dpi. The copy does not print the volume and issue;
`refs.bib` takes them from the citation (107(1)).

**Why this paper.** Ledger row L4. The firmworkers micro model has no law of
motion for human capital yet (TODO.md, pending decisions). Borjas is the
candidate for a group-average term in that law, the route by which
categorical friction could enter accumulation rather than only the rank kernel.

## Primitives (Section II, pp.125–126)

- One-person household in generation `t`, one child, human capital `k_t`,
  sold at price `R` or used to produce the child's human capital. Workers do
  not invest in themselves, so `k_{t+1}` is fixed by generation `t`.
- Utility (1): `U = [δ₁ k_{t+1}^ρ + δ₂ C_t^ρ]^{1/ρ}`, `ρ < 1`, elasticity of
  substitution `σ = 1/(1 − ρ)`.
- Budget (2): `R(1 − s_t)k_t = C_t`, with `s_t` the fraction of time spent on
  the child.
- Technology (3): `k_{t+1} = β₀ (s_t k_t)^{β₁} k̄_t^{β₂}`, with `k̄_t` the
  ethnic group's average human capital ("ethnic capital"), taken as
  exogenous by the household. `β₁, β₂ < 1` (p.126); positivity of `β₁, β₂` is
  used throughout but not stated.

## Derivation chain

1. **Complementarity (p.127).** From (3), the gain from more parental time
   rises with `k̄`. Lean: `childCapital_complementarity`, increasing
   differences in `(s, k̄)` for `β₀, β₁, β₂ > 0`.
2. **Time supply (4) and its elasticities (5a), (5b) (p.127).** Maximising (1)
   subject to (2), (3) gives the interior first-order condition, in logs,
   `log δ₁ + log β₁ + ρ log k_{t+1} + log(1 − s) = log δ₂ + ρ log C + log s`.
   Differentiating with respect to `log k` (or `log k̄`), with
   `d log(1 − s) = −(s/(1 − s)) d log s`, gives a linear equation in
   `ŝ = d log s`. Its unique solution is (5a) `ρ(β₁ − 1)(1 − s)/D` and (5b)
   `ρβ₂(1 − s)/D`, `D = (1 − s)(1 − ρβ₁) + s(1 − ρ)`. Lean: `foc_time_parent`,
   `foc_time_ethnic`, with the differentiated condition as a hypothesis.
3. **Signs (pp.127–128).** With `D > 0`: (5a) is negative iff `ρ > 0`,
   positive iff `ρ < 0`, zero at Cobb–Douglas `ρ = 0`; (5b) is positive when
   `ρ > 0`. Lean: `denom_pos`, `elasTimeParent_neg_of_rho_pos`,
   `elasTimeParent_pos_of_rho_neg`, `elasTimeParent_zero_of_cobbDouglas`,
   `elasTimeEthnic_pos_of_rho_pos`.
4. **Reduced form (6) and (7a), (7b) (p.128).** Substituting (4) into (3),
   `log k_{t+1} = log β₀ + β₁ log s + β₁ log k + β₂ log k̄`, so
   (7a) `= β₁(1 + (5a))` and (7b) `= β₁·(5b) + β₂`. Both simplify to the printed
   forms `β₁(1 − ρ)/D` and `β₂(1 − ρs)/D`, positive for every `ρ < 1`. Lean:
   `eq7a_from_eq5a`, `eq7b_from_eq5b`, `elasChild_pos`.
5. **Group elasticity (8) and the convergence criterion (9) (pp.128–129).**
   With all parents in a group equal, `k = k̄`, `η = (7a) + (7b)`. Then
   `η − 1 = (β₁ + β₂ − 1)(1 − ρs)/D`, and `1 − ρs > 0`, `D > 0`, so the sign of
   `η − 1` is the sign of `β₁ + β₂ − 1`. Lean: `eq8_eq_sum`, `eta_sub_one`,
   `eta_lt_one_iff`, `eta_eq_one_iff`, `one_lt_eta_iff`.
6. **Persistence (p.129).** Under Cobb–Douglas utility `s` is constant, and for
   two groups with common `s` the log gap obeys
   `gap_{t+1} = (β₁ + β₂)·gap_t`. Constant returns keep the gap forever;
   `β₁ + β₂ < 1` sends it to zero; a larger `β₂` keeps it larger at every
   horizon. Lean: `log_childCapital_common`, `group_gap_step`,
   `gap_closed_form`, `gap_persists`, `gap_tendsto_zero`,
   `gap_larger_with_externality`.
7. **Omitted ethnic capital (14) (p.131).** If `y = γ₀ + γ₁x + γ₂x̄_j + ζ`, the
   least-squares slope of `y` on `x` alone is `γ₁ + (1 − π)γ₂`, `π` the within
   share of the variance of `x`, because the covariance of `x̄_j` with `x` is the
   between-group variance. Lean: `sum_within_deviation_zero`,
   `cov_groupMean_eq_between`, `total_eq_within_add_between`,
   `ols_slope_omitting_ethnic_capital` (finite population, `ζ` orthogonal to
   `x`), `ols_understates_transmission`.
8. **Measurement error (15)–(18) (pp.141–142).** With `x₁ = x + v₁`,
   `x₂ = x + v₂`, `cov(x, v₂) = −σ₂²`, the normal equations of `y` on
   `(x₁, x₂)` have the unique solution (18a), (18b), and the two limits sum to
   `δ`. The ethnic-capital limit is zero with exact measurement and falls in the
   reliability `h`. At `δ = 0.4`, `π = 0.9`, noise ratio `0.33` the limits are
   exactly `12/41` and `22/205`, the printed `0.29` and `0.11`. Lean:
   `normal_equations_solution`, `plim_sum_eq_delta`, `plimEthnic_zero_of_exact`,
   `plimEthnic_strictAnti_reliability`, `reliability_of_noise_ratio`,
   `example_p142`, `example_p142_rounds`.

## Empirics (Sections III–VI)

Equation (13), `y_ij(t) = γ₀ + γ₁ y_ij(t − 1) + γ₂ ȳ_j(t − 1) + ζ_ij(t)`,
estimated by random-effects GLS on the GSS (1977–1989, ages 18–64, U.S.-born,
blacks and American Indians excluded) and the NLSY (1987 wave). Tables III–VI
report `γ₁`, `γ₂`; the text reports their sums as the transmission parameter
of a group's mean. Section VI predicts black intergenerational mobility from
third-generation white coefficients (Table VII). The paper says the ethnic
capital reading is not the only one consistent with the data (p.145).

## What could not be reconstructed

- The first-order condition itself and its differentiation are calculus steps
  done by hand here; Lean takes the differentiated condition as a hypothesis.
  An interior optimum is assumed, not shown.
- The estimates in Tables I–VII, the random-effects estimator and the
  sampling rules are data and procedure, not checked.
- Three sums printed in the text differ from the table entries they summarise
  (CLAIMS.md, BJ-D1, BJ-D2).
