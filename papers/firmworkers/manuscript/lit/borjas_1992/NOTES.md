# Notes — Borjas (1992), pass 1 (journal article, read in full)

Run `python3 lit/borjas_1992/verify_borjas_1992.py` from the bundle root
(`papers/firmworkers/manuscript`). It checks the pinned sha256 of the cached
PDF, the title page, each quotation verbatim on its own printed page, two
controls on the quotation check, and measures `lean/Lit/Borjas1992.lean`
(build root, build, no `sorry`, every cited name declared, every declared
theorem audited, no axiom outside `propext`, `Classical.choice`,
`Quot.sound`, controls present).

## What was verified

Our reading of Section II and of equations (14) and (18) is consistent with the
source. Each printed equation that we use either follows in Lean from the ones
before it, or is the unique solution of an equation stated in Lean:

- (5a), (5b) solve the differentiated first-order condition.
- (7a), (7b) follow from (5) and the log of (6).
- (8) is (7a) + (7b), and (9) follows from (8), given ρ < 1, 0 ≤ β₁ < 1 and
  0 ≤ s ≤ 1.
- (14) is an exact finite-population identity once the disturbance is
  orthogonal to parental skill.
- (18a), (18b) solve the normal equations under the covariances stated on
  p.141.
- The p.142 example gives exactly 12/41 and 22/205.

## What could not be verified

- The first-order condition and its differentiation are hypotheses of
  `foc_time_parent` and `foc_time_ethnic`, not theorems. An interior optimum is
  assumed.
- `ols_slope_omitting_ethnic_capital` states (14) for a finite population with
  the disturbance orthogonal to `x`. The paper states an expectation. The two
  agree when the orthogonality holds in expectation.
- Volume 107 and issue 1 are not printed on the copy; they come from the
  citation.
- No estimate in Tables I–VII is checked against data.

## Discrepancies

- BJ-D1 and BJ-D2: three sums printed in the text (0.63 on p.139; 0.50 and 0.53
  on p.140) differ from the table entries they summarise (0.6418, 0.4911,
  0.5352). `table3_gss_occupation_reading` and `table4_sums_against_text`
  record the gaps; `table3_sums_match_text` records the sums that agree.
- BJ-D3: the definition of π on p.131 divides a total by a per-observation
  variance. We read both on one scale.

## Findings from the formalisation

1. The criterion (9) needs β₁ < 1 and ρ < 1 for the denominator to be
   positive. Without β₁ < 1 (ρ = 1/2, β₁ = 4, s = 0), η < 1 although
   β₁ + β₂ > 1 (`control_eta_needs_beta1_lt_one`). Without ρ < 1 (ρ = 2,
   β₁ = 1/2, s = 2/5), η > 1 although β₁ + β₂ < 1
   (`control_eta_needs_rho_lt_one`).
2. The complementarity of p.127 needs β₂ > 0, which the paper uses but does not
   state. With β₂ = −1 it fails (`control_complementarity_needs_beta2_pos`).
3. The sum (18a) + (18b) = δ needs π < 1. At π = 1 the second normal equation
   is vacuous and the coefficients are not pinned down
   (`control_sum_needs_pi_lt_one`).

## Controls

| Check | Control |
|---|---|
| Quotations | BJ-C1, a reversed BJ-8, is absent; BJ-C2, BJ-1 looked up on the wrong page, is absent |
| Quotation suite | mutation-tested 10 Oct 2026; an altered word, a wrong page, an undeclared Lean name and a wrong sha256 pin each fail |
| Lean | six `control_*` theorems, one per cluster (complementarity, criterion (9) twice, persistence, (14), (18)) |

## Relevance to the firmworkers ledger

No model row rests on this paper yet. If a law of motion for human capital is
chosen with a group-average term, the convergence criterion (9) and the gap
dynamics are the results that would carry over: a group gap persists exactly
when the exponents on own and group capital sum to one, and a larger
group-capital exponent slows convergence below that. Whether categorical
friction enters this way is the author's decision (TODO.md).
