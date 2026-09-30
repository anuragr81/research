/-
# Garber (1980), "Field and Jeffrey Conditionalization" (Discussion)

*Philosophy of Science* 47(1), 142-145.

Garber's opening thesis (p. 142): "Field's proposed revision of Jeffrey's
formula is neither correct nor necessary." His counterexample (pp. 143-144)
repeats one and the same weak experience, with the same Field input `α`
each time. Field's relation, Garber's eq. (3) (Field's (5)):
`q = p e^α / (p e^α + (1-p) e^{-α})`; Garber's eq. (4) (Field's definition (4)):
`α = (1/2) log ((q/p)/((1-q)/(1-p)))`.

* A glance in dim light moves `P(E)` from `.3` to `.4`; "an α value of .2209
  (to four places)" (p. 143).
* Repeating it gives the table on p. 144:
  `P₀ … P₉ = .3, .4, .5091, .6173, .7150, .7961, .8586, .9043, .9363, .9581`
  ("after nine repetitions … virtually certain").
* A "slightly richer" experience, `.3 → .5`, takes "only five repetitions …
  to raise S's degree of belief in E above .95" (p. 144).

Garber's prose on p. 143 says the second value "will be .5019 (to four
places)"; the table and the computation give `.5091`, so `.5019` is a typo in
the paper (`garber_prose_typo`).

## What is proved

* `odds_fieldStep` — one Field step with `α` multiplies the odds by `e^{2α}`.
  `odds_iterate`, `fieldStep_iterate` — after `n` steps with a fixed `α` the
  odds are multiplied by `e^{2nα}`, i.e. the result is one Field step with
  `nα`.
* `garber_lr` — Garber's `α` for `.3 → .4` has `e^{2α} = 14/9` exactly (the
  likelihood ratio, see `Literature.Field.exp_two_alphaOf_bayes`), and
  `garber_alpha_4dp` — `.22085 < α < .22095`, so `α = .2209` to four places.
* `garberSeq_closed` — `Pₙ(E) = 3·14ⁿ / (3·14ⁿ + 7·9ⁿ)` exactly.
  `garber_table` — every entry of the p. 144 table is `Pₙ(E)` to four places.
  `garber_nine` — `P₉(E) > .95`, and `P₈(E) < .95`.
* `richer_lr`, `richerSeq_closed`, `richer_five` — the `.3 → .5` experience has
  `e^{2α} = 7/3`, `Pₙ(E) = 3·7ⁿ/(3·7ⁿ + 7·3ⁿ)`, and five repetitions is the
  first to exceed `.95` (`P₅ ≈ .9674 > .95 > .9270 ≈ P₄`).
* `fieldStep_iterate_tendsto_one` — for any `α > 0` and `0 < p < 1`, the
  repeated update tends to certainty.

## Not formalized

Garber's dilemma (p. 145: if `P₁(E)` is independent of `P₀(E)` no
reparametrization is needed; if not, conditionalization of any sort may be
inappropriate) is philosophical.
-/
import Mathlib

namespace Literature.Garber

open Real Filter Topology

/-- Field's eq. (5) = Garber's eq. (3): the new `P(E)` from the old `p` and the input `α`. -/
noncomputable def fieldStep (α p : ℝ) : ℝ := p * exp α / (p * exp α + (1 - p) * exp (-α))

/-- Field's eq. (4) = Garber's eq. (4). -/
noncomputable def alphaOf (p q : ℝ) : ℝ := (1 / 2) * Real.log ((q / p) / ((1 - q) / (1 - p)))

/-- Odds `p/(1-p)`. -/
noncomputable def odds (p : ℝ) : ℝ := p / (1 - p)

theorem fieldStep_pos {α p : ℝ} (hp0 : 0 < p) (hp1 : p < 1) : 0 < fieldStep α p := by
  have : 0 < 1 - p := by linarith
  unfold fieldStep; positivity

theorem fieldStep_lt_one {α p : ℝ} (hp0 : 0 < p) (hp1 : p < 1) : fieldStep α p < 1 := by
  have : 0 < 1 - p := by linarith
  unfold fieldStep
  rw [div_lt_one (by positivity)]
  have : 0 < (1 - p) * exp (-α) := by positivity
  linarith

/-- **One Field step multiplies the odds by `e^{2α}`.** -/
theorem odds_fieldStep (α : ℝ) {p : ℝ} (hp0 : 0 < p) (hp1 : p < 1) :
    odds (fieldStep α p) = exp (2 * α) * odds p := by
  have h1p : 0 < 1 - p := by linarith
  have hD : 0 < p * exp α + (1 - p) * exp (-α) := by positivity
  have h1 : 1 - fieldStep α p = (1 - p) * exp (-α) / (p * exp α + (1 - p) * exp (-α)) := by
    unfold fieldStep; field_simp; ring
  unfold odds
  rw [h1]
  unfold fieldStep
  rw [show 2 * α = α - (-α) by ring, exp_sub]
  field_simp

theorem iterate_mem (α : ℝ) {p : ℝ} (hp0 : 0 < p) (hp1 : p < 1) (n : ℕ) :
    0 < (fieldStep α)^[n] p ∧ (fieldStep α)^[n] p < 1 := by
  induction n with
  | zero => exact ⟨hp0, hp1⟩
  | succ n ih =>
    rw [Function.iterate_succ_apply']
    exact ⟨fieldStep_pos ih.1 ih.2, fieldStep_lt_one ih.1 ih.2⟩

/-- **After `n` repetitions with the same `α` the odds are multiplied by `e^{2nα}`.** -/
theorem odds_iterate (α : ℝ) {p : ℝ} (hp0 : 0 < p) (hp1 : p < 1) (n : ℕ) :
    odds ((fieldStep α)^[n] p) = exp (2 * α) ^ n * odds p := by
  induction n with
  | zero => simp
  | succ n ih =>
    rw [Function.iterate_succ_apply', odds_fieldStep α (iterate_mem α hp0 hp1 n).1
      (iterate_mem α hp0 hp1 n).2, ih, pow_succ]
    ring

/-- A probability in `(0,1)` is recovered from its odds. -/
theorem eq_of_odds {x : ℝ} (hx1 : x < 1) : x = odds x / (1 + odds x) := by
  have : 0 < 1 - x := by linarith
  unfold odds; field_simp; ring

theorem odds_injOn {x y : ℝ} (hx1 : x < 1) (hy1 : y < 1) (h : odds x = odds y) : x = y := by
  rw [eq_of_odds hx1, eq_of_odds hy1, h]

/-- **Closed form:** `n` Field steps with `α` are one Field step with `nα`. -/
theorem fieldStep_iterate (α : ℝ) {p : ℝ} (hp0 : 0 < p) (hp1 : p < 1) (n : ℕ) :
    (fieldStep α)^[n] p = fieldStep (n * α) p := by
  apply odds_injOn (iterate_mem α hp0 hp1 n).2 (fieldStep_lt_one hp0 hp1)
  rw [odds_iterate α hp0 hp1, odds_fieldStep _ hp0 hp1, ← exp_nat_mul]
  ring_nf

/-- The probability after `n` steps, from the prior odds `o` and the likelihood ratio `r`. -/
theorem iterate_eq_of_lr (α : ℝ) {p : ℝ} (hp0 : 0 < p) (hp1 : p < 1) {r : ℝ}
    (hr : exp (2 * α) = r) (n : ℕ) :
    (fieldStep α)^[n] p = r ^ n * odds p / (1 + r ^ n * odds p) := by
  rw [eq_of_odds (iterate_mem α hp0 hp1 n).2, odds_iterate α hp0 hp1, hr]

/-! ## Garber's first example: `.3 → .4`, nine repetitions -/

/-- Garber's `α` for a glance that moves `P(E)` from `.3` to `.4`. -/
noncomputable def α₀ : ℝ := alphaOf (3 / 10) (4 / 10)

/-- **`e^{2α} = 14/9` exactly** for Garber's `α`: the likelihood ratio of one glance. -/
theorem garber_lr : exp (2 * α₀) = 14 / 9 := by
  unfold α₀ alphaOf
  rw [show 2 * (1 / 2 * Real.log ((4 / 10 / (3 / 10)) / ((1 - 4 / 10) / (1 - 3 / 10)))) =
      Real.log (14 / 9) by norm_num; ring]
  exact exp_log (by norm_num)

/-- `exp x` for `x ∈ [0,1]` to within `10⁻⁶`, from `Real.exp_bound` with 8 terms. -/
theorem exp_near {x : ℝ} (hx0 : 0 ≤ x) (hx1 : x ≤ 1 / 2) :
    |exp x - ∑ m ∈ Finset.range 8, x ^ m / m.factorial| ≤ (1 / 2) ^ 8 * (9 / (40320 * 8)) := by
  have h := Real.exp_bound (x := x) (by rw [abs_of_nonneg hx0]; linarith) (n := 8) (by norm_num)
  refine h.trans ?_
  rw [abs_of_nonneg hx0]
  have h8 : x ^ 8 ≤ (1 / 2) ^ 8 := pow_le_pow_left₀ hx0 hx1 8
  norm_num [Nat.factorial] at h8 ⊢
  nlinarith

/-- **Garber's `α = .2209` to four places:** `.22085 < α < .22095`. -/
theorem garber_alpha_4dp : 22085 / 100000 < α₀ ∧ α₀ < 22095 / 100000 := by
  have hlo := exp_near (x := 2 * (22085 / 100000)) (by norm_num) (by norm_num)
  have hhi := exp_near (x := 2 * (22095 / 100000)) (by norm_num) (by norm_num)
  simp only [Finset.sum_range_succ, Finset.sum_range_zero, Nat.factorial] at hlo hhi
  rw [abs_le] at hlo hhi
  norm_num at hlo hhi
  constructor
  · by_contra! h
    have : exp (2 * α₀) ≤ exp (2 * (22085 / 100000)) := exp_le_exp.2 (by linarith)
    rw [garber_lr] at this
    norm_num at this
    linarith [hlo.2]
  · by_contra! h
    have : exp (2 * (22095 / 100000)) ≤ exp (2 * α₀) := exp_le_exp.2 (by linarith)
    rw [garber_lr] at this
    norm_num at this
    linarith [hhi.1]

/-- Garber's sequence `Pₙ(E)`: `n` looks, each with the same `α`, from `P₀(E) = .3`. -/
noncomputable def garberSeq (n : ℕ) : ℝ := (fieldStep α₀)^[n] (3 / 10)

/-- **Closed form of Garber's sequence:** `Pₙ(E) = 3·14ⁿ / (3·14ⁿ + 7·9ⁿ)`. -/
theorem garberSeq_closed (n : ℕ) :
    garberSeq n = 3 * 14 ^ n / (3 * 14 ^ n + 7 * 9 ^ n) := by
  unfold garberSeq
  rw [iterate_eq_of_lr α₀ (by norm_num) (by norm_num) garber_lr]
  unfold odds
  have h9 : (0 : ℝ) < 9 ^ n := by positivity
  rw [div_pow]
  field_simp
  ring

/-- Garber's table, p. 144 (four decimal places). -/
noncomputable def garberTable : Fin 10 → ℝ :=
  ![3 / 10, 4 / 10, 5091 / 10000, 6173 / 10000, 7150 / 10000, 7961 / 10000, 8586 / 10000,
    9043 / 10000, 9363 / 10000, 9581 / 10000]

/-- **Every entry of Garber's table is correct to four places:** `|Pₙ(E) - tableₙ| < 1/20000`. -/
theorem garber_table : ∀ n : Fin 10, |garberSeq n - garberTable n| < 1 / 20000 := by
  intro n
  rw [garberSeq_closed]
  fin_cases n <;> simp [garberTable] <;> norm_num [abs_lt]

/-- **The `.5019` of Garber's prose (p. 143) is a typo:** `P₂(E) = 588/1155 ≈ .50909`,
which is `.5091`, not `.5019`, to four places. -/
theorem garber_prose_typo :
    garberSeq 2 = 588 / 1155 ∧ 1 / 20000 < |garberSeq 2 - 5019 / 10000| := by
  rw [garberSeq_closed]; norm_num [abs_lt, lt_abs]

/-- **After nine repetitions `P(E) > .95`** (and after eight it is still below). -/
theorem garber_nine : 95 / 100 < garberSeq 9 ∧ garberSeq 8 < 95 / 100 := by
  rw [garberSeq_closed, garberSeq_closed]; norm_num

/-! ## Garber's second example: `.3 → .5`, five repetitions -/

/-- `α` for the "slightly richer" experience, `.3 → .5`. -/
noncomputable def α₁ : ℝ := alphaOf (3 / 10) (5 / 10)

/-- `e^{2α} = 7/3` exactly for the richer experience. -/
theorem richer_lr : exp (2 * α₁) = 7 / 3 := by
  unfold α₁ alphaOf
  rw [show 2 * (1 / 2 * Real.log ((5 / 10 / (3 / 10)) / ((1 - 5 / 10) / (1 - 3 / 10)))) =
      Real.log (7 / 3) by norm_num; ring]
  exact exp_log (by norm_num)

noncomputable def richerSeq (n : ℕ) : ℝ := (fieldStep α₁)^[n] (3 / 10)

theorem richerSeq_closed (n : ℕ) :
    richerSeq n = 3 * 7 ^ n / (3 * 7 ^ n + 7 * 3 ^ n) := by
  unfold richerSeq
  rw [iterate_eq_of_lr α₁ (by norm_num) (by norm_num) richer_lr]
  unfold odds
  have h3 : (0 : ℝ) < 3 ^ n := by positivity
  rw [div_pow]
  field_simp
  ring

/-- **Five repetitions of the richer experience exceed `.95`**; four do not. -/
theorem richer_five : 95 / 100 < richerSeq 5 ∧ richerSeq 4 < 95 / 100 := by
  rw [richerSeq_closed, richerSeq_closed]; norm_num

/-! ## Repetition drives any positive `α` to certainty -/

/-- **For every `α > 0` the repeated Field update tends to `P(E) = 1`.** -/
theorem fieldStep_iterate_tendsto_one {α : ℝ} (hα : 0 < α) {p : ℝ} (hp0 : 0 < p) (hp1 : p < 1) :
    Tendsto (fun n : ℕ => (fieldStep α)^[n] p) atTop (𝓝 1) := by
  have hr : 1 < exp (2 * α) := one_lt_exp_iff.2 (by linarith)
  have h1p : 0 < 1 - p := by linarith
  have ho : 0 < odds p := by unfold odds; positivity
  have hform : ∀ n : ℕ, (fieldStep α)^[n] p = 1 - (1 + exp (2 * α) ^ n * odds p)⁻¹ := by
    intro n
    rw [iterate_eq_of_lr α hp0 hp1 rfl]
    have : 0 < 1 + exp (2 * α) ^ n * odds p := by positivity
    field_simp; ring
  simp_rw [hform]
  have h1 : Tendsto (fun n : ℕ => 1 + exp (2 * α) ^ n * odds p) atTop atTop :=
    tendsto_atTop_add_const_left _ _
      ((tendsto_pow_atTop_atTop_of_one_lt hr).atTop_mul_const ho)
  have := h1.inv_tendsto_atTop
  simpa using (tendsto_const_nhds (x := (1 : ℝ))).sub this

end Literature.Garber
