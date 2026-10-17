import Mathlib
import Engagement

/-! Row M19. Budget compatibility, and the quadratic engagement utility
derived from it.

Class `c` affords the budget `b = β·c`. A pair that engages shares one
activity at one level `p`, which each pays from the own budget, leaving
private consumption `b − p`. The payoff from the level `p` is
`γ·p − (h/2)·p² + (b − p) − (a/2)·(b − p)²`, the activity's value less its
curvature, plus private consumption less its curvature. Budgets are
separate, never pooled. The pair sets the level that minimises the sum of
the two losses from the two ideal levels, and each loses `κ·(c_i − c_j)²`
with `κ = a²β²/(8(a + h))`. Pooled budgets, or a linear private utility,
give no such loss. -/

namespace Budget

/-- The payoff of a person with budget `b` from the shared activity at level
`p`, paid from the budget. -/
noncomputable def payoff (a h γ p b : ℝ) : ℝ :=
  γ * p - h / 2 * p ^ 2 + (b - p) - a / 2 * (b - p) ^ 2

/-- The ideal activity level of a person with budget `b`. -/
noncomputable def ideal (a h γ b : ℝ) : ℝ := (a * b + γ - 1) / (a + h)

/-- The loss from the level `p` against the ideal level, for budget `b`. -/
noncomputable def loss (a h γ p b : ℝ) : ℝ :=
  payoff a h γ (ideal a h γ b) b - payoff a h γ p b

/-- The compromise level, the midpoint of the two ideals. -/
noncomputable def mid (a h γ bi bj : ℝ) : ℝ := (ideal a h γ bi + ideal a h γ bj) / 2

/-- The sensitivity derived from the budget slope and the two curvatures. -/
noncomputable def sensitivity (a h β : ℝ) : ℝ := a ^ 2 * β ^ 2 / (8 * (a + h))

/-- The payoff is the ideal payoff less a quadratic loss in the distance from
the ideal level, with curvature `a + h`. -/
theorem payoff_eq (a h γ p b : ℝ) (hah : 0 < a + h) :
    payoff a h γ p b = payoff a h γ (ideal a h γ b) b - (a + h) / 2 * (p - ideal a h γ b) ^ 2 := by
  unfold payoff ideal
  field_simp
  ring

/-- The loss in closed form. -/
theorem loss_eq (a h γ p b : ℝ) (hah : 0 < a + h) :
    loss a h γ p b = (a + h) / 2 * (p - ideal a h γ b) ^ 2 := by
  unfold loss; rw [payoff_eq a h γ p b hah]; ring

/-- Row M19 (i). The ideal level maximises the payoff. -/
theorem ideal_max (a h γ p b : ℝ) (hah : 0 < a + h) :
    payoff a h γ p b ≤ payoff a h γ (ideal a h γ b) b := by
  rw [payoff_eq a h γ p b hah]
  have : 0 ≤ (a + h) / 2 * (p - ideal a h γ b) ^ 2 := by positivity
  linarith

/-- The ideal level is the unique maximiser. -/
theorem ideal_unique (a h γ p b : ℝ) (hah : 0 < a + h) (hp : p ≠ ideal a h γ b) :
    payoff a h γ p b < payoff a h γ (ideal a h γ b) b := by
  rw [payoff_eq a h γ p b hah]
  have h1 : 0 < (p - ideal a h γ b) ^ 2 := by
    have : p - ideal a h γ b ≠ 0 := sub_ne_zero.mpr hp
    positivity
  have : 0 < (a + h) / 2 * (p - ideal a h γ b) ^ 2 := by positivity
  linarith

/-- The ideal level is affine in the budget, with slope `a/(a + h)`. -/
theorem ideal_sub (a h γ b b' : ℝ) (hah : 0 < a + h) :
    ideal a h γ b' - ideal a h γ b = a * (b' - b) / (a + h) := by
  unfold ideal; field_simp; ring

/-- Row M19 (i). With a concave private utility, `a ≥ 0`, the ideal level
does not fall in the budget. The richer person wants the costlier activity. -/
theorem ideal_mono (a h γ b b' : ℝ) (hah : 0 < a + h) (ha : 0 ≤ a) (hb : b ≤ b') :
    ideal a h γ b ≤ ideal a h γ b' := by
  have := ideal_sub a h γ b b' hah
  have : 0 ≤ a * (b' - b) / (a + h) := by
    apply div_nonneg (mul_nonneg ha (by linarith)) hah.le
  linarith

/-- Monotone comparative statics in the abstract. If the payoff has
increasing differences in the level and the budget, a maximiser at the lower
budget is at most the unique maximiser at the higher budget. -/
theorem argmax_mono {V : ℝ → ℝ → ℝ}
    (hID : ∀ p p' b b', p ≤ p' → b ≤ b' → V p' b - V p b ≤ V p' b' - V p b')
    {p q b b' : ℝ} (hb : b ≤ b') (hp : ∀ r, V r b ≤ V p b)
    (hq : ∀ r, r ≠ q → V r b' < V q b') : p ≤ q := by
  by_contra hlt
  push_neg at hlt
  have h1 := hID q p b b' hlt.le hb
  have h2 := hp q
  have h3 := hq p (ne_of_gt hlt)
  linarith

/-- The quadratic payoff has increasing differences when `a ≥ 0`. -/
theorem payoff_increasing_differences (a h γ : ℝ) (ha : 0 ≤ a) :
    ∀ p p' b b', p ≤ p' → b ≤ b' →
      payoff a h γ p' b - payoff a h γ p b ≤ payoff a h γ p' b' - payoff a h γ p b' := by
  intro p p' b b' hp hb
  unfold payoff
  nlinarith [mul_nonneg ha (mul_nonneg (sub_nonneg.mpr hp) (sub_nonneg.mpr hb))]

/-- Four points of a concave function. If `y₁ + y₂ = x + z` with `y₁`
between `x` and `z`, the inner values sum to at least the outer values. -/
theorem concave_four_point {φ : ℝ → ℝ} (hφ : ConcaveOn ℝ Set.univ φ) {x y₁ y₂ z : ℝ}
    (hxy : x ≤ y₁) (hyz : y₁ ≤ z) (hsum : y₁ + y₂ = x + z) :
    φ x + φ z ≤ φ y₁ + φ y₂ := by
  rcases eq_or_lt_of_le (le_trans hxy hyz) with hxz | hxz
  · have e1 : y₁ = x := le_antisymm (hxz ▸ hyz) hxy
    have e2 : y₂ = x := by linarith
    rw [e1, e2, ← hxz]
  · have hd : 0 < z - x := sub_pos.mpr hxz
    set l := (z - y₁) / (z - x) with hl
    set m := (y₁ - x) / (z - x) with hm
    have hl0 : 0 ≤ l := div_nonneg (by linarith) hd.le
    have hm0 : 0 ≤ m := div_nonneg (by linarith) hd.le
    have hlm : l + m = 1 := by rw [hl, hm, ← add_div, div_eq_one_iff_eq hd.ne']; ring
    have c1 := hφ.2 (Set.mem_univ x) (Set.mem_univ z) hl0 hm0 hlm
    have c2 := hφ.2 (Set.mem_univ x) (Set.mem_univ z) hm0 hl0 (by linarith)
    simp only [smul_eq_mul] at c1 c2
    have e1 : l * x + m * z = y₁ := by
      rw [hl, hm]; field_simp; ring
    have e2 : m * x + l * z = y₂ := by
      rw [hl, hm]; field_simp; nlinarith
    rw [e1] at c1; rw [e2] at c2
    have key := add_le_add c1 c2
    have hsplit : l * φ x + m * φ z + (m * φ x + l * φ z) = φ x + φ z := by
      linear_combination (φ x + φ z) * hlm
    linarith

/-- Row M19 (i), the general case. With any concave private utility and any
activity utility, the payoff `φ(b − p) + ψ(p)` has increasing differences in
the level and the budget, so by `argmax_mono` the ideal level does not fall
in the budget. -/
theorem concave_increasing_differences {φ ψ : ℝ → ℝ} (hφ : ConcaveOn ℝ Set.univ φ)
    {p p' b b' : ℝ} (hp : p ≤ p') (hb : b ≤ b') :
    (φ (b - p') + ψ p') - (φ (b - p) + ψ p) ≤ (φ (b' - p') + ψ p') - (φ (b' - p) + ψ p) := by
  have := concave_four_point hφ (x := b - p') (y₁ := b - p) (y₂ := b' - p') (z := b' - p)
    (by linarith) (by linarith) (by ring)
  linarith

/-- Row M19 (ii). The compromise level minimises the sum of the two losses. -/
theorem mid_min (a h γ bi bj p : ℝ) (hah : 0 < a + h) :
    loss a h γ (mid a h γ bi bj) bi + loss a h γ (mid a h γ bi bj) bj ≤
      loss a h γ p bi + loss a h γ p bj := by
  rw [loss_eq a h γ _ bi hah, loss_eq a h γ _ bj hah, loss_eq a h γ p bi hah,
    loss_eq a h γ p bj hah]
  unfold mid
  nlinarith [sq_nonneg (p - (ideal a h γ bi + ideal a h γ bj) / 2), hah]

/-- Row M19 (ii). At the compromise level each person's loss is the
sensitivity times the squared budget gap over the budget slope squared, and
the two losses are equal. -/
theorem loss_mid (a h γ bi bj : ℝ) (hah : 0 < a + h) :
    loss a h γ (mid a h γ bi bj) bi = a ^ 2 / (8 * (a + h)) * (bi - bj) ^ 2 ∧
      loss a h γ (mid a h γ bi bj) bj = a ^ 2 / (8 * (a + h)) * (bi - bj) ^ 2 := by
  constructor
  · rw [loss_eq a h γ _ bi hah]; unfold mid ideal; field_simp; ring
  · rw [loss_eq a h γ _ bj hah]; unfold mid ideal; field_simp; ring

/-- Row M19 (iii). With budgets `β·c`, the engagement utility of either
person is the baseline value less the sensitivity times the squared class
gap. Both ways, the same loss. -/
theorem engagement_utility (a h γ β v ci cj : ℝ) (hah : 0 < a + h) :
    v + payoff a h γ (mid a h γ (β * ci) (β * cj)) (β * ci)
        - payoff a h γ (ideal a h γ (β * ci)) (β * ci) =
      v - sensitivity a h β * (ci - cj) ^ 2 ∧
    v + payoff a h γ (mid a h γ (β * ci) (β * cj)) (β * cj)
        - payoff a h γ (ideal a h γ (β * cj)) (β * cj) =
      v - sensitivity a h β * (ci - cj) ^ 2 := by
  have h1 := (loss_mid a h γ (β * ci) (β * cj) hah).1
  have h2 := (loss_mid a h γ (β * ci) (β * cj) hah).2
  unfold loss at h1 h2
  unfold sensitivity
  constructor
  · have : a ^ 2 / (8 * (a + h)) * (β * ci - β * cj) ^ 2 =
        a ^ 2 * β ^ 2 / (8 * (a + h)) * (ci - cj) ^ 2 := by field_simp
    linarith
  · have : a ^ 2 / (8 * (a + h)) * (β * ci - β * cj) ^ 2 =
        a ^ 2 * β ^ 2 / (8 * (a + h)) * (ci - cj) ^ 2 := by field_simp
    linarith

/-- The sensitivity is positive when the private curvature and the budget
slope are nonzero and the curvatures sum to a positive number. -/
theorem sensitivity_pos (a h β : ℝ) (ha : a ≠ 0) (hβ : β ≠ 0) (hah : 0 < a + h) :
    0 < sensitivity a h β := by
  unfold sensitivity; positivity

/-- Row M19 (iii). Against the outside option, the person engages exactly
when the squared class gap is at most the engagement threshold of A14. -/
theorem engage_iff (a h γ β v u ci cj : ℝ) (ha : a ≠ 0) (hβ : β ≠ 0) (hah : 0 < a + h) :
    u ≤ v + payoff a h γ (mid a h γ (β * ci) (β * cj)) (β * ci)
        - payoff a h γ (ideal a h γ (β * ci)) (β * ci) ↔
      Engagement.engages ((v - u) / sensitivity a h β) ((ci - cj) ^ 2) := by
  rw [(engagement_utility a h γ β v ci cj hah).1]
  exact Engagement.engages_iff v (sensitivity a h β) u ((ci - cj) ^ 2)
    (sensitivity_pos a h β ha hβ hah)

/-- Row M19 (ii), any fixed split. At the level `(1 − ω)·p_i + ω·p_j` the
losses are `(a + h)/2 · ω²` and `(a + h)/2 · (1 − ω)²` times the squared gap
in ideals, so the quadratic form survives any fixed split and the two losses
are equal exactly at the midpoint or at equal ideals. -/
theorem weighted_losses (a h γ bi bj ω : ℝ) (hah : 0 < a + h) :
    loss a h γ ((1 - ω) * ideal a h γ bi + ω * ideal a h γ bj) bi =
        (a + h) / 2 * ω ^ 2 * (ideal a h γ bi - ideal a h γ bj) ^ 2 ∧
      loss a h γ ((1 - ω) * ideal a h γ bi + ω * ideal a h γ bj) bj =
        (a + h) / 2 * (1 - ω) ^ 2 * (ideal a h γ bi - ideal a h γ bj) ^ 2 := by
  constructor
  · rw [loss_eq a h γ _ bi hah]; ring
  · rw [loss_eq a h γ _ bj hah]; ring

/-- The two losses under a fixed split are equal exactly at the midpoint, or
when the ideals coincide. -/
theorem weighted_equal_iff (a h γ bi bj ω : ℝ) (hah : 0 < a + h) :
    loss a h γ ((1 - ω) * ideal a h γ bi + ω * ideal a h γ bj) bi =
        loss a h γ ((1 - ω) * ideal a h γ bi + ω * ideal a h γ bj) bj ↔
      ω = 1 / 2 ∨ ideal a h γ bi = ideal a h γ bj := by
  rw [(weighted_losses a h γ bi bj ω hah).1, (weighted_losses a h γ bi bj ω hah).2]
  constructor
  · intro heq
    by_cases hid : ideal a h γ bi = ideal a h γ bj
    · exact Or.inr hid
    · left
      have hsq : 0 < (ideal a h γ bi - ideal a h γ bj) ^ 2 := by
        have : ideal a h γ bi - ideal a h γ bj ≠ 0 := sub_ne_zero.mpr hid
        positivity
      have : (a + h) / 2 * (ideal a h γ bi - ideal a h γ bj) ^ 2 * (ω ^ 2 - (1 - ω) ^ 2) = 0 := by
        linarith
      have hne : (a + h) / 2 * (ideal a h γ bi - ideal a h γ bj) ^ 2 ≠ 0 := by positivity
      have := (mul_eq_zero.mp this).resolve_left hne
      nlinarith
  · rintro (h | h)
    · rw [h]; ring
    · rw [h]; ring

/-- Row M19 (iv). The compromise level is within the poorer partner's budget
exactly when the budget gap is at most a bound that rises in the poorer
partner's budget. -/
theorem affordable_iff (a h γ bi bj : ℝ) (ha : 0 < a) (hah : 0 < a + h) :
    mid a h γ bi bj ≤ bj ↔ bi - bj ≤ 2 * (h * bj - (γ - 1)) / a := by
  unfold mid ideal
  rw [div_le_iff₀ (by norm_num : (0:ℝ) < 2), le_div_iff₀ ha]
  constructor
  · intro hle
    rw [← add_div, div_le_iff₀ hah] at hle
    nlinarith
  · intro hle
    rw [← add_div, div_le_iff₀ hah]
    nlinarith

/-- The affordability bound rises in the poorer partner's budget when the
activity curvature is positive. -/
theorem bound_mono (a h γ bj bj' : ℝ) (ha : 0 < a) (hh : 0 < h) (hb : bj ≤ bj') :
    2 * (h * bj - (γ - 1)) / a ≤ 2 * (h * bj' - (γ - 1)) / a := by
  apply div_le_div_of_nonneg_right _ ha.le
  nlinarith

/-- Control for (i) and (ii). With a linear private utility, `a = 0`, the
ideal level is the same for every budget and the compromise costs nothing.
The class gap then carries no cost at all. -/
theorem control_linear_private (h γ b bi bj : ℝ) (hh : 0 < h) :
    ideal 0 h γ b = (γ - 1) / h ∧ loss 0 h γ (mid 0 h γ bi bj) bi = 0 ∧
      ∀ β, sensitivity 0 h β = 0 := by
  refine ⟨?_, ?_, ?_⟩
  · unfold ideal; ring
  · rw [loss_eq 0 h γ _ bi (by linarith)]; unfold mid ideal; ring
  · intro β; unfold sensitivity; ring

/-- The pooled payoff. The two budgets are summed and the activity is set at
the ideal of the sum. -/
noncomputable def pooled (a h γ bi bj : ℝ) : ℝ :=
  payoff a h γ (ideal a h γ (bi + bj)) (bi + bj)

/-- Control for (iii), pooling. Two pairs with the same total budget have the
same pooled payoff whatever their gaps, so pooling removes the gap from the
payoff. -/
theorem control_pooled_gap_free (a h γ bi bj bi' bj' : ℝ) (hsum : bi + bj = bi' + bj') :
    pooled a h γ bi bj = pooled a h γ bi' bj' := by
  unfold pooled; rw [hsum]

/-- Control for (iii), pooling. Under pooling the activity level does not fall
in the partner's budget, so a richer partner is never avoided for the gap. -/
theorem control_pooled_level_mono (a h γ bi bj bj' : ℝ) (hah : 0 < a + h) (ha : 0 ≤ a)
    (hb : bj ≤ bj') :
    ideal a h γ (bi + bj) ≤ ideal a h γ (bi + bj') :=
  ideal_mono a h γ _ _ hah ha (by linarith)

/-- Control for (iii), separate budgets. Under separate budgets the engagement
utility falls strictly in the class gap when the sensitivity is positive. -/
theorem control_separate_gap_cost (a h β v ci cj cj' : ℝ) (ha : a ≠ 0) (hβ : β ≠ 0)
    (hah : 0 < a + h) (hgap : (ci - cj) ^ 2 < (ci - cj') ^ 2) :
    v - sensitivity a h β * (ci - cj') ^ 2 < v - sensitivity a h β * (ci - cj) ^ 2 := by
  have := sensitivity_pos a h β ha hβ hah
  nlinarith

end Budget
