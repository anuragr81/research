import Bifurcation

namespace Slowing

open ClosureMap Bifurcation

/-! ### Row M16. Critical slowing above the threshold -/

/-- Just above the threshold, with excess mobility `ε`, the fixed-point
polynomial is a square plus `ε c`. -/
theorem Q_threshold_plus {μ b : ℝ} (hμ : 0 ≤ μ) (hb : 0 ≤ b) (ε c : ℝ) :
    Q μ (gammaStar μ b + ε) b c = (Real.sqrt b * c - Real.sqrt μ) ^ 2 + ε * c := by
  rw [gammaStar_eq hμ hb]
  have hu := Real.sq_sqrt hb
  have hv := Real.sq_sqrt hμ
  unfold Q
  linear_combination (c - c ^ 2) * hu + (c - 1) * hv

/-- Inside the window of half-width `√ε / √b` about the merged root, one
generation moves the standing up by at least 0 and at most `2 ε`. -/
theorem step_bounds {μ b ε c : ℝ} (hμ : 0 < μ) (hε : 0 < ε) (hεμ : ε ≤ μ)
    (hb : Real.sqrt μ + Real.sqrt ε ≤ Real.sqrt b)
    (hlo : Real.sqrt μ / Real.sqrt b - Real.sqrt ε / Real.sqrt b ≤ c)
    (hhi : c ≤ Real.sqrt μ / Real.sqrt b + Real.sqrt ε / Real.sqrt b) :
    0 ≤ H μ (gammaStar μ b + ε) b c - c ∧ H μ (gammaStar μ b + ε) b c - c ≤ 2 * ε := by
  have hv : 0 < Real.sqrt μ := Real.sqrt_pos.2 hμ
  have he : 0 < Real.sqrt ε := Real.sqrt_pos.2 hε
  have hu : 0 < Real.sqrt b := by linarith
  have hb0 : 0 < b := Real.sqrt_pos.1 hu
  have hev : Real.sqrt ε ≤ Real.sqrt μ := Real.sqrt_le_sqrt hεμ
  have hee : Real.sqrt ε * Real.sqrt ε = ε := Real.mul_self_sqrt hε.le
  rw [← sub_div, div_le_iff₀ hu] at hlo
  rw [← add_div, le_div_iff₀ hu] at hhi
  have hc0 : 0 ≤ c := by
    have : 0 ≤ Real.sqrt μ - Real.sqrt ε := by linarith
    by_contra h; push_neg at h; nlinarith [mul_neg_of_neg_of_pos h hu]
  have hc1 : c ≤ 1 := by
    by_contra h; push_neg at h; nlinarith [mul_lt_mul_of_pos_right h hu]
  have hsq : (Real.sqrt b * c - Real.sqrt μ) ^ 2 ≤ ε := by
    have h1 : 0 ≤ Real.sqrt ε - (Real.sqrt b * c - Real.sqrt μ) := by linarith
    have h2 : 0 ≤ Real.sqrt ε + (Real.sqrt b * c - Real.sqrt μ) := by linarith
    nlinarith [mul_nonneg h1 h2]
  rw [H_sub_self, Q_threshold_plus hμ.le hb0.le]
  have hQ0 : 0 ≤ (Real.sqrt b * c - Real.sqrt μ) ^ 2 + ε * c := by positivity
  have hQ2 : (Real.sqrt b * c - Real.sqrt μ) ^ 2 + ε * c ≤ 2 * ε := by nlinarith
  constructor
  · exact mul_nonneg (by linarith) hQ0
  · nlinarith [mul_nonneg (by linarith : 0 ≤ 1 - c) hQ0]

/-- Row M16. From the lower edge of the window, reaching its upper edge takes
at least `1 / (√b √ε)` generations. -/
theorem slowing {μ b ε : ℝ} (hμ : 0 < μ) (hε : 0 < ε) (hεμ : ε ≤ μ)
    (hb : Real.sqrt μ + Real.sqrt ε ≤ Real.sqrt b) (n : ℕ)
    (hn : Real.sqrt μ / Real.sqrt b + Real.sqrt ε / Real.sqrt b ≤
      (H μ (gammaStar μ b + ε) b)^[n] (Real.sqrt μ / Real.sqrt b - Real.sqrt ε / Real.sqrt b)) :
    1 / (Real.sqrt b * Real.sqrt ε) ≤ n := by
  set a := Real.sqrt μ / Real.sqrt b - Real.sqrt ε / Real.sqrt b with ha
  set a' := Real.sqrt μ / Real.sqrt b + Real.sqrt ε / Real.sqrt b with ha'
  set f := H μ (gammaStar μ b + ε) b with hf
  have claim : ∀ k : ℕ, (a ≤ f^[k] a ∧ f^[k] a ≤ a + 2 * ε * k) ∨ a' ≤ a + 2 * ε * k := by
    intro k
    induction k with
    | zero => left; simp
    | succ k ih =>
      rcases ih with ⟨h1, h2⟩ | h
      · rcases lt_or_ge (f^[k] a) a' with hlt | hge
        · left
          obtain ⟨s0, s1⟩ := step_bounds hμ hε hεμ hb h1 hlt.le
          rw [Function.iterate_succ_apply', Nat.cast_succ]
          constructor <;> linarith
        · right; rw [Nat.cast_succ]; linarith
      · right; rw [Nat.cast_succ]; linarith
  have key : a' ≤ a + 2 * ε * n := by
    rcases claim n with ⟨_, h2⟩ | h
    · linarith
    · exact h
  have hv : 0 < Real.sqrt μ := Real.sqrt_pos.2 hμ
  have he : 0 < Real.sqrt ε := Real.sqrt_pos.2 hε
  have hu : 0 < Real.sqrt b := by linarith
  have hee : Real.sqrt ε * Real.sqrt ε = ε := Real.mul_self_sqrt hε.le
  have h1 : Real.sqrt ε / Real.sqrt b ≤ ε * n := by
    rw [ha, ha'] at key; linarith
  rw [div_le_iff₀ hu] at h1
  rw [div_le_iff₀ (by positivity)]
  by_contra hcon
  push_neg at hcon
  have := mul_lt_mul_of_pos_left hcon he
  nlinarith

/-- Control: the start matters. From the upper edge no generation is needed,
while the bound is positive. -/
theorem control_start_matters {μ b ε : ℝ} (hε : 0 < ε) (hb : 0 < b) :
    (H μ (gammaStar μ b + ε) b)^[0] (Real.sqrt μ / Real.sqrt b + Real.sqrt ε / Real.sqrt b) =
      Real.sqrt μ / Real.sqrt b + Real.sqrt ε / Real.sqrt b ∧
    0 < 1 / (Real.sqrt b * Real.sqrt ε) := by
  refine ⟨rfl, ?_⟩
  have := Real.sqrt_pos.2 hε
  have := Real.sqrt_pos.2 hb
  positivity

/-- At `b = 3/10` and excess mobility `1/100`, the window hypotheses hold with
`μ = 1/20` and the bound is at least 18 generations. -/
theorem bound_at_example :
    (1 / 100 : ℝ) ≤ 1 / 20 ∧
    Real.sqrt (1 / 20) + Real.sqrt (1 / 100) ≤ Real.sqrt (3 / 10) ∧
    (18 : ℝ) ≤ 1 / (Real.sqrt (3 / 10) * Real.sqrt (1 / 100)) := by
  have h100 : Real.sqrt (1 / 100 : ℝ) = 1 / 10 := by
    rw [show (1 / 100 : ℝ) = (1 / 10) ^ 2 by norm_num, Real.sqrt_sq (by norm_num)]
  have h20 : Real.sqrt (1 / 20 : ℝ) ≤ 1 / 4 := by
    rw [show (1 / 4 : ℝ) = Real.sqrt ((1 / 4) ^ 2) by rw [Real.sqrt_sq (by norm_num)]]
    exact Real.sqrt_le_sqrt (by norm_num)
  have h310lo : (1 / 2 : ℝ) ≤ Real.sqrt (3 / 10) := by
    rw [show (1 / 2 : ℝ) = Real.sqrt ((1 / 2) ^ 2) by rw [Real.sqrt_sq (by norm_num)]]
    exact Real.sqrt_le_sqrt (by norm_num)
  have h310hi : Real.sqrt (3 / 10 : ℝ) ≤ 5 / 9 := by
    rw [show (5 / 9 : ℝ) = Real.sqrt ((5 / 9) ^ 2) by rw [Real.sqrt_sq (by norm_num)]]
    exact Real.sqrt_le_sqrt (by norm_num)
  refine ⟨by norm_num, by rw [h100]; linarith, ?_⟩
  rw [h100]
  have hpos : 0 < Real.sqrt (3 / 10 : ℝ) := by linarith
  rw [le_div_iff₀ (by positivity)]
  nlinarith

end Slowing
