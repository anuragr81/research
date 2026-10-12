import Mathlib.Analysis.Convex.Function
import Mathlib.Tactic

namespace FriedmanSavage1948

section Choice

theorem prefers_certainty_iff_above_chord {U : ℝ → ℝ} {a I₁ I₂ : ℝ} :
    a * U I₁ + (1 - a) * U I₂ < U (a * I₁ + (1 - a) * I₂) ↔
      ¬ (U (a * I₁ + (1 - a) * I₂) ≤ a * U I₁ + (1 - a) * U I₂) := by
  constructor
  · intro h h'
    linarith
  · intro h
    push_neg at h
    exact h

theorem concave_buys_insurance {U : ℝ → ℝ} {S : Set ℝ} (hU : StrictConcaveOn ℝ S U) {a I₁ I₂ : ℝ}
    (h1 : I₁ ∈ S) (h2 : I₂ ∈ S) (hne : I₁ ≠ I₂) (ha0 : 0 < a) (ha1 : a < 1) :
    a * U I₁ + (1 - a) * U I₂ < U (a * I₁ + (1 - a) * I₂) := by
  have := hU.2 h1 h2 hne ha0 (by linarith : 0 < 1 - a) (by ring)
  simpa [smul_eq_mul] using this

theorem convex_buys_gamble {U : ℝ → ℝ} {S : Set ℝ} (hU : StrictConvexOn ℝ S U) {a I₁ I₂ : ℝ}
    (h1 : I₁ ∈ S) (h2 : I₂ ∈ S) (hne : I₁ ≠ I₂) (ha0 : 0 < a) (ha1 : a < 1) :
    U (a * I₁ + (1 - a) * I₂) < a * U I₁ + (1 - a) * U I₂ := by
  have := hU.2 h1 h2 hne ha0 (by linarith : 0 < 1 - a) (by ring)
  simpa [smul_eq_mul] using this

theorem equivalent_below_mean {U : ℝ → ℝ} (hU : StrictMono U) {Istar Ibar Ubar : ℝ}
    (heq : U Istar = Ubar) (hbelow : Ubar < U Ibar) : Istar < Ibar :=
  hU.lt_iff_lt.mp (heq ▸ hbelow)

theorem control_insurance_needs_strict_concavity (a I₁ I₂ : ℝ) :
    ¬ (a * (fun x : ℝ => x) I₁ + (1 - a) * (fun x : ℝ => x) I₂
      < (fun x : ℝ => x) (a * I₁ + (1 - a) * I₂)) := by
  simp only
  intro h
  linarith

end Choice

section Experiment

theorem experiment_600 {U : ℝ → ℝ} (h500 : U 500 = 0) (h1000 : U 1000 = 1)
    (hind : U 600 = 2 / 5 * U 500 + 3 / 5 * U 1000) : U 600 = 3 / 5 := by
  rw [hind, h500, h1000]
  ring

theorem experiment_10000 {U : ℝ → ℝ} (h500 : U 500 = 0) (h1000 : U 1000 = 1)
    (hind : 4 / 5 * U 500 + 1 / 5 * U 10000 = U 1000) : U 10000 = 5 := by
  rw [h500, h1000] at hind
  linarith

theorem never_pay_more_than_max_gain {U : ℝ → ℝ} (hU : Monotone U) {a x₁ x₂ : ℝ} (ha0 : 0 ≤ a)
    (ha1 : a ≤ 1) : a * U x₁ + (1 - a) * U x₂ ≤ U (max x₁ x₂) := by
  have h1 : U x₁ ≤ U (max x₁ x₂) := hU (le_max_left _ _)
  have h2 : U x₂ ≤ U (max x₁ x₂) := hU (le_max_right _ _)
  nlinarith

theorem compound_gamble_refused {p A B C : ℝ} (hp0 : 0 ≤ p) (hp1 : p ≤ 1) (hA : A < C)
    (hB : B < C) : p * A + (1 - p) * B < C := by
  rcases lt_or_eq_of_le hp1 with h | h
  · have h1 : p * A ≤ p * C := mul_le_mul_of_nonneg_left hA.le hp0
    have h2 : (1 - p) * B < (1 - p) * C := mul_lt_mul_of_pos_left hB (by linarith)
    linarith
  · subst h
    linarith

theorem control_compound_needs_both_refused :
    ¬ ((1 / 2 : ℝ) * 0 + (1 - 1 / 2) * 2 < 1) := by
  norm_num

end Experiment

section Lotteries

theorem single_prize_beats_two {U : ℝ → ℝ} {S : Set ℝ} (hU : ConvexOn ℝ S U) {w P q : ℝ}
    (hq : 0 ≤ q) (h0 : w ∈ S) (h2 : w + 2 * P ∈ S) :
    2 * q * U (w + P) + (1 - 2 * q) * U w ≤ q * U (w + 2 * P) + (1 - q) * U w := by
  have hmid : U (w + P) ≤ (1 / 2) * U w + (1 / 2) * U (w + 2 * P) := by
    have := hU.2 h0 h2 (by norm_num : (0 : ℝ) ≤ 1 / 2) (by norm_num : (0 : ℝ) ≤ 1 / 2) (by norm_num)
    simp only [smul_eq_mul] at this
    have e : 1 / 2 * w + 1 / 2 * (w + 2 * P) = w + P := by ring
    rw [e] at this
    exact this
  nlinarith

theorem control_two_prizes_can_win :
    1 * (Real.sqrt 4) + 0 * Real.sqrt 0 > (1 / 2 : ℝ) * Real.sqrt 8 + (1 - 1 / 2) * Real.sqrt 0 := by
  have h4 : Real.sqrt 4 = 2 := by
    rw [show (4 : ℝ) = 2 ^ 2 by norm_num, Real.sqrt_sq (by norm_num)]
  have h8 : Real.sqrt 8 < 3 := by
    rw [show (3 : ℝ) = Real.sqrt 9 by rw [show (9 : ℝ) = 3 ^ 2 by norm_num, Real.sqrt_sq (by norm_num)]]
    exact Real.sqrt_lt_sqrt (by norm_num) (by norm_num)
  rw [h4, Real.sqrt_zero]
  nlinarith

end Lotteries

end FriedmanSavage1948
