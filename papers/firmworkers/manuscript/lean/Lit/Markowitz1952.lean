import Mathlib.Analysis.Convex.Function
import Mathlib.Tactic

namespace Markowitz1952

section SymmetricBets

theorem avoids_symmetric_bets_iff {U : ℝ → ℝ} (h0 : U 0 = 0) (X : ℝ) :
    (1 / 2) * U X + (1 / 2) * U (-X) < U 0 ↔ U X < -U (-X) := by
  rw [h0]
  constructor <;> intro h <;> linarith

theorem falls_faster_to_the_left {U : ℝ → ℝ} (hneg : U (-1) < 0) (h : |U (-1)| > U 1) :
    (1 / 2) * U 1 + (1 / 2) * U (-1) < 0 := by
  rw [abs_of_neg hneg] at h
  linarith

theorem control_printed_p154_inequality_favours_bets {U : ℝ → ℝ} (hneg : U (-1) < 0)
    (h : U 1 > |U (-1)|) : 0 < (1 / 2) * U 1 + (1 / 2) * U (-1) := by
  rw [abs_of_neg hneg] at h
  linarith

end SymmetricBets

section Participation

theorem symmetric_bet_value_falls_with_stake {U : ℝ → ℝ} (hU : ConcaveOn ℝ Set.univ U) {w s₁ s₂ : ℝ}
    (h1 : 0 ≤ s₁) (h12 : s₁ ≤ s₂) :
    (1 / 2) * U (w + s₂) + (1 / 2) * U (w - s₂) ≤ (1 / 2) * U (w + s₁) + (1 / 2) * U (w - s₁) := by
  rcases eq_or_lt_of_le (h1.trans h12) with h0 | hpos
  · have hs1 : s₁ = 0 := by linarith
    have hs2 : s₂ = 0 := by linarith
    subst hs1 hs2
    simp
  · set t := (s₁ + s₂) / (2 * s₂) with ht
    have ht0 : 0 ≤ t := div_nonneg (by linarith) (by linarith)
    have ht1 : t ≤ 1 := (div_le_one (by linarith)).mpr (by linarith)
    have e1 : t * (w + s₂) + (1 - t) * (w - s₂) = w + s₁ := by
      rw [ht]
      field_simp
      ring
    have e2 : (1 - t) * (w + s₂) + t * (w - s₂) = w - s₁ := by
      rw [ht]
      field_simp
      ring
    have c1 := hU.2 (Set.mem_univ (w + s₂)) (Set.mem_univ (w - s₂)) ht0 (by linarith : 0 ≤ 1 - t)
      (by ring)
    have c2 := hU.2 (Set.mem_univ (w + s₂)) (Set.mem_univ (w - s₂)) (by linarith : 0 ≤ 1 - t) ht0
      (by ring)
    simp only [smul_eq_mul] at c1 c2
    rw [e1] at c1
    rw [e2] at c2
    nlinarith

theorem control_stakes_need_concavity :
    (1 / 2 : ℝ) * (0 + 0) ^ 2 + (1 / 2) * (0 - 0) ^ 2
      < (1 / 2) * (0 + 1) ^ 2 + (1 / 2) * (0 - 1) ^ 2 := by
  norm_num

end Participation

section CustomaryWealth

theorem same_behaviour_rich_or_poor (g : ℝ → ℝ) {w s e : ℝ} (hs : s ≠ 0) :
    g (((w + s * e) - w) / s) = g e := by
  congr 1
  field_simp

theorem windfall_moves_from_middle_inflection {c w gain : ℝ} (hc : c = w) :
    (w + gain) - c = gain := by
  rw [hc]
  ring

theorem control_customary_needs_adaptation_rule (w gain : ℝ) (hg : gain ≠ 0) :
    (w + gain) - (w + gain) ≠ gain := by
  simp
  exact fun h => hg h.symm

end CustomaryWealth

end Markowitz1952
