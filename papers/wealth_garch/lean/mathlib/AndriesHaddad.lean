/-
================================================================================
  Andries and Haddad, "Information Aversion", Journal of Political Economy
  128(5) (2020), 1901-. What lit/andries_haddad_2020/ attributes to the
  paper, in its notation (θ ≥ 0 the coefficient of disappointment aversion,
  μ the certainty equivalent of eq. (2), the reference point). The paper's
  proofs are in an online appendix that was not supplied. The results here
  are derived from Definition 1's eq. (2) alone, for finite lotteries:
  Gul's certainty equivalent of a two-point lottery, and a two-date
  example in which a partial intermediate signal lowers the value of a
  uniform four-point payoff (Propositions 1 and 2 for that example).
================================================================================
-/

import Mathlib

namespace AndriesHaddad

/-- Eq. (2) for a finite lottery: μ solves
    μ (1 + θ P(x < μ)) = E[x] + θ E[x; x < μ]. -/
def solvesCE (θ μ : ℝ) (xs ps : List ℝ) : Prop :=
  μ * (1 + θ * ((List.zipWith (fun x p => if x < μ then p else 0) xs ps).sum))
    = (List.zipWith (fun x p => p * x) xs ps).sum
      + θ * ((List.zipWith (fun x p => if x < μ then p * x else 0) xs ps).sum)

/-- The certainty equivalent of the lottery L with probability q, H with 1 − q. -/
noncomputable def ce2 (θ q L H : ℝ) : ℝ := (q * (1 + θ) * L + (1 - q) * H) / (1 + θ * q)

/-- The two-point certainty equivalent lies strictly between its outcomes. -/
theorem ce2_mem {θ q L H : ℝ} (hθ : 0 ≤ θ) (hq0 : 0 < q) (hq1 : q < 1) (hLH : L < H) :
    L < ce2 θ q L H ∧ ce2 θ q L H < H := by
  unfold ce2
  have hd : 0 < 1 + θ * q := by positivity
  constructor
  · rw [lt_div_iff₀ hd]; nlinarith [mul_pos (sub_pos.mpr hq1) (sub_pos.mpr hLH)]
  · rw [div_lt_iff₀ hd]
    nlinarith [mul_pos (mul_pos hq0 (by linarith : (0:ℝ) < 1 + θ)) (sub_pos.mpr hLH),
      mul_nonneg hθ (mul_nonneg hq0.le (sub_pos.mpr hLH).le)]

/-- `ce2` solves eq. (2) for the two-point lottery: only L disappoints. -/
theorem ce2_solves {θ q L H : ℝ} (hθ : 0 ≤ θ) (hq0 : 0 < q) (hq1 : q < 1) (hLH : L < H) :
    solvesCE θ (ce2 θ q L H) [L, H] [q, 1 - q] := by
  obtain ⟨hL, hH⟩ := ce2_mem hθ hq0 hq1 hLH
  have hd : 0 < 1 + θ * q := by positivity
  unfold solvesCE
  simp only [List.zipWith_cons_cons, List.zipWith_nil_left, List.sum_cons, List.sum_nil,
    if_pos hL, if_neg (not_lt.mpr hH.le)]
  unfold ce2
  field_simp
  ring

/-- The one-shot certainty equivalent of the uniform payoff on {0, 1, 2, 3}. -/
noncomputable def oneShot (θ : ℝ) : ℝ := (6 + θ) / (4 + 2 * θ)

/-- The compound value when a signal first reveals whether the payoff is in {0, 1}
    or {2, 3}: each half is valued by `ce2`, then the two values by `ce2` again. -/
noncomputable def compound (θ : ℝ) : ℝ :=
  ce2 θ (1 / 2) (ce2 θ (1 / 2) 0 1) (ce2 θ (1 / 2) 2 3)

theorem compound_eq {θ : ℝ} (hθ : 0 ≤ θ) : compound θ = 3 / (2 + θ) := by
  unfold compound ce2
  have h1 : (2 + θ) ≠ 0 := by linarith
  have h2 : (1 + θ * (1 / 2)) ≠ 0 := by linarith
  field_simp
  ring

/-- For 0 ≤ θ < 2, `oneShot θ` solves eq. (2) for the uniform four-point payoff:
    0 and 1 disappoint, 2 and 3 do not. -/
theorem oneShot_solves {θ : ℝ} (hθ : 0 ≤ θ) (hθ2 : θ < 2) :
    solvesCE θ (oneShot θ) [0, 1, 2, 3] [1 / 4, 1 / 4, 1 / 4, 1 / 4] := by
  have hd : 0 < 4 + 2 * θ := by linarith
  have h0 : (0:ℝ) < oneShot θ := by unfold oneShot; positivity
  have h1 : (1:ℝ) < oneShot θ := by unfold oneShot; rw [lt_div_iff₀ hd]; linarith
  have h2 : ¬ ((2:ℝ) < oneShot θ) := by
    unfold oneShot; rw [not_lt, div_le_iff₀ hd]; linarith
  have h3 : ¬ ((3:ℝ) < oneShot θ) := by
    unfold oneShot; rw [not_lt, div_le_iff₀ hd]; linarith
  unfold solvesCE
  simp only [List.zipWith_cons_cons, List.zipWith_nil_left, List.sum_cons, List.sum_nil,
    if_pos h0, if_pos h1, if_neg h2, if_neg h3]
  unfold oneShot
  field_simp
  ring

/-- The gap between revealing the payoff at once and revealing it in two steps. -/
theorem oneShot_sub_compound {θ : ℝ} (hθ : 0 ≤ θ) :
    oneShot θ - compound θ = θ / (4 + 2 * θ) := by
  rw [compound_eq hθ]
  unfold oneShot
  have h1 : (2 + θ) ≠ 0 := by linarith
  have h2 : (4 + 2 * θ) ≠ 0 := by linarith
  field_simp
  ring

/-- Propositions 1 and 2 for the example: with disappointment aversion the agent
    strictly prefers the payoff revealed at a single date. -/
theorem information_aversion_example {θ : ℝ} (hθ : 0 < θ) : compound θ < oneShot θ := by
  have h := oneShot_sub_compound hθ.le
  have : 0 < θ / (4 + 2 * θ) := by positivity
  linarith

/-- `ce2` is the only solution of eq. (2) for the two-point lottery. -/
theorem ce2_unique {θ q L H μ : ℝ} (hθ : 0 ≤ θ) (hq0 : 0 < q) (hq1 : q < 1) (hLH : L < H)
    (h : solvesCE θ μ [L, H] [q, 1 - q]) : μ = ce2 θ q L H := by
  unfold solvesCE at h
  simp only [List.zipWith_cons_cons, List.zipWith_nil_left, List.sum_cons, List.sum_nil] at h
  have hd : 0 < 1 + θ * q := by positivity
  rcases le_or_gt μ L with hL | hL
  · -- nothing disappoints, so μ is the mean, which exceeds L
    exfalso
    simp only [if_neg (not_lt.mpr hL), if_neg (not_lt.mpr (hL.trans hLH.le))] at h
    nlinarith [mul_pos (sub_pos.mpr hq1) (sub_pos.mpr hLH)]
  rcases le_or_gt μ H with hH | hH
  · simp only [if_pos hL, if_neg (not_lt.mpr hH)] at h
    unfold ce2
    rw [eq_div_iff hd.ne']
    linear_combination h
  · -- both disappoint, so μ is again the mean, which is below H
    exfalso
    simp only [if_pos hL, if_pos hH] at h
    have hm : (1 + θ) * (μ - (q * L + (1 - q) * H)) = 0 := by linear_combination h
    rcases mul_eq_zero.mp hm with h1 | h1
    · linarith
    · nlinarith [mul_pos hq0 (sub_pos.mpr hLH)]

/-- For 0 ≤ θ < 2, `oneShot θ` is the only solution of eq. (2) for the uniform
    four-point payoff. -/
theorem oneShot_unique {θ μ : ℝ} (hθ : 0 ≤ θ) (hθ2 : θ < 2)
    (h : solvesCE θ μ [0, 1, 2, 3] [1 / 4, 1 / 4, 1 / 4, 1 / 4]) : μ = oneShot θ := by
  unfold solvesCE at h
  simp only [List.zipWith_cons_cons, List.zipWith_nil_left, List.sum_cons, List.sum_nil] at h
  have hd : 0 < 4 + 2 * θ := by linarith
  rcases le_or_gt μ 0 with h0 | h0
  · -- nothing disappoints, so μ = 3/2 > 0
    exfalso
    simp only [if_neg (not_lt.mpr h0), if_neg (not_lt.mpr (h0.trans (by norm_num : (0:ℝ) ≤ 1))),
      if_neg (not_lt.mpr (h0.trans (by norm_num : (0:ℝ) ≤ 2))),
      if_neg (not_lt.mpr (h0.trans (by norm_num : (0:ℝ) ≤ 3)))] at h
    nlinarith
  rcases le_or_gt μ 1 with h1 | h1
  · -- only 0 disappoints, so μ(1 + θ/4) = 3/2, impossible with μ ≤ 1 and θ < 2
    exfalso
    simp only [if_pos h0, if_neg (not_lt.mpr h1), if_neg (not_lt.mpr (h1.trans (by norm_num : (1:ℝ) ≤ 2))),
      if_neg (not_lt.mpr (h1.trans (by norm_num : (1:ℝ) ≤ 3)))] at h
    nlinarith [mul_nonneg hθ (sub_nonneg.mpr h1)]
  rcases le_or_gt μ 2 with h2 | h2
  · -- 0 and 1 disappoint: the case of `oneShot`
    simp only [if_pos h0, if_pos h1, if_neg (not_lt.mpr h2),
      if_neg (not_lt.mpr (h2.trans (by norm_num : (2:ℝ) ≤ 3)))] at h
    unfold oneShot
    rw [eq_div_iff hd.ne']
    linear_combination 4 * h
  rcases le_or_gt μ 3 with h3 | h3
  · -- 0, 1 and 2 disappoint, so μ(4 + 3θ) = 6 + 3θ, impossible with μ > 2
    exfalso
    simp only [if_pos h0, if_pos h1, if_pos h2, if_neg (not_lt.mpr h3)] at h
    nlinarith [mul_pos (sub_pos.mpr h2) (by linarith : (0:ℝ) < 4 + 3 * θ)]
  · -- all disappoint, so μ = 3/2 < 3
    exfalso
    simp only [if_pos h0, if_pos h1, if_pos h2, if_pos h3] at h
    nlinarith [mul_pos (by linarith : (0:ℝ) < 1 + θ) (sub_pos.mpr h3)]

/-- Proposition 3's consumption share C = 1 − exp(kT) rises in T when k < 0. -/
theorem consumption_increasing_in_T {k T₁ T₂ : ℝ} (hk : k < 0) (h : T₁ < T₂) :
    1 - Real.exp (k * T₁) < 1 - Real.exp (k * T₂) := by
  have : k * T₂ < k * T₁ := by nlinarith
  linarith [Real.exp_lt_exp.mpr this]

/-- Control: without disappointment aversion (θ = 0) the two revelations are valued
    the same, so information aversion needs θ > 0. -/
theorem control_no_aversion_at_theta_zero : compound 0 = oneShot 0 := by
  rw [compound_eq le_rfl]
  unfold oneShot
  norm_num

/-- Control: `ce2_mem` needs q < 1; at q = 1 the value is L, not strictly above it. -/
theorem control_ce2_degenerate : ce2 1 1 0 1 = 0 := by
  unfold ce2
  norm_num

end AndriesHaddad
