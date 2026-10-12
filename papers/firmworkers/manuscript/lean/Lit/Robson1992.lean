import Mathlib.Analysis.SpecialFunctions.Pow.Deriv
import Mathlib.Analysis.SpecialFunctions.Sqrt
import Mathlib.Analysis.Convex.Jensen
import Mathlib.Tactic

open Real Finset

namespace Robson1992

section Status

variable {ι : Type*}

noncomputable def tieWeight (a b : ℝ) : ℝ := if a < b then 1 else if a = b then 1 / 2 else 0

noncomputable def statusCount (s : Finset ι) (w : ι → ℝ) (i : ι) : ℝ :=
  ∑ j ∈ s, tieWeight (w j) (w i)

theorem tieWeight_add_swap (a b : ℝ) : tieWeight a b + tieWeight b a = 1 := by
  unfold tieWeight
  rcases lt_trichotomy a b with h | h | h
  · simp [h, not_lt.mpr h.le, ne_of_gt h]
  · subst h
    norm_num
  · simp [h, not_lt.mpr h.le, ne_of_gt h]

theorem total_status (s : Finset ι) (w : ι → ℝ) :
    ∑ i ∈ s, statusCount s w i = (s.card : ℝ) ^ 2 / 2 := by
  unfold statusCount
  have hc : ∑ i ∈ s, ∑ j ∈ s, tieWeight (w j) (w i) = ∑ i ∈ s, ∑ j ∈ s, tieWeight (w i) (w j) :=
    Finset.sum_comm
  have h2 : ∑ i ∈ s, ∑ j ∈ s, tieWeight (w j) (w i) + ∑ i ∈ s, ∑ j ∈ s, tieWeight (w i) (w j)
      = (s.card : ℝ) ^ 2 := by
    rw [← sum_add_distrib]
    simp only [← sum_add_distrib, tieWeight_add_swap, sum_const, nsmul_eq_mul, mul_one, sq]
  linarith

theorem mean_status_half {s : Finset ι} (hs : s.Nonempty) (w : ι → ℝ) :
    (∑ i ∈ s, statusCount s w i / s.card) / s.card = 1 / 2 := by
  have hc : (0 : ℝ) < s.card := by exact_mod_cast hs.card_pos
  rw [← sum_div, total_status]
  field_simp
  ring

theorem control_total_status_needs_half_ties :
    ∑ i ∈ ({0, 1} : Finset ℕ), ∑ j ∈ ({0, 1} : Finset ℕ),
        (if (fun _ : ℕ => (1 : ℝ)) j ≤ (fun _ : ℕ => (1 : ℝ)) i then (1 : ℝ) else 0)
      ≠ (({0, 1} : Finset ℕ).card : ℝ) ^ 2 / 2 := by
  norm_num

end Status

section MassPoint

theorem mass_point_bet_gains {U : ℝ → ℝ} (hU : StrictMono U) {F Fp : ℝ} (hF : F < Fp) :
    ∃ p : ℝ, 0 < p ∧ p < 1 ∧ U (F / 2 + Fp / 2) < p * U Fp + (1 - p) * U F := by
  have h1 : U (F / 2 + Fp / 2) < U Fp := hU (by linarith)
  have h2 : U F < U (F / 2 + Fp / 2) := hU (by linarith)
  have hAC : 0 < U Fp - U F := by linarith
  set q := (U (F / 2 + Fp / 2) - U F) / (U Fp - U F) with hqdef
  have hq0 : 0 < q := div_pos (by linarith) hAC
  have hq1 : q < 1 := (div_lt_one hAC).mpr (by linarith)
  have hqm : q * (U Fp - U F) = U (F / 2 + Fp / 2) - U F := div_mul_cancel₀ _ hAC.ne'
  refine ⟨(q + 1) / 2, by linarith, by linarith, ?_⟩
  nlinarith

theorem control_mass_point_needs_jump (U : ℝ → ℝ) (F p : ℝ) :
    ¬ U (F / 2 + F / 2) < p * U F + (1 - p) * U F := by
  have e : F / 2 + F / 2 = F := by ring
  rw [e]
  intro h
  nlinarith

end MassPoint

section Corollary

variable {ι : Type*}

theorem concave_rejects_fair_gambles {V : ℝ → ℝ} {S : Set ℝ} (hV : ConcaveOn ℝ S V)
    {t : Finset ι} {pr y : ι → ℝ} {w : ℝ} (hpr : ∀ i ∈ t, 0 ≤ pr i) (h1 : ∑ i ∈ t, pr i = 1)
    (hfair : ∑ i ∈ t, pr i * y i = 0) (hS : ∀ i ∈ t, w + y i ∈ S) :
    ∑ i ∈ t, pr i * V (w + y i) ≤ V w := by
  have key := hV.le_map_sum hpr h1 hS
  simp only [smul_eq_mul] at key
  have hsum : ∑ i ∈ t, pr i * (w + y i) = w := by
    simp only [mul_add, sum_add_distrib, hfair, ← sum_mul, h1]
    ring
  rwa [hsum] at key

theorem control_rejection_needs_concavity :
    (0 : ℝ) ^ 2 < ∑ i ∈ ({0, 1} : Finset ℕ),
      (1 / 2 : ℝ) * ((0 : ℝ) + (if i = 0 then (1 : ℝ) else -1)) ^ 2 := by
  norm_num

end Corollary

section ExampleA

theorem exampleA_first_deriv {α β k w : ℝ} (hw : 0 < w) (hα : α ≠ 0) :
    HasDerivAt (fun v => v ^ α / α + k * v ^ β) (w ^ (α - 1) + k * β * w ^ (β - 1)) w := by
  have h1 := (Real.hasDerivAt_rpow_const (p := α) (Or.inl hw.ne')).div_const α
  have h2 := (Real.hasDerivAt_rpow_const (p := β) (Or.inl hw.ne')).const_mul k
  convert h1.add h2 using 1
  field_simp
  ring

theorem exampleA_second_deriv {α β k w : ℝ} (hw : 0 < w) :
    HasDerivAt (fun v => v ^ (α - 1) + k * β * v ^ (β - 1))
      ((α - 1) * w ^ (α - 2) + k * β * (β - 1) * w ^ (β - 2)) w := by
  have h1 := Real.hasDerivAt_rpow_const (p := α - 1) (Or.inl hw.ne')
  have h2 := (Real.hasDerivAt_rpow_const (p := β - 1) (Or.inl hw.ne')).const_mul (k * β)
  convert h1.add h2 using 1
  have ea : α - 1 - 1 = α - 2 := by ring
  have eb : β - 1 - 1 = β - 2 := by ring
  rw [ea, eb]
  ring

noncomputable def inflection (α β k : ℝ) : ℝ := ((1 - α) / (k * β * (β - 1))) ^ (1 / (β - α))

theorem exampleA_convex_iff {α β k w : ℝ} (hw : 0 < w) (hα : α < 1) (hβ : 1 < β) (hk : 0 < k) :
    0 < (α - 1) * w ^ (α - 2) + k * β * (β - 1) * w ^ (β - 2) ↔ inflection α β k < w := by
  have hc : 0 < k * β * (β - 1) := by
    have : 0 < β - 1 := by linarith
    positivity
  have he : 0 < β - α := by linarith
  have hq : 0 ≤ (1 - α) / (k * β * (β - 1)) := div_nonneg (by linarith) hc.le
  have hsplit : w ^ (β - 2) = w ^ (β - α) * w ^ (α - 2) := by
    rw [← Real.rpow_add hw]
    ring_nf
  have hpos : 0 < w ^ (α - 2) := Real.rpow_pos_of_pos hw _
  have hinf : (inflection α β k) ^ (β - α) = (1 - α) / (k * β * (β - 1)) := by
    unfold inflection
    rw [← Real.rpow_mul hq]
    field_simp
  have hinf0 : 0 ≤ inflection α β k := Real.rpow_nonneg hq _
  rw [← Real.rpow_lt_rpow_iff hinf0 hw.le he, hinf, hsplit, div_lt_iff₀ hc]
  constructor
  · intro h
    have : 0 < w ^ (α - 2) * (k * β * (β - 1) * w ^ (β - α) - (1 - α)) := by nlinarith
    have := (pos_iff_pos_of_mul_pos this).mp hpos
    linarith
  · intro h
    have : 0 < w ^ (α - 2) * (k * β * (β - 1) * w ^ (β - α) - (1 - α)) :=
      mul_pos hpos (by linarith)
    nlinarith

theorem inflection_lt_one {α β k : ℝ} (hα : α < 1) (hβ : 1 < β) (hk : 0 < k)
    (h : 1 - α < k * β * (β - 1)) : inflection α β k < 1 := by
  have hc : 0 < k * β * (β - 1) := by
    have : 0 < β - 1 := by linarith
    positivity
  unfold inflection
  exact Real.rpow_lt_one (div_nonneg (by linarith) hc.le) ((div_lt_one hc).mpr h)
    (one_div_pos.mpr (by linarith))

theorem exampleA'_inflection : inflection 0 2 (3 / 4) = Real.sqrt (2 / 3) := by
  unfold inflection
  rw [Real.sqrt_eq_rpow]
  norm_num

theorem control_convex_region_needs_status {α w : ℝ} (hw : 0 < w) (hα : α < 1) :
    ¬ 0 < (α - 1) * w ^ (α - 2) + 0 * 2 * (2 - 1) * w ^ ((2 : ℝ) - 2) := by
  have : (α - 1) * w ^ (α - 2) < 0 := mul_neg_of_neg_of_pos (by linarith) (Real.rpow_pos_of_pos hw _)
  intro h
  linarith

end ExampleA

section Scaling

theorem scaled_inflection {α β k γ : ℝ} (hα : α < 1) (hβ : 1 < β) (hk : 0 < k) (hγ : 0 < γ) :
    inflection α β (k * γ ^ (-β)) / γ = inflection α β k * γ ^ (α / (β - α)) := by
  have hc : 0 < k * β * (β - 1) := by
    have : 0 < β - 1 := by linarith
    positivity
  have he : 0 < β - α := by linarith
  have hq : 0 ≤ (1 - α) / (k * β * (β - 1)) := div_nonneg (by linarith) hc.le
  have hg : 0 < γ ^ β := Real.rpow_pos_of_pos hγ _
  have e1 : (1 - α) / (k * γ ^ (-β) * β * (β - 1)) = (1 - α) / (k * β * (β - 1)) * γ ^ β := by
    rw [Real.rpow_neg hγ.le]
    field_simp
  unfold inflection
  rw [e1, Real.mul_rpow hq hg.le, ← Real.rpow_mul hγ.le, mul_div_assoc]
  congr 1
  rw [← Real.rpow_sub_one hγ.ne']
  congr 1
  field_simp

theorem log_case_proportion_constant {β k γ : ℝ} (hβ : 1 < β) (hk : 0 < k) (hγ : 0 < γ) :
    inflection 0 β (k * γ ^ (-β)) / γ = inflection 0 β k := by
  rw [scaled_inflection (by norm_num) hβ hk hγ]
  simp

theorem proportion_rises {α β k γ₁ γ₂ : ℝ} (hα0 : 0 < α) (hα : α < 1) (hβ : 1 < β) (hk : 0 < k)
    (hγ₁ : 0 < γ₁) (h12 : γ₁ < γ₂) :
    inflection α β (k * γ₁ ^ (-β)) / γ₁ < inflection α β (k * γ₂ ^ (-β)) / γ₂ := by
  have hc : 0 < k * β * (β - 1) := by
    have : 0 < β - 1 := by linarith
    positivity
  have hI : 0 < inflection α β k :=
    Real.rpow_pos_of_pos (div_pos (by linarith) hc) _
  rw [scaled_inflection hα hβ hk hγ₁, scaled_inflection hα hβ hk (hγ₁.trans h12)]
  exact mul_lt_mul_of_pos_left (Real.rpow_lt_rpow hγ₁.le h12 (div_pos hα0 (by linarith))) hI

theorem control_fixed_inflection_proportion_falls {c γ₁ γ₂ : ℝ} (hc : 0 < c) (hγ₁ : 0 < γ₁)
    (h12 : γ₁ < γ₂) : c / γ₂ < c / γ₁ :=
  div_lt_div_of_pos_left hc hγ₁ h12

end Scaling

section Externality

theorem total_fixed_under_reassignment {ι : Type*} [Fintype ι] (σ : Equiv.Perm ι) (w : ι → ℝ) :
    ∑ i, w (σ i) = ∑ i, w i :=
  Equiv.sum_comp σ w

theorem control_total_fixed_needs_whole_population :
    ∑ i ∈ ({0} : Finset (Fin 2)), (fun j : Fin 2 => if j = 0 then (1 : ℝ) else 2) (Equiv.swap 0 1 i)
      ≠ ∑ i ∈ ({0} : Finset (Fin 2)), (fun j : Fin 2 => if j = 0 then (1 : ℝ) else 2) i := by
  simp [Equiv.swap_apply_left]

end Externality

section Lemma3

theorem status_lottery_beats_equal {g : ℝ → ℝ} (hg : StrictConvexOn ℝ (Set.Icc 0 1) g) {a b : ℝ}
    (ha : 0 ≤ a) (hab : a < b) (hb : b ≤ 1) :
    g ((a + b) / 2) < (1 / 2) * g ((3 * a + b) / 4) + (1 / 2) * g ((a + 3 * b) / 4) := by
  have hx : (3 * a + b) / 4 ∈ Set.Icc (0 : ℝ) 1 := ⟨by linarith, by linarith⟩
  have hy : (a + 3 * b) / 4 ∈ Set.Icc (0 : ℝ) 1 := ⟨by linarith, by linarith⟩
  have hne : (3 * a + b) / 4 ≠ (a + 3 * b) / 4 := by
    intro h
    linarith
  have key := hg.2 hx hy hne (by norm_num : (0 : ℝ) < 1 / 2) (by norm_num : (0 : ℝ) < 1 / 2)
    (by norm_num)
  simp only [smul_eq_mul] at key
  have e : 1 / 2 * ((3 * a + b) / 4) + 1 / 2 * ((a + 3 * b) / 4) = (a + b) / 2 := by ring
  rw [e] at key
  exact key

theorem control_status_lottery_needs_strict_convexity (a b : ℝ) :
    ¬ ((fun n : ℝ => n) ((a + b) / 2)
      < (1 / 2) * (fun n : ℝ => n) ((3 * a + b) / 4) + (1 / 2) * (fun n : ℝ => n) ((a + 3 * b) / 4)) := by
  simp only
  intro h
  linarith

end Lemma3

end Robson1992
