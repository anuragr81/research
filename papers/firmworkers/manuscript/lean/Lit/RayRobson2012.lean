import Mathlib.Analysis.SpecialFunctions.Integrals.Basic
import Mathlib.Analysis.Convex.Jensen
import Mathlib.Tactic

open Real Finset

namespace RayRobson2012

section Status

variable {ι : Type*}

noncomputable def tieWeight (η a b : ℝ) : ℝ := if a < b then 1 else if a = b then 1 - η else 0

noncomputable def ties (s : Finset ι) (c : ι → ℝ) : ℝ :=
  ∑ i ∈ s, ∑ j ∈ s, if c j = c i then (1 : ℝ) else 0

theorem tieWeight_add_swap (η a b : ℝ) :
    tieWeight η a b + tieWeight η b a = 1 + (1 - 2 * η) * (if a = b then 1 else 0) := by
  unfold tieWeight
  rcases lt_trichotomy a b with h | h | h
  · simp [h, not_lt.mpr h.le, ne_of_gt h, ne_of_lt h]
  · subst h
    simp
    ring
  · simp [h, not_lt.mpr h.le, ne_of_gt h, ne_of_lt h]

theorem total_status (η : ℝ) (s : Finset ι) (c : ι → ℝ) :
    ∑ i ∈ s, ∑ j ∈ s, tieWeight η (c j) (c i) = (s.card : ℝ) ^ 2 / 2 + (1 / 2 - η) * ties s c := by
  have hc : ∑ i ∈ s, ∑ j ∈ s, tieWeight η (c j) (c i) = ∑ i ∈ s, ∑ j ∈ s, tieWeight η (c i) (c j) :=
    Finset.sum_comm
  have hpt : ∀ i ∈ s, ∑ j ∈ s, tieWeight η (c j) (c i) + ∑ j ∈ s, tieWeight η (c i) (c j)
      = (s.card : ℝ) + (1 - 2 * η) * ∑ j ∈ s, (if c j = c i then (1 : ℝ) else 0) := by
    intro i _
    rw [← sum_add_distrib]
    have hj : ∀ j ∈ s, tieWeight η (c j) (c i) + tieWeight η (c i) (c j)
        = 1 + (1 - 2 * η) * (if c j = c i then (1 : ℝ) else 0) :=
      fun j _ => tieWeight_add_swap η (c j) (c i)
    rw [sum_congr rfl hj, sum_add_distrib, sum_const, nsmul_eq_mul, mul_one, ← mul_sum]
  have h2 : ∑ i ∈ s, ∑ j ∈ s, tieWeight η (c j) (c i) + ∑ i ∈ s, ∑ j ∈ s, tieWeight η (c i) (c j)
      = (s.card : ℝ) ^ 2 + (1 - 2 * η) * ties s c := by
    rw [← sum_add_distrib, sum_congr rfl hpt, sum_add_distrib, sum_const, nsmul_eq_mul, ← mul_sum]
    unfold ties
    ring
  linarith

theorem ties_pos {s : Finset ι} (hs : s.Nonempty) (c : ι → ℝ) : 0 < ties s c := by
  unfold ties
  have hrow : ∀ i ∈ s, 1 ≤ ∑ j ∈ s, (if c j = c i then (1 : ℝ) else 0) := by
    intro i hi
    have h := single_le_sum (f := fun j => if c j = c i then (1 : ℝ) else 0)
      (fun j _ => by dsimp only; split_ifs <;> norm_num) hi
    simpa using h
  exact sum_pos (fun i hi => lt_of_lt_of_le one_pos (hrow i hi)) hs

theorem total_status_half_iff {s : Finset ι} (hs : s.Nonempty) (c : ι → ℝ) (η : ℝ) :
    ∑ i ∈ s, ∑ j ∈ s, tieWeight η (c j) (c i) = (s.card : ℝ) ^ 2 / 2 ↔ η = 1 / 2 := by
  rw [total_status]
  have ht := ties_pos hs c
  constructor
  · intro h
    have : (1 / 2 - η) * ties s c = 0 := by linarith
    rcases mul_eq_zero.mp this with h0 | h0
    · linarith
    · linarith
  · intro h
    rw [h]
    ring

theorem control_total_status_needs_half :
    ∑ i ∈ ({0} : Finset ℕ), ∑ j ∈ ({0} : Finset ℕ), tieWeight 0 ((fun _ : ℕ => (1 : ℝ)) j) ((fun _ : ℕ => (1 : ℝ)) i)
      ≠ (({0} : Finset ℕ).card : ℝ) ^ 2 / 2 := by
  simp [tieWeight]

end Status

section Inefficiency

variable {ι : Type*}

theorem banning_gambles_helps {u : ℝ × ℝ → ℝ} {S : Set (ℝ × ℝ)} (hu : ConcaveOn ℝ S u)
    {t : Finset ι} {pr : ι → ℝ} {x : ι → ℝ × ℝ} (hpr : ∀ i ∈ t, 0 ≤ pr i) (h1 : ∑ i ∈ t, pr i = 1)
    (hS : ∀ i ∈ t, x i ∈ S) :
    ∑ i ∈ t, pr i * u (x i) ≤ u (∑ i ∈ t, pr i * (x i).1, ∑ i ∈ t, pr i * (x i).2) := by
  have key := hu.le_map_sum hpr h1 hS
  simp only [smul_eq_mul] at key
  have e : ∑ i ∈ t, pr i • x i = (∑ i ∈ t, pr i * (x i).1, ∑ i ∈ t, pr i * (x i).2) := by
    ext
    · simp [Prod.fst_sum]
    · simp [Prod.snd_sum]
  rwa [e] at key

theorem mean_below_median_step {u : ℝ → ℝ → ℝ} (hs : ∀ c, StrictMono (u c)) {b Fb Eu : ℝ}
    (haff : u b Fb = Eu) (hjensen : Eu < u b (1 / 2)) : Fb < 1 / 2 := by
  have h : u b Fb < u b (1 / 2) := haff ▸ hjensen
  exact (hs b).lt_iff_lt.mp h

theorem efficiency_fails_at_bottom {uc us ucc ucs F' α : ℝ} (hus : 0 < us) (hα : α = uc)
    (hslope : uc + us * F' = α) (hucc : ucc < 0) : ucc + ucs * F' < 0 := by
  have hF : F' = 0 := by
    have : us * F' = 0 := by linarith
    rcases mul_eq_zero.mp this with h | h
    · linarith
    · exact h
  rw [hF]
  linarith

theorem control_efficiency_needs_strict_concavity {uc us ucs F' α : ℝ} (hus : 0 < us) (hα : α = uc)
    (hslope : uc + us * F' = α) : ¬ ((0 : ℝ) + ucs * F' < 0) := by
  have : us * F' = 0 := by linarith
  have hF : F' = 0 := by
    rcases mul_eq_zero.mp this with h | h
    · linarith
    · exact h
  rw [hF]
  simp

end Inefficiency

section PureStatus

theorem savings_rule {δ x : ℝ} (hδ0 : 0 < δ) (hδ1 : δ < 1) (hx0 : 0 < x) (hx1 : x < 1) :
    1 / x = δ / (1 - δ) * (1 / (1 - x)) ↔ x = 1 - δ := by
  have h1 : (1 : ℝ) - δ ≠ 0 := by linarith
  have h2 : (1 : ℝ) - x ≠ 0 := by linarith
  constructor
  · intro h
    field_simp at h
    nlinarith
  · intro h
    subst h
    have h3 : (1 : ℝ) - (1 - δ) ≠ 0 := by linarith
    field_simp

theorem control_savings_rule_needs_discounting :
    ¬ (1 / (1 / 2 : ℝ) = 0 / (1 - 0) * (1 / (1 - 1 / 2))) := by
  norm_num

theorem uniform_example_mean {b : ℝ} (hb : 0 < b) :
    ∫ c in (0 : ℝ)..(2 * b), c * (1 / (2 * b)) = b := by
  rw [intervalIntegral.integral_mul_const, integral_id]
  field_simp
  ring

end PureStatus

end RayRobson2012
