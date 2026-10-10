import Mathlib.Analysis.SpecialFunctions.Pow.Real
import Mathlib.Analysis.SpecialFunctions.Log.Basic
import Mathlib.Analysis.SpecificLimits.Basic
import Mathlib.Algebra.BigOperators.Field
import Mathlib.Tactic

open Real Filter Topology Finset

namespace Borjas1992

noncomputable def childCapital (β₀ β₁ β₂ s k kbar : ℝ) : ℝ := β₀ * (s * k) ^ β₁ * kbar ^ β₂

theorem childCapital_strictMono_ethnic {β₀ β₁ β₂ s k kb₁ kb₂ : ℝ} (hβ₀ : 0 < β₀) (hβ₂ : 0 < β₂)
    (hs : 0 < s) (hk : 0 < k) (hkb₁ : 0 < kb₁) (hkb : kb₁ < kb₂) :
    childCapital β₀ β₁ β₂ s k kb₁ < childCapital β₀ β₁ β₂ s k kb₂ := by
  unfold childCapital
  have hA : 0 < β₀ * (s * k) ^ β₁ := mul_pos hβ₀ (Real.rpow_pos_of_pos (mul_pos hs hk) _)
  exact mul_lt_mul_of_pos_left (Real.rpow_lt_rpow hkb₁.le hkb hβ₂) hA

theorem childCapital_complementarity {β₀ β₁ β₂ k s₁ s₂ kb₁ kb₂ : ℝ} (hβ₀ : 0 < β₀)
    (hβ₁ : 0 < β₁) (hβ₂ : 0 < β₂) (hk : 0 < k) (hs₁ : 0 < s₁) (hs : s₁ < s₂) (hkb₁ : 0 < kb₁)
    (hkb : kb₁ < kb₂) :
    childCapital β₀ β₁ β₂ s₂ k kb₁ - childCapital β₀ β₁ β₂ s₁ k kb₁
      < childCapital β₀ β₁ β₂ s₂ k kb₂ - childCapital β₀ β₁ β₂ s₁ k kb₂ := by
  unfold childCapital
  have ha : (s₁ * k) ^ β₁ < (s₂ * k) ^ β₁ :=
    Real.rpow_lt_rpow (mul_pos hs₁ hk).le (mul_lt_mul_of_pos_right hs hk) hβ₁
  have hb : kb₁ ^ β₂ < kb₂ ^ β₂ := Real.rpow_lt_rpow hkb₁.le hkb hβ₂
  have key : 0 < β₀ * ((s₂ * k) ^ β₁ - (s₁ * k) ^ β₁) * (kb₂ ^ β₂ - kb₁ ^ β₂) :=
    mul_pos (mul_pos hβ₀ (sub_pos.mpr ha)) (sub_pos.mpr hb)
  have e : (β₀ * (s₂ * k) ^ β₁ * kb₂ ^ β₂ - β₀ * (s₁ * k) ^ β₁ * kb₂ ^ β₂)
      - (β₀ * (s₂ * k) ^ β₁ * kb₁ ^ β₂ - β₀ * (s₁ * k) ^ β₁ * kb₁ ^ β₂)
      = β₀ * ((s₂ * k) ^ β₁ - (s₁ * k) ^ β₁) * (kb₂ ^ β₂ - kb₁ ^ β₂) := by ring
  linarith

theorem control_complementarity_needs_beta2_pos :
    ¬ (childCapital 1 1 (-1) 2 1 1 - childCapital 1 1 (-1) 1 1 1
      < childCapital 1 1 (-1) 2 1 2 - childCapital 1 1 (-1) 1 1 2) := by
  unfold childCapital
  norm_num [Real.rpow_neg_one]

def denom (ρ β₁ s : ℝ) : ℝ := (1 - s) * (1 - ρ * β₁) + s * (1 - ρ)

noncomputable def elasTimeParent (ρ β₁ s : ℝ) : ℝ := ρ * (β₁ - 1) * (1 - s) / denom ρ β₁ s

noncomputable def elasTimeEthnic (ρ β₁ β₂ s : ℝ) : ℝ := ρ * β₂ * (1 - s) / denom ρ β₁ s

noncomputable def elasChildParent (ρ β₁ s : ℝ) : ℝ := β₁ * (1 - ρ) / denom ρ β₁ s

noncomputable def elasChildEthnic (ρ β₁ β₂ s : ℝ) : ℝ := β₂ * (1 - ρ * s) / denom ρ β₁ s

noncomputable def eta (ρ β₁ β₂ s : ℝ) : ℝ := (β₁ * (1 - ρ) + β₂ * (1 - ρ * s)) / denom ρ β₁ s

theorem rho_beta_lt_one {ρ β₁ : ℝ} (hρ : ρ < 1) (hβ₁0 : 0 ≤ β₁) (hβ₁ : β₁ < 1) :
    ρ * β₁ < 1 := by
  rcases le_or_gt ρ 0 with h | h
  · nlinarith
  · nlinarith

theorem one_sub_rho_s_pos {ρ s : ℝ} (hρ : ρ < 1) (hs0 : 0 ≤ s) (hs1 : s ≤ 1) :
    0 < 1 - ρ * s := by
  rcases le_or_gt ρ 0 with h | h
  · nlinarith
  · nlinarith

theorem denom_pos {ρ β₁ s : ℝ} (hρ : ρ < 1) (hβ₁0 : 0 ≤ β₁) (hβ₁ : β₁ < 1) (hs0 : 0 ≤ s)
    (hs1 : s ≤ 1) : 0 < denom ρ β₁ s := by
  unfold denom
  have hA : 0 < 1 - ρ * β₁ := by linarith [rho_beta_lt_one hρ hβ₁0 hβ₁]
  have hB : 0 < 1 - ρ := by linarith
  rcases eq_or_lt_of_le hs1 with h | h
  · rw [h]
    linarith
  · have h1 : 0 < (1 - s) * (1 - ρ * β₁) := mul_pos (by linarith) hA
    have h2 : 0 ≤ s * (1 - ρ) := mul_nonneg hs0 hB.le
    linarith

theorem foc_time_parent {ρ β₁ s sh dl : ℝ} (hD : denom ρ β₁ s ≠ 0)
    (hdl : dl * (1 - s) = -(s * sh))
    (hfoc : ρ * (β₁ * sh + β₁) + dl = ρ * (dl + 1) + sh) :
    sh = elasTimeParent ρ β₁ s := by
  unfold elasTimeParent
  rw [eq_div_iff hD]
  unfold denom
  linear_combination (-(1 - s)) * hfoc + (1 - ρ) * hdl

theorem foc_time_ethnic {ρ β₁ β₂ s sh dl : ℝ} (hD : denom ρ β₁ s ≠ 0)
    (hdl : dl * (1 - s) = -(s * sh))
    (hfoc : ρ * (β₁ * sh + β₂) + dl = ρ * dl + sh) :
    sh = elasTimeEthnic ρ β₁ β₂ s := by
  unfold elasTimeEthnic
  rw [eq_div_iff hD]
  unfold denom
  linear_combination (-(1 - s)) * hfoc + (1 - ρ) * hdl

theorem eq7a_from_eq5a {ρ β₁ s : ℝ} (hD : denom ρ β₁ s ≠ 0) :
    β₁ * (1 + elasTimeParent ρ β₁ s) = elasChildParent ρ β₁ s := by
  unfold elasTimeParent elasChildParent
  rw [eq_div_iff hD, mul_assoc, add_mul, one_mul, div_mul_cancel₀ _ hD]
  unfold denom
  ring

theorem eq7b_from_eq5b {ρ β₁ β₂ s : ℝ} (hD : denom ρ β₁ s ≠ 0) :
    β₁ * elasTimeEthnic ρ β₁ β₂ s + β₂ = elasChildEthnic ρ β₁ β₂ s := by
  unfold elasTimeEthnic elasChildEthnic
  field_simp
  unfold denom
  ring

theorem eq8_eq_sum (ρ β₁ β₂ s : ℝ) :
    eta ρ β₁ β₂ s = elasChildParent ρ β₁ s + elasChildEthnic ρ β₁ β₂ s := by
  unfold eta elasChildParent elasChildEthnic
  rw [add_div]

theorem elasTimeParent_neg_of_rho_pos {ρ β₁ s : ℝ} (hρ0 : 0 < ρ) (hρ : ρ < 1) (hβ₁0 : 0 ≤ β₁)
    (hβ₁ : β₁ < 1) (hs0 : 0 ≤ s) (hs1 : s < 1) : elasTimeParent ρ β₁ s < 0 := by
  unfold elasTimeParent
  apply div_neg_of_neg_of_pos _ (denom_pos hρ hβ₁0 hβ₁ hs0 hs1.le)
  exact mul_neg_of_neg_of_pos (mul_neg_of_pos_of_neg hρ0 (by linarith)) (by linarith)

theorem elasTimeParent_pos_of_rho_neg {ρ β₁ s : ℝ} (hρ0 : ρ < 0) (hβ₁0 : 0 ≤ β₁)
    (hβ₁ : β₁ < 1) (hs0 : 0 ≤ s) (hs1 : s < 1) : 0 < elasTimeParent ρ β₁ s := by
  unfold elasTimeParent
  apply div_pos _ (denom_pos (by linarith) hβ₁0 hβ₁ hs0 hs1.le)
  exact mul_pos (mul_pos_of_neg_of_neg hρ0 (by linarith)) (by linarith)

theorem elasTimeParent_zero_of_cobbDouglas (β₁ s : ℝ) : elasTimeParent 0 β₁ s = 0 := by
  simp [elasTimeParent]

theorem elasTimeEthnic_pos_of_rho_pos {ρ β₁ β₂ s : ℝ} (hρ0 : 0 < ρ) (hρ : ρ < 1)
    (hβ₁0 : 0 ≤ β₁) (hβ₁ : β₁ < 1) (hβ₂ : 0 < β₂) (hs0 : 0 ≤ s) (hs1 : s < 1) :
    0 < elasTimeEthnic ρ β₁ β₂ s := by
  unfold elasTimeEthnic
  exact div_pos (mul_pos (mul_pos hρ0 hβ₂) (by linarith)) (denom_pos hρ hβ₁0 hβ₁ hs0 hs1.le)

theorem elasChild_pos {ρ β₁ β₂ s : ℝ} (hρ : ρ < 1) (hβ₁0 : 0 < β₁) (hβ₁ : β₁ < 1)
    (hβ₂ : 0 < β₂) (hs0 : 0 ≤ s) (hs1 : s ≤ 1) :
    0 < elasChildParent ρ β₁ s ∧ 0 < elasChildEthnic ρ β₁ β₂ s := by
  have hD := denom_pos hρ hβ₁0.le hβ₁ hs0 hs1
  exact ⟨div_pos (mul_pos hβ₁0 (by linarith)) hD,
    div_pos (mul_pos hβ₂ (one_sub_rho_s_pos hρ hs0 hs1)) hD⟩

theorem eta_sub_one {ρ β₁ β₂ s : ℝ} (hD : denom ρ β₁ s ≠ 0) :
    eta ρ β₁ β₂ s - 1 = (β₁ + β₂ - 1) * (1 - ρ * s) / denom ρ β₁ s := by
  unfold eta
  field_simp
  unfold denom
  ring

theorem eta_lt_one_iff {ρ β₁ β₂ s : ℝ} (hρ : ρ < 1) (hβ₁0 : 0 ≤ β₁) (hβ₁ : β₁ < 1)
    (hs0 : 0 ≤ s) (hs1 : s ≤ 1) : eta ρ β₁ β₂ s < 1 ↔ β₁ + β₂ < 1 := by
  have hD := denom_pos hρ hβ₁0 hβ₁ hs0 hs1
  have hp := one_sub_rho_s_pos hρ hs0 hs1
  have he := eta_sub_one (β₂ := β₂) hD.ne'
  constructor
  · intro h
    by_contra hc
    push_neg at hc
    have : 0 ≤ (β₁ + β₂ - 1) * (1 - ρ * s) / denom ρ β₁ s :=
      div_nonneg (mul_nonneg (by linarith) hp.le) hD.le
    linarith
  · intro h
    have : (β₁ + β₂ - 1) * (1 - ρ * s) / denom ρ β₁ s < 0 :=
      div_neg_of_neg_of_pos (mul_neg_of_neg_of_pos (by linarith) hp) hD
    linarith

theorem one_lt_eta_iff {ρ β₁ β₂ s : ℝ} (hρ : ρ < 1) (hβ₁0 : 0 ≤ β₁) (hβ₁ : β₁ < 1)
    (hs0 : 0 ≤ s) (hs1 : s ≤ 1) : 1 < eta ρ β₁ β₂ s ↔ 1 < β₁ + β₂ := by
  have hD := denom_pos hρ hβ₁0 hβ₁ hs0 hs1
  have hp := one_sub_rho_s_pos hρ hs0 hs1
  have he := eta_sub_one (β₂ := β₂) hD.ne'
  constructor
  · intro h
    by_contra hc
    push_neg at hc
    have : (β₁ + β₂ - 1) * (1 - ρ * s) / denom ρ β₁ s ≤ 0 :=
      div_nonpos_of_nonpos_of_nonneg (mul_nonpos_of_nonpos_of_nonneg (by linarith) hp.le) hD.le
    linarith
  · intro h
    have : 0 < (β₁ + β₂ - 1) * (1 - ρ * s) / denom ρ β₁ s :=
      div_pos (mul_pos (by linarith) hp) hD
    linarith

theorem eta_eq_one_iff {ρ β₁ β₂ s : ℝ} (hρ : ρ < 1) (hβ₁0 : 0 ≤ β₁) (hβ₁ : β₁ < 1)
    (hs0 : 0 ≤ s) (hs1 : s ≤ 1) : eta ρ β₁ β₂ s = 1 ↔ β₁ + β₂ = 1 := by
  have hD := denom_pos hρ hβ₁0 hβ₁ hs0 hs1
  have hp := one_sub_rho_s_pos hρ hs0 hs1
  have he := eta_sub_one (β₂ := β₂) hD.ne'
  constructor
  · intro h
    have h0 : (β₁ + β₂ - 1) * (1 - ρ * s) / denom ρ β₁ s = 0 := by rw [← he, h]; ring
    rcases div_eq_zero_iff.mp h0 with h1 | h1
    · rcases mul_eq_zero.mp h1 with h2 | h2
      · linarith
      · linarith
    · linarith
  · intro h
    have h3 : β₁ + β₂ - 1 = 0 := by linarith
    have : eta ρ β₁ β₂ s - 1 = 0 := by rw [he, h3]; ring
    linarith

theorem control_eta_needs_rho_lt_one :
    1 < eta 2 (1 / 2) 0 (2 / 5) ∧ (1 / 2 : ℝ) + 0 < 1 := by
  unfold eta denom
  norm_num

theorem control_eta_needs_beta1_lt_one :
    eta (1 / 2) 4 0 0 < 1 ∧ 1 < (4 : ℝ) + 0 := by
  unfold eta denom
  norm_num

theorem log_childCapital_common {β₀ β₁ β₂ s k : ℝ} (hβ₀ : 0 < β₀) (hs : 0 < s) (hk : 0 < k) :
    Real.log (childCapital β₀ β₁ β₂ s k k) = Real.log β₀ + β₁ * Real.log s + (β₁ + β₂) * Real.log k := by
  unfold childCapital
  have hsk : 0 < s * k := mul_pos hs hk
  have h1 : 0 < (s * k) ^ β₁ := Real.rpow_pos_of_pos hsk _
  have h2 : 0 < k ^ β₂ := Real.rpow_pos_of_pos hk _
  rw [Real.log_mul (mul_pos hβ₀ h1).ne' h2.ne', Real.log_mul hβ₀.ne' h1.ne', Real.log_rpow hsk,
    Real.log_rpow hk, Real.log_mul hs.ne' hk.ne']
  ring

theorem group_gap_step {β₀ β₁ β₂ s kA kB : ℝ} (hβ₀ : 0 < β₀) (hs : 0 < s) (hA : 0 < kA)
    (hB : 0 < kB) :
    Real.log (childCapital β₀ β₁ β₂ s kA kA) - Real.log (childCapital β₀ β₁ β₂ s kB kB)
      = (β₁ + β₂) * (Real.log kA - Real.log kB) := by
  rw [log_childCapital_common hβ₀ hs hA, log_childCapital_common hβ₀ hs hB]
  ring

theorem gap_closed_form {g : ℕ → ℝ} {b : ℝ} (h : ∀ t, g (t + 1) = b * g t) (n : ℕ) :
    g n = b ^ n * g 0 := by
  induction n with
  | zero => simp
  | succ n ih => rw [h, ih, pow_succ]; ring

theorem gap_tendsto_zero {g : ℕ → ℝ} {b : ℝ} (h : ∀ t, g (t + 1) = b * g t) (hb0 : 0 ≤ b)
    (hb1 : b < 1) : Tendsto g atTop (𝓝 0) := by
  have hg : g = fun n => b ^ n * g 0 := funext (gap_closed_form h)
  rw [hg]
  simpa using (tendsto_pow_atTop_nhds_zero_of_lt_one hb0 hb1).mul_const (g 0)

theorem gap_persists {g : ℕ → ℝ} (h : ∀ t, g (t + 1) = 1 * g t) (n : ℕ) : g n = g 0 := by
  rw [gap_closed_form h n, one_pow, one_mul]

theorem gap_larger_with_externality {b₁ b₂ g0 : ℝ} (hb : 0 ≤ b₁) (h12 : b₁ < b₂) (hg : 0 < g0)
    {n : ℕ} (hn : n ≠ 0) : b₁ ^ n * g0 < b₂ ^ n * g0 :=
  mul_lt_mul_of_pos_right (pow_lt_pow_left₀ h12 hb hn) hg

theorem control_gap_needs_lt_one : ¬ Tendsto (fun n : ℕ => (1 : ℝ) ^ n * 1) atTop (𝓝 0) := by
  simp only [one_pow, one_mul]
  intro h
  exact one_ne_zero (tendsto_nhds_unique tendsto_const_nhds h)

section Bias

variable {ι J : Type*} [DecidableEq J]

noncomputable def groupMean (s : Finset ι) (g : ι → J) (x : ι → ℝ) (j : J) : ℝ :=
  (∑ i ∈ s.filter (fun i => g i = j), x i) / ((s.filter (fun i => g i = j)).card : ℝ)

noncomputable def mean (s : Finset ι) (x : ι → ℝ) : ℝ := (∑ i ∈ s, x i) / (s.card : ℝ)

theorem sum_within_deviation_zero (s : Finset ι) (g : ι → J) (x : ι → ℝ) (c : J → ℝ) :
    ∑ i ∈ s, c (g i) * (x i - groupMean s g x (g i)) = 0 := by
  rw [← Finset.sum_fiberwise_of_maps_to (t := s.image g) (g := g)
    (fun i hi => Finset.mem_image_of_mem g hi)]
  apply Finset.sum_eq_zero
  intro j hj
  have hne : (s.filter (fun i => g i = j)).Nonempty := by
    obtain ⟨i, hi, rfl⟩ := Finset.mem_image.mp hj
    exact ⟨i, Finset.mem_filter.mpr ⟨hi, rfl⟩⟩
  have hcard : ((s.filter (fun i => g i = j)).card : ℝ) ≠ 0 := by
    exact_mod_cast (Finset.card_pos.mpr hne).ne'
  have hfill : ((s.filter (fun i => g i = j)).card : ℝ) * groupMean s g x j
      = ∑ i ∈ s.filter (fun i => g i = j), x i := by
    unfold groupMean
    field_simp
  calc ∑ i ∈ s.filter (fun i => g i = j), c (g i) * (x i - groupMean s g x (g i))
      = ∑ i ∈ s.filter (fun i => g i = j), c j * (x i - groupMean s g x j) := by
        apply Finset.sum_congr rfl
        intro i hi
        rw [(Finset.mem_filter.mp hi).2]
    _ = c j * ((∑ i ∈ s.filter (fun i => g i = j), x i)
          - ((s.filter (fun i => g i = j)).card : ℝ) * groupMean s g x j) := by
        rw [← Finset.mul_sum, Finset.sum_sub_distrib, Finset.sum_const, nsmul_eq_mul]
    _ = 0 := by rw [hfill]; ring

theorem sum_dev_mean_zero {s : Finset ι} (hs : s.Nonempty) (x : ι → ℝ) :
    ∑ i ∈ s, (x i - mean s x) = 0 := by
  have hc : (s.card : ℝ) ≠ 0 := by exact_mod_cast (Finset.card_pos.mpr hs).ne'
  unfold mean
  rw [Finset.sum_sub_distrib, Finset.sum_const, nsmul_eq_mul]
  field_simp

theorem cov_groupMean_eq_between (s : Finset ι) (g : ι → J) (x : ι → ℝ) (m : ℝ) :
    ∑ i ∈ s, (x i - m) * (groupMean s g x (g i) - m)
      = ∑ i ∈ s, (groupMean s g x (g i) - m) ^ 2 := by
  have h := sum_within_deviation_zero s g x (fun j => groupMean s g x j - m)
  have e : ∑ i ∈ s, (x i - m) * (groupMean s g x (g i) - m)
      - ∑ i ∈ s, (groupMean s g x (g i) - m) ^ 2
      = ∑ i ∈ s, (groupMean s g x (g i) - m) * (x i - groupMean s g x (g i)) := by
    rw [← Finset.sum_sub_distrib]
    exact Finset.sum_congr rfl (fun i _ => by ring)
  linarith

theorem total_eq_within_add_between (s : Finset ι) (g : ι → J) (x : ι → ℝ) (m : ℝ) :
    ∑ i ∈ s, (x i - m) ^ 2
      = ∑ i ∈ s, (x i - groupMean s g x (g i)) ^ 2 + ∑ i ∈ s, (groupMean s g x (g i) - m) ^ 2 := by
  have h := sum_within_deviation_zero s g x (fun j => groupMean s g x j - m)
  have e : ∑ i ∈ s, (x i - m) ^ 2
      - (∑ i ∈ s, (x i - groupMean s g x (g i)) ^ 2 + ∑ i ∈ s, (groupMean s g x (g i) - m) ^ 2)
      = 2 * ∑ i ∈ s, (groupMean s g x (g i) - m) * (x i - groupMean s g x (g i)) := by
    rw [← Finset.sum_add_distrib, ← Finset.sum_sub_distrib, Finset.mul_sum]
    exact Finset.sum_congr rfl (fun i _ => by ring)
  linarith

theorem ols_slope_omitting_ethnic_capital {s : Finset ι} (hs : s.Nonempty) (g : ι → J)
    (x y ζ : ι → ℝ) (γ₀ γ₁ γ₂ : ℝ)
    (hy : ∀ i ∈ s, y i = γ₀ + γ₁ * x i + γ₂ * groupMean s g x (g i) + ζ i)
    (hζ : ∑ i ∈ s, (x i - mean s x) * ζ i = 0)
    (hT : ∑ i ∈ s, (x i - mean s x) ^ 2 ≠ 0) :
    (∑ i ∈ s, (x i - mean s x) * (y i - mean s y)) / ∑ i ∈ s, (x i - mean s x) ^ 2
      = γ₁ + (1 - (∑ i ∈ s, (x i - groupMean s g x (g i)) ^ 2)
          / ∑ i ∈ s, (x i - mean s x) ^ 2) * γ₂ := by
  set m := mean s x
  set T := ∑ i ∈ s, (x i - m) ^ 2
  set W := ∑ i ∈ s, (x i - groupMean s g x (g i)) ^ 2
  set B := ∑ i ∈ s, (groupMean s g x (g i) - m) ^ 2
  have h0 : ∑ i ∈ s, (x i - m) = 0 := sum_dev_mean_zero hs x
  have hTWB : T = W + B := total_eq_within_add_between s g x m
  have hcov := cov_groupMean_eq_between s g x m
  have hN : ∑ i ∈ s, (x i - m) * (y i - mean s y) = γ₁ * T + γ₂ * B := by
    have e1 : ∀ i ∈ s, (x i - m) * (y i - mean s y)
        = γ₁ * (x i - m) ^ 2 + γ₂ * ((x i - m) * (groupMean s g x (g i) - m))
          + (x i - m) * ζ i + (x i - m) * (γ₀ + γ₁ * m + γ₂ * m - mean s y) := by
      intro i hi
      rw [hy i hi]
      ring
    rw [Finset.sum_congr rfl e1, Finset.sum_add_distrib, Finset.sum_add_distrib,
      Finset.sum_add_distrib, ← Finset.mul_sum, ← Finset.mul_sum, ← Finset.sum_mul, h0, hζ, hcov]
    ring
  rw [hN]
  have hB : B = T - W := by linarith
  rw [hB]
  field_simp
  ring

theorem ols_understates_transmission {γ₁ γ₂ w : ℝ} (hw : 0 < w) (hγ₂ : 0 < γ₂) :
    γ₁ + (1 - w) * γ₂ < γ₁ + γ₂ := by
  nlinarith [mul_pos hw hγ₂]

theorem control_understates_needs_gamma2_pos :
    ¬ ((0 : ℝ) + (1 - 1 / 2) * (-1) < 0 + (-1)) := by
  norm_num

end Bias

noncomputable def reliability (sx s1 : ℝ) : ℝ := sx / (sx + s1)

noncomputable def withinShare (s2 sx : ℝ) : ℝ := s2 / sx

noncomputable def plimParental (w h δ : ℝ) : ℝ := w * h / (1 - h * (1 - w)) * δ

noncomputable def plimEthnic (w h δ : ℝ) : ℝ := (1 - h) / (1 - h * (1 - w)) * δ

theorem normal_equations_solution {sx s1 s2 δ θ₁ θ₂ : ℝ} (hx : 0 < sx) (h1 : 0 ≤ s1)
    (h2 : 0 < s2) (h2x : s2 < sx)
    (hn1 : θ₁ * (sx + s1) + θ₂ * (sx - s2) = δ * sx)
    (hn2 : θ₁ * (sx - s2) + θ₂ * (sx - s2) = δ * (sx - s2)) :
    θ₁ = plimParental (withinShare s2 sx) (reliability sx s1) δ
      ∧ θ₂ = plimEthnic (withinShare s2 sx) (reliability sx s1) δ := by
  have hne : sx - s2 ≠ 0 := by linarith
  have hsum : θ₁ + θ₂ = δ := by
    apply mul_right_cancel₀ hne
    linear_combination hn2
  have hθ₁ : θ₁ * (s1 + s2) = δ * s2 := by linear_combination hn1 - hn2
  have hs12 : s1 + s2 ≠ 0 := by linarith
  have hxs : sx + s1 ≠ 0 := by linarith
  have hθ₁' : θ₁ = δ * s2 / (s1 + s2) := by rw [eq_div_iff hs12]; exact hθ₁
  have hθ₂' : θ₂ = δ * s1 / (s1 + s2) := by
    have : θ₂ = δ - θ₁ := by linarith
    rw [this, hθ₁']
    field_simp
    ring
  have hden : 1 - sx / (sx + s1) * (1 - s2 / sx) = (s1 + s2) / (sx + s1) := by
    field_simp
    ring
  unfold plimParental plimEthnic withinShare reliability
  rw [hden]
  constructor
  · rw [hθ₁']
    field_simp
    ring
  · rw [hθ₂']
    field_simp
    ring

theorem plim_sum_eq_delta {w h δ : ℝ} (hd : 1 - h * (1 - w) ≠ 0) :
    plimParental w h δ + plimEthnic w h δ = δ := by
  unfold plimParental plimEthnic
  field_simp
  ring

theorem plimEthnic_zero_of_exact (w δ : ℝ) : plimEthnic w 1 δ = 0 := by
  simp [plimEthnic]

theorem plimEthnic_strictAnti_reliability {w h₁ h₂ δ : ℝ} (hw : 0 < w)
    (hδ : 0 < δ) (h₁0 : 0 ≤ h₁) (h12 : h₁ < h₂) (h₂1 : h₂ ≤ 1) :
    plimEthnic w h₂ δ < plimEthnic w h₁ δ := by
  unfold plimEthnic
  have hd₁ : 0 < 1 - h₁ * (1 - w) := by nlinarith
  have hd₂ : 0 < 1 - h₂ * (1 - w) := by nlinarith
  apply mul_lt_mul_of_pos_right _ hδ
  rw [div_lt_div_iff₀ hd₂ hd₁]
  nlinarith [mul_pos hw (sub_pos.mpr h12)]

theorem control_sum_needs_pi_lt_one :
    ∃ θ₁ θ₂ : ℝ, θ₁ * (1 + 0) + θ₂ * (1 - 1) = 1 * 1 ∧ θ₁ * (1 - 1) + θ₂ * (1 - 1) = 1 * (1 - 1)
      ∧ θ₁ + θ₂ ≠ 1 :=
  ⟨1, 5, by norm_num⟩

theorem reliability_of_noise_ratio {sx : ℝ} (hx : 0 < sx) :
    reliability sx (33 / 100 * sx) = 100 / 133 := by
  unfold reliability
  field_simp
  ring

theorem example_p142 :
    plimParental (9 / 10) (100 / 133) (2 / 5) = 12 / 41
      ∧ plimEthnic (9 / 10) (100 / 133) (2 / 5) = 22 / 205 := by
  unfold plimParental plimEthnic
  norm_num

theorem example_p142_rounds :
    |(12 / 41 : ℝ) - 0.29| < 0.005 ∧ |(22 / 205 : ℝ) - 0.11| < 0.005 := by
  norm_num [abs_lt]

theorem table3_sums_match_text :
    |((0.2501 : ℝ) + 0.2265) - 0.48| < 0.005 ∧ |((0.2570 : ℝ) + 0.1165) - 0.37| < 0.005
      ∧ (0.3257 : ℝ) + 0.2843 = 0.61 := by
  norm_num [abs_lt]

theorem table3_gss_occupation_reading :
    (0.1829 : ℝ) + 0.4589 = 0.6418 ∧ 0.005 < |(0.6418 : ℝ) - 0.63| := by
  norm_num [abs_lt, lt_abs]

theorem table4_sums_against_text :
    |((0.2496 : ℝ) + 0.2722) - 0.52| < 0.005 ∧ |((0.1865 : ℝ) + 0.4325) - 0.62| < 0.005
      ∧ 0.005 < |((0.3015 : ℝ) + 0.1896) - 0.50| ∧ 0.005 < |((0.3372 : ℝ) + 0.1980) - 0.53| := by
  norm_num [abs_lt, lt_abs]

end Borjas1992
