import Mathlib.Analysis.SpecialFunctions.Pow.Deriv
import Mathlib.Analysis.SpecialFunctions.Integrals.Basic
import Mathlib.Tactic

open Real Finset

namespace BeckerMurphyWerning2005

section Lottery

theorem complements_winner_consumes_more {f : ℝ → ℝ → ℝ}
    (hc : ∀ s, StrictAnti (fun c => f c s)) (hs : ∀ c, StrictMono (fun s => f c s))
    {ch cl sh sl : ℝ} (hss : sl < sh) (heq : f ch sh = f cl sl) : cl < ch := by
  by_contra h
  push_neg at h
  have h1 : f cl sh ≤ f ch sh := (hc sh).antitone h
  have h2 : f cl sl < f cl sh := hs cl hss
  linarith

theorem separable_equal_consumption {g : ℝ → ℝ} (hg : StrictAnti g) {ch cl : ℝ}
    (heq : g ch = g cl) : ch = cl :=
  hg.injective heq

theorem substitutes_winner_consumes_less {f : ℝ → ℝ → ℝ}
    (hc : ∀ s, StrictAnti (fun c => f c s)) (hs : ∀ c, StrictAnti (fun s => f c s))
    {ch cl sh sl : ℝ} (hss : sl < sh) (heq : f ch sh = f cl sl) : ch < cl := by
  by_contra h
  push_neg at h
  have h1 : f ch sh ≤ f cl sh := (hc sh).antitone h
  have h2 : f cl sh < f cl sl := hs cl hss
  linarith

theorem control_more_consumption_needs_complementarity {g : ℝ → ℝ} (hg : StrictAnti g)
    {ch cl : ℝ} (heq : (fun c (_ : ℝ) => g c) ch 1 = (fun c (_ : ℝ) => g c) cl 0) : ¬ cl < ch := by
  have := separable_equal_consumption hg heq
  rw [this]
  exact lt_irrefl cl

theorem convex_kink {f : ℝ → ℝ → ℝ} (hc : ∀ s, StrictAnti (fun c => f c s))
    (hs : ∀ c, StrictMono (fun s => f c s)) {ybar P0 P1 : ℝ} (hP : P0 < P1) :
    f (ybar - P0) 0 < f (ybar - P1) 1 := by
  have h1 : f (ybar - P0) 0 < f (ybar - P0) 1 := hs _ (by norm_num)
  have h2 : f (ybar - P0) 1 < f (ybar - P1) 1 := (hc 1) (by linarith)
  linarith

theorem control_kink_needs_rising_price_or_complementarity (g : ℝ → ℝ) (ybar P : ℝ) :
    ¬ (fun c (_ : ℝ) => g c) (ybar - P) 0 < (fun c (_ : ℝ) => g c) (ybar - P) 1 := by
  simp

theorem linear_value_fair_lottery {ι : Type*} {t : Finset ι} {pr e : ι → ℝ} (lam phi y : ℝ)
    (h1 : ∑ i ∈ t, pr i = 1) (hfair : ∑ i ∈ t, pr i * e i = 0) :
    ∑ i ∈ t, pr i * (lam * (y + e i) + phi) = lam * y + phi := by
  have hexp : ∀ i ∈ t, pr i * (lam * (y + e i) + phi) = (lam * y + phi) * pr i + lam * (pr i * e i) := by
    intro i _
    ring
  rw [sum_congr rfl hexp, sum_add_distrib, ← mul_sum, ← mul_sum, h1, hfair]
  ring

end Lottery

section PowerLaw

noncomputable def delta (α β : ℝ) : ℝ := β / (1 - α)

noncomputable def cstar (α β ω s : ℝ) : ℝ := ω * (1 + delta α β) * s ^ delta α β

noncomputable def pstar (α β ω s : ℝ) : ℝ := β / (α * delta α β) * ω * (1 + delta α β) * s ^ delta α β

theorem cstar_marginal_utility_constant {α β ω s : ℝ} (hα1 : α < 1) (hω : 0 < ω) (hs : 0 < s)
    (hβ : 0 < β) :
    α * cstar α β ω s ^ (α - 1) * s ^ β = α * (ω * (1 + delta α β)) ^ (α - 1) := by
  have hd : 0 < delta α β := div_pos hβ (by linarith)
  have hk : 0 < ω * (1 + delta α β) := by positivity
  unfold cstar
  rw [Real.mul_rpow hk.le (Real.rpow_nonneg hs.le _), ← Real.rpow_mul hs.le, mul_assoc, mul_assoc,
    ← Real.rpow_add hs]
  have e : delta α β * (α - 1) + β = 0 := by
    unfold delta
    have h1a : (1 : ℝ) - α ≠ 0 := by linarith
    field_simp
    ring
  rw [e, Real.rpow_zero, mul_one]

theorem cstar_resource {α β ω : ℝ} (hα1 : α < 1) (hβ : 0 < β) :
    ∫ s in (0 : ℝ)..1, cstar α β ω s = ω := by
  have hd : 0 < delta α β := div_pos hβ (by linarith)
  unfold cstar
  rw [intervalIntegral.integral_const_mul, integral_rpow (Or.inl (by linarith))]
  have hne : delta α β + 1 ≠ 0 := by linarith
  rw [Real.one_rpow, Real.zero_rpow (by linarith), sub_zero, add_comm (delta α β) 1, mul_assoc,
    mul_one_div_cancel (by linarith : (1 : ℝ) + delta α β ≠ 0), mul_one]

theorem price_slope_ratio {α β c s : ℝ} (hα : α ≠ 0) (hc : 0 < c) (hs : 0 < s) :
    (β * c ^ α * s ^ (β - 1)) / (α * c ^ (α - 1) * s ^ β) = β / α * c / s := by
  rw [Real.rpow_sub_one hc.ne', Real.rpow_sub_one hs.ne']
  have h1 : 0 < c ^ α := Real.rpow_pos_of_pos hc _
  have h2 : 0 < s ^ β := Real.rpow_pos_of_pos hs _
  field_simp
  ring

theorem pstar_deriv {α β ω s : ℝ} (hα : α ≠ 0) (hα1 : α < 1) (hβ : 0 < β) (hs : 0 < s) :
    HasDerivAt (fun v => pstar α β ω v) (β / α * cstar α β ω s / s) s := by
  have hd : 0 < delta α β := div_pos hβ (by linarith)
  have h := (Real.hasDerivAt_rpow_const (p := delta α β) (Or.inl hs.ne')).const_mul
    (β / (α * delta α β) * ω * (1 + delta α β))
  unfold pstar
  convert h using 1
  unfold cstar
  rw [Real.rpow_sub_one hs.ne']
  field_simp
  ring

theorem ystar_closed {α β ω s : ℝ} (hα : α ≠ 0) (hα1 : α < 1) (hβ : 0 < β) :
    pstar α β ω s + cstar α β ω s = (1 + delta α β) * ω / α * s ^ delta α β := by
  have hd : 0 < delta α β := div_pos hβ (by linarith)
  unfold pstar cstar
  have e : β / (α * delta α β) = (1 - α) / α := by
    unfold delta
    have : (1 : ℝ) - α ≠ 0 := by linarith
    field_simp
    ring
  rw [e]
  field_simp
  ring

theorem kappa_eq {α β : ℝ} (hα1 : α < 1) : 1 + delta α β = (1 - α + β) / (1 - α) := by
  unfold delta
  have : (1 : ℝ) - α ≠ 0 := by linarith
  field_simp

noncomputable def Fstar (α β ω y : ℝ) : ℝ := (α * y / ((1 + delta α β) * ω)) ^ (1 / delta α β)

theorem Fstar_inverts_ystar {α β ω s : ℝ} (hα0 : 0 < α) (hα1 : α < 1) (hβ : 0 < β) (hω : 0 < ω)
    (hs : 0 ≤ s) :
    Fstar α β ω ((1 + delta α β) * ω / α * s ^ delta α β) = s := by
  have hd : 0 < delta α β := div_pos hβ (by linarith)
  have hk : 0 < (1 + delta α β) * ω := by positivity
  unfold Fstar
  have e : α * ((1 + delta α β) * ω / α * s ^ delta α β) / ((1 + delta α β) * ω) = s ^ delta α β := by
    field_simp
  rw [e, ← Real.rpow_mul hs, mul_one_div_cancel hd.ne', Real.rpow_one]

theorem ystar_top {α β ω : ℝ} : (1 + delta α β) * ω / α * (1 : ℝ) ^ delta α β = (1 + delta α β) * ω / α := by
  rw [Real.one_rpow, mul_one]

theorem control_printed_Fstar_misses_top :
    (1 / 6 : ℝ) ^ (2 : ℝ) * (6 : ℝ) ^ ((1 : ℝ) / 2) ≠ 1 := by
  rw [Real.rpow_two]
  intro h
  have h6 : (6 : ℝ) ^ ((1 : ℝ) / 2) = 36 := by nlinarith
  have hsq : ((6 : ℝ) ^ ((1 : ℝ) / 2)) ^ (2 : ℕ) = 6 := by
    rw [← Real.rpow_natCast, ← Real.rpow_mul (by norm_num)]
    norm_num
  rw [h6] at hsq
  norm_num at hsq

theorem control_density_tail_needs_large_delta :
    (1 : ℝ) / delta (1 / 2) (1 / 2) - 1 = 0 := by
  unfold delta
  norm_num

end PowerLaw

end BeckerMurphyWerning2005
