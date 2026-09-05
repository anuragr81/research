/-
# Proposition DIV (micro divergence), Section 4.1, with Appendix A.1

    Pᴶ_AB - Pᴮ = c · κ · R₁ + O(c²),   R₁ = q ⊗ (1,-1),  κ  = (α-q₀) r₀(1-r₀)/Z
    Pᴶ_BA - Pᴮ = c · κ'· R₂ + O(c²),   R₂ = (1,-1) ⊗ r,  κ' = (β-r₀) q₀(1-q₀)/Z

and the sequence effect `Pᴶ_AB - Pᴶ_BA = c(κR₁ - κ'R₂) + O(c²)`.

Every entry of all three posteriors is a ratio of two functions *affine* in `c`
(the closed forms below), so the `O(c²)` statement is exactly the assertion
that the derivative at `c = 0` is the stated matrix.  That is what is proved
here, as `HasDerivAt`: no asymptotic hand-waving is involved, and the appendix's
Step 2 ("each posterior is a rational function of `c` whose denominator is
nonvanishing near 0, so both differences are analytic there") is discharged by
the closed forms themselves.

Appendix Step 4's conclusion -- `κ = 0 ⟺ q₀ = α or r₀ ∈ {0,1}`, so the gap has
a nonzero leading coefficient whenever the `A`-impression genuinely surprises
and the `B`-impression is genuinely soft -- is `kappa_eq_zero_iff` below.
-/
import JeffreyOrder.PropIMM
import JeffreyOrder.PropPRO
import Mathlib.Analysis.Calculus.Deriv.Add
import Mathlib.Analysis.Calculus.Deriv.Mul
import Mathlib.Analysis.Calculus.Deriv.Inv
import Mathlib.Analysis.SpecialFunctions.Pow.Real

namespace JeffreyOrder
open Mat

variable {α β c q₀ r₀ : ℝ}

/-! ### The derivative of a ratio of affine functions -/

/-- Every entry of every posterior in the model is of this shape, so this single
lemma carries all the differentiation in Proposition DIV. -/
theorem hasDerivAt_affine_div (A B P Q : ℝ) (hP : P ≠ 0) :
    HasDerivAt (fun c : ℝ => (A + B * c) / (P + Q * c)) ((B * P - A * Q) / P ^ 2) 0 := by
  have hn : HasDerivAt (fun c : ℝ => A + B * c) B 0 := by
    have h : HasDerivAt (fun c : ℝ => B * c) B 0 := by
      simpa using (hasDerivAt_id (0 : ℝ)).const_mul B
    exact h.const_add A
  have hd : HasDerivAt (fun c : ℝ => P + Q * c) Q 0 := by
    have h : HasDerivAt (fun c : ℝ => Q * c) Q 0 := by
      simpa using (hasDerivAt_id (0 : ℝ)).const_mul Q
    exact h.const_add P
  have hne : P + Q * 0 ≠ 0 := by simpa using hP
  have h : HasDerivAt (fun c : ℝ => (A + B * c) / (P + Q * c))
      ((B * (P + Q * 0) - (A + B * 0) * Q) / (P + Q * 0) ^ 2) 0 := hn.div hd hne
  simp only [mul_zero, add_zero] at h
  exact h

/-- A denominator that is affine in `c` and nonzero at `c = 0` stays nonzero on
a neighbourhood of `0`, which is what lets the closed forms below be used to
compute a derivative at `0`. -/
theorem eventually_affine_ne (P Q : ℝ) (hP : P ≠ 0) :
    ∀ᶠ c : ℝ in nhds 0, P + Q * c ≠ 0 := by
  have hcont : ContinuousAt (fun c : ℝ => P + Q * c) 0 := by fun_prop
  have := hcont.eventually_ne (by simpa using hP)
  simpa using this

/-! ### Intermediate marginals, in closed form -/

/-- The `B`-marginal after the `A`-step: `Q(B=0) = [αβ(1-α) + c(q₀-α)] / [α(1-α)]`. -/
theorem jeffreyA_prior_mB0 (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) :
    (jeffreyA (prior α β c) q₀).mB0
      = (α * β * (1 - α) + c * (q₀ - α)) / (α * (1 - α)) := by
  rw [jeffreyA_prior hα hα']
  simp only [Mat.mB0]
  field_simp
  try ring

theorem jeffreyA_prior_mB1 (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) :
    (jeffreyA (prior α β c) q₀).mB1
      = (α * (1 - α) * (1 - β) - c * (q₀ - α)) / (α * (1 - α)) := by
  rw [jeffreyA_prior hα hα']
  simp only [Mat.mB1]
  field_simp
  try ring

theorem jeffreyB_prior_mA0 (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    (jeffreyB (prior α β c) r₀).mA0
      = (α * β * (1 - β) + c * (r₀ - β)) / (β * (1 - β)) := by
  rw [jeffreyB_prior hβ hβ']
  simp only [Mat.mA0]
  field_simp
  try ring

theorem jeffreyB_prior_mA1 (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    (jeffreyB (prior α β c) r₀).mA1
      = (β * (1 - α) * (1 - β) - c * (r₀ - β)) / (β * (1 - β)) := by
  rw [jeffreyB_prior hβ hβ']
  simp only [Mat.mA1]
  field_simp
  try ring

/-- The benchmark's normaliser, in closed form. -/
theorem bayesW_prior_total (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    (bayesW (prior α β c) α β q₀ r₀).total
      = (α * β * (1 - α) * (1 - β) + ((q₀ - α) * (r₀ - β)) * c)
          / (α * β * (1 - α) * (1 - β)) := by
  simp only [bayesW, Mat.total, prior]
  field_simp
  try ring

/-! ### Closed forms: every entry is (affine in `c`) / (affine in `c`) -/

-- PJab
theorem PJab_a00_closed (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) (hD : α * β * (1 - α) + ((q₀ - α)) * c ≠ 0) :
    (PJab α β c q₀ r₀).a00 = ((q₀ * r₀ * (1 - α) * (α * β)) + (q₀ * r₀ * (1 - α)) * c) / ((α * β * (1 - α)) + ((q₀ - α)) * c) := by
  rw [PJab]
  simp only [jeffreyB]
  rw [jeffreyA_prior_mB0 hα hα', jeffreyA_prior hα hα']
  field_simp
  try ring

theorem PJab_a01_closed (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) (hD : α * (1 - α) * (1 - β) + (-(q₀ - α)) * c ≠ 0) :
    (PJab α β c q₀ r₀).a01 = ((q₀ * (1 - r₀) * (1 - α) * (α * (1 - β))) + (-(q₀ * (1 - r₀) * (1 - α))) * c) / ((α * (1 - α) * (1 - β)) + (-(q₀ - α)) * c) := by
  rw [PJab]
  simp only [jeffreyB]
  rw [jeffreyA_prior_mB1 hα hα', jeffreyA_prior hα hα']
  field_simp
  try ring

theorem PJab_a10_closed (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) (hD : α * β * (1 - α) + ((q₀ - α)) * c ≠ 0) :
    (PJab α β c q₀ r₀).a10 = ((α * r₀ * (1 - q₀) * (β * (1 - α))) + (-(α * r₀ * (1 - q₀))) * c) / ((α * β * (1 - α)) + ((q₀ - α)) * c) := by
  rw [PJab]
  simp only [jeffreyB]
  rw [jeffreyA_prior_mB0 hα hα', jeffreyA_prior hα hα']
  field_simp
  try ring

theorem PJab_a11_closed (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) (hD : α * (1 - α) * (1 - β) + (-(q₀ - α)) * c ≠ 0) :
    (PJab α β c q₀ r₀).a11 = ((α * (1 - q₀) * (1 - r₀) * ((1 - α) * (1 - β))) + (α * (1 - q₀) * (1 - r₀)) * c) / ((α * (1 - α) * (1 - β)) + (-(q₀ - α)) * c) := by
  rw [PJab]
  simp only [jeffreyB]
  rw [jeffreyA_prior_mB1 hα hα', jeffreyA_prior hα hα']
  field_simp
  try ring


-- PJba
theorem PJba_a00_closed (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) (hD : α * β * (1 - β) + ((r₀ - β)) * c ≠ 0) :
    (PJba α β c q₀ r₀).a00 = ((q₀ * r₀ * (1 - β) * (α * β)) + (q₀ * r₀ * (1 - β)) * c) / ((α * β * (1 - β)) + ((r₀ - β)) * c) := by
  rw [PJba]
  simp only [jeffreyA]
  rw [jeffreyB_prior_mA0 hβ hβ', jeffreyB_prior hβ hβ']
  field_simp
  try ring

theorem PJba_a01_closed (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) (hD : α * β * (1 - β) + ((r₀ - β)) * c ≠ 0) :
    (PJba α β c q₀ r₀).a01 = ((β * q₀ * (1 - r₀) * (α * (1 - β))) + (-(β * q₀ * (1 - r₀))) * c) / ((α * β * (1 - β)) + ((r₀ - β)) * c) := by
  rw [PJba]
  simp only [jeffreyA]
  rw [jeffreyB_prior_mA0 hβ hβ', jeffreyB_prior hβ hβ']
  field_simp
  try ring

theorem PJba_a10_closed (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) (hD : β * (1 - α) * (1 - β) + (-(r₀ - β)) * c ≠ 0) :
    (PJba α β c q₀ r₀).a10 = ((r₀ * (1 - q₀) * (1 - β) * (β * (1 - α))) + (-(r₀ * (1 - q₀) * (1 - β))) * c) / ((β * (1 - α) * (1 - β)) + (-(r₀ - β)) * c) := by
  rw [PJba]
  simp only [jeffreyA]
  rw [jeffreyB_prior_mA1 hβ hβ', jeffreyB_prior hβ hβ']
  field_simp
  try ring

theorem PJba_a11_closed (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) (hD : β * (1 - α) * (1 - β) + (-(r₀ - β)) * c ≠ 0) :
    (PJba α β c q₀ r₀).a11 = ((β * (1 - q₀) * (1 - r₀) * ((1 - α) * (1 - β))) + (β * (1 - q₀) * (1 - r₀)) * c) / ((β * (1 - α) * (1 - β)) + (-(r₀ - β)) * c) := by
  rw [PJba]
  simp only [jeffreyA]
  rw [jeffreyB_prior_mA1 hβ hβ', jeffreyB_prior hβ hβ']
  field_simp
  try ring


-- PB
theorem PB_a00_closed (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) (hD : α * β * (1 - α) * (1 - β) + ((q₀ - α) * (r₀ - β)) * c ≠ 0) :
    (PB α β c q₀ r₀).a00 = ((q₀ * r₀ * (1 - α) * (1 - β) * (α * β)) + (q₀ * r₀ * (1 - α) * (1 - β)) * c) / ((α * β * (1 - α) * (1 - β)) + ((q₀ - α) * (r₀ - β)) * c) := by
  rw [PB, bayes, Mat.normalize, bayesW_prior_total hα hα' hβ hβ']
  simp only [bayesW, prior]
  rw [div_div_eq_mul_div]
  field_simp
  try ring

theorem PB_a01_closed (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) (hD : α * β * (1 - α) * (1 - β) + ((q₀ - α) * (r₀ - β)) * c ≠ 0) :
    (PB α β c q₀ r₀).a01 = ((β * q₀ * (1 - α) * (1 - r₀) * (α * (1 - β))) + (-(β * q₀ * (1 - α) * (1 - r₀))) * c) / ((α * β * (1 - α) * (1 - β)) + ((q₀ - α) * (r₀ - β)) * c) := by
  rw [PB, bayes, Mat.normalize, bayesW_prior_total hα hα' hβ hβ']
  simp only [bayesW, prior]
  rw [div_div_eq_mul_div]
  field_simp
  try ring

theorem PB_a10_closed (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) (hD : α * β * (1 - α) * (1 - β) + ((q₀ - α) * (r₀ - β)) * c ≠ 0) :
    (PB α β c q₀ r₀).a10 = ((α * r₀ * (1 - q₀) * (1 - β) * (β * (1 - α))) + (-(α * r₀ * (1 - q₀) * (1 - β))) * c) / ((α * β * (1 - α) * (1 - β)) + ((q₀ - α) * (r₀ - β)) * c) := by
  rw [PB, bayes, Mat.normalize, bayesW_prior_total hα hα' hβ hβ']
  simp only [bayesW, prior]
  rw [div_div_eq_mul_div]
  field_simp
  try ring

theorem PB_a11_closed (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) (hD : α * β * (1 - α) * (1 - β) + ((q₀ - α) * (r₀ - β)) * c ≠ 0) :
    (PB α β c q₀ r₀).a11 = ((α * β * (1 - q₀) * (1 - r₀) * ((1 - α) * (1 - β))) + (α * β * (1 - q₀) * (1 - r₀)) * c) / ((α * β * (1 - α) * (1 - β)) + ((q₀ - α) * (r₀ - β)) * c) := by
  rw [PB, bayes, Mat.normalize, bayesW_prior_total hα hα' hβ hβ']
  simp only [bayesW, prior]
  rw [div_div_eq_mul_div]
  field_simp
  try ring


/-! ### The derivative at `c = 0`, entry by entry

Each entry is (affine)/(affine), so `hasDerivAt_affine_div` applies once the
closed form is transported along a neighbourhood of `0` on which the
denominator does not vanish.
-/

theorem PJab_a00_hasDerivAt (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) (hP : (α * β * (1 - α)) ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJab α β c q₀ r₀).a00)
      (((q₀ * r₀ * (1 - α)) * (α * β * (1 - α)) - (q₀ * r₀ * (1 - α) * (α * β)) * ((q₀ - α))) / (α * β * (1 - α)) ^ 2) 0 := by
  refine (hasDerivAt_affine_div (q₀ * r₀ * (1 - α) * (α * β)) (q₀ * r₀ * (1 - α)) (α * β * (1 - α)) ((q₀ - α)) hP).congr_of_eventuallyEq ?_
  filter_upwards [eventually_affine_ne (α * β * (1 - α)) ((q₀ - α)) hP] with c hc
  exact PJab_a00_closed hα hα' hc

theorem PJab_a01_hasDerivAt (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) (hP : (α * (1 - α) * (1 - β)) ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJab α β c q₀ r₀).a01)
      (((-(q₀ * (1 - r₀) * (1 - α))) * (α * (1 - α) * (1 - β)) - (q₀ * (1 - r₀) * (1 - α) * (α * (1 - β))) * (-(q₀ - α))) / (α * (1 - α) * (1 - β)) ^ 2) 0 := by
  refine (hasDerivAt_affine_div (q₀ * (1 - r₀) * (1 - α) * (α * (1 - β))) (-(q₀ * (1 - r₀) * (1 - α))) (α * (1 - α) * (1 - β)) (-(q₀ - α)) hP).congr_of_eventuallyEq ?_
  filter_upwards [eventually_affine_ne (α * (1 - α) * (1 - β)) (-(q₀ - α)) hP] with c hc
  exact PJab_a01_closed hα hα' hc

theorem PJab_a10_hasDerivAt (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) (hP : (α * β * (1 - α)) ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJab α β c q₀ r₀).a10)
      (((-(α * r₀ * (1 - q₀))) * (α * β * (1 - α)) - (α * r₀ * (1 - q₀) * (β * (1 - α))) * ((q₀ - α))) / (α * β * (1 - α)) ^ 2) 0 := by
  refine (hasDerivAt_affine_div (α * r₀ * (1 - q₀) * (β * (1 - α))) (-(α * r₀ * (1 - q₀))) (α * β * (1 - α)) ((q₀ - α)) hP).congr_of_eventuallyEq ?_
  filter_upwards [eventually_affine_ne (α * β * (1 - α)) ((q₀ - α)) hP] with c hc
  exact PJab_a10_closed hα hα' hc

theorem PJab_a11_hasDerivAt (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) (hP : (α * (1 - α) * (1 - β)) ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJab α β c q₀ r₀).a11)
      (((α * (1 - q₀) * (1 - r₀)) * (α * (1 - α) * (1 - β)) - (α * (1 - q₀) * (1 - r₀) * ((1 - α) * (1 - β))) * (-(q₀ - α))) / (α * (1 - α) * (1 - β)) ^ 2) 0 := by
  refine (hasDerivAt_affine_div (α * (1 - q₀) * (1 - r₀) * ((1 - α) * (1 - β))) (α * (1 - q₀) * (1 - r₀)) (α * (1 - α) * (1 - β)) (-(q₀ - α)) hP).congr_of_eventuallyEq ?_
  filter_upwards [eventually_affine_ne (α * (1 - α) * (1 - β)) (-(q₀ - α)) hP] with c hc
  exact PJab_a11_closed hα hα' hc

theorem PJba_a00_hasDerivAt (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) (hP : (α * β * (1 - β)) ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJba α β c q₀ r₀).a00)
      (((q₀ * r₀ * (1 - β)) * (α * β * (1 - β)) - (q₀ * r₀ * (1 - β) * (α * β)) * ((r₀ - β))) / (α * β * (1 - β)) ^ 2) 0 := by
  refine (hasDerivAt_affine_div (q₀ * r₀ * (1 - β) * (α * β)) (q₀ * r₀ * (1 - β)) (α * β * (1 - β)) ((r₀ - β)) hP).congr_of_eventuallyEq ?_
  filter_upwards [eventually_affine_ne (α * β * (1 - β)) ((r₀ - β)) hP] with c hc
  exact PJba_a00_closed hβ hβ' hc

theorem PJba_a01_hasDerivAt (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) (hP : (α * β * (1 - β)) ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJba α β c q₀ r₀).a01)
      (((-(β * q₀ * (1 - r₀))) * (α * β * (1 - β)) - (β * q₀ * (1 - r₀) * (α * (1 - β))) * ((r₀ - β))) / (α * β * (1 - β)) ^ 2) 0 := by
  refine (hasDerivAt_affine_div (β * q₀ * (1 - r₀) * (α * (1 - β))) (-(β * q₀ * (1 - r₀))) (α * β * (1 - β)) ((r₀ - β)) hP).congr_of_eventuallyEq ?_
  filter_upwards [eventually_affine_ne (α * β * (1 - β)) ((r₀ - β)) hP] with c hc
  exact PJba_a01_closed hβ hβ' hc

theorem PJba_a10_hasDerivAt (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) (hP : (β * (1 - α) * (1 - β)) ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJba α β c q₀ r₀).a10)
      (((-(r₀ * (1 - q₀) * (1 - β))) * (β * (1 - α) * (1 - β)) - (r₀ * (1 - q₀) * (1 - β) * (β * (1 - α))) * (-(r₀ - β))) / (β * (1 - α) * (1 - β)) ^ 2) 0 := by
  refine (hasDerivAt_affine_div (r₀ * (1 - q₀) * (1 - β) * (β * (1 - α))) (-(r₀ * (1 - q₀) * (1 - β))) (β * (1 - α) * (1 - β)) (-(r₀ - β)) hP).congr_of_eventuallyEq ?_
  filter_upwards [eventually_affine_ne (β * (1 - α) * (1 - β)) (-(r₀ - β)) hP] with c hc
  exact PJba_a10_closed hβ hβ' hc

theorem PJba_a11_hasDerivAt (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) (hP : (β * (1 - α) * (1 - β)) ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJba α β c q₀ r₀).a11)
      (((β * (1 - q₀) * (1 - r₀)) * (β * (1 - α) * (1 - β)) - (β * (1 - q₀) * (1 - r₀) * ((1 - α) * (1 - β))) * (-(r₀ - β))) / (β * (1 - α) * (1 - β)) ^ 2) 0 := by
  refine (hasDerivAt_affine_div (β * (1 - q₀) * (1 - r₀) * ((1 - α) * (1 - β))) (β * (1 - q₀) * (1 - r₀)) (β * (1 - α) * (1 - β)) (-(r₀ - β)) hP).congr_of_eventuallyEq ?_
  filter_upwards [eventually_affine_ne (β * (1 - α) * (1 - β)) (-(r₀ - β)) hP] with c hc
  exact PJba_a11_closed hβ hβ' hc

theorem PB_a00_hasDerivAt (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) (hP : (α * β * (1 - α) * (1 - β)) ≠ 0) :
    HasDerivAt (fun c : ℝ => (PB α β c q₀ r₀).a00)
      (((q₀ * r₀ * (1 - α) * (1 - β)) * (α * β * (1 - α) * (1 - β)) - (q₀ * r₀ * (1 - α) * (1 - β) * (α * β)) * ((q₀ - α) * (r₀ - β))) / (α * β * (1 - α) * (1 - β)) ^ 2) 0 := by
  refine (hasDerivAt_affine_div (q₀ * r₀ * (1 - α) * (1 - β) * (α * β)) (q₀ * r₀ * (1 - α) * (1 - β)) (α * β * (1 - α) * (1 - β)) ((q₀ - α) * (r₀ - β)) hP).congr_of_eventuallyEq ?_
  filter_upwards [eventually_affine_ne (α * β * (1 - α) * (1 - β)) ((q₀ - α) * (r₀ - β)) hP] with c hc
  exact PB_a00_closed hα hα' hβ hβ' hc

theorem PB_a01_hasDerivAt (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) (hP : (α * β * (1 - α) * (1 - β)) ≠ 0) :
    HasDerivAt (fun c : ℝ => (PB α β c q₀ r₀).a01)
      (((-(β * q₀ * (1 - α) * (1 - r₀))) * (α * β * (1 - α) * (1 - β)) - (β * q₀ * (1 - α) * (1 - r₀) * (α * (1 - β))) * ((q₀ - α) * (r₀ - β))) / (α * β * (1 - α) * (1 - β)) ^ 2) 0 := by
  refine (hasDerivAt_affine_div (β * q₀ * (1 - α) * (1 - r₀) * (α * (1 - β))) (-(β * q₀ * (1 - α) * (1 - r₀))) (α * β * (1 - α) * (1 - β)) ((q₀ - α) * (r₀ - β)) hP).congr_of_eventuallyEq ?_
  filter_upwards [eventually_affine_ne (α * β * (1 - α) * (1 - β)) ((q₀ - α) * (r₀ - β)) hP] with c hc
  exact PB_a01_closed hα hα' hβ hβ' hc

theorem PB_a10_hasDerivAt (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) (hP : (α * β * (1 - α) * (1 - β)) ≠ 0) :
    HasDerivAt (fun c : ℝ => (PB α β c q₀ r₀).a10)
      (((-(α * r₀ * (1 - q₀) * (1 - β))) * (α * β * (1 - α) * (1 - β)) - (α * r₀ * (1 - q₀) * (1 - β) * (β * (1 - α))) * ((q₀ - α) * (r₀ - β))) / (α * β * (1 - α) * (1 - β)) ^ 2) 0 := by
  refine (hasDerivAt_affine_div (α * r₀ * (1 - q₀) * (1 - β) * (β * (1 - α))) (-(α * r₀ * (1 - q₀) * (1 - β))) (α * β * (1 - α) * (1 - β)) ((q₀ - α) * (r₀ - β)) hP).congr_of_eventuallyEq ?_
  filter_upwards [eventually_affine_ne (α * β * (1 - α) * (1 - β)) ((q₀ - α) * (r₀ - β)) hP] with c hc
  exact PB_a10_closed hα hα' hβ hβ' hc

theorem PB_a11_hasDerivAt (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) (hP : (α * β * (1 - α) * (1 - β)) ≠ 0) :
    HasDerivAt (fun c : ℝ => (PB α β c q₀ r₀).a11)
      (((α * β * (1 - q₀) * (1 - r₀)) * (α * β * (1 - α) * (1 - β)) - (α * β * (1 - q₀) * (1 - r₀) * ((1 - α) * (1 - β))) * ((q₀ - α) * (r₀ - β))) / (α * β * (1 - α) * (1 - β)) ^ 2) 0 := by
  refine (hasDerivAt_affine_div (α * β * (1 - q₀) * (1 - r₀) * ((1 - α) * (1 - β))) (α * β * (1 - q₀) * (1 - r₀)) (α * β * (1 - α) * (1 - β)) ((q₀ - α) * (r₀ - β)) hP).congr_of_eventuallyEq ?_
  filter_upwards [eventually_affine_ne (α * β * (1 - α) * (1 - β)) ((q₀ - α) * (r₀ - β)) hP] with c hc
  exact PB_a11_closed hα hα' hβ hβ' hc

/-! ### Proposition DIV (i) and (ii)

On the open cube every denominator above is a product of `α`, `β`, `1-α`,
`1-β`, so the derivatives combine.  The leading gap is exactly `κ R₁` on the
`A`-first route and `κ' R₂` on the `B`-first route, and the sequence effect is
their difference `κ R₁ - κ' R₂` -- Proposition DIV (i) and (ii).
-/

/-- **Proposition DIV (i)**, entry `a00`: `d/dc (Pᴶ_AB - Pᴮ)|₀ = κ · R₁`. -/
theorem propDIV_gap_AB_a00 (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJab α β c q₀ r₀).a00 - (PB α β c q₀ r₀).a00)
      (kappa α β q₀ r₀ * (R1 q₀).a00) 0 := by
  have hP1 : (α * β * (1 - α)) ≠ 0 := mul_ne_zero (mul_ne_zero (hα) hβ) hα'
  have hP2 : (α * β * (1 - α) * (1 - β)) ≠ 0 := mul_ne_zero (mul_ne_zero (mul_ne_zero (hα) hβ) hα') hβ'
  have h : HasDerivAt (fun c : ℝ => (PJab α β c q₀ r₀).a00 - (PB α β c q₀ r₀).a00)
      (((q₀ * r₀ * (1 - α)) * (α * β * (1 - α)) - (q₀ * r₀ * (1 - α) * (α * β)) * ((q₀ - α))) / (α * β * (1 - α)) ^ 2 - ((q₀ * r₀ * (1 - α) * (1 - β)) * (α * β * (1 - α) * (1 - β)) - (q₀ * r₀ * (1 - α) * (1 - β) * (α * β)) * ((q₀ - α) * (r₀ - β))) / (α * β * (1 - α) * (1 - β)) ^ 2) 0 :=
    HasDerivAt.sub (PJab_a00_hasDerivAt (q₀ := q₀) (r₀ := r₀) hα hα' hP1)
      (PB_a00_hasDerivAt (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ' hP2)
  convert h using 1
  simp only [kappa, Zpar, R1]
  field_simp
  ring

/-- **Proposition DIV (i)**, mirror, entry `a00`: `d/dc (Pᴶ_BA - Pᴮ)|₀ = κ' · R₂`. -/
theorem propDIV_gap_BA_a00 (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJba α β c q₀ r₀).a00 - (PB α β c q₀ r₀).a00)
      (kappa' α β q₀ r₀ * (R2 r₀).a00) 0 := by
  have hP1 : (α * β * (1 - β)) ≠ 0 := mul_ne_zero (mul_ne_zero (hα) hβ) hβ'
  have hP2 : (α * β * (1 - α) * (1 - β)) ≠ 0 := mul_ne_zero (mul_ne_zero (mul_ne_zero (hα) hβ) hα') hβ'
  have h : HasDerivAt (fun c : ℝ => (PJba α β c q₀ r₀).a00 - (PB α β c q₀ r₀).a00)
      (((q₀ * r₀ * (1 - β)) * (α * β * (1 - β)) - (q₀ * r₀ * (1 - β) * (α * β)) * ((r₀ - β))) / (α * β * (1 - β)) ^ 2 - ((q₀ * r₀ * (1 - α) * (1 - β)) * (α * β * (1 - α) * (1 - β)) - (q₀ * r₀ * (1 - α) * (1 - β) * (α * β)) * ((q₀ - α) * (r₀ - β))) / (α * β * (1 - α) * (1 - β)) ^ 2) 0 :=
    HasDerivAt.sub (PJba_a00_hasDerivAt (α := α) (q₀ := q₀) (r₀ := r₀) hβ hβ' hP1)
      (PB_a00_hasDerivAt (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ' hP2)
  convert h using 1
  simp only [kappa', Zpar, R2]
  field_simp
  ring

/-- **Proposition DIV (ii)**, entry `a00`: the sequence effect
`d/dc (Pᴶ_AB - Pᴶ_BA)|₀ = κ R₁ - κ' R₂`.  Since `R₁` and `R₂` are linearly
independent (`R1_R2_indep`), this vanishes only when `κ = κ' = 0`. -/
theorem propDIV_seq_a00 (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJab α β c q₀ r₀).a00 - (PJba α β c q₀ r₀).a00)
      (kappa α β q₀ r₀ * (R1 q₀).a00 - kappa' α β q₀ r₀ * (R2 r₀).a00) 0 := by
  have h := HasDerivAt.sub (propDIV_gap_AB_a00 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ') (propDIV_gap_BA_a00 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  have heq : (fun c : ℝ => (PJab α β c q₀ r₀).a00 - (PJba α β c q₀ r₀).a00)
      = fun c : ℝ => ((PJab α β c q₀ r₀).a00 - (PB α β c q₀ r₀).a00)
          - ((PJba α β c q₀ r₀).a00 - (PB α β c q₀ r₀).a00) := by
    funext c; ring
  rw [heq]; exact h

/-- **Proposition DIV (i)**, entry `a01`: `d/dc (Pᴶ_AB - Pᴮ)|₀ = κ · R₁`. -/
theorem propDIV_gap_AB_a01 (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJab α β c q₀ r₀).a01 - (PB α β c q₀ r₀).a01)
      (kappa α β q₀ r₀ * (R1 q₀).a01) 0 := by
  have hP1 : (α * (1 - α) * (1 - β)) ≠ 0 := mul_ne_zero (mul_ne_zero (hα) hα') hβ'
  have hP2 : (α * β * (1 - α) * (1 - β)) ≠ 0 := mul_ne_zero (mul_ne_zero (mul_ne_zero (hα) hβ) hα') hβ'
  have h : HasDerivAt (fun c : ℝ => (PJab α β c q₀ r₀).a01 - (PB α β c q₀ r₀).a01)
      (((-(q₀ * (1 - r₀) * (1 - α))) * (α * (1 - α) * (1 - β)) - (q₀ * (1 - r₀) * (1 - α) * (α * (1 - β))) * (-(q₀ - α))) / (α * (1 - α) * (1 - β)) ^ 2 - ((-(β * q₀ * (1 - α) * (1 - r₀))) * (α * β * (1 - α) * (1 - β)) - (β * q₀ * (1 - α) * (1 - r₀) * (α * (1 - β))) * ((q₀ - α) * (r₀ - β))) / (α * β * (1 - α) * (1 - β)) ^ 2) 0 :=
    HasDerivAt.sub (PJab_a01_hasDerivAt (q₀ := q₀) (r₀ := r₀) hα hα' hP1)
      (PB_a01_hasDerivAt (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ' hP2)
  convert h using 1
  simp only [kappa, Zpar, R1]
  field_simp
  ring

/-- **Proposition DIV (i)**, mirror, entry `a01`: `d/dc (Pᴶ_BA - Pᴮ)|₀ = κ' · R₂`. -/
theorem propDIV_gap_BA_a01 (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJba α β c q₀ r₀).a01 - (PB α β c q₀ r₀).a01)
      (kappa' α β q₀ r₀ * (R2 r₀).a01) 0 := by
  have hP1 : (α * β * (1 - β)) ≠ 0 := mul_ne_zero (mul_ne_zero (hα) hβ) hβ'
  have hP2 : (α * β * (1 - α) * (1 - β)) ≠ 0 := mul_ne_zero (mul_ne_zero (mul_ne_zero (hα) hβ) hα') hβ'
  have h : HasDerivAt (fun c : ℝ => (PJba α β c q₀ r₀).a01 - (PB α β c q₀ r₀).a01)
      (((-(β * q₀ * (1 - r₀))) * (α * β * (1 - β)) - (β * q₀ * (1 - r₀) * (α * (1 - β))) * ((r₀ - β))) / (α * β * (1 - β)) ^ 2 - ((-(β * q₀ * (1 - α) * (1 - r₀))) * (α * β * (1 - α) * (1 - β)) - (β * q₀ * (1 - α) * (1 - r₀) * (α * (1 - β))) * ((q₀ - α) * (r₀ - β))) / (α * β * (1 - α) * (1 - β)) ^ 2) 0 :=
    HasDerivAt.sub (PJba_a01_hasDerivAt (α := α) (q₀ := q₀) (r₀ := r₀) hβ hβ' hP1)
      (PB_a01_hasDerivAt (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ' hP2)
  convert h using 1
  simp only [kappa', Zpar, R2]
  field_simp
  ring

/-- **Proposition DIV (ii)**, entry `a01`: the sequence effect
`d/dc (Pᴶ_AB - Pᴶ_BA)|₀ = κ R₁ - κ' R₂`.  Since `R₁` and `R₂` are linearly
independent (`R1_R2_indep`), this vanishes only when `κ = κ' = 0`. -/
theorem propDIV_seq_a01 (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJab α β c q₀ r₀).a01 - (PJba α β c q₀ r₀).a01)
      (kappa α β q₀ r₀ * (R1 q₀).a01 - kappa' α β q₀ r₀ * (R2 r₀).a01) 0 := by
  have h := HasDerivAt.sub (propDIV_gap_AB_a01 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ') (propDIV_gap_BA_a01 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  have heq : (fun c : ℝ => (PJab α β c q₀ r₀).a01 - (PJba α β c q₀ r₀).a01)
      = fun c : ℝ => ((PJab α β c q₀ r₀).a01 - (PB α β c q₀ r₀).a01)
          - ((PJba α β c q₀ r₀).a01 - (PB α β c q₀ r₀).a01) := by
    funext c; ring
  rw [heq]; exact h

/-- **Proposition DIV (i)**, entry `a10`: `d/dc (Pᴶ_AB - Pᴮ)|₀ = κ · R₁`. -/
theorem propDIV_gap_AB_a10 (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJab α β c q₀ r₀).a10 - (PB α β c q₀ r₀).a10)
      (kappa α β q₀ r₀ * (R1 q₀).a10) 0 := by
  have hP1 : (α * β * (1 - α)) ≠ 0 := mul_ne_zero (mul_ne_zero (hα) hβ) hα'
  have hP2 : (α * β * (1 - α) * (1 - β)) ≠ 0 := mul_ne_zero (mul_ne_zero (mul_ne_zero (hα) hβ) hα') hβ'
  have h : HasDerivAt (fun c : ℝ => (PJab α β c q₀ r₀).a10 - (PB α β c q₀ r₀).a10)
      (((-(α * r₀ * (1 - q₀))) * (α * β * (1 - α)) - (α * r₀ * (1 - q₀) * (β * (1 - α))) * ((q₀ - α))) / (α * β * (1 - α)) ^ 2 - ((-(α * r₀ * (1 - q₀) * (1 - β))) * (α * β * (1 - α) * (1 - β)) - (α * r₀ * (1 - q₀) * (1 - β) * (β * (1 - α))) * ((q₀ - α) * (r₀ - β))) / (α * β * (1 - α) * (1 - β)) ^ 2) 0 :=
    HasDerivAt.sub (PJab_a10_hasDerivAt (q₀ := q₀) (r₀ := r₀) hα hα' hP1)
      (PB_a10_hasDerivAt (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ' hP2)
  convert h using 1
  simp only [kappa, Zpar, R1]
  field_simp
  ring

/-- **Proposition DIV (i)**, mirror, entry `a10`: `d/dc (Pᴶ_BA - Pᴮ)|₀ = κ' · R₂`. -/
theorem propDIV_gap_BA_a10 (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJba α β c q₀ r₀).a10 - (PB α β c q₀ r₀).a10)
      (kappa' α β q₀ r₀ * (R2 r₀).a10) 0 := by
  have hP1 : (β * (1 - α) * (1 - β)) ≠ 0 := mul_ne_zero (mul_ne_zero (hβ) hα') hβ'
  have hP2 : (α * β * (1 - α) * (1 - β)) ≠ 0 := mul_ne_zero (mul_ne_zero (mul_ne_zero (hα) hβ) hα') hβ'
  have h : HasDerivAt (fun c : ℝ => (PJba α β c q₀ r₀).a10 - (PB α β c q₀ r₀).a10)
      (((-(r₀ * (1 - q₀) * (1 - β))) * (β * (1 - α) * (1 - β)) - (r₀ * (1 - q₀) * (1 - β) * (β * (1 - α))) * (-(r₀ - β))) / (β * (1 - α) * (1 - β)) ^ 2 - ((-(α * r₀ * (1 - q₀) * (1 - β))) * (α * β * (1 - α) * (1 - β)) - (α * r₀ * (1 - q₀) * (1 - β) * (β * (1 - α))) * ((q₀ - α) * (r₀ - β))) / (α * β * (1 - α) * (1 - β)) ^ 2) 0 :=
    HasDerivAt.sub (PJba_a10_hasDerivAt (α := α) (q₀ := q₀) (r₀ := r₀) hβ hβ' hP1)
      (PB_a10_hasDerivAt (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ' hP2)
  convert h using 1
  simp only [kappa', Zpar, R2]
  field_simp
  ring

/-- **Proposition DIV (ii)**, entry `a10`: the sequence effect
`d/dc (Pᴶ_AB - Pᴶ_BA)|₀ = κ R₁ - κ' R₂`.  Since `R₁` and `R₂` are linearly
independent (`R1_R2_indep`), this vanishes only when `κ = κ' = 0`. -/
theorem propDIV_seq_a10 (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJab α β c q₀ r₀).a10 - (PJba α β c q₀ r₀).a10)
      (kappa α β q₀ r₀ * (R1 q₀).a10 - kappa' α β q₀ r₀ * (R2 r₀).a10) 0 := by
  have h := HasDerivAt.sub (propDIV_gap_AB_a10 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ') (propDIV_gap_BA_a10 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  have heq : (fun c : ℝ => (PJab α β c q₀ r₀).a10 - (PJba α β c q₀ r₀).a10)
      = fun c : ℝ => ((PJab α β c q₀ r₀).a10 - (PB α β c q₀ r₀).a10)
          - ((PJba α β c q₀ r₀).a10 - (PB α β c q₀ r₀).a10) := by
    funext c; ring
  rw [heq]; exact h

/-- **Proposition DIV (i)**, entry `a11`: `d/dc (Pᴶ_AB - Pᴮ)|₀ = κ · R₁`. -/
theorem propDIV_gap_AB_a11 (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJab α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11)
      (kappa α β q₀ r₀ * (R1 q₀).a11) 0 := by
  have hP1 : (α * (1 - α) * (1 - β)) ≠ 0 := mul_ne_zero (mul_ne_zero (hα) hα') hβ'
  have hP2 : (α * β * (1 - α) * (1 - β)) ≠ 0 := mul_ne_zero (mul_ne_zero (mul_ne_zero (hα) hβ) hα') hβ'
  have h : HasDerivAt (fun c : ℝ => (PJab α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11)
      (((α * (1 - q₀) * (1 - r₀)) * (α * (1 - α) * (1 - β)) - (α * (1 - q₀) * (1 - r₀) * ((1 - α) * (1 - β))) * (-(q₀ - α))) / (α * (1 - α) * (1 - β)) ^ 2 - ((α * β * (1 - q₀) * (1 - r₀)) * (α * β * (1 - α) * (1 - β)) - (α * β * (1 - q₀) * (1 - r₀) * ((1 - α) * (1 - β))) * ((q₀ - α) * (r₀ - β))) / (α * β * (1 - α) * (1 - β)) ^ 2) 0 :=
    HasDerivAt.sub (PJab_a11_hasDerivAt (q₀ := q₀) (r₀ := r₀) hα hα' hP1)
      (PB_a11_hasDerivAt (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ' hP2)
  convert h using 1
  simp only [kappa, Zpar, R1]
  field_simp
  ring

/-- **Proposition DIV (i)**, mirror, entry `a11`: `d/dc (Pᴶ_BA - Pᴮ)|₀ = κ' · R₂`. -/
theorem propDIV_gap_BA_a11 (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJba α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11)
      (kappa' α β q₀ r₀ * (R2 r₀).a11) 0 := by
  have hP1 : (β * (1 - α) * (1 - β)) ≠ 0 := mul_ne_zero (mul_ne_zero (hβ) hα') hβ'
  have hP2 : (α * β * (1 - α) * (1 - β)) ≠ 0 := mul_ne_zero (mul_ne_zero (mul_ne_zero (hα) hβ) hα') hβ'
  have h : HasDerivAt (fun c : ℝ => (PJba α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11)
      (((β * (1 - q₀) * (1 - r₀)) * (β * (1 - α) * (1 - β)) - (β * (1 - q₀) * (1 - r₀) * ((1 - α) * (1 - β))) * (-(r₀ - β))) / (β * (1 - α) * (1 - β)) ^ 2 - ((α * β * (1 - q₀) * (1 - r₀)) * (α * β * (1 - α) * (1 - β)) - (α * β * (1 - q₀) * (1 - r₀) * ((1 - α) * (1 - β))) * ((q₀ - α) * (r₀ - β))) / (α * β * (1 - α) * (1 - β)) ^ 2) 0 :=
    HasDerivAt.sub (PJba_a11_hasDerivAt (α := α) (q₀ := q₀) (r₀ := r₀) hβ hβ' hP1)
      (PB_a11_hasDerivAt (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ' hP2)
  convert h using 1
  simp only [kappa', Zpar, R2]
  field_simp
  ring

/-- **Proposition DIV (ii)**, entry `a11`: the sequence effect
`d/dc (Pᴶ_AB - Pᴶ_BA)|₀ = κ R₁ - κ' R₂`.  Since `R₁` and `R₂` are linearly
independent (`R1_R2_indep`), this vanishes only when `κ = κ' = 0`. -/
theorem propDIV_seq_a11 (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ => (PJab α β c q₀ r₀).a11 - (PJba α β c q₀ r₀).a11)
      (kappa α β q₀ r₀ * (R1 q₀).a11 - kappa' α β q₀ r₀ * (R2 r₀).a11) 0 := by
  have h := HasDerivAt.sub (propDIV_gap_AB_a11 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ') (propDIV_gap_BA_a11 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  have heq : (fun c : ℝ => (PJab α β c q₀ r₀).a11 - (PJba α β c q₀ r₀).a11)
      = fun c : ℝ => ((PJab α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11)
          - ((PJba α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11) := by
    funext c; ring
  rw [heq]; exact h

/-! ### Appendix A.1, Step 4: when the leading coefficient vanishes -/

/-- `κ` factors into the surprise in the `A`-impression and the spread of the
`B`-impression, so it vanishes exactly when the credential's impression fails to
surprise (`q₀ = α`) or the letter leaves no soft impression (`r₀ ∈ {0,1}`). -/
theorem kappa_eq_zero_iff (hZ : Zpar α β ≠ 0) :
    kappa α β q₀ r₀ = 0 ↔ α = q₀ ∨ r₀ = 0 ∨ r₀ = 1 := by
  simp only [kappa, div_eq_zero_iff, hZ, or_false]
  constructor
  · intro h
    rcases mul_eq_zero.mp h with h' | h'
    · rcases mul_eq_zero.mp h' with h'' | h''
      · exact Or.inl (sub_eq_zero.mp h'')
      · exact Or.inr (Or.inl h'')
    · exact Or.inr (Or.inr (by linarith))
  · rintro (h | h | h)
    · simp [h]
    · simp [h]
    · simp [h]

/-- `κ'` vanishes exactly on the mirror locus. -/
theorem kappa'_eq_zero_iff (hZ : Zpar α β ≠ 0) :
    kappa' α β q₀ r₀ = 0 ↔ β = r₀ ∨ q₀ = 0 ∨ q₀ = 1 := by
  simp only [kappa', div_eq_zero_iff, hZ, or_false]
  constructor
  · intro h
    rcases mul_eq_zero.mp h with h' | h'
    · rcases mul_eq_zero.mp h' with h'' | h''
      · exact Or.inl (sub_eq_zero.mp h'')
      · exact Or.inr (Or.inl h'')
    · exact Or.inr (Or.inr (by linarith))
  · rintro (h | h | h)
    · simp [h]
    · simp [h]
    · simp [h]

/-- **Appendix A.1, Step 3**: the leading gap preserves the `A`-marginal --
`D_i0 = -D_i1`, the `(1,-1)` contrast made manifest. -/
theorem R1_rows_sum_zero (q₀ : ℝ) :
    (R1 q₀).a00 + (R1 q₀).a01 = 0 ∧ (R1 q₀).a10 + (R1 q₀).a11 = 0 := by
  constructor <;> · simp only [R1]; ring

/-- ... and the mirror direction preserves the `B`-marginal. -/
theorem R2_cols_sum_zero (r₀ : ℝ) :
    (R2 r₀).a00 + (R2 r₀).a10 = 0 ∧ (R2 r₀).a01 + (R2 r₀).a11 = 0 := by
  constructor <;> · simp only [R2]; ring

/-- Both leading coefficients sum to zero across all four cells, as a
difference of probability distributions must. -/
theorem R1_total_zero (q₀ : ℝ) : (R1 q₀).total = 0 := by
  simp only [R1, Mat.total]; ring

theorem R2_total_zero (r₀ : ℝ) : (R2 r₀).total = 0 := by
  simp only [R2, Mat.total]; ring

end JeffreyOrder
