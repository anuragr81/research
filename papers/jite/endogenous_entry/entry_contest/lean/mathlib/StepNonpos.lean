import Mathlib

open MeasureTheory Set intervalIntegral

namespace EntryContestAnalytic

variable {a b : ℝ}

/-- Integration by parts against `d(φ²/2)`, with the boundary term killed by
`φ a = φ b = 0`. This is S5 of `checks/verify_sympy.py` made general: the
boundary condition is a hypothesis, not a family. -/
theorem step_integral_eq
    (K K' φ φ' : ℝ → ℝ)
    (hK : ∀ x ∈ uIcc a b, HasDerivAt K (K' x) x)
    (hφ : ∀ x ∈ uIcc a b, HasDerivAt φ (φ' x) x)
    (hK'i : IntervalIntegrable K' volume a b)
    (hvi : IntervalIntegrable (fun x => φ x * φ' x) volume a b)
    (hφa : φ a = 0) (hφb : φ b = 0) :
    (∫ x in a..b, K x * (φ x * φ' x))
      = -∫ x in a..b, K' x * (φ x ^ 2 / 2) := by
  have hv : ∀ x ∈ uIcc a b, HasDerivAt (fun y => φ y ^ 2 / 2) (φ x * φ' x) x := by
    intro x hx
    have h2 : HasDerivAt (fun y => φ y ^ 2) (2 * φ x ^ 1 * φ' x) x := (hφ x hx).pow 2
    have := h2.div_const 2
    simpa [pow_one, mul_comm, mul_left_comm, mul_assoc] using this
  have hib := integral_mul_deriv_eq_deriv_mul hK hv hK'i hvi
  rw [hib, hφa, hφb]
  simp

/-- The analytic step, in the form `EntryContest.lean` assumes as
`step_nonpos`. `K` is a product of CDFs, hence non-decreasing, so `K' ≥ 0`;
`φ² ≥ 0`; the boundary term vanishes. No family is assumed. -/
theorem step_integral_nonpos
    (hab : a ≤ b) (K K' φ φ' : ℝ → ℝ)
    (hK : ∀ x ∈ uIcc a b, HasDerivAt K (K' x) x)
    (hφ : ∀ x ∈ uIcc a b, HasDerivAt φ (φ' x) x)
    (hK'i : IntervalIntegrable K' volume a b)
    (hvi : IntervalIntegrable (fun x => φ x * φ' x) volume a b)
    (hφa : φ a = 0) (hφb : φ b = 0)
    (hK'nn : ∀ x ∈ Icc a b, 0 ≤ K' x) :
    (∫ x in a..b, K x * (φ x * φ' x)) ≤ 0 := by
  rw [step_integral_eq K K' φ φ' hK hφ hK'i hvi hφa hφb, neg_nonpos]
  refine integral_nonneg hab ?_
  intro x hx
  have h1 : (0:ℝ) ≤ K' x := hK'nn x hx
  have h2 : (0:ℝ) ≤ φ x ^ 2 / 2 := by positivity
  exact mul_nonneg h1 h2

/-- Discharges `EntryContest.step_nonpos` from the P3 representation. `hrep`
is the algebraic identity S1, which is already abstract in `F, G, C`; this
theorem supplies the analytic half that was previously instantiated. -/
theorem step_nonpos_of_representation
    (hab : a ≤ b) (V : ℝ) (hV : 0 ≤ V)
    (Δ : ℕ → ℝ) (Kof K'of : ℕ → ℝ → ℝ) (φ φ' : ℝ → ℝ)
    (hrep : ∀ m, Δ (m + 1) - Δ m = V * ∫ x in a..b, Kof m x * (φ x * φ' x))
    (hK : ∀ m, ∀ x ∈ uIcc a b, HasDerivAt (Kof m) (K'of m x) x)
    (hφ : ∀ x ∈ uIcc a b, HasDerivAt φ (φ' x) x)
    (hK'i : ∀ m, IntervalIntegrable (K'of m) volume a b)
    (hvi : IntervalIntegrable (fun x => φ x * φ' x) volume a b)
    (hφa : φ a = 0) (hφb : φ b = 0)
    (hK'nn : ∀ m, ∀ x ∈ Icc a b, 0 ≤ K'of m x) :
    ∀ m, Δ (m + 1) ≤ Δ m := by
  intro m
  have hI : (∫ x in a..b, Kof m x * (φ x * φ' x)) ≤ 0 :=
    step_integral_nonpos hab (Kof m) (K'of m) φ φ'
      (hK m) hφ (hK'i m) hvi hφa hφb (hK'nn m)
  have : Δ (m + 1) - Δ m ≤ 0 := by
    rw [hrep m]
    exact mul_nonpos_of_nonneg_of_nonpos hV hI
  linarith

end EntryContestAnalytic
