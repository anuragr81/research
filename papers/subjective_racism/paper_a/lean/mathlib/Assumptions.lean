import FrozenMap

namespace Assumptions

open ClosureMap FrozenMap Finset

/-! The plain-English readings of the assumptions in Table 2 of
MANUSCRIPT.tex. Where a reading restates algebra or a property of a formula,
the statement is checked here. -/

/-- A4. A likelihood constant in class, `L c = k`, under a prior that sums to
one, has marginal `∑ L c p c = k`. -/
theorem marginal_of_flat {C : Type*} [Fintype C] {p L : C → ℝ} {k : ℝ}
    (hL : ∀ c, L c = k) (hp : ∑ c, p c = 1) : ∑ c, L c * p c = k := by
  simp only [hL, ← Finset.mul_sum, hp, mul_one]

/-- A5. Salience in `[0, 1]` and a positive cost of exclusion give an exclusion
pressure that is not negative. -/
theorem pressure_nonneg {s δ : ℝ} (hs : 0 ≤ s) (hδ : 0 < δ) : 0 ≤ s * δ :=
  mul_nonneg hs hδ.le

/-- The law of motion of A6, with discrimination rate `φ`. -/
def law (μ γ δ φ c : ℝ) : ℝ :=
  μ + c * (1 - μ + γ - δ * φ) - γ * c ^ 2

/-- A6. The standing rises by `μ (1 − c)`, rises by `γ c (1 − c)` and falls by
`δ φ c`. -/
theorem law_plain (μ γ δ φ c : ℝ) :
    law μ γ δ φ c = c + μ * (1 - c) + γ * c * (1 - c) - δ * φ * c := by
  unfold law; ring

/-- A7. With the rate `φ = s F c` of the current standing, the law of A6 is the
composed map. -/
theorem law_relearned (F : ℝ → ℝ) (μ γ δ s c : ℝ) :
    law μ γ δ (s * F c) c = Hf F μ γ δ s c := by
  unfold law Hf; ring

/-- A8. With the rate `φ = s F β₀` of the frozen prior, the law of A6 is the
frozen map with `k = s δ F β₀`. -/
theorem law_frozen (F : ℝ → ℝ) (β₀ μ γ δ s c : ℝ) :
    law μ γ δ (s * F β₀) c = Hk μ γ (s * δ * F β₀) c := by
  unfold law Hk; ring

/-- The canonical discrimination-rate function of A9. -/
def canonF (c : ℝ) : ℝ := (1 - c) ^ 2

/-- A9. The canonical `F` is zero at full standing. -/
theorem canonF_one : canonF 1 = 0 := by
  norm_num [canonF]

/-- A9. The canonical `F` has zero slope at full standing. -/
theorem canonF_deriv_one : deriv canonF 1 = 0 := by
  have h : HasDerivAt canonF (2 * (1 - 1) * (-1)) 1 := by
    have := ((hasDerivAt_id (1 : ℝ)).const_sub 1).pow 2
    simpa [canonF] using this
  rw [h.deriv]; ring

/-- A9. The canonical `F` falls as standing rises on `[0, 1]`. -/
theorem canonF_strictAntiOn : StrictAntiOn canonF (Set.Icc 0 1) := by
  intro a _ b hb hab
  unfold canonF
  exact pow_lt_pow_left₀ (by linarith [hb.2]) (by linarith [hb.2]) two_ne_zero

/-- A9. Under the canonical `F` the composed map depends on salience and the
cost of exclusion only through their product. -/
theorem canon_through_product {μ γ δ s δ' s' : ℝ} (h : s * δ = s' * δ') (c : ℝ) :
    Hf canonF μ γ δ s c = Hf canonF μ γ δ' s' c := by
  have e : ∀ δ s, Hf canonF μ γ δ s c = H μ γ (s * δ) c := fun δ s => H_eq_Hf μ γ δ s c
  rw [e, e, h]

/-- Control for A9. A function with the same zero at full standing but a
nonzero slope there, `F c = 1 − c`, has slope `−1` at 1. -/
theorem control_slope_needed : deriv (fun c : ℝ => 1 - c) 1 ≠ 0 := by
  have h : HasDerivAt (fun c : ℝ => 1 - c) (-1) 1 := by
    simpa using (hasDerivAt_id (1 : ℝ)).const_sub 1
  rw [h.deriv]; norm_num

end Assumptions
