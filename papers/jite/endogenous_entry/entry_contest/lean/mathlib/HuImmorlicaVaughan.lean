import Mathlib
import KappaSpread

/-!
# Hu, Immorlica and Vaughan, "The Disparate Effects of Strategic Manipulation"

Proceedings of the Conference on Fairness, Accountability, and Transparency (FAT* '19), ACM,
2019, 259-268.  Read from arXiv:1808.08646v4 (29 pages); page numbers are PDF pages.

Setup (pp.3-5): candidates of two groups, A advantaged and B disadvantaged, with a feature `x`
they can raise to `y ≥ x` at cost `c_m(y) − c_m(x)`, under the cost condition
`c_A(y) − c_A(x) ≤ c_B(y) − c_B(x)` for `y ≥ x`; a learner publishes a threshold `σ` and a
candidate gains `1` from a positive classification.  The true thresholds are `τ_B ≤ τ_A`.

## What is proved

1. Proposition 1 (pp.6, 17): the threshold that classifies group A perfectly lies above the one
   for group B, `σ_B ≤ σ_A` (`sigma_order`).
2. Proposition 2 (pp.7, 17-18): at any threshold `σ`, the lowest feature that reaches `σ` at
   cost one is lower for group A, `ℓ_A(σ) ≤ ℓ_B(σ)` (`ell_order`), so the errors are false
   positives on A and false negatives on B.  The appendix prints `ℓ_m(σ) = c_m⁻¹(c_m(σ) + 1)`
   where Definition 2 has `− 1`; `ell_def_sign` records which one reaches `σ` at cost one.
3. Proposition 3, affine case (pp.8, 18): with `c_A = q c_B` and `c_B` affine, `ℓ_B − ℓ_A` does
   not depend on `σ` (`affine_gap_const`).
4. The numbers of Examples 2 and 3 (p.27), with `c_A(x) = 3x`, `c_B(x) = 4x`, `τ_A = 0.4`,
   `τ_B = 0.3` (`example2_numbers`, `example3_numbers`), and `σ_B` of Example 1 (p.13),
   `c_B(x) = 12√x`, `σ_B ≈ 0.398` (`example1_sigmaB`).
5. The bridge to our model: the paper derives its cost condition from a cost felt more heavily by
   group B (pp.5-6).  With a strictly concave utility of wealth, the utility cost of the same
   outlay is lower for the richer agent (`cost_condition_of_wealth`), which is our
   burden-monotonicity.

## Not formalised

Theorem 1 and Proposition 4 (`d` dimensions), Section 4 and Appendix A.3-A.4 (subsidies;
Theorem 2 and Proposition 6 are shown by numerical examples).  The record
(`lit/hu_immorlica_vaughan_2019`) reads them from the paper.
-/

open Set Real

namespace HuImmorlicaVaughan2019

/-- The cost condition (1), p.4: raising the feature costs group A no more than group B. -/
def CostCondition (cA cB : ℝ → ℝ) : Prop := ∀ x y, x ≤ y → cA y - cA x ≤ cB y - cB x

/-- Proposition 1: `σ_m` solves `c_m(σ_m) = c_m(τ_m) + 1`; then `σ_B ≤ σ_A`. -/
theorem sigma_order {cA cB : ℝ → ℝ} (hcc : CostCondition cA cB) (hA : Monotone cA)
    (hB : StrictMono cB) {τA τB σA σB : ℝ} (hτ : τB ≤ τA)
    (hσA : cA σA = cA τA + 1) (hσB : cB σB = cB τB + 1) : σB ≤ σA := by
  have hτσ : τA ≤ σA := by
    by_contra h
    push_neg at h
    have := hA h.le
    linarith
  have h1 := hcc τA σA hτσ
  have h2 : cB τB ≤ cB τA := hB.monotone hτ
  have : cB σB ≤ cB σA := by linarith
  exact hB.le_iff_le.mp this

/-- Proposition 2's core: `ℓ_m(σ)` solves `c_m(σ) − c_m(ℓ_m) = 1` with `ℓ_m ≤ σ`; then
    `ℓ_A(σ) ≤ ℓ_B(σ)`. -/
theorem ell_order {cA cB : ℝ → ℝ} (hcc : CostCondition cA cB) (hB : StrictMono cB)
    {σ ℓA ℓB : ℝ} (hA_le : ℓA ≤ σ) (hℓA : cA σ - cA ℓA = 1) (hℓB : cB σ - cB ℓB = 1) :
    ℓA ≤ ℓB := by
  have h := hcc ℓA σ hA_le
  have : cB ℓA ≤ cB ℓB := by linarith
  exact hB.le_iff_le.mp this

/-- Definition 2 has `ℓ_m(σ) = c_m⁻¹(c_m(σ) − 1)`, the lowest feature that reaches `σ` at
    cost one; the `+ 1` printed in the appendix (pp.17-18) gives a point above `σ`. -/
theorem ell_def_sign {c : ℝ → ℝ} (hc : StrictMono c) {σ ℓ u : ℝ}
    (hℓ : c ℓ = c σ - 1) (hu : c u = c σ + 1) : ℓ < σ ∧ σ < u := by
  constructor
  · exact hc.lt_iff_lt.mp (by linarith)
  · exact hc.lt_iff_lt.mp (by linarith)

/-- Proposition 3, affine case: with `c_B(x) = a x + b` and `c_A = q c_B`, `ℓ_B(σ) = σ − 1/a`
    and `ℓ_A(σ) = σ − 1/(q a)`, so the gap is the same at every `σ`. -/
theorem affine_gap_const (a q σ₁ σ₂ : ℝ) :
    (σ₁ - 1 / a) - (σ₁ - 1 / (q * a)) = (σ₂ - 1 / a) - (σ₂ - 1 / (q * a)) := by ring

/-- Example 2 (p.27): with `c_B(x) = 4x`, `σ_B = τ_B + 1/4 = 0.55`, and the false positives on
    group A start at `ℓ_A(σ_B) = σ_B − 1/3 ≈ 0.217`. -/
theorem example2_numbers :
    (3:ℝ)/10 + 1/4 = 11/20 ∧ (0.216:ℝ) < 11/20 - 1/3 ∧ (11/20 : ℝ) - 1/3 < 0.217 := by
  norm_num

/-- Example 3 (p.27): `σ_A = 0.4 + 1/3 ≈ 0.733`; at `σ = 0.64`, `ℓ_A ≈ 0.31` and `ℓ_B = 0.39`;
    with the subsidy `β = 0.806` at `σ_A`, `ℓ_B^β = σ_A − 1/(4β) ≈ 0.423`. -/
theorem example3_numbers :
    (0.733:ℝ) < 2/5 + 1/3 ∧ (2/5 + 1/3 : ℝ) < 0.734 ∧
    (0.306:ℝ) < 16/25 - 1/3 ∧ (16/25 - 1/3 : ℝ) < 0.307 ∧ (16/25 : ℝ) - 1/4 = 39/100 ∧
    (0.423:ℝ) < 2/5 + 1/3 - 1 / (4 * 0.806) ∧ (2/5 + 1/3 - 1 / (4 * 0.806) : ℝ) < 0.424 := by
  norm_num

/-- Example 1 (p.13): with `c_B(x) = 12√x` and `τ_B = 0.3`, `σ_B = (√0.3 + 1/12)²`, which
    lies in `(0.398, 0.399)`. -/
theorem example1_sigmaB : (0.398:ℝ) < (Real.sqrt 0.3 + 1/12) ^ 2 ∧
    (Real.sqrt 0.3 + 1/12) ^ 2 < 0.399 := by
  have hlo : (0.5477:ℝ) < Real.sqrt 0.3 := by
    rw [Real.lt_sqrt (by norm_num)]; norm_num
  have hhi : Real.sqrt 0.3 < 0.5478 := by
    rw [Real.sqrt_lt' (by norm_num)]; norm_num
  constructor <;> nlinarith

/-- The bridge to our model: with a strictly concave utility of wealth, the same outlay `d`
    costs the richer agent less utility, the cost condition between a richer group A and a
    poorer group B. -/
theorem cost_condition_of_wealth {u : ℝ → ℝ} (hu : StrictConcaveOn ℝ (Ioi 0) u) {d wA wB : ℝ}
    (hd : 0 < d) (hB : d < wB) (hAB : wB < wA) :
    EntryContestKappa.kappa u d wA < EntryContestKappa.kappa u d wB :=
  EntryContestKappa.burden_strictAnti_of_strictConcave u d hd hu
    (show wB ∈ Ioi d from hB) (show wA ∈ Ioi d from by simp; linarith) hAB

/-- Control: without the cost condition the order of Proposition 1 can fail, here with group A
    facing the steeper cost. -/
theorem sigma_order_needs_condition :
    let cA : ℝ → ℝ := fun x => 4 * x
    let cB : ℝ → ℝ := fun x => 3 * x
    cA (2/5 + 1/4) = cA (2/5) + 1 ∧ cB (2/5 + 1/3) = cB (2/5) + 1 ∧
      (2/5 + 1/4 : ℝ) < 2/5 + 1/3 := by
  norm_num

end HuImmorlicaVaughan2019
