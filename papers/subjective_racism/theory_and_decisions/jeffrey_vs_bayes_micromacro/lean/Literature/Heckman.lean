/-
# Heckman, J. J. (1998), "Detecting Discrimination"

*Journal of Economic Perspectives* 12(2), 101-116.  The local copy is the
publisher-typeset article; pages cited are the journal's own (101-116).

Formalization of the formal model in the paper's Appendix, "Implicit Identifying
Assumptions In The Audit Method" (pp.112-115), and of the claim it supports:
"The audit method can find discrimination when in fact none exists; it can also
disguise discrimination when it is present" (p.102; argued pp.108-111, made
precise pp.113-115).  Not a formalization of Paper B's claims.

## The Appendix model

Race `r ∈ {1 (black), 0 (white)}`, characteristics `X = (X₁, X₂)`, firm effect `f`,
productivity `P = X₁ + X₂ + f` (race does not affect productivity).  An audit pair
is matched on `X₁ = X₁*` (the "standardization level") and sent to the same firm;
`X₂` is unobserved by the auditor but acted on by the firm.

* **Linear treatment** `T(P, r) = P + γ r` (p.112): the pair difference is
  `X₂¹ - X₂⁰ + γ` (p.113), so averages over pairs estimate `γ` without bias iff
  `E X₂¹ = E X₂⁰` -- "the crucial identifying assumption" (p.113).
* **Threshold treatment** `T = 1` iff `P ≥ c` (p.113), with race-specific cut-offs
  `c₁, c₀` when there is discrimination (p.114).

## What is formalized

* `audit_diff_linear`, `audit_mean_linear`, `audit_unbiased_iff` -- the linear case.
* `two_component_bias` -- the p.108 two-component example: equal total means, the
  audit matches the component on which blacks are more productive, and finds
  discrimination where there is none.
* Threshold hiring with two-point unobservables of equal mean `0` and spreads `1`
  (black) and `3/2` (white) -- variances `1` and `9/4`, the variance ratio `2.25`
  of Heckman's Figures 1-2 (p.114):
  - `no_disc_finds_disc`, `no_disc_finds_reverse`, `no_disc_finds_equal` -- with no
    discrimination the audit shows discrimination, reverse discrimination, or equal
    treatment depending only on the standardization level (p.111).
  - `disc_disguised`, `disc_reversed` -- with discrimination (`c₁ = 1/4 > c₀ = 0`,
    the cut-offs of Figure 2) the audit can find none, or find reverse
    discrimination (pp.111, 114).
  - `phase_high`, `phase_low` -- the general pattern for any spreads
    `0 ≤ d₁ < d₀`: which group the audit favours is set by the standardization
    level relative to the cut-off (pp.110-111).
  - `equal_means_not_enough` -- equal means of the unobservables do not repair
    the threshold case (p.109).
-/
import Mathlib

namespace Literature.Heckman

open Finset

/-! ## Linear treatment -/

/-- Productivity `P = X₁ + X₂ + f` (p.112). -/
def prod (x₁ x₂ f : ℝ) : ℝ := x₁ + x₂ + f

/-- Linear treatment `T(P, r) = P + γ r` (p.112), race `r` as `0` or `1`. -/
def treatLin (γ P r : ℝ) : ℝ := P + γ * r

/-- The audit-pair difference under linear treatment (p.113):
`T(P₁*,1) - T(P₀*,0) = X₂¹ - X₂⁰ + γ`. -/
theorem audit_diff_linear (γ x₁ x₂₁ x₂₀ f : ℝ) :
    treatLin γ (prod x₁ x₂₁ f) 1 - treatLin γ (prod x₁ x₂₀ f) 0 = x₂₁ - x₂₀ + γ := by
  unfold treatLin prod; ring

/-- Average over a finite population of audit pairs with weights `w` summing to
one: the audit estimate is `E X₂¹ - E X₂⁰ + γ`. -/
theorem audit_mean_linear {ι : Type*} (S : Finset ι) (w : ι → ℝ) (hw : ∑ i ∈ S, w i = 1)
    (γ : ℝ) (x₁ x₂₁ x₂₀ f : ι → ℝ) :
    ∑ i ∈ S, w i * (treatLin γ (prod (x₁ i) (x₂₁ i) (f i)) 1
        - treatLin γ (prod (x₁ i) (x₂₀ i) (f i)) 0)
      = ∑ i ∈ S, w i * x₂₁ i - ∑ i ∈ S, w i * x₂₀ i + γ := by
  simp_rw [audit_diff_linear]
  rw [show (∑ i ∈ S, w i * x₂₁ i - ∑ i ∈ S, w i * x₂₀ i + γ)
      = ∑ i ∈ S, w i * x₂₁ i - ∑ i ∈ S, w i * x₂₀ i + γ * ∑ i ∈ S, w i by rw [hw, mul_one],
    Finset.mul_sum, ← Finset.sum_sub_distrib, ← Finset.sum_add_distrib]
  exact Finset.sum_congr rfl fun i _ => by ring

/-- **The crucial identifying assumption** (p.113): the averaged audit estimate
equals `γ` iff the mean unobserved productivity is the same in both races. -/
theorem audit_unbiased_iff {ι : Type*} (S : Finset ι) (w : ι → ℝ) (hw : ∑ i ∈ S, w i = 1)
    (γ : ℝ) (x₁ x₂₁ x₂₀ f : ι → ℝ) :
    ∑ i ∈ S, w i * (treatLin γ (prod (x₁ i) (x₂₁ i) (f i)) 1
        - treatLin γ (prod (x₁ i) (x₂₀ i) (f i)) 0) = γ
      ↔ ∑ i ∈ S, w i * x₂₁ i = ∑ i ∈ S, w i * x₂₀ i := by
  rw [audit_mean_linear S w hw]
  constructor <;> intro h <;> linarith

/-- **The two-component example** (p.108).  Productivity is the sum of two
components; blacks are more productive on average on the first (mean `1` vs `0`),
whites on the second (`1` vs `0`), so mean productivity is equal.  The audit
equates the first component; with no discrimination (`γ = 0`) the expected audit
difference is `-1`: "the audit estimator is biased toward a finding of
discrimination". -/
theorem two_component_bias :
    let meanB : ℝ × ℝ := (1, 0)
    let meanW : ℝ × ℝ := (0, 1)
    meanB.1 + meanB.2 = meanW.1 + meanW.2
      ∧ meanB.2 - meanW.2 + (0 : ℝ) = -1 := by
  norm_num

/-! ## Threshold treatment -/

/-- Threshold hiring (p.113): hire (`1`) iff perceived productivity is at least the
cut-off `c`. -/
noncomputable def hire (c P : ℝ) : ℝ := if c ≤ P then 1 else 0

/-- Hiring probability at standardization level `x₁` (and `f = 0`, as in the
Figures, p.115) when the unobserved component is `±d` with probability `1/2` each
(mean `0`, variance `d²`). -/
noncomputable def hireProb (c x₁ d : ℝ) : ℝ := (hire c (x₁ - d) + hire c (x₁ + d)) / 2

/-- Black unobservables: spread `1` (variance `1`).  White: spread `3/2` (variance
`9/4 = 2.25`), so `Var(X₂⁰) = 2.25 Var(X₂¹)` as in Figures 1-2 (p.114). -/
def dB : ℝ := 1
/-- See `dB`. -/
noncomputable def dW : ℝ := 3 / 2

/-- **No discrimination, audit finds discrimination against blacks** (p.111,
Figure 1).  Common cut-off `0`; at a low standardization level `x₁ = -5/4` no black
auditor is hired while white auditors are hired half the time. -/
theorem no_disc_finds_disc : hireProb 0 (-5 / 4) dB = 0 ∧ hireProb 0 (-5 / 4) dW = 1 / 2 := by
  unfold hireProb hire dB dW; norm_num

/-- **No discrimination, audit finds reverse discrimination** (p.111): at a high
standardization level `x₁ = 5/4` black auditors are always hired, white ones half
the time. -/
theorem no_disc_finds_reverse : hireProb 0 (5 / 4) dB = 1 ∧ hireProb 0 (5 / 4) dW = 1 / 2 := by
  unfold hireProb hire dB dW; norm_num

/-- **No discrimination, audit finds equal treatment** (p.111): at `x₁ = 2`. -/
theorem no_disc_finds_equal : hireProb 0 2 dB = 1 ∧ hireProb 0 2 dW = 1 := by
  unfold hireProb hire dB dW; norm_num

/-- **Discrimination disguised** (p.111: "the audit study can find no
discrimination at all").  Blacks face the higher cut-off `c₁ = 1/4 > c₀ = 0`
(Figure 2's values, p.114), yet at `x₁ = 3` both auditors are always hired. -/
theorem disc_disguised : hireProb (1 / 4) 3 dB = 1 ∧ hireProb 0 3 dW = 1 := by
  unfold hireProb hire dB dW; norm_num

/-- **Discrimination reversed** (p.115: "audits would appear to reveal
discrimination in favor of blacks when in fact blacks are being held to a higher
standard").  Same cut-offs, `x₁ = 5/4`. -/
theorem disc_reversed : hireProb 0 (5 / 4) dW < hireProb (1 / 4) (5 / 4) dB := by
  unfold hireProb hire dB dW; norm_num

/-- **Discrimination shown, correctly signed** for completeness: same cut-offs,
`x₁ = -5/4`, blacks are hired less. -/
theorem disc_shown : hireProb (1 / 4) (-5 / 4) dB < hireProb 0 (-5 / 4) dW := by
  unfold hireProb hire dB dW; norm_num

/-- **The general pattern** behind the examples (pp.110-111, "the ability of the
audit pair method to detect discrimination will depend on the level at which the
observed level of productivity was standardized").  Two-point unobservables with
spreads `0 ≤ d₁ < d₀` and a common cut-off `c`: when the auditors are highly
qualified (`d₁ ≤ x₁ - c < d₀`) the less dispersed group is always hired and the
more dispersed one half the time. -/
theorem phase_high (c x₁ d₁ d₀ : ℝ) (hd₁ : 0 ≤ d₁) (h₁ : d₁ ≤ x₁ - c) (h₀ : x₁ - c < d₀) :
    hireProb c x₁ d₁ = 1 ∧ hireProb c x₁ d₀ = 1 / 2 := by
  unfold hireProb hire
  rw [if_pos (by linarith), if_pos (by linarith), if_neg (by linarith), if_pos (by linarith)]
  norm_num

/-- ... and when they are poorly qualified (`-d₀ ≤ x₁ - c < -d₁`) the more
dispersed group is hired half the time and the less dispersed one never. -/
theorem phase_low (c x₁ d₁ d₀ : ℝ) (hd₁ : 0 ≤ d₁) (h₁ : x₁ - c < -d₁) (h₀ : -d₀ ≤ x₁ - c) :
    hireProb c x₁ d₁ = 0 ∧ hireProb c x₁ d₀ = 1 / 2 := by
  unfold hireProb hire
  rw [if_neg (by linarith), if_neg (by linarith), if_neg (by linarith), if_pos (by linarith)]
  norm_num

/-- Equal means are not enough under threshold hiring: the two-point
distributions above have equal (zero) means and still produce different hiring
rates (p.109: "even if the means are" the same). -/
theorem equal_means_not_enough :
    ((-dB) + dB) / 2 = ((-dW) + dW) / 2 ∧ hireProb 0 (-5 / 4) dB ≠ hireProb 0 (-5 / 4) dW := by
  refine ⟨by ring, ?_⟩
  unfold hireProb hire dB dW; norm_num

end Literature.Heckman
