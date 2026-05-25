/-
  verify_topology.lean — Lean 4 + Mathlib verification
  Social Discrimination as Informationally Mediated Closure
  Author: Anurag Srivastava (Riskcare Ltd / University of Reading)
  ORCiD: 0000-0002-6477-4430
-/

import Mathlib.Topology.Order.IntermediateValue
import Mathlib.Topology.Order.Compact
import Mathlib.Analysis.Calculus.Deriv.MeanValue
import Mathlib.Analysis.Calculus.Deriv.Mul
import Mathlib.Analysis.Calculus.Deriv.Pow
import Mathlib.Analysis.Calculus.Deriv.Add
import Mathlib.Analysis.Calculus.MeanValue
import Mathlib.Data.Real.Basic
import Mathlib.Tactic

set_option linter.unusedVariables false

open Set Filter Topology

noncomputable def μ_val : ℝ := 1/20
noncomputable def δ_val : ℝ := 4/5
noncomputable def φmax_val : ℝ := 9/10
noncomputable def thresh : ℝ := 3/5

noncomputable def H_inner (x : ℝ) : ℝ :=
  μ_val + x * (1 - μ_val - δ_val * φmax_val) +
    x ^ 2 * (δ_val * φmax_val / thresh)

noncomputable def H_outer (x : ℝ) : ℝ :=
  μ_val + x * (1 - μ_val)

noncomputable def H (x : ℝ) : ℝ :=
  if x ≤ thresh then H_inner x else H_outer x

-- §1 F continuous and antitone

noncomputable def F_cont (x : ℝ) : ℝ :=
  φmax_val * max (1 - x / thresh) 0

lemma F_cont_continuous : Continuous F_cont := by
  unfold F_cont
  exact continuous_const.mul
    ((continuous_const.sub
      (continuous_id.div_const _)).max continuous_const)

lemma F_eq_F_cont (x : ℝ) :
    (if x ≤ thresh then
      φmax_val * (1 - x / thresh) else (0 : ℝ))
      = F_cont x := by
  unfold F_cont
  by_cases h : x ≤ thresh
  · rw [if_pos h, max_eq_left]
    rw [sub_nonneg, div_le_one (by norm_num [thresh])]
    exact h
  · rw [if_neg h]
    have hx : thresh < x := not_le.mp h
    have hth : (0 : ℝ) < thresh := by norm_num [thresh]
    have : 1 - x / thresh ≤ 0 := by
      have hx_ge : thresh ≤ x := le_of_lt hx
      have hthne : thresh ≠ 0 := ne_of_gt hth
      nlinarith [mul_pos hth hth,
                 mul_comm thresh (x / thresh),
                 div_mul_cancel₀ x hthne]
    rw [max_eq_right this, mul_zero]

theorem F_linear_continuousOn :
    ContinuousOn
      (fun x => if x ≤ thresh then
        φmax_val * (1 - x / thresh) else (0 : ℝ))
      (Icc 0 1) := by
  apply ContinuousOn.congr F_cont_continuous.continuousOn
  intro x _
  exact F_eq_F_cont x

theorem F_linear_antitoneOn :
    AntitoneOn
      (fun x => if x ≤ thresh then
        φmax_val * (1 - x / thresh) else (0 : ℝ))
      (Icc 0 1) := by
  intro a ha b hb hab
  simp only []
  by_cases h1 : a ≤ thresh
  · by_cases h2 : b ≤ thresh
    · rw [if_pos h1, if_pos h2]
      apply mul_le_mul_of_nonneg_left _
        (by norm_num [φmax_val])
      apply sub_le_sub_left
      exact div_le_div_of_nonneg_right hab
        (by norm_num [thresh])
    · rw [if_pos h1, if_neg h2]
      apply mul_nonneg (by norm_num [φmax_val])
      rw [sub_nonneg, div_le_one (by norm_num [thresh])]
      exact h1
  · by_cases h2 : b ≤ thresh
    · exfalso; exact h1 (le_trans hab h2)
    · rw [if_neg h1, if_neg h2]

-- §2 Fixed point existence via IVT

theorem closure_fixedpoint_exists
    (H_fn : ℝ → ℝ)
    (hH_co : ContinuousOn H_fn (Icc 0 1))
    (hH_map :
      ∀ x ∈ Icc (0 : ℝ) 1, H_fn x ∈ Icc 0 1)
    (μ_pos : ℝ) (hmu : 0 < μ_pos)
    (hH0 : H_fn 0 = μ_pos) (hH1 : H_fn 1 = 1) :
    ∃ x ∈ Icc (0 : ℝ) 1, H_fn x = x := by
  let Ψ := fun x => H_fn x - x
  have hΨ_co : ContinuousOn Ψ (Icc 0 1) :=
    hH_co.sub (continuousOn_id.mono (subset_univ _))
  have hΨ0 : Ψ 0 = μ_pos := by simp [Ψ, hH0]
  have hΨ1 : Ψ 1 = 0 := by simp [Ψ, hH1]
  obtain ⟨x, hx, hxeq⟩ :=
    intermediate_value_Icc' zero_le_one hΨ_co
      (by rw [hΨ0, hΨ1]
          exact ⟨le_refl _, le_of_lt hmu⟩)
  exact ⟨x, hx, by linarith⟩

lemma H_glue : H_inner thresh = H_outer thresh := by
  unfold H_inner H_outer μ_val δ_val φmax_val thresh
  norm_num

-- H continuous via global continuous extension
-- H(x) = H_outer(x) when x > thresh, H_inner(x) otherwise
-- Both are globally continuous polynomials agreeing at thresh.
-- Use: a function that equals a continuous function on a closed
-- set and another on its complement, with agreement on the
-- boundary, is continuous.
-- Mathlib: ContinuousOn.if requires specific argument order.
-- Simplest: just use that H = F_cont-style max expression.

-- Actually the SIMPLEST: H_inner and H_outer are both globally
-- continuous. For any x, H(x) is one or the other. Since they
-- agree at thresh, the piecewise is continuous.
-- Prove: H is continuous (globally, not just on [0,1]).

lemma H_continuous : Continuous H := by
  have hci : Continuous H_inner := by
    unfold H_inner; fun_prop
  have hco : Continuous H_outer := by
    unfold H_outer; fun_prop
  -- H(x) = if x ≤ thresh then H_inner x else H_outer x
  -- Use Continuous.if_le
  change Continuous (fun x =>
    if x ≤ thresh then H_inner x else H_outer x)
  exact hci.if_le hco continuous_id continuous_const
    (fun x hx => by rw [show x = thresh from hx]; exact H_glue)

theorem H_continuousOn :
    ContinuousOn H (Icc 0 1) :=
  H_continuous.continuousOn

lemma H_self_map :
    ∀ x ∈ Icc (0 : ℝ) 1, H x ∈ Icc 0 1 := by
  intro x ⟨hx0, hx1⟩
  unfold H
  split_ifs with hx
  · constructor
    · unfold H_inner μ_val δ_val φmax_val thresh
      nlinarith [sq_nonneg x]
    · unfold H_inner μ_val δ_val φmax_val thresh
      have : x ≤ 3/5 := hx
      nlinarith [sq_nonneg x, sq_nonneg (3/5 - x)]
  · constructor
    · unfold H_outer μ_val; nlinarith
    · unfold H_outer μ_val; nlinarith

lemma H_at_zero : H 0 = μ_val := by
  unfold H H_inner μ_val δ_val φmax_val thresh
  norm_num

lemma H_at_one : H 1 = 1 := by
  unfold H H_inner H_outer μ_val δ_val φmax_val thresh
  norm_num

theorem closure_equilibrium_exists :
    ∃ x ∈ Icc (0 : ℝ) 1, H x = x :=
  closure_fixedpoint_exists H H_continuousOn H_self_map
    μ_val (by norm_num [μ_val]) H_at_zero H_at_one

-- §3 Platform optimum

theorem platform_optimum_exists
    {X : Type*} [TopologicalSpace X]
    (obj_fn : X → ℝ) (hobj : Continuous obj_fn)
    (K : Set X) (hK : IsCompact K) (hKne : K.Nonempty) :
    ∃ x_opt ∈ K, IsMaxOn obj_fn K x_opt :=
  hK.exists_isMaxOn hKne hobj.continuousOn

-- §4 H' > 1 on interior

lemma H_inner_hasDerivAt (x : ℝ) :
    HasDerivAt H_inner
      ((1 - μ_val - δ_val * φmax_val) +
        2 * x * (δ_val * φmax_val / thresh)) x := by
  unfold H_inner
  have h1 := hasDerivAt_const x μ_val
  have h2 : HasDerivAt
    (fun x => x * (1 - μ_val - δ_val * φmax_val))
    (1 - μ_val - δ_val * φmax_val) x := by
    simpa using (hasDerivAt_id x).mul_const _
  have h3 : HasDerivAt
    (fun x => x ^ 2 * (δ_val * φmax_val / thresh))
    (2 * x * (δ_val * φmax_val / thresh)) x := by
    have := (hasDerivAt_pow 2 x).mul_const
      (δ_val * φmax_val / thresh)
    convert this using 1
    simp only [pow_succ, pow_zero, one_mul]
    ring
  convert (h1.add h2).add h3 using 1; ring

theorem multiplicity_threshold_interior :
    let c := thresh * (μ_val + δ_val * φmax_val) /
      (2 * δ_val * φmax_val)
    0 < c ∧ c < thresh := by
  constructor <;>
    norm_num [μ_val, δ_val, φmax_val, thresh]

theorem H_prime_exceeds_one :
    (1 - μ_val - δ_val * φmax_val) +
      2 * (35/100 : ℝ) *
        (δ_val * φmax_val / thresh) > 1 := by
  norm_num [μ_val, δ_val, φmax_val, thresh]

theorem H_not_global_contraction :
    ∃ x ∈ Ioo (0 : ℝ) thresh,
      (1 - μ_val - δ_val * φmax_val) +
        2 * x * (δ_val * φmax_val / thresh) > 1 :=
  ⟨35/100, by constructor <;> norm_num [thresh],
    H_prime_exceeds_one⟩

-- §5 Local stability

lemma H_outer_hasDerivAt (x : ℝ) :
    HasDerivAt H_outer (1 - μ_val) x := by
  unfold H_outer
  have h1 := hasDerivAt_const x μ_val
  have h2 : HasDerivAt (fun x => x * (1 - μ_val))
    (1 - μ_val) x := by
    simpa using (hasDerivAt_id x).mul_const _
  convert h1.add h2 using 1; ring

theorem integration_fixedpoint_stable :
    (1 : ℝ) - μ_val < 1 := by norm_num [μ_val]

theorem H_outer_minus_id_neg_deriv :
    ∀ x ∈ Ioo thresh 1,
      HasDerivAt (fun x => H_outer x - x)
        (-μ_val) x := by
  intro x _
  have := (H_outer_hasDerivAt x).sub (hasDerivAt_id x)
  convert this using 1; unfold μ_val; ring

-- §6 Path-dependence basin lemma

-- Helper: unfold iterate for the succ case
private lemma iterate_succ_eq (f : ℝ → ℝ) (n : ℕ)
    (x : ℝ) : f^[n + 1] x = f (f^[n] x) :=
  Function.iterate_succ_apply' f n x

theorem orbit_decreasing_below_diagonal
    (H_fn : ℝ → ℝ) (x₀ : ℝ)
    (h_lt : ∀ x, H_fn x < x →
      H_fn (H_fn x) < H_fn x)
    (hx₀ : H_fn x₀ < x₀) :
    ∀ n : ℕ, H_fn^[n + 1] x₀ < H_fn^[n] x₀ := by
  intro n
  induction n with
  | zero => simpa
  | succ k ih =>
      rw [iterate_succ_eq, iterate_succ_eq]
      have key : H_fn (H_fn^[k] x₀) < H_fn^[k] x₀ :=
        iterate_succ_eq H_fn k x₀ ▸ ih
      exact h_lt _ key

theorem orbit_increasing_above_diagonal
    (H_fn : ℝ → ℝ) (x₀ : ℝ)
    (h_gt : ∀ x, H_fn x > x →
      H_fn (H_fn x) > H_fn x)
    (hx₀ : H_fn x₀ > x₀) :
    ∀ n : ℕ, H_fn^[n + 1] x₀ > H_fn^[n] x₀ := by
  intro n
  induction n with
  | zero => simpa
  | succ k ih =>
      rw [iterate_succ_eq, iterate_succ_eq]
      have key : H_fn (H_fn^[k] x₀) > H_fn^[k] x₀ :=
        iterate_succ_eq H_fn k x₀ ▸ ih
      exact h_gt _ key

-- §7 Numeric spot checks

section NumericChecks
example : (0 : ℝ) < (3/5)*(1/20 + 4/5*(9/10)) /
  (2*(4/5)*(9/10)) := by norm_num
example : (3/5)*(1/20 + 4/5*(9/10)) /
  (2*(4/5)*(9/10)) < (3/5 : ℝ) := by norm_num
example : (1 : ℝ) - 1/20 = 19/20 := by norm_num
example : (19 : ℝ)/20 < 1 := by norm_num
example : (1 : ℝ) - 1/20 - 4/5*(9/10) +
  2*(35/100)*(4/5*(9/10)/(3/5)) > 1 := by norm_num
example : (-2 : ℝ)*(7/10 - 1/2) < 0 := by norm_num
example : (2 : ℝ)*(3/10 - 7/10) < 0 := by norm_num
example : (2 : ℝ) > 0 := by norm_num
example : (3/10 - 7/10 : ℝ)^2 +
  (3/10)*(7/10)/5 = 101/500 := by norm_num
example : (3 : ℝ)/10 / (1+4) = 3/50 := by norm_num
example : (2 : ℝ)*(7/10 - 3/10)*(1/2)*
  (2-1)*(1 + 1*(9/10)) > 0 := by norm_num
example : (1-(3 : ℝ)/10)*(8/10-4/10) > 0 := by
  norm_num
example (γ : ℝ) (hγ : 0 < γ) : 0 < γ := hγ
example : (0 : ℝ)*(1-0)/(1+4) = 0 := by norm_num
example : (0 : ℝ) < (1/2 : ℝ)*
  (1 - 1/2)/(1+4) := by norm_num
end NumericChecks
