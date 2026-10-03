/-
# Tversky & Kahneman (1992), "Advances in Prospect Theory: Cumulative Representation of Uncertainty"

*Journal of Risk and Uncertainty* 5, 297-323.  DOI 10.1007/BF00122574.

Source read in full: the journal PDF (27 pp.); `p.N` is the journal page.

## What is formalized

  * **Decision weights** (p.301).  For a risky prospect whose outcomes are
    ranked `0 = x_0 < x_1 < … < x_n` with probabilities `p_0, …, p_n`, the
    weight of the gain `x_i` is `w⁺(p_i + … + p_n) - w⁺(p_{i+1} + … + p_n)`:
    "the difference between the capacities of the events 'the outcome is at
    least as good as x_i' and 'the outcome is strictly better than x_i'"
    (`piPlus`, `tail`).  The top outcome gets `w⁺(p_n)` (`piPlus_top`).
  * **"For both positive and negative prospects, the decision weights add to
    1"** (p.301): `sum_piPlus`, by telescoping.
  * **"If each W is additive ... then π_i is simply the probability of A_i"**
    (p.301): `piPlus_id`.
  * **Rank dependence**: the same probability receives a different weight at a
    different rank (`rank_dependence`, with `w(p) = p²`).
  * **"For mixed prospects ... the sum can be either smaller or greater than 1,
    because the decision weights for gains and for losses are defined by
    separate capacities"** (p.301): `mixed_sum_lt_one`, `mixed_sum_gt_one`.
  * **The die example** (p.301): the gain weights of `f⁺ = (0, 1/2; 2, 1/6;
    4, 1/6; 6, 1/6)` are `w⁺(1/2) - w⁺(1/3)`, `w⁺(1/3) - w⁺(1/6)`,
    `w⁺(1/6) - w⁺(0)` (`die_example`).
  * **The weighting function (6)** (p.309), `w(p) = p^γ / (p^γ + (1-p)^γ)^{1/γ}`:
    `w(0) = 0` and `w(1) = 1` for `γ > 0` (`wTK_zero`, `wTK_one`).
  * **Table 6** (p.312): the eight medians reproduce the tabulated θ to two
    decimals, as `(x - b)/(a - c)`; the note's printed definition
    `θ = (x - b)/(c - a)` gives their negatives (`table6_theta`,
    `table6_printed_sign`).
  * **Median estimates** (pp.311-312): α = β = 0.88, λ = 2.25, γ = 0.61,
    δ = 0.69, and "γ < δ" (`median_gamma_lt_delta`).

## What is not formalized

The axiomatic analysis (Theorems 1 and 2, comonotonic independence, double
matching, sign-comonotonic tradeoff consistency); the experiment's other
tables and figures; the uncertainty (capacity) version beyond its definition.
The theory contains no rule for revising probabilities: valuation "is applied
to framed prospects" (p.299).
-/
import Mathlib

open Finset

namespace Literature.TverskyKahneman

/-! ## Decision weights (p.301) -/

/-- `p_i + … + p_n`: the probability that the outcome is at least as good as `x_i`. -/
def tail (p : ℕ → ℝ) (n i : ℕ) : ℝ := ∑ k ∈ Ico i (n + 1), p k

/-- The decision weight of the gain `x_i`: `w⁺(p_i+…+p_n) - w⁺(p_{i+1}+…+p_n)`. -/
def piPlus (w : ℝ → ℝ) (p : ℕ → ℝ) (n i : ℕ) : ℝ := w (tail p n i) - w (tail p n (i + 1))

theorem tail_past_end (p : ℕ → ℝ) (n : ℕ) : tail p n (n + 1) = 0 := by
  simp [tail]

theorem tail_step (p : ℕ → ℝ) {n i : ℕ} (hi : i ≤ n) :
    tail p n i = p i + tail p n (i + 1) := by
  unfold tail
  rw [Finset.sum_eq_sum_Ico_succ_bot (by omega)]

/-- The top-ranked gain gets `w⁺(p_n)`. -/
theorem piPlus_top (w : ℝ → ℝ) (p : ℕ → ℝ) (n : ℕ) (h0 : w 0 = 0) :
    piPlus w p n n = w (p n) := by
  unfold piPlus
  rw [tail_step p (le_refl n), tail_past_end, h0, add_zero, sub_zero]

/-- **"The decision weights add to 1"** for a positive prospect (p.301). -/
theorem sum_piPlus (w : ℝ → ℝ) (p : ℕ → ℝ) (n : ℕ) (h0 : w 0 = 0) (h1 : w 1 = 1)
    (hp : ∑ k ∈ range (n + 1), p k = 1) :
    ∑ i ∈ range (n + 1), piPlus w p n i = 1 := by
  unfold piPlus
  rw [Finset.sum_range_sub' (fun i => w (tail p n i)) (n + 1), tail_past_end, h0, sub_zero]
  have : tail p n 0 = 1 := by
    unfold tail; rw [← Finset.range_eq_Ico]; exact hp
  rw [this, h1]

/-- **Additive weights recover the probabilities** (p.301). -/
theorem piPlus_id (p : ℕ → ℝ) {n i : ℕ} (hi : i ≤ n) : piPlus id p n i = p i := by
  unfold piPlus
  rw [tail_step p hi]
  simp

/-- **Rank dependence.**  With `w(p) = p²` and three equiprobable gains, the top
gain is weighted `1/9` and the middle one `1/3`: the same probability, a
different weight, because the ranks differ. -/
theorem rank_dependence :
    let p : ℕ → ℝ := fun _ => 1 / 3
    let w : ℝ → ℝ := fun x => x ^ 2
    piPlus w p 2 2 = 1 / 9 ∧ piPlus w p 2 1 = 1 / 3 := by
  intro p w
  refine ⟨?_, ?_⟩
  · simp only [piPlus, tail, p, w]
    rw [show Ico 2 (2 + 1) = ({2} : Finset ℕ) by decide,
      show Ico (2 + 1) (2 + 1) = (∅ : Finset ℕ) by decide]
    norm_num
  · simp only [piPlus, tail, p, w]
    rw [show Ico 1 (2 + 1) = ({1, 2} : Finset ℕ) by decide,
      show Ico (1 + 1) (2 + 1) = ({2} : Finset ℕ) by decide]
    norm_num

/-- **Mixed prospects** (p.301): with separate weighting functions for gains and
losses, the weights of `(-x, 1/2; x, 1/2)` are `w⁺(1/2)` and `w⁻(1/2)`, and their
sum can be below or above 1.  Two increasing weighting functions on `[0,1]` with
`w(0) = 0`, `w(1) = 1`: `p²` and `2p - p²`. -/
theorem mixed_sum_lt_one :
    ((1 : ℝ) / 2) ^ 2 + ((1 : ℝ) / 2) ^ 2 < 1 ∧ ((0 : ℝ)) ^ 2 = 0 ∧ ((1 : ℝ)) ^ 2 = 1 := by
  norm_num

theorem mixed_sum_gt_one :
    (2 * ((1 : ℝ) / 2) - ((1 : ℝ) / 2) ^ 2) + (2 * ((1 : ℝ) / 2) - ((1 : ℝ) / 2) ^ 2) > 1 ∧
      2 * (0 : ℝ) - 0 ^ 2 = 0 ∧ 2 * (1 : ℝ) - 1 ^ 2 = 1 := by
  norm_num

theorem sq_increasing : MonotoneOn (fun x : ℝ => x ^ 2) (Set.Icc 0 1) := by
  intro a ha b _ hab
  simp only
  exact pow_le_pow_left₀ ha.1 hab 2

theorem concave_increasing : MonotoneOn (fun x : ℝ => 2 * x - x ^ 2) (Set.Icc 0 1) := by
  intro a ha b hb hab
  simp only
  nlinarith [ha.1, ha.2, hb.1, hb.2]

/-! ## The die example (p.301) -/

/-- `f⁺ = (0, 1/2; 2, 1/6; 4, 1/6; 6, 1/6)`, ranks 0..3. -/
noncomputable def dieGain : ℕ → ℝ := fun k => if k = 0 then 1 / 2 else 1 / 6

theorem die_example (w : ℝ → ℝ) :
    piPlus w dieGain 3 1 = w (1 / 2) - w (1 / 3) ∧
    piPlus w dieGain 3 2 = w (1 / 3) - w (1 / 6) ∧
    piPlus w dieGain 3 3 = w (1 / 6) - w 0 := by
  have t1 : tail dieGain 3 1 = 1 / 2 := by
    simp only [tail]
    rw [show Ico 1 (3 + 1) = ({1, 2, 3} : Finset ℕ) by decide]
    simp [dieGain]; norm_num
  have t2 : tail dieGain 3 2 = 1 / 3 := by
    simp only [tail]
    rw [show Ico 2 (3 + 1) = ({2, 3} : Finset ℕ) by decide]
    simp [dieGain]; norm_num
  have t3 : tail dieGain 3 3 = 1 / 6 := by
    simp only [tail]
    rw [show Ico 3 (3 + 1) = ({3} : Finset ℕ) by decide]
    simp [dieGain]
  have t4 : tail dieGain 3 4 = 0 := tail_past_end dieGain 3
  refine ⟨?_, ?_, ?_⟩
  · simp only [piPlus]; rw [t1, show (1 + 1 : ℕ) = 2 by rfl, t2]
  · simp only [piPlus]; rw [t2, show (2 + 1 : ℕ) = 3 by rfl, t3]
  · simp only [piPlus]; rw [t3, show (3 + 1 : ℕ) = 4 by rfl, t4]

/-! ## The weighting function (6), p.309 -/

/-- `w(p) = p^γ / (p^γ + (1-p)^γ)^{1/γ}`. -/
noncomputable def wTK (γ p : ℝ) : ℝ := p ^ γ / (p ^ γ + (1 - p) ^ γ) ^ (1 / γ)

theorem wTK_zero {γ : ℝ} (hγ : 0 < γ) : wTK γ 0 = 0 := by
  simp [wTK, Real.zero_rpow hγ.ne']

theorem wTK_one {γ : ℝ} (hγ : 0 < γ) : wTK γ 1 = 1 := by
  simp [wTK, Real.zero_rpow hγ.ne']

/-! ## Table 6, a test of loss aversion (p.312) -/

/-- Table 6 rows `(a, b, c, x, θ)`, where x makes `($a, 1/2; $b, 1/2)` as attractive
as `($c, 1/2; $x, 1/2)`: problems 1-8 in order.  The tabulated θ equals
`(x - b)/(a - c)` to within 1/200 in every row. -/
theorem table6_theta :
    (((61 : ℝ) - (0 : ℝ)) / ((0 : ℝ) - (-25 : ℝ)) - 244/100 ≤ 1/200 ∧ 244/100 - ((61 : ℝ) - (0 : ℝ)) / ((0 : ℝ) - (-25 : ℝ)) ≤ 1/200) ∧
    (((101 : ℝ) - (0 : ℝ)) / ((0 : ℝ) - (-50 : ℝ)) - 202/100 ≤ 1/200 ∧ 202/100 - ((101 : ℝ) - (0 : ℝ)) / ((0 : ℝ) - (-50 : ℝ)) ≤ 1/200) ∧
    (((202 : ℝ) - (0 : ℝ)) / ((0 : ℝ) - (-100 : ℝ)) - 202/100 ≤ 1/200 ∧ 202/100 - ((202 : ℝ) - (0 : ℝ)) / ((0 : ℝ) - (-100 : ℝ)) ≤ 1/200) ∧
    (((280 : ℝ) - (0 : ℝ)) / ((0 : ℝ) - (-150 : ℝ)) - 187/100 ≤ 1/200 ∧ 187/100 - ((280 : ℝ) - (0 : ℝ)) / ((0 : ℝ) - (-150 : ℝ)) ≤ 1/200) ∧
    (((112 : ℝ) - (50 : ℝ)) / ((-20 : ℝ) - (-50 : ℝ)) - 207/100 ≤ 1/200 ∧ 207/100 - ((112 : ℝ) - (50 : ℝ)) / ((-20 : ℝ) - (-50 : ℝ)) ≤ 1/200) ∧
    (((301 : ℝ) - (150 : ℝ)) / ((-50 : ℝ) - (-125 : ℝ)) - 201/100 ≤ 1/200 ∧ 201/100 - ((301 : ℝ) - (150 : ℝ)) / ((-50 : ℝ) - (-125 : ℝ)) ≤ 1/200) ∧
    (((149 : ℝ) - (120 : ℝ)) / ((50 : ℝ) - (20 : ℝ)) - 97/100 ≤ 1/200 ∧ 97/100 - ((149 : ℝ) - (120 : ℝ)) / ((50 : ℝ) - (20 : ℝ)) ≤ 1/200) ∧
    (((401 : ℝ) - (300 : ℝ)) / ((100 : ℝ) - (25 : ℝ)) - 135/100 ≤ 1/200 ∧ 135/100 - ((401 : ℝ) - (300 : ℝ)) / ((100 : ℝ) - (25 : ℝ)) ≤ 1/200) := by
  norm_num

/-- With the note's printed definition `θ = (x - b)/(c - a)`, every value is the
negative of the tabulated θ, to within 1/200. -/
theorem table6_printed_sign :
    (((61 : ℝ) - (0 : ℝ)) / ((-25 : ℝ) - (0 : ℝ)) + 244/100 ≤ 1/200 ∧ -244/100 - ((61 : ℝ) - (0 : ℝ)) / ((-25 : ℝ) - (0 : ℝ)) ≤ 1/200) ∧
    (((101 : ℝ) - (0 : ℝ)) / ((-50 : ℝ) - (0 : ℝ)) + 202/100 ≤ 1/200 ∧ -202/100 - ((101 : ℝ) - (0 : ℝ)) / ((-50 : ℝ) - (0 : ℝ)) ≤ 1/200) ∧
    (((202 : ℝ) - (0 : ℝ)) / ((-100 : ℝ) - (0 : ℝ)) + 202/100 ≤ 1/200 ∧ -202/100 - ((202 : ℝ) - (0 : ℝ)) / ((-100 : ℝ) - (0 : ℝ)) ≤ 1/200) ∧
    (((280 : ℝ) - (0 : ℝ)) / ((-150 : ℝ) - (0 : ℝ)) + 187/100 ≤ 1/200 ∧ -187/100 - ((280 : ℝ) - (0 : ℝ)) / ((-150 : ℝ) - (0 : ℝ)) ≤ 1/200) ∧
    (((112 : ℝ) - (50 : ℝ)) / ((-50 : ℝ) - (-20 : ℝ)) + 207/100 ≤ 1/200 ∧ -207/100 - ((112 : ℝ) - (50 : ℝ)) / ((-50 : ℝ) - (-20 : ℝ)) ≤ 1/200) ∧
    (((301 : ℝ) - (150 : ℝ)) / ((-125 : ℝ) - (-50 : ℝ)) + 201/100 ≤ 1/200 ∧ -201/100 - ((301 : ℝ) - (150 : ℝ)) / ((-125 : ℝ) - (-50 : ℝ)) ≤ 1/200) ∧
    (((149 : ℝ) - (120 : ℝ)) / ((20 : ℝ) - (50 : ℝ)) + 97/100 ≤ 1/200 ∧ -97/100 - ((149 : ℝ) - (120 : ℝ)) / ((20 : ℝ) - (50 : ℝ)) ≤ 1/200) ∧
    (((401 : ℝ) - (300 : ℝ)) / ((25 : ℝ) - (100 : ℝ)) + 135/100 ≤ 1/200 ∧ -135/100 - ((401 : ℝ) - (300 : ℝ)) / ((25 : ℝ) - (100 : ℝ)) ≤ 1/200) := by
  norm_num

/-! ## Median estimates (pp.311-312) -/

/-- "The median exponent of the value function was 0.88 ... The median λ was 2.25
... the median values of γ and δ, respectively, were 0.61 and 0.69", and "γ < δ". -/
theorem median_gamma_lt_delta : (61 : ℚ) / 100 < 69 / 100 ∧ (225 : ℚ) / 100 > 2 := by norm_num

end Literature.TverskyKahneman
