import IscLean.Displacement
import IscLean.Routes
import Mathlib.Tactic

/-!
Lifetime values on a fixed ladder. Every worker lives forever and is either holding a job or
displaced. Each period a job holder is displaced with the same chance `d`, whatever the rank
(M39). A displaced worker chooses a route and holds a job again next period, so the value of
being displaced, `W`, is the same whichever route led there. The value of holding a job paying
`x` then satisfies `V x = x + β ((1 - d) V x + d W)`, and route choices compare these values.
-/

open Finset

namespace Isc

variable {ι : Type*} [DecidableEq ι]

/-- The value of holding a job paying `x`. -/
noncomputable def holdValue (β d W x : ℝ) : ℝ := (x + β * d * W) / (1 - β * (1 - d))

/-- The lifetime multiplier: what one unit of income is worth for as long as the job is held. -/
noncomputable def lifeMult (β d : ℝ) : ℝ := 1 / (1 - β * (1 - d))

theorem hold_denominator_pos {β d : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hd0 : 0 ≤ d) (hd1 : d ≤ 1) :
    0 < 1 - β * (1 - d) := by
  nlinarith [mul_nonneg hβ0 hd0]

theorem holdValue_recursion {β d W x : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hd0 : 0 ≤ d) (hd1 : d ≤ 1) :
    holdValue β d W x = x + β * ((1 - d) * holdValue β d W x + d * W) := by
  have hD := hold_denominator_pos hβ0 hβ1 hd0 hd1
  unfold holdValue
  field_simp
  ring

theorem holdValue_sub {β d W x y : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hd0 : 0 ≤ d) (hd1 : d ≤ 1) :
    holdValue β d W x - holdValue β d W y = (x - y) * lifeMult β d := by
  have hD := hold_denominator_pos hβ0 hβ1 hd0 hd1
  unfold holdValue lifeMult
  field_simp

theorem lifeMult_ge_one {β d : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hd0 : 0 ≤ d) (hd1 : d ≤ 1) :
    1 ≤ lifeMult β d := by
  have hD := hold_denominator_pos hβ0 hβ1 hd0 hd1
  unfold lifeMult
  rw [le_div_iff₀ hD]
  nlinarith [mul_nonneg hβ0 (sub_nonneg.mpr hd1)]

theorem holdValue_mono {β d W : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hd0 : 0 ≤ d) (hd1 : d ≤ 1) :
    Monotone (holdValue β d W) := by
  intro x y hxy
  have h := holdValue_sub (x := y) (y := x) (W := W) hβ0 hβ1 hd0 hd1
  have hA := lifeMult_ge_one hβ0 hβ1 hd0 hd1
  nlinarith [mul_nonneg (sub_nonneg.mpr hxy) (by linarith : (0 : ℝ) ≤ lifeMult β d)]

/-- The return, paying the upskilling cost `C` once, beats the downgrade to income `f` exactly
when the cost is at most the income gap times the lifetime multiplier. -/
theorem return_beats_downgrade_lifetime_iff {β d W f xi C : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1)
    (hd0 : 0 ≤ d) (hd1 : d ≤ 1) :
    holdValue β d W f ≤ holdValue β d W xi - C ↔ C ≤ (xi - f) * lifeMult β d := by
  have h := holdValue_sub (x := xi) (y := f) (W := W) hβ0 hβ1 hd0 hd1
  constructor <;> intro hh <;> linarith

/-- A cost paid once is no worse than the same amount taken off the job's income for good. -/
theorem one_time_cost_cheaper {β d W x L : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hd0 : 0 ≤ d)
    (hd1 : d ≤ 1) (hL : 0 ≤ L) :
    holdValue β d W (x - L) ≤ holdValue β d W x - L := by
  have h := holdValue_sub (x := x) (y := x - L) (W := W) hβ0 hβ1 hd0 hd1
  have hA := lifeMult_ge_one hβ0 hβ1 hd0 hd1
  nlinarith [mul_le_mul_of_nonneg_left hA hL]

/-- The lottery's lifetime value with the lottery cost taken off every partner's income, against
the same with the cost paid once. -/
theorem poolEU_one_time_cost {P : Finset ι} {i : ι} (hi : i ∈ P) (x : ι → ℝ) {L β d W : ℝ}
    (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hd0 : 0 ≤ d) (hd1 : d ≤ 1) :
    poolEU P x L (holdValue β d W) i
      = poolEU P x 0 (holdValue β d W) i - L * lifeMult β d * (((P.erase i).card : ℝ) / ((P.card : ℝ) - 1)) := by
  unfold poolEU
  have e : ∀ k ∈ P.erase i, holdValue β d W (x k - L)
      = holdValue β d W (x k - 0) - L * lifeMult β d := by
    intro k _
    have h := holdValue_sub (x := x k - 0) (y := x k - L) (W := W) hβ0 hβ1 hd0 hd1
    linarith
  rw [sum_congr rfl e, sum_sub_distrib, sum_const, nsmul_eq_mul, sub_div]
  ring

/-- M33 carried over to lifetime values with the lottery cost paid once: when every partner's
former income less the lottery cost is at least the downgrade's income, the lottery is worth at
least the downgrade. -/
theorem lottery_beats_downgrade_lifetime {P : Finset ι} (h2 : 2 ≤ P.card) (x : ι → ℝ)
    {L f β d W : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hd0 : 0 ≤ d) (hd1 : d ≤ 1) (hL0 : 0 ≤ L)
    (hL : ∀ k ∈ P, f ≤ x k - L) :
    ∀ i ∈ P, holdValue β d W f ≤ poolEU P x 0 (holdValue β d W) i - L := by
  intro i hi
  have hmono : MonotoneOn (holdValue β d W) Set.univ :=
    fun a _ b _ hab => holdValue_mono hβ0 hβ1 hd0 hd1 hab
  have h1 := displaced_prefer_lottery h2 x hmono (Set.mem_univ f) (fun k _ => Set.mem_univ _) hL i hi
  have h2' : poolEU P x L (holdValue β d W) i
      = poolEU P x 0 (holdValue β d W) i
        - L * lifeMult β d * (((P.erase i).card : ℝ) / ((P.card : ℝ) - 1)) :=
    poolEU_one_time_cost hi x hβ0 hβ1 hd0 hd1
  have hA := lifeMult_ge_one hβ0 hβ1 hd0 hd1
  have hratio : ((P.erase i).card : ℝ) / ((P.card : ℝ) - 1) = 1 := by
    rw [card_erase_real hi]
    have : (2 : ℝ) ≤ (P.card : ℝ) := by exact_mod_cast h2
    exact div_self (by linarith)
  rw [hratio, mul_one] at h2'
  nlinarith [mul_le_mul_of_nonneg_left hA hL0]

/-- The top case of M40 and M41 together: a displaced worker whose upskilling cost is at least
the income climbed times the lifetime multiplier does no better on the return than on the
downgrade, and so no better than in the lottery. -/
theorem top_displaced_prefer_lottery_lifetime {P : Finset ι} (h2 : 2 ≤ P.card) (x : ι → ℝ)
    {L f β d W C : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hd0 : 0 ≤ d) (hd1 : d ≤ 1) (hL0 : 0 ≤ L)
    (hL : ∀ k ∈ P, f ≤ x k - L) {i : ι} (hi : i ∈ P) (hC : (x i - f) * lifeMult β d ≤ C) :
    holdValue β d W (x i) - C ≤ poolEU P x 0 (holdValue β d W) i - L := by
  have h1 := lottery_beats_downgrade_lifetime (W := W) h2 x hβ0 hβ1 hd0 hd1 hL0 hL i hi
  have h := holdValue_sub (x := x i) (y := f) (W := W) hβ0 hβ1 hd0 hd1
  linarith

/-- The upskilling cost per unit of income climbed, `κ ℓ^(b-1)`, rises with the income climbed
when `b ≥ 1`, so a cost threshold crossed at one rank is crossed at every higher rank. -/
theorem cost_ratio_rises_with_loss {κ b ℓ₁ ℓ₂ : ℝ} (hκ : 0 ≤ κ) (hb : 1 ≤ b) (h1 : 0 < ℓ₁)
    (h12 : ℓ₁ ≤ ℓ₂) : κ * ℓ₁ ^ (b - 1) ≤ κ * ℓ₂ ^ (b - 1) :=
  mul_le_mul_of_nonneg_left (Real.rpow_le_rpow h1.le h12 (by linarith)) hκ

theorem control_lifeMult_needs_discount_below_one : lifeMult 1 0 = 0 := by
  simp [lifeMult]

/-- With `β = 9/10` and `d = 1/10` the multiplier is `100/19`, so a cost of `5` for an income gap
of `1` is paid in lifetime terms although a one-shot comparison rejects it. -/
theorem control_lifetime_pays_what_one_shot_rejects :
    lifeMult (9 / 10) (1 / 10) = 100 / 19 ∧ (5 : ℝ) ≤ 1 * lifeMult (9 / 10) (1 / 10) ∧ ¬ (5 : ℝ) ≤ 1 := by
  refine ⟨by norm_num [lifeMult], by norm_num [lifeMult], by norm_num⟩

end Isc
