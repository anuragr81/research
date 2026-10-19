import IscLean.Routes
import Mathlib.Tactic

/-!
The firm's problem (M61). The firm chooses the price of retraining `κ`; the exponent `b` is
technology and the ladder is given. Each period a displaced worker of rank `i` returns when the
return, worth `x i - κ (x i - x0)^b`, is at least the better of the lottery, worth `V - L`, and
the downgrade, worth `x0`; otherwise, under M33's condition `x0 ≤ V - L`, the worker enters the
lottery and the job is filled by a stranger to it, which costs the firm the specific-skill
share `δ` of the job's income. Profit per displaced cohort is receipts from returners less that
loss. Values are the first approximation of M40 (a common continuation value).
-/

open Finset Classical

namespace Isc

variable {ι : Type*} [Fintype ι]

/-- The return's worth to a displaced worker of rank `i` at price `κ`. -/
noncomputable def returnWorth (x : ι → ℝ) (x0 b κ : ℝ) (i : ι) : ℝ := x i - κ * (x i - x0) ^ b

/-- The ranks that return at price `κ`, when the lottery is worth `V - L` and the downgrade `x0`. -/
noncomputable def band (x : ι → ℝ) (x0 b V L κ : ℝ) : Finset ι :=
  univ.filter (fun i => max (V - L) x0 ≤ returnWorth x x0 b κ i)

/-- The firm's receipts from returners. -/
noncomputable def receipts (x : ι → ℝ) (x0 b V L κ : ℝ) : ℝ :=
  ∑ i ∈ band x x0 b V L κ, κ * (x i - x0) ^ b

/-- The firm's loss on jobs filled through the lottery: the specific-skill share of their income. -/
noncomputable def lotteryLoss (x : ι → ℝ) (x0 b V L κ δ : ℝ) : ℝ :=
  δ * ∑ i ∈ univ \ band x x0 b V L κ, x i

/-- Profit per displaced cohort. -/
noncomputable def profit (x : ι → ℝ) (x0 b V L κ δ : ℝ) : ℝ :=
  receipts x x0 b V L κ - lotteryLoss x x0 b V L κ δ

/-- A higher price shrinks the band of returners. -/
theorem band_antitone_in_price (x : ι → ℝ) {x0 b V L κ κ' : ℝ} (hx : ∀ i, x0 ≤ x i)
    (hκ : κ ≤ κ') : band x x0 b V L κ' ⊆ band x x0 b V L κ := by
  intro i hi
  rw [band, mem_filter] at hi ⊢
  refine ⟨mem_univ _, le_trans hi.2 ?_⟩
  unfold returnWorth
  have : 0 ≤ (x i - x0) ^ b := Real.rpow_nonneg (by linarith [hx i]) b
  nlinarith [mul_le_mul_of_nonneg_right hκ this]

/-- A dearer lottery enlarges the band: whoever returned still returns. -/
theorem band_grows_with_lottery_cost (x : ι → ℝ) {x0 b V L L' κ : ℝ} (hL : L ≤ L') :
    band x x0 b V L κ ⊆ band x x0 b V L' κ := by
  intro i hi
  rw [band, mem_filter] at hi ⊢
  refine ⟨mem_univ _, le_trans ?_ hi.2⟩
  exact max_le_max (by linarith) le_rfl

/-- At a zero price the firm receives nothing. -/
theorem receipts_zero_at_zero_price (x : ι → ℝ) (x0 b V L : ℝ) :
    receipts x x0 b V L 0 = 0 := by
  simp [receipts]

/-- Once the price exceeds every rank's reservation, nobody returns. -/
theorem band_empty_above_reservations (x : ι → ℝ) {x0 b V L κ : ℝ}
    (h : ∀ i, x i - max (V - L) x0 < κ * (x i - x0) ^ b) : band x x0 b V L κ = ∅ := by
  rw [band, filter_eq_empty_iff]
  intro i _
  unfold returnWorth
  linarith [h i]

/-- At a fixed price, profit is nondecreasing in the lottery cost: receipts rise with the band
and the lottery loss falls with it. -/
theorem profit_nondecreasing_in_lottery_cost (x : ι → ℝ) {x0 b V L L' κ δ : ℝ}
    (hx : ∀ i, x0 ≤ x i) (hx0 : 0 ≤ x0) (hκ : 0 ≤ κ) (hδ : 0 ≤ δ) (hL : L ≤ L') :
    profit x x0 b V L κ δ ≤ profit x x0 b V L' κ δ := by
  have hsub := band_grows_with_lottery_cost x (x0 := x0) (b := b) (V := V) (κ := κ) hL
  have hterm : ∀ i, 0 ≤ κ * (x i - x0) ^ b :=
    fun i => mul_nonneg hκ (Real.rpow_nonneg (by linarith [hx i]) b)
  have hrec : receipts x x0 b V L κ ≤ receipts x x0 b V L' κ :=
    sum_le_sum_of_subset_of_nonneg hsub (fun i _ _ => hterm i)
  have hcomp : univ \ band x x0 b V L' κ ⊆ univ \ band x x0 b V L κ :=
    sdiff_subset_sdiff (Finset.Subset.refl _) hsub
  have hxpos : ∀ i, 0 ≤ x i := fun i => le_trans hx0 (hx i)
  have hloss : lotteryLoss x x0 b V L' κ δ ≤ lotteryLoss x x0 b V L κ δ :=
    mul_le_mul_of_nonneg_left (sum_le_sum_of_subset_of_nonneg hcomp (fun i _ _ => hxpos i)) hδ
  unfold profit
  linarith

/-- The subsidy case. A displaced worker whose own job is worth less than the lottery, `x_i < V - L`,
returns at a zero price only if paid `V - L - x_i`; the firm gains from paying it exactly when
that payment is below the specific-skill loss `δ x_i` of letting a stranger fill the job, that
is when `V - L < (1 + δ) x_i`. -/
theorem subsidy_worthwhile_iff {xi V L δ : ℝ} :
    V - L - xi < δ * xi ↔ V - L < (1 + δ) * xi := by
  constructor <;> intro h <;> linarith

/-- The subsidy makes the return exactly as good as the lottery. -/
theorem subsidy_restores_return {xi V L : ℝ} : xi + (V - L - xi) = V - L := by
  ring

/-- Control: with `δ = 1/4` and a lottery worth `2`, the firm subsidises a worker whose job pays
`9/5` (the payment `1/5` is below the loss `9/20`) but not one whose job pays `1` (the payment
`1` exceeds the loss `1/4`). -/
theorem control_subsidy_covers_near_mean_not_bottom :
    (2 : ℝ) < (1 + 1 / 4) * (9 / 5) ∧ ¬ ((2 : ℝ) < (1 + 1 / 4) * 1) := by
  norm_num

/-- Control: a negative price enlarges the band rather than shrinking it, so the antitone result
needs the order of prices as stated and says nothing against subsidies. -/
theorem control_lower_price_keeps_band (x : ι → ℝ) {x0 b V L κ : ℝ} (hx : ∀ i, x0 ≤ x i) :
    band x x0 b V L κ ⊆ band x x0 b V L (κ - 1) :=
  band_antitone_in_price x hx (by linarith)

end Isc
