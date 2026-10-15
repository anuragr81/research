import Mathlib
import Spreads
import PayerShare
import WeightEndpoints
import SaturationBenchmark

/-!
# Implications for the rule (manuscript row M33; conclusions C15 to C18)

Four implications for an institution that sets the fee, the prize or the weight, each resting
on proven rows. This file proves the steps that go beyond those rows.

## What is proved

1. A fee that differs across challengers can reverse the order of entry, so a poorer challenger
   faces the lower cost (`varying_fee_reorders`). With a fee equal for all, the order of entry
   is the order of wealth at every fee (M23 with M5), so a uniform fee cut adds entrants from
   the next-richest down (M11 with M8).
2. The prize caps the count: at a fee at which `(k + 1) κ_k > V`, at most `k` challengers
   enter (`cap_sets_count`, from M10's cap; `cap_sets_count_model` on the paper's gain).
3. A compression of wealth toward a pivot `x₀` raises the count weakly when the marginal entrant
   stands at or below `x₀` (`compression_rises`), and lowers it weakly when the first outsider
   stands at or above `x₀` (`compression_falls`), from M12.
4. The entrants' share against its benchmark: with a non-negative bought component and the
   incumbent scored as an entrant, the share is at least `(k + 1)/(Q + 1)`
   (`share_ge_benchmark`, from M31 and `investorLaw_one`); a share off that benchmark means
   the two score laws differ, so the bought component counts (`share_off_benchmark`).

Controls: a compression of a profile with the marginal entrant above the pivot is not covered
by `compression_rises` (`compression_rises_needs_below` shows the hypothesis bites).
-/

open MeasureTheory ProbabilityTheory Set

set_option linter.unusedSectionVars false

namespace Implications

open EntryContestSpread EntryContestKappa EntryContestShare EntryContestP1P2 EntryContestModel

/-! ## 1. The fee and the order of entry -/

/-- A fee that differs across challengers can reverse the order of costs: with log utility, a
    challenger of wealth `3` facing a fee of `2` bears a higher cost than one of wealth `2`
    facing a fee of `1/2`. -/
theorem varying_fee_reorders : kappa Real.log (1/2) 2 < kappa Real.log 2 3 := by
  unfold kappa
  have h1 : Real.log 2 - Real.log (2 - 1/2) = Real.log (4/3) := by
    rw [show (2:ℝ) - 1/2 = 3/2 by norm_num, ← Real.log_div (by norm_num) (by norm_num)]
    norm_num
  have h2 : Real.log 3 - Real.log (3 - 2) = Real.log 3 := by norm_num
  rw [h1, h2]
  exact Real.log_lt_log (by norm_num) (by norm_num)

/-! ## 2. The prize caps the count -/

/-- If every index that enters satisfies M10's cap `(m + 1) κ_m ≤ V`, a fee at which
    `(k + 1) κ_k > V` admits at most `k` challengers. -/
theorem cap_sets_count (κ : ℝ → ℝ) (c : ℝ) (Δ : ℕ → ℝ) (Q : ℕ) (w : ℕ → ℝ) (V : ℝ)
    (hcap : ∀ m, κ (w m) ≤ Δ m → ((m + 1 : ℕ) : ℝ) * κ (w m) ≤ V)
    {k k' : ℕ} (hk' : IsCount κ c Δ Q w k') (hbig : V < ((k + 1 : ℕ) : ℝ) * κ (w k)) :
    k' ≤ k :=
  count_le_of_fails κ c Δ Q w k k' hk' (fun ⟨_, hκ⟩ => by
    have := hcap k hκ
    linarith)

/-- The same on the paper's gain: M10 supplies the cap for `Δ = Delta V α β C Q`. -/
theorem cap_sets_count_model (α β C : Measure ℝ) [IsProbabilityMeasure α] [NoAtoms α]
    [IsProbabilityMeasure β] [IsProbabilityMeasure C] (V : ℝ) (hV : 0 ≤ V) (Q : ℕ)
    (κ : ℝ → ℝ) (c : ℝ) (w : ℕ → ℝ) {k k' : ℕ}
    (hk' : IsCount κ c (Delta V α β C Q) Q w k')
    (hbig : V < ((k + 1 : ℕ) : ℝ) * κ (w k)) : k' ≤ k :=
  cap_sets_count κ c (Delta V α β C Q) Q w V
    (fun m hm => EntryContestP6P7.p7_cap α β C V hV Q (κ (w m)) m hm) hk' hbig

/-! ## 3. A compression of wealth -/

/-- A compression toward `x₀`: monotone, and it moves every wealth toward `x₀` without
    crossing it. -/
def IsCompression (x₀ : ℝ) (T : ℝ → ℝ) : Prop :=
  Monotone T ∧ ∀ x, (x ≤ x₀ → x ≤ T x ∧ T x ≤ x₀) ∧ (x₀ ≤ x → x₀ ≤ T x ∧ T x ≤ x)

/-- A compression raises the count weakly when the marginal entrant stands at or below the
    pivot. -/
theorem compression_rises (κ : ℝ → ℝ) (c : ℝ) (Δ : ℕ → ℝ) (Q : ℕ)
    (hκ : AntitoneOn κ (Ioi c)) (hΔ : Antitone Δ) (w : ℕ → ℝ) (hw : Antitone w)
    {x₀ : ℝ} {T : ℝ → ℝ} (hT : IsCompression x₀ T) {k k' : ℕ}
    (hk : IsCount κ c Δ Q w k) (hk' : IsCount κ c Δ Q (T ∘ w) k')
    (hbelow : 1 ≤ k → w (k - 1) ≤ x₀) : k ≤ k' :=
  margin_rise κ c Δ Q hκ hΔ w (T ∘ w) (hT.1.comp_antitone hw) k k' hk hk'
    (fun h => ((hT.2 (w (k - 1))).1 (hbelow h)).1)

/-- A compression lowers the count weakly when the first outsider stands at or above the
    pivot. -/
theorem compression_falls (κ : ℝ → ℝ) (c : ℝ) (Δ : ℕ → ℝ) (Q : ℕ)
    (hκ : AntitoneOn κ (Ioi c)) (w : ℕ → ℝ) {x₀ : ℝ} {T : ℝ → ℝ} (hT : IsCompression x₀ T)
    {k k' : ℕ} (hk : IsCount κ c Δ Q w k) (hk' : IsCount κ c Δ Q (T ∘ w) k')
    (habove : k < Q → x₀ ≤ w k) : k' ≤ k :=
  margin_fall κ c Δ Q hκ w (T ∘ w) k k' hk hk'
    (fun h => ((hT.2 (w k)).2 (habove h)).2)

/-- Control: a compression lowers a wealth above the pivot, so the rise branch needs the
    marginal entrant at or below it. -/
theorem compression_rises_needs_below :
    IsCompression 1 (fun x => (x + 1) / 2) ∧ (fun x : ℝ => (x + 1) / 2) 3 < 3 := by
  refine ⟨⟨fun a b hab => by simp only; linarith, fun x => ⟨fun h => ⟨by linarith, by linarith⟩,
    fun h => ⟨by linarith, by linarith⟩⟩⟩, by norm_num⟩

/-! ## 4. The entrants' share against its benchmark -/

/-- If the share is off `(k + 1)/(Q + 1)`, the entrant's and the outsider's score laws differ:
    the bought component counts. -/
theorem share_off_benchmark (α β : Measure ℝ) [IsProbabilityMeasure α] [NoAtoms α]
    [IsProbabilityMeasure β] {Q k : ℕ} (hk : k ≤ Q)
    (h : payerShare α β Q k ≠ ((k : ℝ) + 1) / ((Q : ℝ) + 1)) : α ≠ β := by
  rintro rfl
  exact h (payerShare_at_equal α Q k hk)

/-- With a non-negative bought component, `r` atomless and `0 < μ ≤ 1`, the share is at least
    its benchmark `(k + 1)/(Q + 1)`, the value at `μ = 1`, where the token adds nothing. -/
theorem share_ge_benchmark (ρr ρs : Measure ℝ) [IsProbabilityMeasure ρr] [IsProbabilityMeasure ρs]
    [NoAtoms ρr] (hs : ρs (Iio 0) = 0) {μ : ℝ} (hμ : 0 < μ) (hμ1 : μ ≤ 1) {Q k : ℕ}
    (hk : k ≤ Q) :
    ((k : ℝ) + 1) / ((Q : ℝ) + 1)
      ≤ payerShare (investorLaw μ ρr ρs) (nonInvestorLaw μ ρr) Q k := by
  have h := payerShare_antitone_mu ρr ρs hs 1 μ hμ hμ1 Q k
  rw [WeightEndpoints.investorLaw_one, WeightEndpoints.nonInvestorLaw_one,
    payerShare_at_equal ρr Q k hk] at h
  exact h

end Implications
