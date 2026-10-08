import Mathlib
import MuMonotone
import Equilibrium

open MeasureTheory ProbabilityTheory Set Filter Topology

set_option linter.unusedSectionVars false

namespace EntryContestShare

open EntryContestModel EntryContestP1P2 EntryContestP6P7 EntryContestPMU EntryContestPMUWitness
  EntryContestFall EntryContestEq EntryContestMu

/-! **The paying class's share of the prize.** With `Q` challengers, `k` of whom pay, and the
incumbent paying, there are `k + 1` payers with law `α` and `Q − k` outsiders with law `β`. The
share of the prize going to the paying class is the probability that the best payer beats the
best outsider: the CDF of the largest of `Q − k` outsiders, `G^(Q−k)`, integrated against the
law of the largest of `k + 1` payers, `powLaw α α k`. The share rises with every additional
payer, with no dominance assumption on the laws; it equals the headcount share `(k+1)/(Q+1)`
when the laws coincide; it is invariant to a common positive rescaling; and along the
normalised family `r + t s` it rises in `t`, so it falls in the talent weight `μ`. Composed
with `kstar_antitone_mu`, the equilibrium share is non-increasing in `μ`. -/

section Definition

/-- The probability that the best of `k + 1` payers (law `α`) beats the best of `Q − k`
    outsiders (law `β`). -/
noncomputable def payerShare (α β : Measure ℝ) (Q k : ℕ) : ℝ :=
  ∫ x, cdf β x ^ (Q - k) ∂(powLaw α α k)

variable (α β : Measure ℝ) [IsProbabilityMeasure α] [IsProbabilityMeasure β]

theorem payerShare_nonneg (Q k : ℕ) : 0 ≤ payerShare α β Q k :=
  integral_nonneg (fun x => pow_nonneg (cdf_nonneg β x) _)

theorem payerShare_le_one (Q k : ℕ) : payerShare α β Q k ≤ 1 := by
  unfold payerShare
  calc ∫ x, cdf β x ^ (Q - k) ∂(powLaw α α k) ≤ ∫ _x, (1 : ℝ) ∂(powLaw α α k) :=
        integral_mono (integrable_cdf_pow β _ _) (integrable_const 1)
          (fun x => pow_le_one₀ (cdf_nonneg β x) (cdf_le_one β x))
    _ = 1 := by simp

/-- With no outsider the paying class takes the whole prize, whatever the laws. -/
theorem payerShare_of_le (Q k : ℕ) (hQk : Q ≤ k) : payerShare α β Q k = 1 := by
  unfold payerShare
  rw [Nat.sub_eq_zero_of_le hQk]
  simp

/-- **(1) Every challenger pays.** With `k = Q` there is no outsider and the share is `1`. -/
theorem payerShare_full (Q : ℕ) : payerShare α β Q Q = 1 :=
  payerShare_of_le α β Q Q le_rfl

end Definition

section Swap

variable (α β : Measure ℝ) [IsProbabilityMeasure α] [IsProbabilityMeasure β]

/-- The two ways of writing "the best of `m + 1` payers beats the best of `j + 1` outsiders"
    sum to one, by `two_max_sum`; only the payers' law need be atomless. -/
theorem swap_sum [NoAtoms α] (j m : ℕ) :
    (∫ x, cdf β x ^ (j + 1) ∂(powLaw α α m)) + (∫ x, cdf α x ^ (m + 1) ∂(powLaw β β j)) = 1 := by
  have h := two_max_sum (powLaw β β j) (powLaw α α m)
  simp_rw [cdf_iid α m, cdf_powLaw β β j, ← pow_succ'] at h
  linarith

/-- **(2) The share as one minus the outsiders' win probability.** With at least one
    outsider, the share is `1 − ∫ F^(k+1) dG^(Q−k)`. -/
theorem payerShare_eq_one_sub [NoAtoms α] (Q k : ℕ) (hk : k + 1 ≤ Q) :
    payerShare α β Q k = 1 - ∫ x, cdf α x ^ (k + 1) ∂(powLaw β β (Q - k - 1)) := by
  have h := swap_sum α β (Q - k - 1) k
  rw [show Q - k - 1 + 1 = Q - k by omega] at h
  unfold payerShare
  linarith

end Swap

section Monotone

variable (α β : Measure ℝ) [IsProbabilityMeasure α] [IsProbabilityMeasure β]

/-- A larger exponent on a CDF gives a smaller integral, since `0 ≤ cdf ≤ 1`. -/
theorem integral_cdf_pow_succ_le (ν ρ : Measure ℝ) [IsProbabilityMeasure ν]
    [IsProbabilityMeasure ρ] (j : ℕ) :
    ∫ x, cdf ν x ^ (j + 1) ∂ρ ≤ ∫ x, cdf ν x ^ j ∂ρ :=
  integral_mono (integrable_cdf_pow ν ρ _) (integrable_cdf_pow ν ρ _)
    (fun x => pow_le_pow_of_le_one (cdf_nonneg ν x) (cdf_le_one ν x) (Nat.le_succ j))

/-- Integrating `F^(m+1)` against the maximum of one more outsider draw gives a larger value:
    swap both sides with `swap_sum` and compare `G^(j+2) ≤ G^(j+1)` pointwise. -/
theorem integral_pow_powLaw_succ [NoAtoms α] (j m : ℕ) :
    ∫ x, cdf α x ^ (m + 1) ∂(powLaw β β j)
      ≤ ∫ x, cdf α x ^ (m + 1) ∂(powLaw β β (j + 1)) := by
  have h1 := swap_sum α β j m
  have h2 := swap_sum α β (j + 1) m
  have h3 := integral_cdf_pow_succ_le β (powLaw α α m) (j + 1)
  linarith

/-- **(3) One more payer raises the share.** No dominance assumption is needed: after (2)
    the difference is `∫ F^(k+1) dG^(n) − ∫ F^(k+2) dG^(n−1)`, and both `F^(k+2) ≤ F^(k+1)`
    and `G^(n) ≤ G^(n−1)` push it the right way. -/
theorem payerShare_mono_k [NoAtoms α] (Q k : ℕ) (hk : k + 1 ≤ Q) :
    payerShare α β Q k ≤ payerShare α β Q (k + 1) := by
  rcases Nat.lt_or_ge (k + 1) Q with hlt | hge
  · rw [payerShare_eq_one_sub α β Q k hk, payerShare_eq_one_sub α β Q (k + 1) hlt,
      show Q - (k + 1) - 1 = Q - k - 2 by omega, show Q - k - 1 = Q - k - 2 + 1 by omega]
    have hA := integral_cdf_pow_succ_le α (powLaw β β (Q - k - 2)) (k + 1)
    have hB := integral_pow_powLaw_succ α β (Q - k - 2) k
    linarith
  · have hQ : Q = k + 1 := by omega
    rw [hQ, payerShare_full]
    exact payerShare_le_one α β _ _

/-- The step inequality holds for every `k`: beyond `Q` the share is constantly `1`. -/
theorem payerShare_le_succ [NoAtoms α] (Q k : ℕ) :
    payerShare α β Q k ≤ payerShare α β Q (k + 1) := by
  rcases Nat.lt_or_ge k Q with h | h
  · exact payerShare_mono_k α β Q k h
  · rw [payerShare_of_le α β Q k h, payerShare_of_le α β Q (k + 1) (by omega)]

/-- The share is a monotone function of the number of payers. -/
theorem payerShare_monotone [NoAtoms α] (Q : ℕ) : Monotone (payerShare α β Q) :=
  monotone_nat_of_le_succ (payerShare_le_succ α β Q)

end Monotone

section Equal

variable (α : Measure ℝ) [IsProbabilityMeasure α] [NoAtoms α]

/-- **(4) Identical laws.** The paying class's share is its headcount share `(k+1)/(Q+1)`:
    `∫ F^(Q−k) dF^(k+1) = (k+1) ∫ F^Q dF = (k+1)/(Q+1)`. -/
theorem payerShare_at_equal (Q k : ℕ) (hk : k ≤ Q) :
    payerShare α α Q k = ((k : ℝ) + 1) / ((Q : ℝ) + 1) := by
  have h := integral_iid α k (fun x => cdf α x ^ (Q - k)) ((cdf α).mono.measurable.pow_const _)
    1 (fun x => by
      rw [abs_of_nonneg (pow_nonneg (cdf_nonneg α x) _)]
      exact pow_le_one₀ (cdf_nonneg α x) (cdf_le_one α x))
  simp only [← pow_add, Nat.sub_add_cancel hk] at h
  unfold payerShare
  rw [h, integral_cdf_pow α Q]
  ring

end Equal

section Scaling

/-- The maximum of two laws commutes with a common nonnegative rescaling. -/
theorem maxLaw_map_mul (A B : Measure ℝ) [IsProbabilityMeasure A] [IsProbabilityMeasure B]
    (μ : ℝ) (hμ : 0 ≤ μ) :
    maxLaw (A.map (fun y => μ * y)) (B.map (fun y => μ * y))
      = (maxLaw A B).map (fun y => μ * y) := by
  have hf : Measurable (fun y : ℝ => μ * y) := measurable_nonInvestorScore μ
  have e : (fun p : ℝ × ℝ => max p.1 p.2) ∘ Prod.map (fun y : ℝ => μ * y) (fun y : ℝ => μ * y)
      = (fun y : ℝ => μ * y) ∘ (fun p : ℝ × ℝ => max p.1 p.2) := by
    funext p
    simp only [Function.comp, Prod.map]
    exact ((monotone_mul_left_of_nonneg hμ).map_max).symm
  simp only [maxLaw]
  rw [Measure.map_prod_map _ _ hf hf, Measure.map_map measurable_max2 (hf.prodMap hf), e,
    ← Measure.map_map hf measurable_max2]

/-- The law of the largest of several draws commutes with a common nonnegative rescaling. -/
theorem powLaw_map_mul (A ν : Measure ℝ) [IsProbabilityMeasure A] [IsProbabilityMeasure ν]
    (μ : ℝ) (hμ : 0 ≤ μ) :
    ∀ n : ℕ, powLaw (A.map (fun y => μ * y)) (ν.map (fun y => μ * y)) n
      = (powLaw A ν n).map (fun y => μ * y)
  | 0 => by simp only [powLaw]
  | n + 1 => by
      simp only [powLaw]
      rw [powLaw_map_mul A ν μ hμ n, maxLaw_map_mul _ _ μ hμ]

variable (α β : Measure ℝ) [IsProbabilityMeasure α] [IsProbabilityMeasure β]

/-- **(5) The share is invariant to a common positive rescaling of both laws.** -/
theorem payerShare_map_mul (μ : ℝ) (hμ : 0 < μ) (Q k : ℕ) :
    payerShare (α.map (fun y => μ * y)) (β.map (fun y => μ * y)) Q k
      = payerShare α β Q k := by
  unfold payerShare
  rw [powLaw_map_mul α α μ hμ.le k,
    integral_map (measurable_nonInvestorScore μ).aemeasurable
      ((cdf _).mono.measurable.pow_const _).aestronglyMeasurable]
  simp_rw [cdf_map_mul β μ hμ, mul_div_cancel_left₀ _ hμ.ne']

variable (ρr ρs : Measure ℝ) [IsProbabilityMeasure ρr] [IsProbabilityMeasure ρs]

/-- **Normalisation.** For `μ > 0` the share equals the share of the normalised laws, with the
    payers at `r + t s`, `t = (1 − μ)/μ`, and the outsiders at `r`. -/
theorem payerShare_normalise (μ : ℝ) (hμ : 0 < μ) (Q k : ℕ) :
    payerShare (investorLaw μ ρr ρs) (nonInvestorLaw μ ρr) Q k
      = payerShare (payerLaw ((1 - μ) / μ) ρr ρs) ρr Q k := by
  rw [investorLaw_eq_map ρr ρs μ hμ.ne', nonInvestorLaw_eq_map ρr μ,
    payerShare_map_mul _ _ μ hμ]

end Scaling

section Weight

variable (ρr ρs : Measure ℝ) [IsProbabilityMeasure ρr] [IsProbabilityMeasure ρs]

/-- **(6) The normalised share rises in `t`.** With at least one outsider, (2) writes the
    share as `1 − ∫ F_t^(k+1) dG^(Q−k)` and `F_t` falls pointwise in `t`; with none, both
    shares are `1`. -/
theorem payerShare_antitone_t [NoAtoms ρr] (t t' : ℝ) (htt : t ≤ t') (hs : ρs (Iio 0) = 0)
    (Q k : ℕ) :
    payerShare (payerLaw t ρr ρs) ρr Q k ≤ payerShare (payerLaw t' ρr ρs) ρr Q k := by
  rcases Nat.lt_or_ge k Q with hlt | hge
  · rw [payerShare_eq_one_sub (payerLaw t ρr ρs) ρr Q k hlt,
      payerShare_eq_one_sub (payerLaw t' ρr ρs) ρr Q k hlt]
    have h := pow_integral_antitone ρr ρs t t' htt hs (k + 1) (powLaw ρr ρr (Q - k - 1))
    linarith
  · rw [payerShare_of_le (payerLaw t ρr ρs) ρr Q k hge,
      payerShare_of_le (payerLaw t' ρr ρs) ρr Q k hge]

/-- The normalised weight `(1 − μ)/μ` falls in `μ` on `μ > 0`. -/
theorem normalised_weight_antitone (μ μ' : ℝ) (hμ' : 0 < μ') (hμμ : μ' ≤ μ) :
    (1 - μ) / μ ≤ (1 - μ') / μ' := by
  have hμ : 0 < μ := lt_of_lt_of_le hμ' hμμ
  have e : ∀ a : ℝ, a ≠ 0 → (1 - a) / a = 1 / a - 1 := fun a ha => by
    rw [sub_div, div_self ha]
  rw [e μ hμ.ne', e μ' hμ'.ne']
  linarith [one_div_le_one_div_of_le hμ' hμμ]

/-- **(7) The share is non-increasing in `μ`.** For `0 < μ' ≤ μ`, `s ≥ 0` a.s. and `r`
    atomless, the paying class's share at `μ` is at most its share at `μ'`, for every `Q`
    and `k`. -/
theorem payerShare_antitone_mu [NoAtoms ρr] (hs : ρs (Iio 0) = 0) (μ μ' : ℝ) (hμ' : 0 < μ')
    (hμμ : μ' ≤ μ) (Q k : ℕ) :
    payerShare (investorLaw μ ρr ρs) (nonInvestorLaw μ ρr) Q k
      ≤ payerShare (investorLaw μ' ρr ρs) (nonInvestorLaw μ' ρr) Q k := by
  have hμ : 0 < μ := lt_of_lt_of_le hμ' hμμ
  rw [payerShare_normalise ρr ρs μ hμ Q k, payerShare_normalise ρr ρs μ' hμ' Q k]
  exact payerShare_antitone_t ρr ρs ((1 - μ) / μ) ((1 - μ') / μ')
    (normalised_weight_antitone μ μ' hμ' hμμ) hs Q k

/-- **(8) The equilibrium share is non-increasing in `μ`.** Under the hypotheses of
    `kstar_antitone_mu` (which give `k ≤ k'`), the paying class's share at `μ` with `k`
    payers is at most its share at `μ' ≤ μ` with `k'` payers: more payers raise the share at
    fixed `μ` by (3), and a lower `μ` raises it at fixed `k'` by (7). -/
theorem payerShare_kstar_antitone_mu [NoAtoms ρr] (V : ℝ) (hV : 0 ≤ V) (hs : ρs (Iio 0) = 0)
    (μ μ' : ℝ) (hμ' : 0 < μ') (hμμ : μ' ≤ μ) (Q : ℕ) (κ : ℕ → ℝ) (k k' : ℕ) (hkQ : k ≤ Q)
    (hprefix : ∀ j, j < k →
      κ j ≤ Delta V (investorLaw μ ρr ρs) (nonInvestorLaw μ ρr) (investorLaw μ ρr ρs) Q j)
    (hfail' : k' < Q →
      Delta V (investorLaw μ' ρr ρs) (nonInvestorLaw μ' ρr) (investorLaw μ' ρr ρs) Q k' < κ k') :
    payerShare (investorLaw μ ρr ρs) (nonInvestorLaw μ ρr) Q k
      ≤ payerShare (investorLaw μ' ρr ρs) (nonInvestorLaw μ' ρr) Q k' := by
  have hkk := kstar_antitone_mu ρr ρs V hV hs μ μ' hμ' hμμ Q κ k k' hkQ hprefix hfail'
  have hμ : 0 < μ := lt_of_lt_of_le hμ' hμμ
  calc payerShare (investorLaw μ ρr ρs) (nonInvestorLaw μ ρr) Q k
      = payerShare (payerLaw ((1 - μ) / μ) ρr ρs) ρr Q k :=
        payerShare_normalise ρr ρs μ hμ Q k
    _ ≤ payerShare (payerLaw ((1 - μ) / μ) ρr ρs) ρr Q k' :=
        payerShare_monotone (payerLaw ((1 - μ) / μ) ρr ρs) ρr Q hkk
    _ = payerShare (investorLaw μ ρr ρs) (nonInvestorLaw μ ρr) Q k' :=
        (payerShare_normalise ρr ρs μ hμ Q k').symm
    _ ≤ payerShare (investorLaw μ' ρr ρs) (nonInvestorLaw μ' ρr) Q k' :=
        payerShare_antitone_mu ρr ρs hs μ μ' hμ' hμμ Q k'

end Weight

section Controls

/-- One payer against one outsider: the share is `∫ G dF`. -/
theorem payerShare_one_zero (A β : Measure ℝ) [IsProbabilityMeasure A]
    [IsProbabilityMeasure β] : payerShare A β 1 0 = ∫ x, cdf β x ∂A := by
  unfold payerShare
  simp only [powLaw, Nat.sub_zero, pow_one]

/-- With `s = −1` surely the payer scores `r − t` against one outsider at `r`. -/
theorem share_dirac (t : ℝ) :
    payerShare (payerLaw t unif (Measure.dirac (-1))) unif 1 0
      = ∫ r, cdf unif (r + t * (-1)) ∂unif := by
  rw [payerShare_one_zero, payerLaw_dirac,
    integral_map (by fun_prop : Measurable (fun r : ℝ => r + t * (-1))).aemeasurable
      (cdf unif).mono.measurable.aestronglyMeasurable]

/-- At `t = 0` the two scores are both uniform, and the share is `1/2`. -/
theorem share_dirac_zero :
    payerShare (payerLaw 0 unif (Measure.dirac (-1))) unif 1 0 = 1 / 2 := by
  rw [share_dirac]
  simp only [zero_mul, add_zero]
  have h := integral_cdf_pow unif 1
  simp only [pow_one] at h
  rw [h]
  norm_num

/-- At `t = 1` the payer scores `r − 1 ≤ 0` and never beats the outsider: the share is `0`. -/
theorem share_dirac_one :
    payerShare (payerLaw 1 unif (Measure.dirac (-1))) unif 1 0 = 0 := by
  rw [share_dirac]
  have h : ∀ᵐ r ∂unif, cdf unif (r + 1 * (-1)) = 0 :=
    unif_ae_mem.mono (fun r hr => cdf_unif_of_nonpos _ (by linarith [hr.2]))
  rw [integral_congr_ae h, integral_zero]

/-- **Control: the share comparison needs `s ≥ 0`.** With `s = −1` surely (so
    `ρs (Iio 0) ≠ 0`), `r` uniform on `[0, 1]`, `Q = 1` and `k = 0`, the share *falls* from
    `1/2` at `t = 0` to `0` at `t = 1`, the reverse of `payerShare_antitone_t`. -/
theorem control_share_needs_nonneg :
    Measure.dirac (-1 : ℝ) (Iio 0) ≠ 0 ∧ (0 : ℝ) ≤ 1
      ∧ payerShare (payerLaw 1 unif (Measure.dirac (-1))) unif 1 0
          < payerShare (payerLaw 0 unif (Measure.dirac (-1))) unif 1 0 := by
  refine ⟨control_cdf_without_nonneg.1, zero_le_one, ?_⟩
  rw [share_dirac_one, share_dirac_zero]
  norm_num

/-- **Control: the full share needs `k = Q`.** With identical laws and `k < Q` the share is
    `(k+1)/(Q+1) < 1`, so `payerShare_full` cannot be extended to any `k < Q`. -/
theorem control_full_needs_no_outsiders (α : Measure ℝ) [IsProbabilityMeasure α] [NoAtoms α]
    (Q k : ℕ) (hk : k < Q) : payerShare α α Q k ≠ 1 := by
  rw [payerShare_at_equal α Q k hk.le]
  have hQ : (0 : ℝ) < (Q : ℝ) + 1 := by positivity
  have hkQ : (k : ℝ) + 1 < (Q : ℝ) + 1 := by
    have : (k : ℝ) < Q := by exact_mod_cast hk
    linarith
  rw [Ne, div_eq_one_iff_eq hQ.ne']
  exact hkQ.ne

end Controls

end EntryContestShare
