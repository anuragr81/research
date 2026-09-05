/-
# Proposition DEC (belief-level decoupling), Section 5.2.1

The mechanism behind Proposition DEC is Lemma SEP: *every* updating route in the
model composes to a **separable** (rank-one) reweighting of the prior, and a
separable reweighting can only RESCALE an association, never shift it.

This file formalises that mechanism exactly -- not as an `O(c²)` statement but
as the algebraic identities it rests on:

  `sep_assoc`       : `assoc (sep g₀ g₁ h₀ h₁ Q) = g₀g₁h₀h₁ · assoc Q`
  `sep_oddsRatio`   : a separable reweighting leaves the odds ratio unchanged
  `PJab_sep`, `PJba_sep`, `PB_sep`
                    : each of the two routes and the benchmark IS a separable
                      reweighting of the prior
  `assoc_PJab`, ... : hence each maps `assoc P = c` to a positive multiple of `c`
  `oddsRatio_PJab_eq_prior`, ...
                    : and all three carry the prior's odds ratio *exactly in c*,
                      which is the exact form of the remark following the proof
                      of Proposition DEC in the manuscript.
-/
import JeffreyOrder.PropIMM

namespace JeffreyOrder
open Mat

variable {α β c q₀ r₀ g₀ g₁ h₀ h₁ : ℝ} {Q : Mat}

/-- A separable (rank-one) reweighting `Q'ᵢⱼ = gᵢ hⱼ Qᵢⱼ`, unnormalised. -/
def sep (g₀ g₁ h₀ h₁ : ℝ) (Q : Mat) : Mat :=
  ⟨g₀ * h₀ * Q.a00, g₀ * h₁ * Q.a01, g₁ * h₀ * Q.a10, g₁ * h₁ * Q.a11⟩

/-- **The multiplier identity.**  A separable reweighting multiplies the
association by `g₀g₁h₀h₁`; it can only rescale it, never shift it additively. -/
theorem sep_assoc (g₀ g₁ h₀ h₁ : ℝ) (Q : Mat) :
    (sep g₀ g₁ h₀ h₁ Q).assoc = g₀ * g₁ * h₀ * h₁ * Q.assoc := by
  simp only [sep, Mat.assoc]; ring

/-- In particular, a separable reweighting of an independent law stays
independent: it cannot manufacture association out of nothing. -/
theorem sep_assoc_zero (g₀ g₁ h₀ h₁ : ℝ) (Q : Mat) (hQ : Q.assoc = 0) :
    (sep g₀ g₁ h₀ h₁ Q).assoc = 0 := by
  rw [sep_assoc, hQ, mul_zero]

/-- **Exact odds-ratio invariance.**  The factor `g₀g₁h₀h₁` cancels in the odds
ratio, so a separable reweighting leaves it untouched. -/
theorem sep_oddsRatio (hg₀ : g₀ ≠ 0) (hg₁ : g₁ ≠ 0) (hh₀ : h₀ ≠ 0) (hh₁ : h₁ ≠ 0)
    (h01 : Q.a01 ≠ 0) (h10 : Q.a10 ≠ 0) :
    (sep g₀ g₁ h₀ h₁ Q).oddsRatio = Q.oddsRatio := by
  simp only [sep, Mat.oddsRatio]
  field_simp

/-- Normalising does not change the odds ratio either. -/
theorem normalize_oddsRatio (hZ : Q.total ≠ 0) (h01 : Q.a01 ≠ 0) (h10 : Q.a10 ≠ 0) :
    Q.normalize.oddsRatio = Q.oddsRatio := by
  simp only [Mat.normalize, Mat.oddsRatio]
  field_simp

/-! ### Every route is a separable reweighting of the prior -/

/-- The `A`-first route is the separable reweighting with row factors
`qᵢ / αᵢ` and column factors `rⱼ / mⱼ`, where `mⱼ` is the `B`-marginal *after*
the `A`-step. -/
theorem PJab_sep :
    PJab α β c q₀ r₀ =
      sep (q₀ / α) ((1 - q₀) / (1 - α))
          (r₀ / (jeffreyA (prior α β c) q₀).mB0)
          ((1 - r₀) / (jeffreyA (prior α β c) q₀).mB1)
          (prior α β c) := by
  ext <;>
    simp only [PJab, jeffreyB, jeffreyA, prior_mA0, prior_mA1, sep] <;> ring

/-- The `B`-first route is the separable reweighting with column factors
`rⱼ / βⱼ` and row factors `qᵢ / mᵢ`, `mᵢ` the `A`-marginal after the `B`-step. -/
theorem PJba_sep :
    PJba α β c q₀ r₀ =
      sep (q₀ / (jeffreyB (prior α β c) r₀).mA0)
          ((1 - q₀) / (jeffreyB (prior α β c) r₀).mA1)
          (r₀ / β) ((1 - r₀) / (1 - β))
          (prior α β c) := by
  ext <;>
    simp only [PJba, jeffreyA, jeffreyB, prior_mB0, prior_mB1, sep] <;> ring

/-- Normalising divides the association by the square of the total mass. -/
theorem normalize_assoc (hZ : Q.total ≠ 0) : Q.normalize.assoc = Q.assoc / Q.total ^ 2 := by
  simp only [Mat.normalize, Mat.assoc]
  field_simp

/-- The benchmark's Bayes-factor reweighting is separable, with `gᵢ = qᵢ/αᵢ`
and `hⱼ = rⱼ/βⱼ`.  Unlike the one-cue case (Proposition IMM), the two-cue
normaliser is not `1`, so the benchmark is this reweighting *normalised*. -/
theorem bayesW_eq_sep :
    bayesW (prior α β c) α β q₀ r₀ =
      sep (q₀ / α) ((1 - q₀) / (1 - α)) (r₀ / β) ((1 - r₀) / (1 - β)) (prior α β c) := by
  ext <;> simp only [bayesW, sep] <;> ring

theorem PB_sep :
    PB α β c q₀ r₀ =
      (sep (q₀ / α) ((1 - q₀) / (1 - α)) (r₀ / β) ((1 - r₀) / (1 - β)) (prior α β c)).normalize := by
  rw [PB, bayes, bayesW_eq_sep]

/-! ### Consequences: association is rescaled, odds ratio is preserved -/

/-- The `A`-first route maps `assoc P = c` to a multiple of `c`.  Nothing in the
route can shift the association additively. -/
theorem assoc_PJab :
    (PJab α β c q₀ r₀).assoc =
      (q₀ / α) * ((1 - q₀) / (1 - α))
        * (r₀ / (jeffreyA (prior α β c) q₀).mB0)
        * ((1 - r₀) / (jeffreyA (prior α β c) q₀).mB1) * c := by
  rw [PJab_sep, sep_assoc, prior_assoc]

theorem assoc_PJba :
    (PJba α β c q₀ r₀).assoc =
      (q₀ / (jeffreyB (prior α β c) r₀).mA0) * ((1 - q₀) / (jeffreyB (prior α β c) r₀).mA1)
        * (r₀ / β) * ((1 - r₀) / (1 - β)) * c := by
  rw [PJba_sep, sep_assoc, prior_assoc]

theorem assoc_PB
    (hZ : (sep (q₀ / α) ((1 - q₀) / (1 - α)) (r₀ / β) ((1 - r₀) / (1 - β))
            (prior α β c)).total ≠ 0) :
    (PB α β c q₀ r₀).assoc =
      (q₀ / α) * ((1 - q₀) / (1 - α)) * (r₀ / β) * ((1 - r₀) / (1 - β)) * c
        / (sep (q₀ / α) ((1 - q₀) / (1 - α)) (r₀ / β) ((1 - r₀) / (1 - β))
            (prior α β c)).total ^ 2 := by
  rw [PB_sep, normalize_assoc hZ, sep_assoc, prior_assoc]

/-- **Corollary (independence is preserved).**  At `c = 0` every route leaves
the association at zero -- the exact form of Proposition IMM's second half seen
through the separability mechanism. -/
theorem assoc_PJab_of_indep : (PJab α β 0 q₀ r₀).assoc = 0 := by
  rw [assoc_PJab]; ring

/-- **Exact odds-ratio invariance along the `A`-first route.**  This is the
manuscript's remark after the proof of Proposition DEC: for the odds-ratio form
of association the departure from the benchmark is exactly zero, not `O(c²)`. -/
theorem oddsRatio_PJab_eq_prior (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hq : q₀ ≠ 0) (hq' : (1:ℝ) - q₀ ≠ 0) (hr : r₀ ≠ 0) (hr' : (1:ℝ) - r₀ ≠ 0)
    (hm0 : (jeffreyA (prior α β c) q₀).mB0 ≠ 0)
    (hm1 : (jeffreyA (prior α β c) q₀).mB1 ≠ 0)
    (h01 : (prior α β c).a01 ≠ 0) (h10 : (prior α β c).a10 ≠ 0) :
    (PJab α β c q₀ r₀).oddsRatio = (prior α β c).oddsRatio := by
  rw [PJab_sep]
  exact sep_oddsRatio (div_ne_zero hq hα) (div_ne_zero hq' hα')
    (div_ne_zero hr hm0) (div_ne_zero hr' hm1) h01 h10

theorem oddsRatio_PJba_eq_prior (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0)
    (hq : q₀ ≠ 0) (hq' : (1:ℝ) - q₀ ≠ 0) (hr : r₀ ≠ 0) (hr' : (1:ℝ) - r₀ ≠ 0)
    (hm0 : (jeffreyB (prior α β c) r₀).mA0 ≠ 0)
    (hm1 : (jeffreyB (prior α β c) r₀).mA1 ≠ 0)
    (h01 : (prior α β c).a01 ≠ 0) (h10 : (prior α β c).a10 ≠ 0) :
    (PJba α β c q₀ r₀).oddsRatio = (prior α β c).oddsRatio := by
  rw [PJba_sep]
  exact sep_oddsRatio (div_ne_zero hq hm0) (div_ne_zero hq' hm1)
    (div_ne_zero hr hβ) (div_ne_zero hr' hβ') h01 h10

theorem oddsRatio_PB_eq_prior (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0)
    (hq : q₀ ≠ 0) (hq' : (1:ℝ) - q₀ ≠ 0) (hr : r₀ ≠ 0) (hr' : (1:ℝ) - r₀ ≠ 0)
    (h01 : (prior α β c).a01 ≠ 0) (h10 : (prior α β c).a10 ≠ 0)
    (hZ : (sep (q₀ / α) ((1 - q₀) / (1 - α)) (r₀ / β) ((1 - r₀) / (1 - β))
            (prior α β c)).total ≠ 0)
    (hs01 : (sep (q₀ / α) ((1 - q₀) / (1 - α)) (r₀ / β) ((1 - r₀) / (1 - β))
            (prior α β c)).a01 ≠ 0)
    (hs10 : (sep (q₀ / α) ((1 - q₀) / (1 - α)) (r₀ / β) ((1 - r₀) / (1 - β))
            (prior α β c)).a10 ≠ 0) :
    (PB α β c q₀ r₀).oddsRatio = (prior α β c).oddsRatio := by
  rw [PB_sep, normalize_oddsRatio hZ hs01 hs10]
  exact sep_oddsRatio (div_ne_zero hq hα) (div_ne_zero hq' hα')
    (div_ne_zero hr hβ) (div_ne_zero hr' hβ') h01 h10

/-- **The odds-ratio departure from the benchmark is exactly zero.**  Both
reading sequences carry exactly the benchmark's odds ratio, identically in `c`.
Order effects live in the reset marginals, not in the preserved association. -/
theorem oddsRatio_gap_eq_zero (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0)
    (hq : q₀ ≠ 0) (hq' : (1:ℝ) - q₀ ≠ 0) (hr : r₀ ≠ 0) (hr' : (1:ℝ) - r₀ ≠ 0)
    (hm0 : (jeffreyA (prior α β c) q₀).mB0 ≠ 0)
    (hm1 : (jeffreyA (prior α β c) q₀).mB1 ≠ 0)
    (h01 : (prior α β c).a01 ≠ 0) (h10 : (prior α β c).a10 ≠ 0)
    (hZ : (sep (q₀ / α) ((1 - q₀) / (1 - α)) (r₀ / β) ((1 - r₀) / (1 - β))
            (prior α β c)).total ≠ 0)
    (hs01 : (sep (q₀ / α) ((1 - q₀) / (1 - α)) (r₀ / β) ((1 - r₀) / (1 - β))
            (prior α β c)).a01 ≠ 0)
    (hs10 : (sep (q₀ / α) ((1 - q₀) / (1 - α)) (r₀ / β) ((1 - r₀) / (1 - β))
            (prior α β c)).a10 ≠ 0) :
    (PJab α β c q₀ r₀).oddsRatio - (PB α β c q₀ r₀).oddsRatio = 0 := by
  rw [oddsRatio_PJab_eq_prior hα hα' hq hq' hr hr' hm0 hm1 h01 h10,
    oddsRatio_PB_eq_prior hα hα' hβ hβ' hq hq' hr hr' h01 h10 hZ hs01 hs10, sub_self]

end JeffreyOrder
