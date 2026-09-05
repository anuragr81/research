/-
# Proposition IMM (exact immunity), Section 4.1

  (a) On a single cue, the Jeffrey posterior equals the matched Bayesian
      posterior identically, for every prior and every impression.
  (b) With two cues but `c = 0` (independent attributes),
      `Pᴶ_AB = Pᴶ_BA = Pᴮ = q ⊗ r` exactly.

Both are exact identities: no expansion in `c` is involved.
-/
import JeffreyOrder.Basic

namespace JeffreyOrder
open Mat

variable {α β c q₀ r₀ : ℝ}

/-! ### Closed forms for one Jeffrey step on the prior -/

/-- A Jeffrey step on `A` applied to the prior, in closed form.  The prior's
`A`-marginal is `α` regardless of `c`, so the step divides row `i` by `αᵢ`. -/
theorem jeffreyA_prior (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) :
    jeffreyA (prior α β c) q₀ =
      ⟨q₀ * (α * β + c) / α, q₀ * (α * (1 - β) - c) / α,
       (1 - q₀) * ((1 - α) * β - c) / (1 - α),
       (1 - q₀) * ((1 - α) * (1 - β) + c) / (1 - α)⟩ := by
  simp only [jeffreyA, prior_mA0, prior_mA1]
  ext <;> simp only [prior]

/-- A Jeffrey step on `B` applied to the prior, in closed form. -/
theorem jeffreyB_prior (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    jeffreyB (prior α β c) r₀ =
      ⟨r₀ * (α * β + c) / β, (1 - r₀) * (α * (1 - β) - c) / (1 - β),
       r₀ * ((1 - α) * β - c) / β,
       (1 - r₀) * ((1 - α) * (1 - β) + c) / (1 - β)⟩ := by
  simp only [jeffreyB, prior_mB0, prior_mB1]
  ext <;> simp only [prior]

/-! ### Part (a): a single cue -/

/-- The matched likelihood's normaliser is exactly `1`: the step the proof of
Proposition IMM turns on. -/
theorem bayesAW_total (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) :
    (bayesAW (prior α β c) α q₀).total = 1 := by
  simp only [bayesAW, Mat.total, prior]
  field_simp
  ring

/-- **Proposition IMM (a)**: on a single cue the Jeffrey posterior *is* the
matched Bayesian posterior, identically in `(α, β, c, q₀)`. -/
theorem propIMM_single_cue (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) :
    bayesA (prior α β c) α q₀ = jeffreyA (prior α β c) q₀ := by
  rw [bayesA, Mat.normalize_of_total_one (bayesAW_total (β := β) (c := c) (q₀ := q₀) hα hα'),
    jeffreyA_prior hα hα']
  ext <;> simp only [bayesAW, prior] <;> field_simp <;> ring

/-- The same for a single cue on `B`: the `B`-side reweighting also has
normaliser `1`, and the resulting posterior is the Jeffrey step on `B`. -/
theorem bayesBW_total (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    (Mat.mk ((prior α β c).a00 * (r₀ / β)) ((prior α β c).a01 * ((1 - r₀) / (1 - β)))
      ((prior α β c).a10 * (r₀ / β)) ((prior α β c).a11 * ((1 - r₀) / (1 - β)))).total = 1 := by
  simp only [Mat.total, prior]
  field_simp
  ring

theorem propIMM_single_cue_B (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    (Mat.mk ((prior α β c).a00 * (r₀ / β)) ((prior α β c).a01 * ((1 - r₀) / (1 - β)))
      ((prior α β c).a10 * (r₀ / β))
      ((prior α β c).a11 * ((1 - r₀) / (1 - β)))).normalize
      = jeffreyB (prior α β c) r₀ := by
  rw [Mat.normalize_of_total_one (bayesBW_total (α := α) (c := c) (r₀ := r₀) hβ hβ'),
    jeffreyB_prior hβ hβ']
  ext <;> simp only [prior] <;> field_simp <;> ring

/-! ### Part (b): two cues at `c = 0` -/

theorem PJab_at_zero (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    PJab α β 0 q₀ r₀ = indep q₀ r₀ := by
  have hstep := jeffreyA_prior (β := β) (c := 0) (q₀ := q₀) hα hα'
  have h0 : (jeffreyA (prior α β 0) q₀).mB0 = β := by
    rw [hstep]; simp only [Mat.mB0]; field_simp; ring
  have h1 : (jeffreyA (prior α β 0) q₀).mB1 = 1 - β := by
    rw [hstep]; simp only [Mat.mB1]; field_simp; ring
  ext <;>
    (simp only [PJab, jeffreyB, h0, h1]; rw [hstep]; simp only [indep]
     field_simp; ring)

theorem PJba_at_zero (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    PJba α β 0 q₀ r₀ = indep q₀ r₀ := by
  have hstep := jeffreyB_prior (α := α) (c := 0) (r₀ := r₀) hβ hβ'
  have h0 : (jeffreyB (prior α β 0) r₀).mA0 = α := by
    rw [hstep]; simp only [Mat.mA0]; field_simp; ring
  have h1 : (jeffreyB (prior α β 0) r₀).mA1 = 1 - α := by
    rw [hstep]; simp only [Mat.mA1]; field_simp; ring
  ext <;>
    (simp only [PJba, jeffreyA, h0, h1]; rw [hstep]; simp only [indep]
     field_simp; ring)

theorem PB_at_zero (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    PB α β 0 q₀ r₀ = indep q₀ r₀ := by
  have ht : (bayesW (prior α β 0) α β q₀ r₀).total = 1 := by
    simp only [bayesW, Mat.total, prior]; field_simp; ring
  rw [PB, bayes, Mat.normalize_of_total_one ht]
  ext <;> simp only [bayesW, prior, indep] <;> field_simp <;> ring

/-- **Proposition IMM (b)**: with two cues and `c = 0` the two reading sequences
and the benchmark coincide exactly. -/
theorem propIMM_indep (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    PJab α β 0 q₀ r₀ = PJba α β 0 q₀ r₀ ∧ PJba α β 0 q₀ r₀ = PB α β 0 q₀ r₀ := by
  constructor
  · rw [PJab_at_zero hα hα' hβ hβ', PJba_at_zero hα hα' hβ hβ']
  · rw [PJba_at_zero hα hα' hβ hβ', PB_at_zero hα hα' hβ hβ']

/-- At `c = 0` there is no gap at all. -/
theorem propIMM_no_gap (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    (PJab α β 0 q₀ r₀).sub (PB α β 0 q₀ r₀) = ⟨0, 0, 0, 0⟩ := by
  rw [PJab_at_zero hα hα' hβ hβ', PB_at_zero hα hα' hβ hβ']
  ext <;> simp [Mat.sub]

/-- ... and no sequence effect. -/
theorem propIMM_no_sequence_effect (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    (PJab α β 0 q₀ r₀).sub (PJba α β 0 q₀ r₀) = ⟨0, 0, 0, 0⟩ := by
  rw [PJab_at_zero hα hα' hβ hβ', PJba_at_zero hα hα' hβ hβ']
  ext <;> simp [Mat.sub]

/-- The independent posterior carries no association. -/
@[simp] theorem indep_assoc (q₀ r₀ : ℝ) : (indep q₀ r₀).assoc = 0 := by
  simp only [indep, Mat.assoc]; ring

end JeffreyOrder
