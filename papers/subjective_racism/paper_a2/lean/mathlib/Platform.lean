import Mathlib

namespace Platform

noncomputable section

/-! ### Row M15. The platform's admission margin and the host spillover -/

/-- The predecessor's first-order condition for admitting one outsider member.
`W` is the admittee's willingness to pay, `n` the number of host clients, `α`
the weight on objective quality, `cO` and `cI` the two standings and `A` the
room size. -/
def marg (W n α cO cI A : ℝ) : ℝ := W + n * (1 - α) * (cO - cI) / A

/-- Profit from an admission rate `σ`, linear in `σ` as the predecessor's
derivative is constant. -/
def profit (W n α cO cI A σ : ℝ) : ℝ := σ * marg W n α cO cI A

/-- The admission threshold, the outsider standing at which the margin is 0. -/
def cAdm (W n α cI A : ℝ) : ℝ := cI - W * A / (n * (1 - α))

theorem marg_neg_iff {W n α cO cI A : ℝ} (hA : 0 < A) :
    marg W n α cO cI A < 0 ↔ W * A < n * (1 - α) * (cI - cO) := by
  unfold marg
  rw [← sub_neg, show W + n * (1 - α) * (cO - cI) / A - 0 = (W * A - n * (1 - α) * (cI - cO)) / A
    by field_simp; ring]
  rw [div_neg_iff]
  constructor
  · rintro (⟨_, h⟩ | ⟨h, _⟩)
    · linarith
    · linarith
  · intro h; right; exact ⟨by linarith, hA⟩

/-- The margin is negative exactly when the outsider stands below the
admission threshold. -/
theorem marg_neg_iff_below {W n α cO cI A : ℝ} (hA : 0 < A) (hn : 0 < n * (1 - α)) :
    marg W n α cO cI A < 0 ↔ cO < cAdm W n α cI A := by
  rw [marg_neg_iff hA]
  unfold cAdm
  rw [lt_sub_iff_add_lt, ← lt_sub_iff_add_lt', div_lt_iff₀ hn]
  constructor <;> intro h <;> linarith [mul_comm (n * (1 - α)) (cI - cO)]

/-- With a negative margin, every positive admission rate earns less than
exclusion. -/
theorem exclude_of_neg {W n α cO cI A : ℝ} (h : marg W n α cO cI A < 0) :
    ∀ σ, 0 < σ → profit W n α cO cI A σ < profit W n α cO cI A 0 := by
  intro σ hσ
  unfold profit
  rw [zero_mul]
  exact mul_neg_of_pos_of_neg hσ h

/-- Control: with a positive margin, full admission earns more than exclusion. -/
theorem control_admit_of_pos {W n α cO cI A : ℝ} (h : 0 < marg W n α cO cI A) :
    profit W n α cO cI A 0 < profit W n α cO cI A 1 := by
  unfold profit
  rw [zero_mul, one_mul]
  exact h

/-- The host's effective standing once a fraction `η` of the room is outsiders
at the admission threshold, with spillover sensitivity `θ`. -/
def cEff (cI cE θ η : ℝ) : ℝ := cI - θ * η * (cI - cE)

/-- The critical outsider fraction. -/
def etaCrit (cI cM cE θ : ℝ) : ℝ := (cI - cM) / (θ * (cI - cE))

/-- The host tips exactly when the outsider fraction exceeds the critical one. -/
theorem tip_iff {cI cM cE θ η : ℝ} (hθ : 0 < θ) (hgap : 0 < cI - cE) :
    cEff cI cE θ η < cM ↔ etaCrit cI cM cE θ < η := by
  unfold cEff etaCrit
  rw [div_lt_iff₀ (mul_pos hθ hgap)]
  constructor <;> intro h <;> nlinarith

/-- The critical fraction falls in the spillover sensitivity when the host is
above its tipping point. -/
theorem etaCrit_anti {cI cM cE θ₁ θ₂ : ℝ} (hnum : 0 < cI - cM) (hgap : 0 < cI - cE)
    (h1 : 0 < θ₁) (h12 : θ₁ < θ₂) :
    etaCrit cI cM cE θ₂ < etaCrit cI cM cE θ₁ := by
  unfold etaCrit
  apply div_lt_div_of_pos_left hnum (mul_pos h1 hgap)
  exact mul_lt_mul_of_pos_right h12 hgap

/-- Control: with the host already below its tipping point the critical
fraction rises in the sensitivity. -/
theorem control_etaCrit_sign {cI cM cE θ₁ θ₂ : ℝ} (hnum : cI - cM < 0) (hgap : 0 < cI - cE)
    (h1 : 0 < θ₁) (h12 : θ₁ < θ₂) :
    etaCrit cI cM cE θ₁ < etaCrit cI cM cE θ₂ := by
  unfold etaCrit
  have hp1 := mul_pos h1 hgap
  have hlt : 1 / (θ₂ * (cI - cE)) < 1 / (θ₁ * (cI - cE)) :=
    one_div_lt_one_div_of_lt hp1 (mul_lt_mul_of_pos_right h12 hgap)
  rw [div_eq_mul_one_div, div_eq_mul_one_div (cI - cM) (θ₂ * _)]
  exact mul_lt_mul_of_neg_left hlt hnum

/-! ### Refuted. The admission threshold need not lie below the tipping point -/

/-- For every tipping point below 1 and every room, there is a willingness to
pay that puts the admission threshold above it, and one that puts it below.
So `prop:tokenism` of the predecessor does not follow from its premises. -/
theorem tokenism_not_implied {cM n α A : ℝ} (hM : cM < 1) (hn : 0 < n * (1 - α)) (hA : 0 < A) :
    (∃ W, 0 < W ∧ cM < cAdm W n α 1 A) ∧ (∃ W, 0 < W ∧ cAdm W n α 1 A < cM) := by
  have hM' : 0 < 1 - cM := by linarith
  have hA' : A ≠ 0 := hA.ne'
  unfold cAdm
  generalize hk : n * (1 - α) = k at hn ⊢
  have hk' : k ≠ 0 := hn.ne'
  constructor
  · refine ⟨(1 - cM) * k / (2 * A), div_pos (mul_pos hM' hn) (by positivity), ?_⟩
    rw [show (1 - cM) * k / (2 * A) * A / k = (1 - cM) / 2 by field_simp]
    linarith
  · refine ⟨2 * (1 - cM) * k / A, div_pos (mul_pos (mul_pos two_pos hM') hn) hA, ?_⟩
    rw [show 2 * (1 - cM) * k / A * A / k = 2 * (1 - cM) by field_simp]
    linarith

end

end Platform
