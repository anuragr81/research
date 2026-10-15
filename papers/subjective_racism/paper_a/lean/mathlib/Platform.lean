import Mathlib

namespace Platform

noncomputable section

/-! ### Row M15. The platform's admission margin -/

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

end

end Platform
