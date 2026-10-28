import Mathlib

namespace FedericoEtAl

/-- The level of the bad-state dividend payout share at or below which the bank pays nothing in the bad
    state: λ₂ / (λ₂ + δ) (their Remark 1). -/
noncomputable def banCutoff (l δ : ℝ) : ℝ := l / (l + δ)

/-- The cutoff rises with the probability l of leaving the bad state. -/
theorem banCutoff_rises_in_l {l₁ l₂ δ : ℝ} (hl₁ : 0 < l₁) (hδ : 0 < δ) (h : l₁ < l₂) :
    banCutoff l₁ δ < banCutoff l₂ δ := by
  unfold banCutoff
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]
  nlinarith

/-- The cutoff falls with the manager's impatience δ. -/
theorem banCutoff_falls_in_delta {l δ₁ δ₂ : ℝ} (hl : 0 < l) (hδ₁ : 0 < δ₁) (h : δ₁ < δ₂) :
    banCutoff l δ₂ < banCutoff l δ₁ := by
  unfold banCutoff
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]
  nlinarith

/-- The shareholders' payoff on liquidation, the buffer Γ floored at zero. -/
noncomputable def liquidationPayoff (Γ : ℝ) : ℝ := max Γ 0

theorem liquidationPayoff_nonneg {Γ : ℝ} : 0 ≤ liquidationPayoff Γ := le_max_right _ _

/-- With no impatience the cutoff is one, so any payout share below one is a ban in the bad state. -/
theorem control_no_impatience_cutoff_is_one {l : ℝ} (hl : 0 < l) : banCutoff l 0 = 1 := by
  unfold banCutoff
  simp [div_self hl.ne']

end FedericoEtAl
