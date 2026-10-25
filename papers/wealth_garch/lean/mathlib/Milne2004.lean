import Mathlib

namespace Milne2004

/-- The cost of an issue: a fixed χ, whatever the amount κ issued. -/
def issueCost (χ _κ : ℝ) : ℝ := χ

theorem issueCost_const {χ κ₁ κ₂ : ℝ} : issueCost χ κ₁ = issueCost χ κ₂ := rfl

/-- On audit below the minimum the bank recapitalises from C to C* at par and pays χ.
    Where value has slope one between C and C*, the loss on audit is χ alone, the same at every
    shortfall: no part of it grows with C* - C. -/
theorem audit_loss_is_fixed {V : ℝ → ℝ} {C Cs χ : ℝ} (h : V Cs - V C = Cs - C) :
    V C - (V Cs - χ - (Cs - C)) = χ := by
  linarith

theorem control_free_issue_no_loss {V : ℝ → ℝ} {C Cs : ℝ} (h : V Cs - V C = Cs - C) :
    V C - (V Cs - 0 - (Cs - C)) = 0 := by
  linarith

end Milne2004
