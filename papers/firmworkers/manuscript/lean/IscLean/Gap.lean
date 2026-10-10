import IscLean.Ladder

namespace Isc

theorem exp_gap_strictMono {u₁ d s₁ s₂ : ℝ} (hu₁ : 0 ≤ u₁) (hd : 0 < d) (hs₂ : 0 < s₂)
    (h : s₂ < s₁) :
    Real.exp (s₂ * u₁) * (Real.exp (s₂ * d) - 1) < Real.exp (s₁ * u₁) * (Real.exp (s₁ * d) - 1) := by
  have hA : Real.exp (s₂ * u₁) ≤ Real.exp (s₁ * u₁) :=
    Real.exp_le_exp.mpr (mul_le_mul_of_nonneg_right h.le hu₁)
  have hApos : 0 < Real.exp (s₂ * u₁) := Real.exp_pos _
  have hC : 0 < Real.exp (s₂ * d) - 1 := by
    have : 1 < Real.exp (s₂ * d) := Real.one_lt_exp_iff.mpr (mul_pos hs₂ hd)
    linarith
  have hCD : Real.exp (s₂ * d) - 1 < Real.exp (s₁ * d) - 1 := by
    have : Real.exp (s₂ * d) < Real.exp (s₁ * d) :=
      Real.exp_lt_exp.mpr (mul_lt_mul_of_pos_right h hd)
    linarith
  calc Real.exp (s₂ * u₁) * (Real.exp (s₂ * d) - 1)
      < Real.exp (s₂ * u₁) * (Real.exp (s₁ * d) - 1) := mul_lt_mul_of_pos_left hCD hApos
    _ ≤ Real.exp (s₁ * u₁) * (Real.exp (s₁ * d) - 1) :=
        mul_le_mul_of_nonneg_right hA (hC.trans hCD).le

theorem pareto_gap_strictAnti_tail {x0 a₁ a₂ q₁ q₂ : ℝ} (hx0 : 0 < x0) (ha₁ : 0 < a₁)
    (h12 : a₁ < a₂) (hq₁ : 0 ≤ q₁) (hq : q₁ < q₂) (hq₂ : q₂ < 1) :
    pareto x0 a₂ q₂ - pareto x0 a₂ q₁ < pareto x0 a₁ q₂ - pareto x0 a₁ q₁ := by
  have ha₂ : 0 < a₂ := ha₁.trans h12
  have ht₁ : 0 < 1 - q₁ := by linarith
  have ht₂ : 0 < 1 - q₂ := by linarith
  have hlog₁ : Real.log (1 - q₁) ≤ 0 := Real.log_nonpos ht₁.le (by linarith)
  have hlog : Real.log (1 - q₂) < Real.log (1 - q₁) := Real.log_lt_log ht₂ (by linarith)
  have hs₂ : 0 < 1 / a₂ := by positivity
  have hs : 1 / a₂ < 1 / a₁ := one_div_lt_one_div_of_lt ha₁ h12
  have key := exp_gap_strictMono (u₁ := -Real.log (1 - q₁))
    (d := Real.log (1 - q₁) - Real.log (1 - q₂)) (by linarith) (by linarith) hs₂ hs
  have rw₁ : ∀ s : ℝ, Real.exp (s * -Real.log (1 - q₁)) *
      (Real.exp (s * (Real.log (1 - q₁) - Real.log (1 - q₂))) - 1)
      = (1 - q₂) ^ (-s) - (1 - q₁) ^ (-s) := by
    intro s
    rw [Real.rpow_def_of_pos ht₂, Real.rpow_def_of_pos ht₁, mul_sub, mul_one, ← Real.exp_add]
    congr 2 <;> ring
  rw [rw₁, rw₁] at key
  unfold pareto
  have hk := mul_lt_mul_of_pos_left key hx0
  linarith [hk, mul_sub x0 ((1 - q₂) ^ (-(1 / a₂))) ((1 - q₁) ^ (-(1 / a₂))),
    mul_sub x0 ((1 - q₂) ^ (-(1 / a₁))) ((1 - q₁) ^ (-(1 / a₁)))]

theorem control_gap_needs_a_pos :
    ¬ (pareto 1 1 (1 / 2) - pareto 1 1 0 < pareto 1 (-1) (1 / 2) - pareto 1 (-1) 0) := by
  unfold pareto
  norm_num [Real.rpow_neg_one, Real.rpow_one]

end Isc
