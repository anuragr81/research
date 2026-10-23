/-
================================================================================
  Løkka and Zervos (2008), Optimal dividend and issuance of equity policies in
  the presence of proportional costs
  Companion to lit/lokka_zervos_2008.
================================================================================

The roots α₁, α₂ of (3.5)-(3.6) are those of `PeuraKeppo` with r for ρ. The
file proves the dividend barrier x** of (3.10) and its smooth-pasting
identity, the equivalence x** > 0 ⇔ μ > 0 of (3.12), the coefficients (4.10)
against (4.7)-(4.8), the equivalence of (4.6) and (4.9) under (4.10), and the
concavity sign (4.14). Notation: a = α₁, b = α₂. The verification theorem
(Theorem 5.1) is not formalised.
-/

import PeuraKeppo

open Real

namespace LokkaZervos

noncomputable def xss (a b : ℝ) : ℝ := -(log (a ^ 2) - log (b ^ 2)) / (a - b)

noncomputable def c1 (a b x : ℝ) : ℝ := -b / (a * (a - b)) * exp (-(a * x))

noncomputable def c2 (a b x : ℝ) : ℝ := a / (b * (a - b)) * exp (-(b * x))

theorem roots_sum {μ σ r : ℝ} (hσ : σ ≠ 0) :
    PeuraKeppo.dplus μ σ r + PeuraKeppo.dminus μ σ r = -2 * μ / σ ^ 2 := by
  unfold PeuraKeppo.dplus PeuraKeppo.dminus
  field_simp
  ring

theorem xss_exp {a b : ℝ} (ha : 0 < a) (hb : b < 0) : exp ((a - b) * xss a b) = b ^ 2 / a ^ 2 := by
  have hab : a - b ≠ 0 := by linarith
  have ha2 : 0 < a ^ 2 := by positivity
  have hb2 : 0 < b ^ 2 := by nlinarith
  have h : (a - b) * xss a b = log (b ^ 2 / a ^ 2) := by
    unfold xss
    rw [log_div hb2.ne' ha2.ne']
    field_simp
    ring
  rw [h, exp_log (div_pos hb2 ha2)]

theorem barrier_paste {a b : ℝ} (ha : 0 < a) (hb : b < 0) :
    a ^ 2 * exp (a * xss a b) = b ^ 2 * exp (b * xss a b) := by
  have h := xss_exp ha hb
  have hsplit : exp (a * xss a b) = exp (b * xss a b) * exp ((a - b) * xss a b) := by
    rw [← exp_add]; ring_nf
  rw [hsplit, h]
  have : a ^ 2 ≠ 0 := by positivity
  field_simp

theorem xss_pos_iff {μ σ a b : ℝ} (hσ : σ ≠ 0) (ha : 0 < a) (hb : b < 0)
    (hsum : a + b = -2 * μ / σ ^ 2) : 0 < xss a b ↔ 0 < μ := by
  have hab : 0 < a - b := by linarith
  have hσ2 : 0 < σ ^ 2 := by positivity
  have hb2 : 0 < b ^ 2 := by nlinarith
  have key : 0 < xss a b ↔ a ^ 2 < b ^ 2 := by
    unfold xss
    rw [show -(log (a ^ 2) - log (b ^ 2)) = log (b ^ 2) - log (a ^ 2) by ring, div_pos_iff_of_pos_right hab,
      sub_pos, log_lt_log_iff (by positivity) hb2]
  rw [key]
  have hsq : a ^ 2 < b ^ 2 ↔ a < -b := by
    constructor
    · intro h; nlinarith
    · intro h; nlinarith
  rw [hsq]
  constructor
  · intro h
    have : a + b < 0 := by linarith
    rw [hsum] at this
    by_contra hμ
    push Not at hμ
    have : 0 ≤ -2 * μ / σ ^ 2 := div_nonneg (by linarith) hσ2.le
    linarith
  · intro h
    have : -2 * μ / σ ^ 2 < 0 := div_neg_of_neg_of_pos (by linarith) hσ2
    linarith

theorem coeffs_paste {a b x : ℝ} (ha : 0 < a) (hb : b < 0) :
    c1 a b x * a * exp (a * x) + c2 a b x * b * exp (b * x) = 1 ∧
      c1 a b x * a ^ 2 * exp (a * x) + c2 a b x * b ^ 2 * exp (b * x) = 0 := by
  have hab : a - b ≠ 0 := by linarith
  have e1 : exp (-(a * x)) * exp (a * x) = 1 := by rw [← exp_add]; simp
  have e2 : exp (-(b * x)) * exp (b * x) = 1 := by rw [← exp_add]; simp
  unfold c1 c2
  have ha0 : a ≠ 0 := ha.ne'
  have hb0 : b ≠ 0 := hb.ne
  constructor
  · calc -b / (a * (a - b)) * exp (-(a * x)) * a * exp (a * x) + a / (b * (a - b)) * exp (-(b * x)) * b * exp (b * x)
        = -b / (a - b) * (exp (-(a * x)) * exp (a * x)) + a / (a - b) * (exp (-(b * x)) * exp (b * x)) := by
          field_simp
      _ = 1 := by rw [e1, e2]; field_simp; ring
  · calc -b / (a * (a - b)) * exp (-(a * x)) * a ^ 2 * exp (a * x) + a / (b * (a - b)) * exp (-(b * x)) * b ^ 2 * exp (b * x)
        = -(a * b) / (a - b) * (exp (-(a * x)) * exp (a * x)) + a * b / (a - b) * (exp (-(b * x)) * exp (b * x)) := by
          field_simp
      _ = 0 := by rw [e1, e2]; field_simp; ring

theorem eq_49 {a b x : ℝ} (ha : 0 < a) (hb : b < 0) :
    c1 a b x * a + c2 a b x * b = (a * exp (-(b * x)) - b * exp (-(a * x))) / (a - b) := by
  have hab : a - b ≠ 0 := by linarith
  have ha0 : a ≠ 0 := ha.ne'
  have hb0 : b ≠ 0 := hb.ne
  unfold c1 c2
  field_simp
  ring

theorem v_concave {a b x xs : ℝ} (ha : 0 < a) (hb : b < 0) (hx : x < xs) :
    -(a * b) / (a - b) * (exp (a * (x - xs)) - exp (b * (x - xs))) < 0 := by
  have hab : 0 < a - b := by linarith
  have hk : 0 < -(a * b) / (a - b) := div_pos (by nlinarith) hab
  have hlt : exp (a * (x - xs)) < exp (b * (x - xs)) := exp_lt_exp.mpr (by nlinarith)
  nlinarith

theorem control_concave_needs_below_barrier :
    0 < -((1 : ℝ) * (-1)) / (1 - (-1)) * (exp (1 * (1 : ℝ)) - exp ((-1) * (1 : ℝ))) := by
  have : exp ((-1) * (1 : ℝ)) < exp (1 * 1) := exp_lt_exp.mpr (by norm_num)
  have h2 : (0 : ℝ) < -((1 : ℝ) * (-1)) / (1 - (-1)) := by norm_num
  exact mul_pos h2 (sub_pos.mpr this)

theorem control_coeffs_need_distinct_roots :
    c1 1 1 0 * 1 * exp (1 * 0) + c2 1 1 0 * 1 * exp (1 * 0) ≠ 1 := by
  unfold c1 c2
  norm_num

end LokkaZervos
