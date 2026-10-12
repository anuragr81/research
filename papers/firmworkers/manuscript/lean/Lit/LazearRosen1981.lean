import Mathlib.Analysis.SpecialFunctions.Pow.Deriv
import Mathlib.Analysis.SpecialFunctions.Sqrt
import Mathlib.Data.Real.Pi.Bounds
import Mathlib.Tactic

open Real

namespace LazearRosen1981

/-! Section II: risk neutrality, two-player tournament, density of the noise
difference at zero `g0`. -/

section RiskNeutral

theorem prize_structure {V μ g0 W1 W2 : ℝ} (hg : 0 < g0) (hz : W1 + W2 = 2 * V * μ)
    (hs : (W1 - W2) * g0 = V) : W1 = V * μ + V / (2 * g0) ∧ W2 = V * μ - V / (2 * g0) := by
  have hW : W1 - W2 = V / g0 := by
    rw [eq_div_iff hg.ne']
    exact hs
  have h2 : V / (2 * g0) = V / g0 / 2 := by
    rw [div_div, mul_comm]
  constructor <;> linarith

theorem bond_gamble {V μ W1 W2 : ℝ} (hz : W1 + W2 = 2 * V * μ) : W1 / 2 + W2 / 2 = V * μ := by
  linarith

theorem tournament_matches_piece_rate {V g0 ΔW Cμ : ℝ} (hg : g0 ≠ 0) (hfoc : Cμ = ΔW * g0)
    (hspread : ΔW = V / g0) : Cμ = V := by
  rw [hfoc, hspread]
  field_simp

theorem normal_spread (V σ : ℝ) : V / (1 / (2 * σ * √π)) = 2 * V * σ * √π := by
  rw [one_div, div_inv_eq_mul]
  ring

theorem spread_increasing {V σ σ' : ℝ} (hV : 0 < V) (h : σ < σ') :
    2 * V * σ * √π < 2 * V * σ' * √π := by
  have hs : 0 < √π := Real.sqrt_pos.mpr Real.pi_pos
  have hk : 0 < 2 * V * √π := by positivity
  nlinarith

end RiskNeutral

/-! Section III: risk aversion, second-order approximations with `V = 1`,
`C(μ) = μ²/2` (table 1 note), `x = s C'' σ²`. -/

section RiskAversion

noncomputable def ce (m v s : ℝ) : ℝ := m - m ^ 2 / 2 - s / 2 * v

theorem piece_rate_variance {V σ x : ℝ} (hx : 0 < 1 + x) :
    (V / (1 + x)) ^ 2 * σ ^ 2 = V ^ 2 * σ ^ 2 / (1 + x) ^ 2 := by
  field_simp

theorem contest_variance_from_spread {V σ x : ℝ} (hx : 0 < 1 + π * x) :
    (V / (1 + π * x) / (1 / (2 * σ * √π))) ^ 2 / 4 = π * σ ^ 2 * V ^ 2 / (1 + π * x) ^ 2 := by
  have hs : √π ^ 2 = π := Real.sq_sqrt Real.pi_pos.le
  rw [one_div, div_inv_eq_mul, div_mul_eq_mul_div, div_pow, mul_pow, mul_pow, mul_pow, hs]
  field_simp
  ring

theorem contest_investment_lower {x : ℝ} (hx : 0 < x) : 1 / (1 + π * x) < 1 / (1 + x) := by
  apply one_div_lt_one_div_of_lt (by positivity)
  nlinarith [Real.pi_gt_three]

theorem contest_variance_smaller_iff {V σ x : ℝ} (hV : V ≠ 0) (hσ : σ ≠ 0) (hx : 0 < x) :
    π * σ ^ 2 * V ^ 2 / (1 + π * x) ^ 2 < V ^ 2 * σ ^ 2 / (1 + x) ^ 2 ↔ 1 / √π < x := by
  have hk : 0 < V ^ 2 * σ ^ 2 := by positivity
  have hp : 0 < √π := Real.sqrt_pos.mpr Real.pi_pos
  have hs : √π ^ 2 = π := Real.sq_sqrt Real.pi_pos.le
  have hpi := Real.pi_gt_three
  rw [div_lt_div_iff₀ (by positivity) (by positivity), div_lt_iff₀ hp]
  have key : π * σ ^ 2 * V ^ 2 * (1 + x) ^ 2 < V ^ 2 * σ ^ 2 * (1 + π * x) ^ 2
      ↔ 1 < π * x ^ 2 := by
    have e : V ^ 2 * σ ^ 2 * (1 + π * x) ^ 2 - π * σ ^ 2 * V ^ 2 * (1 + x) ^ 2
        = V ^ 2 * σ ^ 2 * ((π - 1) * (π * x ^ 2 - 1)) := by ring
    constructor
    · intro h
      have h1 : 0 < V ^ 2 * σ ^ 2 * ((π - 1) * (π * x ^ 2 - 1)) := by linarith
      have h2 : 0 < (π - 1) * (π * x ^ 2 - 1) := pos_of_mul_pos_right h1 hk.le
      nlinarith
    · intro h
      have h2 : 0 < (π - 1) * (π * x ^ 2 - 1) := mul_pos (by linarith) (by linarith)
      nlinarith
  have sq : (x * √π) ^ 2 = π * x ^ 2 := by
    rw [mul_pow, hs]
    ring
  have xp : 0 < x * √π := mul_pos hx hp
  rw [key]
  constructor
  · intro h
    nlinarith
  · intro h
    nlinarith

theorem piece_rate_ce {s σ2 : ℝ} (h : 0 < 1 + s * σ2) :
    ce (1 / (1 + s * σ2)) (σ2 / (1 + s * σ2) ^ 2) s = 1 / (1 + s * σ2) / 2 := by
  unfold ce
  field_simp
  ring

theorem contest_ce {s σ2 : ℝ} (h : 0 < 1 + π * (s * σ2)) :
    ce (1 / (1 + π * (s * σ2))) (π * σ2 / (1 + π * (s * σ2)) ^ 2) s = 1 / (1 + π * (s * σ2)) / 2 := by
  unfold ce
  field_simp
  ring

theorem approximations_favour_piece_rate {s σ2 : ℝ} (hs : 0 < s) (hσ : 0 < σ2) :
    ce (1 / (1 + π * (s * σ2))) (π * σ2 / (1 + π * (s * σ2)) ^ 2) s
      < ce (1 / (1 + s * σ2)) (σ2 / (1 + s * σ2) ^ 2) s := by
  have hx : 0 < s * σ2 := mul_pos hs hσ
  rw [piece_rate_ce (by positivity), contest_ce (by positivity)]
  have := contest_investment_lower hx
  linarith

theorem utility_derivatives {a y : ℝ} (hy : 0 < y) :
    HasDerivAt (fun y => a * y ^ a) (a * (a * y ^ (a - 1))) y ∧
      HasDerivAt (fun y => a * (a * y ^ (a - 1))) (a * (a * ((a - 1) * y ^ (a - 1 - 1)))) y := by
  constructor
  · exact (Real.hasDerivAt_rpow_const (Or.inl hy.ne')).const_mul a
  · exact ((Real.hasDerivAt_rpow_const (Or.inl hy.ne')).const_mul a).const_mul a

theorem dara_example {a y : ℝ} (ha : a ≠ 0) (hy : 0 < y) :
    -(a * (a * ((a - 1) * y ^ (a - 1 - 1)))) / (a * (a * y ^ (a - 1))) = (1 - a) / y := by
  have h1 : y ^ (a - 1 - 1) = y ^ (a - 1) / y := Real.rpow_sub_one hy.ne' (a - 1)
  have h2 : 0 < y ^ (a - 1) := Real.rpow_pos_of_pos hy _
  rw [h1]
  field_simp
  ring

theorem table_s_values : (1 - 0.5 : ℝ) / 100 = 0.005 ∧ (1 - 0.5 : ℝ) / 25 = 0.02 := by
  constructor <;> norm_num

theorem rich_rows :
    (5.012465 : ℝ) > 5.012155 ∧ (5.012445 : ℝ) > 5.012150 ∧ (5.012295 : ℝ) > 5.012100 ∧
      (5.011940 : ℝ) > 5.011925 ∧ (5.011800 : ℝ) > 5.011415 ∧ (5.011420 : ℝ) > 5.010515 := by
  norm_num

theorem poor_rows :
    (2.524725 : ℝ) > 2.524665 ∧ (2.524616 : ℝ) > 2.524575 ∧ (2.524237 : ℝ) > 2.523437 ∧
      (2.519930 : ℝ) > 2.514282 := by
  norm_num

theorem unit_variance_sorting :
    (5.012295 : ℝ) > 5.012100 ∧ (2.524237 : ℝ) > 2.523437 ∧ (0.9846 : ℝ) > 0.9807 := by
  norm_num

theorem common_error_tips_balance (σ2 ρ2 : ℝ) : 2 * σ2 < σ2 + ρ2 ↔ σ2 < ρ2 := by
  constructor <;> intro h <;> linarith

end RiskAversion

/-! Section IV: heterogeneous contestants. -/

section Heterogeneous

noncomputable def R (V g0 μs : ℝ) (G : ℝ → ℝ) (μ : ℝ) : ℝ :=
  V * μs - V / g0 * (1 / 2 - G (μ - μs))

theorem revenue_at_own_investment {V g0 μs : ℝ} {G : ℝ → ℝ} (hG : G 0 = 1 / 2) :
    R V g0 μs G μs = V * μs := by
  simp [R, hG]

theorem league_displacement (V g0 μa μb : ℝ) (G : ℝ → ℝ) (μ : ℝ) :
    R V g0 μb G μ = R V g0 μa G (μ + (μa - μb)) - V * (μa - μb) := by
  simp only [R]
  rw [show μ + (μa - μb) - μa = μ - μb by ring]
  ring

theorem players_do_not_self_sort {V g0 μa μb μ : ℝ} {G : ℝ → ℝ}
    (hslope : ∀ u v, u ≤ v → V / g0 * (G v - G u) ≤ V * (v - u)) (hab : μb ≤ μa) :
    R V g0 μb G μ ≤ R V g0 μa G μ := by
  have h := hslope (μ - μa) (μ - μb) (by linarith)
  simp only [R]
  nlinarith

theorem control_self_sort_needs_slope_bound : ¬ ∀ G : ℝ → ℝ, R 1 1 0 G 0 ≤ R 1 1 1 G 0 := by
  intro h
  have := h (fun x => 3 * x)
  simp only [R] at this
  norm_num at this

theorem mixed_efficient_iff_half {α g0 gD : ℝ} (hg : g0 ≠ gD) :
    α * g0 + (1 - α) * gD = α * gD + (1 - α) * g0 ↔ α = 1 / 2 := by
  constructor
  · intro h
    have e : (2 * α - 1) * (g0 - gD) = 0 := by linarith
    rcases mul_eq_zero.mp e with h1 | h1
    · linarith
    · exact absurd (sub_eq_zero.mp h1) hg
  · intro h
    subst h
    ring

noncomputable def winProb (γ Δ h : ℝ) : ℝ := 1 / 2 + γ * (Δ - h)

def gainA (P W1 W2 V μa : ℝ) : ℝ := P * W1 + (1 - P) * W2 - V * μa

def gainB (P W1 W2 V μb : ℝ) : ℝ := (1 - P) * W1 + P * W2 - V * μb

theorem handicap_gain {V γ μa μb h W1 W2 : ℝ} (hγ : γ ≠ 0) (hzp : W1 + W2 = V * (μa + μb))
    (hsp : W1 - W2 = V / γ) :
    gainA (winProb γ (μa - μb) h) W1 W2 V μa = V * ((μa - μb) / 2 - h) := by
  have e : gainA (winProb γ (μa - μb) h) W1 W2 V μa
      = (W1 + W2) / 2 + γ * ((μa - μb) - h) * (W1 - W2) - V * μa := by
    unfold gainA winProb
    ring
  rw [e, hzp, hsp]
  field_simp
  ring

theorem handicap_zero_sum {V P μa μb W1 W2 : ℝ} (hzp : W1 + W2 = V * (μa + μb)) :
    gainA P W1 W2 V μa + gainB P W1 W2 V μb = 0 := by
  unfold gainA gainB
  linarith

theorem competitive_handicap {V γ μa μb W1 W2 : ℝ} (hγ : γ ≠ 0) (hzp : W1 + W2 = V * (μa + μb))
    (hsp : W1 - W2 = V / γ) :
    gainA (winProb γ (μa - μb) ((μa - μb) / 2)) W1 W2 V μa = 0 := by
  rw [handicap_gain hγ hzp hsp]
  ring

theorem competitive_handicap_not_fair {Δ : ℝ} (h : 0 < Δ) : Δ / 2 < Δ := by
  linarith

theorem mixed_preference_below_handicap {V γ μa μb h W1 W2 : ℝ} (hV : 0 < V) (hγ : γ ≠ 0)
    (hzp : W1 + W2 = V * (μa + μb)) (hsp : W1 - W2 = V / γ) (hh : h < (μa - μb) / 2) :
    0 < gainA (winProb γ (μa - μb) h) W1 W2 V μa ∧
      gainB (winProb γ (μa - μb) h) W1 W2 V μb < 0 := by
  have ga := handicap_gain (h := h) hγ hzp hsp
  have zs := handicap_zero_sum (P := winProb γ (μa - μb) h) (μa := μa) (μb := μb) hzp
  have pos : 0 < V * ((μa - μb) / 2 - h) := mul_pos hV (by linarith)
  constructor <;> linarith

theorem reverse_discrimination_clears {V γ μa μb W1 W2 : ℝ} (hγ : γ ≠ 0)
    (hzp : W1 + W2 = V * (μa + μb)) (hsp : W1 - W2 = V / γ) :
    gainA (winProb γ (μa - μb) 0) (W1 - V * (μa - μb) / 2) (W2 - V * (μa - μb) / 2) V μa = 0 ∧
      gainB (winProb γ (μa - μb) 0) (W1 + V * (μa - μb) / 2) (W2 + V * (μa - μb) / 2) V μb = 0 ∧
      (W1 - V * (μa - μb) / 2) - (W2 - V * (μa - μb) / 2) = W1 - W2 := by
  have ga := handicap_gain (h := 0) hγ hzp hsp
  have zs := handicap_zero_sum (P := winProb γ (μa - μb) 0) (μa := μa) (μb := μb) hzp
  refine ⟨?_, ?_, by ring⟩
  · have e : gainA (winProb γ (μa - μb) 0) (W1 - V * (μa - μb) / 2) (W2 - V * (μa - μb) / 2) V μa
        = gainA (winProb γ (μa - μb) 0) W1 W2 V μa - V * (μa - μb) / 2 := by
      unfold gainA
      ring
    rw [e, ga]
    ring
  · have e : gainB (winProb γ (μa - μb) 0) (W1 + V * (μa - μb) / 2) (W2 + V * (μa - μb) / 2) V μb
        = gainB (winProb γ (μa - μb) 0) W1 W2 V μb + V * (μa - μb) / 2 := by
      unfold gainB
      ring
    rw [e]
    linarith

theorem control_handicap_needs_zero_profit {V P μa μb W1 W2 S : ℝ} (hS : W1 + W2 = S)
    (hne : S ≠ V * (μa + μb)) : gainA P W1 W2 V μa + gainB P W1 W2 V μb ≠ 0 := by
  unfold gainA gainB
  intro h
  exact hne (by linarith)

end Heterogeneous

end LazearRosen1981
