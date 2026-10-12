import Mathlib.Tactic

open Finset

namespace JacobsonLaLondeSullivan1993

/-! Model (2), p.12, without the error term: fixed effect `α`, quarter effects `γ`,
controls `xβ`, and displacement effects `δ k` for `k = t - s ≥ -m`. -/

def displaced (α : ℝ) (γ xβ δ : ℤ → ℝ) (m s t : ℤ) : ℝ :=
  α + γ t + xβ t + if -m ≤ t - s then δ (t - s) else 0

def stayer (α : ℝ) (γ xβ : ℤ → ℝ) (t : ℤ) : ℝ :=
  α + γ t + xβ t

def did (yi yj : ℤ → ℝ) (t b : ℤ) : ℝ :=
  (yi t - yi b) - (yj t - yj b)

section Model2

variable (γ δ : ℤ → ℝ) (m s : ℤ)

theorem did_identifies_effect {αi αj : ℝ} {xi xj : ℤ → ℝ} {t b : ℤ}
    (hb : b - s < -m) (ht : -m ≤ t - s) (hx : xi t - xi b = xj t - xj b) :
    did (displaced αi γ xi δ m s) (stayer αj γ xj) t b = δ (t - s) := by
  simp only [did, displaced, stayer, if_pos ht, if_neg (not_le.mpr hb)]
  linarith

theorem fixed_effects_cancel (αi αj αi' αj' : ℝ) (xi xj : ℤ → ℝ) (t b : ℤ) :
    did (displaced αi γ xi δ m s) (stayer αj γ xj) t b
      = did (displaced αi' γ xi δ m s) (stayer αj' γ xj) t b := by
  simp only [did, displaced, stayer]
  ring

theorem short_base_bias {αi αj : ℝ} {xi xj : ℤ → ℝ} {t b : ℤ}
    (hb : -m ≤ b - s) (ht : -m ≤ t - s) (hx : xi t - xi b = xj t - xj b) :
    did (displaced αi γ xi δ m s) (stayer αj γ xj) t b = δ (t - s) - δ (b - s) := by
  simp only [did, displaced, stayer, if_pos ht, if_pos hb]
  linarith

theorem short_base_understates {αi αj : ℝ} {xi xj : ℤ → ℝ} {t b : ℤ}
    (hb : -m ≤ b - s) (ht : -m ≤ t - s) (hx : xi t - xi b = xj t - xj b)
    (hdip : δ (b - s) < 0) :
    δ (t - s) < did (displaced αi γ xi δ m s) (stayer αj γ xj) t b := by
  rw [short_base_bias γ δ m s hb ht hx]
  linarith

theorem simple_change_adequate {α : ℝ} {xβ : ℤ → ℝ} {t b : ℤ}
    (hb : b - s < -m) (ht : -m ≤ t - s) (hγ : γ t = γ b) (hx : xβ t = xβ b) :
    displaced α γ xβ δ m s t - displaced α γ xβ δ m s b = δ (t - s) := by
  simp only [displaced, if_pos ht, if_neg (not_le.mpr hb)]
  rw [hγ, hx]
  ring

theorem control_simple_change_needs_constant_time_effects {α : ℝ} {xβ : ℤ → ℝ} {t b : ℤ}
    (hb : b - s < -m) (ht : -m ≤ t - s) (hγ : γ t ≠ γ b) (hx : xβ t = xβ b) :
    displaced α γ xβ δ m s t - displaced α γ xβ δ m s b ≠ δ (t - s) := by
  simp only [displaced, if_pos ht, if_neg (not_le.mpr hb)]
  rw [hx]
  intro h
  exact hγ (by linarith)

end Model2

/-! Model (3), p.14: worker-specific trends `ω`. -/

def displacedT (α ω : ℝ) (γ xβ δ : ℤ → ℝ) (m s t : ℤ) : ℝ :=
  α + ω * t + γ t + xβ t + if -m ≤ t - s then δ (t - s) else 0

def stayerT (α ω : ℝ) (γ xβ : ℤ → ℝ) (t : ℤ) : ℝ :=
  α + ω * t + γ t + xβ t

noncomputable def detrended (y : ℤ → ℝ) (t b b' : ℤ) : ℝ :=
  (y t - y b) - ((t - b : ℝ) / (b - b')) * (y b - y b')

section Model3

variable (γ xβ δ : ℤ → ℝ) (m s : ℤ)

theorem trend_bias {αi αj ωi ωj : ℝ} {t b : ℤ} (hb : b - s < -m) (ht : -m ≤ t - s) :
    did (displacedT αi ωi γ xβ δ m s) (stayerT αj ωj γ xβ) t b
      = δ (t - s) + (ωi - ωj) * (t - b) := by
  simp only [did, displacedT, stayerT, if_pos ht, if_neg (not_le.mpr hb)]
  ring

theorem slower_trend_overstates {αi αj ωi ωj : ℝ} {t b : ℤ} (hb : b - s < -m)
    (ht : -m ≤ t - s) (hω : ωi < ωj) (htb : b < t) :
    did (displacedT αi ωi γ xβ δ m s) (stayerT αj ωj γ xβ) t b < δ (t - s) := by
  rw [trend_bias γ xβ δ m s hb ht]
  have : (0 : ℝ) < (t : ℝ) - b := by
    have : (b : ℝ) < t := by exact_mod_cast htb
    linarith
  nlinarith

theorem two_base_periods_remove_trend {αi αj ωi ωj : ℝ} {t b b' : ℤ} (hbb : b' < b)
    (hb : b - s < -m) (ht : -m ≤ t - s) :
    detrended (displacedT αi ωi γ xβ δ m s) t b b' - detrended (stayerT αj ωj γ xβ) t b b'
      = δ (t - s) := by
  have hb' : ¬ (-m ≤ b' - s) := by omega
  have hne : ((b : ℝ) - b') ≠ 0 := by
    have : (b' : ℝ) < b := by exact_mod_cast hbb
    linarith
  simp only [detrended, displacedT, stayerT, if_pos ht, if_neg (not_le.mpr hb), if_neg hb']
  field_simp
  ring

end Model3

/-! Model (5'), p.16: dip, drop and recovery trends. -/

def F1 (k : ℤ) : ℝ := if -12 ≤ k ∧ k ≤ 0 then ((k + 13 : ℤ) : ℝ) else 0

def F2 (k : ℤ) : ℝ := if 1 ≤ k then 1 else 0

def F3 (k : ℤ) : ℝ := if 7 ≤ k then ((k - 6 : ℤ) : ℝ) else 0

def loss (δ : ℤ → ℝ) (dip drop rec : ℝ) (k : ℤ) : ℝ :=
  δ k + F1 k * dip + F2 k * drop + F3 k * rec

section Model5

variable (δ : ℤ → ℝ) (dip drop rec : ℝ)

theorem loss_before_dip {k : ℤ} (hk : k ≤ -13) : loss δ dip drop rec k = δ k := by
  have h1 : ¬ (-12 ≤ k ∧ k ≤ 0) := by omega
  have h2 : ¬ (1 ≤ k) := by omega
  have h3 : ¬ (7 ≤ k) := by omega
  simp [loss, F1, F2, F3, h1, h2, h3]

theorem loss_dip {k : ℤ} (hk0 : -12 ≤ k) (hk1 : k ≤ 0) :
    loss δ dip drop rec k = δ k + dip * (k + 13) := by
  have h2 : ¬ (1 ≤ k) := by omega
  have h3 : ¬ (7 ≤ k) := by omega
  simp [loss, F1, F2, F3, hk0, hk1, h2, h3]
  ring

theorem loss_drop {k : ℤ} (hk0 : 1 ≤ k) (hk1 : k ≤ 6) :
    loss δ dip drop rec k = δ k + drop := by
  have h1 : ¬ (-12 ≤ k ∧ k ≤ 0) := by omega
  have h3 : ¬ (7 ≤ k) := by omega
  simp [loss, F1, F2, F3, h1, hk0, h3]

theorem loss_recovery {k : ℤ} (hk : 7 ≤ k) :
    loss δ dip drop rec k = δ k + drop + rec * (k - 6) := by
  have h1 : ¬ (-12 ≤ k ∧ k ≤ 0) := by omega
  have h2 : 1 ≤ k := by omega
  simp [loss, F1, F2, F3, h1, h2, hk]
  ring

theorem gap_zero_before_dip (dip' drop' rec' : ℝ) {k : ℤ} (hk : k ≤ -13) :
    loss δ dip drop rec k - loss δ dip' drop' rec' k = 0 := by
  rw [loss_before_dip δ dip drop rec hk, loss_before_dip δ dip' drop' rec' hk]
  ring

theorem dip_trend_at_separation : F1 0 = 13 := by
  norm_num [F1]

theorem fifth_year_weights :
    F1 17 + F1 18 + F1 19 + F1 20 = 0 ∧ F2 17 + F2 18 + F2 19 + F2 20 = 4 ∧
      F3 17 + F3 18 + F3 19 + F3 20 = 50 := by
  norm_num [F1, F2, F3]

theorem fifth_year_loss_dif :
    (loss δ dip drop rec 17 - δ 17) + (loss δ dip drop rec 18 - δ 18)
      + (loss δ dip drop rec 19 - δ 19) + (loss δ dip drop rec 20 - δ 20)
      = 4 * drop + 50 * rec := by
  norm_num [loss, F1, F2, F3]
  ring

theorem average_loss_is_delta {ι : Type*} (S : Finset ι) (f φ1 φ2 φ3 : ι → ℝ) (k : ℤ)
    (hf : ∑ j ∈ S, f j = 1) (h1 : ∑ j ∈ S, f j * φ1 j = 0) (h2 : ∑ j ∈ S, f j * φ2 j = 0)
    (h3 : ∑ j ∈ S, f j * φ3 j = 0) :
    ∑ j ∈ S, f j * loss δ (φ1 j) (φ2 j) (φ3 j) k = δ k := by
  have e : ∀ j ∈ S, f j * loss δ (φ1 j) (φ2 j) (φ3 j) k
      = δ k * f j + F1 k * (f j * φ1 j) + F2 k * (f j * φ2 j) + F3 k * (f j * φ3 j) := by
    intro j _
    unfold loss
    ring
  rw [sum_congr rfl e]
  simp only [sum_add_distrib, ← mul_sum, hf, h1, h2, h3]
  ring

theorem control_average_needs_balance {ι : Type*} (S : Finset ι) (f φ1 φ2 φ3 : ι → ℝ)
    (h1 : ∑ j ∈ S, f j * φ1 j ≠ 0) (hf : ∑ j ∈ S, f j = 1) :
    ∑ j ∈ S, f j * loss δ (φ1 j) (φ2 j) (φ3 j) 0 ≠ δ 0 := by
  have e : ∀ j ∈ S, f j * loss δ (φ1 j) (φ2 j) (φ3 j) 0 = δ 0 * f j + 13 * (f j * φ1 j) := by
    intro j _
    norm_num [loss, F1, F2, F3]
    ring
  rw [sum_congr rfl e, sum_add_distrib, ← mul_sum, ← mul_sum, hf]
  intro h
  exact h1 (by linarith)

end Model5

/-! The argument of p.18 for long-run effects under stationary errors. -/

theorem spurious_effects_symmetric (c : ℤ → ℝ) (hc : ∀ j, c (-j) = c j) (e : ℝ) (j : ℤ) :
    c j / c 0 * e = c (-j) / c 0 * e := by
  rw [hc]

theorem long_run_bias_bounded {truePre spPre spPost ε : ℝ} (h1 : truePre ≤ 0) (h2 : spPre ≤ 0)
    (hest : -ε ≤ truePre + spPre) (hsym : spPost = spPre) :
    -ε ≤ spPost ∧ spPost ≤ 0 := by
  subst hsym
  constructor <;> linarith

theorem control_bias_bound_needs_same_sign :
    ¬ ∀ tp sp ε : ℝ, 0 ≤ ε → sp ≤ 0 → -ε ≤ tp + sp → -ε ≤ sp := by
  intro h
  have := h 1 (-1) (1 / 2) (by norm_num) (by norm_num) (by norm_num)
  norm_num at this

/-! Arithmetic of the text against Tables 1-3. -/

theorem periods_and_parameters : (Icc (-20 : ℤ) 27).card = 48 ∧ 48 * 12 = 576 := by
  constructor
  · simp
  · norm_num

theorem group_counts_sum :
    (4972 + 1463 : ℕ) = 6435 ∧ (2599 + 2584 + 1252 : ℕ) = 6435 ∧
      (1704 + 1497 + 1381 + 1853 : ℕ) = 6435 ∧
      (247 + 1206 + 1354 + 436 + 632 + 421 + 419 + 441 + 348 + 545 + 183 + 203 : ℕ) = 6435 := by
  norm_num

theorem sex_rows_balance :
    |(4972 * (-10.8) + 1463 * 36.7 : ℝ) / 6435| < 0.05 ∧
      |(4972 * (-217) + 1463 * 738 : ℝ) / 6435| < 0.5 ∧
      |(4972 * 6.5 + 1463 * (-22.0) : ℝ) / 6435| < 0.05 := by
  refine ⟨?_, ?_, ?_⟩ <;> rw [abs_lt] <;> constructor <;> norm_num

theorem separation_gap_men_women :
    ((-10.8) - 36.7) * 13 = (-617.5 : ℝ) ∧ |(617.5 : ℝ) - 618| ≤ 0.5 := by
  constructor
  · norm_num
  · rw [abs_le]
    constructor <;> norm_num

theorem fifth_year_dif_from_coefficients :
    |4 * (-217) + 50 * 6.5 - (-545 : ℝ)| ≤ 2 ∧ |4 * 738 + 50 * (-22.0) - (1853 : ℝ)| ≤ 2 := by
  constructor <;> rw [abs_le] <;> constructor <;> norm_num

theorem fifth_year_gap_men_women :
    (-545 : ℝ) - 1853 = -2398 ∧ |((-7143 : ℝ) - (-4744)) - (-2398)| ≤ 1 := by
  constructor
  · norm_num
  · rw [abs_le]
    constructor <;> norm_num

theorem quarterly_loss_share : |(1600 : ℝ) / (24461 / 4) - 0.25| < 0.015 := by
  rw [abs_lt]
  constructor <;> norm_num

theorem comparison_group_gap : 1 - (1200 : ℝ) / 1500 = 0.2 := by
  norm_num

theorem excluded_never_positive : |(0.38 : ℝ) * 0.70 - 0.25| < 0.02 := by
  rw [abs_lt]
  constructor <;> norm_num

theorem national_scaling_reading : (135000 : ℝ) / 0.05 = 2700000 := by
  norm_num

theorem level_and_share_by_sex :
    (4744 : ℝ) < 7143 ∧ (7143 : ℝ) / 27363 < 4744 / 14897 := by
  constructor <;> norm_num

end JacobsonLaLondeSullivan1993
