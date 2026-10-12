import Mathlib

/-!
# Corcoran (1984), "Long-run equilibrium and total expenditures in rent-seeking"

Public Choice 43(1): 89-94.  Equation numbers are the printed ones.

Setup: `n` identical players (a real number, as in the paper), each spending `a` on a
Tullock lottery with ease-of-influence `r`, for a prize worth `R = P e^{-ρ t}` in present
value.  Player A's payoff when each of the `n-1` rivals spends `b` is eq. (1) generalised
to `n` players (`V`).

## What is proved

1. Short run.  The derivative of `V` at the symmetric point (`hasDerivAt_V`), its zero
   (`foc_iff`: eq. (2)), the winning probability `1/n` (`winning_prob`), total
   expenditure (`Tot_eq`: eq. (3)), Tullock's "one quarter" (`tullock_quarter`), the
   comparison of `T` with the prize (`Tot_vs_prize`), `T = 0` at `n = 1`, `T < rR`,
   `T` increasing in `n` with limit `rR`, increasing in `r`, decreasing in `ρ` and `t`.
2. Long run.  The rate of return (`exp_piRate`: eqs. (4)-(5)), its comparison with `ρ`
   (`piRate_cmp`), the zero of the entry incentive `n = r/(r-1)` (`entry_zero_iff`:
   eqs. (6)-(7)), entry above and exit below that count, and unbounded entry for `r ≤ 1`
   (`unbounded_entry`).
3. `t = 0` (eqs. (8)-(9)).  Profit `R(n+r-nr)/n²` (`profit0_eq`), its sign, its zero at
   `r/(r-1)` (`profit0_zero_iff`), positive for all `n` when `r ≤ 1`, footnote 4
   (`profit0_hasDerivAt_r`).
4. Eq. (10): `T = R` at the long-run count, whatever `r > 1` (`Tot_nLong`,
   `Tot_longrun_indep`); the exact offset between individual expenditure and entry
   (`aStar_nLong`, `nLong_strictAnti`, `aStar_nLong_strictMono`).
5. Scope of "Cournot-Nash" (not in the paper, which uses only the first-order condition):
   for `r ≤ 1` the eq. (2) point is a global best reply for every `n > 1`
   (`nash_of_le_one`); at the long-run count it is a global best reply for every `r > 1`
   (`nash_longrun`); where profit at it is negative it is not an equilibrium
   (`not_nash_of_loss`).

## Not formalised

Whether the eq. (2) point is a best reply for `1 < r` and `n` strictly below `r/(r-1)`;
the paper does not discuss it.  The time to reach long-run equilibrium (section 3, last
paragraph) and the welfare reading in section 4.
-/

open Real Filter Topology

namespace Corcoran1984

/-- Prize in present value, `P e^{-ρ t}`. -/
noncomputable def pv (P ρ t : ℝ) : ℝ := P * Real.exp (-(ρ * t))

/-- Eq. (1) with `n - 1` identical rivals each spending `b`; `R = P e^{-ρ t}`. -/
noncomputable def V (R r n b a : ℝ) : ℝ := R * (a ^ r / (a ^ r + (n - 1) * b ^ r)) - a

/-- Eq. (2): individual expenditure. -/
noncomputable def aStar (R r n : ℝ) : ℝ := (n - 1) * r * R / n ^ 2

/-- Eq. (3): total expenditure `n` times eq. (2). -/
noncomputable def Tot (R r n : ℝ) : ℝ := n * aStar R r n

/-- Eq. (8): expected profit at `t = 0`, `P/n - a`. -/
noncomputable def profit0 (R r n : ℝ) : ℝ := R / n - aStar R r n

/-- Eq. (5): the expected rate of return. -/
noncomputable def piRate (ρ t r n : ℝ) : ℝ := ρ + (1 / t) * Real.log (n / ((n - 1) * r))

/-- Eq. (7): the long-run number of players. -/
noncomputable def nLong (r : ℝ) : ℝ := r / (r - 1)

/-! ### 1. Short run -/

theorem hasDerivAt_V (R r n b : ℝ) (hn : 0 < n) (hb : 0 < b) :
    HasDerivAt (fun a => V R r n b a) (R * (r * (n - 1) / (n ^ 2 * b)) - 1) b := by
  have hbr : 0 < b ^ r := Real.rpow_pos_of_pos hb r
  have hf : HasDerivAt (fun a : ℝ => a ^ r) (r * b ^ (r - 1)) b :=
    Real.hasDerivAt_rpow_const (Or.inl hb.ne')
  have hg : HasDerivAt (fun a : ℝ => a ^ r + (n - 1) * b ^ r) (r * b ^ (r - 1)) b := by
    simpa using hf.add_const ((n - 1) * b ^ r)
  have hgb : b ^ r + (n - 1) * b ^ r ≠ 0 := by
    have : b ^ r + (n - 1) * b ^ r = n * b ^ r := by ring
    rw [this]; positivity
  have hq := hf.div hg hgb
  have h := (hq.const_mul R).sub (hasDerivAt_id b)
  refine h.congr_deriv ?_
  rw [Real.rpow_sub_one hb.ne']
  have : b ^ r + (n - 1) * b ^ r = n * b ^ r := by ring
  rw [this]
  field_simp

theorem foc_iff (R r n b : ℝ) (hR : 0 < R) (hr : 0 < r) (hn : 1 < n) (hb : 0 < b) :
    R * (r * (n - 1) / (n ^ 2 * b)) - 1 = 0 ↔ b = aStar R r n := by
  unfold aStar
  have hn0 : (0 : ℝ) < n := by linarith
  have hn2 : n ^ 2 ≠ 0 := by positivity
  constructor
  · intro h
    have h' : R * (r * (n - 1)) = n ^ 2 * b := by
      field_simp at h
      linarith
    field_simp
    linarith
  · intro h
    have hn1 : n - 1 ≠ 0 := by linarith
    have hr0 : r ≠ 0 := hr.ne'
    have hR0 : R ≠ 0 := hR.ne'
    rw [h]
    field_simp
    ring

/-- The derivative of the payoff at the symmetric point vanishes exactly at eq. (2). -/
theorem symm_foc (R r n b : ℝ) (hR : 0 < R) (hr : 0 < r) (hn : 1 < n) (hb : 0 < b) :
    deriv (fun a => V R r n b a) b = 0 ↔ b = aStar R r n := by
  rw [(hasDerivAt_V R r n b (by linarith) hb).deriv]
  exact foc_iff R r n b hR hr hn hb

theorem winning_prob (r n a : ℝ) (ha : 0 < a) (hn : 0 < n) :
    a ^ r / (a ^ r + (n - 1) * a ^ r) = 1 / n := by
  have h : 0 < a ^ r := Real.rpow_pos_of_pos ha r
  have : a ^ r + (n - 1) * a ^ r = n * a ^ r := by ring
  rw [this]
  field_simp

theorem Tot_eq (R r n : ℝ) (hn : n ≠ 0) : Tot R r n = (n - 1) / n * r * R := by
  unfold Tot aStar
  field_simp

theorem tullock_quarter (R : ℝ) : aStar R 1 2 = R / 4 ∧ Tot R 1 2 = R / 2 := by
  unfold Tot aStar
  constructor
  · norm_num
  · norm_num; ring

theorem Tot_vs_prize (R r n : ℝ) (hR : 0 < R) (hn : 1 < n) :
    (R < Tot R r n ↔ n < (n - 1) * r) ∧ (Tot R r n = R ↔ n = (n - 1) * r) ∧
      (Tot R r n < R ↔ (n - 1) * r < n) := by
  have hn0 : (0 : ℝ) < n := by linarith
  rw [Tot_eq R r n hn0.ne']
  have key : (n - 1) / n * r * R = ((n - 1) * r) * R / n := by ring
  rw [key]
  refine ⟨?_, ?_, ?_⟩
  · rw [lt_div_iff₀ hn0]
    constructor <;> intro h <;> nlinarith
  · rw [div_eq_iff hn0.ne']
    constructor <;> intro h <;> nlinarith
  · rw [div_lt_iff₀ hn0]
    constructor <;> intro h <;> nlinarith

theorem Tot_one (R r : ℝ) : Tot R r 1 = 0 := by
  unfold Tot aStar
  norm_num

theorem Tot_lt (R r n : ℝ) (hR : 0 < R) (hr : 0 < r) (hn : 1 < n) : Tot R r n < r * R := by
  have hn0 : (0 : ℝ) < n := by linarith
  rw [Tot_eq R r n hn0.ne']
  have : (n - 1) / n < 1 := by rw [div_lt_one hn0]; linarith
  have hrR : 0 < r * R := by positivity
  nlinarith

theorem Tot_strictMono_n (R r : ℝ) (hR : 0 < R) (hr : 0 < r) {n₁ n₂ : ℝ} (h₁ : 1 ≤ n₁)
    (h : n₁ < n₂) : Tot R r n₁ < Tot R r n₂ := by
  have h1 : (0 : ℝ) < n₁ := by linarith
  have h2 : (0 : ℝ) < n₂ := by linarith
  rw [Tot_eq R r n₁ h1.ne', Tot_eq R r n₂ h2.ne']
  have hrR : 0 < r * R := by positivity
  have : (n₁ - 1) / n₁ < (n₂ - 1) / n₂ := by
    rw [div_lt_div_iff₀ h1 h2]; nlinarith
  nlinarith

theorem Tot_tendsto (R r : ℝ) :
    Tendsto (fun n => Tot R r n) atTop (𝓝 (r * R)) := by
  have h : Tendsto (fun n : ℝ => r * R * (1 - n⁻¹)) atTop (𝓝 (r * R * (1 - 0))) :=
    tendsto_const_nhds.mul (tendsto_const_nhds.sub tendsto_inv_atTop_zero)
  rw [mul_sub, mul_one, mul_zero, sub_zero] at h
  refine h.congr' ?_
  filter_upwards [eventually_gt_atTop 0] with n hn
  rw [Tot_eq R r n hn.ne']
  field_simp

theorem Tot_strictMono_r (R n : ℝ) (hR : 0 < R) (hn : 1 < n) {r₁ r₂ : ℝ} (h : r₁ < r₂) :
    Tot R r₁ n < Tot R r₂ n := by
  have hn0 : (0 : ℝ) < n := by linarith
  rw [Tot_eq R r₁ n hn0.ne', Tot_eq R r₂ n hn0.ne']
  have : 0 < (n - 1) / n * R := by
    have : 0 < n - 1 := by linarith
    positivity
  nlinarith

theorem Tot_strictMono_R (r n : ℝ) (hr : 0 < r) (hn : 1 < n) {R₁ R₂ : ℝ} (h : R₁ < R₂) :
    Tot R₁ r n < Tot R₂ r n := by
  have hn0 : (0 : ℝ) < n := by linarith
  rw [Tot_eq R₁ r n hn0.ne', Tot_eq R₂ r n hn0.ne']
  have : 0 < (n - 1) / n * r := by
    have : 0 < n - 1 := by linarith
    positivity
  nlinarith

theorem Tot_strictAnti_rho (P t r n : ℝ) (hP : 0 < P) (ht : 0 < t) (hr : 0 < r) (hn : 1 < n)
    {ρ₁ ρ₂ : ℝ} (h : ρ₁ < ρ₂) : Tot (pv P ρ₂ t) r n < Tot (pv P ρ₁ t) r n := by
  apply Tot_strictMono_R r n hr hn
  unfold pv
  apply mul_lt_mul_of_pos_left _ hP
  apply Real.exp_lt_exp.mpr
  nlinarith

theorem Tot_strictAnti_t (P ρ r n : ℝ) (hP : 0 < P) (hρ : 0 < ρ) (hr : 0 < r) (hn : 1 < n)
    {t₁ t₂ : ℝ} (h : t₁ < t₂) : Tot (pv P ρ t₂) r n < Tot (pv P ρ t₁) r n := by
  apply Tot_strictMono_R r n hr hn
  unfold pv
  apply mul_lt_mul_of_pos_left _ hP
  apply Real.exp_lt_exp.mpr
  nlinarith

/-! ### 2. Long run: rate of return, entry and exit -/

theorem exp_piRate (P ρ t r n : ℝ) (hP : 0 < P) (ht : 0 < t) (hr : 0 < r) (hn : 1 < n) :
    Real.exp (piRate ρ t r n * t) = (P / n) / aStar (pv P ρ t) r n := by
  have hn0 : (0 : ℝ) < n := by linarith
  have hn1 : 0 < n - 1 := by linarith
  have hx : 0 < n / ((n - 1) * r) := by positivity
  have h1 : piRate ρ t r n * t = ρ * t + Real.log (n / ((n - 1) * r)) := by
    unfold piRate; field_simp
  rw [h1, Real.exp_add, Real.exp_log hx]
  unfold aStar pv
  rw [Real.exp_neg]
  have : 0 < Real.exp (ρ * t) := Real.exp_pos _
  field_simp

theorem piRate_cmp (ρ t r n : ℝ) (ht : 0 < t) (hr : 0 < r) (hn : 1 < n) :
    (ρ < piRate ρ t r n ↔ (n - 1) * r < n) ∧ (piRate ρ t r n = ρ ↔ (n - 1) * r = n) ∧
      (piRate ρ t r n < ρ ↔ n < (n - 1) * r) := by
  have hn1 : 0 < n - 1 := by linarith
  have hd : 0 < (n - 1) * r := by positivity
  have hx : 0 < n / ((n - 1) * r) := by positivity
  have ht' : 0 < 1 / t := by positivity
  unfold piRate
  refine ⟨?_, ?_, ?_⟩
  · rw [lt_add_iff_pos_right, mul_pos_iff_of_pos_left ht', Real.log_pos_iff hx.le,
      one_lt_div hd]
  · constructor
    · intro h
      have h0 : Real.log (n / ((n - 1) * r)) = 0 := by
        have : (1 / t) * Real.log (n / ((n - 1) * r)) = 0 := by linarith
        rcases mul_eq_zero.mp this with h | h
        · exact absurd h ht'.ne'
        · exact h
      have := Real.eq_one_of_pos_of_log_eq_zero hx h0
      rw [div_eq_one_iff_eq hd.ne'] at this
      exact this.symm
    · intro h
      have h1 : n / ((n - 1) * r) = 1 := by rw [h]; exact div_self (by linarith)
      rw [h1, Real.log_one]; ring
  · rw [add_lt_iff_neg_left]
    constructor
    · intro h
      have hL : Real.log (n / ((n - 1) * r)) < 0 := by
        by_contra hL
        push_neg at hL
        have := mul_nonneg ht'.le hL
        linarith
      exact (div_lt_one hd).mp ((Real.log_neg_iff hx).mp hL)
    · intro h
      exact mul_neg_of_pos_of_neg ht' ((Real.log_neg_iff hx).mpr ((div_lt_one hd).mpr h))

theorem nLong_gt_one {r : ℝ} (hr : 1 < r) : 1 < nLong r := by
  unfold nLong
  have : 0 < r - 1 := by linarith
  rw [lt_div_iff₀ this]; linarith

theorem entry_zero_iff (r n : ℝ) (hr : 1 < r) :
    (n - 1) * r = n ↔ n = nLong r := by
  unfold nLong
  have : 0 < r - 1 := by linarith
  rw [eq_div_iff this.ne']
  constructor <;> intro h <;> nlinarith

theorem entry_iff (r n : ℝ) (hr : 1 < r) :
    ((n - 1) * r < n ↔ n < nLong r) ∧ (n < (n - 1) * r ↔ nLong r < n) := by
  unfold nLong
  have : 0 < r - 1 := by linarith
  rw [lt_div_iff₀ this, div_lt_iff₀ this]
  constructor <;> constructor <;> intro h <;> nlinarith

theorem unbounded_entry (r n : ℝ) (hr : r ≤ 1) (hn : 1 < n) : (n - 1) * r < n := by
  have : 0 < n - 1 := by linarith
  nlinarith

/-! ### 3. Payoff at `t = 0` -/

theorem profit0_eq (R r n : ℝ) (hn : n ≠ 0) : profit0 R r n = R * (n + r - n * r) / n ^ 2 := by
  unfold profit0 aStar
  field_simp
  ring

theorem profit0_cmp (R r n : ℝ) (hR : 0 < R) (hn : 1 < n) :
    (0 < profit0 R r n ↔ (n - 1) * r < n) ∧ (profit0 R r n = 0 ↔ (n - 1) * r = n) ∧
      (profit0 R r n < 0 ↔ n < (n - 1) * r) := by
  have hn0 : (0 : ℝ) < n := by linarith
  rw [profit0_eq R r n hn0.ne']
  have hn2 : 0 < n ^ 2 := by positivity
  have e : n + r - n * r = n - (n - 1) * r := by ring
  rw [e]
  refine ⟨?_, ?_, ?_⟩
  · rw [div_pos_iff_of_pos_right hn2, mul_pos_iff_of_pos_left hR]; constructor <;> intro h <;> linarith
  · rw [div_eq_zero_iff]
    constructor
    · rintro (h | h)
      · rcases mul_eq_zero.mp h with h | h
        · exact absurd h hR.ne'
        · linarith
      · exact absurd h hn2.ne'
    · intro h; left; rw [show n - (n - 1) * r = 0 by linarith]; ring
  · rw [div_lt_iff₀ hn2, zero_mul]
    constructor
    · intro h
      by_contra hL
      push_neg at hL
      have := mul_nonneg hR.le (by linarith : 0 ≤ n - (n - 1) * r)
      linarith
    · intro h
      exact mul_neg_of_pos_of_neg hR (by linarith)

theorem profit0_zero_iff (R r n : ℝ) (hR : 0 < R) (hr : 1 < r) (hn : 1 < n) :
    profit0 R r n = 0 ↔ n = nLong r := by
  rw [(profit0_cmp R r n hR hn).2.1, entry_zero_iff r n hr]

theorem profit0_pos_of_le_one (R r n : ℝ) (hR : 0 < R) (hr : r ≤ 1) (hn : 1 < n) :
    0 < profit0 R r n :=
  (profit0_cmp R r n hR hn).1.mpr (unbounded_entry r n hr hn)

theorem profit0_hasDerivAt_r (R n r : ℝ) :
    HasDerivAt (fun r => R * (n + r - n * r) / n ^ 2) (R * (1 - n) / n ^ 2) r := by
  have h : HasDerivAt (fun r : ℝ => n + r - n * r) (1 - n) r := by
    simpa using ((hasDerivAt_id r).const_add n).sub ((hasDerivAt_id r).const_mul n)
  exact ((h.const_mul R).div_const (n ^ 2))

theorem profit0_deriv_neg (R n : ℝ) (hR : 0 < R) (hn : 1 < n) :
    R * (1 - n) / n ^ 2 < 0 := by
  have : 0 < n ^ 2 := by positivity
  have : R * (1 - n) < 0 := by nlinarith
  exact div_neg_of_neg_of_pos this (by positivity)

theorem profit0_strictAnti_r (R n : ℝ) (hR : 0 < R) (hn : 1 < n) {r₁ r₂ : ℝ} (h : r₁ < r₂) :
    profit0 R r₂ n < profit0 R r₁ n := by
  have hn0 : (0 : ℝ) < n := by linarith
  rw [profit0_eq R r₁ n hn0.ne', profit0_eq R r₂ n hn0.ne']
  have hn2 : 0 < n ^ 2 := by positivity
  rw [div_lt_div_iff_of_pos_right hn2]
  have := mul_pos hR (mul_pos (by linarith : 0 < n - 1) (sub_pos.mpr h))
  nlinarith [this]

/-! ### 4. Eq. (10) and the offset between expenditure and entry -/

theorem Tot_nLong (R r : ℝ) (hr : 1 < r) : Tot R r (nLong r) = R := by
  have h0 : r - 1 ≠ 0 := by linarith
  have hr0 : r ≠ 0 := by linarith
  have hn : nLong r ≠ 0 := by unfold nLong; positivity
  rw [Tot_eq R r _ hn]
  unfold nLong
  field_simp
  ring

theorem Tot_longrun_indep (R r₁ r₂ : ℝ) (h₁ : 1 < r₁) (h₂ : 1 < r₂) :
    Tot R r₁ (nLong r₁) = Tot R r₂ (nLong r₂) := by
  rw [Tot_nLong R r₁ h₁, Tot_nLong R r₂ h₂]

theorem Tot_longrun_pv (P ρ t r : ℝ) (hr : 1 < r) :
    Tot (pv P ρ t) r (nLong r) = P * Real.exp (-(ρ * t)) := by
  rw [Tot_nLong _ r hr]; rfl

theorem aStar_nLong (R r : ℝ) (hr : 1 < r) : aStar R r (nLong r) = R * (r - 1) / r := by
  have h0 : r - 1 ≠ 0 := by linarith
  have hr0 : r ≠ 0 := by linarith
  unfold aStar nLong
  field_simp
  ring

theorem aStar_nLong_strictMono (R : ℝ) (hR : 0 < R) {r₁ r₂ : ℝ} (h₁ : 1 < r₁) (h : r₁ < r₂) :
    aStar R r₁ (nLong r₁) < aStar R r₂ (nLong r₂) := by
  have h₂ : 1 < r₂ := by linarith
  rw [aStar_nLong R r₁ h₁, aStar_nLong R r₂ h₂]
  have p1 : 0 < r₁ := by linarith
  have p2 : 0 < r₂ := by linarith
  rw [div_lt_div_iff₀ p1 p2]
  nlinarith

theorem nLong_strictAnti {r₁ r₂ : ℝ} (h₁ : 1 < r₁) (h : r₁ < r₂) : nLong r₂ < nLong r₁ := by
  have h₂ : 1 < r₂ := by linarith
  unfold nLong
  have p1 : 0 < r₁ - 1 := by linarith
  have p2 : 0 < r₂ - 1 := by linarith
  rw [div_lt_div_iff₀ p2 p1]
  nlinarith

theorem aStar_strictMono_r (R n : ℝ) (hR : 0 < R) (hn : 1 < n) {r₁ r₂ : ℝ} (h : r₁ < r₂) :
    aStar R r₁ n < aStar R r₂ n := by
  unfold aStar
  have hn1 : 0 < n - 1 := by linarith
  have hn2 : 0 < n ^ 2 := by positivity
  rw [div_lt_div_iff_of_pos_right hn2]
  have := mul_pos (mul_pos hn1 hR) (sub_pos.mpr h)
  nlinarith [this]

theorem piRate_strictAnti_r (ρ t n : ℝ) (ht : 0 < t) (hn : 1 < n) {r₁ r₂ : ℝ} (h₁ : 0 < r₁)
    (h : r₁ < r₂) : piRate ρ t r₂ n < piRate ρ t r₁ n := by
  have h₂ : 0 < r₂ := by linarith
  have hn1 : 0 < n - 1 := by linarith
  unfold piRate
  have ht' : 0 < 1 / t := by positivity
  have hlog : Real.log (n / ((n - 1) * r₂)) < Real.log (n / ((n - 1) * r₁)) := by
    apply Real.log_lt_log (by positivity)
    apply div_lt_div_of_pos_left (by linarith) (by positivity)
    nlinarith
  nlinarith

/-! ### 5. Scope of "Cournot-Nash": the first-order condition is an equilibrium -/

theorem V_symm (R r n b : ℝ) (hb : 0 < b) (hn : 0 < n) : V R r n b b = R / n - b := by
  unfold V
  rw [winning_prob r n b hb hn]
  ring

theorem V_zero (R r n b : ℝ) (hr : 0 < r) : V R r n b 0 = 0 := by
  unfold V
  rw [Real.zero_rpow hr.ne']
  simp

theorem not_nash_of_loss (R r n : ℝ) (hR : 0 < R) (hr : 0 < r) (hn : 1 < n)
    (hloss : profit0 R r n < 0) :
    V R r n (aStar R r n) (aStar R r n) < V R r n (aStar R r n) 0 := by
  have hn0 : (0 : ℝ) < n := by linarith
  have hb : 0 < aStar R r n := by unfold aStar; have : 0 < n - 1 := by linarith
                                  positivity
  rw [V_symm R r n _ hb hn0, V_zero R r n _ hr]
  unfold profit0 at hloss
  linarith

private theorem share_tangent (c k y : ℝ) (hc : 0 < c) (hk : 0 < k) (hy : 0 ≤ y) :
    y / (y + k) ≤ c / (c + k) + k * (y - c) / (c + k) ^ 2 := by
  have h1 : 0 < y + k := by linarith
  have h2 : 0 < c + k := by linarith
  have key : c / (c + k) + k * (y - c) / (c + k) ^ 2 - y / (y + k)
      = k * (y - c) ^ 2 / ((y + k) * (c + k) ^ 2) := by
    field_simp
    ring
  have : 0 ≤ k * (y - c) ^ 2 / ((y + k) * (c + k) ^ 2) := by positivity
  linarith

private theorem rpow_le_tangent (b a r : ℝ) (hb : 0 < b) (ha : 0 ≤ a) (hr0 : 0 < r)
    (hr : r ≤ 1) : a ^ r ≤ b ^ r * (1 + r * (a / b - 1)) := by
  have hab : 0 ≤ a / b := div_nonneg ha hb.le
  have hB := rpow_one_add_le_one_add_mul_self (s := a / b - 1) (by linarith) hr0.le hr
  have e : a = b * (a / b) := by field_simp
  have : a ^ r = b ^ r * (a / b) ^ r := by
    conv_lhs => rw [e]
    exact Real.mul_rpow hb.le hab
  rw [this]
  apply mul_le_mul_of_nonneg_left _ (Real.rpow_nonneg hb.le r)
  have e2 : (1 : ℝ) + (a / b - 1) = a / b := by ring
  rw [e2] at hB
  exact hB

private theorem rpow_ge_tangent (b a r : ℝ) (hb : 0 < b) (ha : 0 < a) (hr : 1 ≤ r) :
    a ^ r * (1 + r * (b / a - 1)) ≤ b ^ r := by
  have hab : 0 < b / a := div_pos hb ha
  have hB := one_add_mul_self_le_rpow_one_add (s := b / a - 1) (by linarith) hr
  have e2 : (1 : ℝ) + (b / a - 1) = b / a := by ring
  rw [e2] at hB
  have e : b = a * (b / a) := by field_simp
  have : b ^ r = a ^ r * (b / a) ^ r := by
    conv_lhs => rw [e]
    exact Real.mul_rpow ha.le hab.le
  rw [this]
  exact mul_le_mul_of_nonneg_left hB (Real.rpow_nonneg ha.le r)

private theorem nash_core (R r n b a : ℝ) (hR : 0 < R) (hr0 : 0 < r) (hr : r ≤ 1) (hn : 1 < n)
    (hb : 0 < b) (ha : 0 ≤ a) (hbR : R * (r * (n - 1)) = n ^ 2 * b) :
    R * (a ^ r / (a ^ r + (n - 1) * b ^ r)) ≤ R / n + (a - b) := by
  have hn0 : (0 : ℝ) < n := by linarith
  have hn1 : 0 < n - 1 := by linarith
  obtain ⟨c, hcdef⟩ : ∃ c, c = b ^ r := ⟨_, rfl⟩
  obtain ⟨k, hkdef⟩ : ∃ k, k = (n - 1) * c := ⟨_, rfl⟩
  obtain ⟨Y, hY⟩ : ∃ Y, Y = c * (1 + r * (a / b - 1)) := ⟨_, rfl⟩
  have hc : 0 < c := hcdef ▸ Real.rpow_pos_of_pos hb r
  have hk : 0 < k := by rw [hkdef]; positivity
  have hay : a ^ r ≤ Y := by rw [hY, hcdef]; exact rpow_le_tangent b a r hb ha hr0 hr
  have hY0 : 0 ≤ Y := le_trans (Real.rpow_nonneg ha r) hay
  have hmono : a ^ r / (a ^ r + k) ≤ Y / (Y + k) := by
    have h1 : 0 ≤ a ^ r := Real.rpow_nonneg ha r
    rw [div_le_div_iff₀ (by linarith) (by linarith)]
    nlinarith
  have htan := share_tangent c k Y hc hk hY0
  have hck : c + k = n * c := by rw [hkdef]; ring
  have hval : c / (c + k) + k * (Y - c) / (c + k) ^ 2
      = 1 / n + (n - 1) * r * (a - b) / (n ^ 2 * b) := by
    have hc0 : c ≠ 0 := hc.ne'
    have hb0 : b ≠ 0 := hb.ne'
    rw [hck, hkdef, hY]
    field_simp
    ring
  have hsum : a ^ r / (a ^ r + k) ≤ 1 / n + (n - 1) * r * (a - b) / (n ^ 2 * b) := by
    rw [← hval]; exact le_trans hmono htan
  rw [← hcdef, ← hkdef]
  have h1 : R * (a ^ r / (a ^ r + k)) ≤ R * (1 / n + (n - 1) * r * (a - b) / (n ^ 2 * b)) :=
    mul_le_mul_of_nonneg_left hsum hR.le
  have h2 : R * ((n - 1) * r * (a - b) / (n ^ 2 * b)) = a - b := by
    have hb0 : b ≠ 0 := hb.ne'
    have hn20 : n ^ 2 ≠ 0 := by positivity
    rw [mul_div_assoc', div_eq_iff (by positivity)]
    nlinarith [hbR]
  have h3 : R * (1 / n + (n - 1) * r * (a - b) / (n ^ 2 * b)) = R / n + (a - b) := by
    rw [mul_add, h2]; ring
  linarith

theorem nash_of_le_one (R r n : ℝ) (hR : 0 < R) (hr0 : 0 < r) (hr : r ≤ 1) (hn : 1 < n)
    (a : ℝ) (ha : 0 ≤ a) :
    V R r n (aStar R r n) a ≤ V R r n (aStar R r n) (aStar R r n) := by
  have hn0 : (0 : ℝ) < n := by linarith
  have hn1 : 0 < n - 1 := by linarith
  have hb : 0 < aStar R r n := by unfold aStar; positivity
  have hbR : R * (r * (n - 1)) = n ^ 2 * aStar R r n := by
    unfold aStar; field_simp
  have hcore := nash_core R r n (aStar R r n) a hR hr0 hr hn hb ha hbR
  rw [V_symm R r n _ hb hn0]
  unfold V
  linarith

theorem nash_longrun (R r : ℝ) (hR : 0 < R) (hr : 1 < r) (a : ℝ) (ha : 0 ≤ a) :
    V R r (nLong r) (aStar R r (nLong r)) a ≤
      V R r (nLong r) (aStar R r (nLong r)) (aStar R r (nLong r)) := by
  have hr0 : 0 < r := by linarith
  have hr1 : 0 < r - 1 := by linarith
  have hnL : 1 < nLong r := nLong_gt_one hr
  have hn0 : (0 : ℝ) < nLong r := by linarith
  have hn1 : nLong r - 1 = 1 / (r - 1) := by unfold nLong; field_simp; ring
  obtain ⟨b, hbdef⟩ : ∃ b, b = aStar R r (nLong r) := ⟨_, rfl⟩
  rw [← hbdef]
  have hbd : b = R * (r - 1) / r := by rw [hbdef]; exact aStar_nLong R r hr
  have hb : 0 < b := by rw [hbd]; positivity
  have hRb : R = b * r / (r - 1) := by rw [hbd]; field_simp
  have hV0 : V R r (nLong r) b b = 0 := by
    rw [V_symm R r _ b hb hn0]
    have h0 := (profit0_zero_iff R r (nLong r) hR hr hnL).mpr rfl
    unfold profit0 at h0
    rw [hbdef]
    linarith
  rw [hV0]
  rcases ha.eq_or_lt with h | h
  · rw [← h, V_zero R r _ b hr0]
  · have hxr : 0 < a ^ r := Real.rpow_pos_of_pos h r
    have hbr : 0 < b ^ r := Real.rpow_pos_of_pos hb r
    have hn1' : 0 < nLong r - 1 := by linarith
    have hden : 0 < a ^ r + (nLong r - 1) * b ^ r := by
      have : 0 < (nLong r - 1) * b ^ r := by positivity
      linarith
    have hB := rpow_ge_tangent b a r hb h hr.le
    have hxB : a * b ^ r ≥ a ^ r * (a * (1 - r) + r * b) := by
      have h4 := mul_le_mul_of_nonneg_left hB h.le
      have e3 : a * (a ^ r * (1 + r * (b / a - 1))) = a ^ r * (a * (1 - r) + r * b) := by
        field_simp; ring
      rw [e3] at h4
      linarith
    have key : R * a ^ r ≤ a * (a ^ r + (nLong r - 1) * b ^ r) := by
      rw [hn1, hRb]
      have ha0 : a ≠ 0 := h.ne'
      have hr10 : r - 1 ≠ 0 := hr1.ne'
      rw [div_mul_eq_mul_div, div_le_iff₀ hr1]
      have : a * (a ^ r + 1 / (r - 1) * b ^ r) * (r - 1) =
          (r - 1) * a * a ^ r + a * b ^ r := by field_simp
      rw [this]
      nlinarith [hxB]
    have hq : R * (a ^ r / (a ^ r + (nLong r - 1) * b ^ r)) ≤ a := by
      rw [← mul_div_assoc, div_le_iff₀ hden]
      linarith [key]
    unfold V
    linarith

end Corcoran1984
