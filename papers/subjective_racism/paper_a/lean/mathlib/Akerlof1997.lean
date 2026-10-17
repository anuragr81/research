import Mathlib

/-! # Akerlof (1997), "Social Distance and Social Decisions", Econometrica 65(5)

The status model, eqs. (1) and (2), the conformist model, eqs. (3) and (4),
the quadratic model, eq. (5), the benefit of social exchange of Section 3 and
the utility of eq. (6), in the paper's notation. The theorems named
`contrast_*` are ours. They compare the paper's benefit with the quadratic
loss of Paper A1 and are not results of the paper. -/

namespace Akerlof1997

open Filter Topology

noncomputable section

/-! ### The status model, eqs. (1) and (2) -/

/-- Eq. (1). `xbar` is the choice of everyone else. -/
def Ustatus (a b c d xbar x : ℝ) : ℝ := -d * (xbar - x) - a * x ^ 2 + b * x + c

/-- Eq. (2). With `a > 0` the choice `(b + d)/2a` maximises eq. (1), whatever
everyone else chooses. -/
theorem eq2_status_opt (a b c d xbar x : ℝ) (ha : 0 < a) :
    Ustatus a b c d xbar x ≤ Ustatus a b c d xbar ((b + d) / (2 * a)) := by
  unfold Ustatus
  have hs : 2 * a * ((b + d) / (2 * a)) = b + d := by field_simp
  generalize (b + d) / (2 * a) = s at hs ⊢
  have key : (-d * (xbar - s) - a * s ^ 2 + b * s + c) - (-d * (xbar - x) - a * x ^ 2 + b * x + c)
      = a * (x - s) ^ 2 := by linear_combination (x - s) * hs
  nlinarith [mul_nonneg ha.le (sq_nonneg (x - s))]

/-- The excess over the intrinsic optimum `b/2a` is `d/2a`. -/
theorem eq2_excess (a b d : ℝ) : (b + d) / (2 * a) - b / (2 * a) = d / (2 * a) := by ring

/-! ### The conformist model, eqs. (3) and (4) -/

/-- Eq. (3). -/
def Uconf (a b c d xbar x : ℝ) : ℝ := -d * |x - xbar| - a * x ^ 2 + b * x + c

/-- Eq. (4) and the zone of equilibria. When everyone else chooses `xbar` in
`[(b − d)/2a, (b + d)/2a]`, choosing `xbar` maximises eq. (3). -/
theorem eq4_zone (a b c d xbar x : ℝ) (ha : 0 < a)
    (h1 : (b - d) / (2 * a) ≤ xbar) (h2 : xbar ≤ (b + d) / (2 * a)) :
    Uconf a b c d xbar x ≤ Uconf a b c d xbar xbar := by
  have h2a : 0 < 2 * a := by linarith
  rw [div_le_iff₀ h2a] at h1
  rw [le_div_iff₀ h2a] at h2
  have hab : (x - xbar) * (b - 2 * a * xbar) ≤ |x - xbar| * d := by
    rcases le_total 0 (x - xbar) with h | h
    · rw [abs_of_nonneg h]; nlinarith
    · rw [abs_of_nonpos h]; nlinarith
  unfold Uconf
  rw [sub_self, abs_zero]
  nlinarith [sq_nonneg (x - xbar), mul_nonneg ha.le (sq_nonneg (x - xbar))]

/-- Control for eq. (4). Outside the zone, `xbar` is not a best reply. With
`a = 1`, `b = c = 0`, `d = 1` the zone is `[−1/2, 1/2]`, and at `xbar = 1`
the choice `1/2` does better. -/
theorem control_conform_outside_zone :
    Uconf 1 0 0 1 1 1 < Uconf 1 0 0 1 1 (1 / 2) := by
  unfold Uconf
  rw [show (1 / 2 : ℝ) - 1 = -(1 / 2) by norm_num, abs_neg]
  norm_num [abs_of_pos]

/-! ### The quadratic model, eq. (5) -/

/-- Eq. (5). -/
def Uquad (a b c d xbar x : ℝ) : ℝ := -a * x ^ 2 + b * x + c - d * (x - xbar) ^ 2

/-- Eq. (5) has one equilibrium. If `xbar` is a best reply to everyone else
choosing `xbar`, then `xbar = b/2a`. -/
theorem eq5_unique (a b c d xbar : ℝ) (ha : 0 < a) (hd : 0 ≤ d)
    (hmax : ∀ x, Uquad a b c d xbar x ≤ Uquad a b c d xbar xbar) :
    xbar = b / (2 * a) := by
  have had : 0 < a + d := by linarith
  have h := hmax (xbar + (b - 2 * a * xbar) / (2 * (a + d)))
  have e : Uquad a b c d xbar (xbar + (b - 2 * a * xbar) / (2 * (a + d)))
      = Uquad a b c d xbar xbar + (b - 2 * a * xbar) ^ 2 / (4 * (a + d)) := by
    unfold Uquad
    field_simp
    ring
  rw [e] at h
  have h4 : (b - 2 * a * xbar) ^ 2 / (4 * (a + d)) ≤ 0 := by linarith
  have hsq : (b - 2 * a * xbar) ^ 2 ≤ 0 := by
    have := (div_nonpos_iff.mp h4)
    rcases this with ⟨_, h'⟩ | ⟨_, h'⟩
    · linarith
    · nlinarith
  have hk : b - 2 * a * xbar = 0 := by nlinarith [sq_nonneg (b - 2 * a * xbar)]
  field_simp
  linarith

/-- The converse. `b/2a` is a best reply to everyone else choosing it. -/
theorem eq5_opt (a b c d x : ℝ) (ha : 0 < a) (hd : 0 ≤ d) :
    Uquad a b c d (b / (2 * a)) x ≤ Uquad a b c d (b / (2 * a)) (b / (2 * a)) := by
  unfold Uquad
  have hs : 2 * a * (b / (2 * a)) = b := by field_simp
  generalize b / (2 * a) = s at hs ⊢
  have key : (-a * s ^ 2 + b * s + c - d * (s - s) ^ 2) - (-a * x ^ 2 + b * x + c - d * (x - s) ^ 2)
      = (a + d) * (x - s) ^ 2 := by linear_combination (x - s) * hs
  nlinarith [mul_nonneg (by linarith : (0:ℝ) ≤ a + d) (sq_nonneg (x - s))]

/-! ### The benefit of social exchange, Section 3 -/

/-- The expected benefit of trade between two people at initial distance `d0`
and expected acquired distance `d`, `e/((f + d0)(g + d))`. -/
def benefit (e f g d0 d : ℝ) : ℝ := e / ((f + d0) * (g + d))

/-- The benefit does not rise as the acquired distance grows. -/
theorem benefit_antitone (e f g d0 d d' : ℝ) (he : 0 ≤ e) (hf : 0 < f + d0)
    (hg : 0 < g + d) (hdd : d ≤ d') :
    benefit e f g d0 d' ≤ benefit e f g d0 d := by
  unfold benefit
  have : 0 < (f + d0) * (g + d) := mul_pos hf hg
  gcongr

/-- The benefit at any distance is at most its value at zero distance, which
is finite, the first modification of the pure gravity model. -/
theorem benefit_le_at_zero (e f g d0 d : ℝ) (he : 0 ≤ e) (hf : 0 < f + d0)
    (hg : 0 < g) (hd : 0 ≤ d) :
    benefit e f g d0 d ≤ benefit e f g d0 0 := by
  apply benefit_antitone e f g d0 0 d he hf (by linarith) hd

/-- As the acquired distance grows the benefit tends to zero. -/
theorem benefit_tendsto_zero (e f g d0 : ℝ) (hf : 0 < f + d0) :
    Tendsto (fun d => benefit e f g d0 d) atTop (𝓝 0) := by
  unfold benefit
  apply tendsto_const_nhds.div_atTop
  exact Tendsto.const_mul_atTop hf (tendsto_atTop_add_const_left _ g tendsto_id)

/-- Eq. (6). The utility of person `i` choosing `x`, under static expectations
that every other person stays at the initial position `x0 j`. -/
def U6 {ι : Type*} [Fintype ι] [DecidableEq ι] (e f g a b c : ℝ) (x0 : ι → ℝ)
    (i : ι) (x : ℝ) : ℝ :=
  ∑ j ∈ Finset.univ.erase i, benefit e f g |x0 i - x0 j| |x - x0 j| + (-a * x ^ 2 + b * x + c)

/-- With no value of social exchange, eq. (6) is maximised at the intrinsic
optimum `b/2a`. -/
theorem eq6_intrinsic_opt {ι : Type*} [Fintype ι] [DecidableEq ι] (f g a b c : ℝ)
    (x0 : ι → ℝ) (i : ι) (x : ℝ) (ha : 0 < a) :
    U6 0 f g a b c x0 i x ≤ U6 0 f g a b c x0 i (b / (2 * a)) := by
  unfold U6 benefit
  simp only [zero_div, Finset.sum_const_zero, zero_add]
  have hs : 2 * a * (b / (2 * a)) = b := by field_simp
  generalize b / (2 * a) = s at hs ⊢
  have key : (-a * s ^ 2 + b * s + c) - (-a * x ^ 2 + b * x + c) = a * (x - s) ^ 2 := by
    linear_combination (x - s) * hs
  nlinarith [mul_nonneg ha.le (sq_nonneg (x - s))]

/-! ### Contrasts with the quadratic loss of Paper A1 (ours) -/

/-- Contrast, ours. Two classes `m − s` and `m + s` with equal weight, read by
an observer below both, so that the distances are `D − s` and `D + s` with
`D = g + m − y`. The mean benefit `1/(g + distance)` does not fall as the
spread grows. -/
theorem contrast_spread_helps_distant (D s s' : ℝ) (hs : 0 ≤ s) (hss : s ≤ s')
    (hD : s' < D) :
    (1 / (D - s) + 1 / (D + s)) / 2 ≤ (1 / (D - s') + 1 / (D + s')) / 2 := by
  have h1 : 0 < D - s := by linarith
  have h2 : 0 < D + s := by linarith
  have h3 : 0 < D - s' := by linarith
  have h4 : 0 < D + s' := by linarith
  have e1 : (1 / (D - s) + 1 / (D + s)) / 2 = D / ((D - s) * (D + s)) := by
    field_simp; ring
  have e2 : (1 / (D - s') + 1 / (D + s')) / 2 = D / ((D - s') * (D + s')) := by
    field_simp; ring
  rw [e1, e2]
  have hD0 : 0 ≤ D := by linarith
  have hp : 0 < (D - s') * (D + s') := mul_pos h3 h4
  have hle : (D - s') * (D + s') ≤ (D - s) * (D + s) := by nlinarith
  exact div_le_div_of_nonneg_left hD0 hp hle

/-- Contrast, ours. An observer at the mean of the same two classes is at
distance `s` from each, and the mean benefit does not rise as the spread
grows. -/
theorem contrast_spread_hurts_at_mean (g s s' : ℝ) (hg : 0 < g) (hs : 0 ≤ s)
    (hss : s ≤ s') : 1 / (g + s') ≤ 1 / (g + s) := by
  apply one_div_le_one_div_of_le (by linarith)
  linarith

/-- Contrast, ours. Under the quadratic loss the mean squared distance from
any observer to the two classes is the squared gap to the mean plus `s²`, so
the spread lowers the utility of every observer. -/
theorem contrast_quadratic_spread (m y s : ℝ) :
    ((m - s - y) ^ 2 + (m + s - y) ^ 2) / 2 = (m - y) ^ 2 + s ^ 2 := by ring

/-- Contrast, ours. For any benefit that does not rise in distance, an
observer above every member of a group does not lose when every member's
class rises by `t`, so long as each stays at or below the observer. -/
theorem contrast_shift_up_above {ι : Type*} (S : Finset ι) (p x : ι → ℝ)
    (φ : ℝ → ℝ) (hφ : AntitoneOn φ (Set.Ici 0)) (hp : ∀ i ∈ S, 0 ≤ p i)
    (y t : ℝ) (ht : 0 ≤ t) (habove : ∀ i ∈ S, x i + t ≤ y) :
    ∑ i ∈ S, p i * φ |y - x i| ≤ ∑ i ∈ S, p i * φ |y - (x i + t)| := by
  apply Finset.sum_le_sum
  intro i hi
  have h := habove i hi
  have e1 : |y - x i| = y - x i := abs_of_nonneg (by linarith)
  have e2 : |y - (x i + t)| = y - (x i + t) := abs_of_nonneg (by linarith)
  rw [e1, e2]
  apply mul_le_mul_of_nonneg_left _ (hp i hi)
  apply hφ (by simp; linarith) (by simp; linarith)
  linarith

/-- Contrast, ours. The same shift does not help an observer below every
member. -/
theorem contrast_shift_up_below {ι : Type*} (S : Finset ι) (p x : ι → ℝ)
    (φ : ℝ → ℝ) (hφ : AntitoneOn φ (Set.Ici 0)) (hp : ∀ i ∈ S, 0 ≤ p i)
    (y t : ℝ) (ht : 0 ≤ t) (hbelow : ∀ i ∈ S, y ≤ x i) :
    ∑ i ∈ S, p i * φ |y - (x i + t)| ≤ ∑ i ∈ S, p i * φ |y - x i| := by
  apply Finset.sum_le_sum
  intro i hi
  have h := hbelow i hi
  have e1 : |y - x i| = x i - y := by rw [abs_sub_comm]; exact abs_of_nonneg (by linarith)
  have e2 : |y - (x i + t)| = x i + t - y := by
    rw [abs_sub_comm]; exact abs_of_nonneg (by linarith)
  rw [e1, e2]
  apply mul_le_mul_of_nonneg_left _ (hp i hi)
  apply hφ (by simp; linarith) (by simp; linarith)
  linarith

/-- Control, ours. Between the two cases the benefit and the quadratic loss
part. Classes `0` and `10` with equal weight have standing `5`, and an
observer at `6` stands above it. A shift of `1` leaves the standing at `6`.
The mean squared distance falls from `26` to `25`, as Paper A1's range
predicts, while the mean benefit `1/(1 + distance)` falls from `12/70` to
`1/6`. -/
theorem control_benefit_range_differs :
    (1 / (1 + 5) + 1 / (1 + 5)) / 2 < ((1 : ℝ) / (1 + 6) + 1 / (1 + 4)) / 2 ∧
      ((5 : ℝ) ^ 2 + 5 ^ 2) / 2 < (6 ^ 2 + 4 ^ 2) / 2 := by
  constructor <;> norm_num

end

end Akerlof1997
