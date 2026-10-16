import Mathlib

/-! Corneo and Jeanne, "Conspicuous Consumption and the Existence of Upward
Sloping Demand Curves", University of Bonn Discussion Paper A-461 (December
1994), Sections 2 and 3, in the paper's notation. Consumers are ranked by
`r ∈ [0, 1]`, with rank 0 the richest. `a` is the rank utility, `ā` its mean,
`n` the number of buyers of the conspicuous good, and `σ(n)` its signaling
value, the mean rank utility of buyers minus that of non-buyers, eq. (8). -/

namespace CorneoJeanne1994

open Real intervalIntegral Set

/-- `ā`, the average rank utility over the whole population. -/
noncomputable def abar (a : ℝ → ℝ) : ℝ := ∫ r in (0 : ℝ)..1, a r

/-- Eq. (8), first form. Buyers are the ranks `[0, n]`, and the public values
each group at its own mean rank utility. -/
noncomputable def σ (a : ℝ → ℝ) (n : ℝ) : ℝ :=
  (∫ r in (0 : ℝ)..n, a r) / n - (∫ r in n..1, a r) / (1 - n)

/-- Eq. (8), the identity of its two forms. -/
theorem eq8 {a : ℝ → ℝ} (ha : Continuous a) {n : ℝ} (hn0 : 0 < n) (hn1 : n < 1) :
    σ a n = 1 / (n * (1 - n)) * ∫ r in (0 : ℝ)..n, (a r - abar a) := by
  have hi : ∀ b c : ℝ, IntervalIntegrable a MeasureTheory.volume b c :=
    fun b c => ha.intervalIntegrable b c
  have hsplit : (∫ r in (0 : ℝ)..n, a r) + (∫ r in n..1, a r) = abar a :=
    integral_add_adjacent_intervals (hi 0 n) (hi n 1)
  have hsub : (∫ r in (0 : ℝ)..n, (a r - abar a)) = (∫ r in (0 : ℝ)..n, a r) - n * abar a := by
    rw [integral_sub (hi 0 n) _root_.intervalIntegrable_const, integral_const]
    simp
  unfold σ
  rw [hsub]
  have h1 : (1 - n) ≠ 0 := by linarith
  have h0 : n ≠ 0 := ne_of_gt hn0
  have e : (∫ r in n..1, a r) = abar a - ∫ r in (0 : ℝ)..n, a r := by linarith
  rw [e]
  field_simp
  ring

/-- Eq. (9). If a signaling value `σ` satisfies eq. (8) on `]0, 1[` and is
differentiable at `n`, the rank utility that generates it is
`a(n) = ā + (1 − 2n) σ(n) + n (1 − n) σ'(n)`. -/
theorem eq9 {a s : ℝ → ℝ} {s' n : ℝ} (ha : Continuous a) (hn : n ∈ Ioo (0 : ℝ) 1)
    (h8 : ∀ m ∈ Ioo (0 : ℝ) 1, m * (1 - m) * s m = (∫ r in (0 : ℝ)..m, a r) - m * abar a)
    (hd : HasDerivAt s s' n) :
    a n = abar a + (1 - 2 * n) * s n + n * (1 - n) * s' := by
  have hL : HasDerivAt (fun m => m * (1 - m) * s m)
      ((1 * (1 - n) + n * (-1)) * s n + n * (1 - n) * s') n := by
    have h1 := (hasDerivAt_id n).mul ((hasDerivAt_const n (1 : ℝ)).sub (hasDerivAt_id n))
    have := h1.mul hd
    simpa [id] using this
  have hR : HasDerivAt (fun m => (∫ r in (0 : ℝ)..m, a r) - m * abar a)
      (a n - 1 * abar a) n := by
    have h1 := (ha.integral_hasStrictDerivAt 0 n).hasDerivAt
    exact h1.sub ((hasDerivAt_id n).mul_const (abar a))
  have hev : (fun m => (∫ r in (0 : ℝ)..m, a r) - m * abar a) =ᶠ[nhds n]
      (fun m => m * (1 - m) * s m) := by
    filter_upwards [isOpen_Ioo.mem_nhds hn] with m hm
    exact (h8 m hm).symm
  have hR' := hR.congr_of_eventuallyEq hev.symm
  have := hL.unique hR'
  linarith

/-- Eq. (11). The class `σ(n) = α + β n^γ` of eq. (10), put into eq. (9) with
`σ'(r) = β γ r^{γ − 1}`, gives the rank utility of eq. (11). -/
theorem eq11 {ab α β γ r : ℝ} (hr : 0 < r) :
    ab + (1 - 2 * r) * (α + β * r ^ γ) + r * (1 - r) * (β * γ * r ^ (γ - 1))
      = ab + α * (1 - 2 * r) + β * r ^ γ * (1 + γ - (2 + γ) * r) := by
  rw [Real.rpow_sub_one (ne_of_gt hr)]
  field_simp
  ring

/-- What concavity of `u` supplies to Proposition 1. For a concave `u`, the
utility cost `u(y) − u(y − p)` of a price `p ≥ 0` falls in income `y`. -/
theorem concave_increment {u : ℝ → ℝ} (hu : ConcaveOn ℝ univ u) {y y' p : ℝ}
    (hp : 0 ≤ p) (hyy : y' ≤ y) : u y - u (y - p) ≤ u y' - u (y' - p) := by
  rcases eq_or_lt_of_le hp with h0 | hp
  · subst h0; simp
  rcases eq_or_lt_of_le hyy with h1 | hyy
  · subst h1; exact le_rfl
  have hf : ConvexOn ℝ univ (fun t => -u t) := hu.neg
  -- slope from y' − p to y' is at most the slope from y' − p to y
  have s1 := hf.secant_mono (a := y' - p) (x := y') (y := y) (mem_univ _) (mem_univ _)
    (mem_univ _) (by linarith) (by linarith) hyy.le
  -- slope from y' − p to y is at most the slope from y − p to y
  have s2 := hf.secant_mono (a := y) (x := y' - p) (y := y - p) (mem_univ _) (mem_univ _)
    (mem_univ _) (by linarith) (by linarith) (by linarith)
  have d1 : y' - (y' - p) = p := by ring
  have d2 : y - p - y = -p := by ring
  rw [d1] at s1
  rw [d2] at s2
  have hq : 0 < y - (y' - p) := by linarith
  have hq' : y' - p - y < 0 := by linarith
  rw [div_le_div_iff₀ hp hq] at s1
  have e3 : y' - p - y = -(y - (y' - p)) := by ring
  rw [e3, div_neg, div_neg, neg_le_neg_iff, div_le_div_iff₀ hp hq] at s2
  have key : (u y - u (y - p) - (u y' - u (y' - p))) * (y - (y' - p)) ≤ 0 := by
    nlinarith
  by_contra hc
  push_neg at hc
  have : 0 < (u y - u (y - p) - (u y' - u (y' - p))) * (y - (y' - p)) :=
    mul_pos (by linarith) hq
  linarith

/-- Proposition 1, the snobbish case. Let `y_n` fall in rank, let `u` be
increasing with a utility cost of the price that falls in income (as concavity
gives), and let `p(n)` solve eq. (12), `σ(n) = u(y_n) − u(y_n − p)`. If
`σ(n') < σ(n)` for `n < n'`, then `p(n') < p(n)`. -/
theorem prop1_snob {u : ℝ → ℝ} (hu : Monotone u) (hc : ConcaveOn ℝ univ u)
    {y : ℝ → ℝ} (hy : Antitone y) {s : ℝ → ℝ} {n n' p p' : ℝ} (hn : n < n')
    (hs : s n' < s n) (hp : 0 ≤ p) (h12 : s n = u (y n) - u (y n - p))
    (h12' : s n' = u (y n') - u (y n' - p')) : p' < p := by
  by_contra hcon
  push_neg at hcon
  have hyy : y n' ≤ y n := hy hn.le
  have m1 : u (y n' - p') ≤ u (y n' - p) := hu (by linarith)
  have m2 := concave_increment hc hp hyy
  linarith

/-- Section 3 with `μ = λ = 1`, `γ = 2` and `ū = 0`. With `u(c) = −e^{−c}` and
`N(y) = e^{−y}`, the rank-`n` income is `y_n = −log n`, and
`p(n) = log(1 + n)`, eq. (16), solves eq. (12) for `σ(n) = n²`. -/
theorem example_eq16 {n : ℝ} (hn : 0 < n) :
    n ^ 2 = (-Real.exp (-(-Real.log n))) - (-Real.exp (-(-Real.log n - Real.log (1 + n)))) := by
  have h1 : 0 < 1 + n := by linarith
  have e1 : -(-Real.log n) = Real.log n := by ring
  have e2 : -(-Real.log n - Real.log (1 + n)) = Real.log n + Real.log (1 + n) := by ring
  rw [e1, e2, Real.exp_add, Real.exp_log hn, Real.exp_log h1]
  ring

/-- Control: Proposition 1 needs snobbism. In the conformist example above,
`σ(n) = n²` rises in `n` and so does the price, `p(1/4) < p(1/2)`, with the
good affordable at `n = 1/2` (`n^γ + n^{μ/λ} ≤ 1`). -/
theorem control_conformist_price_rises :
    (1 / 4 : ℝ) ^ 2 < (1 / 2) ^ 2 ∧ (1 / 2 : ℝ) ^ 2 + 1 / 2 ≤ 1 ∧
      Real.log (1 + 1 / 4) < Real.log (1 + 1 / 2) := by
  refine ⟨by norm_num, by norm_num, ?_⟩
  exact Real.log_lt_log (by norm_num) (by norm_num)

/-- Control: the positive signaling value needs a rank utility that does not
rise in income. With `a(r) = r`, worst at the top, `σ(n) = −1/2` at every
`n ∈ ]0, 1[`. -/
theorem control_rising_rank_utility {n : ℝ} (hn0 : 0 < n) (hn1 : n < 1) :
    σ (fun r => r) n = -1 / 2 := by
  unfold σ
  rw [integral_id, integral_id]
  have h1 : (1 - n) ≠ 0 := by linarith
  have h0 : n ≠ 0 := ne_of_gt hn0
  field_simp
  ring

/-! ### The rest of Sections 2 to 4 and the Appendix -/

/-- Eqs. (1) and (3). Utility of a consumer with income `y` who buys
(`δ = 1`) or not (`δ = 0`) at price `p`, with status value `v δ`. -/
noncomputable def Ur (u : ℝ → ℝ) (v : ℕ → ℝ) (y p : ℝ) (δ : ℕ) : ℝ := u (y - p * δ) + v δ

/-- Eq. (4). The consumer buys exactly when the signaling value
`σ = v(1) − v(0)` exceeds the utility cost `u(y) − u(y − p)`. -/
theorem eq4_buy {u : ℝ → ℝ} {v : ℕ → ℝ} {y p : ℝ} :
    Ur u v y p 0 < Ur u v y p 1 ↔ u y - u (y - p) < v 1 - v 0 := by
  unfold Ur; simp; constructor <;> intro h <;> linarith

/-- Eqs. (5) to (7). Under concave `u` the buyers form an upper set of income:
if a consumer buys, so does every richer consumer. -/
theorem buyers_upper {u : ℝ → ℝ} (hc : ConcaveOn ℝ univ u) {σ' y y' p : ℝ} (hp : 0 ≤ p)
    (hyy : y ≤ y') (hbuy : u y - u (y - p) < σ') : u y' - u (y' - p) < σ' :=
  lt_of_le_of_lt (concave_increment hc hp hyy) hbuy

/-- Eq. (8) and Bayes' rule. Buyers' and non-buyers' mean rank utilities,
weighted by their shares, give `ā`. This is why the planner of eq. (18) sees
status only as the constant `ā`. -/
theorem eq18_status_sums {a : ℝ → ℝ} (ha : Continuous a) {n : ℝ} (hn0 : 0 < n) (hn1 : n < 1) :
    n * ((∫ r in (0 : ℝ)..n, a r) / n) + (1 - n) * ((∫ r in n..1, a r) / (1 - n)) = abar a := by
  have h1 : (1 - n) ≠ 0 := by linarith
  have h0 : n ≠ 0 := ne_of_gt hn0
  rw [mul_div_cancel₀ _ h0, mul_div_cancel₀ _ h1]
  exact integral_add_adjacent_intervals (ha.intervalIntegrable 0 n) (ha.intervalIntegrable n 1)

/-- `G(n) = ∫_0^n (a − ā)`, the numerator of the second form of eq. (8). -/
noncomputable def G (a : ℝ → ℝ) (n : ℝ) : ℝ := ∫ r in (0 : ℝ)..n, (a r - abar a)

theorem G_one {a : ℝ → ℝ} (ha : Continuous a) : G a 1 = 0 := by
  unfold G
  rw [integral_sub (ha.intervalIntegrable 0 1) _root_.intervalIntegrable_const, integral_const]
  simp [abar]

/-- The limit `n → 0` of eq. (8), `σ → a(0) − ā`, by L'Hôpital's rule as the
paper says. -/
theorem sigma_lim_zero {a : ℝ → ℝ} (ha : Continuous a) :
    Filter.Tendsto (σ a) (nhdsWithin 0 (Ioi 0)) (nhds (a 0 - abar a)) := by
  have hca : Continuous fun r => a r - abar a := ha.sub continuous_const
  have hd : HasDerivAt (G a) (a 0 - abar a) 0 := (hca.integral_hasStrictDerivAt 0 0).hasDerivAt
  have hs : Filter.Tendsto (slope (G a) 0) (nhdsWithin 0 (Ioi 0)) (nhds (a 0 - abar a)) :=
    (hasDerivAt_iff_tendsto_slope.mp hd).mono_left
      (nhdsWithin_mono _ (fun x hx => ne_of_gt hx))
  have hq : Filter.Tendsto (fun n : ℝ => (1 - n)⁻¹) (nhdsWithin 0 (Ioi 0)) (nhds 1) := by
    have h : ContinuousAt (fun n : ℝ => (1 - n)⁻¹) 0 :=
      (continuous_const.sub continuous_id).continuousAt.inv₀ (by norm_num)
    have := h.tendsto.mono_left (nhdsWithin_le_nhds (s := Ioi (0 : ℝ)))
    simpa using this
  have hprod := hq.mul hs
  rw [one_mul] at hprod
  apply hprod.congr'
  have hev : ∀ᶠ n in nhdsWithin (0 : ℝ) (Ioi 0), n < 1 :=
    nhdsWithin_le_nhds (Iio_mem_nhds (by norm_num : (0 : ℝ) < 1))
  filter_upwards [hev, self_mem_nhdsWithin] with n hn1 hn0
  rw [eq8 ha hn0 hn1, slope_def_field]
  have hG0 : G a 0 = 0 := by simp [G]
  rw [hG0]
  unfold G
  have h1 : (1 - n) ≠ 0 := by linarith
  have h0 : n ≠ 0 := ne_of_gt hn0
  simp only [sub_zero]
  field_simp

/-- The limit `n → 1` of eq. (8), `σ → ā − a(1)`. -/
theorem sigma_lim_one {a : ℝ → ℝ} (ha : Continuous a) :
    Filter.Tendsto (σ a) (nhdsWithin 1 (Iio 1)) (nhds (abar a - a 1)) := by
  have hca : Continuous fun r => a r - abar a := ha.sub continuous_const
  have hd : HasDerivAt (G a) (a 1 - abar a) 1 := (hca.integral_hasStrictDerivAt 0 1).hasDerivAt
  have hs : Filter.Tendsto (slope (G a) 1) (nhdsWithin 1 (Iio 1)) (nhds (a 1 - abar a)) :=
    (hasDerivAt_iff_tendsto_slope.mp hd).mono_left
      (nhdsWithin_mono _ (fun x hx => ne_of_lt hx))
  have hq : Filter.Tendsto (fun n : ℝ => -n⁻¹) (nhdsWithin 1 (Iio 1)) (nhds (-1)) := by
    have h : ContinuousAt (fun n : ℝ => -n⁻¹) 1 :=
      (continuous_id.continuousAt.inv₀ (by norm_num)).neg
    have := h.tendsto.mono_left (nhdsWithin_le_nhds (s := Iio (1 : ℝ)))
    simpa using this
  have hprod := hq.mul hs
  have e : -1 * (a 1 - abar a) = abar a - a 1 := by ring
  rw [e] at hprod
  apply hprod.congr'
  have hev : ∀ᶠ n in nhdsWithin (1 : ℝ) (Iio 1), 0 < n :=
    nhdsWithin_le_nhds (Ioi_mem_nhds (by norm_num : (0 : ℝ) < 1))
  filter_upwards [hev, self_mem_nhdsWithin] with n hn0 hn1
  rw [eq8 ha hn0 hn1, slope_def_field, G_one ha]
  unfold G
  have h1 : (n - 1) ≠ 0 := by simp at hn1; linarith
  have h1' : (1 - n) ≠ 0 := by simp at hn1; linarith
  have h0 : n ≠ 0 := ne_of_gt hn0
  field_simp
  ring

/-- Conformist and snobbish behaviour (p. 9). -/
def Conformist (s : ℝ → ℝ) : Prop := StrictMonoOn s (Ioo 0 1)
def Snobbish (s : ℝ → ℝ) : Prop := StrictAntiOn s (Ioo 0 1)

/-- Eq. (10) with `γ = 1`. The signaling value `α + β n` is conformist when
`β > 0` and snobbish when `β < 0`. -/
theorem gamma_one_conformist {α β : ℝ} (hβ : 0 < β) : Conformist (fun n => α + β * n) :=
  fun _ _ _ _ h => by simp only; nlinarith

theorem gamma_one_snobbish {α β : ℝ} (hβ : β < 0) : Snobbish (fun n => α + β * n) :=
  fun _ _ _ _ h => by simp only; nlinarith

/-- Eq. (11) with `γ = 1` is the quadratic `ā + α(1 − 2r) + β(2r − 3r²)`. -/
theorem eq11_gamma_one {ab α β r : ℝ} :
    ab + α * (1 - 2 * r) + β * r ^ (1 : ℝ) * (1 + 1 - (2 + 1) * r)
      = ab + α * (1 - 2 * r) + β * (2 * r - 3 * r ^ 2) := by
  rw [Real.rpow_one]; ring

/-- With `γ = 1`, conformism (`β > 0`) goes with a concave rank utility. -/
theorem gamma_one_concave {ab α β : ℝ} (hβ : 0 ≤ β) :
    ConcaveOn ℝ univ (fun r => ab + α * (1 - 2 * r) + β * (2 * r - 3 * r ^ 2)) := by
  refine ⟨convex_univ, fun x _ y _ w v hw hv hwv => ?_⟩
  simp only [smul_eq_mul]
  have hv' : v = 1 - w := by linarith
  subst hv'
  have key : ab + α * (1 - 2 * (w * x + (1 - w) * y)) +
      β * (2 * (w * x + (1 - w) * y) - 3 * (w * x + (1 - w) * y) ^ 2)
      - (w * (ab + α * (1 - 2 * x) + β * (2 * x - 3 * x ^ 2)) +
        (1 - w) * (ab + α * (1 - 2 * y) + β * (2 * y - 3 * y ^ 2)))
      = 3 * β * (w * (1 - w)) * (x - y) ^ 2 := by ring
  have : 0 ≤ 3 * β * (w * (1 - w)) * (x - y) ^ 2 := by positivity
  linarith

/-- With `γ = 1`, snobbism (`β < 0`) goes with a convex rank utility. -/
theorem gamma_one_convex {ab α β : ℝ} (hβ : β ≤ 0) :
    ConvexOn ℝ univ (fun r => ab + α * (1 - 2 * r) + β * (2 * r - 3 * r ^ 2)) := by
  refine ⟨convex_univ, fun x _ y _ w v hw hv hwv => ?_⟩
  simp only [smul_eq_mul]
  have hv' : v = 1 - w := by linarith
  subst hv'
  have key : ab + α * (1 - 2 * (w * x + (1 - w) * y)) +
      β * (2 * (w * x + (1 - w) * y) - 3 * (w * x + (1 - w) * y) ^ 2)
      - (w * (ab + α * (1 - 2 * x) + β * (2 * x - 3 * x ^ 2)) +
        (1 - w) * (ab + α * (1 - 2 * y) + β * (2 * y - 3 * y ^ 2)))
      = 3 * β * (w * (1 - w)) * (x - y) ^ 2 := by ring
  have : 0 ≤ 3 * (-β) * (w * (1 - w)) * (x - y) ^ 2 := by
    have : 0 ≤ -β := by linarith
    positivity
  linarith

/-- Control, footnote 8. Conformism does not imply a concave rank utility in
general. With `α = 0`, `β = 1`, `γ = 2`, `σ(n) = n²` is conformist, while the
rank utility `r²(3 − 4r)` of eq. (11) is not concave on `[0, 1]`. -/
theorem control_fn8_conformist_not_concave :
    Conformist (fun n => n ^ 2) ∧ ¬ ConcaveOn ℝ (Icc 0 1) (fun r : ℝ => r ^ 2 * (3 - 4 * r)) := by
  refine ⟨fun x hx y _ h => by simp only; nlinarith [hx.1], fun hc => ?_⟩
  have := hc.2 (show (0 : ℝ) ∈ Icc 0 1 by norm_num) (show (2 / 5 : ℝ) ∈ Icc 0 1 by norm_num)
    (show (0 : ℝ) ≤ 1 / 2 by norm_num) (show (0 : ℝ) ≤ 1 / 2 by norm_num)
    (by norm_num : (1 / 2 : ℝ) + 1 / 2 = 1)
  norm_num at this

/-- Eq. (11) with `α = 0`, `β = 1`, `γ = 2` is `r²(3 − 4r)`. -/
theorem eq11_gamma_two {r : ℝ} :
    (0 : ℝ) * (1 - 2 * r) + 1 * r ^ (2 : ℝ) * (1 + 2 - (2 + 2) * r) = r ^ 2 * (3 - 4 * r) := by
  rw [Real.rpow_two]; ring

/-- Section 3. With `α = 0`, `β = 1`, the rank utility of eq. (11) has a
stationary point at the rank `γ/(2 + γ)` (p. 12). -/
theorem ideal_rank_deriv {γ r : ℝ} (hr : 0 < r) :
    HasDerivAt (fun t => t ^ γ * (1 + γ - (2 + γ) * t))
      ((1 + γ) * r ^ (γ - 1) * (γ - (2 + γ) * r)) r := by
  have h1 := Real.hasDerivAt_rpow_const (p := γ) (Or.inl (ne_of_gt hr))
  have h2 : HasDerivAt (fun t : ℝ => 1 + γ - (2 + γ) * t) (-(2 + γ)) r := by
    simpa using ((hasDerivAt_id r).const_mul (2 + γ)).const_sub (1 + γ)
  convert h1.mul h2 using 1
  have : r ^ γ = r ^ (γ - 1) * r := by
    rw [Real.rpow_sub_one (ne_of_gt hr)]; field_simp
  rw [this]; ring

theorem ideal_rank {γ : ℝ} (hγ : 0 < γ) :
    (1 + γ) * (γ / (2 + γ)) ^ (γ - 1) * (γ - (2 + γ) * (γ / (2 + γ))) = 0 := by
  have : (2 + γ) ≠ 0 := by linarith
  field_simp
  ring

/-- Eq. (13). `σ = n^γ` is conformist for `γ > 0`. -/
theorem eq13_conformist {γ : ℝ} (hγ : 0 < γ) : Conformist (fun n => n ^ γ) :=
  fun _ hx _ _ h => Real.rpow_lt_rpow hx.1.le h hγ

/-- Proposition 1, first claim, existence. If `0 ≤ σ ≤ u(y_n) − u(0)`, a price
in `[0, y_n]` solves eq. (12). -/
theorem prop1_price_exists {u : ℝ → ℝ} (hu : Continuous u) {y s : ℝ} (hy : 0 ≤ y)
    (h0 : 0 ≤ s) (hs : s ≤ u y - u 0) : ∃ p ∈ Icc 0 y, u y - u (y - p) = s := by
  have hc : ContinuousOn (fun p => u y - u (y - p)) (Icc 0 y) :=
    (continuous_const.sub (hu.comp (continuous_const.sub continuous_id))).continuousOn
  have := intermediate_value_Icc hy hc
  have hm : s ∈ Icc ((fun p => u y - u (y - p)) 0) ((fun p => u y - u (y - p)) y) := by
    simp only [sub_zero, sub_self]; exact ⟨h0, hs⟩
  exact this hm

/-- Proposition 1, first claim, uniqueness. With `u` strictly increasing, eq.
(12) has at most one solution in the price. -/
theorem prop1_price_unique {u : ℝ → ℝ} (hu : StrictMono u) {y p p' : ℝ}
    (h : u y - u (y - p) = u y - u (y - p')) : p = p' := by
  have : u (y - p) = u (y - p') := by linarith
  have := hu.injective this
  linarith

/-- Section 3 in its generality. With `u(c) = ū − e^{−μc}` and
`N(y) = e^{−λy}`, the rank-`n` income is `y_n = −(1/λ) log n`, and the price of
eq. (16) solves eq. (12) for `σ = n^γ`. -/
theorem example_eq16_general {ub μ lam γ n : ℝ} (hμ : μ ≠ 0) (hn : 0 < n) :
    (ub - Real.exp (-μ * (-(1 / lam) * Real.log n))) -
        (ub - Real.exp (-μ * (-(1 / lam) * Real.log n -
          (1 / μ) * Real.log (1 + n ^ (γ - μ / lam))))) = n ^ γ := by
  have hpos : 0 < 1 + n ^ (γ - μ / lam) := by positivity
  have e1 : -μ * (-(1 / lam) * Real.log n - (1 / μ) * Real.log (1 + n ^ (γ - μ / lam)))
      = Real.log n * (μ / lam) + Real.log (1 + n ^ (γ - μ / lam)) := by
    field_simp; ring
  have e2 : -μ * (-(1 / lam) * Real.log n) = Real.log n * (μ / lam) := by ring
  rw [e1, e2, Real.exp_add, Real.exp_log hpos, ← Real.rpow_def_of_pos hn]
  have : n ^ (μ / lam) * n ^ (γ - μ / lam) = n ^ γ := by
    rw [← Real.rpow_add hn]; ring_nf
  linear_combination this

/-- Eq. (17). When the budget binds, `p = y_n = −(1/λ) log n`, and indeed
`N(y_n) = e^{−λ y_n} = n`. -/
theorem eq17 {lam n : ℝ} (hl : lam ≠ 0) (hn : 0 < n) :
    Real.exp (-lam * (-(1 / lam) * Real.log n)) = n := by
  have : -lam * (-(1 / lam) * Real.log n) = Real.log n := by field_simp
  rw [this, Real.exp_log hn]

/-- The region of eq. (16). The price of eq. (16) is affordable,
`p ≤ y_n`, exactly when `n^γ + n^{μ/λ} ≤ 1`. -/
theorem eq16_region {μ lam γ n : ℝ} (hμ : 0 < μ) (hl : 0 < lam) (hn : 0 < n) :
    (1 / μ) * Real.log (1 + n ^ (γ - μ / lam)) ≤ -(1 / lam) * Real.log n ↔
      n ^ γ + n ^ (μ / lam) ≤ 1 := by
  have hpos : 0 < 1 + n ^ (γ - μ / lam) := by positivity
  have hk : 0 < n ^ (μ / lam) := Real.rpow_pos_of_pos hn _
  have e1 : (1 / μ) * Real.log (1 + n ^ (γ - μ / lam)) ≤ -(1 / lam) * Real.log n ↔
      Real.log (1 + n ^ (γ - μ / lam)) ≤ Real.log (n ^ (-(μ / lam))) := by
    rw [Real.log_rpow hn]
    constructor
    · intro h
      have := mul_le_mul_of_nonneg_left h hμ.le
      field_simp at this
      field_simp
      linarith
    · intro h
      have h' : (1 / μ) * Real.log (1 + n ^ (γ - μ / lam)) ≤ (1 / μ) * (-(μ / lam) * Real.log n) :=
        mul_le_mul_of_nonneg_left h (by positivity)
      have : (1 / μ) * (-(μ / lam) * Real.log n) = -(1 / lam) * Real.log n := by
        field_simp
      linarith
  rw [e1, Real.log_le_log_iff hpos (Real.rpow_pos_of_pos hn _)]
  have e3 : n ^ (γ - μ / lam) * n ^ (μ / lam) = n ^ γ := by
    rw [← Real.rpow_add hn]; ring_nf
  rw [Real.rpow_neg hn.le, ← one_div, le_div_iff₀ hk]
  constructor <;> intro h <;> nlinarith [e3]

/-- Section 3. For `γ > μ/λ` the price of eq. (16) rises in `n`. -/
theorem eq16_increasing {μ lam γ : ℝ} (hμ : 0 < μ) (hγ : μ / lam < γ) :
    StrictMonoOn (fun n => (1 / μ) * Real.log (1 + n ^ (γ - μ / lam))) (Ioi 0) := by
  intro n₁ h₁ n₂ h₂ h12
  simp only at *
  have he : 0 < γ - μ / lam := by linarith
  have := Real.rpow_lt_rpow (le_of_lt h₁) h12 he
  have hl := Real.log_lt_log (add_pos_of_pos_of_nonneg one_pos (Real.rpow_nonneg (le_of_lt h₁) _))
    (by linarith : 1 + n₁ ^ (γ - μ / lam) < 1 + n₂ ^ (γ - μ / lam))
  have : 0 < 1 / μ := by positivity
  nlinarith

/-- Eq. (18). The planner's welfare with `n` buyers at price `p ≥ 0` is at most
the welfare with none, since status sums to the constant `ā`. -/
theorem eq18_welfare {u : ℝ → ℝ} (hu : Monotone u) {y : ℝ → ℝ} {p n : ℝ}
    (hcu : Continuous fun r => u (y r)) (hcp : Continuous fun r => u (y r - p)) (hp : 0 ≤ p)
    (hn0 : 0 ≤ n) (_hn1 : n ≤ 1) :
    (∫ r in (0 : ℝ)..n, u (y r - p)) + (∫ r in n..1, u (y r)) ≤ ∫ r in (0 : ℝ)..1, u (y r) := by
  have hm : (∫ r in (0 : ℝ)..n, u (y r - p)) ≤ ∫ r in (0 : ℝ)..n, u (y r) :=
    integral_mono_on hn0 (hcp.intervalIntegrable (μ := MeasureTheory.volume) 0 n)
      (hcu.intervalIntegrable (μ := MeasureTheory.volume) 0 n) (fun r _ => hu (by linarith))
  have hs := integral_add_adjacent_intervals (hcu.intervalIntegrable (μ := MeasureTheory.volume) 0 n)
    (hcu.intervalIntegrable (μ := MeasureTheory.volume) n 1)
  linarith

/-- Proposition 2. If the price function is strictly decreasing on `]0, 1[`
with limit `p̂` at 1, no semi-separating equilibrium exists at a price below
`p̂`. With passive conjectures, pooling at 1 is excluded (`prop4_passive`) and
pooling at 0 holds (`prop3_passive`), so `n = 0` is the only equilibrium. -/
theorem prop2 {p : ℝ → ℝ} {phat q : ℝ} (hanti : StrictAntiOn p (Ioo 0 1))
    (hlim : Filter.Tendsto p (nhdsWithin 1 (Iio 1)) (nhds phat)) (hq : q < phat) :
    ∀ n ∈ Ioo (0 : ℝ) 1, p n ≠ q := by
  intro n hn
  set m := (n + 1) / 2
  have hm : m ∈ Ioo (0 : ℝ) 1 := ⟨by simp only [m]; linarith [hn.1], by simp only [m]; linarith [hn.2]⟩
  have hnm : n < m := by simp only [m]; linarith [hn.2]
  have hev : ∀ᶠ t in nhdsWithin (1 : ℝ) (Iio 1), p t ≤ p m := by
    filter_upwards [Ioo_mem_nhdsLT hm.2] with t ht
    exact (hanti hm ⟨lt_trans hm.1 ht.1, ht.2⟩ ht.1).le
  have hle : phat ≤ p m := le_of_tendsto hlim hev
  have := hanti hn hm hnm
  intro h
  linarith

/-- Proposition 3. Pooling at `δ = 0` holds for every consumer when it holds
for the richest, under concave `u`. -/
theorem prop3_pool0 {u : ℝ → ℝ} (hc : ConcaveOn ℝ univ u) {E1 ab p y0 : ℝ} {y : ℝ → ℝ}
    (hp : 0 ≤ p) (hy0 : ∀ r ∈ Icc (0 : ℝ) 1, y r ≤ y0) (h : E1 < ab + u y0 - u (y0 - p)) :
    ∀ r ∈ Icc (0 : ℝ) 1, E1 < ab + u (y r) - u (y r - p) := by
  intro r hr
  have := concave_increment hc hp (hy0 r hr)
  linarith

/-- Proposition 3 under passive conjectures, `E(a(r)/1) = ā`. -/
theorem prop3_passive {u : ℝ → ℝ} (hu : StrictMono u) {ab p y : ℝ} (hp : 0 < p) :
    ab < ab + u y - u (y - p) := by
  have := hu (by linarith : y - p < y)
  linarith

/-- Proposition 4. Pooling at `δ = 1` holds for every consumer when it holds
for the poorest, under concave `u`. -/
theorem prop4_pool1 {u : ℝ → ℝ} (hc : ConcaveOn ℝ univ u) {E0 ab p y1 : ℝ} {y : ℝ → ℝ}
    (hp : 0 ≤ p) (hy1 : ∀ r ∈ Icc (0 : ℝ) 1, y1 ≤ y r) (h : E0 < ab - u y1 + u (y1 - p)) :
    ∀ r ∈ Icc (0 : ℝ) 1, E0 < ab - u (y r) + u (y r - p) := by
  intro r hr
  have := concave_increment hc hp (hy1 r hr)
  linarith

/-- Eq. (24). With `a` decreasing, out-of-equilibrium beliefs `E(a(r)/0)` range
over `[a(1), a(0)]`, and one supporting pooling at 1 exists exactly when
`ā − a(1) > u(y_1) − u(y_1 − p)`. -/
theorem eq24 {a0 a1 ab D : ℝ} (h10 : a1 ≤ a0) :
    (∃ b ∈ Icc a1 a0, b < ab - D) ↔ ab - a1 > D := by
  constructor
  · rintro ⟨b, hb, hlt⟩; linarith [hb.1]
  · intro h; exact ⟨a1, ⟨le_rfl, h10⟩, by linarith⟩

/-- Proposition 4 under passive conjectures, `E(a(r)/0) = ā`. No pooling at
`δ = 1` exists when the poorest consumer bears a positive cost. -/
theorem prop4_passive {u : ℝ → ℝ} (hu : StrictMono u) {ab p y1 : ℝ} (hp : 0 < p) :
    ¬ ab < ab - u y1 + u (y1 - p) := by
  have := hu (by linarith : y1 - p < y1)
  intro h; linarith

/-- Control: the upper set of buyers (eqs. 5 to 7) needs concave `u`. With
`u = exp`, price 1 and signaling value 1, a consumer with income 0 buys and a
richer one with income 1 does not. -/
theorem control_convex_buyers :
    Real.exp 0 - Real.exp (0 - 1) < 1 ∧ ¬ (Real.exp 1 - Real.exp (1 - 1) < 1) := by
  refine ⟨?_, ?_⟩
  · have := Real.exp_pos (-1); norm_num; linarith
  · have := Real.add_one_le_exp 1; norm_num; linarith

/-- Control: Proposition 3's reduction to the richest needs concave `u`. With
`u = exp`, `ā = 0`, `E(a(r)/1) = 1` and price 1, condition (21) holds for the
consumer with income 1 and fails for the one with income 0. -/
theorem control_prop3_concavity :
    (1 : ℝ) < 0 + Real.exp 1 - Real.exp (1 - 1) ∧ ¬ ((1 : ℝ) < 0 + Real.exp 0 - Real.exp (0 - 1)) := by
  refine ⟨?_, ?_⟩
  · have := Real.add_one_le_exp 1
    have h2 : Real.exp 1 ≠ 2 := by
      intro h; have := Real.exp_one_gt_d9; linarith
    simp; rcases lt_or_eq_of_le this with h | h
    · linarith
    · exact absurd (by linarith) h2
  · have := Real.exp_pos (-1); norm_num; linarith

end CorneoJeanne1994
