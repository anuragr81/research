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

end CorneoJeanne1994
