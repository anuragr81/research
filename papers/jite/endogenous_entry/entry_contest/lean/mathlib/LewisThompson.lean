import Mathlib

/-!
# Lewis and Thompson (1981), "Dispersive distributions, and the connection between dispersivity
and strong unimodality"

Our reading of the paper, checked. Locators are printed pages of the *Journal of Applied
Probability* article. Distribution functions `G`, `H` have quantile functions `y`, `z` on
`(0, 1)`, so `G (y α) = α` and `H (z α) = α`.
-/

open Real Set Filter Topology

namespace LewisThompson

/-! ## The order, (1.2) and (1.6), pp.77-78, in three equivalent forms -/

/-- The CDF form (1.6): `G(y_α + c) ≥ H(z_α + c)` for every `α ∈ (0, 1)` and `c > 0`. -/
def OrdCdf (G H y z : ℝ → ℝ) : Prop :=
  ∀ α ∈ Ioo (0 : ℝ) 1, ∀ c : ℝ, 0 < c → H (z α + c) ≤ G (y α + c)

/-- The spacing form: any two quantiles of `H` are at least as far apart as those of `G`. -/
def OrdSpacing (y z : ℝ → ℝ) : Prop :=
  ∀ α ∈ Ioo (0 : ℝ) 1, ∀ β ∈ Ioo (0 : ℝ) 1, α ≤ β → y β - y α ≤ z β - z α

/-- The quantile-difference form, Hopkins and Kornienko's Definition 1: `z − y` does not fall. -/
def OrdDiff (y z : ℝ → ℝ) : Prop := MonotoneOn (fun α => z α - y α) (Ioo 0 1)

theorem spacing_iff_diff (y z : ℝ → ℝ) : OrdSpacing y z ↔ OrdDiff y z := by
  constructor
  · intro h a ha b hb hab
    have := h a ha b hb hab
    simp only
    linarith
  · intro h a ha b hb hab
    have := h ha hb hab
    simp only at this
    linarith

/-- **The CDF form and the spacing form agree** for continuous, strictly increasing distribution
    functions, the case of (1.2). -/
theorem cdf_iff_spacing (G H y z : ℝ → ℝ) (hG : StrictMono G) (hH : StrictMono H)
    (hGr : ∀ x, G x ∈ Ioo (0 : ℝ) 1)
    (hy : ∀ α ∈ Ioo (0 : ℝ) 1, G (y α) = α) (hz : ∀ α ∈ Ioo (0 : ℝ) 1, H (z α) = α)
    (hyG : ∀ x, y (G x) = x) :
    OrdCdf G H y z ↔ OrdSpacing y z := by
  constructor
  · intro h a ha b hb hab
    rcases hab.lt_or_eq with hlt | heq
    · have hya : y a < y b := by
        by_contra hle
        push_neg at hle
        have := hG.monotone hle
        rw [hy a ha, hy b hb] at this
        linarith
      have h1 := h a ha (y b - y a) (by linarith)
      rw [show y a + (y b - y a) = y b by ring, hy b hb] at h1
      have h2 : H (z a + (y b - y a)) ≤ H (z b) := h1.trans_eq (hz b hb).symm
      have := hH.le_iff_le.mp h2
      linarith
    · rw [heq]
      simp
  · intro h a ha c hc
    have hb : G (y a + c) ∈ Ioo (0 : ℝ) 1 := hGr _
    have hyb : y (G (y a + c)) = y a + c := hyG _
    have hab : a ≤ G (y a + c) := by
      have : G (y a) ≤ G (y a + c) := hG.monotone (by linarith)
      rwa [hy a ha] at this
    have h1 := h a ha _ hb hab
    rw [hyb] at h1
    calc H (z a + c) ≤ H (z (G (y a + c))) := hH.monotone (by linarith)
      _ = G (y a + c) := hz _ hb

/-- **The transport map.** With `T = z ∘ G`, which sends each quantile of `G` to the matching
    quantile of `H`, the displacement `T x − x` does not fall exactly when the order holds. -/
theorem transport_disp (G y z : ℝ → ℝ) (hG : StrictMono G) (hGr : ∀ x, G x ∈ Ioo (0 : ℝ) 1)
    (hy : ∀ α ∈ Ioo (0 : ℝ) 1, G (y α) = α) (hyG : ∀ x, y (G x) = x) :
    OrdDiff y z ↔ Monotone (fun x => z (G x) - x) := by
  constructor
  · intro h a b hab
    have := h (hGr a) (hGr b) (hG.monotone hab)
    simp only [hyG] at this
    exact this
  · intro h α hα β hβ hab
    have := h (show y α ≤ y β by
      by_contra hlt
      push_neg at hlt
      have := hG.monotone hlt.le
      rw [hy α hα, hy β hβ] at this
      have hab' : α = β := le_antisymm hab this
      rw [hab'] at hlt
      exact lt_irrefl _ hlt)
    simp only [hy α hα, hy β hβ] at this
    exact this

/-- With a crossing point the transport map is a pivot-spread about it. -/
theorem transport_pivot (T : ℝ → ℝ) (hT : Monotone (fun x => T x - x)) (x0 : ℝ) (h0 : T x0 = x0) :
    (∀ x, x0 ≤ x → x ≤ T x) ∧ (∀ x, x ≤ x0 → T x ≤ x) := by
  refine ⟨fun x hx => ?_, fun x hx => ?_⟩
  · have := hT hx
    simp only [h0, sub_self] at this
    linarith
  · have := hT hx
    simp only [h0, sub_self] at this
    linarith

/-! ## Elementary properties, p.77 -/

/-- Invariant under a shift of location and a common change of scale `k > 0`. -/
theorem affine_invariant (y z : ℝ → ℝ) (a b k : ℝ) (hk : 0 < k) (h : OrdSpacing y z) :
    OrdSpacing (fun α => a + k * y α) (fun α => b + k * z α) := by
  intro α hα β hβ hab
  have := h α hα β hβ hab
  simp only
  nlinarith

/-- For `k ≥ 1` and a non-decreasing quantile function, `X` and `kX` are ordered. -/
theorem scale_pair (y : ℝ → ℝ) (hy : MonotoneOn y (Ioo 0 1)) (k : ℝ) (hk : 1 ≤ k) :
    OrdSpacing y (fun α => k * y α) := by
  intro α hα β hβ hab
  have := hy hα hβ hab
  simp only
  nlinarith

/-- A quantile function whose distribution function is strictly increasing on `ℝ`, with an
    asymmetric part `p²`. -/
noncomputable def qAsym (p : ℝ) : ℝ := log (p / (1 - p)) + p ^ 2

/-- The quantile function of `−X`, `p ↦ −q(1 − p)`. -/
noncomputable def qNeg (p : ℝ) : ℝ := -qAsym (1 - p)

theorem qAsym_strictMonoOn : StrictMonoOn qAsym (Ioo 0 1) := by
  intro a ha b hb hab
  unfold qAsym
  have h1 : a / (1 - a) < b / (1 - b) := by
    rw [div_lt_div_iff₀ (by linarith [ha.2]) (by linarith [hb.2])]
    nlinarith
  have h2 : log (a / (1 - a)) < log (b / (1 - b)) :=
    Real.log_lt_log (div_pos ha.1 (by linarith [ha.2])) h1
  have h3 : a ^ 2 < b ^ 2 := by nlinarith [ha.1]
  linarith

/-- The logistic function, used to show `qAsym` maps `(0, 1)` onto `ℝ`. -/
noncomputable def sig (t : ℝ) : ℝ := 1 / (1 + exp (-t))

theorem sig_mem (t : ℝ) : sig t ∈ Ioo (0 : ℝ) 1 := by
  unfold sig
  have := exp_pos (-t)
  refine ⟨by positivity, ?_⟩
  rw [div_lt_one (by linarith)]
  linarith

theorem qAsym_sig (t : ℝ) : qAsym (sig t) = t + sig t ^ 2 := by
  unfold qAsym
  congr 1
  have he := exp_pos (-t)
  have : sig t / (1 - sig t) = exp t := by
    unfold sig
    have h1 : (1 : ℝ) + exp (-t) ≠ 0 := by linarith
    rw [show 1 - 1 / (1 + exp (-t)) = exp (-t) / (1 + exp (-t)) by field_simp; ring]
    rw [div_div_div_cancel_right₀ h1, Real.exp_neg]
    field_simp
  rw [this, Real.log_exp]

theorem qAsym_continuousOn : ContinuousOn qAsym (Ioo 0 1) := by
  unfold qAsym
  refine ContinuousOn.add ?_ (continuousOn_pow 2)
  refine ContinuousOn.log ?_ ?_
  · exact continuousOn_id.div (continuousOn_const.sub continuousOn_id)
      (fun x hx => by have := hx.2; simp only [ne_eq]; linarith)
  · intro x hx
    exact (div_pos hx.1 (by linarith [hx.2])).ne'

/-- `qAsym` maps `(0, 1)` onto `ℝ`, so it is the quantile function of a distribution function that
    is strictly increasing on `ℝ`. -/
theorem qAsym_onto (x : ℝ) : ∃ p ∈ Ioo (0 : ℝ) 1, qAsym p = x := by
  set a := sig (x - 2)
  set b := sig x
  have ha := sig_mem (x - 2)
  have hb := sig_mem x
  have hab : a ≤ b := by
    simp only [a, b, sig]
    apply one_div_le_one_div_of_le (by positivity)
    have : exp (-x) ≤ exp (-(x - 2)) := Real.exp_le_exp.mpr (by linarith)
    linarith
  have hsub : Icc a b ⊆ Ioo (0 : ℝ) 1 := fun p hp => ⟨lt_of_lt_of_le ha.1 hp.1, lt_of_le_of_lt hp.2 hb.2⟩
  have hqa : qAsym a ≤ x := by
    rw [qAsym_sig]
    have : a ^ 2 < 1 := by nlinarith [ha.1, ha.2]
    linarith
  have hqb : x ≤ qAsym b := by
    rw [qAsym_sig]
    nlinarith [sq_nonneg b]
  obtain ⟨p, hp, hpx⟩ := intermediate_value_Icc hab (qAsym_continuousOn.mono hsub) ⟨hqa, hqb⟩
  exact ⟨p, hsub hp, hpx⟩

/-- The quantile of `−X` at `p` is `−q(1 − p)`: with `F (q (1 − p)) = 1 − p` and no atoms, the
    distribution function of `−X`, `u ↦ 1 − F(−u)`, equals `p` there. -/
theorem neg_quantile (F : ℝ → ℝ) (p : ℝ) (h : F (qAsym (1 - p)) = 1 - p) :
    1 - F (-(qNeg p)) = p := by
  unfold qNeg
  rw [neg_neg, h]
  ring

theorem spacing_gap (a b : ℝ) (ha : a ∈ Ioo (0 : ℝ) 1) (hb : b ∈ Ioo (0 : ℝ) 1) :
    (qNeg b - qNeg a) - (qAsym b - qAsym a) = 2 * (b - a) * (1 - a - b) := by
  unfold qNeg qAsym
  have l : ∀ p ∈ Ioo (0 : ℝ) 1, log ((1 - p) / (1 - (1 - p))) = -log (p / (1 - p)) := by
    intro p hp
    have h1 : (1 : ℝ) - p ≠ 0 := by linarith [hp.2]
    have h2 : p ≠ 0 := hp.1.ne'
    rw [show (1 : ℝ) - (1 - p) = p by ring, Real.log_div h1 h2, Real.log_div h2 h1]
    ring
  rw [l a ha, l b hb]
  ring

/-- **"X and kX form an o.d. pair for k ≠ 1" fails at `k = −1`.** For a distribution function
    strictly increasing on `ℝ`, the spacings of `X` and `−X` compare one way on `(1/10, 2/10)` and
    the other way on `(8/10, 9/10)`, so neither is more dispersed. -/
theorem neg_not_ordered : ¬ OrdSpacing qAsym qNeg ∧ ¬ OrdSpacing qNeg qAsym := by
  have m1 : (1 / 10 : ℝ) ∈ Ioo (0 : ℝ) 1 := ⟨by norm_num, by norm_num⟩
  have m2 : (2 / 10 : ℝ) ∈ Ioo (0 : ℝ) 1 := ⟨by norm_num, by norm_num⟩
  have m8 : (8 / 10 : ℝ) ∈ Ioo (0 : ℝ) 1 := ⟨by norm_num, by norm_num⟩
  have m9 : (9 / 10 : ℝ) ∈ Ioo (0 : ℝ) 1 := ⟨by norm_num, by norm_num⟩
  have g1 := spacing_gap (1 / 10) (2 / 10) m1 m2
  have g2 := spacing_gap (8 / 10) (9 / 10) m8 m9
  refine ⟨fun h => ?_, fun h => ?_⟩
  · have := h (8 / 10) m8 (9 / 10) m9 (by norm_num)
    nlinarith
  · have := h (1 / 10) m1 (2 / 10) m2 (by norm_num)
    nlinarith

/-! ## Densities, Theorems 1 and 2, pp.80-81, in quantile form

With `y' α = 1/g(y_α)` and `z' α = 1/h(z_α)`, `g(y_α) ≥ h(z_α)` reads `y' ≤ z'`. -/

/-- **Theorem 2.** If the quantile derivatives satisfy `y' ≤ z'` on `(0, 1)`, the order holds. -/
theorem thm2 (y z y' z' : ℝ → ℝ) (hy : ∀ α ∈ Ioo (0 : ℝ) 1, HasDerivAt y (y' α) α)
    (hz : ∀ α ∈ Ioo (0 : ℝ) 1, HasDerivAt z (z' α) α) (h : ∀ α ∈ Ioo (0 : ℝ) 1, y' α ≤ z' α) :
    OrdDiff y z := by
  have hd : ∀ α ∈ Ioo (0 : ℝ) 1, HasDerivAt (fun α => z α - y α) (z' α - y' α) α :=
    fun α hα => (hz α hα).sub (hy α hα)
  refine monotoneOn_of_hasDerivWithinAt_nonneg (f' := fun α => z' α - y' α) (convex_Ioo 0 1)
    (fun α hα => (hd α hα).continuousAt.continuousWithinAt) (fun α hα => ?_) (fun α hα => ?_)
  · rw [interior_Ioo] at hα ⊢
    exact (hd α hα).hasDerivWithinAt
  · rw [interior_Ioo] at hα
    linarith [h α hα]

/-- Theorem 2 with densities: `g(y_α) ≥ h(z_α) > 0` gives the order. -/
theorem thm2_density (y z g h : ℝ → ℝ)
    (hy : ∀ α ∈ Ioo (0 : ℝ) 1, HasDerivAt y (1 / g (y α)) α)
    (hz : ∀ α ∈ Ioo (0 : ℝ) 1, HasDerivAt z (1 / h (z α)) α)
    (hpos : ∀ α ∈ Ioo (0 : ℝ) 1, 0 < h (z α)) (hgh : ∀ α ∈ Ioo (0 : ℝ) 1, h (z α) ≤ g (y α)) :
    OrdDiff y z :=
  thm2 y z _ _ hy hz (fun α hα => one_div_le_one_div_of_le (hpos α hα) (hgh α hα))

/-- **Theorem 1.** Where both quantile functions are differentiable, the order gives `y' ≤ z'`. -/
theorem thm1 (y z : ℝ → ℝ) (h : OrdDiff y z) (α y' z' : ℝ) (hα : α ∈ Ioo (0 : ℝ) 1)
    (hy : HasDerivAt y y' α) (hz : HasDerivAt z z' α) : y' ≤ z' := by
  have hd : HasDerivAt (fun a => z a - y a) (z' - y') α := hz.sub hy
  have ht := hd.tendsto_slope_zero_right
  have hev : ∀ᶠ t in 𝓝[>] (0 : ℝ), 0 ≤ t⁻¹ • ((z (α + t) - y (α + t)) - (z α - y α)) := by
    have hmem : ∀ᶠ t in 𝓝[>] (0 : ℝ), α + t ∈ Ioo (0 : ℝ) 1 ∧ 0 < t := by
      have h1 : ∀ᶠ t in 𝓝 (0 : ℝ), α + t < 1 := by
        have : Tendsto (fun t => α + t) (𝓝 0) (𝓝 (α + 0)) := tendsto_const_nhds.add tendsto_id
        rw [add_zero] at this
        exact this.eventually (eventually_lt_nhds hα.2)
      filter_upwards [nhdsWithin_le_nhds h1, self_mem_nhdsWithin] with t ht1 ht2
      exact ⟨⟨by linarith [hα.1, show (0 : ℝ) < t from ht2], ht1⟩, ht2⟩
    filter_upwards [hmem] with t ⟨htm, htpos⟩
    have := h hα htm (by linarith)
    simp only at this
    rw [smul_eq_mul]
    exact mul_nonneg (inv_nonneg.mpr htpos.le) (by linarith)
  have := ge_of_tendsto ht hev
  linarith

/-! ## Examples, Section 6, pp.89-90 -/

/-- **Normals by variance** (6.3 (i)), and any location-scale family: a larger scale is more
    dispersed. -/
theorem scale_family (q : ℝ → ℝ) (hq : MonotoneOn q (Ioo 0 1)) (m1 m2 s1 s2 : ℝ) (hs : s1 ≤ s2) :
    OrdSpacing (fun α => m1 + s1 * q α) (fun α => m2 + s2 * q α) := by
  intro α hα β hβ hab
  have := hq hα hβ hab
  simp only
  nlinarith

/-- **Paretos by index** (6.3 (ii)). With `G(y) = 1 − y^{-r}` and `H(z) = 1 − z^{-s}`, `0 < s < r`,
    the quantiles `(1−α)^{-1/r}` and `(1−α)^{-1/s}` are ordered, the smaller index more dispersed. -/
theorem pareto (r s : ℝ) (hs : 0 < s) (hsr : s < r) :
    OrdDiff (fun α => (1 - α) ^ (-1 / r)) (fun α => (1 - α) ^ (-1 / s)) := by
  have hr : 0 < r := by linarith
  have deriv : ∀ k : ℝ, 0 < k → ∀ α ∈ Ioo (0 : ℝ) 1,
      HasDerivAt (fun α => (1 - α) ^ (-1 / k)) (1 / k * (1 - α) ^ (-1 / k - 1)) α := by
    intro k hk α hα
    have h1 : HasDerivAt (fun α : ℝ => 1 - α) (-1) α := by
      simpa using (hasDerivAt_id α).const_sub 1
    have h2 := h1.rpow_const (p := -1 / k) (Or.inl (by linarith [hα.2]))
    convert h2 using 1
    ring
  refine thm2 _ _ _ _ (deriv r hr) (deriv s hs) (fun α hα => ?_)
  have hu0 : 0 < 1 - α := by linarith [hα.2]
  have hu1 : 1 - α ≤ 1 := by linarith [hα.1]
  have he : -1 / s - 1 ≤ -1 / r - 1 := by
    have : 1 / r < 1 / s := one_div_lt_one_div_of_lt hs hsr
    have e1 : -1 / s = -(1 / s) := by ring
    have e2 : -1 / r = -(1 / r) := by ring
    rw [e1, e2]
    linarith
  have hp := Real.rpow_le_rpow_of_exponent_ge hu0 hu1 he
  have hk : 1 / r ≤ 1 / s := (one_div_lt_one_div_of_lt hs hsr).le
  have hpos : 0 ≤ (1 - α) ^ (-1 / r - 1) := Real.rpow_nonneg hu0.le _
  calc 1 / r * (1 - α) ^ (-1 / r - 1) ≤ 1 / s * (1 - α) ^ (-1 / r - 1) :=
        mul_le_mul_of_nonneg_right hk hpos
    _ ≤ 1 / s * (1 - α) ^ (-1 / s - 1) :=
        mul_le_mul_of_nonneg_left hp (by positivity)

/-- **Lognormals** (6.3 (iii)). With `σ1 ≠ σ2` the density ratio at matching quantiles,
    `(σ2/σ1) exp((μ2 − μ1) + (σ2 − σ1) θ)`, exceeds `1` for some `θ` and falls below `1` for
    another, so by Theorem 1 neither law is more dispersed. -/
theorem lognormal_ratio (μ1 μ2 σ1 σ2 : ℝ) (h1 : 0 < σ1) (h2 : 0 < σ2) (hne : σ1 ≠ σ2) :
    (∃ θ : ℝ, 1 < σ2 / σ1 * exp ((μ2 - μ1) + (σ2 - σ1) * θ))
      ∧ ∃ θ : ℝ, σ2 / σ1 * exp ((μ2 - μ1) + (σ2 - σ1) * θ) < 1 := by
  have hd : σ2 - σ1 ≠ 0 := sub_ne_zero.mpr (Ne.symm hne)
  have hk : 0 < σ2 / σ1 := div_pos h2 h1
  -- the exponent can be set to any value `e`
  have hset : ∀ e : ℝ, σ2 / σ1 * exp ((μ2 - μ1) + (σ2 - σ1) * ((e - (μ2 - μ1)) / (σ2 - σ1)))
      = σ2 / σ1 * exp e := by
    intro e
    congr 2
    field_simp
    ring
  refine ⟨⟨(log (σ1 / σ2) + 1 - (μ2 - μ1)) / (σ2 - σ1), ?_⟩,
    ⟨(log (σ1 / σ2) - 1 - (μ2 - μ1)) / (σ2 - σ1), ?_⟩⟩
  · rw [hset, Real.exp_add, Real.exp_log (div_pos h1 h2)]
    have : σ2 / σ1 * (σ1 / σ2) = 1 := by field_simp
    have he : 1 < exp 1 := by
      have := Real.add_one_le_exp (1 : ℝ)
      linarith
    nlinarith
  · rw [hset, Real.exp_sub, Real.exp_log (div_pos h1 h2)]
    have : σ2 / σ1 * (σ1 / σ2) = 1 := by field_simp
    have he : 1 < exp 1 := by
      have := Real.add_one_le_exp (1 : ℝ)
      linarith
    rw [mul_div_assoc', this, div_lt_one (exp_pos 1)]
    exact he

/-- **No mixture of two exponentials is dispersive** (6.2). For `f = a λ e^{-λx} + b μ e^{-μx}`
    with `a, b > 0` and `λ ≠ μ`, `f f'' − f'² > 0`, so `ln f` is strictly convex and fails the
    condition of Theorem 7. -/
theorem mixture_logconvex (a b lam mu x : ℝ) (ha : 0 < a) (hb : 0 < b) (hlam : 0 < lam)
    (hmu : 0 < mu) (hne : lam ≠ mu) :
    let A := a * lam * exp (-lam * x)
    let B := b * mu * exp (-mu * x)
    0 < (A + B) * (lam ^ 2 * A + mu ^ 2 * B) - (-lam * A - mu * B) ^ 2 := by
  intro A B
  have hA : 0 < A := by positivity
  have hB : 0 < B := by positivity
  have e : (A + B) * (lam ^ 2 * A + mu ^ 2 * B) - (-lam * A - mu * B) ^ 2
      = A * B * (lam - mu) ^ 2 := by ring
  rw [e]
  have : 0 < (lam - mu) ^ 2 := by
    have : lam - mu ≠ 0 := sub_ne_zero.mpr hne
    positivity
  positivity

end LewisThompson
