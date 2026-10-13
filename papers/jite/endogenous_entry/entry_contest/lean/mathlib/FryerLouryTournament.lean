import Mathlib

/-!
# Fryer and Loury, "Categorical Redistribution in Winner-Take-All Markets"

NBER Working Paper 10104 (November 2003), the working paper of "Affirmative Action in
Winner-Take-All Markets", Journal of Economic Inequality 3(3), 263-280 (2005), which is
not checked.  Page numbers are the working paper's printed pages.

Setup (pp.4-5): a continuum of workers with private constant marginal costs `c` drawn from
`F` on `[c_min, c_max]` choose an effort, are paired at random, and the employer hires the
higher effort at wage `ω`.  Two categories have cost laws `F_1`, `F_2` with population
shares `π_1`, `π_2`.

## What is proved

1. Proposition 1, incentive compatibility: with `e*' = -ω f/c`, a worker of cost `c` who
   mimics cost `c'` earns a payoff whose derivative is `ω f(c')(c/c' - 1)`, so mimicking
   no other cost pays (`mimic_hasDerivAt`, `prop1_best_reply`).
2. Footnote 2: `∫ f_1 (1 - F_1) = 1/2` (`half_integral`), so the natural win rate is
   `γ_0 = 1/2 + ∫ f_1 (F_1 - F_2)` (`gamma0_eq`); and the shortfall of category 2's share of
   wins on p.8 holds exactly when `γ_0 > 1/2` (`share_shortfall_iff`).
3. Proposition 3: the indifference conditions give `ê` and `λ` (`ehat_iff`, `lambda_iff`),
   the representation constraint (10) has derivative `(f_1(1-F_2) - f_2(1-F_1))/2` in the
   cut-off (`Gamma_hasDerivAt`), positive under the hazard-rate order (`Gamma_deriv_pos`),
   so a more aggressive target (a lower `γ`) gives a lower cut-off and a larger pool of
   workers exerting zero effort (`pool_grows`); at parity the whole population pools
   (`Gamma_at_min`).

## Not formalised

That the likelihood-ratio order of Definition 1 gives the distribution and hazard-rate
orders (used as hypotheses), the existence and uniqueness of the D1 equilibrium, Proposition
2, and the centralized problem of Section 4.
-/

open MeasureTheory intervalIntegral Set

namespace FryerLoury2003

/-! ## 1. Proposition 1: incentive compatibility (pp.5-6) -/

/-- A worker of cost `c` who mimics cost `y` wins against every higher cost and pays
`c e(y)`. -/
noncomputable def mimic (ω c : ℝ) (F e : ℝ → ℝ) (y : ℝ) : ℝ := ω * (1 - F y) - c * e y

theorem mimic_hasDerivAt (ω c : ℝ) (F e f : ℝ → ℝ) (y : ℝ) (hy : y ≠ 0)
    (hF : HasDerivAt F (f y) y) (he : HasDerivAt e (-(ω * f y / y)) y) :
    HasDerivAt (mimic ω c F e) (ω * f y * (c / y - 1)) y := by
  have h := ((hF.const_sub 1).const_mul ω).sub (he.const_mul c)
  unfold mimic
  refine h.congr_deriv ?_
  field_simp
  ring

/-- Proposition 1: on a cost support `[lo, hi]` with `lo > 0`, mimicking any other cost
does not pay. -/
theorem prop1_best_reply (ω lo hi c : ℝ) (F e f : ℝ → ℝ) (hω : 0 ≤ ω) (hlo : 0 < lo)
    (hF : ∀ y ∈ Icc lo hi, HasDerivAt F (f y) y)
    (he : ∀ y ∈ Icc lo hi, HasDerivAt e (-(ω * f y / y)) y)
    (hf : ∀ y ∈ Icc lo hi, 0 ≤ f y) (hc : c ∈ Icc lo hi) :
    ∀ y ∈ Icc lo hi, mimic ω c F e y ≤ mimic ω c F e c := by
  have hd : ∀ y ∈ Icc lo hi, HasDerivAt (mimic ω c F e) (ω * f y * (c / y - 1)) y :=
    fun y hy => mimic_hasDerivAt ω c F e f y (by linarith [hy.1]) (hF y hy) (he y hy)
  have hcont : ContinuousOn (mimic ω c F e) (Icc lo hi) :=
    fun y hy => (hd y hy).continuousAt.continuousWithinAt
  intro y hy
  rcases le_total y c with hyc | hyc
  · -- increasing on [lo, c]
    have hmono : MonotoneOn (mimic ω c F e) (Icc lo c) := by
      apply monotoneOn_of_deriv_nonneg (convex_Icc lo c)
      · exact hcont.mono (Icc_subset_Icc le_rfl hc.2)
      · intro x hx
        rw [interior_Icc] at hx
        exact (hd x ⟨hx.1.le, by linarith [hx.2, hc.2]⟩).differentiableAt.differentiableWithinAt
      · intro x hx
        rw [interior_Icc] at hx
        have hxI : x ∈ Icc lo hi := ⟨hx.1.le, by linarith [hx.2, hc.2]⟩
        rw [(hd x hxI).deriv]
        have hx0 : 0 < x := by linarith [hx.1]
        have : 0 ≤ c / x - 1 := by
          rw [sub_nonneg, le_div_iff₀ hx0]; linarith [hx.2]
        exact mul_nonneg (mul_nonneg hω (hf x hxI)) this
    exact hmono ⟨hy.1, hyc⟩ ⟨hc.1, le_rfl⟩ hyc
  · -- decreasing on [c, hi]
    have hanti : AntitoneOn (mimic ω c F e) (Icc c hi) := by
      apply antitoneOn_of_deriv_nonpos (convex_Icc c hi)
      · exact hcont.mono (Icc_subset_Icc hc.1 le_rfl)
      · intro x hx
        rw [interior_Icc] at hx
        exact (hd x ⟨by linarith [hx.1, hc.1], hx.2.le⟩).differentiableAt.differentiableWithinAt
      · intro x hx
        rw [interior_Icc] at hx
        have hxI : x ∈ Icc lo hi := ⟨by linarith [hx.1, hc.1], hx.2.le⟩
        rw [(hd x hxI).deriv]
        have hx0 : 0 < x := by linarith [hx.1, hc.1]
        have : c / x - 1 ≤ 0 := by
          rw [sub_nonpos, div_le_one hx0]; linarith [hx.1]
        exact mul_nonpos_of_nonneg_of_nonpos (mul_nonneg hω (hf x hxI)) this
    exact hanti ⟨le_rfl, hc.2⟩ ⟨hyc, hy.2⟩ hyc

/-! ## 2. The natural win rate (footnote 2, p.8) -/

/-- Eq. (6): `∫ f_1 (1 - F_1) = 1/2` over the support. -/
theorem half_integral (F1 f1 : ℝ → ℝ) (a b : ℝ)
    (hF : ∀ x ∈ uIcc a b, HasDerivAt F1 (f1 x) x)
    (hint : IntervalIntegrable (fun x => f1 x * (1 - F1 x)) volume a b)
    (ha : F1 a = 0) (hb : F1 b = 1) :
    ∫ x in a..b, f1 x * (1 - F1 x) = 1 / 2 := by
  have hd : ∀ x ∈ uIcc a b,
      HasDerivAt (fun y => -(1 - F1 y) ^ 2 / 2) (f1 x * (1 - F1 x)) x := by
    intro x hx
    have := (((hF x hx).const_sub 1).pow 2).neg.div_const 2
    refine this.congr_deriv ?_
    simp
    ring
  rw [integral_eq_sub_of_hasDerivAt hd hint, ha, hb]
  norm_num

/-- Eq. (5): `γ_0 = ∫ f_1 (1 - F_2) = 1/2 + ∫ f_1 (F_1 - F_2)`. -/
theorem gamma0_eq (F1 F2 f1 : ℝ → ℝ) (a b : ℝ)
    (hF : ∀ x ∈ uIcc a b, HasDerivAt F1 (f1 x) x)
    (h1 : IntervalIntegrable (fun x => f1 x * (1 - F1 x)) volume a b)
    (h2 : IntervalIntegrable (fun x => f1 x * (F1 x - F2 x)) volume a b)
    (ha : F1 a = 0) (hb : F1 b = 1) :
    ∫ x in a..b, f1 x * (1 - F2 x) = 1 / 2 + ∫ x in a..b, f1 x * (F1 x - F2 x) := by
  rw [← half_integral F1 f1 a b hF h1 ha hb, ← intervalIntegral.integral_add h1 h2]
  congr 1
  funext x
  ring

/-- p.8: category 2's share of wins falls short of its population share exactly when the
natural win rate of category 1 exceeds one half. -/
theorem share_shortfall_iff (π2 γ0 : ℝ) (h0 : 0 < π2) (h1 : π2 < 1) :
    π2 ^ 2 + 2 * π2 * (1 - π2) * (1 - γ0) < π2 ↔ 1 / 2 < γ0 := by
  have hpos : 0 < π2 * (1 - π2) := mul_pos h0 (by linarith)
  constructor
  · intro h; nlinarith
  · intro h; nlinarith

/-! ## 3. Proposition 3: the blind equilibrium (pp.17-19) -/

/-- The cost-`ĉ` worker's indifference between `ê` and zero effort. -/
theorem ehat_iff (ω F ch eh : ℝ) (hc : 0 < ch) :
    ω * (1 - F) - ch * eh = ω * (1 - F) / 2 ↔ eh = ω / 2 * (1 - F) / ch := by
  constructor
  · intro h; field_simp; linarith
  · intro h; rw [h]; field_simp; ring

/-- The firm's indifference `ê + λ ξ̂ = λ ξ_pool`. -/
theorem lambda_iff (eh xih xipool lam : ℝ) (hξ : xih < xipool) :
    eh + lam * xih = lam * xipool ↔ lam = eh / (xipool - xih) := by
  have : xipool - xih ≠ 0 := by linarith
  constructor
  · intro h; field_simp; linarith
  · intro h; rw [h]; field_simp; ring

/-- Eq. (10): the win rate of category 1 in mixed matches as a function of the cut-off. -/
noncomputable def Gamma (I F1 F2 : ℝ → ℝ) (ch : ℝ) : ℝ :=
  I ch + 1 / 2 * (1 - F1 ch) * (1 - F2 ch)

/-- With `I' = f_1 (1 - F_2)`, the constraint moves with the cut-off at the rate
`(f_1 (1 - F_2) - f_2 (1 - F_1)) / 2`. -/
theorem Gamma_hasDerivAt (I F1 F2 f1 f2 : ℝ → ℝ) (ch : ℝ)
    (hI : HasDerivAt I (f1 ch * (1 - F2 ch)) ch) (h1 : HasDerivAt F1 (f1 ch) ch)
    (h2 : HasDerivAt F2 (f2 ch) ch) :
    HasDerivAt (Gamma I F1 F2) ((f1 ch * (1 - F2 ch) - f2 ch * (1 - F1 ch)) / 2) ch := by
  have h := hI.add (((h1.const_sub 1).const_mul (1 / 2)).mul (h2.const_sub 1))
  unfold Gamma
  refine h.congr_deriv ?_
  ring

/-- Under the hazard-rate order `f_2 (1 - F_1) < f_1 (1 - F_2)` the derivative is
positive. -/
theorem Gamma_deriv_pos (F1 F2 f1 f2 : ℝ) (hhaz : f2 * (1 - F1) < f1 * (1 - F2)) :
    0 < (f1 * (1 - F2) - f2 * (1 - F1)) / 2 := by
  linarith

/-- A lower target gives a lower cut-off, so a larger pool `[ĉ, c_max]` exerting zero
effort. -/
theorem pool_grows (Γ : ℝ → ℝ) (S : Set ℝ) (hΓ : StrictMonoOn Γ S) {c c' γ γ' : ℝ}
    (hc : c ∈ S) (hc' : c' ∈ S) (h : Γ c = γ) (h' : Γ c' = γ') (hγ : γ < γ') : c < c' := by
  rw [← h, ← h'] at hγ
  exact (hΓ.lt_iff_lt hc hc').mp hγ

/-- At the lower end of the support the constraint is `1/2`: a parity target pools every
worker at zero effort. -/
theorem Gamma_at_min (I F1 F2 : ℝ → ℝ) (cmin : ℝ) (hI : I cmin = 0) (h1 : F1 cmin = 0)
    (h2 : F2 cmin = 0) : Gamma I F1 F2 cmin = 1 / 2 := by
  unfold Gamma
  rw [hI, h1, h2]
  norm_num

end FryerLoury2003
