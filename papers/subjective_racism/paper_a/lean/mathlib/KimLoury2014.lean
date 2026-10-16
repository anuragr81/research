import Mathlib

/-! Kim and Loury, "Collective Reputation and the Dynamics of Statistical
Discrimination", MPRA Paper 54950, version of 31 March 2014, in the paper's
notation as far as Lean allows.

Names. The paper's `Π` is written `pi`, because `Π` is binder notation in
Lean, and `λ'` is written `lam'`, because `λ` is a keyword. A reputation
used as a variable is `piR` or `r0` (`π` is taken by Lean's constant).
`piR` is a group's collective reputation `Πⁱ`, `piS` (or `piStar`) the
threshold `Π*`, and `piL`, `piH` the reputations `Π_l`, `Π_h`. `Pq` is the
probability that a qualified worker's test proves him qualified, `Pu` that an
unqualified worker's test proves him unqualified, `ω` the wage of Task One,
`xq = ρ xu` the employer's gain and `xu` his loss, `ξ` the probability of the
benefit of the doubt, `δ` the discount rate, `ω'` the normalised return in
the permanent market, `cm` the medium cost and `A` stands for `δ + λ'`.
Page numbers in the comments are the paper's own, one less than the PDF
page.

Coverage. Sections 3.1 to 3.3 (eqs. 1 to 6, Lemmas 1 and 2), the algebra
and the explicit solutions of the dynamic system of Section 4 (eqs. 9
to 19, Theorem 1's steady states, Propositions 1 to 3, Corollary 1,
Theorem 2), the comparative statics on p. 15, Corollary 3, the policies of
Section 5 (eqs. 20 to 25, Lemma 4, Proposition 5, the quota), eq. (26),
the linear algebra of Lemmas 5 and 6, the change of variable of eqs. (27) and
(28), and Condition 1 of the proof of Proposition 4. `CLAIMS.md` lists what is
general, what is a special case and what is not formalised. -/
namespace KimLoury2014

open Real MeasureTheory Set Filter Topology

/-! ### The return to investment, eq. (2) -/

/-- The flow wage of a qualified worker in the temporary market. He is
assigned to Task One when he passes (probability `Pq`) and, when his result is
unclear (probability `1 − Pq`), when he gets the benefit of the doubt. -/
def wageQ (ω Pq ξ : ℝ) : ℝ := ω * (Pq + (1 - Pq) * ξ)

/-- The flow wage of an unqualified worker. He never passes, and his result is
unclear with probability `1 − Pu`. -/
def wageU (ω Pu ξ : ℝ) : ℝ := ω * ((1 - Pu) * ξ)

/-- Eq. (2), `β(ξ) = ω (Pq + ξ (Pu − Pq))`. -/
def beta (ω Pq Pu ξ : ℝ) : ℝ := ω * (Pq + ξ * (Pu - Pq))

/-- Eq. (2) is the gap between the qualified and the unqualified wage, so the
paper's "ωPu when BOD is given, and ωPq otherwise" follows from its signal
structure. -/
theorem beta_eq_gap (ω Pq Pu ξ : ℝ) : beta ω Pq Pu ξ = wageQ ω Pq ξ - wageU ω Pu ξ := by
  unfold beta wageQ wageU; ring

theorem beta_zero (ω Pq Pu : ℝ) : beta ω Pq Pu 0 = ω * Pq := by unfold beta; ring

theorem beta_one (ω Pq Pu : ℝ) : beta ω Pq Pu 1 = ω * Pu := by unfold beta; ring

/-- With `Pq < Pu` (the unqualified are screened more effectively), the return
to investment rises with the benefit of the doubt. -/
theorem beta_strictMono {ω Pq Pu : ℝ} (hω : 0 < ω) (h : Pq < Pu) :
    StrictMono (beta ω Pq Pu) := by
  intro a b hab
  unfold beta
  have : 0 < Pu - Pq := by linarith
  nlinarith [mul_pos hω this]

/-- Control: with the opposite screening assumption, `Pu < Pq`, the return to
investment falls with the benefit of the doubt, as the paper notes on p. 7.
The monotonicity of `beta_strictMono` needs `Pq < Pu`. -/
theorem control_beta_strictAnti {ω Pq Pu : ℝ} (hω : 0 < ω) (h : Pu < Pq) :
    StrictAnti (beta ω Pq Pu) := by
  intro a b hab
  unfold beta
  have : 0 < Pq - Pu := by linarith
  nlinarith [mul_pos hω this]

/-! ### The employer's benefit of the doubt, Section 3.2 -/

/-- Bayes' rule. The posterior that a group-`i` worker with an unclear result
is qualified, given the group's reputation `piR`. -/
noncomputable def post (Pq Pu piR : ℝ) : ℝ :=
  piR * (1 - Pq) / (piR * (1 - Pq) + (1 - piR) * (1 - Pu))

/-- The threshold `Π* = (1 − Pu) / (ρ (1 − Pq) + 1 − Pu)`. -/
noncomputable def piS (ρ Pq Pu : ℝ) : ℝ := (1 - Pu) / (ρ * (1 - Pq) + 1 - Pu)

/-- The threshold lies strictly between 0 and 1. -/
theorem piS_mem {ρ Pq Pu : ℝ} (hρ : 0 < ρ) (hq : Pq < 1) (hu : Pu < 1) :
    0 < piS ρ Pq Pu ∧ piS ρ Pq Pu < 1 := by
  unfold piS
  have h1 : 0 < 1 - Pu := by linarith
  have h2 : 0 < ρ * (1 - Pq) := mul_pos hρ (by linarith)
  have hD : 0 < ρ * (1 - Pq) + 1 - Pu := by linarith
  exact ⟨div_pos h1 hD, (div_lt_one hD).2 (by linarith)⟩

/-- The employer gives the benefit of the doubt iff the expected return of
assigning an unclear worker to Task One is non-negative, and that holds iff the
group's reputation reaches `Π*`. The gain is written `xq = ρ xu`. The
employer's criterion depends on the worker's group only through `piR`. -/
theorem bod_iff {ρ xu Pq Pu piR : ℝ} (hρ : 0 < ρ) (hxu : 0 < xu) (hq : Pq < 1)
    (hu : Pu < 1) (h0 : 0 ≤ piR) (h1 : piR ≤ 1) :
    0 ≤ ρ * xu * post Pq Pu piR - xu * (1 - post Pq Pu piR) ↔ piS ρ Pq Pu ≤ piR := by
  have hq' : 0 < 1 - Pq := by linarith
  have hu' : 0 < 1 - Pu := by linarith
  have hD : 0 < piR * (1 - Pq) + (1 - piR) * (1 - Pu) := by
    rcases eq_or_lt_of_le h0 with h | h
    · subst h; simpa using hu'
    · have := mul_pos h hq'
      nlinarith [mul_nonneg (sub_nonneg.2 h1) hu'.le]
  have hE : 0 < ρ * (1 - Pq) + 1 - Pu := by nlinarith [mul_pos hρ hq']
  have key : ρ * xu * post Pq Pu piR - xu * (1 - post Pq Pu piR)
      = xu * (piR * (ρ * (1 - Pq) + 1 - Pu) - (1 - Pu))
          / (piR * (1 - Pq) + (1 - piR) * (1 - Pu)) := by
    unfold post; field_simp; ring
  rw [key, div_nonneg_iff]
  unfold piS
  rw [div_le_iff₀ hE]
  constructor
  · rintro (⟨ha, _⟩ | ⟨_, hb⟩)
    · have := (mul_nonneg_iff_of_pos_left hxu).1 ha
      linarith
    · linarith
  · intro h
    left
    exact ⟨mul_nonneg hxu.le (by linarith), hD.le⟩

/-! ### The stage game given a reputation, Section 3.3 and Lemma 1 -/

/-- The employer's rule, `ξ = 1` for `Π ≥ Π*` and `ξ = 0` otherwise. -/
noncomputable def xi (piStar piR : ℝ) : ℝ := if piStar ≤ piR then 1 else 0

/-- The return to investment when the rule `ξ` is expected to hold for ever,
`R({ξ}) = (β(ξ) + ω') / (δ + λ')`, from eq. (4). -/
noncomputable def R (ω Pq Pu ω' δ lam' ξ : ℝ) : ℝ := (beta ω Pq Pu ξ + ω') / (δ + lam')

/-- The cost distribution of Section 3, `G(c) = Π_l` below `cm` and `Π_h` from
`cm` on (on the range `(ε, K)` the paper uses). -/
noncomputable def G (piL piH cm c : ℝ) : ℝ := if c < cm then piL else piH

/-- The share of a cohort that invests when the employer's rule is the one
that the reputation `piR` calls for and is expected to stay. -/
noncomputable def invest (ω Pq Pu ω' δ lam' piL piH cm piStar piR : ℝ) : ℝ :=
  G piL piH cm (R ω Pq Pu ω' δ lam' (xi piStar piR))

/-- Lemma 1. Under `(ωPq + ω')/(δ + λ') < cm < (ωPu + ω')/(δ + λ')` and
`Π_l < Π* < Π_h`, both `Π_l` and `Π_h` reproduce themselves. A reputation is
self-confirming when the share that invests under the rule it calls for
equals it, the employers' correct-belief condition of p. 10. -/
theorem lemma1 {ω Pq Pu ω' δ lam' piL piH cm piStar : ℝ}
    (hlo : (ω * Pq + ω') / (δ + lam') < cm) (hhi : cm < (ω * Pu + ω') / (δ + lam'))
    (hL : piL < piStar) (hH : piStar < piH) :
    invest ω Pq Pu ω' δ lam' piL piH cm piStar piL = piL ∧
      invest ω Pq Pu ω' δ lam' piL piH cm piStar piH = piH := by
  unfold invest xi G R
  rw [if_neg (not_le.2 hL), if_pos hH.le, beta_zero, beta_one,
    if_pos hlo, if_neg (not_lt.2 hhi.le)]
  exact ⟨rfl, rfl⟩

/-- Control: drop `Π_l < Π*`. If the low reputation already earns the benefit
of the doubt, `Π_l` does not reproduce itself. -/
theorem control_low_not_equilibrium_without_threshold {ω Pq Pu ω' δ lam' piL piH cm piStar : ℝ}
    (hhi : cm < (ω * Pu + ω') / (δ + lam')) (hLH : piL < piH) (hS : piStar ≤ piL) :
    invest ω Pq Pu ω' δ lam' piL piH cm piStar piL ≠ piL := by
  unfold invest xi G R
  rw [if_pos hS, beta_one, if_neg (not_lt.2 hhi.le)]
  exact ne_of_gt hLH

/-- Control: drop `(ωPq + ω')/(δ + λ') < cm`. If the medium-cost type invests
even without the benefit of the doubt, `Π_l` does not reproduce itself. -/
theorem control_low_not_equilibrium_cheap_cost {ω Pq Pu ω' δ lam' piL piH cm piStar : ℝ}
    (hlo : cm ≤ (ω * Pq + ω') / (δ + lam')) (hLH : piL < piH) (hL : piL < piStar) :
    invest ω Pq Pu ω' δ lam' piL piH cm piStar piL ≠ piL := by
  unfold invest xi G R
  rw [if_neg (not_le.2 hL), beta_zero, if_neg (not_lt.2 hlo)]
  exact ne_of_gt hLH

/-! ### Two closed forms of the dynamics -/

/-- Lemma 2, eq. (7). With the return of eq. (6), the newborn invests iff the
switch date `T` is near enough. Here `A = δ + λ'`, and the conditions are
`ωPq < ωPu` and `(δ + λ') cm > ωPq + ω'`, the left inequality of Lemma 1. -/
theorem lemma2_iff {ω Pq Pu ω' A cm T : ℝ} (hA : 0 < A) (hd : ω * Pq < ω * Pu)
    (hk : ω * Pq + ω' < A * cm) :
    cm ≤ 1 / A * (ω * Pq + ω' + (ω * Pu - ω * Pq) * exp (-(A * T))) ↔
      T ≤ 1 / A * log ((ω * Pu - ω * Pq) / (A * cm - ω * Pq - ω')) := by
  set d := ω * Pu - ω * Pq with hd'
  set k := A * cm - ω * Pq - ω' with hk'
  have dpos : 0 < d := by linarith
  have kpos : 0 < k := by linarith
  have step1 : cm ≤ 1 / A * (ω * Pq + ω' + d * exp (-(A * T))) ↔ k / d ≤ exp (-(A * T)) := by
    rw [one_div, ← div_eq_inv_mul, le_div_iff₀ hA, div_le_iff₀ dpos]
    constructor <;> intro h <;> nlinarith
  have step2 : k / d ≤ exp (-(A * T)) ↔ A * T ≤ log (d / k) := by
    rw [← log_le_iff_le_exp (div_pos kpos dpos), log_div kpos.ne' dpos.ne',
      log_div dpos.ne' kpos.ne']
    constructor <;> intro h <;> linarith
  rw [step1, step2, one_div, ← div_eq_inv_mul, le_div_iff₀ hA, mul_comm]

/-- Theorem 2, eq. (19). The low reputation lies below `π° = Π_h + (Π* − Π_h)
v*^(−a)` iff `(Π_h − Π*)/(Π_h − Π_l) < v*^a`, with `a = λ'/(δ + λ')`. -/
theorem trap_iff {piL piH piStar v a : ℝ} (hv : 0 < v) (hLH : piL < piH) :
    piL < piH + (piStar - piH) * v ^ (-a) ↔ (piH - piStar) / (piH - piL) < v ^ a := by
  have hpa : 0 < v ^ a := rpow_pos_of_pos hv a
  have hgap : 0 < piH - piL := by linarith
  rw [rpow_neg hv.le, div_lt_iff₀ hgap]
  have e : piH + (piStar - piH) * (v ^ a)⁻¹ = piH - (piH - piStar) / v ^ a := by
    rw [div_eq_mul_inv]; ring
  rw [e]
  constructor
  · intro h
    have h' : (piH - piStar) / v ^ a < piH - piL := by linarith
    rw [div_lt_iff₀ hpa] at h'
    linarith
  · intro h
    have h' : (piH - piStar) / v ^ a < piH - piL := by
      rw [div_lt_iff₀ hpa]; linarith
    linarith


/-! ### Section 3.1, eqs. (1), (8) and (9) -/

/-- Eq. (1). The limit `Δt → 0` of the fixed-size condition
`λ = nη + nλ − nηλΔt` is `λ = n(η + λ)`, and then `λ + η = λ/n`. -/
theorem eq1 {lam η n : ℝ} (hn : n ≠ 0) (h : lam = n * η + n * lam) :
    lam + η = lam / n := by
  rw [eq_div_iff hn]; linear_combination -h

/-- The rearrangement of eq. (8). The difference quotient of the group's
reputation in the temporary market. -/
theorem eq8_quotient {lam η n N piR piR' φ Δt : ℝ} (hN : N ≠ 0) (hn : n ≠ 0) (hΔ : Δt ≠ 0)
    (h8 : N * n * piR' = (1 - lam * Δt - η * Δt + lam * η * Δt ^ 2) * N * n * piR
      + N * lam * Δt * φ) :
    (piR' - piR) / Δt = -(lam + η) * piR + lam / n * φ + lam * η * Δt * piR := by
  have h : piR' = (1 - lam * Δt - η * Δt + lam * η * Δt ^ 2) * piR + lam / n * Δt * φ := by
    field_simp
    have := h8
    field_simp at this ⊢
    linear_combination this
  rw [h]; field_simp; ring

/-- Eq. (9). With eq. (1), the difference quotient tends to `λ'(φ − Π)` as
`Δt → 0`. -/
theorem eq9 {lam η n φ piR : ℝ} (h1 : lam + η = lam / n) :
    Tendsto (fun Δt : ℝ => -(lam + η) * piR + lam / n * φ + lam * η * Δt * piR) (𝓝 0)
      (𝓝 ((lam + η) * (φ - piR))) := by
  have hc : Continuous (fun Δt : ℝ => -(lam + η) * piR + lam / n * φ + lam * η * Δt * piR) := by
    fun_prop
  have e : (lam + η) * (φ - piR) = -(lam + η) * piR + lam / n * φ + lam * η * 0 * piR := by
    rw [← h1]; ring
  rw [e]; exact hc.tendsto 0

/-! ### Section 3.3, eqs. (3), (4) and (6) -/

theorem disc_integrable {A : ℝ} (hA : 0 < A) (c t : ℝ) :
    IntegrableOn (fun τ => c * exp (-(A * (τ - t)))) (Ioi t) := by
  have e : (fun τ => c * exp (-(A * (τ - t)))) = fun τ => (c * exp (A * t)) * exp ((-A) * τ) := by
    funext τ; rw [mul_assoc, ← exp_add]; ring_nf
  rw [e]
  exact (integrableOn_exp_mul_Ioi (by linarith) t).const_mul _

/-- `∫_t^∞ c e^{−A(τ−t)} dτ = c/A`. -/
theorem disc_integral {A : ℝ} (hA : 0 < A) (c t : ℝ) :
    ∫ τ in Ioi t, c * exp (-(A * (τ - t))) = c / A := by
  have e : (fun τ => c * exp (-(A * (τ - t)))) = fun τ => (c * exp (A * t)) * exp ((-A) * τ) := by
    funext τ; rw [mul_assoc, ← exp_add]; ring_nf
  rw [e, integral_const_mul, integral_exp_mul_Ioi (by linarith) t]
  rw [show -A * t = -(A * t) by ring, exp_neg]
  field_simp

/-- The return of eq. (4) for an expected path `β` of flow returns. -/
noncomputable def Rint (A ω' : ℝ) (β : ℝ → ℝ) (t : ℝ) : ℝ :=
  (∫ τ in Ioi t, β τ * exp (-(A * (τ - t)))) + ω' / A

/-- Eq. (3). The two integrals of the paper, the temporary and the permanent
market, sum to the compressed form (4) with `λ' = λ + η` and
`ω' = ηω/(δ + λ)`. -/
theorem eq3 {δ lam η ω : ℝ} (hB : 0 < δ + lam) (hη : 0 ≤ η) (β : ℝ → ℝ) (t : ℝ) :
    (∫ τ in Ioi t, (β τ * exp (-(η * (τ - t)))) * exp (-((δ + lam) * (τ - t))))
      + (∫ τ in Ioi t, (ω * (1 - exp (-(η * (τ - t))))) * exp (-((δ + lam) * (τ - t))))
      = Rint (δ + (lam + η)) (η * ω / (δ + lam)) β t := by
  have hA : 0 < δ + lam + η := by linarith
  have e1 : (fun τ => (β τ * exp (-(η * (τ - t)))) * exp (-((δ + lam) * (τ - t))))
      = fun τ => β τ * exp (-((δ + (lam + η)) * (τ - t))) := by
    funext τ; rw [mul_assoc, ← exp_add]; ring_nf
  have e2 : (fun τ => (ω * (1 - exp (-(η * (τ - t))))) * exp (-((δ + lam) * (τ - t))))
      = fun τ => ω * exp (-((δ + lam) * (τ - t))) - ω * exp (-((δ + lam + η) * (τ - t))) := by
    funext τ; rw [mul_sub, mul_one, sub_mul, mul_assoc, ← exp_add]; ring_nf
  rw [e1, e2, integral_sub (disc_integrable hB ω t) (disc_integrable hA ω t),
    disc_integral hB, disc_integral hA]
  unfold Rint
  congr 1
  rw [show δ + (lam + η) = δ + lam + η by ring]
  have := hA.ne'; have := hB.ne'
  field_simp
  ring

/-- Eq. (4) with a constant expected rule. `R({0}) = (ωPq + ω')/(δ + λ')` and
`R({1}) = (ωPu + ω')/(δ + λ')`, the values before Lemma 1. -/
theorem eq4_const {A : ℝ} (hA : 0 < A) (ω' b t : ℝ) :
    Rint A ω' (fun _ => b) t = (b + ω') / A := by
  unfold Rint; rw [disc_integral hA]; ring

theorem eq4_R0 {A : ℝ} (hA : 0 < A) (ω Pq Pu ω' t : ℝ) :
    Rint A ω' (fun _ => beta ω Pq Pu 0) t = (ω * Pq + ω') / A := by
  rw [eq4_const hA, beta_zero]

theorem eq4_R1 {A : ℝ} (hA : 0 < A) (ω Pq Pu ω' t : ℝ) :
    Rint A ω' (fun _ => beta ω Pq Pu 1) t = (ω * Pu + ω') / A := by
  rw [eq4_const hA, beta_one]

/-- The closed form of eq. (6), with flow return `bq` before the switch date
`T` and `bu` after it. -/
noncomputable def Rsw (A ω' bq bu T : ℝ) : ℝ := 1 / A * (bq + ω' + (bu - bq) * exp (-(A * T)))

/-- Eq. (6). The return of a newborn at `t = 0` who expects the benefit of the
doubt to start at `T ≥ 0`. -/
theorem eq6 {A : ℝ} (hA : 0 < A) (ω' bq bu T : ℝ) :
    (∫ τ in (0 : ℝ)..T, bq * exp (-(A * τ))) + (∫ τ in Ioi T, bu * exp (-(A * τ))) + ω' / A
      = Rsw A ω' bq bu T := by
  have h1 : ∫ τ in (0 : ℝ)..T, bq * exp (-(A * τ)) = bq / A * (1 - exp (-(A * T))) := by
    have hd : ∀ x ∈ uIcc (0 : ℝ) T,
        HasDerivAt (fun τ => -(bq / A) * exp (-(A * τ))) (bq * exp (-(A * x))) x := by
      intro x _
      have h := ((hasDerivAt_id x).const_mul A).neg.exp.const_mul (-(bq / A))
      convert h using 1
      · simp [id]; field_simp
    rw [intervalIntegral.integral_eq_sub_of_hasDerivAt hd
      ((continuous_const.mul (continuous_exp.comp (continuous_const.mul continuous_id).neg)).intervalIntegrable _ _)]
    simp; ring
  have h2 : ∫ τ in Ioi T, bu * exp (-(A * τ)) = bu * exp (-(A * T)) / A := by
    have e : (fun τ => bu * exp (-(A * τ))) = fun τ => (bu * exp (-(A * T))) * exp (-(A * (τ - T))) := by
      funext τ; rw [mul_assoc, ← exp_add]; ring_nf
    rw [e, disc_integral hA]
  rw [h1, h2]; unfold Rsw; field_simp; ring

/-! ### Solutions of the linear equations of the system -/

/-- The linear equation `ẋ = a (c − x)` has the unique solution
`x t = c + (x 0 − c) e^{−a t}`. Uniqueness is proved, not assumed. -/
theorem linear_unique {x : ℝ → ℝ} {a c : ℝ} (hx : ∀ t, HasDerivAt x (a * (c - x t)) t) (t : ℝ) :
    x t = c + (x 0 - c) * exp (-(a * t)) := by
  set g : ℝ → ℝ := fun s => (x s - c) * exp (a * s) with hg_def
  have hg : ∀ s, HasDerivAt g 0 s := by
    intro s
    have h1 := ((hx s).sub_const c).mul (((hasDerivAt_id s).const_mul a).exp)
    convert h1 using 1
    simp only [id, mul_one]; ring
  have hdiff : Differentiable ℝ g := fun s => (hg s).differentiableAt
  have hconst := is_const_of_deriv_eq_zero hdiff (fun s => (hg s).deriv) t 0
  simp only [hg_def, mul_zero, exp_zero, mul_one] at hconst
  have : x t - c = (x 0 - c) * exp (-(a * t)) := by
    rw [← hconst, mul_assoc, ← exp_add]; simp
  linarith

/-- The explicit solution satisfies the equation. -/
theorem linear_solves (a c x0 : ℝ) (t : ℝ) :
    HasDerivAt (fun s => c + (x0 - c) * exp (-(a * s)))
      (a * (c - (c + (x0 - c) * exp (-(a * t))))) t := by
  have h := (((hasDerivAt_id t).const_mul a).neg.exp.const_mul (x0 - c)).const_add c
  convert h using 1
  simp only [id, mul_one, Pi.neg_apply]; ring

theorem log_eq_iff_eq_exp {r L : ℝ} (hr : 0 < r) : log r = L ↔ r = exp L := by
  constructor
  · intro h; rw [← h, exp_log hr]
  · intro h; rw [h, log_exp]

/-- Eq. (10). Along `Π̇ = λ'(Π_h − Π)` from `Π_0 < Π* < Π_h`, the reputation
equals `Π*` exactly at `T = (1/λ') ln((Π_h − Π_0)/(Π_h − Π*))`. -/
theorem eq10 {x : ℝ → ℝ} {lam' piH piStar : ℝ} (hl : 0 < lam')
    (hx : ∀ t, HasDerivAt x (lam' * (piH - x t)) t) (h0 : x 0 < piStar) (hS : piStar < piH)
    (t : ℝ) :
    x t = piStar ↔ t = 1 / lam' * log ((piH - x 0) / (piH - piStar)) := by
  rw [linear_unique hx t]
  have p1 : 0 < piH - x 0 := by linarith
  have p2 : 0 < piH - piStar := by linarith
  have e : piH + (x 0 - piH) * exp (-(lam' * t)) = piStar
      ↔ (piH - piStar) / (piH - x 0) = exp (-(lam' * t)) := by
    rw [div_eq_iff p1.ne']
    constructor <;> intro h <;> linarith
  rw [e, ← log_eq_iff_eq_exp (div_pos p2 p1), ← inv_div, log_inv]
  constructor
  · intro h; field_simp; linarith
  · intro h; rw [h]; field_simp

/-- The time `T(Π_0)` that the reputation takes from `Π_0` to `Π*` when every
cohort invests at `Π_h`, eq. (10). -/
noncomputable def Tup (lam' piH piStar pi0 : ℝ) : ℝ := 1 / lam' * log ((piH - pi0) / (piH - piStar))

/-- Eq. (14) along the paths the paper studies (a special case). With flow
return `bq` before `T` and `bu` after, the normalised return
`V(t) = (δ + λ') R` is `bq + ω' + (bu − bq) e^{−A(T − t)}` before `T`, and it
satisfies `V̇ = A (V − β − ω')`. -/
theorem eq14_switch (A ω' bq bu T t : ℝ) :
    HasDerivAt (fun s => bq + ω' + (bu - bq) * exp (-(A * (T - s))))
      (A * ((bq + ω' + (bu - bq) * exp (-(A * (T - t)))) - bq - ω')) t := by
  have h := ((((hasDerivAt_id t).const_sub T).const_mul A).neg.exp.const_mul (bu - bq)).const_add
    (bq + ω')
  convert h using 1
  simp only [id, Pi.neg_apply]; ring_nf

/-- The normalised return at `t = 0` along the switch path is `A · Rsw`. -/
theorem eq13_switch {A : ℝ} (hA : 0 < A) (ω' bq bu T : ℝ) :
    A * Rsw A ω' bq bu T = bq + ω' + (bu - bq) * exp (-(A * (T - 0))) := by
  unfold Rsw; rw [sub_zero]; field_simp

/-! ### Theorem 1, the dynamic system and its steady states -/

/-- The right-hand side of Theorem 1. The first coordinate is `V̇ᵉ`, the
second is `Π̇`, with `φ = G(Vᵉ/(δ + λ'))` and `ξ = 1` iff `Π ≥ Π*`. -/
noncomputable def field (ω Pq Pu ω' A lam' piL piH cm piStar V piR : ℝ) : ℝ × ℝ :=
  (A * (V - beta ω Pq Pu (xi piStar piR) - ω'), lam' * (G piL piH cm (V / A) - piR))

/-- Theorem 1 and Section 4.1. Under Lemma 1's conditions the system's steady
states are exactly `Q_l = (ωPq + ω', Π_l)` and `Q_h = (ωPu + ω', Π_h)`. -/
theorem steady_states {ω Pq Pu ω' A lam' piL piH cm piStar V piR : ℝ} (hA : 0 < A)
    (hl : 0 < lam') (hlo : (ω * Pq + ω') / A < cm) (hhi : cm < (ω * Pu + ω') / A)
    (hL : piL < piStar) (hH : piStar < piH) :
    field ω Pq Pu ω' A lam' piL piH cm piStar V piR = (0, 0) ↔
      (V = ω * Pq + ω' ∧ piR = piL) ∨ (V = ω * Pu + ω' ∧ piR = piH) := by
  unfold field
  simp only [Prod.mk.injEq, mul_eq_zero, hA.ne', hl.ne', false_or, sub_eq_zero]
  constructor
  · rintro ⟨hV, hP⟩
    unfold xi at hV
    split_ifs at hV with hs
    · right
      rw [beta_one] at hV
      refine ⟨by linarith, ?_⟩
      unfold G at hP
      have : V / A = (ω * Pu + ω') / A := by congr 1; linarith
      rw [this, if_neg (not_lt.2 hhi.le)] at hP
      exact hP.symm
    · left
      rw [beta_zero] at hV
      refine ⟨by linarith, ?_⟩
      have : V / A = (ω * Pq + ω') / A := by congr 1; linarith
      unfold G at hP
      rw [this, if_pos hlo] at hP
      exact hP.symm
  · rintro (⟨rfl, rfl⟩ | ⟨rfl, rfl⟩)
    · unfold xi G; rw [if_neg (not_le.2 hL), beta_zero, if_pos hlo]; constructor <;> ring
    · unfold xi G; rw [if_pos hH.le, beta_one, if_neg (not_lt.2 hhi.le)]; constructor <;> ring

/-- Control: without `Π_l < Π*` the point `(ωPq + ω', Π_l)` is not a steady
state, since the low reputation already earns the benefit of the doubt. -/
theorem control_steady_needs_threshold {ω Pq Pu ω' A lam' piL piH cm piStar : ℝ} (hA : 0 < A)
    (hω : 0 < ω) (hPq : Pq < Pu) (hS : piStar ≤ piL) :
    field ω Pq Pu ω' A lam' piL piH cm piStar (ω * Pq + ω') piL ≠ (0, 0) := by
  unfold field xi
  rw [if_pos hS, beta_one]
  intro h
  have := (Prod.mk.injEq _ _ _ _ ▸ h).1
  have h2 : A * (ω * Pq + ω' - ω * Pu - ω') < 0 := by
    apply mul_neg_of_pos_of_neg hA; nlinarith
  linarith

/-! ### Section 4.2 and 4.3, the paths, the overlap and the trap -/

/-- `v* = ((δ + λ')cm − ωPq − ω')/(ωPu − ωPq)`, with `A = δ + λ'`. -/
noncomputable def vS (A ω Pq Pu ω' cm : ℝ) : ℝ := (A * cm - ω * Pq - ω') / (ω * Pu - ω * Pq)

/-- Eq. (17), `π° = Π_h + (Π* − Π_h) v*^{−λ'/(δ+λ')}`. -/
noncomputable def piO (A lam' piH piStar v : ℝ) : ℝ := piH + (piStar - piH) * v ^ (-(lam' / A))

/-- Eq. (18), `πᵖ = Π_l + (Π* − Π_l)(1 − v*)^{−λ'/(δ+λ')}`. -/
noncomputable def piP (A lam' piL piStar v : ℝ) : ℝ := piL + (piStar - piL) * (1 - v) ^ (-(lam' / A))

/-- The time a falling reputation takes from `Π_0 > Π*` to `Π*` when every
cohort invests at `Π_l`. -/
noncomputable def Tdown (lam' piL piStar pi0 : ℝ) : ℝ := 1 / lam' * log ((pi0 - piL) / (piStar - piL))

theorem vS_mem {A ω Pq Pu ω' cm : ℝ} (hk : ω * Pq + ω' < A * cm)
    (hk2 : A * cm < ω * Pu + ω') : 0 < vS A ω Pq Pu ω' cm ∧ vS A ω Pq Pu ω' cm < 1 := by
  unfold vS
  have hd' : 0 < ω * Pu - ω * Pq := by linarith
  exact ⟨div_pos (by linarith) hd', (div_lt_one hd').2 (by linarith)⟩

theorem rpow_neg_eq_exp {v : ℝ} (hv : 0 < v) (a : ℝ) : v ^ (-a) = exp (a * log v⁻¹) := by
  rw [rpow_def_of_pos hv, log_inv]; ring_nf

/-- Eq. (15), the return side. Along `V̇ = (δ+λ')(V − ωPq − ω')` from
`(δ+λ')cm`, the return reaches `ωPu + ω'` exactly at `(1/(δ+λ')) ln(1/v*)`. -/
theorem eq15_return {V : ℝ → ℝ} {A ω Pq Pu ω' cm : ℝ} (hA : 0 < A) (hd : ω * Pq < ω * Pu)
    (hk : ω * Pq + ω' < A * cm) (hV : ∀ t, HasDerivAt V (A * (V t - (ω * Pq + ω'))) t)
    (h0 : V 0 = A * cm) (s : ℝ) :
    V s = ω * Pu + ω' ↔ s = 1 / A * log (vS A ω Pq Pu ω' cm)⁻¹ := by
  have hV' : ∀ t, HasDerivAt V ((-A) * ((ω * Pq + ω') - V t)) t := by
    intro t; convert hV t using 1; ring
  rw [linear_unique hV' s, h0]
  have hk' : 0 < A * cm - ω * Pq - ω' := by linarith
  have hd' : 0 < ω * Pu - ω * Pq := by linarith
  have e : ω * Pq + ω' + (A * cm - (ω * Pq + ω')) * exp (-(-A * s)) = ω * Pu + ω'
      ↔ (vS A ω Pq Pu ω' cm)⁻¹ = exp (A * s) := by
    unfold vS; rw [inv_div, div_eq_iff hk'.ne', neg_mul, neg_neg]
    constructor <;> intro h <;> linarith
  have hpos : 0 < (vS A ω Pq Pu ω' cm)⁻¹ := inv_pos.2 (show 0 < vS A ω Pq Pu ω' cm from div_pos hk' hd')
  rw [e, ← log_eq_iff_eq_exp (r := (vS A ω Pq Pu ω' cm)⁻¹) (L := A * s) hpos]
  constructor
  · intro h; rw [h]; field_simp
  · intro h; rw [h]; field_simp

/-- Control for Lemma 2: drop `(δ + λ')cm > ωPq + ω'`. Then the newborn
invests whatever the switch date, so no bound on `T` characterises investment. -/
theorem control_lemma2_cheap {A ω Pq Pu ω' cm : ℝ} (hA : 0 < A) (hd : ω * Pq < ω * Pu)
    (hc : A * cm ≤ ω * Pq + ω') (T : ℝ) : cm ≤ Rsw A ω' (ω * Pq) (ω * Pu) T := by
  unfold Rsw
  have := exp_pos (-(A * T))
  rw [one_div, ← div_eq_inv_mul, le_div_iff₀ hA]
  nlinarith

/-- Eq. (17). The reputation `r0` from which the optimistic path takes as long
to reach `Π*` as the return takes to reach `ωPu + ω'` is `π°`. -/
theorem eq17 {A lam' piH piStar v r0 : ℝ} (hA : 0 < A) (hl : 0 < lam') (hv : 0 < v)
    (hS : piStar < piH) (hr0 : r0 < piH) :
    Tup lam' piH piStar r0 = 1 / A * log v⁻¹ ↔ r0 = piO A lam' piH piStar v := by
  unfold Tup piO
  have p1 : 0 < piH - r0 := by linarith
  have p2 : 0 < piH - piStar := by linarith
  rw [rpow_neg_eq_exp hv]
  have e1 : 1 / lam' * log ((piH - r0) / (piH - piStar)) = 1 / A * log v⁻¹
      ↔ log ((piH - r0) / (piH - piStar)) = lam' / A * log v⁻¹ := by
    constructor <;> intro h
    · field_simp at h ⊢; linarith
    · rw [h]; field_simp
  rw [e1, log_eq_iff_eq_exp (div_pos p1 p2), div_eq_iff p2.ne']
  constructor <;> intro h <;> linarith

/-- Eq. (10) for a falling reputation. Along `Π̇ = λ'(Π_l − Π)` from
`Π_0 > Π* > Π_l`, the reputation equals `Π*` exactly at `Tdown`. -/
theorem eq10_down {x : ℝ → ℝ} {lam' piL piStar : ℝ} (hl : 0 < lam')
    (hx : ∀ t, HasDerivAt x (lam' * (piL - x t)) t) (h0 : piStar < x 0) (hS : piL < piStar)
    (t : ℝ) : x t = piStar ↔ t = Tdown lam' piL piStar (x 0) := by
  rw [linear_unique hx t]
  unfold Tdown
  have p1 : 0 < x 0 - piL := by linarith
  have p2 : 0 < piStar - piL := by linarith
  have e : piL + (x 0 - piL) * exp (-(lam' * t)) = piStar
      ↔ (piStar - piL) / (x 0 - piL) = exp (-(lam' * t)) := by
    rw [div_eq_iff p1.ne']
    constructor <;> intro h <;> linarith
  rw [e, ← log_eq_iff_eq_exp (div_pos p2 p1), ← inv_div, log_inv]
  constructor
  · intro h; field_simp; linarith
  · intro h; rw [h]; field_simp

/-- Eq. (16), the return side. Along `V̇ = (δ+λ')(V − ωPu − ω')` from
`(δ+λ')cm`, the return reaches `ωPq + ω'` exactly at `(1/(δ+λ')) ln(1/(1−v*))`. -/
theorem eq16_return {V : ℝ → ℝ} {A ω Pq Pu ω' cm : ℝ} (hA : 0 < A) (hd : ω * Pq < ω * Pu)
    (hk2 : A * cm < ω * Pu + ω') (hV : ∀ t, HasDerivAt V (A * (V t - (ω * Pu + ω'))) t)
    (h0 : V 0 = A * cm) (s : ℝ) :
    V s = ω * Pq + ω' ↔ s = 1 / A * log (1 - vS A ω Pq Pu ω' cm)⁻¹ := by
  have hV' : ∀ t, HasDerivAt V ((-A) * ((ω * Pu + ω') - V t)) t := by
    intro t; convert hV t using 1; ring
  rw [linear_unique hV' s, h0]
  have hk' : 0 < ω * Pu + ω' - A * cm := by linarith
  have hd' : 0 < ω * Pu - ω * Pq := by linarith
  have h1v : 1 - vS A ω Pq Pu ω' cm = (ω * Pu + ω' - A * cm) / (ω * Pu - ω * Pq) := by
    unfold vS; rw [one_sub_div hd'.ne']; congr 1; ring
  have e : ω * Pu + ω' + (A * cm - (ω * Pu + ω')) * exp (-(-A * s)) = ω * Pq + ω'
      ↔ (1 - vS A ω Pq Pu ω' cm)⁻¹ = exp (A * s) := by
    rw [h1v, inv_div, div_eq_iff hk'.ne', neg_mul, neg_neg]
    constructor <;> intro h <;> linarith
  have hpos : 0 < (1 - vS A ω Pq Pu ω' cm)⁻¹ := by rw [h1v]; exact inv_pos.2 (div_pos hk' hd')
  rw [e, ← log_eq_iff_eq_exp (r := (1 - vS A ω Pq Pu ω' cm)⁻¹) (L := A * s) hpos]
  constructor
  · intro h; rw [h]; field_simp
  · intro h; rw [h]; field_simp

/-- Eq. (18). The reputation from which the pessimistic path takes as long to
reach `Π*` as the return takes to reach `ωPq + ω'` is `πᵖ`. -/
theorem eq18 {A lam' piL piStar v r0 : ℝ} (hA : 0 < A) (hl : 0 < lam') (hv : v < 1)
    (hS : piL < piStar) (hr0 : piL < r0) :
    Tdown lam' piL piStar r0 = 1 / A * log (1 - v)⁻¹ ↔ r0 = piP A lam' piL piStar v := by
  unfold Tdown piP
  have p1 : 0 < r0 - piL := by linarith
  have p2 : 0 < piStar - piL := by linarith
  rw [rpow_neg_eq_exp (by linarith : (0 : ℝ) < 1 - v)]
  have e1 : 1 / lam' * log ((r0 - piL) / (piStar - piL)) = 1 / A * log (1 - v)⁻¹
      ↔ log ((r0 - piL) / (piStar - piL)) = lam' / A * log (1 - v)⁻¹ := by
    constructor <;> intro h
    · field_simp at h ⊢; linarith
    · rw [h]; field_simp
  rw [e1, log_eq_iff_eq_exp (div_pos p1 p2), div_eq_iff p2.ne']
  constructor <;> intro h <;> linarith

/-- `π° < Π*` when `0 < v* < 1`. -/
theorem piO_lt_piStar {A lam' piH piStar v : ℝ} (hA : 0 < A) (hl : 0 < lam') (hv0 : 0 < v)
    (hv1 : v < 1) (hS : piStar < piH) : piO A lam' piH piStar v < piStar := by
  unfold piO
  have : 1 < v ^ (-(lam' / A)) :=
    one_lt_rpow_of_pos_of_lt_one_of_neg hv0 hv1 (by have := div_pos hl hA; linarith)
  nlinarith

/-- `Π* < πᵖ` when `0 < v* < 1`. -/
theorem piStar_lt_piP {A lam' piL piStar v : ℝ} (hA : 0 < A) (hl : 0 < lam') (hv0 : 0 < v)
    (hv1 : v < 1) (hS : piL < piStar) : piStar < piP A lam' piL piStar v := by
  unfold piP
  have : 1 < (1 - v) ^ (-(lam' / A)) :=
    one_lt_rpow_of_pos_of_lt_one_of_neg (by linarith) (by linarith)
      (by have := div_pos hl hA; linarith)
  nlinarith

/-- Lemma 2 and eq. (10) together, the optimistic side of Lemma 3. From a
reputation `r0 < Π*`, the newborn invests on the optimistic conjecture iff
`r0 ≥ π°`. -/
theorem opt_feasible_iff {A lam' ω Pq Pu ω' cm piH piStar r0 : ℝ} (hA : 0 < A) (hl : 0 < lam')
    (hd : ω * Pq < ω * Pu) (hk : ω * Pq + ω' < A * cm) (hS : piStar < piH) (hr0 : r0 < piH) :
    cm ≤ Rsw A ω' (ω * Pq) (ω * Pu) (Tup lam' piH piStar r0)
      ↔ piO A lam' piH piStar (vS A ω Pq Pu ω' cm) ≤ r0 := by
  have hk' : 0 < A * cm - ω * Pq - ω' := by linarith
  have hd' : 0 < ω * Pu - ω * Pq := by linarith
  have hv : 0 < vS A ω Pq Pu ω' cm := div_pos hk' hd'
  unfold Rsw
  rw [lemma2_iff hA hd hk, show (ω * Pu - ω * Pq) / (A * cm - ω * Pq - ω')
    = (vS A ω Pq Pu ω' cm)⁻¹ by unfold vS; rw [inv_div]]
  unfold Tup piO
  have p1 : 0 < piH - r0 := by linarith
  have p2 : 0 < piH - piStar := by linarith
  rw [rpow_neg_eq_exp hv]
  have e1 : 1 / lam' * log ((piH - r0) / (piH - piStar)) ≤ 1 / A * log (vS A ω Pq Pu ω' cm)⁻¹
      ↔ log ((piH - r0) / (piH - piStar)) ≤ lam' / A * log (vS A ω Pq Pu ω' cm)⁻¹ := by
    rw [one_div_mul_eq_div, one_div_mul_eq_div, div_le_div_iff₀ hl hA, div_mul_eq_mul_div,
      le_div_iff₀ hA]
    constructor <;> intro h <;> nlinarith
  rw [e1, log_le_iff_le_exp (div_pos p1 p2), div_le_iff₀ p2]
  constructor <;> intro h <;> nlinarith

/-- The pessimistic side of Lemma 3. From a reputation `r0 ≥ Π*`, the newborn
stops investing on the pessimistic conjecture iff `r0 < πᵖ`. -/
theorem pes_feasible_iff {A lam' ω Pq Pu ω' cm piL piStar r0 : ℝ} (hA : 0 < A) (hl : 0 < lam')
    (hd : ω * Pq < ω * Pu) (hk2 : A * cm < ω * Pu + ω') (hS : piL < piStar) (hr0 : piL < r0) :
    Rsw A ω' (ω * Pu) (ω * Pq) (Tdown lam' piL piStar r0) < cm
      ↔ r0 < piP A lam' piL piStar (vS A ω Pq Pu ω' cm) := by
  have hk' : 0 < ω * Pu + ω' - A * cm := by linarith
  have hd' : 0 < ω * Pu - ω * Pq := by linarith
  have h1v : 1 - vS A ω Pq Pu ω' cm = (ω * Pu + ω' - A * cm) / (ω * Pu - ω * Pq) := by
    unfold vS; rw [one_sub_div hd'.ne']; congr 1; ring
  have hw : 0 < 1 - vS A ω Pq Pu ω' cm := by rw [h1v]; exact div_pos hk' hd'
  set T := Tdown lam' piL piStar r0 with hT
  have step1 : Rsw A ω' (ω * Pu) (ω * Pq) T < cm ↔ 1 - vS A ω Pq Pu ω' cm < exp (-(A * T)) := by
    unfold Rsw
    rw [h1v, one_div, ← div_eq_inv_mul, div_lt_iff₀ hA, div_lt_iff₀ hd']
    constructor <;> intro h <;> nlinarith
  have step2 : 1 - vS A ω Pq Pu ω' cm < exp (-(A * T)) ↔ A * T < log (1 - vS A ω Pq Pu ω' cm)⁻¹ := by
    rw [log_inv, ← log_lt_iff_lt_exp hw]
    constructor <;> intro h <;> linarith
  rw [step1, step2, hT]
  unfold Tdown piP
  have p1 : 0 < r0 - piL := by linarith
  have p2 : 0 < piStar - piL := by linarith
  rw [rpow_neg_eq_exp hw]
  have e1 : A * (1 / lam' * log ((r0 - piL) / (piStar - piL))) < log (1 - vS A ω Pq Pu ω' cm)⁻¹
      ↔ log ((r0 - piL) / (piStar - piL)) < lam' / A * log (1 - vS A ω Pq Pu ω' cm)⁻¹ := by
    rw [show A * (1 / lam' * log ((r0 - piL) / (piStar - piL)))
        = log ((r0 - piL) / (piStar - piL)) / (lam' / A) by field_simp,
      div_lt_iff₀ (div_pos hl hA)]
    constructor <;> intro h <;> linarith
  rw [e1, log_lt_iff_lt_exp (div_pos p1 p2), div_lt_iff₀ p2]
  constructor <;> intro h <;> nlinarith

/-- `v*^{−a} = ((ωPu − ωPq)/((δ+λ')cm − ωPq − ω'))^{a}`, the bridge between
eqs. (11) and (17). -/
theorem vS_rpow_neg {A ω Pq Pu ω' cm : ℝ} (hd : ω * Pq < ω * Pu) (hk : ω * Pq + ω' < A * cm)
    (a : ℝ) : (vS A ω Pq Pu ω' cm) ^ (-a)
      = ((ω * Pu - ω * Pq) / (A * cm - ω * Pq - ω')) ^ a := by
  have hv : 0 ≤ vS A ω Pq Pu ω' cm := (div_pos (by linarith) (by linarith)).le
  rw [rpow_neg hv, ← inv_rpow hv]; unfold vS; rw [inv_div]

/-- Proposition 1, eq. (11). The conjectured recovery from `Π_l` fails iff
`Π_l < Π_h − (Π_h − Π*)((ωPu − ωPq)/((δ+λ')cm − ωPq − ω'))^{λ'/(δ+λ')}`. -/
theorem prop1 {A lam' ω Pq Pu ω' cm piL piH piStar : ℝ} (hA : 0 < A) (hl : 0 < lam')
    (hd : ω * Pq < ω * Pu) (hk : ω * Pq + ω' < A * cm) (hS : piStar < piH) (hL : piL < piH) :
    Rsw A ω' (ω * Pq) (ω * Pu) (Tup lam' piH piStar piL) < cm ↔
      piL < piH - (piH - piStar) * ((ω * Pu - ω * Pq) / (A * cm - ω * Pq - ω')) ^ (lam' / A) := by
  rw [← not_le, opt_feasible_iff hA hl hd hk hS hL, not_le]
  unfold piO; rw [vS_rpow_neg hd hk]
  constructor <;> intro h <;> linarith

/-- Proposition 2. `Π_l < π°` is the condition (11) of Proposition 1. -/
theorem prop2 {A lam' ω Pq Pu ω' cm piL piH piStar : ℝ} (hd : ω * Pq < ω * Pu)
    (hk : ω * Pq + ω' < A * cm) :
    piL < piO A lam' piH piStar (vS A ω Pq Pu ω' cm) ↔
      piL < piH - (piH - piStar) * ((ω * Pu - ω * Pq) / (A * cm - ω * Pq - ω')) ^ (lam' / A) := by
  unfold piO; rw [vS_rpow_neg hd hk]
  constructor <;> intro h <;> linarith

/-- Corollary 1, eq. (12), with `δ + λ'` in place of `A`. Eq. (11) holds iff
`δ/λ' > [ln(ωPu − ωPq) − ln((δ+λ')cm − ωPq − ω')]/[ln(Π_h − Π_l) − ln(Π_h − Π*)] − 1`. -/
theorem cor1 {δ lam' ω Pq Pu ω' cm piL piH piStar : ℝ} (hA : 0 < δ + lam') (hl : 0 < lam')
    (hd : ω * Pq < ω * Pu) (hk : ω * Pq + ω' < (δ + lam') * cm) (hLS : piL < piStar)
    (hS : piStar < piH) :
    piL < piH - (piH - piStar) * ((ω * Pu - ω * Pq) / ((δ + lam') * cm - ω * Pq - ω')) ^ (lam' / (δ + lam'))
      ↔ δ / lam' > (log (ω * Pu - ω * Pq) - log ((δ + lam') * cm - ω * Pq - ω'))
          / (log (piH - piL) - log (piH - piStar)) - 1 := by
  have hd' : 0 < ω * Pu - ω * Pq := by linarith
  have hk' : 0 < (δ + lam') * cm - ω * Pq - ω' := by linarith
  have p1 : 0 < piH - piL := by linarith
  have p2 : 0 < piH - piStar := by linarith
  have hL : 0 < log (piH - piL) - log (piH - piStar) := by
    rw [← log_div p1.ne' p2.ne']; exact log_pos ((one_lt_div p2).2 (by linarith))
  rw [← log_div hd'.ne' hk'.ne', ← log_div p1.ne' p2.ne'] at *
  have hpos : 0 < (ω * Pu - ω * Pq) / ((δ + lam') * cm - ω * Pq - ω') := div_pos hd' hk'
  rw [rpow_def_of_pos hpos]
  have e1 : piL < piH - (piH - piStar) * exp (log ((ω * Pu - ω * Pq) / ((δ + lam') * cm - ω * Pq - ω'))
      * (lam' / (δ + lam'))) ↔ exp (log ((ω * Pu - ω * Pq) / ((δ + lam') * cm - ω * Pq - ω'))
      * (lam' / (δ + lam'))) < (piH - piL) / (piH - piStar) := by
    rw [lt_div_iff₀ p2]; constructor <;> intro h <;> nlinarith
  rw [e1, ← lt_log_iff_exp_lt (div_pos p1 p2), gt_iff_lt, sub_lt_iff_lt_add, div_lt_iff₀ hL]
  have e2 : (δ / lam' + 1) = (δ + lam') / lam' := by field_simp
  rw [e2]
  constructor <;> intro h
  · have := mul_lt_mul_of_pos_right h (div_pos hA hl)
    calc log ((ω * Pu - ω * Pq) / ((δ + lam') * cm - ω * Pq - ω'))
        = log ((ω * Pu - ω * Pq) / ((δ + lam') * cm - ω * Pq - ω')) * (lam' / (δ + lam'))
          * ((δ + lam') / lam') := by field_simp
      _ < _ := by nlinarith
  · have := mul_lt_mul_of_pos_right h (div_pos hl hA)
    calc log ((ω * Pu - ω * Pq) / ((δ + lam') * cm - ω * Pq - ω')) * (lam' / (δ + lam'))
        < (δ + lam') / lam' * log ((piH - piL) / (piH - piStar)) * (lam' / (δ + lam')) := this
      _ = log ((piH - piL) / (piH - piStar)) := by field_simp

/-- Definition 1 and Lemma 3 on `[Π_l, Π_h]`. The optimistic path is feasible
from `r0` if `r0 ≥ Π*` (the benefit of the doubt is already given and expected
to stay) or if the newborn invests on the optimistic conjecture. -/
def optF (A lam' ω Pq Pu ω' cm piH piStar r0 : ℝ) : Prop :=
  piStar ≤ r0 ∨ cm ≤ Rsw A ω' (ω * Pq) (ω * Pu) (Tup lam' piH piStar r0)

/-- The pessimistic path is feasible from `r0` if `r0 < Π*` or if the newborn
stops investing on the pessimistic conjecture. -/
def pesF (A lam' ω Pq Pu ω' cm piL piStar r0 : ℝ) : Prop :=
  r0 < piStar ∨ Rsw A ω' (ω * Pu) (ω * Pq) (Tdown lam' piL piStar r0) < cm

/-- Proposition 3 on `[Π_l, Π_h]` (a special case). A reputation there is in the
overlap, both paths feasible, iff `π° ≤ r0 < πᵖ`. -/
theorem prop3 {A lam' ω Pq Pu ω' cm piL piH piStar r0 : ℝ} (hA : 0 < A) (hl : 0 < lam')
    (hd : ω * Pq < ω * Pu) (hk : ω * Pq + ω' < A * cm) (hk2 : A * cm < ω * Pu + ω')
    (hLS : piL < piStar) (hS : piStar < piH) (_h1 : piL ≤ r0) (h2 : r0 < piH) :
    (optF A lam' ω Pq Pu ω' cm piH piStar r0 ∧ pesF A lam' ω Pq Pu ω' cm piL piStar r0) ↔
      (piO A lam' piH piStar (vS A ω Pq Pu ω' cm) ≤ r0 ∧ r0 < piP A lam' piL piStar (vS A ω Pq Pu ω' cm)) := by
  obtain ⟨hv0, hv1⟩ := vS_mem hk hk2
  have hO := piO_lt_piStar hA hl hv0 hv1 hS
  have hP := piStar_lt_piP hA hl hv0 hv1 hLS
  unfold optF pesF
  by_cases hr0 : piStar ≤ r0
  · have hr0L : piL < r0 := by linarith
    rw [pes_feasible_iff hA hl hd hk2 hLS hr0L]
    constructor
    · rintro ⟨_, h | h⟩
      · exact absurd h (not_lt.2 hr0)
      · exact ⟨by linarith, h⟩
    · rintro ⟨_, h⟩; exact ⟨Or.inl hr0, Or.inr h⟩
  · push_neg at hr0
    rw [opt_feasible_iff hA hl hd hk hS h2]
    constructor
    · rintro ⟨h | h, _⟩
      · exact absurd h (not_le.2 hr0)
      · exact ⟨h, by linarith⟩
    · rintro ⟨h, _⟩; exact ⟨Or.inr h, Or.inl hr0⟩

/-- Theorem 2 with Definition 2. `Π_l` lies in the trap, where the optimistic
path is not feasible, iff `(Π_h − Π*)/(Π_h − Π_l) < v*^{λ'/(δ+λ')}`. -/
theorem theorem2 {A lam' ω Pq Pu ω' cm piL piH piStar : ℝ} (hA : 0 < A) (hl : 0 < lam')
    (hd : ω * Pq < ω * Pu) (hk : ω * Pq + ω' < A * cm) (hLS : piL < piStar) (hS : piStar < piH) :
    ¬ optF A lam' ω Pq Pu ω' cm piH piStar piL ↔
      (piH - piStar) / (piH - piL) < (vS A ω Pq Pu ω' cm) ^ (lam' / A) := by
  have hv : 0 < vS A ω Pq Pu ω' cm := div_pos (by linarith) (by linarith)
  unfold optF
  rw [not_or, opt_feasible_iff hA hl hd hk hS (by linarith), not_le, not_le,
    ← trap_iff (a := lam' / A) hv (by linarith : piL < piH)]
  unfold piO
  exact ⟨fun h => h.2, fun h => ⟨hLS, h⟩⟩

/-- Control: without the trap condition, `Π_l` can recover. If
`(Π_h − Π*)/(Π_h − Π_l) ≥ v*^{λ'/(δ+λ')}`, the optimistic path from `Π_l` is
feasible. -/
theorem control_no_trap {A lam' ω Pq Pu ω' cm piL piH piStar : ℝ} (hA : 0 < A) (hl : 0 < lam')
    (hd : ω * Pq < ω * Pu) (hk : ω * Pq + ω' < A * cm) (hLS : piL < piStar) (hS : piStar < piH)
    (h : (vS A ω Pq Pu ω' cm) ^ (lam' / A) ≤ (piH - piStar) / (piH - piL)) :
    optF A lam' ω Pq Pu ω' cm piH piStar piL := by
  by_contra hc
  rw [theorem2 hA hl hd hk hLS hS] at hc
  linarith

/-! ### The comparative statics on p. 15 -/

/-- With `v*` held fixed, `π°` rises with `δ` (here with `A = δ + λ'`). -/
theorem piO_mono_fixed_v {A1 A2 lam' piH piStar v : ℝ} (hA1 : 0 < A1) (h12 : A1 < A2)
    (hl : 0 < lam') (hv0 : 0 < v) (hv1 : v < 1) (hS : piStar < piH) :
    piO A1 lam' piH piStar v < piO A2 lam' piH piStar v := by
  unfold piO
  have ha : lam' / A2 < lam' / A1 := div_lt_div_of_pos_left hl hA1 h12
  have := rpow_lt_rpow_of_exponent_gt hv0 hv1 (by linarith : -(lam' / A1) < -(lam' / A2))
  nlinarith

/-- With `v*` held fixed, `πᵖ` falls with `δ`, the paper's claim on p. 15. -/
theorem piP_anti_fixed_v {A1 A2 lam' piL piStar v : ℝ} (hA1 : 0 < A1) (h12 : A1 < A2)
    (hl : 0 < lam') (hv0 : 0 < v) (hv1 : v < 1) (hS : piL < piStar) :
    piP A2 lam' piL piStar v < piP A1 lam' piL piStar v := by
  unfold piP
  have ha : lam' / A2 < lam' / A1 := div_lt_div_of_pos_left hl hA1 h12
  have := rpow_lt_rpow_of_exponent_gt (by linarith : (0 : ℝ) < 1 - v) (by linarith)
    (by linarith : -(lam' / A1) < -(lam' / A2))
  nlinarith

/-- `v*` as a function of `δ`, with `λ' = λ + η` and `ω' = ηω/(δ + λ)`. -/
noncomputable def vSd (lam η ω Pq Pu cm δ : ℝ) : ℝ :=
  vS (δ + (lam + η)) ω Pq Pu (η * ω / (δ + lam)) cm

/-- `v*` rises with `δ`, through `(δ + λ')cm` and through `ω'`. -/
theorem vSd_strictMono {lam η ω Pq Pu cm δ1 δ2 : ℝ} (hB : 0 < δ1 + lam) (h12 : δ1 < δ2)
    (hη : 0 ≤ η) (hω : 0 < ω) (hd : ω * Pq < ω * Pu) (hc : 0 < cm) :
    vSd lam η ω Pq Pu cm δ1 < vSd lam η ω Pq Pu cm δ2 := by
  unfold vSd vS
  have hd' : 0 < ω * Pu - ω * Pq := by linarith
  rw [div_lt_div_iff_of_pos_right hd']
  have hB2 : 0 < δ2 + lam := by linarith
  have : η * ω / (δ2 + lam) ≤ η * ω / (δ1 + lam) :=
    div_le_div_of_nonneg_left (mul_nonneg hη hω.le) hB (by linarith)
  nlinarith

/-- `π°` rises with `δ` when `v*` moves with `δ` as defined, the paper's claim
`∂π°/∂δ > 0` on p. 15 in difference form. -/
theorem piO_strictMono_delta {lam η ω Pq Pu cm δ1 δ2 piH piStar : ℝ} (hB : 0 < δ1 + lam)
    (h12 : δ1 < δ2) (hη : 0 ≤ η) (hl : 0 < lam + η) (hω : 0 < ω) (hd : ω * Pq < ω * Pu)
    (hc : 0 < cm) (hv0 : 0 < vSd lam η ω Pq Pu cm δ1) (hv1 : vSd lam η ω Pq Pu cm δ2 < 1)
    (hS : piStar < piH) :
    piO (δ1 + (lam + η)) (lam + η) piH piStar (vSd lam η ω Pq Pu cm δ1)
      < piO (δ2 + (lam + η)) (lam + η) piH piStar (vSd lam η ω Pq Pu cm δ2) := by
  have hv := vSd_strictMono hB h12 hη hω hd hc
  have hA1 : 0 < δ1 + (lam + η) := by linarith
  have ha : (lam + η) / (δ2 + (lam + η)) < (lam + η) / (δ1 + (lam + η)) :=
    div_lt_div_of_pos_left hl hA1 (by linarith)
  have h1 := rpow_lt_rpow_of_exponent_neg hv0 hv (by have := div_pos hl hA1; linarith :
    -((lam + η) / (δ1 + (lam + η))) < 0)
  have h2 := rpow_lt_rpow_of_exponent_gt (hv0.trans hv) hv1
    (by linarith : -((lam + η) / (δ1 + (lam + η))) < -((lam + η) / (δ2 + (lam + η))))
  unfold piO
  nlinarith

/-- KLB-D10. The paper's claim `∂πᵖ/∂δ < 0` on p. 15 fails once `v*` moves with
`δ`. With `λ = η = 1/2`, `ω = 2`, `Pq = 1/10`, `Pu = 3/5`, `cm = 17/10`, both
`δ = 1/10` and `δ = 1/5` satisfy Lemma 1's conditions, and `πᵖ` is higher at
the larger `δ`. -/
theorem piP_rises_example :
    let lam : ℝ := 1 / 2; let η : ℝ := 1 / 2; let ω : ℝ := 2; let Pq : ℝ := 1 / 10
    let Pu : ℝ := 3 / 5; let cm : ℝ := 17 / 10; let δ1 : ℝ := 1 / 10; let δ2 : ℝ := 1 / 5
    (ω * Pq + η * ω / (δ1 + lam)) / (δ1 + (lam + η)) < cm ∧
      cm < (ω * Pu + η * ω / (δ1 + lam)) / (δ1 + (lam + η)) ∧
      (ω * Pq + η * ω / (δ2 + lam)) / (δ2 + (lam + η)) < cm ∧
      cm < (ω * Pu + η * ω / (δ2 + lam)) / (δ2 + (lam + η)) ∧
      piP (δ1 + (lam + η)) (lam + η) (1 / 10) (1 / 2) (vSd lam η ω Pq Pu cm δ1)
        < piP (δ2 + (lam + η)) (lam + η) (1 / 10) (1 / 2) (vSd lam η ω Pq Pu cm δ2) := by
  intro lam η ω Pq Pu cm δ1 δ2
  have v1 : vSd lam η ω Pq Pu cm δ1 = 1 / 300 := by
    simp only [vSd, vS, lam, η, ω, Pq, Pu, cm, δ1]; norm_num
  have v2 : vSd lam η ω Pq Pu cm δ2 = 72 / 175 := by
    simp only [vSd, vS, lam, η, ω, Pq, Pu, cm, δ2]; norm_num
  refine ⟨by norm_num [lam, η, ω, Pq, Pu, cm, δ1], by norm_num [lam, η, ω, Pq, Pu, cm, δ1],
    by norm_num [lam, η, ω, Pq, Pu, cm, δ2], by norm_num [lam, η, ω, Pq, Pu, cm, δ2], ?_⟩
  rw [v1, v2]
  unfold piP
  have e1 : (lam + η) / (δ1 + (lam + η)) = 10 / 11 := by norm_num [lam, η, δ1]
  have e2 : (lam + η) / (δ2 + (lam + η)) = 5 / 6 := by norm_num [lam, η, δ2]
  rw [e1, e2]
  have key : (299 / 300 : ℝ) ^ (-(10 / 11 : ℝ)) < (103 / 175 : ℝ) ^ (-(5 / 6 : ℝ)) := by
    rw [rpow_def_of_pos (by norm_num), rpow_def_of_pos (by norm_num), exp_lt_exp]
    have hp : (300 / 299 : ℝ) ^ 60 < (175 / 103 : ℝ) ^ 55 := by norm_num
    have hl := log_lt_log (by positivity) hp
    rw [log_pow, log_pow] at hl
    have a1 : log (299 / 300 : ℝ) = -log (300 / 299) := by
      rw [← log_inv]; norm_num
    have a2 : log (103 / 175 : ℝ) = -log (175 / 103) := by
      rw [← log_inv]; norm_num
    rw [a1, a2]
    push_cast at hl
    nlinarith
  norm_num at key ⊢
  nlinarith

/-- KLB-D10, in general. Once `v*` moves with `δ`, `πᵖ` rises with `δ`
whenever `ωPq > 0`, Lemma 1's conditions hold at both rates and `cm > 0`. The
paper's `∂πᵖ/∂δ < 0` holds only with `v*` fixed (`piP_anti_fixed_v`). -/
theorem piP_strictMono_delta {lam η ω Pq Pu cm δ1 δ2 piL piStar : ℝ} (hB : 0 < δ1 + lam)
    (h12 : δ1 < δ2) (hη : 0 ≤ η) (hl : 0 < lam + η) (hω : 0 < ω) (hd : ω * Pq < ω * Pu)
    (hc : 0 < cm) (hq : 0 < ω * Pq) (hv0 : 0 < vSd lam η ω Pq Pu cm δ1)
    (hv1 : vSd lam η ω Pq Pu cm δ2 < 1) (hS : piL < piStar) :
    piP (δ1 + (lam + η)) (lam + η) piL piStar (vSd lam η ω Pq Pu cm δ1)
      < piP (δ2 + (lam + η)) (lam + η) piL piStar (vSd lam η ω Pq Pu cm δ2) := by
  have hv12 := vSd_strictMono hB h12 hη hω hd hc
  set v1 := vSd lam η ω Pq Pu cm δ1 with ev1
  set v2 := vSd lam η ω Pq Pu cm δ2 with ev2
  set A1 := δ1 + (lam + η) with eA1
  set A2 := δ2 + (lam + η) with eA2
  set d := ω * Pu - ω * Pq with ed
  have hd' : 0 < d := by rw [ed]; linarith
  have hA1 : 0 < A1 := by rw [eA1]; linarith
  have hA12 : A1 < A2 := by rw [eA1, eA2]; linarith
  have hB2 : 0 < δ2 + lam := by linarith
  have hw1 : 0 < 1 - v1 := by linarith
  have hw2 : 0 < 1 - v2 := by linarith
  have hw1' : 1 - v1 < 1 := by linarith
  have hw21 : 1 - v2 < 1 - v1 := by linarith
  have hom : η * ω / (δ2 + lam) ≤ η * ω / (δ1 + lam) :=
    div_le_div_of_nonneg_left (mul_nonneg hη hω.le) hB (by linarith)
  have hom0 : 0 ≤ η * ω / (δ1 + lam) := div_nonneg (mul_nonneg hη hω.le) hB.le
  -- F3 and F4, from the definition of `v*`
  have F3 : (A2 - A1) * cm ≤ d * (v2 - v1) := by
    have e1 : d * v1 = A1 * cm - ω * Pq - η * ω / (δ1 + lam) := by
      rw [ev1]; unfold vSd vS; rw [← ed, ← eA1]; field_simp
    have e2 : d * v2 = A2 * cm - ω * Pq - η * ω / (δ2 + lam) := by
      rw [ev2]; unfold vSd vS; rw [← ed, ← eA2]; field_simp
    nlinarith
  have F4 : d * v1 < A1 * cm := by
    have e1 : d * v1 = A1 * cm - ω * Pq - η * ω / (δ1 + lam) := by
      rw [ev1]; unfold vSd vS; rw [← ed, ← eA1]; field_simp
    linarith
  -- F1 and F2, from `log x ≤ x − 1`
  set X := log (1 - v1)⁻¹ with eX
  set Y := log (1 - v2)⁻¹ with eY
  have hX : X = -log (1 - v1) := by rw [eX, log_inv]
  have hY : Y = -log (1 - v2) := by rw [eY, log_inv]
  have F1 : v2 - v1 ≤ (1 - v1) * (Y - X) := by
    have h := log_le_sub_one_of_pos (div_pos hw2 hw1)
    rw [log_div hw2.ne' hw1.ne'] at h
    have h' : (1 - v1) * (log (1 - v2) - log (1 - v1)) ≤ (1 - v1) * ((1 - v2) / (1 - v1) - 1) :=
      mul_le_mul_of_nonneg_left h hw1.le
    have e : (1 - v1) * ((1 - v2) / (1 - v1) - 1) = v1 - v2 := by field_simp; ring
    have e2 : (1 - v1) * (Y - X) = -((1 - v1) * (log (1 - v2) - log (1 - v1))) := by
      rw [hX, hY]; ring
    linarith
  have F2 : (1 - v1) * X ≤ v1 := by
    have h := log_le_sub_one_of_pos (inv_pos.2 hw1)
    have : (1 - v1) * log (1 - v1)⁻¹ ≤ (1 - v1) * ((1 - v1)⁻¹ - 1) :=
      mul_le_mul_of_nonneg_left h hw1.le
    rw [mul_sub, mul_one, mul_inv_cancel₀ hw1.ne'] at this
    rw [eX]; linarith
  -- the comparison `A2 X < A1 Y`
  have key : A2 * X < A1 * Y := by
    have g1 : d * A1 * (v2 - v1) ≤ d * A1 * ((1 - v1) * (Y - X)) :=
      mul_le_mul_of_nonneg_left F1 (by positivity)
    have g2 : d * (A2 - A1) * ((1 - v1) * X) ≤ d * (A2 - A1) * v1 :=
      mul_le_mul_of_nonneg_left F2 (mul_nonneg hd'.le (by linarith))
    have g3 : A1 * ((A2 - A1) * cm) ≤ A1 * (d * (v2 - v1)) := mul_le_mul_of_nonneg_left F3 hA1.le
    have g4 : (A2 - A1) * (d * v1) < (A2 - A1) * (A1 * cm) :=
      mul_lt_mul_of_pos_left F4 (by linarith)
    have hpos : 0 < d * (1 - v1) := mul_pos hd' hw1
    have e : d * (1 - v1) * (A1 * Y - A2 * X)
        = d * A1 * ((1 - v1) * (Y - X)) - d * (A2 - A1) * ((1 - v1) * X) := by ring
    have : 0 < d * (1 - v1) * (A1 * Y - A2 * X) := by
      rw [e]; ring_nf at g1 g2 g3 g4 ⊢; linarith
    have := (mul_pos_iff_of_pos_left hpos).1 this
    linarith
  unfold piP
  rw [rpow_neg_eq_exp hw1, rpow_neg_eq_exp hw2, ← eX, ← eY]
  have hexp : (lam + η) / A1 * X < (lam + η) / A2 * Y := by
    rw [div_mul_eq_mul_div, div_mul_eq_mul_div, div_lt_div_iff₀ hA1 (by linarith)]
    have := mul_lt_mul_of_pos_left key hl
    ring_nf at this ⊢; linarith
  have := exp_lt_exp.2 hexp
  have hSL : 0 < piStar - piL := by linarith
  have := mul_lt_mul_of_pos_left this hSL
  linarith

/-! ### Corollary 3, Pareto dominance -/

/-- Corollary 3, the medium-cost worker. Under Lemma 1's upper condition, his
lifetime payoff in `Q_h`, `ω/(δ + λ) − cm`, is positive, while it is zero in
`Q_l`. -/
theorem cor3_medium {ω Pu δ lam η cm : ℝ} (hω : 0 < ω) (hB : 0 < δ + lam) (hη : 0 ≤ η)
    (hPu : Pu ≤ 1) (hhi : cm < (ω * Pu + η * ω / (δ + lam)) / (δ + (lam + η))) :
    0 < ω / (δ + lam) - cm := by
  have hBη : 0 < δ + (lam + η) := by linarith
  have e : ω / (δ + lam) - (ω * Pu + η * ω / (δ + lam)) / (δ + (lam + η))
      = ω * (1 - Pu) / (δ + (lam + η)) := by
    field_simp; ring
  have : 0 ≤ ω * (1 - Pu) / (δ + (lam + η)) := div_nonneg (by nlinarith) hBη.le
  linarith

/-- Corollary 3, the low- and high-cost workers. With the benefit of the doubt
the qualified worker's flow wage rises from `ωPq` to `ω` and the unqualified
worker's from `0` to `ω(1 − Pu)`. -/
theorem cor3_workers {ω Pq Pu : ℝ} (hω : 0 < ω) (hq : Pq < 1) (hu : Pu < 1) :
    wageQ ω Pq 0 < wageQ ω Pq 1 ∧ wageU ω Pu 0 < wageU ω Pu 1 := by
  unfold wageQ wageU
  constructor <;> nlinarith

/-- Corollary 3, the employers. Profit per temporary worker is
`Π_h xq − (1 − Π_h)(1 − Pu)xu` in `Q_h` and `Π_l Pq xq` in `Q_l`, and per
permanent worker `Π_h xq` and `Π_l xq`. Both are higher in `Q_h`. -/
theorem cor3_employers {ρ xu Pq Pu piL piH : ℝ} (hρ : 0 < ρ) (hxu : 0 < xu) (hq : Pq < 1)
    (hu : Pu < 1) (hPq : 0 < Pq) (hH : piS ρ Pq Pu ≤ piH) (hLH : piL < piH) :
    piL * Pq * (ρ * xu) < piH * (ρ * xu) - (1 - piH) * (1 - Pu) * xu ∧
      piL * (ρ * xu) < piH * (ρ * xu) := by
  have hE : 0 < ρ * (1 - Pq) + 1 - Pu := by nlinarith [mul_pos hρ (by linarith : (0:ℝ) < 1 - Pq)]
  unfold piS at hH
  rw [div_le_iff₀ hE] at hH
  have h1 : (1 - piH) * (1 - Pu) ≤ piH * ρ * (1 - Pq) := by nlinarith
  have h2 : (1 - piH) * (1 - Pu) * xu ≤ piH * ρ * (1 - Pq) * xu := mul_le_mul_of_nonneg_right h1 hxu.le
  have h3 : piL * Pq * (ρ * xu) < piH * Pq * (ρ * xu) :=
    mul_lt_mul_of_pos_right (mul_lt_mul_of_pos_right hLH hPq) (mul_pos hρ hxu)
  constructor
  · nlinarith
  · exact mul_lt_mul_of_pos_right hLH (mul_pos hρ hxu)

/-- Control: without `Π* ≤ Π_h` the employers' ranking of Corollary 3 can fail.
With `ρ = 1/10`, `xu = 1`, `Pq = Pu = 1/2`, `Π_h = 1/2` and `Π_l = 1/4`, the
group at `Π_h` lies below `Π* = 10/11`, and the temporary-market profit at
`Π_h` with the benefit of the doubt is below `Π_l Pq xq`. -/
theorem control_cor3_needs_threshold :
    (1 / 2 : ℝ) < piS (1 / 10) (1 / 2) (1 / 2) ∧
      (1 / 2 : ℝ) * (1 / 10 * 1) - (1 - 1 / 2) * (1 - 1 / 2) * 1 < 1 / 4 * (1 / 2) * (1 / 10 * 1) := by
  unfold piS; norm_num

/-! ### Section 5.2.1, the subsidy, eq. (20) -/

/-- Eq. (20). Along the recovery from `Π_l`, the return of eq. (6) at the date
of eq. (10) is `c̃m = [ωPq + ω' + (ωPu − ωPq)((Π_h − Π*)/(Π_h − Π_l))^{(δ+λ')/λ'}]/(δ+λ')`. -/
theorem eq20 {A lam' ω Pq Pu ω' piL piH piStar : ℝ} (hl : 0 < lam') (hLS : piL < piStar)
    (hS : piStar < piH) :
    Rsw A ω' (ω * Pq) (ω * Pu) (Tup lam' piH piStar piL)
      = 1 / A * (ω * Pq + ω' + (ω * Pu - ω * Pq) * ((piH - piStar) / (piH - piL)) ^ (A / lam')) := by
  have p1 : 0 < piH - piL := by linarith
  have p2 : 0 < piH - piStar := by linarith
  unfold Rsw Tup
  have e : exp (-(A * (1 / lam' * log ((piH - piL) / (piH - piStar)))))
      = ((piH - piStar) / (piH - piL)) ^ (A / lam') := by
    rw [rpow_def_of_pos (div_pos p2 p1), show (piH - piStar) / (piH - piL)
      = ((piH - piL) / (piH - piStar))⁻¹ by rw [inv_div], log_inv]
    congr 1; field_simp
  rw [e]

/-- Control for Lemma 4: at `T = 0` the standard in force before `T` has no
effect on the return. -/
theorem control_lemma4_T_zero (A ω' b ωPu : ℝ) (hA : A ≠ 0) :
    Rsw A ω' b ωPu 0 = (ωPu + ω') / A := by
  unfold Rsw; simp; field_simp; ring

/-! ### Section 5.2.2, the hiring standard, eqs. (21) to (25) and Proposition 5 -/

/-- Eq. (22), `β̂ = ((δ+λ')cm − ω' − ωPu e^{−(δ+λ')T})/(1 − e^{−(δ+λ')T})`. -/
noncomputable def betaHat (A ω' ωPu cm T : ℝ) : ℝ :=
  (A * cm - ω' - ωPu * exp (-(A * T))) / (1 - exp (-(A * T)))

/-- Lemma 4 with eq. (21). With the standard's flow return `b` before `T > 0`
and `ωPu` after, the newborn invests iff `b ≥ β̂`. -/
theorem lemma4 {A ω' ωPu cm T b : ℝ} (hA : 0 < A) (hT : 0 < T) :
    cm ≤ Rsw A ω' b ωPu T ↔ betaHat A ω' ωPu cm T ≤ b := by
  have he : exp (-(A * T)) < 1 := by rw [exp_lt_one_iff]; nlinarith
  have he' : 0 < 1 - exp (-(A * T)) := by linarith
  unfold Rsw betaHat
  rw [one_div, ← div_eq_inv_mul, le_div_iff₀ hA, div_le_iff₀ he']
  constructor <;> intro h <;> nlinarith

/-- `β̂ < (δ+λ')cm − ω'` under Lemma 1's upper condition, p. 21. -/
theorem betaHat_lt {A ω' ωPu cm T : ℝ} (hA : 0 < A) (hT : 0 < T) (hk2 : A * cm - ω' < ωPu) :
    betaHat A ω' ωPu cm T < A * cm - ω' := by
  have he : exp (-(A * T)) < 1 := by rw [exp_lt_one_iff]; nlinarith
  have he' : 0 < 1 - exp (-(A * T)) := by linarith
  have he0 := exp_pos (-(A * T))
  unfold betaHat
  rw [div_lt_iff₀ he']
  nlinarith

/-- `ωPq < β̂` iff the group is trapped at the switch date `T`, p. 21. -/
theorem betaHat_gt_iff {A ω' ω Pq Pu cm T : ℝ} (hA : 0 < A) (hT : 0 < T) :
    ω * Pq < betaHat A ω' (ω * Pu) cm T ↔ Rsw A ω' (ω * Pq) (ω * Pu) T < cm := by
  rw [← not_le, ← not_le, lemma4 hA hT]

/-- `Pq` and `Pu` from the uniform signals of Section 3.2. -/
noncomputable def PqT (θq θu θb : ℝ) : ℝ := (θb - θu) / (θb - θq)
noncomputable def PuT (θq θu : ℝ) : ℝ := θq / θu

/-- Section 5.2.2. `Pq < Pu` iff `θq + θu > θ̄`. -/
theorem Pq_lt_Pu_iff {θq θu θb : ℝ} (h0 : 0 < θq) (h1 : θq < θu) (h2 : θu < θb) :
    PqT θq θu θb < PuT θq θu ↔ θb < θq + θu := by
  unfold PqT PuT
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]
  constructor <;> intro h <;> nlinarith

/-- The flow return to investment under a hiring standard `θ̃`. Below `θq` every
qualified worker passes and an unqualified one passes with probability
`(θu − θ̃)/θu`. Above `θq` the qualified pass with probability
`(θ̄ − θ̃)/(θ̄ − θq)`. -/
noncomputable def betaStd (ω θq θu θb θt : ℝ) : ℝ :=
  if θt ≤ θq then ω - (θu - θt) / θu * ω else (θb - θt) / (θb - θq) * ω - (θu - θt) / θu * ω

/-- Eq. (23). -/
theorem eq23 {ω θq θu θb θt : ℝ} (_h0 : 0 < θq) (h1 : θq < θt) (h2 : θq < θb) (hu : 0 < θu) :
    betaStd ω θq θu θb θt = (θq * θu - (θq + θu - θb) * θt) / ((θb - θq) * θu) * ω := by
  unfold betaStd; rw [if_neg (not_le.2 h1)]
  have : θb - θq ≠ 0 := by linarith
  field_simp; ring

/-- Below `θq` the return is `θ̃ω/θu`, p. 22. -/
theorem eq_below {ω θq θu θb θt : ℝ} (hu : 0 < θu) (h : θt ≤ θq) :
    betaStd ω θq θu θb θt = θt / θu * ω := by
  unfold betaStd; rw [if_pos h]; field_simp; ring

/-- On `(θq, θu)` the return falls with the standard when `θq + θu > θ̄`. -/
theorem eq23_anti {ω θq θu θb s t : ℝ} (hω : 0 < ω) (h0 : 0 < θq) (hs : θq < s) (hst : s < t)
    (h2 : θq < θb) (hu : 0 < θu) (hsum : θb < θq + θu) :
    betaStd ω θq θu θb t < betaStd ω θq θu θb s := by
  rw [eq23 h0 (hs.trans hst) h2 hu, eq23 h0 hs h2 hu]
  have hD : 0 < (θb - θq) * θu := mul_pos (by linarith) hu
  apply mul_lt_mul_of_pos_right _ hω
  rw [div_lt_div_iff_of_pos_right hD]
  nlinarith

/-- Control for eq. (23) and Proposition 5: drop `θq + θu > θ̄`. Then the
return rises with the standard on `(θq, θu)`, the opposite of `eq23_anti`. -/
theorem control_eq23_rises {ω θq θu θb s t : ℝ} (hω : 0 < ω) (h0 : 0 < θq) (hs : θq < s)
    (hst : s < t) (h2 : θq < θb) (hu : 0 < θu) (hsum : θq + θu < θb) :
    betaStd ω θq θu θb s < betaStd ω θq θu θb t := by
  rw [eq23 h0 (hs.trans hst) h2 hu, eq23 h0 hs h2 hu]
  have hD : 0 < (θb - θq) * θu := mul_pos (by linarith) hu
  apply mul_lt_mul_of_pos_right _ hω
  rw [div_lt_div_iff_of_pos_right hD]
  nlinarith

/-- Eq. (24). On `(θq, θu)`, `β(θ̃) ≥ β̂` iff `θ̃ ≤ θ̃'`. -/
theorem eq24 {ω θq θu θb θt bh : ℝ} (hω : 0 < ω) (h0 : 0 < θq) (h1 : θq < θt) (h2 : θq < θb)
    (hu : 0 < θu) (hsum : θb < θq + θu) :
    bh ≤ betaStd ω θq θu θb θt ↔
      θt ≤ (θq * θu * ω - (θb - θq) * θu * bh) / ((θq + θu - θb) * ω) := by
  rw [eq23 h0 h1 h2 hu]
  have hD : 0 < (θb - θq) * θu := mul_pos (by linarith) hu
  have hE : 0 < (θq + θu - θb) * ω := mul_pos (by linarith) hω
  rw [div_mul_eq_mul_div, le_div_iff₀ hD, le_div_iff₀ hE]
  constructor <;> intro h <;> nlinarith

/-- Eq. (25). Below `θq`, `β(θ̃) < β̂` iff `θ̃ < θuβ̂/ω`. -/
theorem eq25 {ω θq θu θb θt bh : ℝ} (hω : 0 < ω) (hu : 0 < θu) (h : θt ≤ θq) :
    betaStd ω θq θu θb θt < bh ↔ θt < θu * bh / ω := by
  rw [eq_below hu h, div_mul_eq_mul_div, div_lt_iff₀ hu, lt_div_iff₀ hω]
  constructor <;> intro h <;> linarith

/-- Proposition 5. With `ωPq < β̂ < ωPu` and `θq + θu > θ̄`, a standard in
`[θ̃'', θ̃']` gives a return of at least `β̂`, and a standard below `θ̃''` gives
less, the patronizing outcome. -/
theorem prop5 {ω θq θu θb θt bh : ℝ} (hω : 0 < ω) (h0 : 0 < θq) (h1 : θq < θu) (h2 : θu < θb)
    (hsum : θb < θq + θu) (hhi : bh < ω * PuT θq θu) :
    ((θu * bh / ω ≤ θt ∧ θt ≤ (θq * θu * ω - (θb - θq) * θu * bh) / ((θq + θu - θb) * ω)) →
        bh ≤ betaStd ω θq θu θb θt) ∧
      (θt < θu * bh / ω → bh > betaStd ω θq θu θb θt) := by
  have hu : 0 < θu := by linarith
  have h2' : θq < θb := by linarith
  have hlow : θu * bh / ω < θq := by
    unfold PuT at hhi
    rw [div_lt_iff₀ hω]
    have : bh * θu < ω * (θq / θu) * θu := mul_lt_mul_of_pos_right hhi hu
    rw [mul_assoc, div_mul_cancel₀ _ hu.ne'] at this
    linarith
  constructor
  · rintro ⟨ha, hb⟩
    by_cases hc : θt ≤ θq
    · have := (eq25 (θb := θb) (bh := bh) hω hu hc).not
      push_neg at this
      exact this.2 ha
    · push_neg at hc
      exact (eq24 hω h0 hc h2' hu hsum).2 hb
  · intro h
    exact (eq25 hω hu (by linarith)).2 h

/-- The quota on p. 22. Equal hiring rates for `B` at reputation `Π_l` and `W`
at `Π_h` with standard `θq` need the standard `θ̃* = θq(1 − Π_h)/(1 − Π_l)`
for `B`, which lies in `(0, θq)`. -/
theorem quota {θq θu piL piH : ℝ} (h0 : 0 < θq) (hu : 0 < θu) (hLH : piL < piH) (hH : piH < 1) :
    let θs := (1 - piH) / (1 - piL) * θq
    piL + (1 - piL) * (1 - θs / θu) = piH + (1 - piH) * (1 - θq / θu) ∧ 0 < θs ∧ θs < θq := by
  intro θs
  have hL : 0 < 1 - piL := by linarith
  have hH' : 0 < 1 - piH := by linarith
  refine ⟨?_, by positivity, ?_⟩
  · simp only [θs]; field_simp; ring
  · simp only [θs]
    rw [div_mul_eq_mul_div, div_lt_iff₀ hL]
    nlinarith

/-! ### Appendix A, generalized functional forms -/

/-- Eq. (26), the hiring standard `s(Π) = min{θ ∈ [0, θ̄] : ψ(θ) ≤ ρΠ/(1 − Π)}`. -/
noncomputable def sStd (ψ : ℝ → ℝ) (ρ θb piR : ℝ) : ℝ :=
  sInf {θ | θ ∈ Icc 0 θb ∧ ψ θ ≤ ρ * piR / (1 - piR)}

/-- Eq. (26). The standard does not rise with the reputation, for `Π < 1`. -/
theorem sStd_antitone {ψ : ℝ → ℝ} {ρ θb p1 p2 : ℝ} (hρ : 0 ≤ ρ) (_h0 : 0 ≤ p1) (h12 : p1 ≤ p2)
    (h2 : p2 < 1) (hne : {θ | θ ∈ Icc 0 θb ∧ ψ θ ≤ ρ * p1 / (1 - p1)}.Nonempty) :
    sStd ψ ρ θb p2 ≤ sStd ψ ρ θb p1 := by
  unfold sStd
  apply csInf_le_csInf ⟨0, fun θ hθ => hθ.1.1⟩ hne
  intro θ ⟨hθ, hψ⟩
  refine ⟨hθ, hψ.trans ?_⟩
  have a1 : 0 < 1 - p1 := by linarith
  have a2 : 0 < 1 - p2 := by linarith
  rw [div_le_div_iff₀ a1 a2]
  nlinarith

/-- Appendix A. With `F_q ≤ F_u`, the return `ω(F_u(s) − F_q(s))` is not
negative. -/
theorem betaGen_nonneg {ω Fq Fu : ℝ} (hω : 0 ≤ ω) (h : Fq ≤ Fu) : 0 ≤ ω * (Fu - Fq) :=
  mul_nonneg hω (by linarith)

/-- The Jacobian of the proof of Lemma 5 at a steady state, with `A = δ + λ'`,
`β' = ∂β/∂Π` and `φ' = ∂φ/∂V`. -/
def JE (A lam' β' φ' : ℝ) : Matrix (Fin 2) (Fin 2) ℝ := !![A, -(A * β'); lam' * φ', -lam']

/-- Its trace is `δ`. -/
theorem JE_trace (δ lam' β' φ' : ℝ) : (JE (δ + lam') lam' β' φ').trace = δ := by
  rw [Matrix.trace_fin_two]; simp [JE]

/-- Its determinant is `λ'(δ + λ')(β'φ' − 1)`. -/
theorem JE_det (δ lam' β' φ' : ℝ) :
    (JE (δ + lam') lam' β' φ').det = lam' * (δ + lam') * (β' * φ' - 1) := by
  rw [Matrix.det_fin_two]; simp [JE]; ring

/-- Lemma 5, saddle. If the determinant is negative, the characteristic
polynomial `r² − δr + det` has one negative and one positive root. -/
theorem lemma5_saddle {δ D : ℝ} (hD : D < 0) :
    ∃ r1 r2 : ℝ, r1 < 0 ∧ 0 < r2 ∧ ∀ r, r ^ 2 - δ * r + D = (r - r1) * (r - r2) := by
  have hdisc : 0 ≤ δ ^ 2 - 4 * D := by nlinarith [sq_nonneg δ]
  set s := Real.sqrt (δ ^ 2 - 4 * D) with hs
  have hs2 : s ^ 2 = δ ^ 2 - 4 * D := Real.sq_sqrt hdisc
  have hs0 : 0 ≤ s := Real.sqrt_nonneg _
  have hsδ : |δ| < s := by
    rw [← Real.sqrt_sq_eq_abs]; exact Real.sqrt_lt_sqrt (sq_nonneg _) (by linarith)
  refine ⟨(δ - s) / 2, (δ + s) / 2, ?_, ?_, ?_⟩
  · have := le_abs_self δ; linarith
  · have := neg_abs_le δ; linarith
  · intro r; nlinarith [hs2]

/-- Lemma 5, source. With a positive trace `δ` and a positive determinant, no
real root of `r² − δr + det` is negative or zero. -/
theorem lemma5_source {δ D r : ℝ} (hδ : 0 < δ) (hD : 0 < D) (h : r ^ 2 - δ * r + D = 0) : 0 < r := by
  by_contra hr; push_neg at hr; nlinarith [sq_nonneg r]

/-- The determinant is negative iff `β'φ' < 1`. -/
theorem lemma5_det_neg_iff {δ lam' β' φ' : ℝ} (hl : 0 < lam') (hA : 0 < δ + lam') :
    lam' * (δ + lam') * (β' * φ' - 1) < 0 ↔ β' * φ' < 1 := by
  have := mul_pos hl hA
  constructor
  · intro h; by_contra hc; push_neg at hc; nlinarith
  · intro h; nlinarith

/-- Lemma 6. With `K = β'φ' − 1 > 0`, the discriminant
`D = δ² − 4λ'(δ + λ')K` is negative iff `(1 + λ'/δ)(λ'/δ) > 1/(4K)`. -/
theorem lemma6 {δ lam' K : ℝ} (hδ : 0 < δ) (hK : 0 < K) :
    δ ^ 2 - 4 * lam' * (δ + lam') * K < 0 ↔ 1 / (4 * K) < (1 + lam' / δ) * (lam' / δ) := by
  have e : (1 + lam' / δ) * (lam' / δ) = lam' * (δ + lam') / δ ^ 2 := by field_simp
  rw [e, div_lt_div_iff₀ (by positivity) (by positivity)]
  constructor <;> intro h <;> nlinarith

/-- Lemma 6, spiral. With a negative discriminant the characteristic
polynomial has no real root, so `Q_m` is a focus. -/
theorem lemma6_no_real_root {δ D r : ℝ} (hdisc : δ ^ 2 - 4 * D < 0) : r ^ 2 - δ * r + D ≠ 0 := by
  intro h; nlinarith [sq_nonneg (2 * r - δ)]

/-- Lemma 6, node. With a positive discriminant, a positive trace and a
positive determinant, the two roots are real, distinct and positive. -/
theorem lemma6_node {δ D : ℝ} (hδ : 0 < δ) (hD : 0 < D) (hdisc : 0 < δ ^ 2 - 4 * D) :
    ∃ r1 r2 : ℝ, 0 < r1 ∧ r1 < r2 ∧ ∀ r, r ^ 2 - δ * r + D = (r - r1) * (r - r2) := by
  set s := Real.sqrt (δ ^ 2 - 4 * D) with hs
  have hs2 : s ^ 2 = δ ^ 2 - 4 * D := Real.sq_sqrt hdisc.le
  have hs0 : 0 < s := Real.sqrt_pos.2 hdisc
  have hsδ : s < δ := by
    rw [← abs_of_pos hδ, ← Real.sqrt_sq_eq_abs]; exact Real.sqrt_lt_sqrt hdisc.le (by linarith)
  refine ⟨(δ - s) / 2, (δ + s) / 2, by linarith, by linarith, ?_⟩
  intro r; nlinarith [hs2]

/-! ### Appendix B, the change of variable of eqs. (27) and (28) -/

/-- Eqs. (27) and (28). With `Vᵉ = ωPq + ω' + ω(Pu − Pq)v`, the equation
`V̇ᵉ = (δ+λ')(Vᵉ − β(ξ) − ω')` becomes `v̇ = (δ+λ')(v − ξ)`, scaled by
`ω(Pu − Pq)`. -/
theorem eq27 (A ω Pq Pu ω' v ξ : ℝ) :
    A * ((ω * Pq + ω' + ω * (Pu - Pq) * v) - beta ω Pq Pu ξ - ω') = ω * (Pu - Pq) * (A * (v - ξ)) := by
  unfold beta; ring

/-- The point `Vᵉ = (δ+λ')cm` is `v = v*` after the change of variable. -/
theorem eq27_vS {A ω Pq Pu ω' cm : ℝ} (hd : ω * Pq < ω * Pu) :
    ω * Pq + ω' + ω * (Pu - Pq) * vS A ω Pq Pu ω' cm = A * cm := by
  unfold vS
  have : ω * Pu - ω * Pq ≠ 0 := by linarith
  rw [show ω * (Pu - Pq) = ω * Pu - ω * Pq by ring, mul_comm (ω * Pu - ω * Pq),
    div_mul_cancel₀ _ this]
  ring

/-! ### Appendix B, Condition 1 of the proof of Proposition 4

`H = Π_h − Π*`, `L = Π* − Π_l`, `ρ' = λ'/(δ + λ')` (written `r`), and `vs` is
`v*`. Eq. (31) defines `v_a` as a function of `v_c`, and eq. (34) defines
`v_a'`. The curves are solved explicitly. -/

/-- The curve of eq. (31), `(1 − v_a)^ρ' H + (1 − v_c)^ρ' L = (1 − v*)^ρ' (H + L)`. -/
noncomputable def va (r H L vs x : ℝ) : ℝ :=
  1 - (((1 - vs) ^ r * (H + L) - (1 - x) ^ r * L) / H) ^ (1 / r)

/-- The curve of eq. (34), `v_a'^ρ' H + v_c^ρ' L = v*^ρ' (H + L)`. -/
noncomputable def va' (r H L vs x : ℝ) : ℝ :=
  ((vs ^ r * (H + L) - x ^ r * L) / H) ^ (1 / r)

/-- `va` solves eq. (31) wherever its inner term is not negative. -/
theorem va_eq31 {r H L vs x : ℝ} (hr : 0 < r) (hH : 0 < H)
    (hin : 0 ≤ ((1 - vs) ^ r * (H + L) - (1 - x) ^ r * L) / H) :
    (1 - va r H L vs x) ^ r * H + (1 - x) ^ r * L = (1 - vs) ^ r * (H + L) := by
  unfold va
  rw [sub_sub_cancel, ← rpow_mul hin, one_div, inv_mul_cancel₀ hr.ne', rpow_one]
  field_simp; ring

/-- `va'` solves eq. (34) wherever its inner term is not negative. -/
theorem va'_eq34 {r H L vs x : ℝ} (hr : 0 < r) (hH : 0 < H)
    (hin : 0 ≤ (vs ^ r * (H + L) - x ^ r * L) / H) :
    (va' r H L vs x) ^ r * H + x ^ r * L = vs ^ r * (H + L) := by
  unfold va'
  rw [← rpow_mul hin, one_div, inv_mul_cancel₀ hr.ne', rpow_one]
  field_simp; ring

/-- `v_a(v*) = v*` and `v_a'(v*) = v*`, as the proof notes. -/
theorem va_at_vs {r H L vs : ℝ} (hr : 0 < r) (hH : 0 < H) (h1 : vs < 1) :
    va r H L vs vs = vs := by
  unfold va
  have e : ((1 - vs) ^ r * (H + L) - (1 - vs) ^ r * L) / H = (1 - vs) ^ r := by
    field_simp; ring
  rw [e, ← rpow_mul (by linarith), mul_one_div_cancel hr.ne', rpow_one]; ring

theorem va'_at_vs {r H L vs : ℝ} (hr : 0 < r) (hH : 0 < H) (h0 : 0 < vs) :
    va' r H L vs vs = vs := by
  unfold va'
  have e : (vs ^ r * (H + L) - vs ^ r * L) / H = vs ^ r := by field_simp; ring
  rw [e, ← rpow_mul h0.le, mul_one_div_cancel hr.ne', rpow_one]

/-- Condition 1 for `v_a`. Its slope at `(v*, v*)` is `−(Π* − Π_l)/(Π_h − Π*)`,
eq. (35) at that point. -/
theorem va_slope {r H L vs : ℝ} (hr : 0 < r) (hH : 0 < H) (h1 : vs < 1) :
    HasDerivAt (va r H L vs) (-(L / H)) vs := by
  have hw : 0 < 1 - vs := by linarith
  have hb : HasDerivAt (fun x => (1 - x) ^ r) (-1 * r * (1 - vs) ^ (r - 1)) vs := by
    have := ((hasDerivAt_id vs).const_sub 1).rpow_const (p := r) (Or.inl hw.ne')
    simpa using this
  have hg : HasDerivAt (fun x => ((1 - vs) ^ r * (H + L) - (1 - x) ^ r * L) / H)
      (-(-1 * r * (1 - vs) ^ (r - 1) * L) / H) vs :=
    ((hb.mul_const L).const_sub _).div_const H
  have hgv : ((1 - vs) ^ r * (H + L) - (1 - vs) ^ r * L) / H = (1 - vs) ^ r := by
    field_simp; ring
  have hgpos : 0 < (1 - vs) ^ r := rpow_pos_of_pos hw r
  have hp := hg.rpow_const (p := 1 / r) (Or.inl (by rw [hgv]; exact hgpos.ne'))
  have hva := hp.const_sub 1
  unfold va
  convert hva using 1
  rw [hgv, ← rpow_mul hw.le]
  have e1 : r * (1 / r - 1) = 1 - r := by field_simp
  rw [e1]
  have e2 : (1 - vs) ^ (r - 1) * (1 - vs) ^ (1 - r) = 1 := by
    rw [← rpow_add hw]; simp
  field_simp
  linear_combination L * e2

/-- Condition 1 for `v_a'`. Its slope at `(v*, v*)` is also
`−(Π* − Π_l)/(Π_h − Π*)`, eq. (36), so the two curves are tangent there. -/
theorem va'_slope {r H L vs : ℝ} (hr : 0 < r) (hH : 0 < H) (h0 : 0 < vs) :
    HasDerivAt (va' r H L vs) (-(L / H)) vs := by
  have hb : HasDerivAt (fun x => x ^ r) (1 * r * vs ^ (r - 1)) vs := by
    have := (hasDerivAt_id vs).rpow_const (p := r) (Or.inl h0.ne')
    simpa using this
  have hg : HasDerivAt (fun x => (vs ^ r * (H + L) - x ^ r * L) / H)
      (-(1 * r * vs ^ (r - 1) * L) / H) vs :=
    ((hb.mul_const L).const_sub _).div_const H
  have hgv : (vs ^ r * (H + L) - vs ^ r * L) / H = vs ^ r := by field_simp; ring
  have hgpos : 0 < vs ^ r := rpow_pos_of_pos h0 r
  have hp := hg.rpow_const (p := 1 / r) (Or.inl (by rw [hgv]; exact hgpos.ne'))
  unfold va'
  convert hp using 1
  rw [hgv, ← rpow_mul h0.le]
  have e1 : r * (1 / r - 1) = 1 - r := by field_simp
  rw [e1]
  have e2 : vs ^ (r - 1) * vs ^ (1 - r) = 1 := by rw [← rpow_add h0]; simp
  field_simp
  linear_combination L * e2

/-- Condition 1 of the proof of Proposition 4. The two curves meet at
`(v*, v*)` with the same slope. -/
theorem prop4_condition1 {r H L vs : ℝ} (hr : 0 < r) (hH : 0 < H) (h0 : 0 < vs) (h1 : vs < 1) :
    va r H L vs vs = va' r H L vs vs ∧
      HasDerivAt (va r H L vs) (-(L / H)) vs ∧ HasDerivAt (va' r H L vs) (-(L / H)) vs :=
  ⟨by rw [va_at_vs hr hH h1, va'_at_vs hr hH h0], va_slope hr hH h1, va'_slope hr hH h0⟩

end KimLoury2014
