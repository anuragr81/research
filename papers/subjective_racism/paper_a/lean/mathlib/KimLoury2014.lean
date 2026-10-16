import Mathlib

/-! Kim and Loury, "Collective Reputation and the Dynamics of Statistical
Discrimination", MPRA Paper 54950, version of 31 March 2014, in the paper's
notation as far as Lean allows.

The paper's `Π` is written `pi` here, because `Π` is binder notation in Lean.
`piR` is a group's collective reputation `Πⁱ` (the share of qualified workers
in its temporary job market), `piS` is the threshold `Π*`, and `piL`, `piH`
are the low and high reputations `Π_l`, `Π_h`. `Pq` is the probability that
a qualified worker's test proves him qualified, `Pu` the probability that an
unqualified worker's test proves him unqualified, `ω` the wage of Task One,
`xq` and `xu` the employer's gain and loss, `ρ = xq / xu`, `ξ` the probability
of the benefit of the doubt, `δ` the discount rate, `λ'` the replacement rate,
`ω'` the normalised return in the permanent market and `cm` the medium cost.

The record formalises the one-period structure of Sections 3.2 and 3.3 (the
employer's assignment given a reputation, the return to investment given the
employer's rule, and the self-confirming reputations of Lemma 1), and two
closed forms of the dynamics (Lemma 2 and Theorem 2), which are algebra once
the paths are solved. The differential equations themselves are not
formalised. -/

namespace KimLoury2014

open Real

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

end KimLoury2014
