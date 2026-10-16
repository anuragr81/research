import Mathlib

/-! Kim and Loury, "Group Reputation and the Dynamics of Statistical
Discrimination", MPRA Paper No. 18765, version of 12 November 2009, in the
paper's notation. Only the one-period structure of the simple reputation model
(section 3.3) is formalised, together with the market-learning threshold of
Appendix A and the linearisation of Lemma 4. The phase paths, the overlap and
the reputation trap are not formalised.

The signal of an unqualified worker is uniform on `[0, θu]`, that of a
qualified worker uniform on `[θq, θbar]`, with `0 < θq < θu < θbar`. `Pq` is the
probability that a qualified worker's signal proves him qualified, `Pu` the
probability that an unqualified worker's signal proves him unqualified. `pi` is
the paper's `Π`, group reputation, the proportion of qualified workers in the
group, and `piL`, `piH` are its `Πl`, `Πh` (Lean reserves `Π`). `xq` is the
employer's return on a qualified worker in task one, `xu` the loss on an
unqualified one, and `ρ = xq / xu`. `ω` is the wage in task one. -/

namespace KimLoury2009

/-- `Pq = (θbar − θu) / (θbar − θq)`, p. 10. -/
noncomputable def Pq (θq θu θbar : ℝ) : ℝ := (θbar - θu) / (θbar - θq)

/-- `Pu = θq / θu`, p. 10. -/
noncomputable def Pu (θq θu : ℝ) : ℝ := θq / θu

/-- Assumption 1 (p. 10). `Pq < Pu` holds exactly when `θq + θu > θbar`. -/
theorem assumption1_iff {θq θu θbar : ℝ} (hq : 0 < θq) (hqu : θq < θu) (hub : θu < θbar) :
    Pq θq θu θbar < Pu θq θu ↔ θbar < θq + θu := by
  unfold Pq Pu
  have h1 : 0 < θbar - θq := by linarith
  have h2 : 0 < θu := by linarith
  rw [div_lt_div_iff₀ h1 h2]
  constructor
  · intro h
    have : (θu - θq) * θbar < (θu - θq) * (θq + θu) := by nlinarith
    exact lt_of_mul_lt_mul_left this (by linarith)
  · intro h
    have : (θu - θq) * θbar < (θu - θq) * (θq + θu) := mul_lt_mul_of_pos_left h (by linarith)
    nlinarith

/-- The employer's expected payoff from assigning to task one a group member
whose signal is unclear, `xq pi (1 − Pq) − xu (1 − pi)(1 − Pu)`, up to the
positive mass of unclear signals (p. 10). -/
def bodPayoff (xq xu Pq Pu pi : ℝ) : ℝ := xq * pi * (1 - Pq) - xu * (1 - pi) * (1 - Pu)

/-- The reputation threshold `Π* = (1 − Pu) / (ρ (1 − Pq) + 1 − Pu)`, p. 10. -/
noncomputable def piStar (ρ Pq Pu : ℝ) : ℝ := (1 - Pu) / (ρ * (1 - Pq) + 1 - Pu)

/-- Pp. 10 to 11. The employer gives the benefit of the doubt (BOD) to a group
exactly when its reputation is at least `Π*`, with `xq = ρ xu`. -/
theorem bod_iff {ρ xu Pq Pu pi : ℝ} (hρ : 0 < ρ) (hxu : 0 < xu) (hq : Pq < 1) (hu : Pu < 1) :
    0 ≤ bodPayoff (ρ * xu) xu Pq Pu pi ↔ piStar ρ Pq Pu ≤ pi := by
  unfold bodPayoff piStar
  have hd : 0 < ρ * (1 - Pq) + 1 - Pu := by nlinarith
  rw [div_le_iff₀ hd]
  constructor
  · intro h
    have : xu * (1 - Pu) ≤ xu * (pi * (ρ * (1 - Pq) + 1 - Pu)) := by nlinarith
    exact le_of_mul_le_mul_left this hxu
  · intro h
    nlinarith

/-- P. 11. The threshold in the uniform case is `(θbar − θq) / (ρ θu + θbar − θq)`,
"identical to the above". -/
theorem piStar_uniform {ρ θq θu θbar : ℝ} (hq : 0 < θq) (hqu : θq < θu) (hub : θu < θbar)
    (hρ : 0 < ρ) :
    piStar ρ (Pq θq θu θbar) (Pu θq θu) = (θbar - θq) / (ρ * θu + θbar - θq) := by
  have a0 : 0 < θbar - θq := by linarith
  have u0 : 0 < θu := by linarith
  have d0 : 0 < θu - θq := by linarith
  have e1 : 1 - Pu θq θu = (θu - θq) / θu := by unfold Pu; field_simp
  have e2 : 1 - Pq θq θu θbar = (θu - θq) / (θbar - θq) := by unfold Pq; field_simp; ring
  have e3 : ρ * (1 - Pq θq θu θbar) + 1 - Pu θq θu
      = (θu - θq) * (ρ * θu + θbar - θq) / (θu * (θbar - θq)) := by
    rw [add_sub_assoc, e1, e2]; field_simp; ring
  have h4 : 0 < ρ * θu + θbar - θq := by nlinarith
  unfold piStar
  rw [e3, e1]
  field_simp

/-- P. 11. The extra benefit of being qualified with BOD is
`ω [Fu(θq) − Fq(θq)] = ω Pu`, and without BOD `ω [Fu(θu) − Fq(θu)] = ω Pq`. -/
theorem beta_values {ω θq θu θbar : ℝ} (hqu : θq < θu) (hub : θu < θbar) :
    ω * (θq / θu - 0) = ω * Pu θq θu ∧
      ω * (1 - (θu - θq) / (θbar - θq)) = ω * Pq θq θu θbar := by
  refine ⟨by unfold Pu; ring, ?_⟩
  unfold Pq
  have h1 : θbar - θq ≠ 0 := by linarith
  field_simp
  ring

/-- Eq. (10). The current extra benefit of being qualified, `ω Pu` at or above
the threshold and `ω Pq` below it. -/
noncomputable def beta (ω Pq Pu piS pi : ℝ) : ℝ := if piS ≤ pi then ω * Pu else ω * Pq

/-- Eq. (10) with Assumption 1. The benefit of investing is nondecreasing in
group reputation, so one member's incentive rises with the others' investment. -/
theorem beta_mono {ω Pq Pu piS pi₁ pi₂ : ℝ} (hω : 0 ≤ ω) (hPu : Pq ≤ Pu) (h : pi₁ ≤ pi₂) :
    beta ω Pq Pu piS pi₁ ≤ beta ω Pq Pu piS pi₂ := by
  unfold beta
  split_ifs with h1 h2 h2
  · exact le_rfl
  · exact absurd (le_trans h1 h) h2
  · exact mul_le_mul_of_nonneg_left hPu hω
  · exact le_rfl

/-- Eq. (3). Individual reputation, the posterior that a member of a group with
reputation `pi` who emits a signal with densities `fq`, `fu` is qualified. -/
noncomputable def R (fq fu pi : ℝ) : ℝ := pi * fq / (pi * fq + (1 - pi) * fu)

/-- P. 8. Individual reputation is strictly increasing in group reputation. -/
theorem R_strictMono {fq fu pi₁ pi₂ : ℝ} (hq : 0 < fq) (hu : 0 < fu) (h0 : 0 ≤ pi₁)
    (h12 : pi₁ < pi₂) (h1 : pi₂ ≤ 1) : R fq fu pi₁ < R fq fu pi₂ := by
  unfold R
  have d1 : 0 < pi₁ * fq + (1 - pi₁) * fu := by nlinarith
  have d2 : 0 < pi₂ * fq + (1 - pi₂) * fu := by nlinarith
  rw [div_lt_div_iff₀ d1 d2]
  nlinarith [mul_pos hq hu]

/-- Eq. (11) in the simple model. A fraction `piL` of a cohort invests when the
normalised lifetime benefit `V` is below `(δ + λ) cm`, written `K`, and `piH` at
or above it. -/
noncomputable def phi (piL piH K V : ℝ) : ℝ := if K ≤ V then piH else piL

/-- A steady state of the simple model, `pi = φ(V)` and `V = β(pi)` (eq. (9)). -/
def Steady (ω Pq Pu piS piL piH K pi V : ℝ) : Prop :=
  pi = phi piL piH K V ∧ V = beta ω Pq Pu piS pi

/-- P. 11. With `Π* ∈ (piL, piH)` and `K ∈ (ω Pq, ω Pu)`, both `Qh = (ω Pu, piH)`
and `Ql = (ω Pq, piL)` are steady states. -/
theorem two_steady_states {ω Pq Pu piS piL piH K : ℝ} (hl : piL < piS) (hh : piS < piH)
    (hK1 : ω * Pq < K) (hK2 : K < ω * Pu) :
    Steady ω Pq Pu piS piL piH K piH (ω * Pu) ∧ Steady ω Pq Pu piS piL piH K piL (ω * Pq) := by
  unfold Steady phi beta
  refine ⟨⟨?_, ?_⟩, ⟨?_, ?_⟩⟩
  · rw [if_pos hK2.le]
  · rw [if_pos hh.le]
  · rw [if_neg (not_le.2 hK1)]
  · rw [if_neg (not_le.2 hl)]

/-- Footnote 3 and the proof of Proposition 3. Employers earn more per period
from a group at `Qh`, with BOD, than at `Ql`, without it, when
`piL < Π* < piH`. -/
theorem employer_prefers_Qh {ρ xu Pq Pu piL piH : ℝ} (hρ : 0 < ρ) (hxu : 0 < xu)
    (hPq0 : 0 < Pq) (hq : Pq < 1) (hu : Pu < 1)
    (hlh : piL < piH) (hh : piStar ρ Pq Pu < piH) :
    piL * Pq * (ρ * xu) < piH * (ρ * xu) - (1 - piH) * (1 - Pu) * xu := by
  have hb : 0 ≤ bodPayoff (ρ * xu) xu Pq Pu piH := (bod_iff hρ hxu hq hu).2 hh.le
  unfold bodPayoff at hb
  have : piL * Pq * (ρ * xu) < piH * Pq * (ρ * xu) := by
    have := mul_pos hPq0 (mul_pos hρ hxu)
    nlinarith
  nlinarith

/-- Proof of Lemma 4. The Jacobian of the linearised system at a steady state has
trace `δ` and determinant `λ (δ + λ)(β′ φ′ − 1)`. -/
theorem jacobian_trace_det (δ l b f : ℝ) :
    Matrix.trace !![δ + l, -(δ + l) * b; l * f, -l] = δ ∧
      Matrix.det !![δ + l, -(δ + l) * b; l * f, -l] = l * (δ + l) * (b * f - 1) := by
  constructor
  · simp [Matrix.trace, Fin.sum_univ_two]
  · simp [Matrix.det_fin_two]; ring

/-- Proof of Lemma 5. With `β′ φ′ > 1`, the discriminant is negative, and the
paths spiral, exactly when `(1 + λ/δ) λ/δ > 1 / (4 (φ′ β′ − 1))`. -/
theorem spiral_iff {δ l k : ℝ} (hδ : 0 < δ) (hk : 1 < k) :
    δ ^ 2 - 4 * l * (δ + l) * (k - 1) < 0 ↔ 1 / (4 * (k - 1)) < (1 + l / δ) * (l / δ) := by
  have hk' : 0 < 4 * (k - 1) := by linarith
  rw [div_lt_iff₀ hk']
  have e : (1 + l / δ) * (l / δ) * (4 * (k - 1)) = 4 * l * (δ + l) * (k - 1) / δ ^ 2 := by
    field_simp
  rw [e, lt_div_iff₀ (by positivity)]
  constructor <;> intro h <;> linarith

/-- Appendix A and its proof. Below `Π*` the `V̇ = 0` locus with market learning
lies at `((δ + λ) ω Pq + η ω) / (δ + λ + η)`. It exceeds `K = (δ + λ) cm`, so that
`Ql` is not a steady state, exactly when
`η > (δ + λ)(K − ω Pq) / (ω − K) = η*`. Here `s = δ + λ`. -/
theorem market_learning_threshold {s η ω Pq K : ℝ} (hs : 0 < s) (hη : 0 ≤ η) (hK : K < ω) :
    K < (s * ω * Pq + η * ω) / (s + η) ↔ s * (K - ω * Pq) / (ω - K) < η := by
  rw [lt_div_iff₀ (by linarith), div_lt_iff₀ (by linarith)]
  constructor <;> intro h <;> nlinarith

/-- When the signal reveals type, `Pq = Pu = 1`, the benefit of investing is `ω`
whatever the group's reputation. The paper states the verbal form on p. 10 and
the limit form in Appendix A. -/
theorem revealed_type_no_group_effect (ω piS pi : ℝ) : beta ω 1 1 piS pi = ω := by
  unfold beta; split_ifs <;> ring

/-- Control: without the cost cutoff between `ω Pq` and `ω Pu`, multiplicity
fails. With `K < ω Pq`, the low state `(ω Pq, piL)` is not a steady state when
`piL ≠ piH`. -/
theorem control_no_low_state_if_cutoff_low {ω Pq Pu piS piL piH K : ℝ} (hK : K < ω * Pq)
    (hne : piL ≠ piH) : ¬ Steady ω Pq Pu piS piL piH K piL (ω * Pq) := by
  rintro ⟨h1, -⟩
  unfold phi at h1
  rw [if_pos hK.le] at h1
  exact hne h1

/-- Control: without Assumption 1's gap, group reputation does not move the
benefit of investing. With `Pq = Pu`, `β` is the same at every reputation. -/
theorem control_equal_informativeness {ω P piS pi₁ pi₂ : ℝ} :
    beta ω P P piS pi₁ = beta ω P P piS pi₂ := by
  unfold beta; split_ifs <;> rfl

/-- Control: without `Π* < piH` the employer ranking can reverse. With
`ρ = xu = 1`, `Pq = 1/2`, `Pu = 9/10`, `piH = 1/10`, `piL = 1/20`, the threshold is
`1/6 > piH`, and the per-period profit at `Ql` exceeds that at `Qh`. -/
theorem control_employer_ranking_needs_threshold :
    (1 / 10 : ℝ) < piStar 1 (1 / 2) (9 / 10) ∧
      (1 / 10 : ℝ) * 1 - (1 - 1 / 10) * (1 - 9 / 10) * 1 < 1 / 20 * (1 / 2) * 1 := by
  unfold piStar
  norm_num

end KimLoury2009
