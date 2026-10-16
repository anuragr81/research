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

/-! ## The rest of the model

The sections below implement the remaining structure of the paper: the static stage
of section 2 in general, the dynamic system of section 3.2, the phase analysis of the
simple model in section 3.3.1, the linearisation of section 3.4, the strategies of
section 5 and Appendix A. -/

section Full

open Real Filter Topology Set MeasureTheory

/-! ### Section 2. The static stage in general -/

/-- Eq. (1), the signals at which a group-`i` worker is assigned to task one, with
`ψ(θ) ≤ ρΠ/(1 − Π)` cross-multiplied so that `Π = 1` is covered. -/
def standardSet (ψ : ℝ → ℝ) (ρ θbar pi : ℝ) : Set ℝ :=
  {θ | θ ∈ Icc 0 θbar ∧ ψ θ * (1 - pi) ≤ ρ * pi}

/-- Eq. (1), the hiring standard `s*(Π)`. -/
noncomputable def sStar (ψ : ℝ → ℝ) (ρ θbar pi : ℝ) : ℝ := sInf (standardSet ψ ρ θbar pi)

/-- P. 5. `s*` is nonincreasing in `Π`. -/
theorem sStar_antitone {ψ : ℝ → ℝ} {ρ θbar p1 p2 : ℝ} (hψ : ∀ θ, 0 ≤ ψ θ) (hρ : 0 ≤ ρ)
    (h12 : p1 ≤ p2) (hne : (standardSet ψ ρ θbar p1).Nonempty) :
    sStar ψ ρ θbar p2 ≤ sStar ψ ρ θbar p1 := by
  apply csInf_le_csInf ⟨0, fun θ hθ => hθ.1.1⟩ hne
  rintro θ ⟨hθ, h⟩
  refine ⟨hθ, ?_⟩
  nlinarith [mul_nonneg (hψ θ) (sub_nonneg.2 h12), mul_nonneg hρ (sub_nonneg.2 h12)]

/-- P. 5. `s*(1) = 0`. -/
theorem sStar_one {ψ : ℝ → ℝ} {ρ θbar : ℝ} (hρ : 0 ≤ ρ) (hθ : 0 ≤ θbar) :
    sStar ψ ρ θbar 1 = 0 := by
  have e : standardSet ψ ρ θbar 1 = Icc 0 θbar := by
    ext θ; simp [standardSet, hρ]
  rw [sStar, e, csInf_Icc hθ]

/-- P. 5. `0 ≤ s*(Π) ≤ θbar` whenever some signal meets the standard, which at `Π = 0`
is the paper's `s*(0) ≤ θbar`. -/
theorem sStar_mem {ψ : ℝ → ℝ} {ρ θbar pi : ℝ} (hne : (standardSet ψ ρ θbar pi).Nonempty) :
    sStar ψ ρ θbar pi ∈ Icc 0 θbar := by
  obtain ⟨θ, hθ⟩ := hne
  refine ⟨le_csInf ⟨θ, hθ⟩ fun x hx => hx.1.1, ?_⟩
  exact (csInf_le ⟨0, fun x hx => hx.1.1⟩ hθ).trans hθ.1.2

/-- P. 6. The benefit of investing against standard `s`, `β̃(s) = W [Fu(s) − Fq(s)]`. -/
def betaTilde (W : ℝ) (Fu Fq : ℝ → ℝ) (s : ℝ) : ℝ := W * (Fu s - Fq s)

/-- P. 6. A worker with cost `c` invests iff `W [Fu(s) − Fq(s)] ≥ c`. -/
def Invests (W : ℝ) (Fu Fq : ℝ → ℝ) (s c : ℝ) : Prop := c ≤ betaTilde W Fu Fq s

/-- P. 6. `G(β̃(0)) = G(β̃(θbar)) = G(0)`, because both CDFs are 0 at 0 and 1 at `θbar`. -/
theorem betaTilde_ends {W θbar : ℝ} {Fu Fq : ℝ → ℝ} (G : ℝ → ℝ) (hu0 : Fu 0 = 0)
    (hq0 : Fq 0 = 0) (hu1 : Fu θbar = 1) (hq1 : Fq θbar = 1) :
    G (betaTilde W Fu Fq 0) = G 0 ∧ G (betaTilde W Fu Fq θbar) = G 0 := by
  simp [betaTilde, hu0, hq0, hu1, hq1]

/-- A steady state of the static model at an interior standard. `Π = G(β̃(s))`, and `s`
is the standard at `Π`, `ψ(s)(1 − Π) = ρΠ`. -/
def StaticSteady (ψ G : ℝ → ℝ) (W ρ : ℝ) (Fu Fq : ℝ → ℝ) (s pi : ℝ) : Prop :=
  pi = G (betaTilde W Fu Fq s) ∧ ψ s * (1 - pi) = ρ * pi

/-- Proposition 1. With `G(β̃(s¹)) > ψ(s¹)/(ρ + ψ(s¹))`, `G(β̃(s²)) < ψ(s²)/(ρ + ψ(s²))`,
`s¹ < s²`, and the boundary conditions at `0` and `θbar` that the paper leaves to
"checking the boundary conditions" stated as hypotheses (KLA-D10), there are three
distinct steady states. -/
theorem prop1 {ψ G Fu Fq : ℝ → ℝ} {W ρ θbar s1 s2 : ℝ} (hρ : 0 < ρ) (hψ : ∀ θ, 0 ≤ ψ θ)
    (hcont : ContinuousOn (fun s => G (betaTilde W Fu Fq s) - ψ s / (ρ + ψ s)) (Icc 0 θbar))
    (h0 : G (betaTilde W Fu Fq 0) < ψ 0 / (ρ + ψ 0))
    (h1 : ψ s1 / (ρ + ψ s1) < G (betaTilde W Fu Fq s1))
    (h2 : G (betaTilde W Fu Fq s2) < ψ s2 / (ρ + ψ s2))
    (h3 : ψ θbar / (ρ + ψ θbar) < G (betaTilde W Fu Fq θbar))
    (hs1 : 0 < s1) (hs12 : s1 < s2) (hs2 : s2 < θbar) :
    ∃ a b c, a < b ∧ b < c ∧ StaticSteady ψ G W ρ Fu Fq a (G (betaTilde W Fu Fq a)) ∧
      StaticSteady ψ G W ρ Fu Fq b (G (betaTilde W Fu Fq b)) ∧
      StaticSteady ψ G W ρ Fu Fq c (G (betaTilde W Fu Fq c)) := by
  set h : ℝ → ℝ := fun s => G (betaTilde W Fu Fq s) - ψ s / (ρ + ψ s) with hh
  have toSteady : ∀ s, h s = 0 → StaticSteady ψ G W ρ Fu Fq s (G (betaTilde W Fu Fq s)) := by
    intro s hs
    refine ⟨rfl, ?_⟩
    have hd : 0 < ρ + ψ s := by linarith [hψ s]
    have e : G (betaTilde W Fu Fq s) = ψ s / (ρ + ψ s) := by simp only [hh] at hs; linarith
    rw [e]; field_simp; ring
  obtain ⟨a, ha, ha0⟩ := intermediate_value_Ioo hs1.le
    (hcont.mono (Icc_subset_Icc le_rfl (by linarith)))
    (show (0:ℝ) ∈ Ioo (h 0) (h s1) from ⟨by simp only [hh]; linarith, by simp only [hh]; linarith⟩)
  obtain ⟨b, hb, hb0⟩ := intermediate_value_Ioo' hs12.le
    (hcont.mono (Icc_subset_Icc hs1.le (by linarith)))
    (show (0:ℝ) ∈ Ioo (h s2) (h s1) from ⟨by simp only [hh]; linarith, by simp only [hh]; linarith⟩)
  obtain ⟨c, hc, hc0⟩ := intermediate_value_Ioo hs2.le
    (hcont.mono (Icc_subset_Icc (by linarith) le_rfl))
    (show (0:ℝ) ∈ Ioo (h s2) (h θbar) from ⟨by simp only [hh]; linarith, by simp only [hh]; linarith⟩)
  exact ⟨a, b, c, ha.2.trans hb.1, hb.2.trans hc.1, toSteady a ha0, toSteady b hb0,
    toSteady c hc0⟩

/-- The uniform CDFs of section 3.3 on `[0, θbar]`. -/
noncomputable def FuU (θu θ : ℝ) : ℝ := min (θ / θu) 1

/-- The uniform CDF of a qualified worker's signal. -/
noncomputable def FqU (θq θbar θ : ℝ) : ℝ := max 0 ((θ - θq) / (θbar - θq))

/-- P. 5. A nonincreasing `ψ` gives `Fq ≤ Fu`, in the uniform case of section 3.3. -/
theorem Fq_le_Fu_uniform {θq θu θbar θ : ℝ} (hq : 0 < θq) (hqu : θq < θu)
    (hub : θu < θbar) (h0 : 0 ≤ θ) (h1 : θ ≤ θbar) : FqU θq θbar θ ≤ FuU θu θ := by
  unfold FqU FuU
  have hu : 0 < θu := by linarith
  have hb : 0 < θbar - θq := by linarith
  rcases le_total θ θu with h | h
  · rw [min_eq_left ((div_le_one hu).2 h)]
    apply max_le (div_nonneg h0 hu.le)
    rw [div_le_div_iff₀ hb hu]
    nlinarith [mul_nonneg h0 (sub_nonneg.2 (le_trans h hub.le)),
      mul_nonneg hq.le (sub_nonneg.2 h), mul_nonneg h0 (sub_nonneg.2 hub.le)]
  · rw [min_eq_right ((one_le_div hu).2 h)]
    apply max_le zero_le_one
    rw [div_le_one hb]; linarith

/-- Eq. (2). Employers recover the proportion qualified from the group's signal
distribution `F^i = Π Fq + (1 − Π) Fu`. -/
theorem eq2 {Fu Fq F pi : ℝ} (hF : F = pi * Fq + (1 - pi) * Fu) (hne : Fu ≠ Fq) :
    pi = (Fu - F) / (Fu - Fq) := by
  rw [eq_div_iff (sub_ne_zero.2 hne), hF]; ring

/-! ### Section 3.2. The dynamic system -/

/-- Eq. (4) with a constant benefit. `(δ + λ) ∫_t^∞ β e^{−(δ+λ)(τ−t)} dτ = β`, written with
`s = δ + λ`. -/
theorem eq4_constant {s : ℝ} (hs : 0 < s) (β t : ℝ) :
    s * ∫ τ in Ioi t, β * exp (-(s * (τ - t))) = β := by
  have e : ∀ τ, β * exp (-(s * (τ - t))) = (β * exp (s * t)) * exp ((-s) * τ) := by
    intro τ
    rw [mul_assoc, ← exp_add]; congr 2; ring
  simp_rw [e]
  rw [integral_const_mul, integral_exp_mul_Ioi (by linarith),
    show -s * t = -(s * t) by ring, exp_neg]
  field_simp

/-- Footnote 2. `W ≡ ∫_t^∞ ω e^{−(δ+λ)(τ−t)} dτ = ω/(δ + λ)`, so the dynamic loci are the
static reaction curves scaled down by `δ + λ`. -/
theorem footnote2 {s : ℝ} (hs : 0 < s) (ω t : ℝ) :
    ∫ τ in Ioi t, ω * exp (-(s * (τ - t))) = ω / s := by
  rw [eq_div_iff hs.ne', mul_comm, eq4_constant hs]

/-- Eq. (5) where `β` is constant. `V_t = β + (V_0 − β) e^{(δ+λ)t}` solves
`V̇ = (δ + λ)(V − β)`. -/
theorem V_path (s β V0 t : ℝ) :
    HasDerivAt (fun t => β + (V0 - β) * exp (s * t))
      (s * ((β + (V0 - β) * exp (s * t)) - β)) t := by
  have h := (((hasDerivAt_id' t).const_mul s).exp.const_mul (V0 - β)).const_add β
  convert h using 1; ring

/-- Eq. (7) where `φ` is constant. `Π_t = φ + (Π_0 − φ) e^{−λt}` solves `Π̇ = λ(φ − Π)`.
The paths `Π_τ = Πh − (Πh − Πl) e^{−λ(τ−t_0)}` of section 5 are of this form. -/
theorem reputation_path (l φ p0 t : ℝ) :
    HasDerivAt (fun t => φ + (p0 - φ) * exp (-(l * t)))
      (l * (φ - (φ + (p0 - φ) * exp (-(l * t))))) t := by
  have h := ((((hasDerivAt_id' t).const_mul l).neg).exp.const_mul (p0 - φ)).const_add φ
  convert h using 1; simp only [Pi.neg_apply]; ring

/-- Theorem 1, the demarcation loci. `Π̇ = 0` iff `Π = φ`, and `V̇ = 0` iff `V = β`. -/
theorem theorem1_loci {l s φ pi V β : ℝ} (hl : 0 < l) (hs : 0 < s) :
    (l * (φ - pi) = 0 ↔ pi = φ) ∧ (s * (V - β) = 0 ↔ V = β) := by
  constructor
  · constructor
    · intro h; have := (mul_eq_zero.1 h).resolve_left hl.ne'; linarith
    · intro h; rw [h]; ring
  · constructor
    · intro h; have := (mul_eq_zero.1 h).resolve_left hs.ne'; linarith
    · intro h; rw [h]; ring

/-! ### Section 3.3. The simple model -/

/-- P. 11. Under pure assignment the middle state `Qm` at `Π = Π*` is not a steady state
when `Π*` differs from `Πl` and `Πh`, so it needs the mixed assignment of footnote 1. -/
theorem Qm_needs_mixing {ω Pq Pu piS piL piH K V : ℝ} (hL : piS ≠ piL) (hH : piS ≠ piH) :
    ¬ Steady ω Pq Pu piS piL piH K piS V := by
  rintro ⟨h, -⟩
  unfold phi at h
  split_ifs at h
  · exact hH h
  · exact hL h

/-- Section 3.3.1. With `V = ωPq + ω(Pu − Pq)v` and `β = ωPq + ω(Pu − Pq)ξ`,
`V̇ = (δ + λ)(V − β)` becomes `v̇ = (δ + λ)(v − ξ)`. -/
theorem v_transform {s ω Pq Pu v ξ vdot : ℝ} (hω : ω * (Pu - Pq) ≠ 0) :
    ω * (Pu - Pq) * vdot
        = s * ((ω * Pq + ω * (Pu - Pq) * v) - (ω * Pq + ω * (Pu - Pq) * ξ)) ↔
      vdot = s * (v - ξ) := by
  constructor
  · intro h; apply mul_left_cancel₀ hω; rw [h]; ring
  · intro h; rw [h]; ring

/-- P. 12. `v* = ((δ + λ)cm − ωPq)/(ω(Pu − Pq))`, with `K = (δ + λ)cm`. -/
noncomputable def vStar (K ω Pq Pu : ℝ) : ℝ := (K - ω * Pq) / (ω * (Pu - Pq))

/-- P. 12. `V = (δ + λ)cm` exactly when `v = v*`. -/
theorem vStar_iff {K ω Pq Pu v : ℝ} (hω : ω * (Pu - Pq) ≠ 0) :
    ω * Pq + ω * (Pu - Pq) * v = K ↔ v = vStar K ω Pq Pu := by
  unfold vStar; rw [eq_div_iff hω]; constructor <;> intro h <;> linarith

/-- P. 12. The indicator of BOD, with the tie read as in eq. (10) (KLA-D5). -/
noncomputable def xi (piS pi : ℝ) : ℝ := if piS ≤ pi then 1 else 0

/-- P. 12. The investment rate of a newborn cohort at normalised benefit `v`. -/
noncomputable def phiv (piL piH vS v : ℝ) : ℝ := if vS ≤ v then piH else piL

/-- A path of the simple model on `t ≥ 0`, `v̇ = (δ + λ)(v − ξ)` and `Π̇ = λ(φ − Π)`. -/
def IsPath (δ l piS piL piH vS : ℝ) (v p : ℝ → ℝ) : Prop :=
  ∀ t, 0 ≤ t → HasDerivAt v ((δ + l) * (v t - xi piS (p t))) t ∧
    HasDerivAt p (l * (phiv piL piH vS (v t) - p t)) t

/-- Definition 1. A state is economically stable if from every reputation near it some
path of the model converges to it. -/
def EconStable (δ l piS piL piH vS v' p' : ℝ) : Prop :=
  ∃ ε > 0, ∀ p0, |p0 - p'| < ε → ∃ v p : ℝ → ℝ, IsPath δ l piS piL piH vS v p ∧
    p 0 = p0 ∧ Tendsto v atTop (𝓝 v') ∧ Tendsto p atTop (𝓝 p')

theorem tendsto_decay {l : ℝ} (hl : 0 < l) (X c : ℝ) :
    Tendsto (fun t => X + c * exp (-(l * t))) atTop (𝓝 X) := by
  have h : Tendsto (fun t => exp (-(l * t))) atTop (𝓝 0) :=
    tendsto_exp_neg_atTop_nhds_zero.comp (tendsto_id.const_mul_atTop hl)
  simpa using (h.const_mul c).const_add X

/-- Proposition 2, the high state. `Qh = (1, Πh)` is economically stable. -/
theorem prop2_Qh {δ l piS piL piH vS : ℝ} (hl : 0 < l) (hSH : piS < piH) (hv1 : vS ≤ 1) :
    EconStable δ l piS piL piH vS 1 piH := by
  refine ⟨piH - piS, by linarith, fun p0 hp0 => ?_⟩
  refine ⟨fun _ => 1, fun t => piH + (p0 - piH) * exp (-(l * t)), ?_, by simp,
    tendsto_const_nhds, tendsto_decay hl _ _⟩
  intro t ht
  have hb := (abs_lt.1 hp0).1
  have he : exp (-(l * t)) ≤ 1 := exp_le_one_iff.2 (by nlinarith)
  have he0 : 0 < exp (-(l * t)) := exp_pos _
  have key : -(piH - piS) < (p0 - piH) * exp (-(l * t)) := by
    rcases le_total p0 piH with h | h
    · have := mul_le_mul_of_nonpos_left he (sub_nonpos.2 h); linarith
    · have := mul_nonneg (sub_nonneg.2 h) he0.le; linarith
  have hx : xi piS (piH + (p0 - piH) * exp (-(l * t))) = 1 := by
    unfold xi; rw [if_pos (by linarith)]
  have hφ : phiv piL piH vS 1 = piH := by unfold phiv; rw [if_pos hv1]
  refine ⟨?_, ?_⟩
  · simp only [hx]; simpa using hasDerivAt_const t (1:ℝ)
  · simp only [hφ]; exact reputation_path l piH p0 t

/-- Proposition 2, the low state. `Ql = (0, Πl)` is economically stable. -/
theorem prop2_Ql {δ l piS piL piH vS : ℝ} (hl : 0 < l) (hLS : piL < piS) (hv0 : 0 < vS) :
    EconStable δ l piS piL piH vS 0 piL := by
  refine ⟨piS - piL, by linarith, fun p0 hp0 => ?_⟩
  refine ⟨fun _ => 0, fun t => piL + (p0 - piL) * exp (-(l * t)), ?_, by simp,
    tendsto_const_nhds, tendsto_decay hl _ _⟩
  intro t ht
  have hb := (abs_lt.1 hp0).2
  have he : exp (-(l * t)) ≤ 1 := exp_le_one_iff.2 (by nlinarith)
  have he0 : 0 < exp (-(l * t)) := exp_pos _
  have key : (p0 - piL) * exp (-(l * t)) < piS - piL := by
    rcases le_total p0 piL with h | h
    · have := mul_nonpos_of_nonpos_of_nonneg (sub_nonpos.2 h) he0.le; linarith
    · have := mul_le_mul_of_nonneg_left he (sub_nonneg.2 h); linarith
  have hx : xi piS (piL + (p0 - piL) * exp (-(l * t))) = 0 := by
    unfold xi; rw [if_neg (by linarith)]
  have hφ : phiv piL piH vS 0 = piL := by unfold phiv; rw [if_neg (by linarith)]
  refine ⟨?_, ?_⟩
  · simp only [hx]; simpa using hasDerivAt_const t (0:ℝ)
  · simp only [hφ]; exact reputation_path l piL p0 t

/-- Control for Proposition 2: with `v* > 1` the high state is not a rest point, since
then a cohort facing `v = 1` invests only at `Πl`. -/
theorem control_prop2_needs_vStar {l piL piH vS : ℝ} (hl : 0 < l) (hv : 1 < vS)
    (hne : piL ≠ piH) : l * (phiv piL piH vS 1 - piH) ≠ 0 := by
  unfold phiv; rw [if_neg (by linarith)]
  exact mul_ne_zero hl.ne' (sub_ne_zero.2 hne)

/-- Eqs. (15), (16), (18), (19). In a region where `v̇ = (δ + λ)(v − ξ)` and
`Π̇ = λ(X − Π)`, the path keeps `|v − ξ|^{λ/(δ+λ)} (Π − X)` constant. -/
theorem region_invariant {s : ℝ} (hs : 0 < s) (l ξ X v0 p0 t : ℝ) :
    |(ξ + (v0 - ξ) * exp (s * t)) - ξ| ^ (l / s) * ((X + (p0 - X) * exp (-(l * t))) - X)
      = |v0 - ξ| ^ (l / s) * (p0 - X) := by
  have e1 : (ξ + (v0 - ξ) * exp (s * t)) - ξ = (v0 - ξ) * exp (s * t) := by ring
  rw [e1, abs_mul, abs_of_pos (exp_pos _), mul_rpow (abs_nonneg _) (exp_pos _).le,
    ← exp_mul]
  have e2 : s * t * (l / s) = l * t := by field_simp
  rw [e2]
  have e3 : exp (l * t) * exp (-(l * t)) = 1 := by rw [← exp_add]; simp
  calc _ = |v0 - ξ| ^ (l / s) * (p0 - X) * (exp (l * t) * exp (-(l * t))) := by ring
    _ = _ := by rw [e3, mul_one]

/-- Eq. (17), from (15) and (16) by eliminating `Π_b`. -/
theorem eq17 {rp va vc vS piS piL piH pb : ℝ} (ha : va < 1) (hc : vc < 1) (hS : vS < 1)
    (hSH : piS < piH) (hLS : piL < piS)
    (h15 : ((1 - va) / (1 - vS)) ^ rp = (piH - pb) / (piH - piS))
    (h16 : ((1 - vc) / (1 - vS)) ^ rp = (pb - piL) / (piS - piL)) :
    (1 - va) ^ rp * (piH - piS) + (1 - vc) ^ rp * (piS - piL)
      = (1 - vS) ^ rp * (piH - piL) := by
  rw [div_rpow (by linarith) (by linarith)] at h15 h16
  have hS' : 0 < (1 - vS) ^ rp := rpow_pos_of_pos (by linarith) _
  rw [div_eq_div_iff hS'.ne' (by linarith)] at h15
  rw [div_eq_div_iff hS'.ne' (by linarith)] at h16
  linear_combination h15 + h16

/-- Eq. (20), from (18) and (19) by eliminating `Π_d`. -/
theorem eq20 {rp va' vc vS piS piL piH pd : ℝ} (ha : 0 ≤ va') (hc : 0 ≤ vc) (hS : 0 < vS)
    (hSH : piS < piH) (hLS : piL < piS)
    (h19 : (va' / vS) ^ rp = (piH - pd) / (piH - piS))
    (h18 : (vc / vS) ^ rp = (pd - piL) / (piS - piL)) :
    va' ^ rp * (piH - piS) + vc ^ rp * (piS - piL) = vS ^ rp * (piH - piL) := by
  rw [div_rpow ha hS.le] at h19
  rw [div_rpow hc hS.le] at h18
  have hS' : 0 < vS ^ rp := rpow_pos_of_pos hS _
  rw [div_eq_div_iff hS'.ne' (by linarith)] at h19
  rw [div_eq_div_iff hS'.ne' (by linarith)] at h18
  linear_combination h19 + h18

/-- The tangent line bounds `x ^ r` from above for `0 < r < 1`, strictly off the point. -/
theorem rpow_tangent {rp x y : ℝ} (hr0 : 0 < rp) (hr1 : rp < 1) (hx : 0 < x) (hy : 0 ≤ y)
    (hxy : y ≠ x) : y ^ rp < x ^ rp + rp * x ^ (rp - 1) * (y - x) := by
  have hs : -1 ≤ y / x - 1 := by have := div_nonneg hy hx.le; linarith
  have hs' : y / x - 1 ≠ 0 := by
    intro h; apply hxy
    exact (div_eq_one_iff_eq hx.ne').1 (by linarith)
  have key := rpow_one_add_lt_one_add_mul_self hs hs' hr0 hr1
  rw [show 1 + (y / x - 1) = y / x by ring, div_rpow hy hx.le] at key
  have hxr : 0 < x ^ rp := rpow_pos_of_pos hx rp
  rw [div_lt_iff₀ hxr] at key
  rw [rpow_sub_one hx.ne']
  have e : (1 + rp * (y / x - 1)) * x ^ rp = x ^ rp + rp * (x ^ rp / x) * (y - x) := by
    field_simp
  linarith

theorem rpow_tangent_le {rp x y : ℝ} (hr0 : 0 < rp) (hr1 : rp < 1) (hx : 0 < x) (hy : 0 ≤ y) :
    y ^ rp ≤ x ^ rp + rp * x ^ (rp - 1) * (y - x) := by
  rcases eq_or_ne y x with h | h
  · subst h; simp
  · exact (rpow_tangent hr0 hr1 hx hy h).le

/-- Lemma 1, with `δ > 0`, that is `0 < ρ′ = λ/(δ + λ) < 1`. A path that leaves `Π = Π*`
at `a = (va, Π*)` right of `v*`, crosses it at `c = (vc, Π*)` left of `v*` and returns at
`a′ = (va′, Π*)` returns farther out, `va < va′`, so paths spiral out of `Qm`. Here
`A = Πh − Π*` and `B = Π* − Πl`. -/
theorem lemma1_spiral_out {rp va va' vc vS A B : ℝ} (hr0 : 0 < rp) (hr1 : rp < 1)
    (hA : 0 < A) (hB : 0 < B) (hc0 : 0 < vc) (hcS : vc < vS) (hS1 : vS < 1) (ha : va ≤ 1)
    (ha' : 0 ≤ va')
    (h17 : (1 - va) ^ rp * A + (1 - vc) ^ rp * B = (1 - vS) ^ rp * (A + B))
    (h20 : va' ^ rp * A + vc ^ rp * B = vS ^ rp * (A + B)) : va < va' := by
  have hP : 0 < 1 - vS := by linarith
  have hS0 : 0 < vS := by linarith
  have k1 : 0 < rp * (1 - vS) ^ (rp - 1) := mul_pos hr0 (rpow_pos_of_pos hP _)
  have k2 : 0 < rp * vS ^ (rp - 1) := mul_pos hr0 (rpow_pos_of_pos hS0 _)
  have tp := rpow_tangent_le hr0 hr1 hP (show 0 ≤ 1 - va by linarith)
  have tU := rpow_tangent hr0 hr1 hP (show 0 ≤ 1 - vc by linarith)
    (show 1 - vc ≠ 1 - vS by intro h; linarith)
  have tq := rpow_tangent_le hr0 hr1 hS0 ha'
  have tW := rpow_tangent hr0 hr1 hS0 hc0.le (ne_of_lt hcS)
  set kP := rp * (1 - vS) ^ (rp - 1)
  set kS := rp * vS ^ (rp - 1)
  have i1 : 0 < kP * (A * ((1 - va) - (1 - vS)) + B * (vS - vc)) := by
    have := mul_le_mul_of_nonneg_left tp hA.le
    have := mul_lt_mul_of_pos_left tU hB
    nlinarith
  have i2 : 0 < kS * (A * (va' - vS) - B * (vS - vc)) := by
    have := mul_le_mul_of_nonneg_left tq hA.le
    have := mul_lt_mul_of_pos_left tW hB
    nlinarith
  have j1 := (pos_iff_pos_of_mul_pos i1).1 k1
  have j2 := (pos_iff_pos_of_mul_pos i2).1 k2
  have : 0 < A * (va' - va) := by nlinarith
  nlinarith

/-- The remark closing the proof of Lemma 1. With `δ = 0`, `ρ′ = 1`, the path returns to
its starting point, `va′ = va`, so the paths are closed orbits. This is also the control
for Lemma 1, which fails without `δ > 0`. -/
theorem control_lemma1_vortex {va va' vc vS A B : ℝ} (hA : 0 < A)
    (h17 : (1 - va) ^ (1:ℝ) * A + (1 - vc) ^ (1:ℝ) * B = (1 - vS) ^ (1:ℝ) * (A + B))
    (h20 : va' ^ (1:ℝ) * A + vc ^ (1:ℝ) * B = vS ^ (1:ℝ) * (A + B)) : va' = va := by
  simp only [rpow_one] at h17 h20
  have : A * (va' - va) = 0 := by linear_combination h17 + h20
  have := (mul_eq_zero.1 this).resolve_left hA.ne'
  linarith

/-- `x ↦ x^(−r)` is convex on `(0, ∞)` for `r > 0`. -/
theorem convexOn_rpow_neg {rp : ℝ} (hr : 0 < rp) :
    ConvexOn ℝ (Ioi 0) (fun x : ℝ => x ^ (-rp)) := by
  apply MonotoneOn.convexOn_of_deriv (convex_Ioi 0)
  · intro x hx
    exact (hasDerivAt_rpow_const (p := -rp) (Or.inl (ne_of_gt hx))).continuousAt.continuousWithinAt
  · rw [interior_Ioi]; intro x hx
    exact (hasDerivAt_rpow_const (p := -rp)
      (Or.inl (ne_of_gt hx))).differentiableAt.differentiableWithinAt
  · rw [interior_Ioi]
    intro x hx y hy hxy
    rw [(hasDerivAt_rpow_const (p := -rp) (Or.inl (ne_of_gt hx))).deriv,
      (hasDerivAt_rpow_const (p := -rp) (Or.inl (ne_of_gt hy))).deriv]
    have : y ^ (-rp - 1) ≤ x ^ (-rp - 1) := rpow_le_rpow_of_nonpos hx hxy (by linarith)
    nlinarith

/-- `v ↦ (1 − v)^(−r)` is convex on `v < 1`. -/
theorem convexOn_one_sub_rpow_neg {rp : ℝ} (hr : 0 < rp) :
    ConvexOn ℝ (Iio 1) (fun v : ℝ => (1 - v) ^ (-rp)) := by
  refine ⟨convex_Iio 1, fun x hx y hy a b ha hb hab => ?_⟩
  have hx' : (1 - x) ∈ Ioi (0:ℝ) := mem_Ioi.2 (by linarith [mem_Iio.1 hx])
  have hy' : (1 - y) ∈ Ioi (0:ℝ) := mem_Ioi.2 (by linarith [mem_Iio.1 hy])
  have h := (convexOn_rpow_neg hr).2 hx' hy' ha hb hab
  simp only [smul_eq_mul] at h ⊢
  have e : 1 - (a * x + b * y) = a * (1 - x) + b * (1 - y) := by linear_combination (-1:ℝ) * hab
  rw [e]; exact h

theorem concave_of_convex_scale {S : Set ℝ} {f : ℝ → ℝ} (hf : ConvexOn ℝ S f) {C X : ℝ}
    (hC : C ≤ 0) : ConcaveOn ℝ S (fun v => X + C * f v) := by
  refine ⟨hf.1, fun x hx y hy a b ha hb hab => ?_⟩
  have h := hf.2 hx hy ha hb hab
  simp only [smul_eq_mul] at h ⊢
  have h2 : C * (a * f x + b * f y) ≤ C * f (a * x + b * y) := mul_le_mul_of_nonpos_left h hC
  calc a * (X + C * f x) + b * (X + C * f y) = X + C * (a * f x + b * f y) := by
        linear_combination X * hab
    _ ≤ _ := by linarith

theorem convex_of_convex_scale {S : Set ℝ} {f : ℝ → ℝ} (hf : ConvexOn ℝ S f) {C X : ℝ}
    (hC : 0 ≤ C) : ConvexOn ℝ S (fun v => X + C * f v) := by
  refine ⟨hf.1, fun x hx y hy a b ha hb hab => ?_⟩
  have h := hf.2 hx hy ha hb hab
  simp only [smul_eq_mul] at h ⊢
  have h2 : C * f (a * x + b * y) ≤ C * (a * f x + b * f y) := mul_le_mul_of_nonneg_left h hC
  calc X + C * f (a * x + b * y) ≤ X + C * (a * f x + b * f y) := by linarith
    _ = _ := by linear_combination (-X) * hab

/-- Lemma 2, right of `v*`. By `region_invariant` a path in region IV is
`Π = Πh + C v^{−ρ′}` and in region I `Π = Πh + C (1 − v)^{−ρ′}`, with `C ≤ 0` below `Πh`.
Both are concave. -/
theorem lemma2_right {rp C piH : ℝ} (hr : 0 < rp) (hC : C ≤ 0) :
    ConcaveOn ℝ (Ioi 0) (fun v => piH + C * v ^ (-rp)) ∧
      ConcaveOn ℝ (Iio 1) (fun v => piH + C * (1 - v) ^ (-rp)) :=
  ⟨concave_of_convex_scale (convexOn_rpow_neg hr) hC,
    concave_of_convex_scale (convexOn_one_sub_rpow_neg hr) hC⟩

/-- Lemma 2, left of `v*`. In regions II and III the paths are `Π = Πl + C (·)^{−ρ′}` with
`C ≥ 0` above `Πl`, and are convex. -/
theorem lemma2_left {rp C piL : ℝ} (hr : 0 < rp) (hC : 0 ≤ C) :
    ConvexOn ℝ (Ioi 0) (fun v => piL + C * v ^ (-rp)) ∧
      ConvexOn ℝ (Iio 1) (fun v => piL + C * (1 - v) ^ (-rp)) :=
  ⟨convex_of_convex_scale (convexOn_rpow_neg hr) hC,
    convex_of_convex_scale (convexOn_one_sub_rpow_neg hr) hC⟩

/-- Control for Lemma 2: the sign of `C` matters. With `C = 1` and `ρ′ = 1`, the curve
`Πh + v^{−1}` is not concave. -/
theorem control_lemma2_sign (piH : ℝ) :
    ¬ ConcaveOn ℝ (Ioi 0) (fun v : ℝ => piH + 1 * v ^ (-(1:ℝ))) := by
  intro h
  have := h.2 (show (1:ℝ) ∈ Ioi 0 by norm_num) (show (3:ℝ) ∈ Ioi 0 by norm_num)
    (show (0:ℝ) ≤ 1/2 by norm_num) (show (0:ℝ) ≤ 1/2 by norm_num) (by norm_num)
  simp only [smul_eq_mul, rpow_neg_one] at this
  norm_num at this
  linarith

/-- Proposition 3, workers. At `Qh` versus `Ql` the low-cost type gets `ω` against `ωPq`,
the high-cost type `ω(1 − Pu)` against `0`, the medium type `ω − (δ + λ)cm` against `0`. -/
theorem prop3_workers {ω Pq Pu K : ℝ} (hω : 0 < ω) (hq : Pq < 1) (hu : Pu < 1) (hK : K < ω) :
    ω * Pq < ω ∧ 0 < ω * (1 - Pu) ∧ 0 < ω - K :=
  ⟨by nlinarith, mul_pos hω (by linarith), by linarith⟩

/-- Control for Proposition 3: the medium type gains only if `(δ + λ)cm < ω`. -/
theorem control_prop3_medium {ω K : ℝ} (hK : ω ≤ K) : ¬ 0 < ω - K := by
  intro h; linarith

/-- P. 15. The lower boundary of the optimistic path, `π^o = Πh + (Π* − Πh) v*^{−ρ′}`. -/
noncomputable def piO (rp piS piH vS : ℝ) : ℝ := piH + (piS - piH) * vS ^ (-rp)

/-- P. 15. The upper boundary of the pessimistic path, `π^p = Πl + (Π* − Πl)(1 − v*)^{−ρ′}`. -/
noncomputable def piP (rp piS piL vS : ℝ) : ℝ := piL + (piS - piL) * (1 - vS) ^ (-rp)

/-- Definition 2, the overlap. -/
def Overlap (rp piS piL piH vS : ℝ) : Set ℝ := Icc (piO rp piS piH vS) (piP rp piS piL vS)

/-- Definition 3. `Ql` is a reputation trap if `Πl` lies below the overlap. -/
def IsTrap (rp piS piL piH vS : ℝ) : Prop := piL < piO rp piS piH vS

/-- P. 15. `π^o` is where the optimistic path through `(1, Π*)` meets `v = v*`, by the
region IV invariant. -/
theorem piO_on_path {rp piS piH vS : ℝ} (hv : 0 < vS) :
    vS ^ rp * (piO rp piS piH vS - piH) = (1:ℝ) ^ rp * (piS - piH) := by
  unfold piO
  rw [one_rpow, rpow_neg hv.le]
  have := (rpow_pos_of_pos hv rp).ne'
  field_simp; ring

/-- P. 15. `π^p` is where the pessimistic path through `(0, Π*)` meets `v = v*`, by the
region II invariant. -/
theorem piP_on_path {rp piS piL vS : ℝ} (hv : vS < 1) :
    (1 - vS) ^ rp * (piP rp piS piL vS - piL) = (1 - 0 : ℝ) ^ rp * (piS - piL) := by
  unfold piP
  have hp : 0 < 1 - vS := by linarith
  rw [sub_zero, one_rpow, rpow_neg hp.le]
  have := (rpow_pos_of_pos hp rp).ne'
  field_simp; ring

/-- Lemma 3, in the form that follows from the printed `π^o` and `π^p` (KLA-D2).
`π^o > Πl` iff `ρ′(−ln v*) < ln(Πh − Πl) − ln(Πh − Π*)`. -/
theorem lemma3_o {rp vS piS piL piH : ℝ} (hv0 : 0 < vS) (hLS : piL < piS) (hSH : piS < piH) :
    piL < piO rp piS piH vS ↔ rp * (-log vS) < log (piH - piL) - log (piH - piS) := by
  unfold piO
  have h1 : 0 < piH - piS := by linarith
  have hpos : 0 < (piH - piS) * vS ^ (-rp) := mul_pos h1 (rpow_pos_of_pos hv0 _)
  have e : piL < piH + (piS - piH) * vS ^ (-rp) ↔ (piH - piS) * vS ^ (-rp) < piH - piL := by
    constructor <;> intro h <;> linarith
  rw [e, ← log_lt_log_iff hpos (by linarith), log_mul h1.ne' (rpow_pos_of_pos hv0 _).ne',
    log_rpow hv0]
  constructor <;> intro h <;> linarith

/-- Lemma 3, the other half. `π^p < Πh` iff
`ρ′(−ln(1 − v*)) < ln(Πh − Πl) − ln(Π* − Πl)`. -/
theorem lemma3_p {rp vS piS piL piH : ℝ} (hv1 : vS < 1) (hLS : piL < piS) (hSH : piS < piH) :
    piP rp piS piL vS < piH ↔ rp * (-log (1 - vS)) < log (piH - piL) - log (piS - piL) := by
  unfold piP
  have hp : 0 < 1 - vS := by linarith
  have h1 : 0 < piS - piL := by linarith
  have hpos : 0 < (piS - piL) * (1 - vS) ^ (-rp) := mul_pos h1 (rpow_pos_of_pos hp _)
  have e : piL + (piS - piL) * (1 - vS) ^ (-rp) < piH ↔
      (piS - piL) * (1 - vS) ^ (-rp) < piH - piL := by
    constructor <;> intro h <;> linarith
  rw [e, ← log_lt_log_iff hpos (by linarith), log_mul h1.ne' (rpow_pos_of_pos hp _).ne',
    log_rpow hp]
  constructor <;> intro h <;> linarith

/-- Eq. (12), the size of the overlap. -/
theorem eq12 (rp piS piL piH vS : ℝ) :
    piP rp piS piL vS - piO rp piS piH vS
      = piL - piH + (piS - piL) * (1 - vS) ^ (-rp) + (piH - piS) * vS ^ (-rp) := by
  unfold piP piO; ring

/-- Proposition 4, the core. The overlap grows with `ρ′ = λ/(δ + λ)`. -/
theorem prop4_overlap_rp {r1 r2 vS piS piL piH : ℝ} (hv0 : 0 < vS) (hv1 : vS < 1)
    (hLS : piL < piS) (hSH : piS < piH) (h12 : r1 < r2) :
    piP r1 piS piL vS - piO r1 piS piH vS < piP r2 piS piL vS - piO r2 piS piH vS := by
  rw [eq12, eq12]
  have a1 : vS ^ (-r1) < vS ^ (-r2) := rpow_lt_rpow_of_exponent_gt hv0 hv1 (by linarith)
  have a2 : (1 - vS) ^ (-r1) < (1 - vS) ^ (-r2) :=
    rpow_lt_rpow_of_exponent_gt (by linarith) (by linarith) (by linarith)
  have := mul_lt_mul_of_pos_left a1 (show 0 < piH - piS by linarith)
  have := mul_lt_mul_of_pos_left a2 (show 0 < piS - piL by linarith)
  linarith

/-- Proposition 4 in `δ`. A larger `δ` gives a smaller overlap. -/
theorem prop4_delta {δ1 δ2 l vS piS piL piH : ℝ} (hl : 0 < l) (hδ : 0 < δ1) (h12 : δ1 < δ2)
    (hv0 : 0 < vS) (hv1 : vS < 1) (hLS : piL < piS) (hSH : piS < piH) :
    piP (l / (δ2 + l)) piS piL vS - piO (l / (δ2 + l)) piS piH vS
      < piP (l / (δ1 + l)) piS piL vS - piO (l / (δ1 + l)) piS piH vS :=
  prop4_overlap_rp hv0 hv1 hLS hSH (div_lt_div_of_pos_left hl (by linarith) (by linarith))

/-- Proposition 4 in `λ`. A larger `λ` gives a larger overlap. -/
theorem prop4_lambda {δ l1 l2 vS piS piL piH : ℝ} (hδ : 0 < δ) (hl : 0 < l1) (h12 : l1 < l2)
    (hv0 : 0 < vS) (hv1 : vS < 1) (hLS : piL < piS) (hSH : piS < piH) :
    piP (l1 / (δ + l1)) piS piL vS - piO (l1 / (δ + l1)) piS piH vS
      < piP (l2 / (δ + l2)) piS piL vS - piO (l2 / (δ + l2)) piS piH vS := by
  apply prop4_overlap_rp hv0 hv1 hLS hSH
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]; nlinarith

/-- Proposition 4 in `cm`. A larger cost raises `v*`, and with it both `π^o` and `π^p`. -/
theorem prop4_cm {rp K1 K2 ω Pq Pu piS piL piH : ℝ} (hr : 0 < rp) (hd : 0 < ω * (Pu - Pq))
    (h12 : K1 < K2) (hv0 : 0 < vStar K1 ω Pq Pu) (hv1 : vStar K2 ω Pq Pu < 1)
    (hLS : piL < piS) (hSH : piS < piH) :
    piO rp piS piH (vStar K1 ω Pq Pu) < piO rp piS piH (vStar K2 ω Pq Pu) ∧
      piP rp piS piL (vStar K1 ω Pq Pu) < piP rp piS piL (vStar K2 ω Pq Pu) := by
  have hv : vStar K1 ω Pq Pu < vStar K2 ω Pq Pu := by
    unfold vStar; exact div_lt_div_of_pos_right (by linarith) hd
  constructor
  · unfold piO
    have := rpow_lt_rpow_of_neg hv0 hv (show -rp < 0 by linarith)
    have := mul_lt_mul_of_pos_left this (show 0 < piH - piS by linarith)
    nlinarith
  · unfold piP
    have := rpow_lt_rpow_of_neg (show 0 < 1 - vStar K2 ω Pq Pu by linarith)
      (show 1 - vStar K2 ω Pq Pu < 1 - vStar K1 ω Pq Pu by linarith) (show -rp < 0 by linarith)
    have := mul_lt_mul_of_pos_left this (show 0 < piS - piL by linarith)
    linarith

/-- Control for Proposition 4: with `Π*` at both `Πl` and `Πh` the overlap does not move
with `ρ′`. -/
theorem control_prop4_degenerate (r1 r2 vS p : ℝ) :
    piP r1 p p vS - piO r1 p p vS = piP r2 p p vS - piO r2 p p vS := by
  rw [eq12, eq12]; ring

/-! ### Section 3.4. The general model, linearised -/

/-- Proof of Lemma 4. The characteristic polynomial of the Jacobian is
`x² − δx + λ(δ + λ)(β′φ′ − 1)`. -/
theorem jacobian_charpoly (δ l b f x : ℝ) :
    Matrix.det (!![δ + l, -(δ + l) * b; l * f, -l] - x • (1 : Matrix (Fin 2) (Fin 2) ℝ))
      = x ^ 2 - δ * x + l * (δ + l) * (b * f - 1) := by
  simp [Matrix.det_fin_two]; ring

/-- Lemma 4, saddles. Real roots with sum `δ` and negative product have opposite signs. -/
theorem lemma4_saddle {δ D r1 r2 : ℝ} (hsum : r1 + r2 = δ) (hprod : r1 * r2 = D) (hD : D < 0) :
    (r1 < 0 ∧ 0 < r2) ∨ (r2 < 0 ∧ 0 < r1) := by
  rcases lt_trichotomy r1 0 with h | h | h
  · left; exact ⟨h, by nlinarith⟩
  · subst h; simp at hprod; linarith
  · right; exact ⟨by nlinarith, h⟩

/-- Lemma 4, the source. Real roots with sum `δ > 0` and positive product are both positive.
Complex roots have real part `δ/2 > 0`. -/
theorem lemma4_source {δ D r1 r2 : ℝ} (hδ : 0 < δ) (hsum : r1 + r2 = δ) (hprod : r1 * r2 = D)
    (hD : 0 < D) : 0 < r1 ∧ 0 < r2 := by
  rcases lt_or_ge r1 0 with h | h
  · exfalso; nlinarith
  · rcases eq_or_lt_of_le h with h0 | h0
    · subst h0; simp at hprod; linarith
    · exact ⟨h0, by nlinarith⟩

/-- Lemma 4. The determinant is negative iff `∂β/∂Π < (∂φ/∂V)^{−1}`, here `β′φ′ < 1`. -/
theorem lemma4_det_neg_iff {δ l b f : ℝ} (hl : 0 < l) (hs : 0 < δ + l) :
    l * (δ + l) * (b * f - 1) < 0 ↔ b * f < 1 := by
  have h := mul_pos hl hs
  constructor
  · intro h1; by_contra h2; push_neg at h2; nlinarith
  · intro h1; nlinarith

/-- Control for Lemma 4: with a positive determinant the roots are not of opposite sign. -/
theorem control_lemma4 {δ D r1 r2 : ℝ} (hδ : 0 < δ) (hsum : r1 + r2 = δ) (hprod : r1 * r2 = D)
    (hD : 0 < D) : ¬ ((r1 < 0 ∧ 0 < r2) ∨ (r2 < 0 ∧ 0 < r1)) := by
  have := lemma4_source hδ hsum hprod hD
  rintro (⟨h, -⟩ | ⟨h, -⟩) <;> linarith [this.1, this.2]

/-- Lemma 5, the roots `r = (δ ± √(δ² − 4λ(δ + λ)[β′φ′ − 1]))/2` solve the characteristic
equation when the discriminant is nonnegative. -/
theorem lemma5_roots {δ l k : ℝ} (hD : 0 ≤ δ ^ 2 - 4 * l * (δ + l) * (k - 1)) :
    let r := (δ + √(δ ^ 2 - 4 * l * (δ + l) * (k - 1))) / 2
    r ^ 2 - δ * r + l * (δ + l) * (k - 1) = 0 := by
  intro r
  have := sq_sqrt hD
  simp only [r]; nlinarith

/-- Control for Lemma 5: with `β′φ′ ≤ 1` the discriminant is nonnegative, so no spiral. -/
theorem control_lemma5 {δ l k : ℝ} (hl : 0 ≤ l) (hδ : 0 ≤ δ) (hk : k ≤ 1) :
    0 ≤ δ ^ 2 - 4 * l * (δ + l) * (k - 1) := by
  have : 0 ≤ l * (δ + l) * (1 - k) := mul_nonneg (mul_nonneg hl (by linarith)) (by linarith)
  nlinarith [sq_nonneg δ]

/-! ### Section 5. Monopolistic principals -/

/-- P. 19. The lowered threshold `Π*′ = Πh − (Πh − Πl) v*^{ρ′}`. -/
noncomputable def piSAdj (rp piL piH vS : ℝ) : ℝ := piH - (piH - piL) * vS ^ rp

/-- P. 19. With threshold `Π*′`, the optimistic path through `(1, Π*′)` passes through
`(v*, Πl)`, by the region IV invariant, so a trapped group can jump onto it. -/
theorem prop5_threshold_path (rp piL piH vS : ℝ) :
    vS ^ rp * (piL - piH) = (1:ℝ) ^ rp * (piSAdj rp piL piH vS - piH) := by
  unfold piSAdj; rw [one_rpow]; ring

/-- P. 19. The time `t_b − t_0 = −ln v*/(δ + λ)` takes `v` from `v*` to `1` in region IV. -/
theorem prop5_tb {s vS : ℝ} (hs : 0 < s) (hv : 0 < vS) : vS * exp (s * (-log vS / s)) = 1 := by
  rw [show s * (-log vS / s) = -log vS by field_simp, exp_neg, exp_log hv]
  exact mul_inv_cancel₀ hv.ne'

/-- P. 19. The subsidised cutoff `v*′ = ((Πh − Π*)/(Πh − Πl))^{(δ+λ)/λ}`. -/
noncomputable def vSAdj (rp piS piL piH : ℝ) : ℝ := ((piH - piS) / (piH - piL)) ^ rp⁻¹

/-- P. 19. With cutoff `v*′`, the optimistic path through `(1, Π*)` passes through
`(v*′, Πl)`. -/
theorem prop5_subsidy_path {rp piS piL piH : ℝ} (hr : rp ≠ 0) (hLS : piL < piS)
    (hSH : piS < piH) :
    vSAdj rp piS piL piH ^ rp * (piL - piH) = (1:ℝ) ^ rp * (piS - piH) := by
  unfold vSAdj
  rw [rpow_inv_rpow (div_nonneg (by linarith) (by linarith)) hr, one_rpow]
  have : piH - piL ≠ 0 := by linarith
  field_simp; ring

/-- P. 19. The subsidy that moves the cutoff from `v*` to `v*′` is
`S′ = cm − cm′ = (v* − v*′) ω(Pu − Pq)/(δ + λ)`. -/
theorem prop5_subsidy_size {s cm cm' ω Pq Pu : ℝ} (hs : 0 < s) (hω : ω * (Pu - Pq) ≠ 0) :
    cm - cm' = (vStar (s * cm) ω Pq Pu - vStar (s * cm') ω Pq Pu) * (ω * (Pu - Pq)) / s := by
  unfold vStar
  rw [div_sub_div_same, div_mul_cancel₀ _ hω]
  field_simp; ring

/-- P. 20. Reputation reaches `Π*` at `t_c = (1/λ) ln((Πh − Πl)/(Πh − Π*)) + t_0`. -/
theorem prop5_tc {l t0 piS piL piH : ℝ} (hl : 0 < l) (hLS : piL < piS) (hSH : piS < piH) :
    piH - (piH - piL) * exp (-(l * ((1 / l * log ((piH - piL) / (piH - piS)) + t0) - t0)))
      = piS := by
  have e : l * ((1 / l * log ((piH - piL) / (piH - piS)) + t0) - t0)
      = log ((piH - piL) / (piH - piS)) := by field_simp; ring
  rw [e, exp_neg, exp_log (div_pos (by linarith) (by linarith)), inv_div]
  have : piH - piL ≠ 0 := by linarith
  field_simp; ring

/-- P. 20. The cost of the subsidy, `TC = ∫_{t0}^{tb} S′ λ e^{−r(τ−t0)} dτ
= S′ λ (1 − e^{−r(tb−t0)})/r`. -/
theorem prop5_TC {S l rr t0 tb : ℝ} (hr : rr ≠ 0) :
    ∫ τ in t0..tb, S * l * exp (-(rr * (τ - t0)))
      = S * l * ((1 - exp (-(rr * (tb - t0)))) / rr) := by
  have hF : ∀ x ∈ uIcc t0 tb, HasDerivAt (fun τ => -(S * l) / rr * exp (-(rr * (τ - t0))))
      (S * l * exp (-(rr * (x - t0)))) x := by
    intro x _
    have := ((((hasDerivAt_id' x).sub_const t0).const_mul rr).neg.exp).const_mul (-(S * l) / rr)
    convert this using 1; simp only [Pi.neg_apply]; field_simp
  rw [intervalIntegral.integral_eq_sub_of_hasDerivAt hF
    (by apply Continuous.intervalIntegrable; fun_prop)]
  simp only [sub_self, mul_zero, neg_zero, exp_zero]
  field_simp; ring

/-- P. 18. Profit from a trapped group, `Y_{Ql} = ∫_{t0}^∞ Pq Πl xq e^{−r(τ−t0)} dτ = Pq Πl xq/r`. -/
theorem YQl_eq {rr : ℝ} (hr : 0 < rr) (Pq piL xq t0 : ℝ) :
    ∫ τ in Ioi t0, Pq * piL * xq * exp (-(rr * (τ - t0))) = Pq * piL * xq / rr :=
  footnote2 hr _ t0

/-- Proposition 5, as the coordinated employers' choice. A strategy with discounted profit
`Y` is taken over staying at `Ql` iff `Y ≥ Y_{Ql}`. -/
theorem prop5 (Y YQl : ℝ) : max Y YQl = Y ↔ YQl ≤ Y := max_eq_left_iff

/-! ### Appendix A. Market learning -/

/-- Appendix A. The lifetime value of the wage after revelation,
`∫_t^∞ (1 − e^{−η(τ−t)}) w e^{−(δ+λ)(τ−t)} dτ = ηw/((δ + λ)(δ + λ + η))`. -/
theorem appA_revelation_value {s η : ℝ} (hs : 0 < s) (hη : 0 ≤ η) (w t : ℝ) :
    ∫ τ in Ioi t, (1 - exp (-(η * (τ - t)))) * w * exp (-(s * (τ - t)))
      = η * w / (s * (s + η)) := by
  have e : ∀ τ, (1 - exp (-(η * (τ - t)))) * w * exp (-(s * (τ - t)))
      = w * exp (-(s * (τ - t))) - w * exp (-((s + η) * (τ - t))) := by
    intro τ
    have : exp (-(η * (τ - t))) * exp (-(s * (τ - t))) = exp (-((s + η) * (τ - t))) := by
      rw [← exp_add]; congr 1; ring
    linear_combination (-w) * this
  simp_rw [e]
  have hs' : 0 < s + η := by linarith
  have i1 : IntegrableOn (fun τ => w * exp (-(s * (τ - t)))) (Ioi t) := by
    have hf : (fun τ => w * exp (-(s * (τ - t))))
        = fun τ => (w * exp (s * t)) * exp ((-s) * τ) := by
      funext τ; rw [mul_assoc, ← exp_add]; congr 2; ring
    rw [hf]
    exact Integrable.const_mul (integrableOn_exp_mul_Ioi (by linarith) t) _
  have i2 : IntegrableOn (fun τ => w * exp (-((s + η) * (τ - t)))) (Ioi t) := by
    have hf : (fun τ => w * exp (-((s + η) * (τ - t))))
        = fun τ => (w * exp ((s + η) * t)) * exp ((-(s + η)) * τ) := by
      funext τ; rw [mul_assoc, ← exp_add]; congr 2; ring
    rw [hf]
    exact Integrable.const_mul (integrableOn_exp_mul_Ioi (by linarith) t) _
  have I1 := footnote2 hs w t
  have I2 := footnote2 hs' w t
  rw [integral_sub i1 i2, I1, I2]
  field_simp
  ring

/-- Appendix A. The `V̇ = 0` locus with market learning. -/
noncomputable def Vlocus (s η ω β : ℝ) : ℝ := (s * β + η * ω) / (s + η)

/-- Appendix A. The locus shifts right as `η` rises, `∂Ṽ/∂η > 0`, for every `β < ω`. -/
theorem appA_locus_shifts {s η1 η2 ω β : ℝ} (hs : 0 < s) (h0 : 0 ≤ η1) (h12 : η1 < η2)
    (hβ : β < ω) : Vlocus s η1 ω β < Vlocus s η2 ω β := by
  unfold Vlocus
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]
  nlinarith [mul_pos (mul_pos hs (sub_pos.2 h12)) (sub_pos.2 hβ)]

/-- Proof of Appendix A. The cutoff with market learning,
`v*′ = ((δ+λ+η)(δ+λ)cm − (δ+λ)ωPq − ηω)/((δ+λ)(ωPu − ωPq))`, with `K = (δ + λ)cm`. -/
noncomputable def vSeta (s η K ω Pq Pu : ℝ) : ℝ :=
  ((s + η) * K - s * ω * Pq - η * ω) / (s * (ω * Pu - ω * Pq))

/-- Appendix A. `∂π^p/∂η < 0`. With `π^p = Πl + (Π* − Πl)(1 − v*′)^{−λ/(δ+λ+η)}`, faster
learning lowers the top of the overlap, so the deterministic range for `Qh` expands. -/
theorem appA_piP_decreasing {s l η1 η2 K ω Pq Pu piS piL : ℝ} (hs : 0 < s) (hl : 0 < l)
    (h0 : 0 ≤ η1) (h12 : η1 < η2) (hK : K < ω) (hden : 0 < ω * Pu - ω * Pq)
    (hv2 : 0 < vSeta s η2 K ω Pq Pu) (hv1 : vSeta s η1 K ω Pq Pu < 1) (hLS : piL < piS) :
    piP (l / (s + η2)) piS piL (vSeta s η2 K ω Pq Pu)
      < piP (l / (s + η1)) piS piL (vSeta s η1 K ω Pq Pu) := by
  have hv : vSeta s η2 K ω Pq Pu < vSeta s η1 K ω Pq Pu := by
    unfold vSeta
    apply div_lt_div_of_pos_right _ (mul_pos hs hden)
    nlinarith
  have hr : l / (s + η2) < l / (s + η1) := div_lt_div_of_pos_left hl (by linarith) (by linarith)
  have hr2 : 0 < l / (s + η2) := div_pos hl (by linarith)
  set x1 := 1 - vSeta s η1 K ω Pq Pu
  set x2 := 1 - vSeta s η2 K ω Pq Pu
  have hx1 : 0 < x1 := by simp only [x1]; linarith
  have hx12 : x1 < x2 := by simp only [x1, x2]; linarith
  have hx2 : x2 < 1 := by simp only [x2]; linarith
  have a1 : x2 ^ (-(l / (s + η2))) < x1 ^ (-(l / (s + η2))) :=
    rpow_lt_rpow_of_neg hx1 hx12 (by linarith)
  have a2 : x1 ^ (-(l / (s + η2))) < x1 ^ (-(l / (s + η1))) :=
    rpow_lt_rpow_of_exponent_gt hx1 (by linarith) (by linarith)
  unfold piP
  have := mul_lt_mul_of_pos_left (a1.trans a2) (show 0 < piS - piL by linarith)
  linarith

end Full

end KimLoury2009
