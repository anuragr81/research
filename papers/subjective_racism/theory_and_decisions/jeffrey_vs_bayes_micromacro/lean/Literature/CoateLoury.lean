/-
# Coate & Loury (1993), "Will Affirmative-Action Policies Eliminate Negative Stereotypes?"

*American Economic Review* 83(5), 1993, 1220-1240.

Formalization of the paper's own model and results, not of Paper B's claims.

## The paper's model (Section I, pp. 1223-1226)

* Two identifiable groups B, W (fraction `λ` of W's).  A worker is qualified for
  task one or not (binary); qualification requires a costly, unobservable
  investment with cost `c ~ G`.
* The employer observes group identity and a signal `θ ∈ [0,1]` with densities
  `f_q`, `f_u`; `φ(θ) = f_u(θ)/f_q(θ)` is nonincreasing (MLRP).
* **Bayes' Rule** (eq. 1, p. 1224): a worker from a group believed qualified with
  probability `π` who emits `θ` is qualified with posterior
  `ξ(π, θ) = π f_q / (π f_q + (1-π) f_u) = 1 / {1 + [(1-π)/π] φ(θ)}`.
* Assign to task one iff `ξ x_q - (1-ξ) x_u ≥ 0`, i.e. iff `r ≥ [(1-π)/π] φ(θ)`,
  `r = x_q/x_u`; the standard is `s*(π) = min{θ ∈ [0,1] | r ≥ [(1-π)/π] φ(θ)}`
  (eq. 2), decreasing in `π`.
* Workers facing standard `s` gain `β(s) = ω[F_u(s) - F_q(s)]` from investing, so a
  fraction `G(β(s))` invests.
* **Definition 1** (p. 1225): an equilibrium is a pair of beliefs with
  `π_i = G(β(s*(π_i)))`, `i = b, w` (eq. 3).  "A discriminatory equilibrium (say,
  one with `π_b < π_w`) can occur whenever (3) has multiple solutions."
* **Stereotype** (p. 1221): "Employers form beliefs about the correlation between
  group identity and productivity which, in the equilibria of our model, must be
  correct.  If workers in one group are seen as less productive, we say that
  employers have negative stereotypes about that group."
* **Proposition 1** (p. 1226): with `φ` continuous, strictly decreasing, strictly
  positive, `G` continuous with `G(0) = 0`, if `G(β(s)) > φ(s)/[r + φ(s)]` for some
  `s ∈ (0,1)`, then (3) has at least two nonzero solutions.
* **Proposition 2** (p. 1229): under affirmative action, if `ρ̂` is decreasing,
  all equilibria entail homogeneous beliefs.
* **Section II.B example** (pp. 1230-1232): uniform costs; a three-outcome test
  with `p_q = (θ_u - θ_q)/(1 - θ_q)`, `p_u = (θ_u - θ_q)/θ_u`, `φ = p_u/p_q` on the
  unclear range; the benefit of the doubt iff `π ≥ π̂ = φ/(r + φ)`;
  `π_ℓ = ω(1 - p_u)`, `π_c = ω(1 - p_q)`; eq. (6); under affirmative action
  `α(π_b) = (π_ℓ - π_b)/(1 - π_b)` (7), equilibrium beliefs about B's solve
  `π_b = [(1 - π_ℓ)/(1 - π_b)] π_ℓ` (8), and the adjustment process (9) converges
  to the patronizing belief `1 - π_ℓ` when `π_ℓ > 1/2` (**Proposition 3**).

## What is formalized

* `posterior_eq_odds` (eq. 1), `posterior_strictMono` (posterior rises with the
  prior), `assign_iff` (the task-one rule), `assign_iff_threshold` (the same rule
  as `π ≥ φ/(r+φ)`).
* `acceptSet_mono`, `threshold_antitone` (s* decreasing in π, from eq. 2),
  `acceptSet_upper` (MLRP makes the acceptance set an upper interval, so a
  threshold rule is optimal), `isLeast_of_EE` (on the EE curve `π = φ(s)/(r+φ(s))`
  the standard is exactly `s`).
* `cov_group_qualified`, `negativeStereotype_iff_cov_pos`: the p. 1221 stereotype
  as a believed positive covariance between being W and being qualified.
* `prop1_two_equilibria`: **Proposition 1**, two distinct nonzero self-confirming
  beliefs, via the intermediate value theorem.
* `smooth_example`: an explicit smooth instance (`φ(θ) = (2-θ)/(1+θ)`, uniform
  costs) with self-confirming beliefs `1/2` and `4/9` -- a discriminatory
  equilibrium between ex ante identical groups.
* `prop2_homogeneous`: **Proposition 2** (it needs `ρ̂` *strictly* decreasing, i.e.
  injective).
* Example II.B: `pihat_eq6` (the middle term of eq. 6 is `π̂`),
  `example_selfConfirming` (both `π_c` and `π_ℓ` self-confirming, and nothing
  else), `alpha_eq7`, `eq8_solutions`, `lambdaHat_iff` (footnote 20),
  `patronSeq_tendsto` (**Proposition 3**'s dynamics: convergence to `1 - π_ℓ`),
  `colorBlind_unstable` (the color-blind root has slope `π_ℓ/(1-π_ℓ) > 1`),
  `stereotype_worsens`, and `fn21_instance` (a concrete parameter point from
  footnote 21).
-/
import Mathlib

namespace Literature.CoateLoury

open Filter Topology Set

/-! ## Bayes' rule and the employer's standard (eqs. 1-2, p. 1224) -/

/-- Eq. (1): the posterior probability of qualification. -/
noncomputable def posterior (π fq fu : ℝ) : ℝ := π * fq / (π * fq + (1 - π) * fu)

/-- **Eq. (1)**, second form: `ξ = 1 / {1 + [(1-π)/π] φ}` with `φ = f_u/f_q`. -/
theorem posterior_eq_odds {π fq fu : ℝ} (hπ : 0 < π) (hπ1 : π < 1) (hfq : 0 < fq)
    (hfu : 0 ≤ fu) : posterior π fq fu = 1 / (1 + ((1 - π) / π) * (fu / fq)) := by
  unfold posterior
  have h1 : 0 < 1 - π := by linarith
  field_simp

/-- A more optimistic prior gives a higher posterior at every signal with
`f_q, f_u > 0`. -/
theorem posterior_strictMono {fq fu : ℝ} (hfq : 0 < fq) (hfu : 0 < fu) {π₁ π₂ : ℝ}
    (h0 : 0 ≤ π₁) (h12 : π₁ < π₂) (h1 : π₂ ≤ 1) :
    posterior π₁ fq fu < posterior π₂ fq fu := by
  unfold posterior
  have d1 : 0 < π₁ * fq + (1 - π₁) * fu := by nlinarith
  have d2 : 0 < π₂ * fq + (1 - π₂) * fu := by nlinarith
  rw [div_lt_div_iff₀ d1 d2]
  nlinarith [mul_pos hfq hfu]

/-- **The task-one rule** (p. 1224).  With `x_q, x_u > 0` and `r = x_q/x_u`, the
expected payoff `ξ x_q - (1-ξ) x_u` is nonnegative iff `r ≥ [(1-π)/π] φ`. -/
theorem assign_iff {π fq fu xq xu : ℝ} (hπ : 0 < π) (hπ1 : π < 1) (hfq : 0 < fq)
    (hfu : 0 ≤ fu) (hxu : 0 < xu) :
    0 ≤ posterior π fq fu * xq - (1 - posterior π fq fu) * xu ↔
      ((1 - π) / π) * (fu / fq) ≤ xq / xu := by
  unfold posterior
  have h1 : 0 < 1 - π := by linarith
  have d : 0 < π * fq + (1 - π) * fu := by nlinarith
  have e : π * fq / (π * fq + (1 - π) * fu) * xq - (1 - π * fq / (π * fq + (1 - π) * fu)) * xu
      = (π * fq * xq - (1 - π) * fu * xu) / (π * fq + (1 - π) * fu) := by
    field_simp; ring
  rw [e, div_nonneg_iff]
  constructor
  · rintro (⟨h, -⟩ | ⟨-, h⟩)
    · rw [div_mul_div_comm, div_le_div_iff₀ (by positivity) hxu]; nlinarith
    · linarith
  · intro h
    left
    refine ⟨?_, d.le⟩
    rw [div_mul_div_comm, div_le_div_iff₀ (by positivity) hxu] at h
    nlinarith

/-- The same rule as a cut-off in the prior: `r ≥ [(1-π)/π] φ ⟺ π ≥ φ/(r + φ)`.
In Example II.B this is the "benefit of the doubt" condition `π ≥ π̂`. -/
theorem assign_iff_threshold {π φ r : ℝ} (hπ : 0 < π) (hφ : 0 ≤ φ) (hr : 0 < r) :
    ((1 - π) / π) * φ ≤ r ↔ φ / (r + φ) ≤ π := by
  rw [div_mul_eq_mul_div, div_le_iff₀ hπ, div_le_iff₀ (by linarith)]
  constructor <;> intro h <;> nlinarith

/-- The acceptance set of eq. (2): signals at which a worker from a group with
prior `π` is assigned to task one. -/
def acceptSet (φ : ℝ → ℝ) (r π : ℝ) : Set ℝ := {θ | θ ∈ Icc (0 : ℝ) 1 ∧ ((1 - π) / π) * φ θ ≤ r}

/-- More optimistic beliefs enlarge the acceptance set. -/
theorem acceptSet_mono {φ : ℝ → ℝ} {r π₁ π₂ : ℝ} (hφ : ∀ θ, 0 ≤ φ θ) (h0 : 0 < π₁)
    (h12 : π₁ ≤ π₂) : acceptSet φ r π₁ ⊆ acceptSet φ r π₂ := by
  rintro θ ⟨hθ, h⟩
  refine ⟨hθ, le_trans ?_ h⟩
  have h2 : 0 < π₂ := lt_of_lt_of_le h0 h12
  have : (1 - π₂) / π₂ ≤ (1 - π₁) / π₁ := by
    rw [div_le_div_iff₀ h2 h0]; nlinarith
  exact mul_le_mul_of_nonneg_right this (hφ θ)

/-- **`s*(·)` is decreasing** (p. 1225: "More optimistic beliefs about a group
will be reflected in easier standards, since `s*(·)` is decreasing in `π`"). -/
theorem threshold_antitone {φ : ℝ → ℝ} {r π₁ π₂ s₁ s₂ : ℝ} (hφ : ∀ θ, 0 ≤ φ θ)
    (h0 : 0 < π₁) (h12 : π₁ ≤ π₂) (hs₁ : IsLeast (acceptSet φ r π₁) s₁)
    (hs₂ : IsLeast (acceptSet φ r π₂) s₂) : s₂ ≤ s₁ :=
  hs₂.2 (acceptSet_mono hφ h0 h12 hs₁.1)

/-- Under MLRP (`φ` nonincreasing on `[0,1]`) the acceptance set is an upper
interval of `[0,1]`: the optimal policy is a threshold rule. -/
theorem acceptSet_upper {φ : ℝ → ℝ} {r π : ℝ} (hπ : 0 < π) (hπ1 : π ≤ 1)
    (hanti : AntitoneOn φ (Icc 0 1)) {θ θ' : ℝ} (hθ : θ ∈ acceptSet φ r π) (hle : θ ≤ θ')
    (h1 : θ' ≤ 1) : θ' ∈ acceptSet φ r π := by
  obtain ⟨hθI, h⟩ := hθ
  have hθ'I : θ' ∈ Icc (0 : ℝ) 1 := ⟨le_trans hθI.1 hle, h1⟩
  refine ⟨hθ'I, le_trans ?_ h⟩
  have hk : 0 ≤ (1 - π) / π := div_nonneg (by linarith) hπ.le
  exact mul_le_mul_of_nonneg_left (hanti hθI hθ'I hle) hk

/-- **The EE locus** (proof of Proposition 1, p. 1226).  With `φ` strictly
decreasing and positive on `[0,1]` and `r > 0`, at the belief
`π = φ(s)/(r + φ(s))` the standard of eq. (2) is exactly `s`. -/
theorem isLeast_of_EE {φ : ℝ → ℝ} {r s : ℝ} (hr : 0 < r)
    (hanti : StrictAntiOn φ (Icc 0 1)) (hpos : ∀ θ ∈ Icc (0 : ℝ) 1, 0 < φ θ)
    (hs : s ∈ Icc (0 : ℝ) 1) :
    IsLeast (acceptSet φ r (φ s / (r + φ s))) s := by
  have hφs := hpos s hs
  have hk : (1 - φ s / (r + φ s)) / (φ s / (r + φ s)) = r / φ s := by
    field_simp; ring
  refine ⟨⟨hs, by rw [hk]; field_simp; rfl⟩, ?_⟩
  rintro θ ⟨hθ, h⟩
  rw [hk] at h
  by_contra hlt
  have := hanti hθ hs (not_le.mp hlt)
  rw [div_mul_eq_mul_div, div_le_iff₀ hφs] at h
  nlinarith

/-! ## The stereotype as a believed correlation (p. 1221) -/

/-- Believed covariance between the indicators "is W" and "is qualified", for a
population with W-share `λ` and believed qualification rates `π_b`, `π_w`:
`P(W ∧ q) - P(W) P(q)`. -/
def covWQ (lam πb πw : ℝ) : ℝ := lam * πw - lam * (lam * πw + (1 - lam) * πb)

theorem cov_group_qualified (lam πb πw : ℝ) : covWQ lam πb πw = lam * (1 - lam) * (πw - πb) := by
  unfold covWQ; ring

/-- A negative stereotype about B's (`π_b < π_w`, p. 1221) is exactly a believed
positive correlation between group identity W and productivity. -/
theorem negativeStereotype_iff_cov_pos {lam πb πw : ℝ} (h0 : 0 < lam) (h1 : lam < 1) :
    πb < πw ↔ 0 < covWQ lam πb πw := by
  rw [cov_group_qualified]
  have : 0 < lam * (1 - lam) := mul_pos h0 (by linarith)
  constructor
  · intro h; exact mul_pos this (by linarith)
  · intro h; by_contra hc
    nlinarith [not_lt.mp hc]

/-! ## Proposition 1: multiple self-confirming beliefs (p. 1226) -/

/-- **Proposition 1.**  Let `φ` be continuous, strictly decreasing and strictly
positive on `[0,1]`, `r > 0`, and let `WW(s) = G(β(s))` be continuous on `[0,1]`
and vanish at `0` and `1` (as `β(0) = β(1) = 0`, `G(0) = 0`).  If
`WW(s₀) > φ(s₀)/[r + φ(s₀)]` at some `s₀ ∈ (0,1)`, there are standards
`0 < s₁ < s₀ < s₂ < 1` whose EE beliefs `π_i = φ(s_i)/(r + φ(s_i))` are distinct,
lie in `(0,1)`, have `s_i` as their optimal standard, and are self-confirming:
`π_i = WW(s*(π_i))`.  Assigning one to each group gives a discriminatory
equilibrium. -/
theorem prop1_two_equilibria {φ WW : ℝ → ℝ} {r s₀ : ℝ} (hr : 0 < r)
    (hφc : ContinuousOn φ (Icc 0 1)) (hanti : StrictAntiOn φ (Icc 0 1))
    (hpos : ∀ θ ∈ Icc (0 : ℝ) 1, 0 < φ θ) (hWc : ContinuousOn WW (Icc 0 1))
    (hW0 : WW 0 = 0) (hW1 : WW 1 = 0) (hs₀ : s₀ ∈ Ioo (0 : ℝ) 1)
    (hgt : φ s₀ / (r + φ s₀) < WW s₀) :
    ∃ s₁ s₂ : ℝ, 0 < s₁ ∧ s₁ < s₀ ∧ s₀ < s₂ ∧ s₂ < 1 ∧
      φ s₂ / (r + φ s₂) < φ s₁ / (r + φ s₁) ∧
      ∀ s ∈ ({s₁, s₂} : Set ℝ),
        0 < φ s / (r + φ s) ∧ φ s / (r + φ s) < 1 ∧
        IsLeast (acceptSet φ r (φ s / (r + φ s))) s ∧ φ s / (r + φ s) = WW s := by
  set EE : ℝ → ℝ := fun s => φ s / (r + φ s) with hEE
  set h : ℝ → ℝ := fun s => WW s - EE s with hh
  have hEEc : ContinuousOn EE (Icc 0 1) := by
    refine hφc.div (continuousOn_const.add hφc) fun x hx => ?_
    have := hpos x hx; linarith
  have hc : ContinuousOn h (Icc 0 1) := hWc.sub hEEc
  have hEEpos : ∀ s ∈ Icc (0 : ℝ) 1, 0 < EE s := fun s hs => by
    have := hpos s hs; simp only [hEE]; positivity
  have hEElt : ∀ s ∈ Icc (0 : ℝ) 1, EE s < 1 := fun s hs => by
    have := hpos s hs; simp only [hEE]; rw [div_lt_one (by linarith)]; linarith
  have h0mem : (0 : ℝ) ∈ Icc (0 : ℝ) 1 := ⟨le_refl _, zero_le_one⟩
  have h1mem : (1 : ℝ) ∈ Icc (0 : ℝ) 1 := ⟨zero_le_one, le_refl _⟩
  have hs₀I : s₀ ∈ Icc (0 : ℝ) 1 := Ioo_subset_Icc_self hs₀
  have hneg0 : h 0 < 0 := by simp only [hh, hW0]; linarith [hEEpos 0 h0mem]
  have hneg1 : h 1 < 0 := by simp only [hh, hW1]; linarith [hEEpos 1 h1mem]
  have hpos0 : 0 < h s₀ := by simp only [hh, hEE]; linarith
  -- a zero on `[0, s₀]`
  obtain ⟨s₁, hs₁I, hs₁0⟩ := intermediate_value_Icc hs₀.1.le (hc.mono (Icc_subset_Icc_right hs₀.2.le))
    (show (0 : ℝ) ∈ Icc (h 0) (h s₀) from ⟨hneg0.le, hpos0.le⟩)
  -- a zero on `[s₀, 1]`
  obtain ⟨s₂, hs₂I, hs₂0⟩ := intermediate_value_Icc' hs₀.2.le (hc.mono (Icc_subset_Icc_left hs₀.1.le))
    (show (0 : ℝ) ∈ Icc (h 1) (h s₀) from ⟨hneg1.le, hpos0.le⟩)
  have hs₁ne0 : s₁ ≠ 0 := by rintro rfl; linarith
  have hs₁ne : s₁ ≠ s₀ := by rintro rfl; linarith
  have hs₂ne1 : s₂ ≠ 1 := by rintro rfl; linarith
  have hs₂ne : s₂ ≠ s₀ := by rintro rfl; linarith
  have hlt1 : 0 < s₁ := lt_of_le_of_ne hs₁I.1 (Ne.symm hs₁ne0)
  have hlt2 : s₁ < s₀ := lt_of_le_of_ne hs₁I.2 hs₁ne
  have hlt3 : s₀ < s₂ := lt_of_le_of_ne hs₂I.1 (Ne.symm hs₂ne)
  have hlt4 : s₂ < 1 := lt_of_le_of_ne hs₂I.2 hs₂ne1
  have hs₁mem : s₁ ∈ Icc (0 : ℝ) 1 := ⟨hlt1.le, by linarith⟩
  have hs₂mem : s₂ ∈ Icc (0 : ℝ) 1 := ⟨by linarith, hlt4.le⟩
  refine ⟨s₁, s₂, hlt1, hlt2, hlt3, hlt4, ?_, ?_⟩
  · -- EE is strictly decreasing in `s` because `φ` is and `x ↦ x/(r+x)` is increasing
    have hφlt : φ s₂ < φ s₁ := hanti hs₁mem hs₂mem (by linarith)
    have p1 := hpos s₁ hs₁mem
    have p2 := hpos s₂ hs₂mem
    rw [div_lt_div_iff₀ (by linarith) (by linarith)]
    nlinarith
  · intro s hs
    have hsI : s ∈ Icc (0 : ℝ) 1 := by
      rcases hs with rfl | rfl
      · exact hs₁mem
      · exact hs₂mem
    have hz : h s = 0 := by
      rcases hs with rfl | rfl
      · exact hs₁0
      · exact hs₂0
    refine ⟨hEEpos s hsI, hEElt s hsI, isLeast_of_EE hr hanti hpos hsI, ?_⟩
    simp only [hh] at hz
    linarith

/-! ### An explicit smooth instance of Definition 1 with a discriminatory
equilibrium

Signal densities `f_q(θ) = 2(1+θ)/3`, `f_u(θ) = 2(2-θ)/3` on `[0,1]`, so
`φ(θ) = (2-θ)/(1+θ)` is continuous, strictly decreasing and in `[1/2, 2]`;
`F_u(s) - F_q(s) = 2s(1-s)/3`.  With `ω = 3`, costs uniform on `[0,1]` and
`r = 1`: `WW(s) = G(β(s)) = 2s(1-s)` and `EE(s) = φ(s)/(1+φ(s)) = (2-s)/3`.
`sympy/check_coate_loury.py` derives these from the densities. -/

/-- The example's likelihood ratio. -/
noncomputable def exφ (θ : ℝ) : ℝ := (2 - θ) / (1 + θ)

/-- The example's `WW` curve, `G(β(s)) = 2s(1-s)`. -/
def exWW (s : ℝ) : ℝ := 2 * s * (1 - s)

theorem exφ_strictAntiOn : StrictAntiOn exφ (Icc 0 1) := by
  intro a ha b hb hab
  unfold exφ
  rw [div_lt_div_iff₀ (by linarith [hb.1]) (by linarith [ha.1])]
  nlinarith

theorem exφ_pos : ∀ θ ∈ Icc (0 : ℝ) 1, 0 < exφ θ := fun θ hθ => by
  unfold exφ; exact div_pos (by linarith [hθ.2]) (by linarith [hθ.1])

/-- **Two nonzero self-confirming beliefs, `1/2` and `4/9`**, with optimal
standards `1/2` and `2/3`.  Believing `π_b = 4/9 < π_w = 1/2` is a discriminatory
equilibrium of Definition 1 for ex ante identical groups; and the hypothesis of
Proposition 1 holds at `s₀ = 7/12`. -/
theorem smooth_example :
    IsLeast (acceptSet exφ 1 (1 / 2)) (1 / 2) ∧ exWW (1 / 2) = 1 / 2 ∧
      IsLeast (acceptSet exφ 1 (4 / 9)) (2 / 3) ∧ exWW (2 / 3) = 4 / 9 ∧
      exφ (7 / 12) / (1 + exφ (7 / 12)) < exWW (7 / 12) := by
  have e1 : exφ (1 / 2) / (1 + exφ (1 / 2)) = 1 / 2 := by norm_num [exφ]
  have e2 : exφ (2 / 3) / (1 + exφ (2 / 3)) = 4 / 9 := by norm_num [exφ]
  have h1 := isLeast_of_EE (r := 1) one_pos exφ_strictAntiOn exφ_pos
    (show (1 / 2 : ℝ) ∈ Icc 0 1 by norm_num)
  have h2 := isLeast_of_EE (r := 1) one_pos exφ_strictAntiOn exφ_pos
    (show (2 / 3 : ℝ) ∈ Icc 0 1 by norm_num)
  rw [e1] at h1
  rw [e2] at h2
  refine ⟨h1, by norm_num [exWW], h2, by norm_num [exWW], by norm_num [exφ, exWW]⟩

/-! ## Proposition 2 (p. 1229) -/

/-- **Proposition 2.**  In any equilibrium under affirmative action,
`ρ̂(s_b) = ρ̂(s_w)` and `π_i = G(β(s_i))`.  If `ρ̂` is strictly decreasing (the
paper says "decreasing"; injectivity is what the argument uses), then `s_b = s_w`
and so `π_b = π_w`. -/
theorem prop2_homogeneous {ρhat WW : ℝ → ℝ} (hanti : StrictAntiOn ρhat (Icc 0 1))
    {sb sw πb πw : ℝ} (hsb : sb ∈ Icc (0 : ℝ) 1) (hsw : sw ∈ Icc (0 : ℝ) 1)
    (hAA : ρhat sb = ρhat sw) (hb : πb = WW sb) (hw : πw = WW sw) : πb = πw := by
  rw [hb, hw, hanti.injOn hsb hsw hAA]

/-! ## Section II.B: the uniform example (pp. 1230-1232) -/

/-- The middle term of **eq. (6)** is `π̂ = φ/(r + φ)` with `φ = p_u/p_q`,
`r = x_q/x_u`. -/
theorem pihat_eq6 {pq pu xq xu : ℝ} (hpq : 0 < pq) (hpu : 0 < pu) (hxq : 0 < xq)
    (hxu : 0 < xu) :
    (pu / pq) / (xq / xu + pu / pq) = xu * pu / (xq * pq + xu * pu) := by
  field_simp

/-- The example's best-response map on beliefs: if `π ≥ π̂` employers are liberal
and a fraction `π_ℓ` invests, otherwise they are conservative and `π_c` invests. -/
noncomputable def exBR (πhat πc πl π : ℝ) : ℝ := if πhat ≤ π then πl else πc

/-- **The example's equilibria without affirmative action** (p. 1231).  If
`π_c < π̂ ≤ π_ℓ`, both `π_c` and `π_ℓ` are self-confirming, and they are the only
self-confirming beliefs; assigning `π_c` to B's and `π_ℓ` to W's is an equilibrium
with a negative stereotype against B's. -/
theorem example_selfConfirming {πhat πc πl : ℝ} (hc : πc < πhat) (hl : πhat ≤ πl) :
    exBR πhat πc πl πc = πc ∧ exBR πhat πc πl πl = πl ∧
      ∀ π, exBR πhat πc πl π = π → π = πc ∨ π = πl := by
  refine ⟨by simp [exBR, not_le.mpr hc], by simp [exBR, hl], fun π h => ?_⟩
  unfold exBR at h
  split_ifs at h
  · exact Or.inr h.symm
  · exact Or.inl h.symm

/-- **Eq. (7)**: the patronization probability that achieves compliance,
`α(π_b) = (π_ℓ - π_b)/(1 - π_b)`. -/
theorem alpha_eq7 {πl πb pu α : ℝ} (hb : πb ≠ 1) (hu : pu ≠ 1)
    (h7 : πl + (1 - πl) * pu = πb + (1 - πb) * (pu + (1 - pu) * α)) :
    α = (πl - πb) / (1 - πb) := by
  have hb' : 1 - πb ≠ 0 := sub_ne_zero.mpr (Ne.symm hb)
  have hu' : 1 - pu ≠ 0 := sub_ne_zero.mpr (Ne.symm hu)
  rw [eq_div_iff hb']
  have : (1 - pu) * ((1 - πb) * α) = (1 - pu) * (πl - πb) := by linarith
  have := mul_left_cancel₀ hu' this
  linarith

/-- **Eq. (8)**: the beliefs about B's consistent with equilibrium under
affirmative action are `π_b = π_ℓ` (color-blind) and `π_b = 1 - π_ℓ`
(patronizing). -/
theorem eq8_solutions {πl πb : ℝ} (hb : πb ≠ 1) :
    πb = (1 - πl) / (1 - πb) * πl ↔ πb = πl ∨ πb = 1 - πl := by
  have hb' : 1 - πb ≠ 0 := sub_ne_zero.mpr (Ne.symm hb)
  constructor
  · intro h
    rw [div_mul_eq_mul_div, eq_div_iff hb'] at h
    have : (πb - πl) * (πb - (1 - πl)) = 0 := by linarith
    rcases mul_eq_zero.mp this with h1 | h1
    · left; linarith
    · right; linarith
  · rintro (h | h)
    · subst h; field_simp
    · subst h
      have : πl ≠ 0 := by intro h0; apply hb; rw [h0]; ring
      field_simp
      ring

/-- **Footnote 20**: preferring to put failing B's into task one rather than
unclear W's into task zero, `[λ/(1-λ)][ξ x_q - (1-ξ) x_u] > x_u`, is equivalent to
`λ > λ̂ = 1/(ξ(1+r))`. -/
theorem lambdaHat_iff {lam ξ xq xu : ℝ} (hlam0 : 0 < lam) (hlam1 : lam < 1) (hξ : 0 < ξ)
    (hxq : 0 < xq) (hxu : 0 < xu) :
    xu < lam / (1 - lam) * (ξ * xq - (1 - ξ) * xu) ↔ 1 / (ξ * (1 + xq / xu)) < lam := by
  have h1 : 0 < 1 - lam := by linarith
  rw [div_mul_eq_mul_div, lt_div_iff₀ h1]
  have : 0 < ξ * (1 + xq / xu) := by positivity
  rw [div_lt_iff₀ this]
  have e : lam * (ξ * (1 + xq / xu)) = lam * ξ * (xu + xq) / xu := by field_simp
  rw [e, lt_div_iff₀ hxu]
  constructor <;> intro h <;> nlinarith

/-- One step of the adjustment process (9): `π_b ↦ [(1 - π_ℓ)/(1 - π_b)] π_ℓ`. -/
noncomputable def patronStep (πl x : ℝ) : ℝ := (1 - πl) * πl / (1 - x)

/-- The adjustment process (9) started at `x₀`. -/
noncomputable def patronSeq (πl x₀ : ℝ) : ℕ → ℝ
  | 0 => x₀
  | t + 1 => patronStep πl (patronSeq πl x₀ t)

theorem patronStep_sub {πl x : ℝ} (hx : x ≠ 1) :
    patronStep πl x - (1 - πl) = (1 - πl) / (1 - x) * (x - (1 - πl)) := by
  unfold patronStep
  have : 1 - x ≠ 0 := sub_ne_zero.mpr (Ne.symm hx)
  field_simp
  ring

/-- **Proposition 3, the dynamics** (p. 1232: "for `π_ℓ > 1/2` the solution of (9)
converges to `1 - π_ℓ` as `t → ∞`").  Proved for every start `x₀ < π_ℓ`, in
particular the paper's `π⁰_b = π_c`; the error contracts geometrically with ratio
`(1 - π_ℓ)/(1 - max(x₀, 1 - π_ℓ)) < 1`. -/
theorem patronSeq_tendsto {πl x₀ : ℝ} (h1 : 1 / 2 < πl) (h2 : πl < 1) (hx₀ : x₀ < πl) :
    Tendsto (patronSeq πl x₀) atTop (𝓝 (1 - πl)) := by
  set a := 1 - πl with ha
  set m := max x₀ a with hm
  have hmlt : m < πl := max_lt hx₀ (by linarith)
  have ha0 : 0 < a := by linarith
  set ρ := a / (1 - m) with hρ
  have hm1 : 0 < 1 - m := by linarith
  have hρ0 : 0 ≤ ρ := div_nonneg ha0.le hm1.le
  have hρ1 : ρ < 1 := by rw [hρ, div_lt_one hm1]; linarith
  have key : ∀ t, patronSeq πl x₀ t ≤ m ∧ |patronSeq πl x₀ t - a| ≤ ρ ^ t * |x₀ - a| := by
    intro t
    induction t with
    | zero => exact ⟨le_max_left _ _, by simp [patronSeq]⟩
    | succ n ih =>
      obtain ⟨hle, hb⟩ := ih
      have hx1 : 0 < 1 - patronSeq πl x₀ n := by linarith
      have hk0 : 0 < a / (1 - patronSeq πl x₀ n) := div_pos ha0 hx1
      have hk1 : a / (1 - patronSeq πl x₀ n) ≤ ρ :=
        div_le_div_of_nonneg_left ha0.le hm1 (by linarith)
      have hs : patronSeq πl x₀ (n + 1) - a
          = a / (1 - patronSeq πl x₀ n) * (patronSeq πl x₀ n - a) := by
        show patronStep πl (patronSeq πl x₀ n) - a = _
        exact patronStep_sub (by linarith)
      have hkl : a / (1 - patronSeq πl x₀ n) < 1 := by rw [div_lt_one hx1]; linarith
      constructor
      · rcases le_total a (patronSeq πl x₀ n) with h | h
        · have : a / (1 - patronSeq πl x₀ n) * (patronSeq πl x₀ n - a)
              ≤ patronSeq πl x₀ n - a := by
            have := mul_le_mul_of_nonneg_right hkl.le (by linarith : 0 ≤ patronSeq πl x₀ n - a)
            linarith
          linarith
        · have : a / (1 - patronSeq πl x₀ n) * (patronSeq πl x₀ n - a) ≤ 0 :=
            mul_nonpos_of_nonneg_of_nonpos hk0.le (by linarith)
          linarith [le_max_right x₀ a]
      · rw [hs, abs_mul, abs_of_pos hk0, pow_succ]
        calc a / (1 - patronSeq πl x₀ n) * |patronSeq πl x₀ n - a|
            ≤ ρ * |patronSeq πl x₀ n - a| := mul_le_mul_of_nonneg_right hk1 (abs_nonneg _)
          _ ≤ ρ * (ρ ^ n * |x₀ - a|) := mul_le_mul_of_nonneg_left hb hρ0
          _ = ρ ^ n * ρ * |x₀ - a| := by ring
  rw [tendsto_iff_norm_sub_tendsto_zero]
  refine squeeze_zero (g := fun t => ρ ^ t * |x₀ - a|) (fun t => norm_nonneg _) (fun t => ?_) ?_
  · simpa [Real.norm_eq_abs] using (key t).2
  · simpa using (tendsto_pow_atTop_nhds_zero_of_lt_one hρ0 hρ1).mul_const |x₀ - a|

/-- **The color-blind belief is unstable** under (9) when `π_ℓ > 1/2`: the step
map has slope `π_ℓ/(1 - π_ℓ) > 1` at `π_b = π_ℓ`, while at the patronizing root
`1 - π_ℓ` its slope is `(1 - π_ℓ)/π_ℓ < 1`. -/
theorem colorBlind_unstable {πl : ℝ} (h1 : 1 / 2 < πl) (h2 : πl < 1) :
    HasDerivAt (patronStep πl) (πl / (1 - πl)) πl ∧ 1 < πl / (1 - πl) ∧
      HasDerivAt (patronStep πl) ((1 - πl) / πl) (1 - πl) ∧ (1 - πl) / πl < 1 := by
  have hd : ∀ x : ℝ, x ≠ 1 →
      HasDerivAt (patronStep πl) ((1 - πl) * πl / (1 - x) ^ 2) x := by
    intro x hx
    have hne : 1 - x ≠ 0 := sub_ne_zero.mpr (Ne.symm hx)
    have := (hasDerivAt_const x ((1 - πl) * πl)).div ((hasDerivAt_id x).const_sub 1) hne
    exact this.congr_deriv (by simp)
  have hl : 0 < 1 - πl := by linarith
  have hp : πl ≠ 0 := (by linarith : (0 : ℝ) < πl).ne'
  refine ⟨(hd πl (by linarith)).congr_deriv (by field_simp), ?_,
    (hd (1 - πl) (by linarith)).congr_deriv
      (by rw [show (1 : ℝ) - (1 - πl) = πl by ring]; field_simp), ?_⟩
  · rw [lt_div_iff₀ hl]; linarith
  · rw [div_lt_one (by linarith)]; linarith

/-- **The stereotype worsens** (p. 1232): if `π_ℓ + π_c > 1`, the patronizing belief
`1 - π_ℓ` is below the laissez-faire belief `π_c`. -/
theorem stereotype_worsens {πl πc : ℝ} (h : 1 < πl + πc) : 1 - πl < πc := by linarith

/-- **Footnote 21, one parameter point.**  `p_u = 1/5`, `p_q = 3/10`, `r = 2/3`,
`ω = 7/10`, `λ = 19/20`: then `π̂ = 1/2`, `π_c = 49/100 < π̂ < π_ℓ = 14/25`, eq. (6)
holds, `π_ℓ > 1/2`, `λ > λ̂ = 1/(ξ_ℓ(1+r))`, and the stereotype worsens
(`1 - π_ℓ < π_c`).  `sympy/check_coate_loury.py` checks the footnote's whole
parameter region. -/
theorem fn21_instance :
    let pu : ℝ := 1 / 5
    let pq : ℝ := 3 / 10
    let r : ℝ := 2 / 3
    let ω : ℝ := 7 / 10
    let lam : ℝ := 19 / 20
    let πhat := (pu / pq) / (r + pu / pq)
    let πl := ω * (1 - pu)
    let πc := ω * (1 - pq)
    let ξl := πl * pq / (πl * pq + (1 - πl) * pu)
    πhat = 1 / 2 ∧ πc < πhat ∧ πhat < πl ∧ 1 / 2 < πl ∧
      1 / (ξl * (1 + r)) < lam ∧ 1 - πl < πc := by
  intro pu pq r ω lam πhat πl πc ξl
  simp only [pu, pq, r, ω, lam, πhat, πl, πc, ξl]
  norm_num

end Literature.CoateLoury
