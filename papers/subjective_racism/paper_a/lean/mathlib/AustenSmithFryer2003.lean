import Mathlib

/-! Austen-Smith and Fryer (2003), NBER Working Paper 9904, the structure Paper
A1 relies on or contrasts with, in the paper's notation.

The peer group accepts or rejects an individual in the school years from the
belief `F = F(θ̂(δ)|σ(θ))`, the probability that the individual's type lies at
or below the critical type `θ̂(δ)` given the education observed. Types at or
below `θ̂(δ)` always contribute and give the group `b` in every period; higher
types give `b` while the required contribution is `0` (probability `π`), and
`−Bk` the first time it is `k`, after which the group rejects them. `γ` is the
group's discount factor. `Bk` is written `Bk`, the product of `B` and `k`.

Formalised: Lemmas 1 to 4 (printed "Lemma 10" to "Lemma 13", AS-D1), Remark 1,
Proposition 1, the comparative statics of eq. (11), the core of Proposition 3(3),
Corollary 1 under a uniform prior, Propositions 8 and 9. The leisure value
`v(1 − θk|1)` enters as a function `V` of the type, continuous and strictly
decreasing. Propositions 2 and 4 to 7 rest on the D1 refinement and on
separating strategies as functions, and are not formalised. The coverage of each
numbered result is listed in `lit/austen_smith_fryer_2003/CLAIMS.md`. -/

namespace AustenSmithFryer2003

/-- The group's expected payoff, discounted to `t = 0`, from accepting an
individual with belief `F` (proof of Lemma 2, PDF p. 28). -/
noncomputable def z (F b Bk π γ : ℝ) : ℝ :=
  F * (γ * b / (1 - γ)) + (1 - F) * (γ * (π * b - (1 - π) * Bk) / (1 - π * γ))

/-- Eq. (11), the minimal belief at which the group accepts. -/
noncomputable def F0 (b Bk π γ : ℝ) : ℝ :=
  ((1 - π) * Bk - π * b) * (1 - γ) / ((b + (1 - γ) * Bk) * (1 - π))

/-- Lemma 2 and the group's half of Proposition 1. Accepting has a nonnegative
value if and only if `F(θ̂|σ(θ)) ≥ F₀`. -/
theorem lemma2_accept_iff {F b Bk π γ : ℝ} (hb : 0 < b) (hB : 0 < Bk) (hπ0 : 0 < π)
    (hπ1 : π < 1) (hγ0 : 0 < γ) (hγ1 : γ < 1) :
    0 ≤ z F b Bk π γ ↔ F0 b Bk π γ ≤ F := by
  have h1 : 0 < 1 - γ := by linarith
  have h2 : 0 < 1 - π * γ := by nlinarith
  have h3 : 0 < 1 - π := by linarith
  have h4 : 0 < b + (1 - γ) * Bk := by positivity
  have n1 : 1 - γ ≠ 0 := h1.ne'
  have n2 : 1 - π * γ ≠ 0 := h2.ne'
  have n2' : 1 - γ * π ≠ 0 := by rw [mul_comm]; exact n2
  have n3 : 1 - π ≠ 0 := h3.ne'
  have n4 : b + (1 - γ) * Bk ≠ 0 := h4.ne'
  have key : z F b Bk π γ = γ * (1 - π) * (b + (1 - γ) * Bk) / ((1 - γ) * (1 - π * γ))
      * (F - F0 b Bk π γ) := by
    unfold z F0
    field_simp
    ring
  have hpos : 0 < γ * (1 - π) * (b + (1 - γ) * Bk) / ((1 - γ) * (1 - π * γ)) := by
    positivity
  rw [key, mul_nonneg_iff_of_pos_left hpos, sub_nonneg]

/-- Assumption (7), first part. If `π < Bk/(Bk + b)` then `0 < F₀ < 1`, so some
beliefs lead to rejection and some to acceptance. -/
theorem F0_mem {b Bk π γ : ℝ} (hb : 0 < b) (hB : 0 < Bk) (hπ0 : 0 < π) (hπ1 : π < 1)
    (hγ1 : γ < 1) (h7 : π < Bk / (Bk + b)) :
    0 < F0 b Bk π γ ∧ F0 b Bk π γ < 1 := by
  have h1 : 0 < 1 - γ := by linarith
  have h3 : 0 < 1 - π := by linarith
  have h4 : 0 < b + (1 - γ) * Bk := by positivity
  have hnum : 0 < (1 - π) * Bk - π * b := by
    rw [lt_div_iff₀ (by linarith)] at h7
    linarith
  unfold F0
  constructor
  · positivity
  · rw [div_lt_one (by positivity)]
    nlinarith [mul_pos (mul_pos hπ0 hb) h1, mul_pos hb h3]

/-- Control: without assumption (7) every individual is accepted. If
`π ≥ Bk/(Bk + b)` then accepting has a nonnegative value at every belief, as the
paper says ("without the assumption all individuals are always accepted into
the group"). -/
theorem control_no_assumption7_all_accepted {F b Bk π γ : ℝ} (hb : 0 < b) (hB : 0 < Bk)
    (hπ0 : 0 < π) (hπ1 : π < 1) (hγ0 : 0 < γ) (hγ1 : γ < 1) (hF0 : 0 ≤ F) (hF1 : F ≤ 1)
    (hnot7 : Bk / (Bk + b) ≤ π) : 0 ≤ z F b Bk π γ := by
  have h1 : 0 < 1 - γ := by linarith
  have h2 : 0 < 1 - π * γ := by nlinarith
  have hhi : 0 ≤ π * b - (1 - π) * Bk := by
    rw [div_le_iff₀ (by linarith)] at hnot7
    linarith
  unfold z
  have a : 0 ≤ F * (γ * b / (1 - γ)) := mul_nonneg hF0 (by positivity)
  have c : 0 ≤ (1 - F) * (γ * (π * b - (1 - π) * Bk) / (1 - π * γ)) :=
    mul_nonneg (by linarith) (div_nonneg (mul_nonneg hγ0.le hhi) h2.le)
  linarith

/-- The belief at a pool under a uniform prior. If all types in `[0, θs]` pool on
one education level and the prior is uniform, then `F(θ̂|pool) = θ̂/θs` for
`θ̂ ≤ θs`. The group accepts the pool if and only if `θs ≤ θ̂/F₀`. -/
theorem pool_accept_iff {θh θs F₀ : ℝ} (hθ : 0 < θh) (hs : θh ≤ θs) (hF : 0 < F₀) :
    F₀ ≤ θh / θs ↔ θs ≤ θh / F₀ := by
  have hs0 : 0 < θs := lt_of_lt_of_le hθ hs
  rw [le_div_iff₀ hs0, le_div_iff₀ hF, mul_comm]

/-- Pooling admits unreliable types. If `0 < F₀ < 1`, a pool `[0, θ̂/F₀]` reaches
strictly above `θ̂` and is accepted, so types above `θ̂`, who will renege on the
group, are accepted in the school years (Proposition 3(3), Corollary 1). -/
theorem pool_admits_unreliable {θh F₀ : ℝ} (hθ : 0 < θh) (hF0 : 0 < F₀) (hF1 : F₀ < 1) :
    θh < θh / F₀ ∧ F₀ ≤ θh / (θh / F₀) := by
  constructor
  · rw [lt_div_iff₀ hF0]; nlinarith
  · have : θh / (θh / F₀) = F₀ := by field_simp
    rw [this]

/-- Control: without pooling the unreliable types are rejected. Under separation
the belief about a type `θ > θ̂` puts no mass at or below `θ̂`, so `F = 0 < F₀`
and, with assumption (7), the group rejects. -/
theorem control_separation_rejects {b Bk π γ : ℝ} (hb : 0 < b) (hB : 0 < Bk)
    (hπ0 : 0 < π) (hπ1 : π < 1) (hγ0 : 0 < γ) (hγ1 : γ < 1) (h7 : π < Bk / (Bk + b)) :
    ¬ 0 ≤ z 0 b Bk π γ := by
  rw [lemma2_accept_iff hb hB hπ0 hπ1 hγ0 hγ1]
  have := (F0_mem hb hB hπ0 hπ1 hγ1 h7).1
  linarith

/-- Proposition 8, the honesty condition, with `v11 = v(1|1)` and `v10 = v(1|0)`
(the printed `v(j|·)`, AS-D3). The gain from stating an intention to contribute
and reneging, over stating the truth, is
`[v11 + δ/(1−δ) v10] − [v10 + δ/(1−δ)(π v11 + (1−π) v10)]`, and it is
nonpositive if and only if `δ ≥ 1/(1 + π)`. -/
theorem prop8_honest_iff {v11 v10 π δ : ℝ} (hv : v10 < v11) (hπ0 : 0 < π)
    (hδ0 : 0 < δ) (hδ1 : δ < 1) :
    (v11 + δ / (1 - δ) * v10) - (v10 + δ / (1 - δ) * (π * v11 + (1 - π) * v10)) ≤ 0 ↔
      1 / (1 + π) ≤ δ := by
  have h1 : 0 < 1 - δ := by linarith
  have key : (v11 + δ / (1 - δ) * v10) - (v10 + δ / (1 - δ) * (π * v11 + (1 - π) * v10))
      = (v11 - v10) * (1 - δ - δ * π) / (1 - δ) := by
    field_simp; ring
  rw [key, div_le_iff₀ (by linarith : (0:ℝ) < 1 + π), div_nonpos_iff]
  constructor
  · rintro (⟨_, h⟩ | ⟨h, _⟩)
    · linarith
    · have : 1 - δ - δ * π ≤ 0 := by
        by_contra hc; push_neg at hc
        have := mul_pos (by linarith : 0 < v11 - v10) hc
        linarith
      linarith
  · intro h
    right
    exact ⟨mul_nonpos_of_nonneg_of_nonpos (by linarith) (by linarith), h1.le⟩

/-- Proposition 9 from the paper's own payoffs (proof on p. 37 of the PDF). The
group prefers peer pressure to tolerance if and only if
`F(θ̂|σ) ≥ ((1−γ)Bk + F(θ^T) b)/((1−γ)Bk + b)`. -/
theorem prop9_derived {F FT b Bk π γ : ℝ} (hb : 0 < b) (hB : 0 < Bk) (hπ0 : 0 < π)
    (hπ1 : π < 1) (hγ1 : γ < 1) :
    F * (b / (1 - γ)) + (1 - F) * ((π * b - (1 - π) * Bk) / (1 - π * γ)) ≥
        FT * (b / (1 - γ)) + (1 - FT) * (π * b / (1 - π * γ)) ↔
      ((1 - γ) * Bk + FT * b) / ((1 - γ) * Bk + b) ≤ F := by
  have h1 : 0 < 1 - γ := by linarith
  have h2 : 0 < 1 - π * γ := by nlinarith
  have h3 : 0 < 1 - π := by linarith
  have h4 : 0 < (1 - γ) * Bk + b := by positivity
  have n1 : 1 - γ ≠ 0 := h1.ne'
  have n2 : 1 - π * γ ≠ 0 := h2.ne'
  have n2' : 1 - γ * π ≠ 0 := by rw [mul_comm]; exact n2
  have key : F * (b / (1 - γ)) + (1 - F) * ((π * b - (1 - π) * Bk) / (1 - π * γ)) -
      (FT * (b / (1 - γ)) + (1 - FT) * (π * b / (1 - π * γ))) =
      (1 - π) / ((1 - γ) * (1 - π * γ)) *
        (F * ((1 - γ) * Bk + b) - ((1 - γ) * Bk + FT * b)) := by
    field_simp; ring
  have hc : 0 < (1 - π) / ((1 - γ) * (1 - π * γ)) := by positivity
  rw [ge_iff_le, ← sub_nonneg, key, mul_nonneg_iff_of_pos_left hc, sub_nonneg,
    div_le_iff₀ h4]

/-- The printed threshold of Proposition 9, `(Bk + F(θ^T) b)/(Bk + b)`, is the
derived one at `γ = 0`. -/
theorem prop9_printed_at_gamma_zero (FT b Bk : ℝ) :
    ((1 - 0) * Bk + FT * b) / ((1 - 0) * Bk + b) = (Bk + FT * b) / (Bk + b) := by
  simp

/-- At `γ = 1/2`, `b = Bk = 1`, `π = 1/2`, `F(θ^T) = 0`, the derived threshold is
`1/3` and the printed one is `1/2`, so at `F(θ̂|σ) = 2/5` the group prefers peer
pressure although the printed condition fails (AS-D2). -/
theorem prop9_printed_differs :
    ((1 - (1/2 : ℝ)) * 1 + 0 * 1) / ((1 - 1/2) * 1 + 1) = 1/3 ∧
      ((1 : ℝ) + 0 * 1) / (1 + 1) = 1/2 ∧
      ((2/5 : ℝ) * (1 / (1 - 1/2)) + (1 - 2/5) * ((1/2 * 1 - (1 - 1/2) * 1) / (1 - 1/2 * (1/2))) ≥
        0 * (1 / (1 - 1/2)) + (1 - 0) * (1/2 * 1 / (1 - 1/2 * (1/2)))) ∧
      ¬ ((1 : ℝ) + 0 * 1) / (1 + 1) ≤ 2/5 := by
  norm_num


/-- `E_(r) v(1 − θκ̃|a) = r v(1|a) + (1 − r) v(1 − θk|a)`, the appendix's notation,
with `v1 = v(1|a)` and `vk = v(1 − θk|a)`. -/
def E (r v1 vk : ℝ) : ℝ := r * v1 + (1 - r) * vk

/-- Eq. (12), the payoff from always contributing, with `v1k = v(1 − θk|1)`,
`v11 = v(1|1)`. -/
noncomputable def U1 (w v1k v11 δ π : ℝ) : ℝ := w + v1k + δ / (1 - δ) * (w + E π v11 v1k)

/-- Eq. (13), the payoff from refusing `k` once and being rejected, `v10 = v(1|0)`. -/
noncomputable def U0 (w v11 v10 δ : ℝ) : ℝ := w + v11 + δ / (1 - δ) * (w + v10)

/-- Eq. (14). -/
theorem eq14 {w v1k v11 v10 δ π : ℝ} (hδ1 : δ < 1) :
    U0 w v11 v10 δ ≤ U1 w v1k v11 δ π ↔
      (1 - δ) * v11 + δ * v10 ≤ E (π * δ) v11 v1k := by
  have h1 : 0 < 1 - δ := by linarith
  have key : U1 w v1k v11 δ π - U0 w v11 v10 δ =
      (E (π * δ) v11 v1k - ((1 - δ) * v11 + δ * v10)) / (1 - δ) := by
    unfold U1 U0 E; field_simp; ring
  rw [← sub_nonneg, key, div_nonneg_iff]
  constructor
  · rintro (⟨h, _⟩ | ⟨_, h⟩)
    · linarith
    · linarith
  · intro h; left; exact ⟨by linarith, h1.le⟩

/-- The left side of (15) as a function of the type, `V θ = v(1 − θk|1)`. -/
def G (V : ℝ → ℝ) (v11 v10 π δ θ : ℝ) : ℝ := E (π * δ) v11 (V θ) - ((1 - δ) * v11 + δ * v10)

theorem G_strictAnti {V : ℝ → ℝ} {v11 v10 π δ k : ℝ} (hV : StrictAntiOn V (Set.Icc 0 (1 / k)))
    (hπ1 : π < 1) (hδ0 : 0 ≤ δ) (hδ1 : δ < 1) :
    StrictAntiOn (G V v11 v10 π δ) (Set.Icc 0 (1 / k)) := by
  intro a ha b hb hab
  have := hV ha hb hab
  have hc : 0 < 1 - π * δ := by nlinarith
  unfold G E
  nlinarith

/-- Lemma 1 (printed "Lemma 10", AS-D1), existence and uniqueness. If
`V θ = v(1 − θk|1)` is continuous and strictly decreasing on `[0, 1/k]`, with
`V 0 = v(1|1)` and `V (1/k) = v(0|1) = 0`, if `0 < v(1|0) < v(1|1)` by (3) and
`π v(1|1) < v(1|0)` by (7), then there is a unique `θ̂ ∈ (0, 1/k)` solving (15). -/
theorem lemma1_exists_unique {V : ℝ → ℝ} {v11 v10 π δ k : ℝ} (hk : 0 < k)
    (hc : ContinuousOn V (Set.Icc 0 (1 / k))) (hV : StrictAntiOn V (Set.Icc 0 (1 / k)))
    (hV0 : V 0 = v11) (hVk : V (1 / k) = 0) (h3 : v10 < v11) (h30 : 0 < v10)
    (h7 : π * v11 < v10) (hπ1 : π < 1) (hδ0 : 0 < δ) (hδ1 : δ < 1) :
    ∃ θh ∈ Set.Ioo 0 (1 / k), G V v11 v10 π δ θh = 0 ∧
      ∀ θ ∈ Set.Icc 0 (1 / k), G V v11 v10 π δ θ = 0 → θ = θh := by
  have hk' : (0 : ℝ) < 1 / k := by positivity
  have hG : StrictAntiOn (G V v11 v10 π δ) (Set.Icc 0 (1 / k)) :=
    G_strictAnti hV hπ1 hδ0.le hδ1
  have g0 : 0 < G V v11 v10 π δ 0 := by
    unfold G E; rw [hV0]; nlinarith
  have gk : G V v11 v10 π δ (1 / k) < 0 := by
    unfold G E; rw [hVk]; nlinarith
  have hcG : ContinuousOn (G V v11 v10 π δ) (Set.Icc 0 (1 / k)) := by
    unfold G E
    fun_prop
  obtain ⟨θh, hθ, hz⟩ := intermediate_value_Icc' hk'.le hcG ⟨gk.le, g0.le⟩
  have hne0 : θh ≠ 0 := by rintro rfl; linarith
  have hnek : θh ≠ 1 / k := by rintro rfl; linarith
  refine ⟨θh, ⟨lt_of_le_of_ne hθ.1 (Ne.symm hne0), lt_of_le_of_ne hθ.2 hnek⟩, hz, ?_⟩
  intro θ hθ' hz'
  by_contra hne
  rcases lt_or_gt_of_ne hne with h | h
  · have := hG hθ' hθ h; linarith
  · have := hG hθ hθ' h; linarith

/-- Lemma 1, the threshold. Types at or below `θ̂` weakly prefer to contribute,
types above strictly prefer to refuse, strategy [P2]. -/
theorem lemma1_threshold {V : ℝ → ℝ} {v11 v10 π δ k θh θ : ℝ}
    (hV : StrictAntiOn V (Set.Icc 0 (1 / k))) (hπ1 : π < 1) (hδ0 : 0 ≤ δ)
    (hδ1 : δ < 1) (hθh : θh ∈ Set.Icc 0 (1 / k)) (hz : G V v11 v10 π δ θh = 0)
    (hθ : θ ∈ Set.Icc 0 (1 / k)) :
    (θ ≤ θh → 0 ≤ G V v11 v10 π δ θ) ∧ (θh < θ → G V v11 v10 π δ θ < 0) := by
  have hG := G_strictAnti hV hπ1 hδ0 hδ1 (v11 := v11) (v10 := v10)
  constructor
  · intro h
    rcases eq_or_lt_of_le h with h | h
    · rw [h, hz]
    · have := hG hθ hθh h; linarith
  · intro h; have := hG hθh hθ h; linarith

/-- Lemma 1, `θ̂(δ)` strictly increasing in `δ`. -/
theorem lemma1_mono {V : ℝ → ℝ} {v11 v10 π δ δ' k θh θh' : ℝ}
    (hV : StrictAntiOn V (Set.Icc 0 (1 / k))) (hV0 : V 0 = v11) (h3 : v10 < v11)
    (hπ0 : 0 < π) (hπ1 : π < 1) (hδ0 : 0 < δ) (hδδ : δ < δ') (hδ1 : δ' < 1)
    (hθh : θh ∈ Set.Icc 0 (1 / k)) (hθh' : θh' ∈ Set.Icc 0 (1 / k))
    (hz : G V v11 v10 π δ θh = 0) (hz' : G V v11 v10 π δ' θh' = 0) : θh < θh' := by
  have hVle : V θh ≤ v11 := by
    rcases eq_or_lt_of_le hθh.1 with h | h
    · rw [← h, hV0]
    · have := hV ⟨le_refl 0, le_trans hθh.1 hθh.2⟩ hθh h; linarith
  have hdiff : G V v11 v10 π δ' θh - G V v11 v10 π δ θh =
      (δ' - δ) * (π * v11 - π * V θh + v11 - v10) := by unfold G E; ring
  have hpos : 0 < G V v11 v10 π δ' θh := by
    have : 0 < (δ' - δ) * (π * v11 - π * V θh + v11 - v10) := by
      apply mul_pos (by linarith); nlinarith
    linarith
  have hG := G_strictAnti hV hπ1 (by linarith) hδ1 (v11 := v11) (v10 := v10)
  by_contra h; push_neg at h
  rcases eq_or_lt_of_le h with h | h
  · subst h; linarith
  · have := hG hθh' hθh h; linarith

/-- Lemma 1, `lim_{δ↓0} θ̂(δ) = 0`, in the form: for any `ε ∈ (0, 1/k]`, once
`δ M < v(1|1) − V ε` with `M = π v(1|1) − π V ε + v(1|1) − v(1|0)`, the critical type
lies below `ε`. -/
theorem lemma1_limit {V : ℝ → ℝ} {v11 v10 π δ k θh ε : ℝ}
    (hV : StrictAntiOn V (Set.Icc 0 (1 / k))) (hπ1 : π < 1) (hδ0 : 0 ≤ δ)
    (hδ1 : δ < 1) (hθh : θh ∈ Set.Icc 0 (1 / k)) (hz : G V v11 v10 π δ θh = 0)
    (hε : ε ∈ Set.Icc 0 (1 / k))
    (hsmall : δ * (π * v11 - π * V ε + v11 - v10) < v11 - V ε) : θh < ε := by
  have hGe : G V v11 v10 π δ ε < 0 := by unfold G E; nlinarith
  by_contra h; push_neg at h
  rcases eq_or_lt_of_le h with h | h
  · subst h; linarith
  · have := G_strictAnti hV hπ1 hδ0 hδ1 (v11 := v11) (v10 := v10) hε hθh h; linarith

/-- Control for Lemma 1: without the second part of (7), `π v(1|1) < v(1|0)`, the
left side of (15) can be positive at `θ = 1/k`, so no type in `[0, 1/k]` is
critical. Here `π = 99/100`, `δ = 99/100`, `v(1|1) = 1`, `v(1|0) = 1/100`. -/
theorem control_lemma1_needs_7 :
    ¬ ((99/100 : ℝ) * 1 < 1/100) ∧
      0 < E ((99/100) * (99/100)) 1 0 - ((1 - 99/100) * 1 + 99/100 * (1/100)) := by
  unfold E; norm_num

/-- Lemma 3 (printed "Lemma 12"). With the same group decision, incentive
compatibility for two types and strictly decreasing differences of `c` (from
`c_sθ < 0`) give education weakly increasing in type. `A s = v(1 − s|a₀)` and
`W s = δ/(1−δ) w(s)`. -/
theorem lemma3 {A W : ℝ → ℝ} {c : ℝ → ℝ → ℝ} {s' s'' θ' θ'' : ℝ}
    (hsc : ∀ s1 s2 t1 t2, s1 < s2 → t1 < t2 → c s2 t2 - c s1 t2 < c s2 t1 - c s1 t1)
    (ic1 : A s'' - c s'' θ' + W s'' ≤ A s' - c s' θ' + W s')
    (ic2 : A s' - c s' θ'' + W s' ≤ A s'' - c s'' θ'' + W s'')
    (hθ : θ' < θ'') : s' ≤ s'' := by
  by_contra h; push_neg at h
  have := hsc s'' s' θ' θ'' h hθ
  linarith

/-- Control for Lemma 3: without single crossing (`c` independent of type) a
lower type may choose more education. -/
theorem control_lemma3_needs_single_crossing :
    let c : ℝ → ℝ → ℝ := fun _ _ => 0
    (0 : ℝ) - c 0 0 + 0 ≤ 0 - c 1 0 + 0 ∧ (0 : ℝ) - c 1 1 + 0 ≤ 0 - c 0 1 + 0 ∧
      (0 : ℝ) < 1 ∧ ¬ (1 : ℝ) ≤ 0 := by
  intro c; norm_num [c]

/-- Lemma 4 (printed "Lemma 13"), the closing step. With
`h s = δ y_s − (1−δ)(v′ + c_s)` strictly decreasing (second-order condition),
`h (σ θ) < 0` by (18) and `h (σ^c θ) = 0` by (19) give `σ θ > σ^c θ`. -/
theorem lemma4_step {h : ℝ → ℝ} {σ σc : ℝ} (hd : StrictAnti h) (h18 : h σ < 0)
    (h19 : h σc = 0) : σc < σ := by
  by_contra hc; push_neg at hc
  rcases eq_or_lt_of_le hc with e | e
  · rw [e] at h18; linarith
  · have := hd e; linarith

/-- Remark 1. If `h₀ > h₁` pointwise (from `v′(l|1) > v′(l|0)`, eq. (3)), `h₀` is
strictly decreasing, and `σ^c_a` solves `h_a = 0`, then `σ^c_0 > σ^c_1`. -/
theorem remark1 {h0 h1 : ℝ → ℝ} {s0 s1 : ℝ} (hd : StrictAnti h0)
    (hgt : ∀ s, h1 s < h0 s) (e0 : h0 s0 = 0) (e1 : h1 s1 = 0) : s1 < s0 := by
  have : 0 < h0 s1 := by have := hgt s1; linarith
  by_contra hc; push_neg at hc
  rcases eq_or_lt_of_le hc with e | e
  · subst e; linarith
  · have := hd e; linarith

/-- Control for Remark 1: without the strict gap `v′(l|1) > v′(l|0)` the two
complete-information choices can coincide. -/
theorem control_remark1 : ∃ s0 s1 : ℝ, (fun s : ℝ => -s) s0 = 0 ∧ (fun s : ℝ => -s) s1 = 0 ∧
    ¬ s1 < s0 := ⟨0, 0, by norm_num, by norm_num, lt_irrefl 0⟩


/-- Eq. (11), comparative statics stated on PDF p. 15. `F₀` is decreasing in `π`. -/
theorem F0_anti_pi {b Bk π π' γ : ℝ} (hb : 0 < b) (hB : 0 < Bk) (hγ1 : γ < 1)
    (hππ : π < π') (hπ1 : π' < 1) : F0 b Bk π' γ < F0 b Bk π γ := by
  have h1 : 0 < 1 - γ := by linarith
  have hD : 0 < b + (1 - γ) * Bk := by positivity
  have a : 0 < 1 - π := by linarith
  have a' : 0 < 1 - π' := by linarith
  unfold F0
  rw [div_lt_div_iff₀ (by positivity) (by positivity)]
  have : 0 < (1 - γ) * (b + (1 - γ) * Bk) * b * (π' - π) := by
    apply mul_pos (mul_pos (mul_pos h1 hD) hb); linarith
  nlinarith

/-- `F₀` is decreasing in `γ` under (7). -/
theorem F0_anti_gamma {b Bk π γ γ' : ℝ} (hb : 0 < b) (hB : 0 < Bk) (hπ1 : π < 1)
    (hN : 0 < (1 - π) * Bk - π * b) (hγγ : γ < γ') (hγ1 : γ' < 1) :
    F0 b Bk π γ' < F0 b Bk π γ := by
  have h1 : 0 < 1 - γ' := by linarith
  have h1' : 0 < 1 - γ := by linarith
  have a : 0 < 1 - π := by linarith
  unfold F0
  rw [div_lt_div_iff₀ (by positivity) (by positivity)]
  have : 0 < ((1 - π) * Bk - π * b) * (1 - π) * b * (γ' - γ) := by
    apply mul_pos (mul_pos (mul_pos hN a) hb); linarith
  nlinarith

/-- `F₀` is decreasing in `b` under (7). -/
theorem F0_anti_b {b b' Bk π γ : ℝ} (hb : 0 < b) (hbb : b < b') (hB : 0 < Bk)
    (hπ0 : 0 < π) (hπ1 : π < 1) (hγ1 : γ < 1) (hN : 0 < (1 - π) * Bk - π * b) :
    F0 b' Bk π γ < F0 b Bk π γ := by
  have h1 : 0 < 1 - γ := by linarith
  have a : 0 < 1 - π := by linarith
  have hD : 0 < b + (1 - γ) * Bk := by positivity
  have hD' : 0 < b' + (1 - γ) * Bk := by nlinarith
  unfold F0
  rw [div_lt_div_iff₀ (by positivity) (by positivity)]
  have e1 : ((1 - π) * Bk - π * b') * (b + (1 - γ) * Bk) <
      ((1 - π) * Bk - π * b) * (b + (1 - γ) * Bk) := by
    apply mul_lt_mul_of_pos_right _ hD; nlinarith
  have e2 : ((1 - π) * Bk - π * b) * (b + (1 - γ) * Bk) <
      ((1 - π) * Bk - π * b) * (b' + (1 - γ) * Bk) := by
    apply mul_lt_mul_of_pos_left _ hN; linarith
  have := mul_pos h1 a
  nlinarith

/-- `F₀` is increasing in `Bk`. -/
theorem F0_mono_Bk {b Bk Bk' π γ : ℝ} (hb : 0 < b) (hB : 0 < Bk) (hBB : Bk < Bk')
    (hπ0 : 0 < π) (hπ1 : π < 1) (hγ1 : γ < 1) : F0 b Bk π γ < F0 b Bk' π γ := by
  have h1 : 0 < 1 - γ := by linarith
  have a : 0 < 1 - π := by linarith
  have hD' : 0 < b + (1 - γ) * Bk' := by nlinarith
  unfold F0
  rw [div_lt_div_iff₀ (by positivity) (by positivity)]
  have key : ((1 - π) * Bk' - π * b) * (b + (1 - γ) * Bk) -
      ((1 - π) * Bk - π * b) * (b + (1 - γ) * Bk') =
      ((1 - π) * b + π * b * (1 - γ)) * (Bk' - Bk) := by ring
  have : 0 < ((1 - π) * b + π * b * (1 - γ)) * (Bk' - Bk) := by
    apply mul_pos (by positivity); linarith
  have hk := mul_pos h1 a
  nlinarith

/-- Proposition 1, assembled from Lemma 1 and Lemma 2: a unique critical type
`θ̂ ∈ (0, 1/k)`, and acceptance in the school years if and only if
`F(θ̂|σ(θ)) ≥ F₀`. -/
theorem prop1 {V : ℝ → ℝ} {v11 v10 π δ k F b Bk γ : ℝ} (hk : 0 < k)
    (hc : ContinuousOn V (Set.Icc 0 (1 / k))) (hV : StrictAntiOn V (Set.Icc 0 (1 / k)))
    (hV0 : V 0 = v11) (hVk : V (1 / k) = 0) (h3 : v10 < v11) (h30 : 0 < v10)
    (h7 : π * v11 < v10) (hπ0 : 0 < π) (hπ1 : π < 1) (hδ0 : 0 < δ) (hδ1 : δ < 1)
    (hb : 0 < b) (hB : 0 < Bk) (hγ0 : 0 < γ) (hγ1 : γ < 1) :
    (∃ θh ∈ Set.Ioo 0 (1 / k), G V v11 v10 π δ θh = 0 ∧
      ∀ θ ∈ Set.Icc 0 (1 / k), G V v11 v10 π δ θ = 0 → θ = θh) ∧
      (0 ≤ z F b Bk π γ ↔ F0 b Bk π γ ≤ F) :=
  ⟨lemma1_exists_unique hk hc hV hV0 hVk h3 h30 h7 hπ1 hδ0 hδ1,
    lemma2_accept_iff hb hB hπ0 hπ1 hγ0 hγ1⟩

/-- Proposition 3(3), the core. A type revealed to lie at or below `θ̂` has
`F(θ̂|σ(θ)) = 1`, and the group's value of accepting it is strictly positive. -/
theorem prop3_3_core {b Bk π γ : ℝ} (hb : 0 < b) (hγ0 : 0 < γ) (hγ1 : γ < 1) :
    0 < z 1 b Bk π γ := by
  unfold z
  have : 0 < 1 - γ := by linarith
  simp only [sub_self, zero_mul, add_zero, one_mul]
  positivity

/-- Corollary 1 under a uniform prior. With `0 < F₀ < 1` and a critical type
`θ̂ ∈ (0, 1/k)`, the pool `[0, θ̂/F₀]` is accepted, and every type in it above
`θ̂` refuses the high contribution when first asked, by Lemma 1. -/
theorem corollary1_uniform {V : ℝ → ℝ} {v11 v10 π δ k θh F₀ : ℝ}
    (hV : StrictAntiOn V (Set.Icc 0 (1 / k))) (hπ1 : π < 1) (hδ0 : 0 ≤ δ) (hδ1 : δ < 1)
    (hθh : θh ∈ Set.Ioo 0 (1 / k)) (hz : G V v11 v10 π δ θh = 0) (hF0 : 0 < F₀) (hF1 : F₀ < 1) :
    θh < θh / F₀ ∧ F₀ ≤ θh / (θh / F₀) ∧
      ∀ θ ∈ Set.Icc 0 (1 / k), θh < θ → G V v11 v10 π δ θ < 0 := by
  obtain ⟨a, b⟩ := pool_admits_unreliable hθh.1 hF0 hF1
  refine ⟨a, b, fun θ hθ hlt => ?_⟩
  exact (lemma1_threshold hV hπ1 hδ0 hδ1 ⟨hθh.1.le, hθh.2.le⟩ hz hθ).2 hlt

/-- Proposition 8, the tolerant group's acceptance. Its value of accepting any
individual, `F(θ^T|σ) b/(1−γ) + (1 − F(θ^T|σ)) πb/(1−πγ)`, is nonnegative at
every belief, so accepting everyone is a best response. -/
theorem prop8_tolerant_accepts {F b π γ : ℝ} (hF0 : 0 ≤ F) (hF1 : F ≤ 1) (hb : 0 < b)
    (hπ0 : 0 < π) (hπ1 : π < 1) (hγ1 : γ < 1) :
    0 ≤ F * (b / (1 - γ)) + (1 - F) * (π * b / (1 - π * γ)) := by
  have h1 : 0 < 1 - γ := by linarith
  have h2 : 0 < 1 - π * γ := by nlinarith
  have := mul_nonneg hF0 (div_nonneg hb.le h1.le)
  have := mul_nonneg (by linarith : 0 ≤ 1 - F) (div_nonneg (mul_nonneg hπ0.le hb.le) h2.le)
  linarith

/-- Control for Proposition 8: without `v(1|1) > v(1|0)` (eq. (3)) the gain from
deception is zero at every `δ`, so honesty holds below `1/(1 + π)` as well. -/
theorem control_prop8_needs_3 :
    let v : ℝ := 1
    let π : ℝ := 1/2
    let δ : ℝ := 1/10
    (v + δ / (1 - δ) * v) - (v + δ / (1 - δ) * (π * v + (1 - π) * v)) ≤ 0 ∧
      ¬ (1 / (1 + π) ≤ δ) := by
  intro v π δ; norm_num [v, π, δ]

/-- Control for Proposition 9: at `π = 1` the two group values coincide at every
belief, so the group weakly prefers peer pressure at `F = 0` although the
threshold is positive. -/
theorem control_prop9_needs_pi_lt_one :
    (0 : ℝ) * (1 / (1 - 1/2)) + (1 - 0) * ((1 * 1 - (1 - 1) * 1) / (1 - 1 * (1/2))) ≥
        0 * (1 / (1 - 1/2)) + (1 - 0) * (1 * 1 / (1 - 1 * (1/2))) ∧
      ¬ (((1 - (1/2 : ℝ)) * 1 + 0 * 1) / ((1 - 1/2) * 1 + 1) ≤ 0) := by
  norm_num

end AustenSmithFryer2003
