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

Formalised: the group's value of acceptance and Lemma 2 (printed "Lemma 11",
AS-D1), which is the group's half of Proposition 1; assumption (7) as the
non-triviality condition; the dilution of a pool under a uniform prior, which
is why pooling lets the group accept types it would reject under complete
information (Proposition 3(3)); the honesty condition of Proposition 8; and
the threshold of Proposition 9 as derived from the paper's own payoffs (AS-D2).
The existence and refinement results (Propositions 2 and 4 to 7) are not
formalised. -/

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

end AustenSmithFryer2003
