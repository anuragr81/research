import Mathlib

/-! Craig and Fryer, "Complementary Bias: A Model of Two-Sided Statistical
Discrimination", NBER Working Paper 23811, September 2017, in the paper's
notation. An employer holds the prior `π_j` that a group `j` worker invested
and sees a signal `θ` with densities `f_q^W(θ)`, `f_u^W(θ)`. A worker holds the
prior `δ_j` that an employer made the group `j` investment and sees a signal
`ψ` with densities `f_q^E(ψ)`, `f_u^E(ψ)`. A match needs both the worker's
application and the employer's offer. Section 3.1 gives a uniform example with
exactly three equilibria. -/

namespace CraigFryer2017

/-- Bayes' rule on p. 9 (and p. 10 for the worker). The posterior that the
other side invested, from the prior `p` and the densities `fq`, `fu` of the
observed signal. The employer's `κ(π_j, θ)` and the worker's `ξ(δ_j, ψ)` are
both this function. -/
noncomputable def post (p fq fu : ℝ) : ℝ := p * fq / (p * fq + (1 - p) * fu)

/-- The employer's offer rule, p. 9. Hiring has positive expected payoff,
`κ χ_q − (1 − κ) χ_u > 0`, exactly when `χ_q/χ_u > ((1 − π_j)/π_j) φ(θ)` with
`φ = f_u^W/f_q^W`. The worker's application rule on p. 10 is the same
statement with `ω_q, ω_u, δ_j, τ`. -/
theorem offer_iff {p fq fu χq χu : ℝ} (h0 : 0 < p) (h1 : p < 1) (hq : 0 < fq) (hu : 0 < fu)
    (hχ : 0 < χu) :
    0 < post p fq fu * χq - (1 - post p fq fu) * χu ↔ (1 - p) / p * (fu / fq) < χq / χu := by
  have hD : 0 < p * fq + (1 - p) * fu := by
    have : 0 < 1 - p := by linarith
    positivity
  have e : post p fq fu * χq - (1 - post p fq fu) * χu
      = (p * fq * χq - (1 - p) * fu * χu) / (p * fq + (1 - p) * fu) := by
    unfold post
    field_simp
    ring
  rw [e, div_pos_iff_of_pos_right hD, div_mul_div_comm, div_lt_div_iff₀ (by positivity) hχ]
  constructor <;> intro h <;> nlinarith

/-- With an uninformative signal (`f_q = f_u`, so `φ = 1`), the posterior is
the group prior. The group then carries all the evidence, as under group
sufficiency. -/
theorem post_uninformative {p f : ℝ} (hf : 0 < f) : post p f f = p := by
  unfold post
  rw [show p * f + (1 - p) * f = f by ring]
  field_simp

/-- Payoffs, p. 8. The worker's gain from a job over no job is `ω_q` with an
employer who invested and `−ω_u` with one who did not, whatever her own
investment, because her cost `c` is paid either way. Her ranking of employers
does not depend on her own type. -/
theorem match_gain_indep_own (ωq ωu c : ℝ) :
    ((ωq - c) - (-c) = ωq - 0) ∧ ((-ωu - c) - (-c) = -ωu - 0) := by
  constructor <;> ring

/-! ### Section 3.1, the uniform example

`G^W(c) = c`, `G^E(k) = k`, `θ_q = ψ_q = 1/3`, `θ_u = ψ_u = 2/3`, `ω_q = 3`,
`ω_u = 1`, `χ_q = 6`, `χ_u = 2`, `λ = 1/2`. -/

/-- `φ̂ = (1 − θ_q)/θ_u = 1`, and the switching beliefs are
`π̂ = φ̂/(χ_q/χ_u + φ̂) = 1/4` and `δ̂ = τ̂/(ω_q/ω_u + τ̂) = 1/4`. -/
theorem hats :
    (1 - 1 / 3 : ℝ) / (2 / 3) = 1 ∧ (1 : ℝ) / (6 / 2 + 1) = 1 / 4 ∧ (1 : ℝ) / (3 / 1 + 1) = 1 / 4 := by
  norm_num

/-- `β_W` from its primitives, eq. (1). `F_u^W(s) − F_q^W(s) = 1/2` at both
thresholds `s = θ_q` and `s = θ_u`. With the lenient application threshold
`t = ψ_q`, `ϖ(δ) = 3δ − (1 − δ)/2`, and with the strict one `t = ψ_u`,
`ϖ(δ) = 3δ/2`. -/
theorem betaW_primitives (δ : ℝ) :
    ((1 / 3 : ℝ) / (2 / 3) - 0 = 1 - (2 / 3 - 1 / 3) / (2 / 3)) ∧
      ((1 / 3 : ℝ) / (2 / 3)) * (δ * (1 - 0) * 3 - (1 - δ) * (1 - (1 / 3) / (2 / 3)) * 1)
        = 7 / 4 * δ - 1 / 4 ∧
      ((1 / 3 : ℝ) / (2 / 3)) * (δ * (1 - (2 / 3 - 1 / 3) / (2 / 3)) * 3 - (1 - δ) * (1 - 1) * 1)
        = 3 / 4 * δ := by
  refine ⟨by norm_num, by ring, by ring⟩

/-- `β_E` from its primitives, eq. (2), with `λ = 1/2`. -/
theorem betaE_primitives (π : ℝ) :
    (1 / 2 : ℝ) * (1 / 2) * (π * (1 - 0) * 6 - (1 - π) * (1 - (1 / 3) / (2 / 3)) * 2)
        = 7 / 4 * π - 1 / 4 ∧
      (1 / 2 : ℝ) * (1 / 2) * (π * (1 - (2 / 3 - 1 / 3) / (2 / 3)) * 6 - (1 - π) * (1 - 1) * 2)
        = 3 / 4 * π := by
  constructor <;> ring

/-- The return to investment of p. 15, the same function for workers (of `δ`)
and for firms (of `π`). -/
noncomputable def βex (x : ℝ) : ℝ := if 1 / 4 ≤ x then 7 / 4 * x - 1 / 4 else 3 / 4 * x

/-- The two branches agree at the switch `1/4`, so the choice of `≥` or `<`
there does not matter. -/
theorem betaex_switch : (7 / 4 : ℝ) * (1 / 4) - 1 / 4 = 3 / 4 * (1 / 4) := by norm_num

/-- `π = min {max {G^W, 0}, 1}`, p. 16. -/
noncomputable def clip (y : ℝ) : ℝ := min (max y 0) 1

/-- The fraction who invest given the other side's rate. -/
noncomputable def g (x : ℝ) : ℝ := clip (βex x)

/-- Eqs. (3) and (4) in the example. Each side's belief equals the rate at
which the other side's best response makes it invest. -/
def IsEq (π δ : ℝ) : Prop := π = g δ ∧ δ = g π

theorem betaex_mono {a b : ℝ} (h : a ≤ b) : βex a ≤ βex b := by
  unfold βex
  split_ifs <;> linarith

theorem g_mono {a b : ℝ} (h : a ≤ b) : g a ≤ g b := by
  unfold g clip
  exact min_le_min (max_le_max (betaex_mono h) le_rfl) le_rfl

/-- The fixed points of `g`. -/
theorem fixed_iff (x : ℝ) : x = g x ↔ x = 0 ∨ x = 1 / 3 ∨ x = 1 := by
  constructor
  · intro hx
    simp only [g, clip, βex, min_def, max_def] at hx
    split_ifs at hx <;>
      first
      | (left; linarith)
      | (right; left; linarith)
      | (right; right; linarith)
  · rintro (h | h | h) <;> subst h <;> norm_num [g, clip, βex]

/-- Figure 3. The example has exactly three equilibria, `Z = (0, 0)`,
`U = (1/3, 1/3)` and `S = (1, 1)`. -/
theorem three_equilibria (π δ : ℝ) :
    IsEq π δ ↔ (π = 0 ∧ δ = 0) ∨ (π = 1 / 3 ∧ δ = 1 / 3) ∨ (π = 1 ∧ δ = 1) := by
  constructor
  · rintro ⟨hπ, hδ⟩
    have hsame : π = δ := by
      rcases le_total π δ with h | h
      · have := g_mono h; linarith
      · have := g_mono h; linarith
    subst hsame
    rcases (fixed_iff π).1 hπ with h | h | h <;> simp [h]
  · rintro (⟨h1, h2⟩ | ⟨h1, h2⟩ | ⟨h1, h2⟩) <;> subst h1 <;> subst h2 <;>
      exact ⟨by norm_num [g, clip, βex], by norm_num [g, clip, βex]⟩

/-- The zero investment equilibrium exists, p. 12. -/
theorem zero_equilibrium : IsEq 0 0 := (three_equilibria 0 0).2 (Or.inl ⟨rfl, rfl⟩)

/-- Control. Holding firm investment at any positive rate, as the one-sided
model does, zero worker investment is no longer a best response. The zero
equilibrium needs the workers' pessimism `δ = 0` as well as the firms'. -/
theorem control_one_sided_zero_fails {δ : ℝ} (hδ : 0 < δ) : ¬ IsEq 0 δ := by
  rintro ⟨h0, -⟩
  have : 0 < g δ := by
    unfold g clip βex
    split_ifs with h
    · rw [lt_min_iff]; exact ⟨lt_max_iff.2 (Or.inl (by linarith)), by norm_num⟩
    · rw [lt_min_iff]; exact ⟨lt_max_iff.2 (Or.inl (by linarith)), by norm_num⟩
  linarith

/-- The firm's return with a general group share `λ`, eq. (2). At `λ = 1/2` it
is `β_ex`. -/
noncomputable def βE (l π : ℝ) : ℝ := 2 * l * βex π

/-- Control (Proposition 3's mechanism). For a group with share `λ = 1/4`
instead of `1/2`, the beliefs `U = (1/3, 1/3)` are no longer an equilibrium,
because firms' return to the group's investment scales with `λ`. -/
theorem control_minority_breaks_U : (1 / 3 : ℝ) ≠ clip (βE (1 / 4) (1 / 3)) := by
  norm_num [clip, βE, βex]

end CraigFryer2017
