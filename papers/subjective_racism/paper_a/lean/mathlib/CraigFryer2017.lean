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

/-- A revealing signal. If an uninvested party cannot send the observed
signal (`f_u = 0`), the posterior is 1 whatever the group prior, as long as
the prior is positive. The group then drops out of the decision. -/
theorem post_revealing {p fq : ℝ} (hp : 0 < p) (hq : 0 < fq) : post p fq 0 = 1 := by
  unfold post
  rw [mul_zero, add_zero, div_self (by positivity)]

/-- Control. With a non-revealing signal (`f_u = f_q`) the posterior is the
prior, so a lower prior still lowers the posterior. -/
theorem control_post_not_revealing {p f : ℝ} (hf : 0 < f) (hp : p < 1) : post p f f < 1 := by
  rw [post_uninformative hf]; exact hp

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

/-! ## The general model of Section 3 -/

/-- The primitives. `GW`, `GE` are the cost distributions, `FqW`, `FuW` the
distributions of the employer's signal for a worker who did and did not
invest, `FqE`, `FuE` those of the worker's signal about the employer. -/
structure Model where
  GW : ℝ → ℝ
  GE : ℝ → ℝ
  FqW : ℝ → ℝ
  FuW : ℝ → ℝ
  FqE : ℝ → ℝ
  FuE : ℝ → ℝ
  ωq : ℝ
  ωu : ℝ
  χq : ℝ
  χu : ℝ

/-- `ϖ(δ)` of p. 10, the worker's expected payoff from a job given the
application threshold `t`. -/
def varpi (M : Model) (δ t : ℝ) : ℝ :=
  δ * (1 - M.FqE t) * M.ωq - (1 - δ) * (1 - M.FuE t) * M.ωu

/-- `χ̄(π)` of p. 10, the employer's expected payoff from a hire given the
hiring threshold `s`. -/
def chibar (M : Model) (π s : ℝ) : ℝ :=
  π * (1 - M.FqW s) * M.χq - (1 - π) * (1 - M.FuW s) * M.χu

/-- Eq. (1). The worker's net return to investment. -/
def betaW (M : Model) (s δ t : ℝ) : ℝ := (M.FuW s - M.FqW s) * varpi M δ t

/-- Eq. (2). The firm's net return to the group `j` investment, with group
share `l`. -/
def betaE (M : Model) (t π s l : ℝ) : ℝ := l * (M.FuE t - M.FqE t) * chibar M π s

/-- Eqs. (3) and (4), the definition of equilibrium, with the thresholds
`s*(π)` and `t*(δ)` given as functions. -/
def IsEquilibrium (M : Model) (sstar tstar : ℝ → ℝ) (l π δ : ℝ) : Prop :=
  π = M.GW (betaW M (sstar π) δ (tstar δ)) ∧ δ = M.GE (betaE M (tstar δ) π (sstar π) l)

/-- p. 12. The zero investment equilibrium. When nobody is believed to invest
both thresholds are 1, so nobody is hired, both returns are 0, and nobody
invests. -/
theorem zero_equilibrium_general (M : Model) {sstar tstar : ℝ → ℝ} (l : ℝ)
    (hs : sstar 0 = 1) (ht : tstar 0 = 1) (hFW : M.FuW 1 = 1) (hFE : M.FuE 1 = 1)
    (hGW : M.GW 0 = 0) (hGE : M.GE 0 = 0) : IsEquilibrium M sstar tstar l 0 0 := by
  unfold IsEquilibrium betaW betaE varpi chibar
  rw [hs, ht, hFW, hFE]
  simp [hGW, hGE]

/-- The EE curve, p. 12. The hiring threshold `s` is optimal at belief `π`
exactly when `π = φ(s)/(χ_q/χ_u + φ(s))`, the curve of Proposition 1's
hypothesis. `r` stands for `χ_q/χ_u`. -/
theorem ee_curve {r φ π : ℝ} (hπ : π ≠ 0) (hr : 0 < r) (hφ : 0 ≤ φ) :
    r = (1 - π) / π * φ ↔ π = φ / (r + φ) := by
  have h : r + φ ≠ 0 := by linarith
  constructor
  · intro e
    rw [eq_div_iff h]
    rw [e]; field_simp; ring
  · intro e
    have hφ0 : φ ≠ 0 := by
      rintro rfl; simp at e; exact hπ e
    rw [e]; field_simp; ring

/-- Section 3.1. With a constant likelihood ratio, an employer offers a job to
an unclear worker exactly when `π_j > π̂_j = φ̂/(χ_q/χ_u + φ̂)`. -/
theorem example_switch {r φ π : ℝ} (h0 : 0 < π) (hr : 0 < r) (hφ : 0 < φ) :
    (1 - π) / π * φ < r ↔ φ / (r + φ) < π := by
  rw [div_mul_eq_mul_div, div_lt_iff₀ h0, div_lt_iff₀ (by linarith)]
  constructor <;> intro h <;> nlinarith

/-- Proposition 1, first part (proof, PDF p. 33). Write `WW(s)` for the
worker curve `G^W(β_W(s, δ))` and `EE(s)` for the employer curve. If `WW`
starts below `EE` at 0, rises above it at some `s₀`, and falls below it again
at 1, eq. (3) has two non-zero solutions, one on each side of `s₀`. -/
theorem prop1_two_solutions {WW EE : ℝ → ℝ} {s₀ : ℝ} (h0 : 0 < s₀) (h1 : s₀ < 1)
    (hc : ContinuousOn (fun s => WW s - EE s) (Set.Icc 0 1))
    (hpos : ∀ s ∈ Set.Icc (0 : ℝ) 1, 0 < EE s)
    (ha : WW 0 < EE 0) (hb : EE s₀ < WW s₀) (he : WW 1 < EE 1) :
    ∃ s₁ s₂, 0 < s₁ ∧ s₁ < s₀ ∧ s₀ < s₂ ∧ s₂ < 1 ∧ WW s₁ = EE s₁ ∧ WW s₂ = EE s₂ ∧
      0 < WW s₁ ∧ 0 < WW s₂ := by
  have c1 : ContinuousOn (fun s => WW s - EE s) (Set.Icc 0 s₀) :=
    hc.mono (Set.Icc_subset_Icc le_rfl h1.le)
  have c2 : ContinuousOn (fun s => WW s - EE s) (Set.Icc s₀ 1) :=
    hc.mono (Set.Icc_subset_Icc h0.le le_rfl)
  obtain ⟨s₁, hs₁, e₁⟩ := intermediate_value_Icc h0.le c1
    (show (0 : ℝ) ∈ Set.Icc (WW 0 - EE 0) (WW s₀ - EE s₀) from ⟨by linarith, by linarith⟩)
  obtain ⟨s₂, hs₂, e₂⟩ := intermediate_value_Icc' h1.le c2
    (show (0 : ℝ) ∈ Set.Icc (WW 1 - EE 1) (WW s₀ - EE s₀) from ⟨by linarith, by linarith⟩)
  simp only at e₁ e₂
  have n1 : s₁ ≠ 0 := by rintro rfl; linarith
  have n2 : s₁ ≠ s₀ := by rintro rfl; linarith
  have n3 : s₂ ≠ s₀ := by rintro rfl; linarith
  have n4 : s₂ ≠ 1 := by rintro rfl; linarith
  have q1 := hpos s₁ ⟨hs₁.1, by linarith [hs₁.2]⟩
  have q2 := hpos s₂ ⟨by linarith [hs₂.1], hs₂.2⟩
  refine ⟨s₁, s₂, lt_of_le_of_ne hs₁.1 (Ne.symm n1), lt_of_le_of_ne hs₁.2 n2,
    lt_of_le_of_ne hs₂.1 (Ne.symm n3), lt_of_le_of_ne hs₂.2 n4, by linarith, by linarith,
    by linarith, by linarith⟩

/-- Proposition 1, second part (proof, PDF p. 33). If the composite of the
largest solutions, `h = π̄* ∘ δ̄*`, is monotone on `[0, 1]`, maps it into
itself, and lies above the diagonal at some `a`, it has a fixed point above
`a`, which is a non-zero equilibrium besides the zero one. The paper argues by
continuity of the diagonal; this proof uses a supremum and needs no
continuity of `h`. -/
theorem prop1_fixed_point {h : ℝ → ℝ} (hm : MonotoneOn h (Set.Icc 0 1))
    (hmap : ∀ x ∈ Set.Icc (0 : ℝ) 1, h x ∈ Set.Icc (0 : ℝ) 1) {a : ℝ} (ha0 : 0 ≤ a)
    (ha1 : a ≤ 1) (hab : a < h a) : ∃ x, a < x ∧ x ≤ 1 ∧ h x = x := by
  set S := {x : ℝ | a ≤ x ∧ x ≤ 1 ∧ x ≤ h x} with hS
  have haS : a ∈ S := ⟨le_rfl, ha1, hab.le⟩
  have hne : S.Nonempty := ⟨a, haS⟩
  have hbdd : BddAbove S := ⟨1, fun x hx => hx.2.1⟩
  set x₀ := sSup S with hx₀
  have hax : a ≤ x₀ := le_csSup hbdd haS
  have hx1 : x₀ ≤ 1 := csSup_le hne (fun x hx => hx.2.1)
  have hxI : x₀ ∈ Set.Icc (0 : ℝ) 1 := ⟨le_trans ha0 hax, hx1⟩
  have hup : x₀ ≤ h x₀ := by
    apply csSup_le hne
    intro x hx
    have hxI' : x ∈ Set.Icc (0 : ℝ) 1 := ⟨le_trans ha0 hx.1, hx.2.1⟩
    exact le_trans hx.2.2 (hm hxI' hxI (le_csSup hbdd hx))
  have haI : a ∈ Set.Icc (0 : ℝ) 1 := ⟨ha0, ha1⟩
  have hha : h a ≤ h x₀ := hm haI hxI hax
  have hhI := hmap x₀ hxI
  have hdown : h x₀ ≤ x₀ := by
    apply le_csSup hbdd
    exact ⟨by linarith, hhI.2, hm hxI hhI hup⟩
  exact ⟨x₀, by linarith, hx1, le_antisymm hdown hup⟩

/-- Control for Proposition 1. Without a point above the diagonal, a monotone
map can have zero as its only fixed point, so the zero equilibrium is the
only one. -/
theorem control_prop1_below_diagonal :
    let h : ℝ → ℝ := fun x => x / 2
    MonotoneOn h (Set.Icc 0 1) ∧ ∀ x, h x = x → x = 0 := by
  refine ⟨fun a _ b _ hab => by show a / 2 ≤ b / 2; linarith, fun x hx => ?_⟩
  simp only at hx
  linarith

/-! ## One-sided policies, Section 4 -/

/-- `ρ(s_j, π_j)` of PDF p. 19, the probability of an offer. -/
def rho (M : Model) (s π : ℝ) : ℝ := π * (1 - M.FqW s) + (1 - π) * (1 - M.FuW s)

/-- `P(s_j, π_j, i_j)` of PDF p. 19, the expected payoff from an offer, with
`Fi` the value `F^E_{i_j}(t_j)`. -/
def Pay (M : Model) (s π Fi : ℝ) : ℝ :=
  π * (1 - M.FqW s) * (1 - Fi) * M.χq - (1 - π) * (1 - M.FuW s) * (1 - Fi) * M.χu

/-- Proposition 2, the first step (proof, PDF pp. 32–33). If no B worker
applies (`F^E_i(t_B) = 1`), the employer's payoff from B is zero at every
hiring standard, so the affirmative action constraint costs nothing. -/
theorem prop2_nonbinding (M : Model) (s π : ℝ) : Pay M s π 1 = 0 := by
  unfold Pay; ring

/-- Proposition 2, the second step. With `π_B = 0` the offer rate is
`1 − F_u^W(s_B)`, and a continuous `F_u^W` from 0 to 1 lets the employer match
any offer rate of the A market. -/
theorem prop2_constraint_solvable (M : Model) (hc : ContinuousOn M.FuW (Set.Icc 0 1))
    (h0 : M.FuW 0 = 0) (h1 : M.FuW 1 = 1) {ρA : ℝ} (hρ : ρA ∈ Set.Icc (0 : ℝ) 1) :
    ∃ sB ∈ Set.Icc (0 : ℝ) 1, rho M sB 0 = ρA := by
  have : 1 - ρA ∈ Set.Icc (M.FuW 0) (M.FuW 1) := by
    rw [h0, h1]; exact ⟨by linarith [hρ.2], by linarith [hρ.1]⟩
  obtain ⟨sB, hsB, e⟩ := intermediate_value_Icc zero_le_one hc this
  exact ⟨sB, hsB, by unfold rho; rw [e]; ring⟩

/-- Proposition 3 (proof, PDF p. 33). With unequal group shares there is no
equilibrium with positive investment and equal employer beliefs. `D` is
`F_u^W(s*(π)) − F_q^W(s*(π))`, common to both groups when beliefs are equal;
`ϖ` is the worker's payoff as a function of `δ` with `t*(δ)` substituted, and
`Y δ = [F_u^E − F_q^E](t*(δ)) χ̄(π)`. The paper takes `ϖ` increasing; here it
is assumed injective. -/
theorem prop3 {GW GE ϖ Y : ℝ → ℝ} {D lA lB π δA δB : ℝ} (hl : lA ≠ lB)
    (hGW : Function.Injective GW) (hGW0 : GW 0 = 0) (hGE : Function.Injective GE)
    (hGE0 : GE 0 = 0) (hϖ : Function.Injective ϖ) (hπ : 0 < π) (hδ : 0 < δA) :
    ¬ (π = GW (D * ϖ δA) ∧ π = GW (D * ϖ δB) ∧ δA = GE (lA * Y δA) ∧ δB = GE (lB * Y δB)) := by
  rintro ⟨e1, e2, e3, e4⟩
  have hD : D ≠ 0 := by
    rintro rfl; rw [zero_mul, hGW0] at e1; linarith
  by_cases hδAB : δA = δB
  · subst hδAB
    have hY : Y δA ≠ 0 := by
      intro h0; rw [h0, mul_zero, hGE0] at e3; linarith
    have := hGE (e3.symm.trans e4)
    exact hl (mul_right_cancel₀ hY this)
  · have h1 : D * ϖ δA = D * ϖ δB := hGW (e1.symm.trans e2)
    exact hδAB (hϖ (mul_left_cancel₀ hD h1))

/-- Control for Proposition 3. With equal shares the four equations can hold
at a positive, homogeneous point: take `G^W = G^E = ϖ = Y = id`, `D = 1`,
`λ_A = λ_B = 1` and `π = δ_A = δ_B = 1/2`. -/
theorem control_prop3_equal_shares :
    Function.Injective (id : ℝ → ℝ) ∧ (id : ℝ → ℝ) 0 = 0 ∧
      (1 / 2 : ℝ) = id (1 * id (1 / 2 : ℝ)) ∧ (1 / 2 : ℝ) = id (1 * id (1 / 2 : ℝ)) :=
  ⟨Function.injective_id, rfl, by norm_num, by norm_num⟩

/-- Proposition 4, the last step (proof, PDF p. 33). Once the firm's return
to the B investment is not positive, no firm invests; with `δ_B = 0` workers
expect nothing from a job and none invests. -/
theorem prop4_collapse (M : Model) {sB tB x : ℝ} (hGE : ∀ y ≤ 0, M.GE y = 0) (hx : x ≤ 0)
    (ht : tB = 1) (hFE : M.FuE 1 = 1) (hGW : M.GW 0 = 0) :
    M.GE x = 0 ∧ M.GW (betaW M sB (M.GE x) tB) = 0 := by
  refine ⟨hGE x hx, ?_⟩
  rw [hGE x hx, ht]
  unfold betaW varpi
  rw [hFE]; simp [hGW]

/-- Control for Proposition 4. A positive return with `G^E = id` leaves
firm investment positive. -/
theorem control_prop4_positive_return {x : ℝ} (hx : 0 < x) : (id : ℝ → ℝ) x ≠ 0 := hx.ne'

/-- `ρ^H(s_j, π_j, i_j)` of PDF p. 20, the probability of a hire. -/
def rhoH (M : Model) (s π Fi : ℝ) : ℝ := rho M s π * (1 - Fi)

/-- Proposition 6, first part (PDF p. 22). The quota caps hiring of A at the
largest hiring rate the firm can reach for B, which is at most `1 − F_i^E(t_B)`
and tends to zero as `t_B → 1`. -/
theorem prop6_cap (M : Model) {s π Fi : ℝ} (hπ0 : 0 ≤ π) (hπ1 : π ≤ 1)
    (hq : 0 ≤ M.FqW s) (hu : 0 ≤ M.FuW s) (hF : Fi ≤ 1) : rhoH M s π Fi ≤ 1 - Fi := by
  unfold rhoH rho
  have hb : π * (1 - M.FqW s) + (1 - π) * (1 - M.FuW s) ≤ 1 := by nlinarith
  nlinarith

theorem prop6_quota {a b ε : ℝ} (hq : a = b) (hb : b ≤ ε) : a ≤ ε := hq ▸ hb

/-- Section 4.C. A wage subsidy `s ≥ ω_u` makes every worker apply. -/
theorem subsidy_always_apply {ξ s ωq ωu : ℝ} (h0 : 0 ≤ ξ) (hs : ωu < s)
    (hq : 0 ≤ ωq) (hu : 0 ≤ ωu) : 0 < ξ * (s + ωq) + (1 - ξ) * (s - ωu) := by
  nlinarith [mul_nonneg h0 (add_nonneg hq hu)]

/-- Section 4.C. If every worker applies (`t = 0`, where both signal
distributions are 0), the firm's return to investment is zero, and no firm
invests. -/
theorem subsidy_kills_firm_investment (M : Model) (π s l : ℝ) (hq : M.FqE 0 = 0)
    (hu : M.FuE 0 = 0) (hGE : M.GE 0 = 0) : M.GE (betaE M 0 π s l) = 0 := by
  unfold betaE; rw [hq, hu]; simp [hGE]

/-- Proposition 7, the failure of `{s, t}` (proof, PDF p. 36). From
`π₀ = δ₀ = 0`, any thresholds leave both returns non-positive. -/
theorem prop7_st_fails (M : Model) {s t l : ℝ} (hl : 0 ≤ l) (hW : M.FqW s ≤ M.FuW s)
    (hE : M.FqE t ≤ M.FuE t) (hFW : M.FuW s ≤ 1) (hFE : M.FuE t ≤ 1) (hωu : 0 ≤ M.ωu)
    (hχu : 0 ≤ M.χu) : betaW M s 0 t ≤ 0 ∧ betaE M t 0 s l ≤ 0 := by
  unfold betaW betaE varpi chibar
  constructor
  · have : 0 ≤ (1 - M.FuE t) * M.ωu := mul_nonneg (by linarith) hωu
    nlinarith [mul_nonneg (sub_nonneg.2 hW) this]
  · have : 0 ≤ (1 - M.FuW s) * M.χu := mul_nonneg (by linarith) hχu
    have h2 : 0 ≤ l * (M.FuE t - M.FqE t) := mul_nonneg hl (sub_nonneg.2 hE)
    nlinarith [mul_nonneg h2 this]

/-- Proposition 8, the payment (proof, PDF p. 37). With an informative
government signal, `F̂_u(s^g) − F̂_q(s^g) = k > 0`, a payment `ω^g` raises the
return from any current value `C` to any target `T`. -/
theorem prop8_payment {k C T : ℝ} (hk : 0 < k) : C + k * ((T - C) / k) = T := by
  field_simp; ring

/-- Control for Proposition 8. An uninformative government signal (`k = 0`)
cannot move the return, whatever the payment. -/
theorem control_prop8_uninformative {C T ω : ℝ} (h : C ≠ T) : C + 0 * ω ≠ T := by
  simpa using h

/-- Proposition 8, the permanent subsidy (PDF p. 37). Keeping
`δ_B = δ_A` needs `χ^g = (λ_A − λ_B) Z / k`, which is zero exactly when the
shares are equal. -/
theorem prop8_permanent_zero_iff {lA lB Z k : ℝ} (hZ : Z ≠ 0) (hk : 0 < k) :
    (lA - lB) * Z / k = 0 ↔ lA = lB := by
  rw [div_eq_zero_iff, mul_eq_zero, sub_eq_zero]
  constructor
  · rintro ((h | h) | h)
    · exact h
    · exact absurd h hZ
    · linarith
  · intro h; exact Or.inl (Or.inl h)

/-! ## Section 6 and Proposition 10 -/

/-- `β_j = σ²_a/(σ²_ε + σ²_a)`, PDF p. 31, the wage elasticity to ability,
with the variances written `vε`, `va`. -/
noncomputable def betaj (vε va : ℝ) : ℝ := va / (vε + va)

/-- Section 6. With a noiseless signal (`σ²_ε = 0`) the return to ability is
1, so the shrinkage toward the group mean, and with it the group's weight in
the wage, vanishes. -/
theorem betaj_perfect_signal {va : ℝ} (hva : 0 < va) : betaj 0 va = 1 ∧ 0 / (0 + va) = (0 : ℝ) := by
  unfold betaj; constructor
  · rw [zero_add, div_self hva.ne']
  · simp

/-- Control. With a noisy signal the return to ability is below 1, and the
group mean keeps a positive weight `σ²_ε/(σ²_ε + σ²_a)`. -/
theorem control_betaj_noise {vε va : ℝ} (hε : 0 < vε) (hva : 0 < va) :
    betaj vε va < 1 ∧ 0 < vε / (vε + va) := by
  unfold betaj
  refine ⟨(div_lt_one (by linarith)).2 (by linarith), by positivity⟩

/-- Proposition 10, the coefficient (proof, PDF pp. 38–39). Substituting
ability recovered from the old wage gives the coefficient
`ρ (σ²_{ε,OLD} + σ²_a)/(σ²_ε + σ²_a)` on the old log wage. -/
theorem prop10_coefficient {ρ vε vεold va : ℝ} (hva : 0 < va) (hd : 0 < vε + va) :
    ρ * betaj vε va * ((vεold + va) / va) = ρ * ((vεold + va) / (vε + va)) := by
  unfold betaj; field_simp

/-- Proposition 10. The estimated gap is `ρ` times the true gap in returns
plus a term in the old employers' information. -/
theorem prop10_decomposition {ρ vεW vεWo vaW vεB vεBo vaB : ℝ} (hW : 0 < vεW + vaW)
    (hB : 0 < vεB + vaB) :
    ρ * ((vεWo + vaW) / (vεW + vaW) - (vεBo + vaB) / (vεB + vaB))
      = ρ * ((betaj vεW vaW - betaj vεB vaB) + (vεWo / (vεW + vaW) - vεBo / (vεB + vaB))) := by
  unfold betaj; field_simp; ring

/-- Proposition 10, the direction of the bias. With `0 < ρ ≤ 1`, a true gap
`Γ ≥ 0` and the old-information term not positive, the estimate understates
the gap. -/
theorem prop10_downward {ρ Γ o : ℝ} (hρ0 : 0 < ρ) (hρ1 : ρ ≤ 1) (hΓ : 0 ≤ Γ) (ho : o ≤ 0) :
    ρ * (Γ + o) ≤ Γ := by
  nlinarith

/-- Control for Proposition 10. If old employers knew more about black than
about white workers, the estimate could overstate the gap. -/
theorem control_prop10_overstate : (1 : ℝ) * (0 + 1 / 10) > 0 := by norm_num

/-! ## Online appendix, Sections 8.2 and 8.3 -/

/-- 8.2.1. The worker's gain from investing, with an outside option `Ū`,
equals `β_W − c` once `ω_q = w_q − Ū` and `ω_u = Ū − w_u`. -/
theorem normalization_invest (Fq Fu FqE FuE δ wq wu Ubar c : ℝ) :
    ((1 - Fq) * (δ * (1 - FqE) * wq + (1 - δ) * (1 - FuE) * wu)
        + (Fq + (1 - Fq) * (δ * FqE + (1 - δ) * FuE)) * Ubar - c)
      - ((1 - Fu) * (δ * (1 - FqE) * wq + (1 - δ) * (1 - FuE) * wu)
        + (Fu + (1 - Fu) * (δ * FqE + (1 - δ) * FuE)) * Ubar)
      = (Fu - Fq) * (δ * (1 - FqE) * (wq - Ubar) - (1 - δ) * (1 - FuE) * (Ubar - wu)) - c := by
  ring

/-- 8.2.1. The application rule with an outside option is the baseline rule. -/
theorem normalization_apply (ξ wq wu Ubar : ℝ) :
    ξ * (wq - Ubar) - (1 - ξ) * (Ubar - wu) = ξ * wq + (1 - ξ) * wu - Ubar := by
  ring

/-- The uniform signal of a party that invested, on `[a, 1]`. -/
noncomputable def Fqunif (a x : ℝ) : ℝ := if x ≤ a then 0 else (x - a) / (1 - a)

/-- The uniform signal of a party that did not invest, on `[0, b]`. -/
noncomputable def Fuunif (b x : ℝ) : ℝ := if x ≤ b then x / b else 1

/-- 8.2.2. The four values of the signal distributions at the two thresholds. -/
theorem unif_values {a b : ℝ} (hab : a < b) (hb : 0 < b) :
    Fqunif a a = 0 ∧ Fuunif b a = a / b ∧ Fqunif a b = (b - a) / (1 - a) ∧ Fuunif b b = 1 := by
  unfold Fqunif Fuunif
  refine ⟨by simp, by rw [if_pos hab.le], by rw [if_neg (not_le.2 hab)], ?_⟩
  rw [if_pos le_rfl, div_self hb.ne']

/-- 8.2.2. `β_W` in its four cases, from the uniform primitives. -/
theorem appendix822_betaW {θq θu ψq ψu δ ωq ωu : ℝ} (hθ : θq < θu) (hθu1 : θu < 1)
    (hψ : ψq < ψu) (hψu1 : ψu < 1) (hθq : 0 < θq) (hψq : 0 < ψq) :
    (Fuunif θu θq - Fqunif θq θq) * (δ * (1 - Fqunif ψq ψq) * ωq - (1 - δ) * (1 - Fuunif ψu ψq) * ωu)
        = θq / θu * (δ * ωq - (ψu - ψq) / ψu * (1 - δ) * ωu) ∧
      (Fuunif θu θu - Fqunif θq θu) * (δ * (1 - Fqunif ψq ψq) * ωq - (1 - δ) * (1 - Fuunif ψu ψq) * ωu)
        = (1 - θu) / (1 - θq) * (δ * ωq - (ψu - ψq) / ψu * (1 - δ) * ωu) ∧
      (Fuunif θu θq - Fqunif θq θq) * (δ * (1 - Fqunif ψq ψu) * ωq - (1 - δ) * (1 - Fuunif ψu ψu) * ωu)
        = θq / θu * ((1 - ψu) / (1 - ψq) * δ * ωq) ∧
      (Fuunif θu θu - Fqunif θq θu) * (δ * (1 - Fqunif ψq ψu) * ωq - (1 - δ) * (1 - Fuunif ψu ψu) * ωu)
        = (1 - θu) / (1 - θq) * ((1 - ψu) / (1 - ψq) * δ * ωq) := by
  obtain ⟨a1, a2, a3, a4⟩ := unif_values hθ (by linarith)
  obtain ⟨b1, b2, b3, b4⟩ := unif_values hψ (by linarith)
  rw [a1, a2, a3, a4, b1, b2, b3, b4]
  have h1 : (1 - θq) ≠ 0 := by linarith
  have h2 : θu ≠ 0 := by linarith
  have h3 : (1 - ψq) ≠ 0 := by linarith
  have h4 : ψu ≠ 0 := by linarith
  refine ⟨?_, ?_, ?_, ?_⟩ <;> field_simp <;> ring

/-- 8.2.2. `β_E` in its four cases, from the uniform primitives, with share `l`. -/
theorem appendix822_betaE {θq θu ψq ψu π χq χu l : ℝ} (hθ : θq < θu) (hθu1 : θu < 1)
    (hψ : ψq < ψu) (hψu1 : ψu < 1) (hθq : 0 < θq) (hψq : 0 < ψq) :
    l * (Fuunif ψu ψq - Fqunif ψq ψq) * (π * (1 - Fqunif θq θq) * χq - (1 - π) * (1 - Fuunif θu θq) * χu)
        = l * (ψq / ψu * (π * χq - (θu - θq) / θu * (1 - π) * χu)) ∧
      l * (Fuunif ψu ψq - Fqunif ψq ψq) * (π * (1 - Fqunif θq θu) * χq - (1 - π) * (1 - Fuunif θu θu) * χu)
        = l * (ψq / ψu * ((1 - θu) / (1 - θq) * π * χq)) ∧
      l * (Fuunif ψu ψu - Fqunif ψq ψu) * (π * (1 - Fqunif θq θq) * χq - (1 - π) * (1 - Fuunif θu θq) * χu)
        = l * ((1 - ψu) / (1 - ψq) * (π * χq - (θu - θq) / θu * (1 - π) * χu)) ∧
      l * (Fuunif ψu ψu - Fqunif ψq ψu) * (π * (1 - Fqunif θq θu) * χq - (1 - π) * (1 - Fuunif θu θu) * χu)
        = l * ((1 - ψu) / (1 - ψq) * ((1 - θu) / (1 - θq) * π * χq)) := by
  obtain ⟨a1, a2, a3, a4⟩ := unif_values hθ (by linarith)
  obtain ⟨b1, b2, b3, b4⟩ := unif_values hψ (by linarith)
  rw [a1, a2, a3, a4, b1, b2, b3, b4]
  have h1 : (1 - θq) ≠ 0 := by linarith
  have h2 : θu ≠ 0 := by linarith
  have h3 : (1 - ψq) ≠ 0 := by linarith
  have h4 : ψu ≠ 0 := by linarith
  refine ⟨?_, ?_, ?_, ?_⟩ <;> field_simp <;> ring

/-- 8.3.1 B. With wages equal to marginal product there is no equilibrium with
positive investment and equal beliefs when the group shares differ.
`Z = (1 − γ) π^γ` and `R` is the integral of the wage against
`f_u^W − f_q^W`, both non-zero at positive investment. -/
theorem wages_no_homogeneous {GW GE : ℝ → ℝ} {lA lB π δA δB Z R : ℝ} (hl : lA ≠ lB)
    (hGW : Function.Injective GW) (hGE : Function.Injective GE) (hZ : Z ≠ 0) (hR : R ≠ 0) :
    ¬ (π = GW (δA * R) ∧ π = GW (δB * R) ∧ δA = GE (lA * Z) ∧ δB = GE (lB * Z)) := by
  rintro ⟨e1, e2, e3, e4⟩
  have hδ : δA = δB := mul_right_cancel₀ hR (hGW (e1.symm.trans e2))
  rw [hδ] at e3
  exact hl (mul_right_cancel₀ hZ (hGE (e3.symm.trans e4)))

/-- 8.3.2, eq. (11). With costs per hire the share `λ` cancels from the
firm's return, so group size no longer matters. -/
theorem eq11_indep_lambda {l N C : ℝ} (hl : l ≠ 0) (hC : C ≠ 0) :
    l * N / (l * C) = N / C := by
  field_simp

/-- Control for Proposition 2. With applicants (`F_i^E(t_B) = 0`) the
payoff from B can be non-zero, so the constraint can bind. -/
theorem control_prop2_applicants :
    Pay ⟨id, id, fun _ => 0, fun _ => 0, fun _ => 0, fun _ => 0, 1, 1, 1, 1⟩ 0 1 0 ≠ 0 := by
  norm_num [Pay]

/-- Control for eq. (11). With a fixed cost the return `λ N` does depend on
`λ`. -/
theorem control_eq11_fixed_cost : (1 / 4 : ℝ) * 1 ≠ (1 / 2) * 1 := by norm_num

/-! ## Dynamics in the example, Section 3.F and p. 16 -/

/-- Near `Z` the adjustment map contracts, so `Z` is stable. -/
theorem example_Z_stable {x : ℝ} (h0 : 0 ≤ x) (h1 : x < 1 / 4) : g x = 3 / 4 * x := by
  unfold g clip βex
  rw [if_neg (not_le.2 h1)]
  rw [max_eq_left (by linarith), min_eq_left (by linarith)]

/-- Near `U` the map expands deviations by `7/4`, so `U` is unstable. -/
theorem example_U_unstable {x : ℝ} (h0 : 1 / 4 ≤ x) (h1 : x ≤ 5 / 7) :
    g x - 1 / 3 = 7 / 4 * (x - 1 / 3) := by
  unfold g clip βex
  rw [if_pos h0, max_eq_left (by linarith), min_eq_left (by linarith)]
  ring

/-- Near `S` the map is constant at 1, so `S` is stable. -/
theorem example_S_stable {x : ℝ} (h0 : 5 / 7 ≤ x) : g x = 1 := by
  unfold g clip βex
  rw [if_pos (by linarith), max_eq_left (by linarith), min_eq_right (by linarith)]

end CraigFryer2017
