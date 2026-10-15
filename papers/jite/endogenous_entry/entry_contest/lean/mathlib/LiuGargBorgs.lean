import Mathlib

/-!
# Liu, Garg and Borgs, "Strategic Ranking"

Proceedings of the 25th International Conference on Artificial Intelligence and Statistics
(AISTATS), PMLR 151, 2022, 2489-2518.  Read from arXiv:2109.08240v2 (30 pages); page numbers
are PDF pages.

Setup (pp.3-4): a unit mass of applicants with skill rank `θ ∈ [0, 1]` and skill `f(θ)`; an
applicant chooses effort `e ≥ 0` at cost `p(e)`, `p` strictly convex, and scores
`v = g(e) f(θ)`, `g` concave and strictly increasing; the school admits rank `θ` with
probability `λ(θ)`, a step function with `E[λ] = ρ`.  Section 4 multiplies the score by an
environment factor `ψ_A > ψ_B` for two equal groups.

## What is proved

1. Rank preservation, Proposition 2.1 (pp.4, 18-19): the increment lemma for a concave function
   (`concave_increment_anti`) and its convex mirror (`convex_increment_mono`); Lemma C.3
   (`lemma_C3`, p.19); Lemma C.4 (`lemma_C4`, p.20); and the closing contradiction, that an applicant of
   lower skill cannot overtake one of higher skill in equilibrium (`no_overtaking`).
2. Proposition 3.2 (pp.6, 23): with `g(e₀) = 0` the school's private utility
   `g(p⁻¹(ρ/(1 − c))) f(c)` is non-decreasing in the cut-off (`upri_mono`).
3. Proposition 4.4 (pp.8-9, 26): access under a two-level policy,
   `A(c) = ρ (1 − t(c))/(1 − c)` with `t = thres_B`, is below `ρ` when `t(c) > c`
   (`access_lt_rho`); pure randomization has access `ρ` (`access_pure`), where the proof prints
   `A = 0`; and `A` is non-increasing in `c` when `t(c) ≥ c` and `t` rises at least as fast as
   `c` (`access_antitone`), which is what the proof's `∂t/∂c ≥ 1` gives
   (`thres_slope_ge_one`).  Control: without `t(c) ≥ c` the access can rise (`access_needs_floor`).

## Not formalised

Theorem 2.2 (the equilibrium efforts), Propositions 3.1, 3.3, 3.4, 4.2, 4.3 and Appendix B.
The record (`lit/liu_garg_borgs_2022`) reads them from the paper.
-/

open Set

namespace LiuGargBorgs2022

/-! ## 1. Rank preservation -/

/-- For a concave `g`, the increase over an interval of length `h` does not grow as the
    interval moves right: `g(y + h) − g(y) ≤ g(x + h) − g(x)` for `x ≤ y`. -/
theorem concave_increment_anti {g : ℝ → ℝ} {D : Set ℝ} (hg : ConcaveOn ℝ D g)
    {x y h : ℝ} (hx : x ∈ D) (hyh : y + h ∈ D) (hxy : x ≤ y) (hh : 0 ≤ h) :
    g (y + h) - g y ≤ g (x + h) - g x := by
  rcases eq_or_lt_of_le hh with h0 | hpos
  · subst h0; simp
  rcases eq_or_lt_of_le hxy with hxy' | hlt
  · subst hxy'; simp
  set L := y + h - x with hL
  have hLpos : 0 < L := by rw [hL]; linarith
  set a := h / L with ha
  have ha0 : 0 ≤ a := div_nonneg hh hLpos.le
  have ha1 : a ≤ 1 := by rw [ha, div_le_one hLpos]; linarith
  -- y = (1 - a) x + a (y + h)? no: y = a x + (1 - a)(y + h) and x + h = (1 - a) x + a (y + h)
  have hy_eq : a * x + (1 - a) * (y + h) = y := by
    rw [ha]; field_simp; ring
  have hxh_eq : (1 - a) * x + a * (y + h) = x + h := by
    rw [ha]; field_simp; ring
  have h1 := hg.2 hx hyh ha0 (by linarith : (0:ℝ) ≤ 1 - a) (by ring)
  have h2 := hg.2 hx hyh (by linarith : (0:ℝ) ≤ 1 - a) ha0 (by ring)
  simp only [smul_eq_mul] at h1 h2
  rw [hy_eq] at h1
  rw [hxh_eq] at h2
  nlinarith

/-- The mirror for a convex `p`: the increase over an interval of length `h` does not shrink as
    the interval moves right. -/
theorem convex_increment_mono {p : ℝ → ℝ} {D : Set ℝ} (hp : ConvexOn ℝ D p)
    {x y h : ℝ} (hx : x ∈ D) (hyh : y + h ∈ D) (hxy : x ≤ y) (hh : 0 ≤ h) :
    p (x + h) - p x ≤ p (y + h) - p y := by
  have := concave_increment_anti hp.neg hx hyh hxy hh
  simp only [Pi.neg_apply] at this
  linarith

/-- Lemma C.3 (p.19): if the higher-skill applicant `ω` keeps effort `e` rather than `et`, and the
    lower-skill applicant `ω'` keeps `e'` rather than `et'`, where `et` gives `ω` the reward of
    `ω'` and `et'` gives `ω'` the reward of `ω`, then `p(e') − p(et') ≤ p(et) − p(e)`. -/
theorem lemma_C3 (p : ℝ → ℝ) {r r' e e' et et' : ℝ}
    (h4 : r - p e ≥ r' - p et) (h5 : r' - p e' ≥ r - p et') :
    p e' - p et' ≤ p et - p e := by linarith

/-- Lemma C.4 (p.20): with `g` concave and strictly increasing and `e < et'`, if the lower-skill
    applicant's step `[et', e']` raises `g` by more than the higher-skill applicant's step
    `[e, et]`, it is longer. -/
theorem lemma_C4 {g : ℝ → ℝ} (hg : ConcaveOn ℝ (Ici 0) g) (hmono : StrictMonoOn g (Ici 0))
    {e et e' et' : ℝ} (he : 0 ≤ e) (hee : e ≤ et) (hee' : e ≤ et')
    (hgap : g et - g e < g e' - g et') (he' : 0 ≤ e') : et - e < e' - et' := by
  by_contra hcon
  push_neg at hcon
  have hle : e' ≤ et' + (et - e) := by linarith
  have hinc := concave_increment_anti hg (by simpa using he) (by simp; linarith) hee'
    (by linarith : (0:ℝ) ≤ et - e)
  have hsimp : e + (et - e) = et := by ring
  rw [hsimp] at hinc
  have hmon : g e' ≤ g (et' + (et - e)) :=
    hmono.monotoneOn (by simpa using he') (by simp; linarith) hle
  linarith

/-- The closing step of Proposition 2.1 (p.19): with `p` convex and strictly increasing, the
    three conclusions `e' − et' > et − e`, `p(e') − p(et') ≤ p(et) − p(e)` and `et' ≥ e` cannot hold
    together, so a lower-skill applicant does not overtake a higher-skill one. -/
theorem no_overtaking {p : ℝ → ℝ} (hp : ConvexOn ℝ (Ici 0) p) (hmono : StrictMonoOn p (Ici 0))
    {e et e' et' : ℝ} (he : 0 ≤ e) (hee : e ≤ et) (hee' : e ≤ et')
    (hlen : et - e < e' - et') (hcost : p e' - p et' ≤ p et - p e) : False := by
  have hinc := convex_increment_mono hp (by simpa using he) (by simp; linarith) hee'
    (by linarith : (0:ℝ) ≤ et - e)
  have hsimp : e + (et - e) = et := by ring
  rw [hsimp] at hinc
  have hlt : p (et' + (et - e)) < p e' :=
    hmono (by simp; linarith) (by simp; linarith) (by linarith)
  linarith

/-! ## 2. Proposition 3.2 -/

/-- With `g(e₀) = 0`, the school's private utility under a two-level policy is
    `g(p⁻¹(ρ/(1 − c))) f(c)`; with `g ∘ p⁻¹` non-decreasing and non-negative and `f`
    non-decreasing and non-negative, it is non-decreasing in `c ∈ [0, 1)`. -/
theorem upri_mono {G f : ℝ → ℝ} {ρ : ℝ} (hρ : 0 < ρ) (hG : Monotone G) (hG0 : ∀ x, 0 ≤ G x)
    (hf : Monotone f) (hf0 : ∀ x, 0 ≤ f x) {c₁ c₂ : ℝ} (h12 : c₁ ≤ c₂) (h2 : c₂ < 1) :
    G (ρ / (1 - c₁)) * f c₁ ≤ G (ρ / (1 - c₂)) * f c₂ := by
  have hlev : ρ / (1 - c₁) ≤ ρ / (1 - c₂) :=
    div_le_div_of_nonneg_left hρ.le (by linarith) (by linarith)
  exact mul_le_mul (hG hlev) (hf h12) (hf0 _) (hG0 _)

/-! ## 3. Proposition 4.4: access -/

/-- Access of the disadvantaged group under a two-level policy with cut-off `c` and group
    threshold `t = thres_B(c)`: `A = ρ/(1 − c) · (1 − t)` (p.26). -/
noncomputable def access (ρ c t : ℝ) : ℝ := ρ / (1 - c) * (1 - t)

/-- Pure randomization admits everyone with probability `ρ`, so its access is `ρ`; the proof on
    p.26 prints `A = 0`, which contradicts the proposition it proves. -/
theorem access_pure (ρ : ℝ) : access ρ 0 0 = ρ := by unfold access; simp

theorem access_lt_rho {ρ c t : ℝ} (hρ : 0 < ρ) (hc : c < 1) (ht : c < t) :
    access ρ c t < ρ := by
  unfold access
  have h1c : 0 < 1 - c := by linarith
  rw [div_mul_eq_mul_div, div_lt_iff₀ h1c]
  nlinarith

/-- Access is non-increasing in the cut-off when the group threshold lies at or above the
    cut-off and rises at least as fast. -/
theorem access_antitone {ρ c₁ c₂ t₁ t₂ : ℝ} (hρ : 0 ≤ ρ) (h12 : c₁ ≤ c₂) (h2 : c₂ < 1)
    (hfloor : c₁ ≤ t₁) (hslope : c₂ - c₁ ≤ t₂ - t₁) :
    access ρ c₂ t₂ ≤ access ρ c₁ t₁ := by
  unfold access
  have hy1 : 0 < 1 - c₁ := by linarith
  have hy2 : 0 < 1 - c₂ := by linarith
  rw [div_mul_eq_mul_div, div_mul_eq_mul_div, div_le_div_iff₀ hy2 hy1]
  have key : (1 - t₂) * (1 - c₁) ≤ (1 - t₁) * (1 - c₂) := by
    nlinarith [mul_nonneg (by linarith : (0:ℝ) ≤ c₂ - c₁) (by linarith : (0:ℝ) ≤ t₁ - c₁)]
  nlinarith [mul_le_mul_of_nonneg_left key hρ]

/-- The proof's step `∂ thres_B/∂c ≥ 1` (p.26): with `a = (f⁻¹)'(·/ψ_A)/ψ_A` and
    `b = (f⁻¹)'(·/ψ_B)/ψ_B`, `b ≥ a > 0` gives `b / ((a + b)/2) ≥ 1`. -/
theorem thres_slope_ge_one {a b : ℝ} (ha : 0 < a) (hab : a ≤ b) : 1 ≤ b / ((a + b) / 2) := by
  rw [le_div_iff₀ (by linarith)]
  linarith

/-- Control: without the floor `t(c) ≥ c` the access can rise in the cut-off. -/
theorem access_needs_floor :
    access 1 0 (-1) < access 1 (1/2) (-1/2) ∧ (1/2 : ℝ) - 0 ≤ -1/2 - (-1) ∧ ¬ ((0:ℝ) ≤ -1) := by
  unfold access; norm_num

end LiuGargBorgs2022
