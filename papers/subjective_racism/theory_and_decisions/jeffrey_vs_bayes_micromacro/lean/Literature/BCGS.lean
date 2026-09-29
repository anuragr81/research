/-
# Bordalo, Coffman, Gennaioli & Shleifer (2016), "Stereotypes"

*Quarterly Journal of Economics* 131(4), 1753-1794.  The local copy is the May 2015
working paper ("First draft, November 2013. This version, May 2015"); every page
cited below is that paper's **printed** page (printed page = PDF page - 1).

Formalization of the paper's own definitions and results, not of Paper B's
claims.

## The paper's setup (pp.11-13)

A type `t` in a finite type space; `π_{t,G} = Pr(T = t | G)` and
`π_{t,-G} = Pr(T = t | -G)`, `-G = Ω \ G`.

* **Definition 1** (p.12): the representativeness of `t` for `G` is
  `R(t,G) = Pr(G | T=t) / Pr(-G | T=t)`; by Bayes' rule it increases in the
  likelihood ratio `π_{t,G}/π_{t,-G}` (eq. 2).
* **Definition 2** (p.13): the decision maker recalls only the `d` most
  representative types (ties included) and holds the *truncated* distribution
  `π^st_{t,G} = π_{t,G} / Σ_{recalled} π_{s,G}` on them, `0` elsewhere (eq. 3).
  Footnote 17 (pp.13-14) gives a smooth variant with weights `δ(π_{t,G}/π_{t,-G})`.

There is a single mechanism: representativeness-driven selective recall
(Definition 1 selects, Definition 2 truncates).  Updating in Section 5 is Bayesian
(fn 33, p.31); there is no sampling distortion.

## What is formalized

* `repr_eq_lr`, `repr_le_iff` -- Definition 1 and eq. (2): representativeness is
  the likelihood ratio times the prior odds, so the two rankings coincide.
* `lr_complement` -- the ranking for `-G` is the reverse of that for `G` (used in
  the proof of Proposition 1, p.41).
* `stereo_sum_one`, `stereo_odds` -- Definition 2: the stereotype is a probability
  distribution, and odds between recalled types are the true odds (p.14).
* `prop1_i_degenerate` -- the proof of Proposition 1(i): a type that is most
  representative for both groups forces the groups to be identical.
* `truncation_raises_mean`, `mlr_mean_order`, `prop2_kernel_of_truth` --
  Proposition 2(i) (pp.19-20): with a monotone likelihood ratio, the stereotype is
  the right tail and `E^st(t|G) > E(t|G) ≥ E(t|-G)`.
* `prop2_strict_needs_mass` -- the strict inequality `E^st(t|G) > E(t|G)` needs a
  truncated type with positive probability; a two-type counterexample.
* `prop4_i` -- Proposition 4(i) (p.32).
* `prop5_threshold` -- Proposition 5(i) (p.33): the exact over-reaction condition
  and `ν < 1/2`.
* `repr2_factor`, `lemma1_i` -- eq. (5) and Lemma 1(i) (pp.26-27).
* `irish_europeans`, `irish_scots` -- the context-dependence example (pp.27-28).
* `welfare_correlation_*` -- a parametric instance of the Section 4.3 claim
  (p.27) that stereotypes produce "an exaggeration of the correlation between
  education and being on welfare"; `welfare_within_group` shows that in the same
  instance the exaggeration is a pooled-population (group-carried) effect.
-/
import Mathlib

namespace Literature.BCGS

open Finset

variable {ι : Type*}

/-! ## Definition 1: representativeness -/

/-- The likelihood ratio `π_{t,G}/π_{t,-G}` (eq. 2, p.12). -/
noncomputable def lr (πG πnG : ι → ℝ) (t : ι) : ℝ := πG t / πnG t

/-- **Definition 1** (p.12).  With `G` having population share `w`,
`Pr(G|t) = w π_{t,G} / Pr(t)` and `Pr(-G|t) = (1-w) π_{t,-G} / Pr(t)`. -/
noncomputable def repr (w : ℝ) (πG πnG : ι → ℝ) (t : ι) : ℝ :=
  (w * πG t / (w * πG t + (1 - w) * πnG t)) / ((1 - w) * πnG t / (w * πG t + (1 - w) * πnG t))

/-- **Eq. (2)**: representativeness is the prior odds times the likelihood ratio. -/
theorem repr_eq_lr (w : ℝ) (πG πnG : ι → ℝ) (t : ι) (hw0 : 0 < w) (hw1 : w < 1)
    (hG : 0 < πG t) (hnG : 0 < πnG t) :
    repr w πG πnG t = w / (1 - w) * lr πG πnG t := by
  have : 0 < 1 - w := by linarith
  unfold repr lr
  field_simp

/-- "Representativeness increases in the likelihood ratio": the two rankings of
types coincide. -/
theorem repr_le_iff (w : ℝ) (πG πnG : ι → ℝ) (t t' : ι) (hw0 : 0 < w) (hw1 : w < 1)
    (hG : 0 < πG t) (hnG : 0 < πnG t) (hG' : 0 < πG t') (hnG' : 0 < πnG t') :
    repr w πG πnG t ≤ repr w πG πnG t' ↔ lr πG πnG t ≤ lr πG πnG t' := by
  rw [repr_eq_lr w πG πnG t hw0 hw1 hG hnG, repr_eq_lr w πG πnG t' hw0 hw1 hG' hnG']
  have : 0 < w / (1 - w) := div_pos hw0 (by linarith)
  exact mul_le_mul_iff_of_pos_left this

/-- The likelihood ratio for `-G` is the reciprocal of that for `G`, so the
representativeness ranking for `-G` is the reverse of that for `G` (p.41). -/
theorem lr_complement (πG πnG : ι → ℝ) (t : ι) : lr πnG πG t = (lr πG πnG t)⁻¹ := by
  unfold lr; rw [inv_div]

/-! ## Definition 2: the truncated stereotype -/

/-- **Definition 2, eq. (3)** (p.13): the stereotype recalls the types in `S` and
renormalises the true probabilities on them. -/
noncomputable def stereo [DecidableEq ι] (π : ι → ℝ) (S : Finset ι) (t : ι) : ℝ :=
  if t ∈ S then π t / ∑ s ∈ S, π s else 0

/-- The stereotype is a probability distribution over any type space containing
the recalled types. -/
theorem stereo_sum_one [DecidableEq ι] (π : ι → ℝ) (S U : Finset ι) (hSU : S ⊆ U)
    (hpos : ∑ s ∈ S, π s ≠ 0) : ∑ t ∈ U, stereo π S t = 1 := by
  unfold stereo
  rw [← Finset.sum_filter, Finset.filter_mem_eq_inter, Finset.inter_eq_right.mpr hSU,
    ← Finset.sum_div, div_self hpos]

/-- "Conditional on coming to mind, the assessed odds ratios of any two types is
consistent with the DM's experience" (p.14). -/
theorem stereo_odds [DecidableEq ι] (π : ι → ℝ) (S : Finset ι) {t t' : ι} (ht : t ∈ S)
    (ht' : t' ∈ S) : stereo π S t * π t' = stereo π S t' * π t := by
  unfold stereo; rw [if_pos ht, if_pos ht']; ring

/-! ## Proposition 1 -/

/-- **Proof of Proposition 1(i)** (pp.17, 41).  If one type `t₀` is simultaneously
most representative for `G` and for `-G` (i.e. maximises and minimises the
likelihood ratio), the two distributions coincide.  So when groups share a modal
type, that type can be the most representative one for both only in the
degenerate case `π_G = π_{-G}`, where every type ties. -/
theorem prop1_i_degenerate (U : Finset ι) (πG πnG : ι → ℝ) (t₀ : ι)
    (hpos : ∀ t ∈ U, 0 < πnG t) (hG : ∑ t ∈ U, πG t = 1) (hnG : ∑ t ∈ U, πnG t = 1)
    (hmax : ∀ t ∈ U, lr πG πnG t ≤ lr πG πnG t₀)
    (hmin : ∀ t ∈ U, lr πG πnG t₀ ≤ lr πG πnG t) :
    ∀ t ∈ U, πG t = πnG t := by
  set r := lr πG πnG t₀
  have hr : ∀ t ∈ U, πG t = r * πnG t := fun t ht => by
    have h := le_antisymm (hmax t ht) (hmin t ht)
    unfold lr at h
    rw [div_eq_iff (hpos t ht).ne'] at h
    exact h
  have : r = 1 := by
    have := Finset.sum_congr rfl hr
    rw [hG, ← Finset.mul_sum, hnG, mul_one] at this
    exact this.symm
  intro t ht; rw [hr t ht, this, one_mul]

/-! ## Proposition 2: the kernel of truth -/

/-- Mean of `x` under weights `π` on `S`. -/
noncomputable def mean (π x : ι → ℝ) (S : Finset ι) : ℝ :=
  (∑ t ∈ S, π t * x t) / ∑ t ∈ S, π t

/-- **Truncating the lower tail raises the mean.**  If every recalled type has a
higher value than every forgotten type, and both parts carry positive mass, the
mean over the recalled set strictly exceeds the mean over all types. -/
theorem truncation_raises_mean [DecidableEq ι] (U S : Finset ι) (π x : ι → ℝ)
    (hSU : S ⊆ U) (hπ : ∀ t ∈ U, 0 ≤ π t)
    (hsep : ∀ s ∈ S, ∀ l ∈ U \ S, x l < x s)
    (hS : 0 < ∑ t ∈ S, π t) (hL : 0 < ∑ t ∈ U \ S, π t) :
    mean π x U < mean π x S := by
  unfold mean
  have hU : ∀ f : ι → ℝ, ∑ t ∈ U, f t = ∑ t ∈ U \ S, f t + ∑ t ∈ S, f t :=
    fun f => (Finset.sum_sdiff hSU).symm
  rw [hU (fun t => π t * x t), hU π, div_lt_div_iff₀ (by linarith) hS]
  -- reduce to  ML * BS < MS * BL
  suffices h : (∑ t ∈ U \ S, π t * x t) * ∑ t ∈ S, π t
      < (∑ t ∈ S, π t * x t) * ∑ t ∈ U \ S, π t by nlinarith
  rw [Finset.sum_mul_sum, Finset.sum_mul_sum, Finset.sum_comm (s := S)]
  rw [← sub_pos, ← Finset.sum_sub_distrib]
  obtain ⟨l, hl, hlpos⟩ := (Finset.sum_pos_iff_of_nonneg
    (fun t ht => hπ t (Finset.sdiff_subset ht))).1 hL
  obtain ⟨s, hs, hspos⟩ := (Finset.sum_pos_iff_of_nonneg (fun t ht => hπ t (hSU ht))).1 hS
  apply Finset.sum_pos'
  · intro l' hl'
    rw [← Finset.sum_sub_distrib]
    apply Finset.sum_nonneg
    intro s' hs'
    have := hsep s' hs' l' hl'
    have h1 := hπ l' (Finset.sdiff_subset hl')
    have h2 := hπ s' (hSU hs')
    nlinarith [mul_nonneg h1 h2]
  · refine ⟨l, hl, ?_⟩
    rw [← Finset.sum_sub_distrib]
    apply Finset.sum_pos'
    · intro s' hs'
      have := hsep s' hs' l hl
      have h1 := hπ l (Finset.sdiff_subset hl)
      have h2 := hπ s' (hSU hs')
      nlinarith [mul_nonneg h1 h2]
    · refine ⟨s, hs, ?_⟩
      have := hsep s hs l hl
      nlinarith [mul_pos hlpos hspos]

/-- **MLR implies mean dominance** (proof of Proposition 2, p.41: "it follows that
`π_{t,-G}` first order stochastically dominates ..."), in cross-multiplied form:
if `x t' < x t` implies `π_{t',G} π_{t,-G} ≤ π_{t,G} π_{t',-G}` (monotone
likelihood ratio, no division needed), then `E(t|G) ≥ E(t|-G)`. -/
theorem mlr_mean_order (U : Finset ι) (πG πnG x : ι → ℝ) (hG : ∑ t ∈ U, πG t = 1)
    (hnG : ∑ t ∈ U, πnG t = 1)
    (hmlr : ∀ t ∈ U, ∀ t' ∈ U, x t' < x t → πG t' * πnG t ≤ πG t * πnG t') :
    ∑ t ∈ U, πnG t * x t ≤ ∑ t ∈ U, πG t * x t := by
  -- E_G - E_{-G} = Σ_{t,t'} πG t πnG t' (x t - x t'), symmetrised
  have e : ∑ t ∈ U, πG t * x t - ∑ t ∈ U, πnG t * x t
      = ∑ t ∈ U, ∑ t' ∈ U, πG t * πnG t' * (x t - x t') := by
    have h1 : ∑ t ∈ U, πG t * x t = (∑ t ∈ U, πG t * x t) * ∑ t' ∈ U, πnG t' := by
      rw [hnG, mul_one]
    have h2 : ∑ t ∈ U, πnG t * x t = (∑ t ∈ U, πG t) * ∑ t' ∈ U, πnG t' * x t' := by
      rw [hG, one_mul]
    rw [h1, h2, Finset.sum_mul_sum, Finset.sum_mul_sum, ← Finset.sum_sub_distrib]
    refine Finset.sum_congr rfl fun t _ => ?_
    rw [← Finset.sum_sub_distrib]
    exact Finset.sum_congr rfl fun t' _ => by ring
  have e2 : 2 * ∑ t ∈ U, ∑ t' ∈ U, πG t * πnG t' * (x t - x t')
      = ∑ t ∈ U, ∑ t' ∈ U, (πG t * πnG t' - πG t' * πnG t) * (x t - x t') := by
    have hc : ∑ t ∈ U, ∑ t' ∈ U, πG t * πnG t' * (x t - x t')
        = ∑ t ∈ U, ∑ t' ∈ U, πG t' * πnG t * (x t' - x t) := Finset.sum_comm
    rw [two_mul]
    nth_rewrite 2 [hc]
    rw [← Finset.sum_add_distrib]
    refine Finset.sum_congr rfl fun t _ => ?_
    rw [← Finset.sum_add_distrib]
    exact Finset.sum_congr rfl fun t' _ => by ring
  have hnn : 0 ≤ ∑ t ∈ U, ∑ t' ∈ U, (πG t * πnG t' - πG t' * πnG t) * (x t - x t') := by
    apply Finset.sum_nonneg; intro t ht
    apply Finset.sum_nonneg; intro t' ht'
    rcases lt_trichotomy (x t') (x t) with h | h | h
    · have := hmlr t ht t' ht' h
      exact mul_nonneg (by linarith) (by linarith)
    · rw [h, sub_self, mul_zero]
    · have := hmlr t' ht' t ht h
      exact mul_nonneg_of_nonpos_of_nonpos (by linarith) (by linarith)
  linarith

/-- **Proposition 2(i)** (pp.19-20).  Types are distinct cardinal values `x`
(`t₁ < ... < t_N`, p.11); the likelihood ratio is monotone in `x` (division-free
form `hmlr`); the stereotype `S` consists of the types whose likelihood ratio is
at least a cut-off `ρ` (Definition 2 with ties included).  If both the stereotype
and the truncated tail carry positive `G`-mass, then
`E(t|-G) ≤ E(t|G) < E^st(t|G)`: the stereotype gets the direction of the group
difference right and exaggerates it (the "kernel of truth", p.20). -/
theorem prop2_kernel_of_truth [DecidableEq ι] (U : Finset ι) (πG πnG x : ι → ℝ) (ρ : ℝ)
    (hπG : ∀ t ∈ U, 0 ≤ πG t) (hπnG : ∀ t ∈ U, 0 < πnG t)
    (hG : ∑ t ∈ U, πG t = 1) (hnG : ∑ t ∈ U, πnG t = 1)
    (hinj : ∀ t ∈ U, ∀ t' ∈ U, x t = x t' → t = t')
    (hmlr : ∀ t ∈ U, ∀ t' ∈ U, x t' < x t → πG t' * πnG t ≤ πG t * πnG t')
    (hS : 0 < ∑ t ∈ U.filter (fun t => ρ ≤ lr πG πnG t), πG t)
    (hL : 0 < ∑ t ∈ U \ U.filter (fun t => ρ ≤ lr πG πnG t), πG t) :
    mean πnG x U ≤ mean πG x U
      ∧ mean πG x U < mean πG x (U.filter (fun t => ρ ≤ lr πG πnG t)) := by
  refine ⟨?_, ?_⟩
  · unfold mean; rw [hG, hnG, div_one, div_one]
    exact mlr_mean_order U πG πnG x hG hnG hmlr
  · apply truncation_raises_mean U _ πG x (Finset.filter_subset _ _) hπG _ hS hL
    intro s hs l hl
    rw [Finset.mem_filter] at hs
    rw [Finset.mem_sdiff, Finset.mem_filter] at hl
    have hlr : lr πG πnG l < ρ := by
      by_contra h; exact hl.2 ⟨hl.1, not_lt.mp h⟩
    by_contra hx
    rw [not_lt] at hx
    rcases hx.lt_or_eq with hx | hx
    · have := hmlr l hl.1 s hs.1 hx
      have h1 : lr πG πnG s ≤ lr πG πnG l := by
        unfold lr
        rw [div_le_div_iff₀ (hπnG s hs.1) (hπnG l hl.1)]
        linarith
      linarith [hs.2]
    · have := hinj s hs.1 l hl.1 hx
      subst this
      linarith [hs.2]

/-- **The strict inequality needs a truncated type with positive mass.**  Two types
`x = 0, 1`; `π_G = (0, 1)`, `π_{-G} = (1/2, 1/2)`.  The likelihood ratio `(0, 2)` is
strictly increasing, `d = 1` recalls type `1` only, yet `E^st(t|G) = E(t|G) = 1`.
Proposition 2 states `E^st(t|G) > E(t|G)` without this proviso. -/
theorem prop2_strict_needs_mass :
    let πG : Fin 2 → ℝ := ![0, 1]
    let πnG : Fin 2 → ℝ := ![1 / 2, 1 / 2]
    let x : Fin 2 → ℝ := ![0, 1]
    lr πG πnG 0 < lr πG πnG 1
      ∧ mean πG x (univ.filter (fun t => (2 : ℝ) ≤ lr πG πnG t)) = mean πG x univ := by
  intro πG πnG x
  have hf : univ.filter (fun t : Fin 2 => (2 : ℝ) ≤ lr πG πnG t) = {1} := by
    ext t; fin_cases t <;> simp [lr, πG, πnG]
  refine ⟨by simp [lr, πG, πnG], ?_⟩
  rw [hf]
  simp [mean, πG, x, Fin.sum_univ_two]

/-! ## Section 5: reaction to information -/

/-- **Proposition 4(i)** (p.32).  If `t` is non-representative for `G` under the
Dirichlet priors (`α_{t,G} < α_{t,-G}`), `n` observations of `t` in both groups do
not make it representative, for every `n`. -/
theorem prop4_i (αG αnG n : ℝ) (hα : 0 < αnG) (h : αG / αnG < 1) (hn : 0 ≤ n) :
    (αG + n) / (αnG + n) < 1 := by
  rw [div_lt_one hα] at h
  rw [div_lt_one (by linarith)]
  linarith

/-- **Proposition 5(i)** (p.33, proof pp.44-45).  `a = α_{t,G}`, `A_d` the prior mass
of the recalled types, `A` the total (`0 < a ≤ A_d < A`).  After one observation
of `t` the stereotyper's assessed probability rises by more than the Bayesian's
iff `a/A < A_d/(1 + A_d + A)`, and that threshold `ν` is below `1/2`. -/
theorem prop5_threshold (a Ad A : ℝ) (ha : 0 < a) (hd : a ≤ Ad) (hA : Ad < A) :
    ((a + 1) / (Ad + 1) - a / Ad > (a + 1) / (A + 1) - a / A
      ↔ a / A < Ad / (1 + Ad + A))
    ∧ Ad / (1 + Ad + A) < 1 / 2 := by
  have hAd : 0 < Ad := by linarith
  have hA0 : 0 < A := by linarith
  refine ⟨?_, ?_⟩
  · have key : ((a + 1) / (Ad + 1) - a / Ad) - ((a + 1) / (A + 1) - a / A)
        = (A - Ad) * (A * Ad - a * (A + Ad + 1)) / (A * Ad * (A + 1) * (Ad + 1)) := by
      field_simp; ring
    rw [gt_iff_lt, ← sub_pos, key, div_lt_div_iff₀ hA0 (by positivity)]
    have hden : 0 < A * Ad * (A + 1) * (Ad + 1) := by positivity
    have hgap : 0 < A - Ad := by linarith
    constructor
    · intro h
      have := (div_pos_iff_of_pos_right hden).1 h
      have := (pos_iff_pos_of_mul_pos this).1 hgap
      nlinarith
    · intro h
      apply div_pos _ hden
      apply mul_pos hgap
      nlinarith
  · rw [div_lt_iff₀ (by positivity)]; linarith

/-! ## Section 4.3: multidimensional types -/

/-- **Eq. (5)** (p.26): with joint = marginal × conditional, representativeness of
`(t₁,t₂)` factors into the marginal ratio times the conditional ratio. -/
theorem repr2_factor (m mn c cn : ℝ) : (m * c) / (mn * cn) = (m / mn) * (c / cn) :=
  mul_div_mul_comm m c mn cn

/-- **Lemma 1(i)** (pp.26-27): if `t₂ | t₁` has the same law in both groups, the
representativeness of `(t₁,t₂)` is that of `t₁` alone, so the stereotype is formed
along `t₁`. -/
theorem lemma1_i (m mn c : ℝ) (hc : c ≠ 0) : (m * c) / (mn * c) = m / mn := by
  rw [mul_div_mul_right _ _ hc]

/-- **Irish vs Europeans** (p.28): hair colour and religion independent in both
groups, equal catholic share `c`, more red hair among the Irish (`r_e < r_i`):
`R(r,c) = R(r,ô) > R(o,c) = R(o,ô)` -- the stereotype is formed along hair colour. -/
theorem irish_europeans (ri re c : ℝ) (h : re < ri) (hre : 0 < re) (hri : ri < 1)
    (hc0 : 0 < c) (hc1 : c < 1) :
    ri * c / (re * c) = ri * (1 - c) / (re * (1 - c))
      ∧ (1 - ri) * c / ((1 - re) * c) = (1 - ri) * (1 - c) / ((1 - re) * (1 - c))
      ∧ (1 - ri) * c / ((1 - re) * c) < ri * c / (re * c) := by
  have h1c : (1 - c) ≠ 0 := by linarith
  refine ⟨by rw [lemma1_i _ _ _ hc0.ne', lemma1_i _ _ _ h1c],
    by rw [lemma1_i _ _ _ hc0.ne', lemma1_i _ _ _ h1c], ?_⟩
  rw [lemma1_i _ _ _ hc0.ne', lemma1_i _ _ _ hc0.ne',
    div_lt_div_iff₀ (by linarith) hre]
  nlinarith

/-- **Irish vs Scots** (p.28): equal red-hair share `r`, more catholics among the
Irish (`c_s < c_i`): `R(r,c) = R(o,c) > R(r,ô) = R(o,ô)` -- the stereotype moves to
religion. -/
theorem irish_scots (r ci cs : ℝ) (h : cs < ci) (hcs : 0 < cs) (hci : ci < 1)
    (hr0 : 0 < r) (hr1 : r < 1) :
    r * ci / (r * cs) = (1 - r) * ci / ((1 - r) * cs)
      ∧ r * (1 - ci) / (r * (1 - cs)) < r * ci / (r * cs) := by
  have h1r : (1 - r) ≠ 0 := by linarith
  refine ⟨by rw [mul_div_mul_left _ _ hr0.ne', mul_div_mul_left _ _ h1r], ?_⟩
  rw [mul_div_mul_left _ _ hr0.ne', mul_div_mul_left _ _ hr0.ne',
    div_lt_div_iff₀ (by linarith) hcs]
  nlinarith

/-! ### Correlation exaggeration (p.27), a parametric instance

Education `e ∈ {0,1}` and welfare `w ∈ {0,1}`; a 2×2 joint law is
`(p₀₀, p₀₁, p₁₀, p₁₁)` with `pₑw`.  Group `G`: `Pr(e=1) = 2/5`,
`Pr(w=1|e=0) = 3/10`, `Pr(w=1|e=1) = 1/10`; group `-G`: `Pr(e=1) = 3/5`,
`Pr(w=1|e=0) = 1/5`, `Pr(w=1|e=1) = 1/20`.  `G` is less educated and, at each
education level, more likely to be on welfare (Lemma 1(ii)).  Equal group sizes.
The `sympy` script recomputes every number below from these primitives. -/

/-- Covariance of `e` and `w` under a 2×2 joint `(p₀₀, p₀₁, p₁₀, p₁₁)`. -/
def cov2 (_p₀₀ p₀₁ p₁₀ p₁₁ : ℚ) : ℚ := p₁₁ - (p₁₀ + p₁₁) * (p₀₁ + p₁₁)

/-- Variance of `e`. -/
def varE (_p₀₀ _p₀₁ p₁₀ p₁₁ : ℚ) : ℚ := (p₁₀ + p₁₁) * (1 - (p₁₀ + p₁₁))

/-- Variance of `w`. -/
def varW (_p₀₀ p₀₁ _p₁₀ p₁₁ : ℚ) : ℚ := (p₀₁ + p₁₁) * (1 - (p₀₁ + p₁₁))

/-- The representativeness ranking for `G` (joint `G` = `(21/50, 9/50, 9/25, 1/25)`,
joint `-G` = `(8/25, 2/25, 57/100, 3/100)`): `(e=0, w=1)` is the most representative
type ("uneducated and on welfare"), and for `-G` it is `(e=1, w=0)` (the ranking for
`-G` is the reverse, `lr_complement`).  The runners-up are `(1,1)` for `G` and
`(0,0)` for `-G`, which fixes the `d = 2` stereotypes. -/
theorem welfare_ranking :
    let rG : ℚ × ℚ × ℚ × ℚ := ((21/50) / (8/25), (9/50) / (2/25), (9/25) / (57/100),
      (1/25) / (3/100))
    rG.1 < rG.2.1 ∧ rG.2.2.2 < rG.2.1 ∧ rG.2.2.1 < rG.2.1
      ∧ rG.2.2.1 < rG.1 ∧ rG.2.2.1 < rG.2.2.2 ∧ rG.1 < rG.2.2.2 := by
  norm_num

/-- **True pooled law**: `(37/100, 13/100, 93/200, 7/200)`; the education-welfare
correlation is negative but far from perfect (`corr² ≈ 0.066`). -/
theorem welfare_correlation_true :
    cov2 (37/100) (13/100) (93/200) (7/200) < 0
      ∧ cov2 (37/100) (13/100) (93/200) (7/200) ^ 2
        < varE (37/100) (13/100) (93/200) (7/200) * varW (37/100) (13/100) (93/200) (7/200) / 10 := by
  norm_num [cov2, varE, varW]

/-- **Stereotyped pooled law, `d = 1`**: each group is recalled as its exemplar,
`G` as `(0,1)` and `-G` as `(1,0)`; the pooled stereotyped law `(0, 1/2, 1/2, 0)`
has **perfect** negative correlation, `cov² = Var(e) Var(w)`. -/
theorem welfare_correlation_stereo_d1 :
    cov2 0 (1/2) (1/2) 0 < 0
      ∧ cov2 0 (1/2) (1/2) 0 ^ 2 = varE 0 (1/2) (1/2) 0 * varW 0 (1/2) (1/2) 0 := by
  norm_num [cov2, varE, varW]

/-- **Stereotyped pooled law, `d = 2`**: `G` recalls `(0,1)` and `(1,1)`, `-G`
recalls `(1,0)` and `(0,0)`; pooled law `(16/89, 9/22, 57/178, 1/11)`.  The squared
correlation (`≈ 0.217`) exceeds three times the true one (`≈ 0.066`). -/
theorem welfare_correlation_stereo_d2 :
    cov2 (16/89) (9/22) (57/178) (1/11) < 0
      ∧ cov2 (37/100) (13/100) (93/200) (7/200) ^ 2
          / (varE (37/100) (13/100) (93/200) (7/200) * varW (37/100) (13/100) (93/200) (7/200))
        * 3
        < cov2 (16/89) (9/22) (57/178) (1/11) ^ 2
          / (varE (16/89) (9/22) (57/178) (1/11) * varW (16/89) (9/22) (57/178) (1/11)) := by
  norm_num [cov2, varE, varW]

/-- **Within a group the truncation removes the association.**  `G`'s `d = 2`
stereotype recalls `(0,1)` and `(1,1)` with probabilities `9/11, 2/11`: everyone
recalled is on welfare, so the within-`G` education-welfare covariance is `0`,
whereas the true within-`G` covariance is `-6/125`.  The exaggerated correlation
of p.27 lives in the population pooled across groups, i.e. it is carried by group
membership.  (For a 2×2 law, `cov2` is the determinant `p₀₀p₁₁ - p₀₁p₁₀`.) -/
theorem welfare_within_group :
    cov2 0 (9/11) 0 (2/11) = 0 ∧ cov2 (21/50) (9/50) (9/25) (1/25) = -6/125
      ∧ cov2 (21/50) (9/50) (9/25) (1/25) = (21/50) * (1/25) - (9/50) * (9/25) := by
  norm_num [cov2]

end Literature.BCGS
