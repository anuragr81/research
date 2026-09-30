/-
# Wilson, "Bounded Memory and Biases in Information Processing"

Published *Econometrica* 82(6), 2014.  The copy read is the Princeton
working-paper **draft dated April 29, 2003** (55 pp.); every theorem, lemma,
equation and page number below is the **draft's**.  The published version was
not available, and its numbering and statements may differ.

Formalization of the paper's own model and results, not of Paper B's claims,
plus one clearly separated project question at the end.

## The model (Section 2, pp.7-10)

A binary state `S ∈ {L, H}`, equally likely; i.i.d. binary signals `s ∈ {l, h}`
with `Pr(l|L) = Pr(h|H) = ρ > 1/2`; termination with probability `η` before
each signal.  A memory process on `𝒩 = {1, …, N}` is `(g₀, σ, a)`: an initial
distribution, a transition rule `σ(i, s)(j)` and an action rule `a(i)`.  It
induces `τ^S_{ij} = Pr(l|S) σ(i,l)(j) + Pr(h|S) σ(i,h)(j)`, the ending
distribution `f^S = ∑_t η(1-η)^t g₀ (T^S)^t` (eq. (1)), the payoff
`Π = ½ ∑ᵢ (f^H_i a(i) + f^L_i (1 - a(i)))` and beliefs `π(i) = f^H_i/(f^H_i + f^L_i)`.

Encoded as `Memory N` (with `Memory.Valid`), `trans`, `IsEndDist` (the
stationary form `f = ηg₀ + (1-η) f T^S` that Lemma 1 derives), `payoff`.

## What is proved

* **Lemma 1** (p.36), existence and uniqueness of `f^S` for `0 < η ≤ 1`
  (`endDist_exists`, `endDist_unique`, via `homog_zero`), and `∑ f^S = 1`.
* **One-step informativeness** (p.15): any transition multiplies the likelihood
  ratio by at most `ρ/(1-ρ)` (`trans_ratio_up`, `trans_ratio_down`), with
  equality iff the move is never taken after the opposing signal
  (`trans_ratio_up_eq_iff`).
* **Eq. (4)** (p.15) for every three-state rule without jumps between the
  extremes (`eq4_balance`, `eq4`), and its consequence that one step up raises
  the likelihood ratio by at most `(ρ/(1-ρ))²` (`eq4_bound`).
* **Theorem 3 at `N = 3` within its own family.**  `rule3 γ` is the rule of
  Theorem 3 (ii)-(iv) with leaving probability `γ` at both extremes.  Its ending
  distribution and payoff are solved in closed form (`rule3_endDist`,
  `rule3_payoff_endDist`).  For `0 < η < 1` the unique optimal `γ` is
  `γ*(η) = (√(2η-η²) - η)/(1-η)` (`rule3_optimal`), and `η ≤ γ*² ≤ 2η`
  (`gstar_sq_bounds`): the leaving probability is positive, tends to `0`, and
  more slowly than `η` (Theorem 3 (i); pp.4-5, 15-16).  Neither absorbing
  extremes (`γ = 0`) nor the pure strategy (`γ = 1`) is optimal.
* **Corollary to Theorem 3 at `N = 3`** (pp.12-13): (i) for every `ε` and small
  `η`, `rule3 γ*(η)` earns within `ε` of `(1 + ((1-ρ)/ρ)²)⁻¹` and never reaches
  it (`corollary_i_N3`, `rule3_payoff_lt_beta`, `rule3_gap` exact); since
  `Π*(η,3)` is a maximum this is the **lower half** of Corollary (i).
  (ii) along `γ*(η)`, `f₃^H/f₃^L → (ρ/(1-ρ))²` (`corollary_ii_N3`, with the exact
  gap `rule3_lr3_gap`), `f₂^H = f₂^L` (`rule3_mid_equal`), and state `1` mirrors
  state `3` (`rule3_mirror`).  With absorbing extremes the limit payoff is `ρ`,
  the gambler's-ruin value "as if there were only `(N-1)/2` memory states"
  (`rule3_absorbing`, `gamblersRuin_eq`, `absorbing_lt_beta`, p.12).
* **Lemma 4** (pp.39-40): its identity and inequality with the equality case
  (`lemma4_identity`, `lemma4`, `lemma4_eq_iff`), and the p.16 sketch's
  `α* = 1` (`alpha_star_one`).
* **Theorem 6** (pp.23-25), given Corollary (ii)'s limiting beliefs `lrLimit`:
  one memory step moves the odds by `(ρ/(1-ρ))²`, "as if he had received two
  h-signals" (`lrLimit_step`, `lrLimit_N3`); (i) overconfidence after short
  sequences (`thm6_i`); (iii) underconfidence when `δ > N - 1` (`thm6_iii`).
* **Theorem 7 (ii)**, its informational core (pp.26-27): a move's likelihood
  ratio is at most the most extreme signal's, strictly less if the move is ever
  made on a weaker (e.g. uninformative) signal (`move_ratio_le`,
  `move_ratio_lt`).
* **Sections 4-5 on the short-run skeleton** (`skel`, `run`: interior moves by
  Theorem 3 (ii), extremes stay).
  - **Theorem 4 (i)**, the pathwise step of its proof (p.19): moving a block of
    `h`-signals from the end to the start never lowers the final memory state,
    for every sequence and block length (`first_impressions_pathwise`), and can
    raise it (`first_impressions_strict`).
  - **Belief polarization**: the p.21 example (all six orderings, as stated)
    and the p.6 example (`section4_example`, `intro_example`).
  - **Theorem 5 (i)** (p.22): the draft's condition `t ≥ N - 1 - (k - j)` is
    **not sufficient**: for every kernel satisfying Theorem 3 (ii), `N = 5`,
    `(j, k) = (2, 4)`, `t = 2` gives probability `0`
    (`thm5_draft_counterexample`; also `N = 7`, `(3, 5)`, `t = 4`).  The proof's
    own sequence works for `t ≥ 2j - 2 + N - k` (`thm5_i_corrected`), and at
    `N = 5` the exact threshold is `min(2j - 1, 2(N - k) + 1)`
    (`thm5_threshold_N5`).
* **Project question (not Wilson's)**: why the model is silent on
  cross-attribute association, and what a two-attribute extension needs
  (`assoc_sign_fixed`, `marginals_do_not_fix_association`, `kernel_invariant`,
  `y_dependent_signal_moves_association`).

## Not formalized

Theorem 1 (optimal ⇒ incentive compatible; Definition 2); Theorem 2
(existence, monotonicity of `Π*`); Theorem 3 for general `N`, and optimality of
`rule3` against *all* three-state rules (so the upper half of Corollary (i),
`Π*(η, N) < (1 + ((1-ρ)/ρ)^{N-1})⁻¹`, is not proved: `rule3_payoff_lt_beta` is
for the family only); Lemmas 2, 3, 5-9 and Claims 0-4; Corollary (ii) for
`N > 3` (Theorem 6 takes it as given through `lrLimit`); the expectation and
`η`-limit steps of Theorem 4 (i) and all of Theorem 4 (ii) (ergodic); Theorem 5
(ii); Theorem 6 (ii); Theorem 7 beyond the one-move bound, and its Corollary.
-/
import Mathlib

set_option linter.unusedSectionVars false

namespace Literature.Wilson

open Finset

/-! ## Section 2: the model -/

/-- The binary signal `s ∈ {l, h}`. -/
inductive Sig
  | l
  | h
  deriving DecidableEq, Repr, Fintype

/-- The state of the world `S ∈ {L, H}`. -/
inductive World
  | L
  | H
  deriving DecidableEq, Repr

/-- `Pr(s | S)`, with `Pr(l|L) = Pr(h|H) = ρ` (p.7). -/
def sigProb (ρ : ℝ) : World → Sig → ℝ
  | .H, .h => ρ
  | .H, .l => 1 - ρ
  | .L, .l => ρ
  | .L, .h => 1 - ρ

/-- A memory process on `N` memory states (p.7-8): initial distribution `g₀`,
transition rule `σ(i, s)(j)`, action rule `a(i)` (probability of action `H`). -/
structure Memory (N : ℕ) where
  g0 : Fin N → ℝ
  σ : Fin N → Sig → Fin N → ℝ
  a : Fin N → ℝ

variable {N : ℕ}

/-- The constraints that make `(g₀, σ, a)` a memory process: `g₀ ∈ Δ(𝒩)`,
`σ(i, s) ∈ Δ(𝒩)`, `a(i) ∈ [0,1]`. -/
structure Memory.Valid (m : Memory N) : Prop where
  g0_nonneg : ∀ i, 0 ≤ m.g0 i
  g0_sum : ∑ i, m.g0 i = 1
  σ_nonneg : ∀ i s j, 0 ≤ m.σ i s j
  σ_sum : ∀ i s, ∑ j, m.σ i s j = 1
  a_nonneg : ∀ i, 0 ≤ m.a i
  a_le_one : ∀ i, m.a i ≤ 1

/-- The transition probability conditional on the state (p.8):
`τ^S_{i,j} = Pr(l|S) σ(i,l)(j) + Pr(h|S) σ(i,h)(j)`. -/
def trans (ρ : ℝ) (m : Memory N) (S : World) (i j : Fin N) : ℝ :=
  sigProb ρ S .l * m.σ i .l j + sigProb ρ S .h * m.σ i .h j

theorem trans_H (ρ : ℝ) (m : Memory N) (i j : Fin N) :
    trans ρ m .H i j = (1 - ρ) * m.σ i .l j + ρ * m.σ i .h j := rfl

theorem trans_L (ρ : ℝ) (m : Memory N) (i j : Fin N) :
    trans ρ m .L i j = ρ * m.σ i .l j + (1 - ρ) * m.σ i .h j := rfl

theorem sigProb_nonneg {ρ : ℝ} (h0 : 0 ≤ ρ) (h1 : ρ ≤ 1) (S : World) (s : Sig) :
    0 ≤ sigProb ρ S s := by
  cases S <;> cases s <;> simp [sigProb] <;> linarith

theorem sigProb_sum (ρ : ℝ) (S : World) : sigProb ρ S .l + sigProb ρ S .h = 1 := by
  cases S <;> simp [sigProb]

theorem trans_nonneg {ρ : ℝ} (h0 : 0 ≤ ρ) (h1 : ρ ≤ 1) {m : Memory N} (hm : m.Valid)
    (S : World) (i j : Fin N) : 0 ≤ trans ρ m S i j := by
  unfold trans
  have := sigProb_nonneg h0 h1 S .l; have := sigProb_nonneg h0 h1 S .h
  have := hm.σ_nonneg i .l j; have := hm.σ_nonneg i .h j
  positivity

/-- Each row of `T^S` is a probability vector. -/
theorem trans_rowsum (ρ : ℝ) {m : Memory N} (hm : m.Valid) (S : World) (i : Fin N) :
    ∑ j, trans ρ m S i j = 1 := by
  unfold trans
  rw [Finset.sum_add_distrib, ← Finset.mul_sum, ← Finset.mul_sum, hm.σ_sum, hm.σ_sum,
    mul_one, mul_one, sigProb_sum]

/-- The distribution `f^S` of the memory state at termination: eq. (1),
`f^S = ∑_t η (1-η)^t g₀ (T^S)^t`, in the stationary form that Lemma 1 derives
(proof of Lemma 1, first line of (A8)): `f^S = η g₀ + (1-η) f^S T^S`. -/
def IsEndDist (η ρ : ℝ) (m : Memory N) (S : World) (f : Fin N → ℝ) : Prop :=
  ∀ j, f j = η * m.g0 j + (1 - η) * ∑ i, f i * trans ρ m S i j

/-- The ending distribution is a probability vector's total: `∑ f^S = 1`. -/
theorem endDist_sum {η ρ : ℝ} (hη : 0 < η) {m : Memory N} (hm : m.Valid) {S : World}
    {f : Fin N → ℝ} (hf : IsEndDist η ρ m S f) : ∑ j, f j = 1 := by
  have h : ∑ j, f j = η + (1 - η) * ∑ j, f j := by
    calc ∑ j, f j = ∑ j, (η * m.g0 j + (1 - η) * ∑ i, f i * trans ρ m S i j) :=
          Finset.sum_congr rfl fun j _ => hf j
      _ = η * ∑ j, m.g0 j + (1 - η) * ∑ i, f i * ∑ j, trans ρ m S i j := by
          rw [Finset.sum_add_distrib, ← Finset.mul_sum, ← Finset.mul_sum, Finset.sum_comm]
          simp_rw [Finset.mul_sum]
      _ = η + (1 - η) * ∑ j, f j := by
          rw [hm.g0_sum]; simp [trans_rowsum ρ hm]
  have : η * (∑ j, f j - 1) = 0 := by linarith
  rcases mul_eq_zero.1 this with h0 | h0
  · exact absurd h0 hη.ne'
  · linarith

/-- The homogeneous equation `d = (1-η) d T^S` has only the zero solution for
`0 < η ≤ 1`: `∑|d| ≤ (1-η) ∑|d|` since `T^S` is stochastic. -/
theorem homog_zero {η ρ : ℝ} (hη : 0 < η) (hη1 : η ≤ 1) (hρ0 : 0 ≤ ρ) (hρ1 : ρ ≤ 1)
    {m : Memory N} (hm : m.Valid) {S : World} {d : Fin N → ℝ}
    (hd : ∀ j, d j = (1 - η) * ∑ i, d i * trans ρ m S i j) : d = 0 := by
  have hT := trans_nonneg hρ0 hρ1 hm S
  have key : ∑ j, |d j| ≤ (1 - η) * ∑ j, |d j| := by
    calc ∑ j, |d j| = ∑ j, (1 - η) * |∑ i, d i * trans ρ m S i j| := by
          refine Finset.sum_congr rfl fun j _ => ?_
          rw [hd j, abs_mul, abs_of_nonneg (by linarith : (0:ℝ) ≤ 1 - η)]
      _ ≤ ∑ j, (1 - η) * ∑ i, |d i| * trans ρ m S i j := by
          refine Finset.sum_le_sum fun j _ => mul_le_mul_of_nonneg_left ?_ (by linarith)
          refine (Finset.abs_sum_le_sum_abs _ _).trans (le_of_eq ?_)
          refine Finset.sum_congr rfl fun i _ => ?_
          rw [abs_mul, abs_of_nonneg (hT i j)]
      _ = (1 - η) * ∑ i, |d i| * ∑ j, trans ρ m S i j := by
          rw [← Finset.mul_sum, Finset.sum_comm]
          simp_rw [Finset.mul_sum]
      _ = (1 - η) * ∑ j, |d j| := by simp [trans_rowsum ρ hm]
  have h0 : ∑ j, |d j| = 0 := by
    have hnn : 0 ≤ ∑ j, |d j| := Finset.sum_nonneg fun j _ => abs_nonneg _
    nlinarith
  funext j
  exact abs_eq_zero.1 ((Finset.sum_eq_zero_iff_of_nonneg (fun j _ => abs_nonneg (d j))).1 h0 j
    (Finset.mem_univ j))

/-- **Lemma 1, uniqueness** (p.36): for `0 < η ≤ 1` the stationary equation has
at most one solution. -/
theorem endDist_unique {η ρ : ℝ} (hη : 0 < η) (hη1 : η ≤ 1) (hρ0 : 0 ≤ ρ) (hρ1 : ρ ≤ 1)
    {m : Memory N} (hm : m.Valid) {S : World} {f f' : Fin N → ℝ}
    (hf : IsEndDist η ρ m S f) (hf' : IsEndDist η ρ m S f') : f = f' := by
  have h := homog_zero (d := fun j => f j - f' j) hη hη1 hρ0 hρ1 hm (S := S) (by
    intro j
    simp only [hf j, hf' j, sub_mul, Finset.sum_sub_distrib]
    ring)
  funext j
  have := congrFun h j
  simp only [Pi.zero_apply] at this
  linarith

/-- **Lemma 1, existence**: for `0 < η ≤ 1` the stationary equation has a
solution: the linear map `f ↦ f - (1-η) f T^S` is injective (`homog_zero`),
hence surjective. -/
theorem endDist_exists {η ρ : ℝ} (hη : 0 < η) (hη1 : η ≤ 1) (hρ0 : 0 ≤ ρ) (hρ1 : ρ ≤ 1)
    {m : Memory N} (hm : m.Valid) (S : World) : ∃ f, IsEndDist η ρ m S f := by
  let L : (Fin N → ℝ) →ₗ[ℝ] (Fin N → ℝ) :=
    { toFun := fun f j => f j - (1 - η) * ∑ i, f i * trans ρ m S i j
      map_add' := by
        intro f g; funext j; simp only [Pi.add_apply, add_mul, Finset.sum_add_distrib]; ring
      map_smul' := by
        intro c f; funext j
        simp only [Pi.smul_apply, smul_eq_mul, RingHom.id_apply, mul_assoc, ← Finset.mul_sum]
        ring }
  have hinj : Function.Injective L := by
    rw [← LinearMap.ker_eq_bot, LinearMap.ker_eq_bot']
    intro d hd
    refine homog_zero hη hη1 hρ0 hρ1 hm (S := S) fun j => ?_
    have := congrFun hd j
    simp only [L, LinearMap.coe_mk, AddHom.coe_mk, Pi.zero_apply] at this
    linarith
  obtain ⟨f, hf⟩ := LinearMap.injective_iff_surjective.1 hinj (fun j => η * m.g0 j)
  refine ⟨f, fun j => ?_⟩
  have := congrFun hf j
  simp only [L, LinearMap.coe_mk, AddHom.coe_mk] at this
  linarith

/-! ## The one-step informativeness bound (p.15, eq. (4)) -/

/-- A transition taken with `σ(i,l)(j), σ(i,h)(j) ≥ 0` multiplies the likelihood
ratio `H : L` by at most `ρ/(1-ρ)`: `(1-ρ) τ^H_{ij} ≤ ρ τ^L_{ij}`. -/
theorem trans_ratio_up {ρ : ℝ} (hρ : 1 / 2 ≤ ρ) {m : Memory N} (hm : m.Valid)
    (i j : Fin N) : (1 - ρ) * trans ρ m .H i j ≤ ρ * trans ρ m .L i j := by
  rw [trans_H, trans_L]
  have := hm.σ_nonneg i .l j
  nlinarith

/-- ... and by at least `(1-ρ)/ρ`: `(1-ρ) τ^L_{ij} ≤ ρ τ^H_{ij}`. -/
theorem trans_ratio_down {ρ : ℝ} (hρ : 1 / 2 ≤ ρ) {m : Memory N} (hm : m.Valid)
    (i j : Fin N) : (1 - ρ) * trans ρ m .L i j ≤ ρ * trans ρ m .H i j := by
  rw [trans_H, trans_L]
  have := hm.σ_nonneg i .h j
  nlinarith

/-- The upward bound is attained iff the move is never taken after an `l`-signal
(`ρ > 1/2`): "the DM should only switch to higher states after an h-signal"
(p.15). -/
theorem trans_ratio_up_eq_iff {ρ : ℝ} (hρ : 1 / 2 < ρ) (m : Memory N) (i j : Fin N) :
    (1 - ρ) * trans ρ m .H i j = ρ * trans ρ m .L i j ↔ m.σ i .l j = 0 := by
  rw [trans_H, trans_L]
  constructor
  · intro h
    have : (2 * ρ - 1) * m.σ i .l j = 0 := by nlinarith
    rcases mul_eq_zero.1 this with h0 | h0
    · linarith
    · exact h0
  · intro h; rw [h]; ring

/-- **Eq. (4)** (p.15) as a balance equation.  For a three-state memory with no
jumps between the extreme states and `g₀(3) = 0`, the stationary equation at
state `3` reads `f₃ (η + (1-η) τ₃₂) = (1-η) f₂ τ₂₃`.  (States `0, 1, 2` are the
paper's `1, 2, 3`.) -/
theorem eq4_balance {η ρ : ℝ} {m : Memory 3} (hm : m.Valid) {S : World} {f : Fin 3 → ℝ}
    (hf : IsEndDist η ρ m S f) (hg : m.g0 2 = 0) (h02 : ∀ s, m.σ 0 s 2 = 0)
    (h20 : ∀ s, m.σ 2 s 0 = 0) :
    f 2 * (η + (1 - η) * trans ρ m S 2 1) = (1 - η) * f 1 * trans ρ m S 1 2 := by
  have hrow := trans_rowsum ρ hm S 2
  simp only [Fin.sum_univ_three] at hrow
  have t02 : trans ρ m S 0 2 = 0 := by simp [trans, h02]
  have t20 : trans ρ m S 2 0 = 0 := by simp [trans, h20]
  have h := hf 2
  simp only [Fin.sum_univ_three, hg, t02] at h
  have t22 : trans ρ m S 2 2 = 1 - trans ρ m S 2 1 := by linarith
  rw [t22] at h
  linarith

/-- **Eq. (4)** (p.15), the likelihood-ratio form:
`f₃^H/f₃^L = (τ^H₂₃/τ^L₂₃) · ((η + (1-η)τ^L₃₂)/(η + (1-η)τ^H₃₂)) · (f₂^H/f₂^L)`. -/
theorem eq4 {η ρ : ℝ} (hη : 0 < η) (hη1 : η ≤ 1) (hρ0 : 0 ≤ ρ) (hρ1 : ρ ≤ 1)
    {m : Memory 3} (hm : m.Valid) {fH fL : Fin 3 → ℝ}
    (hfH : IsEndDist η ρ m .H fH) (hfL : IsEndDist η ρ m .L fL) (hg : m.g0 2 = 0)
    (h02 : ∀ s, m.σ 0 s 2 = 0) (h20 : ∀ s, m.σ 2 s 0 = 0)
    (hL2 : fL 2 ≠ 0) (hL1 : fL 1 ≠ 0) (htL : trans ρ m .L 1 2 ≠ 0) :
    fH 2 / fL 2 = (trans ρ m .H 1 2 / trans ρ m .L 1 2) *
      ((η + (1 - η) * trans ρ m .L 2 1) / (η + (1 - η) * trans ρ m .H 2 1)) * (fH 1 / fL 1) := by
  have eH := eq4_balance hm hfH hg h02 h20
  have eL := eq4_balance hm hfL hg h02 h20
  have dH : 0 < η + (1 - η) * trans ρ m .H 2 1 := by
    have := trans_nonneg hρ0 hρ1 hm .H 2 1; nlinarith
  have dL : 0 < η + (1 - η) * trans ρ m .L 2 1 := by
    have := trans_nonneg hρ0 hρ1 hm .L 2 1; nlinarith
  have hη' : (1 - η) * fL 1 * trans ρ m .L 1 2 ≠ 0 := by
    intro h0; rw [← eL] at h0
    exact (mul_ne_zero hL2 dL.ne') h0
  have h1η : 1 - η ≠ 0 := by
    intro h0; apply hη'; rw [h0]; ring
  rw [div_eq_iff hL2]
  field_simp
  have : fH 2 = (1 - η) * fH 1 * trans ρ m .H 1 2 / (η + (1 - η) * trans ρ m .H 2 1) := by
    rw [eq_div_iff dH.ne']; exact eH
  have h2 : fL 2 = (1 - η) * fL 1 * trans ρ m .L 1 2 / (η + (1 - η) * trans ρ m .L 2 1) := by
    rw [eq_div_iff dL.ne']; exact eL
  rw [this, h2]
  field_simp

/-- **The bound behind eq. (4)** (p.15): from state `2` to state `3` the likelihood
ratio `H : L` grows by at most `(ρ/(1-ρ))²`, whatever the rule:
`(1-ρ)² f₃^H f₂^L ≤ ρ² f₃^L f₂^H`.  One factor `ρ/(1-ρ)` is the move `2 → 3`
(`trans_ratio_up`), the other is the exit `3 → 2`, which with `η > 0` can only
approach `ρ/(1-ρ)`. -/
theorem eq4_bound {η ρ : ℝ} (hη : 0 < η) (hη1 : η ≤ 1) (hρ : 1 / 2 ≤ ρ) (hρ1 : ρ ≤ 1)
    {m : Memory 3} (hm : m.Valid) {fH fL : Fin 3 → ℝ}
    (hfH : IsEndDist η ρ m .H fH) (hfL : IsEndDist η ρ m .L fL) (hg : m.g0 2 = 0)
    (h02 : ∀ s, m.σ 0 s 2 = 0) (h20 : ∀ s, m.σ 2 s 0 = 0)
    (hH1 : 0 ≤ fH 1) (hL1 : 0 ≤ fL 1) :
    (1 - ρ) ^ 2 * (fH 2 * fL 1) ≤ ρ ^ 2 * (fL 2 * fH 1) := by
  have hρ0 : 0 ≤ ρ := by linarith
  have eH := eq4_balance hm hfH hg h02 h20
  have eL := eq4_balance hm hfL hg h02 h20
  set tH12 := trans ρ m .H 1 2; set tL12 := trans ρ m .L 1 2
  set tH21 := trans ρ m .H 2 1; set tL21 := trans ρ m .L 2 1
  have htH12 : 0 ≤ tH12 := trans_nonneg hρ0 hρ1 hm _ _ _
  have htL12 : 0 ≤ tL12 := trans_nonneg hρ0 hρ1 hm _ _ _
  have htH21 : 0 ≤ tH21 := trans_nonneg hρ0 hρ1 hm _ _ _
  have htL21 : 0 ≤ tL21 := trans_nonneg hρ0 hρ1 hm _ _ _
  set DH := η + (1 - η) * tH21; set DL := η + (1 - η) * tL21
  have dH : 0 < DH := by nlinarith
  have dL : 0 < DL := by nlinarith
  have up : (1 - ρ) * tH12 ≤ ρ * tL12 := trans_ratio_up hρ hm 1 2
  have down : (1 - ρ) * tL21 ≤ ρ * tH21 := trans_ratio_down hρ hm 2 1
  have exit : (1 - ρ) * DL ≤ ρ * DH := by
    simp only [DH, DL]; nlinarith
  have key : ((1 - ρ) * tH12) * ((1 - ρ) * DL) ≤ (ρ * tL12) * (ρ * DH) :=
    mul_le_mul up exit (by nlinarith) (by nlinarith)
  have hc : 0 ≤ (1 - η) * fH 1 * fL 1 := by
    have : 0 ≤ 1 - η := by linarith
    positivity
  have := mul_le_mul_of_nonneg_left key hc
  have lhs : (1 - ρ) ^ 2 * (fH 2 * fL 1) * (DH * DL) =
      (1 - η) * fH 1 * fL 1 * (((1 - ρ) * tH12) * ((1 - ρ) * DL)) := by
    have : fH 2 * DH = (1 - η) * fH 1 * tH12 := eH
    linear_combination (1 - ρ) ^ 2 * fL 1 * DL * this
  have rhs : ρ ^ 2 * (fL 2 * fH 1) * (DH * DL) =
      (1 - η) * fH 1 * fL 1 * ((ρ * tL12) * (ρ * DH)) := by
    have : fL 2 * DL = (1 - η) * fL 1 * tL12 := eL
    linear_combination ρ ^ 2 * fH 1 * DH * this
  have hDD : 0 < DH * DL := mul_pos dH dL
  nlinarith

/-! ## The Theorem 3 rule for `N = 3`, solved in closed form -/

/-- The Theorem 3 rule for `N = 3` (states `0, 1, 2` = the paper's `1, 2, 3`):
start in the middle (iii); in the middle move up after `h` and down after `l`
(ii); in an extreme state leave for the middle with probability `γ` after the
opposing signal and otherwise stay (i); act `L` in state `1` and `H` in states
`2, 3` (iv, with `a(2) = 1`).  The same `γ` at both ends is the symmetric
solution (`α* = 1`, p.16). -/
noncomputable def rule3 (γ : ℝ) : Memory 3 where
  g0 := ![0, 1, 0]
  σ := fun i s => match s with
    | .l => ![![1, 0, 0], ![1, 0, 0], ![0, γ, 1 - γ]] i
    | .h => ![![1 - γ, γ, 0], ![0, 0, 1], ![0, 0, 1]] i
  a := ![0, 1, 1]

theorem rule3_valid {γ : ℝ} (h0 : 0 ≤ γ) (h1 : γ ≤ 1) : (rule3 γ).Valid where
  g0_nonneg := by intro i; fin_cases i <;> simp [rule3]
  g0_sum := by simp [rule3, Fin.sum_univ_three]
  σ_nonneg := by
    intro i s j; cases s <;> fin_cases i <;> fin_cases j <;> simp [rule3] <;> linarith
  σ_sum := by intro i s; cases s <;> fin_cases i <;> simp [rule3, Fin.sum_univ_three]
  a_nonneg := by intro i; fin_cases i <;> simp [rule3]
  a_le_one := by intro i; fin_cases i <;> simp [rule3]

/-- The common denominator `Δ = η + γ(1-η) - γ(2-γ)(1-η)² p(1-p)`. -/
noncomputable def D3 (η γ p : ℝ) : ℝ := η + γ * (1 - η) - γ * (2 - γ) * (1 - η) ^ 2 * (p * (1 - p))

/-- The ending distribution of `rule3 γ` in a state where `h` has probability `p`:
`f = (u q A, A B, u p B)/Δ` with `u = 1-η`, `q = 1-p`, `A = η + uγq`,
`B = η + uγp`. -/
noncomputable def fcl (η γ p : ℝ) : Fin 3 → ℝ :=
  ![(1 - η) * (1 - p) * (η + (1 - η) * γ * (1 - p)) / D3 η γ p,
    (η + (1 - η) * γ * (1 - p)) * (η + (1 - η) * γ * p) / D3 η γ p,
    (1 - η) * p * (η + (1 - η) * γ * p) / D3 η γ p]

theorem D3_pos {η γ p : ℝ} (hη : 0 < η) (hη1 : η ≤ 1) (hγ : 0 ≤ γ) (hγ1 : γ ≤ 1)
    (hp : 0 ≤ p) (hp1 : p ≤ 1) : 0 < D3 η γ p := by
  unfold D3
  have hpq : p * (1 - p) ≤ 1 / 4 := by nlinarith [sq_nonneg (p - 1 / 2)]
  have hpq0 : 0 ≤ p * (1 - p) := by nlinarith
  have hu : 0 ≤ 1 - η := by linarith
  have h1 : (2 - γ) * (1 - η) * (p * (1 - p)) ≤ 1 / 2 := by
    have : (2 - γ) * (1 - η) ≤ 2 := by nlinarith
    nlinarith
  have h2 : γ * (2 - γ) * (1 - η) ^ 2 * (p * (1 - p)) ≤ γ * (1 - η) * (1 / 2) := by
    have : γ * (2 - γ) * (1 - η) ^ 2 * (p * (1 - p)) =
        γ * (1 - η) * ((2 - γ) * (1 - η) * (p * (1 - p))) := by ring
    rw [this]
    exact mul_le_mul_of_nonneg_left h1 (mul_nonneg hγ hu)
  nlinarith [mul_nonneg hγ hu]

theorem D3_symm (η γ ρ : ℝ) : D3 η γ (1 - ρ) = D3 η γ ρ := by unfold D3; ring

/-- `trans` for `rule3` in state `S`, written with `p = Pr(h|S)`. -/
theorem rule3_trans (ρ γ : ℝ) (S : World) (i j : Fin 3) :
    trans ρ (rule3 γ) S i j = (1 - sigProb ρ S .h) * (rule3 γ).σ i .l j +
      sigProb ρ S .h * (rule3 γ).σ i .h j := by
  unfold trans
  have := sigProb_sum ρ S
  rw [show sigProb ρ S .l = 1 - sigProb ρ S .h by linarith]

/-- The closed form solves the stationary equation (so, by `endDist_unique`, it
*is* the ending distribution `f^S` of `rule3 γ`). -/
theorem rule3_endDist {η γ ρ : ℝ} (hη : 0 < η) (hη1 : η ≤ 1) (hγ : 0 ≤ γ) (hγ1 : γ ≤ 1)
    (hρ0 : 0 ≤ ρ) (hρ1 : ρ ≤ 1) (S : World) :
    IsEndDist η ρ (rule3 γ) S (fcl η γ (sigProb ρ S .h)) := by
  have hp0 : 0 ≤ sigProb ρ S .h := sigProb_nonneg hρ0 hρ1 S .h
  have hp1 : sigProb ρ S .h ≤ 1 := by
    have := sigProb_sum ρ S; have := sigProb_nonneg hρ0 hρ1 S .l; linarith
  have hD := D3_pos hη hη1 hγ hγ1 hp0 hp1
  intro j
  simp only [Fin.sum_univ_three, rule3_trans]
  generalize sigProb ρ S .h = p at hp0 hp1 hD ⊢
  fin_cases j <;> simp [fcl, rule3] <;> field_simp <;> (try unfold D3) <;> ring

/-- The expected payoff (p.8): `Π = ½ ∑ᵢ (f^H_i a(i) + f^L_i (1 - a(i)))`. -/
noncomputable def payoff (fH fL a : Fin N → ℝ) : ℝ := 1 / 2 * ∑ i, (fH i * a i + fL i * (1 - a i))

/-- The payoff of `rule3 γ`, in closed form. -/
noncomputable def Pi3 (η γ ρ : ℝ) : ℝ :=
  (η + (1 - η) * γ * ρ) * (η + (1 - η) * γ * (1 - ρ) + 2 * (1 - η) * ρ) / (2 * D3 η γ ρ)

theorem rule3_payoff {η γ ρ : ℝ} (hη : 0 < η) (hη1 : η ≤ 1) (hγ : 0 ≤ γ) (hγ1 : γ ≤ 1)
    (hρ0 : 0 ≤ ρ) (hρ1 : ρ ≤ 1) :
    payoff (fcl η γ ρ) (fcl η γ (1 - ρ)) (rule3 γ).a = Pi3 η γ ρ := by
  have hD := D3_pos hη hη1 hγ hγ1 hρ0 hρ1
  unfold payoff Pi3
  simp only [Fin.sum_univ_three, fcl, rule3, D3_symm]
  simp
  field_simp
  ring

/-- The payoff of `rule3 γ`, computed from the model's own definition: whatever
solves the stationary equations is the closed form (`endDist_unique`), and its
payoff is `Pi3`. -/
theorem rule3_payoff_endDist {η γ ρ : ℝ} (hη : 0 < η) (hη1 : η ≤ 1) (hγ : 0 ≤ γ) (hγ1 : γ ≤ 1)
    (hρ0 : 0 ≤ ρ) (hρ1 : ρ ≤ 1) {fH fL : Fin 3 → ℝ}
    (hH : IsEndDist η ρ (rule3 γ) .H fH) (hL : IsEndDist η ρ (rule3 γ) .L fL) :
    payoff fH fL (rule3 γ).a = Pi3 η γ ρ := by
  have hv := rule3_valid hγ hγ1
  have e1 : fH = fcl η γ ρ :=
    endDist_unique hη hη1 hρ0 hρ1 hv hH (rule3_endDist hη hη1 hγ hγ1 hρ0 hρ1 .H)
  have e2 : fL = fcl η γ (1 - ρ) :=
    endDist_unique hη hη1 hρ0 hρ1 hv hL (rule3_endDist hη hη1 hγ hγ1 hρ0 hρ1 .L)
  rw [e1, e2]
  exact rule3_payoff hη hη1 hγ hγ1 hρ0 hρ1

/-- The Corollary's limit payoff at `N = 3`:
`(1 + ((1-ρ)/ρ)^{N-1})⁻¹ = ρ² / (ρ² + (1-ρ)²)`. -/
noncomputable def beta (ρ : ℝ) : ℝ := ρ ^ 2 / (ρ ^ 2 + (1 - ρ) ^ 2)

theorem beta_eq_corollary {ρ : ℝ} (hρ : 0 < ρ) : (1 + ((1 - ρ) / ρ) ^ (3 - 1))⁻¹ = beta ρ := by
  unfold beta
  have : ρ ^ 2 + (1 - ρ) ^ 2 ≠ 0 := by positivity
  have hρ' : ρ ≠ 0 := hρ.ne'
  norm_num
  rw [div_pow, one_add_div (pow_ne_zero 2 hρ'), inv_div]

theorem sq_add_sq_pos (ρ : ℝ) : 0 < ρ ^ 2 + (1 - ρ) ^ 2 := by
  nlinarith [sq_nonneg (ρ - 1 / 2)]

/-- **The gap to the Corollary's bound**, exactly:
`β - Π = (2ρ-1)[ρ(1-ρ)u(γ²u + 2η) + η(η + γu)] / (2(ρ² + (1-ρ)²) Δ)`, `u = 1-η`. -/
theorem rule3_gap {η γ ρ : ℝ} (hD : D3 η γ ρ ≠ 0) :
    beta ρ - Pi3 η γ ρ = (2 * ρ - 1) * (ρ * (1 - ρ) * (1 - η) * (γ ^ 2 * (1 - η) + 2 * η) +
      η * (η + γ * (1 - η))) / (2 * (ρ ^ 2 + (1 - ρ) ^ 2) * D3 η γ ρ) := by
  have := (sq_add_sq_pos ρ).ne'
  unfold beta Pi3
  field_simp
  unfold D3
  ring

/-- `rule3` never reaches the Corollary's bound for `η > 0`. -/
theorem rule3_payoff_lt_beta {η γ ρ : ℝ} (hη : 0 < η) (hη1 : η ≤ 1) (hγ : 0 ≤ γ) (hγ1 : γ ≤ 1)
    (hρ : 1 / 2 < ρ) (hρ1 : ρ < 1) : Pi3 η γ ρ < beta ρ := by
  have hD := D3_pos hη hη1 hγ hγ1 (by linarith : (0:ℝ) ≤ ρ) hρ1.le
  have h := rule3_gap (γ := γ) (ρ := ρ) hD.ne'
  have hnum : 0 < (2 * ρ - 1) * (ρ * (1 - ρ) * (1 - η) * (γ ^ 2 * (1 - η) + 2 * η) +
      η * (η + γ * (1 - η))) := by
    have : 0 ≤ ρ * (1 - ρ) * (1 - η) * (γ ^ 2 * (1 - η) + 2 * η) := by
      have : 0 ≤ 1 - η := by linarith
      have : 0 ≤ 1 - ρ := by linarith
      positivity
    have : 0 < η * (η + γ * (1 - η)) := by
      have : 0 ≤ γ * (1 - η) := mul_nonneg hγ (by linarith)
      positivity
    have : 0 < 2 * ρ - 1 := by linarith
    positivity
  have hden : 0 < 2 * (ρ ^ 2 + (1 - ρ) ^ 2) * D3 η γ ρ := by
    have := sq_add_sq_pos ρ; positivity
  have : 0 < beta ρ - Pi3 η γ ρ := by rw [h]; exact div_pos hnum hden
  linarith

/-- **Absorbing extremes** (`γ = 0`): `Π = ρ - η(ρ - ½)`, whose limit `ρ` is the
gambler's-ruin value `(1 + ((1-ρ)/ρ)^{(N-1)/2})⁻¹` at `N = 3` (p.12): "as if there
were only `(N-1)/2` memory states". -/
theorem rule3_absorbing {η ρ : ℝ} (hη : 0 < η) : Pi3 η 0 ρ = ρ - η * (ρ - 1 / 2) := by
  have hD : D3 η 0 ρ = η := by unfold D3; ring
  unfold Pi3
  rw [hD, div_eq_iff (by positivity)]
  ring

theorem gamblersRuin_eq {ρ : ℝ} (hρ : 0 < ρ) : (1 + ((1 - ρ) / ρ) ^ ((3 - 1) / 2))⁻¹ = ρ := by
  norm_num
  field_simp
  ring

/-- Never leaving the extremes costs payoff even in the limit: `ρ < β`. -/
theorem absorbing_lt_beta {ρ : ℝ} (hρ : 1 / 2 < ρ) (hρ1 : ρ < 1) : ρ < beta ρ := by
  unfold beta
  rw [lt_div_iff₀ (sq_add_sq_pos ρ)]
  nlinarith [mul_pos (by linarith : (0:ℝ) < 2 * ρ - 1) (by linarith : (0:ℝ) < 1 - ρ)]

/-- Payoff differences within the family:
`Π(γ) - Π(γ') = ρ(1-ρ)(1-η)³(2ρ-1)(γ-γ')[η((1-γ)(1-γ')+1) - γγ'] / (2Δ(γ)Δ(γ'))`. -/
theorem rule3_diff {η γ γ' ρ : ℝ} (hD : D3 η γ ρ ≠ 0) (hD' : D3 η γ' ρ ≠ 0) :
    Pi3 η γ ρ - Pi3 η γ' ρ = ρ * (1 - ρ) * (1 - η) ^ 3 * (2 * ρ - 1) * (γ - γ') *
      (η * ((1 - γ) * (1 - γ') + 1) - γ * γ') / (2 * D3 η γ ρ * D3 η γ' ρ) := by
  unfold Pi3
  field_simp
  unfold D3
  ring

/-- The optimal leaving probability within the family,
`γ*(η) = (√(2η - η²) - η)/(1 - η)`, the root in `(0,1)` of
`γ² = η((1-γ)² + 1)`. -/
noncomputable def gstar (η : ℝ) : ℝ := (Real.sqrt (2 * η - η ^ 2) - η) / (1 - η)

theorem gstar_quadratic {η : ℝ} (hη : 0 < η) (hη1 : η < 1) :
    gstar η ^ 2 = η * ((1 - gstar η) ^ 2 + 1) := by
  have hs := Real.sq_sqrt (show (0:ℝ) ≤ 2 * η - η ^ 2 by nlinarith)
  have h1 : 1 - η ≠ 0 := by linarith
  set s := Real.sqrt (2 * η - η ^ 2)
  have hg : (1 - η) * gstar η = s - η := by
    unfold gstar; rw [mul_comm]; exact div_mul_cancel₀ _ h1
  set g := gstar η
  have hQ : (1 - η) * ((1 - η) * g ^ 2 + 2 * η * g - 2 * η) = 0 := by
    linear_combination ((1 - η) * g + s + η) * hg + hs
  have hQ' : (1 - η) * g ^ 2 + 2 * η * g - 2 * η = 0 := by
    rcases mul_eq_zero.1 hQ with h | h
    · exact absurd h h1
    · exact h
  linear_combination hQ'

theorem gstar_pos {η : ℝ} (hη : 0 < η) (hη1 : η < 1) : 0 < gstar η := by
  unfold gstar
  apply div_pos _ (by linarith)
  rw [sub_pos, Real.lt_sqrt hη.le]
  nlinarith

theorem gstar_lt_one {η : ℝ} (hη : 0 < η) (hη1 : η < 1) : gstar η < 1 := by
  unfold gstar
  rw [div_lt_one (by linarith), sub_lt_iff_lt_add, Real.sqrt_lt' (by linarith)]
  nlinarith

/-- `η ≤ γ*² ≤ 2η`: the optimal leaving probability goes to zero, and more slowly
than `η` (Theorem 3 (i) and p.4-5, within the family). -/
theorem gstar_sq_bounds {η : ℝ} (hη : 0 < η) (hη1 : η < 1) :
    η ≤ gstar η ^ 2 ∧ gstar η ^ 2 ≤ 2 * η := by
  have hq := gstar_quadratic hη hη1
  have h0 := gstar_pos hη hη1
  have h1 := gstar_lt_one hη hη1
  have hsq : (1 - gstar η) ^ 2 ≤ 1 := by nlinarith
  constructor
  · rw [hq]; nlinarith [sq_nonneg (1 - gstar η)]
  · rw [hq]; nlinarith [mul_le_mul_of_nonneg_left hsq hη.le]

/-- **Theorem 3 (i) at `N = 3`, within the family `rule3`**: for `0 < η < 1` the
leaving probability `γ*(η)` is the unique optimum over `γ ∈ [0,1]`.  So neither
never leaving (`γ = 0`, absorbing) nor always leaving (`γ = 1`, the pure
strategy) is optimal, and by `gstar_sq_bounds` `γ* → 0` with `η/γ* → 0`. -/
theorem rule3_optimal {η γ ρ : ℝ} (hη : 0 < η) (hη1 : η < 1) (hγ : 0 ≤ γ) (hγ1 : γ ≤ 1)
    (hρ : 1 / 2 < ρ) (hρ1 : ρ < 1) (hne : γ ≠ gstar η) : Pi3 η γ ρ < Pi3 η (gstar η) ρ := by
  have hg0 := gstar_pos hη hη1
  have hg1 := gstar_lt_one hη hη1
  have hq := gstar_quadratic hη hη1
  have hρ0 : (0:ℝ) ≤ ρ := by linarith
  have hD := D3_pos hη hη1.le hγ hγ1 hρ0 hρ1.le
  have hDg := D3_pos hη hη1.le hg0.le hg1.le hρ0 hρ1.le
  have hdiff := rule3_diff (γ := gstar η) (γ' := γ) (ρ := ρ) hDg.ne' hD.ne'
  set g := gstar η
  have hbr : (g - γ) * (η * ((1 - g) * (1 - γ) + 1) - g * γ) = (g - γ) ^ 2 * (η * (1 - g) + g) := by
    linear_combination (-(g - γ)) * hq
  have hK : 0 < ρ * (1 - ρ) * (1 - η) ^ 3 * (2 * ρ - 1) := by
    have : 0 < 1 - ρ := by linarith
    have : 0 < 1 - η := by linarith
    have : 0 < 2 * ρ - 1 := by linarith
    positivity
  have hsq : 0 < (g - γ) ^ 2 * (η * (1 - g) + g) := by
    have : 0 < (g - γ) ^ 2 := by positivity
    have : 0 < η * (1 - g) + g := by nlinarith
    positivity
  have : 0 < Pi3 η g ρ - Pi3 η γ ρ := by
    rw [hdiff, mul_assoc (ρ * (1 - ρ) * (1 - η) ^ 3 * (2 * ρ - 1)) (g - γ), hbr]
    have : 0 < 2 * D3 η g ρ * D3 η γ ρ := by positivity
    rw [← mul_assoc]
    positivity
  linarith

theorem D3_ge {η γ p : ℝ} (hη : 0 < η) (hη1 : η ≤ 1) (hγ : 0 ≤ γ) (hγ1 : γ ≤ 1)
    (hp : 0 ≤ p) (hp1 : p ≤ 1) : η + γ * (1 - η) / 2 ≤ D3 η γ p := by
  unfold D3
  have hpq : p * (1 - p) ≤ 1 / 4 := by nlinarith [sq_nonneg (p - 1 / 2)]
  have hpq0 : 0 ≤ p * (1 - p) := by nlinarith
  have hu : 0 ≤ 1 - η := by linarith
  have h1 : (2 - γ) * (1 - η) * (p * (1 - p)) ≤ 1 / 2 := by
    have : (2 - γ) * (1 - η) ≤ 2 := by nlinarith
    nlinarith
  have : γ * (2 - γ) * (1 - η) ^ 2 * (p * (1 - p)) =
      γ * (1 - η) * ((2 - γ) * (1 - η) * (p * (1 - p))) := by ring
  rw [this]
  nlinarith [mul_le_mul_of_nonneg_left h1 (mul_nonneg hγ hu)]

/-- **Corollary (i), lower half, at `N = 3`** (p.12): for every `ε > 0` there is
`η̄` such that for `0 < η < η̄` the rule `rule3 γ*(η)` (hence the optimum
`Π*(η, 3)`) earns more than `(1 + ((1-ρ)/ρ)²)⁻¹ - ε`; and by
`rule3_payoff_lt_beta` it earns less than the bound. -/
theorem corollary_i_N3 {ρ : ℝ} (hρ : 1 / 2 < ρ) (hρ1 : ρ < 1) {ε : ℝ} (hε : 0 < ε) :
    ∃ η₀ > 0, ∀ η, 0 < η → η < η₀ →
      beta ρ - ε < Pi3 η (gstar η) ρ ∧ Pi3 η (gstar η) ρ < beta ρ := by
  refine ⟨min (1 / 2) (ε ^ 2 / 144), by positivity, fun η hη hlt => ?_⟩
  have hη2 : η < 1 / 2 := lt_of_lt_of_le hlt (min_le_left _ _)
  have hηε : η < ε ^ 2 / 144 := lt_of_lt_of_le hlt (min_le_right _ _)
  have hη1 : η < 1 := by linarith
  have hg0 := gstar_pos hη hη1
  have hg1 := gstar_lt_one hη hη1
  obtain ⟨hlo, hhi⟩ := gstar_sq_bounds hη hη1
  set g := gstar η
  refine ⟨?_, rule3_payoff_lt_beta hη hη1.le hg0.le hg1.le hρ hρ1⟩
  have hρ0 : (0:ℝ) ≤ ρ := by linarith
  have hD := D3_pos hη hη1.le hg0.le hg1.le hρ0 hρ1.le
  have hDge := D3_ge hη hη1.le hg0.le hg1.le hρ0 hρ1.le
  have hgap := rule3_gap (γ := g) (ρ := ρ) hD.ne'
  set G := beta ρ - Pi3 η g ρ
  have hG0 : 0 ≤ G := by
    have := rule3_payoff_lt_beta hη hη1.le hg0.le hg1.le hρ hρ1; simp only [G]; linarith
  -- numerator ≤ 3η, denominator ≥ γ/4
  have hnum : (2 * ρ - 1) * (ρ * (1 - ρ) * (1 - η) * (g ^ 2 * (1 - η) + 2 * η) +
      η * (η + g * (1 - η))) ≤ 3 * η := by
    have hc : ρ * (1 - ρ) ≤ 1 / 4 := by nlinarith [sq_nonneg (ρ - 1 / 2)]
    have hc0 : 0 ≤ ρ * (1 - ρ) := by nlinarith
    have hu : 0 ≤ 1 - η := by linarith
    have a1 : ρ * (1 - ρ) * (1 - η) * (g ^ 2 * (1 - η) + 2 * η) ≤ η := by
      have : g ^ 2 * (1 - η) + 2 * η ≤ 4 * η := by nlinarith
      have : (1 - η) * (g ^ 2 * (1 - η) + 2 * η) ≤ 4 * η := by nlinarith
      nlinarith
    have a2 : η * (η + g * (1 - η)) ≤ 2 * η := by
      have : η + g * (1 - η) ≤ 2 := by nlinarith
      nlinarith [mul_le_mul_of_nonneg_left this hη.le]
    have a3 : 0 ≤ ρ * (1 - ρ) * (1 - η) * (g ^ 2 * (1 - η) + 2 * η) + η * (η + g * (1 - η)) := by
      positivity
    nlinarith
  have hden : g / 4 ≤ 2 * (ρ ^ 2 + (1 - ρ) ^ 2) * D3 η g ρ := by
    have h2 : 1 ≤ 2 * (ρ ^ 2 + (1 - ρ) ^ 2) := by nlinarith [sq_nonneg (ρ - 1 / 2)]
    have h3 : g / 4 ≤ D3 η g ρ := by
      have : 0 ≤ g * (1 / 2 - η) := mul_nonneg hg0.le (by linarith)
      nlinarith
    calc g / 4 ≤ D3 η g ρ := h3
      _ ≤ 2 * (ρ ^ 2 + (1 - ρ) ^ 2) * D3 η g ρ := le_mul_of_one_le_left hD.le h2
  have hd0 : 0 < 2 * (ρ ^ 2 + (1 - ρ) ^ 2) * D3 η g ρ := by
    have := sq_add_sq_pos ρ; positivity
  have hGD : G * (2 * (ρ ^ 2 + (1 - ρ) ^ 2) * D3 η g ρ) ≤ 3 * η := by
    rw [hgap, div_mul_cancel₀ _ hd0.ne']; exact hnum
  have hGg : G * (g / 4) ≤ 3 * η := (mul_le_mul_of_nonneg_left hden hG0).trans hGD
  have hGdef : G = beta ρ - Pi3 η g ρ := rfl
  clear_value G
  clear hgap hnum hGD hden hd0 hDge hD
  -- G² γ² ≤ 144 η² and γ² ≥ η give G² ≤ 144 η < ε²
  have h1 : G * g ≤ 12 * η := by linarith
  have h2 : (G * g) ^ 2 ≤ (12 * η) ^ 2 := pow_le_pow_left₀ (mul_nonneg hG0 hg0.le) h1 2
  have hG2 : G ^ 2 * η ≤ 144 * η * η := by
    have : G ^ 2 * η ≤ G ^ 2 * g ^ 2 := mul_le_mul_of_nonneg_left hlo (sq_nonneg G)
    have e : (G * g) ^ 2 = G ^ 2 * g ^ 2 := mul_pow G g 2
    nlinarith
  have hG3 : G ^ 2 < ε ^ 2 := by
    have : G ^ 2 ≤ 144 * η := le_of_mul_le_mul_right hG2 hη
    linarith
  have : G < ε := lt_of_abs_lt (abs_lt_of_sq_lt_sq hG3 hε.le)
  linarith

/-! ## Corollary (ii) at `N = 3`: the limiting beliefs -/

/-- The middle state carries no information: `f₂^H = f₂^L` (so `π(2) = 1/2`). -/
theorem rule3_mid_equal (η γ ρ : ℝ) : fcl η γ ρ 1 = fcl η γ (1 - ρ) 1 := by
  simp only [fcl, D3_symm]
  simp only [Matrix.cons_val_one, Matrix.cons_val_zero, sub_sub_cancel]
  ring

/-- The extreme states are mirror images: `f₁^S` in one state is `f₃` in the other. -/
theorem rule3_mirror (η γ ρ : ℝ) : fcl η γ ρ 0 = fcl η γ (1 - ρ) 2 ∧ fcl η γ (1 - ρ) 0 = fcl η γ ρ 2 := by
  constructor
  · show (1 - η) * (1 - ρ) * (η + (1 - η) * γ * (1 - ρ)) / D3 η γ ρ =
      (1 - η) * (1 - ρ) * (η + (1 - η) * γ * (1 - ρ)) / D3 η γ (1 - ρ)
    rw [D3_symm]
  · show (1 - η) * (1 - (1 - ρ)) * (η + (1 - η) * γ * (1 - (1 - ρ))) / D3 η γ (1 - ρ) =
      (1 - η) * ρ * (η + (1 - η) * γ * ρ) / D3 η γ ρ
    rw [D3_symm, sub_sub_cancel]

/-- Likelihood ratio of state `3`:
`f₃^H/f₃^L = (ρ/(1-ρ)) · (η + uγρ)/(η + uγ(1-ρ))` (eq. (4) for this rule). -/
theorem rule3_lr3 {η γ ρ : ℝ} (hη : 0 < η) (hη1 : η < 1) (hγ : 0 ≤ γ) (hγ1 : γ ≤ 1)
    (hρ : 0 < ρ) (hρ1 : ρ < 1) :
    fcl η γ ρ 2 / fcl η γ (1 - ρ) 2 =
      ρ * (η + (1 - η) * γ * ρ) / ((1 - ρ) * (η + (1 - η) * γ * (1 - ρ))) := by
  have hD := D3_pos hη hη1.le hγ hγ1 hρ.le hρ1.le
  have hu : 0 ≤ 1 - η := by linarith
  have hu' : 1 - η ≠ 0 := by linarith
  have h1ρ : 1 - ρ ≠ 0 := by linarith
  have hA : 0 < η + (1 - η) * γ * (1 - ρ) := by
    have : 0 ≤ (1 - η) * γ * (1 - ρ) := by have := hρ1; positivity
    linarith
  have hB : 0 < η + (1 - η) * γ * ρ := by
    have : 0 ≤ (1 - η) * γ * ρ := by positivity
    linarith
  simp only [fcl, D3_symm]
  simp only [Matrix.cons_val_two, Matrix.tail_cons, Matrix.head_cons, sub_sub_cancel]
  rw [div_div_div_cancel_right₀ hD.ne']
  field_simp

/-- **The gap to Corollary (ii)** at state `3`:
`(ρ/(1-ρ))² - f₃^H/f₃^L = ρ(2ρ-1)η / ((1-ρ)² (η + uγ(1-ρ)))`.  It vanishes
exactly as `η/γ → 0`; at `γ = 0` the ratio is `ρ/(1-ρ)`, one signal's worth
instead of two. -/
theorem rule3_lr3_gap {η γ ρ : ℝ} (hA : η + (1 - η) * γ * (1 - ρ) ≠ 0) (hρ1 : ρ ≠ 1) :
    (ρ / (1 - ρ)) ^ 2 - ρ * (η + (1 - η) * γ * ρ) / ((1 - ρ) * (η + (1 - η) * γ * (1 - ρ))) =
      ρ * (2 * ρ - 1) * η / ((1 - ρ) ^ 2 * (η + (1 - η) * γ * (1 - ρ))) := by
  have h1 : 1 - ρ ≠ 0 := sub_ne_zero.2 (Ne.symm hρ1)
  have key : ρ ^ 2 * (η + (1 - η) * γ * (1 - ρ)) - ρ * (1 - ρ) * (η + (1 - η) * γ * ρ) =
      ρ * (2 * ρ - 1) * η := by ring
  generalize η + (1 - η) * γ * (1 - ρ) = A at hA key ⊢
  generalize η + (1 - η) * γ * ρ = B at key ⊢
  rw [div_pow, div_sub_div _ _ (pow_ne_zero 2 h1) (mul_ne_zero h1 hA),
    div_eq_div_iff (mul_ne_zero (pow_ne_zero 2 h1) (mul_ne_zero h1 hA))
      (mul_ne_zero (pow_ne_zero 2 h1) hA)]
  linear_combination (1 - ρ) ^ 3 * A * key

theorem rule3_lr3_absorbing {η ρ : ℝ} (hη : 0 < η) :
    ρ * (η + (1 - η) * 0 * ρ) / ((1 - ρ) * (η + (1 - η) * 0 * (1 - ρ))) = ρ / (1 - ρ) := by
  have := hη.ne'
  simp only [mul_zero, zero_mul, add_zero]
  rw [mul_div_mul_right _ _ this]

/-- **Corollary (ii) at `N = 3`, state 3** (p.12-13): along the optimal
`γ*(η)`, `f₃^H/f₃^L → (ρ/(1-ρ))^{(3-1)} ((1-ρ)/ρ)^{0} = (ρ/(1-ρ))²`. -/
theorem corollary_ii_N3 {ρ : ℝ} (hρ : 1 / 2 < ρ) (hρ1 : ρ < 1) {ε : ℝ} (hε : 0 < ε) :
    ∃ η₀ > 0, ∀ η, 0 < η → η < η₀ →
      |fcl η (gstar η) ρ 2 / fcl η (gstar η) (1 - ρ) 2 - (ρ / (1 - ρ)) ^ 2| < ε := by
  set C : ℝ := 2 / (1 - ρ) ^ 3
  have h1ρ : 0 < 1 - ρ := by linarith
  have hC : 0 < C := by positivity
  refine ⟨min (1 / 2) (ε ^ 2 / C ^ 2), by positivity, fun η hη hlt => ?_⟩
  have hη2 : η < 1 / 2 := lt_of_lt_of_le hlt (min_le_left _ _)
  have hηε : η < ε ^ 2 / C ^ 2 := lt_of_lt_of_le hlt (min_le_right _ _)
  have hη1 : η < 1 := by linarith
  have hg0 := gstar_pos hη hη1
  have hg1 := gstar_lt_one hη hη1
  obtain ⟨hlo, -⟩ := gstar_sq_bounds hη hη1
  set g := gstar η
  have hρ0 : 0 < ρ := by linarith
  rw [rule3_lr3 hη hη1 hg0.le hg1.le hρ0 hρ1]
  set A := η + (1 - η) * g * (1 - ρ)
  have hA : g * (1 - ρ) / 2 ≤ A := by
    have : 0 ≤ g * (1 - ρ) * (1 / 2 - η) := by
      have : 0 ≤ 1 / 2 - η := by linarith
      positivity
    simp only [A]; nlinarith
  have hApos : 0 < A := lt_of_lt_of_le (by positivity) hA
  rw [abs_sub_comm, rule3_lr3_gap hApos.ne' hρ1.ne]
  set G := ρ * (2 * ρ - 1) * η / ((1 - ρ) ^ 2 * A)
  have hG0 : 0 ≤ G := by
    have : 0 ≤ 2 * ρ - 1 := by linarith
    positivity
  rw [abs_of_nonneg hG0]
  -- G · g ≤ C η
  have hGg : G * g ≤ C * η := by
    have hden : 0 < (1 - ρ) ^ 2 * A := by positivity
    have e : G * ((1 - ρ) ^ 2 * A) = ρ * (2 * ρ - 1) * η := by
      simp only [G]; field_simp
    have hnum : ρ * (2 * ρ - 1) * η ≤ η := by
      have : ρ * (2 * ρ - 1) ≤ 1 := by nlinarith
      nlinarith
    have : G * ((1 - ρ) ^ 3 * g / 2) ≤ η := by
      have : (1 - ρ) ^ 3 * g / 2 ≤ (1 - ρ) ^ 2 * A := by
        have : (1 - ρ) ^ 3 * g / 2 = (1 - ρ) ^ 2 * (g * (1 - ρ) / 2) := by ring
        rw [this]; exact mul_le_mul_of_nonneg_left hA (by positivity)
      calc G * ((1 - ρ) ^ 3 * g / 2) ≤ G * ((1 - ρ) ^ 2 * A) :=
            mul_le_mul_of_nonneg_left this hG0
        _ = ρ * (2 * ρ - 1) * η := e
        _ ≤ η := hnum
    have hc : C * ((1 - ρ) ^ 3 / 2) = 1 := by simp only [C]; field_simp
    calc G * g = (G * ((1 - ρ) ^ 3 * g / 2)) * C := by
          rw [show (G * ((1 - ρ) ^ 3 * g / 2)) * C = G * g * (C * ((1 - ρ) ^ 3 / 2)) by ring, hc,
            mul_one]
      _ ≤ η * C := mul_le_mul_of_nonneg_right this hC.le
      _ = C * η := by ring
  have hGdef : G = ρ * (2 * ρ - 1) * η / ((1 - ρ) ^ 2 * A) := rfl
  clear_value G
  have h2 : (G * g) ^ 2 ≤ (C * η) ^ 2 := pow_le_pow_left₀ (mul_nonneg hG0 hg0.le) hGg 2
  have hG2 : G ^ 2 * η ≤ C ^ 2 * η * η := by
    have : G ^ 2 * η ≤ G ^ 2 * g ^ 2 := mul_le_mul_of_nonneg_left hlo (sq_nonneg G)
    have e : (G * g) ^ 2 = G ^ 2 * g ^ 2 := mul_pow G g 2
    have e2 : (C * η) ^ 2 = C ^ 2 * η * η := by ring
    linarith
  have hG3 : G ^ 2 < ε ^ 2 := by
    have : G ^ 2 ≤ C ^ 2 * η := le_of_mul_le_mul_right hG2 hη
    have : C ^ 2 * η < ε ^ 2 := by
      rw [lt_div_iff₀ (by positivity)] at hηε; linarith
    linarith
  exact lt_of_abs_lt (abs_lt_of_sq_lt_sq hG3 hε.le)

/-! ## Lemma 4 and the symmetry `α* = 1` (pp.16, 39-40) -/

/-- **Lemma 4's identity** (p.40):
`(1+r)(1+x)(1+r²/x)·[2/(1+r) - (1/(1+x) + 1/(1+r²/x))] = (1-r)(x-r)²/x`. -/
theorem lemma4_identity {r x : ℝ} (hx : 0 < x) (hr : 0 ≤ r) :
    (1 + r) * (1 + x) * (1 + r ^ 2 / x) * (2 / (1 + r) - (1 / (1 + x) + 1 / (1 + r ^ 2 / x))) =
      (1 - r) / x * (x - r) ^ 2 := by
  have h1 : 1 + r ≠ 0 := by positivity
  have h2 : 1 + x ≠ 0 := by positivity
  have h3 : x + r ^ 2 ≠ 0 := by positivity
  have hx' := hx.ne'
  field_simp
  ring

/-- **Lemma 4** (p.39): for `0 ≤ r ≤ 1` and `x > 0`,
`½(1/(1+x) + 1/(1+r²/x)) ≤ 1/(1+r)`, with equality iff `x = r` when `r < 1`. -/
theorem lemma4 {r x : ℝ} (hx : 0 < x) (hr : 0 ≤ r) (hr1 : r ≤ 1) :
    1 / 2 * (1 / (1 + x) + 1 / (1 + r ^ 2 / x)) ≤ 1 / (1 + r) := by
  have hid := lemma4_identity hx hr
  have hpos : 0 < (1 + r) * (1 + x) * (1 + r ^ 2 / x) := by positivity
  have hrhs : 0 ≤ (1 - r) / x * (x - r) ^ 2 := by
    have : 0 ≤ 1 - r := by linarith
    positivity
  have : 0 ≤ 2 / (1 + r) - (1 / (1 + x) + 1 / (1 + r ^ 2 / x)) := by
    by_contra hc
    have := mul_neg_of_pos_of_neg hpos (lt_of_not_ge hc)
    linarith
  have e : 2 / (1 + r) = 2 * (1 / (1 + r)) := by ring
  linarith

theorem lemma4_eq_iff {r x : ℝ} (hx : 0 < x) (hr : 0 ≤ r) (hr1 : r < 1) :
    1 / 2 * (1 / (1 + x) + 1 / (1 + r ^ 2 / x)) = 1 / (1 + r) ↔ x = r := by
  have hid := lemma4_identity hx hr
  have hpos : 0 < (1 + r) * (1 + x) * (1 + r ^ 2 / x) := by positivity
  constructor
  · intro h
    have : 2 / (1 + r) - (1 / (1 + x) + 1 / (1 + r ^ 2 / x)) = 0 := by
      have e : 2 / (1 + r) = 2 * (1 / (1 + r)) := by ring
      rw [e, ← h]; ring
    rw [this, mul_zero] at hid
    have h1r : (1 - r) / x ≠ 0 := div_ne_zero (by linarith) hx.ne'
    have := (mul_eq_zero.1 hid.symm).resolve_left h1r
    have := pow_eq_zero_iff (n := 2) (by norm_num) |>.1 this
    linarith
  · intro h
    subst h
    rw [show x ^ 2 / x = x by field_simp]; ring

/-- **The `N = 3` sketch's symmetry** (p.16): with `q = ((1-ρ)/ρ)²`, the limit
payoff `½[1/(1 + q/α*) + 1/(1 + α* q)]` is maximized at `α* = 1`, where it is
`1/(1+q)`. -/
theorem alpha_star_one {q α : ℝ} (hq : 0 < q) (hq1 : q < 1) (hα : 0 < α) :
    1 / 2 * (1 / (1 + q / α) + 1 / (1 + α * q)) ≤ 1 / (1 + q) ∧
      (1 / 2 * (1 / (1 + q / α) + 1 / (1 + α * q)) = 1 / (1 + q) ↔ α = 1) := by
  have hx : 0 < α * q := mul_pos hα hq
  have e : q / α = q ^ 2 / (α * q) := by field_simp
  rw [e, add_comm (1 / (1 + q ^ 2 / (α * q)))]
  refine ⟨lemma4 hx hq.le hq1.le, ?_⟩
  rw [lemma4_eq_iff hx hq.le hq1]
  constructor
  · intro h
    have : (α - 1) * q = 0 := by linarith
    rcases mul_eq_zero.1 this with h0 | h0
    · linarith
    · exact absurd h0 hq.ne'
  · intro h; subst h; ring

/-! ## Theorem 6: overconfidence and underconfidence -/

/-- Corollary (ii)'s limiting likelihood ratio of memory state `i ∈ {1..N}`:
`(ρ/(1-ρ))^{i-1} ((1-ρ)/ρ)^{N-i}`, "exactly the Bayesian beliefs for a sequence
containing `(i-1)` h-signals and `(N-i)` l-signals" (p.13). -/
noncomputable def lrLimit (ρ : ℝ) (N i : ℕ) : ℝ := (ρ / (1 - ρ)) ^ (i - 1) * ((1 - ρ) / ρ) ^ (N - i)

/-- Moving up one memory state multiplies the limiting likelihood ratio by
`(ρ/(1-ρ))²`: "beliefs adjust as if he had received two h-signals" (p.24). -/
theorem lrLimit_step {ρ : ℝ} (hρ : 0 < ρ) (hρ1 : ρ < 1) {N i : ℕ} (hi : 1 ≤ i) (hiN : i < N) :
    lrLimit ρ N (i + 1) = (ρ / (1 - ρ)) ^ 2 * lrLimit ρ N i := by
  unfold lrLimit
  have h1 : 1 - ρ ≠ 0 := by linarith
  obtain ⟨a, rfl⟩ : ∃ a, i = a + 1 := ⟨i - 1, by omega⟩
  obtain ⟨b, hb⟩ : ∃ b, N - (a + 1) = b + 1 := ⟨N - (a + 1) - 1, by omega⟩
  have hb' : N - (a + 1 + 1) = b := by omega
  rw [hb, hb', show a + 1 + 1 - 1 = a + 1 by omega, show a + 1 - 1 = a by omega]
  have htu : ρ / (1 - ρ) * ((1 - ρ) / ρ) = 1 := by field_simp
  calc (ρ / (1 - ρ)) ^ (a + 1) * ((1 - ρ) / ρ) ^ b
      = (ρ / (1 - ρ)) ^ (a + 1) * ((1 - ρ) / ρ) ^ b * (ρ / (1 - ρ) * ((1 - ρ) / ρ)) := by
        rw [htu, mul_one]
    _ = (ρ / (1 - ρ)) ^ 2 * ((ρ / (1 - ρ)) ^ a * ((1 - ρ) / ρ) ^ (b + 1)) := by ring

theorem lrLimit_top {ρ : ℝ} (N : ℕ) : lrLimit ρ N N = (ρ / (1 - ρ)) ^ (N - 1) := by
  simp [lrLimit]

/-- At `N = 3` these are the limits proved above: `(ρ/(1-ρ))²`, `1`, `((1-ρ)/ρ)²`. -/
theorem lrLimit_N3 {ρ : ℝ} (hρ : 0 < ρ) (hρ1 : ρ < 1) :
    lrLimit ρ 3 3 = (ρ / (1 - ρ)) ^ 2 ∧ lrLimit ρ 3 2 = 1 ∧ lrLimit ρ 3 1 = ((1 - ρ) / ρ) ^ 2 := by
  have h1 : 1 - ρ ≠ 0 := by linarith
  refine ⟨by simp [lrLimit], ?_, by simp [lrLimit]⟩
  simp [lrLimit]
  field_simp

/-- The probability of `H` implied by odds `t^k` (`t = ρ/(1-ρ)`); confidence is
its distance from `½`, which is increasing in `k` (p.23). -/
noncomputable def oddsProb (t : ℝ) (k : ℕ) : ℝ := t ^ k / (1 + t ^ k)

theorem oddsProb_strictMono {t : ℝ} (ht : 1 < t) : StrictMono (oddsProb t) := by
  intro k k' hkk
  unfold oddsProb
  have h1 : t ^ k < t ^ k' := pow_lt_pow_right₀ ht hkk
  have h0 : 0 < t ^ k := by positivity
  rw [div_lt_div_iff₀ (by positivity) (by positivity)]
  nlinarith

theorem oddsProb_gt_half {t : ℝ} (ht : 1 < t) {k : ℕ} (hk : 0 < k) : 1 / 2 < oddsProb t k := by
  unfold oddsProb
  have : 1 < t ^ k := one_lt_pow₀ ht hk.ne'
  rw [lt_div_iff₀ (by positivity)]
  linarith

/-- **Theorem 6 (i)** (pp.23-24).  In the limit beliefs, a sequence with net
`Δ > 0` h-signals that stays in the interior moves the DM `Δ` states up from the
middle, to odds `(ρ/(1-ρ))^{2Δ}`; the Bayesian's odds are `(ρ/(1-ρ))^Δ`.  The
DM is strictly more confident: overconfident. -/
theorem thm6_i {ρ : ℝ} (hρ : 1 / 2 < ρ) (hρ1 : ρ < 1) {Δ : ℕ} (hΔ : 0 < Δ) :
    |oddsProb (ρ / (1 - ρ)) Δ - 1 / 2| < |oddsProb (ρ / (1 - ρ)) (2 * Δ) - 1 / 2| := by
  have ht : 1 < ρ / (1 - ρ) := by rw [lt_div_iff₀ (by linarith)]; linarith
  have h1 := oddsProb_gt_half ht hΔ
  have h2 := oddsProb_strictMono ht (show Δ < 2 * Δ by omega)
  rw [abs_of_pos (by linarith), abs_of_pos (by linarith)]
  linarith

/-- **Theorem 6 (iii)** (p.24).  No memory state is more confident than state
`N`, whose limiting odds are `(ρ/(1-ρ))^{N-1}`; after a sequence with net
`δ > N-1` signals the Bayesian is strictly more confident: underconfident. -/
theorem thm6_iii {ρ : ℝ} (hρ : 1 / 2 < ρ) (hρ1 : ρ < 1) {N δ k : ℕ} (hk : k ≤ N - 1)
    (hδ : N - 1 < δ) :
    oddsProb (ρ / (1 - ρ)) k < oddsProb (ρ / (1 - ρ)) δ := by
  have ht : 1 < ρ / (1 - ρ) := by rw [lt_div_iff₀ (by linarith)]; linarith
  exact oddsProb_strictMono ht (by omega)

/-! ## Theorem 7 (ii): why weak signals are ignored -/

/-- **The informational core of Theorem 7 (ii)** (pp.26-27).  With `K` signals,
a transition taken with probabilities `x_k = σ(i,k)(j) ≥ 0` has likelihood ratio
`∑ μ^H_k x_k / ∑ μ^L_k x_k`, at most the largest signal ratio `M`. -/
theorem move_ratio_le {K : Type*} [Fintype K] (μH μL x : K → ℝ) (M : ℝ)
    (hM : ∀ k, μH k ≤ M * μL k) (hx : ∀ k, 0 ≤ x k) :
    ∑ k, μH k * x k ≤ M * ∑ k, μL k * x k := by
  rw [Finset.mul_sum]
  exact Finset.sum_le_sum fun k _ => by nlinarith [hM k, hx k]

/-- ... and strictly less as soon as the move is ever taken after a signal whose
own ratio is below `M`, e.g. an uninformative signal (`μ^H_k = μ^L_k`, ratio `1`).
So the most informative upward moves use only the most extreme signal. -/
theorem move_ratio_lt {K : Type*} [Fintype K] (μH μL x : K → ℝ) (M : ℝ)
    (hM : ∀ k, μH k ≤ M * μL k) (hx : ∀ k, 0 ≤ x k) (k₀ : K) (hx₀ : 0 < x k₀)
    (hlt : μH k₀ < M * μL k₀) :
    ∑ k, μH k * x k < M * ∑ k, μL k * x k := by
  rw [Finset.mul_sum]
  exact Finset.sum_lt_sum (fun k _ => by nlinarith [hM k, hx k])
    ⟨k₀, Finset.mem_univ _, by nlinarith⟩

/-! ## Sections 4-5 in the short run: the Theorem 3 skeleton

Memory states are the naturals `1, …, N`.  By Theorem 3 (ii) an interior state
moves up after `h` and down after `l` with probability one.  By Theorem 3 (i) an
extreme state is left with probability below `ε`, so over a finite horizon and
small `η` the DM stays there with probability close to one; `skel` is the
path on which every visit to an extreme state stays. -/

/-- One step of the skeleton: interior states move by one, extremes stay. -/
def skel (N i : ℕ) (s : Sig) : ℕ :=
  if i ≤ 1 ∨ N ≤ i then i else
    match s with
    | .h => i + 1
    | .l => i - 1

/-- The skeleton state after the signal sequence `w`. -/
def run (N i : ℕ) (w : List Sig) : ℕ := w.foldl (skel N) i

theorem run_nil (N i : ℕ) : run N i [] = i := rfl

theorem skel_h {N i : ℕ} (h1 : 1 < i) (hN : i < N) : skel N i .h = i + 1 := by
  unfold skel; rw [if_neg (by omega)]

theorem skel_l {N i : ℕ} (h1 : 1 < i) (hN : i < N) : skel N i .l = i - 1 := by
  unfold skel; rw [if_neg (by omega)]

theorem skel_one (N : ℕ) (s : Sig) : skel N 1 s = 1 := by
  unfold skel; rw [if_pos (by omega)]

theorem skel_top (N : ℕ) (s : Sig) : skel N N s = N := by
  unfold skel; rw [if_pos (by omega)]

theorem run_cons (N i : ℕ) (s : Sig) (w : List Sig) : run N i (s :: w) = run N (skel N i s) w :=
  rfl

theorem run_append (N i : ℕ) (u v : List Sig) : run N i (u ++ v) = run N (run N i u) v := by
  simp [run, List.foldl_append]

theorem run_one (N : ℕ) (w : List Sig) : run N 1 w = 1 := by
  induction w with
  | nil => rfl
  | cons s w ih => rw [run_cons, skel_one, ih]

theorem run_top (N : ℕ) (w : List Sig) : run N N w = N := by
  induction w with
  | nil => rfl
  | cons s w ih => rw [run_cons, skel_top, ih]

theorem skel_bounds {N i : ℕ} (s : Sig) (h1 : 1 ≤ i) (hN : i ≤ N) :
    1 ≤ skel N i s ∧ skel N i s ≤ N := by
  unfold skel
  split_ifs with h
  · exact ⟨h1, hN⟩
  · cases s <;> simp <;> omega

theorem run_bounds {N i : ℕ} (w : List Sig) (h1 : 1 ≤ i) (hN : i ≤ N) :
    1 ≤ run N i w ∧ run N i w ≤ N := by
  induction w generalizing i with
  | nil => exact ⟨h1, hN⟩
  | cons s w ih =>
    rw [run_cons]
    obtain ⟨a, b⟩ := skel_bounds s h1 hN
    exact ih a b

theorem run_rep_h {N : ℕ} : ∀ (m i : ℕ), 1 < i → i ≤ N →
    run N i (List.replicate m .h) = min (i + m) N := by
  intro m
  induction m with
  | zero => intro i _ hN; simp [run_nil]; omega
  | succ m ih =>
    intro i h1 hN
    rw [List.replicate_succ, run_cons]
    by_cases hiN : i = N
    · subst hiN
      rw [skel_top, run_top]; omega
    · rw [skel_h h1 (by omega), ih (i + 1) (by omega) (by omega)]
      omega

theorem run_rep_l {N : ℕ} : ∀ (m i : ℕ), m + 1 ≤ i → i < N →
    run N i (List.replicate m .l) = i - m := by
  intro m
  induction m with
  | zero => intro i _ _; simp [run_nil]
  | succ m ih =>
    intro i h1 hN
    rw [List.replicate_succ, run_cons, skel_l (by omega) hN, ih (i - 1) (by omega) (by omega)]
    omega

/-- **Theorem 4 (i), the pathwise step of its proof** (p.19): with the extreme
states absorbing over the horizon, moving a block `h^τ` of high signals from the
end of a sequence to its start "cannot reduce the posterior memory state".
Starting in any interior state `c`,
`run(w ++ h^τ) ≤ run(h^τ ++ w)` for every `w` and `τ`. -/
theorem first_impressions_pathwise {N c : ℕ} (hc1 : 1 < c) (hcN : c < N) (τ : ℕ)
    (w : List Sig) :
    run N c (w ++ List.replicate τ .h) ≤ run N c (List.replicate τ .h ++ w) := by
  -- the coupling: two paths started `y - x` apart stay that far apart until one is absorbed
  have P : ∀ (w : List Sig) (x y : ℕ), 1 ≤ x → x ≤ y → y ≤ N →
      run N x w = 1 ∨ min (run N x w + (y - x)) N ≤ run N y w := by
    intro w
    induction w with
    | nil => intro x y _ hxy hyN; right; simp [run_nil]; omega
    | cons s w ih =>
      intro x y hx hxy hyN
      rw [run_cons, run_cons]
      by_cases hx1 : x ≤ 1
      · left
        have hx' : x = 1 := by omega
        subst hx'; rw [skel_one, run_one]
      by_cases hyN' : N ≤ y
      · right
        have hy' : y = N := by omega
        subst hy'; rw [skel_top, run_top]; omega
      have hxN : x < N := by omega
      have hy1 : 1 < y := by omega
      cases s with
      | h =>
        rw [skel_h (by omega) hxN, skel_h hy1 (by omega)]
        have := ih (x + 1) (y + 1) (by omega) (by omega) (by omega)
        rwa [show y + 1 - (x + 1) = y - x by omega] at this
      | l =>
        rw [skel_l (by omega) hxN, skel_l hy1 (by omega)]
        have := ih (x - 1) (y - 1) (by omega) (by omega) (by omega)
        rwa [show y - 1 - (x - 1) = y - x by omega] at this
  rw [run_append, run_append, run_rep_h τ c hc1 hcN.le]
  set y := min (c + τ) N
  obtain ⟨hz1, hzN⟩ := run_bounds (N := N) w (by omega : 1 ≤ c) hcN.le
  have hy := run_bounds (N := N) w (i := y) (by omega) (by omega)
  rcases P w c y (by omega) (by omega) (by omega) with h | h
  · rw [h, run_one]; exact hy.1
  · by_cases hz : run N c w = 1
    · rw [hz, run_one]; exact hy.1
    rw [run_rep_h τ _ (by omega) hzN]
    by_cases hyc : c + τ ≤ N
    · have : y = c + τ := by omega
      rw [this] at h ⊢
      rwa [show c + τ - c = τ by omega] at h
    · have : y = N := by omega
      rw [this, run_top]; omega

/-- ... and the inequality can be strict: `N = 5`, start in the middle, `τ = 1`,
`w = l l` ends in state `1` with the block last and in state `2` with it first. -/
theorem first_impressions_strict :
    run 5 3 ([.l, .l] ++ [.h]) = 1 ∧ run 5 3 ([.h] ++ [.l, .l]) = 2 := by decide

/-- **The Section 4 example** (p.21): `N = 5`, Agent 1 in state 2, Agent 2 in
state 4, the six orderings of two `L` and two `H` reports.  Final skeleton states
`(Agent 1, Agent 2)`: `LHHL ↦ (1,5)`, `LHLH ↦ (1,4)`, `LLHH ↦ (1,4)`,
`HLLH ↦ (1,5)`, `HLHL ↦ (2,5)`, `HHLL ↦ (2,5)`, as the text says.  (The text
has "`τ = 4`, so there are `C(4,2) = 6` possible orderings"; with `τ` reports of
each type that is `τ = 2`.) -/
theorem section4_example :
    [[Sig.l, .h, .h, .l], [.l, .h, .l, .h], [.l, .l, .h, .h],
      [.h, .l, .l, .h], [.h, .l, .h, .l], [.h, .h, .l, .l]].map (fun w => (run 5 2 w, run 5 4 w)) =
      [(1, 5), (1, 4), (1, 4), (1, 5), (2, 5), (2, 5)] ∧ Nat.choose 4 2 = 6 := by
  constructor <;> decide

/-- **The introduction's example** (p.6): four memory states, agents in states 2
and 3, signals `l, h, h`: Agent 1 ends in state 1, Agent 2 in state 4.  (The
model assumes `N` odd, fn 8; the example uses `N = 4`.) -/
theorem intro_example : run 4 2 [.l, .h, .h] = 1 ∧ run 4 3 [.l, .h, .h] = 4 := by decide

/-! ### Theorem 5 (i): positive probability of polarization -/

/-- A memory transition kernel on the states `1..N` (as naturals). -/
abbrev Kernel := ℕ → Sig → ℕ → ℝ

/-- Theorem 3 (ii): from an interior state the move is `skel` with probability one. -/
def Thm3ii (N : ℕ) (σ : Kernel) : Prop :=
  ∀ i s j, 1 < i → i < N → 0 < σ i s j → j = skel N i s

/-- `σ(i, s) ∈ Δ(𝒩)`: moves stay in `1..N`. -/
def InStates (N : ℕ) (σ : Kernel) : Prop := ∀ i s j, 0 < σ i s j → 1 ≤ j ∧ j ≤ N

/-- `j` is reached from `i` along `w` with positive probability. -/
inductive PosReach (σ : Kernel) : ℕ → List Sig → ℕ → Prop
  | nil (i : ℕ) : PosReach σ i [] i
  | cons {i i' j : ℕ} {s : Sig} {w : List Sig} :
      0 < σ i s i' → PosReach σ i' w j → PosReach σ i (s :: w) j

/-- Two agents who start in `j < k` and see the same signals `w` end with
`i^j < j < k < i^k`, with positive probability.  (Every signal sequence has
positive probability for `0 < ρ < 1`; the agents' memory draws are independent.) -/
def Diverges (σ : Kernel) (j k : ℕ) (w : List Sig) : Prop :=
  ∃ a b, PosReach σ j w a ∧ PosReach σ k w b ∧ a < j ∧ k < b

/-- A superset of the positive-probability states under Theorem 3 (ii) alone
(extreme states may go anywhere). -/
def nextOK (N i : ℕ) (s : Sig) (j : ℕ) : Bool :=
  if 1 < i ∧ i < N then j == skel N i s else (1 ≤ j && j ≤ N)

def reachL (N : ℕ) : List ℕ → List Sig → List ℕ
  | S, [] => S
  | S, s :: w => reachL N ((List.range' 1 N).filter fun j => S.any fun i => nextOK N i s j) w

theorem reach_sound {N : ℕ} {σ : Kernel} (h3 : Thm3ii N σ) (hS : InStates N σ)
    {i : ℕ} {w : List Sig} {j : ℕ} (h : PosReach σ i w j) :
    ∀ S : List ℕ, i ∈ S → j ∈ reachL N S w := by
  induction h with
  | nil i => intro S hi; exact hi
  | @cons i i' j s w hpos _ ih =>
    intro S hi
    apply ih
    obtain ⟨h1, hN⟩ := hS i s i' hpos
    simp only [List.mem_filter, List.mem_range', List.any_eq_true]
    refine ⟨⟨i' - 1, by omega, by omega⟩, i, hi, ?_⟩
    unfold nextOK
    split_ifs with hint
    · simp [h3 i s i' hint.1 hint.2 hpos]
    · simp [h1, hN]

/-- **Theorem 5 (i) as stated in the draft fails** (p.22).  With `N = 5`, `j = 2`,
`k = 4`, `t = 2`, the hypotheses `1 < j < k < N` and `t ≥ N - 1 - (k - j)` hold,
but for *every* kernel satisfying Theorem 3 (ii) no two-signal sequence
polarizes the two agents: `Pr{i^j_t < j < k < i^k_t} = 0`. -/
theorem thm5_draft_counterexample (σ : Kernel) (h3 : Thm3ii 5 σ) (hS : InStates 5 σ) :
    (1 < 2 ∧ 2 < 4 ∧ 4 < 5 ∧ 5 - 1 - (4 - 2) ≤ 2) ∧
      ∀ w : List Sig, w.length = 2 → ¬ Diverges σ 2 4 w := by
  refine ⟨by decide, fun w hw ⟨a, b, ha, hb, hlt, hgt⟩ => ?_⟩
  have ha' := reach_sound h3 hS ha [2] (by simp)
  have hb' := reach_sound h3 hS hb [4] (by simp)
  match w, hw with
  | [s₁, s₂], _ =>
    have key : ∀ s₁ s₂ : Sig, ∀ a ∈ reachL 5 [2] [s₁, s₂], ∀ b ∈ reachL 5 [4] [s₁, s₂],
        ¬ (a < 2 ∧ 4 < b) := by decide
    exact key s₁ s₂ a ha' b hb' ⟨hlt, hgt⟩

/-- The same failure at `N = 7`, `j = 3`, `k = 5`, `t = 4 = N - 1 - (k - j)`. -/
theorem thm5_draft_counterexample_N7 (σ : Kernel) (h3 : Thm3ii 7 σ) (hS : InStates 7 σ) :
    ∀ w : List Sig, w.length = 4 → ¬ Diverges σ 3 5 w := by
  intro w hw ⟨a, b, ha, hb, hlt, hgt⟩
  have ha' := reach_sound h3 hS ha [3] (by simp)
  have hb' := reach_sound h3 hS hb [5] (by simp)
  match w, hw with
  | [s₁, s₂, s₃, s₄], _ =>
    have key : ∀ s₁ s₂ s₃ s₄ : Sig, ∀ a ∈ reachL 7 [3] [s₁, s₂, s₃, s₄],
        ∀ b ∈ reachL 7 [5] [s₁, s₂, s₃, s₄], ¬ (a < 3 ∧ 5 < b) := by decide
    exact key s₁ s₂ s₃ s₄ a ha' b hb' ⟨hlt, hgt⟩

/-- Theorem 3's structure with positive staying probability at the extremes
(Theorem 3 (i): `τ_{11}, τ_{NN} > 1 - ε > 0`, per signal). -/
def StayOK (N : ℕ) (σ : Kernel) : Prop :=
  (∀ s, 0 < σ 1 s 1) ∧ (∀ s, 0 < σ N s N) ∧ ∀ i s, 1 < i → i < N → 0 < σ i s (skel N i s)

theorem posReach_run {N : ℕ} {σ : Kernel} (hσ : StayOK N σ) :
    ∀ (w : List Sig) (i : ℕ), 1 ≤ i → i ≤ N → PosReach σ i w (run N i w) := by
  intro w
  induction w with
  | nil => intro i _ _; exact .nil i
  | cons s w ih =>
    intro i h1 hN
    rw [run_cons]
    obtain ⟨a, b⟩ := skel_bounds (N := N) s h1 hN
    refine .cons ?_ (ih _ a b)
    by_cases hi1 : i ≤ 1
    · have : i = 1 := by omega
      subst this; rw [skel_one]; exact hσ.1 s
    by_cases hiN : N ≤ i
    · have : i = N := by omega
      subst this; rw [skel_top]; exact hσ.2.1 s
    exact hσ.2.2 i s (by omega) (by omega)

/-- **Theorem 5 (i), with the length its proof needs** (p.22).  For
`1 < j < k < N`, the proof's sequence — `(j-1)` low signals, then at least
`(j-1+N-k)` high ones — takes the first agent to state `1` and the second to
`N`; so polarization has positive probability for every
`t ≥ (j-1) + (j-1+N-k) = 2j - 2 + N - k`. -/
theorem thm5_i_corrected {N j k : ℕ} (hj : 1 < j) (hjk : j < k) (hk : k < N) (m : ℕ)
    (hm : N - k + j - 1 ≤ m) (σ : Kernel) (hσ : StayOK N σ) :
    let w := List.replicate (j - 1) Sig.l ++ List.replicate m .h
    run N j w = 1 ∧ run N k w = N ∧ Diverges σ j k w ∧ w.length = j - 1 + m := by
  intro w
  have hrj : run N j w = 1 := by
    simp only [w]
    rw [run_append, run_rep_l (N := N) (j - 1) j (by omega) (by omega),
      show j - (j - 1) = 1 by omega, run_one]
  have hrk : run N k w = N := by
    simp only [w]
    rw [run_append, run_rep_l (N := N) (j - 1) k (by omega) (by omega),
      run_rep_h (N := N) m (k - (j - 1)) (by omega) (by omega)]
    omega
  refine ⟨hrj, hrk, ⟨1, N, ?_, ?_, by omega, hk⟩, by simp [w]⟩
  · rw [← hrj]; exact posReach_run hσ w j (by omega) (by omega)
  · rw [← hrk]; exact posReach_run hσ w k (by omega) (by omega)

/-- Whether the skeleton polarizes `j < k` along some sequence of length `t`. -/
def allSeqs : ℕ → List (List Sig)
  | 0 => [[]]
  | t + 1 => (allSeqs t).flatMap fun w => [Sig.l :: w, Sig.h :: w]

def skelDiverges (N j k t : ℕ) : Bool :=
  (allSeqs t).any fun w => decide (run N j w < j) && decide (k < run N k w)

def supDiverges (N j k t : ℕ) : Bool :=
  (allSeqs t).any fun w => (reachL N [j] w).any (· < j) && (reachL N [k] w).any (k < ·)

/-- **The exact threshold at `N = 5`.**  For every `1 < j < k < 5` and `t ≤ 6`,
polarization is possible (on the skeleton, and on the Theorem 3 (ii) superset)
iff `t ≥ min(2j - 1, 2(N-k) + 1)`.  The draft's `N - 1 - (k - j)` is below this
threshold at `(j, k) = (2, 4)`. -/
theorem thm5_threshold_N5 :
    ∀ j ∈ [2, 3], ∀ k ∈ [3, 4], j < k → ∀ t ∈ List.range 7,
      skelDiverges 5 j k t = decide (min (2 * j - 1) (2 * (5 - k) + 1) ≤ t) ∧
      supDiverges 5 j k t = decide (min (2 * j - 1) (2 * (5 - k) + 1) ≤ t) := by
  decide

/-! ## Project question (not Wilson's): why the model is silent on association

Wilson's state is a single binary `S ∈ {L, H}` (p.7) and each memory state
carries one number, `π(i) = Pr(H | i)`.  Paper B's identifying statistic is an
association between two attributes.  The results below say what the model
fixes and what a two-attribute extension would have to add.  They are the
project's, not Wilson's; Wilson never discusses a second attribute. -/

/-- A second binary attribute `Y` attached through a kernel `κ_S = Pr(Y = 1 | S)`:
the covariance of `1{S = H}` and `Y` under a belief `π` on `S`. -/
def assocCov (π κH κL : ℝ) : ℝ := π * κH - π * (π * κH + (1 - π) * κL)

theorem assocCov_eq (π κH κL : ℝ) : assocCov π κH κL = π * (1 - π) * (κH - κL) := by
  unfold assocCov; ring

/-- **The sign of the association is the kernel's, in every memory state.**  For
any interior belief `π(i)`, the covariance is positive iff `κ_H > κ_L`: memory can
rescale the association by `π(1-π)` but never create, remove or reverse it. -/
theorem assoc_sign_fixed {π κH κL : ℝ} (h0 : 0 < π) (h1 : π < 1) :
    (0 < assocCov π κH κL ↔ κL < κH) ∧ (assocCov π κH κL = 0 ↔ κH = κL) := by
  rw [assocCov_eq]
  have hp : 0 < π * (1 - π) := mul_pos h0 (by linarith)
  constructor
  · constructor
    · intro h; by_contra hc; push Not at hc; nlinarith
    · intro h; nlinarith
  · constructor
    · intro h
      rcases mul_eq_zero.1 h with h' | h'
      · exact absurd h' hp.ne'
      · linarith
    · intro h; rw [h]; ring

/-- **The single-state belief does not determine the association.**  For every
belief `π ∈ (0,1)` about `S` and every marginal `m ∈ (0,1)` for `Y` there are two
kernels with the same marginals and associations of opposite sign.  Any
prediction of Wilson's model is a function of `π` (and `ρ, σ, g₀, a, η`), so it
cannot pin the association down. -/
theorem marginals_do_not_fix_association {π m : ℝ} (hπ0 : 0 < π) (hπ1 : π < 1)
    (hm0 : 0 < m) (hm1 : m < 1) :
    ∃ κH κL κH' κL' : ℝ,
      (0 ≤ κH ∧ κH ≤ 1 ∧ 0 ≤ κL ∧ κL ≤ 1 ∧ 0 ≤ κH' ∧ κH' ≤ 1 ∧ 0 ≤ κL' ∧ κL' ≤ 1) ∧
      π * κH + (1 - π) * κL = m ∧ π * κH' + (1 - π) * κL' = m ∧
      0 < assocCov π κH κL ∧ assocCov π κH' κL' < 0 := by
  set δ := min m (1 - m)
  have hδ : 0 < δ := lt_min hm0 (by linarith)
  have hδm : δ ≤ m := min_le_left _ _
  have hδ1 : δ ≤ 1 - m := min_le_right _ _
  refine ⟨m + δ * (1 - π), m - δ * π, m - δ * (1 - π), m + δ * π, ?_, by ring, by ring, ?_, ?_⟩
  · refine ⟨?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩ <;> nlinarith
  · rw [assocCov_eq]
    have : 0 < π * (1 - π) := mul_pos hπ0 (by linarith)
    have e : m + δ * (1 - π) - (m - δ * π) = δ := by ring
    rw [e]; positivity
  · rw [assocCov_eq]
    have : 0 < π * (1 - π) := mul_pos hπ0 (by linarith)
    have e : m - δ * (1 - π) - (m + δ * π) = -δ := by ring
    rw [e]; nlinarith

/-- **What an extension needs.**  Put the second attribute into the state, so the
belief is a joint law `q(S, Y)` on four atoms, and update it by any signal whose
likelihood `ℓ(S)` depends on `S` alone (Wilson's signals, `Pr(s | S)`).  The
posterior `q'(S, Y) ∝ ℓ(S) q(S, Y)` keeps every conditional `q(Y | S)`: the
kernel, hence (by `assoc_sign_fixed`) the sign of the association, never moves.
Changing the association needs signals whose likelihood depends on `Y` given
`S`, and memory states indexing beliefs on the joint (three free numbers per
state, not one). -/
theorem kernel_invariant (q : Bool → Bool → ℝ) (ℓ : Bool → ℝ) (S Y : Bool)
    (hℓ : ℓ S ≠ 0) (Z : ℝ) (hZ : Z ≠ 0) :
    let q' : Bool → Bool → ℝ := fun S Y => ℓ S * q S Y / Z
    q' S Y / (q' S true + q' S false) = q S Y / (q S true + q S false) := by
  intro q'
  simp only [q']
  by_cases h : q S true + q S false = 0
  · have : ℓ S * q S true / Z + ℓ S * q S false / Z = 0 := by
      rw [← add_div, ← mul_add, h, mul_zero, zero_div]
    rw [this, h, div_zero, div_zero]
  · have : ℓ S * q S true / Z + ℓ S * q S false / Z = ℓ S * (q S true + q S false) / Z := by
      rw [← add_div, ← mul_add]
    rw [this]
    field_simp

/-- ... whereas one signal whose likelihood depends on `Y` given `S` reverses the
association: from the independent-uniform joint, a signal with likelihood `2`
on `(H,1), (L,0)` and `1` elsewhere gives `Cov = +1/12 > 0`, and the mirror
signal `-1/12 < 0`. -/
theorem y_dependent_signal_moves_association :
    let cov : (Bool → Bool → ℝ) → ℝ := fun q =>
      q true true - (q true true + q true false) * (q true true + q false true)
    cov (fun S Y => (if S = Y then 2 else 1) / 6) = 1 / 12 ∧
      cov (fun S Y => (if S = Y then 1 else 2) / 6) = -1 / 12 := by
  intro cov
  simp only [cov]
  norm_num

end Literature.Wilson
