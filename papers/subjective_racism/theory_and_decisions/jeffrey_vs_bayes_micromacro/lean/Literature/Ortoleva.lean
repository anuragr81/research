/-
# Ortoleva (2012), "Modeling the Change of Paradigm: Non-Bayesian Reactions to Unexpected News"

*American Economic Review* 102(6), 2410-2436.

**Source caveat.** The primary is not available. This file formalizes the
restatement in Ortoleva (2024), "Alternatives to Bayesian Updating", *Annu. Rev.
Econ.* 16:545-570, Section 2.2.2 (pp. 549-550) and Section 4 (pp. 558-560). The
survey rewrites the 2012 model in terms of beliefs instead of preferences
(fn. 17, p. 558: "the mapping is routine"). Everything below follows the survey's
wording, not the AER paper's.

## Setup (finite `Ω`; the survey assumes finite `Ω`, p. 549)

* A belief is `p : Ω → ℝ`, nonnegative with total mass `1`. An event is a
  `Finset Ω`, and `prob p A = ∑_{ω ∈ A} p ω`.
* **Bayes' rule** `π^BU_A(ω) = π(ω)/π(A)` on `A` and `0` off it (`bayes`).
* An *act* is identified with its utility profile `f : Ω → ℝ` (`u ∘ f`), and
  `eu p f = ∑ p ω f ω`. `fAg` is `splice f A g`. Theorem 1's "only if" needs
  enough acts; using all of `ℝ^Ω` as utility profiles is the survey's setting
  with a utility onto an interval, after an affine rescaling.
* An *updating rule* is `U : Finset Ω → (Ω → ℝ)`, the posterior after each event.

## Axioms (belief-level forms of the survey's preference axioms)

* **Axiom 1, Consequentialism (p. 549).** `f = g` on `A` ⇒ `f ∼_A g`.
* **Axiom 2, Dynamic Consistency (p. 549).** If `π(A) > 0`, `fAg ≽ g ⇔ f ≽_A g`.
* **Axiom 3, Dynamic Coherence (p. 560).** If `π_{A_i}(A_{i+1}) = 1` for
  `i < n` and `π_{A_n}(A_1) = 1`, then `π_{A_1} = π_{A_n}`.

## The Hypothesis Testing (HT) model (p. 559)

A prior over priors `ρ`, the prior `π` its unique mode, a threshold
`ε ∈ [0,1)`. After `A`: if `π(A) > ε`, update `π` by Bayes. Otherwise update
`ρ` by Bayes, `ρ^BU_A(π̄) ∝ π̄(A) ρ(π̄)`, take its unique maximizer `π̄`, and
update `π̄` by Bayes. Here `ρ` has **finite support**: an indexed family
`P : ι → belief` with weights `ρ : ι → ℝ` (the survey allows any `ρ ∈ Δ(Δ(Ω))`).
Uniqueness of the maximizer is the survey's fn. 18. The field `full` asks that
some prior in the support gives `A` positive probability, for every nonempty
`A`. This keeps `ρ^BU_A` well defined (its denominator is positive). The survey
does not state this, but its formula needs it.

## What is proved

* `thm1_iff`: **Theorem 1 (p. 550; Ghirardato 2002)**. Given `π(A) > 0` and a
  posterior belief `q`, Consequentialism and Dynamic Consistency at `A` hold iff
  `q = π^BU_A`. Both directions.
* `ht_consequentialism`, `ht_prob_self`, `ht_dynamicCoherence`: **the "if"
  direction of Theorem 2 (p. 560)**. Every HT model satisfies Consequentialism
  and Dynamic Coherence (minimality is not needed).
* `ht_bayes_of_gt`: HT coincides with Bayes on every event with `π(A) > ε`
  (p. 560).
* `ht_eps_zero_bayes`, `ht_eps_zero_dynCons`: **the "Moreover", ε = 0 ⇒ DC.**
  With `ε = 0`, HT is Bayes on every positive-probability event and satisfies
  Dynamic Consistency.
* `ht_dynCons_same_as_eps_zero`, `ht_minimal_dynCons_eps_zero`: **the
  "Moreover", DC ⇒ ε = 0.** If an HT model satisfies Dynamic Consistency, the
  same `(π, ρ)` with `ε = 0` gives the same posteriors after every event. So a
  model that is minimal in `ε` (for fixed `(π, ρ)`) has `ε = 0`.
* `ex_*`: an explicit three-state, two-prior model with `ε = 1/20` in which
  "unexpected news" `A = {b, c}` (`π(A) = 1/25 ≤ ε`) triggers a change of prior:
  HT gives `(0, 1/4, 3/4)` where Bayes gives `(0, 3/4, 1/4)`. Likely news is
  still handled by Bayes. The model violates Dynamic Consistency, with explicit
  acts.

**Not formalized.** The "only if" direction of Theorem 2 (Consequentialism +
Dynamic Coherence ⇒ a minimal HT representation). It needs Ortoleva's (2012)
construction of `ρ` from the posteriors, which the survey does not reproduce.
-/
import Mathlib

namespace Literature.Ortoleva

open Finset

set_option linter.unusedSectionVars false

variable {Ω : Type*} [Fintype Ω] [DecidableEq Ω]

/-! ## Beliefs, Bayes' rule, expected utility -/

/-- Probability of an event. -/
def prob (p : Ω → ℝ) (A : Finset Ω) : ℝ := ∑ ω ∈ A, p ω

/-- A belief: nonnegative, total mass one. -/
def IsBelief (p : Ω → ℝ) : Prop := (∀ ω, 0 ≤ p ω) ∧ ∑ ω, p ω = 1

/-- **Bayes' rule** `π^BU_A` (p. 547-548): `π(ω)/π(A)` on `A`, `0` off `A`. -/
noncomputable def bayes (p : Ω → ℝ) (A : Finset Ω) : Ω → ℝ :=
  fun ω => if ω ∈ A then p ω / prob p A else 0

/-- Expected utility of the utility profile `f` under belief `p` (fn. 9, p. 549). -/
def eu (p : Ω → ℝ) (f : Ω → ℝ) : ℝ := ∑ ω, p ω * f ω

/-- The act `fAg`: `f` on `A`, `g` off `A` (p. 549). -/
def splice (f : Ω → ℝ) (A : Finset Ω) (g : Ω → ℝ) : Ω → ℝ :=
  fun ω => if ω ∈ A then f ω else g ω

/-- **Axiom 1 (Consequentialism)** at event `A` for posterior `q`. -/
def ConsAt (A : Finset Ω) (q : Ω → ℝ) : Prop :=
  ∀ f g : Ω → ℝ, (∀ ω ∈ A, f ω = g ω) → eu q f = eu q g

/-- **Axiom 2 (Dynamic Consistency)** at event `A`: `fAg ≽ g ⇔ f ≽_A g`. -/
def DynConsAt (π : Ω → ℝ) (A : Finset Ω) (q : Ω → ℝ) : Prop :=
  ∀ f g : Ω → ℝ, eu π g ≤ eu π (splice f A g) ↔ eu q g ≤ eu q f

/-- Consequentialism for an updating rule. -/
def Consequentialism (U : Finset Ω → Ω → ℝ) : Prop := ∀ A, ConsAt A (U A)

/-- Dynamic Consistency for an updating rule: at every `A` with `π(A) > 0`. -/
def DynamicConsistency (π : Ω → ℝ) (U : Finset Ω → Ω → ℝ) : Prop :=
  ∀ A, 0 < prob π A → DynConsAt π A (U A)

/-- **Axiom 3 (Dynamic Coherence)**, p. 560, for a cycle `A 0, …, A n`. -/
def DynamicCoherence (U : Finset Ω → Ω → ℝ) : Prop :=
  ∀ (n : ℕ) (A : Fin (n + 1) → Finset Ω),
    (∀ i : Fin n, prob (U (A i.castSucc)) (A i.succ) = 1) →
    prob (U (A (Fin.last n))) (A 0) = 1 → U (A 0) = U (A (Fin.last n))

/-! ### Elementary facts -/

theorem prob_nonneg {p : Ω → ℝ} (hp : ∀ ω, 0 ≤ p ω) (A : Finset Ω) : 0 ≤ prob p A :=
  sum_nonneg fun ω _ => hp ω

theorem prob_mono {p : Ω → ℝ} (hp : ∀ ω, 0 ≤ p ω) {A B : Finset Ω} (h : A ⊆ B) :
    prob p A ≤ prob p B :=
  sum_le_sum_of_subset_of_nonneg h fun ω _ _ => hp ω

theorem bayes_of_not_mem {p : Ω → ℝ} {A : Finset Ω} {ω : Ω} (h : ω ∉ A) :
    bayes p A ω = 0 := by simp [bayes, h]

/-- `π^BU_A(B) = π(B ∩ A)/π(A)`. -/
theorem prob_bayes (p : Ω → ℝ) (A B : Finset Ω) :
    prob (bayes p A) B = prob p (B ∩ A) / prob p A := by
  unfold prob bayes
  rw [sum_ite_mem, sum_div]
  rfl

theorem prob_bayes_self {p : Ω → ℝ} {A : Finset Ω} (h : 0 < prob p A) :
    prob (bayes p A) A = 1 := by
  rw [prob_bayes, inter_self, div_self h.ne']

/-- A Bayes posterior of a belief on an event of positive probability is a belief. -/
theorem bayes_isBelief {p : Ω → ℝ} (hp : ∀ ω, 0 ≤ p ω) {A : Finset Ω} (h : 0 < prob p A) :
    IsBelief (bayes p A) := by
  refine ⟨fun ω => ?_, ?_⟩
  · unfold bayes; split_ifs
    · exact div_nonneg (hp ω) h.le
    · exact le_refl 0
  · have := prob_bayes_self h
    unfold prob at this
    rw [← this]
    unfold bayes
    rw [sum_ite_mem, univ_inter, sum_ite_mem, inter_self]

/-- Every Bayes posterior satisfies Consequentialism, whatever it conditions. -/
theorem bayes_consAt (p : Ω → ℝ) (A : Finset Ω) : ConsAt A (bayes p A) := by
  intro f g hfg
  unfold eu
  refine sum_congr rfl fun ω _ => ?_
  by_cases hω : ω ∈ A
  · rw [hfg ω hω]
  · simp [bayes_of_not_mem hω]

/-- The identity behind Theorem 1:
`EU_π(fAg) - EU_π(g) = π(A) (EU_{π_A}(f) - EU_{π_A}(g))`. -/
theorem eu_splice_sub {π : Ω → ℝ} {A : Finset Ω} (hA : 0 < prob π A) (f g : Ω → ℝ) :
    eu π (splice f A g) - eu π g = prob π A * (eu (bayes π A) f - eu (bayes π A) g) := by
  unfold eu splice bayes
  rw [← sum_sub_distrib, ← sum_sub_distrib, mul_sum]
  refine sum_congr rfl fun ω _ => ?_
  split_ifs
  · field_simp
  · ring

/-! ## Theorem 1 (Bayes' rule from Consequentialism + Dynamic Consistency) -/

/-- Point-mass utility profile. -/
def pt (ω : Ω) (c : ℝ) : Ω → ℝ := fun x => if x = ω then c else 0

theorem eu_pt (p : Ω → ℝ) (ω : Ω) (c : ℝ) : eu p (pt ω c) = p ω * c := by
  simp [eu, pt, mul_ite]

/-- **Theorem 1, "if" (p. 550).** Bayes' rule satisfies Consequentialism and
Dynamic Consistency at every event of positive probability. -/
theorem thm1_if {π : Ω → ℝ} {A : Finset Ω} (hA : 0 < prob π A) :
    ConsAt A (bayes π A) ∧ DynConsAt π A (bayes π A) := by
  refine ⟨bayes_consAt π A, fun f g => ?_⟩
  have h := eu_splice_sub hA f g
  constructor
  · intro hle
    by_contra hc
    push Not at hc
    have : prob π A * (eu (bayes π A) f - eu (bayes π A) g) < 0 :=
      mul_neg_of_pos_of_neg hA (by linarith)
    linarith
  · intro hle
    have : 0 ≤ prob π A * (eu (bayes π A) f - eu (bayes π A) g) :=
      mul_nonneg hA.le (by linarith)
    linarith

/-- **Theorem 1, "only if" (p. 550).** A posterior belief `q` that satisfies
Consequentialism and (the forward half of) Dynamic Consistency at an event with
`π(A) > 0` is the Bayes update. -/
theorem thm1_only_if {π q : Ω → ℝ} (_hπ : IsBelief π) (hq : IsBelief q) {A : Finset Ω}
    (hA : 0 < prob π A) (hC : ConsAt A q)
    (hD : ∀ f g : Ω → ℝ, eu π g ≤ eu π (splice f A g) → eu q g ≤ eu q f) :
    q = bayes π A := by
  -- off `A`, `q` vanishes (Consequentialism with a point act)
  have hoff : ∀ ω, ω ∉ A → q ω = 0 := by
    intro ω hω
    have := hC (pt ω 1) (fun _ => 0) (fun x hx => by
      have : x ≠ ω := fun h => hω (h ▸ hx)
      simp [pt, this])
    rw [eu_pt] at this
    simpa [eu] using this
  -- on `A`, relative likelihoods are preserved
  have hrel : ∀ ω ∈ A, ∀ ω' ∈ A, q ω * π ω' ≤ q ω' * π ω := by
    intro ω hω ω' hω'
    have hs : splice (pt ω' (π ω)) A (pt ω (π ω')) = pt ω' (π ω) := by
      funext x
      by_cases hx : x ∈ A
      · simp [splice, hx]
      · have h1 : x ≠ ω := fun h => hx (h ▸ hω)
        have h2 : x ≠ ω' := fun h => hx (h ▸ hω')
        simp [splice, pt, hx, h1, h2]
    have := hD (pt ω' (π ω)) (pt ω (π ω')) (by
      rw [hs, eu_pt, eu_pt]; linarith [mul_comm (π ω) (π ω')])
    rw [eu_pt, eu_pt] at this
    exact this
  have hsumA : ∑ ω' ∈ A, q ω' = 1 := by
    rw [← hq.2, ← sum_subset (subset_univ A) (fun x _ hx => hoff x hx)]
  funext ω
  by_cases hω : ω ∈ A
  · have e1 : q ω * prob π A = ∑ ω' ∈ A, q ω * π ω' := by unfold prob; rw [mul_sum]
    have e2 : π ω = ∑ ω' ∈ A, q ω' * π ω := by rw [← sum_mul, hsumA, one_mul]
    have key : q ω * prob π A = π ω := by
      rw [e1]
      conv_rhs => rw [e2]
      exact le_antisymm (sum_le_sum fun ω' h' => hrel ω hω ω' h')
        (sum_le_sum fun ω' h' => hrel ω' h' ω hω)
    simp only [bayes, hω, if_true]
    rw [eq_div_iff hA.ne']
    exact key
  · rw [hoff ω hω, bayes_of_not_mem hω]

/-- **Theorem 1 (p. 550), both directions.** -/
theorem thm1_iff {π q : Ω → ℝ} (hπ : IsBelief π) (hq : IsBelief q) {A : Finset Ω}
    (hA : 0 < prob π A) :
    (ConsAt A q ∧ DynConsAt π A q) ↔ q = bayes π A := by
  constructor
  · rintro ⟨hC, hD⟩
    exact thm1_only_if hπ hq hA hC fun f g h => (hD f g).mp h
  · rintro rfl; exact thm1_if hA

/-! ## The Hypothesis Testing model (Section 4.1, pp. 558-559) -/

/-- Bayes update of the prior over priors: `ρ^BU_A(i) = P_i(A) ρ(i) / Σ_j P_j(A) ρ(j)`. -/
noncomputable def rhoBU {ι : Type*} [Fintype ι] (P : ι → Ω → ℝ) (ρ : ι → ℝ)
    (A : Finset Ω) (i : ι) : ℝ :=
  prob (P i) A * ρ i / ∑ j, prob (P j) A * ρ j

/-- An HT model `(π, ρ, ε)` with finitely supported `ρ`. `P i₀` is `π`; `sel A`
is `π̄`, the unique maximizer of `ρ^BU_A`. -/
structure HT (Ω ι : Type*) [Fintype Ω] [DecidableEq Ω] [Fintype ι] where
  P : ι → Ω → ℝ
  ρ : ι → ℝ
  i₀ : ι
  ε : ℝ
  sel : Finset Ω → ι
  P_belief : ∀ i, IsBelief (P i)
  ρ_nonneg : ∀ i, 0 ≤ ρ i
  ρ_sum : ∑ i, ρ i = 1
  /-- `{π} = argmax ρ` (p. 559). -/
  i₀_max : ∀ i, ρ i ≤ ρ i₀
  i₀_unique : ∀ i, ρ i = ρ i₀ → i = i₀
  ε_nonneg : 0 ≤ ε
  ε_lt_one : ε < 1
  /-- `{π̄} = argmax ρ^BU_A` (p. 559 and fn. 18). -/
  sel_max : ∀ A i, rhoBU P ρ A i ≤ rhoBU P ρ A (sel A)
  sel_unique : ∀ A : Finset Ω, A.Nonempty → ∀ i, rhoBU P ρ A i = rhoBU P ρ A (sel A) → i = sel A
  /-- `ρ^BU_A` is well defined on every nonempty event. -/
  full : ∀ A : Finset Ω, A.Nonempty → ∃ i, 0 < prob (P i) A * ρ i

namespace HT

variable {ι : Type*} [Fintype ι] (M : HT Ω ι)

/-- The prior `π`. -/
def π : Ω → ℝ := M.P M.i₀

/-- **The HT updating rule** (display on p. 559):
`π_A = π^BU_A` if `π(A) > ε`, and `π̄^BU_A` otherwise. -/
noncomputable def post (A : Finset Ω) : Ω → ℝ :=
  if M.ε < prob M.π A then bayes M.π A else bayes (M.P (M.sel A)) A

/-- The score `π'(A) ρ(π')` whose maximizer is `π̄`. -/
def score (A : Finset Ω) (i : ι) : ℝ := prob (M.P i) A * M.ρ i

theorem score_nonneg (A : Finset Ω) (i : ι) : 0 ≤ M.score A i :=
  mul_nonneg (prob_nonneg (M.P_belief i).1 A) (M.ρ_nonneg i)

theorem denom_pos {A : Finset Ω} (hA : A.Nonempty) : 0 < ∑ j, M.score A j := by
  obtain ⟨i, hi⟩ := M.full A hA
  exact lt_of_lt_of_le hi (single_le_sum (fun j _ => M.score_nonneg A j) (mem_univ i))

theorem rhoBU_le_iff {A : Finset Ω} (hA : A.Nonempty) (i j : ι) :
    rhoBU M.P M.ρ A i ≤ rhoBU M.P M.ρ A j ↔ M.score A i ≤ M.score A j :=
  div_le_div_iff_of_pos_right (M.denom_pos hA)

theorem score_le_sel {A : Finset Ω} (hA : A.Nonempty) (i : ι) :
    M.score A i ≤ M.score A (M.sel A) :=
  (M.rhoBU_le_iff hA i _).mp (M.sel_max A i)

theorem sel_unique_score {A : Finset Ω} (hA : A.Nonempty) (i : ι)
    (h : M.score A (M.sel A) ≤ M.score A i) : i = M.sel A := by
  refine M.sel_unique A hA i (le_antisymm (M.sel_max A i) ((M.rhoBU_le_iff hA _ _).mpr h))

theorem score_sel_pos {A : Finset Ω} (hA : A.Nonempty) : 0 < M.score A (M.sel A) := by
  obtain ⟨i, hi⟩ := M.full A hA
  exact lt_of_lt_of_le hi (M.score_le_sel hA i)

theorem prob_sel_pos {A : Finset Ω} (hA : A.Nonempty) : 0 < prob (M.P (M.sel A)) A :=
  pos_of_mul_pos_left (M.score_sel_pos hA) (M.ρ_nonneg _)

theorem post_of_gt {A : Finset Ω} (h : M.ε < prob M.π A) : M.post A = bayes M.π A := by
  simp [post, h]

theorem post_of_le {A : Finset Ω} (h : prob M.π A ≤ M.ε) :
    M.post A = bayes (M.P (M.sel A)) A := by
  simp [post, not_lt.mpr h]

/-- **p. 560:** "the model's predictions coincide with Bayes' rule for all events
with a likelihood above ε". -/
theorem ht_bayes_of_gt {A : Finset Ω} (h : M.ε < prob M.π A) : M.post A = bayes M.π A :=
  M.post_of_gt h

/-- **Theorem 2, "if": Consequentialism.** -/
theorem ht_consequentialism : Consequentialism M.post := by
  intro A
  unfold post; split_ifs
  · exact bayes_consAt _ A
  · exact bayes_consAt _ A

/-- Belief form of Consequentialism: after a nonempty `A`, `π_A(A) = 1`. -/
theorem ht_prob_self {A : Finset Ω} (hA : A.Nonempty) : prob (M.post A) A = 1 := by
  by_cases h : M.ε < prob M.π A
  · rw [M.post_of_gt h]; exact prob_bayes_self (lt_of_le_of_lt M.ε_nonneg h)
  · rw [M.post_of_le (not_lt.mp h)]; exact prob_bayes_self (M.prob_sel_pos hA)

theorem post_isBelief {A : Finset Ω} (hA : A.Nonempty) : IsBelief (M.post A) := by
  by_cases h : M.ε < prob M.π A
  · rw [M.post_of_gt h]
    exact bayes_isBelief (M.P_belief _).1 (lt_of_le_of_lt M.ε_nonneg h)
  · rw [M.post_of_le (not_lt.mp h)]
    exact bayes_isBelief (M.P_belief _).1 (M.prob_sel_pos hA)

end HT

/-! ### Dynamic Coherence: the cycle argument -/

/-- A value that weakly rises along every edge of a cycle is constant on it. -/
theorem cycle_const {n : ℕ} (f : Fin (n + 1) → ℝ) (h : ∀ i : Fin n, f i.castSucc ≤ f i.succ)
    (hc : f (Fin.last n) ≤ f 0) : ∀ i, f i = f 0 := by
  have up : ∀ i, f 0 ≤ f i := by
    intro i
    induction i using Fin.induction with
    | zero => exact le_rfl
    | succ i ih => exact ih.trans (h i)
  have down : ∀ i, f i ≤ f (Fin.last n) := by
    intro i
    induction i using Fin.reverseInduction with
    | last => exact le_rfl
    | cast i ih => exact (h i).trans ih
  intro i
  exact le_antisymm ((down i).trans hc) (up i)

/-- If Bayes on `A` makes `B` certain, every state of `A` that `q` charges is in `B`. -/
theorem edge_support {q : Ω → ℝ} (hq : ∀ ω, 0 ≤ q ω) {A B : Finset Ω} (hA : 0 < prob q A)
    (h : prob (bayes q A) B = 1) : ∀ ω ∈ A, q ω ≠ 0 → ω ∈ B := by
  rw [prob_bayes, div_eq_one_iff_eq hA.ne'] at h
  have hsplit := sum_inter_add_sum_sdiff A B q
  have hdiff : ∑ ω ∈ A \ B, q ω = 0 := by
    unfold prob at h; rw [inter_comm] at h; linarith
  intro ω hω hne
  by_contra hB
  exact hne ((sum_eq_zero_iff_of_nonneg fun x _ => hq x).mp hdiff ω (mem_sdiff.mpr ⟨hω, hB⟩))

/-- …and hence `q(A) ≤ q(B)`. -/
theorem edge_prob_le {q : Ω → ℝ} (hq : ∀ ω, 0 ≤ q ω) {A B : Finset Ω} (hA : 0 < prob q A)
    (h : prob (bayes q A) B = 1) : prob q A ≤ prob q B := by
  have hsub : A.filter (fun ω => q ω ≠ 0) ⊆ B := fun ω hω => by
    rw [mem_filter] at hω; exact edge_support hq hA h ω hω.1 hω.2
  unfold prob
  rw [← sum_filter_ne_zero]
  exact sum_le_sum_of_subset_of_nonneg hsub fun ω _ _ => hq ω

/-- **Bayes with a fixed prior satisfies Dynamic Coherence** on cycles of events
of positive probability. -/
theorem bayes_cycle {q : Ω → ℝ} (hq : ∀ ω, 0 ≤ q ω) {n : ℕ} (A : Fin (n + 1) → Finset Ω)
    (hpos : ∀ i, 0 < prob q (A i))
    (hedge : ∀ i : Fin n, prob (bayes q (A i.castSucc)) (A i.succ) = 1)
    (hclose : prob (bayes q (A (Fin.last n))) (A 0) = 1) :
    bayes q (A 0) = bayes q (A (Fin.last n)) := by
  have fwd : ∀ i, ∀ ω ∈ A 0, q ω ≠ 0 → ω ∈ A i := by
    intro i
    induction i using Fin.induction with
    | zero => exact fun ω h _ => h
    | succ i ih => exact fun ω h hne =>
        edge_support hq (hpos _) (hedge i) ω (ih ω h hne) hne
  have back : ∀ ω ∈ A (Fin.last n), q ω ≠ 0 → ω ∈ A 0 :=
    edge_support hq (hpos _) hclose
  have hprob : prob q (A 0) = prob q (A (Fin.last n)) :=
    le_antisymm (edge_prob_le hq (hpos 0) (by
      -- `A 0 → A last` along the chain: use `fwd`
      rw [prob_bayes, div_eq_one_iff_eq (hpos 0).ne']
      unfold prob
      rw [← sum_filter_ne_zero, ← sum_filter_ne_zero (s := A 0)]
      congr 1; ext ω
      simp only [mem_filter, mem_inter]
      exact ⟨fun h => ⟨h.1.2, h.2⟩, fun h => ⟨⟨fwd _ ω h.1 h.2, h.1⟩, h.2⟩⟩))
      (edge_prob_le hq (hpos _) hclose)
  funext ω
  unfold bayes
  by_cases h0 : q ω = 0
  · simp [h0]
  · by_cases hω : ω ∈ A 0
    · rw [if_pos hω, if_pos (fwd _ ω hω h0), hprob]
    · have : ω ∉ A (Fin.last n) := fun h => hω (back ω h h0)
      rw [if_neg hω, if_neg this]

namespace HT

variable {ι : Type*} [Fintype ι] (M : HT Ω ι)

theorem nonempty_of_edge {A B : Finset Ω} (h : prob (M.post A) B = 1) : A.Nonempty := by
  by_contra hA
  rw [not_nonempty_iff_eq_empty] at hA
  subst hA
  have : M.post ∅ = fun _ => 0 := by
    funext ω; unfold post; split_ifs <;> simp [bayes]
  rw [this] at h; simp [prob] at h

/-- An edge out of a "likely" event lands on a likely event. -/
theorem edge_high {A B : Finset Ω} (hA : M.ε < prob M.π A) (h : prob (M.post A) B = 1) :
    M.ε < prob M.π B := by
  rw [M.post_of_gt hA] at h
  exact lt_of_lt_of_le hA
    (edge_prob_le (M.P_belief _).1 (lt_of_le_of_lt M.ε_nonneg hA) h)

/-- An edge out of an "unlikely" event does not lower the top score. -/
theorem edge_low {A B : Finset Ω} (hA : prob M.π A ≤ M.ε) (hBne : B.Nonempty)
    (h : prob (M.post A) B = 1) :
    M.score A (M.sel A) ≤ M.score B (M.sel B) := by
  have hAne := M.nonempty_of_edge h
  rw [M.post_of_le hA] at h
  have hle := edge_prob_le (M.P_belief _).1 (M.prob_sel_pos hAne) h
  calc M.score A (M.sel A) ≤ M.score B (M.sel A) :=
        mul_le_mul_of_nonneg_right hle (M.ρ_nonneg _)
    _ ≤ M.score B (M.sel B) := M.score_le_sel hBne _

/-- **Theorem 2, "if": Dynamic Coherence (p. 560).** Every HT model satisfies
Axiom 3. Along a cycle, either every event is "likely" (`π(A_i) > ε`), and
then all posteriors are Bayes updates of `π`, or every event is unlikely; then
the top score `max_π' π'(A_i) ρ(π')` is constant around the cycle, so the
selected prior `π̄` is the same for every `A_i` (by uniqueness), and all
posteriors are Bayes updates of that one `π̄`. Either way Bayes with a fixed
prior closes the cycle. -/
theorem ht_dynamicCoherence : DynamicCoherence M.post := by
  intro n A hedge hclose
  have hne : ∀ i, (A i).Nonempty := by
    intro i
    induction i using Fin.reverseInduction with
    | last => exact M.nonempty_of_edge hclose
    | cast i _ => exact M.nonempty_of_edge (hedge i)
  -- all likely or all unlikely
  let h : Fin (n + 1) → ℝ := fun i => if M.ε < prob M.π (A i) then 1 else 0
  have hh : ∀ i, h i = h 0 := by
    refine cycle_const h (fun i => ?_) ?_
    · simp only [h]
      split_ifs with h1 h2 <;> try norm_num
      exact h2 (M.edge_high h1 (hedge i))
    · simp only [h]
      split_ifs with h1 h2 <;> try norm_num
      exact h2 (M.edge_high h1 hclose)
  by_cases h0 : M.ε < prob M.π (A 0)
  · have hall : ∀ i, M.ε < prob M.π (A i) := by
      intro i
      have := hh i
      simp only [h, if_pos h0] at this
      by_contra hc; rw [if_neg hc] at this; norm_num at this
    have hpost : ∀ i, M.post (A i) = bayes M.π (A i) := fun i => M.post_of_gt (hall i)
    rw [hpost, hpost]
    refine bayes_cycle (M.P_belief _).1 A
      (fun i => lt_of_le_of_lt M.ε_nonneg (hall i)) (fun i => ?_) ?_
    · have := hedge i; rw [hpost] at this; exact this
    · have := hclose; rw [hpost] at this; exact this
  · have hall : ∀ i, prob M.π (A i) ≤ M.ε := by
      intro i
      have := hh i
      simp only [h, if_neg h0] at this
      by_contra hc; rw [not_le] at hc; rw [if_pos hc] at this; norm_num at this
    -- the top score is constant around the cycle
    let m : Fin (n + 1) → ℝ := fun i => M.score (A i) (M.sel (A i))
    have hm : ∀ i, m i = m 0 :=
      cycle_const m (fun i => M.edge_low (hall _) (hne _) (hedge i))
        (M.edge_low (hall _) (hne _) hclose)
    -- hence the selected prior is constant along edges
    have hsel_step : ∀ i : Fin n, M.sel (A i.succ) = M.sel (A i.castSucc) := by
      intro i
      have hAne := hne i.castSucc
      have e := hedge i
      rw [M.post_of_le (hall _)] at e
      have hle := edge_prob_le (M.P_belief _).1 (M.prob_sel_pos hAne) e
      refine (M.sel_unique_score (hne i.succ) _ ?_).symm
      calc M.score (A i.succ) (M.sel (A i.succ)) = m i.succ := rfl
        _ = m i.castSucc := by rw [hm i.succ, hm i.castSucc]
        _ = M.score (A i.castSucc) (M.sel (A i.castSucc)) := rfl
        _ ≤ M.score (A i.succ) (M.sel (A i.castSucc)) :=
          mul_le_mul_of_nonneg_right hle (M.ρ_nonneg _)
    have hsel : ∀ i, M.sel (A i) = M.sel (A 0) := by
      intro i
      induction i using Fin.induction with
      | zero => rfl
      | succ i ih => rw [hsel_step i, ih]
    have hpost : ∀ i, M.post (A i) = bayes (M.P (M.sel (A 0))) (A i) := by
      intro i; rw [M.post_of_le (hall i), hsel i]
    rw [hpost, hpost]
    refine bayes_cycle (M.P_belief _).1 A (fun i => ?_) (fun i => ?_) ?_
    · rw [← hsel i]; exact M.prob_sel_pos (hne i)
    · have := hedge i; rw [hpost] at this; exact this
    · have := hclose; rw [hpost] at this; exact this

/-! ### The "Moreover" of Theorem 2: `ε = 0` iff Dynamic Consistency -/

/-- The same `(π, ρ)` with another threshold `e ∈ [0, 1)`. -/
def withEps (e : ℝ) (he0 : 0 ≤ e) (he1 : e < 1) : HT Ω ι :=
  { M with ε := e, ε_nonneg := he0, ε_lt_one := he1 }

/-- With `ε = 0`, HT is Bayes on every positive-probability event (p. 560: "behavior
coincides with Bayes' rule whenever defined"). -/
theorem ht_eps_zero_bayes (h0 : M.ε = 0) {A : Finset Ω} (hA : 0 < prob M.π A) :
    M.post A = bayes M.π A :=
  M.post_of_gt (h0 ▸ hA)

/-- **Theorem 2, "Moreover", `ε = 0 ⇒` Dynamic Consistency.** -/
theorem ht_eps_zero_dynCons (h0 : M.ε = 0) : DynamicConsistency M.π M.post := by
  intro A hA
  rw [M.ht_eps_zero_bayes h0 hA]
  exact (thm1_if hA).2

/-- **Theorem 2, "Moreover", Dynamic Consistency `⇒ ε = 0`, in substance.** If an
HT model satisfies Dynamic Consistency, then setting `ε = 0` (same `π`, `ρ`)
changes no posterior at all. -/
theorem ht_dynCons_same_as_eps_zero (hD : DynamicConsistency M.π M.post) (A : Finset Ω) :
    (M.withEps 0 le_rfl zero_lt_one).post A = M.post A := by
  by_cases hA : 0 < prob M.π A
  · have hAne : A.Nonempty := by
      by_contra hc; rw [not_nonempty_iff_eq_empty] at hc; subst hc; simp [prob] at hA
    have h1 : M.post A = bayes M.π A :=
      (thm1_iff (M.P_belief _) (M.post_isBelief hAne) hA).mp
        ⟨M.ht_consequentialism A, hD A hA⟩
    rw [h1]
    exact (M.withEps 0 le_rfl zero_lt_one).post_of_gt hA
  · have hA0 : prob M.π A ≤ 0 := not_lt.mp hA
    rw [M.post_of_le (hA0.trans M.ε_nonneg)]
    exact (M.withEps 0 le_rfl zero_lt_one).post_of_le hA0

/-- Minimality in `ε` (p. 560: "no strictly smaller ε represents the same
behavior"), here for fixed `(π, ρ)`. -/
def IsMinimal : Prop :=
  ∀ e (he0 : 0 ≤ e) (he : e < M.ε), ∃ A, (M.withEps e he0 (he.trans M.ε_lt_one)).post A ≠ M.post A

/-- **Theorem 2, "Moreover", Dynamic Consistency `⇒ ε = 0`** for a minimal model. -/
theorem ht_minimal_dynCons_eps_zero (hmin : M.IsMinimal)
    (hD : DynamicConsistency M.π M.post) : M.ε = 0 := by
  by_contra hne
  have hpos : 0 < M.ε := lt_of_le_of_ne M.ε_nonneg (Ne.symm hne)
  obtain ⟨A, hA⟩ := hmin 0 le_rfl hpos
  exact hA (M.ht_dynCons_same_as_eps_zero hD A)

end HT

/-! ## An explicit example: a change of paradigm on unexpected news

States `a, b, c` = `0, 1, 2`. Two candidate priors: `π = (24/25, 3/100, 1/100)`
with weight `9/10` and `π' = (1/5, 1/5, 3/5)` with weight `1/10`. Threshold
`ε = 1/20`. -/

/-- The two candidate priors. -/
noncomputable def exP : Fin 2 → Fin 3 → ℝ := ![![24/25, 3/100, 1/100], ![1/5, 1/5, 3/5]]

/-- The prior over priors. -/
noncomputable def exρ : Fin 2 → ℝ := ![9/10, 1/10]

/-- The maximizer of `ρ^BU_A`: the second prior iff its score is higher. -/
noncomputable def exSel (A : Finset (Fin 3)) : Fin 2 :=
  if prob (exP 0) A * exρ 0 < prob (exP 1) A * exρ 1 then 1 else 0

theorem prob_eq_sum_ite (p : Ω → ℝ) (A : Finset Ω) :
    prob p A = ∑ ω, if ω ∈ A then p ω else 0 := by
  unfold prob; rw [sum_ite_mem, univ_inter]

theorem exP_nonneg (i : Fin 2) (ω : Fin 3) : 0 ≤ exP i ω := by
  fin_cases i <;> fin_cases ω <;> simp [exP] <;> norm_num

theorem exρ_pos (i : Fin 2) : 0 < exρ i := by
  fin_cases i <;> simp [exρ]

/-- The two scores `π'(A) ρ(π')` never tie on a nonempty event, so `ρ^BU_A`
has a unique maximizer (fn. 18). -/
theorem ex_no_tie {A : Finset (Fin 3)} (hA : A.Nonempty) :
    prob (exP 0) A * exρ 0 ≠ prob (exP 1) A * exρ 1 := by
  rw [prob_eq_sum_ite, prob_eq_sum_ite, Fin.sum_univ_three, Fin.sum_univ_three]
  by_cases h0 : (0 : Fin 3) ∈ A <;> by_cases h1 : (1 : Fin 3) ∈ A <;>
    by_cases h2 : (2 : Fin 3) ∈ A <;> simp [h0, h1, h2, exP, exρ] <;> norm_num
  obtain ⟨x, hx⟩ := hA
  fin_cases x <;> simp_all

/-- The example as an HT model: `π = (24/25, 3/100, 1/100)`, `ρ(π) = 9/10`,
`π' = (1/5, 1/5, 3/5)`, `ρ(π') = 1/10`, `ε = 1/20`. -/
noncomputable def exM : HT (Fin 3) (Fin 2) where
  P := exP
  ρ := exρ
  i₀ := 0
  ε := 1 / 20
  sel := exSel
  P_belief := fun i => ⟨exP_nonneg i, by
    fin_cases i <;> simp [exP, Fin.sum_univ_three] <;> norm_num⟩
  ρ_nonneg := fun i => (exρ_pos i).le
  ρ_sum := by simp [exρ, Fin.sum_univ_two]; norm_num
  i₀_max := by intro i; fin_cases i <;> (simp [exρ]; try norm_num)
  i₀_unique := by intro i; fin_cases i <;> (simp [exρ]; try norm_num)
  ε_nonneg := by norm_num
  ε_lt_one := by norm_num
  sel_max := by
    intro A i
    unfold rhoBU
    refine div_le_div_of_nonneg_right ?_
      (sum_nonneg fun j _ => mul_nonneg (prob_nonneg (exP_nonneg j) A) (exρ_pos j).le)
    unfold exSel
    split_ifs with h <;> fin_cases i <;> simp <;> linarith
  sel_unique := by
    intro A hA i h
    have hZ : 0 < ∑ j, prob (exP j) A * exρ j := by
      refine lt_of_lt_of_le ?_ (single_le_sum (f := fun j => prob (exP j) A * exρ j)
        (fun j _ => mul_nonneg (prob_nonneg (exP_nonneg j) A) (exρ_pos j).le) (mem_univ 1))
      refine mul_pos ?_ (exρ_pos 1)
      exact sum_pos (fun ω _ => by fin_cases ω <;> simp [exP]) hA
    unfold rhoBU at h
    rw [div_left_inj' hZ.ne'] at h
    have ht := ex_no_tie hA
    unfold exSel at h ⊢
    split_ifs at h ⊢ with hs <;> fin_cases i <;> simp_all
  full := by
    intro A hA
    refine ⟨1, mul_pos ?_ (exρ_pos 1)⟩
    exact sum_pos (fun ω _ => by fin_cases ω <;> simp [exP]) hA

/-- The unexpected news `A = {b, c}`. -/
def exA : Finset (Fin 3) := {1, 2}

theorem ex_prob_A : prob exM.π exA = 1 / 25 := by
  simp [prob, exA, exM, HT.π, exP]; norm_num

/-- `A` is unexpected: `π(A) = 1/25 ≤ ε = 1/20`, so the prior is questioned. -/
theorem ex_unexpected : prob exM.π exA ≤ exM.ε := by
  rw [ex_prob_A]; simp [exM]; norm_num

/-- `ρ^BU_A` now favours `π'`: `π(A) ρ(π) = 9/250 < 2/25 = π'(A) ρ(π')`. -/
theorem ex_sel_A : exM.sel exA = 1 := by
  simp [exM, exSel, prob, exA, exP, exρ]; norm_num

/-- Bayes' rule would give `(0, 3/4, 1/4)`. -/
theorem ex_bayes_A : bayes exM.π exA = ![0, 3 / 4, 1 / 4] := by
  funext ω
  fin_cases ω <;> simp [bayes, prob, exA, exM, HT.π, exP] <;> norm_num

/-- **HT gives `(0, 1/4, 3/4)`**: a change of paradigm on unexpected news. -/
theorem ex_ht_A : exM.post exA = ![0, 1 / 4, 3 / 4] := by
  rw [exM.post_of_le ex_unexpected, ex_sel_A]
  funext ω
  fin_cases ω <;> simp [bayes, prob, exA, exM, exP] <;> norm_num

/-- **HT departs from Bayes on unlikely news.** -/
theorem ex_departs : exM.post exA ≠ bayes exM.π exA := by
  rw [ex_ht_A, ex_bayes_A]
  intro h
  have := congrFun h 1
  simp at this; norm_num at this

/-- On likely news (`π({a, b}) = 99/100 > ε`) the same model is Bayesian. -/
theorem ex_likely : exM.post {0, 1} = bayes exM.π {0, 1} := by
  apply exM.post_of_gt
  simp [prob, exM, HT.π, exP]; norm_num

/-- **The example violates Dynamic Consistency (so, by Theorem 2, it has no
representation with `ε = 0`).** With `f` paying `1` on `b` and `g` paying `1`
on `c`: before the news `fAg ≻ g` (`3/100` vs `1/100`), after it `g ≻_A f`
(`3/4` vs `1/4`). -/
theorem ex_not_dynCons : ¬ DynamicConsistency exM.π exM.post := by
  intro hD
  have hA : 0 < prob exM.π exA := by rw [ex_prob_A]; norm_num
  have hs : splice (pt 1 1) exA (pt 2 1) = pt (1 : Fin 3) 1 := by
    funext x; fin_cases x <;> simp [splice, pt, exA]
  have := (hD exA hA (pt 1 1) (pt 2 1)).mp (by
    rw [hs, eu_pt, eu_pt]; simp [exM, HT.π, exP]; norm_num)
  rw [eu_pt, eu_pt, ex_ht_A] at this
  simp at this; norm_num at this

end Literature.Ortoleva
