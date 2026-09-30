/-
# Cassell, "Commutativity, Normativity, and Holism: Lange Revisited"

*Canadian Journal of Philosophy* 50(2), 159-173; published online 2019
(© 2019), volume year 2020 (the paper's own citation line: "Cassell, L. 2020"),
doi:10.1017/can.2019.17.  Page numbers are the journal's.

Formalization of the mathematical content of the paper's reconstruction of
Lange (2000) and of Cassell's reply, not of Paper B's claims.  Lange (2000) is
not in the paper set; every Lange claim here is as Cassell states it.

## The formal core

* **Jeffrey Conditionalization** (p.160): `p'(A) = ∑ᵢ p(A | Eᵢ) p'(Eᵢ)` on a
  partition `{Eᵢ}`; `jc` on a finite space with the partition given by
  `e : Ω → ι`.  It realizes its input (`jc_cell`) and is rigid (`jc_rigid`).
* **Non-commutativity over weighted evidence partitions** (pp.160-161): two
  updates on the same partition end at the later input (`jc_twice`); the
  raven example (`.9` then `.7` ends at `.7`, reversed at `.9`; `raven`).
* **Lange's argument as Cassell reconstructs it** (pp.161-163): under the
  Bayes-factor reading of "the impact of an experience" (p.165),
  the same posterior from different priors means different Bayes factors
  (`different_priors_different_bf`); Figure 1 (`.99 → .8 → .75` vs
  `.99 → .75 → .8`) and Figure 2 (`.1 → .3 → .7` vs `.1 → .7 → .3`) have
  `ξ₁ ≠ ξ₄`, `ξ₂ ≠ ξ₃`, with the confirming/disconfirming directions the text
  describes (`figure1`, `figure2`, `lange_quote`); and, in general, reversing
  the *input values* reverses the *Bayes factors* only when nothing moves
  (`reversed_inputs_reversed_bf_iff`), and reversing the factors reverses the
  input values only when nothing moves (`reversed_bf_reversed_inputs_iff`).
  Weisberg's jellybean numbers are one instance (`jellybean_weisberg`).
* **Bayes factors commute** (p.165, fn 5): Field's rule recovers its factor
  (`bf_oddsUpdate`) and updates by factors commute, on one partition
  (`oddsUpdate_comm`) and on any two partitions (`bfUpdate_comm`, the direction
  of the fn 5 biconditional credited to Field).  On a single partition the
  converse direction fails (`same_partition_converse_fails`): fn 5's "just in
  case" needs Wagner's (2002) conditions, which a partition fails with itself.
* **ECJC** (p.167) as a formal rule: an experience-to-factor map `β`; its
  updates commute over any reordering of experiences (`ecjc_perm`), whereas
  JC with inputs keyed to experiences commutes on two experiences only when
  their inputs coincide (`jc_experience_comm_iff`).  The Joan case (pp.167-168)
  gives the same experience two different factor vectors (`joan`).
* **Certain evidence is not a Bayes-factor input** (p.168): JC with input `1`
  is conditioning (`jc_input_one`), but no positive factor reaches certainty
  (`oddsUpdate_lt_one`, `no_factor_reaches_certainty`).
* **The objective-likelihood proposal** (p.169): likelihoods `.8/.2` give the
  factor `4` for every prior (`objective_factor`).
* **Garber's problem** (p.170): a repeated factor `α > 1` drives the credence
  to `1` (`garber_repeated`); Wagner's considered experiences with a bounded
  product of factors keep it bounded below `1` (`considered_bounded`).

## What is philosophical, not formalized

Which elements *ought* to commute (Lange's Assumption, first conjunct, p.163),
whether experiences are individuated by phenomenal character or by their impact
(pp.164-167), the "normativity problem" that no norm can govern Bayes factors
qua magnitudes (pp.168-170), the "holism problem" of mapping considered
experiences to factors (pp.170-171), and the Carnap/Field history (pp.171-173).
The formal results above are the premises those arguments use; the conclusion
that "the Jeffrey framework is defective … either … in virtue of not commuting
its inputs, or else … in virtue of commuting the wrong kinds of ones" (p.173)
is a normative claim with no mathematical content beyond them.
-/
import Mathlib

set_option linter.unusedSectionVars false

namespace Literature.Cassell

open Finset

/-! ## Jeffrey Conditionalization on a finite space (p.160) -/

section JC

variable {Ω ι : Type*} [Fintype Ω] [DecidableEq ι]

/-- `p(Eᵢ)`, the probability of the cell `i` of the partition `e`. -/
def cellProb (p : Ω → ℝ) (e : Ω → ι) (i : ι) : ℝ := ∑ ω ∈ univ.filter (fun ω => e ω = i), p ω

/-- **Jeffrey Conditionalization** (p.160): `p'(ω) = p'(E_{e ω}) p(ω) / p(E_{e ω})`,
i.e. `p'(A) = ∑ᵢ p(A | Eᵢ) p'(Eᵢ)`. -/
noncomputable def jc (p : Ω → ℝ) (e : Ω → ι) (q : ι → ℝ) : Ω → ℝ :=
  fun ω => q (e ω) * p ω / cellProb p e (e ω)

/-- JC realizes its input: `p'(Eᵢ) = qᵢ`. -/
theorem jc_cell (p : Ω → ℝ) (e : Ω → ι) (q : ι → ℝ) (i : ι) (h : cellProb p e i ≠ 0) :
    cellProb (jc p e q) e i = q i := by
  show ∑ ω ∈ univ.filter (fun ω => e ω = i), jc p e q ω = q i
  have hc : ∀ ω ∈ univ.filter (fun ω => e ω = i),
      jc p e q ω = q i * p ω / cellProb p e i := by
    intro ω hω
    simp only [Finset.mem_filter] at hω
    simp only [jc, hω.2]
  rw [Finset.sum_congr rfl hc, ← Finset.sum_div, ← Finset.mul_sum]
  change q i * cellProb p e i / cellProb p e i = q i
  field_simp

/-- JC is rigid: within a cell, ratios of probabilities are unchanged. -/
theorem jc_rigid (p : Ω → ℝ) (e : Ω → ι) (q : ι → ℝ) {ω ω' : Ω} (he : e ω = e ω')
    (hq : q (e ω) ≠ 0) (hc : cellProb p e (e ω) ≠ 0) :
    jc p e q ω * p ω' = jc p e q ω' * p ω := by
  unfold jc
  rw [← he]
  field_simp

/-- **Non-commutativity on one partition** (pp.160-161): two JC updates on the
same partition end at the later input, `jc (jc p e q₁) e q₂ = jc p e q₂`. -/
theorem jc_twice (p : Ω → ℝ) (e : Ω → ι) (q₁ q₂ : ι → ℝ)
    (hc : ∀ i, cellProb p e i ≠ 0) (hq : ∀ i, q₁ i ≠ 0) :
    jc (jc p e q₁) e q₂ = jc p e q₂ := by
  funext ω
  have h1 := jc_cell p e q₁ (e ω) (hc _)
  simp only [jc] at h1 ⊢
  rw [h1]
  have := hq (e ω); have := hc (e ω)
  field_simp

/-- Hence the two orders differ whenever the inputs differ on a cell with
positive mass somewhere. -/
theorem jc_noncomm (p : Ω → ℝ) (e : Ω → ι) (q₁ q₂ : ι → ℝ)
    (hc : ∀ i, cellProb p e i ≠ 0) (hq₁ : ∀ i, q₁ i ≠ 0) (hq₂ : ∀ i, q₂ i ≠ 0) (i : ι)
    (hne : q₁ i ≠ q₂ i) :
    jc (jc p e q₁) e q₂ ≠ jc (jc p e q₂) e q₁ := by
  rw [jc_twice p e q₁ q₂ hc hq₁, jc_twice p e q₂ q₁ hc hq₂]
  intro h
  apply hne
  have a := jc_cell p e q₂ i (hc i)
  have b := jc_cell p e q₁ i (hc i)
  rw [h] at a
  rw [← a, b]

/-- **Certain evidence** (p.168): JC with input `1` on the cell `i₀` and `0`
elsewhere is Bayesian Conditionalization on that cell. -/
theorem jc_input_one (p : Ω → ℝ) (e : Ω → ι) (i₀ : ι) (ω : Ω) :
    jc p e (fun i => if i = i₀ then 1 else 0) ω =
      if e ω = i₀ then p ω / cellProb p e i₀ else 0 := by
  simp only [jc]
  split_ifs with h
  · simp [h]
  · simp

/-- Updating by Bayes factors `β` on a partition (Field's rule, as in Wagner):
`p'(ω) ∝ β(e ω) p(ω)`. -/
noncomputable def bfUpdate (p : Ω → ℝ) (e : Ω → ι) (β : ι → ℝ) : Ω → ℝ :=
  fun ω => β (e ω) * p ω / ∑ ω', β (e ω') * p ω'

/-- **Bayes-factor updates commute, on any two partitions** (p.165, fn 5, the
direction credited to Field): factoring by `β` on `e` and then `γ` on `f` is
factoring by `β γ` at once, in either order. -/
theorem bfUpdate_comm {κ : Type*} [DecidableEq κ] (p : Ω → ℝ) (e : Ω → ι) (f : Ω → κ)
    (β : ι → ℝ) (γ : κ → ℝ) (hZ1 : ∑ ω, β (e ω) * p ω ≠ 0) (hZ2 : ∑ ω, γ (f ω) * p ω ≠ 0) :
    bfUpdate (bfUpdate p e β) f γ = bfUpdate (bfUpdate p f γ) e β := by
  funext ω
  unfold bfUpdate
  have e1 : ∑ ω', γ (f ω') * (β (e ω') * p ω' / ∑ ω'', β (e ω'') * p ω'') =
      (∑ ω', β (e ω') * γ (f ω') * p ω') / ∑ ω'', β (e ω'') * p ω'' := by
    rw [Finset.sum_div]
    refine Finset.sum_congr rfl fun ω' _ => ?_
    ring
  have e2 : ∑ ω', β (e ω') * (γ (f ω') * p ω' / ∑ ω'', γ (f ω'') * p ω'') =
      (∑ ω', β (e ω') * γ (f ω') * p ω') / ∑ ω'', γ (f ω'') * p ω'' := by
    rw [Finset.sum_div]
    refine Finset.sum_congr rfl fun ω' _ => ?_
    ring
  rw [e1, e2]
  by_cases h0 : ∑ ω', β (e ω') * γ (f ω') * p ω' = 0
  · rw [h0]; simp
  field_simp

end JC

/-! ## Two cells: Bayes factors and Field's rule (pp.160-165) -/

/-- The Bayes factor of a move `pE → qE` on `{E, Ē}` (p.165):
`(q(E)/q(Ē)) / (p(E)/p(Ē))`. -/
noncomputable def bf (qE pE : ℝ) : ℝ := (qE / (1 - qE)) / (pE / (1 - pE))

/-- Field's rule: the credence in `E` after updating `pE` by the factor `α`. -/
noncomputable def oddsUpdate (α pE : ℝ) : ℝ := α * pE / (α * pE + (1 - pE))

theorem bf_oddsUpdate {α pE : ℝ} (hα : 0 < α) (h0 : 0 < pE) (h1 : pE < 1) :
    bf (oddsUpdate α pE) pE = α := by
  have hD : 0 < α * pE + (1 - pE) := by nlinarith
  have hsub : 1 - oddsUpdate α pE = (1 - pE) / (α * pE + (1 - pE)) := by
    unfold oddsUpdate; field_simp; ring
  unfold bf; rw [hsub]; unfold oddsUpdate
  have : 1 - pE ≠ 0 := by linarith
  field_simp

theorem oddsUpdate_bf {qE pE : ℝ} (hq0 : 0 < qE) (hq1 : qE < 1) (h0 : 0 < pE) (h1 : pE < 1) :
    oddsUpdate (bf qE pE) pE = qE := by
  unfold oddsUpdate bf
  have : 1 - pE ≠ 0 := by linarith
  have : 1 - qE ≠ 0 := by linarith
  field_simp
  ring

theorem oddsUpdate_pos {α pE : ℝ} (hα : 0 < α) (h0 : 0 < pE) (h1 : pE < 1) :
    0 < oddsUpdate α pE ∧ oddsUpdate α pE < 1 := by
  unfold oddsUpdate
  have hD : 0 < α * pE + (1 - pE) := by nlinarith
  constructor
  · positivity
  · rw [div_lt_one hD]; linarith

/-- **Identical experiences commute** (p.165): updating by `a` then `b` is
updating by `a b`, in either order. -/
theorem oddsUpdate_comm {a b pE : ℝ} (ha : 0 < a) (hb : 0 < b) (h0 : 0 < pE) (h1 : pE < 1) :
    oddsUpdate a (oddsUpdate b pE) = oddsUpdate (a * b) pE ∧
    oddsUpdate a (oddsUpdate b pE) = oddsUpdate b (oddsUpdate a pE) := by
  have key : ∀ a b : ℝ, 0 < a → 0 < b → oddsUpdate a (oddsUpdate b pE) = oddsUpdate (a * b) pE := by
    intro a b ha hb
    have hD : 0 < b * pE + (1 - pE) := by nlinarith
    have hD' : 0 < a * b * pE + (1 - pE) := by nlinarith [mul_pos ha hb]
    unfold oddsUpdate
    have e : a * (b * pE / (b * pE + (1 - pE))) + (1 - b * pE / (b * pE + (1 - pE))) =
        (a * b * pE + (1 - pE)) / (b * pE + (1 - pE)) := by
      field_simp; ring
    rw [e]
    field_simp
  refine ⟨key a b ha hb, ?_⟩
  rw [key a b ha hb, key b a hb ha, mul_comm]

/-- The Bayes factor is a strictly increasing function of the posterior, and a
strictly decreasing function of the prior. -/
theorem bf_eq_iff_prior {q p p' : ℝ} (hq0 : 0 < q) (hq1 : q < 1) (hp0 : 0 < p) (hp1 : p < 1)
    (hp0' : 0 < p') (hp1' : p' < 1) : bf q p = bf q p' ↔ p = p' := by
  constructor
  · intro h
    unfold bf at h
    have h1 : 1 - p ≠ 0 := by linarith
    have h2 : 1 - p' ≠ 0 := by linarith
    have h3 : 1 - q ≠ 0 := by linarith
    field_simp at h
    nlinarith
  · intro h; rw [h]

theorem bf_eq_iff_post {q q' p : ℝ} (hq1 : q < 1) (hq1' : q' < 1) (hp0 : 0 < p) (hp1 : p < 1) : bf q p = bf q' p ↔ q = q' := by
  constructor
  · intro h
    unfold bf at h
    have h1 : 1 - p ≠ 0 := by linarith
    have h2 : 1 - q ≠ 0 := by linarith
    have h3 : 1 - q' ≠ 0 := by linarith
    field_simp at h
    nlinarith
  · intro h; rw [h]

/-- **Lange's inference** (pp.161-162), under the Bayes-factor reading of the
impact of an experience (p.165): reaching the same posterior from different
priors takes different Bayes factors, i.e. different experiences. -/
theorem different_priors_different_bf {q p p' : ℝ} (hq0 : 0 < q) (hq1 : q < 1) (hp0 : 0 < p)
    (hp1 : p < 1) (hp0' : 0 < p') (hp1' : p' < 1) (hne : p ≠ p') : bf q p ≠ bf q p' :=
  fun h => hne ((bf_eq_iff_prior hq0 hq1 hp0 hp1 hp0' hp1').1 h)

/-- **Reversing input values does not reverse experiences** (pp.160-163).  From
the prior `p`, the sequence `p → x → y` has factors `(bf x p, bf y x)` and the
reversed input values `p → y → x` have `(bf y p, bf x y)`.  The second is the
first in reverse order iff `x = p` and `y = p`: only when neither update moves
anything. -/
theorem reversed_inputs_reversed_bf_iff {p x y : ℝ} (hp0 : 0 < p) (hp1 : p < 1)
    (hx0 : 0 < x) (hx1 : x < 1) (hy0 : 0 < y) (hy1 : y < 1) :
    (bf y p = bf y x ∧ bf x y = bf x p) ↔ (x = p ∧ y = p) := by
  rw [bf_eq_iff_prior hy0 hy1 hp0 hp1 hx0 hx1, bf_eq_iff_prior hx0 hx1 hy0 hy1 hp0 hp1]
  constructor
  · rintro ⟨h1, h2⟩; exact ⟨h1.symm, h2⟩
  · rintro ⟨h1, h2⟩; exact ⟨h1.symm, h2⟩

theorem oddsUpdate_eq_self_iff {a pE : ℝ} (h0 : 0 < pE) (h1 : pE < 1) (ha : 0 < a) :
    oddsUpdate a pE = pE ↔ a = 1 := by
  unfold oddsUpdate
  have hD : 0 < a * pE + (1 - pE) := by nlinarith
  rw [div_eq_iff hD.ne']
  constructor
  · intro h
    have : (a - 1) * (pE * (1 - pE)) = 0 := by linear_combination h
    rcases mul_eq_zero.1 this with h' | h'
    · linarith
    · nlinarith
  · intro h; rw [h]; ring

theorem oddsUpdate_inj {b p₁ p₂ : ℝ} (hb : 0 < b) (h0 : 0 < p₁) (h1 : p₁ < 1) (h0' : 0 < p₂)
    (h1' : p₂ < 1) (h : oddsUpdate b p₁ = oddsUpdate b p₂) : p₁ = p₂ := by
  unfold oddsUpdate at h
  have hD1 : 0 < b * p₁ + (1 - p₁) := by nlinarith
  have hD2 : 0 < b * p₂ + (1 - p₂) := by nlinarith
  rw [div_eq_div_iff hD1.ne' hD2.ne'] at h
  have : b * (p₁ - p₂) = 0 := by linear_combination h
  rcases mul_eq_zero.1 this with h' | h'
  · linarith
  · linarith

/-- **The other direction** (the form in `notes/interior_omega.tex`): reversing
two *experiences* (factors `a` then `b`, against `b` then `a`) reverses the
*input values* only if `a = b = 1`, i.e. only if neither experience moves
anything.  Together with `reversed_inputs_reversed_bf_iff`, the two orders of
description (inputs, experiences) come apart in every nontrivial case. -/
theorem reversed_bf_reversed_inputs_iff {a b p : ℝ} (ha : 0 < a) (hb : 0 < b) (h0 : 0 < p)
    (h1 : p < 1) :
    (oddsUpdate b p = oddsUpdate b (oddsUpdate a p) ∧
        oddsUpdate a (oddsUpdate b p) = oddsUpdate a p) ↔ (a = 1 ∧ b = 1) := by
  obtain ⟨ua0, ua1⟩ := oddsUpdate_pos ha h0 h1
  obtain ⟨ub0, ub1⟩ := oddsUpdate_pos hb h0 h1
  constructor
  · rintro ⟨e1, e2⟩
    have hpa : p = oddsUpdate a p := oddsUpdate_inj hb h0 h1 ua0 ua1 e1
    have hpb : oddsUpdate b p = p := oddsUpdate_inj ha ub0 ub1 h0 h1 e2
    exact ⟨(oddsUpdate_eq_self_iff h0 h1 ha).1 hpa.symm, (oddsUpdate_eq_self_iff h0 h1 hb).1 hpb⟩
  · rintro ⟨rfl, rfl⟩
    have : oddsUpdate 1 p = p := (oddsUpdate_eq_self_iff h0 h1 one_pos).2 rfl
    simp [this]

/-- Weisberg's jellybean (Weisberg pp.3-4, not in Cassell): `.1 → .8 → .9`
against `.1 → .9 → .8` is the nontrivial case of
`reversed_inputs_reversed_bf_iff`: factors `(36, 9/4)` against `(81, 4/9)`. -/
theorem jellybean_weisberg :
    bf (8 / 10) (1 / 10) = 36 ∧ bf (9 / 10) (8 / 10) = 9 / 4 ∧
    bf (9 / 10) (1 / 10) = 81 ∧ bf (8 / 10) (9 / 10) = 4 / 9 := by
  refine ⟨?_, ?_, ?_, ?_⟩ <;> norm_num [bf]

/-- **Two glances at a raven** (pp.160-161): `.9` then `.7` ends at `.7`; the
reverse order ends at `.9` (the later input wins, `jc_twice`).  In factors the
two orders are different experiences: `.9 → .7` is `bf = 7/27`, `.7 → .9` is
`bf = 27/7`. -/
theorem raven : bf (7 / 10) (9 / 10) = 7 / 27 ∧ bf (9 / 10) (7 / 10) = 27 / 7 := by
  constructor <;> norm_num [bf]

/-- **Figure 1** (p.162), `p(e) = .99`: `ξ₁ : .99 → .8`, `ξ₂ : .8 → .75`,
`ξ₃ : .99 → .75`, `ξ₄ : .75 → .8`.  Lange's conditions hold
(`p(e) ≠ q(e)`, `p(e) ≠ q'(e)`, `q(e) = r'(e)`, `q'(e) = r(e)`), and the factors
are `4/99`, `3/4`, `1/33`, `4/3`: `ξ₁ ≠ ξ₄` and `ξ₂ ≠ ξ₃`. -/
theorem figure1 :
    ((99 : ℝ) / 100 ≠ 8 / 10 ∧ (99 : ℝ) / 100 ≠ 75 / 100) ∧
    (bf (8 / 10) (99 / 100) = 4 / 99 ∧ bf (75 / 100) (8 / 10) = 3 / 4 ∧
      bf (75 / 100) (99 / 100) = 1 / 33 ∧ bf (8 / 10) (75 / 100) = 4 / 3) ∧
    ((4 : ℝ) / 99 ≠ 4 / 3 ∧ (3 : ℝ) / 4 ≠ 1 / 33) := by
  refine ⟨⟨by norm_num, by norm_num⟩, ⟨?_, ?_, ?_, ?_⟩, by norm_num, by norm_num⟩ <;> norm_num [bf]

/-- **Lange's quoted example** (p.161, quoting Lange 2000, 398): lowering `e`
from `0.99` to `0.8` is a disconfirming experience (`bf = 4/99 < 1`), raising it
from `0.75` to `0.8` a confirming one (`bf = 4/3 > 1`). -/
theorem lange_quote : bf (8 / 10) (99 / 100) < 1 ∧ 1 < bf (8 / 10) (75 / 100) := by
  constructor <;> norm_num [bf]

/-- **Figure 2, the smell of pie** (p.163): `.1 → .3 → .7` against
`.1 → .7 → .3`.  The same weighted partition `R ↦ .3` is a confirming experience
first (`bf = 27/7 > 1`, "a decisive whiff of rhubarb pie") and a disconfirming
one second (`bf = 9/49 < 1`, "maybe a whiff of lemon"); likewise `R ↦ .7`
(`49/9` second, `21` first). -/
theorem figure2 :
    bf (3 / 10) (1 / 10) = 27 / 7 ∧ bf (7 / 10) (3 / 10) = 49 / 9 ∧
    bf (7 / 10) (1 / 10) = 21 ∧ bf (3 / 10) (7 / 10) = 9 / 49 ∧
    (1 : ℝ) < 27 / 7 ∧ (9 : ℝ) / 49 < 1 := by
  refine ⟨?_, ?_, ?_, ?_, by norm_num, by norm_num⟩ <;> norm_num [bf]

/-- **Fn 5's converse fails on one partition.**  From `p = 1/10` the input pair
`(1/2, 1/2)` commutes trivially (both orders end at `1/2`), yet the factor
sequences `(9, 1)` and `(9, 1)` are not each other's reverse.  "Two updates will
commute just in case they yield the same Bayes factors" needs Wagner's (2002)
conditions, which a partition fails with itself. -/
theorem same_partition_converse_fails :
    bf (1 / 2) (1 / 10) = 9 ∧ bf (1 / 2) (1 / 2) = 1 ∧ (9 : ℝ) ≠ 1 := by
  refine ⟨?_, ?_, by norm_num⟩ <;> norm_num [bf]

/-! ## ECJC (p.167) and JC with experience-keyed inputs -/

section ECJC

variable {Ξ : Type*}

/-- **ECJC**, clause 2 as a function: each experience `ξ` carries one Bayes
factor `β ξ`, whatever the prior; clause 1 is JC, here in Field's form. -/
noncomputable def ecjc (β : Ξ → ℝ) (pE : ℝ) (xs : List Ξ) : ℝ :=
  xs.foldl (fun c ξ => oddsUpdate (β ξ) c) pE

theorem ecjc_eq_prod {β : Ξ → ℝ} (hβ : ∀ ξ, 0 < β ξ) :
    ∀ (xs : List Ξ) (pE : ℝ), 0 < pE → pE < 1 → ecjc β pE xs = oddsUpdate (xs.map β).prod pE := by
  intro xs
  induction xs with
  | nil =>
    intro pE _ _
    simp [ecjc, oddsUpdate]
  | cons ξ xs ih =>
    intro pE h0 h1
    obtain ⟨a0, a1⟩ := oddsUpdate_pos (hβ ξ) h0 h1
    have : ecjc β pE (ξ :: xs) = ecjc β (oddsUpdate (β ξ) pE) xs := rfl
    rw [this, ih _ a0 a1]
    have hprod : 0 < (xs.map β).prod := List.prod_pos (by simpa using fun x _ => hβ x)
    rw [(oddsUpdate_comm hprod (hβ ξ) h0 h1).1]
    simp [List.map_cons, List.prod_cons, mul_comm]

/-- **Lange's Assumption, second conjunct, holds for ECJC**: any reordering of
the same experiences ends at the same credence. -/
theorem ecjc_perm {β : Ξ → ℝ} (hβ : ∀ ξ, 0 < β ξ) {xs ys : List Ξ} (h : xs.Perm ys)
    {pE : ℝ} (h0 : 0 < pE) (h1 : pE < 1) : ecjc β pE xs = ecjc β pE ys := by
  rw [ecjc_eq_prod hβ xs pE h0 h1, ecjc_eq_prod hβ ys pE h0 h1, (h.map β).prod_eq]

/-- JC with the *input value* keyed to the experience (`x ξ`): on one partition
the result of a nonempty sequence is the last experience's input. -/
def jcKeyed (x : Ξ → ℝ) (pE : ℝ) (xs : List Ξ) : ℝ := xs.foldl (fun _ ξ => x ξ) pE

/-- **JC alone does not commute experiences**: two experiences commute under
experience-keyed JC iff they carry the same input value. -/
theorem jc_experience_comm_iff (x : Ξ → ℝ) (pE : ℝ) (ξ₁ ξ₂ : Ξ) :
    jcKeyed x pE [ξ₁, ξ₂] = jcKeyed x pE [ξ₂, ξ₁] ↔ x ξ₁ = x ξ₂ := by
  simp only [jcKeyed, List.foldl_cons, List.foldl_nil]
  exact ⟨fun h => h.symm, fun h => h.symm⟩

end ECJC

/-- **Joan** (pp.167-168), on the partition `{R, C, O}` (rhubarb, cherry,
other) with the uniform prior: the child's update (`C ↦ .7`, the rest shared
`.15, .15`) and the adult's (`R ↦ .7`) follow the same phenomenal experience but
have different factor vectors, e.g. `β(R : C) = 3/14` against `14/3`.  So
ECJC's clause 2 is violated, as the text says. -/
theorem joan :
    ((15 / 100 : ℝ) / (70 / 100)) / ((1 / 3) / (1 / 3)) = 3 / 14 ∧
    ((70 / 100 : ℝ) / (15 / 100)) / ((1 / 3) / (1 / 3)) = 14 / 3 ∧ (3 : ℝ) / 14 ≠ 14 / 3 := by
  norm_num

/-! ## Certainty, objective likelihoods, Garber (pp.168-170) -/

theorem oddsUpdate_lt_one {α pE : ℝ} (hα : 0 < α) (h0 : 0 < pE) (h1 : pE < 1) :
    oddsUpdate α pE < 1 := (oddsUpdate_pos hα h0 h1).2

/-- **Bayes' rule is not a special case under Bayes-factor inputs** (p.168):
"Bayes factors are undefined whenever one of the evidence propositions receives a
value of one"; no positive factor takes an uncertain `E` to certainty, although
JC with input `1` does (`jc_input_one`). -/
theorem no_factor_reaches_certainty {pE : ℝ} (h0 : 0 < pE) (h1 : pE < 1) :
    ¬ ∃ α : ℝ, 0 < α ∧ oddsUpdate α pE = 1 :=
  fun ⟨_, hα, h⟩ => (oddsUpdate_lt_one hα h0 h1).ne h

/-- **The objective-likelihood proposal** (p.169): if the experience has
probability `.8` given `R` and `.2` given `¬R`, conditioning on it has Bayes
factor `4` for every prior. -/
theorem objective_factor {pE : ℝ} (h0 : 0 < pE) (h1 : pE < 1) :
    bf (8 / 10 * pE / (8 / 10 * pE + 2 / 10 * (1 - pE))) pE = 4 ∧
      8 / 10 * pE / (8 / 10 * pE + 2 / 10 * (1 - pE)) = oddsUpdate 4 pE := by
  have e : 8 / 10 * pE / (8 / 10 * pE + 2 / 10 * (1 - pE)) = oddsUpdate 4 pE := by
    unfold oddsUpdate
    rw [div_eq_div_iff (by nlinarith) (by nlinarith)]
    ring
  exact ⟨by rw [e]; exact bf_oddsUpdate (by norm_num) h0 h1, e⟩

/-- **Garber's problem** (p.170): the same factor `α > 1` applied `n` times
drives the credence arbitrarily close to certainty. -/
theorem garber_repeated {α pE : ℝ} (hα : 1 < α) (h0 : 0 < pE) (h1 : pE < 1) {ε : ℝ}
    (hε : 0 < ε) : ∃ n : ℕ, 1 - ε < oddsUpdate (α ^ n) pE := by
  -- 1 - oddsUpdate a pE = (1-pE)/(a pE + (1-pE)) ≤ (1-pE)/(a pE)
  obtain ⟨n, hn⟩ := pow_unbounded_of_one_lt ((1 - pE) / (ε * pE)) hα
  refine ⟨n, ?_⟩
  have ha : 0 < α ^ n := by positivity
  have hD : 0 < α ^ n * pE + (1 - pE) := by nlinarith
  have hsub : 1 - oddsUpdate (α ^ n) pE = (1 - pE) / (α ^ n * pE + (1 - pE)) := by
    unfold oddsUpdate; field_simp; ring
  have : (1 - pE) / (α ^ n * pE + (1 - pE)) < ε := by
    rw [div_lt_iff₀ hD]
    rw [div_lt_iff₀ (by positivity)] at hn
    nlinarith
  linarith

/-- Field's rule is increasing in the factor. -/
theorem oddsUpdate_mono {a b pE : ℝ} (ha : 0 < a) (hab : a ≤ b) (h0 : 0 < pE) (h1 : pE < 1) :
    oddsUpdate a pE ≤ oddsUpdate b pE := by
  unfold oddsUpdate
  have hDa : 0 < a * pE + (1 - pE) := by nlinarith
  have hDb : 0 < b * pE + (1 - pE) := by nlinarith
  rw [div_le_div_iff₀ hDa hDb]
  nlinarith [mul_le_mul_of_nonneg_right hab (le_of_lt (mul_pos h0 (sub_pos.2 h1)))]

/-- **Wagner's considered experiences** (p.171): if the factors of successive
experiences have product at most `M`, the credence never exceeds
`oddsUpdate M pE < 1`, however often the phenomenal experience recurs. -/
theorem considered_bounded {Ξ : Type*} {β : Ξ → ℝ} (hβ : ∀ ξ, 0 < β ξ) (xs : List Ξ)
    {M pE : ℝ} (hM : (xs.map β).prod ≤ M) (h0 : 0 < pE) (h1 : pE < 1) :
    ecjc β pE xs ≤ oddsUpdate M pE ∧ oddsUpdate M pE < 1 := by
  have hprod : 0 < (xs.map β).prod := List.prod_pos (by simpa using fun x _ => hβ x)
  rw [ecjc_eq_prod hβ xs pE h0 h1]
  exact ⟨oddsUpdate_mono hprod hM h0 h1, oddsUpdate_lt_one (lt_of_lt_of_le hprod hM) h0 h1⟩

end Literature.Cassell
