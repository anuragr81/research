/-
# Weisberg, "Commutativity or Holism? A Dilemma for Conditionalizers"

Preprint `JCvF.pdf`, 21 pp. (published version believed to be *BJPS* 60(4),
2009, 793-812; not confirmable from the preprint).  Page numbers are the
preprint's own.

Formalization of the paper's own mathematical claims, not of Paper B's.

## What is formalized

* **Strict Conditionalization** is commutative on propositions (p.7,
  `cond_cond`), and the p.8 step of the Strict dilemma: if
  `p(·|EF) = p(·|E'F)` then `p(E|E'F) = 1` (`strict_dilemma`).
* **Jeffrey Conditionalization** (p.9) on a two-cell partition `{E, Ē}`
  (`jeffrey2`) and on an arbitrary partition (`jeffrey`); the p.9
  non-commutativity on input distributions (two updates on one partition: the
  later value wins, `jeffrey2_twice`, `jeffrey2_not_comm_inputs`); and the p.10
  triviality ("To get an arbitrary `q` from `p` via Jeffrey Conditionalization,
  we just do a Jeffrey Conditionalization update on the set of epistemic
  possibilities", `jeffrey_finest`).
* **Bayes factors and Field's rule** (p.11): `bf`, `oddsUpdate`; Field's rule
  recovers its factor and is commutative on experiences (`bf_oddsUpdate`,
  `oddsUpdate_comm`).
* **The jellybean numbers**: pp.3-4 (Lange's point: `.1 → .8 → .9` vs
  `.1 → .9 → .8`; the Bayes factors are `36, 9/4` vs `81, 4/9`, and reversing
  the *experiences* under Field returns to `.9`) and p.13 (`1/10 → 9/10` is
  Bayes factor `81`; with `q'(E) = 1/10`, `r'(E) = 9/10`).
* **Wagner's theorem as proved in the Appendix** (pp.18-20): the identities
  (26)/(27) expressing each step's Bayes factor through the prior and final
  cell probabilities, and the conclusion (28)/(29), for the cell model
  `Ω = ι × κ` of the two partitions `E = {Eᵢ}` and `F = {Fⱼ}`
  (`appendix26`, `appendix27`, `wagner_E`, and the `F` versions).
  A worked instance of the p.13 jellybean application is `jellybean_wagner`.
* **Rigidity Preserves Independence** (p.16, with the proof of note 12):
  `rigidity_preserves_independence`, for a general finite space and any
  proposition `F`, with rigidity as a hypothesis; `jeffrey2_rigid` shows
  Jeffrey's rule is rigid, and `no_undercutting` is the p.16 incompatibility of
  (6) and (7).

## What formalizing revealed

* Note 12's proof says "By rigidity then, `q(F|E) = q(F)`".  Rigidity gives
  `q(F|E) = p(F|E) = p(F)` directly; getting `p(F) = q(F)` needs rigidity on
  **both** cells `E` and `Ē` together with independence.  The Lean proof passes
  through `q(F) = p(F)` (`rpi_qF`): a rigid update on `{E, Ē}` leaves the
  probability of an independent `F` unchanged.  The theorem needs
  `0 < p(E) < 1` and `p(F) > 0` so that the conditionals are defined.
* In the cell model, the Appendix's weakened hypothesis (5)/(8)
  (`r(EᵢFⱼ) = r'(EᵢFⱼ)` only on the cells) is the whole of what the proof uses;
  (26) and (27) are pure consequences of rigidity and hold with no
  commutativity assumption at all.  Our statement assumes every cell has
  positive prior probability, which is stronger than Wagner's overlap
  conditions (1)-(2); with it, each identity holds for every `j`.
* The p.13 jellybean application holds exactly: `jellybean_wagner` exhibits a
  prior with `E` independent of `F`, both orders of the two updates ending in
  the same state, and Bayes factor `81` for the `E`-experience in both orders.

## Not formalized

Holism (p.5) and occasional, partial commutativity on experiences (p.5) are
normative desiderata about experiences, which have no representation here.
The defeater vocabulary (undercutting vs rebutting, p.15-16) is interpretation
of the formal result, not part of it.  The Appendix variant with time-indexed
partitions `E'`, `F'` and hypothesis (9) reduces, in a cell model, to the
statement proved here with the cells relabelled.
-/
import Mathlib

set_option linter.unusedSectionVars false

namespace Literature.Weisberg

open Finset

section General

variable {Ω : Type*} [Fintype Ω] [DecidableEq Ω]

/-- `p(A)`. -/
def mass (p : Ω → ℝ) (A : Finset Ω) : ℝ := ∑ x ∈ A, p x

/-- **Strict Conditionalization** (p.7): `q(·) = p(·|E)`. -/
noncomputable def cond (p : Ω → ℝ) (A : Finset Ω) : Ω → ℝ :=
  fun x => if x ∈ A then p x / mass p A else 0

theorem mass_nonneg {p : Ω → ℝ} (hp : ∀ x, 0 ≤ p x) (A : Finset Ω) : 0 ≤ mass p A :=
  Finset.sum_nonneg fun x _ => hp x

theorem mass_cond (p : Ω → ℝ) (A B : Finset Ω) :
    mass (cond p A) B = mass p (B ∩ A) / mass p A := by
  unfold mass cond
  rw [← Finset.sum_ite_mem, Finset.sum_div]
  refine Finset.sum_congr rfl fun x _ => ?_
  by_cases hx : x ∈ A <;> simp [hx, mass]

/-- **Strict Conditionalization is commutative on propositions** (p.7: "In either
case, the resulting probability function will be `p(·|EF)`"). -/
theorem cond_cond {p : Ω → ℝ} (hp : ∀ x, 0 ≤ p x) (A B : Finset Ω) :
    cond (cond p A) B = cond p (A ∩ B) := by
  funext x
  have hm := mass_cond p A B
  unfold cond at *
  rw [hm, Finset.inter_comm B A]
  by_cases hB : x ∈ B <;> by_cases hA : x ∈ A <;> simp [hA, hB]
  have h1 : mass p (A ∩ B) ≤ mass p A :=
    Finset.sum_le_sum_of_subset_of_nonneg Finset.inter_subset_left fun x _ _ => hp x
  have h0 : 0 ≤ mass p (A ∩ B) := mass_nonneg hp _
  rcases h0.lt_or_eq with h | h
  · have : mass p A ≠ 0 := by linarith
    field_simp
  · rw [← h]; simp

/-- **The Strict dilemma, p.8**: "It's logically possible, of course, for
`p(·|EF)` to be the same as `p(·|E'F)`, but this can only happen when
`p(E|E'F) = 1`." -/
theorem strict_dilemma (p : Ω → ℝ) (E E' F : Finset Ω) (h1 : mass p (E ∩ F) ≠ 0)
    (h : cond p (E ∩ F) = cond p (E' ∩ F)) :
    mass p (E ∩ (E' ∩ F)) / mass p (E' ∩ F) = 1 := by
  have := congrArg (fun r => mass r E) h
  simp only [mass_cond] at this
  rw [← this, ← Finset.inter_assoc, Finset.inter_self]
  exact div_self h1

/-- **Jeffrey Conditionalization on `{E, Ē}`** (p.9): `q(·) = p(·|E)x + p(·|Ē)(1-x)`. -/
noncomputable def jeffrey2 (p : Ω → ℝ) (E : Finset Ω) (x : ℝ) : Ω → ℝ :=
  fun w => if w ∈ E then x * p w / mass p E else (1 - x) * p w / mass p (univ \ E)

theorem mass_jeffrey2_inter (p : Ω → ℝ) (E H : Finset Ω) (x : ℝ) :
    mass (jeffrey2 p E x) (H ∩ E) = x * mass p (H ∩ E) / mass p E := by
  unfold mass jeffrey2
  rw [Finset.mul_sum, Finset.sum_div]
  refine Finset.sum_congr rfl fun w hw => ?_
  rw [if_pos (Finset.mem_inter.mp hw).2]; rfl

theorem mass_jeffrey2_sdiff (p : Ω → ℝ) (E H : Finset Ω) (x : ℝ) :
    mass (jeffrey2 p E x) (H \ E) = (1 - x) * mass p (H \ E) / mass p (univ \ E) := by
  unfold mass jeffrey2
  rw [Finset.mul_sum, Finset.sum_div]
  refine Finset.sum_congr rfl fun w hw => ?_
  rw [if_neg (Finset.mem_sdiff.mp hw).2]; rfl

/-- The input value is the new probability of `E`. -/
theorem mass_jeffrey2_E (p : Ω → ℝ) (E : Finset Ω) (x : ℝ) (hE : mass p E ≠ 0) :
    mass (jeffrey2 p E x) E = x := by
  have := mass_jeffrey2_inter p E E x
  rw [Finset.inter_self] at this
  rw [this]; field_simp

/-- **Rigidity** with respect to `{E, Ē}` (p.15: "they preserve the conditional
probabilities on the evidence"), cross-multiplied: `q(H|E) = p(H|E)` and
`q(H|Ē) = p(H|Ē)` for every `H`. -/
def Rigid (p q : Ω → ℝ) (E : Finset Ω) : Prop :=
  ∀ H : Finset Ω,
    mass q (H ∩ E) * mass p E = mass p (H ∩ E) * mass q E ∧
    mass q (H \ E) * mass p (univ \ E) = mass p (H \ E) * mass q (univ \ E)

/-- Jeffrey Conditionalization is rigid (p.15). -/
theorem jeffrey2_rigid (p : Ω → ℝ) (E : Finset Ω) (x : ℝ) (hE : mass p E ≠ 0)
    (hnE : mass p (univ \ E) ≠ 0) : Rigid p (jeffrey2 p E x) E := by
  intro H
  have h1 := mass_jeffrey2_inter p E H x
  have h2 := mass_jeffrey2_sdiff p E H x
  have h3 := mass_jeffrey2_inter p E univ x
  have h4 := mass_jeffrey2_sdiff p E univ x
  rw [Finset.univ_inter] at h3
  refine ⟨?_, ?_⟩
  · rw [h1, h3]; field_simp
  · rw [h2, h4]; field_simp

theorem mass_split (p : Ω → ℝ) (F E : Finset Ω) :
    mass p F = mass p (F ∩ E) + mass p (F \ E) := by
  unfold mass; exact (Finset.sum_inter_add_sum_sdiff F E p).symm

/-- A rigid update on `{E, Ē}` leaves the probability of a `p`-independent `F`
unchanged.  (The step note 12 leaves implicit.) -/
theorem rpi_qF {p q : Ω → ℝ} {E F : Finset Ω} (hp1 : mass p univ = 1)
    (hq1 : mass q univ = 1) (hE0 : mass p E ≠ 0) (hE1 : mass p (univ \ E) ≠ 0)
    (hrig : Rigid p q E) (hind : mass p (F ∩ E) = mass p E * mass p F) :
    mass q F = mass p F := by
  obtain ⟨r1, r2⟩ := hrig F
  have hpu := mass_split p univ E
  have hqu := mass_split q univ E
  rw [Finset.univ_inter] at hpu hqu
  have hpF := mass_split p F E
  have hqF := mass_split q F E
  -- `p(F ∖ E) = p(F) p(Ē)`
  have hb : mass p (F \ E) = mass p F * mass p (univ \ E) := by
    linear_combination -hpF - hind + mass p F * (hpu - hp1)
  have e1 : mass q (F ∩ E) = mass p F * mass q E := by
    apply mul_right_cancel₀ hE0
    rw [r1, hind]; ring
  have e2 : mass q (F \ E) = mass p F * mass q (univ \ E) := by
    apply mul_right_cancel₀ hE1
    rw [r2, hb]; ring
  rw [hqF, e1, e2]
  linear_combination mass p F * (hq1 - hqu)

/-- **Rigidity Preserves Independence** (p.16).  If the transition from `p` to
`q` is rigid with respect to `{E, Ē}` and (6) `p(E|F) = p(E)`, then
`q(E|F) = q(E)`. -/
theorem rigidity_preserves_independence {p q : Ω → ℝ} {E F : Finset Ω}
    (hp1 : mass p univ = 1) (hq1 : mass q univ = 1) (hE0 : mass p E ≠ 0)
    (hE1 : mass p (univ \ E) ≠ 0) (hF : mass p F ≠ 0) (hrig : Rigid p q E)
    (h6 : mass p (E ∩ F) / mass p F = mass p E) :
    mass q (E ∩ F) / mass q F = mass q E := by
  have hind : mass p (F ∩ E) = mass p E * mass p F := by
    rw [Finset.inter_comm]; rw [div_eq_iff hF] at h6; exact h6
  have hqF := rpi_qF hp1 hq1 hE0 hE1 hrig hind
  have e1 : mass q (F ∩ E) = mass p F * mass q E := by
    apply mul_right_cancel₀ hE0
    rw [(hrig F).1, hind]; ring
  rw [Finset.inter_comm, e1, hqF]
  field_simp

/-- (6) and (7) are incompatible under rigidity (p.16): a later-discovered
`F` that was initially irrelevant to `E` cannot lower `E`'s probability, so it
cannot act as an undercutting defeater. -/
theorem no_undercutting {p q : Ω → ℝ} {E F : Finset Ω}
    (hp1 : mass p univ = 1) (hq1 : mass q univ = 1) (hE0 : mass p E ≠ 0)
    (hE1 : mass p (univ \ E) ≠ 0) (hF : mass p F ≠ 0) (hrig : Rigid p q E) :
    ¬ (mass p (E ∩ F) / mass p F = mass p E ∧ mass q (E ∩ F) / mass q F < mass q E) := by
  rintro ⟨h6, h7⟩
  rw [rigidity_preserves_independence hp1 hq1 hE0 hE1 hF hrig h6] at h7
  exact lt_irrefl _ h7

/-- Rigidity Preserves Independence for Jeffrey Conditionalization itself. -/
theorem rpi_jeffrey2 {p : Ω → ℝ} {E F : Finset Ω} (x : ℝ)
    (hp1 : mass p univ = 1) (hE0 : mass p E ≠ 0) (hE1 : mass p (univ \ E) ≠ 0)
    (hF : mass p F ≠ 0) (h6 : mass p (E ∩ F) / mass p F = mass p E) :
    mass (jeffrey2 p E x) (E ∩ F) / mass (jeffrey2 p E x) F = mass (jeffrey2 p E x) E := by
  have hq1 : mass (jeffrey2 p E x) univ = 1 := by
    rw [mass_split _ univ E, Finset.univ_inter, mass_jeffrey2_sdiff,
      mass_jeffrey2_E p E x hE0, mul_div_assoc, div_self hE1]
    ring
  exact rigidity_preserves_independence hp1 hq1 hE0 hE1 hF (jeffrey2_rigid p E x hE0 hE1) h6

/-- Two Jeffrey updates on one partition `{E, Ē}`: the later input value wins
(p.9: "The first update leaves `E` with its input value, `x`, and the second
leaves it with `y`"). -/
theorem jeffrey2_twice (p : Ω → ℝ) (E : Finset Ω) (x y : ℝ) (hE0 : mass p E ≠ 0)
    (hE1 : mass p (univ \ E) ≠ 0) (hx0 : x ≠ 0) (hx1 : x ≠ 1) :
    jeffrey2 (jeffrey2 p E x) E y = jeffrey2 p E y := by
  have hm1 : mass (jeffrey2 p E x) E = x := mass_jeffrey2_E p E x hE0
  have hm2 : mass (jeffrey2 p E x) (univ \ E) = 1 - x := by
    rw [mass_jeffrey2_sdiff]; field_simp
  have hx1' : 1 - x ≠ 0 := sub_ne_zero.mpr (Ne.symm hx1)
  funext w
  rw [show jeffrey2 (jeffrey2 p E x) E y w = if w ∈ E then y * jeffrey2 p E x w /
      mass (jeffrey2 p E x) E else (1 - y) * jeffrey2 p E x w / mass (jeffrey2 p E x) (univ \ E)
      from rfl, hm1, hm2]
  by_cases hw : w ∈ E <;> simp only [jeffrey2, hw, if_true, if_false] <;> field_simp

/-- **Jeffrey Conditionalization is not commutative on input distributions**
(p.9): on one partition with values `x` then `y`, `E` ends at `y`; reversed, at
`x`. -/
theorem jeffrey2_not_comm_inputs (p : Ω → ℝ) (E : Finset Ω) (x y : ℝ)
    (hE0 : mass p E ≠ 0) (hE1 : mass p (univ \ E) ≠ 0) (hx0 : x ≠ 0) (hx1 : x ≠ 1)
    (hy0 : y ≠ 0) (hy1 : y ≠ 1) (hxy : x ≠ y) :
    mass (jeffrey2 (jeffrey2 p E x) E y) E = y ∧
    mass (jeffrey2 (jeffrey2 p E y) E x) E = x ∧
    jeffrey2 (jeffrey2 p E x) E y ≠ jeffrey2 (jeffrey2 p E y) E x := by
  rw [jeffrey2_twice p E x y hE0 hE1 hx0 hx1, jeffrey2_twice p E y x hE0 hE1 hy0 hy1,
    mass_jeffrey2_E p E y hE0, mass_jeffrey2_E p E x hE0]
  refine ⟨rfl, rfl, fun h => hxy ?_⟩
  have := congrArg (fun r => mass r E) h
  simp only [mass_jeffrey2_E p E _ hE0] at this
  exact this.symm

/-- Mass of cell `i` of a partition `u`. -/
def cellMass {ι : Type*} [DecidableEq ι] (p : Ω → ℝ) (u : Ω → ι) (i : ι) : ℝ :=
  ∑ y, if u y = i then p y else 0

/-- Jeffrey Conditionalization on an arbitrary partition (p.9). -/
noncomputable def jeffrey {ι : Type*} [DecidableEq ι] (p : Ω → ℝ) (u : Ω → ι)
    (q : ι → ℝ) : Ω → ℝ :=
  fun x => q (u x) * p x / cellMass p u (u x)

/-- **Jeffrey Conditionalization is trivially satisfiable** (p.10): any `q` is
obtained from `p` by one Jeffrey update on the partition into the epistemic
possibilities `{wᵢ}` with input values `q(wᵢ)` (here, `p(w) ≠ 0` for every
`w`). -/
theorem jeffrey_finest (p q : Ω → ℝ) (hp : ∀ w, p w ≠ 0) : jeffrey p id q = q := by
  funext w
  have : cellMass p id w = p w := by
    unfold cellMass
    rw [Finset.sum_eq_single w]
    · simp
    · intro b _ hb; simp [hb]
    · simp
  simp only [jeffrey, id, this]
  field_simp [hp w]

end General

/-! ## Bayes factors, Field's rule, and the jellybean numbers (pp.3-4, 11, 13) -/

section BayesFactor

/-- The Bayes factor `β_{q,p}(E : Ē) = (q(E)/q(Ē)) / (p(E)/p(Ē))` (p.11). -/
noncomputable def bf (qE pE : ℝ) : ℝ := (qE / (1 - qE)) / (pE / (1 - pE))

/-- Field's rule (p.11): "we solve for `q(E)` in the equation `α = β`". -/
noncomputable def oddsUpdate (α pE : ℝ) : ℝ := α * pE / (α * pE + (1 - pE))

theorem bf_oddsUpdate {α pE : ℝ} (hα : 0 < α) (h0 : 0 < pE) (h1 : pE < 1) :
    bf (oddsUpdate α pE) pE = α := by
  have hD : 0 < α * pE + (1 - pE) := by nlinarith
  have hsub : 1 - oddsUpdate α pE = (1 - pE) / (α * pE + (1 - pE)) := by
    unfold oddsUpdate; field_simp; ring
  unfold bf; rw [hsub]; unfold oddsUpdate
  have : 1 - pE ≠ 0 := by linarith
  field_simp

/-- **Field's rule is commutative on experiences** (p.11: "Doing a Jeffrey
Conditionalization update with `α = x` and then `α = y` yields the same result
as doing an update with `α = y` first and then with `α = x`"). -/
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

/-- **Lange's point, jellybean (pp.3-4).**  `.1 → .8 → .9` has Bayes factors `36`
and `9/4`; `.1 → .9 → .8`, the reversed input values, has `81` and `4/9`.
Reversing the *experiences* (the factors) instead ends where the first order
ended, at `.9`; reversing the input values ends at `.8`. -/
theorem lange_jellybean :
    bf (8/10) (1/10) = 36 ∧ bf (9/10) (8/10) = 9/4 ∧
    bf (9/10) (1/10) = 81 ∧ bf (8/10) (9/10) = 4/9 ∧
    oddsUpdate (9/4) (oddsUpdate 36 (1/10)) = 9/10 ∧
    oddsUpdate 36 (oddsUpdate (9/4) (1/10)) = 9/10 := by
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_⟩ <;> norm_num [bf, oddsUpdate]

/-- **The p.13 jellybean numbers**: "`E` gets boosted initially; from `1/10` to
`9/10`", so `β = 81`; with `q'(E) = 1/10`, the same factor gives
`r'(E) = 9/10`. -/
theorem jellybean_81 : bf (9/10) (1/10) = 81 ∧ oddsUpdate 81 (1/10) = 9/10 := by
  constructor <;> norm_num [bf, oddsUpdate]

end BayesFactor

/-! ## Wagner's theorem via the Appendix (pp.12-13, 18-20) -/

section Wagner

variable {ι κ : Type*} [Fintype ι] [Fintype κ]

/-- `p(Eᵢ)` in the cell model `Ω = ι × κ` (cells `EᵢFⱼ`). -/
def margE (p : ι × κ → ℝ) (i : ι) : ℝ := ∑ j, p (i, j)

/-- `p(Fⱼ)`. -/
def margF (p : ι × κ → ℝ) (j : κ) : ℝ := ∑ i, p (i, j)

/-- Jeffrey Conditionalization on `E = {Eᵢ}` with input values `a`. -/
noncomputable def jeffE (p : ι × κ → ℝ) (a : ι → ℝ) : ι × κ → ℝ :=
  fun x => a x.1 * p x / margE p x.1

/-- Jeffrey Conditionalization on `F = {Fⱼ}` with input values `b`. -/
noncomputable def jeffF (p : ι × κ → ℝ) (b : κ → ℝ) : ι × κ → ℝ :=
  fun x => b x.2 * p x / margF p x.2

/-- The Bayes factor `β_{q,p}(k₁ : k₂) = (q(k₁)/q(k₂)) / (p(k₁)/p(k₂))`. -/
noncomputable def bfv {α : Type*} (q p : α → ℝ) (k₁ k₂ : α) : ℝ :=
  (q k₁ / q k₂) / (p k₁ / p k₂)

theorem margE_jeffE (p : ι × κ → ℝ) (a : ι → ℝ) (i : ι) (h : margE p i ≠ 0) :
    margE (jeffE p a) i = a i := by
  show ∑ j, a i * p (i, j) / margE p i = a i
  rw [← Finset.sum_div, ← Finset.mul_sum, mul_div_assoc]
  show a i * (margE p i / margE p i) = a i
  rw [div_self h, mul_one]

theorem margF_jeffF (p : ι × κ → ℝ) (b : κ → ℝ) (j : κ) (h : margF p j ≠ 0) :
    margF (jeffF p b) j = b j := by
  show ∑ i, b j * p (i, j) / margF p j = b j
  rw [← Finset.sum_div, ← Finset.mul_sum, mul_div_assoc]
  show b j * (margF p j / margF p j) = b j
  rw [div_self h, mul_one]

theorem margE_pos {p : ι × κ → ℝ} (hp : ∀ x, 0 < p x) (i : ι) (j : κ) : 0 < margE p i :=
  Finset.sum_pos (fun _ _ => hp _) ⟨j, Finset.mem_univ j⟩

theorem margF_pos {p : ι × κ → ℝ} (hp : ∀ x, 0 < p x) (i : ι) (j : κ) : 0 < margF p j :=
  Finset.sum_pos (fun _ _ => hp _) ⟨i, Finset.mem_univ i⟩

theorem jeffE_pos {p : ι × κ → ℝ} (hp : ∀ x, 0 < p x) {a : ι → ℝ} (ha : ∀ i, 0 < a i)
    (x : ι × κ) : 0 < jeffE p a x :=
  div_pos (mul_pos (ha _) (hp _)) (margE_pos hp x.1 x.2)

theorem jeffF_pos {p : ι × κ → ℝ} (hp : ∀ x, 0 < p x) {b : κ → ℝ} (hb : ∀ j, 0 < b j)
    (x : ι × κ) : 0 < jeffF p b x :=
  div_pos (mul_pos (hb _) (hp _)) (margF_pos hp x.1 x.2)

/-- **Appendix (26)** (p.20), for `p →E q →F r`:
`β_{q,p}(E_{i₁} : E_{i₂}) = p(E_{i₂}F_j) r(E_{i₁}F_j) / (p(E_{i₁}F_j) r(E_{i₂}F_j))`,
for every `j`.  Only rigidity is used. -/
theorem appendix26 {p : ι × κ → ℝ} (hp : ∀ x, 0 < p x) {a : ι → ℝ} (ha : ∀ i, 0 < a i)
    {b : κ → ℝ} (hb : ∀ j, 0 < b j) (i₁ i₂ : ι) (j : κ) :
    bfv (margE (jeffE p a)) (margE p) i₁ i₂ =
      p (i₂, j) * jeffF (jeffE p a) b (i₁, j) / (p (i₁, j) * jeffF (jeffE p a) b (i₂, j)) := by
  unfold bfv
  rw [margE_jeffE p a i₁ (margE_pos hp i₁ j).ne', margE_jeffE p a i₂ (margE_pos hp i₂ j).ne']
  have hq := margF_pos (jeffE_pos hp ha) i₁ j
  have h1 := margE_pos hp i₁ j
  have h2 := margE_pos hp i₂ j
  have := ha i₁; have := ha i₂; have := hb j; have := hp (i₁, j); have := hp (i₂, j)
  simp only [jeffF, jeffE]
  field_simp

/-- **Appendix (27)** (p.20), for `p →F q' →E r'`:
`β_{r',q'}(E_{i₁} : E_{i₂}) = p(E_{i₂}F_j) r'(E_{i₁}F_j) / (p(E_{i₁}F_j) r'(E_{i₂}F_j))`. -/
theorem appendix27 {p : ι × κ → ℝ} (hp : ∀ x, 0 < p x) {a : ι → ℝ} (ha : ∀ i, 0 < a i)
    {b : κ → ℝ} (hb : ∀ j, 0 < b j) (i₁ i₂ : ι) (j : κ) :
    bfv (margE (jeffE (jeffF p b) a)) (margE (jeffF p b)) i₁ i₂ =
      p (i₂, j) * jeffE (jeffF p b) a (i₁, j) / (p (i₁, j) * jeffE (jeffF p b) a (i₂, j)) := by
  unfold bfv
  have hq1 := margE_pos (jeffF_pos hp hb) i₁ j
  have hq2 := margE_pos (jeffF_pos hp hb) i₂ j
  rw [margE_jeffE _ a i₁ hq1.ne', margE_jeffE _ a i₂ hq2.ne']
  have hF := margF_pos hp i₁ j
  have := ha i₁; have := ha i₂; have := hb j; have := hp (i₁, j); have := hp (i₂, j)
  simp only [jeffF, jeffE]
  field_simp

/-- **Wagner's theorem, identity (3)/(28)** (pp.12, 20): if the two orders agree on
the cells `EᵢFⱼ` (hypothesis (5)), the `E`-experience carries the same Bayes
factor in both orders, `β_{q,p}(Eᵢ : Eⱼ) = β_{r',q'}(Eᵢ : Eⱼ)`. -/
theorem wagner_E [Nonempty κ] {p : ι × κ → ℝ} (hp : ∀ x, 0 < p x)
    {a a' : ι → ℝ} (ha : ∀ i, 0 < a i) (ha' : ∀ i, 0 < a' i)
    {b b' : κ → ℝ} (hb : ∀ j, 0 < b j) (hb' : ∀ j, 0 < b' j)
    (h5 : jeffF (jeffE p a) b = jeffE (jeffF p b') a') (i₁ i₂ : ι) :
    bfv (margE (jeffE p a)) (margE p) i₁ i₂ =
      bfv (margE (jeffE (jeffF p b') a')) (margE (jeffF p b')) i₁ i₂ := by
  obtain ⟨j⟩ := ‹Nonempty κ›
  rw [appendix26 hp ha hb i₁ i₂ j, appendix27 hp ha' hb' i₁ i₂ j, h5]

/-- The `F`-side identity used for (4)/(29), order `p →E q →F r`. -/
theorem appendix26F {p : ι × κ → ℝ} (hp : ∀ x, 0 < p x) {a : ι → ℝ} (ha : ∀ i, 0 < a i)
    {b : κ → ℝ} (hb : ∀ j, 0 < b j) (i : ι) (j₁ j₂ : κ) :
    bfv (margF (jeffF (jeffE p a) b)) (margF (jeffE p a)) j₁ j₂ =
      p (i, j₂) * jeffF (jeffE p a) b (i, j₁) / (p (i, j₁) * jeffF (jeffE p a) b (i, j₂)) := by
  unfold bfv
  have hq1 := margF_pos (jeffE_pos hp ha) i j₁
  have hq2 := margF_pos (jeffE_pos hp ha) i j₂
  rw [margF_jeffF _ b j₁ hq1.ne', margF_jeffF _ b j₂ hq2.ne']
  have hE := margE_pos hp i j₁
  have := hb j₁; have := hb j₂; have := ha i; have := hp (i, j₁); have := hp (i, j₂)
  simp only [jeffF, jeffE]
  field_simp

/-- The `F`-side identity, order `p →F q' →E r'`. -/
theorem appendix27F {p : ι × κ → ℝ} (hp : ∀ x, 0 < p x) {a : ι → ℝ} (ha : ∀ i, 0 < a i)
    {b : κ → ℝ} (hb : ∀ j, 0 < b j) (i : ι) (j₁ j₂ : κ) :
    bfv (margF (jeffF p b)) (margF p) j₁ j₂ =
      p (i, j₂) * jeffE (jeffF p b) a (i, j₁) / (p (i, j₁) * jeffE (jeffF p b) a (i, j₂)) := by
  unfold bfv
  rw [margF_jeffF p b j₁ (margF_pos hp i j₁).ne', margF_jeffF p b j₂ (margF_pos hp i j₂).ne']
  have hq := margE_pos (jeffF_pos hp hb) i j₁
  have h1 := margF_pos hp i j₁
  have h2 := margF_pos hp i j₂
  have := hb j₁; have := hb j₂; have := ha i; have := hp (i, j₁); have := hp (i, j₂)
  simp only [jeffF, jeffE]
  field_simp

/-- **Wagner's theorem, identity (4)/(29)**: `β_{r,q}(Fᵢ : Fⱼ) = β_{q',p}(Fᵢ : Fⱼ)`. -/
theorem wagner_F [Nonempty ι] {p : ι × κ → ℝ} (hp : ∀ x, 0 < p x)
    {a a' : ι → ℝ} (ha : ∀ i, 0 < a i) (ha' : ∀ i, 0 < a' i)
    {b b' : κ → ℝ} (hb : ∀ j, 0 < b j) (hb' : ∀ j, 0 < b' j)
    (h5 : jeffF (jeffE p a) b = jeffE (jeffF p b') a') (j₁ j₂ : κ) :
    bfv (margF (jeffF (jeffE p a) b)) (margF (jeffE p a)) j₁ j₂ =
      bfv (margF (jeffF p b')) (margF p) j₁ j₂ := by
  obtain ⟨i⟩ := ‹Nonempty ι›
  rw [appendix26F hp ha hb i j₁ j₂, appendix27F hp ha' hb' i j₁ j₂, h5]

/-! ### The p.13 application, worked -/

/-- Prior with `p(E) = 1/10` (jellybean red), `p(F) = 1/5` (lighting tinted),
`E` and `F` independent. -/
noncomputable def jbPrior : Bool × Bool → ℝ :=
  fun x => (if x.1 then 1/10 else 9/10) * (if x.2 then 1/5 else 4/5)

/-- **p.13, exactly.**  Order 1: the jellybean experience takes `E` from `1/10`
to `9/10`, then the lighting experience takes `F` to `4/5`.  Order 2: the
lighting first (`F` to `4/5`), leaving `q'(E) = 1/10`; then the jellybean
experience, which must again carry Bayes factor `81`, so `r'(E) = 9/10`.  The
two orders end in the same state and the `E`-factor is `81` in both. -/
theorem jellybean_wagner :
    jeffF (jeffE jbPrior (fun i => if i then 9/10 else 1/10)) (fun j => if j then 4/5 else 1/5) =
      jeffE (jeffF jbPrior (fun j => if j then 4/5 else 1/5)) (fun i => if i then 9/10 else 1/10) ∧
    margE (jeffF jbPrior (fun j => if j then 4/5 else 1/5)) true = 1/10 ∧
    bfv (margE (jeffE jbPrior (fun i => if i then 9/10 else 1/10))) (margE jbPrior) true false = 81 ∧
    bfv (margE (jeffE (jeffF jbPrior (fun j => if j then 4/5 else 1/5))
        (fun i => if i then 9/10 else 1/10)))
      (margE (jeffF jbPrior (fun j => if j then 4/5 else 1/5))) true false = 81 := by
  refine ⟨?_, ?_, ?_, ?_⟩
  · funext ⟨x, y⟩
    cases x <;> cases y <;> simp [jeffE, jeffF, margE, margF, jbPrior] <;> norm_num
  · simp [jeffF, margE, margF, jbPrior]; norm_num
  · simp [bfv, jeffE, margE, jbPrior]; norm_num
  · simp [bfv, jeffE, jeffF, margE, margF, jbPrior]; norm_num

end Wagner

end Literature.Weisberg
