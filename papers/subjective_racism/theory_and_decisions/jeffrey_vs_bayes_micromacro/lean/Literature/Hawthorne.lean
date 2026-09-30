/-
# Hawthorne (2004), "Three Models of Sequential Belief Updating on Uncertain Evidence"

*Journal of Philosophical Logic* 33, 89-123.

Formalization of the paper's own definitions, theses, examples and theorems, not
of Paper B's claims.  Page numbers are the journal pagination.  Every displayed
formula used below was read on the rendered page, not from the text layer (which
drops his `ε` and his subscripts).

## Setting

A belief function is `p : Ω → ℝ` on a finite set of atoms.  An evidence basis
(p. 93, "a partition") is a labelling `u : Ω → ι`; the basis sentence `E_i` is
the cell `{x | u x = i}` and `Q[E_i]` is `cellMass p u i`.  A two-basis
conjunction `D_i · E_j` has mass `jointMass p u v i j`.

* **Basic Jeffrey Updating** (p. 93): `Q_e[S] = Σ_{i : Q[E_i] > 0} Q[S·E_i] ·
  (Q_e[E_i]/Q[E_i])`, on atoms `jeffrey p u q x = q (u x) · p x / Q[E_{u x}]`.
  Lean's `x / 0 = 0` is his restriction of the sum to `Q[E_i] > 0`.
* **Normed-Likelihood factor** (p. 95): `NL[Q, e, E_i] = Q_e[E_i]/Q[E_i]`, `NL`.
* **Likelihood-Ratio factor** (p. 104): `LR[Q, e, E_j, E_k] = NL[Q,e,E_j] /
  NL[Q,e,E_k]`, `LR`.  His note 20 records that Jeffrey calls these factors
  "Bayes factors", following Good.
* A state that supplies a factor vector `w` on its basis updates by
  `factorUpdate p u w x = w (u x) · p x / Σ_y w (u y) · p y`.  This is the form
  both the Normed-Likelihood model (Section 6) and the Likelihood-Ratio model
  (Section 7) take under Extended Rigidity: the normalising denominator is his
  (p. 102, p. 105).

## What is proved

**Sections 2-4 (basic updating and update factors).**
* `cellMass_jeffrey`: the updated basis takes the supplied values.
* `jeffrey_rigid`: rigidity, `Q_e[S | E_i] = Q[S | E_i]` (p. 93).
* `NL_jeffrey`, `jeffrey_eq_NL_mul`: `Q_e[E_j] = NL[Q,e,E_j] · Q[E_j]` (p. 95),
  on atoms and with no side condition.
* `basic_sequential`: the Basic Sequential Update Formula for two states (p. 94),
  which is also clause (1) of his Bases Decomposition Lemma (p. 118).

**Section 5 (the Amnestic Update Model = Standard Sequential Updating).**
* `amnestic_basis_value`, `amnestic_thesis`: the Amnestic Update-Factor Thesis
  (p. 96), `Q_αde[E_i] = Q_αe[E_i]` for *any* intervening transformation `d`;
  the latest update on a basis fixes that basis's values whatever came before
  (p. 97: they "do not depend on the initial belief function Q at all").
* `amnestic_same_basis`: two updates on one basis, the later one wins.
* `commute_of_jeffreyIndep`, `jeffreyIndep_of_commute`, `commutation_criterion`:
  the p. 97 criterion (credited in note 12 to Diaconis-Zabell Thm 3.2): two
  amnestic updates on distinct bases commute iff neither moves the other's basis
  marginal.  The "if" direction holds with no hypotheses.  The "only if"
  direction is proved for arbitrary finite bases (not only binary) under
  strictly positive targets and **every joint cell `Q[E_i · F_j] > 0`**.
* `criterion_needs_positive_cells`: without that last hypothesis the "only if"
  direction is false.  On a two-cell basis and a four-cell basis with
  block-diagonal support, the two amnestic updates commute although each moves
  the other's basis marginal.  Diaconis-Zabell's proof (their (3.5), "choose
  A = E_i0 F_j0") divides by `P(E_i0 F_j0)`; neither paper states the
  hypothesis.  Hawthorne's own Section 9 (compatibility classes) is what covers
  this case.

**The medical example (pp. 97-98), exactly, by `norm_num`.**
* `med_Qf_C = 7/50` (.14), `med_Qfe_C = 24689/36256` (≈ .681, printed .68),
  `med_Qe_C = 43/50` (.86), `med_Qef_C = 11567/36256` (≈ .319, printed .32).
* `med_overwrite`: the explicit overwrite `Q_e[E] = Q_fe[E] = .90` (p. 98).  The
  example has two distinct bases `{E,∼E}`, `{F,∼F}`, each cued once.
* `med_crossBasis`: the `F`-update alone moves `Q[E]` from `1/2` to `22/125`,
  and the `E`-update alone moves `Q[F]` to `103/125`, so the p. 97 criterion fails.
* `med_note14`: note 14's variant with likelihoods .99 gives
  `1477317/1778144 ≈ .831` and `300827/1778144 ≈ .169` (printed .83 and .17).

**Sections 6-7 (Normed-Likelihood and Likelihood-Ratio models).**
* `cellMass_factorUpdate`, `NL_factorUpdate`, `LR_factorUpdate`: a supplied factor
  vector `w` gives `NL = w_j / Z` (Z the normaliser) and `LR = w_j / w_k`,
  independent of the prior (p. 103: the LR term "may take any non-negative
  value, with no regard for the values of the prior probabilities").
* `LR_ratio`: the defining equation of LR factors (p. 104).
* `factorUpdate_smul`: only ratios of a factor vector matter, so NL factors and
  LR factors induce the same revision on a basis of **any** size.
* `jeffrey_eq_factorUpdate`: a Basic Jeffrey update is the factor update with
  its own NL factors (p. 96: each kind of factor generates the other from the
  prior).
* `factorUpdate_seq`: the Extended Sequential Update Formula for two bases
  (pp. 102, 105), **with its normalising denominator**; `factorUpdate_comm`: the
  two orders agree.
* `extendedRigidity_of_factor`: a state that supplies the same factors in either
  context satisfies the NL Extended Rigidity Thesis (p. 100), with
  `r = Z_α / Z_αd`.
* `erLR_iff_erNL`: the LR version of Extended Rigidity (p. 104) is the NL version.
* `extendedRigidity_comm`: **what exactly is needed**.  For two Basic Jeffrey
  updates on distinct bases, with targets allowed to depend on the order,
  Extended Rigidity in both orders makes them commute.  Only positivity and the
  probability axioms are used.  The two constants `r, r'` are forced equal by
  normalisation.
* `med_LR_*`: the LR example (p. 107) with `LR[f,F,∼F] = .50` and
  `LR[e,E,∼E] = 2`: `Q_f[C] = 7/20`, `Q_fe[C] = Q_ef[C] = 1/2`.
  `med_bayesFactorReading`: the LR factors implicit in the amnestic reports
  (.90 read against the prior 1/2) are `1/9` and `9`, not `.50` and `2`.  With
  those the result is again `1/2` in both orders (`med_LR_ninths`).
* `med_NL_denominator`: the NL factors computed against the initial prior
  (`Q_e[E_i]/Q[E_i]`) give `Q[C] = 1/2` after normalisation.  The denominator is
  `301/625`, not 1, so the product `P · ℓ^A · ℓ^B` without it is not a
  probability.  `nl_denominator_indep`: the denominator is 1 when the two bases
  are independent under the prior.

**Section 8 (Basis-Overwrite and Basis-Commuting versions).**
`extUpdate` is the Extended Sequential Update Formula (LR version, p. 105) with
one factor `Λ k γ` for each basis-homogeneous subsequence `γ`.  The factor need
not decompose into per-state factors (Hawthorne's reply to Garber, pp. 109-110).
* `extUpdate_suitable`: reorderings that keep the order within each basis agree
  ("independent of update order -- except, perhaps, within basis homogeneous
  subsequences", pp. 103, 105).
* `extUpdate_basisOverwrite`: Basis-Overwrite Version, where only the last state
  on each basis counts.
* `extUpdate_basisCommuting`: Basis-Commuting Version, where the factor is
  invariant under reordering within a basis.  Then **every** reordering agrees
  ("completely order-independent", p. 108).
* `seqFactor_eq`, `seqFactor_perm`: the decomposable (Field) case.  Sequential
  factor updating equals the extended formula with product factors, and every
  permutation agrees.

**Section 9 (Update Reordering Theorem), two-state finite case.**
* `commutation_theorem`: his Commutation Theorem (p. 118), the two-state core of
  the Update Reordering Theorem (p. 112).  `d` (basis `u`) and `ε` (basis `v`)
  are Basic Jeffrey updates whose targets may depend on the order.  They commute
  iff for each `Q_αdε`-possible `D_i`, with **his** `r = NL[Q_αε, d, D_i] /
  NL[Q_α, d, D_i] > 0`, every `E_j` that is `Q_αdε`-compatible with `D_i`
  satisfies `NL[Q_αd, ε, E_j] = r · NL[Q_α, ε, E_j]`.  Hypotheses: nonnegative
  prior and strictly positive targets, which give his "plausible principle"
  (p. 119) for free.
* `commutation_append`: appending any later sequence `β` preserves agreement
  (p. 120).
* Block-diagonal example (`blk_*`): `D ∈ {1,2}`, `E ∈ {1,..,4}`, `D=1` compatible
  only with `E ∈ {1,2}`.  The updates commute for **every** within-block split
  of the `E` targets (`blk_commute`).  His `r` is `2/5` on the first class and
  `14/5` on the second, whatever the split (`blk_r`).  Clause (2) holds with those
  values (`blk_clause2`), but NL Extended Rigidity fails: no single `r` works
  (`blk_not_extendedRigidity`).  This is his "somewhat weaker" condition
  (pp. 112-113) as a verified instance, and `r` is his ratio, fixed by `d`'s
  factors.

## Not formalized

The Commutation Reduction Theorem (p. 117), which reduces arbitrary suitable
reorderings of long sequences to adjacent swaps, and hence the Reordering
Theorem for sequences of arbitrary length.  Countable bases (note 3).  The
psychological and normative assessments (pp. 97-99, 115-116).  The claim
(p. 109) that `n` identical glances under within-basis Extended Rigidity drive
the update to certainty is not formalized; only the decomposition it rests on
(`seqFactor_eq`) is.
-/
import Mathlib

namespace Literature.Hawthorne

open Finset

section Basic

variable {Ω ι κ : Type*} [Fintype Ω] [DecidableEq ι] [DecidableEq κ]

/-- `Q[E_i]`: the mass of basis sentence `E_i = {x | u x = i}`. -/
def cellMass (p : Ω → ℝ) (u : Ω → ι) (i : ι) : ℝ :=
  ∑ y, if u y = i then p y else 0

/-- `Q[D_i · E_j]`: the mass of a conjunction of sentences from two bases. -/
def jointMass (p : Ω → ℝ) (u : Ω → ι) (v : Ω → κ) (i : ι) (j : κ) : ℝ :=
  ∑ y, if u y = i ∧ v y = j then p y else 0

/-- **Basic Jeffrey Updating** (p. 93): `Q_e[S] = Σ_{i:Q[E_i]>0} Q[S·E_i] ·
(Q_e[E_i]/Q[E_i])`, written on atoms.  `q i` is the supplied `Q_e[E_i]`. -/
noncomputable def jeffrey (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ) : Ω → ℝ :=
  fun x => q (u x) * p x / cellMass p u (u x)

/-- **Normed-Likelihood update factor** (p. 95): `NL[Q, e, E_i] = Q_e[E_i]/Q[E_i]`,
for the prior `p = Q` and the updated `p' = Q_e`. -/
noncomputable def NL (p p' : Ω → ℝ) (u : Ω → ι) (i : ι) : ℝ :=
  cellMass p' u i / cellMass p u i

/-- **Likelihood-Ratio update factor** (p. 104): `LR[Q, e, E_j, E_k] =
NL[Q,e,E_j] / NL[Q,e,E_k]`.  Note 20: Jeffrey calls these "Bayes factors". -/
noncomputable def LR (p p' : Ω → ℝ) (u : Ω → ι) (j k : ι) : ℝ :=
  NL p p' u j / NL p p' u k

/-- Update by a supplied factor vector `w` on basis `u`, normalised (the form of
both Extended Sequential Update Formulas, pp. 102, 105). -/
noncomputable def factorUpdate (p : Ω → ℝ) (u : Ω → ι) (w : ι → ℝ) : Ω → ℝ :=
  fun x => w (u x) * p x / ∑ y, w (u y) * p y

theorem cellMass_nonneg {p : Ω → ℝ} (hp : ∀ x, 0 ≤ p x) (u : Ω → ι) (i : ι) :
    0 ≤ cellMass p u i :=
  sum_nonneg fun y _ => by split_ifs <;> simp [hp y]

theorem le_cellMass {p : Ω → ℝ} (hp : ∀ x, 0 ≤ p x) (u : Ω → ι) (x : Ω) :
    p x ≤ cellMass p u (u x) := by
  have := Finset.single_le_sum (f := fun y => if u y = u x then p y else 0)
    (fun y _ => by split_ifs <;> simp [hp y]) (Finset.mem_univ x)
  simpa [cellMass] using this

theorem le_jointMass {p : Ω → ℝ} (hp : ∀ x, 0 ≤ p x) (u : Ω → ι) (v : Ω → κ) (x : Ω) :
    p x ≤ jointMass p u v (u x) (v x) := by
  have := Finset.single_le_sum (f := fun y => if u y = u x ∧ v y = v x then p y else 0)
    (fun y _ => by split_ifs <;> simp [hp y]) (Finset.mem_univ x)
  simpa [jointMass] using this

theorem jointMass_le_cellMass_left {p : Ω → ℝ} (hp : ∀ x, 0 ≤ p x) (u : Ω → ι) (v : Ω → κ)
    (i : ι) (j : κ) : jointMass p u v i j ≤ cellMass p u i :=
  Finset.sum_le_sum fun y _ => by
    by_cases h1 : u y = i <;> by_cases h2 : v y = j <;> simp [h1, h2, hp y]

theorem jointMass_le_cellMass_right {p : Ω → ℝ} (hp : ∀ x, 0 ≤ p x) (u : Ω → ι) (v : Ω → κ)
    (i : ι) (j : κ) : jointMass p u v i j ≤ cellMass p v j :=
  Finset.sum_le_sum fun y _ => by
    by_cases h1 : u y = i <;> by_cases h2 : v y = j <;> simp [h1, h2, hp y]

/-- A positive-mass conjunction contains a positive atom. -/
theorem exists_pos_of_jointMass_pos {p : Ω → ℝ} {u : Ω → ι} {v : Ω → κ} {i : ι} {j : κ}
    (h : 0 < jointMass p u v i j) : ∃ x, u x = i ∧ v x = j ∧ 0 < p x := by
  by_contra hne
  push Not at hne
  have : jointMass p u v i j ≤ 0 :=
    Finset.sum_nonpos fun y _ => by
      split_ifs with hy
      · exact hne y hy.1 hy.2
      · exact le_rfl
  linarith

theorem sum_cellMass [Fintype ι] (p : Ω → ℝ) (u : Ω → ι) :
    ∑ i, cellMass p u i = ∑ x, p x := by
  unfold cellMass
  rw [Finset.sum_comm]
  simp

theorem jeffrey_nonneg {p : Ω → ℝ} (hp : ∀ x, 0 ≤ p x) (u : Ω → ι) {q : ι → ℝ}
    (hq : ∀ i, 0 ≤ q i) (x : Ω) : 0 ≤ jeffrey p u q x :=
  div_nonneg (mul_nonneg (hq _) (hp x)) (cellMass_nonneg hp u _)

theorem jeffrey_apply (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ) (x : Ω) :
    jeffrey p u q x = q (u x) * p x / cellMass p u (u x) := rfl

/-- The updated basis takes the supplied values: `Q_e[E_i] = q_i` (p. 93). -/
theorem cellMass_jeffrey (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ) (i : ι)
    (h : cellMass p u i ≠ 0) : cellMass (jeffrey p u q) u i = q i := by
  have key : cellMass (jeffrey p u q) u i = q i / cellMass p u i * cellMass p u i := by
    conv_rhs => rw [cellMass, Finset.mul_sum]
    unfold cellMass jeffrey
    refine Finset.sum_congr rfl fun y _ => ?_
    by_cases hy : u y = i
    · simp only [hy, if_true]; unfold cellMass; ring
    · simp [hy]
  rw [key]; field_simp

/-- A basis sentence of zero prior mass keeps zero mass (Hawthorne's note 4). -/
theorem cellMass_jeffrey_zero (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ) (i : ι)
    (h : cellMass p u i = 0) : cellMass (jeffrey p u q) u i = 0 := by
  show (∑ y, if u y = i then jeffrey p u q y else 0) = 0
  refine Finset.sum_eq_zero fun y _ => ?_
  split_ifs with hy
  · rw [jeffrey_apply, hy, h, div_zero]
  · rfl

/-- **Rigidity** (p. 93): `Q_e[S | E_i] = Q[S | E_i]`, on atoms. -/
theorem jeffrey_rigid (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ) (x : Ω)
    (hm : cellMass p u (u x) ≠ 0) (hq : q (u x) ≠ 0) :
    jeffrey p u q x / cellMass (jeffrey p u q) u (u x) = p x / cellMass p u (u x) := by
  rw [cellMass_jeffrey p u q (u x) hm, jeffrey_apply]
  field_simp

/-- The NL factor of a Basic Jeffrey update is the supplied value over the prior
value, `NL[Q,e,E_i] = Q_e[E_i]/Q[E_i]` (p. 95).  No side condition is needed. -/
theorem NL_jeffrey (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ) (i : ι) :
    NL p (jeffrey p u q) u i = q i / cellMass p u i := by
  unfold NL
  by_cases h : cellMass p u i = 0
  · rw [cellMass_jeffrey_zero p u q i h, h]; simp
  · rw [cellMass_jeffrey p u q i h]

/-- `Q_βe[E_j] = NL[Q_β, e, E_j] · Q_β[E_j]` (p. 95), on atoms. -/
theorem jeffrey_eq_NL_mul (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ) (x : Ω) :
    jeffrey p u q x = NL p (jeffrey p u q) u (u x) * p x := by
  rw [NL_jeffrey, jeffrey_apply]; ring

/-- **Basic Sequential Update Formula** (p. 94), two states `e` (basis `u`) and
`f` (basis `v`), on atoms: `Q_ef[x] = Q[x] · NL[Q, e, E_i] · NL[Q_e, f, F_j]`.
This is also clause (1) of the Bases Decomposition Lemma (p. 118). -/
theorem basic_sequential (p : Ω → ℝ) (u : Ω → ι) (v : Ω → κ) (q : ι → ℝ) (r : κ → ℝ)
    (x : Ω) :
    jeffrey (jeffrey p u q) v r x =
      p x * NL p (jeffrey p u q) u (u x) * NL (jeffrey p u q) (jeffrey (jeffrey p u q) v r) v (v x) := by
  rw [jeffrey_eq_NL_mul (jeffrey p u q) v r x, jeffrey_eq_NL_mul p u q x]; ring

end Basic

/-! ## Section 5: the Amnestic Update Model -/

section Amnestic

variable {Ω ι κ : Type*} [Fintype Ω] [DecidableEq ι] [DecidableEq κ]

/-- The belief strengths for basis sentences after an amnestic update are the
supplied values, whatever the prior (p. 97: they "do not depend on the initial
belief function Q at all, but only on the experiential states themselves"). -/
theorem amnestic_basis_value (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ) (i : ι)
    (h : cellMass p u i ≠ 0) : cellMass (jeffrey p u q) u i = q i :=
  cellMass_jeffrey p u q i h

/-- **Amnestic Update-Factor Thesis** (p. 96): `Q_αde[E_i] = Q_αe[E_i]` for any
state `d` and sequence `α`.  Here `p` is `Q_α` and `d` is an *arbitrary*
transformation of belief functions, so the latest update on a basis fixes that
basis's values whatever came before. -/
theorem amnestic_thesis (p : Ω → ℝ) (d : (Ω → ℝ) → (Ω → ℝ)) (u : Ω → ι) (q : ι → ℝ)
    (i : ι) (h1 : cellMass (d p) u i ≠ 0) (h2 : cellMass p u i ≠ 0) :
    cellMass (jeffrey (d p) u q) u i = cellMass (jeffrey p u q) u i := by
  rw [cellMass_jeffrey _ u q i h1, cellMass_jeffrey _ u q i h2]

/-- Two amnestic updates on one basis: the later one wins. -/
theorem amnestic_same_basis (p : Ω → ℝ) (u : Ω → ι) (q r : ι → ℝ)
    (hm : ∀ x, cellMass p u (u x) ≠ 0) (hq : ∀ x, q (u x) ≠ 0) :
    jeffrey (jeffrey p u q) u r = jeffrey p u r := by
  funext x
  rw [jeffrey_apply, cellMass_jeffrey p u q (u x) (hm x), jeffrey_apply, jeffrey_apply]
  have := hm x; have := hq x
  field_simp

/-- **Commutation criterion, "if"** (p. 97, note 12): if neither update moves the
other's basis marginal (`Q_βf[E_i] = Q_β[E_i]`, `Q_βe[F_j] = Q_β[F_j]`), the two
amnestic updates commute.  No further hypothesis. -/
theorem commute_of_jeffreyIndep (p : Ω → ℝ) (u : Ω → ι) (v : Ω → κ) (q : ι → ℝ)
    (r : κ → ℝ)
    (hE : ∀ i, cellMass (jeffrey p v r) u i = cellMass p u i)
    (hF : ∀ j, cellMass (jeffrey p u q) v j = cellMass p v j) :
    jeffrey (jeffrey p u q) v r = jeffrey (jeffrey p v r) u q := by
  funext x
  rw [jeffrey_apply, jeffrey_apply (jeffrey p v r), hF, hE, jeffrey_apply, jeffrey_apply]
  ring

omit [DecidableEq κ] in
private theorem sum_cellMass_jeffrey [Fintype ι] (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ)
    (hm : ∀ i, cellMass p u i ≠ 0) (hq1 : ∑ i, q i = 1) :
    ∑ x, jeffrey p u q x = 1 := by
  rw [← sum_cellMass (jeffrey p u q) u, ← hq1]
  exact Finset.sum_congr rfl fun i _ => cellMass_jeffrey p u q i (hm i)

/-- **Commutation criterion, "only if"** (p. 97, note 12; Diaconis-Zabell
Thm 3.2), for arbitrary finite bases.  Hypotheses: probability prior, strictly
positive targets summing to one, and **every conjunction `E_i · F_j` of positive
prior mass**.  Without the last hypothesis the statement is false
(`criterion_needs_positive_cells`). -/
theorem jeffreyIndep_of_commute [Fintype ι] [Fintype κ] (p : Ω → ℝ) (u : Ω → ι)
    (v : Ω → κ) (q : ι → ℝ) (r : κ → ℝ)
    (hp : ∀ x, 0 ≤ p x) (hp1 : ∑ x, p x = 1)
    (hq : ∀ i, 0 < q i) (hr : ∀ j, 0 < r j) (hq1 : ∑ i, q i = 1) (hr1 : ∑ j, r j = 1)
    (hcell : ∀ i j, 0 < jointMass p u v i j)
    (hcomm : jeffrey (jeffrey p u q) v r = jeffrey (jeffrey p v r) u q) :
    (∀ i, cellMass (jeffrey p v r) u i = cellMass p u i) ∧
      (∀ j, cellMass (jeffrey p u q) v j = cellMass p v j) := by
  set pe := jeffrey p u q with hpe
  set pf := jeffrey p v r with hpf
  have hmu : ∀ i, 0 < cellMass p u i := fun i => by
    obtain ⟨j⟩ : Nonempty κ := by
      by_contra h; rw [not_nonempty_iff] at h; simp at hr1
    exact lt_of_lt_of_le (hcell i j) (jointMass_le_cellMass_left hp u v i j)
  have hmv : ∀ j, 0 < cellMass p v j := fun j => by
    obtain ⟨i⟩ : Nonempty ι := by
      by_contra h; rw [not_nonempty_iff] at h; simp at hq1
    exact lt_of_lt_of_le (hcell i j) (jointMass_le_cellMass_right hp u v i j)
  have hpe0 : ∀ x, 0 ≤ pe x := jeffrey_nonneg hp u fun i => (hq i).le
  have hpf0 : ∀ x, 0 ≤ pf x := jeffrey_nonneg hp v fun j => (hr j).le
  -- the key identity on each joint cell
  have key : ∀ i j, cellMass p u i * cellMass pe v j = cellMass p v j * cellMass pf u i := by
    intro i j
    obtain ⟨x, hxu, hxv, hx⟩ := exists_pos_of_jointMass_pos (hcell i j)
    have hpex : 0 < pe x := by
      rw [hpe, jeffrey_apply, hxu]; exact div_pos (mul_pos (hq i) hx) (hmu i)
    have hpfx : 0 < pf x := by
      rw [hpf, jeffrey_apply, hxv]; exact div_pos (mul_pos (hr j) hx) (hmv j)
    have hme : 0 < cellMass pe v j := hxv ▸ lt_of_lt_of_le hpex (le_cellMass hpe0 v x)
    have hmf : 0 < cellMass pf u i := hxu ▸ lt_of_lt_of_le hpfx (le_cellMass hpf0 u x)
    have h := congrFun hcomm x
    rw [jeffrey_apply, jeffrey_apply] at h
    have hpe_x : pe x = q i * p x / cellMass p u i := by rw [hpe, jeffrey_apply, hxu]
    have hpf_x : pf x = r j * p x / cellMass p v j := by rw [hpf, jeffrey_apply, hxv]
    rw [hpe_x, hpf_x, hxu, hxv] at h
    have hmu' := (hmu i).ne'; have hmv' := (hmv j).ne'
    have hq' := (hq i).ne'; have hr' := (hr j).ne'; have hx' := hx.ne'
    field_simp at h
    linarith
  have hsum_pe : ∑ j, cellMass pe v j = 1 := by
    rw [sum_cellMass]; exact sum_cellMass_jeffrey p u q (fun i => (hmu i).ne') hq1
  have hsum_pf : ∑ i, cellMass pf u i = 1 := by
    rw [sum_cellMass]; exact sum_cellMass_jeffrey p v r (fun j => (hmv j).ne') hr1
  have hsum_v : ∑ j, cellMass p v j = 1 := by rw [sum_cellMass]; exact hp1
  have hsum_u : ∑ i, cellMass p u i = 1 := by rw [sum_cellMass]; exact hp1
  constructor
  · intro i
    have := Finset.sum_congr rfl (fun j (_ : j ∈ Finset.univ) => key i j)
    rw [← Finset.mul_sum, ← Finset.sum_mul, hsum_pe, hsum_v] at this
    linarith
  · intro j
    have := Finset.sum_congr rfl (fun i (_ : i ∈ Finset.univ) => key i j)
    rw [← Finset.sum_mul, ← Finset.mul_sum, hsum_u, hsum_pf] at this
    linarith

/-- **The p. 97 commutation criterion** as an equivalence (finite bases,
positive targets, every joint cell of positive prior mass). -/
theorem commutation_criterion [Fintype ι] [Fintype κ] (p : Ω → ℝ) (u : Ω → ι)
    (v : Ω → κ) (q : ι → ℝ) (r : κ → ℝ)
    (hp : ∀ x, 0 ≤ p x) (hp1 : ∑ x, p x = 1)
    (hq : ∀ i, 0 < q i) (hr : ∀ j, 0 < r j) (hq1 : ∑ i, q i = 1) (hr1 : ∑ j, r j = 1)
    (hcell : ∀ i j, 0 < jointMass p u v i j) :
    jeffrey (jeffrey p u q) v r = jeffrey (jeffrey p v r) u q ↔
      (∀ i, cellMass (jeffrey p v r) u i = cellMass p u i) ∧
        (∀ j, cellMass (jeffrey p u q) v j = cellMass p v j) :=
  ⟨jeffreyIndep_of_commute p u v q r hp hp1 hq hr hq1 hr1 hcell,
    fun h => commute_of_jeffreyIndep p u v q r h.1 h.2⟩

end Amnestic

/-! ## Sections 6-7: Normed-Likelihood and Likelihood-Ratio factor models -/

section Factor

variable {Ω ι κ : Type*} [Fintype Ω] [DecidableEq ι] [DecidableEq κ]

omit [DecidableEq ι] in
theorem factorUpdate_apply (p : Ω → ℝ) (u : Ω → ι) (w : ι → ℝ) (x : Ω) :
    factorUpdate p u w x = w (u x) * p x / ∑ y, w (u y) * p y := rfl

/-- The new mass of `E_i` under a factor update: `w_i · Q[E_i] / Z`. -/
theorem cellMass_factorUpdate (p : Ω → ℝ) (u : Ω → ι) (w : ι → ℝ) (i : ι) :
    cellMass (factorUpdate p u w) u i = w i * cellMass p u i / ∑ y, w (u y) * p y := by
  unfold cellMass factorUpdate
  rw [Finset.mul_sum, Finset.sum_div]
  refine Finset.sum_congr rfl fun y _ => ?_
  split_ifs with hy
  · rw [hy]
  · simp

/-- The NL factor of a factor update is the supplied weight over the normaliser
(p. 95, p. 100). -/
theorem NL_factorUpdate (p : Ω → ℝ) (u : Ω → ι) (w : ι → ℝ) (i : ι)
    (h : cellMass p u i ≠ 0) :
    NL p (factorUpdate p u w) u i = w i / ∑ y, w (u y) * p y := by
  unfold NL; rw [cellMass_factorUpdate]; field_simp

/-- The LR factor of a factor update is the ratio of the supplied weights,
whatever the prior (p. 103). -/
theorem LR_factorUpdate (p : Ω → ℝ) (u : Ω → ι) (w : ι → ℝ) (j k : ι)
    (hj : cellMass p u j ≠ 0) (hk : cellMass p u k ≠ 0) (hZ : ∑ y, w (u y) * p y ≠ 0) :
    LR p (factorUpdate p u w) u j k = w j / w k := by
  unfold LR; rw [NL_factorUpdate p u w j hj, NL_factorUpdate p u w k hk]
  exact div_div_div_cancel_right₀ hZ _ _

/-- The defining equation of LR factors (p. 104):
`Q_βε[E_j]/Q_βε[E_k] = LR[Q_β, ε, E_j, E_k] · (Q_β[E_j]/Q_β[E_k])`. -/
theorem LR_ratio (p p' : Ω → ℝ) (u : Ω → ι) (j k : ι)
    (hj : cellMass p u j ≠ 0) (hk : cellMass p u k ≠ 0) (hk' : cellMass p' u k ≠ 0) :
    cellMass p' u j / cellMass p' u k = LR p p' u j k * (cellMass p u j / cellMass p u k) := by
  unfold LR NL; field_simp

omit [DecidableEq ι] in
/-- Only the ratios of a factor vector matter: rescaling leaves the update
unchanged.  So NL factors and the LR factors built from them (`w_j / w_k`) induce
the same revision, on a basis of any size. -/
theorem factorUpdate_smul (p : Ω → ℝ) (u : Ω → ι) (w : ι → ℝ) {c : ℝ} (hc : c ≠ 0) :
    factorUpdate p u (fun i => c * w i) = factorUpdate p u w := by
  funext x
  unfold factorUpdate
  have : ∑ y, c * w (u y) * p y = c * ∑ y, w (u y) * p y := by
    rw [Finset.mul_sum]; exact Finset.sum_congr rfl fun y _ => by ring
  rw [this, mul_assoc, mul_div_mul_left _ _ hc]

/-- A Basic Jeffrey update is the factor update with its own NL factors (p. 96:
each kind of factor "may be used to generate the other from the prior
probabilities of basis sentences"). -/
theorem jeffrey_eq_factorUpdate [Fintype ι] (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ)
    (hm : ∀ i, cellMass p u i ≠ 0) (hq1 : ∑ i, q i = 1) :
    jeffrey p u q = factorUpdate p u (fun i => q i / cellMass p u i) := by
  have hZ : ∑ y, q (u y) / cellMass p u (u y) * p y = 1 := by
    have := sum_cellMass_jeffrey p u q hm hq1
    rw [← this]
    exact Finset.sum_congr rfl fun y _ => by rw [jeffrey_apply]; ring
  funext x
  rw [factorUpdate_apply, hZ, div_one, jeffrey_apply]; ring

omit [DecidableEq ι] [DecidableEq κ] in
/-- **Extended Sequential Update Formula** (pp. 102, 105) for two bases: the two
factor updates compose to one update on the product of the factors, **with the
normalising denominator**. -/
theorem factorUpdate_seq (p : Ω → ℝ) (u : Ω → ι) (v : Ω → κ) (w : ι → ℝ) (z : κ → ℝ)
    (hZ : ∑ y, w (u y) * p y ≠ 0) :
    factorUpdate (factorUpdate p u w) v z =
      fun x => w (u x) * z (v x) * p x / ∑ y, w (u y) * z (v y) * p y := by
  funext x
  unfold factorUpdate
  have h1 : ∑ y, z (v y) * (w (u y) * p y / ∑ y', w (u y') * p y') =
      (∑ y, w (u y) * z (v y) * p y) / ∑ y', w (u y') * p y' := by
    rw [Finset.sum_div]; exact Finset.sum_congr rfl fun y _ => by ring
  rw [h1, show z (v x) * (w (u x) * p x / ∑ y', w (u y') * p y') =
      (w (u x) * z (v x) * p x) / ∑ y', w (u y') * p y' by ring]
  exact div_div_div_cancel_right₀ hZ _ _

omit [DecidableEq ι] [DecidableEq κ] in
/-- **Factor updates on distinct bases commute** (pp. 103, 105; p. 107: "whatever
values the update factors ... may have, update order will produce net no
effect"). -/
theorem factorUpdate_comm (p : Ω → ℝ) (u : Ω → ι) (v : Ω → κ) (w : ι → ℝ) (z : κ → ℝ)
    (hw : ∑ y, w (u y) * p y ≠ 0) (hz : ∑ y, z (v y) * p y ≠ 0) :
    factorUpdate (factorUpdate p u w) v z = factorUpdate (factorUpdate p v z) u w := by
  rw [factorUpdate_seq p u v w z hw, factorUpdate_seq p v u z w hz]
  funext x
  have : ∑ y, z (v y) * w (u y) * p y = ∑ y, w (u y) * z (v y) * p y :=
    Finset.sum_congr rfl fun y _ => by ring
  rw [this]; ring

/-- A state that supplies the same factor vector `w` whatever precedes it
satisfies the **NL Extended Rigidity Thesis** (p. 100): there is `r > 0` with
`NL[Q_αd, ε, E_j] = r · NL[Q_α, ε, E_j]` for every relevant `E_j`.  Here
`p = Q_α`, `pd = Q_αd`, and `r = Z_α / Z_αd` is the ratio of normalisers. -/
theorem extendedRigidity_of_factor (p pd : Ω → ℝ) (v : Ω → κ) (w : κ → ℝ)
    (hZ : 0 < ∑ y, w (v y) * p y) (hZd : 0 < ∑ y, w (v y) * pd y) :
    ∃ r > 0, ∀ j, cellMass p v j ≠ 0 → cellMass pd v j ≠ 0 →
      NL pd (factorUpdate pd v w) v j = r * NL p (factorUpdate p v w) v j := by
  refine ⟨(∑ y, w (v y) * p y) / ∑ y, w (v y) * pd y, div_pos hZ hZd, fun j hj hjd => ?_⟩
  rw [NL_factorUpdate _ _ _ _ hj, NL_factorUpdate _ _ _ _ hjd]
  field_simp

/-- The **LR version of Extended Rigidity** (p. 104: `LR[Q_αd, ε, E_j, E_k] =
LR[Q_α, ε, E_j, E_k]` against a fixed `E_k`) is the **NL version** (p. 100). -/
theorem erLR_iff_erNL {κ : Type*} (a b : κ → ℝ) (k : κ) (ha : a k ≠ 0) (hb : b k ≠ 0) :
    (∀ j, b j / b k = a j / a k) ↔ ∃ r, ∀ j, b j = r * a j := by
  constructor
  · intro h
    refine ⟨b k / a k, fun j => ?_⟩
    have := h j
    field_simp at this ⊢
    linear_combination this
  · rintro ⟨r, hr⟩ j
    have hr0 : r ≠ 0 := by rintro rfl; exact hb (by simpa using hr k)
    rw [hr j, hr k]; field_simp

/-- **Extended Rigidity in both orders makes updates on distinct bases commute**
(Sections 6-7).  `d` (basis `u`) and `ε` (basis `v`) are Basic Jeffrey updates of
`p = Q_α`.  Their targets may depend on the order: `sd = Q_αd[D_i]`,
`sdε = Q_αεd[D_i]`, `tε = Q_αε[E_j]`, `tεd = Q_αdε[E_j]`.  Assumed: nonnegative
prior, positive targets, both results probability functions, and the Extended
Rigidity Thesis for `ε` after `d` (constant `r`) and for `d` after `ε`
(constant `r'`).  Normalisation forces `r = r'`. -/
theorem extendedRigidity_comm (p : Ω → ℝ) (u : Ω → ι) (v : Ω → κ) (sd sdε : ι → ℝ)
    (tε tεd : κ → ℝ) (hp : ∀ x, 0 ≤ p x)
    (hsd : ∀ i, 0 < sd i) (hsdε : ∀ i, 0 < sdε i) (htε : ∀ j, 0 < tε j)
    (htεd : ∀ j, 0 < tεd j)
    (h1 : ∑ x, jeffrey (jeffrey p u sd) v tεd x = 1)
    (h2 : ∑ x, jeffrey (jeffrey p v tε) u sdε x = 1)
    (hERε : ∃ r, ∀ j, 0 < cellMass (jeffrey (jeffrey p u sd) v tεd) v j →
      NL (jeffrey p u sd) (jeffrey (jeffrey p u sd) v tεd) v j = r * NL p (jeffrey p v tε) v j)
    (hERd : ∃ r', ∀ i, 0 < cellMass (jeffrey (jeffrey p v tε) u sdε) u i →
      NL (jeffrey p v tε) (jeffrey (jeffrey p v tε) u sdε) u i = r' * NL p (jeffrey p u sd) u i) :
    jeffrey (jeffrey p u sd) v tεd = jeffrey (jeffrey p v tε) u sdε := by
  obtain ⟨r, hr⟩ := hERε
  obtain ⟨r', hr'⟩ := hERd
  set pd := jeffrey p u sd with hpd
  set pε := jeffrey p v tε with hpε
  set g : Ω → ℝ := fun x => p x * (sd (u x) / cellMass p u (u x)) * (tε (v x) / cellMass p v (v x))
    with hg
  have hpd0 : ∀ x, 0 ≤ pd x := jeffrey_nonneg hp u fun i => (hsd i).le
  have hpε0 : ∀ x, 0 ≤ pε x := jeffrey_nonneg hp v fun j => (htε j).le
  have hR1 : ∀ x, jeffrey pd v tεd x = r * g x := by
    intro x
    by_cases hx : p x = 0
    · simp [hg, jeffrey_apply, hpd, hx]
    have hx' : 0 < p x := lt_of_le_of_ne (hp x) (Ne.symm hx)
    have hmu : 0 < cellMass p u (u x) := lt_of_lt_of_le hx' (le_cellMass hp u x)
    have hpdx : 0 < pd x := by
      rw [hpd, jeffrey_apply]; exact div_pos (mul_pos (hsd _) hx') hmu
    have hmd : 0 < cellMass pd v (v x) := lt_of_lt_of_le hpdx (le_cellMass hpd0 v x)
    have hR1x : 0 < jeffrey pd v tεd x := by
      rw [jeffrey_apply]; exact div_pos (mul_pos (htεd _) hpdx) hmd
    have hpos : 0 < cellMass (jeffrey pd v tεd) v (v x) :=
      lt_of_lt_of_le hR1x (le_cellMass (jeffrey_nonneg hpd0 v fun j => (htεd j).le) v x)
    have e := hr (v x) hpos
    rw [NL_jeffrey, NL_jeffrey] at e
    rw [jeffrey_apply, hg]
    simp only
    rw [show tεd (v x) * pd x / cellMass pd v (v x) = tεd (v x) / cellMass pd v (v x) * pd x
      by ring, e, hpd, jeffrey_apply]
    ring
  have hR2 : ∀ x, jeffrey pε u sdε x = r' * g x := by
    intro x
    by_cases hx : p x = 0
    · simp [hg, jeffrey_apply, hpε, hx]
    have hx' : 0 < p x := lt_of_le_of_ne (hp x) (Ne.symm hx)
    have hmv : 0 < cellMass p v (v x) := lt_of_lt_of_le hx' (le_cellMass hp v x)
    have hpεx : 0 < pε x := by
      rw [hpε, jeffrey_apply]; exact div_pos (mul_pos (htε _) hx') hmv
    have hmε : 0 < cellMass pε u (u x) := lt_of_lt_of_le hpεx (le_cellMass hpε0 u x)
    have hR2x : 0 < jeffrey pε u sdε x := by
      rw [jeffrey_apply]; exact div_pos (mul_pos (hsdε _) hpεx) hmε
    have hpos : 0 < cellMass (jeffrey pε u sdε) u (u x) :=
      lt_of_lt_of_le hR2x (le_cellMass (jeffrey_nonneg hpε0 u fun i => (hsdε i).le) u x)
    have e := hr' (u x) hpos
    rw [NL_jeffrey, NL_jeffrey] at e
    rw [jeffrey_apply, hg]
    simp only
    rw [show sdε (u x) * pε x / cellMass pε u (u x) = sdε (u x) / cellMass pε u (u x) * pε x
      by ring, e, hpε, jeffrey_apply]
    ring
  have hs1 : r * ∑ x, g x = 1 := by
    rw [Finset.mul_sum, ← h1]; exact Finset.sum_congr rfl fun x _ => (hR1 x).symm
  have hs2 : r' * ∑ x, g x = 1 := by
    rw [Finset.mul_sum, ← h2]; exact Finset.sum_congr rfl fun x _ => (hR2 x).symm
  have hG : ∑ x, g x ≠ 0 := by intro h0; rw [h0, mul_zero] at hs1; exact zero_ne_one hs1
  have hrr : r = r' := mul_right_cancel₀ hG (hs1.trans hs2.symm)
  funext x
  rw [hR1 x, hR2 x, hrr]

end Factor

/-! ## Section 8: Basis-Overwrite and Basis-Commuting versions -/

section Extended

variable {Ω S K ι : Type*} [Fintype Ω] [Fintype K] [DecidableEq K]

/-- The **Extended Sequential Update Formula, LR version** (p. 105) on atoms.
States `s : S` affect basis `b s : K` (labelling `u k`); `Λ k γ` is the factor
that the basis-homogeneous subsequence `γ` of states on basis `k` supplies
(`LR[Q, γ, C_i, C]`).  `Λ k γ` need not be a product of per-state factors. -/
noncomputable def extUpdate (p : Ω → ℝ) (b : S → K) (u : K → Ω → ι)
    (Λ : K → List S → ι → ℝ) (δ : List S) : Ω → ℝ :=
  fun x => p x * (∏ k, Λ k (δ.filter (fun s => b s = k)) (u k x)) /
    ∑ y, p y * ∏ k, Λ k (δ.filter (fun s => b s = k)) (u k y)

/-- Reorderings that keep the order among basis-sharing states (his *suitable*
reorderings, p. 111) agree: "independent of update order -- except, perhaps,
within basis homogeneous subsequences" (pp. 103, 105). -/
theorem extUpdate_suitable (p : Ω → ℝ) (b : S → K) (u : K → Ω → ι)
    (Λ : K → List S → ι → ℝ) (δ δ' : List S)
    (h : ∀ k, δ.filter (fun s => b s = k) = δ'.filter (fun s => b s = k)) :
    extUpdate p b u Λ δ = extUpdate p b u Λ δ' := by
  unfold extUpdate; simp only [h]

/-- **Basis-Overwrite Version** (p. 108): each basis's factor is that of the last
state on it.  Two sequences with the same last state on every basis agree. -/
theorem extUpdate_basisOverwrite (p : Ω → ℝ) (b : S → K) (u : K → Ω → ι)
    (Λ : K → List S → ι → ℝ) (ω : K → Option S → ι → ℝ)
    (hΛ : ∀ k γ, Λ k γ = ω k γ.getLast?) (δ δ' : List S)
    (h : ∀ k, (δ.filter (fun s => b s = k)).getLast? = (δ'.filter (fun s => b s = k)).getLast?) :
    extUpdate p b u Λ δ = extUpdate p b u Λ δ' := by
  unfold extUpdate; simp only [hΛ, h]

/-- **Basis-Commuting Version** (p. 108): the factor of a basis-homogeneous
sequence is invariant under reordering it.  Then **every** reordering agrees:
"Sequential updating is *completely* order-independent on this model". -/
theorem extUpdate_basisCommuting (p : Ω → ℝ) (b : S → K) (u : K → Ω → ι)
    (Λ : K → List S → ι → ℝ) (hΛ : ∀ k γ γ', γ.Perm γ' → Λ k γ = Λ k γ')
    (δ δ' : List S) (hδ : δ.Perm δ') :
    extUpdate p b u Λ δ = extUpdate p b u Λ δ' := by
  unfold extUpdate
  have : ∀ k, Λ k (δ.filter (fun s => b s = k)) = Λ k (δ'.filter (fun s => b s = k)) :=
    fun k => hΛ k _ _ (hδ.filter _)
  simp only [this]

/-- Sequential factor updating, one state at a time, each state `s` supplying a
fixed factor `w s` on its basis `u (b s)` (the decomposable case: Field's
factors, Extended Rigidity applied to all states). -/
noncomputable def seqFactor (b : S → K) (u : K → Ω → ι) (w : S → ι → ℝ) :
    (Ω → ℝ) → List S → Ω → ℝ
  | p, [] => p
  | p, s :: δ => seqFactor b u w (factorUpdate p (u (b s)) (w s)) δ

omit [Fintype K] [DecidableEq K] in
/-- In the decomposable case sequential updating is the extended formula with
product factors. -/
theorem seqFactor_eq (b : S → K) (u : K → Ω → ι) (w : S → ι → ℝ)
    (hw : ∀ s i, 0 < w s i) (δ : List S) :
    ∀ p : Ω → ℝ, (∀ x, 0 ≤ p x) → ∑ x, p x = 1 →
      seqFactor b u w p δ = fun x => p x * (δ.map fun s => w s (u (b s) x)).prod /
        ∑ y, p y * (δ.map fun s => w s (u (b s) y)).prod := by
  induction δ with
  | nil =>
    intro p _ hp1
    funext x; simp [seqFactor, hp1]
  | cons s δ ih =>
    intro p hp hp1
    set Z := ∑ y, w s (u (b s) y) * p y with hZdef
    have hZ : 0 < Z := by
      obtain ⟨x0, hx0⟩ : ∃ x, 0 < p x := by
        by_contra h; push Not at h
        have : ∑ x, p x ≤ 0 := Finset.sum_nonpos fun x _ => h x
        linarith
      exact lt_of_lt_of_le (mul_pos (hw s _) hx0)
        (Finset.single_le_sum (f := fun y => w s (u (b s) y) * p y)
          (fun y _ => mul_nonneg (hw s _).le (hp y)) (Finset.mem_univ x0))
    have hq0 : ∀ x, 0 ≤ factorUpdate p (u (b s)) (w s) x := fun x =>
      div_nonneg (mul_nonneg (hw s _).le (hp x)) hZ.le
    have hq1 : ∑ x, factorUpdate p (u (b s)) (w s) x = 1 := by
      simp only [factorUpdate_apply]; rw [← Finset.sum_div, div_self hZ.ne']
    show seqFactor b u w (factorUpdate p (u (b s)) (w s)) δ = _
    rw [ih _ hq0 hq1]
    funext x
    simp only [factorUpdate_apply, List.map_cons, List.prod_cons]
    have hnum : ∀ y, w s (u (b s) y) * p y / Z * (δ.map fun s => w s (u (b s) y)).prod =
        (p y * (w s (u (b s) y) * (δ.map fun s => w s (u (b s) y)).prod)) / Z :=
      fun y => by ring
    rw [← hZdef]
    simp only [hnum]
    rw [← Finset.sum_div]
    exact div_div_div_cancel_right₀ hZ.ne' _ _

omit [Fintype K] [DecidableEq K] in
/-- With decomposable factors every permutation of the states gives the same
belief (Field 1978; Wagner 2002, Thm 3.1, cited in note 22). -/
theorem seqFactor_perm (b : S → K) (u : K → Ω → ι) (w : S → ι → ℝ)
    (hw : ∀ s i, 0 < w s i) (p : Ω → ℝ) (hp : ∀ x, 0 ≤ p x) (hp1 : ∑ x, p x = 1)
    (δ δ' : List S) (hδ : δ.Perm δ') :
    seqFactor b u w p δ = seqFactor b u w p δ' := by
  rw [seqFactor_eq b u w hw δ p hp hp1, seqFactor_eq b u w hw δ' p hp hp1]
  have : ∀ y, (δ.map fun s => w s (u (b s) y)).prod = (δ'.map fun s => w s (u (b s) y)).prod :=
    fun y => (hδ.map _).prod_eq
  simp only [this]

end Extended

/-! ## Section 9: the Commutation Theorem (two-state core of the Update
Reordering Theorem) -/

section Reordering

variable {Ω ι κ : Type*} [Fintype Ω] [DecidableEq ι] [DecidableEq κ]

private theorem alg_core {A B C D P a b c e : ℝ} (hP : 0 < P) (ha : 0 < a) (hb : 0 < b)
    (hc : 0 < c) (he : 0 < e) (hB : 0 < B) (hD : 0 < D) (hC : 0 < C) :
    (A * (B * P / a) / b = C * (D * P / c) / e) ↔ A / b = (C / e) / (B / a) * (D / c) := by
  constructor
  · intro h
    field_simp at h ⊢
    linear_combination h
  · intro h
    rw [show A * (B * P / a) / b = A / b * (B / a) * P by ring, h]
    field_simp

/-- **Commutation Theorem** (p. 118), two-state finite case, the core of the
**Update Reordering Theorem** (p. 112).  `p = Q_α`; `d` updates basis `u`
(`D_i`), `ε` updates basis `v` (`E_j`); the targets may depend on the order
(`sd = Q_αd[D_i]`, `sdε = Q_αεd[D_i]`, `tε = Q_αε[E_j]`, `tεd = Q_αdε[E_j]`).
Then `Q_αdε = Q_αεd` iff for each `Q_αdε`-possible `D_i` there is
`r = NL[Q_αε, d, D_i]/NL[Q_α, d, D_i] > 0` such that every `E_j` that is
`Q_αdε`-compatible with `D_i` has `NL[Q_αd, ε, E_j] = r · NL[Q_α, ε, E_j]`.
The value of `r` is fixed by `d`'s factors, as Hawthorne states it; it is not
free. -/
theorem commutation_theorem (p : Ω → ℝ) (u : Ω → ι) (v : Ω → κ) (sd sdε : ι → ℝ)
    (tε tεd : κ → ℝ) (hp : ∀ x, 0 ≤ p x)
    (hsd : ∀ i, 0 < sd i) (hsdε : ∀ i, 0 < sdε i) (htε : ∀ j, 0 < tε j)
    (htεd : ∀ j, 0 < tεd j) :
    jeffrey (jeffrey p u sd) v tεd = jeffrey (jeffrey p v tε) u sdε ↔
      ∀ i, 0 < cellMass (jeffrey (jeffrey p u sd) v tεd) u i →
        0 < NL (jeffrey p v tε) (jeffrey (jeffrey p v tε) u sdε) u i / NL p (jeffrey p u sd) u i ∧
        ∀ j, 0 < jointMass (jeffrey (jeffrey p u sd) v tεd) u v i j →
          NL (jeffrey p u sd) (jeffrey (jeffrey p u sd) v tεd) v j =
            (NL (jeffrey p v tε) (jeffrey (jeffrey p v tε) u sdε) u i /
              NL p (jeffrey p u sd) u i) * NL p (jeffrey p v tε) v j := by
  simp only [NL_jeffrey]
  set pd := jeffrey p u sd with hpd
  set pε := jeffrey p v tε with hpε
  have hpd0 : ∀ x, 0 ≤ pd x := jeffrey_nonneg hp u fun i => (hsd i).le
  have hpε0 : ∀ x, 0 ≤ pε x := jeffrey_nonneg hp v fun j => (htε j).le
  have hR10 : ∀ x, 0 ≤ jeffrey pd v tεd x := jeffrey_nonneg hpd0 v fun j => (htεd j).le
  -- positivity facts at an atom of positive prior mass
  have facts : ∀ x, 0 < p x →
      0 < cellMass p u (u x) ∧ 0 < cellMass p v (v x) ∧
      0 < cellMass pd v (v x) ∧ 0 < cellMass pε u (u x) := by
    intro x hx
    have hmu : 0 < cellMass p u (u x) := lt_of_lt_of_le hx (le_cellMass hp u x)
    have hmv : 0 < cellMass p v (v x) := lt_of_lt_of_le hx (le_cellMass hp v x)
    have hpdx : 0 < pd x := by rw [hpd, jeffrey_apply]; exact div_pos (mul_pos (hsd _) hx) hmu
    have hpεx : 0 < pε x := by rw [hpε, jeffrey_apply]; exact div_pos (mul_pos (htε _) hx) hmv
    exact ⟨hmu, hmv, lt_of_lt_of_le hpdx (le_cellMass hpd0 v x),
      lt_of_lt_of_le hpεx (le_cellMass hpε0 u x)⟩
  have route1 : ∀ x, jeffrey pd v tεd x =
      tεd (v x) * (sd (u x) * p x / cellMass p u (u x)) / cellMass pd v (v x) := by
    intro x; rw [jeffrey_apply, hpd, jeffrey_apply]
  have route2 : ∀ x, jeffrey pε u sdε x =
      sdε (u x) * (tε (v x) * p x / cellMass p v (v x)) / cellMass pε u (u x) := by
    intro x; rw [jeffrey_apply, hpε, jeffrey_apply]
  constructor
  · intro hcomm i _
    -- positivity of r needs some atom in D_i; get it from a compatible cell or
    -- directly from `Q_αdε[D_i] > 0`
    refine ⟨?_, fun j hij => ?_⟩
    · rename_i hi
      obtain ⟨x, hxu, hx⟩ : ∃ x, u x = i ∧ 0 < jeffrey pd v tεd x := by
        by_contra h; push Not at h
        have : cellMass (jeffrey pd v tεd) u i ≤ 0 :=
          Finset.sum_nonpos fun y _ => by
            split_ifs with hy
            · exact h y hy
            · exact le_rfl
        linarith
      have hpx : 0 < p x := by
        rcases (hp x).lt_or_eq with h | h
        · exact h
        · rw [route1, ← h] at hx; simp at hx
      obtain ⟨hmu, -, -, hmε⟩ := facts x hpx
      rw [← hxu]
      exact div_pos (div_pos (hsdε _) hmε) (div_pos (hsd _) hmu)
    · obtain ⟨x, hxu, hxv, hx⟩ := exists_pos_of_jointMass_pos hij
      have hpx : 0 < p x := by
        rcases (hp x).lt_or_eq with h | h
        · exact h
        · rw [route1, ← h] at hx; simp at hx
      obtain ⟨hmu, hmv, hmd, hmε⟩ := facts x hpx
      have h := congrFun hcomm x
      rw [route1, route2] at h
      rw [← hxu, ← hxv]
      exact (alg_core hpx hmu hmd hmv hmε (hsd _) (htε _) (hsdε _)).1 h
  · intro hcl
    funext x
    rw [route1, route2]
    rcases (hp x).lt_or_eq with hpx | hpx
    · obtain ⟨hmu, hmv, hmd, hmε⟩ := facts x hpx
      have hR1x : 0 < jeffrey pd v tεd x := by
        rw [route1]; exact div_pos (mul_pos (htεd _) (div_pos (mul_pos (hsd _) hpx) hmu)) hmd
      have hi : 0 < cellMass (jeffrey pd v tεd) u (u x) :=
        lt_of_lt_of_le hR1x (le_cellMass hR10 u x)
      have hij : 0 < jointMass (jeffrey pd v tεd) u v (u x) (v x) :=
        lt_of_lt_of_le hR1x (le_jointMass hR10 u v x)
      exact (alg_core hpx hmu hmd hmv hmε (hsd _) (htε _) (hsdε _)).2 ((hcl _ hi).2 _ hij)
    · rw [← hpx]; simp

/-- The end of the proof (p. 120): once `Q_αdε = Q_αεd`, any further sequence
`β` applied to both gives the same result. -/
theorem commutation_append {Ω : Type*} (P P' : Ω → ℝ) (β : (Ω → ℝ) → (Ω → ℝ))
    (h : P = P') : β P = β P' := by rw [h]

end Reordering

/-! ## The medical example (Section 5, pp. 97-98; Section 7, p. 107; note 14) -/

section Medical

/-- Atoms `(c, e, f)`: cancer, x-ray mass image, cancer-like cells. -/
abbrev Atom := Bool × Bool × Bool

/-- `Q[E | C] = Q[F | C] = l`, `Q[E | ∼C] = Q[F | ∼C] = 1 - l`. -/
noncomputable def lik (l : ℝ) (a c : Bool) : ℝ := if a = c then l else 1 - l

/-- The physician's prior: `Q[C] = .5`, tests conditionally independent given
`C` and given `∼C` (pp. 97-98). -/
noncomputable def medPrior (l : ℝ) : Atom → ℝ := fun x => 1 / 2 * lik l x.2.1 x.1 * lik l x.2.2 x.1

/-- The hypothesis partition `{C, ∼C}` and the two evidence bases `{E, ∼E}`,
`{F, ∼F}`. -/
def cC : Atom → Bool := fun x => x.1
def eE : Atom → Bool := fun x => x.2.1
def fF : Atom → Bool := fun x => x.2.2

/-- The technician: `Q_f[∼F] = .90`. -/
noncomputable def tgtF : Bool → ℝ := fun b => if b then 1 / 10 else 9 / 10
/-- The radiologist: `Q_e[E] = .90`. -/
noncomputable def tgtE : Bool → ℝ := fun b => if b then 9 / 10 else 1 / 10

/-- `Q_f[C] = .14` (p. 98). -/
theorem med_Qf_C : cellMass (jeffrey (medPrior (19 / 20)) fF tgtF) cC true = 7 / 50 := by
  simp [cellMass, jeffrey, medPrior, lik, cC, fF, tgtF, Fintype.sum_prod_type]; norm_num

/-- `Q_fe[C] = .68` (p. 98): exactly `24689/36256 ≈ .681`. -/
theorem med_Qfe_C :
    cellMass (jeffrey (jeffrey (medPrior (19 / 20)) fF tgtF) eE tgtE) cC true = 24689 / 36256 := by
  simp [cellMass, jeffrey, medPrior, lik, cC, fF, eE, tgtF, tgtE, Fintype.sum_prod_type]
  norm_num

/-- `Q_e[C] = .86` (p. 98). -/
theorem med_Qe_C : cellMass (jeffrey (medPrior (19 / 20)) eE tgtE) cC true = 43 / 50 := by
  simp [cellMass, jeffrey, medPrior, lik, cC, eE, tgtE, Fintype.sum_prod_type]; norm_num

/-- `Q_ef[C] = .32` (p. 98): exactly `11567/36256 ≈ .319`. -/
theorem med_Qef_C :
    cellMass (jeffrey (jeffrey (medPrior (19 / 20)) eE tgtE) fF tgtF) cC true = 11567 / 36256 := by
  simp [cellMass, jeffrey, medPrior, lik, cC, fF, eE, tgtF, tgtE, Fintype.sum_prod_type]
  norm_num

/-- The two orders differ by `6561/18128 ≈ .362` (the two ".18" swings). -/
theorem med_order_effect :
    cellMass (jeffrey (jeffrey (medPrior (19 / 20)) fF tgtF) eE tgtE) cC true -
      cellMass (jeffrey (jeffrey (medPrior (19 / 20)) eE tgtE) fF tgtF) cC true = 6561 / 18128 := by
  rw [med_Qfe_C, med_Qef_C]; norm_num

/-- **The explicit overwrite on two distinct bases** (p. 98): "the physician
adopts the radiologist's degree of confidence that an image of a mass is present,
`Q_e[E] = Q_fe[E] = .90`".  The `E`-marginal after `f` then `e` is the one after
`e` alone. -/
theorem med_overwrite :
    cellMass (jeffrey (jeffrey (medPrior (19 / 20)) fF tgtF) eE tgtE) eE true = 9 / 10 ∧
      cellMass (jeffrey (medPrior (19 / 20)) eE tgtE) eE true = 9 / 10 := by
  constructor <;>
  · simp [cellMass, jeffrey, medPrior, lik, fF, eE, tgtF, tgtE, Fintype.sum_prod_type]
    norm_num

/-- Cross-basis influence (p. 97): the `F`-update alone moves `Q[E]` from `1/2` to
`22/125`, and the `E`-update alone moves `Q[F]` to `103/125`, so the commutation
criterion fails. -/
theorem med_crossBasis :
    cellMass (medPrior (19 / 20)) eE true = 1 / 2 ∧
      cellMass (jeffrey (medPrior (19 / 20)) fF tgtF) eE true = 22 / 125 ∧
      cellMass (medPrior (19 / 20)) fF true = 1 / 2 ∧
      cellMass (jeffrey (medPrior (19 / 20)) eE tgtE) fF true = 103 / 125 := by
  refine ⟨?_, ?_, ?_, ?_⟩ <;>
  · simp [cellMass, jeffrey, medPrior, lik, fF, eE, tgtF, tgtE, Fintype.sum_prod_type]
    norm_num

/-- Note 14: with likelihoods .99 the orders give `.83` and `.17`; exactly
`1477317/1778144 ≈ .831` and `300827/1778144 ≈ .169`. -/
theorem med_note14 :
    cellMass (jeffrey (jeffrey (medPrior (99 / 100)) fF tgtF) eE tgtE) cC true =
        1477317 / 1778144 ∧
      cellMass (jeffrey (jeffrey (medPrior (99 / 100)) eE tgtE) fF tgtF) cC true =
        300827 / 1778144 := by
  constructor <;>
  · simp [cellMass, jeffrey, medPrior, lik, cC, fF, eE, tgtF, tgtE, Fintype.sum_prod_type]
    norm_num

/-- `LR[f, F, ∼F] = .50` as a factor vector on `{F, ∼F}`. -/
noncomputable def lrF : Bool → ℝ := fun b => if b then 1 / 2 else 1
/-- `LR[e, E, ∼E] = 2` as a factor vector on `{E, ∼E}`. -/
noncomputable def lrE : Bool → ℝ := fun b => if b then 2 else 1

/-- The technician's factor is indeed `LR[f, F, ∼F] = .50`. -/
theorem med_LR_factor :
    LR (medPrior (19 / 20)) (factorUpdate (medPrior (19 / 20)) fF lrF) fF true false = 1 / 2 := by
  rw [LR_factorUpdate] <;>
  · simp [cellMass, medPrior, lik, fF, lrF, Fintype.sum_prod_type] <;> norm_num

/-- The LR example (p. 107): `Q_f[C] = .35`. -/
theorem med_LR_Qf_C : cellMass (factorUpdate (medPrior (19 / 20)) fF lrF) cC true = 7 / 20 := by
  simp [cellMass, factorUpdate, medPrior, lik, cC, fF, lrF, Fintype.sum_prod_type]; norm_num

/-- The LR example (p. 107): `Q_fe[C] = .50`, and `Q_ef[C] = Q_fe[C]`. -/
theorem med_LR_Qfe_Qef :
    cellMass (factorUpdate (factorUpdate (medPrior (19 / 20)) fF lrF) eE lrE) cC true = 1 / 2 ∧
      cellMass (factorUpdate (factorUpdate (medPrior (19 / 20)) eE lrE) fF lrF) cC true = 1 / 2 := by
  constructor <;>
  · simp [cellMass, factorUpdate, medPrior, lik, cC, fF, eE, lrF, lrE, Fintype.sum_prod_type]
    norm_num

/-- The Bayes-factor reading of the amnestic reports (.90 against the prior 1/2)
gives LR factors `1/9` (sputum) and `9` (x-ray), not Hawthorne's `.50` and `2`. -/
theorem med_bayesFactorReading :
    LR (medPrior (19 / 20)) (jeffrey (medPrior (19 / 20)) fF tgtF) fF true false = 1 / 9 ∧
      LR (medPrior (19 / 20)) (jeffrey (medPrior (19 / 20)) eE tgtE) eE true false = 9 := by
  constructor <;>
  · simp [LR, NL, cellMass, jeffrey, medPrior, lik, fF, eE, tgtF, tgtE, Fintype.sum_prod_type]
    norm_num

/-- With the factors `1/9` and `9` the result is also `1/2` in both orders. -/
theorem med_LR_ninths :
    cellMass (factorUpdate (factorUpdate (medPrior (19 / 20)) fF (fun b => if b then 1 / 9 else 1))
        eE (fun b => if b then 9 else 1)) cC true = 1 / 2 ∧
      cellMass (factorUpdate (factorUpdate (medPrior (19 / 20)) eE (fun b => if b then 9 else 1))
        fF (fun b => if b then 1 / 9 else 1)) cC true = 1 / 2 := by
  constructor <;>
  · simp [cellMass, factorUpdate, medPrior, lik, cC, fF, eE, Fintype.sum_prod_type]
    norm_num

/-- NL factors taken against the initial prior, `Q_e[E_i]/Q[E_i]` and
`Q_f[F_j]/Q[F_j]` (as in the NL Extended Sequential Update Formula, p. 102). -/
noncomputable def nlE : Bool → ℝ := fun b => if b then 9 / 5 else 1 / 5
noncomputable def nlF : Bool → ℝ := fun b => if b then 1 / 5 else 9 / 5

/-- The **normalising denominator** of the NL formula on the medical example is
`301/625`, not 1.  After normalisation `Q[C] = 1/2`, the cancellation Hawthorne
calls intuitive; without it the `C`-mass is `301/1250`. -/
theorem med_NL_denominator :
    (∑ x, nlE (eE x) * nlF (fF x) * medPrior (19 / 20) x) = 301 / 625 ∧
      cellMass (fun x => nlE (eE x) * nlF (fF x) * medPrior (19 / 20) x) cC true = 301 / 1250 ∧
      cellMass (factorUpdate (factorUpdate (medPrior (19 / 20)) eE nlE) fF nlF) cC true = 1 / 2 := by
  refine ⟨?_, ?_, ?_⟩ <;>
  · simp [cellMass, factorUpdate, medPrior, lik, cC, fF, eE, nlE, nlF, Fintype.sum_prod_type]
    norm_num

end Medical

section Denominator

variable {Ω ι κ : Type*} [Fintype Ω] [Fintype ι] [Fintype κ] [DecidableEq ι] [DecidableEq κ]

private theorem sum_by_cells (u : Ω → ι) (v : Ω → κ) (f : Ω → ℝ) :
    ∑ x, f x = ∑ i, ∑ j, ∑ x, if u x = i ∧ v x = j then f x else 0 := by
  symm
  calc ∑ i, ∑ j, ∑ x, (if u x = i ∧ v x = j then f x else 0)
      = ∑ i, ∑ x, ∑ j, (if u x = i ∧ v x = j then f x else 0) :=
        Finset.sum_congr rfl fun i _ => Finset.sum_comm
    _ = ∑ x, ∑ i, ∑ j, (if u x = i ∧ v x = j then f x else 0) := Finset.sum_comm
    _ = ∑ x, f x := Finset.sum_congr rfl fun x _ => by simp [ite_and]

/-- When the two bases are independent under the prior
(`Q[D_i·E_j] = Q[D_i]·Q[E_j]`), the NL denominator with prior-relative factors
`q_i/Q[D_i]`, `t_j/Q[E_j]` is 1.  Only then is `P(i,j)·ℓ^A_i·ℓ^B_j` a
probability without normalising. -/
theorem nl_denominator_indep (p : Ω → ℝ) (u : Ω → ι) (v : Ω → κ) (q : ι → ℝ) (t : κ → ℝ)
    (hq1 : ∑ i, q i = 1) (ht1 : ∑ j, t j = 1)
    (hmu : ∀ i, cellMass p u i ≠ 0) (hmv : ∀ j, cellMass p v j ≠ 0)
    (hind : ∀ i j, jointMass p u v i j = cellMass p u i * cellMass p v j) :
    ∑ x, q (u x) / cellMass p u (u x) * (t (v x) / cellMass p v (v x)) * p x = 1 := by
  rw [sum_by_cells u v]
  have : ∀ i j, (∑ x, if u x = i ∧ v x = j then
      q (u x) / cellMass p u (u x) * (t (v x) / cellMass p v (v x)) * p x else 0) =
      q i * t j := by
    intro i j
    have h1 : (∑ x, if u x = i ∧ v x = j then
        q (u x) / cellMass p u (u x) * (t (v x) / cellMass p v (v x)) * p x else 0) =
        q i / cellMass p u i * (t j / cellMass p v j) * jointMass p u v i j := by
      unfold jointMass
      rw [Finset.mul_sum]
      refine Finset.sum_congr rfl fun x _ => ?_
      split_ifs with h
      · rw [h.1, h.2]
      · simp
    rw [h1, hind]
    have := hmu i; have := hmv j
    field_simp
  simp only [this, ← Finset.mul_sum, ht1, mul_one, hq1]

end Denominator

/-! ## Block-diagonal example for the Update Reordering Theorem (Section 9) -/

section Block

/-- The prior on the four `E`-atoms; `D = 1` holds on `E₁, E₂`, `D = 2` on
`E₃, E₄`. -/
noncomputable def blkPrior : Fin 4 → ℝ := ![1 / 10, 1 / 5, 3 / 10, 2 / 5]
/-- `d`'s basis `{D₁, D₂}`. -/
def blkD : Fin 4 → Fin 2 := ![0, 0, 1, 1]
/-- `ε`'s basis `{E₁, ..., E₄}`. -/
def blkE : Fin 4 → Fin 4 := id
/-- `d` delivers `Q[D₁] = 3/4`. -/
noncomputable def blkS : Fin 2 → ℝ := ![3 / 4, 1 / 4]
/-- `ε` delivers `(t₁, 3/4 - t₁, t₃, 1/4 - t₃)`: any within-block split. -/
noncomputable def blkT (t₁ t₃ : ℝ) : Fin 4 → ℝ := ![t₁, 3 / 4 - t₁, t₃, 1 / 4 - t₃]

/-- The two amnestic updates commute for **every** within-block split. -/
theorem blk_commute (t₁ t₃ : ℝ) :
    jeffrey (jeffrey blkPrior blkD blkS) blkE (blkT t₁ t₃) =
      jeffrey (jeffrey blkPrior blkE (blkT t₁ t₃)) blkD blkS := by
  funext x
  fin_cases x <;>
  · simp [jeffrey, cellMass, blkPrior, blkD, blkE, blkS, blkT, Fin.sum_univ_four]
    ring_nf

/-- Hawthorne's `r = NL[Q_αε, d, D_i] / NL[Q_α, d, D_i]`: `2/5` on the class of
`D₁` and `14/5` on the class of `D₂`, for every split. -/
theorem blk_r (t₁ t₃ : ℝ) :
    NL (jeffrey blkPrior blkE (blkT t₁ t₃))
        (jeffrey (jeffrey blkPrior blkE (blkT t₁ t₃)) blkD blkS) blkD 0 /
        NL blkPrior (jeffrey blkPrior blkD blkS) blkD 0 = 2 / 5 ∧
      NL (jeffrey blkPrior blkE (blkT t₁ t₃))
        (jeffrey (jeffrey blkPrior blkE (blkT t₁ t₃)) blkD blkS) blkD 1 /
        NL blkPrior (jeffrey blkPrior blkD blkS) blkD 1 = 14 / 5 := by
  simp only [NL_jeffrey]
  constructor <;>
  · simp [jeffrey, cellMass, blkPrior, blkD, blkE, blkS, blkT, Fin.sum_univ_four]
    norm_num

/-- Clause (2) of the Reordering Theorem holds with those class-specific values
(split `t₁ = 1/10`, `t₃ = 1/20`): `NL[Q_αd, ε, E_j] = r_i · NL[Q_α, ε, E_j]`
for `E_j` in the class of `D_i`. -/
theorem blk_clause2 :
    let pd := jeffrey blkPrior blkD blkS
    let pε := jeffrey blkPrior blkE (blkT (1 / 10) (1 / 20))
    NL pd (jeffrey pd blkE (blkT (1 / 10) (1 / 20))) blkE 0 = 2 / 5 * NL blkPrior pε blkE 0 ∧
    NL pd (jeffrey pd blkE (blkT (1 / 10) (1 / 20))) blkE 1 = 2 / 5 * NL blkPrior pε blkE 1 ∧
    NL pd (jeffrey pd blkE (blkT (1 / 10) (1 / 20))) blkE 2 = 14 / 5 * NL blkPrior pε blkE 2 ∧
    NL pd (jeffrey pd blkE (blkT (1 / 10) (1 / 20))) blkE 3 = 14 / 5 * NL blkPrior pε blkE 3 := by
  simp only [NL_jeffrey]
  refine ⟨?_, ?_, ?_, ?_⟩ <;>
  · simp [jeffrey, cellMass, blkPrior, blkD, blkE, blkS, blkT, Fin.sum_univ_four]
    norm_num

/-- **NL Extended Rigidity fails** in the block example: no single `r` relates
`ε`'s factors after `d` to its factors without `d`.  Commutation holds anyway
(`blk_commute`), so the Reordering Theorem's condition is strictly weaker. -/
theorem blk_not_extendedRigidity :
    ¬ ∃ r : ℝ, ∀ j, 0 < cellMass (jeffrey (jeffrey blkPrior blkD blkS) blkE
        (blkT (1 / 10) (1 / 20))) blkE j →
      NL (jeffrey blkPrior blkD blkS)
          (jeffrey (jeffrey blkPrior blkD blkS) blkE (blkT (1 / 10) (1 / 20))) blkE j =
        r * NL blkPrior (jeffrey blkPrior blkE (blkT (1 / 10) (1 / 20))) blkE j := by
  rintro ⟨r, hr⟩
  obtain ⟨h0, h1, h2, h3⟩ := blk_clause2
  have hpos : ∀ j : Fin 4, 0 < cellMass (jeffrey (jeffrey blkPrior blkD blkS) blkE
      (blkT (1 / 10) (1 / 20))) blkE j := by
    intro j
    fin_cases j <;>
    · simp [jeffrey, cellMass, blkPrior, blkD, blkE, blkS, blkT, Fin.sum_univ_four]
      norm_num
  have e0 := hr 0 (hpos 0)
  have e2 := hr 2 (hpos 2)
  rw [h0] at e0; rw [h2] at e2
  have n0 : NL blkPrior (jeffrey blkPrior blkE (blkT (1 / 10) (1 / 20))) blkE 0 = 1 := by
    rw [NL_jeffrey]; simp [cellMass, blkPrior, blkE, blkT]
  have n2 : NL blkPrior (jeffrey blkPrior blkE (blkT (1 / 10) (1 / 20))) blkE 2 = 1 / 6 := by
    rw [NL_jeffrey]; simp [cellMass, blkPrior, blkE, blkT]; norm_num
  rw [n0] at e0; rw [n2] at e2
  linarith

/-- The p. 97 criterion's "only if" needs positive joint cells: here the updates
commute (`blk_commute`), yet `d` moves `ε`'s basis marginal (`Q_d[E₁] = 1/4`
against `Q[E₁] = 1/10`) and `ε` moves `d`'s (`Q_ε[D₁] = 3/4` against `3/10`). -/
theorem criterion_needs_positive_cells :
    jeffrey (jeffrey blkPrior blkD blkS) blkE (blkT (1 / 10) (1 / 20)) =
        jeffrey (jeffrey blkPrior blkE (blkT (1 / 10) (1 / 20))) blkD blkS ∧
      cellMass (jeffrey blkPrior blkD blkS) blkE 0 ≠ cellMass blkPrior blkE 0 ∧
      cellMass (jeffrey blkPrior blkE (blkT (1 / 10) (1 / 20))) blkD 0 ≠ cellMass blkPrior blkD 0 := by
  refine ⟨blk_commute _ _, ?_, ?_⟩ <;>
  · simp [jeffrey, cellMass, blkPrior, blkD, blkE, blkS, blkT, Fin.sum_univ_four]
    norm_num

end Block

end Literature.Hawthorne
