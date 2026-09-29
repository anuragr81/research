/-
# Domotor (1980), "Probability Kinematics and Representation of Belief Change"

*Philosophy of Science* 47, 384-403.

Formalization of the paper's own definitions and claims, not of Paper B's.
Throughout, the sample space `X` is finite (`Ω` below, a `Fintype`), a belief
state is a function `P : Ω → ℝ`, and a partition is a labelling `u : Ω → ι`.
Lean's convention `x / 0 = 0` makes the paper's improper state `0` (p.389-390,
"put `0(A)` equal zero for all `A`") appear automatically as the result of
conditioning on a null proposition, so the Bayesian machine lives on
`P'(X) = P(X) + (0)` exactly as in (4).

## What is formalized

* **Machines** (p.385): a triple `(P, E, *)` with triviality `P₁ = P` and
  composition `(P_E)_{E'} = P_{E ∧ E'}` (`Machine`).
* **Bayesian machine** (p.389-390, (4)): states are probabilities or the zero
  state, inputs are propositions composed by intersection (`bayesMachine`).
  Also the monotonicity laws (1) and (2) (p.387-388), the mixing law (3)
  (p.388), and the "impossible state" law of p.399 (`cond_disjoint`).
* **Jeffrey's formula** (0)/(5) (p.386, p.392) and the **Jeffrey machine** on
  finitary strings of partition-probability pairs, the free monoid under
  concatenation (p.393, `jeffreyMachine`); its observations (i) dominance and
  (iii) irrelevance (p.396); the Markovian iteration law of p.399
  (`jeffrey_markov`); non-commutativity (p.395, p.399) by an explicit 2×2
  instance (`jeffrey_not_comm`).
* The p.393 description of an iterate as a mixture of conditionals on the
  meet `U₁ ∧ … ∧ Uₙ`: `jeffrey_seq_on_meet` proves that two sequential Jeffrey
  steps are always one Jeffrey step on the meet.
* **Field's conditional and machine** (p.396-397): `fieldCond`, with the
  composition law `[P_{(U,α)}]_{(V,β)} = P_{(U∧V, α∧β)}` and hence
  commutativity (`field_comp`, `field_comm`), and clause (i), mutual
  dominance (`field_zero_iff`).
* **The embedding** `h_P(U,α) = (U,p)`, `p(A) = e^{α_A} P(A) : α_X` (p.397):
  Jeffrey on `h_P(U,α)` is Field on `(U,α)` (`field_eq_jeffrey_embed`); the unit
  goes to `(U, P|_U)` (`embed_unit`); the image depends on the current state
  (`embed_depends_on_state`); and every strictly positive Jeffrey input on a
  partition whose cells all have positive mass is in the image
  (`embed_surjective`).

## What formalizing revealed

* **Clause (ii) of p.397 is false as stated.**  Domotor asserts
  `(P +_a Q)_{(U,α)} = P_{(U,α)} +_a Q_{(U,α)}`, "conditionalization commutes
  with mixing in a trivial fashion.  All one modifies is the mixands and not the
  mixing coefficient."  `field_not_convex` is a two-point counterexample.  The
  correct law (`field_mix`) keeps the mixands' Field conditionals but changes
  the coefficient to `a·Z_P / (a·Z_P + (1-a)·Z_Q)`, with `Z` the normalizing
  constants, exactly as in the Bayesian law (3), where `a_A = a·P(A) : [P+Q](A)`.
* **p.395-396: "the foregoing iterated application of inputs … does not reduce
  to one joint input on the meet `U ∧ V`, save the special cases of refinement
  and probabilistic independence."**  `jeffrey_seq_on_meet` shows the iterate
  *is* always a single Jeffrey update on the meet; what fails is that the joint
  input is not a function of `(p, q)` alone (it depends on `P`).  The claim is
  true only in that reading.
* **p.397: "Field's `F_X` in fact covers only what is probabilistically
  independent in Jeffrey's `E_X`."**  For a single input, `embed_surjective`
  shows `h_P` reaches every strictly positive Jeffrey input.  The restriction
  concerns compositions, not single inputs.
* Minor (not formalized): p.387 calls the belief space of a die "a 6-simplex …
  in the five-dimensional real Euclidean space"; the probability simplex on six
  points is 5-dimensional (a 5-simplex).  p.401 says `p` is found by
  *maximizing* `H_P(U,p) = Σ p(A) log(p(A)/P(A))`, which is the Kullback-Leibler
  divergence and is minimized, not maximized; the stated solution
  `p(A) = (1/α_X) P(A) e^{-λα_A}` is the minimizer.  `α_X` is used on p.397
  as the normalizing constant and in (7) as an expectation.

## Not formalized

The Boolean machine on filters (p.391-392) and its conditional `⊳`; the
general coarse-graining conditions (i)-(iii)(a)-(d) of p.394 and their claimed
equivalence ("we claim (but will not prove in detail here)"); the particle
example of p.395 (a measure on `ℝ`); clause (ii) of p.396 (orthogonality
preserved); the reduction sketch of §3 (p.398-400: product spaces, Hahn-Banach,
the Miller principle), which is a sketch with no theorem; the internalized
conditionals of p.400; and §4 on maximum entropy and Martin-Löf's limit theorem
(p.400-403), which is calculus and asymptotics, not stated as theorems.
-/
import Mathlib

set_option linter.unusedSectionVars false

namespace Literature.Domotor

open Finset

/-! ## Machines (p.385) -/

/-- A **machine** `(P, E, *)` (p.385): a transition `* : P × E → P` with
(a) triviality `P₁ = P` and (b) composition `(P_E)_{E'} = P_{E ∧ E'}`. -/
structure Machine (S E : Type*) where
  unit : E
  comp : E → E → E
  act : S → E → S
  triviality : ∀ P, act P unit = P
  composition : ∀ P e e', act (act P e) e' = act P (comp e e')

section General

variable {Ω : Type*} [Fintype Ω] [DecidableEq Ω]

/-- `P(A)`. -/
def mass (P : Ω → ℝ) (A : Finset Ω) : ℝ := ∑ x ∈ A, P x

/-- The standard conditional `P_A = P * A` (p.389). -/
noncomputable def cond (P : Ω → ℝ) (A : Finset Ω) : Ω → ℝ :=
  fun x => if x ∈ A then P x / mass P A else 0

/-- Belief states of the Bayesian machine: probabilities together with the
improper state `0` (p.389-390: `P'(X) = P(X) + (0)`). -/
def State (Ω : Type*) [Fintype Ω] : Type _ :=
  {P : Ω → ℝ // (∀ x, 0 ≤ P x) ∧ (∑ x, P x = 1 ∨ P = 0)}

theorem mass_nonneg {P : Ω → ℝ} (hP : ∀ x, 0 ≤ P x) (A : Finset Ω) : 0 ≤ mass P A :=
  Finset.sum_nonneg fun x _ => hP x

theorem mass_mono {P : Ω → ℝ} (hP : ∀ x, 0 ≤ P x) {A B : Finset Ω} (h : A ⊆ B) :
    mass P A ≤ mass P B :=
  Finset.sum_le_sum_of_subset_of_nonneg h fun x _ _ => hP x

theorem le_mass {P : Ω → ℝ} (hP : ∀ x, 0 ≤ P x) {A : Finset Ω} {x : Ω} (hx : x ∈ A) :
    P x ≤ mass P A :=
  Finset.single_le_sum (fun y _ => hP y) hx

theorem mass_cond (P : Ω → ℝ) (A B : Finset Ω) :
    mass (cond P A) B = mass P (B ∩ A) / mass P A := by
  unfold mass cond
  rw [← Finset.sum_ite_mem, Finset.sum_div]
  refine Finset.sum_congr rfl fun x _ => ?_
  by_cases hx : x ∈ A <;> simp [hx, mass]

theorem cond_nonneg {P : Ω → ℝ} (hP : ∀ x, 0 ≤ P x) (A : Finset Ω) (x : Ω) :
    0 ≤ cond P A x := by
  unfold cond; split_ifs
  · exact div_nonneg (hP x) (mass_nonneg hP A)
  · exact le_refl 0

/-- **Composition law (b) for the Bayesian machine** (p.390: `(P_A)_B = P_{A∩B}`),
for every nonnegative `P`, including the improper state. -/
theorem cond_cond {P : Ω → ℝ} (hP : ∀ x, 0 ≤ P x) (A B : Finset Ω) :
    cond (cond P A) B = cond P (A ∩ B) := by
  funext x
  have hm := mass_cond P A B
  unfold cond at *
  rw [hm, Finset.inter_comm B A]
  by_cases hB : x ∈ B <;> by_cases hA : x ∈ A <;> simp [hA, hB]
  -- remaining: x ∈ A ∩ B
  have h1 : mass P (A ∩ B) ≤ mass P A := mass_mono hP Finset.inter_subset_left
  have h0 : 0 ≤ mass P (A ∩ B) := mass_nonneg hP _
  rcases h0.lt_or_eq with h | h
  · have : mass P A ≠ 0 := by linarith
    field_simp
  · rw [← h]; simp

/-- Triviality (a) (p.390: `P_X = P`). -/
theorem cond_univ {P : Ω → ℝ} (h : ∑ x, P x = 1) : cond P univ = P := by
  funext x; simp [cond, mass, h]

/-- The zero state is an attractor (p.390: `0_A = 0`), and the impossible
evidence `∅` sends every state to `0`. -/
theorem cond_zero (A : Finset Ω) : cond 0 A = 0 := by
  funext x; simp [cond, mass]

theorem cond_empty (P : Ω → ℝ) : cond P ∅ = 0 := by
  funext x; simp [cond]

/-- Conditionals of probabilities are probabilities or the zero state. -/
theorem cond_state {P : Ω → ℝ} (hP : ∀ x, 0 ≤ P x) (A : Finset Ω) :
    (∀ x, 0 ≤ cond P A x) ∧ (∑ x, cond P A x = 1 ∨ cond P A = 0) := by
  refine ⟨cond_nonneg hP A, ?_⟩
  by_cases h : mass P A = 0
  · right; funext x; simp [cond, h]
  · left
    have := mass_cond P A univ
    simp only [mass, Finset.univ_inter] at this
    rw [this]; exact div_self h

/-- **The Bayesian machine** `(P'(X), E_X, *)` (p.390, (4)). -/
noncomputable def bayesMachine : Machine (State Ω) (Finset Ω) where
  unit := univ
  comp := (· ∩ ·)
  act P A := ⟨cond P.1 A, cond_state P.2.1 A⟩
  triviality P := by
    apply Subtype.ext
    rcases P.2.2 with h | h
    · exact cond_univ h
    · simp only [h]; exact cond_zero _
  composition P A B := Subtype.ext (cond_cond P.2.1 A B)

/-- The p.399 "impossible state": conditioning on two incompatible propositions
gives `0` (`(P_{A+Ā})_{A+Ā} = 0` if `a ≠ b`). -/
theorem cond_disjoint {P : Ω → ℝ} (hP : ∀ x, 0 ≤ P x) {A B : Finset Ω}
    (h : Disjoint A B) : cond (cond P A) B = 0 := by
  rw [cond_cond hP, Finset.disjoint_iff_inter_eq_empty.mp h, cond_empty]

/-- **Dominance** `P ≪ Q` (p.387): `Q(A) = 0 ⇒ P(A) = 0`; on a finite space,
pointwise. -/
def Dominated (P Q : Ω → ℝ) : Prop := ∀ x, Q x = 0 → P x = 0

/-- **Monotonicity (1)**, first formula (p.387): `A ⊂ B ⇒ P_A ≪ P_B`. -/
theorem cond_dominated_of_subset {P : Ω → ℝ} (hP : ∀ x, 0 ≤ P x) {A B : Finset Ω}
    (h : A ⊆ B) : Dominated (cond P A) (cond P B) := by
  intro x hx
  unfold cond at *
  by_cases hA : x ∈ A
  · have hB : x ∈ B := h hA
    simp only [hA, hB, if_true] at hx ⊢
    rcases div_eq_zero_iff.mp hx with hx | hx
    · simp [hx]
    · have : mass P A = 0 :=
        le_antisymm (hx ▸ mass_mono hP h) (mass_nonneg hP A)
      simp [this]
  · simp [hA]

/-- **Orthogonality** `P ⊥ Q` (p.388): some proposition is certain under `P`
and impossible under `Q`. -/
def Orth (P Q : Ω → ℝ) : Prop := ∃ A : Finset Ω, mass P A = 1 ∧ mass Q A = 0

/-- **Monotonicity (2)**, first formula (p.388): `A ∩ B = ∅ ⇒ P_A ⊥ P_B`, for
probabilistically possible `A`. -/
theorem cond_orth_of_disjoint (P : Ω → ℝ) {A B : Finset Ω}
    (hA : mass P A ≠ 0) (h : Disjoint A B) : Orth (cond P A) (cond P B) := by
  refine ⟨A, ?_, ?_⟩
  · rw [mass_cond, Finset.inter_self]; exact div_self hA
  · rw [mass_cond, Finset.disjoint_iff_inter_eq_empty.mp h]
    simp [mass]

/-- Convex mixing `P +_a Q` (p.388). -/
def mix (a : ℝ) (P Q : Ω → ℝ) : Ω → ℝ := fun x => a * P x + (1 - a) * Q x

theorem mass_mix (a : ℝ) (P Q : Ω → ℝ) (A : Finset Ω) :
    mass (mix a P Q) A = a * mass P A + (1 - a) * mass Q A := by
  unfold mass mix
  rw [Finset.sum_add_distrib, Finset.mul_sum, Finset.mul_sum]

/-- **The mixing law (3)** (p.388): `[P +_a Q]_A = P_A +_{a_A} Q_A` with
`a_A = a·P(A) : [P +_a Q](A)`.  "Standard conditioning commutes with convex
mixing"; what changes is the coefficient. -/
theorem cond_mix (a : ℝ) (P Q : Ω → ℝ) (A : Finset Ω) (hP : mass P A ≠ 0)
    (hQ : mass Q A ≠ 0) (hM : mass (mix a P Q) A ≠ 0) :
    cond (mix a P Q) A =
      mix (a * mass P A / mass (mix a P Q) A) (cond P A) (cond Q A) := by
  have hmm := mass_mix a P Q A
  have hM' := hM
  rw [hmm] at hM'
  funext x
  simp only [cond, mix] at *
  rw [hmm]
  split_ifs
  · field_simp; ring
  · ring

end General

/-! ## Jeffrey's conditional and the Jeffrey machine (p.386, p.392-396, p.399) -/

section Jeffrey

variable {Ω ι κ : Type*} [Fintype Ω] [DecidableEq ι] [DecidableEq κ]

/-- Mass of cell `i` of the partition `u`. -/
def cellMass (P : Ω → ℝ) (u : Ω → ι) (i : ι) : ℝ := ∑ y, if u y = i then P y else 0

/-- **Jeffrey's formula** (5) (p.392): `P'(H) = Σ_A p(A)·P_A(H)`, written on atoms,
"undefined conditionals put equal to zero". -/
noncomputable def jeffrey (P : Ω → ℝ) (u : Ω → ι) (p : ι → ℝ) : Ω → ℝ :=
  fun x => p (u x) * P x / cellMass P u (u x)

theorem cellMass_nonneg {P : Ω → ℝ} (hP : ∀ x, 0 ≤ P x) (u : Ω → ι) (i : ι) :
    0 ≤ cellMass P u i :=
  Finset.sum_nonneg fun y _ => by split_ifs <;> simp [hP y]

theorem le_cellMass {P : Ω → ℝ} (hP : ∀ x, 0 ≤ P x) (u : Ω → ι) (x : Ω) :
    P x ≤ cellMass P u (u x) := by
  unfold cellMass
  have := Finset.single_le_sum (f := fun y => if u y = u x then P y else 0)
    (fun y _ => by split_ifs <;> simp [hP y]) (Finset.mem_univ x)
  simpa using this

theorem cellMass_jeffrey (P : Ω → ℝ) (u : Ω → ι) (p : ι → ℝ) (i : ι)
    (h : cellMass P u i ≠ 0) : cellMass (jeffrey P u p) u i = p i := by
  have key : cellMass (jeffrey P u p) u i = p i / cellMass P u i * cellMass P u i := by
    conv_rhs => rw [cellMass, Finset.mul_sum]
    unfold cellMass jeffrey
    refine Finset.sum_congr rfl fun y _ => ?_
    by_cases hy : u y = i
    · simp only [hy, if_true]; unfold cellMass; ring
    · simp [hy]
  rw [key]; field_simp

/-- Summing a cell-dependent weight against `P` cell by cell. -/
theorem sum_by_cell [Fintype ι] (P : Ω → ℝ) (u : Ω → ι) (g : ι → ℝ) :
    ∑ x, g (u x) * P x = ∑ i, g i * cellMass P u i := by
  unfold cellMass
  simp_rw [Finset.mul_sum]
  rw [Finset.sum_comm]
  refine Finset.sum_congr rfl fun x _ => ?_
  simp [mul_ite]

/-- **Observation (i)** (p.396): `P' = P_{(U,p)} ⇒ P' ≪ P`: "zeros cannot be
revised". -/
theorem jeffrey_dominated (P : Ω → ℝ) (u : Ω → ι) (p : ι → ℝ) :
    Dominated (jeffrey P u p) P := by
  intro x hx; simp [jeffrey, hx]

/-- **Observation (iii)** (p.396): `P_{(U,p)}(H) = P(H)` if `H` is `P`-independent
of every element of `U` ("ancillary evidence does not change the current belief
state"). -/
theorem jeffrey_irrelevant [Fintype ι] [DecidableEq Ω] (P : Ω → ℝ) (u : Ω → ι) (p : ι → ℝ)
    (H : Finset Ω) (hpos : ∀ i, cellMass P u i ≠ 0) (hp : ∑ i, p i = 1)
    (hind : ∀ i, cellMass (fun x => if x ∈ H then P x else 0) u i =
      mass P H * cellMass P u i) :
    mass (jeffrey P u p) H = mass P H := by
  have e : mass (jeffrey P u p) H =
      ∑ x, (fun i => p i / cellMass P u i) (u x) * (fun x => if x ∈ H then P x else 0) x := by
    unfold mass jeffrey
    simp only [mul_ite, mul_zero]
    rw [Finset.sum_ite_mem, Finset.univ_inter]
    exact Finset.sum_congr rfl fun x _ => by ring
  rw [e, sum_by_cell (fun x => if x ∈ H then P x else 0) u (fun i => p i / cellMass P u i)]
  simp_rw [hind]
  calc ∑ i, p i / cellMass P u i * (mass P H * cellMass P u i)
      = ∑ i, mass P H * p i :=
        Finset.sum_congr rfl fun i _ => by have := hpos i; field_simp
    _ = mass P H := by rw [← Finset.mul_sum, hp, mul_one]

/-- The **Jeffrey machine** (p.393): inputs are finitary strings of
partition-probability pairs, composed by concatenation, and a string acts by
iterated application. -/
noncomputable def jeffreyRun (P : Ω → ℝ) (s : List ((Ω → ι) × (ι → ℝ))) : Ω → ℝ :=
  s.foldl (fun Q e => jeffrey Q e.1 e.2) P

/-- `(P', E_X, *)` with `E_X` the free monoid of strings is a machine: laws (a)
and (b) "hold by definition" (p.393). -/
noncomputable def jeffreyMachine : Machine (Ω → ℝ) (List ((Ω → ι) × (ι → ℝ))) where
  unit := []
  comp := (· ++ ·)
  act := jeffreyRun
  triviality _ := rfl
  composition P s t := by simp [jeffreyRun, List.foldl_append]

/-- The single-cell input `({X}, 1)` acts trivially on probabilities (p.393:
"Here `({X}, 1)` acts as a unit"). -/
theorem jeffrey_unit {P : Ω → ℝ} (h : ∑ x, P x = 1) :
    jeffrey P (fun _ => ()) (fun _ => 1) = P := by
  funext x; simp [jeffrey, cellMass, h]

/-- **The Markovian iteration law** (p.399): two inputs on the same partition,
`(P_A +_a P_Ā)_A +_b (P_A +_a P_Ā)_Ā = P_A +_b P_Ā`: the later input wins. -/
theorem jeffrey_markov (P : Ω → ℝ) (u : Ω → ι) (p q : ι → ℝ)
    (hm : ∀ x, cellMass P u (u x) ≠ 0) (hp : ∀ x, p (u x) ≠ 0) :
    jeffrey (jeffrey P u p) u q = jeffrey P u q := by
  funext x
  have h1 := cellMass_jeffrey P u p (u x) (hm x)
  unfold jeffrey at *
  rw [h1]
  have := hm x; have := hp x
  field_simp

/-- If a new belief is `P` rescaled by a factor constant on each cell of `f`,
it is the Jeffrey update of `P` on `f` with its own cell masses. -/
theorem jeffrey_of_cellwise {P Q : Ω → ℝ} (hP : ∀ x, 0 ≤ P x) (f : Ω → ι) (c : ι → ℝ)
    (hQ : ∀ x, Q x = c (f x) * P x) : Q = jeffrey P f (cellMass Q f) := by
  have hc : ∀ i, cellMass Q f i = c i * cellMass P f i := by
    intro i; unfold cellMass; rw [Finset.mul_sum]
    refine Finset.sum_congr rfl fun y _ => ?_
    by_cases hy : f y = i
    · simp [hy, hQ]
    · simp [hy]
  funext x
  unfold jeffrey
  rw [hc, hQ]
  by_cases hm : cellMass P f (f x) = 0
  · have : P x = 0 := le_antisymm (hm ▸ le_cellMass hP f x) (hP x)
    simp [this]
  · field_simp

/-- **An iterate is one Jeffrey update on the meet** (cf. p.393, "`P'` is defined
by the mixture `Σ p̄₁(A₁ ∩ … ∩ Aₙ)·P_{A₁∩…∩Aₙ}`", and p.395-396).  Two sequential
Jeffrey steps on `U` then `V` equal a single Jeffrey step on `U ∧ V` whose input
is the iterate's own distribution over the meet.  So the iterate always reduces
to one input on the meet; that input depends on `P`, not only on `(p, q)`. -/
theorem jeffrey_seq_on_meet {P : Ω → ℝ} (hP : ∀ x, 0 ≤ P x) (u : Ω → ι) (v : Ω → κ)
    (p : ι → ℝ) (q : κ → ℝ) :
    jeffrey (jeffrey P u p) v q =
      jeffrey P (fun x => (u x, v x))
        (cellMass (jeffrey (jeffrey P u p) v q) (fun x => (u x, v x))) := by
  apply jeffrey_of_cellwise hP _
    (fun ij => q ij.2 / cellMass (jeffrey P u p) v ij.2 * (p ij.1 / cellMass P u ij.1))
  intro x
  simp only [jeffrey]
  ring

/-! ### Non-commutativity (p.386, p.395, p.399) -/

/-- A 2×2 table on `Bool × Bool`, entries in the order `AB, AB̄, ĀB, ĀB̄`. -/
def tbl (ab anb nab nanb : ℝ) : Bool × Bool → ℝ
  | (true, true) => ab
  | (true, false) => anb
  | (false, true) => nab
  | (false, false) => nanb

/-- The input `(A, a)`: probability `a` on the cell labelled `true`. -/
noncomputable def two (a : ℝ) : Bool → ℝ := fun b => if b then a else 1 - a

/-- An explicit instance of "in a Jeffrey machine we have a noncommutative
transition from `P` to `P_{(A,a)}` and then to `[P_{(A,a)}]_{(B,b)}`" (p.399;
p.395: `[P_{(U,p)}]_{(V,q)} = [P_{(V,q)}]_{(U,p)}` "fails in general").  Prior
`P(AB) = P(ĀB̄) = 2/5`, `P(AB̄) = P(ĀB) = 1/10`; inputs `(A, 4/5)`, `(B, 1/5)`. -/
theorem jeffrey_not_comm :
    jeffrey (jeffrey (tbl (2/5) (1/10) (1/10) (2/5)) Prod.fst (two (4/5))) Prod.snd (two (1/5)) =
      tbl (16/85) (2/5) (1/85) (2/5) ∧
    jeffrey (jeffrey (tbl (2/5) (1/10) (1/10) (2/5)) Prod.snd (two (1/5))) Prod.fst (two (4/5)) =
      tbl (2/5) (2/5) (1/85) (16/85) ∧
    tbl (16/85) (2/5) (1/85) (2/5) ≠ tbl (2/5) (2/5) (1/85) (16/85) := by
  refine ⟨?_, ?_, ?_⟩
  · funext ⟨a, b⟩
    cases a <;> cases b <;>
      simp [jeffrey, cellMass, Fintype.sum_prod_type, tbl, two] <;> norm_num
  · funext ⟨a, b⟩
    cases a <;> cases b <;>
      simp [jeffrey, cellMass, Fintype.sum_prod_type, tbl, two] <;> norm_num
  · intro h
    have := congrFun h (true, true)
    simp [tbl] at this
    norm_num at this

end Jeffrey

/-! ## Field's conditional, Field machine, and the embedding (p.396-397) -/

section Field

variable {Ω ι κ : Type*} [Fintype Ω]

/-- The normalizing constant `α_X = Σ_A e^{α_A} P(A)`. -/
noncomputable def fieldZ (P : Ω → ℝ) (u : Ω → ι) (α : ι → ℝ) : ℝ :=
  ∑ y, Real.exp (α (u y)) * P y

/-- **Field's conditional** (p.397): `P'(H) = (1/α_X) Σ_A e^{α_A} P(A ∩ H)`, on
atoms. -/
noncomputable def fieldCond (P : Ω → ℝ) (u : Ω → ι) (α : ι → ℝ) : Ω → ℝ :=
  fun x => Real.exp (α (u x)) * P x / fieldZ P u α

theorem fieldZ_pos {P : Ω → ℝ} (hP : ∀ x, 0 ≤ P x) (h1 : ∑ x, P x = 1)
    (u : Ω → ι) (α : ι → ℝ) : 0 < fieldZ P u α := by
  unfold fieldZ
  have hnn : ∀ y ∈ (univ : Finset Ω), 0 ≤ Real.exp (α (u y)) * P y :=
    fun y _ => mul_nonneg (Real.exp_pos _).le (hP y)
  rcases (Finset.sum_nonneg hnn).lt_or_eq with h | h
  · exact h
  · exfalso
    have hz := (Finset.sum_eq_zero_iff_of_nonneg hnn).mp h.symm
    have : ∀ y ∈ (univ : Finset Ω), P y = 0 := fun y hy => by
      have := hz y hy
      rcases mul_eq_zero.mp this with h | h
      · exact absurd h (Real.exp_pos _).ne'
      · exact h
    rw [Finset.sum_eq_zero this] at h1
    exact zero_ne_one h1

/-- **Composition of Field inputs** (p.397):
`[P_{(U,α)}]_{(V,β)} = P_{(U ∧ V, α ∧ β)}`, `[α ∧ β]_{A∩B} = α_A + β_B` (p.396). -/
theorem field_comp (P : Ω → ℝ) (u : Ω → ι) (v : Ω → κ) (α : ι → ℝ) (β : κ → ℝ)
    (hZ : fieldZ P u α ≠ 0) :
    fieldCond (fieldCond P u α) v β =
      fieldCond P (fun x => (u x, v x)) (fun ij => α ij.1 + β ij.2) := by
  funext x
  unfold fieldCond fieldZ at *
  have h1 : ∑ y, Real.exp (β (v y)) * (Real.exp (α (u y)) * P y / ∑ y', Real.exp (α (u y')) * P y') =
      (∑ y, Real.exp (α (u y) + β (v y)) * P y) / ∑ y', Real.exp (α (u y')) * P y' := by
    rw [Finset.sum_div]
    exact Finset.sum_congr rfl fun y _ => by rw [Real.exp_add]; ring
  rw [h1, Real.exp_add]
  by_cases hT : ∑ y, Real.exp (α (u y) + β (v y)) * P y = 0
  · rw [hT]; simp
  · field_simp

/-- **Field's machine is commutative** (p.397). -/
theorem field_comm {P : Ω → ℝ} (hP : ∀ x, 0 ≤ P x) (h1 : ∑ x, P x = 1)
    (u : Ω → ι) (v : Ω → κ) (α : ι → ℝ) (β : κ → ℝ) :
    fieldCond (fieldCond P u α) v β = fieldCond (fieldCond P v β) u α := by
  rw [field_comp P u v α β (fieldZ_pos hP h1 u α).ne',
    field_comp P v u β α (fieldZ_pos hP h1 v β).ne']
  funext x
  unfold fieldCond fieldZ
  simp only [add_comm (α _) (β _)]

/-- **Clause (i)** (p.397): `P' = P_{(U,α)} ⇒ (P' ≪ P & P ≪ P')`: "zeros cannot
be created or destroyed". -/
theorem field_zero_iff (P : Ω → ℝ) (u : Ω → ι) (α : ι → ℝ) (hZ : fieldZ P u α ≠ 0)
    (x : Ω) : fieldCond P u α x = 0 ↔ P x = 0 := by
  unfold fieldCond
  rw [div_eq_zero_iff, mul_eq_zero]
  simp [hZ, (Real.exp_pos _).ne']

/-- Convex mixing, as in the Bayesian section. -/
def fmix (a : ℝ) (P Q : Ω → ℝ) : Ω → ℝ := fun x => a * P x + (1 - a) * Q x

/-- **Clause (ii) of p.397 is false.**  Domotor states
`(P +_a Q)_{(U,α)} = P_{(U,α)} +_a Q_{(U,α)}` with the *same* coefficient `a`.
Two points, `U` the partition into atoms, `α = (log 2, 0)`, `P, Q` the two
point masses, `a = 1/2`: the left side gives the first atom `2/3`, the right
side `1/2`. -/
theorem field_not_convex :
    fieldCond (fmix (1/2) (fun b : Bool => if b then 1 else 0) (fun b => if b then 0 else 1)) id
        (fun b => if b then Real.log 2 else 0) true ≠
      fmix (1/2) (fieldCond (fun b : Bool => if b then 1 else 0) id
          (fun b => if b then Real.log 2 else 0))
        (fieldCond (fun b : Bool => if b then 0 else 1) id
          (fun b => if b then Real.log 2 else 0)) true := by
  simp [fieldCond, fieldZ, fmix, Real.exp_log]
  norm_num

/-- **The corrected clause (ii)**: Field conditioning maps a mixture to the
mixture of the Field conditionals, with coefficient
`a·Z_P / (a·Z_P + (1-a)·Z_Q)`, the analogue of `a_A` in (3). -/
theorem field_mix (a : ℝ) (P Q : Ω → ℝ) (u : Ω → ι) (α : ι → ℝ)
    (hP : fieldZ P u α ≠ 0) (hQ : fieldZ Q u α ≠ 0)
    (hM : a * fieldZ P u α + (1 - a) * fieldZ Q u α ≠ 0) :
    fieldCond (fmix a P Q) u α =
      fmix (a * fieldZ P u α / (a * fieldZ P u α + (1 - a) * fieldZ Q u α))
        (fieldCond P u α) (fieldCond Q u α) := by
  have hZ : fieldZ (fmix a P Q) u α = a * fieldZ P u α + (1 - a) * fieldZ Q u α := by
    unfold fieldZ fmix
    rw [Finset.mul_sum, Finset.mul_sum, ← Finset.sum_add_distrib]
    exact Finset.sum_congr rfl fun y _ => by ring
  funext x
  unfold fieldCond
  rw [hZ]
  unfold fmix
  field_simp
  ring

variable [DecidableEq ι]

/-- **The embedding** `h_P : F_X → E_X`, `h_P(U,α) = (U,p)` with
`p(A) = e^{α_A} P(A) : α_X` (p.397). -/
noncomputable def embed (P : Ω → ℝ) (u : Ω → ι) (α : ι → ℝ) : ι → ℝ :=
  fun i => Real.exp (α i) * cellMass P u i / fieldZ P u α

/-- Jeffrey on `h_P(U,α)` is Field on `(U,α)`: the embedding represents Field's
conditional inside the Jeffrey machine, at the current state `P`. -/
theorem field_eq_jeffrey_embed {P : Ω → ℝ} (hP : ∀ x, 0 ≤ P x) (u : Ω → ι) (α : ι → ℝ) :
    fieldCond P u α = jeffrey P u (embed P u α) := by
  funext x
  unfold fieldCond jeffrey embed
  by_cases hm : cellMass P u (u x) = 0
  · have : P x = 0 := le_antisymm (hm ▸ le_cellMass hP u x) (hP x)
    simp [this]
  · field_simp

/-- "This embedding sends the unit `(U,0)` to `(U, P|_U)`" (p.397). -/
theorem embed_unit {P : Ω → ℝ} (h1 : ∑ x, P x = 1) (u : Ω → ι) :
    embed P u (fun _ => 0) = cellMass P u := by
  funext i; simp [embed, fieldZ, h1]

/-- The image of a fixed Field input depends on the current state: `α = (log 2, 0)`
on the two-atom partition gives `p = (2/3, 1/3)` at `P = (1/2, 1/2)` but
`p = (2/5, 3/5)` at `P = (1/4, 3/4)`. -/
theorem embed_depends_on_state :
    embed (fun b : Bool => if b then 1/2 else 1/2) id (fun b => if b then Real.log 2 else 0) true = 2/3 ∧
    embed (fun b : Bool => if b then 1/4 else 3/4) id (fun b => if b then Real.log 2 else 0) true = 2/5 := by
  constructor <;> simp [embed, fieldZ, cellMass, Real.exp_log] <;> norm_num

/-- Every strictly positive Jeffrey input on a partition whose cells all have
positive mass is `h_P` of a Field input (`α_A = log(p(A)/P(A))`).  For a single
input, then, Field's `F_X` covers all such Jeffrey inputs; the restriction the
paper points to (p.397) concerns compositions. -/
theorem embed_surjective [Fintype ι] (P : Ω → ℝ) (u : Ω → ι) (p : ι → ℝ)
    (hm : ∀ i, 0 < cellMass P u i) (hp : ∀ i, 0 < p i) (h1 : ∑ i, p i = 1) :
    embed P u (fun i => Real.log (p i / cellMass P u i)) = p := by
  have hZ : fieldZ P u (fun i => Real.log (p i / cellMass P u i)) = 1 := by
    unfold fieldZ
    rw [sum_by_cell P u (fun i => Real.exp (Real.log (p i / cellMass P u i)))]
    rw [← h1]
    refine Finset.sum_congr rfl fun i _ => ?_
    rw [Real.exp_log (div_pos (hp i) (hm i))]
    field_simp [(hm i).ne']
  funext i
  unfold embed
  rw [hZ, Real.exp_log (div_pos (hp i) (hm i))]
  field_simp [(hm i).ne']

end Field

end Literature.Domotor
