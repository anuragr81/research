/-
# Bonnay and Cozic, "Weighted averaging, Jeffrey conditioning and invariance",
# *Theory and Decision* (2018), doi:10.1007/s11238-017-9639-3

Read in full from Drive: `Weighted_averaging_Jeffrey_conditioning.pdf` (19 pp.).
Section and definition numbers are the paper's; page numbers are the PDF's
(the journal copy has no printed page numbers).

The paper axiomatizes three updating rules by one condition, invariance under
embedding (IE): Jeffrey conditioning as an individual rule (Theorem 1), weighted
averaging as a social rule on fully revealed priors (Theorem 2), and their
combination, extended weighted averaging, for priors revealed only on a
partition (Definition 6, Theorem 3).

## What is formalized

* **Jeffrey's rule on events** (§2, p. 4, and its partitional form p. 8):
  `J_U(p)(B) = Σ_i p(B|A_i) r_i` (`mass_kin`), with the shared `kin` of
  `Literature/Jeffrey.lean` as the rule on atoms.
* **Rigidity** (R) and within-cell ratio preservation (R′) (p. 8):
  `kin_rigid`, `kin_ratio`.
* **Theorem 1, (⇒)** (p. 6): Jeffrey conditioning is invariant under
  embedding.  An embedding of finite algebras is given on atoms by a
  surjection `π` from the atoms of the larger algebra to those of the smaller,
  with the smaller prior the pushforward of the larger (`push`).  The update of
  the pullback instruction pushes forward to the update (`kin_push`).
* **Theorem 1, (⇐)** (pp. 6-7), at the rational, non-degenerate ratios the
  proof treats: any rule with Success and IE assigns an atom `b` with
  `p(b)/p(A) = m/k`, `0 < m < k`, exactly the Jeffrey value `p(b|A) r`
  (`IE_forces_jeffrey`).  The proof follows the paper's: coarse-grain to three
  atoms `X, Y, Z`, refine to `k` equiprobable atoms in `A`, and use the swaps
  of those atoms as self-embeddings.
* **Weighted averaging** (Definition 5, p. 10) and **Theorem 2 (i)⇒(vii)**
  (p. 14): weighted averaging is invariant under embedding (`wa_push`); on
  events it is the same average (`mass_wa`).
* **Extended weighted averaging** (Definition 6, p. 10): the Jeffrey update to
  the averaged cell probabilities (`ewa`).  It reduces to weighted averaging on
  the revealed cells (`cellMass_ewa`, the paper's "when `A ∈ S ∩ S′`,
  `EF^W_λ` reduces to `F^W_λ`"), satisfies Definition 6's formula on events
  (`mass_ewa`), is Jeffrey conditioning at `λ = 1` (`ewa_one`), leaves the
  prior unchanged at `λ = 0` (`ewa_zero`), is plain weighted averaging when
  every atom is revealed (`ewa_id`), and is invariant under embedding
  (`ewa_push`, Theorem 3 (i)⇒(ii)).
* **The Joe/John example** (pp. 10-11): `.515` and `.65` (`joe_john`), and the
  general two-step gap `λ_a λ_b (y - x)` (`wa_two_step_gap`).
* **"Weighted averaging ... does not preserve independence"** (p. 15): the
  `λ`-mixture of two product `2×2` tables has association
  `λ(1-λ)(a-a′)(b-b′)` (`wa_assoc_product`).

## Not formalized

* Theorem 1 (⇐) at irrational ratios and at null atoms.  The paper closes
  the irrational case by continuity (p. 6).  Its rational argument also needs
  `0 < m < k`: with `m = 0` (a null atom) the block `X₁, …, X_m` is empty and
  the map onto `X` is not an embedding, and with `m = k` the block `Y` is
  empty.  Continuity (Definition 1) closes both; the paper does not say so.
* Theorem 2 (i)-(vi): the equivalences are McConway's (1981) and Wagner's
  (1982); the paper proves only (vii)⇒(vi) and (i)⇒(vii).  Theorem 3 (ii)⇒(i)
  combines Theorems 1 and 2 (ii)⇒(i).
* §5 (geometric averaging, Theorems 4-5).

## Consequences used in the discussion (not claims of the paper)

* `ewa_comm_of_indep`: extended weighted averaging on two partitions that are
  independent under the prior commutes, whatever weights the two cues carry.
  So when the weight belongs to the cue, a product prior gives no sequence
  effect; a sequence effect at independence needs the weight to depend on the
  cue's place in the sequence.
-/
import Mathlib
import Literature.Jeffrey

namespace Literature.BonnayCozic

open Finset Classical
open Literature.Jeffrey (cellMass kin sum_fiber cellMass_kin Indep cellMass_kin_of_indep
  kin_comm_of_indep)

variable {Ω Ω' ι κ : Type*} [Fintype Ω] [Fintype Ω']

/-! ## §2: Jeffrey conditioning -/

/-- The probability of the event `B`. -/
noncomputable def mass (p : Ω → ℝ) (B : Ω → Prop) : ℝ :=
  ∑ x, if B x then p x else 0

/-- §2, p. 4 and p. 8: `J_U(p)(B) = Σ_i p(B|A_i) r_i`, written with
`p(B|A_i) = p(B ∩ A_i) / p(A_i)`. -/
theorem mass_kin [Fintype ι] (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ) (B : Ω → Prop) :
    mass (kin p u q) B = ∑ i, mass p (fun x => B x ∧ u x = i) / cellMass p u i * q i := by
  unfold mass
  rw [sum_fiber u]
  refine Finset.sum_congr rfl fun i _ => ?_
  rw [Finset.sum_div, Finset.sum_mul]
  refine Finset.sum_congr rfl fun y _ => ?_
  by_cases hu : u y = i
  · subst hu
    by_cases hB : B y
    · simp only [hB, and_self, if_true, kin]; ring
    · simp [hB]
  · simp [hu]

/-- (R′), p. 8: the ratio of two atoms in one cell is unchanged. -/
theorem kin_ratio (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ) {x y : Ω} (h : u x = u y) :
    kin p u q x * p y = kin p u q y * p x := by
  simp only [kin, h]; ring

/-- (R), p. 8: probabilities conditional on a cell of the instruction are
unchanged, wherever the new cell probability is nonzero. -/
theorem kin_rigid (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ) (x : Ω)
    (hm : cellMass p u (u x) ≠ 0) (hq : q (u x) ≠ 0) :
    kin p u q x / cellMass (kin p u q) u (u x) = p x / cellMass p u (u x) := by
  rw [cellMass_kin p u q (u x) hm]
  unfold kin
  field_simp

/-- The prior of the coarser algebra induced by an embedding: the atom map
`π` sends each atom of the finer algebra to the atom of the coarser one whose
image contains it (Definition 2, probability preservation). -/
noncomputable def push (π : Ω' → Ω) (p' : Ω' → ℝ) : Ω → ℝ :=
  fun y => ∑ x, if π x = y then p' x else 0

/-- Cell masses are preserved along an embedding. -/
theorem cellMass_push (π : Ω' → Ω) (p' : Ω' → ℝ) (u : Ω → ι) (i : ι) :
    cellMass (push π p') u i = cellMass p' (u ∘ π) i := by
  unfold cellMass push
  have h : ∀ y, (if u y = i then ∑ x, (if π x = y then p' x else 0) else 0) =
      ∑ x, if π x = y then (if u y = i then p' x else 0) else 0 := by
    intro y
    by_cases hy : u y = i
    · simp [hy]
    · simp [hy]
  rw [Finset.sum_congr rfl fun y _ => h y, Finset.sum_comm]
  refine Finset.sum_congr rfl fun x _ => ?_
  rw [Finset.sum_ite_eq]
  simp

/-- **Theorem 1 (⇒)**, p. 6: Jeffrey conditioning is invariant under
embedding.  Updating the larger state on the pulled-back instruction and then
coarse-graining gives the update of the smaller state. -/
theorem kin_push (π : Ω' → Ω) (p' : Ω' → ℝ) (u : Ω → ι) (q : ι → ℝ) :
    push π (kin p' (u ∘ π) q) = kin (push π p') u q := by
  funext y
  show (∑ x, if π x = y then kin p' (u ∘ π) q x else 0) =
    q (u y) * push π p' y / cellMass (push π p') u (u y)
  rw [cellMass_push]
  unfold push
  rw [Finset.mul_sum, Finset.sum_div]
  refine Finset.sum_congr rfl fun x _ => ?_
  by_cases h : π x = y
  · subst h; simp [kin, Function.comp]
  · simp [h]

/-! ## §3-4: weighted averaging and its extension -/

/-- Definition 5, p. 10: weighted averaging, on atoms. -/
noncomputable def wa (l : ℝ) (p p' : Ω → ℝ) : Ω → ℝ :=
  fun x => (1 - l) * p x + l * p' x

/-- Definition 5 on events: `F^W_λ(p, p′)(A) = (1-λ) p(A) + λ p′(A)`. -/
theorem mass_wa (l : ℝ) (p p' : Ω → ℝ) (B : Ω → Prop) :
    mass (wa l p p') B = (1 - l) * mass p B + l * mass p' B := by
  unfold mass wa
  rw [Finset.mul_sum, Finset.mul_sum, ← Finset.sum_add_distrib]
  refine Finset.sum_congr rfl fun x _ => ?_
  by_cases h : B x <;> simp [h]

omit [Fintype Ω] in
/-- **Theorem 2 (i)⇒(vii)**, p. 14: weighted averaging is invariant under
embedding. -/
theorem wa_push (l : ℝ) (π : Ω' → Ω) (p₁ p₂ : Ω' → ℝ) :
    push π (wa l p₁ p₂) = wa l (push π p₁) (push π p₂) := by
  funext y
  unfold push wa
  rw [Finset.mul_sum, Finset.mul_sum, ← Finset.sum_add_distrib]
  refine Finset.sum_congr rfl fun x _ => ?_
  by_cases h : π x = y <;> simp [h]

/-- Definition 6, p. 10: extended weighted averaging.  The other agent reveals
only the probabilities `x i` of the cells of `u`.  The trade-off stage sets
cell `i` to `(1-λ) p(A_i) + λ x_i`, and Jeffrey conditioning adjusts the rest:
`EF^W_λ(p, p′)(A) = Σ_i p(A|A_i) F^W_λ(p, p′)(A_i)`. -/
noncomputable def ewa (l : ℝ) (p : Ω → ℝ) (u : Ω → ι) (x : ι → ℝ) : Ω → ℝ :=
  kin p u (fun i => (1 - l) * cellMass p u i + l * x i)

/-- p. 10: on a revealed cell, extended weighted averaging is weighted
averaging. -/
theorem cellMass_ewa (l : ℝ) (p : Ω → ℝ) (u : Ω → ι) (x : ι → ℝ) (i : ι)
    (hm : cellMass p u i ≠ 0) :
    cellMass (ewa l p u x) u i = (1 - l) * cellMass p u i + l * x i :=
  cellMass_kin p u _ i hm

/-- Definition 6's formula on events. -/
theorem mass_ewa [Fintype ι] (l : ℝ) (p : Ω → ℝ) (u : Ω → ι) (x : ι → ℝ) (B : Ω → Prop) :
    mass (ewa l p u x) B =
      ∑ i, mass p (fun y => B y ∧ u y = i) / cellMass p u i *
        ((1 - l) * cellMass p u i + l * x i) :=
  mass_kin p u _ B

/-- At `λ = 1` the revealed probabilities are adopted: Jeffrey conditioning. -/
theorem ewa_one (p : Ω → ℝ) (u : Ω → ι) (x : ι → ℝ) : ewa 1 p u x = kin p u x := by
  unfold ewa
  congr 1
  funext i
  ring

/-- At `λ = 0` the prior is kept. -/
theorem ewa_zero (p : Ω → ℝ) (u : Ω → ι) (x : ι → ℝ) (hm : ∀ i, cellMass p u i ≠ 0) :
    ewa 0 p u x = p := by
  funext y
  unfold ewa kin
  have := hm (u y)
  field_simp
  ring

/-- When every atom is revealed (the restricted case, `S′ = S`), extended
weighted averaging is weighted averaging. -/
theorem ewa_id (l : ℝ) (p x : Ω → ℝ) (hp : ∀ y, p y ≠ 0) : ewa l p id x = wa l p x := by
  have hc : ∀ y, cellMass p id y = p y := by
    intro y
    unfold cellMass
    rw [Finset.sum_eq_single y]
    · simp
    · intro z _ hz; simp [hz]
    · simp
  funext y
  unfold ewa kin wa
  simp only [id, hc]
  have := hp y
  field_simp

/-- **Theorem 3 (i)⇒(ii)**, p. 14: extended weighted averaging is invariant
under embedding. -/
theorem ewa_push (l : ℝ) (π : Ω' → Ω) (p' : Ω' → ℝ) (u : Ω → ι) (x : ι → ℝ) :
    push π (ewa l p' (u ∘ π) x) = ewa l (push π p') u x := by
  unfold ewa
  have h : (fun i => (1 - l) * cellMass p' (u ∘ π) i + l * x i) =
      (fun i => (1 - l) * cellMass (push π p') u i + l * x i) := by
    funext i
    rw [cellMass_push]
  rw [h, kin_push]

/-! ## §3: order dependence of weighted averaging -/

/-- pp. 10-11: prior `.5` for `A`; John's prior `.8`, weight `.5`; Joe's prior `.5`,
weight `.9`.  Meeting John then Joe gives `.515`; Joe then John gives `.65`. -/
theorem joe_john :
    (1 - 9/10 : ℝ) * ((1 - 1/2) * (1/2) + 1/2 * (8/10)) + 9/10 * (1/2) = 515/1000 ∧
    (1 - 1/2 : ℝ) * ((1 - 9/10) * (1/2) + 9/10 * (1/2)) + 1/2 * (8/10) = 65/100 := by
  norm_num

/-- Two weighted-averaging steps on one event, witness `a` (weight `λ_a`,
opinion `x`) and witness `b` (weight `λ_b`, opinion `y`): meeting `a` first
minus meeting `b` first is `λ_a λ_b (y - x)`.  The witness met last pulls the
result toward their opinion. -/
theorem wa_two_step_gap (p x y la lb : ℝ) :
    ((1 - lb) * ((1 - la) * p + la * x) + lb * y) -
      ((1 - la) * ((1 - lb) * p + lb * y) + la * x) = la * lb * (y - x) := by
  ring

/-- Joe/John is an instance: `.65 - .515 = .9 × .5 × (.8 - .5)`. -/
theorem joe_john_gap : (65/100 : ℝ) - 515/1000 = 9/10 * (1/2) * (8/10 - 1/2) := by
  norm_num

/-! ## §4: weighted averaging does not preserve independence (p. 15) -/

/-- The `λ`-mixture of the product tables with `P(A=0) = a, P(B=0) = b` and
`P(A=0) = a′, P(B=0) = b′` has association `λ(1-λ)(a-a′)(b-b′)`. -/
theorem wa_assoc_product (l a b a' b' : ℝ) :
    let m00 := (1 - l) * (a * b) + l * (a' * b')
    let m01 := (1 - l) * (a * (1 - b)) + l * (a' * (1 - b'))
    let m10 := (1 - l) * ((1 - a) * b) + l * ((1 - a') * b')
    let m11 := (1 - l) * ((1 - a) * (1 - b)) + l * ((1 - a') * (1 - b'))
    m00 * m11 - m01 * m10 = l * (1 - l) * (a - a') * (b - b') := by
  intro m00 m01 m10 m11
  simp only [m00, m01, m10, m11]
  ring

/-! ## Consequence used in the discussion: weights that belong to the cue -/

/-- Not a claim of the paper.  Extended weighted averaging on two partitions
independent under the prior commutes for any weights `λ`, `μ` attached to the
two cues. -/
theorem ewa_comm_of_indep [Fintype ι] [Fintype κ] (l μ : ℝ) (p : Ω → ℝ) (u : Ω → ι)
    (v : Ω → κ) (x : ι → ℝ) (y : κ → ℝ) (hp : ∑ z, p z = 1) (hx : ∑ i, x i = 1)
    (hy : ∑ j, y j = 1) (hmu : ∀ i, cellMass p u i ≠ 0) (hmv : ∀ j, cellMass p v j ≠ 0)
    (hind : Indep p u v) :
    ewa μ (ewa l p u x) v y = ewa l (ewa μ p v y) u x := by
  set tA : ι → ℝ := fun i => (1 - l) * cellMass p u i + l * x i with htA
  set tB : κ → ℝ := fun j => (1 - μ) * cellMass p v j + μ * y j with htB
  have hcu : ∑ i, cellMass p u i = 1 := by rw [← hp, sum_fiber u p]; rfl
  have hcv : ∑ j, cellMass p v j = 1 := by rw [← hp, sum_fiber v p]; rfl
  have hsA : ∑ i, tA i = 1 := by
    simp only [htA, Finset.sum_add_distrib, ← Finset.mul_sum, hcu, hx]; ring
  have hsB : ∑ j, tB j = 1 := by
    simp only [htB, Finset.sum_add_distrib, ← Finset.mul_sum, hcv, hy]; ring
  have h1 : ∀ j, cellMass (kin p u tA) v j = cellMass p v j :=
    cellMass_kin_of_indep p u v tA hsA hmu hind
  have h2 : ∀ i, cellMass (kin p v tB) u i = cellMass p u i :=
    cellMass_kin_of_indep p v u tB hsB hmv hind.symm
  show kin (kin p u tA) v (fun j => (1 - μ) * cellMass (kin p u tA) v j + μ * y j) =
    kin (kin p v tB) u (fun i => (1 - l) * cellMass (kin p v tB) u i + l * x i)
  simp only [h1, h2]
  exact kin_comm_of_indep p u v tA tB hsA hsB hmu hmv hind

/-! ## Theorem 1 (⇐), pp. 6-7: invariance under embedding forces Jeffrey's rule

Belief states have atoms `Fin n`.  An instruction `(A, r)` is a two-cell
partition `u : Fin n → Bool` (cell `true` is `A`) with new cell probabilities
`q`, `q true = r`, `q false = 1 - r`. -/

/-- An individual updating rule (Definition 1) on all finite algebras. -/
abbrev Rule := ∀ n : ℕ, (Fin n → ℝ) → (Fin n → Bool) → (Bool → ℝ) → Fin n → ℝ

/-- A probability on the atoms. -/
def IsProb {n : ℕ} (p : Fin n → ℝ) : Prop := (∀ x, 0 ≤ p x) ∧ ∑ x, p x = 1

/-- Definition 1's domain: a probability with `0 < p(A) < 1`. -/
def Dom {n : ℕ} (p : Fin n → ℝ) (u : Fin n → Bool) : Prop :=
  IsProb p ∧ ∀ c, 0 < cellMass p u c

/-- A new probability for `A` and its complement. -/
def Target (q : Bool → ℝ) : Prop := (∀ c, 0 ≤ q c) ∧ q true + q false = 1

/-- Definition 1, clause 2: the update gives `A` the probability `r`, and so
its complement `1 - r`. -/
def Success (F : Rule) : Prop :=
  ∀ n (p : Fin n → ℝ) u q, Dom p u → Target q → ∀ c, cellMass (F n p u q) u c = q c

/-- Definitions 2-3.  An embedding of the algebra with atoms `Fin n` into the
one with atoms `Fin n′` preserves unions and complements and is injective, so
it sends each atom to a nonempty union of atoms: read backwards it is a
surjection `π : Fin n′ → Fin n`.  Probability preservation makes the smaller
prior the pushforward of the larger, and `f(U)` is the pulled-back
instruction.  (IE) asks that the embedding also embed the updated states. -/
def IE (F : Rule) : Prop :=
  ∀ n n' (π : Fin n' → Fin n), Function.Surjective π → ∀ (p' : Fin n' → ℝ) u q,
    Dom p' (u ∘ π) → Target q → push π (F n' p' (u ∘ π) q) = F n (push π p') u q

/-- At a point whose fibre is a single atom, the pushforward is the value
there. -/
theorem push_single {n n' : ℕ} (π : Fin n' → Fin n) (g : Fin n' → ℝ) (b : Fin n')
    (h : ∀ x, π x = π b → x = b) : push π g (π b) = g b := by
  unfold push
  rw [Finset.sum_eq_single b]
  · simp
  · intro x _ hx
    have : π x ≠ π b := fun e => hx (h x e)
    simp [this]
  · simp

/-- Along a bijection, the pushforward is the value at the preimage. -/
theorem push_equiv {n : ℕ} (σ : Fin n ≃ Fin n) (g : Fin n → ℝ) (y : Fin n) :
    push σ g y = g (σ.symm y) := by
  unfold push
  rw [Finset.sum_eq_single (σ.symm y)]
  · simp
  · intro x _ hx
    have : σ x ≠ y := by intro h; apply hx; rw [← h]; simp
    simp [this]
  · simp

/-- Counting on `Fin (k+1)`: `Σ_i [i < m] a = m a` for `m ≤ k + 1`. -/
theorem sum_lt {k m : ℕ} (hm : m ≤ k + 1) (a : ℝ) :
    (∑ i : Fin (k + 1), if (i : ℕ) < m then a else 0) = m * a := by
  have h := Fin.sum_univ_eq_sum_range (fun i => if i < m then a else 0) (k + 1)
  rw [h, Finset.sum_ite, Finset.sum_const_zero, add_zero, Finset.sum_const, nsmul_eq_mul]
  have : (Finset.range (k + 1)).filter (fun i => i < m) = Finset.range m := by
    ext i; simp only [Finset.mem_filter, Finset.mem_range]; omega
  rw [this, Finset.card_range]

/-- A sum over the two cells, starting from either. -/
theorem bool_sum (f : Bool → ℝ) (c : Bool) : ∑ d, f d = f c + f (!c) := by
  cases c <;> simp [add_comm]

/-- **Theorem 1 (⇐)**, pp. 6-7, at rational, non-degenerate ratios.  A rule
with Success and (IE) gives an atom `b` with `p(b)/p(A_{u b}) = m/k`,
`0 < m < k`, its Jeffrey value `p(b|A_{u b}) q(u b)`. -/
theorem IE_forces_jeffrey (F : Rule) (hS : Success F) (hIE : IE F) (n : ℕ)
    (p : Fin n → ℝ) (u : Fin n → Bool) (q : Bool → ℝ) (hD : Dom p u) (hq : Target q)
    (b : Fin n) (m k : ℕ) (hm : 0 < m) (hmk : m < k)
    (hr : p b * k = m * cellMass p u (u b)) :
    F n p u q b = kin p u q b := by
  obtain ⟨c, hc⟩ : ∃ c, u b = c := ⟨_, rfl⟩
  obtain ⟨M, hM⟩ : ∃ M, cellMass p u c = M := ⟨_, rfl⟩
  obtain ⟨N, hN⟩ : ∃ N, cellMass p u (!c) = N := ⟨_, rfl⟩
  rw [hc, hM] at hr
  have hnc : ∀ d : Bool, d ≠ c → d = !c := by
    intro d hd; cases d <;> cases c <;> simp_all
  have hcn : c ≠ !c := by cases c <;> decide
  have hMpos : 0 < M := hM ▸ hD.2 c
  have hNpos : 0 < N := hN ▸ hD.2 (!c)
  have hk : 0 < k := lt_trans hm hmk
  have hkpos : (0 : ℝ) < k := by exact_mod_cast hk
  have hM0 : M ≠ 0 := ne_of_gt hMpos
  have hk0 : (k : ℝ) ≠ 0 := ne_of_gt hkpos
  have hMN : M + N = 1 := by
    have h := sum_fiber u p
    rw [hD.1.2] at h
    change 1 = ∑ d, cellMass p u d at h
    rw [bool_sum _ c, hM, hN] at h
    linarith
  have hpb : p b = m * M / k := by rw [eq_div_iff hk0]; exact hr
  -- the cell of `b` is `b` and the rest of the cell
  have hsplit : M = p b + ∑ y, if y ≠ b ∧ u y = c then p y else 0 := by
    rw [← hM]
    unfold cellMass
    trans ∑ y, ((if y = b then p b else 0) + (if y ≠ b ∧ u y = c then p y else 0))
    · refine Finset.sum_congr rfl fun y _ => ?_
      by_cases hyb : y = b
      · subst hyb; simp [hc]
      · simp [hyb]
    · rw [Finset.sum_add_distrib, Finset.sum_ite_eq']
      simp
  -- witnesses for the surjectivity of the coarse-graining
  have hx1 : ∃ y, y ≠ b ∧ u y = c := by
    by_contra h
    push Not at h
    have h0 : (∑ y, if y ≠ b ∧ u y = c then p y else 0) = 0 :=
      Finset.sum_eq_zero fun y _ => by
        by_cases hyb : y = b
        · simp [hyb]
        · simp [hyb, h y hyb]
    have hpbM : p b = M := by linarith [hsplit]
    rw [hpbM] at hr
    have hkm : (k : ℝ) = m := by
      apply mul_left_cancel₀ hM0
      linear_combination hr
    have : k = m := by exact_mod_cast hkm
    omega
  have hx2 : ∃ y, u y = !c := by
    by_contra h
    push Not at h
    have : N = 0 := by
      rw [← hN]; unfold cellMass
      exact Finset.sum_eq_zero fun y _ => by simp [h y]
    linarith
  obtain ⟨x1, hx1b, hx1c⟩ := hx1
  obtain ⟨x2, hx2c⟩ := hx2
  /- Step 1.  Coarse-grain to three atoms: `X = {b}`, `Y` = the rest of `b`'s
  cell, `Z` = the other cell. -/
  let π1 : Fin n → Fin 3 := fun y => if y = b then 0 else if u y = c then 1 else 2
  let u1 : Fin 3 → Bool := fun j => if j = 2 then !c else c
  have hcomp1 : u1 ∘ π1 = u := by
    funext y
    simp only [Function.comp, π1, u1]
    by_cases hyb : y = b
    · subst hyb; simp [hc]
    · by_cases hyc : u y = c
      · simp [hyb, hyc]
      · simp [hyb, hnc (u y) hyc]
  have hsurj1 : Function.Surjective π1 := by
    intro j
    fin_cases j
    · exact ⟨b, by simp [π1]⟩
    · exact ⟨x1, by simp [π1, hx1b, hx1c]⟩
    · refine ⟨x2, ?_⟩
      have hb2 : x2 ≠ b := by
        rintro rfl
        rw [hc] at hx2c
        exact hcn hx2c
      have hc2 : u x2 ≠ c := by rw [hx2c]; exact hcn.symm
      simp [π1, hb2, hc2]
  have hIE1 := hIE 3 n π1 hsurj1 p u1 q (by rw [hcomp1]; exact hD) hq
  rw [hcomp1] at hIE1
  have h0b : π1 b = 0 := by simp [π1]
  have hfib : ∀ x, π1 x = π1 b → x = b := by
    intro x hx
    by_contra hxb
    rw [h0b] at hx
    have : π1 x ≠ 0 := by
      simp only [π1, hxb, if_false]
      split_ifs <;> decide
    exact this hx
  have hstep1 : F n p u q b = F 3 (push π1 p) u1 q 0 := by
    rw [← congrFun hIE1 0, ← h0b, push_single π1 _ b hfib]
  have hp1_0 : push π1 p 0 = p b := by rw [← h0b, push_single π1 p b hfib]
  have hp1_1 : push π1 p 1 = M - p b := by
    have : push π1 p 1 = ∑ y, if y ≠ b ∧ u y = c then p y else 0 := by
      unfold push
      refine Finset.sum_congr rfl fun y _ => ?_
      by_cases hyb : y = b
      · subst hyb; simp [π1]
      · by_cases hyc : u y = c
        · simp [π1, hyb, hyc]
        · simp [π1, hyb, hyc]
    rw [this]; linarith [hsplit]
  have hp1_2 : push π1 p 2 = N := by
    rw [← hN]
    unfold push cellMass
    refine Finset.sum_congr rfl fun y _ => ?_
    by_cases hyb : y = b
    · subst hyb; simp [π1, hc, hcn]
    · by_cases hyc : u y = c
      · simp [π1, hyb, hyc, hcn]
      · simp [π1, hyb, hnc (u y) hyc]
  /- Step 2.  Refine to `k` equiprobable atoms in `b`'s cell, the first `m`
  of them making up `X`, and one atom for the other cell. -/
  let p2 : Fin (k + 1) → ℝ := fun i => if (i : ℕ) < k then M / k else N
  let u2 : Fin (k + 1) → Bool := fun i => if (i : ℕ) < k then c else !c
  let π2 : Fin (k + 1) → Fin 3 := fun i =>
    if (i : ℕ) < m then 0 else if (i : ℕ) < k then 1 else 2
  have hcomp2 : u1 ∘ π2 = u2 := by
    funext i
    simp only [Function.comp, π2, u1, u2]
    by_cases him : (i : ℕ) < m
    · have hik : (i : ℕ) < k := lt_trans him hmk
      simp [him, hik]
    · by_cases hik : (i : ℕ) < k
      · simp [him, hik]
      · simp [him, hik]
  have hsurj2 : Function.Surjective π2 := by
    intro j
    fin_cases j
    · exact ⟨⟨0, by omega⟩, by simp [π2, hm]⟩
    · exact ⟨⟨m, by omega⟩, by simp [π2, hmk]⟩
    · have : ¬ k < m := by omega
      exact ⟨⟨k, by omega⟩, by simp [π2, this]⟩
  have hcm2c : cellMass p2 u2 c = M := by
    unfold cellMass
    trans ∑ i : Fin (k + 1), (if (i : ℕ) < k then M / k else 0)
    · refine Finset.sum_congr rfl fun i _ => ?_
      by_cases h : (i : ℕ) < k
      · simp [u2, p2, h]
      · simp [u2, p2, h, hcn.symm]
    · rw [sum_lt (by omega) (M / k)]
      field_simp
  have hcm2n : cellMass p2 u2 (!c) = N := by
    unfold cellMass
    trans ∑ i : Fin (k + 1), (if i = Fin.last k then N else 0)
    · refine Finset.sum_congr rfl fun i _ => ?_
      by_cases h : (i : ℕ) < k
      · have hne : i ≠ Fin.last k := by
          intro e; rw [e, Fin.val_last] at h; omega
        simp [u2, p2, h, hcn, hne]
      · have he : i = Fin.last k := Fin.ext (by rw [Fin.val_last]; have := i.isLt; omega)
        subst he
        simp [u2, p2]
    · rw [Finset.sum_ite_eq']
      simp
  have hD2 : Dom p2 u2 := by
    refine ⟨⟨fun i => ?_, ?_⟩, fun d => ?_⟩
    · simp only [p2]
      split_ifs
      · exact div_nonneg (le_of_lt hMpos) (le_of_lt hkpos)
      · exact le_of_lt hNpos
    · rw [sum_fiber u2 p2]
      change ∑ d, cellMass p2 u2 d = 1
      rw [bool_sum _ c, hcm2c, hcm2n, hMN]
    · by_cases hd : d = c
      · rw [hd, hcm2c]; exact hMpos
      · rw [hnc d hd, hcm2n]; exact hNpos
  have hpush2 : push π2 p2 = push π1 p := by
    funext j
    fin_cases j
    · show push π2 p2 0 = push π1 p 0
      rw [hp1_0]
      unfold push
      trans ∑ i : Fin (k + 1), (if (i : ℕ) < m then M / k else 0)
      · refine Finset.sum_congr rfl fun i _ => ?_
        by_cases him : (i : ℕ) < m
        · have hik : (i : ℕ) < k := lt_trans him hmk
          simp [π2, p2, him, hik]
        · by_cases hik : (i : ℕ) < k <;> simp [π2, p2, him, hik]
      · rw [sum_lt (by omega) (M / k), hpb]
        ring
    · show push π2 p2 1 = push π1 p 1
      rw [hp1_1]
      unfold push
      trans ∑ i : Fin (k + 1),
        ((if (i : ℕ) < k then M / k else 0) - (if (i : ℕ) < m then M / k else 0))
      · refine Finset.sum_congr rfl fun i _ => ?_
        by_cases him : (i : ℕ) < m
        · have hik : (i : ℕ) < k := lt_trans him hmk
          simp [π2, p2, him, hik]
        · by_cases hik : (i : ℕ) < k <;> simp [π2, p2, him, hik]
      · rw [Finset.sum_sub_distrib, sum_lt (by omega) (M / k), sum_lt (by omega) (M / k), hpb]
        field_simp
    · show push π2 p2 2 = push π1 p 2
      rw [hp1_2]
      unfold push
      trans ∑ i : Fin (k + 1), (if i = Fin.last k then N else 0)
      · refine Finset.sum_congr rfl fun i _ => ?_
        by_cases hik : (i : ℕ) < k
        · have hne : i ≠ Fin.last k := by
            intro e; rw [e, Fin.val_last] at hik; omega
          by_cases him : (i : ℕ) < m <;> simp [π2, p2, him, hik, hne]
        · have he : i = Fin.last k := Fin.ext (by rw [Fin.val_last]; have := i.isLt; omega)
          subst he
          have : ¬ k < m := by omega
          simp [π2, p2, this]
      · rw [Finset.sum_ite_eq']
        simp
  have hIE2 := hIE 3 (k + 1) π2 hsurj2 p2 u1 q (by rw [hcomp2]; exact hD2) hq
  rw [hcomp2, hpush2] at hIE2
  /- Step 3.  Swapping two atoms of `b`'s cell is an embedding of the refined
  state into itself, so the update treats them alike. -/
  have hsym : ∀ i j : Fin (k + 1), (i : ℕ) < k → (j : ℕ) < k →
      F (k + 1) p2 u2 q j = F (k + 1) p2 u2 q i := by
    intro i j hi hj
    let σ := Equiv.swap i j
    have hkeep : ∀ x : Fin (k + 1), ((σ x : ℕ) < k ↔ (x : ℕ) < k) := by
      intro x
      simp only [σ, Equiv.swap_apply_def]
      split_ifs with h1 h2
      · subst h1; simp [hi, hj]
      · subst h2; simp [hi, hj]
      · rfl
    have hcompσ : u2 ∘ σ = u2 := by
      funext x
      simp only [Function.comp, u2, hkeep]
    have hpushσ : push σ p2 = p2 := by
      funext y
      rw [push_equiv σ p2 y]
      simp only [σ, Equiv.symm_swap]
      simp only [p2]
      rw [if_congr (hkeep y) rfl rfl]
    have hIEσ := hIE (k + 1) (k + 1) σ σ.surjective p2 u2 q (by rw [hcompσ]; exact hD2) hq
    rw [hcompσ, hpushσ] at hIEσ
    have h := congrFun hIEσ i
    rw [push_equiv σ _ i] at h
    simpa [σ] using h
  /- Step 4.  Success spreads `q c` evenly over the `k` atoms. -/
  have hval : ∀ i : Fin (k + 1), (i : ℕ) < k → F (k + 1) p2 u2 q i = q c / k := by
    intro i hi
    have hsucc := hS (k + 1) p2 u2 q hD2 hq c
    have hsum : cellMass (F (k + 1) p2 u2 q) u2 c = k * F (k + 1) p2 u2 q i := by
      unfold cellMass
      trans ∑ j : Fin (k + 1), (if (j : ℕ) < k then F (k + 1) p2 u2 q i else 0)
      · refine Finset.sum_congr rfl fun j _ => ?_
        by_cases hj : (j : ℕ) < k
        · simp [u2, hj, hsym i j hi hj]
        · simp [u2, hj, hcn.symm]
      · rw [sum_lt (by omega)]
    rw [hsum] at hsucc
    rw [eq_div_iff hk0]
    linear_combination hsucc
  /- Conclusion: the `m` atoms of `X` carry `m q(c) / k = p(b|A) q(c)`. -/
  have hlast := congrFun hIE2 0
  rw [hstep1, ← hlast]
  unfold push
  trans ∑ i : Fin (k + 1), (if (i : ℕ) < m then q c / k else 0)
  · refine Finset.sum_congr rfl fun i _ => ?_
    by_cases him : (i : ℕ) < m
    · have hik : (i : ℕ) < k := lt_trans him hmk
      simp [π2, him, hval i hik]
    · by_cases hik : (i : ℕ) < k <;> simp [π2, him, hik]
  rw [sum_lt (by omega)]
  unfold kin
  rw [hc, hM, hpb]
  field_simp

end Literature.BonnayCozic
