/-
# Pettigrew & Weisberg (2025), "Jeffrey Pooling"

*Philosophers' Imprint* 25(8), July 2025, pp. 1-16, doi 10.3998/phimp.3806.  Page references
are to the published pagination ("- 1 -" to "- 16 -"); every formula was read on the
rendered page.

## What is proved

**Binary rules.**
* `upco`: Equation (1), `P'(E) = P(E)Q(E) / [P(E)Q(E) + P(Ē)Q(Ē)]`; `linPool`, linear pooling.
  `opening_example`: 40% and 80% give 60% linearly and `8/11 ≈ 0.73` by upco (pp. 1-2).
* `field`: Equation (2), Field updating on `(E, β)`.  `eq3`: Equation (3), it is upco with
  `Q(E) = β/(β+1)`.  `no_prior_opinion`: **P-W's own main-text gloss (p. 7)** that with no
  prior opinion (`P(E) = 1/2`) you defer to `β/(β+1)`; this gloss is P-W's, not Field's (P-W's
  footnote 9 says only that Field uses a log-scaled `β`, his `α`).  `beta_is_bf`: "β just is the
  Bayes factor" (p. 9).

**Jeffrey pooling on finite partitions** (Definitions 1-3, Section 6).
* `upcoV`: Definition 2; `upcoV_binary`: it restricts to Equation (1).  `jpool g P E Q`:
  Definition 3, Step 1 pools with `g`, Step 2 Jeffrey-conditionalizes.
* `thm1`: **Theorem 1** (Field, p. 3): upco ensures that Jeffrey pooling commutes **for any
  regular `P`** (every `E_iF_j` positive) and any `Q`, `R`; stated for finite partitions (the
  paper's Theorem 1 is the binary case).  `thm5`: Theorem 5's "whenever defined" form;
  `route`: both orders equal `P(ω)Q(E_i)R(F_j)` renormalized.
* `example_P1`, `example_P2`, `example_other_order`: the p. 4 worked example exactly,
  `P'(E) = 8/11`, `P' = (6/11, 2/11, 1/11, 2/11)`, `P''(F) = 21/29`,
  `P'' = (18/29, 4/29, 3/29, 4/29)`; the other order passes through `(9/25, 2/25, 6/25, 8/25)` to
  the same `P''`.

**Theorem 2 / Theorem 8 (uniqueness), regular part.**
* `thm8_regular`: a pooling operator that is uniformity preserving (Def. 6), monotonic
  (Def. 7), symmetric (Def. 8), continuous (Def. 9) and extensional (Def. 10, built into the
  type `PoolOp n`), and makes Jeffrey pooling commute, **agrees with upco on regular inputs**.
  Via `lemma7` (**Lemma 7**, uniform distributions are neutral) and `wagner_grid` (**Theorem 6**,
  Wagner's necessity theorem as P-W state it, `E`-half).  For `n`-cell partitions; the
  main-text Theorem 2 is `n = 2`.
* What differs from the paper, precisely:
  1. An added hypothesis, `RegularityPreserving` (pooling regular distributions gives a
     regular distribution).  P-W's proof divides by pooled values in (6) and (8) and uses the
     pooled result as a probability function; this is the assumption that makes that legitimate.
  2. The hypotheses are only required on regular inputs, and commutativity only on the grid of
     two `n`-cell partitions with regular `P`, `Q`, `R`; this weakens the hypotheses.
  3. **Not formalized:** the last paragraph of Theorem 8's proof, which extends agreement from
     regular to non-regular inputs by continuity.
* `upco_regPres`, `upco_UP`, `upco_mono`, `upco_symm`, `upco_cont`, `upco_commutes`: upco
  satisfies every hypothesis, including the added one, so `thm8_regular` is not vacuous.

## The project's question, not P-W's

* `upco_eq_self_iff`: `upco(p, q) = q` iff `p = 1/2`.  `naive_opinion`: the opinion upco must
  pool with the prior `p` to return a delivered credence `q` is `β/(β+1) = q(1-p)/(p+q-2pq)`,
  equal to `q` only at `p = 1/2`.
* `PB_is_upco_pooling`: Paper B's `P^B` **is** Jeffrey pooling with upco, in either order,
  when each cue's pooled opinion is its matched likelihood against the **prior** marginal
  (`matchedOpinion`), not its delivered credence.  `PB_vs_example`: on P-W's own p. 4 numbers
  read as two delivered credences, pooling the credences themselves gives `18/29` at `EF`,
  `P^B` gives `27/40`.
* P-W pool the prior with each source's opinion; they do not pool successive inputs with
  each other.
-/
import Mathlib

namespace Literature.PettigrewWeisberg

set_option linter.unusedSectionVars false

open Finset Filter Topology

/-! ## Binary pooling rules (Section 1) and Field updating (Section 3) -/

/-- Linear pooling (p. 3). -/
noncomputable def linPool (x y : ℝ) : ℝ := (x + y) / 2

/-- Upco, Equation (1) (p. 3): `P'(E) = P(E)Q(E) / [P(E)Q(E) + P(Ē)Q(Ē)]`. -/
noncomputable def upco (x y : ℝ) : ℝ := x * y / (x * y + (1 - x) * (1 - y))

/-- The opening example (pp. 1-2): linear pooling of 40% and 80% gives 60%; upco gives
`8/11 ≈ 0.73`. -/
theorem opening_example :
    linPool 0.4 0.8 = 0.6 ∧ upco 0.4 0.8 = 8 / 11 ∧ |(8 / 11 : ℝ) - 0.73| < 0.005 := by
  unfold linPool upco; norm_num [abs_lt]

/-- Field updating, Equation (2) (p. 7): `P'(E) = β P(E) / (β P(E) + P(Ē))`, `β ≥ 0`. -/
noncomputable def field (β x : ℝ) : ℝ := β * x / (β * x + (1 - x))

/-- Equation (3) (p. 7): Field updating on `(E, β)` is upco with `Q(E) = β/(β+1)`,
`Q(Ē) = 1/(β+1)`. -/
theorem eq3 {β : ℝ} (hβ : 0 ≤ β) (x : ℝ) : field β x = upco x (β / (β + 1)) := by
  unfold field upco
  have h1 : β + 1 ≠ 0 := by linarith
  have e : 1 - β / (β + 1) = 1 / (β + 1) := by field_simp; ring
  rw [e, show x * (β / (β + 1)) + (1 - x) * (1 / (β + 1)) = (β * x + (1 - x)) / (β + 1) by
    field_simp, show x * (β / (β + 1)) = (β * x) / (β + 1) by ring,
    div_div_div_cancel_right₀ h1]

/-- P-W's own gloss (p. 7, main text): with no prior opinion (`P(E) = P(Ē) = 1/2`),
Equation (3) delivers `β/(β+1)`, i.e. upco with a uniform prior defers to the pooled
opinion. -/
theorem no_prior_opinion {β : ℝ} (hβ : 0 ≤ β) :
    field β (1 / 2) = β / (β + 1) ∧ ∀ y, upco (1 / 2) y = y := by
  refine ⟨?_, fun y => ?_⟩
  · unfold field; have : β + 1 ≠ 0 := by linarith
    field_simp; ring
  · unfold upco; field_simp; ring

/-- The binary Bayes factor `[P'(E)/P'(Ē)] / [P(E)/P(Ē)]` (p. 8). -/
noncomputable def bf2 (x' x : ℝ) : ℝ := (x' / (1 - x')) / (x / (1 - x))

/-- p. 9: "β just is the Bayes factor": solving (2) for `β`. -/
theorem beta_is_bf {β x : ℝ} (hβ : 0 < β) (hx0 : 0 < x) (hx1 : x < 1) :
    bf2 (field β x) x = β := by
  unfold bf2 field
  have h1 : 0 < 1 - x := by linarith
  have hd : 0 < β * x + (1 - x) := by positivity
  have e : 1 - β * x / (β * x + (1 - x)) = (1 - x) / (β * x + (1 - x)) := by field_simp; ring
  rw [e, div_div_div_cancel_right₀ hd.ne']
  field_simp

/-! ## The project's question, binary case: `P^B` is not `upco(prior, delivered credence)` -/

/-- `upco(p, q) = q` iff `p = 1/2` (for `0 < p, q < 1`).  So pooling the prior with the
delivered credence itself does not return the delivered credence. -/
theorem upco_eq_self_iff {p q : ℝ} (hp0 : 0 < p) (hp1 : p < 1) (hq0 : 0 < q) (hq1 : q < 1) :
    upco p q = q ↔ p = 1 / 2 := by
  unfold upco
  have hd : 0 < p * q + (1 - p) * (1 - q) := by
    have h1 := mul_pos hp0 hq0
    have h2 := mul_pos (sub_pos.2 hp1) (sub_pos.2 hq1)
    linarith
  rw [div_eq_iff hd.ne']
  constructor
  · intro h
    have : q * (1 - q) * (2 * p - 1) = 0 := by linear_combination h
    rcases mul_eq_zero.1 this with h' | h'
    · rcases mul_eq_zero.1 h' with h'' | h'' <;> linarith
    · linarith
  · rintro rfl; ring

/-- The opinion that upco must pool with the prior `p` to return the delivered credence `q`
is `Q(E) = β/(β+1)` with `β` the Bayes factor of `p → q` (Field's reading, via Equation (3));
it equals `q(1-p)/(p+q-2pq)`, and it is `q` only when `p = 1/2`. -/
theorem naive_opinion {p q : ℝ} (hp0 : 0 < p) (hp1 : p < 1) (hq0 : 0 < q) (hq1 : q < 1) :
    let β := bf2 q p
    upco p (β / (β + 1)) = q ∧ β / (β + 1) = q * (1 - p) / (p + q - 2 * p * q) ∧
      (β / (β + 1) = q ↔ p = 1 / 2) := by
  intro β
  have h1p : 0 < 1 - p := by linarith
  have h1q : 0 < 1 - q := by linarith
  have hβ : β = q * (1 - p) / (p * (1 - q)) := by
    simp only [β, bf2]; field_simp
  have hβpos : 0 < β := by rw [hβ]; positivity
  have hD : 0 < p + q - 2 * p * q := by nlinarith [mul_pos hp0 h1q, mul_pos hq0 h1p]
  have hform : β / (β + 1) = q * (1 - p) / (p + q - 2 * p * q) := by
    have hDen : p * (1 - q) ≠ 0 := by positivity
    rw [hβ, div_add_one hDen, div_div_div_cancel_right₀ hDen]; congr 1; ring
  refine ⟨?_, hform, ?_⟩
  · rw [← eq3 hβpos.le]
    have hf : field β p = β * p / (β * p + (1 - p)) := rfl
    have hn : β * p = q * (1 - p) / (1 - q) := by rw [hβ]; field_simp
    have hd : β * p + (1 - p) = (1 - p) / (1 - q) := by rw [hn]; field_simp; ring
    rw [hf, hd, hn, div_div_div_cancel_right₀ h1q.ne', mul_div_cancel_right₀ _ h1p.ne']
  · rw [hform, div_eq_iff hD.ne']
    constructor
    · intro h
      have : q * (1 - q) * (2 * p - 1) = 0 := by linear_combination -h
      rcases mul_eq_zero.1 this with h' | h'
      · rcases mul_eq_zero.1 h' with h'' | h'' <;> linarith
      · linarith
    · rintro rfl; ring

/-! ## Jeffrey pooling on finite partitions (Definitions 1-3, Section 6) -/

variable {Ω ι κ : Type*} [Fintype Ω] [DecidableEq Ω] [Fintype ι] [DecidableEq ι]
  [Fintype κ] [DecidableEq κ]

def IsProb (P : Ω → ℝ) : Prop := (∀ ω, 0 ≤ P ω) ∧ ∑ ω, P ω = 1

noncomputable def pr (P : Ω → ℝ) (A : Finset Ω) : ℝ := ∑ ω ∈ A, P ω

def cell (E : Ω → ι) (i : ι) : Finset Ω := univ.filter (fun ω => E ω = i)

@[simp] theorem mem_cell {E : Ω → ι} {i : ι} {ω : Ω} : ω ∈ cell E i ↔ E ω = i := by simp [cell]

/-- The marginal of `P` on the partition `E`. -/
noncomputable def marg (P : Ω → ℝ) (E : Ω → ι) : ι → ℝ := fun i => pr P (cell E i)

/-- Definition 2: upco over a partition, `⟨PQ⟩^U_E(E_i) = P(E_i)Q(E_i) / Σ_j P(E_j)Q(E_j)`,
with `P` and `Q` given by their values on the cells. -/
noncomputable def upcoV (P Q : ι → ℝ) (i : ι) : ℝ := P i * Q i / ∑ j, P j * Q j

theorem upcoV_binary (x y : ℝ) : upcoV ![x, 1 - x] ![y, 1 - y] 0 = upco x y := by
  simp [upcoV, upco, Fin.sum_univ_two]

/-- Definition 3 (Jeffrey pooling), with a pooling operator `g` on the partition:
Step 1 pools `P(E)` with `Q(E)`, Step 2 Jeffrey-conditionalizes,
`P'(ω) = P(ω | E_i) · ⟨PQ⟩_E(E_i)` for `ω ∈ E_i`. -/
noncomputable def jpool (g : (ι → ℝ) → (ι → ℝ) → ι → ℝ) (P : Ω → ℝ) (E : Ω → ι) (Q : ι → ℝ) :
    Ω → ℝ :=
  fun ω => P ω / marg P E (E ω) * g (marg P E) Q (E ω)

theorem le_marg {P : Ω → ℝ} (hP : ∀ ω, 0 ≤ P ω) (E : Ω → ι) (ω : Ω) : P ω ≤ marg P E (E ω) :=
  single_le_sum (f := P) (fun ω' _ => hP ω') (by simp : ω ∈ cell E (E ω))

theorem sum_marg (P : Ω → ℝ) (E : Ω → ι) : ∑ i, marg P E i = ∑ ω, P ω := by
  unfold marg pr cell; exact sum_fiberwise univ E P

theorem sum_fiber_mul (P : Ω → ℝ) (E : Ω → ι) (f : ι → ℝ) :
    ∑ ω, P ω * f (E ω) = ∑ i, marg P E i * f i := by
  rw [← sum_fiberwise univ E (fun ω => P ω * f (E ω))]
  refine sum_congr rfl fun i _ => ?_
  unfold marg pr cell; rw [sum_mul]
  exact sum_congr rfl fun ω hω => by rw [(mem_filter.1 hω).2]

/-- Jeffrey pooling with upco multiplies each atom by the pooled opinion of its cell:
`⟪PQ⟫^U_E(ω) = P(ω) Q(E_i) / Σ_j P(E_j)Q(E_j)` (the display in the proof of Theorem 5). -/
theorem jpool_upco {P : Ω → ℝ} (hP : ∀ ω, 0 ≤ P ω) (E : Ω → ι) (Q : ι → ℝ) (ω : Ω) :
    jpool upcoV P E Q ω = P ω * Q (E ω) / ∑ i, marg P E i * Q i := by
  unfold jpool upcoV
  by_cases hm : marg P E (E ω) = 0
  · have : P ω = 0 := le_antisymm (hm ▸ le_marg hP E ω) (hP ω)
    simp [this]
  · field_simp

theorem jpool_upco_nonneg {P : Ω → ℝ} (hP : ∀ ω, 0 ≤ P ω) (E : Ω → ι) {Q : ι → ℝ}
    (hQ : ∀ i, 0 ≤ Q i) (ω : Ω) : 0 ≤ jpool upcoV P E Q ω := by
  rw [jpool_upco hP]
  exact div_nonneg (mul_nonneg (hP ω) (hQ _))
    (sum_nonneg fun i _ => mul_nonneg (sum_nonneg fun ω _ => hP ω) (hQ i))

/-- One route of Theorem 5: pooling on `E` then on `F` gives `P(ω)Q(E_i)R(F_j)`
renormalized, provided the first upco step is defined. -/
theorem route {P : Ω → ℝ} (hP : ∀ ω, 0 ≤ P ω) (E : Ω → ι) (F : Ω → κ) {Q : ι → ℝ} {R : κ → ℝ}
    (hQ : ∀ i, 0 ≤ Q i) (hZE : ∑ i, marg P E i * Q i ≠ 0) (ω : Ω) :
    jpool upcoV (jpool upcoV P E Q) F R ω =
      P ω * Q (E ω) * R (F ω) / ∑ ω', P ω' * Q (E ω') * R (F ω') := by
  have h1 : ∀ ω, jpool upcoV P E Q ω = P ω * Q (E ω) / ∑ i, marg P E i * Q i :=
    jpool_upco hP E Q
  rw [jpool_upco (jpool_upco_nonneg hP E hQ) F R ω, ← sum_fiber_mul _ F R]
  simp_rw [h1, div_mul_eq_mul_div, ← sum_div]
  rw [div_div_div_cancel_right₀ hZE]

/-- **Theorem 5** (Field), for finite partitions: whenever the first upco step of each route
is defined, Jeffrey pooling with upco commutes, and both orders give
`P(ω)Q(E_i)R(F_j)` renormalized ("ultimately, all we did was multiply", p. 4).  (If the
second-step normalizer vanished, both sides would be Lean's `x/0 = 0`; for regular `P` it does
not, see `thm1`.) -/
theorem thm5 {P : Ω → ℝ} (hP : ∀ ω, 0 ≤ P ω) (E : Ω → ι) (F : Ω → κ) {Q : ι → ℝ} {R : κ → ℝ}
    (hQ : ∀ i, 0 ≤ Q i) (hR : ∀ j, 0 ≤ R j)
    (hZE : ∑ i, marg P E i * Q i ≠ 0) (hZF : ∑ j, marg P F j * R j ≠ 0) :
    jpool upcoV (jpool upcoV P E Q) F R = jpool upcoV (jpool upcoV P F R) E Q := by
  funext ω
  rw [route hP E F hQ hZE, route hP F E hR hZF]
  congr 1
  · ring
  · exact sum_congr rfl fun ω' _ => by ring

theorem nonempty_of_prob {P : Ω → ℝ} (hP : IsProb P) : Nonempty Ω := by
  by_contra h; rw [not_nonempty_iff] at h; have := hP.2; simp at this

theorem normalizer_pos {P : Ω → ℝ} (E : Ω → ι) {Q : ι → ℝ}
    (hm : ∀ i, 0 < marg P E i) (hQ : ∀ i, 0 ≤ Q i) (hQ1 : ∑ i, Q i = 1) :
    0 < ∑ i, marg P E i * Q i := by
  obtain ⟨i, -, hi⟩ : ∃ i ∈ univ, 0 < Q i := by
    by_contra h; push Not at h
    have : ∑ i, Q i ≤ 0 := sum_nonpos fun i hi => h i hi
    linarith
  exact sum_pos' (fun k _ => mul_nonneg (hm k).le (hQ k)) ⟨i, mem_univ i, mul_pos (hm i) hi⟩

/-- **Theorem 1** (Field; P-W p. 3): upco ensures that Jeffrey pooling commutes for any regular
`P` (every cell `E_i F_j` has positive probability) and any `Q` and `R`.  Stated for finite
partitions; P-W's Theorem 1 is the binary case `ι = κ = Fin 2`. -/
theorem thm1 {P : Ω → ℝ} (hP : IsProb P) (E : Ω → ι) (F : Ω → κ)
    (hreg : ∀ i j, 0 < pr P (cell E i ∩ cell F j)) {Q : ι → ℝ} {R : κ → ℝ}
    (hQ : ∀ i, 0 ≤ Q i) (hQ1 : ∑ i, Q i = 1) (hR : ∀ j, 0 ≤ R j) (hR1 : ∑ j, R j = 1) :
    jpool upcoV (jpool upcoV P E Q) F R = jpool upcoV (jpool upcoV P F R) E Q := by
  obtain ⟨ω₀⟩ := nonempty_of_prob hP
  have hsub : ∀ {A B : Finset Ω}, A ⊆ B → pr P A ≤ pr P B := fun h =>
    sum_le_sum_of_subset_of_nonneg h fun ω _ _ => hP.1 ω
  have hmE : ∀ i, 0 < marg P E i := fun i =>
    lt_of_lt_of_le (hreg i (F ω₀)) (hsub inter_subset_left)
  have hmF : ∀ j, 0 < marg P F j := fun j =>
    lt_of_lt_of_le (hreg (E ω₀) j) (hsub inter_subset_right)
  exact thm5 hP.1 E F hQ hR (normalizer_pos E hmE hQ hQ1).ne'
    (normalizer_pos F hmF hR hR1).ne'

/-! ### The worked example of p. 4

`Fin 2 × Fin 2`, with `0` for `E` (resp. `F`) and `1` for `Ē` (resp. `F̄`): cells
`EF, EF̄, ĒF, ĒF̄` are `(0,0), (0,1), (1,0), (1,1)`. -/

theorem marg_fst (P : Fin 2 × Fin 2 → ℝ) (i : Fin 2) : marg P Prod.fst i = P (i, 0) + P (i, 1) := by
  unfold marg pr cell; rw [sum_filter, Fintype.sum_prod_type]
  fin_cases i <;> simp [Fin.sum_univ_two]

theorem marg_snd (P : Fin 2 × Fin 2 → ℝ) (j : Fin 2) : marg P Prod.snd j = P (0, j) + P (1, j) := by
  unfold marg pr cell; rw [sum_filter, Fintype.sum_prod_type]
  fin_cases j <;> simp [Fin.sum_univ_two]

/-- A table `![![a, b], ![c, d]]` read as a function on `Fin 2 × Fin 2`. -/
def tbl (a b c d : ℝ) : Fin 2 × Fin 2 → ℝ := fun ω => ![![a, b], ![c, d]] ω.1 ω.2

noncomputable def exP : Fin 2 × Fin 2 → ℝ := tbl (3/10) (1/10) (2/10) (4/10)
noncomputable def exQ : Fin 2 → ℝ := ![8/10, 2/10]
noncomputable def exR : Fin 2 → ℝ := ![6/10, 4/10]

theorem jpool_tbl_fst (a b c d : ℝ) (Q : Fin 2 → ℝ) :
    jpool upcoV (tbl a b c d) Prod.fst Q = fun ω =>
      tbl a b c d ω / marg (tbl a b c d) Prod.fst ω.1 *
        upcoV (marg (tbl a b c d) Prod.fst) Q ω.1 := rfl

/-- p. 4, `P` pooled with `Q` first: `P'(E) = 8/11`, `P' = (6/11, 2/11, 1/11, 2/11)`. -/
theorem example_P1 :
    upco (4/10) (8/10) = 8/11 ∧
      jpool upcoV exP Prod.fst exQ = tbl (6/11) (2/11) (1/11) (2/11) := by
  refine ⟨by unfold upco; norm_num, funext fun ω => ?_⟩
  rcases ω with ⟨i, j⟩
  simp only [jpool, upcoV, marg_fst, Fin.sum_univ_two]
  fin_cases i <;> fin_cases j <;> simp [exP, exQ, tbl] <;> norm_num

/-- p. 4, then pooled with `R`: `P'(F) = 7/11`, `P''(F) = 21/29`,
`P'' = (18/29, 4/29, 3/29, 4/29)`. -/
theorem example_P2 :
    marg (tbl (6/11) (2/11) (1/11) (2/11)) Prod.snd 0 = 7/11 ∧ upco (7/11) (6/10) = 21/29 ∧
      jpool upcoV (jpool upcoV exP Prod.fst exQ) Prod.snd exR =
        tbl (18/29) (4/29) (3/29) (4/29) := by
  refine ⟨by rw [marg_snd]; simp [tbl]; norm_num, by unfold upco; norm_num, ?_⟩
  rw [example_P1.2]; funext ω; rcases ω with ⟨i, j⟩
  simp only [jpool, upcoV, marg_snd, Fin.sum_univ_two]
  fin_cases i <;> fin_cases j <;> simp [exR, tbl] <;> norm_num

/-- p. 4, the other order: `P` pooled with `R` first gives `(9/25, 2/25, 6/25, 8/25)`, and
then with `Q` gives the same `P''`. -/
theorem example_other_order :
    jpool upcoV exP Prod.snd exR = tbl (9/25) (2/25) (6/25) (8/25) ∧
      jpool upcoV (jpool upcoV exP Prod.snd exR) Prod.fst exQ =
        tbl (18/29) (4/29) (3/29) (4/29) := by
  have h1 : jpool upcoV exP Prod.snd exR = tbl (9/25) (2/25) (6/25) (8/25) := by
    funext ω; rcases ω with ⟨i, j⟩
    simp only [jpool, upcoV, marg_snd, Fin.sum_univ_two]
    fin_cases i <;> fin_cases j <;> simp [exP, exR, tbl] <;> norm_num
  refine ⟨h1, ?_⟩
  rw [h1]; funext ω; rcases ω with ⟨i, j⟩
  simp only [jpool, upcoV, marg_fst, Fin.sum_univ_two]
  fin_cases i <;> fin_cases j <;> simp [exQ, tbl] <;> norm_num

/-! ## Theorem 2 (Theorem 8 of the appendix): only upco, on regular inputs -/

section Uniqueness

variable {n : ℕ}

/-- A regular probability vector on an `n`-cell partition. -/
def PosProb (v : Fin n → ℝ) : Prop := (∀ i, 0 < v i) ∧ ∑ i, v i = 1

/-- The uniform distribution on `n` cells. -/
noncomputable def unif (n : ℕ) : Fin n → ℝ := fun _ => 1 / n

/-- A pooling operator in extensional form (Definition 10): on a partition of size `n` the
pooled distribution depends only on the two vectors of cell probabilities.  The same `g` is
used for `E` and for `F`. -/
abbrev PoolOp (n : ℕ) := (Fin n → ℝ) → (Fin n → ℝ) → Fin n → ℝ

/-- Definition 6 (uniformity preservation). -/
def UniformityPreserving (g : PoolOp n) : Prop :=
  ∀ i k, g (unif n) (unif n) i = g (unif n) (unif n) k

/-- Definition 7 (monotonicity), on regular inputs. -/
def Monotone' (g : PoolOp n) : Prop :=
  ∀ Q R, PosProb Q → PosProb R → ∀ i, Q i < R i → g (unif n) Q i < g (unif n) R i

/-- Definition 8 (symmetry), on regular inputs. -/
def Symmetric (g : PoolOp n) : Prop := ∀ P Q, PosProb P → PosProb Q → g P Q = g Q P

/-- Definition 9 (continuity), for sequences of regular inputs with a regular limit. -/
def ContinuousOnReg (g : PoolOp n) : Prop :=
  ∀ (P : ℕ → Fin n → ℝ) (Pinf Q : Fin n → ℝ), (∀ m, PosProb (P m)) → PosProb Pinf → PosProb Q →
    Tendsto P atTop (𝓝 Pinf) → Tendsto (fun m => g (P m) Q) atTop (𝓝 (g Pinf Q))

/-- **Added hypothesis, not in P-W:** pooling two regular distributions gives a regular
distribution.  P-W's proofs of Lemma 7 and Theorem 8 divide by pooled values (Equations (6)
and (8)) and treat pooled results as probability functions, which uses this silently. -/
def RegularityPreserving (g : PoolOp n) : Prop := ∀ P Q, PosProb P → PosProb Q → PosProb (g P Q)

/-- A regular distribution on the grid `E × F` of two `n`-cell partitions. -/
def Reg (P : Fin n → Fin n → ℝ) : Prop := (∀ i j, 0 < P i j) ∧ ∑ i, ∑ j, P i j = 1

def mE (P : Fin n → Fin n → ℝ) : Fin n → ℝ := fun i => ∑ j, P i j
def mF (P : Fin n → Fin n → ℝ) : Fin n → ℝ := fun j => ∑ i, P i j

/-- Jeffrey pooling on `E` (Definition 3) on the grid. -/
noncomputable def jE (g : PoolOp n) (P : Fin n → Fin n → ℝ) (Q : Fin n → ℝ) :
    Fin n → Fin n → ℝ := fun i j => P i j / mE P i * g (mE P) Q i

/-- Jeffrey pooling on `F` on the grid. -/
noncomputable def jF (g : PoolOp n) (P : Fin n → Fin n → ℝ) (R : Fin n → ℝ) :
    Fin n → Fin n → ℝ := fun i j => P i j / mF P j * g (mF P) R j

/-- Theorem 8's hypothesis, restricted to two `n`-cell partitions forming a grid and to
regular `P`, `Q`, `R` (a weaker hypothesis than P-W's "any finite partitions and any
compatible `P`, `Q`, `R`"). -/
def CommutesOnGrid (g : PoolOp n) : Prop :=
  ∀ P Q R, Reg P → PosProb Q → PosProb R → jF g (jE g P Q) R = jE g (jF g P R) Q

theorem n_pos_of {v : Fin n → ℝ} (h : ∑ i, v i = 1) : 0 < n := by
  rcases Nat.eq_zero_or_pos n with rfl | h'
  · simp at h
  · exact h'

theorem unif_posProb (hn : 0 < n) : PosProb (unif n) := by
  refine ⟨fun _ => by unfold unif; positivity, ?_⟩
  simp [unif]; field_simp

theorem reg_mE {P : Fin n → Fin n → ℝ} (hP : Reg P) : PosProb (mE P) := by
  have hn := n_pos_of (v := mE P) hP.2
  haveI : NeZero n := ⟨hn.ne'⟩
  exact ⟨fun i => sum_pos (fun j _ => hP.1 i j) univ_nonempty, hP.2⟩

theorem reg_mF {P : Fin n → Fin n → ℝ} (hP : Reg P) : PosProb (mF P) := by
  have hn := n_pos_of (v := mE P) hP.2
  haveI : NeZero n := ⟨hn.ne'⟩
  exact ⟨fun j => sum_pos (fun i _ => hP.1 i j) univ_nonempty, by
    unfold mF; rw [sum_comm]; exact hP.2⟩

theorem mE_jE {g : PoolOp n} {P : Fin n → Fin n → ℝ} (hP : Reg P) (Q : Fin n → ℝ) :
    mE (jE g P Q) = g (mE P) Q := by
  funext i
  show ∑ j, P i j / mE P i * g (mE P) Q i = g (mE P) Q i
  rw [← sum_mul, ← sum_div]
  show mE P i / mE P i * _ = _
  rw [div_self ((reg_mE hP).1 i).ne', one_mul]

theorem mF_jF {g : PoolOp n} {P : Fin n → Fin n → ℝ} (hP : Reg P) (R : Fin n → ℝ) :
    mF (jF g P R) = g (mF P) R := by
  funext j
  show ∑ i, P i j / mF P j * g (mF P) R j = g (mF P) R j
  rw [← sum_mul, ← sum_div]
  show mF P j / mF P j * _ = _
  rw [div_self ((reg_mF hP).1 j).ne', one_mul]

theorem jE_reg {g : PoolOp n} (hg : RegularityPreserving g) {P : Fin n → Fin n → ℝ} (hP : Reg P)
    {Q : Fin n → ℝ} (hQ : PosProb Q) : Reg (jE g P Q) := by
  have hpool := hg _ _ (reg_mE hP) hQ
  refine ⟨fun i j => ?_, ?_⟩
  · unfold jE; exact mul_pos (div_pos (hP.1 i j) ((reg_mE hP).1 i)) (hpool.1 i)
  · have := congrArg (fun v : Fin n → ℝ => ∑ i, v i) (mE_jE (g := g) hP Q)
    simp only [mE] at this; rw [this]; exact hpool.2

theorem jF_reg {g : PoolOp n} (hg : RegularityPreserving g) {P : Fin n → Fin n → ℝ} (hP : Reg P)
    {R : Fin n → ℝ} (hR : PosProb R) : Reg (jF g P R) := by
  have hpool := hg _ _ (reg_mF hP) hR
  refine ⟨fun i j => ?_, ?_⟩
  · unfold jF; exact mul_pos (div_pos (hP.1 i j) ((reg_mF hP).1 j)) (hpool.1 j)
  · have := congrArg (fun v : Fin n → ℝ => ∑ j, v j) (mF_jF (g := g) hP R)
    simp only [mF] at this; rw [sum_comm, this]; exact hpool.2

/-- **Theorem 6** (Wagner's necessity theorem as P-W state it), `E`-half, on the grid, in
cross-multiplied form: if the two Jeffrey-pooling routes agree at a regular `P`, the
`E`-Bayes factors of the first and of the last step agree:
`[⟪PQ⟫_E(E_{i₁})/P(E_{i₁})] / [⟪PQ⟫_E(E_{i₂})/P(E_{i₂})]` equals the same ratio for the
revision of `⟪PR⟫_F` by `Q` (P-W Equation (6)). -/
theorem wagner_grid {g : PoolOp n} (hg : RegularityPreserving g) {P : Fin n → Fin n → ℝ}
    (hP : Reg P) {Q R : Fin n → ℝ} (hR : PosProb R)
    (hc : jF g (jE g P Q) R = jE g (jF g P R) Q) (i₁ i₂ : Fin n) :
    g (mE P) Q i₁ / mE P i₁ * (g (mE (jF g P R)) Q i₂ / mE (jF g P R) i₂) =
      g (mE P) Q i₂ / mE P i₂ * (g (mE (jF g P R)) Q i₁ / mE (jF g P R) i₁) := by
  have hn := n_pos_of (v := mE P) hP.2
  obtain j : Fin n := ⟨0, hn⟩
  set X := jE g P Q
  set X' := jF g P R
  set α := fun i => g (mE P) Q i / mE P i
  set β := fun j => g (mF X) R j / mF X j
  set γ := fun j => g (mF P) R j / mF P j
  set δ := fun i => g (mE X') Q i / mE X' i
  have hγ : γ j ≠ 0 := (div_pos ((hg _ _ (reg_mF hP) hR).1 j) ((reg_mF hP).1 j)).ne'
  have key : ∀ i, α i * β j = γ j * δ i := by
    intro i
    have h := congrFun (congrFun hc i) j
    have e1 : jF g X R i j = P i j * (α i * β j) := by
      simp only [jF, X, jE, α, β]; ring
    have e2 : jE g X' Q i j = P i j * (γ j * δ i) := by
      simp only [jE, X', jF, γ, δ]; ring
    rw [e1, e2] at h
    exact mul_left_cancel₀ (hP.1 i j).ne' h
  show α i₁ * δ i₂ = α i₂ * δ i₁
  apply mul_left_cancel₀ hγ
  linear_combination (-α i₁) * key i₂ + α i₂ * key i₁

/-- The prior of Lemma 7: `P(E_iF_j) = 1/n - (n-1)ε/n` on the diagonal and `ε/n` off it,
written `(ε + [i = j](1 - nε))/n`. -/
noncomputable def Peps (n : ℕ) (ε : ℝ) : Fin n → Fin n → ℝ :=
  fun i j => (ε + if i = j then 1 - n * ε else 0) / n

theorem sum_Peps_row (n : ℕ) (ε : ℝ) (i : Fin n) (x : Fin n → ℝ) :
    ∑ j, (ε + if i = j then 1 - n * ε else 0) * x j = ε * ∑ j, x j + (1 - n * ε) * x i := by
  simp_rw [add_mul, sum_add_distrib, ← mul_sum, ite_mul, zero_mul, sum_ite_eq]; simp

theorem Peps_facts (hn : 0 < n) {ε : ℝ} (hε : 0 < ε) (hnε : n * ε < 1) :
    Reg (Peps n ε) ∧ mE (Peps n ε) = unif n ∧ mF (Peps n ε) = unif n := by
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hrow : ∀ i, mE (Peps n ε) i = 1 / n := by
    intro i; unfold mE Peps; rw [← sum_div]
    have := sum_Peps_row n ε i (fun _ => 1); simp only [mul_one, sum_const, card_univ,
      Fintype.card_fin, nsmul_eq_mul] at this
    rw [this]; congr 1; ring
  have hcol : ∀ j, mF (Peps n ε) j = 1 / n := by
    intro j; unfold mF Peps; rw [← sum_div]
    have : ∀ i : Fin n, (if i = j then 1 - n * ε else 0) = if j = i then 1 - n * ε else 0 := by
      intro i; simp only [eq_comm]
    simp_rw [this]
    have h := sum_Peps_row n ε j (fun _ => 1); simp only [mul_one, sum_const, card_univ,
      Fintype.card_fin, nsmul_eq_mul] at h
    rw [h]; congr 1; ring
  refine ⟨⟨fun i j => ?_, ?_⟩, funext hrow, funext hcol⟩
  · unfold Peps; apply div_pos _ (by exact_mod_cast hn)
    split_ifs <;> linarith
  · simp_rw [show ∀ i, ∑ j, Peps n ε i j = 1 / n from hrow]
    simp; field_simp

/-- The `E`-marginal after Jeffrey pooling `P_ε` with `R` on `F`: `ε + (1 - nε) x_i`, where
`x = ⟨unif, R⟩` (the displayed formula for `⟪PR⟫_F(E_i)` in Lemma 7). -/
theorem mE_jF_Peps {g : PoolOp n} (hn : 0 < n) {ε : ℝ} (hε : 0 < ε) (hnε : n * ε < 1)
    (R : Fin n → ℝ) (hx : ∑ i, g (unif n) R i = 1) :
    mE (jF g (Peps n ε) R) = fun i => ε + (1 - n * ε) * g (unif n) R i := by
  obtain ⟨-, -, hF⟩ := Peps_facts hn hε hnε
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  funext i; unfold mE jF; rw [hF]
  have : ∀ j, Peps n ε i j / unif n j * g (unif n) R j =
      (ε + if i = j then 1 - n * ε else 0) * g (unif n) R j := by
    intro j; unfold Peps unif; field_simp
  simp_rw [this]; rw [sum_Peps_row, hx, mul_one]

theorem eps_seq (hn : 0 < n) :
    (∀ m : ℕ, 0 < 1 / ((n : ℝ) * (m + 2))) ∧ (∀ m : ℕ, (n : ℝ) * (1 / (n * (m + 2))) < 1) ∧
      Tendsto (fun m : ℕ => 1 / ((n : ℝ) * (m + 2))) atTop (𝓝 0) := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  refine ⟨fun m => by positivity, fun m => ?_, ?_⟩
  · rw [mul_one_div, div_lt_one (by positivity)]
    nlinarith [show (0 : ℝ) ≤ m from m.cast_nonneg]
  · apply tendsto_const_nhds.div_atTop
    apply Tendsto.const_mul_atTop hn'
    exact tendsto_atTop_add_const_right _ 2 tendsto_natCast_atTop_atTop

theorem tendsto_w {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) :
    Tendsto (fun m : ℕ => fun i => 1 / ((n : ℝ) * (m + 2)) +
      (1 - n * (1 / ((n : ℝ) * (m + 2)))) * x i) atTop (𝓝 x) := by
  obtain ⟨-, -, hε⟩ := eps_seq hn
  rw [tendsto_pi_nhds]; intro i
  have := hε.add (((tendsto_const_nhds (x := (1 : ℝ))).sub (hε.const_mul (n : ℝ))).mul
    (tendsto_const_nhds (x := x i)))
  simpa using this

theorem posProb_eq_of_ratio {v w : Fin n → ℝ} (hv : PosProb v) (hw : PosProb w)
    (h : ∀ i k, v i / w i = v k / w k) : v = w := by
  have hn := n_pos_of hv.2
  have hc : ∀ i, v i = (v ⟨0, hn⟩ / w ⟨0, hn⟩) * w i := by
    intro i; have := (hw.1 i).ne'
    rw [← h i ⟨0, hn⟩]; field_simp
  have hc1 : v ⟨0, hn⟩ / w ⟨0, hn⟩ = 1 := by
    have : ∑ i, v i = (v ⟨0, hn⟩ / w ⟨0, hn⟩) * ∑ i, w i := by
      rw [mul_sum]; exact sum_congr rfl fun i _ => hc i
    rw [hv.2, hw.2] at this; linarith
  funext i; rw [hc i, hc1, one_mul]

theorem exists_lt_of_ne {x R : Fin n → ℝ} (hx : ∑ i, x i = 1) (hR : ∑ i, R i = 1) (hne : x ≠ R) :
    ∃ k, x k < R k := by
  by_contra h; push Not at h
  apply hne; funext k
  have := (sum_eq_sum_iff_of_le (s := univ) (fun i _ => h i)).1 (by rw [hx, hR])
  exact (this k (mem_univ k)).symm

/-- **Lemma 7** (P-W p. 14), on regular inputs: a uniformity preserving, monotonic, symmetric,
continuous (and, by construction, extensional) pooling operator that makes Jeffrey pooling
commute treats the uniform distribution as neutral: `⟨unif, R⟩ = R`. -/
theorem lemma7 {g : PoolOp n} (hg : RegularityPreserving g) (hUP : UniformityPreserving g)
    (hMono : Monotone' g) (hSym : Symmetric g) (hCont : ContinuousOnReg g)
    (hC : CommutesOnGrid g) {R : Fin n → ℝ} (hR : PosProb R) : g (unif n) R = R := by
  have hn := n_pos_of hR.2
  have hu := unif_posProb hn
  set x := g (unif n) R
  have hx : PosProb x := hg _ _ hu hR
  -- for each admissible ε, ⟨w_ε, unif⟩ = w_ε
  have step : ∀ ε : ℝ, 0 < ε → n * ε < 1 →
      g (fun i => ε + (1 - n * ε) * x i) (unif n) = fun i => ε + (1 - n * ε) * x i := by
    intro ε hε hnε
    obtain ⟨hP, hE, -⟩ := Peps_facts hn hε hnε
    have hw := mE_jF_Peps (g := g) hn hε hnε R hx.2
    have hwreg : PosProb (mE (jF g (Peps n ε) R)) := reg_mE (jF_reg hg hP hR)
    have hW := wagner_grid hg hP hR (hC _ _ _ hP hu hR)
    rw [hE, hw] at hW; rw [hw] at hwreg
    simp only at hW
    apply posProb_eq_of_ratio (hg _ _ hwreg hu) hwreg
    intro i k
    have h1 := hW i k
    have hA : g (unif n) (unif n) i / unif n i = g (unif n) (unif n) k / unif n k := by
      rw [hUP i k]; rfl
    have hne : g (unif n) (unif n) k / unif n k ≠ 0 :=
      (div_pos ((hg _ _ hu hu).1 k) (hu.1 k)).ne'
    rw [hA] at h1
    exact (mul_left_cancel₀ hne h1).symm
  obtain ⟨hεpos, hεn, -⟩ := eps_seq hn
  set w : ℕ → Fin n → ℝ := fun m i => 1 / ((n : ℝ) * (m + 2)) +
    (1 - n * (1 / ((n : ℝ) * (m + 2)))) * x i
  have hwpos : ∀ m, PosProb (w m) := by
    intro m
    refine ⟨fun i => ?_, ?_⟩
    · have := hεn m; have := hεpos m; have := hx.1 i
      simp only [w]; nlinarith
    · simp only [w]; rw [sum_add_distrib, ← mul_sum, hx.2]; simp
  have hlim := hCont w x (unif n) hwpos hx hu (tendsto_w hn x)
  have heq : ∀ m, g (w m) (unif n) = w m := fun m => step _ (hεpos m) (hεn m)
  simp_rw [heq] at hlim
  have hfix : g x (unif n) = x := tendsto_nhds_unique hlim (tendsto_w hn x)
  rw [hSym _ _ hx hu] at hfix
  -- monotonicity forces x = R
  by_contra hne
  obtain ⟨k, hk⟩ := exists_lt_of_ne hx.2 hR.2 hne
  have := hMono x R hx hR k hk
  rw [hfix] at this
  exact lt_irrefl _ this

theorem prop_to_upco {v c : Fin n → ℝ} (hv : ∑ i, v i = 1) (hc : ∀ i, 0 < c i)
    (h : ∀ i k, v k * c i = v i * c k) : ∀ i, v i = c i / ∑ k, c k := by
  have hn := n_pos_of hv
  haveI : NeZero n := ⟨hn.ne'⟩
  have hS : 0 < ∑ k, c k := sum_pos (fun k _ => hc k) univ_nonempty
  intro i
  rw [eq_div_iff hS.ne', mul_sum]
  calc ∑ k, v i * c k = ∑ k, v k * c i := sum_congr rfl fun k _ => (h i k).symm
    _ = c i := by rw [← sum_mul, hv, one_mul]

/-- **Theorem 2 / Theorem 8, regular part** (P-W pp. 6, 15): a pooling operator that is
uniformity preserving, monotonic, symmetric, continuous and extensional, and that makes
Jeffrey pooling commute, agrees with upco on regular inputs.  Proved for `n`-cell partitions
(P-W's main-text Theorem 2 is `n = 2`), under the added `RegularityPreserving` hypothesis.
The last step of P-W's proof (extending from regular to all inputs by continuity) is not
formalized. -/
theorem thm8_regular {g : PoolOp n} (hg : RegularityPreserving g) (hUP : UniformityPreserving g)
    (hMono : Monotone' g) (hSym : Symmetric g) (hCont : ContinuousOnReg g)
    (hC : CommutesOnGrid g) {Q R : Fin n → ℝ} (hQ : PosProb Q) (hR : PosProb R) :
    g R Q = upcoV R Q := by
  have hn := n_pos_of hR.2
  have hu := unif_posProb hn
  have hL7 : ∀ {S : Fin n → ℝ}, PosProb S → g (unif n) S = S :=
    fun hS => lemma7 hg hUP hMono hSym hCont hC hS
  -- for each admissible ε, Equation (8) in cross-multiplied form
  have step : ∀ ε : ℝ, 0 < ε → n * ε < 1 → ∀ i₁ i₂,
      Q i₁ * g (fun i => ε + (1 - n * ε) * R i) Q i₂ * (ε + (1 - n * ε) * R i₁) =
        Q i₂ * g (fun i => ε + (1 - n * ε) * R i) Q i₁ * (ε + (1 - n * ε) * R i₂) := by
    intro ε hε hnε i₁ i₂
    obtain ⟨hP, hE, -⟩ := Peps_facts hn hε hnε
    have hw := mE_jF_Peps (g := g) hn hε hnε R (by rw [hL7 hR]; exact hR.2)
    rw [hL7 hR] at hw
    have hwreg : PosProb (mE (jF g (Peps n ε) R)) := reg_mE (jF_reg hg hP hR)
    have hW := wagner_grid hg hP hR (hC _ _ _ hP hQ hR) i₁ i₂
    rw [hE, hw, hL7 hQ] at hW; rw [hw] at hwreg
    have h1 := (hwreg.1 i₁).ne'; have h2 := (hwreg.1 i₂).ne'
    have hu1 : unif n i₁ ≠ 0 := (hu.1 i₁).ne'; have hu2 : unif n i₂ ≠ 0 := (hu.1 i₂).ne'
    simp only [unif] at hW hu1 hu2
    field_simp at hW
    linear_combination hW
  obtain ⟨hεpos, hεn, -⟩ := eps_seq hn
  set w : ℕ → Fin n → ℝ := fun m i => 1 / ((n : ℝ) * (m + 2)) +
    (1 - n * (1 / ((n : ℝ) * (m + 2)))) * R i
  have hwpos : ∀ m, PosProb (w m) := by
    intro m
    refine ⟨fun i => ?_, ?_⟩
    · have := hεn m; have := hεpos m; have := hR.1 i
      simp only [w]; nlinarith
    · simp only [w]; rw [sum_add_distrib, ← mul_sum, hR.2]; simp
  have hlim := hCont w R Q hwpos hR hQ (tendsto_w hn R)
  have hwlim := tendsto_w hn R
  have hcross : ∀ i₁ i₂, Q i₁ * g R Q i₂ * R i₁ = Q i₂ * g R Q i₁ * R i₂ := by
    intro i₁ i₂
    have hL : Tendsto (fun m => Q i₁ * g (w m) Q i₂ * w m i₁) atTop
        (𝓝 (Q i₁ * g R Q i₂ * R i₁)) :=
      ((tendsto_const_nhds.mul ((continuous_apply i₂).continuousAt.tendsto.comp hlim)).mul
        ((continuous_apply i₁).continuousAt.tendsto.comp hwlim))
    have hR' : Tendsto (fun m => Q i₂ * g (w m) Q i₁ * w m i₂) atTop
        (𝓝 (Q i₂ * g R Q i₁ * R i₂)) :=
      ((tendsto_const_nhds.mul ((continuous_apply i₁).continuousAt.tendsto.comp hlim)).mul
        ((continuous_apply i₂).continuousAt.tendsto.comp hwlim))
    have heq : ∀ m, Q i₁ * g (w m) Q i₂ * w m i₁ = Q i₂ * g (w m) Q i₁ * w m i₂ :=
      fun m => step _ (hεpos m) (hεn m) i₁ i₂
    simp_rw [heq] at hL
    exact tendsto_nhds_unique hL hR'
  have hgR := hg _ _ hR hQ
  funext i
  rw [prop_to_upco hgR.2 (fun i => mul_pos (hR.1 i) (hQ.1 i)) (fun i k => by
    have := hcross i k; linarith) i]
  rfl

/-! ### Upco satisfies every hypothesis, so `thm8_regular` is not vacuous -/

theorem upco_regPres : RegularityPreserving (upcoV : PoolOp n) := by
  intro P Q hP hQ
  have hn := n_pos_of hP.2
  haveI : NeZero n := ⟨hn.ne'⟩
  have hS : 0 < ∑ j, P j * Q j := sum_pos (fun j _ => mul_pos (hP.1 j) (hQ.1 j)) univ_nonempty
  exact ⟨fun i => div_pos (mul_pos (hP.1 i) (hQ.1 i)) hS, by
    unfold upcoV; rw [← sum_div, div_self hS.ne']⟩

theorem upco_UP : UniformityPreserving (upcoV : PoolOp n) := by
  intro i k; simp [upcoV, unif]

theorem upco_unif {Q : Fin n → ℝ} (hQ : PosProb Q) : upcoV (unif n) Q = Q := by
  have hn := n_pos_of hQ.2
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  funext i; unfold upcoV unif
  rw [← mul_sum, hQ.2, mul_one]; field_simp

theorem upco_mono : Monotone' (upcoV : PoolOp n) := by
  intro Q R hQ hR i h; rwa [upco_unif hQ, upco_unif hR]

theorem upco_symm : Symmetric (upcoV : PoolOp n) := by
  intro P Q _ _; funext i; unfold upcoV; simp_rw [mul_comm (P _)]

theorem upco_cont : ContinuousOnReg (upcoV : PoolOp n) := by
  intro P Pinf Q _ hPinf hQ hlim
  have hn := n_pos_of hQ.2
  haveI : NeZero n := ⟨hn.ne'⟩
  rw [tendsto_pi_nhds] at hlim ⊢
  intro i
  have hS : 0 < ∑ j, Pinf j * Q j :=
    sum_pos (fun j _ => mul_pos (hPinf.1 j) (hQ.1 j)) univ_nonempty
  exact ((hlim i).mul_const (Q i)).div
    (tendsto_finsetSum _ fun j _ => (hlim j).mul_const (Q j)) hS.ne'

theorem jE_upco {P : Fin n → Fin n → ℝ} (hP : Reg P) (Q : Fin n → ℝ) (i j : Fin n) :
    jE upcoV P Q i j = P i j * Q i / ∑ k, mE P k * Q k := by
  unfold jE upcoV; have := ((reg_mE hP).1 i).ne'; field_simp

theorem jF_upco {P : Fin n → Fin n → ℝ} (hP : Reg P) (R : Fin n → ℝ) (i j : Fin n) :
    jF upcoV P R i j = P i j * R j / ∑ l, mF P l * R l := by
  unfold jF upcoV; have := ((reg_mF hP).1 j).ne'; field_simp

theorem upco_commutes : CommutesOnGrid (upcoV : PoolOp n) := by
  intro P Q R hP hQ hR
  have hX := jE_reg upco_regPres hP hQ
  have hY := jF_reg upco_regPres hP hR
  have hZ : (∑ k, mE P k * Q k) ≠ 0 := by
    have := (upco_regPres _ _ (reg_mE hP) hQ).1 ⟨0, n_pos_of hQ.2⟩
    unfold upcoV at this; intro h0; rw [h0, div_zero] at this; exact lt_irrefl _ this
  have hZ' : (∑ l, mF P l * R l) ≠ 0 := by
    have := (upco_regPres _ _ (reg_mF hP) hR).1 ⟨0, n_pos_of hR.2⟩
    unfold upcoV at this; intro h0; rw [h0, div_zero] at this; exact lt_irrefl _ this
  have sF : ∑ l, mF (jE upcoV P Q) l * R l =
      (∑ l, ∑ k, P k l * Q k * R l) / ∑ k, mE P k * Q k := by
    unfold mF; simp_rw [jE_upco hP, sum_mul, div_mul_eq_mul_div, ← sum_div]
  have sE : ∑ k, mE (jF upcoV P R) k * Q k =
      (∑ k, ∑ l, P k l * R l * Q k) / ∑ l, mF P l * R l := by
    unfold mE; simp_rw [jF_upco hP, sum_mul, div_mul_eq_mul_div, ← sum_div]
  funext i j
  rw [jF_upco hX, jE_upco hY, sF, sE, jE_upco hP, jF_upco hP,
    div_mul_eq_mul_div, div_mul_eq_mul_div, div_div_div_cancel_right₀ hZ,
    div_div_div_cancel_right₀ hZ', sum_comm]
  congr 1
  · ring
  · exact sum_congr rfl fun l _ => sum_congr rfl fun k _ => by ring

end Uniqueness

/-! ## The project's question, not P-W's: `P^B` as Jeffrey pooling with upco

Paper B's benchmark `P^B(ω) ∝ p(ω) ℓ^A_{E ω} ℓ^B_{F ω}` with `ℓ^A_i = x_i / p(E_i)`,
`ℓ^B_j = y_j / p(F_j)` (`x`, `y` the delivered credences). -/

/-- The matched likelihood `x_i / P(E_i)`, normalized to a probability on the cells: the
opinion that upco must pool with the prior to return the delivered credence `x`. -/
noncomputable def matchedOpinion (P : Ω → ℝ) (E : Ω → ι) (x : ι → ℝ) (i : ι) : ℝ :=
  (x i / marg P E i) / ∑ k, x k / marg P E k

/-- Paper B's `P^B`. -/
noncomputable def benchmark (P : Ω → ℝ) (E : Ω → ι) (F : Ω → κ) (x : ι → ℝ) (y : κ → ℝ) :
    Ω → ℝ :=
  fun ω => P ω * (x (E ω) / marg P E (E ω)) * (y (F ω) / marg P F (F ω)) /
    ∑ ω', P ω' * (x (E ω') / marg P E (E ω')) * (y (F ω') / marg P F (F ω'))

/-- Upco of the prior marginal with the matched opinion returns the delivered credence. -/
theorem upco_matched {P : Ω → ℝ} {E : Ω → ι} (hm : ∀ i, 0 < marg P E i) {x : ι → ℝ}
    (hx : ∀ i, 0 < x i) (hx1 : ∑ i, x i = 1) : upcoV (marg P E) (matchedOpinion P E x) = x := by
  obtain ⟨i₀⟩ : Nonempty ι := by
    by_contra h; rw [not_nonempty_iff] at h; simp at hx1
  have hS : 0 < ∑ k, x k / marg P E k :=
    sum_pos (fun k _ => div_pos (hx k) (hm k)) ⟨i₀, mem_univ _⟩
  funext i; unfold upcoV matchedOpinion
  have hc : ∀ k, marg P E k * (x k / marg P E k / ∑ k, x k / marg P E k) =
      x k / ∑ k, x k / marg P E k := by
    intro k; have := (hm k).ne'; field_simp
  simp_rw [hc]; rw [← sum_div, hx1, div_div_div_cancel_right₀ hS.ne', div_one]

/-- `P^B` is Jeffrey pooling with upco, in either order, when each cue's pooled opinion is its
matched likelihood against the **prior** marginal (the matched opinion), not its delivered
credence.  This is P-W's Theorem 1 applied to that choice of opinions. -/
theorem PB_is_upco_pooling {P : Ω → ℝ} (hP : IsProb P) (E : Ω → ι) (F : Ω → κ)
    (hreg : ∀ i j, 0 < pr P (cell E i ∩ cell F j)) {x : ι → ℝ} {y : κ → ℝ}
    (hx : ∀ i, 0 < x i) (hy : ∀ j, 0 < y j) :
    jpool upcoV (jpool upcoV P E (matchedOpinion P E x)) F (matchedOpinion P F y) =
        benchmark P E F x y ∧
      jpool upcoV (jpool upcoV P F (matchedOpinion P F y)) E (matchedOpinion P E x) =
        benchmark P E F x y := by
  obtain ⟨ω₀⟩ := nonempty_of_prob hP
  have hsub : ∀ {A B : Finset Ω}, A ⊆ B → pr P A ≤ pr P B := fun h =>
    sum_le_sum_of_subset_of_nonneg h fun ω _ _ => hP.1 ω
  have hmE : ∀ i, 0 < marg P E i := fun i =>
    lt_of_lt_of_le (hreg i (F ω₀)) (hsub inter_subset_left)
  have hmF : ∀ j, 0 < marg P F j := fun j =>
    lt_of_lt_of_le (hreg (E ω₀) j) (hsub inter_subset_right)
  have hSA : 0 < ∑ k, x k / marg P E k :=
    sum_pos (fun k _ => div_pos (hx k) (hmE k)) ⟨E ω₀, mem_univ _⟩
  have hSB : 0 < ∑ k, y k / marg P F k :=
    sum_pos (fun k _ => div_pos (hy k) (hmF k)) ⟨F ω₀, mem_univ _⟩
  have hQA : ∀ i, 0 ≤ matchedOpinion P E x i := fun i =>
    (div_pos (div_pos (hx i) (hmE i)) hSA).le
  have hQB : ∀ j, 0 ≤ matchedOpinion P F y j := fun j =>
    (div_pos (div_pos (hy j) (hmF j)) hSB).le
  have hQA1 : ∑ i, matchedOpinion P E x i = 1 := by
    unfold matchedOpinion; rw [← sum_div, div_self hSA.ne']
  have hQB1 : ∑ j, matchedOpinion P F y j = 1 := by
    unfold matchedOpinion; rw [← sum_div, div_self hSB.ne']
  have hcomm := thm1 hP E F hreg hQA hQA1 hQB hQB1
  have h1 : jpool upcoV (jpool upcoV P E (matchedOpinion P E x)) F (matchedOpinion P F y) =
      benchmark P E F x y := by
    funext ω
    rw [route hP.1 E F hQA (normalizer_pos E hmE hQA hQA1).ne']
    unfold benchmark matchedOpinion
    have e : ∀ ω', P ω' * (x (E ω') / marg P E (E ω') / ∑ k, x k / marg P E k) *
        (y (F ω') / marg P F (F ω') / ∑ k, y k / marg P F k) =
        P ω' * (x (E ω') / marg P E (E ω')) * (y (F ω') / marg P F (F ω')) /
          ((∑ k, x k / marg P E k) * ∑ k, y k / marg P F k) := by
      intro ω'; field_simp
    simp_rw [e]; rw [← sum_div, div_div_div_cancel_right₀ (mul_pos hSA hSB).ne']
  exact ⟨h1, hcomm ▸ h1⟩

/-- On P-W's own p. 4 numbers read as Paper B's two cues (prior `exP`, delivered credences
`Q(E) = .8`, `R(F) = .6`): Jeffrey pooling with the delivered credences themselves as the
pooled opinions gives `P''(EF) = 18/29`, while `P^B(EF) = 27/40`.  So `P^B` is upco-Jeffrey
pooling only with the matched opinions of `PB_is_upco_pooling`. -/
theorem PB_vs_example :
    benchmark exP Prod.fst Prod.snd exQ exR (0, 0) = 27 / 40 ∧
      jpool upcoV (jpool upcoV exP Prod.fst exQ) Prod.snd exR (0, 0) = 18 / 29 := by
  refine ⟨?_, by rw [example_P2.2.2]; simp [tbl]⟩
  simp only [benchmark, marg_fst, marg_snd, Fintype.sum_prod_type, Fin.sum_univ_two]
  simp [exP, exQ, exR, tbl]; norm_num

end Literature.PettigrewWeisberg
