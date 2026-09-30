/-
# Wagner (2002), "Probability Kinematics and Commutativity"

*Philosophy of Science* 69(2), 266-278.  The copy used is the author's preprint, paginated
1-14 with no journal page numbers; page references are to the preprint.  Every formula was
read on the rendered page: the text layer drops primes (`β_{r',q'}` comes out as `βr ,q`).

## Setting and what is formalized

Wagner works on `(Ω, A, p)` with countable families `E = {E_i}`, `F = {F_j}` of disjoint
events and positive targets (note 1).  Here `Ω` is finite, events are `Finset Ω`, and a
family is a labelling `E : Ω → ι` with cells `E_i = {ω | E ω = i}`, so the family covers `Ω`.
With `ι`, `κ` finite this is Field's finite case, proved with Wagner's argument.

* (1.1) `bf q p A B` is `β_{q,p}(A:B)`; (1.2) `pf q p A` is `π_{q,p}(A)`; (1.3) `eq13`.
* (2.1) `kin p E e`.  "`q` comes from `p` by probability kinematics on `E`" is
  `ComesByPK q p E`: some `e_i > 0` with `Σ e_i = 1`, every `p(E_i) > 0`, and
  `q = kin p E e`.  (2.1)-(2.2) for all events: `eq21`; (2.3) rigidity: `eq23`.
* Schema (3.1) (p. 4) is `p →E q →F r` and `p →F q' →E r'`, i.e. the four hypotheses
  `ComesByPK q p E`, `ComesByPK r q F`, `ComesByPK q' p F`, `ComesByPK r' q' E`.  The
  second-step targets are free: `r'(E_i)` need not equal `q(E_i)`, nor `q'(F_j)` equal `r(F_j)`.
  Schema (2.7) (p. 3) is the matched-target special case.

**Section 3.**
* `thm31`: **Theorem 3.1** (Field's theorem).  (3.2) `β_{r',q'}(E_{i₁}:E_{i₂}) = β_{q,p}(E_{i₁}:E_{i₂})`
  and (3.3) `β_{q',p}(F_{j₁}:F_{j₂}) = β_{r,q}(F_{j₁}:F_{j₂})` imply `r' = r`.
* `eq35`, `eq37`: the normal forms (3.5)-(3.8).  `remark31`: Field's own form (3.9)-(3.10)
  with the geometric mean `G_i`.
* `remark32`: (3.2) ⇔ (3.11).  `remark33`: **Jeffrey (1988)** as a corollary, (3.13)-(3.14)
  imply `r' = r`.
* `thm32_E`, `thm32_F`: **Theorem 3.2**.  `remark34`: **Diaconis-Zabell** as a corollary: in
  schema (2.7), Jeffrey independence (3.15)-(3.16) implies `r = r'`.  `remark34_indep`:
  `p`-independence entails Jeffrey independence.

**Section 4.**
* `thm41`: **Theorem 4.1** (partial converse).  Under
  (4.3) `∀ i₁ i₂ ∃ j, p(E_{i₁}F_j) p(E_{i₂}F_j) > 0` and
  (4.4) `∀ j₁ j₂ ∃ i, p(E_iF_{j₁}) p(E_iF_{j₂}) > 0`, `r' = r` implies (3.2) and (3.3).
* `remark41_qi`: **Remark 4.1**: (4.3)-(4.4) hold when `E`, `F` are *qualitatively
  independent* (every `E_iF_j ≠ ∅`) and `p` is strictly coherent.  Full support alone is not
  enough: `remark41_FeqE` shows (4.3) fails for `F = E` with two cells, whatever `p`.
* `sec4_FeqE`, `sec4_example`: the example opening Section 4: with `F = E`, `r' = r` while
  (3.2) fails (`β_{r',q'} = 1`, `β_{q,p} = 1/3`).
* `remark42`, `remark43` (Diaconis-Zabell's necessity as a corollary of Theorem 4.1).

**Section 5.**
* `bf_cond`: (1.1) remark, a Bayes factor of conditioning is a likelihood ratio.
  `note8`: **Remark 5.1 / note 8**, Garber's repeated glances: "all Bayes factors beyond the
  first are equal to one".  Remark 5.1 is Wagner's explicit answer to Garber (1980).
* `remark52`, `remark52_bf`: **Remark 5.2** (`F = E` in schema (2.7)).
* `eq55`: **Remark 5.3, (5.5)**.  `eq55_exists`: for finite partitions (5.3) always holds,
  so the recipe always yields `r'`.  `note7`: **note 7**, a Bayes factor vector can be
  realized from any prior.

Not formalized: countable partitions (Wagner's actual generality, and note 11's divergent
example, which `sympy/check_theorems.py` checks term by term); Remark 3.5 (sequences of more
than two revisions, left by Wagner "as an exercise"); Principles I and II of Section 5 as prose.

## The project's question, not Wagner's: what the theorems license about `P^B`

Paper B's benchmark is `P^B(ω) ∝ p(ω) (x_i/p(E_i)) (y_j/p(F_j))` for delivered credences
`x` on `E` and `y` on `F` (`benchmark`).
* `PB_endpoint`: `P^B` is the common endpoint `r = r'` of schema (3.1) when each cue's
  *first-position* revision is its delivered credence against the prior (`q = kin p E x`,
  `q' = kin p F y`) and the second-position revisions carry the same Bayes factors.
  `PB_endpoint_exists`: such second steps exist.
* `PB_unique`: with that anchoring, and (4.3)-(4.4), `r' = r` forces `r = P^B`
  (Theorem 4.1).  `grid_43_44`: on Paper B's grid with a full-support prior, (4.3)-(4.4) hold.
* `completion`, `wagner_does_not_single_out_PB`: **the theorems do not pick `P^B` out by
  themselves.**  Every route `p →F q' →E r'` has a partner route satisfying (3.2)-(3.3) that
  ends at `r'`.  In particular the B-first Jeffrey sequence (`36/55` at cell `(0,0)` in the
  example) is the endpoint of a Wagner-consistent commuting schema, and it differs from
  `P^B` (`27/40`).  What selects `P^B` is the modelling choice that each cue's Bayes factor
  is the one it has against the prior.
-/
import Mathlib

namespace Literature.Wagner2002

set_option linter.unusedSectionVars false

open Finset

variable {Ω ι κ : Type*} [Fintype Ω] [DecidableEq Ω] [Fintype ι] [Fintype κ] [DecidableEq ι] [DecidableEq κ]

/-- A probability on the finite space `Ω`. -/
def IsProb (p : Ω → ℝ) : Prop := (∀ ω, 0 ≤ p ω) ∧ ∑ ω, p ω = 1

/-- The probability of an event. -/
noncomputable def pr (p : Ω → ℝ) (A : Finset Ω) : ℝ := ∑ ω ∈ A, p ω

/-- The cell `E_i` of the family `E`. -/
def cell (E : Ω → ι) (i : ι) : Finset Ω := univ.filter (fun ω => E ω = i)

/-- (1.1) the Bayes factor `β_{q,p}(A : B) = [q(A)/q(B)] / [p(A)/p(B)]`. -/
noncomputable def bf (q p : Ω → ℝ) (A B : Finset Ω) : ℝ := (pr q A / pr q B) / (pr p A / pr p B)

/-- (1.2) the probability factor `π_{q,p}(A) = q(A)/p(A)`. -/
noncomputable def pf (q p : Ω → ℝ) (A : Finset Ω) : ℝ := pr q A / pr p A

/-- (1.3) `β_{q,p}(A : B) = π_{q,p}(A) / π_{q,p}(B)`. -/
theorem eq13 (q p : Ω → ℝ) (A B : Finset Ω) : bf q p A B = pf q p A / pf q p B := by
  unfold bf pf; rw [div_div_div_comm]

/-- (2.1) `q(A) = Σ_i e_i p(A|E_i)`, written at atoms. -/
noncomputable def kin (p : Ω → ℝ) (E : Ω → ι) (e : ι → ℝ) : Ω → ℝ :=
  fun ω => e (E ω) * p ω / pr p (cell E (E ω))

/-- `q` comes from `p` by probability kinematics on `E` (p. 2): positive targets `e_i`
summing to one, every `p(E_i) > 0`, and (2.1). -/
def ComesByPK (q p : Ω → ℝ) (E : Ω → ι) : Prop :=
  ∃ e : ι → ℝ, (∀ i, 0 < e i) ∧ ∑ i, e i = 1 ∧ (∀ i, 0 < pr p (cell E i)) ∧ q = kin p E e

@[simp] theorem mem_cell {E : Ω → ι} {i : ι} {ω : Ω} : ω ∈ cell E i ↔ E ω = i := by
  simp [cell]

theorem pr_eq_mul_of_forall {q p : Ω → ℝ} {A : Finset Ω} {c : ℝ}
    (h : ∀ ω ∈ A, q ω = c * p ω) : pr q A = c * pr p A := by
  unfold pr; rw [mul_sum]; exact sum_congr rfl h

theorem pr_cell_of_mul {q p : Ω → ℝ} {E : Ω → ι} {a : ι → ℝ}
    (h : ∀ ω, q ω = a (E ω) * p ω) (i : ι) : pr q (cell E i) = a i * pr p (cell E i) :=
  pr_eq_mul_of_forall fun ω hω => by rw [h, mem_cell.1 hω]

theorem sum_pr_cell (p : Ω → ℝ) (E : Ω → ι) : ∑ i, pr p (cell E i) = ∑ ω, p ω := by
  unfold pr cell; exact sum_fiberwise univ E p

theorem pr_split (p : Ω → ℝ) (A : Finset Ω) (F : Ω → κ) :
    pr p A = ∑ j, pr p (A ∩ cell F j) := by
  have h : ∀ j, A ∩ cell F j = A.filter (fun ω => F ω = j) := by
    intro j; ext ω; simp
  simp_rw [h]; unfold pr; exact (sum_fiberwise A F p).symm

theorem pr_pos_of_mul {q p : Ω → ℝ} {A : Finset Ω} (hp : ∀ ω, 0 ≤ p ω) {c : Ω → ℝ}
    (hc : ∀ ω, 0 < c ω) (h : ∀ ω, q ω = c ω * p ω) (hA : 0 < pr p A) : 0 < pr q A := by
  obtain ⟨ω, hω, hpos⟩ : ∃ ω ∈ A, 0 < p ω := by
    by_contra hcon; push Not at hcon
    exact absurd hA (not_lt.2 (sum_nonpos hcon))
  unfold pr
  refine sum_pos' (fun ω _ => by rw [h]; exact mul_nonneg (hc ω).le (hp ω)) ⟨ω, hω, ?_⟩
  rw [h]; exact mul_pos (hc ω) hpos

theorem kin_eq (p : Ω → ℝ) (E : Ω → ι) (e : ι → ℝ) :
    ∀ ω, kin p E e ω = (e (E ω) / pr p (cell E (E ω))) * p ω := by
  intro ω; unfold kin; ring

theorem pr_kin_cell {p : Ω → ℝ} {E : Ω → ι} {e : ι → ℝ} {i : ι} (hpos : 0 < pr p (cell E i)) :
    pr (kin p E e) (cell E i) = e i := by
  rw [pr_cell_of_mul (a := fun i => e i / pr p (cell E i)) (kin_eq p E e)]; field_simp

namespace ComesByPK

variable {q p : Ω → ℝ} {E : Ω → ι}

theorem p_pos (h : ComesByPK q p E) (i : ι) : 0 < pr p (cell E i) := by
  obtain ⟨e, -, -, hpE, -⟩ := h; exact hpE i

theorem cell_eq (h : ComesByPK q p E) :
    ∃ e : ι → ℝ, (∀ i, 0 < e i) ∧ ∑ i, e i = 1 ∧ ∀ i, pr q (cell E i) = e i := by
  obtain ⟨e, he, hs, hpE, rfl⟩ := h
  exact ⟨e, he, hs, fun i => pr_kin_cell (hpE i)⟩

theorem pos (h : ComesByPK q p E) (i : ι) : 0 < pr q (cell E i) := by
  obtain ⟨e, he, -, hc⟩ := h.cell_eq; rw [hc]; exact he i

theorem sum_cell (h : ComesByPK q p E) : ∑ i, pr q (cell E i) = 1 := by
  obtain ⟨e, -, hs, hc⟩ := h.cell_eq; simp_rw [hc]; exact hs

theorem pf_pos (h : ComesByPK q p E) (i : ι) : 0 < pf q p (cell E i) :=
  div_pos (h.pos i) (h.p_pos i)

/-- `q = π_{q,p}(E_{E ω}) · p` pointwise: the atom form of (2.1)-(2.3). -/
theorem factor (h : ComesByPK q p E) : ∀ ω, q ω = pf q p (cell E (E ω)) * p ω := by
  obtain ⟨e, he, hs, hpE, rfl⟩ := h
  intro ω; rw [kin_eq]; unfold pf; rw [pr_kin_cell (hpE _)]

theorem nonempty (h : ComesByPK q p E) : Nonempty ι := by
  by_contra hι; rw [not_nonempty_iff] at hι
  have := h.sum_cell; simp at this

theorem prob (hp : IsProb p) (h : ComesByPK q p E) : IsProb q := by
  refine ⟨fun ω => ?_, ?_⟩
  · rw [h.factor]; exact mul_nonneg (h.pf_pos _).le (hp.1 ω)
  · rw [← sum_pr_cell q E]; exact h.sum_cell

end ComesByPK

/-- **(2.1)-(2.2)** for every event: `q(A) = Σ_i q(E_i) p(A|E_i)`. -/
theorem eq21 {q p : Ω → ℝ} {E : Ω → ι} (hq : ComesByPK q p E) (A : Finset Ω) :
    pr q A = ∑ i, pr q (cell E i) * (pr p (A ∩ cell E i) / pr p (cell E i)) := by
  rw [pr_split q A E]
  refine sum_congr rfl fun i _ => ?_
  rw [pr_eq_mul_of_forall (c := pf q p (cell E i)) fun ω hω => by
    rw [hq.factor ω, mem_cell.1 (mem_inter.1 hω).2]]
  unfold pf; ring

/-- **(2.3)**, the rigidity condition: `q(A|E_i) = p(A|E_i)`. -/
theorem eq23 {q p : Ω → ℝ} {E : Ω → ι} (hq : ComesByPK q p E) (A : Finset Ω) (i : ι) :
    pr q (A ∩ cell E i) / pr q (cell E i) = pr p (A ∩ cell E i) / pr p (cell E i) := by
  rw [pr_eq_mul_of_forall (c := pf q p (cell E i)) fun ω hω => by
    rw [hq.factor ω, mem_cell.1 (mem_inter.1 hω).2],
    pr_cell_of_mul (a := fun i => pf q p (cell E i)) hq.factor i,
    mul_div_mul_left _ _ (hq.pf_pos i).ne']

/-! ## Vector lemmas -/

theorem ratio_iff_prop {a a' : ι → ℝ} (ha : ∀ i, 0 < a i) (ha' : ∀ i, 0 < a' i) :
    (∀ i₁ i₂, a' i₁ / a' i₂ = a i₁ / a i₂) ↔ ∃ c, 0 < c ∧ ∀ i, a' i = c * a i := by
  constructor
  · intro h
    rcases isEmpty_or_nonempty ι with hι | ⟨⟨i₀⟩⟩
    · exact ⟨1, one_pos, fun i => isEmptyElim i⟩
    refine ⟨a' i₀ / a i₀, div_pos (ha' i₀) (ha i₀), fun i => ?_⟩
    have := h i i₀
    have h1 := (ha i₀).ne'; have h2 := (ha' i₀).ne'
    field_simp at this ⊢; linarith
  · rintro ⟨c, hc, h⟩ i₁ i₂
    rw [h, h, mul_div_mul_left _ _ hc.ne']

theorem eq_of_prop {x y : ι → ℝ} {c : ℝ} (hx : ∑ i, x i = 1) (hy : ∑ i, y i = 1)
    (h : ∀ i, x i = c * y i) : x = y := by
  have hc : c = 1 := by
    have : ∑ i, x i = c * ∑ i, y i := by rw [mul_sum]; exact sum_congr rfl fun i _ => h i
    rw [hx, hy] at this; linarith
  funext i; rw [h, hc, one_mul]

theorem eq_normalize {x w : ι → ℝ} {c : ℝ} (hx : ∑ i, x i = 1) (h : ∀ i, x i = c * w i) :
    ∀ i, x i = w i / ∑ k, w k := by
  have hs : c * ∑ k, w k = 1 := by
    rw [← hx, mul_sum]; exact sum_congr rfl fun i _ => (h i).symm
  have hne : ∑ k, w k ≠ 0 := by rintro h0; rw [h0, mul_zero] at hs; exact zero_ne_one hs
  intro i; rw [h, eq_div_iff hne, mul_right_comm, hs, one_mul]

/-- Two positive vectors summing to one with the same ratios are equal. -/
theorem eq_of_ratio {x y : ι → ℝ} (hx : ∀ i, 0 < x i) (hy : ∀ i, 0 < y i)
    (hsx : ∑ i, x i = 1) (hsy : ∑ i, y i = 1) (h : ∀ i₁ i₂, x i₁ / x i₂ = y i₁ / y i₂) :
    x = y := by
  obtain ⟨c, -, hc⟩ := (ratio_iff_prop hy hx).1 h
  exact eq_of_prop hsx hsy hc

/-- The shape shared by the two halves of Theorem 3.2: with a common numerator `t`,
the ratio identity, the pointwise identity and `x = y` are equivalent. -/
theorem common_numerator {t x y : ι → ℝ} (ht : ∀ i, 0 < t i) (hx : ∀ i, 0 < x i)
    (hy : ∀ i, 0 < y i) (hsx : ∑ i, x i = 1) (hsy : ∑ i, y i = 1) :
    ((∀ i₁ i₂, (t i₁ / x i₁) / (t i₂ / x i₂) = (t i₁ / y i₁) / (t i₂ / y i₂)) ↔
        ∀ i, t i / x i = t i / y i) ∧
      ((∀ i, t i / x i = t i / y i) ↔ ∀ i, x i = y i) := by
  have hB : (∀ i, t i / x i = t i / y i) ↔ ∀ i, x i = y i := by
    constructor
    · intro h i; have hi := h i
      have := (ht i).ne'; have := (hx i).ne'; have := (hy i).ne'
      field_simp at hi; nlinarith [ht i]
    · intro h i; rw [h i]
  refine ⟨⟨fun h => ?_, fun h i₁ i₂ => by rw [h i₁, h i₂]⟩, hB⟩
  obtain ⟨c, hc, hc'⟩ := (ratio_iff_prop (fun i => div_pos (ht i) (hy i))
    (fun i => div_pos (ht i) (hx i))).1 h
  -- t/x = c t/y  gives  y = c x
  have hyx : ∀ i, y i = c * x i := by
    intro i; have hi := hc' i
    have := (ht i).ne'; have := (hx i).ne'; have := (hy i).ne'
    field_simp at hi; nlinarith [ht i]
  have := eq_of_prop hsy hsx hyx
  exact hB.2 fun i => by rw [this]

/-! ## Schema (3.1) and Theorem 3.1 -/

variable {p q r q' r' : Ω → ℝ} {E : Ω → ι} {F : Ω → κ}

/-- A route of schema (3.1) in product form: `r(ω) = π_{q,p}(E_i) π_{r,q}(F_j) p(ω)`. -/
theorem two_step (hq : ComesByPK q p E) (hr : ComesByPK r q F) :
    ∀ ω, r ω = pf q p (cell E (E ω)) * pf r q (cell F (F ω)) * p ω := by
  intro ω; rw [hr.factor, hq.factor]; ring

theorem two_step' (hq' : ComesByPK q' p F) (hr' : ComesByPK r' q' E) :
    ∀ ω, r' ω = pf r' q' (cell E (E ω)) * pf q' p (cell F (F ω)) * p ω := by
  intro ω; rw [hr'.factor, hq'.factor]; ring

/-- **Theorem 3.1** (Field's theorem, finite partitions; Wagner 2002 p. 4).  In schema (3.1),
the Bayes factor identities (3.2) `β_{r',q'}(E_{i₁}:E_{i₂}) = β_{q,p}(E_{i₁}:E_{i₂})` and (3.3)
`β_{q',p}(F_{j₁}:F_{j₂}) = β_{r,q}(F_{j₁}:F_{j₂})` imply `r' = r`.  The second-step targets
need not match the first route's marginals. -/
theorem thm31 (hp : IsProb p) (hq : ComesByPK q p E) (hr : ComesByPK r q F)
    (hq' : ComesByPK q' p F) (hr' : ComesByPK r' q' E)
    (h32 : ∀ i₁ i₂, bf r' q' (cell E i₁) (cell E i₂) = bf q p (cell E i₁) (cell E i₂))
    (h33 : ∀ j₁ j₂, bf q' p (cell F j₁) (cell F j₂) = bf r q (cell F j₁) (cell F j₂)) :
    r' = r := by
  simp_rw [eq13] at h32 h33
  obtain ⟨c, -, hc⟩ := (ratio_iff_prop hq.pf_pos hr'.pf_pos).1 h32
  obtain ⟨d, -, hd⟩ := (ratio_iff_prop hr.pf_pos hq'.pf_pos).1 h33
  have hr1 := (hr.prob (hq.prob hp)).2
  have hr'1 := (hr'.prob (hq'.prob hp)).2
  refine eq_of_prop (c := c * d) hr'1 hr1 fun ω => ?_
  rw [two_step' hq' hr', two_step hq hr, hc, hd]; ring

/-- **Remark 3.2**, (3.2) ⇔ (3.11): the Bayes factor identity is the probability factor
proportionality `π_{r',q'}(E_i) ∝ π_{q,p}(E_i)` (note 4 gives the constant). -/
theorem remark32 (hq : ComesByPK q p E) (hr' : ComesByPK r' q' E) :
    (∀ i₁ i₂, bf r' q' (cell E i₁) (cell E i₂) = bf q p (cell E i₁) (cell E i₂)) ↔
      ∃ c, 0 < c ∧ ∀ i, pf r' q' (cell E i) = c * pf q p (cell E i) := by
  simp_rw [eq13]; exact ratio_iff_prop hq.pf_pos hr'.pf_pos

/-- **Remark 3.3** (Jeffrey 1988 as a corollary of Theorem 3.1): the probability factor
identities (3.13) and (3.14) imply `r' = r`. -/
theorem remark33 (hp : IsProb p) (hq : ComesByPK q p E) (hr : ComesByPK r q F)
    (hq' : ComesByPK q' p F) (hr' : ComesByPK r' q' E)
    (h313 : ∀ i, pf r' q' (cell E i) = pf q p (cell E i))
    (h314 : ∀ j, pf q' p (cell F j) = pf r q (cell F j)) : r' = r :=
  thm31 hp hq hr hq' hr' (fun i₁ i₂ => by rw [eq13, eq13, h313, h313])
    (fun j₁ j₂ => by rw [eq13, eq13, h314, h314])

theorem sum_cell_of_prob (hp : IsProb p) (E : Ω → ι) : ∑ i, pr p (cell E i) = 1 := by
  rw [sum_pr_cell]; exact hp.2

/-- **Theorem 3.2** (Wagner 2002 p. 6), first half: if `r'(E_i) = q(E_i)` for all `i`, then
(3.2) ⇔ (3.13) and (3.13) ⇔ (3.15) `q'(E_i) = p(E_i)`. -/
theorem thm32_E (hp : IsProb p) (hq : ComesByPK q p E) (hq' : ComesByPK q' p F)
    (hr' : ComesByPK r' q' E) (hm : ∀ i, pr r' (cell E i) = pr q (cell E i)) :
    ((∀ i₁ i₂, bf r' q' (cell E i₁) (cell E i₂) = bf q p (cell E i₁) (cell E i₂)) ↔
        ∀ i, pf r' q' (cell E i) = pf q p (cell E i)) ∧
      ((∀ i, pf r' q' (cell E i) = pf q p (cell E i)) ↔
        ∀ i, pr q' (cell E i) = pr p (cell E i)) := by
  have key := common_numerator (t := fun i => pr q (cell E i)) hq.pos hr'.p_pos hq.p_pos
    (sum_cell_of_prob (hq'.prob hp) E) (sum_cell_of_prob hp E)
  simp only [eq13, pf, hm]; exact key

/-- **Theorem 3.2**, second half: if `q'(F_j) = r(F_j)` for all `j`, then
(3.3) ⇔ (3.14) and (3.14) ⇔ (3.16) `q(F_j) = p(F_j)`. -/
theorem thm32_F (hp : IsProb p) (hq : ComesByPK q p E) (hr : ComesByPK r q F)
    (hq' : ComesByPK q' p F) (hm : ∀ j, pr q' (cell F j) = pr r (cell F j)) :
    ((∀ j₁ j₂, bf q' p (cell F j₁) (cell F j₂) = bf r q (cell F j₁) (cell F j₂)) ↔
        ∀ j, pf q' p (cell F j) = pf r q (cell F j)) ∧
      ((∀ j, pf q' p (cell F j) = pf r q (cell F j)) ↔
        ∀ j, pr p (cell F j) = pr q (cell F j)) := by
  have key := common_numerator (t := fun j => pr r (cell F j)) hr.pos hq'.p_pos hr.p_pos
    (sum_cell_of_prob hp F) (sum_cell_of_prob (hq.prob hp) F)
  simp only [eq13, pf, hm]; exact key

/-- **Remark 3.4** (Diaconis-Zabell 1982 as a corollary of Theorem 3.1): in the matched-target
schema (2.7) (`r'(E_i) = q(E_i)`, `q'(F_j) = r(F_j)`), Jeffrey independence (3.15)-(3.16)
implies `r = r'`. -/
theorem remark34 (hp : IsProb p) (hq : ComesByPK q p E) (hr : ComesByPK r q F)
    (hq' : ComesByPK q' p F) (hr' : ComesByPK r' q' E)
    (hmE : ∀ i, pr r' (cell E i) = pr q (cell E i))
    (hmF : ∀ j, pr q' (cell F j) = pr r (cell F j))
    (h315 : ∀ i, pr q' (cell E i) = pr p (cell E i))
    (h316 : ∀ j, pr q (cell F j) = pr p (cell F j)) : r' = r := by
  obtain ⟨hE1, hE2⟩ := thm32_E hp hq hq' hr' hmE
  obtain ⟨hF1, hF2⟩ := thm32_F hp hq hr hq' hmF
  exact thm31 hp hq hr hq' hr' (hE1.2 (hE2.2 h315)) (hF1.2 (hF2.2 fun j => (h316 j).symm))

/-- **Remark 3.4**, last sentence: `p`-independence of `E` and `F` entails Jeffrey
independence (here (3.15); (3.16) is the same statement with `E` and `F` exchanged). -/
theorem remark34_indep (hp : IsProb p) (hq' : ComesByPK q' p F)
    (hind : ∀ i j, pr p (cell E i ∩ cell F j) = pr p (cell E i) * pr p (cell F j)) (i : ι) :
    pr q' (cell E i) = pr p (cell E i) := by
  have hcell : ∀ j, pr q' (cell E i ∩ cell F j) = pf q' p (cell F j) * pr p (cell E i ∩ cell F j) :=
    fun j => pr_eq_mul_of_forall fun ω hω => by
      rw [hq'.factor ω, (mem_cell.1 (mem_inter.1 hω).2)]
  rw [pr_split q' _ F]; simp_rw [hcell, hind]
  have : ∀ j, pf q' p (cell F j) * (pr p (cell E i) * pr p (cell F j)) =
      pr p (cell E i) * pr q' (cell F j) := by
    intro j; unfold pf; have := (hq'.p_pos j).ne'; field_simp
  simp_rw [this, ← mul_sum, sum_cell_of_prob (hq'.prob hp), mul_one]

/-! ## Theorem 4.1 (partial converse) -/

theorem pos_pair {x y : ℝ} (hx : 0 ≤ x) (hy : 0 ≤ y) (h : 0 < x * y) : 0 < x ∧ 0 < y := by
  refine ⟨lt_of_le_of_ne hx ?_, lt_of_le_of_ne hy ?_⟩ <;> rintro rfl <;> simp at h

theorem pr_nonneg (hp : IsProb p) (A : Finset Ω) : 0 ≤ pr p A := sum_nonneg fun ω _ => hp.1 ω

/-- **Theorem 4.1** (Wagner 2002 p. 7).  In schema (3.1), suppose
(4.3) `∀ i₁ i₂ ∃ j, p(E_{i₁}F_j) p(E_{i₂}F_j) > 0` and
(4.4) `∀ j₁ j₂ ∃ i, p(E_iF_{j₁}) p(E_iF_{j₂}) > 0`.
If `r' = r`, then the Bayes factor identities (3.2) and (3.3) hold. -/
theorem thm41 (hp : IsProb p) (hq : ComesByPK q p E) (hr : ComesByPK r q F)
    (hq' : ComesByPK q' p F) (hr' : ComesByPK r' q' E)
    (h43 : ∀ i₁ i₂, ∃ j, 0 < pr p (cell E i₁ ∩ cell F j) * pr p (cell E i₂ ∩ cell F j))
    (h44 : ∀ j₁ j₂, ∃ i, 0 < pr p (cell E i ∩ cell F j₁) * pr p (cell E i ∩ cell F j₂))
    (heq : r' = r) :
    (∀ i₁ i₂, bf r' q' (cell E i₁) (cell E i₂) = bf q p (cell E i₁) (cell E i₂)) ∧
      (∀ j₁ j₂, bf q' p (cell F j₁) (cell F j₂) = bf r q (cell F j₁) (cell F j₂)) := by
  set a := fun i => pf q p (cell E i)
  set b := fun j => pf r q (cell F j)
  set a' := fun i => pf r' q' (cell E i)
  set b' := fun j => pf q' p (cell F j)
  -- (4.9)-(4.10) on the cells `E_i F_j`
  have cellr : ∀ i j, pr r (cell E i ∩ cell F j) = a i * b j * pr p (cell E i ∩ cell F j) :=
    fun i j => pr_eq_mul_of_forall fun ω hω => by
      obtain ⟨h1, h2⟩ := mem_inter.1 hω
      rw [two_step hq hr ω, mem_cell.1 h1, mem_cell.1 h2]
  have cellr' : ∀ i j, pr r' (cell E i ∩ cell F j) = a' i * b' j * pr p (cell E i ∩ cell F j) :=
    fun i j => pr_eq_mul_of_forall fun ω hω => by
      obtain ⟨h1, h2⟩ := mem_inter.1 hω
      rw [two_step' hq' hr' ω, mem_cell.1 h1, mem_cell.1 h2]
  have key : ∀ i j, 0 < pr p (cell E i ∩ cell F j) → a i * b j = a' i * b' j := by
    intro i j hpos
    have h1 := cellr i j; have h2 := cellr' i j
    rw [heq, h1] at h2
    exact mul_right_cancel₀ hpos.ne' h2
  have ha : ∀ i, 0 < a i := hq.pf_pos
  have hb : ∀ j, 0 < b j := hr.pf_pos
  have ha' : ∀ i, 0 < a' i := hr'.pf_pos
  have hb' : ∀ j, 0 < b' j := hq'.pf_pos
  simp_rw [eq13]
  constructor
  · intro i₁ i₂
    obtain ⟨j, hj⟩ := h43 i₁ i₂
    obtain ⟨p1, p2⟩ := pos_pair (pr_nonneg hp _) (pr_nonneg hp _) hj
    have e1 := key i₁ j p1; have e2 := key i₂ j p2
    show a' i₁ / a' i₂ = a i₁ / a i₂
    rw [div_eq_div_iff (ha' i₂).ne' (ha i₂).ne']
    apply mul_right_cancel₀ (hb' j).ne'
    linear_combination (-(a i₂)) * e1 + a i₁ * e2
  · intro j₁ j₂
    obtain ⟨i, hi⟩ := h44 j₁ j₂
    obtain ⟨p1, p2⟩ := pos_pair (pr_nonneg hp _) (pr_nonneg hp _) hi
    have e1 := key i j₁ p1; have e2 := key i j₂ p2
    show b' j₁ / b' j₂ = b j₁ / b j₂
    rw [div_eq_div_iff (hb' j₂).ne' (hb j₂).ne']
    apply mul_right_cancel₀ (ha' i).ne'
    linear_combination (-(b j₂)) * e1 + b j₁ * e2

theorem nonempty_of_prob (hp : IsProb p) : Nonempty Ω := by
  by_contra h; rw [not_nonempty_iff] at h; have := hp.2; simp at this

/-- **Remark 4.1**, second sentence: (4.3) and (4.4) hold when `E` and `F` are qualitatively
independent (`E_i F_j ≠ ∅` for all `i, j`) and `p` is strictly coherent (every nonempty
event has positive probability; on a finite space, every atom). -/
theorem remark41_qi (hp : IsProb p) (hpos : ∀ ω, 0 < p ω) (hqi : ∀ i j, (cell E i ∩ cell F j).Nonempty) :
    (∀ i₁ i₂, ∃ j, 0 < pr p (cell E i₁ ∩ cell F j) * pr p (cell E i₂ ∩ cell F j)) ∧
      (∀ j₁ j₂, ∃ i, 0 < pr p (cell E i ∩ cell F j₁) * pr p (cell E i ∩ cell F j₂)) := by
  have hc : ∀ i j, 0 < pr p (cell E i ∩ cell F j) := fun i j =>
    sum_pos (fun ω _ => hpos ω) (hqi i j)
  obtain ⟨ω₀⟩ := nonempty_of_prob hp
  exact ⟨fun _ _ => ⟨F ω₀, mul_pos (hc _ _) (hc _ _)⟩,
    fun _ _ => ⟨E ω₀, mul_pos (hc _ _) (hc _ _)⟩⟩

/-- **Remark 4.1**, first sentence: when `F = E`, (4.3) fails as soon as `E` has two distinct
labels, whatever `p` is. -/
theorem remark41_FeqE {i₁ i₂ : ι} (hne : i₁ ≠ i₂) :
    ¬ ∀ i₁ i₂, ∃ j, 0 < pr p (cell E i₁ ∩ cell E j) * pr p (cell E i₂ ∩ cell E j) := by
  intro h
  obtain ⟨j, hj⟩ := h i₁ i₂
  have hempty : ∀ i, i ≠ j → cell E i ∩ cell E j = ∅ := by
    intro i hij; ext ω; simp only [mem_inter, mem_cell, notMem_empty, iff_false, not_and]
    intro h1 h2; exact hij (h1.symm.trans h2)
  by_cases h1 : i₁ = j
  · rw [hempty i₂ (fun h2 => hne (h1.trans h2.symm))] at hj; simp [pr] at hj
  · rw [hempty i₁ h1] at hj; simp [pr] at hj

/-- The example opening Section 4 (p. 6): if `F = E` and `r(E_i) = r'(E_i)` for all `i`, then
`r' = r` "no matter what values are assigned to `q(E_i)` and `q'(E_i)`". -/
theorem sec4_FeqE (hq : ComesByPK q p E) (hr : ComesByPK r q E) (hq' : ComesByPK q' p E)
    (hr' : ComesByPK r' q' E) (hm : ∀ i, pr r (cell E i) = pr r' (cell E i)) : r' = r := by
  funext ω
  have h1 := hr.factor ω; have h2 := hr'.factor ω
  rw [hq.factor ω] at h1; rw [hq'.factor ω] at h2
  rw [h1, h2]; unfold pf; rw [hm]
  have := (hq.pos (E ω)).ne'; have := (hq'.pos (E ω)).ne'; have := (hq.p_pos (E ω)).ne'
  field_simp

/-- **Remark 4.3** (Diaconis-Zabell's necessity as a corollary of Theorem 4.1): in the
matched-target schema (2.7), under (4.3)-(4.4), `r' = r` implies Jeffrey independence. -/
theorem remark43 (hp : IsProb p) (hq : ComesByPK q p E) (hr : ComesByPK r q F)
    (hq' : ComesByPK q' p F) (hr' : ComesByPK r' q' E)
    (h43 : ∀ i₁ i₂, ∃ j, 0 < pr p (cell E i₁ ∩ cell F j) * pr p (cell E i₂ ∩ cell F j))
    (h44 : ∀ j₁ j₂, ∃ i, 0 < pr p (cell E i ∩ cell F j₁) * pr p (cell E i ∩ cell F j₂))
    (hmE : ∀ i, pr r' (cell E i) = pr q (cell E i))
    (hmF : ∀ j, pr q' (cell F j) = pr r (cell F j)) (heq : r' = r) :
    (∀ i, pr q' (cell E i) = pr p (cell E i)) ∧ (∀ j, pr q (cell F j) = pr p (cell F j)) := by
  obtain ⟨h32, h33⟩ := thm41 hp hq hr hq' hr' h43 h44 heq
  obtain ⟨hE1, hE2⟩ := thm32_E hp hq hq' hr' hmE
  obtain ⟨hF1, hF2⟩ := thm32_F hp hq hr hq' hmF
  exact ⟨hE2.1 (hE1.1 h32), fun j => (hF2.1 (hF1.1 h33) j).symm⟩

/-- **Remark 4.2**: given (4.3)-(4.4), if Jeffrey independence (3.15)-(3.16) holds but
`r'(E_i) ≠ q(E_i)` for some `i`, or `q'(F_j) ≠ r(F_j)` for some `j`, then `r' ≠ r`. -/
theorem remark42 (hp : IsProb p) (hq : ComesByPK q p E) (hr : ComesByPK r q F)
    (hq' : ComesByPK q' p F) (hr' : ComesByPK r' q' E)
    (h43 : ∀ i₁ i₂, ∃ j, 0 < pr p (cell E i₁ ∩ cell F j) * pr p (cell E i₂ ∩ cell F j))
    (h44 : ∀ j₁ j₂, ∃ i, 0 < pr p (cell E i ∩ cell F j₁) * pr p (cell E i ∩ cell F j₂))
    (h315 : ∀ i, pr q' (cell E i) = pr p (cell E i))
    (h316 : ∀ j, pr q (cell F j) = pr p (cell F j))
    (hne : (∃ i, pr r' (cell E i) ≠ pr q (cell E i)) ∨ (∃ j, pr q' (cell F j) ≠ pr r (cell F j))) :
    r' ≠ r := by
  intro heq
  obtain ⟨h32, h33⟩ := thm41 hp hq hr hq' hr' h43 h44 heq
  obtain ⟨c, -, hc⟩ := (remark32 hq hr').1 h32
  simp_rw [eq13] at h33
  obtain ⟨d, -, hd⟩ := (ratio_iff_prop hr.pf_pos hq'.pf_pos).1 h33
  have hE : (fun i => pr r' (cell E i)) = fun i => pr q (cell E i) := by
    refine eq_of_prop (c := c) (sum_cell_of_prob (hr'.prob (hq'.prob hp)) E)
      (sum_cell_of_prob (hq.prob hp) E) fun i => ?_
    have hci := hc i; unfold pf at hci; rw [h315 i] at hci
    have := (hq.p_pos i).ne'; field_simp at hci ⊢; linarith
  have hF : (fun j => pr q' (cell F j)) = fun j => pr r (cell F j) := by
    refine eq_of_prop (c := d) (sum_cell_of_prob (hq'.prob hp) F)
      (sum_cell_of_prob (hr.prob (hq.prob hp)) F) fun j => ?_
    have hdj := hd j; unfold pf at hdj; rw [h316 j] at hdj
    have := (hq'.p_pos j).ne'; field_simp at hdj ⊢; linarith
  rcases hne with ⟨i, hi⟩ | ⟨j, hj⟩
  · exact hi (congrFun hE i)
  · exact hj (congrFun hF j)

/-! ## The normal forms (3.5), (3.7) and Field's (3.9)-(3.10) -/

/-- **(3.5)-(3.6)**: `q(ω) = B_i p(ω) / Σ_k B_k p(E_k)` with `B_i := β_{q,p}(E_i : E_{i₀})`
(Wagner writes `i₀ = 1`). -/
theorem eq35 (hq : ComesByPK q p E) (i₀ : ι) :
    ∀ ω, q ω = bf q p (cell E (E ω)) (cell E i₀) * p ω /
      ∑ i, bf q p (cell E i) (cell E i₀) * pr p (cell E i) := by
  have ha0 := (hq.pf_pos i₀).ne'
  have hsum : ∑ i, bf q p (cell E i) (cell E i₀) * pr p (cell E i) = 1 / pf q p (cell E i₀) := by
    simp_rw [eq13, div_mul_eq_mul_div, ← sum_div]
    have : ∀ i, pf q p (cell E i) * pr p (cell E i) = pr q (cell E i) := by
      intro i; unfold pf; have := (hq.p_pos i).ne'; field_simp
    simp_rw [this, hq.sum_cell]
  intro ω; rw [hsum, eq13, hq.factor ω]; field_simp

/-- A double-sum identity used for (3.7): summing `f(E ω) g(F ω) p(ω)` over atoms equals
summing `f(i) g(j) p(E_i F_j)` over cells. -/
theorem sum_cells (p : Ω → ℝ) (E : Ω → ι) (F : Ω → κ) (f : ι → ℝ) (g : κ → ℝ) :
    ∑ ω, f (E ω) * g (F ω) * p ω = ∑ i, ∑ j, f i * g j * pr p (cell E i ∩ cell F j) := by
  set x : Ω → ℝ := fun ω => f (E ω) * g (F ω) * p ω
  have hx : ∀ i j, pr x (cell E i ∩ cell F j) = f i * g j * pr p (cell E i ∩ cell F j) :=
    fun i j => pr_eq_mul_of_forall fun ω hω => by
      obtain ⟨h1, h2⟩ := mem_inter.1 hω; simp only [x]; rw [mem_cell.1 h1, mem_cell.1 h2]
  rw [show ∑ ω, f (E ω) * g (F ω) * p ω = ∑ ω, x ω from rfl, ← sum_pr_cell x E]
  refine sum_congr rfl fun i _ => ?_
  rw [pr_split x _ F]; exact sum_congr rfl fun j _ => hx i j

/-- **(3.7)**: `r(ω) = B_i b_j p(ω) / Σ_{i,j} B_i b_j p(E_i F_j)` with
`B_i := β_{q,p}(E_i : E_{i₀})`, `b_j := β_{r,q}(F_j : F_{j₀})`.  (3.8) is the same formula for
the other route, i.e. this theorem with `E` and `F` exchanged. -/
theorem eq37 (hp : IsProb p) (hq : ComesByPK q p E) (hr : ComesByPK r q F) (i₀ : ι) (j₀ : κ) :
    ∀ ω, r ω = bf q p (cell E (E ω)) (cell E i₀) * bf r q (cell F (F ω)) (cell F j₀) * p ω /
      ∑ i, ∑ j, bf q p (cell E i) (cell E i₀) * bf r q (cell F j) (cell F j₀) *
        pr p (cell E i ∩ cell F j) := by
  have ha0 := (hq.pf_pos i₀).ne'; have hb0 := (hr.pf_pos j₀).ne'
  set K := pf q p (cell E i₀) * pf r q (cell F j₀)
  have hK : K ≠ 0 := mul_ne_zero ha0 hb0
  have hB : ∀ i j, bf q p (cell E i) (cell E i₀) * bf r q (cell F j) (cell F j₀) =
      pf q p (cell E i) * pf r q (cell F j) / K := by
    intro i j; rw [eq13, eq13]; simp only [K]; field_simp
  have hsum : ∑ i, ∑ j, bf q p (cell E i) (cell E i₀) * bf r q (cell F j) (cell F j₀) *
      pr p (cell E i ∩ cell F j) = 1 / K := by
    simp_rw [hB, div_mul_eq_mul_div, ← sum_div]
    rw [← sum_cells p E F (fun i => pf q p (cell E i)) (fun j => pf r q (cell F j))]
    simp_rw [← two_step hq hr]; rw [(hr.prob (hq.prob hp)).2]
  intro ω; rw [hsum, hB, two_step hq hr ω]; field_simp

/-- **Remark 3.1** (Field's form (3.9)-(3.10)): with `G_i` the geometric mean
`(∏_k β_{q,p}(E_i : E_k))^{1/m}`, `q(ω) = G_i p(ω) / Σ_k G_k p(E_k)`. -/
theorem remark31 (hp : IsProb p) (hq : ComesByPK q p E) :
    ∀ ω, q ω = (∏ k, bf q p (cell E (E ω)) (cell E k)) ^ (1 / (Fintype.card ι : ℝ)) * p ω /
      ∑ i, (∏ k, bf q p (cell E i) (cell E k)) ^ (1 / (Fintype.card ι : ℝ)) * pr p (cell E i) := by
  haveI := nonempty_of_prob hp
  have hm : Fintype.card ι ≠ 0 := by
    haveI : Nonempty ι := ⟨E (Classical.arbitrary Ω)⟩; exact Fintype.card_ne_zero
  set a := fun i => pf q p (cell E i)
  have ha : ∀ i, 0 < a i := hq.pf_pos
  set P₀ := ∏ k, a k
  have hP₀ : 0 < P₀ := prod_pos fun k _ => ha k
  set K := P₀ ^ (1 / (Fintype.card ι : ℝ))
  have hKpos : 0 < K := Real.rpow_pos_of_pos hP₀ _
  have hG : ∀ i, (∏ k, bf q p (cell E i) (cell E k)) ^ (1 / (Fintype.card ι : ℝ)) = a i / K := by
    intro i
    have : ∏ k, bf q p (cell E i) (cell E k) = a i ^ Fintype.card ι / P₀ := by
      simp_rw [eq13]; rw [prod_div_distrib, prod_const, card_univ]
    rw [this, Real.div_rpow (pow_nonneg (ha i).le _) hP₀.le, one_div,
      Real.pow_rpow_inv_natCast (ha i).le hm, ← one_div]
  have hsum : ∑ i, a i / K * pr p (cell E i) = 1 / K := by
    simp_rw [div_mul_eq_mul_div, ← sum_div]
    have : ∀ i, a i * pr p (cell E i) = pr q (cell E i) := by
      intro i; simp only [a, pf]; have := (hq.p_pos i).ne'; field_simp
    simp_rw [this, hq.sum_cell]
  intro ω; simp_rw [hG]; rw [hsum, hq.factor ω]; field_simp; simp only [a]; ring

/-! ## Section 5 -/

/-- Conditioning on an event. -/
noncomputable def cond (p : Ω → ℝ) (A : Finset Ω) : Ω → ℝ :=
  fun ω => if ω ∈ A then p ω / pr p A else 0

theorem pr_cond (p : Ω → ℝ) (A B : Finset Ω) : pr (cond p A) B = pr p (B ∩ A) / pr p A := by
  unfold pr cond; rw [sum_ite_mem, sum_div]; rfl

/-- (1.1) remark (p. 2): when `q = p(·|A)`, the Bayes factor `β_{q,p}(B:G)` is the likelihood
ratio `p(A|B)/p(A|G)`. -/
theorem bf_cond {p : Ω → ℝ} {A : Finset Ω} (hA : pr p A ≠ 0) (B G : Finset Ω) :
    bf (cond p A) p B G = (pr p (B ∩ A) / pr p B) / (pr p (G ∩ A) / pr p G) := by
  unfold bf; rw [pr_cond, pr_cond, div_div_div_cancel_right₀ hA, div_div_div_comm]

theorem cond_cond {p : Ω → ℝ} {A : Finset Ω} (hA : pr p A ≠ 0) : cond (cond p A) A = cond p A := by
  have h1 : pr (cond p A) A = 1 := by rw [pr_cond, inter_self, div_self hA]
  funext ω
  show (if ω ∈ A then cond p A ω / pr (cond p A) A else 0) = cond p A ω
  rw [h1, div_one]; unfold cond; split_ifs <;> rfl

/-- **Remark 5.1 / note 8** (Garber's repeated glances): with a phenomenological event `A`,
`q = p(·|A)` and `r = q(·|A)`, every Bayes factor of the second revision is one. -/
theorem note8 {p : Ω → ℝ} {A B G : Finset Ω} (hA : pr p A ≠ 0)
    (hB : pr (cond p A) B ≠ 0) (hG : pr (cond p A) G ≠ 0) :
    bf (cond (cond p A) A) (cond p A) B G = 1 := by
  rw [cond_cond hA]; unfold bf; exact div_self (div_ne_zero hB hG)

/-- Probability kinematics twice on the same partition is kinematics on the last targets. -/
theorem kin_kin {e f : ι → ℝ} (he : ∀ i, 0 < e i) (hpE : ∀ i, 0 < pr p (cell E i)) :
    kin (kin p E e) E f = kin p E f := by
  funext ω
  show f (E ω) * kin p E e ω / pr (kin p E e) (cell E (E ω)) = kin p E f ω
  rw [pr_kin_cell (hpE _)]; unfold kin
  have := (he (E ω)).ne'; have := (hpE (E ω)).ne'; field_simp

/-- **Remark 5.2**, first sentence: if `F = E` in schema (2.7), then `r' = q` and `r = q'`. -/
theorem remark52 {e f : ι → ℝ} (he : ∀ i, 0 < e i) (hf : ∀ i, 0 < f i)
    (hpE : ∀ i, 0 < pr p (cell E i)) :
    kin (kin p E f) E e = kin p E e ∧ kin (kin p E e) E f = kin p E f :=
  ⟨kin_kin hf hpE, kin_kin he hpE⟩

/-- **Remark 5.2**, the Bayes factor argument: with `F = E`, `r' = q` and `r = q'`, identity
(3.2) forces `q' = p` and identity (3.3) forces `q = p`.  So if `q' ≠ q`, one of them fails. -/
theorem remark52_bf (hp : IsProb p) (hq : ComesByPK q p E) (hq' : ComesByPK q' p E) :
    ((∀ i₁ i₂, bf q q' (cell E i₁) (cell E i₂) = bf q p (cell E i₁) (cell E i₂)) → q' = p) ∧
      ((∀ i₁ i₂, bf q' p (cell E i₁) (cell E i₂) = bf q' q (cell E i₁) (cell E i₂)) → q = p) := by
  have back : ∀ {x : Ω → ℝ}, ComesByPK x p E → (∀ i, pr x (cell E i) = pr p (cell E i)) → x = p := by
    intro x hx h; funext ω; rw [hx.factor ω]; unfold pf; rw [h, div_self (hx.p_pos _).ne', one_mul]
  constructor
  · intro h
    have key := common_numerator (t := fun i => pr q (cell E i)) hq.pos hq'.pos hq.p_pos
      (sum_cell_of_prob (hq'.prob hp) E) (sum_cell_of_prob hp E)
    simp only [eq13, pf] at h
    exact back hq' (key.2.1 (key.1.1 h))
  · intro h
    have key := common_numerator (t := fun i => pr q' (cell E i)) hq'.pos hq.p_pos hq.pos
      (sum_cell_of_prob hp E) (sum_cell_of_prob (hq.prob hp) E)
    simp only [eq13, pf] at h
    exact back hq fun i => (key.2.1 (key.1.1 h) i).symm

/-- **Note 7** (and the finite case of Wagner 2003 Theorem 5.1): any positive vector of Bayes
factors can be realized from any prior with positive cells, so a Bayes factor vector carries no
information about the prior.  Here `q(E_i) = w_i p(E_i) / Σ_k w_k p(E_k)`. -/
theorem note7 [Nonempty ι] {x : Ω → ℝ} {G : Ω → ι} (hG : ∀ i, 0 < pr x (cell G i)) {w : ι → ℝ}
    (hw : ∀ i, 0 < w i) :
    ∃ y, ComesByPK y x G ∧ ∀ i, pf y x (cell G i) = w i / ∑ k, w k * pr x (cell G k) := by
  have hS : 0 < ∑ k, w k * pr x (cell G k) :=
    sum_pos (fun k _ => mul_pos (hw k) (hG k)) univ_nonempty
  refine ⟨kin x G (fun i => w i * pr x (cell G i) / ∑ k, w k * pr x (cell G k)),
    ⟨_, fun i => div_pos (mul_pos (hw i) (hG i)) hS, ?_, hG, rfl⟩, fun i => ?_⟩
  · rw [← sum_div, div_self hS.ne']
  · unfold pf; rw [pr_kin_cell (hG i)]; have := (hG i).ne'; field_simp

theorem ratio_of_pf {y x : Ω → ℝ} {G : Ω → ι} {w : ι → ℝ} {S : ℝ}
    (h : ∀ i, pf y x (cell G i) = w i / S) (i₁ i₂ : ι) (hS : S ≠ 0) :
    bf y x (cell G i₁) (cell G i₂) = w i₁ / w i₂ := by
  rw [eq13, h, h, div_div_div_cancel_right₀ hS]

/-- **Remark 5.3, (5.5)**: a probability `r'` from `q'` by kinematics on `E` satisfies (3.2)
iff `r'(E_i) = [q(E_i) q'(E_i)/p(E_i)] / Σ_k q(E_k) q'(E_k)/p(E_k)`. -/
theorem eq55 (hp : IsProb p) (hq : ComesByPK q p E) (hq' : ComesByPK q' p F)
    (hr' : ComesByPK r' q' E) :
    (∀ i₁ i₂, bf r' q' (cell E i₁) (cell E i₂) = bf q p (cell E i₁) (cell E i₂)) ↔
      ∀ i, pr r' (cell E i) = pr q (cell E i) * pr q' (cell E i) / pr p (cell E i) /
        ∑ k, pr q (cell E k) * pr q' (cell E k) / pr p (cell E k) := by
  have hw : ∀ i, 0 < pr q (cell E i) * pr q' (cell E i) / pr p (cell E i) :=
    fun i => div_pos (mul_pos (hq.pos i) (hr'.p_pos i)) (hq.p_pos i)
  rw [remark32 hq hr']
  constructor
  · rintro ⟨c, -, hc⟩
    refine eq_normalize (c := c) (sum_cell_of_prob (hr'.prob (hq'.prob hp)) E) fun i => ?_
    have hci := hc i; unfold pf at hci
    have := (hr'.p_pos i).ne'; have := (hq.p_pos i).ne'
    field_simp at hci ⊢; linarith
  · intro h
    have hS : 0 < ∑ k, pr q (cell E k) * pr q' (cell E k) / pr p (cell E k) :=
      sum_pos (fun k _ => hw k) (by haveI := hq.nonempty; exact univ_nonempty)
    refine ⟨1 / ∑ k, pr q (cell E k) * pr q' (cell E k) / pr p (cell E k), by positivity,
      fun i => ?_⟩
    unfold pf; rw [h i]
    have := (hr'.p_pos i).ne'; have := (hq.p_pos i).ne'; have := hS.ne'
    field_simp

/-- **Remark 5.3** for finite partitions: (5.3) always holds, so (3.2) is always a recipe that
produces an `r'`. -/
theorem eq55_exists (hq : ComesByPK q p E) (hq'E : ∀ i, 0 < pr q' (cell E i)) :
    ∃ r', ComesByPK r' q' E ∧
      ∀ i₁ i₂, bf r' q' (cell E i₁) (cell E i₂) = bf q p (cell E i₁) (cell E i₂) := by
  haveI := hq.nonempty
  obtain ⟨r', hr', hpf⟩ := note7 hq'E hq.pf_pos
  have hS : ∑ k, pf q p (cell E k) * pr q' (cell E k) ≠ 0 :=
    (sum_pos (fun k _ => mul_pos (hq.pf_pos k) (hq'E k)) univ_nonempty).ne'
  exact ⟨r', hr', fun i₁ i₂ => by rw [ratio_of_pf hpf i₁ i₂ hS, eq13]⟩

/-! ## The project's question, not Wagner's: what Theorems 3.1 and 4.1 say about `P^B`

Paper B's benchmark is `P^B(ω) ∝ p(ω) ℓ^A_{E ω} ℓ^B_{F ω}` with `ℓ^A_i = x_i / p(E_i)` and
`ℓ^B_j = y_j / p(F_j)`, where `x` and `y` are the credences the two cues deliver. -/

/-- The matched likelihood `x_i / p(E_i)` of a delivered credence `x` against the prior. -/
noncomputable def lik (p : Ω → ℝ) (E : Ω → ι) (x : ι → ℝ) (i : ι) : ℝ := x i / pr p (cell E i)

/-- Paper B's `P^B`. -/
noncomputable def benchmark (p : Ω → ℝ) (E : Ω → ι) (F : Ω → κ) (x : ι → ℝ) (y : κ → ℝ) :
    Ω → ℝ :=
  fun ω => lik p E x (E ω) * lik p F y (F ω) * p ω /
    ∑ ω', lik p E x (E ω') * lik p F y (F ω') * p ω'

theorem kin_isPK {x : ι → ℝ} (hx : ∀ i, 0 < x i) (hxs : ∑ i, x i = 1)
    (hpE : ∀ i, 0 < pr p (cell E i)) : ComesByPK (kin p E x) p E := ⟨x, hx, hxs, hpE, rfl⟩

theorem pf_kin {x : ι → ℝ} (hpE : ∀ i, 0 < pr p (cell E i)) (i : ι) :
    pf (kin p E x) p (cell E i) = lik p E x i := by
  unfold pf lik; rw [pr_kin_cell (hpE i)]

/-- `P^B` is the common endpoint of schema (3.1) when each cue's first-position revision is its
delivered credence against the prior (`q = kin p E x`, `q' = kin p F y`) and the
second-position revisions carry the same Bayes factors, (3.2)-(3.3). -/
theorem PB_endpoint (hp : IsProb p) {x : ι → ℝ} {y : κ → ℝ} (hx : ∀ i, 0 < x i)
    (hxs : ∑ i, x i = 1) (hy : ∀ j, 0 < y j) (hys : ∑ j, y j = 1)
    (hpE : ∀ i, 0 < pr p (cell E i)) (hpF : ∀ j, 0 < pr p (cell F j))
    (hr : ComesByPK r (kin p E x) F) (hr' : ComesByPK r' (kin p F y) E)
    (h32 : ∀ i₁ i₂, bf r' (kin p F y) (cell E i₁) (cell E i₂) =
      bf (kin p E x) p (cell E i₁) (cell E i₂))
    (h33 : ∀ j₁ j₂, bf (kin p F y) p (cell F j₁) (cell F j₂) =
      bf r (kin p E x) (cell F j₁) (cell F j₂)) :
    r = benchmark p E F x y ∧ r' = benchmark p E F x y := by
  have hq := kin_isPK hx hxs hpE
  have hq' := kin_isPK hy hys hpF
  have hrr := thm31 hp hq hr hq' hr' h32 h33
  simp_rw [eq13] at h33
  obtain ⟨d, hd, hdj⟩ := (ratio_iff_prop hr.pf_pos hq'.pf_pos).1 h33
  have hform : ∀ ω, r ω = d⁻¹ * (lik p E x (E ω) * lik p F y (F ω) * p ω) := by
    intro ω; rw [two_step hq hr ω, ← pf_kin hpE, ← pf_kin (x := y) hpF, hdj]
    field_simp
  have hr1 := (hr.prob (hq.prob hp)).2
  refine ⟨funext fun ω => eq_normalize (c := d⁻¹) hr1 hform ω, ?_⟩
  rw [hrr]; exact funext fun ω => eq_normalize (c := d⁻¹) hr1 hform ω

theorem pos_cross (hp : IsProb p) (hq : ComesByPK q p E) {F : Ω → κ}
    (hpF : ∀ j, 0 < pr p (cell F j)) (j : κ) : 0 < pr q (cell F j) :=
  pr_pos_of_mul hp.1 (fun ω => hq.pf_pos (E ω)) hq.factor (hpF j)

/-- The second-position revisions required by (3.2)-(3.3) always exist for finite partitions
(Remark 5.3), so `P^B` is attained: there is a commuting schema of the kind in
`PB_endpoint`. -/
theorem PB_endpoint_exists (hp : IsProb p) {x : ι → ℝ} {y : κ → ℝ} (hx : ∀ i, 0 < x i)
    (hxs : ∑ i, x i = 1) (hy : ∀ j, 0 < y j) (hys : ∑ j, y j = 1)
    (hpE : ∀ i, 0 < pr p (cell E i)) (hpF : ∀ j, 0 < pr p (cell F j)) :
    ∃ r r', ComesByPK r (kin p E x) F ∧ ComesByPK r' (kin p F y) E ∧
      (∀ i₁ i₂, bf r' (kin p F y) (cell E i₁) (cell E i₂) =
        bf (kin p E x) p (cell E i₁) (cell E i₂)) ∧
      (∀ j₁ j₂, bf (kin p F y) p (cell F j₁) (cell F j₂) =
        bf r (kin p E x) (cell F j₁) (cell F j₂)) := by
  have hq := kin_isPK hx hxs hpE
  have hq' := kin_isPK hy hys hpF
  obtain ⟨r', hr', h32⟩ := eq55_exists hq (pos_cross hp hq' hpE)
  obtain ⟨r, hr, h33⟩ := eq55_exists hq' (pos_cross hp hq hpF)
  exact ⟨r, r', hr, hr', h32, fun j₁ j₂ => (h33 j₁ j₂).symm⟩

/-- **Uniqueness relative to the anchoring**, from Theorem 4.1: if both first-position
revisions are the delivered cues against the prior, then under (4.3)-(4.4) the only way
the two routes can end in the same place is at `P^B`. -/
theorem PB_unique (hp : IsProb p) {x : ι → ℝ} {y : κ → ℝ} (hx : ∀ i, 0 < x i)
    (hxs : ∑ i, x i = 1) (hy : ∀ j, 0 < y j) (hys : ∑ j, y j = 1)
    (hpE : ∀ i, 0 < pr p (cell E i)) (hpF : ∀ j, 0 < pr p (cell F j))
    (h43 : ∀ i₁ i₂, ∃ j, 0 < pr p (cell E i₁ ∩ cell F j) * pr p (cell E i₂ ∩ cell F j))
    (h44 : ∀ j₁ j₂, ∃ i, 0 < pr p (cell E i ∩ cell F j₁) * pr p (cell E i ∩ cell F j₂))
    (hr : ComesByPK r (kin p E x) F) (hr' : ComesByPK r' (kin p F y) E) (heq : r' = r) :
    r = benchmark p E F x y := by
  have hq := kin_isPK hx hxs hpE
  have hq' := kin_isPK hy hys hpF
  obtain ⟨h32, h33⟩ := thm41 hp hq hr hq' hr' h43 h44 heq
  exact (PB_endpoint hp hx hxs hy hys hpE hpF hr hr' h32 h33).1

/-- **What Theorems 3.1/4.1 do not say.**  For *any* route `p → q' → r'` (kinematics on `F`,
then on `E`) there is a route `p → q → r` (on `E`, then `F`) satisfying (3.2)-(3.3), and it
ends at `r'`.  So the endpoint of a Wagner-consistent commuting schema is fixed only once one
says which revision carries each cue's Bayes factor. -/
theorem completion (hp : IsProb p) (hq' : ComesByPK q' p F) (hr' : ComesByPK r' q' E)
    (hpE : ∀ i, 0 < pr p (cell E i)) :
    ∃ q r, ComesByPK q p E ∧ ComesByPK r q F ∧
      (∀ i₁ i₂, bf r' q' (cell E i₁) (cell E i₂) = bf q p (cell E i₁) (cell E i₂)) ∧
      (∀ j₁ j₂, bf q' p (cell F j₁) (cell F j₂) = bf r q (cell F j₁) (cell F j₂)) ∧ r = r' := by
  haveI := hr'.nonempty; haveI := hq'.nonempty
  obtain ⟨q, hq, hpfq⟩ := note7 hpE hr'.pf_pos
  have hS : ∑ k, pf r' q' (cell E k) * pr p (cell E k) ≠ 0 :=
    (sum_pos (fun k _ => mul_pos (hr'.pf_pos k) (hpE k)) univ_nonempty).ne'
  have h32 : ∀ i₁ i₂, bf r' q' (cell E i₁) (cell E i₂) = bf q p (cell E i₁) (cell E i₂) :=
    fun i₁ i₂ => by rw [ratio_of_pf hpfq i₁ i₂ hS, eq13]
  have hqF := pos_cross hp hq hq'.p_pos
  obtain ⟨r, hr, hpfr⟩ := note7 hqF hq'.pf_pos
  have hS' : ∑ k, pf q' p (cell F k) * pr q (cell F k) ≠ 0 :=
    (sum_pos (fun k _ => mul_pos (hq'.pf_pos k) (hqF k)) univ_nonempty).ne'
  have h33 : ∀ j₁ j₂, bf q' p (cell F j₁) (cell F j₂) = bf r q (cell F j₁) (cell F j₂) :=
    fun j₁ j₂ => by rw [ratio_of_pf hpfr j₁ j₂ hS', eq13]
  exact ⟨q, r, hq, hr, h32, h33, (thm31 hp hq hr hq' hr' h32 h33).symm⟩

/-! ### Paper B's grid `A × B` -/

theorem pr_fst (p : ι × κ → ℝ) (i : ι) : pr p (cell Prod.fst i) = ∑ j, p (i, j) := by
  unfold pr cell; rw [sum_filter, Fintype.sum_prod_type, sum_eq_single i]
  · simp
  · intro b _ hb; simp [hb]
  · simp

theorem pr_snd (p : ι × κ → ℝ) (j : κ) : pr p (cell Prod.snd j) = ∑ i, p (i, j) := by
  unfold pr cell; rw [sum_filter, Fintype.sum_prod_type]; simp

/-- On a grid `A × B` with a full-support prior the two cue partitions are qualitatively
independent, so (4.3)-(4.4) hold (Remark 4.1) and Theorem 4.1 applies. -/
theorem grid_43_44 {p : ι × κ → ℝ} (hp : IsProb p) (hpos : ∀ ω, 0 < p ω) :
    (∀ i₁ i₂, ∃ j, 0 < pr p (cell Prod.fst i₁ ∩ cell Prod.snd j) *
        pr p (cell Prod.fst i₂ ∩ cell Prod.snd j)) ∧
      (∀ j₁ j₂, ∃ i, 0 < pr p (cell Prod.fst i ∩ cell Prod.snd j₁) *
        pr p (cell Prod.fst i ∩ cell Prod.snd j₂)) :=
  remark41_qi hp hpos fun i j => ⟨(i, j), by simp⟩

/-- A numerical instance on `Fin 2 × Fin 2` (the prior of Pettigrew-Weisberg's p. 4 example,
cues `x = (4/5, 1/5)`, `y = (3/5, 2/5)`).  `P^B(0,0) = 27/40`, the A-first sequence gives
`27/50`, the B-first sequence gives `36/55`. -/
noncomputable def p₀ : Fin 2 × Fin 2 → ℝ := fun ω => ![![3/10, 1/10], ![2/10, 4/10]] ω.1 ω.2
noncomputable def x₀ : Fin 2 → ℝ := ![4/5, 1/5]
noncomputable def y₀ : Fin 2 → ℝ := ![3/5, 2/5]

theorem p₀_prob : IsProb p₀ := by
  refine ⟨fun ω => ?_, ?_⟩
  · rcases ω with ⟨i, j⟩; fin_cases i <;> fin_cases j <;> simp [p₀] <;> norm_num
  · simp [p₀, Fintype.sum_prod_type, Fin.sum_univ_two]; norm_num

theorem p₀_fst (i : Fin 2) : 0 < pr p₀ (cell Prod.fst i) := by
  rw [pr_fst]; fin_cases i <;> simp [p₀, Fin.sum_univ_two] <;> norm_num

theorem p₀_snd (j : Fin 2) : 0 < pr p₀ (cell Prod.snd j) := by
  rw [pr_snd]; fin_cases j <;> simp [p₀, Fin.sum_univ_two] <;> norm_num

theorem benchmark_p₀ : benchmark p₀ Prod.fst Prod.snd x₀ y₀ (0, 0) = 27 / 40 := by
  simp only [benchmark, lik, pr_fst, pr_snd]
  simp [p₀, x₀, y₀, Fintype.sum_prod_type, Fin.sum_univ_two]; norm_num

theorem JAB_p₀ : kin (kin p₀ Prod.fst x₀) Prod.snd y₀ (0, 0) = 27 / 50 := by
  simp only [kin, pr_fst, pr_snd]
  simp [p₀, x₀, y₀, Fin.sum_univ_two]; norm_num

theorem JBA_p₀ : kin (kin p₀ Prod.snd y₀) Prod.fst x₀ (0, 0) = 36 / 55 := by
  simp only [kin, pr_fst, pr_snd]
  simp [p₀, x₀, y₀, Fin.sum_univ_two]; norm_num

/-- **Theorems 3.1/4.1 do not single out `P^B`.**  The B-first Jeffrey sequence is itself the
common endpoint of a schema satisfying Wagner's (3.2)-(3.3), and it differs from `P^B`.  What
selects `P^B` is the modelling choice that each cue's Bayes factor is the one it has against
the prior (first position), as in `PB_unique`. -/
theorem wagner_does_not_single_out_PB :
    ∃ q r, ComesByPK q p₀ Prod.fst ∧ ComesByPK r q Prod.snd ∧
      (∀ i₁ i₂, bf (kin (kin p₀ Prod.snd y₀) Prod.fst x₀) (kin p₀ Prod.snd y₀)
          (cell Prod.fst i₁) (cell Prod.fst i₂) = bf q p₀ (cell Prod.fst i₁) (cell Prod.fst i₂)) ∧
      (∀ j₁ j₂, bf (kin p₀ Prod.snd y₀) p₀ (cell Prod.snd j₁) (cell Prod.snd j₂) =
          bf r q (cell Prod.snd j₁) (cell Prod.snd j₂)) ∧
      r = kin (kin p₀ Prod.snd y₀) Prod.fst x₀ ∧ r ≠ benchmark p₀ Prod.fst Prod.snd x₀ y₀ := by
  have hy : ∀ j, 0 < y₀ j := by intro j; fin_cases j <;> simp [y₀]
  have hys : ∑ j, y₀ j = 1 := by simp [y₀, Fin.sum_univ_two]; norm_num
  have hx : ∀ i, 0 < x₀ i := by intro i; fin_cases i <;> simp [x₀]
  have hxs : ∑ i, x₀ i = 1 := by simp [x₀, Fin.sum_univ_two]; norm_num
  have hq' := kin_isPK hy hys p₀_snd
  have hr' := kin_isPK (p := kin p₀ Prod.snd y₀) (E := Prod.fst) hx hxs
    (pos_cross p₀_prob hq' p₀_fst)
  obtain ⟨q, r, hq, hr, h32, h33, hrr⟩ := completion p₀_prob hq' hr' p₀_fst
  refine ⟨q, r, hq, hr, h32, h33, hrr, fun h => ?_⟩
  have := congrFun h (0, 0)
  rw [hrr, JBA_p₀, benchmark_p₀] at this; norm_num at this

/-! ### A numerical instance of the example opening Section 4 -/

/-- With `F = E` (here `Ω = Fin 2`, `E = id`), `r' = r` although (3.2) fails:
`β_{r',q'}(E₀:E₁) = 1` while `β_{q,p}(E₀:E₁) = 1/3`. -/
theorem sec4_example :
    let p : Fin 2 → ℝ := ![1/2, 1/2]
    let q := kin p id ![1/4, 3/4]
    let r := kin q id ![1/2, 1/2]
    let q' := kin p id ![1/2, 1/2]
    let r' := kin q' id ![1/2, 1/2]
    ComesByPK q p id ∧ ComesByPK r q id ∧ ComesByPK q' p id ∧ ComesByPK r' q' id ∧ r' = r ∧
      bf r' q' (cell id 0) (cell id 1) = 1 ∧ bf q p (cell id 0) (cell id 1) = 1 / 3 := by
  intro p q r q' r'
  have hcell : ∀ (x : Fin 2 → ℝ) (i : Fin 2), pr x (cell id i) = x i := by
    intro x i; unfold pr cell; simp [filter_eq']
  have hp : ∀ i, 0 < pr p (cell id i) := by
    intro i; rw [hcell]; fin_cases i <;> simp [p]
  have hpos2 : ∀ i : Fin 2, 0 < (![1/2, 1/2] : Fin 2 → ℝ) i := by
    intro i; fin_cases i <;> simp
  have hs2 : ∑ i, (![1/2, 1/2] : Fin 2 → ℝ) i = 1 := by simp [Fin.sum_univ_two]; norm_num
  have hposq : ∀ i : Fin 2, 0 < (![1/4, 3/4] : Fin 2 → ℝ) i := by
    intro i; fin_cases i <;> simp
  have hsq : ∑ i, (![1/4, 3/4] : Fin 2 → ℝ) i = 1 := by simp [Fin.sum_univ_two]; norm_num
  have hq : ComesByPK q p id := kin_isPK hposq hsq hp
  have hq' : ComesByPK q' p id := kin_isPK hpos2 hs2 hp
  have hr : ComesByPK r q id := kin_isPK hpos2 hs2 hq.pos
  have hr' : ComesByPK r' q' id := kin_isPK hpos2 hs2 hq'.pos
  refine ⟨hq, hr, hq', hr', sec4_FeqE hq hr hq' hr' fun i => ?_, ?_, ?_⟩
  · simp only [r, r']; rw [pr_kin_cell (hq.pos i), pr_kin_cell (hq'.pos i)]
  · unfold bf; simp only [r']
    rw [pr_kin_cell (hq'.pos 0), pr_kin_cell (hq'.pos 1)]; simp only [q']
    rw [pr_kin_cell (hp 0), pr_kin_cell (hp 1)]; norm_num
  · unfold bf; simp only [q]
    rw [pr_kin_cell (hp 0), pr_kin_cell (hp 1), hcell, hcell]; simp [p]; norm_num

end Literature.Wagner2002
