/-
# Good & Mittal (1987), "The Amalgamation and Geometry of Two-by-Two Contingency Tables"

*Annals of Statistics* 15(2), 694-711.  (Read in full from the scanned original,
pp. 694-711 including the Appendix and references.)

Formalization of the paper's own definitions and its Section 4 results on when
amalgamation can and cannot produce the paradox, not of Paper B's claims.

## The paper's setup (pp. 694-696)

A two-by-two table `a = [a, b; c, d]` has rows `T, T̄` (treatment / non-treatment)
and columns `S, S̄` (success / failure), with `abcd ≠ 0` and `N = a+b+c+d`
(p. 694).  Tables `a_i` of `n` mutually exclusive subpopulations are *amalgamated*
by addition, `A = [Σa_i, Σb_i; Σc_i, Σd_i]` (pp. 694-695).  A *measure of
association* is a function `α(a)` (p. 694).

**Definition 1.1 (p. 695).** The amalgamation paradox occurs if
`max_i α(a_i) < α(A)` or `α(A) < min_i α(a_i)`.  It includes Yule's (1903) case,
`α(a_i) = 0` for all `i` but `α(A) ≠ 0`, which is why the authors prefer
"amalgamation" to "reversal" paradox (p. 695).

**Definitions 2.1-2.2 (p. 696).** Row-uniform (row-fair): `(a_i+b_i)/(c_i+d_i) = λ`
for all `i`; column-uniform (column-fair): `(a_i+c_i)/(b_i+d_i) = μ` for all `i`.

**Measures (Section 3, pp. 698-700).** Peirce's `π_R = a/(a+b) - c/(c+d)` (3.1),
`π_C = a/(a+c) - b/(b+d)` (3.3); Yule's `y = (ad-bc)/N²` (3.5); the odds ratio
`κ = ad/(bc)` (3.6); weights of evidence `W_R = log[a(c+d)/(c(a+b))]` (3.10) and
`W_C` (3.8); causal propensities `Q_R = log[d(a+b)/(b(c+d))]` (3.12) and `Q_C` (3.14).

## What is formalized

* `Tab`, `amalg`, `Paradox` (Definition 1.1, in the form "every subtable measure is
  strictly below, or every one strictly above, the aggregate"), and
  `paradox_iff_max_min`, which recovers the paper's `max`/`min` wording.
* `RowUniform`, `ColUniform` (Definitions 2.1-2.2, cross-multiplied).
* `mediant_between` — **Lemma 4.1** (p. 701), in the between-ness form it is used.
* `piR_amalg_general` — with *no* design assumption, `π_R(A)` is the difference of
  two separately weighted averages: the treated-row success rates weighted by the
  treated-row totals `a_i+b_i`, minus the untreated-row success rates weighted by
  the untreated-row totals `c_i+d_i`.  This is the identity behind the paper's
  proof of Theorem 5.4 ((5.16)-(5.17), p. 705), stated for `n` tables.
* `piR_weighted_rowUniform`, `yule_weighted_rowUniform`, `no_paradox_piR`,
  `no_paradox_yule` — **Theorem 4.1** (p. 701): under a row-uniform design,
  `α(A) = Σ (N_i/N) α(a_i)` for `α = π_R, y`, hence the paradox cannot occur.
* `no_paradox_piC` — **Corollary 4.1** (p. 701), by transposition.
* `expQR_weighted_rowUniform`, `expWR_weighted_rowUniform`, `no_paradox_QR`,
  `no_paradox_WR` — **Theorem 4.2** (p. 702): `exp α(A) = Σ δ_i exp α(a_i)` with
  `δ_i = b_i/Σb` (for `Q_R`) or `c_i/Σc` (for `W_R`), hence no paradox.
* `kappa_between_of_uniform` — **Theorem 4.3** (p. 702-703) for `n = 2`: under a
  design both row- and column-uniform, `κ(A)` lies between `κ(a_1)` and `κ(a_2)`.
  The proof here is not the paper's four-case argument: the two uniformities
  force `a_1 = ρ a_2 + t`, `b_1 = ρ b_2 - t`, `c_1 = ρ c_2 - t`, `d_1 = ρ d_2 + t`
  (with `ρ = N_1/N_2`), so the sign of `t` alone orders `a/c` and `d/b` in the
  same direction in both tables, which is the paper's concluding step (p. 703).
* `note_p702_kappa_paradox` — the paper's **Note** (p. 702): `a_1 = [3,1;1,9]`,
  `a_2 = [889,203;381,2349]` are row-uniform with `λ = 0.4`, `κ(a_1) = κ(a_2) = 27`,
  yet `κ(A) = 26.991...`: row-uniformity alone does not protect `κ`.
* `equalSize_reversal` — a worked instance of the paper's remark (p. 696) that the
  paradox "can happen even though `N_i ∝ p_i`": two subpopulations of *equal* size
  (e.g. men and women, `N_1 = N_2 = 100`), the treatment beneficial in each
  (`π_R > 0`), harmful in the aggregate (`π_R(A) < 0`).  The design is not
  row-uniform (`equalSize_not_rowUniform`).
* `yule_case_paradox` — Yule's case (p. 695): no association in either
  subpopulation (`π_R = 0`, `κ = 1`), association in the aggregate.
* `kappa_homogeneous_iff` — **Theorem 5.1** (pp. 703-704): `κ(a_1) = κ(a_2) = κ(A)`
  iff (5.2) `a_1/c_1 = a_2/c_2 ∧ b_1/d_1 = b_2/d_2` or (5.3)
  `a_1/b_1 = a_2/b_2 ∧ c_1/d_1 = c_2/d_2` (cross-multiplied).

Not formalized: Theorems 5.2-5.6 (homogeneity for `W`, `Q`, `π`), the general-`n`
extension of Theorem 4.3 (the paper's "amalgamate two, then add one at a time"
step is itself only valid when the partial amalgams stay row- and column-uniform,
which they do; omitted for length), and the Appendix's approximate-fairness
Theorems A.1-A.2 (a perturbation/derivative argument).
-/
import Mathlib

namespace Literature.GoodMittal

open Finset

/-! ## Tables, amalgamation, the paradox -/

/-- A two-by-two table `[a, b; c, d]`: rows `T, T̄`, columns `S, S̄` (p. 694). -/
@[ext]
structure Tab where
  a : ℝ
  b : ℝ
  c : ℝ
  d : ℝ

namespace Tab

/-- All four cells positive (the paper's `abcd ≠ 0` for frequency tables). -/
def Pos (t : Tab) : Prop := 0 < t.a ∧ 0 < t.b ∧ 0 < t.c ∧ 0 < t.d

/-- Sample size `N = a+b+c+d`. -/
def N (t : Tab) : ℝ := t.a + t.b + t.c + t.d

/-- Cellwise sum of two tables. -/
def add (s t : Tab) : Tab := ⟨s.a + t.a, s.b + t.b, s.c + t.c, s.d + t.d⟩

/-- Transpose `a' = [a, c; b, d]` (p. 698). -/
def tr (t : Tab) : Tab := ⟨t.a, t.c, t.b, t.d⟩

end Tab

variable {ι : Type*}

/-- Amalgamation: `A = [Σa_i, Σb_i; Σc_i, Σd_i]` (pp. 694-695). -/
def amalg (s : Finset ι) (t : ι → Tab) : Tab :=
  ⟨∑ i ∈ s, (t i).a, ∑ i ∈ s, (t i).b, ∑ i ∈ s, (t i).c, ∑ i ∈ s, (t i).d⟩

/-- **Definition 1.1** (p. 695): the amalgamation paradox for the measure `α`.
Every subtable's measure lies strictly below the aggregate's, or every one
strictly above.  For nonempty `s` this is the paper's
`max_i α(a_i) < α(A) ∨ α(A) < min_i α(a_i)`; see `paradox_iff_max_min`. -/
def Paradox (α : Tab → ℝ) (s : Finset ι) (t : ι → Tab) : Prop :=
  (∀ i ∈ s, α (t i) < α (amalg s t)) ∨ (∀ i ∈ s, α (amalg s t) < α (t i))

/-- Definition 1.1 in the paper's own `max`/`min` wording. -/
theorem paradox_iff_max_min (α : Tab → ℝ) {s : Finset ι} (hs : s.Nonempty)
    (t : ι → Tab) :
    Paradox α s t ↔
      s.sup' hs (fun i => α (t i)) < α (amalg s t) ∨
        α (amalg s t) < s.inf' hs (fun i => α (t i)) := by
  rw [Paradox, Finset.sup'_lt_iff, Finset.lt_inf'_iff]

/-- **Definition 2.1** (p. 696), row-uniform (row-fair) design:
`(a_i+b_i)/(c_i+d_i) = λ` for all `i`, cross-multiplied. -/
def RowUniform (s : Finset ι) (t : ι → Tab) (lam : ℝ) : Prop :=
  ∀ i ∈ s, (t i).a + (t i).b = lam * ((t i).c + (t i).d)

/-- **Definition 2.1**, (2.2) (p. 696), column-uniform (column-fair) design:
`(a_i+c_i)/(b_i+d_i) = μ` for all `i`, cross-multiplied. -/
def ColUniform (s : Finset ι) (t : ι → Tab) (μ : ℝ) : Prop :=
  ∀ i ∈ s, (t i).a + (t i).c = μ * ((t i).b + (t i).d)

/-! ## Measures of association (Section 3) -/

/-- Peirce's measure (3.1), p. 698: `π_R = a/(a+b) - c/(c+d)`. -/
noncomputable def piR (t : Tab) : ℝ := t.a / (t.a + t.b) - t.c / (t.c + t.d)

/-- Peirce's column measure (3.3), p. 699: `π_C = a/(a+c) - b/(b+d)`. -/
noncomputable def piC (t : Tab) : ℝ := t.a / (t.a + t.c) - t.b / (t.b + t.d)

/-- Yule's measure (3.5), p. 699: `y = (ad - bc)/N²`. -/
noncomputable def yule (t : Tab) : ℝ := (t.a * t.d - t.b * t.c) / t.N ^ 2

/-- The odds ratio (3.6), p. 699: `κ = ad/(bc)`. -/
noncomputable def kappa (t : Tab) : ℝ := t.a * t.d / (t.b * t.c)

/-- `exp W_R = a(c+d)/(c(a+b))`, (3.10) p. 700. -/
noncomputable def expWR (t : Tab) : ℝ := t.a * (t.c + t.d) / (t.c * (t.a + t.b))

/-- `exp Q_R = d(a+b)/(b(c+d))`, (3.12) p. 700. -/
noncomputable def expQR (t : Tab) : ℝ := t.d * (t.a + t.b) / (t.b * (t.c + t.d))

/-- Weight of evidence `W_R` (3.10). -/
noncomputable def WR (t : Tab) : ℝ := Real.log (expWR t)

/-- Causal propensity `Q_R` (3.12). -/
noncomputable def QR (t : Tab) : ℝ := Real.log (expQR t)

/-! ## Lemma 4.1 and the generic "weighted average ⇒ no paradox" step -/

/-- **Lemma 4.1** (p. 701), as used: for positive `t, u, v, w`, the mediant
`(t+u)/(v+w)` lies between `t/v` and `u/w`. -/
theorem mediant_between {t u v w : ℝ} (hv : 0 < v) (hw : 0 < w)
    (h : t / v ≤ u / w) : t / v ≤ (t + u) / (v + w) ∧ (t + u) / (v + w) ≤ u / w := by
  rw [div_le_div_iff₀ hv hw] at h
  constructor
  · rw [div_le_div_iff₀ hv (by linarith)]; nlinarith
  · rw [div_le_div_iff₀ (by linarith) hw]; nlinarith

/-- If `α(A)` is a positively weighted average of the `α(a_i)`, the paradox
cannot occur.  This is the step "(4.1) hence (4.2)" of Theorems 4.1-4.2. -/
theorem not_paradox_of_weighted (α : Tab → ℝ) {s : Finset ι} (hs : s.Nonempty)
    (t : ι → Tab) (w : ι → ℝ) (hw : ∀ i ∈ s, 0 < w i)
    (h : α (amalg s t) * ∑ i ∈ s, w i = ∑ i ∈ s, w i * α (t i)) :
    ¬ Paradox α s t := by
  rintro (hlt | hgt)
  · have : ∑ i ∈ s, w i * α (t i) < ∑ i ∈ s, w i * α (amalg s t) :=
      Finset.sum_lt_sum_of_nonempty hs fun i hi =>
        mul_lt_mul_of_pos_left (hlt i hi) (hw i hi)
    rw [← Finset.sum_mul, mul_comm] at this
    linarith
  · have : ∑ i ∈ s, w i * α (amalg s t) < ∑ i ∈ s, w i * α (t i) :=
      Finset.sum_lt_sum_of_nonempty hs fun i hi =>
        mul_lt_mul_of_pos_left (hgt i hi) (hw i hi)
    rw [← Finset.sum_mul, mul_comm] at this
    linarith

/-! ## The general structure of `π_R(A)` -/

/-- With no design assumption, `π_R(A)` is the treated-row success rates averaged
with weights `a_i+b_i`, minus the untreated-row success rates averaged with
weights `c_i+d_i` (cf. (5.16)-(5.17), p. 705).  The two rows weight the
subpopulations differently unless the design is row-uniform; that difference,
not the population shares `N_i/N`, is what drives the paradox for `π_R`. -/
theorem piR_amalg_general {s : Finset ι} (t : ι → Tab)
    (hpos : ∀ i ∈ s, (t i).Pos) :
    piR (amalg s t) =
      (∑ i ∈ s, ((t i).a + (t i).b) * ((t i).a / ((t i).a + (t i).b))) /
          ∑ i ∈ s, ((t i).a + (t i).b)
        - (∑ i ∈ s, ((t i).c + (t i).d) * ((t i).c / ((t i).c + (t i).d))) /
          ∑ i ∈ s, ((t i).c + (t i).d) := by
  have h1 : ∀ i ∈ s, ((t i).a + (t i).b) * ((t i).a / ((t i).a + (t i).b)) = (t i).a :=
    fun i hi => by
      obtain ⟨ha, hb, -, -⟩ := hpos i hi
      field_simp
  have h2 : ∀ i ∈ s, ((t i).c + (t i).d) * ((t i).c / ((t i).c + (t i).d)) = (t i).c :=
    fun i hi => by
      obtain ⟨-, -, hc, hd⟩ := hpos i hi
      field_simp
  rw [Finset.sum_congr rfl h1, Finset.sum_congr rfl h2, piR, amalg]
  simp only [Finset.sum_add_distrib]

/-! ## Theorem 4.1 and Corollary 4.1 -/

/-- One row-uniform table: `N · π_R = (λ+1)(a/λ - c)` (proof of Thm 4.1, p. 701). -/
theorem N_mul_piR_of_row (t : Tab) {lam : ℝ} (hlam : 0 < lam) (hcd : 0 < t.c + t.d)
    (hrow : t.a + t.b = lam * (t.c + t.d)) :
    t.N * piR t = (lam + 1) * (t.a / lam - t.c) := by
  have hN : t.N = (lam + 1) * (t.c + t.d) := by rw [Tab.N]; linarith
  rw [hN, piR, hrow]
  field_simp

/-- The amalgam of a row-uniform family is row-uniform with the same `λ`. -/
theorem amalg_row {s : Finset ι} {t : ι → Tab} {lam : ℝ} (h : RowUniform s t lam) :
    (amalg s t).a + (amalg s t).b = lam * ((amalg s t).c + (amalg s t).d) := by
  simp only [amalg, ← Finset.sum_add_distrib, Finset.mul_sum]
  exact Finset.sum_congr rfl h

theorem amalg_cd_pos {s : Finset ι} (hs : s.Nonempty) {t : ι → Tab}
    (hpos : ∀ i ∈ s, (t i).Pos) : 0 < (amalg s t).c + (amalg s t).d := by
  simp only [amalg, ← Finset.sum_add_distrib]
  exact Finset.sum_pos (fun i hi => by obtain ⟨-, -, hc, hd⟩ := hpos i hi; linarith) hs

/-- **Theorem 4.1** (p. 701), (4.1) for `π_R`: under a row-uniform design
`N · π_R(A) = Σ N_i π_R(a_i)`, i.e. `π_R(A) = Σ (N_i/N) π_R(a_i)`. -/
theorem piR_weighted_rowUniform {s : Finset ι} (hs : s.Nonempty) (t : ι → Tab)
    (hpos : ∀ i ∈ s, (t i).Pos) {lam : ℝ} (hlam : 0 < lam) (hrow : RowUniform s t lam) :
    (amalg s t).N * piR (amalg s t) = ∑ i ∈ s, (t i).N * piR (t i) := by
  have hi : ∀ i ∈ s, (t i).N * piR (t i) = (lam + 1) * ((t i).a / lam - (t i).c) :=
    fun i hi => by
      obtain ⟨-, -, hc, hd⟩ := hpos i hi
      exact N_mul_piR_of_row (t i) hlam (by linarith) (hrow i hi)
  rw [Finset.sum_congr rfl hi, N_mul_piR_of_row _ hlam (amalg_cd_pos hs hpos) (amalg_row hrow),
    ← Finset.mul_sum, Finset.sum_sub_distrib, ← Finset.sum_div]
  rfl

/-- On a row-uniform table, `y = π_R · λ/(λ+1)²` (proof of Thm 4.1, p. 702). -/
theorem yule_eq_of_row (t : Tab) (hp : t.Pos) {lam : ℝ} (hlam : 0 < lam)
    (hrow : t.a + t.b = lam * (t.c + t.d)) :
    yule t = piR t * (lam / (lam + 1) ^ 2) := by
  obtain ⟨ha, hb, hc, hd⟩ := hp
  have hN : t.N = (lam + 1) * (t.c + t.d) := by rw [Tab.N]; linarith
  have hab : t.a + t.b ≠ 0 := by linarith
  have hb' : t.b = lam * (t.c + t.d) - t.a := by linarith
  rw [yule, piR, hN, hrow, hb']
  field_simp
  ring

theorem amalg_pos {s : Finset ι} (hs : s.Nonempty) {t : ι → Tab}
    (hpos : ∀ i ∈ s, (t i).Pos) : (amalg s t).Pos :=
  ⟨Finset.sum_pos (fun i hi => (hpos i hi).1) hs,
   Finset.sum_pos (fun i hi => (hpos i hi).2.1) hs,
   Finset.sum_pos (fun i hi => (hpos i hi).2.2.1) hs,
   Finset.sum_pos (fun i hi => (hpos i hi).2.2.2) hs⟩

/-- **Theorem 4.1** (p. 701), (4.1) for Yule's `y`. -/
theorem yule_weighted_rowUniform {s : Finset ι} (hs : s.Nonempty) (t : ι → Tab)
    (hpos : ∀ i ∈ s, (t i).Pos) {lam : ℝ} (hlam : 0 < lam) (hrow : RowUniform s t lam) :
    (amalg s t).N * yule (amalg s t) = ∑ i ∈ s, (t i).N * yule (t i) := by
  have hi : ∀ i ∈ s, (t i).N * yule (t i) =
      ((t i).N * piR (t i)) * (lam / (lam + 1) ^ 2) := fun i hi => by
    rw [yule_eq_of_row _ (hpos i hi) hlam (hrow i hi)]; ring
  rw [Finset.sum_congr rfl hi, ← Finset.sum_mul, ← piR_weighted_rowUniform hs t hpos hlam hrow,
    yule_eq_of_row _ (amalg_pos hs hpos) hlam (amalg_row hrow)]
  ring

theorem N_pos {t : Tab} (h : t.Pos) : 0 < t.N := by
  obtain ⟨ha, hb, hc, hd⟩ := h; rw [Tab.N]; linarith

theorem amalg_N {s : Finset ι} (t : ι → Tab) : (amalg s t).N = ∑ i ∈ s, (t i).N := by
  simp only [Tab.N, amalg, Finset.sum_add_distrib]

/-- **Theorem 4.1** (p. 701), (4.2): under a row-uniform design the paradox
cannot occur for `π_R`. -/
theorem no_paradox_piR {s : Finset ι} (hs : s.Nonempty) (t : ι → Tab)
    (hpos : ∀ i ∈ s, (t i).Pos) {lam : ℝ} (hlam : 0 < lam) (hrow : RowUniform s t lam) :
    ¬ Paradox piR s t :=
  not_paradox_of_weighted piR hs t (fun i => (t i).N) (fun i hi => N_pos (hpos i hi)) (by
    rw [← amalg_N, mul_comm]; exact piR_weighted_rowUniform hs t hpos hlam hrow)

/-- **Theorem 4.1** (p. 701), (4.2) for Yule's `y`. -/
theorem no_paradox_yule {s : Finset ι} (hs : s.Nonempty) (t : ι → Tab)
    (hpos : ∀ i ∈ s, (t i).Pos) {lam : ℝ} (hlam : 0 < lam) (hrow : RowUniform s t lam) :
    ¬ Paradox yule s t :=
  not_paradox_of_weighted yule hs t (fun i => (t i).N) (fun i hi => N_pos (hpos i hi)) (by
    rw [← amalg_N, mul_comm]; exact yule_weighted_rowUniform hs t hpos hlam hrow)

theorem piC_eq_piR_tr (t : Tab) : piC t = piR t.tr := rfl

theorem amalg_tr {s : Finset ι} (t : ι → Tab) :
    amalg s (fun i => (t i).tr) = (amalg s t).tr := rfl

/-- **Corollary 4.1** (p. 701): under a column-uniform design the paradox cannot
occur for `π_C` (transpose Theorem 4.1). -/
theorem no_paradox_piC {s : Finset ι} (hs : s.Nonempty) (t : ι → Tab)
    (hpos : ∀ i ∈ s, (t i).Pos) {μ : ℝ} (hμ : 0 < μ) (hcol : ColUniform s t μ) :
    ¬ Paradox piC s t := by
  have hpos' : ∀ i ∈ s, ((fun i => (t i).tr) i).Pos := fun i hi => by
    obtain ⟨ha, hb, hc, hd⟩ := hpos i hi; exact ⟨ha, hc, hb, hd⟩
  have h := no_paradox_piR hs (fun i => (t i).tr) hpos' hμ hcol
  simpa [Paradox, amalg_tr, piC_eq_piR_tr] using h

/-! ## Theorem 4.2 -/

/-- **Theorem 4.2** (p. 702), (4.4) for `Q_R`: under a row-uniform design,
`exp Q_R(A) · Σb_i = Σ b_i exp Q_R(a_i)`, i.e. `δ_i = b_i/Σb`. -/
theorem expQR_weighted_rowUniform {s : Finset ι} (hs : s.Nonempty) (t : ι → Tab)
    (hpos : ∀ i ∈ s, (t i).Pos) {lam : ℝ} (hrow : RowUniform s t lam) :
    expQR (amalg s t) * ∑ i ∈ s, (t i).b = ∑ i ∈ s, (t i).b * expQR (t i) := by
  have key : ∀ u : Tab, 0 < u.b → 0 < u.c + u.d → u.a + u.b = lam * (u.c + u.d) →
      expQR u = lam * u.d / u.b := fun u hb hcd h => by
    rw [expQR, h]; field_simp
  have hi : ∀ i ∈ s, (t i).b * expQR (t i) = lam * (t i).d := fun i hi => by
    obtain ⟨-, hb, hc, hd⟩ := hpos i hi
    rw [key _ hb (by linarith) (hrow i hi)]; field_simp
  obtain ⟨-, hB, -, -⟩ := amalg_pos hs hpos
  rw [Finset.sum_congr rfl hi, key _ hB (amalg_cd_pos hs hpos) (amalg_row hrow),
    ← Finset.mul_sum]
  change lam * (amalg s t).d / (amalg s t).b * (amalg s t).b = _
  field_simp
  rfl

/-- **Theorem 4.2** (p. 702), (4.4) for `W_R`: `δ_i = c_i/Σc`. -/
theorem expWR_weighted_rowUniform {s : Finset ι} (hs : s.Nonempty) (t : ι → Tab)
    (hpos : ∀ i ∈ s, (t i).Pos) {lam : ℝ} (hlam : 0 < lam) (hrow : RowUniform s t lam) :
    expWR (amalg s t) * ∑ i ∈ s, (t i).c = ∑ i ∈ s, (t i).c * expWR (t i) := by
  have key : ∀ u : Tab, 0 < u.c → 0 < u.c + u.d → u.a + u.b = lam * (u.c + u.d) →
      expWR u = u.a / (lam * u.c) := fun u hc hcd h => by
    rw [expWR, h]; field_simp
  have hi : ∀ i ∈ s, (t i).c * expWR (t i) = (t i).a / lam := fun i hi => by
    obtain ⟨-, -, hc, hd⟩ := hpos i hi
    rw [key _ hc (by linarith) (hrow i hi)]; field_simp
  obtain ⟨-, -, hC, -⟩ := amalg_pos hs hpos
  rw [Finset.sum_congr rfl hi, key _ hC (amalg_cd_pos hs hpos) (amalg_row hrow),
    ← Finset.sum_div]
  change (amalg s t).a / (lam * (amalg s t).c) * (amalg s t).c = _
  field_simp
  rfl

/-- Taking logs preserves "no paradox" when all measures are positive
(`log` is strictly increasing on `(0,∞)`). -/
theorem not_paradox_log (f : Tab → ℝ) {s : Finset ι} (t : ι → Tab)
    (hf : ∀ i ∈ s, 0 < f (t i)) (hA : 0 < f (amalg s t)) (h : ¬ Paradox f s t) :
    ¬ Paradox (fun u => Real.log (f u)) s t := by
  rintro (hlt | hgt)
  · exact h (Or.inl fun i hi => (Real.log_lt_log_iff (hf i hi) hA).1 (hlt i hi))
  · exact h (Or.inr fun i hi => (Real.log_lt_log_iff hA (hf i hi)).1 (hgt i hi))

theorem expQR_pos {u : Tab} (h : u.Pos) : 0 < expQR u := by
  obtain ⟨ha, hb, hc, hd⟩ := h; unfold expQR; positivity

theorem expWR_pos {u : Tab} (h : u.Pos) : 0 < expWR u := by
  obtain ⟨ha, hb, hc, hd⟩ := h; unfold expWR; positivity

/-- **Theorem 4.2** (p. 702): under a row-uniform design the paradox cannot occur
for `Q_R`. -/
theorem no_paradox_QR {s : Finset ι} (hs : s.Nonempty) (t : ι → Tab)
    (hpos : ∀ i ∈ s, (t i).Pos) {lam : ℝ} (hrow : RowUniform s t lam) :
    ¬ Paradox QR s t :=
  not_paradox_log expQR t (fun i hi => expQR_pos (hpos i hi)) (expQR_pos (amalg_pos hs hpos))
    (not_paradox_of_weighted expQR hs t (fun i => (t i).b) (fun i hi => (hpos i hi).2.1)
      (expQR_weighted_rowUniform hs t hpos hrow))

/-- **Theorem 4.2** (p. 702): under a row-uniform design the paradox cannot occur
for `W_R`. -/
theorem no_paradox_WR {s : Finset ι} (hs : s.Nonempty) (t : ι → Tab)
    (hpos : ∀ i ∈ s, (t i).Pos) {lam : ℝ} (hlam : 0 < lam) (hrow : RowUniform s t lam) :
    ¬ Paradox WR s t :=
  not_paradox_log expWR t (fun i hi => expWR_pos (hpos i hi)) (expWR_pos (amalg_pos hs hpos))
    (not_paradox_of_weighted expWR hs t (fun i => (t i).c) (fun i hi => (hpos i hi).2.2.1)
      (expWR_weighted_rowUniform hs t hpos hlam hrow))

/-! ## Theorem 4.3 (two subpopulations) -/

/-- Orientation lemma: if `a₁/c₁ ≤ a₂/c₂` and `d₁/b₁ ≤ d₂/b₂`, then
`κ(a₁) ≤ κ(A) ≤ κ(a₂)` (last step of the proof of Thm 4.3, p. 703, via Lemma 4.1). -/
theorem kappa_between_of_ratios {t₁ t₂ : Tab} (h₁ : t₁.Pos) (h₂ : t₂.Pos)
    (hac : t₁.a / t₁.c ≤ t₂.a / t₂.c) (hdb : t₁.d / t₁.b ≤ t₂.d / t₂.b) :
    kappa t₁ ≤ kappa (t₁.add t₂) ∧ kappa (t₁.add t₂) ≤ kappa t₂ := by
  obtain ⟨a1, b1, c1, d1⟩ := h₁
  obtain ⟨a2, b2, c2, d2⟩ := h₂
  obtain ⟨m1, m2⟩ := mediant_between c1 c2 hac
  obtain ⟨n1, n2⟩ := mediant_between b1 b2 hdb
  have e : ∀ u : Tab, 0 < u.b → 0 < u.c → kappa u = (u.a / u.c) * (u.d / u.b) :=
    fun u hb hc => by rw [kappa]; field_simp
  have hA : kappa (t₁.add t₂) = ((t₁.a + t₂.a) / (t₁.c + t₂.c)) * ((t₁.d + t₂.d) / (t₁.b + t₂.b)) :=
    e (t₁.add t₂) (by simp [Tab.add]; linarith) (by simp [Tab.add]; linarith)
  rw [hA, e t₁ b1 c1, e t₂ b2 c2]
  constructor
  · exact mul_le_mul m1 n1 (by positivity) (by positivity)
  · exact mul_le_mul m2 n2 (by positivity) (by positivity)

/-- **Theorem 4.3** (pp. 702-703), `n = 2`: if the design is both row-uniform and
column-uniform, then `κ(A)` lies in the closed interval between `κ(a₁)` and
`κ(a₂)` — the paradox cannot occur for the odds ratio.  Uniformity is stated as
the cross-multiplied equalities of the two row ratios and the two column ratios. -/
theorem kappa_between_of_uniform {t₁ t₂ : Tab} (h₁ : t₁.Pos) (h₂ : t₂.Pos)
    (hrow : (t₁.a + t₁.b) * (t₂.c + t₂.d) = (t₂.a + t₂.b) * (t₁.c + t₁.d))
    (hcol : (t₁.a + t₁.c) * (t₂.b + t₂.d) = (t₂.a + t₂.c) * (t₁.b + t₁.d)) :
    (kappa t₁ ≤ kappa (t₁.add t₂) ∧ kappa (t₁.add t₂) ≤ kappa t₂) ∨
      (kappa t₂ ≤ kappa (t₁.add t₂) ∧ kappa (t₁.add t₂) ≤ kappa t₁) := by
  obtain ⟨a1, b1, c1, d1⟩ := h₁
  obtain ⟨a2, b2, c2, d2⟩ := h₂
  -- Row and column uniformity scale every row and column total by N₁/N₂.
  set N₁ := t₁.a + t₁.b + t₁.c + t₁.d
  set N₂ := t₂.a + t₂.b + t₂.c + t₂.d
  have r : (t₁.a + t₁.b) * N₂ = (t₂.a + t₂.b) * N₁ := by simp only [N₁, N₂]; nlinarith
  have k : (t₁.a + t₁.c) * N₂ = (t₂.a + t₂.c) * N₁ := by simp only [N₁, N₂]; nlinarith
  -- T = a₁N₂ - a₂N₁; then b, c get -T and d gets +T.
  set T := t₁.a * N₂ - t₂.a * N₁ with hT
  have hN₂ : 0 < N₂ := by simp only [N₂]; linarith
  have hac : t₁.a * t₂.c - t₂.a * t₁.c = T * (t₂.a + t₂.c) / N₂ := by
    field_simp; simp only [hT, N₁, N₂] at r k ⊢; nlinarith
  have hdb : t₁.d * t₂.b - t₂.d * t₁.b = T * (t₂.b + t₂.d) / N₂ := by
    field_simp; simp only [hT, N₁, N₂] at r k ⊢; nlinarith
  rcases le_total T 0 with hT0 | hT0
  · left
    apply kappa_between_of_ratios ⟨a1, b1, c1, d1⟩ ⟨a2, b2, c2, d2⟩
    · rw [div_le_div_iff₀ c1 c2]
      have : T * (t₂.a + t₂.c) / N₂ ≤ 0 :=
        div_nonpos_of_nonpos_of_nonneg (mul_nonpos_of_nonpos_of_nonneg hT0 (by linarith)) hN₂.le
      linarith
    · rw [div_le_div_iff₀ b1 b2]
      have : T * (t₂.b + t₂.d) / N₂ ≤ 0 :=
        div_nonpos_of_nonpos_of_nonneg (mul_nonpos_of_nonpos_of_nonneg hT0 (by linarith)) hN₂.le
      linarith
  · right
    have hsw : t₂.add t₁ = t₁.add t₂ := by
      simp only [Tab.add, Tab.mk.injEq]; refine ⟨?_, ?_, ?_, ?_⟩ <;> ring
    rw [← hsw]
    apply kappa_between_of_ratios ⟨a2, b2, c2, d2⟩ ⟨a1, b1, c1, d1⟩
    · rw [div_le_div_iff₀ c2 c1]
      have : 0 ≤ T * (t₂.a + t₂.c) / N₂ := by positivity
      linarith
    · rw [div_le_div_iff₀ b2 b1]
      have : 0 ≤ T * (t₂.b + t₂.d) / N₂ := by positivity
      linarith

/-! ## The paper's Note on p. 702, and two illustrations of Definition 1.1 -/

/-- The two tables of the Note on p. 702: `a₁ = [3,1;1,9]`, `a₂ = [889,203;381,2349]`. -/
def noteTabs : Fin 2 → Tab
  | 0 => ⟨3, 1, 1, 9⟩
  | 1 => ⟨889, 203, 381, 2349⟩

/-- **Note** (p. 702): the design is row-uniform with `λ = 0.4`, `κ(a₁) = κ(a₂) = 27`,
yet `κ(A) = 2103336/77928 ≈ 26.991 < 27` — the amalgamation paradox occurs for the
odds ratio.  Row-uniformity alone does not suffice for Theorem 4.3. -/
theorem note_p702_kappa_paradox :
    RowUniform Finset.univ noteTabs (2 / 5) ∧
      kappa (noteTabs 0) = 27 ∧ kappa (noteTabs 1) = 27 ∧
      kappa (amalg Finset.univ noteTabs) = 2103336 / 77928 ∧
      Paradox kappa Finset.univ noteTabs := by
  refine ⟨?_, ?_, ?_, ?_, ?_⟩
  · intro i _; fin_cases i <;> simp [noteTabs] <;> norm_num
  · simp [kappa, noteTabs]; norm_num
  · simp [kappa, noteTabs]; norm_num
  · simp [kappa, amalg, noteTabs, Fin.sum_univ_two]; norm_num
  · right; intro i _
    fin_cases i <;> simp [kappa, amalg, noteTabs, Fin.sum_univ_two] <;> norm_num

/-- Two subpopulations of equal size (`N₁ = N₂ = 100`): `a₁ = [8,2;60,30]`,
`a₂ = [20,70;1,9]`. -/
def equalSizeTabs : Fin 2 → Tab
  | 0 => ⟨8, 2, 60, 30⟩
  | 1 => ⟨20, 70, 1, 9⟩

/-- The paper's remark (p. 696) that a drug "can be judged to be beneficial ... for
both men and women considered separately, but can seem to be harmful for the
population at large ... This can happen even though `N_i ∝ p_i`": here
`N₁ = N₂` (equal population shares), `π_R(a₁) = 2/15 > 0`, `π_R(a₂) = 11/90 > 0`,
but `π_R(A) = -33/100 < 0`. -/
theorem equalSize_reversal :
    (equalSizeTabs 0).N = 100 ∧ (equalSizeTabs 1).N = 100 ∧
      piR (equalSizeTabs 0) = 2 / 15 ∧ piR (equalSizeTabs 1) = 11 / 90 ∧
      piR (amalg Finset.univ equalSizeTabs) = -33 / 100 ∧
      Paradox piR Finset.univ equalSizeTabs := by
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_⟩
  · simp [Tab.N, equalSizeTabs]; norm_num
  · simp [Tab.N, equalSizeTabs]; norm_num
  · simp [piR, equalSizeTabs]; norm_num
  · simp [piR, equalSizeTabs]; norm_num
  · simp [piR, amalg, equalSizeTabs, Fin.sum_univ_two]; norm_num
  · right; intro i _
    fin_cases i <;> simp [piR, amalg, equalSizeTabs, Fin.sum_univ_two] <;> norm_num

/-- The equal-size example is not row-uniform (row ratios `1/9` and `9`), which is
what Theorem 4.1 says it must fail to be. -/
theorem equalSize_not_rowUniform : ∀ lam : ℝ, ¬ RowUniform Finset.univ equalSizeTabs lam := by
  intro lam h
  have h0 := h 0 (Finset.mem_univ _)
  have h1 := h 1 (Finset.mem_univ _)
  simp [equalSizeTabs] at h0 h1
  norm_num at h0 h1
  linarith

/-- Yule's case (p. 695): `a₁ = [9,9;9,9]`, `a₂ = [4,8;8,16]` (equal sizes `36`). -/
def yuleTabs : Fin 2 → Tab
  | 0 => ⟨9, 9, 9, 9⟩
  | 1 => ⟨4, 8, 8, 16⟩

/-- **Yule's case of Definition 1.1** (p. 695): no association in either
subpopulation (`π_R = 0`, `κ = 1`), association in the aggregate
(`π_R(A) = 1/35`, `κ(A) = 325/289`).  An effect *appears*; nothing is erased or
reversed. -/
theorem yule_case_paradox :
    piR (yuleTabs 0) = 0 ∧ piR (yuleTabs 1) = 0 ∧
      kappa (yuleTabs 0) = 1 ∧ kappa (yuleTabs 1) = 1 ∧
      piR (amalg Finset.univ yuleTabs) = 1 / 35 ∧
      kappa (amalg Finset.univ yuleTabs) = 325 / 289 ∧
      Paradox piR Finset.univ yuleTabs ∧ Paradox kappa Finset.univ yuleTabs := by
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · simp [piR, yuleTabs]
  · simp [piR, yuleTabs]; norm_num
  · simp [kappa, yuleTabs]
  · simp [kappa, yuleTabs]; norm_num
  · simp [piR, amalg, yuleTabs, Fin.sum_univ_two]; norm_num
  · simp [kappa, amalg, yuleTabs, Fin.sum_univ_two]; norm_num
  · left; intro i _
    fin_cases i <;> simp [piR, amalg, yuleTabs, Fin.sum_univ_two] <;> norm_num
  · left; intro i _
    fin_cases i <;> simp [kappa, amalg, yuleTabs, Fin.sum_univ_two] <;> norm_num

/-! ## Theorem 5.1 -/

/-- **Theorem 5.1** (pp. 703-704): two subpopulations are homogeneous for the odds
ratio, `κ(a₁) = κ(a₂) = κ(A)`, iff (5.2) `a₁/c₁ = a₂/c₂` and `b₁/d₁ = b₂/d₂`, or
(5.3) `a₁/b₁ = a₂/b₂` and `c₁/d₁ = c₂/d₂` (all cross-multiplied).  The proof is
the paper's factorisation `(b₁d₂ - b₂d₁)(c₁d₂ - c₂d₁) = 0`. -/
theorem kappa_homogeneous_iff {t₁ t₂ : Tab} (h₁ : t₁.Pos) (h₂ : t₂.Pos) :
    (kappa t₁ = kappa t₂ ∧ kappa t₂ = kappa (t₁.add t₂)) ↔
      (t₁.a * t₂.c = t₂.a * t₁.c ∧ t₁.b * t₂.d = t₂.b * t₁.d) ∨
        (t₁.a * t₂.b = t₂.a * t₁.b ∧ t₁.c * t₂.d = t₂.c * t₁.d) := by
  obtain ⟨a1, b1, c1, d1⟩ := h₁
  obtain ⟨a2, b2, c2, d2⟩ := h₂
  obtain ⟨A1, B1, C1, D1⟩ := t₁
  obtain ⟨A2, B2, C2, D2⟩ := t₂
  simp only [kappa, Tab.add] at *
  rw [div_eq_div_iff (by positivity) (by positivity),
    div_eq_div_iff (by positivity) (by positivity)]
  constructor
  · rintro ⟨e1, e2⟩
    -- a₁ = b₁c₁k/d₁, a₂ = b₂c₂k/d₂ with k = κ; the factorisation of (5.4).
    have key : (B1 * D2 - B2 * D1) * (C1 * D2 - C2 * D1) * A2 = 0 := by
      linear_combination (-D1) * e2 - (D1 + D2) * e1
    rcases mul_eq_zero.1 key with h | h
    · rcases mul_eq_zero.1 h with h | h
      · -- b₁/d₁ = b₂/d₂, then a₁/c₁ = a₂/c₂ from κ₁ = κ₂
        left
        have hb : B1 * D2 = B2 * D1 := by linarith
        refine ⟨?_, hb⟩
        have : (A1 * C2 - A2 * C1) * (B2 * D1) = 0 := by
          linear_combination e1 + A2 * C1 * hb
        rcases mul_eq_zero.1 this with h' | h'
        · linarith
        · exact absurd h' (by positivity)
      · -- c₁/d₁ = c₂/d₂, then a₁/b₁ = a₂/b₂
        right
        have hc : C1 * D2 = C2 * D1 := by linarith
        refine ⟨?_, hc⟩
        have : (A1 * B2 - A2 * B1) * (C2 * D1) = 0 := by
          linear_combination e1 + A2 * B1 * hc
        rcases mul_eq_zero.1 this with h' | h'
        · linarith
        · exact absurd h' (by positivity)
    · exact absurd h (by positivity)
  · rintro (⟨h1, h2⟩ | ⟨h1, h2⟩)
    · exact ⟨by linear_combination D1 * B2 * h1 - A2 * C1 * h2,
        by linear_combination (A2 * C2 + A2 * C1) * h2 - (B2 * D2 + D1 * B2) * h1⟩
    · exact ⟨by linear_combination D1 * C2 * h1 - A2 * B1 * h2,
        by linear_combination (A2 * B2 + A2 * B1) * h2 - (C2 * D2 + D1 * C2) * h1⟩

end Literature.GoodMittal
