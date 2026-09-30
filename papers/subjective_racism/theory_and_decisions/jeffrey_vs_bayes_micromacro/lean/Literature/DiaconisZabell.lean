/-
# Diaconis & Zabell (1982), "Updating Subjective Probability"

*Journal of the American Statistical Association* 77(380), 822-830.

Everything is on a finite space `Ω` with a prior `P` of full support (D-Z work on a
countable `Ω`, and in Section 6 on an abstract space). A partition is given by a
labelling `e : Ω → ι`, with cells `Eᵢ = {e = i}`. Jeffrey's rule is
`jeffrey P e p ω = pᵢ P(ω) / P(Eᵢ)` for `ω ∈ Eᵢ`, D-Z (1.1)/(2.2).

## What is proved

**Jeffrey's rule and condition (J)** (pp. 822-824).
* `marg_jeffrey` — the update attains the target: `P*(Eᵢ) = pᵢ`.
* `jeffrey_jcond` — rigidity: `P*(A | Eᵢ) = P(A | Eᵢ)` for every `A` and `i`, i.e. (J).
* `jeffrey_mass` — (1.1): `P*(A) = Σᵢ P(A | Eᵢ) P*(Eᵢ)`.
* `jcond_iff_jeffrey`, `eq_jeffrey_of_jcond` — (J) holds iff `P*` is the Jeffrey
  update of `P` to `P*`'s own cell probabilities. So *given (J)*, the new cell
  probabilities determine the revision. That uniqueness follows from (J) itself;
  D-Z give no coherence argument for it.

**Theorem 2.1** (p. 824). `thm21`: `P*` can be obtained from `P` by conditioning
(on a finite enlarged space) iff `P* ≤ B P` for some `B ≥ 1`, eq. (2.1).
`thm21_finite`: on a finite space with full-support `P`, always.

**Theorem 2.2 and sufficiency** (p. 824). D-Z: finding a partition with (J) "is simply
the problem of finding a *sufficient* partition for the two-element family `{P, P*}`".
* `thm22_factorization` — first statement, eq. (2.2): under (J) and common support,
  `P*(ω) = P*(Eᵢ)/P(Eᵢ) · P(ω)` on `Eᵢ`.
* `jcond_iff_ratio_const` — (J) iff `P*/P` is constant on each cell.
* `thm22_lr_sufficient`, `thm22_lr_minimal` — second statement: the partition into
  level sets of the likelihood ratio `P*/P` satisfies (J), and every partition that
  satisfies (J) refines it. So it is **the minimal sufficient partition, and it is
  defined relative to the one pair `{P, P*}`**.
* `jcond_of_refines`, `jcond_atoms`, `jcond_trivial_self` — sufficient partitions are
  not unique: every refinement of one is one, the atoms always are, and when
  `P* = P` the one-cell partition is. `lr_jeffrey_iff` — the cue's own partition is the
  minimal one exactly when the ratios `pᵢ/P(Eᵢ)` differ across cells (for a binary cue:
  `p ≠ P(E)`).

**Sections 5-6: minimum distance ("mechanical updating").** D-Z's divergences put the
candidate `Q` first and the prior `P` second: `I(Q, P) = Σ Q log (Q/P)` (5.3), i.e.
`KL(Q ‖ P)`, and `I_f(Q, P) = Σ P f(Q/P)` (p. 829). `C` = all `Q ≥ 0` with
`Q(Eᵢ) = pᵢ` (`Feasible`).
* `thm61_le`, `thm61_unique` — **Theorem 6.1** for a partition of a finite space: for
  every convex `f`, Jeffrey's `P*` minimizes `I_f(·, P)` over `C`, with value
  `Σᵢ P(Eᵢ) f(pᵢ/P(Eᵢ))` (`fdiv_jeffrey`); for strictly convex `f` it is the unique
  minimizer.
* `thm51_KL_le`, `thm51_KL_eq_iff` — **Theorem 5.1 (5.6)**:
  `KL(Q ‖ P) ≥ Σᵢ pᵢ log (pᵢ/P(Eᵢ))`, with equality iff `Q = P*`.
* `thm51_hellinger_le`, `thm51_hellinger_eq_iff` — **(5.5)**:
  `H(Q, P) ≥ Σᵢ (√pᵢ - √P(Eᵢ))²`, with equality iff `Q = P*`.
* `thm51_tv_le`, `tv_jeffrey` — **(5.4)**: the variation distance is at least
  `(1/2) Σᵢ |pᵢ - P(Eᵢ)|`, and `P*` attains it; `remark_a_tv_not_unique` —
  **Remark (a)**: it does not do so uniquely (explicit 3-point example).
* `example51` — **Example 5.1** (§5.3): the I-projection `Pᴵ` of the uniform `2 × 2`
  table onto margins `(1/3, 2/3)` has variation distance `7/36`, the minimum is `1/6`,
  attained at `Pⱽ`.

**Section 3: successive updating.** `PEF` = update on `𝓔` then on `𝓕`; `PFE` the
other order; `JIndep` = Jeffrey independence (p. 825); `PIndep` = (3.4).
* `thm32` — **Theorem 3.2**: `P_𝓔𝓕 = P_𝓕𝓔` iff `𝓔, 𝓕` are Jeffrey independent, for
  finite partitions of any size, by D-Z's direct algebra (3.5)-(3.6), **under the
  added hypothesis that every `Eᵢ ∩ Fⱼ` is nonempty**.
  `thm32_needs_qualitative_independence` — without it the forward direction is false
  (`𝓔 = 𝓕`, `p = q ≠ P(E)`). D-Z state the theorem with no such hypothesis.
* `thm33_forward` — Theorem 3.3, (3.7): P-independence implies J-independence for all
  targets.
* On the `2 × 2` table `P2 α β c` (margins `α, β`, covariance `c`): `marg_jE_snd` —
  updating `E` to `p` moves `P(F)` by `(p-α)c/(α(1-α))`; `remark826` — the Remark on
  p. 826: J-independence iff `(p = α ∨ c = 0) ∧ (q = β ∨ c = 0)`; `thm32_2x2`,
  `commute_of_indep` (`c = 0`: the orders agree for all targets),
  `not_commute_of_corr` (`c ≠ 0`, `p ≠ α`: they differ).
* `example32` (Ex. 3.2 numbers `.56, .24, .14, .06`), `example33` (Ex. 3.3:
  J-independence without P-independence), `example34` (Ex. 3.4:
  `P_𝓔𝓕(E) = 1/2`, `P_𝓕𝓔(F) = 371/851 ≠ 7/15`), `witness_gap` (the project's own
  witness `-1032/369935`).

## Not formalized

Theorem 3.1 (proved by D-Z through Csiszár's I-projection theorem), Theorem 3.3's
converse, Section 4 (Theorem 4.1, via Strassen), Theorem 5.2 and the IPFP, and
Section 6 on general measure spaces. The prose Remarks 1-3 of Section 4 (p. 827) and
the "J condition … Mathematics has nothing to offer here" of §3.2 are recorded in the
README, not here.
-/
import Mathlib

set_option linter.unusedSectionVars false

namespace Literature.DiaconisZabell

open Finset Real

noncomputable section

variable {Ω ι κ : Type*} [Fintype Ω]

/-! ## Setting: a finite space, a partition, Jeffrey's rule -/

section Basic

variable [DecidableEq Ω] [DecidableEq ι]

/-- The cell `Eᵢ = {ω : e ω = i}` of the partition given by the labelling `e`. -/
def cellSet (e : Ω → ι) (i : ι) : Finset Ω := univ.filter (fun ω => e ω = i)

/-- `P(A) = Σ_{ω ∈ A} P(ω)`. -/
def mass (P : Ω → ℝ) (A : Finset Ω) : ℝ := ∑ ω ∈ A, P ω

/-- `P(Eᵢ)`. -/
def marg (P : Ω → ℝ) (e : Ω → ι) (i : ι) : ℝ := mass P (cellSet e i)

/-- `P(A | Eᵢ) = P(A ∩ Eᵢ)/P(Eᵢ)`. -/
def cond (P : Ω → ℝ) (A : Finset Ω) (e : Ω → ι) (i : ι) : ℝ :=
  mass P (A ∩ cellSet e i) / marg P e i

/-- **Jeffrey's rule**, D-Z (1.1) in the pointwise form (2.2):
`P*(ω) = P*(Eᵢ) P(ω) / P(Eᵢ)` for `ω ∈ Eᵢ`, with `P*(Eᵢ) = pᵢ`. -/
def jeffrey (P : Ω → ℝ) (e : Ω → ι) (p : ι → ℝ) : Ω → ℝ :=
  fun ω => p (e ω) * P ω / marg P e (e ω)

/-- **Condition (J)**, D-Z p. 823: `P*(A | Eᵢ) = P(A | Eᵢ)` for all `A` and `i`.
D-Z p. 824: this "is simply the problem of finding a *sufficient* partition for
the two-element family `{P, P*}`". -/
def JCond (P Ps : Ω → ℝ) (e : Ω → ι) : Prop :=
  ∀ (A : Finset Ω) (i : ι), cond Ps A e i = cond P A e i

@[simp] theorem mem_cellSet {e : Ω → ι} {i : ι} {ω : Ω} : ω ∈ cellSet e i ↔ e ω = i := by
  simp [cellSet]

theorem marg_pos {P : Ω → ℝ} (hP : ∀ ω, 0 < P ω) (e : Ω → ι) (ω : Ω) : 0 < marg P e (e ω) :=
  Finset.sum_pos (fun x _ => hP x) ⟨ω, by simp⟩

theorem marg_pos_of_surj {P : Ω → ℝ} (hP : ∀ ω, 0 < P ω) {e : Ω → ι}
    (he : Function.Surjective e) (i : ι) : 0 < marg P e i := by
  obtain ⟨ω, rfl⟩ := he i; exact marg_pos hP e ω

theorem marg_nonneg {P : Ω → ℝ} (hP : ∀ ω, 0 ≤ P ω) (e : Ω → ι) (i : ι) : 0 ≤ marg P e i :=
  Finset.sum_nonneg fun x _ => hP x

/-- On `A ∩ Eᵢ` Jeffrey's rule rescales `P` by `pᵢ / P(Eᵢ)`. -/
theorem mass_inter_cell_jeffrey (P : Ω → ℝ) (e : Ω → ι) (p : ι → ℝ) (A : Finset Ω) (i : ι) :
    mass (jeffrey P e p) (A ∩ cellSet e i) = p i / marg P e i * mass P (A ∩ cellSet e i) := by
  unfold mass
  rw [Finset.mul_sum]
  refine Finset.sum_congr rfl fun ω hω => ?_
  have : e ω = i := mem_cellSet.1 (Finset.mem_inter.1 hω).2
  simp only [jeffrey, this]; ring

theorem marg_jeffrey_eq (P : Ω → ℝ) (e : Ω → ι) (p : ι → ℝ) (i : ι) :
    marg (jeffrey P e p) e i = p i / marg P e i * marg P e i := by
  have := mass_inter_cell_jeffrey P e p univ i
  rwa [Finset.univ_inter] at this

/-- **Jeffrey's rule attains the target marginal**: `P*(Eᵢ) = pᵢ` for every cell. -/
theorem marg_jeffrey {P : Ω → ℝ} (hP : ∀ ω, 0 < P ω) {e : Ω → ι} (he : Function.Surjective e)
    (p : ι → ℝ) (i : ι) : marg (jeffrey P e p) e i = p i := by
  rw [marg_jeffrey_eq, div_mul_cancel₀ _ (marg_pos_of_surj hP he i).ne']

theorem exists_mem_of_marg_ne_zero {P : Ω → ℝ} {e : Ω → ι} {i : ι} (h : marg P e i ≠ 0) :
    ∃ ω, e ω = i := by
  obtain ⟨ω, hω, -⟩ := Finset.exists_ne_zero_of_sum_ne_zero h
  exact ⟨ω, mem_cellSet.1 hω⟩

/-- **Rigidity: Jeffrey's rule satisfies (J).** It keeps every conditional
probability given a cell, `P*(A | Eᵢ) = P(A | Eᵢ)`, provided the new probability
of each occupied cell is nonzero. -/
theorem jeffrey_jcond (P : Ω → ℝ) (e : Ω → ι) {p : ι → ℝ} (hp : ∀ ω, p (e ω) ≠ 0) :
    JCond P (jeffrey P e p) e := by
  intro A i
  unfold cond
  rw [mass_inter_cell_jeffrey, marg_jeffrey_eq]
  by_cases hm : marg P e i = 0
  · simp [hm]
  · obtain ⟨ω, rfl⟩ := exists_mem_of_marg_ne_zero hm
    rw [mul_div_mul_left _ _ (div_ne_zero (hp ω) hm)]

/-- `P(A) = Σᵢ P(A ∩ Eᵢ)`. -/
theorem mass_eq_sum_cells [Fintype ι] (P : Ω → ℝ) (e : Ω → ι) (A : Finset Ω) :
    mass P A = ∑ i, mass P (A ∩ cellSet e i) := by
  unfold mass
  rw [← Finset.sum_fiberwise A e P]
  refine Finset.sum_congr rfl fun i _ => ?_
  congr 1; ext ω; simp

/-- `Σ_ω P(ω) = Σᵢ P(Eᵢ)`. -/
theorem sum_eq_sum_marg [Fintype ι] (P : Ω → ℝ) (e : Ω → ι) :
    ∑ ω, P ω = ∑ i, marg P e i := by
  have := mass_eq_sum_cells P e univ
  simpa [mass, marg, Finset.univ_inter] using this

/-- **D-Z (1.1)**: `P*(A) = Σᵢ P(A | Eᵢ) P*(Eᵢ)`. -/
theorem jeffrey_mass [Fintype ι] (P : Ω → ℝ) (e : Ω → ι) (p : ι → ℝ) (A : Finset Ω) :
    mass (jeffrey P e p) A = ∑ i, cond P A e i * p i := by
  rw [mass_eq_sum_cells _ e]
  refine Finset.sum_congr rfl fun i _ => ?_
  rw [mass_inter_cell_jeffrey, cond]; ring

/-- Jeffrey's rule to the prior's own marginal returns the prior. -/
theorem jeffrey_self {P : Ω → ℝ} (hP : ∀ ω, 0 < P ω) (e : Ω → ι) :
    jeffrey P e (marg P e) = P := by
  funext ω; simp only [jeffrey]; field_simp [(marg_pos hP e ω).ne']

/-! ## Section 2.2: Jeffrey's rule and sufficiency (Theorem 2.2) -/

/-- **Theorem 2.2, first statement** (D-Z p. 824, a version of Fisher-Neyman
factorization). If `P, P*` have common support and the partition satisfies (J),
then `P*(ω) = P*(Eᵢ)/P(Eᵢ) · P(ω)` for `ω ∈ Eᵢ`, eq. (2.2). -/
theorem thm22_factorization {P Ps : Ω → ℝ} (hP : ∀ ω, 0 < P ω) (hPs : ∀ ω, 0 < Ps ω)
    {e : Ω → ι} (hJ : JCond P Ps e) (ω : Ω) :
    Ps ω = marg Ps e (e ω) / marg P e (e ω) * P ω := by
  have h := hJ {ω} (e ω)
  have hmem : ω ∈ cellSet e (e ω) := by simp
  simp only [cond, mass, Finset.singleton_inter_of_mem hmem, Finset.sum_singleton] at h
  have h1 := (marg_pos hP e ω).ne'
  have h2 := (marg_pos hPs e ω).ne'
  rw [div_eq_div_iff h2 h1] at h
  rw [div_mul_eq_mul_div, eq_div_iff h1]
  linear_combination h

/-- **(J) holds iff `P*` is Jeffrey's update of `P` to its own marginal.** So,
given (J), the revision is determined by the new probabilities of the cells: a
uniqueness that comes from (J) itself, not from any coherence argument. -/
theorem jcond_iff_jeffrey {P Ps : Ω → ℝ} (hP : ∀ ω, 0 < P ω) (hPs : ∀ ω, 0 < Ps ω)
    (e : Ω → ι) : JCond P Ps e ↔ Ps = jeffrey P e (marg Ps e) := by
  constructor
  · intro hJ; funext ω
    rw [thm22_factorization hP hPs hJ ω, jeffrey]; ring
  · intro h
    rw [h]
    exact jeffrey_jcond P e fun ω => (marg_pos hPs e ω).ne'

/-- Given (J) and the new cell probabilities `pᵢ`, the revision is Jeffrey's rule. -/
theorem eq_jeffrey_of_jcond {P Ps : Ω → ℝ} (hP : ∀ ω, 0 < P ω) (hPs : ∀ ω, 0 < Ps ω)
    {e : Ω → ι} {p : ι → ℝ} (hJ : JCond P Ps e) (hm : ∀ i, marg Ps e i = p i) :
    Ps = jeffrey P e p := by
  rw [(jcond_iff_jeffrey hP hPs e).1 hJ]
  have : marg Ps e = p := funext hm
  rw [this]

/-- **(J) holds iff the likelihood ratio `P*/P` is constant on every cell.** -/
theorem jcond_iff_ratio_const {P Ps : Ω → ℝ} (hP : ∀ ω, 0 < P ω) (hPs : ∀ ω, 0 < Ps ω)
    (e : Ω → ι) :
    JCond P Ps e ↔ ∀ ω ω', e ω = e ω' → Ps ω / P ω = Ps ω' / P ω' := by
  constructor
  · intro hJ ω ω' h
    rw [thm22_factorization hP hPs hJ ω, thm22_factorization hP hPs hJ ω', h]
    field_simp [(hP ω).ne', (hP ω').ne']
  · intro hc A i
    by_cases hne : ∃ ω₀, e ω₀ = i
    · obtain ⟨ω₀, rfl⟩ := hne
      set x := Ps ω₀ / P ω₀ with hx
      have hxpos : 0 < x := div_pos (hPs ω₀) (hP ω₀)
      have hpt : ∀ ω ∈ cellSet e (e ω₀), Ps ω = x * P ω := fun ω hω => by
        have := hc ω ω₀ (mem_cellSet.1 hω)
        rw [hx, ← this]; field_simp [(hP ω).ne']
      have hS : ∀ S ⊆ cellSet e (e ω₀), mass Ps S = x * mass P S := fun S hS => by
        unfold mass; rw [Finset.mul_sum]
        exact Finset.sum_congr rfl fun ω hω => hpt ω (hS hω)
      unfold cond marg
      rw [hS _ Finset.inter_subset_right, hS _ subset_rfl,
        mul_div_mul_left _ _ hxpos.ne']
    · push Not at hne
      have hempty : cellSet e i = ∅ := by
        ext ω; simp [hne ω]
      simp [cond, marg, mass, hempty]

/-- The likelihood-ratio labelling `ω ↦ P*(ω)/P(ω)`; its level sets are the partition
`{E_x}` of Theorem 2.2. -/
def lr (P Ps : Ω → ℝ) : Ω → ℝ := fun ω => Ps ω / P ω

/-- **Theorem 2.2, second statement.** The likelihood-ratio partition is sufficient
for `{P, P*}` (it satisfies (J)) ... -/
theorem thm22_lr_sufficient {P Ps : Ω → ℝ} (hP : ∀ ω, 0 < P ω) (hPs : ∀ ω, 0 < Ps ω) :
    JCond P Ps (lr P Ps) :=
  (jcond_iff_ratio_const hP hPs _).2 fun _ _ h => h

/-- ... **and it is coarser than every sufficient partition**: if `{Eᵢ}` satisfies (J),
each `Eᵢ` lies inside one level set of `P*/P`. So it is the *minimal* sufficient
partition, and it depends on the pair `{P, P*}`. -/
theorem thm22_lr_minimal {P Ps : Ω → ℝ} (hP : ∀ ω, 0 < P ω) (hPs : ∀ ω, 0 < Ps ω)
    {e : Ω → ι} (hJ : JCond P Ps e) : ∀ ω ω', e ω = e ω' → lr P Ps ω = lr P Ps ω' :=
  (jcond_iff_ratio_const hP hPs e).1 hJ

/-- **Sufficient partitions are not unique:** every refinement of a partition that
satisfies (J) also satisfies (J). -/
theorem jcond_of_refines {P Ps : Ω → ℝ} (hP : ∀ ω, 0 < P ω) (hPs : ∀ ω, 0 < Ps ω)
    {e : Ω → ι} {e' : Ω → κ} [DecidableEq κ] (hJ : JCond P Ps e)
    (href : ∀ ω ω', e' ω = e' ω' → e ω = e ω') : JCond P Ps e' :=
  (jcond_iff_ratio_const hP hPs e').2 fun ω ω' h =>
    (jcond_iff_ratio_const hP hPs e).1 hJ ω ω' (href ω ω' h)

/-- In particular the partition into atoms satisfies (J) for every pair. -/
theorem jcond_atoms {P Ps : Ω → ℝ} (hP : ∀ ω, 0 < P ω) (hPs : ∀ ω, 0 < Ps ω) :
    JCond P Ps (id : Ω → Ω) :=
  (jcond_iff_ratio_const hP hPs _).2 fun _ _ h => by simp only [id] at h; rw [h]

/-- If the new opinion equals the prior, the one-cell partition is sufficient, so no
partition with two or more cells is minimal sufficient. -/
theorem jcond_trivial_self {P : Ω → ℝ} (hP : ∀ ω, 0 < P ω) :
    JCond P P (fun _ : Ω => ()) :=
  (jcond_iff_ratio_const hP hP _).2 fun ω ω' _ => by
    rw [div_self (hP ω).ne', div_self (hP ω').ne']

/-- **When the cue partition is the minimal one.** For a Jeffrey update whose
ratios `pᵢ/P(Eᵢ)` differ from cell to cell, the likelihood-ratio partition is the
cue's own partition. (For a two-cell partition and probabilities summing to one,
this holds iff `p₁ ≠ P(E₁)`; when `p₁ = P(E₁)`, `jcond_trivial_self` applies.) -/
theorem lr_jeffrey_iff {P : Ω → ℝ} (hP : ∀ ω, 0 < P ω) {e : Ω → ι} {p : ι → ℝ}
    (hinj : ∀ ω ω', p (e ω) / marg P e (e ω) = p (e ω') / marg P e (e ω') → e ω = e ω')
    (ω ω' : Ω) : lr P (jeffrey P e p) ω = lr P (jeffrey P e p) ω' ↔ e ω = e ω' := by
  have key : ∀ x, lr P (jeffrey P e p) x = p (e x) / marg P e (e x) := fun x => by
    simp only [lr, jeffrey]; field_simp [(hP x).ne']
  rw [key, key]
  exact ⟨hinj ω ω', fun h => by rw [h]⟩

/-! ## Section 2.1: obtaining `P*` from `P` by conditioning (Theorem 2.1) -/

/-- D-Z p. 824: `P*` *can be obtained from `P` by conditioning* if there are a
probability space `(Ω̃, Q)`, events `E_ω` with `Q(E_ω) = P(ω)`, and an event `E` with
`Q(E) > 0` and `Q(E_ω | E) = P*(ω)`. Here `Ω̃` is finite. -/
def ObtainableByConditioning (P Ps : Ω → ℝ) : Prop :=
  ∃ (Ω' : Type) (_ : Fintype Ω') (_ : DecidableEq Ω') (Q : Ω' → ℝ) (Eω : Ω → Finset Ω')
    (E : Finset Ω'), (∀ x, 0 ≤ Q x) ∧ ∑ x, Q x = 1 ∧ (∀ ω, ∑ x ∈ Eω ω, Q x = P ω) ∧
    0 < ∑ x ∈ E, Q x ∧ ∀ ω, (∑ x ∈ Eω ω ∩ E, Q x) / (∑ x ∈ E, Q x) = Ps ω

/-- **Theorem 2.1** (finite `Ω̃`). `P*` can be obtained from `P` by conditioning iff
there is a constant `B ≥ 1` with `P*(ω) ≤ B P(ω)` for all `ω`, eq. (2.1). -/
theorem thm21 {Ω : Type} [Fintype Ω] {P Ps : Ω → ℝ} (hP1 : ∑ ω, P ω = 1)
    (hPs : ∀ ω, 0 ≤ Ps ω) (hPs1 : ∑ ω, Ps ω = 1) :
    ObtainableByConditioning P Ps ↔ ∃ B : ℝ, 1 ≤ B ∧ ∀ ω, Ps ω ≤ B * P ω := by
  constructor
  · rintro ⟨Ω', _, _, Q, Eω, E, hQ, hQ1, hEω, hE, hcond⟩
    refine ⟨1 / ∑ x ∈ E, Q x, ?_, fun ω => ?_⟩
    · rw [le_div_iff₀ hE, one_mul, ← hQ1]
      exact Finset.sum_le_sum_of_subset_of_nonneg (Finset.subset_univ _)
        fun x _ _ => hQ x
    · rw [← hcond ω, ← hEω ω, one_div, ← div_eq_inv_mul]
      apply div_le_div_of_nonneg_right _ hE.le
      exact Finset.sum_le_sum_of_subset_of_nonneg Finset.inter_subset_left
        fun x _ _ => hQ x
  · rintro ⟨B, hB, hle⟩
    have hB0 : 0 < B := by linarith
    classical
    -- `Ω̃ = Ω × Bool`, `Q(ω, true) = P*(ω)/B`, `Q(ω, false) = P(ω) - P*(ω)/B`.
    refine ⟨Ω × Bool, inferInstance, inferInstance,
      fun x => if x.2 then Ps x.1 / B else P x.1 - Ps x.1 / B,
      fun ω => {(ω, true), (ω, false)}, univ ×ˢ {true}, ?_, ?_, ?_, ?_, ?_⟩
    · rintro ⟨ω, b⟩
      cases b
      · simp only [Bool.false_eq_true, ↓reduceIte, sub_nonneg]
        rw [div_le_iff₀ hB0, mul_comm]; exact hle ω
      · simp only [↓reduceIte]; exact div_nonneg (hPs ω) hB0.le
    · rw [Fintype.sum_prod_type]
      simp only [Fintype.sum_bool, ↓reduceIte, Bool.false_eq_true]
      rw [← hP1]
      exact Finset.sum_congr rfl fun ω _ => by ring
    · intro ω; simp
    · rw [Finset.sum_product]
      simp only [Finset.sum_singleton, ↓reduceIte]
      rw [← Finset.sum_div, hPs1]; positivity
    · intro ω
      have hinter : ({(ω, true), (ω, false)} : Finset (Ω × Bool)) ∩ univ ×ˢ {true} =
          {(ω, true)} := by
        ext ⟨a, b⟩; cases b <;> simp
      rw [hinter, Finset.sum_product]
      simp only [Finset.sum_singleton, ↓reduceIte]
      rw [← Finset.sum_div, hPs1]; field_simp

/-- D-Z p. 824: when `P` has full support on a finite space, (2.1) always holds, so
every `P*` can be obtained from `P` by conditioning. -/
theorem thm21_finite {Ω : Type} [Fintype Ω] {P Ps : Ω → ℝ} (hP : ∀ ω, 0 < P ω)
    (hP1 : ∑ ω, P ω = 1) (hPs : ∀ ω, 0 ≤ Ps ω) (hPs1 : ∑ ω, Ps ω = 1) :
    ObtainableByConditioning P Ps := by
  refine (thm21 hP1 hPs hPs1).2 ⟨1 + ∑ ω, Ps ω / P ω, ?_, fun ω => ?_⟩
  · have : 0 ≤ ∑ ω, Ps ω / P ω := Finset.sum_nonneg fun ω _ => div_nonneg (hPs ω) (hP ω).le
    linarith
  · have h1 : Ps ω / P ω ≤ ∑ ω, Ps ω / P ω :=
      Finset.single_le_sum (f := fun ω => Ps ω / P ω)
        (fun ω _ => div_nonneg (hPs ω) (hP ω).le) (Finset.mem_univ ω)
    rw [div_le_iff₀ (hP ω)] at h1
    nlinarith [hP ω]

/-! ## Sections 5-6: Jeffrey's rule as a minimum-distance ("mechanical") update -/

/-- Csiszár's `f`-divergence, D-Z p. 829: `I_f(Q, P) = Σ_ω P(ω) f(Q(ω)/P(ω))`.
The candidate posterior `Q` is the first argument, the prior `P` the second. -/
def fdiv (f : ℝ → ℝ) (Q P : Ω → ℝ) : ℝ := ∑ ω, P ω * f (Q ω / P ω)

/-- The Kullback-Leibler number of `Q` with respect to `P`, D-Z (5.3):
`I(Q, P) = Σ_ω Q(ω) log (Q(ω)/P(ω))`, i.e. `KL(Q ‖ P)`, posterior relative to prior. -/
def KL (Q P : Ω → ℝ) : ℝ := ∑ ω, Q ω * Real.log (Q ω / P ω)

/-- The Hellinger distance, D-Z (5.2): `H(P, Q) = Σ_ω (√P(ω) - √Q(ω))²`. -/
def hellinger (Q P : Ω → ℝ) : ℝ := ∑ ω, (√(Q ω) - √(P ω)) ^ 2

/-- The variation distance, D-Z (5.1): `‖P - Q‖ = (1/2) Σ_ω |P(ω) - Q(ω)|`. -/
def tv (Q P : Ω → ℝ) : ℝ := (1 / 2) * ∑ ω, |Q ω - P ω|

/-- The feasible set `C` of D-Z Theorems 5.1 and 6.1: nonnegative `Q` with
`Q(Eᵢ) = pᵢ` for every cell. -/
def Feasible (e : Ω → ι) (p : ι → ℝ) (Q : Ω → ℝ) : Prop :=
  (∀ ω, 0 ≤ Q ω) ∧ ∀ i, marg Q e i = p i

theorem jeffrey_feasible {P : Ω → ℝ} (hP : ∀ ω, 0 < P ω) {e : Ω → ι}
    (he : Function.Surjective e) {p : ι → ℝ} (hp : ∀ i, 0 ≤ p i) :
    Feasible e p (jeffrey P e p) :=
  ⟨fun ω => div_nonneg (mul_nonneg (hp _) (hP ω).le) (marg_pos hP e ω).le,
    marg_jeffrey hP he p⟩

/-- `I_f(P*, P)` of Jeffrey's `P*` is the divergence between the two measures on the
partition, `Σᵢ P(Eᵢ) f(pᵢ/P(Eᵢ))` (D-Z Theorem 6.1, first equality). -/
theorem fdiv_jeffrey [Fintype ι] (f : ℝ → ℝ) {P : Ω → ℝ} (hP : ∀ ω, 0 < P ω) (e : Ω → ι)
    (p : ι → ℝ) : fdiv f (jeffrey P e p) P = ∑ i, marg P e i * f (p i / marg P e i) := by
  unfold fdiv
  have h : ∀ ω, P ω * f (jeffrey P e p ω / P ω) =
      P ω * f (p (e ω) / marg P e (e ω)) := fun ω => by
    congr 2; simp only [jeffrey]; field_simp [(hP ω).ne']
  simp_rw [h]
  rw [← Finset.sum_fiberwise univ e]
  refine Finset.sum_congr rfl fun i _ => ?_
  rw [marg, mass, Finset.sum_mul]
  refine Finset.sum_congr (by ext; simp) fun ω hω => ?_
  rw [(Finset.mem_filter.1 hω).2]
  rfl

/-- Per cell, Jensen: `P(Eᵢ) f(pᵢ/P(Eᵢ)) ≤ Σ_{ω ∈ Eᵢ} P(ω) f(Q(ω)/P(ω))`. -/
theorem cell_jensen {f : ℝ → ℝ} (hf : ConvexOn ℝ (Set.Ici 0) f) {P Q : Ω → ℝ}
    (hP : ∀ ω, 0 < P ω) (hQ : ∀ ω, 0 ≤ Q ω) (e : Ω → ι) (i : ι) :
    marg P e i * f (marg Q e i / marg P e i) ≤
      ∑ ω ∈ cellSet e i, P ω * f (Q ω / P ω) := by
  by_cases hne : (cellSet e i).Nonempty
  · have hm : 0 < marg P e i := Finset.sum_pos (fun x _ => hP x) hne
    have hw1 : ∑ ω ∈ cellSet e i, P ω / marg P e i = 1 := by
      rw [← Finset.sum_div]; exact div_self hm.ne'
    have hJ := hf.map_sum_le (t := cellSet e i) (w := fun ω => P ω / marg P e i)
      (p := fun ω => Q ω / P ω) (fun ω _ => (div_pos (hP ω) hm).le) hw1
      (fun ω _ => Set.mem_Ici.2 (div_nonneg (hQ ω) (hP ω).le))
    have hsum : ∑ ω ∈ cellSet e i, (P ω / marg P e i) • (Q ω / P ω) = marg Q e i / marg P e i := by
      simp only [smul_eq_mul, marg, mass]
      rw [Finset.sum_div]
      exact Finset.sum_congr rfl fun ω _ => by field_simp [(hP ω).ne']
    rw [hsum] at hJ
    simp only [smul_eq_mul] at hJ
    calc marg P e i * f (marg Q e i / marg P e i)
        ≤ marg P e i * ∑ ω ∈ cellSet e i, P ω / marg P e i * f (Q ω / P ω) :=
          mul_le_mul_of_nonneg_left hJ hm.le
      _ = _ := by
          rw [Finset.mul_sum]
          exact Finset.sum_congr rfl fun ω _ => by field_simp
  · rw [Finset.not_nonempty_iff_eq_empty] at hne
    simp [marg, mass, hne]

/-- **D-Z Theorem 6.1** (finite `Ω`, `𝒜₀` generated by the partition). For convex `f`,
Jeffrey's `P*` minimizes `I_f(Q, P)` over all `Q` with `Q(Eᵢ) = pᵢ`. -/
theorem thm61_le [Fintype ι] {f : ℝ → ℝ} (hf : ConvexOn ℝ (Set.Ici 0) f) {P Q : Ω → ℝ}
    (hP : ∀ ω, 0 < P ω) {e : Ω → ι} {p : ι → ℝ} (hQ : Feasible e p Q) :
    fdiv f (jeffrey P e p) P ≤ fdiv f Q P := by
  rw [fdiv_jeffrey f hP e p, fdiv, ← Finset.sum_fiberwise univ e]
  refine Finset.sum_le_sum fun i _ => ?_
  have := cell_jensen hf hP hQ.1 e i
  rw [hQ.2 i] at this
  refine this.trans (le_of_eq (Finset.sum_congr (by ext; simp) fun _ _ => rfl))

/-- **D-Z Theorem 6.1, uniqueness.** If `f` is strictly convex, Jeffrey's `P*` is the
*unique* minimizer: any feasible `Q` with `I_f(Q,P) = I_f(P*,P)` equals `P*`. -/
theorem thm61_unique [Fintype ι] {f : ℝ → ℝ} (hf : StrictConvexOn ℝ (Set.Ici 0) f)
    {P Q : Ω → ℝ} (hP : ∀ ω, 0 < P ω) {e : Ω → ι} {p : ι → ℝ} (hQ : Feasible e p Q)
    (heq : fdiv f Q P = fdiv f (jeffrey P e p) P) : Q = jeffrey P e p := by
  rw [fdiv_jeffrey f hP e p, fdiv, ← Finset.sum_fiberwise univ e] at heq
  have hle : ∀ i ∈ (univ : Finset ι), marg P e i * f (p i / marg P e i) ≤
      ∑ ω ∈ univ with e ω = i, P ω * f (Q ω / P ω) := fun i _ => by
    have := cell_jensen hf.convexOn hP hQ.1 e i
    rw [hQ.2 i] at this
    exact this.trans (le_of_eq (Finset.sum_congr (by ext; simp) fun _ _ => rfl))
  have hcell := (Finset.sum_eq_sum_iff_of_le hle).1 heq.symm
  funext ω
  set i := e ω
  have hm : 0 < marg P e i := marg_pos hP e ω
  have hEq := hcell i (Finset.mem_univ _)
  have hw1 : ∑ x ∈ cellSet e i, P x / marg P e i = 1 := by
    rw [← Finset.sum_div]; exact div_self hm.ne'
  have hsum : ∑ x ∈ cellSet e i, (P x / marg P e i) • (Q x / P x) = p i / marg P e i := by
    simp only [smul_eq_mul]
    rw [← hQ.2 i]
    have : marg Q e i / marg P e i = ∑ x ∈ cellSet e i, Q x / marg P e i := by
      unfold marg mass; rw [Finset.sum_div]
    rw [this]
    exact Finset.sum_congr rfl fun x _ => by field_simp [(hP x).ne']
  have hJeq : f (∑ x ∈ cellSet e i, (P x / marg P e i) • (Q x / P x)) =
      ∑ x ∈ cellSet e i, (P x / marg P e i) • f (Q x / P x) := by
    rw [hsum]
    simp only [smul_eq_mul]
    have : ∑ x ∈ cellSet e i, P x * f (Q x / P x) = marg P e i * f (p i / marg P e i) := by
      rw [hEq]; rfl
    rw [show ∑ x ∈ cellSet e i, P x / marg P e i * f (Q x / P x) =
        (∑ x ∈ cellSet e i, P x * f (Q x / P x)) / marg P e i by
      rw [Finset.sum_div]; exact Finset.sum_congr rfl fun x _ => by ring, this]
    field_simp
  have hall := (hf.map_sum_eq_iff_of_pos (t := cellSet e i)
    (w := fun x => P x / marg P e i) (p := fun x => Q x / P x)
    (fun x _ => div_pos (hP x) hm) hw1
    (fun x _ => Set.mem_Ici.2 (div_nonneg (hQ.1 x) (hP x).le))).1 hJeq
  have hωmem : ω ∈ cellSet e i := by simp [i]
  have hconst : Q ω / P ω = p i / marg P e i := by
    rw [← hsum]
    simp only [smul_eq_mul]
    rw [Finset.sum_congr rfl fun x hx => by rw [hall hx hωmem], ← Finset.sum_mul, hw1, one_mul]
  simp only [jeffrey]
  rw [div_eq_div_iff (hP ω).ne' hm.ne'] at hconst
  rw [eq_div_iff hm.ne']
  linear_combination hconst

/-! ### Theorem 5.1 (5.6): Kullback-Leibler -/

theorem fdiv_mulLog_eq_KL {P : Ω → ℝ} (hP : ∀ ω, 0 < P ω) (Q : Ω → ℝ) :
    fdiv (fun u => u * Real.log u) Q P = KL Q P := by
  unfold fdiv KL
  exact Finset.sum_congr rfl fun ω _ => by rw [← mul_assoc, mul_div_cancel₀ _ (hP ω).ne']

theorem KL_jeffrey [Fintype ι] {P : Ω → ℝ} (hP : ∀ ω, 0 < P ω) {e : Ω → ι}
    (he : Function.Surjective e) (p : ι → ℝ) :
    KL (jeffrey P e p) P = ∑ i, p i * Real.log (p i / marg P e i) := by
  rw [← fdiv_mulLog_eq_KL hP, fdiv_jeffrey _ hP]
  exact Finset.sum_congr rfl fun i _ => by
    rw [← mul_assoc, mul_div_cancel₀ _ (marg_pos_of_surj hP he i).ne']

/-- **D-Z Theorem 5.1, (5.6).** For every `Q` with `Q(Eᵢ) = pᵢ`,
`I(Q, P) ≥ Σᵢ pᵢ log (pᵢ/P(Eᵢ))`, where `I(Q,P) = Σ Q log (Q/P)` = `KL(Q ‖ P)`. -/
theorem thm51_KL_le [Fintype ι] {P Q : Ω → ℝ} (hP : ∀ ω, 0 < P ω) {e : Ω → ι}
    (he : Function.Surjective e) {p : ι → ℝ} (hQ : Feasible e p Q) :
    ∑ i, p i * Real.log (p i / marg P e i) ≤ KL Q P := by
  rw [← KL_jeffrey hP he, ← fdiv_mulLog_eq_KL hP, ← fdiv_mulLog_eq_KL hP]
  exact thm61_le Real.strictConvexOn_mul_log.convexOn hP hQ

/-- **D-Z Theorem 5.1, equality in (5.6).** Equality holds iff `Q` is Jeffrey's
update: Jeffrey's rule is the *unique* minimizer of `KL(Q ‖ P)` over `C`. -/
theorem thm51_KL_eq_iff [Fintype ι] {P Q : Ω → ℝ} (hP : ∀ ω, 0 < P ω) {e : Ω → ι}
    (he : Function.Surjective e) {p : ι → ℝ} (hQ : Feasible e p Q) :
    KL Q P = ∑ i, p i * Real.log (p i / marg P e i) ↔ Q = jeffrey P e p := by
  rw [← KL_jeffrey hP he]
  constructor
  · intro h
    rw [← fdiv_mulLog_eq_KL hP, ← fdiv_mulLog_eq_KL hP] at h
    exact thm61_unique Real.strictConvexOn_mul_log hP hQ h
  · intro h; rw [h]

/-! ### Theorem 5.1 (5.5): Hellinger -/

/-- The generator of the Hellinger distance, `f(u) = (√u - 1)² = u - 2√u + 1`. -/
def fH (u : ℝ) : ℝ := u - 2 * √u + 1

theorem strictConvexOn_fH : StrictConvexOn ℝ (Set.Ici 0) fH := by
  have h1 : StrictConvexOn ℝ (Set.Ici (0 : ℝ)) (-(√·)) := Real.strictConcaveOn_sqrt.neg
  have h2 : ConvexOn ℝ (Set.Ici (0 : ℝ)) (fun u : ℝ => u + 1) :=
    (convexOn_id (convex_Ici 0)).add_const 1
  refine ((h1.add h1).add_convexOn h2).congr fun u _ => ?_
  simp only [Pi.add_apply, Pi.neg_apply, fH]; ring

theorem mul_fH {a b : ℝ} (ha : 0 ≤ a) (hb : 0 < b) : b * fH (a / b) = (√a - √b) ^ 2 := by
  unfold fH
  rw [Real.sqrt_div' _ hb.le]
  have hsb : 0 < √b := Real.sqrt_pos.2 hb
  have h1 : √a ^ 2 = a := Real.sq_sqrt ha
  have h2 : √b ^ 2 = b := Real.sq_sqrt hb.le
  field_simp
  nlinarith [h1, h2]

theorem fdiv_fH_eq_hellinger {P Q : Ω → ℝ} (hP : ∀ ω, 0 < P ω) (hQ : ∀ ω, 0 ≤ Q ω) :
    fdiv fH Q P = hellinger Q P :=
  Finset.sum_congr rfl fun ω _ => mul_fH (hQ ω) (hP ω)

/-- **D-Z Theorem 5.1, (5.5).** `H(Q, P) ≥ Σᵢ (√P(Eᵢ) - √pᵢ)²` for every `Q ∈ C` ... -/
theorem thm51_hellinger_le [Fintype ι] {P Q : Ω → ℝ} (hP : ∀ ω, 0 < P ω) {e : Ω → ι}
    (he : Function.Surjective e) {p : ι → ℝ} (hQ : Feasible e p Q) :
    ∑ i, (√(p i) - √(marg P e i)) ^ 2 ≤ hellinger Q P := by
  have hp : ∀ i, 0 ≤ p i := fun i => hQ.2 i ▸ marg_nonneg hQ.1 e i
  have hval : fdiv fH (jeffrey P e p) P = ∑ i, (√(p i) - √(marg P e i)) ^ 2 := by
    rw [fdiv_jeffrey _ hP]
    exact Finset.sum_congr rfl fun i _ => mul_fH (hp i) (marg_pos_of_surj hP he i)
  rw [← hval, ← fdiv_fH_eq_hellinger hP hQ.1]
  exact thm61_le strictConvexOn_fH.convexOn hP hQ

/-- ... **with equality iff `Q` is Jeffrey's update** (D-Z p. 828). -/
theorem thm51_hellinger_eq_iff [Fintype ι] {P Q : Ω → ℝ} (hP : ∀ ω, 0 < P ω) {e : Ω → ι}
    (he : Function.Surjective e) {p : ι → ℝ} (hQ : Feasible e p Q) :
    hellinger Q P = ∑ i, (√(p i) - √(marg P e i)) ^ 2 ↔ Q = jeffrey P e p := by
  have hp : ∀ i, 0 ≤ p i := fun i => hQ.2 i ▸ marg_nonneg hQ.1 e i
  have hval : fdiv fH (jeffrey P e p) P = ∑ i, (√(p i) - √(marg P e i)) ^ 2 := by
    rw [fdiv_jeffrey _ hP]
    exact Finset.sum_congr rfl fun i _ => mul_fH (hp i) (marg_pos_of_surj hP he i)
  rw [← hval, ← fdiv_fH_eq_hellinger hP hQ.1]
  constructor
  · exact thm61_unique strictConvexOn_fH hP hQ
  · intro h; rw [h]

/-! ### Theorem 5.1 (5.4) and Remark (a): variation distance -/

/-- **D-Z Theorem 5.1, (5.4).** `‖Q - P‖ ≥ (1/2) Σᵢ |P(Eᵢ) - pᵢ|` for every `Q ∈ C`. -/
theorem thm51_tv_le [Fintype ι] {P Q : Ω → ℝ} {e : Ω → ι} {p : ι → ℝ}
    (hQ : Feasible e p Q) : (1 / 2) * ∑ i, |p i - marg P e i| ≤ tv Q P := by
  unfold tv
  apply mul_le_mul_of_nonneg_left _ (by norm_num)
  rw [← Finset.sum_fiberwise univ e]
  refine Finset.sum_le_sum fun i _ => ?_
  rw [← hQ.2 i, marg, marg, mass, mass, ← Finset.sum_sub_distrib]
  exact (Finset.abs_sum_le_sum_abs _ _).trans (le_of_eq (Finset.sum_congr (by ext; simp) fun _ _ => rfl))

/-- Jeffrey's rule attains the bound (5.4). -/
theorem tv_jeffrey [Fintype ι] {P : Ω → ℝ} (hP : ∀ ω, 0 < P ω) {e : Ω → ι}
    (he : Function.Surjective e) (p : ι → ℝ) :
    tv (jeffrey P e p) P = (1 / 2) * ∑ i, |p i - marg P e i| := by
  unfold tv
  congr 1
  rw [← Finset.sum_fiberwise univ e]
  refine Finset.sum_congr rfl fun i _ => ?_
  have hm := marg_pos_of_surj hP he i
  have hpt : ∀ ω ∈ univ.filter (fun ω => e ω = i), |jeffrey P e p ω - P ω| =
      |p i / marg P e i - 1| * P ω := fun ω hω => by
    have hi : e ω = i := (Finset.mem_filter.1 hω).2
    simp only [jeffrey, hi]
    rw [show p i * P ω / marg P e i - P ω = (p i / marg P e i - 1) * P ω by ring, abs_mul,
      abs_of_pos (hP ω)]
  rw [Finset.sum_congr rfl hpt, ← Finset.mul_sum]
  change |p i / marg P e i - 1| * marg P e i = _
  rw [show p i / marg P e i - 1 = (p i - marg P e i) / marg P e i by field_simp, abs_div,
    abs_of_pos hm, div_mul_cancel₀ _ hm.ne']

/-- **Remark (a), p. 828: the variation-distance minimizer is not unique.** On a
three-point space with cells `{0,1}` and `{2}`, prior `(1/4, 1/4, 1/2)` and new cell
probabilities `(3/4, 1/4)`, Jeffrey's update `(3/8, 3/8, 1/4)` and the different
`Q = (1/2, 1/4, 1/4)` have the same marginal and the same, minimal, variation
distance `1/4` from the prior. -/
theorem remark_a_tv_not_unique :
    let e : Fin 3 → Bool := ![true, true, false]
    let P : Fin 3 → ℝ := ![1 / 4, 1 / 4, 1 / 2]
    let p : Bool → ℝ := fun b => if b then 3 / 4 else 1 / 4
    let Q : Fin 3 → ℝ := ![1 / 2, 1 / 4, 1 / 4]
    Feasible e p Q ∧ Q ≠ jeffrey P e p ∧ tv Q P = 1 / 4 ∧ tv (jeffrey P e p) P = 1 / 4 ∧
      (1 / 2) * ∑ i, |p i - marg P e i| = 1 / 4 := by
  intro e P p Q
  have hm : ∀ b, marg P e b = 1 / 2 := by
    intro b; cases b <;>
      (simp [marg, mass, cellSet, Finset.sum_filter, Fin.sum_univ_three, e, P]; try norm_num)
  refine ⟨⟨fun ω => by fin_cases ω <;> simp [Q], fun b => ?_⟩, fun h => ?_, ?_, ?_, ?_⟩
  · cases b <;> (simp [marg, mass, cellSet, Finset.sum_filter, Fin.sum_univ_three, e, Q, p]; try norm_num)
  · have := congrFun h 0
    simp only [jeffrey, hm] at this
    simp [Q, P, p, e] at this
    norm_num at this
  · simp [tv, Fin.sum_univ_three, Q, P]; norm_num [abs_of_pos, abs_of_neg]
  · simp only [tv, jeffrey, hm]
    simp [Fin.sum_univ_three, P, p, e]; norm_num [abs_of_pos, abs_of_neg]
  · simp [hm, p]; norm_num [abs_of_pos, abs_of_neg]

/-! ### Example 5.1 (Section 5.3): the I-projection is not the variation minimizer -/

/-- **Example 5.1, p. 828-829.** From the uniform `2 × 2` table `P⁰` to margins
`(1/3, 2/3)` on rows and columns. The independent table `Pᴵ = (1/9, 2/9, 2/9, 4/9)`
(the I-projection: it keeps `P⁰`'s association factor, odds ratio 1) has variation
distance `7/36`, while `Pⱽ = (1/12, 1/4, 1/4, 5/12)` has `1/6`, and `1/6` is the
minimum over every table with those margins. -/
theorem example51 :
    (1 / 9 + 2 / 9 = (1 / 3 : ℝ) ∧ 2 / 9 + 4 / 9 = (2 / 3 : ℝ) ∧ (1 / 9) * (4 / 9) = (2 / 9 : ℝ) * (2 / 9)) ∧
    (1 / 2 : ℝ) * (|1 / 9 - 1 / 4| + |2 / 9 - 1 / 4| + |2 / 9 - 1 / 4| + |4 / 9 - 1 / 4|) = 7 / 36 ∧
    (1 / 12 + 1 / 4 = (1 / 3 : ℝ) ∧ 1 / 4 + 5 / 12 = (2 / 3 : ℝ)) ∧
    (1 / 2 : ℝ) * (|1 / 12 - 1 / 4| + |1 / 4 - 1 / 4| + |1 / 4 - 1 / 4| + |5 / 12 - 1 / 4|) = 1 / 6 ∧
    ∀ p₁ p₂ p₃ p₄ : ℝ, p₁ + p₂ = 1 / 3 → p₃ + p₄ = 2 / 3 → p₁ + p₃ = 1 / 3 → p₂ + p₄ = 2 / 3 →
      1 / 6 ≤ (1 / 2) * (|p₁ - 1 / 4| + |p₂ - 1 / 4| + |p₃ - 1 / 4| + |p₄ - 1 / 4|) := by
  refine ⟨by norm_num, by norm_num [abs_of_pos, abs_of_neg], by norm_num,
    by norm_num [abs_of_pos, abs_of_neg], fun p₁ p₂ p₃ p₄ h1 h2 h3 h4 => ?_⟩
  have a1 := neg_abs_le (p₁ - 1 / 4)
  have a2 := abs_nonneg (p₂ - 1 / 4)
  have a3 := abs_nonneg (p₃ - 1 / 4)
  have a4 := le_abs_self (p₄ - 1 / 4)
  linarith

/-! ## Section 3: successive updating and commutativity -/

section Successive

variable [DecidableEq κ]

/-- `P_{𝓔𝓕}`: Jeffrey-update on `𝓔 = {Eᵢ}` to `pᵢ`, then on `𝓕 = {Fⱼ}` to `qⱼ` (p. 825). -/
def PEF (P : Ω → ℝ) (e : Ω → ι) (p : ι → ℝ) (f : Ω → κ) (q : κ → ℝ) : Ω → ℝ :=
  jeffrey (jeffrey P e p) f q

/-- `P_{𝓕𝓔}`: the other order. -/
def PFE (P : Ω → ℝ) (e : Ω → ι) (p : ι → ℝ) (f : Ω → κ) (q : κ → ℝ) : Ω → ℝ :=
  jeffrey (jeffrey P f q) e p

/-- **Jeffrey independence** (Definition, p. 825): updating on `𝓔` to `p` leaves
the probabilities of `𝓕` alone, and updating on `𝓕` to `q` leaves those of `𝓔`
alone: `P_𝓔(Fⱼ) = P(Fⱼ)` and `P_𝓕(Eᵢ) = P(Eᵢ)`. -/
def JIndep (P : Ω → ℝ) (e : Ω → ι) (p : ι → ℝ) (f : Ω → κ) (q : κ → ℝ) : Prop :=
  (∀ j, marg (jeffrey P e p) f j = marg P f j) ∧ (∀ i, marg (jeffrey P f q) e i = marg P e i)

/-- **P-independence** (3.4): `P(Eᵢ ∩ Fⱼ) = P(Eᵢ) P(Fⱼ)`. -/
def PIndep (P : Ω → ℝ) (e : Ω → ι) (f : Ω → κ) : Prop :=
  ∀ i j, mass P (cellSet e i ∩ cellSet f j) = marg P e i * marg P f j

theorem jeffrey_apply (P : Ω → ℝ) (e : Ω → ι) (p : ι → ℝ) (ω : Ω) :
    jeffrey P e p ω = p (e ω) * P ω / marg P e (e ω) := rfl

theorem jeffrey_pos {P : Ω → ℝ} (hP : ∀ ω, 0 < P ω) (e : Ω → ι) {p : ι → ℝ}
    (hp : ∀ i, 0 < p i) (ω : Ω) : 0 < jeffrey P e p ω :=
  div_pos (mul_pos (hp _) (hP ω)) (marg_pos hP e ω)

theorem sum_jeffrey [Fintype ι] {P : Ω → ℝ} (hP : ∀ ω, 0 < P ω) {e : Ω → ι}
    (he : Function.Surjective e) (p : ι → ℝ) : ∑ ω, jeffrey P e p ω = ∑ i, p i := by
  rw [sum_eq_sum_marg _ e]
  exact Finset.sum_congr rfl fun i _ => marg_jeffrey hP he p i

/-- **D-Z Theorem 3.2** (p. 825), with the qualitative-independence hypothesis that
every `Eᵢ ∩ Fⱼ` is nonempty. For a full-support prior and positive targets,
`P_𝓔𝓕 = P_𝓕𝓔` iff `𝓔` and `𝓕` are Jeffrey independent with respect to `P, p, q`.
The proof is D-Z's direct algebra, (3.5)-(3.6). Without the hypothesis the forward
direction fails: see `thm32_needs_qualitative_independence`. -/
theorem thm32 [Fintype ι] [Fintype κ] {P : Ω → ℝ} (hP : ∀ ω, 0 < P ω) (hP1 : ∑ ω, P ω = 1)
    {e : Ω → ι} {f : Ω → κ} {p : ι → ℝ} {q : κ → ℝ} (hp : ∀ i, 0 < p i) (hp1 : ∑ i, p i = 1)
    (hq : ∀ j, 0 < q j) (hq1 : ∑ j, q j = 1) (hqi : ∀ i j, ∃ ω, e ω = i ∧ f ω = j) :
    PEF P e p f q = PFE P e p f q ↔ JIndep P e p f q := by
  have he : Function.Surjective e := fun i => by
    obtain ⟨ω₀, -⟩ := Finset.exists_ne_zero_of_sum_ne_zero (hP1 ▸ one_ne_zero : ∑ ω, P ω ≠ 0)
    obtain ⟨ω, h, -⟩ := hqi i (f ω₀); exact ⟨ω, h⟩
  have hf : Function.Surjective f := fun j => by
    obtain ⟨ω₀, -⟩ := Finset.exists_ne_zero_of_sum_ne_zero (hP1 ▸ one_ne_zero : ∑ ω, P ω ≠ 0)
    obtain ⟨ω, -, h⟩ := hqi (e ω₀) j; exact ⟨ω, h⟩
  set J₁ := jeffrey P e p
  set J₂ := jeffrey P f q
  have hJ₁ := jeffrey_pos hP e hp
  have hJ₂ := jeffrey_pos hP f hq
  -- the four marginals of the proof: `m = P(Eᵢ)`, `n = P(Fⱼ)`, `a = P_𝓔(Fⱼ)`, `b = P_𝓕(Eᵢ)`
  have hm := marg_pos_of_surj hP he
  have hn := marg_pos_of_surj hP hf
  have ha := marg_pos_of_surj hJ₁ hf
  have hb := marg_pos_of_surj hJ₂ he
  have sm : ∑ i, marg P e i = 1 := by rw [← sum_eq_sum_marg, hP1]
  have sn : ∑ j, marg P f j = 1 := by rw [← sum_eq_sum_marg, hP1]
  have sa : ∑ j, marg J₁ f j = 1 := by rw [← sum_eq_sum_marg, sum_jeffrey hP he, hp1]
  have sb : ∑ i, marg J₂ e i = 1 := by rw [← sum_eq_sum_marg, sum_jeffrey hP hf, hq1]
  -- pointwise, the two orders agree iff `P(Eᵢ) P_𝓔(Fⱼ) = P(Fⱼ) P_𝓕(Eᵢ)`
  have key : ∀ ω, PEF P e p f q ω = PFE P e p f q ω ↔
      marg P e (e ω) * marg J₁ f (f ω) = marg P f (f ω) * marg J₂ e (e ω) := fun ω => by
    have h1 := hm (e ω); have h2 := hn (f ω); have h3 := ha (f ω); have h4 := hb (e ω)
    have hX : 0 < p (e ω) * q (f ω) * P ω := mul_pos (mul_pos (hp _) (hq _)) (hP ω)
    simp only [PEF, PFE, jeffrey_apply]
    generalize marg J₁ f (f ω) = A at h3 ⊢
    generalize marg J₂ e (e ω) = B at h4 ⊢
    generalize marg P e (e ω) = M at h1 ⊢
    generalize marg P f (f ω) = N at h2 ⊢
    rw [show q (f ω) * (p (e ω) * P ω / M) / A = (p (e ω) * q (f ω) * P ω) / (M * A) by ring,
      show p (e ω) * (q (f ω) * P ω / N) / B = (p (e ω) * q (f ω) * P ω) / (N * B) by ring,
      div_eq_div_iff (by positivity) (by positivity)]
    constructor
    · intro h; exact (mul_left_cancel₀ hX.ne' h).symm
    · intro h; rw [h]
  constructor
  · intro heq
    have hij : ∀ i j, marg P e i * marg J₁ f j = marg P f j * marg J₂ e i := fun i j => by
      obtain ⟨ω, rfl, rfl⟩ := hqi i j
      exact (key ω).1 (congrFun heq ω)
    refine ⟨fun j => ?_, fun i => ?_⟩
    · calc marg J₁ f j = ∑ i, marg P e i * marg J₁ f j := by rw [← Finset.sum_mul, sm, one_mul]
        _ = ∑ i, marg P f j * marg J₂ e i := Finset.sum_congr rfl fun i _ => hij i j
        _ = marg P f j := by rw [← Finset.mul_sum, sb, mul_one]
    · calc marg J₂ e i = ∑ j, marg P f j * marg J₂ e i := by rw [← Finset.sum_mul, sn, one_mul]
        _ = ∑ j, marg P e i * marg J₁ f j := Finset.sum_congr rfl fun j _ => (hij i j).symm
        _ = marg P e i := by rw [← Finset.mul_sum, sa, mul_one]
  · rintro ⟨hF, hE⟩
    funext ω
    rw [key ω, hF, hE, mul_comm]

/-- **Theorem 3.2 needs qualitative independence.** With `𝓔 = 𝓕` (D-Z's Example 3.1)
and equal targets `p = q = (4/5, 1/5)` against a prior `(1/2, 1/2)`, the two orders
agree, yet `𝓔, 𝓕` are not Jeffrey independent. D-Z state Theorem 3.2 without a
hypothesis; their proof divides by `P(A Eᵢ Fⱼ)` for `A = E_{i₀} F_{j₀}`, which is `0`
when `E_{i₀} ∩ F_{j₀} = ∅`. -/
theorem thm32_needs_qualitative_independence :
    let P : Bool → ℝ := fun _ => 1 / 2
    let p : Bool → ℝ := fun b => if b then 4 / 5 else 1 / 5
    PEF P id p id p = PFE P id p id p ∧ ¬ JIndep P id p id p := by
  intro P p
  refine ⟨rfl, fun ⟨h, _⟩ => ?_⟩
  have := h true
  have hc : cellSet (id : Bool → Bool) true = {true} := by ext x; simp
  simp only [marg, mass, id, hc, Finset.sum_singleton, jeffrey] at this
  norm_num [P, p] at this

/-- **D-Z Theorem 3.3, forward direction** (3.7): P-independent partitions are
Jeffrey independent for every choice of targets. -/
theorem thm33_forward [Fintype ι] [Fintype κ] {P : Ω → ℝ} (hP : ∀ ω, 0 < P ω)
    {e : Ω → ι} {f : Ω → κ} (he : Function.Surjective e) (hf : Function.Surjective f)
    (hind : PIndep P e f) {p : ι → ℝ} {q : κ → ℝ} (hp1 : ∑ i, p i = 1) (hq1 : ∑ j, q j = 1) :
    JIndep P e p f q := by
  refine ⟨fun j => ?_, fun i => ?_⟩
  · rw [marg, mass_eq_sum_cells _ e,
      Finset.sum_congr rfl fun i _ => by
        rw [mass_inter_cell_jeffrey, Finset.inter_comm, hind,
          div_mul_eq_mul_div, mul_div_assoc, mul_div_cancel_left₀ _ (marg_pos_of_surj hP he i).ne'],
      ← Finset.sum_mul, hp1, one_mul]
  · rw [marg, mass_eq_sum_cells _ f,
      Finset.sum_congr rfl fun j _ => by
        rw [mass_inter_cell_jeffrey, hind, div_mul_eq_mul_div, mul_comm (marg P e i),
          mul_div_assoc, mul_div_cancel_left₀ _ (marg_pos_of_surj hP hf j).ne'],
      ← Finset.sum_mul, hq1, one_mul]

end Successive

/-! ### Two binary partitions: the `2 × 2` table -/

/-- A binary target: `P*(E) = x`, `P*(Ē) = 1 - x`. -/
def bin (x : ℝ) : Bool → ℝ := fun b => if b then x else 1 - x

/-- A `2 × 2` prior on `Bool × Bool` (first coordinate `E`, second `F`) with margins
`P(E) = α`, `P(F) = β` and covariance `c`: `P(E F) = αβ + c`, etc. -/
def P2 (α β c : ℝ) : Bool × Bool → ℝ
  | (true, true) => α * β + c
  | (true, false) => α * (1 - β) - c
  | (false, true) => (1 - α) * β - c
  | (false, false) => (1 - α) * (1 - β) + c

theorem P2_pos_iff {α β c : ℝ} :
    (∀ ω, 0 < P2 α β c ω) ↔ 0 < α * β + c ∧ 0 < α * (1 - β) - c ∧ 0 < (1 - α) * β - c ∧
      0 < (1 - α) * (1 - β) + c := by
  constructor
  · intro h; exact ⟨h (true, true), h (true, false), h (false, true), h (false, false)⟩
  · rintro ⟨h1, h2, h3, h4⟩ ⟨_ | _, _ | _⟩ <;> assumption

theorem cellSet_fst (b : Bool) :
    cellSet (Prod.fst : Bool × Bool → Bool) b = {(b, true), (b, false)} := by
  ext ⟨x, y⟩; cases x <;> cases y <;> cases b <;> simp [cellSet]

theorem cellSet_snd (b : Bool) :
    cellSet (Prod.snd : Bool × Bool → Bool) b = {(true, b), (false, b)} := by
  ext ⟨x, y⟩; cases x <;> cases y <;> cases b <;> simp [cellSet]

theorem marg_fst (X : Bool × Bool → ℝ) (b : Bool) :
    marg X Prod.fst b = X (b, true) + X (b, false) := by
  simp [marg, mass, cellSet_fst]

theorem marg_snd (X : Bool × Bool → ℝ) (b : Bool) :
    marg X Prod.snd b = X (true, b) + X (false, b) := by
  simp [marg, mass, cellSet_snd]

theorem marg_P2_fst (α β c : ℝ) (b : Bool) : marg (P2 α β c) Prod.fst b = bin α b := by
  rw [marg_fst]; cases b <;> simp [P2, bin] <;> ring

theorem marg_P2_snd (α β c : ℝ) (b : Bool) : marg (P2 α β c) Prod.snd b = bin β b := by
  rw [marg_snd]; cases b <;> simp [P2, bin] <;> ring

/-- Updating `E` to `p` moves `P(F)` by `(p - α) c / (α (1 - α))`. -/
theorem marg_jE_snd {α β c : ℝ} (hα0 : α ≠ 0) (hα1 : 1 - α ≠ 0) (p : ℝ) (b : Bool) :
    marg (jeffrey (P2 α β c) Prod.fst (bin p)) Prod.snd b =
      bin β b + (if b then 1 else -1) * ((p - α) * c / (α * (1 - α))) := by
  rw [marg_snd]
  simp only [jeffrey_apply, marg_P2_fst]
  cases b <;> simp [P2, bin] <;> field_simp <;> ring

/-- Updating `F` to `q` moves `P(E)` by `(q - β) c / (β (1 - β))`. -/
theorem marg_jF_fst {α β c : ℝ} (hβ0 : β ≠ 0) (hβ1 : 1 - β ≠ 0) (q : ℝ) (b : Bool) :
    marg (jeffrey (P2 α β c) Prod.snd (bin q)) Prod.fst b =
      bin α b + (if b then 1 else -1) * ((q - β) * c / (β * (1 - β))) := by
  rw [marg_fst]
  simp only [jeffrey_apply, marg_P2_snd]
  cases b <;> simp [P2, bin] <;> field_simp <;> ring

/-- **The Remark on p. 826, for two binary partitions.** Jeffrey independence holds
iff each update is trivial or the covariance is zero:
`(p = α ∨ c = 0) ∧ (q = β ∨ c = 0)`. So for any nontrivial target (`p ≠ α`),
J-independence is P-independence (`c = 0`). -/
theorem remark826 {α β c : ℝ} (hα0 : 0 < α) (hα1 : α < 1) (hβ0 : 0 < β) (hβ1 : β < 1)
    (p q : ℝ) :
    JIndep (P2 α β c) Prod.fst (bin p) Prod.snd (bin q) ↔
      (p = α ∨ c = 0) ∧ (q = β ∨ c = 0) := by
  have ha : α * (1 - α) ≠ 0 := by have : 0 < 1 - α := by linarith
                                  positivity
  have hb : β * (1 - β) ≠ 0 := by have : 0 < 1 - β := by linarith
                                  positivity
  simp only [JIndep, marg_jE_snd hα0.ne' (by linarith : 1 - α ≠ 0),
    marg_jF_fst hβ0.ne' (by linarith : 1 - β ≠ 0), marg_P2_fst, marg_P2_snd]
  have e1 : (p - α) * c / (α * (1 - α)) = 0 ↔ (p = α ∨ c = 0) := by
    rw [div_eq_zero_iff, mul_eq_zero, sub_eq_zero]; simp [ha]
  have e2 : (q - β) * c / (β * (1 - β)) = 0 ↔ (q = β ∨ c = 0) := by
    rw [div_eq_zero_iff, mul_eq_zero, sub_eq_zero]; simp [hb]
  rw [← e1, ← e2]
  constructor
  · rintro ⟨h1, h2⟩
    have := h1 true; have := h2 true
    constructor <;> simp_all
  · rintro ⟨h1, h2⟩
    refine ⟨fun b => ?_, fun b => ?_⟩ <;> simp [h1, h2]

/-- **Theorem 3.2 on the `2 × 2` table.** For a full-support prior and nondegenerate
targets, the two orders of Jeffrey updating agree iff
`(p = α ∨ c = 0) ∧ (q = β ∨ c = 0)`. -/
theorem thm32_2x2 {α β c p q : ℝ} (hP : ∀ ω, 0 < P2 α β c ω) (hp0 : 0 < p) (hp1 : p < 1)
    (hq0 : 0 < q) (hq1 : q < 1) :
    PEF (P2 α β c) Prod.fst (bin p) Prod.snd (bin q) =
        PFE (P2 α β c) Prod.fst (bin p) Prod.snd (bin q) ↔
      (p = α ∨ c = 0) ∧ (q = β ∨ c = 0) := by
  have hm1 := marg_pos hP Prod.fst (true, true)
  have hm0 := marg_pos hP Prod.fst (false, true)
  have hn1 := marg_pos hP Prod.snd (true, true)
  have hn0 := marg_pos hP Prod.snd (true, false)
  simp only [marg_P2_fst, marg_P2_snd, bin, ↓reduceIte, Bool.false_eq_true] at hm1 hm0 hn1 hn0
  rw [thm32 hP (by simp [Fintype.sum_prod_type, P2]; ring)
    (fun b => by cases b <;> simp [bin] <;> linarith) (by simp [bin])
    (fun b => by cases b <;> simp [bin] <;> linarith) (by simp [bin])
    (fun i j => ⟨(i, j), rfl, rfl⟩)]
  exact remark826 hm1 (by linarith) hn1 (by linarith) p q

/-- **The `c = 0` case:** with an independent prior the two orders agree for all targets. -/
theorem commute_of_indep {α β p q : ℝ} (hα0 : 0 < α) (hα1 : α < 1) (hβ0 : 0 < β)
    (hβ1 : β < 1) (hp0 : 0 < p) (hp1 : p < 1) (hq0 : 0 < q) (hq1 : q < 1) :
    PEF (P2 α β 0) Prod.fst (bin p) Prod.snd (bin q) =
      PFE (P2 α β 0) Prod.fst (bin p) Prod.snd (bin q) := by
  have hP : ∀ ω, 0 < P2 α β 0 ω := by
    have : 0 < 1 - α := by linarith
    have : 0 < 1 - β := by linarith
    rintro ⟨_ | _, _ | _⟩ <;> simp [P2] <;> positivity
  exact (thm32_2x2 hP hp0 hp1 hq0 hq1).2 ⟨Or.inr rfl, Or.inr rfl⟩

/-- **With a correlated prior (`c ≠ 0`) and a nontrivial target, the orders differ.** -/
theorem not_commute_of_corr {α β c p q : ℝ} (hP : ∀ ω, 0 < P2 α β c ω) (hc : c ≠ 0)
    (hp0 : 0 < p) (hp1 : p < 1) (hq0 : 0 < q) (hq1 : q < 1) (hpα : p ≠ α) :
    PEF (P2 α β c) Prod.fst (bin p) Prod.snd (bin q) ≠
      PFE (P2 α β c) Prod.fst (bin p) Prod.snd (bin q) := fun h => by
  have := ((thm32_2x2 hP hp0 hp1 hq0 hq1).1 h).1
  tauto

/-- The numeric witness used in the project's SymPy record (a Paper B computation, not
a number from D-Z): at `(α, β, c, p, q) = (.3, .55, .05, .4, .6)` the two orders differ
in cell `E F` by `-1032/369935`. -/
theorem witness_gap :
    PEF (P2 (3 / 10) (11 / 20) (1 / 20)) Prod.fst (bin (2 / 5)) Prod.snd (bin (3 / 5)) (true, true) -
      PFE (P2 (3 / 10) (11 / 20) (1 / 20)) Prod.fst (bin (2 / 5)) Prod.snd (bin (3 / 5))
        (true, true) = -1032 / 369935 := by
  simp only [PEF, PFE, jeffrey_apply, marg_fst, marg_snd]
  simp [P2, bin]; norm_num

/-! ### D-Z's examples in Section 3 -/

/-- **Example 3.2** (p. 825). Four suspects, uniform prior; `E₁ = {a, b}` left-handed
goes to `.8`, then `F₁ = {a, c}` female goes to `.7`. Here `a = (L, W)`, `b = (L, M)`,
`c = (R, W)`, `d = (R, M)`. `P_𝓔𝓕 = (.56, .24, .14, .06)`, and the order does not
matter (the prior is independent). -/
theorem example32 :
    let P := P2 (1 / 2) (1 / 2) 0
    let R := PEF P Prod.fst (bin (8 / 10)) Prod.snd (bin (7 / 10))
    R (true, true) = 56 / 100 ∧ R (true, false) = 24 / 100 ∧ R (false, true) = 14 / 100 ∧
      R (false, false) = 6 / 100 ∧ R = PFE P Prod.fst (bin (8 / 10)) Prod.snd (bin (7 / 10)) := by
  intro P R
  refine ⟨?_, ?_, ?_, ?_, commute_of_indep (by norm_num) (by norm_num) (by norm_num)
    (by norm_num) (by norm_num) (by norm_num) (by norm_num) (by norm_num)⟩ <;>
  · simp only [R, P, PEF, jeffrey_apply, marg_fst, marg_snd]
    simp [P2, bin]; norm_num

/-- The prior of **Example 3.4** (p. 826): `P(E F) = 1/8`, `P(E F̄) = 1/4`,
`P(Ē F) = 3/8`, `P(Ē F̄) = 1/4`. -/
def P34 : Bool × Bool → ℝ
  | (true, true) => 1 / 8
  | (true, false) => 1 / 4
  | (false, true) => 3 / 8
  | (false, false) => 1 / 4

/-- **Example 3.4** (p. 826), `p₁ = p₂ = 1/2`, `q₁ = 7/15`, `q₂ = 8/15`.
`P_𝓔𝓕(E) = 1/2` (so `P_𝓔𝓕` incorporates both targets, (4.1)), but
`P_𝓕𝓔(F) = 371/851 ≠ 7/15 = q₁`; hence `P_𝓔𝓕 ≠ P_𝓕𝓔`. D-Z say "`P_𝓕𝓔(F) ≠ q₁`";
the value `371/851` is computed here, it is not printed in the paper. -/
theorem example34 :
    let R₁ := PEF P34 Prod.fst (bin (1 / 2)) Prod.snd (bin (7 / 15))
    let R₂ := PFE P34 Prod.fst (bin (1 / 2)) Prod.snd (bin (7 / 15))
    marg R₁ Prod.fst true = 1 / 2 ∧ marg R₁ Prod.snd true = 7 / 15 ∧
      marg R₂ Prod.fst true = 1 / 2 ∧ marg R₂ Prod.snd true = 371 / 851 ∧
      (371 / 851 : ℝ) ≠ 7 / 15 ∧ R₁ ≠ R₂ := by
  intro R₁ R₂
  have h1 : marg R₁ Prod.fst true = 1 / 2 := by
    simp only [R₁, PEF, jeffrey_apply, marg_fst, marg_snd]; simp [P34, bin]; norm_num
  have h2 : marg R₁ Prod.snd true = 7 / 15 := by
    simp only [R₁, PEF, jeffrey_apply, marg_fst, marg_snd]; simp [P34, bin]; norm_num
  have h3 : marg R₂ Prod.fst true = 1 / 2 := by
    simp only [R₂, PFE, jeffrey_apply, marg_fst, marg_snd]; simp [P34, bin]; norm_num
  have h4 : marg R₂ Prod.snd true = 371 / 851 := by
    simp only [R₂, PFE, jeffrey_apply, marg_fst, marg_snd]; simp [P34, bin]; norm_num
  refine ⟨h1, h2, h3, h4, by norm_num, fun h => ?_⟩
  rw [h] at h2
  rw [h2] at h4
  norm_num at h4

/-- `marg` on a product space, first coordinate. -/
theorem marg_prod_fst {α β : Type*} [Fintype α] [Fintype β] [DecidableEq α] [DecidableEq β]
    (X : α × β → ℝ) (a : α) : marg X Prod.fst a = ∑ b, X (a, b) := by
  simp only [marg, mass, cellSet, Finset.sum_filter, Fintype.sum_prod_type]
  rw [Finset.sum_eq_single a (fun a' _ h => by simp [h]) (by simp)]
  simp

/-- `marg` on a product space, second coordinate. -/
theorem marg_prod_snd {α β : Type*} [Fintype α] [Fintype β] [DecidableEq α] [DecidableEq β]
    (X : α × β → ℝ) (b : β) : marg X Prod.snd b = ∑ a, X (a, b) := by
  simp only [marg, mass, cellSet, Finset.sum_filter, Fintype.sum_prod_type]
  refine Finset.sum_congr rfl fun a _ => ?_
  rw [Finset.sum_eq_single b (fun b' _ h => by simp [h]) (by simp)]
  simp

/-- The `3 × 3` table of **Example 3.3** (p. 826). -/
def P33 : Fin 3 × Fin 3 → ℝ := fun ω =>
  ![![1 / 4, 1 / 8, 1 / 8], ![1 / 8, 0, 1 / 8], ![1 / 8, 1 / 8, 0]] ω.1 ω.2

/-- The target family of Example 3.3: `(x, (1-x)/2, (1-x)/2)`. -/
def pv (x : ℝ) : Fin 3 → ℝ := ![x, (1 - x) / 2, (1 - x) / 2]

/-- **Example 3.3 (p. 826): J-independence does not imply P-independence.** For the
`3 × 3` table and targets `p = (p, (1-p)/2, (1-p)/2)`, `q = (q, (1-q)/2, (1-q)/2)`, the
partitions are Jeffrey independent for every `p, q`, yet not P-independent
(`P(E₂ F₂) = 0 ≠ 1/16`). -/
theorem example33 (x y : ℝ) :
    JIndep P33 Prod.fst (pv x) Prod.snd (pv y) ∧ ¬ PIndep P33 Prod.fst Prod.snd := by
  refine ⟨⟨fun j => ?_, fun i => ?_⟩, fun h => ?_⟩
  · simp only [marg_prod_snd, jeffrey_apply, marg_prod_fst]
    fin_cases j <;> simp [Fin.sum_univ_three, P33, pv] <;> ring
  · simp only [marg_prod_fst, jeffrey_apply, marg_prod_snd]
    fin_cases i <;> simp [Fin.sum_univ_three, P33, pv] <;> ring
  · have := h 1 1
    have hc : cellSet (Prod.fst : Fin 3 × Fin 3 → Fin 3) 1 ∩ cellSet Prod.snd 1 = {(1, 1)} := by
      ext ⟨a, b⟩; simp [cellSet, Prod.ext_iff]
    rw [hc, marg_prod_fst, marg_prod_snd] at this
    simp [mass, Fin.sum_univ_three, P33] at this

end Basic

end

end Literature.DiaconisZabell
