import Mathlib

/-! Peng, Zhang and Pan (2010), "Bayesian network reasoning with uncertain
evidences", the structure Paper A1 relies on, in the paper's notation, on a
finite set of joint states `X` (here `Ω`). `cell` sends each joint state `x`
to its instantiation `y` of the evidence variables `Y`, so `P(y)` is
`cellMass P cell y`.

* Eq. (2), Jeffrey's rule, `Q(x) = P(x) R(y)/P(y)` (`jeffrey`).
* Sec. 3.1, the virtual evidence method. A likelihood ratio `L(Y)` on the
  instantiations of `Y` is applied as hard evidence on a virtual child node,
  which is Bayes' rule with likelihood `L(y)`, `Q(x) ∝ P(x) L(y)` (`virtual`).
* Eq. (9), the conversion of a soft evidence `R(Y)` into the likelihood ratio
  `L(y) = R(y)/P(y)`.
* Eq. (4), the total variation, and Theorem 2.
* Example 1 (Fig. 1, Table 1), on the variables `A, B, C`; `D` is a child of
  `B` and `C` and sums out of every marginal used. -/

namespace PengEtAl2010

open Finset

variable {Ω ι : Type*} [Fintype Ω] [Fintype ι] [DecidableEq ι]

/-- `P(y)`, the marginal of `P` at instantiation `i` of `Y`. -/
def cellMass (P : Ω → ℝ) (cell : Ω → ι) (i : ι) : ℝ :=
  ∑ x, if cell x = i then P x else 0

/-- Eq. (2), Jeffrey's rule for the soft evidence `R(Y)`. -/
noncomputable def jeffrey (P : Ω → ℝ) (cell : Ω → ι) (R : ι → ℝ) (x : Ω) : ℝ :=
  P x * R (cell x) / cellMass P cell (cell x)

/-- Sec. 3.1, the virtual evidence method, Bayes' rule with the likelihood
`L(y)` of the virtual observation. -/
noncomputable def virtual (P : Ω → ℝ) (cell : Ω → ι) (L : ι → ℝ) (x : Ω) : ℝ :=
  P x * L (cell x) / ∑ x', P x' * L (cell x')

/-- Eq. (4), the total variation. -/
def tv (P Q : Ω → ℝ) : ℝ := ∑ x, |P x - Q x|

/-- A sum over joint states is a sum over instantiations of `Y` of the sums
within each. -/
theorem sum_by_cell (cell : Ω → ι) (f : Ω → ℝ) :
    ∑ x, f x = ∑ i, ∑ x, if cell x = i then f x else 0 := by
  rw [Finset.sum_comm]
  refine Finset.sum_congr rfl (fun x _ => ?_)
  rw [Finset.sum_ite_eq]; simp

/-- A function of `y` summed against `P` is summed against the marginal `P(y)`. -/
theorem sum_cell (P : Ω → ℝ) (cell : Ω → ι) (g : ι → ℝ) :
    ∑ x, P x * g (cell x) = ∑ i, cellMass P cell i * g i := by
  rw [sum_by_cell cell]
  refine Finset.sum_congr rfl (fun i _ => ?_)
  unfold cellMass
  rw [Finset.sum_mul]
  refine Finset.sum_congr rfl (fun x _ => ?_)
  split_ifs with h
  · rw [h]
  · ring

omit [Fintype ι] in
/-- Jeffrey's rule gives `Y` the marginal `R(Y)`, `Q(Y) = R(Y)`. -/
theorem jeffrey_marginal {P : Ω → ℝ} {cell : Ω → ι} {R : ι → ℝ} {i : ι}
    (h : cellMass P cell i ≠ 0) : cellMass (jeffrey P cell R) cell i = R i := by
  unfold cellMass jeffrey
  have e : ∀ x, (if cell x = i then P x * R (cell x) / cellMass P cell (cell x) else 0)
      = R i / cellMass P cell i * (if cell x = i then P x else 0) := by
    intro x; split_ifs with hx
    · rw [hx]; ring
    · ring
  rw [Finset.sum_congr rfl (fun x _ => e x), ← Finset.mul_sum]
  change R i / cellMass P cell i * cellMass P cell i = R i
  field_simp

/-- Eq. (9) and the result the paper cites as Theorem 5 of its Ref. 3. The
soft evidence `R(Y)` applied by Jeffrey's rule and the virtual evidence
`L(y) = R(y)/P(y)` applied by Bayes' rule give the same distribution. -/
theorem eq9_soft_eq_virtual {P : Ω → ℝ} {cell : Ω → ι} {R : ι → ℝ}
    (hP : ∀ i, cellMass P cell i ≠ 0) (hR : ∑ i, R i = 1) :
    virtual P cell (fun i => R i / cellMass P cell i) = jeffrey P cell R := by
  funext x
  unfold virtual jeffrey
  have hZ : ∑ x', P x' * (R (cell x') / cellMass P cell (cell x')) = 1 := by
    rw [sum_cell P cell (fun i => R i / cellMass P cell i)]
    rw [← hR]
    refine Finset.sum_congr rfl (fun i _ => ?_)
    field_simp [hP i]
  rw [hZ, div_one]; ring

omit [Fintype ι] in
/-- The converse. Every virtual evidence applied by Bayes' rule is Jeffrey's
rule for the soft evidence `R(y) = L(y) P(y)/Σ L P`, so in a single update
neither rule reaches a distribution the other cannot. -/
theorem virtual_eq_jeffrey {P : Ω → ℝ} {cell : Ω → ι} {L : ι → ℝ}
    (hP : ∀ i, cellMass P cell i ≠ 0) (hZ : ∑ x', P x' * L (cell x') ≠ 0) :
    virtual P cell L
      = jeffrey P cell (fun i => L i * cellMass P cell i / ∑ x', P x' * L (cell x')) := by
  funext x
  unfold virtual jeffrey
  have := hP (cell x)
  field_simp

omit [Fintype ι] [DecidableEq ι] in
/-- Sec. 3.1. A virtual evidence is a likelihood ratio, given up to scale, and
the update does not depend on the scale. -/
theorem virtual_scale {P : Ω → ℝ} {cell : Ω → ι} {L : ι → ℝ} {a : ℝ} (ha : a ≠ 0) :
    virtual P cell (fun i => a * L i) = virtual P cell L := by
  funext x
  unfold virtual
  have e : ∀ y, P y * (a * L (cell y)) = a * (P y * L (cell y)) := fun y => by ring
  simp_rw [e, ← Finset.mul_sum]
  rw [mul_div_mul_left _ _ ha]

/-- The soft evidence that the converse produces is a distribution on `Y`. -/
theorem virtual_soft_sums_to_one {P : Ω → ℝ} {cell : Ω → ι} {L : ι → ℝ}
    (hZ : ∑ x', P x' * L (cell x') ≠ 0) :
    ∑ i, L i * cellMass P cell i / ∑ x', P x' * L (cell x') = 1 := by
  rw [← Finset.sum_div, div_eq_one_iff_eq hZ, sum_cell P cell L]
  refine Finset.sum_congr rfl (fun i _ => ?_); ring

omit [Fintype ι] [DecidableEq ι] in
/-- Two virtual evidences commute, as Table 1, rows 4 and 5, shows for
Example 1. -/
theorem virtual_commute {P : Ω → ℝ} {c₁ c₂ : Ω → ι} {L₁ L₂ : ι → ℝ}
    (h₁ : ∑ x, P x * L₁ (c₁ x) ≠ 0) (h₂ : ∑ x, P x * L₂ (c₂ x) ≠ 0) :
    virtual (virtual P c₁ L₁) c₂ L₂ = virtual (virtual P c₂ L₂) c₁ L₁ := by
  funext x
  unfold virtual
  simp_rw [div_mul_eq_mul_div, ← Finset.sum_div]
  rw [div_div_div_cancel_right₀ h₁, div_div_div_cancel_right₀ h₂]
  congr 1
  · ring
  · exact Finset.sum_congr rfl (fun y _ => by ring)

/-- Theorem 2. Jeffrey's rule (the I-projection, eq. 6) is at total variation
`Σ_y |P(y) − R(y)|` from `P`, and no distribution with marginal `R(Y)` is
closer. -/
theorem theorem2 {P Q : Ω → ℝ} {cell : Ω → ι} {R : ι → ℝ} (hP0 : ∀ x, 0 ≤ P x)
    (hP : ∀ i, 0 < cellMass P cell i) (hQ : ∀ i, cellMass Q cell i = R i) :
    tv P (jeffrey P cell R) = ∑ i, |cellMass P cell i - R i| ∧
      tv P (jeffrey P cell R) ≤ tv P Q := by
  have hval : tv P (jeffrey P cell R) = ∑ i, |cellMass P cell i - R i| := by
    unfold tv jeffrey
    have e : ∀ x, |P x - P x * R (cell x) / cellMass P cell (cell x)|
        = P x * |1 - R (cell x) / cellMass P cell (cell x)| := by
      intro x
      rw [show P x - P x * R (cell x) / cellMass P cell (cell x)
          = P x * (1 - R (cell x) / cellMass P cell (cell x)) by ring, abs_mul, abs_of_nonneg (hP0 x)]
    rw [Finset.sum_congr rfl (fun x _ => e x),
      sum_cell P cell (fun i => |1 - R i / cellMass P cell i|)]
    refine Finset.sum_congr rfl (fun i _ => ?_)
    have hi := hP i
    rw [← abs_of_pos hi, ← abs_mul, abs_of_pos hi]
    congr 1
    field_simp
  refine ⟨hval, ?_⟩
  rw [hval]
  unfold tv
  rw [sum_by_cell cell (fun x => |P x - Q x|)]
  refine Finset.sum_le_sum (fun i _ => ?_)
  rw [← hQ i]
  unfold cellMass
  rw [← Finset.sum_sub_distrib]
  refine le_trans (Finset.abs_sum_le_sum_abs _ _) (le_of_eq ?_)
  refine Finset.sum_congr rfl (fun x _ => ?_)
  split_ifs <;> simp

omit [Fintype ι] in
/-- Control: the dominance condition `R(Y) << P(Y)` is needed. Where `P(y) = 0`
and `R(y) > 0`, Jeffrey's rule cannot give `y` its new probability. -/
theorem control_dominance_needed {P : Ω → ℝ} {cell : Ω → ι} {R : ι → ℝ} {i : ι}
    (h0 : cellMass P cell i = 0) (hR : 0 < R i) :
    cellMass (jeffrey P cell R) cell i ≠ R i := by
  have : cellMass (jeffrey P cell R) cell i = 0 := by
    unfold cellMass jeffrey
    refine Finset.sum_eq_zero (fun x _ => ?_)
    split_ifs with hx
    · rw [hx, h0, div_zero]
    · rfl
  rw [this]; exact hR.ne

/-! ### Example 1

Joint states of `(A, B, C)` in the order `111, 110, 101, 100, 011, 010, 001,
000`, with `P(A=1) = .4`, `P(B=1|A) = .2, .6` and `P(C=1|A) = .6, .35` for
`A = 1, 0` (Fig. 1). Instantiation `0` of `cellB` is `B = 1`, and likewise for
`C`. -/

/-- The joint of `A, B, C` from Fig. 1. -/
noncomputable def J : Fin 8 → ℝ :=
  ![48 / 1000, 32 / 1000, 192 / 1000, 128 / 1000, 126 / 1000, 234 / 1000, 84 / 1000, 156 / 1000]

def cellB : Fin 8 → Fin 2 := ![0, 0, 1, 1, 0, 0, 1, 1]
def cellC : Fin 8 → Fin 2 := ![0, 1, 0, 1, 0, 1, 0, 1]

/-- The soft evidences `se1: R(B) = (0.7, 0.3)` and `se2: R(C) = (0.3, 0.7)`. -/
noncomputable def RB : Fin 2 → ℝ := ![7 / 10, 3 / 10]
noncomputable def RC : Fin 2 → ℝ := ![3 / 10, 7 / 10]

/-- Example 1. `P(B) = (0.44, 0.56)` and `P(C) = (0.45, 0.55)`. -/
theorem ex1_marginals :
    cellMass J cellB 0 = 44 / 100 ∧ cellMass J cellB 1 = 56 / 100 ∧
      cellMass J cellC 0 = 45 / 100 ∧ cellMass J cellC 1 = 55 / 100 := by
  simp [cellMass, Fin.sum_univ_eight, J, cellB, cellC]
  norm_num

/-- Table 1, row 2. Applying `se1` alone moves the belief on `C = 1` to
`327/770`, which the table prints as `0.425`. -/
theorem ex1_row2 :
    cellMass (jeffrey J cellB RB) cellC 0 = 327 / 770 ∧
      |(327 / 770 : ℝ) - 425 / 1000| < 5 / 10000 := by
  constructor
  · simp [cellMass, jeffrey, Fin.sum_univ_eight, J, cellB, cellC, RB]
    norm_num
  · rw [abs_lt]; constructor <;> norm_num

/-- Control, Table 1, row 4. The two soft evidences converted by eq. (9) and
applied together as virtual evidences give `B = 1` the belief `245/344`
(printed `0.712`), not the `0.7` that `se1` requires. Multiple soft evidences
are not multiple virtual evidences. -/
theorem control_two_virtual_not_soft :
    cellMass (virtual (virtual J cellB (fun i => RB i / cellMass J cellB i)) cellC
        (fun i => RC i / cellMass J cellC i)) cellB 0 = 245 / 344 ∧
      (245 / 344 : ℝ) ≠ 7 / 10 := by
  constructor
  · simp [cellMass, virtual, Fin.sum_univ_eight, J, cellB, cellC, RB, RC]
    norm_num
  · norm_num

end PengEtAl2010
