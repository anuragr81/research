/-
# Jeffrey, *The Logic of Decision* (2nd ed., 1983), ch. 11, and
# *Subjective Probability: The Real Thing* (draft of 4 Nov 2002), ch. 3

Formalization of Jeffrey's own formal claims about probability kinematics that
are not already in `JeffreyOrder/` (which works on the 2x2 table only) or in
`Literature/Doring.lean` (which already has rigidity, reversibility, "the second
assignment on one partition replaces the first", and commutativity of Field's
factor rule).  Page numbers are the printed pages of each book.

## What is formalized

* **(11-5), the relevance identity** (1983, p. 170):
  `PROB A - prob A = (PROB B - prob B) rel(A/B)` with
  `rel(A/B) = prob(A/B) - prob(A/B̄)` (`relevance_identity`), and the mudrunner
  numbers of Examples 3-4 (pp. 169-170: `.52`, `.31`, and `.21 = .7 × .3`,
  `mudrunner`).
* **Kinematics fixes the delivered marginal** (1983 (11-7)/(11-8), p. 173-174;
  2004 §3.2, p. 58): `cellMass_kin`, and the update has total mass `Σ q`
  (`total_kin`).
* **Commutativity when the partitions are independent** (1983, p. 183: "This
  cannot happen if the `A_i` and `B_j` are independent relative to the initial
  probability function"; 2004 §3.4.1, p. 62: order is immaterial "when the two
  partitions are independent relative to your old").  For any finite space and
  any two finite partitions: a kinematical update on one partition leaves the
  other partition's marginal unchanged (`cellMass_kin_of_indep`), and the two
  orders agree (`kin_comm_of_indep`).  `JeffreyOrder` has only the 2x2, `c = 0`
  instance.
* **Factor updates** (2004 §3.3-§3.4.2, pp. 60-63).  Updating by factors on two
  partitions, in either order, is "equivalent to a single mapping ... with
  partition `{D'_i ∧ D''_j}` and factors `f'_i f''_j`" (p. 63):
  `fac_fac_eq_product`, hence `fac_comm`.  Factors matter only up to a constant
  (p. 61: "any fixed diagnosis `D_k` would do as well as `D_1`"; formulas (1)-(3)
  "remain valid with anchored Bayes factors in place of probability factors"):
  `fac_smul`.
* **One step, two parametrizations.**  A single factor update is the Jeffrey
  update whose delivered credences are `w_i old(D_i) / Σ w old` (2004 §3.3,
  formula (2), p. 60: `new(H) = Σ π(D_i) old(H ∧ D_i)`): `fac_eq_kin`.
  Conversely a Jeffrey update is the factor update with the probability factors
  `π_i = new(D_i)/old(D_i)` (p. 57 (3)): `kin_eq_fac`.  So the credence reading
  and the factor reading differ only in what is held fixed when an input is
  reused, never in a single step.
* **Kinematics as conditioning on a richer space** (2004 §3.5, pp. 64-65,
  after Skyrms 1980): if (1) `old(D_i | ℰ) = d_i` and (2)
  `old(H | D_i ∧ ℰ) = old(H | D_i)`, then (3)
  `old(H | ℰ) = Σ d_i old(H | D_i)` (`skyrms_three`); and for every prior, cue
  partition and delivered credences there is an expanded assignment on
  `Ω × Bool` whose `Ω`-marginal is the prior and whose conditional on the
  softcore proposition `ℰ` is the Jeffrey update (`expansion`).
* **The draft's worked numbers** (2004, pp. 58-60): Example 4's `41/60`
  (`example4`), and Example 5.  As printed, Example 5's "your" factors
  `π(D_i) = 4/3, 2/3, 2` do not satisfy the normalization of formula (1): with
  `old(D_i) = 1/4, 1/2, 1/4` and `π'(D_i) = 1, 1/2, 3/2` formula (1) gives
  `8/7, 4/7, 12/7`, and then `new(H) = 3/7`, not the printed `1/2`
  (`example5_printed`, `example5_normalized`).  This is an arithmetic slip in
  the 2002 draft; the published 2004 text was not available to check.
-/
import Mathlib

namespace Literature.Jeffrey

open Finset Classical

/-! ## 1983, section 11.4: relevance -/

/-- (11-5), p. 170.  If `prob A = x prob B + y (1 - prob B)` and the change
originates in `B`, so that `PROB A = x PROB B + y (1 - PROB B)` with the same
conditionals `x = prob(A/B)`, `y = prob(A/B̄)` (11-1), then
`PROB A - prob A = (PROB B - prob B) rel(A/B)`. -/
theorem relevance_identity (x y pB PB : ℝ) :
    (x * PB + y * (1 - PB)) - (x * pB + y * (1 - pB)) = (PB - pB) * (x - y) := by
  ring

/-- The mudrunner, examples 3-4 (pp. 169-170): `(.8)(.6) + (.1)(.4) = .52`,
`(.8)(.3) + (.1)(.7) = .31`, and the increase `.21` is seven tenths of `.3`. -/
theorem mudrunner :
    (8/10 : ℝ) * (6/10) + (1/10) * (4/10) = 52/100 ∧
    (8/10 : ℝ) * (3/10) + (1/10) * (7/10) = 31/100 ∧
    (52/100 : ℝ) - 31/100 = (6/10 - 3/10) * (8/10 - 1/10) := by
  norm_num

/-! ## Kinematics on a finite space -/

variable {Ω ι κ : Type*} [Fintype Ω]

/-- Prior mass of the cell `u = i`. -/
noncomputable def cellMass (p : Ω → ℝ) (u : Ω → ι) (i : ι) : ℝ :=
  ∑ y, if u y = i then p y else 0

/-- Probability kinematics, (11-7) on atoms / 2004 p. 58:
`new(x) = new(D_{u x}) · old(x) / old(D_{u x})`. -/
noncomputable def kin (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ) : Ω → ℝ :=
  fun x => q (u x) * p x / cellMass p u (u x)

/-- Summing over the space is summing over the cells of a partition. -/
theorem sum_fiber [Fintype ι] (u : Ω → ι) (h : Ω → ℝ) :
    ∑ y, h y = ∑ i, ∑ y, if u y = i then h y else 0 := by
  rw [Finset.sum_comm]
  refine Finset.sum_congr rfl fun y _ => ?_
  simp

/-- The delivered credences become the new cell probabilities. -/
theorem cellMass_kin (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ) (i : ι)
    (h : cellMass p u i ≠ 0) : cellMass (kin p u q) u i = q i := by
  have key : cellMass (kin p u q) u i = q i / cellMass p u i * cellMass p u i := by
    conv_rhs => rw [cellMass, Finset.mul_sum]
    unfold cellMass kin
    refine Finset.sum_congr rfl fun y _ => ?_
    by_cases hy : u y = i
    · simp only [hy, if_true]; unfold cellMass; ring
    · simp [hy]
  rw [key]; field_simp

/-- The update is normalized when the delivered credences are. -/
theorem total_kin [Fintype ι] (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ)
    (hm : ∀ i, cellMass p u i ≠ 0) : ∑ x, kin p u q x = ∑ i, q i := by
  rw [sum_fiber u]
  exact Finset.sum_congr rfl fun i _ => cellMass_kin p u q i (hm i)

/-- The two partitions `u`, `v` are independent relative to `p`. -/
def Indep (p : Ω → ℝ) (u : Ω → ι) (v : Ω → κ) : Prop :=
  ∀ i j, (∑ y, if u y = i ∧ v y = j then p y else 0) = cellMass p u i * cellMass p v j

theorem Indep.symm {p : Ω → ℝ} {u : Ω → ι} {v : Ω → κ} (h : Indep p u v) : Indep p v u := by
  intro j i
  rw [mul_comm, ← h i j]
  refine Finset.sum_congr rfl fun y _ => ?_
  simp only [and_comm]

/-- 1983 p. 183 / 2004 §3.4.1 p. 62: when the partitions are independent,
updating on the first leaves the probabilities of the second unchanged. -/
theorem cellMass_kin_of_indep [Fintype ι] (p : Ω → ℝ) (u : Ω → ι) (v : Ω → κ)
    (q : ι → ℝ) (hq : ∑ i, q i = 1) (hm : ∀ i, cellMass p u i ≠ 0)
    (hind : Indep p u v) (j : κ) :
    cellMass (kin p u q) v j = cellMass p v j := by
  unfold cellMass
  rw [sum_fiber u]
  have step : ∀ i, (∑ y, if u y = i then (if v y = j then kin p u q y else 0) else 0) =
      q i / cellMass p u i * (∑ y, if u y = i ∧ v y = j then p y else 0) := by
    intro i
    rw [Finset.mul_sum]
    refine Finset.sum_congr rfl fun y _ => ?_
    by_cases hu : u y = i
    · by_cases hv : v y = j
      · simp only [hu, hv, and_self, if_true, kin]; ring
      · simp [hu, hv]
    · simp [hu]
  rw [Finset.sum_congr rfl fun i _ => step i]
  have step2 : ∀ i, q i / cellMass p u i * (∑ y, if u y = i ∧ v y = j then p y else 0) =
      q i * cellMass p v j := by
    intro i
    rw [hind i j]
    have := hm i
    field_simp
  rw [Finset.sum_congr rfl fun i _ => step2 i, ← Finset.sum_mul, hq, one_mul]
  rfl

/-- **Commutativity under independence** (1983, p. 183; 2004 §3.4.1, p. 62).
For any two finite partitions independent relative to the prior, the two
orders of kinematical updating agree. -/
theorem kin_comm_of_indep [Fintype ι] [Fintype κ] (p : Ω → ℝ) (u : Ω → ι) (v : Ω → κ)
    (q : ι → ℝ) (r : κ → ℝ) (hq : ∑ i, q i = 1) (hr : ∑ j, r j = 1)
    (hmu : ∀ i, cellMass p u i ≠ 0) (hmv : ∀ j, cellMass p v j ≠ 0)
    (hind : Indep p u v) :
    kin (kin p u q) v r = kin (kin p v r) u q := by
  funext x
  have h1 := cellMass_kin_of_indep p u v q hq hmu hind (v x)
  have h2 := cellMass_kin_of_indep p v u r hr hmv hind.symm (u x)
  show r (v x) * kin p u q x / cellMass (kin p u q) v (v x) =
    q (u x) * kin p v r x / cellMass (kin p v r) u (u x)
  rw [h1, h2]
  unfold kin
  have := hmu (u x); have := hmv (v x)
  field_simp

/-! ## Factor updates (2004 §3.3-§3.4.2) -/

/-- Updating by factors `w` on the partition `u`, normalized
(2004 p. 60 (1)-(2), p. 62). -/
noncomputable def fac (p : Ω → ℝ) (u : Ω → ι) (w : ι → ℝ) : Ω → ℝ :=
  fun x => w (u x) * p x / ∑ y, w (u y) * p y

/-- 2004 p. 63: two factor updates, in sequence, are "equivalent to a single
mapping ... with partition `{D'_i ∧ D''_j}` and factors `f'_i f''_j`". -/
theorem fac_fac_eq_product (p : Ω → ℝ) (u : Ω → ι) (v : Ω → κ) (w : ι → ℝ) (z : κ → ℝ)
    (hw : ∑ y, w (u y) * p y ≠ 0) :
    fac (fac p u w) v z = fac p (fun y => (u y, v y)) (fun ij => w ij.1 * z ij.2) := by
  funext x
  unfold fac
  have h1 : ∑ y, z (v y) * (w (u y) * p y / ∑ y', w (u y') * p y') =
      (∑ y, w (u y) * z (v y) * p y) / ∑ y', w (u y') * p y' := by
    rw [Finset.sum_div]; exact Finset.sum_congr rfl fun y _ => by ring
  rw [h1]
  by_cases hT : ∑ y, w (u y) * z (v y) * p y = 0
  · simp only [hT, zero_div, div_zero]
  · field_simp

/-- 2004 §3.4.2, p. 62: "In updating by Bayes or probability factors ... order
cannot matter." -/
theorem fac_comm (p : Ω → ℝ) (u : Ω → ι) (v : Ω → κ) (w : ι → ℝ) (z : κ → ℝ)
    (hw : ∑ y, w (u y) * p y ≠ 0) (hz : ∑ y, z (v y) * p y ≠ 0) :
    fac (fac p u w) v z = fac (fac p v z) u w := by
  rw [fac_fac_eq_product p u v w z hw, fac_fac_eq_product p v u z w hz]
  funext x
  unfold fac
  have hs : ∑ y, z (v y) * w (u y) * p y = ∑ y, w (u y) * z (v y) * p y :=
    Finset.sum_congr rfl fun y _ => by ring
  simp only
  rw [hs, mul_comm (z (v x)) (w (u x))]

/-- 2004 p. 61: factors matter only up to a constant multiplier (the choice of
anchor `D_k` is arbitrary; Bayes factors may replace probability factors). -/
theorem fac_smul (p : Ω → ℝ) (u : Ω → ι) (w : ι → ℝ) (c : ℝ) (hc : c ≠ 0) :
    fac p u (fun i => c * w i) = fac p u w := by
  funext x
  unfold fac
  have hs : ∑ y, c * w (u y) * p y = c * ∑ y, w (u y) * p y := by
    rw [Finset.mul_sum]; exact Finset.sum_congr rfl fun y _ => by ring
  rw [hs]
  by_cases hS : ∑ y, w (u y) * p y = 0
  · simp [hS]
  · field_simp

/-- A factor update is the Jeffrey update whose delivered credences are
`w_i old(D_i) / Σ w old` (2004 p. 60, formula (2)). -/
theorem fac_eq_kin (p : Ω → ℝ) (u : Ω → ι) (w : ι → ℝ) (hm : ∀ i, cellMass p u i ≠ 0) :
    fac p u w = kin p u (fun i => w i * cellMass p u i / ∑ y, w (u y) * p y) := by
  funext x
  unfold fac kin
  have := hm (u x)
  by_cases hS : ∑ y, w (u y) * p y = 0
  · simp [hS]
  · field_simp

/-- A Jeffrey update is the factor update with the probability factors
`π_i = new(D_i)/old(D_i)` (2004 p. 57, (3)), when the delivered credences sum
to one. -/
theorem kin_eq_fac [Fintype ι] (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ)
    (hq : ∑ i, q i = 1) (hm : ∀ i, cellMass p u i ≠ 0) :
    kin p u q = fac p u (fun i => q i / cellMass p u i) := by
  have hS : ∑ y, q (u y) / cellMass p u (u y) * p y = 1 := by
    have : ∑ y, q (u y) / cellMass p u (u y) * p y = ∑ x, kin p u q x :=
      Finset.sum_congr rfl fun y _ => by unfold kin; ring
    rw [this, total_kin p u q hm, hq]
  funext x
  unfold fac
  rw [hS, div_one]
  unfold kin; ring

/-! ## 2004 §3.5: kinematics as conditioning on a richer space -/

/-- Mass of an event. -/
noncomputable def mass (Q : Ω → ℝ) (S : Ω → Prop) : ℝ :=
  ∑ y, if S y then Q y else 0

/-- 2004 p. 65, (1)-(3) (after Skyrms): if `old(D_i | ℰ) = d_i` and
`old(H | D_i ∧ ℰ) = old(H | D_i)`, then `old(H | ℰ) = Σ_i d_i old(H | D_i)`.
Condition (2) is stated cross-multiplied so that cells with `d_i = 0` are
allowed. -/
theorem skyrms_three [Fintype ι] (Q : Ω → ℝ) (u : Ω → ι) (E H : Ω → Prop) (d : ι → ℝ)
    (h1 : ∀ i, mass Q (fun y => u y = i ∧ E y) = d i * mass Q E)
    (h2 : ∀ i, mass Q (fun y => H y ∧ u y = i ∧ E y) =
      mass Q (fun y => u y = i ∧ E y) * (mass Q (fun y => H y ∧ u y = i) / mass Q (fun y => u y = i)))
    (hE : mass Q E ≠ 0) :
    mass Q (fun y => H y ∧ E y) / mass Q E =
      ∑ i, d i * (mass Q (fun y => H y ∧ u y = i) / mass Q (fun y => u y = i)) := by
  have split : mass Q (fun y => H y ∧ E y) = ∑ i, mass Q (fun y => H y ∧ u y = i ∧ E y) := by
    unfold mass
    rw [sum_fiber u]
    refine Finset.sum_congr rfl fun i _ => Finset.sum_congr rfl fun y _ => ?_
    by_cases hu : u y = i
    · simp [hu]
    · simp [hu]
  rw [split, Finset.sum_div]
  refine Finset.sum_congr rfl fun i _ => ?_
  rw [h2 i, h1 i]
  field_simp

/-- 2004 p. 64: "it is possible to expand the domain of the function `old` so as
to allow conditioning on `ℰ` in a way that yields the same result you would get
via probability kinematics."  The expanded assignment on `Ω × Bool` (second
coordinate: whether `ℰ` holds) has the prior as its `Ω`-marginal, and its
conditional on `ℰ` is the Jeffrey update.  It is non-negative whenever the
weight `t` on `ℰ` is small enough that `t · kin ≤ p`. -/
theorem expansion [Fintype ι] (p : Ω → ℝ) (u : Ω → ι) (d : ι → ℝ) (t : ℝ)
    (ht : t ≠ 0) (hd : ∑ i, d i = 1) (hm : ∀ i, cellMass p u i ≠ 0) :
    let Q : Ω × Bool → ℝ := fun z => if z.2 then t * kin p u d z.1 else p z.1 - t * kin p u d z.1
    (∀ x, Q (x, true) + Q (x, false) = p x) ∧
    (∀ x, Q (x, true) / (∑ x', Q (x', true)) = kin p u d x) ∧
    ((0 ≤ t ∧ ∀ x, 0 ≤ kin p u d x ∧ t * kin p u d x ≤ p x) → ∀ z, 0 ≤ Q z) := by
  intro Q
  refine ⟨fun x => by simp [Q], fun x => ?_, fun ⟨ht0, hx⟩ z => ?_⟩
  · have hsum : ∑ x', Q (x', true) = t := by
      simp only [Q, if_true]
      rw [← Finset.mul_sum, total_kin p u d hm, hd, mul_one]
    rw [hsum]
    simp only [Q, if_true]
    field_simp
  · obtain ⟨x, b⟩ := z
    have := hx x
    cases b
    · simp only [Q, Bool.false_eq_true, if_false]; linarith
    · simp only [Q, if_true]; exact mul_nonneg ht0 this.1

/-! ## 2004, the draft's worked examples (pp. 58-60) -/

/-- Example 4, p. 59: `new'(H) = 41/60`. -/
theorem example4 : (1/3 : ℝ) * (4/10) + (1/6) * (6/10) + (1/2) * (9/10) = 41/60 := by
  norm_num

/-- Example 5, p. 60, as printed: the stated `old(H ∧ D_i) = 3/16, 3/32, 3/32`
give `old(H) = 3/8`, and the printed factors `4/3, 2/3, 2` give `new(H) = 1/2`
and `π(H) = 4/3`; but those factors do not normalize against
`old(D_i) = 1/4, 1/2, 1/4` (the implied `new(D_i)` sum to `7/6`). -/
theorem example5_printed :
    (3/16 : ℝ) + 3/32 + 3/32 = 3/8 ∧
    (4/3 : ℝ) * (3/16) + (2/3) * (3/32) + 2 * (3/32) = 1/2 ∧
    (1/2 : ℝ) / (3/8) = 4/3 ∧
    (4/3 : ℝ) * (1/4) + (2/3) * (1/2) + 2 * (1/4) = 7/6 := by
  norm_num

/-- Example 5 with formula (1) applied as written: `π'(D_i) = 1, 1/2, 3/2`
(her priors `1/3` each), `Σ π' old = 7/8`, so `π(D_i) = 8/7, 4/7, 12/7`, the new
diagnostic probabilities sum to one, and `new(H) = 3/7`. -/
theorem example5_normalized :
    (1 : ℝ) * (1/4) + (1/2) * (1/2) + (3/2) * (1/4) = 7/8 ∧
    (8/7 : ℝ) * (1/4) + (4/7) * (1/2) + (12/7) * (1/4) = 1 ∧
    (8/7 : ℝ) * (3/16) + (4/7) * (3/32) + (12/7) * (3/32) = 3/7 := by
  norm_num

end Literature.Jeffrey
