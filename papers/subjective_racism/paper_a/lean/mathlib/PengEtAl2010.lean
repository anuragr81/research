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

omit [Fintype ι] in
/-- PDF p. 7 (PZ-Q10). Hard evidence `Y = y₀` is the soft evidence that puts mass
one on `y₀`; Jeffrey's rule then conditions on `y₀`. -/
theorem hard_evidence {P : Ω → ℝ} {cell : Ω → ι} {i₀ : ι} (x : Ω) :
    jeffrey P cell (fun i => if i = i₀ then 1 else 0) x
      = if cell x = i₀ then P x / cellMass P cell i₀ else 0 := by
  unfold jeffrey
  by_cases h : cell x = i₀ <;> simp [h]

omit [Fintype ι] in
/-- Hard evidence as a virtual evidence. The likelihood that is one on `y₀` and
zero elsewhere, applied by Bayes' rule, conditions on `y₀`. -/
theorem hard_evidence_virtual {P : Ω → ℝ} {cell : Ω → ι} {i₀ : ι} (x : Ω) :
    virtual P cell (fun i => if i = i₀ then 1 else 0) x
      = if cell x = i₀ then P x / cellMass P cell i₀ else 0 := by
  unfold virtual
  have hZ : ∑ x', P x' * (if cell x' = i₀ then 1 else 0) = cellMass P cell i₀ := by
    unfold cellMass; refine Finset.sum_congr rfl (fun y _ => ?_); split_ifs <;> simp
  rw [hZ]
  by_cases h : cell x = i₀ <;> simp [h]

omit [Fintype ι] in
/-- Hard evidence forgets the prior's weights across instantiations of `Y`. Two
priors proportional within `y₀`, whatever they put elsewhere, give the same
posterior. -/
theorem hard_evidence_forgets_prior {P P' : Ω → ℝ} {cell : Ω → ι} {i₀ : ι} {k : ℝ} (hk : k ≠ 0)
    (hprop : ∀ x, cell x = i₀ → P' x = k * P x) (x : Ω) :
    virtual P' cell (fun i => if i = i₀ then 1 else 0) x
      = virtual P cell (fun i => if i = i₀ then 1 else 0) x := by
  rw [hard_evidence_virtual, hard_evidence_virtual]
  split_ifs with h
  · have hm : cellMass P' cell i₀ = k * cellMass P cell i₀ := by
      unfold cellMass; rw [Finset.mul_sum]
      refine Finset.sum_congr rfl (fun y _ => ?_)
      split_ifs with hy
      · exact hprop y hy
      · ring
    rw [hprop x h, hm]
    by_cases h0 : cellMass P cell i₀ = 0
    · simp [h0]
    · field_simp
  · rfl

/-- Control: a likelihood short of hard evidence keeps the prior's weights
across instantiations. With `L = (1, 1/2)`, priors `(1/2, 1/2)` and
`(1/4, 3/4)` give the first state `2/3` and `2/5`. -/
theorem control_soft_keeps_prior :
    virtual (![1 / 2, 1 / 2] : Fin 2 → ℝ) id ![1, 1 / 2] 0 = 2 / 3 ∧
      virtual (![1 / 4, 3 / 4] : Fin 2 → ℝ) id ![1, 1 / 2] 0 = 2 / 5 := by
  constructor <;> simp [virtual, Fin.sum_univ_two] <;> norm_num

/-! ### Eq. (1), Definitions 1, 3 and 4, Theorem 1 and Theorem 4 -/

omit [Fintype ι] in
/-- Eq. (1). Jeffrey's rule gives an event `Z` the probability
`Σ_y P(Z | y) R(y) = Σ_y P(Z, y)/P(y) R(y)`. -/
theorem eq1 {P : Ω → ℝ} {cell : Ω → ι} {R : ι → ℝ} (Z : Ω → Prop) [DecidablePred Z]
    [Fintype ι] :
    (∑ x, if Z x then jeffrey P cell R x else 0)
      = ∑ i, (∑ x, if Z x ∧ cell x = i then P x else 0) / cellMass P cell i * R i := by
  rw [sum_by_cell cell]
  refine Finset.sum_congr rfl (fun i _ => ?_)
  rw [Finset.sum_div, Finset.sum_mul]
  refine Finset.sum_congr rfl (fun x _ => ?_)
  unfold jeffrey
  by_cases hz : Z x <;> by_cases hc : cell x = i <;> simp [hz, hc]
  ring

/-- Definition 1, the I-divergence on a finite set, for `P << Q`. With Lean's
`log 0 = 0`, terms with `P(x) = 0` vanish, as in eq. (3). -/
noncomputable def kl (P Q : Ω → ℝ) : ℝ := ∑ x, P x * Real.log (P x / Q x)

/-- Definition 3. `Q` is an I-projection of `P` on `S`. -/
def IsIProjection (Q P : Ω → ℝ) (S : Set (Ω → ℝ)) : Prop :=
  Q ∈ S ∧ ∀ Q' ∈ S, kl Q P ≤ kl Q' P

/-- `P_{R(Y)}`, the distributions whose marginal on `Y` is `R(Y)`. -/
def PR (cell : Ω → ι) (R : ι → ℝ) : Set (Ω → ℝ) :=
  {Q | (∀ x, 0 ≤ Q x) ∧ ∀ i, cellMass Q cell i = R i}

/-- The pointwise inequality behind Gibbs' inequality. -/
theorem kl_term_ge {a b : ℝ} (ha : 0 ≤ a) (hb : 0 < b) : a - b ≤ a * Real.log (a / b) := by
  rcases ha.eq_or_lt with h | h
  · subst h; simp; linarith
  · have h1 : Real.log (b / a) ≤ b / a - 1 := Real.log_le_sub_one_of_pos (div_pos hb h)
    have h2 : Real.log (a / b) = - Real.log (b / a) := by
      rw [← Real.log_inv, inv_div]
    rw [h2]
    have : a * (b / a - 1) = b - a := by field_simp
    nlinarith

omit [Fintype ι] in
theorem le_cellMass {Q : Ω → ℝ} {cell : Ω → ι} (h0 : ∀ x, 0 ≤ Q x) (x : Ω) :
    Q x ≤ cellMass Q cell (cell x) := by
  unfold cellMass
  have := Finset.single_le_sum (f := fun y => if cell y = cell x then Q y else 0)
    (fun y _ => by by_cases h : cell y = cell x <;> simp [h, h0 y]) (Finset.mem_univ x)
  simpa using this

/-- Theorem 1, finite case with a strictly positive prior and every instantiation
of `Y` possible. Jeffrey's rule is the I-projection of `P` on `P_{R(Y)}`. -/
theorem theorem1 {P : Ω → ℝ} {cell : Ω → ι} {R : ι → ℝ} (hP : ∀ x, 0 < P x)
    (hc : ∀ i, 0 < cellMass P cell i) (hR : ∀ i, 0 ≤ R i) :
    IsIProjection (jeffrey P cell R) P (PR cell R) := by
  have hQ0 : ∀ x, 0 ≤ jeffrey P cell R x := fun x => by
    unfold jeffrey; exact div_nonneg (mul_nonneg (hP x).le (hR _)) (hc _).le
  have hQm : ∀ i, cellMass (jeffrey P cell R) cell i = R i := fun i =>
    jeffrey_marginal (hc i).ne'
  refine ⟨⟨hQ0, hQm⟩, fun Qt ⟨hQt0, hQt⟩ => ?_⟩
  set g : ι → ℝ := fun i => Real.log (R i / cellMass P cell i)
  have hsumR : ∀ Q : Ω → ℝ, (∀ i, cellMass Q cell i = R i) →
      ∑ x, Q x * g (cell x) = ∑ i, R i * g i := by
    intro Q hQ; rw [sum_cell Q cell g]; simp [hQ]
  have hmass : ∀ Q : Ω → ℝ, (∀ i, cellMass Q cell i = R i) → ∑ x, Q x = ∑ i, R i := by
    intro Q hQ
    have := sum_cell Q cell (fun _ => (1 : ℝ)); simp only [mul_one] at this
    rw [this]; simp [hQ]
  have hklQ : kl (jeffrey P cell R) P = ∑ x, jeffrey P cell R x * g (cell x) := by
    unfold kl
    refine Finset.sum_congr rfl (fun x _ => ?_)
    congr 2
    unfold jeffrey
    have := (hP x).ne'
    field_simp
  have hpt : ∀ x, Qt x * g (cell x) + Qt x - jeffrey P cell R x
      ≤ Qt x * Real.log (Qt x / P x) := by
    intro x
    rcases (hQt0 x).eq_or_lt with h | h
    · rw [← h]; simp; exact hQ0 x
    · have hRx : 0 < R (cell x) := by
        have := le_cellMass (cell := cell) hQt0 x; rw [hQt] at this; linarith
      have hq : 0 < jeffrey P cell R x := by
        unfold jeffrey; exact div_pos (mul_pos (hP x) hRx) (hc _)
      have hsplit : Real.log (Qt x / P x)
          = Real.log (Qt x / jeffrey P cell R x) + g (cell x) := by
        have e : Qt x / P x = (Qt x / jeffrey P cell R x) * (R (cell x) / cellMass P cell (cell x)) := by
          unfold jeffrey; have := (hP x).ne'; have := (hc (cell x)).ne'
          field_simp
        rw [e, Real.log_mul (div_pos h hq).ne' (div_pos hRx (hc _)).ne']
      rw [hsplit, mul_add]
      have := kl_term_ge (hQt0 x) hq
      linarith
  have hsum := Finset.sum_le_sum (fun x (_ : x ∈ Finset.univ) => hpt x)
  simp only [Finset.sum_sub_distrib, Finset.sum_add_distrib] at hsum
  rw [hsumR Qt hQt, hmass Qt hQt, hmass _ hQm] at hsum
  rw [hklQ, hsumR _ hQm]
  unfold kl
  linarith

/-- Control for Theorem 1: the strictly positive prior (dominance `Q̃ << P`) is
needed. With `P = (1, 0)`, one instantiation of `Y` and `R = 1`, the distribution
`(1/2, 1/2)` satisfies the constraint but has a smaller finite I-divergence than
Jeffrey's rule; in the paper its I-divergence is `+∞` by eq. (3). -/
theorem control_theorem1_dominance :
    let P : Fin 2 → ℝ := ![1, 0]
    let Q : Fin 2 → ℝ := ![1 / 2, 1 / 2]
    let cell : Fin 2 → Fin 1 := fun _ => 0
    Q ∈ PR cell (fun _ => 1) ∧ kl Q P < kl (jeffrey P cell (fun _ => 1)) P := by
  intro P Q cell
  have hl : Real.log (1 / 2) < 0 := Real.log_neg (by norm_num) (by norm_num)
  refine ⟨⟨fun x => by fin_cases x <;> simp [Q], fun i => ?_⟩, ?_⟩
  · fin_cases i; simp [cellMass, Q, cell, Fin.sum_univ_two]; norm_num
  · simp [kl, jeffrey, cellMass, P, Q, cell, Fin.sum_univ_two]
    have := Real.log_pos (show (1 : ℝ) < 2 by norm_num)
    linarith

/-- Definition 4 for a finite list of constraints. Each step is eq. (7), the
I-projection of eq. (6) on the next constraint. -/
noncomputable def ipfp (P : Ω → ℝ) : List ((Ω → ι) × (ι → ℝ)) → Ω → ℝ
  | [] => P
  | c :: l => ipfp (jeffrey P c.1 c.2) l

/-- Eq. (10), one step of SR-IPFP. -/
noncomputable def srStep (α : ℝ) (Q : Ω → ℝ) (cell : Ω → ι) (R : ι → ℝ) (x : Ω) : ℝ :=
  (1 - α) * Q x + α * jeffrey Q cell R x

/-- Eq. (11), the I-aggregate of `Q` to the constraints, weighted by `w`. -/
noncomputable def iAggregate {m : ℕ} (w : Fin m → ℝ) (cells : Fin m → Ω → ι)
    (R : Fin m → ι → ℝ) (Q : Ω → ℝ) : ℝ :=
  ∑ j, w j * ∑ i, R j i * Real.log (R j i / cellMass Q (cells j) i)

section theorem4

variable {κ : Type*} [DecidableEq κ]

omit [Fintype ι] [DecidableEq κ] in
/-- One IPFP step by a constraint on `Y^j ⊆ Y` rescales `Q` by a function of
the instantiation of `Y`. -/
theorem step_factor {Q : Ω → ℝ} {c : Ω → ι} {R : ι → ℝ} {cellY : Ω → κ} {f : κ → ι}
    (hf : ∀ x, c x = f (cellY x)) :
    ∀ x, jeffrey Q c R x = Q x * (R (f (cellY x)) / cellMass Q c (f (cellY x))) := by
  intro x; unfold jeffrey; rw [hf x]; ring

omit [Fintype ι] [DecidableEq κ] in
/-- Theorem 4 for every finite iterate. If every constraint is on a subset of
`Y`, IPFP multiplies `Q_0` by a function of `y`, so `Q_k(x) = Q_0(x) K(y)`,
which is eq. (8) with `K(y) = Q_k(y)/Q_0(y)`. -/
theorem theorem4_finite (cellY : Ω → κ) :
    ∀ (l : List ((Ω → ι) × (ι → ℝ))) (P : Ω → ℝ),
      (∀ c ∈ l, ∃ f : κ → ι, ∀ x, c.1 x = f (cellY x)) →
      ∃ K : κ → ℝ, ∀ x, ipfp P l x = P x * K (cellY x) := by
  intro l
  induction l with
  | nil => intro P _; exact ⟨fun _ => 1, fun x => by simp [ipfp]⟩
  | cons c l ih =>
    intro P hl
    obtain ⟨f, hf⟩ := hl c (List.mem_cons_self ..)
    obtain ⟨K, hK⟩ := ih (jeffrey P c.1 c.2) (fun c' hc' => hl c' (List.mem_cons_of_mem _ hc'))
    refine ⟨fun y => c.2 (f y) / cellMass P c.1 (f y) * K y, fun x => ?_⟩
    show ipfp (jeffrey P c.1 c.2) l x = _
    rw [hK x, step_factor hf x]; ring

omit [Fintype ι] in
/-- Theorem 4, the rigidity form. Every finite IPFP iterate keeps
`Q_k(x) Q_0(y) = Q_0(x) Q_k(y)`. -/
theorem theorem4_rigid (cellY : Ω → κ) (l : List ((Ω → ι) × (ι → ℝ))) (P : Ω → ℝ)
    (hl : ∀ c ∈ l, ∃ f : κ → ι, ∀ x, c.1 x = f (cellY x)) (x : Ω) :
    ipfp P l x * cellMass P cellY (cellY x) = P x * cellMass (ipfp P l) cellY (cellY x) := by
  obtain ⟨K, hK⟩ := theorem4_finite cellY l P hl
  have hm : cellMass (ipfp P l) cellY (cellY x) = cellMass P cellY (cellY x) * K (cellY x) := by
    unfold cellMass; rw [Finset.sum_mul]
    refine Finset.sum_congr rfl (fun y _ => ?_)
    split_ifs with h
    · rw [hK y, h]
    · simp
  rw [hm, hK x]; ring

end theorem4

/-! ### Theorem 7, the step of its proof -/

omit [Fintype ι] in
/-- Eq. (13-1). Projecting on the smoothed constraint `αR + (1 − α)Q(Y)` is the
mixture of the projection on `R` and `Q` itself. -/
theorem eq13_1 {Q : Ω → ℝ} {cell : Ω → ι} {R : ι → ℝ} (α : ℝ) {x : Ω}
    (h : cellMass Q cell (cell x) ≠ 0) :
    jeffrey Q cell (fun i => α * R i + (1 - α) * cellMass Q cell i) x
      = α * jeffrey Q cell R x + (1 - α) * Q x := by
  unfold jeffrey; field_simp

/-- (A-5). The step to the mixture is `α` times the step to the projection. -/
theorem eqA5 {Q₁ Q₂' : Ω → ℝ} {α : ℝ} (hα : 0 ≤ α) :
    tv Q₁ (fun x => α * Q₂' x + (1 - α) * Q₁ x) = α * tv Q₁ Q₂' := by
  unfold tv; rw [Finset.mul_sum]
  refine Finset.sum_congr rfl (fun x _ => ?_)
  rw [show Q₁ x - (α * Q₂' x + (1 - α) * Q₁ x) = α * (Q₁ x - Q₂' x) by ring, abs_mul,
    abs_of_nonneg hα]

/-- (A-4) with (A-5). One SMOOTH step from `Q₁` is at most `α` times the
distance from `Q₁` back to any `Q₀` that satisfies the constraint, the
contraction on which the proof of Theorem 7 rests. -/
theorem theorem7_step {Q₀ Q₁ : Ω → ℝ} {cell : Ω → ι} {R : ι → ℝ} {α : ℝ} (hα : 0 ≤ α)
    (hQ₁ : ∀ x, 0 ≤ Q₁ x) (hc : ∀ i, 0 < cellMass Q₁ cell i)
    (hQ₀ : ∀ i, cellMass Q₀ cell i = R i) :
    tv Q₁ (fun x => α * jeffrey Q₁ cell R x + (1 - α) * Q₁ x) ≤ α * tv Q₁ Q₀ := by
  rw [eqA5 hα]
  exact mul_le_mul_of_nonneg_left (theorem2 hQ₁ hc hQ₀).2 hα

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

/-- Table 1, row 3. Applying `se2` alone gives `B = 1` the belief `5/11`, printed
`0.455`. -/
theorem ex1_row3 : cellMass (jeffrey J cellC RC) cellB 0 = 5 / 11 := by
  simp [cellMass, jeffrey, Fin.sum_univ_eight, J, cellB, cellC, RC]
  norm_num

/-- Table 1, row 6. `L(B)` then the recomputed `L'(C)` satisfies `se2` and gives
`B = 1` the belief `343049/482870`, printed `0.710`. The recomputed likelihood
uses `Q(C = 1) = 327/770` (PZ-D2). -/
theorem ex1_row6 :
    cellMass (jeffrey (jeffrey J cellB RB) cellC RC) cellB 0 = 343049 / 482870 ∧
      cellMass (jeffrey (jeffrey J cellB RB) cellC RC) cellC 0 = 3 / 10 := by
  constructor
  · simp [cellMass, jeffrey, Fin.sum_univ_eight, J, cellB, cellC, RB, RC]
    norm_num
  · simp [cellMass, jeffrey, Fin.sum_univ_eight, J, cellB, cellC, RB, RC]
    norm_num

/-! ### Theorem 5 fails

Two binary variables, constraints on each marginal, `R = (1/2, 1/2)` for both,
and `Q_0 = (.01, .09, .09, .81)` in the order `11, 10, 01, 00`. IPFP reaches the
uniform distribution in two steps and stays there, so the uniform distribution
is its converging distribution `Q*`. A distribution in `S` is nearer to `Q_0` in
total variation. -/

def rowsCell : Fin 4 → Fin 2 := ![0, 0, 1, 1]
def colsCell : Fin 4 → Fin 2 := ![0, 1, 0, 1]
noncomputable def half : Fin 2 → ℝ := ![1 / 2, 1 / 2]
noncomputable def Q0 : Fin 4 → ℝ := ![1 / 100, 9 / 100, 9 / 100, 81 / 100]
noncomputable def Qstar : Fin 4 → ℝ := fun _ => 1 / 4
noncomputable def Qt : Fin 4 → ℝ := ![41 / 100, 9 / 100, 9 / 100, 41 / 100]

theorem t5_two_steps : jeffrey (jeffrey Q0 rowsCell half) colsCell half = Qstar := by
  funext x
  fin_cases x <;> simp [jeffrey, cellMass, Fin.sum_univ_four, Q0, rowsCell, colsCell, half,
    Qstar] <;> norm_num

theorem t5_fixed_rows : jeffrey Qstar rowsCell half = Qstar := by
  funext x
  fin_cases x <;> simp [jeffrey, cellMass, Fin.sum_univ_four, rowsCell, half, Qstar] <;> norm_num

theorem t5_fixed_cols : jeffrey Qstar colsCell half = Qstar := by
  funext x
  fin_cases x <;> simp [jeffrey, cellMass, Fin.sum_univ_four, colsCell, half, Qstar] <;> norm_num

/-- IPFP from `Q_0` with the two constraints in any later order stays at `Q*`. -/
theorem t5_ipfp_limit (l : List ((Fin 4 → Fin 2) × (Fin 2 → ℝ)))
    (hl : ∀ c ∈ l, c = (rowsCell, half) ∨ c = (colsCell, half)) :
    ipfp Q0 ((rowsCell, half) :: (colsCell, half) :: l) = Qstar := by
  show ipfp (jeffrey (jeffrey Q0 rowsCell half) colsCell half) l = Qstar
  rw [t5_two_steps]
  induction l with
  | nil => rfl
  | cons c l ih =>
    rcases hl c (List.mem_cons_self ..) with h | h
    · subst h; show ipfp (jeffrey Qstar rowsCell half) l = Qstar
      rw [t5_fixed_rows]; exact ih (fun c' hc' => hl c' (List.mem_cons_of_mem _ hc'))
    · subst h; show ipfp (jeffrey Qstar colsCell half) l = Qstar
      rw [t5_fixed_cols]; exact ih (fun c' hc' => hl c' (List.mem_cons_of_mem _ hc'))

/-- Theorem 5 as printed fails. `Q̃` satisfies both constraints and is nearer to
`Q_0` in total variation than IPFP's converging distribution `Q*`. -/
theorem theorem5_fails :
    cellMass Qt rowsCell 0 = 1 / 2 ∧ cellMass Qt rowsCell 1 = 1 / 2 ∧
      cellMass Qt colsCell 0 = 1 / 2 ∧ cellMass Qt colsCell 1 = 1 / 2 ∧
      tv Q0 Qt = 4 / 5 ∧ tv Q0 Qstar = 28 / 25 ∧ tv Q0 Qt < tv Q0 Qstar := by
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_, ?_⟩ <;>
    simp [cellMass, tv, Fin.sum_univ_four, Qt, Q0, Qstar, rowsCell, colsCell] <;>
    norm_num [abs_of_neg, abs_of_pos, abs_of_nonneg]

/-- Control for Theorem 4: a constraint on a variable outside `Y` breaks the
rigidity. With `Y` the row variable, a step on the column marginal changes
`Q(x)/Q(y)` at the first atom. -/
theorem control_theorem4_factor :
    jeffrey Q0 colsCell half 0 * cellMass Q0 rowsCell (rowsCell 0)
      ≠ Q0 0 * cellMass (jeffrey Q0 colsCell half) rowsCell (rowsCell 0) := by
  simp [jeffrey, cellMass, Fin.sum_univ_four, Q0, rowsCell, colsCell, half]
  norm_num

/-! ### Eq. (14) and Table 4 -/

/-- Eq. (14), the sigmoid schedule. -/
noncomputable def alphaSched (A B k : ℝ) : ℝ := Real.exp (A - k / B) / (1 + Real.exp (A - k / B))

/-- Eq. (14) at `k = 0` is `1/(1 + exp(−A))`. -/
theorem eq14_initial (A B : ℝ) : alphaSched A B 0 = 1 / (1 + Real.exp (-A)) := by
  unfold alphaSched
  have h := Real.exp_pos A
  rw [zero_div, sub_zero, Real.exp_neg]
  field_simp
  ring

/-- Control. The printed `α₀ = 1/(1 + exp(A))` is below `1/2` for `A > 0`, while
eq. (14) gives `α₀ > 1/2`, so the printed formula cannot give `α₀ ≈ 0.99` at
`A = 4.595` (PZ-D10). -/
theorem control_eq14_printed {A B : ℝ} (hA : 0 < A) :
    1 / (1 + Real.exp A) < 1 / 2 ∧ 1 / 2 < alphaSched A B 0 := by
  have h1 : 1 < Real.exp A := Real.one_lt_exp_iff.mpr hA
  have h2 : Real.exp (-A) < 1 := by rw [← Real.exp_zero]; exact Real.exp_lt_exp.mpr (by linarith)
  have h3 := Real.exp_pos (-A)
  rw [eq14_initial]
  constructor
  · rw [div_lt_div_iff₀ (by positivity) (by norm_num)]; linarith
  · rw [div_lt_div_iff₀ (by norm_num) (by positivity)]; linarith

/-- Table 4's constraints on `(X₁, X₂, X₃)`, atoms indexed `4x₁ + 2x₂ + x₃`. -/
def table4 (ε : ℝ) (q : Fin 8 → ℝ) : Prop :=
  q 0 + q 1 = 1 / 2 - ε ∧ q 2 + q 3 = ε ∧ q 4 + q 5 = ε ∧ q 6 + q 7 = 1 / 2 - ε ∧
  q 0 + q 4 = 1 / 2 - ε ∧ q 1 + q 5 = ε ∧ q 2 + q 6 = ε ∧ q 3 + q 7 = 1 / 2 - ε ∧
  q 0 + q 2 = ε ∧ q 1 + q 3 = 1 / 2 - ε ∧ q 4 + q 6 = 1 / 2 - ε ∧ q 5 + q 7 = ε

/-- Experiment 5-2. At `ε = 3/20` (CONS1) no distribution satisfies all three
constraints. -/
theorem table4_inconsistent : ¬ ∃ q : Fin 8 → ℝ, (∀ i, 0 ≤ q i) ∧ table4 (3 / 20) q := by
  rintro ⟨q, h0, h⟩
  obtain ⟨_, h2, h3, _, _, h6, h7, _, _, h10, h11, _⟩ := h
  have := h0 0; have := h0 1; have := h0 2; have := h0 3
  have := h0 4; have := h0 5; have := h0 6; have := h0 7
  linarith

/-- Experiment 5-1. At `ε = 4/20` (CONS0) the constraints are consistent. -/
theorem table4_consistent :
    let q : Fin 8 → ℝ := ![1 / 10, 2 / 10, 1 / 10, 1 / 10, 2 / 10, 0, 1 / 10, 2 / 10]
    (∀ i, 0 ≤ q i) ∧ table4 (4 / 20) q := by
  intro q
  refine ⟨fun i => ?_, ?_⟩
  · fin_cases i <;> simp [q] <;> norm_num
  · simp [table4, q]; norm_num

end PengEtAl2010
