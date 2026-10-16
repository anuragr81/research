import Mathlib

/-! Kim and Loury (2012), "Stereotypes and Inequality: A 'Signaling' Theory of
Identity Choice", KDI Journal of Economic Policy 34(2), pp. 1–15, in the
paper's notation.

`V₀ π` and `V₁ π` are the expected wages of an unskilled and a skilled worker in
a group believed to invest at rate `π` (eq. 2). They are taken here as given
functions. Eq. (2) derives them from signal densities under MLRP, and that
derivation (and with it the hump shape of `R`) is not formalised.
`R π = V₁ π − V₀ π` is the return to skill (eq. 3), `U π c` the payoff of a
worker with skill cost `c` (eq. 9), and `dU πA πB c` the gain `ΔU(πA, πB; c)`
from affect A over affect B.

The cost distribution `G` is taken to be finite, a tractable special case of
the paper's continuous `G`. Cost types are `j : ι` with cost `c j` and mass
`g j`. `H` is the distribution function of the affect cost `k`, and a worker
chooses affect A if and only if `ΔU ≥ k`, so a type chooses A with
probability `H (ΔU)` (eqs. 10 to 13). -/

namespace KimLoury2012

open Finset

/-- Eq. (3). The return to acquiring skill in a group believed to invest at `π`. -/
def R (V₀ V₁ : ℝ → ℝ) (π : ℝ) : ℝ := V₁ π - V₀ π

/-- Eq. (9). The payoff of a worker with skill cost `c` in a group believed to
invest at `π`, `U(π, c) = max {V₁(π) − c; V₀(π)}`. -/
noncomputable def U (V₀ V₁ : ℝ → ℝ) (π c : ℝ) : ℝ := max (V₁ π - c) (V₀ π)

/-- `ΔU(πA, πB; c) ≡ U(πA, c) − U(πB, c)`, the gain from affect A over affect B. -/
noncomputable def dU (V₀ V₁ : ℝ → ℝ) (πA πB c : ℝ) : ℝ := U V₀ V₁ πA c - U V₀ V₁ πB c

/-- Eq. (15). `U(π, c) = max {R(π) − c; 0} + V₀(π)`. -/
theorem eq_15 (V₀ V₁ : ℝ → ℝ) (π c : ℝ) :
    U V₀ V₁ π c = max (R V₀ V₁ π - c) 0 + V₀ π := by
  unfold U R
  simp only [max_def]
  split_ifs <;> linarith

/-- Lemma 1, low costs. Below both returns, every type invests in either group
and `ΔU = V₁(πA) − V₁(πB)`. -/
theorem lemma1_low {V₀ V₁ : ℝ → ℝ} {πA πB c : ℝ}
    (hA : c ≤ R V₀ V₁ πA) (hB : c ≤ R V₀ V₁ πB) :
    dU V₀ V₁ πA πB c = V₁ πA - V₁ πB := by
  unfold dU U
  unfold R at hA hB
  rw [max_eq_left (by linarith), max_eq_left (by linarith)]
  ring

/-- Lemma 1, high costs. Above both returns, no type invests and
`ΔU = V₀(πA) − V₀(πB)`. -/
theorem lemma1_high {V₀ V₁ : ℝ → ℝ} {πA πB c : ℝ}
    (hA : R V₀ V₁ πA ≤ c) (hB : R V₀ V₁ πB ≤ c) :
    dU V₀ V₁ πA πB c = V₀ πA - V₀ πB := by
  unfold dU U
  unfold R at hA hB
  rw [max_eq_right (by linarith), max_eq_right (by linarith)]

/-- Lemma 1, eq. (16), first line. Between the returns, with `R(πA) ≥ R(πB)`,
`ΔU = V₁(πA) − V₀(πB) − c`. -/
theorem lemma1_mid {V₀ V₁ : ℝ → ℝ} {πA πB c : ℝ}
    (hB : R V₀ V₁ πB ≤ c) (hA : c ≤ R V₀ V₁ πA) :
    dU V₀ V₁ πA πB c = V₁ πA - V₀ πB - c := by
  unfold dU U
  unfold R at hA hB
  rw [max_eq_left (by linarith), max_eq_right (by linarith)]
  ring

/-- Lemma 1, read as on p. 11. `ΔU` is non-increasing in `c` whenever
`R(πA) ≥ R(πB)`. -/
theorem dU_antitone {V₀ V₁ : ℝ → ℝ} {πA πB : ℝ} (h : R V₀ V₁ πB ≤ R V₀ V₁ πA) :
    Antitone (dU V₀ V₁ πA πB) := by
  intro x y hxy
  simp only [dU, eq_15]
  unfold R at h ⊢
  simp only [max_def]
  split_ifs <;> linarith

/-- `ΔU(πB, πA; c) = −ΔU(πA, πB; c)` (p. 9). -/
theorem dU_swap (V₀ V₁ : ℝ → ℝ) (πA πB c : ℝ) :
    dU V₀ V₁ πB πA c = - dU V₀ V₁ πA πB c := by
  unfold dU; ring

/-- Proposition 1, pointwise. If both wages are higher in group A (`V_e` rising
in `π` and `πA > πB`), every cost type gains from affect A. -/
theorem prop1_pointwise {V₀ V₁ : ℝ → ℝ} {πA πB : ℝ}
    (h0 : V₀ πB < V₀ πA) (h1 : V₁ πB < V₁ πA) (c : ℝ) :
    0 < dU V₀ V₁ πA πB c := by
  unfold dU U
  have : max (V₁ πB - c) (V₀ πB) < max (V₁ πA - c) (V₀ πA) :=
    max_lt (lt_max_of_lt_left (by linarith)) (lt_max_of_lt_right h0)
  linarith

/-! ### The finite cost distribution -/

variable {ι : Type*} [Fintype ι]

/-- `G(r)`, the mass of cost types at or below `r`. -/
noncomputable def G (c g : ι → ℝ) (r : ℝ) : ℝ :=
  ∑ j ∈ univ.filter (fun j => c j ≤ r), g j

/-- The investment rate among the members of a group whose members are drawn
with weights `w`, when members invest at costs up to `r`. -/
noncomputable def selRate (c g w : ι → ℝ) (r : ℝ) : ℝ :=
  (∑ j ∈ univ.filter (fun j => c j ≤ r), g j * w j) / ∑ j, g j * w j

/-- Eq. (10). `Σ^A`, the fraction choosing affect A. -/
noncomputable def SigmaA (V₀ V₁ H : ℝ → ℝ) (c g : ι → ℝ) (πA πB : ℝ) : ℝ :=
  ∑ j, g j * H (dU V₀ V₁ πA πB (c j))

/-- Eq. (14). `φ(πA, πB) = σ^A / Σ^A`, the actual investment rate in group A
given the beliefs `(πA, πB)`. With the arguments swapped it is `φ(πB, πA)`, the
rate in group B. -/
noncomputable def phi (V₀ V₁ H : ℝ → ℝ) (c g : ι → ℝ) (πA πB : ℝ) : ℝ :=
  selRate c g (fun j => H (dU V₀ V₁ πA πB (c j))) (R V₀ V₁ πA)

/-- The Phenotypic Stereotyping Equilibrium of p. 8, `π̂ = G(R(π̂))`. -/
def IsPSE (V₀ V₁ : ℝ → ℝ) (c g : ι → ℝ) (x : ℝ) : Prop := x = G c g (R V₀ V₁ x)

/-- The Affective Stereotyping Equilibrium of p. 10. The employers' belief about
each group equals the actual investment rate of the group's members,
`π*_A = φ(π*_A, π*_B)` and `π*_B = φ(π*_B, π*_A)`. -/
def IsASE (V₀ V₁ H : ℝ → ℝ) (c g : ι → ℝ) (πA πB : ℝ) : Prop :=
  πA = phi V₀ V₁ H c g πA πB ∧ πB = phi V₀ V₁ H c g πB πA

/-- The symmetry `H(k) = 1 − H(−k)` gives `H(0) = 1/2`. -/
theorem H_zero {H : ℝ → ℝ} (hH : ∀ k, H k = 1 - H (-k)) : H 0 = 1 / 2 := by
  have := hH 0; simp at this; linarith

/-- Proposition 1. With `V_e(πB) < V_e(πA)`, a symmetric and strictly increasing
`H`, and a cost distribution of total mass 1, more than half the workers choose
affect A, `Σ^A > .5`. -/
theorem prop1 {V₀ V₁ H : ℝ → ℝ} {c g : ι → ℝ} {πA πB : ℝ}
    (h0 : V₀ πB < V₀ πA) (h1 : V₁ πB < V₁ πA)
    (hH : ∀ k, H k = 1 - H (-k)) (hmono : StrictMono H)
    (hg : ∀ j, 0 ≤ g j) (hsum : ∑ j, g j = 1) :
    1 / 2 < SigmaA V₀ V₁ H c g πA πB := by
  have hpos : ∃ j ∈ (univ : Finset ι), 0 < g j * (H (dU V₀ V₁ πA πB (c j)) - 1 / 2) := by
    by_contra hne
    push_neg at hne
    have hall : ∀ j, g j = 0 := by
      intro j
      have hd := hmono (prop1_pointwise h0 h1 (c j))
      rw [H_zero hH] at hd
      have := hne j (mem_univ j)
      have hgj := hg j
      nlinarith
    simp [hall] at hsum
  have hs : 0 < ∑ j, g j * (H (dU V₀ V₁ πA πB (c j)) - 1 / 2) := by
    apply Finset.sum_pos' _ hpos
    intro j _
    have hd := hmono (prop1_pointwise h0 h1 (c j))
    rw [H_zero hH] at hd
    exact mul_nonneg (hg j) (by linarith)
  have e : ∑ j, g j * (H (dU V₀ V₁ πA πB (c j)) - 1 / 2)
      = SigmaA V₀ V₁ H c g πA πB - 1 / 2 * ∑ j, g j := by
    unfold SigmaA
    rw [Finset.mul_sum, ← Finset.sum_sub_distrib]
    exact Finset.sum_congr rfl (fun j _ => by ring)
  rw [e, hsum] at hs
  linarith

/-- The core of Lemma 2. If a group draws its members with weights that are
higher for every type that invests than for every type that does not, its
investment rate is at least `G(r)`, the rate under random membership. -/
theorem selRate_ge {c g w : ι → ℝ} {r : ℝ} (hg : ∀ j, 0 ≤ g j)
    (hsum : ∑ j, g j = 1) (hpos : 0 < ∑ j, g j * w j)
    (hw : ∀ j k, c j ≤ r → r < c k → w k ≤ w j) :
    G c g r ≤ selRate c g w r := by
  unfold selRate G
  set S := univ.filter (fun j => c j ≤ r)
  set T := univ.filter (fun j => ¬ c j ≤ r)
  have split1 : ∑ j, g j = ∑ j ∈ S, g j + ∑ j ∈ T, g j :=
    (Finset.sum_filter_add_sum_filter_not _ _ _).symm
  have split2 : ∑ j, g j * w j = ∑ j ∈ S, g j * w j + ∑ j ∈ T, g j * w j :=
    (Finset.sum_filter_add_sum_filter_not _ _ _).symm
  have key : (∑ j ∈ S, g j * w j) * (∑ k ∈ T, g k) - (∑ j ∈ S, g j) * (∑ k ∈ T, g k * w k)
      = ∑ j ∈ S, ∑ k ∈ T, g j * g k * (w j - w k) := by
    rw [Finset.sum_mul_sum, Finset.sum_mul_sum, ← Finset.sum_sub_distrib]
    refine Finset.sum_congr rfl (fun j _ => ?_)
    rw [← Finset.sum_sub_distrib]
    exact Finset.sum_congr rfl (fun k _ => by ring)
  have nonneg : 0 ≤ ∑ j ∈ S, ∑ k ∈ T, g j * g k * (w j - w k) := by
    apply Finset.sum_nonneg
    intro j hj
    apply Finset.sum_nonneg
    intro k hk
    have hj' : c j ≤ r := (Finset.mem_filter.1 hj).2
    have hk' : r < c k := lt_of_not_ge (Finset.mem_filter.1 hk).2
    exact mul_nonneg (mul_nonneg (hg j) (hg k)) (by linarith [hw j k hj' hk'])
  rw [le_div_iff₀ hpos]
  rw [split1] at hsum
  rw [split2]
  have e : ∑ j ∈ S, g j * w j = (∑ j ∈ S, g j * w j) * (∑ j ∈ S, g j + ∑ j ∈ T, g j) := by
    rw [hsum, mul_one]
  nlinarith [key, nonneg, e]

/-- The mirror of `selRate_ge`. Weights higher for every type that does not
invest give an investment rate of at most `G(r)`. -/
theorem selRate_le {c g w : ι → ℝ} {r : ℝ} (hg : ∀ j, 0 ≤ g j)
    (hsum : ∑ j, g j = 1) (hpos : 0 < ∑ j, g j * w j)
    (hw : ∀ j k, c j ≤ r → r < c k → w j ≤ w k) :
    selRate c g w r ≤ G c g r := by
  unfold selRate G
  set S := univ.filter (fun j => c j ≤ r)
  set T := univ.filter (fun j => ¬ c j ≤ r)
  have split1 : ∑ j, g j = ∑ j ∈ S, g j + ∑ j ∈ T, g j :=
    (Finset.sum_filter_add_sum_filter_not _ _ _).symm
  have split2 : ∑ j, g j * w j = ∑ j ∈ S, g j * w j + ∑ j ∈ T, g j * w j :=
    (Finset.sum_filter_add_sum_filter_not _ _ _).symm
  have key : (∑ j ∈ S, g j) * (∑ k ∈ T, g k * w k) - (∑ j ∈ S, g j * w j) * (∑ k ∈ T, g k)
      = ∑ j ∈ S, ∑ k ∈ T, g j * g k * (w k - w j) := by
    rw [Finset.sum_mul_sum, Finset.sum_mul_sum, ← Finset.sum_sub_distrib]
    refine Finset.sum_congr rfl (fun j _ => ?_)
    rw [← Finset.sum_sub_distrib]
    exact Finset.sum_congr rfl (fun k _ => by ring)
  have nonneg : 0 ≤ ∑ j ∈ S, ∑ k ∈ T, g j * g k * (w k - w j) := by
    apply Finset.sum_nonneg
    intro j hj
    apply Finset.sum_nonneg
    intro k hk
    have hj' : c j ≤ r := (Finset.mem_filter.1 hj).2
    have hk' : r < c k := lt_of_not_ge (Finset.mem_filter.1 hk).2
    exact mul_nonneg (mul_nonneg (hg j) (hg k)) (by linarith [hw j k hj' hk'])
  rw [div_le_iff₀ hpos]
  rw [split1] at hsum
  rw [split2]
  have e : ∑ j ∈ S, g j * w j = (∑ j ∈ S, g j * w j) * (∑ j ∈ S, g j + ∑ j ∈ T, g j) := by
    rw [hsum, mul_one]
  nlinarith [key, nonneg, e]

/-- Lemma 2 and Proposition 2, the selection result, in the finite case.
Whenever `R(πA) ≥ R(πB)`, the investment rate of group A is at least
`G(R(πA))` and that of group B at most `G(R(πB))`. The more able (low `c`)
types are drawn to the group with the greater return to skill. -/
theorem prop2 {V₀ V₁ H : ℝ → ℝ} {c g : ι → ℝ} {πA πB : ℝ}
    (hR : R V₀ V₁ πB ≤ R V₀ V₁ πA) (hmono : Monotone H)
    (hg : ∀ j, 0 ≤ g j) (hsum : ∑ j, g j = 1)
    (hA : 0 < ∑ j, g j * H (dU V₀ V₁ πA πB (c j)))
    (hB : 0 < ∑ j, g j * H (dU V₀ V₁ πB πA (c j))) :
    G c g (R V₀ V₁ πA) ≤ phi V₀ V₁ H c g πA πB ∧
      phi V₀ V₁ H c g πB πA ≤ G c g (R V₀ V₁ πB) := by
  have anti := dU_antitone hR
  refine ⟨selRate_ge hg hsum hA (fun j k hj hk => ?_), selRate_le hg hsum hB (fun j k hj hk => ?_)⟩
  · exact hmono (anti (by linarith))
  · apply hmono
    rw [dU_swap V₀ V₁ πA πB (c j), dU_swap V₀ V₁ πA πB (c k)]
    have := anti (show c j ≤ c k by linarith)
    linarith

/-- Every PSE gives a trivial ASE, `(x, x) ∈ Ω_KL` if `x ∈ Ω_CL` (p. 10). With
equal beliefs `ΔU = 0`, every type chooses A with probability `H(0) = 1/2`, and
`φ(x, x) = G(R(x))`. -/
theorem trivial_ase {V₀ V₁ H : ℝ → ℝ} {c g : ι → ℝ} {x : ℝ}
    (hH : ∀ k, H k = 1 - H (-k)) (hsum : ∑ j, g j = 1) (hx : IsPSE V₀ V₁ c g x) :
    IsASE V₀ V₁ H c g x x := by
  have h0 := H_zero hH
  have hd : ∀ y, dU V₀ V₁ x x y = 0 := fun y => by unfold dU; ring
  have hphi : phi V₀ V₁ H c g x x = G c g (R V₀ V₁ x) := by
    unfold phi selRate G
    simp only [hd, h0]
    rw [← Finset.sum_mul, ← Finset.sum_mul, hsum]
    field_simp
  exact ⟨hx.trans hphi.symm, hx.trans hphi.symm⟩

/-- Control: with equal returns, `R(πA) = R(πB)`, there is no selection, even
when `πA ≠ πB`. `ΔU` is the same for every cost type, so group A's investment
rate is `G(R(πA))` exactly (Lemma 2, last sentence). The selection of
`prop2` needs a difference in returns, not only a difference in beliefs. -/
theorem control_no_selection_equal_return {V₀ V₁ H : ℝ → ℝ} {c g : ι → ℝ}
    {πA πB : ℝ} (hR : R V₀ V₁ πA = R V₀ V₁ πB) (hsum : ∑ j, g j = 1)
    (hpos : 0 < ∑ j, g j * H (dU V₀ V₁ πA πB (c j))) :
    phi V₀ V₁ H c g πA πB = G c g (R V₀ V₁ πA) := by
  have hconst : ∀ y, dU V₀ V₁ πA πB y = V₀ πA - V₀ πB := by
    intro y
    rw [dU, eq_15, eq_15, hR]; ring
  have hw : ∑ j, g j * H (dU V₀ V₁ πA πB (c j)) = H (V₀ πA - V₀ πB) := by
    simp only [hconst]; rw [← Finset.sum_mul, hsum, one_mul]
  unfold phi selRate G
  rw [hw]
  simp only [hconst]
  rw [← Finset.sum_mul]
  have hne : H (V₀ πA - V₀ πB) ≠ 0 := by rw [← hw]; exact hpos.ne'
  field_simp

/-! ### Control: a better reputation without a greater return

Group A is believed to invest at `πA = .9` and group B at `πB = .6`, with
`V₀(π) = 2π` and `V₁(π) = 3π − π²`, so both wages are higher in A but the
return `R(π) = π − π²` is lower in A (`.09` against `.24`), as in the right
graph of Figure 3, Panel A. `H` is uniform on `[−1, 1]`, which footnote 3
allows. Two cost types, `c = .05` and `c = .5`, have mass `1/2` each. -/

noncomputable def V₀ex (π : ℝ) : ℝ := 2 * π
noncomputable def V₁ex (π : ℝ) : ℝ := 3 * π - π ^ 2
noncomputable def Hu (k : ℝ) : ℝ := max 0 (min 1 (1 / 2 + k / 2))
noncomputable def cex : Fin 2 → ℝ := ![1 / 20, 1 / 2]
noncomputable def gex : Fin 2 → ℝ := ![1 / 2, 1 / 2]

/-- In the example, both wages are higher in group A. -/
theorem example_wages_higher :
    V₀ex (6 / 10) < V₀ex (9 / 10) ∧ V₁ex (6 / 10) < V₁ex (9 / 10) := by
  unfold V₀ex V₁ex; norm_num

/-- Control: a better reputation does not by itself draw the able. Group A has
the better reputation and pays more at every skill level, so more than half
choose A (Proposition 1), yet A's investment rate falls below `G(R(πA))`. The
selection runs toward the group with the greater return to skill, B. -/
theorem control_reputation_without_return :
    phi V₀ex V₁ex Hu cex gex (9 / 10) (6 / 10) < G cex gex (R V₀ex V₁ex (9 / 10)) := by
  unfold phi selRate G R dU U V₀ex V₁ex Hu cex gex
  simp only [Fin.sum_univ_two, Finset.sum_filter, Matrix.cons_val_zero, Matrix.cons_val_one]
  norm_num [max_def, min_def]

end KimLoury2012
