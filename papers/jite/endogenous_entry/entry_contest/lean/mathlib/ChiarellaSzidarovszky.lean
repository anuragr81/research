import Mathlib

/-!
# Chiarella and Szidarovszky (2002), "The asymptotic behavior of dynamic rent-seeking games"

Computers and Mathematics with Applications 43, 169-178.  Equation numbers are the printed
ones.

Setup: `n` agents with efforts `x_i`, chance of winning `x_i / Σ x_j`, a prize of one and
costs `c_i`; each adjusts its effort at speed `k_i > 0` in proportion to its marginal profit
(eq. (7)).  At an equilibrium `x̄`, `s̄ = Σ x̄_j`, `Q̄_i = s̄ - x̄_i`, the Jacobian is
`(D + a 1ᵀ)/s̄^3` with `d_i = k_i(-s̄ - s̄^3 c_i'')` and `e_i = k_i(x̄_i - Q̄_i)` (eq. (9)).

## What is proved

1. The game (pp.170-171): the derivatives behind eqs. (3)-(4), their equilibrium forms
   (5)-(6), and a cost with prescribed `c'`, `c''` at `x̄` making `x̄` a best reply
   (`costWitness_best_reply`).
2. Eqs. (7)-(9): the partial derivatives of the adjustment field are the printed entries
   (`field_partial_own`, `field_partial_other`) and the Jacobian is `(D + a 1ᵀ)/s̄^3`
   (`jac_eq`).
3. Eqs. (10)-(12) and Theorem 1(i): the determinant identity (`det_bigM_sub`), eigenvalues
   off the poles solve eq. (12) (`eigen_iff_eq12`), grouping into `γ_l` (`sum_by_group`),
   the eigenvalue `d_l` for `m_l ≥ 2` (`bigM_eigen_pair`, `thm1_i`).
4. Eq. (6) for every agent gives `d_i + e_i < 0` (`d_add_e_neg`), hence `m_l d_l + γ_l < 0`
   for every group (`group_bound`); the paper draws it for the dominant agent only.  At
   most one dominant agent, `γ ≤ 0` without it, eq. (18) (`dominant_unique`, `gamma_nonpos`,
   `eq18`).
5. Case A (Theorem 2, Figure 1): all roots real (`caseA_real`), none above the largest pole
   (`caseA_below_max`), Theorem 2(i)-(iii) (`thm2_i`, `thm2_ii`, `thm2_iii`), `g` falling
   between poles, exactly one root in each gap and one below the smallest pole
   (`g_strictAnti`, `caseA_root_unique`, `caseA_root_between`, `caseA_root_below`).
6. Eqs. (20)-(23) (`term_at_imag`, `eq22_eq23`, `eq23_case_i`).
7. Two parameter sets that satisfy eq. (6), realised from efforts, speeds and `c''`: one
   meets every hypothesis of Theorem 2(ii) with `g(0) < -1` and has the eigenvalue `1/27`
   (`wA_unstable`); one has the eigenvalues `1/100 ± 3i/8` (`wB_unstable_pair`).
8. Examples 1 and 2: the algebra, the printed numbers, the exact root and the printed
   `dλ/dγ ≈ 0.468 + 0.719 i` (`dLam_printed`), positive real part at every purely imaginary
   root of the family (`dLam_re_pos`), and that Example 2 does not satisfy eq. (6)
   (`ex2_not_admissible`, `ex2_printed_group2_fails`).

## Not formalised

Linearised stability (the step from the signs of the eigenvalues to local asymptotic
stability or instability of eq. (7)) and the Hopf bifurcation theorem: neither is in
Mathlib, so Theorems 1-3 are proved as statements about the eigenvalues.  Theorem 3(ii)-(iv)
(root locations in Case B).  The strictly increasing cost in the construction of p.171.

## 1. The game and the conditions at an equilibrium (pp.170-171)
-/

open Real Filter Topology Finset Matrix

namespace ChiarellaSzidarovszky2002

/-- Eqs. (1)-(2): the chance of winning with effort `x` against rival effort `Q`. -/
noncomputable def win (x Q : ℝ) : ℝ := x / (x + Q)

theorem hasDerivAt_win (x Q : ℝ) (h : 0 < x + Q) :
    HasDerivAt (fun y => win y Q) (Q / (x + Q) ^ 2) x := by
  have h1 : HasDerivAt (fun y : ℝ => y + Q) 1 x := (hasDerivAt_id x).add_const Q
  have h2 := (hasDerivAt_id x).div h1 h.ne'
  refine h2.congr_deriv ?_
  simp only [id]
  field_simp
  ring

/-- The derivative of the marginal chance of winning in own effort, the term in eq. (4). -/
theorem hasDerivAt_marginal_own (x Q : ℝ) (h : 0 < x + Q) :
    HasDerivAt (fun y => Q / (y + Q) ^ 2) (-2 * Q / (x + Q) ^ 3) x := by
  have h1 : HasDerivAt (fun y : ℝ => (y + Q) ^ 2) (2 * (x + Q)) x := by
    simpa using ((hasDerivAt_id x).add_const Q).pow 2
  have h2 := (hasDerivAt_const x Q).div h1 (pow_ne_zero 2 h.ne')
  refine h2.congr_deriv ?_
  field_simp
  ring

/-- The derivative of the marginal chance of winning in rival effort, the off-diagonal term
of eq. (8). -/
theorem hasDerivAt_marginal_rival (x Q : ℝ) (h : 0 < x + Q) :
    HasDerivAt (fun q => q / (x + q) ^ 2) ((x - Q) / (x + Q) ^ 3) Q := by
  have h1 : HasDerivAt (fun q : ℝ => (x + q) ^ 2) (2 * (x + Q)) Q := by
    simpa using ((hasDerivAt_id Q).const_add x).pow 2
  have h2 := (hasDerivAt_id Q).div h1 (pow_ne_zero 2 h.ne')
  refine h2.congr_deriv ?_
  simp only [id]
  field_simp
  ring

/-- Eq. (3) at an equilibrium is eq. (5). -/
theorem foc_iff (Q s c1 : ℝ) (hs : 0 < s) : Q / s ^ 2 - c1 = 0 ↔ Q = c1 * s ^ 2 := by
  have : (0 : ℝ) < s ^ 2 := by positivity
  constructor
  · intro h; field_simp at h; linarith
  · intro h; rw [h]; field_simp; ring

/-- Eq. (4) at an equilibrium is eq. (6). -/
theorem soc_iff (Q s c2 : ℝ) : -2 * Q / s ^ 3 - c2 < 0 ↔ -2 * Q / s ^ 3 < c2 := by
  constructor <;> intro h <;> linarith

/-- The construction behind "the last two relations characterise the equilibrium" (p.171):
for `xb > 0`, `Q > 0` and any `c2` satisfying eq. (6) there is a twice differentiable cost
with `c'(xb) = Q/s^2` and `c''(xb) = c2` for which `xb` is a best reply to `Q` over all
efforts.  (The paper also asks the cost to be strictly increasing; that is not shown.) -/
noncomputable def costWitness (xb Q c2 : ℝ) (y : ℝ) : ℝ :=
  y / (y + Q) + (c2 + 2 * Q / (xb + Q) ^ 3) / 2 * (y - xb) ^ 2

/-- The cost's derivative. -/
noncomputable def costWitness' (xb Q c2 : ℝ) (y : ℝ) : ℝ :=
  Q / (y + Q) ^ 2 + (c2 + 2 * Q / (xb + Q) ^ 3) * (y - xb)

theorem costWitness_hasDerivAt (xb Q c2 y : ℝ) (h : 0 < y + Q) :
    HasDerivAt (costWitness xb Q c2) (costWitness' xb Q c2 y) y := by
  have h1 := hasDerivAt_win y Q h
  have h2 : HasDerivAt (fun y : ℝ => (c2 + 2 * Q / (xb + Q) ^ 3) / 2 * (y - xb) ^ 2)
      ((c2 + 2 * Q / (xb + Q) ^ 3) * (y - xb)) y := by
    have := (((hasDerivAt_id y).sub_const xb).pow 2).const_mul ((c2 + 2 * Q / (xb + Q) ^ 3) / 2)
    refine this.congr_deriv ?_
    simp only [id]
    ring
  have := h1.add h2
  unfold costWitness costWitness'
  exact this

theorem costWitness'_at (xb Q c2 : ℝ) :
    costWitness' xb Q c2 xb = Q / (xb + Q) ^ 2 := by
  unfold costWitness'; ring

theorem costWitness'_hasDerivAt (xb Q c2 : ℝ) (h : 0 < xb + Q) :
    HasDerivAt (costWitness' xb Q c2) c2 xb := by
  have h1 := hasDerivAt_marginal_own xb Q h
  have h2 : HasDerivAt (fun y : ℝ => (c2 + 2 * Q / (xb + Q) ^ 3) * (y - xb))
      (c2 + 2 * Q / (xb + Q) ^ 3) xb := by
    simpa using ((hasDerivAt_id xb).sub_const xb).const_mul (c2 + 2 * Q / (xb + Q) ^ 3)
  have := h1.add h2
  refine this.congr_deriv ?_
  ring

theorem costWitness_best_reply (xb Q c2 : ℝ) (h6 : -2 * Q / (xb + Q) ^ 3 < c2) (y : ℝ) :
    win y Q - costWitness xb Q c2 y ≤ win xb Q - costWitness xb Q c2 xb := by
  have hk : 0 ≤ (c2 + 2 * Q / (xb + Q) ^ 3) / 2 := by
    have : -2 * Q / (xb + Q) ^ 3 = -(2 * Q / (xb + Q) ^ 3) := by ring
    linarith
  unfold win costWitness
  have : 0 ≤ (c2 + 2 * Q / (xb + Q) ^ 3) / 2 * (y - xb) ^ 2 := by positivity
  nlinarith

/-! ## 2. The adjustment process and its Jacobian (eqs. (7)-(9)) -/

variable {n : ℕ}

/-- Total rival effort `Q_i`. -/
def rivals (x : Fin n → ℝ) (i : Fin n) : ℝ := ∑ j, x j - x i

theorem rivals_add (x : Fin n → ℝ) (i : Fin n) : x i + rivals x i = ∑ j, x j := by
  unfold rivals; ring

/-- Eq. (7): agent `i` moves its effort in proportion to its marginal profit; `c1 i` is
`c_i'`. -/
noncomputable def field (k : Fin n → ℝ) (c1 : Fin n → ℝ → ℝ) (x : Fin n → ℝ) (i : Fin n) : ℝ :=
  k i * (rivals x i / (x i + rivals x i) ^ 2 - c1 i (x i))

theorem rivals_update_self (x : Fin n → ℝ) (i : Fin n) (t : ℝ) :
    rivals (Function.update x i t) i = rivals x i := by
  unfold rivals
  rw [Finset.sum_update_of_mem (Finset.mem_univ i)]
  simp only [Function.update_self]
  rw [← Finset.add_sum_erase _ _ (Finset.mem_univ i)]
  have : ∑ j ∈ (univ : Finset (Fin n)) \ {i}, x j = ∑ j ∈ univ.erase i, x j := by
    rw [Finset.sdiff_singleton_eq_erase]
  rw [this]; ring

theorem rivals_update_other (x : Fin n → ℝ) {i j : Fin n} (hij : j ≠ i) (t : ℝ) :
    rivals (Function.update x j t) i = rivals x i - x j + t := by
  unfold rivals
  rw [Finset.sum_update_of_mem (Finset.mem_univ j)]
  simp only [Function.update_of_ne hij.symm]
  rw [← Finset.add_sum_erase _ _ (Finset.mem_univ j)]
  have : ∑ l ∈ (univ : Finset (Fin n)) \ {j}, x l = ∑ l ∈ univ.erase j, x l := by
    rw [Finset.sdiff_singleton_eq_erase]
  rw [this]; ring

/-- Eq. (8), diagonal: the own partial derivative of the field at `x`. -/
theorem field_partial_own (k : Fin n → ℝ) (c1 : Fin n → ℝ → ℝ) (c2 : Fin n → ℝ)
    (x : Fin n → ℝ) (i : Fin n) (hs : 0 < ∑ j, x j)
    (hc : HasDerivAt (c1 i) (c2 i) (x i)) :
    HasDerivAt (fun t => field k c1 (Function.update x i t) i)
      (k i * (-2 * rivals x i - (∑ j, x j) ^ 3 * c2 i) / (∑ j, x j) ^ 3) (x i) := by
  have hpos : 0 < x i + rivals x i := by rw [rivals_add]; exact hs
  have h1 := ((hasDerivAt_marginal_own (x i) (rivals x i) hpos).sub hc).const_mul (k i)
  have heq : (fun t => field k c1 (Function.update x i t) i) =
      fun t => k i * (rivals x i / (t + rivals x i) ^ 2 - c1 i t) := by
    funext t; unfold field; rw [rivals_update_self]; simp
  rw [heq]
  refine h1.congr_deriv ?_
  rw [rivals_add]
  field_simp

/-- Eq. (8), off the diagonal: the partial derivative of agent `i`'s field in agent `j`'s
effort. -/
theorem field_partial_other (k : Fin n → ℝ) (c1 : Fin n → ℝ → ℝ) (x : Fin n → ℝ)
    {i j : Fin n} (hij : j ≠ i) (hs : 0 < ∑ l, x l) :
    HasDerivAt (fun t => field k c1 (Function.update x j t) i)
      (k i * (x i - rivals x i) / (∑ l, x l) ^ 3) (x j) := by
  have hpos : 0 < x i + rivals x i := by rw [rivals_add]; exact hs
  have hq : HasDerivAt (fun t : ℝ => rivals x i - x j + t) 1 (x j) := by
    simpa using (hasDerivAt_id (x j)).const_add (rivals x i - x j)
  have hm := hasDerivAt_marginal_rival (x i) (rivals x i) hpos
  have hm' : HasDerivAt (fun t : ℝ => (rivals x i - x j + t) / (x i + (rivals x i - x j + t)) ^ 2)
      ((x i - rivals x i) / (x i + rivals x i) ^ 3 * 1) (x j) := by
    have hm2 : HasDerivAt (fun q => q / (x i + q) ^ 2)
        ((x i - rivals x i) / (x i + rivals x i) ^ 3) (rivals x i - x j + x j) := by
      rw [sub_add_cancel]; exact hm
    exact hm2.comp (x j) hq
  have h1 := (hm'.sub_const (c1 i (x i))).const_mul (k i)
  have heq : (fun t => field k c1 (Function.update x j t) i) =
      fun t => k i * ((rivals x i - x j + t) / (x i + (rivals x i - x j + t)) ^ 2 - c1 i (x i)) := by
    funext t; unfold field; rw [rivals_update_other x hij]
    simp [Function.update_of_ne hij.symm]
  rw [heq]
  refine h1.congr_deriv ?_
  rw [rivals_add]
  ring

/-- The Jacobian printed in eq. (8). -/
noncomputable def jac (k c2 x : Fin n → ℝ) : Matrix (Fin n) (Fin n) ℝ := fun i j =>
  if i = j then k i * (-2 * rivals x i - (∑ l, x l) ^ 3 * c2 i) / (∑ l, x l) ^ 3
  else k i * (x i - rivals x i) / (∑ l, x l) ^ 3

/-- Eq. (9): `d_i` and `e_i`. -/
noncomputable def dvec (k c2 x : Fin n → ℝ) (i : Fin n) : ℝ :=
  k i * (-(∑ l, x l) - (∑ l, x l) ^ 3 * c2 i)

noncomputable def evec (k x : Fin n → ℝ) (i : Fin n) : ℝ := k i * (x i - rivals x i)

/-- The matrix `D + a 1ᵀ` of eq. (8). -/
def bigM {ι : Type*} [DecidableEq ι] {K : Type*} [Field K] (d e : ι → K) : Matrix ι ι K :=
  Matrix.diagonal d + Matrix.vecMulVec e (fun _ => 1)

/-- Eq. (8): `J = (D + a 1ᵀ) / s^3`. -/
theorem jac_eq (k c2 x : Fin n → ℝ) :
    jac k c2 x = (1 / (∑ l, x l) ^ 3) • bigM (dvec k c2 x) (evec k x) := by
  ext i j
  unfold jac bigM dvec evec
  by_cases hij : i = j
  · subst hij
    simp only [if_true, Matrix.smul_apply, Matrix.add_apply, Matrix.diagonal_apply_eq,
      Matrix.vecMulVec_apply, smul_eq_mul]
    have := rivals_add x i
    rw [div_eq_mul_one_div, mul_comm]
    congr 1
    rw [← this]; ring
  · simp only [hij, if_false, Matrix.smul_apply, Matrix.add_apply,
      Matrix.diagonal_apply_ne _ hij, Matrix.vecMulVec_apply, smul_eq_mul]
    ring

/-! ## 3. The spectrum of `D + a 1ᵀ` (eqs. (10)-(12) and Theorem 1) -/

section Spectrum

variable {ι : Type*} [Fintype ι] [DecidableEq ι] {K : Type*} [Field K]

theorem bigM_mulVec (d e v : ι → K) :
    bigM d e *ᵥ v = fun i => d i * v i + e i * ∑ j, v j := by
  ext i
  simp only [bigM, Matrix.add_mulVec, Pi.add_apply, Matrix.mulVec_diagonal]
  congr 1
  simp [Matrix.mulVec, dotProduct, Matrix.vecMulVec_apply, Finset.mul_sum]

/-- A vector supported on agents with a common `d` and summing to zero is an eigenvector:
the source of the eigenvalue `d_l` of multiplicity `m_l - 1` (p.172). -/
theorem bigM_eigen_of_sum_zero (d e v : ι → K) (δ : K) (hsum : ∑ j, v j = 0)
    (hd : ∀ i, v i ≠ 0 → d i = δ) : bigM d e *ᵥ v = δ • v := by
  rw [bigM_mulVec]
  ext i
  simp only [hsum, mul_zero, add_zero, Pi.smul_apply, smul_eq_mul]
  by_cases hv : v i = 0
  · simp [hv]
  · rw [hd i hv]

/-- Two agents with the same `d` give the eigenvalue `d` (p.172, `m_l ≥ 2`). -/
theorem bigM_eigen_pair (d e : ι → K) {i j : ι} (hij : i ≠ j) (h : d i = d j) :
    (Pi.single i 1 - Pi.single j 1 : ι → K) ≠ 0 ∧
    bigM d e *ᵥ (Pi.single i 1 - Pi.single j 1) = d i • (Pi.single i 1 - Pi.single j 1) := by
  constructor
  · intro h0
    have := congrFun h0 i
    simp [hij] at this
  · apply bigM_eigen_of_sum_zero
    · simp [Finset.sum_sub_distrib]
    · intro l hl
      by_cases hli : l = i
      · rw [hli]
      · by_cases hlj : l = j
        · rw [hlj, h]
        · simp [hli, hlj] at hl

/-- Theorem 1(i): two agents with the same `d > 0` give the Jacobian a positive real
eigenvalue `d / s^3`. -/
theorem thm1_i (k c2 x : Fin n → ℝ) (hs : 0 < ∑ l, x l) {i j : Fin n} (hij : i ≠ j)
    (h : dvec k c2 x i = dvec k c2 x j) (hpos : 0 < dvec k c2 x i) :
    ∃ v : Fin n → ℝ, v ≠ 0 ∧ 0 < dvec k c2 x i / (∑ l, x l) ^ 3 ∧
      jac k c2 x *ᵥ v = (dvec k c2 x i / (∑ l, x l) ^ 3) • v := by
  obtain ⟨hne, hv⟩ := bigM_eigen_pair (dvec k c2 x) (evec k x) hij h
  refine ⟨_, hne, by positivity, ?_⟩
  rw [jac_eq, Matrix.smul_mulVec, hv, smul_smul]
  congr 1
  ring

theorem bigM_sub_eq (d e : ι → K) (μ : K) (h : ∀ i, d i ≠ μ) :
    bigM d e - μ • (1 : Matrix ι ι K) =
      Matrix.diagonal (fun i => d i - μ) *
        (1 + Matrix.replicateCol (Fin 1) (fun i => e i / (d i - μ)) *
          Matrix.replicateRow (Fin 1) (fun _ => (1 : K))) := by
  rw [← Matrix.vecMulVec_eq, Matrix.mul_add, Matrix.mul_one]
  ext i j
  have hi : d i - μ ≠ 0 := sub_ne_zero.mpr (h i)
  by_cases hij : i = j
  · subst hij
    simp [bigM, Matrix.diagonal_mul, Matrix.vecMulVec_apply]
    field_simp
    ring
  · simp [bigM, Matrix.diagonal_mul, Matrix.vecMulVec_apply, hij]
    field_simp

/-- Eq. (10): the characteristic polynomial of `D + a 1ᵀ` off the poles. -/
theorem det_bigM_sub (d e : ι → K) (μ : K) (h : ∀ i, d i ≠ μ) :
    (bigM d e - μ • (1 : Matrix ι ι K)).det =
      (∏ i, (d i - μ)) * (1 + ∑ i, e i / (d i - μ)) := by
  rw [bigM_sub_eq d e μ h, Matrix.det_mul, Matrix.det_diagonal,
    Matrix.det_one_add_replicateCol_mul_replicateRow]
  simp [dotProduct]

/-- Eqs. (10)-(12): off the poles, `μ` is an eigenvalue exactly when it solves eq. (12). -/
theorem eigen_iff_eq12 (d e : ι → K) (μ : K) (h : ∀ i, d i ≠ μ) :
    (bigM d e - μ • (1 : Matrix ι ι K)).det = 0 ↔ 1 + ∑ i, e i / (d i - μ) = 0 := by
  rw [det_bigM_sub d e μ h]
  have hp : (∏ i, (d i - μ)) ≠ 0 :=
    Finset.prod_ne_zero_iff.mpr (fun i _ => sub_ne_zero.mpr (h i))
  constructor
  · intro h0; exact (mul_eq_zero.mp h0).resolve_left hp
  · intro h0; rw [h0, mul_zero]

omit [DecidableEq ι] in
/-- Eq. (11): agents with a common `d_l` enter eq. (12) through `γ_l = Σ_{i ∈ I_l} e_i`. -/
theorem sum_by_group {κ : Type*} [Fintype κ] [DecidableEq κ] (grp : ι → κ) (dl : κ → K)
    (e : ι → K) (μ : K) :
    ∑ i, e i / (dl (grp i) - μ) =
      ∑ l, (∑ i ∈ univ.filter (fun i => grp i = l), e i) / (dl l - μ) := by
  rw [← Finset.sum_fiberwise univ grp (fun i => e i / (dl (grp i) - μ))]
  refine Finset.sum_congr rfl (fun l _ => ?_)
  rw [Finset.sum_div]
  refine Finset.sum_congr rfl (fun i hi => ?_)
  rw [(Finset.mem_filter.mp hi).2]

end Spectrum

/-! ### Scope of "if `m_l = 1` then `λ = d_l` is not an eigenvalue" (p.172) -/

/-- In the symmetric duopoly every `e_i` is zero. -/
theorem evec_symmetric_duopoly (k x : Fin 2 → ℝ) (h : x 0 = x 1) : evec k x = 0 := by
  ext i
  fin_cases i <;> simp [evec, rivals, Fin.sum_univ_two, h]

/-- With `e = 0` every `d_i` is an eigenvalue, also when `m_l = 1`: the sentence on p.172
needs `γ_l ≠ 0`. -/
theorem eigen_of_e_zero {ι : Type*} [Fintype ι] [DecidableEq ι] (d : ι → ℝ) (i : ι) :
    bigM d 0 *ᵥ (Pi.single i 1 : ι → ℝ) = d i • (Pi.single i 1 : ι → ℝ) := by
  rw [bigM_mulVec]
  ext l
  by_cases hl : l = i
  · subst hl; simp
  · simp [Pi.single_apply, hl]

/-! ## 4. What eq. (6) implies for every agent and every group (pp.173-174) -/

/-- Eq. (6) and `k_i > 0` give `d_i + e_i < 0` for every agent.  The paper draws this for
the dominant agent only (p.174). -/
theorem d_add_e_neg (k c2 x : Fin n → ℝ) (i : Fin n) (hk : 0 < k i) (hs : 0 < ∑ l, x l)
    (h6 : -2 * rivals x i / (∑ l, x l) ^ 3 < c2 i) :
    dvec k c2 x i + evec k x i < 0 := by
  have hs3 : 0 < (∑ l, x l) ^ 3 := by positivity
  have h6' : -2 * rivals x i < (∑ l, x l) ^ 3 * c2 i := by
    rw [div_lt_iff₀ hs3] at h6; linarith
  have hsum := rivals_add x i
  unfold dvec evec
  have : k i * (-(∑ l, x l) - (∑ l, x l) ^ 3 * c2 i) + k i * (x i - rivals x i) =
      k i * (-2 * rivals x i - (∑ l, x l) ^ 3 * c2 i) := by rw [← hsum]; ring
  rw [this]
  have : -2 * rivals x i - (∑ l, x l) ^ 3 * c2 i < 0 := by linarith
  nlinarith

/-- Summed over a group with common `d = δ`: `m δ + γ < 0`. -/
theorem group_bound {ι : Type*} (d e : ι → ℝ) (I : Finset ι) (δ : ℝ) (hI : I.Nonempty)
    (hd : ∀ i ∈ I, d i = δ) (hsoc : ∀ i ∈ I, d i + e i < 0) :
    (I.card : ℝ) * δ + ∑ i ∈ I, e i < 0 := by
  have h1 : ∑ i ∈ I, (d i + e i) < ∑ i ∈ I, (0 : ℝ) :=
    Finset.sum_lt_sum_of_nonempty hI hsoc
  rw [Finset.sum_add_distrib, Finset.sum_congr rfl hd] at h1
  simpa [Finset.sum_const, nsmul_eq_mul] using h1

/-- A group with `γ ≤ 0` has `δ + γ < 0`. -/
theorem group_d_add_gamma_neg {ι : Type*} (d e : ι → ℝ) (I : Finset ι) (δ : ℝ)
    (hI : I.Nonempty) (hd : ∀ i ∈ I, d i = δ) (hsoc : ∀ i ∈ I, d i + e i < 0)
    (hγ : ∑ i ∈ I, e i ≤ 0) : δ + ∑ i ∈ I, e i < 0 := by
  have hb := group_bound d e I δ hI hd hsoc
  have hc : (1 : ℝ) ≤ I.card := by exact_mod_cast hI.card_pos
  by_cases hδ : δ ≤ 0
  · rcases lt_or_eq_of_le hδ with h | h
    · linarith
    · subst h; simp at hb; linarith
  · push_neg at hδ; nlinarith

/-- At most one agent spends more than all the others together (p.173). -/
theorem dominant_unique (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i) {i j : Fin n} (hij : i ≠ j)
    (hi : rivals x i < x i) (hj : rivals x j < x j) : False := by
  have hpair : x i + x j ≤ ∑ l, x l := by
    rw [← Finset.sum_pair hij]
    exact Finset.sum_le_sum_of_subset_of_nonneg (Finset.subset_univ _) (fun l _ _ => hx l)
  unfold rivals at hi hj
  linarith

/-- A group without the dominant agent has `γ ≤ 0` (p.173). -/
theorem gamma_nonpos (k x : Fin n → ℝ) (I : Finset (Fin n)) (hk : ∀ i, 0 ≤ k i)
    (hnd : ∀ i ∈ I, x i ≤ rivals x i) : ∑ i ∈ I, evec k x i ≤ 0 := by
  apply Finset.sum_nonpos
  intro i hi
  unfold evec
  have := hnd i hi
  have := hk i
  nlinarith

/-- Eq. (18): the group of the dominant agent `p` has `d_{l*} < -γ_{l*} < 0`. -/
theorem eq18 {ι : Type*} [DecidableEq ι] (d e : ι → ℝ) (I : Finset ι) (δ : ℝ) {p : ι}
    (hp : p ∈ I) (hd : d p = δ) (hsoc : d p + e p < 0)
    (hothers : ∀ i ∈ I, i ≠ p → e i ≤ 0) (hγ : 0 < ∑ i ∈ I, e i) :
    δ < -∑ i ∈ I, e i ∧ -∑ i ∈ I, e i < 0 := by
  have hsplit := Finset.add_sum_erase I e hp
  have hrest : ∑ i ∈ I.erase p, e i ≤ 0 :=
    Finset.sum_nonpos (fun i hi => hothers i (Finset.mem_of_mem_erase hi) (Finset.ne_of_mem_erase hi))
  constructor <;> linarith

/-- Theorem 3(i): its branch `d_r ≥ 0` is empty, since eq. (18) gives `d_{l*} < 0`. -/
theorem thm3_i_branch_empty {ι : Type*} [DecidableEq ι] (d e : ι → ℝ) (I : Finset ι) (δ : ℝ)
    {p : ι} (hp : p ∈ I) (hd : d p = δ) (hsoc : d p + e p < 0)
    (hothers : ∀ i ∈ I, i ≠ p → e i ≤ 0) (hγ : 0 < ∑ i ∈ I, e i) : δ < 0 := by
  have := eq18 d e I δ hp hd hsoc hothers hγ
  linarith [this.1, this.2]

/-! ## 5. Case A: `γ_l ≤ 0` in every group (Theorem 2, Figure 1) -/

section CaseA

variable {κ : Type*} [Fintype κ]

/-- Eq. (15). -/
noncomputable def g (d γ : κ → ℝ) (μ : ℝ) : ℝ := ∑ l, γ l / (d l - μ)

/-- The left side of eq. (12) less one, at a complex point. -/
noncomputable def gC (d γ : κ → ℝ) (μ : ℂ) : ℂ := ∑ l, (γ l : ℂ) / ((d l : ℂ) - μ)

theorem gC_ofReal (d γ : κ → ℝ) (μ : ℝ) : gC d γ (μ : ℂ) = (g d γ μ : ℂ) := by
  unfold gC g
  push_cast
  rfl

theorem im_term (γ d : ℝ) (μ : ℂ) :
    ((γ : ℂ) / ((d : ℂ) - μ)).im = γ * μ.im / ((d - μ.re) ^ 2 + μ.im ^ 2) := by
  simp [Complex.div_im, Complex.normSq_apply]
  ring

/-- Case A (p.173): every root of eq. (12) is real. -/
theorem caseA_real (d γ : κ → ℝ) (hγ : ∀ l, γ l ≤ 0) (μ : ℂ)
    (h12 : 1 + gC d γ μ = 0) : μ.im = 0 := by
  by_contra hy
  have him := congrArg Complex.im h12
  simp only [Complex.add_im, Complex.one_im, zero_add, Complex.zero_im, gC, Complex.im_sum,
    im_term] at him
  have hpos : ∀ l, 0 < (d l - μ.re) ^ 2 + μ.im ^ 2 := fun l => by
    have : 0 < μ.im ^ 2 := by positivity
    positivity
  have hfac : ∑ l, γ l * μ.im / ((d l - μ.re) ^ 2 + μ.im ^ 2) =
      μ.im * ∑ l, γ l / ((d l - μ.re) ^ 2 + μ.im ^ 2) := by
    rw [Finset.mul_sum]; refine Finset.sum_congr rfl (fun l _ => ?_); ring
  rw [hfac] at him
  have hsum : ∑ l, γ l / ((d l - μ.re) ^ 2 + μ.im ^ 2) = 0 :=
    (mul_eq_zero.mp him).resolve_left hy
  have hnp : ∀ l ∈ (univ : Finset κ), γ l / ((d l - μ.re) ^ 2 + μ.im ^ 2) ≤ 0 :=
    fun l _ => div_nonpos_of_nonpos_of_nonneg (hγ l) (hpos l).le
  have hz := (Finset.sum_eq_zero_iff_of_nonpos hnp).mp hsum
  have hγ0 : ∀ l, γ l = 0 := fun l => by
    have := hz l (Finset.mem_univ l)
    rcases div_eq_zero_iff.mp this with h | h
    · exact h
    · exact absurd h (hpos l).ne'
  have : gC d γ μ = 0 := by simp [gC, hγ0]
  rw [this] at h12
  simp at h12

/-- Case A: no root lies at or above the largest `d_l`. -/
theorem caseA_below_max (d γ : κ → ℝ) (hγ : ∀ l, γ l ≤ 0) (μ : ℝ)
    (hpole : ∀ l, d l ≠ μ) (h12 : 1 + g d γ μ = 0) : ∃ l, μ < d l := by
  by_contra hno
  push_neg at hno
  have hterm : ∀ l ∈ (univ : Finset κ), 0 ≤ γ l / (d l - μ) := fun l _ => by
    have : d l - μ < 0 := by
      have := lt_of_le_of_ne (hno l) (hpole l); linarith
    exact div_nonneg_of_nonpos (hγ l) this.le
  have : 0 ≤ g d γ μ := Finset.sum_nonneg hterm
  linarith

/-- Theorem 2(i): if every `γ_l ≤ 0` and every `d_l ≤ 0`, every root of eq. (12) is real
and negative. -/
theorem thm2_i [Nonempty κ] (d γ : κ → ℝ) (hγ : ∀ l, γ l ≤ 0) (hd : ∀ l, d l ≤ 0) (μ : ℂ)
    (hpole : ∀ l, (d l : ℂ) ≠ μ) (h12 : 1 + gC d γ μ = 0) : μ.im = 0 ∧ μ.re < 0 := by
  have him := caseA_real d γ hγ μ h12
  refine ⟨him, ?_⟩
  have hμ : μ = (μ.re : ℂ) := Complex.ext (by simp) (by simp [him])
  rw [hμ, gC_ofReal] at h12
  have h12r : 1 + g d γ μ.re = 0 := by exact_mod_cast h12
  have hpr : ∀ l, d l ≠ μ.re := fun l h => hpole l (by rw [hμ, h])
  obtain ⟨l, hl⟩ := caseA_below_max d γ hγ μ.re hpr h12r
  linarith [hd l]

/-- Eq. (16): `g` falls strictly between consecutive poles when every `γ_l < 0`. -/
theorem g_strictAnti [Nonempty κ] (d γ : κ → ℝ) (hγ : ∀ l, γ l < 0) {μ ν : ℝ} (hμν : μ < ν)
    (hsep : ∀ l, d l < μ ∨ ν < d l) : g d γ ν < g d γ μ := by
  apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
  intro l _
  have hprod : 0 < (d l - ν) * (d l - μ) := by
    rcases hsep l with h | h
    · exact mul_pos_of_neg_of_neg (by linarith) (by linarith)
    · exact mul_pos (by linarith) (by linarith)
  have h1 : d l - ν ≠ 0 := by intro h0; rw [h0, zero_mul] at hprod; exact lt_irrefl 0 hprod
  have h2 : d l - μ ≠ 0 := by intro h0; rw [h0, mul_zero] at hprod; exact lt_irrefl 0 hprod
  have key : γ l / (d l - ν) - γ l / (d l - μ) = γ l * (ν - μ) / ((d l - ν) * (d l - μ)) := by
    field_simp; ring
  have : γ l * (ν - μ) / ((d l - ν) * (d l - μ)) < 0 :=
    div_neg_of_neg_of_pos (mul_neg_of_neg_of_pos (hγ l) (by linarith)) hprod
  linarith

/-- In Case A two roots of eq. (12) between the same consecutive poles coincide. -/
theorem caseA_root_unique [Nonempty κ] (d γ : κ → ℝ) (hγ : ∀ l, γ l < 0) {μ ν : ℝ}
    (hsep : ∀ l, d l < min μ ν ∨ max μ ν < d l) (hμ : 1 + g d γ μ = 0)
    (hν : 1 + g d γ ν = 0) : μ = ν := by
  by_contra hne
  rcases lt_or_gt_of_ne hne with h | h
  · have := g_strictAnti d γ hγ h (fun l => by
      rcases hsep l with h' | h'
      · left; simpa [min_eq_left h.le] using h'
      · right; simpa [max_eq_right h.le] using h')
    linarith
  · have := g_strictAnti d γ hγ h (fun l => by
      rcases hsep l with h' | h'
      · left; simpa [min_eq_right h.le] using h'
      · right; simpa [max_eq_left h.le] using h')
    linarith

/-- Theorem 2(ii): with the root `λ*` and `0` between the same consecutive poles,
`λ* < 0` exactly when `g(0) < -1`, and `λ* > 0` exactly when `g(0) > -1`. -/
theorem thm2_ii [Nonempty κ] (d γ : κ → ℝ) (hγ : ∀ l, γ l < 0) {μ : ℝ}
    (hsep : ∀ l, d l < min μ 0 ∨ max μ 0 < d l) (hμ : 1 + g d γ μ = 0) :
    (μ < 0 ↔ g d γ 0 < -1) ∧ (0 < μ ↔ -1 < g d γ 0) := by
  have sepA : μ < 0 → ∀ l, d l < μ ∨ 0 < d l := fun h l => by
    rcases hsep l with h' | h'
    · left; simpa [min_eq_left h.le] using h'
    · right; simpa [max_eq_right h.le] using h'
  have sepB : 0 < μ → ∀ l, d l < 0 ∨ μ < d l := fun h l => by
    rcases hsep l with h' | h'
    · left; simpa [min_eq_right h.le] using h'
    · right; simpa [max_eq_left h.le] using h'
  have hA : μ < 0 → g d γ 0 < -1 := fun h => by
    have := g_strictAnti d γ hγ h (sepA h); linarith
  have hB : 0 < μ → -1 < g d γ 0 := fun h => by
    have := g_strictAnti d γ hγ h (sepB h); linarith
  refine ⟨⟨hA, fun h => ?_⟩, ⟨hB, fun h => ?_⟩⟩
  · rcases lt_trichotomy μ 0 with h' | h' | h'
    · exact h'
    · subst h'; linarith
    · linarith [hB h']
  · rcases lt_trichotomy μ 0 with h' | h' | h'
    · linarith [hA h']
    · subst h'; linarith
    · exact h'

theorem tendsto_term_right (γ d : ℝ) (hγ : γ < 0) :
    Tendsto (fun μ => γ / (d - μ)) (𝓝[>] d) atTop := by
  have h1 : Tendsto (fun μ : ℝ => d - μ) (𝓝[>] d) (𝓝[<] 0) := by
    apply tendsto_nhdsWithin_of_tendsto_nhds_of_eventually_within
    · have : Tendsto (fun μ : ℝ => d - μ) (𝓝 d) (𝓝 (d - d)) :=
        tendsto_const_nhds.sub tendsto_id
      rw [sub_self] at this
      exact this.mono_left nhdsWithin_le_nhds
    · filter_upwards [self_mem_nhdsWithin] with μ hμ
      simp only [Set.mem_Iio]; simp only [Set.mem_Ioi] at hμ; linarith
  have h2 := (tendsto_inv_nhdsLT_zero.comp h1).const_mul_atBot_of_neg hγ
  refine h2.congr (fun μ => ?_)
  simp [div_eq_mul_inv]

theorem tendsto_term_left (γ d : ℝ) (hγ : γ < 0) :
    Tendsto (fun μ => γ / (d - μ)) (𝓝[<] d) atBot := by
  have h1 : Tendsto (fun μ : ℝ => d - μ) (𝓝[<] d) (𝓝[>] 0) := by
    apply tendsto_nhdsWithin_of_tendsto_nhds_of_eventually_within
    · have : Tendsto (fun μ : ℝ => d - μ) (𝓝 d) (𝓝 (d - d)) :=
        tendsto_const_nhds.sub tendsto_id
      rw [sub_self] at this
      exact this.mono_left nhdsWithin_le_nhds
    · filter_upwards [self_mem_nhdsWithin] with μ hμ
      simp only [Set.mem_Ioi]; simp only [Set.mem_Iio] at hμ; linarith
  have h2 := (tendsto_inv_nhdsGT_zero.comp h1).const_mul_atTop_of_neg hγ
  refine h2.congr (fun μ => ?_)
  simp [div_eq_mul_inv]

variable [DecidableEq κ]

theorem tendsto_rest (d γ : κ → ℝ) (hinj : Function.Injective d) (l0 : κ) (F : Filter ℝ)
    (hF : F ≤ 𝓝 (d l0)) :
    Tendsto (fun μ => ∑ l ∈ univ.erase l0, γ l / (d l - μ)) F
      (𝓝 (∑ l ∈ univ.erase l0, γ l / (d l - d l0))) := by
  apply tendsto_finset_sum
  intro l hl
  have hne : d l - d l0 ≠ 0 := sub_ne_zero.mpr (fun h => (Finset.ne_of_mem_erase hl) (hinj h))
  have h1 : Tendsto (fun μ : ℝ => d l - μ) F (𝓝 (d l - d l0)) :=
    ((continuous_const.sub continuous_id).tendsto (d l0)).mono_left hF
  exact (tendsto_const_nhds (x := γ l)).div h1 hne

theorem g_split (d γ : κ → ℝ) (l0 : κ) (μ : ℝ) :
    g d γ μ = γ l0 / (d l0 - μ) + ∑ l ∈ univ.erase l0, γ l / (d l - μ) := by
  unfold g
  exact (Finset.add_sum_erase _ _ (Finset.mem_univ l0)).symm

omit [DecidableEq κ] in
theorem g_continuousOn (d γ : κ → ℝ) (S : Set ℝ) (hS : ∀ μ ∈ S, ∀ l, d l ≠ μ) :
    ContinuousOn (g d γ) S := by
  unfold g
  apply continuousOn_finset_sum
  intro l _
  apply ContinuousOn.div continuousOn_const (continuousOn_const.sub continuousOn_id)
  intro μ hμ
  exact sub_ne_zero.mpr (hS μ hμ l)

/-- Figure 1 and Theorem 2(iii): in Case A there is a root of eq. (12) between two
consecutive poles. -/
theorem caseA_root_between (d γ : κ → ℝ) (hinj : Function.Injective d) (hγ : ∀ l, γ l < 0)
    (lo hi : κ) (hlh : d lo < d hi) (hgap : ∀ l, d l ≤ d lo ∨ d hi ≤ d l) :
    ∃ μ, d lo < μ ∧ μ < d hi ∧ 1 + g d γ μ = 0 := by
  have hS : ∀ μ ∈ Set.Ioo (d lo) (d hi), ∀ l, d l ≠ μ := fun μ hμ l h => by
    rcases hgap l with h' | h' <;> [linarith [hμ.1]; linarith [hμ.2]]
  have hlo : Tendsto (g d γ) (𝓝[>] (d lo)) atTop := by
    have := (tendsto_term_right (γ lo) (d lo) (hγ lo)).atTop_add
      (tendsto_rest d γ hinj lo _ nhdsWithin_le_nhds)
    exact this.congr (fun μ => (g_split d γ lo μ).symm)
  have hhi : Tendsto (g d γ) (𝓝[<] (d hi)) atBot := by
    have := (tendsto_term_left (γ hi) (d hi) (hγ hi)).atBot_add
      (tendsto_rest d γ hinj hi _ nhdsWithin_le_nhds)
    exact this.congr (fun μ => (g_split d γ hi μ).symm)
  set m := (d lo + d hi) / 2 with hm
  obtain ⟨p, hp1, hp2⟩ := ((hlo.eventually (eventually_gt_atTop (-1))).and
    (Ioo_mem_nhdsGT (show d lo < m by rw [hm]; linarith))).exists
  obtain ⟨q, hq1, hq2⟩ := ((hhi.eventually (eventually_lt_atBot (-1))).and
    (Ioo_mem_nhdsLT (show m < d hi by rw [hm]; linarith))).exists
  have hpq : p ≤ q := by linarith [hp2.2, hq2.1]
  have hcont : ContinuousOn (g d γ) (Set.Icc p q) :=
    (g_continuousOn d γ _ hS).mono (fun x hx => ⟨by linarith [hp2.1, hx.1], by linarith [hq2.2, hx.2]⟩)
  obtain ⟨c, hc, hgc⟩ := intermediate_value_Icc' hpq hcont ⟨hq1.le, hp1.le⟩
  exact ⟨c, by linarith [hp2.1, hc.1], by linarith [hq2.2, hc.2], by rw [hgc]; ring⟩

/-- Theorem 2(iii): if `d_{r-1} ≥ 0` there is a positive root of eq. (12). -/
theorem thm2_iii (d γ : κ → ℝ) (hinj : Function.Injective d) (hγ : ∀ l, γ l < 0)
    (lo hi : κ) (hlh : d lo < d hi) (hgap : ∀ l, d l ≤ d lo ∨ d hi ≤ d l) (h0 : 0 ≤ d lo) :
    ∃ μ, 0 < μ ∧ 1 + g d γ μ = 0 := by
  obtain ⟨μ, h1, _, h3⟩ := caseA_root_between d γ hinj hγ lo hi hlh hgap
  exact ⟨μ, by linarith, h3⟩


/-- Figure 1: in Case A there is a root of eq. (12) below the smallest pole. -/
theorem caseA_root_below (d γ : κ → ℝ) (hinj : Function.Injective d) (hγ : ∀ l, γ l < 0)
    (lo : κ) (hmin : ∀ l, d lo ≤ d l) : ∃ μ, μ < d lo ∧ 1 + g d γ μ = 0 := by
  set S := ∑ l, -γ l with hSdef
  have hS : 0 ≤ S := Finset.sum_nonneg (fun l _ => by linarith [hγ l])
  set p := d lo - S - 1 with hp
  have hgp : -1 < g d γ p := by
    have hterm : ∀ l ∈ (univ : Finset κ), γ l / (S + 1) ≤ γ l / (d l - p) := fun l _ => by
      have hD : S + 1 ≤ d l - p := by rw [hp]; linarith [hmin l]
      rw [div_le_div_iff₀ (by linarith) (by linarith)]
      nlinarith [hγ l]
    have h1 : ∑ l, γ l / (S + 1) ≤ g d γ p := Finset.sum_le_sum hterm
    have h2 : ∑ l, γ l / (S + 1) = -S / (S + 1) := by
      rw [← Finset.sum_div, hSdef, Finset.sum_neg_distrib, neg_neg]
    have h3 : -1 < -S / (S + 1) := by
      rw [lt_div_iff₀ (by linarith)]; linarith
    linarith
  have hhi : Tendsto (g d γ) (𝓝[<] (d lo)) atBot := by
    have := (tendsto_term_left (γ lo) (d lo) (hγ lo)).atBot_add
      (tendsto_rest d γ hinj lo _ nhdsWithin_le_nhds)
    exact this.congr (fun μ => (g_split d γ lo μ).symm)
  have hpl : p < d lo := by rw [hp]; linarith
  obtain ⟨q, hq1, hq2⟩ := ((hhi.eventually (eventually_lt_atBot (-1))).and
    (Ioo_mem_nhdsLT hpl)).exists
  have hcont : ContinuousOn (g d γ) (Set.Icc p q) :=
    g_continuousOn d γ _ (fun μ hμ l h => by linarith [hmin l, hμ.2, hq2.2])
  obtain ⟨c, hc, hgc⟩ := intermediate_value_Icc' hq2.1.le hcont ⟨hq1.le, hgp.le⟩
  exact ⟨c, by linarith [hc.2, hq2.2], by rw [hgc]; ring⟩

end CaseA

/-! ## 6. Purely imaginary roots in Case B (eqs. (20)-(23)) -/

/-- Eq. (21) term by term: real and imaginary parts of `γ / (d - iα)`. -/
theorem term_at_imag (γ d α : ℝ) (h : 0 < d ^ 2 + α ^ 2) :
    (γ : ℂ) / ((d : ℂ) - α * Complex.I) =
      ((γ * d / (d ^ 2 + α ^ 2) : ℝ) : ℂ) + ((γ * α / (d ^ 2 + α ^ 2) : ℝ) : ℂ) * Complex.I := by
  have hne : (d : ℂ) - α * Complex.I ≠ 0 := by
    intro h0
    have h1 := congrArg Complex.re h0
    have h2 := congrArg Complex.im h0
    simp at h1 h2
    subst h1; subst h2; simp at h
  rw [div_eq_iff hne]
  generalize hA : γ * d / (d ^ 2 + α ^ 2) = A
  generalize hB : γ * α / (d ^ 2 + α ^ 2) = B
  have hA' : A * (d ^ 2 + α ^ 2) = γ * d := by rw [← hA]; field_simp
  have hB' : B * (d ^ 2 + α ^ 2) = γ * α := by rw [← hB]; field_simp
  simp only [Complex.ext_iff, Complex.mul_re, Complex.mul_im, Complex.add_re, Complex.add_im,
    Complex.sub_re, Complex.sub_im, Complex.ofReal_re, Complex.ofReal_im, Complex.I_re,
    Complex.I_im]
  constructor
  · have : (A * d + B * α) * (d ^ 2 + α ^ 2) = γ * (d ^ 2 + α ^ 2) := by
      linear_combination d * hA' + α * hB'
    have := mul_right_cancel₀ h.ne' this
    linarith
  · have : (B * d - A * α) * (d ^ 2 + α ^ 2) = 0 * (d ^ 2 + α ^ 2) := by
      linear_combination d * hB' - α * hA'
    have := mul_right_cancel₀ h.ne' this
    linarith

/-- Eqs. (22) and (23) from eq. (21) when `α ≠ 0`; groups `1, ..., r-1` are indexed by `κ`
and group `r` is written out. -/
theorem eq22_eq23 {κ : Type*} [Fintype κ] (γ d : κ → ℝ) (γr dr α : ℝ) (hα : α ≠ 0)
    (hre : ∑ l, γ l * d l / (d l ^ 2 + α ^ 2) + γr * dr / (dr ^ 2 + α ^ 2) = -1)
    (him : ∑ l, γ l * α / (d l ^ 2 + α ^ 2) + γr * α / (dr ^ 2 + α ^ 2) = 0) :
    γr = -(dr ^ 2 + α ^ 2) * ∑ l, γ l / (d l ^ 2 + α ^ 2) ∧
      ∑ l, γ l * (d l - dr) / (d l ^ 2 + α ^ 2) = -1 := by
  have hNr : 0 < dr ^ 2 + α ^ 2 := by positivity
  have him' : ∑ l, γ l / (d l ^ 2 + α ^ 2) + γr / (dr ^ 2 + α ^ 2) = 0 := by
    have : α * (∑ l, γ l / (d l ^ 2 + α ^ 2) + γr / (dr ^ 2 + α ^ 2)) = 0 := by
      rw [← him, mul_add, Finset.mul_sum]
      congr 1
      · exact Finset.sum_congr rfl (fun l _ => by ring)
      · ring
    exact (mul_eq_zero.mp this).resolve_left hα
  constructor
  · have : γr / (dr ^ 2 + α ^ 2) = -∑ l, γ l / (d l ^ 2 + α ^ 2) := by linarith
    field_simp at this ⊢
    linarith
  · have hsplit : ∑ l, γ l * (d l - dr) / (d l ^ 2 + α ^ 2) =
        ∑ l, γ l * d l / (d l ^ 2 + α ^ 2) - dr * ∑ l, γ l / (d l ^ 2 + α ^ 2) := by
      rw [Finset.mul_sum, ← Finset.sum_sub_distrib]
      exact Finset.sum_congr rfl (fun l _ => by ring)
    have hr : γr * dr / (dr ^ 2 + α ^ 2) = dr * (γr / (dr ^ 2 + α ^ 2)) := by ring
    rw [hsplit]
    have : γr / (dr ^ 2 + α ^ 2) = -∑ l, γ l / (d l ^ 2 + α ^ 2) := by linarith
    rw [hr, this] at hre
    linarith

/-- Theorem 3(i) in eq. (23): with every `γ_l < 0` and `d_l < d_r` the left side is
positive, so there is no purely imaginary root (p.176). -/
theorem eq23_case_i {κ : Type*} [Fintype κ] (γ d : κ → ℝ) (dr α : ℝ) (hα : α ≠ 0)
    (hγ : ∀ l, γ l < 0) (hd : ∀ l, d l < dr) :
    0 ≤ ∑ l, γ l * (d l - dr) / (d l ^ 2 + α ^ 2) := by
  apply Finset.sum_nonneg
  intro l _
  have : 0 < d l ^ 2 + α ^ 2 := by positivity
  have : 0 < γ l * (d l - dr) := mul_pos_of_neg_of_neg (hγ l) (by linarith [hd l])
  positivity

/-! ## 7. Two parameter sets that satisfy eq. (6) -/

/-- Theorem 2(ii) needs Theorem 1(ii)'s hypothesis `d_l < 0` whenever `m_l ≥ 2`.  Three
agents, `x̄ = (1,1,1)`, `k = (1/2,2,2)`, `c'' = (-1/27,-7/54,-7/54)`. -/
noncomputable def wA_x : Fin 3 → ℝ := ![1, 1, 1]
noncomputable def wA_k : Fin 3 → ℝ := ![1 / 2, 2, 2]
noncomputable def wA_c2 : Fin 3 → ℝ := ![-1 / 27, -7 / 54, -7 / 54]

theorem wA_d : dvec wA_k wA_c2 wA_x = ![-1, 1, 1] := by
  ext i; fin_cases i <;> norm_num [Matrix.cons_val_zero, Matrix.cons_val_one, Matrix.cons_val_two, Matrix.head_cons, Matrix.tail_cons, dvec, wA_k, wA_c2, wA_x, Fin.sum_univ_three]

theorem wA_e : evec wA_k wA_x = ![-1 / 2, -2, -2] := by
  ext i; fin_cases i <;> norm_num [Matrix.cons_val_zero, Matrix.cons_val_one, Matrix.cons_val_two, Matrix.head_cons, Matrix.tail_cons, evec, rivals, wA_k, wA_x, Fin.sum_univ_three]

theorem wA_soc (i : Fin 3) :
    -2 * rivals wA_x i / (∑ l, wA_x l) ^ 3 < wA_c2 i := by
  fin_cases i <;> norm_num [Matrix.cons_val_zero, Matrix.cons_val_one, Matrix.cons_val_two, Matrix.head_cons, Matrix.tail_cons, rivals, wA_x, wA_c2, Fin.sum_univ_three]

/-- The groups are `{0}` with `d = -1`, `γ = -1/2`, and `{1,2}` with `d = 1`, `γ = -4`; all
`γ_l < 0` (Case A), `d_r = 1 > 0`, `d_1 = -1 < 0` and `g(0) = -7/2 < -1`, so Theorem 2(ii)
would give stability.  Yet the Jacobian has the eigenvalue `1/27 > 0`. -/
theorem wA_thm2_ii_hyps :
    g ![(-1 : ℝ), 1] ![(-1 / 2 : ℝ), -4] 0 = -7 / 2 ∧ (-7 / 2 : ℝ) < -1 := by
  norm_num [g, Fin.sum_univ_two]

theorem wA_unstable :
    jac wA_k wA_c2 wA_x *ᵥ ![0, 1, -1] = (1 / 27 : ℝ) • ![0, 1, -1] := by
  rw [jac_eq, wA_d, wA_e, Matrix.smul_mulVec, bigM_mulVec]
  ext i; fin_cases i <;> norm_num [Matrix.cons_val_zero, Matrix.cons_val_one, Matrix.cons_val_two, Matrix.head_cons, Matrix.tail_cons, wA_x, Fin.sum_univ_three]

/-- An unstable complex pair under eq. (6).  Three agents, `x̄ = (1,3,1)`; agent 1 is
dominant. -/
noncomputable def wB_x : Fin 3 → ℝ := ![1, 3, 1]
noncomputable def wB_k : Fin 3 → ℝ := ![28729 / 420000, 24987 / 100000, 44829 / 131250]
noncomputable def wB_c2 : Fin 3 → ℝ :=
  ![(3 / 4 * (420000 / 28729) - 5) / 125, (1 / 4 * (100000 / 24987) - 5) / 125,
    (-1 * (131250 / 44829) - 5) / 125]

theorem wB_d : dvec wB_k wB_c2 wB_x = ![-3 / 4, -1 / 4, 1] := by
  ext i; fin_cases i <;> norm_num [Matrix.cons_val_zero, Matrix.cons_val_one, Matrix.cons_val_two, Matrix.head_cons, Matrix.tail_cons, dvec, wB_k, wB_c2, wB_x, Fin.sum_univ_three]

theorem wB_e : evec wB_k wB_x = ![-28729 / 140000, 24987 / 100000, -44829 / 43750] := by
  ext i; fin_cases i <;> norm_num [Matrix.cons_val_zero, Matrix.cons_val_one, Matrix.cons_val_two, Matrix.head_cons, Matrix.tail_cons, evec, rivals, wB_k, wB_x, Fin.sum_univ_three]

theorem wB_soc (i : Fin 3) :
    -2 * rivals wB_x i / (∑ l, wB_x l) ^ 3 < wB_c2 i := by
  fin_cases i <;> norm_num [Matrix.cons_val_zero, Matrix.cons_val_one, Matrix.cons_val_two, Matrix.head_cons, Matrix.tail_cons, rivals, wB_x, wB_c2, Fin.sum_univ_three]

theorem wB_k_pos (i : Fin 3) : 0 < wB_k i := by
  fin_cases i <;> norm_num [wB_k]

/-- The characteristic polynomial of `D + a 1ᵀ` for this witness is
`-(μ + 1)((μ - 1/100)^2 + 9/64)`. -/
theorem wB_charpoly (μ : ℂ) :
    (bigM (fun i => ((![-3 / 4, -1 / 4, 1] : Fin 3 → ℝ) i : ℂ))
        (fun i => ((![-28729 / 140000, 24987 / 100000, -44829 / 43750] : Fin 3 → ℝ) i : ℂ)) -
      μ • (1 : Matrix (Fin 3) (Fin 3) ℂ)).det =
      -(μ + 1) * ((μ - 1 / 100) ^ 2 + 9 / 64) := by
  rw [Matrix.det_fin_three]
  simp [bigM, Matrix.vecMulVec_apply]
  ring

/-- So `1/100 + (3/8) i`, with positive real part, is an eigenvalue. -/
theorem wB_unstable_pair :
    (bigM (fun i => ((![-3 / 4, -1 / 4, 1] : Fin 3 → ℝ) i : ℂ))
        (fun i => ((![-28729 / 140000, 24987 / 100000, -44829 / 43750] : Fin 3 → ℝ) i : ℂ)) -
      (1 / 100 + 3 / 8 * Complex.I) • (1 : Matrix (Fin 3) (Fin 3) ℂ)).det = 0 ∧
      0 < (1 / 100 + 3 / 8 * Complex.I).re ∧ (1 / 100 + 3 / 8 * Complex.I).im ≠ 0 := by
  refine ⟨?_, by norm_num, by norm_num⟩
  rw [wB_charpoly]
  have : (1 / 100 + 3 / 8 * Complex.I - 1 / 100) ^ 2 + 9 / 64 = 0 := by
    ring_nf; rw [Complex.I_sq]; ring
  rw [this, mul_zero]

/-! ## 8. Example 1 (pp.175-176) -/

/-- The display on p.175 is `γ_1` with `s̄_1 = s̄ - x̄_2` the effort of group 1 and
`γ = 2 x̄_2 - s̄`. -/
theorem ex1_gamma1 (m s x2 : ℝ) :
    2 * (s - x2) - m * s = -((m - 1) * s) - (2 * x2 - s) := by ring

/-- Eq. (12) for Example 1 is the printed quadratic. -/
theorem ex1_iff (α γ : ℝ) (μ : ℂ) (h0 : μ ≠ 0) (h1 : μ ≠ -1) :
    ((-α - γ : ℝ) : ℂ) / (0 - μ) + (γ : ℂ) / (-1 - μ) = -1 ↔
      μ ^ 2 + μ * (1 + α) + (α + γ) = 0 := by
  have ha : (0 : ℂ) - μ ≠ 0 := by simpa using h0
  have hb : (-1 : ℂ) - μ ≠ 0 := by
    intro h; apply h1; linear_combination -h
  rw [div_add_div _ _ ha hb, div_eq_iff (mul_ne_zero ha hb)]
  push_cast
  constructor <;> intro h <;> linear_combination h

theorem ex1_disc (α γ : ℝ) : (1 + α) ^ 2 - 4 * (α + γ) = (1 - α) ^ 2 - 4 * γ := by ring

/-- "Both real and complex roots are possible", with `α ≥ 0` and `0 < γ < 1`. -/
theorem ex1_both :
    (0 < (1 - (0 : ℝ)) ^ 2 - 4 * (1 / 8)) ∧ ((1 - (1 : ℝ)) ^ 2 - 4 * (1 / 2) < 0) := by
  norm_num

/-- Not stated in the paper: with `α ≥ 0` and `γ > 0` both roots of Example 1 have negative
real part. -/
theorem ex1_roots_stable (α γ : ℝ) (hα : 0 ≤ α) (hγ : 0 < γ) (μ : ℂ)
    (h : μ ^ 2 + μ * (1 + α) + (α + γ) = 0) : μ.re < 0 := by
  have hre := congrArg Complex.re h
  have him := congrArg Complex.im h
  simp [pow_two] at hre him
  by_cases hy : μ.im = 0
  · rw [hy] at hre
    by_contra hx; push_neg at hx
    nlinarith
  · have : 2 * μ.re + (1 + α) = 0 := by
      have : μ.im * (2 * μ.re + (1 + α)) = 0 := by linarith
      exact (mul_eq_zero.mp this).resolve_left hy
    linarith

/-! ## 9. Example 2 (pp.176-177) -/

/-- The left side of eq. (24). -/
noncomputable def F2 (a b γ : ℝ) (μ : ℂ) : ℂ :=
  -(a : ℂ) / (-2 - μ) + -(b : ℂ) / (1 - μ) + (γ : ℂ) / (-1 - μ)

/-- Eq. (24) at `λ = iα` is the pair of real equations printed on p.177. -/
theorem ex2_parts (a b γ α : ℝ) :
    F2 a b γ (α * Complex.I) = -1 ↔
      (2 * a / (4 + α ^ 2) + -b / (1 + α ^ 2) + -γ / (1 + α ^ 2) = -1 ∧
        -a * α / (4 + α ^ 2) + -b * α / (1 + α ^ 2) + γ * α / (1 + α ^ 2) = 0) := by
  have t1 := term_at_imag (-a) (-2) α (by positivity)
  have t2 := term_at_imag (-b) 1 α (by positivity)
  have t3 := term_at_imag γ (-1) α (by positivity)
  have e : F2 a b γ (α * Complex.I) =
      ((-a : ℝ) : ℂ) / (((-2 : ℝ) : ℂ) - α * Complex.I) +
        ((-b : ℝ) : ℂ) / (((1 : ℝ) : ℂ) - α * Complex.I) +
        (γ : ℂ) / (((-1 : ℝ) : ℂ) - α * Complex.I) := by
    unfold F2; push_cast; ring
  rw [e, t1, t2, t3, Complex.ext_iff]
  simp only [Complex.add_re, Complex.add_im, Complex.ofReal_re, Complex.ofReal_im,
    Complex.mul_re, Complex.mul_im, Complex.I_re, Complex.I_im, Complex.neg_re, Complex.neg_im,
    Complex.one_re, Complex.one_im, mul_zero, mul_one, sub_zero, add_zero, zero_add]
  have q1 : (-2 : ℝ) ^ 2 + α ^ 2 = 4 + α ^ 2 := by norm_num
  have q2 : (1 : ℝ) ^ 2 + α ^ 2 = 1 + α ^ 2 := by norm_num
  have q3 : (-1 : ℝ) ^ 2 + α ^ 2 = 1 + α ^ 2 := by norm_num
  rw [q1, q2, q3]
  constructor
  · rintro ⟨h1, h2⟩
    refine ⟨by linarith [h1, show -a * -2 / (4 + α ^ 2) = 2 * a / (4 + α ^ 2) by ring,
      show -b * 1 / (1 + α ^ 2) = -b / (1 + α ^ 2) by ring,
      show γ * -1 / (1 + α ^ 2) = -γ / (1 + α ^ 2) by ring], by linarith⟩
  · rintro ⟨h1, h2⟩
    refine ⟨by linarith [h1, show -a * -2 / (4 + α ^ 2) = 2 * a / (4 + α ^ 2) by ring,
      show -b * 1 / (1 + α ^ 2) = -b / (1 + α ^ 2) by ring,
      show γ * -1 / (1 + α ^ 2) = -γ / (1 + α ^ 2) by ring], by linarith⟩

/-- Eq. (25), both forms. -/
theorem ex2_eq25 (a b γ α : ℝ) (hα : α ≠ 0)
    (h1 : 2 * a / (4 + α ^ 2) + -b / (1 + α ^ 2) + -γ / (1 + α ^ 2) = -1)
    (h2 : -a * α / (4 + α ^ 2) + -b * α / (1 + α ^ 2) + γ * α / (1 + α ^ 2) = 0) :
    γ / (1 + α ^ 2) = 2 * a / (4 + α ^ 2) - b / (1 + α ^ 2) + 1 ∧
      γ / (1 + α ^ 2) = a / (4 + α ^ 2) + b / (1 + α ^ 2) := by
  constructor
  · have : -γ / (1 + α ^ 2) = -(γ / (1 + α ^ 2)) := by ring
    have : -b / (1 + α ^ 2) = -(b / (1 + α ^ 2)) := by ring
    linarith
  · have : α * (-a / (4 + α ^ 2) + -b / (1 + α ^ 2) + γ / (1 + α ^ 2)) = 0 := by
      rw [← h2]; ring
    have := (mul_eq_zero.mp this).resolve_left hα
    have e1 : -a / (4 + α ^ 2) = -(a / (4 + α ^ 2)) := by ring
    have e2 : -b / (1 + α ^ 2) = -(b / (1 + α ^ 2)) := by ring
    linarith

/-- Eq. (26). -/
theorem ex2_eq26 (a b γ z : ℝ) (hz : 0 ≤ z)
    (h1 : γ / (1 + z) = 2 * a / (4 + z) - b / (1 + z) + 1)
    (h2 : γ / (1 + z) = a / (4 + z) + b / (1 + z)) :
    z ^ 2 + z * (5 + a - 2 * b) + (4 + a - 8 * b) = 0 := by
  have e : 2 * a / (4 + z) = 2 * (a / (4 + z)) := by ring
  have h : a / (4 + z) - 2 * (b / (1 + z)) + 1 = 0 := by linarith
  have h4 : (4 + z) ≠ 0 := by positivity
  have h1' : (1 + z) ≠ 0 := by positivity
  field_simp at h
  linear_combination h

/-- The second form of eq. (25) gives `γ` explicitly; it exceeds `b` when `a > 0`. -/
theorem ex2_gamma_eq (a b γ z : ℝ) (hz : 0 ≤ z) (h2 : γ / (1 + z) = a / (4 + z) + b / (1 + z)) :
    γ = b + a * (1 + z) / (4 + z) := by
  have h1 : (1 + z) ≠ 0 := by positivity
  have h4 : (4 + z) ≠ 0 := by positivity
  field_simp at h2 ⊢
  linear_combination h2

theorem ex2_gamma_gt_b (a b z : ℝ) (ha : 0 < a) (hz : 0 ≤ z) :
    b < b + a * (1 + z) / (4 + z) := by
  have : 0 < a * (1 + z) / (4 + z) := by positivity
  linarith

/-- The condition `γ < 1` is the printed inequality `α^2 (a + b - 1) < 4 - a - 4b`. -/
theorem ex2_gamma_lt_one_iff (a b z : ℝ) (hz : 0 ≤ z) :
    b + a * (1 + z) / (4 + z) < 1 ↔ z * (a + b - 1) < 4 - a - 4 * b := by
  have h4 : 0 < 4 + z := by positivity
  rw [← sub_pos, ← sub_pos (a := 4 - a - 4 * b)]
  have : 1 - (b + a * (1 + z) / (4 + z)) = (4 - a - 4 * b - z * (a + b - 1)) / (4 + z) := by
    field_simp; ring
  rw [this]
  exact div_pos_iff_of_pos_right h4

/-- Example 2 does not satisfy eq. (6).  Eq. (6) gives `m_2 d_2 + γ_2 < 0` for group 2
(`group_bound`), here `m_2 - b < 0`, so `b > 1`; eq. (18) for group 3 needs `γ < 1`; and at
any purely imaginary root `γ > b`.  No `a > 0` and `b` meet all three. -/
theorem ex2_not_admissible (a b z : ℝ) (ha : 0 < a) (hz : 0 ≤ z) (m : ℕ) (hm : 1 ≤ m)
    (hgrp2 : (m : ℝ) * 1 + -b < 0) (h18 : b + a * (1 + z) / (4 + z) < 1) : False := by
  have hm' : (1 : ℝ) ≤ m := by exact_mod_cast hm
  have := ex2_gamma_gt_b a b z ha hz
  linarith

/-- With the printed `a = 0.15`, `b = 0.75`, group 2 already fails eq. (6): `m_2 - 0.75 < 0`
is false for every `m_2 ≥ 1`. -/
theorem ex2_printed_group2_fails (m : ℕ) (hm : 1 ≤ m) : ¬ ((m : ℝ) * 1 + -(3 / 4) < 0) := by
  have : (1 : ℝ) ≤ m := by exact_mod_cast hm
  linarith

/-- With the printed values, `γ < 1` holds for every `α^2 ≥ 0` (p.177). -/
theorem ex2_printed_gamma_lt_one (z : ℝ) (hz : 0 ≤ z) :
    z * (3 / 20 + 3 / 4 - 1) < 4 - 3 / 20 - 4 * (3 / 4) := by nlinarith

/-- With the printed values eq. (26) is `20 z^2 + 73 z - 37 = 0`, with roots
`(-73 ± 3√921)/40`. -/
theorem ex2_printed_eq26 (z : ℝ) :
    z ^ 2 + z * (5 + 3 / 20 - 2 * (3 / 4)) + (4 + 3 / 20 - 8 * (3 / 4)) =
      (20 * z ^ 2 + 73 * z - 37) / 20 := by ring

noncomputable def ex2_t : ℝ := Real.sqrt 921
noncomputable def ex2_z1 : ℝ := (-73 + 3 * ex2_t) / 40
noncomputable def ex2_z2 : ℝ := (-73 - 3 * ex2_t) / 40
noncomputable def ex2_alpha : ℝ := Real.sqrt ex2_z1
noncomputable def ex2_gamma : ℝ := 3 / 4 + 3 / 20 * (1 + ex2_z1) / (4 + ex2_z1)

theorem ex2_t_sq : ex2_t ^ 2 = 921 := Real.sq_sqrt (by norm_num)

theorem ex2_t_bounds : 30.3479 < ex2_t ∧ ex2_t < 30.3480 := by
  have h0 : 0 ≤ ex2_t := Real.sqrt_nonneg _
  have := ex2_t_sq
  constructor <;> nlinarith

theorem ex2_roots : 20 * ex2_z1 ^ 2 + 73 * ex2_z1 - 37 = 0 ∧
    20 * ex2_z2 ^ 2 + 73 * ex2_z2 - 37 = 0 := by
  have := ex2_t_sq
  refine ⟨?_, ?_⟩
  · unfold ex2_z1; linear_combination (9 / 80 : ℝ) * this
  · unfold ex2_z2; linear_combination (9 / 80 : ℝ) * this

/-- The printed `α^2 ≈ 0.451` and `α^2 ≈ -4.101`. -/
theorem ex2_z_approx : |ex2_z1 - 0.451| < 0.0005 ∧ |ex2_z2 - (-4.101)| < 0.0005 := by
  obtain ⟨h1, h2⟩ := ex2_t_bounds
  unfold ex2_z1 ex2_z2
  constructor <;> rw [abs_lt] <;> constructor <;> linarith

theorem ex2_z1_bounds : 0.45109 < ex2_z1 ∧ ex2_z1 < 0.45111 := by
  obtain ⟨h1, h2⟩ := ex2_t_bounds
  unfold ex2_z1
  constructor <;> linarith

/-- The printed `α ≈ ±0.672`. -/
theorem ex2_alpha_bounds : 0.6716 < ex2_alpha ∧ ex2_alpha < 0.6717 ∧
    |ex2_alpha - 0.672| < 0.0005 := by
  obtain ⟨h1, h2⟩ := ex2_z1_bounds
  have hpos : 0 ≤ ex2_z1 := by linarith
  have hsq : ex2_alpha ^ 2 = ex2_z1 := Real.sq_sqrt hpos
  have h0 : 0 ≤ ex2_alpha := Real.sqrt_nonneg _
  have a1 : 0.6716 < ex2_alpha := by nlinarith
  have a2 : ex2_alpha < 0.6717 := by nlinarith
  refine ⟨a1, a2, ?_⟩
  rw [abs_lt]; constructor <;> linarith

/-- The printed `γ ≈ 0.799`. -/
theorem ex2_gamma_approx : |ex2_gamma - 0.799| < 0.0005 := by
  obtain ⟨h1, h2⟩ := ex2_z1_bounds
  unfold ex2_gamma
  have h4 : 0 < 4 + ex2_z1 := by linarith
  rw [abs_lt]
  constructor
  · rw [← sub_pos]
    have : 3 / 4 + 3 / 20 * (1 + ex2_z1) / (4 + ex2_z1) - 0.799 - -0.0005 =
        (3 / 20 * (1 + ex2_z1) - (0.799 - 0.0005 - 3 / 4) * (4 + ex2_z1)) / (4 + ex2_z1) := by
      field_simp; ring
    rw [this]; apply div_pos _ h4; nlinarith
  · rw [← sub_pos]
    have : 0.0005 - (3 / 4 + 3 / 20 * (1 + ex2_z1) / (4 + ex2_z1) - 0.799) =
        ((0.799 + 0.0005 - 3 / 4) * (4 + ex2_z1) - 3 / 20 * (1 + ex2_z1)) / (4 + ex2_z1) := by
      field_simp; ring
    rw [this]; apply div_pos _ h4; nlinarith

/-- `λ = iα` solves eq. (24) exactly at the computed `γ`. -/
theorem ex2_root_exact : F2 (3 / 20) (3 / 4) ex2_gamma (ex2_alpha * Complex.I) = -1 := by
  obtain ⟨h1, h2⟩ := ex2_z1_bounds
  have hpos : 0 ≤ ex2_z1 := by linarith
  have hsq : ex2_alpha ^ 2 = ex2_z1 := Real.sq_sqrt hpos
  have hr := ex2_roots.1
  rw [ex2_parts, hsq]
  unfold ex2_gamma
  have h4 : (4 + ex2_z1) ≠ 0 := by positivity
  have h1' : (1 + ex2_z1) ≠ 0 := by positivity
  constructor
  · field_simp; linear_combination 4 * hr
  · field_simp; ring

/-! ### The derivative `dλ/dγ` of p.177 -/

/-- The printed denominator of `dλ/dγ`: the derivative of eq. (24)'s left side in `λ`. -/
noncomputable def F2' (a b γ : ℝ) (μ : ℂ) : ℂ :=
  -(a : ℂ) / (-2 - μ) ^ 2 - (b : ℂ) / (1 - μ) ^ 2 + (γ : ℂ) / (1 + μ) ^ 2

/-- The printed `dλ/dγ`. -/
noncomputable def dLam (a b γ : ℝ) (μ : ℂ) : ℂ := (1 / (1 + μ)) / F2' a b γ μ

theorem F2_hasDerivAt (a b γ : ℝ) (μ : ℂ) (h2 : -2 - μ ≠ 0) (h1 : 1 - μ ≠ 0)
    (hm : -1 - μ ≠ 0) : HasDerivAt (F2 a b γ) (F2' a b γ μ) μ := by
  have d1 : HasDerivAt (fun ν : ℂ => -2 - ν) (-1) μ := by
    simpa using (hasDerivAt_id μ).const_sub (-2)
  have d2 : HasDerivAt (fun ν : ℂ => 1 - ν) (-1) μ := by
    simpa using (hasDerivAt_id μ).const_sub 1
  have d3 : HasDerivAt (fun ν : ℂ => -1 - ν) (-1) μ := by
    simpa using (hasDerivAt_id μ).const_sub (-1)
  have e1 := (hasDerivAt_const μ (-(a : ℂ))).div d1 h2
  have e2 := (hasDerivAt_const μ (-(b : ℂ))).div d2 h1
  have e3 := (hasDerivAt_const μ (γ : ℂ)).div d3 hm
  have := (e1.add e2).add e3
  unfold F2 F2'
  refine this.congr_deriv ?_
  have hp : (1 + μ) = -(-1 - μ) := by ring
  rw [hp]
  field_simp
  ring

/-- Eq. (24)'s left side is affine in `γ` with slope `1/(-1-λ)`, so the implicit derivative
`-(∂F/∂γ)/(∂F/∂λ)` is the printed `dλ/dγ`. -/
theorem F2_affine_gamma (a b γ : ℝ) (μ : ℂ) :
    F2 a b γ μ = F2 a b 0 μ + (γ : ℂ) * (1 / (-1 - μ)) := by
  unfold F2; push_cast; ring

theorem dLam_eq_implicit (a b γ : ℝ) (μ : ℂ) :
    dLam a b γ μ = -(1 / (-1 - μ)) / F2' a b γ μ := by
  unfold dLam
  have : (1 : ℂ) + μ = -(-1 - μ) := by ring
  rw [this, div_neg, neg_div]

/-- `coef / (c - iα)^2` in real and imaginary parts. -/
theorem sq_term (coef c α : ℝ) (h : 0 < c ^ 2 + α ^ 2) :
    (coef : ℂ) / ((c : ℂ) - α * Complex.I) ^ 2 =
      ((coef * (c ^ 2 - α ^ 2) / (c ^ 2 + α ^ 2) ^ 2 : ℝ) : ℂ) +
        ((coef * (2 * c * α) / (c ^ 2 + α ^ 2) ^ 2 : ℝ) : ℂ) * Complex.I := by
  have hne : (c : ℂ) - α * Complex.I ≠ 0 := by
    intro h0
    have h1 := congrArg Complex.re h0
    have h2 := congrArg Complex.im h0
    simp at h1 h2
    subst h1; subst h2; simp at h
  rw [div_eq_iff (pow_ne_zero 2 hne)]
  have hN : 0 < (c ^ 2 + α ^ 2) ^ 2 := by positivity
  generalize hA : coef * (c ^ 2 - α ^ 2) / (c ^ 2 + α ^ 2) ^ 2 = A
  generalize hB : coef * (2 * c * α) / (c ^ 2 + α ^ 2) ^ 2 = B
  have hA' : A * (c ^ 2 + α ^ 2) ^ 2 = coef * (c ^ 2 - α ^ 2) := by rw [← hA]; field_simp
  have hB' : B * (c ^ 2 + α ^ 2) ^ 2 = coef * (2 * c * α) := by rw [← hB]; field_simp
  rw [pow_two]
  simp only [Complex.ext_iff, Complex.mul_re, Complex.mul_im, Complex.add_re, Complex.add_im,
    Complex.sub_re, Complex.sub_im, Complex.ofReal_re, Complex.ofReal_im, Complex.I_re,
    Complex.I_im]
  constructor
  · have : (A * (c ^ 2 - α ^ 2) + B * (2 * c * α)) * (c ^ 2 + α ^ 2) ^ 2 =
        coef * (c ^ 2 + α ^ 2) ^ 2 := by
      linear_combination (c ^ 2 - α ^ 2) * hA' + (2 * c * α) * hB'
    have := mul_right_cancel₀ hN.ne' this
    nlinarith [this]
  · have : (B * (c ^ 2 - α ^ 2) - A * (2 * c * α)) * (c ^ 2 + α ^ 2) ^ 2 =
        0 * (c ^ 2 + α ^ 2) ^ 2 := by
      linear_combination (c ^ 2 - α ^ 2) * hB' - (2 * c * α) * hA'
    have := mul_right_cancel₀ hN.ne' this
    nlinarith [this]

/-- Real and imaginary parts of a quotient. -/
theorem div_parts (p q u v : ℝ) (h : 0 < u ^ 2 + v ^ 2) :
    ((p : ℂ) + q * Complex.I) / ((u : ℂ) + v * Complex.I) =
      (((p * u + q * v) / (u ^ 2 + v ^ 2) : ℝ) : ℂ) +
        (((q * u - p * v) / (u ^ 2 + v ^ 2) : ℝ) : ℂ) * Complex.I := by
  have hne : (u : ℂ) + v * Complex.I ≠ 0 := by
    intro h0
    have h1 := congrArg Complex.re h0
    have h2 := congrArg Complex.im h0
    simp at h1 h2
    subst h1; subst h2; simp at h
  rw [div_eq_iff hne]
  generalize hA : (p * u + q * v) / (u ^ 2 + v ^ 2) = A
  generalize hB : (q * u - p * v) / (u ^ 2 + v ^ 2) = B
  have hA' : A * (u ^ 2 + v ^ 2) = p * u + q * v := by rw [← hA]; field_simp
  have hB' : B * (u ^ 2 + v ^ 2) = q * u - p * v := by rw [← hB]; field_simp
  simp only [Complex.ext_iff, Complex.mul_re, Complex.mul_im, Complex.add_re, Complex.add_im,
    Complex.ofReal_re, Complex.ofReal_im, Complex.I_re, Complex.I_im]
  constructor
  · have : (A * u - B * v) * (u ^ 2 + v ^ 2) = p * (u ^ 2 + v ^ 2) := by
      linear_combination u * hA' - v * hB'
    have := mul_right_cancel₀ h.ne' this
    nlinarith [this]
  · have : (A * v + B * u) * (u ^ 2 + v ^ 2) = q * (u ^ 2 + v ^ 2) := by
      linear_combination v * hA' + u * hB'
    have := mul_right_cancel₀ h.ne' this
    nlinarith [this]

/-- `F2'` at `λ = iα` in real and imaginary parts. -/
noncomputable def Dre (a b γ α : ℝ) : ℝ :=
  -a * ((-2) ^ 2 - α ^ 2) / ((-2) ^ 2 + α ^ 2) ^ 2 + -b * (1 ^ 2 - α ^ 2) / (1 ^ 2 + α ^ 2) ^ 2 +
    γ * (1 ^ 2 - (-α) ^ 2) / (1 ^ 2 + (-α) ^ 2) ^ 2

noncomputable def Dim (a b γ α : ℝ) : ℝ :=
  -a * (2 * (-2) * α) / ((-2) ^ 2 + α ^ 2) ^ 2 + -b * (2 * 1 * α) / (1 ^ 2 + α ^ 2) ^ 2 +
    γ * (2 * 1 * (-α)) / (1 ^ 2 + (-α) ^ 2) ^ 2

theorem F2'_at_imag (a b γ α : ℝ) :
    F2' a b γ (α * Complex.I) = (Dre a b γ α : ℂ) + (Dim a b γ α : ℂ) * Complex.I := by
  have t1 := sq_term (-a) (-2) α (by positivity)
  have t2 := sq_term (-b) 1 α (by positivity)
  have t3 := sq_term γ 1 (-α) (by positivity)
  have e : F2' a b γ (α * Complex.I) =
      ((-a : ℝ) : ℂ) / (((-2 : ℝ) : ℂ) - α * Complex.I) ^ 2 +
        ((-b : ℝ) : ℂ) / (((1 : ℝ) : ℂ) - α * Complex.I) ^ 2 +
        (γ : ℂ) / (((1 : ℝ) : ℂ) - ((-α : ℝ) : ℂ) * Complex.I) ^ 2 := by
    unfold F2'; push_cast; ring
  rw [e, t1, t2, t3]
  unfold Dre Dim
  push_cast
  ring

theorem N_at_imag (α : ℝ) :
    1 / (1 + α * Complex.I) =
      (((1 / (1 + α ^ 2)) : ℝ) : ℂ) + (((-α / (1 + α ^ 2)) : ℝ) : ℂ) * Complex.I := by
  have := term_at_imag 1 1 (-α) (by positivity)
  have e : (1 : ℂ) / (1 + α * Complex.I) =
      ((1 : ℝ) : ℂ) / (((1 : ℝ) : ℂ) - ((-α : ℝ) : ℂ) * Complex.I) := by push_cast; ring_nf
  rw [e, this]
  push_cast
  ring_nf

/-- `γ` from eq. (25) with the printed `a`, `b`. -/
noncomputable def gam25 (α : ℝ) : ℝ := 3 / 4 + 3 / 20 * (1 + α ^ 2) / (4 + α ^ 2)

theorem Dre_printed (α : ℝ) :
    Dre (3 / 20) (3 / 4) (gam25 α) α = -9 * α ^ 2 / (10 * (1 + α ^ 2) * (4 + α ^ 2) ^ 2) := by
  unfold Dre gam25
  have h1 : (0 : ℝ) < 1 + α ^ 2 := by positivity
  have h4 : (0 : ℝ) < 4 + α ^ 2 := by positivity
  field_simp
  ring

theorem Dim_printed (α : ℝ) :
    Dim (3 / 20) (3 / 4) (gam25 α) α =
      α * (-27 * (α ^ 2 + 3) * (α ^ 2 + 6) / (10 * (1 + α ^ 2) ^ 2 * (4 + α ^ 2) ^ 2)) := by
  unfold Dim gam25
  have h1 : (0 : ℝ) < 1 + α ^ 2 := by positivity
  have h4 : (0 : ℝ) < 4 + α ^ 2 := by positivity
  field_simp
  ring

/-- The real part of `dλ/dγ` at `λ = iα`, as a function of `z = α^2`. -/
theorem dLam_re (α : ℝ) (hα : α ≠ 0) :
    (dLam (3 / 20) (3 / 4) (gam25 α) (α * Complex.I)).re =
      10 * (α ^ 2 + 1) * (α ^ 2 + 4) * (3 * α ^ 4 + 26 * α ^ 2 + 53) /
        (9 * (9 * α ^ 6 + 127 * α ^ 4 + 547 * α ^ 2 + 729)) := by
  have hz : 0 < α ^ 2 := by positivity
  have h1 : (0 : ℝ) < 1 + α ^ 2 := by positivity
  have h4 : (0 : ℝ) < 4 + α ^ 2 := by positivity
  have hD : 0 < Dre (3 / 20) (3 / 4) (gam25 α) α ^ 2 + Dim (3 / 20) (3 / 4) (gam25 α) α ^ 2 := by
    rw [Dre_printed]
    have : 0 < (-9 * α ^ 2 / (10 * (1 + α ^ 2) * (4 + α ^ 2) ^ 2)) ^ 2 := by
      have : -9 * α ^ 2 / (10 * (1 + α ^ 2) * (4 + α ^ 2) ^ 2) ≠ 0 := by
        apply div_ne_zero; nlinarith; positivity
      positivity
    positivity
  unfold dLam
  rw [N_at_imag, F2'_at_imag, div_parts _ _ _ _ hD]
  simp only [Complex.add_re, Complex.ofReal_re, Complex.mul_re, Complex.ofReal_im,
    Complex.I_re, Complex.I_im, mul_zero, mul_one, sub_zero, zero_sub, add_zero]
  rw [Dre_printed, Dim_printed]
  field_simp
  ring

/-- The imaginary part, likewise. -/
theorem dLam_im (α : ℝ) (hα : α ≠ 0) :
    (dLam (3 / 20) (3 / 4) (gam25 α) (α * Complex.I)).im =
      α * (20 * (α ^ 2 + 1) * (α ^ 2 + 4) * (2 * α ^ 4 + 14 * α ^ 2 + 27) /
        (9 * α ^ 2 * (9 * α ^ 6 + 127 * α ^ 4 + 547 * α ^ 2 + 729))) := by
  have hz : 0 < α ^ 2 := by positivity
  have h1 : (0 : ℝ) < 1 + α ^ 2 := by positivity
  have h4 : (0 : ℝ) < 4 + α ^ 2 := by positivity
  have hD : 0 < Dre (3 / 20) (3 / 4) (gam25 α) α ^ 2 + Dim (3 / 20) (3 / 4) (gam25 α) α ^ 2 := by
    rw [Dre_printed]
    have : 0 < (-9 * α ^ 2 / (10 * (1 + α ^ 2) * (4 + α ^ 2) ^ 2)) ^ 2 := by
      have : -9 * α ^ 2 / (10 * (1 + α ^ 2) * (4 + α ^ 2) ^ 2) ≠ 0 := by
        apply div_ne_zero; nlinarith; positivity
      positivity
    positivity
  unfold dLam
  rw [N_at_imag, F2'_at_imag, div_parts _ _ _ _ hD]
  simp only [Complex.add_im, Complex.ofReal_re, Complex.mul_im, Complex.ofReal_im,
    Complex.I_re, Complex.I_im, mul_zero, mul_one, zero_add, add_zero]
  rw [Dre_printed, Dim_printed]
  field_simp
  ring

/-- Transversality (p.177): the real part of `dλ/dγ` is positive at every purely imaginary
root of eq. (24) with the printed `a`, `b`, not only at the computed one. -/
theorem dLam_re_pos (α : ℝ) (hα : α ≠ 0) :
    0 < (dLam (3 / 20) (3 / 4) (gam25 α) (α * Complex.I)).re := by
  rw [dLam_re α hα]
  have : 0 ≤ α ^ 2 := by positivity
  positivity

theorem gam25_eq : gam25 ex2_alpha = ex2_gamma := by
  have h := ex2_z1_bounds
  have hsq : ex2_alpha ^ 2 = ex2_z1 := Real.sq_sqrt (by linarith [h.1])
  unfold gam25 ex2_gamma
  rw [hsq]

/-- The printed value `dλ/dγ ≈ 0.468 + 0.719 i`. -/
theorem dLam_printed :
    |(dLam (3 / 20) (3 / 4) ex2_gamma (ex2_alpha * Complex.I)).re - 0.468| < 0.0005 ∧
      |(dLam (3 / 20) (3 / 4) ex2_gamma (ex2_alpha * Complex.I)).im - 0.719| < 0.0005 := by
  obtain ⟨a1, a2, _⟩ := ex2_alpha_bounds
  obtain ⟨z1, z2⟩ := ex2_z1_bounds
  have hα : ex2_alpha ≠ 0 := by linarith
  have hsq : ex2_alpha ^ 2 = ex2_z1 := Real.sq_sqrt (by linarith)
  rw [← gam25_eq, dLam_re ex2_alpha hα, dLam_im ex2_alpha hα]
  have e4 : ex2_alpha ^ 4 = ex2_z1 ^ 2 := by rw [← hsq]; ring
  have e6 : ex2_alpha ^ 6 = ex2_z1 ^ 3 := by rw [← hsq]; ring
  rw [e4, e6, hsq]
  set z := ex2_z1
  set x := ex2_alpha
  have zp : 0 < z := by linarith
  have b2 : (0.45109 : ℝ) ^ 2 < z ^ 2 := pow_lt_pow_left₀ z1 (by norm_num) (by norm_num)
  have b2' : z ^ 2 < (0.45111 : ℝ) ^ 2 := pow_lt_pow_left₀ z2 zp.le (by norm_num)
  have b3 : (0.45109 : ℝ) ^ 3 < z ^ 3 := pow_lt_pow_left₀ z1 (by norm_num) (by norm_num)
  have b3' : z ^ 3 < (0.45111 : ℝ) ^ 3 := pow_lt_pow_left₀ z2 zp.le (by norm_num)
  have b4 : (0.45109 : ℝ) ^ 4 < z ^ 4 := pow_lt_pow_left₀ z1 (by norm_num) (by norm_num)
  have b4' : z ^ 4 < (0.45111 : ℝ) ^ 4 := pow_lt_pow_left₀ z2 zp.le (by norm_num)
  have hP : 0 < 9 * (9 * z ^ 3 + 127 * z ^ 2 + 547 * z + 729) := by positivity
  have hzP : 0 < 9 * z * (9 * z ^ 3 + 127 * z ^ 2 + 547 * z + 729) := by positivity
  have eN : 10 * (z + 1) * (z + 4) * (3 * z ^ 2 + 26 * z + 53) =
      30 * z ^ 4 + 410 * z ^ 3 + 1950 * z ^ 2 + 3690 * z + 2120 := by ring
  have eP : 9 * (9 * z ^ 3 + 127 * z ^ 2 + 547 * z + 729) =
      81 * z ^ 3 + 1143 * z ^ 2 + 4923 * z + 6561 := by ring
  have eQ : 20 * (z + 1) * (z + 4) * (2 * z ^ 2 + 14 * z + 27) =
      40 * z ^ 4 + 480 * z ^ 3 + 2100 * z ^ 2 + 3820 * z + 2160 := by ring
  have ezP : 9 * z * (9 * z ^ 3 + 127 * z ^ 2 + 547 * z + 729) =
      81 * z ^ 4 + 1143 * z ^ 3 + 4923 * z ^ 2 + 6561 * z := by ring
  constructor
  · rw [abs_lt]; constructor
    · rw [lt_sub_iff_add_lt, lt_div_iff₀ hP, eN, eP]; linarith
    · rw [sub_lt_iff_lt_add, div_lt_iff₀ hP, eN, eP]; linarith
  · have hQ0 : 0 ≤ 20 * (z + 1) * (z + 4) * (2 * z ^ 2 + 14 * z + 27) := by positivity
    rw [abs_lt]; constructor
    · rw [lt_sub_iff_add_lt, mul_div_assoc', lt_div_iff₀ hzP]
      have hxQ := mul_le_mul_of_nonneg_right a1.le hQ0
      rw [eQ] at hxQ ⊢; rw [ezP]; linarith
    · rw [sub_lt_iff_lt_add, mul_div_assoc', div_lt_iff₀ hzP]
      have hxQ := mul_le_mul_of_nonneg_right a2.le hQ0
      rw [eQ] at hxQ ⊢; rw [ezP]; linarith

end ChiarellaSzidarovszky2002
