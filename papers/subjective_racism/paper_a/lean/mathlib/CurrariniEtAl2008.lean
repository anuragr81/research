import Mathlib

/-! Currarini, Jackson and Pin, "An Economic Model of Friendship: Homophily,
Minorities and Segregation", working paper revised 29 July 2008, Sections 2,
4.5, 5, 6 and 7.1, in the paper's notation. `s i` and `d i` are the average
numbers of same-type and different-type friendships of type `i`, `w i` its
relative size, `q i` the probability that a meeting of a type-`i` agent is with
its own type, and `t i` its time in the matching. In equilibrium `H_i = q_i`
(Section 4.4). The search model itself, its existence proofs and the fitted
curves are not formalised. -/

namespace CurrariniEtAl2008

open Finset

/-- Definition 1. The homophily index `H_i = s_i/(s_i + d_i)`. -/
noncomputable def H (s d : ℝ) : ℝ := s / (s + d)

/-- Definition 6. Coleman's index of inbreeding homophily,
`IH_i = (H_i − w_i)/(1 − w_i)`. -/
noncomputable def IH (h w : ℝ) : ℝ := (h - w) / (1 - w)

/-- Definitions 4 to 6. For `w_i < 1`, inbreeding homophily `H_i > w_i` holds
if and only if `IH_i > 0`, and heterophily if and only if `IH_i < 0`. -/
theorem IH_pos_iff {h w : ℝ} (hw : w < 1) : 0 < IH h w ↔ w < h := by
  unfold IH; rw [div_pos_iff_of_pos_right (by linarith)]; constructor <;> intro <;> linarith

theorem IH_neg_iff {h w : ℝ} (hw : w < 1) : IH h w < 0 ↔ h < w := by
  unfold IH; rw [div_neg_iff]; constructor
  · rintro (⟨_, h2⟩ | ⟨h1, _⟩)
    · linarith
    · linarith
  · intro hh; right; constructor <;> linarith

/-- Section 3, Table 1. With 85 percent same-race friendships, the index is
`34/49 ≈ 0.69` for whites (51 percent) and `47/62 ≈ 0.76` for blacks (38
percent), as printed. -/
theorem table1_white_black :
    IH (85 / 100) (51 / 100) = 34 / 49 ∧ IH (85 / 100) (38 / 100) = 47 / 62 := by
  constructor <;> (unfold IH; norm_num)

/-- Section 3, Table 1, Hispanics (CJ-D1). With 2 percent same-type friendships
and the table's 5 percent share, the index is `−3/95 ≈ −0.03`. With the 2
percent share the text gives, it is 0. Neither is the printed `−0.11`. -/
theorem table1_hispanic_not_reproduced :
    IH (2 / 100) (5 / 100) = -3 / 95 ∧ IH (2 / 100) (2 / 100) = 0 ∧
      IH (2 / 100) (5 / 100) ≠ -11 / 100 ∧ IH (2 / 100) (2 / 100) ≠ -11 / 100 := by
  refine ⟨?_, ?_, ?_, ?_⟩ <;> (unfold IH; norm_num)

/-- Section 4.4. In equilibrium `s_i = q_i t_i` and `d_i = (1 − q_i) t_i`, so the
homophily index is the meeting probability, `H_i = q_i`. -/
theorem H_eq_q {q t : ℝ} (ht : t ≠ 0) : H (q * t) ((1 - q) * t) = q := by
  unfold H; field_simp; ring

/-- Proposition 1, the step that does the work. If `U` is type neutral,
`U(s, d) = V(s + d)`, the objective (2) does not depend on `q`, so a time that
is optimal against one meeting probability is optimal against every one. -/
theorem prop1 (V : ℝ → ℝ) (c q q' t : ℝ)
    (hopt : ∀ t', V (q * t' + (1 - q) * t') - c * t' ≤ V (q * t + (1 - q) * t) - c * t) :
    ∀ t', V (q' * t' + (1 - q') * t') - c * t' ≤ V (q' * t + (1 - q') * t) - c * t := by
  intro t'
  have e : ∀ (x u : ℝ), x * u + (1 - x) * u = u := fun x u => by ring
  have := hopt t'
  rw [e, e] at this ⊢
  exact this

/-- Proposition 2. Under unbiased matching the meeting probabilities are the
relative stocks, which sum to 1 like the relative sizes. So if some type is
homophilous, `q_i > w_i`, another type is heterophilous, `q_j < w_j`. -/
theorem prop2 {ι : Type*} [Fintype ι] (q w : ι → ℝ) (hq : ∑ j, q j = 1) (hw : ∑ j, w j = 1)
    (i : ι) (hi : w i < q i) : ∃ j, q j < w j := by
  by_contra hcon
  push_neg at hcon
  have : ∑ j, w j < ∑ j, q j := Finset.sum_lt_sum (fun j _ => hcon j) ⟨i, mem_univ _, hi⟩
  linarith

/-- Eq. (6). With two types and unbiased matching, `q₂ = 1 − q₁`, so
`IH₂ = (w₁ − q₁)/w₁`, and `IH₂ < 0` if and only if `IH₁ > 0`. -/
theorem eq6 {q1 w1 : ℝ} (hw0 : 0 < w1) (hw1 : w1 < 1) :
    IH (1 - q1) (1 - w1) = (w1 - q1) / w1 ∧ (IH (1 - q1) (1 - w1) < 0 ↔ 0 < IH q1 w1) := by
  refine ⟨by unfold IH; ring_nf, ?_⟩
  rw [IH_neg_iff (by linarith), IH_pos_iff hw1]
  constructor <;> intro <;> linarith

/-- Section 4.5. With `U(s, d) = (s + γ d)^{1/2}` the optimal time solves
`(γ + (1 − γ) q)/(2 √(t (γ + (1 − γ) q))) = c`, that is
`t = (γ + (1 − γ) q)/(4c²)`. At `γ = 1/2` this is `(1 + q)/(8c²)`. -/
theorem t_half {c q : ℝ} (hc : c ≠ 0) :
    (1 / 2 + (1 - 1 / 2) * q) / (4 * c ^ 2) = (1 + q) / (8 * c ^ 2) := by
  field_simp; ring

/-- Section 4.5, the full solution at `γ = 1/2`, `w₁ = 2/3`, `w₂ = 1/3`. The
printed values `t₁ = √3/(8c²)`, `t₂ = (3 − √3)/(8c²)`, `q₁ = √3 − 1`,
`q₂ = 2 − √3` satisfy `t_i = (1 + q_i)/(8c²)` and the steady-state conditions
`q₁ = 2t₁/(2t₁ + t₂)`, `q₂ = t₂/(2t₁ + t₂)`, and the larger group is
inbreeding homophilous, `q₁ > w₁`, while the smaller is heterophilous. -/
theorem example_45 {c : ℝ} (hc : c ≠ 0) :
    let t1 := Real.sqrt 3 / (8 * c ^ 2)
    let t2 := (3 - Real.sqrt 3) / (8 * c ^ 2)
    let q1 := Real.sqrt 3 - 1
    let q2 := 2 - Real.sqrt 3
    t1 = (1 + q1) / (8 * c ^ 2) ∧ t2 = (1 + q2) / (8 * c ^ 2) ∧
      q1 = 2 * t1 / (2 * t1 + t2) ∧ q2 = t2 / (2 * t1 + t2) ∧
      (2 : ℝ) / 3 < q1 ∧ q2 < 1 / 3 := by
  intro t1 t2 q1 q2
  have h3 : Real.sqrt 3 ^ 2 = 3 := Real.sq_sqrt (by norm_num)
  have hlo : (5 : ℝ) / 3 < Real.sqrt 3 := by
    rw [show (5 : ℝ) / 3 = Real.sqrt ((5 / 3) ^ 2) by rw [Real.sqrt_sq (by norm_num)]]
    exact Real.sqrt_lt_sqrt (by norm_num) (by norm_num)
  have hc2 : 0 < 8 * c ^ 2 := by positivity
  have hden : 2 * t1 + t2 = (Real.sqrt 3 + 3) / (8 * c ^ 2) := by
    simp only [t1, t2]; field_simp; ring
  refine ⟨by simp only [t1, q1]; ring_nf, by simp only [t2, q2]; ring_nf, ?_, ?_, ?_, ?_⟩
  · rw [hden]; simp only [t1, q1]
    rw [eq_div_iff (by positivity)]; field_simp; nlinarith
  · rw [hden]; simp only [t2, q2]
    rw [eq_div_iff (by positivity)]; field_simp; nlinarith
  · simp only [q1]; linarith
  · simp only [q2]; linarith

/-- Control: Proposition 1 needs type-neutral preferences. In the Section 4.5
example, where `γ = 1/2` makes preferences type-sensitive, the two types form
different numbers of friendships, `t₁ ≠ t₂`. -/
theorem control_type_sensitive_numbers_differ {c : ℝ} (hc : c ≠ 0) :
    Real.sqrt 3 / (8 * c ^ 2) ≠ (3 - Real.sqrt 3) / (8 * c ^ 2) := by
  have hlo : (5 : ℝ) / 3 < Real.sqrt 3 := by
    rw [show (5 : ℝ) / 3 = Real.sqrt ((5 / 3) ^ 2) by rw [Real.sqrt_sq (by norm_num)]]
    exact Real.sqrt_lt_sqrt (by norm_num) (by norm_num)
  intro h
  have hc2 : 8 * c ^ 2 ≠ 0 := by positivity
  rw [div_left_inj' hc2] at h
  linarith

/-- Control: Proposition 2 needs unbiased matching. Under the biased process
(7) with `β = 2`, `q₁² + q₂² = 1`, two equal groups `w₁ = w₂ = 1/2` with equal
stocks can both meet their own type with probability `√2/2 > 1/2`, so both
are inbreeding homophilous, as Section 7.1 says. Balance
`M₁(1 − q₁) = M₂(1 − q₂)` holds with `M₁ = M₂`. -/
theorem control_biased_matching_all_homophilous :
    let q := Real.sqrt 2 / 2
    q ^ 2 + q ^ 2 = 1 ∧ 0 < IH q (1 / 2) := by
  intro q
  have h2 : Real.sqrt 2 ^ 2 = 2 := Real.sq_sqrt (by norm_num)
  have hlo : (1 : ℝ) < Real.sqrt 2 := by
    rw [show (1 : ℝ) = Real.sqrt 1 by simp]
    exact Real.sqrt_lt_sqrt (by norm_num) (by norm_num)
  refine ⟨by simp only [q]; nlinarith, ?_⟩
  rw [IH_pos_iff (by norm_num)]
  simp only [q]; linarith

end CurrariniEtAl2008
