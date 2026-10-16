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

open Finset Filter Topology Set

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

/-! ### Section 2, the remaining definitions and footnote 10 -/

/-- Definition 2. Relative homophily. -/
def relHomophily {ι : Type*} (H w : ι → ℝ) : Prop := ∀ i j, w j < w i → H j < H i

/-- Definition 3. Baseline homophily. -/
def baseline {ι : Type*} (H w : ι → ℝ) : Prop := ∀ i, H i = w i

/-- Definition 3, remark. Baseline homophily implies relative homophily. -/
theorem baseline_rel {ι : Type*} (H w : ι → ℝ) (h : baseline H w) : relHomophily H w := by
  intro i j hij; rw [h i, h j]; exact hij

/-- Footnote 10. With `N_i` of type `i` among `N` and friendships formed
uniformly at random, the expected same-type share is `(N_i − 1)/(N − 1)`, so
`E[H_i] − w_i = −(N − N_i)/(N(N − 1))` and the expected `IH_i` is `−1/(N − 1)`. -/
theorem fn10 {N Ni : ℝ} (hN : 1 < N) (hNi : Ni < N) :
    (Ni - 1) / (N - 1) - Ni / N = -(N - Ni) / (N * (N - 1)) ∧
      ((Ni - 1) / (N - 1) - Ni / N) / (1 - Ni / N) = -1 / (N - 1) := by
  have h1 : N - 1 ≠ 0 := by linarith
  have h2 : N ≠ 0 := by linarith
  have h3 : N - Ni ≠ 0 := by linarith
  constructor
  · field_simp; ring
  · have hw : 1 - Ni / N ≠ 0 := by
      intro h0
      have : Ni / N = 1 := by linarith
      rw [div_eq_one_iff_eq h2] at this; linarith
    rw [div_eq_div_iff hw h1]
    field_simp; ring

/-- Table 1. Each column sums to 100, and the shares `131, 96, 13, 15` of 255
round to the printed 51, 38, 5 and 6 percent. -/
theorem table1_columns :
    (85 + 4 + 4 + 7 : ℕ) = 100 ∧ (7 + 85 + 6 + 2 : ℕ) = 100 ∧ (47 + 46 + 2 + 5 : ℕ) = 100 ∧
      (74 + 11 + 4 + 11 : ℕ) = 100 ∧ (131 + 96 + 13 + 15 : ℕ) = 255 ∧
      |(131 : ℝ) / 255 - 51 / 100| < 1 / 200 ∧ |(96 : ℝ) / 255 - 38 / 100| < 1 / 200 ∧
      |(13 : ℝ) / 255 - 5 / 100| < 1 / 200 ∧ |(15 : ℝ) / 255 - 6 / 100| < 1 / 200 := by
  refine ⟨by norm_num, by norm_num, by norm_num, by norm_num, by norm_num, ?_, ?_, ?_, ?_⟩ <;>
    (rw [abs_lt]; constructor <;> norm_num)

/-! ### Section 4, the decision problem and steady state -/

/-- Lemma 1. If `t` maximises `U(q t, (1 − q) t) − c t` and `g(t) = U(q t, (1 − q) t)`
has derivative `g′ = q U_s + (1 − q) U_d` at `t`, then `g′ = c`, eq. (3). -/
theorem lemma1 (g : ℝ → ℝ) {g' c t : ℝ} (hd : HasDerivAt g g' t)
    (hmax : IsLocalMax (fun t => g t - c * t) t) : g' = c := by
  have h2 : HasDerivAt (fun t => g t - c * t) (g' - c * 1) t :=
    hd.sub ((hasDerivAt_id t).const_mul c)
  have := hmax.hasDerivAt_eq_zero h2
  linarith

/-- Section 4.4, conditions (ii) and (iii) with two types and unbiased
matching. Stocks `M_i = N_i t_i`, and `q_i = M_i/(M_i + M_j)`. -/
def steady2 (Ni Nj ti tj qi : ℝ) : Prop := qi = Ni * ti / (Ni * ti + Nj * tj)

/-- Proof of Proposition 3. Eq. (8), `N_j t_j q_i = N_i t_i (1 − q_i)`, which is the
balance `N_i d_i = N_j d_j`, is equivalent to condition (iii). -/
theorem eq8_iff {Ni Nj ti tj qi : ℝ} (hpos : 0 < Ni * ti + Nj * tj) :
    Nj * tj * qi = Ni * ti * (1 - qi) ↔ steady2 Ni Nj ti tj qi := by
  unfold steady2
  rw [eq_div_iff hpos.ne']
  constructor <;> intro h <;> linarith

/-- Section 4.5, the substitution before eq. (4). With `t(q) ∝ γ + (1 − γ) q`,
condition (iii) `q_i/q_j = w_i t_i/(w_j t_j)` with `q_j = 1 − q_i` is a quadratic in
`q_i`, with `x = w_i/(1 − w_i)`. -/
theorem eq4_quadratic (x γ q : ℝ) :
    q * ((1 - γ) * (1 - q) + γ) - x * ((1 - q) * ((1 - γ) * q + γ))
      = (x - 1) * (1 - γ) * q ^ 2 + (1 - x * (1 - 2 * γ)) * q - x * γ := by ring

/-- Eq. (4). The printed root solves the quadratic. -/
theorem eq4_root {x γ : ℝ} (hx : x ≠ 1) (hγ : γ ≠ 1)
    (hD : 0 ≤ (x - (1 - 2 * γ ^ 2)) ^ 2 + 4 * γ ^ 2 * (1 - γ ^ 2)) :
    let r := Real.sqrt ((x - (1 - 2 * γ ^ 2)) ^ 2 + 4 * γ ^ 2 * (1 - γ ^ 2))
    let q := ((1 - 2 * γ) * x - 1 + r) / (2 * (x - 1) * (1 - γ))
    (x - 1) * (1 - γ) * q ^ 2 + (1 - x * (1 - 2 * γ)) * q - x * γ = 0 := by
  intro r q
  have hr : r ^ 2 = (x - (1 - 2 * γ ^ 2)) ^ 2 + 4 * γ ^ 2 * (1 - γ ^ 2) := Real.sq_sqrt hD
  have h1 : x - 1 ≠ 0 := sub_ne_zero.mpr hx
  have h2 : 1 - γ ≠ 0 := sub_ne_zero.mpr (Ne.symm hγ)
  simp only [q]
  field_simp
  ring_nf
  nlinarith [hr]

/-! ### Section 6, two types -/

/-- Proposition 3. With `N_i > N_j > 0` and an optimal-time function `t`
continuous and positive on `[0, 1]`, eq. (8) has a solution `q_i ∈ (1/2, 1)`, so an
equilibrium exists in which `q_i > 1/2 > q_j` and relative homophily holds. The
paper's proof uses that `t` is bounded and bounded away from 0 (footnote 46),
which continuity on `[0, 1]` gives. -/
theorem prop3 {Ni Nj : ℝ} (hN : Nj < Ni) (hNj : 0 < Nj) (t : ℝ → ℝ)
    (ht : ContinuousOn t (Icc 0 1)) (hpos : ∀ q ∈ Icc (0 : ℝ) 1, 0 < t q) :
    ∃ q, 1 / 2 < q ∧ q < 1 ∧ Nj * t (1 - q) * q = Ni * t q * (1 - q) := by
  set F : ℝ → ℝ := fun q => Ni * t q * (1 - q) - Nj * t (1 - q) * q with hF
  have hsub : Icc (1 / 2 : ℝ) 1 ⊆ Icc 0 1 := Icc_subset_Icc (by norm_num) le_rfl
  have hmaps : MapsTo (fun q : ℝ => 1 - q) (Icc (1 / 2) 1) (Icc 0 1) := by
    intro q hq; constructor <;> linarith [hq.1, hq.2]
  have hc1 : ContinuousOn (fun q => t (1 - q)) (Icc (1 / 2 : ℝ) 1) :=
    ht.comp (continuousOn_const.sub continuousOn_id) hmaps
  have hcF : ContinuousOn F (Icc (1 / 2 : ℝ) 1) := by
    apply ContinuousOn.sub
    · exact ((continuousOn_const.mul (ht.mono hsub)).mul (continuousOn_const.sub continuousOn_id))
    · exact ((continuousOn_const.mul hc1).mul continuousOn_id)
  have ht12 := hpos (1 / 2) ⟨by norm_num, by norm_num⟩
  have ht0 := hpos 0 ⟨le_rfl, by norm_num⟩
  have hFa : 0 < F (1 / 2) := by
    simp only [hF]; norm_num; nlinarith
  have hFb : F 1 < 0 := by
    simp only [hF]; norm_num; positivity
  obtain ⟨q, hq, hq0⟩ := intermediate_value_Icc' (by norm_num : (1 / 2 : ℝ) ≤ 1) hcF
    ⟨hFb.le, hFa.le⟩
  have hne1 : q ≠ 1 / 2 := by rintro rfl; linarith
  have hne2 : q ≠ 1 := by rintro rfl; linarith
  refine ⟨q, lt_of_le_of_ne hq.1 (Ne.symm hne1), lt_of_le_of_ne hq.2 hne2, ?_⟩
  simp only [hF] at hq0; linarith

/-- Control for Proposition 3. With equal inflows, `q = 1/2` solves eq. (8),
so the strict homophily of the larger group needs `N_i > N_j`. -/
theorem control_equal_inflows_half (N : ℝ) (t : ℝ → ℝ) :
    N * t (1 - 1 / 2) * (1 / 2) = N * t (1 / 2) * (1 - 1 / 2) := by norm_num

/-- Claim 1. If `f(q) = t(q)(1 − q)` is strictly decreasing on `(0, 1)`, eq. (8),
which reads `N_j f(1 − q) = N_i f(q)`, has at most one solution. -/
theorem claim1 {Ni Nj : ℝ} (hNi : 0 < Ni) (hNj : 0 < Nj) (f : ℝ → ℝ)
    (hf : StrictAntiOn f (Ioo 0 1)) {q1 q2 : ℝ} (h1 : q1 ∈ Ioo (0 : ℝ) 1)
    (h2 : q2 ∈ Ioo (0 : ℝ) 1) (e1 : Nj * f (1 - q1) = Ni * f q1)
    (e2 : Nj * f (1 - q2) = Ni * f q2) : q1 = q2 := by
  have m1 : 1 - q1 ∈ Ioo (0 : ℝ) 1 := ⟨by linarith [h1.2], by linarith [h1.1]⟩
  have m2 : 1 - q2 ∈ Ioo (0 : ℝ) 1 := ⟨by linarith [h2.2], by linarith [h2.1]⟩
  by_contra hne
  rcases lt_or_gt_of_ne hne with h | h
  · have a := hf h1 h2 h
    have b := hf m2 m1 (by linarith)
    nlinarith
  · have a := hf h2 h1 h
    have b := hf m1 m2 (by linarith)
    nlinarith

/-- Control for Claim 1. Without strict monotonicity uniqueness fails. With
`f` constant and equal inflows every `q` solves eq. (8). -/
theorem control_claim1_needs_anti (q : ℝ) : (1 : ℝ) * (fun _ => (1 : ℝ)) (1 - q) =
    1 * (fun _ => (1 : ℝ)) q := rfl

/-- Proposition 4, in terms of the marginal return `φ(q, t) = ∂/∂t U(q t, (1 − q) t)`.
Overall diminishing returns, eq. (1), make `φ(q, ·)` strictly decreasing. With
same-type bias at `t_j`, `φ(q_i, t_j) > φ(1 − q_i, t_j)`, and the first-order
conditions `φ(q_i, t_i) = c = φ(1 − q_i, t_j)`, the larger group forms more
friendships. With the balance `N_i t_i (1 − q_i) = N_j t_j q_i` and `N_i > N_j`, it
forms more same-type and fewer different-type friendships, and
`q_i/(1 − q_i) > N_i/N_j`, which is inbreeding homophily for `i` and heterophily
for `j`. -/
theorem prop4 (φ : ℝ → ℝ → ℝ) (hdec : ∀ q, StrictAnti (φ q)) {qi ti tj c Ni Nj : ℝ}
    (hbias : φ (1 - qi) tj < φ qi tj) (hi : φ qi ti = c) (hj : φ (1 - qi) tj = c)
    (hqi : 1 / 2 < qi) (hq1 : qi < 1) (htj : 0 < tj) (hNj : 0 < Nj)
    (hN : Nj < Ni) (hbal : Ni * (ti * (1 - qi)) = Nj * (tj * qi)) :
    tj < ti ∧ ti * (1 - qi) < tj * qi ∧ tj * (1 - qi) < ti * qi ∧
      Ni / Nj < qi / (1 - qi) ∧ Ni / (Ni + Nj) < qi ∧ 1 - qi < Nj / (Ni + Nj) := by
  have h1 : tj < ti := by
    by_contra h; push_neg at h
    have := (hdec qi).antitone h
    linarith
  have hdi : ti * (1 - qi) < tj * qi := by
    have hpos : 0 < tj * qi := by positivity
    nlinarith
  have hs : tj * (1 - qi) < ti * qi := by nlinarith
  have hr : Ni / Nj < qi / (1 - qi) := by
    rw [div_lt_div_iff₀ hNj (by linarith)]
    have hpos : 0 < Ni * (1 - qi) := mul_pos (by linarith) (by linarith)
    have m := mul_lt_mul_of_pos_left h1 hpos
    have key : Ni * (1 - qi) * tj < qi * Nj * tj := by nlinarith
    exact lt_of_mul_lt_mul_right key htj.le
  have hw : Ni / (Ni + Nj) < qi := by
    rw [div_lt_iff₀ (by linarith)]
    rw [div_lt_div_iff₀ hNj (by linarith)] at hr; nlinarith
  refine ⟨h1, hdi, hs, hr, hw, ?_⟩
  have : Nj / (Ni + Nj) = 1 - Ni / (Ni + Nj) := by
    rw [eq_sub_iff_add_eq, ← add_div, add_comm Nj Ni, div_self (by linarith)]
  rw [this]; linarith

/-- Control for Proposition 4. Without same-type bias, when `φ` does not depend
on the mix, the first-order conditions give the two groups the same time. -/
theorem control_no_bias_equal_times (ψ : ℝ → ℝ) (hdec : StrictAnti ψ) {ti tj c : ℝ}
    (hi : ψ ti = c) (hj : ψ tj = c) : ti = tj :=
  hdec.injective (hi.trans hj.symm)

/-- Proposition 5, first step. Under strong same-type bias (Definition 8, `φ`
strictly increasing in `q`) and diminishing returns, the optimal time `t(q)`
defined by `φ(q, t(q)) = c` is strictly increasing. -/
theorem prop5_t_mono (φ : ℝ → ℝ → ℝ) (hdec : ∀ q, StrictAnti (φ q))
    (hinc : ∀ t, StrictMono (fun q => φ q t)) (t : ℝ → ℝ) {c : ℝ}
    (hfoc : ∀ q, φ q (t q) = c) : StrictMono t := by
  intro q q' hqq
  by_contra h; push_neg at h
  have a : φ q (t q) < φ q' (t q) := hinc (t q) hqq
  have b : φ q' (t q) ≤ φ q' (t q') := (hdec q').antitone h
  linarith [hfoc q, hfoc q']

/-- Proposition 5, second step. If the right side `R` of eq. (8) is strictly
decreasing (Claim 1), and the left side is `(1 − w_i)/w_i`, a larger share gives a
larger `q_i`, and then by `prop5_t_mono` more friendships. -/
theorem prop5_q_mono (R : ℝ → ℝ) (hR : StrictAnti R) {w w' q q' : ℝ} (hw : 0 < w)
    (hww : w < w') (hq : R q = (1 - w) / w) (hq' : R q' = (1 - w') / w') : q < q' := by
  by_contra h; push_neg at h
  have a : R q ≤ R q' := hR.antitone h
  have b : (1 - w') / w' < (1 - w) / w := by
    rw [div_lt_div_iff₀ (by linarith) hw]; nlinarith
  linarith

/-- Control for Proposition 5. If `φ` does not depend on `q`, the optimal time
is the same at every mix. -/
theorem control_prop5_flat (ψ : ℝ → ℝ) (hdec : StrictAnti ψ) (t : ℝ → ℝ) {c : ℝ}
    (hfoc : ∀ q, ψ (t q) = c) (q q' : ℝ) : t q = t q' :=
  hdec.injective ((hfoc q).trans (hfoc q').symm)

/-! ### Section 8, many types -/

/-- Proof of Proposition 6. Condition (iii) makes `t_i w_i/q_i` common to all
types, so with `t_i > t_j`, `q_i/w_i > q_j/w_j`. -/
theorem prop6_ratio {ti tj wi wj qi qj K : ℝ} (hK : 0 < K) (hwi : 0 < wi) (hwj : 0 < wj)
    (hi : ti * wi = K * qi) (hj : tj * wj = K * qj)
    (ht : tj < ti) : qj / wj < qi / wi := by
  rw [div_lt_div_iff₀ hwj hwi]
  have e1 : qi = ti * wi / K := by field_simp; linarith
  have e2 : qj = tj * wj / K := by field_simp; linarith
  rw [e1, e2, div_mul_eq_mul_div, div_mul_eq_mul_div, div_lt_div_iff_of_pos_right hK]
  nlinarith [mul_pos hwi hwj]

/-- Proposition 6, the last step, where it holds. If `q_i/w_i > q_j/w_j`,
`w_i > w_j` and the smaller type `j` is at or above baseline, `IH_i > IH_j`. -/
theorem prop6_IH_order {qi qj wi wj : ℝ} (hwj : 0 < wj) (hwij : wj < wi) (hwi : wi < 1)
    (hr : qj / wj < qi / wi) (hj : wj ≤ qj) : IH qj wj < IH qi wi := by
  unfold IH
  have hwi0 : 0 < wi := by linarith
  have ri : qi = qi / wi * wi := by field_simp
  have rj : qj = qj / wj * wj := by field_simp
  set a := qi / wi
  set b := qj / wj
  have hb : 1 ≤ b := by rw [le_div_iff₀ hwj]; linarith
  rw [ri, rj, div_lt_div_iff₀ (by linarith) (by linarith)]
  have c1 : 0 ≤ (b - 1) * (wi * (1 - wj) - wj * (1 - wi)) :=
    mul_nonneg (by linarith) (by nlinarith)
  have c2 : 0 < (a - b) * (wi * (1 - wj)) := mul_pos (by linarith) (mul_pos hwi0 (by linarith))
  nlinarith

/-- CJP-D8. The step "it also follows easily" in the proof of Proposition 6 fails
for types below baseline. Shares `w = (1/10, 2/5, 1/2)` and meeting probabilities
`q = (1/25, 1/5, 19/25)` sum to 1 and have `q_i/w_i = (2/5, 1/2, 38/25)` rising in
`w_i`, as the proof requires, yet the middle type has a lower `IH` than the
smallest. Whether some preferences realise these numbers in equilibrium is not
checked. -/
theorem control_prop6_step_fails :
    (1 : ℝ) / 25 + 1 / 5 + 19 / 25 = 1 ∧ (1 : ℝ) / 10 + 2 / 5 + 1 / 2 = 1 ∧
      (1 / 25 : ℝ) / (1 / 10) < (1 / 5) / (2 / 5) ∧ (1 / 5 : ℝ) / (2 / 5) < (19 / 25) / (1 / 2) ∧
      IH (1 / 5) (2 / 5) < IH (1 / 25) (1 / 10) := by
  refine ⟨by norm_num, by norm_num, by norm_num, by norm_num, ?_⟩
  unfold IH; norm_num

/-! ### Section 9, welfare -/

/-- Proposition 7, eq. (16). If `U` is homogeneous of degree `α`, then along the ray
`U(q t, (1 − q) t) = t^α u` with `u = U(q, 1 − q)`, and the first-order condition
`α t^{α−1} u = c` gives `U(q t, (1 − q) t) = c t/α`. Welfare is proportional to the
number of friends. -/
theorem prop7 {α c t u : ℝ} (hα : α ≠ 0) (ht : 0 < t) (hfoc : α * t ^ (α - 1) * u = c) :
    t ^ α * u = c * t / α := by
  rw [← hfoc, Real.rpow_sub_one ht.ne']
  field_simp

/-- Control for Proposition 7. Without homogeneity the proportionality fails.
Under the logarithmic utility of Appendix D the optimal time does not depend on
`q` (`appD_time`) while utility does. At `a = g = 1`, `b = 0`, the mixes `q = 1/2`
and `q = 1/4` give different values of `log q + log(1 − q)`. -/
theorem control_log_not_proportional :
    Real.log (1 / 2) + Real.log (1 - 1 / 2) ≠ Real.log (1 / 4) + Real.log (1 - 1 / 4) := by
  rw [← Real.log_mul (by norm_num) (by norm_num), ← Real.log_mul (by norm_num) (by norm_num)]
  intro h
  have := Real.log_injOn_pos (Set.mem_Ioi.2 (by norm_num : (0 : ℝ) < 1 / 2 * (1 - 1 / 2)))
    (Set.mem_Ioi.2 (by norm_num : (0 : ℝ) < 1 / 4 * (1 - 1 / 4))) h
  norm_num at this

/-- Proposition 8. With stocks `N_i t_i = M q_i` (conditions (ii), (iii)), utilities
`U_i = c t_i/α` (Proposition 7), shares `w_i = N_i/N`, and `Σ q_i = 1`, average
welfare is `w_k U_k/q_k` for any type `k`, which is `c w_k t_k/(α q_k)`. -/
theorem prop8 {ι : Type*} [Fintype ι] (Nf t q w U : ι → ℝ) (M Ntot c α : ℝ) (hα : α ≠ 0)
    (hNt : Ntot ≠ 0) (hst : ∀ i, Nf i * t i = M * q i) (hU : ∀ i, U i = c * t i / α)
    (hw : ∀ i, w i = Nf i / Ntot) (hq : ∑ i, q i = 1) (k : ι) (hqk : q k ≠ 0) :
    ∑ i, w i * U i = w k * U k / q k := by
  have e : ∀ i, w i * U i = c * M / (α * Ntot) * q i := by
    intro i; rw [hw, hU, ← mul_div_assoc]
    rw [show Nf i / Ntot * (c * t i) = c * (Nf i * t i) / Ntot by ring, hst]
    field_simp
  simp only [e, ← Finset.mul_sum, hq, mul_one]
  field_simp

/-- Section 9, `U(s, d) = √s + γ √d`. The first-order condition gives
`t = (√q + γ √(1 − q))²/(4c²)`, which is the printed
`((1 − γ²) q + γ² + 2γ √(q(1 − q)))/(4c²)`. -/
theorem sec9_t {γ q c : ℝ} (hq0 : 0 ≤ q) (hq1 : q ≤ 1) :
    (Real.sqrt q + γ * Real.sqrt (1 - q)) ^ 2 / (4 * c ^ 2)
      = ((1 - γ ^ 2) * q + γ ^ 2 + 2 * γ * Real.sqrt (q * (1 - q))) / (4 * c ^ 2) := by
  congr 1
  have h1 : Real.sqrt q ^ 2 = q := Real.sq_sqrt hq0
  have h2 : Real.sqrt (1 - q) ^ 2 = 1 - q := Real.sq_sqrt (by linarith)
  rw [Real.sqrt_mul hq0]
  nlinarith [h1, h2]

/-- Section 9. The time `t(q)` has its critical point at `q* = 1/(1 + γ²)`. There
`√(q*(1 − q*)) = γ/(1 + γ²)` and the derivative of the numerator,
`(1 − γ²) + γ (1 − 2q)/√(q(1 − q))`, vanishes. For `0 < γ < 1`, `1/2 < q* < 1`. -/
theorem sec9_argmax {γ : ℝ} (hγ : 0 < γ) :
    let qs := 1 / (1 + γ ^ 2)
    Real.sqrt (qs * (1 - qs)) = γ / (1 + γ ^ 2) ∧
      (1 - γ ^ 2) + γ * (1 - 2 * qs) / Real.sqrt (qs * (1 - qs)) = 0 ∧
      (γ < 1 → 1 / 2 < qs ∧ qs < 1) := by
  intro qs
  have hd : 0 < 1 + γ ^ 2 := by positivity
  have e : qs * (1 - qs) = (γ / (1 + γ ^ 2)) ^ 2 := by
    simp only [qs]; field_simp; ring
  have hs : Real.sqrt (qs * (1 - qs)) = γ / (1 + γ ^ 2) := by
    rw [e, Real.sqrt_sq (by positivity)]
  refine ⟨hs, ?_, ?_⟩
  · rw [hs]; simp only [qs]; field_simp; ring
  · intro h1
    simp only [qs]
    constructor
    · rw [div_lt_div_iff₀ (by norm_num) hd]; nlinarith
    · rw [div_lt_one hd]; nlinarith

/-! ### Appendix C, Proposition 10 -/

/-- Proof of Proposition 10, eqs. (21) and (22). From
`t′ = α (t (U_s − U_d) + t′ c)/c` it follows that `t′ = α/(1 − α) · t (U_s − U_d)/c`. -/
theorem eq22 {α c t t' Us Ud : ℝ} (hc : c ≠ 0) (hα : α ≠ 1)
    (h21 : t' = α * (t * (Us - Ud) + t' * c) / c) :
    t' = α / (1 - α) * (t * (Us - Ud) / c) := by
  have h1 : 1 - α ≠ 0 := sub_ne_zero.mpr (Ne.symm hα)
  field_simp at h21 ⊢
  linarith

/-- Proposition 10, the bound. With `U_d > 0`, `0 < α < 1` and the first-order
condition `q U_s + (1 − q) U_d = c > 0`, the elasticity
`t′ q/t = α/(1 − α) · q (U_s − U_d)/c` is below `α/(1 − α)`, which is at most 1 when
`α ≤ 1/2`. -/
theorem prop10_bound {α c q Us Ud : ℝ} (hα0 : 0 < α) (hα : α ≤ 1 / 2) (hc : 0 < c)
    (hUd : 0 < Ud) (hfoc : q * Us + (1 - q) * Ud = c) :
    α / (1 - α) * (q * (Us - Ud) / c) < 1 := by
  have h1 : 0 < 1 - α := by linarith
  have hk : α / (1 - α) ≤ 1 := by rw [div_le_one h1]; linarith
  have hk0 : 0 < α / (1 - α) := div_pos hα0 h1
  have hx : q * (Us - Ud) / c < 1 := by
    rw [div_lt_one hc]; nlinarith
  by_cases hneg : q * (Us - Ud) / c ≤ 0
  · nlinarith
  · push_neg at hneg; nlinarith

/-- Proof of Proposition 10. `∂(t/q)/∂q < 0` if and only if `t′ q/t < 1`. -/
theorem prop10_ratio {t t' q : ℝ} (ht : 0 < t) (hq : 0 < q) :
    (t' * q - t) / q ^ 2 < 0 ↔ t' * q / t < 1 := by
  rw [div_neg_iff, div_lt_one ht]
  constructor
  · rintro (⟨a, _⟩ | ⟨a, _⟩)
    · nlinarith [sq_nonneg q]
    · linarith
  · intro h; right; exact ⟨by linarith, by positivity⟩

/-- Control for Proposition 10. At `α = 0.9`, `α/(1 − α) = 9 > 1`, so the bound that
yields well-ordered equilibria fails, as footnote 47's example shows. -/
theorem control_prop10_alpha : (9 / 10 : ℝ) / (1 - 9 / 10) = 9 := by norm_num

/-! ### Proposition 9, biased matching -/

/-- Proposition 9. A continuous `f` from `[0, 1]` into `[0, 1]` has a fixed point
`q̂`, by the intermediate value theorem. -/
theorem prop9_fixed {f : ℝ → ℝ} (hf : ContinuousOn f (Icc 0 1))
    (hmap : ∀ q ∈ Icc (0 : ℝ) 1, f q ∈ Icc (0 : ℝ) 1) : ∃ q ∈ Icc (0 : ℝ) 1, f q = q := by
  have hc : ContinuousOn (fun q => q - f q) (Icc 0 1) := continuousOn_id.sub hf
  have h0 := hmap 0 ⟨le_rfl, zero_le_one⟩
  have h1 := hmap 1 ⟨zero_le_one, le_rfl⟩
  obtain ⟨q, hq, e⟩ := intermediate_value_Icc (zero_le_one' ℝ) hc
    (show (0 : ℝ) ∈ Icc ((fun q => q - f q) 0) ((fun q => q - f q) 1) from
      ⟨by simp only; linarith [h0.1], by simp only; linarith [h1.2]⟩)
  exact ⟨q, hq, by simp only at e; linarith⟩

/-- Proposition 9. A strictly decreasing `f` has at most one fixed point, and
`q₁ > q̂` implies `q₂ = f(q₁) < q̂ < q₁`. -/
theorem prop9_order {f : ℝ → ℝ} (hf : StrictAnti f) {qh q1 : ℝ} (hfix : f qh = qh)
    (h : qh < q1) : f q1 < qh ∧ f q1 < q1 ∧ ∀ a, f a = a → a = qh := by
  refine ⟨by rw [← hfix]; exact hf h, by have := hf h; linarith, fun a ha => ?_⟩
  by_contra hne
  rcases lt_or_gt_of_ne hne with h' | h'
  · have := hf h'; linarith
  · have := hf h'; linarith

/-- Control for Proposition 9. Without monotonicity the fixed point need not be
unique. The identity fixes every point. -/
theorem control_prop9_needs_anti : (fun q : ℝ => q) 0 = 0 ∧ (fun q : ℝ => q) 1 = 1 :=
  ⟨rfl, rfl⟩

/-! ### Appendix D, logarithmic preferences -/

/-- Appendix D. With `U(s, d) = a log s + b log(s + d) + g log d`, along the ray,
`U(q t, (1 − q) t) = a log q + g log(1 − q) + (a + b + g) log t`. -/
theorem appD_ray {a b g q t : ℝ} (hq0 : 0 < q) (hq1 : q < 1) (ht : 0 < t) :
    a * Real.log (q * t) + b * Real.log (q * t + (1 - q) * t) + g * Real.log ((1 - q) * t)
      = a * Real.log q + g * Real.log (1 - q) + (a + b + g) * Real.log t := by
  have e : q * t + (1 - q) * t = t := by ring
  rw [e, Real.log_mul hq0.ne' ht.ne', Real.log_mul (by linarith) ht.ne']
  ring

/-- Footnote 48. The first-order condition `(a + b + g)/t = c` gives
`t = (a + b + g)/c`, the same for every type, so unbiased matching yields
`q_i = N_i t/Σ N_j t = w_i`, baseline homophily. -/
theorem appD_time {ι : Type*} [Fintype ι] (Nf : ι → ℝ) (A c : ℝ) (hc : 0 < c) (hA : 0 < A)
    (hN : 0 < ∑ j, Nf j) (i : ι) :
    Nf i * (A / c) / ∑ j, Nf j * (A / c) = Nf i / ∑ j, Nf j := by
  rw [← Finset.sum_mul]
  have : A / c ≠ 0 := (div_pos hA hc).ne'
  field_simp

/-- Appendix D, the case `a = 1`, `b = 0`, `g > 0`. The derivative of
`log(w(1 + g)/c) + g log((1 − w)(1 + g)/c)` in `w`, `1/w − g/(1 − w)`, vanishes at
`w = 1/(1 + g)`. -/
theorem appD_peak {g : ℝ} (hg : 0 < g) :
    1 / (1 / (1 + g)) - g / (1 - 1 / (1 + g)) = 0 := by
  have h1 : 1 - 1 / (1 + g) = g / (1 + g) := by
    have : (1 + g) ≠ 0 := by positivity
    field_simp; ring
  rw [h1, one_div_one_div, div_div_eq_mul_div, mul_div_cancel_left₀ _ hg.ne']; ring

/-- Appendix D. The printed numerator of the second derivative,
`−g + (1 + g) w (1 − w)`, is negative at every `w ∈ (0, 1)` if and only if `g > 1/3`.
The second derivative itself is not formalised. -/
theorem appD_concave (g : ℝ) :
    (∀ w ∈ Ioo (0 : ℝ) 1, -g + (1 + g) * w * (1 - w) < 0) ↔ 1 / 3 < g := by
  constructor
  · intro h; have := h (1 / 2) ⟨by norm_num, by norm_num⟩; linarith
  · intro hg w hw
    have : w * (1 - w) ≤ 1 / 4 := by nlinarith [sq_nonneg (w - 1 / 2)]
    nlinarith

end CurrariniEtAl2008
