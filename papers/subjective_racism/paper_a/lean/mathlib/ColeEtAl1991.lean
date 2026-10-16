import Mathlib

/-! Cole, Mailath and Postlewaite, "Social Norms, Savings Behavior, and
Growth", CARESS Working Paper 91-14 (June 1990, revised June 1991), in the
paper's notation.

Men are indexed by `i ∈ [0, 1]` and women by their endowment `j`, uniform on
`[0, 1]`, which enters utility linearly. `u` is CRRA of degree `γ`, `β` the
discount factor, `A` the gross return, `c = A k − k'` (eq. 1). Matching is
voluntary under complete information. A social norm is a status assignment
rule with status-dependent matching. Three norms are studied:
wealth-is-status, aristocratic (eq. 12) and hybrid.

Coverage. Formalised in general: eqs. (6) to (8), (12), (13), the closed form
of `L_t(j)`, the hybrid match value and loss, the welfare comparison of p. 25
(matching part), and the bound of Section 5.3. Formalised in a special case:
eq. (2) (first-order condition in ratio form), `∂λ(j)/∂j < 0` (as a monotone
inverse), eq. (10)'s comparative claim (matching term held fixed),
Property 1 (the exchange argument) and Property 3 (the step for a uniform
pool). Not formalised: Propositions I to III, Properties 2, 4, 5, 6, the
Lemma, eqs. (11), (14) and (15). They rest on Mas-Colell's existence theorem
and on limits in `t` and `β`, which Mathlib does not carry in usable form.
See `lit/cole_mailath_postlewaite_1991/CLAIMS.md`. -/

namespace ColeEtAl1991

open MeasureTheory

/-! ### Primitives and solution concepts -/

/-- The CRRA felicity `u(c) = (1 − γ)⁻¹ c^{1−γ}`. -/
noncomputable def u (γ c : ℝ) : ℝ := (1 - γ)⁻¹ * c ^ (1 - γ)

/-- The stationary consumption ratio `λ* = 1 − (A^{1−γ} β)^{1/γ}` (PDF p. 13). -/
noncomputable def lamStar (A β γ : ℝ) : ℝ := 1 - (A ^ (1 - γ) * β) ^ (1 / γ)

/-- A voluntary matching (PDF p. 7). `Pman i a b` says man `i` strictly prefers
woman `a` to woman `b`, and `Pwoman j a b` says woman `j` strictly prefers man
`a` to man `b`. (i) No pair blocks. (ii) The matching preserves Lebesgue
measure. -/
def VoluntaryMatching (m : ℝ → ℝ) (Pman Pwoman : ℝ → ℝ → ℝ → Prop) : Prop :=
  (¬ ∃ i i', i ≠ i' ∧ i ∈ Set.Icc (0:ℝ) 1 ∧ i' ∈ Set.Icc (0:ℝ) 1 ∧
      Pman i (m i') (m i) ∧ Pwoman (m i') i i') ∧
  (∀ B ⊆ Set.Icc (0:ℝ) 1, MeasurableSet B → volume (m '' B) = volume B)

/-- Wealth-is-status. A woman accepts the wealthiest proposer, so man `i`'s
match is the mass of men with less capital (CM-Q6). With capital strictly
increasing in the index, that mass is `i`, so `m(i) = i`. -/
theorem wis_rank {k : ℝ → ℝ} (hk : StrictMono k) {i : ℝ} (hi1 : i ≤ 1) :
    volume {i' ∈ Set.Icc (0:ℝ) 1 | k i' < k i} = ENNReal.ofReal i := by
  have e : {i' ∈ Set.Icc (0:ℝ) 1 | k i' < k i} = Set.Ico 0 i := by
    ext x
    simp only [Set.mem_setOf_eq, Set.mem_Icc, Set.mem_Ico, hk.lt_iff_lt]
    constructor
    · rintro ⟨⟨h0, _⟩, h⟩; exact ⟨h0, h⟩
    · rintro ⟨h0, h⟩; exact ⟨⟨h0, by linarith⟩, h⟩
  rw [e, Real.volume_Ico, sub_zero]

/-- Control on `wis_rank`: with an atom, here all men equally rich, no man has
anyone below him, and the rank no longer equals the index. -/
theorem control_wis_rank_atom {κ i : ℝ} :
    volume {i' ∈ Set.Icc (0:ℝ) 1 | (fun _ : ℝ => κ) i' < (fun _ : ℝ => κ) i} = 0 := by
  simp

/-- Eq. (12), the aristocratic status rule. Status passes to the son if the
father matched the woman whose endowment equals his status, and is 0
otherwise. -/
noncomputable def n (s j : ℝ) : ℝ := if s = j then s else 0

/-- Eq. (12). Status is kept after an appropriate match, lost after any other,
and zero status is absorbing. -/
theorem eq12 (s j : ℝ) : (s = j → n s j = s) ∧ (s ≠ j → n s j = 0) ∧ n 0 j = 0 := by
  unfold n
  refine ⟨fun h => if_pos h, fun h => if_neg h, ?_⟩
  split_ifs <;> rfl

/-! ### Results -/

/-- Condition (13). A man `i` with capital `k` does not match with a woman `j`
when `V*(k) + j ≤ V*(k) + i/(1 − β)`, that is when `(1 − β) j ≤ i`. -/
theorem eq13 {β i j Vk : ℝ} (hβ : β < 1) :
    Vk + j ≤ Vk + i / (1 - β) ↔ (1 - β) * j ≤ i := by
  have h : 0 < 1 - β := by linarith
  rw [add_le_add_iff_left, le_div_iff₀ h, mul_comm]

/-- Control: (13) fails for a man of status 0 facing any woman of positive
endowment, whatever the discount factor, as the paper says after (13). -/
theorem control_zero_status_deviates {β j : ℝ} (hβ : β < 1) (hj : 0 < j) :
    ¬ ((1 - β) * j ≤ 0) := by
  have : 0 < (1 - β) * j := mul_pos (by linarith) hj
  linarith

/-- The capital `L_t(j)` that leaves woman `j` indifferent, defined by
`V*(L) + j = V*(k) + j/(1 − β)`. The status she gives up is worth
`β j/(1 − β)` in consumption value, so with `V*` strictly increasing `L`
is at least `k`, with equality only at `j = 0`. -/
theorem L_ge_k {V : ℝ → ℝ} (hV : StrictMono V) {β j k L : ℝ} (hβ0 : 0 ≤ β) (hβ : β < 1)
    (hj : 0 ≤ j) (hL : V L + j = V k + j / (1 - β)) :
    V L - V k = β * j / (1 - β) ∧ k ≤ L := by
  have h : (1 : ℝ) - β ≠ 0 := by linarith
  have hd : V L - V k = β * j / (1 - β) := by
    have : V L - V k = j / (1 - β) - j := by linarith
    rw [this]; field_simp; ring
  refine ⟨hd, ?_⟩
  have hnn : 0 ≤ β * j / (1 - β) := div_nonneg (mul_nonneg hβ0 hj) (by linarith)
  by_contra hlt
  push_neg at hlt
  have := hV hlt
  linarith

/-- Control: with no status at stake (`j = 0`) the compensating capital is `k`
itself, so the inequality of `L_ge_k` is not strict in general. -/
theorem control_L_eq_k_at_zero {V : ℝ → ℝ} (hV : StrictMono V) {β k L : ℝ}
    (hL : V L + 0 = V k + 0 / (1 - β)) : L = k := by
  simp at hL
  exact hV.injective hL

/-- The hybrid equilibrium. A man in the upper group `[i, 1]` is matched with a
woman drawn uniformly from `[i, 1]`, so the expected value of his match is the
group mean `(1 + i)/2`. -/
theorem upper_group_mean {i : ℝ} (hi : i < 1) :
    (∫ x in i..1, x) / (1 - i) = (1 + i) / 2 := by
  rw [integral_id]
  have h : (1 : ℝ) - i ≠ 0 := by linarith
  field_simp
  ring

/-- Banishment to the lower group, where the best match is below `i`, costs at
least `(1 + i)/2 − i = (1 − i)/2`, the loss the paper states. -/
theorem banishment_loss (i : ℝ) : (1 + i) / 2 - i = (1 - i) / 2 := by ring

/-- Control: the printed expected value `(1 − i)/2` of a match in the upper
group is not the group mean for any `i > 0` (reading CM-D2). -/
theorem control_printed_value_not_mean {i : ℝ} (hi0 : 0 < i) (hi : i < 1) :
    (∫ x in i..1, x) / (1 - i) ≠ (1 - i) / 2 := by
  rw [upper_group_mean hi]
  intro h
  linarith

/-- The match payoff is linear in `j`, so a random match within a group is
worth the group's mean whatever its spread. Two uniform groups with the same
mean `m` and different half-widths `s` give the same value. -/
theorem spread_blind {m s : ℝ} (hs : 0 < s) :
    (∫ x in (m - s)..(m + s), x) / ((m + s) - (m - s)) = m := by
  rw [integral_id]
  have h : (m + s) - (m - s) ≠ 0 := by linarith
  field_simp
  ring

/-- Eq. (6) at a stationary point. Writing `x = λ^γ` and `y = (1 − λ)^γ`,
`x = x/y · A^{1−γ} β` with `x, y ≠ 0` forces `y = A^{1−γ} β`, which is the
stationary ratio `λ* = 1 − (A^{1−γ} β)^{1/γ}`. -/
theorem stationary_ratio {x y K : ℝ} (hx : x ≠ 0) (hy : y ≠ 0) (h : x = x / y * K) :
    y = K := by
  have e : x / y * K = x * (K / y) := by ring
  rw [e] at h
  have h1 : K / y = 1 := mul_left_cancel₀ hx (h.symm.trans (mul_one x).symm)
  rw [div_eq_one_iff_eq hy] at h1
  exact h1.symm

/-- Eq. (2), the two-period example. With `u` CRRA, the first-order condition
of `max_λ u(AKλ) + β u(A²K(1 − λ))` reduces to `((1 − λ)/λ)^γ = β A^{1−γ}`,
and `λ(0) = [1 + (β A^{1−γ})^{1/γ}]⁻¹` solves it and lies in `(0, 1)`. -/
theorem lambda0_foc {β A γ : ℝ} (hβ : 0 < β) (hA : 0 < A) (hγ : 0 < γ) :
    let K := β * A ^ (1 - γ)
    let l := (1 + K ^ (1 / γ))⁻¹
    0 < l ∧ l < 1 ∧ ((1 - l) / l) ^ γ = K := by
  intro K l
  have hK : 0 < K := mul_pos hβ (Real.rpow_pos_of_pos hA _)
  have hr : 0 < K ^ (1 / γ) := Real.rpow_pos_of_pos hK _
  have hd : 0 < 1 + K ^ (1 / γ) := by linarith
  have hl : 0 < l := inv_pos.2 hd
  have hl1 : l < 1 := inv_lt_one_of_one_lt₀ (by linarith)
  refine ⟨hl, hl1, ?_⟩
  have e : (1 - l) / l = K ^ (1 / γ) := by
    show (1 - (1 + K ^ (1 / γ))⁻¹) / (1 + K ^ (1 / γ))⁻¹ = K ^ (1 / γ)
    field_simp
    ring
  rw [e, one_div, Real.rpow_inv_rpow hK.le hγ.ne']

/-- The two-period example, `∂λ(j)/∂j < 0`. `g(λ) = u(AKλ) + β u(A²K(1 − λ))`
is strictly increasing below `λ(0)`, and `V(j) = V(0)` gives
`g(λ(j)) = V(0) − β j`, so `λ(j)` falls as `j` rises. The monotone inverse
replaces the paper's implicit derivative. -/
theorem lambda_j_antitone {g : ℝ → ℝ} {S : Set ℝ} (hg : StrictMonoOn g S)
    {V0 β : ℝ} (hβ : 0 < β) {lam : ℝ → ℝ} (hS : ∀ j, lam j ∈ S)
    (hV : ∀ j, g (lam j) = V0 - β * j) {j1 j2 : ℝ} (h12 : j1 < j2) : lam j2 < lam j1 := by
  have : g (lam j2) < g (lam j1) := by rw [hV, hV]; nlinarith
  exact (hg.lt_iff_lt (hS _) (hS _)).1 this

/-- Eq. (7). Under exogenous matching `k_{t+1} = (1 − λ*) A k_t`, so
`k_{t+n} = (1 − λ*)^n A^n k_t`. -/
theorem eq7 {l A : ℝ} {k : ℕ → ℝ} (hk : ∀ t, k (t + 1) = (1 - l) * A * k t) (t n : ℕ) :
    k (t + n) = (1 - l) ^ n * A ^ n * k t := by
  induction n with
  | zero => simp
  | succ n ih => rw [← add_assoc, hk, ih]; ring

/-- The identity behind eq. (8). `(1 − λ*)^γ = A^{1−γ} β` gives
`β ((1 − λ*) A)^{1−γ} = 1 − λ*`. -/
theorem discount_identity {β A γ l : ℝ} (hA : 0 < A) (hl1 : l < 1)
    (h : (1 - l) ^ γ = A ^ (1 - γ) * β) : β * ((1 - l) * A) ^ (1 - γ) = 1 - l := by
  have h1 : 0 < 1 - l := by linarith
  rw [Real.mul_rpow h1.le hA.le]
  have e : (1 - l) ^ (1 - γ) * (1 - l) ^ γ = 1 - l := by
    rw [← Real.rpow_add h1]; simp
  calc β * ((1 - l) ^ (1 - γ) * A ^ (1 - γ)) = (1 - l) ^ (1 - γ) * (A ^ (1 - γ) * β) := by ring
    _ = 1 - l := by rw [← h, e]

/-- A real power of a natural power. -/
theorem pow_rpow_swap {x y : ℝ} (hx : 0 ≤ x) (n : ℕ) : (x ^ n) ^ y = (x ^ y) ^ n := by
  rw [← Real.rpow_natCast, ← Real.rpow_natCast, ← Real.rpow_mul hx, ← Real.rpow_mul hx, mul_comm]

/-- Eq. (8). The discounted utility of consumption along eq. (7) sums to
`(λ* A k_0)^{1−γ}/((1 − γ) λ*) = V*(k_0)`, using the stationarity condition
`(1 − λ*)^γ = A^{1−γ} β`. -/
theorem eq8 {β A γ l k0 : ℝ} (hA : 0 < A) (hl0 : 0 < l) (hl1 : l < 1) (hk : 0 < k0)
    (h : (1 - l) ^ γ = A ^ (1 - γ) * β) :
    HasSum (fun t : ℕ => β ^ t * u γ (l * (1 - l) ^ t * A ^ (t + 1) * k0))
      ((l * A * k0) ^ (1 - γ) / ((1 - γ) * l)) := by
  have h1 : 0 < 1 - l := by linarith
  have hr := discount_identity hA hl1 h
  have term : ∀ t : ℕ, β ^ t * u γ (l * (1 - l) ^ t * A ^ (t + 1) * k0)
      = ((1 - γ)⁻¹ * (l * A * k0) ^ (1 - γ)) * (1 - l) ^ t := by
    intro t
    unfold u
    have e1 : l * (1 - l) ^ t * A ^ (t + 1) * k0 = (l * A * k0) * ((1 - l) * A) ^ t := by
      rw [mul_pow, pow_succ]; ring
    rw [e1, Real.mul_rpow (by positivity) (by positivity),
      pow_rpow_swap (by positivity)]
    conv_rhs => rw [← hr, mul_pow]
    ring
  rw [show (fun t : ℕ => β ^ t * u γ (l * (1 - l) ^ t * A ^ (t + 1) * k0))
      = fun t : ℕ => ((1 - γ)⁻¹ * (l * A * k0) ^ (1 - γ)) * (1 - l) ^ t from funext term]
  have hs := (hasSum_geometric_of_lt_one h1.le (by linarith : 1 - l < 1)).mul_left
    ((1 - γ)⁻¹ * (l * A * k0) ^ (1 - γ))
  convert hs using 1
  rw [show (1 : ℝ) - (1 - l) = l by ring]
  field_simp

/-- Eq. (10) and the claim after it. With `u'` decreasing (`up`), and `k_t`,
`k_{t+2}` fixed, a larger matching term `m'_{t+1}` gives a larger bequest
`k_{t+1}`. The matching term is held fixed at the solution. -/
theorem bequest_rises {up : ℝ → ℝ} (hu : Antitone up) {A β k k2 m1 m2 k1' k2' : ℝ}
    (hA : 0 < A) (hβ : 0 < β) (hm : m1 < m2)
    (h1 : up (A * k - k1') = β * (A * up (A * k1' - k2) + m1))
    (h2 : up (A * k - k2') = β * (A * up (A * k2' - k2) + m2)) : k1' < k2' := by
  by_contra hc
  push_neg at hc
  have a1 : up (A * k - k2') ≤ up (A * k - k1') := hu (by linarith)
  have a2 : up (A * k1' - k2) ≤ up (A * k2' - k2) := hu (by nlinarith)
  have : β * (A * up (A * k1' - k2) + m1) < β * (A * up (A * k2' - k2) + m2) := by
    apply mul_lt_mul_of_pos_left _ hβ
    nlinarith
  linarith

/-- Control: with the same matching term the solution of eq. (10) is unique, so
the bequest rises only because the matching term does. -/
theorem control_euler_unique {up : ℝ → ℝ} (hu : StrictAnti up) {A β k k2 m k1' k2' : ℝ}
    (hA : 0 < A) (hβ : 0 < β)
    (h1 : up (A * k - k1') = β * (A * up (A * k1' - k2) + m))
    (h2 : up (A * k - k2') = β * (A * up (A * k2' - k2) + m)) : k1' = k2' := by
  rcases lt_trichotomy k1' k2' with h | h | h
  · exfalso
    have a1 : up (A * k - k1') < up (A * k - k2') := hu (by linarith)
    have a2 : up (A * k2' - k2) < up (A * k1' - k2) := hu (by nlinarith)
    have : β * (A * up (A * k2' - k2) + m) < β * (A * up (A * k1' - k2) + m) := by
      apply mul_lt_mul_of_pos_left _ hβ; nlinarith
    linarith
  · exact h
  · exfalso
    have a1 : up (A * k - k2') < up (A * k - k1') := hu (by linarith)
    have a2 : up (A * k1' - k2) < up (A * k2' - k2) := hu (by nlinarith)
    have : β * (A * up (A * k1' - k2) + m) < β * (A * up (A * k2' - k2) + m) := by
      apply mul_lt_mul_of_pos_left _ hβ; nlinarith
    linarith

/-- Property 1, the exchange inequality behind the proof (Appendix A.2). For
strictly concave `u`, the pair of middle consumptions beats the pair of
extreme ones. -/
theorem property1_exchange {f : ℝ → ℝ} (hf : StrictConcaveOn ℝ Set.univ f) {a b x y : ℝ}
    (hab : b < a) (hxy : y < x) : f (a - y) + f (b - x) < f (a - x) + f (b - y) := by
  set θ := (a - b) / (a - b + (x - y)) with hθ
  have hd : 0 < a - b + (x - y) := by linarith
  have hθ0 : 0 < θ := div_pos (by linarith) hd
  have hθ1 : θ < 1 := (div_lt_one hd).2 (by linarith)
  have hne : a - y ≠ b - x := by intro h; linarith
  have e1 : a - x = θ * (a - y) + (1 - θ) * (b - x) := by
    rw [hθ]; field_simp; ring
  have e2 : b - y = (1 - θ) * (a - y) + θ * (b - x) := by
    rw [hθ]; field_simp; ring
  have c1 := hf.2 (Set.mem_univ (a - y)) (Set.mem_univ (b - x)) hne hθ0 (by linarith : 0 < 1 - θ)
    (by ring : θ + (1 - θ) = 1)
  have c2 := hf.2 (Set.mem_univ (a - y)) (Set.mem_univ (b - x)) hne (by linarith : 0 < 1 - θ) hθ0
    (by ring : (1 - θ) + θ = 1)
  simp only [smul_eq_mul] at c1 c2
  rw [e1, e2]
  linarith

/-- Property 1, no switching. If the richer line (`a = A k_t(i) > b`) and the
poorer line both choose optimally against the same continuation `W`
(conditions (i) and (ii) of the proof), the richer line does not bequeath
less. The paper's strict ordering also needs Property 3. -/
theorem property1_no_switch {f W : ℝ → ℝ} (hf : StrictConcaveOn ℝ Set.univ f) {a b xi xi' : ℝ}
    (hab : b < a)
    (hi : f (a - xi') + W xi' ≤ f (a - xi) + W xi)
    (hi' : f (b - xi) + W xi ≤ f (b - xi') + W xi') : xi' ≤ xi := by
  by_contra h
  push_neg at h
  have := property1_exchange hf hab h
  linarith

/-- Control: with linear `u` the two optimality conditions of Property 1 sum to
an identity, so they do not rule out a switch. Strict concavity is needed. -/
theorem control_linear_allows_switch {a b xi xi' : ℝ} (W : ℝ → ℝ) :
    (a - xi') + W xi' + ((b - xi) + W xi) = (a - xi) + W xi + ((b - xi') + W xi') := by ring

/-- The closed form of `L_t(j)` (PDF p. 18). With `V*(x) = C x^{1−γ}` (eq. 8),
`V*(L) + j = V*(k) + j/(1 − β)` gives `L^{1−γ} = k^{1−γ} + β j/((1 − β) C)`. -/
theorem L_closed_form {C γ β j k L : ℝ} (hC : C ≠ 0)
    (hL : C * L ^ (1 - γ) + j = C * k ^ (1 - γ) + j / (1 - β)) (hβ : β ≠ 1) :
    L ^ (1 - γ) = k ^ (1 - γ) + β / (1 - β) * j / C := by
  have h : (1 : ℝ) - β ≠ 0 := sub_ne_zero.2 (Ne.symm hβ)
  field_simp
  field_simp at hL
  linarith

/-- The constant of `L_t(j)`. With `C = (λ* A)^{1−γ}/((1 − γ) λ*)`,
`1/C = (1 − γ) (λ*)^γ A^{γ−1}`, the printed coefficient. -/
theorem C_inv {l A γ : ℝ} (hl : 0 < l) (hA : 0 < A) (hγ : γ ≠ 1) :
    ((l * A) ^ (1 - γ) / ((1 - γ) * l))⁻¹ = (1 - γ) * l ^ γ * A ^ (γ - 1) := by
  have h1 : (1 : ℝ) - γ ≠ 0 := sub_ne_zero.2 (Ne.symm hγ)
  rw [Real.mul_rpow hl.le hA.le]
  have el : l ^ (1 - γ) * l ^ γ = l := by rw [← Real.rpow_add hl]; simp
  have ea : A ^ (1 - γ) * A ^ (γ - 1) = 1 := by rw [← Real.rpow_add hA]; simp
  have hlp : 0 < l ^ γ := Real.rpow_pos_of_pos hl _
  have hap : 0 < A ^ (γ - 1) := Real.rpow_pos_of_pos hA _
  have hlq : 0 < l ^ (1 - γ) := Real.rpow_pos_of_pos hl _
  have haq : 0 < A ^ (1 - γ) := Real.rpow_pos_of_pos hA _
  rw [inv_div, div_eq_iff (by positivity)]
  calc (1 - γ) * l = (1 - γ) * (l ^ (1 - γ) * l ^ γ) * (A ^ (1 - γ) * A ^ (γ - 1)) := by
        rw [el, ea]; ring
    _ = (1 - γ) * l ^ γ * A ^ (γ - 1) * (l ^ (1 - γ) * A ^ (1 - γ)) := by ring

/-- Control on `spread_blind`, and the contrast with Paper A. Under a quadratic
loss the expected squared gap to a uniform group of half-width `s` is the
squared gap to its mean plus `s²/3`, so spread matters once the payoff is not
linear. -/
theorem quadratic_spread {c m s : ℝ} (hs : 0 < s) :
    (∫ x in (m - s)..(m + s), (c - x) ^ 2) / ((m + s) - (m - s)) = (c - m) ^ 2 + s ^ 2 / 3 := by
  rw [intervalIntegral.integral_comp_sub_left (fun x => x ^ 2) c, integral_pow]
  have h : (m + s) - (m - s) ≠ 0 := by linarith
  field_simp
  ring

/-- Property 3, the step of its proof. Within a pool of women of positive
measure, here uniform on `[a, b]`, the best woman beats the pool's average, so
a line in an atom gains discretely from a slightly larger bequest. -/
theorem property3_step {a b : ℝ} (hab : a < b) : (∫ x in a..b, x) / (b - a) < b := by
  rw [integral_id]
  have h : 0 < b - a := by linarith
  rw [div_lt_iff₀ h]
  nlinarith

/-- The welfare comparison of PDF p. 25, matching part. A man of index `j` in
the upper group gains from the hybrid equilibrium relative to
wealth-is-status exactly when `j < (1 + i)/2`. -/
theorem hybrid_welfare {i j : ℝ} : j < (1 + i) / 2 ↔ 0 < (1 + i) / 2 - j := by
  constructor <;> intro h <;> linarith

/-- The man of index 1 loses `(1 − i)/2` in matching. -/
theorem top_man_loss (i : ℝ) : 1 - (1 + i) / 2 = (1 - i) / 2 := by ring

/-- Section 5.3. A match changes utility by at most one unit a period, so by
at most `1/(1 − β)` in discounted value. -/
theorem match_value_bound {β : ℝ} (h0 : 0 ≤ β) (h1 : β < 1) :
    HasSum (fun t : ℕ => β ^ t * 1) (1 - β)⁻¹ := by
  simpa using hasSum_geometric_of_lt_one h0 h1

/-- Control on `lambda_j_antitone`: without the matching term (`β j` replaced
by `0 · j`) the consumption share does not vary with the match. -/
theorem control_lambda_j_flat {g : ℝ → ℝ} {S : Set ℝ} (hg : StrictMonoOn g S)
    {V0 : ℝ} {lam : ℝ → ℝ} (hS : ∀ j, lam j ∈ S)
    (hV : ∀ j, g (lam j) = V0 - 0 * j) (j1 j2 : ℝ) : lam j1 = lam j2 :=
  hg.injOn (hS _) (hS _) (by rw [hV, hV]; ring)

/-- The stationary ratio `λ*` satisfies eq. (6)'s stationarity condition
`(1 − λ*)^γ = A^{1−γ} β`. -/
theorem lamStar_stationary {A β γ : ℝ} (hA : 0 < A) (hβ : 0 < β) (hγ : 0 < γ) :
    (1 - lamStar A β γ) ^ γ = A ^ (1 - γ) * β := by
  unfold lamStar
  have hK : 0 < A ^ (1 - γ) * β := mul_pos (Real.rpow_pos_of_pos hA _) hβ
  rw [sub_sub_cancel, one_div, Real.rpow_inv_rpow hK.le hγ.ne']

end ColeEtAl1991
