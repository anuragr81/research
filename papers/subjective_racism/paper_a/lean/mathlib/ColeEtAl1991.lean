import Mathlib

/-! Cole, Mailath and Postlewaite, "Social Norms, Savings Behavior, and
Growth", CARESS Working Paper 91-14 (June 1990, revised June 1991). The
structure Paper A would rely on or contrast with, in the paper's notation.

Women are indexed by their endowment `j`, uniform on `[0, 1]`, and `j` enters
utility linearly. `β` is the discount factor. In the aristocratic equilibrium
a man of status `s` has value `V*(k) + s/(1 − β)`, where `V*` is the value of
consumption from capital `k` (eq. 8). In the hybrid equilibrium the upper
group `[i, 1]` matches at random.

The existence results (Propositions I to III) and the asymptotic Properties 1
to 6 are not formalised. What is formalised is the aristocratic no-deviation
condition (13), the capital that compensates a woman for lost status, the
value of a random match within the upper group of the hybrid equilibrium, and
the stationary consumption ratio of eq. (6). -/

namespace ColeEtAl1991

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

end ColeEtAl1991
