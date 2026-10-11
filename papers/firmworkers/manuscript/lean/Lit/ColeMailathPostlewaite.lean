import Mathlib.Analysis.SpecialFunctions.Pow.Real
import Mathlib.Analysis.SpecificLimits.Basic
import Mathlib.Order.Filter.Extr
import Mathlib.Tactic

open Real Finset

namespace ColeMailathPostlewaite

section Rank

variable {ι : Type*}

noncomputable def rankBelow (s : Finset ι) (x : ι → ℝ) (i : ι) : ℕ := (s.filter (fun k => x k < x i)).card

theorem rankBelow_mono (s : Finset ι) (x : ι → ℝ) {i j : ι} (h : x i ≤ x j) :
    rankBelow s x i ≤ rankBelow s x j := by
  unfold rankBelow
  apply Finset.card_le_card
  intro k hk
  rw [Finset.mem_filter] at hk ⊢
  exact ⟨hk.1, lt_of_lt_of_le hk.2 h⟩

theorem rankBelow_bottom (s : Finset ι) (x : ι → ℝ) {i : ι} (h : ∀ k ∈ s, x i ≤ x k) :
    rankBelow s x i = 0 := by
  unfold rankBelow
  rw [Finset.card_eq_zero, Finset.filter_eq_empty_iff]
  intro k hk
  exact not_lt.mpr (h k hk)

theorem rankBelow_unchanged_by_removing_above (s : Finset ι) (x : ι → ℝ) (i : ι) :
    rankBelow (s.filter (fun k => x k ≤ x i)) x i = rankBelow s x i := by
  unfold rankBelow
  congr 1
  ext k
  simp only [Finset.mem_filter]
  constructor
  · rintro ⟨⟨hs, _⟩, hlt⟩
    exact ⟨hs, hlt⟩
  · rintro ⟨hs, hlt⟩
    exact ⟨⟨hs, hlt.le⟩, hlt⟩

theorem control_rank_changes_when_removing_below :
    rankBelow ({0, 1, 2} : Finset ℕ) (fun k => (k : ℝ)) 2 = 2
      ∧ rankBelow ({1, 2} : Finset ℕ) (fun k => (k : ℝ)) 2 = 1 := by
  unfold rankBelow
  have h1 : ({0, 1, 2} : Finset ℕ).filter (fun k => ((k : ℕ) : ℝ) < ((2 : ℕ) : ℝ)) = {0, 1} := by
    ext k
    simp only [Finset.mem_filter, Finset.mem_insert, Finset.mem_singleton, Nat.cast_lt]
    omega
  have h2 : ({1, 2} : Finset ℕ).filter (fun k => ((k : ℕ) : ℝ) < ((2 : ℕ) : ℝ)) = {1} := by
    ext k
    simp only [Finset.mem_filter, Finset.mem_insert, Finset.mem_singleton, Nat.cast_lt]
    omega
  simp only [h1, h2]
  decide

end Rank

section Aristocratic

noncomputable def statusRule (s j : ℝ) : ℝ := if s = j then s else 0

theorem statusRule_keep (s : ℝ) : statusRule s s = s := by simp [statusRule]

theorem statusRule_lose {s j : ℝ} (h : s ≠ j) : statusRule s j = 0 := by simp [statusRule, h]

theorem status_zero_absorbing (j : ℝ) : statusRule 0 j = 0 := by
  unfold statusRule
  split_ifs <;> rfl

theorem status_path_keep (s : ℝ) : ∀ l : List ℝ, (∀ j ∈ l, j = s) → l.foldl statusRule s = s := by
  intro l
  induction l with
  | nil => intro _; rfl
  | cons j t ih =>
    intro h
    have hj : j = s := h j List.mem_cons_self
    simp only [List.foldl_cons, hj, statusRule_keep]
    exact ih (fun k hk => h k (List.mem_cons_of_mem _ hk))

theorem status_path_lose (s : ℝ) : ∀ l : List ℝ, (∃ j ∈ l, j ≠ s) → l.foldl statusRule s = 0 := by
  intro l
  induction l generalizing s with
  | nil => rintro ⟨_, h, _⟩; simp at h
  | cons j t ih =>
    rintro ⟨k, hk, hne⟩
    simp only [List.foldl_cons]
    by_cases hj : s = j
    · subst hj
      rw [statusRule_keep]
      apply ih
      refine ⟨k, ?_, hne⟩
      rcases List.mem_cons.mp hk with h | h
      · exact absurd h hne
      · exact h
    · rw [statusRule_lose hj]
      have hz : t.foldl statusRule 0 = 0 := by
        clear ih hk hne hj
        induction t with
        | nil => rfl
        | cons a r ihr => simp only [List.foldl_cons, status_zero_absorbing]; exact ihr
      exact hz

theorem control_status_rule_needs_exact_match : statusRule 1 2 = 0 ∧ statusRule 1 2 ≠ 1 := by
  simp [statusRule]

theorem status_stream_value {β s : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) :
    ∑' t : ℕ, β ^ t * s = s / (1 - β) := by
  rw [tsum_mul_right, tsum_geometric_of_lt_one hβ0 hβ1, div_eq_mul_inv]
  exact mul_comm _ _

theorem argmax_unchanged_by_constant {α : Type*} (f : α → ℝ) (c : ℝ) (S : Set α) (a : α) :
    IsMaxOn (fun k => f k + c) S a ↔ IsMaxOn f S a := by
  simp only [isMaxOn_iff]
  constructor <;> intro h x hx <;> linarith [h x hx]

theorem aristocratic_separable {α : Type*} (W : α → ℝ) (β s : ℝ) (hβ0 : 0 ≤ β) (hβ1 : β < 1)
    (S : Set α) (a : α) :
    IsMaxOn (fun k => W k + ∑' t : ℕ, β ^ t * s) S a ↔ IsMaxOn W S a := by
  rw [status_stream_value hβ0 hβ1]
  exact argmax_unchanged_by_constant W _ S a

theorem control_separability_needs_match_independent_of_capital :
    IsMaxOn (fun k : ℝ => -(k ^ 2)) Set.univ 0
      ∧ ¬ IsMaxOn (fun k : ℝ => -(k ^ 2) + 4 * k) Set.univ 0 := by
  constructor
  · rw [isMaxOn_iff]
    intro x _
    nlinarith [sq_nonneg x]
  · rw [isMaxOn_iff]
    push_neg
    exact ⟨2, Set.mem_univ _, by norm_num⟩

theorem foc_aristocratic_is_exogenous {u' : ℝ → ℝ} {A β k k₁ k₂ V' : ℝ}
    (hfoc : u' (A * k - k₁) = β * V') (henv : V' = u' (A * k₁ - k₂) * A) :
    u' (A * k - k₁) = β * u' (A * k₁ - k₂) * A := by
  rw [hfoc, henv]
  ring

theorem wealth_is_status_foc_distorts {u' : ℝ → ℝ} {A β k k₁ k₂ m' : ℝ} (hβ : 0 < β) (hm : 0 < m')
    (hfoc : u' (A * k - k₁) = β * (A * u' (A * k₁ - k₂) + m')) :
    β * A * u' (A * k₁ - k₂) < u' (A * k - k₁) := by
  rw [hfoc]
  nlinarith

theorem no_deviation_iff {β i j V : ℝ} (hβ1 : β < 1) :
    V + j ≤ V + i / (1 - β) ↔ (1 - β) * j ≤ i := by
  have h : 0 < 1 - β := by linarith
  rw [add_le_add_iff_left, le_div_iff₀ h, mul_comm]

theorem low_status_deviates {β i j V : ℝ} (hβ1 : β < 1) (h : i < (1 - β) * j) :
    V + i / (1 - β) < V + j := by
  have hp : 0 < 1 - β := by linarith
  rw [add_lt_add_iff_left, div_lt_iff₀ hp, mul_comm]
  exact h

theorem high_status_never_deviates {β i j V : ℝ} (hβ1 : β < 1) (hj : j ≤ 1)
    (hi : 1 - β ≤ i) : V + j ≤ V + i / (1 - β) := by
  rw [no_deviation_iff hβ1]
  nlinarith

theorem control_deviation_needs_beta_lt_one :
    (1 - (2 : ℝ)) * 1 ≤ 0 ∧ ¬ ((0 : ℝ) + 1 ≤ 0 + 0 / (1 - 2)) := by
  norm_num

end Aristocratic

section Indifference

theorem lambda_star_identity {A β γ : ℝ} (hA : 0 < A) (hβ : 0 < β) (hγ : 0 < γ) :
    1 - β * ((1 - (1 - (β * A ^ (1 - γ)) ^ (1 / γ))) * A) ^ (1 - γ)
      = 1 - (β * A ^ (1 - γ)) ^ (1 / γ) := by
  have hq0 : 0 < β * A ^ (1 - γ) := mul_pos hβ (Real.rpow_pos_of_pos hA _)
  have hr : 0 < (β * A ^ (1 - γ)) ^ (1 / γ) := Real.rpow_pos_of_pos hq0 _
  have hpow : ((β * A ^ (1 - γ)) ^ (1 / γ)) ^ γ = β * A ^ (1 - γ) := by
    rw [← Real.rpow_mul hq0.le, one_div_mul_cancel hγ.ne', Real.rpow_one]
  rw [sub_sub_cancel, Real.mul_rpow hr.le hA.le]
  have key : β * (((β * A ^ (1 - γ)) ^ (1 / γ)) ^ (1 - γ) * A ^ (1 - γ))
      = (β * A ^ (1 - γ)) ^ (1 / γ) := by
    calc β * (((β * A ^ (1 - γ)) ^ (1 / γ)) ^ (1 - γ) * A ^ (1 - γ))
        = ((β * A ^ (1 - γ)) ^ (1 / γ)) ^ (1 - γ) * (β * A ^ (1 - γ)) := by ring
      _ = ((β * A ^ (1 - γ)) ^ (1 / γ)) ^ (1 - γ) * ((β * A ^ (1 - γ)) ^ (1 / γ)) ^ γ := by
          rw [hpow]
      _ = ((β * A ^ (1 - γ)) ^ (1 / γ)) ^ ((1 - γ) + γ) := by rw [← Real.rpow_add hr]
      _ = (β * A ^ (1 - γ)) ^ (1 / γ) := by rw [sub_add_cancel, Real.rpow_one]
  rw [key]

theorem value_closed_form_denominator {A β γ lam k : ℝ} (hγ : γ ≠ 1) (hA : 0 < A) (hl : 0 < lam)
    (hk : 0 < k) (hid : 1 - β * ((1 - lam) * A) ^ (1 - γ) = lam) :
    (lam * A * k) ^ (1 - γ) / ((1 - γ) * (1 - β * ((1 - lam) * A) ^ (1 - γ)))
      = lam ^ (-γ) * A ^ (1 - γ) * k ^ (1 - γ) / (1 - γ) := by
  rw [hid, Real.mul_rpow (mul_pos hl hA).le hk.le, Real.mul_rpow hl.le hA.le]
  have hsplit : lam ^ (1 - γ) = lam ^ (-γ) * lam := by
    have := Real.rpow_add hl (-γ) 1
    rw [Real.rpow_one] at this
    rw [show (1 : ℝ) - γ = -γ + 1 by ring, this]
  rw [hsplit]
  have hg : (1 : ℝ) - γ ≠ 0 := sub_ne_zero.mpr (Ne.symm hγ)
  field_simp
  ring

theorem indifference_level {C β γ j k L : ℝ} (hC : C ≠ 0) (hβ : β ≠ 1) :
    C * L ^ (1 - γ) + j = C * k ^ (1 - γ) + j / (1 - β)
      ↔ L ^ (1 - γ) = k ^ (1 - γ) + β / (1 - β) * j / C := by
  have h1 : (1 : ℝ) - β ≠ 0 := sub_ne_zero.mpr (Ne.symm hβ)
  have e : j / (1 - β) - j = β / (1 - β) * j := by
    field_simp
    ring
  constructor
  · intro h
    have h2 : C * L ^ (1 - γ) = C * k ^ (1 - γ) + β / (1 - β) * j := by linarith [e]
    have h3 : L ^ (1 - γ) = (C * k ^ (1 - γ) + β / (1 - β) * j) / C := by
      rw [← h2]
      field_simp
    rw [h3]
    field_simp
    ring
  · intro h
    rw [h]
    field_simp
    ring

theorem indifference_printed_form {A β γ j k L lam : ℝ} (hγ : γ ≠ 1) (hβ : β ≠ 1) (hA : 0 < A)
    (hl : 0 < lam)
    (h : L ^ (1 - γ) = k ^ (1 - γ)
      + β / (1 - β) * j / (lam ^ (-γ) * A ^ (1 - γ) / (1 - γ))) :
    L ^ (1 - γ) = k ^ (1 - γ) + β / (1 - β) * j * (1 - γ) * lam ^ γ * A ^ (γ - 1) := by
  rw [h]
  have hg : (1 : ℝ) - γ ≠ 0 := sub_ne_zero.mpr (Ne.symm hγ)
  have hb : (1 : ℝ) - β ≠ 0 := sub_ne_zero.mpr (Ne.symm hβ)
  have hln : lam ^ (-γ) = (lam ^ γ)⁻¹ := Real.rpow_neg hl.le γ
  have hAn : A ^ (1 - γ) = (A ^ (γ - 1))⁻¹ := by
    rw [show (1 : ℝ) - γ = -(γ - 1) by ring, Real.rpow_neg hA.le]
  have hlp : 0 < lam ^ γ := Real.rpow_pos_of_pos hl _
  have hAp : 0 < A ^ (γ - 1) := Real.rpow_pos_of_pos hA _
  rw [hln, hAn]
  field_simp
  ring

theorem L_exceeds_k {γ j k L c : ℝ} (hγ : 1 < γ) (hj : 0 < j) (hc : 0 < c) (hL : 0 < L) (h : L ^ (1 - γ) = k ^ (1 - γ) + j * (1 - γ) * c) : k < L := by
  have hneg : j * (1 - γ) * c < 0 := by
    have : 1 - γ < 0 := by linarith
    have := mul_neg_of_pos_of_neg hj this
    exact mul_neg_of_neg_of_pos this hc
  have hlt : L ^ (1 - γ) < k ^ (1 - γ) := by linarith
  by_contra hcon
  push_neg at hcon
  rcases eq_or_lt_of_le hcon with heq | hlt'
  · rw [heq] at hlt
    exact lt_irrefl _ hlt
  · have := Real.rpow_lt_rpow_of_exponent_neg hL hlt' (by linarith : 1 - γ < 0)
    linarith

theorem L_equals_k_at_zero_status {γ k L c : ℝ} (hγ : γ ≠ 1) (hk : 0 < k) (hL : 0 < L)
    (h : L ^ (1 - γ) = k ^ (1 - γ) + 0 * (1 - γ) * c) : L = k := by
  rw [zero_mul, zero_mul, add_zero] at h
  have hne : (1 : ℝ) - γ ≠ 0 := sub_ne_zero.mpr (Ne.symm hγ)
  have := congrArg (fun z => z ^ (1 / (1 - γ))) h
  simp only at this
  rw [← Real.rpow_mul hL.le, ← Real.rpow_mul hk.le, mul_one_div_cancel hne, Real.rpow_one,
    Real.rpow_one] at this
  exact this

theorem control_L_exceeds_needs_positive_status :
    ∀ k : ℝ, 0 < k → (k ^ ((1 : ℝ) - 2) = k ^ ((1 : ℝ) - 2) + 0 * (1 - 2) * 1 ∧ ¬ (k < k)) := by
  intro k _
  exact ⟨by ring, lt_irrefl k⟩

theorem spread_out_blocks_deviation {A k L k' : ℝ} (hfeas : k' ≤ A * k) (hL : A * k ≤ L) :
    k' ≤ L := hfeas.trans hL

end Indifference

section Effort

theorem cmp95_effort_closed_form {a g l : ℝ} (hfoc : a + g * a - 2 * l = 0) :
    l = a * (1 + g) / 2 := by linarith

theorem cmp95_match_consistent {a g j α : ℝ} (ha : a ^ 2 = 2 * α ^ 2 * j)
    (hg : g * (1 + g) * α ^ 2 = 1) : g * (a * (a * (1 + g) / 2)) = j := by
  have e : g * (a * (a * (1 + g) / 2)) = g * (1 + g) * a ^ 2 / 2 := by ring
  rw [e, ha]
  linear_combination j * hg

theorem cmp95_slope_falls_with_dispersion {g g' α α' : ℝ} (hg0 : 0 < g) (hg0' : 0 < g')
    (hα : 0 < α) (hα' : α < α') (h : g * (1 + g) * α ^ 2 = 1) (h' : g' * (1 + g') * α' ^ 2 = 1) :
    g' < g := by
  by_contra hc
  push_neg at hc
  have h1 : g * (1 + g) ≤ g' * (1 + g') := by nlinarith
  have h2 : α ^ 2 < α' ^ 2 := by nlinarith
  have h3 : g * (1 + g) * α ^ 2 < g' * (1 + g') * α' ^ 2 := by
    have hp : 0 < g * (1 + g) := by positivity
    nlinarith
  linarith

theorem control_slope_needs_dispersion_change {g g' α : ℝ} (hg0 : 0 < g) (hg0' : 0 < g')
    (hα : 0 < α) (h : g * (1 + g) * α ^ 2 = 1) (h' : g' * (1 + g') * α ^ 2 = 1) : ¬ (g' < g) := by
  intro hc
  have hp : 0 < α ^ 2 := by positivity
  have : g' * (1 + g') < g * (1 + g) := by nlinarith
  nlinarith

theorem bottom_effort_undistorted {a0 g : ℝ} (ha : a0 = 0) : a0 * (1 + g) / 2 = 0 := by
  rw [ha]; ring

end Effort

end ColeMailathPostlewaite
