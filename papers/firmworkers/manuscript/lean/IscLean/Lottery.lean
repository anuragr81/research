import Mathlib.Algebra.BigOperators.Fin
import Mathlib.Algebra.BigOperators.Field
import Mathlib.Algebra.Order.Field.Basic
import Mathlib.Data.Real.Basic
import Mathlib.Tactic.Linarith
import Mathlib.Tactic.Positivity
import Mathlib.Tactic.Ring

open Finset

namespace Isc

theorem swap_preserves_total {N : ℕ} (x : Fin N → ℝ) (i k : Fin N) :
    ∑ j, x (Equiv.swap i k j) = ∑ j, x j :=
  Equiv.sum_comp (Equiv.swap i k) x

theorem sum_others_diff {N : ℕ} (x : Fin N → ℝ) (i : Fin N) :
    ∑ k ∈ univ.erase i, (x k - x i) = (∑ k, x k) - (N : ℝ) * x i := by
  have hN : 1 ≤ N := Nat.one_le_iff_ne_zero.mpr (fun h => by subst h; exact i.elim0)
  have hS : ∑ k ∈ univ.erase i, x k = (∑ k, x k) - x i := by
    rw [← add_sum_erase univ x (mem_univ i)]
    ring
  rw [sum_sub_distrib, hS, sum_const, card_erase_of_mem (mem_univ i), card_univ,
    Fintype.card_fin, nsmul_eq_mul, Nat.cast_sub hN]
  push_cast
  ring

noncomputable def lotteryGain {N : ℕ} (x : Fin N → ℝ) (L : ℝ) (i : Fin N) : ℝ :=
  (∑ k ∈ univ.erase i, (x k - x i)) / ((N : ℝ) - 1) - L

theorem lotteryGain_closed {N : ℕ} (x : Fin N → ℝ) (L : ℝ) (i : Fin N) :
    lotteryGain x L i = ((∑ k, x k) - (N : ℝ) * x i) / ((N : ℝ) - 1) - L := by
  rw [lotteryGain, sum_others_diff]

theorem lotteryGain_total {N : ℕ} (hN : 2 ≤ N) (x : Fin N → ℝ) (L : ℝ) :
    ∑ i, lotteryGain x L i = -((N : ℝ) * L) := by
  have hpos : (0 : ℝ) < (N : ℝ) - 1 := by
    have : (2 : ℝ) ≤ (N : ℝ) := by exact_mod_cast hN
    linarith
  simp_rw [lotteryGain_closed]
  rw [sum_sub_distrib, ← sum_div, sum_sub_distrib, ← mul_sum, sum_const, sum_const,
    card_univ, Fintype.card_fin, nsmul_eq_mul, nsmul_eq_mul]
  rw [sub_self, zero_div]
  ring

theorem lotteryGain_pos_iff {N : ℕ} (hN : 2 ≤ N) (x : Fin N → ℝ) (L : ℝ) (i : Fin N) :
    0 < lotteryGain x L i ↔ x i < ((∑ k, x k) - ((N : ℝ) - 1) * L) / (N : ℝ) := by
  have hN2 : (2 : ℝ) ≤ (N : ℝ) := by exact_mod_cast hN
  have hpos : (0 : ℝ) < (N : ℝ) - 1 := by linarith
  have hNpos : (0 : ℝ) < (N : ℝ) := by linarith
  rw [lotteryGain_closed, sub_pos, lt_div_iff₀ hpos, lt_div_iff₀ hNpos]
  constructor <;> intro h <;> linarith

theorem taylor_expectation {ι : Type*} (s : Finset ι) (w e : ι → ℝ)
    (hw : ∑ j ∈ s, w j = 1) (h1 : ∑ j ∈ s, w j * e j = 0) (h2 : ∑ j ∈ s, w j * e j ^ 2 = 1)
    (u0 u1 u2 σ : ℝ) :
    ∑ j ∈ s, w j * (u0 + u1 * (σ * e j) + (1 / 2) * u2 * (σ * e j) ^ 2)
      = u0 + (1 / 2) * u2 * σ ^ 2 := by
  have hexp : ∀ j ∈ s, w j * (u0 + u1 * (σ * e j) + (1 / 2) * u2 * (σ * e j) ^ 2)
      = u0 * w j + (u1 * σ) * (w j * e j) + ((1 / 2) * u2 * σ ^ 2) * (w j * e j ^ 2) := by
    intro j _
    ring
  rw [sum_congr rfl hexp, sum_add_distrib, sum_add_distrib, ← mul_sum, ← mul_sum, ← mul_sum,
    hw, h1, h2]
  ring

theorem premium_pos_iff {u2 σ : ℝ} (hσ : σ ≠ 0) : 0 < (1 / 2) * u2 * σ ^ 2 ↔ 0 < u2 := by
  have hs : 0 < σ ^ 2 := by positivity
  constructor
  · intro h
    by_contra hu
    push_neg at hu
    have : (1 / 2) * u2 * σ ^ 2 ≤ 0 := by
      have : (1 / 2) * u2 ≤ 0 := by linarith
      exact mul_nonpos_of_nonpos_of_nonneg this hs.le
    linarith
  · intro h
    positivity

theorem premium_scales_with_income (u2 s m lam : ℝ) :
    (1 / 2) * u2 * (s * (1 - lam) * m) ^ 2 = ((1 / 2) * u2 * s ^ 2 * m ^ 2) * (1 - lam) ^ 2 := by
  ring

end Isc
