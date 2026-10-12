import IscLean.Unravelling
import Mathlib.Analysis.SpecialFunctions.Pow.Real

open Finset

namespace Isc

variable {ι : Type*} [DecidableEq ι]

theorem poolEU_id_avg {P : Finset ι} (h2 : 2 ≤ P.card) {i : ι} (hi : i ∈ P) (x : ι → ℝ)
    (L : ℝ) : poolEU P x L id i = ((∑ k ∈ P, x k) - x i) / ((P.card : ℝ) - 1) - L := by
  have hpos : (P.card : ℝ) - 1 ≠ 0 := by
    have : (2 : ℝ) ≤ (P.card : ℝ) := by exact_mod_cast h2
    linarith
  rw [poolEU_id h2 hi, poolGain_closed hi]
  field_simp
  ring

theorem poolEU_sub {P : Finset ι} {i j : ι} (hi : i ∈ P) (hj : j ∈ P) (x : ι → ℝ) (L : ℝ)
    (u : ℝ → ℝ) :
    poolEU P x L u j - poolEU P x L u i = (u (x i - L) - u (x j - L)) / ((P.card : ℝ) - 1) := by
  have ei := add_sum_erase P (fun k => u (x k - L)) hi
  have ej := add_sum_erase P (fun k => u (x k - L)) hj
  simp only at ei ej
  unfold poolEU
  rw [← sub_div]
  congr 1
  linarith

theorem lottery_value_antitone {P : Finset ι} (h2 : 2 ≤ P.card) {i j : ι} (hi : i ∈ P)
    (hj : j ∈ P) (x : ι → ℝ) (L : ℝ) {u : ℝ → ℝ} {S : Set ℝ} (hu : MonotoneOn u S)
    (hSi : x i - L ∈ S) (hSj : x j - L ∈ S) (hij : x i ≤ x j) :
    poolEU P x L u j ≤ poolEU P x L u i := by
  have hpos : (0 : ℝ) < (P.card : ℝ) - 1 := by
    have : (2 : ℝ) ≤ (P.card : ℝ) := by exact_mod_cast h2
    linarith
  have hd := poolEU_sub hi hj x L u
  have hle : u (x i - L) ≤ u (x j - L) := hu hSi hSj (by linarith)
  have hq : (u (x i - L) - u (x j - L)) / ((P.card : ℝ) - 1) ≤ 0 :=
    div_nonpos_of_nonpos_of_nonneg (by linarith) hpos.le
  linarith

theorem lottery_sorting {P : Finset ι} (h2 : 2 ≤ P.card) {i j : ι} (hi : i ∈ P) (hj : j ∈ P)
    (x : ι → ℝ) (L d : ℝ) {u : ℝ → ℝ} {S : Set ℝ} (hu : MonotoneOn u S)
    (hSi : x i - L ∈ S) (hSj : x j - L ∈ S) (hij : x i ≤ x j)
    (hjl : u d ≤ poolEU P x L u j) : u d ≤ poolEU P x L u i :=
  le_trans hjl (lottery_value_antitone h2 hi hj x L hu hSi hSj hij)

theorem displaced_stable_iff_richest {P : Finset ι} (h2 : 2 ≤ P.card) (x : ι → ℝ) (L d : ℝ)
    {u : ℝ → ℝ} {S : Set ℝ} (hu : MonotoneOn u S) (hS : ∀ k ∈ P, x k - L ∈ S) {r : ι}
    (hr : r ∈ P) (hmax : ∀ k ∈ P, x k ≤ x r) :
    (∀ i ∈ P, u d ≤ poolEU P x L u i) ↔ u d ≤ poolEU P x L u r := by
  constructor
  · intro h
    exact h r hr
  · intro h i hi
    exact lottery_sorting h2 hi hr x L d hu (hS i hi) (hS r hr) (hmax i hi) h

theorem displaced_prefer_lottery {P : Finset ι} (h2 : 2 ≤ P.card) (x : ι → ℝ) {L d : ℝ}
    {u : ℝ → ℝ} {S : Set ℝ} (hu : MonotoneOn u S) (hd : d ∈ S) (hS : ∀ k ∈ P, x k - L ∈ S)
    (hL : ∀ k ∈ P, d ≤ x k - L) : ∀ i ∈ P, u d ≤ poolEU P x L u i := by
  intro i hi
  have hpos : (0 : ℝ) < (P.card : ℝ) - 1 := by
    have : (2 : ℝ) ≤ (P.card : ℝ) := by exact_mod_cast h2
    linarith
  unfold poolEU
  rw [le_div_iff₀ hpos]
  calc u d * ((P.card : ℝ) - 1) = ∑ _k ∈ P.erase i, u d := by
        rw [sum_const, nsmul_eq_mul, card_erase_real hi]
        ring
    _ ≤ ∑ k ∈ P.erase i, u (x k - L) :=
        sum_le_sum (fun k hk => hu hd (hS k (mem_of_mem_erase hk)) (hL k (mem_of_mem_erase hk)))

theorem return_beats_downgrade_iff {D b : ℝ} (hD : 0 < D) (hb : 1 < b) : D ^ b < D ↔ D < 1 := by
  constructor
  · intro h
    by_contra hc
    push_neg at hc
    have h1 : D ^ (1 : ℝ) ≤ D ^ b := Real.rpow_le_rpow_of_exponent_le hc hb.le
    rw [Real.rpow_one] at h1
    linarith
  · intro h
    have h1 : D ^ b < D ^ (1 : ℝ) := Real.rpow_lt_rpow_of_exponent_gt hD h hb
    rwa [Real.rpow_one] at h1

theorem return_vs_downgrade {xi x0 b : ℝ} (h : x0 < xi) (hb : 1 < b) :
    x0 < xi - (xi - x0) ^ b ↔ xi - x0 < 1 := by
  have e := return_beats_downgrade_iff (sub_pos.mpr h) hb
  constructor
  · intro h'
    exact e.mp (by linarith)
  · intro h'
    linarith [e.mpr h']

theorem control_displaced_needs_cost_bound :
    poolEU ({0, 1} : Finset ℕ) (fun k => if k = 0 then (2 : ℝ) else 3) 2 id 1 < 1 := by
  norm_num [poolEU]

theorem control_sorting_needs_monotone :
    poolEU ({0, 1} : Finset ℕ) (fun k => if k = 0 then (2 : ℝ) else 3) 0 (fun t => -t) 0
      < poolEU ({0, 1} : Finset ℕ) (fun k => if k = 0 then (2 : ℝ) else 3) 0 (fun t => -t) 1 := by
  norm_num [poolEU]

theorem control_return_depends_on_income_unit :
    (1 / 2 : ℝ) ^ (2 : ℝ) < 1 / 2 ∧ ¬ ((2 : ℝ) ^ (2 : ℝ) < 2) := by
  rw [Real.rpow_two, Real.rpow_two]
  norm_num

end Isc
