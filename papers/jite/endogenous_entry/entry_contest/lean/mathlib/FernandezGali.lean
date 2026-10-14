import Mathlib
import KappaSpread

/-!
# Fernández and Galí, "To Each According to...? Markets, Tournaments, and the Matching
Problem with Borrowing Constraints"

NBER Working Paper 5930 (February 1997); published in the Review of Economic Studies 66(4),
1999, 799-824.  Page numbers are PDF pages of the working paper (printed page = PDF page − 1).

Setup (pp.6-7, 11-12): a continuum of agents, ability `a` and wealth `w` uniform and
independent on the unit square; a continuum of schools of quality `s`; output `X(a, s)` with
`X_as > 0`.  A market sells each school at a price `P(s)`; a tournament ranks agents by a
signal `V(a, e)` bought with a wasted expenditure `e`, and assigns schools by rank.  Utility
is consumption, `X + w − expenditure`, so it is linear.  Under borrowing constraints an
agent cannot spend more than `w`.

## What is proved

1. Single crossing (Lemma 4.3(i), p.33; Lemma 4.10(i), p.35): revealed preference and a
   strictly supermodular payoff sort higher ability into weakly better schools
   (`sorting_of_strictSupermod`); the market form, where the price cancels
   (`market_sorting`); the tournament form, where the cost of the signal has `e_va ≤ 0`
   (`tournament_sorting`).  Control: with an additive `X` the reverse sorting is consistent
   with revealed preference (`sorting_needs_strict`).
2. Theorem 2.2 (pp.8, 32): a joint c.d.f. with uniform marginals lies below `min a s`, which
   assortative matching attains (`frechet_upper`, `assortative_attains`).  Its finite
   counterpart: for a supermodular `X` on `Fin n`, assortative matching maximises output over
   all permutations (`assortative_max`).  Control: for a submodular `X` the swap does better
   (`assortative_needs_supermod`).
3. Market clearing under credit constraints, eqs. (4.4)-(4.5) (p.20):
   `1 − s = (1 − a)(1 − P)` gives `a = (s − P)/(1 − P)` (`clearing_iff`), below `s` whenever
   `0 < P < 1` and `s < 1` (`clearing_lt`).  Control: with `P = 0` the lowest ability in
   school `s` is `s` (`clearing_P0`).
4. Eq. (3.17) (p.15): with `P' = X_s` and `e' = X_s − V_a/V_e`, `V_a/V_e ≥ 0` and
   `P(0) = e(0) = 0`, the tournament expenditure never exceeds the price
   (`price_ge_expenditure`).
5. Lemma 4.2 (pp.18, 31): with `X_s < 1` and `P(0) = 0`, no school sells at price one
   (`lemma42_core`).
6. The bridge to our model: with linear utility the utility cost of a fixed fee does not
   depend on wealth (`kappa_linear`), so wealth can act only through a constraint, as in the
   paper; with concave utility it does depend on wealth (`EntryContestKappa.log_burden_strictAnti`).

## Not formalised

The continuum equilibria (Lemmas 4.3(ii)-4.17, Propositions 4.7 and 4.14), Theorem 4.18 and
the limit results in the signal power (Lemma 4.19, Theorem 4.20, Corollary 4.21), which need
the measure theory of the continuum of agents.  The record (`lit/fernandez_gali_1999`) reads
them from the paper.
-/

open MeasureTheory Set Finset

namespace FernandezGali1997

/-! ## 1. Single crossing -/

/-- Strict supermodularity of a payoff in (ability, school), the discrete form of `X_as > 0`. -/
def StrictSupermod (U : ℝ → ℝ → ℝ) : Prop :=
  ∀ a a' s s', a' < a → s < s' → U a' s' + U a s < U a s' + U a' s

/-- Revealed preference with a strictly supermodular payoff: if `a > a'`, `a` chooses `s` and
    `a'` chooses `s'`, then `s' ≤ s`. -/
theorem sorting_of_strictSupermod (U : ℝ → ℝ → ℝ) (hU : StrictSupermod U) {a a' s s' : ℝ}
    (ha : a' < a) (h1 : U a s' ≤ U a s) (h2 : U a' s ≤ U a' s') : s' ≤ s := by
  by_contra h
  push_neg at h
  have := hU a a' s s' ha h
  linarith

/-- Lemma 4.3(i): under market prices the payoff is `X(a, s) − P(s)`; the price cancels from
    the cross difference, so `X_as > 0` sorts. -/
theorem market_sorting (X : ℝ → ℝ → ℝ) (P : ℝ → ℝ) (hX : StrictSupermod X)
    {a a' s s' : ℝ} (ha : a' < a) (h1 : X a s' - P s' ≤ X a s - P s)
    (h2 : X a' s - P s ≤ X a' s' - P s') : s' ≤ s := by
  refine sorting_of_strictSupermod (fun a s => X a s - P s) ?_ ha h1 h2
  intro a a' s s' ha hs
  have := hX a a' s s' ha hs
  simp only
  linarith

/-- Lemma 4.10(i): in the tournament the payoff is `X(a, s) − C(a, s)`, with `C(a, s)` the
    expenditure ability `a` needs for school `s`.  `e_va ≤ 0` makes the extra cost of a better
    school no larger for higher ability, and with `X_as > 0` that sorts. -/
theorem tournament_sorting (X C : ℝ → ℝ → ℝ) (hX : StrictSupermod X)
    (hC : ∀ a a' s s', a' < a → s < s' → C a s' - C a s ≤ C a' s' - C a' s)
    {a a' s s' : ℝ} (ha : a' < a) (h1 : X a s' - C a s' ≤ X a s - C a s)
    (h2 : X a' s - C a' s ≤ X a' s' - C a' s') : s' ≤ s := by
  refine sorting_of_strictSupermod (fun a s => X a s - C a s) ?_ ha h1 h2
  intro a a' s s' ha hs
  have := hX a a' s s' ha hs
  have := hC a a' s s' ha hs
  simp only
  linarith

/-- Control: with `X(a, s) = a + s` (no complementarity) and `P(s) = s`, the higher ability at
    the worse school is consistent with revealed preference. -/
theorem sorting_needs_strict :
    let X : ℝ → ℝ → ℝ := fun a s => a + s
    let P : ℝ → ℝ := fun s => s
    X 1 1 - P 1 ≤ X 1 0 - P 0 ∧ X 0 0 - P 0 ≤ X 0 1 - P 1 ∧ (0 : ℝ) < 1 := by
  norm_num

/-! ## 2. Theorem 2.2: assortative matching is efficient -/

/-- The step of Theorem 2.2's proof (p.32): a joint c.d.f. on the unit square, monotone in each
    argument, with uniform marginals, lies below `min a s`. -/
theorem frechet_upper (Φ : ℝ → ℝ → ℝ) (hmono_a : ∀ s, MonotoneOn (fun a => Φ a s) (Icc 0 1))
    (hmono_s : ∀ a, MonotoneOn (Φ a) (Icc 0 1)) (hma : ∀ a, Φ a 1 = a) (hms : ∀ s, Φ 1 s = s)
    {a s : ℝ} (ha : a ∈ Icc (0 : ℝ) 1) (hs : s ∈ Icc (0 : ℝ) 1) : Φ a s ≤ min a s := by
  have h1 : Φ a s ≤ Φ a 1 := hmono_s a hs ⟨by norm_num, le_rfl⟩ hs.2
  have h2 : Φ a s ≤ Φ 1 s := hmono_a s ha ⟨by norm_num, le_rfl⟩ ha.2
  rw [hma] at h1
  rw [hms] at h2
  exact le_min h1 h2

/-- Assortative matching, `S*(a, w) = a`, has joint c.d.f. `min a s`, which meets the marginals. -/
theorem assortative_attains {a s : ℝ} (ha : a ≤ 1) (hs : s ≤ 1) :
    min a 1 = a ∧ min 1 s = s :=
  ⟨min_eq_left ha, min_eq_right hs⟩

/-- Supermodularity on a finite grid, the discrete form of `X_as ≥ 0`. -/
def Supermod {n : ℕ} (X : Fin n → Fin n → ℝ) : Prop :=
  ∀ i j k l : Fin n, i < j → k < l → X i l + X j k ≤ X i k + X j l

/-- Output of the matching `σ` (agent `i` to school `σ i`). -/
def output {n : ℕ} (X : Fin n → Fin n → ℝ) (σ : Equiv.Perm (Fin n)) : ℝ := ∑ i, X i (σ i)

/-- Swapping the images of `j` and `k` changes only two terms. -/
theorem output_swap {n : ℕ} (X : Fin n → Fin n → ℝ) (σ : Equiv.Perm (Fin n)) {j k : Fin n}
    (hjk : j ≠ k) :
    output X (σ * Equiv.swap j k) - output X σ
      = X j (σ k) + X k (σ j) - X j (σ j) - X k (σ k) := by
  unfold output
  have hk : k ∈ (univ : Finset (Fin n)).erase j := mem_erase.mpr ⟨hjk.symm, mem_univ _⟩
  rw [← add_sum_erase _ _ (mem_univ j), ← add_sum_erase _ _ hk,
    ← add_sum_erase _ (fun i => X i (σ i)) (mem_univ j), ← add_sum_erase _ _ hk]
  have hrest : ∑ x ∈ ((univ : Finset (Fin n)).erase j).erase k, X x ((σ * Equiv.swap j k) x)
      = ∑ x ∈ ((univ : Finset (Fin n)).erase j).erase k, X x (σ x) := by
    refine sum_congr rfl fun x hx => ?_
    have hxk := ne_of_mem_erase hx
    have hxj := ne_of_mem_erase (mem_of_mem_erase hx)
    rw [Equiv.Perm.mul_apply, Equiv.swap_apply_of_ne_of_ne hxj hxk]
  rw [hrest, Equiv.Perm.mul_apply, Equiv.Perm.mul_apply, Equiv.swap_apply_left,
    Equiv.swap_apply_right]
  ring

/-- The finite counterpart of Theorem 2.2: with supermodular output, assortative matching (the
    identity) gives at least the output of every matching.  The proof moves, from the top school
    down, the agent at each school into the school of her rank; each move is one swap, which
    supermodularity makes weakly profitable. -/
theorem assortative_max {n : ℕ} (X : Fin n → Fin n → ℝ) (hX : Supermod X)
    (σ : Equiv.Perm (Fin n)) : output X σ ≤ output X 1 := by
  suffices H : ∀ m : ℕ, ∀ σ : Equiv.Perm (Fin n), (∀ i : Fin n, m ≤ i.val → σ i = i) →
      output X σ ≤ output X 1 by
    exact H n σ fun i hi => absurd i.isLt (not_lt.mpr hi)
  intro m
  induction m with
  | zero =>
    intro σ hσ
    have : σ = 1 := Equiv.ext fun i => hσ i (Nat.zero_le _)
    rw [this]
  | succ m ih =>
    intro σ hσ
    by_cases hm : m < n
    · set k : Fin n := ⟨m, hm⟩ with hkdef
      set j := σ.symm k with hjdef
      have hσj : σ j = k := by simp [hjdef]
      -- `j ≤ k`: an agent above `k` is fixed, so it is not sent to `k`
      have hjle : j.val ≤ m := by
        by_contra h
        push_neg at h
        have := hσ j h
        rw [hσj] at this
        have : (k : Fin n).val = j.val := by rw [this]
        simp [hkdef] at this
        omega
      -- the new matching fixes every agent from `k` up
      have hτ : ∀ i : Fin n, m ≤ i.val → (σ * Equiv.swap j k) i = i := by
        intro i hi
        rw [Equiv.Perm.mul_apply]
        rcases eq_or_ne i k with hik | hik
        · rw [hik, Equiv.swap_apply_right, hσj]
        · have him : m + 1 ≤ i.val := by
            rcases Nat.lt_or_ge m i.val with h | h
            · omega
            · exact absurd (Fin.ext (by simp [hkdef]; omega)) hik
          have hij : i ≠ j := by
            intro hij
            rw [hij] at him
            omega
          rw [Equiv.swap_apply_of_ne_of_ne hij hik]
          exact hσ i him
      have hstep := ih _ hτ
      rcases eq_or_ne j k with hjk | hjk
      · have : σ * Equiv.swap j k = σ := by rw [hjk, Equiv.swap_self]; rfl
        rw [this] at hstep
        exact hstep
      · -- `σ k < k`: the school of `k` is below `k`
        have hjlt : j < k := by
          rw [Fin.lt_iff_val_lt_val]
          have : j.val ≠ m := fun h => hjk (Fin.ext (by simp [hkdef, h]))
          simp [hkdef]
          omega
        have hlk : σ k < k := by
          rw [Fin.lt_iff_val_lt_val]
          by_contra h
          push_neg at h
          have hkm : k.val = m := rfl
          rcases eq_or_lt_of_le h with h | h
          · have : σ k = k := Fin.ext (by omega)
            exact hjk (σ.injective (by rw [hσj, this]))
          · have hfix := hσ (σ k) (by omega)
            have : σ k = k := σ.injective hfix
            rw [this] at h
            exact lt_irrefl _ h
        have hsw := output_swap X σ hjk
        have hsup := hX j k (σ k) k hjlt hlk
        rw [hσj] at hsw
        linarith
    · exact ih σ fun i hi => hσ i (by have := i.isLt; omega)

/-- Control: with a submodular `X(i, k) = −i k` on two agents the swap beats assortative
    matching, so supermodularity is needed. -/
theorem assortative_needs_supermod :
    output (fun i k : Fin 2 => -((i.val : ℝ) * k.val)) 1
      < output (fun i k : Fin 2 => -((i.val : ℝ) * k.val)) (Equiv.swap 0 1) := by
  simp [output, Fin.sum_univ_two, Equiv.swap_apply_left, Equiv.swap_apply_right]

/-! ## 3. Market clearing under credit constraints, eqs. (4.4)-(4.5) -/

/-- Eq. (4.4): `1 − s = (1 − a(s))(1 − P(s))` says `a(s) = (s − P)/(1 − P)`. -/
theorem clearing_iff {s P a : ℝ} (hP : P ≠ 1) :
    1 - s = (1 - a) * (1 - P) ↔ a = (s - P) / (1 - P) := by
  have h : (1 : ℝ) - P ≠ 0 := sub_ne_zero.mpr (Ne.symm hP)
  rw [eq_div_iff h]
  constructor <;> intro h' <;> linarith

/-- Eq. (4.5): with a positive price below one, the lowest ability in school `s < 1` is below
    `s`, so the market allocation under credit constraints is not assortative. -/
theorem clearing_lt {s P : ℝ} (hs : s < 1) (hP0 : 0 < P) (hP1 : P < 1) :
    (s - P) / (1 - P) < s := by
  rw [div_lt_iff₀ (by linarith)]
  nlinarith

/-- Control: at a zero price the lowest ability in school `s` is `s` itself. -/
theorem clearing_P0 (s : ℝ) : (s - 0) / (1 - 0) = s := by norm_num

/-! ## 4. Eq. (3.17): the price is never below the tournament expenditure -/

/-- With `P(s) = ∫₀ˢ X_s` (eq. 3.4) and `e(s) = ∫₀ˢ (X_s − V_a/V_e)` (eq. 3.12),
    `P(s) − e(s) = ∫₀ˢ V_a/V_e ≥ 0`. -/
theorem price_ge_expenditure (f g : ℝ → ℝ) {s : ℝ} (hs : 0 ≤ s)
    (hf : IntervalIntegrable f volume 0 s) (hg : IntervalIntegrable g volume 0 s)
    (hg0 : ∀ z, 0 ≤ g z) :
    ∫ z in (0 : ℝ)..s, (f z - g z) ≤ ∫ z in (0 : ℝ)..s, f z := by
  rw [intervalIntegral.integral_sub hf hg]
  have : 0 ≤ ∫ z in (0 : ℝ)..s, g z := intervalIntegral.integral_nonneg hs fun z _ => hg0 z
  linarith

/-! ## 5. Lemma 4.2: no school sells at price one -/

/-- The inequality of Lemma 4.2's proof: if `X(a, ·)` has slope below one then
    `X(a, s) − 1 < X(a, 0)` for `0 < s ≤ 1`, so paying one for school `s` is worse than school
    `0` at price `P(0) = 0`. -/
theorem lemma42_core (Xa Xa' : ℝ → ℝ) (hX : ∀ x, HasDerivAt Xa (Xa' x) x)
    (hX' : ∀ x, Xa' x < 1) {s : ℝ} (hs : 0 < s) (hs1 : s ≤ 1) : Xa s - 1 < Xa 0 := by
  obtain ⟨c, _, hc⟩ := exists_hasDerivAt_eq_slope Xa Xa' hs
    (fun x _ => (hX x).continuousAt.continuousWithinAt) (fun x _ => hX x)
  have h1 := hX' c
  rw [hc, div_lt_one (by linarith)] at h1
  linarith

/-! ## 6. The bridge to our model -/

/-- With linear utility, the paper's case, the utility cost of a fixed fee does not depend on
    wealth: wealth can then matter only through a constraint on spending. -/
theorem kappa_linear (α β c w : ℝ) :
    EntryContestKappa.kappa (fun x => α * x + β) c w = α * c := by
  unfold EntryContestKappa.kappa
  ring

/-- Control: with log utility the utility cost of the fee falls strictly with wealth, which is
    the channel of our model. -/
theorem kappa_log_not_const : ¬ (EntryContestKappa.kappa Real.log 1 2 =
    EntryContestKappa.kappa Real.log 1 3) := by
  intro h
  have := EntryContestKappa.log_burden_strictAnti 1 one_pos
    (show (2 : ℝ) ∈ Set.Ioi 1 by norm_num) (show (3 : ℝ) ∈ Set.Ioi 1 by norm_num) (by norm_num)
  linarith

end FernandezGali1997
