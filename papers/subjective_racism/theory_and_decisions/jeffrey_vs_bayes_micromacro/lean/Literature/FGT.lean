/-
# Foster, Greer & Thorbecke (1984), "A Class of Decomposable Poverty Measures"

*Econometrica* 52(3), 761-766.

Formalization of the paper's own definitions and Propositions 1-2, not of
Paper B's claims.

## The paper's setup (pp. 761-763)

Incomes `y = (y_1, ..., y_n)`, poverty line `z > 0`, shortfall `g_i = z - y_i`,
`q` the number of poor households, those with income "no greater than `z`"
(p. 761).  Headcount ratio `H = q/n`, income-gap ratio `I = Σ_{i≤q} g_i/(qz)`
(p. 762).

**(3), p. 763.** For each `α ≥ 0`, `P_α(y; z) = (1/n) Σ_{i=1}^{q} (g_i/z)^α`.
"The measure `P_0` is simply the headcount ratio `H`, while `P_1` is `H·I`, a
renormalization of the income-gap measure.  The measure `P` is obtained by setting
`α = 2`."

**(2), p. 762.** `P = P_2 = H[I² + (1-I)² C_p²]`, `C_p²` the squared coefficient of
variation among the poor.

**Proposition 1 (p. 763).** `P_α` satisfies the Monotonicity Axiom for `α > 0`, the
Transfer Axiom for `α > 1`, and the Transfer Sensitivity Axiom for `α > 2`.

**Proposition 2 (p. 764), (4).** For a population split into subgroups `y^(j)` of
sizes `n_j`, `P_α(y; z) = Σ_j (n_j/n) P_α(y^(j); z)`: additive decomposability with
population-share weights; subgroup monotonicity follows.

## What is formalized

A population is a `Finset ι` of households with incomes `y : ι → ℝ`; household `i`
contributes `((z - y_i)/z)^α` (real power) if `y_i ≤ z` and `0` otherwise.

* `P0_eq_H` — `P_0 = H` (p. 763).  With the paper's weak inequality, a household
  exactly at the line counts in `P_0` (since `0^0 = 1`) and contributes `0` for
  `α > 0`.
* `P1_eq_H_mul_I` — `P_1 = H·I` (p. 763).
* `P1_eq_popMean_normGap` — `P_1` is the *whole-population* mean of the normalised
  shortfall `max(z - y_i, 0)/z`, the non-poor contributing `0`.
* `I_eq_meanGapPoor_div_z`, `P1_eq_H_mul_meanGapPoor` — the mean shortfall *among
  the poor*, `(1/q) Σ g_i`, is `z·I`; it equals `P_1` only after multiplication by
  `H` and division by `z`.
* `I_not_decomposable` — the income-gap ratio `I` itself is *not* additively
  decomposable with population-share weights (explicit counterexample), while
  `P_1` is on the same data (`P1_decomposes_on_example`).
* `P2_formula` — equation (2), `P_2 = H[I² + (1-I)² C_p²]`.
* `P_decomp` — **Proposition 2**, equation (4), for any finite partition.
* `subgroup_monotone` — the Subgroup Monotonicity Axiom (p. 763), for a two-way
  split, as a consequence of decomposability.
* `P_monotone` — **Proposition 1**, Monotonicity Axiom for `α > 0`.
* `P0_not_monotone` — the Monotonicity Axiom fails at `α = 0`: the headcount
  ignores depth.
* `P_transfer` — **Proposition 1**, Transfer Axiom for `α > 1`, in the paper's
  case (i) (transfer from a poor household to a richer poor household that stays
  poor), via strict convexity of `x ↦ x^α` (the paper's own argument).
* `P_transfer_to_nonpoor` — case (ii) of the paper's proof (recipient at or above
  the line), valid for `α > 0`.
* `P1_transfer_neutral` — the Transfer Axiom fails at `α = 1` (a case-(i)
  transfer leaves `P_1` unchanged while raising `P_2`).

Not formalized: the Transfer Sensitivity Axiom for `α > 2` (the paper cites Kolm
[11, p. 88] rather than proving it), the Rawlsian limit `α → ∞`, the relation of
`C²` to `P`, and the Section 4 Nairobi data (checked numerically in the sympy
script instead).
-/
import Mathlib

namespace Literature.FGT

open Finset

variable {ι : Type*}

/-! ## Definitions -/

/-- Household contribution: `((z - y)/z)^α` if `y ≤ z` (poor), else `0`. -/
noncomputable def contrib (α z y : ℝ) : ℝ := if y ≤ z then ((z - y) / z) ^ α else 0

/-- **(3), p. 763**: `P_α(y; z) = (1/n) Σ_{i poor} (g_i/z)^α`. -/
noncomputable def P (α z : ℝ) (s : Finset ι) (y : ι → ℝ) : ℝ :=
  (∑ i ∈ s, contrib α z (y i)) / s.card

/-- The poor: households with income no greater than `z` (p. 761). -/
noncomputable def poor (z : ℝ) (s : Finset ι) (y : ι → ℝ) : Finset ι :=
  s.filter (fun i => y i ≤ z)

/-- Headcount ratio `H = q/n` (p. 762). -/
noncomputable def H (z : ℝ) (s : Finset ι) (y : ι → ℝ) : ℝ :=
  (poor z s y).card / s.card

/-- Income-gap ratio `I = Σ_{poor} g_i/(qz)` (p. 762). -/
noncomputable def I (z : ℝ) (s : Finset ι) (y : ι → ℝ) : ℝ :=
  (∑ i ∈ poor z s y, (z - y i)) / ((poor z s y).card * z)

/-- The mean shortfall among the poor, `(1/q) Σ_{poor} g_i`. -/
noncomputable def meanGapPoor (z : ℝ) (s : Finset ι) (y : ι → ℝ) : ℝ :=
  (∑ i ∈ poor z s y, (z - y i)) / (poor z s y).card

/-! ## `P_0 = H`, `P_1 = H·I` -/

theorem sum_contrib (α z : ℝ) (s : Finset ι) (y : ι → ℝ) :
    ∑ i ∈ s, contrib α z (y i) = ∑ i ∈ poor z s y, ((z - y i) / z) ^ α := by
  simp only [contrib, poor, Finset.sum_filter]

/-- **p. 763**: "The measure `P_0` is simply the headcount ratio `H`." -/
theorem P0_eq_H (z : ℝ) (s : Finset ι) (y : ι → ℝ) : P 0 z s y = H z s y := by
  rw [P, H, sum_contrib]
  simp

/-- **p. 763**: "`P_1` is `H · I`". -/
theorem P1_eq_H_mul_I {z : ℝ} (hz : 0 < z) (s : Finset ι) (y : ι → ℝ) :
    P 1 z s y = H z s y * I z s y := by
  rw [P, H, I, sum_contrib]
  simp only [Real.rpow_one, ← Finset.sum_div]
  by_cases hq : ((poor z s y).card : ℝ) = 0
  · have : poor z s y = ∅ := by exact_mod_cast Finset.card_eq_zero.1 (by exact_mod_cast hq)
    simp [this]
  · field_simp

/-- `P_1` is the whole-population average of the normalised shortfall, with the
non-poor contributing zero. -/
theorem P1_eq_popMean_normGap (z : ℝ) (s : Finset ι) (y : ι → ℝ) :
    P 1 z s y = (∑ i ∈ s, max (z - y i) 0 / z) / s.card := by
  rw [P]
  congr 1
  refine Finset.sum_congr rfl fun i _ => ?_
  rw [contrib, Real.rpow_one]
  split_ifs with h
  · rw [max_eq_left (by linarith)]
  · rw [max_eq_right (by linarith), zero_div]

/-- The income-gap ratio is the mean shortfall among the poor divided by `z`. -/
theorem I_eq_meanGapPoor_div_z (z : ℝ) (s : Finset ι) (y : ι → ℝ) :
    I z s y = meanGapPoor z s y / z := by
  rw [I, meanGapPoor, div_div]

/-- `P_1 = H · (mean shortfall among the poor) / z`. -/
theorem P1_eq_H_mul_meanGapPoor {z : ℝ} (hz : 0 < z) (s : Finset ι) (y : ι → ℝ) :
    P 1 z s y = H z s y * (meanGapPoor z s y / z) := by
  rw [P1_eq_H_mul_I hz, I_eq_meanGapPoor_div_z]

/-! ## Equation (2): `P_2 = H[I² + (1-I)² C_p²]` -/

/-- Mean income of the poor, `ȳ_p`. -/
noncomputable def meanPoor (z : ℝ) (s : Finset ι) (y : ι → ℝ) : ℝ :=
  (∑ i ∈ poor z s y, y i) / (poor z s y).card

/-- Squared coefficient of variation among the poor,
`C_p² = Σ_{poor} (ȳ_p - y_i)²/(q ȳ_p²)` (p. 762). -/
noncomputable def Cp2 (z : ℝ) (s : Finset ι) (y : ι → ℝ) : ℝ :=
  (∑ i ∈ poor z s y, (meanPoor z s y - y i) ^ 2) /
    ((poor z s y).card * meanPoor z s y ^ 2)

/-- The algebra behind (2), in the sums `q = #poor`, `S = Σ_poor y_i`,
`S₂ = Σ_poor y_i²`. -/
theorem P2_alg {n q z S S₂ : ℝ} (hn : n ≠ 0) (hq : q ≠ 0) (hz : z ≠ 0) (hS : S ≠ 0) :
    (q * z ^ 2 - 2 * z * S + S₂) / z ^ 2 / n =
      q / n * (((q * z - S) / (q * z)) ^ 2 + (1 - (q * z - S) / (q * z)) ^ 2 *
        ((q * (S / q) ^ 2 - 2 * (S / q) * S + S₂) / (q * (S / q) ^ 2))) := by
  field_simp
  ring

/-- **Equation (2), p. 762**: `P = P_2 = H[I² + (1-I)² C_p²]`, when there is at
least one poor household and the poor have nonzero mean income. -/
theorem P2_formula {z : ℝ} (hz : 0 < z) (s : Finset ι) (y : ι → ℝ)
    (hq : (poor z s y).Nonempty) (hμ : meanPoor z s y ≠ 0) :
    P 2 z s y = H z s y * (I z s y ^ 2 + (1 - I z s y) ^ 2 * Cp2 z s y) := by
  have hqpos : (0 : ℝ) < (poor z s y).card := by exact_mod_cast hq.card_pos
  have hn : (0 : ℝ) < s.card := by
    exact_mod_cast lt_of_lt_of_le hq.card_pos (Finset.card_filter_le _ _)
  have e1 : ∑ i ∈ poor z s y, ((z - y i) / z) ^ (2 : ℝ) =
      (((poor z s y).card : ℝ) * z ^ 2 - 2 * z * ∑ i ∈ poor z s y, y i +
        ∑ i ∈ poor z s y, y i ^ 2) / z ^ 2 := by
    simp only [Real.rpow_two, div_pow, ← Finset.sum_div, sub_sq, Finset.sum_add_distrib,
      Finset.sum_sub_distrib, Finset.sum_const, nsmul_eq_mul, ← Finset.mul_sum]
  have e2 : ∑ i ∈ poor z s y, (z - y i) =
      ((poor z s y).card : ℝ) * z - ∑ i ∈ poor z s y, y i := by
    rw [Finset.sum_sub_distrib, Finset.sum_const, nsmul_eq_mul]
  have e3 : ∀ m : ℝ, ∑ i ∈ poor z s y, (m - y i) ^ 2 =
      ((poor z s y).card : ℝ) * m ^ 2 - 2 * m * ∑ i ∈ poor z s y, y i +
        ∑ i ∈ poor z s y, y i ^ 2 := fun m => by
    simp only [sub_sq, Finset.sum_add_distrib, Finset.sum_sub_distrib, Finset.sum_const,
      nsmul_eq_mul, ← Finset.mul_sum]
  have hS : ∑ i ∈ poor z s y, y i ≠ 0 := by
    intro h; apply hμ; rw [meanPoor, h, zero_div]
  rw [P, sum_contrib, H, I, Cp2, e3, meanPoor, e1, e2]
  exact P2_alg hn.ne' hqpos.ne' hz.ne' hS

/-! ## Proposition 2: additive decomposability -/

/-- A subgroup's sum is its size times its `P_α` (trivially true for an empty group). -/
theorem card_mul_P (α z : ℝ) (s : Finset ι) (y : ι → ℝ) :
    (s.card : ℝ) * P α z s y = ∑ i ∈ s, contrib α z (y i) := by
  rw [P]
  by_cases h : (s.card : ℝ) = 0
  · have : s = ∅ := by exact_mod_cast Finset.card_eq_zero.1 (by exact_mod_cast h)
    simp [this]
  · field_simp

/-- **Proposition 2, (4), p. 764**: for a population split into disjoint subgroups
`S j`, `n · P_α(y) = Σ_j n_j · P_α(y^(j))`, i.e. `P_α = Σ_j (n_j/n) P_α(y^(j))`. -/
theorem P_decomp {κ : Type*} [DecidableEq ι] (α z : ℝ) (G : Finset κ) (S : κ → Finset ι)
    (hdisj : (G : Set κ).PairwiseDisjoint S) (y : ι → ℝ) :
    ((G.biUnion S).card : ℝ) * P α z (G.biUnion S) y =
      ∑ j ∈ G, ((S j).card : ℝ) * P α z (S j) y := by
  rw [card_mul_P, Finset.sum_biUnion hdisj]
  exact Finset.sum_congr rfl fun j _ => (card_mul_P α z (S j) y).symm

/-- **Proposition 2** in the population-share form `P_α = Σ_j (n_j/n) P_α^(j)`. -/
theorem P_decomp_shares {κ : Type*} [DecidableEq ι] (α z : ℝ) (G : Finset κ)
    (S : κ → Finset ι) (hdisj : (G : Set κ).PairwiseDisjoint S) (y : ι → ℝ)
    (hn : (G.biUnion S).Nonempty) :
    P α z (G.biUnion S) y =
      ∑ j ∈ G, ((S j).card / (G.biUnion S).card : ℝ) * P α z (S j) y := by
  have hc : (0 : ℝ) < (G.biUnion S).card := by exact_mod_cast hn.card_pos
  have := P_decomp α z G S hdisj y
  simp only [div_mul_eq_mul_div, ← Finset.sum_div]
  rw [← this]
  field_simp

/-- **Subgroup Monotonicity Axiom** (p. 763), two-group form: if incomes change
only in subgroup `s₁` (sizes fixed) and `s₁` gets more poverty, total poverty rises. -/
theorem subgroup_monotone [DecidableEq ι] (α z : ℝ) {s₁ s₂ : Finset ι}
    (hd : Disjoint s₁ s₂) (y y' : ι → ℝ) (hsame : ∀ i ∈ s₂, y' i = y i)
    (hmore : P α z s₁ y < P α z s₁ y') :
    P α z (s₁ ∪ s₂) y < P α z (s₁ ∪ s₂) y' := by
  have hne : s₁.Nonempty := by
    rcases s₁.eq_empty_or_nonempty with h | h
    · simp [h, P] at hmore
    · exact h
  have h1 : (0 : ℝ) < s₁.card := by exact_mod_cast hne.card_pos
  have hsum : ∑ i ∈ s₁, contrib α z (y i) < ∑ i ∈ s₁, contrib α z (y' i) := by
    rw [← card_mul_P, ← card_mul_P]; exact mul_lt_mul_of_pos_left hmore h1
  have h2 : ∑ i ∈ s₂, contrib α z (y' i) = ∑ i ∈ s₂, contrib α z (y i) :=
    Finset.sum_congr rfl fun i hi => by rw [hsame i hi]
  have hc : (0 : ℝ) < (s₁ ∪ s₂).card := by
    exact_mod_cast (hne.mono Finset.subset_union_left).card_pos
  rw [P, P, Finset.sum_union hd, Finset.sum_union hd, h2]
  exact div_lt_div_of_pos_right (by linarith) hc

/-! ## Proposition 1: monotonicity -/

/-- Changing incomes at one household only changes the sum by that household's
difference. -/
theorem sum_update [DecidableEq ι] (f : ℝ → ℝ) {s : Finset ι} {i : ι} (hi : i ∈ s)
    (y y' : ι → ℝ) (hsame : ∀ j ∈ s, j ≠ i → y' j = y j) :
    ∑ j ∈ s, f (y' j) - ∑ j ∈ s, f (y j) = f (y' i) - f (y i) := by
  rw [← Finset.add_sum_erase s _ hi, ← Finset.add_sum_erase s _ hi,
    Finset.sum_congr rfl fun j hj => by rw [hsame j (Finset.mem_of_mem_erase hj)
      (Finset.ne_of_mem_erase hj)]]
  ring

/-- **Proposition 1, Monotonicity Axiom (α > 0), p. 763**: a reduction in the
income of a poor household strictly increases `P_α`. -/
theorem P_monotone [DecidableEq ι] {α z : ℝ} (hα : 0 < α) (hz : 0 < z) {s : Finset ι}
    {i : ι} (hi : i ∈ s) (y y' : ι → ℝ) (hpoor : y i ≤ z) (hdrop : y' i < y i)
    (hsame : ∀ j ∈ s, j ≠ i → y' j = y j) :
    P α z s y < P α z s y' := by
  have hc : (0 : ℝ) < s.card := by exact_mod_cast Finset.card_pos.2 ⟨i, hi⟩
  have hd := sum_update (contrib α z) hi y y' hsame
  have hlt : contrib α z (y i) < contrib α z (y' i) := by
    rw [contrib, contrib, if_pos hpoor, if_pos (by linarith)]
    apply Real.rpow_lt_rpow (div_nonneg (by linarith) hz.le) _ hα
    exact div_lt_div_of_pos_right (by linarith) hz
  rw [P, P]
  exact div_lt_div_of_pos_right (by linarith) hc

/-- At `α = 0` the Monotonicity Axiom fails: one poor household at `y = 1/2`
(line `z = 1`) falling to `0` leaves the headcount unchanged. -/
theorem P0_not_monotone :
    P 0 1 (Finset.univ : Finset (Fin 1)) (fun _ => 1 / 2) =
      P 0 1 (Finset.univ : Finset (Fin 1)) (fun _ => 0) := by
  simp [P, contrib]
  norm_num

/-! ## Proposition 1: the Transfer Axiom -/

/-- Strict convexity spreads: for `a < b ≤ c < d` with `a + d = b + c`,
`f b + f c < f a + f d`. -/
theorem strictConvex_spread {f : ℝ → ℝ} (hf : StrictConvexOn ℝ (Set.Ici 0) f)
    {a b c d : ℝ} (ha : 0 ≤ a) (hab : a < b) (hbc : b ≤ c) (hcd : c < d)
    (hsum : a + d = b + c) : f b + f c < f a + f d := by
  have hda : 0 < d - a := by linarith
  set θ := (d - b) / (d - a) with hθ
  have hθ0 : 0 < θ := div_pos (by linarith) hda
  have hθ1 : 0 < 1 - θ := by
    rw [hθ, one_sub_div hda.ne']; exact div_pos (by linarith) hda
  have hb : θ • a + (1 - θ) • d = b := by
    simp only [smul_eq_mul, hθ]; field_simp; ring
  have hc : (1 - θ) • a + θ • d = c := by
    rw [show c = a + d - b by linarith]
    simp only [smul_eq_mul, hθ]; field_simp; ring
  have had : a ≠ d := by linarith
  have ha' : a ∈ Set.Ici (0 : ℝ) := ha
  have hd' : d ∈ Set.Ici (0 : ℝ) := by simp only [Set.mem_Ici]; linarith
  have h1 := hf.2 ha' hd' had hθ0 hθ1 (by ring)
  have h2 := hf.2 ha' hd' had hθ1 hθ0 (by ring)
  rw [hb] at h1
  rw [hc] at h2
  simp only [smul_eq_mul] at h1 h2
  linarith

/-- **Proposition 1, Transfer Axiom (α > 1), p. 763**, case (i) of the paper's
proof: a transfer `t > 0` from poor household `i` to a richer household `j` that
stays poor (`y_j + t ≤ z`) strictly increases `P_α`.  The paper's argument —
strict convexity of `P_α` in the poor incomes — is `strictConvex_spread` applied
to `x ↦ x^α` on the normalised gaps. -/
theorem P_transfer [DecidableEq ι] {α z t : ℝ} (hα : 1 < α) (hz : 0 < z) (ht : 0 < t)
    {s : Finset ι} {i j : ι} (hi : i ∈ s) (hj : j ∈ s) (hij : i ≠ j)
    (y y' : ι → ℝ) (hricher : y i < y j) (hstay : y j + t ≤ z)
    (hyi : y' i = y i - t) (hyj : y' j = y j + t)
    (hsame : ∀ k ∈ s, k ≠ i → k ≠ j → y' k = y k) :
    P α z s y < P α z s y' := by
  have hc : (0 : ℝ) < s.card := by exact_mod_cast Finset.card_pos.2 ⟨i, hi⟩
  have hj' : j ∈ s.erase i := Finset.mem_erase.2 ⟨hij.symm, hj⟩
  have split : ∀ w : ι → ℝ, ∑ k ∈ s, contrib α z (w k) =
      contrib α z (w i) + (contrib α z (w j) + ∑ k ∈ (s.erase i).erase j, contrib α z (w k)) :=
    fun w => by
      rw [← Finset.add_sum_erase s (fun k => contrib α z (w k)) hi,
        ← Finset.add_sum_erase (s.erase i) (fun k => contrib α z (w k)) hj']
  have hrest : ∑ k ∈ (s.erase i).erase j, contrib α z (y' k) =
      ∑ k ∈ (s.erase i).erase j, contrib α z (y k) :=
    Finset.sum_congr rfl fun k hk => by
      obtain ⟨hkj, hk'⟩ := Finset.mem_erase.1 hk
      obtain ⟨hki, hks⟩ := Finset.mem_erase.1 hk'
      rw [hsame k hks hki hkj]
  have core : contrib α z (y i) + contrib α z (y j) <
      contrib α z (y' i) + contrib α z (y' j) := by
    rw [contrib, contrib, contrib, contrib, if_pos (by linarith), if_pos (by linarith),
      if_pos (by rw [hyi]; linarith), if_pos (by rw [hyj]; exact hstay), hyi, hyj]
    have key := strictConvex_spread (strictConvexOn_rpow hα)
      (a := (z - (y j + t)) / z) (b := (z - y j) / z) (c := (z - y i) / z)
      (d := (z - (y i - t)) / z)
      (div_nonneg (by linarith) hz.le)
      (div_lt_div_of_pos_right (by linarith) hz)
      (div_le_div_of_nonneg_right (by linarith) hz.le)
      (div_lt_div_of_pos_right (by linarith) hz)
      (by field_simp; ring)
    linarith
  rw [P, P, split y, split y', hrest]
  exact div_lt_div_of_pos_right (by linarith) hc

/-- Case (ii) of the paper's proof of the Transfer Axiom: a transfer from a poor
household to one at or above the line increases `P_α` "by inspection", for every
`α > 0` (the recipient contributes `0` before and after). -/
theorem P_transfer_to_nonpoor [DecidableEq ι] {α z t : ℝ} (hα : 0 < α) (hz : 0 < z)
    (ht : 0 < t) {s : Finset ι} {i j : ι} (hi : i ∈ s) (hj : j ∈ s) (hij : i ≠ j)
    (y y' : ι → ℝ) (hpoor : y i ≤ z) (hrich : z ≤ y j)
    (hyi : y' i = y i - t) (hyj : y' j = y j + t)
    (hsame : ∀ k ∈ s, k ≠ i → k ≠ j → y' k = y k) :
    P α z s y < P α z s y' := by
  -- First lower household i, then raise household j; the second step is neutral.
  classical
  let y₁ : ι → ℝ := Function.update y i (y i - t)
  have step1 : P α z s y < P α z s y₁ :=
    P_monotone hα hz hi y y₁ hpoor (by simp [y₁]; linarith)
      (fun k _ hk => by simp [y₁, hk])
  have hj0 : contrib α z (y j) = 0 := by
    rw [contrib]; split_ifs with h
    · have : y j = z := le_antisymm h hrich
      rw [this, sub_self, zero_div, Real.zero_rpow hα.ne']
    · rfl
  have hj1 : contrib α z (y' j) = 0 := by
    rw [contrib, if_neg (by rw [hyj]; linarith)]
  have step2 : P α z s y₁ = P α z s y' := by
    have hd := sum_update (contrib α z) hj y₁ y' (fun k hk hkj => by
      by_cases hki : k = i
      · subst hki; simp [y₁, hyi]
      · rw [hsame k hk hki hkj]; simp [y₁, hki])
    have hy₁j : y₁ j = y j := by simp [y₁, Ne.symm hij]
    rw [hy₁j, hj0, hj1] at hd
    rw [P, P]; congr 1; linarith
  linarith

/-- At `α = 1` the Transfer Axiom fails: with `z = 1`, incomes `(1/4, 1/2)` and a
transfer of `1/8` from the poorer to the richer (both stay poor), `P_1` is
unchanged at `5/8` while `P_2` rises from `13/32` to `29/64`. -/
theorem P1_transfer_neutral :
    P 1 1 (Finset.univ : Finset (Fin 2)) ![1 / 4, 1 / 2] = 5 / 8 ∧
      P 1 1 (Finset.univ : Finset (Fin 2)) ![1 / 8, 5 / 8] = 5 / 8 ∧
      P 2 1 (Finset.univ : Finset (Fin 2)) ![1 / 4, 1 / 2] = 13 / 32 ∧
      P 2 1 (Finset.univ : Finset (Fin 2)) ![1 / 8, 5 / 8] = 29 / 64 := by
  refine ⟨?_, ?_, ?_, ?_⟩ <;>
    simp [P, contrib, Fin.sum_univ_two] <;> norm_num

/-! ## The income-gap ratio is not population-share decomposable -/

/-- Three households with `z = 1`, incomes `(0, 1/2, 2)`; subgroups `{0}` and
`{1, 2}`. -/
noncomputable def exY : Fin 3 → ℝ := ![0, 1 / 2, 2]

/-- The mean normalised shortfall among the poor, `I`, is not additively
decomposable with population-share weights: overall `I = 3/4`, but
`(1/3)·I^(1) + (2/3)·I^(2) = (1/3)·1 + (2/3)·(1/2) = 2/3`. -/
theorem I_not_decomposable :
    I 1 Finset.univ exY = 3 / 4 ∧ I 1 {0} exY = 1 ∧ I 1 {1, 2} exY = 1 / 2 ∧
      I 1 Finset.univ exY ≠ (1 / 3) * I 1 {0} exY + (2 / 3) * I 1 {1, 2} exY := by
  have hu : poor 1 (Finset.univ : Finset (Fin 3)) exY = {0, 1} := by
    ext k; fin_cases k <;> (simp [poor, exY]; try norm_num)
  have h0 : poor 1 ({0} : Finset (Fin 3)) exY = {0} := by
    ext k; fin_cases k <;> simp [poor, exY]
  have h12 : poor 1 ({1, 2} : Finset (Fin 3)) exY = {1} := by
    ext k; fin_cases k <;> (simp [poor, exY]; try norm_num)
  have e1 : I 1 Finset.univ exY = 3 / 4 := by
    rw [I, hu]; simp [exY]; norm_num
  have e2 : I 1 {0} exY = 1 := by rw [I, h0]; simp [exY]
  have e3 : I 1 {1, 2} exY = 1 / 2 := by rw [I, h12]; simp [exY]; norm_num
  refine ⟨e1, e2, e3, ?_⟩
  rw [e1, e2, e3]; norm_num

/-- On the same data, `P_1` does decompose (Proposition 2): overall `P_1 = 1/2`
and `(1/3)·P_1^(1) + (2/3)·P_1^(2) = (1/3)·1 + (2/3)·(1/4) = 1/2`. -/
theorem P1_decomposes_on_example :
    P 1 1 Finset.univ exY = 1 / 2 ∧ P 1 1 {0} exY = 1 ∧ P 1 1 {1, 2} exY = 1 / 4 ∧
      P 1 1 Finset.univ exY = (1 / 3) * P 1 1 {0} exY + (2 / 3) * P 1 1 {1, 2} exY := by
  have e1 : P 1 1 Finset.univ exY = 1 / 2 := by
    simp [P, contrib, exY, Fin.sum_univ_three]; norm_num
  have e2 : P 1 1 {0} exY = 1 := by simp [P, contrib, exY]
  have e3 : P 1 1 {1, 2} exY = 1 / 4 := by
    simp [P, contrib, exY]; norm_num
  refine ⟨e1, e2, e3, ?_⟩
  rw [e1, e2, e3]; norm_num

end Literature.FGT
