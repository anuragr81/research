import IscLean.Displacement
import IscLean.Ladder
import Mathlib.Analysis.Convex.SpecificFunctions.Basic

open Finset

namespace Isc

noncomputable def returnFromLoss (κ b x0 ℓ : ℝ) : ℝ := x0 + ℓ - κ * ℓ ^ b

theorem returnFromLoss_le_floor_plus_loss {κ b x0 ℓ : ℝ} (hκ : 0 ≤ κ) (hℓ : 0 ≤ ℓ) :
    returnFromLoss κ b x0 ℓ ≤ x0 + ℓ := by
  unfold returnFromLoss
  have : 0 ≤ κ * ℓ ^ b := mul_nonneg hκ (Real.rpow_nonneg hℓ b)
  linarith

theorem upCost_rises_in_b_iff {g b₁ b₂ : ℝ} (hg : 0 < g) (hb : b₁ < b₂) :
    g ^ b₁ < g ^ b₂ ↔ 1 < g := by
  constructor
  · intro h
    by_contra hc
    push_neg at hc
    rcases eq_or_lt_of_le hc with h1 | h1
    · rw [h1, Real.one_rpow, Real.one_rpow] at h
      exact lt_irrefl _ h
    · have := Real.rpow_lt_rpow_of_exponent_gt hg h1 hb
      linarith
  · intro h
    exact Real.rpow_lt_rpow_of_exponent_lt h hb

theorem upCost_falls_in_b {g b₁ b₂ : ℝ} (hg : 0 < g) (hg1 : g < 1) (hb : b₁ < b₂) :
    g ^ b₂ < g ^ b₁ :=
  Real.rpow_lt_rpow_of_exponent_gt hg hg1 hb

theorem scale_free_cost_falls_in_b {g x b₁ b₂ : ℝ} (hg : 0 < g) (hgx : g < x) (hb : b₁ < b₂) :
    x * (g / x) ^ b₂ < x * (g / x) ^ b₁ := by
  have hx : 0 < x := lt_trans hg hgx
  have h0 : 0 < g / x := div_pos hg hx
  have h1 : g / x < 1 := (div_lt_one hx).mpr hgx
  exact mul_lt_mul_of_pos_left (Real.rpow_lt_rpow_of_exponent_gt h0 h1 hb) hx

theorem loss_and_cost_rise_with_rank {x0 xi xj b : ℝ} (h0 : x0 ≤ xi) (hij : xi < xj) (hb : 0 < b) :
    xi - x0 < xj - x0 ∧ (xi - x0) ^ b < (xj - x0) ^ b :=
  ⟨by linarith, Real.rpow_lt_rpow (by linarith) (by linarith) hb⟩

theorem return_beats_downgrade_iff_scaled {κ b x0 ℓ : ℝ} (hℓ : 0 < ℓ) :
    x0 < returnFromLoss κ b x0 ℓ ↔ κ * ℓ ^ (b - 1) < 1 := by
  unfold returnFromLoss
  have e : κ * ℓ ^ b = (κ * ℓ ^ (b - 1)) * ℓ := by
    rw [Real.rpow_sub_one hℓ.ne']
    field_simp
  rw [e]
  constructor
  · intro h
    have : (κ * ℓ ^ (b - 1)) * ℓ < 1 * ℓ := by linarith
    exact lt_of_mul_lt_mul_right this hℓ.le
  · intro h
    have : (κ * ℓ ^ (b - 1)) * ℓ < 1 * ℓ := mul_lt_mul_of_pos_right h hℓ
    linarith

theorem return_set_interval {κ b x0 V ℓ₁ ℓ₂ ℓ₃ : ℝ} (hb : 1 ≤ b) (hκ : 0 ≤ κ) (h0 : 0 ≤ ℓ₁)
    (h12 : ℓ₁ ≤ ℓ₂) (h23 : ℓ₂ ≤ ℓ₃) (hv1 : V < returnFromLoss κ b x0 ℓ₁)
    (hv3 : V < returnFromLoss κ b x0 ℓ₃) : V < returnFromLoss κ b x0 ℓ₂ := by
  rcases eq_or_lt_of_le (le_trans h12 h23) with h13 | h13
  · have e : ℓ₂ = ℓ₁ := le_antisymm (h13 ▸ h23) h12
    rw [e]
    exact hv1
  · set t := (ℓ₃ - ℓ₂) / (ℓ₃ - ℓ₁) with ht
    have hd : 0 < ℓ₃ - ℓ₁ := by linarith
    have t0 : 0 ≤ t := div_nonneg (by linarith) hd.le
    have t1 : 0 ≤ 1 - t := by
      rw [ht, one_sub_div hd.ne']
      exact div_nonneg (by linarith) hd.le
    have hmix : t * ℓ₁ + (1 - t) * ℓ₃ = ℓ₂ := by
      rw [ht]
      field_simp
      ring
    have hconv0 := (convexOn_rpow hb).2 (Set.mem_Ici.mpr h0) (Set.mem_Ici.mpr (show (0 : ℝ) ≤ ℓ₃ by linarith))
      t0 t1 (by ring)
    have hconv : (t * ℓ₁ + (1 - t) * ℓ₃) ^ b ≤ t * ℓ₁ ^ b + (1 - t) * ℓ₃ ^ b := by
      simpa only [smul_eq_mul] using hconv0
    rw [hmix] at hconv
    unfold returnFromLoss at hv1 hv3 ⊢
    have key : t * (x0 + ℓ₁ - κ * ℓ₁ ^ b) + (1 - t) * (x0 + ℓ₃ - κ * ℓ₃ ^ b)
        ≤ x0 + ℓ₂ - κ * ℓ₂ ^ b := by
      have : κ * ℓ₂ ^ b ≤ κ * (t * ℓ₁ ^ b + (1 - t) * ℓ₃ ^ b) := mul_le_mul_of_nonneg_left hconv hκ
      have e : t * (x0 + ℓ₁ - κ * ℓ₁ ^ b) + (1 - t) * (x0 + ℓ₃ - κ * ℓ₃ ^ b)
          = x0 + (t * ℓ₁ + (1 - t) * ℓ₃) - κ * (t * ℓ₁ ^ b + (1 - t) * ℓ₃ ^ b) := by ring
      rw [e, hmix]
      linarith
    have hpos : V < t * (x0 + ℓ₁ - κ * ℓ₁ ^ b) + (1 - t) * (x0 + ℓ₃ - κ * ℓ₃ ^ b) := by
      rcases eq_or_lt_of_le t0 with ht0 | ht0
      · rw [← ht0]
        linarith
      · have e : t * (x0 + ℓ₁ - κ * ℓ₁ ^ b) + (1 - t) * (x0 + ℓ₃ - κ * ℓ₃ ^ b) - V
            = t * ((x0 + ℓ₁ - κ * ℓ₁ ^ b) - V) + (1 - t) * ((x0 + ℓ₃ - κ * ℓ₃ ^ b) - V) := by ring
        have h1 : 0 < t * ((x0 + ℓ₁ - κ * ℓ₁ ^ b) - V) := mul_pos ht0 (by linarith)
        have h3 : 0 ≤ (1 - t) * ((x0 + ℓ₃ - κ * ℓ₃ ^ b) - V) := mul_nonneg t1 (by linarith)
        linarith
    linarith

theorem return_value_falls_with_loss {κ b x0 ℓ₁ ℓ₂ : ℝ} (hb : 1 ≤ b) (hκ : 0 ≤ κ) (h1 : 0 < ℓ₁)
    (h12 : ℓ₁ ≤ ℓ₂) (hturn : 1 ≤ κ * b * ℓ₁ ^ (b - 1)) :
    returnFromLoss κ b x0 ℓ₂ ≤ returnFromLoss κ b x0 ℓ₁ := by
  set s := ℓ₂ / ℓ₁ - 1 with hs
  have hs0 : 0 ≤ s := by
    rw [hs, sub_nonneg, le_div_iff₀ h1]
    linarith
  have hbern := one_add_mul_self_le_rpow_one_add (by linarith : (-1 : ℝ) ≤ s) hb
  have hl2 : ℓ₂ = ℓ₁ * (1 + s) := by
    rw [hs]
    field_simp
  have hpow : ℓ₂ ^ b = ℓ₁ ^ b * (1 + s) ^ b := by
    rw [hl2, Real.mul_rpow h1.le (by linarith)]
  have hsplit : ℓ₁ ^ b = ℓ₁ ^ (b - 1) * ℓ₁ := by
    rw [Real.rpow_sub_one h1.ne']
    field_simp
  have hpb : 0 ≤ ℓ₁ ^ b := Real.rpow_nonneg h1.le b
  have hgrow : ℓ₁ ^ b * (1 + b * s) ≤ ℓ₂ ^ b := by
    rw [hpow]
    exact mul_le_mul_of_nonneg_left hbern hpb
  have hs_l : ℓ₁ * s = ℓ₂ - ℓ₁ := by
    rw [hs]
    field_simp
  have hinc : κ * b * ℓ₁ ^ (b - 1) * (ℓ₂ - ℓ₁) ≤ κ * (ℓ₂ ^ b - ℓ₁ ^ b) := by
    have : ℓ₁ ^ b * (b * s) ≤ ℓ₂ ^ b - ℓ₁ ^ b := by nlinarith
    have e : ℓ₁ ^ b * (b * s) = b * ℓ₁ ^ (b - 1) * (ℓ₂ - ℓ₁) := by
      rw [hsplit, ← hs_l]
      ring
    rw [e] at this
    have := mul_le_mul_of_nonneg_left this hκ
    linarith
  have hd : 0 ≤ ℓ₂ - ℓ₁ := by linarith
  have : ℓ₂ - ℓ₁ ≤ κ * b * ℓ₁ ^ (b - 1) * (ℓ₂ - ℓ₁) := by nlinarith
  unfold returnFromLoss
  linarith

theorem return_value_antitone_in_b {κ x0 ℓ b₁ b₂ : ℝ} (hκ : 0 ≤ κ) (hℓ : 1 ≤ ℓ) (hb : b₁ ≤ b₂) :
    returnFromLoss κ b₂ x0 ℓ ≤ returnFromLoss κ b₁ x0 ℓ := by
  unfold returnFromLoss
  have := mul_le_mul_of_nonneg_left (Real.rpow_le_rpow_of_exponent_le hℓ hb) hκ
  linarith

theorem return_value_monotone_in_b {κ x0 ℓ b₁ b₂ : ℝ} (hκ : 0 ≤ κ) (hℓ0 : 0 < ℓ) (hℓ : ℓ ≤ 1)
    (hb : b₁ ≤ b₂) : returnFromLoss κ b₁ x0 ℓ ≤ returnFromLoss κ b₂ x0 ℓ := by
  unfold returnFromLoss
  have := mul_le_mul_of_nonneg_left (Real.rpow_le_rpow_of_exponent_ge hℓ0 hℓ hb) hκ
  linarith

theorem movers_rise_in_b {ι : Type*} (D : Finset ι) (ℓ A : ι → ℝ) {κ x0 b₁ b₂ : ℝ}
    {u : ℝ → ℝ} (hu : Monotone u) (hκ : 0 ≤ κ) (hb : b₁ ≤ b₂) (hD : ∀ i ∈ D, 1 ≤ ℓ i) :
    (D.filter (fun i => u (returnFromLoss κ b₁ x0 (ℓ i)) ≤ A i)).card
      ≤ (D.filter (fun i => u (returnFromLoss κ b₂ x0 (ℓ i)) ≤ A i)).card := by
  apply card_le_card
  intro i hi
  rw [mem_filter] at hi ⊢
  exact ⟨hi.1, le_trans (hu (return_value_antitone_in_b hκ (hD i hi.1) hb)) hi.2⟩

variable {ι : Type*} [DecidableEq ι]

theorem top_displaced_prefer_lottery {P : Finset ι} (h2 : 2 ≤ P.card) (x : ι → ℝ) {L x0 κ b : ℝ}
    {u : ℝ → ℝ} {S : Set ℝ} (hu : MonotoneOn u S) (hx0 : x0 ∈ S) (hS : ∀ k ∈ P, x k - L ∈ S)
    (hL : ∀ k ∈ P, x0 ≤ x k - L) {i : ι} (hi : i ∈ P) (hloss : 0 < x i - x0)
    (hret : returnFromLoss κ b x0 (x i - x0) ∈ S) (hcost : 1 ≤ κ * (x i - x0) ^ (b - 1)) :
    u (returnFromLoss κ b x0 (x i - x0)) ≤ poolEU P x L u i ∧ u x0 ≤ poolEU P x L u i := by
  have hdown := displaced_prefer_lottery h2 x hu hx0 hS hL i hi
  have hle : returnFromLoss κ b x0 (x i - x0) ≤ x0 := by
    by_contra hc
    push_neg at hc
    have := (return_beats_downgrade_iff_scaled hloss).mp hc
    linarith
  exact ⟨le_trans (hu hret hx0 hle) hdown, hdown⟩

theorem up_moves_fall_with_rank (P : Finset ι) (x : ι → ℝ) {i j : ι} (hij : x i ≤ x j) :
    ((P.erase j).filter (fun k => x j < x k)).card ≤ ((P.erase i).filter (fun k => x i < x k)).card := by
  apply card_le_card
  intro k hk
  rw [mem_filter, mem_erase] at hk ⊢
  have hik : x i < x k := lt_of_le_of_lt hij hk.2
  exact ⟨⟨fun h => by rw [h] at hik; exact lt_irrefl _ hik, hk.1.2⟩, hik⟩

theorem rank_dependent_externality {P : Finset ι} (h2 : 2 ≤ P.card) {i j : ι} (hi : i ∈ P)
    (hj : j ∉ P) (x : ι → ℝ) (L : ℝ) :
    poolGain (insert j P) x L i - poolGain P x L i
      = (x j - ((∑ k ∈ P, x k) - x i) / ((P.card : ℝ) - 1)) / (P.card : ℝ) := by
  have hi' : i ∈ insert j P := mem_insert_of_mem hi
  rw [poolGain_closed hi', poolGain_closed hi, card_insert_of_notMem hj, sum_insert hj]
  have hN2 : (2 : ℝ) ≤ (P.card : ℝ) := by exact_mod_cast h2
  have h1 : (P.card : ℝ) - 1 ≠ 0 := by linarith
  have h0 : (P.card : ℝ) ≠ 0 := by linarith
  push_cast
  field_simp
  ring

theorem pool_prizes_rise_with_inequality {Q : Finset ℝ} (hne : Q.Nonempty) {x0 a₁ a₂ : ℝ}
    (hx0 : 0 < x0) (ha₁ : 0 < a₁) (h12 : a₁ < a₂) (hQ : ∀ q ∈ Q, 0 < q ∧ q < 1) :
    ∑ q ∈ Q, pareto x0 a₂ q < ∑ q ∈ Q, pareto x0 a₁ q :=
  sum_lt_sum_of_nonempty hne (fun q hq => pareto_strictAnti_tail hx0 ha₁ h12 (hQ q hq).1 (hQ q hq).2)

theorem inequality_pushes_top_to_lottery {κ b x0 ℓ₁ ℓ₂ V₁ V₂ : ℝ} (hb : 1 ≤ b) (hκ : 0 ≤ κ)
    (h1 : 0 < ℓ₁) (h12 : ℓ₁ ≤ ℓ₂) (hturn : 1 ≤ κ * b * ℓ₁ ^ (b - 1)) (hV : V₁ ≤ V₂)
    (hlot : returnFromLoss κ b x0 ℓ₁ ≤ V₁) : returnFromLoss κ b x0 ℓ₂ ≤ V₂ :=
  le_trans (le_trans (return_value_falls_with_loss hb hκ h1 h12 hturn) hlot) hV

theorem proportional_top_downgrades {κ b δ xi V : ℝ} (hδ0 : 0 < δ) (hxi : 0 < xi)
    (hcost : 1 ≤ κ * (δ * xi) ^ (b - 1)) (hV : V ≤ (1 - δ) * xi) :
    xi - κ * (δ * xi) ^ b ≤ (1 - δ) * xi ∧ V ≤ (1 - δ) * xi := by
  have hl : 0 < δ * xi := mul_pos hδ0 hxi
  have e : κ * (δ * xi) ^ b = κ * (δ * xi) ^ (b - 1) * (δ * xi) := by
    rw [Real.rpow_sub_one hl.ne']
    field_simp
  refine ⟨?_, hV⟩
  rw [e]
  have : 1 * (δ * xi) ≤ κ * (δ * xi) ^ (b - 1) * (δ * xi) := mul_le_mul_of_nonneg_right hcost hl.le
  nlinarith

theorem proportional_bottom_lottery {κ b δ xi V : ℝ} (hκ : 0 ≤ κ) (hδ0 : 0 ≤ δ) (hxi : 0 ≤ xi)
    (hV : xi ≤ V) : xi - κ * (δ * xi) ^ b ≤ V ∧ (1 - δ) * xi ≤ V := by
  have : 0 ≤ κ * (δ * xi) ^ b := mul_nonneg hκ (Real.rpow_nonneg (mul_nonneg hδ0 hxi) b)
  constructor
  · linarith
  · nlinarith

theorem pareto_eq_exp {x0 a q : ℝ} (hq : q < 1) :
    pareto x0 a q = x0 * Real.exp (-Real.log (1 - q) * (1 / a)) := by
  unfold pareto
  rw [Real.rpow_def_of_pos (by linarith)]
  congr 2
  ring

omit [DecidableEq ι] in
theorem exp_mean_chord {K : Finset ι} (hK : K.Nonempty) (c : ι → ℝ) {t θ : ℝ} (t0 : 0 ≤ t)
    (t1 : t ≤ 1) :
    (∑ k ∈ K, Real.exp (c k * (t * θ))) / (K.card : ℝ) - 1
      ≤ t * ((∑ k ∈ K, Real.exp (c k * θ)) / (K.card : ℝ) - 1) := by
  have hm : (0 : ℝ) < (K.card : ℝ) := by exact_mod_cast hK.card_pos
  have each : ∀ k ∈ K, Real.exp (c k * (t * θ)) ≤ t * Real.exp (c k * θ) + (1 - t) := by
    intro k _
    have hc := convexOn_exp.2 (Set.mem_univ (c k * θ)) (Set.mem_univ 0) t0
      (by linarith : (0 : ℝ) ≤ 1 - t) (by ring)
    simp only [smul_eq_mul, mul_zero, add_zero, Real.exp_zero, mul_one] at hc
    have e : t * (c k * θ) = c k * (t * θ) := by ring
    rw [e] at hc
    exact hc
  have hs := sum_le_sum each
  rw [sum_add_distrib, ← mul_sum, sum_const, nsmul_eq_mul] at hs
  have key : (∑ k ∈ K, Real.exp (c k * (t * θ))) / (K.card : ℝ)
      ≤ t * ((∑ k ∈ K, Real.exp (c k * θ)) / (K.card : ℝ)) + (1 - t) := by
    rw [div_le_iff₀ hm]
    have e : (t * ((∑ k ∈ K, Real.exp (c k * θ)) / (K.card : ℝ)) + (1 - t)) * (K.card : ℝ)
        = t * ∑ k ∈ K, Real.exp (c k * θ) + (K.card : ℝ) * (1 - t) := by
      field_simp
      ring
    rw [e]
    exact hs
  linarith

omit [DecidableEq ι] in
theorem lottery_gap_rises_below_pool_mean {K : Finset ι} (hK : K.Nonempty) (y : ι → ℝ)
    {yi x0 θ₁ θ₂ : ℝ} (hx0 : 0 < x0) (hyi : 0 ≤ yi) (h1 : 0 < θ₁) (h12 : θ₁ ≤ θ₂)
    (hbelow : x0 * Real.exp (yi * θ₁) ≤ (∑ k ∈ K, x0 * Real.exp (y k * θ₁)) / (K.card : ℝ)) :
    (∑ k ∈ K, x0 * Real.exp (y k * θ₁)) / (K.card : ℝ) - x0 * Real.exp (yi * θ₁)
      ≤ (∑ k ∈ K, x0 * Real.exp (y k * θ₂)) / (K.card : ℝ) - x0 * Real.exp (yi * θ₂) := by
  have hm : (0 : ℝ) < (K.card : ℝ) := by exact_mod_cast hK.card_pos
  have h2 : 0 < θ₂ := lt_of_lt_of_le h1 h12
  set t := θ₁ / θ₂ with ht
  have t0 : 0 ≤ t := div_nonneg h1.le h2.le
  have t1 : t ≤ 1 := (div_le_one h2).mpr h12
  have htθ : t * θ₂ = θ₁ := by
    rw [ht]
    field_simp
  let H : ℝ → ℝ := fun θ => (∑ k ∈ K, Real.exp ((y k - yi) * θ)) / (K.card : ℝ) - 1
  have hfactor : ∀ θ, (∑ k ∈ K, x0 * Real.exp (y k * θ)) / (K.card : ℝ) - x0 * Real.exp (yi * θ)
      = x0 * Real.exp (yi * θ) * H θ := by
    intro θ
    have e : ∀ k, x0 * Real.exp (y k * θ) = x0 * Real.exp (yi * θ) * Real.exp ((y k - yi) * θ) := by
      intro k
      rw [mul_assoc, ← Real.exp_add]
      ring_nf
    simp only [H]
    simp_rw [e, ← mul_sum]
    rw [mul_div_assoc]
    ring
  have hchord : H θ₁ ≤ t * H θ₂ := by
    have := exp_mean_chord hK (fun k => y k - yi) t0 t1 (θ := θ₂)
    simp only [htθ] at this
    exact this
  have hpos1 : 0 < x0 * Real.exp (yi * θ₁) := mul_pos hx0 (Real.exp_pos _)
  have hH1 : 0 ≤ H θ₁ := by
    have h := hfactor θ₁
    have hprod : 0 ≤ x0 * Real.exp (yi * θ₁) * H θ₁ := by rw [← h]; linarith
    by_contra hc
    push_neg at hc
    have := mul_neg_of_pos_of_neg hpos1 hc
    linarith
  have hH2 : H θ₁ ≤ H θ₂ := by
    have hH2nn : 0 ≤ H θ₂ := by
      rcases eq_or_lt_of_le t0 with ht0 | ht0
      · exact absurd ht0.symm (div_pos h1 h2).ne'
      · nlinarith
    nlinarith
  have hexp : x0 * Real.exp (yi * θ₁) ≤ x0 * Real.exp (yi * θ₂) :=
    mul_le_mul_of_nonneg_left (Real.exp_le_exp.mpr (mul_le_mul_of_nonneg_left h12 hyi)) hx0.le
  rw [hfactor θ₁, hfactor θ₂]
  calc x0 * Real.exp (yi * θ₁) * H θ₁ ≤ x0 * Real.exp (yi * θ₂) * H θ₁ :=
        mul_le_mul_of_nonneg_right hexp hH1
    _ ≤ x0 * Real.exp (yi * θ₂) * H θ₂ :=
        mul_le_mul_of_nonneg_left hH2 (mul_pos hx0 (Real.exp_pos _)).le

theorem pareto_values_for_counterexample :
    pareto 1 2 (8 / 9) = 3 ∧ pareto 1 1 (8 / 9) = 9 ∧
      pareto 1 2 (341 / 441) = 21 / 10 ∧ pareto 1 1 (341 / 441) = 441 / 100 := by
  unfold pareto
  have sq : ∀ c : ℝ, 0 < c → (c ^ 2) ^ (-(1 / (2 : ℝ))) = c⁻¹ := by
    intro c hc
    rw [← Real.rpow_natCast, ← Real.rpow_mul hc.le]
    norm_num
    exact Real.rpow_neg_one c
  refine ⟨?_, ?_, ?_, ?_⟩
  · rw [show (1 : ℝ) - 8 / 9 = (1 / 3) ^ 2 by norm_num, sq _ (by norm_num)]
    norm_num
  · rw [show -(1 / (1 : ℝ)) = -1 by norm_num, Real.rpow_neg_one]
    norm_num
  · rw [show (1 : ℝ) - 341 / 441 = (10 / 21) ^ 2 by norm_num, sq _ (by norm_num)]
    norm_num
  · rw [show -(1 / (1 : ℝ)) = -1 by norm_num, Real.rpow_neg_one]
    norm_num

theorem lottery_play_can_fall_with_inequality :
    returnFromLoss (1 / 2) (21 / 20) 1 (pareto 1 2 (8 / 9) - 1) < pareto 1 2 (341 / 441) - 1 / 10 ∧
      returnFromLoss (1 / 2) (21 / 20) 1 (pareto 1 2 (341 / 441) - 1) < pareto 1 2 (8 / 9) - 1 / 10 ∧
      1 < pareto 1 2 (341 / 441) - 1 / 10 ∧
      pareto 1 1 (341 / 441) - 1 / 10 < returnFromLoss (1 / 2) (21 / 20) 1 (pareto 1 1 (8 / 9) - 1) ∧
      1 < returnFromLoss (1 / 2) (21 / 20) 1 (pareto 1 1 (8 / 9) - 1) := by
  obtain ⟨e1, e2, e3, e4⟩ := pareto_values_for_counterexample
  rw [e1, e2, e3, e4]
  unfold returnFromLoss
  have h2 : (2 : ℝ) < (3 - 1 : ℝ) ^ (21 / 20 : ℝ) := by
    have := Real.rpow_lt_rpow_of_exponent_lt (by norm_num : (1 : ℝ) < 2) (by norm_num : (1 : ℝ) < 21 / 20)
    rw [Real.rpow_one] at this
    norm_num
    exact this
  have hk : 0 ≤ (21 / 10 - 1 : ℝ) ^ (21 / 20 : ℝ) := Real.rpow_nonneg (by norm_num) _
  have h8 : (9 - 1 : ℝ) ^ (21 / 20 : ℝ) < 8 * (111 / 100) := by
    have e : (8 : ℝ) ^ (21 / 20 : ℝ) = 8 * (8 : ℝ) ^ (1 / 20 : ℝ) := by
      rw [show (21 / 20 : ℝ) = 1 + 1 / 20 by norm_num, Real.rpow_add (by norm_num), Real.rpow_one]
    have r : (8 : ℝ) ^ (1 / 20 : ℝ) < 111 / 100 := by
      apply lt_of_pow_lt_pow_left₀ 20 (by norm_num)
      rw [← Real.rpow_natCast, ← Real.rpow_mul (by norm_num)]
      norm_num
    norm_num
    rw [e]
    linarith
  refine ⟨by linarith, by linarith, by norm_num, by linarith, by linarith⟩

theorem lottery_play_rises_below_pool_mean {m₁ m₂ x₁ x₂ c₁ c₂ L : ℝ} (hgap : m₁ - x₁ ≤ m₂ - x₂)
    (hcost : c₁ ≤ c₂) (hlot : x₁ - c₁ ≤ m₁ - L) : x₂ - c₂ ≤ m₂ - L := by
  linarith

theorem tails_gamble_example :
    returnFromLoss 1 2 1 (1 / 10) < 6 / 5 ∧ 6 / 5 < returnFromLoss 1 2 1 (1 / 2) ∧
      returnFromLoss 1 2 1 2 < 6 / 5 := by
  unfold returnFromLoss
  rw [show (2 : ℝ) = ((2 : ℕ) : ℝ) by norm_num, Real.rpow_natCast, Real.rpow_natCast, Real.rpow_natCast]
  norm_num

theorem cost_exceeds_regain_upward {κ b ℓ₁ ℓ₂ : ℝ} (hκ : 0 ≤ κ) (hb : 1 ≤ b) (h1 : 0 < ℓ₁)
    (h12 : ℓ₁ ≤ ℓ₂) (h : 1 ≤ κ * ℓ₁ ^ (b - 1)) : 1 ≤ κ * ℓ₂ ^ (b - 1) := by
  have hp : ℓ₁ ^ (b - 1) ≤ ℓ₂ ^ (b - 1) := Real.rpow_le_rpow h1.le h12 (by linarith)
  nlinarith [mul_le_mul_of_nonneg_left hp hκ]

theorem scale_free_return_beats_step {b δ x : ℝ} (hb : 1 < b) (hδ0 : 0 < δ) (hδ1 : δ < 1)
    (hx : 0 < x) : (1 - δ) * x < x - x * δ ^ b := by
  have h : δ ^ b < δ ^ (1 : ℝ) := Real.rpow_lt_rpow_of_exponent_gt hδ0 hδ1 hb
  rw [Real.rpow_one] at h
  nlinarith [mul_lt_mul_of_pos_left h hx]

theorem control_step_beats_scale_free_return_below_unit {b δ x : ℝ} (hb : b < 1) (hδ0 : 0 < δ)
    (hδ1 : δ < 1) (hx : 0 < x) : x - x * δ ^ b < (1 - δ) * x := by
  have h : δ ^ (1 : ℝ) < δ ^ b := Real.rpow_lt_rpow_of_exponent_gt hδ0 hδ1 hb
  rw [Real.rpow_one] at h
  nlinarith [mul_lt_mul_of_pos_left h hx]

theorem control_mobility_falls_in_b_below_unit :
    returnFromLoss 1 2 0 (1 / 2) < returnFromLoss 1 3 0 (1 / 2) := by
  unfold returnFromLoss
  rw [show (3 : ℝ) = ((3 : ℕ) : ℝ) by norm_num, show (2 : ℝ) = ((2 : ℕ) : ℝ) by norm_num,
    Real.rpow_natCast, Real.rpow_natCast]
  norm_num

theorem control_return_interval_needs_b_ge_one :
    returnFromLoss 1 (1 / 2) 0 0 = 0 ∧ returnFromLoss 1 (1 / 2) 0 1 = 0 ∧
      returnFromLoss 1 (1 / 2) 0 4 = 2 := by
  unfold returnFromLoss
  refine ⟨by simp, by simp, ?_⟩
  rw [show (4 : ℝ) = 2 ^ (2 : ℝ) by norm_num, ← Real.rpow_mul (by norm_num)]
  norm_num

theorem control_top_lottery_needs_cost_bound :
    poolEU ({0, 1} : Finset ℕ) (fun k => if k = 0 then (2 : ℝ) else 3) 2 id 1 < 1 := by
  norm_num [poolEU]

/-- At the source's own parameters, `κ = 1`, `b = 2`, income in units of the bottom income, the
return to a job paying `x` is worth `x - (x - 1)²`, which never exceeds `5/4`. So whenever the
lottery is worth at least `5/4`, as it is when the pool's mean prize is the ladder's mean `2`
(Pareto with `a = 2`) and the lottery cost is at most `3/4`, no rank strictly prefers the return:
there is no middle band, and the whole ladder gambles. -/
theorem no_middle_band_at_source_parameters (x V : ℝ) (hV : 5 / 4 ≤ V) :
    returnFromLoss 1 2 1 (x - 1) ≤ V := by
  unfold returnFromLoss
  rw [show (2 : ℝ) = ((2 : ℕ) : ℝ) by norm_num, Real.rpow_natCast]
  nlinarith [sq_nonneg (x - 3 / 2)]

/-- Control: with a cheaper return, `κ = 1/4`, the middle band exists at the same lottery value:
the job paying `2` is regained for `1/4` and kept, worth `7/4`, above a lottery worth `3/2`. -/
theorem control_middle_band_with_cheap_return :
    (3 / 2 : ℝ) < returnFromLoss (1 / 4) 2 1 (2 - 1) := by
  unfold returnFromLoss
  rw [show (2 : ℝ) = ((2 : ℕ) : ℝ) by norm_num, Real.rpow_natCast]
  norm_num

end Isc
