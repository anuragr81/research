import Mathlib.Topology.Order.Basic
import Mathlib.Topology.Instances.Real.Lemmas
import Mathlib.Tactic

open Finset

namespace KoszegiRabin2006

section GainLoss

theorem symmetric_sum_nonpos {μ : ℝ → ℝ} (h0 : μ 0 = 0) (hc : ContinuousAt μ 0)
    (hA2 : ∀ x y, 0 < x → x < y → μ y + μ (-y) < μ x + μ (-x)) {x : ℝ} (hx : 0 < x) :
    μ x + μ (-x) ≤ 0 := by
  by_contra hpos
  push_neg at hpos
  set g := fun t => μ t + μ (-t) with hg
  have hgc : ContinuousAt g 0 := by
    have hneg : ContinuousAt (fun t : ℝ => μ (-t)) 0 := by
      have : ContinuousAt (fun t : ℝ => -t) 0 := continuous_neg.continuousAt
      have hc' : ContinuousAt μ (-(0 : ℝ)) := by simpa using hc
      exact hc'.comp this
    exact hc.add hneg
  have hg0 : g 0 = 0 := by simp [hg, h0]
  have hev : g ⁻¹' Set.Iio (g x) ∈ nhds (0 : ℝ) :=
    hgc.tendsto (Iio_mem_nhds (show g 0 < g x by rw [hg0]; exact hpos))
  rw [Metric.mem_nhds_iff] at hev
  obtain ⟨ε, hε, hball⟩ := hev
  set t := min (ε / 2) (x / 2) with ht
  have ht0 : 0 < t := lt_min (by linarith) (by linarith)
  have htx : t < x := lt_of_le_of_lt (min_le_right _ _) (by linarith)
  have htε : dist t 0 < ε := by
    rw [Real.dist_eq, sub_zero, abs_of_pos ht0]
    exact lt_of_le_of_lt (min_le_left _ _) (by linarith)
  have h1 : g t < g x := hball (by rw [Metric.mem_ball]; exact htε)
  have h2 : g x < g t := hA2 t x ht0 htx
  linarith

theorem symmetric_sum_neg {μ : ℝ → ℝ} (h0 : μ 0 = 0) (hc : ContinuousAt μ 0)
    (hA2 : ∀ x y, 0 < x → x < y → μ y + μ (-y) < μ x + μ (-x)) {y : ℝ} (hy : 0 < y) :
    μ y + μ (-y) < 0 :=
  lt_of_lt_of_le (hA2 (y / 2) y (by linarith) (by linarith))
    (symmetric_sum_nonpos h0 hc hA2 (by linarith))

theorem status_quo_bias {μ m : ℝ → ℝ} (h0 : μ 0 = 0) (hc : ContinuousAt μ 0)
    (hA2 : ∀ x y, 0 < x → x < y → μ y + μ (-y) < μ x + μ (-x)) (hm : StrictMono m) {c c' : ℝ}
    (hne : c ≠ c') (h : m c + μ (m c - m c') ≥ m c' + μ (m c' - m c')) :
    m c + μ (m c - m c) > m c' + μ (m c' - m c) := by
  by_contra hle
  push_neg at hle
  simp only [sub_self, h0, add_zero] at h hle
  have hd : m c - m c' ≠ 0 := sub_ne_zero.mpr (hm.injective.ne hne)
  have hsum : μ (m c - m c') + μ (-(m c - m c')) ≥ 0 := by
    have : m c' - m c = -(m c - m c') := by ring
    rw [this] at hle
    linarith
  rcases lt_or_gt_of_ne hd with hlt | hgt
  · have := symmetric_sum_neg h0 hc hA2 (show 0 < -(m c - m c') by linarith)
    rw [neg_neg] at this
    linarith
  · have := symmetric_sum_neg h0 hc hA2 hgt
    linarith

noncomputable def mu (η lam x : ℝ) : ℝ := if 0 < x then η * x else η * lam * x

theorem mu_symmetric_sum (η lam x : ℝ) : mu η lam x + mu η lam (-x) = -(η * (lam - 1)) * |x| := by
  unfold mu
  rcases lt_trichotomy x 0 with h | h | h
  · rw [if_neg (not_lt.mpr h.le), if_pos (by linarith), abs_of_neg h]
    ring
  · subst h
    simp
  · rw [if_pos h, if_neg (by linarith), abs_of_pos h]
    ring

theorem mu_satisfies_A2 {η lam : ℝ} (hη : 0 < η) (hl : 1 < lam) :
    ∀ x y, 0 < x → x < y → mu η lam y + mu η lam (-y) < mu η lam x + mu η lam (-x) := by
  intro x y hx hxy
  rw [mu_symmetric_sum, mu_symmetric_sum, abs_of_pos hx, abs_of_pos (hx.trans hxy)]
  have : 0 < η * (lam - 1) := mul_pos hη (by linarith)
  nlinarith

theorem control_A2_needs_loss_aversion (x : ℝ) : mu 1 1 x + mu 1 1 (-x) = 0 := by
  rw [mu_symmetric_sum]
  ring

end GainLoss

section Lotteries

variable {ι : Type*}

theorem expected_gain_loss {η lam : ℝ} (s : Finset ι) (p x : ι → ℝ) :
    ∑ i ∈ s, ∑ j ∈ s, p i * p j * mu η lam (x i - x j)
      = -(η * (lam - 1)) / 2 * ∑ i ∈ s, ∑ j ∈ s, p i * p j * |x i - x j| := by
  have hc : ∑ i ∈ s, ∑ j ∈ s, p i * p j * mu η lam (x i - x j)
      = ∑ i ∈ s, ∑ j ∈ s, p j * p i * mu η lam (x j - x i) := Finset.sum_comm
  have h2 : ∑ i ∈ s, ∑ j ∈ s, p i * p j * mu η lam (x i - x j)
      + ∑ i ∈ s, ∑ j ∈ s, p j * p i * mu η lam (x j - x i)
      = -(η * (lam - 1)) * ∑ i ∈ s, ∑ j ∈ s, p i * p j * |x i - x j| := by
    rw [← sum_add_distrib, mul_sum]
    refine sum_congr rfl (fun i _ => ?_)
    rw [← sum_add_distrib, mul_sum]
    refine sum_congr rfl (fun j _ => ?_)
    have e : x j - x i = -(x i - x j) := by ring
    rw [e, mul_comm (p j) (p i), ← mul_add, mu_symmetric_sum]
    ring
  linarith

theorem expected_gain_loss_nonpos {η lam : ℝ} (hη : 0 < η) (hl : 1 < lam) (s : Finset ι)
    {p : ι → ℝ} (hp : ∀ i ∈ s, 0 ≤ p i) (x : ι → ℝ) :
    ∑ i ∈ s, ∑ j ∈ s, p i * p j * mu η lam (x i - x j) ≤ 0 := by
  rw [expected_gain_loss]
  have hk : -(η * (lam - 1)) / 2 ≤ 0 := by
    have : 0 < η * (lam - 1) := mul_pos hη (by linarith)
    linarith
  have hs : 0 ≤ ∑ i ∈ s, ∑ j ∈ s, p i * p j * |x i - x j| :=
    sum_nonneg (fun i hi => sum_nonneg (fun j hj => mul_nonneg (mul_nonneg (hp i hi) (hp j hj))
      (abs_nonneg _)))
  exact mul_nonpos_of_nonpos_of_nonneg hk hs

theorem linear_consumption_utility (a b : ℝ) (μ : ℝ → ℝ) (c r : ℝ) :
    (a * c + b + μ ((a * c + b) - (a * r + b))) - (a * r + b + μ ((a * r + b) - (a * r + b)))
      = (a * (c - r) + μ (a * (c - r))) - μ 0 := by
  have e1 : (a * c + b) - (a * r + b) = a * (c - r) := by ring
  rw [e1, sub_self]
  ring

end Lotteries

section Shopping

theorem pmin_lt_one_lt_pmax {η lam : ℝ} (hη : 0 < η) (hl : 1 < lam) :
    (1 + η) / (1 + η * lam) < 1 ∧ 1 < (1 + η * lam) / (1 + η) := by
  have h1 : 0 < 1 + η := by linarith
  have h2 : 1 + η < 1 + η * lam := by nlinarith
  exact ⟨(div_lt_one (by linarith)).mpr h2, (one_lt_div h1).mpr h2⟩

theorem attachment_effect {η lam q pM : ℝ} (hη : 0 < η) (hl : 1 < lam) :
    (q * (η * 0 - η * lam)) < (1 - pM) + (1 - q) * (η - η * lam * pM) - q * η * lam * (pM - 0)
      ↔ pM < 1 - (1 - q) * (η * (lam - 1) / (1 + η * lam)) := by
  have hd : 0 < 1 + η * lam := by nlinarith
  rw [← sub_pos, ← sub_pos (a := 1 - (1 - q) * (η * (lam - 1) / (1 + η * lam)))]
  have e : (1 - pM) + (1 - q) * (η - η * lam * pM) - q * η * lam * (pM - 0) - q * (η * 0 - η * lam)
      = (1 + η * lam) * (1 - (1 - q) * (η * (lam - 1) / (1 + η * lam)) - pM) := by
    field_simp
    ring
  rw [e]
  exact ⟨fun h => (pos_iff_pos_of_mul_pos h).mp hd, fun h => mul_pos hd h⟩

theorem comparison_effect {η lam pL pM : ℝ} (hη : 0 < η) (hl : 1 < lam) :
    (η * pL - η * lam) < (1 - pM) - η * lam * (pM - pL)
      ↔ pM < 1 + pL * (η * (lam - 1) / (1 + η * lam)) := by
  have hd : 0 < 1 + η * lam := by nlinarith
  rw [← sub_pos, ← sub_pos (a := 1 + pL * (η * (lam - 1) / (1 + η * lam)))]
  have e : (1 - pM) - η * lam * (pM - pL) - (η * pL - η * lam)
      = (1 + η * lam) * (1 + pL * (η * (lam - 1) / (1 + η * lam)) - pM) := by
    field_simp
    ring
  rw [e]
  exact ⟨fun h => (pos_iff_pos_of_mul_pos h).mp hd, fun h => mul_pos hd h⟩

theorem control_effects_need_loss_aversion (q pL : ℝ) :
    1 - (1 - q) * (1 * (1 - 1) / (1 + 1 * 1)) = 1 ∧ 1 + pL * (1 * (1 - 1) / (1 + 1 * 1)) = 1 := by
  constructor <;> ring

end Shopping

end KoszegiRabin2006
