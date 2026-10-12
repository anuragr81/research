import Mathlib.Analysis.Convex.Function
import Mathlib.Tactic

namespace KahnemanTversky1979

section ValueFunction

theorem concave_gains_subadditive {v : ℝ → ℝ} (hv : ConcaveOn ℝ (Set.Ici 0) v) (h0 : v 0 = 0)
    {a b : ℝ} (ha : 0 ≤ a) (hb : 0 ≤ b) : v (a + b) ≤ v a + v b := by
  rcases eq_or_lt_of_le (add_nonneg ha hb) with hs | hs
  · have ha0 : a = 0 := by linarith
    have hb0 : b = 0 := by linarith
    subst ha0 hb0
    simp [h0]
  · have key : ∀ t : ℝ, 0 ≤ t → t ≤ 1 → t * v (a + b) ≤ v (t * (a + b)) := by
      intro t ht0 ht1
      have := hv.2 (Set.mem_Ici.mpr (le_of_lt hs)) (Set.mem_Ici.mpr le_rfl) ht0
        (by linarith : 0 ≤ 1 - t) (by ring)
      simp only [smul_eq_mul, mul_zero, add_zero, h0] at this
      linarith
    have h1 := key (a / (a + b)) (div_nonneg ha hs.le) ((div_le_one hs).mpr (by linarith))
    have h2 := key (b / (a + b)) (div_nonneg hb hs.le) ((div_le_one hs).mpr (by linarith))
    have e1 : a / (a + b) * (a + b) = a := by field_simp
    have e2 : b / (a + b) * (a + b) = b := by field_simp
    rw [e1] at h1
    rw [e2] at h2
    have hsum : a / (a + b) * v (a + b) + b / (a + b) * v (a + b) = v (a + b) := by
      field_simp
      ring
    linarith

theorem convex_losses_superadditive {v : ℝ → ℝ} (hv : ConvexOn ℝ (Set.Iic 0) v) (h0 : v 0 = 0)
    {a b : ℝ} (ha : a ≤ 0) (hb : b ≤ 0) : v a + v b ≤ v (a + b) := by
  have hneg : ConcaveOn ℝ (Set.Ici 0) (fun t => -v (-t)) := by
    have hc : ConvexOn ℝ (Set.Ici 0) (fun t => v (-t)) := by
      refine ⟨convex_Ici 0, fun x hx y hy s t hs ht hst => ?_⟩
      have hx' : -x ∈ Set.Iic (0 : ℝ) := by simp at hx ⊢; linarith
      have hy' : -y ∈ Set.Iic (0 : ℝ) := by simp at hy ⊢; linarith
      have := hv.2 hx' hy' hs ht hst
      simp only [smul_eq_mul] at this ⊢
      have e : -(s * x + t * y) = s * -x + t * -y := by ring
      rw [e]
      exact this
    exact hc.neg
  have := concave_gains_subadditive hneg (by simp [h0]) (neg_nonneg.mpr ha) (neg_nonneg.mpr hb)
  simp only [neg_add_rev, neg_neg] at this
  rw [add_comm b a] at this
  linarith

theorem loss_aversion_from_symmetric_bets {v : ℝ → ℝ} (h0 : v 0 = 0)
    (hbets : ∀ x y, 0 ≤ y → y < x → v x + v (-x) < v y + v (-y)) {x : ℝ} (hx : 0 < x) :
    v x < -v (-x) := by
  have := hbets x 0 le_rfl hx
  simp [h0] at this
  linarith

theorem control_loss_aversion_needs_aversion_to_bets (x : ℝ) : (fun t : ℝ => t) x = -(fun t : ℝ => t) (-x) := by
  simp

end ValueFunction

section Weights

theorem subcertainty_from_allais {v2400 v2500 p66 p33 p34 : ℝ} (hv : 0 < v2400)
    (h1 : v2400 > p66 * v2400 + p33 * v2500) (h2 : p33 * v2500 > p34 * v2400) :
    p66 + p34 < 1 := by
  have : (1 - p66) * v2400 > p34 * v2400 := by nlinarith
  have := lt_of_mul_lt_mul_right this hv.le
  linarith

theorem overweighting_from_lottery {v : ℝ → ℝ} (hv : StrictConcaveOn ℝ (Set.Ici 0) v) (h0 : v 0 = 0)
    (hpos : 0 < v 5000) {w : ℝ} (hlottery : w * v 5000 ≥ v 5) : 0.001 < w := by
  have hc : 0.001 * v 5000 < v 5 := by
    have := hv.2 (Set.mem_Ici.mpr (by norm_num : (0 : ℝ) ≤ 5000)) (Set.mem_Ici.mpr le_rfl)
      (by norm_num : (5000 : ℝ) ≠ 0) (by norm_num : (0 : ℝ) < 0.001) (by norm_num : (0 : ℝ) < 0.999)
      (by norm_num)
    simp only [smul_eq_mul, mul_zero, add_zero, h0] at this
    norm_num at this ⊢
    linarith
  by_contra h
  push_neg at h
  nlinarith

theorem equation_two_rewrites (v_x v_y w : ℝ) : v_y + w * (v_x - v_y) = w * v_x + (1 - w) * v_y := by
  ring

theorem control_equations_differ_under_subcertainty :
    (1 : ℝ) + 0.4 * (2 - 1) ≠ 0.4 * 2 + 0.4 * 1 := by
  norm_num

end Weights

section Insurance

theorem eu_prefers_probabilistic_insurance {u : ℝ → ℝ} {S : Set ℝ} (hu : StrictConcaveOn ℝ S u)
    {w y p r : ℝ} (hw : w ∈ S) (hy : w - y ∈ S) (hy0 : y ≠ 0) (hr0 : 0 < r) (hr1 : r < 1)
    (huw : u w = 1) (huy : u (w - y) = 1 - p) : 1 - r * p < u (w - r * y) := by
  have key := hu.2 hy hw (by intro h; apply hy0; linarith) hr0 (by linarith : 0 < 1 - r) (by ring)
  simp only [smul_eq_mul] at key
  have e : r * (w - y) + (1 - r) * w = w - r * y := by ring
  rw [e, huw, huy] at key
  linarith

theorem control_probabilistic_needs_strict_concavity (w y p r : ℝ) (huy : w - y = 1 - p) (huw : w = 1) :
    ¬ (1 - r * p < (fun t : ℝ => t) (w - r * y)) := by
  simp only
  intro h
  have : y = p := by linarith
  subst this
  linarith

end Insurance

section ReferenceShift

theorem unadapted_loss_accepts_gamble {v : ℝ → ℝ} {wp wq x y z : ℝ} (hwp : 0 < wp) (hwq : 0 ≤ wq)
    (hsub : wp + wq < 1) (hacc : wp * v x + wq * v (-y) = 0)
    (hgain : v x - v z ≤ v (x - z)) (hloss : v (-y) + v (-z) ≤ v (-y - z))
    (haversion : v (-z) < -v z) (hneg : v (-z) < 0) :
    v (-z) < wp * v (x - z) + wq * v (-y - z) := by
  have h1 : wp * (v x - v z) ≤ wp * v (x - z) := mul_le_mul_of_nonneg_left hgain hwp.le
  have h2 : wq * (v (-y) + v (-z)) ≤ wq * v (-y - z) := mul_le_mul_of_nonneg_left hloss hwq
  have h3 : wp * v (-z) < wp * (-v z) := mul_lt_mul_of_pos_left haversion hwp
  have h4 : v (-z) < (wp + wq) * v (-z) := by nlinarith
  nlinarith

theorem unadapted_loss_from_shape {v : ℝ → ℝ} (hg : ConcaveOn ℝ (Set.Ici 0) v)
    (hl : ConvexOn ℝ (Set.Iic 0) v) (h0 : v 0 = 0) {x y z : ℝ} (hxz : z < x) (hz : 0 ≤ z)
    (hy : 0 ≤ y) : v x - v z ≤ v (x - z) ∧ v (-y) + v (-z) ≤ v (-y - z) := by
  constructor
  · have := concave_gains_subadditive hg h0 (by linarith : 0 ≤ x - z) hz
    rw [show x - z + z = x by ring] at this
    linarith
  · have := convex_losses_superadditive hl h0 (by linarith : -y ≤ 0) (by linarith : -z ≤ 0)
    rw [show -y + -z = -y - z by ring] at this
    exact this

theorem control_gamble_needs_subcertainty {v : ℝ → ℝ} (hvz : v (-1) = -2) (hv1 : v 1 = 1) :
    ¬ (v (-1) < (1 / 2) * (v 1 - v 1) + (1 / 2) * (v (-1) + v (-1))) := by
  rw [hvz, hv1]
  norm_num

end ReferenceShift

end KahnemanTversky1979
