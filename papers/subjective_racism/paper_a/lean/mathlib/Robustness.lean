import Mathlib

/-! # Row M21. What survives a change in the form of the engagement utility

A14 values engagement at `v − κ (c − y)²`, a benefit that falls in the class
distance `|c − y|` and is concave in it. Akerlof's benefit `(g + d)⁻¹` also
falls in distance but is convex in it. This file states what holds for any
benefit `Φ` of the class distance that does not rise in distance, and what
turns on its curvature. -/

namespace Robustness

noncomputable section

/-- The expected benefit to an observer of class `y` of a person drawn from a
group whose members have classes `x i` with weights `p i`. -/
def expBen {ι : Type*} (S : Finset ι) (p x : ι → ℝ) (Φ : ℝ → ℝ) (y : ℝ) : ℝ :=
  ∑ i ∈ S, p i * Φ |y - x i|

/-- The same group after every member's class rises by `t`. -/
def shifted {ι : Type*} (x : ι → ℝ) (t : ℝ) : ι → ℝ := fun i => x i + t

/-! ### (i) The direction at the two ends, for any benefit that falls in distance -/

/-- An observer at or above every member, after the shift, does not lose when
the group's classes rise. -/
theorem shift_up_above {ι : Type*} (S : Finset ι) (p x : ι → ℝ) (Φ : ℝ → ℝ)
    (hΦ : AntitoneOn Φ (Set.Ici 0)) (hp : ∀ i ∈ S, 0 ≤ p i) (y t : ℝ) (ht : 0 ≤ t)
    (habove : ∀ i ∈ S, x i + t ≤ y) :
    expBen S p x Φ y ≤ expBen S p (shifted x t) Φ y := by
  unfold expBen shifted
  apply Finset.sum_le_sum
  intro i hi
  have h := habove i hi
  rw [abs_of_nonneg (by linarith : (0:ℝ) ≤ y - x i),
    abs_of_nonneg (by linarith : (0:ℝ) ≤ y - (x i + t))]
  apply mul_le_mul_of_nonneg_left _ (hp i hi)
  exact hΦ (by simp only [Set.mem_Ici]; linarith) (by simp only [Set.mem_Ici]; linarith)
    (by linarith)

/-- An observer at or below every member, before the shift, does not gain. -/
theorem shift_up_below {ι : Type*} (S : Finset ι) (p x : ι → ℝ) (Φ : ℝ → ℝ)
    (hΦ : AntitoneOn Φ (Set.Ici 0)) (hp : ∀ i ∈ S, 0 ≤ p i) (y t : ℝ) (ht : 0 ≤ t)
    (hbelow : ∀ i ∈ S, y ≤ x i) :
    expBen S p (shifted x t) Φ y ≤ expBen S p x Φ y := by
  unfold expBen shifted
  apply Finset.sum_le_sum
  intro i hi
  have h := hbelow i hi
  rw [abs_sub_comm y (x i), abs_sub_comm y (x i + t),
    abs_of_nonneg (by linarith : (0:ℝ) ≤ x i - y),
    abs_of_nonneg (by linarith : (0:ℝ) ≤ x i + t - y)]
  apply mul_le_mul_of_nonneg_left _ (hp i hi)
  exact hΦ (by simp only [Set.mem_Ici]; linarith) (by simp only [Set.mem_Ici]; linarith)
    (by linarith)

/-- The refusal share. Among observers who all stand at or above every member
after the shift, those who refuse after the shift, by an expected benefit
below the threshold `D`, are no more than those who refused before. -/
theorem refusers_above {ι κ : Type*} [DecidableEq κ] (J : Finset κ) (yo : κ → ℝ)
    (S : Finset ι) (p x : ι → ℝ) (Φ : ℝ → ℝ) (hΦ : AntitoneOn Φ (Set.Ici 0))
    (hp : ∀ i ∈ S, 0 ≤ p i) (t D : ℝ) (ht : 0 ≤ t)
    (habove : ∀ j ∈ J, ∀ i ∈ S, x i + t ≤ yo j) :
    (J.filter (fun j => expBen S p (shifted x t) Φ (yo j) < D)).card ≤
      (J.filter (fun j => expBen S p x Φ (yo j) < D)).card := by
  apply Finset.card_le_card
  intro j hj
  rw [Finset.mem_filter] at hj ⊢
  exact ⟨hj.1, lt_of_le_of_lt (shift_up_above S p x Φ hΦ hp (yo j) t ht (habove j hj.1)) hj.2⟩

/-- Among observers who all stand at or below every member before the shift,
those who refuse before the shift are no more than those who refuse after. -/
theorem refusers_below {ι κ : Type*} [DecidableEq κ] (J : Finset κ) (yo : κ → ℝ)
    (S : Finset ι) (p x : ι → ℝ) (Φ : ℝ → ℝ) (hΦ : AntitoneOn Φ (Set.Ici 0))
    (hp : ∀ i ∈ S, 0 ≤ p i) (t D : ℝ) (ht : 0 ≤ t)
    (hbelow : ∀ j ∈ J, ∀ i ∈ S, yo j ≤ x i) :
    (J.filter (fun j => expBen S p x Φ (yo j) < D)).card ≤
      (J.filter (fun j => expBen S p (shifted x t) Φ (yo j) < D)).card := by
  apply Finset.card_le_card
  intro j hj
  rw [Finset.mem_filter] at hj ⊢
  exact ⟨hj.1, lt_of_le_of_lt (shift_up_below S p x Φ hΦ hp (yo j) t ht (hbelow j hj.1)) hj.2⟩

/-- Control for (i). Between the two ends the direction turns on the form.
Classes `0` and `10` with equal weight have standing `5`, and an observer at
`6` stands above it. A shift of `1` keeps the standing at or below `6`. The
mean squared distance falls from `26` to `25`, as M18 says for the quadratic
loss, while the mean of Akerlof's benefit `(1 + d)⁻¹` also falls, from
`12/70` to `1/6`, so that observer is worse off under that benefit. -/
theorem control_range_needs_quadratic :
    ((1 + 5 : ℝ)⁻¹ + (1 + 5)⁻¹) / 2 < ((1 + 6 : ℝ)⁻¹ + (1 + 4)⁻¹) / 2 ∧
      ((5 : ℝ) ^ 2 + 5 ^ 2) / 2 < (6 ^ 2 + 4 ^ 2) / 2 := by
  constructor <;> norm_num

/-! ### (ii) The dispersion, whose sign follows the curvature of the benefit -/

/-- Two points `a ≤ b` inside `[a', b']` with the same sum. A function convex
on a set holding `a'` and `b'` has `Φ a + Φ b ≤ Φ a' + Φ b'`. -/
theorem majorize {S : Set ℝ} {Φ : ℝ → ℝ} (hΦ : ConvexOn ℝ S Φ) {a b a' b' : ℝ}
    (ha' : a' ∈ S) (hb' : b' ∈ S) (h1 : a' ≤ a) (h2 : b ≤ b') (hab : a ≤ b)
    (hsum : a + b = a' + b') : Φ a + Φ b ≤ Φ a' + Φ b' := by
  rcases eq_or_lt_of_le (show a' ≤ b' by linarith) with h | h
  · have e1 : a = a' := by linarith
    have e2 : b = b' := by linarith
    rw [e1, e2]
  · have hd : 0 < b' - a' := by linarith
    have ht' : (b' - a) / (b' - a') * (b' - a') = b' - a := by field_simp
    generalize (b' - a) / (b' - a') = t at ht'
    have t0 : 0 ≤ t := by nlinarith
    have t1 : 0 ≤ 1 - t := by nlinarith
    have ea : t * a' + (1 - t) * b' = a := by linear_combination -ht'
    have eb : (1 - t) * a' + t * b' = b := by linear_combination ht' - hsum
    have k1 := hΦ.2 ha' hb' t0 t1 (by ring)
    have k2 := hΦ.2 ha' hb' t1 t0 (by ring)
    simp only [smul_eq_mul] at k1 k2
    rw [ea] at k1
    rw [eb] at k2
    linarith

/-- A benefit convex in distance. Two members at distances `D − s` and `D + s`
from the observer, as for an observer beyond both members of a pair centred
at distance `D`. A wider dispersion does not lower the mean benefit. -/
theorem spread_convex {Φ : ℝ → ℝ} (hΦ : ConvexOn ℝ (Set.Ici 0) Φ) (D s s' : ℝ)
    (hs : 0 ≤ s) (hss : s ≤ s') (hD : s' ≤ D) :
    (Φ (D - s) + Φ (D + s)) / 2 ≤ (Φ (D - s') + Φ (D + s')) / 2 := by
  have := majorize hΦ (a := D - s) (b := D + s) (a' := D - s') (b' := D + s')
    (by simp only [Set.mem_Ici]; linarith) (by simp only [Set.mem_Ici]; linarith)
    (by linarith) (by linarith) (by linarith) (by ring)
  linarith

/-- A benefit concave in distance. The same wider dispersion does not raise the
mean benefit. -/
theorem spread_concave {Φ : ℝ → ℝ} (hΦ : ConcaveOn ℝ (Set.Ici 0) Φ) (D s s' : ℝ)
    (hs : 0 ≤ s) (hss : s ≤ s') (hD : s' ≤ D) :
    (Φ (D - s') + Φ (D + s')) / 2 ≤ (Φ (D - s) + Φ (D + s)) / 2 := by
  have h := spread_convex (neg_convexOn_iff.mpr hΦ) D s s' hs hss hD
  simp only [Pi.neg_apply] at h
  linarith

/-- An observer at the centre of the pair is at distance `s` from both, and for
any benefit that falls in distance a wider dispersion does not raise the mean
benefit. -/
theorem spread_at_centre {Φ : ℝ → ℝ} (hΦ : AntitoneOn Φ (Set.Ici 0)) (s s' : ℝ)
    (hs : 0 ≤ s) (hss : s ≤ s') : (Φ s' + Φ s') / 2 ≤ (Φ s + Φ s) / 2 := by
  have := hΦ (show s ∈ Set.Ici (0:ℝ) from hs)
    (show s' ∈ Set.Ici (0:ℝ) by simp only [Set.mem_Ici]; linarith) hss
  linarith

/-! ### (ii) in general. A mean-preserving widening of a group, for any observer -/

/-- A widening of a group. Member `i`, of class `x i`, is replaced by the
classes `z i k` for `k` in `K i`, with weights `q i k` that sum to one and
average to `x i`. This is the expected benefit after the widening. -/
def widened {ι κ : Type*} (S : Finset ι) (K : ι → Finset κ) (p : ι → ℝ)
    (q z : ι → κ → ℝ) (Φ : ℝ → ℝ) (y : ℝ) : ℝ :=
  ∑ i ∈ S, p i * ∑ k ∈ K i, q i k * Φ |y - z i k|

/-- A benefit concave and falling in distance is concave in the other's
class, for any observer. -/
theorem concave_in_class {Φ : ℝ → ℝ} (hc : ConcaveOn ℝ (Set.Ici 0) Φ)
    (ha : AntitoneOn Φ (Set.Ici 0)) (y : ℝ) :
    ConcaveOn ℝ Set.univ (fun x : ℝ => Φ |y - x|) := by
  refine ⟨convex_univ, ?_⟩
  intro x1 _ x2 _ a b ha0 hb0 hab
  simp only [smul_eq_mul]
  have e : y - (a * x1 + b * x2) = a * (y - x1) + b * (y - x2) := by
    linear_combination (-y) * hab
  have h1 : |y - (a * x1 + b * x2)| ≤ a * |y - x1| + b * |y - x2| := by
    rw [e]
    calc |a * (y - x1) + b * (y - x2)| ≤ |a * (y - x1)| + |b * (y - x2)| := abs_add_le _ _
      _ = a * |y - x1| + b * |y - x2| := by
        rw [abs_mul, abs_mul, abs_of_nonneg ha0, abs_of_nonneg hb0]
  have h2 : Φ (a * |y - x1| + b * |y - x2|) ≤ Φ |y - (a * x1 + b * x2)| :=
    ha (Set.mem_Ici.mpr (abs_nonneg _))
      (show a * |y - x1| + b * |y - x2| ∈ Set.Ici (0:ℝ) from
        add_nonneg (mul_nonneg ha0 (abs_nonneg _)) (mul_nonneg hb0 (abs_nonneg _))) h1
  have h3 := hc.2 (Set.mem_Ici.mpr (abs_nonneg (y - x1)))
    (Set.mem_Ici.mpr (abs_nonneg (y - x2))) ha0 hb0 hab
  simp only [smul_eq_mul] at h3
  linarith

/-- A benefit concave in distance. Any mean-preserving widening of a group
does not raise the expected benefit to any observer. -/
theorem widen_concave {ι κ : Type*} (S : Finset ι) (K : ι → Finset κ) (p x : ι → ℝ)
    (q z : ι → κ → ℝ) (Φ : ℝ → ℝ) (hc : ConcaveOn ℝ (Set.Ici 0) Φ)
    (ha : AntitoneOn Φ (Set.Ici 0)) (hp : ∀ i ∈ S, 0 ≤ p i)
    (hq : ∀ i ∈ S, ∀ k ∈ K i, 0 ≤ q i k) (hq1 : ∀ i ∈ S, ∑ k ∈ K i, q i k = 1)
    (hmean : ∀ i ∈ S, ∑ k ∈ K i, q i k * z i k = x i) (y : ℝ) :
    widened S K p q z Φ y ≤ expBen S p x Φ y := by
  unfold widened expBen
  apply Finset.sum_le_sum
  intro i hi
  apply mul_le_mul_of_nonneg_left _ (hp i hi)
  have h := (concave_in_class hc ha y).le_map_sum (t := K i) (w := q i) (p := z i)
    (hq i hi) (hq1 i hi) (fun k _ => Set.mem_univ _)
  simp only [smul_eq_mul] at h
  rw [hmean i hi] at h
  exact h

/-- A benefit convex in distance is convex in the other's class on the classes
at or below the observer. -/
theorem convex_in_class_below {Φ : ℝ → ℝ} (hc : ConvexOn ℝ (Set.Ici 0) Φ) (y : ℝ) :
    ConvexOn ℝ (Set.Iic y) (fun x : ℝ => Φ |y - x|) := by
  refine ⟨convex_Iic y, ?_⟩
  intro x1 hx1 x2 hx2 a b ha0 hb0 hab
  simp only [Set.mem_Iic] at hx1 hx2
  simp only [smul_eq_mul]
  have e : y - (a * x1 + b * x2) = a * (y - x1) + b * (y - x2) := by
    linear_combination (-y) * hab
  have hm : 0 ≤ y - (a * x1 + b * x2) := by
    rw [e]; exact add_nonneg (mul_nonneg ha0 (by linarith)) (mul_nonneg hb0 (by linarith))
  rw [abs_of_nonneg (by linarith : (0:ℝ) ≤ y - x1),
    abs_of_nonneg (by linarith : (0:ℝ) ≤ y - x2), abs_of_nonneg hm, e]
  have h := hc.2 (show y - x1 ∈ Set.Ici (0:ℝ) by simp only [Set.mem_Ici]; linarith)
    (show y - x2 ∈ Set.Ici (0:ℝ) by simp only [Set.mem_Ici]; linarith) ha0 hb0 hab
  simpa only [smul_eq_mul] using h

/-- The same on the classes at or above the observer. -/
theorem convex_in_class_above {Φ : ℝ → ℝ} (hc : ConvexOn ℝ (Set.Ici 0) Φ) (y : ℝ) :
    ConvexOn ℝ (Set.Ici y) (fun x : ℝ => Φ |y - x|) := by
  refine ⟨convex_Ici y, ?_⟩
  intro x1 hx1 x2 hx2 a b ha0 hb0 hab
  simp only [Set.mem_Ici] at hx1 hx2
  simp only [smul_eq_mul]
  have e : a * x1 + b * x2 - y = a * (x1 - y) + b * (x2 - y) := by
    linear_combination y * hab
  have hm : 0 ≤ a * x1 + b * x2 - y := by
    rw [e]; exact add_nonneg (mul_nonneg ha0 (by linarith)) (mul_nonneg hb0 (by linarith))
  rw [abs_sub_comm y x1, abs_sub_comm y x2, abs_sub_comm y (a * x1 + b * x2),
    abs_of_nonneg (by linarith : (0:ℝ) ≤ x1 - y),
    abs_of_nonneg (by linarith : (0:ℝ) ≤ x2 - y), abs_of_nonneg hm, e]
  have h := hc.2 (show x1 - y ∈ Set.Ici (0:ℝ) by simp only [Set.mem_Ici]; linarith)
    (show x2 - y ∈ Set.Ici (0:ℝ) by simp only [Set.mem_Ici]; linarith) ha0 hb0 hab
  simpa only [smul_eq_mul] using h

/-- A benefit convex in distance. A mean-preserving widening whose classes all
stay at or below the observer does not lower the expected benefit to that
observer. -/
theorem widen_convex_above {ι κ : Type*} (S : Finset ι) (K : ι → Finset κ) (p x : ι → ℝ)
    (q z : ι → κ → ℝ) (Φ : ℝ → ℝ) (hc : ConvexOn ℝ (Set.Ici 0) Φ)
    (hp : ∀ i ∈ S, 0 ≤ p i)
    (hq : ∀ i ∈ S, ∀ k ∈ K i, 0 ≤ q i k) (hq1 : ∀ i ∈ S, ∑ k ∈ K i, q i k = 1)
    (hmean : ∀ i ∈ S, ∑ k ∈ K i, q i k * z i k = x i) (y : ℝ)
    (habove : ∀ i ∈ S, ∀ k ∈ K i, z i k ≤ y) :
    expBen S p x Φ y ≤ widened S K p q z Φ y := by
  unfold widened expBen
  apply Finset.sum_le_sum
  intro i hi
  apply mul_le_mul_of_nonneg_left _ (hp i hi)
  have h := (convex_in_class_below hc y).map_sum_le (t := K i) (w := q i) (p := z i)
    (hq i hi) (hq1 i hi) (fun k hk => by simp only [Set.mem_Iic]; exact habove i hi k hk)
  simp only [smul_eq_mul] at h
  rw [hmean i hi] at h
  exact h

/-- The same for an observer at or below every class of the widening. -/
theorem widen_convex_below {ι κ : Type*} (S : Finset ι) (K : ι → Finset κ) (p x : ι → ℝ)
    (q z : ι → κ → ℝ) (Φ : ℝ → ℝ) (hc : ConvexOn ℝ (Set.Ici 0) Φ)
    (hp : ∀ i ∈ S, 0 ≤ p i)
    (hq : ∀ i ∈ S, ∀ k ∈ K i, 0 ≤ q i k) (hq1 : ∀ i ∈ S, ∑ k ∈ K i, q i k = 1)
    (hmean : ∀ i ∈ S, ∑ k ∈ K i, q i k * z i k = x i) (y : ℝ)
    (hbelow : ∀ i ∈ S, ∀ k ∈ K i, y ≤ z i k) :
    expBen S p x Φ y ≤ widened S K p q z Φ y := by
  unfold widened expBen
  apply Finset.sum_le_sum
  intro i hi
  apply mul_le_mul_of_nonneg_left _ (hp i hi)
  have h := (convex_in_class_above hc y).map_sum_le (t := K i) (w := q i) (p := z i)
    (hq i hi) (hq1 i hi) (fun k hk => by simp only [Set.mem_Ici]; exact hbelow i hi k hk)
  simp only [smul_eq_mul] at h
  rw [hmean i hi] at h
  exact h

/-- Control for the convex case. Akerlof's benefit `(1 + d)⁻¹` is convex in
distance, yet an observer inside the widening loses. One member at `0`,
widened to `−1` and `1` with equal weight, gives an observer at `0` a mean
benefit of `1/2` in place of `1`. -/
theorem control_widen_inside :
    ((1 / 2 : ℝ) * (1 + 1)⁻¹ + (1 / 2) * (1 + 1)⁻¹) < (1 + 0 : ℝ)⁻¹ := by
  norm_num

/-! ### (iii) The two forms -/

/-- A14's benefit `v − κ d²` is concave in distance, so (ii) gives the dispersion
the sign of a penalty for every observer. -/
theorem quadratic_concave (v κ : ℝ) (hκ : 0 ≤ κ) :
    ConcaveOn ℝ (Set.Ici 0) (fun d : ℝ => v - κ * d ^ 2) := by
  refine ⟨convex_Ici 0, ?_⟩
  intro x _ y _ a b ha hb hab
  simp only [smul_eq_mul]
  have hb' : b = 1 - a := by linarith
  subst hb'
  nlinarith [mul_nonneg (mul_nonneg ha hb) (mul_nonneg hκ (sq_nonneg (x - y)))]

/-- A14's benefit falls in distance. -/
theorem quadratic_antitone (v κ : ℝ) (hκ : 0 ≤ κ) :
    AntitoneOn (fun d : ℝ => v - κ * d ^ 2) (Set.Ici 0) := by
  intro x hx y _ hxy
  simp only [Set.mem_Ici] at hx
  have : x ^ 2 ≤ y ^ 2 := by nlinarith
  simp only
  nlinarith

/-- Akerlof's benefit `(g + d)⁻¹` is convex in distance, so (ii) gives the
dispersion the sign of a gain for an observer beyond both members. -/
theorem gravity_convex (g : ℝ) (hg : 0 < g) :
    ConvexOn ℝ (Set.Ici 0) (fun d : ℝ => (g + d)⁻¹) := by
  have h := (convexOn_zpow (𝕜 := ℝ) (-1)).translate_right g
  have h2 : ConvexOn ℝ (Set.Ici 0) ((fun x : ℝ => x ^ (-1 : ℤ)) ∘ fun z => g + z) :=
    h.subset (fun z hz => by
      simp only [Set.mem_Ici] at hz
      show (0 : ℝ) < g + z
      linarith) (convex_Ici 0)
  convert h2 using 1
  funext d
  simp

/-- Akerlof's benefit falls in distance. -/
theorem gravity_antitone (g : ℝ) (hg : 0 < g) :
    AntitoneOn (fun d : ℝ => (g + d)⁻¹) (Set.Ici 0) := by
  intro x hx y _ hxy
  simp only [Set.mem_Ici] at hx
  exact inv_anti₀ (by linarith) (by linarith)

/-- Control for (ii). At distance `3` a dispersion of `1` raises the mean of
`(1 + d)⁻¹` from `1/4` to `4/15`, and lowers the mean of `−d²` from `−9` to
`−10`. -/
theorem control_spread_sign :
    ((1 + 3 : ℝ)⁻¹ + (1 + 3)⁻¹) / 2 < ((1 + 2 : ℝ)⁻¹ + (1 + 4)⁻¹) / 2 ∧
      (-(2 : ℝ) ^ 2 + -(4 ^ 2)) / 2 < (-(3 : ℝ) ^ 2 + -(3 ^ 2)) / 2 := by
  constructor <;> norm_num

end

end Robustness
