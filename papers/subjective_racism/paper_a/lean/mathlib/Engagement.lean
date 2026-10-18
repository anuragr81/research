import Mathlib

/-! Row M18. The engagement decision, and the discrimination-rate function
derived from it.

An observer of class `y` meets a person whose class is drawn from the mixture
of the within-group priors `p g` by the group weights `w`, the posterior on
group of M2 and M3. The engagement utility is `v − κ·(c − y)²`, so the
observer engages exactly when the expected squared distance is at most the
engagement threshold `D = (v − ū)/κ`. The expected squared distance to a
group is the squared gap between the group's standing and the observer's own
class, plus the group's dispersion. -/

namespace Engagement

variable {G C ι : Type*} [Fintype G] [Fintype C]

/-- The standing of group `g`, the mean class under the prior `p g`, with `x c`
the class value of `c`. -/
noncomputable def standing (p : G → C → ℝ) (x : C → ℝ) (g : G) : ℝ := ∑ c, p g c * x c

/-- The dispersion of group `g`, the within-group variance of class, named `spread` in the code. -/
noncomputable def spread (p : G → C → ℝ) (x : C → ℝ) (g : G) : ℝ :=
  ∑ c, p g c * (x c - standing p x g) ^ 2

/-- The expected squared distance from an observer of class `y` to a person
whose class is drawn from the mixture of the priors by the weights `w`. -/
noncomputable def expDist (p : G → C → ℝ) (x : C → ℝ) (w : G → ℝ) (y : ℝ) : ℝ :=
  ∑ c, (∑ g, p g c * w g) * (x c - y) ^ 2

/-- The second moment of a distribution about `y` is the variance plus the
squared gap between the mean and `y`. -/
theorem second_moment (q : C → ℝ) (x : C → ℝ) (y : ℝ) (hq : ∑ c, q c = 1) :
    ∑ c, q c * (x c - y) ^ 2 =
      (∑ c, q c * x c - y) ^ 2 + ∑ c, q c * (x c - ∑ c', q c' * x c') ^ 2 := by
  have h0 : ∑ c, q c * (x c - ∑ c', q c' * x c') = 0 := by
    simp_rw [mul_sub]
    rw [Finset.sum_sub_distrib, ← Finset.sum_mul, hq, one_mul, sub_self]
  have h : ∀ c, q c * (x c - y) ^ 2 =
      q c * (x c - ∑ c', q c' * x c') ^ 2 +
        2 * ((∑ c', q c' * x c') - y) * (q c * (x c - ∑ c', q c' * x c')) +
        ((∑ c', q c' * x c') - y) ^ 2 * q c := by
    intro c; ring
  rw [Finset.sum_congr rfl fun c _ => h c, Finset.sum_add_distrib, Finset.sum_add_distrib,
    ← Finset.mul_sum, h0, mul_zero, add_zero, ← Finset.mul_sum, hq, mul_one]
  ring

/-- Row M18 (i). Under priors that each sum to one, the expected squared
distance to the mixture is the weighted sum over groups of the squared gap to
the group's standing plus the group's dispersion. -/
theorem expDist_decomp (p : G → C → ℝ) (x : C → ℝ) (w : G → ℝ) (y : ℝ)
    (hp : ∀ g, ∑ c, p g c = 1) :
    expDist p x w y = ∑ g, w g * ((standing p x g - y) ^ 2 + spread p x g) := by
  unfold expDist
  simp_rw [Finset.sum_mul]
  rw [Finset.sum_comm]
  refine Finset.sum_congr rfl fun g _ => ?_
  rw [show ∑ c, p g c * w g * (x c - y) ^ 2 = w g * ∑ c, p g c * (x c - y) ^ 2 by
    rw [Finset.mul_sum]; refine Finset.sum_congr rfl fun c _ => by ring]
  rw [second_moment (p g) x y (hp g)]
  rfl

/-- Row M18 (ii). When the person is read as group `O` with certainty, the
expected squared distance is the squared gap to `O`'s standing plus `O`'s
dispersion. -/
theorem expDist_certain [DecidableEq G] (p : G → C → ℝ) (x : C → ℝ) (O : G) (y : ℝ)
    (hp : ∀ g, ∑ c, p g c = 1) :
    expDist p x (fun g => if g = O then 1 else 0) y =
      (standing p x O - y) ^ 2 + spread p x O := by
  rw [expDist_decomp p x _ y hp]
  simp [Finset.sum_ite_eq']

/-- The observer engages when the expected squared distance is at most the
engagement threshold `D`. -/
def engages (D e : ℝ) : Prop := e ≤ D

/-- The threshold rule from the utility. With sensitivity `κ > 0`, the utility
`v − κ·e` is at least the outside option `u` exactly when `e ≤ (v − u)/κ`. -/
theorem engages_iff (v κ u e : ℝ) (hκ : 0 < κ) : u ≤ v - κ * e ↔ engages ((v - u) / κ) e := by
  unfold engages
  rw [le_div_iff₀ hκ]
  constructor <;> intro h <;> linarith

/-- Row M18 (ii). Read as `O` with certainty, the observer refuses exactly when
the squared gap to `O`'s standing plus `O`'s dispersion exceeds the threshold. -/
theorem refusal_iff [DecidableEq G] (p : G → C → ℝ) (x : C → ℝ) (O : G) (y D : ℝ)
    (hp : ∀ g, ∑ c, p g c = 1) :
    ¬ engages D (expDist p x (fun g => if g = O then 1 else 0) y) ↔
      D < (standing p x O - y) ^ 2 + spread p x O := by
  rw [expDist_certain p x O y hp]; unfold engages; exact not_le

/-- The discrimination-rate function derived from the threshold. Among the
observers `J` with classes `y`, the share who refuse a person read as the
outsider group with certainty, as a function of that group's standing `m`
and dispersion `s`. -/
noncomputable def refusalShare [DecidableEq ι] (J : Finset ι) (y : ι → ℝ) (s D m : ℝ) : ℝ :=
  ((J.filter fun j => D < (m - y j) ^ 2 + s).card : ℝ) / J.card

/-- Row M18 (iii), graded discrimination. On the range of standings at or
below every observer's class, the derived discrimination-rate function does
not rise in the standing. -/
theorem refusalShare_antitone [DecidableEq ι] (J : Finset ι) (y : ι → ℝ) (s D : ℝ)
    {m m' : ℝ} (hmm : m ≤ m') (hJ : ∀ j ∈ J, m' ≤ y j) :
    refusalShare J y s D m' ≤ refusalShare J y s D m := by
  unfold refusalShare
  apply div_le_div_of_nonneg_right _ (Nat.cast_nonneg _)
  exact_mod_cast Finset.card_le_card fun j hj => by
    rw [Finset.mem_filter] at hj ⊢
    refine ⟨hj.1, lt_of_lt_of_le hj.2 ?_⟩
    have := hJ j hj.1
    nlinarith

/-- Control for (iii). An observer below the standing refuses more as the
standing rises. One observer of class `0`, dispersion `0`, threshold `1`. At a
standing of `1` the observer engages, and at `2` the observer refuses. -/
theorem control_observer_below :
    refusalShare ({0} : Finset (Fin 1)) (fun _ => 0) 0 1 1 = 0 ∧
      refusalShare ({0} : Finset (Fin 1)) (fun _ => 0) 0 1 2 = 1 := by
  constructor
  · unfold refusalShare
    rw [Finset.filter_eq_empty_iff.mpr (by intro j _; norm_num)]
    simp
  · unfold refusalShare
    rw [Finset.filter_true_of_mem (by intro j _; norm_num)]
    simp

/-- Row M18 (iii), the other direction. On the range of standings at or above
every observer's class, the derived discrimination-rate function does not
fall in the standing. -/
theorem refusalShare_monotone_above [DecidableEq ι] (J : Finset ι) (y : ι → ℝ) (s D : ℝ)
    {m m' : ℝ} (hmm : m ≤ m') (hJ : ∀ j ∈ J, y j ≤ m) :
    refusalShare J y s D m ≤ refusalShare J y s D m' := by
  unfold refusalShare
  apply div_le_div_of_nonneg_right _ (Nat.cast_nonneg _)
  exact_mod_cast Finset.card_le_card fun j hj => by
    rw [Finset.mem_filter] at hj ⊢
    refine ⟨hj.1, lt_of_lt_of_le hj.2 ?_⟩
    have := hJ j hj.1
    nlinarith

/-- The derived discrimination-rate function takes values in `[0, 1]`. -/
theorem refusalShare_mem [DecidableEq ι] (J : Finset ι) (y : ι → ℝ) (s D m : ℝ) :
    0 ≤ refusalShare J y s D m ∧ refusalShare J y s D m ≤ 1 := by
  unfold refusalShare
  constructor
  · positivity
  · rcases J.eq_empty_or_nonempty with h | h
    · simp [h]
    · rw [div_le_one (by exact_mod_cast h.card_pos)]
      exact_mod_cast Finset.card_filter_le _ _

/-- Row M18 (iv), the salience effect. With weight `s` on the outsider group and
`1 − s` on the host, the expected squared distance is affine in `s`, with slope
the outsider's expected distance less the host's. It rises in `s` exactly when
the outsider's exceeds the host's. -/
theorem salience_slope (eO eI s : ℝ) :
    (1 - s) * eI + s * eO = eI + s * (eO - eI) := by ring

/-- Row M18 (iv). A larger weight on the outsider group raises the expected
squared distance when the outsider's expected distance exceeds the host's, and
lowers it when the host's exceeds the outsider's. -/
theorem salience_effect {eO eI s s' : ℝ} (hs : s ≤ s') :
    (eI < eO → (1 - s) * eI + s * eO ≤ (1 - s') * eI + s' * eO) ∧
      (eO < eI → (1 - s') * eI + s' * eO ≤ (1 - s) * eI + s * eO) := by
  constructor <;> intro h <;> nlinarith

/-- Control for (iv). With equal expected distances the weight does nothing. -/
theorem control_equal_distances (e s s' : ℝ) :
    (1 - s) * e + s * e = (1 - s') * e + s' * e := by ring

/-- Row M18 (iv), the two-group posterior. The expected squared distance under
weights `(1 − s, s)` on host `I` and outsider `O` is the salience mixture of the
two groups' expected distances. -/
theorem expDist_two_groups (p : Bool → C → ℝ) (x : C → ℝ) (s y : ℝ)
    (hp : ∀ g, ∑ c, p g c = 1) :
    expDist p x (fun g => if g then s else 1 - s) y =
      (1 - s) * ((standing p x false - y) ^ 2 + spread p x false) +
        s * ((standing p x true - y) ^ 2 + spread p x true) := by
  rw [expDist_decomp p x _ y hp]
  simp
  ring

end Engagement
