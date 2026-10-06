set_option linter.unusedSimpArgs false

namespace SchroyenTreich

theorem two_mul_int (x : Int) : 2 * x = x + x := by omega

theorem three_mul_int (x : Int) : 3 * x = x + x + x := by omega

theorem four_mul_int (x : Int) : 4 * x = x + x + x + x := by omega

macro "poly" : tactic =>
  `(tactic| (try simp only [two_mul_int, three_mul_int, four_mul_int]
             try simp only [Int.mul_sub, Int.sub_mul, Int.mul_add, Int.add_mul, Int.mul_assoc,
               Int.mul_comm, Int.mul_left_comm, Int.neg_mul, Int.mul_neg, Int.neg_neg,
               Int.mul_one, Int.one_mul, Int.mul_zero, Int.zero_mul]
             all_goals omega))

theorem pos_mul_iff (c g : Int) (hc : 0 < c) : 0 < c * g ↔ 0 < g := by
  constructor
  · intro h
    exact Int.lt_of_mul_lt_mul_left (by simpa using h) (Int.le_of_lt hc)
  · intro h
    exact Int.mul_pos hc h

theorem neg_mul_iff (c g : Int) (hc : c < 0) : c * g < 0 ↔ 0 < g := by
  have e : c * g = -((-c) * g) := by poly
  have i := pos_mul_iff (-c) g (by omega)
  constructor
  · intro h
    exact i.mp (by omega)
  · intro h
    have := i.mpr h
    omega

theorem sq_pos_of_ne (t : Int) (ht : t ≠ 0) : 0 < t * t := by
  rcases Int.lt_or_gt_of_ne ht with h | h
  · exact Int.mul_pos_of_neg_of_neg h h
  · exact Int.mul_pos h h

theorem sq_nonneg_int (t : Int) : 0 ≤ t * t := by
  rcases Int.lt_or_le t 0 with h | h
  · exact Int.le_of_lt (Int.mul_pos_of_neg_of_neg h h)
  · exact Int.mul_nonneg h h

theorem sq_lt_sq_iff (k n : Int) (hk : 0 ≤ k) (hn : 0 ≤ n) : k * k < n * n ↔ k < n := by
  constructor
  · intro h
    rcases Int.lt_or_le k n with hkn | hkn
    · exact hkn
    · have := Int.mul_le_mul hkn hkn hn hk
      omega
  · intro h
    have h1 : k * k ≤ k * n := Int.mul_le_mul_of_nonneg_left (Int.le_of_lt h) hk
    have h2 : k * n < n * n := Int.mul_lt_mul_of_pos_right h (by omega)
    omega

def uK (z : Int) : Int := if z ≤ 0 then 2 * z else z

def privilege (u : Int → Int) (w x q D r : Int) : Int := D * u (w - x) + q * r

def ability (u c : Int → Int) (w x q D r : Int) : Int := q * u (w + r) + (D - q) * u w - D * c x

def rentSeeking (u : Int → Int) (w x q D r : Int) : Int :=
  q * u (w + r - x) + (D - q) * u (w - x)

theorem privilege_rent_margin (u : Int → Int) (w x q D r : Int) :
    privilege u w x q D (r + 1) - privilege u w x q D r = q := by
  unfold privilege
  simp only [Int.mul_add, Int.mul_one]
  omega

theorem privilege_cost_margin_depends_on_wealth :
    privilege uK 0 0 1 1 0 - privilege uK 0 1 1 1 0
      ≠ privilege uK 2 0 1 1 0 - privilege uK 2 1 1 1 0 := by
  decide

theorem ability_cost_margin (u c : Int → Int) (w x q D r : Int) :
    ability u c w x q D r - ability u c w (x + 1) q D r = D * c (x + 1) - D * c x := by
  unfold ability
  omega

theorem ability_rent_margin_depends_on_wealth :
    ability uK (fun _ => 0) (-2) 0 1 1 1 - ability uK (fun _ => 0) (-2) 0 1 1 0
      ≠ ability uK (fun _ => 0) 2 0 1 1 1 - ability uK (fun _ => 0) 2 0 1 1 0 := by
  decide

theorem rentSeeking_rent_margin_depends_on_wealth :
    rentSeeking uK (-2) 0 1 1 1 - rentSeeking uK (-2) 0 1 1 0
      ≠ rentSeeking uK 2 0 1 1 1 - rentSeeking uK 2 0 1 1 0 := by
  decide

theorem rentSeeking_cost_margin_depends_on_wealth :
    rentSeeking uK 0 0 1 2 1 - rentSeeking uK 0 1 1 2 1
      ≠ rentSeeking uK 3 0 1 2 1 - rentSeeking uK 3 1 1 2 1 := by
  decide

def gap (A P k n : Int) : Int := 2 * A * (n * n - k * k) - P * (n * n)

def Thm3 (A P k n : Int) : Prop := 0 < gap A P k n

def IsA (u1 u2 A d : Int) : Prop := A * u1 = -u2 * d

def IsP (u2 u3 P d : Int) : Prop := P * u2 = -u3 * d

def Thm3U (u1 u2 u3 k n : Int) : Prop := u1 * u3 * (n * n) < 2 * (u2 * u2) * (n * n - k * k)

theorem gap_alt (A P k n : Int) : gap A P k n = (2 * A - P) * (n * n) - 2 * A * (k * k) := by
  unfold gap
  poly

theorem thm3_iff_alt (A P k n : Int) :
    Thm3 A P k n ↔ 2 * A * (k * k) < (2 * A - P) * (n * n) := by
  unfold Thm3
  rw [gap_alt]
  constructor
  · intro h
    omega
  · intro h
    omega

theorem thm3U_iff_thm3 (u1 u2 u3 A P d k n : Int)
    (hu1 : 0 < u1) (hu2 : u2 < 0) (hd : 0 < d)
    (hA : IsA u1 u2 A d) (hP : IsP u2 u3 P d) :
    Thm3U u1 u2 u3 k n ↔ Thm3 A P k n := by
  unfold IsA at hA
  unfold IsP at hP
  unfold Thm3U Thm3
  have e1 : u1 * (-u2) * gap A P k n
      = 2 * (A * u1) * (-u2) * (n * n - k * k) + (P * u2) * u1 * (n * n) := by
    unfold gap
    poly
  rw [hA, hP] at e1
  have e2 : 2 * (-u2 * d) * (-u2) * (n * n - k * k) + (-u3 * d) * u1 * (n * n)
      = d * (2 * (u2 * u2) * (n * n - k * k) - u1 * u3 * (n * n)) := by
    poly
  rw [e2] at e1
  have hc : 0 < u1 * (-u2) := Int.mul_pos hu1 (by omega)
  have i1 := pos_mul_iff (u1 * (-u2)) (gap A P k n) hc
  have i2 := pos_mul_iff d (2 * (u2 * u2) * (n * n - k * k) - u1 * u3 * (n * n)) hd
  rw [e1] at i1
  constructor
  · intro h
    exact i1.mp (i2.mpr (by omega))
  · intro h
    have := i2.mp (i1.mpr h)
    omega

def qf (v1 v2 h11 h12 h22 : Int) : Int := v1 * (h11 * v1 + h12 * v2) + v2 * (h12 * v1 + h22 * v2)

def a9num (f1 f2 f11 f12 f22 : Int) : Int :=
  f2 * f2 * f11 - 2 * (1 + f1) * f2 * f12 + (1 + f1) * (1 + f1) * f22

theorem fn8_hessian_form (f1 f2 f11 f12 f22 : Int) :
    qf (-f2) (1 + f1) f11 f12 f22 = a9num f1 f2 f11 f12 f22 := by
  unfold qf a9num
  poly

theorem a9_at_symmetric_privilege (f2 f11 f12 f22 : Int) :
    a9num 0 f2 f11 f12 f22 = f2 * f2 * f11 - 2 * f2 * f12 + f22
      ∧ (1 + 0) * (1 - 0 * 0) = (1 : Int) := by
  constructor
  · unfold a9num
    poly
  · decide

theorem lemma1_first_order_cancels (F0 D Q t : Int) :
    (2 * F0 + 2 * D * t + Q * (t * t)) + (2 * F0 - 2 * D * t + Q * (t * t))
      = 4 * F0 + 2 * (Q * (t * t)) := by
  omega

theorem lemma1_sign (F0 D Q t : Int) (ht : t ≠ 0) :
    4 * F0 < (2 * F0 + 2 * D * t + Q * (t * t)) + (2 * F0 - 2 * D * t + Q * (t * t))
      ↔ 0 < Q := by
  rw [lemma1_first_order_cancels]
  have i := pos_mul_iff (t * t) Q (sq_pos_of_ne t ht)
  have e : Q * (t * t) = t * t * Q := Int.mul_comm _ _
  constructor
  · intro h
    exact i.mp (by omega)
  · intro h
    have := i.mpr h
    omega

theorem thm3_proof_chain
    (r u1 u2 u3 h1 p1 p11 p111 p112 p122 f2 f11 f12 f22 A P d : Int)
    (hf2 : f2 * h1 = u2)
    (hf11 : f11 * h1 = -(r * p122))
    (hf12 : f12 * (h1 * h1) = -(r * u2 * p112))
    (hf22 : f22 * (h1 * h1 * h1) = -(r * (-(u3 * (p11 * p11) * r) + u2 * u2 * p111)))
    (hsym : p122 = -p112)
    (hfoc : p1 * r = u1)
    (hA : IsA u1 u2 A d) (hP : IsP u2 u3 P d) :
    d * p1 * (h1 * h1 * h1) * a9num 0 f2 f11 f12 f22
      = r * u1 * u2 * (A * p1 * (p111 - 3 * p112) - P * (p11 * p11)) := by
  unfold IsA at hA
  unfold IsP at hP
  subst hsym
  subst hfoc
  have s1 : (h1 * h1 * h1) * a9num 0 f2 f11 f12 f22
      = (f2 * h1) * (f2 * h1) * (f11 * h1) - 2 * (f2 * h1) * (f12 * (h1 * h1))
        + f22 * (h1 * h1 * h1) := by
    unfold a9num
    poly
  rw [hf2, hf11, hf12, hf22] at s1
  have s2 : d * p1 * (h1 * h1 * h1) * a9num 0 f2 f11 f12 f22
      = d * p1 * ((h1 * h1 * h1) * a9num 0 f2 f11 f12 f22) := Int.mul_assoc _ _ _
  rw [s2, s1]
  have s3 : r * (p1 * r) * u2 * (A * p1 * (p111 - 3 * p112) - P * (p11 * p11))
      = r * u2 * p1 * (p111 - 3 * p112) * (A * (p1 * r))
        - r * (p1 * r) * (p11 * p11) * (P * u2) := by
    poly
  rw [s3, hA, hP]
  poly

theorem thm3_proof_sign
    (r u1 u2 u3 h1 p1 p11 p111 p112 p122 f2 f11 f12 f22 A P d : Int)
    (hf2 : f2 * h1 = u2)
    (hf11 : f11 * h1 = -(r * p122))
    (hf12 : f12 * (h1 * h1) = -(r * u2 * p112))
    (hf22 : f22 * (h1 * h1 * h1) = -(r * (-(u3 * (p11 * p11) * r) + u2 * u2 * p111)))
    (hsym : p122 = -p112)
    (hfoc : p1 * r = u1)
    (hA : IsA u1 u2 A d) (hP : IsP u2 u3 P d)
    (hr : 0 < r) (hu1 : 0 < u1) (hu2 : u2 < 0) (hh1 : h1 < 0) (hp1 : 0 < p1) (hd : 0 < d) :
    0 < a9num 0 f2 f11 f12 f22 ↔ 0 < A * p1 * (p111 - 3 * p112) - P * (p11 * p11) := by
  have key := thm3_proof_chain r u1 u2 u3 h1 p1 p11 p111 p112 p122 f2 f11 f12 f22 A P d
    hf2 hf11 hf12 hf22 hsym hfoc hA hP
  have hh3 : h1 * h1 * h1 < 0 := Int.mul_neg_of_pos_of_neg (sq_pos_of_ne h1 (by omega)) hh1
  have hL : d * p1 * (h1 * h1 * h1) < 0 := Int.mul_neg_of_pos_of_neg (Int.mul_pos hd hp1) hh3
  have hR : r * u1 * u2 < 0 := Int.mul_neg_of_pos_of_neg (Int.mul_pos hr hu1) hu2
  have iL := neg_mul_iff (d * p1 * (h1 * h1 * h1)) (a9num 0 f2 f11 f12 f22) hL
  have iR := neg_mul_iff (r * u1 * u2) (A * p1 * (p111 - 3 * p112) - P * (p11 * p11)) hR
  rw [key] at iL
  constructor
  · intro h
    exact iR.mp (iL.mpr h)
  · intro h
    exact iL.mp (iR.mpr h)

theorem thm3_from_A2 (A P k n S1 S11 S111 S112 : Int)
    (h1 : S1 = 2 * k * (n * n)) (h11 : S11 = -(2 * k * (n * n)))
    (h111 : S111 = 4 * k * (n * n) - k * k * k) (h112 : S112 = k * k * k) :
    A * S1 * (S111 - 3 * S112) - P * (S11 * S11) = 4 * (k * k) * (n * n) * gap A P k n := by
  subst h1 h11 h111 h112
  unfold gap
  poly

theorem thm3_from_A2_sign (A P k n S1 S11 S111 S112 : Int)
    (hk : k ≠ 0) (hn : n ≠ 0)
    (h1 : S1 = 2 * k * (n * n)) (h11 : S11 = -(2 * k * (n * n)))
    (h111 : S111 = 4 * k * (n * n) - k * k * k) (h112 : S112 = k * k * k) :
    0 < A * S1 * (S111 - 3 * S112) - P * (S11 * S11) ↔ Thm3 A P k n := by
  rw [thm3_from_A2 A P k n S1 S11 S111 S112 h1 h11 h111 h112]
  unfold Thm3
  have hc : 0 < 4 * (k * k) * (n * n) :=
    Int.mul_pos (Int.mul_pos (by decide) (sq_pos_of_ne k hk)) (sq_pos_of_ne n hn)
  exact pos_mul_iff _ _ hc

theorem thm3_end_to_end
    (r u1 u2 u3 h1 p1 p11 p111 p112 p122 f2 f11 f12 f22 A P d k n c1 c2 c3 : Int)
    (hf2 : f2 * h1 = u2)
    (hf11 : f11 * h1 = -(r * p122))
    (hf12 : f12 * (h1 * h1) = -(r * u2 * p112))
    (hf22 : f22 * (h1 * h1 * h1) = -(r * (-(u3 * (p11 * p11) * r) + u2 * u2 * p111)))
    (hsym : p122 = -p112)
    (hfoc : p1 * r = u1)
    (hA : IsA u1 u2 A d) (hP : IsP u2 u3 P d)
    (hr : 0 < r) (hu1 : 0 < u1) (hu2 : u2 < 0) (hh1 : h1 < 0) (hp1 : 0 < p1) (hd : 0 < d)
    (hc1 : c1 * p1 = 2 * k * (n * n)) (hc2 : c2 * p11 = -(2 * k * (n * n)))
    (hc3a : c3 * p111 = 4 * k * (n * n) - k * k * k) (hc3b : c3 * p112 = k * k * k)
    (hcc : c1 * c3 = c2 * c2) (hcpos : 0 < c1 * c3) (hk : k ≠ 0) (hn : n ≠ 0) :
    0 < a9num 0 f2 f11 f12 f22 ↔ Thm3 A P k n := by
  have s := thm3_proof_sign r u1 u2 u3 h1 p1 p11 p111 p112 p122 f2 f11 f12 f22 A P d
    hf2 hf11 hf12 hf22 hsym hfoc hA hP hr hu1 hu2 hh1 hp1 hd
  have l : c1 * c3 * (A * p1 * (p111 - 3 * p112) - P * (p11 * p11))
      = A * (c1 * p1) * (c3 * p111 - 3 * (c3 * p112)) - P * ((c2 * p11) * (c2 * p11)) := by
    calc c1 * c3 * (A * p1 * (p111 - 3 * p112) - P * (p11 * p11))
        = A * (c1 * p1) * (c3 * p111 - 3 * (c3 * p112)) - P * ((c1 * c3) * (p11 * p11)) := by
          poly
      _ = A * (c1 * p1) * (c3 * p111 - 3 * (c3 * p112)) - P * ((c2 * c2) * (p11 * p11)) := by
          rw [hcc]
      _ = A * (c1 * p1) * (c3 * p111 - 3 * (c3 * p112)) - P * ((c2 * p11) * (c2 * p11)) := by
          poly
  have t := thm3_from_A2_sign A P k n (c1 * p1) (c2 * p11) (c3 * p111) (c3 * p112) hk hn
    hc1 hc2 hc3a hc3b
  rw [← l] at t
  have i := pos_mul_iff (c1 * c3) (A * p1 * (p111 - 3 * p112) - P * (p11 * p11)) hcpos
  constructor
  · intro h
    exact t.mp (i.mpr (s.mp h))
  · intro h
    exact s.mpr (i.mp (t.mpr h))

theorem wtp_concave_mps_negative (A P k n : Int) (hA : 0 < A) (hn : n ≠ 0)
    (hc : A * (2 * A - P) < 0) : gap A P k n < 0 := by
  have h2 : 2 * A - P < 0 := by
    rcases Int.lt_or_le (2 * A - P) 0 with h | h
    · exact h
    · exact absurd hc (Int.not_lt.mpr (Int.mul_nonneg (Int.le_of_lt hA) h))
  rw [gap_alt]
  have h3 : (2 * A - P) * (n * n) < 0 := Int.mul_neg_of_neg_of_pos h2 (sq_pos_of_ne n hn)
  have h4 : 0 ≤ 2 * A * (k * k) := Int.mul_nonneg (by omega) (sq_nonneg_int k)
  omega

theorem quadratic_P_zero (u2 P d : Int) (hu2 : u2 ≠ 0) (h : IsP u2 0 P d) : P = 0 := by
  unfold IsP at h
  have h0 : P * u2 = 0 := by
    rw [h]
    poly
  rcases Int.mul_eq_zero.mp h0 with hP | hP
  · exact hP
  · exact absurd hP hu2

theorem quadratic_reduction (u1 u2 k n : Int) (hu2 : u2 ≠ 0) (hk : 0 ≤ k) (hn : 0 ≤ n) :
    Thm3U u1 u2 0 k n ↔ k < n := by
  unfold Thm3U
  have i := pos_mul_iff (2 * (u2 * u2)) (n * n - k * k)
    (Int.mul_pos (by decide) (sq_pos_of_ne u2 hu2))
  have j := sq_lt_sq_iff k n hk hn
  simp only [Int.mul_zero, Int.zero_mul]
  constructor
  · intro h
    exact j.mp (by have := i.mp h; omega)
  · intro h
    exact i.mpr (by have := j.mpr h; omega)

theorem cara_AP (s al : Int) :
    IsA s (-(al * s)) al 1 ∧ IsP (-(al * s)) (al * (al * s)) al 1 := by
  unfold IsA IsP
  constructor <;> poly

theorem cara_reduction (A k n : Int) (hA : 0 < A) : Thm3 A A k n ↔ 2 * (k * k) < n * n := by
  unfold Thm3
  have e : gap A A k n = A * (n * n - 2 * (k * k)) := by
    unfold gap
    poly
  rw [e]
  have i := pos_mul_iff A (n * n - 2 * (k * k)) hA
  constructor
  · intro h
    have := i.mp h
    omega
  · intro h
    exact i.mpr (by omega)

theorem cara_boundary_707 : Thm3 1 1 707 1000 ∧ ¬ Thm3 1 1 708 1000 := by
  unfold Thm3 gap
  decide

theorem crra_AP (z r : Int) :
    IsA (z * z) (-(r * z)) r z ∧ IsP (-(r * z)) (r * (r + 1)) (r + 1) z := by
  unfold IsA IsP
  constructor <;> poly

theorem crra_identity (g h k n : Int) :
    gap g (g + h) k n = g * (n * n - 2 * (k * k)) - h * (n * n) := by
  unfold gap
  poly

theorem crra_reduction (g h k n : Int) :
    Thm3 g (g + h) k n ↔ h * (n * n) < g * (n * n - 2 * (k * k)) := by
  unfold Thm3
  rw [crra_identity]
  constructor
  · intro h
    omega
  · intro h
    omega

theorem gap_scale (s A P k n : Int) : gap (s * A) (s * P) k n = s * gap A P k n := by
  unfold gap
  poly

theorem relative_measures_same_sign (s A P k n : Int) (hs : 0 < s) :
    Thm3 (s * A) (s * P) k n ↔ Thm3 A P k n := by
  unfold Thm3
  rw [gap_scale]
  exact pos_mul_iff s _ hs

theorem separator_values : gap 1 1 1 2 = 2 ∧ gap 1 2 1 2 = -2 := by
  decide

theorem separator_AP :
    (IsA 1 (-1) 1 1 ∧ IsP (-1) 1 1 1) ∧ (IsA 1 (-1) 1 1 ∧ IsP (-1) 2 2 1) := by
  unfold IsA IsP
  decide

theorem separator : Thm3 1 1 1 2 ∧ ¬ Thm3 1 2 1 2 := by
  unfold Thm3 gap
  decide

theorem separator_derivs : Thm3U 1 (-1) 1 1 2 ∧ ¬ Thm3U 1 (-1) 2 1 2 := by
  unfold Thm3U
  decide

theorem separator_interval (A k n : Int) (hA : 0 < A) (hk : k ≠ 0)
    (hm : 2 * (k * k) < n * n) : Thm3 A A k n ∧ ¬ Thm3 A (2 * A) k n := by
  constructor
  · exact (cara_reduction A k n hA).mpr hm
  · unfold Thm3
    have e : gap A (2 * A) k n = -(2 * (A * (k * k))) := by
      unfold gap
      poly
    rw [e]
    have : 0 < A * (k * k) := Int.mul_pos hA (sq_pos_of_ne k hk)
    omega

theorem m_dependence : Thm3 1 1 1 2 ∧ ¬ Thm3 1 1 1 1 := by
  unfold Thm3 gap
  decide

theorem separator_fails_above_boundary : ¬ Thm3 1 1 4 5 ∧ ¬ Thm3 1 2 4 5 := by
  unfold Thm3 gap
  decide

theorem cara_reduction_needs_pos_A : ¬ (Thm3 0 0 1 2 ↔ 2 * (1 * 1) < (2 : Int) * 2) := by
  unfold Thm3 gap
  decide

theorem thm3_from_A2_sign_needs_m_pos :
    Thm3 1 0 0 1
      ∧ 1 * (2 * 0 * (1 * 1)) * ((4 * 0 * (1 * 1) - 0 * 0 * 0) - 3 * (0 * 0 * 0))
          - 0 * ((-(2 * 0 * (1 * 1))) * (-(2 * 0 * (1 * 1)))) = (0 : Int) := by
  unfold Thm3 gap
  decide

def hRS (c e U0 U1 V0 V1 : Int) : Int := 2 * c * (U1 - U0) - e * (V1 + V0)

def dhRS (c e V0 V1 W0 W1 : Int) : Int := 2 * c * (V1 - V0) - e * (W1 + W0)

theorem rentSeeking_cara_identity (c e U0 U1 V0 V1 W0 W1 an ad : Int)
    (hV0 : ad * V0 = -(an * U0)) (hV1 : ad * V1 = -(an * U1))
    (hW0 : ad * W0 = -(an * V0)) (hW1 : ad * W1 = -(an * V1)) :
    ad * dhRS c e V0 V1 W0 W1 = -an * hRS c e U0 U1 V0 V1 := by
  unfold dhRS hRS
  have s : ad * (2 * c * (V1 - V0) - e * (W1 + W0))
      = 2 * c * (ad * V1 - ad * V0) - e * (ad * W1 + ad * W0) := by
    poly
  rw [s, hV0, hV1, hW0, hW1]
  poly

theorem rentSeeking_cara_no_wealth_effect (c e U0 U1 V0 V1 W0 W1 an ad : Int)
    (hV0 : ad * V0 = -(an * U0)) (hV1 : ad * V1 = -(an * U1))
    (hW0 : ad * W0 = -(an * V0)) (hW1 : ad * W1 = -(an * V1))
    (hd : 0 < ad) (hH : hRS c e U0 U1 V0 V1 = 0) :
    dhRS c e V0 V1 W0 W1 = 0 := by
  have key := rentSeeking_cara_identity c e U0 U1 V0 V1 W0 W1 an ad hV0 hV1 hW0 hW1
  rw [hH, Int.mul_zero] at key
  rcases Int.mul_eq_zero.mp key with h | h
  · omega
  · exact h

theorem rentSeeking_crra2_no_cancel :
    hRS 5 4 (-4) (-2) 4 1 = 0 ∧ dhRS 5 4 4 1 (-8) (-1) = 6 := by
  decide

theorem rentSeeking_cancel_needs_cara :
    ¬ ∃ an ad : Int, 0 < ad ∧ ad * 4 = -(an * (-4)) ∧ ad * 1 = -(an * (-2))
      ∧ ad * (-8) = -(an * 4) ∧ ad * (-1) = -(an * 1) := by
  intro ⟨an, ad, hd, hV0, hV1, hW0, hW1⟩
  have := rentSeeking_cara_no_wealth_effect 5 4 (-4) (-2) 4 1 (-8) (-1) an ad
    hV0 hV1 hW0 hW1 hd rentSeeking_crra2_no_cancel.1
  rw [rentSeeking_crra2_no_cancel.2] at this
  exact absurd this (by decide)

end SchroyenTreich
