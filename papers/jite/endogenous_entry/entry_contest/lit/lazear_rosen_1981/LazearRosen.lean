namespace LazearRosen

def numA (u1 u2 u3 : Int) : Int := u2 * u2 - u3 * u1

def QuotientRule (u1 u2 u3 dA : Int) : Prop := dA * (u1 * u1) = numA u1 u2 u3

theorem dara_iff_numerator_neg (u1 u2 u3 dA : Int) (hu1 : 0 < u1)
    (hq : QuotientRule u1 u2 u3 dA) : dA < 0 ↔ numA u1 u2 u3 < 0 := by
  have hsq : 0 < u1 * u1 := Int.mul_pos hu1 hu1
  unfold QuotientRule at hq
  constructor
  · intro h
    have := Int.mul_neg_of_neg_of_pos h hsq
    omega
  · intro h
    rcases Int.lt_or_le dA 0 with hlt | hge
    · exact hlt
    · have := Int.mul_nonneg hge (Int.le_of_lt hsq)
      omega

theorem dara_forces_positive_third (u1 u2 u3 dA : Int) (hu1 : 0 < u1)
    (hq : QuotientRule u1 u2 u3 dA) (hdara : dA < 0) :
    u2 * u2 < u3 * u1 ∧ 0 < u3 := by
  have hnum := (dara_iff_numerator_neg u1 u2 u3 dA hu1 hq).1 hdara
  unfold numA at hnum
  have hsq : 0 ≤ u2 * u2 := by
    rcases Int.lt_or_le u2 0 with h | h
    · exact Int.mul_nonneg_of_nonpos_of_nonpos (Int.le_of_lt h) (Int.le_of_lt h)
    · exact Int.mul_nonneg h h
  refine ⟨by omega, ?_⟩
  rcases Int.lt_or_le 0 u3 with h | h
  · exact h
  · have := Int.mul_nonpos_of_nonpos_of_nonneg h (Int.le_of_lt hu1)
    omega

theorem concave_zero_third_is_iara (u1 u2 dA : Int) (hu1 : 0 < u1) (hu2 : u2 < 0)
    (hq : QuotientRule u1 u2 0 dA) : 0 < dA := by
  unfold QuotientRule numA at hq
  have hsq : 0 < u1 * u1 := Int.mul_pos hu1 hu1
  have hn : 0 < u2 * u2 := Int.mul_pos_of_neg_of_neg hu2 hu2
  rcases Int.lt_or_le 0 dA with h | h
  · exact h
  · have := Int.mul_nonpos_of_nonpos_of_nonneg h (Int.le_of_lt hsq)
    omega

theorem positive_marginal_utility_needed :
    ∃ u1 u2 u3 dA : Int, QuotientRule u1 u2 u3 dA ∧ dA < 0 ∧ u3 < 0 :=
  ⟨-1, 0, -1, -1, by unfold QuotientRule numA; decide, by decide, by decide⟩

theorem strict_concavity_needed_for_iara (dA : Int) (hq : QuotientRule 1 0 0 dA) :
    dA = 0 := by
  unfold QuotientRule numA at hq
  omega

def incr (u : Int → Int) (x : Int) : Int := u (x + 1) - u x

def StrictlyConcave (u : Int → Int) : Prop := ∀ x y, x < y → incr u y < incr u x

def burden (u : Int → Int) (c w : Int) : Int := u w - u (w - c)

theorem burden_falls_of_concave (u : Int → Int) (hu : StrictlyConcave u) (c w : Int)
    (hc : 0 < c) : burden u c (w + 1) < burden u c w := by
  have h := hu (w - c) w (by omega)
  unfold incr at h
  unfold burden
  have e : w + 1 - c = w - c + 1 := by omega
  rw [e]
  omega

theorem linear_utility_burden_flat (c w : Int) :
    ¬ StrictlyConcave (fun x => x) ∧
      burden (fun x => x) c (w + 1) = burden (fun x => x) c w := by
  refine ⟨?_, ?_⟩
  · intro h
    have := h 0 1 (by decide)
    simp only [incr] at this
    omega
  · simp only [burden]
    omega

def quad (b g w : Int) : Int := b * w - g * (w * w)

theorem quad_incr (b g x : Int) : incr (quad b g) x = b - g * (x + x + 1) := by
  simp only [incr, quad, Int.mul_add, Int.add_mul, Int.mul_one, Int.one_mul]
  omega

theorem quad_second_and_third_difference (b g x : Int) :
    incr (quad b g) (x + 1) - incr (quad b g) x = -(2 * g) ∧
      (incr (quad b g) (x + 2) - incr (quad b g) (x + 1)) -
        (incr (quad b g) (x + 1) - incr (quad b g) x) = 0 := by
  simp only [quad_incr, Int.mul_add, Int.mul_one]
  exact ⟨by omega, by omega⟩

theorem quad_strictly_concave (b g : Int) (hg : 0 < g) : StrictlyConcave (quad b g) := by
  intro x y hxy
  rw [quad_incr, quad_incr]
  have := Int.mul_lt_mul_of_pos_left (show x + x + 1 < y + y + 1 by omega) hg
  omega

theorem quad_burden_step (b g c w : Int) :
    burden (quad b g) c (w + 1) - burden (quad b g) c w = -(2 * (g * c)) := by
  have h1 := quad_incr b g w
  have h2 := quad_incr b g (w - c)
  unfold incr at h1 h2
  simp only [Int.mul_add, Int.mul_sub, Int.mul_one] at h1 h2
  unfold burden
  have e : w + 1 - c = w - c + 1 := by omega
  rw [e]
  omega

theorem quadratic_separates (b g c w dA : Int) (hg : 0 < g) (hc : 0 < c)
    (hu1 : 0 < b - 2 * g * w) (hq : QuotientRule (b - 2 * g * w) (-(2 * g)) 0 dA) :
    burden (quad b g) c (w + 1) < burden (quad b g) c w ∧ 0 < dA :=
  ⟨burden_falls_of_concave _ (quad_strictly_concave b g hg) c w hc,
    concave_zero_third_is_iara _ _ dA hu1 (by omega) hq⟩

theorem documents_witness (c dA : Int) (hc : 0 < c)
    (hq : QuotientRule (8 - 2 * 1 * 3) (-(2 * 1)) 0 dA) :
    burden (quad 8 1) c (3 + 1) - burden (quad 8 1) c 3 = -(2 * c) ∧ dA = 1 ∧
      burden (quad 8 1) c (3 + 1) < burden (quad 8 1) c 3 := by
  have hs := quad_burden_step 8 1 c 3
  unfold QuotientRule numA at hq
  refine ⟨by omega, by omega, by omega⟩

def gainA2 (P2 W1 W2 Wa1 Wa2 : Int) : Int := P2 * W1 + (2 - P2) * W2 - (Wa1 + Wa2)

def gainB2 (P2 W1 W2 Wb1 Wb2 : Int) : Int := (2 - P2) * W1 + P2 * W2 - (Wb1 + Wb2)

structure Handicap (V g ma mb h P2 W1 W2 Wa1 Wa2 Wb1 Wb2 : Int) : Prop where
  mixed_zero_profit : W1 + W2 = V * (ma + mb)
  a_zero_profit : Wa1 + Wa2 = 2 * (V * ma)
  b_zero_profit : Wb1 + Wb2 = 2 * (V * mb)
  spread : g * (W1 - W2) = V
  win_prob : P2 = 1 + 2 * (g * ((ma - mb) - h))

theorem handicap_zero_sum (V ma mb P2 W1 W2 Wa1 Wa2 Wb1 Wb2 : Int)
    (hmix : W1 + W2 = V * (ma + mb)) (ha : Wa1 + Wa2 = 2 * (V * ma))
    (hb : Wb1 + Wb2 = 2 * (V * mb)) :
    gainA2 P2 W1 W2 Wa1 Wa2 + gainB2 P2 W1 W2 Wb1 Wb2 = 0 := by
  rw [Int.mul_add] at hmix
  simp only [gainA2, gainB2, Int.sub_mul]
  omega

theorem handicap_gain (V g ma mb h P2 W1 W2 Wa1 Wa2 Wb1 Wb2 : Int)
    (H : Handicap V g ma mb h P2 W1 W2 Wa1 Wa2 Wb1 Wb2) :
    gainA2 P2 W1 W2 Wa1 Wa2 = V * ((ma - mb) - 2 * h) ∧
      gainB2 P2 W1 W2 Wb1 Wb2 = -(V * ((ma - mb) - 2 * h)) := by
  obtain ⟨hmix, ha, hb, hspread, hlin⟩ := H
  have hsum := handicap_zero_sum V ma mb P2 W1 W2 Wa1 Wa2 Wb1 Wb2 hmix ha hb
  have h1 : P2 * W1 + (2 - P2) * W2 = P2 * (W1 - W2) + 2 * W2 := by
    rw [Int.sub_mul, Int.mul_sub]
    omega
  have h3 : g * ((ma - mb) - h) * (W1 - W2) = V * ((ma - mb) - h) := by
    rw [Int.mul_assoc, Int.mul_comm ((ma - mb) - h) (W1 - W2), ← Int.mul_assoc, hspread]
  have h2 : P2 * (W1 - W2) = (W1 - W2) + 2 * (V * ((ma - mb) - h)) := by
    rw [hlin, Int.add_mul, Int.one_mul, Int.mul_assoc, h3]
  have hVD : V * ((ma - mb) - h) = V * ma - V * mb - V * h := by
    rw [Int.mul_sub, Int.mul_sub]
  have hr : V * ((ma - mb) - 2 * h) = V * ma - V * mb - 2 * (V * h) := by
    rw [Int.mul_sub, Int.mul_sub, Int.mul_left_comm V 2 h]
  rw [Int.mul_add] at hmix
  have hA : gainA2 P2 W1 W2 Wa1 Wa2 = V * ((ma - mb) - 2 * h) := by
    unfold gainA2
    omega
  exact ⟨hA, by omega⟩

theorem competitive_handicap (V g ma mb h P2 W1 W2 Wa1 Wa2 Wb1 Wb2 : Int) (hV : 0 < V)
    (H : Handicap V g ma mb h P2 W1 W2 Wa1 Wa2 Wb1 Wb2) :
    (gainA2 P2 W1 W2 Wa1 Wa2 = 0 ∧ gainB2 P2 W1 W2 Wb1 Wb2 = 0 ↔ 2 * h = ma - mb) ∧
      (2 * h < ma - mb → 0 < gainA2 P2 W1 W2 Wa1 Wa2 ∧ gainB2 P2 W1 W2 Wb1 Wb2 < 0) ∧
      (ma - mb < 2 * h → gainA2 P2 W1 W2 Wa1 Wa2 < 0 ∧ 0 < gainB2 P2 W1 W2 Wb1 Wb2) := by
  obtain ⟨hA, hB⟩ := handicap_gain V g ma mb h P2 W1 W2 Wa1 Wa2 Wb1 Wb2 H
  rw [hA, hB]
  have pos : ∀ d : Int, 0 < d → 0 < V * d := fun d hd => Int.mul_pos hV hd
  have neg : ∀ d : Int, d < 0 → V * d < 0 := fun d hd => Int.mul_neg_of_pos_of_neg hV hd
  refine ⟨⟨fun hz => ?_, fun he => ?_⟩, fun hl => ?_, fun hg => ?_⟩
  · rcases Int.lt_or_le 0 ((ma - mb) - 2 * h) with p | p
    · have := pos _ p
      omega
    · rcases Int.lt_or_le ((ma - mb) - 2 * h) 0 with q | q
      · have := neg _ q
        omega
      · omega
  · have : (ma - mb) - 2 * h = 0 := by omega
    rw [this, Int.mul_zero]
    decide
  · have := pos ((ma - mb) - 2 * h) (by omega)
    exact ⟨by omega, by omega⟩
  · have := neg ((ma - mb) - 2 * h) (by omega)
    exact ⟨by omega, by omega⟩

theorem competitive_handicap_not_fair (V g ma mb P2 W1 W2 Wa1 Wa2 Wb1 Wb2 hs : Int)
    (hV : 0 < V) (hd : 0 < ma - mb) (hstar : 2 * hs = ma - mb)
    (Hfair : Handicap V g ma mb (ma - mb) P2 W1 W2 Wa1 Wa2 Wb1 Wb2) :
    hs < ma - mb ∧ gainA2 P2 W1 W2 Wa1 Wa2 < 0 := by
  have h3 := (competitive_handicap V g ma mb (ma - mb) P2 W1 W2 Wa1 Wa2 Wb1 Wb2 hV Hfair).2.2
  exact ⟨by omega, (h3 (by omega)).1⟩

theorem zero_sum_needs_mixed_zero_profit :
    ∃ V ma mb P2 W1 W2 Wa1 Wa2 Wb1 Wb2 : Int,
      Wa1 + Wa2 = 2 * (V * ma) ∧ Wb1 + Wb2 = 2 * (V * mb) ∧ W1 + W2 ≠ V * (ma + mb) ∧
        gainA2 P2 W1 W2 Wa1 Wa2 + gainB2 P2 W1 W2 Wb1 Wb2 ≠ 0 :=
  ⟨1, 1, 0, 1, 3, 0, 2, 0, 0, 0, by decide⟩

theorem gain_needs_spread_rule :
    ∃ V g ma mb h P2 W1 W2 Wa1 Wa2 : Int,
      W1 + W2 = V * (ma + mb) ∧ Wa1 + Wa2 = 2 * (V * ma) ∧
        P2 = 1 + 2 * (g * ((ma - mb) - h)) ∧ g * (W1 - W2) ≠ V ∧
          gainA2 P2 W1 W2 Wa1 Wa2 ≠ V * ((ma - mb) - 2 * h) :=
  ⟨1, 1, 1, 0, 0, 3, 2, -1, 2, 0, by decide⟩

theorem hypotheses_satisfiable :
    QuotientRule 1 (-1) 2 (-1) ∧ QuotientRule (8 - 2 * 1 * 3) (-(2 * 1)) 0 1 ∧
      Handicap 1 1 1 0 0 3 1 0 2 0 0 0 := by
  refine ⟨by unfold QuotientRule numA; decide, by unfold QuotientRule numA; decide, ?_⟩
  exact ⟨by decide, by decide, by decide, by decide, by decide⟩

structure Row where
  s2 : Nat
  mu : Nat
  muS : Nat
  eu : Nat
  euS : Nat

def rich : List Row :=
  [⟨1, 9995, 9984, 5012155, 5012465⟩, ⟨5, 9975, 9922, 5012150, 5012445⟩,
   ⟨10, 9950, 9846, 5012100, 5012295⟩, ⟨30, 9852, 9552, 5011940, 5011925⟩,
   ⟨60, 9710, 9142, 5011800, 5011415⟩, ⟨120, 9436, 8420, 5011420, 5010515⟩]

def poor : List Row :=
  [⟨1, 9980, 9938, 2524665, 2524725⟩, ⟨2, 9960, 9878, 2524616, 2524575⟩,
   ⟨10, 9807, 9419, 2524237, 2523437⟩, ⟨120, 8094, 5741, 2519930, 2514282⟩]

def contestPreferred (r : Row) : Bool := decide (r.eu < r.euS)

def rowAt (t : List Row) (s : Nat) : Option Row := t.find? (fun r => r.s2 == s)

def sorts (s : Nat) : Bool :=
  match rowAt rich s, rowAt poor s with
  | some r, some p => decide (r.eu < r.euS) && decide (p.euS < p.eu)
  | _, _ => false

def strictlyFalling : List Nat → Bool
  | a :: b :: t => decide (b < a) && strictlyFalling (b :: t)
  | _ => true

theorem table_s_values : 1 * 1000 = 5 * (2 * 100) ∧ 1 * 1000 = 20 * (2 * 25) ∧ 5 < 20 := by
  decide

theorem rich_contest_rows : (rich.filter contestPreferred).map Row.s2 = [1, 5, 10] := by
  decide

theorem poor_contest_rows : (poor.filter contestPreferred).map Row.s2 = [1] := by
  decide

theorem sorting_at_unit_variance :
    sorts 10 = true ∧ (rowAt rich 10).map Row.muS = some 9846 ∧
      (rowAt poor 10).map Row.mu = some 9807 ∧ 9807 < 9846 := by
  decide

theorem shared_variances :
    (rich.map Row.s2).filter (fun s => (poor.map Row.s2).contains s) = [1, 10, 120] := by
  decide

theorem sorting_only_at_unit_variance :
    [1, 10, 120].filter sorts = [10] ∧
      (rich.filter contestPreferred).map Row.s2 ≠ (poor.filter contestPreferred).map Row.s2 := by
  decide

theorem table_investment_orderings :
    rich.all (fun r => decide (r.muS < r.mu)) = true ∧
      poor.all (fun r => decide (r.muS < r.mu)) = true ∧
      strictlyFalling (rich.map Row.mu) = true ∧ strictlyFalling (rich.map Row.muS) = true ∧
      strictlyFalling (poor.map Row.mu) = true ∧ strictlyFalling (poor.map Row.muS) = true ∧
      [1, 10, 120].all (fun s => match rowAt rich s, rowAt poor s with
        | some r, some p => decide (p.mu < r.mu) && decide (p.muS < r.muS)
        | _, _ => false) = true := by
  decide

theorem rows_below_variance_threshold :
    rich.all (fun r => decide (4 * (r.s2 * r.s2) < (20 * 100) * (20 * 100))) = true ∧
      poor.all (fun r => decide (4 * (r.s2 * r.s2) < (20 * 25) * (20 * 25))) = true := by
  decide

end LazearRosen
