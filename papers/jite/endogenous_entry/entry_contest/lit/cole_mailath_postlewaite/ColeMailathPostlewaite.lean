namespace ColeMailathPostlewaite

def rankOf (w : List Int) (x : Int) : Nat := (w.filter (fun y => decide (y < x))).length

theorem rank_mono (w : List Int) (x y : Int) (h : x ≤ y) : rankOf w x ≤ rankOf w y := by
  induction w with
  | nil => simp [rankOf]
  | cons a t ih =>
    unfold rankOf at ih ⊢
    simp only [List.filter_cons]
    by_cases ha : a < x
    · have hb : a < y := Int.lt_of_lt_of_le ha h
      simp [ha, hb]
      omega
    · by_cases hb : a < y
      · simp [ha, hb]
        omega
      · simp [ha, hb]
        omega

theorem rank_ordinal (w : List Int) (f : Int → Int)
    (hf : ∀ a b, a < b → f a < f b) (x : Int) :
    rankOf (w.map f) (f x) = rankOf w x := by
  have key : ∀ a, (f a < f x) ↔ (a < x) := by
    intro a
    constructor
    · intro h
      rcases Int.lt_or_le a x with h1 | h1
      · exact h1
      · rcases Int.lt_or_eq_of_le h1 with h2 | h2
        · have := hf x a h2
          omega
        · subst h2
          omega
    · exact hf a x
  induction w with
  | nil => simp [rankOf]
  | cons a t ih =>
    unfold rankOf at ih ⊢
    simp only [List.map_cons, List.filter_cons]
    by_cases ha : a < x
    · have hb : f a < f x := (key a).mpr ha
      simp [ha, hb, ih]
    · have hb : ¬ f a < f x := fun h => ha ((key a).mp h)
      simp [ha, hb, ih]

theorem rank_none_below (w : List Int) (z : Int) (h : ∀ y, y ∈ w → ¬ y < z) :
    rankOf w z = 0 := by
  induction w with
  | nil => rfl
  | cons a t ih =>
    unfold rankOf at ih ⊢
    have ha : ¬ a < z := h a (List.mem_cons_self)
    rw [List.filter_cons_of_neg (p := fun y => decide (y < z)) (fun hc => ha (of_decide_eq_true hc))]
    exact ih (fun y hy => h y (List.mem_cons_of_mem a hy))

theorem rank_bottom (w : List Int) (x x' : Int)
    (hmin : ∀ y, y ∈ w → x ≤ y) (h : x' ≤ x) :
    rankOf w x' = 0 ∧ rankOf w x = 0 := by
  constructor
  · exact rank_none_below w x' (fun y hy => by have := hmin y hy; omega)
  · exact rank_none_below w x (fun y hy => by have := hmin y hy; omega)

def total : List Int → Int
  | [] => 0
  | a :: t => a + total t

theorem total_append (l1 l2 : List Int) : total (l1 ++ l2) = total l1 + total l2 := by
  induction l1 with
  | nil => simp [total]
  | cons a t ih =>
    simp only [List.cons_append, total, ih]
    omega

theorem total_rep_pair (n : Nat) (a b c d : Int) (h : a + b = c + d) :
    total (List.replicate n a) + total (List.replicate n b) =
      total (List.replicate n c) + total (List.replicate n d) := by
  induction n with
  | zero => simp [total]
  | succ k ih =>
    simp only [List.replicate_succ, total]
    omega

def onePoint (K : Int) (n : Nat) : List Int := List.replicate (2 * n) K

def twoPoint (K e : Int) (n : Nat) : List Int :=
  List.replicate n (K - e) ++ List.replicate n (K + e)

theorem twoPoint_mean (K e : Int) (n : Nat) :
    total (twoPoint K e n) = total (onePoint K n) := by
  unfold twoPoint onePoint
  have h2 : 2 * n = n + n := by omega
  rw [h2, ← List.replicate_append_replicate, total_append, total_append]
  exact total_rep_pair n (K - e) (K + e) K K (by omega)

theorem twoPoint_unequal (K e : Int) (n : Nat) (he : 0 < e) (hn : 0 < n) :
    (K - e) ∈ twoPoint K e n ∧ (K + e) ∈ twoPoint K e n ∧ K - e < K + e := by
  unfold twoPoint
  refine ⟨?_, ?_, by omega⟩
  · apply List.mem_append_left
    exact List.mem_replicate.mpr ⟨by omega, rfl⟩
  · apply List.mem_append_right
    exact List.mem_replicate.mpr ⟨by omega, rfl⟩

theorem rank_rep_below (n : Nat) (a x : Int) (h : a < x) :
    rankOf (List.replicate n a) x = n := by
  unfold rankOf
  induction n with
  | zero => rfl
  | succ k ih =>
    rw [List.replicate_succ, List.filter_cons_of_pos (p := fun y => decide (y < x)) (decide_eq_true h), List.length_cons, ih]

theorem rank_rep_not_below (n : Nat) (a x : Int) (h : ¬ a < x) :
    rankOf (List.replicate n a) x = 0 := by
  unfold rankOf
  induction n with
  | zero => rfl
  | succ k ih =>
    rw [List.replicate_succ, List.filter_cons_of_neg (p := fun y => decide (y < x)) (fun hc => h (of_decide_eq_true hc)), ih]

theorem rank_append (l1 l2 : List Int) (x : Int) :
    rankOf (l1 ++ l2) x = rankOf l1 x + rankOf l2 x := by
  unfold rankOf
  rw [List.filter_append, List.length_append]

theorem twoPoint_ranks (K e : Int) (n : Nat) (he : 0 < e) :
    rankOf (twoPoint K e n) (K - e) = 0 ∧ rankOf (twoPoint K e n) (K + e) = n := by
  unfold twoPoint
  rw [rank_append, rank_append,
    rank_rep_not_below n (K - e) (K - e) (Int.lt_irrefl _),
    rank_rep_not_below n (K + e) (K - e) (by omega),
    rank_rep_below n (K - e) (K + e) (by omega),
    rank_rep_not_below n (K + e) (K + e) (Int.lt_irrefl _)]
  exact ⟨rfl, rfl⟩

def Dispersive (d : Nat → Int) : Prop := ∀ r s, r ≤ s → d r ≤ d s

def qTwo (K e : Int) (n r : Nat) : Int := if r < n then K - e else K + e

def dispCompared (K e : Int) (n : Nat) (r : Nat) : Int := qTwo K e n r - (K + e)

def dispMeanPreserving (K e : Int) (n : Nat) (r : Nat) : Int := qTwo K e n r - K

theorem compared_dispersive (K e : Int) (n : Nat) (he : 0 ≤ e) :
    Dispersive (dispCompared K e n) := by
  intro r s hrs
  unfold dispCompared qTwo
  by_cases hr : r < n <;> by_cases hs : s < n <;> simp [hr, hs] <;> omega

theorem compared_nonpositive (K e : Int) (n : Nat) (he : 0 ≤ e) :
    ∀ r, dispCompared K e n r ≤ 0 := by
  intro r
  unfold dispCompared qTwo
  by_cases hr : r < n <;> simp [hr] <;> omega

theorem compared_zero_top (K e : Int) (n : Nat) :
    ∀ r, n ≤ r → dispCompared K e n r = 0 := by
  intro r hr
  unfold dispCompared qTwo
  have : ¬ r < n := by omega
  simp [this]

theorem meanPreserving_crosses (K e : Int) (n : Nat) (he : 0 < e) :
    Dispersive (dispMeanPreserving K e n) ∧
      (∀ r, r < n → dispMeanPreserving K e n r < 0) ∧
      (∀ r, n ≤ r → 0 < dispMeanPreserving K e n r) := by
  refine ⟨?_, ?_, ?_⟩
  · intro r s hrs
    unfold dispMeanPreserving qTwo
    by_cases hr : r < n <;> by_cases hs : s < n <;> simp [hr, hs] <;> omega
  · intro r hr
    unfold dispMeanPreserving qTwo
    simp [hr]
    omega
  · intro r hr
    unfold dispMeanPreserving qTwo
    have : ¬ r < n := by omega
    simp [this]
    omega

def StrictUpBelow (U : Int → Int) (ls : Int) : Prop :=
  ∀ a b, a < b → b ≤ ls → U a < U b

theorem matching_raises_savings (U : Int → Int) (ls l bj S : Int)
    (hV : U l + bj = U ls) (hl : l ≤ ls) (hj : 0 < bj) :
    S - ls < S - l := by
  rcases Int.lt_or_eq_of_le hl with h | h
  · omega
  · subst h
    omega

theorem lambda_decreasing_in_j (U : Int → Int) (ls : Int) (hU : StrictUpBelow U ls)
    (V l1 l2 bj1 bj2 : Int)
    (h1 : U l1 + bj1 = V) (h2 : U l2 + bj2 = V)
    (hl2 : l2 ≤ ls) (hj : bj1 < bj2) : l2 < l1 := by
  rcases Int.lt_or_le l2 l1 with h | h
  · exact h
  · rcases Int.lt_or_eq_of_le h with h' | h'
    · have := hU l1 l2 h' hl2
      omega
    · subst h'
      omega

theorem lower_V_lower_lambda (U : Int → Int) (ls : Int) (hU : StrictUpBelow U ls)
    (V V' l l' bj : Int)
    (h : U l + bj = V) (h' : U l' + bj = V')
    (hl : l ≤ ls) (hV : V ≤ V') : l ≤ l' := by
  rcases Int.lt_or_le l' l with c | c
  · have := hU l' l c hl
    omega
  · exact c

theorem slack_of_feasible (U : Int → Int) (ls Uc bh lh kminus : Int) (kson : Int → Int)
    (hmax : ∀ l, kminus ≤ kson l → l ≤ ls → U l ≤ Uc)
    (hlh : U lh + bh = U ls) (hlhs : lh ≤ ls) (hfeas : kminus ≤ kson lh) :
    U ls ≤ Uc + bh := by
  have := hmax lh hfeas hlhs
  omega

theorem compact_saves_more (U : Int → Int) (ls : Int) (hU : StrictUpBelow U ls)
    (S l1 l2 bj Uc bh : Int)
    (hone : U l1 + bj = U ls) (htwo : U l2 + bj = Uc + bh)
    (hl1 : l1 ≤ ls) (hslack : U ls ≤ Uc + bh) :
    S - l2 ≤ S - l1 := by
  have := lower_V_lower_lambda U ls hU (U ls) (Uc + bh) l1 l2 bj hone htwo hl1 hslack
  omega

theorem compact_saves_more_from_primitives (U : Int → Int) (ls : Int)
    (hU : StrictUpBelow U ls) (kson : Int → Int)
    (S l1 l2 lh bj Uc bh kminus : Int)
    (hmax : ∀ l, kminus ≤ kson l → l ≤ ls → U l ≤ Uc)
    (hlh : U lh + bh = U ls) (hlhs : lh ≤ ls) (hfeas : kminus ≤ kson lh)
    (hone : U l1 + bj = U ls) (htwo : U l2 + bj = Uc + bh) (hl1 : l1 ≤ ls) :
    S - l2 ≤ S - l1 :=
  compact_saves_more U ls hU S l1 l2 bj Uc bh hone htwo hl1
    (slack_of_feasible U ls Uc bh lh kminus kson hmax hlh hlhs hfeas)

theorem compact_saves_strictly_more (U : Int → Int) (ls : Int) (hU : StrictUpBelow U ls)
    (S l1 l2 bj Uc bh : Int)
    (hone : U l1 + bj = U ls) (htwo : U l2 + bj = Uc + bh)
    (hl1 : l1 ≤ ls) (hslack : U ls < Uc + bh) :
    S - l2 < S - l1 := by
  rcases Int.lt_or_le l1 l2 with c | c
  · omega
  · rcases Int.lt_or_eq_of_le c with c' | c'
    · have := hU l2 l1 c' hl1
      omega
    · subst c'
      omega

theorem comparison_needs_slack :
    ∃ (U : Int → Int) (ls S l1 l2 bj Uc bh : Int),
      StrictUpBelow U ls ∧ l1 ≤ ls ∧ l2 ≤ ls ∧
      U l1 + bj = U ls ∧ U l2 + bj = Uc + bh ∧
      ¬ (U ls ≤ Uc + bh) ∧ S - l1 < S - l2 := by
  refine ⟨fun x => x, 10, 10, 8, 2, 2, 3, 1, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · intro a b hab _
    exact hab
  all_goals decide

theorem comparison_needs_equal_income :
    ∃ (U1 U2 : Int → Int) (ls1 ls2 S l1 l2 bj Uc bh : Int),
      StrictUpBelow U1 ls1 ∧ StrictUpBelow U2 ls2 ∧ l1 ≤ ls1 ∧ l2 ≤ ls2 ∧
      U1 l1 + bj = U1 ls1 ∧ U2 l2 + bj = Uc + bh ∧
      U1 ls1 ≤ Uc + bh ∧ S - l1 < S - l2 := by
  refine ⟨fun x => x, fun x => 2 * x, 10, 10, 10, 8, 7, 2, 15, 1,
    ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · intro a b hab _
    exact hab
  · intro a b hab _
    show 2 * a < 2 * b
    omega
  all_goals decide

structure Q where
  n : Int
  d : Int

namespace Q

def add (x y : Q) : Q := ⟨x.n * y.d + y.n * x.d, x.d * y.d⟩
def sub (x y : Q) : Q := ⟨x.n * y.d - y.n * x.d, x.d * y.d⟩
def mul (x y : Q) : Q := ⟨x.n * y.n, x.d * y.d⟩
def div (x y : Q) : Q := ⟨x.n * y.d, x.d * y.n⟩

def Eqv (x y : Q) : Prop := x.d ≠ 0 ∧ y.d ≠ 0 ∧ x.n * y.d = y.n * x.d
def Lt (x y : Q) : Prop :=
  x.d ≠ 0 ∧ y.d ≠ 0 ∧ x.n * x.d * (y.d * y.d) < y.n * y.d * (x.d * x.d)

instance (x y : Q) : Decidable (Eqv x y) := inferInstanceAs (Decidable (_ ∧ _ ∧ _))
instance (x y : Q) : Decidable (Lt x y) := inferInstanceAs (Decidable (_ ∧ _ ∧ _))

end Q

def qA : Q := ⟨3, 1⟩
def qBeta : Q := ⟨3, 4⟩
def qOne : Q := ⟨1, 1⟩
def qZero : Q := ⟨0, 1⟩
def qHalf : Q := ⟨1, 2⟩

def u (c : Q) : Q := Q.div ⟨-1, 1⟩ c
def uPrime (c : Q) : Q := Q.div qOne (c.mul c)

def c0 (K l : Q) : Q := (qA.mul K).mul l
def c1 (K l : Q) : Q := ((qA.mul qA).mul K).mul (qOne.sub l)
def kson (K l : Q) : Q := (qA.mul K).mul (qOne.sub l)

def objective (K l : Q) : Q := (u (c0 K l)).add (qBeta.mul (u (c1 K l)))
def Vmatch (K l j : Q) : Q := (u (c0 K l)).add (qBeta.mul ((u (c1 K l)).add j))

def lam0 : Q := ⟨2, 3⟩
def kLow : Q := ⟨1, 1⟩
def kHigh : Q := ⟨5, 3⟩
def lamLowHalf : Q := ⟨1, 3⟩
def kMinusHalf : Q := ⟨2, 1⟩
def lamBar : Q := ⟨3, 5⟩
def jStar : Q := ⟨5, 9⟩
def lamOne : Q := ⟨1, 4⟩
def lamTwo : Q := ⟨1, 2⟩

def vHalf : Q := (objective kHigh lamBar).add (qBeta.mul qHalf)

def implicitLHS (l : Q) : Q := (Q.div qOne l).add ((Q.div qBeta qA).mul (Q.div qOne (qOne.sub l)))
def implicitRHS (V K j : Q) : Q := (V.sub (qBeta.mul j)).mul ((⟨-1, 1⟩ : Q).mul (qA.mul K))
def dLambdaDen (l : Q) : Q :=
  (Q.div qOne (l.mul l)).sub ((Q.div qBeta qA).mul (Q.div qOne ((qOne.sub l).mul (qOne.sub l))))

theorem inst_lambda0_formula :
    Q.Eqv (Q.div qBeta qA) ⟨1, 4⟩ ∧ Q.Eqv ⟨1, 4⟩ (qHalf.mul qHalf) ∧
      Q.Eqv (lam0.mul (qOne.add qHalf)) qOne := by
  decide

theorem inst_lambda0_foc :
    Q.Eqv ((qA.mul kLow).mul (uPrime (c0 kLow lam0)))
      (((qBeta.mul qA).mul (qA.mul kLow)).mul (uPrime (c1 kLow lam0))) ∧
    Q.Eqv ((qA.mul kHigh).mul (uPrime (c0 kHigh lam0)))
      (((qBeta.mul qA).mul (qA.mul kHigh)).mul (uPrime (c1 kHigh lam0))) := by
  decide

theorem inst_lower_half :
    Q.Eqv (Vmatch kLow lamLowHalf qHalf) (objective kLow lam0) ∧
      Q.Lt lamLowHalf lam0 ∧ Q.Eqv (kson kLow lamLowHalf) kMinusHalf := by
  decide

theorem inst_restriction_binds :
    Q.Eqv (qOne.sub (Q.div kMinusHalf (kHigh.mul qA))) lamBar ∧
      Q.Eqv (kson kHigh lamBar) kMinusHalf ∧ Q.Lt lamBar lam0 := by
  decide

theorem inst_welfare_levels :
    Q.Eqv (objective kHigh lam0) ⟨-9, 20⟩ ∧ Q.Eqv vHalf ⟨-1, 12⟩ ∧
      Q.Lt (objective kHigh lam0) vHalf := by
  decide

theorem inst_one_point_match :
    Q.Eqv (Vmatch kHigh lamOne jStar) (objective kHigh lam0) ∧ Q.Lt lamOne lam0 ∧
      Q.Eqv (implicitLHS lamOne) (implicitRHS (objective kHigh lam0) kHigh jStar) := by
  decide

theorem inst_two_point_match :
    Q.Eqv (Vmatch kHigh lamTwo jStar) vHalf ∧ Q.Lt lamTwo lam0 ∧ Q.Lt qHalf jStar ∧
      Q.Eqv (implicitLHS lamTwo) (implicitRHS vHalf kHigh jStar) := by
  decide

theorem inst_compact_saves_more :
    Q.Lt (qOne.sub lamTwo) (qOne.sub lamOne) ∧ Q.Eqv (qOne.sub lamOne) ⟨3, 4⟩ ∧
      Q.Eqv (qOne.sub lamTwo) qHalf := by
  decide

theorem inst_dlambda_dj_negative :
    Q.Lt qZero (dLambdaDen lamOne) ∧ Q.Lt qZero (dLambdaDen lamTwo) ∧
      Q.Lt qZero (dLambdaDen lamLowHalf) ∧ Q.Eqv (dLambdaDen lam0) qZero := by
  decide

theorem inst_not_mean_preserving :
    Q.Eqv ((kLow.add kHigh).mul qHalf) ⟨4, 3⟩ ∧ Q.Lt ⟨4, 3⟩ kHigh := by
  decide

theorem cmp95_slope_falls_with_alpha (N M GA GB SA SB : Nat)
    (hN : 0 < N) (hM : 0 < M)
    (hA : GA * (N + GA) * SA = N * N * M) (hB : GB * (N + GB) * SB = N * N * M)
    (hS : SB < SA) : GA < GB := by
  rcases Nat.lt_or_ge GA GB with h | h
  · exact h
  · have hpos : 0 < GA * (N + GA) := by
      rcases Nat.eq_zero_or_pos (GA * (N + GA)) with z | z
      · rw [z, Nat.zero_mul] at hA
        have : 0 < N * N * M := Nat.mul_pos (Nat.mul_pos hN hN) hM
        omega
      · exact z
    have hle : GB * (N + GB) ≤ GA * (N + GA) :=
      Nat.mul_le_mul h (Nat.add_le_add_left h N)
    have := Nat.mul_lt_mul_of_le_of_lt hle hS hpos
    omega

theorem cmp95_effort_falls_with_alpha (a N GA GB : Nat) (ha : 0 < a) (h : GA < GB) :
    a * (N + GA) < a * (N + GB) :=
  Nat.mul_lt_mul_of_pos_left (by omega) ha

theorem cmp95_eq28 (G N S M j : Nat) (h25 : G * (N + G) * S = N * N * M) :
    G * S * (N + G) * j = j * (N * M * N) := by
  have e : G * S * (N + G) = G * (N + G) * S := Nat.mul_right_comm G S (N + G)
  rw [e, h25, Nat.mul_comm j (N * M * N), Nat.mul_right_comm N N M]

theorem cmp95_eq25_instance : 1 * (1 + 1) * 1 = 1 * 1 * 2 := by decide

theorem property1_no_switch (phi : Int → Int → Int) (W M : Int → Int)
    (hsup : ∀ a b c d, b < a → c < d → phi a c + phi b d < phi a d + phi b c)
    (ki kj ki' kj' : Int)
    (hi : phi ki kj' + M ki + W kj' ≤ phi ki ki' + M ki + W ki')
    (hj : phi kj ki' + M kj + W ki' ≤ phi kj kj' + M kj + W kj')
    (hk : kj < ki) : kj' ≤ ki' := by
  rcases Int.lt_or_le ki' kj' with h | h
  · have := hsup ki kj ki' kj' hk h
    omega
  · exact h

theorem property1_tie_not_excluded :
    ∃ (phi : Int → Int → Int) (W M : Int → Int) (ki kj ki' kj' : Int),
      (∀ a b c d, b < a → c < d → phi a c + phi b d < phi a d + phi b c) ∧
      phi ki kj' + M ki + W kj' ≤ phi ki ki' + M ki + W ki' ∧
      phi kj ki' + M kj + W ki' ≤ phi kj kj' + M kj + W kj' ∧
      kj < ki ∧ ki' = kj' := by
  refine ⟨fun a c => a * c, fun _ => 0, fun _ => 0, 2, 1, 5, 5, ?_, ?_, ?_, ?_, rfl⟩
  · intro a b c d hab hcd
    show a * c + b * d < a * d + b * c
    have h := Int.mul_pos (Int.sub_pos.mpr hab) (Int.sub_pos.mpr hcd)
    rw [Int.sub_mul, Int.mul_sub, Int.mul_sub] at h
    omega
  all_goals decide

end ColeMailathPostlewaite
