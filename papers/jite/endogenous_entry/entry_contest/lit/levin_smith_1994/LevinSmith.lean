namespace LevinSmith

def binom : Nat → Nat → Nat
  | _, 0 => 1
  | 0, _ + 1 => 0
  | n + 1, k + 1 => binom n k + binom n (k + 1)

theorem binom_zero_right (n : Nat) : binom n 0 = 1 := by
  simp only [binom]

theorem binom_row_five :
    binom 5 0 = 1 ∧ binom 5 1 = 5 ∧ binom 5 2 = 10 ∧ binom 5 3 = 10
      ∧ binom 5 4 = 5 ∧ binom 5 5 = 1 ∧ binom 5 6 = 0 := by
  decide

theorem binom_zero_of_lt : ∀ n k : Nat, n < k → binom n k = 0
  | 0, 0, h => absurd h (Nat.lt_irrefl 0)
  | 0, _ + 1, _ => rfl
  | _ + 1, 0, h => absurd h (Nat.not_lt_zero _)
  | n + 1, k + 1, h => by
      show binom n k + binom n (k + 1) = 0
      rw [binom_zero_of_lt n k (by omega), binom_zero_of_lt n (k + 1) (by omega)]

theorem binom_absorption : ∀ n k : Nat,
    (k + 1) * binom (n + 1) (k + 1) = (n + 1) * binom n k
  | 0, 0 => rfl
  | 0, _ + 1 => rfl
  | n + 1, 0 => by
      have ih := binom_absorption n 0
      have h1 : binom (n + 1 + 1) (0 + 1) = binom (n + 1) 0 + binom (n + 1) (0 + 1) := rfl
      have h0 : binom (n + 1) 0 = 1 := rfl
      rw [h1, h0]
      rw [binom_zero_right n] at ih
      omega
  | n + 1, k + 1 => by
      have ih1 := binom_absorption n k
      have ih2 := binom_absorption n (k + 1)
      have hp : binom (n + 1 + 1) (k + 1 + 1)
          = binom (n + 1) (k + 1) + binom (n + 1) (k + 1 + 1) := rfl
      have hq : binom (n + 1) (k + 1) = binom n k + binom n (k + 1) := rfl
      rw [hp, Nat.mul_add]
      have e1 : (k + 1 + 1) * binom (n + 1) (k + 1)
          = (k + 1) * binom (n + 1) (k + 1) + binom (n + 1) (k + 1) := Nat.succ_mul _ _
      have e2 : (n + 1 + 1) * binom (n + 1) (k + 1)
          = (n + 1) * binom (n + 1) (k + 1) + binom (n + 1) (k + 1) := Nat.succ_mul _ _
      have e3 : (n + 1) * binom (n + 1) (k + 1)
          = (n + 1) * binom n k + (n + 1) * binom n (k + 1) := by
        rw [hq, Nat.mul_add]
      omega

theorem binom_ratio : ∀ n k : Nat, (n - k) * binom n k = (k + 1) * binom n (k + 1)
  | 0, _ => by
      rw [Nat.zero_sub, Nat.zero_mul]
      rfl
  | m + 1, 0 => by
      rw [binom_absorption m 0, binom_zero_right m]
      rfl
  | m + 1, j + 1 => by
      rw [binom_absorption m (j + 1)]
      have hA := binom_absorption m j
      have hB : binom (m + 1) (j + 1) = binom m j + binom m (j + 1) := rfl
      rcases Nat.lt_or_ge m j with h | h
      · have hz : m + 1 - (j + 1) = 0 := by omega
        rw [hz, Nat.zero_mul, binom_zero_of_lt m (j + 1) (by omega), Nat.mul_zero]
      · have hsplit : (m + 1 - (j + 1)) * binom (m + 1) (j + 1)
            + (j + 1) * binom (m + 1) (j + 1) = (m + 1) * binom (m + 1) (j + 1) := by
          rw [← Nat.add_mul]
          congr 1
          omega
        have e3 : (m + 1) * binom (m + 1) (j + 1)
            = (m + 1) * binom m j + (m + 1) * binom m (j + 1) := by
          rw [hB, Nat.mul_add]
        omega

def IsCutoff (E : Nat → Int) (k : Nat) : Prop := 0 ≤ E k ∧ E (k + 1) < 0

def AntiN (E : Nat → Int) : Prop := ∀ a b, a ≤ b → E b ≤ E a

theorem cutoff_unique (E : Nat → Int) (hE : AntiN E) (k k' : Nat)
    (hk : IsCutoff E k) (hk' : IsCutoff E k') : k = k' := by
  obtain ⟨hk1, hk2⟩ := hk
  obtain ⟨hk1', hk2'⟩ := hk'
  rcases Nat.lt_trichotomy k k' with h | h | h
  · have := hE (k + 1) k' h
    omega
  · exact h
  · have := hE (k' + 1) k h
    omega

theorem cutoff_exists (E : Nat → Int) (a : Nat) (ha : 0 ≤ E a) :
    ∀ b, a < b → E b < 0 → ∃ k, a ≤ k ∧ k < b ∧ IsCutoff E k
  | 0, h, _ => absurd h (Nat.not_lt_zero _)
  | b + 1, h, hb => by
      rcases Nat.lt_or_ge a b with hlt | hge
      · rcases Int.lt_or_le (E b) 0 with hEb | hEb
        · obtain ⟨k, hak, hkb, hk⟩ := cutoff_exists E a ha b hlt hEb
          exact ⟨k, hak, by omega, hk⟩
        · exact ⟨b, by omega, by omega, hEb, hb⟩
      · have hab : a = b := by omega
        subst hab
        exact ⟨a, Nat.le_refl a, by omega, ha, hb⟩

def entrants (l : List Bool) : Nat := l.count true

def PureEq (E : Nat → Int) (l : List Bool) : Prop :=
  (true ∈ l → 0 ≤ E (entrants l)) ∧ (false ∈ l → E (entrants l + 1) ≤ 0)

theorem pure_count_pinned (E : Nat → Int) (hE : AntiN E) (nstar : Nat)
    (hcut : IsCutoff E nstar) (hstrict : 0 < E nstar) (l : List Bool)
    (heq : PureEq E l) (hin : true ∈ l) (hout : false ∈ l) :
    entrants l = nstar := by
  have h1 := heq.1 hin
  have h2 := heq.2 hout
  obtain ⟨_, hc2⟩ := hcut
  rcases Nat.lt_trichotomy (entrants l) nstar with h | h | h
  · have := hE (entrants l + 1) nstar h
    omega
  · exact h
  · have := hE (nstar + 1) (entrants l) h
    omega

theorem pure_identity_free (E : Nat → Int) (l l' : List Bool) (hp : l.Perm l') :
    PureEq E l ↔ PureEq E l' := by
  unfold PureEq entrants
  rw [hp.count_eq true, hp.mem_iff, hp.mem_iff]

def twoBidderGain (k : Nat) : Int := if k ≤ 1 then 1 else -1

theorem identities_not_pinned :
    PureEq twoBidderGain [true, false] ∧ PureEq twoBidderGain [false, true]
      ∧ [true, false] ≠ [false, true] := by
  unfold PureEq entrants twoBidderGain
  decide

def knifeEdgeGain (k : Nat) : Int := if k ≤ 2 then 0 else -1

theorem control_knife_edge_count :
    IsCutoff knifeEdgeGain 2 ∧ AntiN knifeEdgeGain
      ∧ PureEq knifeEdgeGain [true, false, false]
      ∧ PureEq knifeEdgeGain [true, true, false]
      ∧ entrants [true, false, false] ≠ entrants [true, true, false] := by
  refine ⟨?_, ?_, ?_, ?_, ?_⟩
  · unfold IsCutoff knifeEdgeGain
    decide
  · intro a b hab
    unfold knifeEdgeGain
    split <;> split <;> omega
  · unfold PureEq entrants knifeEdgeGain
    decide
  · unfold PureEq entrants knifeEdgeGain
    decide
  · decide

def StrictAnti (A : Int → Int) : Prop := ∀ a b, a < b → A b < A a

def Anti (A : Int → Int) : Prop := ∀ a b, a ≤ b → A b ≤ A a

theorem symmetric_root_unique (Phi : Int → Int) (hPhi : StrictAnti Phi) (q1 q2 : Int)
    (h1 : Phi q1 = 0) (h2 : Phi q2 = 0) : q1 = q2 := by
  rcases Int.lt_trichotomy q1 q2 with h | h | h
  · have := hPhi q1 q2 h
    omega
  · exact h
  · have := hPhi q2 q1 h
    omega

def sumTo (f : Nat → Int) : Nat → Int
  | 0 => 0
  | n + 1 => sumTo f n + f (n + 1)

def sumFrom2 (f : Nat → Int) : Nat → Int
  | 0 => 0
  | 1 => 0
  | n + 2 => sumFrom2 f (n + 1) + f (n + 2)

theorem sumTo_split (f : Nat → Int) : ∀ N : Nat, 1 ≤ N → sumTo f N = f 1 + sumFrom2 f N
  | 0, h => absurd h (by decide)
  | 1, _ => by
      show sumTo f 0 + f 1 = f 1 + 0
      show (0 : Int) + f 1 = f 1 + 0
      omega
  | n + 2, _ => by
      have ih := sumTo_split f (n + 1) (by omega)
      show sumTo f (n + 1) + f (n + 2) = f 1 + (sumFrom2 f (n + 1) + f (n + 2))
      omega

theorem sumTo_congr (f g : Nat → Int) :
    ∀ N : Nat, (∀ n, 1 ≤ n → n ≤ N → f n = g n) → sumTo f N = sumTo g N
  | 0, _ => rfl
  | n + 1, h => by
      have ih := sumTo_congr f g n (fun m hm1 hm2 => h m hm1 (by omega))
      show sumTo f n + f (n + 1) = sumTo g n + g (n + 1)
      rw [ih, h (n + 1) (by omega) (Nat.le_refl _)]

theorem sumTo_zero (f : Nat → Int) :
    ∀ N : Nat, (∀ n, 1 ≤ n → n ≤ N → f n = 0) → sumTo f N = 0
  | 0, _ => rfl
  | n + 1, h => by
      have ih := sumTo_zero f n (fun m hm1 hm2 => h m hm1 (by omega))
      show sumTo f n + f (n + 1) = 0
      rw [ih, h (n + 1) (by omega) (Nat.le_refl _)]
      rfl

theorem sumFrom2_congr (f g : Nat → Int) :
    ∀ N : Nat, (∀ n, 2 ≤ n → n ≤ N → f n = g n) → sumFrom2 f N = sumFrom2 g N
  | 0, _ => rfl
  | 1, _ => rfl
  | n + 2, h => by
      have ih := sumFrom2_congr f g (n + 1) (fun m hm1 hm2 => h m hm1 (by omega))
      show sumFrom2 f (n + 1) + f (n + 2) = sumFrom2 g (n + 1) + g (n + 2)
      rw [ih, h (n + 2) (by omega) (Nat.le_refl _)]

theorem sumFrom2_nonneg (f : Nat → Int) :
    ∀ N : Nat, (∀ n, 2 ≤ n → n ≤ N → 0 ≤ f n) → 0 ≤ sumFrom2 f N
  | 0, _ => Int.le_refl 0
  | 1, _ => Int.le_refl 0
  | n + 2, h => by
      have ih := sumFrom2_nonneg f (n + 1) (fun m hm1 hm2 => h m hm1 (by omega))
      have ht := h (n + 2) (by omega) (Nat.le_refl _)
      show 0 ≤ sumFrom2 f (n + 1) + f (n + 2)
      omega

theorem sumFrom2_pos (f : Nat → Int) :
    ∀ N : Nat, (∀ n, 2 ≤ n → n ≤ N → 0 ≤ f n) →
      (∃ m, 2 ≤ m ∧ m ≤ N ∧ 0 < f m) → 0 < sumFrom2 f N
  | 0, _, ⟨m, hm1, hm2, _⟩ => absurd hm2 (by omega)
  | 1, _, ⟨m, hm1, hm2, _⟩ => absurd hm2 (by omega)
  | n + 2, h, ⟨m, hm1, hm2, hm3⟩ => by
      show 0 < sumFrom2 f (n + 1) + f (n + 2)
      rcases Nat.lt_or_ge m (n + 2) with hlt | hge
      · have ih := sumFrom2_pos f (n + 1) (fun k hk1 hk2 => h k hk1 (by omega))
          ⟨m, hm1, by omega, hm3⟩
        have ht := h (n + 2) (by omega) (Nat.le_refl _)
        omega
      · have hmeq : m = n + 2 := by omega
        subst hmeq
        have hrest := sumFrom2_nonneg f (n + 1) (fun k hk1 hk2 => h k hk1 (by omega))
        omega

theorem sumFrom2_zero_fn : ∀ N : Nat, sumFrom2 (fun _ => (0 : Int)) N = 0
  | 0 => rfl
  | 1 => rfl
  | n + 2 => by
      show sumFrom2 (fun _ => (0 : Int)) (n + 1) + 0 = 0
      rw [sumFrom2_zero_fn (n + 1)]
      rfl

def bidderPayoff (w V W : Nat → Int) (c e : Int) (N : Nat) : Int :=
  sumTo (fun n => w n * (V n - W n)) N - (c + e)

def stealing (w W : Nat → Int) (Vf : Int) (N : Nat) : Int :=
  sumFrom2 (fun n => w n * (Vf - W n)) N

def Eq6 (payoff : Int) : Prop := payoff = 0

def Eq9 (alone c : Int) : Prop := alone = c

theorem cv_payoff_split (w V W : Nat → Int) (Vf c e : Int) (N : Nat) (hN : 1 ≤ N)
    (hV : ∀ n, 1 ≤ n → V n = Vf) (hW1 : W 1 = 0) :
    bidderPayoff w V W c e N = w 1 * Vf + stealing w W Vf N - (c + e) := by
  unfold bidderPayoff stealing
  rw [sumTo_split _ N hN]
  have h1 : w 1 * (V 1 - W 1) = w 1 * Vf := by
    rw [hV 1 (Nat.le_refl 1), hW1, Int.sub_zero]
  have h2 : sumFrom2 (fun n => w n * (V n - W n)) N
      = sumFrom2 (fun n => w n * (Vf - W n)) N :=
    sumFrom2_congr _ _ N (fun n hn1 _ => by
      show w n * (V n - W n) = w n * (Vf - W n)
      rw [hV n (by omega)])
  rw [h1, h2]

theorem stealing_nonneg (w W : Nat → Int) (Vf : Int) (N : Nat)
    (hw : ∀ n, 2 ≤ n → n ≤ N → 0 ≤ w n) (hIR : ∀ n, 2 ≤ n → n ≤ N → W n ≤ Vf) :
    0 ≤ stealing w W Vf N :=
  sumFrom2_nonneg _ N (fun n hn1 hn2 =>
    Int.mul_nonneg (hw n hn1 hn2) (by have := hIR n hn1 hn2; omega))

theorem stealing_pos (w W : Nat → Int) (Vf : Int) (N : Nat)
    (hw : ∀ n, 2 ≤ n → n ≤ N → 0 ≤ w n) (hIR : ∀ n, 2 ≤ n → n ≤ N → W n ≤ Vf)
    (hm : ∃ m, 2 ≤ m ∧ m ≤ N ∧ 0 < w m ∧ W m < Vf) :
    0 < stealing w W Vf N := by
  obtain ⟨m, hm1, hm2, hm3, hm4⟩ := hm
  exact sumFrom2_pos _ N
    (fun n hn1 hn2 => Int.mul_nonneg (hw n hn1 hn2) (by have := hIR n hn1 hn2; omega))
    ⟨m, hm1, hm2, Int.mul_pos hm3 (by omega)⟩

theorem stealing_zero_of_full_extraction (w W : Nat → Int) (Vf : Int) (N : Nat)
    (hW : ∀ n, 2 ≤ n → n ≤ N → W n = Vf) : stealing w W Vf N = 0 := by
  unfold stealing
  rw [sumFrom2_congr _ (fun _ => 0) N (fun n hn1 hn2 => by
    show w n * (Vf - W n) = 0
    rw [hW n hn1 hn2, Int.sub_self, Int.mul_zero])]
  exact sumFrom2_zero_fn N

theorem planner_foc_iff_eq9 (N : Nat) (hN : 0 < N) (slope alone c : Int)
    (h8 : slope = (N : Int) * (alone - c)) : slope = 0 ↔ Eq9 alone c := by
  unfold Eq9
  constructor
  · intro h
    rw [h8] at h
    rcases Int.mul_eq_zero.mp h with h0 | h0
    · omega
    · omega
  · intro h
    rw [h8, h, Int.sub_self, Int.mul_zero]

theorem reservation_iff_eq9 (dev alone c : Int) (hdev : dev = alone - c) :
    dev = 0 ↔ Eq9 alone c := by
  unfold Eq9
  constructor
  · intro h
    omega
  · intro h
    omega

theorem free_entry_not_eq9 (w V W : Nat → Int) (Vf c : Int) (N : Nat) (hN : 1 ≤ N)
    (hV : ∀ n, 1 ≤ n → V n = Vf) (hW1 : W 1 = 0)
    (heq : Eq6 (bidderPayoff w V W c 0 N)) (hsteal : 0 < stealing w W Vf N) :
    ¬ Eq9 (w 1 * Vf) c := by
  unfold Eq6 at heq
  unfold Eq9
  rw [cv_payoff_split w V W Vf c 0 N hN hV hW1] at heq
  omega

theorem free_entry_slope_neg (w V W : Nat → Int) (Vf c : Int) (N : Nat) (hN : 1 ≤ N)
    (hV : ∀ n, 1 ≤ n → V n = Vf) (hW1 : W 1 = 0)
    (heq : Eq6 (bidderPayoff w V W c 0 N)) (hsteal : 0 < stealing w W Vf N)
    (slope : Int) (h8 : slope = (N : Int) * (w 1 * Vf - c)) : slope < 0 := by
  unfold Eq6 at heq
  rw [cv_payoff_split w V W Vf c 0 N hN hV hW1] at heq
  have hgap : w 1 * Vf - c = -stealing w W Vf N := by omega
  rw [h8, hgap, Int.mul_neg]
  have : 0 < (N : Int) * stealing w W Vf N := Int.mul_pos (by omega) hsteal
  omega

theorem optimal_fee_eq_stealing (w V W : Nat → Int) (Vf c e : Int) (N : Nat) (hN : 1 ≤ N)
    (hV : ∀ n, 1 ≤ n → V n = Vf) (hW1 : W 1 = 0)
    (heq : Eq6 (bidderPayoff w V W c e N)) (h9 : Eq9 (w 1 * Vf) c) :
    e = stealing w W Vf N := by
  unfold Eq6 at heq
  unfold Eq9 at h9
  rw [cv_payoff_split w V W Vf c e N hN hV hW1] at heq
  omega

theorem optimal_fee_pos (w V W : Nat → Int) (Vf c e : Int) (N : Nat) (hN : 1 ≤ N)
    (hV : ∀ n, 1 ≤ n → V n = Vf) (hW1 : W 1 = 0)
    (heq : Eq6 (bidderPayoff w V W c e N)) (h9 : Eq9 (w 1 * Vf) c)
    (hsteal : 0 < stealing w W Vf N) : 0 < e := by
  rw [optimal_fee_eq_stealing w V W Vf c e N hN hV hW1 heq h9]
  exact hsteal

theorem free_entry_exceeds_planner (A G : Int → Int) (c qs qf : Int) (hA : Anti A)
    (h9 : Eq9 (A qs) c) (h6 : Eq6 (A qf + G qf - c)) (hG : 0 < G qf) : qs < qf := by
  unfold Eq9 at h9
  unfold Eq6 at h6
  rcases Int.lt_or_le qs qf with h | h
  · exact h
  · have := hA qf qs h
    omega

theorem reservation_bound_below_equilibrium (A G : Int → Int) (c q0 qf : Int)
    (hA : StrictAnti A) (h0 : Eq9 (A q0) c) (h6 : Eq6 (A qf + G qf - c))
    (hG : 0 ≤ G qf) : q0 ≤ qf := by
  unfold Eq9 at h0
  unfold Eq6 at h6
  rcases Int.lt_or_le qf q0 with h | h
  · have := hA qf q0 h
    omega
  · exact h

theorem one_root_two_roles (A : Int → Int) (c q0 qs : Int) (hA : StrictAnti A)
    (h0 : Eq9 (A q0) c) (hs : Eq9 (A qs) c) : q0 = qs := by
  unfold Eq9 at h0 hs
  rcases Int.lt_trichotomy q0 qs with h | h | h
  · have := hA q0 qs h
    omega
  · exact h
  · have := hA qs q0 h
    omega

theorem control_no_stealing (w V W : Nat → Int) (Vf c : Int) (N : Nat) (hN : 1 ≤ N)
    (hV : ∀ n, 1 ≤ n → V n = Vf) (hW1 : W 1 = 0)
    (hW : ∀ n, 2 ≤ n → n ≤ N → W n = Vf) :
    Eq6 (bidderPayoff w V W c 0 N) ↔ Eq9 (w 1 * Vf) c := by
  unfold Eq6 Eq9
  rw [cv_payoff_split w V W Vf c 0 N hN hV hW1, stealing_zero_of_full_extraction w W Vf N hW]
  constructor
  · intro h
    omega
  · intro h
    omega

def risingAlone (q : Int) : Int := q

def constStealing (_ : Int) : Int := 1

theorem control_needs_concavity :
    ∃ A G : Int → Int, ∃ c qs qf : Int,
      Eq9 (A qs) c ∧ Eq6 (A qf + G qf - c) ∧ 0 < G qf ∧ ¬ qs < qf :=
  ⟨risingAlone, constStealing, 0, 0, -1, by
    unfold Eq9 Eq6 risingAlone constStealing
    decide⟩

theorem seller_revenue_is_welfare (B Pi S : Int) (h5 : S = B + Pi) (h6 : B = 0) :
    Pi = S := by
  omega

theorem prop1_fee_attains_planner (S qOf rev : Int → Int) (qs estar : Int)
    (hmax : ∀ q, S q ≤ S qs) (h7 : ∀ e, rev e = S (qOf e)) (hind : qOf estar = qs) :
    ∀ e, rev e ≤ rev estar := by
  intro e
  rw [h7 e, h7 estar, hind]
  exact hmax (qOf e)

theorem prop8_no_entry (x V c : Int) (M : Nat) (h9 : x ^ M * V = c) :
    x ^ (M + 1) * V = x * c := by
  rw [Int.pow_succ, ← h9, Int.mul_assoc, Int.mul_left_comm, ← Int.mul_assoc x]

def Eq18 (V W : Nat → Int) (n : Nat) : Prop := V n - W n = (n : Int) * (V n - V (n - 1))

theorem fn16_gives_eq18 (V W : Nat → Int) (n : Nat)
    (h16 : W n = (n : Int) * V (n - 1) - ((n : Int) - 1) * V n) : Eq18 V W n := by
  unfold Eq18
  have e1 : ((n : Int) - 1) * V n = (n : Int) * V n - V n := by
    rw [Int.sub_mul, Int.one_mul]
  have e2 : (n : Int) * (V n - V (n - 1)) = (n : Int) * V n - (n : Int) * V (n - 1) :=
    Int.mul_sub _ _ _
  rw [e2, h16, e1]
  omega

theorem eq18_iff_private_eq_social (V W : Nat → Int) (c : Int) (n : Nat) :
    Eq18 V W n ↔ (V n - W n) - (n : Int) * c = (n : Int) * (V n - V (n - 1) - c) := by
  unfold Eq18
  rw [Int.mul_sub (n : Int) (V n - V (n - 1)) c]
  constructor
  · intro h
    omega
  · intro h
    omega

def gap (V W : Nat → Int) (n : Nat) : Int := (V n - W n) - (n : Int) * (V n - V (n - 1))

theorem fixed_prize_eq18_gap (V W : Nat → Int) (Vf : Int) (n : Nat) (hn : 2 ≤ n)
    (hV : ∀ k, 1 ≤ k → V k = Vf) : gap V W n = Vf - W n := by
  unfold gap
  rw [hV n (by omega), hV (n - 1) (by omega), Int.sub_self, Int.mul_zero]
  omega

theorem fixed_prize_eq18_iff (V W : Nat → Int) (Vf : Int) (n : Nat) (hn : 2 ≤ n)
    (hV : ∀ k, 1 ≤ k → V k = Vf) : Eq18 V W n ↔ W n = Vf := by
  have h := fixed_prize_eq18_gap V W Vf n hn hV
  unfold gap at h
  unfold Eq18
  constructor
  · intro h18
    omega
  · intro hW
    omega

theorem fixed_prize_eq18_fails (V W : Nat → Int) (Vf : Int) (n : Nat) (hn : 2 ≤ n)
    (hV : ∀ k, 1 ≤ k → V k = Vf) (hW : W n < Vf) : ¬ Eq18 V W n := by
  rw [fixed_prize_eq18_iff V W Vf n hn hV]
  omega

theorem control_fixed_prize_full_extraction (V W : Nat → Int) (Vf : Int) (n : Nat)
    (hn : 2 ≤ n) (hV : ∀ k, 1 ≤ k → V k = Vf) (hW : W n = Vf) : Eq18 V W n :=
  (fixed_prize_eq18_iff V W Vf n hn hV).mpr hW

theorem fixed_prize_eq18_at_one (V W : Nat → Int) (Vf : Int)
    (hV0 : V 0 = 0) (hV1 : V 1 = Vf) (hW1 : W 1 = 0) : Eq18 V W 1 := by
  unfold Eq18
  show V 1 - W 1 = ((1 : Nat) : Int) * (V 1 - V 0)
  rw [hV0, hV1, hW1]
  omega

theorem fixed_prize_social_gain (V : Nat → Int) (Vf c : Int) (n : Nat) (hn : 2 ≤ n)
    (hV : ∀ k, 1 ≤ k → V k = Vf) (hc : 0 < c) :
    V n - V (n - 1) - c = -c ∧ V n - V (n - 1) - c < 0 := by
  rw [hV n (by omega), hV (n - 1) (by omega)]
  exact ⟨by omega, by omega⟩

theorem fixed_prize_private_exceeds_social (V W : Nat → Int) (Vf c : Int) (n : Nat)
    (hn : 2 ≤ n) (hV : ∀ k, 1 ≤ k → V k = Vf) (hW : W n < Vf) :
    (n : Int) * (V n - V (n - 1) - c) < (V n - W n) - (n : Int) * c := by
  rw [hV n (by omega), hV (n - 1) (by omega), Int.sub_self, Int.zero_sub, Int.mul_neg]
  omega

def wedge (w V W : Nat → Int) (N : Nat) : Int := sumTo (fun n => w n * gap V W n) N

theorem eq18_everywhere_zero_wedge (w V W : Nat → Int) (N : Nat)
    (h : ∀ n, 1 ≤ n → n ≤ N → Eq18 V W n) : wedge w V W N = 0 := by
  unfold wedge
  exact sumTo_zero _ N (fun n hn1 hn2 => by
    have h18 := h n hn1 hn2
    unfold Eq18 at h18
    show w n * gap V W n = 0
    unfold gap
    rw [h18, Int.sub_self, Int.mul_zero])

theorem fixed_prize_wedge_is_stealing (w V W : Nat → Int) (Vf : Int) (N : Nat) (hN : 1 ≤ N)
    (hV0 : V 0 = 0) (hV : ∀ k, 1 ≤ k → V k = Vf) (hW1 : W 1 = 0) :
    wedge w V W N = stealing w W Vf N := by
  unfold wedge stealing
  rw [sumTo_split _ N hN]
  have h1 : w 1 * gap V W 1 = 0 := by
    have h18 := fixed_prize_eq18_at_one V W Vf hV0 (hV 1 (Nat.le_refl 1)) hW1
    unfold Eq18 at h18
    unfold gap
    rw [h18, Int.sub_self, Int.mul_zero]
  rw [h1, Int.zero_add]
  exact sumFrom2_congr _ _ N (fun n hn1 _ => by
    show w n * gap V W n = w n * (Vf - W n)
    rw [fixed_prize_eq18_gap V W Vf n hn1 hV])

theorem cv_free_entry_excessive (w V W : Nat → Int) (Vf : Int) (N : Nat) (hN : 1 ≤ N)
    (hV0 : V 0 = 0) (hV : ∀ k, 1 ≤ k → V k = Vf) (hW1 : W 1 = 0)
    (hw : ∀ n, 2 ≤ n → n ≤ N → 0 ≤ w n) (hIR : ∀ n, 2 ≤ n → n ≤ N → W n ≤ Vf)
    (hm : ∃ m, 2 ≤ m ∧ m ≤ N ∧ 0 < w m ∧ W m < Vf)
    (slope : Int) (hslope : slope = -wedge w V W N) : slope < 0 := by
  rw [hslope, fixed_prize_wedge_is_stealing w V W Vf N hN hV0 hV hW1]
  exact Int.neg_neg_of_pos (stealing_pos w W Vf N hw hIR hm)

theorem ipv_free_entry_optimal (w V W : Nat → Int) (N : Nat)
    (h : ∀ n, 1 ≤ n → n ≤ N → Eq18 V W n)
    (slope : Int) (hslope : slope = -wedge w V W N) : slope = 0 := by
  rw [hslope, eq18_everywhere_zero_wedge w V W N h]
  rfl

def witV (n : Nat) : Int := if n = 0 then 0 else 6

def witW (n : Nat) : Int := if n = 1 then 0 else if n = 2 then 2 else 4

def witw (_ : Nat) : Int := 1

theorem witness_cv_chain :
    witV 0 = 0 ∧ (∀ k, 1 ≤ k → witV k = 6) ∧ witW 1 = 0
      ∧ (∀ n, 2 ≤ n → n ≤ 3 → 0 ≤ witw n) ∧ (∀ n, 2 ≤ n → n ≤ 3 → witW n ≤ 6)
      ∧ (∃ m, 2 ≤ m ∧ m ≤ 3 ∧ 0 < witw m ∧ witW m < 6)
      ∧ wedge witw witV witW 3 = 6 ∧ stealing witw witW 6 3 = 6 := by
  refine ⟨rfl, ?_, rfl, ?_, ?_, ⟨2, by decide, by decide, by decide, by decide⟩, ?_, ?_⟩
  · intro k hk
    unfold witV
    split <;> omega
  · intro n _ _
    unfold witw
    decide
  · intro n hn1 hn2
    unfold witW
    split
    · omega
    · split <;> omega
  · unfold wedge gap witw witV witW
    decide
  · unfold stealing witw witW
    decide

def negId (q : Int) : Int := -q

def unitSteal (_ : Int) : Int := 1

theorem witness_ordering :
    Anti negId ∧ StrictAnti negId ∧ Eq9 (negId 0) 0 ∧ Eq6 (negId 1 + unitSteal 1 - 0)
      ∧ 0 < unitSteal 1 := by
  refine ⟨?_, ?_, ?_, ?_, ?_⟩
  · intro a b h
    unfold negId
    omega
  · intro a b h
    unfold negId
    omega
  · unfold Eq9 negId
    decide
  · unfold Eq6 negId unitSteal
    decide
  · unfold unitSteal
    decide

def balW (n : Nat) : Int := if n = 2 then 2 else 1

def balV (n : Nat) : Int := if n = 0 then 0 else if n = 1 then 4 else if n = 2 then 6 else 7

def balPay (n : Nat) : Int := if n = 2 then 1 else if n = 3 then 6 else 0

theorem control_wedge_zero_without_eq18 :
    wedge balW balV balPay 3 = 0 ∧ ¬ Eq18 balV balPay 2 ∧ ¬ Eq18 balV balPay 3 := by
  unfold wedge Eq18 gap balW balV balPay
  decide

theorem sumTo_shift (a V : Nat → Int) :
    ∀ M : Nat, sumTo (fun n => a n * V (n - 1)) (M + 1)
      = a 1 * V 0 + sumTo (fun n => a (n + 1) * V n) M
  | 0 => by
      show (0 : Int) + a 1 * V 0 = a 1 * V 0 + 0
      omega
  | M + 1 => by
      have ih := sumTo_shift a V M
      show sumTo (fun n => a n * V (n - 1)) (M + 1) + a (M + 2) * V (M + 1)
        = a 1 * V 0 + (sumTo (fun n => a (n + 1) * V n) M + a (M + 2) * V (M + 1))
      rw [ih]
      omega

theorem eq19_vanishes (a r V : Nat → Int) (M : Nat)
    (hr : ∀ n, 1 ≤ n → n ≤ M → r n = a (n + 1)) (hrN : r (M + 1) = 0) (hV0 : V 0 = 0) :
    sumTo (fun n => a n * V (n - 1)) (M + 1) - sumTo (fun n => r n * V n) (M + 1) = 0 := by
  rw [sumTo_shift a V M, hV0, Int.mul_zero, Int.zero_add]
  show sumTo (fun n => a (n + 1) * V n) M - (sumTo (fun n => r n * V n) M + r (M + 1) * V (M + 1)) = 0
  rw [hrN, Int.zero_mul, Int.add_zero]
  rw [sumTo_congr (fun n => r n * V n) (fun n => a (n + 1) * V n) M (fun n hn1 hn2 => by
    show r n * V n = a (n + 1) * V n
    rw [hr n hn1 hn2])]
  exact Int.sub_self _

end LevinSmith
