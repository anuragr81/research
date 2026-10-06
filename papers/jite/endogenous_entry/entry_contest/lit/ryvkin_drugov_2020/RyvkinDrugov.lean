namespace RyvkinDrugov

def K (N a k : Nat) : Nat := a ^ (k - 1) * (N - a)

def TP2On (S1 S2 : Nat → Prop) (v : Nat → Nat → Nat) : Prop :=
  ∀ x1 x2 y1 y2, S1 x1 → S1 x2 → S2 y1 → S2 y2 → x1 < x2 → y1 < y2 →
    v x1 y2 * v x2 y1 ≤ v x1 y1 * v x2 y2

theorem negH_theta_eq_kernel (N a k : Nat) (ha : a ≤ N) (hk : 1 ≤ k) :
    ((a ^ (k - 1) * N : Nat) : Int) - ((a ^ k : Nat) : Int) = ((K N a k : Nat) : Int) := by
  obtain ⟨j, rfl⟩ : ∃ j, k = j + 1 := ⟨k - 1, by omega⟩
  unfold K
  rw [Nat.add_sub_cancel, Nat.pow_succ, Int.natCast_mul, Int.natCast_mul, Int.natCast_mul,
    Int.ofNat_sub ha, Int.mul_sub]

theorem kernel_cross (N a1 a2 k1 k2 : Nat) (ha : a1 ≤ a2) (hk1 : 1 ≤ k1) (hk : k1 ≤ k2) :
    K N a1 k2 * K N a2 k1 ≤ K N a1 k1 * K N a2 k2 := by
  obtain ⟨d, rfl⟩ : ∃ d, k2 = k1 + d := ⟨k2 - k1, by omega⟩
  unfold K
  have he : k1 + d - 1 = (k1 - 1) + d := by omega
  rw [he, Nat.pow_add, Nat.pow_add]
  have hd : a1 ^ d ≤ a2 ^ d := Nat.pow_le_pow_left ha d
  have h := Nat.mul_le_mul_left (a1 ^ (k1 - 1) * (N - a1) * (a2 ^ (k1 - 1) * (N - a2))) hd
  calc _ = a1 ^ (k1 - 1) * (N - a1) * (a2 ^ (k1 - 1) * (N - a2)) * a1 ^ d := by ac_rfl
    _ ≤ a1 ^ (k1 - 1) * (N - a1) * (a2 ^ (k1 - 1) * (N - a2)) * a2 ^ d := h
    _ = _ := by ac_rfl

theorem kernel_tp2 (N : Nat) : TP2On (fun a => a ≤ N) (fun k => 1 ≤ k) (K N) := by
  intro x1 x2 y1 y2 _ _ hy1 _ hx hy
  exact kernel_cross N x1 x2 y1 y2 (Nat.le_of_lt hx) hy1 (Nat.le_of_lt hy)

theorem kernel_cross_strict (N a1 a2 k1 k2 : Nat) (h0 : 0 < a1) (ha : a1 < a2) (hN : a2 < N)
    (hk1 : 1 ≤ k1) (hk : k1 < k2) :
    K N a1 k2 * K N a2 k1 < K N a1 k1 * K N a2 k2 := by
  obtain ⟨d, rfl⟩ : ∃ d, k2 = k1 + d := ⟨k2 - k1, by omega⟩
  unfold K
  have he : k1 + d - 1 = (k1 - 1) + d := by omega
  rw [he, Nat.pow_add, Nat.pow_add]
  have hd : a1 ^ d < a2 ^ d := Nat.pow_lt_pow_left ha (by omega)
  have hC : 0 < a1 ^ (k1 - 1) * (N - a1) * (a2 ^ (k1 - 1) * (N - a2)) :=
    Nat.mul_pos (Nat.mul_pos (Nat.pow_pos h0) (by omega))
      (Nat.mul_pos (Nat.pow_pos (by omega)) (by omega))
  have h := Nat.mul_lt_mul_of_pos_left hd hC
  calc _ = a1 ^ (k1 - 1) * (N - a1) * (a2 ^ (k1 - 1) * (N - a2)) * a1 ^ d := by ac_rfl
    _ < a1 ^ (k1 - 1) * (N - a1) * (a2 ^ (k1 - 1) * (N - a2)) * a2 ^ d := h
    _ = _ := by ac_rfl

def PeakAt (N k a0 : Nat) : Prop := ∀ a, a ≤ N → a ≠ a0 → K N a k < K N a0 k

theorem peak_k2 : PeakAt 24 2 12 := by unfold PeakAt; decide

theorem peak_k3 : PeakAt 36 3 24 := by unfold PeakAt; decide

theorem peak_k4 : PeakAt 48 4 36 := by unfold PeakAt; decide

theorem peak_k5 : PeakAt 60 5 48 := by unfold PeakAt; decide

theorem peak_k6 : PeakAt 72 6 60 := by unfold PeakAt; decide

theorem peak_not_at_half_k3 : ¬ PeakAt 36 3 18 := by unfold PeakAt; decide

theorem weight_tp2 (N : Nat) (F G : Nat → Nat) (hG : ∀ i j, i ≤ j → G i ≤ G j) :
    TP2On (fun _ => True) (fun k => 1 ≤ k) (fun i Q => F i * K N (G i) Q) := by
  intro x1 x2 y1 y2 _ _ hy1 _ hx hy
  have h := kernel_cross N (G x1) (G x2) y1 y2 (hG x1 x2 (Nat.le_of_lt hx)) hy1 (Nat.le_of_lt hy)
  have h2 := Nat.mul_le_mul_left (F x1 * F x2) h
  show F x1 * K N (G x1) y2 * (F x2 * K N (G x2) y1) ≤ F x1 * K N (G x1) y1 * (F x2 * K N (G x2) y2)
  calc _ = F x1 * F x2 * (K N (G x1) y2 * K N (G x2) y1) := by ac_rfl
    _ ≤ F x1 * F x2 * (K N (G x1) y1 * K N (G x2) y2) := h2
    _ = _ := by ac_rfl

def Gdown (i : Nat) : Nat := 2 - i

theorem weight_tp2_fails_without_monotone_G :
    ¬ TP2On (fun _ => True) (fun k => 1 ≤ k) (fun i Q => 1 * K 4 (Gdown i) Q) := by
  intro h
  have h1 := h 0 1 1 2 trivial trivial (by decide) (by decide) (by decide) (by decide)
  revert h1
  decide

def SCpm (u : Nat → Int) : Prop := ∀ x y, x < y → u x < 0 → u y ≤ 0

def SCmp (u : Nat → Int) : Prop := ∀ x y, x < y → 0 < u x → 0 ≤ u y

def SCpmFrom (m : Nat) (u : Nat → Int) : Prop := ∀ x y, m ≤ x → x < y → u x < 0 → u y ≤ 0

theorem scpm_neg_iff_scmp (u : Nat → Int) : SCpm (fun x => - u x) ↔ SCmp u := by
  constructor
  · intro h x y hxy hx
    have h1 := h x y hxy (show - u x < 0 by omega)
    show 0 ≤ u y
    have h2 : - u y ≤ 0 := h1
    omega
  · intro h x y hxy hx
    have hx' : - u x < 0 := hx
    have h1 := h x y hxy (by omega)
    show - u y ≤ 0
    omega

def phiPrime (i : Nat) : Int := 4 - 2 * (i : Int)

def fMinusG (i : Nat) : Int := 2 * (i : Int) - 4

def docIntegrand (i : Nat) : Int := ((i * i : Nat) : Int) * fMinusG i

theorem phiPrime_scpm : SCpm phiPrime := by
  intro x y hxy hx
  unfold phiPrime at *
  omega

theorem phiPrime_not_scmp : ¬ SCmp phiPrime := by
  intro h
  have h1 := h 0 3 (by decide) (by decide)
  revert h1
  decide

theorem fMinusG_scmp : SCmp fMinusG := by
  intro x y hxy hx
  unfold fMinusG at *
  omega

theorem fMinusG_not_scpm : ¬ SCpm fMinusG := by
  intro h
  have h1 := h 0 3 (by decide) (by decide)
  revert h1
  decide

theorem docIntegrand_scmp : SCmp docIntegrand := by
  intro x y hxy hx
  unfold docIntegrand fMinusG at *
  have hx2 : 2 < x := by
    rcases Nat.lt_or_ge 2 x with h | h
    · exact h
    · have h1 : 2 * (x : Int) - 4 ≤ 0 := by omega
      have h2 : 0 ≤ ((x * x : Nat) : Int) := by omega
      have h3 := Int.mul_nonpos_of_nonneg_of_nonpos h2 h1
      omega
  have hsq : 0 ≤ ((y * y : Nat) : Int) := by omega
  have h4 : 0 ≤ 2 * (y : Int) - 4 := by omega
  exact Int.mul_nonneg hsq h4

theorem docIntegrand_not_scpm : ¬ SCpm docIntegrand := by
  intro h
  have h1 := h 1 3 (by decide) (by decide)
  revert h1
  decide

theorem neg_docIntegrand_scpm : SCpm (fun i => - docIntegrand i) :=
  (scpm_neg_iff_scmp docIntegrand).mpr docIntegrand_scmp

def sumTo : Nat → (Nat → Int) → Int
  | 0, _ => 0
  | n + 1, h => sumTo n h + h n

def psi (n : Nat) (Kz : Nat → Nat → Int) (u : Nat → Int) (y : Nat) : Int :=
  sumTo n (fun x => Kz x y * u x)

theorem karlin_ratio (n c y1 y2 : Nat) (Kz : Nat → Nat → Int) (u : Nat → Int)
    (hu_lo : ∀ x, x ≤ c → 0 ≤ u x) (hu_hi : ∀ x, c < x → u x ≤ 0)
    (hlo : ∀ x, x ≤ c → Kz x y2 * Kz c y1 ≤ Kz x y1 * Kz c y2)
    (hhi : ∀ x, c < x → Kz c y2 * Kz x y1 ≤ Kz c y1 * Kz x y2) :
    Kz c y1 * psi n Kz u y2 ≤ Kz c y2 * psi n Kz u y1 := by
  induction n with
  | zero => simp [psi, sumTo]
  | succ n ih =>
    simp only [psi, sumTo] at ih ⊢
    rw [Int.mul_add, Int.mul_add]
    apply Int.add_le_add ih
    rcases Nat.lt_or_ge c n with h | h
    · have h1 := Int.mul_le_mul_of_nonpos_right (hhi n h) (hu_hi n h)
      calc Kz c y1 * (Kz n y2 * u n) = Kz c y1 * Kz n y2 * u n := by ac_rfl
        _ ≤ Kz c y2 * Kz n y1 * u n := h1
        _ = Kz c y2 * (Kz n y1 * u n) := by ac_rfl
    · have h1 := Int.mul_le_mul_of_nonneg_right (hlo n h) (hu_lo n h)
      calc Kz c y1 * (Kz n y2 * u n) = Kz n y2 * Kz c y1 * u n := by ac_rfl
        _ ≤ Kz n y1 * Kz c y2 * u n := h1
        _ = Kz c y2 * (Kz n y1 * u n) := by ac_rfl

theorem karlin_step (n c y1 y2 : Nat) (Kz : Nat → Nat → Int) (u : Nat → Int)
    (hu_lo : ∀ x, x ≤ c → 0 ≤ u x) (hu_hi : ∀ x, c < x → u x ≤ 0)
    (hlo : ∀ x, x ≤ c → Kz x y2 * Kz c y1 ≤ Kz x y1 * Kz c y2)
    (hhi : ∀ x, c < x → Kz c y2 * Kz x y1 ≤ Kz c y1 * Kz x y2)
    (hpos1 : 0 < Kz c y1) (hpos2 : 0 ≤ Kz c y2) (hneg : psi n Kz u y1 < 0) :
    psi n Kz u y2 ≤ 0 := by
  have h1 := karlin_ratio n c y1 y2 Kz u hu_lo hu_hi hlo hhi
  have h2 := Int.mul_nonpos_of_nonneg_of_nonpos hpos2 (Int.le_of_lt hneg)
  rcases Int.lt_or_le 0 (psi n Kz u y2) with h | h
  · have h3 := Int.mul_pos hpos1 h
    omega
  · exact h

theorem karlin_scpm (n c : Nat) (Kz : Nat → Nat → Int) (u : Nat → Int)
    (hu_lo : ∀ x, x ≤ c → 0 ≤ u x) (hu_hi : ∀ x, c < x → u x ≤ 0)
    (htp2 : ∀ x1 x2 y1 y2, x1 ≤ x2 → 1 ≤ y1 → y1 ≤ y2 →
      Kz x1 y2 * Kz x2 y1 ≤ Kz x1 y1 * Kz x2 y2)
    (hpos : ∀ y, 1 ≤ y → 0 < Kz c y) :
    SCpmFrom 1 (psi n Kz u) := by
  intro y1 y2 hy1 hy hneg
  have hlo : ∀ x, x ≤ c → Kz x y2 * Kz c y1 ≤ Kz x y1 * Kz c y2 :=
    fun x hx => htp2 x c y1 y2 hx hy1 (Nat.le_of_lt hy)
  have hhi : ∀ x, c < x → Kz c y2 * Kz x y1 ≤ Kz c y1 * Kz x y2 :=
    fun x hx => htp2 c x y1 y2 (Nat.le_of_lt hx) hy1 (Nat.le_of_lt hy)
  exact karlin_step n c y1 y2 Kz u hu_lo hu_hi hlo hhi (hpos y1 hy1)
    (Int.le_of_lt (hpos y2 (by omega))) hneg

def Kanti (x y : Nat) : Int := if x + 1 = y then 1 else 2

def uex (i : Nat) : Int := 1 - 2 * (i : Int)

theorem karlin_fails_without_tp2 :
    (∀ x, x ≤ 0 → 0 ≤ uex x) ∧ (∀ x, 0 < x → uex x ≤ 0) ∧ (∀ y, 0 < Kanti 0 y) ∧
    ¬ (Kanti 0 2 * Kanti 1 1 ≤ Kanti 0 1 * Kanti 1 2) ∧
    ¬ SCpmFrom 1 (psi 2 Kanti uex) := by
  refine ⟨?_, ?_, ?_, ?_, ?_⟩
  · intro x hx
    have hx0 : x = 0 := by omega
    subst hx0
    decide
  · intro x hx
    unfold uex
    omega
  · intro y
    unfold Kanti
    split <;> decide
  · decide
  · intro h
    have h1 := h 1 2 (by decide) (by decide) (by decide)
    revert h1
    decide

def Kint (N : Nat) (G : Nat → Nat) (i Q : Nat) : Int := ((K N (G i) Q : Nat) : Int)

def Dpmu (n N : Nat) (G : Nat → Nat) (w : Nat → Int) (Q : Nat) : Int :=
  - sumTo n (fun i => Kint N G i Q * w i)

theorem sumTo_neg (n : Nat) (h : Nat → Int) : - sumTo n h = sumTo n (fun i => - h i) := by
  induction n with
  | zero => simp [sumTo]
  | succ n ih =>
    simp only [sumTo]
    rw [Int.neg_add, ih]

theorem Dpmu_eq_psi (n N : Nat) (G : Nat → Nat) (w : Nat → Int) (Q : Nat) :
    Dpmu n N G w Q = psi n (Kint N G) (fun i => - w i) Q := by
  unfold Dpmu psi
  rw [sumTo_neg]
  congr 1
  funext i
  rw [Int.mul_neg]

theorem pmu_orientation (n N c : Nat) (G : Nat → Nat) (w : Nat → Int)
    (hG : ∀ i j, i ≤ j → G i ≤ G j) (hc0 : 0 < G c) (hcN : G c < N)
    (hw_lo : ∀ i, i ≤ c → w i ≤ 0) (hw_hi : ∀ i, c < i → 0 ≤ w i) :
    SCpmFrom 1 (Dpmu n N G w) := by
  have hu_lo : ∀ x, x ≤ c → 0 ≤ (fun i => - w i) x := by
    intro x hx
    have h1 := hw_lo x hx
    show 0 ≤ - w x
    omega
  have hu_hi : ∀ x, c < x → (fun i => - w i) x ≤ 0 := by
    intro x hx
    have h1 := hw_hi x hx
    show - w x ≤ 0
    omega
  have htp2 : ∀ x1 x2 y1 y2, x1 ≤ x2 → 1 ≤ y1 → y1 ≤ y2 →
      Kint N G x1 y2 * Kint N G x2 y1 ≤ Kint N G x1 y1 * Kint N G x2 y2 := by
    intro x1 x2 y1 y2 hx hy1 hy
    unfold Kint
    have h1 := kernel_cross N (G x1) (G x2) y1 y2 (hG x1 x2 hx) hy1 hy
    exact_mod_cast h1
  have hpos : ∀ y, 1 ≤ y → 0 < Kint N G c y := by
    intro y _
    unfold Kint
    have h1 : 0 < K N (G c) y := Nat.mul_pos (Nat.pow_pos hc0) (by omega)
    exact_mod_cast h1
  have hk := karlin_scpm n c (Kint N G) (fun i => - w i) hu_lo hu_hi htp2 hpos
  intro y1 y2 hy1 hy hneg
  rw [Dpmu_eq_psi] at hneg ⊢
  exact hk y1 y2 hy1 hy hneg

def Gex (i : Nat) : Nat := 1 + 2 * i

def wex (i : Nat) : Int := 3 * (i : Int) - 1

theorem minus_sum_scpm : SCpmFrom 1 (Dpmu 2 4 Gex wex) := by
  apply pmu_orientation 2 4 0 Gex wex
  · intro i j hij
    unfold Gex
    omega
  · decide
  · decide
  · intro i hi
    have hi0 : i = 0 := by omega
    subst hi0
    decide
  · intro i hi
    unfold wex
    omega

theorem plus_sum_not_scpm : ¬ SCpmFrom 1 (fun Q => sumTo 2 (fun i => Kint 4 Gex i Q * wex i)) := by
  intro h
  have h1 := h 1 2 (by decide) (by decide) (by decide)
  revert h1
  decide

def fracLt (n1 d1 n2 d2 : Nat) : Prop := n1 * d2 < n2 * d1

def bGumbelNum (k : Nat) : Nat := k - 1

def bGumbelDen (k : Nat) : Nat := k * k

def bLogNum (p k : Nat) : Nat := p * (k - 1)

def bLogDen (p q k : Nat) : Nat := k * (p * k + q)

def ETullockNum (k : Nat) : Nat := k - 1

def ETullockDen (k : Nat) : Nat := k

def EF22Num (_k : Nat) : Nat := 2

def EF22Den (k : Nat) : Nat := k + 1

theorem individual_reversal :
    fracLt (bGumbelNum 3) (bGumbelDen 3) (bGumbelNum 2) (bGumbelDen 2) ∧
    fracLt (bLogNum 1 2) (bLogDen 1 5 2) (bLogNum 1 3) (bLogDen 1 5 3) := by
  unfold fracLt
  decide

theorem gumbel_decreasing :
    ∀ k, k ≤ 40 → 2 ≤ k →
      fracLt (bGumbelNum (k + 1)) (bGumbelDen (k + 1)) (bGumbelNum k) (bGumbelDen k) := by
  unfold fracLt
  decide

theorem logistic_max_at_khat3 :
    (∀ k, k ≤ 40 → 2 ≤ k → k ≠ 3 → k ≠ 4 →
      fracLt (bLogNum 1 k) (bLogDen 1 5 k) (bLogNum 1 3) (bLogDen 1 5 3)) ∧
    bLogNum 1 3 * bLogDen 1 5 4 = bLogNum 1 4 * bLogDen 1 5 3 := by
  unfold fracLt
  decide

theorem logistic_symmetric_prop2 :
    bLogNum 1 2 * bLogDen 1 1 3 = bLogNum 1 3 * bLogDen 1 1 2 ∧
    (∀ k, k ≤ 40 → 3 ≤ k →
      fracLt (bLogNum 1 (k + 1)) (bLogDen 1 1 (k + 1)) (bLogNum 1 k) (bLogDen 1 1 k)) := by
  unfold fracLt
  decide

theorem aggregate_reversal :
    (∀ k, 1 ≤ k →
      fracLt (ETullockNum k) (ETullockDen k) (ETullockNum (k + 1)) (ETullockDen (k + 1))) ∧
    (∀ k, fracLt (EF22Num (k + 1)) (EF22Den (k + 1)) (EF22Num k) (EF22Den k)) := by
  constructor
  · intro k hk
    obtain ⟨j, rfl⟩ : ∃ j, k = j + 1 := ⟨k - 1, by omega⟩
    unfold fracLt ETullockNum ETullockDen
    simp only [Nat.add_sub_cancel]
    simp only [Nat.mul_add, Nat.add_mul, Nat.mul_one, Nat.one_mul]
    omega
  · intro k
    unfold fracLt EF22Num EF22Den
    omega

end RyvkinDrugov
