namespace FuLu

theorem eq7_from_lemmas
    (N : Nat) (Γ₀ V S C e π E : Int)
    (hpay : (N : Int) * π = V - (N : Int) * e + (N : Int) * S - (N : Int) * C)
    (htotal : E = (N : Int) * e)
    (hlemma5 : V = Γ₀ - (N : Int) * S)
    (hlemma4 : π = 0) :
    E = Γ₀ - (N : Int) * C := by
  subst hlemma4
  rw [Int.mul_zero] at hpay
  omega

theorem eq7_count_cap
    (N : Nat) (Γ₀ C E : Int)
    (h7 : E = Γ₀ - (N : Int) * C)
    (heffort : 0 ≤ E) :
    (N : Int) * C ≤ Γ₀ := by
  omega

theorem eq7_dissipation_exact
    (N : Nat) (Γ₀ C e E : Int)
    (htotal : E = (N : Int) * e)
    (h7 : E = Γ₀ - (N : Int) * C) :
    (N : Int) * (C + e) = Γ₀ := by
  rw [Int.mul_add]
  omega

theorem eq7_rhs_strictly_decreasing
    (Γ₀ C : Int) (N₁ N₂ : Nat)
    (hC : 0 < C) (hN : N₁ < N₂) :
    Γ₀ - (N₂ : Int) * C < Γ₀ - (N₁ : Int) * C := by
  have h : (N₁ : Int) * C < (N₂ : Int) * C :=
    Int.mul_lt_mul_of_pos_right (by omega) hC
  omega

theorem eq7_effort_bound
    (N : Nat) (Γ₀ C E : Int)
    (h7 : E = Γ₀ - (N : Int) * C)
    (hN : 2 ≤ N) (hC : 0 ≤ C) :
    E ≤ Γ₀ - 2 * C := by
  have h : (2 : Int) * C ≤ (N : Int) * C :=
    Int.mul_le_mul_of_nonneg_right (by omega) hC
  omega

theorem theorem1_exactly_two
    (N : Nat) (Γ₀ C E : Int)
    (h7 : E = Γ₀ - (N : Int) * C)
    (hN : 2 ≤ N) (hC : 0 < C)
    (hattain : Γ₀ - 2 * C ≤ E) :
    N = 2 ∧ E = Γ₀ - 2 * C := by
  have hle : (N : Int) * C ≤ 2 * C := by omega
  have h2 : (N : Int) ≤ 2 := Int.le_of_mul_le_mul_right hle hC
  have hN2 : N = 2 := by omega
  subst hN2
  exact ⟨rfl, h7⟩

theorem theorem1_needs_positive_cost :
    ¬ (∀ (N : Nat) (Γ₀ C E : Int),
        E = Γ₀ - (N : Int) * C → 2 ≤ N → 0 ≤ C → Γ₀ - 2 * C ≤ E → N = 2) := by
  intro h
  have h3 := h 3 10 0 10 (by decide) (by decide) (by decide) (by decide)
  omega

theorem zero_cost_effort_is_budget
    (N : Nat) (Γ₀ E : Int)
    (h7 : E = Γ₀ - (N : Int) * 0) :
    E = Γ₀ := by
  rw [Int.mul_zero] at h7
  omega

theorem fixed_rules_cap
    (N : Nat) (Γ₀ V S C e π E : Int)
    (hpay : (N : Int) * π = V - (N : Int) * e + (N : Int) * S - (N : Int) * C)
    (htotal : E = (N : Int) * e)
    (hfeasible : V ≤ Γ₀ - (N : Int) * S)
    (hentry : 0 ≤ π) :
    E ≤ Γ₀ - (N : Int) * C := by
  have h : 0 ≤ (N : Int) * π := Int.mul_nonneg (by omega) hentry
  omega

theorem fixed_rules_count_cap
    (N : Nat) (Γ₀ V S C e π E : Int)
    (hpay : (N : Int) * π = V - (N : Int) * e + (N : Int) * S - (N : Int) * C)
    (htotal : E = (N : Int) * e)
    (hfeasible : V ≤ Γ₀ - (N : Int) * S)
    (hentry : 0 ≤ π)
    (heffort : 0 ≤ E) :
    (N : Int) * C ≤ Γ₀ := by
  have h := fixed_rules_cap N Γ₀ V S C e π E hpay htotal hfeasible hentry
  omega

theorem eq7_iff_lemma4_and_lemma5
    (N : Nat) (Γ₀ V S C e π E : Int)
    (hN : 1 ≤ N)
    (hpay : (N : Int) * π = V - (N : Int) * e + (N : Int) * S - (N : Int) * C)
    (htotal : E = (N : Int) * e)
    (hfeasible : V ≤ Γ₀ - (N : Int) * S)
    (hentry : 0 ≤ π) :
    E = Γ₀ - (N : Int) * C ↔ (π = 0 ∧ V = Γ₀ - (N : Int) * S) := by
  constructor
  · intro h7
    have h0 : 0 ≤ (N : Int) * π := Int.mul_nonneg (by omega) hentry
    have hNπ : (N : Int) * π = 0 := by omega
    have hπ : π = 0 := by
      cases Int.lt_or_le 0 π with
      | inl hpos =>
        have : 0 < (N : Int) * π := Int.mul_pos (by omega) hpos
        omega
      | inr hle => omega
    refine ⟨hπ, ?_⟩
    omega
  · intro h
    exact eq7_from_lemmas N Γ₀ V S C e π E hpay htotal h.2 h.1

theorem eq7_fails_without_lemma4 :
    ¬ (∀ (N : Nat) (Γ₀ V S C e π E : Int),
        (N : Int) * π = V - (N : Int) * e + (N : Int) * S - (N : Int) * C →
        E = (N : Int) * e →
        V = Γ₀ - (N : Int) * S →
        0 ≤ π →
        E = Γ₀ - (N : Int) * C) := by
  intro h
  have h' := h 2 8 8 0 1 2 1 4 (by decide) (by decide) (by decide) (by decide)
  omega

theorem eq7_fails_without_lemma5 :
    ¬ (∀ (N : Nat) (Γ₀ V S C e π E : Int),
        (N : Int) * π = V - (N : Int) * e + (N : Int) * S - (N : Int) * C →
        E = (N : Int) * e →
        V ≤ Γ₀ - (N : Int) * S →
        π = 0 →
        E = Γ₀ - (N : Int) * C) := by
  intro h
  have h' := h 2 10 4 0 1 1 0 2 (by decide) (by decide) (by decide) (by decide)
  omega

theorem eq7_hypotheses_satisfiable :
    ∃ (N : Nat) (Γ₀ V S C e π E : Int),
      (N : Int) * π = V - (N : Int) * e + (N : Int) * S - (N : Int) * C ∧
      E = (N : Int) * e ∧
      V = Γ₀ - (N : Int) * S ∧
      π = 0 ∧
      0 < C ∧
      N = 2 ∧
      E = Γ₀ - (N : Int) * C :=
  ⟨2, 10, 16, -3, 1, 4, 0, 8, by decide⟩

def countAfter (π : Nat → Int) : Nat → Nat
  | 0 => 0
  | k + 1 => if 0 ≤ π (countAfter π k + 1) then countAfter π k + 1 else countAfter π k

def Enters (π : Nat → Int) (k : Nat) : Prop := 0 ≤ π (countAfter π k + 1)

instance (π : Nat → Int) (k : Nat) : Decidable (Enters π k) :=
  inferInstanceAs (Decidable (0 ≤ π (countAfter π k + 1)))

def IsLemma2Count (π : Nat → Int) (M n : Nat) : Prop :=
  n ≤ M ∧ (∀ m, 1 ≤ m → m ≤ n → 0 ≤ π m) ∧ (∀ m, n < m → m ≤ M → π m < 0)

theorem countAfter_succ_of_enters (π : Nat → Int) (k : Nat) (h : Enters π k) :
    countAfter π (k + 1) = countAfter π k + 1 := by
  unfold Enters at h
  simp [countAfter, h]

theorem countAfter_succ_of_not_enters (π : Nat → Int) (k : Nat) (h : ¬ Enters π k) :
    countAfter π (k + 1) = countAfter π k := by
  unfold Enters at h
  simp [countAfter, h]

theorem countAfter_le (π : Nat → Int) (k : Nat) : countAfter π k ≤ k := by
  induction k with
  | zero => simp [countAfter]
  | succ k ih =>
    by_cases h : Enters π k
    · rw [countAfter_succ_of_enters π k h]; omega
    · rw [countAfter_succ_of_not_enters π k h]; omega

theorem countAfter_mono (π : Nat → Int) (i d : Nat) :
    countAfter π i ≤ countAfter π (i + d) := by
  induction d with
  | zero => simp
  | succ d ih =>
    have hs : i + (d + 1) = (i + d) + 1 := by omega
    rw [hs]
    by_cases h : Enters π (i + d)
    · rw [countAfter_succ_of_enters π (i + d) h]; omega
    · rw [countAfter_succ_of_not_enters π (i + d) h]; omega

theorem stay_out_persists (π : Nat → Int) (i : Nat) (h : ¬ Enters π i) (d : Nat) :
    countAfter π (i + d) = countAfter π i ∧ ¬ Enters π (i + d) := by
  induction d with
  | zero => exact ⟨rfl, h⟩
  | succ d ih =>
    have hs : i + (d + 1) = (i + d) + 1 := by omega
    rw [hs, countAfter_succ_of_not_enters π (i + d) ih.2]
    refine ⟨ih.1, ?_⟩
    unfold Enters
    rw [countAfter_succ_of_not_enters π (i + d) ih.2, ih.1]
    exact h

theorem entrants_form_prefix (π : Nat → Int) (i j : Nat) (hij : i ≤ j) (hj : Enters π j) :
    Enters π i := by
  if hi : Enters π i then
    exact hi
  else
    have := (stay_out_persists π i hi (j - i)).2
    rw [show i + (j - i) = j by omega] at this
    exact absurd hj this

theorem countAfter_of_all_enter (π : Nat → Int) (n : Nat) (h : ∀ i, i < n → Enters π i) :
    countAfter π n = n := by
  induction n with
  | zero => rfl
  | succ n ih =>
    rw [countAfter_succ_of_enters π n (h n (by omega)), ih (fun i hi => h i (by omega))]

theorem enters_iff_position (π : Nat → Int) (M k : Nat) (hk : k < M) :
    Enters π k ↔ k < countAfter π M := by
  constructor
  · intro hk'
    have hall : ∀ i, i < k + 1 → Enters π i :=
      fun i hi => entrants_form_prefix π i k (by omega) hk'
    have h1 : countAfter π (k + 1) = k + 1 := countAfter_of_all_enter π (k + 1) hall
    have h2 := countAfter_mono π (k + 1) (M - (k + 1))
    rw [show k + 1 + (M - (k + 1)) = M by omega, h1] at h2
    omega
  · intro hlt
    if hi : Enters π k then
      exact hi
    else
      have h := (stay_out_persists π k hi (M - k)).1
      rw [show k + (M - k) = M by omega] at h
      have := countAfter_le π k
      omega

theorem countAfter_invariant (π : Nat → Int) (k : Nat) :
    (1 ≤ countAfter π k → 0 ≤ π (countAfter π k)) ∧
    (countAfter π k < k → π (countAfter π k + 1) < 0) := by
  induction k with
  | zero => simp [countAfter]
  | succ k ih =>
    if h : Enters π k then
      rw [countAfter_succ_of_enters π k h]
      refine ⟨fun _ => h, fun hlt => ?_⟩
      have hall : ∀ i, i < k → Enters π i :=
        fun i hi => entrants_form_prefix π i k (by omega) h
      have hc := countAfter_of_all_enter π k hall
      omega
    else
      rw [countAfter_succ_of_not_enters π k h]
      refine ⟨ih.1, fun _ => ?_⟩
      unfold Enters at h
      omega

theorem lemma2_count
    (π : Nat → Int)
    (hdec : ∀ a b, 1 ≤ a → a < b → π b < π a)
    (M : Nat) :
    IsLemma2Count π M (countAfter π M) := by
  have hinv := countAfter_invariant π M
  have hle := countAfter_le π M
  refine ⟨hle, ?_, ?_⟩
  · intro m hm1 hmn
    have hn := hinv.1 (by omega)
    if heq : m = countAfter π M then
      rw [heq]
      exact hn
    else
      have := hdec m (countAfter π M) hm1 (by omega)
      omega
  · intro m hnm hmM
    have h1 := hinv.2 (by omega)
    if heq : m = countAfter π M + 1 then
      rw [heq]
      exact h1
    else
      have := hdec (countAfter π M + 1) m (by omega) (by omega)
      omega

theorem lemma2_count_unique (π : Nat → Int) (M n₁ n₂ : Nat)
    (h₁ : IsLemma2Count π M n₁) (h₂ : IsLemma2Count π M n₂) :
    n₁ = n₂ := by
  rcases h₁ with ⟨h1M, h1pos, h1neg⟩
  rcases h₂ with ⟨h2M, h2pos, h2neg⟩
  apply Nat.le_antisymm
  · if h : n₁ ≤ n₂ then
      exact h
    else
      have a := h1pos n₁ (by omega) (Nat.le_refl _)
      have b := h2neg n₁ (by omega) h1M
      omega
  · if h : n₂ ≤ n₁ then
      exact h
    else
      have a := h2pos n₂ (by omega) (Nat.le_refl _)
      have b := h1neg n₂ (by omega) h2M
      omega

theorem lemma2_needs_decreasing :
    ¬ (∀ (π : Nat → Int) (M : Nat), IsLemma2Count π M (countAfter π M)) := by
  intro h
  have h' := (h (fun n => if n = 2 then 5 else -1) 3).2.2 2 (by decide) (by decide)
  exact absurd h' (by decide)

def simCount (s : Nat → Bool) : Nat → Nat
  | 0 => 0
  | k + 1 => simCount s k + (if s k then 1 else 0)

def SimultaneousNash (π : Nat → Int) (M : Nat) (s : Nat → Bool) : Prop :=
  ∀ k, k < M →
    (s k = true → 0 ≤ π (simCount s M)) ∧ (s k = false → π (simCount s M + 1) < 0)

theorem simultaneous_identity_not_pinned :
    ∃ (π : Nat → Int) (s t : Nat → Bool),
      (∀ a b, 1 ≤ a → a < b → π b < π a) ∧
      SimultaneousNash π 3 s ∧ SimultaneousNash π 3 t ∧
      simCount s 3 = simCount t 3 ∧ s 0 = true ∧ t 0 = false ∧
      countAfter π 3 = 2 ∧ Enters π 0 := by
  refine ⟨fun n => 5 - 2 * (n : Int), fun k => decide (k < 2), fun k => decide (1 ≤ k ∧ k < 3),
    ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · intro a b _ hab
    show 5 - 2 * (b : Int) < 5 - 2 * (a : Int)
    omega
  · unfold SimultaneousNash
    decide
  · unfold SimultaneousNash
    decide
  · decide
  · decide
  · decide
  · decide
  · unfold Enters
    decide

end FuLu
