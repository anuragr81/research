namespace MorganOrzenSefton

theorem foc_symmetric (n P x : Int) (hx : n * n * x = (n - 1) * P) :
    P * ((n - 1) * x) = (n * x) * (n * x) := by
  have h1 : (n * x) * (n * x) = (n * n * x) * x := by ac_rfl
  rw [h1, hx]
  ac_rfl

theorem foc_symmetric_unique (n P x : Int) (hxpos : 0 < x)
    (hfoc : P * ((n - 1) * x) = (n * x) * (n * x)) :
    n * n * x = (n - 1) * P := by
  have e1 : x * ((n - 1) * P) = P * ((n - 1) * x) := by ac_rfl
  have e2 : x * (n * n * x) = (n * x) * (n * x) := by ac_rfl
  have h1 : x * ((n - 1) * P) = x * (n * n * x) := by
    rw [e1, e2]
    exact hfoc
  exact (Int.eq_of_mul_eq_mul_left (by omega) h1).symm

theorem best_response (P Y X s : Int) (hX : 0 ≤ X) (hfoc : X * X = P * Y) :
    (s - Y) * (P - s) * X ≤ (X - Y) * (P - X) * s := by
  have hsq : 0 ≤ (s - X) * (s - X) := by
    rcases Int.le_total 0 (s - X) with h | h
    · exact Int.mul_nonneg h h
    · exact Int.mul_nonneg_of_nonpos_of_nonpos h h
  have hk : 0 ≤ X * ((s - X) * (s - X)) := Int.mul_nonneg hX hsq
  have h1 : s * (X * X) = s * (P * Y) := by rw [hfoc]
  have h2 : X * (X * X) = X * (P * Y) := by rw [hfoc]
  simp only [Int.mul_sub, Int.mul_comm, Int.mul_left_comm] at hk h1 h2 ⊢
  omega

theorem symmetric_best_response (n P x z : Int) (hn : 0 ≤ n) (hx0 : 0 ≤ x)
    (hx : n * n * x = (n - 1) * P) :
    z * (P - (z + (n - 1) * x)) * (n * x) ≤ x * (P - n * x) * (z + (n - 1) * x) := by
  have hfoc : (n * x) * (n * x) = P * ((n - 1) * x) := (foc_symmetric n P x hx).symm
  have h := best_response P ((n - 1) * x) (n * x) (z + (n - 1) * x)
    (Int.mul_nonneg hn hx0) hfoc
  have e1 : z + (n - 1) * x - (n - 1) * x = z := by omega
  have e2 : n * x - (n - 1) * x = x := by
    rw [Int.sub_mul, Int.one_mul]
    omega
  rw [e1, e2] at h
  exact h

theorem equilibrium_payoff (n P x : Int) (hx : n * n * x = (n - 1) * P) :
    n * n * (x * P - x * (n * x)) = P * (n * x) := by
  have h1 : n * n * x * P = (n - 1) * P * P := by rw [hx]
  have h2 : n * n * x * (n * x) = (n - 1) * P * (n * x) := by rw [hx]
  simp only [Int.mul_sub, Int.mul_one, Int.mul_comm, Int.mul_left_comm] at h1 h2 ⊢
  omega

def StaysIn (P F m : Nat) : Prop := F * (m * m) < P

def IsLargestCount (P F n : Nat) : Prop :=
  StaysIn P F n ∧ ∀ m, n < m → ¬ StaysIn P F m

def IsFloorSqrt (P F n : Nat) : Prop :=
  F * (n * n) ≤ P ∧ P < F * ((n + 1) * (n + 1))

def Generic (P F : Nat) : Prop := ∀ k, F * (k * k) ≠ P

theorem sq_mono (F a b : Nat) (h : a ≤ b) : F * (a * a) ≤ F * (b * b) :=
  Nat.mul_le_mul_left F (Nat.mul_le_mul h h)

theorem largest_iff_floor (P F n : Nat) (hg : Generic P F) :
    IsLargestCount P F n ↔ IsFloorSqrt P F n := by
  constructor
  · intro h
    have hin := h.1
    have h1 := h.2 (n + 1) (Nat.lt_succ_self n)
    have h2 := hg (n + 1)
    unfold StaysIn at hin h1
    exact ⟨Nat.le_of_lt hin, by omega⟩
  · intro h
    have h0 := hg n
    have hle := h.1
    have hlt := h.2
    refine ⟨by unfold StaysIn; omega, ?_⟩
    intro m hm
    have := sq_mono F (n + 1) m hm
    unfold StaysIn
    omega

theorem floor_unique (P F a b : Nat) (ha : IsFloorSqrt P F a) (hb : IsFloorSqrt P F b) :
    a = b := by
  have ha1 := ha.1
  have ha2 := ha.2
  have hb1 := hb.1
  have hb2 := hb.2
  rcases Nat.lt_trichotomy a b with h | h | h
  · have := sq_mono F (a + 1) b h
    omega
  · exact h
  · have := sq_mono F (b + 1) a h
    omega

def rootSearch (q : Nat) : Nat → Nat
  | 0 => 0
  | k + 1 => if (k + 1) * (k + 1) ≤ q then k + 1 else rootSearch q k

def floorRoot (q : Nat) : Nat := rootSearch q q

theorem rootSearch_spec (q : Nat) : ∀ k,
    rootSearch q k * rootSearch q k ≤ q ∧
    ∀ j, rootSearch q k < j → j ≤ k → q < j * j
  | 0 => by
    refine ⟨Nat.zero_le q, ?_⟩
    intro j h1 h2
    unfold rootSearch at h1
    exfalso
    omega
  | k + 1 => by
    rcases Nat.lt_or_ge q ((k + 1) * (k + 1)) with hk | hk
    · have hn : ¬ (k + 1) * (k + 1) ≤ q := Nat.not_le_of_gt hk
      have e : rootSearch q (k + 1) = rootSearch q k := by
        simp only [rootSearch, hn, ↓reduceIte]
      rw [e]
      have ih := rootSearch_spec q k
      refine ⟨ih.1, ?_⟩
      intro j h1 h2
      rcases Nat.lt_or_ge j (k + 1) with hj | hj
      · exact ih.2 j h1 (by omega)
      · have : j = k + 1 := by omega
        rw [this]
        exact hk
    · have e : rootSearch q (k + 1) = k + 1 := by
        simp only [rootSearch, hk, ↓reduceIte]
      rw [e]
      refine ⟨hk, ?_⟩
      intro j h1 h2
      exfalso
      omega

theorem floorRoot_spec (q : Nat) :
    floorRoot q * floorRoot q ≤ q ∧ q < (floorRoot q + 1) * (floorRoot q + 1) := by
  have h := rootSearch_spec q q
  unfold floorRoot
  refine ⟨h.1, ?_⟩
  rcases Nat.lt_or_ge q (rootSearch q q + 1) with hr | hr
  · have := Nat.le_mul_self (rootSearch q q + 1)
    omega
  · exact h.2 (rootSearch q q + 1) (Nat.lt_succ_self _) hr

theorem floor_iff_root (P F n : Nat) (hF : 0 < F) :
    IsFloorSqrt P F n ↔ n = floorRoot (P / F) := by
  have hs1 := (floorRoot_spec (P / F)).1
  have hs2 := (floorRoot_spec (P / F)).2
  have hs1' : F * (floorRoot (P / F) * floorRoot (P / F)) ≤ P := by
    rw [Nat.mul_comm]
    exact (Nat.le_div_iff_mul_le hF).mp hs1
  have hs2' : P < F * ((floorRoot (P / F) + 1) * (floorRoot (P / F) + 1)) := by
    rw [Nat.mul_comm]
    exact (Nat.div_lt_iff_lt_mul hF).mp hs2
  constructor
  · intro h
    exact floor_unique P F n (floorRoot (P / F)) h ⟨hs1', hs2'⟩
  · intro h
    rw [h]
    exact ⟨hs1', hs2'⟩

theorem boundary_exists (P F : Nat) (hPF : F < P) :
    ∀ N, P ≤ F * (N * N) →
      ∃ n, 1 ≤ n ∧ n < N ∧ F * (n * n) < P ∧ P ≤ F * ((n + 1) * (n + 1))
  | 0, h => by
    simp only [Nat.mul_zero] at h
    exfalso
    omega
  | k + 1, h => by
    rcases Nat.lt_or_ge (F * (k * k)) P with hk | hk
    · cases k with
      | zero =>
        simp only [Nat.zero_add, Nat.mul_one] at h
        exfalso
        omega
      | succ j => exact ⟨j + 1, by omega, by omega, hk, h⟩
    · obtain ⟨n, h1, h2, h3, h4⟩ := boundary_exists P F hPF k hk
      exact ⟨n, h1, by omega, h3, h4⟩

theorem count_exists (P F N : Nat) (hPF : F < P) (hN : P < F * (N * N)) :
    ∃ n, 1 ≤ n ∧ n < N ∧ IsLargestCount P F n := by
  obtain ⟨n, h1, h2, h3, h4⟩ := boundary_exists P F hPF N (Nat.le_of_lt hN)
  refine ⟨n, h1, h2, h3, ?_⟩
  intro m hm
  have := sq_mono F (n + 1) m hm
  unfold StaysIn
  omega

theorem generic_of_bracket (P F n : Nat) (h1 : F * (n * n) < P)
    (h2 : P < F * ((n + 1) * (n + 1))) : Generic P F := by
  intro k hk
  rcases Nat.lt_or_ge n k with h | h
  · have := sq_mono F (n + 1) k h
    omega
  · have := sq_mono F k n h
    omega

theorem design_hypotheses :
    (10 < 50 ∧ 50 < 10 * (6 * 6) ∧ Generic 50 10) ∧
    (10 < 200 ∧ 200 < 10 * (6 * 6) ∧ Generic 200 10) :=
  ⟨⟨by decide, by decide, generic_of_bracket 50 10 2 (by decide) (by decide)⟩,
   ⟨by decide, by decide, generic_of_bracket 200 10 4 (by decide) (by decide)⟩⟩

theorem design_small : floorRoot (50 / 10) = 2 :=
  ((floor_iff_root 50 10 2 (by decide)).mp ⟨by decide, by decide⟩).symm

theorem design_large : floorRoot (200 / 10) = 4 :=
  ((floor_iff_root 200 10 4 (by decide)).mp ⟨by decide, by decide⟩).symm

theorem design_large_count : IsLargestCount 200 10 4 :=
  (largest_iff_floor 200 10 4 design_hypotheses.2.2.2).mpr ⟨by decide, by decide⟩

theorem design_small_count : IsLargestCount 50 10 2 :=
  (largest_iff_floor 50 10 2 design_hypotheses.1.2.2).mpr ⟨by decide, by decide⟩

def roundsTo (P n t : Nat) : Bool :=
  decide (20 * ((n - 1) * P) ≤ 2 * t * (n * n) + n * n) &&
  decide (2 * t * (n * n) ≤ 20 * ((n - 1) * P) + n * n)

def table2 : List (Nat × Nat × Nat) :=
  [(50, 1, 0), (50, 2, 125), (50, 3, 111), (50, 4, 94), (50, 5, 80), (50, 6, 69),
   (200, 1, 0), (200, 2, 500), (200, 3, 444), (200, 4, 375), (200, 5, 320), (200, 6, 278)]

theorem table2_investment :
    table2.all (fun r => roundsTo r.1 r.2.1 r.2.2) = true := by decide

def entrants (s : List Bool) : Nat := s.count true

def NoExit (P F m : Nat) : Prop := F * (m * m) ≤ P

def NoEntry (P F m : Nat) : Prop := P ≤ F * ((m + 1) * (m + 1))

def PureEq (P F : Nat) (s : List Bool) : Prop :=
  (true ∈ s → NoExit P F (entrants s)) ∧ (false ∈ s → NoEntry P F (entrants s))

theorem count_true_add_false (s : List Bool) :
    s.count true + s.count false = s.length := by
  induction s with
  | nil => rfl
  | cons b l ih =>
    cases b
    · have e1 : (false :: l).count true = l.count true := List.count_cons_of_ne (by decide)
      have e2 : (false :: l).count false = l.count false + 1 := List.count_cons_self
      rw [e1, e2, List.length_cons]
      omega
    · have e1 : (true :: l).count true = l.count true + 1 := List.count_cons_self
      have e2 : (true :: l).count false = l.count false := List.count_cons_of_ne (by decide)
      rw [e1, e2, List.length_cons]
      omega

theorem mem_true_iff (s : List Bool) : true ∈ s ↔ 0 < entrants s :=
  List.count_pos_iff.symm

theorem mem_false_iff (s : List Bool) : false ∈ s ↔ entrants s < s.length := by
  rw [← List.count_pos_iff]
  have := count_true_add_false s
  unfold entrants
  constructor
  · intro h
    omega
  · intro h
    omega

theorem pure_eq_of_floor (P F : Nat) (s : List Bool) (h : IsFloorSqrt P F (entrants s)) :
    PureEq P F s :=
  ⟨fun _ => h.1, fun _ => Nat.le_of_lt h.2⟩

theorem pure_eq_iff_floor (P F N : Nat) (s : List Bool) (hg : Generic P F)
    (hPF : F < P) (hN : P < F * (N * N)) (hs : s.length = N) :
    PureEq P F s ↔ IsFloorSqrt P F (entrants s) := by
  constructor
  · intro h
    have hle : entrants s ≤ s.length := List.count_le_length
    have hN0 : 0 < N := by
      cases N with
      | zero =>
        simp only [Nat.mul_zero] at hN
        omega
      | succ j => omega
    have hlt : entrants s < N := by
      rcases Nat.lt_or_ge (entrants s) N with h1 | h1
      · exact h1
      · have hm : entrants s = N := by omega
        have ht : true ∈ s := (mem_true_iff s).mpr (by omega)
        have hx := h.1 ht
        unfold NoExit at hx
        rw [hm] at hx
        omega
    have hf : false ∈ s := (mem_false_iff s).mpr (by omega)
    have hout := h.2 hf
    unfold NoEntry at hout
    have hpos : 0 < entrants s := by
      rcases Nat.eq_zero_or_pos (entrants s) with h0 | h0
      · rw [h0] at hout
        simp only [Nat.zero_add, Nat.mul_one] at hout
        omega
      · exact h0
    have ht : true ∈ s := (mem_true_iff s).mpr hpos
    have hin := h.1 ht
    unfold NoExit at hin
    have hgen := hg (entrants s + 1)
    exact ⟨hin, by omega⟩
  · exact pure_eq_of_floor P F s

theorem count_pinned (P F N : Nat) (s t : List Bool) (hg : Generic P F)
    (hPF : F < P) (hN : P < F * (N * N)) (hs : s.length = N) (ht : t.length = N)
    (es : PureEq P F s) (et : PureEq P F t) : entrants s = entrants t :=
  floor_unique P F _ _ ((pure_eq_iff_floor P F N s hg hPF hN hs).mp es)
    ((pure_eq_iff_floor P F N t hg hPF hN ht).mp et)

theorem entrants_block (a b : Nat) :
    entrants (List.replicate a true ++ List.replicate b false) = a ∧
    entrants (List.replicate b false ++ List.replicate a true) = a := by
  unfold entrants
  simp only [List.count_append, List.count_replicate]
  constructor <;> simp

theorem identity_unpinned (P F N n : Nat) (hn : IsFloorSqrt P F n) (h1 : 1 ≤ n) (h2 : n < N) :
    PureEq P F (List.replicate n true ++ List.replicate (N - n) false) ∧
    PureEq P F (List.replicate (N - n) false ++ List.replicate n true) ∧
    (List.replicate n true ++ List.replicate (N - n) false).length = N ∧
    (List.replicate (N - n) false ++ List.replicate n true).length = N ∧
    (List.replicate n true ++ List.replicate (N - n) false).head? = some true ∧
    (List.replicate (N - n) false ++ List.replicate n true).head? = some false := by
  have e := entrants_block n (N - n)
  refine ⟨pure_eq_of_floor P F _ (by rw [e.1]; exact hn),
    pure_eq_of_floor P F _ (by rw [e.2]; exact hn), ?_, ?_, ?_, ?_⟩
  · simp only [List.length_append, List.length_replicate]
    omega
  · simp only [List.length_append, List.length_replicate]
    omega
  · obtain ⟨k, rfl⟩ : ∃ k, n = k + 1 := ⟨n - 1, by omega⟩
    rfl
  · obtain ⟨j, hj⟩ : ∃ j, N - n = j + 1 := ⟨N - n - 1, by omega⟩
    rw [hj]
    rfl

def profiles : Nat → List (List Bool)
  | 0 => [[]]
  | k + 1 => (profiles k).map (List.cons true) ++ (profiles k).map (List.cons false)

theorem mem_profiles : ∀ s : List Bool, s ∈ profiles s.length
  | [] => List.mem_singleton_self _
  | b :: l => by
    have ih := mem_profiles l
    cases b
    · exact List.mem_append_right _ (List.mem_map_of_mem ih)
    · exact List.mem_append_left _ (List.mem_map_of_mem ih)

theorem design_eq_sets :
    (profiles 6).length = 64 ∧ (profiles 6).Nodup ∧
    ((profiles 6).filter (fun s => s.count true == 4)).length = 15 ∧
    ((profiles 6).filter (fun s => s.count true == 2)).length = 15 := by decide

theorem design_roles :
    ∀ i, i < 6 →
      (∃ s ∈ profiles 6, s.length = 6 ∧ s.count true = 4 ∧ s[i]? = some true) ∧
      (∃ s ∈ profiles 6, s.length = 6 ∧ s.count true = 4 ∧ s[i]? = some false) := by
  decide

theorem design_identity_unpinned (i : Nat) (hi : i < 6) :
    (∃ s : List Bool, s.length = 6 ∧ PureEq 200 10 s ∧ s[i]? = some true) ∧
    (∃ s : List Bool, s.length = 6 ∧ PureEq 200 10 s ∧ s[i]? = some false) := by
  have hfl : IsFloorSqrt 200 10 4 := ⟨by decide, by decide⟩
  obtain ⟨⟨s, _, hs, hc, hv⟩, ⟨t, _, ht, tc, tv⟩⟩ := design_roles i hi
  refine ⟨⟨s, hs, pure_eq_of_floor 200 10 s ?_, hv⟩, ⟨t, ht, pure_eq_of_floor 200 10 t ?_, tv⟩⟩
  · unfold entrants
    rw [hc]
    exact hfl
  · unfold entrants
    rw [tc]
    exact hfl

theorem design_pure_eq_iff (s : List Bool) (hs : s.length = 6) :
    PureEq 200 10 s ↔ entrants s = 4 := by
  have hfl : IsFloorSqrt 200 10 4 := ⟨by decide, by decide⟩
  rw [pure_eq_iff_floor 200 10 6 s design_hypotheses.2.2.2 (by decide) (by decide) hs]
  constructor
  · intro h
    exact floor_unique 200 10 _ 4 h hfl
  · intro h
    rw [h]
    exact hfl

theorem design_small_pure_eq_iff (s : List Bool) (hs : s.length = 6) :
    PureEq 50 10 s ↔ entrants s = 2 := by
  have hfl : IsFloorSqrt 50 10 2 := ⟨by decide, by decide⟩
  rw [pure_eq_iff_floor 50 10 6 s design_hypotheses.1.2.2 (by decide) (by decide) hs]
  constructor
  · intro h
    exact floor_unique 50 10 _ 2 h hfl
  · intro h
    rw [h]
    exact hfl

def seqEntry (n : Nat) : Nat → List Nat → List Nat
  | _, [] => []
  | k, a :: l => if k < n then a :: seqEntry n (k + 1) l else seqEntry n k l

theorem seqEntry_take (n : Nat) : ∀ (σ : List Nat) (k : Nat), seqEntry n k σ = σ.take (n - k)
  | [], k => by
    unfold seqEntry
    rw [List.take_nil]
  | a :: l, k => by
    unfold seqEntry
    by_cases hk : k < n
    · simp only [hk, ↓reduceIte]
      rw [seqEntry_take n l (k + 1)]
      obtain ⟨j, hj⟩ : ∃ j, n - k = j + 1 := ⟨n - k - 1, by omega⟩
      rw [hj, List.take_succ_cons]
      have : n - (k + 1) = j := by omega
      rw [this]
    · simp only [hk, ↓reduceIte]
      rw [seqEntry_take n l k]
      have : n - k = 0 := by omega
      rw [this, List.take_zero, List.take_zero]

theorem prop1_count (n : Nat) (σ : List Nat) (h : n ≤ σ.length) :
    (seqEntry n 0 σ).length = n := by
  rw [seqEntry_take n σ 0, Nat.sub_zero, List.length_take]
  omega

theorem prop1_step_out (P F n k : Nat) (hn : IsFloorSqrt P F n) (hk : n ≤ k) :
    P < F * ((k + 1) * (k + 1)) := by
  have := sq_mono F (n + 1) (k + 1) (by omega)
  have := hn.2
  omega

theorem prop1_step_in (P F n k : Nat) (hg : Generic P F) (hn : IsFloorSqrt P F n)
    (hk : k < n) : F * ((k + 1) * (k + 1)) < P := by
  have := sq_mono F (k + 1) n hk
  have := hn.1
  have := hg n
  omega

theorem prop1_rule (P F n k : Nat) (hg : Generic P F) (hn : IsFloorSqrt P F n) :
    k < n ↔ F * ((k + 1) * (k + 1)) < P := by
  constructor
  · exact prop1_step_in P F n k hg hn
  · intro h
    rcases Nat.lt_or_ge k n with h1 | h1
    · exact h1
    · have := prop1_step_out P F n k hn h1
      omega

theorem prop1_first_enters (n i : Nat) (rest : List Nat) (hn : 1 ≤ n) :
    i ∈ seqEntry n 0 (i :: rest) := by
  rw [seqEntry_take n (i :: rest) 0, Nat.sub_zero]
  obtain ⟨j, rfl⟩ : ∃ j, n = j + 1 := ⟨n - 1, by omega⟩
  rw [List.take_succ_cons]
  exact List.mem_cons_self

theorem prop1_last_out (n i : Nat) (rest : List Nat) (hi : i ∉ rest) (hn : n ≤ rest.length) :
    i ∉ seqEntry n 0 (rest ++ [i]) := by
  rw [seqEntry_take n (rest ++ [i]) 0, Nat.sub_zero, List.take_append_of_le_length hn]
  intro h
  exact hi (List.mem_of_mem_take h)

theorem design_prop1_orders :
    seqEntry 4 0 [0, 1, 2, 3, 4, 5] = [0, 1, 2, 3] ∧
    seqEntry 4 0 [5, 4, 3, 2, 1, 0] = [5, 4, 3, 2] ∧
    seqEntry 2 0 [0, 1, 2, 3, 4, 5] = [0, 1] ∧
    seqEntry 2 0 [5, 4, 3, 2, 1, 0] = [5, 4] := by decide

theorem control_floor_needs_generic :
    ¬ Generic 40 10 ∧ IsLargestCount 40 10 1 ∧ IsFloorSqrt 40 10 2 := by
  refine ⟨fun h => h 2 (by decide), ⟨by unfold StaysIn; decide, ?_⟩, ⟨by decide, by decide⟩⟩
  intro m hm
  have := sq_mono 10 2 m hm
  unfold StaysIn
  omega

theorem control_count_needs_generic :
    10 < 40 ∧ 40 < 10 * (6 * 6) ∧
    PureEq 40 10 [true, false, false, false, false, false] ∧
    PureEq 40 10 [true, true, false, false, false, false] ∧
    entrants [true, false, false, false, false, false] = 1 ∧
    entrants [true, true, false, false, false, false] = 2 := by
  unfold PureEq NoExit NoEntry entrants
  decide

theorem control_count_needs_bound :
    ¬ (200 < 10 * (3 * 3)) ∧ PureEq 200 10 [true, true, true] ∧
    entrants [true, true, true] = 3 ∧ ¬ IsFloorSqrt 200 10 3 := by
  unfold PureEq NoExit NoEntry entrants IsFloorSqrt
  decide

theorem control_best_response_needs_foc :
    (3 : Int) * 3 ≠ 4 * 1 ∧ ¬ ((2 - 1) * (4 - 2) * 3 ≤ ((3 : Int) - 1) * (4 - 3) * 2) := by
  decide

theorem control_rounding : roundsTo 50 4 93 = false ∧ roundsTo 200 3 445 = false := by
  decide

end MorganOrzenSefton
