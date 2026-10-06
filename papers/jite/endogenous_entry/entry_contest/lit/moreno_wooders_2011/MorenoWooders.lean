namespace MorenoWooders

def EqLit (G : Int → Int) (phi lo hi t : Int) : Prop :=
  lo ≤ t ∧ t ≤ hi ∧
    ∀ z, lo ≤ z → z ≤ hi → (z + phi < G t → z < t) ∧ (G t < z + phi → t < z)

def EqTie (G : Int → Int) (phi lo hi t : Int) : Prop :=
  lo ≤ t ∧ t ≤ hi ∧
    ∀ z, lo ≤ z → z ≤ hi → z ≠ t → (z + phi < G t → z < t) ∧ (G t < z + phi → t < z)

theorem eq3_gives_eqLit (G : Int → Int) (phi lo hi t : Int)
    (hlo : lo ≤ t) (hhi : t ≤ hi) (h3 : G t = t + phi) : EqLit G phi lo hi t := by
  refine ⟨hlo, hhi, ?_⟩
  intro z _ _
  constructor <;> intro h <;> omega

theorem eqLit_forces_eq3 (G : Int → Int) (phi lo hi t : Int)
    (h : EqLit G phi lo hi t) : G t = t + phi := by
  obtain ⟨hlo, hhi, hz⟩ := h
  obtain ⟨h1, h2⟩ := hz t hlo hhi
  have a : ¬ (t + phi < G t) := fun h => Int.lt_irrefl t (h1 h)
  have b : ¬ (G t < t + phi) := fun h => Int.lt_irrefl t (h2 h)
  clear h1 h2 hz
  omega

theorem corner_not_eqLit (G : Int → Int) (phi lo hi : Int)
    (hG : ∀ t, G t < lo + phi) (t : Int) : ¬ EqLit G phi lo hi t := by
  intro h
  have e := eqLit_forces_eq3 G phi lo hi t h
  have g := hG t
  have l := h.1
  omega

theorem corner_eqTie (G : Int → Int) (phi lo hi : Int)
    (hlohi : lo ≤ hi) (hG : G lo < lo + phi) : EqTie G phi lo hi lo := by
  refine ⟨Int.le_refl lo, hlohi, ?_⟩
  intro z hz1 _ hz3
  constructor <;> intro h <;> omega

theorem corner_eq3_fails :
    ∃ (G : Int → Int) (phi lo hi t : Int), EqTie G phi lo hi t ∧ G t ≠ t + phi :=
  ⟨fun _ => 0, 0, 1, 2, 1, corner_eqTie _ _ _ _ (by decide) (by decide), by decide⟩

theorem tstar_strict_anti_phi (G : Int → Int) (hG : ∀ a b, a ≤ b → G b ≤ G a)
    (phi1 phi2 t1 t2 : Int) (h1 : G t1 = t1 + phi1) (h2 : G t2 = t2 + phi2)
    (hphi : phi1 < phi2) : t2 < t1 := by
  rcases Int.lt_or_le t2 t1 with h | h
  · exact h
  · have m := hG t1 t2 h
    omega

theorem tstar_strict_anti_v (Gv : Int → Int → Int) (phi : Int)
    (hGt : ∀ v a b, a ≤ b → Gv v b ≤ Gv v a)
    (hGv : ∀ v1 v2 t, v1 < v2 → Gv v2 t < Gv v1 t)
    (v1 v2 t1 t2 : Int) (h1 : Gv v1 t1 = t1 + phi) (h2 : Gv v2 t2 = t2 + phi)
    (hv : v1 < v2) : t2 < t1 := by
  rcases Int.lt_or_le t2 t1 with h | h
  · exact h
  · have m := hGt v2 t1 t2 h
    have s := hGv v1 v2 t1 hv
    omega

theorem symmetric_common_utility
    (N : Nat) (Ui : Nat → (Nat → Int) → Int) (g : Int → Int)
    (hpriv : ∀ i (thr : Nat → Int) t, i < N →
      (∀ j, j < N → j ≠ i → thr j = t) → Ui i thr = g t)
    (thr : Nat → Int) (t : Int) (hsym : ∀ i, i < N → thr i = t) :
    ∀ i j, i < N → j < N → Ui i thr = Ui j thr := by
  intro i j hi hj
  rw [hpriv i thr t hi (fun k hk _ => hsym k hk),
      hpriv j thr t hj (fun k hk _ => hsym k hk)]

theorem symmetric_equilibrium_flat_cutoff
    (N : Nat) (Ui : Nat → (Nat → Int) → Int) (g : Int → Int) (phi : Int)
    (hpriv : ∀ i (thr : Nat → Int) t, i < N →
      (∀ j, j < N → j ≠ i → thr j = t) → Ui i thr = g t)
    (thr : Nat → Int) (t : Int) (hsym : ∀ i, i < N → thr i = t)
    (h3 : g t = t + phi) :
    ∀ i, i < N → ∀ z, (z + phi < Ui i thr ↔ z < t) ∧ (Ui i thr < z + phi ↔ t < z) := by
  intro i hi z
  rw [hpriv i thr t hi (fun k hk _ => hsym k hk), h3]
  exact ⟨⟨fun h => by omega, fun h => by omega⟩, ⟨fun h => by omega, fun h => by omega⟩⟩

def Ux (i : Nat) (thr : Nat → Int) : Int := if i = 0 then 10 - thr 1 else 10 - thr 0

theorem private_info_without_symmetry :
    (∀ i (thr : Nat → Int) t, i < 2 →
      (∀ j, j < 2 → j ≠ i → thr j = t) → Ux i thr = 10 - t) ∧
    ∃ thr : Nat → Int, Ux 0 thr ≠ Ux 1 thr := by
  refine ⟨?_, ⟨fun j => if j = 0 then 1 else 2, by decide⟩⟩
  intro i thr t hi h
  unfold Ux
  split
  · next h0 => rw [h 1 (by decide) (by omega)]
  · next h0 => rw [h 0 (by decide) (by omega)]

def entrants (z : Nat → Int) (t : Int) : Nat → List Nat
  | 0 => []
  | n + 1 => entrants z t n ++ (if z n < t then [n] else [])

theorem mem_entrants (z : Nat → Int) (t : Int) :
    ∀ N i, i ∈ entrants z t N ↔ i < N ∧ z i < t
  | 0, i => by simp [entrants]
  | N + 1, i => by
    have ih := mem_entrants z t N i
    simp only [entrants, List.mem_append]
    by_cases hN : z N < t
    · simp only [hN, ite_true, List.mem_singleton]
      constructor
      · intro h
        rcases h with h | h
        · have l := (ih.1 h).1
          exact ⟨by omega, (ih.1 h).2⟩
        · subst h
          exact ⟨by omega, hN⟩
      · intro ⟨h1, h2⟩
        by_cases e : i = N
        · exact Or.inr e
        · exact Or.inl (ih.2 ⟨by omega, h2⟩)
    · simp only [hN, ite_false, List.not_mem_nil, or_false]
      constructor
      · intro h
        exact ⟨by have := (ih.1 h).1; omega, (ih.1 h).2⟩
      · intro ⟨h1, h2⟩
        by_cases e : i = N
        · subst e
          exact absurd h2 hN
        · exact ih.2 ⟨by omega, h2⟩

theorem count_not_pinned :
    (entrants (fun _ => 1) 5 2).length = 2 ∧ (entrants (fun _ => 9) 5 2).length = 0 := by
  decide

theorem entrant_set_not_pinned :
    entrants (fun i => if i = 0 then 1 else 9) 5 2 = [0] ∧
    entrants (fun i => if i = 0 then 9 else 1) 5 2 = [1] := by
  decide

def Collapses {α : Type} (le : α → α → Prop) (D : Nat → α) (Q : Nat) : Prop :=
  ∃ t, ∀ j, j < Q → ∀ x, (le x (D j) ↔ le x t)

def ConstOn {α : Type} (D : Nat → α) (Q : Nat) : Prop :=
  ∀ i j, i < Q → j < Q → D i = D j

theorem collapse_iff_const {α : Type} (le : α → α → Prop)
    (lerefl : ∀ a, le a a) (leanti : ∀ a b, le a b → le b a → a = b)
    (D : Nat → α) (Q : Nat) (hQ : 0 < Q) :
    Collapses le D Q ↔ ConstOn D Q := by
  constructor
  · intro ⟨t, ht⟩ i j hi hj
    have ei : D i = t :=
      leanti _ _ ((ht i hi (D i)).1 (lerefl _)) ((ht i hi t).2 (lerefl _))
    have ej : D j = t :=
      leanti _ _ ((ht j hj (D j)).1 (lerefl _)) ((ht j hj t).2 (lerefl _))
    rw [ei, ej]
  · intro hc
    refine ⟨D 0, ?_⟩
    intro j hj x
    rw [hc j 0 hj hQ]

theorem strict_no_collapse {α : Type} (le lt : α → α → Prop)
    (lerefl : ∀ a, le a a) (leanti : ∀ a b, le a b → le b a → a = b)
    (ltirrefl : ∀ a, ¬ lt a a)
    (D : Nat → α) (Q : Nat) (hQ : 2 ≤ Q)
    (hstrict : ∀ i j, i < j → j < Q → lt (D j) (D i)) :
    ¬ Collapses le D Q := by
  intro hc
  have hconst := (collapse_iff_const le lerefl leanti D Q (by omega)).1 hc
  have e := hconst 0 1 (by omega) (by omega)
  have l := hstrict 0 1 (by decide) (by omega)
  rw [e] at l
  exact ltirrefl _ l

def Delta3 (j : Nat) : Int := if j = 0 then 12 else if j = 1 then 10 else 1

def Kap3 (j : Nat) : Int := if j = 0 then 2 else if j = 1 then 5 else 8

theorem Delta3_strict : ∀ i j, i < j → j < 3 → Delta3 j < Delta3 i := by
  intro i j hij hj
  have hb : ∀ i, i < 3 → ∀ j, j < 3 → i < j → Delta3 j < Delta3 i := by decide
  exact hb i (by omega) j hj hij

theorem Delta3_no_collapse : ¬ Collapses (fun a b : Int => a ≤ b) Delta3 3 := by
  intro ⟨t, ht⟩
  have a : (11 : Int) ≤ t := (ht 0 (by decide) 11).1 (by decide)
  have b : (11 : Int) ≤ Delta3 1 := (ht 1 (by decide) 11).2 a
  exact absurd b (by decide)

theorem Delta3_no_collapse_general : ¬ Collapses (fun a b : Int => a ≤ b) Delta3 3 :=
  strict_no_collapse (fun a b : Int => a ≤ b) (fun a b : Int => a < b)
    Int.le_refl (fun _ _ h1 h2 => Int.le_antisymm h1 h2) Int.lt_irrefl
    Delta3 3 (by decide) Delta3_strict

theorem antitone_constant_collapses :
    (∀ i j : Nat, i ≤ j → (fun _ : Nat => (5 : Int)) j ≤ (fun _ : Nat => (5 : Int)) i) ∧
    Collapses (fun a b : Int => a ≤ b) (fun _ => (5 : Int)) 3 :=
  ⟨fun _ _ _ => Int.le_refl 5, 5, fun _ _ _ => Iff.rfl⟩

theorem realised_decisions_flat (kap D : Nat → Int)
    (hk : ∀ i j, i < j → kap i < kap j) (hD : ∀ i j, i ≤ j → D j ≤ D i) :
    ∀ Q : Nat, ∃ t, ∀ j, j < Q → (kap j ≤ D j ↔ kap j ≤ t)
  | 0 => ⟨0, fun j hj => absurd hj (Nat.not_lt_zero j)⟩
  | Q + 1 => by
    obtain ⟨t, ht⟩ := realised_decisions_flat kap D hk hD Q
    by_cases hQ : kap Q ≤ D Q
    · refine ⟨kap Q, ?_⟩
      intro j hj
      rcases Nat.lt_or_ge j Q with h | h
      · have k1 := hk j Q h
        have d1 := hD j Q (Nat.le_of_lt h)
        clear ht
        exact ⟨fun _ => by omega, fun _ => by omega⟩
      · have e : j = Q := by omega
        subst e
        exact ⟨fun _ => Int.le_refl _, fun _ => hQ⟩
    · refine ⟨if t ≤ kap Q - 1 then t else kap Q - 1, ?_⟩
      intro j hj
      rcases Nat.lt_or_ge j Q with h | h
      · have e := ht j h
        have k1 := hk j Q h
        clear ht
        split
        · exact e
        · next hn =>
          have e2 := e.2
          clear e
          exact ⟨fun _ => by omega, fun _ => e2 (by omega)⟩
      · have e : j = Q := by omega
        subst e
        clear ht
        split
        · next hn => exact ⟨fun hc => absurd hc hQ, fun hc => absurd (by omega) hQ⟩
        · exact ⟨fun hc => absurd hc hQ, fun hc => absurd (by omega) hQ⟩

theorem Delta3_realised_flat_fit : ∀ j, j < 3 → (Kap3 j ≤ Delta3 j ↔ Kap3 j ≤ 6) := by
  intro j hj
  have : j = 0 ∨ j = 1 ∨ j = 2 := by omega
  rcases this with rfl | rfl | rfl <;> decide

def IsMaxOn (W : Int → Int → Int) (vbar lo hi w : Int) : Prop :=
  (∃ v t, 0 ≤ v ∧ v ≤ vbar ∧ lo ≤ t ∧ t ≤ hi ∧ W v t = w) ∧
  ∀ v t, 0 ≤ v → v ≤ vbar → lo ≤ t → t ≤ hi → W v t ≤ w

theorem root_unique (G : Int → Int) (hG : ∀ a b, a ≤ b → G b ≤ G a)
    (a b : Int) (ha : G a = a) (hb : G b = b) : a = b := by
  rcases Int.lt_or_le a b with h | h
  · have m := hG a b (Int.le_of_lt h)
    omega
  · rcases Int.lt_or_le b a with h' | h'
    · have m := hG b a (Int.le_of_lt h')
      omega
    · omega

theorem lemmaA1 (W : Int → Int → Int) (G : Int → Int) (vbar lo hi tW : Int)
    (hv : ∀ v t, 0 ≤ v → v ≤ vbar → W v t ≤ W 0 t)
    (hG : ∀ a b, a ≤ b → G b ≤ G a)
    (hinc : ∀ a b, lo ≤ a → a ≤ b → b ≤ hi →
      (∀ s, a ≤ s → s < b → s < G s) → W 0 a ≤ W 0 b)
    (hdec : ∀ a b, lo ≤ a → a ≤ b → b ≤ hi →
      (∀ s, a < s → s ≤ b → G s < s) → W 0 b ≤ W 0 a)
    (hvbar : 0 ≤ vbar) (hlo : lo ≤ tW) (hhi : tW ≤ hi) (hW : G tW = tW) :
    IsMaxOn W vbar lo hi (W 0 tW) := by
  refine ⟨⟨0, tW, Int.le_refl 0, hvbar, hlo, hhi, rfl⟩, ?_⟩
  intro v t hv0 hv1 ht0 ht1
  have h1 := hv v t hv0 hv1
  have h2 : W 0 t ≤ W 0 tW := by
    rcases Int.lt_or_le t tW with h | h
    · apply hinc t tW ht0 (Int.le_of_lt h) hhi
      intro s _ hs2
      have := hG s tW (Int.le_of_lt hs2)
      omega
    · apply hdec tW t hlo h ht1
      intro s hs1 _
      have := hG tW s (Int.le_of_lt hs1)
      omega
  exact Int.le_trans h1 h2

theorem prop3 (W : Int → Int → Int) (G : Int → Int) (vbar lo hi tW tstar : Int)
    (hv : ∀ v t, 0 ≤ v → v ≤ vbar → W v t ≤ W 0 t)
    (hG : ∀ a b, a ≤ b → G b ≤ G a)
    (hinc : ∀ a b, lo ≤ a → a ≤ b → b ≤ hi →
      (∀ s, a ≤ s → s < b → s < G s) → W 0 a ≤ W 0 b)
    (hdec : ∀ a b, lo ≤ a → a ≤ b → b ≤ hi →
      (∀ s, a < s → s ≤ b → G s < s) → W 0 b ≤ W 0 a)
    (hvbar : 0 ≤ vbar) (hlo : lo ≤ tW) (hhi : tW ≤ hi) (hW : G tW - tW = 0)
    (h3 : G tstar = tstar + 0) :
    IsMaxOn W vbar lo hi (W 0 tstar) := by
  have e : tstar = tW := root_unique G hG tstar tW (by omega) (by omega)
  rw [e]
  exact lemmaA1 W G vbar lo hi tW hv hG hinc hdec hvbar hlo hhi (by omega)

def Gc (t : Int) : Int := if t = 2 then 2 else 0

def Wc (v t : Int) : Int := (if t = 0 then 1 else 0) - v

theorem prop3_needs_monotone_U :
    (∀ v t, 0 ≤ v → v ≤ 1 → Wc v t ≤ Wc 0 t) ∧
    (∀ a b, 0 ≤ a → a ≤ b → b ≤ 2 →
      (∀ s, a ≤ s → s < b → s < Gc s) → Wc 0 a ≤ Wc 0 b) ∧
    (∀ a b, 0 ≤ a → a ≤ b → b ≤ 2 →
      (∀ s, a < s → s ≤ b → Gc s < s) → Wc 0 b ≤ Wc 0 a) ∧
    Gc 2 = 2 + 0 ∧
    ¬ (∀ a b, a ≤ b → Gc b ≤ Gc a) ∧
    ¬ IsMaxOn Wc 1 0 2 (Wc 0 2) := by
  refine ⟨?_, ?_, ?_, by decide, ?_, ?_⟩
  · intro v t h1 _
    unfold Wc
    split <;> omega
  · intro a b ha hab hb hs
    rcases Int.lt_or_le a b with h | h
    · have g := hs a (Int.le_refl a) h
      unfold Gc at g
      split at g <;> omega
    · have e : a = b := by omega
      subst e
      exact Int.le_refl _
  · intro a b ha hab hb hs
    rcases Int.lt_or_le a b with h | h
    · have g := hs b h (Int.le_refl b)
      have b1 : b = 1 := by
        unfold Gc at g
        split at g <;> omega
      have a0 : a = 0 := by omega
      subst b1
      subst a0
      decide
    · have e : a = b := by omega
      subst e
      exact Int.le_refl _
  · intro h
    exact absurd (h 1 2 (by decide)) (by decide)
  · intro h
    exact absurd (h.2 0 0 (by decide) (by decide) (by decide) (by decide)) (by decide)

theorem inframarginal_rent (G : Int → Int) (phi t z : Int) (h3 : G t = t + phi)
    (hz : z < t) : 0 < G t - phi - z ∧ G t - phi - t = 0 :=
  ⟨by omega, by omega⟩

end MorenoWooders
