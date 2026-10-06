namespace Suen2007

def psum (f : Nat → Int) : Nat → Int
  | 0 => 0
  | n + 1 => psum f n + f n

def wsum (a f : Nat → Int) : Nat → Int
  | 0 => 0
  | n + 1 => wsum a f n + a n * f n

def dsum (a f : Nat → Int) : Nat → Int
  | 0 => 0
  | n + 1 => dsum a f n + (a n - a (n + 1)) * psum f (n + 1)

theorem abel_end (a f : Nat → Int) (n : Nat) :
    wsum a f n = a n * psum f n + dsum a f n := by
  induction n with
  | zero => simp [wsum, psum, dsum]
  | succ n ih =>
    show wsum a f n + a n * f n
      = a (n + 1) * (psum f n + f n) + (dsum a f n + (a n - a (n + 1)) * (psum f n + f n))
    rw [ih]
    simp only [Int.mul_add, Int.sub_mul]
    omega

theorem eq6_identity (a I : Nat → Int) (m : Nat) :
    wsum a I (m + 1) = a m * psum I (m + 1) + dsum a I m := by
  rw [abel_end]
  show a (m + 1) * psum I (m + 1) + (dsum a I m + (a m - a (m + 1)) * psum I (m + 1))
    = a m * psum I (m + 1) + dsum a I m
  rw [Int.sub_mul]
  omega

theorem eq6_printed_not_identity :
    ∃ (a I : Nat → Int) (m : Nat),
      wsum a I (m + 1) ≠ a m * (-(psum I (m + 1))) + dsum a I m :=
  ⟨fun _ => 1, fun _ => -1, 0, by decide⟩

theorem dsum_nonpos (a I : Nat → Int) (m : Nat)
    (hmono : ∀ j, j < m → a (j + 1) ≤ a j)
    (hS : ∀ j, j < m → psum I (j + 1) ≤ 0) :
    dsum a I m ≤ 0 := by
  induction m with
  | zero => exact Int.le_refl 0
  | succ m ih =>
    have h1 : dsum a I m ≤ 0 :=
      ih (fun j hj => hmono j (Nat.lt_succ_of_lt hj)) (fun j hj => hS j (Nat.lt_succ_of_lt hj))
    have h2 : (a m - a (m + 1)) * psum I (m + 1) ≤ 0 :=
      Int.mul_nonpos_of_nonneg_of_nonpos
        (by have := hmono m (Nat.lt_succ_self m); omega) (hS m (Nat.lt_succ_self m))
    show dsum a I m + (a m - a (m + 1)) * psum I (m + 1) ≤ 0
    omega

theorem eq6_sign (a I : Nat → Int) (m : Nat)
    (hpos : 0 ≤ a m)
    (hmono : ∀ j, j < m → a (j + 1) ≤ a j)
    (hCL : ∀ j, j ≤ m → psum I (j + 1) ≤ 0) :
    wsum a I (m + 1) ≤ 0 := by
  rw [eq6_identity]
  have h1 : a m * psum I (m + 1) ≤ 0 :=
    Int.mul_nonpos_of_nonneg_of_nonpos hpos (hCL m (Nat.le_refl m))
  have h2 : dsum a I m ≤ 0 :=
    dsum_nonpos a I m hmono (fun j hj => hCL j (Nat.le_of_lt hj))
  omega

theorem dsum_nonpos_of_drops (c D : Nat → Int) (n : Nat)
    (hmono : ∀ t, t < n → c (t + 1) ≤ c t)
    (hdrop : ∀ t, t < n → c (t + 1) < c t → psum D (t + 1) ≤ 0) :
    dsum c D n ≤ 0 := by
  induction n with
  | zero => exact Int.le_refl 0
  | succ n ih =>
    have h1 : dsum c D n ≤ 0 :=
      ih (fun t ht => hmono t (Nat.lt_succ_of_lt ht))
        (fun t ht => hdrop t (Nat.lt_succ_of_lt ht))
    have h2 : (c n - c (n + 1)) * psum D (n + 1) ≤ 0 := by
      rcases Int.lt_or_eq_of_le (hmono n (Nat.lt_succ_self n)) with h | h
      · exact Int.mul_nonpos_of_nonneg_of_nonpos (by omega) (hdrop n (Nat.lt_succ_self n) h)
      · rw [h, Int.sub_self, Int.zero_mul]
        exact Int.le_refl 0
    show dsum c D n + (c n - c (n + 1)) * psum D (n + 1) ≤ 0
    omega

theorem wsum_nonpos_of_drops (c D : Nat → Int) (n : Nat)
    (hend : 0 ≤ c n)
    (hS : psum D n ≤ 0)
    (hmono : ∀ t, t < n → c (t + 1) ≤ c t)
    (hdrop : ∀ t, t < n → c (t + 1) < c t → psum D (t + 1) ≤ 0) :
    wsum c D n ≤ 0 := by
  rw [abel_end]
  have h1 : c n * psum D n ≤ 0 := Int.mul_nonpos_of_nonneg_of_nonpos hend hS
  have h2 := dsum_nonpos_of_drops c D n hmono hdrop
  omega

theorem recombination (s a d : Int) (h : (d ≤ 0 ∧ a ≤ s) ∨ (0 ≤ d ∧ s ≤ a)) :
    s * d ≤ a * d := by
  rcases h with ⟨hd, hs⟩ | ⟨hd, hs⟩
  · exact Int.mul_le_mul_of_nonpos_right hs hd
  · exact Int.mul_le_mul_of_nonneg_right hs hd

theorem range_bound (f D : Nat → Int) (α : Int) (lo : Nat) :
    ∀ m : Nat, (∀ t, lo ≤ t → t < lo + m → f t ≤ α * D t) →
      psum f (lo + m) - psum f lo ≤ α * (psum D (lo + m) - psum D lo) := by
  intro m
  induction m with
  | zero =>
    intro _
    simp
  | succ m ih =>
    intro h
    have h1 := ih (fun t h1 h2 => h t h1 (by omega))
    have h2 := h (lo + m) (by omega) (by omega)
    show psum f (lo + m) + f (lo + m) - psum f lo
      ≤ α * (psum D (lo + m) + D (lo + m) - psum D lo)
    rw [Int.mul_sub, Int.mul_add]
    rw [Int.mul_sub] at h1
    omega

theorem eq6_inequality (e o : Nat → Nat) (s D dphi : Nat → Int)
    (he0 : e 0 = 0)
    (heo : ∀ j, e j ≤ o j) (hoe : ∀ j, o j ≤ e (j + 1))
    (hneg : ∀ j t, e j ≤ t → t < o j → D t ≤ 0)
    (hpos : ∀ j t, o j ≤ t → t < e (j + 1) → 0 ≤ D t)
    (hs : ∀ t u, t ≤ u → s u ≤ s t)
    (htan : ∀ t, dphi t ≤ s t * D t) :
    ∀ i, psum dphi (e i) ≤
      wsum (fun j => s (o j)) (fun j => psum D (e (j + 1)) - psum D (e j)) i := by
  intro i
  induction i with
  | zero =>
    rw [he0]
    exact Int.le_refl 0
  | succ i ih =>
    have hle : e i ≤ e (i + 1) := Nat.le_trans (heo i) (hoe i)
    obtain ⟨m, hm⟩ : ∃ m, e (i + 1) = e i + m := ⟨e (i + 1) - e i, by omega⟩
    have hblock : ∀ t, e i ≤ t → t < e i + m → dphi t ≤ s (o i) * D t := by
      intro t h1 h2
      have ht := htan t
      have hr : s t * D t ≤ s (o i) * D t := by
        apply recombination
        by_cases hc : t < o i
        · exact Or.inl ⟨hneg i t h1 hc, hs t (o i) (Nat.le_of_lt hc)⟩
        · exact Or.inr ⟨hpos i t (by omega) (by omega), hs (o i) t (by omega)⟩
      exact Int.le_trans ht hr
    have hb := range_bound dphi D (s (o i)) (e i) m hblock
    rw [← hm] at hb
    show psum dphi (e (i + 1)) ≤
      wsum (fun j => s (o j)) (fun j => psum D (e (j + 1)) - psum D (e j)) i
        + s (o i) * (psum D (e (i + 1)) - psum D (e i))
    omega

theorem telescope (g : Nat → Int) :
    ∀ n, psum (fun j => g (j + 1) - g j) n = g n - g 0 := by
  intro n
  induction n with
  | zero => simp [psum]
  | succ n ih =>
    show psum (fun j => g (j + 1) - g j) n + (g (n + 1) - g n) = g (n + 1) - g 0
    omega

theorem e_mono (e o : Nat → Nat) (heo : ∀ j, e j ≤ o j) (hoe : ∀ j, o j ≤ e (j + 1)) :
    ∀ j d, e j ≤ e (j + d) := by
  intro j d
  induction d with
  | zero => exact Nat.le_refl _
  | succ d ih =>
    have := heo (j + d)
    have := hoe (j + d)
    show e j ≤ e (j + d + 1)
    omega

theorem eq6_hhat_nonpos (e o : Nat → Nat) (s D dphi : Nat → Int)
    (he0 : e 0 = 0)
    (heo : ∀ j, e j ≤ o j) (hoe : ∀ j, o j ≤ e (j + 1))
    (hneg : ∀ j t, e j ≤ t → t < o j → D t ≤ 0)
    (hpos : ∀ j t, o j ≤ t → t < e (j + 1) → 0 ≤ D t)
    (hs : ∀ t u, t ≤ u → s u ≤ s t)
    (hs0 : ∀ t, 0 ≤ s t)
    (htan : ∀ t, dphi t ≤ s t * D t)
    (n : Nat)
    (hCL : ∀ j, j ≤ n → psum D (e j) ≤ 0) :
    ∀ i, i ≤ n → psum dphi (e i) ≤ 0 := by
  intro i hi
  cases i with
  | zero =>
    rw [he0]
    exact Int.le_refl 0
  | succ m =>
    have h1 := eq6_inequality e o s D dphi he0 heo hoe hneg hpos hs htan (m + 1)
    have h2 : wsum (fun j => s (o j)) (fun j => psum D (e (j + 1)) - psum D (e j)) (m + 1) ≤ 0 := by
      apply eq6_sign
      · exact hs0 (o m)
      · intro j _
        have := heo (j + 1)
        have := hoe j
        exact hs (o j) (o (j + 1)) (by omega)
      · intro j hj
        rw [telescope (fun j => psum D (e j)) (j + 1)]
        have := hCL (j + 1) (by omega)
        rw [he0]
        show psum D (e (j + 1)) - 0 ≤ 0
        omega
    exact Int.le_trans h1 h2

theorem psum_antitone_on (f : Nat → Int) (lo hi : Nat)
    (h : ∀ t, lo ≤ t → t < hi → f t ≤ 0) :
    ∀ m, lo + m ≤ hi → psum f (lo + m) ≤ psum f lo := by
  intro m
  induction m with
  | zero => intro _; exact Int.le_refl _
  | succ m ih =>
    intro hm
    have h1 := ih (by omega)
    have h2 := h (lo + m) (by omega) (by omega)
    show psum f (lo + m) + f (lo + m) ≤ psum f lo
    omega

theorem psum_monotone_on (f : Nat → Int) (x : Nat) :
    ∀ m, (∀ u, x ≤ u → u < x + m → 0 ≤ f u) → psum f x ≤ psum f (x + m) := by
  intro m
  induction m with
  | zero => intro _; exact Int.le_refl _
  | succ m ih =>
    intro h
    have h1 := ih (fun u hu1 hu2 => h u hu1 (by omega))
    have h2 := h (x + m) (by omega) (by omega)
    show psum f x ≤ psum f (x + m) + f (x + m)
    omega

theorem block_cover (e : Nat → Nat) (he0 : e 0 = 0) :
    ∀ n t, t < e n → ∃ j, j < n ∧ e j ≤ t ∧ t < e (j + 1) := by
  intro n
  induction n with
  | zero => intro t ht; omega
  | succ n ih =>
    intro t ht
    by_cases hc : t < e n
    · obtain ⟨j, hj, h1, h2⟩ := ih t hc
      exact ⟨j, by omega, h1, h2⟩
    · exact ⟨n, by omega, by omega, ht⟩

theorem hhat_nonpos_between (e o : Nat → Nat) (D dphi : Nat → Int)
    (he0 : e 0 = 0)
    (hneg : ∀ j t, e j ≤ t → t < o j → D t ≤ 0)
    (hpos : ∀ j t, o j ≤ t → t < e (j + 1) → 0 ≤ D t)
    (hsgn_neg : ∀ t, D t ≤ 0 → dphi t ≤ 0)
    (hsgn_pos : ∀ t, 0 ≤ D t → 0 ≤ dphi t)
    (n : Nat)
    (hmax : ∀ j, j ≤ n → psum dphi (e j) ≤ 0) :
    ∀ t, t ≤ e n → psum dphi t ≤ 0 := by
  intro t ht
  by_cases hlt : t < e n
  · obtain ⟨j, hj, h1, h2⟩ := block_cover e he0 n t hlt
    by_cases hc : t ≤ o j
    · have hdec := psum_antitone_on dphi (e j) (o j)
        (fun u hu1 hu2 => hsgn_neg u (hneg j u hu1 hu2)) (t - e j) (by omega)
      have e1 : e j + (t - e j) = t := by omega
      rw [e1] at hdec
      have := hmax j (by omega)
      omega
    · have hinc := psum_monotone_on dphi t (e (j + 1) - t)
        (fun u hu1 hu2 => hsgn_pos u (hpos j u (by omega) (by omega)))
      exact Int.le_trans (by
        have e1 : t + (e (j + 1) - t) = e (j + 1) := by omega
        rw [e1] at hinc
        exact hinc) (hmax (j + 1) (by omega))
  · have e1 : t = e n := by omega
    rw [e1]
    exact hmax n (Nat.le_refl n)

theorem cl_at_even_crossings (e o : Nat → Nat) (D : Nat → Int)
    (he0 : e 0 = 0)
    (hneg : ∀ j t, e j ≤ t → t < o j → D t ≤ 0)
    (hpos : ∀ j t, o j ≤ t → t < e (j + 1) → 0 ≤ D t)
    (n : Nat)
    (hCL : ∀ j, j ≤ n → psum D (e j) ≤ 0) :
    ∀ t, t ≤ e n → psum D t ≤ 0 :=
  hhat_nonpos_between e o D D he0 hneg hpos (fun _ h => h) (fun _ h => h) n hCL

theorem eq5_identity (tp dphi : Nat → Int) (x : Nat) :
    wsum tp dphi x = tp x * psum dphi x + dsum tp dphi x :=
  abel_end tp dphi x

theorem eq5_H_nonpos (tp dphi : Nat → Int) (x : Nat)
    (htp0 : 0 ≤ tp x)
    (htp : ∀ t, t < x → tp (t + 1) ≤ tp t)
    (hHhat : ∀ t, t ≤ x → psum dphi t ≤ 0) :
    wsum tp dphi x ≤ 0 :=
  wsum_nonpos_of_drops tp dphi x htp0 (hHhat x (Nat.le_refl x)) htp
    (fun t ht _ => hHhat (t + 1) ht)

theorem psum_mul_eq_wsum (tp dphi : Nat → Int) :
    ∀ x, psum (fun t => tp t * dphi t) x = wsum tp dphi x := by
  intro x
  induction x with
  | zero => rfl
  | succ x ih =>
    show psum (fun t => tp t * dphi t) x + tp x * dphi x = wsum tp dphi x + tp x * dphi x
    rw [ih]

theorem wsum_const_sub (C : Int) (g D : Nat → Int) :
    ∀ n, wsum (fun t => C - g t) D n = C * psum D n - wsum g D n := by
  intro n
  induction n with
  | zero => simp [wsum, psum]
  | succ n ih =>
    show wsum (fun t => C - g t) D n + (C - g n) * D n
      = C * (psum D n + D n) - (wsum g D n + g n * D n)
    rw [ih, Int.sub_mul, Int.mul_add]
    omega

theorem product_rule (m Δ : Nat → Int) :
    ∀ n, wsum m (psum Δ) n + wsum (fun t => psum m (t + 1)) Δ n = psum m n * psum Δ n := by
  intro n
  induction n with
  | zero => simp [wsum, psum]
  | succ n ih =>
    show wsum m (psum Δ) n + m n * psum Δ n
        + (wsum (fun t => psum m (t + 1)) Δ n + (psum m n + m n) * Δ n)
      = (psum m n + m n) * (psum Δ n + Δ n)
    simp only [Int.add_mul, Int.mul_add]
    omega

theorem eq4_first_equality (m Δ : Nat → Int) (N : Nat) :
    wsum m (psum Δ) N = wsum (fun t => psum m N - psum m (t + 1)) Δ N := by
  rw [wsum_const_sub]
  have := product_rule m Δ N
  omega

theorem eq4_identity (ρ h : Nat → Int) (N : Nat) (hρN : ρ N = 0) :
    wsum ρ h N = dsum ρ h N := by
  rw [abel_end, hρN, Int.zero_mul, Int.zero_add]

theorem eq4_sign (ρ h : Nat → Int) (N : Nat)
    (hρN : ρ N = 0)
    (hρ : ∀ t, t < N → ρ (t + 1) ≤ ρ t)
    (hH : ∀ t, t ≤ N → psum h t ≤ 0) :
    wsum ρ h N ≤ 0 :=
  wsum_nonpos_of_drops ρ h N (by omega) (hH N (Nat.le_refl N)) hρ
    (fun t ht _ => hH (t + 1) ht)

theorem prop2_skeleton (e o : Nat → Nat) (s D dphi tp ρ : Nat → Int) (n : Nat)
    (he0 : e 0 = 0)
    (heo : ∀ j, e j ≤ o j) (hoe : ∀ j, o j ≤ e (j + 1))
    (hneg : ∀ j t, e j ≤ t → t < o j → D t ≤ 0)
    (hpos : ∀ j t, o j ≤ t → t < e (j + 1) → 0 ≤ D t)
    (hs : ∀ t u, t ≤ u → s u ≤ s t)
    (hs0 : ∀ t, 0 ≤ s t)
    (htan : ∀ t, dphi t ≤ s t * D t)
    (hsgn_neg : ∀ t, D t ≤ 0 → dphi t ≤ 0)
    (hsgn_pos : ∀ t, 0 ≤ D t → 0 ≤ dphi t)
    (hCL : ∀ j, j ≤ n → psum D (e j) ≤ 0)
    (htp0 : ∀ t, 0 ≤ tp t)
    (htp : ∀ t, tp (t + 1) ≤ tp t)
    (hρN : ρ (e n) = 0)
    (hρ : ∀ t, t < e n → ρ (t + 1) ≤ ρ t) :
    wsum ρ (fun t => tp t * dphi t) (e n) ≤ 0 := by
  have hmax := eq6_hhat_nonpos e o s D dphi he0 heo hoe hneg hpos hs hs0 htan n hCL
  have hall := hhat_nonpos_between e o D dphi he0 hneg hpos hsgn_neg hsgn_pos n hmax
  apply eq4_sign ρ _ (e n) hρN hρ
  intro x hx
  rw [psum_mul_eq_wsum]
  exact eq5_H_nonpos tp dphi x (htp0 x) (fun t _ => htp t)
    (fun t ht => hall t (Nat.le_trans ht hx))

theorem psum_le_wsum (s D dphi : Nat → Int) (htan : ∀ t, dphi t ≤ s t * D t) :
    ∀ x, psum dphi x ≤ wsum s D x := by
  intro x
  induction x with
  | zero => exact Int.le_refl 0
  | succ x ih =>
    have := htan x
    show psum dphi x + dphi x ≤ wsum s D x + s x * D x
    omega

theorem hhat_nonpos_direct (s D dphi : Nat → Int) (x : Nat)
    (hs : ∀ t, s (t + 1) ≤ s t)
    (hs0 : ∀ t, 0 ≤ s t)
    (htan : ∀ t, dphi t ≤ s t * D t)
    (hCL : ∀ t, t ≤ x → psum D t ≤ 0) :
    psum dphi x ≤ 0 :=
  Int.le_trans (psum_le_wsum s D dphi htan x)
    (wsum_nonpos_of_drops s D x (hs0 x) (hCL x (Nat.le_refl x)) (fun t _ => hs t)
      (fun t ht _ => hCL (t + 1) ht))

def pos (z : Int) : Int := max z 0

def shortfall (x : Nat → Int) (N : Nat) (y : Int) : Int :=
  psum (fun i => pos (y - x i)) N

def Sorted (x : Nat → Int) (N : Nat) : Prop := ∀ i j, i ≤ j → j < N → x i ≤ x j

theorem psum_le_psum (f g : Nat → Int) (h : ∀ i, f i ≤ g i) :
    ∀ k, psum f k ≤ psum g k := by
  intro k
  induction k with
  | zero => exact Int.le_refl 0
  | succ k ih =>
    have := h k
    show psum f k + f k ≤ psum g k + g k
    omega

theorem psum_le_of_nonneg (f : Nat → Int) (h : ∀ i, 0 ≤ f i) (k : Nat) :
    ∀ d, psum f k ≤ psum f (k + d) := by
  intro d
  induction d with
  | zero => exact Int.le_refl _
  | succ d ih =>
    have := h (k + d)
    show psum f k ≤ psum f (k + d) + f (k + d)
    omega

theorem shortfall_ge (x : Nat → Int) (N : Nat) (y : Int) (k : Nat) (hk : k ≤ N) :
    psum (fun i => y - x i) k ≤ shortfall x N y := by
  have h1 := psum_le_psum (fun i => y - x i) (fun i => pos (y - x i))
    (fun i => by show y - x i ≤ max (y - x i) 0; omega) k
  have h2 := psum_le_of_nonneg (fun i => pos (y - x i))
    (fun i => by show 0 ≤ max (y - x i) 0; omega) k (N - k)
  have e1 : k + (N - k) = N := by omega
  rw [e1] at h2
  exact Int.le_trans h1 h2

theorem shortfall_below (x : Nat → Int) (N k : Nat) (hs : Sorted x N) (hkN : k + 1 ≤ N) :
    ∀ n, n ≤ k + 1 →
      psum (fun i => pos (x k - x i)) n = psum (fun i => x k - x i) n := by
  intro n
  induction n with
  | zero => intro _; rfl
  | succ n ih =>
    intro hn
    have h1 := ih (by omega)
    have h2 := hs n k (by omega) (by omega)
    show psum (fun i => pos (x k - x i)) n + max (x k - x n) 0
      = psum (fun i => x k - x i) n + (x k - x n)
    omega

theorem shortfall_above (x : Nat → Int) (N k : Nat) (hs : Sorted x N) :
    ∀ d, k + 1 + d ≤ N →
      psum (fun i => pos (x k - x i)) (k + 1 + d) = psum (fun i => pos (x k - x i)) (k + 1) := by
  intro d
  induction d with
  | zero => intro _; rfl
  | succ d ih =>
    intro hd
    have h1 := ih (by omega)
    have h2 := hs k (k + 1 + d) (by omega) (by omega)
    show psum (fun i => pos (x k - x i)) (k + 1 + d) + max (x k - x (k + 1 + d)) 0
      = psum (fun i => pos (x k - x i)) (k + 1)
    omega

theorem shortfall_at_quantile (x : Nat → Int) (N k : Nat) (hs : Sorted x N) (hkN : k + 1 ≤ N) :
    shortfall x N (x k) = psum (fun i => x k - x i) (k + 1) := by
  unfold shortfall
  have h1 := shortfall_above x N k hs (N - (k + 1)) (by omega)
  have e1 : k + 1 + (N - (k + 1)) = N := by omega
  rw [e1] at h1
  rw [h1]
  exact shortfall_below x N k hs hkN (k + 1) (Nat.le_refl _)

theorem psum_const_sub (y : Int) (x0 x1 : Nat → Int) :
    ∀ k, psum (fun i => y - x1 i) k - psum (fun i => y - x0 i) k = psum x0 k - psum x1 k := by
  intro k
  induction k with
  | zero => rfl
  | succ k ih =>
    show psum (fun i => y - x1 i) k + (y - x1 k) - (psum (fun i => y - x0 i) k + (y - x0 k))
      = psum x0 k + x0 k - (psum x1 k + x1 k)
    omega

theorem cl_quantile_reversal (x0 x1 : Nat → Int) (N : Nat)
    (h1 : Sorted x1 N)
    (hsosd : ∀ y, shortfall x0 N y ≤ shortfall x1 N y) :
    ∀ k, k ≤ N → psum x1 k ≤ psum x0 k := by
  intro k hk
  cases k with
  | zero => exact Int.le_refl 0
  | succ k =>
    have hA := shortfall_at_quantile x1 N k h1 hk
    have hB := shortfall_ge x0 N (x1 k) (k + 1) hk
    have hC := hsosd (x1 k)
    have hD := psum_const_sub (x1 k) x0 x1 (k + 1)
    omega

theorem psum_sub (f g : Nat → Int) :
    ∀ k, psum (fun t => f t - g t) k = psum f k - psum g k := by
  intro k
  induction k with
  | zero => rfl
  | succ k ih =>
    show psum (fun t => f t - g t) k + (f k - g k) = psum f k + f k - (psum g k + g k)
    omega

theorem prop2_from_sosd (e o : Nat → Nat) (x0 x1 s dphi tp ρ : Nat → Int) (n : Nat)
    (he0 : e 0 = 0)
    (heo : ∀ j, e j ≤ o j) (hoe : ∀ j, o j ≤ e (j + 1))
    (h1 : Sorted x1 (e n))
    (hsosd : ∀ y, shortfall x0 (e n) y ≤ shortfall x1 (e n) y)
    (hneg : ∀ j t, e j ≤ t → t < o j → x1 t - x0 t ≤ 0)
    (hpos : ∀ j t, o j ≤ t → t < e (j + 1) → 0 ≤ x1 t - x0 t)
    (hs : ∀ t u, t ≤ u → s u ≤ s t)
    (hs0 : ∀ t, 0 ≤ s t)
    (htan : ∀ t, dphi t ≤ s t * (x1 t - x0 t))
    (hsgn_neg : ∀ t, x1 t - x0 t ≤ 0 → dphi t ≤ 0)
    (hsgn_pos : ∀ t, 0 ≤ x1 t - x0 t → 0 ≤ dphi t)
    (htp0 : ∀ t, 0 ≤ tp t)
    (htp : ∀ t, tp (t + 1) ≤ tp t)
    (hρN : ρ (e n) = 0)
    (hρ : ∀ t, t < e n → ρ (t + 1) ≤ ρ t) :
    wsum ρ (fun t => tp t * dphi t) (e n) ≤ 0 := by
  apply prop2_skeleton e o s (fun t => x1 t - x0 t) dphi tp ρ n he0 heo hoe hneg hpos hs hs0
    htan hsgn_neg hsgn_pos _ htp0 htp hρN hρ
  intro j hj
  rw [psum_sub]
  have hej : e j ≤ e n := by
    have := e_mono e o heo hoe j (n - j)
    have e1 : j + (n - j) = n := by omega
    rw [e1] at this
    exact this
  have := cl_quantile_reversal x0 x1 (e n) h1 hsosd (e j) hej
  omega

def incrW : Nat → Int := fun j => (j : Int) + 1

def spreadI : Nat → Int
  | 0 => -1
  | _ => 1

theorem control_weight_increasing :
    psum spreadI 1 ≤ 0 ∧ psum spreadI 2 ≤ 0 ∧ 0 ≤ incrW 1 ∧ incrW 0 < incrW 1 ∧
      0 < wsum incrW spreadI 2 := by
  decide

def q0 : Nat → Int := fun _ => 1

def q1 : Nat → Int
  | 0 => 0
  | _ => 2

def sq (y : Int) : Int := y * y

def rhoC : Nat → Int
  | 0 => 1
  | 1 => 1
  | _ => 0

theorem control_convex_phi :
    psum (fun t => q1 t - q0 t) 0 ≤ 0 ∧ psum (fun t => q1 t - q0 t) 1 ≤ 0 ∧
      psum (fun t => q1 t - q0 t) 2 = 0 ∧
      sq 0 < sq 1 ∧ sq 1 < sq 2 ∧ sq 1 - sq 0 < sq 2 - sq 1 ∧
      ¬ (sq (q1 0) - sq (q0 0) ≤ 2 * (q1 0 - q0 0)) ∧
      rhoC 2 = 0 ∧ rhoC 1 ≤ rhoC 0 ∧ rhoC 2 ≤ rhoC 1 ∧
      0 < psum (fun t => sq (q1 t) - sq (q0 t)) 2 ∧
      0 < wsum rhoC (fun t => 1 * (sq (q1 t) - sq (q0 t))) 2 := by
  decide

def xUnsorted : Nat → Int
  | 0 => 2
  | _ => 0

theorem control_cl_unsorted :
    (∀ y, shortfall q0 2 y ≤ shortfall xUnsorted 2 y) ∧ ¬ Sorted xUnsorted 2 ∧
      psum q0 2 = psum xUnsorted 2 ∧ psum q0 1 < psum xUnsorted 1 := by
  refine ⟨?_, ?_, by decide, by decide⟩
  · intro y
    simp only [shortfall, psum, pos, q0, xUnsorted]
    omega
  · intro h
    exact absurd (h 0 1 (by decide) (by decide)) (by decide)

def dropW : Nat → Int := fun t => 3 - (t : Int)

def bumpD : Nat → Int
  | 0 => 0
  | 1 => 1
  | _ => -1

theorem control_interior_rank :
    psum bumpD 0 ≤ 0 ∧ psum bumpD 1 ≤ 0 ∧ 0 < psum bumpD 2 ∧ psum bumpD 3 ≤ 0 ∧
      dropW 1 < dropW 0 ∧ dropW 2 < dropW 1 ∧ dropW 3 < dropW 2 ∧ 0 ≤ dropW 3 ∧
      0 < wsum dropW bumpD 3 := by
  decide

def rhoUp : Nat → Int
  | 0 => 1
  | 1 => 2
  | _ => 0

def hD : Nat → Int
  | 0 => -1
  | _ => 1

theorem control_rho_increasing :
    psum hD 0 ≤ 0 ∧ psum hD 1 ≤ 0 ∧ psum hD 2 ≤ 0 ∧ rhoUp 2 = 0 ∧ rhoUp 0 < rhoUp 1 ∧
      0 < wsum rhoUp hD 2 := by
  decide

def ConcaveI (φ : Int → Int) : Prop := ∀ y : Int, φ (y + 2) - φ (y + 1) ≤ φ (y + 1) - φ y

def MonoI (φ : Int → Int) : Prop := ∀ y : Int, φ y ≤ φ (y + 1)

def ConcaveN (q : Nat → Int) : Prop := ∀ u : Nat, q (u + 2) - q (u + 1) ≤ q (u + 1) - q u

def MonoN (q : Nat → Int) : Prop := ∀ u : Nat, q u ≤ q (u + 1)

theorem diff_anti (φ : Int → Int) (hc : ConcaveI φ) :
    ∀ (d : Nat) (x : Int), φ (x + d + 1) - φ (x + d) ≤ φ (x + 1) - φ x := by
  intro d
  induction d with
  | zero =>
    intro x
    have e1 : x + ((0 : Nat) : Int) = x := by omega
    rw [e1]
    exact Int.le_refl _
  | succ d ih =>
    intro x
    have h1 := ih x
    have h2 := hc (x + d)
    have e1 : x + ((d + 1 : Nat) : Int) + 1 = x + d + 2 := by omega
    have e2 : x + ((d + 1 : Nat) : Int) = x + d + 1 := by omega
    rw [e1, e2]
    omega

theorem incr_shift (φ : Int → Int) (hc : ConcaveI φ) :
    ∀ (m d : Nat) (x : Int), φ (x + d + m) - φ (x + d) ≤ φ (x + m) - φ x := by
  intro m
  induction m with
  | zero =>
    intro d x
    have e1 : x + (d : Int) + ((0 : Nat) : Int) = x + d := by omega
    have e2 : x + ((0 : Nat) : Int) = x := by omega
    rw [e1, e2]
    omega
  | succ m ih =>
    intro d x
    have h1 := ih d x
    have h2 := diff_anti φ hc d (x + m)
    have e1 : x + (m : Int) + (d : Int) + 1 = x + d + ((m + 1 : Nat) : Int) := by omega
    have e2 : x + (m : Int) + (d : Int) = x + d + m := by omega
    have e3 : x + (m : Int) + 1 = x + ((m + 1 : Nat) : Int) := by omega
    rw [e1, e2, e3] at h2
    omega

theorem mono_shift (φ : Int → Int) (hm : MonoI φ) :
    ∀ (d : Nat) (x : Int), φ x ≤ φ (x + d) := by
  intro d
  induction d with
  | zero =>
    intro x
    have e1 : x + ((0 : Nat) : Int) = x := by omega
    rw [e1]
    exact Int.le_refl _
  | succ d ih =>
    intro x
    have h1 := ih x
    have h2 := hm (x + d)
    have e1 : x + ((d + 1 : Nat) : Int) = x + d + 1 := by omega
    rw [e1]
    omega

theorem fn1_concave_comp (φ : Int → Int) (q : Nat → Int)
    (hm : MonoI φ) (hc : ConcaveI φ) (hq : MonoN q) (hqc : ConcaveN q) :
    ConcaveN (fun u => φ (q u)) := by
  intro u
  show φ (q (u + 2)) - φ (q (u + 1)) ≤ φ (q (u + 1)) - φ (q u)
  have h1 : q (u + 1) ≤ q (u + 2) := hq (u + 1)
  have h2 := hqc u
  obtain ⟨m, hmq⟩ : ∃ m : Nat, q (u + 2) = q (u + 1) + m :=
    ⟨(q (u + 2) - q (u + 1)).toNat, by omega⟩
  obtain ⟨e, heq⟩ : ∃ e : Nat, q (u + 1) = q u + e + m :=
    ⟨(q (u + 1) - q u - m).toNat, by omega⟩
  have A := incr_shift φ hc m m (q u + e)
  have B := mono_shift φ hm e (q u)
  rw [hmq, heq]
  omega

theorem fn1_converse (q : Nat → Int)
    (hall : ∀ φ : Int → Int, MonoI φ → ConcaveI φ → ConcaveN (fun u => φ (q u))) :
    ConcaveN q :=
  hall (fun y => y) (fun y => by show y ≤ y + 1; omega)
    (fun y => by show y + 2 - (y + 1) ≤ y + 1 - y; omega)

theorem fn1_iff (q : Nat → Int) (hq : MonoN q) :
    (∀ φ : Int → Int, MonoI φ → ConcaveI φ → ConcaveN (fun u => φ (q u))) ↔ ConcaveN q :=
  ⟨fn1_converse q, fun hqc φ hm hc => fn1_concave_comp φ q hm hc hq hqc⟩

def phiC (y : Int) : Int := min (4 * y) (min (3 * y + 1) (2 * y + 3))

def qC : Nat → Int
  | 0 => 0
  | 1 => 1
  | n + 2 => 3 * (n : Int) + 3

theorem fn1_counterexample :
    MonoI phiC ∧ ConcaveI phiC ∧ MonoN qC ∧
      (phiC 2 - phiC 1 < phiC 1 - phiC 0 ∧ phiC 3 - phiC 2 < phiC 2 - phiC 1) ∧
      ¬ ConcaveN qC ∧ ¬ ConcaveN (fun u => phiC (qC u)) := by
  refine ⟨?_, ?_, ?_, by decide, ?_, ?_⟩
  · intro y
    unfold phiC
    omega
  · intro y
    unfold phiC
    omega
  · intro u
    rcases u with _ | _ | n
    · decide
    · decide
    · simp only [qC]
      omega
  · intro h
    exact absurd (h 0) (by decide)
  · intro h
    exact absurd (h 0) (by decide)

end Suen2007
