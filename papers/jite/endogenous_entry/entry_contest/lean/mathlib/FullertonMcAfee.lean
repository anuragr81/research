import Mathlib

/-!
# Fullerton and McAfee (1999), "Auctioning Entry into Tournaments"

Our reading of the paper, checked. Locators are printed pages of the JPE article. The effort
subgame of Section II is solved here from its primitives, which is more than the paper's
appendix sketch does, and the entry stage is then studied with that solution.
-/

open Real Finset Filter Topology

namespace FullertonMcAfee

/-! ## Eq. (1), p.578: the ratio-form win probability

With the base law uniform on `[0, 1]`, firm `i`'s best draw has density `z_i t^{z_i - 1}` and the
others' best draws have CDF `t^{Z - z_i}`, so the integrand of eq. (1) is `z_i t^{Z - 1}`. -/

theorem win_prob (zi Z : ℝ) (hZ : 0 < Z) :
    ∫ t in (0 : ℝ)..1, zi * t ^ (Z - 1) = zi / Z := by
  rw [intervalIntegral.integral_const_mul, integral_rpow (Or.inl (by linarith))]
  rw [sub_add_cancel, Real.one_rpow, Real.zero_rpow hZ.ne']
  field_simp
  ring

/-! ## The effort subgame, Theorem 1 and eqs. (3), (4), pp.578-579 and p.596

`others z i` is the rivals' total effort. A deviation by `i` to `y` earns
`P y / (y + others z i) - c_i y`, and there is no prize when nobody exerts effort. -/

variable {ι : Type*} [Fintype ι] [DecidableEq ι]

set_option linter.unusedSectionVars false

/-- The rivals' total effort. -/
def others (z : ι → ℝ) (i : ι) : ℝ := ∑ j ∈ univ.erase i, z j

/-- Expected profit without the fixed cost, eq. (1). -/
noncomputable def profit (P : ℝ) (c : ι → ℝ) (z : ι → ℝ) (i : ι) : ℝ :=
  P * z i / (z i + others z i) - c i * z i

/-- A Nash equilibrium of the effort subgame among the firms of `ι`. -/
def IsNash (P : ℝ) (c : ι → ℝ) (z : ι → ℝ) : Prop :=
  (∀ i, 0 ≤ z i) ∧ ∀ i y, 0 ≤ y → P * y / (y + others z i) - c i * y ≤ profit P c z i

theorem self_add_others (z : ι → ℝ) (i : ι) : z i + others z i = ∑ j, z j :=
  Finset.add_sum_erase univ z (mem_univ i)

/-- The equation for the threshold `t = P / Z`: `Σ_i (t - c_i)^+ = t`. -/
def IsRoot (c : ι → ℝ) (t : ℝ) : Prop := ∑ i, max (t - c i) 0 = t

theorem root_two (c : ι → ℝ) (hc : ∀ i, 0 < c i) (t : ℝ) (ht : 0 < t) (h : IsRoot c t) :
    2 ≤ (univ.filter (fun i => c i < t)).card := by
  have hsum : ∑ i, max (t - c i) 0 = ∑ i ∈ univ.filter (fun i => c i < t), (t - c i) := by
    rw [Finset.sum_filter]
    refine Finset.sum_congr rfl (fun i _ => ?_)
    split_ifs with hi
    · exact max_eq_left (by linarith)
    · exact max_eq_right (by linarith [not_lt.mp hi])
  by_contra hlt
  push_neg at hlt
  unfold IsRoot at h
  rw [hsum] at h
  interval_cases hk : (univ.filter (fun i => c i < t)).card
  · rw [Finset.card_eq_zero] at hk
    rw [hk, Finset.sum_empty] at h
    linarith
  · obtain ⟨a, ha⟩ := Finset.card_eq_one.mp hk
    rw [ha, Finset.sum_singleton] at h
    linarith [hc a]

/-- **The threshold is unique.** -/
theorem root_unique (c : ι → ℝ) (hc : ∀ i, 0 < c i) (t t' : ℝ) (ht : 0 < t) (ht' : 0 < t')
    (h : IsRoot c t) (h' : IsRoot c t') : t = t' := by
  have key : ∀ a b : ℝ, 0 < a → a < b → IsRoot c a → IsRoot c b → False := by
    intro a b ha hab hra hrb
    have h2 := root_two c hc a ha hra
    have hterm : ∀ i, (if c i < a then b - a else 0) ≤ max (b - c i) 0 - max (a - c i) 0 := by
      intro i
      split_ifs with hi
      · rw [max_eq_left (by linarith), max_eq_left (by linarith)]
        linarith
      · have hai : a - c i ≤ 0 := by linarith [not_lt.mp hi]
        rw [max_eq_right hai, sub_zero]
        exact le_max_right _ _
    have hsum := Finset.sum_le_sum (fun i (_ : i ∈ univ) => hterm i)
    rw [Finset.sum_sub_distrib, ← Finset.sum_filter, Finset.sum_const, nsmul_eq_mul] at hsum
    unfold IsRoot at hra hrb
    rw [hra, hrb] at hsum
    have : (2 : ℝ) ≤ ((univ.filter (fun i => c i < a)).card : ℝ) := by exact_mod_cast h2
    nlinarith
  rcases lt_trichotomy t t' with hlt | heq | hgt
  · exact (key t t' ht hlt h h').elim
  · exact heq
  · exact (key t' t ht' hgt h' h).elim

/-- The best-response inequality behind the existence half: at the first-order condition
    `c (x + Y)^2 = P Y` with `Y > 0`, no `y ≥ 0` does better than `x`. -/
theorem best_response (P c x Y y : ℝ) (hP : 0 < P) (hY : 0 < Y) (hx : 0 ≤ x) (hy : 0 ≤ y)
    (hfoc : c * (x + Y) ^ 2 = P * Y) :
    P * y / (y + Y) - c * y ≤ P * x / (x + Y) - c * x := by
  have hxY : 0 < x + Y := by linarith
  have hyY : 0 < y + Y := by linarith
  have hcq : c = P * Y / (x + Y) ^ 2 := by
    field_simp
    linarith
  have key : P * x / (x + Y) - c * x - (P * y / (y + Y) - c * y)
      = P * Y * (x - y) ^ 2 / ((x + Y) ^ 2 * (y + Y)) := by
    rw [hcq]
    field_simp
    ring
  have : 0 ≤ P * Y * (x - y) ^ 2 / ((x + Y) ^ 2 * (y + Y)) := by positivity
  linarith

/-- The candidate equilibrium for a threshold `t`, eq. (3): `z_i = (P/t) (1 - c_i/t)^+`. -/
noncomputable def zstar (P : ℝ) (c : ι → ℝ) (t : ℝ) (i : ι) : ℝ := P / t * max (1 - c i / t) 0

theorem zstar_sum (P : ℝ) (c : ι → ℝ) (t : ℝ) (ht : 0 < t) (h : IsRoot c t) :
    ∑ i, zstar P c t i = P / t := by
  have e : ∀ i, zstar P c t i = P / t ^ 2 * max (t - c i) 0 := by
    intro i
    unfold zstar
    have : max (1 - c i / t) 0 = max (t - c i) 0 / t := by
      rw [show 1 - c i / t = (t - c i) / t by field_simp]
      rcases le_total (t - c i) 0 with hle | hle
      · rw [max_eq_right hle, max_eq_right (div_nonpos_of_nonpos_of_nonneg hle ht.le), zero_div]
      · rw [max_eq_left hle, max_eq_left (div_nonneg hle ht.le)]
    rw [this]
    field_simp
  simp_rw [e]
  rw [← Finset.mul_sum, h]
  field_simp

/-- **Existence, Theorem 1.** For a threshold solving `Σ (t - c_i)^+ = t`, eq. (3) is a Nash
    equilibrium of the effort subgame. -/
theorem zstar_isNash (P : ℝ) (c : ι → ℝ) (hP : 0 < P) (hc : ∀ i, 0 < c i) (t : ℝ) (ht : 0 < t)
    (h : IsRoot c t) : IsNash P c (zstar P c t) := by
  have hZ := zstar_sum P c t ht h
  refine ⟨fun i => by unfold zstar; positivity, fun i y hy => ?_⟩
  have hYeq : others (zstar P c t) i = P / t - zstar P c t i := by
    have := self_add_others (zstar P c t) i
    linarith
  unfold profit
  rw [hYeq]
  rcases lt_or_ge (c i) t with hi | hi
  · -- an active firm
    have hz : zstar P c t i = P / t * (1 - c i / t) := by
      unfold zstar
      rw [max_eq_left]
      rw [sub_nonneg, div_le_one ht]
      exact hi.le
    have hY : P / t - zstar P c t i = P * c i / t ^ 2 := by
      rw [hz]
      field_simp
      ring
    rw [hY]
    have hx : 0 ≤ zstar P c t i := by
      rw [hz]
      have : c i / t < 1 := (div_lt_one ht).mpr hi
      exact mul_nonneg (by positivity) (by linarith)
    refine best_response P (c i) _ _ y hP (by have := hc i; positivity) hx hy ?_
    rw [hz]
    field_simp
    ring
  · -- an inactive firm
    have hz : zstar P c t i = 0 := by
      unfold zstar
      rw [max_eq_right]
      · ring
      · rw [sub_nonpos, le_div_iff₀ ht]
        linarith
    rw [hz]
    simp only [sub_zero, zero_add, mul_zero, zero_div]
    have hPt : 0 < P / t := by positivity
    have h1 : P * y / (y + P / t) ≤ P * y / (P / t) :=
      div_le_div_of_nonneg_left (by positivity) hPt (by linarith)
    have h2 : P * y / (P / t) = t * y := by field_simp
    nlinarith

/-- **Uniqueness and characterisation, Theorem 1 and eq. (3).** Every Nash equilibrium is `zstar`
    for the unique threshold, and the threshold is `P / Z`. -/
theorem nash_char (P : ℝ) (c : ι → ℝ) (hP : 0 < P) (hc : ∀ i, 0 < c i) [Nonempty ι]
    (z : ι → ℝ) (hz : IsNash P c z) :
    0 < ∑ j, z j ∧ IsRoot c (P / ∑ j, z j) ∧ z = zstar P c (P / ∑ j, z j) := by
  obtain ⟨hnn, hbr⟩ := hz
  set Z := ∑ j, z j with hZdef
  have hZi : ∀ i, z i + others z i = Z := fun i => self_add_others z i
  have hothers_nn : ∀ i, 0 ≤ others z i := fun i => Finset.sum_nonneg (fun j _ => hnn j)
  -- total effort is positive
  have hZpos : 0 < Z := by
    rcases (Finset.sum_nonneg (fun j (_ : j ∈ univ) => hnn j)).lt_or_eq with hlt | heq
    · exact hlt
    · exfalso
      obtain ⟨i⟩ := ‹Nonempty ι›
      have hzero : ∀ j, z j = 0 := by
        intro j
        have := (Finset.sum_eq_zero_iff_of_nonneg (fun j (_ : j ∈ univ) => hnn j)).mp heq.symm j
          (mem_univ j)
        exact this
      have ho : others z i = 0 := Finset.sum_eq_zero (fun j _ => hzero j)
      have := hbr i (P / (2 * c i)) (by have := hc i; positivity)
      unfold profit at this
      rw [ho, hzero i] at this
      have hci := hc i
      have e : P * (P / (2 * c i)) / (P / (2 * c i) + 0) - c i * (P / (2 * c i)) = P / 2 := by
        field_simp
        ring
      rw [e] at this
      simp at this
      linarith
  -- an active firm faces positive rival effort
  have hactive_others : ∀ i, 0 < z i → 0 < others z i := by
    intro i hi
    rcases (hothers_nn i).lt_or_eq with hlt | heq
    · exact hlt
    · exfalso
      have := hbr i (z i / 2) (by positivity)
      unfold profit at this
      rw [← heq, add_zero, add_zero] at this
      have hci := hc i
      have e1 : P * (z i / 2) / (z i / 2) = P := by field_simp
      have e2 : P * z i / z i = P := by field_simp
      rw [e1, e2] at this
      nlinarith
  -- first-order condition for an active firm, by Fermat's theorem
  have hfoc : ∀ i, 0 < z i → c i * Z ^ 2 = P * others z i := by
    intro i hi
    set Y := others z i
    have hY : 0 < Y := hactive_others i hi
    let g : ℝ → ℝ := fun y => P * y / (y + Y) - c i * y
    have hmax : IsLocalMax g (z i) := by
      filter_upwards [Ioi_mem_nhds hi] with y hy
      have := hbr i y (le_of_lt hy)
      unfold profit at this
      exact this
    have hne : z i + Y ≠ 0 := by linarith
    have hd : HasDerivAt g ((P * 1 * (z i + Y) - P * z i * 1) / (z i + Y) ^ 2 - c i * 1) (z i) := by
      have h1 : HasDerivAt (fun y => P * y) (P * 1) (z i) := (hasDerivAt_id (z i)).const_mul P
      have h2 : HasDerivAt (fun y => y + Y) 1 (z i) := (hasDerivAt_id (z i)).add_const Y
      have h3 := h1.div h2 hne
      have h4 : HasDerivAt (fun y => c i * y) (c i * 1) (z i) := (hasDerivAt_id (z i)).const_mul (c i)
      exact h3.sub h4
    have h0 := hmax.hasDerivAt_eq_zero hd
    have hZ' : z i + Y = Z := hZi i
    rw [hZ'] at h0
    have hZne : Z ≠ 0 := hZpos.ne'
    field_simp at h0
    have : Y = Z - z i := by linarith [hZi i]
    rw [this]
    nlinarith
  -- an inactive firm has cost at least P / Z
  have hinactive : ∀ i, z i = 0 → P ≤ c i * Z := by
    intro i hi
    by_contra hlt
    push_neg at hlt
    have hci := hc i
    have hY : others z i = Z := by rw [← hZi i, hi, zero_add]
    set y := (P - c i * Z) / (2 * c i)
    have hy : 0 < y := by
      have : 0 < P - c i * Z := by linarith
      positivity
    have := hbr i y hy.le
    unfold profit at this
    rw [hi, hY] at this
    simp only [mul_zero, zero_add, zero_div, sub_zero] at this
    have hyZ : 0 < y + Z := by linarith
    have hgt : c i * (y + Z) < P := by
      have : c i * y = (P - c i * Z) / 2 := by
        simp only [y]
        field_simp
      nlinarith
    have : 0 < P * y / (y + Z) - c i * y := by
      rw [sub_pos, lt_div_iff₀ hyZ]
      nlinarith
    linarith
  -- each firm's effort is `Z (1 - c_i t^{-1})^+` with `t = P / Z`
  set t := P / Z with ht
  have htpos : 0 < t := by positivity
  have hform : ∀ i, z i = zstar P c t i := by
    intro i
    unfold zstar
    have hPt : P / t = Z := by rw [ht]; field_simp
    rw [hPt]
    rcases (hnn i).lt_or_eq with hi | hi
    · have hf := hfoc i hi
      have hY : others z i = Z - z i := by linarith [hZi i]
      rw [hY] at hf
      have hcit : c i / t = c i * Z / P := by rw [ht]; field_simp
      have hlt : c i / t < 1 := by
        rw [hcit, div_lt_one hP]
        nlinarith
      rw [max_eq_left (by linarith), hcit]
      field_simp
      nlinarith
    · rw [← hi]
      have hle := hinactive i hi.symm
      have hcit : 1 ≤ c i / t := by
        rw [ht, le_div_iff₀ htpos, ht]
        rw [one_mul, div_le_iff₀ hZpos]
        linarith
      rw [max_eq_right (by linarith), mul_zero]
  refine ⟨hZpos, ?_, funext hform⟩
  -- the threshold equation, from summing the efforts
  have hsum : ∑ i, zstar P c t i = Z := by
    rw [hZdef]
    exact Finset.sum_congr rfl (fun i _ => (hform i).symm)
  unfold IsRoot
  have e : ∀ i, zstar P c t i = P / t ^ 2 * max (t - c i) 0 := by
    intro i
    unfold zstar
    have : max (1 - c i / t) 0 = max (t - c i) 0 / t := by
      rw [show 1 - c i / t = (t - c i) / t by field_simp]
      rcases le_total (t - c i) 0 with hle | hle
      · rw [max_eq_right hle, max_eq_right (div_nonpos_of_nonpos_of_nonneg hle htpos.le), zero_div]
      · rw [max_eq_left hle, max_eq_left (div_nonneg hle htpos.le)]
    rw [this]
    field_simp
  simp_rw [e] at hsum
  rw [← Finset.mul_sum] at hsum
  have hPt : P / t = Z := by rw [ht]; field_simp
  have : P / t ^ 2 = Z / t := by rw [← hPt]; ring
  rw [this] at hsum
  field_simp at hsum
  linarith

/-- **Theorem 1, uniqueness.** The effort subgame has at most one Nash equilibrium. -/
theorem nash_unique (P : ℝ) (c : ι → ℝ) (hP : 0 < P) (hc : ∀ i, 0 < c i) [Nonempty ι]
    (z z' : ι → ℝ) (hz : IsNash P c z) (hz' : IsNash P c z') : z = z' := by
  obtain ⟨h1, h2, h3⟩ := nash_char P c hP hc z hz
  obtain ⟨h1', h2', h3'⟩ := nash_char P c hP hc z' hz'
  have := root_unique c hc _ _ (by positivity) (by positivity) h2 h2'
  rw [h3, h3', this]

/-- **Theorem 1, the active set.** The firms with positive effort are the lowest-cost ones. -/
theorem active_prefix (P : ℝ) (c : ι → ℝ) (hP : 0 < P) (hc : ∀ i, 0 < c i) [Nonempty ι]
    (z : ι → ℝ) (hz : IsNash P c z) (i j : ι) (hi : 0 < z i) (hji : c j ≤ c i) : 0 < z j := by
  obtain ⟨hZ, -, hform⟩ := nash_char P c hP hc z hz
  set t := P / ∑ k, z k
  have ht : 0 < t := by positivity
  rw [hform] at hi ⊢
  unfold zstar at hi ⊢
  have hPt : 0 < P / t := by positivity
  have hmi : 0 < max (1 - c i / t) 0 := pos_of_mul_pos_right hi hPt.le
  have hlt : 0 < 1 - c i / t := by
    rcases le_total (1 - c i / t) 0 with h | h
    · rw [max_eq_right h] at hmi; exact absurd hmi (lt_irrefl 0)
    · rw [max_eq_left h] at hmi; exact hmi
  have : 0 < 1 - c j / t := by
    have : c j / t ≤ c i / t := div_le_div_of_nonneg_right hji ht.le
    linarith
  rw [max_eq_left this.le]
  positivity

/-- **Eq. (4).** At the equilibrium each firm earns `P (1 - c_i / t)^+²`. -/
theorem profit_zstar (P : ℝ) (c : ι → ℝ) (hP : 0 < P) (t : ℝ) (ht : 0 < t) (h : IsRoot c t)
    (i : ι) : profit P c (zstar P c t) i = P * max (1 - c i / t) 0 ^ 2 := by
  unfold profit
  rw [self_add_others, zstar_sum P c t ht h]
  unfold zstar
  rcases le_total (1 - c i / t) 0 with hle | hle
  · rw [max_eq_right hle]
    simp
  · rw [max_eq_left hle]
    field_simp

/-- **All firms active.** With `m` firms and `(m - 1) c_i < Σ c` for every `i`, the threshold is
    `Σ c / (m - 1)`, every firm is active, and eq. (4) reads `P (1 - c_i (m-1) / Σ c)^2`. -/
theorem all_active (P : ℝ) (c : ι → ℝ) (hP : 0 < P) (hc : ∀ i, 0 < c i) [Nonempty ι]
    (hm : 2 ≤ Fintype.card ι)
    (hact : ∀ i, ((Fintype.card ι : ℝ) - 1) * c i < ∑ j, c j) :
    IsNash P c (zstar P c ((∑ j, c j) / ((Fintype.card ι : ℝ) - 1)))
      ∧ ∀ z, IsNash P c z → ∀ i, 0 < z i
          ∧ profit P c z i = P * (1 - c i * ((Fintype.card ι : ℝ) - 1) / ∑ j, c j) ^ 2 := by
  have h2 : (2 : ℝ) ≤ (Fintype.card ι : ℝ) := by exact_mod_cast hm
  set m : ℝ := (Fintype.card ι : ℝ)
  have hm1 : 0 < m - 1 := by linarith
  set S := ∑ j, c j
  have hS : 0 < S := Finset.sum_pos (fun j _ => hc j) univ_nonempty
  set t := S / (m - 1)
  have ht : 0 < t := by positivity
  have hlt : ∀ i, c i < t := fun i => by
    rw [lt_div_iff₀ hm1]
    linarith [hact i]
  have hroot : IsRoot c t := by
    unfold IsRoot
    rw [Finset.sum_congr rfl (fun i _ => max_eq_left (by linarith [hlt i]))]
    rw [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, nsmul_eq_mul]
    simp only [t]
    field_simp
    ring
  have hN := zstar_isNash P c hP hc t ht hroot
  refine ⟨hN, fun z hz i => ?_⟩
  have := nash_unique P c hP hc z _ hz hN
  subst this
  refine ⟨?_, ?_⟩
  · unfold zstar
    have : 0 < 1 - c i / t := by rw [sub_pos, div_lt_one ht]; exact hlt i
    rw [max_eq_left this.le]
    positivity
  · rw [profit_zstar P c hP t ht hroot i]
    have : 0 ≤ 1 - c i / t := by rw [sub_nonneg, div_le_one ht]; exact (hlt i).le
    rw [max_eq_left this]
    congr 2
    simp only [t]
    field_simp

/-! ## The entry stage: equilibria of different sizes, p.579

A subgame-perfect equilibrium of the entry stage needs "(1) the firms that pay γ make
nonnegative profits and (2) the firms that do not pay γ would make negative profits by paying γ"
(p.579). Every subgame below has a unique Nash equilibrium (`nash_unique`), so each condition is
a statement about that equilibrium. With costs `(2.1, 2.3, 2.5, 2.6)`, prize `1` and fixed cost
`γ = 19/250`, the entrant sets `{2.1, 2.3}` and `{2.1, 2.5, 2.6}` are both equilibria. The count
differs across equilibria, and the second set is not the lowest-cost one. -/

/-- Every firm active, at prize `1`: existence, positivity and eq. (4), for a vector of costs. -/
theorem fin_all_active {k : ℕ} (c : Fin k → ℝ) (hk : 2 ≤ k) (hc : ∀ i, 0 < c i)
    (hact : ∀ i, ((k : ℝ) - 1) * c i < ∑ j, c j) :
    (∃ z, IsNash 1 c z) ∧ ∀ z, IsNash 1 c z → ∀ i, 0 < z i
      ∧ profit 1 c z i = (1 - c i * ((k : ℝ) - 1) / ∑ j, c j) ^ 2 := by
  haveI : Nonempty (Fin k) := ⟨⟨0, by omega⟩⟩
  have hcard : Fintype.card (Fin k) = k := Fintype.card_fin k
  have hact' : ∀ i, ((Fintype.card (Fin k) : ℝ) - 1) * c i < ∑ j, c j := by
    rw [hcard]; exact hact
  obtain ⟨hN, hall⟩ := all_active 1 c one_pos hc (by rw [hcard]; exact hk) hact'
  refine ⟨⟨_, hN⟩, fun z hz i => ?_⟩
  obtain ⟨h1, h2⟩ := hall z hz i
  rw [hcard] at h2
  exact ⟨h1, by rw [h2, one_mul]⟩

noncomputable def costA : Fin 2 → ℝ := ![21 / 10, 23 / 10]
noncomputable def costA3 : Fin 3 → ℝ := ![21 / 10, 23 / 10, 5 / 2]
noncomputable def costA4 : Fin 3 → ℝ := ![21 / 10, 23 / 10, 13 / 5]
noncomputable def costB : Fin 3 → ℝ := ![21 / 10, 5 / 2, 13 / 5]
noncomputable def costB2 : Fin 4 → ℝ := ![21 / 10, 5 / 2, 13 / 5, 23 / 10]

/-- **Two entry equilibria of different sizes.** In each listed subgame the unique equilibrium
    gives the stated profits. `{2.1, 2.3}`: both entrants active and above `γ`, while the firm at
    `2.5` or at `2.6` would earn below `γ` on entering. `{2.1, 2.5, 2.6}`: all three active and
    above `γ`, while the firm at `2.3` would earn below `γ` on entering. -/
theorem two_sizes :
    ((∃ z, IsNash 1 costA z) ∧ ∀ z, IsNash 1 costA z → ∀ i, 0 < z i ∧ 19 / 250 ≤ profit 1 costA z i)
    ∧ (∀ z, IsNash 1 costA3 z → profit 1 costA3 z 2 < 19 / 250)
    ∧ (∀ z, IsNash 1 costA4 z → profit 1 costA4 z 2 < 19 / 250)
    ∧ ((∃ z, IsNash 1 costB z) ∧ ∀ z, IsNash 1 costB z → ∀ i, 0 < z i ∧ 19 / 250 ≤ profit 1 costB z i)
    ∧ (∀ z, IsNash 1 costB2 z → profit 1 costB2 z 3 < 19 / 250) := by
  have pA := fin_all_active costA le_rfl (fun i => by fin_cases i <;> norm_num [costA])
    (fun i => by fin_cases i <;> simp [costA, Fin.sum_univ_two] <;> norm_num)
  have pA3 := fin_all_active costA3 (by norm_num) (fun i => by fin_cases i <;> norm_num [costA3])
    (fun i => by fin_cases i <;> simp [costA3, Fin.sum_univ_three] <;> norm_num)
  have pA4 := fin_all_active costA4 (by norm_num) (fun i => by fin_cases i <;> norm_num [costA4])
    (fun i => by fin_cases i <;> simp [costA4, Fin.sum_univ_three] <;> norm_num)
  have pB := fin_all_active costB (by norm_num) (fun i => by fin_cases i <;> norm_num [costB])
    (fun i => by fin_cases i <;> simp [costB, Fin.sum_univ_three] <;> norm_num)
  have pB2 := fin_all_active costB2 (by norm_num) (fun i => by fin_cases i <;> norm_num [costB2])
    (fun i => by fin_cases i <;> simp [costB2, Fin.sum_univ_four] <;> norm_num)
  refine ⟨⟨pA.1, fun z hz i => ⟨(pA.2 z hz i).1, ?_⟩⟩, fun z hz => ?_, fun z hz => ?_,
    ⟨pB.1, fun z hz i => ⟨(pB.2 z hz i).1, ?_⟩⟩, fun z hz => ?_⟩
  · rw [(pA.2 z hz i).2]
    fin_cases i <;> simp [costA, Fin.sum_univ_two] <;> norm_num
  · rw [(pA3.2 z hz 2).2]
    simp [costA3, Fin.sum_univ_three]
    norm_num
  · rw [(pA4.2 z hz 2).2]
    simp [costA4, Fin.sum_univ_three]
    norm_num
  · rw [(pB.2 z hz i).2]
    fin_cases i <;> simp [costB, Fin.sum_univ_three] <;> norm_num
  · rw [(pB2.2 z hz 3).2]
    simp [costB2, Fin.sum_univ_four]
    norm_num

/-! ## Lemma 1, pp.579-580 and p.597 -/

/-- **Lemma 1, the bound.** If firm `i`'s entry would leave every entrant active and earn `i` less
    than the costliest entrant `k` now earns, then `c_i ≥ [(m² - m)/(m² - m + 1)] c_k`. -/
theorem lemma1_deviation (P ci ck m S γ : ℝ) (hP : 0 < P) (hS : 0 < S) (hci : 0 < ci)
    (hbi : 0 ≤ 1 - ci * m / (S + ci)) (hbk : 0 ≤ 1 - ck * (m - 1) / S)
    (hi : P * (1 - ci * m / (S + ci)) ^ 2 < γ) (hk : γ ≤ P * (1 - ck * (m - 1) / S) ^ 2) :
    ck * (m - 1) * (S + ci) < ci * m * S := by
  have hsq : (1 - ci * m / (S + ci)) ^ 2 < (1 - ck * (m - 1) / S) ^ 2 := by
    have := lt_of_lt_of_le hi hk
    exact lt_of_mul_lt_mul_left this hP.le
  have hlt : 1 - ci * m / (S + ci) < 1 - ck * (m - 1) / S := by
    by_contra h
    push_neg at h
    nlinarith
  have hSc : 0 < S + ci := by linarith
  have : ck * (m - 1) / S < ci * m / (S + ci) := by linarith
  rw [div_lt_div_iff₀ hS hSc] at this
  linarith

theorem lemma1_bound (m ci ck S : ℝ) (hm : 1 ≤ m) (hci : 0 < ci) (hS : 0 < S)
    (hSle : S ≤ m * ck) (hdev : ck * (m - 1) * (S + ci) ≤ ci * m * S) :
    (m ^ 2 - m) / (m ^ 2 - m + 1) * ck ≤ ci := by
  have hden : 0 < m ^ 2 - m + 1 := by nlinarith
  rw [div_mul_eq_mul_div, div_le_iff₀ hden]
  have h1 : ck * m * (m - 1) * ci ≥ S * (m - 1) * ci := by
    have : 0 ≤ (m - 1) * ci := by nlinarith
    nlinarith
  have h2 : ci * m ^ 2 * S ≥ ck * m * (m - 1) * S + ck * m * (m - 1) * ci := by nlinarith
  have h3 : ci * (m ^ 2 - m + 1) * S ≥ ck * m * (m - 1) * S := by nlinarith
  have := le_of_mul_le_mul_right (by nlinarith : ck * m * (m - 1) * S ≤ ci * (m ^ 2 - m + 1) * S) hS
  nlinarith

/-- The other case of the proof (p.597). If `i`'s entry would push `k` out, `i` faces the sum
    `S - c_k + c_i`, and `c_i < c_k` gives `i` a larger bracket than `k` has now. -/
theorem lemma1_case_out (ci ck S : ℝ) (hci : 0 < ci) (hlt : ci < ck) (hS : ck < S) :
    ci / (S - ck + ci) < ck / S := by
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]
  nlinarith

/-- **The bound is exact** (p.580). With `m` entrants at cost `c`, the fixed cost `P/m²` that
    leaves them at zero profit, and a firm at `[(m² - m)/(m² - m + 1)] c`, that firm's entry
    keeps every firm active and earns it exactly `P/m²`. -/
theorem lemma1_exact (P c m : ℝ) (hP : 0 < P) (hc : 0 < c) (hm : 2 ≤ m) :
    let θ := (m ^ 2 - m) / (m ^ 2 - m + 1)
    P * (1 - c * (m - 1) / (m * c)) ^ 2 = P / m ^ 2
      ∧ m * c < m * c + θ * c
      ∧ m * (θ * c) < m * c + θ * c
      ∧ P * (1 - θ * c * m / (m * c + θ * c)) ^ 2 = P / m ^ 2 := by
  intro θ
  have hden : 0 < m ^ 2 - m + 1 := by nlinarith
  have hθ : 0 < θ := by
    simp only [θ]
    apply div_pos _ hden
    nlinarith
  have hθ1 : θ * (m - 1) < m := by
    simp only [θ]
    rw [div_mul_eq_mul_div, div_lt_iff₀ hden]
    nlinarith
  have hm0 : 0 < m := by linarith
  refine ⟨?_, by nlinarith, by nlinarith, ?_⟩
  · field_simp
    ring
  · have hθdef : θ * (m ^ 2 - m + 1) = m ^ 2 - m := by
      simp only [θ]
      exact div_mul_cancel₀ _ hden.ne'
    have hmθ : 0 < m + θ := by linarith
    have e1 : θ * c * m / (m * c + θ * c) = θ * m / (m + θ) := by
      rw [show m * c + θ * c = (m + θ) * c by ring, mul_right_comm θ c m,
        mul_div_mul_right _ _ hc.ne']
    have e2 : θ * m / (m + θ) = (m - 1) / m := by
      rw [div_eq_div_iff hmθ.ne' hm0.ne']
      linear_combination hθdef
    have e3 : 1 - (m - 1) / m = 1 / m := by
      calc 1 - (m - 1) / m = m / m - (m - 1) / m := by rw [div_self hm0.ne']
        _ = (m - (m - 1)) / m := by rw [div_sub_div_same]
        _ = 1 / m := by ring_nf
    rw [e1, e2, e3]
    field_simp

/-! ## Theorem 2, p.580 and pp.597-598 -/

/-- `γ ≥ P x²` exactly when `1 - √(γ/P) ≤ 1 - x`, for a nonnegative bracket `x`. The proof's
    first display writes this as an iff without the sign condition. -/
theorem thm2_iff (P γ x : ℝ) (hP : 0 < P) (hγ : 0 ≤ γ) (hx : 0 ≤ x) :
    P * x ^ 2 ≤ γ ↔ x ≤ Real.sqrt (γ / P) := by
  rw [Real.le_sqrt hx, le_div_iff₀ hP, mul_comm]
  exact div_nonneg hγ hP.le

/-- The bracket can be negative, and then the iff fails. -/
theorem thm2_iff_needs_sign : ¬ (1 * (-2 : ℝ) ^ 2 ≤ 1 ↔ (-2 : ℝ) ≤ Real.sqrt (1 / 1)) := by
  rw [div_one, Real.sqrt_one]
  norm_num

/-- **Theorem 2's step.** If firm `k` cannot profitably enter, neither can firm `k + 1`. -/
theorem thm2_step (a S ck cn k : ℝ) (ha : a ≤ 1) (hk : 1 ≤ k) (hcn : 0 ≤ cn) (hc : ck ≤ cn)
    (h : a * S ≤ ck * (k - 1)) : a * (S + cn) ≤ k * cn := by
  nlinarith

/-! ## The symmetric case and Theorem 3, pp.580-581 and pp.598-599 -/

/-- With `m` identical firms at cost `c`, the prize `P = m c Z/(m - 1)` buys total effort `Z`, each
    firm earns `P/m²`, and the entry fee `P/m² - γ` leaves a total cost of `c Z + m γ`. -/
theorem symmetric_case (c Z γ m : ℝ) (hc : 0 < c) (hm : 1 < m) :
    let P := m * c * Z / (m - 1)
    P * (m - 1) / (m * c) = Z
      ∧ P * (1 - c * (m - 1) / (m * c)) ^ 2 = P / m ^ 2
      ∧ P - m * (P / m ^ 2 - γ) = c * Z + m * γ := by
  intro P
  have hm1 : m - 1 ≠ 0 := by linarith
  have hm0 : m ≠ 0 := by linarith
  refine ⟨?_, ?_, ?_⟩ <;> simp only [P] <;> field_simp <;> ring

/-- The total cost, `TC_m = Z Σ c (-1 + 2Δ_m - ((m-1)/m) Δ_m²) + m γ` with `Δ_m = m c_m / Σ c`. -/
noncomputable def TC (Z S cm γ m : ℝ) : ℝ :=
  Z * S * (-1 + 2 * (m * cm / S) - (m - 1) / m * (m * cm / S) ^ 2) + m * γ

/-- **The cost formula** (p.598). The prize `Z Σ c/(m - 1)` less `m` times the fee that takes the
    `m`th firm's profit. -/
theorem TC_formula (Z S cm γ m : ℝ) (hS : 0 < S) (hm : 1 < m) :
    let P := Z * S / (m - 1)
    P - m * (P * (1 - cm * (m - 1) / S) ^ 2 - γ) = TC Z S cm γ m := by
  intro P
  have hm1 : m - 1 ≠ 0 := by linarith
  have hm0 : m ≠ 0 := by linarith
  simp only [P, TC]
  field_simp
  ring

/-- **Theorem 3's step.** With `c_m ≤ c_{m+1}`, `Δ_m ≥ 1`, `Δ_m ≤ Δ_{m+1}`, and `m + 1` firms able to
    be active (`Δ_{m+1} ≤ (m+1)/m`), the cost does not fall from `m` to `m + 1`. -/
theorem TC_step (Z S cm cn γ m : ℝ) (hZ : 0 ≤ Z) (hγ : 0 ≤ γ) (hm : 1 ≤ m) (hS : 0 < S)
    (hcm : 0 < cm) (hc : cm ≤ cn) (hD1 : S ≤ m * cm) (hfeas : m * cn ≤ S + cn)
    (hmono : m * cm / S ≤ (m + 1) * cn / (S + cn)) :
    TC Z S cm γ m ≤ TC Z (S + cn) cn γ (m + 1) := by
  have hm0 : 0 < m := by linarith
  have hSn : 0 < S + cn := by linarith
  set D := m * cm / S with hD
  set D' := (m + 1) * cn / (S + cn) with hD'
  have hD1' : 1 ≤ D := by rw [hD, le_div_iff₀ hS]; linarith
  have hD'le : D' ≤ (m + 1) / m := by
    rw [hD', div_le_div_iff₀ hSn hm0]
    nlinarith
  have hDle : D ≤ (m + 1) / m := le_trans hmono hD'le
  have hmD : m * D ≤ m + 1 := by
    have := mul_le_mul_of_nonneg_left hDle hm0.le
    rwa [mul_div_cancel₀ _ hm0.ne'] at this
  have hmD' : m * D' ≤ m + 1 := by
    have := mul_le_mul_of_nonneg_left hD'le hm0.le
    rwa [mul_div_cancel₀ _ hm0.ne'] at this
  -- (i) the next bracket is nondecreasing up to (m+1)/m
  have hi : -1 + 2 * D - m / (m + 1) * D ^ 2 ≤ -1 + 2 * D' - m / (m + 1) * D' ^ 2 := by
    have e : (-1 + 2 * D' - m / (m + 1) * D' ^ 2) - (-1 + 2 * D - m / (m + 1) * D ^ 2)
        = (D' - D) * (2 * (m + 1) - m * (D + D')) / (m + 1) := by
      field_simp
      ring
    have : 0 ≤ (D' - D) * (2 * (m + 1) - m * (D + D')) / (m + 1) := by
      apply div_nonneg _ (by linarith)
      apply mul_nonneg (by linarith)
      nlinarith
    linarith
  -- (ii) the decomposition at the old Δ
  have hii : (S + cn) * (-1 + 2 * D - m / (m + 1) * D ^ 2)
      - S * (-1 + 2 * D - (m - 1) / m * D ^ 2)
      = cn * (-1 + 2 * D - m / (m + 1) * D ^ 2) - cm * D / (m + 1) := by
    rw [hD]
    field_simp
    ring
  -- (iii) the next bracket at the old Δ exceeds Δ/(m+1)
  have hiii : D / (m + 1) ≤ -1 + 2 * D - m / (m + 1) * D ^ 2 := by
    have e : (-1 + 2 * D - m / (m + 1) * D ^ 2) - D / (m + 1)
        = (D - 1) * ((m + 1) - m * D) / (m + 1) := by
      field_simp
      ring
    have : 0 ≤ (D - 1) * ((m + 1) - m * D) / (m + 1) := by
      apply div_nonneg _ (by linarith)
      apply mul_nonneg <;> linarith
    linarith
  have hpos : 0 ≤ -1 + 2 * D - m / (m + 1) * D ^ 2 := le_trans (by positivity) hiii
  -- (iv) combine
  have hiv : S * (-1 + 2 * D - (m - 1) / m * D ^ 2)
      ≤ (S + cn) * (-1 + 2 * D' - (m + 1 - 1) / (m + 1) * D' ^ 2) := by
    have e : (m + 1 - 1) / (m + 1) = m / (m + 1) := by ring_nf
    rw [e]
    have h1 : cm * D / (m + 1) ≤ cn * (-1 + 2 * D - m / (m + 1) * D ^ 2) := by
      calc cm * D / (m + 1) = cm * (D / (m + 1)) := by ring
        _ ≤ cm * (-1 + 2 * D - m / (m + 1) * D ^ 2) := mul_le_mul_of_nonneg_left hiii hcm.le
        _ ≤ cn * (-1 + 2 * D - m / (m + 1) * D ^ 2) := mul_le_mul_of_nonneg_right hc hpos
    have h2 := mul_le_mul_of_nonneg_left hi hSn.le
    linarith
  unfold TC
  rw [← hD, ← hD']
  have : (m + 1) * cn / (S + cn) = D' := rfl
  nlinarith [mul_le_mul_of_nonneg_left hiv hZ]

/-! ## Lemma 2, p.581 and p.599 -/

/-- **Lemma 2 with the induction hypothesis made explicit.** With `S_{m-1} = s - a`, if
    `Δ_{m-1} ≤ Δ_m` and `c_m/c_{m+1} ≤ 1/m + ((m-1)/m)(c_{m-1}/c_m)`, then `Δ_m ≤ Δ_{m+1}`. -/
theorem lemma2_step (m s a a' b : ℝ) (hm : 1 ≤ m) (ha : 0 < a) (hb : 0 < b)
    (hs : a < s) (hprev : (m - 1) * a' / (s - a) ≤ m * a / s)
    (hcond : a / b ≤ 1 / m + (m - 1) / m * (a' / a)) :
    m * a / s ≤ (m + 1) * b / (s + b) := by
  have hs0 : 0 < s := by linarith
  have hsa : 0 < s - a := by linarith
  have hm0 : 0 < m := by linarith
  rw [div_le_div_iff₀ hsa hs0] at hprev
  rw [div_le_div_iff₀ hs0 (by linarith)]
  rw [div_le_iff₀ hb] at hcond
  have e : (1 / m + (m - 1) / m * (a' / a)) * b = (a + (m - 1) * a') * b / (m * a) := by
    field_simp
  rw [e, le_div_iff₀ (by positivity)] at hcond
  nlinarith [mul_pos ha hb, mul_pos hs0 hb]

/-- **Lemma 2 read for a single `m` is false.** With costs `(12/5, 42/5, 43/5, 93/10)`, three and
    four firms can both be active, the condition holds at `m = 3`, and `Δ_4 < Δ_3`. The proof
    needs the condition at every smaller `m` too. -/
theorem lemma2_single_m_fails :
    let c1 : ℝ := 12 / 5
    let c2 : ℝ := 42 / 5
    let c3 : ℝ := 43 / 5
    let c4 : ℝ := 93 / 10
    c1 ≤ c2 ∧ c2 ≤ c3 ∧ c3 ≤ c4
      ∧ 2 * c3 < c1 + c2 + c3 ∧ 3 * c4 < c1 + c2 + c3 + c4
      ∧ c3 / c4 ≤ 1 / 3 + 2 / 3 * (c2 / c3)
      ∧ 4 * c4 / (c1 + c2 + c3 + c4) < 3 * c3 / (c1 + c2 + c3) := by
  norm_num

/-- The two families the paper names satisfy the condition at every `m ≥ 2`. Constant increments
    `c_i = a + b i`. -/
theorem lemma2_constant_increment (a b m : ℝ) (ha : 0 < a) (hb : 0 ≤ b) (hm : 2 ≤ m) :
    (a + b * m) / (a + b * (m + 1)) ≤ 1 / m + (m - 1) / m * ((a + b * (m - 1)) / (a + b * m)) := by
  have hm0 : 0 < m := by linarith
  have h1 : 0 < a + b * m := by nlinarith
  have h2 : 0 < a + b * (m + 1) := by nlinarith
  rw [div_le_iff₀ h2]
  have e : (1 / m + (m - 1) / m * ((a + b * (m - 1)) / (a + b * m))) * (a + b * (m + 1))
      = (a + b * m + (m - 1) * (a + b * (m - 1))) * (a + b * (m + 1)) / (m * (a + b * m)) := by
    field_simp
  rw [e, le_div_iff₀ (by positivity)]
  nlinarith [mul_nonneg hb hb, mul_nonneg (mul_nonneg hb hb) (by linarith : (0 : ℝ) ≤ m - 1),
    mul_nonneg ha.le hb, mul_nonneg (mul_nonneg ha.le hb) (by linarith : (0 : ℝ) ≤ m - 1)]

/-- Proportional increments `c_i = α (1 + δ)^i`, with ratio `q = 1 + δ ≥ 1`. -/
theorem lemma2_proportional (q m : ℝ) (hq : 1 ≤ q) (hm : 1 ≤ m) :
    1 / q ≤ 1 / m + (m - 1) / m * (1 / q) := by
  have hm0 : 0 < m := by linarith
  have hq0 : 0 < q := by linarith
  rw [show 1 / m + (m - 1) / m * (1 / q) = (q + (m - 1)) / (m * q) by field_simp]
  rw [div_le_div_iff₀ hq0 (by positivity)]
  nlinarith

/-! ## Uniform costs, p.583

With two entrants and costs uniform on `[0, c̄]`, eq. (7) gives
`B(c) = P (1/c) ∫_0^c (x/(c + x))² dx`. The substitution `x = c t` shows that `B` does not depend
on `c`, so it is not strictly decreasing and the auction does not sort. -/

theorem uniform_bid_scale (c : ℝ) (hc : 0 < c) :
    1 / c * ∫ x in (0 : ℝ)..c, (x / (c + x)) ^ 2 = ∫ t in (0 : ℝ)..1, (t / (1 + t)) ^ 2 := by
  have h := intervalIntegral.integral_comp_mul_left (fun x => (x / (c + x)) ^ 2) hc.ne'
    (a := 0) (b := 1)
  simp only [mul_zero, mul_one, smul_eq_mul] at h
  rw [← one_div] at h
  rw [← h]
  refine intervalIntegral.integral_congr (fun t ht => ?_)
  rw [Set.uIcc_of_le zero_le_one] at ht
  have h1 : 0 < 1 + t := by linarith [ht.1]
  have h2 : 0 < c + c * t := by nlinarith [ht.1]
  congr 1
  rw [div_eq_div_iff h2.ne' h1.ne']
  ring

theorem uniform_bid_constant (P c c' : ℝ) (hc : 0 < c) (hc' : 0 < c') :
    P * (1 / c * ∫ x in (0 : ℝ)..c, (x / (c + x)) ^ 2)
      = P * (1 / c' * ∫ x in (0 : ℝ)..c', (x / (c' + x)) ^ 2) := by
  rw [uniform_bid_scale c hc, uniform_bid_scale c' hc']

/-! ## Lemma 4, p.589 and pp.601-602 -/

/-- **Lemma 4.** When the best entrant's endowment has `H(w_max) ≥ e^{-c/P}`, a rival who draws
    `z ≥ 0` new innovations alone earns `P (1 - H^z) - c z ≤ 0`, so no one researches. -/
theorem lemma4 (P c H : ℝ) (hP : 0 < P) (hH0 : 0 < H) (hH : exp (-(c / P)) ≤ H) (z : ℝ)
    (hz : 0 ≤ z) : P * (1 - H ^ z) - c * z ≤ 0 := by
  have hlog : -(c / P) ≤ log H := by
    rw [← Real.log_exp (-(c / P))]
    exact Real.log_le_log (exp_pos _) hH
  rw [Real.rpow_def_of_pos hH0]
  have he := Real.add_one_le_exp (log H * z)
  have h1 : 1 - exp (log H * z) ≤ z * (c / P) := by nlinarith
  have h2 : P * (z * (c / P)) = c * z := by field_simp
  nlinarith

/-- **The condition is needed.** Below `e^{-c/P}` some small amount of research pays. -/
theorem lemma4_sharp (P c H : ℝ) (hP : 0 < P) (hc : 0 < c) (hH0 : 0 < H) (hH : H < exp (-(c / P))) :
    ∃ z, 0 < z ∧ 0 < P * (1 - H ^ z) - c * z := by
  set K := -log H with hK
  have hKc : c / P < K := by
    have : log H < -(c / P) := by
      rw [← Real.log_exp (-(c / P))]
      exact Real.log_lt_log hH0 hH
    linarith
  have hK0 : 0 < K := lt_of_le_of_lt (by positivity) hKc
  have hPK : c < P * K := by rwa [div_lt_iff₀ hP, mul_comm] at hKc
  set z := (P * K - c) / (2 * c * K) with hz
  have hz0 : 0 < z := by
    apply div_pos (by linarith); positivity
  refine ⟨z, hz0, ?_⟩
  rw [Real.rpow_def_of_pos hH0]
  have hx : log H * z = -(K * z) := by rw [hK]; ring
  rw [hx]
  have hKz : 0 < K * z := by positivity
  -- e^{-x} ≤ 1/(1 + x) for x ≥ 0
  have hexp : exp (-(K * z)) ≤ 1 / (1 + K * z) := by
    have := Real.add_one_le_exp (K * z)
    rw [Real.exp_neg, ← one_div]
    exact one_div_le_one_div_of_le (by linarith) (by linarith)
  have hgap : 1 - 1 / (1 + K * z) = K * z / (1 + K * z) := by
    field_simp
    ring
  have hmain : c * z < P * (K * z / (1 + K * z)) := by
    have h1z : 0 < 1 + K * z := by linarith
    rw [show P * (K * z / (1 + K * z)) = P * K * z / (1 + K * z) by ring, lt_div_iff₀ h1z]
    have e : c * K * z = (P * K - c) / 2 := by
      rw [hz]
      field_simp
    nlinarith [mul_pos hz0 (sub_pos.mpr hPK)]
  nlinarith

/-! ## Theorem 4, p.586 -/

/-- The hypothesis of Theorem 4 as printed, writing `Ψ w` for `Ψ(w, w)`. -/
def Thm4Hyp (Ψ : ℝ → ℝ) (lo hi : ℝ) : Prop :=
  ∃ w0 ∈ Set.Icc lo hi, ∀ w ∈ Set.Icc lo hi, w < w0 → Ψ w0 ≤ Ψ w

/-- **As printed, the hypothesis holds for every `Ψ`**, by taking `w̃` at the bottom of the support. -/
theorem thm4_hyp_always (Ψ : ℝ → ℝ) (lo hi : ℝ) (h : lo ≤ hi) : Thm4Hyp Ψ lo hi :=
  ⟨lo, ⟨le_rfl, h⟩, fun _ hw hlt => absurd hlt (not_lt.mpr hw.1)⟩

/-- So the printed hypothesis does not stop the candidate bid `Ψ(w, w)` from being strictly
    increasing, which is the efficient case of Lemma 3. -/
theorem thm4_hyp_with_increasing_bid : Thm4Hyp id 0 1 ∧ StrictMonoOn id (Set.Icc (0 : ℝ) 1) :=
  ⟨thm4_hyp_always id 0 1 zero_le_one, strictMono_id.strictMonoOn _⟩

/-- **With `w̃` above the bottom of the support**, the candidate bid is not strictly increasing. -/
theorem thm4_interior (Ψ : ℝ → ℝ) (lo hi w0 : ℝ) (hw0 : w0 ∈ Set.Ioc lo hi)
    (h : ∀ w ∈ Set.Icc lo hi, w < w0 → Ψ w0 ≤ Ψ w) : ¬ StrictMonoOn Ψ (Set.Icc lo hi) := by
  intro hmono
  have hlo : lo ∈ Set.Icc lo hi := ⟨le_rfl, hw0.1.le.trans hw0.2⟩
  have hw0' : w0 ∈ Set.Icc lo hi := ⟨hw0.1.le, hw0.2⟩
  have h1 := hmono hlo hw0' hw0.1
  have h2 := h lo hlo hw0.1
  linarith

end FullertonMcAfee
