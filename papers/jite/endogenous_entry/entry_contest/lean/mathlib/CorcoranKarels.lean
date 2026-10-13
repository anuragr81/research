import Mathlib
import Corcoran

/-!
# Corcoran and Karels (1985), "Rent-seeking behavior in the long-run"

Public Choice 46: 227-246.  Page numbers are the printed ones.

Setup (pp.227-228): identical players bet on a Tullock lottery with winning probability
`a^r/(a^r + Σ b_i^r)` for a prize `X` (100 in every example).  The symmetric Cournot-Nash
bet is eq. (3), `a = (n-1) r X / n²` (`Corcoran1984.aStar`, proved in `Corcoran.lean`), and
zero profit gives eq. (4), `n = r/(r-1)`.

## What is proved

1. Section 1 (pp.228-229): eq. (5) inverts eq. (4) (`eq5_iff`); `r = 1.6` gives `n = 8/3`,
   printed 2.6 (`n_at_r16`); the numbers at `r = 3` (`r3_numbers`).
2. Section 2 (pp.230-233), `r ≤ 1`: three misprinted cells of Table 1 (`table1_r05_n1000`,
   `table1_r075_n4`, `table1_r075_n1000`) and one of Table 2 (`table2_r05_n110`); the
   splitting example (`split_r05`), and splitting failing at `r = 1` (`split_fails_r1`,
   `split_neutral_r1`); the minimum bet: when it binds (`minbet_binds`), the counts of
   Table 2 (`minbet_count_1`, `minbet_count_099`), dissipation exactly when `X/Z` is whole
   (`minbet_dissipation`), and the best reply of rule (2) on p.231 (`minbet_best_reply`).
3. Note 3: the derivatives of the winning probability (`F_hasDerivAt`, `F'_hasDerivAt`),
   the printed concavity condition (`note3_iff`) and `r ≤ 1` sufficing (`note3_sufficient`).
4. Section 3 and notes 8, 10, `r > 1`: against committed bets with `Σ b_i^r ≥ Bc` the
   entrant's best reply earns at most zero, and exactly zero at `(r-1)X/r` (`key_amgm`,
   `ent_le`, `ent_zero`); below `Bc` entry pays (`ent_pos_of_lt`); note 8's bet
   (`note8`, `deter_iff`); the low end falls more slowly than the upper end
   (`low_end_slower`); the one-player minimum is `X/2`, at `r = 2` (`blow_one_ge_half`,
   `blow_one_r2`).
5. Sections 4-5: the range is non-empty exactly when `(m(r-1))^{r-1} ≤ r^r`
   (`range_nonempty_iff`), which gives every hardball count of Table 6 (`count_r11` to
   `count_r6`) and two sustained players at `r = 4.2` against "for r-values larger than 4,
   only one player can be sustained" (`two_sustained_r42`); the numbers in the text
   (`cn4_r14`, `cn_band_r11`, `blow_r2_m4`, `table3_r12_m1`, `text_r12_m14`,
   `text_r25_m3`).

## Not formalised

The Kuhn-Tucker derivation of note 4 beyond rule (2), the claim that the maximum number of
incumbents falls as `r` rises (checked at the table's values only, by the counts), the
policy reading of transactions costs and licences (pp.240-242), and the welfare discussion.
-/

open Real Set Filter

namespace CorcoranKarels1985

open Corcoran1984 (aStar V V_symm)

/-! ## 0. Rational exponents -/

/-- For `q > 0`, comparing `x^s` with `y^t` when `s q = a` and `t q = b` is comparing `x^a`
with `y^b`. -/
theorem rpow_cmp_pow (x y s t : ℝ) (hx : 0 ≤ x) (hy : 0 ≤ y) (q a b : ℕ) (hq : 0 < q)
    (hs : s * q = a) (ht : t * q = b) : x ^ s ≤ y ^ t ↔ x ^ a ≤ y ^ b := by
  rw [← pow_le_pow_iff_left₀ (Real.rpow_nonneg hx s) (Real.rpow_nonneg hy t) hq.ne',
    ← Real.rpow_natCast (x ^ s), ← Real.rpow_natCast (y ^ t), ← Real.rpow_mul hx,
    ← Real.rpow_mul hy, hs, ht, Real.rpow_natCast, Real.rpow_natCast]

theorem lt_rpow_iff (c x s : ℝ) (hc : 0 ≤ c) (hx : 0 ≤ x) (q a : ℕ) (hq : 0 < q)
    (hs : s * q = a) : c < x ^ s ↔ c ^ q < x ^ a := by
  rw [← pow_lt_pow_iff_left₀ hc (Real.rpow_nonneg hx s) hq.ne', ← Real.rpow_natCast (x ^ s),
    ← Real.rpow_mul hx, hs, Real.rpow_natCast]

theorem rpow_lt_iff (c x s : ℝ) (hc : 0 ≤ c) (hx : 0 ≤ x) (q a : ℕ) (hq : 0 < q)
    (hs : s * q = a) : x ^ s < c ↔ x ^ a < c ^ q := by
  rw [← pow_lt_pow_iff_left₀ (Real.rpow_nonneg hx s) hc hq.ne', ← Real.rpow_natCast (x ^ s),
    ← Real.rpow_mul hx, hs, Real.rpow_natCast]

/-! ## 1. Section 1 (pp.227-229) -/

/-- Eq. (5) inverts eq. (4). -/
theorem eq5_iff (r n : ℝ) (hr : 1 < r) (hn : 1 < n) : r = n / (n - 1) ↔ n = r / (r - 1) := by
  rw [eq_div_iff (by linarith), eq_div_iff (by linarith)]
  constructor <;> intro h <;> linarith

/-- p.228: at `r = 1.6`, eq. (4) gives `n = 8/3`; the text prints 2.6. -/
theorem n_at_r16 : (1.6 : ℝ) / (1.6 - 1) = 8 / 3 ∧ (8 / 3 : ℝ) ≠ 2.6 := by norm_num

/-- pp.228-229: at `r = 3`, eq. (4) gives `n = 1.5`; with two players each bets 75 and expects
to lose 25. -/
theorem r3_numbers :
    (3 : ℝ) / (3 - 1) = 1.5 ∧ aStar 100 3 2 = 75 ∧ 100 / 2 - aStar 100 3 2 = -25 := by
  unfold aStar; norm_num

/-! ## 2. Section 2 (pp.230-233): `r ≤ 1` -/

/-- Table 1, `r = .5`, `n = 1000`: the bet is .04995 (printed .0004995) and the profit .05005;
the printed profit .0995005 is 0.1 less the printed bet. -/
theorem table1_r05_n1000 :
    aStar 100 (1 / 2) 1000 = 0.04995 ∧ aStar 100 (1 / 2) 1000 ≠ 0.0004995 ∧
      100 / 1000 - aStar 100 (1 / 2) 1000 = 0.05005 ∧ (100 / 1000 : ℝ) - 0.0004995 = 0.0995005 := by
  unfold aStar; norm_num

/-- Table 1, `r = .75`, `n = 4`: the bet is 14.0625 (printed 14.0675); the printed profit
10.9375 is right. -/
theorem table1_r075_n4 :
    aStar 100 (3 / 4) 4 = 14.0625 ∧ aStar 100 (3 / 4) 4 ≠ 14.0675 ∧
      100 / 4 - aStar 100 (3 / 4) 4 = 10.9375 := by
  unfold aStar; norm_num

/-- Table 1, `r = .75`, `n = 1000`: the bet .074925 is right, and the profit is .025075
(printed .0025075). -/
theorem table1_r075_n1000 :
    aStar 100 (3 / 4) 1000 = 0.074925 ∧ 100 / 1000 - aStar 100 (3 / 4) 1000 = 0.025075 ∧
      100 / 1000 - aStar 100 (3 / 4) 1000 ≠ 0.0025075 := by
  unfold aStar; norm_num

/-- Table 2, `r = .5`, `n = 110`: the Cournot-Nash bet is between .4504 and .4505 (printed
.492). -/
theorem table2_r05_n110 : 0.4504 < aStar 100 (1 / 2) 110 ∧ aStar 100 (1 / 2) 110 < 0.4505 := by
  unfold aStar; norm_num

/-- p.230: with `r = .5`, a player with two of three bets earns 44.44 against 37.50 with one
of two, and splitting again earns 31.25 against 22.22. -/
theorem split_r05 :
    100 / 2 - aStar 100 (1 / 2) 2 = 37.5 ∧ 2 * (100 / 3 - aStar 100 (1 / 2) 3) = 400 / 9 ∧
      (37.5 : ℝ) < 400 / 9 ∧ 2 * (100 / 4 - aStar 100 (1 / 2) 4) = 31.25 ∧
      100 / 3 - aStar 100 (1 / 2) 3 < 31.25 := by
  unfold aStar; norm_num

/-- p.230 says splitting always pays for `r ≤ 1` and cites Table 1's other `r` values; at
`r = 1` two of three bets earn 22.22, less than the 25 of one of two. -/
theorem split_fails_r1 :
    100 / 2 - aStar 100 1 2 = 25 ∧ 2 * (100 / 3 - aStar 100 1 3) = 200 / 9 ∧
      (200 / 9 : ℝ) < 25 := by
  unfold aStar; norm_num

/-- At `r = 1`, with the rival's bet held fixed, two bets of `a` earn exactly what one bet of
`2a` earns, so splitting gains nothing either way. -/
theorem split_neutral_r1 (X a b : ℝ) :
    X * ((a + a) / (a + a + b)) - (a + a) = X * (2 * a / (2 * a + b)) - 2 * a := by ring

/-- p.231: with `r = .5`, the Cournot-Nash bet falls below the minimum of $1 from 49 players
on ("actually 49 or more players forces a minimum bid"). -/
theorem minbet_binds : 1 < aStar 100 (1 / 2) 48 ∧ aStar 100 (1 / 2) 49 < 1 := by
  unfold aStar; norm_num

/-- Table 2, minimum $1: with everyone at the minimum, profit is non-negative exactly up to
100 players. -/
theorem minbet_count_1 (n : ℝ) (hn : 0 < n) : 0 ≤ 100 / n - 1 ↔ n ≤ 100 := by
  rw [sub_nonneg, le_div_iff₀ hn]
  constructor <;> intro h <;> linarith

/-- Table 2, minimum $.99: 101 players earn .000099 each and 102 lose .0096 each. -/
theorem minbet_count_099 :
    0.000099 < (100 : ℝ) / 101 - 0.99 ∧ (100 : ℝ) / 101 - 0.99 < 0.0001 ∧
      -0.0097 < (100 : ℝ) / 102 - 0.99 ∧ (100 : ℝ) / 102 - 0.99 < -0.0096 := by
  norm_num

/-- p.232 and Section 6: with every entrant at the minimum `Z`, the largest count with
non-negative profit is `⌊X/Z⌋`, and the bets use up the rent exactly when `X/Z` is whole. -/
theorem minbet_dissipation (X Z : ℝ) (hX : 0 ≤ X) (hZ : 0 < Z) :
    (⌊X / Z⌋₊ : ℝ) * Z ≤ X ∧ X < ((⌊X / Z⌋₊ : ℝ) + 1) * Z ∧
      ((⌊X / Z⌋₊ : ℝ) * Z = X ↔ ∃ k : ℕ, X = k * Z) := by
  have h0 : 0 ≤ X / Z := div_nonneg hX hZ.le
  have hXZ : X / Z * Z = X := by field_simp
  refine ⟨?_, ?_, ?_⟩
  · calc (⌊X / Z⌋₊ : ℝ) * Z ≤ X / Z * Z := mul_le_mul_of_nonneg_right (Nat.floor_le h0) hZ.le
      _ = X := hXZ
  · calc X = X / Z * Z := hXZ.symm
      _ < ((⌊X / Z⌋₊ : ℝ) + 1) * Z := mul_lt_mul_of_pos_right (Nat.lt_floor_add_one _) hZ
  · constructor
    · intro h
      exact ⟨_, h.symm⟩
    · rintro ⟨k, hk⟩
      have : X / Z = k := by rw [hk]; field_simp
      rw [this, Nat.floor_natCast, hk]

theorem share_tangent (c k y : ℝ) (hc : 0 < c) (hk : 0 < k) (hy : 0 ≤ y) :
    y / (y + k) ≤ c / (c + k) + k * (y - c) / (c + k) ^ 2 := by
  have h1 : 0 < y + k := by linarith
  have h2 : 0 < c + k := by linarith
  have key : c / (c + k) + k * (y - c) / (c + k) ^ 2 - y / (y + k)
      = k * (y - c) ^ 2 / ((y + k) * (c + k) ^ 2) := by
    field_simp
    ring
  have : 0 ≤ k * (y - c) ^ 2 / ((y + k) * (c + k) ^ 2) := by positivity
  linarith

theorem rpow_le_tangent (b a r : ℝ) (hb : 0 < b) (ha : 0 ≤ a) (hr0 : 0 < r) (hr : r ≤ 1) :
    a ^ r ≤ b ^ r * (1 + r * (a / b - 1)) := by
  have hab : 0 ≤ a / b := div_nonneg ha hb.le
  have hB := rpow_one_add_le_one_add_mul_self (s := a / b - 1) (by linarith) hr0.le hr
  have e : a = b * (a / b) := by field_simp
  have : a ^ r = b ^ r * (a / b) ^ r := by
    conv_lhs => rw [e]
    exact Real.mul_rpow hb.le hab
  rw [this]
  apply mul_le_mul_of_nonneg_left _ (Real.rpow_nonneg hb.le r)
  have e2 : (1 : ℝ) + (a / b - 1) = a / b := by ring
  rw [e2] at hB
  exact hB

/-- p.231, rule (2) (note 4): for `r ≤ 1`, when every rival bets the minimum `Z` and the
Cournot-Nash bet is at most `Z`, no bet of at least `Z` does better than `Z`. -/
theorem minbet_best_reply (R r n Z a : ℝ) (hR : 0 < R) (hr0 : 0 < r) (hr : r ≤ 1) (hn : 1 < n)
    (hZ : 0 < Z) (hcn : aStar R r n ≤ Z) (ha : Z ≤ a) : V R r n Z a ≤ V R r n Z Z := by
  have hn0 : (0 : ℝ) < n := by linarith
  have hn1 : 0 < n - 1 := by linarith
  obtain ⟨c, hcdef⟩ : ∃ c, c = Z ^ r := ⟨_, rfl⟩
  obtain ⟨k, hkdef⟩ : ∃ k, k = (n - 1) * c := ⟨_, rfl⟩
  obtain ⟨Y, hY⟩ : ∃ Y, Y = c * (1 + r * (a / Z - 1)) := ⟨_, rfl⟩
  have hc : 0 < c := hcdef ▸ Real.rpow_pos_of_pos hZ r
  have hk : 0 < k := by rw [hkdef]; positivity
  have ha0 : 0 ≤ a := by linarith
  have hay : a ^ r ≤ Y := by rw [hY, hcdef]; exact rpow_le_tangent Z a r hZ ha0 hr0 hr
  have hY0 : 0 ≤ Y := le_trans (Real.rpow_nonneg ha0 r) hay
  have hmono : a ^ r / (a ^ r + k) ≤ Y / (Y + k) := by
    have h1 : 0 ≤ a ^ r := Real.rpow_nonneg ha0 r
    rw [div_le_div_iff₀ (by linarith) (by linarith)]
    nlinarith
  have htan := share_tangent c k Y hc hk hY0
  have hck : c + k = n * c := by rw [hkdef]; ring
  have hval : c / (c + k) + k * (Y - c) / (c + k) ^ 2
      = 1 / n + (n - 1) * r * (a - Z) / (n ^ 2 * Z) := by
    have hc0 : c ≠ 0 := hc.ne'
    have hZ0 : Z ≠ 0 := hZ.ne'
    rw [hck, hkdef, hY]
    field_simp
    ring
  have hsum : a ^ r / (a ^ r + k) ≤ 1 / n + (n - 1) * r * (a - Z) / (n ^ 2 * Z) := by
    rw [← hval]; exact le_trans hmono htan
  have hcnZ : R * (r * (n - 1)) ≤ n ^ 2 * Z := by
    unfold aStar at hcn
    rw [div_le_iff₀ (by positivity)] at hcn
    nlinarith
  have h2 : R * ((n - 1) * r * (a - Z) / (n ^ 2 * Z)) ≤ a - Z := by
    rw [mul_div_assoc', div_le_iff₀ (by positivity)]
    have : 0 ≤ a - Z := by linarith
    nlinarith
  have h1 : R * (a ^ r / (a ^ r + k)) ≤ R * (1 / n + (n - 1) * r * (a - Z) / (n ^ 2 * Z)) :=
    mul_le_mul_of_nonneg_left hsum hR.le
  rw [V_symm R r n Z hZ hn0]
  unfold V
  rw [← hcdef, ← hkdef]
  have h3 : R * (1 / n + (n - 1) * r * (a - Z) / (n ^ 2 * Z))
      = R / n + R * ((n - 1) * r * (a - Z) / (n ^ 2 * Z)) := by ring
  linarith

/-! ## 3. Note 3: concavity of the winning probability -/

theorem F_hasDerivAt (r B a : ℝ) (ha : 0 < a) (hB : 0 ≤ B) :
    HasDerivAt (fun x => x ^ r / (x ^ r + B)) (r * a ^ (r - 1) * B / (a ^ r + B) ^ 2) a := by
  have hf : HasDerivAt (fun x : ℝ => x ^ r) (r * a ^ (r - 1)) a :=
    Real.hasDerivAt_rpow_const (Or.inl ha.ne')
  have hpos : 0 < a ^ r + B := by have := Real.rpow_pos_of_pos ha r; linarith
  have h := hf.div (hf.add_const B) hpos.ne'
  refine h.congr_deriv ?_
  field_simp
  ring

theorem F'_hasDerivAt (r B a : ℝ) (ha : 0 < a) (hB : 0 ≤ B) :
    HasDerivAt (fun x => r * x ^ (r - 1) * B / (x ^ r + B) ^ 2)
      (r * B * a ^ (r - 2) * ((r - 1) * (a ^ r + B) - 2 * r * a ^ r) / (a ^ r + B) ^ 3) a := by
  have hf : HasDerivAt (fun x : ℝ => x ^ r) (r * a ^ (r - 1)) a :=
    Real.hasDerivAt_rpow_const (Or.inl ha.ne')
  have hg : HasDerivAt (fun x : ℝ => x ^ (r - 1)) ((r - 1) * a ^ (r - 1 - 1)) a :=
    Real.hasDerivAt_rpow_const (Or.inl ha.ne')
  have hpos : 0 < a ^ r + B := by have := Real.rpow_pos_of_pos ha r; linarith
  have hden : HasDerivAt (fun x : ℝ => (x ^ r + B) ^ 2) (2 * (a ^ r + B) * (r * a ^ (r - 1))) a := by
    have := (hf.add_const B).pow 2
    simpa [mul_comm, mul_assoc, mul_left_comm] using this
  have h := ((hg.const_mul r).mul_const B).div hden (by positivity)
  refine h.congr_deriv ?_
  have e1 : a ^ (r - 1 - 1) = a ^ (r - 2) := by ring_nf
  have e2 : a ^ (r - 1) * a ^ (r - 1) = a ^ (r - 2) * a ^ r := by
    rw [← Real.rpow_add ha, ← Real.rpow_add ha]; ring_nf
  have e3 : (a ^ (r - 1)) ^ 2 = a ^ (r - 2) * a ^ r := by rw [sq, e2]
  rw [e1]
  field_simp
  rw [e3]
  ring

/-- Note 3: the second derivative is negative exactly when `a⁻¹(r-1) < 2[r a^{r-1}(a^r+B)⁻¹]`,
the printed condition. -/
theorem note3_iff (r B a : ℝ) (hr : 0 < r) (ha : 0 < a) (hB : 0 < B) :
    r * B * a ^ (r - 2) * ((r - 1) * (a ^ r + B) - 2 * r * a ^ r) / (a ^ r + B) ^ 3 < 0 ↔
      a⁻¹ * (r - 1) < 2 * (r * a ^ (r - 1) * (a ^ r + B)⁻¹) := by
  have har : 0 < a ^ r := Real.rpow_pos_of_pos ha r
  have hpos : 0 < a ^ r + B := by linarith
  have hP : 0 < (a ^ r + B) ^ 3 := pow_pos hpos 3
  have hK : 0 < r * B * a ^ (r - 2) := by
    have := Real.rpow_pos_of_pos ha (r - 2); positivity
  set D := (r - 1) * (a ^ r + B) - 2 * r * a ^ r with hD
  have lhs : r * B * a ^ (r - 2) * D / (a ^ r + B) ^ 3 < 0 ↔ D < 0 := by
    rw [div_lt_iff₀ hP, zero_mul]
    constructor
    · intro h; by_contra hc; push_neg at hc; nlinarith [mul_nonneg hK.le hc]
    · intro h; exact mul_neg_of_pos_of_neg hK h
  have e : a ^ (r - 1) = a ^ r / a := by rw [Real.rpow_sub_one ha.ne']
  have diff : 2 * (r * a ^ (r - 1) * (a ^ r + B)⁻¹) - a⁻¹ * (r - 1)
      = -D / (a * (a ^ r + B)) := by
    rw [e, hD]; field_simp; ring
  have rhs : a⁻¹ * (r - 1) < 2 * (r * a ^ (r - 1) * (a ^ r + B)⁻¹) ↔ D < 0 := by
    rw [← sub_pos, diff, div_pos_iff_of_pos_right (by positivity)]
    constructor <;> intro h <;> linarith
  rw [lhs, rhs]

/-- Note 3: `r ≤ 1` suffices. -/
theorem note3_sufficient (r B a : ℝ) (hr : 0 < r) (hr1 : r ≤ 1) (ha : 0 < a) (hB : 0 < B) :
    r * B * a ^ (r - 2) * ((r - 1) * (a ^ r + B) - 2 * r * a ^ r) / (a ^ r + B) ^ 3 < 0 := by
  have har : 0 < a ^ r := Real.rpow_pos_of_pos ha r
  have hpos : 0 < a ^ r + B := by linarith
  apply div_neg_of_neg_of_pos _ (pow_pos hpos 3)
  apply mul_neg_of_pos_of_neg (by positivity)
  nlinarith

/-! ## 4. Section 3 and notes 8, 10: the entrant against committed bets, `r > 1` -/

/-- The profit of an entrant who bets `e` against committed bets with `Σ b_i^r = B`. -/
noncomputable def ent (X r B e : ℝ) : ℝ := X * (e ^ r / (e ^ r + B)) - e

/-- The critical `Σ b_i^r`. -/
noncomputable def Bc (X r : ℝ) : ℝ := X / r * (X * (r - 1) / r) ^ (r - 1)

/-- Note 8: the lowest pre-emptive bet when `m` incumbents bet alike. -/
noncomputable def blow (X r m : ℝ) : ℝ := X * (r - 1) / r / (m * (r - 1)) ^ (1 / r)

theorem Bc_pos (X r : ℝ) (hX : 0 < X) (hr : 1 < r) : 0 < Bc X r := by
  unfold Bc
  have : 0 < X * (r - 1) / r := by apply div_pos (mul_pos hX (by linarith)) (by linarith)
  have := Real.rpow_pos_of_pos this (r - 1)
  have : 0 < X / r := div_pos hX (by linarith)
  positivity

/-- Weighted AM-GM with weights `(r-1)/r` and `1/r`: `X e^{r-1} ≤ e^r + Bc` for `e > 0`. -/
theorem key_amgm (X r e : ℝ) (hX : 0 < X) (hr : 1 < r) (he : 0 < e) :
    X * e ^ (r - 1) ≤ e ^ r + Bc X r := by
  have hr0 : 0 < r := by linarith
  have hr1 : 0 < r - 1 := by linarith
  set c := X * (r - 1) / r with hcdef
  have hc : 0 < c := by rw [hcdef]; exact div_pos (mul_pos hX hr1) hr0
  have her : 0 < e ^ r := Real.rpow_pos_of_pos he r
  have hw : (r - 1) / r + 1 / r = 1 := by
    rw [← add_div, sub_add_cancel, div_self hr0.ne']
  have hp1 : 0 ≤ r * e ^ r / (r - 1) := by positivity
  have hp2 : 0 ≤ r * Bc X r := by have := Bc_pos X r hX hr; positivity
  have h := Real.geom_mean_le_arith_mean2_weighted (div_nonneg hr1.le hr0.le)
    (div_nonneg zero_le_one hr0.le) hp1 hp2 hw
  have rhs : (r - 1) / r * (r * e ^ r / (r - 1)) + 1 / r * (r * Bc X r) = e ^ r + Bc X r := by
    field_simp
  have hcr : 0 < c ^ (r - 1) := Real.rpow_pos_of_pos hc _
  have p2 : r * Bc X r = X * c ^ (r - 1) := by
    unfold Bc; rw [← hcdef]; field_simp
  have l1 : (r * e ^ r / (r - 1)) ^ ((r - 1) / r) = (r / (r - 1)) ^ ((r - 1) / r) * e ^ (r - 1) := by
    rw [show r * e ^ r / (r - 1) = r / (r - 1) * e ^ r by ring,
      Real.mul_rpow (by positivity) her.le, ← Real.rpow_mul he.le]
    congr 2
    field_simp
  have l2 : (X * c ^ (r - 1)) ^ (1 / r) = X ^ (1 / r) * c ^ ((r - 1) / r) := by
    rw [Real.mul_rpow hX.le hcr.le, ← Real.rpow_mul hc.le]
    congr 2
    ring
  have l3 : (r / (r - 1)) ^ ((r - 1) / r) * c ^ ((r - 1) / r) = X ^ ((r - 1) / r) := by
    rw [← Real.mul_rpow (by positivity) hc.le]
    congr 1
    rw [hcdef]; field_simp
  have l4 : X ^ ((r - 1) / r) * X ^ (1 / r) = X := by
    rw [← Real.rpow_add hX, hw, Real.rpow_one]
  have lhs : (r * e ^ r / (r - 1)) ^ ((r - 1) / r) * (r * Bc X r) ^ (1 / r) = X * e ^ (r - 1) := by
    rw [p2, l1, l2]
    calc (r / (r - 1)) ^ ((r - 1) / r) * e ^ (r - 1) * (X ^ (1 / r) * c ^ ((r - 1) / r))
        = ((r / (r - 1)) ^ ((r - 1) / r) * c ^ ((r - 1) / r)) * X ^ (1 / r) * e ^ (r - 1) := by
          ring
      _ = X * e ^ (r - 1) := by rw [l3, l4]
  rw [← lhs, ← rhs]
  exact h

/-- Notes 8 and 10: against committed bets with `Σ b_i^r ≥ Bc`, no bet earns the entrant a
positive profit. -/
theorem ent_le (X r B e : ℝ) (hX : 0 < X) (hr : 1 < r) (he : 0 ≤ e) (hB : Bc X r ≤ B) :
    ent X r B e ≤ 0 := by
  have hBc := Bc_pos X r hX hr
  have hB0 : 0 < B := lt_of_lt_of_le hBc hB
  unfold ent
  rcases he.eq_or_lt with h | h
  · rw [← h, Real.zero_rpow (by linarith)]
    simp
  · have her : 0 < e ^ r := Real.rpow_pos_of_pos h r
    have hk := key_amgm X r e hX hr h
    have esplit : e ^ r = e * e ^ (r - 1) := by
      rw [Real.rpow_sub_one h.ne']; field_simp
    rw [sub_nonpos, mul_div_assoc', div_le_iff₀ (by linarith), esplit]
    have : 0 ≤ e := h.le
    nlinarith [mul_le_mul_of_nonneg_left hk this]

/-- Note 10: at `Σ b_i^r = Bc`, the bet `(r-1)X/r` earns the entrant exactly zero. -/
theorem ent_zero (X r : ℝ) (hX : 0 < X) (hr : 1 < r) :
    ent X r (Bc X r) (X * (r - 1) / r) = 0 := by
  have hr0 : 0 < r := by linarith
  set c := X * (r - 1) / r with hcdef
  have hc : 0 < c := by rw [hcdef]; exact div_pos (mul_pos hX (by linarith)) hr0
  have hcr : 0 < c ^ (r - 1) := Real.rpow_pos_of_pos hc _
  have esplit : c ^ r = c * c ^ (r - 1) := by
    rw [Real.rpow_sub_one hc.ne']; field_simp
  have hsum : c ^ r + Bc X r = X * c ^ (r - 1) := by
    unfold Bc; rw [← hcdef, esplit, hcdef]; field_simp; ring
  unfold ent
  rw [hsum, esplit]
  field_simp
  ring

/-- Below `Bc` the entrant profits at `(r-1)X/r`. -/
theorem ent_pos_of_lt (X r B : ℝ) (hX : 0 < X) (hr : 1 < r) (hB0 : 0 ≤ B) (hB : B < Bc X r) :
    0 < ent X r B (X * (r - 1) / r) := by
  have h0 := ent_zero X r hX hr
  have hc : 0 < X * (r - 1) / r := div_pos (mul_pos hX (by linarith)) (by linarith)
  have hcr : 0 < (X * (r - 1) / r) ^ r := Real.rpow_pos_of_pos hc r
  unfold ent at h0 ⊢
  have : (X * (r - 1) / r) ^ r / ((X * (r - 1) / r) ^ r + Bc X r) <
      (X * (r - 1) / r) ^ r / ((X * (r - 1) / r) ^ r + B) :=
    div_lt_div_of_pos_left hcr (by linarith) (by linarith)
  nlinarith

/-- Note 8: `m · blow^r = Bc`. -/
theorem note8 (X r m : ℝ) (hX : 0 < X) (hr : 1 < r) (hm : 0 < m) :
    m * blow X r m ^ r = Bc X r := by
  have hr0 : 0 < r := by linarith
  have hr1 : 0 < r - 1 := by linarith
  set c := X * (r - 1) / r with hcdef
  have hc : 0 < c := by rw [hcdef]; exact div_pos (mul_pos hX hr1) hr0
  have ht : 0 < m * (r - 1) := mul_pos hm hr1
  have htr : 0 < (m * (r - 1)) ^ (1 / r) := Real.rpow_pos_of_pos ht _
  unfold blow
  rw [← hcdef, Real.div_rpow hc.le htr.le, ← Real.rpow_mul ht.le,
    show 1 / r * r = 1 by field_simp, Real.rpow_one]
  have esplit : c ^ r = c * c ^ (r - 1) := by
    rw [Real.rpow_sub_one hc.ne']; field_simp
  unfold Bc
  rw [← hcdef, esplit, hcdef]
  field_simp

/-- A common bet `b` by `m` incumbents deters entry exactly when `b ≥ blow`. -/
theorem deter_iff (X r m b : ℝ) (hX : 0 < X) (hr : 1 < r) (hm : 0 < m) (hb : 0 ≤ b) :
    Bc X r ≤ m * b ^ r ↔ blow X r m ≤ b := by
  have hr0 : 0 < r := by linarith
  have hbl : 0 ≤ blow X r m := by
    unfold blow
    have : 0 < X * (r - 1) / r := div_pos (mul_pos hX (by linarith)) hr0
    exact div_nonneg this.le (Real.rpow_nonneg (mul_nonneg hm.le (by linarith)) _)
  rw [← note8 X r m hX hr hm, mul_le_mul_iff_right₀ hm, Real.rpow_le_rpow_iff hbl hb hr0]

/-- p.233: the low end falls more slowly than the upper end `X/m`, i.e. `m · blow` rises
with `m`. -/
theorem low_end_slower (X r m₁ m₂ : ℝ) (hX : 0 < X) (hr : 1 < r) (h1 : 0 < m₁) (h12 : m₁ < m₂) :
    m₁ * blow X r m₁ < m₂ * blow X r m₂ := by
  have hr0 : 0 < r := by linarith
  have hr1 : 0 < r - 1 := by linarith
  have h2 : 0 < m₂ := by linarith
  have hc : 0 < X * (r - 1) / r := div_pos (mul_pos hX hr1) hr0
  have form : ∀ m : ℝ, 0 < m →
      m * blow X r m = X * (r - 1) / r / (r - 1) ^ (1 / r) * m ^ (1 - 1 / r) := by
    intro m hm
    unfold blow
    rw [Real.mul_rpow hm.le hr1.le, Real.rpow_sub hm, Real.rpow_one]
    have : 0 < m ^ (1 / r) := Real.rpow_pos_of_pos hm _
    have : 0 < (r - 1) ^ (1 / r) := Real.rpow_pos_of_pos hr1 _
    field_simp
  rw [form m₁ h1, form m₂ h2]
  have hexp : 0 < 1 - 1 / r := by
    rw [sub_pos, div_lt_one hr0]; exact hr
  apply mul_lt_mul_of_pos_left (Real.rpow_lt_rpow h1.le h12 hexp)
  have : 0 < (r - 1) ^ (1 / r) := Real.rpow_pos_of_pos hr1 _
  positivity

/-- p.236: for one player the minimum pre-emptive bet is at least `X/2` for every `r > 1`. -/
theorem blow_one_ge_half (X r : ℝ) (hX : 0 < X) (hr : 1 < r) : X / 2 ≤ blow X r 1 := by
  have hr0 : 0 < r := by linarith
  have hr1 : 0 < r - 1 := by linarith
  have hw : (r - 1) / r + 1 / r = 1 := by
    rw [← add_div, sub_add_cancel, div_self hr0.ne']
  set u := r / (2 * (r - 1))
  set v := r / 2
  have hu : 0 < u := by positivity
  have hv : 0 < v := by positivity
  have h := Real.geom_mean_le_arith_mean2_weighted (div_nonneg hr1.le hr0.le)
    (div_nonneg zero_le_one hr0.le) hu.le hv.le hw
  have rhs : (r - 1) / r * u + 1 / r * v = 1 := by
    simp only [u, v]; field_simp; ring
  rw [rhs] at h
  have hu' : u = v / (r - 1) := by simp only [u, v]; field_simp
  rw [hu', Real.div_rpow hv.le hr1.le] at h
  have hvv : v ^ ((r - 1) / r) * v ^ (1 / r) = v := by
    rw [← Real.rpow_add hv, hw, Real.rpow_one]
  have hq : 0 < (r - 1) ^ ((r - 1) / r) := Real.rpow_pos_of_pos hr1 _
  have hle : v ≤ (r - 1) ^ ((r - 1) / r) := by
    have : v ^ ((r - 1) / r) / (r - 1) ^ ((r - 1) / r) * v ^ (1 / r)
        = v / (r - 1) ^ ((r - 1) / r) := by
      rw [div_mul_eq_mul_div, hvv]
    rw [this, div_le_one hq] at h
    exact h
  have hsplit : (r - 1) ^ ((r - 1) / r) = (r - 1) / (r - 1) ^ (1 / r) := by
    rw [show (r - 1) / r = 1 - 1 / r by field_simp, Real.rpow_sub hr1, Real.rpow_one]
  rw [hsplit] at hle
  unfold blow
  rw [one_mul]
  have hs : 0 < (r - 1) ^ (1 / r) := Real.rpow_pos_of_pos hr1 _
  rw [le_div_iff₀ hs]
  rw [le_div_iff₀ hs] at hle
  simp only [v] at hle
  have : X / 2 * (r - 1) ^ (1 / r) = X / r * (r / 2 * (r - 1) ^ (1 / r)) := by field_simp
  rw [this, show X * (r - 1) / r = X / r * (r - 1) by ring]
  exact mul_le_mul_of_nonneg_left hle (by positivity)

/-- p.236: the one-player minimum 50.00 is attained at `r = 2`. -/
theorem blow_one_r2 (X : ℝ) : blow X 2 1 = X / 2 := by
  unfold blow
  norm_num

/-! ## 5. Sections 4-5: counts -/

/-- The range of pre-emptive bets for `m` incumbents is non-empty exactly when
`(m(r-1))^{r-1} ≤ r^r`, that is `m ≤ r^{r/(r-1)}/(r-1)`. -/
theorem range_nonempty_iff (X r m : ℝ) (hX : 0 < X) (hr : 1 < r) (hm : 0 < m) :
    blow X r m ≤ X / m ↔ (m * (r - 1)) ^ (r - 1) ≤ r ^ r := by
  have hr0 : 0 < r := by linarith
  have hr1 : 0 < r - 1 := by linarith
  set t := m * (r - 1) with htdef
  have ht : 0 < t := mul_pos hm hr1
  have hs : 0 < t ^ (1 / r) := Real.rpow_pos_of_pos ht _
  unfold blow
  rw [← htdef, div_le_div_iff₀ hs hm]
  -- X (r-1)/r · m ≤ X t^{1/r}  ↔  t/r ≤ t^{1/r}
  have e1 : X * (r - 1) / r * m = X * (t / r) := by rw [htdef]; ring
  rw [e1, mul_le_mul_iff_right₀ hX]
  -- t/r ≤ t^{1/r} ↔ t^{1-1/r} ≤ r
  have e2 : t / r ≤ t ^ (1 / r) ↔ t ^ (1 - 1 / r) ≤ r := by
    rw [Real.rpow_sub ht, Real.rpow_one, div_le_iff₀ hs, div_le_iff₀ hr0]
    constructor <;> intro h <;> linarith
  rw [e2]
  have hexp : 0 < 1 - 1 / r := by rw [sub_pos, div_lt_one hr0]; exact hr
  rw [← Real.rpow_le_rpow_iff (Real.rpow_nonneg ht.le _) hr0.le hr0, ← Real.rpow_mul ht.le,
    show (1 - 1 / r) * r = r - 1 by field_simp]

/-- The rational-exponent form of `range_nonempty_iff`, for `r = p/q`. -/
theorem sust_iff_pow (r m : ℝ) (hr : 1 < r) (hm : 0 < m) (q a b : ℕ) (hq : 0 < q)
    (ha : (r - 1) * q = a) (hb : r * q = b) :
    blow 100 r m ≤ 100 / m ↔ (m * (r - 1)) ^ a ≤ r ^ b := by
  rw [range_nonempty_iff 100 r m (by norm_num) hr hm]
  exact rpow_cmp_pow _ _ _ _ (mul_pos hm (by linarith)).le (by linarith) q a b hq ha hb

/-- Table 6 and p.240: at `r = 1.1` the hardball count is 28. -/
theorem count_r11 : blow 100 (11 / 10) 28 ≤ 100 / 28 ∧ ¬ blow 100 (11 / 10) 29 ≤ 100 / 29 := by
  rw [sust_iff_pow _ _ (by norm_num) (by norm_num) 10 1 11 (by norm_num) (by norm_num) (by norm_num),
    sust_iff_pow _ _ (by norm_num) (by norm_num) 10 1 11 (by norm_num) (by norm_num) (by norm_num)]
  norm_num

theorem count_r12 : blow 100 (6 / 5) 14 ≤ 100 / 14 ∧ ¬ blow 100 (6 / 5) 15 ≤ 100 / 15 := by
  rw [sust_iff_pow _ _ (by norm_num) (by norm_num) 5 1 6 (by norm_num) (by norm_num) (by norm_num),
    sust_iff_pow _ _ (by norm_num) (by norm_num) 5 1 6 (by norm_num) (by norm_num) (by norm_num)]
  norm_num

theorem count_r13 : blow 100 (13 / 10) 10 ≤ 100 / 10 ∧ ¬ blow 100 (13 / 10) 11 ≤ 100 / 11 := by
  rw [sust_iff_pow _ _ (by norm_num) (by norm_num) 10 3 13 (by norm_num) (by norm_num) (by norm_num),
    sust_iff_pow _ _ (by norm_num) (by norm_num) 10 3 13 (by norm_num) (by norm_num) (by norm_num)]
  norm_num

/-- p.236 and p.240: at `r = 1.4` the count is 8. -/
theorem count_r14 : blow 100 (7 / 5) 8 ≤ 100 / 8 ∧ ¬ blow 100 (7 / 5) 9 ≤ 100 / 9 := by
  rw [sust_iff_pow _ _ (by norm_num) (by norm_num) 5 2 7 (by norm_num) (by norm_num) (by norm_num),
    sust_iff_pow _ _ (by norm_num) (by norm_num) 5 2 7 (by norm_num) (by norm_num) (by norm_num)]
  norm_num

/-- p.240: at `r = 1.5` the count is 6. -/
theorem count_r15 : blow 100 (3 / 2) 6 ≤ 100 / 6 ∧ ¬ blow 100 (3 / 2) 7 ≤ 100 / 7 := by
  rw [sust_iff_pow _ _ (by norm_num) (by norm_num) 2 1 3 (by norm_num) (by norm_num) (by norm_num),
    sust_iff_pow _ _ (by norm_num) (by norm_num) 2 1 3 (by norm_num) (by norm_num) (by norm_num)]
  norm_num

theorem count_r16 : blow 100 (8 / 5) 5 ≤ 100 / 5 ∧ ¬ blow 100 (8 / 5) 6 ≤ 100 / 6 := by
  rw [sust_iff_pow _ _ (by norm_num) (by norm_num) 5 3 8 (by norm_num) (by norm_num) (by norm_num),
    sust_iff_pow _ _ (by norm_num) (by norm_num) 5 3 8 (by norm_num) (by norm_num) (by norm_num)]
  norm_num

theorem count_r18 : blow 100 (9 / 5) 4 ≤ 100 / 4 ∧ ¬ blow 100 (9 / 5) 5 ≤ 100 / 5 := by
  rw [sust_iff_pow _ _ (by norm_num) (by norm_num) 5 4 9 (by norm_num) (by norm_num) (by norm_num),
    sust_iff_pow _ _ (by norm_num) (by norm_num) 5 4 9 (by norm_num) (by norm_num) (by norm_num)]
  norm_num

/-- p.240: at `r = 2.0` the count is 4, where the two ends of the range meet. -/
theorem count_r2 : blow 100 2 4 ≤ 100 / 4 ∧ ¬ blow 100 2 5 ≤ 100 / 5 := by
  rw [sust_iff_pow _ _ (by norm_num) (by norm_num) 1 1 2 (by norm_num) (by norm_num) (by norm_num),
    sust_iff_pow _ _ (by norm_num) (by norm_num) 1 1 2 (by norm_num) (by norm_num) (by norm_num)]
  norm_num

theorem count_r25 : blow 100 (5 / 2) 3 ≤ 100 / 3 ∧ ¬ blow 100 (5 / 2) 4 ≤ 100 / 4 := by
  rw [sust_iff_pow _ _ (by norm_num) (by norm_num) 2 3 5 (by norm_num) (by norm_num) (by norm_num),
    sust_iff_pow _ _ (by norm_num) (by norm_num) 2 3 5 (by norm_num) (by norm_num) (by norm_num)]
  norm_num

/-- p.236: at `r = 3.0` the count is 2. -/
theorem count_r3 : blow 100 3 2 ≤ 100 / 2 ∧ ¬ blow 100 3 3 ≤ 100 / 3 := by
  rw [sust_iff_pow _ _ (by norm_num) (by norm_num) 1 2 3 (by norm_num) (by norm_num) (by norm_num),
    sust_iff_pow _ _ (by norm_num) (by norm_num) 1 2 3 (by norm_num) (by norm_num) (by norm_num)]
  norm_num

theorem count_r4 : blow 100 4 2 ≤ 100 / 2 ∧ ¬ blow 100 4 3 ≤ 100 / 3 := by
  rw [sust_iff_pow _ _ (by norm_num) (by norm_num) 1 3 4 (by norm_num) (by norm_num) (by norm_num),
    sust_iff_pow _ _ (by norm_num) (by norm_num) 1 3 4 (by norm_num) (by norm_num) (by norm_num)]
  norm_num

theorem count_r5 : blow 100 5 1 ≤ 100 / 1 ∧ ¬ blow 100 5 2 ≤ 100 / 2 := by
  rw [sust_iff_pow _ _ (by norm_num) (by norm_num) 1 4 5 (by norm_num) (by norm_num) (by norm_num),
    sust_iff_pow _ _ (by norm_num) (by norm_num) 1 4 5 (by norm_num) (by norm_num) (by norm_num)]
  norm_num

theorem count_r6 : blow 100 6 1 ≤ 100 / 1 ∧ ¬ blow 100 6 2 ≤ 100 / 2 := by
  rw [sust_iff_pow _ _ (by norm_num) (by norm_num) 1 5 6 (by norm_num) (by norm_num) (by norm_num),
    sust_iff_pow _ _ (by norm_num) (by norm_num) 1 5 6 (by norm_num) (by norm_num) (by norm_num)]
  norm_num

/-- p.240 says that for `r` larger than 4 only one player can be sustained; at `r = 4.2` two
can. -/
theorem two_sustained_r42 : blow 100 (21 / 5) 2 ≤ 100 / 2 := by
  rw [sust_iff_pow _ _ (by norm_num) (by norm_num) 5 16 21 (by norm_num) (by norm_num) (by norm_num)]
  norm_num

/-- p.236: at `r = 1.4` the Cournot-Nash bet for four players, 26.25, exceeds the maximum
pre-emptive bet 25. -/
theorem cn4_r14 : aStar 100 (7 / 5) 4 = 26.25 ∧ (100 / 4 : ℝ) < aStar 100 (7 / 5) 4 := by
  unfold aStar; norm_num

/-- p.236: at `r = 1.1` the Cournot-Nash bet lies below the range at 4 players (entry), within
it at 5, at its top at 11 (zero profit), and above it at 12 (loss). -/
theorem cn_band_r11 :
    aStar 100 (11 / 10) 4 < blow 100 (11 / 10) 4 ∧ blow 100 (11 / 10) 5 ≤ aStar 100 (11 / 10) 5 ∧
      aStar 100 (11 / 10) 11 = 100 / 11 ∧ 100 / 12 < aStar 100 (11 / 10) 12 := by
  have hy4 : ((4 : ℝ) * (11 / 10 - 1)) ^ (1 / (11 / 10 : ℝ)) < 0.44 := by
    rw [rpow_lt_iff _ _ _ (by norm_num) (by norm_num) 11 10 (by norm_num) (by norm_num)]
    norm_num
  have hy5 : (0.52 : ℝ) < ((5 : ℝ) * (11 / 10 - 1)) ^ (1 / (11 / 10 : ℝ)) := by
    rw [lt_rpow_iff _ _ _ (by norm_num) (by norm_num) 11 10 (by norm_num) (by norm_num)]
    norm_num
  have p4 : 0 < ((4 : ℝ) * (11 / 10 - 1)) ^ (1 / (11 / 10 : ℝ)) := Real.rpow_pos_of_pos (by norm_num) _
  refine ⟨?_, ?_, ?_, ?_⟩
  · unfold blow aStar
    rw [lt_div_iff₀ p4]
    nlinarith
  · unfold blow aStar
    rw [div_le_iff₀ (by linarith)]
    nlinarith
  · unfold aStar; norm_num
  · unfold aStar; norm_num

/-- p.240: at `r = 2` and 4 incumbents the range is the single bet 25. -/
theorem blow_r2_m4 : blow 100 2 4 = 25 := by
  unfold blow
  have : ((4 : ℝ) * (2 - 1)) ^ (1 / (2 : ℝ)) = 2 := by
    rw [show (4 : ℝ) * (2 - 1) = 2 ^ (2 : ℝ) by norm_num, ← Real.rpow_mul (by norm_num)]
    norm_num
  rw [this]
  norm_num

/-- Table 3 prints 67.73 for one player at `r = 1.2`; the lowest pre-emptive bet is between
63.72 and 63.73, which Tables 4 and 5 print (63.73). -/
theorem table3_r12_m1 : 63.72 < blow 100 (6 / 5) 1 ∧ blow 100 (6 / 5) 1 < 63.73 := by
  have lo : (0.26152 : ℝ) < ((1 : ℝ) * (6 / 5 - 1)) ^ (1 / (6 / 5 : ℝ)) := by
    rw [lt_rpow_iff _ _ _ (by norm_num) (by norm_num) 6 5 (by norm_num) (by norm_num)]
    norm_num
  have hi : ((1 : ℝ) * (6 / 5 - 1)) ^ (1 / (6 / 5 : ℝ)) < 0.26156 := by
    rw [rpow_lt_iff _ _ _ (by norm_num) (by norm_num) 6 5 (by norm_num) (by norm_num)]
    norm_num
  have p : 0 < ((1 : ℝ) * (6 / 5 - 1)) ^ (1 / (6 / 5 : ℝ)) := by linarith
  unfold blow
  constructor
  · rw [lt_div_iff₀ p]; nlinarith
  · rw [div_lt_iff₀ p]; nlinarith

/-- p.238 says 14 players at `r = 1.2` bet $98.18 in aggregate; the aggregate low end is between
98.9 and 99, as Tables 4 and 6 print (98.98). -/
theorem text_r12_m14 : 98.9 < 14 * blow 100 (6 / 5) 14 ∧ 14 * blow 100 (6 / 5) 14 < 99 := by
  have lo : (2.357 : ℝ) < ((14 : ℝ) * (6 / 5 - 1)) ^ (1 / (6 / 5 : ℝ)) := by
    rw [lt_rpow_iff _ _ _ (by norm_num) (by norm_num) 6 5 (by norm_num) (by norm_num)]
    norm_num
  have hi : ((14 : ℝ) * (6 / 5 - 1)) ^ (1 / (6 / 5 : ℝ)) < 2.359 := by
    rw [rpow_lt_iff _ _ _ (by norm_num) (by norm_num) 6 5 (by norm_num) (by norm_num)]
    norm_num
  have p : 0 < ((14 : ℝ) * (6 / 5 - 1)) ^ (1 / (6 / 5 : ℝ)) := by linarith
  unfold blow
  constructor
  · rw [← mul_div_assoc, lt_div_iff₀ p]; nlinarith
  · rw [← mul_div_assoc, div_lt_iff₀ p]; nlinarith

/-- p.238 says three players at `r = 2.5` bet $98.84 in aggregate, and Table 4 prints 98.94;
the aggregate low end is between 98.6 and 98.7, as Table 6 prints (98.64). -/
theorem text_r25_m3 : 98.6 < 3 * blow 100 (5 / 2) 3 ∧ 3 * blow 100 (5 / 2) 3 < 98.7 := by
  have lo : (1.824 : ℝ) < ((3 : ℝ) * (5 / 2 - 1)) ^ (1 / (5 / 2 : ℝ)) := by
    rw [lt_rpow_iff _ _ _ (by norm_num) (by norm_num) 5 2 (by norm_num) (by norm_num)]
    norm_num
  have hi : ((3 : ℝ) * (5 / 2 - 1)) ^ (1 / (5 / 2 : ℝ)) < 1.8255 := by
    rw [rpow_lt_iff _ _ _ (by norm_num) (by norm_num) 5 2 (by norm_num) (by norm_num)]
    norm_num
  have p : 0 < ((3 : ℝ) * (5 / 2 - 1)) ^ (1 / (5 / 2 : ℝ)) := by linarith
  unfold blow
  constructor
  · rw [← mul_div_assoc, lt_div_iff₀ p]; nlinarith
  · rw [← mul_div_assoc, div_lt_iff₀ p]; nlinarith

end CorcoranKarels1985
