import Mathlib

/-!
# Fu and Lu (2009), "Contest with pre-contest investment"

Economics Letters 103(3), 142-145.  Read as the authors' manuscript of February 2009
(8 pages); equation numbers are the manuscript's.

Setup (pp.3-4): `K ≥ 2` identical contestants first invest `I_k ≥ 0` (publicly observed),
which sets the marginal cost of effort `c(I_k) = λ / (λ + I_k)`, then play a Tullock contest
for a prize of one.  When all others invest `I` and `k` invests `I_k`, write
`a = λ + I_k`, `b = λ + I`, `N = (K-1) a - (K-2) b`, `D = (K-1) a + b`; eq. (9) gives `k`'s
payoff `(N / D)^2 - I_k` when `N > 0`.

## What is proved

1. The second stage: eqs. (7)-(8) solve the first-order conditions (5)-(6)
   (`effort_foc_k`, `effort_foc_other`), the payoff is the square of the winning
   probability (`stage_payoff`), and eq. (9) (`ratio_eq9`).
2. Eq. (13) as a derivative (`hasDerivAt_pay`), the symmetric solution (11)
   (`sym_foc_iff`), eqs. (12) and (14) (`effort12`, `payoff14`), and the sign of the second
   derivative used for Proposition 1(iii) (`hasDerivAt_pay'`, `soc_at_symmetric`).
3. The derivative of `Φ` printed on p.6 is wrong (`hasDerivAt_Phi`,
   `phi_deriv_printed_wrong`): `Φ` rises near zero for `K ≥ 4`.
4. Proposition 1(ii) is false for `K ≥ 4`: zero investment is an equilibrium exactly when
   `λ ≥ (K-1)(K+1)^2/(4K^3)` (`zero_inv_sufficient`, `zero_inv_necessary`), above the
   printed `2(K-1)^2/K^3`; at the paper's `λ = 0.23` and `K = 7` a deviation pays
   (`prop1ii_fails_K7`).
5. For `K = 3` the positive-investment candidate is never an equilibrium: investing
   nothing pays more for every `λ < 8/27` (`K3_zero_beats_candidate`), so Corollary 1 fails.
   The condition stated in Proposition 1(iv) compares efforts, not payoffs, and holds at
   `λ = 0.23` where the payoff comparison fails (`cor1_condition_misstated`).
6. For `K = 2` the candidate beats investing nothing (`K2_candidate_beats_zero`).

## Not formalised

That for `K = 2` the candidate is a global best reply (the paper's bell-shape argument),
and the passage from a positive second derivative at the symmetric point to a profitable
deviation for `K ≥ 4`.
-/

namespace FuLu2009

/-! ## 1. The second stage (eqs. (5)-(9)) -/

/-- Eq. (7): effort of each contestant other than `k`. -/
noncomputable def xOther (K ck c : ℝ) : ℝ := ck * (K - 1) / (c * (K - 1) + ck) ^ 2

/-- Eq. (8): effort of contestant `k`. -/
noncomputable def xK (K ck c : ℝ) : ℝ := (c * (K - 1) - ck * (K - 2)) * (K - 1) / (c * (K - 1) + ck) ^ 2

theorem total_effort (K ck c : ℝ) (hK : 1 < K) (hc : 0 < c) (hck : 0 < ck) :
    xK K ck c + (K - 1) * xOther K ck c = (K - 1) / (c * (K - 1) + ck) := by
  unfold xK xOther
  have hD : 0 < c * (K - 1) + ck := by nlinarith
  field_simp
  ring

/-- Eq. (5) holds at eqs. (7)-(8). -/
theorem effort_foc_k (K ck c : ℝ) (hK : 1 < K) (hc : 0 < c) (hck : 0 < ck) :
    (K - 1) * xOther K ck c / (xK K ck c + (K - 1) * xOther K ck c) ^ 2 = ck := by
  rw [total_effort K ck c hK hc hck]
  unfold xOther
  have hD : c * (K - 1) + ck ≠ 0 := by nlinarith
  have hK1 : (K - 1) ≠ 0 := by linarith
  field_simp
  exact div_self (by nlinarith)

/-- Eq. (6) holds at eqs. (7)-(8). -/
theorem effort_foc_other (K ck c : ℝ) (hK : 1 < K) (hc : 0 < c) (hck : 0 < ck) :
    (xK K ck c + (K - 2) * xOther K ck c) / (xK K ck c + (K - 1) * xOther K ck c) ^ 2 = c := by
  have h := total_effort K ck c hK hc hck
  have e : xK K ck c + (K - 2) * xOther K ck c =
      (xK K ck c + (K - 1) * xOther K ck c) - xOther K ck c := by ring
  rw [e, h]
  unfold xOther
  have hD : c * (K - 1) + ck ≠ 0 := by nlinarith
  have hK1 : (K - 1) ≠ 0 := by linarith
  rw [div_eq_iff (by positivity)]
  field_simp
  ring

/-- In the second stage `k` earns the square of its winning probability. -/
theorem stage_payoff (K ck c : ℝ) (hK : 1 < K) (hc : 0 < c) (hck : 0 < ck) :
    xK K ck c / (xK K ck c + (K - 1) * xOther K ck c) - ck * xK K ck c =
      ((c * (K - 1) - ck * (K - 2)) / (c * (K - 1) + ck)) ^ 2 := by
  rw [total_effort K ck c hK hc hck]
  unfold xK
  have hD : 0 < c * (K - 1) + ck := by nlinarith
  have hK1 : (K - 1) ≠ 0 := by linarith
  field_simp
  ring

/-- Eq. (9): with `c(I) = λ/(λ+I)` the winning probability is `N/D`. -/
theorem ratio_eq9 (K lam Ik I : ℝ) (hl : 0 < lam) (hIk : 0 ≤ Ik) (hI : 0 ≤ I) (hK : 1 < K) :
    (lam / (lam + I) * (K - 1) - lam / (lam + Ik) * (K - 2)) /
        (lam / (lam + I) * (K - 1) + lam / (lam + Ik)) =
      ((lam + Ik) * (K - 1) - (lam + I) * (K - 2)) / ((lam + Ik) * (K - 1) + (lam + I)) := by
  have ha : 0 < lam + Ik := by linarith
  have hb : 0 < lam + I := by linarith
  have hD : 0 < (lam + Ik) * (K - 1) + (lam + I) := by nlinarith
  field_simp

/-! ## 2. The investment stage (eqs. (11)-(14)) -/

/-- Eq. (9): `k`'s payoff with `a = λ + I_k`, `b = λ + I`. -/
noncomputable def pay (K lam Ik I : ℝ) : ℝ :=
  (((lam + Ik) * (K - 1) - (lam + I) * (K - 2)) / ((lam + Ik) * (K - 1) + (lam + I))) ^ 2 - Ik

/-- Eq. (13): the derivative of the payoff in own investment. -/
theorem hasDerivAt_pay (K lam Ik I : ℝ) (hK : 1 < K) (hD : 0 < (lam + Ik) * (K - 1) + (lam + I)) :
    HasDerivAt (fun y => pay K lam y I)
      (((lam + Ik) * (K - 1) - (lam + I) * (K - 2)) / ((lam + Ik) * (K - 1) + (lam + I)) *
        (2 * (K - 1) ^ 2 * (lam + I) / ((lam + Ik) * (K - 1) + (lam + I)) ^ 2) - 1) Ik := by
  have hN : HasDerivAt (fun y : ℝ => (lam + y) * (K - 1) - (lam + I) * (K - 2)) (K - 1) Ik := by
    have := ((hasDerivAt_id Ik).const_add lam).mul_const (K - 1)
    simpa using this.sub_const ((lam + I) * (K - 2))
  have hDd : HasDerivAt (fun y : ℝ => (lam + y) * (K - 1) + (lam + I)) (K - 1) Ik := by
    have := ((hasDerivAt_id Ik).const_add lam).mul_const (K - 1)
    simpa using this.add_const (lam + I)
  have hR := (hN.div hDd hD.ne').pow 2
  have h := hR.sub (hasDerivAt_id Ik)
  unfold pay
  refine h.congr_deriv ?_
  simp only [Pi.div_apply, Pi.pow_apply, id]
  push_cast
  field_simp
  ring

/-- Eq. (11): at a symmetric profile eq. (13) holds exactly at `λ + I* = 2(K-1)^2/K^3`. -/
theorem sym_foc_iff (K lam I : ℝ) (hK : 1 < K) (hb : 0 < lam + I) :
    ((lam + I) * (K - 1) - (lam + I) * (K - 2)) / ((lam + I) * (K - 1) + (lam + I)) *
        (2 * (K - 1) ^ 2 * (lam + I) / ((lam + I) * (K - 1) + (lam + I)) ^ 2) - 1 = 0 ↔
      lam + I = 2 * (K - 1) ^ 2 / K ^ 3 := by
  have hK0 : 0 < K := by linarith
  have e1 : (lam + I) * (K - 1) - (lam + I) * (K - 2) = lam + I := by ring
  have e2 : (lam + I) * (K - 1) + (lam + I) = (lam + I) * K := by ring
  rw [e1, e2]
  have : (lam + I) / ((lam + I) * K) * (2 * (K - 1) ^ 2 * (lam + I) / ((lam + I) * K) ^ 2) =
      2 * (K - 1) ^ 2 / ((lam + I) * K ^ 3) := by
    field_simp
  rw [this, sub_eq_zero, div_eq_one_iff_eq (by positivity)]
  constructor
  · intro h; field_simp; linarith
  · intro h; rw [h]; field_simp

/-- Eq. (14): the payoff at the symmetric candidate. -/
theorem payoff14 (K lam : ℝ) (hK : 1 < K) :
    pay K lam (2 * (K - 1) ^ 2 / K ^ 3 - lam) (2 * (K - 1) ^ 2 / K ^ 3 - lam) =
      1 / K ^ 2 * (1 - 2 * (K - 1) ^ 2 / K) + lam := by
  unfold pay
  have hK0 : 0 < K := by linarith
  have hK1 : 0 < K - 1 := by linarith
  have hb : lam + (2 * (K - 1) ^ 2 / K ^ 3 - lam) = 2 * (K - 1) ^ 2 / K ^ 3 := by ring
  rw [hb]
  have hbpos : 0 < 2 * (K - 1) ^ 2 / K ^ 3 := by positivity
  field_simp
  ring

/-- Eq. (12): effort at the symmetric candidate, `c(I*) = λ K^3 / (2(K-1)^2)`. -/
theorem effort12 (K lam : ℝ) (hK : 1 < K) (hl : 0 < lam) :
    xOther K (lam / (2 * (K - 1) ^ 2 / K ^ 3)) (lam / (2 * (K - 1) ^ 2 / K ^ 3)) =
      2 * (K - 1) ^ 3 / (lam * K ^ 5) := by
  unfold xOther
  have hK0 : 0 < K := by linarith
  have hK1 : 0 < K - 1 := by linarith
  field_simp
  ring

/-- The derivative of eq. (13)'s left side less one: the second derivative of the payoff,
whose sign is that of `(3K-5) b - (2K-2) a` (p.6). -/
theorem hasDerivAt_pay' (K lam I : ℝ) (Ik : ℝ) (hD : 0 < (lam + Ik) * (K - 1) + (lam + I)) :
    HasDerivAt (fun y => ((lam + y) * (K - 1) - (lam + I) * (K - 2)) /
        ((lam + y) * (K - 1) + (lam + I)) *
        (2 * (K - 1) ^ 2 * (lam + I) / ((lam + y) * (K - 1) + (lam + I)) ^ 2) - 1)
      (2 * (K - 1) ^ 3 * (lam + I) * ((3 * K - 5) * (lam + I) - (2 * K - 2) * (lam + Ik)) /
        ((lam + Ik) * (K - 1) + (lam + I)) ^ 4) Ik := by
  have hN : HasDerivAt (fun y : ℝ => (lam + y) * (K - 1) - (lam + I) * (K - 2)) (K - 1) Ik := by
    have := ((hasDerivAt_id Ik).const_add lam).mul_const (K - 1)
    simpa using this.sub_const ((lam + I) * (K - 2))
  have hDd : HasDerivAt (fun y : ℝ => (lam + y) * (K - 1) + (lam + I)) (K - 1) Ik := by
    have := ((hasDerivAt_id Ik).const_add lam).mul_const (K - 1)
    simpa using this.add_const (lam + I)
  have h1 := hN.div hDd hD.ne'
  have h2 := ((hasDerivAt_const Ik (2 * (K - 1) ^ 2 * (lam + I))).div (hDd.pow 2)
    (pow_ne_zero 2 hD.ne'))
  have h := (h1.mul h2).sub_const 1
  refine h.congr_deriv ?_
  simp only [Pi.div_apply, Pi.pow_apply, Pi.mul_apply]
  push_cast
  field_simp
  ring

/-- Proposition 1(iii): at the symmetric point the second derivative has the sign of
`K - 3`, so the second-order condition fails for `K ≥ 4`. -/
theorem soc_at_symmetric (K b : ℝ) (hb : 0 < b) (hK : 3 < K) :
    0 < 2 * (K - 1) ^ 3 * b * ((3 * K - 5) * b - (2 * K - 2) * b) / (b * (K - 1) + b) ^ 4 := by
  have hK1 : 0 < K - 1 := by linarith
  have : (3 * K - 5) * b - (2 * K - 2) * b = (K - 3) * b := by ring
  rw [this]
  have : 0 < K - 3 := by linarith
  have : 0 < b * (K - 1) + b := by nlinarith
  positivity

/-! ## 3. The derivative of `Φ` on p.6 -/

/-- `Φ(I_k)`: the left side of eq. (13) when the others invest nothing. -/
noncomputable def Phi (K lam y : ℝ) : ℝ :=
  (lam + y * (K - 1)) / (lam * K + y * (K - 1)) ^ 3 * (2 * (K - 1) ^ 2 * lam)

theorem hasDerivAt_Phi (K lam y : ℝ) (hv : 0 < lam * K + y * (K - 1)) :
    HasDerivAt (Phi K lam)
      (2 * (K - 1) ^ 3 * lam * (lam * (K - 3) - 2 * y * (K - 1)) / (lam * K + y * (K - 1)) ^ 4) y := by
  have hu : HasDerivAt (fun x : ℝ => lam + x * (K - 1)) (K - 1) y := by
    simpa using ((hasDerivAt_id y).mul_const (K - 1)).const_add lam
  have hv' : HasDerivAt (fun x : ℝ => lam * K + x * (K - 1)) (K - 1) y := by
    simpa using ((hasDerivAt_id y).mul_const (K - 1)).const_add (lam * K)
  have h := (hu.div (hv'.pow 3) (pow_ne_zero 3 hv.ne')).mul_const (2 * (K - 1) ^ 2 * lam)
  unfold Phi
  refine h.congr_deriv ?_
  simp only [Pi.div_apply, Pi.pow_apply]
  push_cast
  field_simp
  ring

/-- The printed derivative `(K-1) λ (3-4K)/[λK + I_k(K-1)]^4` is negative where the true one
is positive: `K = 5`, `λ = 3/10`, `I_k = 1/50`. -/
theorem phi_deriv_printed_wrong :
    (5 - 1) * (3 / 10 : ℝ) * (3 - 4 * 5) / ((3 / 10) * 5 + (1 / 50) * (5 - 1)) ^ 4 < 0 ∧
      0 < 2 * (5 - 1) ^ 3 * (3 / 10 : ℝ) * ((3 / 10) * (5 - 3) - 2 * (1 / 50) * (5 - 1)) /
        ((3 / 10) * 5 + (1 / 50) * (5 - 1)) ^ 4 := by
  norm_num

/-! ## 4. Proposition 1(ii): when investing nothing is an equilibrium -/

/-- The key inequality: a perfect square. -/
theorem zero_inv_key (K t : ℝ) :
    4 * K * (K - 1) * (2 * K + t * (K + 1)) ≤ (K + 1) ^ 2 * (K + t) ^ 2 := by
  nlinarith [sq_nonneg ((K + 1) * t - K * (K - 3))]

/-- With the others at zero, a deviation to `I ≥ 0` gains at most
`I ((K-1)(K+1)^2/(4K^3λ) - 1)` over investing nothing. -/
theorem zero_inv_gain_le (K lam I : ℝ) (hK : 1 < K) (hl : 0 < lam) (hI : 0 ≤ I) :
    pay K lam I 0 - pay K lam 0 0 ≤ I * ((K - 1) * (K + 1) ^ 2 / (4 * K ^ 3 * lam) - 1) := by
  unfold pay
  have hK0 : 0 < K := by linarith
  have hK1 : 0 < K - 1 := by linarith
  set t := I * (K - 1) / lam with ht
  have ht0 : 0 ≤ t := by positivity
  have e1 : ((lam + I) * (K - 1) - (lam + 0) * (K - 2)) / ((lam + I) * (K - 1) + (lam + 0)) =
      (1 + t) / (K + t) := by
    rw [ht]; field_simp; ring
  have e0 : ((lam + 0) * (K - 1) - (lam + 0) * (K - 2)) / ((lam + 0) * (K - 1) + (lam + 0)) =
      1 / K := by
    field_simp; ring
  rw [e1, e0]
  have hIt : I = lam * t / (K - 1) := by rw [ht]; field_simp
  have key := zero_inv_key K t
  have hKt : 0 < K + t := by linarith
  rw [hIt]
  rw [← sub_nonneg]
  have e : lam * t / (K - 1) * ((K - 1) * (K + 1) ^ 2 / (4 * K ^ 3 * lam) - 1) -
      (((1 + t) / (K + t)) ^ 2 - lam * t / (K - 1) - ((1 / K) ^ 2 - 0)) =
      t * ((K + 1) ^ 2 * (K + t) ^ 2 - 4 * K * (K - 1) * (2 * K + t * (K + 1))) /
        (4 * K ^ 3 * (K + t) ^ 2) := by
    field_simp
    ring
  rw [e]
  apply div_nonneg
  · exact mul_nonneg ht0 (by linarith)
  · positivity

/-- Sufficiency: investing nothing is a best reply to zero when `λ ≥ (K-1)(K+1)^2/(4K^3)`. -/
theorem zero_inv_sufficient (K lam I : ℝ) (hK : 1 < K) (hl : 0 < lam) (hI : 0 ≤ I)
    (hlam : (K - 1) * (K + 1) ^ 2 / (4 * K ^ 3) ≤ lam) : pay K lam I 0 ≤ pay K lam 0 0 := by
  have h := zero_inv_gain_le K lam I hK hl hI
  have hK0 : 0 < K := by linarith
  have : (K - 1) * (K + 1) ^ 2 / (4 * K ^ 3 * lam) ≤ 1 := by
    rw [div_le_one (by positivity)]
    have := (div_le_iff₀ (by positivity : (0 : ℝ) < 4 * K ^ 3)).mp hlam
    linarith
  nlinarith

/-- Necessity for `K > 3`: below the bound, investing `λ t*/(K-1)` with
`t* = K(K-3)/(K+1)` pays more than investing nothing. -/
theorem zero_inv_necessary (K lam : ℝ) (hK : 3 < K) (hl : 0 < lam)
    (hlam : lam < (K - 1) * (K + 1) ^ 2 / (4 * K ^ 3)) :
    pay K lam (lam * (K * (K - 3) / (K + 1)) / (K - 1)) 0 > pay K lam 0 0 := by
  unfold pay
  have hK0 : 0 < K := by linarith
  have hK1 : 0 < K - 1 := by linarith
  have hK3 : 0 < K - 3 := by linarith
  rw [gt_iff_lt, ← sub_pos]
  have e : (((lam + lam * (K * (K - 3) / (K + 1)) / (K - 1)) * (K - 1) - (lam + 0) * (K - 2)) /
        ((lam + lam * (K * (K - 3) / (K + 1)) / (K - 1)) * (K - 1) + (lam + 0))) ^ 2 -
        lam * (K * (K - 3) / (K + 1)) / (K - 1) -
      ((((lam + 0) * (K - 1) - (lam + 0) * (K - 2)) / ((lam + 0) * (K - 1) + (lam + 0))) ^ 2 - 0) =
      K * (K - 3) / ((K + 1) * (K - 1)) * ((K - 1) * (K + 1) ^ 2 / (4 * K ^ 3) - lam) := by
    field_simp
    ring
  rw [e]
  have : 0 < (K - 1) * (K + 1) ^ 2 / (4 * K ^ 3) - lam := by linarith
  positivity

/-- At the paper's `λ = 0.23` and `K = 7` the printed condition `λ ≥ 2(K-1)^2/K^3` holds, yet
investing `0.23` against others at zero pays `(7/13)^2 - 0.23 > 1/49`. -/
theorem prop1ii_fails_K7 :
    2 * (7 - 1) ^ 2 / (7 : ℝ) ^ 3 ≤ 23 / 100 ∧
      pay 7 (23 / 100) (23 / 100) 0 > pay 7 (23 / 100) 0 0 := by
  constructor
  · norm_num
  · unfold pay; norm_num

/-! ## 5. Proposition 1(iv) and Corollary 1 -/

/-- For `K = 3` and every `λ` with `I* = 8/27 - λ > 0` and `I* ≥ 0` feasible, investing
nothing beats the candidate: the gap is a multiple of `(8 - 27λ)^3`. -/
theorem K3_zero_beats_candidate (lam : ℝ) (hlo : 5 / 27 ≤ lam) (hhi : lam < 8 / 27) :
    pay 3 lam (8 / 27 - lam) (8 / 27 - lam) < pay 3 lam 0 (8 / 27 - lam) := by
  unfold pay
  have hD : 0 < 2 * lam + 8 / 27 := by linarith
  have r0 : ((lam + 0) * (3 - 1) - (lam + (8 / 27 - lam)) * (3 - 2)) /
      ((lam + 0) * (3 - 1) + (lam + (8 / 27 - lam))) = (2 * lam - 8 / 27) / (2 * lam + 8 / 27) := by
    congr 1 <;> ring
  have r1 : ((lam + (8 / 27 - lam)) * (3 - 1) - (lam + (8 / 27 - lam)) * (3 - 2)) /
      ((lam + (8 / 27 - lam)) * (3 - 1) + (lam + (8 / 27 - lam))) = 1 / 3 := by
    norm_num
  rw [r0, r1, ← sub_pos]
  have e : ((2 * lam - 8 / 27) / (2 * lam + 8 / 27)) ^ 2 - 0 - ((1 / 3) ^ 2 - (8 / 27 - lam)) =
      4 * (8 - 27 * lam) ^ 3 / (19683 * (2 * lam + 8 / 27) ^ 2) := by
    have := hD.ne'
    field_simp
    ring
  rw [e]
  have : 0 < 8 - 27 * lam := by linarith
  positivity

/-- The effort comparison stated in Proposition 1(iv) holds at `K = 3`, `λ = 0.23`, although
the payoff comparison fails there: `x*_k(I*, I*) ≥ x*_k(0, I*)`. -/
theorem cor1_condition_misstated :
    xK 3 (1 / (1 : ℝ)) (23 / 100 / (8 / 27)) ≤ xK 3 (23 / 100 / (8 / 27)) (23 / 100 / (8 / 27)) ∧
      pay 3 (23 / 100) (8 / 27 - 23 / 100) (8 / 27 - 23 / 100) <
        pay 3 (23 / 100) 0 (8 / 27 - 23 / 100) := by
  refine ⟨?_, K3_zero_beats_candidate _ (by norm_num) (by norm_num)⟩
  unfold xK
  norm_num

/-- For `K = 2` the candidate `I* = 1/4 - λ` beats investing nothing:
the gap is `λ (λ - 1/4)^2 / (λ + 1/4)^2`. -/
theorem K2_candidate_beats_zero (lam : ℝ) (hl : 0 < lam) (hhi : lam < 1 / 4) :
    pay 2 lam 0 (1 / 4 - lam) ≤ pay 2 lam (1 / 4 - lam) (1 / 4 - lam) := by
  unfold pay
  have hD : 0 < lam + 1 / 4 := by linarith
  have r0 : ((lam + 0) * (2 - 1) - (lam + (1 / 4 - lam)) * (2 - 2)) /
      ((lam + 0) * (2 - 1) + (lam + (1 / 4 - lam))) = lam / (lam + 1 / 4) := by
    congr 1 <;> ring
  have r1 : ((lam + (1 / 4 - lam)) * (2 - 1) - (lam + (1 / 4 - lam)) * (2 - 2)) /
      ((lam + (1 / 4 - lam)) * (2 - 1) + (lam + (1 / 4 - lam))) = 1 / 2 := by
    norm_num
  rw [r0, r1, ← sub_nonneg]
  have e : (1 / 2 : ℝ) ^ 2 - (1 / 4 - lam) - ((lam / (lam + 1 / 4)) ^ 2 - 0) =
      lam * (lam - 1 / 4) ^ 2 / (lam + 1 / 4) ^ 2 := by
    have := hD.ne'
    field_simp
    ring
  rw [e]
  positivity

end FuLu2009
