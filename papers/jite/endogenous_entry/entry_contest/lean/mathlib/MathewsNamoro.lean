import Mathlib

/-!
# Mathews and Namoro, "Participation incentives in rank order tournaments with endogenous entry"

Timothy Mathews and Soiliou Daw Namoro, *Journal of Economics* 95 (2008), 1-23; this file follows
the working paper of 29 March 2005 that was read (the page numbers are its printed pages).

Two events, two entrants each. The strategic agents are `H` (high ability), `L` (low ability); the
non-strategic `N` fill the remaining slots. A winner takes the prize `p`, a runner-up takes `α p`
with `0 ≤ α < 1`. Event 1 pays `p₁ = τ p₂` with `τ ≥ 1`. `λ = P(H beats L)`, `ω = P(H beats N)`,
`ρ = P(L beats N)`. The paper's "intuitive restrictions" (p.7) are `ω > λ > 1/2`, `ω > ρ > 1/2`.

Two payoff functions carry the whole paper, with the prize normalised to 1 (equilibrium payoffs
are proportional to the prize, `single_payoff_A`, `single_payoff_B`):

* `topPay α z q = α + (1-α) q - z (1-α) q (1-q)`: the equilibrium payoff of the player who wins with
  probability `q ≥ 1/2` in a two-player tournament,
* `botPay α z q = 1 - (1-α) q - z (1-α) q (1-q)`: that of the player facing a rival who wins with
  probability `q`.

`z ∈ [0,1]` is the elasticity of the contest success function in effort.

The sections are:

1. The single tournament (pp.5-6, Appendix A): the first-order conditions, the equal-effort
   equilibrium, and a best-reply proof (Nash equilibrium) for `0 < z ≤ 1`.
2. Condition 1 (p.9) and the backward-induction classification of the allocations a, b, c, d.
3. The cutoffs `Ψ` and `Θ` (footnotes 9 and 10): `Ψ(1) = Θ(1) = 1`, decreasing, convex.
4. The comparison `η = (Ψ - Θ)·top(λ)·bot(λ)` and Conditions 2 and 3 (Section 5.1).
5. The numerical examples (pp.12, 14), the prize cut that attracts a stronger field (Section 5),
   the three outcomes (Section 5.2).
6. The probabilities of Section 6: the first-order-dominance example, computed as integrals from
   the printed distribution functions, and the power functions (Condition 2 polynomial; `c` never
   arises).
7. The organiser's problem of Section 5.3 and Appendix B: the cheapest `b` costs `1076/279`, the
   cheapest `c` costs `(2√90440 + 618)/319`, and `c` is cheaper.

Not formalised: the printed piecewise linear programmes of Appendix B (only the two minima and
their comparison are), the claims that the power functions are ordered by the monotone hazard-rate
and likelihood-ratio conditions and that the first-order example is not (the orders are not
defined here), and Table 1's payoffs beyond the ones the backward induction of Section 2 uses.
-/

open Set

namespace MathewsNamoro2008

/-! ## The two payoff functions -/

/-- Equilibrium payoff, prize 1, of the player who wins with probability `q`. -/
noncomputable def topPay (α z q : ℝ) : ℝ := α + (1 - α) * q - z * (1 - α) * q * (1 - q)

/-- Equilibrium payoff, prize 1, of the player whose rival wins with probability `q`. -/
noncomputable def botPay (α z q : ℝ) : ℝ := 1 - (1 - α) * q - z * (1 - α) * q * (1 - q)

/-- `A q = q (1 - z (1 - q))`: `topPay α z q = α + (1 - α) * A q`. -/
noncomputable def Aq (z q : ℝ) : ℝ := q * (1 - z * (1 - q))

/-- `B q = q (1 + z (1 - q))`: `botPay α z q = 1 - (1 - α) * B q`. -/
noncomputable def Bq (z q : ℝ) : ℝ := q * (1 + z * (1 - q))

theorem topPay_eq (α z q : ℝ) : topPay α z q = α + (1 - α) * Aq z q := by
  unfold topPay Aq; ring

theorem botPay_eq (α z q : ℝ) : botPay α z q = 1 - (1 - α) * Bq z q := by
  unfold botPay Bq; ring

theorem topPay_pos {α z q : ℝ} (hα0 : 0 ≤ α) (hα1 : α ≤ 1) (hz1 : z ≤ 1)
    (hq0 : 0 < q) (hq1 : q ≤ 1) : q ^ 2 ≤ topPay α z q := by
  unfold topPay
  have h1 : 0 ≤ (1 - α) := by linarith
  have h2 : q * q ≤ q * (1 - z * (1 - q)) := by
    apply mul_le_mul_of_nonneg_left _ hq0.le
    nlinarith
  nlinarith [mul_nonneg h1 (sub_nonneg.2 h2), mul_nonneg hα0 (sub_nonneg.2 (show q ^ 2 ≤ 1 by nlinarith))]

theorem botPay_pos {α z q : ℝ} (hα0 : 0 ≤ α) (hα1 : α ≤ 1) (hz1 : z ≤ 1)
    (hq0 : 0 ≤ q) (hq1 : q ≤ 1) : (1 - q) ^ 2 ≤ botPay α z q := by
  unfold botPay
  have h1 : 0 ≤ (1 - α) := by linarith
  have h2 : q * (1 + z * (1 - q)) ≤ q * (2 - q) := by
    apply mul_le_mul_of_nonneg_left _ hq0
    nlinarith
  nlinarith [mul_nonneg h1 (sub_nonneg.2 h2), mul_nonneg hα0 (sub_nonneg.2 (show (1 - q) ^ 2 ≤ 1 by nlinarith))]

theorem one_sub_add_pos {z s : ℝ} (hz0 : 0 ≤ z) (hz1 : z ≤ 1) (hs : 0 < s) : 0 < 1 - z + z * s := by
  rcases lt_or_eq_of_le hz1 with hlt | rfl
  · nlinarith [mul_nonneg hz0 hs.le]
  · linarith

/-- `topPay` rises with `q` (for `α < 1`), so a player who beats the rival more often is paid
    more. -/
theorem topPay_strictMono {α z : ℝ} (hα1 : α < 1) (hz0 : 0 ≤ z) (hz1 : z ≤ 1) {q q' : ℝ}
    (hq : 0 ≤ q) (hqq : q < q') : topPay α z q < topPay α z q' := by
  unfold topPay
  have h1 : 0 < 1 - α := by linarith
  have : topPay α z q' - topPay α z q = (1 - α) * (q' - q) * (1 - z + z * (q + q')) := by
    unfold topPay; ring
  have h3 : 0 < 1 - z + z * (q + q') := one_sub_add_pos hz0 hz1 (by linarith)
  have := mul_pos (mul_pos h1 (sub_pos.2 hqq)) h3
  unfold topPay at *
  linarith

/-! ## 1. The single tournament (pp.5-6, Appendix A)

The contest success function of A against B with efforts `x`, `y`:
`δ x^z / ((1 - δ) y^z + δ x^z)` (p.5); it is homogeneous of degree 0, equals `δ` at equal efforts,
and with `z = 0` does not depend on effort (Appendix A: effort is then zero). Cost is `c(e) = e`.
-/

/-- A's probability of winning with effort `x` against effort `y`. -/
noncomputable def csf (δ z x y : ℝ) : ℝ := δ * x ^ z / ((1 - δ) * y ^ z + δ * x ^ z)

/-- A's expected payoff in a tournament with prize `p` and runner-up share `α`. -/
noncomputable def payA (p α δ z x y : ℝ) : ℝ := p * (α + (1 - α) * csf δ z x y) - x

/-- B's expected payoff: B wins with probability `1 - csf`. -/
noncomputable def payB (p α δ z x y : ℝ) : ℝ := p * (α + (1 - α) * (1 - csf δ z x y)) - y

theorem csf_self {δ z e : ℝ} (he : 0 < e) : csf δ z e e = δ := by
  unfold csf
  have : 0 < e ^ z := Real.rpow_pos_of_pos he z
  field_simp
  ring

theorem csf_homogeneous {δ z t x y : ℝ} (ht : 0 < t) (hx : 0 < x) (hy : 0 < y) :
    csf δ z (t * x) (t * y) = csf δ z x y := by
  unfold csf
  rw [Real.mul_rpow ht.le hx.le, Real.mul_rpow ht.le hy.le]
  have ht' : 0 < t ^ z := Real.rpow_pos_of_pos ht z
  have hx' : 0 < x ^ z := Real.rpow_pos_of_pos hx z
  have hy' : 0 < y ^ z := Real.rpow_pos_of_pos hy z
  by_cases hd : (1 - δ) * y ^ z + δ * x ^ z = 0
  · have : (1 - δ) * (t ^ z * y ^ z) + δ * (t ^ z * x ^ z) = 0 := by
      have : (1 - δ) * (t ^ z * y ^ z) + δ * (t ^ z * x ^ z) = t ^ z * ((1 - δ) * y ^ z + δ * x ^ z) := by ring
      rw [this, hd, mul_zero]
    rw [hd, this]; simp
  · have hd' : (1 - δ) * (t ^ z * y ^ z) + δ * (t ^ z * x ^ z) = t ^ z * ((1 - δ) * y ^ z + δ * x ^ z) := by ring
    rw [hd']
    field_simp

theorem csf_z_zero {δ x y : ℝ} : csf δ 0 x y = δ := by
  unfold csf
  simp only [Real.rpow_zero, mul_one]
  have : (1 - δ) + δ = 1 := by ring
  rw [this]; simp

/-- Appendix A, `z = 0`: when the odds do not depend on effort, A's payoff falls one for one with
    effort, so the unique best reply is zero effort. -/
theorem payA_z_zero {p α δ x y : ℝ} (hx : 0 < x) :
    payA p α δ 0 x y < payA p α δ 0 0 y := by
  unfold payA
  rw [csf_z_zero, csf_z_zero]
  linarith

/-! ### The derivative of the odds

`∂/∂x [δ x^z / ((1-δ) y^z + δ x^z)] = z δ (1-δ) x^(z-1) y^z / ((1-δ) y^z + δ x^z)^2`. -/

theorem csf_hasDerivAt {δ z x y : ℝ} (hx : 0 < x) (hy : 0 < y) (hδ0 : 0 ≤ δ) (hδ1 : δ ≤ 1) :
    HasDerivAt (fun u => csf δ z u y)
      (z * δ * (1 - δ) * x ^ (z - 1) * y ^ z / ((1 - δ) * y ^ z + δ * x ^ z) ^ 2) x := by
  have hxz : 0 < x ^ z := Real.rpow_pos_of_pos hx z
  have hyz : 0 < y ^ z := Real.rpow_pos_of_pos hy z
  have hd : HasDerivAt (fun u : ℝ => u ^ z) (z * x ^ (z - 1)) x :=
    Real.hasDerivAt_rpow_const (Or.inl hx.ne')
  have hnum : HasDerivAt (fun u : ℝ => δ * u ^ z) (δ * (z * x ^ (z - 1))) x := hd.const_mul δ
  have hden : HasDerivAt (fun u : ℝ => (1 - δ) * y ^ z + δ * u ^ z) (δ * (z * x ^ (z - 1))) x := by
    simpa using hnum.const_add ((1 - δ) * y ^ z)
  have hne : (1 - δ) * y ^ z + δ * x ^ z ≠ 0 := by
    have : 0 < (1 - δ) * y ^ z + δ * x ^ z := by
      rcases eq_or_lt_of_le hδ0 with h | h
      · subst h; simp; exact hyz
      · nlinarith [mul_nonneg (sub_nonneg.2 hδ1) hyz.le, mul_pos h hxz]
    exact this.ne'
  have := hnum.div hden hne
  unfold csf
  convert this using 1
  field_simp
  ring

/-- Both first-order conditions of the tournament: A's `∂payA/∂x = 0` and B's `∂payB/∂y = 0`,
    written with the closed form of the derivative. -/
noncomputable def focA (p α δ z x y : ℝ) : ℝ :=
  p * (1 - α) * (z * δ * (1 - δ) * x ^ (z - 1) * y ^ z / ((1 - δ) * y ^ z + δ * x ^ z) ^ 2) - 1

noncomputable def focB (p α δ z x y : ℝ) : ℝ :=
  p * (1 - α) * (z * (1 - δ) * δ * y ^ (z - 1) * x ^ z / (δ * x ^ z + (1 - δ) * y ^ z) ^ 2) - 1

theorem payA_hasDerivAt {p α δ z x y : ℝ} (hx : 0 < x) (hy : 0 < y) (hδ0 : 0 ≤ δ) (hδ1 : δ ≤ 1) :
    HasDerivAt (fun u => payA p α δ z u y) (focA p α δ z x y) x := by
  have h := ((csf_hasDerivAt (z := z) hx hy hδ0 hδ1).const_mul (1 - α)).const_add α
  have h2 := (h.const_mul p).sub (hasDerivAt_id x)
  unfold payA focA
  convert h2 using 1
  ring

/-- B's problem is A's with `δ` and `1 - δ` and the two efforts exchanged. -/
theorem csf_swap {δ z x y : ℝ} (hx : 0 < x) (hy : 0 < y) (hδ0 : 0 ≤ δ) (hδ1 : δ ≤ 1) :
    1 - csf δ z x y = csf (1 - δ) z y x := by
  have hxz : 0 < x ^ z := Real.rpow_pos_of_pos hx z
  have hyz : 0 < y ^ z := Real.rpow_pos_of_pos hy z
  unfold csf
  have hne : (1 - δ) * y ^ z + δ * x ^ z ≠ 0 := by
    have : 0 < (1 - δ) * y ^ z + δ * x ^ z := by
      rcases eq_or_lt_of_le hδ0 with h | h
      · subst h; simp; exact hyz
      · nlinarith [mul_nonneg (sub_nonneg.2 hδ1) hyz.le, mul_pos h hxz]
    exact this.ne'
  have hden : (1 - (1 - δ)) * x ^ z + (1 - δ) * y ^ z = (1 - δ) * y ^ z + δ * x ^ z := by ring
  rw [hden]
  field_simp
  ring

theorem payB_eq_payA_swap {p α δ z x y : ℝ} (hx : 0 < x) (hy : 0 < y) (hδ0 : 0 ≤ δ) (hδ1 : δ ≤ 1) :
    payB p α δ z x y = payA p α (1 - δ) z y x := by
  unfold payB payA
  rw [csf_swap hx hy hδ0 hδ1]

/-- **Appendix A, the equal-effort equilibrium.** Interior first-order conditions of both players
    hold iff the efforts are equal and `e* = z p (1 - α) δ (1 - δ)`. -/
theorem foc_iff {p α δ z x y : ℝ} (hx : 0 < x) (hy : 0 < y) (hδ0 : 0 < δ) (hδ1 : δ < 1)
    (hz : 0 < z) (hp : 0 < p) (hα : α < 1) :
    (focA p α δ z x y = 0 ∧ focB p α δ z x y = 0) ↔
      (x = z * p * (1 - α) * δ * (1 - δ) ∧ y = z * p * (1 - α) * δ * (1 - δ)) := by
  have hxz : 0 < x ^ z := Real.rpow_pos_of_pos hx z
  have hyz : 0 < y ^ z := Real.rpow_pos_of_pos hy z
  have hD : 0 < (1 - δ) * y ^ z + δ * x ^ z := by
    nlinarith [mul_pos (sub_pos.2 hδ1) hyz, mul_pos hδ0 hxz]
  have hD' : δ * x ^ z + (1 - δ) * y ^ z = (1 - δ) * y ^ z + δ * x ^ z := by ring
  have hxs : x ^ (z - 1) = x ^ z / x := by rw [Real.rpow_sub hx, Real.rpow_one]
  have hys : y ^ (z - 1) = y ^ z / y := by rw [Real.rpow_sub hy, Real.rpow_one]
  have hk : 0 < p * (1 - α) * z * δ * (1 - δ) := by
    have h1 : 0 < 1 - α := by linarith
    have h2 : 0 < 1 - δ := by linarith
    positivity
  have hD2 : 0 < ((1 - δ) * y ^ z + δ * x ^ z) ^ 2 := pow_pos hD 2
  have fA : focA p α δ z x y =
      (p * (1 - α) * z * δ * (1 - δ) * (x ^ z * y ^ z) - x * ((1 - δ) * y ^ z + δ * x ^ z) ^ 2)
        / (x * ((1 - δ) * y ^ z + δ * x ^ z) ^ 2) := by
    unfold focA; rw [hxs]; field_simp
  have fB : focB p α δ z x y =
      (p * (1 - α) * z * δ * (1 - δ) * (x ^ z * y ^ z) - y * ((1 - δ) * y ^ z + δ * x ^ z) ^ 2)
        / (y * ((1 - δ) * y ^ z + δ * x ^ z) ^ 2) := by
    unfold focB; rw [hys, hD']; field_simp
  rw [fA, fB, div_eq_zero_iff, div_eq_zero_iff, sub_eq_zero, sub_eq_zero]
  have hne1 : x * ((1 - δ) * y ^ z + δ * x ^ z) ^ 2 ≠ 0 := (mul_pos hx hD2).ne'
  have hne2 : y * ((1 - δ) * y ^ z + δ * x ^ z) ^ 2 ≠ 0 := (mul_pos hy hD2).ne'
  simp only [hne1, hne2, or_false]
  constructor
  · rintro ⟨e1, e2⟩
    have hxy : x = y := mul_right_cancel₀ hD2.ne' (e1.symm.trans e2)
    subst hxy
    have hden : (1 - δ) * x ^ z + δ * x ^ z = x ^ z := by ring
    rw [hden] at e1
    have h2 : 0 < x ^ z * x ^ z := mul_pos hxz hxz
    have e3 : (p * (1 - α) * z * δ * (1 - δ)) * (x ^ z * x ^ z) = x * (x ^ z * x ^ z) := by
      rw [e1]; ring
    have := mul_right_cancel₀ h2.ne' e3
    constructor <;> (rw [← this]; ring)
  · rintro ⟨h1, h2⟩
    have e : x = y := h1.trans h2.symm
    subst e
    have hden : (1 - δ) * x ^ z + δ * x ^ z = x ^ z := by ring
    rw [hden]
    constructor
    · rw [h1]; ring_nf
    · rw [h1]; ring_nf

/-- Equilibrium payoffs of the single tournament (p.5): A's payoff at `e*` is `p · topPay α z δ`
    and B's is `p · botPay α z δ`. -/
theorem single_payoff_A (p α δ z : ℝ) {e : ℝ} (he : 0 < e) (hxe : e = z * p * (1 - α) * δ * (1 - δ)) :
    payA p α δ z e e = p * topPay α z δ := by
  unfold payA topPay
  rw [csf_self he, hxe]; ring

theorem single_payoff_B (p α δ z : ℝ) {e : ℝ} (he : 0 < e) (hxe : e = z * p * (1 - α) * δ * (1 - δ)) :
    payB p α δ z e e = p * botPay α z δ := by
  unfold payB botPay
  rw [csf_self he, hxe]; ring

/-! ### The global best reply (`0 < z ≤ 1`) -/

/-- Tangent bound: against the rival's effort `e`, the odds are concave in own effort, so they lie
    below their tangent at `e`. -/
theorem csf_tangent {δ z x e : ℝ} (hδ0 : 0 < δ) (hδ1 : δ < 1) (hz0 : 0 < z) (hz1 : z ≤ 1)
    (he : 0 < e) (hx : 0 ≤ x) :
    csf δ z x e ≤ δ + z * δ * (1 - δ) * (x - e) / e := by
  have hu0 : 0 < e ^ z := Real.rpow_pos_of_pos he z
  have hu : 0 ≤ x ^ z := Real.rpow_nonneg hx z
  -- Bernoulli: (x/e)^z ≤ 1 + z (x/e - 1)
  have hs : -1 ≤ x / e - 1 := by
    have : 0 ≤ x / e := div_nonneg hx he.le
    linarith
  have hb := rpow_one_add_le_one_add_mul_self hs hz0.le hz1
  have hxe : (1 + (x / e - 1)) = x / e := by ring
  rw [hxe, Real.div_rpow hx he.le] at hb
  -- d := x^z - e^z ≤ z e^z (x - e)/e
  have hd : x ^ z - e ^ z ≤ z * e ^ z * ((x - e) / e) := by
    have h1 : x ^ z ≤ e ^ z * (1 + z * (x / e - 1)) := by
      rw [div_le_iff₀ hu0] at hb
      linarith
    have h2 : e ^ z * (1 + z * (x / e - 1)) = e ^ z + z * e ^ z * ((x - e) / e) := by
      field_simp
    linarith
  set d := x ^ z - e ^ z with hdd
  set s := (x - e) / e with hss
  have hD : 0 < (1 - δ) * e ^ z + δ * x ^ z := by
    nlinarith [mul_pos (sub_pos.2 hδ1) hu0, mul_nonneg hδ0.le hu]
  have hDe : (1 - δ) * e ^ z + δ * x ^ z = e ^ z + δ * d := by rw [hdd]; ring
  have key : csf δ z x e - δ = δ * (1 - δ) * d / ((1 - δ) * e ^ z + δ * x ^ z) := by
    unfold csf
    field_simp
    rw [hdd]; ring
  have hmain : d / ((1 - δ) * e ^ z + δ * x ^ z) ≤ z * s := by
    rw [div_le_iff₀ hD]
    have hzs : d ≤ z * e ^ z * s := hd
    rcases le_total 0 d with h0 | h0
    · -- d ≥ 0: D ≥ e^z
      have hD1 : e ^ z ≤ (1 - δ) * e ^ z + δ * x ^ z := by rw [hDe]; nlinarith
      have hzs0 : 0 ≤ z * s := by
        by_contra hneg
        push_neg at hneg
        have : z * e ^ z * s < 0 := by
          have : z * e ^ z * s = e ^ z * (z * s) := by ring
          rw [this]; exact mul_neg_of_pos_of_neg hu0 hneg
        linarith
      nlinarith [mul_le_mul_of_nonneg_left hD1 hzs0]
    · -- d ≤ 0: D ≤ e^z
      have hD1 : (1 - δ) * e ^ z + δ * x ^ z ≤ e ^ z := by rw [hDe]; nlinarith
      rcases le_total 0 (z * s) with hz | hz
      · nlinarith [mul_nonneg hz hD.le]
      · nlinarith [mul_le_mul_of_nonneg_left hD1 (neg_nonneg.2 hz)]
  have h3 : δ * (1 - δ) * d / ((1 - δ) * e ^ z + δ * x ^ z) ≤ δ * (1 - δ) * (z * s) := by
    rw [mul_div_assoc]
    exact mul_le_mul_of_nonneg_left hmain (by nlinarith)
  have : z * δ * (1 - δ) * (x - e) / e = δ * (1 - δ) * (z * s) := by rw [hss]; ring
  rw [this]
  linarith

/-- **A cannot gain by deviating** from `e* = z p (1-α) δ (1-δ)` against a rival at `e*`:
    any effort `x ≥ 0`, including zero, pays no more (`0 < z ≤ 1`). -/
theorem payA_best_reply {p α δ z x e : ℝ} (hp : 0 < p) (hα : α < 1) (hδ0 : 0 < δ) (hδ1 : δ < 1)
    (hz0 : 0 < z) (hz1 : z ≤ 1) (hxe : e = z * p * (1 - α) * δ * (1 - δ)) (hx : 0 ≤ x) :
    payA p α δ z x e ≤ payA p α δ z e e := by
  have h1 : 0 < 1 - α := by linarith
  have h2 : 0 < 1 - δ := by linarith
  have he : 0 < e := by rw [hxe]; positivity
  have hc := csf_tangent hδ0 hδ1 hz0 hz1 he hx
  have hk : p * (1 - α) * (z * δ * (1 - δ) * (x - e) / e) = x - e := by
    have : p * (1 - α) * (z * δ * (1 - δ) * (x - e) / e) = p * (1 - α) * (z * δ * (1 - δ)) * (x - e) / e := by
      field_simp
    have hk0 : p * (1 - α) * (z * δ * (1 - δ)) = e := by rw [hxe]; ring
    rw [this, hk0]
    field_simp
  have hm := mul_le_mul_of_nonneg_left hc (show 0 ≤ p * (1 - α) by positivity)
  unfold payA
  rw [csf_self he]
  nlinarith

/-- B cannot gain by deviating either (`y ≥ 0`). -/
theorem payB_best_reply {p α δ z y e : ℝ} (hp : 0 < p) (hα : α < 1) (hδ0 : 0 < δ) (hδ1 : δ < 1)
    (hz0 : 0 < z) (hz1 : z ≤ 1) (hxe : e = z * p * (1 - α) * δ * (1 - δ)) (hy : 0 ≤ y) :
    payB p α δ z e y ≤ payB p α δ z e e := by
  have h1 : 0 < 1 - α := by linarith
  have h2 : 0 < 1 - δ := by linarith
  have he : 0 < e := by rw [hxe]; positivity
  rcases eq_or_lt_of_le hy with hy0 | hy0
  · subst hy0
    have hc : csf δ z e 0 = 1 := by
      unfold csf
      rw [Real.zero_rpow hz0.ne']
      have : 0 < e ^ z := Real.rpow_pos_of_pos he z
      field_simp
      ring
    unfold payB
    rw [hc, csf_self he]
    have hb : e ≤ p * (1 - α) * (1 - δ) := by
      rw [hxe]
      have : z * δ ≤ 1 := by nlinarith
      nlinarith [mul_pos (mul_pos hp h1) h2]
    nlinarith
  · rw [payB_eq_payA_swap he hy0 hδ0.le hδ1.le, payB_eq_payA_swap he he hδ0.le hδ1.le]
    exact payA_best_reply hp hα (by linarith) (by linarith) hz0 hz1 (by rw [hxe]; ring) hy

/-- **The equal-effort pair is a Nash equilibrium** of the single tournament for `0 < z ≤ 1`. -/
theorem single_nash {p α δ z e : ℝ} (hp : 0 < p) (hα : α < 1) (hδ0 : 0 < δ) (hδ1 : δ < 1)
    (hz0 : 0 < z) (hz1 : z ≤ 1) (hxe : e = z * p * (1 - α) * δ * (1 - δ)) :
    (∀ x : ℝ, 0 ≤ x → payA p α δ z x e ≤ payA p α δ z e e) ∧
      (∀ y : ℝ, 0 ≤ y → payB p α δ z e y ≤ payB p α δ z e e) :=
  ⟨fun _ hx => payA_best_reply hp hα hδ0 hδ1 hz0 hz1 hxe hx,
    fun _ hy => payB_best_reply hp hα hδ0 hδ1 hz0 hz1 hxe hy⟩

/-! ## 2. Condition 1 and the allocations

Condition 1 (p.9) is `(ρ + λ - 1) + z [λ(1-λ) - ρ(1-ρ)] > 0`. It factors as
`(ρ + λ - 1)(1 + z(ρ - λ))`, so it holds exactly when `ρ + λ > 1` (the second factor is positive
for `ρ, λ ∈ (1/2, 1]`), for every `z ∈ [0,1]`. At `z = 1` its value is `ρ² - (1-λ)²`; the paper
prints `λ(1-λ) - [1-ρ²]` there (p.9), which is `0` nowhere near `ρ = λ = 1/2` (see
`cond1_printed_wrong`). -/

noncomputable def cond1 (z lam ρ : ℝ) : ℝ := (ρ + lam - 1) + z * (lam * (1 - lam) - ρ * (1 - ρ))

theorem cond1_factor (z lam ρ : ℝ) : cond1 z lam ρ = (ρ + lam - 1) * (1 + z * (ρ - lam)) := by
  unfold cond1; ring

theorem cond1_z_one (lam ρ : ℝ) : cond1 1 lam ρ = ρ ^ 2 - (1 - lam) ^ 2 := by
  unfold cond1; ring

theorem cond1_zero_at_half : cond1 1 (1 / 2) (1 / 2) = 0 := by
  unfold cond1; norm_num

/-- MN-D1: the printed form of Condition 1 at `z = 1` is not the value; it differs from it at
    `ρ = λ = 1/2`, where the paper says the value is zero. -/
theorem cond1_printed_wrong :
    (1 / 2 : ℝ) * (1 - 1 / 2) - (1 - (1 / 2) ^ 2) ≠ cond1 1 (1 / 2) (1 / 2) := by
  rw [cond1_zero_at_half]; norm_num

theorem cond1_pos {z lam ρ : ℝ} (hz0 : 0 ≤ z) (hz1 : z ≤ 1) (hρ : 1 / 2 < ρ) (hl : 1 / 2 < lam)
    (hl1 : lam ≤ 1) : 0 < cond1 z lam ρ := by
  rw [cond1_factor]
  apply mul_pos
  · linarith
  · nlinarith [mul_nonneg hz0 (show 0 ≤ ρ - lam + 1 / 2 by linarith)]

/-- Control: with `ρ = 0.05`, `λ = 0.9` Condition 1 fails for every `z ∈ [0,1]`. -/
theorem cond1_control {z : ℝ} (hz1 : z ≤ 1) : cond1 z (9 / 10) (1 / 20) < 0 := by
  rw [cond1_factor]
  have : 0 < 1 + z * (1 / 20 - 9 / 10) := by nlinarith
  nlinarith

theorem topPay_sub_botPay (α z lam ρ : ℝ) :
    topPay α z ρ - botPay α z lam = (1 - α) * cond1 z lam ρ := by
  unfold topPay botPay cond1; ring

/-- The four allocations of Table 1 (p.8): a `{H,L | N,N}`, b `{H,N | L,N}`, c `{L,N | H,N}`,
    d `{N,N | H,L}`. -/
inductive Alloc | a | b | c | d
  deriving DecidableEq

/-- L's reply once H is in Event 1: join (a) iff `Π^a_L = τ·bot(λ) ≥ Π^b_L = top(ρ)`. -/
noncomputable def lAfterH1 (τ α z lam ρ : ℝ) : Alloc :=
  if topPay α z ρ ≤ τ * botPay α z lam then Alloc.a else Alloc.b

/-- L's reply once H is in Event 2: Event 1 (c) iff `Π^c_L = τ·top(ρ) ≥ Π^d_L = bot(λ)`. -/
noncomputable def lAfterH2 (τ α z lam ρ : ℝ) : Alloc :=
  if botPay α z lam ≤ τ * topPay α z ρ then Alloc.c else Alloc.d

/-- H's payoff in each allocation (Table 1), prize of Event 2 normalised to 1. -/
noncomputable def hPay (τ α z lam ω : ℝ) : Alloc → ℝ
  | Alloc.a => τ * topPay α z lam
  | Alloc.b => τ * topPay α z ω
  | Alloc.c => topPay α z ω
  | Alloc.d => topPay α z lam

/-- Backward induction, ties broken toward Event 1: H enters first and picks the event whose
    resulting allocation pays more. -/
noncomputable def outcome (τ α z lam ρ ω : ℝ) : Alloc :=
  if hPay τ α z lam ω (lAfterH2 τ α z lam ρ) ≤ hPay τ α z lam ω (lAfterH1 τ α z lam ρ)
  then lAfterH1 τ α z lam ρ else lAfterH2 τ α z lam ρ

/-- The cutoffs of p.10: `Ψ = τΠ^b_L / Π^a_L` and `Θ = τΠ^c_H / Π^a_H`. -/
noncomputable def Psi (α z lam ρ : ℝ) : ℝ := topPay α z ρ / botPay α z lam

noncomputable def Theta (α z lam ω : ℝ) : ℝ := topPay α z ω / topPay α z lam

/-- The standing hypotheses of Sections 3-5: `0 ≤ α < 1`, `z ∈ [0,1]`, `λ, ρ > 1/2`, `λ < 1`,
    `ρ, ω ≤ 1`, `ω > 0`, `τ ≥ 1`. -/
structure Regime (α z lam ρ ω τ : ℝ) : Prop where
  hα0 : 0 ≤ α
  hα1 : α < 1
  hz0 : 0 ≤ z
  hz1 : z ≤ 1
  hρ : 1 / 2 < ρ
  hρ1 : ρ ≤ 1
  hl : 1 / 2 < lam
  hl1 : lam < 1
  hω : 0 < ω
  hω1 : ω ≤ 1
  hτ : 1 ≤ τ

section Alloc

variable {α z lam ρ ω τ : ℝ}

theorem Regime.top_lam_pos (h : Regime α z lam ρ ω τ) : 0 < topPay α z lam := by
  have := topPay_pos h.hα0 h.hα1.le h.hz1 (by linarith [h.hl]) h.hl1.le
  nlinarith [h.hl]

theorem Regime.top_om_pos (h : Regime α z lam ρ ω τ) : 0 < topPay α z ω := by
  have := topPay_pos h.hα0 h.hα1.le h.hz1 h.hω h.hω1
  nlinarith [h.hω]

theorem Regime.top_rho_pos (h : Regime α z lam ρ ω τ) : 0 < topPay α z ρ := by
  have := topPay_pos h.hα0 h.hα1.le h.hz1 (by linarith [h.hρ]) h.hρ1
  nlinarith [h.hρ]

theorem Regime.bot_lam_pos (h : Regime α z lam ρ ω τ) : 0 < botPay α z lam := by
  have := botPay_pos h.hα0 h.hα1.le h.hz1 (by linarith [h.hl]) h.hl1.le
  have : 0 < (1 - lam) ^ 2 := pow_pos (by linarith [h.hl1]) 2
  linarith

/-- Condition 1 in payoff form: `Π^c_L ≥ Π^d_L` for every `τ ≥ 1`, because
    `top(ρ) - bot(λ) = (1-α)·cond1`. -/
theorem Regime.top_rho_ge_bot_lam (h : Regime α z lam ρ ω τ) : botPay α z lam < topPay α z ρ := by
  have := topPay_sub_botPay α z lam ρ
  have h1 := cond1_pos h.hz0 h.hz1 h.hρ h.hl h.hl1.le
  have h2 : 0 < 1 - α := by linarith [h.hα1]
  nlinarith [mul_pos h2 h1]

theorem Regime.lAfterH2_eq (h : Regime α z lam ρ ω τ) : lAfterH2 τ α z lam ρ = Alloc.c := by
  unfold lAfterH2
  have := h.top_rho_ge_bot_lam
  have := h.top_rho_pos
  rw [if_pos]
  nlinarith [h.hτ]

theorem Regime.psi_iff (h : Regime α z lam ρ ω τ) :
    Psi α z lam ρ ≤ τ ↔ topPay α z ρ ≤ τ * botPay α z lam := by
  unfold Psi
  exact div_le_iff₀ h.bot_lam_pos

theorem Regime.theta_iff (h : Regime α z lam ρ ω τ) :
    Theta α z lam ω ≤ τ ↔ topPay α z ω ≤ τ * topPay α z lam := by
  unfold Theta
  exact div_le_iff₀ h.top_lam_pos

/-- **The allocation, Table 1 and p.10.** Allocation `b` arises iff `τ < Ψ`. -/
theorem outcome_b_iff (h : Regime α z lam ρ ω τ) :
    outcome τ α z lam ρ ω = Alloc.b ↔ τ < Psi α z lam ρ := by
  have hpsi := h.psi_iff
  have h2 := h.lAfterH2_eq
  have htop := h.top_om_pos
  unfold outcome
  rw [h2]
  by_cases hj : topPay α z ρ ≤ τ * botPay α z lam
  · have h1 : lAfterH1 τ α z lam ρ = Alloc.a := by unfold lAfterH1; rw [if_pos hj]
    rw [h1]
    by_cases hc : hPay τ α z lam ω Alloc.c ≤ hPay τ α z lam ω Alloc.a
    · rw [if_pos hc]
      constructor
      · intro hh; cases hh
      · intro hlt; exact absurd (hpsi.2 hj) (not_le.2 hlt)
    · rw [if_neg hc]
      constructor
      · intro hh; cases hh
      · intro hlt; exact absurd (hpsi.2 hj) (not_le.2 hlt)
  · have h1 : lAfterH1 τ α z lam ρ = Alloc.b := by unfold lAfterH1; rw [if_neg hj]
    rw [h1]
    have hc : hPay τ α z lam ω Alloc.c ≤ hPay τ α z lam ω Alloc.b := by
      unfold hPay; nlinarith [h.hτ]
    rw [if_pos hc]
    constructor
    · intro _; exact not_le.1 (fun hh => hj (hpsi.1 hh))
    · intro _; rfl

/-- Allocation `a` arises iff `τ ≥ max(Ψ, Θ)`. -/
theorem outcome_a_iff (h : Regime α z lam ρ ω τ) :
    outcome τ α z lam ρ ω = Alloc.a ↔ Psi α z lam ρ ≤ τ ∧ Theta α z lam ω ≤ τ := by
  have hpsi := h.psi_iff
  have hth := h.theta_iff
  have h2 := h.lAfterH2_eq
  unfold outcome
  rw [h2]
  by_cases hj : topPay α z ρ ≤ τ * botPay α z lam
  · have h1 : lAfterH1 τ α z lam ρ = Alloc.a := by unfold lAfterH1; rw [if_pos hj]
    rw [h1]
    by_cases hc : hPay τ α z lam ω Alloc.c ≤ hPay τ α z lam ω Alloc.a
    · rw [if_pos hc]
      have : topPay α z ω ≤ τ * topPay α z lam := hc
      exact ⟨fun _ => ⟨hpsi.2 hj, hth.2 this⟩, fun _ => rfl⟩
    · rw [if_neg hc]
      have : ¬ topPay α z ω ≤ τ * topPay α z lam := hc
      exact ⟨fun hh => (by cases hh), fun hh => absurd (hth.1 hh.2) this⟩
  · have h1 : lAfterH1 τ α z lam ρ = Alloc.b := by unfold lAfterH1; rw [if_neg hj]
    rw [h1]
    have hc : hPay τ α z lam ω Alloc.c ≤ hPay τ α z lam ω Alloc.b := by
      unfold hPay; nlinarith [h.hτ, h.top_om_pos]
    rw [if_pos hc]
    exact ⟨fun hh => (by cases hh), fun hh => absurd (hpsi.1 hh.1) hj⟩

/-- Allocation `c` arises iff `Ψ ≤ τ < Θ`. -/
theorem outcome_c_iff (h : Regime α z lam ρ ω τ) :
    outcome τ α z lam ρ ω = Alloc.c ↔ Psi α z lam ρ ≤ τ ∧ τ < Theta α z lam ω := by
  have hpsi := h.psi_iff
  have hth := h.theta_iff
  have h2 := h.lAfterH2_eq
  unfold outcome
  rw [h2]
  by_cases hj : topPay α z ρ ≤ τ * botPay α z lam
  · have h1 : lAfterH1 τ α z lam ρ = Alloc.a := by unfold lAfterH1; rw [if_pos hj]
    rw [h1]
    by_cases hc : hPay τ α z lam ω Alloc.c ≤ hPay τ α z lam ω Alloc.a
    · rw [if_pos hc]
      have : topPay α z ω ≤ τ * topPay α z lam := hc
      exact ⟨fun hh => (by cases hh), fun hh => absurd (hth.2 this) (not_le.2 hh.2)⟩
    · rw [if_neg hc]
      have : ¬ topPay α z ω ≤ τ * topPay α z lam := hc
      exact ⟨fun _ => ⟨hpsi.2 hj, not_le.1 (fun hh => this (hth.1 hh))⟩, fun _ => rfl⟩
  · have h1 : lAfterH1 τ α z lam ρ = Alloc.b := by unfold lAfterH1; rw [if_neg hj]
    rw [h1]
    have hc : hPay τ α z lam ω Alloc.c ≤ hPay τ α z lam ω Alloc.b := by
      unfold hPay; nlinarith [h.hτ, h.top_om_pos]
    rw [if_pos hc]
    exact ⟨fun hh => (by cases hh), fun hh => absurd (hpsi.1 hh.1) hj⟩

/-- **Observation 1: allocation `d` never occurs.** -/
theorem outcome_ne_d (h : Regime α z lam ρ ω τ) : outcome τ α z lam ρ ω ≠ Alloc.d := by
  have h2 := h.lAfterH2_eq
  unfold outcome lAfterH1
  rw [h2]
  split_ifs <;> simp

end Alloc

/-! ## 3. The cutoffs Ψ and Θ (footnotes 9 and 10)

Writing `a = A ρ`, `b = B λ`: `Ψ = (α + (1-α) a) / (1 - (1-α) b)`, a Möbius function of `α`, with
`Ψ(1) = 1`, `Ψ - 1 = (1-α)·cond1/bot(λ)`, `Ψ' = -cond1/bot(λ)²`, `Ψ'' = 2·cond1·B λ/bot(λ)³`.
Likewise `Θ' = -(A ω - A λ)/top(λ)²`. The paper states these signs without proof. -/

theorem Aq_sub (z q q' : ℝ) : Aq z q' - Aq z q = (q' - q) * (1 - z + z * (q + q')) := by
  unfold Aq; ring

theorem Aq_pos {z q : ℝ} (hz1 : z ≤ 1) (hq : 0 < q) (hq1 : q ≤ 1) : 0 < Aq z q := by
  unfold Aq
  apply mul_pos hq
  nlinarith

theorem Aq_lt_one {z q : ℝ} (hz0 : 0 ≤ z) (hq1 : q < 1) (hq : 0 < q) : Aq z q < 1 := by
  unfold Aq
  nlinarith [mul_nonneg hz0 (sub_nonneg.2 hq1.le)]

theorem Bq_pos {z q : ℝ} (hz0 : 0 ≤ z) (hq : 0 < q) (hq1 : q ≤ 1) : 0 < Bq z q := by
  unfold Bq
  apply mul_pos hq
  nlinarith [mul_nonneg hz0 (sub_nonneg.2 hq1)]

theorem cond1_eq_A_B (z lam ρ : ℝ) : cond1 z lam ρ = Aq z ρ + Bq z lam - 1 := by
  unfold cond1 Aq Bq; ring

theorem Psi_one (z lam ρ : ℝ) : Psi 1 z lam ρ = 1 := by
  unfold Psi topPay botPay; norm_num

theorem Theta_one (z lam ω : ℝ) : Theta 1 z lam ω = 1 := by
  unfold Theta topPay; norm_num

section Cutoffs

variable {α z lam ρ ω τ : ℝ}

/-- `Ψ(α) > 1` for `α < 1` (footnote 9: `Ψ(0) > 1`; the paper does not prove it). -/
theorem Regime.psi_sub_one (h : Regime α z lam ρ ω τ) :
    Psi α z lam ρ - 1 = (1 - α) * cond1 z lam ρ / botPay α z lam := by
  unfold Psi
  rw [div_sub_one h.bot_lam_pos.ne', topPay_sub_botPay]

theorem Regime.one_lt_psi (h : Regime α z lam ρ ω τ) : 1 < Psi α z lam ρ := by
  have h1 : 0 < 1 - α := by linarith [h.hα1]
  have h2 := cond1_pos h.hz0 h.hz1 h.hρ h.hl h.hl1.le
  have := h.bot_lam_pos
  have : 0 < (1 - α) * cond1 z lam ρ / botPay α z lam := by positivity
  linarith [h.psi_sub_one]

theorem Regime.theta_sub_one (h : Regime α z lam ρ ω τ) :
    Theta α z lam ω - 1 = (1 - α) * (Aq z ω - Aq z lam) / topPay α z lam := by
  unfold Theta
  rw [div_sub_one h.top_lam_pos.ne', topPay_eq, topPay_eq]
  ring

theorem Regime.one_lt_theta (h : Regime α z lam ρ ω τ) (hωl : lam < ω) : 1 < Theta α z lam ω := by
  have h1 : 0 < 1 - α := by linarith [h.hα1]
  have h2 : 0 < Aq z ω - Aq z lam := by
    rw [Aq_sub]
    exact mul_pos (by linarith) (one_sub_add_pos h.hz0 h.hz1 (by linarith [h.hω, h.hl]))
  have := h.top_lam_pos
  have : 0 < (1 - α) * (Aq z ω - Aq z lam) / topPay α z lam := by positivity
  linarith [h.theta_sub_one]

end Cutoffs

theorem topPay_hasDerivAt (z q α : ℝ) : HasDerivAt (fun a => topPay a z q) (1 - Aq z q) α := by
  have h := (hasDerivAt_id α).add (((hasDerivAt_id α).const_sub 1).mul_const (Aq z q))
  have e : (fun a => topPay a z q) = fun a => id a + (1 - id a) * Aq z q := by
    funext a; exact topPay_eq a z q
  rw [e]
  convert h using 1
  ring

theorem botPay_hasDerivAt (z q α : ℝ) : HasDerivAt (fun a => botPay a z q) (Bq z q) α := by
  have h := (hasDerivAt_const α (1 : ℝ)).sub (((hasDerivAt_id α).const_sub 1).mul_const (Bq z q))
  have e : (fun a => botPay a z q) = fun a => 1 - (1 - id a) * Bq z q := by
    funext a; exact botPay_eq a z q
  rw [e]
  convert h using 1
  ring

/-- `Ψ' = -cond1 / bot(λ)²`. -/
theorem Psi_hasDerivAt (z lam ρ α : ℝ) (hb : botPay α z lam ≠ 0) :
    HasDerivAt (fun a => Psi a z lam ρ) (-(cond1 z lam ρ) / (botPay α z lam) ^ 2) α := by
  have h := (topPay_hasDerivAt z ρ α).div (botPay_hasDerivAt z lam α) hb
  unfold Psi
  convert h using 1
  rw [topPay_eq, botPay_eq, cond1_eq_A_B]
  congr 1
  ring

/-- `Ψ'' = 2 cond1 · B λ / bot(λ)³`. -/
theorem Psi_deriv_hasDerivAt (z lam ρ α : ℝ) (hb : botPay α z lam ≠ 0) :
    HasDerivAt (fun a => -(cond1 z lam ρ) / (botPay a z lam) ^ 2)
      (2 * cond1 z lam ρ * Bq z lam / (botPay α z lam) ^ 3) α := by
  have h := (hasDerivAt_const α (-(cond1 z lam ρ))).div ((botPay_hasDerivAt z lam α).pow 2)
    (pow_ne_zero 2 hb)
  convert h using 1
  simp only [Pi.pow_apply]
  norm_num
  field_simp

/-- `Θ' = -(A ω - A λ) / top(λ)²`. -/
theorem Theta_hasDerivAt (z lam ω α : ℝ) (ht : topPay α z lam ≠ 0) :
    HasDerivAt (fun a => Theta a z lam ω) (-(Aq z ω - Aq z lam) / (topPay α z lam) ^ 2) α := by
  have h := (topPay_hasDerivAt z ω α).div (topPay_hasDerivAt z lam α) ht
  unfold Theta
  convert h using 1
  have e1 : topPay α z lam = α + (1 - α) * Aq z lam := topPay_eq α z lam
  have e2 : topPay α z ω = α + (1 - α) * Aq z ω := topPay_eq α z ω
  rw [e1] at ht ⊢
  rw [e2]
  field_simp
  ring

/-- `Θ'' = 2 (A ω - A λ)(1 - A λ) / top(λ)³`. -/
theorem Theta_deriv_hasDerivAt (z lam ω α : ℝ) (ht : topPay α z lam ≠ 0) :
    HasDerivAt (fun a => -(Aq z ω - Aq z lam) / (topPay a z lam) ^ 2)
      (2 * (Aq z ω - Aq z lam) * (1 - Aq z lam) / (topPay α z lam) ^ 3) α := by
  have h := (hasDerivAt_const α (-(Aq z ω - Aq z lam))).div
    ((topPay_hasDerivAt z lam α).pow 2) (pow_ne_zero 2 ht)
  convert h using 1
  simp only [Pi.pow_apply]
  norm_num
  field_simp
  ring

section CutoffSigns

variable {α z lam ρ ω τ : ℝ}

/-- Footnote 9: `Ψ(0) > 1`, `Ψ(1) = 1`, `Ψ' < 0`, `Ψ'' > 0`. -/
theorem Regime.psi_deriv_neg (h : Regime α z lam ρ ω τ) :
    -(cond1 z lam ρ) / (botPay α z lam) ^ 2 < 0 := by
  have h2 := cond1_pos h.hz0 h.hz1 h.hρ h.hl h.hl1.le
  have := h.bot_lam_pos
  have : 0 < cond1 z lam ρ / (botPay α z lam) ^ 2 := by positivity
  rw [neg_div]; linarith

theorem Regime.psi_deriv2_pos (h : Regime α z lam ρ ω τ) :
    0 < 2 * cond1 z lam ρ * Bq z lam / (botPay α z lam) ^ 3 := by
  have h2 := cond1_pos h.hz0 h.hz1 h.hρ h.hl h.hl1.le
  have h3 := Bq_pos h.hz0 (show 0 < lam by linarith [h.hl]) h.hl1.le
  have := h.bot_lam_pos
  positivity

/-- Footnote 10: `Θ' < 0` and `Θ'' > 0` (for `ω > λ`). -/
theorem Regime.theta_deriv_neg (h : Regime α z lam ρ ω τ) (hωl : lam < ω) :
    -(Aq z ω - Aq z lam) / (topPay α z lam) ^ 2 < 0 := by
  have h2 : 0 < Aq z ω - Aq z lam := by
    rw [Aq_sub]
    exact mul_pos (by linarith) (one_sub_add_pos h.hz0 h.hz1 (by linarith [h.hω, h.hl]))
  have := h.top_lam_pos
  have : 0 < (Aq z ω - Aq z lam) / (topPay α z lam) ^ 2 := by positivity
  rw [neg_div]; linarith

theorem Regime.theta_deriv2_pos (h : Regime α z lam ρ ω τ) (hωl : lam < ω) :
    0 < 2 * (Aq z ω - Aq z lam) * (1 - Aq z lam) / (topPay α z lam) ^ 3 := by
  have h2 : 0 < Aq z ω - Aq z lam := by
    rw [Aq_sub]
    exact mul_pos (by linarith) (one_sub_add_pos h.hz0 h.hz1 (by linarith [h.hω, h.hl]))
  have h3 : 0 < 1 - Aq z lam := by
    have := Aq_lt_one h.hz0 h.hl1 (show 0 < lam by linarith [h.hl])
    linarith
  have := h.top_lam_pos
  positivity

end CutoffSigns

/-! ## 4. Section 5.1: when does H enter Event 2?

`η(α) = [Ψ(α) - Θ(α)]·top(λ)·bot(λ)` (p.14) is the quadratic `α² ηa - α ηb + ηc` with the
coefficients printed on p.14. Allocation `c` can arise at `α` exactly when `η(α) < 0`. -/

noncomputable def etaA (z lam ρ ω : ℝ) : ℝ :=
  (1 - ρ) * (1 + z * ρ) * (1 - lam) * (1 + z * lam)
    - (1 - ω) * (1 + z * ω) * lam * (1 + z * (1 - lam))

noncomputable def etaB (z lam ρ ω : ℝ) : ℝ :=
  (1 - ω) * (1 + z * ω) * (1 - lam) * (1 - z * lam)
    + ω * (1 - z * (1 - ω)) * lam * (1 + z * (1 - lam))
    - (1 - ρ) * (1 + z * ρ) * lam * (1 - z * (1 - lam))
    - ρ * (1 - z * (1 - ρ)) * (1 - lam) * (1 + z * lam)

noncomputable def etaC (z lam ρ ω : ℝ) : ℝ :=
  ρ * (1 - z * (1 - ρ)) * lam * (1 - z * (1 - lam))
    - ω * (1 - z * (1 - ω)) * (1 - lam) * (1 - z * lam)

noncomputable def eta (α z lam ρ ω : ℝ) : ℝ :=
  α ^ 2 * etaA z lam ρ ω - α * etaB z lam ρ ω + etaC z lam ρ ω

/-- The printed quadratic equals `top(ρ) top(λ) - top(ω) bot(λ)`. -/
theorem eta_eq (α z lam ρ ω : ℝ) :
    eta α z lam ρ ω = topPay α z ρ * topPay α z lam - topPay α z ω * botPay α z lam := by
  unfold eta etaA etaB etaC topPay botPay; ring

theorem eta_mul {α z lam ρ ω τ : ℝ} (h : Regime α z lam ρ ω τ) :
    eta α z lam ρ ω = (Psi α z lam ρ - Theta α z lam ω) * (topPay α z lam * botPay α z lam) := by
  rw [eta_eq]
  unfold Psi Theta
  have h1 := h.top_lam_pos.ne'
  have h2 := h.bot_lam_pos.ne'
  field_simp

theorem eta_zero (z lam ρ ω : ℝ) : eta 0 z lam ρ ω = etaC z lam ρ ω := by
  unfold eta; ring

theorem eta_one (z lam ρ ω : ℝ) : eta 1 z lam ρ ω = 0 := by
  unfold eta etaA etaB etaC; ring

theorem etaB_eq (z lam ρ ω : ℝ) : etaB z lam ρ ω = etaA z lam ρ ω + etaC z lam ρ ω := by
  unfold etaA etaB etaC; ring

theorem eta_factor (α z lam ρ ω : ℝ) :
    eta α z lam ρ ω = (α - 1) * (etaA z lam ρ ω * α - etaC z lam ρ ω) := by
  unfold eta; rw [etaB_eq]; ring

/-- `η'(1) = 2 ηa - ηb`, in closed form. -/
theorem etaPrime_eq (z lam ρ ω : ℝ) :
    2 * etaA z lam ρ ω - etaB z lam ρ ω
      = (ω - ρ + 1 - 2 * lam) + z * (ω - ρ) * (ω + ρ - 1) := by
  unfold etaA etaB; ring

theorem etaPrime_z_zero (lam ρ ω : ℝ) :
    2 * etaA 0 lam ρ ω - etaB 0 lam ρ ω = ω - ρ + 1 - 2 * lam := by
  rw [etaPrime_eq]; ring

theorem etaPrime_z_one (lam ρ ω : ℝ) :
    2 * etaA 1 lam ρ ω - etaB 1 lam ρ ω = ω ^ 2 - ρ ^ 2 + 1 - 2 * lam := by
  rw [etaPrime_eq]; ring

theorem etaC_z_one (lam ρ ω : ℝ) : etaC 1 lam ρ ω = ρ ^ 2 * lam ^ 2 - ω ^ 2 * (1 - lam) ^ 2 := by
  unfold etaC; ring

/-- The paper's `∂η'(1)/∂z = ρ(1-ρ) - ω(1-ω)` (p.15). -/
theorem etaPrime_dz (z z' lam ρ ω : ℝ) :
    (2 * etaA z' lam ρ ω - etaB z' lam ρ ω) - (2 * etaA z lam ρ ω - etaB z lam ρ ω)
      = (z' - z) * (ρ * (1 - ρ) - ω * (1 - ω)) := by
  rw [etaPrime_eq, etaPrime_eq]; ring

/-- A quadratic `(α-1)(Aα - C)` is non-negative on `[0,1)` iff `C ≥ 0` and `A - C ≤ 0`
    (the paper's "if and only if `η(0) ≥ 0` and `η'(1) ≤ 0`", p.14). -/
theorem quad_nonneg_iff (A C : ℝ) :
    (∀ α : ℝ, 0 ≤ α → α < 1 → 0 ≤ (α - 1) * (A * α - C)) ↔ 0 ≤ C ∧ A - C ≤ 0 := by
  constructor
  · intro h
    have h0 := h 0 le_rfl zero_lt_one
    have hC : 0 ≤ C := by simpa using h0
    refine ⟨hC, ?_⟩
    by_contra hd
    push_neg at hd
    have hApos : 0 < A := by linarith
    set ε := (A - C) / (2 * A) with hε
    have hε0 : 0 < ε := div_pos (by linarith) (by linarith)
    have hε1 : ε < 1 := by rw [hε, div_lt_one (by linarith)]; linarith
    have hεA : ε * (2 * A) = A - C := by rw [hε]; field_simp
    have := h (1 - ε) (by linarith) (by linarith)
    have hg : A * (1 - ε) - C = (A - C) / 2 := by linarith [hεA]
    rw [hg] at this
    nlinarith
  · rintro ⟨hC, hd⟩ α hα0 hα1
    have h1 : 0 ≤ 1 - α := by linarith
    nlinarith [mul_nonneg (mul_nonneg h1 h1) hC, mul_nonneg (mul_nonneg h1 hα0) (neg_nonneg.2 hd)]

/-- Outcome (iii) of Section 5.2: if `C < 0 < A - C`... more generally `C < 0` and `C < A`. -/
theorem quad_neg (A C : ℝ) (hC : C < 0) (hd : C < A) :
    ∀ α : ℝ, 0 ≤ α → α < 1 → (α - 1) * (A * α - C) < 0 := by
  intro α hα0 hα1
  have h1 : 0 < 1 - α := by linarith
  have : 0 < A * α - C := by nlinarith [mul_nonneg hα0 (sub_nonneg.2 hd.le), mul_pos h1 (neg_pos.2 hC)]
  nlinarith

/-- Outcome (ii): with `0 ≤ C < A` the cut-off is `α̂ = C / A ∈ [0,1)`, and it is unique. -/
theorem quad_cutoff (A C : ℝ) (hC : 0 ≤ C) (hd : C < A) :
    ∃! a : ℝ, 0 ≤ a ∧ a < 1 ∧
      ∀ α : ℝ, 0 ≤ α → α < 1 → (0 ≤ (α - 1) * (A * α - C) ↔ α ≤ a) := by
  have hA : 0 < A := by linarith
  have key : ∀ α : ℝ, α < 1 → (0 ≤ (α - 1) * (A * α - C) ↔ α ≤ C / A) := by
    intro α hα1
    have h1 : α - 1 < 0 := by linarith
    rw [le_div_iff₀ hA]
    constructor
    · intro h
      by_contra hc
      push_neg at hc
      nlinarith
    · intro h
      nlinarith
  refine ⟨C / A, ⟨div_nonneg hC hA.le, (div_lt_one hA).2 hd, fun α _ hα1 => key α hα1⟩, ?_⟩
  rintro a ⟨ha0, ha1, ha⟩
  by_contra hne
  rcases lt_or_gt_of_ne hne with hlt | hgt
  · have h1 := (ha (C / A) (div_nonneg hC hA.le) ((div_lt_one hA).2 hd)).1
      ((key (C / A) ((div_lt_one hA).2 hd)).2 le_rfl)
    linarith
  · have h1 := (key a ha1).1 ((ha a ha0 ha1).2 le_rfl)
    linarith

theorem eta_nonneg_iff (z lam ρ ω : ℝ) :
    (∀ α : ℝ, 0 ≤ α → α < 1 → 0 ≤ eta α z lam ρ ω) ↔
      0 ≤ etaC z lam ρ ω ∧ 2 * etaA z lam ρ ω - etaB z lam ρ ω ≤ 0 := by
  have e : ∀ α, eta α z lam ρ ω = (α - 1) * (etaA z lam ρ ω * α - etaC z lam ρ ω) :=
    fun α => eta_factor α z lam ρ ω
  simp only [e]
  rw [quad_nonneg_iff, etaB_eq]
  constructor <;> rintro ⟨h1, h2⟩ <;> exact ⟨h1, by linarith⟩

/-! ### Conditions 2 and 3 -/

/-- Condition 3 (p.15): `η'(1) ≤ 0` for some `z ∈ [0,1]` iff `λ ≥ (ω - ρ + 1)/2`. -/
theorem condition3_iff {lam ρ ω : ℝ} (hρω : ρ ≤ ω) (hρ : 1 / 2 < ρ) :
    (∃ z : ℝ, 0 ≤ z ∧ z ≤ 1 ∧ 2 * etaA z lam ρ ω - etaB z lam ρ ω ≤ 0) ↔
      (ω - ρ + 1) / 2 ≤ lam := by
  have hp : 0 ≤ (ω - ρ) * (ω + ρ - 1) := mul_nonneg (by linarith) (by linarith)
  constructor
  · rintro ⟨z, hz0, hz1, h⟩
    rw [etaPrime_eq] at h
    nlinarith [mul_nonneg hz0 hp]
  · intro h
    refine ⟨0, le_rfl, zero_le_one, ?_⟩
    rw [etaPrime_z_zero]; linarith

/-- Condition 2 (p.15): `η'(1) ≤ 0` for every `z ∈ [0,1]` iff `λ ≥ (ω² - ρ² + 1)/2`. -/
theorem condition2_iff {lam ρ ω : ℝ} (hρω : ρ ≤ ω) (hρ : 1 / 2 < ρ) :
    (∀ z : ℝ, 0 ≤ z → z ≤ 1 → 2 * etaA z lam ρ ω - etaB z lam ρ ω ≤ 0) ↔
      (ω ^ 2 - ρ ^ 2 + 1) / 2 ≤ lam := by
  have hp : 0 ≤ (ω - ρ) * (ω + ρ - 1) := mul_nonneg (by linarith) (by linarith)
  constructor
  · intro h
    have := h 1 zero_le_one le_rfl
    rw [etaPrime_z_one] at this; linarith
  · intro h z hz0 hz1
    rw [etaPrime_eq]
    nlinarith [mul_nonneg (sub_nonneg.2 hz1) hp]

/-- Condition 2 as the paper restates it: `Pr(LbN)² - Pr(LbH) ≥ Pr(HbN)² - Pr(HbL)`. -/
theorem condition2_restated (lam ρ ω : ℝ) :
    (ω ^ 2 - ρ ^ 2 + 1) / 2 ≤ lam ↔ ω ^ 2 - lam ≤ ρ ^ 2 - (1 - lam) := by
  constructor <;> intro h <;> linarith

/-- Condition 3 as the paper restates it: `Pr(LbN) - Pr(LbH) ≥ Pr(HbN) - Pr(HbL)`. -/
theorem condition3_restated (lam ρ ω : ℝ) :
    (ω - ρ + 1) / 2 ≤ lam ↔ ω - lam ≤ ρ - (1 - lam) := by
  constructor <;> intro h <;> linarith

/-- Condition 2 implies Condition 3. -/
theorem condition2_imp_condition3 {lam ρ ω : ℝ} (hρω : ρ ≤ ω) (hρ : 1 / 2 < ρ)
    (h : (ω ^ 2 - ρ ^ 2 + 1) / 2 ≤ lam) : (ω - ρ + 1) / 2 ≤ lam := by
  have : 0 ≤ (ω - ρ) * (ω + ρ - 1) := mul_nonneg (by linarith) (by linarith)
  nlinarith

/-- **The step the paper argues on p.15-16:** if `η'(1) ≤ 0` then `η(0) = ηc ≥ 0`. At
    `λ₀ = (ω - ρ + 1)/2`, `4 ηc = (ω-ρ)(ω+ρ-1)[(1-z)(2-z) + z(2ρ + (ω-ρ) - (ω-ρ)² z)] ≥ 0`, and
    `ηc` is non-decreasing in `λ`. -/
theorem etaC_at_lam0 (z ρ ω : ℝ) :
    4 * etaC z ((ω - ρ + 1) / 2) ρ ω
      = (ω - ρ) * (ω + ρ - 1)
          * ((1 - z) * (2 - z) + z * (2 * ρ + (ω - ρ) - (ω - ρ) ^ 2 * z)) := by
  unfold etaC; ring

theorem etaC_at_lam0_nonneg {z ρ ω : ℝ} (hz0 : 0 ≤ z) (hz1 : z ≤ 1) (hρ : 1 / 2 < ρ)
    (hρω : ρ ≤ ω) (hω1 : ω ≤ 1) : 0 ≤ etaC z ((ω - ρ + 1) / 2) ρ ω := by
  have h1 : 0 ≤ (ω - ρ) * (ω + ρ - 1) := mul_nonneg (by linarith) (by linarith)
  have h2 : 0 ≤ (1 - z) * (2 - z) := mul_nonneg (by linarith) (by linarith)
  have hd1 : ω - ρ ≤ 1 := by linarith
  have h3 : 0 ≤ 2 * ρ + (ω - ρ) - (ω - ρ) ^ 2 * z := by
    nlinarith [mul_nonneg (sub_nonneg.2 hz1) (sq_nonneg (ω - ρ)), sq_nonneg (ω - ρ)]
  have h4 : 0 ≤ (1 - z) * (2 - z) + z * (2 * ρ + (ω - ρ) - (ω - ρ) ^ 2 * z) :=
    add_nonneg h2 (mul_nonneg hz0 h3)
  have := mul_nonneg h1 h4
  rw [← etaC_at_lam0] at this
  linarith

theorem etaC_mono {z ρ ω l0 l : ℝ} (hz0 : 0 ≤ z) (hz1 : z ≤ 1) (hρ : 0 < ρ) (hρ1 : ρ ≤ 1)
    (hω : 0 < ω) (hω1 : ω ≤ 1) (hl0 : 0 ≤ l0) (hll : l0 ≤ l) (hl1 : l ≤ 1) :
    etaC z l0 ρ ω ≤ etaC z l ρ ω := by
  have e : etaC z l ρ ω - etaC z l0 ρ ω
      = (l - l0) * (Aq z ρ * (1 - z + z * (l + l0)) + Aq z ω * (1 + z - z * (l + l0))) := by
    unfold etaC Aq; ring
  have a1 : 0 < Aq z ρ := Aq_pos hz1 hρ hρ1
  have a2 : 0 < Aq z ω := Aq_pos hz1 hω hω1
  have b1 : 0 ≤ 1 - z + z * (l + l0) := by nlinarith [mul_nonneg hz0 (add_nonneg (hl0.trans hll) hl0)]
  have b2 : 0 ≤ 1 + z - z * (l + l0) := by nlinarith [mul_nonneg hz0 (show 0 ≤ 2 - (l + l0) by linarith)]
  have : 0 ≤ (l - l0) * (Aq z ρ * (1 - z + z * (l + l0)) + Aq z ω * (1 + z - z * (l + l0))) :=
    mul_nonneg (by linarith) (add_nonneg (mul_nonneg a1.le b1) (mul_nonneg a2.le b2))
  linarith

/-- **`η'(1) ≤ 0` implies `η(0) ≥ 0`** (p.15-16), for `1/2 < ρ ≤ ω ≤ 1`, `λ ≤ 1`. -/
theorem etaC_nonneg_of_etaPrime {z lam ρ ω : ℝ} (hz0 : 0 ≤ z) (hz1 : z ≤ 1) (hρ : 1 / 2 < ρ)
    (hρω : ρ ≤ ω) (hω1 : ω ≤ 1) (hl1 : lam ≤ 1)
    (h : 2 * etaA z lam ρ ω - etaB z lam ρ ω ≤ 0) : 0 ≤ etaC z lam ρ ω := by
  have hp : 0 ≤ (ω - ρ) * (ω + ρ - 1) := mul_nonneg (by linarith) (by linarith)
  have hl : (ω - ρ + 1) / 2 ≤ lam := by
    rw [etaPrime_eq] at h
    nlinarith [mul_nonneg hz0 hp]
  have h0 : 0 ≤ etaC z ((ω - ρ + 1) / 2) ρ ω := etaC_at_lam0_nonneg hz0 hz1 hρ hρω hω1
  have hm := etaC_mono hz0 hz1 (show 0 < ρ by linarith) (hρω.trans hω1) (show 0 < ω by linarith) hω1
    (show 0 ≤ (ω - ρ + 1) / 2 by linarith) hl hl1
  linarith

/-- Under the paper's restrictions `η ≥ 0` on `[0,1)` iff `η'(1) ≤ 0`. -/
theorem eta_nonneg_iff_etaPrime {z lam ρ ω : ℝ} (hz0 : 0 ≤ z) (hz1 : z ≤ 1) (hρ : 1 / 2 < ρ)
    (hρω : ρ ≤ ω) (hω1 : ω ≤ 1) (hl1 : lam ≤ 1) :
    (∀ α : ℝ, 0 ≤ α → α < 1 → 0 ≤ eta α z lam ρ ω) ↔
      2 * etaA z lam ρ ω - etaB z lam ρ ω ≤ 0 := by
  rw [eta_nonneg_iff]
  exact ⟨fun h => h.2, fun h => ⟨etaC_nonneg_of_etaPrime hz0 hz1 hρ hρω hω1 hl1 h, h⟩⟩

section COccurs

variable {α z lam ρ ω τ : ℝ}

theorem Regime.withTau (h : Regime α z lam ρ ω τ) {τ' : ℝ} (hτ' : 1 ≤ τ') :
    Regime α z lam ρ ω τ' := { h with hτ := hτ' }

/-- Allocation `c` arises at some prize ratio `τ ≥ 1` iff `Ψ(α) < Θ(α)`. -/
theorem c_arises_iff (h : Regime α z lam ρ ω 1) :
    (∃ τ : ℝ, 1 ≤ τ ∧ outcome τ α z lam ρ ω = Alloc.c) ↔ Psi α z lam ρ < Theta α z lam ω := by
  constructor
  · rintro ⟨τ, hτ, hc⟩
    have := (outcome_c_iff (h.withTau hτ)).1 hc
    exact lt_of_le_of_lt this.1 this.2
  · intro hlt
    refine ⟨Psi α z lam ρ, h.one_lt_psi.le, ?_⟩
    exact (outcome_c_iff (h.withTau h.one_lt_psi.le)).2 ⟨le_rfl, hlt⟩

theorem psi_lt_theta_iff_eta_neg (h : Regime α z lam ρ ω 1) :
    Psi α z lam ρ < Theta α z lam ω ↔ eta α z lam ρ ω < 0 := by
  have hpos : 0 < topPay α z lam * botPay α z lam := mul_pos h.top_lam_pos h.bot_lam_pos
  rw [eta_mul h]
  constructor
  · intro hl; exact mul_neg_of_neg_of_pos (by linarith) hpos
  · intro hn
    by_contra hge
    push_neg at hge
    exact absurd hn (not_lt.2 (mul_nonneg (by linarith) hpos.le))

end COccurs

/-- **Section 5.1.** Allocation `c` can never arise (at any `α ∈ [0,1)` and any `τ ≥ 1`) iff
    `η'(1) ≤ 0`; for all `z ∈ [0,1]` that is Condition 2, for some `z` Condition 3. -/
theorem c_never_iff {z lam ρ ω : ℝ} (hz0 : 0 ≤ z) (hz1 : z ≤ 1) (hρ : 1 / 2 < ρ) (hρω : ρ ≤ ω)
    (hω1 : ω ≤ 1) (hl : 1 / 2 < lam) (hl1 : lam < 1) :
    (∀ α τ : ℝ, 0 ≤ α → α < 1 → 1 ≤ τ → outcome τ α z lam ρ ω ≠ Alloc.c) ↔
      2 * etaA z lam ρ ω - etaB z lam ρ ω ≤ 0 := by
  rw [← eta_nonneg_iff_etaPrime hz0 hz1 hρ hρω hω1 hl1.le]
  have hR : ∀ α : ℝ, 0 ≤ α → α < 1 → Regime α z lam ρ ω 1 := fun α hα0 hα1 =>
    ⟨hα0, hα1, hz0, hz1, hρ, hρω.trans hω1, hl, hl1, by linarith, hω1, le_rfl⟩
  constructor
  · intro h α hα0 hα1
    by_contra hneg
    push_neg at hneg
    obtain ⟨τ, hτ, hc⟩ := (c_arises_iff (hR α hα0 hα1)).2 ((psi_lt_theta_iff_eta_neg (hR α hα0 hα1)).2 hneg)
    exact h α τ hα0 hα1 hτ hc
  · intro h α τ hα0 hα1 hτ hc
    have := (psi_lt_theta_iff_eta_neg (hR α hα0 hα1)).1 ((c_arises_iff (hR α hα0 hα1)).1 ⟨τ, hτ, hc⟩)
    linarith [h α hα0 hα1]

/-! ## 5. Examples and the three outcomes of Section 5.2 -/

/-- `ηc < 0` forces `η'(1) > 0` (the restriction `η'(1) ≤ 0 ⇒ η(0) ≥ 0`, contraposed). -/
theorem etaPrime_pos_of_etaC_neg {z lam ρ ω : ℝ} (hz0 : 0 ≤ z) (hz1 : z ≤ 1) (hρ : 1 / 2 < ρ)
    (hρω : ρ ≤ ω) (hω1 : ω ≤ 1) (hl1 : lam ≤ 1) (h : etaC z lam ρ ω < 0) :
    0 < 2 * etaA z lam ρ ω - etaB z lam ρ ω := by
  by_contra hn
  push_neg at hn
  exact absurd (etaC_nonneg_of_etaPrime hz0 hz1 hρ hρω hω1 hl1 hn) (not_le.2 h)

/-- Outcome (i): `η'(1) ≤ 0`; then `η ≥ 0` on `[0,1)` and `c` never arises. -/
theorem outcome_i {z lam ρ ω : ℝ} (hz0 : 0 ≤ z) (hz1 : z ≤ 1) (hρ : 1 / 2 < ρ) (hρω : ρ ≤ ω)
    (hω1 : ω ≤ 1) (hl1 : lam ≤ 1) (h : 2 * etaA z lam ρ ω - etaB z lam ρ ω ≤ 0) :
    ∀ α : ℝ, 0 ≤ α → α < 1 → 0 ≤ eta α z lam ρ ω :=
  (eta_nonneg_iff_etaPrime hz0 hz1 hρ hρω hω1 hl1).2 h

/-- Outcome (iii): `η(0) < 0`; then `η < 0` on all of `[0,1)`. -/
theorem outcome_iii {z lam ρ ω : ℝ} (hz0 : 0 ≤ z) (hz1 : z ≤ 1) (hρ : 1 / 2 < ρ) (hρω : ρ ≤ ω)
    (hω1 : ω ≤ 1) (hl1 : lam ≤ 1) (h : etaC z lam ρ ω < 0) :
    ∀ α : ℝ, 0 ≤ α → α < 1 → eta α z lam ρ ω < 0 := by
  have hp := etaPrime_pos_of_etaC_neg hz0 hz1 hρ hρω hω1 hl1 h
  intro α hα0 hα1
  rw [eta_factor]
  apply quad_neg _ _ h _ α hα0 hα1
  rw [etaB_eq] at hp
  linarith

/-- Outcome (ii): `η(0) ≥ 0` and `η'(1) > 0`; there is a unique `α̂ ∈ [0,1)` with `η ≥ 0` iff
    `α ≤ α̂`. -/
theorem outcome_ii {z lam ρ ω : ℝ} (hC : 0 ≤ etaC z lam ρ ω)
    (hP : 0 < 2 * etaA z lam ρ ω - etaB z lam ρ ω) :
    ∃! a : ℝ, 0 ≤ a ∧ a < 1 ∧
      ∀ α : ℝ, 0 ≤ α → α < 1 → (0 ≤ eta α z lam ρ ω ↔ α ≤ a) := by
  have hlt : etaC z lam ρ ω < etaA z lam ρ ω := by
    rw [etaB_eq] at hP; linarith
  simpa only [eta_factor] using quad_cutoff _ _ hC hlt

/-- The three outcomes exhaust the cases. -/
theorem outcomes_exhaust (z lam ρ ω : ℝ) :
    2 * etaA z lam ρ ω - etaB z lam ρ ω ≤ 0 ∨ etaC z lam ρ ω < 0 ∨
      (0 ≤ etaC z lam ρ ω ∧ 0 < 2 * etaA z lam ρ ω - etaB z lam ρ ω) := by
  by_cases h1 : 2 * etaA z lam ρ ω - etaB z lam ρ ω ≤ 0
  · exact Or.inl h1
  · by_cases h2 : etaC z lam ρ ω < 0
    · exact Or.inr (Or.inl h2)
    · exact Or.inr (Or.inr ⟨not_lt.1 h2, not_le.1 h1⟩)

/-- The paper's three examples at `z = 1` (p.16): `ω = .8, λ = ρ = 2/3` gives (i);
    `ω = 1, λ = ρ = .6275` gives (ii); `ω = 1, λ = ρ = .55` gives (iii). -/
theorem example_i :
    0 ≤ etaC 1 (2 / 3) (2 / 3) (4 / 5) ∧ 2 * etaA 1 (2 / 3) (2 / 3) (4 / 5) - etaB 1 (2 / 3) (2 / 3) (4 / 5) ≤ 0 := by
  rw [etaPrime_z_one, etaC_z_one]; norm_num

theorem example_ii :
    0 ≤ etaC 1 (251 / 400) (251 / 400) 1 ∧ 0 < 2 * etaA 1 (251 / 400) (251 / 400) 1 - etaB 1 (251 / 400) (251 / 400) 1 := by
  rw [etaPrime_z_one, etaC_z_one]; norm_num

theorem example_iii :
    etaC 1 (11 / 20) (11 / 20) 1 < 0 ∧ 0 < 2 * etaA 1 (11 / 20) (11 / 20) 1 - etaB 1 (11 / 20) (11 / 20) 1 := by
  rw [etaPrime_z_one, etaC_z_one]; norm_num

/-! ### The numerical example of Section 4 (pp.12, 14) -/

theorem example_regime (τ : ℝ) (hτ : 1 ≤ τ) :
    Regime (1 / 2) 1 (251 / 400) (251 / 400) 1 τ :=
  ⟨by norm_num, by norm_num, by norm_num, by norm_num, by norm_num, by norm_num, by norm_num,
    by norm_num, by norm_num, by norm_num, hτ⟩

theorem example_Psi : Psi (1 / 2) 1 (251 / 400) (251 / 400) = 223001 / 182201 := by
  unfold Psi topPay botPay; norm_num

theorem example_Theta : Theta (1 / 2) 1 (251 / 400) 1 = 320000 / 223001 := by
  unfold Theta topPay; norm_num

/-- `p₁ = 30,000`, `p₂ = 25,000` (`τ = 1.2`): H and L both enter Event 1's rival, allocation b. -/
theorem example_b : outcome (6 / 5) (1 / 2) 1 (251 / 400) (251 / 400) 1 = Alloc.b := by
  rw [outcome_b_iff (example_regime _ (by norm_num)), example_Psi]; norm_num

/-- `p₂ = 24,000` (`τ = 1.25`): allocation c, the counterintuitive one. -/
theorem example_c : outcome (5 / 4) (1 / 2) 1 (251 / 400) (251 / 400) 1 = Alloc.c := by
  rw [outcome_c_iff (example_regime _ (by norm_num)), example_Psi, example_Theta]; norm_num

/-- Section 5: lowering the prize of Event 2... raising it attracts the weaker field: if `c`
    arises at `τ̄` then `b` arises at some `1 < τ̂ < τ̄`. -/
theorem lower_ratio_attracts {α z lam ρ ω τ : ℝ} (h : Regime α z lam ρ ω τ)
    (hc : outcome τ α z lam ρ ω = Alloc.c) :
    ∃ τ' : ℝ, 1 < τ' ∧ τ' < τ ∧ outcome τ' α z lam ρ ω = Alloc.b := by
  have hψ := (outcome_c_iff h).1 hc
  have h1 := h.one_lt_psi
  refine ⟨(1 + Psi α z lam ρ) / 2, by linarith, by linarith [hψ.1], ?_⟩
  exact (outcome_b_iff (h.withTau (by linarith))).2 (by linarith)

/-! ## 6. Adding structure to the probabilities (Section 6) -/

section FOSD
open MeasureTheory

/-! ### 6.1 First-order dominance is not sufficient (p.20-21) -/

/-- `F_H`: `0` on `[0,.5)`, `51x - 25.5` on `[.5,.51)`, `x` on `[.51,1]`. -/
noncomputable def FH (v : ℝ) : ℝ := max 0 (min v (51 * v - 51 / 2))
/-- `F_L(x) = x`. -/
noncomputable def FL (v : ℝ) : ℝ := v
/-- `F_N`: `x` on `[0,.49)`, `51x - 24.5` on `[.49,.5)`, `1` on `[.5,1]`. -/
noncomputable def FN (v : ℝ) : ℝ := min 1 (max v (51 * v - 49 / 2))

/-- The densities, right-continuous versions. -/
noncomputable def fH (v : ℝ) : ℝ := if v < 1 / 2 then 0 else if v < 51 / 100 then 51 else 1
noncomputable def fL (_ : ℝ) : ℝ := 1
noncomputable def fN (v : ℝ) : ℝ := if v < 49 / 100 then 1 else if v < 1 / 2 then 51 else 0

theorem FH_cont : Continuous FH := by unfold FH; fun_prop
theorem FN_cont : Continuous FN := by unfold FN; fun_prop

/-- Integral over an interval of a function that agrees with a continuous one on its interior. -/
theorem piece {f g : ℝ → ℝ} {a b : ℝ} (hab : a ≤ b) (hg : Continuous g)
    (h : ∀ x ∈ Ioo a b, f x = g x) :
    IntervalIntegrable f volume a b ∧ ∫ x in a..b, f x = ∫ x in a..b, g x := by
  have hae : ∀ᵐ x ∂(volume : Measure ℝ), x ∈ Set.uIoc a b → f x = g x := by
    have hb : ∀ᵐ x ∂(volume : Measure ℝ), x ≠ b := by
      rw [ae_iff]; simp
    filter_upwards [hb] with x hxb hx
    rw [Set.uIoc_of_le hab] at hx
    exact h x ⟨hx.1, lt_of_le_of_ne hx.2 hxb⟩
  refine ⟨?_, intervalIntegral.integral_congr_ae hae⟩
  have hgi : IntervalIntegrable g volume a b := hg.intervalIntegrable a b
  refine (intervalIntegrable_congr_ae ?_).2 hgi
  rw [Filter.EventuallyEq, ae_restrict_iff' measurableSet_uIoc]
  filter_upwards [hae] with x hx hxI using hx hxI

theorem integral_three {f g1 g2 g3 : ℝ → ℝ} {a b c d : ℝ} (hab : a ≤ b) (hbc : b ≤ c) (hcd : c ≤ d)
    (hg1 : Continuous g1) (hg2 : Continuous g2) (hg3 : Continuous g3)
    (h1 : ∀ x ∈ Ioo a b, f x = g1 x) (h2 : ∀ x ∈ Ioo b c, f x = g2 x)
    (h3 : ∀ x ∈ Ioo c d, f x = g3 x) :
    ∫ x in a..d, f x = (∫ x in a..b, g1 x) + (∫ x in b..c, g2 x) + ∫ x in c..d, g3 x := by
  obtain ⟨i1, e1⟩ := piece hab hg1 h1
  obtain ⟨i2, e2⟩ := piece hbc hg2 h2
  obtain ⟨i3, e3⟩ := piece hcd hg3 h3
  rw [← e1, ← e2, ← e3, intervalIntegral.integral_add_adjacent_intervals i1 i2,
    intervalIntegral.integral_add_adjacent_intervals (i1.trans i2) i3]

theorem FN_ge_half {v : ℝ} (hv : 1 / 2 ≤ v) : FN v = 1 := by
  unfold FN
  rw [min_eq_left]
  exact le_max_of_le_right (by linarith)

theorem FN_lt_49 {v : ℝ} (hv : v < 49 / 100) : FN v = v := by
  unfold FN
  rw [max_eq_left (by linarith), min_eq_right (by linarith)]

theorem FN_lt_49' {v : ℝ} (hv : v ≤ 49 / 100) : FN v = v := by
  unfold FN
  rw [max_eq_left (by linarith), min_eq_right (by linarith)]

theorem FN_mid {v : ℝ} (h1 : 49 / 100 ≤ v) (h2 : v ≤ 1 / 2) : FN v = 51 * v - 49 / 2 := by
  unfold FN
  rw [max_eq_right (by linarith), min_eq_right (by linarith)]

/-- `ω = P(H beats N) = ∫ F_N f_H = 1`. -/
theorem fosd_omega : ∫ v in (0 : ℝ)..1, FN v * fH v = 1 := by
  rw [integral_three (g1 := fun _ => 0) (g2 := fun _ => 51) (g3 := fun _ => 1)
    (b := 1 / 2) (c := 51 / 100) (by norm_num) (by norm_num) (by norm_num)
    continuous_const continuous_const continuous_const]
  · norm_num
  · intro x hx
    unfold fH
    rw [if_pos hx.2]; ring
  · intro x hx
    unfold fH
    rw [if_neg (not_lt.2 hx.1.le), if_pos hx.2, FN_ge_half hx.1.le]; ring
  · intro x hx
    unfold fH
    rw [if_neg (not_lt.2 (by linarith [hx.1])), if_neg (not_lt.2 hx.1.le),
      FN_ge_half (by linarith [hx.1])]; ring

/-- `λ = P(H beats L) = ∫ F_L f_H = 251/400`. -/
theorem fosd_lambda : ∫ v in (0 : ℝ)..1, FL v * fH v = 251 / 400 := by
  rw [integral_three (g1 := fun _ => 0) (g2 := fun v => 51 * v) (g3 := fun v => v)
    (b := 1 / 2) (c := 51 / 100) (by norm_num) (by norm_num) (by norm_num)
    continuous_const (by fun_prop) continuous_id]
  · have e2 : ∫ x in (1 / 2 : ℝ)..51 / 100, 51 * x = 51 * ((51 / 100) ^ 2 / 2 - (1 / 2) ^ 2 / 2) := by
      rw [intervalIntegral.integral_const_mul, integral_id]; ring
    simp only [integral_id, intervalIntegral.integral_const, e2]
    norm_num
  · intro x hx
    unfold fH FL
    rw [if_pos hx.2]; ring
  · intro x hx
    unfold fH FL
    rw [if_neg (not_lt.2 hx.1.le), if_pos hx.2]; ring
  · intro x hx
    unfold fH FL
    rw [if_neg (not_lt.2 (by linarith [hx.1])), if_neg (not_lt.2 hx.1.le)]; ring

/-- `ρ = P(L beats N) = ∫ F_N f_L = 251/400`. -/
theorem fosd_rho : ∫ v in (0 : ℝ)..1, FN v * fL v = 251 / 400 := by
  rw [integral_three (g1 := fun v => v) (g2 := fun v => 51 * v - 49 / 2) (g3 := fun _ => 1)
    (b := 49 / 100) (c := 1 / 2) (by norm_num) (by norm_num) (by norm_num)
    continuous_id (by fun_prop) continuous_const]
  · have e2 : ∫ x in (49 / 100 : ℝ)..1 / 2, (51 * x - 49 / 2) =
        51 * ((1 / 2) ^ 2 / 2 - (49 / 100) ^ 2 / 2) - 49 / 2 * (1 / 2 - 49 / 100) := by
      rw [intervalIntegral.integral_sub (Continuous.intervalIntegrable (by fun_prop) _ _)
        (Continuous.intervalIntegrable (by fun_prop) _ _), intervalIntegral.integral_const_mul,
        integral_id, intervalIntegral.integral_const]
      simp; ring
    simp only [integral_id, intervalIntegral.integral_const, e2]
    norm_num
  · intro x hx
    unfold fL
    rw [FN_lt_49 hx.2]; ring
  · intro x hx
    unfold fL
    rw [FN_mid hx.1.le hx.2.le]; ring
  · intro x hx
    unfold fL
    rw [FN_ge_half hx.1.le]; ring

/-! The densities are the derivatives of the printed distribution functions (off the two kinks), and
`F(x) = ∫₀ˣ f`. -/

theorem FH_le_half {v : ℝ} (hv : v ≤ 1 / 2) : FH v = 0 := by
  unfold FH
  rw [min_eq_right (by linarith), max_eq_left (by linarith)]

theorem FH_mid {v : ℝ} (h1 : 1 / 2 ≤ v) (h2 : v ≤ 51 / 100) : FH v = 51 * v - 51 / 2 := by
  unfold FH
  rw [min_eq_right (by linarith), max_eq_right (by linarith)]

theorem FH_ge {v : ℝ} (hv : 51 / 100 ≤ v) : FH v = v := by
  unfold FH
  rw [min_eq_left (by linarith), max_eq_right (by linarith)]

theorem FH_hasDerivAt {v : ℝ} (h1 : v ≠ 1 / 2) (h2 : v ≠ 51 / 100) : HasDerivAt FH (fH v) v := by
  unfold fH
  rcases lt_trichotomy v (1 / 2) with hv | hv | hv
  · rw [if_pos hv]
    exact (hasDerivAt_const v (0 : ℝ)).congr_of_eventuallyEq
      (by filter_upwards [Iio_mem_nhds hv] with x hx using FH_le_half hx.le)
  · exact absurd hv h1
  · rw [if_neg (not_lt.2 hv.le)]
    rcases lt_trichotomy v (51 / 100) with hw | hw | hw
    · rw [if_pos hw]
      have : HasDerivAt (fun x : ℝ => 51 * x - 51 / 2) 51 v := by
        simpa using ((hasDerivAt_id v).const_mul (51 : ℝ)).sub_const (51 / 2 : ℝ)
      exact this.congr_of_eventuallyEq
        (by filter_upwards [Ioo_mem_nhds hv hw] with x hx using FH_mid hx.1.le hx.2.le)
    · exact absurd hw h2
    · rw [if_neg (not_lt.2 hw.le)]
      exact (hasDerivAt_id v).congr_of_eventuallyEq
        (by filter_upwards [Ioi_mem_nhds hw] with x hx using FH_ge hx.le)

theorem FN_hasDerivAt {v : ℝ} (h1 : v ≠ 49 / 100) (h2 : v ≠ 1 / 2) : HasDerivAt FN (fN v) v := by
  unfold fN
  rcases lt_trichotomy v (49 / 100) with hv | hv | hv
  · rw [if_pos hv]
    exact (hasDerivAt_id v).congr_of_eventuallyEq
      (by filter_upwards [Iio_mem_nhds hv] with x hx using FN_lt_49 hx)
  · exact absurd hv h1
  · rw [if_neg (not_lt.2 hv.le)]
    rcases lt_trichotomy v (1 / 2) with hw | hw | hw
    · rw [if_pos hw]
      have : HasDerivAt (fun x : ℝ => 51 * x - 49 / 2) 51 v := by
        simpa using ((hasDerivAt_id v).const_mul (51 : ℝ)).sub_const (49 / 2 : ℝ)
      exact this.congr_of_eventuallyEq
        (by filter_upwards [Ioo_mem_nhds hv hw] with x hx using FN_mid hx.1.le hx.2.le)
    · exact absurd hw h2
    · rw [if_neg (not_lt.2 hw.le)]
      exact (hasDerivAt_const v (1 : ℝ)).congr_of_eventuallyEq
        (by filter_upwards [Ioi_mem_nhds hw] with x hx using FN_ge_half hx.le)

theorem fH_integrable : IntervalIntegrable fH volume 0 1 := by
  have h := integral_three (f := fH) (g1 := fun _ => 0) (g2 := fun _ => 51) (g3 := fun _ => 1)
    (b := 1 / 2) (c := 51 / 100) (a := 0) (d := 1) (by norm_num) (by norm_num) (by norm_num)
    continuous_const continuous_const continuous_const
  obtain ⟨i1, -⟩ := piece (f := fH) (g := fun _ => (0 : ℝ)) (a := 0) (b := 1 / 2) (by norm_num)
    continuous_const (fun x hx => by unfold fH; rw [if_pos hx.2])
  obtain ⟨i2, -⟩ := piece (f := fH) (g := fun _ => (51 : ℝ)) (a := 1 / 2) (b := 51 / 100) (by norm_num)
    continuous_const (fun x hx => by unfold fH; rw [if_neg (not_lt.2 hx.1.le), if_pos hx.2])
  obtain ⟨i3, -⟩ := piece (f := fH) (g := fun _ => (1 : ℝ)) (a := 51 / 100) (b := 1) (by norm_num)
    continuous_const (fun x hx => by
      unfold fH
      rw [if_neg (not_lt.2 (by linarith [hx.1])), if_neg (not_lt.2 hx.1.le)])
  exact (i1.trans i2).trans i3

theorem fN_integrable : IntervalIntegrable fN volume 0 1 := by
  obtain ⟨i1, -⟩ := piece (f := fN) (g := fun _ => (1 : ℝ)) (a := 0) (b := 49 / 100) (by norm_num)
    continuous_const (fun x hx => by unfold fN; rw [if_pos hx.2])
  obtain ⟨i2, -⟩ := piece (f := fN) (g := fun _ => (51 : ℝ)) (a := 49 / 100) (b := 1 / 2) (by norm_num)
    continuous_const (fun x hx => by unfold fN; rw [if_neg (not_lt.2 hx.1.le), if_pos hx.2])
  obtain ⟨i3, -⟩ := piece (f := fN) (g := fun _ => (0 : ℝ)) (a := 1 / 2) (b := 1) (by norm_num)
    continuous_const (fun x hx => by
      unfold fN
      rw [if_neg (not_lt.2 (by linarith [hx.1])), if_neg (not_lt.2 hx.1.le)])
  exact (i1.trans i2).trans i3

/-- A density is the derivative of its distribution function off two kinks. -/
theorem integral_density {F f : ℝ → ℝ} {b1 b2 x : ℝ} (hx0 : 0 ≤ x) (hx1 : x ≤ 1)
    (hF : Continuous F) (hd : ∀ v, v ≠ b1 → v ≠ b2 → HasDerivAt F (f v) v)
    (hi : IntervalIntegrable f volume 0 1) : ∫ t in (0 : ℝ)..x, f t = F x - F 0 := by
  refine MeasureTheory.integral_eq_of_hasDerivAt_off_countable_of_le F f hx0
    (s := {b1, b2}) (Set.toFinite _ |>.countable) hF.continuousOn ?_ ?_
  · intro v hv
    exact hd v (fun h => hv.2 (Or.inl h)) (fun h => hv.2 (Or.inr h))
  · refine hi.mono_set ?_
    rw [Set.uIcc_of_le hx0, Set.uIcc_of_le zero_le_one]
    exact Set.Icc_subset_Icc le_rfl hx1

/-- `F_H(x) = ∫₀ˣ f_H` on `[0,1]`: `f_H` is the density of the printed `F_H`. -/
theorem FH_eq_integral {x : ℝ} (hx0 : 0 ≤ x) (hx1 : x ≤ 1) : ∫ t in (0 : ℝ)..x, fH t = FH x := by
  rw [integral_density hx0 hx1 FH_cont (fun v h1 h2 => FH_hasDerivAt h1 h2) fH_integrable,
    FH_le_half (by norm_num : (0 : ℝ) ≤ 1 / 2)]
  ring

/-- `F_N(x) = ∫₀ˣ f_N` on `[0,1]`. -/
theorem FN_eq_integral {x : ℝ} (hx0 : 0 ≤ x) (hx1 : x ≤ 1) : ∫ t in (0 : ℝ)..x, fN t = FN x := by
  rw [integral_density hx0 hx1 FN_cont (fun v h1 h2 => FN_hasDerivAt h1 h2) fN_integrable,
    FN_lt_49 (by norm_num : (0 : ℝ) < 49 / 100)]
  ring

/-- `F_L(x) = x = ∫₀ˣ f_L`. -/
theorem FL_eq_integral (x : ℝ) : ∫ t in (0 : ℝ)..x, fL t = FL x := by
  unfold fL FL; simp

/-- The three printed distribution functions are first-order stochastically ordered,
    `F_H ≤ F_L ≤ F_N` on `[0,1]` (`X_H` FSD `X_L` FSD `X_N`), with `F(0) = 0` and `F(1) = 1`. -/
theorem fosd_order {v : ℝ} (hv0 : 0 ≤ v) (hv1 : v ≤ 1) : FH v ≤ FL v ∧ FL v ≤ FN v := by
  unfold FL
  constructor
  · rcases le_total v (1 / 2) with h | h
    · rw [FH_le_half h]; exact hv0
    · rcases le_total v (51 / 100) with h2 | h2
      · rw [FH_mid h h2]; linarith
      · rw [FH_ge h2]
  · rcases le_total v (49 / 100) with h | h
    · rw [FN_lt_49' h]
    · rcases le_total v (1 / 2) with h2 | h2
      · rw [FN_mid h h2]; linarith
      · rw [FN_ge_half h2]; exact hv1

theorem fosd_endpoints : FH 0 = 0 ∧ FH 1 = 1 ∧ FL 0 = 0 ∧ FL 1 = 1 ∧ FN 0 = 0 ∧ FN 1 = 1 := by
  refine ⟨FH_le_half (by norm_num), ?_, rfl, rfl, ?_, FN_ge_half (by norm_num)⟩
  · rw [FH_ge (by norm_num)]
  · rw [FN_lt_49 (by norm_num)]


/-- **First-order dominance does not rule out `c`** (p.21): with the printed `F_H, F_L, F_N`, the
    probabilities computed from them as integrals are `ω = 1`, `λ = ρ = 251/400`, and at `α = 1/2`,
    `z = 1`, `τ = 5/4` (any `τ ∈ (1.2239, 1.4350)`) the allocation is `c`. -/
theorem fosd_example_c :
    outcome (5 / 4) (1 / 2) 1 (∫ v in (0 : ℝ)..1, FL v * fH v) (∫ v in (0 : ℝ)..1, FN v * fL v)
      (∫ v in (0 : ℝ)..1, FN v * fH v) = Alloc.c := by
  rw [fosd_omega, fosd_lambda, fosd_rho]
  exact example_c

end FOSD

/-! ### 6.2 Performance drawn from power functions (p.21-22) -/

/-- `P(X beats Y) = ∫ F_Y f_X` for `F_X = x^a`, `F_Y = x^b`: the power functions of Section 6.2
    give `a / (a + b)`. -/
theorem prob_power {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    ∫ v in (0 : ℝ)..1, v ^ b * (a * v ^ (a - 1)) = a / (a + b) := by
  have hab : 0 < a + b := by linarith
  have h1 : ∫ v in (0 : ℝ)..1, v ^ b * (a * v ^ (a - 1)) = ∫ v in (0 : ℝ)..1, a * v ^ (a + b - 1) := by
    apply intervalIntegral.integral_congr_ae
    refine Filter.Eventually.of_forall (fun v hv => ?_)
    rw [Set.uIoc_of_le zero_le_one] at hv
    have hv0 : 0 < v := hv.1
    have : v ^ (a + b - 1) = v ^ b * v ^ (a - 1) := by
      rw [← Real.rpow_add hv0]; congr 1; ring
    rw [this]; ring
  have e : a + b - 1 + 1 = a + b := by ring
  rw [h1, intervalIntegral.integral_const_mul, integral_rpow (Or.inl (by linarith)), e]
  simp only [Real.one_rpow, Real.zero_rpow hab.ne']
  field_simp
  ring

/-- With `F_H = x^H`, `F_L = x^L`, `F_N = x^N`: `λ = H/(L+H)`, `ρ = L/(N+L)`, `ω = H/(N+H)`. -/
noncomputable def lamPow (L H : ℝ) : ℝ := H / (L + H)
noncomputable def rhoPow (N L : ℝ) : ℝ := L / (N + L)
noncomputable def omPow (N H : ℝ) : ℝ := H / (N + H)

theorem lamPow_eq {L H : ℝ} (hL : 0 < L) (hH : 0 < H) :
    ∫ v in (0 : ℝ)..1, v ^ L * (H * v ^ (H - 1)) = lamPow L H := by
  rw [prob_power hH hL]; unfold lamPow; ring_nf

theorem rhoPow_eq {N L : ℝ} (hN : 0 < N) (hL : 0 < L) :
    ∫ v in (0 : ℝ)..1, v ^ N * (L * v ^ (L - 1)) = rhoPow N L := by
  rw [prob_power hL hN]; unfold rhoPow; ring_nf

theorem omPow_eq {N H : ℝ} (hN : 0 < N) (hH : 0 < H) :
    ∫ v in (0 : ℝ)..1, v ^ N * (H * v ^ (H - 1)) = omPow N H := by
  rw [prob_power hH hN]; unfold omPow; ring_nf

/-- Condition 2 for the power functions, in the paper's polynomial form (p.22). -/
theorem power_condition2_iff {N L H : ℝ} (hN : 0 < N) (hL : 0 < L) (hH : 0 < H) :
    (omPow N H ^ 2 - rhoPow N L ^ 2 + 1) / 2 ≤ lamPow L H ↔
      0 ≤ N ^ 4 * (H - L) + 2 * N ^ 2 * H * L * (H - L) + H ^ 2 * L ^ 2 * (H - L)
        + 2 * N ^ 3 * (H ^ 2 - L ^ 2) := by
  have hd : 0 < 2 * ((L + H) * (N + H) ^ 2 * (N + L) ^ 2) := by positivity
  have hid : lamPow L H - (omPow N H ^ 2 - rhoPow N L ^ 2 + 1) / 2 =
      (N ^ 4 * (H - L) + 2 * N ^ 2 * H * L * (H - L) + H ^ 2 * L ^ 2 * (H - L)
        + 2 * N ^ 3 * (H ^ 2 - L ^ 2)) / (2 * ((L + H) * (N + H) ^ 2 * (N + L) ^ 2)) := by
    unfold lamPow omPow rhoPow
    field_simp
    ring
  rw [← sub_nonneg, hid, div_nonneg_iff]
  constructor
  · rintro (⟨h, _⟩ | ⟨_, h⟩)
    · exact h
    · exact absurd h (not_le.2 hd)
  · intro h; exact Or.inl ⟨h, hd.le⟩

/-- The polynomial is non-negative when `0 < N` and `L < H`. -/
theorem power_poly_nonneg {N L H : ℝ} (hN : 0 < N) (hL : 0 < L) (hLH : L < H) :
    0 ≤ N ^ 4 * (H - L) + 2 * N ^ 2 * H * L * (H - L) + H ^ 2 * L ^ 2 * (H - L)
        + 2 * N ^ 3 * (H ^ 2 - L ^ 2) := by
  have hH : 0 < H := hL.trans hLH
  have h1 : 0 < H - L := sub_pos.2 hLH
  have h2 : 0 ≤ H ^ 2 - L ^ 2 := by nlinarith
  positivity

/-- The paper's numerical remark (footnote 19): `N = .5, L = 1, H = 2` gives `ω = .8`,
    `λ = ρ = 2/3`. -/
theorem power_example :
    omPow (1 / 2) 2 = 4 / 5 ∧ lamPow 1 2 = 2 / 3 ∧ rhoPow (1 / 2) 1 = 2 / 3 := by
  unfold omPow lamPow rhoPow; norm_num


/-- **With power functions allocation `c` never arises** (p.22): for `0 < N < L < H`, every
    `z ∈ [0,1]`, `α ∈ [0,1)` and `τ ≥ 1`. -/
theorem power_functions_no_c {N L H : ℝ} (hN : 0 < N) (hLN : N < L) (hLH : L < H) {z : ℝ}
    (hz0 : 0 ≤ z) (hz1 : z ≤ 1) :
    ∀ α τ : ℝ, 0 ≤ α → α < 1 → 1 ≤ τ →
      outcome τ α z (lamPow L H) (rhoPow N L) (omPow N H) ≠ Alloc.c := by
  have hL : 0 < L := hN.trans hLN
  have hH : 0 < H := hL.trans hLH
  have hρ : 1 / 2 < rhoPow N L := by
    unfold rhoPow; rw [lt_div_iff₀ (by linarith)]; linarith
  have hρω : rhoPow N L ≤ omPow N H := by
    unfold rhoPow omPow; rw [div_le_div_iff₀ (by linarith) (by linarith)]; nlinarith
  have hω1 : omPow N H ≤ 1 := by
    unfold omPow; rw [div_le_one (by linarith)]; linarith
  have hl : 1 / 2 < lamPow L H := by
    unfold lamPow; rw [lt_div_iff₀ (by linarith)]; linarith
  have hl1 : lamPow L H < 1 := by
    unfold lamPow; rw [div_lt_one (by linarith)]; linarith
  have hc2 := (power_condition2_iff hN hL hH).2 (power_poly_nonneg hN hL hLH)
  have h2 := (condition2_iff hρω hρ).2 hc2 z hz0 hz1
  exact (c_never_iff hz0 hz1 hρ hρω hω1 hl hl1).2 h2

/-! ## 7. The organiser's problem (Section 5.3, Appendix B)

Event 1 pays `p₁`, Event 2 pays `p₂ ≤ p₁`, a runner-up gets the share `α`; the organiser minimises
`(1+α)(p₁+p₂)` over the prizes that produce a separating allocation. With reservation payoffs
`r_L`, `r_H` the paper's conditions (p.18, Appendix B) are, in terms of `topPay`/`botPay` (prize 1):

* `b` (H in Event 1, L in Event 2): `ΠbL > ΠaL`, `ΠbL ≥ r_L`, `ΠbH ≥ r_H`;
* `c` (H in Event 2, L in Event 1): `ΠaL ≥ ΠbL`, `ΠaL ≥ r_L`, `ΠcH > ΠaH`, `ΠcH ≥ r_H`,

with `ΠaL = p₁ bot(λ)`, `ΠbL = p₂ top(ρ)`, `ΠbH = p₁ top(ω)`, `ΠaH = p₁ top(λ)`, `ΠcH = p₂ top(ω)`.
-/

/-- The conditions under which allocation `b` results (p.18, Appendix B), with `p₂ ≤ p₁`. -/
def bFeasible (z lam ρ ω rL rH α p1 p2 : ℝ) : Prop :=
  0 ≤ α ∧ α < 1 ∧ 0 ≤ p2 ∧ p2 ≤ p1 ∧ p1 * botPay α z lam < p2 * topPay α z ρ ∧
    rL ≤ p2 * topPay α z ρ ∧ rH ≤ p1 * topPay α z ω

/-- The conditions under which allocation `c` results (p.18, Appendix B), with `p₂ ≤ p₁`. -/
def cFeasible (z lam ρ ω rL rH α p1 p2 : ℝ) : Prop :=
  0 ≤ α ∧ α < 1 ∧ 0 ≤ p2 ∧ p2 ≤ p1 ∧ p2 * topPay α z ρ ≤ p1 * botPay α z lam ∧
    rL ≤ p1 * botPay α z lam ∧ p1 * topPay α z lam < p2 * topPay α z ω ∧ rH ≤ p2 * topPay α z ω

/-- Total prizes paid: `(1+α)(p₁+p₂)`. -/
def prizeCost (α p1 p2 : ℝ) : ℝ := (1 + α) * (p1 + p2)

/-- The example's payoffs at `z = 1`, `ρ = λ = 11/20`, `ω = 1` (Appendix B): `(279α+121)/400`,
    `(319α+81)/400` and `1`. -/
theorem example_payoffs (α : ℝ) :
    topPay α 1 (11 / 20) = (279 * α + 121) / 400 ∧ botPay α 1 (11 / 20) = (319 * α + 81) / 400 ∧
      topPay α 1 1 = 1 := by
  unfold topPay botPay; refine ⟨?_, ?_, ?_⟩ <;> ring

/-- **The cheapest way to realise `b`** costs at least `1076/279 ≈ 3.8566`. -/
theorem b_cost_lower {α p1 p2 : ℝ}
    (h : bFeasible 1 (11 / 20) (11 / 20) 1 (19 / 20) 1 α p1 p2) :
    1076 / 279 ≤ prizeCost α p1 p2 := by
  obtain ⟨hα0, hα1, hp2, hp21, -, hrL, hrH⟩ := h
  obtain ⟨hT, -, hO⟩ := example_payoffs α
  rw [hT] at hrL
  rw [hO] at hrH
  unfold prizeCost
  have hp1 : 1 ≤ p1 := by linarith
  have ht : 0 < 279 * α + 121 := by linarith
  have hp2t : 380 ≤ p2 * (279 * α + 121) := by nlinarith
  have key : 1076 / 279 * (279 * α + 121) ≤ (1 + α) * (p1 + p2) * (279 * α + 121) := by
    rcases le_total α (259 / 279) with hα | hα
    · have h2 : 760 ≤ (p1 + p2) * (279 * α + 121) := by nlinarith
      nlinarith [mul_le_mul_of_nonneg_left h2 (show 0 ≤ 1 + α by linarith)]
    · have h2 : 279 * α + 501 ≤ (p1 + p2) * (279 * α + 121) := by nlinarith
      nlinarith [mul_le_mul_of_nonneg_left h2 (show 0 ≤ 1 + α by linarith),
        mul_nonneg (show 0 ≤ 279 * α - 259 by linarith) (show 0 ≤ 279 * α - 37 by linarith)]
  exact le_of_mul_le_mul_right key ht

/-- `p₁ = p₂ = 1`, `α = 259/279` realises `b` at exactly that cost. -/
theorem b_plan :
    bFeasible 1 (11 / 20) (11 / 20) 1 (19 / 20) 1 (259 / 279) 1 1 ∧
      prizeCost (259 / 279) 1 1 = 1076 / 279 := by
  obtain ⟨hT, hB, hO⟩ := example_payoffs (259 / 279)
  refine ⟨?_, ?_⟩
  · unfold bFeasible
    rw [hT, hB, hO]; norm_num
  · unfold prizeCost; norm_num

theorem b_min_cost :
    IsLeast {c : ℝ | ∃ α p1 p2, bFeasible 1 (11 / 20) (11 / 20) 1 (19 / 20) 1 α p1 p2 ∧
      c = prizeCost α p1 p2} (1076 / 279) :=
  ⟨⟨_, 1, 1, b_plan.1, b_plan.2.symm⟩, by
    rintro c ⟨α, p1, p2, h, rfl⟩
    exact b_cost_lower h⟩

/-- **The cheapest way to realise `c`** costs at least `(2√90440 + 618)/319 ≈ 3.8228`: the two
    participation constraints `p₂ ≥ 1` and `p₁ (319α + 81) ≥ 380` already force it, by
    `(u - √90440)² ≥ 0` for `u = 319α + 81`. -/
theorem c_cost_lower {α p1 p2 : ℝ}
    (h : cFeasible 1 (11 / 20) (11 / 20) 1 (19 / 20) 1 α p1 p2) :
    (2 * Real.sqrt 90440 + 618) / 319 ≤ prizeCost α p1 p2 := by
  obtain ⟨hα0, hα1, hp2, hp21, -, hrL, -, hrH⟩ := h
  obtain ⟨-, hB, hO⟩ := example_payoffs α
  rw [hB] at hrL
  rw [hO] at hrH
  unfold prizeCost
  have hs := Real.mul_self_sqrt (show (0 : ℝ) ≤ 90440 by norm_num)
  set s := Real.sqrt 90440
  have hu : 0 < 319 * α + 81 := by linarith
  have hp1u : 380 ≤ p1 * (319 * α + 81) := by nlinarith
  have hp2' : 1 ≤ p2 := by linarith
  have key : (2 * s + 618) / 319 * (319 * α + 81) ≤ (1 + α) * (p1 + p2) * (319 * α + 81) := by
    have h2 : 380 + (319 * α + 81) ≤ (p1 + p2) * (319 * α + 81) := by nlinarith
    nlinarith [mul_le_mul_of_nonneg_left h2 (show 0 ≤ 1 + α by linarith), sq_nonneg (319 * α + 81 - s)]
  exact le_of_mul_le_mul_right key hu

theorem sqrt_90440_bounds : 300.73 < Real.sqrt 90440 ∧ Real.sqrt 90440 < 300.74 := by
  constructor
  · apply Real.lt_sqrt_of_sq_lt; norm_num
  · rw [Real.sqrt_lt' (by norm_num)]; norm_num

/-- `α = (√90440 − 81)/319`, `p₁ = 380/√90440`, `p₂ = 1` realises `c` at exactly that cost. -/
theorem c_plan :
    cFeasible 1 (11 / 20) (11 / 20) 1 (19 / 20) 1 ((Real.sqrt 90440 - 81) / 319)
        (380 / Real.sqrt 90440) 1 ∧
      prizeCost ((Real.sqrt 90440 - 81) / 319) (380 / Real.sqrt 90440) 1 =
        (2 * Real.sqrt 90440 + 618) / 319 := by
  obtain ⟨hlo, hhi⟩ := sqrt_90440_bounds
  have hs := Real.mul_self_sqrt (show (0 : ℝ) ≤ 90440 by norm_num)
  set s := Real.sqrt 90440
  have hs0 : 0 < s := by linarith
  obtain ⟨hT, hB, hO⟩ := example_payoffs ((s - 81) / 319)
  have hu : 319 * ((s - 81) / 319) + 81 = s := by ring
  refine ⟨?_, ?_⟩
  · unfold cFeasible
    rw [hT, hB, hO, hu]
    have e1 : 380 / s * (s / 400) = 19 / 20 := by field_simp; ring
    refine ⟨by linarith, by linarith, by norm_num, ?_, ?_, ?_, ?_, ?_⟩
    · rw [le_div_iff₀ hs0]; linarith
    · rw [e1]
      have : (279 * ((s - 81) / 319) + 121) / 400 ≤ 19 / 20 := by linarith
      linarith
    · rw [e1]
    · have : 380 / s * ((279 * ((s - 81) / 319) + 121) / 400) < 1 * 1 := by
        rw [div_mul_eq_mul_div, div_lt_iff₀ hs0]; linarith
      linarith
    · norm_num
  · unfold prizeCost
    field_simp
    nlinarith

/-- `(2√90440 + 618)/319 ∈ (3.8227, 3.8229)`: the paper's `≈ 3.8228`. -/
theorem c_cost_approx : 3.8227 < (2 * Real.sqrt 90440 + 618) / 319 ∧
    (2 * Real.sqrt 90440 + 618) / 319 < 3.8229 := by
  obtain ⟨hlo, hhi⟩ := sqrt_90440_bounds
  constructor <;> linarith

theorem c_min_cost :
    IsLeast {c : ℝ | ∃ α p1 p2, cFeasible 1 (11 / 20) (11 / 20) 1 (19 / 20) 1 α p1 p2 ∧
      c = prizeCost α p1 p2} ((2 * Real.sqrt 90440 + 618) / 319) :=
  ⟨⟨_, _, 1, c_plan.1, c_plan.2.symm⟩, by
    rintro c ⟨α, p1, p2, h, rfl⟩
    exact c_cost_lower h⟩

/-- **The monopolist prefers `c`**: `3.8228 < 3.8566`. -/
theorem c_cheaper_than_b : (2 * Real.sqrt 90440 + 618) / 319 < 1076 / 279 := by
  obtain ⟨hlo, hhi⟩ := sqrt_90440_bounds
  linarith

end MathewsNamoro2008
