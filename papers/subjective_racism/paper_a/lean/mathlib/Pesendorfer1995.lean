import Mathlib

/-! Pesendorfer (1995), "Design Innovation and Fashion Cycles", in the paper's
notation.

Consumers `q ∈ [0, 1]`, high types `q ≤ α`, low types `q > α`. The match
payoff is `u(i, j)`, with `vh = u(h,h) − u(h,ℓ)` and `vl = u(ℓ,h) − u(ℓ,ℓ)`.
Assumption (1), complementarity, is `vl < vh`. Users of the same design are
matched at random (matching technology (i)), so the chance of meeting a high
type is the share of high types among the actual users. `f` is eq. (4).

Coverage. Formalised in general: matching technology (i), eqs. (2) to (5),
the shape of `f`, the static sorting and separating price, footnotes 24 and
26, and the price floor of PDF p. 15. Formalised in a special case: the
static step of Proposition 1 (i), Proposition 1 (ii) from the revenue premise
of its proof, Proposition 1 (iii) as the unbounded-gain step, Proposition 3
(price path, cycle length, separation), the examples (prices that follow from
eq. 9). Not formalised: Theorem 1, Proposition 1 (iv), Proposition 2, eqs.
(8) to (10) as an equilibrium, Claims 1 and 2 and (A1) to (A4), footnote 9's
continuum example. They rest on the durable-goods monopoly results of Gul,
Sonnenschein and Wilson (1986) and on limits in `δ`. See
`lit/pesendorfer_1995/CLAIMS.md`. -/

namespace Pesendorfer1995

/-! ### Primitives -/

/-- Eq. (4). The maximum price consumer `q` pays when `[0, q]` use the design. -/
noncomputable def f (α vh vl q : ℝ) : ℝ :=
  if q ≤ α then vh * ((1 - α) / (1 - q)) else vl * (α / q)

/-- Matching technology (i). A user of a design whose users have masses `μh`
of high and `μl` of low types meets a high type with probability
`μh/(μh + μl)`. -/
noncomputable def meetHigh (μh μl : ℝ) : ℝ := μh / (μh + μl)

/-- Eq. (5), a consumer's period payoff. `ν` is her chance of meeting a high
type, `uih` and `uil` her payoffs from a high and a low partner, `p · y` her
spending. -/
def eq5 (δ ν uih uil p y : ℝ) : ℝ := (1 - δ) * (ν * uih + (1 - ν) * uil) - p * y

/-- Eqs. (6) and (7), discounted sums of period payoffs. -/
noncomputable def discounted (δ : ℝ) (v : ℕ → ℝ) : ℝ := ∑' t : ℕ, δ ^ t * v t

/-! ### Results -/

/-- Eq. (2). A high type `q ≤ α` meets a high type with probability 1 inside the
design and `(α − q)/(1 − q)` outside it, so her gain is `vh (1 − α)/(1 − q)`. -/
theorem eq2 {α vh q : ℝ} (hq : q < 1) :
    vh * (1 - (α - q) / (1 - q)) = vh * ((1 - α) / (1 - q)) := by
  have h : (1 : ℝ) - q ≠ 0 := by linarith
  congr 1
  field_simp
  ring

/-- Eq. (3). A low type `q > α` meets a high type with probability `α/q` inside
the design and 0 outside it, so her gain is `vl α / q`. -/
theorem eq3 {α vl q : ℝ} : vl * (α / q - 0) = vl * (α / q) := by ring

/-- The bandwagon range. For `q ≤ α` the inverse demand rises with `q`. -/
theorem bandwagon {α vh vl q₁ q₂ : ℝ} (hα : α < 1) (hvh : 0 < vh)
    (h12 : q₁ < q₂) (h2 : q₂ ≤ α) : f α vh vl q₁ < f α vh vl q₂ := by
  have h1 : q₁ ≤ α := by linarith
  unfold f
  rw [if_pos h1, if_pos h2]
  have hq2 : 0 < 1 - q₂ := by linarith
  have hq1 : 0 < 1 - q₁ := by linarith
  have hnum : 0 < 1 - α := by linarith
  apply mul_lt_mul_of_pos_left _ hvh
  exact div_lt_div_of_pos_left hnum hq2 (by linarith)

/-- The snob range. For `q > α` the inverse demand falls with `q`. -/
theorem snob {α vh vl q₁ q₂ : ℝ} (hα : 0 < α) (hvl : 0 < vl)
    (h1 : α < q₁) (h12 : q₁ < q₂) : f α vh vl q₂ < f α vh vl q₁ := by
  have h2 : α < q₂ := by linarith
  unfold f
  rw [if_neg (not_le.2 h1), if_neg (not_le.2 h2)]
  apply mul_lt_mul_of_pos_left _ hvl
  exact div_lt_div_of_pos_left hα (by linarith) h12

/-- The peak. `f α = vh`, and under complementarity no `q` gives more. -/
theorem peak {α vh vl : ℝ} (hα0 : 0 < α) (hα : α < 1) (hvl : 0 < vl) (hc : vl < vh) :
    f α vh vl α = vh ∧ ∀ q, 0 ≤ q → q ≤ 1 → f α vh vl q ≤ vh := by
  constructor
  · unfold f
    rw [if_pos le_rfl]
    have : (1 : ℝ) - α ≠ 0 := by linarith
    field_simp
  · intro q hq0 hq1
    unfold f
    split_ifs with h
    · have hd : 0 < 1 - q := by linarith
      have : (1 - α) / (1 - q) ≤ 1 := (div_le_one hd).2 (by linarith)
      nlinarith
    · have hqpos : 0 < q := by linarith [not_le.1 h]
      have : α / q ≤ 1 := (div_le_one hqpos).2 (le_of_lt (not_le.1 h))
      nlinarith

/-- "Even if all consumers purchase the fashion" the value is positive,
`f 1 = α vl > 0`. -/
theorem f_one {α vh vl : ℝ} (hα0 : 0 < α) (hα : α < 1) (hvl : 0 < vl) :
    f α vh vl 1 = α * vl ∧ 0 < f α vh vl 1 := by
  unfold f
  rw [if_neg (not_le.2 hα)]
  constructor
  · ring
  · rw [div_one]; positivity

/-- Sorting. With random matching inside a design, the gain from a pool whose
share of high types is higher by `d > 0` is `vh d` for a high type and `vl d`
for a low type. Complementarity makes the high type's gain the larger. -/
theorem single_crossing {vh vl d : ℝ} (hd : 0 < d) (hc : vl < vh) : vl * d < vh * d :=
  mul_lt_mul_of_pos_right hc hd

/-- Price screening in the static game. When the in-group is exactly the high
types, a high type gains `vh` from it and a low type `vl`. A price `p` with
`vl < p ≤ vh` keeps the high types in and the low types out, and such a price
exists exactly when complementarity holds. -/
theorem separating_price_iff {vh vl : ℝ} :
    (∃ p, vl < p ∧ p ≤ vh) ↔ vl < vh := by
  constructor
  · rintro ⟨p, h1, h2⟩; linarith
  · intro h; exact ⟨vh, h, le_rfl⟩

/-- Control: without complementarity no price separates the types. -/
theorem control_no_separation_without_complementarity {vh vl : ℝ} (h : vh ≤ vl) :
    ¬ ∃ p, vl < p ∧ p ≤ vh := by
  rintro ⟨p, h1, h2⟩; linarith

/-- Control: the bandwagon range needs `vh > 0`. With `vh = 0` the inverse
demand is flat on `[0, α]`. -/
theorem control_no_bandwagon_without_gain {α vl q₁ q₂ : ℝ}
    (h1 : q₁ ≤ α) (h2 : q₂ ≤ α) : f α 0 vl q₁ = f α 0 vl q₂ := by
  unfold f; rw [if_pos h1, if_pos h2]; ring

/-! ### Contrast with Paper A's engagement utility

Paper A's utility `U(c_i, c_j) = v − κ (c_i − c_j)²` on two classes, high `1`
and low `0`. -/

/-- Paper A's utility on two classes. -/
def U (v κ ci cj : ℝ) : ℝ := v - κ * (ci - cj) ^ 2

/-- Under Paper A's utility, complementarity (1) holds: the high class gains
`κ` from a high partner, the low class gains `−κ`. -/
theorem quadratic_complementarity {v κ : ℝ} (hκ : 0 < κ) :
    U v κ 0 1 - U v κ 0 0 < U v κ 1 1 - U v κ 1 0 := by
  unfold U; nlinarith

/-- Control: Pesendorfer's other assumption, that both types prefer a high
partner, fails under Paper A's utility. The low class prefers a low partner. -/
theorem control_quadratic_low_prefers_low {v κ : ℝ} (hκ : 0 < κ) :
    ¬ (U v κ 0 0 < U v κ 0 1) := by
  unfold U; intro h; nlinarith

/-- Under Paper A's utility on two classes, avoidance runs both ways. The high
class's expected utility rises with a pool's share `π` of high partners and
the low class's falls. -/
theorem quadratic_avoidance_both_ways {v κ π₁ π₂ : ℝ} (hκ : 0 < κ) (h : π₁ < π₂) :
    π₁ * U v κ 1 1 + (1 - π₁) * U v κ 1 0 < π₂ * U v κ 1 1 + (1 - π₂) * U v κ 1 0 ∧
      π₂ * U v κ 0 1 + (1 - π₂) * U v κ 0 0 < π₁ * U v κ 0 1 + (1 - π₁) * U v κ 0 0 := by
  unfold U
  constructor <;> nlinarith

/-- With two types, the expected squared gap to a partner drawn from a pool
with high share `π` is the squared gap to the mean plus the variance
`π (1 − π)`, and it equals `1 − π` for the high class and `π` for the low
class. The variance is fixed by the mean, so with two types the spread of a
pool cannot move independently of its mean. -/
theorem two_type_gap (π : ℝ) :
    (π - 1) ^ 2 + π * (1 - π) = 1 - π ∧ (π - 0) ^ 2 + π * (1 - π) = π := by
  constructor <;> ring

/-- Matching technology (i) inside the design. When the users are `[0, q]`
with `q ≤ α`, all users are high types and a user meets a high type with
probability 1. The chance is computed from the actual users. -/
theorem in_pool_high {q : ℝ} (hq : 0 < q) : meetHigh q 0 = 1 := by
  unfold meetHigh; rw [add_zero, div_self hq.ne']

/-- Matching technology (i) outside the design. The non-users are
`(q, 1]`, of whom `α − q` are high, so the chance is `(α − q)/(1 − q)`, the
term in eq. (2). -/
theorem out_pool_high {α q : ℝ} : meetHigh (α - q) (1 - α) = (α - q) / (1 - q) := by
  unfold meetHigh; congr 1; ring

/-- Matching technology (i) inside the design when `[0, q]` with `q > α` use
it. The chance is `α/q`, the term in eq. (3). -/
theorem in_pool_low {α q : ℝ} : meetHigh α (q - α) = α / q := by
  unfold meetHigh; congr 1; ring

/-- Proposition 1 (i), the step in its proof (PDF p. 19). As a function of
`q`, `(q − q′) f(q)` is maximised at `α` or at `1`, so in the period before an
innovation the designer sells to exactly the high types or to everyone. -/
theorem prop1_static_step {α vh vl q q' : ℝ} (hα0 : 0 < α) (hα : α < 1) (hvh : 0 ≤ vh)
    (hvl : 0 ≤ vl) (hq' : 0 ≤ q') (hqq : q' ≤ q) (hq1 : q ≤ 1) :
    (q - q') * f α vh vl q ≤ max ((α - q') * f α vh vl α) ((1 - q') * f α vh vl 1) := by
  have fα : f α vh vl α = vh := by
    unfold f; rw [if_pos le_rfl]
    have : (1 : ℝ) - α ≠ 0 := by linarith
    field_simp
  have f1 : f α vh vl 1 = vl * α := by
    unfold f; rw [if_neg (not_le.2 hα), div_one]
  rw [fα, f1]
  unfold f
  split_ifs with h
  · apply le_max_of_le_left
    have hd : 0 < 1 - q := by linarith
    have hr : (1 - α) / (1 - q) ≤ 1 := (div_le_one hd).2 (by linarith)
    have hr0 : 0 ≤ (1 - α) / (1 - q) := div_nonneg (by linarith) hd.le
    have : vh * ((1 - α) / (1 - q)) ≤ vh := by nlinarith
    calc (q - q') * (vh * ((1 - α) / (1 - q))) ≤ (q - q') * vh :=
          mul_le_mul_of_nonneg_left this (by linarith)
      _ ≤ (α - q') * vh := mul_le_mul_of_nonneg_right (by linarith) hvh
  · apply le_max_of_le_right
    have hq : α < q := not_le.1 h
    have hqp : 0 < q := by linarith
    have e : (q - q') * (vl * (α / q)) = vl * α * (1 - q' / q) := by
      field_simp
    rw [e]
    have : q' ≤ q' / q := by rw [le_div_iff₀ hqp]; nlinarith
    have hva : 0 ≤ vl * α := mul_nonneg hvl hα0.le
    nlinarith

/-- Proposition 1 (ii). If the revenue `(1 − δ^T) α f(α)` of a cycle of
`T` periods covers the innovation cost `c` (the premise the proof takes from
eqs. 9 and 10), then `T > log(1 − c/α f(α))/log δ`. Here `R = α f(α)`. -/
theorem prop1_ii {δ R c : ℝ} {T : ℕ} (hδ0 : 0 < δ) (hδ1 : δ < 1) (hR : 0 < R)
    (h : c < (1 - δ ^ T) * R) : Real.log (1 - c / R) / Real.log δ < T := by
  have hlog : Real.log δ < 0 := Real.log_neg hδ0 hδ1
  have hpow : δ ^ T < 1 - c / R := by
    rw [lt_sub_iff_add_lt, ← lt_sub_iff_add_lt', div_lt_iff₀ hR]; linarith
  have hpos : 0 < δ ^ T := pow_pos hδ0 T
  have hl := Real.log_lt_log hpos hpow
  rw [Real.log_pow] at hl
  rw [div_lt_iff_of_neg hlog]
  linarith

/-- Proposition 3 (ii). A price `v^ℓ (1 − δ^n)` with `n` periods of use left
is exactly the value to a low type of joining an all-high pool for those
periods, `(1 − δ) Σ_{s<n} δ^s v^ℓ`, so a low type gains nothing from buying
(reading PF-D2 on the index `n`). -/
theorem prop3_price (δ v : ℝ) (n : ℕ) :
    (1 - δ) * (∑ s ∈ Finset.range n, δ ^ s) * v = v * (1 - δ ^ n) := by
  have := geom_sum_mul δ n
  linear_combination (-v) * this

/-- Proposition 3, the cycle length. If `c < α v^ℓ/N`, a `T` with
`(1 − δ^{T+1}) α v^ℓ/N > c` exists, as the proof says. -/
theorem prop3_T_exists {δ α vl c N : ℝ} (hδ0 : 0 ≤ δ) (hδ1 : δ < 1) (hN : 0 < N)
    (hpos : 0 < α * vl) (hc : c < α * vl / N) :
    ∃ T : ℕ, c < (1 - δ ^ (T + 1)) * α * vl / N := by
  have hε : 0 < 1 - c * N / (α * vl) := by
    rw [sub_pos, div_lt_one hpos]; rwa [lt_div_iff₀ hN] at hc
  obtain ⟨n, hn⟩ := exists_pow_lt_of_lt_one hε hδ1
  refine ⟨n, ?_⟩
  have h1 : δ ^ (n + 1) ≤ δ ^ n := pow_le_pow_of_le_one hδ0 hδ1.le (Nat.le_succ n)
  have h2 : δ ^ (n + 1) < 1 - c * N / (α * vl) := lt_of_le_of_lt h1 hn
  rw [lt_div_iff₀ hN]
  have h3 : c * N / (α * vl) < 1 - δ ^ (n + 1) := by linarith
  rw [div_lt_iff₀ hpos] at h3
  linarith

/-- PDF p. 15. Under competition each of `N ≥ 2` designers sells to `α/N`
consumers and must cover `c`, so the price is at least `2c/α`. -/
theorem price_floor {p α c N : ℝ} (hα : 0 < α) (hN : 2 ≤ N) (hc : 0 ≤ c)
    (h : c ≤ p * α / N) : 2 * c / α ≤ p := by
  have hN0 : 0 < N := by linarith
  rw [le_div_iff₀ hN0] at h
  rw [div_le_iff₀ hα]
  nlinarith

/-- Footnote 24. With enough designers, `(v^h − v^ℓ)(1 − α) > v^ℓ/N`, the
condition under which high types switch after a large price cut. -/
theorem fn24 {vh vl α : ℝ} (hgap : 0 < (vh - vl) * (1 - α)) :
    ∃ N : ℕ, 0 < N ∧ vl / N < (vh - vl) * (1 - α) := by
  obtain ⟨N, hN⟩ := exists_nat_gt (vl / ((vh - vl) * (1 - α)))
  refine ⟨N + 1, Nat.succ_pos N, ?_⟩
  have hN1 : (0 : ℝ) < (N + 1 : ℕ) := by positivity
  rw [div_lt_iff₀ hN1]
  rw [div_lt_iff₀ hgap] at hN
  push_cast
  nlinarith

/-- Footnote 26. At `δ = 0` the monopoly price is `f(α) = v^h` and the price of
Proposition 3 is `v^ℓ`. -/
theorem fn26 {α vh vl : ℝ} (hα : α < 1) (n : ℕ) (hn : 0 < n) :
    f α vh vl α = vh ∧ vl * (1 - (0 : ℝ) ^ n) = vl := by
  constructor
  · unfold f; rw [if_pos le_rfl]
    have : (1 : ℝ) - α ≠ 0 := by linarith
    field_simp
  · rw [zero_pow hn.ne']; ring

/-- Example 1. The third price follows from eq. (9) with no resale value,
`(1 − δ) f(1/2) = 6`. -/
theorem example1_last : (1 - 9 / 10 : ℝ) * f (1 / 2) 60 10 (1 / 2) = 6 := by
  unfold f; norm_num

/-- Example 1. Eq. (9) with equality at the printed `q = 0.4135` and resale
at the printed 6 gives `10.515…`, not the printed `10.54` (reading PF-D5). -/
theorem example1_second_by_eq9 :
    (1 - 9 / 10 : ℝ) * f (1 / 2) 60 10 (4135 / 10000) + 9 / 10 * 6 ≠ 1054 / 100 := by
  unfold f; norm_num

/-- Example 2. The second price follows from eq. (9) with resale at the
printed third price 1, `(1 − δ) f(1/2) + δ · 1 = 4.9`. -/
theorem example2_second : (1 - 9 / 10 : ℝ) * f (1 / 2) 40 10 (1 / 2) + 9 / 10 * 1 = 49 / 10 := by
  unfold f; norm_num

/-- Example 2. Eq. (9) at `q = 1` with no resale gives `1/2`, not the printed
third price 1 (reading PF-D5). -/
theorem example2_last_by_eq9 : (1 - 9 / 10 : ℝ) * f (1 / 2) 40 10 1 = 1 / 2 := by
  unfold f; norm_num

/-- Control on `prop3_T_exists`: if `c ≥ α v^ℓ/N` no cycle length covers the
cost. -/
theorem control_no_T_when_costly {δ α vl c N : ℝ} (hδ0 : 0 ≤ δ) (hN : 0 < N)
    (hpos : 0 < α * vl) (hc : α * vl / N ≤ c) (T : ℕ) :
    ¬ c < (1 - δ ^ (T + 1)) * α * vl / N := by
  have hp : 0 ≤ δ ^ (T + 1) := pow_nonneg hδ0 _
  have : (1 - δ ^ (T + 1)) * α * vl / N ≤ α * vl / N := by
    apply div_le_div_of_nonneg_right _ hN.le
    nlinarith
  linarith

/-- Control on `price_floor`: with one designer the floor is only `c/α`. -/
theorem control_price_floor_one_designer {α c : ℝ} (hα : 0 < α) (hc : 0 < c) :
    c ≤ (c / α) * α / 1 ∧ c / α < 2 * c / α := by
  constructor
  · rw [div_one, div_mul_cancel₀ c hα.ne']
  · exact div_lt_div_of_pos_right (by linarith) hα

/-- Eq. (5) is linear in `ν`, so the value of a pool rises with its share of
high types for every type that prefers a high partner. Avoidance runs one way. -/
theorem pool_value_mono {δ uih uil p y ν₁ ν₂ : ℝ} (hδ : δ < 1) (hu : uil < uih) (h : ν₁ < ν₂) :
    eq5 δ ν₁ uih uil p y < eq5 δ ν₂ uih uil p y := by
  unfold eq5
  have : 0 < (1 - δ) * ((ν₂ - ν₁) * (uih - uil)) :=
    mul_pos (by linarith) (mul_pos (by linarith) (by linarith))
  nlinarith

/-- Our reading of PF-Q13. If types were observed and matching could condition
on them, a high type would meet a high type with probability 1 without the
design, and the design would be worth nothing to her. -/
theorem observed_type_no_value {vh q α : ℝ} (hq : 0 < q) (hα : 0 < α) :
    vh * (meetHigh q 0 - meetHigh α 0) = 0 := by
  rw [in_pool_high hq, in_pool_high hα]; ring

/-- Control on `observed_type_no_value`: with types unobserved the design is
worth `vh (1 − α)/(1 − q) > 0` to a high type `q < α`. -/
theorem control_unobserved_positive {vh q α : ℝ} (hvh : 0 < vh) (hα : α < 1) (hq : q < 1) :
    0 < vh * (meetHigh q 0 - meetHigh (α - q) (1 - α)) ∨ q ≤ 0 := by
  rcases le_or_gt q 0 with h | h
  · exact Or.inr h
  · left
    rw [in_pool_high h, out_pool_high, eq2 hq]
    exact mul_pos hvh (div_pos (by linarith) (by linarith))

/-- Proposition 1 (iii), the step in its proof (PDF p. 19). The designer's
value from selling only to high types, at least `α v^h − c/(1 − δ)`, exceeds
any bound on the gain from selling to low types once `v^h` is large enough. -/
theorem prop1_iii_step {α δ c : ℝ} (hα : 0 < α) (M : ℝ) :
    ∃ vh : ℝ, M < α * vh - c / (1 - δ) := by
  refine ⟨(M + c / (1 - δ) + 1) / α, ?_⟩
  rw [mul_div_cancel₀ _ hα.ne']
  linarith

end Pesendorfer1995
