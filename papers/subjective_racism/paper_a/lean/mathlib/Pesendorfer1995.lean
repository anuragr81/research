import Mathlib

/-! Pesendorfer (1995), "Design Innovation and Fashion Cycles", the static
structure of Section II that Paper A would rely on or contrast with, in the
paper's notation.

Consumers `q ∈ [0, 1]`, high types `q ≤ α`, low types `q > α`. The match
payoff is `u(i, j)`, with `vh = u(h,h) − u(h,ℓ)` and `vl = u(ℓ,h) − u(ℓ,ℓ)`.
Assumption (1), complementarity, is `vl < vh`. Within a design, partners are
drawn at random, so a consumer's match payoff is linear in the share of high
types among the users of her design. `f` is eq. (4), the one-period inverse
demand for a new design when consumers `[0, q]` use it.

The dynamic game of Sections III and IV (durable-goods pricing, innovation,
competition) is not formalised. The separating price below is the static
counterpart of the price floor in Proposition 3, not Proposition 3 itself. -/

namespace Pesendorfer1995

/-- Eq. (4). The maximum price consumer `q` pays when `[0, q]` use the design. -/
noncomputable def f (α vh vl q : ℝ) : ℝ :=
  if q ≤ α then vh * ((1 - α) / (1 - q)) else vl * (α / q)

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

/-- With two types, the expected squared gap to a partner drawn from a pool
with high share `π` is the squared gap to the mean plus the variance
`π (1 − π)`, and it equals `1 − π` for the high class and `π` for the low
class. The variance is fixed by the mean, so with two types the spread of a
pool cannot move independently of its mean. -/
theorem two_type_gap (π : ℝ) :
    (π - 1) ^ 2 + π * (1 - π) = 1 - π ∧ (π - 0) ^ 2 + π * (1 - π) = π := by
  constructor <;> ring

end Pesendorfer1995
