import Mathlib

/-! Mailath and Postlewaite, "Social Assets", PIER Working Paper 06-003
(January 5, 2006), in the paper's notation. An agent's characteristic is an
income `H` or `L` and the presence (`y`) or absence (`n`) of a heritable
attribute, and both are known when partners match. Utility is normalised by
`U(2L) = 0`, `U(2H) = 1`, `U(H + L) = u` with `u ∈ [1/2, 1)`. `β` is the
discount factor, `k` the productivity of the attribute (a `y` agent has income
`H` with probability `1/2 + k`, an `n` agent with `1/2 − k`), and values are
average discounted values. `Vy`, `Vn` are the values of having and not having
the attribute before income is realised.

Section 3 is formalised twice. First through the two incentive constraints the
paper writes down, then on the model of characteristics (`Ch`, `payoff`,
`Matching`, `Stable`, `ValueEq`, `Equilibrium`), where stability is checked
against every pair of agents. Section 5 gives the law of motion (5.1), its
limits, and Proposition 5 on our reading (MP-D4). Section 6 gives the steady
state `μ_y^A`, the ordering of purchases and the algebra of Example 2.
Section 7 gives the white-shoe example. What is not formalised is listed in
CLAIMS.md, "Coverage of the source's results". -/

namespace MailathPostlewaite2006

/-! ## Section 3.1, assortative matching -/

/-- The value equation of a `y` agent under assortative matching (p. 7). -/
def assortValueY (β k Vy : ℝ) : Prop :=
  Vy = (1/2 + k) * ((1 - β) * 1 + β * Vy) + (1/2 - k) * ((1 - β) * 0 + β * Vy)

/-- The value equation of an `n` agent under assortative matching (p. 7). -/
def assortValueN (β k Vn : ℝ) : Prop :=
  Vn = (1/2 - k) * ((1 - β) * 1 + β * Vn) + (1/2 + k) * ((1 - β) * 0 + β * Vn)

/-- `V_y^A = 1/2 + k` and `V_n^A = 1/2 − k` (p. 7). -/
theorem assort_values {β k Vy Vn : ℝ} (hβ : β < 1) (hy : assortValueY β k Vy)
    (hn : assortValueN β k Vn) : Vy = 1/2 + k ∧ Vn = 1/2 - k := by
  unfold assortValueY at hy; unfold assortValueN at hn
  have h1 : 0 < 1 - β := by linarith
  constructor
  · have : (1 - β) * (Vy - (1/2 + k)) = 0 := by linear_combination hy
    rcases mul_eq_zero.1 this with h | h
    · linarith
    · linarith
  · have : (1 - β) * (Vn - (1/2 - k)) = 0 := by linear_combination hn
    rcases mul_eq_zero.1 this with h | h
    · linarith
    · linarith

/-- An `Hn` agent strictly prefers an `Ly` partner to the prescribed `Hn`
partner (p. 8). -/
def assortHnBlocks (β u k : ℝ) : Prop :=
  1 - β + β * (1/2 - k) < (1 - β) * u + β/2 * ((1/2 + k) + (1/2 - k))

/-- An `Ly` agent strictly prefers an `Hn` partner to the prescribed `Ly`
partner (p. 8). -/
def assortLyBlocks (β u k : ℝ) : Prop :=
  β * (1/2 + k) < u * (1 - β) + β/2 * ((1/2 + k) + (1/2 - k))

/-- Proposition 1. Matching assortatively on income and attribute is stable,
that is no `Hn`, `Ly` pair blocks it, if and only if `βk ≤ (1 − u)(1 − β)` or
`u(1 − β) ≤ βk`. -/
theorem prop1 {β u k : ℝ} :
    ¬ (assortHnBlocks β u k ∧ assortLyBlocks β u k) ↔
      (β * k ≤ (1 - u) * (1 - β) ∨ u * (1 - β) ≤ β * k) := by
  unfold assortHnBlocks assortLyBlocks
  constructor
  · intro h
    by_contra hc
    push_neg at hc
    exact h ⟨by linarith [hc.1], by linarith [hc.2]⟩
  · rintro (h | h) ⟨h1, h2⟩ <;> linarith

/-! ## Section 3.2, mixed matching -/

/-- Eqs. (3.2) and (3.3), the value equations under mixed matching. -/
def mixedValues (β u k Vy Vn : ℝ) : Prop :=
  Vy = (1/2 + k) * (1 - β + β * Vy) + (1/2 - k) * (u * (1 - β) + β/2 * (Vy + Vn)) ∧
  Vn = (1/2 - k) * (u * (1 - β) + β/2 * (Vy + Vn)) + (1/2 + k) * β * Vn

/-- Eq. (3.5), the incentive constraint of an `Hn` agent. -/
def HnIC (β u Vy Vn : ℝ) : Prop := 1 - β + β * Vn ≤ u * (1 - β) + β/2 * (Vy + Vn)

/-- The incentive constraint of an `Ly` agent (p. 10). -/
def LyIC (β u Vy Vn : ℝ) : Prop := β * Vy ≤ u * (1 - β) + β/2 * (Vy + Vn)

/-- The mixed matching is stable when both incentive constraints hold, (3.6). -/
def mixedStable (β u Vy Vn : ℝ) : Prop := HnIC β u Vy Vn ∧ LyIC β u Vy Vn

lemma denom_pos {β k : ℝ} (hβ0 : 0 ≤ β) (hβ : β < 1) (_hk0 : 0 ≤ k) (hk : k < 1/2) :
    0 < 2 - β * (1 + 2 * k) := by nlinarith

/-- Eq. (3.4). `V_y^M − V_n^M = (1 + 2k)(1 − β)/(2 − β(1 + 2k))`. -/
theorem eq_3_4 {β u k Vy Vn : ℝ} (h : mixedValues β u k Vy Vn)
    (hd : 2 - β * (1 + 2 * k) ≠ 0) :
    Vy - Vn = (1 + 2 * k) * (1 - β) / (2 - β * (1 + 2 * k)) := by
  obtain ⟨h1, h2⟩ := h
  rw [eq_div_iff hd]
  linear_combination 2 * h1 - 2 * h2

/-- The printed bound of (3.7), `β(1 + 2k)/(2(2 − β(1 + 2k)))`. -/
noncomputable def bound (β k : ℝ) : ℝ := β * (1 + 2 * k) / (2 * (2 - β * (1 + 2 * k)))

/-- Proposition 2. The mixed matching is stable if and only if
`1 − u ≤ β(1 + 2k)/(2(2 − β(1 + 2k))) ≤ u`, eq. (3.7). -/
theorem prop2 {β u k Vy Vn : ℝ} (hβ0 : 0 ≤ β) (hβ : β < 1) (hk0 : 0 ≤ k) (hk : k < 1/2)
    (h : mixedValues β u k Vy Vn) :
    mixedStable β u Vy Vn ↔ (1 - u ≤ bound β k ∧ bound β k ≤ u) := by
  have hd := denom_pos hβ0 hβ hk0 hk
  have hD := eq_3_4 h hd.ne'
  have h1 : 0 < 1 - β := by linarith
  have key : β/2 * (Vy - Vn) = (1 - β) * bound β k := by
    rw [hD]; unfold bound; field_simp
  have eHn : HnIC β u Vy Vn ↔ (1 - β) * (1 - u) ≤ (1 - β) * bound β k := by
    unfold HnIC; constructor <;> intro hh <;> nlinarith [key]
  have eLy : LyIC β u Vy Vn ↔ (1 - β) * bound β k ≤ (1 - β) * u := by
    unfold LyIC; constructor <;> intro hh <;> nlinarith [key]
  unfold mixedStable
  rw [eHn, eLy, mul_le_mul_iff_of_pos_left h1, mul_le_mul_iff_of_pos_left h1]

/-! ## Section 3.3, the unproductive attribute -/

/-- Eq. (3.8). With `k = 0` the left inequality of (3.7) is
`u ≥ (4 − 3β)/(2(2 − β))`. -/
theorem eq_3_8 {β u : ℝ} (hβ : β < 1) :
    1 - u ≤ bound β 0 ↔ (4 - 3 * β) / (2 * (2 - β)) ≤ u := by
  have h2 : 0 < 2 * (2 - β) := by linarith
  unfold bound
  simp only [mul_zero, add_zero, mul_one]
  rw [le_div_iff₀ h2, div_le_iff₀ h2]
  constructor <;> intro hh <;> nlinarith

/-- With `k = 0`, the right inequality of (3.7) holds for every `u ≥ 1/2` and
`β ≤ 1` (p. 11). -/
theorem right_ineq_free {β u : ℝ} (hβ : β < 1) (hu : 1/2 ≤ u) : bound β 0 ≤ u := by
  have h2 : 0 < 2 * (2 - β) := by linarith
  unfold bound
  simp only [mul_zero, add_zero, mul_one]
  rw [div_le_iff₀ h2]; nlinarith

/-- The appendix. With `k = 0`, (3.2) and (3.3) give
`V_y^M = (4 − 3β)/(4(2 − β)) + u/2` and `V_n^M = β/(4(2 − β)) + u/2`. -/
theorem values_k0 {β u Vy Vn : ℝ} (hβ : β < 1) (h : mixedValues β u 0 Vy Vn) :
    Vy = (4 - 3 * β) / (4 * (2 - β)) + u / 2 ∧ Vn = β / (4 * (2 - β)) + u / 2 := by
  have hb : (2 : ℝ) - β ≠ 0 := by intro e; linarith
  have h4 : (4 : ℝ) * (2 - β) ≠ 0 := by intro e; linarith
  have h1b : (1 : ℝ) - β ≠ 0 := by intro e; linarith
  have hD := eq_3_4 h (by simpa using hb)
  simp only [mul_zero, add_zero, mul_one] at hD
  rw [eq_div_iff hb] at hD
  obtain ⟨-, h2⟩ := h
  simp only [add_zero, sub_zero] at h2
  have hn' : Vn * (4 * (2 - β)) = β + 2 * u * (2 - β) := by
    have e1 : (1 - β) * (Vn * (4 * (2 - β))) = (1 - β) * (β + 2 * u * (2 - β)) := by
      linear_combination 4 * (2 - β) * h2 + β * hD
    exact mul_left_cancel₀ h1b e1
  have hy' : Vy * (4 * (2 - β)) = 4 - 3 * β + 2 * u * (2 - β) := by
    linear_combination hn' + 4 * hD
  have hn : Vn = (β + 2 * u * (2 - β)) / (4 * (2 - β)) := by rw [eq_div_iff h4]; exact hn'
  have hy : Vy = (4 - 3 * β + 2 * u * (2 - β)) / (4 * (2 - β)) := by
    rw [eq_div_iff h4]; exact hy'
  constructor
  · rw [hy]; field_simp; ring
  · rw [hn]; field_simp; ring

/-- The appendix, `V_m^M`, the common value of `Hn` and `Ly` agents under mixed
matching, against `V_H^A = (2 − β)/2`. They (weakly) prefer the mixed matching
if and only if (3.8) holds. -/
theorem prop3_Hn {β u : ℝ} (hβ : β < 1) :
    (2 - β) / 2 ≤ u * (1 - β/2) + β/4 ↔ (4 - 3 * β) / (2 * (2 - β)) ≤ u := by
  have h2 : 0 < 2 * (2 - β) := by linarith
  rw [div_le_iff₀ h2]
  constructor <;> intro hh <;> nlinarith

/-- The appendix, `Ln` agents. `V_Ln^M = β V_n^M ≥ β/2 = V_Ln^A` if and only if
(3.8) holds, for `0 < β < 1`. -/
theorem prop3_Ln {β u Vy Vn : ℝ} (hβ0 : 0 < β) (hβ : β < 1)
    (h : mixedValues β u 0 Vy Vn) :
    β / 2 ≤ β * Vn ↔ (4 - 3 * β) / (2 * (2 - β)) ≤ u := by
  obtain ⟨-, hn⟩ := values_k0 hβ h
  have h2 : 0 < 2 * (2 - β) := by linarith
  have h4 : 0 < 4 * (2 - β) := by linarith
  rw [hn, div_le_iff₀ h2]
  constructor
  · intro hh
    have : 1/2 ≤ β / (4 * (2 - β)) + u / 2 := by nlinarith
    rw [div_add_div _ _ h4.ne' (by norm_num : (2:ℝ) ≠ 0), le_div_iff₀ (by positivity)] at this
    nlinarith
  · intro hh
    have : 1/2 ≤ β / (4 * (2 - β)) + u / 2 := by
      rw [div_add_div _ _ h4.ne' (by norm_num : (2:ℝ) ≠ 0), le_div_iff₀ (by positivity)]
      nlinarith
    nlinarith

/-! ## Section 5, rationing when `μ_y > 1/2`

An `Ly` agent meets an `Hn` agent with probability `α = μ_n/μ_y`, and an `Ly`
partner otherwise. The value of `n` is still (3.3) with `k = 0`. -/

/-- The value equations of p. 18 and (3.3), with `k = 0`. -/
def rationedValues (β u α Vy Vn : ℝ) : Prop :=
  Vy = 1/2 * (1 - β + β * Vy)
      + 1/2 * (α * (u * (1 - β) + β/2 * (Vy + Vn)) + (1 - α) * β * Vy) ∧
  Vn = 1/2 * (u * (1 - β) + β/2 * (Vy + Vn)) + 1/2 * β * Vn

/-- Our reading of the difference on p. 19. The value equations give
`V_y^M − V_n^M = 2(1 − β)(1 − u + αu)/(4 − 3β + αβ)`. The printed denominator is
`4 − β − αβ` (reading MP-D4). -/
theorem rationed_diff {β u α Vy Vn : ℝ} (h : rationedValues β u α Vy Vn)
    (hd : 4 - 3 * β + α * β ≠ 0) :
    Vy - Vn = 2 * (1 - β) * (1 - u + α * u) / (4 - 3 * β + α * β) := by
  obtain ⟨h1, h2⟩ := h
  rw [eq_div_iff hd]
  linear_combination 4 * h1 - 4 * h2

/-- The printed difference is not the solution of the printed value equations
whenever `0 < β`, `α < 1` and `1 − u + αu ≠ 0`, and `β < 1`. -/
theorem printed_diff_differs {β u α Vy Vn : ℝ} (hβ0 : 0 < β) (hβ : β < 1)
    (hα0 : 0 ≤ α) (hα : α < 1) (hu : 0 < 1 - u + α * u)
    (h : rationedValues β u α Vy Vn) :
    Vy - Vn ≠ 2 * (1 - β) * (1 - u + α * u) / (4 - β - α * β) := by
  have hd1 : 0 < 4 - 3 * β + α * β := by nlinarith
  have hd2 : 0 < 4 - β - α * β := by nlinarith
  rw [rationed_diff h hd1.ne']
  intro e
  rw [div_eq_div_iff hd1.ne' hd2.ne'] at e
  have hn : 0 < 2 * (1 - β) * (1 - u + α * u) := by
    have : 0 < 1 - β := by linarith
    positivity
  have : 2 * (1 - β) * (1 - u + α * u) * (2 * β * (1 - α)) = 0 := by linear_combination e
  rcases mul_eq_zero.1 this with h0 | h0
  · linarith
  · nlinarith

/-- Our reading of the `Hn` constraint (3.5) under rationing. It holds if and
only if `4 − 4β + αβ ≤ u(4 − 4β + 2αβ)`, which is
`u ≥ 1 − αβ/(2(2 − 2β + αβ))` rather than the printed `u ≥ 1 − αβ/(2(2 − β))`. -/
theorem rationed_HnIC {β u α Vy Vn : ℝ} (hβ0 : 0 ≤ β) (hβ : β < 1) (hα0 : 0 ≤ α)
    (h : rationedValues β u α Vy Vn) :
    HnIC β u Vy Vn ↔ 4 - 4 * β + α * β ≤ u * (4 - 4 * β + 2 * α * β) := by
  have hd : 0 < 4 - 3 * β + α * β := by nlinarith
  have hD := rationed_diff h hd.ne'
  have h1 : 0 < 1 - β := by linarith
  have e : HnIC β u Vy Vn ↔ (1 - β) * (1 - u) ≤ β/2 * (Vy - Vn) := by
    unfold HnIC; constructor <;> intro hh <;> nlinarith
  rw [e, hD, mul_div_assoc', le_div_iff₀ hd]
  constructor
  · intro hh
    have : (1 - β) * ((1 - u) * (4 - 3 * β + α * β)) ≤ (1 - β) * (β * (1 - u + α * u)) := by
      nlinarith
    have := le_of_mul_le_mul_left this h1
    nlinarith
  · intro hh
    nlinarith

/-- At `β = 1/2`, `α = 1/2`, `u = 91/100`, the `Hn` constraint holds on our
reading, while the printed threshold (5.2) `1 − αβ/(2(2 − β)) = 11/12` exceeds
`u`. -/
theorem rationed_threshold_example {Vy Vn : ℝ}
    (h : rationedValues (1/2) (91/100) (1/2) Vy Vn) :
    HnIC (1/2) (91/100) Vy Vn ∧ (91/100 : ℝ) < 1 - (1/2) * (1/2) / (2 * (2 - 1/2)) := by
  refine ⟨(rationed_HnIC (by norm_num) (by norm_num) (by norm_num) h).2 (by norm_num), by norm_num⟩

/-! ## Controls -/

/-- Control. Without care for descendants (`β = 0`) the `Hn` constraint fails
for every `u < 1`, so the mixed matching is not stable (p. 9). -/
theorem control_no_altruism {u Vy Vn : ℝ} (hu : u < 1) : ¬ HnIC 0 u Vy Vn := by
  unfold HnIC; intro hh; nlinarith

/-- Control. Without concavity (`u = 1/2`, linear `U`) and with an unproductive
attribute, the mixed matching is not stable for any `β < 1`. Proposition 3's
Pareto ranking needs (3.8), and (3.8) needs `u` above `1/2`. -/
theorem control_risk_neutral {β Vy Vn : ℝ} (hβ0 : 0 ≤ β) (hβ : β < 1)
    (h : mixedValues β (1/2) 0 Vy Vn) : ¬ mixedStable β (1/2) Vy Vn := by
  intro hs
  have := ((prop2 hβ0 hβ le_rfl (by norm_num) h).1 hs).1
  rw [eq_3_8 hβ] at this
  have h2 : 0 < 2 * (2 - β) := by linarith
  rw [div_le_iff₀ h2] at this
  nlinarith

/-- Control. Assortative matching is stable when the attribute is unproductive,
but not for every productive attribute. At `β = 1/2`, `u = 9/10`, `k = 3/20`,
condition (3.1) holds and an `Hn`, `Ly` pair blocks. -/
theorem control_productive_blocks :
    assortHnBlocks (1/2) (9/10) (3/20) ∧ assortLyBlocks (1/2) (9/10) (3/20) := by
  unfold assortHnBlocks assortLyBlocks; constructor <;> norm_num

/-- With `k = 0`, no pair blocks the assortative matching (p. 8). -/
theorem assort_stable_unproductive {β u : ℝ} (hβ : β < 1) (hu : u < 1) :
    ¬ (assortHnBlocks β u 0 ∧ assortLyBlocks β u 0) := by
  rw [prop1]; left; nlinarith

/-! ## The model on characteristics, Sections 2 and 3 -/


/-! ## Section 2, the model on characteristics -/

/-- Income, high or low (p. 6). -/
inductive Inc | H | L
  deriving DecidableEq

/-- The attribute, present (`y`) or absent (`n`) (p. 6). -/
inductive Attr | y | n
  deriving DecidableEq

/-- An agent's characteristic, income and attribute together (p. 7, MP-Q6).
Both are known to a prospective partner. -/
abbrev Ch := Inc × Attr

/-- Normalised utility of a couple's joint consumption, `U(2H) = 1`,
`U(2L) = 0`, `U(H + L) = u` (p. 6). -/
def flow (u : ℝ) : Inc → Inc → ℝ
  | .H, .H => 1
  | .L, .L => 0
  | _, _ => u

/-- Probability that the children have the attribute, given the parents'
attributes, with transmission probability `ρ` from a mixed couple (p. 6). -/
def trans (ρ : ℝ) : Attr → Attr → ℝ
  | .y, .y => 1
  | .n, .n => 0
  | _, _ => ρ

/-- The average discounted value of an agent of characteristic `c` matched with
`d`, when the values of having and lacking the attribute are `Vy`, `Vn`. -/
def payoff (β u ρ Vy Vn : ℝ) (c d : Ch) : ℝ :=
  (1 - β) * flow u c.1 d.1 + β * (trans ρ c.2 d.2 * Vy + (1 - trans ρ c.2 d.2) * Vn)

/-- A matching on characteristics. -/
def Matching := Ch → Ch

/-- Assortative matching, everyone matches his or her own characteristic. -/
def assortative : Matching := id

/-- Mixed matching, `Hn` matches `Ly` and the others match their own
characteristic (p. 9). -/
def mixed : Matching := fun c =>
  if c = (.H, .n) then (.L, .y) else if c = (.L, .y) then (.H, .n) else c

/-- A pair of agents with characteristics `c`, `d` blocks `m` if both strictly
gain by matching each other (p. 6, no side payments). -/
def Blocks (β u ρ Vy Vn : ℝ) (m : Matching) (c d : Ch) : Prop :=
  payoff β u ρ Vy Vn c (m c) < payoff β u ρ Vy Vn c d ∧
    payoff β u ρ Vy Vn d (m d) < payoff β u ρ Vy Vn d c

/-- Stability, no pair of agents blocks (p. 6). -/
def Stable (β u ρ Vy Vn : ℝ) (m : Matching) : Prop :=
  ∀ c d, ¬ Blocks β u ρ Vy Vn m c d

/-- The stationary value equations of the balanced case `ρ = 1/2`,
`μ_y = 1/2`: the value of each attribute is the expectation over income of the
payoff from the prescribed match (pp. 7, 9). -/
def ValueEq (β u k Vy Vn : ℝ) (m : Matching) : Prop :=
  Vy = (1/2 + k) * payoff β u (1/2) Vy Vn (.H, .y) (m (.H, .y))
      + (1/2 - k) * payoff β u (1/2) Vy Vn (.L, .y) (m (.L, .y)) ∧
  Vn = (1/2 - k) * payoff β u (1/2) Vy Vn (.H, .n) (m (.H, .n))
      + (1/2 + k) * payoff β u (1/2) Vy Vn (.L, .n) (m (.L, .n))

/-- A stationary equilibrium of the balanced case, values solving the value
equations and a stable matching (p. 6). With `ρ = 1/2` and `μ_y = 1/2` both
matchings below are feasible and leave `μ_y` unchanged (p. 6). -/
def Equilibrium (β u k Vy Vn : ℝ) (m : Matching) : Prop :=
  ValueEq β u k Vy Vn m ∧ Stable β u (1/2) Vy Vn m

/-- The value equations under mixed matching are (3.2) and (3.3). -/
theorem valueEq_mixed {β u k Vy Vn : ℝ} :
    ValueEq β u k Vy Vn mixed ↔ mixedValues β u k Vy Vn := by
  simp only [ValueEq, mixedValues, payoff, mixed, flow, trans]
  simp
  constructor <;> rintro ⟨h1, h2⟩ <;> constructor <;> linarith


/-- An `Hy` agent never gains by deviating (p. 8). -/
theorem hy_never_deviates {β u ρ Vy Vn : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hu1 : u ≤ 1)
    (hρ1 : ρ ≤ 1) (hV : Vn ≤ Vy) (d : Ch) :
    payoff β u ρ Vy Vn (.H, .y) d ≤ 1 - β + β * Vy := by
  have h1 : 0 ≤ 1 - β := by linarith
  have hb := mul_nonneg hβ0 (sub_nonneg.2 hV)
  rcases d with ⟨i, a⟩
  cases i <;> cases a <;> simp [payoff, flow, trans] <;> nlinarith [mul_nonneg h1 (sub_nonneg.2 hu1),
    mul_nonneg hb (sub_nonneg.2 hρ1), mul_nonneg hβ0 (sub_nonneg.2 hV)]

/-- Under the mixed matching, with `Vn ≤ Vy`, no pair blocks except possibly
two `Hn` agents or two `Ly` agents, so the matching is stable if and only if
the incentive constraints (3.5) and that of `Ly` hold (p. 10). -/
theorem stable_mixed_iff {β u Vy Vn : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hu1 : u ≤ 1) (hV : Vn ≤ Vy) :
    Stable β u (1/2) Vy Vn mixed ↔ mixedStable β u Vy Vn := by
  have h1 : 0 ≤ 1 - β := by linarith
  have hb := mul_nonneg hβ0 (sub_nonneg.2 hV)
  constructor
  · intro hs
    have hHn := hs (.H, .n) (.H, .n)
    have hLy := hs (.L, .y) (.L, .y)
    simp only [Blocks, payoff, mixed, flow, trans] at hHn hLy
    simp at hHn hLy
    unfold mixedStable HnIC LyIC
    constructor <;> nlinarith
  · rintro ⟨hn, hl⟩ c d ⟨b1, b2⟩
    unfold HnIC at hn; unfold LyIC at hl
    rcases c with ⟨i1, a1⟩; rcases d with ⟨i2, a2⟩
    cases i1 <;> cases a1 <;> cases i2 <;> cases a2 <;>
      simp [payoff, mixed, flow, trans] at b1 b2 <;>
      nlinarith [mul_nonneg h1 (sub_nonneg.2 hu1)]

/-- Under assortative matching, with `Vn ≤ Vy`, the only pair that can block is
an `Hn`, `Ly` pair (p. 8, "It is clear that agents with the characteristic
`Hy` never have an incentive to deviate, while `Ln` agents can never induce a
matching"). -/
theorem stable_assort_iff {β u Vy Vn : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hu1 : u ≤ 1) (hV : Vn ≤ Vy) :
    Stable β u (1/2) Vy Vn assortative ↔
      ¬ Blocks β u (1/2) Vy Vn assortative (.H, .n) (.L, .y) := by
  have h1 : 0 ≤ 1 - β := by linarith
  have hb := mul_nonneg hβ0 (sub_nonneg.2 hV)
  constructor
  · intro hs; exact hs _ _
  · intro hnb c d hcd
    obtain ⟨b1, b2⟩ := hcd
    simp only [Blocks, payoff, assortative, flow, trans, id] at hnb
    rcases c with ⟨i1, a1⟩; rcases d with ⟨i2, a2⟩
    cases i1 <;> cases a1 <;> cases i2 <;> cases a2 <;>
      simp [payoff, assortative, flow, trans] at b1 b2 <;>
      first
        | nlinarith [mul_nonneg h1 (sub_nonneg.2 hu1)]
        | exact hnb ⟨by simpa using b1, by simpa using b2⟩
        | exact hnb ⟨by simpa using b2, by simpa using b1⟩


/-- The value equations under assortative matching give `V_y^A = 1/2 + k`,
`V_n^A = 1/2 − k` (p. 7). -/
theorem valueEq_assort {β u k Vy Vn : ℝ} (hβ : β < 1)
    (h : ValueEq β u k Vy Vn assortative) : Vy = 1/2 + k ∧ Vn = 1/2 - k := by
  simp only [ValueEq, payoff, assortative, flow, trans, id] at h
  simp at h
  exact assort_values hβ (by unfold assortValueY; linarith [h.1])
    (by unfold assortValueN; linarith [h.2])

/-- Proposition 1 with stability over all pairs. Given the assortative values,
assortative matching is stable if and only if `βk ≤ (1 − u)(1 − β)` or
`u(1 − β) ≤ βk`. -/
theorem prop1_stable {β u k Vy Vn : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hu1 : u ≤ 1)
    (hk : 0 ≤ k) (h : ValueEq β u k Vy Vn assortative) :
    Stable β u (1/2) Vy Vn assortative ↔
      (β * k ≤ (1 - u) * (1 - β) ∨ u * (1 - β) ≤ β * k) := by
  obtain ⟨hy, hn⟩ := valueEq_assort hβ1 h
  rw [stable_assort_iff hβ0 hβ1 hu1 (by rw [hy, hn]; linarith), ← prop1]
  simp only [Blocks, payoff, assortative, flow, trans, id, assortHnBlocks, assortLyBlocks, hy, hn]
  constructor <;> intro hh hb <;> apply hh <;> obtain ⟨b1, b2⟩ := hb <;>
    exact ⟨by norm_num at b1 ⊢; linarith, by norm_num at b2 ⊢; linarith⟩

lemma mixed_gap {β u k Vy Vn : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hk0 : 0 ≤ k) (hk : k < 1/2)
    (h : mixedValues β u k Vy Vn) : Vn ≤ Vy := by
  have hd := denom_pos hβ0 hβ1 hk0 hk
  have hD := eq_3_4 h hd.ne'
  have : 0 ≤ (1 + 2 * k) * (1 - β) / (2 - β * (1 + 2 * k)) := by
    apply div_nonneg _ hd.le; nlinarith
  linarith

/-- Proposition 2 with stability over all pairs. Given the values (3.2),
(3.3), the mixed matching is stable if and only if (3.7). -/
theorem prop2_stable {β u k Vy Vn : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hu1 : u ≤ 1)
    (hk0 : 0 ≤ k) (hk : k < 1/2) (h : ValueEq β u k Vy Vn mixed) :
    Stable β u (1/2) Vy Vn mixed ↔ (1 - u ≤ bound β k ∧ bound β k ≤ u) := by
  rw [valueEq_mixed] at h
  rw [stable_mixed_iff hβ0 hβ1 hu1 (mixed_gap hβ0 hβ1 hk0 hk h)]
  exact prop2 hβ0 hβ1 hk0 hk h

/-- Section 3.3. With an unproductive attribute and (3.8), both the
assortative and the mixed matching are equilibria (p. 11). -/
theorem multiple_equilibria_k0 {β u VyA VnA Vy Vn : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1)
    (hu : 1/2 ≤ u) (hu1 : u < 1) (h38 : (4 - 3 * β) / (2 * (2 - β)) ≤ u)
    (hA : ValueEq β u 0 VyA VnA assortative) (hM : ValueEq β u 0 Vy Vn mixed) :
    Equilibrium β u 0 VyA VnA assortative ∧ Equilibrium β u 0 Vy Vn mixed := by
  refine ⟨⟨hA, ?_⟩, ⟨hM, ?_⟩⟩
  · rw [prop1_stable hβ0 hβ1 hu1.le le_rfl hA]; left; nlinarith
  · rw [prop2_stable hβ0 hβ1 hu1.le le_rfl (by norm_num) hM]
    exact ⟨(eq_3_8 hβ1).2 h38, right_ineq_free hβ1 hu⟩

lemma values_k0_poly {β u Vy Vn : ℝ} (hβ : β < 1) (h : mixedValues β u 0 Vy Vn) :
    Vy * (4 * (2 - β)) = 4 - 3 * β + 2 * u * (2 - β) ∧
      Vn * (4 * (2 - β)) = β + 2 * u * (2 - β) := by
  obtain ⟨hy, hn⟩ := values_k0 hβ h
  have hb : (2 : ℝ) - β ≠ 0 := by intro e; linarith
  constructor
  · rw [hy]; field_simp; ring
  · rw [hn]; field_simp; ring

/-- Proposition 3. With an unproductive attribute, the mixed matching weakly
Pareto dominates the assortative one, every characteristic weakly gains, if
and only if (3.8) holds. -/
theorem prop3 {β u VyA VnA Vy Vn : ℝ} (hβ0 : 0 < β) (hβ1 : β < 1)
    (hA : ValueEq β u 0 VyA VnA assortative) (hM : ValueEq β u 0 Vy Vn mixed) :
    (∀ c : Ch, payoff β u (1/2) VyA VnA c (assortative c) ≤ payoff β u (1/2) Vy Vn c (mixed c))
      ↔ (4 - 3 * β) / (2 * (2 - β)) ≤ u := by
  obtain ⟨hya, hna⟩ := valueEq_assort hβ1 hA
  simp only [add_zero, sub_zero] at hya hna
  rw [valueEq_mixed] at hM
  obtain ⟨py, pn⟩ := values_k0_poly hβ1 hM
  have h2 : 0 < 2 * (2 - β) := by linarith
  rw [div_le_iff₀ h2]
  constructor
  · intro hall
    have := hall (.H, .n)
    simp [payoff, assortative, mixed, flow, trans, hya, hna] at this
    nlinarith
  · intro h38 c
    rcases c with ⟨i, a⟩
    cases i <;> cases a <;> simp [payoff, assortative, mixed, flow, trans, hya, hna] <;>
      nlinarith




/-! ## Section 3.4, emergence -/

/-- Section 3.4. If `0 < k < min{(4 − 3β)/(6β), (1 − β)/β}`, some `u ∈ [1/2, 1)`
satisfies (3.7), (3.8) and (3.1) together. -/
theorem section34_config {β k : ℝ} (hβ0 : 0 < β) (hβ1 : β < 1) (hk0 : 0 < k)
    (hk1 : k < (4 - 3 * β) / (6 * β)) (hk2 : k < (1 - β) / β) :
    ∃ u, 1/2 ≤ u ∧ u < 1 ∧ (1 - u ≤ bound β k ∧ bound β k ≤ u) ∧ 1 - u ≤ bound β 0 ∧
      (1 - u < β * k / (1 - β) ∧ β * k / (1 - β) < u) := by
  have h6 : 0 < 6 * β := by linarith
  have h1b : 0 < 1 - β := by linarith
  rw [lt_div_iff₀ h6] at hk1
  rw [lt_div_iff₀ hβ0] at hk2
  have hden : 0 < 2 - β * (1 + 2 * k) := by nlinarith
  have hden0 : 0 < 2 * (2 - β * (1 + 2 * 0)) := by nlinarith
  have hbk1 : bound β k < 1 := by
    unfold bound; rw [div_lt_one (by linarith)]; nlinarith
  have hbk0 : 0 < bound β k := by unfold bound; apply div_pos <;> nlinarith
  have hb00 : 0 < bound β 0 := by unfold bound; apply div_pos <;> nlinarith
  set cc := β * k / (1 - β) with hcc
  have hc0 : 0 < cc := div_pos (by positivity) h1b
  have hc1 : cc < 1 := by rw [hcc, div_lt_one h1b]; linarith
  set ε := min (min (bound β 0) (bound β k)) (min cc (min (1 - bound β k) (min (1 - cc) (1/2)))) / 2
    with hε
  have hm0 : 0 < min (min (bound β 0) (bound β k)) (min cc (min (1 - bound β k) (min (1 - cc) (1/2)))) := by
    simp only [lt_min_iff]; refine ⟨⟨hb00, hbk0⟩, hc0, by linarith, by linarith, by norm_num⟩
  have e1 : ε ≤ bound β 0 / 2 := by
    rw [hε]; gcongr; exact (min_le_left _ _).trans (min_le_left _ _)
  have e2 : ε ≤ bound β k / 2 := by
    rw [hε]; gcongr; exact (min_le_left _ _).trans (min_le_right _ _)
  have e3 : ε ≤ cc / 2 := by
    rw [hε]; gcongr; exact (min_le_right _ _).trans (min_le_left _ _)
  have e4 : ε ≤ (1 - bound β k) / 2 := by
    rw [hε]; gcongr; exact (min_le_right _ _).trans ((min_le_right _ _).trans (min_le_left _ _))
  have e5 : ε ≤ (1 - cc) / 2 := by
    rw [hε]; gcongr
    exact (min_le_right _ _).trans ((min_le_right _ _).trans ((min_le_right _ _).trans (min_le_left _ _)))
  have e6 : ε ≤ (1/2) / 2 := by
    rw [hε]; gcongr
    exact (min_le_right _ _).trans ((min_le_right _ _).trans ((min_le_right _ _).trans (min_le_right _ _)))
  have ep : 0 < ε := by rw [hε]; positivity
  refine ⟨1 - ε, by linarith, by linarith, ⟨by linarith, by linarith⟩, by linarith, by linarith, by linarith⟩

/-- Control. If `k ≥ (1 − β)/β`, the right inequality of (3.1) fails for every
`u < 1`, so the configuration of Section 3.4 needs a small `k`. -/
theorem control_config_needs_small_k {β k u : ℝ} (hβ0 : 0 < β) (hβ1 : β < 1)
    (hk : (1 - β) / β ≤ k) (hu : u < 1) : ¬ (β * k / (1 - β) < u) := by
  have h1b : 0 < 1 - β := by linarith
  rw [div_le_iff₀ hβ0] at hk
  rw [div_lt_iff₀ h1b]; intro h; nlinarith

/-! ## Section 4, footnote 9 -/

/-- Footnote 9. If `U(αx) = a U(x) + b` with `a ≠ 0`, which covers
`U(x) = c x^γ` and `U(x) = ln x`, the normalised utility `u` of `H + L` is the
same at `(αL, αH)` as at `(L, H)`, so the incentive constraints of Section 3
are identical at the two income levels. -/
theorem crra_u_invariant (U : ℝ → ℝ) {α a b H L : ℝ} (ha : a ≠ 0)
    (hU : ∀ x, U (α * x) = a * U x + b) :
    (U (α * (H + L)) - U (α * (2 * L))) / (U (α * (2 * H)) - U (α * (2 * L)))
      = (U (H + L) - U (2 * L)) / (U (2 * H) - U (2 * L)) := by
  rw [hU, hU, hU]
  have e1 : a * U (H + L) + b - (a * U (2 * L) + b) = a * (U (H + L) - U (2 * L)) := by ring
  have e2 : a * U (2 * H) + b - (a * U (2 * L) + b) = a * (U (2 * H) - U (2 * L)) := by ring
  rw [e1, e2, mul_div_mul_left _ _ ha]

/-! ## Section 5, the law of motion (5.1) -/

/-- Eq. (5.1), the share with the attribute next period under mixed matching. -/
noncomputable def nextMu (ρ μ : ℝ) : ℝ :=
  if μ < 1/2 then μ * (1/2 + ρ) else μ + (ρ - 1/2) * (1 - μ)

/-- Eq. (5.1) for `μ_y ≥ 1/2`, from the four contributions counted on p. 18. -/
theorem law_5_1_high {ρ μ : ℝ} (h : 1/2 ≤ μ) :
    1/2 * μ + 1/2 * (1 - μ) * ρ + 1/2 * (1 - μ) * ρ + 1/2 * (μ - (1 - μ)) = nextMu ρ μ := by
  unfold nextMu; rw [if_neg (not_lt.2 h)]; ring

/-- Eq. (5.1) for `μ_y < 1/2` (p. 18). -/
theorem law_5_1_low {ρ μ : ℝ} (h : μ < 1/2) :
    μ * (1/2 + 1/2 * ρ) + 1/2 * μ * ρ = nextMu ρ μ := by
  unfold nextMu; rw [if_pos h]; ring

/-- The path of `μ_y` under mixed matching. -/
noncomputable def muSeq (ρ μ0 : ℝ) : ℕ → ℝ
  | 0 => μ0
  | t + 1 => nextMu ρ (muSeq ρ μ0 t)

/-- With balanced transmission every share is a steady state of (5.1) (p. 18). -/
theorem rho_half_steady (μ : ℝ) : nextMu (1/2) μ = μ := by
  unfold nextMu; split_ifs <;> ring

lemma low_phase {ρ m : ℝ} (hρ0 : 0 ≤ ρ) (hρ : ρ < 1/2) (hm0 : 0 ≤ m) (hm : m < 1/2) (μ0 : ℝ)
    (t0 : ℕ) (ht : muSeq ρ μ0 t0 = m) :
    ∀ s, muSeq ρ μ0 (s + t0) = m * (1/2 + ρ) ^ s := by
  intro s
  induction s with
  | zero => simpa using ht
  | succ s ih =>
    have hr0 : 0 ≤ 1/2 + ρ := by linarith
    have hr1 : 1/2 + ρ < 1 := by linarith
    have hp : (1/2 + ρ) ^ s ≤ 1 := pow_le_one₀ hr0 hr1.le
    have hlt : m * (1/2 + ρ) ^ s < 1/2 := by nlinarith [pow_nonneg hr0 s]
    rw [show s + 1 + t0 = (s + t0) + 1 by omega]
    show nextMu ρ (muSeq ρ μ0 (s + t0)) = _
    rw [ih]; unfold nextMu; rw [if_pos hlt]; ring

/-- Section 5.1. With `ρ < 1/2` and `μ_y^0 < 1`, the share with the attribute
converges to 0 under mixed matching, so `μ = 0` is the globally stable rest
point of (5.1) on `[0, 1)` (p. 20). -/
theorem rho_lt_half_tendsto {ρ μ0 : ℝ} (hρ0 : 0 ≤ ρ) (hρ : ρ < 1/2) (h0 : 0 ≤ μ0)
    (h1 : μ0 < 1) : Filter.Tendsto (muSeq ρ μ0) Filter.atTop (nhds 0) := by
  have reach : ∃ t0, 0 ≤ muSeq ρ μ0 t0 ∧ muSeq ρ μ0 t0 < 1/2 := by
    by_contra hc
    push_neg at hc
    have key : ∀ t, 1/2 ≤ muSeq ρ μ0 t ∧ 1 - muSeq ρ μ0 t = (1 - μ0) * (3/2 - ρ) ^ t := by
      intro t
      induction t with
      | zero =>
        refine ⟨?_, by simp [muSeq]⟩
        by_contra hlt; push_neg at hlt; exact absurd (hc 0 h0) (not_le.2 hlt)
      | succ t ih =>
        obtain ⟨ha, hb⟩ := ih
        have hnext : muSeq ρ μ0 (t + 1) = muSeq ρ μ0 t + (ρ - 1/2) * (1 - muSeq ρ μ0 t) := by
          show nextMu ρ _ = _; unfold nextMu; rw [if_neg (not_lt.2 ha)]
        have hle1 : muSeq ρ μ0 t ≤ 1 := by
          have : 0 ≤ (1 - μ0) * (3/2 - ρ) ^ t := by
            apply mul_nonneg <;> [linarith; exact pow_nonneg (by linarith) _]
          linarith
        have hnn : 0 ≤ muSeq ρ μ0 (t + 1) := by rw [hnext]; nlinarith
        refine ⟨hc _ hnn, ?_⟩
        rw [hnext, pow_succ]; nlinarith [hb]
    have hr : 1 < 3/2 - ρ := by linarith
    have ht : Filter.Tendsto (fun t => (1 - μ0) * (3/2 - ρ) ^ t) Filter.atTop Filter.atTop :=
      Filter.Tendsto.const_mul_atTop (by linarith) (tendsto_pow_atTop_atTop_of_one_lt hr)
    obtain ⟨t, htt⟩ := (ht.eventually_gt_atTop 1).exists
    have := key t
    linarith [this.1, this.2]
  obtain ⟨t0, ha, hb⟩ := reach
  have hseq := low_phase hρ0 hρ ha hb μ0 t0 rfl
  rw [← Filter.tendsto_add_atTop_iff_nat t0]
  have hlim : Filter.Tendsto (fun s : ℕ => muSeq ρ μ0 t0 * (1/2 + ρ) ^ s) Filter.atTop
      (nhds (muSeq ρ μ0 t0 * 0)) :=
    (tendsto_pow_atTop_nhds_zero_of_lt_one (by linarith) (by linarith)).const_mul _
  rw [mul_zero] at hlim
  exact hlim.congr (fun s => (hseq s).symm)

/-- Control. With balanced transmission the share does not move, so the
convergence to 0 needs `ρ < 1/2`. -/
theorem control_rho_half_stays (μ0 : ℝ) (t : ℕ) : muSeq (1/2) μ0 t = μ0 := by
  induction t with
  | zero => rfl
  | succ t ih => show nextMu _ _ = _; rw [ih, rho_half_steady]

lemma high_phase {ρ m : ℝ} (hρ : 1/2 < ρ) (hρ1 : ρ ≤ 1) (hm : 1/2 ≤ m) (hm1 : m ≤ 1) (μ0 : ℝ)
    (t0 : ℕ) (ht : muSeq ρ μ0 t0 = m) :
    ∀ s, 1 - muSeq ρ μ0 (s + t0) = (1 - m) * (3/2 - ρ) ^ s ∧ 1/2 ≤ muSeq ρ μ0 (s + t0) := by
  intro s
  induction s with
  | zero =>
    constructor
    · simp [ht]
    · simp only [zero_add, ht]; exact hm
  | succ s ih =>
    obtain ⟨ha, hb⟩ := ih
    have hnext : muSeq ρ μ0 (s + 1 + t0)
        = muSeq ρ μ0 (s + t0) + (ρ - 1/2) * (1 - muSeq ρ μ0 (s + t0)) := by
      rw [show s + 1 + t0 = (s + t0) + 1 by omega]
      show nextMu ρ _ = _; unfold nextMu; rw [if_neg (not_lt.2 hb)]
    have hle : muSeq ρ μ0 (s + t0) ≤ 1 := by
      have : 0 ≤ (1 - m) * (3/2 - ρ) ^ s := mul_nonneg (by linarith) (pow_nonneg (by linarith) _)
      linarith
    constructor
    · rw [hnext, pow_succ]; nlinarith [ha]
    · rw [hnext]; nlinarith

/-- Section 5.1. With `ρ > 1/2` and `μ_y^0 > 0`, the share with the attribute
converges to 1 under mixed matching, so `μ = 1` is the globally stable rest
point of (5.1) on `(0, 1]` (p. 21). -/
theorem rho_gt_half_tendsto {ρ μ0 : ℝ} (hρ : 1/2 < ρ) (hρ1 : ρ ≤ 1) (h0 : 0 < μ0)
    (h1 : μ0 ≤ 1) : Filter.Tendsto (muSeq ρ μ0) Filter.atTop (nhds 1) := by
  have reach : ∃ t0, 1/2 ≤ muSeq ρ μ0 t0 ∧ muSeq ρ μ0 t0 ≤ 1 := by
    by_contra hc
    push_neg at hc
    have key : ∀ t, muSeq ρ μ0 t < 1/2 ∧ muSeq ρ μ0 t = μ0 * (1/2 + ρ) ^ t := by
      intro t
      induction t with
      | zero =>
        refine ⟨?_, by simp [muSeq]⟩
        by_contra hge; push_neg at hge; exact absurd (hc 0 hge) (not_lt.2 (by simpa [muSeq] using h1))
      | succ t ih =>
        obtain ⟨ha, hb⟩ := ih
        have hnext : muSeq ρ μ0 (t + 1) = muSeq ρ μ0 t * (1/2 + ρ) := by
          show nextMu ρ _ = _; unfold nextMu; rw [if_pos ha]
        have hpos : 0 ≤ muSeq ρ μ0 t := by rw [hb]; positivity
        have hle1 : muSeq ρ μ0 (t + 1) ≤ 1 := by rw [hnext]; nlinarith
        refine ⟨?_, by rw [hnext, hb, pow_succ]; ring⟩
        by_contra hge; push_neg at hge; exact absurd (hc _ hge) (not_lt.2 hle1)
    have hr : 1 < 1/2 + ρ := by linarith
    have ht : Filter.Tendsto (fun t => μ0 * (1/2 + ρ) ^ t) Filter.atTop Filter.atTop :=
      Filter.Tendsto.const_mul_atTop h0 (tendsto_pow_atTop_atTop_of_one_lt hr)
    obtain ⟨t, htt⟩ := (ht.eventually_gt_atTop 1).exists
    have := key t
    linarith [this.1, this.2]
  obtain ⟨t0, ha, hb⟩ := reach
  have hseq := high_phase hρ hρ1 ha hb μ0 t0 rfl
  rw [← Filter.tendsto_add_atTop_iff_nat t0]
  have hlim : Filter.Tendsto (fun s : ℕ => 1 - (1 - muSeq ρ μ0 t0) * (3/2 - ρ) ^ s) Filter.atTop
      (nhds (1 - (1 - muSeq ρ μ0 t0) * 0)) :=
    tendsto_const_nhds.sub
      ((tendsto_pow_atTop_nhds_zero_of_lt_one (by linarith) (by linarith)).const_mul _)
  rw [mul_zero, sub_zero] at hlim
  exact hlim.congr (fun s => by have := (hseq s).1; linarith)

/-! ## Section 5, Proposition 5 -/

/-- The value equations when `μ_y < 1/2`: `Ly` is not rationed, and an `Hn`
agent meets an `Ly` agent with probability `γ = μ_y/μ_n` and another `Hn`
otherwise (p. 19, "a similar calculation"). Our reconstruction. -/
def lowValues (β u γ Vy Vn : ℝ) : Prop :=
  Vy = 1/2 * (1 - β + β * Vy) + 1/2 * (u * (1 - β) + β/2 * (Vy + Vn)) ∧
  Vn = 1/2 * (γ * (u * (1 - β) + β/2 * (Vy + Vn)) + (1 - γ) * (1 - β + β * Vn))
      + 1/2 * β * Vn

/-- Proposition 5, `μ_y < 1/2`. The mixed matching is stable if and only if
(3.8) holds, whatever the rationing of `Hn`. -/
theorem prop5_low {β u γ Vy Vn : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hγ0 : 0 ≤ γ)
    (hu : 1/2 ≤ u) (h : lowValues β u γ Vy Vn) :
    mixedStable β u Vy Vn ↔ 1 - u ≤ bound β 0 := by
  obtain ⟨h1, h2⟩ := h
  set X := β/2 * (Vy - Vn) - (1 - u) * (1 - β) with hX
  have hrel : 2 * (Vy - Vn) = (1 - β) + β * (Vy - Vn) + (1 - γ) * X := by
    rw [hX]; linear_combination 2 * h1 - 2 * h2
  have hc : 0 < 2 * (2 - β) - β * (1 - γ) := by nlinarith
  have hXc : X * (2 * (2 - β) - β * (1 - γ)) = (1 - β) * (β - 2 * (2 - β) * (1 - u)) := by
    rw [hX] at hrel ⊢; linear_combination β * hrel
  have h1b : 0 < 1 - β := by linarith
  have h2b : 0 < 2 * (2 - β) := by linarith
  have eHn : HnIC β u Vy Vn ↔ 0 ≤ X := by unfold HnIC; rw [hX]; constructor <;> intro <;> linarith
  have eLy : LyIC β u Vy Vn ↔ X ≤ (2 * u - 1) * (1 - β) := by
    unfold LyIC; rw [hX]; constructor <;> intro <;> linarith
  have eB : 1 - u ≤ bound β 0 ↔ 0 ≤ β - 2 * (2 - β) * (1 - u) := by
    unfold bound; simp only [mul_zero, add_zero, mul_one]
    rw [le_div_iff₀ h2b]; constructor <;> intro <;> linarith
  have hX0 : 0 ≤ X ↔ 0 ≤ β - 2 * (2 - β) * (1 - u) := by
    constructor
    · intro hx
      have : 0 ≤ (1 - β) * (β - 2 * (2 - β) * (1 - u)) := by rw [← hXc]; positivity
      by_contra hneg; push_neg at hneg
      have := mul_neg_of_pos_of_neg h1b hneg
      linarith
    · intro hb
      by_contra hneg; push_neg at hneg
      have : X * (2 * (2 - β) - β * (1 - γ)) < 0 := mul_neg_of_neg_of_pos hneg hc
      nlinarith
  have hLyAlways : X ≤ (2 * u - 1) * (1 - β) := by
    by_contra hneg; push_neg at hneg
    have : (2 * u - 1) * (1 - β) * (2 * (2 - β) - β * (1 - γ)) < X * (2 * (2 - β) - β * (1 - γ)) :=
      mul_lt_mul_of_pos_right hneg hc
    rw [hXc] at this
    have key : 0 ≤ (2 * u - 1) * (2 * (2 - β) - β * (1 - γ)) - (β - 2 * (2 - β) * (1 - u)) := by
      nlinarith [mul_nonneg (mul_nonneg (by linarith : (0:ℝ) ≤ 2 * u - 1) hβ0) hγ0,
        mul_nonneg (by linarith : (0:ℝ) ≤ u) h1b.le]
    nlinarith [mul_nonneg h1b.le key]
  unfold mixedStable
  rw [eHn, eLy, eB, hX0]
  exact ⟨fun hh => hh.1, fun hh => ⟨hh, hLyAlways⟩⟩

/-- Control for Proposition 5. Without concavity (`u = 1/2`) the mixed matching
is not stable when `μ_y < 1/2`, for any `β < 1`. -/
theorem control_prop5_risk_neutral {β γ Vy Vn : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hγ0 : 0 ≤ γ)
    (h : lowValues β (1/2) γ Vy Vn) : ¬ mixedStable β (1/2) Vy Vn := by
  rw [prop5_low hβ0 hβ1 hγ0 le_rfl h]
  have h2b : 0 < 2 * (2 - β * (1 + 2 * 0)) := by linarith
  unfold bound; rw [le_div_iff₀ h2b]; intro hh; nlinarith

/-- Under rationing of `Ly` (`μ_y > 1/2`), an `Ly` agent always wants an `Hn`
partner (p. 19, MP-Q12), for `u ≥ 1/2`. -/
theorem rationed_LyIC {β u α Vy Vn : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hα0 : 0 ≤ α)
    (hu : 1/2 ≤ u) (h : rationedValues β u α Vy Vn) : LyIC β u Vy Vn := by
  have hd : 0 < 4 - 3 * β + α * β := by nlinarith
  have hD := rationed_diff h hd.ne'
  have key : β/2 * (Vy - Vn) ≤ u * (1 - β) := by
    rw [hD, mul_div_assoc', div_le_iff₀ hd]
    have h1b : 0 ≤ 1 - β := by linarith
    nlinarith [mul_nonneg h1b (mul_nonneg hα0 hβ0), mul_nonneg h1b hβ0,
      mul_nonneg h1b (mul_nonneg (by linarith : (0:ℝ) ≤ u - 1/2) (by linarith : (0:ℝ) ≤ 2 - β))]
  unfold LyIC; linarith

/-- Proposition 5, `μ_y > 1/2`, on our reading (MP-D4). The mixed matching is
stable if and only if `4 − 4β + αβ ≤ u(4 − 4β + 2αβ)`. -/
theorem prop5_high {β u α Vy Vn : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β < 1) (hα0 : 0 ≤ α)
    (hu : 1/2 ≤ u) (h : rationedValues β u α Vy Vn) :
    mixedStable β u Vy Vn ↔ 4 - 4 * β + α * β ≤ u * (4 - 4 * β + 2 * α * β) := by
  unfold mixedStable
  rw [rationed_HnIC hβ0 hβ1 hα0 h]
  exact ⟨fun hh => hh.1, fun hh => ⟨hh, rationed_LyIC hβ0 hβ1 hα0 hu h⟩⟩

/-- The step of Proposition 7 and of Section 5's discussion that is
formalised. When an `Ly` agent meets an `Hn` agent with small enough
probability `α`, the `Hn` constraint fails for every `u < 1`. -/
theorem prop7_core {β u α Vy Vn : ℝ} (hβ0 : 0 < β) (hβ1 : β < 1) (hα0 : 0 ≤ α)
    (hu1 : u < 1) (hα : α * β < (1 - u) * (4 - 4 * β))
    (h : rationedValues β u α Vy Vn) : ¬ HnIC β u Vy Vn := by
  rw [rationed_HnIC hβ0.le hβ1 hα0 h]
  intro hh
  nlinarith [mul_nonneg hα0 hβ0.le]

/-- Control for `prop7_core`. Without rationing (`α = 1`) the `Hn` constraint
holds at `β = 1/2`, `u = 9/10`. -/
theorem control_prop7_no_rationing {Vy Vn : ℝ} (h : rationedValues (1/2) (9/10) 1 Vy Vn) :
    HnIC (1/2) (9/10) Vy Vn :=
  (rationed_HnIC (by norm_num) (by norm_num) (by norm_num) h).2 (by norm_num)

/-! ## Section 6, endogenous attribute -/

/-- The share with the attribute next period under assortative matching with
purchases (p. 23). A `y` couple's child inherits with probability `2p` and is
otherwise bought the attribute with probability `q`; an `n` couple's child
only through purchase. Our reconstruction of the law behind `μ_y^A`. -/
noncomputable def nextMuA (k p qHH qLL μ : ℝ) : ℝ :=
  μ * ((1/2 + k) * (2 * p + (1 - 2 * p) * qHH) + (1/2 - k) * (2 * p + (1 - 2 * p) * qLL))
    + (1 - μ) * ((1/2 - k) * qHH + (1/2 + k) * qLL)

/-- The steady state `μ_y^A` printed on p. 24 (printed 23) is the fixed point
of `nextMuA`. -/
theorem steady_state_assort {k p qHH qLL μ : ℝ}
    (hden : 2 * ((1 - 2 * p) * (1 - qLL) + qHH - (1 + 2 * k) * (1 - p) * (qHH - qLL)) ≠ 0)
    (h : nextMuA k p qHH qLL μ = μ) :
    μ = (qHH * (1 - 2 * k) + qLL * (1 + 2 * k))
      / (2 * ((1 - 2 * p) * (1 - qLL) + qHH - (1 + 2 * k) * (1 - p) * (qHH - qLL))) := by
  rw [eq_div_iff hden]
  unfold nextMuA at h
  linear_combination (-2) * h

/-- Example 1, the assortative part. With no purchases (`k = 0` makes the
marginal benefit 0 and `c'(0) = 0` makes the optimal `q` 0) and `p < 1/2`,
the only steady state is `μ_y^A = 0`. -/
theorem example1_muA_zero {k p μ : ℝ} (hp : p < 1/2) (h : nextMuA k p 0 0 μ = μ) : μ = 0 := by
  unfold nextMuA at h
  have : μ * (1 - 2 * p) = 0 := by linear_combination -h
  rcases mul_eq_zero.1 this with h0 | h0
  · exact h0
  · linarith

/-- p. 23, "1 > q_HH > q_HL > q_LL > 0", the ordering part. If `U'` is
positive and strictly decreasing, `c` and `c'` are monotone, and two couples'
choices satisfy the first-order condition `U'(ℓ − c(q)) c'(q) = M` with the
same `M > 0`, the richer couple buys more. -/
theorem q_order (Up c c' : ℝ → ℝ) (hU : StrictAnti Up) (hUpos : ∀ x, 0 < Up x)
    (hc : Monotone c) (hc' : Monotone c') {M ℓ1 ℓ2 q1 q2 : ℝ} (hM : 0 < M) (hℓ : ℓ2 < ℓ1)
    (h1 : Up (ℓ1 - c q1) * c' q1 = M) (h2 : Up (ℓ2 - c q2) * c' q2 = M) : q2 < q1 := by
  by_contra hle; push_neg at hle
  have hcc : c q1 ≤ c q2 := hc hle
  have hU12 : Up (ℓ1 - c q1) < Up (ℓ2 - c q2) := hU (by linarith)
  have hc'12 : c' q1 ≤ c' q2 := hc' hle
  have hc'1 : 0 < c' q1 := by
    by_contra hn; push_neg at hn
    have := mul_nonpos_of_nonneg_of_nonpos (hUpos (ℓ1 - c q1)).le hn
    linarith
  have : Up (ℓ1 - c q1) * c' q1 < Up (ℓ2 - c q2) * c' q2 := by
    calc Up (ℓ1 - c q1) * c' q1 ≤ Up (ℓ1 - c q1) * c' q2 :=
          mul_le_mul_of_nonneg_left hc'12 (hUpos _).le
      _ < Up (ℓ2 - c q2) * c' q2 := mul_lt_mul_of_pos_right hU12 (by linarith)
  linarith

/-- p. 23, the positivity part. With `c'(0) = 0` and `M > 0`, the optimal `q`
is not 0. -/
theorem q_pos (Up c c' : ℝ → ℝ) (hc'0 : c' 0 = 0) {M ℓ q : ℝ} (hM : 0 < M) (hq : 0 ≤ q)
    (h : Up (ℓ - c q) * c' q = M) : 0 < q := by
  rcases hq.lt_or_eq with hq | hq
  · exact hq
  · subst hq; rw [hc'0, mul_zero] at h; linarith

/-- Control for `q_order`. With `U'` constant (linear `U`, not strictly
concave) two couples of different income can choose the same `q`. -/
theorem control_q_order_linear :
    ∃ ℓ1 ℓ2 q : ℝ, ℓ2 < ℓ1 ∧ (fun _ : ℝ => (1 : ℝ)) (ℓ1 - q ^ 2) * (2 * q) = 1 ∧
      (fun _ : ℝ => (1 : ℝ)) (ℓ2 - q ^ 2) * (2 * q) = 1 :=
  ⟨1, 0, 1/2, by norm_num, by norm_num, by norm_num⟩

/-- Eq. (6.3) is (6.1) rearranged, with `p_HL = p + (1 − p) q_HL`. -/
theorem eq_6_3 {β p Vy Vn uHL uHLc uHHc qHL qHH : ℝ} :
    p * ((1 - β) * uHL + β * Vy) + (1 - p) * ((1 - β) * uHLc + β * (qHL * Vy + (1 - qHL) * Vn))
        ≥ (1 - β) * uHHc + β * (qHH * Vy + (1 - qHH) * Vn) ↔
      β * ((p + (1 - p) * qHL) - qHH) * (Vy - Vn)
        ≥ (1 - β) * (uHHc - (p * uHL + (1 - p) * uHLc)) := by
  constructor <;> intro hh <;> linarith

/-- Eq. (6.4) is (6.2) rearranged, with `p_LL = 2p + (1 − 2p) q_LL`. -/
theorem eq_6_4 {β p Vy Vn uHL uHLc uLL uLLc qHL qLL : ℝ} :
    p * ((1 - β) * uHL + β * Vy) + (1 - p) * ((1 - β) * uHLc + β * (qHL * Vy + (1 - qHL) * Vn))
        ≥ 2 * p * ((1 - β) * uLL + β * Vy)
          + (1 - 2 * p) * ((1 - β) * uLLc + β * (qLL * Vy + (1 - qLL) * Vn)) ↔
      β * ((p + (1 - p) * qHL) - (2 * p + (1 - 2 * p) * qLL)) * (Vy - Vn)
        ≥ (1 - β) * ((2 * p * uLL + (1 - 2 * p) * uLLc) - (p * uHL + (1 - p) * uHLc)) := by
  constructor <;> intro hh <;> linarith

/-- Example 2, the two gaps (p. 27). Under (6.5) and `q_LL = 0`,
`p_HL − p_LL = (1 − p) q_HL − p > 0` and `p_HL − q_HH > ε`. -/
theorem example2_gaps {p qHL qHH ε : ℝ} (hp : p < 1) (h1 : p / (1 - p) < qHL)
    (h2 : qHH < 2 * p - ε) :
    (p + (1 - p) * qHL) - (2 * p + (1 - 2 * p) * 0) = (1 - p) * qHL - p ∧
      0 < (1 - p) * qHL - p ∧ ε < (p + (1 - p) * qHL) - qHH := by
  have h1p : 0 < 1 - p := by linarith
  rw [div_lt_iff₀ h1p] at h1
  refine ⟨by ring, by linarith, by nlinarith⟩

/-- Example 2, the decomposition of `V_y − V_n` (p. 28, printed 27). -/
theorem eq_decomp (k VHy VLy VHn VLn : ℝ) :
    ((1/2 + k) * VHy + (1/2 - k) * VLy) - ((1/2 - k) * VHn + (1/2 + k) * VLn)
      = 1/2 * ((VHy - VHn) + (VLy - VLn)) + k * ((VHy - VLy) + (VHn - VLn)) := by ring

/-- Proof of Proposition 8, `Φ₁` for `μ_y ≥ 1/2` (p. 35, printed 34). The
expression counted by matched pairs simplifies as printed. -/
theorem phi1_high {k p qHH qHL qLL μ : ℝ} (hμ : μ ≠ 0) :
    μ * ((1/2 + k) * (2 * p + (1 - 2 * p) * qHH)
        + (1/2 - k) * ((1 - μ) / μ * (p + (1 - p) * qHL)
          + (2 * μ - 1) / μ * (2 * p + (1 - 2 * p) * qLL)))
      + (1 - μ) * ((1/2 - k) * (p + (1 - p) * qHL) + (1/2 + k) * qLL)
    = μ * (1/2 + k) * (2 * p + (1 - 2 * p) * qHH) + (1 - 2 * k) * (1 - μ) * (p + (1 - p) * qHL)
      + (1/2 - k) * (2 * μ - 1) * (2 * p + (1 - 2 * p) * qLL) + (1 - μ) * (1/2 + k) * qLL := by
  field_simp
  ring

/-- Proof of Proposition 8, `Φ₁` for `μ_y < 1/2` (p. 35, printed 34). -/
theorem phi1_low {k p qHH qHL qLL μ : ℝ} (hμ : 1 - μ ≠ 0) :
    μ * ((1/2 + k) * (2 * p + (1 - 2 * p) * qHH) + (1/2 - k) * (p + (1 - p) * qHL))
      + (1 - μ) * ((1/2 - k) * (μ / (1 - μ) * (p + (1 - p) * qHL)
          + (1 - 2 * μ) / (1 - μ) * qHH) + (1/2 + k) * qLL)
    = μ * (1/2 + k) * (2 * p + (1 - 2 * p) * qHH) + (1 - 2 * k) * μ * (p + (1 - p) * qHL)
      + (1/2 - k) * (1 - 2 * μ) * qHH + (1 - μ) * (1/2 + k) * qLL := by
  field_simp
  ring

/-! ## Section 7, the white-shoe lawyers -/

/-- Section 7, point 3. With `u(c₁, c₂) = c₁ c₂` and wage `w = α < 2`, a young
lawyer of index `i` weakly prefers the apprenticeship, utility `α(2 − α + i)`,
to practice on his own, utility `2i`, if and only if `i ≤ α` (p. 30). -/
theorem lawyer_accept {α i : ℝ} (hα : α < 2) : 2 * i ≤ α * (2 - α + i) ↔ i ≤ α := by
  constructor <;> intro h <;> nlinarith

/-- Section 7, point 3. At wage 2 every young lawyer is indifferent, so the
social skills have no value (p. 30). -/
theorem lawyer_wage_two (i : ℝ) : 2 * (2 - 2 + i) = 2 * i := by ring

end MailathPostlewaite2006
