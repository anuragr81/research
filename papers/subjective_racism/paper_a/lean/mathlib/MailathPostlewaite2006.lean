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

What is formalised is the balanced case `ρ = 1/2`, `μ_y = 1/2` of Section 3
(the value equations (3.2), (3.3), the difference (3.4), Propositions 1 and 2,
inequality (3.8) and the appendix computation behind Proposition 3), and the
rationed case `μ_y > 1/2` of Section 5, where our reading differs from the
printed difference and threshold (5.2). Sections 4 and 6 are not formalised. -/

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

end MailathPostlewaite2006
