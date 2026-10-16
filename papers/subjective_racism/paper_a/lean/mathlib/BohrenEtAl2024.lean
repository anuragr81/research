import Mathlib

/-! Bohren, Hull and Imas, "Systemic Discrimination: Theory and Measurement",
NBER working paper 29820, March 2022, revised December 2024, Sections 3.1,
3.2 and 4.1, in the paper's notation. We fix a qualification level `y⁰` and
take a finite signal space `𝒮`, so that the group-`g` signal distribution
`σ*(y⁰, g)` is a weight `sig g s` on each signal and the action rule is
`A g s = A*(g, s)`. The paper allows general measurable spaces; the finite
case is ours and keeps the integrals as sums. -/

namespace BohrenEtAl2024

open Finset Filter Topology

/-- The two groups `{b, w}`. -/
inductive Grp | w | b
  deriving DecidableEq

/-- The other group `g'`. -/
def other : Grp → Grp
  | .w => .b
  | .b => .w

variable {S : Type*} [Fintype S]

/-- `μ*(y⁰, g)`, the average action for group `g`, from eq. (1). -/
def mu (sig A : Grp → S → ℝ) (g : Grp) : ℝ := ∑ s, sig g s * A g s

/-- `μ̃*(y⁰, g)`, the counterfactual average action, eq. (2). Group `g`'s
signal distribution under group `g'`'s action rule. -/
def muT (sig A : Grp → S → ℝ) (g : Grp) : ℝ := ∑ s, sig g s * A (other g) s

/-- Definition 2. Total discrimination `Δᵀ(y⁰)`. -/
def total (sig A : Grp → S → ℝ) : ℝ := mu sig A .w - mu sig A .b

/-- Definition 3. Systemic discrimination under the group `w` action rule,
`Δˢ(y⁰, w) = μ*(y⁰, w) − μ̃*(y⁰, b)`. -/
def sysW (sig A : Grp → S → ℝ) : ℝ := mu sig A .w - muT sig A .b

/-- Definition 3. Systemic discrimination under the group `b` action rule,
`Δˢ(y⁰, b) = μ̃*(y⁰, w) − μ*(y⁰, b)`. -/
def sysB (sig A : Grp → S → ℝ) : ℝ := muT sig A .w - mu sig A .b

/-- Eq. (3). Average direct discrimination with respect to the group-`g`
signal distribution, with `δ(s) = A*(w, s) − A*(b, s)` from Definition 1. -/
def direct (sig A : Grp → S → ℝ) (g : Grp) : ℝ := ∑ s, sig g s * (A .w s - A .b s)

/-- Proposition 1, eq. (4). `Δᵀ(y⁰) = Δᴰ(y⁰, w) + Δˢ(y⁰, b)`. -/
theorem prop1 (sig A : Grp → S → ℝ) : total sig A = direct sig A .w + sysB sig A := by
  simp only [total, direct, sysB, muT, mu, other, mul_sub, sum_sub_distrib]; ring

/-- Eq. (5). `Δᵀ(y⁰) = Δᴰ(y⁰, b) + Δˢ(y⁰, w)`. -/
theorem eq5 (sig A : Grp → S → ℝ) : total sig A = direct sig A .b + sysW sig A := by
  simp only [total, direct, sysW, muT, mu, other, mul_sub, sum_sub_distrib]; ring

/-- Eq. (6), the average of eqs. (4) and (5). -/
theorem eq6 (sig A : Grp → S → ℝ) :
    total sig A = (direct sig A .w + direct sig A .b) / 2 + (sysW sig A + sysB sig A) / 2 := by
  have h4 := prop1 sig A; have h5 := eq5 sig A; linarith

/-- Footnote 26. Under an action rule that is the same for both groups
(facially neutral), average direct discrimination is zero and total
discrimination is systemic. -/
theorem neutral_rule_all_systemic (sig A : Grp → S → ℝ) (h : A .w = A .b) :
    direct sig A .w = 0 ∧ direct sig A .b = 0 ∧ total sig A = sysB sig A ∧
      total sig A = sysW sig A := by
  have hd : ∀ g, direct sig A g = 0 := by intro g; simp [direct, h]
  refine ⟨hd _, hd _, ?_, ?_⟩
  · rw [prop1, hd]; ring
  · rw [eq5, hd]; ring

/-- When the two groups have the same signal distribution at the reference
qualification, systemic discrimination is zero under either rule, and total
discrimination is average direct discrimination. This is the case of an
observer who sees nothing about the individual beyond the group (the signal
does not differ by group), and the case in which the reference qualification
is itself the signal shown. -/
theorem same_signal_all_direct (sig A : Grp → S → ℝ) (h : sig .w = sig .b) :
    sysW sig A = 0 ∧ sysB sig A = 0 ∧ total sig A = direct sig A .w := by
  have hw : sysW sig A = 0 := by simp [sysW, mu, muT, other, h]
  have hb : sysB sig A = 0 := by simp [sysB, mu, muT, other, h]
  refine ⟨hw, hb, ?_⟩
  rw [prop1, hb]; ring

/-- A group-neutral rule applied to a signal whose distribution at the
reference qualification is the same for both groups produces no total
discrimination at that qualification. A screen on class alone, with class as
the reference qualification, leaves no gap in the paper's sense, whatever
the groups' class distributions. -/
theorem neutral_rule_same_signal_no_gap (sig A : Grp → S → ℝ) (hA : A .w = A .b)
    (hs : sig .w = sig .b) : total sig A = 0 := by
  simp [total, mu, hA, hs]

/-! ### The two-node subsystem of Section 4.1

With `Y⁰ = S¹`, a deterministic initial-node rule `A1 g y⁰` and focal signal
`S*(a¹, y⁰)`, eqs. (7) and (8) give `μ*(y⁰, g) = A*(g, S*(A¹(g, y⁰), y⁰))` and
`μ̃*(y⁰, g) = A*(g', S*(A¹(g, y⁰), y⁰))`. -/

/-- Eqs. (7) and (8): `Δˢ(y⁰, b) = A*(b, S*(A¹(w, y⁰), y⁰)) − A*(b, S*(A¹(b, y⁰), y⁰))`. -/
def sysB2 {Y A1sp Ssp : Type*} (Afoc : Grp → Ssp → ℝ) (Sfoc : A1sp → Y → Ssp)
    (A1 : Grp → Y → A1sp) (y : Y) : ℝ :=
  Afoc .b (Sfoc (A1 .w y) y) - Afoc .b (Sfoc (A1 .b y) y)

/-- Without direct discrimination at the initial node, there is no systemic
discrimination at the focal node in the two-node subsystem. -/
theorem no_initial_direct_no_systemic {Y A1sp Ssp : Type*} (Afoc : Grp → Ssp → ℝ)
    (Sfoc : A1sp → Y → Ssp) (A1 : Grp → Y → A1sp) (y : Y) (h : A1 .w y = A1 .b y) :
    sysB2 Afoc Sfoc A1 y = 0 := by
  simp [sysB2, h]

/-! ### Controls -/

/-- The example of the controls. Two signals, group `w` always draws signal 0
and group `b` signal 1. -/
def sig0 : Grp → Fin 2 → ℝ
  | .w => ![1, 0]
  | .b => ![0, 1]

/-- Control: eq. (4) pairs direct discrimination under the `w` signal
distribution with systemic discrimination under the `b` action rule. Pairing
it with systemic discrimination under the `w` rule fails. With the rule
`A(w, ·) = (1, 0)`, `A(b, ·) = (0, 0)`, total discrimination is 1 while
`Δᴰ(y⁰, w) + Δˢ(y⁰, w) = 2`. -/
theorem control_pairing_matters :
    let A : Grp → Fin 2 → ℝ := fun g => match g with | .w => ![1, 0] | .b => ![0, 0]
    total sig0 A = 1 ∧ direct sig0 A .w + sysW sig0 A = 2 := by
  simp [total, direct, sysW, mu, muT, other, sig0, Fin.sum_univ_two]
  norm_num

/-- Control: a conventional audit of the focal node measures only direct
discrimination. With the same rule for both groups but different signal
distributions, direct discrimination is zero while total discrimination is 1,
so total equals direct only when the signal distributions coincide
(`same_signal_all_direct`). -/
theorem control_audit_misses_systemic :
    let A : Grp → Fin 2 → ℝ := fun _ => ![1, 0]
    direct sig0 A .w = 0 ∧ direct sig0 A .b = 0 ∧ total sig0 A = 1 := by
  simp [total, direct, mu, sig0, Fin.sum_univ_two]

/-! ### Section 3.3, sources of systemic discrimination -/

/-- Section 3.3, "signal inflation ... can be offset via 'reverse' direct
discrimination in the action rule". Group `b`'s signal is one step below group
`w`'s at the same qualification. A rule that credits group `b` one step makes
total discrimination zero, with direct and systemic discrimination of equal
size and opposite sign. -/
theorem signal_inflation_offset :
    let A : Grp → Fin 2 → ℝ := fun g => match g with | .w => ![1, 0] | .b => ![0, 1]
    total sig0 A = 0 ∧ direct sig0 A .w = 1 ∧ sysB sig0 A = -1 := by
  simp [total, direct, sysB, mu, muT, other, sig0, Fin.sum_univ_two]

/-! ### Section 4, the two-node subsystem and the Iterated Audit -/

/-- Eqs. (7) and (8): total discrimination in the two-node subsystem. -/
def total2 {Y A1sp Ssp : Type*} (Afoc : Grp → Ssp → ℝ) (Sfoc : A1sp → Y → Ssp)
    (A1 : Grp → Y → A1sp) (y : Y) : ℝ :=
  Afoc .w (Sfoc (A1 .w y) y) - Afoc .b (Sfoc (A1 .b y) y)

/-- Section 4.1. Total discrimination is the treatment component at the focal
node, `A*(w, s) − A*(b, s)` at `s = S*(A¹(w, y⁰), y⁰)`, plus the systemic term
of eqs. (7) and (8), which is where the interaction component enters. -/
theorem two_node_decomp {Y A1sp Ssp : Type*} (Afoc : Grp → Ssp → ℝ) (Sfoc : A1sp → Y → Ssp)
    (A1 : Grp → Y → A1sp) (y : Y) :
    total2 Afoc Sfoc A1 y
      = (Afoc .w (Sfoc (A1 .w y) y) - Afoc .b (Sfoc (A1 .w y) y)) + sysB2 Afoc Sfoc A1 y := by
  unfold total2 sysB2; ring

/-- Section 4.3 and Figure 4, the experimental Iterated Audit. With average
outcomes `A` (endogenous-m), `B` (exogenous-f) and `C` (endogenous-f), direct
discrimination is `A − B`, systemic discrimination is `B − C`, and they sum to
total discrimination `A − C`. -/
theorem experimental_ia (a b c : ℝ) : (a - b) + (b - c) = a - c := by ring

/-- Footnote 44. With independent offers of probability `p` at each of `J`
openings, the probability of gaining experience is `1 − (1 − p)^J`. -/
def pExp (p : ℝ) (J : ℕ) : ℝ := 1 - (1 - p) ^ J

/-- Footnote 44 and Section 5.2, thin and thick markets. The gap in experience
between groups is zero with no openings, positive with one opening, and tends
to zero as openings grow, so first-round direct discrimination moves
experience least in very thin and very thick markets. -/
theorem fn44_thin_thick {pb pw : ℝ} (hb : 0 < pb) (hbw : pb < pw) (hw : pw ≤ 1) :
    pExp pw 0 - pExp pb 0 = 0 ∧ 0 < pExp pw 1 - pExp pb 1 ∧
      Tendsto (fun J : ℕ => pExp pw J - pExp pb J) atTop (𝓝 0) := by
  refine ⟨by simp [pExp], by simp [pExp]; linarith, ?_⟩
  have h1 : Tendsto (fun J : ℕ => (1 - pb) ^ J) atTop (𝓝 0) :=
    tendsto_pow_atTop_nhds_zero_of_lt_one (by linarith) (by linarith)
  have h2 : Tendsto (fun J : ℕ => (1 - pw) ^ J) atTop (𝓝 0) :=
    tendsto_pow_atTop_nhds_zero_of_abs_lt_one (by rw [abs_lt]; constructor <;> linarith)
  have := h1.sub h2
  simp only [sub_zero] at this
  refine this.congr (fun J => ?_)
  simp [pExp]

/-- Control for footnote 44. Without first-round direct discrimination,
`p_b = p_w`, there is no gap in experience at any thickness. -/
theorem control_fn44_no_direct (p : ℝ) (J : ℕ) : pExp p J - pExp p J = 0 := by ring

/-! ### Section 5.2 and Appendix B.1.3, printed arithmetic -/

/-- Table 1 and Section 5.2, in tenths of a percentage point. `28.3 − 20.0 =
8.3`, `25.6 − 13.2 = 12.4`, `31.1 − 26.9 = 4.2`, `25.6 − 12.4 = 13.2`,
`31.1 − 4.2 = 26.9`, `19.4 + 9.5 = 28.9`, and the return to experience is
`9.5/19.4 ≈ 49` percent. The introduction's "13 percentage points (90%)" is
`12.4/13.2 ≈ 94` percent (BHI-D9). -/
theorem table1_arith :
    (283 - 200 : ℤ) = 83 ∧ (256 - 132 : ℤ) = 124 ∧ (311 - 269 : ℤ) = 42 ∧
      (256 - 124 : ℤ) = 132 ∧ (311 - 42 : ℤ) = 269 ∧ (194 + 95 : ℤ) = 289 ∧
      (48 : ℝ) / 100 < 95 / 194 ∧ (95 : ℝ) / 194 < 50 / 100 ∧
      (93 : ℝ) / 100 < 124 / 132 ∧ (124 : ℝ) / 132 < 95 / 100 := by
  norm_num

/-- Table 3, in hundredths. Total equals average direct plus systemic in each
column, `0.24 = 0.15 + 0.09` and `0.03 = 0.07 − 0.04`, and the difference column
is the difference of the first two, `0.21`, `0.08`, `0.13`. -/
theorem table3_arith :
    (24 : ℤ) = 15 + 9 ∧ (3 : ℤ) = 7 + (-4) ∧ (24 - 3 : ℤ) = 21 ∧ (15 - 7 : ℤ) = 8 ∧
      (9 - (-4) : ℤ) = 13 := by
  norm_num

/-! ### Appendix B.1.1, differential screening -/

/-- B.1.1. With productivity `N(0, 1)` and signal precision `η`, the posterior
kernel `y² + η (y − s)²` is normal with mean `η s/(1 + η)`. -/
theorem b11_posterior {η : ℝ} (h : 0 < η) (y s : ℝ) :
    y ^ 2 + η * (y - s) ^ 2 = (1 + η) * (y - η * s / (1 + η)) ^ 2 + η / (1 + η) * s ^ 2 := by
  have : (1 + η) ≠ 0 := by positivity
  field_simp; ring

/-- B.1.1. Hiring at posterior mean `t` means a signal cut-off `t (1 + η)/η`,
which is higher for the noisier group, so group `b` faces direct
discrimination on the interval between the two cut-offs (for `t > 0`). -/
theorem b11_cutoff_order {ηb ηw : ℝ} (hb : 0 < ηb) (hbw : ηb < ηw) :
    (1 + ηw) / ηw < (1 + ηb) / ηb := by
  rw [div_lt_div_iff₀ (by linarith) hb]; nlinarith

/-- B.1.1, the systemic term with the square roots restored (BHI-D5), for any
strictly increasing `Φ`. At cut-off `κ`, systemic discrimination
`Φ(√η_b (κ − y)) − Φ(√η_w (κ − y))` hurts group `b` above the cut-off, helps it
below, and is zero at it. -/
theorem b11_systemic_sign {Φ : ℝ → ℝ} (hΦ : StrictMono Φ) {ηb ηw κ y : ℝ} (hb : 0 < ηb)
    (hbw : ηb < ηw) :
    (κ < y → 0 < Φ (Real.sqrt ηb * (κ - y)) - Φ (Real.sqrt ηw * (κ - y))) ∧
      (y < κ → Φ (Real.sqrt ηb * (κ - y)) - Φ (Real.sqrt ηw * (κ - y)) < 0) ∧
      Φ (Real.sqrt ηb * (κ - κ)) - Φ (Real.sqrt ηw * (κ - κ)) = 0 := by
  have hs : Real.sqrt ηb < Real.sqrt ηw := Real.sqrt_lt_sqrt hb.le hbw
  refine ⟨fun h => ?_, fun h => ?_, by simp⟩
  · have : Real.sqrt ηw * (κ - y) < Real.sqrt ηb * (κ - y) := by
      have : κ - y < 0 := by linarith
      nlinarith
    linarith [hΦ this]
  · have : Real.sqrt ηb * (κ - y) < Real.sqrt ηw * (κ - y) := by
      have : 0 < κ - y := by linarith
      nlinarith
    linarith [hΦ this]

/-- Control for B.1.1. With equal precisions there is no systemic
discrimination at any productivity. -/
theorem control_equal_precision_no_systemic (Φ : ℝ → ℝ) (η κ y : ℝ) :
    Φ (Real.sqrt η * (κ - y)) - Φ (Real.sqrt η * (κ - y)) = 0 := by ring

end BohrenEtAl2024
