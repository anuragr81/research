import Mathlib

/-! Bohren, Hull and Imas, "Systemic Discrimination: Theory and Measurement",
NBER working paper 29820, March 2022, revised December 2024, Sections 3.1,
3.2 and 4.1, in the paper's notation. We fix a qualification level `y⁰` and
take a finite signal space `𝒮`, so that the group-`g` signal distribution
`σ*(y⁰, g)` is a weight `sig g s` on each signal and the action rule is
`A g s = A*(g, s)`. The paper allows general measurable spaces; the finite
case is ours and keeps the integrals as sums. -/

namespace BohrenEtAl2024

open Finset

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

end BohrenEtAl2024
