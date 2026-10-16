import Mathlib

/-! Goffman, "Símbolos de status de clase", the authorised Spanish translation
by Servando Ortoll (Universidad de Sonora) of "Symbols of Class Status",
*British Journal of Sociology* 2(4), 1951, pp. 294–304. The translation is the
version read. The English original is not read.

The essay states no model. What follows formalises **our reading** of three
structures in it, in its own terms where they exist.

* A *símbolo de status* is a sign used to place a person socially. A *prueba
  de status* is a sign that gives reliable evidence of position (PDF p. 3). We
  read a symbol as a *prueba* of a standing `p` when every person who displays
  it has standing at least `p`.
* The *restricción intrínseca* (PDF p. 6). A symbol of wealth that consumes
  wealth implies that the owner has at least as much money as the symbol costs
  on the open market. We read display as feasible only when `p ≤ W i`.
* The decline of a class (PDF pp. 13–14). Its members' association with
  symbols that need no current outlay "reduce el valor de estas señales ante
  los ojos de los demás". We read the value of a symbol in others' eyes as the
  mean standing of those who display it.
* Several restrictions at once (PDF p. 13). "La eficacia de un tipo de
  restricción actúa como un control sobre el fracaso de otro". We read a
  pretender as having to evade every restriction a symbol carries, each with a
  probability of success `e`. -/

namespace Goffman1951es

open Finset

variable {ι : Type*}

/-- *Prueba de status*. The symbol displayed by `D` is reliable evidence of a
standing of at least `p` when every displayer has standing at least `p`. -/
def IsPrueba (W : ι → ℝ) (D : Finset ι) (p : ℝ) : Prop := ∀ i ∈ D, p ≤ W i

/-- *Restricción intrínseca*. Of those who want the symbol, `S`, only those who
can pay its market price `p` out of their wealth display it. -/
noncomputable def displayers (W : ι → ℝ) (S : Finset ι) (p : ℝ) : Finset ι :=
  by classical exact S.filter (fun i => p ≤ W i)

/-- The intrinsic restriction makes the symbol a *prueba* of wealth `p`, "el
propietario tiene al menos tanto dinero como los símbolos pueden conseguir en
el mercado abierto" (PDF p. 6). -/
theorem intrinsic_is_prueba (W : ι → ℝ) (S : Finset ι) (p : ℝ) :
    IsPrueba W (displayers W S p) p := by
  classical
  intro i hi
  simp only [displayers, Finset.mem_filter] at hi
  exact hi.2

/-- The mean wealth of the displayers of a *prueba* of `p` is at least `p`. -/
theorem prueba_mean_ge (W : ι → ℝ) (D : Finset ι) (p : ℝ) (h : IsPrueba W D p)
    (hD : D.Nonempty) : p ≤ (∑ i ∈ D, W i) / D.card := by
  have hc : (0 : ℝ) < D.card := by exact_mod_cast hD.card_pos
  have hs : (D.card : ℝ) * p ≤ ∑ i ∈ D, W i := by
    have := Finset.card_nsmul_le_sum D W p h
    simpa [nsmul_eq_mul] using this
  rw [le_div_iff₀ hc]
  linarith

/-- The decline of a class (PDF pp. 13–14). When `k` new displayers whose mean
standing `t` is below the users' mean `m` take up a symbol, the symbol's value,
the mean standing of all who display it, falls below `m`. -/
theorem association_lowers_value {n k m t : ℝ} (hn : 0 < n) (hk : 0 < k) (ht : t < m) :
    (n * m + k * t) / (n + k) < m := by
  rw [div_lt_iff₀ (by linarith)]
  nlinarith

/-- Several restrictions at once (PDF p. 13). A pretender who must evade two
restrictions, with success probabilities `e₁` and `e₂`, succeeds no more often
than against either one alone. -/
theorem conjunctive_restrictions {e₁ e₂ : ℝ} (h1 : 0 ≤ e₁) (h1' : e₁ ≤ 1)
    (h2 : 0 ≤ e₂) (h2' : e₂ ≤ 1) :
    e₁ * e₂ ≤ e₁ ∧ e₁ * e₂ ≤ e₂ := by
  constructor <;> nlinarith

/-- Control: drop the intrinsic restriction. If the symbol can be had for a
counterfeit price `1` below its market price `5`, a person of wealth `1`
displays it, and the symbol is no *prueba* of wealth `5`. -/
theorem control_counterfeit :
    ¬ IsPrueba (![1, 10] : Fin 2 → ℝ) (displayers ![1, 10] Finset.univ 1) 5 := by
  intro h
  have h0 : (0 : Fin 2) ∈ displayers (![1, 10] : Fin 2 → ℝ) Finset.univ 1 := by
    simp [displayers]
  have := h 0 h0
  norm_num at this

/-- Control: drop the condition that the newcomers stand below the users. New
displayers at or above the users' mean do not lower the symbol's value. -/
theorem control_association_from_above {n k m t : ℝ} (hn : 0 < n) (hk : 0 < k)
    (ht : m ≤ t) : m ≤ (n * m + k * t) / (n + k) := by
  rw [le_div_iff₀ (by linarith)]
  nlinarith

/-- Control: drop the condition that a pretender must evade every restriction.
If evading any one of two restrictions suffices, the pretender succeeds at
least as often as against either one alone, so a second restriction weakens
the symbol rather than protecting it. -/
theorem control_alternative_restrictions {e₁ e₂ : ℝ} (h1 : 0 ≤ e₁) (h1' : e₁ ≤ 1)
    (h2 : 0 ≤ e₂) (h2' : e₂ ≤ 1) :
    e₁ ≤ 1 - (1 - e₁) * (1 - e₂) ∧ e₂ ≤ 1 - (1 - e₁) * (1 - e₂) := by
  constructor <;> nlinarith

end Goffman1951es
