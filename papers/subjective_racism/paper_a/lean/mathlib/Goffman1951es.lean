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

/-! ### Symbols that are not *pruebas* (PDF pp. 3, 14)

A symbol fails as a *prueba* exactly when someone below the standing displays
it, which is how fraud (PDF p. 3) and the curators (PDF p. 14), recruited from
lower classes and expert in higher symbols, break it. -/

/-- A symbol is no *prueba* of `p` exactly when some displayer stands below
`p`. -/
theorem not_prueba_iff (W : ι → ℝ) (D : Finset ι) (p : ℝ) :
    ¬ IsPrueba W D p ↔ ∃ i ∈ D, W i < p := by
  unfold IsPrueba
  push_neg
  rfl

/-- One curator below the standing who displays the symbol breaks it. -/
theorem curator_breaks_prueba (W : ι → ℝ) (D : Finset ι) (p : ℝ) {i : ι}
    (hi : i ∈ D) (hlow : W i < p) : ¬ IsPrueba W D p :=
  (not_prueba_iff W D p).2 ⟨i, hi, hlow⟩

/-! ### (I) Moral restrictions (PDF p. 6)

Internalised scruple `k`, reinforced by the sanction `s` of opinion in the
person's own group and in the class whose symbol is used, against the benefit
`b` of misrepresentation. -/

/-- The net gain from misrepresentation. -/
def moralGain (b k s : ℝ) : ℝ := b - k - s

/-- Misrepresentation is deterred when scruple and sanction together reach the
benefit. -/
theorem moral_deters {b k s : ℝ} (h : b ≤ k + s) : moralGain b k s ≤ 0 := by
  unfold moralGain; linarith

/-- Internalised scruple lowers the external sanction that deters, "la
eficacia de estas sanciones externas se debe en parte a la disposición con la
que las restricciones morales interiorizadas las refuerzan". -/
theorem scruple_reinforces_sanction {b k : ℝ} (hk : 0 < k) :
    moralGain b k (b - k) ≤ 0 ∧ b - k < b := by
  unfold moralGain; constructor <;> linarith

/-- Control: with no scruple and no sanction, any positive benefit gains. -/
theorem control_no_moral_restriction {b : ℝ} (hb : 0 < b) : 0 < moralGain b 0 0 := by
  unfold moralGain; linarith

/-! ### (3) Natural restrictions (PDF pp. 7 to 9)

Scarce units that go to those of highest standing make the symbol a *prueba*
of the standing of anyone left out. Association with high-status persons is
scarce because each can be intimate with only a few people, and a theatre
performance is scarce because its audience is limited. -/

/-- The displayers are the top of the order, standing at least that of anyone
outside. -/
def IsTop (W : ι → ℝ) (D : Finset ι) : Prop := ∀ i ∈ D, ∀ j, j ∉ D → W j ≤ W i

/-- A scarce symbol held by the top is a *prueba* of the standing of anyone
outside. -/
theorem scarcity_prueba (W : ι → ℝ) (D : Finset ι) (h : IsTop W D) {j : ι} (hj : j ∉ D) :
    IsPrueba W D (W j) := fun i hi => h i hi j hj

/-- The capacity bound. If each of the high-status persons `H` can be intimate
with at most `K` people (or each performance seats at most `K`), at most
`|H| K` people can display the association (or the attendance). -/
theorem association_capacity [DecidableEq ι] (H : Finset ι) (intimates : ι → Finset ι)
    (K : ℕ) (hK : ∀ h ∈ H, (intimates h).card ≤ K) :
    (H.biUnion intimates).card ≤ H.card * K :=
  Finset.card_biUnion_le_card_mul H intimates K hK

/-- Control: drop the scarcity. If the units are not held by the top, someone
outside may stand higher than a holder, and the symbol is no *prueba* of that
person's standing. -/
theorem control_not_top :
    ¬ IsPrueba (![1, 10] : Fin 2 → ℝ) {0} 10 := by
  intro h
  have := h 0 (by simp)
  norm_num at this

/-! ### Price and display (PDF pp. 6, 7)

The high price of certain scarce objects is explained "refiriéndonos a los
beneficios sociales que obtienen sus propietarios al mostrar estas posesiones".
We read a price as experiential value `u` plus display value `d`. -/

/-- Two objects of the same experiential value with different prices differ in
display value. -/
theorem price_from_display {u₁ u₂ d₁ d₂ : ℝ} (hu : u₁ = u₂) (hp : u₁ + d₁ ≠ u₂ + d₂) :
    d₁ ≠ d₂ := by
  intro hd; apply hp; rw [hu, hd]

/-! ### (4) Restrictions of socialisation (PDF p. 10)

An imitated style is told from a habitual one with probability `δ`. A
pretender gains `b` if not detected and loses `L` if detected. -/

/-- The expected gain from imitating a style. -/
def imitationGain (δ b L : ℝ) : ℝ := (1 - δ) * b - δ * L

/-- Detection at or above `b/(b + L)` deters imitation. -/
theorem socialisation_deters {δ b L : ℝ} (hb : 0 < b) (hL : 0 ≤ L)
    (hδ : b / (b + L) ≤ δ) : imitationGain δ b L ≤ 0 := by
  unfold imitationGain
  have hbl : 0 < b + L := by linarith
  rw [div_le_iff₀ hbl] at hδ
  nlinarith

/-- Control: drop the detection. With `δ = 0`, imitation always gains. -/
theorem control_undetectable {b L : ℝ} (hb : 0 < b) : 0 < imitationGain 0 b L := by
  unfold imitationGain; linarith

/-! ### (5) Restrictions of refinement (PDF p. 11)

Refinement takes time `T` with a cost per unit of time `c i`, lower for those
of higher standing (leisure and long unpaid activity). A person acquires it
when the benefit `b` covers `c i T`. -/

/-- Cost of time falling in standing makes the acquirers an upper set. -/
theorem refinement_upper_set (W c : ι → ℝ) (hc : ∀ i j, W i ≤ W j → c j ≤ c i)
    {b T : ℝ} (hT : 0 ≤ T) {i j : ι} (hi : c i * T ≤ b) (hij : W i ≤ W j) :
    c j * T ≤ b := by
  have := hc i j hij
  nlinarith

/-- Control: drop the falling cost of time. Then a person of higher standing
may not acquire a refinement that one of lower standing acquires. -/
theorem control_cost_not_falling :
    let W : Fin 2 → ℝ := ![0, 1]
    let c : Fin 2 → ℝ := ![1, 3]
    c 0 * 1 ≤ 2 ∧ W 0 ≤ W 1 ∧ ¬ c 1 * 1 ≤ 2 := by
  norm_num

/-! ### Restrictions in combination (PDF pp. 12, 13) -/

/-- If one restriction fails entirely, `e₁ = 1`, the symbol is still as well
protected as the other restriction makes it. -/
theorem one_checks_failure (e₂ : ℝ) : (1 : ℝ) * e₂ = e₂ := one_mul e₂

/-- Every restriction is evaded somewhere (GF-Q11), so with every evasion
probability positive a pretender succeeds with positive probability. -/
theorem evasion_always_possible {e₁ e₂ : ℝ} (h1 : 0 < e₁) (h2 : 0 < e₂) : 0 < e₁ * e₂ :=
  mul_pos h1 h2

/-! ### The margin of doubt (PDF pp. 4, 5)

Class rests on a configuration of sources weighed against one another. We read
standing as `w₁ x₁ + w₂ x₂` over two sources with weights an observer
chooses. -/

/-- Standing under weights `w` of a profile `x` over two sources. -/
def standing (w x : Fin 2 → ℝ) : ℝ := w 0 * x 0 + w 1 * x 1

/-- Two observers with different weights rank the same two persons in opposite
orders, so neither ranking is a fact both can check. -/
theorem opposite_rankings :
    standing ![1, 2] ![1, 0] < standing ![1, 2] ![0, 1] ∧
      standing ![2, 1] ![0, 1] < standing ![2, 1] ![1, 0] := by
  norm_num [standing]

/-- Control: drop the conflict between sources. If one person stands at least
as high on every source, every non-negative weighting agrees. -/
theorem control_dominance_agrees {w x y : Fin 2 → ℝ} (hw0 : 0 ≤ w 0) (hw1 : 0 ≤ w 1)
    (h0 : y 0 ≤ x 0) (h1 : y 1 ≤ x 1) : standing w y ≤ standing w x := by
  unfold standing; nlinarith

/-! ### Open treatment and public proof (PDF p. 5)

"a aquellos que ofrecen prueba pública de poseer los valores favoritos de su
sociedad no se les puede negar abiertamente el status que sus símbolos les
permiten exigir". We read open treatment as a function of public proof alone,
and treatment not openly approved as unrestricted. -/

/-- Open treatment that depends on public proof alone treats two persons with
the same public proof alike. -/
theorem open_treatment_equal {ι' : Type*} (proof : ι' → ℝ) (f : ℝ → ℝ) {i j : ι'}
    (h : proof i = proof j) : f (proof i) = f (proof j) := by rw [h]

/-- Control: drop the restriction to public proof. Treatment that is not
openly approved can differ between two persons with the same public proof. -/
theorem control_covert_treatment :
    ∃ (proof covert : Fin 2 → ℝ), proof 0 = proof 1 ∧ covert 0 ≠ covert 1 :=
  ⟨fun _ => 0, ![0, 1], rfl, by simp⟩

end Goffman1951es
