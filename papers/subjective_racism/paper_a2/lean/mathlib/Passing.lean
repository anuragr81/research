import Mathlib

namespace Passing

variable {ι : Type*} [DecidableEq ι]

/-- The mean class of a finite set of people. -/
noncomputable def mean (c : ι → ℝ) (V : Finset ι) : ℝ := (∑ i ∈ V, c i) / V.card

omit [DecidableEq ι] in
/-- The cross sum that compares two sets. -/
theorem cross (c : ι → ℝ) (S P : Finset ι) :
    ∑ p ∈ S ×ˢ P, (c p.2 - c p.1) = S.card * ∑ j ∈ P, c j - P.card * ∑ i ∈ S, c i := by
  rw [Finset.sum_product]
  simp only [Finset.sum_sub_distrib, Finset.sum_const, nsmul_eq_mul, Finset.mul_sum]

/-- Row M14 (i). If every leaver's class is at least every stayer's, the mean
of the stayers is at most the mean of the whole. -/
theorem mean_sdiff_le {c : ι → ℝ} {V P : Finset ι} (hP : P ⊆ V)
    (hS : (V \ P).Nonempty) (hPn : P.Nonempty)
    (h : ∀ i ∈ P, ∀ j ∈ V \ P, c j ≤ c i) :
    mean c (V \ P) ≤ mean c V := by
  have hsum : ∑ i ∈ V \ P, c i + ∑ i ∈ P, c i = ∑ i ∈ V, c i := Finset.sum_sdiff hP
  have hcard : (V \ P).card + P.card = V.card := Finset.card_sdiff_add_card_eq_card hP
  have hS' : (0 : ℝ) < (V \ P).card := by exact_mod_cast hS.card_pos
  have hP' : (0 : ℝ) < P.card := by exact_mod_cast hPn.card_pos
  have hx : 0 ≤ ∑ p ∈ (V \ P) ×ˢ P, (c p.2 - c p.1) :=
    Finset.sum_nonneg fun p hp => by
      rw [Finset.mem_product] at hp
      linarith [h p.2 hp.2 p.1 hp.1]
  rw [cross] at hx
  unfold mean
  rw [div_le_iff₀ hS', div_mul_eq_mul_div, le_div_iff₀ (by rw [← hcard]; push_cast; linarith)]
  rw [← hsum, ← hcard]
  push_cast
  nlinarith

/-- Row M14 (i), strict. If some leaver is above some stayer, the mean falls. -/
theorem mean_sdiff_lt {c : ι → ℝ} {V P : Finset ι} (hP : P ⊆ V)
    (hS : (V \ P).Nonempty) (hPn : P.Nonempty)
    (h : ∀ i ∈ P, ∀ j ∈ V \ P, c j ≤ c i)
    (h' : ∃ i ∈ P, ∃ j ∈ V \ P, c j < c i) :
    mean c (V \ P) < mean c V := by
  have hsum : ∑ i ∈ V \ P, c i + ∑ i ∈ P, c i = ∑ i ∈ V, c i := Finset.sum_sdiff hP
  have hcard : (V \ P).card + P.card = V.card := Finset.card_sdiff_add_card_eq_card hP
  have hS' : (0 : ℝ) < (V \ P).card := by exact_mod_cast hS.card_pos
  have hP' : (0 : ℝ) < P.card := by exact_mod_cast hPn.card_pos
  obtain ⟨i, hi, j, hj, hij⟩ := h'
  have hx : 0 < ∑ p ∈ (V \ P) ×ˢ P, (c p.2 - c p.1) :=
    Finset.sum_pos' (fun p hp => by
        rw [Finset.mem_product] at hp
        linarith [h p.2 hp.2 p.1 hp.1])
      ⟨(j, i), Finset.mem_product.2 ⟨hj, hi⟩, by simp; linarith⟩
  rw [cross] at hx
  unfold mean
  rw [div_lt_iff₀ hS', div_mul_eq_mul_div, lt_div_iff₀ (by rw [← hcard]; push_cast; linarith)]
  rw [← hsum, ← hcard]
  push_cast
  nlinarith

/-- Row M14 (ii). With a benefit of passing that does not fall in class and a
cost that does not rise in it, whoever passes is joined by everyone above. -/
theorem upper_set_of_threshold {benefit cost : ℝ → ℝ} (hb : Monotone benefit)
    (hk : Antitone cost) {x y : ℝ} (hxy : x ≤ y) (hx : cost x ≤ benefit x) :
    cost y ≤ benefit y :=
  le_trans (hk hxy) (le_trans hx (hb hxy))

/-- Row M14 (iii). A lower visible mean raises the discrimination rate under a
decreasing `F`. -/
theorem rate_rises {F : ℝ → ℝ} (hF : Antitone F) {s m m' : ℝ} (hs : 0 ≤ s)
    (h : m' ≤ m) : s * F m ≤ s * F m' :=
  mul_le_mul_of_nonneg_left (hF h) hs

/-- Control: without the ordering, exit can raise the mean. Two people with
classes `0` and `1`. If the one with class `0` leaves, the visible mean rises
from `1/2` to `1`. -/
theorem control_ordering_needed :
    mean (![0, 1] : Fin 2 → ℝ) ({0, 1} \ {0}) = 1 ∧
      mean (![0, 1] : Fin 2 → ℝ) {0, 1} = 1 / 2 := by
  constructor
  · rw [show ({0, 1} : Finset (Fin 2)) \ {0} = {1} by decide]
    simp [mean]
  · simp [mean]

end Passing
