import Mathlib

/-! Labov (1964), *The Social Stratification of English in New York City*,
Columbia University Ph.D. dissertation (University Microfilms 67-9411), read as
PDF pages 1 to 646.

This file formalises our reading of the structures the dissertation states, in its
own notation. It is not a formalisation of linguistic data. The definitions are the
measures the dissertation defines (the (r) index, the (th)-(dh) index, the (eh)
index, the MFY socio-economic index, the index of linguistic insecurity and the
deviation measure of a structure), its department store ordering, its lower middle
class cross-over, and the inference from speech to class that it describes as its
"second approach" (PDF pp. 237 to 238 and 312 to 313). The last of these is our
reading of a stated inference, written as Bayes' rule with two classes. -/

namespace Labov1964

/-! ## The phonological indexes (PDF pp. 75, 77, 80) -/

/-- The (r) index: the percentage of constricted (r-1) among the `n1 + n0`
determinate tokens; indeterminate tokens are not counted (PDF p. 75). -/
noncomputable def rIndex (n1 n0 : ℕ) : ℝ := 100 * n1 / (n1 + n0)

/-- The (r) index lies between 00 and 100. -/
theorem rIndex_mem {n1 n0 : ℕ} (h : 0 < n1 + n0) :
    0 ≤ rIndex n1 n0 ∧ rIndex n1 n0 ≤ 100 := by
  unfold rIndex
  have hpos : (0 : ℝ) < (n1 : ℝ) + n0 := by exact_mod_cast h
  have h0 : (0 : ℝ) ≤ n0 := by positivity
  refine ⟨by positivity, ?_⟩
  rw [div_le_iff₀ hpos]
  nlinarith

/-- A speaker with only (r-1) has (r)-100; one with only (r-0) has (r)-00. -/
theorem rIndex_ends {n : ℕ} (hn : 0 < n) : rIndex n 0 = 100 ∧ rIndex 0 n = 0 := by
  have h : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  refine ⟨?_, by simp [rIndex]⟩
  unfold rIndex
  field_simp
  simp

/-- The (th) or (dh) index for `a` fricatives (value 1), `b` affricates (value 2)
and `c` stops (value 3): the average value, less 1, times 100 (PDF p. 80). -/
noncomputable def thIndex (a b c : ℕ) : ℝ :=
  100 * (((a : ℝ) + 2 * b + 3 * c) / (a + b + c) - 1)

/-- The (th) index in closed form. -/
theorem thIndex_eq {a b c : ℕ} (h : 0 < a + b + c) :
    thIndex a b c = 100 * ((b : ℝ) + 2 * c) / (a + b + c) := by
  have hpos : (0 : ℝ) < (a : ℝ) + b + c := by exact_mod_cast h
  unfold thIndex
  field_simp
  ring

/-- (th)-00 for one who uses only fricatives, (th)-200 for one who uses only stops
(PDF p. 80). -/
theorem thIndex_ends {n : ℕ} (hn : 0 < n) : thIndex n 0 0 = 0 ∧ thIndex 0 0 n = 200 := by
  have h : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  constructor
  · simp [thIndex, h]
  · unfold thIndex
    simp only [Nat.cast_zero, mul_zero, zero_add, add_zero]
    rw [mul_div_assoc, div_self h]
    norm_num

/-- Every (th) index lies between 00 and 200. -/
theorem thIndex_mem {a b c : ℕ} (h : 0 < a + b + c) :
    0 ≤ thIndex a b c ∧ thIndex a b c ≤ 200 := by
  rw [thIndex_eq h]
  have hpos : (0 : ℝ) < (a : ℝ) + b + c := by exact_mod_cast h
  have ha : (0 : ℝ) ≤ a := by positivity
  have hb : (0 : ℝ) ≤ b := by positivity
  have hc : (0 : ℝ) ≤ c := by positivity
  refine ⟨by positivity, ?_⟩
  rw [div_le_iff₀ hpos]
  nlinarith

/-- The (eh) or (oh) index: ten times the average value on the six-point scale
(PDF p. 77). -/
def ehIndex (vals : List ℕ) : ℚ := 10 * (vals.sum : ℚ) / vals.length

/-- PDF p. 77: half (eh-3) and half (eh-2) gives (eh)-25, and a vowel always
level with that of *bat* (eh-4) gives (eh)-40. -/
theorem ehIndex_examples : ehIndex [3, 2] = 25 ∧ ehIndex [4, 4, 4, 4] = 40 := by
  norm_num [ehIndex]

/-! ## The socio-economic index (PDF pp. 241 to 244) -/

/-- The MFY socio-economic class: occupation, education and income ranks I to IV,
written `0` to `3`, given equal weight and summed into a ten-point scale (PDF
p. 243, Table 1 on p. 244). -/
def sec (o e i : Fin 4) : ℕ := o.val + e.val + i.val

theorem sec_le_nine (o e i : Fin 4) : sec o e i ≤ 9 := by
  unfold sec; omega

/-- Classes 0 and 9 are the most uniform: only the lowest, or only the highest,
category on all three indicators (PDF p. 244). -/
theorem sec_extremes_uniform (o e i : Fin 4) :
    (sec o e i = 9 ↔ o = 3 ∧ e = 3 ∧ i = 3) ∧ (sec o e i = 0 ↔ o = 0 ∧ e = 0 ∧ i = 0) := by
  revert o e i; decide

/-- In classes 4 and 5 there are twelve possible configurations of occupation,
education and income (PDF p. 244). -/
theorem sec_twelve_configurations :
    ((Finset.univ : Finset (Fin 4 × Fin 4 × Fin 4)).filter
        (fun x => sec x.1 x.2.1 x.2.2 = 4)).card = 12 ∧
    ((Finset.univ : Finset (Fin 4 × Fin 4 × Fin 4)).filter
        (fun x => sec x.1 x.2.1 x.2.2 = 5)).card = 12 := by
  decide

/-- Control: uniformity holds only at the extremes. A middle class such as 4 is
reached from different configurations, so the class does not determine them. -/
theorem control_sec_middle_not_uniform :
    ∃ a b : Fin 4 × Fin 4 × Fin 4, a ≠ b ∧
      sec a.1 a.2.1 a.2.2 = 4 ∧ sec b.1 b.2.1 b.2.2 = 4 := by
  exact ⟨(3, 1, 0), (0, 1, 3), by decide, by decide, by decide⟩

/-! ## The index of linguistic insecurity (PDF pp. 177, 473 to 474) -/

/-- For `n` words heard in two pronunciations, `circled k` is the variant the
respondent calls correct and `checked k` the one he says he uses. The index is the
number of items on which the two differ (eighteen words in the survey). -/
def ili {n : ℕ} (circled checked : Fin n → Bool) : ℕ :=
  (Finset.univ.filter fun k => circled k ≠ checked k).card

theorem ili_le {n : ℕ} (circled checked : Fin n → Bool) : ili circled checked ≤ n := by
  unfold ili
  exact (Finset.card_filter_le _ _).trans (by simp)

/-- The index is zero exactly when every reported usage is the variant the
respondent calls correct. -/
theorem ili_eq_zero_iff {n : ℕ} (circled checked : Fin n → Bool) :
    ili circled checked = 0 ↔ circled = checked := by
  unfold ili
  rw [Finset.card_eq_zero, Finset.filter_eq_empty_iff]
  constructor
  · intro h
    funext x
    by_contra hx
    exact h (Finset.mem_univ x) hx
  · rintro rfl x _ hx
    exact hx rfl

/-! ## The department store ordering (PDF pp. 88 to 98) -/

inductive Store
  | klein
  | macys
  | saks
  deriving DecidableEq

/-- The prestige ranking of the stores (PDF pp. 89 to 93). -/
def prestigeRank : Store → ℕ
  | .klein => 0
  | .macys => 1
  | .saks => 2

/-- Percentages using all or some (r-1): Saks 62, Macy's 51, Klein's 20 (PDF p. 98). -/
def allOrSome : Store → ℕ
  | .klein => 20
  | .macys => 51
  | .saks => 62

/-- The hypothesis of PDF p. 88: groups ranked by social stratification are ranked
in the same order by their use of (r). -/
def Stratified {ι : Type} (rank : ι → ℕ) (score : ι → ℕ) : Prop :=
  ∀ a b, rank a < rank b → score a < score b

theorem store_stratified : Stratified prestigeRank allOrSome := by
  intro a b h
  cases a <;> cases b <;> simp_all [prestigeRank, allOrSome]

/-- Wages do not stratify the employees in the same order: Saks pays less than
Macy's (PDF p. 92). We write the wage order as Klein's, Saks, Macy's. -/
def wageRank : Store → ℕ
  | .klein => 0
  | .saks => 1
  | .macys => 2

/-- Control: the ordering of (r) holds for the prestige ranking and fails for the
wage ranking, so the result needs the prestige ranking of the stores. -/
theorem control_wage_order_not_stratified : ¬ Stratified wageRank allOrSome := by
  intro h
  have := h .saks .macys (by simp [wageRank])
  simp [allOrSome] at this

/-! ## Regular structure and the lower middle class cross-over (PDF pp. 258 to 271) -/

/-- The deviation of a structure: deviant relations over expected relations. For
Figure 3, nine of thirty, a deviation of .30, below the .50 at which a structure is
"not worth considering" (PDF p. 260). -/
def deviation (deviant expected : ℕ) : ℚ := deviant / expected

theorem fig3_deviation : deviation 9 30 = 3 / 10 ∧ deviation 9 30 < 1 / 2 := by
  norm_num [deviation]

/-- Two class lines cross between styles `s` and `t`: class `x` is below class `y`
at `s` and above it at `t`. -/
def Crosses {σ : Type} (x y : σ → ℝ) (s t : σ) : Prop := x s < y s ∧ y t < x t

/-- A cross-over is a deviation from regular structure: neither class is above the
other at every style. -/
theorem crossover_breaks_order {σ : Type} {x y : σ → ℝ} {s t : σ} (h : Crosses x y s t) :
    ¬ (∀ u, x u < y u) ∧ ¬ (∀ u, y u < x u) := by
  obtain ⟨h1, h2⟩ := h
  exact ⟨fun hall => lt_asymm (hall t) h2, fun hall => lt_asymm (hall s) h1⟩

/-- The (r) cross-over of PDF pp. 269 to 271: in casual speech the lower middle
class has (r)-04 and the upper middle class (r)-18; in isolated words the lower
middle class is the higher (stated on PDF pp. 269 and 374 without a class 9 value,
so it enters as a hypothesis `hD`). -/
theorem r_crossover {lmcD umcD : ℝ} (hD : umcD < lmcD) :
    Crosses (fun s : Bool => if s then lmcD else 4) (fun s => if s then umcD else 18)
      false true := by
  simp only [Crosses, Bool.false_eq_true, if_false, if_true]
  exact ⟨by norm_num, hD⟩

/-- Control: if one class is below the other at every style, there is no
cross-over. -/
theorem control_regular_no_crossover {σ : Type} {x y : σ → ℝ} (h : ∀ u, x u < y u)
    (s t : σ) : ¬ Crosses x y s t := fun hc => lt_asymm (h t) hc.2

/-! ## The inference from speech to class (our reading of PDF pp. 237 to 238, 312 to 313) -/

/-- Posterior that a speaker is of the higher class `H` after hearing a form whose
frequency is `pH` in `H` and `pL` in the lower class `L`, from prior `π`. -/
noncomputable def posterior (π pH pL : ℝ) : ℝ := π * pH / (π * pH + (1 - π) * pL)

/-- If the form is stratified, used more by the higher class, hearing it raises the
probability that the speaker is of the higher class. -/
theorem posterior_gt_prior {π pH pL : ℝ} (h0 : 0 < π) (h1 : π < 1) (hL : 0 < pL)
    (h : pL < pH) : π < posterior π pH pL := by
  unfold posterior
  have hden : 0 < π * pH + (1 - π) * pL := by nlinarith
  rw [lt_div_iff₀ hden]
  nlinarith [mul_pos (mul_pos h0 (sub_pos.2 h1)) (sub_pos.2 h)]

/-- Control: without stratification the form carries no information about class,
and the posterior equals the prior. -/
theorem control_posterior_eq_prior_unstratified {π p : ℝ} (hp : 0 < p) :
    posterior π p p = π := by
  unfold posterior
  have : π * p + (1 - π) * p = p := by ring
  rw [this, mul_div_assoc, div_self hp.ne', mul_one]

end Labov1964
