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

/-! ## Second pass: the arrays of Chapter VII as regular structures (PDF pp. 249 to 262)

Index values are written times ten, so that (r)-2.5 is `25`. Rows are class groups from
low to high, columns are styles from informal to formal. A relation is expected between
every pair of styles within a class group and every pair of class groups within a
style, and it is deviant when the order it should show fails (PDF pp. 258 to 260). -/

/-- The direction a relation should take: increasing (`true`) or decreasing. -/
def inOrder (up : Bool) (a b : ℕ) : Bool := if up then decide (a < b) else decide (b < a)

/-- The number of deviant relations of an array of classes `Fin m` by styles `Fin n`.
`upStyle` says whether the index rises toward formal styles, `upClass` whether it rises
toward higher classes. -/
def deviantCount {m n : ℕ} (v : Fin m → Fin n → ℕ) (upStyle upClass : Bool) : ℕ :=
  ((Finset.univ : Finset (Fin m × Fin n × Fin n)).filter
      (fun x => x.2.1 < x.2.2 ∧ inOrder upStyle (v x.1 x.2.1) (v x.1 x.2.2) = false)).card +
  ((Finset.univ : Finset (Fin n × Fin m × Fin m)).filter
      (fun x => x.2.1 < x.2.2 ∧ inOrder upClass (v x.2.1 x.1) (v x.2.2 x.1) = false)).card

/-- The number of expected relations: `m · C(n,2) + n · C(m,2)` (thirty for Figure 3). -/
def expectedCount (m n : ℕ) : ℕ := m * n.choose 2 + n * m.choose 2

/-- Table 4, (r), classes 0-2, 3-5, 6-9 by styles A, B, C, D, D′ (PDF p. 249). -/
def tab4r : Fin 3 → Fin 5 → ℕ :=
  ![![25, 105, 145, 235, 495], ![40, 125, 210, 350, 550], ![125, 250, 290, 555, 700]]

/-- Table 4, (eh), styles A to D. -/
def tab4eh : Fin 3 → Fin 4 → ℕ :=
  ![![230, 270, 290, 320], ![250, 280, 305, 320], ![270, 300, 340, 350]]

/-- Table 4, (oh), styles A to D. -/
def tab4oh : Fin 3 → Fin 4 → ℕ :=
  ![![230, 240, 240, 210], ![195, 220, 230, 240], ![200, 235, 265, 295]]

/-- Table 4, (th), styles A to C. -/
def tab4th : Fin 3 → Fin 3 → ℕ :=
  ![![780, 650, 435], ![680, 535, 270], ![255, 165, 100]]

/-- Table 4, (dh), styles A to C. -/
def tab4dh : Fin 3 → Fin 3 → ℕ :=
  ![![785, 560, 490], ![635, 445, 340], ![295, 165, 130]]

/-- Figures 1, 4 and 5 are regular structures: (r) rises with style and class at all
fifteen points, and (th) and (dh) fall with both (PDF pp. 251 to 253). This is the
hypothesis of PDF p. 88 holding for the Lower East Side class groups in every style. -/
theorem table4_regular :
    deviantCount tab4r true true = 0 ∧ expectedCount 3 5 = 45 ∧
    deviantCount tab4th false false = 0 ∧ deviantCount tab4dh false false = 0 := by
  decide

/-- Figure 2 shows one deviation, the lower and working classes meeting at (eh)-32 in
Style D (PDF pp. 251, 259). -/
theorem table4_eh_one_deviation : deviantCount tab4eh true true = 1 := by decide

/-- Figure 3 shows nine deviant relations out of thirty, a deviation of .30 (PDF
p. 260), computed from the printed Table 4. -/
theorem table4_oh_nine_of_thirty :
    deviantCount tab4oh true true = 9 ∧ expectedCount 3 4 = 30 := by decide

/-- The (oh) array without the lower class, classes 3-5 and 6-9. -/
def tab4ohUpper : Fin 2 → Fin 4 → ℕ := ![![195, 220, 230, 240], ![200, 235, 265, 295]]

/-- PDF p. 260: removing group 0-2 from Figure 3 leaves "a regular but less complex
structure". Every deviation of Figure 3 involves the lower class, in both the class and
the style direction, the double deviation of PDF p. 256. -/
theorem oh_without_lower_class_regular : deviantCount tab4ohUpper true true = 0 := by
  decide

/-- Control: the regularity of (r) is not automatic. The same count on the (oh) array
finds deviations (`table4_oh_nine_of_thirty`), and reversing the expected direction of
(r) makes every one of its 45 relations deviant. -/
theorem control_r_wrong_direction : deviantCount tab4r false false = 45 := by decide

/-! ## The printed matrix of the socio-economic index (Table 1, PDF p. 244) -/

/-- Table 1 as printed. Index `0` is rank IV (high) and `3` is rank I (low); the entry
for income `i`, education `e` and occupation `o` is row `e`, block `i`, column `o`. -/
def table1 : Fin 4 → Fin 4 → Fin 4 → ℕ :=
  ![![![9, 8, 7, 6], ![8, 7, 6, 5], ![7, 6, 5, 4], ![6, 5, 4, 3]],
    ![![8, 7, 6, 5], ![7, 6, 5, 4], ![6, 5, 4, 3], ![5, 4, 3, 2]],
    ![![7, 6, 5, 4], ![6, 5, 4, 3], ![5, 4, 3, 2], ![4, 3, 2, 1]],
    ![![6, 5, 4, 3], ![5, 4, 3, 2], ![4, 3, 2, 1], ![3, 2, 1, 0]]]

/-- Every entry of the printed matrix is the equal-weight sum `sec`, with rank IV
counted 3 and rank I counted 0. -/
theorem table1_is_sec :
    ∀ i e o : Fin 4, table1 i e o = sec (Fin.rev o) (Fin.rev e) (Fin.rev i) := by
  decide

/-! ## The department store tables (PDF pp. 96 to 107) -/

/-- Round `100 k / n` to the nearest whole per cent. -/
def pct (k n : ℕ) : ℕ := (200 * k + n) / (2 * n)

/-- PDF pp. 96, 99 to 107. The three samples total 264. Table 2 (complete responses),
Table 3 (Negro employees), Table 4 (Saks floors) and Table 5 (age) each sum to 100.
The Saks floors add to the Saks sample of 68. The percentages of Negro informants,
03, 14 and 25, are 2 of 68, 17 of 125 and 18 of 71. -/
theorem store_tables_arithmetic :
    68 + 125 + 71 = 264 ∧
    24 + 46 + 30 = 100 ∧ 22 + 37 + 41 = 100 ∧ 6 + 12 + 82 = 100 ∧
    50 + 0 + 50 = 100 ∧ 12 + 35 + 53 = 100 ∧ 0 + 6 + 94 = 100 ∧
    23 + 23 + 54 = 100 ∧ 34 + 40 + 26 = 100 ∧ 30 + 38 = 68 ∧
    24 + 21 + 55 = 100 ∧ 20 + 28 + 52 = 100 ∧ 20 + 22 + 58 = 100 ∧
    pct 2 68 = 3 ∧ pct 17 125 = 14 ∧ pct 18 71 = 25 := by
  decide

/-- The orderings of the department store survey beyond Figure 1. Complete responses
with all (r-1): Saks 24, Macy's 22, Klein's 6 (Table 2). Negro employees with all
(r-1): 50, 12, 0 (Table 3). Upper floors of Saks above the ground floor, 34 against 23
(Table 4). Stops in *fourth*: 0, 4 and 15 per cent, in the reverse order (p. 105). -/
theorem store_orderings :
    Stratified prestigeRank (fun s => match s with | .klein => 6 | .macys => 22 | .saks => 24) ∧
    Stratified prestigeRank (fun s => match s with | .klein => 0 | .macys => 12 | .saks => 50) ∧
    23 < 34 ∧
    Stratified prestigeRank (fun s => match s with | .klein => 0 | .macys => 4 | .saks => 15) := by
  refine ⟨?_, ?_, by norm_num, ?_⟩ <;>
  · intro a b h
    cases a <;> cases b <;> simp_all [prestigeRank]

/-! ## The sample (Chapter VI and Appendices C and D) -/

/-- PDF pp. 205, 208, 220 and 614 to 615. Of 312 native speakers, 8 died and 109
moved, leaving 195. 158 interviewed is 81 per cent of them, the 122 full interviews 63
per cent. Table 5 of Chapter VI sums to 81 New Yorkers and 37 others. In Appendix D,
119 of 189 is 63 per cent, and the 70 non-respondents are 27 refusals and 43 not
reached, 33 of them sampled by television (16 and 17). -/
theorem sample_arithmetic :
    312 - 8 - 109 = 195 ∧ pct 158 195 = 81 ∧ pct 122 195 = 63 ∧
    7 + 7 + 9 + 13 + 10 + 5 + 8 + 9 + 2 + 11 = 81 ∧
    5 + 3 + 7 + 1 + 5 + 5 + 2 + 2 + 4 + 3 = 37 ∧
    pct 119 189 = 63 ∧ 27 + 43 = 70 ∧ 16 + 17 = 33 ∧ 11 + 26 = 37 := by
  decide

/-! ## Apparent time: the cases of Chapter IX (PDF pp. 351 to 358) -/

/-- Case I-B, a stigmatised feature with change in progress, in the form Labov tests
on /ʌy/: below the top class the younger use less than the older, and the top class
uses none in either age (PDF p. 352). Values for classes low to high, younger and
older. -/
def CaseIB {k : ℕ} (young old : Fin (k + 1) → ℕ) : Prop :=
  (∀ c : Fin (k + 1), c ≠ Fin.last k → young c < old c) ∧
    young (Fin.last k) = 0 ∧ old (Fin.last k) = 0

/-- Case II-B, a prestige feature with change in progress, as Labov reads it from the
top and the middle-ranking groups: the top class younger above older, the
middle-ranking classes older above younger (PDF pp. 353 to 354). -/
def CaseIIB {k : ℕ} (young old : Fin (k + 2) → ℕ) : Prop :=
  old (Fin.last (k + 1)) < young (Fin.last (k + 1)) ∧
    young (Fin.castSucc (Fin.last k)) < old (Fin.castSucc (Fin.last k))

/-- Table 2 of Chapter IX: per cent using any /ʌy/, classes 0-1, 2-5, 6-8, 9, aged
20 to 39 and over 40 (PDF p. 365). -/
def ayYoung : Fin 4 → ℕ := ![75, 35, 9, 0]
def ayOld : Fin 4 → ℕ := ![85, 57, 35, 0]

theorem ay_caseIB : CaseIB ayYoung ayOld := by
  refine ⟨?_, rfl, rfl⟩
  decide

/-- Table 4 of Chapter IX: (r) in Style A for classes 0-1, 2-5, 6-8, 9, aged 20 to
39 and over 40 (PDF p. 369). -/
def rYoung : Fin 4 → ℕ := ![0, 0, 0, 34]
def rOld : Fin 4 → ℕ := ![0, 6, 9, 9]

theorem r_caseIIB : CaseIIB rYoung rOld := by unfold CaseIIB; decide

/-- Control: the classification separates the cases. The /ʌy/ data do not have the
Case II-B signature, since the top class does not rise in the younger group, and the
(r) data do not have the Case I-B signature. -/
theorem control_cases_distinct : ¬ CaseIIB ayYoung ayOld ∧ ¬ CaseIB rYoung rOld := by
  refine ⟨by unfold CaseIIB; decide, ?_⟩
  rintro ⟨h, _, _⟩
  exact absurd (h 0 (by decide)) (by decide)

/-- Table 3 of Chapter IX (PDF p. 366) recomputed. The row and column marginals add
up, 49 of 162 in all, and most printed percentages are the rounded fractions. Two are
not: 10 of 39 rounds to 26, printed 24, and 15 of 41 rounds to 37, printed 38. Table 2
prints N 16 for classes 2-5 aged 20 to 39, where Table 3 gives 7 + 10 = 17, and its
35 per cent is 6 of 17 (LV-D11). -/
theorem ay_table3_arithmetic :
    2 + 3 + 1 + 3 + 7 = 16 ∧ 7 + 4 + 3 + 3 + 7 = 24 ∧
    0 + 3 + 5 + 2 + 5 = 15 ∧ 11 + 7 + 14 + 4 + 5 = 41 ∧
    16 + 15 + 11 + 7 + 0 = 49 ∧ 24 + 41 + 34 + 44 + 19 = 162 ∧
    pct 2 51 = 4 ∧ pct 14 42 = 33 ∧ pct 10 17 = 59 ∧ pct 16 24 = 67 ∧
    pct 11 34 = 32 ∧ pct 7 44 = 16 ∧
    pct 10 39 = 26 ∧ pct 15 41 = 37 ∧
    pct (3 + 3) (7 + 10) = 35 ∧ pct 11 13 = 85 ∧ pct 20 35 = 57 ∧ pct 6 17 = 35 := by
  decide

/-! ## The subjective reaction test (Chapter XI) -/

/-- Ratings are positions on the scale of occupational suitability, higher meaning a
better job. A response is (oh)-negative when the (oh) sentences 6, 7 and 8 are rated no
higher than the same speakers' zero sentences 1, 2 and 5 (PDF p. 440). -/
def ohNegative (r1 r2 r5 r6 r7 r8 : ℕ) : Prop := r6 ≤ r1 ∧ r7 ≤ r2 ∧ r8 ≤ r5

/-- The two-choice test: the inconsistent (r-1) sentences 18 and 19 rated no higher
than the consistent 14 and 15 of the same speakers (PDF p. 453). -/
def rPositiveTwo (r14 r15 r18 r19 : ℕ) : Prop := r18 ≤ r14 ∧ r19 ≤ r15

/-- The four-choice test adds that the consistent sentences be rated no lower than the
speakers' zero sentences 2 and 4 (PDF p. 455). -/
def rPositiveFour (r2 r4 r14 r15 r18 r19 : ℕ) : Prop :=
  r2 ≤ r14 ∧ r4 ≤ r15 ∧ r18 ≤ r14 ∧ r19 ≤ r15

/-- (th)-sensitive: Sentence 21, read with fricatives, rated above both 20 and 22
(PDF p. 460). -/
def thSensitive (r20 r21 r22 : ℕ) : Prop := r20 < r21 ∧ r22 < r21

/-- The four-choice test is the "more difficult" one (PDF p. 456): every response it
counts as (r)-positive the two-choice test counts as well. -/
theorem four_choice_implies_two {r2 r4 r14 r15 r18 r19 : ℕ}
    (h : rPositiveFour r2 r4 r14 r15 r18 r19) : rPositiveTwo r14 r15 r18 r19 :=
  ⟨h.2.2.1, h.2.2.2⟩

/-- Control: the converse fails. A respondent who rates the consistent sentence below
the speaker's zero sentence passes the two-choice test and fails the four-choice. -/
theorem control_two_not_four :
    rPositiveTwo 3 3 3 3 ∧ ¬ rPositiveFour 5 3 3 3 3 3 := by
  refine ⟨⟨le_rfl, le_rfl⟩, ?_⟩
  rintro ⟨h, _⟩
  omega

/-- The printed totals respect the implication: four-choice 37, 88, 87, 48 against
two-choice 61, 100, 100, 62 for ages 8-17, 18-19, 20-39 and over 40 (Tables 11 and 12,
PDF pp. 454 to 455). -/
theorem tables11_12_order : 37 ≤ 61 ∧ 88 ≤ 100 ∧ 87 ≤ 100 ∧ 48 ≤ 62 := by decide

/-- PDF pp. 454 and 456. The choices of the four-choice test total four per respondent:
128 + 35 + 5 = 4 · 42 for ages 18 to 39 and 108 + 48 + 48 = 4 · 51 over 40. The
four-cell table gives 42 respondents aged 18 to 39, while Table 11's Ns for ages 18-19
and 20-39 sum to 8 + 32 = 40. Over 40 the four-cell table has 32 + 10 = 42, while the Ns
sum to 51 and 62 per cent of 51 is about 32, leaving 19 (r)-negative, not 10 (LV-D10). -/
theorem table11_arithmetic :
    128 + 35 + 5 = 4 * 42 ∧ 108 + 48 + 48 = 4 * 51 ∧
    (2 + 2 + 1 + 3) + (3 + 6 + 7 + 11 + 5) = 40 ∧
    8 + 18 + 8 + 10 + 7 = 51 ∧ 32 + 10 = 42 ∧ 51 - 32 = 19 ∧ pct 32 51 = 63 := by
  decide

/-- Table 2 of Chapter XI, per cent (oh)-negative by SEC 0 to 9 (classes 7-8
together), is highest in the lower middle class, class 6 (PDF p. 441). -/
def ohNegTable : Fin 9 → ℕ := ![37, 20, 13, 59, 56, 80, 100, 73, 58]

theorem ohNeg_max_lower_middle : ∀ c, ohNegTable c ≤ ohNegTable 6 := by decide

/-- Table 6 of Chapter XI, responses to Sentence 11 against Sentence 5: higher 15,
equal 13, lower 54 (PDF p. 447). -/
theorem table6_xi_sums :
    3 + 1 + 1 + 3 + 1 + 1 + 1 + 1 + 3 = 15 ∧ 2 + 0 + 0 + 4 + 0 + 1 + 3 + 2 + 1 = 13 ∧
    2 + 4 + 7 + 8 + 8 + 3 + 6 + 8 + 8 = 54 := by decide

/-- Table 16 of Chapter XI: the index of linguistic insecurity, per cent of each SEC
group (0-2, 3-5, 6-8, 9) with scores 0, 1-2, 3-7 and 8-13 (PDF p. 474). Each column
sums to 100, and the lower middle class has the largest share at 3 to 7. -/
def iliTable : Fin 4 → Fin 4 → ℕ :=
  ![![44, 50, 16, 20], ![25, 21, 16, 70], ![12, 25, 58, 10], ![19, 4, 10, 0]]

theorem table16_ili :
    (∀ g : Fin 4, iliTable 0 g + iliTable 1 g + iliTable 2 g + iliTable 3 g = 100) ∧
    (∀ g : Fin 4, iliTable 2 g ≤ iliTable 2 2) := by decide

/-- Control for the index of linguistic insecurity: a score of zero does not identify
the usage. Two respondents who report different usages, each calling his own usage
correct, both score zero. -/
theorem control_ili_zero_not_usage :
    ∃ c₁ c₂ : Fin 18 → Bool, c₁ ≠ c₂ ∧ ili c₁ c₁ = 0 ∧ ili c₂ c₂ = 0 := by
  refine ⟨fun _ => true, fun _ => false, ?_, ?_, ?_⟩
  · intro h; exact absurd (congrFun h 0) (by decide)
  · exact (ili_eq_zero_iff _ _).2 rfl
  · exact (ili_eq_zero_iff _ _).2 rfl

/-! ## Chapter XII co-variation (PDF pp. 489, 507, 518) -/

/-- Table 1 of Chapter XII: average (oh) rises across the five (eh) columns, 16, 19,
20, 23, 24. Table 3: average (oh) rises across the /ah/ options I, I/II-III, II, III,
17, 18, 24, 25. Table 7: the minimal-pair categories split the sample 33, 33, 34. -/
theorem xii_covariation :
    16 < 19 ∧ 19 < 20 ∧ 20 < 23 ∧ 23 < 24 ∧ 17 < 18 ∧ 18 < 24 ∧ 24 < 25 ∧
    33 + 33 + 34 = 100 := by decide

/-! ## The social class index of Chapter VIII and the (ay) index of Chapter X -/

/-- The social class index SC (PDF pp. 305 to 306), from the respondent's education
level `e` (0 to 8) and occupation level `o` (1 to 7). No year of high school gives SC 1;
otherwise professionals and proprietors (1 to 2) are SC 4, clerical and sales (3) SC 3,
and manual workers (4 to 7) SC 2. Income does not enter. -/
def sc (e o : ℕ) : ℕ := if e ≤ 3 then 1 else if o ≤ 2 then 4 else if o = 3 then 3 else 2

/-- Without a year of high school the occupation does not matter, and with it the
three occupational groups are separated. -/
theorem sc_structure (o : ℕ) : sc 3 o = 1 ∧ sc 4 1 = 4 ∧ sc 4 3 = 3 ∧ sc 4 5 = 2 := by
  simp [sc]

/-- PDF p. 415: the group (ay) index is the mean of the individual values times ten, so
five speakers with 0, 0, 1, 1, 2 give (ay)-08. -/
theorem ay_index_example : ehIndex [0, 0, 1, 1, 2] = 8 := by norm_num [ehIndex]

end Labov1964
