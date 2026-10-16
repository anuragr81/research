import Mathlib

/-! Crabtree, Gaddis, Holbein and Larsen (2022), "Racially Distinctive Names
Signal Both Race/Ethnicity and Social Class", Sociological Science 9, in the
paper's terms. Respondents give a perceived class for each name. Each name
carries an intended racial signal. The paper regresses perceptions on dummies
for the intended signal and plots group predictions (Figures 1, 3, 5) and
name-level predictions (Figures 2, 4).

Three structures are formalised. The sample and name counts the paper states
(pp. 454 to 458). Group sufficiency of a name, the property Paper A1 assumes of
its marker, and its failure when names within one intended group are perceived
differently, as Figures 2 and 4 show. And our reading of Study 5 (Figure 5): a
perceived class that mixes a stated education with the group's mean by a
common weight would give the same Black–White gap at every education level, so
the narrower gap at the Ivy League degree needs a weight on education that
rises with the degree. The paper does not state a model of perception. The
linear form is ours. -/

namespace CrabtreeEtAl2022

/-! ### Counts the paper states -/

/-- The five samples sum to the 11,530 respondents of p. 456. -/
theorem sample_total : 1004 + 2002 + 1035 + 5631 + 1858 = 11530 := by norm_num

/-- Studies 1 to 3 use 10 × 10 × 4 = 400 names, and Study 4 uses 80 White, 80
Black and 40 Hispanic first names with three last names each, 600 names. Study
5 draws its names from Study 4, so the distinct names number 1,000 (p. 456). -/
theorem names_total : 10 * 10 * 4 + (80 + 80 + 40) * 3 = 1000 ∧
    (80 * 3 = 240 ∧ 40 * 3 = 120) := by norm_num

/-! ### Group sufficiency of a name -/

variable {N G : Type*}

/-- A perception `m` of names is group-sufficient when names with the same
intended group are perceived alike. -/
def GroupSufficient (m : N → ℝ) (grp : N → G) : Prop :=
  ∀ a b, grp a = grp b → m a = m b

/-- Group sufficiency holds exactly when the perception factors through the
group. -/
theorem sufficient_iff_factor [Nonempty N] (m : N → ℝ) (grp : N → G) :
    GroupSufficient m grp ↔ ∃ f : G → ℝ, ∀ n, m n = f (grp n) := by
  classical
  constructor
  · intro h
    refine ⟨fun g => if hg : ∃ n, grp n = g then m (Classical.choose hg) else 0, fun n => ?_⟩
    have hg : ∃ n', grp n' = grp n := ⟨n, rfl⟩
    simp only [hg, dite_true]
    exact h _ _ (Classical.choose_spec hg).symm
  · rintro ⟨f, hf⟩ a b hab
    rw [hf a, hf b, hab]

/-- Under group sufficiency the names of one group have zero dispersion about
their mean. -/
theorem within_dispersion_zero (m : N → ℝ) (grp : N → G) (h : GroupSufficient m grp)
    (s : Finset N) (g : G) (hs : ∀ n ∈ s, grp n = g) (n₀ : N) (hn₀ : n₀ ∈ s) :
    ∑ n ∈ s, (m n - (∑ k ∈ s, m k) / s.card) ^ 2 = 0 := by
  have hc : ∀ n ∈ s, m n = m n₀ := fun n hn => h n n₀ (by rw [hs n hn, hs n₀ hn₀])
  have hcard : (s.card : ℝ) ≠ 0 := by
    have : 0 < s.card := Finset.card_pos.mpr ⟨n₀, hn₀⟩
    exact_mod_cast this.ne'
  have hsum : ∑ k ∈ s, m k = s.card * m n₀ := by
    rw [Finset.sum_congr rfl hc, Finset.sum_const, nsmul_eq_mul]
  rw [hsum, mul_div_cancel_left₀ _ hcard]
  apply Finset.sum_eq_zero
  intro n hn
  rw [hc n hn]; ring

/-- Control. Two names with the same intended group perceived at different
levels, as Roosevelt and Lakisha are in Figure 2, make the perception fail
group sufficiency. -/
theorem control_name_heterogeneity {a b : ℝ} (hab : a ≠ b) :
    ¬ GroupSufficient (![a, b] : Fin 2 → ℝ) (fun _ => ()) := by
  intro h
  exact hab (h 0 1 rfl)

/-! ### Our reading of Study 5 -/

/-- Perceived class of a named person described with education signal `s`, as
a mix with weight `w` on the signal and `1 − w` on the group mean `μ`. -/
def perc (w s μ : ℝ) : ℝ := w * s + (1 - w) * μ

/-- The White–Black gap at signal `s`. -/
def gap (w s μW μB : ℝ) : ℝ := perc w s μW - perc w s μB

/-- The gap is `(1 − w)(μW − μB)` and does not depend on the education stated. -/
theorem gap_eq (w s μW μB : ℝ) : gap w s μW μB = (1 - w) * (μW - μB) := by
  unfold gap perc; ring

/-- With a common weight the gap is the same at every education level. -/
theorem gap_indep_signal (w s t μW μB : ℝ) : gap w s μW μB = gap w t μW μB := by
  rw [gap_eq, gap_eq]

/-- A larger weight on the stated education gives a smaller gap. -/
theorem gap_shrinks {w₁ w₂ s t μW μB : ℝ} (h : w₁ < w₂) (hμ : μB < μW) :
    gap w₂ t μW μB < gap w₁ s μW μB := by
  rw [gap_eq, gap_eq]; nlinarith

/-- The gap vanishes exactly when the stated education takes the whole weight,
given a difference in group means. -/
theorem gap_zero_iff {w s μW μB : ℝ} (hμ : μW ≠ μB) : gap w s μW μB = 0 ↔ w = 1 := by
  rw [gap_eq, mul_eq_zero]
  constructor
  · rintro (h | h)
    · linarith
    · exact absurd (sub_eq_zero.mp h) hμ
  · intro h; left; linarith

/-- Control. Drop the full weight and the gap persists. With `w < 1` and a
higher White mean, the gap is positive at every education level, as Figure 5
shows even at the Ivy League degree. -/
theorem control_gap_persists {w s μW μB : ℝ} (hw : w < 1) (hμ : μB < μW) :
    0 < gap w s μW μB := by
  rw [gap_eq]; nlinarith

end CrabtreeEtAl2022
