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

/-! ### The estimating model (p. 6)

"ordinary least squares models that regress respondent answers ... on a set of
dummy variables that indicate the racial signal each name was intended to
convey". With one dummy per group, the fitted value for group `g` is a free
coefficient `β g`, and least squares minimises the sum of squared residuals. -/

section OLS

variable {n : ℕ} {K : Type*} [Fintype K] [DecidableEq K]

/-- The sum of squared residuals of the dummy specification. -/
def ssr (y : Fin n → ℝ) (grp : Fin n → K) (β : K → ℝ) : ℝ := ∑ i, (y i - β (grp i)) ^ 2

/-- The number of answers about names of group `g`. -/
def cnt (grp : Fin n → K) (g : K) : ℝ := ∑ i, if grp i = g then (1 : ℝ) else 0

/-- The mean answer about names of group `g`. -/
noncomputable def groupMean (y : Fin n → ℝ) (grp : Fin n → K) (g : K) : ℝ :=
  (∑ i, if grp i = g then y i else 0) / cnt grp g

omit [Fintype K] in
/-- Within each group, the residuals from the group mean sum to zero. -/
theorem resid_sum_zero (y : Fin n → ℝ) (grp : Fin n → K) (g : K) :
    ∑ i, (if grp i = g then y i - groupMean y grp g else 0) = 0 := by
  have e : ∑ i, (if grp i = g then y i - groupMean y grp g else 0)
      = (∑ i, if grp i = g then y i else 0) - groupMean y grp g * cnt grp g := by
    unfold cnt
    rw [Finset.mul_sum, ← Finset.sum_sub_distrib]
    refine Finset.sum_congr rfl (fun i _ => ?_)
    split_ifs <;> ring
  rw [e]
  by_cases hc : cnt grp g = 0
  · have hz : (∑ i, if grp i = g then y i else 0) = 0 := by
      have hall : ∀ i, grp i ≠ g := by
        intro i hi
        have hle : (1 : ℝ) ≤ cnt grp g := by
          unfold cnt
          calc (1 : ℝ) = if grp i = g then (1 : ℝ) else 0 := by simp [hi]
            _ ≤ ∑ j, if grp j = g then (1 : ℝ) else 0 :=
              Finset.single_le_sum (f := fun j => if grp j = g then (1 : ℝ) else 0)
                (fun j _ => by dsimp only; split_ifs <;> norm_num) (Finset.mem_univ i)
        linarith
      exact Finset.sum_eq_zero (fun i _ => by simp [hall i])
    rw [hz, hc]; ring
  · unfold groupMean; field_simp; ring

/-- The cross term vanishes. For any coefficients, the residuals from the group
means are orthogonal to any function of the group. -/
theorem cross_zero (y : Fin n → ℝ) (grp : Fin n → K) (d : K → ℝ) :
    ∑ i, (y i - groupMean y grp (grp i)) * d (grp i) = 0 := by
  have h : ∀ i, (y i - groupMean y grp (grp i)) * d (grp i)
      = ∑ g, d g * (if grp i = g then y i - groupMean y grp g else 0) := by
    intro i
    rw [Finset.sum_eq_single (grp i)]
    · simp; ring
    · intro b _ hb; simp [Ne.symm hb]
    · intro h; exact absurd (Finset.mem_univ _) h
  rw [Finset.sum_congr rfl (fun i _ => h i), Finset.sum_comm]
  refine Finset.sum_eq_zero (fun g _ => ?_)
  rw [← Finset.mul_sum, resid_sum_zero, mul_zero]

/-- The least squares fit of the dummy specification is the group means. Every
other choice of coefficients leaves a larger or equal sum of squared residuals,
so the plotted predictions of Figures 1, 3 and 5 are the mean answers by
intended group. -/
theorem ols_dummies_group_means (y : Fin n → ℝ) (grp : Fin n → K) (β : K → ℝ) :
    ssr y grp (groupMean y grp) ≤ ssr y grp β := by
  have e : ssr y grp β = ssr y grp (groupMean y grp)
      + ∑ i, (groupMean y grp (grp i) - β (grp i)) ^ 2
      + 2 * ∑ i, (y i - groupMean y grp (grp i)) * (groupMean y grp (grp i) - β (grp i)) := by
    unfold ssr
    rw [Finset.mul_sum, ← Finset.sum_add_distrib, ← Finset.sum_add_distrib]
    refine Finset.sum_congr rfl (fun i _ => ?_)
    ring
  rw [e, cross_zero y grp (fun g => groupMean y grp g - β g)]
  have : 0 ≤ ∑ i, (groupMean y grp (grp i) - β (grp i)) ^ 2 :=
    Finset.sum_nonneg (fun i _ => sq_nonneg _)
  linarith

/-- Control. Drop least squares as the criterion and the group mean is not the
fit. Under least absolute deviations, the answers 1, 1, 3 of one group are
fitted better by 1 than by their mean 5/3. -/
theorem control_lad_not_mean :
    |(1 : ℝ) - 1| + |1 - 1| + |3 - 1| < |(1 : ℝ) - 5 / 3| + |1 - 5 / 3| + |3 - 5 / 3| := by
  norm_num [abs_of_neg, abs_of_pos]

end OLS

/-! ### Choosing racially distinctive names (p. 5)

A name counts as distinctive for group `g` when the share of its users in `g`
is at least the threshold `θ g`: 0.9 for most first and last names, 0.8 for
Black first names and Hispanic last names, 0.45 for Black last names. -/

/-- The selection rule. -/
def distinctive (share θ : ℝ) : Prop := θ ≤ share

/-- With thresholds above one half, a name is distinctive for at most one of
two groups, since the shares sum to at most one. -/
theorem distinctive_unique {s₁ s₂ θ₁ θ₂ : ℝ} (hsum : s₁ + s₂ ≤ 1)
    (h₁ : 1 / 2 < θ₁) (h₂ : 1 / 2 < θ₂) : ¬ (distinctive s₁ θ₁ ∧ distinctive s₂ θ₂) := by
  unfold distinctive; rintro ⟨a, b⟩; linarith

/-- Control. Drop the threshold above one half and a "Black" surname can be
used mostly by White people. A surname used 45% by Black and 50% by White
people passes the 0.45 rule for Black last names. -/
theorem control_black_surname_threshold :
    distinctive (45 / 100) (45 / 100) ∧ (45 / 100 : ℝ) < 50 / 100 := by
  unfold distinctive; norm_num

/-! ### Study 5, the interaction (p. 9)

Cell means `m r e` by intended race `r` and stated education `e`. The gap at
`e` is `m W e − m B e`, and "reduced considerably" compares gaps across
education levels, a difference in differences. -/

/-- The Black–White gap at education `e`, for cell means `mW`, `mB`. -/
def cellGap {E : Type*} (mW mB : E → ℝ) (e : E) : ℝ := mW e - mB e

/-- The change in the gap between two education levels is the difference in
differences of the four cell means. -/
theorem gap_change_did {E : Type*} (mW mB : E → ℝ) (e e' : E) :
    cellGap mW mB e - cellGap mW mB e' = (mW e - mW e') - (mB e - mB e') := by
  unfold cellGap; ring

/-- A change in the gap across education levels rules out every common-weight
mixture. If the cell means came from `perc w (s e) μ` with one `w`, the gap
could not change. -/
theorem varying_gap_needs_varying_weight {E : Type*} (mW mB : E → ℝ) (s : E → ℝ) (e e' : E)
    (hne : cellGap mW mB e ≠ cellGap mW mB e') :
    ¬ ∃ w μW μB, ∀ x, mW x = perc w (s x) μW ∧ mB x = perc w (s x) μB := by
  rintro ⟨w, μW, μB, h⟩
  apply hne
  unfold cellGap
  rw [(h e).1, (h e).2, (h e').1, (h e').2]
  exact gap_indep_signal w (s e) (s e') μW μB

end CrabtreeEtAl2022
