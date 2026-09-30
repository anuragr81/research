/-
# Zhao & Osherson (2010), "Updating beliefs in light of uncertain evidence: Descriptive assessment of Jeffrey's rule"

*Thinking & Reasoning* 16(4), 288-307.  DOI 10.1080/13546783.2010.521695.

Formalization of the paper's own normative framework (pp. 288-291) and a check
of the summary numbers it reports and the project quotes
(`literature/measurement_susceptibility_survey.md`; `notes/citation_audit.md`
item L10; `notes/citation_audit/verify_record_papers.md` §3).  ZO is an
empirical paper; its formal content is the textbook account of Jeffrey's rule
that it tests.

## Terminology (p.289 and footnote 1)

ZO call the condition they test **invariance**, attributing it to Jeffrey
(2004, §3.2): "If experience has not influenced the conditional probability of
G given B nor that of G given B̄ then invariance is said to hold".  Footnote 1:
"Many authors, including Oaksford and Chater (2007) and Over and
Hadjichristidis (2009) use the term 'rigidity' instead of 'invariance'."  The
names below follow ZO.

## The model

Two binary events `B` (the card is blue) and `G` (a giraffe is on it), so a
distribution is four cell weights `P b g` (`b`: whether `B` holds, `g`:
whether `G` holds).

  * Eq. (1) SIMPLE UPDATING, `Pr₂(A) = Pr₁(A | B)`: `jeffrey P 1` (`simple_updating`).
  * Eq. (2) total probability: `total_probability`.
  * Eq. (3) INVARIANCE FOR G, B: `Pr₂(G|B) = Pr₁(G|B)` and `Pr₂(G|B̄) = Pr₁(G|B̄)`:
    `Invariance`.
  * Eq. (4) GENERALISED UPDATING (Jeffrey's rule),
    `Pr₂(G) = Pr₁(G|B) Pr₂(B) + Pr₁(G|B̄) Pr₂(B̄)`: `jeffrey`, `jeffrey_rule`.
  * Eq. (5) Pearl's criterion, `Pr₁(G | B, e) = Pr₁(G | B)`: `Pearl`, with a
    third binary variable `e` (the experience).
  * Eqs. (6)/(7): invariance violation and converse movement,
    `|Pr₂(·) − Pr₁(·)| / Pr₁(·)`: `movement`.

## What is proved

  * `jeffrey_isDist`, `jeffrey_prB` — (4) defines a genuine distribution with
    `Pr₂(B) = q` (p.290, "it can be demonstrated that (4) defines a genuine
    probability distribution Pr₂, and that Pr₂(B) is set to its new value").
  * `jeffrey_invariance` — **Jeffrey's rule on the partition {B, B̄} preserves
    `Pr(G|B)` and `Pr(G|B̄)`**, i.e. satisfies (3).
  * `invariance_imp_jeffrey` — conversely, "substituting (3) into (2) yields
    (4)" (p.290): every distribution with `Pr₂(B) = q` satisfying (3) is the
    Jeffrey update, cell by cell.
  * `simple_updating` — at `q = 1` Jeffrey's rule is conditioning, rule (1)
    (p.290).
  * `jeffrey_converse_odds`, `converse_invariant_iff` — **but it does not
    preserve the converse `Pr(B|G)`**: the odds of `B` given `G` are multiplied
    by exactly the factor that multiplies the odds of `B`, so (for
    `0 < Pr₁(G|B), Pr₁(G|B̄)`) `Pr₂(B|G) = Pr₁(B|G)` iff `Pr₂(B) = Pr₁(B)`.
    `converse_at_independence`: even when `G` is independent of `B`,
    `Pr₂(B|G) = q` moves with `Pr₂(B)`.  `jeffrey_movement` restates this in
    ZO's measures (6)/(7): invariance violation `0`, converse movement `> 0`.
  * `pearl_invariance_iff`, `pearl_jeffrey` — ZO's normative argument (p.291):
    if `Pr₂` is `Pr₁` conditioned on the experience `e`, then `Pr₂(G|B)` *is*
    `Pr₁(G|B,e)`, so invariance ⟺ (5); and under (5) for both `B` and `B̄` the
    posterior on `(B, G)` is exactly the Jeffrey update with `q = Pr₁(B|e)`.
  * **Explicit instance on ZO's own deck** (Table 1, p.292: blue giraffe 8,
    blue hippo 4, purple giraffe 10, purple hippo 28): `deck_objective` checks
    the "Objective" row of Table 2 (0.36, 0.24, 0.67, 0.26, 0.44, 0.13);
    `deck_jeffrey_instance` updates `Pr(B)` from 0.24 to 0.75 (Jeffrey's value
    in the candle example, p.289): `Pr(G|B) = 2/3` and `Pr(G|B̄) = 5/19` stay
    put, `Pr(B|G)` moves from `4/9` to `38/43`.
  * `rain_example` — p.290: `.6 × .8 + .1 × .2 = .5`.
  * **Reported counts and averages** (`exp1_*`, `exp2_*`): 22 of 40 changed
    `Pr(G|B)` (11 Blue + 11 Purple, a majority), 18 of 40 changed `Pr(G|B̄)`
    (8 + 10), so 40 of 80 conditional judgments moved; 32 of 40 changed the
    converse `Pr(B|G)` (15 + 17); 33.0% vs 73.0% are the means of the Blue and
    Purple group means (23.5, 42.5; 82.3, 63.7); 27.73% is the mean of the four
    Experiment 1 violation means; 0.18 vs 0.45 after 5 outliers (N = 38 and 37
    in Fig. 1, t(34)); Experiment 2's 71 of 100 (33 + 38) and "less than 2%"
    (1.42%, 1.33%).
  * `exp2_ultimatum_mean_slip` — **one printed number does not reproduce**:
    the ultimatum mean violation "18.45%" (p.304) is not the mean of the two
    reported means 18.71% and 17.38% (that is 18.045%), although the companion
    figure 27.73% for Experiment 1 is exactly the mean of its four reported
    means.  Only a roughly 4 : 1 weighting of the two questions would give
    18.45 (`exp2_ultimatum_weight`).  Not a number the project uses.

## What is not formalized

The inferential statistics (t-tests, Wilcoxon tests; the binomial tests are
recomputed in `literature/zhao_osherson2010/sympy/check_zo.py`); the
regressions; finer partitions (ZO fn 2 restrict themselves to the binary case).
-/
import Mathlib

namespace Literature.ZhaoOsherson

/-! ## Distributions on two binary events -/

/-- A (candidate) distribution on the four cells: `P b g` is the weight of
`B = b ∧ G = g`. -/
abbrev Dist := Bool → Bool → ℝ

/-- `P` is a probability distribution. -/
def IsDist (P : Dist) : Prop :=
  (∀ b g, 0 ≤ P b g) ∧ P true true + P true false + P false true + P false false = 1

/-- `Pr(B)`. -/
def prB (P : Dist) : ℝ := P true true + P true false
/-- `Pr(B̄)`. -/
def prNB (P : Dist) : ℝ := P false true + P false false
/-- `Pr(G)`. -/
def prG (P : Dist) : ℝ := P true true + P false true
/-- `Pr(Ḡ)`. -/
def prNG (P : Dist) : ℝ := P true false + P false false
/-- `Pr(G | B)`. -/
noncomputable def condGB (P : Dist) : ℝ := P true true / prB P
/-- `Pr(G | B̄)`. -/
noncomputable def condGNB (P : Dist) : ℝ := P false true / prNB P
/-- `Pr(B | G)`, the converse of `Pr(G | B)`. -/
noncomputable def condBG (P : Dist) : ℝ := P true true / prG P
/-- `Pr(B | Ḡ)`. -/
noncomputable def condBNG (P : Dist) : ℝ := P true false / prNG P

theorem prNB_eq (P : Dist) (h : IsDist P) : prNB P = 1 - prB P := by
  unfold prNB prB; linarith [h.2]

/-- **Eq. (2)**, the law of total probability (p.289):
`Pr(G) = Pr(G|B) Pr(B) + Pr(G|B̄) Pr(B̄)`. -/
theorem total_probability (P : Dist) (hB : prB P ≠ 0) (hNB : prNB P ≠ 0) :
    prG P = condGB P * prB P + condGNB P * prNB P := by
  unfold condGB condGNB prG
  field_simp

/-- **Eq. (3)**, INVARIANCE FOR G, B (p.289), relating `P₁` and `P₂`. -/
def Invariance (P₁ P₂ : Dist) : Prop :=
  condGB P₂ = condGB P₁ ∧ condGNB P₂ = condGNB P₁

/-! ## Jeffrey's rule -/

/-- Jeffrey's rule on the partition `{B, B̄}` with new weight `q = Pr₂(B)`:
each cell inside `B` is rescaled by `q / Pr₁(B)`, each inside `B̄` by
`(1 - q) / Pr₁(B̄)`. -/
noncomputable def jeffrey (P : Dist) (q : ℝ) : Dist := fun b g =>
  if b then q * P b g / prB P else (1 - q) * P b g / prNB P

@[simp] theorem jeffrey_true (P : Dist) (q : ℝ) (g : Bool) :
    jeffrey P q true g = q * P true g / prB P := rfl

@[simp] theorem jeffrey_false (P : Dist) (q : ℝ) (g : Bool) :
    jeffrey P q false g = (1 - q) * P false g / prNB P := rfl

/-- (4) sets `Pr₂(B)` to its new value `q` (p.290). -/
theorem jeffrey_prB (P : Dist) (q : ℝ) (hB : prB P ≠ 0) : prB (jeffrey P q) = q := by
  have : P true true + P true false ≠ 0 := hB
  simp only [prB, jeffrey_true]
  field_simp

theorem jeffrey_prNB (P : Dist) (q : ℝ) (hNB : prNB P ≠ 0) :
    prNB (jeffrey P q) = 1 - q := by
  have : P false true + P false false ≠ 0 := hNB
  simp only [prNB, jeffrey_false]
  field_simp

/-- (4) "defines a genuine probability distribution" (p.290). -/
theorem jeffrey_isDist (P : Dist) (hP : IsDist P) (q : ℝ) (hq0 : 0 ≤ q) (hq1 : q ≤ 1)
    (hB : 0 < prB P) (hNB : 0 < prNB P) : IsDist (jeffrey P q) := by
  refine ⟨fun b g => ?_, ?_⟩
  · cases b
    · simp only [jeffrey_false]
      exact div_nonneg (mul_nonneg (by linarith) (hP.1 _ _)) hNB.le
    · simp only [jeffrey_true]
      exact div_nonneg (mul_nonneg hq0 (hP.1 _ _)) hB.le
  · have h1 := jeffrey_prB P q hB.ne'
    have h2 := jeffrey_prNB P q hNB.ne'
    unfold prB at h1; unfold prNB at h2
    linarith

/-- **Jeffrey's rule satisfies invariance (3)**: updating the partition
`{B, B̄}` leaves `Pr(G|B)` and `Pr(G|B̄)` unchanged (for `0 < q < 1`). -/
theorem jeffrey_invariance (P : Dist) (q : ℝ) (hq0 : q ≠ 0) (hq1 : q ≠ 1)
    (hB : prB P ≠ 0) (hNB : prNB P ≠ 0) : Invariance P (jeffrey P q) := by
  have hq1' : 1 - q ≠ 0 := sub_ne_zero.mpr (Ne.symm hq1)
  constructor
  · unfold condGB; rw [jeffrey_prB P q hB]; simp only [jeffrey_true]
    field_simp
  · unfold condGNB; rw [jeffrey_prNB P q hNB]; simp only [jeffrey_false]
    field_simp

/-- **Eq. (4), Jeffrey's rule** (p.290):
`Pr₂(G) = Pr₁(G|B) Pr₂(B) + Pr₁(G|B̄) Pr₂(B̄)`. -/
theorem jeffrey_rule (P : Dist) (q : ℝ) :
    prG (jeffrey P q) = condGB P * q + condGNB P * (1 - q) := by
  simp only [prG, condGB, condGNB, jeffrey_true, jeffrey_false]
  ring

/-- **"Substituting (3) into (2) yields (4)"** (p.290), in its strongest form:
a distribution `Q` with `Q(B) = q ∈ (0,1)` that satisfies invariance relative
to `P` *is* the Jeffrey update of `P`, cell by cell. -/
theorem invariance_imp_jeffrey (P Q : Dist) (hQ : IsDist Q) (q : ℝ)
    (hq0 : q ≠ 0) (hq1 : q ≠ 1) (hB : prB P ≠ 0) (hNB : prNB P ≠ 0)
    (hQB : prB Q = q) (hinv : Invariance P Q) : ∀ b g, Q b g = jeffrey P q b g := by
  subst hQB
  have hs : 1 - prB Q = prNB Q := (prNB_eq Q hQ).symm
  have hNBQ : prNB Q ≠ 0 := by rw [← hs]; exact sub_ne_zero.mpr (Ne.symm hq1)
  obtain ⟨h1, h2⟩ := hinv
  simp only [condGB, condGNB] at h1 h2
  rw [div_eq_div_iff hq0 hB] at h1
  rw [div_eq_div_iff hNBQ hNB] at h2
  intro b g
  cases b <;> cases g
  · simp only [jeffrey_false]; rw [hs, eq_div_iff hNB]
    simp only [prNB] at h2 ⊢; linear_combination (-1 : ℝ) * h2
  · simp only [jeffrey_false]; rw [hs, eq_div_iff hNB]
    simp only [prNB] at h2 ⊢; linear_combination h2
  · simp only [jeffrey_true]; rw [eq_div_iff hB]
    simp only [prB] at h1 ⊢; linear_combination (-1 : ℝ) * h1
  · simp only [jeffrey_true]; rw [eq_div_iff hB]
    simp only [prB] at h1 ⊢; linear_combination h1

/-- **Rule (1), SIMPLE UPDATING, is the case `q = 1`** (p.290: "in the special
case of Pr₂(B) = 1, Pr₂ agrees with the result of applying conditionalisation"):
every cell outside `B` gets weight 0 and `Pr₂(G) = Pr₁(G|B)`. -/
theorem simple_updating (P : Dist) :
    (∀ g, jeffrey P 1 false g = 0) ∧ prG (jeffrey P 1) = condGB P := by
  refine ⟨fun g => by simp, ?_⟩
  rw [jeffrey_rule]; ring

/-! ## The converse `Pr(B|G)` is not invariant -/

/-- Under Jeffrey's rule the **odds of `B` given `G`** are
`q/(1-q) · Pr₁(G|B)/Pr₁(G|B̄)`: the converse moves by exactly the factor by
which the odds of `B` move. -/
theorem jeffrey_converse_odds (P : Dist) (q : ℝ) :
    jeffrey P q true true / jeffrey P q false true
      = (q / (1 - q)) * (condGB P / condGNB P) := by
  simp only [jeffrey_true, jeffrey_false, condGB, condGNB, div_eq_mul_inv, mul_inv, inv_inv]
  ring

/-- The converse under Jeffrey's rule, in terms of `P`'s likelihoods. -/
theorem jeffrey_condBG (P : Dist) (q : ℝ) :
    condBG (jeffrey P q) = q * condGB P / (q * condGB P + (1 - q) * condGNB P) := by
  simp only [condBG, prG, jeffrey_true, jeffrey_false, condGB, condGNB]
  ring

/-- The same formula for `P` itself, with `p = Pr₁(B)`. -/
theorem condBG_eq (P : Dist) (hP : IsDist P) (hB : prB P ≠ 0) (hNB : prNB P ≠ 0) :
    condBG P = prB P * condGB P / (prB P * condGB P + (1 - prB P) * condGNB P) := by
  rw [← prNB_eq P hP]
  simp only [condBG, prG, condGB, condGNB]
  field_simp

/-- **The converse is invariant only if `Pr(B)` does not move.**  When both
likelihoods `Pr₁(G|B)`, `Pr₁(G|B̄)` are positive and `0 < Pr₁(B), q < 1`,
`Pr₂(B|G) = Pr₁(B|G)` iff `q = Pr₁(B)`.  So the update that ZO's flashlight
effects on `Pr(B)` (p.294, p.296) must, normatively, move `Pr(B|G)`: this is
the "converse movement" they contrast with invariance (p.293, pp.294-295). -/
theorem converse_invariant_iff (P : Dist) (hP : IsDist P) (q : ℝ)
    (hB0 : 0 < prB P) (hB1 : prB P < 1) (hq0 : 0 < q) (hq1 : q < 1)
    (ha : 0 < condGB P) (hc : 0 < condGNB P) :
    condBG (jeffrey P q) = condBG P ↔ q = prB P := by
  have hNB : prNB P ≠ 0 := by rw [prNB_eq P hP]; linarith
  rw [jeffrey_condBG, condBG_eq P hP hB0.ne' hNB]
  set p := prB P
  set a := condGB P
  set c := condGNB P
  have hd1 : q * a + (1 - q) * c ≠ 0 := by nlinarith
  have hd2 : p * a + (1 - p) * c ≠ 0 := by nlinarith
  rw [div_eq_div_iff hd1 hd2]
  constructor
  · intro h
    have : a * c * (q - p) = 0 := by linear_combination h
    rcases mul_eq_zero.mp this with h' | h'
    · rcases mul_eq_zero.mp h' with h'' | h'' <;> linarith
    · linarith
  · intro h; rw [h]

/-- Corollary: whenever the flashlight moves `Pr(B)`, Jeffrey's rule moves the
converse `Pr(B|G)`. -/
theorem converse_moves (P : Dist) (hP : IsDist P) (q : ℝ)
    (hB0 : 0 < prB P) (hB1 : prB P < 1) (hq0 : 0 < q) (hq1 : q < 1)
    (ha : 0 < condGB P) (hc : 0 < condGNB P) (hq : q ≠ prB P) :
    condBG (jeffrey P q) ≠ condBG P :=
  fun h => hq ((converse_invariant_iff P hP q hB0 hB1 hq0 hq1 ha hc).mp h)

/-- Even when `G` is independent of `B` (`Pr₁(G|B) = Pr₁(G|B̄) > 0`), the
converse is not invariant: it becomes `q`. -/
theorem converse_at_independence (P : Dist) (q : ℝ) (hind : condGB P = condGNB P)
    (ha : 0 < condGB P) : condBG (jeffrey P q) = q := by
  rw [jeffrey_condBG, ← hind]
  field_simp
  ring

/-- ZO's measures (6)/(7): the movement of a probability as a fraction of its
original value, `|x₂ − x₁| / x₁`. -/
noncomputable def movement (x₁ x₂ : ℝ) : ℝ := |x₂ - x₁| / x₁

/-- **In ZO's own measures**: under Jeffrey's rule the invariance violations (6)
and (8) are zero and the converse movement (7) is positive whenever `Pr(B)`
moves. -/
theorem jeffrey_movement (P : Dist) (hP : IsDist P) (q : ℝ)
    (hB0 : 0 < prB P) (hB1 : prB P < 1) (hq0 : 0 < q) (hq1 : q < 1)
    (ha : 0 < condGB P) (hc : 0 < condGNB P) (hq : q ≠ prB P)
    (hBG : 0 < condBG P) :
    movement (condGB P) (condGB (jeffrey P q)) = 0 ∧
    movement (condGNB P) (condGNB (jeffrey P q)) = 0 ∧
    0 < movement (condBG P) (condBG (jeffrey P q)) := by
  have hNB : prNB P ≠ 0 := by rw [prNB_eq P hP]; linarith
  obtain ⟨h1, h2⟩ := jeffrey_invariance P q hq0.ne' hq1.ne hB0.ne' hNB
  refine ⟨by simp [movement, h1], by simp [movement, h2], ?_⟩
  unfold movement
  have hne := converse_moves P hP q hB0 hB1 hq0 hq1 ha hc hq
  exact div_pos (abs_pos.mpr (sub_ne_zero.mpr hne)) hBG

/-! ## Pearl's criterion, eq. (5) (p.291) -/

/-- A distribution over `B`, `G` and the experience `e`: `R b g e`. -/
abbrev Dist3 := Bool → Bool → Bool → ℝ

/-- The time-1 marginal on `(B, G)`. -/
def marg (R : Dist3) : Dist := fun b g => R b g true + R b g false

/-- `Pr₁(e)`. -/
def prE (R : Dist3) : ℝ := R true true true + R true false true + R false true true + R false false true

/-- `Pr₂ = Pr₁(· | e)` on `(B, G)`: the experience `e` is what transpires
between times 1 and 2 (p.291). -/
noncomputable def post (R : Dist3) : Dist := fun b g => R b g true / prE R

/-- `Pr₁(G | B = b, e)`. -/
noncomputable def condGBe (R : Dist3) (b : Bool) : ℝ := R b true true / (R b true true + R b false true)

/-- **Eq. (5)**, conditional independence of `G` from `e` given `B = b`:
`Pr₁(G | b, e) = Pr₁(G | b)`. -/
def Pearl (R : Dist3) (b : Bool) : Prop :=
  condGBe R b = marg R b true / (marg R b true + marg R b false)

/-- `Pr₂(G | B) = Pr₁(G | B, e)`: the first step of ZO's argument (p.291). -/
theorem post_condGB (R : Dist3) (hE : prE R ≠ 0) : condGB (post R) = condGBe R true := by
  simp only [condGB, prB, post, condGBe]
  rw [← add_div, div_div_div_cancel_right₀ hE]

theorem post_condGNB (R : Dist3) (hE : prE R ≠ 0) : condGNB (post R) = condGBe R false := by
  simp only [condGNB, prNB, post, condGBe]
  rw [← add_div, div_div_div_cancel_right₀ hE]

/-- **"Invariance and the conditional independence expressed by (5) are
equivalent"** (p.291). -/
theorem pearl_invariance_iff (R : Dist3) (hE : prE R ≠ 0) :
    Invariance (marg R) (post R) ↔ Pearl R true ∧ Pearl R false := by
  unfold Invariance Pearl
  rw [post_condGB R hE, post_condGNB R hE]
  simp only [condGB, condGNB, prB, prNB]

/-- The posterior sums to one. -/
theorem post_sum (R : Dist3) (hE : prE R ≠ 0) : prB (post R) + prNB (post R) = 1 := by
  simp only [prB, prNB, post]
  field_simp
  unfold prE; ring

/-- **Under (5) for both cells of the partition, conditioning on the
experience is Jeffrey's rule** with `q = Pr₁(B | e)`: the posterior on `(B, G)`
is the Jeffrey update of the prior marginal (p.291, with Pearl 1988 §2.3.3). -/
theorem pearl_jeffrey (R : Dist3) (hE : prE R ≠ 0)
    (hB : prB (marg R) ≠ 0) (hNB : prNB (marg R) ≠ 0)
    (hBe : prB (post R) ≠ 0) (hNBe : prNB (post R) ≠ 0)
    (hT : Pearl R true) (hF : Pearl R false) :
    ∀ b g, post R b g = jeffrey (marg R) (prB (post R)) b g := by
  obtain ⟨h1, h2⟩ := (pearl_invariance_iff R hE).mpr ⟨hT, hF⟩
  simp only [condGB, condGNB] at h1 h2
  rw [div_eq_div_iff hBe hB] at h1
  rw [div_eq_div_iff hNBe hNB] at h2
  have hs : 1 - prB (post R) = prNB (post R) := by linarith [post_sum R hE]
  intro b g
  cases b <;> cases g
  · simp only [jeffrey_false]; rw [hs, eq_div_iff hNB]
    simp only [prNB] at h2 ⊢; linear_combination (-1 : ℝ) * h2
  · simp only [jeffrey_false]; rw [hs, eq_div_iff hNB]
    simp only [prNB] at h2 ⊢; linear_combination h2
  · simp only [jeffrey_true]; rw [eq_div_iff hB]
    simp only [prB] at h1 ⊢; linear_combination (-1 : ℝ) * h1
  · simp only [jeffrey_true]; rw [eq_div_iff hB]
    simp only [prB] at h1 ⊢; linear_combination h1

/-! ## Explicit instance: ZO's Experiment 1 deck (Table 1, p.292) -/

/-- Table 1 (p.292) as frequencies over the 50 cards: blue giraffe 8, blue
hippo 4, purple giraffe 10, purple hippo 28 (`b`: blue, `g`: giraffe). -/
noncomputable def deck : Dist := fun b g =>
  match b, g with
  | true, true => 8 / 50
  | true, false => 4 / 50
  | false, true => 10 / 50
  | false, false => 28 / 50

theorem deck_isDist : IsDist deck := by
  refine ⟨fun b g => ?_, ?_⟩
  · cases b <;> cases g <;> norm_num [deck]
  · norm_num [deck]

/-- Exact objective values of the six probabilities ZO elicit. -/
theorem deck_exact :
    prG deck = 9 / 25 ∧ prB deck = 6 / 25 ∧ condGB deck = 2 / 3 ∧
    condGNB deck = 5 / 19 ∧ condBG deck = 4 / 9 ∧ condBNG deck = 1 / 8 := by
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_⟩ <;>
    norm_num [prG, prB, prNB, prNG, condGB, condGNB, condBG, condBNG, deck]

/-- **The "Objective" row of Table 2** (p.294): 0.36, 0.24, 0.67, 0.26, 0.44,
0.13, each the exact value rounded to two decimals (`1/8 = 0.125` rounds up to
0.13). -/
theorem deck_objective :
    |prG deck - 0.36| ≤ 0.005 ∧ |prB deck - 0.24| ≤ 0.005 ∧
    |condGB deck - 0.67| ≤ 0.005 ∧ |condGNB deck - 0.26| ≤ 0.005 ∧
    |condBG deck - 0.44| ≤ 0.005 ∧ |condBNG deck - 0.13| ≤ 0.005 := by
  obtain ⟨h1, h2, h3, h4, h5, h6⟩ := deck_exact
  rw [h1, h2, h3, h4, h5, h6]
  norm_num [abs_le]

/-- **The explicit instance.**  Jeffrey-update ZO's deck on `{blue, purple}`
from `Pr(blue) = 0.24` to `0.75` (the value in Jeffrey's candle example as ZO
report it, p.289): `Pr(G|B) = 2/3` and `Pr(G|B̄) = 5/19` are unchanged, while the
converse `Pr(B|G)` moves from `4/9 ≈ 0.44` to `38/43 ≈ 0.88`. -/
theorem deck_jeffrey_instance :
    prB (jeffrey deck (3 / 4)) = 3 / 4 ∧
    condGB (jeffrey deck (3 / 4)) = 2 / 3 ∧ condGNB (jeffrey deck (3 / 4)) = 5 / 19 ∧
    condBG deck = 4 / 9 ∧ condBG (jeffrey deck (3 / 4)) = 38 / 43 ∧
    condBG (jeffrey deck (3 / 4)) ≠ condBG deck := by
  have hB : prB deck ≠ 0 := by norm_num [prB, deck]
  have hNB : prNB deck ≠ 0 := by norm_num [prNB, deck]
  obtain ⟨i1, i2⟩ := jeffrey_invariance deck (3 / 4) (by norm_num) (by norm_num) hB hNB
  obtain ⟨-, -, e3, e4, e5, -⟩ := deck_exact
  have c : condBG (jeffrey deck (3 / 4)) = 38 / 43 := by
    rw [jeffrey_condBG, e3, e4]; norm_num
  refine ⟨jeffrey_prB deck _ hB, by rw [i1, e3], by rw [i2, e4], e5, c, ?_⟩
  rw [c, e5]; norm_num

/-- The rain example (p.290): `Pr₂(cancelled) = .6 × .8 + .1 × .2 = .5`. -/
theorem rain_example : (0.6 : ℝ) * 0.8 + 0.1 * (1 - 0.8) = 0.5 := by norm_num

/-! ## Reported counts and averages -/

/-- Experiment 1 design (pp.292-293): 70 participants, 40 experimental (20 Blue,
20 Purple, each answering twice) and 30 control (answering once). -/
theorem exp1_design : 40 + 30 = 70 ∧ 20 + 20 = 40 := by decide

/-- **22 of 40 changed `Pr(G|B)`** (p.298, p.305): 11 of 20 Blue (p.294) and 11
of 20 Purple (p.296).  A majority. -/
theorem exp1_changed_GB : 11 + 11 = 22 ∧ 40 < 2 * 22 := by decide

/-- **18 of 40 changed `Pr(G|B̄)`** (p.298, p.305): 8 Blue (p.294) and 10 Purple
(p.296); "almost half". -/
theorem exp1_changed_GNB : 8 + 10 = 18 ∧ 2 * 18 < 40 := by decide

/-- Together 40 of the 80 invariant conditional judgments moved (p.304, "only
40 out of 80 did in Experiment 1"). -/
theorem exp1_changed_total : 22 + 18 = 40 ∧ 2 * 40 = 80 := by decide

/-- The converse `Pr(B|G)` changed for 15 of 20 Blue (p.295) and 17 of 20
Purple (p.296) participants: 32 of 40, against 22 of 40 for `Pr(G|B)`. -/
theorem exp1_changed_converse : 15 + 17 = 32 ∧ 22 < 32 := by decide

/-- The "33.0%" and "73.0%" of p.298 are the means of the Blue and Purple group
means of (6) (23.5%, 42.5%; pp.295-296) and of (7) (82.3%, 63.7%). -/
theorem exp1_means_33_73 :
    ((23.5 : ℚ) + 42.5) / 2 = 33.0 ∧ ((82.3 : ℚ) + 63.7) / 2 = 73.0 := by norm_num

/-- The "27.73%" of p.304 is the mean of Experiment 1's four violation means:
`Pr(G|B)` and `Pr(G|B̄)`, Blue and Purple (23.5, 15.7, 42.5, 29.2), `= 27.725`. -/
theorem exp1_mean_27_73 :
    ((23.5 : ℚ) + 15.7 + 42.5 + 29.2) / 4 = 27.725 ∧
    |((23.5 : ℚ) + 15.7 + 42.5 + 29.2) / 4 - 27.73| ≤ 0.005 := by norm_num [abs_le]

/-- Outliers (p.297): 2 above 100% violation, 3 above 100% converse movement;
Fig. 1's N = 38 and N = 37, and `t(34)` on the 35 remaining; the averages
0.18 < 0.45. -/
theorem exp1_outliers : 40 - 2 = 38 ∧ 40 - 3 = 37 ∧ 40 - (2 + 3) - 1 = 34 ∧
    (0.18 : ℚ) < 0.45 := by norm_num

/-- Experiment 1's separate conditioning check (p.298): Blue `Pr₂(G)` 0.59 vs
`Pr₁(G|B)` 0.56; Purple 0.27 vs 0.24 — both within 0.03. -/
theorem exp1_conditioning_check :
    |(0.59 : ℚ) - 0.56| = 0.03 ∧ |(0.27 : ℚ) - 0.24| = 0.03 := by norm_num [abs_of_nonneg]

/-- Experiment 2 design (p.299): 100 participants, 50 experimental, 50 control. -/
theorem exp2_design : 50 + 50 = 100 := by decide

/-- Lottery (p.302): mean violations 1.42% and 1.33%, both "less than 2%"
(p.305); only 12 and 15 of 50 changed. -/
theorem exp2_lottery : (1.42 : ℚ) < 2 ∧ (1.33 : ℚ) < 2 ∧ 2 * 12 < 50 ∧ 2 * 15 < 50 := by
  norm_num

/-- Ultimatum (p.303): 33 of 50 changed `Pr(A|O)` (19 up, 14 down), 38 of 50
changed `Pr(A|Ō)` (14 up, 24 down): 71 of 100 (p.304). -/
theorem exp2_ultimatum_counts : 19 + 14 = 33 ∧ 14 + 24 = 38 ∧ 33 + 38 = 71 := by decide

/-- **A printed number that does not reproduce.**  p.304 compares "18.45%
versus 27.73%, averaging over all participants and relevant questions".  The
two ultimatum means reported on p.303 are 18.71% and 17.38% over the same 50
participants; their mean is 18.045%, not 18.45%. -/
theorem exp2_ultimatum_mean_slip :
    ((18.71 : ℚ) + 17.38) / 2 = 18.045 ∧ |((18.71 : ℚ) + 17.38) / 2 - 18.45| > 0.4 := by
  norm_num [abs_of_neg, abs_of_pos]

/-- 18.45 is a weighted mean of 18.71 and 17.38 only with weight ratio
`(18.45 − 17.38)/(18.71 − 18.45) = 107/26 ≈ 4.1` on the first. -/
theorem exp2_ultimatum_weight (w : ℚ) (hw : 0 < w) :
    (w * 18.71 + 17.38) / (w + 1) = 18.45 ↔ w = 107 / 26 := by
  have : w + 1 ≠ 0 := by linarith
  rw [div_eq_iff this]
  constructor <;> intro h <;> linarith

end Literature.ZhaoOsherson
