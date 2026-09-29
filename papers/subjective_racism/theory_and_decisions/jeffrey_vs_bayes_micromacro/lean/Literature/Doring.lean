/-
# Döring (1999), "Why Bayesian Psychology Is Incomplete"

*Philosophy of Science* 66 (Proceedings), S379-S389.

Formalization of the paper's own mathematical content, not of Paper B's claims.
The paper is short and mostly argumentative; its mathematics is Jeffrey's rule
(S381), its rigidity and reversibility (S380-S382), one worked 2×2 order-effect
example (S382-S383, Figure 1), the one-step remedy (S384-S385, Figure 2), the
Dempster-rule variant (S385-S386), Field's odds-factor reparametrisation and its
commutativity (S386), and a dampened variant of the example (S387-S388,
Figure 3).  All of these are formalized below.

## Setting

A belief is a function `p : Ω → ℝ` on a finite set of worlds.  A partition is a
labelling `u : Ω → ι`; the cell of `x` is `{y | u y = u x}`.  Jeffrey's rule
(S381, displayed) is

  `p_new(x) = Σ_{e ∈ E} p_new(e) · p_old(x | e)`,

which on atoms reads `jeffrey p u q x = q (u x) * p x / p(cell of x)`.

The worked example lives on `Ω = Bool × Bool`, the first coordinate the truth
value of `A`, the second that of `B`.  The prior (S382) is
`p(AB) = p(¬AB) = .05`, `p(A¬B) = p(¬A¬B) = .45`.  The two cues are the
*two-cell* partitions `{A ∨ B, ¬A¬B}` and `{¬A ∨ B, A¬B}` (S382): each singles
out one joint cell; neither is a partition by the value of `A` or of `B`.

## Findings recorded by the formalization

* Every table entry in Figures 1-3 is reproduced **exactly** (as rationals); the
  printed figures are rounded percentages, as Döring says (S383).
* The conditional `P(A | ¬B)` is exactly `19/118 ≈ 0.161` after sequence 1 and
  `99/118 ≈ 0.839` after sequence 2, which the paper reports as `1/6` and `5/6`
  (S383).  The ratio `P(A¬B)/P(¬A¬B)` is exactly `19/99`, reported as "one
  fifth".  Both are fair roundings, stated as such (`condA_notB_seq1_approx`).
* The marginal of `A` already differs between the orders (`91/190` vs
  `99/190`), before any third step.
* With the paper's own numbers, a third step lowering `P(B)` to `.01` moves
  `P(A)` to `97/590 ≈ 0.164` vs `493/590 ≈ 0.836`, i.e. near `1/6` and `5/6`,
  not "near 0 and 1" (S383).  "Near 0 and 1" needs the "playing with the
  numbers" limit, which is `gap_tends_to_one` below.
* The Dempster-rule variant (S385-S386) gives `99/202` and `1/101`, which differ
  from Figure 2's `.49` and `.01` by exactly `1/10100`, about **1/100** of a
  percentage point, not "within 1/1000 of a percentage point" as S386 says
  (`dempster_gap`).  The qualitative point (practically the same result)
  stands.
* Figure 3, second row, prints `42.2` for `P(¬A¬B)`; the exact value is
  `391/925 = 42.27…%`, which rounds to `42.3` (`fig3_row2`; the other eleven
  printed Figure 3 entries round correctly).  A rounding slip only.

## Not formalized

The paper's philosophical theses (incompleteness of Jeffrey conditionalization
as an account of *rational* belief change, S379/S386; the criticism of Field's
"uninformativeness" argument, S386-S387; the call for non-incremental schemes,
S388-S389).  Skyrms's embedding of Jeffrey updating into classical
conditionalization (S384) is cited, not proved, by Döring; only the displayed
commutativity of classical conditioning is formalized (`cond_comm`).  Dempster's
rule is formalized only as the displayed arithmetic of the orthogonal sum
(S385), not as a general rule on belief functions.
-/
import Mathlib

namespace Literature.Doring

open Finset

section General

variable {Ω ι κ : Type*} [Fintype Ω] [DecidableEq ι] [DecidableEq κ]

/-- Mass of cell `i` of the partition `u`: `p(u⁻¹ i)`. -/
def cellMass (p : Ω → ℝ) (u : Ω → ι) (i : ι) : ℝ :=
  ∑ y, if u y = i then p y else 0

/-- **Jeffrey's rule** (S381): `p_new(x) = Σ_e p_new(e) p_old(x | e)`, written on
atoms.  `q i` is the new probability of cell `i`. -/
noncomputable def jeffrey (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ) : Ω → ℝ :=
  fun x => q (u x) * p x / cellMass p u (u x)

/-- The new cell probabilities are the assigned ones (S381: experience "causes
the agent to assign new probabilities `p_new` to the cells"). -/
theorem cellMass_jeffrey (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ) (i : ι)
    (h : cellMass p u i ≠ 0) : cellMass (jeffrey p u q) u i = q i := by
  have key : cellMass (jeffrey p u q) u i = q i / cellMass p u i * cellMass p u i := by
    conv_rhs => rw [cellMass, Finset.mul_sum]
    unfold cellMass jeffrey
    refine Finset.sum_congr rfl fun y _ => ?_
    by_cases hy : u y = i
    · simp only [hy, if_true]; unfold cellMass; ring
    · simp [hy]
  rw [key]; field_simp

/-- **Rigidity** (S381-S382): "all probability ratios within each region —
`p(x|e)/p(y|e)` — be left intact".  Cross-multiplied form. -/
theorem jeffrey_rigid (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ) {x y : Ω}
    (hxy : u x = u y) : jeffrey p u q x * p y = jeffrey p u q y * p x := by
  unfold jeffrey; rw [hxy]; ring

/-- A cell set to probability 1 reproduces classical conditioning (S381: "If
one cell in the partition receives posterior probability 1, the prescriptions
of Jeffrey's and of the classical rule coincide"). -/
theorem jeffrey_certain (p : Ω → ℝ) (u : Ω → ι) (i : ι) (x : Ω) :
    jeffrey p u (fun j => if j = i then 1 else 0) x =
      if u x = i then p x / cellMass p u i else 0 := by
  unfold jeffrey
  by_cases h : u x = i
  · simp [h]
  · simp [h]

/-- **Reversibility** (S380, S382): "For as long as no contingent proposition
receives probability extremes 0 or 1, the effect of any update by Jeffrey's
rule can be undone by subsequent applications of the same rule" ("no balloons
are popped").  Updating back to the old cell masses restores the old belief. -/
theorem jeffrey_reversible (p : Ω → ℝ) (u : Ω → ι) (q : ι → ℝ)
    (hm : ∀ x, cellMass p u (u x) ≠ 0) (hq : ∀ x, q (u x) ≠ 0) :
    jeffrey (jeffrey p u q) u (cellMass p u) = p := by
  funext x
  have h1 := cellMass_jeffrey p u q (u x) (hm x)
  unfold jeffrey at *
  rw [h1]
  have := hm x; have := hq x
  field_simp

/-- Two applications on the *same* partition: the later assignment wins (the
reason reversibility holds, and the simplest order effect). -/
theorem jeffrey_same_partition (p : Ω → ℝ) (u : Ω → ι) (q r : ι → ℝ)
    (hm : ∀ x, cellMass p u (u x) ≠ 0) (hq : ∀ x, q (u x) ≠ 0) :
    jeffrey (jeffrey p u q) u r = jeffrey p u r := by
  funext x
  have h1 := cellMass_jeffrey p u q (u x) (hm x)
  unfold jeffrey at *
  rw [h1]
  have := hm x; have := hq x
  field_simp

/-- Classical conditioning on a set. -/
noncomputable def cond (p : Ω → ℝ) (e : Ω → Prop) [DecidablePred e] : Ω → ℝ :=
  fun x => if e x then p x / ∑ y, (if e y then p y else 0) else 0

/-- **Classical conditioning is order-independent** (S384, displayed:
`p_{yz}(x) = p(x|yz) = p_{zy}(x)`). -/
theorem cond_comm (p : Ω → ℝ) (hp : ∀ x, 0 ≤ p x) (e f : Ω → Prop)
    [DecidablePred e] [DecidablePred f] :
    cond (cond p e) f = cond (cond p f) e := by
  funext x
  unfold cond
  have hs : (∑ y, if f y then (if e y then p y / ∑ z, (if e z then p z else 0) else 0) else 0)
      = (∑ y, if e y ∧ f y then p y else 0) / ∑ z, (if e z then p z else 0) := by
    rw [Finset.sum_div]
    refine Finset.sum_congr rfl fun y _ => ?_
    by_cases h1 : e y <;> by_cases h2 : f y <;> simp [h1, h2]
  have ht : (∑ y, if e y then (if f y then p y / ∑ z, (if f z then p z else 0) else 0) else 0)
      = (∑ y, if e y ∧ f y then p y else 0) / ∑ z, (if f z then p z else 0) := by
    rw [Finset.sum_div]
    refine Finset.sum_congr rfl fun y _ => ?_
    by_cases h1 : e y <;> by_cases h2 : f y <;> simp [h1, h2]
  rw [hs, ht]
  have hS0 : 0 ≤ ∑ y, (if e y ∧ f y then p y else 0) :=
    Finset.sum_nonneg fun y _ => by split_ifs <;> simp [hp y]
  have hSe : (∑ y, if e y ∧ f y then p y else 0) ≤ ∑ z, (if e z then p z else 0) :=
    Finset.sum_le_sum fun y _ => by
      by_cases h1 : e y <;> by_cases h2 : f y <;> simp [h1, h2, hp y]
  have hSf : (∑ y, if e y ∧ f y then p y else 0) ≤ ∑ z, (if f z then p z else 0) :=
    Finset.sum_le_sum fun y _ => by
      by_cases h1 : e y <;> by_cases h2 : f y <;> simp [h1, h2, hp y]
  set S := ∑ y, if e y ∧ f y then p y else 0
  set Se := ∑ z, if e z then p z else 0
  set Sf := ∑ z, if f z then p z else 0
  by_cases h1 : e x <;> by_cases h2 : f x <;> simp only [h1, h2, if_true, if_false, zero_div]
  rcases hS0.lt_or_eq with hS | hS
  · have : Se ≠ 0 := by linarith
    have : Sf ≠ 0 := by linarith
    field_simp
  · rw [← hS]; simp

/-- **Field's reparametrisation** (S386): the input is a change factor `w i` for
the odds of each cell; the new belief is proportional to `w (u x) * p x`.  For a
two-cell partition this is exactly "changing the odds for `e` by a factor"
(`field_odds`). -/
noncomputable def fieldUpdate (p : Ω → ℝ) (u : Ω → ι) (w : ι → ℝ) : Ω → ℝ :=
  fun x => w (u x) * p x / ∑ y, w (u y) * p y

omit [DecidableEq ι] [DecidableEq κ] in
/-- **Field's rule is commutative** (S386: "Changing first the odds for `e₁` by a
factor `r₁` and then the odds for `e₂` by a factor `r₂` results in the same final
probabilities as proceeding in reverse order"), for any two partitions. -/
theorem field_comm (p : Ω → ℝ) (u : Ω → ι) (v : Ω → κ) (w : ι → ℝ) (z : κ → ℝ)
    (hw : ∑ y, w (u y) * p y ≠ 0) (hz : ∑ y, z (v y) * p y ≠ 0) :
    fieldUpdate (fieldUpdate p u w) v z = fieldUpdate (fieldUpdate p v z) u w := by
  funext x
  unfold fieldUpdate
  have h1 : ∑ y, z (v y) * (w (u y) * p y / ∑ y', w (u y') * p y') =
      (∑ y, w (u y) * z (v y) * p y) / ∑ y', w (u y') * p y' := by
    rw [Finset.sum_div]; exact Finset.sum_congr rfl fun y _ => by ring
  have h2 : ∑ y, w (u y) * (z (v y) * p y / ∑ y', z (v y') * p y') =
      (∑ y, w (u y) * z (v y) * p y) / ∑ y', z (v y') * p y' := by
    rw [Finset.sum_div]; exact Finset.sum_congr rfl fun y _ => by ring
  rw [h1, h2]
  by_cases hT : ∑ y, w (u y) * z (v y) * p y = 0
  · rw [hT]; simp
  · field_simp

/-- Cell masses under Field's rule. -/
theorem cellMass_fieldUpdate (p : Ω → ℝ) (u : Ω → ι) (w : ι → ℝ) (i : ι) :
    cellMass (fieldUpdate p u w) u i = w i * cellMass p u i / ∑ y, w (u y) * p y := by
  unfold cellMass fieldUpdate
  rw [Finset.mul_sum, Finset.sum_div]
  refine Finset.sum_congr rfl fun y _ => ?_
  by_cases hy : u y = i
  · simp [hy]
  · simp [hy]

/-- Field's factor is a **Bayes factor** (S386: "`α = o_new(e)/o_old(e)`"): on a
two-cell partition with factor `α` on `e` and `1` on `¬e`, the new odds are `α`
times the old. -/
theorem field_odds (p : Ω → ℝ) (u : Ω → Bool) (α : ℝ)
    (hZ : ∑ y, (fun b => if b then α else 1) (u y) * p y ≠ 0) :
    cellMass (fieldUpdate p u (fun b => if b then α else 1)) u true /
        cellMass (fieldUpdate p u (fun b => if b then α else 1)) u false =
      α * (cellMass p u true / cellMass p u false) := by
  rw [cellMass_fieldUpdate, cellMass_fieldUpdate]
  simp only [if_true, if_false, Bool.false_eq_true]
  by_cases h0 : cellMass p u false = 0
  · simp [h0]
  · field_simp

end General

/-! ## The worked example (S382-S383, Figure 1) -/

section Example

/-- A 2×2 table on `Bool × Bool`; `(a, b)` are the truth values of `A` and `B`.
Arguments in the order `AB, ¬AB, A¬B, ¬A¬B`. -/
def tbl (ab nab anb nanb : ℝ) : Bool × Bool → ℝ
  | (true, true) => ab
  | (false, true) => nab
  | (true, false) => anb
  | (false, false) => nanb

/-- The prior of S382: `p(AB) = p(¬AB) = .05`, `p(A¬B) = p(¬A¬B) = .45`. -/
noncomputable def prior : Bool × Bool → ℝ := tbl (1/20) (1/20) (9/20) (9/20)

/-- The first cue's partition `{A ∨ B, ¬A¬B}` (label `true` = the cell `¬A¬B`). -/
def cueOr : Bool × Bool → Bool := fun ω => decide (ω = (false, false))

/-- The second cue's partition `{¬A ∨ B, A¬B}` (label `true` = the cell `A¬B`). -/
def cueNotAOr : Bool × Bool → Bool := fun ω => decide (ω = (true, false))

/-- The input "raise the disjunction to `1 - ε`": the singled-out cell gets `ε`. -/
noncomputable def raise (ε : ℝ) : Bool → ℝ := fun b => if b then ε else 1 - ε

/-- `P(A | ¬B)`. -/
noncomputable def condA_notB (t : Bool × Bool → ℝ) : ℝ :=
  t (true, false) / (t (true, false) + t (false, false))

/-- `P(A)`. -/
def margA (t : Bool × Bool → ℝ) : ℝ := t (true, true) + t (true, false)

/-- Sequence 1: first `A ∨ B` to `.99`, then `¬A ∨ B` to `.99`. -/
noncomputable def seq1 (p : Bool × Bool → ℝ) (ε : ℝ) : Bool × Bool → ℝ :=
  jeffrey (jeffrey p cueOr (raise ε)) cueNotAOr (raise ε)

/-- Sequence 2: the same cues in reverse order. -/
noncomputable def seq2 (p : Bool × Bool → ℝ) (ε : ℝ) : Bool × Bool → ℝ :=
  jeffrey (jeffrey p cueNotAOr (raise ε)) cueOr (raise ε)

/-- S382, displayed: raising `A ∨ B` to `.99` gives `p'(AB) = 0.99 · 0.05/0.55 = 0.09`;
the whole middle table of Figure 1, row 1 (exact). -/
theorem fig1_step1 : jeffrey prior cueOr (raise (1/100)) = tbl (9/100) (9/100) (81/100) (1/100) := by
  funext ⟨a, b⟩
  cases a <;> cases b <;>
    simp [jeffrey, cellMass, Fintype.sum_prod_type, prior, tbl, cueOr, raise] <;> norm_num

/-- Figure 1, row 2, middle table (exact). -/
theorem fig1_step1' : jeffrey prior cueNotAOr (raise (1/100)) = tbl (9/100) (9/100) (1/100) (81/100) := by
  funext ⟨a, b⟩
  cases a <;> cases b <;>
    simp [jeffrey, cellMass, Fintype.sum_prod_type, prior, tbl, cueNotAOr, raise] <;> norm_num

/-- Figure 1, row 1, right table, exactly: `(891/1900, 891/1900, 1/100, 99/1900)`,
printed as `47, 47, 1, 5` percent. -/
theorem fig1_seq1 : seq1 prior (1/100) = tbl (891/1900) (891/1900) (1/100) (99/1900) := by
  unfold seq1; rw [fig1_step1]
  funext ⟨a, b⟩
  cases a <;> cases b <;>
    simp [jeffrey, cellMass, Fintype.sum_prod_type, tbl, cueNotAOr, raise] <;> norm_num

/-- Figure 1, row 2, right table, exactly: the mirror image. -/
theorem fig1_seq2 : seq2 prior (1/100) = tbl (891/1900) (891/1900) (99/1900) (1/100) := by
  unfold seq2; rw [fig1_step1']
  funext ⟨a, b⟩
  cases a <;> cases b <;>
    simp [jeffrey, cellMass, Fintype.sum_prod_type, tbl, cueOr, raise] <;> norm_num

/-- **The order effect** (S383): `P(A | ¬B)` is `19/118` after sequence 1 and
`99/118` after sequence 2. -/
theorem order_effect_condA_notB :
    condA_notB (seq1 prior (1/100)) = 19/118 ∧ condA_notB (seq2 prior (1/100)) = 99/118 := by
  rw [fig1_seq1, fig1_seq2]
  constructor <;> simp [condA_notB, tbl] <;> norm_num

/-- The paper's "1/6 after sequence 1 and 5/6 after sequence 2" (S383) are
roundings of the exact `19/118` and `99/118` (each within `1/100`). -/
theorem condA_notB_seq1_approx :
    |(19/118 : ℝ) - 1/6| < 1/100 ∧ |(99/118 : ℝ) - 5/6| < 1/100 := by
  constructor <;> rw [abs_lt] <;> constructor <;> norm_num

/-- "The probability of `A¬B` is one fifth of the probability of `¬A¬B`" (S383):
exactly `19/99`. -/
theorem ratio_seq1 : (seq1 prior (1/100)) (true, false) / (seq1 prior (1/100)) (false, false) = 19/99 := by
  rw [fig1_seq1]; simp [tbl]; norm_num

/-- The marginal of `A` differs between the two orders already (`91/190` vs
`99/190`, about 48% vs 52%). -/
theorem order_effect_margA :
    margA (seq1 prior (1/100)) = 91/190 ∧ margA (seq2 prior (1/100)) = 99/190 := by
  rw [fig1_seq1, fig1_seq2]
  constructor <;> simp [margA, tbl] <;> norm_num

/-- The two sequences end in different beliefs: Jeffrey's rule is not
commutative (S382). -/
theorem jeffrey_not_comm : seq1 prior (1/100) ≠ seq2 prior (1/100) := by
  rw [fig1_seq1, fig1_seq2]
  intro h
  have := congrFun h (true, false)
  simp [tbl] at this
  norm_num at this

/-- The third step of S383 with the paper's own numbers: lowering `P(B)` to
`.01` (Jeffrey on the partition by `B`) sends `P(A)` to `97/590 ≈ .164` after
sequence 1 and `493/590 ≈ .836` after sequence 2: the roles of `A` and `¬A` are
reversed, but the values sit near `1/6` and `5/6`, not near 0 and 1. -/
theorem third_step :
    margA (jeffrey (seq1 prior (1/100)) Prod.snd (raise (1/100))) = 97/590 ∧
    margA (jeffrey (seq2 prior (1/100)) Prod.snd (raise (1/100))) = 493/590 := by
  rw [fig1_seq1, fig1_seq2]
  constructor <;>
    simp [margA, jeffrey, cellMass, Fintype.sum_prod_type, tbl, raise] <;> norm_num

/-! ### "By playing with the numbers" (S383) -/

/-- A one-parameter family of priors of Döring's shape:
`p(A¬B) = p(¬A¬B) = n/(2n+1)`, `p(AB) = p(¬AB) = 1/(2(2n+1))`, with the cues
pushing the singled-out cell to `ε = 1/n`.  The paper's prior is `fam (9/2)`
(`fam_nine_halves`); tying the cue strength to the prior (`ε = 1/n`) only keeps
the closed form short. -/
noncomputable def fam (n : ℝ) : Bool × Bool → ℝ :=
  tbl (1 / (2 * (2 * n + 1))) (1 / (2 * (2 * n + 1))) (n / (2 * n + 1)) (n / (2 * n + 1))

/-- The paper's prior belongs to the family. -/
theorem fam_nine_halves : fam (9/2) = prior := by
  funext ⟨a, b⟩; cases a <;> cases b <;> simp [fam, prior, tbl] <;> norm_num

/-- Jeffrey on the first cue, for an arbitrary table. -/
theorem jeffrey_cueOr (a b c d ε : ℝ) (hd : d ≠ 0) :
    jeffrey (tbl a b c d) cueOr (raise ε) =
      tbl ((1 - ε) * a / (a + b + c)) ((1 - ε) * b / (a + b + c)) ((1 - ε) * c / (a + b + c)) ε := by
  funext ⟨x, y⟩
  cases x <;> cases y <;> simp [jeffrey, cellMass, Fintype.sum_prod_type, tbl, cueOr, raise, hd] <;>
    ring

/-- Jeffrey on the second cue, for an arbitrary table. -/
theorem jeffrey_cueNotAOr (a b c d ε : ℝ) (hc : c ≠ 0) :
    jeffrey (tbl a b c d) cueNotAOr (raise ε) =
      tbl ((1 - ε) * a / (a + b + d)) ((1 - ε) * b / (a + b + d)) ε ((1 - ε) * d / (a + b + d)) := by
  funext ⟨x, y⟩
  cases x <;> cases y <;> simp [jeffrey, cellMass, Fintype.sum_prod_type, tbl, cueNotAOr, raise, hc] <;>
    ring

theorem fam_seq1 (n : ℝ) (hn : 2 ≤ n) :
    seq1 (fam n) (1 / n) =
      tbl ((n - 1) ^ 2 / (4 * n ^ 2)) ((n - 1) ^ 2 / (4 * n ^ 2)) (1 / n) ((n ^ 2 - 1) / (2 * n ^ 2)) := by
  have h0 : n ≠ 0 := by positivity
  have h1 : n + 1 ≠ 0 := by positivity
  have h2 : 2 * n + 1 ≠ 0 := by positivity
  have h5 : 1 - 1 / n ≠ 0 := by
    have : 1 / n < 1 := (div_lt_one (by positivity)).mpr (by linarith)
    linarith
  unfold seq1 fam
  rw [jeffrey_cueOr _ _ _ _ _ (by positivity)]
  have hs : 1 / (2 * (2 * n + 1)) + 1 / (2 * (2 * n + 1)) + n / (2 * n + 1) = (n + 1) / (2 * n + 1) := by
    field_simp; ring
  rw [hs, jeffrey_cueNotAOr _ _ _ _ _ (by
    rw [div_ne_zero_iff]; exact ⟨mul_ne_zero h5 (by positivity), by positivity⟩)]
  have ht : (1 - 1 / n) * (1 / (2 * (2 * n + 1))) / ((n + 1) / (2 * n + 1)) +
      (1 - 1 / n) * (1 / (2 * (2 * n + 1))) / ((n + 1) / (2 * n + 1)) + 1 / n = 2 / (n + 1) := by
    field_simp; ring
  rw [ht]
  congr 1 <;> field_simp <;> ring

theorem fam_seq2 (n : ℝ) (hn : 2 ≤ n) :
    seq2 (fam n) (1 / n) =
      tbl ((n - 1) ^ 2 / (4 * n ^ 2)) ((n - 1) ^ 2 / (4 * n ^ 2)) ((n ^ 2 - 1) / (2 * n ^ 2)) (1 / n) := by
  have h0 : n ≠ 0 := by positivity
  have h1 : n + 1 ≠ 0 := by positivity
  have h2 : 2 * n + 1 ≠ 0 := by positivity
  have h5 : 1 - 1 / n ≠ 0 := by
    have : 1 / n < 1 := (div_lt_one (by positivity)).mpr (by linarith)
    linarith
  unfold seq2 fam
  rw [jeffrey_cueNotAOr _ _ _ _ _ (by positivity)]
  have hs : 1 / (2 * (2 * n + 1)) + 1 / (2 * (2 * n + 1)) + n / (2 * n + 1) = (n + 1) / (2 * n + 1) := by
    field_simp; ring
  rw [hs, jeffrey_cueOr _ _ _ _ _ (by
    rw [div_ne_zero_iff]; exact ⟨mul_ne_zero h5 (by positivity), by positivity⟩)]
  have ht : (1 - 1 / n) * (1 / (2 * (2 * n + 1))) / ((n + 1) / (2 * n + 1)) +
      (1 - 1 / n) * (1 / (2 * (2 * n + 1))) / ((n + 1) / (2 * n + 1)) + 1 / n = 2 / (n + 1) := by
    field_simp; ring
  rw [ht]
  congr 1 <;> field_simp <;> ring

/-- The discrepancy in `P(A | ¬B)` in the family, in closed form:
`1 - 4n/(n² + 2n - 1)`. -/
theorem fam_gap (n : ℝ) (hn : 2 ≤ n) :
    condA_notB (seq2 (fam n) (1 / n)) - condA_notB (seq1 (fam n) (1 / n)) =
      1 - 4 * n / (n ^ 2 + 2 * n - 1) := by
  rw [fam_seq1 n hn, fam_seq2 n hn]
  have h0 : n ≠ 0 := by positivity
  have h4 : n ^ 2 + 2 * n - 1 ≠ 0 := by nlinarith
  simp only [condA_notB, tbl]
  have e1 : (n ^ 2 - 1) / (2 * n ^ 2) + 1 / n = (n ^ 2 + 2 * n - 1) / (2 * n ^ 2) := by
    field_simp; ring
  have e2 : 1 / n + (n ^ 2 - 1) / (2 * n ^ 2) = (n ^ 2 + 2 * n - 1) / (2 * n ^ 2) := by
    field_simp; ring
  rw [e1, e2]
  field_simp
  have h4' : n * (n + 2) - 1 ≠ 0 := by nlinarith
  rw [eq_sub_iff_add_eq, ← add_div, div_eq_one_iff_eq h4']
  ring

/-- **"By playing with the numbers, this discrepancy can be brought as close to 1
as you please"** (S383): for every `δ > 0` some prior of Döring's shape and some
cue strength make `P(A|¬B)` differ between the two orders by more than `1 - δ`. -/
theorem gap_tends_to_one (δ : ℝ) (hδ : 0 < δ) :
    ∃ n : ℝ, 2 ≤ n ∧
      1 - δ < condA_notB (seq2 (fam n) (1 / n)) - condA_notB (seq1 (fam n) (1 / n)) := by
  refine ⟨max 2 (8 / δ), le_max_left _ _, ?_⟩
  set n := max 2 (8 / δ) with hn
  have h2 : 2 ≤ n := le_max_left _ _
  have h8 : 8 / δ ≤ n := le_max_right _ _
  rw [fam_gap n h2]
  have hD : 0 < n ^ 2 + 2 * n - 1 := by nlinarith
  have hnδ : 8 ≤ n * δ := by
    have := (div_le_iff₀ hδ).mp h8; linarith
  have : 4 * n / (n ^ 2 + 2 * n - 1) < δ := by
    rw [div_lt_iff₀ hD]; nlinarith
  linarith

/-! ## The remedy (S384-S385, Figure 2) and Dempster's rule (S385-S386) -/

/-- The merged evidence partition `{A¬B, ¬A¬B, B}` of S385. -/
def merged : Bool × Bool → Fin 3 := fun ω => if ω.2 then 0 else if ω.1 then 1 else 2

/-- **Figure 2** (S385): "Jeffrey conditionalizing in one step on the new
assignments" `p(A¬B) = p(¬A¬B) = .01`, `p(B) = .98` yields `49, 49, 1, 1`
exactly.  The remedy is itself one application of Jeffrey's rule to the
original prior. -/
theorem fig2 : jeffrey prior merged ![98/100, 1/100, 1/100] =
    tbl (49/100) (49/100) (1/100) (1/100) := by
  funext ⟨a, b⟩
  cases a <;> cases b <;>
    simp [jeffrey, cellMass, Fintype.sum_prod_type, prior, tbl, merged] <;> norm_num

/-- The orthogonal sum of S385: `m(¬A¬B) = m(A¬B) = 0.01·0.99/(1-0.01²)` and
`m(B) = 0.99²/(1-0.01²)`, exactly `1/101`, `1/101`, `99/101`. -/
theorem dempster_masses :
    (1/100 * (99/100)) / (1 - (1/100 : ℝ) ^ 2) = 1/101 ∧
    (99/100 : ℝ) ^ 2 / (1 - (1/100 : ℝ) ^ 2) = 99/101 := by
  constructor <;> norm_num

/-- Jeffrey on the Dempster masses (S386). -/
theorem dempster_result : jeffrey prior merged ![99/101, 1/101, 1/101] =
    tbl (99/202) (99/202) (1/101) (1/101) := by
  funext ⟨a, b⟩
  cases a <;> cases b <;>
    simp [jeffrey, cellMass, Fintype.sum_prod_type, prior, tbl, merged] <;> norm_num

/-- S386 says the Dempster route "yields the result in the second table in
Figure 2 to within 1/1000 of a percentage point".  Exactly, every cell differs
by `1/10100`, which is about `1/100` of a percentage point (`10⁻⁴`), and more
than `1/1000` of one (`10⁻⁵`). -/
theorem dempster_gap :
    (99/202 : ℝ) - 49/100 = 1/10100 ∧ (1/100 : ℝ) - 1/101 = 1/10100 ∧
    (1/100000 : ℝ) < 1/10100 ∧ (1/10100 : ℝ) < 1/10000 := by
  refine ⟨?_, ?_, ?_, ?_⟩ <;> norm_num

/-! ## The dampened variant (S387-S388, Figure 3) -/

/-- Evidence posteriors "as weighted averages of the priors and the
probabilities from experience" (S387): the singled-out cell gets
`w · (its current probability) + (1 - w) · ε`. -/
noncomputable def damp (w ε c : ℝ) : Bool → ℝ := raise (w * c + (1 - w) * ε)

/-- Figure 3, first row (prior weight `0.1`), exactly:
`8.6, 8.6, 77.4, 5.4` then `34.765…, 34.765…, 8.64, 21.829…`. -/
theorem fig3_row1 :
    jeffrey prior cueOr (damp (1/10) (1/100) (9/20)) = tbl (43/500) (43/500) (387/500) (27/500) ∧
    jeffrey (tbl (43/500) (43/500) (387/500) (27/500)) cueNotAOr (damp (1/10) (1/100) (387/500)) =
      tbl (24553/70625) (24553/70625) (54/625) (15417/70625) := by
  constructor <;> funext ⟨a, b⟩ <;> cases a <;> cases b <;>
    simp [jeffrey, cellMass, Fintype.sum_prod_type, prior, tbl, cueOr, cueNotAOr, damp, raise] <;>
    norm_num

/-- Figure 3, second row (weights `0.5`), exactly:
`7, 7, 63, 23` then `12.86…, 12.86…, 32, 42.27…`. -/
theorem fig3_row2 :
    jeffrey prior cueOr (damp (1/2) (1/100) (9/20)) = tbl (7/100) (7/100) (63/100) (23/100) ∧
    jeffrey (tbl (7/100) (7/100) (63/100) (23/100)) cueNotAOr (damp (1/2) (1/100) (63/100)) =
      tbl (119/925) (119/925) (8/25) (391/925) := by
  constructor <;> funext ⟨a, b⟩ <;> cases a <;> cases b <;>
    simp [jeffrey, cellMass, Fintype.sum_prod_type, prior, tbl, cueOr, cueNotAOr, damp, raise] <;>
    norm_num

end Example

end Literature.Doring
