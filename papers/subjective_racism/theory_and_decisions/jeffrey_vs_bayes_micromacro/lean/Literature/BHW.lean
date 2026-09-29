/-
# Bikhchandani, Hirshleifer & Welch (1992), "A Theory of Fads, Fashion, Custom, and Cultural Change as Informational Cascades"

*Journal of Political Economy* 100(5), 992-1026.

Formalization of the paper's specific model (Section IIA, pp.996-999) and of the
algebra behind its closed-form cascade probabilities, not of Paper B's claims.

## The paper's setup (Sec. IIA, p.996)

Individuals decide in an exogenous, commonly known order whether to adopt or
reject.  Each observes the actions (not the signals) of all predecessors.  The
gain `V ∈ {0,1}` has prior `1/2`; the cost is `C = 1/2`, so an individual adopts
iff the posterior `Pr(V = 1)` exceeds `1/2`.  Signals are conditionally i.i.d.,
`Pr(X = H | V = 1) = Pr(X = L | V = 0) = p > 1/2` (Table 1).  Tie-break: "an
individual indifferent between adoption and rejection adopts or rejects with
equal probability" (p.996).

## What is formalized

Histories are lists of actions (`true` = adopt), most recent first.

* `adoptProb d s` — the **counting rule**: with `d = #adopt - #reject`, follow the
  signal at `d = 0`; at `d = 1` adopt on `H` and flip a coin on `L`; at `d = -1`
  flip on `H` and reject on `L`; and once `|d| ≥ 2` ignore the signal.
* `lik p v h` — the probability of history `h` given `V = v` when every agent
  uses the counting rule; each factor is `actProb`, the signal-averaged action
  probability.
* `lik_ratio` — **the public likelihood ratio is a function of `d` alone**, with
  values `1`, `p/(1-p)`, and `p(1+p)/((1-p)(2-p))` at `d = 0, 1, ≥ 2` (and the
  reciprocals at `-1, ≤ -2`).  Footnote 23 (p.1009): the difference between
  adoptions and rejections "substitutes perfectly for knowledge of the entire
  history".
* `counting_rule_is_bayes` — **the counting rule is exactly the Bayes rule with
  the paper's tie-break**: for every history of positive probability, the
  counting rule's adoption probability is `1`, `0` or `1/2` according as the
  posterior is above, below or equal to `1/2`.  This is the p.996-997 narrative
  (and the Result 3 proof, pp.1005-1006: "An UP (DOWN) cascade ensues as soon as
  an individual observes two more adopt (reject) than reject (adopt) decisions")
  proved for every history.
* `cascade_ignores_signal`, `cascade_uninformative`, `cascade_forever` — the
  DEFINITION (p.1000: "An informational cascade occurs if an individual's action
  does not depend on his private information signal") holds once `|d| ≥ 2`; the
  cascade action multiplies both likelihoods by `1`, so it "conveys no
  information" (p.1000) and the posterior is unchanged; hence the cascade lasts
  forever (p.1000).
* `informative_outside_cascade` — before a cascade the action's likelihood differs
  across the two states, so it does convey information.
* `eq3` — **eqs. (2)-(3)** (p.998): after `n = 2m` individuals, given `V = 1`
  (and, mirrored, `V = 0`), the probabilities of an UP cascade, no cascade and a
  DOWN cascade.  `eq1` — **eq. (1)** (p.997), unconditional.  These are computed
  on `dist`, the distribution over the five regimes of `d` (UP, `1`, `0`, `-1`,
  DOWN) whose one-step transition probabilities are the `actProb` of the
  counting rule; the lift from `dist` to sums of `lik` over histories is not
  formalized (it is checked by enumeration in `sympy/check_bhw.py`).
* `no_cascade_after_ten` — p.997-998: the probability of no cascade after 10
  individuals is below 0.1 percent for every `p`.
* `tendsto_correct_cascade` — the `n → ∞` limit `p(p+1)/(2(1-p+p²))` (Fig. 1).
* `A6_posterior_up` — **(A6)** (p.1020): `Pr(V=1 | UP cascade started in period
  2k) = p(p+1)/(2(p²-p+1)) > 1/2`, derived from the chain's increments.
* `A10_closed_form`, `A10_value`, `A11_simplify`, `A11_value` — the algebra of
  Result 4's proof (pp.1022-1023): `E[W | UP, L] ≈ .488` and the reversal bound
  `.0935` at `p = .9`.

## Not formalized

Proposition 1 (cascades start a.s. under MLRP; a strong-law argument), the
general-model inference sets `J_i`, Result 1 (fashion leaders), Result 2 (checked
numerically in `sympy/check_bhw.py`), Proposition 2, Result 3, Proposition 3,
the conditional-on-cascade structure in Result 4 beyond its displayed algebra,
and Table 2 (see the README: its "no public information" columns do not equal the
eq. (3) limit exactly).
-/
import Mathlib

namespace Literature.BHW

/-! ## The specific model -/

/-- `Pr(X = s | V = v)`, with `s = true` the signal `H` (Table 1, p.997). -/
def sigLik (p : ℝ) (v s : Bool) : ℝ := if s = v then p else 1 - p

/-- The counting rule: the probability of adopting when the public difference
`#adopt - #reject` is `d` and the private signal is `s`. -/
noncomputable def adoptProb (d : ℤ) (s : Bool) : ℝ :=
  if 2 ≤ d then 1
  else if d ≤ -2 then 0
  else if d = 0 then (if s then 1 else 0)
  else if d = 1 then (if s then 1 else 1 / 2)
  else (if s then 1 / 2 else 0)

lemma sigLik_tt (p : ℝ) : sigLik p true true = p := by simp [sigLik]
lemma sigLik_tf (p : ℝ) : sigLik p true false = 1 - p := by simp [sigLik]
lemma sigLik_ft (p : ℝ) : sigLik p false true = 1 - p := by simp [sigLik]
lemma sigLik_ff (p : ℝ) : sigLik p false false = p := by simp [sigLik]

/-- Probability of action `a` (`true` = adopt) given `V = v` and public
difference `d`, averaging over the private signal. -/
noncomputable def actProb (p : ℝ) (v : Bool) (d : ℤ) (a : Bool) : ℝ :=
  sigLik p v true * (if a then adoptProb d true else 1 - adoptProb d true) +
  sigLik p v false * (if a then adoptProb d false else 1 - adoptProb d false)

/-- `#adopt - #reject` of a history (most recent action first). -/
def diff : List Bool → ℤ
  | [] => 0
  | a :: h => diff h + (if a then 1 else -1)

/-- `Pr(history | V = v)` when every individual uses the counting rule. -/
noncomputable def lik (p : ℝ) (v : Bool) : List Bool → ℝ
  | [] => 1
  | a :: h => lik p v h * actProb p v (diff h) a

/-- Weight of `V = 1` in the public likelihood ratio at difference `d`. -/
noncomputable def c1 (p : ℝ) (d : ℤ) : ℝ :=
  if 2 ≤ d then p * (1 + p)
  else if d ≤ -2 then (1 - p) * (2 - p)
  else if d = 1 then p
  else if d = -1 then 1 - p
  else 1

/-- Weight of `V = 0` in the public likelihood ratio at difference `d`. -/
noncomputable def c0 (p : ℝ) (d : ℤ) : ℝ :=
  if 2 ≤ d then (1 - p) * (2 - p)
  else if d ≤ -2 then p * (1 + p)
  else if d = 1 then 1 - p
  else if d = -1 then p
  else 1

/-- The posterior `Pr(V = 1 | history h, signal s)` with prior `1/2`. -/
noncomputable def post (p : ℝ) (h : List Bool) (s : Bool) : ℝ :=
  lik p true h * sigLik p true s /
    (lik p true h * sigLik p true s + lik p false h * sigLik p false s)

/-! ### The action probabilities in each regime -/

lemma actProb_up {p : ℝ} {v : Bool} {d : ℤ} (hd : 2 ≤ d) (a : Bool) :
    actProb p v d a = if a then 1 else 0 := by
  cases v <;> cases a <;> simp [actProb, adoptProb, sigLik, hd]

lemma actProb_down {p : ℝ} {v : Bool} {d : ℤ} (hd : d ≤ -2) (a : Bool) :
    actProb p v d a = if a then 0 else 1 := by
  have h2 : ¬ 2 ≤ d := by omega
  cases v <;> cases a <;> simp [actProb, adoptProb, sigLik, hd, h2]

lemma actProb_zero (p : ℝ) (v a : Bool) : actProb p v 0 a = sigLik p v a := by
  cases v <;> cases a <;> simp [actProb, adoptProb, sigLik]

lemma actProb_one (p : ℝ) (v : Bool) :
    actProb p v 1 true = sigLik p v true + sigLik p v false / 2 ∧
    actProb p v 1 false = sigLik p v false / 2 := by
  constructor <;> simp [actProb, adoptProb] <;> ring

lemma actProb_negOne (p : ℝ) (v : Bool) :
    actProb p v (-1) true = sigLik p v true / 2 ∧
    actProb p v (-1) false = sigLik p v false + sigLik p v true / 2 := by
  constructor <;> simp [actProb, adoptProb] <;> ring

/-- The public difference of a history is one of five regimes. -/
lemma regimes (d : ℤ) : 2 ≤ d ∨ d ≤ -2 ∨ d = 1 ∨ d = 0 ∨ d = -1 := by omega

/-! ## The counting characterisation -/

/-- **The public likelihood ratio depends on the history only through `d`.**
For every history, `lik₁ · c0(d) = lik₀ · c1(d)`: the ratio `lik₁ / lik₀` is `1`
at `d = 0`, `p/(1-p)` at `d = 1`, and `p(1+p)/((1-p)(2-p))` at `d ≥ 2`
(reciprocals for negative `d`).  Footnote 23, p.1009. -/
theorem lik_ratio (p : ℝ) (h : List Bool) :
    lik p true h * c0 p (diff h) = lik p false h * c1 p (diff h) := by
  induction h with
  | nil => simp [lik, diff, c0, c1]
  | cons a h ih =>
    simp only [lik, diff]
    generalize hd : diff h = d at ih ⊢
    set L1 := lik p true h
    set L0 := lik p false h
    rcases regimes d with h2 | h2 | h2 | h2 | h2
    · have e1 : 2 ≤ d + 1 := by omega
      cases a
      · simp [actProb_up h2]
      · simp only [actProb_up h2, if_true]
        simp only [c0, c1, if_pos h2] at ih
        simp [c0, c1, e1]; linarith
    · have e1 : d + -1 ≤ -2 := by omega
      have e2 : ¬ 2 ≤ d + -1 := by omega
      have e3 : ¬ 2 ≤ d := by omega
      cases a
      · simp only [actProb_down h2]
        simp only [c0, c1, if_neg e3, if_pos h2] at ih
        simp [c0, c1, e1, e2]; linarith
      · simp [actProb_down h2]
    · subst h2
      obtain ⟨hT, hF⟩ := actProb_one p true
      obtain ⟨hT', hF'⟩ := actProb_one p false
      simp only [c0, c1] at ih
      norm_num at ih
      cases a
      · simp only [hF, hF', sigLik]; norm_num [c0, c1]
        linear_combination (1 / 2 : ℝ) * ih
      · simp only [hT, hT', sigLik]; norm_num [c0, c1]
        linear_combination ((2 - p) * (1 + p) / 2) * ih
    · subst h2
      simp only [c0, c1] at ih
      norm_num at ih
      cases a
      · simp only [actProb_zero, sigLik]; norm_num [c0, c1]; rw [ih]; ring
      · simp only [actProb_zero, sigLik]; norm_num [c0, c1]; rw [ih]; ring
    · subst h2
      obtain ⟨hT, hF⟩ := actProb_negOne p true
      obtain ⟨hT', hF'⟩ := actProb_negOne p false
      simp only [c0, c1] at ih
      norm_num at ih
      cases a
      · simp only [hF, hF', sigLik]; norm_num [c0, c1]
        linear_combination ((1 + p) * (2 - p) / 2) * ih
      · simp only [hT, hT', sigLik]; norm_num [c0, c1]
        linear_combination (1 / 2 : ℝ) * ih

lemma sigLik_nonneg {p : ℝ} (hp0 : 0 ≤ p) (hp1 : p ≤ 1) (v s : Bool) : 0 ≤ sigLik p v s := by
  unfold sigLik; split_ifs <;> linarith

lemma adoptProb_mem (d : ℤ) (s : Bool) : 0 ≤ adoptProb d s ∧ adoptProb d s ≤ 1 := by
  unfold adoptProb; split_ifs <;> norm_num

lemma actProb_nonneg {p : ℝ} (hp0 : 0 ≤ p) (hp1 : p ≤ 1) (v : Bool) (d : ℤ) (a : Bool) :
    0 ≤ actProb p v d a := by
  have h1 := adoptProb_mem d true
  have h2 := adoptProb_mem d false
  unfold actProb
  apply add_nonneg <;> apply mul_nonneg (sigLik_nonneg hp0 hp1 _ _) <;> split_ifs <;> linarith

lemma lik_nonneg {p : ℝ} (hp0 : 0 ≤ p) (hp1 : p ≤ 1) (v : Bool) (h : List Bool) :
    0 ≤ lik p v h := by
  induction h with
  | nil => simp [lik]
  | cons a h ih => exact mul_nonneg ih (actProb_nonneg hp0 hp1 v _ a)

/-- Posterior above / below / at one half is decided by comparing the two
likelihood-weighted numerators. -/
lemma half_cmp {A B : ℝ} (hAB : 0 < A + B) :
    (1 / 2 < A / (A + B) ↔ B < A) ∧ (A / (A + B) < 1 / 2 ↔ A < B) ∧
    (A / (A + B) = 1 / 2 ↔ A = B) := by
  refine ⟨?_, ?_, ?_⟩
  · rw [lt_div_iff₀ hAB]; constructor <;> intro h <;> linarith
  · rw [div_lt_iff₀ hAB]; constructor <;> intro h <;> linarith
  · rw [div_eq_iff hAB.ne']; constructor <;> intro h <;> linarith

/-- The Bayes rule with the paper's tie-break, as a probability of adopting. -/
noncomputable def bayesAdopt (q : ℝ) : ℝ := if 1 / 2 < q then 1 else if q < 1 / 2 then 0 else 1 / 2

/-- **The counting rule is the Bayes rule** (Sec. IIA, pp.996-997).  For every
history of positive probability and every signal, the counting rule's adoption
probability equals the Bayes-optimal one: adopt if the posterior exceeds `C = 1/2`,
reject if below, flip a coin if equal. -/
theorem counting_rule_is_bayes {p : ℝ} (hp : 1 / 2 < p) (hp1 : p < 1) (h : List Bool)
    (hpos : 0 < lik p true h + lik p false h) (s : Bool) :
    adoptProb (diff h) s = bayesAdopt (post p h s) := by
  have hr := lik_ratio p h
  have n1 := lik_nonneg (by linarith) hp1.le true h
  have n0 := lik_nonneg (by linarith) hp1.le false h
  set L1 := lik p true h
  set L0 := lik p false h
  -- both likelihoods are positive in every regime, since `c0, c1 > 0`
  have hq : 0 < 1 - p := by linarith
  have hc1 : 0 < c1 p (diff h) := by unfold c1; split_ifs <;> nlinarith
  have hc0 : 0 < c0 p (diff h) := by unfold c0; split_ifs <;> nlinarith
  have hL1 : 0 < L1 := by
    rcases n1.lt_or_eq with h1 | h1
    · exact h1
    · exfalso; rw [← h1] at hr
      have : L0 = 0 := by nlinarith
      linarith
  have hL0 : 0 < L0 := by
    rcases n0.lt_or_eq with h0 | h0
    · exact h0
    · exfalso; rw [← h0] at hr
      have : L1 = 0 := by nlinarith
      linarith
  have key : ∀ A B : ℝ, 0 < A → 0 < B →
      bayesAdopt (A / (A + B)) = if B < A then 1 else if A < B then 0 else 1 / 2 := by
    intro A B hA hB
    obtain ⟨e1, e2, _⟩ := half_cmp (A := A) (B := B) (by linarith)
    unfold bayesAdopt
    by_cases h1 : B < A
    · rw [if_pos (e1.mpr h1), if_pos h1]
    · have n1 : ¬ 1 / 2 < A / (A + B) := fun h => h1 (e1.mp h)
      rw [if_neg n1, if_neg h1]
      by_cases h2 : A < B
      · rw [if_pos (e2.mpr h2), if_pos h2]
      · rw [if_neg (fun h => h2 (e2.mp h)), if_neg h2]
  have hsp : ∀ v, 0 < sigLik p v s := by
    intro v; unfold sigLik; split_ifs <;> linarith
  unfold post
  rw [key _ _ (mul_pos hL1 (hsp true)) (mul_pos hL0 (hsp false))]
  generalize hd : diff h = d at hr
  have hp0 : 0 < p := by linarith
  have h2p : 0 < 2 * p - 1 := by linarith
  rcases regimes d with h2 | h2 | h2 | h2 | h2
  · simp only [c0, c1, if_pos h2] at hr
    have base : L0 * p < L1 * (1 - p) := by
      by_contra hc
      push Not at hc
      have := mul_le_mul_of_nonneg_right hc (by linarith : (0 : ℝ) ≤ 2 - p)
      nlinarith [mul_pos (mul_pos hL0 hp0) h2p]
    have lt : L0 * sigLik p false s < L1 * sigLik p true s := by
      cases s
      · simpa [sigLik] using base
      · rw [sigLik_tt, sigLik_ft]; nlinarith
    have hA : adoptProb d s = 1 := by simp [adoptProb, h2]
    rw [hA, if_pos lt]
  · have e3 : ¬ 2 ≤ d := by omega
    simp only [c0, c1, if_neg e3, if_pos h2] at hr
    have base : L1 * p < L0 * (1 - p) := by
      by_contra hc
      push Not at hc
      have := mul_le_mul_of_nonneg_right hc (by linarith : (0 : ℝ) ≤ 2 - p)
      nlinarith [mul_pos (mul_pos hL1 hp0) h2p]
    have lt : L1 * sigLik p true s < L0 * sigLik p false s := by
      cases s
      · rw [sigLik_tf, sigLik_ff]; nlinarith
      · simpa [sigLik] using base
    have hA : adoptProb d s = 0 := by simp [adoptProb, h2, e3]
    rw [hA, if_neg (not_lt.mpr lt.le), if_pos lt]
  · subst h2
    norm_num [c0, c1] at hr
    cases s
    · have eq : L1 * sigLik p true false = L0 * sigLik p false false := by
        rw [sigLik_tf, sigLik_ff]; linarith
      have hA : adoptProb 1 false = 1 / 2 := by norm_num [adoptProb]
      rw [hA, if_neg (by rw [eq]; exact lt_irrefl _), if_neg (by rw [eq]; exact lt_irrefl _)]
    · have lt : L0 * sigLik p false true < L1 * sigLik p true true := by
        rw [sigLik_tt, sigLik_ft]; nlinarith
      have hA : adoptProb 1 true = 1 := by norm_num [adoptProb]
      rw [hA, if_pos lt]
  · subst h2
    norm_num [c0, c1] at hr
    cases s
    · have lt : L1 * sigLik p true false < L0 * sigLik p false false := by
        rw [sigLik_tf, sigLik_ff]; rw [hr]; nlinarith
      have hA : adoptProb 0 false = 0 := by norm_num [adoptProb]
      rw [hA, if_neg (not_lt.mpr lt.le), if_pos lt]
    · have lt : L0 * sigLik p false true < L1 * sigLik p true true := by
        rw [sigLik_tt, sigLik_ft]; rw [hr]; nlinarith
      have hA : adoptProb 0 true = 1 := by norm_num [adoptProb]
      rw [hA, if_pos lt]
  · subst h2
    norm_num [c0, c1] at hr
    cases s
    · have lt : L1 * sigLik p true false < L0 * sigLik p false false := by
        rw [sigLik_tf, sigLik_ff]; nlinarith
      have hA : adoptProb (-1) false = 0 := by norm_num [adoptProb]
      rw [hA, if_neg (not_lt.mpr lt.le), if_pos lt]
    · have eq : L1 * sigLik p true true = L0 * sigLik p false true := by
        rw [sigLik_tt, sigLik_ft]; linarith
      have hA : adoptProb (-1) true = 1 / 2 := by norm_num [adoptProb]
      rw [hA, if_neg (by rw [eq]; exact lt_irrefl _), if_neg (by rw [eq]; exact lt_irrefl _)]

/-! ## Cascades -/

/-- **A cascade's action does not depend on the private signal** (the DEFINITION,
p.1000): once `d ≥ 2` both signals adopt; once `d ≤ -2` both reject. -/
theorem cascade_ignores_signal (d : ℤ) :
    (2 ≤ d → adoptProb d true = 1 ∧ adoptProb d false = 1) ∧
    (d ≤ -2 → adoptProb d true = 0 ∧ adoptProb d false = 0) := by
  refine ⟨fun hd => by simp [adoptProb, hd], fun hd => ?_⟩
  have : ¬ 2 ≤ d := by omega
  simp [adoptProb, hd, this]

/-- **A cascade's action conveys no information** (p.1000, "If an individual i is
in a cascade, then his action conveys no information"; p.999, "actions convey no
information about private signals").  In an UP cascade the adopt action
multiplies the likelihood under both values of `V` by `1`, so the next
individual's posterior is the same as his predecessor's, and the reject action
has probability zero. -/
theorem cascade_uninformative {p : ℝ} {h : List Bool} (hd : 2 ≤ diff h) :
    (∀ v, lik p v (true :: h) = lik p v h) ∧ (∀ v, lik p v (false :: h) = 0) ∧
    (∀ s, post p (true :: h) s = post p h s) := by
  have e : ∀ v, lik p v (true :: h) = lik p v h := by
    intro v; simp [lik, actProb_up hd]
  refine ⟨e, fun v => by simp [lik, actProb_up hd], fun s => by simp [post, e]⟩

/-- The same for a DOWN cascade. -/
theorem cascade_uninformative_down {p : ℝ} {h : List Bool} (hd : diff h ≤ -2) :
    (∀ v, lik p v (false :: h) = lik p v h) ∧ (∀ v, lik p v (true :: h) = 0) := by
  exact ⟨fun v => by simp [lik, actProb_down hd], fun v => by simp [lik, actProb_down hd]⟩

/-- **A cascade once started lasts forever** (p.1000): after any number `k` of
further individuals, the history has only adoptions appended, the public
likelihoods are unchanged, and the difference stays `≥ 2`. -/
theorem cascade_forever {p : ℝ} {h : List Bool} (hd : 2 ≤ diff h) (k : ℕ) :
    2 ≤ diff (List.replicate k true ++ h) ∧
    ∀ v, lik p v (List.replicate k true ++ h) = lik p v h := by
  induction k with
  | zero => simpa using hd
  | succ k ih =>
    obtain ⟨ih1, ih2⟩ := ih
    refine ⟨?_, fun v => ?_⟩
    · simp only [List.replicate_succ, List.cons_append, diff]; simp; omega
    · simp only [List.replicate_succ, List.cons_append]
      rw [(cascade_uninformative (p := p) ih1).1 v, ih2 v]

/-- Before a cascade (`|d| ≤ 1`) the adopt action is informative: its probability
differs across `V = 1` and `V = 0` whenever `p ≠ 1/2`. -/
theorem informative_outside_cascade {p : ℝ} (hp : 1 / 2 < p) {d : ℤ} (h1 : -1 ≤ d) (h2 : d ≤ 1) :
    actProb p true d true ≠ actProb p false d true := by
  have : d = -1 ∨ d = 0 ∨ d = 1 := by omega
  rcases this with rfl | rfl | rfl
  · rw [(actProb_negOne p true).1, (actProb_negOne p false).1]; simp [sigLik]; intro h; linarith
  · rw [actProb_zero, actProb_zero]; simp [sigLik]; intro h; linarith
  · rw [(actProb_one p true).1, (actProb_one p false).1]; simp [sigLik]; intro h; linarith

/-! ## Closed-form cascade probabilities: eqs. (1)-(3) -/

/-- Distribution over the five regimes after `n` individuals:
UP cascade (`d ≥ 2`), `d = 1`, `d = 0`, `d = -1`, DOWN cascade (`d ≤ -2`). -/
structure Dist where
  up : ℝ
  p1 : ℝ
  z : ℝ
  m1 : ℝ
  dn : ℝ

/-- One individual's move, with transition probabilities `actProb` of the
counting rule. -/
noncomputable def step (p : ℝ) (v : Bool) (D : Dist) : Dist where
  up := D.up * actProb p v 2 true + D.p1 * actProb p v 1 true
  p1 := D.z * actProb p v 0 true
  z := D.p1 * actProb p v 1 false + D.m1 * actProb p v (-1) true
  m1 := D.z * actProb p v 0 false
  dn := D.dn * actProb p v (-2) false + D.m1 * actProb p v (-1) false

/-- The distribution after `n` individuals, starting from `d = 0`. -/
noncomputable def dist (p : ℝ) (v : Bool) : ℕ → Dist
  | 0 => ⟨0, 0, 1, 0, 0⟩
  | n + 1 => step p v (dist p v n)

/-- `r = p - p²`, the probability of no cascade over one pair. -/
def r (p : ℝ) : ℝ := p - p ^ 2

lemma den_pos (p : ℝ) : 0 < 1 - p + p ^ 2 := by nlinarith [sq_nonneg (p - 1 / 2)]

/-- Two moves from any distribution, given `V = v`. -/
lemma step_step (p : ℝ) (v : Bool) (D : Dist) (h1 : D.p1 = 0) (hm : D.m1 = 0) :
    (step p v (step p v D)).up = D.up + D.z * sigLik p v true *
        (sigLik p v true + sigLik p v false / 2) ∧
    (step p v (step p v D)).z = D.z * sigLik p v true * sigLik p v false ∧
    (step p v (step p v D)).dn = D.dn + D.z * sigLik p v false *
        (sigLik p v false + sigLik p v true / 2) ∧
    (step p v (step p v D)).p1 = 0 ∧ (step p v (step p v D)).m1 = 0 := by
  have u2 : actProb p v 2 true = 1 := by rw [actProb_up (by norm_num)]; simp
  have d2 : actProb p v (-2) false = 1 := by rw [actProb_down (by norm_num)]; simp
  obtain ⟨o1, o2⟩ := actProb_one p v
  obtain ⟨n1, n2⟩ := actProb_negOne p v
  simp only [step, u2, d2, o1, o2, n1, n2, actProb_zero, h1, hm]
  refine ⟨by ring, by ring, by ring, by ring, by ring⟩

/-- **Eqs. (2)-(3)** (p.998): after `n = 2m` individuals, given `V = 1`,
`Pr(UP) = p(p+1)[1-(p-p²)^m] / (2(1-p+p²))`, `Pr(no cascade) = (p-p²)^m`,
`Pr(DOWN) = (p-2)(p-1)[1-(p-p²)^m] / (2(1-p+p²))`, and the odd regimes are empty.
Also the mirror image given `V = 0`. -/
theorem eq3 (p : ℝ) (m : ℕ) :
    (dist p true (2 * m)).up = p * (p + 1) * (1 - r p ^ m) / (2 * (1 - p + p ^ 2)) ∧
    (dist p true (2 * m)).z = r p ^ m ∧
    (dist p true (2 * m)).dn = (p - 2) * (p - 1) * (1 - r p ^ m) / (2 * (1 - p + p ^ 2)) ∧
    (dist p true (2 * m)).p1 = 0 ∧ (dist p true (2 * m)).m1 = 0 ∧
    (dist p false (2 * m)).up = (p - 2) * (p - 1) * (1 - r p ^ m) / (2 * (1 - p + p ^ 2)) ∧
    (dist p false (2 * m)).z = r p ^ m ∧
    (dist p false (2 * m)).dn = p * (p + 1) * (1 - r p ^ m) / (2 * (1 - p + p ^ 2)) ∧
    (dist p false (2 * m)).p1 = 0 ∧ (dist p false (2 * m)).m1 = 0 := by
  have hden := (den_pos p).ne'
  induction m with
  | zero => simp [dist]
  | succ m ih =>
    obtain ⟨a1, a2, a3, a4, a5, b1, b2, b3, b4, b5⟩ := ih
    have e : 2 * (m + 1) = 2 * m + 1 + 1 := by ring
    rw [e]
    simp only [dist]
    obtain ⟨t1, t2, t3, t4, t5⟩ := step_step p true (dist p true (2 * m)) a4 a5
    obtain ⟨f1, f2, f3, f4, f5⟩ := step_step p false (dist p false (2 * m)) b4 b5
    rw [t1, t2, t3, f1, f2, f3, a1, a2, a3, b1, b2, b3]
    simp only [sigLik_tt, sigLik_tf, sigLik_ft, sigLik_ff, r, pow_succ]
    refine ⟨?_, ?_, ?_, t4, t5, ?_, ?_, ?_, f4, f5⟩ <;> field_simp <;> ring

/-- **Eq. (1)** (p.997): unconditionally (prior `1/2`), after `n = 2m`
individuals, `Pr(UP) = Pr(DOWN) = [1-(p-p²)^m]/2` and `Pr(no cascade) = (p-p²)^m`. -/
theorem eq1 (p : ℝ) (m : ℕ) :
    ((dist p true (2 * m)).up + (dist p false (2 * m)).up) / 2 = (1 - r p ^ m) / 2 ∧
    ((dist p true (2 * m)).z + (dist p false (2 * m)).z) / 2 = r p ^ m ∧
    ((dist p true (2 * m)).dn + (dist p false (2 * m)).dn) / 2 = (1 - r p ^ m) / 2 := by
  obtain ⟨a1, a2, a3, -, -, b1, b2, b3, -, -⟩ := eq3 p m
  have hden := (den_pos p).ne'
  rw [a1, a2, a3, b1, b2, b3]
  refine ⟨?_, by ring, ?_⟩ <;> field_simp <;> ring

/-- p.997-998: "Even for a very noisy signal, as when `p = 1/2 + ε` ... this
probability [of no cascade] after only 10 individuals is less than 0.1 percent!"
In fact it holds for every `p ∈ [0,1]`, since `p - p² ≤ 1/4`. -/
theorem no_cascade_after_ten {p : ℝ} (h0 : 0 ≤ p) (h1 : p ≤ 1) :
    ((dist p true (2 * 5)).z + (dist p false (2 * 5)).z) / 2 < 1 / 1000 := by
  rw [(eq1 p 5).2.1]
  have hr0 : 0 ≤ r p := by unfold r; nlinarith
  have hr : r p ≤ 1 / 4 := by unfold r; nlinarith [sq_nonneg (p - 1 / 2)]
  calc r p ^ 5 ≤ (1 / 4) ^ 5 := pow_le_pow_left₀ hr0 hr 5
    _ < 1 / 1000 := by norm_num

/-- The `n → ∞` limit of the correct-cascade probability (Fig. 1, p.998). -/
theorem tendsto_correct_cascade {p : ℝ} (h0 : 0 ≤ p) (h1 : p ≤ 1) :
    Filter.Tendsto (fun m : ℕ => (dist p true (2 * m)).up) Filter.atTop
      (nhds (p * (p + 1) / (2 * (1 - p + p ^ 2)))) := by
  have hr0 : 0 ≤ r p := by unfold r; nlinarith
  have hr : r p < 1 := by unfold r; nlinarith [sq_nonneg (p - 1 / 2)]
  have ht := tendsto_pow_atTop_nhds_zero_of_lt_one hr0 hr
  have : Filter.Tendsto (fun m : ℕ => p * (p + 1) * (1 - r p ^ m) / (2 * (1 - p + p ^ 2)))
      Filter.atTop (nhds (p * (p + 1) * (1 - 0) / (2 * (1 - p + p ^ 2)))) := by
    apply Filter.Tendsto.div_const
    exact (ht.const_sub 1).const_mul _
  simp only [sub_zero, mul_one] at this
  refine this.congr (fun m => ?_)
  rw [(eq3 p m).1]

/-! ## Result 4's algebra (Appendix, pp.1020-1023) -/

/-- **(A6)** (p.1020).  The probability that an UP cascade starts exactly at pair
`k+1` is `(p(p+1)/2) r^k` given `V = 1` and `((2-p)(1-p)/2) r^k` given `V = 0`
(increments of `eq3`), so by Bayes with prior `1/2`,
`Pr(V = 1 | UP started in period 2(k+1)) = p(p+1)/(2(p²-p+1))`, which exceeds `1/2`
for `p > 1/2`. -/
theorem A6_posterior_up {p : ℝ} (hp : 1 / 2 < p) (hp1 : p < 1) (k : ℕ) :
    let a := (dist p true (2 * (k + 1))).up - (dist p true (2 * k)).up
    let b := (dist p false (2 * (k + 1))).up - (dist p false (2 * k)).up
    a = p * (p + 1) / 2 * r p ^ k ∧ b = (2 - p) * (1 - p) / 2 * r p ^ k ∧
    (0 < r p → a / (a + b) = p * (p + 1) / (2 * (p ^ 2 - p + 1))) ∧
    1 / 2 < p * (p + 1) / (2 * (p ^ 2 - p + 1)) := by
  intro a b
  have hden := (den_pos p).ne'
  have ha : a = p * (p + 1) / 2 * r p ^ k := by
    simp only [a, (eq3 p (k + 1)).1, (eq3 p k).1, pow_succ, r]
    field_simp; ring
  have hb : b = (2 - p) * (1 - p) / 2 * r p ^ k := by
    simp only [b, (eq3 p (k + 1)).2.2.2.2.2.1, (eq3 p k).2.2.2.2.2.1, pow_succ, r]
    field_simp; ring
  have hpos : 0 < p ^ 2 - p + 1 := by nlinarith [sq_nonneg (p - 1 / 2)]
  refine ⟨ha, hb, fun hr => ?_, ?_⟩
  · rw [ha, hb]
    have hrk : 0 < r p ^ k := pow_pos hr k
    have : p * (p + 1) / 2 * r p ^ k + (2 - p) * (1 - p) / 2 * r p ^ k
        = (p ^ 2 - p + 1) * r p ^ k := by ring
    rw [this]; field_simp
  · rw [lt_div_iff₀ (by linarith)]; nlinarith

/-- **(A10)** (p.1022): with `A = p(p+1)/(2(p²-p+1))` and `B = (2-p)(1-p)/(2(p²-p+1))`
from (A6)/(A7), and `Pr(W = V) = .95`, `E[W | UP, L]` has the printed closed form. -/
theorem A10_closed_form {p : ℝ} (hp : 1 / 2 < p) (hp1 : p < 1) :
    let A := p * (p + 1) / (2 * (p ^ 2 - p + 1))
    let B := (2 - p) * (1 - p) / (2 * (p ^ 2 - p + 1))
    (A * (95 / 100) * (1 - p) + B * (5 / 100) * (1 - p)) /
      ((A * (95 / 100) * (1 - p) + B * (5 / 100) * (1 - p)) +
        (A * (5 / 100) * p + B * (95 / 100) * p))
      = (1 - p) * (2 + 16 * p + 20 * p ^ 2) / (2 + 52 * p - 52 * p ^ 2) := by
  intro A B
  have hpos : 0 < p ^ 2 - p + 1 := by nlinarith [sq_nonneg (p - 1 / 2)]
  have h52 : 0 < 2 + 52 * p - 52 * p ^ 2 := by nlinarith
  simp only [A, B]
  rw [div_eq_div_iff (by
    have : 0 < (p * (p + 1) / (2 * (p ^ 2 - p + 1)) * (95 / 100) * (1 - p) +
        (2 - p) * (1 - p) / (2 * (p ^ 2 - p + 1)) * (5 / 100) * (1 - p)) +
        (p * (p + 1) / (2 * (p ^ 2 - p + 1)) * (5 / 100) * p +
        (2 - p) * (1 - p) / (2 * (p ^ 2 - p + 1)) * (95 / 100) * p) := by
      have h1 : 0 < 1 - p := by linarith
      have : 0 < p * (p + 1) / (2 * (p ^ 2 - p + 1)) := by positivity
      have : 0 < (2 - p) * (1 - p) / (2 * (p ^ 2 - p + 1)) := by
        apply div_pos; nlinarith; positivity
      positivity
    exact this.ne') h52.ne']
  field_simp
  ring

/-- (A10) at `p = .9` is `.488` (to three places), below `1/2`. -/
theorem A10_value :
    let p : ℝ := 9 / 10
    (1 - p) * (2 + 16 * p + 20 * p ^ 2) / (2 + 52 * p - 52 * p ^ 2) < 1 / 2 ∧
    |(1 - p) * (2 + 16 * p + 20 * p ^ 2) / (2 + 52 * p - 52 * p ^ 2) - 488 / 1000| < 1 / 2000 := by
  norm_num [abs_lt]

/-- **(A11)** (p.1023): the printed simplification
`p(p+1)(1-p)² + (2-p)(1-p)p² = p(1-p)(1+2p-2p²)`. -/
theorem A11_simplify (p : ℝ) :
    p * (p + 1) * (1 - p) ^ 2 + (2 - p) * (1 - p) * p ^ 2 = p * (1 - p) * (1 + 2 * p - 2 * p ^ 2) := by
  ring

/-- (A11) at `p = .9`: the reversal lower bound is `.0935` (to four places), and
`.0935 / .05 = 1.87` ("87 percent higher", p.1015). -/
theorem A11_value :
    let p : ℝ := 9 / 10
    let v := 9 / 10 * (p * (1 - p) * (1 + 2 * p - 2 * p ^ 2) / (2 * (p ^ 2 - p + 1))) +
      5 / 100 * ((1 - p) ^ 2 + p ^ 2)
    |v - 935 / 10000| < 1 / 20000 ∧ (935 / 10000 : ℝ) / (5 / 100) = 187 / 100 := by
  norm_num [abs_lt]

end Literature.BHW
