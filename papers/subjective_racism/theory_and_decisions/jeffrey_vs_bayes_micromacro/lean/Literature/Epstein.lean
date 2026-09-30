/-
# Epstein (2006), "An Axiomatic Model of Non-Bayesian Updating"

*Review of Economic Studies* 73, 413-436 (published version, 24 pp.).

Formalization of the belief algebra in the paper's representation, not of Paper
B's claims (except in the last section, which is labelled as the Paper B
question).  Page numbers are the journal's printed pages (PDF page + 412).

## The setting (pp. 416-419)

Three periods: at `t = 0` the agent ranks *contingent menus*
`F : S1 → M(S2)`; at `t = 1` a signal `s1 ∈ S1` is realized, she updates and
picks an act from `F(s1)`; at `t = 2` the state `s2 ∈ S2` is realized.  `S1` and
`S2` are finite (p. 417).  There is **one** interim signal.

**Theorem 1 (p. 425).** Order, Continuity, Independence, Nondegeneracy,
Set-Betweenness, SRL, State Independence and `S1`-Full Support hold iff `≽` has
the representation (4)-(6) with Reg1-Reg4:
  * (4) `U(F) = ∫_{S1} U(F(s1); s1) dp1`;
  * (6) `U(F(s1); s1) = max_{f∈F(s1)} {∫u(f)dp(·|s1) + α(s1)∫u(f)dq(·|s1)}
                      - max_{f'∈F(s1)} α(s1)∫u(f')dq(·|s1)`, the
    Gul-Pesendorfer form (1) `U(A) = max_x {U(x) + V(x)} - max_y V(y)` (p. 415)
    with a change of *beliefs* as the temptation;
  * Reg2: `q(·|s1) ≪ p(·|s1)`; Reg3: `α ≥ 0`; Reg4: `p1` has full support.

Interim choice maximizes (8), which is expected utility under the Bayesian
update of the *compromise prior* (9)
`p*(s1,s2) = [p(s2|s1) + α(s1) q(s2|s1)] / (1 + α(s1)) · p1(s1)`, whose
conditional is (10) (p. 419).

**Corollary 3 (p. 429).** Under the axioms of Theorem 1, Prior-Bias (Positive,
Negative Prior-Bias) holds iff for each `s1` either `α(s1) = 0` or `α(s1) > 0`
and (23) `q(·|s1) = (1-λ(s1)) p(·|s1) + λ(s1) p2(·)` with `λ(s1) ≤ 1`
(`0 < λ(s1) ≤ 1`, `λ(s1) ≤ 0`).  (23) is the same display as (11) (p. 420).

## What is formalized (finite `S1`, `S2`; measures as real tables)

  * Bayesian conditionals `cond`, marginals, `cond_sum`, `joint_eq_marg_mul_cond`.
  * The GP form (1) / menu utility (6) for a finite menu, and its reduction to
    conditional expected utility under `p` on singleton menus (the
    commitment case behind (7)): `gp`, `menuU_singleton`.
  * (9)-(10): `pstar`, `marg1_pstar` (same `S1`-marginal), `cond_pstar`,
    `cond_pstar_mix` (the `s1`-dependent mixture of the conditionals of `p` and
    `q`), `compromise_objective` ((8) is `(1+α)` times expected utility under
    `p*(·|s1)`).
  * Corollary 3's algebra: `cond_pstar_priorBias` (eq. (12), with
    `s1`-dependent `α`, `λ`): `p*(·|s1) = (1-γ) p(·|s1) + γ p2`,
    `γ = αλ/(1+α)`; `gam_lt_one`; `pstar_deviation`; `cor3_case1` (the
    step "solve (A.11) for `q(·|s1)` and deduce (23)", p. 433, with `λ ≤ 1`).
  * When updating is Bayesian: `bayes_of_lam_zero`, `bayes_of_alpha_zero`,
    `cond_eq_marg2_of_product` and `bayes_of_product` (a product `p` makes
    every signal neutral, `p(·|s1) = p2`, so (23) collapses to `p(·|s1)`
    whatever `λ`), `cond_eq_marg2_of_subsingleton` (`S1` a singleton);
    conversely `not_bayes_of_nonneutral`.
  * Section 2.3: `overreaction_form` (the rewrite on p. 420),
    `sensitivity_scaled` (differences across signals scale by `1-γ`),
    `confirm_marginal` (the claim after (13), p. 421), `representativeness`
    (p. 421), `sampleBias_nonneg_iff` (fn 10, p. 421).
  * Section 4 (pp. 429-430), the law of iterated expectations:
    `lie_violation_example` (explicit, `norm_num`): an act `f` with
    `{f} ≻ {-f}` at `t = 0` while `-f` is chosen from `{f,-f}` at every `s1`.
    **Qualification found:** `priorBias_no_uniform_reversal` shows that no
    agent satisfying Prior-Bias (23) can exhibit this; and
    `pstar_average_const` shows that with `γ` constant across signals the
    interim posteriors average back to `p2`, so the LIE holds.  The concluding
    remark's violation needs a `q` outside (23), or `γ` varying with `s1`.
  * Absolute continuity (Reg2) under (23): `priorBias_absCont` — if `λ(s1) ≠ 0`
    then `p(·|s1)` must charge every `s2` that `p2` charges.  Not stated in the
    paper.

## The Paper B question (last section)

`priorBias_eq_damped`, `compromise_eq_damped`: (23) and (12) are, state by
state, the project's partial-adoption target `(1-ω)·current + ω·delivered`
(`JeffreyOrder.dampedTarget`, with `current = Q.mB1`, `delivered = 1 - r₀`),
with `current = p2(s2)` and `delivered = p(s2|s1)`.  `ω = 1 - λ(s1)` for the
temptation posterior `q`; `ω = 1 - α(s1)λ(s1)/(1+α(s1))` (`omegaEff`) for the
posterior that governs choice.  `omegaEff_pos`: `ω = 0` is never attained;
`omegaEff_surj`: every `ω > 0` is.  `dampedJeffrey_eq_mix`: the project's
damped Jeffrey step is itself the (23)-type mixture of "no update" and "full
update" on the whole joint.

## Not formalized

Theorem 1 itself (both directions), Lemma 1, Corollaries 1-2, the sufficiency
direction of Corollary 3 beyond Case 1's algebra (the Motzkin step producing
(A.11), and Case 2, p. 433),
Theorem 2 / Appendix B, and the menus-of-acts topology.  The preference side
(axioms, contingent menus) is not modelled; only the belief objects the
representation produces are.
-/
import Mathlib

namespace Literature.Epstein

open Finset

variable {S1 S2 : Type*} [Fintype S1] [Fintype S2]

/-! ## Joint measures on `S1 × S2` and Bayesian conditionals -/

/-- `S1`-marginal `p1(s1) = ∑_{s2} p(s1,s2)`. -/
def marg1 (p : S1 → S2 → ℝ) (s1 : S1) : ℝ := ∑ s2, p s1 s2

/-- `S2`-marginal `p2(s2) = ∑_{s1} p(s1,s2)` (p. 420, "`p2` denotes the
`S2`-marginal of `p`"). -/
def marg2 (p : S1 → S2 → ℝ) (s2 : S2) : ℝ := ∑ s1, p s1 s2

/-- Total mass. -/
def total (p : S1 → S2 → ℝ) : ℝ := ∑ s1, ∑ s2, p s1 s2

/-- The Bayesian conditional `p(s2|s1) = p(s1,s2) / p1(s1)`. -/
noncomputable def cond (p : S1 → S2 → ℝ) (s1 : S1) (s2 : S2) : ℝ :=
  p s1 s2 / marg1 p s1

/-- Expected value of `v` under a measure `μ` on `S2`. -/
def expect (μ : S2 → ℝ) (v : S2 → ℝ) : ℝ := ∑ s2, μ s2 * v s2

omit [Fintype S1] in
theorem cond_sum (p : S1 → S2 → ℝ) {s1 : S1} (h : marg1 p s1 ≠ 0) :
    ∑ s2, cond p s1 s2 = 1 := by
  unfold cond
  rw [← Finset.sum_div]
  exact div_self h

omit [Fintype S1] in
/-- `p` is generated by `p1` and its conditionals (p. 419). -/
theorem joint_eq_marg_mul_cond (p : S1 → S2 → ℝ) {s1 : S1} (h : marg1 p s1 ≠ 0)
    (s2 : S2) : p s1 s2 = marg1 p s1 * cond p s1 s2 := by
  unfold cond
  field_simp

theorem sum_marg2 (p : S1 → S2 → ℝ) : ∑ s2, marg2 p s2 = total p := by
  unfold marg2 total
  exact Finset.sum_comm

theorem sum_marg1 (p : S1 → S2 → ℝ) : ∑ s1, marg1 p s1 = total p := rfl

/-- The tower property for the commitment prior:
`∑_{s1} p1(s1) p(s2|s1) = p2(s2)`. -/
theorem tower (p : S1 → S2 → ℝ) (h : ∀ s1, marg1 p s1 ≠ 0) (s2 : S2) :
    ∑ s1, marg1 p s1 * cond p s1 s2 = marg2 p s2 := by
  unfold marg2
  refine Finset.sum_congr rfl (fun s1 _ => ?_)
  rw [← joint_eq_marg_mul_cond p (h s1)]

/-! ## The Gul-Pesendorfer form (1) and the menu utility (6) -/

/-- **Eq. (1)** (p. 415): `U(A) = max_{x∈A} {U(x) + V(x)} - max_{y∈A} V(y)`,
for a finite nonempty menu. -/
noncomputable def gp {X : Type*} (U V : X → ℝ) (A : Finset X) (hA : A.Nonempty) : ℝ :=
  A.sup' hA (fun x => U x + V x) - A.sup' hA V

/-- **Eq. (6)** (p. 418) at a fixed `s1`: a menu of acts, each act given by its
utility vector `u ∘ f : S2 → ℝ`, valued by the GP form with
`U(f) = ∫u(f) dp(·|s1)` and `V(f) = α ∫u(f) dq(·|s1)`. -/
noncomputable def menuU (pc qc : S2 → ℝ) (a : ℝ) (M : Finset (S2 → ℝ))
    (hM : M.Nonempty) : ℝ :=
  gp (fun v => expect pc v) (fun v => a * expect qc v) M hM

/-- On a singleton menu (commitment) (6) is conditional expected utility under
the commitment prior: temptation plays no role.  With (4) this gives (7). -/
theorem menuU_singleton (pc qc : S2 → ℝ) (a : ℝ) (v : S2 → ℝ) :
    menuU pc qc a {v} (Finset.singleton_nonempty v) = expect pc v := by
  simp [menuU, gp]

/-! ## The compromise prior, eqs. (9)-(10) -/

/-- **Eq. (9)** (p. 419): `p*(s1,s2) = [p(s2|s1) + α(s1) q(s2|s1)]/(1+α(s1)) · p1(s1)`.
`qc` is the conditional kernel `q(·|s1)`. -/
noncomputable def pstar (p : S1 → S2 → ℝ) (qc : S1 → S2 → ℝ) (α : S1 → ℝ) :
    S1 → S2 → ℝ :=
  fun s1 s2 => (cond p s1 s2 + α s1 * qc s1 s2) / (1 + α s1) * marg1 p s1

omit [Fintype S1] in
/-- `p*` has the same `S1`-marginal as `p`. -/
theorem marg1_pstar (p qc : S1 → S2 → ℝ) (α : S1 → ℝ) {s1 : S1}
    (hp : marg1 p s1 ≠ 0) (hq : ∑ s2, qc s1 s2 = 1) (ha : 0 ≤ α s1) :
    marg1 (pstar p qc α) s1 = marg1 p s1 := by
  have h1 := cond_sum p hp
  have ha' : (1 : ℝ) + α s1 ≠ 0 := by linarith
  show ∑ s2, (cond p s1 s2 + α s1 * qc s1 s2) / (1 + α s1) * marg1 p s1 = marg1 p s1
  rw [← Finset.sum_mul, ← Finset.sum_div, Finset.sum_add_distrib, ← Finset.mul_sum, h1, hq]
  field_simp

omit [Fintype S1] in
/-- **Eq. (10)** (p. 419): the Bayesian update of `p*` is
`p*(·|s1) = [p(·|s1) + α(s1) q(·|s1)] / (1 + α(s1))`. -/
theorem cond_pstar (p qc : S1 → S2 → ℝ) (α : S1 → ℝ) {s1 : S1}
    (hp : marg1 p s1 ≠ 0) (hq : ∑ s2, qc s1 s2 = 1) (ha : 0 ≤ α s1) (s2 : S2) :
    cond (pstar p qc α) s1 s2 = (cond p s1 s2 + α s1 * qc s1 s2) / (1 + α s1) := by
  have ha' : (1 : ℝ) + α s1 ≠ 0 := by linarith
  show pstar p qc α s1 s2 / marg1 (pstar p qc α) s1 = _
  rw [marg1_pstar p qc α hp hq ha]
  simp only [pstar]
  field_simp

omit [Fintype S1] in
/-- (10) as an `s1`-dependent mixture of the conditionals of `p` and `q`, with
weights `1/(1+α(s1))` and `α(s1)/(1+α(s1))` (p. 416, "each `s1`-conditional of
`p*` is a mixture of the conditionals of `p` and `q`, where the mixture weights
may vary with the signal"). -/
theorem cond_pstar_mix (p qc : S1 → S2 → ℝ) (α : S1 → ℝ) {s1 : S1}
    (hp : marg1 p s1 ≠ 0) (hq : ∑ s2, qc s1 s2 = 1) (ha : 0 ≤ α s1) (s2 : S2) :
    cond (pstar p qc α) s1 s2
      = (1 / (1 + α s1)) * cond p s1 s2 + (α s1 / (1 + α s1)) * qc s1 s2 := by
  have ha' : (1 : ℝ) + α s1 ≠ 0 := by linarith
  rw [cond_pstar p qc α hp hq ha]
  field_simp

omit [Fintype S1] in
/-- **(8) is expected utility under `p*(·|s1)`** (p. 419): for every act,
`∫u(f)dp(·|s1) + α ∫u(f)dq(·|s1) = (1+α) ∫u(f) dp*(·|s1)`, so maximizing (8) over
a menu is maximizing expected utility under the compromise posterior. -/
theorem compromise_objective (p qc : S1 → S2 → ℝ) (α : S1 → ℝ) {s1 : S1}
    (hp : marg1 p s1 ≠ 0) (hq : ∑ s2, qc s1 s2 = 1) (ha : 0 ≤ α s1) (v : S2 → ℝ) :
    expect (cond p s1) v + α s1 * expect (qc s1) v
      = (1 + α s1) * expect (cond (pstar p qc α) s1) v := by
  have ha' : (1 : ℝ) + α s1 ≠ 0 := by linarith
  unfold expect
  rw [Finset.mul_sum, Finset.mul_sum, ← Finset.sum_add_distrib]
  refine Finset.sum_congr rfl (fun s2 _ => ?_)
  rw [cond_pstar p qc α hp hq ha]
  field_simp

omit [Fintype S1] in
/-- `α(s1) = 0`: interim choice is Bayesian, whatever `q`. -/
theorem bayes_of_alpha_zero (p qc : S1 → S2 → ℝ) (α : S1 → ℝ) {s1 : S1}
    (hp : marg1 p s1 ≠ 0) (hq : ∑ s2, qc s1 s2 = 1) (ha : α s1 = 0) (s2 : S2) :
    cond (pstar p qc α) s1 s2 = cond p s1 s2 := by
  rw [cond_pstar p qc α hp hq ha.ge, ha]
  ring

/-! ## Prior-Bias: eqs. (11)/(23) and (12) -/

/-- **Eq. (23)** (p. 429) = **(11)** (p. 420):
`q(·|s1) = (1-λ(s1)) p(·|s1) + λ(s1) p2(·)`. -/
noncomputable def priorBias (p : S1 → S2 → ℝ) (lam : S1 → ℝ) : S1 → S2 → ℝ :=
  fun s1 s2 => (1 - lam s1) * cond p s1 s2 + lam s1 * marg2 p s2

/-- The weight on the prior marginal in the compromise posterior,
`γ = α λ / (1 + α)` (p. 421, "`γ = αλ/(1+α)`"). -/
noncomputable def gam (α lam : S1 → ℝ) (s1 : S1) : ℝ := α s1 * lam s1 / (1 + α s1)

theorem priorBias_sum (p : S1 → S2 → ℝ) (lam : S1 → ℝ) {s1 : S1}
    (hp : marg1 p s1 ≠ 0) (ht : total p = 1) : ∑ s2, priorBias p lam s1 s2 = 1 := by
  unfold priorBias
  rw [Finset.sum_add_distrib, ← Finset.mul_sum, ← Finset.mul_sum, cond_sum p hp,
    sum_marg2, ht]
  ring

theorem priorBias_nonneg (p : S1 → S2 → ℝ) (lam : S1 → ℝ) {s1 : S1}
    (hnn : ∀ s1 s2, 0 ≤ p s1 s2) (hl0 : 0 ≤ lam s1) (hl1 : lam s1 ≤ 1) (s2 : S2) :
    0 ≤ priorBias p lam s1 s2 := by
  have hm : 0 ≤ marg1 p s1 := Finset.sum_nonneg (fun s2 _ => hnn s1 s2)
  have hc : 0 ≤ cond p s1 s2 := div_nonneg (hnn s1 s2) hm
  have h2 : 0 ≤ marg2 p s2 := Finset.sum_nonneg (fun s1 _ => hnn s1 s2)
  unfold priorBias
  have : 0 ≤ 1 - lam s1 := by linarith
  positivity

omit [Fintype S1] in
/-- **Corollary 3, proof, Case 1** (p. 433): solving (A.11)
`y_p p(·|s1) - y_q q(·|s1) + y_2 p2(·) = 0` with `y_p ≥ 0`, `y_q > 0` for
`q(·|s1)` gives (23) with `λ = y_2 / y_q ≤ 1`.  (Summing (A.11) over `S2` gives
`y_q = y_p + y_2`, so `1 - λ = y_p / y_q ≥ 0`.)  The Motzkin step that produces
(A.11) from the Prior-Bias axiom is not formalized. -/
theorem cor3_case1 (P Q P2 : S2 → ℝ) (hP : ∑ s, P s = 1) (hQ : ∑ s, Q s = 1)
    (hP2 : ∑ s, P2 s = 1) {yp yq y2 : ℝ} (hyp : 0 ≤ yp) (hyq : 0 < yq)
    (hA : ∀ s, yp * P s - yq * Q s + y2 * P2 s = 0) :
    ∃ l : ℝ, l ≤ 1 ∧ ∀ s, Q s = (1 - l) * P s + l * P2 s := by
  have hsum : yp - yq + y2 = 0 := by
    have h := Finset.sum_congr rfl (fun s (_ : s ∈ (Finset.univ : Finset S2)) => hA s)
    rw [Finset.sum_const_zero, Finset.sum_add_distrib, Finset.sum_sub_distrib,
      ← Finset.mul_sum, ← Finset.mul_sum, ← Finset.mul_sum, hP, hQ, hP2] at h
    linarith
  refine ⟨y2 / yq, ?_, fun s => ?_⟩
  · rw [div_le_one hyq]; linarith
  · have h := hA s
    field_simp
    linear_combination -h + P s * hsum

/-- The rewrite on p. 420: `q(s2|s1) = p(s2|s1) - λ(s1)(p(s2|s1) - p2(s2))`. -/
theorem overreaction_form (p : S1 → S2 → ℝ) (lam : S1 → ℝ) (s1 : S1) (s2 : S2) :
    priorBias p lam s1 s2 = cond p s1 s2 - lam s1 * (cond p s1 s2 - marg2 p s2) := by
  unfold priorBias; ring

/-- **Eq. (12)** (p. 420), with `α` and `λ` allowed to depend on `s1`:
under (23), `p*(·|s1) = (1 - γ) p(·|s1) + γ p2(·)` with `γ = αλ/(1+α)`. -/
theorem cond_pstar_priorBias (p : S1 → S2 → ℝ) (α lam : S1 → ℝ) {s1 : S1}
    (hp : marg1 p s1 ≠ 0) (ht : total p = 1) (ha : 0 ≤ α s1) (s2 : S2) :
    cond (pstar p (priorBias p lam) α) s1 s2
      = (1 - gam α lam s1) * cond p s1 s2 + gam α lam s1 * marg2 p s2 := by
  have ha' : (1 : ℝ) + α s1 ≠ 0 := by linarith
  rw [cond_pstar p _ α hp (priorBias_sum p lam hp ht) ha]
  unfold priorBias gam
  field_simp
  ring

/-- The deviation from Bayes under (23): `p*(·|s1) - p(·|s1) = γ (p2 - p(·|s1))`. -/
theorem pstar_deviation (p : S1 → S2 → ℝ) (α lam : S1 → ℝ) {s1 : S1}
    (hp : marg1 p s1 ≠ 0) (ht : total p = 1) (ha : 0 ≤ α s1) (s2 : S2) :
    cond (pstar p (priorBias p lam) α) s1 s2 - cond p s1 s2
      = gam α lam s1 * (marg2 p s2 - cond p s1 s2) := by
  rw [cond_pstar_priorBias p α lam hp ht ha]; ring

omit [Fintype S1] in
/-- With `λ ≤ 1` (Corollary 3) and `α ≥ 0` (Reg3), `γ < 1`: the compromise
posterior never puts full weight on the prior marginal. -/
theorem gam_lt_one (α lam : S1 → ℝ) {s1 : S1} (ha : 0 ≤ α s1) (hl : lam s1 ≤ 1) :
    gam α lam s1 < 1 := by
  unfold gam
  rw [div_lt_one (by linarith)]
  nlinarith

omit [Fintype S1] in
theorem gam_nonneg (α lam : S1 → ℝ) {s1 : S1} (ha : 0 ≤ α s1) (hl : 0 ≤ lam s1) :
    0 ≤ gam α lam s1 := by
  unfold gam; positivity

omit [Fintype S1] in
theorem gam_nonpos (α lam : S1 → ℝ) {s1 : S1} (ha : 0 ≤ α s1) (hl : lam s1 ≤ 0) :
    gam α lam s1 ≤ 0 := by
  unfold gam
  exact div_nonpos_of_nonpos_of_nonneg (mul_nonpos_of_nonneg_of_nonpos ha hl)
    (by linarith)

omit [Fintype S1] in
theorem gam_eq_zero_iff (α lam : S1 → ℝ) {s1 : S1} (ha : 0 ≤ α s1) :
    gam α lam s1 = 0 ↔ α s1 = 0 ∨ lam s1 = 0 := by
  unfold gam
  have : (1 : ℝ) + α s1 ≠ 0 := by linarith
  rw [div_eq_zero_iff, mul_eq_zero]
  constructor
  · rintro (h | h)
    · exact h
    · exact absurd h this
  · intro h; exact Or.inl h

/-- `λ(s1) = 0`: updating is Bayesian at `s1`. -/
theorem bayes_of_lam_zero (p : S1 → S2 → ℝ) (α lam : S1 → ℝ) {s1 : S1}
    (hp : marg1 p s1 ≠ 0) (ht : total p = 1) (ha : 0 ≤ α s1) (hl : lam s1 = 0)
    (s2 : S2) : cond (pstar p (priorBias p lam) α) s1 s2 = cond p s1 s2 := by
  have hg : gam α lam s1 = 0 := (gam_eq_zero_iff α lam ha).2 (Or.inr hl)
  rw [cond_pstar_priorBias p α lam hp ht ha, hg]; ring

/-- A product measure: `p(s1,s2) = a(s1) b(s2)`. -/
def IsProduct (p : S1 → S2 → ℝ) : Prop :=
  ∃ a : S1 → ℝ, ∃ b : S2 → ℝ, ∀ s1 s2, p s1 s2 = a s1 * b s2

/-- **Why a product measure gives Bayesian updating** (p. 429, "updating is
standard ... if `p` is a product measure"): every signal is *neutral*,
`p(·|s1) = p2(·)` (p. 428), i.e. `s1` carries no information about `S2`. -/
theorem cond_eq_marg2_of_product (p : S1 → S2 → ℝ) (hprod : IsProduct p)
    (ht : total p = 1) {s1 : S1} (hp : marg1 p s1 ≠ 0) (s2 : S2) :
    cond p s1 s2 = marg2 p s2 := by
  obtain ⟨a, b, hab⟩ := hprod
  have hm1 : marg1 p s1 = a s1 * ∑ t, b t := by
    unfold marg1; simp_rw [hab]; rw [Finset.mul_sum]
  have hm2 : marg2 p s2 = (∑ r, a r) * b s2 := by
    unfold marg2; simp_rw [hab]; rw [Finset.sum_mul]
  have htot : (∑ r, a r) * (∑ t, b t) = 1 := by
    rw [← ht]; unfold total; simp_rw [hab]
    rw [Finset.sum_mul]
    refine Finset.sum_congr rfl (fun r _ => ?_)
    rw [Finset.mul_sum]
  rw [hm1] at hp
  have ha : a s1 ≠ 0 := left_ne_zero_of_mul hp
  have hb : (∑ t, b t) ≠ 0 := right_ne_zero_of_mul hp
  have hA : (∑ r, a r) = (∑ t, b t)⁻¹ := eq_inv_of_mul_eq_one_left htot
  unfold cond
  rw [hm1, hm2, hab, mul_div_mul_left _ _ ha, hA, div_eq_inv_mul]

/-- Under (23) a neutral signal makes `q(·|s1) = p(·|s1)`, whatever `λ(s1)`. -/
theorem priorBias_of_neutral (p : S1 → S2 → ℝ) (lam : S1 → ℝ) {s1 : S1}
    (hn : ∀ s2, cond p s1 s2 = marg2 p s2) (s2 : S2) :
    priorBias p lam s1 s2 = cond p s1 s2 := by
  unfold priorBias; rw [hn]; ring

/-- Hence a product commitment prior gives Bayesian interim choice, for every
`α` and every `λ`. -/
theorem bayes_of_product (p : S1 → S2 → ℝ) (α lam : S1 → ℝ) (hprod : IsProduct p)
    (ht : total p = 1) {s1 : S1} (hp : marg1 p s1 ≠ 0) (ha : 0 ≤ α s1) (s2 : S2) :
    cond (pstar p (priorBias p lam) α) s1 s2 = cond p s1 s2 := by
  rw [cond_pstar_priorBias p α lam hp ht ha, cond_eq_marg2_of_product p hprod ht hp]
  ring

/-- `S1` a singleton (p. 429): the lone signal is neutral. -/
theorem cond_eq_marg2_of_subsingleton [Subsingleton S1] (p : S1 → S2 → ℝ)
    (ht : total p = 1) (s1 : S1) (s2 : S2) : cond p s1 s2 = marg2 p s2 := by
  have e2 : marg2 p s2 = p s1 s2 := by
    unfold marg2; exact Fintype.sum_subsingleton (fun r => p r s2) s1
  have e1 : marg1 p s1 = 1 := by
    rw [← ht]; unfold total marg1
    exact (Fintype.sum_subsingleton (fun r => ∑ t, p r t) s1).symm
  unfold cond; rw [e1, e2, div_one]

/-- Conversely, with `γ(s1) ≠ 0` (i.e. `α(s1) > 0` and `λ(s1) ≠ 0`), a
non-neutral signal moves interim beliefs off the Bayesian update. -/
theorem not_bayes_of_nonneutral (p : S1 → S2 → ℝ) (α lam : S1 → ℝ) {s1 : S1}
    (hp : marg1 p s1 ≠ 0) (ht : total p = 1) (ha : 0 ≤ α s1) {s2 : S2}
    (hg : gam α lam s1 ≠ 0) (hn : cond p s1 s2 ≠ marg2 p s2) :
    cond (pstar p (priorBias p lam) α) s1 s2 ≠ cond p s1 s2 := by
  intro h
  have := pstar_deviation p α lam hp ht ha s2
  rw [h, sub_self] at this
  rcases mul_eq_zero.1 this.symm with h1 | h1
  · exact hg h1
  · exact hn (sub_eq_zero.1 h1).symm

/-- **Reg2 under (23).**  `q(·|s1) ≪ p(·|s1)` together with (23) and
`λ(s1) ≠ 0` forces `p2(s2) = 0` wherever `p(s2|s1) = 0`: a Prior-Bias agent with
`λ(s1) ≠ 0` cannot receive a signal that rules out an a priori possible `s2`.
(Not stated in the paper; its examples satisfy it.) -/
theorem priorBias_absCont (p : S1 → S2 → ℝ) (lam : S1 → ℝ) {s1 : S1} {s2 : S2}
    (hl : lam s1 ≠ 0) (hc : cond p s1 s2 = 0)
    (hac : cond p s1 s2 = 0 → priorBias p lam s1 s2 = 0) : marg2 p s2 = 0 := by
  have h := hac hc
  unfold priorBias at h
  rw [hc, mul_zero, zero_add] at h
  rcases mul_eq_zero.1 h with h1 | h1
  · exact absurd h1 hl
  · exact h1

/-! ## Section 2.3: the examples -/

/-- **Under/overreaction** (p. 420): with the same `γ` at two signals, the
compromise posteriors differ by `(1-γ)` times the Bayesian ones: less sensitive
to the signal when `0 < γ < 1` (`λ > 0`), more when `γ < 0` (`λ < 0`). -/
theorem sensitivity_scaled (p : S1 → S2 → ℝ) (α lam : S1 → ℝ) {s1 s1' : S1}
    (hp : marg1 p s1 ≠ 0) (hp' : marg1 p s1' ≠ 0) (ht : total p = 1)
    (ha : 0 ≤ α s1) (ha' : 0 ≤ α s1') (hg : gam α lam s1 = gam α lam s1') (s2 : S2) :
    cond (pstar p (priorBias p lam) α) s1 s2 - cond (pstar p (priorBias p lam) α) s1' s2
      = (1 - gam α lam s1) * (cond p s1 s2 - cond p s1' s2) := by
  rw [cond_pstar_priorBias p α lam hp ht ha, cond_pstar_priorBias p α lam hp' ht ha', hg]
  ring

/-- **Confirmatory bias, eq. (13)** (p. 421): with `S1 = {a,b}`, `S2 = {A,B}` and
`p(a|A) = p(b|B) = θ`, `p1(b) - 1/2 = (2θ - 1)(p2(B) - 1/2)`; so for `θ > 1/2`,
`p2(B) > 1/2` iff `p1(b) > 1/2`. -/
theorem confirm_marginal (θ pB : ℝ) :
    ((1 - pB) * (1 - θ) + pB * θ) - 1 / 2 = (2 * θ - 1) * (pB - 1 / 2) := by ring

theorem confirm_iff {θ pB : ℝ} (hθ : 1 / 2 < θ) :
    1 / 2 < pB ↔ 1 / 2 < (1 - pB) * (1 - θ) + pB * θ := by
  have h := confirm_marginal θ pB
  have hθ' : 0 < 2 * θ - 1 := by linarith
  constructor
  · intro h1; nlinarith
  · intro h1
    by_contra h2
    have h3 := not_lt.1 h2
    nlinarith

omit [Fintype S1] in
/-- **Representativeness** (p. 421): if `q(A|a) = 1`, `p(A|a) < 1` and
`α(a) > 0`, then `p*(A|a) > p(A|a)`. -/
theorem representativeness (p qc : S1 → S2 → ℝ) (α : S1 → ℝ) {s1 : S1} {s2 : S2}
    (hp : marg1 p s1 ≠ 0) (hq : ∑ t, qc s1 t = 1) (ha : 0 < α s1)
    (hq1 : qc s1 s2 = 1) (hlt : cond p s1 s2 < 1) :
    cond p s1 s2 < cond (pstar p qc α) s1 s2 := by
  rw [cond_pstar p qc α hp hq ha.le, hq1, lt_div_iff₀ (by linarith)]
  nlinarith

/-- **Sample-Bias, fn 10** (p. 421): for `λ < 1`, the `s`-component of
`(1-λ) p(·|s) + λ δ_s` is nonnegative iff `p(s|s) ≥ -λ/(1-λ)`. -/
theorem sampleBias_nonneg_iff {l x : ℝ} (hl : l < 1) :
    0 ≤ (1 - l) * x + l ↔ -l / (1 - l) ≤ x := by
  rw [div_le_iff₀ (by linarith)]
  constructor <;> intro h <;> linarith

/-! ## Section 4: the law of iterated expectations -/

/-- The expected interim posterior under (23): the tower property of `p` plus a
correction `∑ p1(s1) γ(s1) (p2 - p(·|s1))`. -/
theorem pstar_average (p : S1 → S2 → ℝ) (α lam : S1 → ℝ)
    (hp : ∀ s1, marg1 p s1 ≠ 0) (ht : total p = 1) (ha : ∀ s1, 0 ≤ α s1) (s2 : S2) :
    ∑ s1, marg1 p s1 * cond (pstar p (priorBias p lam) α) s1 s2
      = marg2 p s2 + ∑ s1, marg1 p s1 * gam α lam s1 * (marg2 p s2 - cond p s1 s2) := by
  have key : ∀ s1, marg1 p s1 * cond (pstar p (priorBias p lam) α) s1 s2
      = marg1 p s1 * cond p s1 s2
        + marg1 p s1 * gam α lam s1 * (marg2 p s2 - cond p s1 s2) := by
    intro s1
    rw [cond_pstar_priorBias p α lam (hp s1) ht (ha s1)]
    ring
  rw [Finset.sum_congr rfl (fun s1 _ => key s1), Finset.sum_add_distrib, tower p hp s2]

/-- **With `γ` constant across signals the LIE holds**: the interim compromise
posteriors average back to the prior marginal `p2`. -/
theorem pstar_average_const (p : S1 → S2 → ℝ) (α lam : S1 → ℝ) (g : ℝ)
    (hp : ∀ s1, marg1 p s1 ≠ 0) (ht : total p = 1) (ha : ∀ s1, 0 ≤ α s1)
    (hg : ∀ s1, gam α lam s1 = g) (s2 : S2) :
    ∑ s1, marg1 p s1 * cond (pstar p (priorBias p lam) α) s1 s2 = marg2 p s2 := by
  rw [pstar_average p α lam hp ht ha s2]
  simp_rw [hg]
  have key : ∀ s1, marg1 p s1 * g * (marg2 p s2 - cond p s1 s2)
      = (g * marg2 p s2) * marg1 p s1 - g * (marg1 p s1 * cond p s1 s2) := by
    intro s1; ring
  rw [Finset.sum_congr rfl (fun s1 _ => key s1), Finset.sum_sub_distrib,
    ← Finset.mul_sum, ← Finset.mul_sum, tower p hp s2, sum_marg1, ht]
  ring

/-- **Prior-Bias rules out the concluding example.**  Under (23) with `λ ≤ 1`,
`α ≥ 0` and `p1` of full support, for every act (utility vector `v`) there is a
signal at which its interim value under `p*(·|s1)` is at least its time-0 value
under `p2`.  So `{f} ≻ {-f}` at `t = 0` (`E_{p2} v > 0`) can never be followed by
strict choice of `-f` from `{f,-f}` at *every* `s1`, which is the violation of
the "sure-thing principle for action rules" on p. 430. -/
theorem priorBias_no_uniform_reversal (p : S1 → S2 → ℝ) (α lam : S1 → ℝ)
    (hp : ∀ s1, 0 < marg1 p s1) (ht : total p = 1) (ha : ∀ s1, 0 ≤ α s1)
    (hl : ∀ s1, lam s1 ≤ 1) (v : S2 → ℝ) :
    ∃ s1, expect (marg2 p) v ≤ expect (cond (pstar p (priorBias p lam) α) s1) v := by
  have hp' : ∀ s1, marg1 p s1 ≠ 0 := fun s1 => (hp s1).ne'
  set c := expect (marg2 p) v with hc
  set e : S1 → ℝ := fun s1 => expect (cond p s1) v with he
  -- the tower property for `v`
  have htow : ∑ s1, marg1 p s1 * e s1 = c := by
    simp only [he, hc, expect, Finset.mul_sum]
    rw [Finset.sum_comm]
    refine Finset.sum_congr rfl (fun s2 _ => ?_)
    rw [← tower p hp' s2, Finset.sum_mul]
    refine Finset.sum_congr rfl (fun s1 _ => ?_)
    ring
  have hne : (Finset.univ : Finset S1).Nonempty := by
    by_contra h
    rw [Finset.not_nonempty_iff_eq_empty] at h
    have : total p = 0 := by unfold total; rw [h, Finset.sum_empty]
    rw [this] at ht; exact zero_ne_one ht
  have hle : ∑ s1, marg1 p s1 * c ≤ ∑ s1, marg1 p s1 * e s1 := by
    rw [htow, ← Finset.sum_mul, sum_marg1, ht, one_mul]
  obtain ⟨s1, -, hs1⟩ := Finset.exists_le_of_sum_le hne hle
  have hce : c ≤ e s1 := le_of_mul_le_mul_left hs1 (hp s1)
  refine ⟨s1, ?_⟩
  have hval : expect (cond (pstar p (priorBias p lam) α) s1) v
      = (1 - gam α lam s1) * e s1 + gam α lam s1 * c := by
    simp only [he, hc, expect, Finset.mul_sum, ← Finset.sum_add_distrib]
    refine Finset.sum_congr rfl (fun s2 _ => ?_)
    rw [cond_pstar_priorBias p α lam (hp' s1) ht (ha s1)]
    ring
  rw [hval]
  have hg := gam_lt_one α lam (ha s1) (hl s1)
  nlinarith

/-! ### The concluding remark (pp. 429-430), as an explicit example

`S1 = S2 = Fin 2` (`0 = a, 1 = b`; `0 = A, 1 = B`). -/

/-- The commitment prior `p = [[2/5, 1/10], [1/5, 3/10]]`: `p1 = (1/2, 1/2)`,
`p2 = (3/5, 2/5)`, `p(·|a) = (4/5, 1/5)`, `p(·|b) = (2/5, 3/5)`. -/
noncomputable def exP : Fin 2 → Fin 2 → ℝ := ![![2/5, 1/10], ![1/5, 3/10]]

/-- The temptation conditionals `q(·|s1) = δ_B` for both signals. -/
def exQ : Fin 2 → Fin 2 → ℝ := fun _ => ![0, 1]

/-- `α ≡ 1`. -/
def exAlpha : Fin 2 → ℝ := fun _ => 1

/-- The act `f` with utility `(1, -1)`. -/
def exF : Fin 2 → ℝ := ![1, -1]

theorem exP_marg1 (s1 : Fin 2) : marg1 exP s1 = 1/2 := by
  fin_cases s1 <;> simp [exP, marg1, Fin.sum_univ_two] <;> norm_num

theorem exP_cond (s1 s2 : Fin 2) :
    cond exP s1 s2 = ![![4/5, 1/5], ![2/5, 3/5]] s1 s2 := by
  unfold cond; rw [exP_marg1]
  fin_cases s1 <;> fin_cases s2 <;> simp [exP] <;> norm_num

theorem exQ_sum (s1 : Fin 2) : ∑ t, exQ s1 t = 1 := by
  simp [exQ, Fin.sum_univ_two]

/-- **The LIE violation of p. 430.**  `q ≪ p`; `{f} ≻ {-f}` at `t = 0`
(`E_{p2} f = 1/5 > 0`); at each `s1` the interim objective (8) strictly prefers
`-f` to `f`; equivalently `E_{p*(·|a)} f = -1/5 < 0` and `E_{p*(·|b)} f = -3/5 < 0`.
`q` is not of the form (23), as `priorBias_no_uniform_reversal` requires. -/
theorem lie_violation_example :
    (∀ s1 s2, cond exP s1 s2 = 0 → exQ s1 s2 = 0) ∧
    total exP = 1 ∧
    0 < expect (marg2 exP) exF ∧
    (∀ s1, expect (cond exP s1) exF + exAlpha s1 * expect (exQ s1) exF
        < expect (cond exP s1) (-exF) + exAlpha s1 * expect (exQ s1) (-exF)) ∧
    expect (cond (pstar exP exQ exAlpha) 0) exF = -1/5 ∧
    expect (cond (pstar exP exQ exAlpha) 1) exF = -3/5 := by
  have hm : ∀ s1, marg1 exP s1 ≠ 0 := fun s1 => by rw [exP_marg1]; norm_num
  have ha : ∀ s1, 0 ≤ exAlpha s1 := fun _ => zero_le_one
  have hs : ∀ s1 s2, cond (pstar exP exQ exAlpha) s1 s2
      = (cond exP s1 s2 + exAlpha s1 * exQ s1 s2) / (1 + exAlpha s1) :=
    fun s1 s2 => cond_pstar exP exQ exAlpha (hm s1) (exQ_sum s1) (ha s1) s2
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_⟩
  · intro s1 s2 h
    rw [exP_cond] at h
    fin_cases s1 <;> fin_cases s2 <;> simp at h
  · simp [total, exP, Fin.sum_univ_two]; norm_num
  · simp [expect, marg2, exP, exF, Fin.sum_univ_two]; norm_num
  · intro s1
    fin_cases s1 <;> simp [expect, exP_cond, exQ, exAlpha, exF, Fin.sum_univ_two] <;> norm_num
  · simp [expect, hs, exP_cond, exQ, exAlpha, exF, Fin.sum_univ_two]; norm_num
  · simp [expect, hs, exP_cond, exQ, exAlpha, exF, Fin.sum_univ_two]; norm_num

/-! ## The Paper B question: (23) and the project's partial-adoption step

Paper B's anchoring family (`JeffreyOrder/Anchoring.lean`) responds to a cue by
moving one marginal to the damped target
`JeffreyOrder.dampedTarget Q r₀ δ = (1-δ) * Q.mB1 + δ * (1 - r₀)`, i.e.
`(1-ω)·current + ω·delivered` with `current = Q.mB1`, `delivered = 1 - r₀`.
It is restated here on bare reals so this file stays standalone on Mathlib. -/

/-- The project's partial-adoption target on a single marginal. -/
def dampedTarget (current delivered ω : ℝ) : ℝ := (1 - ω) * current + ω * delivered

/-- The adoption weight of the posterior that governs interim choice. -/
noncomputable def omegaEff (α lam : S1 → ℝ) (s1 : S1) : ℝ := 1 - gam α lam s1

/-- **(23) is the damped target**, state by state, with `current = p2(s2)`,
`delivered = p(s2|s1)` and `ω = 1 - λ(s1)`. -/
theorem priorBias_eq_damped (p : S1 → S2 → ℝ) (lam : S1 → ℝ) (s1 : S1) (s2 : S2) :
    priorBias p lam s1 s2 = dampedTarget (marg2 p s2) (cond p s1 s2) (1 - lam s1) := by
  unfold priorBias dampedTarget; ring

/-- **(12) is the damped target** for the posterior that governs choice, with
`ω = 1 - α(s1)λ(s1)/(1+α(s1))`. -/
theorem compromise_eq_damped (p : S1 → S2 → ℝ) (α lam : S1 → ℝ) {s1 : S1}
    (hp : marg1 p s1 ≠ 0) (ht : total p = 1) (ha : 0 ≤ α s1) (s2 : S2) :
    cond (pstar p (priorBias p lam) α) s1 s2
      = dampedTarget (marg2 p s2) (cond p s1 s2) (omegaEff α lam s1) := by
  rw [cond_pstar_priorBias p α lam hp ht ha]
  unfold dampedTarget omegaEff; ring

omit [Fintype S1] in
theorem omegaEff_eq (α lam : S1 → ℝ) {s1 : S1} (ha : 0 ≤ α s1) :
    omegaEff α lam s1 = (1 + α s1 * (1 - lam s1)) / (1 + α s1) := by
  have : (1 : ℝ) + α s1 ≠ 0 := by linarith
  unfold omegaEff gam; field_simp; ring

omit [Fintype S1] in
/-- `ω = 0` (the cue ignored) is never attained by the choice posterior. -/
theorem omegaEff_pos (α lam : S1 → ℝ) {s1 : S1} (ha : 0 ≤ α s1) (hl : lam s1 ≤ 1) :
    0 < omegaEff α lam s1 := by
  unfold omegaEff; linarith [gam_lt_one α lam ha hl]

omit [Fintype S1] in
/-- Positive Prior-Bias (`0 < λ ≤ 1`) with `α > 0`: `1/(1+α) ≤ ω < 1`,
interior partial adoption. -/
theorem omegaEff_positive (α lam : S1 → ℝ) {s1 : S1} (ha : 0 < α s1)
    (hl0 : 0 < lam s1) (hl1 : lam s1 ≤ 1) :
    1 / (1 + α s1) ≤ omegaEff α lam s1 ∧ omegaEff α lam s1 < 1 := by
  have h1 : (0 : ℝ) < 1 + α s1 := by linarith
  rw [omegaEff_eq α lam ha.le]
  constructor
  · rw [div_le_div_iff_of_pos_right h1]; nlinarith
  · rw [div_lt_one h1]; nlinarith

omit [Fintype S1] in
/-- Negative Prior-Bias (`λ ≤ 0`): `ω ≥ 1`, overshooting the delivered belief,
outside Paper B's `ω ∈ [0,1]`. -/
theorem omegaEff_negative (α lam : S1 → ℝ) {s1 : S1} (ha : 0 ≤ α s1) (hl : lam s1 ≤ 0) :
    1 ≤ omegaEff α lam s1 := by
  unfold omegaEff; linarith [gam_nonpos α lam ha hl]

/-- Every `ω > 0` is some `(α ≥ 0, λ ≤ 1)`'s effective weight. -/
theorem omegaEff_surj {ω : ℝ} (hω : 0 < ω) :
    ∃ a l : ℝ, 0 ≤ a ∧ l ≤ 1 ∧ 1 - a * l / (1 + a) = ω := by
  rcases le_or_gt ω 1 with h | h
  · refine ⟨(1 - ω) / ω, 1, div_nonneg (by linarith) hω.le, le_rfl, ?_⟩
    field_simp; ring
  · refine ⟨1, 2 * (1 - ω), zero_le_one, by linarith, ?_⟩
    ring

/-- The deviation form of the damped target (the scalar content of
`JeffreyOrder.dampedB_deviation`): `target - delivered = (1-ω)(current - delivered)`. -/
theorem damped_deviation (current delivered ω : ℝ) :
    dampedTarget current delivered ω - delivered = (1 - ω) * (current - delivered) := by
  unfold dampedTarget; ring

/-- The weight is identified by the posterior whenever the signal is non-neutral
at `s2` (`current ≠ delivered`). -/
theorem omega_identified {current delivered ω ω' : ℝ} (hne : current ≠ delivered)
    (h : dampedTarget current delivered ω = dampedTarget current delivered ω') : ω = ω' := by
  unfold dampedTarget at h
  have : (ω - ω') * (current - delivered) = 0 := by linear_combination -h
  rcases mul_eq_zero.1 this with h1 | h1
  · linarith
  · exact absurd (sub_eq_zero.1 h1) hne

/-- **The damped Jeffrey step is a (23)-type mixture on the whole joint.**  For a
joint `Q` over `A × B` with `B`-marginal `QB`, the Jeffrey step on `B` to the damped
target `(1-ω) QB + ω d` equals `(1-ω) Q + ω J`, where `J` is the full Jeffrey
update to `d`.  So with "prior" = the current joint and "Bayesian update" = the
full Jeffrey update, the project's step has exactly the form of (23). -/
theorem dampedJeffrey_eq_mix {A B : Type*} (Q : A → B → ℝ) (QB d : B → ℝ) (ω : ℝ)
    (hQB : ∀ b, QB b ≠ 0) (a : A) (b : B) :
    Q a b / QB b * dampedTarget (QB b) (d b) ω
      = (1 - ω) * Q a b + ω * (Q a b / QB b * d b) := by
  have := hQB b
  unfold dampedTarget; field_simp

end Literature.Epstein
