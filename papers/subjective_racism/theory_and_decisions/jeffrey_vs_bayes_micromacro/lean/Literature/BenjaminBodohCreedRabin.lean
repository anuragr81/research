/-
# Benjamin, Bodoh-Creed & Rabin (2019), "Base-Rate Neglect: Foundations and Implications"

Working paper, July 19, 2019 (62 pp.).  Page numbers are the paper's printed
page numbers, which coincide with the PDF page numbers.

## The paper's model

* **One-shot rule** (p.2, and eq. (2) on p.10 with subjective likelihoods):
  `p_α(θ|s) = p(s|θ) p(θ)^α / ∑_θ' p(s|θ') p(θ')^α`, with `α ∈ [0,1)`.
  `α = 1` is Bayes ("Tommy"); `α < 1` is base-rate neglect ("Saki").
* **Dynamic assumption** (p.3, p.19: "the major modeling gambit of the
  paper"): "each time Saki receives a signal her updated beliefs become her
  priors when interpreting the next signal".
* **Closed form** (eq. (4), p.20, two signals; the display below it for `t`
  signals; eq. (5), p.21, conditionally i.i.d. signals):
  `p_α(θ|s₁..s_t)/p_α(θ̃|s₁..s_t)
     = (p(θ)/p(θ̃))^{α^t} ∏_{τ=1}^t (p(s_τ|θ)/p(s_τ|θ̃))^{α^{t-τ}}`.
* **Log form** (eq. (6), p.21):
  `L = ∑_τ α^{t-τ} l_τ + α^t l₀`.  The paper reads off: a long run of
  uninformative signals drives beliefs to uniform (p.21); "Saki's beliefs
  exhibit a recency bias" (p.21); the influence of a signal is "exponentially
  declining in the number of intervening signals.  This generates a recency
  effect" (p.3).
* **Non-convergence** (eq. (7), p.22): if `|l_τ| ≤ L̄` then the log odds are
  bounded by `L̄/(1-α)`.
* **Proposition 1** (extreme moderation, p.14; proof p.52): from eq. (17)
  `pα(θ|s)/pα(θ'|s) = [p(s|θ)/p(s|θ')] (p(θ)/p(θ'))^α`, the posterior odds fall
  below the prior odds iff the likelihood ratio is below `(p(θ)/p(θ'))^{1-α}`.

## What is formalized

Section 1 (the paper's own claims), on a finite hypothesis set `Θ`, with a
strictly positive prior and strictly positive likelihoods:

* `brn` (the one-shot rule) and `brnIter` (the dynamic rule, posterior becomes
  prior);
* `brnIter_closedForm`: after `n` signals the posterior is the normalisation of
  `p₀^{α^n} ∏_{k<n} ℓ_k^{α^{n-1-k}}` (0-indexed `k`, so signal `k` is followed
  by `n-1-k` others), i.e. eq. (5) in proportional form;
* `logOdds_brnIter`: eq. (6), the weight on signal `k` in the log odds of any
  two hypotheses is `α^{n-1-k}`, and the prior's weight is `α^n`;
* `ratio_brnIter`: eq. (5) in the paper's ratio form;
* `logOdds_uninformative`: the long-run moderation remark (p.21);
* `logOdds_bounded`: eq. (7), finite-horizon form;
* `twoSignal_logOdds_order`, `twoSignal_order_iff`, `recency_two_signals`:
  order dependence and recency for two signals, and `brn_order_invariant_iff`:
  on the whole posterior, reversing two signals changes nothing iff the two
  likelihood functions are proportional;
* `extreme_moderation_iff`: the one-step inequality behind Proposition 1.

Section 2 is **the Paper B question, not a claim of the paper**.  On the 2×2
joint `Θ = Fin 2 × Fin 2` (attribute A, attribute B), with one cue whose
likelihood depends on A only and one whose likelihood depends on B only:

* `assoc_brnAB`, `assoc_brnBA`, `assoc_brn_orders_eq`: the log odds ratio after
  two BRN steps is `α²` times the prior's, in either order;
  `assoc_bayes` and `assoc_brnAB_vs_bayes`: the Bayes benchmark `P^B` keeps the
  prior's log odds ratio, so BRN's believed association is `α² · assoc(P^B)`;
* `margOddsA_brnAB_indep`, `margOddsA_brnBA_indep`, `margOddsA_order_ratio`:
  at independence (`P(i,j) = u_i v_j`) the A-marginal odds are
  `(a₀/a₁)^α (u₀/u₁)^{α²}` when A is read first and `(a₀/a₁)(u₀/u₁)^{α²}` when
  A is read last, so they differ whenever `α ≠ 1` and `a₀ ≠ a₁`
  (`margOddsA_orders_ne`);
* `example_marginal_gap`: an explicit rational instance (`α = 1/2`, uniform
  prior, `a = b = (49/50, 1/50)`): the A-marginal is `7/8` when A is read first
  and `49/50` when A is read last.

## What is not formalized

Propositions 2-10 (hypothesis dependence, prediction momentum, learning traps,
persuasion, reputation, NBLLN), the infinite-past form of eq. (7), eq. (8), the
ergodicity argument, the prospective-belief "completion" (p.23), and the
Section 9 extensions (fortified signals, Peggy).  The paper's numerical
examples are checked in `literature/benjamin2019/sympy/check_brn.py`.
-/
import Mathlib

namespace Literature.BenjaminBodohCreedRabin

open Finset Real

/-! ## Section 1: the paper's model on a finite hypothesis set -/

section General

variable {Θ : Type*} [Fintype Θ]

/-- Normalisation of a nonnegative weight function. -/
noncomputable def normalize (w : Θ → ℝ) : Θ → ℝ := fun θ => w θ / ∑ θ', w θ'

/-- The unnormalised BRN weight `p(s|θ) p(θ)^α`. -/
noncomputable def brnWeight (α : ℝ) (ℓ p : Θ → ℝ) : Θ → ℝ := fun θ => ℓ θ * p θ ^ α

/-- **The paper's one-shot rule** (p.2):
`p_α(θ|s) = p(s|θ) p(θ)^α / ∑_θ' p(s|θ') p(θ')^α`. -/
noncomputable def brn (α : ℝ) (ℓ p : Θ → ℝ) : Θ → ℝ := normalize (brnWeight α ℓ p)

/-- **The dynamic rule** (p.19-20): the posterior after each signal is the prior
for the next.  `ℓ k` is the likelihood of the `k`-th signal (0-indexed). -/
noncomputable def brnIter (α : ℝ) (ℓ : ℕ → Θ → ℝ) (p₀ : Θ → ℝ) : ℕ → Θ → ℝ
  | 0 => p₀
  | n + 1 => brn α (ℓ n) (brnIter α ℓ p₀ n)

/-- The closed-form weight after `n` signals:
`p₀(θ)^{α^n} ∏_{k<n} ℓ_k(θ)^{α^{n-1-k}}`. -/
noncomputable def closedWeight (α : ℝ) (ℓ : ℕ → Θ → ℝ) (p₀ : Θ → ℝ) (n : ℕ) : Θ → ℝ :=
  fun θ => p₀ θ ^ (α ^ n) * ∏ k ∈ range n, ℓ k θ ^ (α ^ (n - 1 - k))

/-- Log odds of `θ` against `θ'`. -/
noncomputable def logOdds (p : Θ → ℝ) (θ θ' : Θ) : ℝ := Real.log (p θ / p θ')

lemma sum_pos_of_pos {w : Θ → ℝ} (hw : ∀ θ, 0 < w θ) (θ₀ : Θ) : 0 < ∑ θ, w θ :=
  Finset.sum_pos (fun θ _ => hw θ) ⟨θ₀, Finset.mem_univ _⟩

lemma normalize_pos {w : Θ → ℝ} (hw : ∀ θ, 0 < w θ) (θ : Θ) : 0 < normalize w θ :=
  div_pos (hw θ) (sum_pos_of_pos hw θ)

lemma normalize_sum {w : Θ → ℝ} (hw : ∀ θ, 0 < w θ) [Nonempty Θ] :
    ∑ θ, normalize w θ = 1 := by
  obtain ⟨θ₀⟩ := ‹Nonempty Θ›
  unfold normalize
  rw [← Finset.sum_div]
  exact div_self (sum_pos_of_pos hw θ₀).ne'

/-- Normalisation ignores a positive common factor. -/
lemma normalize_smul {w : Θ → ℝ} {c : ℝ} (hc : c ≠ 0) :
    normalize (fun θ => c * w θ) = normalize w := by
  funext θ
  unfold normalize
  rw [← Finset.mul_sum, mul_div_mul_left _ _ hc]

lemma normalize_ratio {w : Θ → ℝ} (hw : ∀ θ, 0 < w θ) (θ θ' : Θ) :
    normalize w θ / normalize w θ' = w θ / w θ' := by
  unfold normalize
  have hS := (sum_pos_of_pos hw θ).ne'
  field_simp

omit [Fintype Θ] in
lemma brnWeight_pos {α : ℝ} {ℓ p : Θ → ℝ} (hℓ : ∀ θ, 0 < ℓ θ) (hp : ∀ θ, 0 < p θ)
    (θ : Θ) : 0 < brnWeight α ℓ p θ :=
  mul_pos (hℓ θ) (Real.rpow_pos_of_pos (hp θ) α)

lemma brn_pos {α : ℝ} {ℓ p : Θ → ℝ} (hℓ : ∀ θ, 0 < ℓ θ) (hp : ∀ θ, 0 < p θ)
    (θ : Θ) : 0 < brn α ℓ p θ :=
  normalize_pos (brnWeight_pos hℓ hp) θ

/-- BRN ignores a positive rescaling of the prior: `(c p)^α = c^α p^α`. -/
lemma brn_smul {α c : ℝ} (hc : 0 < c) (ℓ : Θ → ℝ) {p : Θ → ℝ} (hp : ∀ θ, 0 ≤ p θ) :
    brn α ℓ (fun θ => c * p θ) = brn α ℓ p := by
  unfold brn brnWeight
  have : (fun θ => ℓ θ * (c * p θ) ^ α) = fun θ => c ^ α * (ℓ θ * p θ ^ α) := by
    funext θ
    rw [Real.mul_rpow hc.le (hp θ)]
    ring
  rw [this, normalize_smul (Real.rpow_pos_of_pos hc α).ne']

/-- BRN applied to a normalised prior equals BRN applied to the raw weights. -/
lemma brn_normalize {α : ℝ} (ℓ : Θ → ℝ) {w : Θ → ℝ} (hw : ∀ θ, 0 < w θ) [Nonempty Θ] :
    brn α ℓ (normalize w) = brn α ℓ w := by
  obtain ⟨θ₀⟩ := ‹Nonempty Θ›
  have hS := sum_pos_of_pos hw θ₀
  have : normalize w = fun θ => (∑ θ', w θ')⁻¹ * w θ := by
    funext θ; unfold normalize; rw [div_eq_inv_mul]
  rw [this, brn_smul (inv_pos.mpr hS) ℓ (fun θ => (hw θ).le)]

lemma brnIter_pos {α : ℝ} {ℓ : ℕ → Θ → ℝ} {p₀ : Θ → ℝ} (hℓ : ∀ k θ, 0 < ℓ k θ)
    (hp : ∀ θ, 0 < p₀ θ) : ∀ n θ, 0 < brnIter α ℓ p₀ n θ
  | 0, θ => hp θ
  | n + 1, θ => brn_pos (hℓ n) (brnIter_pos hℓ hp n) θ

omit [Fintype Θ] in
lemma closedWeight_pos {α : ℝ} {ℓ : ℕ → Θ → ℝ} {p₀ : Θ → ℝ} (hℓ : ∀ k θ, 0 < ℓ k θ)
    (hp : ∀ θ, 0 < p₀ θ) (n : ℕ) (θ : Θ) : 0 < closedWeight α ℓ p₀ n θ := by
  unfold closedWeight
  exact mul_pos (Real.rpow_pos_of_pos (hp θ) _)
    (Finset.prod_pos fun k _ => Real.rpow_pos_of_pos (hℓ k θ) _)

omit [Fintype Θ] in
/-- One more signal on the closed-form weight gives the next closed-form weight. -/
lemma brnWeight_closedWeight {α : ℝ} {ℓ : ℕ → Θ → ℝ} {p₀ : Θ → ℝ} (hℓ : ∀ k θ, 0 < ℓ k θ)
    (hp : ∀ θ, 0 < p₀ θ) (n : ℕ) :
    brnWeight α (ℓ n) (closedWeight α ℓ p₀ n) = closedWeight α ℓ p₀ (n + 1) := by
  funext θ
  unfold brnWeight closedWeight
  rw [Real.mul_rpow (Real.rpow_pos_of_pos (hp θ) _).le
      (Finset.prod_nonneg fun k _ => (Real.rpow_pos_of_pos (hℓ k θ) _).le),
    ← Real.rpow_mul (hp θ).le,
    ← Real.finsetProd_rpow _ _ (fun k _ => (Real.rpow_pos_of_pos (hℓ k θ) _).le),
    Finset.prod_range_succ]
  have hprod : ∏ k ∈ range n, (ℓ k θ ^ (α ^ (n - 1 - k))) ^ α
      = ∏ k ∈ range n, ℓ k θ ^ (α ^ (n + 1 - 1 - k)) := by
    refine Finset.prod_congr rfl fun k hk => ?_
    rw [← Real.rpow_mul (hℓ k θ).le]
    congr 1
    have hk' : k < n := Finset.mem_range.mp hk
    have : n + 1 - 1 - k = (n - 1 - k) + 1 := by omega
    rw [this, pow_succ]
  rw [hprod, show n + 1 - 1 - n = 0 by omega, pow_zero, Real.rpow_one, pow_succ]
  ring

/-- **Closed form, proportional version of eq. (5)** (p.21; eq. (4) on p.20 is
`n = 2`).  After `n` signals the BRN posterior is proportional to
`p₀^{α^n} ∏_{k<n} ℓ_k^{α^{n-1-k}}`. -/
theorem brnIter_closedForm [Nonempty Θ] {α : ℝ} {ℓ : ℕ → Θ → ℝ} {p₀ : Θ → ℝ}
    (hℓ : ∀ k θ, 0 < ℓ k θ) (hp : ∀ θ, 0 < p₀ θ) (hsum : ∑ θ, p₀ θ = 1) :
    ∀ n, brnIter α ℓ p₀ n = normalize (closedWeight α ℓ p₀ n)
  | 0 => by
      funext θ
      simp [brnIter, normalize, closedWeight, hsum]
  | n + 1 => by
      rw [brnIter, brnIter_closedForm hℓ hp hsum n,
        brn_normalize _ (closedWeight_pos hℓ hp n), brn,
        brnWeight_closedWeight hℓ hp n]

/-- One step of the rule in log odds:
`L' = log(ℓθ/ℓθ') + α L` (eq. (17), p.52, in logs). -/
theorem logOdds_brn {α : ℝ} {ℓ p : Θ → ℝ} (hℓ : ∀ θ, 0 < ℓ θ) (hp : ∀ θ, 0 < p θ)
    (θ θ' : Θ) :
    logOdds (brn α ℓ p) θ θ' = Real.log (ℓ θ / ℓ θ') + α * logOdds p θ θ' := by
  unfold logOdds brn
  rw [normalize_ratio (brnWeight_pos hℓ hp)]
  unfold brnWeight
  have h1 := hℓ θ; have h2 := hℓ θ'; have h3 := hp θ; have h4 := hp θ'
  rw [mul_div_mul_comm, ← Real.div_rpow h3.le h4.le,
    Real.log_mul (div_pos h1 h2).ne' (Real.rpow_pos_of_pos (div_pos h3 h4) α).ne',
    Real.log_rpow (div_pos h3 h4)]

/-- **Eq. (6)** (p.21).  After `n` signals, the log odds of `θ` against `θ'` are
`α^n l₀ + ∑_{k<n} α^{n-1-k} l_k`: the weight on signal `k` is `α^{n-1-k}`,
exponentially declining in the number `n-1-k` of later signals. -/
theorem logOdds_brnIter {α : ℝ} {ℓ : ℕ → Θ → ℝ} {p₀ : Θ → ℝ} (hℓ : ∀ k θ, 0 < ℓ k θ)
    (hp : ∀ θ, 0 < p₀ θ) (θ θ' : Θ) :
    ∀ n, logOdds (brnIter α ℓ p₀ n) θ θ'
      = α ^ n * logOdds p₀ θ θ'
        + ∑ k ∈ range n, α ^ (n - 1 - k) * Real.log (ℓ k θ / ℓ k θ')
  | 0 => by simp [brnIter]
  | n + 1 => by
      rw [brnIter, logOdds_brn (hℓ n) (brnIter_pos hℓ hp n), logOdds_brnIter hℓ hp θ θ' n,
        Finset.sum_range_succ, show n + 1 - 1 - n = 0 by omega, pow_zero, one_mul,
        mul_add, Finset.mul_sum]
      have : ∑ k ∈ range n, α * (α ^ (n - 1 - k) * Real.log (ℓ k θ / ℓ k θ'))
          = ∑ k ∈ range n, α ^ (n + 1 - 1 - k) * Real.log (ℓ k θ / ℓ k θ') := by
        refine Finset.sum_congr rfl fun k hk => ?_
        have hk' : k < n := Finset.mem_range.mp hk
        rw [show n + 1 - 1 - k = (n - 1 - k) + 1 by omega, pow_succ]
        ring
      rw [this, pow_succ]
      ring

/-- **Eq. (5)** (p.21) in the paper's ratio form:
`p_n(θ)/p_n(θ') = (p₀θ/p₀θ')^{α^n} ∏_{k<n} (ℓ_kθ/ℓ_kθ')^{α^{n-1-k}}`. -/
theorem ratio_brnIter {α : ℝ} {ℓ : ℕ → Θ → ℝ} {p₀ : Θ → ℝ} (hℓ : ∀ k θ, 0 < ℓ k θ)
    (hp : ∀ θ, 0 < p₀ θ) (θ θ' : Θ) (n : ℕ) :
    brnIter α ℓ p₀ n θ / brnIter α ℓ p₀ n θ'
      = (p₀ θ / p₀ θ') ^ (α ^ n) * ∏ k ∈ range n, (ℓ k θ / ℓ k θ') ^ (α ^ (n - 1 - k)) := by
  have hL := logOdds_brnIter (α := α) hℓ hp θ θ' n
  have hpos : 0 < brnIter α ℓ p₀ n θ / brnIter α ℓ p₀ n θ' :=
    div_pos (brnIter_pos hℓ hp n θ) (brnIter_pos hℓ hp n θ')
  have hr : ∀ k, 0 < ℓ k θ / ℓ k θ' := fun k => div_pos (hℓ k θ) (hℓ k θ')
  have h0 : 0 < p₀ θ / p₀ θ' := div_pos (hp θ) (hp θ')
  have hrhs : 0 < (p₀ θ / p₀ θ') ^ (α ^ n)
      * ∏ k ∈ range n, (ℓ k θ / ℓ k θ') ^ (α ^ (n - 1 - k)) :=
    mul_pos (Real.rpow_pos_of_pos h0 _)
      (Finset.prod_pos fun k _ => Real.rpow_pos_of_pos (hr k) _)
  apply Real.log_injOn_pos (Set.mem_Ioi.mpr hpos) (Set.mem_Ioi.mpr hrhs)
  unfold logOdds at hL
  rw [hL, Real.log_mul (Real.rpow_pos_of_pos h0 _).ne'
      (Finset.prod_pos fun k _ => Real.rpow_pos_of_pos (hr k) _).ne',
    Real.log_rpow h0, Real.log_prod (fun k _ => (Real.rpow_pos_of_pos (hr k) _).ne')]
  congr 1
  exact Finset.sum_congr rfl fun k _ => (Real.log_rpow (hr k) _).symm

/-- **Long-run moderation** (p.21): if every signal is uninformative between
`θ` and `θ'`, the log odds are `α^n l₀`, which tends to `0` for `α < 1`. -/
theorem logOdds_uninformative {α : ℝ} {ℓ : ℕ → Θ → ℝ} {p₀ : Θ → ℝ}
    (hℓ : ∀ k θ, 0 < ℓ k θ) (hp : ∀ θ, 0 < p₀ θ) (θ θ' : Θ)
    (hunin : ∀ k, ℓ k θ = ℓ k θ') (n : ℕ) :
    logOdds (brnIter α ℓ p₀ n) θ θ' = α ^ n * logOdds p₀ θ θ' := by
  rw [logOdds_brnIter hℓ hp θ θ' n]
  have : ∀ k ∈ range n, α ^ (n - 1 - k) * Real.log (ℓ k θ / ℓ k θ') = 0 := by
    intro k _
    rw [hunin k, div_self (hℓ k θ').ne', Real.log_one, mul_zero]
  rw [Finset.sum_eq_zero this, add_zero]

/-- **Eq. (7)** (p.22), finite-horizon form.  If every signal's log likelihood
ratio is at most `L̄` in absolute value, then the part of the log odds due to
signals is at most `L̄/(1-α)` in absolute value, however many signals arrive. -/
theorem logOdds_bounded {α L : ℝ} (hα0 : 0 ≤ α) (hα1 : α < 1) {ℓ : ℕ → Θ → ℝ}
    {p₀ : Θ → ℝ} (hℓ : ∀ k θ, 0 < ℓ k θ) (hp : ∀ θ, 0 < p₀ θ) (θ θ' : Θ)
    (hL : ∀ k, |Real.log (ℓ k θ / ℓ k θ')| ≤ L) (n : ℕ) :
    |logOdds (brnIter α ℓ p₀ n) θ θ' - α ^ n * logOdds p₀ θ θ'| ≤ L / (1 - α) := by
  rw [logOdds_brnIter hℓ hp θ θ' n, add_sub_cancel_left]
  have hgeom : ∑ k ∈ range n, α ^ (n - 1 - k) = ∑ j ∈ range n, α ^ j :=
    Finset.sum_range_reflect (fun j => α ^ j) n
  have hL0 : 0 ≤ L := le_trans (abs_nonneg _) (hL 0)
  have h1α : 0 < 1 - α := by linarith
  have hsum_le : ∑ j ∈ range n, α ^ j ≤ 1 / (1 - α) := by
    rw [le_div_iff₀ h1α, mul_comm, mul_neg_geom_sum]
    linarith [pow_nonneg hα0 n]
  calc |∑ k ∈ range n, α ^ (n - 1 - k) * Real.log (ℓ k θ / ℓ k θ')|
      ≤ ∑ k ∈ range n, |α ^ (n - 1 - k) * Real.log (ℓ k θ / ℓ k θ')| :=
        Finset.abs_sum_le_sum_abs _ _
    _ ≤ ∑ k ∈ range n, α ^ (n - 1 - k) * L := by
        refine Finset.sum_le_sum fun k _ => ?_
        rw [abs_mul, abs_of_nonneg (pow_nonneg hα0 _)]
        exact mul_le_mul_of_nonneg_left (hL k) (pow_nonneg hα0 _)
    _ = L * ∑ j ∈ range n, α ^ j := by rw [← Finset.sum_mul, hgeom, mul_comm]
    _ ≤ L * (1 / (1 - α)) := mul_le_mul_of_nonneg_left hsum_le hL0
    _ = L / (1 - α) := by ring

/-! ### Order dependence and recency for two signals -/

/-- Two signals, in the order `ℓ₁` then `ℓ₂`. -/
noncomputable def brn2 (α : ℝ) (ℓ₁ ℓ₂ p₀ : Θ → ℝ) : Θ → ℝ := brn α ℓ₂ (brn α ℓ₁ p₀)

/-- Eq. (4) (p.20) with conditionally independent signals, in log odds:
`L = α² l₀ + α l₁ + l₂`. -/
theorem logOdds_brn2 {α : ℝ} {ℓ₁ ℓ₂ p₀ : Θ → ℝ} (h₁ : ∀ θ, 0 < ℓ₁ θ) (h₂ : ∀ θ, 0 < ℓ₂ θ)
    (hp : ∀ θ, 0 < p₀ θ) (θ θ' : Θ) :
    logOdds (brn2 α ℓ₁ ℓ₂ p₀) θ θ'
      = α ^ 2 * logOdds p₀ θ θ' + α * Real.log (ℓ₁ θ / ℓ₁ θ') + Real.log (ℓ₂ θ / ℓ₂ θ') := by
  unfold brn2
  rw [logOdds_brn h₂ (brn_pos h₁ hp), logOdds_brn h₁ hp]
  ring

/-- **Order effect for two signals.**  Reading `ℓ₁` then `ℓ₂` rather than `ℓ₂`
then `ℓ₁` shifts the log odds by `(1-α)(l₂ - l₁)`. -/
theorem twoSignal_logOdds_order {α : ℝ} {ℓ₁ ℓ₂ p₀ : Θ → ℝ} (h₁ : ∀ θ, 0 < ℓ₁ θ)
    (h₂ : ∀ θ, 0 < ℓ₂ θ) (hp : ∀ θ, 0 < p₀ θ) (θ θ' : Θ) :
    logOdds (brn2 α ℓ₁ ℓ₂ p₀) θ θ' - logOdds (brn2 α ℓ₂ ℓ₁ p₀) θ θ'
      = (1 - α) * (Real.log (ℓ₂ θ / ℓ₂ θ') - Real.log (ℓ₁ θ / ℓ₁ θ')) := by
  rw [logOdds_brn2 h₁ h₂ hp, logOdds_brn2 h₂ h₁ hp]
  ring

/-- For `α ≠ 1` the two orders give the same odds of `θ` against `θ'` iff the
two signals have the same likelihood ratio for that pair. -/
theorem twoSignal_order_iff {α : ℝ} (hα : α ≠ 1) {ℓ₁ ℓ₂ p₀ : Θ → ℝ} (h₁ : ∀ θ, 0 < ℓ₁ θ)
    (h₂ : ∀ θ, 0 < ℓ₂ θ) (hp : ∀ θ, 0 < p₀ θ) (θ θ' : Θ) :
    logOdds (brn2 α ℓ₁ ℓ₂ p₀) θ θ' = logOdds (brn2 α ℓ₂ ℓ₁ p₀) θ θ'
      ↔ ℓ₁ θ / ℓ₁ θ' = ℓ₂ θ / ℓ₂ θ' := by
  rw [← sub_eq_zero, twoSignal_logOdds_order h₁ h₂ hp]
  have h1α : (1 - α) ≠ 0 := sub_ne_zero.mpr (Ne.symm hα)
  rw [mul_eq_zero, or_iff_right h1α, sub_eq_zero]
  constructor
  · intro h
    exact (Real.log_injOn_pos (Set.mem_Ioi.mpr (div_pos (h₂ θ) (h₂ θ')))
      (Set.mem_Ioi.mpr (div_pos (h₁ θ) (h₁ θ'))) h).symm
  · intro h; rw [h]

/-- **Recency** (p.21: "she draws stronger inferences from signals observed
recently").  For `0 ≤ α < 1`, if `ℓ₂` favours `θ` over `θ'` more than `ℓ₁`
does, the odds on `θ` are higher when `ℓ₂` comes last. -/
theorem recency_two_signals {α : ℝ} (hα : α < 1) {ℓ₁ ℓ₂ p₀ : Θ → ℝ} (h₁ : ∀ θ, 0 < ℓ₁ θ)
    (h₂ : ∀ θ, 0 < ℓ₂ θ) (hp : ∀ θ, 0 < p₀ θ) (θ θ' : Θ)
    (hstronger : ℓ₁ θ / ℓ₁ θ' < ℓ₂ θ / ℓ₂ θ') :
    logOdds (brn2 α ℓ₂ ℓ₁ p₀) θ θ' < logOdds (brn2 α ℓ₁ ℓ₂ p₀) θ θ' := by
  have hd := twoSignal_logOdds_order (α := α) h₁ h₂ hp θ θ'
  have hlog : Real.log (ℓ₁ θ / ℓ₁ θ') < Real.log (ℓ₂ θ / ℓ₂ θ') :=
    Real.log_lt_log (div_pos (h₁ θ) (h₁ θ')) hstronger
  have : 0 < (1 - α) * (Real.log (ℓ₂ θ / ℓ₂ θ') - Real.log (ℓ₁ θ / ℓ₁ θ')) :=
    mul_pos (by linarith) (by linarith)
  linarith

/-- **Order invariance on the whole posterior.**  For `α ≠ 1`, the two orders
give the same posterior iff the two likelihood functions are proportional
(the same likelihood ratio for every pair of hypotheses). -/
theorem brn_order_invariant_iff {α : ℝ} (hα : α ≠ 1) {ℓ₁ ℓ₂ p₀ : Θ → ℝ}
    (h₁ : ∀ θ, 0 < ℓ₁ θ) (h₂ : ∀ θ, 0 < ℓ₂ θ) (hp : ∀ θ, 0 < p₀ θ) :
    brn2 α ℓ₁ ℓ₂ p₀ = brn2 α ℓ₂ ℓ₁ p₀ ↔ ∀ θ θ', ℓ₁ θ / ℓ₁ θ' = ℓ₂ θ / ℓ₂ θ' := by
  constructor
  · intro h θ θ'
    exact (twoSignal_order_iff hα h₁ h₂ hp θ θ').mp (by rw [h])
  · intro h
    -- both posteriors are normalisations of positive weights with equal ratios
    have hpos12 : ∀ θ, 0 < brn2 α ℓ₁ ℓ₂ p₀ θ := brn_pos h₂ (brn_pos h₁ hp)
    have hpos21 : ∀ θ, 0 < brn2 α ℓ₂ ℓ₁ p₀ θ := brn_pos h₁ (brn_pos h₂ hp)
    have hratio : ∀ θ θ', brn2 α ℓ₁ ℓ₂ p₀ θ / brn2 α ℓ₁ ℓ₂ p₀ θ'
        = brn2 α ℓ₂ ℓ₁ p₀ θ / brn2 α ℓ₂ ℓ₁ p₀ θ' := by
      intro θ θ'
      have hl := (twoSignal_order_iff hα h₁ h₂ hp θ θ').mpr (h θ θ')
      unfold logOdds at hl
      exact Real.log_injOn_pos (Set.mem_Ioi.mpr (div_pos (hpos12 θ) (hpos12 θ')))
        (Set.mem_Ioi.mpr (div_pos (hpos21 θ) (hpos21 θ'))) hl
    -- both sum to one
    have hne : Nonempty Θ ∨ IsEmpty Θ := (isEmpty_or_nonempty Θ).symm
    rcases hne with hne | hne
    · have s12 : ∑ θ, brn2 α ℓ₁ ℓ₂ p₀ θ = 1 := normalize_sum (brnWeight_pos h₂ (brn_pos h₁ hp))
      have s21 : ∑ θ, brn2 α ℓ₂ ℓ₁ p₀ θ = 1 := normalize_sum (brnWeight_pos h₁ (brn_pos h₂ hp))
      funext θ
      -- `q θ' = q θ * (p θ' / p θ)` for both, then sum
      have key : ∀ θ', brn2 α ℓ₂ ℓ₁ p₀ θ'
          = brn2 α ℓ₂ ℓ₁ p₀ θ / brn2 α ℓ₁ ℓ₂ p₀ θ * brn2 α ℓ₁ ℓ₂ p₀ θ' := by
        intro θ'
        have := hratio θ' θ
        rw [div_eq_div_iff (hpos12 θ).ne' (hpos21 θ).ne'] at this
        rw [div_mul_eq_mul_div, eq_div_iff (hpos12 θ).ne']
        linear_combination -this
      have hsum : ∑ θ', brn2 α ℓ₂ ℓ₁ p₀ θ'
          = brn2 α ℓ₂ ℓ₁ p₀ θ / brn2 α ℓ₁ ℓ₂ p₀ θ * ∑ θ', brn2 α ℓ₁ ℓ₂ p₀ θ' := by
        rw [Finset.mul_sum]; exact Finset.sum_congr rfl fun θ' _ => key θ'
      rw [s21, s12, mul_one, eq_div_iff (hpos12 θ).ne', one_mul] at hsum
      exact hsum
    · funext θ; exact hne.elim θ

/-! ### Proposition 1 (extreme moderation), one step -/

/-- **Core of Proposition 1** (p.14; proof p.52).  With prior odds `O > 0` and
likelihood ratio `z > 0`, the BRN posterior odds `z O^α` fall below the prior
odds `O` iff `z < O^{1-α}`.  Part (1) takes `O > z^{1/(1-α)}`; part (2) takes
`z < O^{1-α}` directly. -/
theorem extreme_moderation_iff {α O z : ℝ} (hO : 0 < O) :
    z * O ^ α < O ↔ z < O ^ (1 - α) := by
  have hOα : 0 < O ^ α := Real.rpow_pos_of_pos hO α
  have hsplit : O = O ^ (1 - α) * O ^ α := by
    rw [← Real.rpow_add hO, sub_add_cancel, Real.rpow_one]
  constructor
  · intro h
    have : z * O ^ α < O ^ (1 - α) * O ^ α := by rw [← hsplit]; exact h
    exact lt_of_mul_lt_mul_right this hOα.le
  · intro h
    calc z * O ^ α < O ^ (1 - α) * O ^ α := mul_lt_mul_of_pos_right h hOα
      _ = O := hsplit.symm

end General

/-! ## Section 2: the Paper B question (not a claim of the paper)

The 2×2 joint `Θ = Fin 2 × Fin 2`, `θ = (i, j)` with `i` the value of attribute
A and `j` the value of attribute B.  A cue on A has a likelihood `a i` that
depends on `i` only; a cue on B has a likelihood `b j` that depends on `j`
only.  BRN is applied with the four cells as the hypotheses. -/

section PaperB

/-- Likelihood of the cue on attribute A. -/
def cueA (a : Fin 2 → ℝ) : Fin 2 × Fin 2 → ℝ := fun θ => a θ.1

/-- Likelihood of the cue on attribute B. -/
def cueB (b : Fin 2 → ℝ) : Fin 2 × Fin 2 → ℝ := fun θ => b θ.2

/-- BRN, A-cue first then B-cue. -/
noncomputable def brnAB (α : ℝ) (a b : Fin 2 → ℝ) (P : Fin 2 × Fin 2 → ℝ) :
    Fin 2 × Fin 2 → ℝ := brn2 α (cueA a) (cueB b) P

/-- BRN, B-cue first then A-cue. -/
noncomputable def brnBA (α : ℝ) (a b : Fin 2 → ℝ) (P : Fin 2 × Fin 2 → ℝ) :
    Fin 2 × Fin 2 → ℝ := brn2 α (cueB b) (cueA a) P

/-- The Bayes-factor benchmark `P^B(i,j) ∝ P(i,j) a_i b_j`. -/
noncomputable def bayesPB (a b : Fin 2 → ℝ) (P : Fin 2 × Fin 2 → ℝ) : Fin 2 × Fin 2 → ℝ :=
  normalize fun θ => a θ.1 * b θ.2 * P θ

/-- Believed association: the log odds ratio
`log P(0,0) + log P(1,1) - log P(0,1) - log P(1,0)`. -/
noncomputable def assoc (P : Fin 2 × Fin 2 → ℝ) : ℝ :=
  Real.log (P (0, 0)) + Real.log (P (1, 1)) - Real.log (P (0, 1)) - Real.log (P (1, 0))

/-- The A-marginal. -/
def margA (P : Fin 2 × Fin 2 → ℝ) (i : Fin 2) : ℝ := P (i, 0) + P (i, 1)

/-- Normalisation leaves the log odds ratio unchanged. -/
lemma assoc_normalize {w : Fin 2 × Fin 2 → ℝ} (hw : ∀ θ, 0 < w θ) :
    assoc (normalize w) = assoc w := by
  have hS := sum_pos_of_pos hw (0, 0)
  unfold assoc normalize
  rw [Real.log_div (hw _).ne' hS.ne', Real.log_div (hw _).ne' hS.ne',
    Real.log_div (hw _).ne' hS.ne', Real.log_div (hw _).ne' hS.ne']
  ring

/-- A likelihood of product form `f i * g j` scales the log odds ratio by `α`
under one BRN step. -/
theorem assoc_brn_product {α : ℝ} {f g : Fin 2 → ℝ} (hf : ∀ i, 0 < f i) (hg : ∀ j, 0 < g j)
    {P : Fin 2 × Fin 2 → ℝ} (hP : ∀ θ, 0 < P θ) :
    assoc (brn α (fun θ => f θ.1 * g θ.2) P) = α * assoc P := by
  have hℓ : ∀ θ : Fin 2 × Fin 2, 0 < f θ.1 * g θ.2 := fun θ => mul_pos (hf _) (hg _)
  unfold brn
  rw [assoc_normalize (brnWeight_pos hℓ hP)]
  unfold assoc brnWeight
  simp only
  rw [Real.log_mul (hℓ (0, 0)).ne' (Real.rpow_pos_of_pos (hP _) _).ne',
    Real.log_mul (hℓ (1, 1)).ne' (Real.rpow_pos_of_pos (hP _) _).ne',
    Real.log_mul (hℓ (0, 1)).ne' (Real.rpow_pos_of_pos (hP _) _).ne',
    Real.log_mul (hℓ (1, 0)).ne' (Real.rpow_pos_of_pos (hP _) _).ne',
    Real.log_mul (hf _).ne' (hg _).ne', Real.log_mul (hf _).ne' (hg _).ne',
    Real.log_mul (hf _).ne' (hg _).ne', Real.log_mul (hf _).ne' (hg _).ne',
    Real.log_rpow (hP _), Real.log_rpow (hP _), Real.log_rpow (hP _), Real.log_rpow (hP _)]
  ring

lemma cueA_eq (a : Fin 2 → ℝ) : cueA a = fun θ => a θ.1 * (fun _ => (1 : ℝ)) θ.2 := by
  funext θ; simp [cueA]

lemma cueB_eq (b : Fin 2 → ℝ) : cueB b = fun θ => (fun _ => (1 : ℝ)) θ.1 * b θ.2 := by
  funext θ; simp [cueB]

/-- **Paper B (i), order A then B.**  The log odds ratio after two BRN steps is
`α²` times the prior's. -/
theorem assoc_brnAB {α : ℝ} {a b : Fin 2 → ℝ} (ha : ∀ i, 0 < a i) (hb : ∀ j, 0 < b j)
    {P : Fin 2 × Fin 2 → ℝ} (hP : ∀ θ, 0 < P θ) :
    assoc (brnAB α a b P) = α ^ 2 * assoc P := by
  have hA : ∀ θ, 0 < cueA a θ := fun θ => ha _
  unfold brnAB brn2
  rw [cueB_eq, assoc_brn_product (fun _ => one_pos) hb (brn_pos hA hP), cueA_eq,
    assoc_brn_product ha (fun _ => one_pos) hP]
  ring

/-- **Paper B (i), order B then A.** -/
theorem assoc_brnBA {α : ℝ} {a b : Fin 2 → ℝ} (ha : ∀ i, 0 < a i) (hb : ∀ j, 0 < b j)
    {P : Fin 2 × Fin 2 → ℝ} (hP : ∀ θ, 0 < P θ) :
    assoc (brnBA α a b P) = α ^ 2 * assoc P := by
  have hB : ∀ θ, 0 < cueB b θ := fun θ => hb _
  unfold brnBA brn2
  rw [cueA_eq, assoc_brn_product ha (fun _ => one_pos) (brn_pos hB hP), cueB_eq,
    assoc_brn_product (fun _ => one_pos) hb hP]
  ring

/-- **Paper B (i).**  The believed association is identical across the two
orders, at every prior (every `c`). -/
theorem assoc_brn_orders_eq {α : ℝ} {a b : Fin 2 → ℝ} (ha : ∀ i, 0 < a i)
    (hb : ∀ j, 0 < b j) {P : Fin 2 × Fin 2 → ℝ} (hP : ∀ θ, 0 < P θ) :
    assoc (brnAB α a b P) = assoc (brnBA α a b P) := by
  rw [assoc_brnAB ha hb hP, assoc_brnBA ha hb hP]

/-- The Bayes benchmark keeps the prior's log odds ratio. -/
theorem assoc_bayes {a b : Fin 2 → ℝ} (ha : ∀ i, 0 < a i) (hb : ∀ j, 0 < b j)
    {P : Fin 2 × Fin 2 → ℝ} (hP : ∀ θ, 0 < P θ) :
    assoc (bayesPB a b P) = assoc P := by
  have h := assoc_brn_product (α := 1) ha hb hP
  rw [one_mul] at h
  rw [← h]
  unfold bayesPB brn brnWeight
  congr 2
  funext θ
  rw [Real.rpow_one]

/-- **Paper B (i), against the benchmark.**  BRN's believed association is
`α²` times the Bayes benchmark's, in either order. -/
theorem assoc_brnAB_vs_bayes {α : ℝ} {a b : Fin 2 → ℝ} (ha : ∀ i, 0 < a i)
    (hb : ∀ j, 0 < b j) {P : Fin 2 × Fin 2 → ℝ} (hP : ∀ θ, 0 < P θ) :
    assoc (brnAB α a b P) = α ^ 2 * assoc (bayesPB a b P)
      ∧ assoc (brnBA α a b P) = α ^ 2 * assoc (bayesPB a b P) := by
  rw [assoc_bayes ha hb hP]
  exact ⟨assoc_brnAB ha hb hP, assoc_brnBA ha hb hP⟩

/-! ### The A-marginal at independence -/

/-- The A-marginal odds of the normalisation of a product weight `X i * Y j`
are `X 0 / X 1`. -/
lemma margA_ratio_product {X Y : Fin 2 → ℝ} (hX : ∀ i, 0 < X i) (hY : ∀ j, 0 < Y j) :
    margA (normalize fun θ => X θ.1 * Y θ.2) 0 / margA (normalize fun θ => X θ.1 * Y θ.2) 1
      = X 0 / X 1 := by
  have hS := sum_pos_of_pos (fun θ : Fin 2 × Fin 2 => mul_pos (hX θ.1) (hY θ.2)) (0, 0)
  unfold margA normalize
  simp only
  have h0 := hX 0; have h1 := hX 1; have hy0 := hY 0; have hy1 := hY 1
  have hy : 0 < Y 0 + Y 1 := by linarith
  field_simp

/-- Two BRN steps from a positive prior equal the normalisation of
`ℓ₂ ℓ₁^α P^{α²}` (eq. (4), p.20). -/
lemma brn2_eq {α : ℝ} {Θ : Type*} [Fintype Θ] [Nonempty Θ] {ℓ₁ ℓ₂ P : Θ → ℝ}
    (h₁ : ∀ θ, 0 < ℓ₁ θ) (hP : ∀ θ, 0 < P θ) :
    brn2 α ℓ₁ ℓ₂ P = normalize fun θ => ℓ₂ θ * ℓ₁ θ ^ α * P θ ^ (α ^ 2) := by
  unfold brn2
  rw [show brn α ℓ₁ P = normalize (brnWeight α ℓ₁ P) from rfl,
    brn_normalize _ (brnWeight_pos h₁ hP)]
  unfold brn brnWeight
  congr 1
  funext θ
  rw [Real.mul_rpow (h₁ θ).le (Real.rpow_pos_of_pos (hP θ) _).le,
    ← Real.rpow_mul (hP θ).le, sq]
  ring

/-- **Paper B (ii), A read first.**  At independence, `P(i,j) = u_i v_j`, the
A-marginal odds after A then B are `(a₀/a₁)^α (u₀/u₁)^{α²}`. -/
theorem margOddsA_brnAB_indep {α : ℝ} {a b u v : Fin 2 → ℝ} (ha : ∀ i, 0 < a i)
    (hb : ∀ j, 0 < b j) (hu : ∀ i, 0 < u i) (hv : ∀ j, 0 < v j) :
    margA (brnAB α a b fun θ => u θ.1 * v θ.2) 0 / margA (brnAB α a b fun θ => u θ.1 * v θ.2) 1
      = (a 0 / a 1) ^ α * (u 0 / u 1) ^ (α ^ 2) := by
  have hP : ∀ θ : Fin 2 × Fin 2, 0 < u θ.1 * v θ.2 := fun θ => mul_pos (hu _) (hv _)
  have hA : ∀ θ, 0 < cueA a θ := fun θ => ha θ.1
  unfold brnAB
  rw [brn2_eq hA hP]
  have hX : ∀ i, 0 < a i ^ α * u i ^ (α ^ 2) := fun i =>
    mul_pos (Real.rpow_pos_of_pos (ha i) _) (Real.rpow_pos_of_pos (hu i) _)
  have hY : ∀ j, 0 < b j * v j ^ (α ^ 2) := fun j =>
    mul_pos (hb j) (Real.rpow_pos_of_pos (hv j) _)
  have hw : (fun θ : Fin 2 × Fin 2 => cueB b θ * cueA a θ ^ α * (u θ.1 * v θ.2) ^ (α ^ 2))
      = fun θ => (a θ.1 ^ α * u θ.1 ^ (α ^ 2)) * (b θ.2 * v θ.2 ^ (α ^ 2)) := by
    funext θ
    simp only [cueA, cueB]
    rw [Real.mul_rpow (hu _).le (hv _).le]
    ring
  rw [hw, margA_ratio_product hX hY, Real.div_rpow (ha 0).le (ha 1).le,
    Real.div_rpow (hu 0).le (hu 1).le]
  field_simp

/-- **Paper B (ii), A read last.**  At independence the A-marginal odds after
B then A are `(a₀/a₁)(u₀/u₁)^{α²}`. -/
theorem margOddsA_brnBA_indep {α : ℝ} {a b u v : Fin 2 → ℝ} (ha : ∀ i, 0 < a i)
    (hb : ∀ j, 0 < b j) (hu : ∀ i, 0 < u i) (hv : ∀ j, 0 < v j) :
    margA (brnBA α a b fun θ => u θ.1 * v θ.2) 0 / margA (brnBA α a b fun θ => u θ.1 * v θ.2) 1
      = (a 0 / a 1) * (u 0 / u 1) ^ (α ^ 2) := by
  have hP : ∀ θ : Fin 2 × Fin 2, 0 < u θ.1 * v θ.2 := fun θ => mul_pos (hu _) (hv _)
  have hB : ∀ θ, 0 < cueB b θ := fun θ => hb θ.2
  unfold brnBA
  rw [brn2_eq hB hP]
  have hX : ∀ i, 0 < a i * u i ^ (α ^ 2) := fun i =>
    mul_pos (ha i) (Real.rpow_pos_of_pos (hu i) _)
  have hY : ∀ j, 0 < b j ^ α * v j ^ (α ^ 2) := fun j =>
    mul_pos (Real.rpow_pos_of_pos (hb j) _) (Real.rpow_pos_of_pos (hv j) _)
  have hw : (fun θ : Fin 2 × Fin 2 => cueA a θ * cueB b θ ^ α * (u θ.1 * v θ.2) ^ (α ^ 2))
      = fun θ => (a θ.1 * u θ.1 ^ (α ^ 2)) * (b θ.2 ^ α * v θ.2 ^ (α ^ 2)) := by
    funext θ
    simp only [cueA, cueB]
    rw [Real.mul_rpow (hu _).le (hv _).le]
    ring
  rw [hw, margA_ratio_product hX hY, Real.div_rpow (hu 0).le (hu 1).le]
  field_simp

/-- **Paper B (ii), the position channel.**  At independence the A-marginal
odds when A is read first are `(a₀/a₁)^{α-1}` times those when A is read last:
the earlier cue is down-weighted from exponent `1` to exponent `α`. -/
theorem margOddsA_order_ratio {α : ℝ} {a b u v : Fin 2 → ℝ} (ha : ∀ i, 0 < a i)
    (hb : ∀ j, 0 < b j) (hu : ∀ i, 0 < u i) (hv : ∀ j, 0 < v j) :
    margA (brnAB α a b fun θ => u θ.1 * v θ.2) 0 / margA (brnAB α a b fun θ => u θ.1 * v θ.2) 1
      = (a 0 / a 1) ^ (α - 1) *
        (margA (brnBA α a b fun θ => u θ.1 * v θ.2) 0
          / margA (brnBA α a b fun θ => u θ.1 * v θ.2) 1) := by
  rw [margOddsA_brnAB_indep ha hb hu hv, margOddsA_brnBA_indep ha hb hu hv]
  have hr : 0 < a 0 / a 1 := div_pos (ha 0) (ha 1)
  have h0 := (ha 0).ne'
  have h1 := (ha 1).ne'
  rw [Real.rpow_sub hr, Real.rpow_one]
  field_simp

/-- **Paper B (ii).**  At independence, for `α ≠ 1` and an informative A-cue
(`a₀ ≠ a₁`), the A-marginal differs between the two orders. -/
theorem margOddsA_orders_ne {α : ℝ} (hα : α ≠ 1) {a b u v : Fin 2 → ℝ} (ha : ∀ i, 0 < a i)
    (hb : ∀ j, 0 < b j) (hu : ∀ i, 0 < u i) (hv : ∀ j, 0 < v j) (hinf : a 0 ≠ a 1) :
    margA (brnAB α a b fun θ => u θ.1 * v θ.2) 0 / margA (brnAB α a b fun θ => u θ.1 * v θ.2) 1
      ≠ margA (brnBA α a b fun θ => u θ.1 * v θ.2) 0
          / margA (brnBA α a b fun θ => u θ.1 * v θ.2) 1 := by
  rw [margOddsA_brnAB_indep ha hb hu hv, margOddsA_brnBA_indep ha hb hu hv]
  intro h
  have hr : 0 < a 0 / a 1 := div_pos (ha 0) (ha 1)
  have hU : 0 < (u 0 / u 1) ^ (α ^ 2) := Real.rpow_pos_of_pos (div_pos (hu 0) (hu 1)) _
  have h' : (a 0 / a 1) ^ α = (a 0 / a 1) ^ (1 : ℝ) := by
    rw [Real.rpow_one]; exact mul_right_cancel₀ hU.ne' h
  have hlog := congrArg Real.log h'
  rw [Real.log_rpow hr, Real.log_rpow hr, one_mul] at hlog
  have hlog0 : Real.log (a 0 / a 1) ≠ 0 := by
    intro h0
    have := Real.eq_one_of_pos_of_log_eq_zero hr h0
    exact hinf ((div_eq_one_iff_eq (ha 1).ne').mp this)
  have : (α - 1) * Real.log (a 0 / a 1) = 0 := by rw [sub_mul, hlog]; ring
  rcases mul_eq_zero.mp this with h1 | h1
  · exact hα (by linarith)
  · exact hlog0 h1

/-! ### An explicit rational instance -/

lemma sq_rpow_half {x : ℝ} (hx : 0 ≤ x) : (x ^ 2) ^ (1 / 2 : ℝ) = x := by
  rw [← Real.sqrt_eq_rpow, Real.sqrt_sq hx]

/-- The instance: uniform prior, `α = 1/2`, `a = b = (49/50, 1/50)`. -/
noncomputable def exPrior : Fin 2 × Fin 2 → ℝ := fun _ => 1 / 4
noncomputable def exCue : Fin 2 → ℝ := ![49 / 50, 1 / 50]

/-- **Paper B (ii), explicit instance.**  At independence (uniform prior), with
`α = 1/2` and `a = b = (49/50, 1/50)`, the believed probability of `A = 0` is
`7/8` when A is read first and `49/50` when A is read last (the Bayes
benchmark's value is also `49/50`). -/
theorem example_marginal_gap :
    margA (brnAB (1 / 2) exCue exCue exPrior) 0 = 7 / 8
      ∧ margA (brnBA (1 / 2) exCue exCue exPrior) 0 = 49 / 50 := by
  have hq : (1 / 2 : ℝ) ^ 2 = 1 / 4 := by norm_num
  have hP : ∀ θ : Fin 2 × Fin 2, 0 < exPrior θ := fun _ => by norm_num [exPrior]
  have hc : ∀ θ : Fin 2 × Fin 2, 0 < cueA exCue θ := by
    intro θ; fin_cases θ <;> norm_num [cueA, exCue]
  have hc' : ∀ θ : Fin 2 × Fin 2, 0 < cueB exCue θ := by
    intro θ; fin_cases θ <;> norm_num [cueB, exCue]
  -- the common factor `(1/4)^{1/4}` of the prior cancels in the normalisation
  have hprior : (fun θ : Fin 2 × Fin 2 => exPrior θ ^ ((1 / 2 : ℝ) ^ 2))
      = fun _ => (1 / 4 : ℝ) ^ ((1 / 2 : ℝ) ^ 2) := rfl
  have hk : 0 < (1 / 4 : ℝ) ^ ((1 / 2 : ℝ) ^ 2) := Real.rpow_pos_of_pos (by norm_num) _
  have h49 : (49 / 50 : ℝ) ^ (1 / 2 : ℝ) = 7 / (5 * (2 : ℝ) ^ (1 / 2 : ℝ)) := by
    rw [show (49 / 50 : ℝ) = (7 / 5) ^ 2 / 2 by norm_num,
      Real.div_rpow (by positivity) (by norm_num), sq_rpow_half (by norm_num)]
    field_simp
  have h1 : (1 / 50 : ℝ) ^ (1 / 2 : ℝ) = 1 / (5 * (2 : ℝ) ^ (1 / 2 : ℝ)) := by
    rw [show (1 / 50 : ℝ) = (1 / 5) ^ 2 / 2 by norm_num,
      Real.div_rpow (by positivity) (by norm_num), sq_rpow_half (by norm_num)]
    field_simp
  have hs : 0 < (2 : ℝ) ^ (1 / 2 : ℝ) := Real.rpow_pos_of_pos (by norm_num) _
  constructor
  · unfold brnAB
    rw [brn2_eq hc hP]
    have hw : (fun θ : Fin 2 × Fin 2 =>
          cueB exCue θ * cueA exCue θ ^ (1 / 2 : ℝ) * exPrior θ ^ ((1 / 2 : ℝ) ^ 2))
        = fun θ => (1 / 4 : ℝ) ^ ((1 / 2 : ℝ) ^ 2) *
            (cueA exCue θ ^ (1 / 2 : ℝ) * cueB exCue θ) := by
      funext θ; simp only [exPrior]; ring
    rw [hw, normalize_smul hk.ne']
    unfold margA normalize
    simp only [Fintype.sum_prod_type, Fin.sum_univ_two, cueA, cueB, exCue,
      Matrix.cons_val_zero, Matrix.cons_val_one, h49, h1]
    field_simp
    norm_num
  · unfold brnBA
    rw [brn2_eq hc' hP]
    have hw : (fun θ : Fin 2 × Fin 2 =>
          cueA exCue θ * cueB exCue θ ^ (1 / 2 : ℝ) * exPrior θ ^ ((1 / 2 : ℝ) ^ 2))
        = fun θ => (1 / 4 : ℝ) ^ ((1 / 2 : ℝ) ^ 2) *
            (cueA exCue θ * cueB exCue θ ^ (1 / 2 : ℝ)) := by
      funext θ; simp only [exPrior]; ring
    rw [hw, normalize_smul hk.ne']
    unfold margA normalize
    simp only [Fintype.sum_prod_type, Fin.sum_univ_two, cueA, cueB, exCue,
      Matrix.cons_val_zero, Matrix.cons_val_one, h49, h1]
    field_simp
    norm_num

end PaperB

end Literature.BenjaminBodohCreedRabin
