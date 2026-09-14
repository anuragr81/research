/-
# Augenblick & Rabin (2021), "Belief Movement, Uncertainty Reduction, and Rational Updating"

*Quarterly Journal of Economics* 136(2), 933-985.

Formalization of the paper's own definitions and its Proposition 1, not of
Paper B's claims.

Definitions (their Section 2, verbatim):
  * a belief stream is `θ : ℕ → ℝ`, `θ t` the belief in state 1 after period `t`;
  * uncertainty `u_t(θ) = (1 - θ t) * θ t`;
  * movement `m_{t₁,t₂}(θ) = ∑_{τ=t₁}^{t₂-1} (θ_{τ+1} - θ_τ)²`;
  * uncertainty reduction `r_{t₁,t₂}(θ) = ∑_{τ=t₁}^{t₂-1} (u_τ(θ) - u_{τ+1}(θ))`.

**Proposition 1.** For any DGP and any `t₁, t₂`, `EM_{t₁,t₂} = ER_{t₁,t₂}`.

The paper's proof hinges on the displayed one-period rewriting

  `EM_{t,t+1} - ER_{t,t+1} = E[(2θ_t - 1)(θ_t - θ_{t+1})]`,

which `excess_step` below shows is a *pointwise algebraic identity*, true of any
real sequence whatever, with no probability in it.  The probabilistic content of
Proposition 1 is therefore exactly this: the martingale difference is orthogonal
to the single instrument `2θ_t - 1`.  `prop1_of_orthogonal` states the
proposition with that orthogonality as its hypothesis, which is strictly weaker
than the full martingale property and is all the proof consumes.  This matches
the paper's own framing in its Section 4, where the martingale property is
written `E[f(θ₀,…,θ_t)(θ_t - θ_{t+1})] = 0` for an arbitrary instrument `f` and
their test is identified as the choice `f = 2θ_t - 1`.
-/
import Mathlib

namespace Literature.AugenblickRabin

open Finset

/-! ## The paper's definitions -/

/-- Uncertainty of a belief, `u(θ) = (1-θ)θ`. -/
def unc (θ : ℝ) : ℝ := (1 - θ) * θ

/-- One period's contribution to movement, `(θ_{t+1} - θ_t)²`. -/
def movementStep (θ : ℕ → ℝ) (t : ℕ) : ℝ := (θ (t + 1) - θ t) ^ 2

/-- One period's contribution to uncertainty reduction, `u_t - u_{t+1}`. -/
def reductionStep (θ : ℕ → ℝ) (t : ℕ) : ℝ := unc (θ t) - unc (θ (t + 1))

/-- `m_{t₁,t₂}(θ)`. -/
def movement (θ : ℕ → ℝ) (t₁ t₂ : ℕ) : ℝ := ∑ τ ∈ Ico t₁ t₂, movementStep θ τ

/-- `r_{t₁,t₂}(θ)`. -/
def reduction (θ : ℕ → ℝ) (t₁ t₂ : ℕ) : ℝ := ∑ τ ∈ Ico t₁ t₂, reductionStep θ τ

/-- The instrument the paper's test amounts to, `2θ_t - 1`. -/
def instrument (θ : ℕ → ℝ) (t : ℕ) : ℝ := 2 * θ t - 1

/-- The martingale difference at `t`. -/
def mgDiff (θ : ℕ → ℝ) (t : ℕ) : ℝ := θ t - θ (t + 1)

/-! ## The algebraic core of Proposition 1 -/

/-- **The paper's displayed one-period rewriting, as an identity.**
`m_{t,t+1} - r_{t,t+1} = (2θ_t - 1)(θ_t - θ_{t+1})` holds pointwise for every
real sequence; no probabilistic assumption is involved. -/
theorem excess_step (θ : ℕ → ℝ) (t : ℕ) :
    movementStep θ t - reductionStep θ t = instrument θ t * mgDiff θ t := by
  unfold movementStep reductionStep unc instrument mgDiff
  ring

/-- Uncertainty reduction telescopes: `r_{t₁,t₂} = u_{t₁} - u_{t₂}`.  The paper
states this as part of the definition. -/
theorem reduction_telescope (θ : ℕ → ℝ) {t₁ t₂ : ℕ} (h : t₁ ≤ t₂) :
    reduction θ t₁ t₂ = unc (θ t₁) - unc (θ t₂) := by
  unfold reduction reductionStep
  induction t₂, h using Nat.le_induction with
  | base => simp
  | succ n hn ih =>
      rw [Finset.sum_Ico_succ_top hn, ih]
      ring

/-- Summed form of `excess_step`. -/
theorem movement_sub_reduction (θ : ℕ → ℝ) (t₁ t₂ : ℕ) :
    movement θ t₁ t₂ - reduction θ t₁ t₂
      = ∑ τ ∈ Ico t₁ t₂, instrument θ τ * mgDiff θ τ := by
  unfold movement reduction
  rw [← Finset.sum_sub_distrib]
  exact Finset.sum_congr rfl fun τ _ => excess_step θ τ

/-! ## Proposition 1 in expectation

A finite DGP is a finite index type of histories `ι` with weights `w : ι → ℝ`,
and a belief stream `θ i` on each history.  Expectations are the weighted sums
`∑ i, w i * ·`.  No further structure is needed, and in particular `w` is not
required to be nonnegative or to sum to one: Proposition 1 is an identity of
weighted sums, and its only real hypothesis is the orthogonality below. -/

variable {ι : Type*} [Fintype ι]

/-- Expectation of a per-history quantity against weights `w`. -/
def expect (w : ι → ℝ) (f : ι → ℝ) : ℝ := ∑ i, w i * f i

/-- **Proposition 1.**  If at every period in `[t₁, t₂)` the martingale
difference is orthogonal to the instrument `2θ_t - 1`, then expected movement
equals expected uncertainty reduction. -/
theorem prop1_of_orthogonal (w : ι → ℝ) (θ : ι → ℕ → ℝ) (t₁ t₂ : ℕ)
    (h : ∀ τ ∈ Ico t₁ t₂, expect w (fun i => instrument (θ i) τ * mgDiff (θ i) τ) = 0) :
    expect w (fun i => movement (θ i) t₁ t₂) = expect w (fun i => reduction (θ i) t₁ t₂) := by
  simp only [expect] at h ⊢
  rw [← sub_eq_zero, ← Finset.sum_sub_distrib]
  calc ∑ i, (w i * movement (θ i) t₁ t₂ - w i * reduction (θ i) t₁ t₂)
      = ∑ i, ∑ τ ∈ Ico t₁ t₂, w i * (instrument (θ i) τ * mgDiff (θ i) τ) := by
        refine Finset.sum_congr rfl fun i _ => ?_
        rw [← mul_sub, movement_sub_reduction, Finset.mul_sum]
    _ = ∑ τ ∈ Ico t₁ t₂, ∑ i, w i * (instrument (θ i) τ * mgDiff (θ i) τ) :=
        Finset.sum_comm
    _ = 0 := Finset.sum_eq_zero fun τ hτ => h τ hτ

/-- A Bayesian's belief stream is a martingale, and the martingale property says
the difference is orthogonal to *every* instrument measurable at `t`.  Taking
`f = 2θ_t - 1` gives the hypothesis of `prop1_of_orthogonal`; this lemma records
that specialization. -/
theorem orthogonal_of_martingale (w : ι → ℝ) (θ : ι → ℕ → ℝ) (τ : ℕ)
    (hmg : ∀ f : ℝ → ℝ, expect w (fun i => f (θ i τ) * mgDiff (θ i) τ) = 0) :
    expect w (fun i => instrument (θ i) τ * mgDiff (θ i) τ) = 0 :=
  hmg (fun x => 2 * x - 1)

/-- **Corollary 1** (resolving streams).  If every history resolves, meaning the
terminal belief is `0` or `1`, the terminal uncertainty vanishes, so expected
movement equals the initial uncertainty. -/
theorem resolving (w : ι → ℝ) (θ : ι → ℕ → ℝ) (T : ℕ)
    (hres : ∀ i, θ i T = 0 ∨ θ i T = 1)
    (h : ∀ τ ∈ Ico 0 T, expect w (fun i => instrument (θ i) τ * mgDiff (θ i) τ) = 0) :
    expect w (fun i => movement (θ i) 0 T) = expect w (fun i => unc (θ i 0)) := by
  rw [prop1_of_orthogonal w θ 0 T h]
  simp only [expect]
  refine Finset.sum_congr rfl fun i _ => ?_
  rw [reduction_telescope (θ i) (Nat.zero_le T)]
  rcases hres i with h0 | h1
  · rw [h0]; unfold unc; ring
  · rw [h1]; unfold unc; ring

end Literature.AugenblickRabin
