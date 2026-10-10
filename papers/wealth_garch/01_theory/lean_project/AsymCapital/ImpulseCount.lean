/-
================================================================================
  FIN (lem:fin): no chattering — the discounted impulse count is finite
  Companion to PROOFS_v2.tex, Lemma FIN.  Lean 4 + Mathlib.  DRAFT — not
  yet compiled.
================================================================================

SCOPE, in the same style as Envelope.lean.

The paper's proof has one analytic input and one summability argument.
Analytic input (stays in the document): each impulse costs at least K, so
along any admissible strategy the discounted cost over the first n impulses
is at least K * (d_0 + ... + d_{n-1}), where d_i = e^{-rho_L tau_i}; and the
strategy's value is finite, bounding that cost by some B.  Taking the
expectation and comparing against the finite unconstrained value is the
probabilistic content, and it enters below only through the hypothesis
`hcost : forall n, K * (partial sum of d over range n) <= B`.

What is machine-checked here is everything after that point:
  * `partial_sums_bounded`  -- the division step: with K > 0 the cost bound
                               caps every partial sum of d by B / K.
  * `count_summable`        -- bounded partial sums of a nonnegative sequence
                               make it summable: N = tsum d exists.
  * `count_le`              -- and N <= B / K, the quantitative form of KN <= B.
  * `impulse_sum_summable`  -- the consequence FIN is cited for in VER: any
                               per-impulse contribution dominated by C * d_i
                               converges absolutely.

The discount factors enter as an abstract nonnegative sequence d : nat -> real;
nothing here needs d_i = e^{-rho_L tau_i} beyond 0 <= d_i, which keeps the file
free of any stochastic object, exactly as intended.

KEY LEMMAS.  `count_summable` uses `summable_of_sum_range_le` (bounded partial
sums of a nonnegative sequence are summable).  `count_le` reads the bound off
the limit: `Summable.tendsto_sum_tsum_nat` sends the partial sums to `∑' i, d i`
and `le_of_tendsto'` carries the uniform bound `≤ C` to the limit.
`impulse_sum_summable` is the direct comparison test via `Summable.of_norm_bounded`
(majorant first) with `Summable.mul_left` for the `C · d` majorant.
-/

import Mathlib

namespace ImpulseCount

/-- **Division step.**  From the cost bound `K * (sum over range n of d) <= B`
with `K > 0`, every partial sum of `d` is at most `B / K`.  Pure ordered-field
algebra. -/
theorem partial_sums_bounded
    (K B : ℝ) (hK : 0 < K) (d : ℕ → ℝ)
    (hcost : ∀ n : ℕ, K * ∑ i ∈ Finset.range n, d i ≤ B) :
    ∀ n : ℕ, ∑ i ∈ Finset.range n, d i ≤ B / K := by
  intro n
  rw [le_div_iff₀ hK, mul_comm]
  exact hcost n

/-- **FIN, existence half.**  A nonnegative sequence whose partial sums are
uniformly bounded is summable: the discounted impulse count
`N = ∑' i, d i` is a well-defined real number. -/
theorem count_summable
    (C : ℝ) (d : ℕ → ℝ) (hd : ∀ i, 0 ≤ d i)
    (hb : ∀ n : ℕ, ∑ i ∈ Finset.range n, d i ≤ C) :
    Summable d :=
  summable_of_sum_range_le hd hb

/-- **FIN, quantitative half.**  Under the same hypotheses, `N <= C`;
with `C = B / K` this is exactly the paper's `K N <= B`. -/
theorem count_le
    (C : ℝ) (d : ℕ → ℝ) (hd : ∀ i, 0 ≤ d i)
    (hb : ∀ n : ℕ, ∑ i ∈ Finset.range n, d i ≤ C) :
    ∑' i, d i ≤ C := by
  have hsum : Summable d := summable_of_sum_range_le hd hb
  exact le_of_tendsto' hsum.tendsto_sum_tsum_nat hb

/-- **The consequence VER cites.**  Any per-impulse contribution `a i`
dominated in absolute value by `C * d i` — in VER, the impulse term
`e^{-rho_L tau_i} (v(X_{tau_i}) - v(X_{tau_i}^-))` with the growth bound
supplying `C` — converges absolutely once the count does.  Direct comparison
test. -/
theorem impulse_sum_summable
    (d : ℕ → ℝ) (hsum : Summable d)
    (a : ℕ → ℝ) (C : ℝ) (hbound : ∀ i, |a i| ≤ C * d i) :
    Summable a ∧ Summable fun i => |a i| := by
  have hmaj : Summable fun i => C * d i := hsum.mul_left C
  have habs : Summable fun i => |a i| := by
    refine hmaj.of_norm_bounded ?_
    intro i
    simpa [Real.norm_eq_abs, abs_abs] using hbound i
  refine ⟨?_, habs⟩
  refine hmaj.of_norm_bounded ?_
  intro i
  simpa [Real.norm_eq_abs] using hbound i

end ImpulseCount
