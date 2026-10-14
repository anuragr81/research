import Mathlib

/-!
# Hassin and Haviv, "Equilibrium Threshold Strategies: The Case of Queues with Priorities"

Operations Research 45(6), 1997, 966-973.  Read from the authors' preprint (17 pages); page
numbers are the preprint's.

Setup (pp.5-6): Poisson arrivals at rate `λ` to one exponential server at rate `μ`, `ρ = λ/μ < 1`;
an arrival sees the queues and may buy preemptive priority at a toll `θ`; waiting costs one per
unit of time; `B = 1/(μ − λ)` is the expected busy period.  Customers are identical.  Those who
do not buy are still served, after the priority customers.

## What is proved

1. Section 2 (p.4): with an avoid-the-crowd best response (a non-increasing threshold map) there
   is at most one equilibrium (`atc_unique`); with follow the crowd there can be many (`ftc_many`).
   This is the logic of our count invariance: the gain from paying does not rise with the number
   who pay.
2. Proposition 4.1 (p.7) as a definition (`PureEq`), and the counting step of Proposition 5.1
   (p.10): if `E` rises by at least `1/μ` per step, the pure equilibrium thresholds lie within
   `⌊Bμ⌋ = ⌊1/(1 − ρ)⌋` steps of each other (`span_le`), so there are at most `⌊1/(1 − ρ)⌋ + 1`.
3. The paper states at most `⌊1/(1 − ρ)⌋`.  The appendix recursion (p.17), computed exactly at
   `ρ = 3/5` (`E35`), gives three consecutive pure equilibria for one toll
   (`three_pure_equilibria`) while `1/(1 − ρ) = 5/2` (`floor_bound_35`).  Controls: the computed
   `E` meets `E(1) = B` (p.8, `E35_one`) and the increment bound of Proposition 5.1 for
   `n = 1, …, 10` (`E35_increments`), and thresholds 4 and 8 are not equilibria (`four_eight_not_equilibria`).

## Not formalised

Proposition 3.1 (follow the crowd; a sample-path argument), the limit in Proposition 5.1, and
Section 6 (balking).  The record (`lit/hassin_haviv_1997`) reads them from the paper.
-/

namespace HassinHaviv1997

/-! ## 1. Avoid the crowd and follow the crowd -/

/-- A non-increasing threshold map has at most one fixed point. -/
theorem atc_unique {k : ℕ → ℕ} (hk : Antitone k) {a b : ℕ} (ha : k a = a) (hb : k b = b) :
    a = b := by
  rcases lt_trichotomy a b with h | h | h
  · have := hk h.le
    omega
  · exact h
  · have := hk h.le
    omega

/-- Control: a non-decreasing threshold map can have several fixed points. -/
theorem ftc_many : Monotone (id : ℕ → ℕ) ∧ id 3 = 3 ∧ id 4 = 4 ∧ (3 : ℕ) ≠ 4 :=
  ⟨monotone_id, rfl, rfl, by decide⟩

/-! ## 2. Proposition 4.1 and the counting step of Proposition 5.1 -/

/-- Proposition 4.1: the integer threshold `n ≥ 1` is a Nash equilibrium iff
    `1/μ + θ − B ≤ E(n) ≤ 1/μ + θ`. -/
def PureEq (E : ℕ → ℚ) (θ μ B : ℚ) (n : ℕ) : Prop :=
  1 ≤ n ∧ 1 / μ + θ - B ≤ E n ∧ E n ≤ 1 / μ + θ

theorem incr_ge (E : ℕ → ℝ) (d : ℝ) (hE : ∀ n, E n + d ≤ E (n + 1)) (a : ℕ) :
    ∀ m : ℕ, E a + m * d ≤ E (a + m) := by
  intro m
  induction m with
  | zero => simp
  | succ m ih =>
    have := hE (a + m)
    push_cast
    rw [← add_assoc]
    linarith

/-- The counting step: if `E` rises by at least `d > 0` per step, two thresholds whose values
    lie in a window of length `ℓ` are at most `ℓ/d` apart.  With `d = 1/μ` and `ℓ = B` this is
    `Bμ = 1/(1 − ρ)`, so the pure equilibria number at most `⌊1/(1 − ρ)⌋ + 1`. -/
theorem span_le (E : ℕ → ℝ) {d ℓ L : ℝ} (hd : 0 < d) (hE : ∀ n, E n + d ≤ E (n + 1))
    {a b : ℕ} (hab : a ≤ b) (ha : L ≤ E a) (hb : E b ≤ L + ℓ) : ((b - a : ℕ) : ℝ) ≤ ℓ / d := by
  have h := incr_ge E d hE a (b - a)
  rw [Nat.add_sub_cancel' hab] at h
  rw [le_div_iff₀ hd]
  linarith

/-! ## 3. The appendix recursion, computed exactly -/

/-- One row of the appendix recursion (p.17) at an integer threshold, in time units with
    `λ + μ = 1`.  `prev` is row `i − 1` (row 0 is zero), `kf` is the number behind position `i`
    at which the low-priority queue is full, and the argument `d` gives the value at
    `k = kf − d`.  At a full queue the next arrival buys priority and costs a busy period. -/
def cell (lam mu B : ℚ) (prev : ℕ → ℚ) (kf : ℕ) : ℕ → ℚ
  | 0 => (1 + lam * B + mu * prev kf) / mu
  | d + 1 => 1 + lam * cell lam mu B prev kf d + mu * prev (kf - (d + 1))

/-- `H i k` for the threshold `m = n + 1`: row `i`, `k` customers behind. -/
def H (lam mu B : ℚ) (n : ℕ) : ℕ → ℕ → ℚ
  | 0 => fun _ => 0
  | i + 1 => fun k => cell lam mu B (H lam mu B n i) (n - i) (n - i - k)

/-- `E(m) = H_{m,0}` at the integer threshold `m ≥ 1`. -/
def E (lam mu : ℚ) (m : ℕ) : ℚ := H lam mu (1 / (mu - lam)) (m - 1) m 0

/-- `ρ = 3/5`: `λ = 3/8`, `μ = 5/8`, `B = 4`, `1/μ = 8/5`. -/
def E35 : ℕ → ℚ := E (3 / 8) (5 / 8)

theorem floor_bound_35 : (1 : ℚ) / (1 - 3 / 5) = 5 / 2 ∧ (5 / 2 : ℚ) < 3 := by norm_num

/-- Control: `E(1) = B` (p.8). -/
theorem E35_one : E35 1 = 4 := by decide +kernel

/-- Control: the increment bound of Proposition 5.1, `E(n + 1) − E(n) ≥ 1/μ`, for
    `n = 1, …, 10`. -/
theorem E35_increments : ∀ n ∈ Finset.Icc 1 10, E35 n + 8 / 5 ≤ E35 (n + 1) := by decide +kernel

/-- Three consecutive pure equilibria at `ρ = 3/5`, for the toll `θ = E(7) − 1/μ`, against the
    stated bound `⌊1/(1 − ρ)⌋ = 2`. -/
theorem three_pure_equilibria :
    PureEq E35 (E35 7 - 8 / 5) (5 / 8) 4 5 ∧ PureEq E35 (E35 7 - 8 / 5) (5 / 8) 4 6 ∧
      PureEq E35 (E35 7 - 8 / 5) (5 / 8) 4 7 := by
  unfold PureEq
  refine ⟨⟨by norm_num, ?_, ?_⟩, ⟨by norm_num, ?_, ?_⟩, ⟨by norm_num, ?_, ?_⟩⟩ <;>
    norm_num <;> decide +kernel

/-- Control: the window holds exactly these three, since thresholds 4 and 8 are not equilibria. -/
theorem four_eight_not_equilibria :
    ¬ PureEq E35 (E35 7 - 8 / 5) (5 / 8) 4 4 ∧ ¬ PureEq E35 (E35 7 - 8 / 5) (5 / 8) 4 8 := by
  unfold PureEq
  constructor
  · intro ⟨_, h, _⟩
    norm_num at h
    revert h
    decide +kernel
  · intro ⟨_, _, h⟩
    norm_num at h
    revert h
    decide +kernel

end HassinHaviv1997
