/-
# Banerjee, A.V. (1992), "A Simple Model of Herd Behavior"

*Quarterly Journal of Economics* 107(3), 797-817.

Formalization of the paper's basic model (Sections II-IV, pp.802-811) at the
level of its displayed computations, not of Paper B's claims.

## The paper's setup (Sec. II, pp.802-803)

Options are indexed by `i ∈ [0,1]`; exactly one, `i*`, pays `z > 0`, and `i*` has
a uniform prior.  Each of `N` agents, in a fixed order, receives a signal with
probability `α`; a signal is true with probability `β`, and otherwise uniform on
`[0,1]` and uninformative.  Each agent observes predecessors' choices but not
whether they had a signal.  Tie-breaks: (A) an uninformed agent facing only
`i = 0` choices picks `0`; (B) indifferent between own signal and following, she
follows her own signal; (C) indifferent between predecessors, she follows the one
with the highest `i`.  Proposition 1 (p.806): the unique equilibrium rule is `D`
(first agent follows her signal or picks 0; later agents follow their own signal
iff it matches a chosen option or no option other than `0` has been chosen twice;
otherwise they join the option chosen by more than one person).

## What is formalized

* `wBar`, `wPrime`, `lemma1` — **Lemma 1** (p.805): if agents 1 and 2 both chose
  `ī ≠ 0` and agent 3 has signal `i'`, the posterior weight on `i* = ī` exceeds
  that on `i* = i'`.  The weights are built factor by factor from the model;
  `lemma1_weights` shows the first equals the printed
  `α³β²(1-β) + α²β(1-β)(1-α)` and the second is `α²β(1-β)(1-α)`.  The paper prints
  the second with an extra factor `β` (`printed_second_weight_lt`); the lemma's
  conclusion is unaffected.
* `followProb`, `herd_follow_uninformative` — once a herd has formed, an agent's
  choice of the herd option has probability `1` both when the herd is right and
  when `i*` has not yet been chosen: joining the herd conveys no information
  (the "herd externality", p.799).  It is informative only against the event that
  `i*` is one of the other already-chosen options.
* `chain`, `PiHerd` — the equilibrium path under rule `D` on a four-state abstraction
  (only `i = 0` chosen so far; distinct wrong options, no herd; a herd on a wrong
  option; `i*` chosen), with per-agent transition probabilities read off rule `D`.
* `pi_hasSum` — **the herd probability** (p.808, repeated p.810)
  `Π = [1 - α(1-β)]⁻¹(1-α)(1-β)` is the sum of the herding-path series.
* `no_one_correct_ge_Pi`, `tendsto_no_one_correct` — for every population size
  `N`, the probability that no one chooses `i*` is at least `Π`, and it tends to
  `Π` as `N → ∞`: "bounded away from zero for any size of the population" (p.800).
* `Pi_antitone_alpha`, `Pi_antitone_beta`, `Pi_beta_zero` — `Π` is decreasing in
  `α` and in `β`, and `Π = 1` at `β = 0` ("by making the probability β small, we
  can make this probability as large as we like", p.800; p.808).
* `independent_tendsto_zero` — the contrast: without observation, no one is
  correct with probability `(1-αβ)^N → 0` (p.800, p.808).
* `dstar_formula` — the displayed expression on p.810,
  `1 - (1-αβ)^{n-1} - (n-1)(1-αβ)^{n-2}αβ`, is the binomial upper tail
  `Pr(Bin(n-1, αβ) ≥ 2)`: the probability that at least two of the first `n-1`
  agents *have* received the true signal (the text says "have not received").

## Not formalized

The full equilibrium strategy of Proposition 1 over arbitrary histories
(including the off-path parts left open in fn 16) and its uniqueness argument;
the welfare comparison with rule `D*` beyond `dstar_formula` (its normalisation
is discussed in the README); Proposition 2 (rewards for being first); Sections
V.B-C and VI.  The four-state chain is an abstraction of rule `D`: it is exact for
the event "someone has chosen `i*`" because a false signal matches an
already-chosen option with probability zero.
-/
import Mathlib

namespace Literature.Banerjee

/-! ## Lemma 1 -/

/-- Posterior weight on `i* = ī` given `H` (agents 1 and 2 chose `ī ≠ 0`, agent 3
has signal `i'`): agent 1 informed and right (`αβ`); agent 2 uninformed and
following (`1-α`) or informed and right (`αβ`); agent 3 informed with a false
signal at `i'` (`α(1-β)`, density `1`). -/
def wBar (α β : ℝ) : ℝ := (α * β) * ((1 - α) + α * β) * (α * (1 - β))

/-- Posterior weight on `i* = i'` given `H`: agent 1 informed with a false signal
at `ī` (`α(1-β)`); agent 2 must be uninformed (`1-α`), since a true signal would
send her to `i'` and a false one hits `ī` with probability zero; agent 3 informed
and right (`αβ`). -/
def wPrime (α β : ℝ) : ℝ := (α * (1 - β)) * (1 - α) * (α * β)

/-- The weights in the paper's notation (p.805). -/
theorem lemma1_weights (α β : ℝ) :
    wBar α β = α ^ 3 * β ^ 2 * (1 - β) + α ^ 2 * β * (1 - β) * (1 - α) ∧
    wPrime α β = α ^ 2 * β * (1 - β) * (1 - α) := by
  exact ⟨by unfold wBar; ring, by unfold wPrime; ring⟩

/-- **Lemma 1** (p.805): "If the first and the second decision makers have both
chosen the same `ī ≠ 0`, the third decision maker should choose to follow them."
The difference of the weights is `α³β²(1-β) > 0`. -/
theorem lemma1 {α β : ℝ} (hα : 0 < α) (hβ0 : 0 < β) (hβ1 : β < 1) :
    wPrime α β < wBar α β := by
  have h : wBar α β - wPrime α β = α ^ 3 * β ^ 2 * (1 - β) := by unfold wBar wPrime; ring
  have : 0 < α ^ 3 * β ^ 2 * (1 - β) := by
    have : 0 < 1 - β := by linarith
    positivity
  linarith

/-- The second weight as printed on p.805, `α²β(1-β)(1-α)β`, carries an extra
factor `β`: it is strictly smaller than the weight the model gives, so the printed
comparison is a fortiori true but the printed expression is not the posterior
weight. -/
theorem printed_second_weight_lt {α β : ℝ} (hα0 : 0 < α) (hα1 : α < 1) (hβ0 : 0 < β) (hβ1 : β < 1) :
    α ^ 2 * β * (1 - β) * (1 - α) * β < wPrime α β ∧
    α ^ 2 * β * (1 - β) * (1 - α) * β < wBar α β := by
  have hw := (lemma1_weights α β).2
  have h1 : 0 < α ^ 2 * β * (1 - β) * (1 - α) := by
    have : 0 < 1 - β := by linarith
    have : 0 < 1 - α := by linarith
    positivity
  have : α ^ 2 * β * (1 - β) * (1 - α) * β < α ^ 2 * β * (1 - β) * (1 - α) := by
    nlinarith
  exact ⟨by rw [hw]; exact this, lt_trans (by rw [hw]; exact this) (lemma1 hα0 hβ0 hβ1)⟩

/-! ## Joining a herd conveys no information -/

/-- Where the true option lies relative to the history when a herd has formed on
`ī`: `ī` itself, another already-chosen option, or not yet chosen. -/
inductive Hyp
  | herd
  | otherChosen
  | unchosen

/-- Probability, under rule `D`, that an agent facing a herd on `ī` chooses `ī`.
Uninformed (`1-α`): joins.  Informed with a false signal (`α(1-β)`): the signal
matches no chosen option (probability one), so she joins.  Informed with a true
signal (`αβ`): she follows her own signal, which is `ī` if the herd is right, a
different chosen option if `i*` was chosen by someone else, and an unchosen option
otherwise — in which case rule `D` item 4 sends her to the herd. -/
def followProb (α β : ℝ) : Hyp → ℝ
  | .herd => (1 - α) + α * (1 - β) + α * β
  | .otherChosen => (1 - α) + α * (1 - β)
  | .unchosen => (1 - α) + α * (1 - β) + α * β

/-- **Joining the herd is uninformative** between "the herd is right" and "`i*` has
not been chosen": both likelihoods are `1`, so the next agent's posterior over
these hypotheses is unchanged.  This is the herd externality (p.799): "her choice
therefore provides no new information to the next person in line". -/
theorem herd_follow_uninformative (α β : ℝ) :
    followProb α β .herd = 1 ∧ followProb α β .unchosen = 1 ∧
    followProb α β .otherChosen = 1 - α * β := by
  refine ⟨?_, ?_, ?_⟩ <;> simp only [followProb] <;> ring

/-! ## The herd probability `Π` -/

/-- `Π = [1 - α(1-β)]⁻¹ (1-α)(1-β)` (p.808, p.810). -/
noncomputable def PiHerd (α β : ℝ) : ℝ := (1 - α) * (1 - β) / (1 - α * (1 - β))

lemma den_pos {α β : ℝ} (hα : α < 1) (hβ0 : 0 ≤ β) (hβ1 : β ≤ 1) : 0 < 1 - α * (1 - β) := by
  rcases le_or_gt α 0 with h | h
  · nlinarith
  · nlinarith

/-- **The herd probability as a series**: the first informed agent is wrong
(`1-β`), then `k` further informed agents are wrong (`(α(1-β))^k`), then an
uninformed agent joins the last one and a herd forms (`1-α`). -/
theorem pi_hasSum {α β : ℝ} (hα0 : 0 ≤ α) (hα1 : α < 1) (hβ0 : 0 ≤ β) (hβ1 : β ≤ 1) :
    HasSum (fun k : ℕ => (1 - β) * ((α * (1 - β)) ^ k * (1 - α))) (PiHerd α β) := by
  have h0 : 0 ≤ α * (1 - β) := mul_nonneg hα0 (by linarith)
  have h1 : α * (1 - β) < 1 := by nlinarith
  have hs := ((hasSum_geometric_of_lt_one h0 h1).mul_right (1 - α)).mul_left (1 - β)
  have hD := (den_pos hα1 hβ0 hβ1).ne'
  have e : PiHerd α β = (1 - β) * ((1 - α * (1 - β))⁻¹ * (1 - α)) := by
    unfold PiHerd; field_simp
  rw [e]; exact hs

/-- States of the equilibrium path under rule `D`:
`s0` only `i = 0` chosen so far; `op` distinct wrong options chosen, no herd;
`hd` a herd on a wrong option (absorbing: later signals match a chosen option only
if `i*` was chosen); `fd` someone has chosen `i*` (absorbing). -/
structure St where
  s0 : ℝ
  op : ℝ
  hd : ℝ
  fd : ℝ

/-- One agent under rule `D`.  From `s0`: uninformed picks `0` (stay), informed
right finds `i*`, informed wrong opens a wrong option.  From `op`: uninformed joins
the highest option and a herd forms, informed right finds `i*`, informed wrong
opens another wrong option (Assumption B). -/
def step (α β : ℝ) (x : St) : St where
  s0 := x.s0 * (1 - α)
  op := (x.s0 + x.op) * (α * (1 - β))
  hd := x.hd + x.op * (1 - α)
  fd := x.fd + (x.s0 + x.op) * (α * β)

/-- The distribution after `N` agents. -/
def chain (α β : ℝ) : ℕ → St
  | 0 => ⟨1, 0, 0, 0⟩
  | n + 1 => step α β (chain α β n)

lemma chain_nonneg {α β : ℝ} (hα0 : 0 ≤ α) (hα1 : α ≤ 1) (hβ0 : 0 ≤ β) (hβ1 : β ≤ 1) (n : ℕ) :
    0 ≤ (chain α β n).s0 ∧ 0 ≤ (chain α β n).op ∧ 0 ≤ (chain α β n).hd ∧ 0 ≤ (chain α β n).fd := by
  induction n with
  | zero => simp [chain]
  | succ n ih =>
    obtain ⟨a, b, c, d⟩ := ih
    simp only [chain, step]
    have : 0 ≤ 1 - α := by linarith
    have : 0 ≤ 1 - β := by linarith
    refine ⟨by positivity, by positivity, by positivity, by positivity⟩

lemma chain_total (α β : ℝ) (n : ℕ) :
    (chain α β n).s0 + (chain α β n).op + (chain α β n).hd + (chain α β n).fd = 1 := by
  induction n with
  | zero => simp [chain]
  | succ n ih => simp only [chain, step]; linarith

/-- The probability of eventually finding `i*` from `op`. -/
noncomputable def Fo (α β : ℝ) : ℝ := α * β / (1 - α * (1 - β))

/-- The eventual-success value is conserved along the chain (a martingale):
`(1-Π) s0 + Fo op + fd = 1 - Π` at every `N`. -/
lemma value_invariant {α β : ℝ} (hα1 : α < 1) (hβ0 : 0 ≤ β) (hβ1 : β ≤ 1) (n : ℕ) :
    (1 - PiHerd α β) * (chain α β n).s0 + Fo α β * (chain α β n).op + (chain α β n).fd = 1 - PiHerd α β := by
  have hD := (den_pos hα1 hβ0 hβ1).ne'
  induction n with
  | zero => simp [chain]
  | succ n ih =>
    have hP : 1 - PiHerd α β = β / (1 - α * (1 - β)) := by
      unfold PiHerd; field_simp; ring
    rw [hP] at ih ⊢
    unfold Fo at ih ⊢
    simp only [chain, step]
    conv_rhs => rw [← ih]
    field_simp
    ring

/-- **The probability that no one chooses the right option is at least `Π` for
every population size** (p.800, item 2; p.808). -/
theorem no_one_correct_ge_Pi {α β : ℝ} (hα0 : 0 ≤ α) (hα1 : α < 1) (hβ0 : 0 ≤ β) (hβ1 : β ≤ 1)
    (N : ℕ) : PiHerd α β ≤ 1 - (chain α β N).fd := by
  have hv := value_invariant hα1 hβ0 hβ1 N
  obtain ⟨a, b, -, -⟩ := chain_nonneg hα0 hα1.le hβ0 hβ1 N
  have hD := den_pos hα1 hβ0 hβ1
  have h1 : 0 ≤ 1 - PiHerd α β := by
    unfold PiHerd; rw [sub_nonneg, div_le_one hD]; nlinarith
  have h2 : 0 ≤ Fo α β := by unfold Fo; positivity
  nlinarith [mul_nonneg h1 a, mul_nonneg h2 b]

/-- The transient mass decays geometrically: `s0 + op ≤ (1-αβ)^N`. -/
lemma transient_le {α β : ℝ} (hα0 : 0 ≤ α) (hα1 : α ≤ 1) (hβ0 : 0 ≤ β) (hβ1 : β ≤ 1) (n : ℕ) :
    (chain α β n).s0 + (chain α β n).op ≤ (1 - α * β) ^ n := by
  induction n with
  | zero => simp [chain]
  | succ n ih =>
    obtain ⟨a, b, -, -⟩ := chain_nonneg hα0 hα1 hβ0 hβ1 n
    simp only [chain, step, pow_succ]
    have hab : α * (1 - β) ≤ 1 - α * β := by nlinarith
    have hq : 0 ≤ 1 - α * β := by nlinarith
    calc (chain α β n).s0 * (1 - α) + ((chain α β n).s0 + (chain α β n).op) * (α * (1 - β))
        = (chain α β n).s0 * (1 - α * β) + (chain α β n).op * (α * (1 - β)) := by ring
      _ ≤ (chain α β n).s0 * (1 - α * β) + (chain α β n).op * (1 - α * β) := by
          gcongr
      _ = ((chain α β n).s0 + (chain α β n).op) * (1 - α * β) := by ring
      _ ≤ (1 - α * β) ^ n * (1 - α * β) := by gcongr

/-- **The probability that no one chooses the right option tends to `Π`** as the
population grows (p.808: "however large the population"). -/
theorem tendsto_no_one_correct {α β : ℝ} (hα0 : 0 < α) (hα1 : α < 1) (hβ0 : 0 < β) (hβ1 : β ≤ 1) :
    Filter.Tendsto (fun N => 1 - (chain α β N).fd) Filter.atTop (nhds (PiHerd α β)) := by
  have hD := den_pos hα1 hβ0.le hβ1
  have hq0 : 0 ≤ 1 - α * β := by nlinarith
  have hq1 : 1 - α * β < 1 := by nlinarith [mul_pos hα0 hβ0]
  have hgeo := tendsto_pow_atTop_nhds_zero_of_lt_one hq0 hq1
  have h1 : 1 - PiHerd α β ≤ 1 := by
    unfold PiHerd; have : 0 ≤ (1 - α) * (1 - β) / (1 - α * (1 - β)) := by
      apply div_nonneg _ hD.le; nlinarith
    linarith
  have hFo : Fo α β ≤ 1 := by unfold Fo; rw [div_le_one hD]; nlinarith
  have hFo0 : 0 ≤ Fo α β := by unfold Fo; positivity
  have h10 : 0 ≤ 1 - PiHerd α β := by
    unfold PiHerd; rw [sub_nonneg, div_le_one hD]; nlinarith
  -- `1 - fd - Π = (1-Π) s0 + Fo op ∈ [0, s0 + op]`
  have key : ∀ N, 0 ≤ (1 - (chain α β N).fd) - PiHerd α β ∧
      (1 - (chain α β N).fd) - PiHerd α β ≤ (1 - α * β) ^ N := by
    intro N
    have hv := value_invariant hα1 hβ0.le hβ1 N
    obtain ⟨a, b, -, -⟩ := chain_nonneg hα0.le hα1.le hβ0.le hβ1 N
    have ht := transient_le hα0.le hα1.le hβ0.le hβ1 N
    constructor
    · nlinarith [mul_nonneg h10 a, mul_nonneg hFo0 b]
    · nlinarith [mul_le_mul_of_nonneg_right h1 a, mul_le_mul_of_nonneg_right hFo b,
        mul_nonneg h10 a, mul_nonneg hFo0 b]
  have hz : Filter.Tendsto (fun N => (1 - (chain α β N).fd) - PiHerd α β) Filter.atTop (nhds 0) :=
    squeeze_zero (fun N => (key N).1) (fun N => (key N).2) hgeo
  have := hz.add_const (PiHerd α β)
  simpa using this

/-- `Π` is decreasing in `β` (p.808). -/
theorem Pi_antitone_beta {α β₁ β₂ : ℝ} (hα1 : α < 1) (h0 : 0 ≤ β₁) (h12 : β₁ ≤ β₂)
    (h2 : β₂ ≤ 1) : PiHerd α β₂ ≤ PiHerd α β₁ := by
  have d1 := den_pos hα1 h0 (h12.trans h2)
  have d2 := den_pos hα1 (h0.trans h12) h2
  unfold PiHerd
  rw [div_le_div_iff₀ d2 d1]
  have e : (1 - α) * (1 - β₁) * (1 - α * (1 - β₂)) - (1 - α) * (1 - β₂) * (1 - α * (1 - β₁))
      = (1 - α) * (β₂ - β₁) := by ring
  nlinarith

/-- `Π` is decreasing in `α` (p.808). -/
theorem Pi_antitone_alpha {α₁ α₂ β : ℝ} (h12 : α₁ ≤ α₂) (h2 : α₂ < 1)
    (hβ0 : 0 ≤ β) (hβ1 : β ≤ 1) : PiHerd α₂ β ≤ PiHerd α₁ β := by
  have d1 := den_pos (h12.trans_lt h2) hβ0 hβ1
  have d2 := den_pos h2 hβ0 hβ1
  unfold PiHerd
  rw [div_le_div_iff₀ d2 d1]
  have e : (1 - α₁) * (1 - β) * (1 - α₂ * (1 - β)) - (1 - α₂) * (1 - β) * (1 - α₁ * (1 - β))
      = (1 - β) * β * (α₂ - α₁) := by ring
  have : 0 ≤ (1 - β) * β * (α₂ - α₁) := by
    have : 0 ≤ 1 - β := by linarith
    have : 0 ≤ α₂ - α₁ := by linarith
    positivity
  nlinarith

/-- At `β = 0` the herd probability is `1`; by `Pi_antitone_beta` it is as close to
`1` as we like for `β` small (p.800, p.808). -/
theorem Pi_beta_zero {α : ℝ} (hα1 : α < 1) : PiHerd α 0 = 1 := by
  unfold PiHerd
  have : (1 : ℝ) - α ≠ 0 := by linarith
  simp [this]

/-- The contrast (p.800, item 2): if agents chose without observing each other,
the probability that no one is right would be `(1-αβ)^N → 0`. -/
theorem independent_tendsto_zero {α β : ℝ} (hα0 : 0 < α) (hα1 : α ≤ 1) (hβ0 : 0 < β) (hβ1 : β ≤ 1) :
    Filter.Tendsto (fun N : ℕ => (1 - α * β) ^ N) Filter.atTop (nhds 0) :=
  tendsto_pow_atTop_nhds_zero_of_lt_one (by nlinarith) (by nlinarith [mul_pos hα0 hβ0])

/-! ## The `D*` expression (p.810) -/

/-- **The p.810 expression is a binomial upper tail.**  With `t = αβ` and `m = n-1`
observers, `1 - (1-t)^m - m (1-t)^{m-1} t = Σ_{j ≥ 2} C(m,j) t^j (1-t)^{m-j}`, the
probability that *at least two* have received the true signal.  Stated for
`m = k + 1 ≥ 1`. -/
theorem dstar_formula (t : ℝ) (k : ℕ) :
    ∑ j ∈ Finset.range k, (k + 1).choose (j + 2) * t ^ (j + 2) * (1 - t) ^ (k + 1 - (j + 2))
      = 1 - (1 - t) ^ (k + 1) - (k + 1) * (1 - t) ^ k * t := by
  have h := add_pow t (1 - t) (k + 1)
  rw [add_sub_cancel, one_pow] at h
  rw [Finset.sum_range_succ', Finset.sum_range_succ'] at h
  have e : ∑ j ∈ Finset.range k, t ^ (j + 1 + 1) * (1 - t) ^ (k + 1 - (j + 1 + 1)) *
      ((k + 1).choose (j + 1 + 1) : ℝ)
      = ∑ j ∈ Finset.range k, (k + 1).choose (j + 2) * t ^ (j + 2) * (1 - t) ^ (k + 1 - (j + 2)) := by
    apply Finset.sum_congr rfl; intro j _; ring
  rw [e] at h
  simp only [pow_zero, one_mul, Nat.choose_zero_right, Nat.cast_one, mul_one, zero_add, pow_one,
    Nat.choose_one_right, Nat.sub_zero, Nat.add_sub_cancel] at h
  push_cast at h
  have e2 : ∀ j : ℕ, k + 1 - (j + 2) = k - (j + 1) := by intro j; omega
  simp only [e2]
  linarith

end Literature.Banerjee
