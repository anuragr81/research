# Notes — Fullerton & McAfee (1999), pass 1 (full read and Lean)

Run from the repository root `python3 lit/fullerton_mcafee_1999/verify_fm.py`.
The suite measures `lean/mathlib/FullertonMcAfee.lean`, which uses Mathlib
because the claims need real analysis. It follows the Mathlib audit rule of
`checks/verify_mathlib.py`, which allows `Classical.choice` as well as
`propext` and `Quot.sound`.

**Scope.** Our reading of the source is consistent with it, and the parts
listed in §1 are proved in Lean. Theorem 1 is proved from the primitives,
existence and uniqueness both, which is more than the appendix sketch gives.
Theorems 5 and 6, Lemma 3 beyond two entrants, Lemma A1 and the bidding
derivations of eqs. (6) to (12) are not formalised.

## 1. What is proved

| Claim | Lean | What the proof covers |
|---|---|---|
| FM-1, eq. (1) | `win_prob` | `∫_0^1 z_i t^{Z−1} dt = z_i/Z`, the integrand of eq. (1) after the probability-integral transform |
| FM-2, Theorem 1 | `zstar_isNash`, `nash_char`, `nash_unique`, `active_prefix`, `profit_zstar`, `all_active` | every Nash equilibrium of the effort subgame has `z_i = (P/t)(1 − c_i/t)^+` for the unique `t > 0` with `Σ_i (t − c_i)^+ = t`, and this `z` is one. The active firms are the lowest-cost ones, and profits are eq. (4) |
| FM-3, Lemma 1 | `lemma1_deviation`, `lemma1_bound`, `lemma1_case_out`, `lemma1_exact` | the algebra of both cases of the proof, and the example on p.580 in which the bound binds |
| FM-4, Theorem 2 | `thm2_iff`, `thm2_step` | the step "if it is unprofitable for firm k to enter, it is unprofitable for firm k + 1" |
| FM-5 | `symmetric_case` | prize, profits and total cost `cZ + mγ` |
| FM-6, Theorem 3 | `TC_formula`, `TC_step` | the cost formula on p.598 and `TC_m ≤ TC_{m+1}` |
| FM-7, Lemma 2 | `lemma2_step`, `lemma2_single_m_fails`, `lemma2_constant_increment`, `lemma2_proportional` | the inductive step, a counterexample to the single-`m` reading, and the two named families |
| FM-8 | `uniform_bid_scale`, `uniform_bid_constant` | with two entrants and uniform costs the bid of eq. (7) does not depend on cost |
| FM-9, Theorem 4 | `thm4_hyp_always`, `thm4_hyp_with_increasing_bid`, `thm4_interior` | the hypothesis as printed, and the reading the proof needs |
| FM-10, Lemma 4 | `lemma4`, `lemma4_sharp` | the lemma, and that its condition cannot be dropped |
| FM-X | `two_sizes` | entry equilibria of sizes 2 and 3 in one economy |

## 2. Discrepancies against the source

1. **Lemma 2 as printed is false for a single `m`** (p.581). The statement
   assumes the cost condition at `m` alone. The proof (p.599) also uses
   `Δ_{m−1} ≤ Δ_m`, which holds when the condition holds at every smaller `m`
   ("by induction"). With costs `(12/5, 42/5, 43/5, 93/10)`, three and four
   firms can both be active, the condition holds at `m = 3`, and
   `Δ_4 < Δ_3` (`lemma2_single_m_fails`). The two families the paper names
   satisfy the condition at every `m` (`lemma2_constant_increment`,
   `lemma2_proportional`), so the paper's use of the lemma is unaffected.
2. **Theorem 4's hypothesis as printed holds for every `Ψ`** (p.586). With
   `w̃ ∈ [w̲, w̄]`, taking `w̃ = w̲` makes "for all `w < w̃`" range over no
   type, so the hypothesis always holds (`thm4_hyp_always`), including for
   a strictly increasing candidate bid (`thm4_hyp_with_increasing_bid`).
   Read literally, the theorem would deny the efficient equilibrium that
   Lemma 3 grants under a decreasing hazard ratio. The proof needs
   `w̃ > w̲`, and with that the candidate bid is not strictly increasing
   (`thm4_interior`). The intent is clear and the statement should read
   `w̃ ∈ (w̲, w̄]`.
3. **Theorem 2's first display is an iff only for a nonnegative bracket**
   (p.597). `γ ≥ P x²` and `1 − √(γ/P) ≤ 1 − x` agree when `x ≥ 0`
   (`thm2_iff`) and can disagree otherwise (`thm2_iff_needs_sign`). In the
   proof `x ≥ 0` holds for an active firm, so the gap is harmless.
4. **Theorem 3's proof says the difference "is positive"** (p.599). It is
   nonnegative. The symmetric case with `γ = 0` gives equality. `TC_step`
   proves `TC_m ≤ TC_{m+1}` under `Δ_m ≤ Δ_{m+1}`, `Δ_m ≥ 1` and
   `Δ_{m+1} ≤ (m+1)/m`, the last two of which the paper notes
   ("`1 ≤ Δ_m ≤ m/(m − 1)`").
5. **Timing for Lemma 1 and Theorem 2.** Section II says costs become common
   knowledge after the entry decision (p.578), while both results condition
   entry on rivals' costs. They hold under complete information at the entry
   stage, which the paper does not state.
6. **Lemma 1's case split** (p.597) asserts which firms stay active after a
   deviation. `lemma1_deviation` and `lemma1_case_out` prove the algebra of
   each case given that assertion, and `all_active` can check it in any given
   economy, which is how `two_sizes` uses it.
7. The appendix heading "Deviation of Equation (7)" (p.599) is a misprint for
   "Derivation".

## 3. What the paper means for our claims

1. **The count is not invariant in their entry game.** With costs
   `(2.1, 2.3, 2.5, 2.6)`, prize `1` and fixed cost `γ = 19/250`, the sets
   `{2.1, 2.3}` and `{2.1, 2.5, 2.6}` are both entry equilibria in the sense
   of p.579 (`two_sizes`). An entrant's profit in their eq. (4) depends on its
   rivals' costs, so the gain is not anonymous. Our count invariance (M6)
   rests on the gain depending on the count alone. Their model is therefore
   an exact external witness that anonymity carries the result, which the
   novelty ledger records as a condition (A1) and the E6 control already
   showed numerically.
2. **Entrant identity is a property of types there.** The efficient entry
   equilibrium admits the `m` lowest-cost firms (Theorem 2), and the
   contestant selection auction admits the `m` best types (Theorem 5).
   `LITERATURE.tex` said that if this held, "differs in kind" would narrow.
   It holds, so the claim narrows. What remains different is the sorting
   variable. Their types enter the contest itself (research cost or quality),
   while our wealth enters only the utility cost of a fixed fee, `κ(w)`, and
   the contest is anonymous in wealth.
3. **Non-assortative equilibria with close types are theirs.** Lemma 1 shows
   an excluded firm can be cheaper than an entrant only by a factor of at
   least `(m² − m)/(m² − m + 1)`. Our M7 characterises when identities are
   pinned (`Δ(k*−1) < κ_{k*}`). Their lemma is a precedent for identity
   multiplicity under heterogeneous types, and the novelty ledger's identity
   rows should cite it.
4. **The prefix structure is shared.** Their Theorem 1 gives a lowest-cost
   prefix of active firms with a threshold `t = P/Z` solving
   `Σ (t − c_i)^+ = t`. Our P5 (M5) gives a prefix of entrants with a
   threshold `Δ(j)`. Theirs is an effort subgame with a threshold that depends
   on the active firms' costs, and ours is an entry decision against a gain
   that does not.
5. **The weakest entrant.** Their Theorem 4 turns on the profit of the
   weakest entrant, `Ψ(w, w)`, and customary auctions fail to sort when it
   does not rise with type. In our model the marginal entrant's gain
   `Δ(k*−1)` does not depend on wealth at all, and sorting runs through
   `κ`. A fixed fee, which they reject as informationally demanding for a
   sponsor (p.593), is the primitive of our model, where no sponsor chooses
   it.
6. **The draws device is shared.** Their eq. (1) obtains a ratio-form win
   probability from independent draws, the device behind our step identity
   (exactly one of several independent draws is the largest).
7. **Two contestants are optimal for a designer.** Theorem 3 is a sponsor's
   cost minimisation, like Fu and Lu's Theorem 1, and does not bear on P7's
   cap, which is an equilibrium bound with no designer.
