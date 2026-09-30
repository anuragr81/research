# Wilson, A. (2014), "Bounded Memory and Biases in Information Processing"

*Econometrica* 82(6), 2014.

**Source.** The Drive copy (`AndreaWilson.pdf`, here `wilson.pdf`) is the
Princeton working-paper **draft dated April 29, 2003**, 55 pages (main text
pp. 1-31, appendix pp. 31-55). It is not the published paper. **Every theorem,
lemma, corollary, equation and page number below is the draft's.** The
published version was not available, so its numbering, statements and constants
are unchecked. Read in full on 2026-09-30 (the Claims 0-4 algebra of A.4 at a
skim); formulas checked on the rendered pages 12, 21 and 22.

## Claims formalized

The model (Section 2, pp.7-10):

* State `S ∈ {L, H}`, equally likely; i.i.d. binary signals with
  `Pr(l|L) = Pr(h|H) = ρ > 1/2`; before each signal the problem ends with
  probability `η` and the DM acts, earning 1 if the action matches the state.
* Memory `𝒩 = {1, …, N}` (`N` odd, fn 8); a memory process `(g₀, σ, a)` with
  `σ(i, s)(j)` the transition rule; `τ^H_{ij} = (1-ρ)σ(i,l)(j) + ρσ(i,h)(j)`
  and `τ^L_{ij} = ρσ(i,l)(j) + (1-ρ)σ(i,h)(j)`.
* The ending distribution `f^S = ∑_t η(1-η)^t g₀(T^S)^t` (eq. (1)), the payoff
  `Π = ½∑ᵢ(f^H_i a(i) + f^L_i(1-a(i)))`, beliefs `π(i) = f^H_i/(f^H_i + f^L_i)`.

Results used (draft numbering):

* **Theorem 3** (p.12): for `N ≥ 3` and small `η`, (i)
  `1 > τ̂^S_{11} > 1-ε`, `1 > τ̂^S_{NN} > 1-ε`; (ii) interior states move up
  after `h` and down after `l` with probability 1; (iii) start in the middle;
  (iv) act `L` below the middle and `H` above.
* **Corollary** (pp.12-13): (i) `(1+((1-ρ)/ρ)^{N-1})⁻¹ - ε < Π*(η,N) <
  (1+((1-ρ)/ρ)^{N-1})⁻¹`; (ii) `f^H_i/f^L_i → (ρ/(1-ρ))^{i-1}((1-ρ)/ρ)^{N-i}`.
  With absorbing extremes the payoff is the gambler's-ruin value
  `(1+((1-ρ)/ρ)^{(N-1)/2})⁻¹`, "as if there were only `(N-1)/2` memory states"
  (p.12).
* **Eq. (4)** (p.15), the three-state sketch:
  `f^H_3/f^L_3 = (τ^H_{23}/τ^L_{23})((η+(1-η)τ^L_{32})/(η+(1-η)τ^H_{32}))(f^H_2/f^L_2)`,
  and **`α* = 1`** (p.16).
* **Theorem 4** (pp.18-19): (i) first impressions matter in the short run;
  (ii) last impressions in the long run.
* **Theorem 5** (p.22): (i) "If `1 < j < k < N` and `t ≥ N - 1 - (k - j)`, then
  `Pr{i^j_t < j < k < i^k_t} > 0`"; (ii) monotonicity in `j`, `N - k`.
* **Theorem 6** (p.24): overconfidence after short or weak sequences,
  underconfidence when `δ > N - 1`; in the proof, one memory step moves beliefs
  "as if he had received two h-signals".
* **Theorem 7** (p.26): with `K` signals, for small `η` all but the two most
  extreme signals are ignored.
* **Lemma 1** (p.36): `f^S` exists, is unique, and is the stationary
  distribution with `f^S = ηg₀ + (1-η)f^S T^S` (first line of (A8)).
* **Lemma 4** (pp.39-40): `½(1/(1+x) + 1/(1+r²/x)) ≤ 1/(1+r)`, equality iff
  `x = r`, by the identity
  `(1+r)(1+x)(1+r²/x)[2/(1+r) - …] = ((1-r)/x)(x-r)²`.

## Result

`lean/Wilson.lean` is a symlink to `lean/Literature/Wilson.lean`. It is
standalone on Mathlib, checks with `lake env lean`, and contains no `sorry`. Its
85 theorems use only `[propext, Classical.choice, Quot.sound]` or a subset (the
finite checks use `decide`, not `native_decide`). The header of the Lean file
lists what is and is not formalized.

| Lean theorem | Content |
|---|---|
| `trans_H`, `trans_L`, `trans_rowsum` | the model's `τ^S`; rows of `T^S` sum to 1 |
| `endDist_exists`, `endDist_unique`, `homog_zero`, `endDist_sum` | **Lemma 1** for `0 < η ≤ 1` |
| `trans_ratio_up`, `trans_ratio_down`, `trans_ratio_up_eq_iff` | one move changes the likelihood ratio by at most `ρ/(1-ρ)`, with equality iff it is never made on the opposing signal (p.15) |
| `eq4_balance`, `eq4` | **eq. (4)** for every 3-state rule without 1↔3 jumps and `g₀(3) = 0` |
| `eq4_bound` | from state 2 to 3 the likelihood ratio grows by at most `(ρ/(1-ρ))²`, for every such rule |
| `rule3`, `rule3_endDist`, `rule3_payoff_endDist` | the Theorem 3 rule at `N = 3` with leaving probability `γ`: ending distribution `f = (uqA, AB, upB)/Δ` and payoff `Pi3`, from the model's own definitions |
| `rule3_optimal`, `gstar_quadratic`, `gstar_sq_bounds` | **Theorem 3 (i) at `N = 3` within the family**: the unique optimal `γ*(η) = (√(2η-η²)-η)/(1-η)`, with `η ≤ γ*² ≤ 2η` |
| `rule3_gap`, `rule3_payoff_lt_beta`, `corollary_i_N3` | **Corollary (i) at `N = 3`**: exact gap to `(1+((1-ρ)/ρ)²)⁻¹`; the family never reaches it, and `γ*(η)` comes within any `ε` (the lower half of Corollary (i) for `Π*`) |
| `rule3_absorbing`, `gamblersRuin_eq`, `absorbing_lt_beta` | absorbing extremes: `Π = ρ - η(ρ - ½) → ρ`, the gambler's-ruin value, strictly below the bound |
| `rule3_mid_equal`, `rule3_mirror`, `rule3_lr3`, `rule3_lr3_gap`, `corollary_ii_N3` | **Corollary (ii) at `N = 3`**: `f^H_2 = f^L_2`, mirror symmetry, and `f^H_3/f^L_3 → (ρ/(1-ρ))²` along `γ*` |
| `lemma4_identity`, `lemma4`, `lemma4_eq_iff`, `alpha_star_one` | **Lemma 4** and the p.16 `α* = 1` |
| `lrLimit_step`, `lrLimit_top`, `lrLimit_N3`, `thm6_i`, `thm6_iii` | **Theorem 6 (i), (iii)** from Corollary (ii)'s beliefs: "as if two h-signals" |
| `move_ratio_le`, `move_ratio_lt` | **Theorem 7 (ii)**, informational core: a move made on a weaker signal is strictly less informative |
| `skel`, `run`, `first_impressions_pathwise`, `first_impressions_strict` | **Theorem 4 (i)**, the pathwise step of the proof (p.19), for every `N`, start, block length and sequence |
| `section4_example`, `intro_example` | the polarization examples of p.21 and p.6 |
| `reach_sound`, `thm5_draft_counterexample`, `thm5_draft_counterexample_N7` | **Theorem 5 (i) as stated fails** |
| `posReach_run`, `thm5_i_corrected`, `thm5_threshold_N5` | Theorem 5 (i) with the length its proof needs, and the exact threshold at `N = 5` |
| `assoc_sign_fixed`, `marginals_do_not_fix_association`, `kernel_invariant`, `y_dependent_signal_moves_association` | **project question** (below) |

`sympy/check_wilson.py` (all PASS, exit 0) checks:

* the `N = 3` closed form against the stationary equations, the matrix inverse
  and 400 terms of the series (1);
* eq. (4), the payoff, the exact gap, the payoff-difference formula, `γ*` and a
  grid search for the optimal `γ`;
* **numerically, beyond the Lean proofs**, the Theorem 3 rule at `N = 5` and
  `N = 7` with `γ = √η`, `η = 10⁻⁹`: payoff `0.96735` vs Corollary (i)
  `0.96737` (`N = 5`, `ρ = .7`) and `0.99383` vs `0.99384` (`N = 7`); beliefs
  within `5·10⁻³` in log-odds of Corollary (ii); absorbing extremes give the
  gambler's-ruin value;
* Lemma 4 and `α* = 1`, Theorem 6, the skeleton claims (Theorem 4 (i) pathwise,
  exhaustive for `N ∈ {3,5,7}`: 0 violations, 2608 strict cases), the p.21 and
  p.6 examples, and the Theorem 5 thresholds at `N = 5, 7, 9`;
* the project-question identities.

## What formalizing revealed

1. **Theorem 5 (i) is false as stated in the draft.** With `N = 5`, `j = 2`,
   `k = 4` the hypothesis `t ≥ N - 1 - (k - j) = 2` holds at `t = 2`, yet no
   two-signal sequence can polarize the agents: an interior state moves by one
   on every signal (Theorem 3 (ii)), so from 2 and 4 the only way to end with
   `i^2 < 2` and `i^4 > 4` takes three signals. This holds for every kernel
   satisfying Theorem 3 (ii), whatever the extremes do
   (`thm5_draft_counterexample`). Other failures: `(N,j,k,t) = (7,2,6,2)`,
   `(7,3,5,4)`, `(9,2,8,2)`, `(9,3,7,4)`. The proof's own sequence (`j-1` low
   signals, then `j-1+N-k` high) needs `t ≥ 2j - 2 + N - k`, and with that
   hypothesis the statement holds (`thm5_i_corrected`). On the skeleton the
   exact threshold is `min(2j-1, 2(N-k)+1)` (checked for `N = 5, 7, 9`; proved
   at `N = 5`). The published version may have corrected this.
2. **The p.21 example has a slip.** "Suppose that `τ = 4`, so there are
   `C(4,2) = 6` possible orderings": with `τ` reports of each type, six
   orderings means `τ = 2`. The six outcomes stated in the text are all
   correct (`section4_example`).
3. **At `N = 3` Theorem 3 (i) has a closed form within its family.** Among
   rules with the structure of Theorem 3 (ii)-(iv) and a common leaving
   probability `γ`, the payoff is maximized exactly at
   `γ*(η) = (√(2η-η²)-η)/(1-η)`, which does not depend on `ρ` and satisfies
   `√η ≤ γ* ≤ √(2η)`. This is the paper's "positive probability `γ` which goes
   to zero as `η → 0`, but at a much slower rate" (p.5), made exact. Optimality
   against all three-state rules is not proved here.
4. **The "two signals" adjustment is a limit.** For every `η > 0` and every
   three-state rule the step from state 2 to 3 moves the likelihood ratio by
   strictly less than `(ρ/(1-ρ))²` (`eq4_bound`; the gap for `rule3` is
   `ρ(2ρ-1)η/((1-ρ)²(η+uγ(1-ρ)))`). Theorem 6 (i)'s overconfidence is a
   statement about the limit beliefs of Corollary (ii).
5. **Minor.** The p.12 gambler's-ruin formula writes `p` for `ρ`; the
   introduction's polarization example (p.6) uses `N = 4` although the model
   assumes `N` odd (fn 8); the Corollary's lower bound is stated for `Π*` and
   follows from any rule that attains it, which is what `corollary_i_N3`
   exhibits.

## Project question (not Wilson's): cross-attribute association

The notes (LOG:200-202, 697-701) say Wilson's agent "holds a single belief and so
makes no prediction about cross-attribute association". Wilson never discusses a
second attribute; the claim is the project's, and it is now proved in the
following sense.

* Each memory state carries one number `π(i)`. If a second attribute `Y` is
  attached through a kernel `κ_S = Pr(Y=1|S)`, then
  `Cov(1{S=H}, Y) = π(1-π)(κ_H - κ_L)` (`assocCov_eq`): its sign is the
  kernel's in every memory state (`assoc_sign_fixed`). Memory can rescale the
  association but not create, remove or reverse it.
* For every `π ∈ (0,1)` and `Y`-marginal `m ∈ (0,1)` there are two kernels with
  the same marginals and associations of opposite sign
  (`marginals_do_not_fix_association`): nothing in the model pins the
  association down.
* **What a two-attribute extension needs.** Putting `Y` into the state (a joint
  law on four atoms) is not enough: updating by signals whose likelihood depends
  on `S` alone, as Wilson's do, keeps every `Pr(Y|S)` fixed
  (`kernel_invariant`), so the association's sign never moves. The extension
  needs signals whose likelihood depends on `Y` given `S` (one such signal
  gives `Cov = ±1/12` from the independent-uniform joint,
  `y_dependent_signal_moves_association`), and memory states indexing beliefs
  on the joint (three free numbers per state instead of one). None of this is
  in Wilson's paper, and the optimal memory for such an extension is not
  derived here.

## Bearing on Paper B

Wilson is a rival *explanation* of order effects, not a rival updating rule
(LOG:122-124). Her model generates order effects rationally: short-run primacy
(Theorem 4 (i)), long-run recency (Theorem 4 (ii)), polarization (Theorem 5) and
over-adjustment in interior states (Theorem 6). It says nothing about
cross-attribute association, for the structural reason proved above. The damped
family of `JeffreyOrder/Anchoring.lean` is a two-attribute stand-in only for the
"ignore information at the extreme states" / short-run primacy part of her
signature: a weight `δ ∈ [0,1]` cannot produce the interior over-adjustment of
Theorem 6 (beliefs move by `(ρ/(1-ρ))²` per signal in the limit) or the long-run
recency of Theorem 4 (ii).

On single-attribute data Theorem 4 (i) predicts short-run **primacy**, so
Wilson's model is a bounded-memory account of Asch-type primacy
(verify_record_papers §5 note), which reinforces the LOG:702-705 caution that
Asch cannot be cited for the amnestic mechanism.

## Audit findings (2026-09-30)

This section answers `notes/citation_audit.md` item L11 and
`notes/citation_audit/verify_record_papers.md` §5.

* **L11 (identity).** Confirmed: the Drive copy is the April 29, 2003 draft,
  not the 2014 Econometrica paper. Every Wilson theorem number in the notes is
  a draft number.
* **§5.1 (NC), cited as "Wilson (2014), Econometrica"** (PLAN:1338-1339,
  LOG:122, 828). Stands: the published version was not read.
* **§5.2, 5.3 (VERIFIED).** Confirmed by Theorem 4 (pp.18-19) and Theorem 1
  (p.10); the pathwise core of Theorem 4 (i) is now proved
  (`first_impressions_pathwise`).
* **§5.4 (VWC), "a single belief, so no cross-attribute prediction".** Now
  proved as a project question (above); the attribution caveat stands: the paper
  never says it.
* **§5.5 (VWC), the "stand-in".** Confirmed and sharpened: the missing half of
  the signature is `thm6_i` (over-adjustment, as a statement about the limit
  beliefs) and Theorem 4 (ii) (not formalized).
* **New: Theorem 5 (i)'s hypothesis is insufficient in the draft** (finding 1),
  and the p.21 `τ = 4` slip (finding 2). Neither affects the notes, which cite
  neither.

### Proposed wording (text only, not applied)

* **PLAN:1337-1338**, replace "A bounded-memory citation (Wilson 2014,
  Econometrica) is available" with:
  > A bounded-memory citation (Wilson 2014, Econometrica; read in the 2003
  > working-paper draft, whose theorem numbers may differ) is available
* **LOG:699-701**, replace "The answer was that Wilson's agent holds a single
  belief and so makes no prediction about cross-attribute association." with:
  > The answer was that Wilson's state is a single binary variable, so each
  > memory state carries one probability; with a second attribute attached by
  > any fixed kernel, the sign of the cross-attribute association is the
  > kernel's in every memory state (`literature/wilson2014`, project
  > question). Wilson herself does not discuss a second attribute.
* **LOG:771-772**, replace "so the damped rule is a two-attribute stand-in for
  her behavioural signature rather than a translation" with:
  > so the damped rule is a two-attribute stand-in for one part of her
  > behavioural signature (information ignored at the extreme states, hence
  > short-run primacy), not for her interior over-adjustment (Theorem 6) or her
  > long-run recency (Theorem 4 (ii)), and not a translation

## Not formalized

* Theorem 1 (optimal ⇒ incentive compatible, Definition 2) and Theorem 2
  (existence; monotonicity of `Π*` in `η` and `N`).
* Theorem 3 for general `N`, and optimality of the `N = 3` family against all
  three-state rules. Hence the **upper** half of Corollary (i),
  `Π*(η,N) < (1+((1-ρ)/ρ)^{N-1})⁻¹`, is not proved (`rule3_payoff_lt_beta` is
  for the family only); nor Lemmas 2, 3, 5-9 or Claims 0-4.
* Corollary (ii) for `N > 3`: Theorem 6 takes the limit beliefs as given
  (`lrLimit`); the sympy script checks them numerically at `N = 5, 7`.
* Theorem 4: the expectation over sequences and the `η`-limit in (i), and all
  of (ii) (ergodic argument). Theorem 5 (ii). Theorem 6 (ii).
* Theorem 7 beyond the one-move bound, and its Corollary (`ρ̃`).
