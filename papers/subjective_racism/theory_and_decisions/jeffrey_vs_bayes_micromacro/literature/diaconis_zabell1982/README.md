# Diaconis, P. and Zabell, S.L. (1982), "Updating Subjective Probability"

*Journal of the American Statistical Association* 77(380), 822-830.

The manuscript cites this paper in the body text: at MS ~78 (with Hawthorne, on
commutation), at MS ~117 (Jeffrey's rule as the revision that holds conditionals
fixed, and as the minimal change consistent with the delivered marginal), and at
MS ~190-195 (the invariance condition as sufficiency, and minimal revision in
I-divergence). (An earlier version of this note pointed to a "footnote at line ~81";
that location is stale.) Hawthorne (2004) restates D-Z's Theorem 3.2 as his
"Diaconis-Zabell 3.2 condition".

Page numbers below are journal pages. All ten PDF pages were read, with every
equation checked on the rendered page (the two-column text layer is interleaved).

## Lean

`lean/DiaconisZabell.lean` is a symlink to `lean/Literature/DiaconisZabell.lean`
(namespace `Literature.DiaconisZabell`, standalone, `import Mathlib` only). It
builds with no `sorry`, and every theorem depends only on `propext`,
`Classical.choice` and `Quot.sound`. Setting: a finite space with a full-support
prior `P`; a partition is a labelling `e : Ω → ι`; `jeffrey P e p` is D-Z (1.1) in
the pointwise form (2.2).

| Theorem | What it establishes | D-Z |
|---|---|---|
| `marg_jeffrey` | Jeffrey's update has the target marginal, `P*(Eᵢ) = pᵢ` | (1.1) |
| `jeffrey_jcond` | rigidity: `P*(A \| Eᵢ) = P(A \| Eᵢ)` for all `A`, `i` | (J), p. 823 |
| `jeffrey_mass` | `P*(A) = Σᵢ P(A \| Eᵢ) P*(Eᵢ)` | (1.1) |
| `jcond_iff_jeffrey`, `eq_jeffrey_of_jcond` | (J) holds iff `P*` is the Jeffrey update of `P` to `P*`'s own cell probabilities; so, *given (J)*, the delivered marginal determines the posterior | (J) + (1.1) |
| `thm21`, `thm21_finite` | `P*` can be obtained from `P` by conditioning (finite enlarged space) iff `P* ≤ B P` for some `B ≥ 1`; always, for a full-support finite `P` | Thm 2.1, (2.1), p. 824 |
| `thm22_factorization` | under (J), `P*(ω) = P*(Eᵢ)/P(Eᵢ) · P(ω)` on `Eᵢ` | Thm 2.2, (2.2) |
| `jcond_iff_ratio_const` | (J) iff the likelihood ratio `P*/P` is constant on every cell | Thm 2.2 |
| `thm22_lr_sufficient`, `thm22_lr_minimal` | the level sets of `P*/P` satisfy (J), and every (J)-partition refines them: the likelihood-ratio partition is the minimal sufficient partition *for the pair* `{P, P*}` | Thm 2.2, second statement |
| `jcond_of_refines`, `jcond_atoms`, `jcond_trivial_self` | (J)-partitions are not unique: refinements of one are too, the atoms always are, and when `P* = P` the one-cell partition is | p. 824 |
| `lr_jeffrey_iff` | the cue's own partition is the minimal sufficient one exactly when the ratios `pᵢ/P(Eᵢ)` differ across cells (binary cue: `p ≠ P(E)`) | Thm 2.2 |
| `fdiv_jeffrey`, `thm61_le`, `thm61_unique` | for every convex `f`, Jeffrey's `P*` minimizes `I_f(Q,P) = Σ P f(Q/P)` over `Q` with the target marginal, with value `Σᵢ P(Eᵢ) f(pᵢ/P(Eᵢ))`; for strictly convex `f` it is the unique minimizer | Thm 6.1 (finite case), p. 829 |
| `thm51_KL_le`, `thm51_KL_eq_iff` | `I(Q,P) = Σ Q log(Q/P) ≥ Σᵢ pᵢ log(pᵢ/P(Eᵢ))`, equality iff `Q = P*` | Thm 5.1, (5.6), p. 828 |
| `thm51_hellinger_le`, `thm51_hellinger_eq_iff` | `H(Q,P) = Σ(√Q - √P)² ≥ Σᵢ(√pᵢ - √P(Eᵢ))²`, equality iff `Q = P*` | Thm 5.1, (5.5) |
| `thm51_tv_le`, `tv_jeffrey`, `remark_a_tv_not_unique` | variation distance `≥ ½Σᵢ\|pᵢ - P(Eᵢ)\|`, attained by `P*`, but also by a different `Q` (explicit 3-point example) | Thm 5.1 (5.4), Remark (a) |
| `example51` | `Pᴵ = (1/9,2/9,2/9,4/9)` (odds ratio 1) has variation distance `7/36`; the minimum over tables with those margins is `1/6`, at `Pⱽ = (1/12,1/4,1/4,5/12)` | Ex. 5.1, §5.3, pp. 828-829 |
| `thm32` | `P_𝓔𝓕 = P_𝓕𝓔` iff Jeffrey independence, for finite partitions of any size, by D-Z's direct algebra (3.5)-(3.6), **assuming every `Eᵢ ∩ Fⱼ` is nonempty** | Thm 3.2, p. 825 |
| `thm32_needs_qualitative_independence` | without that assumption the forward direction is false (`𝓔 = 𝓕`, `p = q ≠ P(E)`) | new |
| `thm33_forward` | P-independence implies J-independence for all targets | Thm 3.3, (3.7) |
| `marg_jE_snd`, `marg_jF_fst` | on the `2×2` prior `(α, β, c)`: updating `E` to `p` moves `P(F)` by `(p-α)c/(α(1-α))` | |
| `remark826` | J-independence iff `(p = α ∨ c = 0) ∧ (q = β ∨ c = 0)` | Remark, p. 826 |
| `thm32_2x2`, `commute_of_indep`, `not_commute_of_corr` | the two orders agree iff that condition holds; at `c = 0` for all targets; at `c ≠ 0`, `p ≠ α` they differ | Thm 3.2 + Remark |
| `example32`, `example33`, `example34` | Ex. 3.2 (`.56, .24, .14, .06`, order irrelevant); Ex. 3.3 (J-independent for all `p, q`, not P-independent); Ex. 3.4 (`P_𝓔𝓕(E) = 1/2`, `P_𝓕𝓔(F) = 371/851 ≠ 7/15`) | pp. 825-826 |
| `witness_gap` | the project's own witness `-1032/369935` | Paper B, not D-Z |

Not formalized: Theorem 3.1 (D-Z omit its proof; it is "an immediate consequence of
Csiszár (1975, Theorem 3.2)", p. 825), the converse of Theorem 3.3, Section 4
(Theorem 4.1 via Strassen), Theorem 5.2 and the IPFP, and Section 6 on general
measure spaces.

## SymPy

`sympy/check_theorem32.py` asserts every claim below and exits 0 iff all pass
(15/15). Example 3.4: `P_𝓔𝓕(E) = 1/2`, `P_𝓔𝓕(F) = 7/15`, `P_𝓕𝓔(E) = 1/2`,
`P_𝓕𝓔(F) = 371/851 ≠ 7/15`. Example 3.2's numbers and order-invariance. On the
prior `(α, β, c)`: the marginal shifts `(p-α)c/(α(1-α))` and `(q-β)c/(β(1-β))`; at
`c = 0` the orders agree identically; at `c ≠ 0` the gap is not identically zero,
witness `-1032/369935` at `(α, β, c, p, q) = (.3, .55, .05, .4, .6)` (a Paper B
computation, not a number from D-Z); trivial targets commute. The `𝓔 = 𝓕`
counterexample to Theorem 3.2 without qualitative independence.

## What the paper says, in its own terms

- **Four routes, one of them Jeffrey's** (pp. 822-823): complete reassessment,
  retrospective conditioning, exchangeability, Jeffrey's rule. Jeffrey's rule "is
  valid whenever there is a partition `{Eᵢ}` … such that (J)"; approaches 2-4 "are
  all special routes to the requantification of approach 1; each is valid or useful
  under different assumptions".
- **Coherence** in D-Z (§4.1, p. 827) means that degrees of belief on two partitions
  extend to one probability measure (Theorem 4.1). It is not a property that singles
  out Jeffrey's rule.
- **Sufficiency** (§2.2, p. 824): finding a (J)-partition "is simply the problem of
  finding a *sufficient* partition for the two-element family `{P, P*}`"; "a coarsest
  sufficient partition is said to be minimal sufficient"; Theorem 2.2 identifies it
  as the likelihood-ratio partition.
- **Full adoption is their setting, not their premise.** §3.1 sets up successive
  updating with each step a Jeffrey step to its target: "Clearly, the order of
  updating matters, since the second opinion dominates" (p. 825).
- **Scope disclaimer** (§3.2, p. 825): "The J condition is an internal or
  psychological condition that must be checked or accepted at each stage.
  Mathematics has nothing to offer here." Pairs with Hawthorne p. 115.
- **Theorem 3.2's proof** is direct algebra, (3.5)-(3.6), pp. 825-826. Csiszár
  (1975) is the omitted proof of Theorem 3.1. (An earlier version of this note said
  Theorem 3.2 was proved via Csiszár; that was wrong.) The `c = 0` reading on the
  `2×2` table rests on the Remark on p. 826: if one partition has two elements,
  J-independence for some `p, q` is equivalent to P-independence. That holds for
  nontrivial targets; `remark826` gives the exact condition.
- **Theorem 3.2 as printed has a gap.** It is stated with no hypothesis beyond
  `P(Eᵢ) > 0`, `P(Fⱼ) > 0`, but the proof chooses `A = E_{i₀}F_{j₀}` and cancels
  `P(A Eᵢ Fⱼ)`, which is `0` when `E_{i₀} ∩ F_{j₀} = ∅`. With `𝓔 = 𝓕` (their own
  Example 3.1) and `p = q ≠ P(E)` the two orders agree but the partitions are not
  Jeffrey independent. The theorem holds when every `Eᵢ ∩ Fⱼ` has positive
  probability, which is automatic for Paper B's `2×2` full-support prior.
- **Section 4.2** (p. 827) attributes the successive route to Jeffrey (1957, Ch. 4)
  and lists two issues, "1. When does successive updating satisfy (4.1)? 2. When is
  successive updating reasonable?". The second is not left open: D-Z continue "One
  approach to this is via checking the Jeffrey condition at each stage", and add that
  examples such as Example 3.4 "show that this can be tricky".
- **Remarks 1-2** (p. 827, prose, recorded here only). Remark 1: "There is no reason
  to require `P_𝓔𝓕 = P_𝓕𝓔` for successive updating to be useful and valid." Remark 2:
  condition (4.3) implies that `P_𝓔𝓕` and `P_𝓕𝓔` "cannot both incorporate (4.1) and
  both be judged acceptable updates (in the sense that the (J) conditions have been
  checked) without `P_𝓔𝓕 = P_𝓕𝓔`. Thus noncommutativity is not a real problem for
  successive Jeffrey updating."
- **Both-margin alternative.** Section 4 (simultaneous adoption of both targets,
  existence by Strassen) and §5.2 (the I-projection, computed by IPFP). Example 5.1 is
  in **§5.3** "Comparing Different Metrics" (not §5.2 as an earlier version of this
  note said); it notes that "I projections preserve the association factor of a 2 × 2
  table (see, e.g., Mosteller 1968, p. 3)", the odds-ratio invariance of Lemma SEP,
  stated for the both-margin fit. D-Z draw the moral: "any claims to the effect that
  maximum-entropy revision is the only correct route to probability revision should
  be viewed with considerable caution because of its strong dependence on the measure
  of closeness being used" (p. 829).
- **Mechanical updating** (§5, pp. 827-828) is D-Z's name for the minimum-distance
  justification. They do not present it as an axiomatization.

## What the manuscript may and may not attribute to D-Z

From the Lean record above.

**May attribute:**
1. Jeffrey's rule holds the conditionals given the cue's partition fixed (rigidity,
   condition (J)), and under (J) the delivered marginal determines the posterior
   (`jeffrey_jcond`, `jcond_iff_jeffrey`; D-Z (1.1), (J), Theorem 2.2 (2.2)).
2. Among all distributions with the delivered marginal, Jeffrey's posterior is the
   **unique** minimizer of the Kullback-Leibler divergence `I(Q,P) = Σ Q log(Q/P)`
   of the revised `Q` from the prior `P` (posterior first, prior second), and of the
   Hellinger distance; more generally of every `f`-divergence `I_f(Q,P)` with `f`
   strictly convex (`thm51_KL_eq_iff`, `thm51_hellinger_eq_iff`, `thm61_unique`;
   D-Z Theorems 5.1, 6.1). It also minimizes the variation distance, but **not
   uniquely** (`remark_a_tv_not_unique`; Remark (a), p. 828).
3. Condition (J) says that the cue's partition is **sufficient for the pair**
   `{P, P*}`; the minimal sufficient partition is the likelihood-ratio partition
   (Theorem 2.2). For a binary cue whose delivered credence differs from the prior
   marginal, that is the cue's own partition (`lr_jeffrey_iff`).
4. Two successive Jeffrey updates commute iff the partitions are Jeffrey independent
   (Theorem 3.2, under qualitative independence, which holds in the paper's
   full-support `2×2` setting); for two binary cues with nontrivial targets this is
   `c = 0` (`thm32_2x2`, Remark p. 826).
5. D-Z hold that noncommutativity "is not a real problem" (Remarks 1-2, p. 827).

**May not attribute:**
1. "The unique coherent revision" (MS ~117). D-Z prove no coherence or Dutch-book
   uniqueness. Their uniqueness results are metric (strictly convex `f`) or come from
   (J) itself, and they present Jeffrey's rule as one of four legitimate routes.
2. "Equivalent to making the minimal change" without naming the distance. The
   minimizer is unique for KL and Hellinger but not for variation distance, and
   Example 5.1 shows the answer depends on the metric.
3. "The partition satisfying the invariance condition is **the minimal sufficient
   statistic** for revising the prior to **any** candidate posterior" (MS ~190-193).
   Sufficiency is relative to one pair `{P, P*}`. Many partitions satisfy (J): every
   refinement of one does (`jcond_of_refines`). The minimal one is the
   likelihood-ratio partition, which is the cue partition only when the delivered
   credence differs from the prior marginal; when it equals it, the one-cell
   partition is sufficient (`jcond_trivial_self`).
4. "Axiomatic grounding" (MS ~191). D-Z call Section 5 *mechanical updating* and give
   no axioms.
5. That D-Z treat order dependence as "a defect to repair" (MS ~78-79; audit M24).

**Corrected wording (text only; the manuscript is not edited here).**

MS ~117, replacing "As a mechanism that holds conditionals fixed, Jeffrey updating is
the unique coherent revision for credence delivered on cue's partition and is
equivalent to making the minimal change of the prior consistent with the delivered
marginal \citep{DiaconisZabell1982}":

> Jeffrey updating holds fixed the conditional probabilities given the cue's
> partition, and under that condition the delivered marginal determines the
> posterior; among all distributions with the delivered marginal, the Jeffrey
> posterior is the unique one closest to the prior in Kullback-Leibler divergence
> (and in Hellinger distance) \citep[Theorems 5.1 and 6.1]{DiaconisZabell1982}.

MS ~190-195, replacing "The axiomatic grounding is twofold. The partition satisfying
the invariance condition is the minimal sufficient statistic for revising the prior
to any candidate posterior on that partition \citep{DiaconisZabell1982}, and the rule
is the minimal revision of the prior consistent with the delivered marginal, in the
$I$-divergence sense \citep{DiaconisZabell1982}.":

> Two results of \citet{DiaconisZabell1982} support the rule. The invariance
> condition says that the cue's partition is sufficient for the pair formed by the
> prior and the posterior; for a binary cue whose delivered credence differs from
> the prior marginal it is the minimal sufficient partition, the partition by the
> likelihood ratio of posterior to prior (their Section 2.2 and Theorem 2.2). And the
> rule is the unique minimiser of the $I$-divergence
> $I(Q,P)=\sum_{\omega}Q(\omega)\log\{Q(\omega)/P(\omega)\}$ of a revised $Q$ from the
> prior $P$ among all $Q$ with the delivered marginal (their Theorem 5.1), the
> justification they call mechanical updating.

## Bearing on Paper B

Example 3.4 is the 1982 precedent for what the manuscript calls "pinning": the last
update fixes its own marginal by construction, and whether the earlier one survives
depends on the route (in Example 3.4 it survives for `𝓔`-then-`𝓕` and fails for
`𝓕`-then-`𝓔`). Paper B's Proposition DRF is the general (all `α, β, c, q, r`) form of
the phenomenon their Example 3.4 witnesses at one point. The `c = 0` commutation
condition is D-Z's own (Theorem 3.2 with the Remark on p. 826), so it can be cited
directly rather than only via Hawthorne's restatement. D-Z are on the paper's side on
order dependence (Remarks 1-2), so they should not be grouped with Hawthorne as
treating it as a defect.
