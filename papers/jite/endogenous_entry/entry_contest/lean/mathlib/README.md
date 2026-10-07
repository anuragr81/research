# Lean with Mathlib, for the analytic steps

This lake project holds the parts of the proofs that need measure theory,
which `../EntryContest.lean` keeps out by design. The core file stays on core
Lean, so its audit admits only `propext` and `Quot.sound`. Files here import
Mathlib, so their audit admits the three standard axioms `propext`,
`Classical.choice` and `Quot.sound` and nothing else (`checks/verify_mathlib.py`,
suite 12 of `verify.sh`). The project also builds `../EntryContest.lean` as a
library, so the theorems here can apply its results directly.

These files are for our verification and are not part of the submission.

## Setup

The project pins Lean `v4.24.0-rc1` (`lean-toolchain`) and Mathlib at commit
`14871d5175e8011a94c78e54b431080aabeeab00` (`lakefile.toml`, with every
dependency fixed in `lake-manifest.json`). From this directory run

    lake exe cache get
    lake build

`cache get` downloads Mathlib's prebuilt files, about 5 GB unpacked, or unpacks
them from `~/.cache/mathlib` when they are already there. Without it, `lake
build` would compile Mathlib from source. `.lake/` is not committed.

## StepIdentity.lean, the step identity in full generality

For probability measures `α`, `β`, `κ` on the reals with no atoms, with
CDFs `F`, `G`, `K`,

    (∫ K F dα − ∫ K F dβ) − (∫ K G dα − ∫ K G dβ) = −½ ∫ (F − G)² dκ.

No densities and no integration by parts are used. Each integral of a product
of two CDFs against a third measure is the probability that one coordinate of
an independent triple is the largest (`EX_eq`, `EY_eq`, `EZ_eq`). With no
atoms, ties have probability zero (`tie_xy_null`, `tie_xz_null`,
`tie_yz_null`), so the three probabilities sum to one (`three_max_sum`). The
identity follows from that sum for the triples `(κ, α, α)`, `(κ, β, β)` and
`(κ, α, β)` (`step_identity`), and its sign from the square (`step_nonpos`).

## EntryContestModel.lean, from the score laws to P5

| Theorem or definition | Content |
|---|---|
| `maxLaw`, `cdf_maxLaw`, `maxLaw_noAtoms` | The law of the larger of two independent scores has CDF equal to the product of the CDFs, and no atoms when neither factor has any |
| `rivals`, `cdf_rivals` | The law of the best rival score, with CDF `C · F^m · G^(Q−1−m)`, which is `H_m` in `PROOFS.tex` |
| `Delta` | `Δ(m) = V (∫ H_m dF − ∫ H_m dG)`, built from the score laws |
| `Delta_step` | `Δ(m+1) − Δ(m) = −(V/2) ∫ (F − G)² dK` with `K = C · F^m · G^(Q−2−m)`, for `m + 2 ≤ Q` |
| `Delta_step_nonpos`, `DeltaUpTo_step` | `Δ` does not increase in `m` (extended as a constant beyond `Q − 1`) |
| `p5_threshold_from_primitives` | `EntryContest.equilibrium_is_threshold` applied to this `Δ`, with its hypothesis `step_nonpos` discharged rather than assumed |

The only assumptions are the model's own. The score laws `α` (investor, `F`),
`β` (non-investor, `G`) and the incumbent's law `C` are probability measures
with no atoms, which is the continuity of `F` and `G` stated in the primitives,
and `V ≥ 0`.

## RepresentationFOSD.lean, P1 and P2

| Theorem or definition | Content |
|---|---|
| `two_max_sum` | For independent draws with no atoms in the second law, `∫ cdf_B dA + ∫ cdf_A dB = 1` (exactly one draw is the larger) |
| `p1_representation`, `Delta_eq_expectation` | P1. `Δ(m) = V ∫ (G − F) dH_m = V E[φ(M_m)]` |
| `investorLaw`, `nonInvestorLaw` | The score laws built from the laws of `r` and `s`, of `μr + (1 − μ)s` and of `μr` |
| `p2_fosd` | P2. `F ≤ G` everywhere, when `s ≥ 0` almost surely and `μ ≤ 1` |
| `p2_strict_point`, `p2_strict_interval` | P2, strict part. When also `μ < 1` and `s > 0` with positive probability, `F < G` on an interval `[q, u)` |
| `Delta_nonneg`, `Delta_nonneg_from_primitives` | Investing never hurts. `Δ(m) ≥ 0` for every `m` |

P2 needs only `μ ≤ 1`. The hypothesis `0 ≤ μ` was in the first draft and the
compiler reported it unused, since the `μr` term is common to both scores.

## SaturationBenchmark.lean, P7 and P6

| Theorem | Content |
|---|---|
| `integral_maxLaw` | `∫ g d(max law) = ∫ g G dF + ∫ g F dG` for bounded measurable `g`, the first law without atoms |
| `integral_iid`, `integral_cdf_pow` | For the maximum of `n + 1` draws from `F`, `∫ g = (n + 1) ∫ g F^n dF`, and hence `∫ F^m dF = 1/(m + 1)` |
| `p7_Delta_le` | P7. `Δ(m) ≤ V/(m + 1)` for every `Q`, every `m` and every incumbent law `C` |
| `p7_cap`, `p7_count` | Entry at index `m` gives `(m + 1) κ ≤ V`, through `EntryContest.entry_index_bounded`, and `m + 1 ≤ V/κ` when `κ > 0` |
| `p6_evaluation`, `p6_at_zero` | P6 at `μ = 0`. `Δ(m) = V/(m + 2)` for every `Q`, with the incumbent investing |
| `tendsto_cdf_investorLaw`, `tendsto_cdf_nonInvestorLaw` | As `μ → 0`, `F_μ(y_μ) → F_0(y)` when `y` is not an atom of `s`, and `G_μ(y_μ) → 1` when `y > 0` |
| `p6_limit` | P6 as stated. `Δ(m) → V/(m + 2)` as `μ → 0`, for every `Q` |

P7 needs no atoms in the investor's law `α` only. P6 assumes `s ≥ 0` almost
surely and no atoms in the law of `s`. The law of `r` is arbitrary. At
`μ = 0` the non-investor's score is the point mass at 0, which has an atom, so
`p6_at_zero` evaluates the formula for `Δ` at a point outside the primitives,
while `p6_limit` stays inside them for every `μ ≠ 0`. The limit is taken in a
neighbourhood of 0 and so also holds as `μ` falls to 0 from above.

## KappaSpread.lean, the cost of entry and P9 strictness

| Theorem | Content |
|---|---|
| `burden_strictAnti_of_strictConcave` | If `u` is strictly concave on the positive reals, `κ(w) = u(w) − u(w − c)` is strictly decreasing on `(c, ∞)` |
| `kappa_diverges_iff` | For `u` continuous at `c`, `κ → ∞` as `w ↓ c` exactly when `u → −∞` as its argument falls to 0 |
| `log_burden_strictAnti`, `log_kappa_diverges` | Log utility has both properties |
| `crra_strictConcave`, `crra_burden_strictAnti` | CRRA utility `x^(1−γ)/(1−γ)` is strictly concave and its burden strictly decreasing, for every `γ > 0`, `γ ≠ 1` |
| `crra_kappa_diverges`, `crra_kappa_not_diverges` | The cost diverges for `γ > 1` and stays bounded for `γ < 1` |
| `bm_example_not_diverges` | The separating example `√x + ε sin(kx)` of Claim BM has a bounded cost at `c` |
| `enters_downward` | After any spread with `λ ≥ 0`, the entry set is a prefix of the wealth ranking |
| `exit_below_pivot` | A challenger below the pivot fails to enter once `λ` is large enough |
| `p9_strict`, `p9_monotone` | P9 strictness. With an entrant below the pivot at `λ = 1`, the count is non-increasing in `λ ≥ 1` and strictly lower for every `λ` above some `λ̄` |
| `p9_strict_model` | The same with the model's `Δ` from `EntryContestModel.lean` and the cost from any strictly concave `u` with `u(0+) = −∞` |

A challenger enters when its wealth after the spread exceeds `c` and its cost
is within the gain at its index (`Enters`). The first conjunct is the
convention that a challenger whose wealth falls to `c` or below does not
enter, which large spreads require, because they push the poorest challengers
below the support floor `c`.

## BurdenWeaker.lean, Claim BM

| Theorem | Content |
|---|---|
| `burden_strictAnti_of_deriv` | If `u'(w) < u'(w − c)` for every `w > c`, the burden is strictly decreasing, with no sign of `u''` assumed |
| `not_burden_of_convex_stretch` | If `u` is convex on `[a, b]` with `b − a > c`, the burden is not strictly decreasing |
| `bm_example_not_monotone` | The example `√x + ε sin(kx)` of `PROOFS.tex` is not increasing, for any `ε > 0` and `k > 0` |
| `rampU`, `rampU_hasDerivAt` | The corrected example `log x + x + η ∫₀ˣ ramp`, with marginal utility `1/x + 1 + η · ramp(x)` |
| `rampU_strictMono`, `rampU_strictConvex`, `rampU_not_concave` | It is strictly increasing on the positive reals and strictly convex on `[x0, x0 + σ]` |
| `rampU_burden`, `rampU_kappa_diverges` | Its burden is strictly decreasing and diverges at `c` |
| `admissibleParams_open`, `rampU_admissible` | The parameters `(x0, σ, η)` that make it admissible and non-concave form an open set |
| `admissible_width` | Every convex width `σ < c` is attained by some admissible parameters |
| `bm_strictly_weaker` | Claim BM. For every `c > 0`, some admissible utility is not concave |

The example is continuously differentiable. It is not twice differentiable at
`x0` and `x0 + σ`, where the ramp has corners.

## PMU.lean, the first entrant's gain as `Q` grows

Write `D(Q) = Δ(0, Q + 1) − Δ(0, Q)` and `W = C · G^(Q−1) · (1 − G)`.

| Theorem | Content |
|---|---|
| `pmu_identity`, `pmu_sign_iff` | P-MU. `D(Q) = −V (∫ W dF − ∫ W dG)` for `Q ≥ 1` and every incumbent law, so with `V > 0` the gain rises exactly when `∫ W dF < ∫ W dG` |
| `kernel_rises`, `kernel_falls`, `kernel_max` | `g ↦ g^(Q−1)(1 − g)` rises on `[0, (Q−1)/Q]`, falls on `[(Q−1)/Q, 1]` and peaks at `(Q−1)/Q` |
| `pmu_single_crossing` | If `dF − dG ≤ 0` on `(−∞, x0]` and `≥ 0` on `(x0, ∞)`, then `D(Q2) ≤ G(x0)^(Q2−Q1) D(Q1)` for `1 ≤ Q1 ≤ Q2` |
| `pmu_step_nonpos_persists`, `pmu_quasiconcave` | Under that hypothesis, once `D` is nonpositive it stays so, and `Δ(0, ·)` is quasi-concave on `Q ≥ 1` |
| `unif`, `uniform_left`, `uniform_right` | With `r` uniform on `[0, 1]`, `s ≥ 0` and `0 < μ ≤ 1`, the hypothesis holds at `x0 = μ` |
| `pmu_quasiconcave_uniform` | For a uniform base score, `Δ(0, ·)` rises and then falls in `Q`, for any law of `s` and any incumbent |

The hypothesis is stated on measures, `α|(−∞, x0] ≤ β|(−∞, x0]` and
`β|(x0, ∞) ≤ α|(x0, ∞)`, so no densities are assumed. With densities it says
that `φ = G − F` is single-peaked.

## PMUWitness.lean, first-order dominance does not sign P-MU

| Theorem | Content |
|---|---|
| `cdf_unif_of_mem`, `integral_udiff` | The uniform law has `cdf = x` on `[0, 1]`, and `∫ (u^k − u^(k+1)) du = 1/(k+1) − 1/(k+2)` |
| `pairB_fosd`, `pairB_step`, `pairB_step_neg` | Pair B, `F = x²` (the larger of two uniforms) and `G = x`. `F ≤ G`, and `D(Q) = −V Q/((Q+2)(Q+3)(Q+4)) < 0` for every `Q ≥ 1` |
| `lawA`, `cdf_lawA`, `pairA_fosd` | Pair A, `F = x` and `G = 1 − (1 − x)²` (the smaller of two uniforms, built by reflecting the larger). `F ≤ G` |
| `pairA_step_one`, `pairA_step_two` | `D(1) = V/60` and `D(2) = V/420` for pair A |
| `fosd_does_not_sign` | At `Q = 1` and `Q = 2`, the two pairs give opposite signs, both with `F ≤ G`, no atoms and the incumbent investing |

## Refutations.lean, R1 and R2

| Theorem | Content |
|---|---|
| `r1_lower_bound` | If non-investor scores never exceed `xG`, then `Δ(m, Q) ≥ V (∫_{x ≥ xG} C F^m dF − 1/(Q − m))` |
| `pmu_step_nonneg_uniform`, `Delta_mono_uniform` | With `r` uniform on `[0, 1]`, any law of `s ≥ 0`, `0 < μ ≤ 1` and any incumbent, `Δ(0, ·)` never falls in `Q` |
| `bern`, `bern_restrict` | The success-or-failure family. `s = 0` with probability `p`, `s = 1` otherwise, `r` uniform, `μ ≤ 1/2`. Below `μ` the investor's law is `p` times the non-investor's |
| `bern_Delta` | `Δ(0, Q) = V ((1 − p²)/2 − p(1 − p)/(Q + 1))` for `Q ≥ 1`, with the incumbent investing |
| `bern_Delta_strictMono` | R2 refuted. `Δ(0, ·)` rises strictly at every `Q` for `0 < p < 1` |
| `bern_Delta_lower`, `bern_Delta_limit` | R1 refuted. `Δ(0, Q) ≥ V(1 − p)/2` at every `Q`, and `Δ(0, Q) → V(1 − p²)/2` |
| `bern_investor_above` | An investor scores above `μ` with probability `1 − p` |

## Anonymity.lean, the anonymity boundary

| Theorem or definition | Content |
|---|---|
| `winProb`, `profile`, `gain`, `IsEquilibrium` | Challengers may draw from different laws. Challenger `i` wins with probability `∫ C · ∏_{j ≠ i} F_j dF_i`, the formula `Delta` uses for identical laws |
| `gain_anonymous` | Proposition (anonymity). With identical entrant laws the gain is `Δ(|T|)`, a function of the number of other entrants alone |
| `gain_zero` | At `μ = 0`, with abilities `n_i` (the largest of `n_i` uniform draws), the gain is `V n_i / (1 + n_i + Σ_{j ∈ T} n_j)` |
| `κ3_values`, `ability_against_wealth`, `gains3` | Three challengers with wealth `11/4, 9/4, 2`, abilities `1, 2, 3`, CRRA `γ = 2`, `c = 1`, `V = 1` |
| `anon_witness_zero` | At `μ = 0`, the poorest challenger alone and the two richest together are both equilibria |
| `winProb_tendsto`, `gain_tendsto` | Every win probability is continuous at `μ = 0` |
| `anon_witness_positive`, `anon_witness_exists`, `laws_noAtoms` | Both equilibria persist for every small `μ > 0`, where every law has no atoms, so count invariance fails inside the primitives once ability falls with wealth |

## FallWitness.lean, the gain rises and then falls in `Q`

The base score `r` is the smaller of two uniform draws, with density `2(1 − r)`.
The investment fails (`s = 0`) with probability `p` and otherwise lifts the
score by `1 − μ`, with `μ = 3/4`, and the incumbent invests.

| Theorem | Content |
|---|---|
| `investorLaw_bern`, `cdf_investorLaw_bern`, `integral_investorLaw_bern` | The investor's law is `p` times the base law scaled by `μ` plus `1 − p` times the same law shifted by `1 − μ`, with the CDF and integrals that follow |
| `integral_poly8` | The integral of a polynomial of degree at most 7 over an interval, by its antiderivative |
| `base1_one` to `shift1_two` | The six piecewise polynomial integrals that `Δ(0, 1)`, `Δ(0, 2)` and `Δ(0, 3)` reduce to, each verified by `ring` |
| `stepQ_one`, `stepQ_two` | `Δ(0, 2) − Δ(0, 1) = V(−11p²/162 + 1901p/21870 − 208/10935)` and `Δ(0, 3) − Δ(0, 2) = V(−4933p²/393660 + 77219p/2755620 − 10672/688905)` |
| `rise_then_fall` | At `p = 1/2`, `Δ(0, 1) < Δ(0, 2)` and `Δ(0, 3) < Δ(0, 2)` |
| `fall_from_one` | At `p = 1/4`, `Δ(0, 2) < Δ(0, 1)`, the direction Referee 2 conjectured |
| `αp_le_β0`, `αp_noAtoms`, `β0_noAtoms` | The witness satisfies P2 and has no atoms, so it lies inside the primitives |

The polynomial coefficients were generated outside Lean and pasted in. Lean
re-verifies each piece against the CDF formulas and each closed form against
the pieces, so a wrong coefficient would fail the build.

## ShiftClass.lean, quasi-concavity for a class of primitives

| Theorem or definition | Content |
|---|---|
| `ShiftMono` | A law never loses mass when a set above `c > 0` moves down by `c` |
| `left_of_lift`, `right_of_lift` | With a single-step lift of `1 − μ`, `dF − dG ≤ 0` up to `1 − μ` and `≥ 0` beyond it, the second when the base law is shift-monotone |
| `quasiconcave_of_shiftMono` | For any shift-monotone base law with no mass at or below 0, any `0 < μ < 1`, any failure probability and any incumbent, `Δ(0, ·)` is quasi-concave in `Q` |
| `shiftMono_of_density` | A law with a density that does not rise on the positive reals is shift-monotone |
| `lawA_eq_densLaw`, `lawA_shiftMono` | The witness's base law has density `2(1 − r)` on `[0, 1]` and is shift-monotone |
| `witness_quasiconcave` | The witness of `FallWitness.lean` lies in the class |

## MustFall.lean, when the gain must fall in `Q`

Write `G_t` for the non-investor's law `G` shifted up by the lift `t = 1 − μ`.

| Theorem | Content |
|---|---|
| `must_fall_general` | If `G_t` has no mass at or below `t`, dominates `G` above `t`, and puts strictly more mass than `G` on some `(a, b]` with `t < a < b`, `G(t) < G(a)`, `G(b) < 1` and `C(a) > 0`, then `∫ W_Q dG < ∫ W_Q dG_t` for every `Q` from some `Q0` on |
| `shift_dominates`, `stepQ_bern` | For a shift-monotone base law, `G_t` dominates `G` above the lift, and the step in `Q` is `−V(1 − p)(∫ W_Q dG_t − ∫ W_Q dG)` |
| `must_fall` | In the single-step class with `p < 1`, the strict gain on `(a, b]` makes `Δ(0, ·)` fall strictly at every `Q` from some `Q0` on |
| `witness_must_fall` | The base density `2(1 − r)` with `μ = 3/4` meets the condition on `(1/2, 5/8]`, so its gain must fall, for every `0 ≤ p < 1` |
| `uniform_fails_gain` | A uniform base law meets the condition on no interval, because its gain never falls |
| `unif_shiftMono`, `uniform_never_gains` | The uniform law is shift-monotone, so the last result needs no hypothesis (pass 4d) |

## MeasurementMap.lean, the measurement-map checks and P5-inv

Each group replaces the SymPy block of the same name in
`checks/verify_measurement_map.py`, stated over the reals and, where the SymPy
fixed a size or a family, in general.

| Theorem | Content |
|---|---|
| `mm1a` to `mm1d` | MM1. From the displacements at two ranks, `λ = 1 + (d_a − d_b)/(w_a − w_b)` and `x0 = (d_b w_a − d_a w_b)/(d_b − d_a)`; the swapped numerator fails |
| `mm2a` to `mm2c` | MM2. For any number of challengers, a spread about the profile mean keeps the mean, `λ` is read off any challenger, and a spread about another point with `λ ≠ 1` moves the mean |
| `mm3`, `mm4`, `mm5`, `mm5a`, `mm5b` | MM3 to MM5. P6, the P7 base integral and the P-MU identity with the factor `V`, from the earlier files, and `−V/70` for `F = x²`, `G = x`, `Q = 3` |
| `mm6a` to `mm6f`, `Delta_scale` | MM6. Under `(u, V) ↦ (a u + b, a V)` with `a > 0`, `κ` and `Δ` scale by `a`, every entry condition is unchanged, invariant statements depend on `κ/V` only, and cost and payoff differences scale by `a` |
| `mm7_equilibria`, `mm7a`, `mm7b` | MM7. With `Δ = (12, 10, 1)` and `κ = (2, 5, 8)` the equilibria are the three pairs, all of count 2, and one omits the rank-2 challenger |
| `mm8a`, `mm8b` | MM8. With `r` uniform on `[0, b]`, the non-investor's CDF reaches 1 exactly from `μ b` on, and the top is `μ` exactly when `b = 1` |
| `mm9a`, `mm9b`, `mm9_model` | MM9. Expected payoff with entry minus without is `V(P1 − P0) − κ(w)`, and in the model entering is weakly better exactly when `κ(w) ≤ Δ` |
| `eu_affine`, `mm10a`, `mm10a_example`, `mm10b` | MM10. Expected-payoff rankings are unchanged by `a U + b` with `a > 0`, and the example's ranking reverses under `(√)⁴` |
| `exists_mem_ge`, `exists_not_mem_le`, `count_invariance` | P5-inv with the two counting facts proved, so every pure-strategy equilibrium has size `k*` with no extra hypothesis |

## Equilibrium.lean, equilibrium structure for the manuscript (pass 4a)

Challengers are indexed `0, …, Q − 1` in increasing order of cost. `k*` satisfies
the entry condition below it and fails it at `k*` when `k* < Q`.

| Theorem or definition | Content |
|---|---|
| `Delta_step_strict` | The gain falls strictly from `m` to `m + 1` when `V > 0` and `∫ φ² dK_m > 0` |
| `IsEquilibriumR`, `count_invariance_fin` | Every pure-strategy equilibrium among `Q` challengers has size `k*` |
| `assortative_is_equilibrium` | N4 (i). The `k*` cheapest challengers form an equilibrium |
| `assortative_unique` | An equilibrium of size `k*` whose members all lie below `k*` is the assortative set |
| `three_equilibria` | With costs `(2, 5, 8)` and gains `(12, 10, 1)`, the sets `{0, 1}`, `{0, 2}` and `{1, 2}` are all equilibria, so identities are not pinned without M7's condition (pass 5) |
| `assortative_min_cost` | N4 (ii). No set of `k` challengers costs less in total than the `k` cheapest |
| `prod_profile_anonymous`, `prize_term_anonymous` | With identical score laws, the challengers' win probabilities sum to a function of the entrant count alone |
| `payoffSum`, `payoff_gap_anonymous` | N4 (iii). Payoff sums of two equal-size entrant sets differ exactly by their total costs |
| `kstar_mono`, `Delta_mono_prize`, `kappa_mono_fee` | P8. `k*` rises when costs fall and gains rise, the gain rises with the prize, and the cost rises with the fee for non-decreasing `u` |

## Spreads.lean, wealth spreads and the entrant count (pass 4c)

Profiles are sorted with the richest at rank 0. Rank `j` enters when its wealth
exceeds `c` and its cost is within `Δ(j)` (`EntersP`), and `IsCount` fixes the
count as the length of the prefix of entrants.

| Theorem | Content |
|---|---|
| `enters_down`, `count_ge_of_enters`, `count_le_of_fails` | Entrants form a prefix, and the count is bounded by which ranks enter |
| `margin_rise`, `margin_fall` | If the marginal entrant's wealth does not fall, the count does not fall; if the first outsider's wealth does not rise, the count does not rise |
| `tail_rise`, `tail_fall` | The rank-by-rank tail condition of earlier drafts of `PROOFS.tex` as a special case |
| `IsPivotSpread`, `pivot_rise`, `pivot_fall`, `spread_isPivot` | P9-gen and P9. The direction is set by the marginal entrant's position against the pivot, and the linear spread with `λ ≥ 1` is a pivot-spread |
| `InBand`, `branch_of_not_band`, `not_band_of_single_crossing`, `pivot_single_crossing` | The direction is open only when the displacement is negative at the margin and positive at the first outsider, which a pivot-spread never produces |
| `witness_counts`, `witness_signs`, `band_both_directions`, `band_both_directions_positive` | Inside the band both directions occur, with the same signs, at the benchmark gains and for every small `μ > 0` |
| `mps_lowers_count` | A change that keeps the total and majorizes the profile lowers the count with the marginal entrant above the mean, so the mean cannot replace the pivot |
| `mPre`, `mUp`, `mDown`, `antitone_of_four`, `mps_band_signs`, `mps_witness_counts`, `mps_band_both_directions`, `mps_band_both_directions_positive` | Four challengers. Two changes that keep the total and majorize the profile, with the same signs at every rank, put the margin in the band and move the count up and down, at the benchmark gains and for every small `μ > 0` |

## FullertonMcAfee.lean, the literature check of Fullerton and McAfee (1999)

The Lean half of `lit/fullerton_mcafee_1999/`, kept here because the claims
need real analysis. Locators are printed pages of the *JPE* article.

| Theorem | Content |
|---|---|
| `win_prob` | Eq. (1). `∫_0^1 z_i t^{Z−1} dt = z_i / Z`, the ratio-form win probability from independent draws |
| `IsNash`, `zstar_isNash`, `nash_char`, `nash_unique`, `active_prefix`, `profit_zstar`, `all_active` | Theorem 1 from the primitives. The effort subgame has exactly one Nash equilibrium, `z_i = (P/t)(1 − c_i/t)^+` with `t` the unique root of `Σ (t − c_i)^+ = t`, the active firms are the lowest-cost ones, and profits are eq. (4) |
| `root_two`, `root_unique`, `best_response`, `zstar_sum` | The steps behind it |
| `fin_all_active`, `two_sizes` | Entry equilibria of sizes 2 and 3 in one economy, so the count is not invariant in their model |
| `lemma1_deviation`, `lemma1_bound`, `lemma1_case_out`, `lemma1_exact` | Lemma 1 and the example in which its bound binds |
| `thm2_iff`, `thm2_iff_needs_sign`, `thm2_step` | Theorem 2's step, and the sign condition its first display needs |
| `symmetric_case`, `TC`, `TC_formula`, `TC_step` | The symmetric case and Theorem 3 |
| `lemma2_step`, `lemma2_single_m_fails`, `lemma2_constant_increment`, `lemma2_proportional` | Lemma 2 with its induction hypothesis made explicit, a counterexample to the single-`m` reading, and the two named families |
| `uniform_bid_scale`, `uniform_bid_constant` | With two entrants and uniform costs the uniform-price bid does not depend on cost |
| `lemma4`, `lemma4_sharp` | Lemma 4, and that its condition cannot be dropped |
| `Thm4Hyp`, `thm4_hyp_always`, `thm4_hyp_with_increasing_bid`, `thm4_interior` | Theorem 4's hypothesis as printed holds for every `Ψ`, and the reading its proof needs |

## LewisThompson.lean, the literature check of Lewis and Thompson (1981)

The Lean half of `lit/lewis_thompson_1981/`, an earlier source for the dispersive
order. Locators are printed pages of the *J. Appl. Prob.* article.

| Theorem | Content |
|---|---|
| `OrdCdf`, `OrdSpacing`, `OrdDiff`, `cdf_iff_spacing`, `spacing_iff_diff` | Their (1.6), quantile spacing and Hopkins and Kornienko's Definition 1 are one order for continuous, strictly increasing distribution functions |
| `transport_disp`, `transport_pivot` | The transport map has a non-decreasing displacement exactly under the order, and with a crossing it is a pivot-spread |
| `affine_invariant`, `scale_pair` | Invariance under location and positive scale, and `X` against `kX` for `k ≥ 1` |
| `qAsym`, `qNeg`, `qAsym_strictMonoOn`, `sig`, `sig_mem`, `qAsym_sig`, `qAsym_continuousOn`, `qAsym_onto`, `neg_quantile`, `spacing_gap`, `neg_not_ordered` | "X and kX form an o.d. pair for k ≠ 1" fails at `k = −1` for a distribution function strictly increasing on `ℝ` |
| `thm1`, `thm2`, `thm2_density` | Theorems 1 and 2 in quantile-derivative form |
| `scale_family`, `pareto`, `lognormal_ratio`, `mixture_logconvex` | The examples of Section 6 |

## What is still assumed

- That the score laws have no atoms. The primitives assume it for `F` and `G`;
  deriving it from the laws of `r` and `s` is not attempted.

`StepNonpos.lean`, compiled in pass 2, assumed densities. It was removed on
6 October 2026 because `StepIdentity.lean` proves the same sign without them.
