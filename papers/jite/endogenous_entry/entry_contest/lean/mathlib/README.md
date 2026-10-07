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

## What is still assumed

- That the score laws have no atoms. The primitives assume it for `F` and `G`;
  deriving it from the laws of `r` and `s` is not attempted.

`StepNonpos.lean`, compiled in pass 2, assumed densities. It was removed on
6 October 2026 because `StepIdentity.lean` proves the same sign without them.
