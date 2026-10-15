# Lean with Mathlib

Every file here is a build root in `lakefile.toml` and is audited by
`checks/verify_mathlib.py`, which admits `propext`, `Classical.choice` and
`Quot.sound` and nothing else. One namespace per file. Model rows cite
`Namespace.theorem`.

## Sufficiency.lean, row M1

| Theorem or definition | Content |
|---|---|
| `posterior` | The Bayesian posterior on class within a group, `L c * p c / ∑ L c' * p c'`, over a finite class space |
| `posterior_eq_prior_of_flat` | A likelihood constant in class, `L c = k ≠ 0`, leaves the posterior equal to the prior |
| `flat_of_posterior_eq_prior` | Under a prior with full support and a nonzero marginal, a posterior equal to the prior forces `L c` to equal the marginal at every class |
| `control_flat_needed` | Prior `(1/2, 1/2)` and likelihood `(1, 0)`. The posterior at class 0 is 1, not 1/2 |
| `control_support_needed` | Prior `(1, 0)` and likelihood `(1, 2)`. The posterior equals the prior while the likelihood depends on class |

## ClassPosterior.lean, rows M2 and M3

| Theorem or definition | Content |
|---|---|
| `mixture`, `marg`, `bayesClass`, `bayesGroup` | The mixture `∑ g, p g c * w g`, the marker's marginal, and the Bayesian posteriors on class and on group, over finite groups and classes |
| `bayes_class_eq_mixture` | M2. With a likelihood constant in class and normalised class distributions, the Bayesian posterior on class is the mixture by the Bayesian posterior on group |
| `control_sufficiency_needed` | One group, class distribution `(1/2, 1/2)`, likelihood `(1, 0)`. Bayes gives 1 and the mixture gives 1/2 |
| `control_normalised_needed` | Class weights `(1, 1)` with a flat likelihood. Bayes gives 1/2 and the mixture gives 1 |
| `ratioLik` | The likelihood `κ w g / q g`, constant in class |
| `bayes_group_of_ratio`, `bayes_class_of_ratio` | M3. With `q g > 0`, `∑ w = 1` and `κ ≠ 0`, the Bayesian group posterior is `w` and the class posterior is the mixture by `w` |
| `ratioLik_mem_unit` | The likelihood lies in `[0, 1]` when `0 < κ ≤ q g` and `0 ≤ w g ≤ 1` |
| `control_prior_support_needed` | Prior `(1, 0)`. Group 1 keeps posterior weight 0 under every likelihood |

## ClosureMap.lean, rows M4 to M6

`Hf` is the composed map for a general `F`, and `H` the map for
`F c = (1 − c)²` with `b = s δ`. `Q c = b c² − (b − γ) c + μ`.

| Theorem | Content |
|---|---|
| `Hf_zero`, `Hf_one_iff`, `H_eq_Hf` | M4. `Hf 0 = μ`; with `s δ ≠ 0`, `Hf 1 = 1 ↔ F 1 = 0`; the canonical `F` gives `H` |
| `control_F_one_needed` | `F ≡ 1` sends 1 to 0 |
| `H_sub_self`, `fixed_iff_Q`, `H_one` | `H c − c = (1 − c) Q c`, so the fixed points other than 1 are the roots of `Q` |
| `vieta`, `Q_factor` | Two distinct roots satisfy `b (c₁ + c₂) = b − γ` and `b c₁ c₂ = μ`, and then `Q c = b (c − c₁)(c − c₂)` |
| `two_interior_iff` | M5. With `μ > 0`, `γ ≥ 0`, `b > 0`, two fixed points in `(0, 1)` exist exactly when `b − γ > 0` and `(b − γ)² > 4 μ b` |
| `fixed_points_exactly` | The fixed points are then exactly `c₁`, `c₂` and 1 |
| `trap_at_figure_parameters`, `control_no_trap` | The condition holds at `(1/20, 1/10, 1/2)` and fails at `(1/20, 1/10, 1/5)` |
| `tendsto_up`, `tendsto_down` | A continuous map, nondecreasing on an interval, with a fixed endpoint and moving every point towards it, carries every orbit to that endpoint |
| `H_continuous`, `H_strictMonoOn` | `H` is continuous, and strictly increasing on `[0, 1]` when `b < 1 − μ + γ` and `μ + γ < 1` |
| `basins` | M6. Under those bounds, orbits from `[0, c₂)` converge to `c₁` and orbits from `(c₂, 1]` converge to 1 |
| `basins_hypotheses_at_figure` | The bounds hold at `(1/20, 1/10, 1/2)` |
| `control_leaves_unit`, `control_not_monotone` | At `(1/20, 0, 3)`, `H (1/5) < 0`. At `(1/20, 0, 1)`, `H (1/100) < H 0` |
| `control_basin_fails` | At `(3/5, 0, 5/2)` the trap condition holds with fixed points `2/5` and `3/5`, the bound `b < 1 − μ + γ` fails, and the orbit of 0 converges to `3/5`, not `2/5` |

## FrozenMap.lean, rows M7 and M8

`Hk` is the composed map with the group prior frozen, so the discrimination
rate is the constant `k = s δ F β₀`. `P c = γ c² + (μ + k − γ) c − μ`, and
`Hk c − c = − P c`.

| Theorem | Content |
|---|---|
| `Hf_frozen` | `Hf` with `F` held at `F β₀` is `Hk` with `k = s δ F β₀` |
| `Hk_sub_self`, `fixed_iff_P` | The fixed points of `Hk` are the roots of `P` |
| `P_cross` | `y P x − x P y = (x − y)(γ x y + μ)` |
| `fixed_unique` | M7. With `μ > 0`, `γ ≥ 0`, `P` has at most one root in `[0, ∞)` |
| `exists_fixed` | M7. With `μ > 0`, `k > 0`, `P` has a root in `(0, 1)` |
| `P_neg_below`, `P_pos_above` | `P` is negative below the root and positive above it, on `[0, ∞)` |
| `fixed_anti`, `fixed_mono_belief` | M7. A higher `k` gives a lower root, so a lower `F β₀` gives a higher one |
| `control_mu_needed` | At `(0, 1/2, 1/4)`, both 0 and 1/2 are roots |
| `Hk_continuous`, `Hk_strictMonoOn` | `Hk` is continuous, and strictly increasing up to 1 when `γ ≥ 0` and `k < 1 − μ − γ` |
| `converges_to_fixed` | M8. Under that bound, every orbit from `[0, 1]` converges to the root |
| `control_cycle_fixed`, `control_cycle` | At `(1/2, 0, 3/2)`, `Hk c = 1/2 − c`, the root is 1/4, and the orbit of 0 has no limit |

## Bifurcation.lean, rows M9 and M10

`gammaStar μ b = b − 2 √(μ b)` is the mobility threshold and
`bStar μ γ = (√μ + √(μ + γ))²` the threshold on `b = s δ`.

| Theorem | Content |
|---|---|
| `gammaStar_eq` | `γ* = (√b − √μ)² − μ` |
| `trap_iff_gamma` | M9. The trap condition of M5 holds exactly when `γ < γ*` |
| `Q_pos_above`, `only_one_above` | M9. Above `γ*`, `Q > 0` on `[0, ∞)` and 1 is the only fixed point in `[0, 1]` |
| `at_threshold_factor`, `at_threshold_fixed` | M9. At `γ*`, `H c − c = (1 − c)(√b c − √μ)²`, and the fixed points are `√μ / √b` and 1 |
| `double_root_mem` | The double root lies in `(0, 1)` when `μ < b` |
| `gammaStar_pos_iff` | M9. `γ* > 0 ↔ b > 4 μ` |
| `control_threshold_zero` | `γ* = 0` at `(μ, b) = (1/20, 1/5)` |
| `trap_iff_b` | M10. With `γ ≥ 0`, `γ < γ* ↔ b* < b` |
| `gammaStar_strictMono`, `gammaStar_strictMono_salience` | M10. `γ*` rises with `b` from `b = μ` on, and so with `s` at fixed `δ > 0` |
| `bStar_at_figure` | `0.29 < b* < 0.30` at `μ = γ = 1/20` |
| `control_not_mono_below_mu` | At `μ = 1`, `γ*(1/4) = −3/4 < 0 = γ*(0)` |
| `H_hasDerivAt`, `slope_one_at_threshold` | The derivative of `H`, and `H' = 1` at `√μ / √b` when `γ = γ*` |
| `control_slope_off_threshold`, `gammaStar_example` | At `(1/20, 1/10, 4/5)` the slope at `1/4` is `17/20`, while `γ*(1/20, 4/5) = 2/5` |

## Outsider.lean, row M11

`step` moves an outsider by `H μ γ b` and a host by `H μI γI bI`.

| Theorem | Content |
|---|---|
| `decoupled` | The `n`-th joint iterate is the pair of separate `n`-th iterates |
| `outsider_trapped` | M11 (i). Below `γ*` and under the bounds of M6, an outsider from `[0, c_M)` converges to `c_L` for every host, whose orbit is its own |
| `integrates_above` | M11 (ii). Above `γ*` and under the same bounds, every orbit from `[0, 1]` converges to 1 |
| `trapped_hypotheses_example`, `integrates_hypotheses_example` | The hypotheses of each part hold at `(1/20, 1/10, 1/2)` and `(1/20, 1/10, 1/5)` |
| `control_threshold_needed` | At `(1/20, 1/10, 1/2)` the orbit of 0 does not converge to 1 |

## GeneralF.lean, row M12

| Theorem | Content |
|---|---|
| `Hf_sub_self` | `Hf c − c = (1 − c)(μ + γ c) − s δ c F c` |
| `rise_near_one` | With `F 1 = 0`, `F' 1 = 0`, `μ > 0`, `γ ≥ 0`, `s δ ≥ 0`, the map is above the diagonal at some `d` between any `c ∈ [0, 1)` and 1 |
| `two_fixed_of_dip` | M12. `F` continuous on `[0, 1]`, a dip at `c` and a rise at `d > c` give fixed points in `(0, c)` and `(c, d)` |
| `dip_of_trap` | For the canonical map, the midpoint of two interior fixed points is a dip |
| `control_dip_needed` | `F ≡ 0` puts the map above the diagonal on `[0, 1)` |
