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
| `trap_at_example`, `control_no_trap` | The condition holds at the illustrative `(1/20, 1/10, 1/2)` and fails at `(1/20, 1/10, 1/5)`. The predecessor's Figure 1 is checked in `Figures.lean` |
| `tendsto_up`, `tendsto_down` | A continuous map, nondecreasing on an interval, with a fixed endpoint and moving every point towards it, carries every orbit to that endpoint |
| `H_continuous`, `H_strictMonoOn` | `H` is continuous, and strictly increasing on `[0, 1]` when `b < 1 − μ + γ` and `μ + γ < 1` |
| `basins` | M6. Under those bounds, orbits from `[0, c₂)` converge to `c₁` and orbits from `(c₂, 1]` converge to 1 |
| `basins_hypotheses_at_example` | The bounds hold at `(1/20, 1/10, 1/2)` |
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

## MeasurementMap.lean, the recovery formulas of the measurement map

| Theorem | Content |
|---|---|
| `recovery_mu_gamma` | With `b = 0`, two pairs of consecutive standings give `γ` and `μ` |
| `recovery_b`, `control_recovery_b_interior` | Under the canonical `F`, one pair inside `(0, 1)` gives `b`. At standing 0 the formula returns 0 |
| `recovery_delta` | With the discrimination rate recorded, one pair gives `δ` |
| `recovery_F` | With salience and the rate, `F` is read off pointwise |

## Intensity.lean, row M13

`Hrho μ γ δ s ρ = H μ γ (ρ s δ)`, the predecessor's intensity-scaled map, and
`rhoStar = bStar / (s δ)`.

| Theorem | Content |
|---|---|
| `Hrho_eq` | `Hrho` is the predecessor's equation (loury-rho) under the canonical `F` |
| `gammaStar_lt_iff` | The strict form of M10, `γ* < γ ↔ b < b*` |
| `trap_iff_rho` | M13 (i). The trap condition holds exactly when `ρ > ρ*` |
| `only_integration_below` | M13 (ii). Below `ρ*`, 1 is the only fixed point in `[0, 1]` |
| `rhoStar_mem_iff`, `rhoStar_interior_example` | M13 (iii). `0 < ρ* < 1 ↔ b* < s δ`, which holds at `(1/20, 1/10, 1/2)` |
| `control_rhoStar_not_interior` | At `(1/20, 1/20, 1/5)`, `ρ* > 1` |

## Passing.lean, row M14

`mean c V` is the mean class of a finite set of people.

| Theorem | Content |
|---|---|
| `cross` | `∑ over S × P of (c i − c j) = |S| ∑_P c − |P| ∑_S c` |
| `mean_sdiff_le`, `mean_sdiff_lt` | M14 (i). Leavers at or above every stayer lower the visible mean, strictly when one is strictly above |
| `upper_set_of_threshold` | M14 (ii). A monotone benefit and an antitone cost make the passers an upper set |
| `rate_rises` | M14 (iii). A lower mean raises `s F` under antitone `F` |
| `control_ordering_needed` | Classes `(0, 1)` with the class-0 member leaving give a visible mean of 1 against 1/2 |

## Platform.lean, row M15 and R1

`marg` is the predecessor's admission first-order condition, `profit σ = σ marg`,
`cAdm` the admission threshold, `cEff` the host's effective standing and
`etaCrit` the critical outsider fraction.

| Theorem | Content |
|---|---|
| `marg_neg_iff`, `marg_neg_iff_below` | M15 (i). The margin is negative exactly when `W |A| < n (1 − α)(c_I − c_O)`, that is when `c_O < c_E` |
| `exclude_of_neg` | M15 (ii). A negative margin makes every positive admission rate earn less than exclusion |
| `control_admit_of_pos` | A positive margin makes full admission earn more |
| `tip_iff` | M15 (iii). The host tips exactly when `η > η_crit` |
| `etaCrit_anti` | M15 (iv). `η_crit` falls in `θ` while the host is above its tipping point |
| `control_etaCrit_sign` | With the host below its tipping point, `η_crit` rises in `θ` |
| `tokenism_not_implied` | R1. For every `c_M < 1` there is a `W` with `c_E > c_M` and one with `c_E < c_M` |

## Slowing.lean, row M16

| Theorem | Content |
|---|---|
| `Q_threshold_plus` | At `γ = γ* + ε`, `Q c = (√b c − √μ)² + ε c` |
| `step_bounds` | Inside the window of half-width `√ε/√b` about the merged root, one step moves the standing up by between 0 and `2ε` |
| `slowing` | M16. From the window's lower edge, reaching its upper edge takes at least `1/(√b √ε)` generations |
| `control_start_matters` | From the upper edge no generation is needed, while the bound is positive |
| `bound_at_example` | At `(μ, b, ε) = (1/20, 3/10, 1/100)` the hypotheses hold and the bound is at least 18 |

## Figures.lean, row M17

The predecessor's Figure 1 (`fig:closure-map`) uses `μ = 0.05`, `δ = 0.5`,
`s = 1`, start `0.55`, `γ = 0.05` in panel (a) and `γ = 0.30` in panel (b),
read from `figures/generate_figures.py`. Figure 2 is checked by
`Bifurcation.bStar_at_figure`.

| Theorem | Content |
|---|---|
| `fig1a_start_between`, `fig1a_trapped` | Panel (a). The start `11/20` lies between the interior fixed points, and its orbit converges to the lower |
| `fig1b_integrates` | Panel (b). The orbit of `11/20` converges to 1 |
| `control_fig1a_not_integrated` | In panel (a) the orbit of `11/20` does not converge to 1 |
| `rhoStar_at_fig1` | `0.58 < ρ* < 0.60` at panel (a), against the predecessor's unexplained `0.31` |
| `stranger_example` | The worked example's mixtures, `1/2` and `19/50` |

## Assumptions.lean, the plain-English readings of Table 2

Where a reading in the In plain English column of the assumptions table
restates algebra or a property of a formula, the statement is proved here.
`law` is the law of motion of A6 with the discrimination rate `φ` free, and
`canonF c = (1 − c)²`.

| Theorem or definition | Content |
|---|---|
| `marginal_of_flat` | A4. A likelihood constant in class, under a prior that sums to one, has that constant as its marginal |
| `pressure_nonneg` | A5. Salience in `[0, 1]` and a positive cost of exclusion give an exclusion pressure that is not negative |
| `law_plain` | A6. The law of motion is `c + μ (1 − c) + γ c (1 − c) − δ φ c` |
| `law_relearned` | A7. With `φ = s F c`, the law is the composed map `Hf` |
| `law_frozen` | A8. With `φ = s F β₀`, the law is the frozen map `Hk` with `k = s δ F β₀` |
| `canonF_one`, `canonF_deriv_one` | A9. The canonical `F` is zero with zero slope at full standing |
| `canonF_strictAntiOn` | A9. The canonical `F` falls as standing rises on `[0, 1]` |
| `canon_through_product` | A9. Under the canonical `F`, two pairs `(s, δ)` with the same product give the same composed map |
| `control_slope_needed` | `F c = 1 − c` is zero at 1 but has slope `−1` there |
