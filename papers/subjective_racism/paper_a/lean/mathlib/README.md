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
