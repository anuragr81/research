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
