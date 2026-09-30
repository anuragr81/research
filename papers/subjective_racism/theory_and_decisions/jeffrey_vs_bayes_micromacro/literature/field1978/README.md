# Field (1978), "A Note on Jeffrey Conditionalization"

*Philosophy of Science* 45(3), 361-367.

All eight PDF pages (JSTOR cover plus pp. 361-367) were read; equations (3)-(7) and
(3')-(6') were checked on the rendered pages.

## Claims formalized

- eq. (4)/(5), p. 364: the reparametrization
  $\alpha = \tfrac12\log\big((q/p)/((1-q)/(1-p))\big)$ is the inverse of
  $q = pe^\alpha/(pe^\alpha+(1-p)e^{-\alpha})$.
- $e^{2\alpha}$ is the odds ratio $(q/p)/((1-q)/(1-p))$, i.e. the **likelihood
  ratio** (Bayes factor) of the update, and $e^{\alpha}$ is its square root. So
  $\alpha$ is *half* the log-odds shift (Field, p. 364: "The (1/2) log is of course
  just in there for reasons of scale").
- eq. (6), p. 364: the $\alpha$-update is Jeffrey's rule (3) with $q$ given by (5).
- eq. (7), p. 366: for two binary partitions $E$, $E'$, the $\alpha$-update applied
  $E$-then-$E'$ equals $E'$-then-$E$, and both equal the closed form
  $P''(F_k)\propto P(F_k)\, e^{\pm\alpha\pm\alpha'}$ (signs by whether
  $F_k\subseteq E,\neg E$ and $E',\neg E'$).
- (5')/(6'), pp. 366-367: tilting a $k$-cell partition by $e^{\alpha_i}$ is Jeffrey's
  rule (3') with $q_i = p_i e^{\alpha_i}/\sum_j p_j e^{\alpha_j}$.
- p. 365: the same two updates, expressed in the delivered credences $q, q'$ instead
  of $\alpha,\alpha'$, do **not** commute ("a very complicated law; moreover, it is an
  asymmetric law").

## Lean

`lean/Field.lean` is a symlink to `lean/Literature/Field.lean` (namespace
`Literature.Field`, standalone, `import Mathlib` only). It builds with no `sorry`,
and every theorem depends only on `propext`, `Classical.choice` and `Quot.sound`.

| Theorem | What it establishes | Field |
|---|---|---|
| `qOf_alphaOf`, `alphaOf_qOf` | (4) and (5) are mutually inverse (`0 < p, q < 1`, any real `α`) | (4), (5), p. 364 |
| `exp_two_alphaOf`, `exp_alphaOf_sq` | `e^{2α} = (q/p)/((1-q)/(1-p))`; `(e^α)² =` the same | (4) |
| `exp_two_alphaOf_bayes` | if `q` is the Bayes posterior of `p` under likelihoods `ℓ₁` (on `E`), `ℓ₀` (on `¬E`), then `e^{2α} = ℓ₁/ℓ₀`: **`e^{2α}` is the likelihood ratio** | (4) |
| `qOf_eq_bayesPost` | (5) is the Bayes update with likelihoods `e^{α}`, `e^{-α}` | (5) |
| `tilt_eq_jeffrey`, `tilt_zero` | (6) is Jeffrey's (3) with `q` from (5); `α = 0` leaves `P` unchanged | (6), p. 365 |
| `reweight_reweight`, `field_eq7`, `field_eq7_comm` | two `α`-updates on two binary partitions of any finite space, in either order, equal the closed form (7) | (7), p. 366 |
| `reweight_cells_eq_jeffrey` | the `k`-cell tilt is Jeffrey's (3') with `qᵢ = pᵢe^{αᵢ}/Σⱼpⱼe^{αⱼ}` | (5'), (6') |
| `jeffrey_EE'`, `jeffrey_E'E`, `jeffrey_not_comm` | prior `(.1, .2, .3, .4)`, `q = .7` on `E`, `q' = .3` on `E'`: `147/760` vs `63/370` in cell `E ∧ E'`, gap `651/28120`, so the credence-input updates do not commute | p. 365 |
| `tilt_comm_P₀` | on the same prior the `α`-updates commute | (7) |

Not formalized: the limits `α → ±∞` (strict conditioning as a limiting case, p. 365)
and the normalization `Σαᵢ = 0` of (4') (a choice of scale). Whether `α` is an input
parameter is Field's philosophical claim.

## SymPy

`sympy/check_commutativity.py` asserts every claim above and exits 0 iff all pass
(14/14): the inversion both ways, `e^{2α}` = odds ratio = likelihood ratio, (6) =
(3)+(5) in all four cells, eq. (7) both routes equal to each other and to the closed
form in all four cells, and the credence-input gap (not identically zero; `147/760`,
`63/370`, `651/28120` at the test point).

**Bug fixed (audit L1).** The earlier script's `tilt_step1(F, al)` ignored its
argument `F` and re-tilted the prior symbols, so the `E'`-then-`E` route was never
computed and the printed "differences" were nonzero, while this README reported
eq. (7) as verified. The function (now `tilt_E`) reweights its argument; a
regression check tests exactly that, and the commutation now passes.

## Bearing on Paper B

Field's `α`-update on a `2×2` cell structure is the same object as Paper B's
Bayes-factor benchmark $\PB$ applied sequentially, **provided each `α` is computed
from its cue against the prior marginal** (so the second `α` is not recomputed
against the intermediate belief). Eq. (7) is then, in different notation, the
commutation half of Propositions IMM/DIV (Jeffrey conditioning in credence
parameters does not commute; fixed likelihood-ratio reweighting does). Field never
uses the words "Bayes factor".

The scaling: by (4), $e^{2\alpha} = (q/p)/((1-q)/(1-p))$ **is** the likelihood ratio
(Bayes factor), Paper B's $\ell_1/\ell_0$ for the cue; $e^{\alpha}$ is its square
root. (An earlier version of this note said "$e^{2\alpha}$ is a squared likelihood
ratio" and called $\alpha$ "a log-odds shift"; both were wrong by a factor of two:
$\alpha$ is half the log-odds shift.)

## What the manuscript may and may not attribute to Field

The manuscript cites Field in the footnote at MS ~117, "for how Jeffrey updates are
asymmetric in the delivered-credence parametrisation". That is accurate (p. 365;
`jeffrey_not_comm`). It may add that in Field's `α` parametrization successive
updates commute (eq. (7); `field_eq7_comm`). It should not say that Field's rule is
the Bayes-factor rule without the proviso that each `α` is computed against the prior
marginal, and it should not describe `e^{2α}` as anything but the likelihood ratio.
