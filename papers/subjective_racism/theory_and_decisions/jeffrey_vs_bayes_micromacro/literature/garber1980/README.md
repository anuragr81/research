# Garber, D. (1980), "Field and Jeffrey Conditionalization" (Discussion)

*Philosophy of Science* 47(1), 142-145.

All five PDF pages (JSTOR cover plus pp. 142-145) were read on the rendered pages.
The text layer is clean for the prose but garbles the displayed equations (3)-(4),
which were read from the page images. (An earlier version of this note said no image
rendering was needed; for the equations it is.)

## Claims formalized

Garber's opening thesis (p. 142): "I shall argue that Field's proposed revision of
Jeffrey's formula is neither correct nor necessary." His counterexample: repeated,
phenomenally identical, weakly informative glances at a ball in dim light, each
processed with Field's (1978) update and the *same* input parameter $\alpha$, drive
the belief in "the ball is blue" from a modest start to near certainty in a handful
of repetitions. Garber's eq. (3) is Field's (5), $q = pe^\alpha/(pe^\alpha+(1-p)e^{-\alpha})$;
his eq. (4) is Field's definition (4), $\alpha = \tfrac12\log((q/p)/((1-q)/(1-p)))$.

- **First example** (pp. 143-144): prior $P_0(E)=.3$; one glance raises it to
  $P_1(E)=.4$; "an α value of .2209 (to four places)". Repeating the same $\alpha$
  gives the table on p. 144:
  $.3,\ .4,\ .5091,\ .6173,\ .7150,\ .7961,\ .8586,\ .9043,\ .9363,\ .9581$
  ("after *nine* repetitions of the *same* rather uninformative experience, S will
  become *virtually certain*").
- **Second example** (p. 144): a "slightly richer" experience, $.3\to.5$, "would have
  taken only *five* repetitions of the experience to raise S's degree of belief in E
  above .95".
- **A typo in the paper.** Garber's prose on p. 143 says the second value "will be
  .5019 (to four places)"; his own table and the computation give $.5091$.

The exact mechanism: one Field step with $\alpha$ multiplies the odds of $E$ by
$e^{2\alpha}$, the likelihood ratio. For $.3\to.4$ that ratio is exactly $14/9$, so
$P_n(E) = 3\cdot14^n/(3\cdot14^n+7\cdot9^n)$; for $.3\to.5$ it is $7/3$ and
$P_n(E) = 3\cdot7^n/(3\cdot7^n+7\cdot3^n)$.

## Lean

`lean/Garber.lean` is a symlink to `lean/Literature/Garber.lean` (namespace
`Literature.Garber`, standalone, `import Mathlib` only). It builds with no `sorry`,
and every theorem depends only on `propext`, `Classical.choice` and `Quot.sound`.
All thresholds are proved exactly, in rationals, through the odds form; the only
real-analysis numerics are in `garber_alpha_4dp`.

| Theorem | What it establishes |
|---|---|
| `odds_fieldStep` | one Field step multiplies the odds by `e^{2α}` |
| `odds_iterate`, `fieldStep_iterate` | `n` steps with a fixed `α` multiply the odds by `e^{2nα}`; equivalently, they are one step with `nα` |
| `garber_lr` | Garber's `α` for `.3 → .4` has `e^{2α} = 14/9` exactly |
| `garber_alpha_4dp` | `.22085 < α < .22095`, i.e. `α = .2209` to four places (via `Real.exp_bound`) |
| `garberSeq_closed` | `Pₙ(E) = 3·14ⁿ/(3·14ⁿ + 7·9ⁿ)` |
| `garber_table` | each of the ten table entries is `Pₙ(E)` to four places (`\|Pₙ - tableₙ\| < 1/20000`) |
| `garber_prose_typo` | `P₂(E) = 588/1155`, which is not `.5019` to four places |
| `garber_nine` | `P₉(E) > .95 > P₈(E)` |
| `richer_lr`, `richerSeq_closed`, `richer_five` | `.3 → .5`: `e^{2α} = 7/3`, `Pₙ = 3·7ⁿ/(3·7ⁿ + 7·3ⁿ)`, `P₅ > .95 > P₄` (five is the first repetition above `.95`) |
| `fieldStep_iterate_tendsto_one` | for every `α > 0` and `0 < p < 1`, repetition drives `P(E) → 1` |

## SymPy

`sympy/check_repeated_glances.py` asserts every claim above and exits 0 iff all pass
(21/21): `e^{2α} = 14/9` exactly, `α = .2209` to four places, the iterated eq. (3)
equals the odds form exactly, each table entry to four places, the `.5019` typo,
`P₉ > .95 > P₈`; for the richer experience `e^{2α} = 7/3`, `P₅ ≈ .9674 > .95 >
.9270 ≈ P₄`.

## The philosophical point, not formalized

Garber's dilemma (p. 145): if $P_1(E)$ is independent of $P_0(E)$, rules (1) and (2)
(ordinary and Jeffrey conditionalization) "are clearly applicable", $P_1(E)$ already
meets Field's requirements for an input parameter, and no reparametrization is
needed; if $P_1(E)$ is not independent of $P_0(E)$, "there is reason to believe that
we are dealing with a situation in which conditionalization of any sort is just not
appropriate", and again reparametrization is not necessary. His response is "not the
reparameterization that Field attempts, but the far more interesting (and far more
difficult) task of finding an alternative way of characterizing rational belief
change" (p. 145). The thesis that Field's revision is "neither correct nor necessary"
is stated at the outset (p. 142), not as the conclusion. Garber never discusses the
order of updates, only repetition of the same experience on the same $E$.

## Bearing on Paper B

This is the counterexample Hawthorne answers. Garber's repeated glances are a
*within-basis* phenomenon (the same event $E$, glanced at repeatedly), whereas Paper
B's two cues are *distinct-basis* by construction (Assumption `as:local`), the
configuration in which Hawthorne shows Field/Bayes-factor updating is safest. So
Garber's objection does not directly threaten Paper B's benchmark $\PB$: it is a
caution about reusing a *fixed* portable $\alpha$ under *repetition on the same
basis* (`fieldStep_iterate_tendsto_one`), not about combining two cues on different
attributes.

## What the manuscript may and may not attribute to Garber

The manuscript does not cite Garber. The notes' gloss that Garber "objects that a
portable factor compounds implausibly under repetition" is accurate (p. 144;
`garber_nine`, `fieldStep_iterate_tendsto_one`). If he is cited, quote the opening
thesis as the thesis (p. 142) and keep the parentheses in "the far more interesting
(and far more difficult) task" (p. 145).
