# Notes — Lazear & Rosen (1981), pass 1

Run: `python3 verify_lr.py` — **6 checks, 0 failures**.

**Scope.** Our reading is arithmetically consistent. The survey's channel
claim is confirmed decisively; two qualifications are added, one of which
strengthens the paper's position and one of which the survey omits. Nothing
here reproves any Lazear–Rosen result.

---

## The precedent is real — the survey was right to flag it

p.855, subsection *Income Distributions*, states it plainly:

> "If this difference is large enough, it can be optimal to pay piece rates to
> those with small values of endowed income and to pay prizes to those with
> large values. **Individuals will self-select the payment scheme in accordance
> with their wealth.**"

Wealth-based self-selection into a tournament is in print in 1981. The Verdict
is right that the contribution sentence cannot ignore it, and `PROOFS.tex`
currently cites nothing at all at this point.

---

## Qualification 1: it is an example, and the authors say so

The subsection's very first clause:

> "**While it is not possible to make a general argument based on an example**,
> table 1 suggests that persons with more endowed income and smaller absolute
> risk aversion are more likely to prefer contests..."

The result rests on Table 1: a specific utility (`U = a·y^a`), quadratic costs,
normal errors, two values of `y₀`. The authors decline to generalise it.

This materially changes how the acknowledgement should read. P5's threshold
structure and P9's sign rule are theorems, distribution-free. The honest
sentence is that Lazear–Rosen **suggest** the sorting on an example under DARA,
while the present model **derives** it under concavity alone — not that they
established it and we restate it.

I did **not** recompute Table 1; only its internal consistency was checked
(LR-5): the reported `s(y₀) = .005` at `y₀ = 100` and `.020` at `y₀ = 25` do
follow from `s(y) = (1−a)/y` at `a = 1/2`, and the reported comparisons at
`σ² = 1` do point the way the text says (rich prefer the contest
`5.012295 > 5.012100`; poor prefer the piece rate `2.524237 > 2.523437`).

---

## Qualification 2: the channel difference is real — verified decisively

This is the load-bearing claim in the survey's instruction, "(`u''` here,
`u'''` there)", and it holds.

**Our channel (LR-1).** `κ(w) = u(w) − u(w−c)` has
`κ'(w) = u'(w) − u'(w−c) < 0` whenever `u'' < 0`, because `u'` is then
decreasing and `w > w−c`. Verified on five concave utilities — log, √, CRRA,
CARA, quadratic. **No condition on `u'''` is used anywhere.**

**Their channel (LR-2).** DARA means `A'(w) < 0` for `A = −u''/u'`. The
numerator of `A'` is `(u'')² − u'''u'`, so `A' < 0` requires
`(u'')² < u'''u'`. With `u''' = 0` the left side is strictly positive and
`A' > 0`: increasing absolute risk aversion, never DARA. **`u'''` enters
essentially.**

**LR-3, the decisive witness.** Take `u = w − w²/8` on `w < 4`:

| quantity | value |
|---|---|
| `u''` | `−1/4` (concave) |
| `u'''` | `0` |
| `κ'(w)` | `−c/4 < 0` — **our sorting operates** |
| `A(w)` | `−1/(w−4)` |
| `A'(3)` | `+1 > 0` — IARA, so **their sorting fails** |

A single primitive on which our channel works and theirs does not. The
distinction is substantive, not verbal.

*Method note:* this reuses the ST-12a template from
`../schroyen_treich_2016/` — to test a claimed channel distinction, exhibit one
primitive that separates them. There, `P` varied with `A` fixed; here, `u'''`
varies with concavity fixed.

---

## A second separation the survey does not name

The survey frames the difference purely as `u''` versus `u'''`. There is a
second, arguably more important one: **the object of choice differs**.

- Lazear–Rosen: a choice **between two compensation schemes**, piece rate or
  tournament, *both of which pay*. The sorting is about tolerance for the
  binomial spread of a lottery versus a concentrated normal — a pure
  risk-preference story.
- entry_contest: a choice **whether to pay a fixed entry cost at all**. The
  sorting is about affordability of a fee in utility units.

These are different economics, not merely different derivatives. A referee who
accepts the `u''`/`u'''` point might still ask why that matters; the
choice-object difference is the more robust answer, and it should be in the
contribution sentence alongside the derivative point.

---

## Results

| ID | Result |
|---|---|
| LR-1 | CONSISTENT. `κ` strictly decreasing under concavity alone, on five utilities including one with `u''' = 0`. |
| LR-2 | CONSISTENT. `A'`'s numerator is exactly `(u'')² − u'''u'`; DARA is a third-derivative condition. |
| LR-3 | CONSISTENT and decisive. Quadratic utility separates the two channels. |
| LR-4 | CONSISTENT. `U = a·y^a` gives `A(y) = (1−a)/y` and constant relative risk aversion `1−a`, matching the paper's stated `s(y)` exactly. |
| LR-5 | CONSISTENT. Table 1's `s(y₀)` values follow from `a = 1/2`, and the reported comparisons point as the text says. |
| LR-6 | CONSISTENT. `(32)` gives `h* = Δμ/2`, `y_a + y_b = 0` identically, `dy_a/dh = −Vg < 0`. |

## Not attempted

- Table 1's numbers were **not** recomputed (they need the authors' quadratic
  cost and normal-error specification and their optimisation over `μ`).
- §II's prize-structure derivation, (11)–(12).
- §III's comparative statics on `μ` under risk aversion; footnote 10's claim
  that `μ` sits below the wealth-maximising level.
- The approximation from `(30)` to `(32)`.
- §IV's efficiency claims for mixed leagues beyond `(32)`'s algebra.

Anything leaning on these is **[A]-grade** despite the [F] tag.

## On LR-C, the handicap "tool"

LR-6 confirms the algebra the handover wants to use: `h* = Δμ/2`, gains
zero-sum, `y_a` decreasing in `h`. But note what §IV is *about* — it makes
**mixed** contests efficient between two known types who both participate. It
is a handicapping result, not an abstention result. Whether it is the right
tool for "an equilibrium in which the incumbent abstains while a challenger
invests" is a modelling judgement and is **not** established by this pass. The
nearest usable piece is `y_a(h)`, the gain from playing a mixed match rather
than an own-type one, which is a participation-selection object; adapting it to
an abstention margin is untried.
