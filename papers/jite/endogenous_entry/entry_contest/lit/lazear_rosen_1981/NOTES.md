# Notes — Lazear & Rosen (1981), pass 1

Run `python3 verify_lr.py` from the repository root. Pass 2 (2026-10-06)
gives **12 checks, 0 failures**, including the Lean build and axiom audit of
`LazearRosen.lean`. Pass 1 ran 6 checks.

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

Pass 1 did **not** recompute Table 1 and checked only its internal consistency
(LR-5). Pass 2 recomputes it in LR-7, and the printed E(U) column does not
follow from the paper's own formulas (finding F3 below). The LR-5 facts stand
as transcriptions: the reported `s(y₀) = .005` at `y₀ = 100` and `.020` at `y₀ = 25` do
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
| LR-6 | CONSISTENT (pass 2, corrected). `(30)` with `(29)`, the p.862 zero-profit constraint and `(7)` reduces to `(32)`, `y_a(h) = V·(Δμ/2 − h)` with no factor `g`; `h* = Δμ/2`; `y_a + y_b = 0`; `dy_a/dh = −V`. Pass 1 asserted `V·g·(Δμ/2 − h)`, which the control now rejects. |
| LR-7a | CONSISTENT. Under `(16)`, `(17)`, `(24)`, `(25)` with `V = 1`, `C = μ²/2`, certainty-equivalent net income is `μ/2` for the piece rate and `μ*/2` for the contest, so the piece rate is preferred at every `s, σ² > 0`. |
| LR-7b | DISCREPANCY IN THE SOURCE. Table 1's μ, μ* and E(U*) columns reproduce from those formulas; its E(U) column lies 0.000163 to 0.000362 below them in every row. Finding F3. |
| LR-L | Lean. `LazearRosen.lean` compiles with no errors or warnings, has no `sorry`, and all 28 theorems use at most `propext` and `Quot.sound`. |

## Not attempted

- Table 1's numbers were not recomputed in pass 1. Pass 2 recomputes them from
  the paper's approximations (16), (17), (24), (25) in LR-7. An exact
  optimisation of the two contracts under normal errors is still not
  attempted.
- §II's prize-structure derivation, (11)–(12).
- §III's comparative statics on `μ` under risk aversion; footnote 10's claim
  that `μ` sits below the wealth-maximising level.
- The linearisation `P̄ ≐ ½ + [g(Δμ − h)](Δμ − h)` of p.862. Pass 2 grants it
  and derives `(32)` from `(30)` exactly.
- §IV's efficiency claims for mixed leagues beyond `(32)`'s algebra.

Anything leaning on these is **[A]-grade** despite the [F] tag.

## On LR-C, the handicap "tool"

LR-6 confirms the algebra the handover wants to use: `h* = Δμ/2`, gains
zero-sum, `y_a` decreasing in `h` with slope `−V`. But note what §IV is *about* — it makes
**mixed** contests efficient between two known types who both participate. It
is a handicapping result, not an abstention result. Whether it is the right
tool for "an equilibrium in which the incumbent abstains while a challenger
invests" is a modelling judgement and is **not** established by this pass. The
nearest usable piece is `y_a(h)`, the gain from playing a mixed match rather
than an own-type one, which is a participation-selection object; adapting it to
an abstention margin is untried.

---

## Pass 2, 2026-10-06. Source re-read in full, Lean formalisation

### Source identity

The source is the Drive file `1981-lazear.pdf` (id
`1K2jyuShICvvy0On7b0fEku7GQDatN9uS`), 24 pages, a JSTOR scan of the published
article. Its title page, p.841, carries the title "Rank-Order Tournaments as
Optimum Labor Contracts", the authors Edward P. Lazear and Sherwin Rosen, and
the line "Journal of Political Economy, 1981, vol. 89, no. 5". The PDF
metadata gives pp. 841-864. The text layer was read in full. The text layer
garbles displayed mathematics, so equations (6), (7), (10), (16), (17),
(21)-(25), (28)-(32), footnote 8 and Table 1 with its note were read from page
images rendered from the same file. Every quotation below is checked against
those images.

### Findings against our documents

**F1. Equation (32) has no factor `g`.** p.862 prints "γa(h) ≐ V · (Δμ/2 −
h). (32)". Pass 1 wrote `V·g·(Δμ/2 − h)` and `dy_a/dh = −Vg` in
`RECONSTRUCTION.md` §4, in this file and in check LR-6. The factor cancels
because the spread of (29), `ΔW̃ = V/g(Δμ − h)`, multiplies the density in the
linearised win probability `P̄ ≐ ½ + [g(Δμ − h)](Δμ − h)`, which leaves
`V(Δμ − h)`. LR-6 now derives (32) from (29), (30), the p.862 zero-profit
constraint and (7), and its control rejects the form with `g`.
`handicap_gain` proves the same reduction in Lean. The competitive handicap
`h* = Δμ/2` and the signs of the gains are the same under either form because
`g > 0`, so `LITERATURE.tex` and `PROOFS.tex`, which use only `h* = Δμ/2`, are
unaffected. `RECONSTRUCTION.md` §4 is corrected in place. The paper names the
gain `γa(h)`, where our documents write `y_a`.

**F2. Table 1 shows the wealth sorting at one of the three variances it
tabulates for both endowments.** `LITERATURE.tex` (§Background and toolkit)
says "decreasing absolute risk aversion makes the rich prefer the tournament
and the poor prefer the piece rate". The paper states the sorting as a
likelihood and a possibility. p.855 has "persons with more endowed income and
smaller absolute risk aversion are more likely to prefer contests" and "If this
difference is large enough, it can be optimal to pay piece rates to those with
small values of endowed income and to pay prizes to those with large values."
Table 1 tabulates σ² = .1, 1 and 12 for both endowments. At σ² = 1 the rich
prefer the contest and the poor prefer the piece rate. At σ² = .1 both
endowments prefer the contest (5.012465 > 5.012155 and 2.524725 > 2.524665).
At σ² = 12 both prefer the piece rate (5.011420 > 5.010515 and 2.519930 >
2.514282). Read off the two p.854 sentences quoted under `rich_contest_rows`
and `poor_contest_rows` below, the sorting window is .2 ≤ σ² < 3. The
`LITERATURE.tex` sentence says "makes" where the paper says "more likely" and
"can be", and it omits the σ² condition. `PROOFS.tex` says "They
\emph{suggest} the sorting under declining absolute risk aversion", which
matches the source. Falsifier. Were the sorting printed at σ² = .1 or σ² = 12
as well, the unconditional sentence would be a fair summary, and
`sorting_only_at_unit_variance` decides from the printed rows that it is not.

**F3. Table 1's E(U) column does not follow from the approximations the paper
gives for it.** This is an inference from recomputation (LR-7), not a
statement of the paper. The table note fixes "U = αy^α" and "α = .5, V = 1,
C(μ) = μ²/2" (p.854). Footnote 8 (p.852) says "All these approximations use
first-order expansions for terms in U′(·) and second-order expansions for
terms in U(·). The same is true of the approximations below for the
tournament." Under (16) and (17) for the piece rate and (24) and (25) for the
contest, with those values, certainty-equivalent net income is μ/2 for the
piece rate and μ*/2 for the contest (LR-7a). The piece rate is then preferred
whenever μ* < μ, and p.853 states that ordering, "Equations (16) and (24)
indicate that investment and expected income are lower for the contest than
for the piece rate at given values of s" (footnote marker 10 after "income"
omitted). The gap is (π − 1)x / 2(1 + x)(1 + πx) with x = sσ², positive for
every s and σ². Recomputing the table with s evaluated at mean income, as
p.852 defines s, gives three results (LR-7b).

- All 20 printed μ and μ* values equal the recomputed values truncated to four
  places.
- 8 of the 10 printed E(U*) values lie within 5e-6 of the recomputed values.
  The printed value is 0.000057 above the recomputed one at y0 = 100,
  σ² = .5 and 0.000037 below it at y0 = 25, σ² = 12.
- Every printed E(U) lies below the recomputed value, by 0.000163 to 0.000362.
  The recomputed piece-rate E(U) exceeds the printed contest E(U*) in all 10
  rows. The four rows in which the table prints a contest preference
  (σ² = .1, .5 and 1 at y0 = 100, σ² = .1 at y0 = 25) therefore reverse, and
  the σ² = 1 comparison quoted on p.855 reverses with them.

Falsifier. The printed E(U) could come from a computation the paper does not
describe. Three facts weigh against that. The contract r = μ, I = μ(1 − μ)
satisfies (13) and (14), so an exact optimisation over piece rates can only
raise the piece rate's expected utility above that contract's value. The
exact normal expectation of U at that contract, computed by quadrature in
LR-7b, exceeds the printed E(U) by 0.000163 to 0.000361 and exceeds the
printed E(U*) in all 10 rows. And the gap is already 0.000323 at y0 = 100,
σ² = .1, where the second-order risk term is 6.2e-6, so the gap is not a
neglected risk term. Evaluating s at each scheme's own mean income does not
rescue the contest either. Under declining absolute risk aversion the
contest's lower mean income gives it the larger s, which lowers
μ* = 1/(1 + πsσ²) and with it the contest's certainty equivalent μ*/2.
Neither `LITERATURE.tex` nor `PROOFS.tex` quotes a Table 1 number, so neither
is wrong on this count. The bearing on LR-A is that the 1981 example, which
its authors already call an example, may not survive its own approximations.
This is recorded for the author and is not proposed as manuscript text.

**F4. The adverse-selection result assumes private types, and the handicap
result assumes public types.** `LITERATURE.tex` says "\S IV (heterogeneous
ability, asymmetric information) gives adverse selection into the top league
and a competitive handicap $h^* = \Delta\mu/2$". The adverse-selection
subsection opens on p.858 with "Suppose that each person knows to which class
he belongs but that this information is not available to anyone else." The
handicap subsection opens on p.861 with "This section moves to the opposite
extreme of the previous discussion and assumes that the identities of each
type of player are known to everyone." The parenthetical in `LITERATURE.tex`
attaches asymmetric information to both results. Any use of the handicap
algebra for LR-C inherits the assumption that every player's type is known to
everyone.

**F5. The third-derivative statements are ours, and the paper's DARA is their
premise.** The paper never mentions u‴, prudence or quadratic utility. Its
DARA statement is on p.854, "However, when there is declining absolute risk
aversion, we have examples where the contest dominates the piece rate", and s
is defined on p.852, "where s ≡ −U″/U′ evaluated at mean income is the measure
of absolute risk aversion." `PROOFS.tex` presents "$A'(w)<0$ has numerator
$(u'')^2 - u'''u'$ and so requires $u'''>0$" as its own derivation about DARA
and not as a statement of the paper, which is the correct attribution. LR-L1
and LR-L2 check that derivation.

### Lean formalisation

`LazearRosen.lean` is one core Lean 4 file with everything in
`namespace LazearRosen`. It has no Mathlib, no import, no comment, no `sorry`,
no user `axiom` and no `native_decide`. It compiles with `lean
LazearRosen.lean` under the toolchain on this machine, which `lean --version`
reports as 4.34.1, where the brief named 4.33.1. Analytic facts enter as named
hypotheses in the style of `Dispersive.lean`.

- `QuotientRule u1 u2 u3 dA` is the quotient rule for A = −u″/u′ at one wealth
  level, A′·(u′)² = (u″)² − u‴u′, with u′, u″, u‴ and A′ as values.
- `StrictlyConcave u` says the increments u(x + 1) − u(x) strictly decrease,
  the discrete form of u″ < 0. `burden u c w` is κ(w) = u(w) − u(w − c).
- `Handicap` bundles the mixed-league zero-profit constraint of p.862, the
  own-league constraints from (7), the spread (29) written as `g·ΔW̃ = V`, and
  the linearised win probability of p.862 doubled to `2P̄ = 1 + 2g(Δμ − h)`.
  `gainA2` and `gainB2` are (30) and (31) multiplied by 2.
- Table 1 enters as scaled naturals, σ² × 10, μ × 10⁴ and E(U) × 10⁶, and each
  table theorem is decided by evaluation.

All quantities are integers. Each proof combines the hypotheses ring-linearly
and uses sign facts that hold in every ordered ring, so the same steps go
through over the rationals and the reals, but only the integer instance is
compiled. The derivative values of the quadratic, u′ = b − 2gw, u″ = −2g and
u‴ = 0, are analytic input passed through `QuotientRule`. The discrete second
and third differences of `quad`, −2g and 0, are proved.

Method note for later bundles. `omega` applied to a goal that is a
conjunction pulls in `Classical.choice`, and the same `omega` applied to each
conjunct separately does not. Every conjunction here is split before `omega`.
`grind` also pulls in `Classical.choice` and is not used.

### Theorem by theorem

The quotation column gives the paper verbatim where the paper states the
content. Where the content is our own derivation, the column quotes the
document sentence the theorem checks and says so.

| Theorem | ID | Locator | Paper verbatim, or the document sentence checked |
|---|---|---|---|
| `dara_iff_numerator_neg` | LR-L1 | p.852 below (17); p.854 | Paper, p.852, "where s ≡ −U″/U′ evaluated at mean income is the measure of absolute risk aversion." Checked against `PROOFS.tex`, "$A'(w)<0$ has numerator $(u'')^2 - u'''u'$" |
| `dara_forces_positive_third` | LR-L1 | p.854, §III *Comparisons* | Paper, p.854, "However, when there is declining absolute risk aversion, we have examples where the contest dominates the piece rate." Checked against `PROOFS.tex`, "and so requires $u'''>0$" |
| `concave_zero_third_is_iara` | LR-L2 | not in the paper | Our claim. `PROOFS.tex`, "it is concave, has $u'''=0$, and exhibits \emph{increasing} absolute risk aversion" |
| `positive_marginal_utility_needed` | LR-L3 | control | With u′ = −1, u″ = 0, u‴ = −1 the quotient rule gives A′ = −1 < 0 while u‴ < 0, so `dara_forces_positive_third` needs u′ > 0 |
| `strict_concavity_needed_for_iara` | LR-L3 | control | With u″ = u‴ = 0 the quotient rule forces A′ = 0, so `concave_zero_third_is_iara` needs u″ < 0 |
| `burden_falls_of_concave` | LR-L4 | not in the paper | Our claim. `PROOFS.tex`, "Here the sorting runs through $\kappa(w) = u(w)-u(w-c)$ being decreasing, which follows from $u''<0$ alone." |
| `linear_utility_burden_flat` | LR-L4 | control | Linear utility is not strictly concave, and its burden is the same at w and w + 1 |
| `quad_incr` | LR-L5 | not in the paper | The increment of b·w − g·w² at x is b − g(2x + 1) |
| `quad_second_and_third_difference` | LR-L5 | not in the paper | Second difference −2g and third difference 0, the discrete u″ < 0 and u‴ = 0 |
| `quad_strictly_concave` | LR-L5 | not in the paper | g > 0 makes the quadratic strictly concave |
| `quad_burden_step` | LR-L5 | not in the paper | κ(w + 1) − κ(w) = −2gc exactly, whatever w |
| `quadratic_separates` | LR-L5 | not in the paper | Our claim. `PROOFS.tex`, "so $\kappa$ still sorts while DARA fails outright" |
| `documents_witness` | LR-L5 | pass 1 LR-3 | `quad 8 1` is 8 times u = w − w²/8. At w = 3 the burden step is −2c, which is −c/4 for u itself, and A′(3) = 1, the values in the pass-1 LR-3 table |
| `handicap_zero_sum` | LR-L6 | p.862, (30), (31) | "The zero-profit constraints in a-a, a-b, and b-b require that γa(h) + γb(h) = 0 for all admissible h." |
| `handicap_gain` | LR-L6 | p.861 (29); p.862 (32); p.846 (7) | "If Ca(μ) is not greatly different from Cb(μ), then Δμ = μa* − μb* is small and P̄ ≐ ½ + [g(Δμ − h)](Δμ − h). This approximation and the zero-profit constraint reduce (30) to γa(h) ≐ V · (Δμ/2 − h). (32)" |
| `competitive_handicap` | LR-L6 | p.862 | "Therefore, h* = Δμ/2 is the competitive handicap, since it implies γa(h*) = γb(h*) = 0. If the actual handicap is less than h*, then γa is positive and a's prefer to play in mixed contests rather than with their own type, while b's prefer to play with b's only. The opposite is true if h > h*." |
| `competitive_handicap_not_fair` | LR-L6 | p.862 | "The competitive handicap does not result in a fair game, since h* = Δμ/2 < Δμ." |
| `zero_sum_needs_mixed_zero_profit` | LR-L7 | control | Own-league zero profit holds and mixed-league zero profit fails, and the doubled gains sum to 4 rather than 0 |
| `gain_needs_spread_rule` | LR-L7 | control | Zero profit and the linearisation hold with the spread off (29), and the doubled gain is 5 where the doubled (32) gives 1 |
| `hypotheses_satisfiable` | LR-L8 | non-vacuity | Log utility at w = 1 (u′ = 1, u″ = −1, u‴ = 2, A′ = −1) meets the DARA hypotheses. The documents' quadratic at w = 3 and one integer handicap instance meet theirs |
| `table_s_values` | LR-L9 | p.854 and Table 1 | "Illustrative calculations are shown in table 1 using the utility function U = αy^α, which exhibits constant relative but declining absolute risk aversion, s(y) = (1 − α)/y." Table headers "y0 = 100; s(y0) = .005" and "y0 = 25; s(y0) = .020" |
| `rich_contest_rows` | LR-L9 | p.854 | "Table 1 shows that when y0 = 100 so that s = .005, the contest is preferred until σ² ≥ 3." |
| `poor_contest_rows` | LR-L9 | p.854 | "However, if y0 = 25 so that s = .020, the contest is only preferred for σ² < .2." |
| `sorting_at_unit_variance` | LR-L9 | p.855, *Income Distributions* | "for example, if σ² = 1 then the rich prefer contests (5.012295 > 5.012100) and the poor prefer piece rates (2.524237 > 2.523437), but μ* = .9846 exceeds μ = .9807." |
| `shared_variances` | LR-L9 | Table 1, p.854 | The y0 = 100 panel lists σ² = .1, .5, 1, 3, 6, 12 and the y0 = 25 panel lists σ² = .1, .2, 1, 12 |
| `sorting_only_at_unit_variance` | LR-L9 | p.855, *Income Distributions* | "If this difference is large enough, it can be optimal to pay piece rates to those with small values of endowed income and to pay prizes to those with large values." Finding F2 |
| `table_investment_orderings` | LR-L9 | pp.852-853 | p.853, "Equations (16) and (24) indicate that investment and expected income are lower for the contest than for the piece rate at given values of s." p.852, "Investment increases (see [16]) in V and decreases in s, C″, and σ²" |
| `rows_below_variance_threshold` | LR-L10 | p.853, §III *Comparisons* | "Moreover, for values of σ² in excess of 1/sC″√π, the variance of income in the tournament is smaller than for the piece rate." The theorem decides 4x² < 1 for x = s(y0)σ² in every row. With the analytic input π < 4 that gives πx² < 1, and LR-7a factors the variance gap as (π − 1)(πx² − 1), so in every row the contest has the larger income variance |

The hypothesis fields of `Handicap` come from p.861, "ΔW̃ = V/g(Δμ − h).
(29)", from p.862, "Prizes W̃1 and W̃2 must also satisfy the zero-profit
constraint W̃1 + W̃2 = V · (μa* + μb*) independent of h", and from p.846,
"Vμ = (W1 + W2)/2. (7)".

### Non-vacuity and the audit

Rule 3 of `../README.md` asks that every check can fail. The controls are
`positive_marginal_utility_needed`, `strict_concavity_needed_for_iara`,
`linear_utility_burden_flat`, `zero_sum_needs_mixed_zero_profit` and
`gain_needs_spread_rule`. Each one exhibits a concrete instance in which a
conclusion fails once one hypothesis is dropped. `hypotheses_satisfiable`
shows that the hypothesis bundles of LR-L1, LR-L5 and LR-L6 have instances, so
those theorems are not vacuous. `sorting_only_at_unit_variance` would fail if
Table 1 printed the sorting at σ² = .1 or 12. In Python, the LR-6 control
rejects the form with `g`, and LR-7b fails if one printed E(U) is replaced by
its recomputed value, which was tried on a scratch copy.

The audit in `verify_lr.py` compiles the file, fails if `lean` is not on PATH,
fails on a nonzero return, on any compiler output, on any `sorry`, on
`axiom`, `import`, `native_decide` or a comment, and runs `#print axioms` on
every name matched by `^\s*(?:theorem|lemma)\s+([A-Za-z0-9_']+)`. It also
fails if the number of matched names differs from the number of `theorem`
and `lemma` keywords, so a name the pattern misses cannot pass silently. The
allowed axioms are `propext` and `Quot.sound`. The audit was mutation-tested
on scratch copies. A theorem closed by `omega` on a conjunction fails it
through `Classical.choice`. The same theorem under the name `t2'x9` is audited
and fails, which shows that names with digits and primes are covered. A
`sorry` fails three checks, and removing `lean` from PATH fails the build
check.

The audit reports 28 theorems, 28 audited and 0 sorry. Nine theorems use no axiom, three
use `propext` only and sixteen use `propext` and `Quot.sound`. No theorem uses
`Classical.choice`.

## Quotations used in `MANUSCRIPT.tex`

One line each, copied from the block quotations above so the manuscript
checker can match them. No new reading.

| Page | Quotation |
|---|---|
| p.855 | "Individuals will self-select the payment scheme in accordance with their wealth." |
