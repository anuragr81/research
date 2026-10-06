# Reconstruction — Lazear & Rosen (1981)

**Source.** Edward P. Lazear and Sherwin Rosen, "Rank-Order Tournaments as
Optimum Labor Contracts", *Journal of Political Economy* 89(5), 1981,
pp. 841–864. Published version, text layer present (OCR spacing is degraded
but legible), 24pp.

This is the paper that most directly threatens the novelty claim: the survey's
Verdict says its §III already has *"agents self-selecting into a tournament by
endowed wealth, through DARA; the mechanism has a 1981 precedent."*

---

## 1. Structure

- §I Introduction
- §II Piece Rates and Tournaments with Risk Neutrality
- §III **Optimal Compensation with Risk Aversion** — containing the
  subsections *Prize Structure*, ***Income Distributions***, *Error Structure*
- §IV Heterogeneous Contestants — containing *Handicap Systems*
- §V Summary and Conclusions

The survey cites "§III 'Income Distributions'". That is accurate: it is a
subsection heading inside §III, not a section of its own.

## 2. The core model (§II)

Contestants invest `μ` at cost `C(μ)`; output is `μ + ε` with noise; the
winner of a two-person tournament takes `W₁`, the loser `W₂`. Investment
incentives are driven by the **spread** `ΔW = W₁ − W₂` against the density of
the noise difference: the first-order condition sets `g(0)(W₁−W₂) = C'(μ)`.
Under risk neutrality, tournaments and piece rates are equivalent in inducing
the efficient `μ*`.

## 3. §III — where risk aversion bites, and where the precedent lives

With risk aversion the equivalence breaks, because a tournament pays a
**binomial** income (`W₁` or `W₂`, half the weight at each) while a piece rate
pays a **normal** income concentrated near its mean. Which is preferred depends
on the worker's risk attitude *and* on `σ²`.

The paper's own illustration uses `U = a·y^a`, which has

> `s(y) = (1−a)/y` — constant relative, **declining absolute** risk aversion

and a nonlabor income `y₀` is added so the domain stays positive. Table 1
reports `E(U)` (piece rate) against `E(U*)` (tournament) for `y₀ = 100`
(`s = .005`) and `y₀ = 25` (`s = .020`).

**The Income Distributions subsection, quoted in full at the load-bearing
point:**

> "**While it is not possible to make a general argument based on an example**,
> table 1 suggests that persons with more endowed income and smaller absolute
> risk aversion are more likely to prefer contests, and those with low levels
> of endowed wealth and larger absolute risk aversion are more likely to prefer
> piece rates. Consider a situation in which all persons have the same utility
> function [...] the only difference being the fact that some workers have
> larger endowed incomes than others. If this difference is large enough, it
> can be optimal to pay piece rates to those with small values of endowed
> income and to pay prizes to those with large values. **Individuals will
> self-select the payment scheme in accordance with their wealth.**"

So the precedent is real — and it is explicitly **an example, not a theorem**.
The authors disclaim generality in the first clause.

**The mechanism.** Richer workers have lower absolute risk aversion
(`s(y) = (1−a)/y` falls in `y`), so they tolerate the binomial spread of the
tournament; poorer workers prefer the concentrated piece-rate distribution.
The channel is **declining absolute risk aversion**.

## 4. §IV — handicaps

Two known types `a`, `b` with socially optimal investments `μ*_a`, `μ*_b`,
difference `Δμ`. In a mixed match with handicap `h` to the inferior player,
the spread must satisfy `(29) ΔW = V/g(Δμ − h)`. The gain to an `a` from
playing a handicapped `b` rather than another `a` is `(30) y_a(h)`, and for
small `Δμ` this reduces to

> `(32)`  `γa(h) ≐ V · (Δμ/2 − h)`

(Corrected 2026-10-06 against the p.862 page image. Pass 1 wrote
`V·g·(Δμ/2 − h)` here. The paper's (32) has no factor `g`, because the spread
`V/g(Δμ − h)` of (29) cancels the density in the linearised win probability.
The paper names the gain `γa`; this file and the suite write `y_a`.)

with `y_b` the same and sign-reversed, so `y_a(h) + y_b(h) = 0` for all
admissible `h`. Hence `h* = Δμ/2` is the **competitive handicap**: below it,
`a`'s prefer mixed contests and `b`'s prefer segregated ones; above it, the
reverse. The competitive handicap is *not* a fair game
(`h* = Δμ/2 < Δμ`).

## 5. What could not be reconstructed

1. §II's derivation of the optimal prize structure and the zero-profit
   conditions (11)–(12).
2. §III's full comparative statics on `μ` under risk aversion, including the
   claim that `μ` lies below the wealth-maximising level (footnote 10).
3. Table 1's numbers themselves — these are the authors' computations under
   quadratic costs and normal errors, and were **not** recomputed here. Only
   the internal consistency of the reported `s(y₀)` values and the direction of
   the reported comparisons was checked. (Pass 2, 2026-10-06, recomputes the
   table from (16), (17), (24) and (25) in check LR-7. See finding F3 in
   `NOTES.md`.)
4. The approximation step from `(30)` to `(32)`. (Pass 2 derives `(32)` from
   `(30)` exactly once the linearised win probability of p.862 is granted, in
   LR-6 and in `handicap_gain` of `LazearRosen.lean`. The linearisation itself
   stays assumed.)
5. §IV's efficiency claims for mixed leagues beyond the algebra of `(32)`.

Anything leaning on these is **[A]-grade** despite the [F] tag.

## 6. What this implies for entry_contest

**The precedent is genuine and must be acknowledged.** Wealth-based
self-selection into a tournament is in print in 1981, and `PROOFS.tex`
currently cites nothing at all here.

**But it is weaker than a theorem, and it runs on a different channel.** Two
separations, both real:

1. **Channel.** Their sorting runs through **declining absolute risk
   aversion** — a restriction on `u'''`, since `A' < 0` requires
   `(u'')² < u'''u'`. Ours runs through `κ(w) = u(w) − u(w−c)` falling in `w`,
   which needs only `u'' < 0`. Concavity alone gives our sorting and cannot
   give theirs: quadratic utility is concave, has `u''' = 0`, and is
   **increasing** absolute risk aversion. The survey's "`u''` here, `u'''`
   there" is exactly right and is now checked (LR-1, LR-2, LR-3).

2. **Object of choice.** Theirs is a choice *between compensation schemes* —
   piece rate versus tournament, both of which pay. Ours is a choice *whether
   to pay a fixed entry cost at all*. Their sorting is about tolerance for the
   binomial spread of a lottery; ours is about affordability of a fee. These
   are different economics, not merely different derivatives, and the survey
   does not note the second difference.

**Status of their claim.** They write "it is not possible to make a general
argument based on an example". P5's threshold structure and P9's sign rule are
theorems. The acknowledgement should say that the 1981 paper *suggests* the
sorting on an example under DARA, and that the present model *derives* it under
concavity alone.
