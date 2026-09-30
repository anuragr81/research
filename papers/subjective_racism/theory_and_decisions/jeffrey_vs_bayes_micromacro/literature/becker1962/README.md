# Becker, G. S. (1962), "Irrational Behavior and Economic Theory"

*Journal of Political Economy* 70(1), 1-13.

**Source. The copy is from the web, not from Drive.** Becker (1962) is not in
Drive. The copy used (`becker1962_web.pdf`, 14 pp.) was fetched from
`https://cooperative-individualism.org/becker-gary_irrational-behavior-and-economic-theory-1962-feb.pdf`.
It is a JSTOR download: a JSTOR cover page ("Stable URL:
https://www.jstor.org/stable/1827018") followed by the scanned journal
pp. 1-13, each stamped "downloaded … on Thu, 20 Jan 2022". Journal page numbers
are printed on the scan and are cited as `p. N`. Footnotes 10, 14 and 15 were
checked on the rendered pages 6 and 8, because the text layer garbles their
formulas. Read in full on 2026-09-30.

## Claims formalized

Section II ("Households", pp. 2-9) argues that the "fundamental theorem",
negatively inclined market demand, "largely results from the change in
opportunities alone and is largely independent of the decision rule" (p. 4).
It uses two models of irrational households:

* **Impulsive households (pp. 5-6).** "every opportunity has an equal chance of
  being selected".
  * On the budget line the average of many independent households "would
    almost certainly be at the middle of the opportunity set, which is also the
    (mathematically) expected consumption of a single household" (p. 5).
  * The midpoint is `(I/2P_x, I/2P_y)` (fn. 10).
  * A compensated rise in `P_x`, which rotates the line through `p` (a
    Laspeyres index held constant, p. 5), "always shifts the midpoint of the
    budget line upward and to the left" (p. 6).
  * "The expected demand curve of each household must also be negatively
    inclined, although many actual individual curves would not be" (p. 6).
  * "what is simply more probable for a particular household becomes a
    certainty for a large number of independent ones" (p. 6).
  * Erratic households have "unitary elastic market demand curves" (p. 8),
    with `X = k I/P_x` (fn. 15).
* **Inert households (pp. 6-8).** "wherever possible, households consume exactly
  what they did in the past".
  * After the compensated rise, households on `Ap` can remain. Those on `pB`
    "could not remain there … because pB would be outside the new opportunity
    set OCD" (p. 6).
  * "If the average household in pB had been consuming more than OD of X, the
    average amount of X consumed by all households would necessarily decline"
    (p. 6).
  * Otherwise, X "would probably decline even when not arithmetically
    necessary" (p. 7).
  * Fn. 14: with a 10 % rise, households uniformly distributed on the line, and
    forced adjusters moving to the new midpoint,
    `X₁ = ½(I/4P_x) + ½(I/2.2P_x) = (31/88) I/P_x` and
    `(X₁ − X₀)/X₀ = −.3`. That is "about 30 per cent, giving a high elasticity
    of −3", and "a smaller price change or a larger dispersion would yield a
    still higher elasticity" (p. 8).
* **Weighted average (p. 7).** "Since market demand curves at both these
  extremes would tend to be negatively inclined, the market curves of any
  weighted average would also tend to be."
* **Inefficient households (p. 9, fn. 16).** Uniform on the whole set `OAB`,
  the centre of gravity moves left and up.

**Becker's mechanism, in his words.** It has two ingredients:

1. **The budget constraint.** A compensated price change shifts every
   household's *opportunity set* away from the good that became dearer (pp. 4,
   6). "In both cases the word 'survive' simply refers to a resource constraint
   on behavior" (p. 10). "Even irrational decision units must accept reality
   and could not … maintain a choice that was no longer within their
   opportunity set" (p. 12). "Irrational units would often be 'forced' by a
   change in opportunities to respond rationally" (p. 12).
2. **Averaging over many independent units.** This turns a shift in each
   household's distribution of choices into a sure market response (p. 6).
   "A group of irrational units would … respond more smoothly and rationally
   than a single unit would" (p. 13).

Becker separates his result from a mere *arithmetic of aggregation*. He writes:
"This analytical statement must be distinguished from the frequently
encountered arithmetical statement that a market would behave rationally even
if only a few households did … The same arithmetic demonstrates that a market
would behave irrationally even if only a few households did … Our statement
goes beyond arithmetic and stems from an analysis of the responses of rational
and irrational households" (p. 7).

## Result

`lean/Becker.lean` is a symlink to `lean/Literature/Becker.lean`. It is
standalone on Mathlib, checks with `lake env lean`, and contains no `sorry`.
All 20 of its theorems use only the axioms `[propext, Classical.choice,
Quot.sound]` (checked in a scratch copy).

| Lean theorem | Content |
|---|---|
| `budgetLine_midpoint`, `budgetLine_param` | fn. 10: the midpoint of the budget segment is `(I/2p₁, I/2p₂)`. The line is an affine, constant-speed image of `x ∈ [0, I/p₁]`, so uniform on the line means uniform in `x` |
| `impulsive_mean` | `x` uniform on `[0, I/p₁]` has mean `I/(2p₁)` (via Mathlib's `pdf.IsUniform.integral_eq`) |
| `impulsive_integrable` | such an `x` is integrable |
| `impulsive_market_average` | **the market level**: for pairwise-independent, identically distributed impulsive households, the market average of `x` converges **almost surely** to `I/(2p₁)`. This is Becker's "a certainty for a large number of independent ones", via Mathlib's strong law `strong_law_ae_real` |
| `impulsive_demand_strictAnti` | **the downward slope**: `p₁ ↦ I/(2p₁)` is strictly decreasing |
| `impulsive_unit_elastic` | fn. 15: `p₁ · I/(2p₁) = I/2` |
| `rotation_through_point`, `compensated_midpoint_shift` | p. 6: a compensated rise in `p₁` (new line through the old midpoint) lowers `p₂`, and moves the midpoint left and up |
| `constraint_without_averaging` | **one household**: with the constraint but no averaging, `x` can rise after the price rise (p. 6, "many actual individual curves would not be") |
| `averaging_without_constraint` | **no constraint** (a remark; its content is definitional): if the choice law ignores the budget, the market mean is the same at every price, however many households are averaged |
| `inert_outside_iff` | p. 6: after the compensated rise, a point of the old line is unaffordable iff it has more `x` than the rotation point, so `pB` must move and `Ap` need not |
| `inert_decline_of_mean_gt` | **p. 6, "necessarily decline"**, for any finite population |
| `inert_decline_not_necessary` | explicit case (`I = p₁ = p₂ = 1`, rise to `(11/10, 9/10)`) in which a forced household *increases* its `x` (3/5 → 9/10) |
| `inertX₁`, `inert_fn14` | fn. 14: `X₁ = (31/88) I/P_x` |
| `inert_fn14_change`, `inert_fn14_elasticity` | fn. 14: change `−13/44 ≈ −0.295`; arc elasticity `−65/22 ≈ −2.95` (Becker: "−.3", "−3") |
| `inert_elasticity_formula`, `inert_elasticity_smaller_change` | for a rise by the factor `1+t`, the elasticity is `−(1+3t)/(4t(1+t))`, more negative for smaller `t` (p. 8) |
| `mixture_strictAnti` | p. 7: a weighted average of two strictly decreasing demands is strictly decreasing |

`sympy/check_becker.py` (19/19 PASS) checks the following:

* Fn. 10's midpoint and constant arc speed.
* The compensated prices `(1+t)p₁, (1−t)p₂`.
* The shift of the midpoint.
* Unit elasticity (fn. 15, both forms).
* The triangle's centre of gravity `(I/3p₁, I/3p₂)` and its compensated shift
  (p. 9, fn. 16).
* A seeded 200000-household Monte Carlo, within 0.5 % of `I/(2p₁)` at two
  prices.
* Fn. 14 from the uniform distribution: the ½-½ split, the means `x₀/2` and
  `3x₀/2`, `31/88`, `−13/44` and `−65/22`.
* The general-`t` elasticity and its divergence as `t → 0⁺`.
* The failure of p. 6's sufficient condition in fn. 14's own example (see
  below), and the threshold `t > 1/3`.
* Fn. 12 (the mean on `pB` rises with the dispersion).
* The mixture's derivative.

## What formalizing revealed

1. **Both ingredients are needed, and each alone fails.**
   * Constraint without averaging: a single impulsive household's realized
     demand can rise with its price (`constraint_without_averaging`). Becker
     says so himself (p. 6).
   * Averaging without the constraint: a price-independent choice law gives a
     flat market demand, however many households are averaged
     (`averaging_without_constraint`).

   The slope in `impulsive_demand_strictAnti` enters only through the support
   `[0, I/p₁]`, that is, through the budget constraint. The *certainty* in
   `impulsive_market_average` comes from averaging.
2. **In fn. 14, the decline is not "arithmetically necessary".** Under a
   uniform initial distribution, the mean on `pB` is `3x₀/2 = 0.75 I/P_x`. For
   a 10 % rise, `OD = I/1.1P_x ≈ 0.909 I/P_x`, which is larger. So p. 6's
   sufficient condition ("more than OD") fails in Becker's own example. It
   holds only for rises above `t = 1/3`. The −30 % in fn. 14 comes from the
   *assumed* rule for the forced households, who move to the new midpoint. That
   is the case Becker flags on p. 7 as "probably decline even when not
   arithmetically necessary". Becker does not claim otherwise, but fn. 14 is an
   illustration under a behavioural assumption, not a consequence of the
   constraint alone. `inert_decline_not_necessary` shows the assumption is
   doing work.
3. **"A smaller price change would yield a still higher elasticity" is exact**,
   and the elasticity diverges. The arc elasticity is
   `−(1+3t)/(4t(1+t)) → −∞` as `t → 0⁺`. This is because half the population
   jumps from mean `3x₀/2` to about `x₀` however small the rise is. The inert
   model's market demand has a kink at the initial price.
4. **The "weighted average" (p. 7) is a mixture of demands**, a population
   split between the two types. Fn. 13 describes a first-order Markov process,
   but the p. 7 argument applies only to the mixture reading. For that reading
   it is exact (`mixture_strictAnti`).

## Bearing on Paper B

The manuscript cites Becker once, at MS:282-283:

> The lineage distinction the concluding section trades on---protection
> intrinsic to the arithmetic of aggregation, rather than imposed by an
> enforced constraint on behaviour---goes back at least as far as
> \citet{Becker1962}.

There is a genuine parallel. Becker's thesis is that "households may be
irrational and yet markets quite rational" (p. 8). Paper B's conclusion says
that "a population's aggregate beliefs can look externally Bayesian … even
when no member's belief is" (MS:940-942). But the mechanisms are on opposite
sides of the distinction the sentence draws:

* In Becker, the market regularity is **produced by the constraint**. The
  budget constraint moves every unit's opportunity set, and averaging turns
  that into a sure market response. He calls it a "resource constraint on
  behavior" (p. 10) and says irrational units are "'forced' by a change in
  opportunities" (p. 12).
* Becker **explicitly distinguishes his result from the arithmetic of
  aggregation** ("Our statement goes beyond arithmetic", p. 7).
* In Paper B, the protection of the mean association and of the
  surplus-weighted loss comes from the averaging alone: separability and the
  stake-weighting of the flip band (MS:944-947). Nothing constrains the
  evaluators.

So Becker is an ancestor of the *observation* (aggregate rationality without
individual rationality). He is not an ancestor of the *mechanism* the
sentence attributes to him, and on the sentence's own dichotomy he belongs to
the constraint side. The sentence reads correctly only if "enforced
constraint" means a regulatory or institutional constraint, as opposed to a
budget constraint. Nothing in the manuscript says that, and Becker's paper
does not use the distinction.

## Audit findings (2026-09-30)

This section responds to `notes/citation_audit.md` items M13 and P5.

* **M13 (UNSUPPORTED cross-reference + CAVEAT). Confirmed, and strengthened.**
  * (a) *Cross-reference.* The Concluding remarks (MS:900-967) never mention
    Becker or any aggregation-versus-constraint distinction. This was
    re-checked, and the only "constraint" in the manuscript is at MS:283.
  * (b) *The gloss.* The audit said Becker's mechanism is averaging *plus* "a
    resource constraint on behavior" (p. 10), so the sentence "nearly inverts
    him". Confirmed on the full text, which adds that Becker himself sets his
    result apart from "arithmetical" aggregation statements (p. 7). The
    formalization shows that neither ingredient suffices alone (point 1
    above).
  * (c) *Source.* The audit's copy was "a JSTOR copy". The copy used here is
    also a JSTOR scan, obtained from the web; it is not in Drive.
* **P5 / PLAN:877 ("the manuscript cites that lineage (Phelps, Arrow,
  Coate-Loury, Becker)").** Confirmed as a conflation. Becker (1962) is about
  irrational households and firms and market demand. It mentions
  discrimination only in the title of a work cited in fn. 21, *The Economics of
  Discrimination* (1957). That book, not the 1962 paper, is the Becker of the
  statistical-discrimination literature.
* **Bibliographic entry** `Becker1962` (bibliography.bib:187): author, title,
  journal, volume 70, number 1, pages 1-13 and year 1962 all match the scan
  ("Volume LXX, February 1962, Number 1"; JSTOR cover "Vol. 70, No. 1 …
  pp. 1-13").

### Proposed corrected wording (text only, not applied)

**Option A (keep Becker, fix the gloss).** Replace MS:282-283 "The lineage
distinction the concluding section trades on---protection intrinsic to the
arithmetic of aggregation, rather than imposed by an enforced constraint on
behaviour---goes back at least as far as \citet{Becker1962}." with:

> That an aggregate can look rational while its members are not goes back at
> least to \citet{Becker1962}, who showed that households choosing at random on
> their budget sets still produce downward-sloping market demand. In Becker the
> regularity is forced by the budget constraint, which shifts every
> household's opportunities when prices change, and averaging over many
> households turns that shift into a sure market response. The protection
> studied here needs no constraint: it comes from the averaging alone.

If Option A is used, the concluding section should echo it in one clause, so
that the forward reference is true. For example, after "even when no member's
belief is": "and, unlike the market rationality of \citet{Becker1962}, without
any constraint on what the members may believe".

**Option B (drop).** Delete the sentence. Nothing else in the manuscript
depends on it.

**PLAN:877.** Replace "(Phelps, Arrow, Coate-Loury, Becker)" with "(Phelps,
Arrow, Coate-Loury)". If Becker's discrimination work is meant, cite Becker
(1957), *The Economics of Discrimination*, University of Chicago Press, and add
a separate bibliography entry. Do not cite Becker (1962) for it.

## Not formalized

* Section III (firms, pp. 9-12): impulsive firms uniform on the production
  opportunity set `QₑQᵤ`, cartelization shifting it left, and unit-elastic
  input demand (fn. 17). These are the same mechanism on a different
  opportunity set. They are not checked.
* The first-order Markov model (fn. 13), which Becker names but does not
  analyse.
* The claim that with a larger dispersion the inert elasticity is "still
  higher" (p. 8) is covered only through fn. 12 (the mean on `pB` rises with
  dispersion). The full comparative static depends on the adjustment rule.
* Revealed-preference consistency of the market (p. 7).
