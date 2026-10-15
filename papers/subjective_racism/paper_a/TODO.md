# TODO, Paper A skeleton

Conventions as in the reference bundle. `[ ]` open, `[~]` in progress, `[x]`
done, `[!]` needs a decision from the author. Each item says what counts as done.

Last updated 15 October 2026.

## PLAN

The author's instruction of 14 October 2026: build a fresh skeleton from what
has been established so far, and go claim by claim.

| Pass | Status |
|---|---|
| 1. Scaffold the skeleton, checker and controls | [x] 14 Oct. Empty tables, appendix stubs, `verify.sh`, MS-1 to MS-6 each with a passing and a failing control |
| 2. Lean project pinned, build roots, audit | [x] 14 Oct. `lean/mathlib/`, Lean `v4.24.0-rc1`, Mathlib `14871d5` |
| 3 and 4. Claims into Lean and model rows, one at a time | [x] 15 Oct. M1 to M17 and R1. Every predecessor claim is a row, a refuted conjecture, or recorded as not stated (Q12) |
| 5. Appendices B to G | [~] filled for M1 to M17, and Appendix D records D1 and D4 |
| 6. Retire `../theory_and_decisions/manuscript.tex` | [x] 15 Oct. Removed, `RETIREMENT.md` maps every part, suite 5 checks it against git commit `91a74cb1` |
| 7. Novelty reading | [ ] |
| 8. Conclusions, then introduction headlines | [x] 15 Oct. C1 to C11 on model rows, C12 to C16 pending, K1 to K6 with scope notes |
| 9. Literature table | [ ] |
| 10. Readability rounds | [ ] |
| 11. Referee comments | [ ] not applicable unless the paper has reports |

## Claim queue

Taken from the predecessor manuscript. Each becomes a model row only once it is
a Lean theorem with a control. Labels are those of the predecessor, retired at
pass 6 and readable from git at commit `91a74cb1` (`RETIREMENT.md`).

- [x] **Q1, M1. Group sufficiency, both forms.** `ass:sufficiency` states the
  likelihood form and the posterior form as equivalent. Proved over a finite
  class space, with full support needed for one direction
  (`Sufficiency.lean`). This is the substantive step of `prop:coincidence`.
- [x] **Q2, M2 and M3. Bayes and the mixture formula give the same
  posteriors.** `eq:jeffrey`. Under sufficiency the Bayesian posterior on
  class is the within-group distributions mixed by the Bayesian group
  posterior (M2). Every revision of group weights from a full-support prior is
  a Bayesian update for a likelihood over groups (M3). Controls drop
  sufficiency, normalisation and full support (`ClassPosterior.lean`).
- [ ] **Q2b. Salience as a Bayesian group posterior.** The predecessor reads
  the salience `s_g` as Jeffrey's directly given group weight. With Jeffrey
  dropped (D4), `s_g` needs a Bayesian reading, which M3 supplies. Write the
  map from `s_g` to `q'` once the engagement decision enters.
- [x] **Q3, M4. The composed map's boundary values.** `H_F(0) = mu` for every
  F, and `H_F(1) = 1` exactly when `F(1) = 0` (given `s delta != 0`).
- [x] **Q4, M5 and M6. Class closure, canonical F.** `prop:class-closure`.
  M5: two fixed points inside (0, 1) exactly when `b - gamma > 0` and
  `(b - gamma)^2 > 4 mu b`, with `b = s_g delta`, and then the fixed points in
  [0, 1] are those two and 1. M6: when also `b < 1 - mu + gamma` and
  `mu + gamma < 1`, the map is increasing, every standing below the middle
  fixed point converges to the low one and every standing above it converges
  to 1. Found on the way. The predecessor's "sufficiently large delta" is
  bounded above, since for large `s_g delta` the map leaves [0, 1]
  (`control_leaves_unit`). The compiler showed `basins` needs neither
  `mu > 0` nor `gamma >= 0`, and both were dropped.
- [x] **Q4c. A true control for M6.** `control_basin_fails`, 15 Oct. At
  `(mu, gamma, b) = (3/5, 0, 5/2)` the trap condition holds, `c_L = 2/5`,
  `c_M = 3/5`, the bound `b < 1 - mu + gamma` fails, and `H 0 = mu = c_M`, so
  the orbit of 0 sits on the tipping point and does not converge to `c_L`.
- [x] **Q4d, M12. Class closure under `ass:Freg`.** `GeneralF.lean`.
  Sufficient conditions only. With `F 1 = 0` and `F' 1 = 0` the map rises
  above the diagonal near 1 (`rise_near_one`), and a dip at any `c` then gives
  a fixed point in `(0, c)` and one in `(c, d)` (`two_fixed_of_dip`). The
  canonical trap condition supplies the dip (`dip_of_trap`). Decreasing and
  convex `F` were not needed for this. An exact region and the count of fixed
  points for the general `F` are not stated.
- [x] **Q4e, M9 and M10. The bifurcation.** `prop:bifurcation` and
  `cor:salience-threshold`, canonical F, `Bifurcation.lean`. M9: the trap
  condition holds exactly when `gamma < gamma* = b - 2 sqrt(mu b)`. Above
  `gamma*` integration is the only fixed point in [0, 1]. At `gamma*` the
  low and middle fixed points merge into the double root `sqrt(mu)/sqrt(b)`.
  `gamma* > 0` exactly when `b > 4 mu`. M10: with `gamma >= 0`, the trap
  condition holds exactly when `s_g delta > b* = (sqrt mu + sqrt(mu + gamma))^2`,
  and `gamma*` rises with `b`, so with salience, from `b = mu` on. Found on
  the way. (1) The predecessor's "there exists gamma* > 0" needs
  `s_g delta > 4 mu` (`control_threshold_zero`). (2) Its salience corollary
  claims `d gamma*/d s_g > 0` without restriction, which fails for
  `s_g delta < mu` (`control_not_mono_below_mu`), a region where the threshold
  is negative and so harmless. (3) Its figure's `delta* ~ 0.29` is confirmed,
  `0.29 < b* < 0.30` at `mu = gamma = 0.05`, `s_g = 1` (`bStar_at_figure`).
  Not in Lean: the slope `H' = 1` at the merged root (Q4f).
- [x] **Q4f. The slope at the merged root.** `slope_one_at_threshold`,
  15 Oct. `H' = 1` at `sqrt(mu)/sqrt(b)` when `gamma = gamma*`, with
  `H_hasDerivAt` giving the derivative everywhere. Control at
  `(1/20, 1/10, 4/5)`, slope `17/20` at `1/4`.
- [x] **Q5, M7 and M8. The frozen-belief map has one equilibrium.** With
  `phi` held at `s_g F(beta_0)`, so `k = s_g delta F(beta_0)`, the fixed points
  are the roots of `P c = gamma c^2 + (mu + k - gamma) c - mu`. M7: for `k > 0`
  exactly one lies in [0, 1], it is inside (0, 1), and it falls as `k` rises,
  so it rises with the frozen belief `beta_0`. Uniqueness holds on `[0, inf)`
  for `gamma >= 0`, through `y P x - x P y = (x - y)(gamma x y + mu)`, which
  avoids the case split at `gamma = 0` that the root-product argument needs.
  M8: when `k < 1 - mu - gamma` the map is increasing and every standing in
  [0, 1] converges to the fixed point. Both rows hold for every F. Unlike M6,
  M8 has a true control: at `(1/2, 0, 3/2)` the orbit of 0 cycles and
  converges to nothing (`control_cycle`). Found on the way. The frozen map is
  not free of persistence. Its long-run standing is set by `beta_0` and does
  not depend on the starting standing, so raising a group's standing without
  moving the belief has no lasting effect. What it lacks is the tipping point.
- [x] **Q6. Moot, 14 Oct.** The dynamic half of `prop:coincidence` compared
  Bayes and Jeffrey across generations. With D1 (re-learned prior) and D4
  (Bayes only) there is no second rule to compare.
- [x] **Q7, M11. The outsider trap.** `prop:enemy`, `Outsider.lean`. Below
  the threshold, an outsider starting below its `c_M` converges to its own
  `c_L` for every host, and the host's orbit is its own. Above the threshold
  every standing in [0, 1] converges to 1. Both parts need the bounds of M6,
  which the predecessor omits. The independence of the two groups is
  structural, since neither group's standing enters the other's map
  (Appendix D, "Two groups"). Part (ii) has a true control on the threshold.
  Its bounds have the same gap as M6 (Q4c).
- [x] **Q8, M13. `prop:loury-nesting`.** `Intensity.lean`, 15 Oct. The
  predecessor's `rho` multiplies `s_g delta` and appears nowhere else, so
  `rho* = b*/(s_g delta)` and the proposition is M10 rescaled. Interior
  threshold exactly when `s_g delta > b*`, with a control where it is not
  (`rho* > 1` at the bifurcation figure's `s_g delta = 1/5`). Appendix D
  records that the rows cannot read `rho` as rigidity.
- [x] **Q9, M14. `prop:passing`.** `Passing.lean`, 15 Oct. Finite
  population. Leavers above every stayer lower the visible mean, strictly
  when one is strictly above (`mean_sdiff_lt`). Threshold structure from a
  monotone benefit and an antitone cost. Rate rises under antitone `F`.
  Control without the ordering. The passing decision itself is not a row.
- [x] **Q10, M15 and R1. The platform channel.** `Platform.lean`, 15 Oct.
  `prop:exclusion` and `prop:spillover` as algebra on the predecessor's
  first-order condition and spillover formula, taken as definitions (M15).
  `prop:tokenism` is refuted as stated, Appendix F R1, since `c_E` can lie
  on either side of `c_M` (`tokenism_not_implied`). `prop:shadow` is not a
  row, since `S*` and `S_rand` are undefined (Appendix D, "The platform
  channel").
- [x] **Q11, M16. `cor:Tstar`, critical slowing.** `Slowing.lean`, 15 Oct.
  From the lower edge of the window of half-width `sqrt(eps)/sqrt(b)` about
  the merged root, crossing it takes at least `1/(sqrt(b) sqrt(eps))`
  generations, `eps = gamma - gamma*`. Exponent `-1/2` as the predecessor
  claims, as a lower bound. At `(1/20, 3/10, 1/100)` at least 18
  generations, against the predecessor's "approximately 65". The constant
  `C = pi/sqrt|ab|` and an upper bound are not stated.
- [ ] **Q12. What the appendix pass and the retirement left.** New rows, not
  predecessor claims. A row deriving `F` from the engagement decision
  (`ass:preferences`, `ass:beliefs`, the threshold `d_bar`), which would also
  carry `rem:graded` and `rem:salience`. The signal precision `tau` of
  `rem:resolution`. A platform objective derived rather than assumed. An
  upper bound on the escape time. Cohorts, if the Discussion's claim that
  the trap regenerates after forced integration is to be stated, since M6
  says a single group pushed above `c_M` integrates. The welfare addendum.
- [x] **Q13, M17. The predecessor's figures and worked example.**
  `Figures.lean`, 15 Oct. Found in the retirement inventory: the parameters
  earlier called "the figure's", `(1/20, 1/10, 1/2)`, are not Figure 1's,
  whose script uses `gamma = 0.05` and `0.30`. The theorems were renamed
  `trap_at_example` and `basins_hypotheses_at_example`, and M17 checks the
  real panels.
- [x] **Measurement map.** `MEASUREMENT_MAP.tex`, 15 Oct 2026. Inputs,
  internal objects and outputs, each with one name, a type, what it
  represents and how it is obtained, and four recovery formulas proved in
  `MeasurementMap.lean`. Suite 4 checks it. Objects no row carries yet, the
  engagement threshold and the within-group dispersion, are named there so
  the names are fixed. Format follows
  `papers/jite/endogenous_entry/entry_contest/MEASUREMENT_MAP.tex`.

## Decisions for the author

- [x] **D1. Is the group prior re-learned each generation? Decided 14 Oct
  2026 by the author: re-learned.** Recorded in Appendix D, "The group prior
  across generations". Two readings exist
  and they give opposite results.
  - Re-learned. The discrimination rate is `phi_t = s_g F(c_bar_t)` with
    `c_bar_t` the group's current standing. This is what the predecessor's
    own equations say (`eq:prior`, the loop of `fig:loop`, `eq:dynamic` and
    `eq:composed`). The map is cubic, and the trap of `prop:class-closure`
    exists under both updating rules.
  - Frozen. `phi = s_g F(beta_0)` for good. This is the reading of
    `verify_bayes_jeffrey.py` and of `notes/from_archive/PAPER_A_DIRECTION.md`.
    The map is quadratic with one equilibrium (Q5), so there is no trap.
  - Recommendation. Re-learned, because it is the model the predecessor
    already writes down and the only one that carries the trap and the
    bifurcation. The frozen map then enters as Q5, a row of its own that shows
    what the trap needs. Under this reading the trap rests on the group prior
    following the group's standing, and on neither updating rule.
- [x] **D2. The research question. Decided 15 Oct 2026 by the author.**
  "When does reading class from group markers trap a group at a low
  standing, and how much mobility does it take to get out?" Every part is a
  row (M5, M6, M9, M10, M16).
- [x] **D3. The title. Decided 15 Oct 2026 by the author.** "Closure Traps
  from Social Inference". "Stereotype Rigidity" was dropped because the
  rigid, frozen belief has no trap (M7, M8) and the predecessor's rigidity
  parameter is only a scale on the exclusion pressure (M13).
- [x] **D4. Drop Jeffrey's rule. Decided 14 Oct 2026 by the author**, on the
  condition that the two rules are shown not to differ here. M2 and M3 show
  it for one reading of a marker under group sufficiency. The skeleton names
  Jeffrey only in Appendix D, "The updating rule", which gives the reason.
  Not covered is a sequence of several soft readings, where order could
  matter. The model has none.

## Novelty ledger

Nothing claimed as new yet. Every source the predecessor cites is unread for
this skeleton (Jeffrey 1965, Diaconis and Zabell 1982, Coate and Loury 1993,
Phelps 1972, Arrow 1973, among others), so every claim about them is
unverified.
