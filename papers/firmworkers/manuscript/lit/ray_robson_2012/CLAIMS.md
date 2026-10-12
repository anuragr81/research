# Claims about Ray and Robson (2012)

**Source.** RR, *Econometrica* 80(4) pp.1505–1531, read in full [F], the
Supplemental Material not read. See `RECONSTRUCTION.md` for the copy, sha256
and cache path. Cited in `claims.yaml` as row L18 and in `refs.bib` as
`RayRobson2012`. Pages are the printed pages. Quotations are checked verbatim
on their page by `verify_rr.py`. Results are proved in
`lean/Lit/RayRobson2012.lean` (see `LEAN`), namespace `RayRobson2012`.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| RR-1 | 1505 | "an equilibrium must converge to a unique steady state in which there is recurrent endogenous risk" | The main result |
| RR-2 | 1505 | "A concern for status thus implies that persistent and inefficient risk-taking hinders the attainment of full equality." | Inefficient gambling sustains inequality |
| RR-3 | 1505 | "there is a competitive industry that can supply such gambles at zero profit" | Fair gambles, freely supplied |
| RR-4 | 1505 | "Such convergence implies there would be large gains in relative consumption from small increases in absolute consumption." | Bunching makes rank steep |
| RR-5 | 1506 | "Our main result delivers the Friedman–Savage findings with no assumption at all on the curvature of utility in status." | No curvature assumption in status |
| RR-6 | 1506 | "They sketched a model with two classes, where changes in wealth within either class led to decreasing marginal utility, but changes that promoted an individual from the lower class to the upper class led to increasing marginal utility." | Their reading of Friedman and Savage's class model; to be checked in Friedman and Savage itself |
| RR-7 | 1507 | "Another involves the use of income distributions to create endogenous “reference points” with high marginal gains and losses in the departure of wealth from the reference points." | Reference points from the distribution, credited to Genicot and Ray (2010) |
| RR-8 | 1508 | "Status at a particular consumption level is the fraction of the population who are consuming strictly less, plus a fraction of those who have exactly the same level of consumption." | Status rule (2) |
| RR-9 | 1508 | "Setting η = 1/2 is attractive since total status is always then 1/2, across all distributions, with or without atoms." | Total status ½ needs `η = ½` |
| RR-10 | 1509 | "risk-taking includes occupational choice or entrepreneurial ventures in addition to the assumption of short-term risk" | What counts as a gamble |
| RR-11 | 1511 | "If μ(c) were not concave, profitable deviations involving gambling would necessarily exist" | Condition R2 |
| RR-12 | 1511 | "These two restrictions imply linearity over the range of any randomization" | Condition R3 |
| RR-13 | 1513 | "This corollary describes how a common consumption budget must be spread out by gambling." | Corollary 1 |
| RR-14 | 1514 | "When they do, the preservation of concavity in μt requires that consumption be suitably spread out using endogenous risk-taking." | Why convergence forces gambling |
| RR-15 | 1516 | "In contrast, the convexity of f ensures that wealth and consumption distributions stay dispersed at every date: the lack of bunching induces a concave optimization problem." | Convex production, no gambling |
| RR-16 | 1517 | "In the steady state, there is aversion to downside risk; no individual would ever take bets that would lead them into the consumption region" | The Friedman–Savage property |
| RR-17 | 1518 | "Unless the Friedman–Savage utility function moves around to accommodate different wealths in exactly the right way, it is not possible in their approach to generate the same phenomenon at diverse aggregate wealth levels." | Scale-neutrality against Friedman–Savage |
| RR-18 | 1518 | "I consider the status consequences for me of my choice to gamble, but there must be consequences for others as well, and these I ignore." | The externality |
| RR-19 | 1519 | "That is, the mean of the distribution is less than the median, which conflicts with the stylized fact that the mean exceeds the median." | Mean below median under joint concavity |
| RR-20 | 1521 | "In our main result, obtained with conventional concavity restrictions on the utility and production functions, there must be persistent, endogenous, and inefficient risk-taking in equilibrium." | Conclusion |
| RR-21 | 1521 | "as did Friedman and Savage, but that is only one example" | State lotteries are one gamble among many |
| RR-22 | 1522 | "can, on average, have only a modest chance of promotion within the gang" | Promotion as a gamble |
| RR-23 | 1522 | "But the current explanation is attractive in that it does not rely on such misperception." | No misperceived odds |
| RR-24 | 1522 | "Suppose that a population of individuals can decide to either enter an activity with purely aggregate risk, or stay out." | Entry with status depending on entrants, left open (footnote 25) |

## Results in Lean (`lean/Lit/RayRobson2012.lean`)

| Result | Lean | What is proved |
|---|---|---|
| Status rule (2) and footnote 10 (p.1508) | `RayRobson2012.tieWeight_add_swap`, `RayRobson2012.total_status`, `RayRobson2012.ties_pos`, `RayRobson2012.total_status_half_iff` | in a finite population, total status is `n²/2 + (½ − η)T` with `T > 0` the ordered pairs of equal consumption, so it is `n²/2` for every distribution exactly when `η = ½` |
| Banning gambling (p.1519) | `RayRobson2012.banning_gambles_helps` | for `u` concave on consumption–status pairs, expected utility of a finite lottery is at most `u` at the mean consumption and mean status |
| Mean below median (p.1519) | `RayRobson2012.mean_below_median_step` | if `u(b, F(b))` equals expected utility (affine segment) and that is below `u(b, ½)` (Jensen), and `u` rises in status, then `F(b) < ½` |
| Proposition 5's proof (p.1530) | `RayRobson2012.efficiency_fails_at_bottom` | from `u_c + u_s F′ = α = u_c` and `u_s > 0`, `F′ = 0`, so the slope of `u_c(c, F(c))` at `a` is `u_cc < 0` |
| Proposition 4(ii)'s last step (p.1529) | `RayRobson2012.savings_rule` | for `0 < δ, x < 1`, `1/x = (δ/(1 − δ))·1/(1 − x)` iff `x = 1 − δ` |
| Footnote 18 example | `RayRobson2012.uniform_example_mean` | consumption uniform on `[0, 2b]` has mean `b` |

Controls: `RayRobson2012.control_total_status_needs_half`,
`RayRobson2012.control_efficiency_needs_strict_concavity`,
`RayRobson2012.control_savings_rule_needs_discounting`.

## Readings recorded

- RR-D1 (p.1530): the proof of Proposition 5 writes `α = u_c(c, 0)`;
  Corollary 1(iii) has `α = u_c(a, 0)`, which is what the argument uses at
  `c = a`. Read as `u_c(a, 0)`.
- RR-D2 (p.1531 references): Becker, Murphy and Werning (2005) is listed with
  pages 282–301; the article runs to p.310 (see `refs.bib`).
