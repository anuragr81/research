# Claims about Lazear and Rosen (1981)

**Source.** LR, *JPE* 89(5), pp.841–864, read in full [F]. See
`RECONSTRUCTION.md`. Cited in `claims.yaml` as row L27 and in `refs.bib` as
`LazearRosen1981`. Pages are the journal's. The text layer drops spaces between
words, so `verify_lr.py` finds each quotation on its page with all spaces
removed (after the usual normalisation); table rows are quoted as printed and
normalisation drops their signs and points. Results are proved in
`lean/Lit/LazearRosen1981.lean` (see `LEAN`), namespace `LazearRosen1981`.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| LR-1 | 841 | "Under some circumstances, risk-averse workers actually prefer to be paid on the basis of rank." | Abstract |
| LR-2 | 841 | "if workers are heterogeneous in ability, low-quality workers attempt to contaminate high-quality firms, resulting in adverse selection" | Abstract, §IV |
| LR-3 | 841 | "if ability is known in advance, a competitive handicapping structure exists which allows all workers to compete efficiently in the same organization" | Abstract, §IV |
| LR-4 | 844 | "It will be seen that a worker's incentives to invest increase with the spread between winning and losing prizes" | Spread drives investment |
| LR-5 | 846 | "Levels of the prizes only influence the decision to enter the game, which requires nonnegativity of expected wealth." | Levels drive entry |
| LR-6 | 846 | "the players receive their expected product combined with a fair winner-take-all gamble over the total entrance fees or bonds" | The contest as a fair gamble |
| LR-7 | 846 | "Therefore, competitive tournaments, like piece rates, are efficient and both result in exactly the same allocation of resources." | Risk-neutral equivalence |
| LR-8 | 849 | "Little else but the well-known paper by Friedman (1953) based on Friedman-Savage preferences for lotteries exists in economics." | Pointer to Friedman (1953) |
| LR-9 | 849 | "There players (and horses, for that matter) who are known to be of higher quality ex ante may play in games with higher stakes." | Footnote 5: sorting into contests by type |
| LR-10 | 850 | "A tournament is a highly nonlinear transformation: It converts the continuous distribution of productivity into a discrete, binomial distribution of income." | Binomial income |
| LR-11 | 850 | "We have been unable to completely characterize the conditions under which piece rates dominate rank-order tournaments and vice versa" | No general result |
| LR-12 | 852 | "Using familiar Taylor series approximations to the utility function and a normal density for" | The approximations (16), (17) |
| LR-13 | 852 | "All these approximations use first-order expansions for terms in U'( ) and second-order expansions for terms in U( )." | Footnote 8: their order |
| LR-14 | 853 | "Equations (16) and (24) indicate that investment and expected income" | Comparison at given `s` |
| LR-15 | 853 | "are lower for the contest than for the piece rate at given values of s" | Comparison at given `s` |
| LR-16 | 853 | "the variance of income in the tournament is smaller than for the piece rate" | Variance comparison (threshold read from the page image) |
| LR-17 | 854 | "It can be shown that the expected indirect utility of the optimal piece rate exceeds that of the optimal tournament for all values of" | CARA case |
| LR-18 | 854 | "However, when there is declining absolute risk aversion, we have examples where the contest dominates the piece rate." | DARA examples |
| LR-19 | 854 | "which exhibits constant relative but declining absolute risk aversion" | `U = a y^a` |
| LR-20 | 854 | "1 .9950 .9846 5.012100 5.012295" | Table 1, `y_0 = 100`, `σ² = 1` |
| LR-21 | 854 | "3 .9852 .9552 5.011940 5.011925" | Table 1, `y_0 = 100`, `σ² = 3` |
| LR-22 | 854 | ".2 .9960 .9878 2.524616 2.524575" | Table 1, `y_0 = 25`, `σ² = .2` |
| LR-23 | 854 | "1 .9807 .9419 2.524237 2.523437" | Table 1, `y_0 = 25`, `σ² = 1` |
| LR-24 | 855 | "While it is not possible to make a general argument based on an example, table 1 suggests that persons with more endowed income and smaller absolute risk aversion are more likely to prefer contests" | The precedent, and its limit |
| LR-25 | 855 | "Consider a situation in which all persons have the same utility function" | Common utility, different endowments |
| LR-26 | 855 | "Individuals will self-select the payment scheme in accordance with their wealth." | Wealth sorting |
| LR-27 | 855 | "then the rich prefer contests (5.012295 > 5.012100) and the poor prefer piece rates (2.524237 > 2.523437)" | The example at `σ² = 1` |
| LR-28 | 855 | "He showed that the Friedman-Savage utility function leads to a two-class distribution." | Friedman (1953) |
| LR-29 | 855 | "persons in the risk-preferring region buy lottery tickets in very risky occupations in which few win very large prizes" | Friedman (1953) |
| LR-30 | 856 | "Therefore, Friedman's assignment of people to jobs really follows endowed wealth" | Friedman (1953) |
| LR-31 | 856 | "workers in this model are risk averse for all values of incomes, but even so gambles can be the optimal policy" | Gambles under concave utility |
| LR-32 | 857 | "Risk-averse workers increase utility by competing against an opponent and eliminating this kind of firm effect." | Common shocks |
| LR-33 | 858 | "Instead, all workers prefer to work in firms with the best workers (the major leagues)." | Adverse selection |
| LR-34 | 858 | "Therefore, tournament structures naturally require credentials and other nonprice signals to differentiate people and assign them to the appropriate contest." | Nonprice sorting |
| LR-35 | 859 | "Therefore, independent of cost curves, it is always better to play in the a league than the b league: Workers will not self-select." | No self-sorting |
| LR-36 | 860 | "Therefore, except in that very special case, mixed contests yield inefficient investment" | Mixed contests |
| LR-37 | 860 | "The obvious practical resolution of these difficulties is the use of nonprice rationing and certification to sort people into the appropriate leagues based on past performance." | Rationing by certification |
| LR-38 | 861 | "Competitive handicaps yield efficient mixed contests." | Handicaps |
| LR-39 | 862 | "The gain of playing mixed matches to a is completely offset by the loss to b and vice versa." | Zero sum |
| LR-40 | 862 | "a's prefer to play in mixed contests rather than with their own type, while b's prefer to play with b's only" | Below the competitive handicap |
| LR-41 | 862 | "The competitive handicap does not result in a fair game" | `h* < Δμ` |
| LR-42 | 863 | "This yields the surprising conclusion that reverse discrimination, where the less able are given a head start or rewarded more lucratively if they happen to accomplish the unlikely and win the contest, can be consistent with efficient incentive mechanisms" | Reverse discrimination |
| LR-43 | 863 | "In particular, low-quality workers attempt to contaminate firms composed of high-quality workers, even if there are no complementarities in production." | Conclusion |

## Results in Lean (`lean/Lit/LazearRosen1981.lean`)

| Result | Lean | What is proved |
|---|---|---|
| Prizes (7), (10) (p.846) | `LazearRosen1981.prize_structure`, `LazearRosen1981.bond_gamble` | from `W_1 + W_2 = 2Vμ` and `(W_1 − W_2) g(0) = V`, `W_{1,2} = Vμ ± V/2g(0)`; the expected prize is `Vμ` |
| Equivalence (6), (9) | `LazearRosen1981.tournament_matches_piece_rate` | `C'(μ) = ΔW g(0)` with `ΔW = V/g(0)` gives `C'(μ) = V`, the piece-rate condition |
| Normal spread (p.847) | `LazearRosen1981.normal_spread`, `LazearRosen1981.spread_increasing` | with `g(0) = 1/(2σ√π)` the spread is `2Vσ√π`, increasing in `σ` |
| Variances (17), (25) | `LazearRosen1981.piece_rate_variance`, `LazearRosen1981.contest_variance_from_spread` | `r²σ²` with `r = V/(1 + x)`; `(ΔW)²/4` with `ΔW = (V/(1 + πx))/g(0)` equals `πσ²V²/(1 + πx)²` |
| Comparisons (p.853) | `LazearRosen1981.contest_investment_lower`, `LazearRosen1981.contest_variance_smaller_iff` | `1/(1 + πx) < 1/(1 + x)`; the contest variance is smaller iff `x > 1/√π` |
| Second-order welfare | `LazearRosen1981.piece_rate_ce`, `LazearRosen1981.contest_ce`, `LazearRosen1981.approximations_favour_piece_rate` | with `V = 1`, `C = μ²/2`, mean less `(s/2)` variance is `μ/2` and `μ*/2`, so the piece rate is ahead at every `s, σ² > 0` |
| `U = a y^a` (p.854) | `LazearRosen1981.utility_derivatives`, `LazearRosen1981.dara_example`, `LazearRosen1981.table_s_values` | first and second derivatives; `−U''/U' = (1 − a)/y`; `s = .005` at 100 and `.020` at 25 with `a = .5` |
| Table 1 | `LazearRosen1981.rich_rows`, `LazearRosen1981.poor_rows`, `LazearRosen1981.unit_variance_sorting` | contest ahead for `y_0 = 100` at `σ² ≤ 1` and behind at `σ² ≥ 3`; for `y_0 = 25` ahead only at `σ² = .1`; at `σ² = 1` the rich prefer the contest, the poor the piece rate, and `μ* = .9846 > μ = .9807` |
| Common error (p.856) | `LazearRosen1981.common_error_tips_balance` | `2σ² < σ² + σ_p²` iff `σ² < σ_p²` |
| Adverse selection (26), (27) | `LazearRosen1981.revenue_at_own_investment`, `LazearRosen1981.league_displacement`, `LazearRosen1981.players_do_not_self_sort` | `R_i(μ_i*) = Vμ_i*`; `R_b(μ) = R_a(μ + Δμ) − VΔμ`; under the slope bound `(V/g(0))(G(v) − G(u)) ≤ V(v − u)`, `R_b ≤ R_a` |
| Mixed contests (p.860) | `LazearRosen1981.mixed_efficient_iff_half` | with `g(0) ≠ g(Δμ)`, efficiency holds iff `α = 1/2` |
| Handicaps (29)–(32) | `LazearRosen1981.handicap_gain`, `LazearRosen1981.handicap_zero_sum`, `LazearRosen1981.competitive_handicap`, `LazearRosen1981.competitive_handicap_not_fair`, `LazearRosen1981.mixed_preference_below_handicap`, `LazearRosen1981.reverse_discrimination_clears` | with `P = 1/2 + γ(Δμ − h)` and `ΔW = V/γ`, `y_a = V(Δμ/2 − h)`; `y_a + y_b = 0`; `h* = Δμ/2 < Δμ`; below `h*` a's gain and b's lose; at `h = 0` shifting both prizes by `∓VΔμ/2` clears the market and keeps the spread |

Controls: `LazearRosen1981.control_self_sort_needs_slope_bound`,
`LazearRosen1981.control_handicap_needs_zero_profit`.

## Readings recorded

- LR-D1 (p.853, (17), (25)): the displays are garbled in the text layer. We read
  (17) as `V²σ²/(1 + sC''σ²)²` (footnote 8's `r` times `σ²`) and (25) as
  `πV²σ²/(1 + πsC''σ²)²`, the binomial variance `(ΔW)²/4` of p.855 with the
  spread of footnote 9; `contest_variance_from_spread` shows the two agree.
- LR-D2 (p.853): the variance threshold "1/sC''√π" is read from the page image;
  `contest_variance_smaller_iff` gives `x > 1/√π`, i.e. `σ² > 1/(sC''√π)`.
- LR-D3 (pp.853–855): under the paper's own second-order approximations, with
  the table's `V = 1` and `C = μ²/2` and one value of `s` for both schemes (each
  panel of Table 1 is labelled by a single `s(y_0)`), the piece rate's mean
  income less half `s` times its variance exceeds the contest's at every `s`
  and `σ²` (`approximations_favour_piece_rate`). Table 1's contest-preferred
  rows therefore do not follow from (16), (17), (24), (25) at the stated order
  with a common `s`; the paper does not say how Table 1 was computed. We did not
  recompute it.
- LR-D4 (p.862, (32)): the printed linearisation of the win probability is
  garbled; we take `P = 1/2 + γ(Δμ − h)` with `γ` the same density value that
  sets the spread in (29), which gives (32) with no density factor.
- LR-D5 (p.859): the comparison of the revenue functions uses `R_i' ≤ V`, which
  needs `g(·) ≤ g(0)`; we state it as the slope bound in
  `players_do_not_self_sort`, and the control shows the conclusion fails
  without it.
