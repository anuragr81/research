# Claims about Drugov and Ryvkin (2020)

**Source.** NES Working Paper 256 (February 2020), read in full [F]; the
*JET* version is not checked. Cited in `MANUSCRIPT.tex` (row L22, C9, C12) and
in `refs.bib` as `DrugovRyvkin2020a`. Pages are the working paper's.
Quotations are checked verbatim against the extracted text by
`verify_drugov_ryvkin.py`. The results we use are proved in
`lean/mathlib/DrugovRyvkinNoise.lean` (see `LEAN`).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| DR-1 | 1 | "For rank-order tournaments with arbitrary prizes, equilibrium effort decreases as noise becomes more dispersed, in the sense of the dispersive order." | The intensive margin: effort falls with dispersion |
| DR-2 | 10 | "the dispersive order is a necessary and sufficient condition for the ranking of equilibrium efforts in general" | Proposition 1 |
| DR-3 | 11 | "A straightforward example of stretching is a scaling transformation" | A scale change of the noise is a dispersive change |
| DR-4 | 20 | "Consider a two-stage game where at the first stage the N potential players simultaneously and independently decide whether to enter the tournament, or to stay out and receive an outside option" | They have an entry margin; non-entrants take an outside option and cannot win |
| DR-5 | 21 | "First, we show that nX ≤ nY." | With the less dispersed noise X, weakly fewer enter |
| DR-6 | 21 | "For example, Y can be dominated by X in the dispersive order." | The example for the hypothesis that effort is higher under X |
| DR-7 | 23 | "Our results extend to tournaments with stochastic and endogenous participation, with caveats regarding the effects of discreteness in the equilibrium number of entrants in the latter case." | Their own summary of Section 5.3 |
| DR-8 | 6 | "We focus on the effect of adding noise to a tournament with full participation." | Dropping out at low noise is left to Morgan, Tumlinson and Vardy |
| DR-9 | 5 | "show that an increase in r may lead to a reduction in the number of entrants" | Their report of Fu, Jiao and Lu (2015): more discriminatory power, fewer entrants |

## Results in Lean (`lean/mathlib/DrugovRyvkinNoise.lean`, namespace `DrugovRyvkin2020`)

| Result | Lean | What is proved |
|---|---|---|
| Proposition 1, sufficiency | `B_le`, `effort_le`, `prop1_sufficiency`, `prop1_eq6` | with `m_X ≤ m_Y` on [0,1], every weighted coefficient of (6) is lower, and with a strictly increasing marginal cost, effort is lower; `prop1_eq6` in their form (4) with (6) |
| Scaling (p.11) | `m_scale`, `quantile_scale`, `density_scale`, `scale_lowers_m`, `scale_lowers_effort` | for X = σY, m_X = m_Y/σ, and effort is lower for σ ≥ 1 |
| Section 5.3, Claim 1 | `entry_count_le`, `profit_le`, `entry_count_le_model`, `entry_count_le_of_decX` | if effort is higher under X at every count and payoffs fall in the count, X has weakly fewer entrants; it is enough that either payoff falls |
| Section 5.3, Claims 2 and 3 | `claim2_cost`, `claim2_effort`, `claim3_total_cost` | effort, and total cost, compare up to the discreteness correction; Claim 3 needs a non-negative outside option |
| The sign, from dispersion | `less_noise_fewer_entrants` | with Y more dispersed, (4) and (6) give higher effort under X at every count, hence weakly fewer entrants under X |
| Controls | `control_needs_decreasing`, `control_needs_dominance`, `entry_count_le_needs_decreasing`, `entry_count_le_needs_dominance`, `control_claim3_needs_nonneg_outside` | without falling payoffs, or without the effort dominance, Claim 1 fails; with a negative outside option, Claim 3 fails |

Not formalised: the necessity half of Proposition 1, and Section 4.

## Reading recorded

| ID | Page | Text | Our reading |
|---|---|---|---|
| DR-D1 | 21 | "Y can be dominated by X in the dispersive order" | The hypothesis is that effort is higher under X for every n. By Proposition 1 that holds when Y is the more dispersed noise, so the example must mean Y more dispersed than X. Claim 1 then reads: the less dispersed noise has weakly fewer entrants |

## The comparison with M28

On the intensive margin their sign is M28's: effort falls as noise becomes
more dispersed, and the count of payers falls as the base score is scaled up
against the bought component, which is a dispersive change of the common
component. On the entry margin their sign is the opposite: the less noisy
tournament has weakly fewer entrants, as in Morgan, Tumlinson and Vardy,
because effort rises as noise falls and lowers what entry pays. In the model
here paying for the bought component is the investment itself; there is no
effort after entry to escalate, and non-payers still compete, so it moves
like their effort, not like their entry.
