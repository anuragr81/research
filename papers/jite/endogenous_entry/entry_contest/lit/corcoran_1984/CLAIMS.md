# Claims about Corcoran (1984)

**Source.** William J. Corcoran, "Long-Run Equilibrium and Total Expenditures
in Rent-Seeking", *Public Choice* 43(1), 89-94 (1984), uploaded by the author
on Drive as a JSTOR copy; read in full [F] and checked on page images [P].
Pages are the printed pages (the printed page is the PDF page plus 87). Cited
in `skeleton_who_pays.tex` (row L24) and in `refs.bib` as `Corcoran1984`. The text
layer garbles the equations and most of the second paragraph after (10) on
p.92, so the three p.92 rows marked P below were read from the page image,
which `verify_corcoran.py` pins by sha256 but cannot match against text. The
other rows are checked verbatim, on the page named, against the extracted
text. The results we use are proved in `lean/mathlib/Corcoran.lean` (see
`LEAN`).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CO-1 | 89 | "Posner (1975) and Becker (1968) assert that total expenditures in the rent-seeking process will just equal the value of the rents to be gained although neither is specific about how this result would occur." | The claim he sets out to derive |
| CO-2 | 89 | "Tullock (1980) shows that in a simple two-player lottery each player will maximize his expected value by investing one-quarter of the payoff at stake, not one-half, where total expenditures would equal the payoff." | The short-run benchmark, n = 2 |
| CO-3 | 90 | "it can be seen that Tullock's model, as portrayed, pertains to the short-run" | Tullock's count is fixed |
| CO-4 | 91 | "Total expenditures would be zero where the number of players is held to one." | Eq. (3) at n = 1 |
| CO-5 | 91 | "If the rate of return obtainable on other projects is less than that offered by the rent-seeking option then players will enter; if greater, players will exit." | Entry and exit by comparing returns: a threshold rule |
| CO-6 | 91 | "The crucial consideration for determining the long-run solution is the possibility of entry and exit in the rent-seeking process." | The entry margin |
| CO-7 | 92 | "entry is unbounded; this is akin to the results expected in industries with persisting decreasing returns to scale" | The case r <= 1 |
| CO-8 | 93 | "If the long-run equilibrium is attained total expenditures will equal the payoff regardless of the marginal cost." | Eq. (10) |
| CO-9 | 93 | "It is possible that total expenditures will increase as entry mounts, but this cannot be determined." | Left open for r <= 1 (see NOTES) |
| CO-10 | 93 | "When this occurs total expenditure in rent-seeking is found to equal the payoff as in the standard competitive case." | The summary of the long-run result |
| CO-11 | 94 | "in the long-run each opportunity for rent - a transfer of wealth - displaces an opportunity for the creation of value which will be equal to or greater than the rent payoff." | His welfare reading |
| CO-P1 | 92 | "which indicates that in long-run equilibrium the total expenditures by all competitors in the rent-seeking process will equal the payoff discounted by the opportunity cost rate of return." | Eq. (10), T = P e^(-rho t) |
| CO-P2 | 92 | "The resulting increase in the number of competitors exactly offsets the decline in individual expenditures so the total expenditure remains constant." | The offset behind invariance in r |
| CO-P3 | 92 | "Therefore, as equation (10) indicates, T is invariant with respect to r." | The long-run total does not depend on the marginal cost r |

Equations (6), (7) and (9), the long-run count n = r/(r-1), were read from the
page image of p.92 as well (the text layer shows the fraction without its
terms).

## Results in Lean (`lean/mathlib/Corcoran.lean`, namespace `Corcoran1984`)

| Result | Lean | What is proved |
|---|---|---|
| Short run, eqs. (1)-(3) | `hasDerivAt_V`, `foc_iff`, `symm_foc`, `winning_prob`, `Tot_eq`, `tullock_quarter`, `Tot_vs_prize`, `Tot_one` | the first-order condition at the symmetric point holds iff expenditure is (n-1) r R / n^2; each wins with probability 1/n; total expenditure is (n-1) r R / n; for n = 2, r = 1 each spends R/4 and the total is R/2; T is below, at or above R as (n-1) r is below, at or above n; T = 0 at n = 1 |
| Total expenditure against the count and the parameters | `Tot_lt`, `Tot_strictMono_n`, `Tot_tendsto`, `Tot_strictMono_r`, `Tot_strictMono_R`, `Tot_strictAnti_rho`, `Tot_strictAnti_t` | T < r R; T rises strictly with n and tends to r R; it rises with r and with R, and falls with the discount rate and the lag |
| Long run, eqs. (4)-(7) | `exp_piRate`, `piRate_cmp`, `piRate_strictAnti_r`, `nLong_gt_one`, `entry_zero_iff`, `entry_iff`, `unbounded_entry` | the rate of return exceeds, equals or falls short of rho as (n-1) r is below, at or above n; entry stops exactly at n = r/(r-1) > 1 for r > 1, players enter below and exit above it; for r <= 1 entry is unbounded |
| Payoff at once, eqs. (8)-(9) and footnote 4 | `profit0_eq`, `profit0_cmp`, `profit0_zero_iff`, `profit0_pos_of_le_one`, `profit0_hasDerivAt_r`, `profit0_deriv_neg`, `profit0_strictAnti_r` | profit is R (n + r - n r)/n^2, positive for every n when r <= 1, zero at r/(r-1) when r > 1, and falls in r with derivative R (1-n)/n^2 |
| Eq. (10) and the offset | `Tot_nLong`, `Tot_longrun_indep`, `Tot_longrun_pv`, `aStar_nLong`, `aStar_nLong_strictMono`, `nLong_strictAnti`, `aStar_strictMono_r` | at the long-run count T = R whatever r > 1, and with the discounted prize T = P e^(-rho t); individual expenditure R (r-1)/r rises with r while the count falls with r |
| Scope: when the point is an equilibrium (not in the paper) | `V_symm`, `V_zero`, `not_nash_of_loss`, `nash_of_le_one`, `nash_longrun` | for r <= 1 the first-order point is a best reply for every n > 1; at the long-run count it is a best reply for every r > 1; where profit at it is negative, spending nothing pays more |

Not formalised: whether the first-order point is a best reply for r > 1 and
n strictly below r/(r-1) (the paper does not discuss it); the time to reach
long-run equilibrium (Section 3, last paragraph); and the welfare reading in
Section 4.

## Reading recorded

| ID | Page | Text | Our reading |
|---|---|---|---|
| CO-D1 | 93 | "It is possible that total expenditures will increase as entry mounts, but this cannot be determined." | Within his own eq. (3) it can: T = (n-1) r R / n rises strictly with n and is below r R (`Tot_strictMono_n`, `Tot_lt`), so for r <= 1, where entry never stops, the total rises towards r R, which is at most R. What cannot be settled from the paper is whether n actually mounts, since that depends on the entry path he does not model |
| CO-D2 | 91 | "If the rate of return obtainable on other projects is less than that offered by the rent-seeking option then players will enter; if greater, players will exit." | At a count above r/(r-1) the first-order point has negative profit and is not an equilibrium: a player does better spending nothing (`not_nash_of_loss`). The paper reads this as exit. It is a statement about the profit at the point, and it is exact in his model |
