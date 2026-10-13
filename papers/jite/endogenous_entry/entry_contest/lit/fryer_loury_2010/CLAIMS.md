# Claims about Fryer and Loury (2010)

**Source.** Roland G. Fryer, Jr. and Glenn Loury, "Valuing Identity", NBER
Working Paper 16568 (December 2010), 28 printed pages and a figure, downloaded
from nber.org; read in full [F]. It was published as "Valuing Diversity",
*Journal of Political Economy* 121(4), 747-774 (2013), which is not checked.
Pages are the printed pages; the printed page is the PDF page minus 2. Cited
in `MANUSCRIPT.tex` (row L25) and in `refs.bib` as `FryerLoury2010`; the
published version, `FryerLoury2013`, is recorded in the last section. Each
quotation is checked verbatim, on the page named, against the extracted text.
The results we use are proved in `lean/mathlib/FryerLoury.lean` (see `LEAN`).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| FL-1 | 5 | "Imagine a world where “agents” are assigned to “slots” to produce “widgets.”" | The model |
| FL-2 | 5 | "Although our model is simple — a continuum of positions in a hierarchy" | A continuum, not a finite contest |
| FL-3 | 5 | "At the ex ante stage agents, distinguished by their identity and effort cost (i, c), choose whether or not to exert effort." | A binary investment before assignment |
| FL-4 | 5 | "We imagine these costs to encompass everything that hinders or helps individuals invest in skills: peer and neighborhood effects, innate ability, quality of schooling, and so on." | Costs stand for endowments generally, not wealth |
| FL-5 | 6 | "is taken to be a noisy function of prior effort at the individual level." | Those who do not invest still compete |
| FL-6 | 8 | "So, a disadvantaged group would be underrepresented (holding a fraction of the slots that is less than their fraction in the population), since their effort costs are higher (Assumption 1), and effort is productive (Assumption 2)." | The group gap under laissez-faire |
| FL-7 | 8 | "Lemma 1 shows that an efficient assignment of workers to slots is top-down" | Assignment by rank |
| FL-8 | 11 | "In competitive equilibrium, this private incentive equals the social return from greater effort, ensuring efficiency." | Theorem 2 |
| FL-9 | 11 | "That is, given any z, there is a greater fraction of group b workers assigned to slots at or below z." | The group gap formula |
| FL-10 | 20 | "Rather than a measure of status inequality, in this simple model Assumption 1 will ensure that there are a smaller fraction of Bs assigned to jobs relative to As." | The two-job version |
| FL-11 | 21 | "So, a blind ex post preference lowers productivity standards by moving from “select the very best” to “select those who are good enough.”" | Blind policy rations at random |
| FL-12 | 24 | "The efficient policy subsidizes human capital investment for all agents" | Proposition 1 |

## Results in Lean (`lean/mathlib/FryerLoury.lean`, namespace `FryerLoury2010`)

| Result | Lean | What is proved |
|---|---|---|
| Mixtures and the group gap (pp.7, 11) | `pop_mix`, `group_gap`, `group_gap_pos`, `group_gap_of_costs` | the population law is the mixture at the population effort rate; the gap between the groups' cumulative shares is `(π_a - π_b)(F_0 - F_1)`, positive when group a invests more and effort raises productivity; a cost order at the common return gives it |
| Lemma 1, the exchange step (p.8) | `assignment_monotone` | if swapping two agents' slots cannot raise surplus, the more productive holds the higher slot |
| Corollary 1 (p.9) | `cor1` | with the payment rule of eq. (5), the worker's payoff rises in productivity at the rate of the slot's effectiveness |
| The private return to effort (proof of Theorem 2, p.11) | `gain_by_parts` | `∫ W (f_1 - f_0) = ∫ W' ΔF` on a bounded support where `ΔF` vanishes at both ends |
| The two-job version (pp.20-22) | `alphaC_lt_one`, `Rcon_zero_iff`, `R_hasDerivAt`, `R_deriv_pos` | the rationing probability of eq. (14) is below one; eq. (17) is the target rate as rationing times the willing-buyer rate; footnote 12's derivative, and its sign when group b's hazard at the ceiling is above the population's |
| The investors' share of slots (not in the paper) | `investor_share_eq`, `investor_share_gt` | in the two-job version investors hold slots in proportion to their number when effort does not change productivity at the threshold, and more than that when it raises it |

Not formalised: that the likelihood-ratio orders of Assumptions 1 and 2 imply
the distribution orders (pp.6-7; hypotheses in the Lean file), Theorem 1's
payment schedule, the change of variables that completes Theorem 2, Lemma 2,
Theorems 3 and 4, and Proposition 1.

## Reading recorded

| ID | Page | Text | Our reading |
|---|---|---|---|
| FL-D1 | 16 | "Theorem 2 says that no explicit skills subsidy should be used to efficiently promote the access of a disadvantaged group to scarce positions when identity is fully contractible." | The result described is Theorem 3, the sighted case; Theorem 2 is laissez-faire |
| FL-D2 | 20 | "a universal skills subsidy is the optimal group-sighted affirmative action policy." | The sentence follows Theorem 4, which is about a blind environment; "group-blind" is meant |
| FL-D3 | 23 | "Thus, by Lemma 2, the efficient blind policy is defined by the optimization problem" | The blind problem is Lemma 3's; Lemma 2 is the sighted one |
| FL-D4 | 26 | "On the other hand, blindness lowers investment rate in both groups (I think!)" | A conjecture in a draft passage of the concluding remarks, not derived. It is the direction of M28: random assignment above a threshold lowers the return to investing |

## Comparison with the model rows

Fryer and Loury's structure is the closest found to ours. Effort is binary,
productivity after effort has law `F_1` and without it `F_0`, and agents who
do not invest still compete. In our model these are the entrant and outsider
score laws `F` and `G`. Their gap between groups with different cost laws is
the counterpart of the class gap of M31 and C10. Three things differ:

- their agents form a continuum and slots are priced in a market, so there is
  no count of payers, no fixed prize and no equilibrium set to pin down (M5,
  M6);
- there is no weight on talent, so nothing corresponds to M28;
- they study policy between identity groups and do not derive the investors'
  share as a function of how many invest, which is M31's `ξ(k)`.

## Published version: "Valuing Diversity", JPE 121(4), 747-774 (2013)

Read in full on 13 October 2026 [F] from the author's Drive upload
`Fryer&Loury_JPE2013.pdf`. The file is the journal's page proofs of 8 June 2013
(Achorn International, 28 proof pages and a page of queries to the authors),
cached at `~/.cache/entry_contest/fryer_loury_2013_jpe.pdf`. Pages below are
proof pages. The final journal pagination (747-774, the same length) and any
corrections made after the proofs are not checked. `verify_fryer_loury.py`
checks each quotation verbatim on its proof page.

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| FLP-1 | 1 | "When agent identity is contractible, efficient policy grants preferred access to slots but offers no direct assistance for acquiring skills." | The sighted result |
| FLP-2 | 3 | "With no policy intervention the disadvantaged will be less than proportionally represented among slot owners, compared to their presence in the population, since agents face a common slots price in market equilibrium." | The group gap |
| FLP-3 | 6 | "We take it that there is a fixed measure of slots" | Scarce positions |
| FLP-4 | 6 | "agents who do not purchase a slot are assumed to earn a payoff that is independent of productivity" | The outside option |
| FLP-5 | 6 | "The two stages of the model are linked because productivity is taken to be a noisy function of prior skill investment at the individual level" | Non-investors still compete |
| FLP-6 | 6 | "In most settings in which affirmative action policies are employed, such as college admissions, the scarce positions for which agents compete are allocated administratively and are not auctioned to the highest bidders." | Footnote 7: the market is an abstraction |
| FLP-7 | 8 | "Only the agents with cost c that is no greater than the anticipated net benefit from investment" | A cost threshold: investors are the lowest-cost agents |
| FLP-8 | 11 | "So, the equation can have at most one solution" | Proposition 1: a unique investment rate |
| FLP-9 | 11 | "That is, B’s invest in skills at the ex ante stage and acquire slots at the ex post stage at a lower rate than A’s in equilibrium." | The group slot rates |
| FLP-10 | 5 | "the efficient blind ex post stage policy is a mix of competitive bidding and random assignment of slots" | Theorem 1 |
| FLP-11 | 26 | "That is, under blind affirmative action the optimal ex ante policy entails a universal skill subsidy whenever B’s are relatively more prevalent on the development margin than they are on the slot assignment margin." | The ex ante condition |
| FLP-12 | 27 | "In our simple and transparent laboratory—two jobs, supply-demand allocation late, rational skill acquisition early, and a heterogeneous population" | The published model is the two-job one |

**Working paper against published version.**
- *Model.* The working paper assigns every agent to a continuum of slots
  `z ∈ [0,1]` with payoff `µβ(z)`. The published version has a fixed measure
  `θ < 1` of slots sold to the highest bidders, payoff `max{m − p, 0} − cs`, and
  zero for those without a slot: the working paper's two-job special case made
  the model.
- *Results.*
  - Proposition 1 adds that the laissez-faire investment rate is unique
    (FLP-8).
  - The slot rates `ξ_a > θ > ξ_b` are stated (FLP-9).
  - The sighted result is a fixed slot subsidy.
  - The blind ex post policy is Theorem 1, a lottery of free slots plus a
    price floor, under a concave `f_1/f_0` (FLP-10).
  - The ex ante condition is the working paper's.
- *Dropped.* The working paper's draft conjecture on blindness and investment
  (FL-D4) and its three wrong cross-references (FL-D1 to FL-D3) are gone.

| Result | Lean | What is proved |
|---|---|---|
| Group slot rates (proof p.11) | `jpe_slot_rates` | with a common price, `ξ_b < θ < ξ_a` when group a invests more and effort raises productivity at the price |
| Proposition 1, uniqueness (proof pp.10-11) | `jpe_unique_rate` | a strictly increasing cost quantile and a non-increasing return cross at most once |
| Footnote 10 (proof p.16) | `fn10_convex` | the second derivative of y is positive where `r'' < 0` |
| The example of proof pp.16-18 | `jpe_example_v`, `jpe_example_v2`, `jpe_example_v_signs` | the printed first and second derivatives of v, and their signs |

Not formalised for the published version: Theorem 1's optimality argument and
the derivatives on proof pp.24-25.

