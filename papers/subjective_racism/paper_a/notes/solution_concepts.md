# Solution concepts in the papers read, 17 October 2026

A planning note, not part of the paper. The author asked for a concrete
contrast of how the papers read are solved, since a paper "could be decision
theory and still use nash equilibrium and type dependent signalling", and
said the contrast drives the literature survey. Each paper is placed on
seven separate axes, because one model can hold a decision problem, an
equilibrium and a signal at once. Every entry rests on a quotation ID of the
paper's `CLAIMS.md`, a verbatim run of its cached text, or a Lean theorem
that exists. Ten of the solution-concept quotations were checked again
against the cached texts on 17 October 2026.

## 1. The axes

| Axis | Question |
|---|---|
| Decision | Who chooses what, against what taken as given. Is some agent's problem a pure decision problem? |
| Equilibrium | What pins the things each decision takes as given, in the paper's own word |
| Hidden | What is hidden from whom, and is it read by Bayes' rule |
| Signal | Is the signal exogenous, or an action whose cost or benefit varies with the hidden type (single crossing) |
| Beliefs | Exogenous, equal to the actual by assumption, an equilibrium fixed point, or learned |
| Consent | Unilateral, mutual, or none |
| Own type | Does the deciding agent's own type enter the decision |

The axes show how the three labels fit together. A decision problem is
what each agent solves, given something. An equilibrium is the condition
that the given thing equals what the other agents' choices produce. A
signal is type dependent when the action that feeds another agent's belief
is chosen with a cost or benefit that varies with the hidden type. Corneo
and Jeanne have all three. The consumer buys iff
`σ > u(y_r) − u(y_r − p)` given the status value σ (eq. 4, a decision).
σ must equal what spectators infer by Bayes' rule from who buys (eq. 8, an
equilibrium). The utility cost of the price falls with income, so buyers
are an upper set (`concave_increment`, `buyers_upper`, a type-dependent
signal).

## 2. The papers on the axes

| Paper | Decision | Equilibrium, the paper's word | Hidden, Bayes | Signal | Beliefs | Consent | Own type |
|---|---|---|---|---|---|---|---|
| Phelps (1972) | Employer predicts from score and colour | None. "least-squares predictor" | Qualification. Regression, Bayes not named | Exogenous (PH-Q1) | Exogenous (PH-Q2, PH-Q3) | Unilateral | No |
| Aigner and Cain (1977) | Employer, mean or mean less a variance penalty (eqs. 2, 8) | None solved. Competition invoked for correct beliefs | Skill. Conditional expectation | Exogenous. Spence only cited | Equal to actual by assumption (AC-Q3) | Unilateral | No |
| Fryer and Jackson (2008) | Decision maker chooses categories (eqs. 4, 5) | None. "not a multiple equilibrium phenomenon" (FJ-Q4) | Nothing (FJ-Q5) | None | Learned from own encounters | None | No |
| Bohren, Imas and Rosenberg (2019) | Each evaluator, given the history (eq. 2) | None in the main model | Quality. Bayes, possibly misspecified | Exogenous | Exogenous, learned (Prop. 2) | Unilateral | Taste and belief type, not own group (BIR-Q16) |
| Bohren, Hull and Imas (2024) | Manager's action rule, reduced form | None. Nash, self-confirming, Berk–Nash all admitted (BHI-Q17) | Productivity | Exogenous | Folded into the rule | None | No |
| Laouénan and Rathelot (2022) | Host prices against demand (eq. 1) | None | Quality. Normal posterior (LR-Q8) | Exogenous, reviews | Exogenous prior, learned (LR-Q3, LR-Q6) | Mutual on the platform, unilateral in the model | Host yes, guest no |
| Kim and Loury (2009, 2014) | Employer given the group's share, worker invests given the path | "equilibrium path", steady state, "self-confirming equilibrium paths", "coordination failure" | Qualification. "Using Bayes' rule" | Exogenous test | Equal to actual by assumption (KLA-Q5, KLB-Q13) | Unilateral | Employer no, worker yes |
| Kim and Loury (2012) | Worker chooses group, then investment, employer pays posterior mean | "self-confirming", PSE and ASE fixed points (KLI-Q10) | Skill. Posterior formula | Exogenous test, plus a group choice whose benefit falls in cost (KLI-Q2, `prop2_strict`) | Fixed point | Unilateral | Worker yes, employer no |
| Coate and Loury, via Fang and Moro (2010) | Firm assigns given π, worker invests | Fixed point, "coordination failure" | Investment. "the Bayes' rule" (eq. 4) | Exogenous test | Fixed point (eqs. 15 to 17) | Unilateral | Employer no |
| Moro and Norman, Fang (2001), via Fang and Moro | Each player optimises against the others | "A Bayesian Nash equilibrium of the game" | Skill | Fang (2001) endogenous single crossing (FM-Q9, `fang_eq38`) | Fixed point | Unilateral | Worker yes |
| Craig and Fryer (2017) | Worker invests and applies, firm invests and hires, each given the other side's rates | "Bayesian Nash Equilibrium", beliefs "self-confirming" (CF-Q4) | Investment, both sides. Bayes | Exogenous signal of a chosen investment | Fixed point (CF-Q5, CF-Q9) | Mutual (CF-Q14) | Investment yes, match decisions no (CF-Q10, `match_gain_indep_own`) |
| Corneo and Jeanne (1994) | Consumer buys given σ and p (eq. 4) | "semi-separating equilibria", beliefs formed "rationally" | Income. "Using Bayes' rule" (eq. 8) | Type dependent (`buyers_upper`) | Fixed point, passive conjectures off path | None | Yes |
| Moav and Neeman (2012) | Each splits income given the social belief | "a unique fully separating equilibrium", intuitive criterion | Income. Consistency | Type dependent (`stepA1_ii`), plus exogenous h | Fixed point | None | Yes |
| Charles, Hurst and Roussanov (2007) | Each consumes given the status schedule (eq. 3) | "a signaling equilibrium" | Income. Correct inference assumed (CH-Q3) | Type dependent (`own_income_increasing`), group mean exogenous | Fixed point | None | Yes |
| Pesendorfer (1995) | Consumer picks a design given its pool, designer prices | "subgame-perfect equilibria", weak Markov coordination | Type. Pool share, Bayes not named | Type dependent (`single_crossing`, `separating_price_iff`) | Fixed point | None. Random matching within a pool, entry by purchase | Yes, u(i,j) |
| Austen-Smith and Fryer (2003) | Individual chooses schooling and contribution, group accepts, firms bid | "Perfect Bayesian equilibrium", D1 | Ability. Bayes (AS-Q4, AS-Q5) | Type dependent (`lemma3`) | Fixed point | Unilateral, the group accepts or rejects (`lemma2_accept_iff`) | Individual yes, group has no type |
| Eguia (2013) | Entrant assimilates by a cutoff, incumbents propose a barrier | "perfect Bayes Nash", "perfect Bayesian" | Ability, but no one updates (EG-Q10) | Type-dependent self-selection under a screen | Fixed point on composition | Unilateral entry | Yes, both sides (EG-Q7, EG-Q8) |
| Pęski and Szentes (2012) | Employer hires by a cutoff that depends on others' cutoffs | "steady state equilibria", myopic best-response stability | Nothing (PS-Q5) | None. Colour exogenous and uninformative | Fixed point | Unilateral | Yes, own colour (PS-Q8) |
| Eeckhout (2006) | Partners play a prisoner's dilemma | "stationary perfect public equilibrium" | No hidden type (EE-Q5) | None. Colour a coordination device | Fixed point | Random formation, either can exit | Same or different colour only |
| Currarini, Jackson and Pin (2008) | Each type chooses search time given q (Lemma 1) | "steady-state equilibrium" | Nothing | None | Fixed point | None, no rejection (CJP-Q7) | Same or different only (CJP-Q6) |
| Mailath and Postlewaite (2006) | Matching, attribute purchase in Sec. 6 | Stable each period, "rational expectations" | Nothing (MP-Q14) | None | Fixed point | Mutual | Yes |
| Cole, Mailath and Postlewaite (1991) | Bequest given the norm | "essentially a Nash equilibrium", subgame perfect | Nothing (CM-Q4) | None | Fixed point, the norm | Mutual, propose and accept | Yes, eq. 13 |
| Hitsch, Hortaçsu and Ariely (2006) | E-mail iff utility at least the reservation value (eq. 1), estimated as non-strategic | Stationary search equilibrium, Gale–Shapley stable | Nothing among users | None | Fixed point | Mutual | Yes, distance terms (HHA-Q11) |
| Rothschild and White (1993) | Planner's criterion, a cited Spence agent | None solved. Stability and competition cited | Not settled by the record | Cited only | Not modelled | Mutual, admission and choice (RW-Q5) | In the Spence sketch |

## 3. Four families

The table falls into four families by the two axes that matter most for
the friending-bias question, the signal and the consent.

1. **A decision against an exogenous signal.** Phelps, Aigner and Cain,
   Bohren, Imas and Rosenberg, Bohren, Hull and Imas, Laouénan and Rathelot,
   and Fryer and Jackson with no signal at all. One agent decides, beliefs
   are given or learned, and nobody else chooses. No equilibrium is needed.
2. **A belief equilibrium over decisions against an exogenous signal.**
   Coate and Loury, Kim and Loury, Craig and Fryer, Moro and Norman. Each
   agent solves a decision problem given the group's rate, and the rate
   must equal what the decisions produce. This is the family Fang and Moro
   call Bayesian Nash. Discrimination is a coordination failure.
3. **Type-dependent signalling.** Corneo and Jeanne, Moav and Neeman,
   Charles, Hurst and Roussanov, Pesendorfer, Austen-Smith and Fryer, Fang
   (2001), and Eguia's screen. Each agent's choice is a decision given the
   belief schedule, the schedule is a fixed point, and single crossing makes
   the choice reveal type.
4. **Matching and contact with types seen.** Currarini, Jackson and Pin,
   Mailath and Postlewaite, Cole, Mailath and Postlewaite, Hitsch,
   Hortaçsu and Ariely, Eeckhout, Pęski and Szentes. The contact decision is
   strategic and often mutual, but nothing is hidden.

## 4. Where A1 sits

A1 is in family 1 with one feature none of family 1 has.

| Axis | A1 | Row |
|---|---|---|
| Decision | Observer engages or refuses against the posterior on group, platform admits or excludes against the standings | A14, M18, M15 |
| Equilibrium | None settled. Mutual engagement is open | U2 |
| Hidden | Class. Bayes' rule | A2, A3, M1 to M3 |
| Signal | Marker exogenous. Passing is type dependent by single crossing but taken as a rule | A4, A12, M14 (ii) |
| Beliefs | Standings are the means of the observer's prior. No consistency condition | A14 |
| Consent | Unilateral. The pair's activity level is a joint rule, not Nash | A14, A15 |
| Own type | Yes, the observer's own class | M18 |

What the contrast says for the survey.

- **Own type with a hidden type.** In families 1 and 2 the decider's own
  type does not enter, or enters only the decider's own investment
  (Craig and Fryer's match decisions are independent of own investment,
  CF-Q10). In family 4 own type enters but nothing is hidden. In family 3
  own type enters the signaller's choice, not the reader's. A1's
  observer is a reader whose own class enters (M18). Two papers come near
  and must be named against contribution 1. Pesendorfer's consumer has
  `u(i,j)` with a hidden partner type, but chooses a pool, not a person,
  and nobody refuses. Eguia's incumbents prefer a barrier that depends on
  their own skill, but set it for a group, with no inference.
- **The nearest model of refusal.** Austen-Smith and Fryer's group accepts
  an individual iff the Bayesian posterior clears a threshold
  (`lemma2_accept_iff`), as A1's observer engages iff the expected distance
  clears one. Their group has no type ("anthropomorphized"), so the
  acceptance cannot depend on the reader's own class. Their individual
  signals by a single-crossing choice, where A1's marker is exogenous.
- **Beliefs.** Families 2 and 3 pin beliefs by a fixed point. A1 does not.
  Laouénan and Rathelot find guests' beliefs below the actual mean (LR-Q6),
  so leaving beliefs as a prior is defensible, but it should be said.
- **Consent.** Mutual consent with hidden types is in Craig and Fryer only,
  and there the match decision ignores own type. Closing U2 would put A1
  where no paper read sits, mutual consent, hidden class and own class in
  the decision.

## 5. What this means for the Literature section

The section now goes paper by paper. The contrast suggests it go family by
family, with A1 placed on the axes against each. A1's contribution is then
stated as a position on the axes. It is a reader of an exogenous marker
whose own class enters the decision, which no family has. Two moves would
take A1 into the other families, and each has its nearest papers.

| Move | Family | Nearest papers |
|---|---|---|
| Close U2, mutual engagement with hidden class | 2, as a Bayesian Nash equilibrium | Craig and Fryer, Hitsch, Hortaçsu and Ariely |
| Derive passing from its gain and cost | 3, type-dependent signalling | Austen-Smith and Fryer, Kim and Loury (2012), Corneo and Jeanne |
| Impose the consistency of standings | 2 | Kim and Loury, Coate and Loury |
