# Fang and Moro (2010), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the cached text (LC-2). They copy the text
  layer's ligatures and avoid passages set in mathematical symbols (FM-D7).
- Eq. (1), the conditional variance of Section 2.2.1, eqs. (4), (6) and (7)
  and the pooled variance of Section 7.1 are Lean theorems in the survey's
  notation (LC-4).
- Our reading is consistent with the source on these points, apart from the
  pooled variance, where the survey's expression is not the variance of the
  pooled population (FM-D3).

## What could not be verified

- The results of the surveyed models are reported by the survey without
  proofs. A statement below about a cited model is the survey's account,
  tagged [C], unless the model is read in this bundle (FM-D10). Of the
  models named below, only Aigner and Cain (1977) and Eeckhout (2006) are
  read.

## The bearing on Paper A1

Nothing in the survey contradicts the A1 model as described. The survey
states its own scope as labour markets (FM-Q2), and no model it covers is set
in social contact between strangers.

The brief asks for every surveyed model that has the observer's own type in
the decision, a variance penalty in matching, or a contact or social setting.

**The observer's own type in the decision.** No surveyed model has it as the
survey describes them. The decision-makers are employers, admission offices,
planners or firms, with payoffs that do not depend on their own type.
- Lang (1986), cited as "Lang (1996)" (FM-D5), appears only in footnote 2, as
  a rationale for noisier signals, "communication of language barriers
  between employers and minorities" (FM-Q4). That makes the noise depend on
  the pair of groups, not the preference on the observer's own class. [C]
- Chan and Eyster (2003) give the admission office a taste for diversity
  (eq. 56), a preference over the group composition of the admitted, not over
  distance from the office's own type. [C]
- Fadlon (2010), "Statistical Discrimination and the Implications of
  Employer-Employee Racial Matches", is cited only in footnote 31, on returns
  to aptitude scores. The survey says nothing of its model. [C, title only]

**A variance penalty in matching or selection.** Three, all in labour markets.
- Aigner and Cain (1977). Risk-averse employers pay less to the group with
  the larger conditional variance (FM-Q5, FM-Q6). Read in full in
  `aigner_cain_1977/`.
- Lundberg and Startz (1983). The group with the noisier signal invests less
  and earns less on average (eq. 2). [C]
- Cornell and Welch (1996). With slots scarce, the group with the more
  informative signal has the larger variance of expected productivity and
  fills the top (FM-Q7). The variance helps the group at the top of the
  distribution, the opposite sign to A1's `σ_g²`. [C]
- Phelps's Case 2 alone gives no average penalty (FM-Q3).

**Contact or social settings.**
- Eeckhout (2006). A marriage market in which partners randomly match to play
  a repeated prisoner's dilemma, with race as a public randomisation device
  (FM-Q12). Read in full in `eeckhout_2006/`.
- Chaudhuri and Sethi (2008). Peer effects through a segregation parameter,
  where integration can make negative stereotypes impossible to sustain
  (FM-Q10). Integration enters the cost of skill, not a choice of contact. [C]
- Fang (2001). An a priori irrelevant cultural activity becomes, through
  self-selection, a marker of skill (FM-Q9). The survey calls it endogenous
  group formation. [C]
- Fang and Loury (2005a, b), footnote 13. A theory of "dysfunctional
  collective identity in a repeated risk sharing game". [C, footnote only]
- Chung (2000). Role models, beliefs about the labour market updated from
  observed placements. [C]

**Venues, pools and prices that screen on an unseen type.**
- Schwab (1986). A standardised market pays everyone the pool's average
  productivity, and the marginal worker is worth more than the wage (FM-Q14).
  A pool valued at its average type with selective exit. [C]
- Epple, Romano and Sieg (2008). Vertically differentiated colleges compete
  for students with aid and admission, caring about ability, income and race
  (FM-Q13). [C]
- Mailath, Samuelson and Shaked (2000). Firms search only white workers, and
  the group penalty survives "perfect information" (FM-Q8). [C]

**An equilibrium consistency condition on group means.** Coate and Loury's
eq. (17), `π*_j = G(I(θ̃(π*_j)))`, makes the firms' belief about a group equal
the group's realised investment, and Fang (2001) has the fixed point
`Ψ(π_A) = π_A` on the share of skilled among those who take the activity. [C]

By contribution, the survey bears on A1 as follows.

1. **Contribution 1.** The variance half has a labour-market precedent in
   Aigner and Cain, which the survey restates (FM-Q5, FM-Q6). The
   observer-dependent half is in no surveyed model.
2. **Contribution 2 (budget compatibility).** Not in the survey.
3. **Contribution 3 (venue exclusion from client inference).** Not in the
   survey. Schwab (FM-Q14) and Epple, Romano and Sieg (FM-Q13) are pools and
   venues that screen on type, and A1 should check them against its own
   venue before claiming novelty. Neither, as the survey describes them,
   builds exclusion from clients' inference from a group marker.
4. **Contribution 4 (the test of the class channel).** The labour-market
   counterpart is Altonji and Pierret's test with employer learning (FM-Q11),
   in which statistical discrimination predicts that the effect of race falls
   as the employer learns the worker's productivity. Mailath, Samuelson and
   Shaked give the rival prediction, a penalty under full information
   (FM-Q8). A1's test for contact is the same logic in a new setting and
   should cite both.

Passing as a chosen marker has a precedent in Fang (2001), where a chosen
activity becomes a marker of skill (FM-Q9), and the survey itself allows
chosen group characteristics (FM-Q1).

## Controls

- LC-2. A fabricated quotation, "statistical discrimination has been applied
  mostly to social contact between strangers", the opposite of FM-Q2, is not
  found. A true quotation is not found on another page.
- Lean. `control_condVar_zero_spread` shows the noise penalty needs spread
  within the group, and `control_survey_pooled_variance` shows the survey's
  pooled variance fails the law of total variance.

## Page images read

The 110 dpi images of PDF pages 9, 27, 57 and 66 were rendered from the pinned
PDF with `pdftoppm`. They are reproducible from it and not pinned separately.
