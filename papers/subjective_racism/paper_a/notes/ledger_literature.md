# Ledger diagrams of three approaches in the literature, 17 October 2026

A planning note, not part of the paper. The author asked for ledger diagrams
of at most three approaches that address the same or a similar problem as
the manuscript, drawn from the papers whose Lean records exist. The drawing
is `notes/ledger_literature.tex`, one page per approach, rendered to
`notes/ledger_literature_1.png` to `_3.png`.

## 1. Which approaches, and why

`notes/motivation_scan.md` names the rivals to the manuscript's account. Three
are chosen, one for each part of the manuscript's problem.

| Approach | Papers | Part of the manuscript's problem | Lean record |
|---|---|---|---|
| Statistical discrimination | Phelps (1972), Aigner and Cain (1977) | Reading an unseen type from the group | `Phelps1972.lean`, `AignerCain1977.lean` |
| Categorisation | Fryer and Jackson (2008) | How an observer files people by group, and the sign of contact | `FryerJackson2008.lean` |
| Choice and chance in friendship | Currarini, Jackson and Pin (2008) | Homophily in friendships, the outcome Chetty et al. measure | `CurrariniEtAl2008.lean` |

Left out, with the reason:

- **Pęski and Szentes (2012).** Their model runs over generations, and the
  claim the manuscript uses (PS-Q4) is a statement about statistical
  discrimination, already drawn in diagram 1.
- **Status signalling, Moav and Neeman (2012) and Charles, Hurst and
  Roussanov (2007).** These read income from visible consumption. They are
  the nearest fourth approach, and they are left out only because of the
  limit of three.
- **Eguia (2013), and Kim and Loury.** These are about assimilation and
  group reputation over time, outside the static problem.

## 2. Rules of the drawing

The manuscript's rules hold. An entity acts or is acted on and is not a role,
an attribute or an action. The status of an arrow is read against the
source, not the manuscript.

- **Solid.** The paper derives the action, and its Lean record proves it.
- **Dashed.** The paper assumes the action, or claims it without the record
  formalising the claim.
- **Dotted.** The paper states the action in words and does not model it.

Each page closes with a note on where the approach parts from the manuscript.

## 3. Statistical discrimination

**Entities.** Worker and Employer. The worker box holds the role Applicant and
the population state (each group's mean and spread, and the test's noise).
The employer's attributes are risk aversion, a belief about each group's
mean that equals the actual mean (assumed, AC-Q2, AC-Q3), and a law of equal
pay for equal score that would leave the gap (stated only, AC-Q15).

| From | To | Action | Source | Status | Lean |
|---|---|---|---|---|---|
| Applicant | Employer | Shows colour, a proxy for data not sampled | PH-Q1 | Dashed | |
| Applicant | Employer | Shows the test score, evidence about the individual | eq. 1, PH-Q4 | Dashed | |
| Population state | Employer | Mean, spread and noise set the weight on the score | eqs. 2, 3, PH-Q4 | Solid | `Phelps1972.pred`, `Phelps1972.weights_sum`, `AignerCain1977.eq2_least_squares`, `AignerCain1977.fn15` |
| Employer | Applicant | Pays the prediction. With equal spreads the lower-mean group is paid less at every score, and with a wider spread its line crosses. Recorded as pay by group at equal score | Phelps Cases 1 and 2, AC Figs. 1 and 5 | Solid | `Phelps1972.case1_parallel`, `Phelps1972.case2_crossing`, `AignerCain1977.fig1_crossing`, `AignerCain1977.fig5_parallel` |
| Employer | Applicant | Risk averse, pays the mean less a variance penalty, so the noisier group is paid less | eq. 8, Fig. 2, AC-Q6, AC-Q9 | Solid | `AignerCain1977.eq8_order`, `AignerCain1977.fig2_black_paid_less` |
| Employer | Applicant | Hires only above a cutoff, a rule blind to colour that leaves a group gap | Fig. 4, AC-Q12, AC-Q17 | Solid | `AignerCain1977.fig4_truncation` |
| Employer | Population state | The prior comes from experience with the groups and may perpetuate itself | PH-Q2, PH-Q3 | Dotted | |
| Employer | Population state | Tests tailored to each group would find a market | AC-Q16 | Dotted | |

**Against the manuscript.** The score is evidence about the individual, and
the manuscript's marker is evidence about the group alone (PH-Q4, M1). Every
employer ranks the groups alike (`AignerCain1977.contrast_cara_common_ranking`),
and in the manuscript the observer's own class sets the ranking
(`AignerCain1977.contrast_quadratic_ranking_depends_on_own_class`, M18).

## 4. Categorisation

**Entities.** Person only. The box holds the roles Decision maker and Person
encountered, and the population state (how often each type appears among the
decision maker's encounters, FJ-Q9). The decision maker's own attributes do
not enter its loss (`FryerJackson2008.meanDev_eq`). Title VII's intent
standard does not reach categorisation, stated only (FJ-Q14).

| From | To | Action | Source | Status | Lean |
|---|---|---|---|---|---|
| Person encountered | Decision maker | Is met and seen in full, at frequencies taken as given | FJ-Q5, FJ-Q8 | Dashed | |
| Decision maker | itself | Files past encounters into categories that minimise the variation within them, and with two categories splits by human capital | eqs. 4, 5, Lemma 4, Theorem 1, Prop. 1, FJ-Q10 | Solid | `FryerJackson2008.lemma4`, `FryerJackson2008.theorem1`, `FryerJackson2008.prop1`, `FryerJackson2008.control_two_categories` |
| Population state | Decision maker | The rare types are lumped into wide categories | FJ-Q1, Example 1, Lemma 5 and Corollary 1 in part | Solid | `FryerJackson2008.example1_race_optimal`, `FryerJackson2008.lemma5`, `FryerJackson2008.corollary1_step` |
| Decision maker | Person encountered | Predicts by the prototype, below the type of a high member of a pooled minority. The payoff is the accuracy of the prediction | eq. 3, FJ-Q3, FJ-Q6, FJ-Q7 | Solid | `FryerJackson2008.example1_prototype_error`, `FryerJackson2008.eu_iff_var` |
| Person encountered | Population state | A new encounter is filed where variation rises least | Prop. 2 | Solid | `FryerJackson2008.prop2` |
| Person encountered | Population state | More encounters with a group give finer categories and less discrimination | FJ-Q12 | Dashed | |
| Person encountered | itself | Members of a minority gain by congregating together | FJ-Q13 | Dotted | |

**Against the manuscript.** Nothing is unseen (FJ-Q5). The decision maker's
own attributes do not enter. More contact gives less discrimination (FJ-Q12),
the opposite sign to the gradient in friending bias.

## 5. Choice and chance in friendship

**Entities.** Person and Matching process. The person box holds the roles
Searcher and Friend met, and the population state (each type's share and
inflow, set by the school). The searcher sees only same or different
(CJP-Q5), has preferences that are the same for every type (CJP-Q6), and has
a same-type bias by assumption (Definition 7).

| From | To | Action | Source | Status | Lean |
|---|---|---|---|---|---|
| Searcher and Friend met | each other | Befriend at every meeting, nobody rejected. Recorded as homophily equal to the meeting chance | CJP-Q7, Sec. 4.4 | Dashed | `CurrariniEtAl2008.H_eq_q` for the record |
| Friend met | Searcher | Same types share interests and sustain risk sharing more easily | CJP-Q15, CJP-Q16 | Dotted | |
| Searcher | Matching process | Spends search time, chosen to maximise the utility of friends less its cost | Lemma 1, eq. 3 | Solid | `CurrariniEtAl2008.lemma1` |
| Matching process | Searcher | With unbiased meeting the chance of the own type is its share of the stock, so one type's homophily is another's heterophily | Prop. 2, eq. 6 | Solid | `CurrariniEtAl2008.prop2`, `CurrariniEtAl2008.eq6` |
| Matching process | Searcher | Meeting biased toward the own type, assumed, and largest for small groups in the fit | eq. 7, CJP-Q11, CJP-Q14 | Dashed | |
| Searcher | Matching process | Joins clubs and activities of the own type, meets friends of friends | CJP-Q10, CJP-Q17 | Dotted | |
| Population state | Matching process | The stocks set the meeting chances in steady state, and an equilibrium with relative homophily exists | eq. 8, Prop. 3, Claim 1 | Solid | `CurrariniEtAl2008.eq8_iff`, `CurrariniEtAl2008.prop3`, `CurrariniEtAl2008.claim1` |
| Population state | Searcher | A larger own share gives more same-type meetings and more friends, under same-type bias | Props. 4, 5 | Solid | `CurrariniEtAl2008.prop4`, `CurrariniEtAl2008.prop5_t_mono`, `CurrariniEtAl2008.prop5_q_mono` |

**Against the manuscript.** Types are seen and nobody refuses (CJP-Q7).
Preferences ignore the agent's own type (CJP-Q6). Homophily is by race and
not across class.

## 6. What the four ledgers show together

| | Manuscript | Statistical discrimination | Categorisation | Choice and chance |
|---|---|---|---|---|
| Entities | Person, Platform | Worker, Employer | Person | Person, Matching process |
| What is unseen | Class | Productivity | Nothing | Nothing |
| Evidence read | A marker, about the group alone | Colour and a score about the individual | Every attribute | The type |
| Observer's own type in the decision | Yes, own class | No | No | No |
| Refusal | Yes | Pay or a hiring cutoff | No decision on contact | No, every meeting is a friendship |
| Where homophily comes from | Separate budgets and one shared activity | Not addressed | Not addressed | Assumed taste and assumed meeting bias |

Only the manuscript's ledger has an arrow on which the observer's own type
changes the action, and only the manuscript derives homophily rather than
assuming it.
