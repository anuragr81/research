# Fang and Moro (2010), notes

## What was verified

- Twenty-four quotations are found on the PDF pages they cite, by exact
  normalised containment in the cached text (LC-2). They copy the text
  layer's ligatures and avoid passages set in mathematical symbols (FM-D7).
- Each model the survey presents is formalised in its own section of the
  Lean file, as far as the survey states it (LC-4). The coverage table in
  `CLAIMS.md` has 31 general rows, 10 special cases and 13 not formalised.
- Our reading is consistent with the source except at six points where the
  printed formula or inequality fails and the Lean file shows it. These are the
  pooled variance (FM-D3), the exponent of Section 2.2.1 (FM-D11), the threshold
  order of Section 6.3 (FM-D13), the net social product (FM-D14), the strict
  Pareto step (FM-D15) and, not formalised, the sign of eq. (61) (FM-D16).

## What could not be verified

- The surveyed models' results are reported by the survey. A statement below
  about a cited model is the survey's account, tagged [C], unless the model
  is read in this bundle (FM-D10). Of the models named, only Aigner and Cain
  (1977) and Eeckhout (2006) are read.
- The formalisation is of the survey's statements of these models, not of
  the original papers.

## The bearing on Paper A1

Nothing in the survey contradicts the corrected account of A1. The survey's
scope is labour markets (FM-Q2), and no model it covers is set in social
contact between strangers.

### The puzzle

No surveyed model explains why friending bias across class rises with racial
diversity. Two touch the composition of a setting. Chaudhuri and Sethi make
integration act through peer effects on the cost of skill, and find that it
can make negative stereotypes impossible to sustain (FM-Q10,
`cs_peer_gap`), the opposite direction to the puzzle, and on skill, not on
contact. Moro and Norman find that extreme segregation is hard to sustain
against a large group (FM-Q17). Neither is a model of contact. [C]

### Budget compatibility

No surveyed model derives assortative matching or homophily from budgets.
The origins of group interaction the survey records are production
complementarities (Moro and Norman, `mn_eq18`), peer effects in the cost of
skill (Chaudhuri and Sethi, `cs_peer_gap`), repeated cooperation in a
marriage market (Eeckhout, FM-Q12), and, in a footnote, risk sharing within a
group (Fang and Loury, FM-Q19). The last is the only touch on pooled
resources. Nothing bears on affordability or shared consumption. [C]

### Avoidance that depends on the observer's class

No surveyed decision-maker's own type enters the decision. Firms search only
one group in Mailath, Samuelson and Shaked, but while they "would strictly
prefer to hire workers from the discriminated group" (FM-Q8,
`mss_VB_lt_VW`), so the avoidance runs one way and does not come from the
firms' own type. Lang's language barriers make signal noise depend on the
pair of groups (FM-Q4), the nearest thing to a pair-dependent effect, and it
acts on precision, not preference. The survey neither supports nor
contradicts directions (i) and (ii). [C]

### The penalty vanishing when class is shown

The survey states the property for the information-friction models. In
Phelps's model a precise signal "provides a precise estimate of the worker's
ability" (FM-Q16), and the group mean drops out of eq. (1)
(`phelps_precise_signal`, `control_phelps_noisy_signal`). In Section 4.2's
summary of the earlier models, "given skills, color does not play any
additional role in explaining racial wage differences" (FM-Q18). The
conditions are the information-friction channel alone and a fully revealing
signal. Altonji and Pierret's test with employer learning is the empirical
form (FM-Q11). Mailath, Samuelson and Shaked are the counter-case, a group
penalty with perfect information (FM-Q8). A1's property is the first case,
and A1 should cite the second as the channel in which it fails.

### The shadow market

The survey has several mechanisms of the kind the predecessor calls a shadow
market, all in labour markets or admissions.
- Correlated variables substitute for a monitored criterion. In Lundberg
  (1991) "companies may use variables that are correlated with race to evade the
  imposition of policies that monitor the employment process" (FM-Q22). [C]
- Group-neutral rules on correlated variables. Texas's top-ten-percent rule
  after the ban on race in admissions (FM-Q23), and Chan and Eyster's
  colour-blind office, which values a score partly by "the likelihood that a
  test score of t is coming from a black applicant" (FM-Q24, `ce_eq58`,
  `ce_share_decreasing`). [C]
- Discrimination moving to a channel the rule does not reach. Under a quota on
  assignment, Moro and Norman's wages differ by group in the simple task
  (FM-Q21, `mnaa_threshold_order`), and Coate and Loury's firms meet the
  quota while the stereotype persists (FM-Q20, `patron_eq51`). [C]
- A marker acquiring value. Fang's a priori irrelevant activity becomes a
  signal of skill and is taken up for that reason (FM-Q9, `fang_eq38`). [C]
- The choice of whom to meet. Mailath, Samuelson and Shaked's firms choose
  which group to search (FM-Q8), the nearest the survey comes to the contact
  channel. [C]
None of these has venues as intermediaries of social information, and none
has clients who weight social quality.

### Premises of A1 the survey does not share

- Vertical payoffs. Every decision-maker values the other's productivity.
- Endogenous skill in the coordination models (Sections 3 and 4), where A1's
  class is exogenous. Phelps's exogenous differences (FM-Q3,
  `phelps_case1`) are closer to A1.
- Ex ante identical groups in the coordination models, where A1's groups
  differ in their class distributions.
- Individual signals about the worker, where A1's marker carries information
  about the group only. The uninformative signal is A1's case
  (`control_uninformative_signal`).

## Controls

- LC-2. A fabricated quotation, "statistical discrimination has been applied
  mostly to social contact between strangers", the opposite of FM-Q2, is not
  found. A true quotation is not found on another page.
- Lean. Fifteen `control_` theorems each drop one condition of a main result,
  among them `control_condVar_zero_spread` (the noise penalty needs spread),
  `control_survey_pooled_variance` (the survey's pooled variance fails),
  `control_phelps_noisy_signal`, `control_cl_trivial`,
  `control_mss_no_firm_search`, `control_fang_eq38`, `control_fn_precision`
  and `control_ls_printed_net_product`.

## Page images read

The 110 dpi images of PDF pages 9, 27, 57 and 66 were rendered from the pinned
PDF with `pdftoppm`. They are reproducible from it and not pinned separately.
