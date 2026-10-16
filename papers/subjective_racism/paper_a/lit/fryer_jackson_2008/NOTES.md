# Fryer and Jackson (2008), notes

## What was verified

- Fourteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the publisher's text layer (LC-2).
- The model is implemented in Lean in the paper's notation. Lemmas 2, 3 and 4,
  Theorem 1, Propositions 1 and 2, the equivalence of Section 3, Examples 1
  and 2 and the two-category claim of Section 7 are formalised in the paper's
  generality. Lemma 1, Lemma 5, Corollary 1 and Examples 3 and 4 are
  formalised in part (coverage table in `CLAIMS.md`).
- Our reading is consistent with the source on these points.

## What could not be verified

- Example 4's printed variations and optimum (FJ-D3).
- The parts of Lemma 1, Lemma 5 and Corollary 1 listed in the coverage table.

## The bearing on Paper A1

Nothing in the source contradicts the corrected account of A1. The two models
answer different questions. Fryer and Jackson ask how an observer who sees
every attribute files past encounters. A1 asks whom an observer engages when
class is unseen.

### The puzzle

The source bears on the puzzle as a rival with the opposite sign. In Fryer and
Jackson a group is filed coarsely because the observer meets it rarely
(FJ-Q1), and more encounters give finer categories and less discrimination
(FJ-Q12). Read as a theory of friending bias, the model predicts less bias
where groups mix more, while Chetty et al. (2022) find more bias across class
lines in racially diverse schools. The source therefore cannot explain the
puzzle, and A1 can cite it as the categorical account that the puzzle's sign
rules out.

### Budget compatibility

The source assumes no homophily, by budgets or otherwise. The decision maker's
payoff is the accuracy of a prediction (FJ-Q6, `FryerJackson2008.eu_iff_var`),
and the population of encounters is given rather than chosen (FJ-Q8). Nothing
bears on cost, affordability or shared consumption. A1's budget foundation
neither conflicts with the source nor gains support from it.

### Avoidance that depends on the observer's class

- **Both directions.** The source supports direction (i) for categorization,
  though not for avoidance. A minority is defined relative to "the set of
  objects that a given observer will have encountered" (FJ-Q9), so where each
  group mostly meets its own members, each group files the other coarsely.
  The face-recognition evidence the authors cite (PDF pp. 26–27) is of this
  two-way kind.
- **The low member of the high group.** The source says nothing about
  direction (ii). The observer's own attributes do not enter the loss
  (`FryerJackson2008.meanDev_eq`).
- **The σ² question.** A spread term appears, but in the observer's
  prediction loss and not in a preference over partners. The expected error
  per member of a category is `2 p (1 − p)` in one attribute, twice the
  variance of the share (`FryerJackson2008.meanDev_eq`). The metric is
  city-block, so the error does not split into a gap term plus a variance
  term as A1's `(c̄_g − c_i)² + σ_g²` does. The harm the paper names works
  through the category mean (FJ-Q3, `FryerJackson2008.example1_prototype_error`),
  which matches A1's mean term rather than its σ² term.

### The penalty vanishing when class is shown

The source states the contrast that A1 needs. A statistical discrimination
model "would not allow for such discrimination" once skills are observable,
while categorical discrimination "can exist even when skills are observable"
(FJ-Q11, FJ-Q5). The condition in the source is a limited number of
categories, under which a shown attribute is still filed in a coarse category
(FJ-Q2, `FryerJackson2008.example1_race_optimal`). With enough categories, or
with two in Example 1, the coarse filing of the minority disappears
(FJ-Q10, `FryerJackson2008.control_two_categories`). A1's property, that the
group penalty vanishes when class is shown, is therefore the statistical side
of the authors' own contrast, and it would fail for an observer with limited
categories in their sense.

### The shadow market

The source has no market of social information and no venues. Two remarks
touch the corrected account. The authors note that disparate-treatment law
rests on malevolent intent and that "regulating cognitive processes" is
impossible (FJ-Q14), so their discrimination escapes an intent standard
because it has no intent, not because it moves to channels the law does not
reach. They also advise minority members to apply to schools and firms "more
frequented by other minorities" and to congregate (FJ-Q13), which gives
information about where to go a value. That remark is about observers' memory,
is not derived, and involves no intermediary.

### Premises of A1 the source does not share

- Every attribute is observed (FJ-Q5). A1's class is unseen.
- The observer's limit is the number of categories, a bound on memory
  (FJ-Q8). A1's observer is Bayesian with group sufficiency.
- The loss is city-block, not squared (`FryerJackson2008.lemma4`), and the
  observer's own type does not enter it.
- There is no engagement decision and no equilibrium. The population of
  encounters is fixed.
- Coarseness comes from a group's rarity in the observer's experience, not
  from the group's within variance in class.

## Controls

- LC-2. A fabricated quotation, "In our model, discriminating behavior
  disappears once skills are observable", the opposite of FJ-Q11, is not
  found. A true quotation is not found on a neighbouring page.
- Lean. Each main result has a control that drops one condition.
  - `control_lemma4_needs_binary` drops binary attributes from Lemmas 3 and 4.
  - `control_no_minority` drops the minority from Theorem 1, Proposition 1
    and Example 1.
  - `control_lemma1_needs_triangle` drops the triangle inequality from
    Lemma 1.
  - `control_hypercube_tight` shows the bound in Lemma 5's fact is tight.
  - `control_corollary1` drops `r_E (2 − r_I) > 1`.
  - `control_eu_unequal_weights` drops equal weights from Section 3.
  - `control_two_categories` drops the third category from Example 1.
- The suite names `control_no_minority` and `control_two_categories`.
