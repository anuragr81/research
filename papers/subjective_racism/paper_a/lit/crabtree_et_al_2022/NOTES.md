# Crabtree, Gaddis, Holbein and Larsen (2022), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite in the text layer
  (LC-2). Two elide a hyphenated line break (CR-D6).
- The paper's estimating model is formalised in general for its point
  estimates (`ols_dummies_group_means`). So are its name-selection rule
  (`distinctive_unique`), its stated counts (`sample_total`, `names_total`),
  and the difference in differences behind Figure 5 (`gap_change_did`). The
  coverage table in `CLAIMS.md` lists every result.
- Group sufficiency is A1's property, confronted with the paper's Figures 2
  and 4. The common-weight mixture for Study 5 is our reading. The paper
  states no model of perception.
- Our reading is consistent with the published article on these points.

## What could not be verified

- The plotted values, read from the images (CR-D4), and the robust standard
  errors and significance tests.
- Whether perceived class matches the groups' actual class. The paper makes
  no comparison with group data (CR-Q4).
- Any result by respondent type. Respondent class and race are collected in
  Studies 2–5 and not used in the reported results.

## Where the source bears against A1's model

A1's group sufficiency says the posterior on class given the marker is the
group's class distribution, so two members of one group are read alike. Read
with the name as the marker, the paper contradicts this. Names within one
intended group are perceived at different class levels (CR-Q8,
`control_name_heterogeneity`). The selection rule adds a second reason. The
0.45 threshold for Black surnames admits surnames used mostly by others
(`control_black_surname_threshold`), so the intended group of a name is not
its users' group. The group still separates most names, since the bulk of the
White and Black distributions do not overlap (CR-Q8). A1 can keep group
sufficiency only by taking the marker to be perceived race rather than the
name, and the paper does not report perceptions against perceived race
(CR-Q7, `ols_dummies_group_means` on intended race).

## The bearing on Paper A1

**The puzzle.** This is the source among the three that speaks most directly
to A1's mechanism for friending bias in diverse settings. A racial signal moves
perceived class (CR-Q1). Names of White and Asian people are read at the top,
Black next and Hispanic lowest (CR-Q6). The authors frame the step as belief
updating from race to class (CR-Q3) and allow that perceived race causes
perceived class (CR-Q15). Where a racial marker is present and class is not,
the marker carries class. The paper measures perception only, prompted by the
question (CR-Q13), with race asked first in Studies 1–3 (CR-Q14). It measures
no contact or friendship.

**Budget compatibility.** Not in the source. The paper offers no account of
why class matters to anyone, beyond naming the class channel as a possible
mechanism of discrimination (CR-Q12).

**Avoidance that depends on the observer's class.** Not reported. The paper
collected respondent class and race but reports only pooled results, so it
neither supports nor limits direction (i) or direction (ii).

**The penalty vanishing when class is shown.** The paper's exact statement is
for perception, not for contact. With an education cue shown beside the name,
the Black–White gap in perceived class narrows at an Ivy League degree
(CR-Q9) and stays significant (CR-Q10). The authors read Figure 5 as showing
that equal perceived standing needs "incredibly elite levels of education"
(CR-Q11). Its conditions are a single stated degree, three levels, a prompted
class question and two races. In our reading the gap is `(1 − w)(μW − μB)` for
a weight `w` on the stated education, which vanishes exactly when `w = 1`
(`gap_zero_iff`) and persists for any `w < 1` (`control_gap_persists`). This
is the statistical-discrimination form of A1's property, with the group gap
in Phelps's Case 1 surviving a partial individual signal. A gap that narrows
with the degree needs a weight that rises with it
(`varying_gap_needs_varying_weight`). So the paper is consistent with A1's
property under a cue observers do not take as complete. It cannot show the
property, since no cue in it reveals class fully.

**The shadow market.** Little. The paper is about experimental design, not
law. Two points bear on it. Names are markers that carry race and class
together and are used where race itself is not shown (CR-Q1), and the
authors note that a name's class content matters most "in most audit studies
outside employment contexts", where no other class cue is present (CR-Q11).
Contact settings are of that kind. Decision-makers in some contexts "may have
access to explicit signals of social class" (CR-Q13). Nothing on venues,
platforms or information about where to go.

**Premises of A1 the source does not share.**
- A1 requires in equilibrium that a group's imputed standing equal the actual
  mean of its visible members. The paper does not test this and warns that
  "people often misperceive economic reality" (CR-Q4).
- Names are not group-sufficient markers (CR-Q8), as set out above.
- Perceived class responds to an individual education cue (CR-Q2, CR-Q9), so
  observers combine the group with individual evidence when they have it,
  which A1 excludes by assumption for the marker alone.

## Controls

- LC-2. A fabricated quotation, "names commonly used to signal race/ethnicity
  do not influence perceptions about socioeconomic status and social class",
  the opposite of CR-Q1, is not found. A true quotation is not found on a
  neighbouring page.
- Lean. Each main result has a control. `control_lad_not_mean` shows the
  group-mean fit needs least squares. `control_black_surname_threshold` shows
  unique assignment needs thresholds above one half.
  `control_name_heterogeneity` shows group sufficiency fails with
  heterogeneous names. `control_gap_persists` shows the gap survives any
  education cue given less than full weight.
- Axiom audit by hand against the scratch olean, all 18 declared theorems
  checked, none beyond propext, Classical.choice and Quot.sound.
