# Crabtree, Gaddis, Holbein and Larsen (2022), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite in the text layer
  (LC-2). Two elide a hyphenated line break (CR-D6).
- The stated sample and name counts, group sufficiency of a name with its
  failure under the paper's Figures 2 and 4, and our linear reading of
  Study 5 are Lean theorems (LC-4). Group sufficiency is A1's property, here
  confronted with the paper's figures. The Study 5 mixture is our reading;
  the paper states no model of perception.
- Our reading is consistent with the published article on these points.

## What could not be verified

- The plotted values, read from the images (CR-D4).
- Whether perceived class matches the groups' actual class. The paper makes
  no comparison with group data (CR-Q4).
- Any result by respondent type. Respondent class and race are collected in
  Studies 2–5 and not used in the reported results.

## Where the source bears against A1's model

A1's group sufficiency says the posterior on class given the marker is the
group's class distribution, so two members of one group are read alike. Read
with the name as the marker, the paper contradicts this. Names within one
intended group are perceived at different class levels, with Roosevelt above
and Lakisha below the Black mean, and with Study 4's name means spread over a
wide range in each group (CR-Q8, `control_name_heterogeneity`). The group
still separates most names, since the bulk of the White and Black
distributions do not overlap (CR-Q8). A1 can keep group sufficiency only by
taking the marker to be perceived race rather than the name, and the paper
does not report perceptions against perceived race (CR-Q7).

## The bearing on Paper A1

**How accurately class is read from a marker.** The paper does not measure
accuracy. It measures perceived class from a name and never compares it with
the named person's or the group's actual class. The authors note that
"people often misperceive economic reality" (CR-Q4). The paper is evidence
that class is inferred from the racial marker, not that the inference is
correct.

**A1's premise that the group serves as evidence about class.** Supported in
perception, which is the most direct support among the three sources read in
this wave. Racially distinctive names move perceived education, income and
class in a tiered order (CR-Q1, CR-Q6), and the authors frame the mechanism
as belief updating from race to class (CR-Q3) and allow that perceived race
causes perceived class (CR-Q15). Three limits carry over to any citation.
Perceived class is elicited by prompting (CR-Q13). In Studies 1–3 the race
question comes first (CR-Q14). And the regressor is the intended signal, not
the perceived race (CR-Q7).

**Is a racial signal read as a class signal?** Yes. That is the paper's
finding (CR-Q1), on 11,530 respondents and 1,000 names (CR-Q5), across four
platforms and seven years of data collection.

**Contributions 1 to 4.**
1. Group avoidance from class homophily. Not present. No observer-type
   dependence is reported, although respondent class was collected, and no
   group variance enters. The within-group spread of name means (CR-Q8) is a
   spread of perceptions across names, not the class variance of a group.
2. Budget compatibility. Not present.
3. Venue exclusion. Not present.
4. A test of the class channel. The perception-level counterpart is here.
   Study 5 shows an education cue with the name. The Black–White gap in
   perceived class narrows at the Ivy League degree (CR-Q9) and remains
   significant (CR-Q10). In our linear reading the gap is `(1 − w)(μW − μB)`,
   which vanishes only when the stated education takes full weight
   (`gap_zero_iff`, `control_gap_persists`), and its narrowing needs a weight
   that rises with the degree (`gap_indep_signal`, `gap_shrinks`). The lesson
   for A1's test is that "class shown" must be a cue observers treat as
   complete. A residual group gap under a partial cue does not by itself
   reject the class channel. The test on behaviour, crossing race with class,
   is proposed as future work (CR-Q12) and is not run. No contact outcome is
   measured.

**What A1 should cite it for.** The racial signal read as a class signal
(CR-Q1, CR-Q6), the persistence of the group in perceived class beside an
individual education cue (CR-Q2, CR-Q10), and the authors' own framing of the
class channel as a mechanism of discrimination (CR-Q12). Not for accuracy of
class reading, and not for group sufficiency of names.

## Controls

- LC-2. A fabricated quotation, "names commonly used to signal race/ethnicity
  do not influence perceptions about socioeconomic status and social class",
  the opposite of CR-Q1, is not found. A true quotation is not found on a
  neighbouring page.
- Lean. `control_name_heterogeneity` shows group sufficiency fails once two
  names of one group are perceived differently. `control_gap_persists` shows
  the group gap survives any education cue given less than full weight.
- The suite's LC-4 build-root check fails until the coordinator adds
  `CrabtreeEtAl2022` to `lakefile.toml`. The axiom audit passed against an
  olean built in the reader's scratch directory.
