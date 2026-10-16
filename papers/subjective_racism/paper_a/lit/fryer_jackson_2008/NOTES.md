# Fryer and Jackson (2008), notes

## What was verified

- Fourteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the publisher's text layer (LC-2).
- Lemma 4 in one attribute, Proposition 1, Table 1 of Example 1, the
  prototype error of Example 1, the mean prediction error of a category, the
  identity in the proof of Proposition 2 and the two-category remark of
  Section 7 are Lean theorems (LC-4).
- Our reading is consistent with the source on these points.

## What could not be verified

- Example 4 and its footnote 22. Our computation from the stated primitives
  does not give the printed variations or the printed optimum (FJ-D3).
- The last step of the proof of Corollary 1 (FJ-D4). Corollary 1 is not
  formalised and not relied on.

## The bearing on Paper A1

Nothing in the source contradicts the A1 model. The two models answer
different questions. Fryer and Jackson ask how an observer who sees every
attribute stores past encounters. Paper A1 asks whom an observer engages
when class is unseen.

1. **Group avoidance from class homophily under inference (contribution 1).**
   Not in the source. The observer sees all attributes and stores them
   coarsely (FJ-Q5), so nothing is inferred about an unseen type from a
   marker. The observer's utility is the accuracy of a prediction (FJ-Q6,
   `meanDev_eq`) and does not depend on the observer's own attributes, so
   there is no observer-dependent squared gap. There is no engagement
   decision. Past encounters are given, and the observer chooses only how to
   file them (FJ-Q8).
   - **The σ² question.** A spread term does appear, in the observer's loss
     and not in a preference over groups. In one binary attribute the
     expected prediction error per member of a category is `2 p (1 − p)`,
     twice the variance of the share (`meanDev_eq`). The minority's category is
     more heterogeneous (FJ-Q1, FJ-Q2), so predictions about its members are
     less accurate on average. The metric is city-block, not squared, so the
     loss of a single member is that member's distance from the prototype and
     does not split into a gap term plus a variance term as A1's
     `E[(c_i − c_j)² | g] = (c̄_g − c_i)² + σ_g²` does. The harm the paper
     names falls on the high member through the category mean (FJ-Q3,
     `example1_prototype_error`), which corresponds to A1's mean term, not
     to its σ² term. Fryer and Jackson therefore give a reason the imputed
     variance of a minority is higher, namely coarser storage. They do not
     give a variance penalty in a choice of partner, and they do not show
     that every observer avoids a group with more spread.
2. **Budget compatibility as the micro-foundation of class homophily
   (contribution 2).** Not in the source. There are no budgets or prices.
3. **Venue exclusion and price screening (contribution 3).** Not in the
   source. FJ-Q13 says minority members benefit from congregating, because
   observers there have more minority encounters. That remark is about the
   observer's memory, it is stated without derivation, and it involves no
   screening on an unobserved type.
4. **The test of the class channel (contribution 4).** The source contains
   the rival prediction. In this model discrimination "can exist even when
   skills are observable" (FJ-Q11), because a shown attribute is still filed
   in a coarse category. A1 predicts that the group penalty in contact
   vanishes when class is shown. A finding that the penalty survives when
   class is shown would fit Fryer and Jackson and not A1, so the test
   separates the two models, and A1 should cite FJ-Q11 as the alternative
   its test rules out. FJ-Q12 gives a second separating prediction. More
   minority encounters lower categorical discrimination.

What A1 should cite. FJ-Q1 and FJ-Q2 as the source of a higher imputed
spread for small groups, distinct from A1's σ² term, which is the group's
actual within variance in class. FJ-Q11 as the rival to A1's test. FJ-Q14
records that the authors read chosen attributes as identity, informally and
without a model of passing.

## Controls

- LC-2. A fabricated quotation, "In our model, discriminating behavior
  disappears once skills are observable", the opposite of FJ-Q11, is not
  found. A true quotation is not found on a neighbouring page.
- Lean. `control_no_minority` shows that the strict optimality of the
  race-based categorization needs the black group to be a minority. With 25
  of each type the two merges tie. `control_two_categories` shows that the
  coarse sorting of the minority needs three categories. With two, the split
  by human capital is strictly best and the black workers are sorted by
  human capital.

## Suite status

The coordinator adds `FryerJackson2008` to the build roots. Until then LC-4
fails its build-root check, and the axiom audit, which imports the module,
cannot run. The audit was run by hand on a scratch copy of the file with
`#print axioms` appended for every cited theorem, and every theorem depends
only on `propext`, `Classical.choice` and `Quot.sound`.
