# Charles, Hurst and Roussanov (2007), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the text layer (LC-2). CH-Q13 carries an elision
  where the text layer places the parenthesis of `g(c^i)` inside the sentence.
- Eq. (4)'s sign argument, the Theory Appendix's status function, its
  no-status share and its stated solution are Lean theorems in the paper's
  notation (LC-4).
- Our reading is consistent with the working paper on these points.

## What could not be verified

- The QJE (2009) version is not read (CH-D1). A row that cites the 2009
  article for anything in this record carries an unverified mark until the
  article is read.
- The Theory Appendix's change of variables is not the chain rule (CH-D2).
  Only the stated solution is formalised.
- The definition of the dispersion regressor is inconsistent between the
  text and the note to Table 6 (CH-D3), so only its sign is used.

## The bearing on Paper A1

1. **How the group enters.** The observer does not infer income from race.
   Outsiders see the person's visible consumption and know the group's income
   (CH-Q2), and the signalling equilibrium separates, so consumption reveals
   income (CH-Q3). Race fixes the reference group (CH-Q7, CH-Q11), and the
   group mean is the yardstick of status, `s = g(c)/μ_G` (CH-Q6, CH-Q13). A
   richer own group therefore lowers a member's status at given income. In A1
   a richer group raises the class an outsider imputes to a member. The two
   roles of the group mean are different objects, so A1 should not cite this
   paper as an instance of an outsider imputing class from the group. The
   group's variance is named in footnote 24 and left out of the model (CH-Q4).
2. **Contribution 1, avoidance from homophily under inference.** Not in the
   paper. Observers are homogeneous outsiders in anonymous interactions
   (CH-Q12), no one chooses contact, and nothing depends on the observer's own
   income. The variance does not enter as a penalty. The empirical dispersion
   coefficient is negative for Whites (CH-Q8) and positive and not
   significant for minorities (CH-Q9), and its definition is uncertain
   (CH-D3).
3. **Contribution 2, budget compatibility.** Not in the paper.
4. **Contribution 3, venues and price screening.** Not in the paper. Visible
   goods signal income by their cost, but no venue, club or price screens
   whom a person meets.
5. **Contribution 4, the class-channel test.** Not in the paper in A1's form.
   The nearest result is Table 8, where the racial gap in visible spending
   vanishes once the own-group mean income is controlled (CH-Q10). That test
   conditions on the group mean and measures the sender's spending. A1's test
   conditions on the individual's class and measures the observer's contact.
   A1 can cite Table 8 as a precedent for a race coefficient that disappears
   when the income channel is controlled, and for the authors' reading that
   race matters only through the reference group (CH-Q11).

What A1 might cite. The working paper as evidence that race is treated as
an economically meaningful reference group, the explicit note that the group
variance matters in a pure signalling model but is not modelled (CH-Q4), and
Table 8 as the consumption-side analogue of a test that removes the group
coefficient by controlling the income channel.

## Controls

- LC-2. A fabricated quotation, "outsiders infer a person's income from the
  person's race alone, using the variance of group income", the opposite of
  CH-Q2 and CH-Q4, is not found. A true quotation is not found on a
  neighbouring page.
- Lean. `control_status_gain_rising` shows the decline in the group mean needs
  `s_cμ < 0`, and `control_soc_needed` that it needs the second order
  condition.
