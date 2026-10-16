# Kim and Loury (2012), notes

Pages here are PDF pages of the issue, as in `CLAIMS.md`. The journal page is
the PDF page minus 3.

## What was verified

- Fourteen quotations are found on the PDF pages they cite in the publisher's
  text layer, by exact normalised containment (LC-2).
- Eq. (15), the three cases of Lemma 1, the monotonicity of `ΔU` in `c`,
  Proposition 1, the selection of Lemma 2 and Proposition 2 in weak form, and
  the trivial ASE are Lean theorems for a finite cost distribution, with `V₀`
  and `V₁` taken as given functions (LC-4).
- The axiom audit of LC-4 imports the module and so needs it as a build root.
  Until the root is added, the audit was run on a copy of the file with
  `#print axioms` for each of the twelve cited theorems. Each uses only
  `propext`, `Classical.choice` and `Quot.sound`.
- Our reading is consistent with the source on these points.

## What could not be verified

- The existence of a non-trivial ASE and the comparison of inequality between
  endogenous and exogenous groups are argued informally on p. 14 and not
  proved (KLI-Q12, KLI-Q13, KLI-D5). No row may cite them as results.
- Eqs. (2) to (8), and with them the hump of `R`, are checked by hand against
  the page images and not formalised.
- The published version (*Journal of Public Economics*, 2019) is not read
  (KLI-D1).

## The bearing on Paper A1

Nothing in the article contradicts the A1 model as described. One reading
qualifies how A1 states what is known. The article's passing result is
positive selection into the group with the greater return to skill, `R`,
which is the group with the better reputation only where `R` rises in the
reputation (KLI-Q11, KLI-D2). The authors' own Figure 3 (p. 18) draws a case
in which the better-reputed group offers the lower return, and there the able
are drawn the other way (`control_reputation_without_return`). The setting is
a wage, `ω` times the posterior chance of skill given a signal and the group
(KLI-Q8, KLI-Q9), and ability is a low cost of acquiring skill, which is
chosen (KLI-Q6). A1's class is given and its setting is contact. The
benchmark of Proposition 2 is random membership at the same beliefs, `G(R(π))`,
so the result says the disfavoured group's investment rate is below what it
would be without selection, `φ(πB, πA) < G(R(πB))`, rather than below the
group's rate before anyone moved.

1. **Group avoidance from class homophily under inference.** Not in the
   article. The evaluator is a risk-neutral employer who pays the posterior
   mean (KLI-Q9). The employer has no type, so nothing depends on the
   observer's own standing, and the wage is linear in the posterior, so a
   group's spread carries no penalty. What depends on a person's type in the
   article is the gain from joining a group, on the side of the person who
   chooses the group (KLI-Q2, `dU_antitone`). A1's dependence is on the side
   of the person who evaluates.
2. **Budget compatibility as the micro-foundation of class homophily.** Not
   in the article. There is no consumption, budget or interaction between
   workers.
3. **Venue exclusion and price screening as certification of class.** Venue
   exclusion is not in the article. Certification is, in words and in a
   labor market: a costly programme that the good types join more readily
   because they gain more from a job (KLI-Q14), and, in the introduction, the
   use of chosen cultural traits in screening (KLI-Q5). The screen works by a
   gain that rises with the unseen type, not by ability to pay. A1 should cite
   the article for self-selection into a costly certificate and claim only
   the price mechanism and the venue as its own.
4. **The group penalty vanishes when class is shown.** Not in the article.
   The employer always sees an individual signal as well as the group
   (KLI-Q8), and the article never varies its precision. The article's
   remark on chosen markers runs the other way, a further group marker
   improving screening (KLI-Q5).

**The consistency condition.** The ASE requires each group's believed
investment rate to equal the actual rate of the members who chose it
(KLI-Q10, `IsASE`, `trivial_ase`). A1's condition, that the imputed standing
equal the actual mean of the visible members, has this form. A1 should cite
the article for it and state the difference, a mean of an exogenous class in
A1 against a rate of chosen skill in the article.

**Unseen type inferred from a visible group.** Present (KLI-Q3), with an
individual signal alongside (KLI-Q8), which A1's group sufficiency rules out
for the marker.

## Controls

- LC-2. A fabricated quotation, "The disproportionately less talented
  workers, whose human capital investment costs (c) are relatively higher,
  choose the 'affect' that corresponds to the greater return", the opposite
  of KLI-Q11, is not found. A true quotation is not found on a neighbouring
  page.
- Lean. `control_no_selection_equal_return` shows that with equal returns
  there is no selection even when the beliefs differ, so Proposition 2 needs
  `R(πA) ≠ R(πB)`. `control_reputation_without_return` shows that a better
  reputation with higher wages at every skill level, but a lower return to
  skill, draws the able away, so "better reputation" alone does not give the
  selection.
