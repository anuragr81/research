# Peng, Zhang and Pan (2010), notes

## What was verified

- Fourteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the cached text (LC-2).
- Eq. (2), eq. (9) in both directions, Theorem 2 and the numbers of Example 1
  (Table 1, rows 1, 2 and 4) are Lean theorems in the paper's notation
  (LC-4).
- Our reading is consistent with the source on these points.

## What could not be verified

- Theorems 3 to 7 and the experiments are read, not formalised.
- The direction from soft to virtual evidence is a citation of Chan and
  Darwiche (2005), which is not read (PZ-D5). The Lean proof is our own.

## The bearing on Paper A1

Nothing in the paper contradicts the A1 model as described, and the paper
bears on none of contributions 1 to 4. It bears on one methodological
statement of A1, that soft evidence adds nothing over Bayes with virtual
evidence in a single update.

The exact relation, for one update on a partition `Y` of the joint states,
has three parts.

- Jeffrey's rule with soft evidence `R(Y)` is Bayes' rule with the virtual
  likelihood `L(y) = R(y)/P(y)` (eq. 9, PZ-Q11, `eq9_soft_eq_virtual`).
- Bayes' rule with any virtual likelihood `L(y)` is Jeffrey's rule with
  `R(y) = L(y)P(y)/Σ_y' L(y')P(y')` (`virtual_eq_jeffrey`,
  `virtual_soft_sums_to_one`). The paper states the equivalence for "a single
  uncertain evidence" (PZ-Q3, PZ-Q2), and the converse written out is ours
  (PZ-D6).
- So in one update the two rules reach exactly the same set of posteriors.
  The conditions are that `P(y) > 0` wherever `R(y) > 0`, the dominance
  condition (`control_dominance_needed`, PZ-D3), and that the likelihood is
  computed from the prior, since `P(y)` appears in eq. (9).

A1's statement is therefore consistent with the source, with two
qualifications A1 should carry.

1. The equivalence is for one update. With two soft evidences, the converted
   virtual evidences commute (`virtual_commute`) but satisfy neither
   marginal (PZ-Q12, PZ-Q14, `control_two_virtual_not_soft`), and Jeffrey's
   rule applied in sequence does not commute (PZ-Q13; cf. JF-Q6). A1 has one
   reading of a marker, so the difference lies outside it rather than being
   refuted by it.
2. The two kinds of evidence differ in what they hold fixed. Soft evidence
   fixes the marginal (PZ-Q9) and virtual evidence fixes the likelihood ratio
   (PZ-Q1, PZ-Q8). The same soft evidence is a different likelihood under a
   different prior. Our inference, not the paper's, is that under group
   sufficiency a marker carries a likelihood that does not depend on the
   observer's prior, so the virtual reading is the natural one for A1.

Theorem 2 adds a characterisation A1 may use if it keeps Jeffrey's rule
anywhere. Besides the minimum I-divergence (Theorem 1), the rule is the
distribution with marginal `R(Y)` nearest to `P` in total variation
(`theorem2`).

## Controls

- LC-2. A fabricated quotation, "a soft evidence cannot be converted into a
  virtual evidence, even for a single update", the opposite of PZ-Q2, is not
  found. A true quotation is not found on another page.
- Lean. `control_two_virtual_not_soft` shows the single-update equivalence
  fails with two evidences, and `control_dominance_needed` shows it needs
  `P(y) > 0` where `R(y) > 0`.

## Page images read

The 110 dpi images of PDF pages 3, 4 and 7 were rendered from the pinned PDF
with `pdftoppm`. They are reproducible from it and not pinned separately.
