# Peng, Zhang and Pan (2010), notes

## What was verified

- Fourteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the cached text (LC-2).
- Eqs. (1), (2), (9), (13-1), (14), Theorems 1, 2 and 4 (finite iterates), the
  step of Theorem 7, Example 1 and Table 4 are Lean theorems in the paper's
  notation (LC-4). The coverage table in `CLAIMS.md` has 28 rows, 16 general,
  7 special cases and 5 not formalised.
- Our reading is consistent with the source on these points, with three
  exceptions. Theorem 5 is false as stated (PZ-D11, `theorem5_fails`), the
  printed initial value of eq. (14) has the wrong sign (PZ-D10), and the
  convergence step of Theorem 7 needs the contraction by `α` (PZ-D12).

## What could not be verified

- Theorem 3, the limits in Theorems 4, 6 and 7, and the experiments are read,
  not formalised.
- The direction from soft to virtual evidence is a citation of Chan and
  Darwiche (2005), which is not read (PZ-D5). The Lean proof is our own.

## The bearing on Paper A1

Nothing in the paper contradicts the corrected account of A1. The paper is a
methods paper on belief update in Bayesian networks and bears on one
methodological statement of A1 and on one property.

### The puzzle

The paper says nothing about contact, class or diversity.

### Budget compatibility

The paper has no preferences and no matching. Nothing in it bears on the
origin of homophily, on cost, or on shared consumption.

### Avoidance that depends on the observer's class

Not in the paper, which has no decision-maker. Its updates do not depend on
who holds the belief beyond the prior.

### The penalty vanishing when class is shown

The paper supplies the inferential half of this property. Hard evidence is
the soft evidence that puts mass one on the observed state (PZ-Q10,
`hard_evidence`). Applied as a virtual likelihood, it conditions on that
state and forgets the prior's weights across states
(`hard_evidence_virtual`, `hard_evidence_forgets_prior`); a likelihood short
of hard evidence keeps them (`control_soft_keeps_prior`). In A1, showing class
is hard evidence on class, so the posterior on class no longer depends on the
group prior, and with it the group penalty goes. The link from belief to
penalty is A1's, not the paper's.

### The shadow market

Not in the paper.

### Premises of A1 the paper does not share

- Soft evidence as a true observation of a distribution that must be
  preserved (PZ-Q9). A1's marker is read as evidence about the group, which
  is the virtual reading (PZ-Q1, PZ-Q8). In one update the two readings reach
  the same posteriors. Jeffrey's rule with `R(Y)` is Bayes with
  `L(y) = R(y)/P(y)` (PZ-Q11, `eq9_soft_eq_virtual`), and Bayes with any `L` is
  Jeffrey's rule with `R(y) ∝ L(y)P(y)` (`virtual_eq_jeffrey`, PZ-Q3, PZ-D6).
  The conditions are dominance, `R(y) > 0` only where `P(y) > 0`
  (`control_dominance_needed`, PZ-D3), and a likelihood computed from the prior.
  A1's statement that soft evidence adds nothing over Bayes with virtual
  evidence in a single update is consistent with the source.
- Several evidences at once. Virtual evidences commute (`virtual_commute`)
  but do not preserve soft marginals (PZ-Q12, PZ-Q14,
  `control_two_virtual_not_soft`), and Jeffrey's rule in sequence does not
  commute (PZ-Q13; cf. JF-Q6). A1 has one reading of a marker, so this lies
  outside it.
- Our inference, not the paper's, is that under group sufficiency a marker
  carries a likelihood that does not depend on the observer's prior, so the
  virtual reading is the natural one for A1.

If A1 keeps Jeffrey's rule anywhere, Theorems 1 and 2 characterise it as the
distribution with marginal `R(Y)` nearest to `P` in I-divergence and in total
variation (`theorem1`, `theorem2`). Theorem 5 should not be cited (PZ-D11).

## Controls

- LC-2. A fabricated quotation, "a soft evidence cannot be converted into a
  virtual evidence, even for a single update", the opposite of PZ-Q2, is not
  found. A true quotation is not found on another page.
- Lean. `control_two_virtual_not_soft` shows the single-update equivalence
  fails with two evidences, `control_dominance_needed` and
  `control_theorem1_dominance` show the dominance condition is needed,
  `control_theorem4_factor` that Theorem 4 needs constraints inside `Y`,
  `control_soft_keeps_prior` that only hard evidence forgets the prior, and
  `control_eq14_printed` that the printed eq. (14) value is wrong.

## Page images read

The 110 dpi images of PDF pages 3, 4 and 7 were rendered from the pinned PDF
with `pdftoppm`. They are reproducible from it and not pinned separately.
