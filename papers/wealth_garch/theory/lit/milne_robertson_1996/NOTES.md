# Notes: Milne and Robertson (1996)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated printed page
  of the pinned draft (offset -2). Control: a fabricated quotation and a true
  quotation on the wrong page are both refused.
- The Lean results are declared in `theory/lean/MilneRobertson.lean`, which
  builds with no `sorry` and audits within propext, Classical.choice and
  Quot.sound.
- The printed barrier $x^*=0.17872$ from the closed form (MR-D2).

## Not verified

- The JEDC article (1996), which was not read. Before submission its
  statements of Propositions 1 to 3 and of footnote 5 should be checked
  against the draft.
- Sections 5 and 6 (investment, aggregation), read for orientation; no row
  uses them.

## Added 30 October 2026, for the theory paper's Sections 3.5 and 4.1

- The no-issuance base of the bank papers (MR-Q1), the penalty rate on a
  shortfall offered in place of liquidation (MR-Q3, MR-Q4), and concavity
  everywhere below the barrier without an option to issue (MR-Q6), against
  M4's convexity above the trigger.
