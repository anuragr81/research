# Notes: Peura and Keppo (2006)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim in the text layer of its
  printed page of the pinned JSTOR copy (offset 2161); each was first read
  on the page images of the earlier copy. Controls: a fabricated
  quotation, a true quotation on the wrong page, and a page before the
  first PDF page are all refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/PeuraKeppo.lean`, which builds with no `sorry` and audits
  within propext, Classical.choice and Quot.sound. Our reading of the
  closed forms (A2), (A4), (A5) and (17) is consistent with the source.

## Not verified

- The function $M$ of (14), Lemmas 1 to 5 and the existence of the
  solution of (18i)-(18ii). Read, not formalised.
- Every numerical result (Figures 2 to 7, Tables 1 to 3).

## Discrepancies

- PK-D4: the page range differs from BCVW's citation by one page.

## Controls

- `PeuraKeppo.control_dplus_needs_positive_rho`.
- `PeuraKeppo.control_paste_needs_distinct_roots`.

## Consequences for this bundle

- The baseline question in `TODO.md`. Peura and Keppo is a published bank
  capital model that was itself calibrated to US bank data and judged by
  its fit to capital ratios, with sigma defined on accounting returns
  (PK-D3), which removes the book-versus-economic problem of G8 for this
  baseline.
- Exact targeting (G15) is a prediction of this baseline too (PK-D2), and
  so is the underprediction of capital (PK-Q12).
- PK-Q18 calls for a graduated penalty below the requirement, which is the
  role of the shortfall penalty with $\lambda_S$ in our model.

## Added 23 October 2026

- PK-Q22 to PK-Q27, PK-D7 and three Lean results. Table 2 has the sign
  pattern of M6 in $(\Delta,K)$ at small $K$, and the authors pin $(D,K)$ from
  issue cost and frequency, the gap being driven by $K$ (PK-Q22, PK-Q23). The
  theory paper's identification headline cites this as its precedent.
- Control: `PeuraKeppo.control_delay_lowers_trigger_at_high_K`.

## Added 23 October 2026, second pass

- PK-Q28 to PK-Q31, from the Discussion and Conclusions (p. 2192). They
  state what the theory paper takes as the question the literature leaves
  open: the penalty at the minimum is extreme (PK-Q30), a graduated penalty
  is called for (PK-Q18), the specification is left to future research
  (PK-Q29), the parameters beyond the return moments are imposed by
  intuition (PK-Q28), and which constituency binds is the issue (PK-Q31).

## Added 24 October 2026, for the theory paper's common formulation

- PK-Q32 to PK-Q34, three rows of Table 2 at high cost. At the base point
  $(\Delta,K)=(0.5,1.00)$ the printed finite differences give a determinant
  of the dividend and issue barriers in $(\Delta,K)$ of $+0.04$, against
  $-0.29$ at $(0.08,0.10)$ (`PeuraKeppo.table2_det_value`,
  `PeuraKeppo.table2_det_high_K`). Read at one printed decimal (PK-D7).
