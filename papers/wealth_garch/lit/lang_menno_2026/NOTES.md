# Notes: Lang and Menno (2026)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated printed
  page (offset -1). Controls refuse a fabricated quotation and a true one on
  the wrong page. Lean in `lean/mathlib/LangMenno.lean`.
- Read in full on 25 October 2026 by a reading agent of this session, every
  quotation then checked against the PDF by the verifier.

## Not verified

- Their propositions' proofs and figures.

## Consequences for this bundle

- Closest model found, but a different object: two periods, one
  representative bank, a common shock, a euro-area illustration (LM-Q2,
  LM-Q3, LM-Q6). No barrier, no cross-section, no fit to observed buffers,
  no out-of-sample test.
- In theory, supports a floor at minimum plus conservation buffer (LM-Q1,
  LM-Q5, LM-Q7), the construction of our 10.5% after 2009.
- Their equilibrium has no buffers far above the requirement (LM-Q7), so
  it cannot produce F2; the enhanced model's level result is not in it.

## Added 25 October 2026, for the theory paper's Section 3.1

- LM-Q13 to LM-Q24 and LM-D2 to LM-D7 were added from a second full reading,
  checked by the same suite. They record the form of the stigma cost, a
  charge per unit of loans switched on by an indicator that is the same at
  every shortfall (`LangMenno.stigma_flat_below`), and the absence of any
  fixed or proportional issuance cost.
- The theory paper cites the record in L51, against the graduated shortfall
  penalty (`LangMenno.graduated_grows_below`).
