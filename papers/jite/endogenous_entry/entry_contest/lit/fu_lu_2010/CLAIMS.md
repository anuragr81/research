# Claims about Fu & Lu (2010)

**Paper.** Qiang Fu, Jingfeng Lu, "Contest design and optimal endogenous
entry". Source read: **MPRA Paper No. 945**, posted 28 Nov 2006, the working
version of the *Economic Inquiry* article. Evidence level **[F, WP version]**.

`LITERATURE.tex` flags that the title differs between the MPRA posting and
Fu–Jiao–Lu's citation of it, and lists a published-version check as
outstanding. The MPRA number and date in the survey ("MPRA Paper 945,
November 2006") are confirmed from the PDF cover.

## Claims as stated in `LITERATURE.tex` §sec:entry

| ID | Claim | Source locator |
|---|---|---|
| FL-A | `M >= 3` identical contestants, sequential entry with full observation of current participants, organiser chooses prize and a per-entrant fee or subsidy. | Model section |
| FL-B | **Equation (7)**: total effort equals budget minus `N` times the entry cost. | p.10, eq. (7) |
| FL-C | This "is again the P7 accounting identity". | Survey's positioning |
| FL-D | Equation (7) is what drives **Theorem 1**: the optimal contest attracts exactly two entrants. | Theorem 1 |
| FL-E | Their conclusion lists different types of contestants as future research, so the design literature on endogenous entry is homogeneous-agent by construction. | Conclusion |
| FL-F | Footnote 6 uses sequential entry with observability to break the entrant-identity tie. | Footnote 6 |

## What is checkable here

- **FL-B** confirmed verbatim (see `NOTES.md`), and its monotonicity in `N` —
  which the source itself asserts — is checked (FL-1).
- **FL-D**'s arithmetic is checked: `E` falls in `N`, so with `N >= 2` the
  maximum is at `N = 2` with value `Pi_0 - 2C`, exactly Theorem 1's stated
  value (FL-2).
- **FL-C** is the same positioning claim as FJL-D. Its formal content is
  proved once, in `../fu_jiao_lu_2015/Accounting.lean`, rather than
  duplicated here; FL-4 records the instantiation.
- **FL-A, FL-E, FL-F** are structural and textual readings; unchecked.

A control (FL-3) verifies that the entry cost is what drives Theorem 1: with
`C = 0` the count-dependence vanishes entirely, so FL-1 and FL-2 are testing
the entry-cost channel rather than an artefact of the functional form.
