# TODO: the enhancement paper

The paper of the enhancement thread of papers/wealth_garch, written as a
skeleton under papers/MANUSCRIPT_SKELETON.md. The writing rules are the
parent's notes/writing_discipline.md. Its evidence is the parent bundle's:
Lean under lean/mathlib, records under lit/, results files under comparison/.

## Decisions

- 2026-10-25. The author asked for the write-up after the 2025 hold-out, in
  the order baseline failure, what buffers are not sized to, the enhanced
  model and its record, limits. The paper lives in enhancement/, apart from
  the parent manuscript (asymmetric capital buffer control) and the theory
  paper, because its question is the level of buffers, which neither of
  those addresses.
- 2026-10-25. The floor of the enhanced model is 10% to 2009Q4 and 10.5% from
  2010Q1, from Orozco and Rubio (2025) and Lang and Menno (2026)
  (comparison/PREREG_comparison_20.md). The choice was made after a floor
  near R had been seen to work, and the 2025 hold-out decides the frozen
  model (comparison/results_holdout.md).
- 2026-10-25. Scored facts are F1 and F2 only (comparison/PREREG_comparison_11.md).

## PLAN

| Pass | Work | State |
|---|---|---|
| 1 | Scaffold: tables, IDs, appendices, map, checker with controls, verify.sh | done 25 October 2026 |
| 2 | Lean: the parent project; no new file needed so far | done (parent) |
| 3 | Lean for any new model claim (none yet; a monotonicity of $u_0$ in $s$ would support the reading of E3 and is not claimed) | open |
| 4 | Model rows M1 to M5 with proofs, assumptions A1 to A6, map | done 25 October 2026 |
| 5 | Appendices B to D filled; E (refuted conjectures) not yet written, candidates the measures of E3 that failed their gates | open |
| 7 | Novelty: the four papers of comparison/NOVELTY_20261025.md read; C5 to C7 wait on a citation search | open |
| 8 | Conclusions and introduction written from the rows; overreach review of each headline | first draft 25 October 2026 |
| 9 | Literature table L1 to L14 | done 25 October 2026 |
| 10 | Readability rounds under the writing discipline | open |

## Open

- MM rules of the skeleton specification beyond MAP-1 and MAP-2 (every
  Terminology term a map name, colons in the map's prose) are not yet
  checked.
- The measure of risk in A6 contains banks' own smoothing (E8); a model that
  separates the shock from the bank's response is the natural next model.
- The constant margin smaller banks hold (comparison/results_do_nothing.md)
  and the size gradient (E9) are unexplained.
