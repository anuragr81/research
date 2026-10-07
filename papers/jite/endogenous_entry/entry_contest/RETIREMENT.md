# Retirement of PROOFS.tex (pass 6, 7 October 2026)

`PROOFS.tex` was removed after this record was written. Its last version is in
git at commit `7132717f`
(`git show 7132717f:papers/jite/endogenous_entry/entry_contest/PROOFS.tex`).
Line numbers below refer to that version. `checks/verify_retirement.py` reads
it from git, takes an inventory of its environments, sections, Lean names and
numbers, and fails if any item is neither present in the documents that
replace it nor listed here with a reason.

The documents that replace it are `MANUSCRIPT.tex` (model rows M1 to M27,
Appendices A to G), `PROOFS_ADDENDUM.tex`, `LITERATURE.tex` (the section on
the contribution, carried whole), `MEASUREMENT_MAP.tex`, `TODO.md` (item O1,
the open items) and the `lit/` records.

## Environments

| Line | Environment | Goes to |
|---|---|---|
| 497 | claim `claim:bmweak`, burden-monotonicity strictly weaker than concavity | M24, with the corrected construction. The sine example is dropped, since it is not increasing and violates the divergence assumption (`bm_example_not_monotone`, `bm_example_not_diverges`) |
| 713 | proposition, P1 | M1 |
| 727 | proposition, P2 | M2 |
| 749 | theorem, key result (P3) | M3 |
| 770 | corollary, monotonicity | M4 |
| 788 | proposition, P5 | M5 |
| 815 | proposition `prop:countinv`, count invariance | M6 |
| 847 | proposition `prop:idunique`, identities pinned | M7 |
| 879 | proposition `prop:assort`, assortative selection | M8 |
| 972 | proposition, P6 | M9, with the factor `V` the original omitted |
| 1016 | theorem, P7 | M10 |
| 1106 | lemma `lem:crossing`, single crossing | M5 |
| 1118 | proposition `prop:anon`, anonymity | M26, with the boundary in M27 |
| 1126 | theorem, P8 | M11 |
| 1151 | theorem `thm:P9`, the linear spread | M13 and M15 |
| 1224 | theorem `thm:P9gen`, pivot-spreads | M13 |
| 1314 | proposition `prop:endogenous`, endogenous margin | M13, whose hypothesis is read at the pre-change margin, and M12 |
| 1386 | proposition `prop:tailprop`, margin condition | M12 |
| 1524 | proposition `prop:strict`, strictness | M15 |
| 1562 | proposition, P-MU | M16, with M17 to M22 |

## Sections

| Section | Goes to |
|---|---|
| Scope and verification methodology | Appendix B, rewritten for the Lean-only state |
| What the three evidence tiers mean | Appendix B. The tiers are retired |
| Verification index | The Lean column of the model table. Its sampling entries are dropped |
| Primitives | Appendix D |
| Terminology | Appendix C |
| Proofs | Appendix A, rows M1 to M27, as in the table above |
| P1: representation | M1 |
| P2: first-order stochastic dominance | M2 |
| P3: the difference identity | M3 |
| Corollary: monotonicity | M4 |
| P5: equilibrium existence and uniqueness | M5 |
| P5-inv: the count is an invariant of the game | M6, M7 |
| N4: assortative entry as a selection | M8 |
| P6: the \texorpdfstring{$\mu\to0$ | M9 |
| R1: no collapse threshold (refuted) | Appendix E |
| P7: saturation, uniformly in \texorpdfstring{$Q$ | M10. The paragraph on what is new goes with the contribution section to `LITERATURE.tex` |
| R2: the \texorpdfstring{$Q$ | Appendix E |
| P8: prize and affordability, separated | M11 |
| P9: inequality and aggregate expenditure | M13, M15 |
| P9 general: arbitrary pivot-spreads | M13 |
| N1: the margin is endogenous, and the rule survives it | M13 |
| R1: the margin condition, and the spreads it covers | M12, M13 |
| The band: which margins the margin condition covers | M14 |
| P9 strictness | M15 |
| P-MU: the \texorpdfstring{$Q$ | M16 to M22 |
| Contribution and its precedents | `LITERATURE.tex`, §sec:contribution-carried, carried whole as input to the conclusions |
| How this addresses the referee reports | Appendix F, and Appendix G for the coverage |
| Asymmetric \texorpdfstring{$k$ | Appendix F |
| Prize versus affordability | Appendix F |
| The mobility ceiling | Appendix F |
| Open items | `TODO.md`, item O1 |

## Lean names of the core file

These theorems stay in `lean/EntryContest.lean`, which `verify.sh` still
builds and audits. The manuscript cites their Mathlib successors instead.

| Core name | Successor in the manuscript |
|---|---|
| `antitone_of_step`, `strict_of_step`, `step_factor`, `step_sign` | M3, M4 (`step_identity`, `Delta_step_nonpos`, `Delta_step_strict`) |
| `below_subset_of_pathwise`, `cdf_le_of_pathwise` | M2 (`p2_fosd`) |
| `cost_falls_above_pivot`, `cost_rises_below_pivot`, `order_preserved`, `entry_preserved_above_pivot`, `nonentry_preserved_below_pivot`, `entry_set_grows_above_pivot` | M13 (`pivot_rise`, `pivot_fall`) |
| `count_ge_kstar`, `count_le_kstar` | M6 (`count_invariance_fin`) |
| `enters_downward_closed`, `entry_monotone`, `nonentry_monotone` | M5, M11 (`single_crossing`, `kstar_mono`) |
| `entry_ceiling_of_marginal_exit`, `strict_drop_of_marginal_exit`, `linSpread_strict_anti_in_lam`, `linSpread_unbounded_below` | M15 (`exit_below_pivot`, `p9_strict`) |
| `linSpread_isPivotSpread`, `linSpread_monotone` | M13 (`spread_isPivot`) |

## Numbers not carried

Pass 6 drops statements that only sampling supports. Every number below is a
sampling count or a numerical illustration, except the two column widths.

| Number | Where | Reason |
|---|---|---|
| 1500 | P4 index row and l.1299, adversarial pairs and economies sampled | sampling count |
| 230 | P9 index row and l.1199, cases for the rise branch | sampling count |
| 629 | l.1200, strict cases of the fall branch | sampling count |
| 729 | P9-gen index row and l.1297, control violations | sampling count |
| 141 | l.862, "11,141 sampled instances" | sampling count |
| 0.6,0.45 | l.1196, a lognormal used to illustrate P9 | numerical illustration |
| 0.4110 | l.1197, the entry rate at which P9's sign flips for that lognormal | numerical illustration |
| 235 | l.595, a column width of the terminology table | layout |
| 185 | l.595, a column width of the terminology table | layout |
| 16/77 | M27 writes it as a fraction, `\tfrac{16}{77}` | carried in another form |
| 16/45 | M27 writes it as a fraction, `\tfrac{16}{45}` | carried in another form |
| 2.9,2.8,2.0 | M13 writes `(29/10, 14/5, 2)` | carried in another form |
| 3.6,2.2,1.9 | M13 writes `(18/5, 11/5, 19/10)` | carried in another form |
| 4,2.8,2.2,2 | M14 writes `(4, 14/5, 11/5, 2)` | carried in another form |
| 4.1,2.7,2.6,1.6 | M14 writes `(41/10, 27/10, 13/5, 8/5)` | carried in another form |
| 4.6,2.25,2.25,1.9 | M14 writes `(23/5, 9/4, 9/4, 19/10)` | carried in another form |

## Statements dropped as sampling-only

1. Count invariance with ability rising in wealth (A1-4b). Appendix D says it
   held in every sampled economy and is not claimed.
2. The frequencies of the superseded band (47.2% and the T-series of
   `checks/verify_tailband.py`), dropped at pass 4c.
3. Violation counts of the sampled suites cited as support (`verify_p9gen.py`,
   `verify_p89.py`, `run_all.py`). The suites still run as illustrations.
4. The crossover in `μ` for Beta(2,2) scores, already withdrawn as a result in
   `PROOFS.tex`. `MEASUREMENT_MAP.tex` says no row defines it.
