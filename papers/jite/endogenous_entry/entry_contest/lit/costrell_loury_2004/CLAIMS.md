# Claims about Costrell & Loury

**Paper.** Robert M. Costrell, Glenn C. Loury, "Distribution of Ability and
Earnings in a Hierarchical Job Assignment Model", *Journal of Political
Economy* 112(6), 1322-1363 (2004). The **published version was read in full on
13 October 2026** [F], from the author's Drive upload
`Distribution_of_ability_and_earnings_in.pdf` (42 pages, cached at
`~/.cache/entry_contest/costrell_loury_2004_jpe.pdf`, sha256 pinned in
`verify_cl.py`). The earlier passes read the draft of 11 December 2003; their
tables below keep the draft's page numbers, and the table that follows maps
them to the journal.

## Published version: quotations checked against the journal text

Pages are the journal's. `verify_cl.py` checks each quotation verbatim on its
page. The journal's symbol font renders Greek letters as Latin ones (θ as `v`,
β as `b`, μ as `m`), so the quotations avoid formulas.

| ID | Page | Quotation | Draft locator it replaces |
|---|---|---|---|
| CLP-1 | 1327 | "The key feature of the model is that jobs must be filled in fixed proportions to one another" | §2, p.6 |
| CLP-2 | 1327 | "completely characterize the technology" | §2, p.6 (row L9) |
| CLP-3 | 1330 | "the role of the ability distribution in the wage function is entirely captured by" | p.9 |
| CLP-4 | 1330 | "This improves ability in the right tail and reduces ability in the left tail (with possible multiple crossings in between)." | p.9 |
| CLP-5 | 1330 | "The effect depends on whether the worker on the margin between jobs, at quantile" | p.9 |
| CLP-6 | 1330 | "If proper job assignment is most critical in the top jobs" | p.9 |
| CLP-7 | 1330 | "Conversely, if assignment matters most toward the bottom of the spectrum" | p.9 ("toward to" in the draft) |
| CLP-8 | 1342 | "with possibly multiple crossings (see the Appendix for the proof)." | §4.3, p.19 |
| CLP-9 | 1342 | "but the two distributions have the same mean" | Prop 5, p.19 |
| CLP-10 | 1343 | "So the inequality of the lemma is the principal characterization result for second-order stochastic dominance (the expectation of a convex function is no less under a riskier distribution)." | Lemma 1, p.20 ("principle" in the draft) |
| CLP-11 | 1344 | "then a greater dispersion of ability (on fixed support) lowers wages for the most (least) skilled workers and compresses (widens) the wage span." | discussion of Prop 6, p.21 |
| CLP-12 | 1352 | "quite unlike the fixed-proportions case (proposition 6), where the shape of" | §5.4, p.28 |
| CLP-13 | 1353 | "the reoptimizing assignment reverses these results." | §5.4, p.28 |
| CLP-14 | 1353 | "We have found opposite effects on the wage span from a mean-preserving spread in abilities under Cobb-Douglas and Leontief crowding technologies" | §5.5, p.28 |
| CLP-15 | 1358 | "Integrate by parts, and recall" | Appendix, p.33 |
| CLP-16 | 1341 | "Consider two tests of worker abilities, and suppose that test 1 is more informative than test 2 in the sense of Blackwell (1953)." | Theorem 1, p.18 |
| CLP-17 | 1340 | "Thus we are neglecting here the important issue of how changing the quality of information available to employers about worker abilities affects the incentives that workers have to acquire skills." | (footnote 14) |
| CLP-18 | 1355 | "Under perfect observability, the model generalizes nicely, and we are able to show that investment in ability is efficient." | (conclusion) |
| CLP-19 | 1355 | "However, if there is imperfect observability, investment is suboptimal and, more interesting, may in some cases become even more suboptimal when information becomes less imperfect." | (conclusion) |

**Draft against journal.** Every proposition, the theorem, Lemma 1 and the two
corollaries keep their numbers, and their statements are unchanged in
substance. Equation numbers agree up to (16); the draft's (17), Lemma 1's
inequality, is unnumbered in the journal, where (17) is output under variable
proportions. The draft's Sections 5.4 and 5.5 are the journal's Sections V.D
and V.E. Three wordings in the draft quotations recorded in `NOTES.md` were
corrected in print: "toward to the bottom" (CLP-7), "principle
characterization" (CLP-10) and "non-decreasing" (CLP-15, "nondecreasing"). The
journal's Proposition 10 ends with a sentence on mean-preserving spreads, and
the theorem table's quotation of it stops before that sentence. Nothing in the
journal signs a change in the wage distribution by two ranks: the two-job
results read one marginal quantile fixed by technology, and the continuum
results read the curvature or shape of `β`.

## Claims as stated in `LITERATURE.tex` §sec:assignment and §Verdict

| ID | Claim | Source locator | Relevance claimed to `PROOFS.tex` |
|---|---|---|---|
| CL-A | In the two-job model, jobs are filled in proportions `θ`, `1−θ`, and the wage schedule is determined by one sufficient statistic `μ̂ = F⁻¹(θ)`. Any shift raising `μ̂` raises low-ability wages and lowers high-ability wages. | §2, eqs. (1)–(2) on p.7; sufficient-statistic sentence on p.9 (locator corrected 2026-10-06, the earlier row gave p.9 for the equations) | Structural template for P9-gen's pivot |
| CL-B | Under an MPS on fixed support they **state** that the effect depends on whether the marginal worker at quantile `θ` lies in the upper or lower tail: `θ` high ⟹ `μ̂` rises, wage distribution narrows; `θ` low ⟹ `μ̂` falls, widens. | p.9 | "the same logic as P9-gen" — `dk*` flips on `w_(k*)` vs `x₀`. `LITERATURE.tex` also calls this a rule located "relative to the spread's crossing point" (742–744, 1074–1075); see NOTES finding F2 |
| CL-C | "P9-gen should be presented as the entry-count analogue of the Costrell–Loury two-job comparative static." | Survey's instruction | Positioning of P9-gen (`prop:P9gen`) |
| CL-D | Proposition 6: with `G` riskier than `F` on `[0,1]`, concave `β` lowers `w(0)` and widens the span; convex `β` lowers `w(1)` and narrows it. "The curvature of the payoff map decides. For us that map is `κ`." | Prop 6, p.20 | Governs direction when there is no single marginal agent |
| CL-E | Lemma 1 shows `G` riskier than `F` implies the quantile functions reverse the order, `F⁻¹` riskier than `G⁻¹`. | Lemma 1, p.20. `LITERATURE.tex` 754–755 places Lemma 1 "in the proof of Proposition 5"; the source places Lemma 1 in the proof of Proposition 6 (NOTES finding F3) | Tool |
| CL-F | Proposition 5 signs an aggregate (output) under a general spread with multiple crossings **by integrating `Γ(ρ) = ∫_ρ^1[G⁻¹−F⁻¹]` against `dβ`**. | Prop 5, p.19; Appendix proof, p.33 | Open item 2 — general spreads |
| CL-G | Theorem 1: a Blackwell-more-informative signal induces a second-order-riskier distribution of posterior means, so any MPS result has an information reading. | Theorem 1, p.18; proof pp.32–33 | Interpretation |
| CL-H | Proposition 10 (Cobb–Douglas crowding) removes the curvature dependence altogether: a spread always narrows the span. | Prop 10, p.27; proof p.35 | Robustness caution |
| CL-I | **Superseded.** The earlier §Verdict text read "Two independent published sources — Suen's Proposition 2 and Costrell–Loury's Proposition 5 — sign an aggregate under a general mean-preserving spread **using the same technique**: quantile-difference decomposition at the crossing points, **concavity of the payoff map**, and a **hazard-rate or log-concavity condition** on the own-side distribution." | Survey's synthesis | `LITERATURE.tex` 782–806 now carries the corrected text recorded in NOTES pass 1, and the corrected claim is CL-M below |

## Claims as stated in `PROOFS.tex` (and the corrected §Verdict)

| ID | Claim | Source locator | Where in our documents |
|---|---|---|---|
| CL-J | The marginal quantile `θ` is fixed by technology, because jobs come in fixed proportions, so only `μ̂ = F⁻¹(θ)` moves when `F` does. | §2, p.6 (fixed proportions, "θ,β0 and β1 completely characterize the technology"); p.9 | `PROOFS.tex` 1244–1248, 1309–1313, 1636–1637 |
| CL-K | Under Cobb–Douglas crowding, where the assignment re-optimises, the direction of the inequality effect reverses relative to the fixed-proportions Proposition 6. | Prop 10, p.27; reversal sentence and §5.5 opening, p.28 | `PROOFS.tex` 1250–1257, 1678–1682. The source states the reversal for concave `β` only (NOTES finding F1) |
| CL-M | Proposition 5 signs output under a general spread and needs only a non-decreasing weight `β`, with no concavity and no hazard-rate condition; Costrell–Loury "sign smooth aggregates" and need "a ... monotone-weight condition". | Prop 5, p.19; Appendix p.33 | `LITERATURE.tex` 793–798; `PROOFS.tex` 1316–1318, 1433–1439, 1499, 1793–1796, 1813. The monotone weight covers output; the wage-span results need curvature (NOTES finding F4) |

## What is checkable here

- **CL-A** is the wage algebra (1)–(2): checked as the sign pattern of
  `∂W/∂μ̂` on each side of the margin (CL-1), plus continuity at the margin
  and the no-arbitrage slopes as transcription checks (CL-2, CL-3).
- **CL-F**'s operative condition is checked (CL-4) with a control confirming
  it is load-bearing (CL-5).
- **CL-D/CL-E**'s direction is checked as the SOSD characterisation on an
  explicit mean-preserving pair (CL-6).
- **CL-I is where the survey erred.** See `NOTES.md`; `LITERATURE.tex` has
  since been corrected.
- **CL-G** was confirmed by reading against the source; it is textual and is
  unchecked.
- **CL-C** is a positioning instruction, not a claim about the paper.
  `RECONSTRUCTION.md` §7 records the disanalogy it must carry.
- **Lean (`CostrellLoury.lean`, checks CL-L1 to CL-L4).** CL-A, CL-B, CL-J
  (two-job wage schedule and the tail rule at a fixed marginal rank), CL-F and
  CL-M (Proposition 5 by Abel summation against a non-decreasing weight),
  CL-D and CL-E (Lemma 1 and the span part of Proposition 6 by a double Abel
  summation), CL-H and CL-K (the Proposition 10 sign skeleton set against
  Proposition 6), and CL-C at the level of one bridge theorem. Every theorem
  in the file maps to one of these IDs. The table with page locators and
  verbatim quotes is in `NOTES.md`, section "Lean pass".
