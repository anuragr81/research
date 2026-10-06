# Claims about Costrell & Loury

**Paper.** Robert M. Costrell, Glenn C. Loury, "Distribution of Ability and
Earnings in a Hierarchical Job Assignment Model". Read as the **draft of 11
December 2003**; published as *JPE* 112(6), 2004. Evidence level
**[F, draft version]**. Proposition numbers are the draft's.

The Lean pass of 2026-10-06 re-read the same draft in full (Drive file
`job_assignment.pdf`, id `1ZER3CMY4nv4fFSEc7ux_hEQk-1uumFlC`, title page dated
December 11, 2003, 37 pages). The published *JPE* article is not on Drive, so
every proposition number below is still the draft's number.

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
