# Claims about Costrell & Loury

**Paper.** Robert M. Costrell, Glenn C. Loury, "Distribution of Ability and
Earnings in a Hierarchical Job Assignment Model". Read as the **draft of 11
December 2003**; published as *JPE* 112(6), 2004. Evidence level
**[F, draft version]**. Proposition numbers are the draft's.

## Claims as stated in `LITERATURE.tex` §sec:assignment and §Verdict

| ID | Claim | Source locator | Relevance claimed to `PROOFS.tex` |
|---|---|---|---|
| CL-A | In the two-job model, jobs are filled in proportions `θ`, `1−θ`, and the wage schedule is determined by one sufficient statistic `μ̂ = F⁻¹(θ)`. Any shift raising `μ̂` raises low-ability wages and lowers high-ability wages. | §2, eqs. (1)–(2), p.9 | Structural template for P9-gen's pivot |
| CL-B | Under an MPS on fixed support they **state** that the effect depends on whether the marginal worker at quantile `θ` lies in the upper or lower tail: `θ` high ⟹ `μ̂` rises, wage distribution narrows; `θ` low ⟹ `μ̂` falls, widens. | p.9 | "the same logic as P9-gen" — `dk*` flips on `w_(k*)` vs `x₀` |
| CL-C | "P9-gen should be presented as the entry-count analogue of the Costrell–Loury two-job comparative static." | Survey's instruction | Positioning of P9-gen (`prop:P9gen`) |
| CL-D | Proposition 6: with `G` riskier than `F` on `[0,1]`, concave `β` lowers `w(0)` and widens the span; convex `β` lowers `w(1)` and narrows it. "The curvature of the payoff map decides. For us that map is `κ`." | Prop 6 | Governs direction when there is no single marginal agent |
| CL-E | Lemma 1 shows `G` riskier than `F` implies the quantile functions reverse the order, `F⁻¹` riskier than `G⁻¹`. | Lemma 1 | Tool |
| CL-F | Proposition 5 signs an aggregate (output) under a general spread with multiple crossings **by integrating `Γ(ρ) = ∫_ρ^1[G⁻¹−F⁻¹]` against `dβ`**. | Prop 5 + Appendix | Open item 2 — general spreads |
| CL-G | Theorem 1: a Blackwell-more-informative signal induces a second-order-riskier distribution of posterior means, so any MPS result has an information reading. | Theorem 1 | Interpretation |
| CL-H | Proposition 10 (Cobb–Douglas crowding) removes the curvature dependence altogether: a spread always narrows the span. | Prop 10 | Robustness caution |
| CL-I | **§Verdict / open item 2:** "Two independent published sources — Suen's Proposition 2 and Costrell–Loury's Proposition 5 — sign an aggregate under a general mean-preserving spread **using the same technique**: quantile-difference decomposition at the crossing points, **concavity of the payoff map**, and a **hazard-rate or log-concavity condition** on the own-side distribution." | Survey's synthesis | Drives the proposed extension: "replace the pivot hypothesis with log-concavity of `1 − Λ`" |

## What is checkable here

- **CL-A** is the wage algebra (1)–(2): checked as the sign pattern of
  `∂W/∂μ̂` on each side of the margin (CL-1), plus continuity at the margin
  and the no-arbitrage slopes as transcription checks (CL-2, CL-3).
- **CL-F**'s operative condition is checked (CL-4) with a control confirming
  it is load-bearing (CL-5).
- **CL-D/CL-E**'s direction is checked as the SOSD characterisation on an
  explicit mean-preserving pair (CL-6).
- **CL-I is where the survey errs.** See `NOTES.md`.
- **CL-B, CL-G, CL-H** were confirmed by reading against the source; they are
  textual, not arithmetic, and are unchecked.
- **CL-C** is a positioning instruction, not a claim about the paper.
  `RECONSTRUCTION.md` §7 records the disanalogy it must carry.
