# Claims about Ryvkin & Drugov (2020)

**Paper.** Dmitry Ryvkin, Mikhail Drugov, "The shape of luck and competition in
winner-take-all tournaments", *Theoretical Economics* 15 (2020), pp. 1587–1626.
Evidence level **[F]**, published version.

## Claims as stated in `LITERATURE.tex` §Verdict and the handover

| ID | Claim | Source locator | Relevance claimed to `PROOFS.tex` |
|---|---|---|---|
| RD-A | P-MU is "the exact condition for the sign of the competitor-number effect in a binary-entry contest, as the discrete analogue of the \citet{RyvkinDrugov2020} **hazard-rate result**". | Survey's Verdict item 3 | P-MU (`prop:PMU`) — the third surviving contribution |
| RD-B | "The full read of \citet{RyvkinDrugov2020} identifies the object to compare: their `b_k = E[f(X_(k−1:k−1))]` against our `Δ(0)` as a functional of `Q`." | eqs. (3), (9) | Same |
| RD-C | **Handover next-step 4:** "Relate P-MU's weighting `W` to Ryvkin–Drugov's `b_k` and their **log-supermodularity condition** — a computation, not a read. If the conditions coincide that is a result worth stating." | Prop 1, Cor 1 | Same |

**Status of RD-A (pass 2, 2026-10-06).** The quoted wording is no longer in
`LITERATURE.tex`. Lines 1077–1083 now read "as the discrete analogue of the
\citet{RyvkinDrugov2020} \emph{density} result $b_k = \mathbb{E}[f(X_{(k-1:k-1)})]$
--- \emph{not} their hazard-rate result". The fix proposed in `NOTES.md` pass 1
has been applied there.

## Claims as stated in `PROOFS.tex` and `LITERATURE.tex` (pass 2)

| ID | Claim (verbatim, our documents) | Where in our documents | RD locator |
|---|---|---|---|
| RD-D | "$G^{Q-1}(1-G)$, peaking at $G=(Q-1)/Q$, is exactly the log-supermodular kernel $F^{k-1}(1-F)$, peaking at $F=(k-1)/k$, that \citet{RyvkinDrugov2020} use to drive their comparative statics in the number of players" | `PROOFS.tex` l.1558–1562; `LITERATURE.tex` l.1089–1093 | p.1597 (before Cor 1); p.1610 (App. A.2) |
| RD-E | "The log-supermodularity that drives their argument does carry over to $W$" | `PROOFS.tex` l.1566–1568; `LITERATURE.tex` l.1095–1096 | p.1597; p.1610; p.1615 (proof of Prop 1) |
| RD-F | "Karlin's variation-diminishing step requires the integrand to cross $+-$, whereas $F(f-g) = -F\varphi'$ crosses $-+$. Consequently no unimodality of $\Delta(0,Q)$ in $Q$ is claimed here" and "Reworking the argument for the reversed orientation would plausibly yield an interior \emph{minimum} of $\Delta(0,Q)$ in $Q$" | `PROOFS.tex` l.1568–1571 and l.1825–1829; `LITERATURE.tex` l.1096–1101 | p.1597 (Karlin step); p.1615 (definition of single crossing $+-$) |
| RD-G | "the order-statistic kernel of \citet{RyvkinDrugov2020}, whose message --- that no competitor-number prediction is universal across noise distributions --- is theirs in print" | `PROOFS.tex` l.1719–1722; `LITERATURE.tex` l.700–702 ("the sentence on their p.~1589") | p.1589; eq. (8) p.1596; p.1598; p.1601; p.1603 |
| RD-H | "Proposition~2: symmetric unimodal density gives $e^*_2 = e^*_3$ and decreasing thereafter." | `LITERATURE.tex` l.685–686 | p.1600 |
| RD-I | "Their corresponding object is $b_k = \mathbb{E}[f(X_{(k-1:k-1)})]$, the individual marginal benefit of effort, which is the right comparator for $\Delta(0)$ --- their hazard-rate result concerns \emph{aggregate} effort and a different order statistic." | `PROOFS.tex` l.1563–1566 | eq. (5) p.1594; eq. (9) p.1597; eq. (11) p.1601 |

## What is checkable here

- **RD-B** confirmed: `(3)` and `(9)` agree, and `b_k` is exactly the expected
  noise density at the best rival's shock (RD-1). The worked example is
  reproduced independently from the primitives (RD-2), which is the strongest
  available check on the reading.
- **RD-C is the substantive one and is now computed.** RD-4 shows the
  correspondence is exact rather than analogical; RD-5 shows the
  log-supermodularity condition transfers; **RD-6 shows the second hypothesis
  does not**. See `NOTES.md`. *Pass 2 supersedes the RD-6 reading. The
  single-crossing hypothesis does transfer, in RD's own `+−` orientation, once
  the leading minus sign of the P-MU identity is kept (finding F1 in
  `NOTES.md`, check RD-8, Lean `pmu_orientation`).*
- **RD-A contains a mislabelling.** The hazard rate governs their *aggregate*
  effort result; `b_k`, the right comparator for `Δ(0)`, is a *density*
  object. RD-7 separates the two order statistics involved.
- **RD-D to RD-H** are formalised in `RyvkinDrugov.lean` (pass 2). RD-I is
  covered by the SymPy checks RD-1 and RD-7 and has no Lean counterpart.
