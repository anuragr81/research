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

## What is checkable here

- **RD-B** confirmed: `(3)` and `(9)` agree, and `b_k` is exactly the expected
  noise density at the best rival's shock (RD-1). The worked example is
  reproduced independently from the primitives (RD-2), which is the strongest
  available check on the reading.
- **RD-C is the substantive one and is now computed.** RD-4 shows the
  correspondence is exact rather than analogical; RD-5 shows the
  log-supermodularity condition transfers; **RD-6 shows the second hypothesis
  does not**. See `NOTES.md`.
- **RD-A contains a mislabelling.** The hazard rate governs their *aggregate*
  effort result; `b_k`, the right comparator for `Δ(0)`, is a *density*
  object. RD-7 separates the two order statistics involved.
