# Notes — Becker, Murphy and Werning (2005), pass 1 (manuscript read in full)

Run `python3 lit/becker_murphy_werning_2005/verify_bmw.py` from the bundle
root. It checks the pinned sha256 of the PDF, the first page, that every block
ends with its page number, each quotation verbatim on its page, two controls,
and measures `lean/Lit/BeckerMurphyWerning2005.lean` (build root, build, no
`sorry`, every cited name declared, every declared theorem audited, allowed
axioms only, controls present).

## What was verified

Our reading of §2, of the indifference and convex-kink steps of §4, and of the
power-law example is consistent with the manuscript:

- Equal marginal utility across lottery prizes gives winners more consumption
  under complementarity, the same under separability, less under substitutes.
- Without lotteries, a compensating-differential price schedule gives the
  indirect utility a convex kink at the common income.
- With linear indirect utility on the support, every fair lottery there is a
  matter of indifference.
- The power-law ∗-allocation: constant marginal utility, the resource
  constraint (by integration in Lean), the price slope as the marginal rate of
  substitution (by `HasDerivAt`), `y∗ = (κω̄/α)s^δ`, and its inverse.

## What could not be verified

Proposition 1, the single-crossing step, the mean-preserving-spread step of
Proposition 2, Proposition 4, and the rank derivative (7). The JPE version was
not read; the three slips on p.23 (BMW-D1 to D3) may be corrected there.

## Discrepancies and readings

BMW-D1 to BMW-D3 in `CLAIMS.md`, all on p.23 and all in the illustrative
power-law example: the exponent on the constant of `F∗`, the support of `y∗`,
and "δ tends to zero" for "1/δ tends to zero". None affects a proposition.

## Findings for the firmworkers ledger

1. **A second route to gambling, and a different one.** Robson gets the
   Friedman–Savage shape from convexity in status rank; BMW get gambling from
   complementarity, `u_cs > 0`, with utility concave in income for given status
   and status bought in a market or assigned by rank. Neither uses a reference
   income. A reference set by the own group's income norm, as decided for the
   sub-section on 12 Oct 2026, is a third mechanism; the novelty check must keep
   these apart.
2. **Endowed status removes the lottery motive in BMW.** BMW-20 and BMW-21:
   where family background, race or religion fix status, status is endowed, not
   won, and measured income reflects consumption alone. The firmworkers `h` is
   exactly endowed status (decision of 11 Oct 2026). In BMW's terms the
   firmworkers lottery cannot be a lottery for status; it is a lottery for
   positions on the ladder, with `h` fixed. That is a clean point of
   difference, and it means BMW's mechanism cannot be borrowed for the
   sub-section without changing what `h` is.
3. **Fair, continuum lotteries.** BMW's lotteries are fair and their law of
   large numbers keeps average income fixed (BMW-12). The firmworkers lottery is
   negative-sum net of entry costs (M12) among a finite pool. BMW themselves
   call negative-sum lotteries the inferior way to gamble when productive risks
   exist (BMW-22); that is a reason a negative-sum swap lottery needs the
   displacement of M32 to be chosen at all.
4. **Rank externality.** With status as rank, a winner lowers others' ranks
   (BMW-24), as in the firmworkers swap, where a winner's gain is a partner's
   loss. BMW conclude the market then departs from the planner (BMW-25).

## Controls

| Check | Control |
|---|---|
| Quotations | BMW-C1, BMW-10 with "substitutes" replaced by "complements", absent; BMW-C2, BMW-21 looked up on p.26, absent |
| Quotation suite | mutation-tested 12 Oct 2026: an altered word (BMW-10), a wrong page (BMW-21 on p.26), an undeclared Lean name and a wrong sha256 pin each fail |
| Lean | four `control_*` theorems: separability gives no extra consumption to winners; no rising price and no complementarity, no kink; the printed `F∗` misses 1 at the top of the support; `δ = 1` gives a flat density |

## Relevance to the firmworkers ledger

Row L17. No model row rests on BMW yet. It bears on the novelty of the
reference-dependent sub-section and on the reading of `h` as endowed status.
