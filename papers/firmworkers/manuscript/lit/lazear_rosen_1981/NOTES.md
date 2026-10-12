# Notes — Lazear and Rosen (1981), pass 1 (JPE article read in full)

Run `python3 lit/lazear_rosen_1981/verify_lr.py` from the bundle root. It checks
the pinned sha256 of the PDF, the first page, the page numbers, each quotation
on its page with all spaces removed (the text layer drops spaces between
words), two controls, and measures `lean/Lit/LazearRosen1981.lean` (build root,
build, no `sorry`, every cited name declared, every declared theorem audited,
allowed axioms only, controls present).

## What was verified

Our reading of the risk-neutral tournament (prizes, fair gamble, equivalence
with the piece rate, normal spread), of the variance and investment comparisons
under risk aversion, of the utility `U = a y^a` and Table 1's comparisons, of
the revenue-function argument for adverse selection, of the efficiency
condition for mixed contests and of the handicap algebra is consistent with the
source.

## What could not be verified

The existence of a pure-strategy equilibrium, the derivation of the
approximations (16), (17), (24), (25), the CARA claim of p.854, the numbers in
Table 1 and the linearisation behind (32). The entry_contest record of the same
paper reports, from a SymPy and mpmath recomputation, that Table 1's E(U)
column does not follow from its own formulas; that check is not admissible
here (Lean only) and is not adopted. LR-D3 points the same way and is proved.

## Discrepancies and readings

LR-D1 to LR-D5 in `CLAIMS.md`. LR-D3 is a finding about the source: under its
own second-order approximations, with one `s` for both schemes, the piece rate
is ahead at every `s` and `σ²`, so the contest-preferred rows of Table 1, on
which the wealth sorting rests, need more than the stated approximations.

## Findings for the firmworkers ledger

1. **Wealth sorting into a rank-order gamble has a 1981 precedent, in the
   opposite direction.** LR suggest that richer workers choose contests and
   poorer ones piece rates (LR-24, LR-26, LR-27). The authors call it an example
   (LR-24), it rests on declining absolute risk aversion (LR-18, LR-19), and the
   paper's own approximations do not produce it at a common `s` (LR-D3). In
   firmworkers, M34 has the lottery worth less to a richer displaced entrant for
   every nondecreasing `u`, through exclusion from the own draw, of order
   `1/(|P| − 1)`; the source's S-shaped claim has low-rank workers preferring
   the lottery. Direction, channel and object all differ: poorer rather than
   richer workers take the gamble; no curvature rather than DARA; a lottery over
   other workers' positions rather than a pay scheme that induces investment.
   The novelty statement should cite LR as the earliest wealth sorting between a
   rank-order gamble and a safe alternative, and state these three differences.
2. **Gambles without a convex region.** LR: "workers in this model are risk
   averse for all values of incomes, but even so gambles can be the optimal
   policy" (LR-31). M33 has the same feature for a different reason (the
   lottery dominates the fallback `x_0` in the first-order sense). Neither needs
   the convex region the source's appendix invokes.
3. **Levels for entry, spreads for effort.** LR-5: levels of prizes govern
   entry and the spread governs investment. In firmworkers the prize is a
   position, a level, and `L` the entry cost; no spread-driven investment
   exists in the lottery.
4. **Segregated contests.** With private abilities, low types enter high
   leagues (LR-33, LR-35), mixed contests are inefficient unless types are half
   and half (LR-36, `mixed_efficient_iff_half`), and sorting needs credentials
   or nonprice rationing (LR-34, LR-37). Firmworkers' closure `σ` blocks
   cross-group draws by `h`, which is non-productive standing (decision of 11
   Oct 2026), while LR's rationing is by productivity. LR are the
   productivity-based alternative explanation of segregated contests that the
   closure claims (M29) must be distinguished from.
5. **Friedman (1953) is a direct novelty threat for the reference-dependent
   sub-section.** LR say it is almost the only earlier work on lotteries in
   occupations (LR-8): Friedman–Savage utility yields a two-class distribution,
   those in the risk-preferring region "buy lottery tickets in very risky
   occupations" (LR-29), and the assignment "really follows endowed wealth"
   (LR-30). That is close to the source's appendix (S-shaped utility, lottery
   for positions, sorting by rank). Friedman, M. (1953), "Choice, Chance, and
   the Personal Distribution of Income", *JPE* 61(4), 277–290 is not on Drive
   (searched by title and full text on 12 Oct 2026) and is not read.

## Controls

| Check | Control |
|---|---|
| Quotations | LR-C1, LR-26 with wealth replaced by ability, absent; LR-C2, LR-26 looked up on p.854, absent |
| Quotation suite | mutation-tested 12 Oct 2026: an altered word (LR-26), a wrong page (LR-35 on p.858), an undeclared Lean name and a wrong sha256 pin each fail |
| Lean | two `control_*` theorems: without the slope bound on `G` the low league can pay more than the high one; without mixed zero profit the handicap gains do not sum to zero |

## Relevance to the firmworkers ledger

Row L27. No model row rests on LR. It is a novelty record for M33, M34, M29 and
the reference-dependent sub-section, and it points to Friedman (1953).
