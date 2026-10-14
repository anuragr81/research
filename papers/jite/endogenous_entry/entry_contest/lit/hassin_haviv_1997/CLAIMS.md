# Claims about Hassin and Haviv (1997)

**Source.** Refael Hassin and Moshe Haviv, "Equilibrium Threshold Strategies: The Case of Queues
with Priorities", *Operations Research* 45(6): 966-973 (1997). Read in full from the authors'
preprint (17 pages; uploaded by the author to Drive as `ay.pdf`, 14 October 2026)
[F, preprint version]. Cited in `refs.bib` as `HassinHaviv1997`. Pages are the preprint's.

Every quotation is checked verbatim, on the page named, against the PDF's text layer
(`pdftotext -layout`), ignoring case, punctuation and spaces. The PDF is pinned by sha256. In
the text layer the floor brackets ⌊ ⌋ print as "b" and "c". The results we use are proved in
`lean/mathlib/HassinHaviv.lean` (see `LEAN`).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| HH-1 | 1 | "We provide a detailed analysis of a queueing model in which two priority levels can be purchased." | The question |
| HH-2 | 5 | "Upon arrival, each customer decides whether to purchase high priority, and if so he pays for it an amount θ." | A fixed toll buys priority |
| HH-3 | 5 | "Without loss of generality, we assume that low priority comes for free." | Who does not buy is still served |
| HH-4 | 5 | "The cost of waiting is assumed, without loss of generality, to be one per unit of time." | Identical customers, linear cost; no wealth |
| HH-5 | 4 | "In ATC case there is a single fixed point." | Avoid the crowd gives uniqueness (`atc_unique`) |
| HH-6 | 4 | "The FTC case is more involved. It may have multiple equilibria, of both pure and mixed type." | Follow the crowd gives multiplicity (`ftc_many`) |
| HH-7 | 6 | "Then, k(x), the (integer) optimal threshold for this individual, is non-decreasing in x." | Proposition 3.1: priority purchase is FTC |
| HH-8 | 7 | "The integer threshold n ≥ 1 specifies a Nash equilibrium if and only if" | Proposition 4.1 (`PureEq`) |
| HH-9 | 8 | "Recall that E(x) = B for all 0 < x ≤ 1." | The control `E35_one` |
| HH-10 | 10 | "Hence, the number of pure threshold Nash equilibria is at most b1/(1 − ρ)c." | The stated bound, ⌊1/(1 − ρ)⌋ |
| HH-11 | 10 | "The upper bound on the number of pure Nash equilibrium threshold strategies is now immediate by Proposition 4.1 and the fact that B = 1/(µ(1 − ρ))." | The counting step (`span_le`) |
| HH-12 | 12 | "Moreover, the set of pure equilibria thresholds consists of a set of consecutive integers." | Consecutive equilibria |
| HH-13 | 17 | "We select the time units so that λ + µ = 1." | The units of the recursion in `E35` |

## Readings recorded

| ID | Point | Our reading |
|---|---|---|
| HH-D1 | HH-2 to HH-4 | A fixed price buys standing (an earlier place in service), and those who do not pay are still served. These are properties P1 and P2 of the application search. Customers are identical, so neither wealth nor ability enters, and the equilibrium object is a threshold in the queue length, not a number of buyers out of a fixed pool. |
| HH-D2 | HH-5 to HH-7 | The contrast with our count invariance is the paper's own Section 2. Its priority purchase is follow-the-crowd, and the equilibrium threshold need not be unique. Our gain from paying does not rise with the number who pay (M4), the avoid-the-crowd case, and the count is pinned (M6). `atc_unique` is the shared logic. |
| HH-D3 | HH-10, HH-11 | The counting step gives at most ⌊1/(1 − ρ)⌋ + 1 pure equilibria: thresholds whose `E` lies in a window of length `B` are at most `Bμ = 1/(1 − ρ)` steps apart (`span_le`). The stated ⌊1/(1 − ρ)⌋ fails when `1/(1 − ρ)` is not an integer. At `ρ = 3/5` the appendix recursion, computed exactly, gives three pure equilibria (`n = 5, 6, 7`) for the toll `θ = E(7) − 1/μ` (`three_pure_equilibria`), where ⌊5/2⌋ = 2. The paper's figures use `ρ = 0.8`, where `1/(1 − ρ) = 5` and the bound holds. |
