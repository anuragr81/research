# Claims about Levin & Smith (1994)

**Paper.** Dan Levin, James L. Smith, "Equilibrium in Auctions with Entry",
*American Economic Review* 84(3), June 1994, pp. 585–599. Evidence level
**[F]**, published version.

Source is a JSTOR scan with **no text layer on pp. 586–599**; it was read
page by page as rendered images. Quotations below were transcribed by eye, so
a second reading is advisable before any of them is quoted in print.

This is the paper the deferred **welfare section** will be built on.

## Claims as stated in `LITERATURE.tex` §sec:entry

| ID | Claim | Source locator |
|---|---|---|
| LS-A | `N` identical risk-neutral potential bidders, fixed entry cost `c` paid ex ante before learning one's value (contrast Samuelson's interim costs), `n` revealed before bidding. | pp. 586–587, Assumptions 1–4, fn. 7 |
| LS-B | Their `n*` is the largest integer at which the marginal entrant's expected gain is non-negative — the same object as `k*` for identical agents. | p. 587 |
| LS-C | The symmetric equilibrium is in mixed strategies with `n ~ Binomial(N, q*)`. | p. 587 |
| LS-D | Footnote 2: of the prior literature's deterministic asymmetric equilibria, the process by which potential bidders divide into entrants and non-entrants is not explained. | p. 586 |
| LS-E | **Proposition 3**: in common-value auctions free entry is excessive by a business-stealing argument (citing Mankiw & Whinston 1986), and the seller taxes entry. | p. 590 |
| LS-F | **Proposition 6, equation (18)**: in IPV auctions free entry is optimal because the marginal entrant's private gain equals the social gain, via `V_n - W_n = n(V_n - V_{n-1})`. | pp. 592–593 |
| LS-G | **Propositions 8–9**: welfare falls monotonically as the pool `N` grows beyond `n*`, through the coordination cost of mixed entry. | pp. 594–595 |
| LS-H | **Equation (9)**, `(1-q)^{N-1} V = c`, is the "win only if alone" reservation condition and coincides with Fu–Jiao–Lu's Definition 1. | p. 590 |
| LS-I | A winner-take-all contest with a fixed prize is common-value-like in the relevant sense, so the natural welfare benchmark is Prop 3 rather than Prop 6, and the expected verdict is excessive entry. | Survey's inference |

## What is checkable here

- **LS-F** is fully checkable: eq. (18) from the order statistics (LS-1) and
  the private/social coincidence it delivers (LS-2).
- **LS-H** is checkable and is **where the survey goes wrong**. See
  `NOTES.md` — eq. (9) is the social planner's first-order condition, not a
  reservation condition. LS-3 and LS-6 establish this.
- **LS-E**'s formal content — what "excessive" means — is checked as
  concavity of `S` with the equilibrium above its peak (LS-4).
- **LS-G**'s Prop 8 algebra is checked (LS-5).
- **LS-A, LS-B, LS-C, LS-D** were confirmed by reading but are structural or
  textual, not arithmetic; unchecked.
- **LS-I** is the survey's own modelling judgement about the entry_contest
  model, not a claim about the paper. Not checkable here.

## No Lean

Nothing in this paper is discrete or order-theoretic in the way
`Dispersive.lean` or `Accounting.lean` are. Per `lit/README.md`, absence is
the default.
