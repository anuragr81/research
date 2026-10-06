# Claims about Levin & Smith (1994)

**Paper.** Dan Levin, James L. Smith, "Equilibrium in Auctions with Entry",
*American Economic Review* 84(3), June 1994, pp. 585–599. Evidence level
**[F]**, published version.

**Source.** Drive file `entry.pdf` (id `1O_ZYYtuS1c5FiM7CJg8yhE0PFafIY1A7`),
a JSTOR scan identified by its title page. The PDF's own text layer covers the
JSTOR cover sheet only, so none of pp. 585–599 carries one. Drive returns an
OCR of every page, in which the prose is legible and the displayed equations
are garbled. On 2026-10-06 all fifteen content pages were rendered as images
and read in full, and every quotation used in this directory was re-checked
against the images (TODO S2). No page was unreadable. The re-check table is in
`NOTES.md`, pass 2.

This is the paper the deferred **welfare section** will be built on.

## Claims as stated in `LITERATURE.tex` §sec:entry

| ID | Claim | Source locator |
|---|---|---|
| LS-A | `N` identical risk-neutral potential bidders, fixed entry cost `c` paid ex ante before learning one's value (contrast Samuelson's interim costs), `n` revealed before bidding. Also line 565–567, Samuelson 1985 as the interim-cost variant Levin–Smith position against. | p. 586, Assumptions 1–4 and fn. 3; p. 587, Assumption 5, the definition of `E[π|n,m]` and fn. 7 |
| LS-B | Their `n*` is the largest integer at which the marginal entrant's expected gain is non-negative, the same object as `k*` for identical agents. | p. 587 |
| LS-C | The symmetric equilibrium is in mixed strategies with `n ~ Binomial(N, q*)`. | p. 585; p. 587 and fn. 6 |
| LS-D | On p. 586 (main text, with footnote 2 attached to the preceding sentence) the paper says of the prior literature's deterministic asymmetric equilibria that the process by which potential bidders divide into entrants and non-entrants is not explained. Levin and Smith restore symmetry by mixed entry, so the entrant count becomes stochastic. | p. 586 |
| LS-E | **Proposition 3.** In common-value auctions free entry is excessive by a business-stealing argument (citing Mankiw & Whinston 1986), and the seller taxes entry. | p. 590 |
| LS-F | **Proposition 6, equation (18).** In IPV auctions free entry is optimal because the marginal entrant's private gain equals the social gain, via `V_n - W_n = n(V_n - V_{n-1})`. | pp. 592–593 |
| LS-G | **Proposition 9.** Welfare falls monotonically as the pool `N` grows beyond `n*`, through the coordination cost of mixed entry; **Proposition 8** is the supporting statement that the probability of no entry rises with `N` under an optimal mechanism. | p. 594 (Prop 8), p. 595 (proof of Prop 8, Prop 9) |
| LS-H | **Equation (9)**, `(1-q)^{N-1} V = c`, is not an entry-equilibrium condition. It is the first-order condition of the welfare problem (8), characterises the socially optimal entry probability that the optimal fee `e*` is chosen to induce, and is written `(1-q^s_N)^{N-1} = c/V` in the proof of Prop 8. The bidders' equilibrium condition is (6). It is algebraically identical to Fu–Jiao–Lu's Definition 1, which reaches the same equation as a lower bound on equilibrium entry. (An earlier survey version called (9) the "win only if alone" reservation condition; that role error is recorded in `NOTES.md`, pass 1.) | p. 588 (6); pp. 589–590 (8), (9); p. 595 |
| LS-I | Proposition 6 holds exactly when (18) holds. With a prize fixed at `V`, `V_n ≡ V`, so the social gain from any entrant beyond the first is zero while the cost is `c > 0` (p. 596 quoted). A fixed-prize contest is therefore in the common-value branch, the benchmark is Prop 3, and only a prize that grew with participation could reinstate Prop 6. Also line 548–552. | p. 596; the branch reading is the survey's inference |
| LS-L | Line 521 says that in Moreno–Wooders revenue may rise or fall in `N`, "unlike Levin–Smith". | p. 595, Corollary to Prop 9 |

## Claims as stated in `PROOFS.tex`

| ID | Claim | Source locator |
|---|---|---|
| LS-J | Lines 569–575 say that identical-agent endogenous entry has the entrant count pinned but the identities not, resolved by mixed entry in Levin–Smith. | p. 586; p. 587 fn. 6 |
| LS-K | Lines 954–958 say that in a fixed-prize contest entry is rent-dissipating and the business-stealing logic of Levin–Smith applies. | p. 590 |

## What is checkable here

- **LS-F** is checked in SymPy (LS-1, LS-2) and in Lean, where fn. 16 gives
  (18), (18) is the private-equals-social identity, and the (19) step "vanishes
  since `V_0 = 0`" is proved.
- **LS-H** is checked in SymPy (LS-3, LS-6) and in Lean, which states the
  planner reading and the reservation reading of (9) as two propositions about
  one expression and proves that the free-entry equilibrium condition is a
  third.
- **LS-E** is checked in SymPy (LS-4) and in Lean, which derives `e*` in (11)
  as the rent term of entrants who meet rivals and proves `q^s < q*` at free
  entry.
- **LS-I** and **LS-K** are checked in SymPy (LS-7, LS-8) and in Lean, which
  computes the failure of (18) under a fixed prize as exactly `V - W_n` and
  proves that the weighted failures sum to the business-stealing term.
- **LS-B, LS-C, LS-D, LS-J** have discrete content that Lean checks
  (uniqueness of `n*`, the binomial identities, count pinning and identity
  freedom of pure equilibria). Their interpretive part, that the prior
  literature leaves the division into entrants unexplained and that Levin and
  Smith answer it by mixing, is verified by quotation (`NOTES.md` pass 2, Q1
  to Q6).
- **LS-G** is checked numerically (LS-5); Lean checks only the algebraic step
  of the Prop 8 proof.
- **LS-A** and **LS-L** are claims about interpretation, not mathematics.
  They are verified by quotation only (`NOTES.md` pass 2, Q23 to Q26), and
  no Lean theorem stands behind them.
- **LS-E**'s attribution of the business-stealing reading and **LS-I**'s
  reading of a fixed-prize contest as common-value-like are interpretation.
  The first is verified by quotation (Q16). The second is the survey's own
  inference, and what Lean checks is the criterion beneath it.

## Lean

`LevinSmith.lean`, 55 theorems, core Lean 4, run and axiom-audited by
`verify_ls.py` (LS-L). The table maps claims to theorems. Locators and
verbatim quotations per theorem are in `NOTES.md`, pass 2.

| Claim | Theorems |
|---|---|
| LS-B | `cutoff_unique`, `cutoff_exists` |
| LS-C | `binom_zero_right`, `binom_row_five`, `binom_zero_of_lt`, `binom_absorption`, `symmetric_root_unique` |
| LS-D, LS-J | `pure_count_pinned`, `pure_identity_free`, `identities_not_pinned`, `control_knife_edge_count` |
| LS-E | `sumTo_split`, `sumFrom2_congr`, `sumFrom2_nonneg`, `sumFrom2_pos`, `sumFrom2_zero_fn`, `cv_payoff_split`, `stealing_nonneg`, `stealing_pos`, `stealing_zero_of_full_extraction`, `free_entry_slope_neg`, `optimal_fee_eq_stealing`, `optimal_fee_pos`, `free_entry_exceeds_planner`, `control_needs_concavity`, `witness_ordering` |
| LS-F | `binom_ratio`, `sumTo_congr`, `sumTo_zero`, `sumTo_shift`, `eq19_vanishes`, `fn16_gives_eq18`, `eq18_iff_private_eq_social`, `eq18_everywhere_zero_wedge`, `ipv_free_entry_optimal` |
| LS-G | `prop8_no_entry` |
| LS-H | `planner_foc_iff_eq9`, `reservation_iff_eq9`, `free_entry_not_eq9`, `reservation_bound_below_equilibrium`, `one_root_two_roles`, `control_no_stealing`, `seller_revenue_is_welfare`, `prop1_fee_attains_planner` |
| LS-I, LS-K | `fixed_prize_eq18_gap`, `fixed_prize_eq18_iff`, `fixed_prize_eq18_fails`, `control_fixed_prize_full_extraction`, `fixed_prize_eq18_at_one`, `fixed_prize_social_gain`, `fixed_prize_private_exceeds_social`, `fixed_prize_wedge_is_stealing`, `cv_free_entry_excessive`, `witness_cv_chain`, `control_wedge_zero_without_eq18` |

The pass-1 note here said nothing in the paper is discrete enough for Lean.
That was true of the analytic results, and it is still true that Lean
reproves none of them. What Lean does check is the role structure (which
proposition each displayed equation is), the finite-sum algebra between (2),
(6), (9), (10), (11), (18) and (19), and the combinatorial identities behind
the binomial weights.
