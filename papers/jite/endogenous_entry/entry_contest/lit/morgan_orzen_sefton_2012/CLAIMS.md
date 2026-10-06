# Claims about Morgan, Orzen & Sefton (2012)

**Paper.** John Morgan, Henrik Orzen, Martin Sefton, "Endogenous entry in
contests", *Economic Theory* 51 (2012), pp. 435–463. Evidence level **[F]**,
published version.

## Claims as stated in `LITERATURE.tex` §sec:entry

| ID | Claim | Source locator | Relevance claimed to `PROOFS.tex` |
|---|---|---|---|
| MOS-A | p.441: because all players are identical the identity of those opting in is not uniquely determined. | p.441 | The identity problem; hangs P5's contribution |
| MOS-B | They use sequential entry with observability (Proposition 1) and add **private random delays** to make the order well defined. | Prop 1, p.441–442 | One of three literature devices for the identity problem |
| MOS-C | The theory section gives `n* = ⌊√(P/F)⌋` for identical agents with equal endowment `w`, outside option `F` and prize `P`. | §3.1, p.441 | Comparator for `k*` |
| MOS-D | The experiment holds `w = 100` for every subject **by design**. | Table 2, p.445; §4, p.444 | The wealth channel is switched off |
| MOS-E | Predicted entry was 2 (small prize) and 4 (large prize); observed was 2.5 and 3.7. | Table 2, p.445 (predictions); §5.2 text, p.452 (means) | Context |
| MOS-F | Earnings **not** equalised across contest and outside option in either treatment. | §5.2 | Context |
| MOS-G | A self-selection-by-risk-attitude explanation is rejected by comparing three-player contests across treatments. | §5.2.1 | Context |

**Locator correction, 2026-10-06.** MOS-D and MOS-E previously cited "Table 1"
for the design. In the published version Table 1 (pp.439–440) is "Summary of
previous contest experiments", and the design is Table 2 (p.445), "Experimental
design and equilibrium benchmarks". The mean entry figures 2.7, 2.5, 3.6 and
3.7 are printed in the text of p.452. Table 5 on the same page gives the
distribution of the number of entrants in percent, not the means. See
`NOTES.md`, findings F1 and F2.

## Claim as stated in `PROOFS.tex` §Primitives, paragraph "Information."

| ID | Claim | Source locator | Use in `PROOFS.tex` |
|---|---|---|---|
| MOS-H | In the endogenous-entry literature with identical agents "the entrant count is pinned but the identities are not", resolved in this paper "by private random delays". | p.441 (count and identity); Prop 1, p.441–442 (delays) | Contrast with heterogeneous `κ(w_i)` under complete information, where no tie-breaking rule is needed |

## Claims formalised in `MorganOrzenSefton.lean`

Each row is a statement the Lean file proves. The last column names the
analytic content the file takes as a hypothesis or as a definition instead of
proving. Theorem-by-theorem locators and verbatim quotes are in `NOTES.md`.

| ID | Statement proved | Supports | Source locator | Taken as given, not proved |
|---|---|---|---|---|
| MOS-L1 | For the payoff `π_i = w + x_i P/X − x_i`, the investment `x = (n−1)P/n²` solves the symmetric first-order condition `P(n−1)x = (nx)²`, is the only positive solution of that condition, and is a global best response to `n−1` rivals who each invest `x` (for `n ≥ 2`, in cleared form). | MOS-C | §3.1, p.441 | Necessity of the first-order condition at an interior optimum; the cap `x ≤ w` |
| MOS-L2 | At `x = (n−1)P/n²` each contestant's payoff is `π = w + P/n²`. | MOS-C | §3.1, p.441 | Nothing beyond MOS-L1 |
| MOS-L3 | Under `P > F` and `F·N² > P`, a largest integer `n` with `P/n² > F` exists and lies in `[1, N−1]`. When `√(P/F)` is not an integer, that integer equals `⌊√(P/F)⌋`, computed by a floor root over `Nat` whose specification is proved. | MOS-C | §3.1, p.441 | Nothing |
| MOS-L4 | The Table 2 design satisfies `P > F > P/N²` and the genericity condition, gives `⌊√(50/10)⌋ = 2` and `⌊√(200/10)⌋ = 4`, and its twelve printed investments are `(n−1)P/n²` rounded to one decimal. | MOS-C, MOS-D, MOS-E (predictions only) | Table 2, p.445 | Nothing |
| MOS-L5 | In a one-shot entry stage where `m` entrants each receive `w + P/m²` and an outsider receives `w + F`, every pure equilibrium has exactly `⌊√(P/F)⌋` entrants under `P > F > P/N²` and genericity. In the Table 2 design a six-player profile is an equilibrium exactly when it has 4 entrants (large prize) or 2 entrants (small prize). | MOS-H (count pinned) | §3.1, p.441 | The reduction of the paper's continuous-time entry game to a one-shot entry stage (`NOTES.md`, finding F10) |
| MOS-L6 | Whenever `1 ≤ n* < N`, two equilibrium profiles of the entry stage give the first player opposite roles. In the Table 2 design every one of the six players enters in some equilibrium and stays out in another, and 15 of the 64 six-player profiles are equilibria in each treatment. | MOS-A, MOS-H (identity unpinned) | §3.1, p.441 | As MOS-L5 |
| MOS-L7 | Proposition 1's structure. Players who arrive in delay order and enter while fewer than `n*` have entered produce exactly the first `n*` arrivals. The rule "enter while fewer than `n*` have entered" coincides with "enter while `P/(k+1)² > F`" for `k` prior entrants (properties 1 and 2 of the proof). The player with the shortest delay enters and the player with the longest delay stays out, so the entrant set is fixed by the delay realisation and not by any trait of the player. | MOS-B, MOS-H (the resolution) | Prop 1 and its proof, p.442 | That this profile is the unique PBE (property 3 and the no-extra-delay argument) |
| MOS-L8 | Controls. Each control removes one hypothesis of a theorem above and exhibits parameters at which that theorem's conclusion fails. | Non-vacuity, `lit/README.md` rule 3 | n/a | n/a |

## What is checkable here

- **MOS-C** derived from the payoff function rather than assumed: MOS-1
  recovers `x_n* = (n−1)P/n²` and `π_n* = w + P/n²`; MOS-2 gets the `√(P/F)`
  threshold. MOS-L1 to MOS-L3 prove the same steps in Lean, with the
  hypotheses `P > F > P/N²` and genericity explicit.
- **MOS-3** reproduces all twelve entries of their Table 2 (p.445)
  investment column, and **MOS-4** their predicted `n* = 2, 4`. MOS-L4 proves
  both over `Nat` in Lean. This is the strongest available check that §3.1
  was read correctly.
- **MOS-A** made arithmetic in MOS-6 (`C(6,2) = C(6,4) = 15` entrant sets)
  and in MOS-L6, which also proves that every player has both roles across
  the equilibria of the Table 2 design.
- **MOS-B** and **MOS-H** checked against Proposition 1 in MOS-L7. Whether
  "private random delays" describes the mechanism accurately is assessed in
  `NOTES.md`, finding F5.
- **MOS-E** is correct but under-qualified; see `NOTES.md`. MOS-5 records the
  asymmetry the survey omits.
- **MOS-D, MOS-F, MOS-G** confirmed by reading; textual, unchecked.
  None of the statistical analysis was re-run.
