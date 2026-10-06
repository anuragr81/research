# Claims about Lazear & Rosen (1981)

**Paper.** Edward P. Lazear, Sherwin Rosen, "Rank-Order Tournaments as Optimum
Labor Contracts", *JPE* 89(5), 1981, pp. 841–864. Evidence level **[F]**,
published version.

## Claims as stated in `LITERATURE.tex` §Verdict / §Background and in the handover

| ID | Claim | Source locator | Relevance claimed to `PROOFS.tex` |
|---|---|---|---|
| LR-A | §III "Income Distributions" already has agents self-selecting into a tournament by endowed wealth, through DARA. | §III, subsection *Income Distributions*, p.855 | **The novelty threat.** The mechanism has a 1981 precedent |
| LR-B | "the contribution sentence must acknowledge it and name the channel difference (`u''` here, `u'''` there)". | Survey's instruction | Contribution sentence; `PROOFS.tex` §Open items 4 |
| LR-C | Lazear–Rosen §IV handicap algebra is the tool for the open question of an equilibrium in which the incumbent abstains while a challenger invests. | §IV, *Handicap Systems*, eqs. (28)–(32) | Open item 1 (`sec:open`), incumbent entry |

## What is checkable here

- **LR-A** is confirmed textually against p.855, with one material
  qualification recorded in `NOTES.md` — the authors explicitly disclaim
  generality. The utility they use is checked (LR-4) and the table's internal
  consistency and sorting direction verified (LR-5).
- **LR-B is the decisive one and is fully checkable.** LR-1 shows `κ` falls in
  `w` under concavity alone; LR-2 shows DARA is a third-derivative condition;
  **LR-3 exhibits a utility where our channel operates and theirs fails**, so
  the distinction is substantive rather than verbal.
- **LR-C**'s algebra is checked (LR-6): `h* = Δμ/2`, gains zero-sum, `y_a`
  decreasing in `h`. Whether it is "the tool" for endogenising incumbent entry
  is a modelling judgement, not a claim about the paper, and is not checked.

## Note on method

LR-3 reuses the template established by ST-12a in
`../schroyen_treich_2016/`: to test whether a claimed channel distinction is
real, exhibit a single primitive on which one channel operates and the other
does not. Where ST-12a varied `P` holding `A` fixed, LR-3 varies `u'''` holding
concavity fixed.
