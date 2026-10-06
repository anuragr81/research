# Claims about Lazear & Rosen (1981)

**Paper.** Edward P. Lazear, Sherwin Rosen, "Rank-Order Tournaments as Optimum
Labor Contracts", *JPE* 89(5), 1981, pp. 841–864. Evidence level **[F]**,
published version.

## Claims as stated in `LITERATURE.tex` §Verdict / §Background, in `PROOFS.tex` and in the handover

| ID | Claim | Source locator | Relevance claimed to `PROOFS.tex` |
|---|---|---|---|
| LR-A | §III "Income Distributions" already has agents self-selecting into a tournament by endowed wealth, through DARA. | §III, subsection *Income Distributions*, p.855 | **The novelty threat.** The mechanism has a 1981 precedent |
| LR-B | "the contribution sentence must acknowledge it and name the channel difference (`u''` here, `u'''` there)". | Survey's instruction | Contribution sentence; `PROOFS.tex` §Open items 4 |
| LR-C | Lazear–Rosen §IV handicap algebra is the tool for the open question of an equilibrium in which the incumbent abstains while a challenger invests. | §IV, *Handicap Systems*, eqs. (28)–(32) | Open item 1 (`sec:open`), incumbent entry |
| LR-D | "decreasing absolute risk aversion makes the rich prefer the tournament and the poor prefer the piece rate" (`LITERATURE.tex`, §Background and toolkit) | §III *Income Distributions*, p.855; Table 1, p.854 | Statement of the precedent. Pass 2 finding F2 in `NOTES.md` |
| LR-E | "\S IV (heterogeneous ability, asymmetric information) gives adverse selection into the top league and a competitive handicap $h^* = \Delta\mu/2$" (`LITERATURE.tex`, §Background and toolkit) | §IV, pp.858 and 861–862 | Frames LR-C. Pass 2 finding F4 in `NOTES.md` |
| LR-F | "$A'(w)<0$ has numerator $(u'')^2 - u'''u'$ and so requires $u'''>0$" and quadratic utility "is concave, has $u'''=0$, and exhibits \emph{increasing} absolute risk aversion" (`PROOFS.tex`, §The 1981 precedent) | Our derivation from the DARA of p.854; the paper never mentions $u'''$ | The channel distinction of LR-B |

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

- **LR-6** (pass 2) derives (32) from (29), (30), the p.862 zero-profit
  constraint and (7), and rejects the `V·g·(Δμ/2 − h)` form that pass 1
  carried. The paper prints `γa(h) ≐ V · (Δμ/2 − h)` with no factor `g`.
- **LR-7** (pass 2) recomputes Table 1 from the paper's own approximations
  (16), (17), (24), (25). The μ, μ* and E(U*) columns reproduce and the E(U)
  column does not (finding F3 in `NOTES.md`).

## Lean claim IDs (`LazearRosen.lean`, pass 2)

Each Lean theorem carries one of these IDs. `NOTES.md` lists every theorem
with its locator and the verbatim text it formalises.

| ID | Theorems | Serves | Locator |
|---|---|---|---|
| LR-L1 | `dara_iff_numerator_neg`, `dara_forces_positive_third` | LR-F, LR-B, LR-2 | DARA p.854; `s ≡ −U″/U′` p.852 |
| LR-L2 | `concave_zero_third_is_iara` | LR-F, LR-3 | our claim, `PROOFS.tex` |
| LR-L3 | `positive_marginal_utility_needed`, `strict_concavity_needed_for_iara` | controls for LR-L1 and LR-L2 | none |
| LR-L4 | `burden_falls_of_concave`, `linear_utility_burden_flat` | LR-B, LR-1 | our claim, `PROOFS.tex` |
| LR-L5 | `quad_incr`, `quad_second_and_third_difference`, `quad_strictly_concave`, `quad_burden_step`, `quadratic_separates`, `documents_witness` | LR-F, LR-3 | our claim, `PROOFS.tex`; witness from pass 1 LR-3 |
| LR-L6 | `handicap_zero_sum`, `handicap_gain`, `competitive_handicap`, `competitive_handicap_not_fair` | LR-C, LR-6 | p.862, eqs. (29)–(32) and (7) |
| LR-L7 | `zero_sum_needs_mixed_zero_profit`, `gain_needs_spread_rule` | controls for LR-L6 | none |
| LR-L8 | `hypotheses_satisfiable` | non-vacuity of LR-L1, LR-L5, LR-L6 | none |
| LR-L9 | `table_s_values`, `rich_contest_rows`, `poor_contest_rows`, `sorting_at_unit_variance`, `shared_variances`, `sorting_only_at_unit_variance`, `table_investment_orderings` | LR-A, LR-D, LR-4, LR-5 | Table 1 p.854; text pp.852–855 |
| LR-L10 | `rows_below_variance_threshold` | LR-7 | p.853 variance threshold |

## Note on method

LR-3 reuses the template established by ST-12a in
`../schroyen_treich_2016/`: to test whether a claimed channel distinction is
real, exhibit a single primitive on which one channel operates and the other
does not. Where ST-12a varied `P` holding `A` fixed, LR-3 varies `u'''` holding
concavity fixed.
