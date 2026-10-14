# TODO, Paper A skeleton

Conventions as in the reference bundle. `[ ]` open, `[~]` in progress, `[x]`
done, `[!]` needs a decision from the author. Each item says what counts as done.

Last updated 14 October 2026.

## PLAN

The author's instruction of 14 October 2026: build a fresh skeleton from what
has been established so far, and go claim by claim.

| Pass | Status |
|---|---|
| 1. Scaffold the skeleton, checker and controls | [x] 14 Oct. Empty tables, appendix stubs, `verify.sh`, MS-1 to MS-6 each with a passing and a failing control |
| 2. Lean project pinned, build roots, audit | [x] 14 Oct. `lean/mathlib/`, Lean `v4.24.0-rc1`, Mathlib `14871d5` |
| 3 and 4. Claims into Lean and model rows, one at a time | [~] M1 to M3 done. Next is Q3 |
| 5. Appendices B to G | [~] filled for M1 to M3, and Appendix D records D1 and D4 |
| 6. Retire `../theory_and_decisions/manuscript.tex` | [ ] |
| 7. Novelty reading | [ ] |
| 8. Conclusions, then introduction headlines | [ ] |
| 9. Literature table | [ ] |
| 10. Readability rounds | [ ] |
| 11. Referee comments | [ ] not applicable unless the paper has reports |

## Claim queue

Taken from the predecessor manuscript. Each becomes a model row only once it is
a Lean theorem with a control. Labels are those of `../theory_and_decisions/manuscript.tex`.

- [x] **Q1, M1. Group sufficiency, both forms.** `ass:sufficiency` states the
  likelihood form and the posterior form as equivalent. Proved over a finite
  class space, with full support needed for one direction
  (`Sufficiency.lean`). This is the substantive step of `prop:coincidence`.
- [x] **Q2, M2 and M3. Bayes and the mixture formula give the same
  posteriors.** `eq:jeffrey`. Under sufficiency the Bayesian posterior on
  class is the within-group distributions mixed by the Bayesian group
  posterior (M2). Every revision of group weights from a full-support prior is
  a Bayesian update for a likelihood over groups (M3). Controls drop
  sufficiency, normalisation and full support (`ClassPosterior.lean`).
- [ ] **Q2b. Salience as a Bayesian group posterior.** The predecessor reads
  the salience `s_g` as Jeffrey's directly given group weight. With Jeffrey
  dropped (D4), `s_g` needs a Bayesian reading, which M3 supplies. Write the
  map from `s_g` to `q'` once the engagement decision enters.
- [ ] **Q3. The composed map.** `eq:composed`. `H(0) = mu` and `H(1) = 1` for
  every salience, given `F(1) = 0`.
- [ ] **Q4. Class closure.** `prop:class-closure`. With `F = (1-c)^2` first,
  then under `ass:Freg`, three fixed points with the low one stable and the
  middle one unstable, for small `gamma` and large `delta`. The precise
  parameter region has to come from the theorem, not from the prose.
- [ ] **Q5. The frozen-belief map has one equilibrium.** With `phi` held at
  `s_g F(beta_0)`, the fixed-point equation is the quadratic
  `gamma c^2 + (mu + delta phi - gamma) c - mu = 0`. Its roots multiply to
  `-mu/gamma < 0`, so it has exactly one positive root, for every `beta_0`. This
  is the rival object that the two-object rule of `notes/from_archive/POSTMORTEM.md`
  asks for, and it carries no trap.
- [x] **Q6. Moot, 14 Oct.** The dynamic half of `prop:coincidence` compared
  Bayes and Jeffrey across generations. With D1 (re-learned prior) and D4
  (Bayes only) there is no second rule to compare.
- [ ] **Q7 onward.** `prop:bifurcation`, `cor:salience-threshold`,
  `prop:enemy`, and the appendix propositions (`prop:passing`, `prop:shadow`,
  `prop:exclusion`, `prop:tokenism`, `prop:spillover`, `prop:loury-nesting`,
  `cor:Tstar`). To be ordered once Q1 to Q6 are in.

## Decisions for the author

- [x] **D1. Is the group prior re-learned each generation? Decided 14 Oct
  2026 by the author: re-learned.** Recorded in Appendix D, "The group prior
  across generations". Two readings exist
  and they give opposite results.
  - Re-learned. The discrimination rate is `phi_t = s_g F(c_bar_t)` with
    `c_bar_t` the group's current standing. This is what the predecessor's
    own equations say (`eq:prior`, the loop of `fig:loop`, `eq:dynamic` and
    `eq:composed`). The map is cubic, and the trap of `prop:class-closure`
    exists under both updating rules.
  - Frozen. `phi = s_g F(beta_0)` for good. This is the reading of
    `verify_bayes_jeffrey.py` and of `notes/from_archive/PAPER_A_DIRECTION.md`.
    The map is quadratic with one equilibrium (Q5), so there is no trap.
  - Recommendation. Re-learned, because it is the model the predecessor
    already writes down and the only one that carries the trap and the
    bifurcation. The frozen map then enters as Q5, a row of its own that shows
    what the trap needs. Under this reading the trap rests on the group prior
    following the group's standing, and on neither updating rule.
- [!] **D2. The research question.** The skeleton's opening sentence recasts
  the first sentence of the predecessor's abstract as a question. Confirm or
  replace it.
- [!] **D3. The title.** "Stereotype Rigidity" names a mechanism that the
  rows so far do not carry. The author agreed on 14 Oct that it may not
  survive. Decide once Q4 and Q5 are in.
- [x] **D4. Drop Jeffrey's rule. Decided 14 Oct 2026 by the author**, on the
  condition that the two rules are shown not to differ here. M2 and M3 show
  it for one reading of a marker under group sufficiency. The skeleton names
  Jeffrey only in Appendix D, "The updating rule", which gives the reason.
  Not covered is a sequence of several soft readings, where order could
  matter. The model has none.

## Novelty ledger

Nothing claimed as new yet. Every source the predecessor cites is unread for
this skeleton (Jeffrey 1965, Diaconis and Zabell 1982, Coate and Loury 1993,
Phelps 1972, Arrow 1973, among others), so every claim about them is
unverified.
