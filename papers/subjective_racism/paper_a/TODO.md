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
| 3 and 4. Claims into Lean and model rows, one at a time | [~] M1 done. Next is the claim queue below, in order |
| 5. Appendices B to G | [~] stubs filled for M1 |
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
- [ ] **Q2. The two rules share the second step.** `eq:jeffrey`. Under
  sufficiency, Bayes and Jeffrey both reach `P'(c) = sum_k F_gk P'(g_k)`, and
  they differ only in where `P'(g_k)` comes from. Done when it is a theorem
  with a control that drops sufficiency.
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
- [ ] **Q6. The dynamic half of `prop:coincidence`.** Which object Bayes and
  Jeffrey share across generations depends on decision D1.
- [ ] **Q7 onward.** `prop:bifurcation`, `cor:salience-threshold`,
  `prop:enemy`, and the appendix propositions (`prop:passing`, `prop:shadow`,
  `prop:exclusion`, `prop:tokenism`, `prop:spillover`, `prop:loury-nesting`,
  `cor:Tstar`). To be ordered once Q1 to Q6 are in.

## Decisions for the author

- [!] **D1. Is the group prior re-learned each generation?** Two readings exist
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
- [!] **D3. The title.** "Stereotype Rigidity" names a mechanism that M1 and
  Q6 may show is not what carries the trap. Keep or change, once Q4 to Q6 are in.

## Novelty ledger

Nothing claimed as new yet. Every source the predecessor cites is unread for
this skeleton (Jeffrey 1965, Diaconis and Zabell 1982, Coate and Loury 1993,
Phelps 1972, Arrow 1973, among others), so every claim about them is
unverified.
