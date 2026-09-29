# Döring, F. (1999), "Why Bayesian Psychology Is Incomplete"

*Philosophy of Science* 66 (Proceedings), S379-S389. Read in full (all 11
pages, visually; the scan has no text layer) on 2026-09-29.

## Claims formalized

The paper's mathematics is small and all of it is formalized:

* **Jeffrey's rule** (S381, displayed), with **rigidity** ("all probability
  ratios within each region ... be left intact", S381-S382), coincidence with
  classical conditioning when a cell gets probability 1 (S381), and
  **reversibility** while no cell is popped (S380, S382).
* **Classical conditioning is order-independent** (S384, displayed:
  `p_{yz}(x) = p(x|yz) = p_{zy}(x)`).
* **The worked example** (S382-S383, Figure 1). Prior on the 2x2 table over
  `A, B`: `p(AB) = p(¬AB) = .05`, `p(A¬B) = p(¬A¬B) = .45`. The cues are the
  two-cell partitions `{A∨B, ¬A¬B}` and `{¬A∨B, A¬B}`, each raised to `.99`.
  Each cue singles out one joint cell; neither is a partition by `A` or by `B`.
  Text claims: `A¬B` is "one fifth" of `¬A¬B` after sequence 1, and the reverse
  after sequence 2; `P(A|¬B)` is "1/6" vs "5/6"; "by playing with the numbers,
  this discrepancy can be brought as close to 1 as you please"; a third step
  lowering `P(B)` "would force the unconditional probabilities for A and ¬A
  near 0 and 1, with the roles of A and ¬A reversed" (S383).
* **The remedy** (S384-S385, Figure 2): revert to the prior and Jeffrey-update
  once on `{A¬B, ¬A¬B, B}` with `.01, .01, .98`, giving `49, 49, 1, 1`.
* **Dempster's rule** (S385-S386): orthogonal sum
  `m(¬A¬B) = m(A¬B) = .01·.99/(1-.01²)`, `m(B) = .99²/(1-.01²)`, then Jeffrey;
  "to within 1/1000 of a percentage point" of Figure 2.
* **Field's rule** (S386): input is the odds factor `α = o_new(e)/o_old(e)`, a
  Bayes factor; it is commutative.
* **The dampened variant** (S387-S388, Figure 3): evidence posteriors as
  weighted averages of prior and experience (`w = 0.1` and `w = 0.5`).

## Result

`lean/Doring.lean` (symlink to `lean/Literature/Doring.lean`; compiles with
`lake env lean Literature/Doring.lean`; no `sorry`; every theorem's axioms are
`[propext, Classical.choice, Quot.sound]`):

* General rule: `cellMass_jeffrey`, `jeffrey_rigid`, `jeffrey_certain`,
  `jeffrey_reversible`, `jeffrey_same_partition`, `cond_comm`.
* Field: `field_comm` (any two partitions), `cellMass_fieldUpdate`,
  `field_odds` (the factor multiplies the odds).
* Figure 1: `fig1_step1`, `fig1_step1'`, `fig1_seq1`, `fig1_seq2` (all four
  tables, exact); `order_effect_condA_notB` (`19/118` vs `99/118`),
  `condA_notB_seq1_approx`, `ratio_seq1` (`19/99`), `order_effect_margA`
  (`91/190` vs `99/190`), `jeffrey_not_comm`, `third_step`.
* "As close to 1 as you please": a family `fam n` containing the paper's prior
  (`fam_nine_halves`: it is `fam (9/2)`), closed forms `fam_seq1`, `fam_seq2`,
  the gap `fam_gap` (`1 - 4n/(n²+2n-1)`), and `gap_tends_to_one`
  (for every `δ > 0` the gap exceeds `1 - δ`).
* Figure 2 and Dempster: `fig2`, `dempster_masses`, `dempster_result`,
  `dempster_gap`.
* Figure 3: `fig3_row1`, `fig3_row2` (exact).

`sympy/check_examples.py` (28 checks, exact rationals, exit status 0): every
table entry of Figures 1-3 exactly and against the printed percentages, the S382
display, the S383 ratios and conditionals, the third step, the family gap and
its limit, Figure 2, the Dempster masses and gap, and Field's commutativity and
odds-factor reading on Döring's own cues (symbolic prior and factors).

## What formalizing revealed

* **The printed numbers are roundings, and two of them are off.** The exact
  Figure 1 values are `891/1900, 891/1900, 1/100, 99/1900` (printed
  `47, 47, 1, 5`). `P(A|¬B)` is exactly `19/118 ≈ .161` and `99/118 ≈ .839`
  ("1/6", "5/6"); the "one fifth" is `19/99`. These are fair roundings.
  Two are not quite right:
  * S386 says the Dempster route matches Figure 2 "to within 1/1000 of a
    percentage point". The exact gap is `1/10100` in every cell, about
    **1/100** of a percentage point (`dempster_gap`).
  * Figure 3, second row, prints `42.2` for `¬A¬B`. The exact value
    `391/925 = 42.27%` rounds to `42.3`. The other printed Figure 3 entries
    round correctly.

  Neither slip affects the argument.
* **With the paper's own numbers, the third step does not reach "near 0 and
  1".** Lowering `P(B)` to `.01` moves `P(A)` to `97/590 ≈ .164` vs
  `493/590 ≈ .836`, i.e. to about `1/6` vs `5/6` (`third_step`). The "near 0
  and 1" rhetoric needs the "playing with the numbers" limit, which is real:
  `gap_tends_to_one`. The reversal of roles is exact.
* **The marginal of `A` already differs** between the orders before any third
  step: `91/190` vs `99/190` (about 48% vs 52%).
* **Döring's remedy is one Jeffrey update on the original prior** (`fig2`).
  What Jeffrey's rule cannot supply is the incremental route to it, as §5 says.
* **Figure 3's two rows are the same order with different weights**
  (`w = 0.1` and `w = 0.5`), not the two orders. The order effect there rests
  on the "mirror images" remark (S388), which by symmetry of the prior is the
  same computation with `A` and `¬A` swapped.

## Bearing on Paper B

Döring's example is a 2x2 joint, but his cues are **not attribute-local**.
Each cue is a two-cell partition isolating one joint cell (`¬A¬B`, then `A¬B`).
Paper B's cues are Jeffrey steps on the partition by `A` and the partition by
`B`. So Döring's tables are not an instance of Paper B's model, and his numbers
cannot be quoted as Paper B's order effect. What carries over is the general
fact: rigid propagation on overlapping partitions, with a correlated prior (here
the prior is heavily weighted to `¬B`), gives order effects visible in cells,
conditionals and marginals.

`field_comm` is the Döring-side statement of the benchmark `P^B`: odds-factor
inputs commute on any two partitions.

## Audit findings (2026-09-29)

From `verify_scanned.md` §1, with this reading's confirmations:

* **The objection is normative, not psychological** (D12, CONTRADICTED as the
  project uses it). S379: "an exercise in Bayesian *rational* psychology"; the
  contention is that Jeffrey conditionalization "cannot be a complete account
  of *rational* belief change". S383: the order dependence "seems wholly
  unjustified". S386: "Jeffrey conditionalization alone cannot be all there is
  to rational belief change". Döring never claims that people do not update this
  way. `notes/papers_dialectic.tex` should not present him as the sharpest
  statement of a realism attack.
* **"An adjustment that Jeffrey's rule cannot supply" overstates him** (D4).
  His remedy (S384-S385) *is* a one-step Jeffrey update on the prior (`fig2`).
  The shortfall is incremental assimilation (§5, S388-S389). Use his own words,
  "cannot be understood as an assimilation of incoming evidence by Jeffrey's
  rule" (S379).
* **He goes beyond cells** (D8). He reads the effect in `P(A|¬B)` and, with a
  third step, in the unconditional `P(A)` (S383). The claim that the step "from
  his tables to observable statistics is the step this paper supplies" is safe
  only if narrowed to *population* statistics: his example is one agent (the
  investigators). This reading adds that, with his own numbers, the marginal
  effect of the third step is `.164` vs `.836`, not near 0 and 1.
* **Cues are disjunctive, not attribute-local** (D7, D9); see Bearing above.
* **This directory now exists** (D6: previously no `literature/doring*`). The
  plan's "formalized in `literature/`" is now true for Döring.
* Bibliographic line verified from the S379 footer: *Philosophy of Science*
  66 (Proceedings), pp. S379-S389, 1999 (D11). `Doring1999` is not yet in
  `bibliography.bib`.
* Jeffrey (1988) is in his reference list (S389), cited at S384 for Skyrms's
  embedding of Jeffrey updating in classical conditionalization, not for a
  commutativity result (D10).

## Not formalized

The philosophical theses (incompleteness; the belief-relativity of
"informativeness" against Field, S386-S387; the call for non-incremental
schemes, S388-S389). Skyrms's embedding (S384), which Döring cites and does
not prove. Dempster's rule in general (only the displayed orthogonal sum). The
plane-crash story (S383-S384) is the same numbers with an interpretation.
