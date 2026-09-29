# Weisberg, "Commutativity or Holism?": formal record

Companion to `README.md` in this directory (the reading notes, which are
unchanged). Re-read in full on 2026-09-29 from the preprint `weisberg_JCvF.pdf`
(21 pp.; page numbers are the preprint's own).

## Claims formalized

* **Strict Conditionalization** is commutative on propositions (p.7). The
  step of the Strict dilemma on p.8: `p(·|EF) = p(·|E'F)` "can only happen when
  `p(E|E'F) = 1`".
* **Jeffrey Conditionalization** (p.9). It is not commutative on input
  distributions: two updates on `{E, Ē}` with `x` then `y` leave `E` at `y`,
  reversed at `x` (p.9). It is trivially satisfiable on the partition into
  epistemic possibilities (p.10).
* **Field's proposal** (p.11): the input is the Bayes factor
  `β_{q,p}(E:Ē) = (q(E)/q(Ē))/(p(E)/p(Ē))`. `q(E)` is obtained by solving
  `α = β`. The rule is commutative on experiences.
* **Jellybean numbers.** pp.3-4 (Lange's point): `.1 → .8 → .9` vs
  `.1 → .9 → .8`. p.13: `1/10 → 9/10` gives `β = 81`; "`q'(E)` will be `1/10`
  and `r'(E)` will thus have to be `9/10`".
* **Wagner's theorem** (p.12) via the Appendix (pp.18-20): identities (26),
  (27) and conclusions (28)/(3), (29)/(4), under hypothesis (5) (agreement on
  the cells `EᵢFⱼ` only).
* **Rigidity Preserves Independence** (p.16, with the proof in note 12). If the
  transition `p → q` is rigid on `{E, Ē}` and (6) `p(E|F) = p(E)`, then
  `q(E|F) = q(E)`, so (6) and (7) `q(E|F) < q(E)` are incompatible.

## Result

`lean/Weisberg.lean` (symlink to `lean/Literature/Weisberg.lean`; compiles with
`lake env lean Literature/Weisberg.lean`; no `sorry`; axioms
`[propext, Classical.choice, Quot.sound]` for every theorem):

* p.7-8: `cond_cond`, `strict_dilemma`.
* p.9-10: `jeffrey2`, `mass_jeffrey2_E`, `jeffrey2_twice`,
  `jeffrey2_not_comm_inputs`, `jeffrey_finest`.
* p.11: `bf`, `oddsUpdate`, `bf_oddsUpdate`, `oddsUpdate_comm`.
* Jellybean: `lange_jellybean` (pp.3-4: factors `36, 9/4` vs `81, 4/9`; Field
  on the reversed *experiences* returns to `.9`), `jellybean_81` (p.13).
* Appendix/Wagner, in the cell model `Ω = ι × κ`: `appendix26`, `appendix27`,
  `wagner_E` ((3)/(28)), `appendix26F`, `appendix27F`, `wagner_F` ((4)/(29)).
  A worked p.13 instance: `jellybean_wagner` (prior with `p(E) = 1/10`,
  `p(F) = 1/5`, independent; both orders end in the same state;
  `q'(E) = 1/10`; `E`-factor 81 in both orders).
* p.15-16: `Rigid`, `jeffrey2_rigid`, `rpi_qF`,
  `rigidity_preserves_independence`, `no_undercutting`, `rpi_jeffrey2`, all
  on a general finite space with `F` an arbitrary proposition.

`sympy/check_claims.py` (21 checks, exact/symbolic, exit status 0): the pp.3-4
and p.13 numbers, the p.9 single-partition result, Field's factor recovery and
commutativity, the p.13 two-order instance, (26) and (27) symbolically on a
free 2x2 prior with free inputs, (28) when both orders use the same Bayes
factors, Rigidity Preserves Independence on an 8-world space (with a nuisance
coordinate, so `F` is not a union of cells of anything the update is rigid on),
the p.8 claim on an instance, and p.10.

## What formalizing revealed

* **Note 12 skips a step.** The proof reads: "From (6) and the symmetry of
  independence, `p(F|E) = p(F)`. By rigidity then, `q(F|E) = q(F)`." Rigidity
  gives `q(F|E) = p(F|E) = p(F)`. To get `p(F) = q(F)` needs rigidity on **both**
  cells `E` and `Ē` together with independence (`rpi_qF`: a rigid update on
  `{E, Ē}` leaves an independent `F`'s probability unchanged). The theorem is
  correct. It needs `0 < p(E) < 1` and `p(F) > 0` for the conditionals to be
  defined.
* **(26) and (27) use rigidity only.** They hold for every pair of sequences,
  with no commutativity assumption. Commutativity enters only at (28), and there
  only through the cell values `r(EᵢFⱼ) = r'(EᵢFⱼ)`, i.e. hypothesis (5)/(8).
  This formalization assumes every cell has positive prior probability. That is
  stronger than Wagner's overlap conditions (1)-(2), and it makes the identity
  hold for every `j`.
* **The jellybean numbers check exactly.** On pp.3-4 the factors are `36` and
  `9/4` in one order and `81` and `4/9` in the other. Applying the first
  order's *factors* in reverse (`.1 → .2 → .9`) ends at `.9`, which is Lange's
  point in numbers. On p.13 `β = 81`, and the two-order instance satisfies
  everything the text asserts.
* **p.9's non-commutativity is on a single partition.** The later input wins
  (`jeffrey2_twice`). That is the only rule-level example in the paper. The
  correlated-two-partitions mechanism is not in Weisberg.

## Bearing on Paper B

`rigidity_preserves_independence` together with `rpi_qF` is the general-space
form of what Paper B's Proposition IMM uses at `c = 0`. A rigid step on the
`A`-partition leaves an independent `B` independent, with its probability
unchanged. A later rigid step on `B` then cannot move `A`. The two statements
share mathematics but differ in reading. Weisberg's `F` is a *defeater
proposition* discovered later, about the reliability of the experience. Paper
B's second cue is a Jeffrey step on another attribute's partition. The
undercut/rebut vocabulary carries over only by analogy.

`wagner_E`/`wagner_F` are the Bayes-factor identities behind Paper B's benchmark
`P^B`. If both orders end in the same state, each experience carries the same
Bayes factor in either position. That is the commutative horn.

## Audit findings (2026-09-29)

From `verify_hawthorne_weisberg.md` §2 (26 items: 17 verified, 6 with caveat, 3
not checkable). All page references it checked (pp.3-4, 5, 9, 10, 11, 12-13,
16, 17) are correct. The caveats that bear on use:

* **W17 (substantive).** Citing p.9 as the place where Weisberg "sets aside"
  non-commutativity on input distributions, and housing the `ω=1`
  delivered-credence model there, misreads him. He calls that non-commutativity
  *desirable* (p.9), because the same experiences in reversed order *should*
  yield different inputs (Lange, p.3). The `ω=1` model fixes the inputs across
  orders, so its non-commutativity falls on commutativity on experiences, which
  Weisberg keeps as an (occasional, partial) desideratum (p.5).
  `lange_jellybean` makes the point numerically: the reversed-experience
  sequence ends where the original did.
* **W16 (over-attribution).** The rule-vs-inputs locus of order dependence
  (`notes/interior_omega.tex:89-91`) is credited to Weisberg. He separates the
  rule from the inputs problem (p.10). But his only rule-level example is one
  partition, two values (p.9, `jeffrey2_twice`). "Two rigid steps on correlated
  partitions" is not in Weisberg.
* **W24 (analogy).** "A later cue can never undercut, only rebut" carries
  Weisberg's defeater vocabulary over to a second Jeffrey cue. The mathematics
  is shared (see Bearing); keep the mapping marked ◇.
* **W19.** The result is conditional on the defeater being initially
  independent, (6). The formalization also shows it needs `0 < p(E) < 1`,
  `p(F) > 0`, and rigidity on both cells.
* **W22.** Weisberg's Bayes factor is an odds ratio (p.11, `bf`). The project's
  `P^B` is written with NL factors. The two are equivalent for binary
  partitions.
* **W1.** The BJPS 60(4) 2009 793-812 citation cannot be confirmed from the
  preprint (the only trace is "an anonymous referee for BJPS", p.1). Weisberg is
  correctly absent from `bibliography.bib`.
* **W5/W25.** Every Lange claim is secondhand through Weisberg. The text does
  not say whether the jellybean numbers (pp.3-4) are Lange's or Weisberg's.

Cross-reference: Weisberg lists Domotor (1980) and Döring (1999) as part of the
"long history" of the commutativity worry (p.2), which "recent work seems to
have resolved" (Lange 2000; Wagner 2002). See `literature/domotor1980` and
`literature/doring1999`.

## Not formalized

Holism and occasional, partial commutativity on experiences (p.5) are normative
desiderata about experiences, with no formal object to attach to. The
undercut/rebut reading (pp.15-16) is interpretation. The Appendix variant with
time-indexed partitions `E'`, `F'` and hypothesis (9) is, in a cell model, the
proved statement with the cells relabelled.
