# Hawthorne, J. (2004), "Three Models of Sequential Belief Updating on Uncertain Evidence"

*Journal of Philosophical Logic* 33, 89-123 (the PDF footer prints "33: 89-123,
2004"; the issue number (1) is not printed). Page numbers below are the journal
pagination (PDF page n = journal page 88+n).

The most consequential paper for positioning Paper B. It names Paper B's own
updating model: Amnestic Updating, which he identifies with Standard Sequential
Updating, Jeffrey's original extension. He credits its commutation criterion to
Diaconis-Zabell (note 12) and does not name the criterion himself. His medical
example has a hidden hypothesis $C$ and two observable bases $E,F$, each cued
once. It shows the same phenomenon the manuscript studies, and it is richer
than Paper B's 2x2.

Read in full again on 2026-09-30, all 35 pages. Every formula used here was
checked on the rendered page. The text layer drops his $\epsilon$ and his
subscripts. For example, his $r=\mathrm{NL}[Q_{\alpha\epsilon},d,D_i]/\mathrm{NL}[Q_\alpha,d,D_i]$
appears there as a ratio of two identical-looking terms.

## The paper's structure (what is where)

- **Section 2 (p. 93).** Basic Jeffrey updating: rigidity, and $Q_e[S]=\sum_i Q[S\cdot E_i]\,Q_e[E_i]/Q[E_i]$.
- **Section 3 (p. 94).** The Basic Sequential Update Formula. It follows from rigidity alone.
- **Section 4 (p. 95).** Update factors. The Normed-Likelihood (NL) factor is
  $\mathrm{NL}[Q,e,E_i]=Q_e[E_i]/Q[E_i]$. Each kind of factor generates the
  other from the prior (p. 96).
- **Section 5 (pp. 96-99).**
  - The Amnestic Update Model and the **Amnestic Update-Factor Thesis** (p. 96):
    $Q_{\alpha de}[E_i]=Q_{\alpha e}[E_i]$.
  - The car example (p. 97, single basis).
  - The commutation criterion (p. 97, note 12).
  - The **medical example** (pp. 97-98, two bases, one cue each).
  - The two concerns (pp. 98-99). The first begins on p. 98.
- **Section 6 (pp. 99-103).** The NL factor model.
  - The **NL Extended Rigidity Thesis** (p. 100).
  - The NL Extended Sequential Update Formula (p. 102), which has a normalising denominator.
- **Section 7 (pp. 103-107).** The Likelihood-Ratio (LR) factor model.
  - The LR factor is $\mathrm{LR}[Q,\epsilon,E_j,E_k]=\mathrm{NL}_j/\mathrm{NL}_k$ (p. 104).
  - The **LR Extended Rigidity Thesis** (p. 104).
  - The LR Extended Sequential Update Formula (p. 105), which he calls "Field Updating".
  - The **LR medical example** (pp. 106-107).
  - Note 20 records that Jeffrey calls LR factors "Bayes factors", following Good.
- **Section 8 (pp. 107-111).**
  - Order-independence across bases.
  - The **Basis-Overwrite** and **Basis-Commuting** versions of the LR model (p. 108).
  - Reply to Garber: Extended Rigidity may fail within a basis while commutativity holds (pp. 108-111).
- **Section 9 (pp. 111-114).** The **Update Reordering Theorem** (p. 112), with
  compatibility classes (pp. 112-114). The Appendix (pp. 116-120) proves it: the
  Commutation Reduction Theorem, then the Commutation Theorem.
- **Section 10 (pp. 114-116).** Conclusion. He ranks LR above NL, and names
  Basis-Overwrite and Basis-Commuting LR as the most useful normative guides (p. 116).

His taxonomy has **three** models: Amnestic (Section 5), NL (Section 6) and LR
(Section 7). NL is the middle one, and he ranks LR above it (p. 116). **Extended
Rigidity is Sections 6-7** (p. 100 for NL, p. 104 for LR). **Section 8 contains
only the Basis-Overwrite and Basis-Commuting versions.**

## Lean

`lean/Hawthorne.lean` is a symlink to `lean/Literature/Hawthorne.lean`
(namespace `Literature.Hawthorne`, standalone, `import Mathlib`). It works on a
finite space. A basis is a labelling of atoms. It has no `sorry` and adds no
axioms. Every theorem depends only on `[propext, Classical.choice, Quot.sound]`.
The checking command is `lake env lean Literature/Hawthorne.lean`.

| Hawthorne | Lean |
|---|---|
| Basic Jeffrey updating and rigidity (p. 93) | `jeffrey`, `cellMass_jeffrey`, `jeffrey_rigid` |
| $Q_e[E_j]=\mathrm{NL}\cdot Q[E_j]$ (p. 95) | `NL_jeffrey`, `jeffrey_eq_NL_mul` |
| Basic Sequential Update Formula (p. 94) = Bases Decomposition Lemma (1) (p. 118) | `basic_sequential` |
| Amnestic Update-Factor Thesis (p. 96): the latest update on a basis fixes it, whatever came before | `amnestic_basis_value`, `amnestic_thesis` (any intervening transformation `d`), `amnestic_same_basis` |
| Commutation criterion (p. 97, note 12; D-Z Thm 3.2) | `commute_of_jeffreyIndep` ("if", no hypotheses); `jeffreyIndep_of_commute`, `commutation_criterion` ("only if" / iff, arbitrary finite bases, positive targets, **all joint cells positive**) |
| ... and the hypothesis is needed | `criterion_needs_positive_cells` |
| Medical example (pp. 97-98) | `med_Qf_C` 7/50, `med_Qfe_C` 24689/36256, `med_Qe_C` 43/50, `med_Qef_C` 11567/36256, `med_order_effect`, `med_crossBasis` (22/125, 103/125) |
| The overwrite $Q_e[E]=Q_{fe}[E]=.90$ (p. 98) | `med_overwrite` |
| Note 14 (likelihoods .99: .83, .17) | `med_note14` |
| NL and LR factors, LR free of the prior (pp. 95, 103-104) | `NL_factorUpdate`, `LR_factorUpdate`, `LR_ratio` |
| Only ratios matter, so NL and LR induce the same revision on any basis | `factorUpdate_smul` |
| Each factor generates the other (p. 96) | `jeffrey_eq_factorUpdate` |
| Extended Sequential Update Formula, two bases, with denominator (pp. 102, 105) | `factorUpdate_seq`, `factorUpdate_comm` |
| NL Extended Rigidity (p. 100) from state-fixed factors; $r=Z_\alpha/Z_{\alpha d}$ | `extendedRigidity_of_factor` |
| LR Extended Rigidity = NL Extended Rigidity (p. 104) | `erLR_iff_erNL` |
| Extended Rigidity in both orders implies commutation (Sections 6-7), with order-dependent targets allowed | `extendedRigidity_comm` |
| LR example (p. 107): LR .50 and 2 give .35, then .50 in both orders | `med_LR_factor`, `med_LR_Qf_C`, `med_LR_Qfe_Qef` |
| Bayes-factor reading of the .90 reports is 1/9 and 9 | `med_bayesFactorReading`, `med_LR_ninths` |
| NL formula's denominator on the example: 301/625 | `med_NL_denominator`; `nl_denominator_indep` (=1 when the bases are independent under the prior) |
| Extended formula over basis-homogeneous subsequences; suitable reorderings agree (pp. 103, 105) | `extUpdate`, `extUpdate_suitable` |
| Basis-Overwrite Version (p. 108) | `extUpdate_basisOverwrite` |
| Basis-Commuting Version, "completely order-independent" (p. 108) | `extUpdate_basisCommuting` |
| Decomposable (Field) factors: sequential = extended formula, every permutation agrees | `seqFactor_eq`, `seqFactor_perm` |
| Commutation Theorem (p. 118), the two-state core of the Update Reordering Theorem (p. 112), with his $r$ | `commutation_theorem`, `commutation_append` |
| Block-diagonal case: class-specific $r$ (2/5, 14/5); ER fails; commutation holds for every split | `blk_commute`, `blk_r`, `blk_clause2`, `blk_not_extendedRigidity` |

In `commutation_theorem` the two states are arbitrary Basic Jeffrey updates.
Their targets may depend on the order ($Q_{\alpha d}[D_i]$ and
$Q_{\alpha\epsilon d}[D_i]$ can differ). The hypotheses are a nonnegative prior
and strictly positive targets. These give his "plausible principle" (p. 119,
Case 1) for free. Clause (2) is stated as he states it. For each
$Q_{\alpha d\epsilon}$-possible $D_i$,
$r=\mathrm{NL}[Q_{\alpha\epsilon},d,D_i]/\mathrm{NL}[Q_\alpha,d,D_i]>0$, and every
$E_j$ that is $Q_{\alpha d\epsilon}$-compatible with $D_i$ satisfies
$\mathrm{NL}[Q_{\alpha d},\epsilon,E_j]=r\cdot\mathrm{NL}[Q_\alpha,\epsilon,E_j]$.

Not formalized:
- the Commutation Reduction Theorem (p. 117), which reduces arbitrary suitable
  reorderings of long sequences to adjacent swaps; hence the Reordering Theorem
  for sequences of arbitrary length is not formalized;
- countable bases;
- the claim (p. 109) that $n$ identical glances under within-basis Extended
  Rigidity approach certainty;
- the psychological and normative assessments.

## Sympy (both scripts assert; exit 0 iff all pass)

`python3 literature/run_all.py hawthorne` gives 2/2.

- `sympy/check_examples.py` (30 checks):
  - the medical example exactly, both orders;
  - the overwrite $Q_e[E]=Q_{fe}[E]=9/10$;
  - cross-basis influence 22/125 and 103/125;
  - note 14: $1477317/1778144\approx.831$ and $300827/1778144\approx.169$;
  - the LR example: factors .50 and 2 give $Q_f[C]=7/20$ and $Q_{fe}[C]=Q_{ef}[C]=1/2$ on every atom;
  - the Bayes-factor reading of the .90 reports is 1/9 and 9. With those
    factors the result is again 1/2 in both orders;
  - the NL formula's denominator is 301/625. The unnormalised $C$-mass is
    301/1250 and the normalised value is 1/2.
- `sympy/check_reordering_theorem.py` (21 checks). It now computes **Hawthorne's
  $r$**, not $t_j/w_j$. The old script printed $t_j/w_j$, which is
  $\mathrm{NL}[Q_\alpha,\epsilon,E_j]$ and not his $r$. The checks:
  - the solution family ($t_1+t_2=s_1$, $t_3+t_4=s_2$; $t_1$ and $t_3$ free);
  - $r_1=2/5=(w_1+w_2)/s_1$ and $r_2=14/5=(w_3+w_4)/s_2$ for every split;
  - clause (2) on each class, symbolically;
  - Extended Rigidity fails;
  - a non-commuting target violates clause (2);
  - the p. 97 criterion's "only if" fails when joint cells are zero.

## Claims, as the paper makes them

- **Amnestic Update-Factor Thesis** (p. 96): "For any state e that directly
  affects an evidence basis {E_i} and for any other state d and sequence of
  states α, Q_αde[E_i] = Q_αe[E_i]." Then: "Indeed Amnestic Updating is just
  Standard Sequential Updating -- Jeffrey's original approach to sequential
  updating.^10" His gloss is "no trace of the impact of previous experiential
  states remains". "Full adoption" is the project's term, not his.
- **Commutation criterion** (p. 97). Two states commute "just in case neither e
  nor f can, on its own, influence (even indirectly) the basis sentences of the
  other", i.e. $Q_{\beta f}[E_i]=Q_\beta[E_i]$ and $Q_{\beta e}[F_j]=Q_\beta[F_j]$.
  Note 12 credits this to Diaconis-Zabell (1982), Theorem 3.2.
  - **Finding (2026-09-30).** The "only if" half needs every conjunction
    $E_i\cdot F_j$ to have positive prior probability. Neither Hawthorne nor D-Z
    states this hypothesis. D-Z's proof step "choose $A=E_{i_0}F_{j_0}$" divides
    by $P(E_{i_0}F_{j_0})$.
  - Counterexample (`criterion_needs_positive_cells`). A 2-cell basis and a
    4-cell basis with block-diagonal support. The two amnestic updates commute,
    yet each moves the other's basis marginal ($Q[E_1]$: 1/10 to 1/4; $Q[D_1]$:
    3/10 to 3/4).
  - Hawthorne's own Section 9 compatibility classes handle this case.
  - For Paper B's 2x2 with an interior prior every cell is positive, so the
    criterion holds as stated there.
- **Medical example** (Section 5, pp. 97-98). $Q[C]=.5$; $Q[E|C]=Q[F|C]=.95$;
  $Q[E|\sim C]=Q[F|\sim C]=.05$; tests independent given $C$ and given $\sim C$.
  - The reports are $Q_f[\sim F]=.90$ and $Q_e[E]=.90$.
  - Results: $Q_f[C]=.14$, $Q_{fe}[C]=.68$; $Q_e[C]=.86$, $Q_{ef}[C]=.32$.
  - Exact values: 7/50, 24689/36256 ≈ .681, 43/50, 11567/36256 ≈ .319.
  - The overwrite is explicit on two distinct bases: "the physician adopts the
    radiologist's degree of confidence that an image of a mass is present,
    $Q_e[E]=Q_{fe}[E]=.90$" (p. 98).
  - Printing slip: p. 98 prints "$Q[E\cdot F|C]=Q[E|\sim C]\cdot Q[F|\sim C]$"
    where $\sim C$ is meant on the left.
  - Note 14 has a slip, "$Q[\sim F|\sim E]$", where $\sim C$ is meant.
- **LR example** (Section 7, pp. 106-107; not Section 5). It uses the same
  prior. The technician's credence "may well turn out to be .90, just as before",
  but "what the physician wants from the technician is his Likelihood-Ratio
  update factor, e.g., LR[f,F,∼F] = .50". The radiologist supplies LR[e,E,∼E] = 2.
  - Results: $Q_f[C]=.35$ (7/20), then $Q_{fe}[C]=.50$. "This example was
    constructed to make the two update factors counteract each other ...
    whatever values the update factors ... may have, update order will produce
    net no effect: $Q_{fe}[C]=Q_{ef}[C]$."
  - **The reports are not the same as in the amnestic example.** The factors
    .50 and 2 are new inputs. The Bayes-factor reading of the .90 reports
    against the prior 1/2 would be 1/9 and 9. Those factors also cancel, to 1/2
    in both orders, but they are not the numbers Hawthorne uses.
- **Update Reordering Theorem** (Section 9, p. 112). Order-independence of all
  suitable reorderings holds iff clause (2): for each $Q_{\alpha d\epsilon}$-possible $D_i$
  there is an $r=\mathrm{NL}[Q_{\alpha\epsilon},d,D_i]/\mathrm{NL}[Q_\alpha,d,D_i]>0$
  such that every $E_j$ that is $Q_{\alpha d\epsilon}$-compatible with $D_i$ has
  $\mathrm{NL}[Q_{\alpha d},\epsilon,E_j]=r\cdot\mathrm{NL}[Q_\alpha,\epsilon,E_j]$.
  - **$r$ is fixed by $d$'s factors, not free.** He repeats this on p. 113:
    "provided that the constraint r = ... is satisfied".
  - What may differ between compatibility classes is the *value* of this fixed
    ratio.
  - Extended Rigidity is the case in which all classes share one $r$.
  - It follows whenever the classes are linked through a chain of overlapping
    classes (pp. 112-113). It also holds when unlinked classes happen to share $r$.
  - "Classes don't overlap" is therefore too loose. The right condition is "not
    linked by a chain".
  - He calls clause (2) "somewhat weaker" and says it "falls only a little
    short" of Extended Rigidity. The block example shows the gap is real (ER
    fails, commutation holds). The gap needs zero-probability conjunctions.

## Bearing on Paper B

- Hawthorne's medical example is structurally Paper B's setup with a causal
  generative story: a hidden hypothesis drives two conditionally independent
  observables, each with its own basis.
  - Each soft cue targets only its own basis, yet the order effect on $C$ is
    large (the two ".18" swings).
  - This is the amnestic phenomenon without the locality question that
    Döring's disjunctive cues raise.
- The LR companion example shows the factor-model contrast. With factor inputs
  (.50 and 2) the two orders agree exactly. It is not a re-reading of the same
  reports. The Bayes-factor reading of the amnestic reports would be 1/9 and 9,
  which also cancel.
- The Reordering Theorem's extra generality (class-specific $r$) needs zero-mass
  conjunctions of basis sentences. In Paper B's 2x2 with an interior prior
  there is a single compatibility class. So the theorem reduces to NL Extended
  Rigidity, and for amnestic updates to the Diaconis-Zabell criterion ($c=0$).
  - It becomes relevant only if Paper B's scope discussion admits logically
    related attributes: a cue whose basis refines another's, or structural
    zeros in the joint.

## What the manuscript may and may not attribute to Hawthorne

Wording proposals only; the manuscript, plan and dialectic are not edited here.
Cross-references are to `notes/citation_audit.md`.

**May attribute:**
- The name of the model, "Amnestic Update Model", and its identification with
  Standard Sequential Updating (p. 96).
- The Amnestic Update-Factor Thesis (p. 96).
- The attribution of the commutation criterion to D-Z Thm 3.2 (note 12). The
  criterion needs positive joint cells, which Paper B's interior 2x2 satisfies.
- The medical example and its numbers (pp. 97-98). Its overwrite
  $Q_e[E]=Q_{fe}[E]=.90$ on two distinct bases (p. 98).
- The three-model taxonomy: Amnestic, NL, LR (Sections 5-7).
- That factor updates on distinct bases commute (pp. 103, 105, 107-108), and
  that only the Basis-Commuting Version is fully order-free (p. 108).
- The psychological objection (pp. 98-99, 115) and the concession that "there
  may be some specialized systems for which this model is appropriate" (p. 99).
- That he calls LR factors Bayes factors, after Jeffrey and Good (note 20).

**May not attribute:**
- a partial-adoption weight;
- any claim about association or identification;
- a "repair" that removes all order effects;
- the D-Z view that noncommutativity is "not a real problem" (that is D-Z, M24);
- a single-basis illustration as his only example.

**MS ~78-80, footnote (M19).** Current text: "Hawthorne2004 repairs this by
letting a cue multiply the old belief instead of replacing it, and under his
order-free variants the sequence effect disappears." Proposed:

> \citet{Hawthorne2004} offers factor-based alternatives in which a cue supplies
> a normed-likelihood or likelihood-ratio factor instead of a new probability;
> under these, updates on distinct bases commute, and in his Basis-Commuting
> Version (his Section 8) all updates do.

In the main sentence, attribute the defect view to Hawthorne ("very troubling",
p. 99). Do not attribute it to D-Z (M24). Note that he also allows that "each
theory has its uses" (p. 96).

**MS ~97 (M20).** Current text: "Hawthorne's variants leave the association
just as uninformative, because each still revises one attribute at a time".
Hawthorne says nothing about association. Proposed:

> Each factor-based variant in \citet{Hawthorne2004}'s taxonomy multiplies the
> prior by a function of one attribute's cell, so it is a separable reweighting
> and, by Lemma~\ref{lem:SEP}, leaves the association as uninformative as
> amnestic updating does; these variants remove the sequence effect across
> attributes without removing the problem of identifying it.

The separability is visible in `factorUpdate` and `extUpdate`. The conclusion
is the paper's own (Lemma SEP), not his.

**Plan 1.D, ~418-420 (P8).** Current text: "Hawthorne names the premise and
objects that full adoption is implausible, so that the alternative to a weight
of one on the later cue is a weight below one." Proposed:

> \citet{Hawthorne2004} names the premise, the Amnestic Update-Factor Thesis,
> and objects that it is implausible (pp.~98--99); the alternatives he develops
> are factor-based models, not a partial weight. The weight used here is this
> paper's own device. It has two endpoints, ...

**Plan 3.A taxonomy sentences (M21).** "the two ends of his taxonomy" is wrong.
He has three models and NL is the middle one. Proposed replacement for "The
comparison drawn throughout is therefore between the two ends of his taxonomy":

> The comparison drawn throughout is therefore between his amnestic model and
> his factor models.

On the benchmark ("has the form of the extended update formula built on the
second"):

> the benchmark $\PB$ of Section~\ref{sec:jeffrey} has the form of his Extended
> Sequential Update Formula (p.~102), normalising denominator included, with
> each factor taken against the initial prior.

The manuscript's own $\PB\propto P\,\ell^A\ell^B$ carries the $\propto$. The
plan's note "$P^B(i,j)=P(i,j)\,l^A_i\,l^B_j$" drops it. On the medical example
the omitted denominator is 301/625, not 1. It equals 1 only when the two bases
are independent under the prior (`nl_denominator_indep`; $c=0$ in Paper B).

Proposed replacement for the parenthetical ("What Section jeffrey calls a Bayes
factor is his normed-likelihood factor; on a two-element basis the two induce
the same revision"):

> (The factors $\ell$ of Section~\ref{sec:jeffrey} are his normed-likelihood
> factors taken against the prior; since only their ratios matter, they are
> equally his likelihood-ratio factors, which he, following Jeffrey and Good,
> calls Bayes factors (note~20). On a basis of any size the two induce the
> same revision.)

Also, the manuscript's "this ratio multiplies whatever belief it meets" (MS
~115) fits NL factors. A likelihood ratio multiplies odds.

**Plan 6.A (~1283-1285, ~1324-1327) and `papers_dialectic.tex:91-95` (P9, D5).**
Drop the claim that his illustration is "a single basis cued twice" and so
misses a one-cue-per-attribute model. The car example (p. 97) is single-basis.
But the objection (pp. 98-99) follows directly on the two-basis medical
example, where each basis is cued once and the overwrite is explicit. Proposed
for 6.A, replacing "What does not apply is the illustration ... gives each cue
a basis of its own.":

> His own worked example has one cue per basis: in the medical example
> (pp.~97--98) the x-ray report fixes the $E$-marginal,
> $Q_e[E]=Q_{fe}[E]=.90$, whatever the sputum report had implied about it.

The rest of the 6.A reply stands ("the reply offered here is not that the
objection misses but that the weight is recovered ..."). In the dialectic, cite
the objection as pp. 98-99, not p. 99. The dialectic's bibitem note, "both
worked examples and the Update Reordering Theorem verified", should read (D7c):

> both worked examples reproduced exactly, and the two-state Commutation Theorem
> proved on finite spaces in Lean; the reduction to longer sequences is not
> formalized.

## The scope disclaimer (pp. 115-116, Section 10 "Conclusion") -- read 2026-09-22

Verbatim, from the PDF (Drive: `Three_Models_of_Sequential_Belief_Updating_on_Unce.pdf`).
The last clause runs onto p. 116:

> Which extension of Basic Jeffrey Updating is the more plausible model of human
> agents? I'm a logician, not a psychologist. But Amnestic Updating seems
> psychologically less plausible than the more Bayesian approaches, not because it
> is un-Bayesian, but because it seems unlikely that we dismiss previous
> experiences so completely. However, I am mainly interested in whether these
> models capture useful normative conceptions of belief updating, and whether they
> might find useful employment in automated reasoning systems.

The passage makes four moves in three sentences:
1. He poses the descriptive question.
2. He disclaims the standing to answer it.
3. He answers it anyway, by intuition.
4. He rules it out of scope in favour of normative properties.

Paper B uses this passage in Section 3 (plan Change 12, added paragraph). It
says there that the full-adoption premise is contested on psychological grounds
and left unsettled. That is why the paper carries the degree of adoption as a
weight rather than taking a side.

Related passages:
- The objection proper, pp. 98-99, beginning on p. 98: "it seems implausible
  that the most recent experience ... should completely dictate belief strengths
  for basis sentences, with no regard for the import of previous experiences or
  states".
- The car-in-dim-light version, p. 97: "This seems highly implausible".
- The concession, p. 99: "there may be some specialized systems for which this
  model is appropriate". So the premise is contested for human agents, not refuted.
- Note 15 cites D-Z for the ubiquity of the order effect and cites Doring **and
  Lange (2000)** for its implausibility.
- Notes 2 and 10 list Doring among the defenders and supporters of Standard
  Sequential Updating.
