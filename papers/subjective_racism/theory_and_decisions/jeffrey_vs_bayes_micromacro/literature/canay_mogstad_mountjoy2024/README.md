# Canay, I. A., Mogstad, M. & Mountjoy, J. (2024), "On the Use of Outcome Tests for Detecting Bias in Decision Making"

*Review of Economic Studies* 91(4), 2135-2167. DOI 10.1093/restud/rdad082.

**Source.** NBER Working Paper 27802, revised 10 June 2023, with its
supplemental appendix. Sections 2-4 read in full on 2026-10-03, and Appendix A
through Definitions A.1-A.3 and the proof of Theorem 4.1, cases (i) and (ii).
Page references `p.N` are printed main-text pages (the PDF page minus two);
`A-p.N` are the appendix's own pages. The published version has not been read;
its pages differ. Not in the Drive folder.

## Claims formalized

* **The decision model** (Definition 2.1, p.10). A judge releases a defendant
  of race `r` and non-race characteristics `v` iff `E[Δ|r,v] ≤ τ(z,r,v)`, the
  Generalized Roy Model (GRM), with `τ = c + λ + β` the perceived benefit of
  release (detention cost, prediction or measurement error, taste for
  discrimination). The Extended Roy Model (ERM, Definition 2.2, p.12) restricts
  `τ(z,r,v) = τ(z,r)`.
* **The outcome test** (Section 4.1, p.19). With `v*_{z,r}` the marginal
  defendant, `E[Δ|r,v*] = τ(z,r,v*)` (eq. 12, assumed unique), the test reads
  `E[Δ|w,v*_w] - E[Δ|b,v*_b]` (eq. 15) and infers bias against black defendants
  if it is positive, none if it is zero.
* **Definitions 3.2 and 3.4** (pp.15, 17): τ-unbiased, locally and globally
  τ-biased against either race, and unclassified.
* **Theorem 4.1** (pp.20-21): in the GRM, for each of the four cases there are
  functions, even in the continuous monotone class `F^cm(V)`, for which the
  difference (15) "could be positive, negative, or zero". The paper's gloss:
  "the marginal outcome test may conclude bias even if the judge is racially
  unbiased. Second, the outcome test may conclude no bias even if the judge is
  locally or globally racially biased" (p.21).
* **Theorem 4.2** (p.24): in the ERM the difference equals `τ(z,w) - τ(z,b)`,
  so the test is logically valid.

## Result

`lean/CanayMogstadMountjoy.lean` is a symlink to
`lean/Literature/CanayMogstadMountjoy.lean`; standalone on Mathlib, no `sorry`,
standard axioms only.

* Theorem 4.2 holds in general (`thm42`, `thm42_gt_iff`, `thm42_eq_iff`): it is
  the definition of the marginal defendant applied twice.
* Theorem 4.1 holds by explicit witnesses on `V = ℝ`, range `I = ℝ`, in
  `F^cm(ℝ)` (continuous, weakly monotone, a crossing, condition 5 for the white
  benefit function; condition 3 is vacuous for `I = ℝ`), with a unique marginal
  for each race. In every case (unbiased; globally biased against black,
  against white; locally biased against black, against white; unclassified)
  the difference (15) takes any prescribed real value `t`
  (`thm41_case_i` ... `thm41_case_iv`). The witnesses use costs `k_r - v` and
  benefits `v`, `v + 1`, `2v` and the kinked `v + max(0,-v)/2`. They are
  simpler than the appendix's construction, which builds the cost function
  around an arbitrary given benefit function; they prove the same statement
  for this class.
* `biased_judge_passes`: a judge globally τ-biased against black defendants
  whose marginal white and black defendants have equal outcomes, the form
  plan entry 7.2 cites. `unbiased_judge_fails` is the converse.

`sympy/check_canay_mogstad_mountjoy2024.py` recomputes each witness's crossings
(and their uniqueness), the bias properties and the outcome difference (33/33).

## What formalizing revealed

Theorem 4.1 for the class `F` is, in the authors' words, "trivial, as these are
two different functions evaluated at two different points" (A-p.2); its content
is that the failure survives monotonicity and continuity. The witnesses show
how little is needed: a benefit function that varies with `v` at different
rates for the two races (case iv), or a constant gap read at different points
of differently placed cost curves (case ii), already makes the difference any
number.

## Bearing on Paper B

Quoted in the Grounds of plan entry 7.2 (pending) as the economics precedent
for the form of its last sentence: an audit can be silent about what it is
meant to detect, here a belief audit that asks a difference question and
cannot rule out a role for the reading sequence. The parallel is in the form
of the claim, an existence statement within an admissible class of models, not
in the mechanism: their silence comes from benefits that vary with
unobserved characteristics, Paper B's from a statistic whose differential at
independence annihilates the route plane (Proposition PRO). The manuscript
does not cite the paper; if 7.2 or Section 3's identification paragraph comes
to cite it, this record covers what would be cited.

## Not formalized

Theorem 4.1 for the larger classes `F`, `F^m(V')`, `F^cm(V')` with `V' ⊊ V`;
Remark 4.1 (β-bias); Section 5 (econometric viability).
