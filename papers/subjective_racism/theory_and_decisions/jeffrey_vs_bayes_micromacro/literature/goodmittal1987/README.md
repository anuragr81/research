# Good, I. J. & Mittal, Y. (1987), "The Amalgamation and Geometry of Two-by-Two Contingency Tables"

*Annals of Statistics* 15(2), 694-711. Read in full, every page, from the scanned
original (`goodmittal1987_1.pdf`, pp. 694-711, including the Appendix and references).
The file `goodmittal1987.pdf` in the source folder is a different paper: I. J. Good
(1960), "Weight of Evidence, Corroboration, Explanatory Power, Information and the
Utility of Experiments", *JRSS B* 22(2), 319-331. It is mislabelled and should not
be used to check claims about Good & Mittal.

## Claims formalized

Setup (p. 694): a table `a = [a, b; c, d]` with rows `T, T̄` (treatment and
non-treatment), columns `S, S̄` (success and failure), and `abcd ≠ 0`. The tables
`a_i` of `n` disjoint subpopulations are *amalgamated* by addition into
`A = [Σa_i, Σb_i; Σc_i, Σd_i]`.

* **Definition 1.1** (p. 695). The amalgamation paradox occurs if
  `max_i α(a_i) < α(A)` or `α(A) < min_i α(a_i)`. It explicitly includes Yule's
  case: `α(a_i) = 0` for all `i` but `α(A) ≠ 0`. That case is why the authors
  reject the name "reversal paradox".
* **Definitions 2.1-2.2** (p. 696). A design is row-uniform if
  `(a_i+b_i)/(c_i+d_i) = λ` for all `i`, and column-uniform if
  `(a_i+c_i)/(b_i+d_i) = μ` for all `i`.
* **Measures** (Section 3, pp. 698-700):
  * `π_R = a/(a+b) - c/(c+d)`
  * `π_C`
  * Yule's `y = (ad-bc)/N²`
  * the odds ratio `κ = ad/(bc)`
  * `W_R = log[a(c+d)/(c(a+b))]`
  * `Q_R = log[d(a+b)/(b(c+d))]`
* **Lemma 4.1** (p. 701). The mediant lies between the two ratios.
* **Theorem 4.1 and Corollary 4.1** (p. 701). Under a row-uniform design,
  `α(A) = Σ (N_i/N) α(a_i)` for `α = π_R, y`, so the paradox cannot occur. The
  same holds for `π_C` (and `y`) under a column-uniform design.
* **Theorem 4.2** (p. 702). Under a row-uniform design,
  `exp α(A) = Σ δ_i exp α(a_i)` for `α = Q_R, W_R`, so there is no paradox.
* **Theorem 4.3** (pp. 702-703). If the design is both row- and column-uniform,
  there is no paradox for `κ`. The **Note** on p. 702 shows that row-uniformity
  alone is not enough.
* **Theorem 5.1** (pp. 703-704). Homogeneity for `κ` holds iff (5.2) or (5.3).
* **The remark on p. 696.** A drug can look beneficial for men and for women but
  harmful overall, and "this can happen even though `N_i ∝ p_i`".

## Result

`lean/GoodMittal.lean` is a symlink to `lean/Literature/GoodMittal.lean`. It
compiles with `lake env lean`, has no `sorry`, and its headline theorems use only
the axioms `[propext, Classical.choice, Quot.sound]`.

* `Paradox`, `paradox_iff_max_min`: Definition 1.1, including the paper's
  max/min wording.
* `mediant_between`: Lemma 4.1.
* `piR_amalg_general`: an identity with **no design assumption**. `π_R(A)` equals
  the treated-row success rates averaged with weights `a_i+b_i`, minus the
  untreated-row rates averaged with weights `c_i+d_i`. This is the paper's
  (5.16)-(5.17) for `n` tables.
* `piR_weighted_rowUniform`, `yule_weighted_rowUniform`, `no_paradox_piR`,
  `no_paradox_yule`: **Theorem 4.1**, for general `n`.
* `no_paradox_piC`: **Corollary 4.1**, proved by transposition.
* `expQR_weighted_rowUniform`, `expWR_weighted_rowUniform`, `no_paradox_QR`,
  `no_paradox_WR`: **Theorem 4.2**. The weights are `δ_i = b_i/Σb` and
  `c_i/Σc`, as on p. 702.
* `kappa_between_of_uniform`: **Theorem 4.3** for `n = 2`.
* `note_p702_kappa_paradox`: the paper's **Note**.
  * `a_1 = [3,1;1,9]` and `a_2 = [889,203;381,2349]` are row-uniform with `λ = 2/5`.
  * `κ(a_1) = κ(a_2) = 27`.
  * `κ(A) = 87639/3247 ≈ 26.9908 < 27`.
* `equalSize_reversal`, `equalSize_not_rowUniform`: the p. 696 remark as an
  explicit table.
  * The two subpopulations have equal size (`N_1 = N_2 = 100`).
  * `π_R(a_1) = 2/15 > 0` and `π_R(a_2) = 11/90 > 0`, but `π_R(A) = -33/100`.
  * The design is not row-uniform: the row ratios are 1/9 and 9.
* `yule_case_paradox`: Yule's case.
  * `a_1 = [9,9;9,9]` and `a_2 = [4,8;8,16]` both have `π_R = 0` and `κ = 1`.
  * The aggregate has `π_R(A) = 1/35` and `κ(A) = 325/289`.
  * An association **appears** where there was none.
* `kappa_homogeneous_iff`: **Theorem 5.1**, both directions, via the paper's
  factorisation `(b_1d_2 - b_2d_1)(c_1d_2 - c_2d_1) = 0`.

`sympy/check_amalgamation.py` runs 27/27 checks in exact rationals and exits 0.

* The Note's numbers.
* Theorems 4.1 and 4.2, checked symbolically for two row-uniform tables.
* The structure theorem behind 4.3, checked symbolically (see below), plus 58
  rational instances.
* The factorisation (5.4).
* The equal-size reversal and the Yule case.
* The general row-weight identity.

## What formalizing revealed

**1. The paradox comes from the two rows weighting the subpopulations
differently, not from the population shares.** `piR_amalg_general` shows that
with no design assumption, the aggregate contrast compares two weighted averages
with different weights. The treated row weights subpopulation `i` by its share of
the treated, and the untreated row by its share of the untreated.

In the equal-size example the population shares are (1/2, 1/2). The treated-row
weights are (1/10, 9/10) and the untreated-row weights are (9/10, 1/10). The
reversal comes entirely from that mismatch. Row-uniformity is exactly the
condition that makes both sets of row weights equal to `N_i/N`, and then
Theorem 4.1 turns the aggregate into a convex average.

So "how the subpopulations are weighted" is the right place to look, but the
relevant weights are the **row-specific** ones. The shares `N_i/N` are not the
source. The paper states this directly: it happens "even though `N_i ∝ p_i`"
(p. 696). In the paper, `N_i ∝ p_i` is a scaling convention adopted on p. 695.

**2. Whether a design protects against the paradox depends on the measure.** A
row-uniform design protects `π_R`, `y`, `W_R` and `Q_R`, but not `κ` (the Note).
`κ` needs both uniformities. The Note's paradox is also tiny: 26.991 against 27.
It is a slight dilution, not an erasure or a reversal. Definition 1.1 has no
threshold for size or sign.

**3. Theorem 4.3 has a shorter proof than the paper's four-case argument.** Row-
and column-uniformity together force `N_1/N_2 = ρ` to scale every row and column
total. Therefore

`a_1 = ρa_2 + t, b_1 = ρb_2 - t, c_1 = ρc_2 - t, d_1 = ρd_2 + t`.

The sympy script checks symbolically that every uniform pair has this form. The
sign of the single scalar `t` then orders both `a/c` and `d/b` in the same
direction, which is the paper's concluding step on p. 703. The Lean proof uses
this route.

**4. Yule's case (a null association in every subpopulation) is a paradox of the
same standing.** It is formalized with an explicit table.

## Bearing on Paper B

Paper B (MS:285-288) cites Good & Mittal as a contrast.

* **G&M:** aggregating subpopulations moves an association measure outside the
  range of its subpopulation values.
* **Paper B:** within a single population, a first-order share of affected
  individuals yields only a second-order aggregate loss.

The contrast in kind is sound. The two phenomena also differ in a way the
manuscript could exploit: G&M's paradox is a **design artefact**. It disappears
under a uniform design (Theorems 4.1-4.3), where aggregation is a convex average
and nothing is erased. Whether anything analogous to a balanced design would
remove Paper B's order gap is a question for the manuscript's own theorems. This
formalization does not check it; the manuscript should claim it only if it holds.

The manuscript's "the same kind of erasure" is therefore a loose analogy. In G&M
the "erasure" is the aggregate leaving the subpopulation interval. In Paper B the
aggregate loss is an average of individually small (near-threshold) losses; see
the FGT README for the `P_1 = H·I` reading of `L(c)`.

## Not formalized

* Theorems 5.2-5.6: homogeneity for `W`, `Q` and `π`, and the three-measure
  corollary.
* Theorem 4.3 for general `n`. The paper amalgamates two tables and then adds one
  at a time; the partial amalgams stay row- and column-uniform, so the `n = 2`
  result iterates. This was omitted for length.
* The Appendix's approximate-fairness Theorems A.1-A.2 (a derivative and
  perturbation argument).
* Section 3's scale-invariance characterisation of `κ` (3.7).

## Audit findings (2026-09-29)

**How the manuscript uses the paper.** MS:285-288:

> "the amalgamation paradox \citep{GoodMittal1987}, where a real effect present
> in every subpopulation is erased or reversed by a confound in how the
> subpopulations are weighted together. Whereas the amalgamation paradox erases
> a real effect by combining subpopulations under confounded weights, …"

The bibliography entry is `bibliography.bib:208-217`.

**What the audit found** (`verify_scanned.md`, G1-G4), with what the
formalization adds:

* **G1: "a real effect present in every subpopulation is erased or reversed"**
  (VERIFIED-WITH-CAVEAT).
  * This is narrower than Definition 1.1, which only requires the aggregate
    measure to fall outside `[min, max]` of the subpopulation measures.
  * `yule_case_paradox` shows that Yule's case, which G&M make central, has **no**
    effect in any subpopulation and an effect in the aggregate. Nothing is erased
    or reversed.
  * `note_p702_kappa_paradox` shows a paradox in which the aggregate is merely
    slightly below a common subpopulation value (26.991 against 27).
  * Erasure and reversal are special cases, not the definition. **Confirmed.**
* **G2: "a confound in how the subpopulations are weighted" / "confounded
  weights"** (VERIFIED-WITH-CAVEAT).
  * The formalization **partly qualifies** the audit here. The audit says the
    mechanism is "not the weights on the subpopulations themselves".
    `piR_amalg_general` shows it *is* a weighting of subpopulations: the two rows
    use different subpopulation weights (`a_i+b_i` against `c_i+d_i`).
  * The audit is right on the substance. The weights at fault are not the
    population weights `N_i/N`. G&M rule those out as the cause (p. 696), and
    `equalSize_reversal` has equal `N_i`.
  * The cure is a design in which treatment is allocated in the same proportion
    in every subpopulation (Definition 2.1; Theorem 4.1).
  * In the causal-inference sense, subpopulation membership confounds the
    treatment comparison because it is associated with treatment. So "confound"
    is defensible, but G&M never use the word, and "confounded weights" invites
    the population-share reading that G&M reject.
* **G3: bibliographic data** (VERIFIED for authors, title, journal and pages
  694-711). The scan does not show volume, issue or DOI. The received and revised
  dates (1985/1986) are consistent with 15(2), 1987.
* **G4: file identity.** `goodmittal1987.pdf` is Good (1960). **Confirmed.**

**Proposed corrected wording for MS:285-288** (not applied):

> It is worth contrasting the phenomenon explored in the current paper against
> the amalgamation paradox \citep{GoodMittal1987}, in which a measure of
> association computed on the pooled table falls outside the range of its values
> in the subpopulations: an association present in every subpopulation can vanish
> or reverse on pooling, and one absent from every subpopulation can appear.
> Good and Mittal trace the paradox to the design rather than to the population
> shares: when treatment is not allocated in the same proportion in every
> subpopulation, the treated and untreated groups weight the subpopulations
> differently, and the pooled comparison can mislead even when each subpopulation
> enters in proportion to its size; under a balanced (row-uniform) design pooling
> is a weighted average and the paradox cannot occur. Whereas the amalgamation
> paradox is thus an artefact of unbalanced pooling across subpopulations, our
> decision-space result shows that a comparable disconnect between individual and
> aggregate effects arises within a single population: a first-order share of individuals who are individually affected
> contributes only a second-order loss in aggregate (Theorem~\ref{thm:LOS}).

A shorter alternative, if the paragraph must stay two sentences, is to replace
"a real effect present in every subpopulation is erased or reversed by a
confound in how the subpopulations are weighted together" with "an association
measured in every subpopulation can be erased, reversed or even created by
pooling them, when treatment is unevenly allocated across the subpopulations".
Then replace "under confounded weights" with "with unevenly allocated
treatment".
