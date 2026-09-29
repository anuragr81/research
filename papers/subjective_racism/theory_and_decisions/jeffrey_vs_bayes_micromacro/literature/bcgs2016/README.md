# Bordalo, P., Coffman, K., Gennaioli, N. & Shleifer, A. (2016), "Stereotypes"

*Quarterly Journal of Economics* 131(4), 1753-1794, doi 10.1093/qje/qjw029. (Drive
copy `bcgs2016.pdf` is the **May 2015 working paper**: "First draft, November 2013.
This version, May 2015", 73 PDF pages. All page numbers below are its printed pages;
printed page = PDF page - 1.)

## Claims formalized

* **Definition 1** (p.12): the representativeness of type `t` for group `G` is
  `R(t,G) = Pr(G|T=t)/Pr(-G|T=t)`. By Bayes' rule it increases in the likelihood ratio
  `π_{t,G}/π_{t,-G}` (eq. 2).
* **Definition 2** (p.13): the decision maker recalls the `d` most representative
  types (ties included) and holds the truncated, renormalised distribution on them
  (eq. 3). Footnote 17 (pp.13-14) gives a smooth variant with weights
  `δ(π_{t,G}/π_{t,-G})`. BIR's eq. (30) (their p.80) is the case `δ(x) = x^θ`.
* **Proposition 1** (p.17): when groups share a likelihood ranking, the most
  representative type is modal for at most one group.
* **Proposition 2** (pp.19-20): under MLRP the stereotype is the extreme tail, and
  `E^st(t|G) > E(t|G) > E(t|-G)`. This is the "kernel of truth": the direction is
  right and the difference is exaggerated.
* **Proposition 3** (pp.20-21): symmetric U-shaped (inverse-U) likelihood ratios
  exaggerate (shrink) the variance.
* **Propositions 4(i) and 5** (pp.32-33): stereotype persistence, and the
  over-reaction threshold.
* **Section 4.3** (pp.26-29): eq. (5), Lemma 1, the Irish/Europeans/Scots example, and
  the claim of "an exaggeration of the correlation between education and being on
  welfare" (p.27).

## Result

`lean/BCGS.lean` is a symlink to `lean/Literature/BCGS.lean`. It compiles as a single
file, has no `sorry`, and uses only `[propext, Classical.choice, Quot.sound]`.

* `repr_eq_lr`, `repr_le_iff` -- Definition 1 and eq. (2). `lr_complement` shows the
  ranking for `-G` is the reverse of that for `G`.
* `stereo_sum_one`, `stereo_odds` -- Definition 2: the stereotype is a distribution,
  and odds among recalled types are the true odds (p.14).
* `prop1_i_degenerate` -- the proof of Proposition 1(i).
* `truncation_raises_mean`, `mlr_mean_order`, `prop2_kernel_of_truth` --
  **Proposition 2(i) in general**, for any finite type set.
  - `mlr_mean_order` states MLR in division-free, cross-multiplied form and proves
    MLR ⇒ mean dominance by symmetrising the double sum.
  - `prop2_kernel_of_truth` gives `E(t|-G) ≤ E(t|G) < E^st(t|G)`.
* `prop2_strict_needs_mass` -- a counterexample to the strict inequality when the
  truncated tail carries no mass.
* `prop4_i`, and `prop5_threshold` (the exact over-reaction condition
  `a/A < A_d/(1+A_d+A)`, plus `ν < 1/2`).
* `repr2_factor`, `lemma1_i`, `irish_europeans`, `irish_scots` -- Section 4.3.
* `welfare_ranking`, `welfare_correlation_true`, `welfare_correlation_stereo_d1`,
  `welfare_correlation_stereo_d2`, `welfare_within_group` -- a parametric instance of
  the p.27 claim.
  - Group `G` has `Pr(e=1) = 2/5`, `Pr(w=1|e) = (3/10, 1/10)`.
  - Group `-G` has `Pr(e=1) = 3/5`, `Pr(w=1|e) = (1/5, 1/20)`.
  - The groups are of equal size.

`sympy/check_stereotypes.py` passes 33/33 checks, all in exact rational arithmetic:

* Definition 1;
* the worked examples: Florida (p.4), Republicans/Democrats (pp.45-46),
  Americans/Europeans (p.46), and the medical test (p.25), where Bayes gives
  `P(sick|+) = 18/37` but the `d = 1` stereotype gives 1;
* every experimental design's counts, means and representativeness: Table 2 (p.64),
  the shapes (pp.54-56) and the T-shirts (p.8);
* Proposition 2 on a binomial MLR family for `d = 1, 2, 3, 5`, and the zero-mass
  counterexample;
* Proposition 3 on a symmetric U-shaped case;
* the Proposition 5 algebra;
* the Irish algebra;
* the welfare example, including the within-group computation;
* BIR's eq. (30) as footnote 17's `δ(x) = x^θ`.

## What formalizing revealed

1. **Proposition 2's first strict inequality needs a truncated type with positive
   mass.** Take `π_G = (0,1)`, `π_{-G} = (1/2,1/2)` and `d = 1`. The likelihood ratio
   is strictly increasing, yet `E^st(t|G) = E(t|G)`. The proof (pp.41-42) says "by
   truncating the lower tail, it follows that `E^st(t|G) > E(t|G)`", without this
   proviso. The Lean theorem assumes positive mass in the truncated tail, plus
   distinct type values (the paper's `t₁ < ... < t_N`, p.11).
2. **MLR ⇒ mean dominance needs no division and no FOSD detour.** The proof
   symmetrises `Σ_{t,t'} π_{t,G} π_{t',-G}(x_t - x_{t'})`. It gives the second
   inequality of Proposition 2 as `≥` in general, and as `>` given a strict pair.
3. **Proposition 1(i) has a degenerate exception.** If the groups are identical,
   every type ties, and the modal type is "most representative" for both groups.
   `prop1_i_degenerate` shows this is the only way it can happen.
4. **Proposition 5's threshold is exact.** Over-reaction holds iff
   `α_t/Σ_N α < Σ_d α/(1 + Σ_d α + Σ_N α)`, given that some prior mass is truncated
   (`A_d < A`). If nothing is truncated, the stereotyper *is* the Bayesian, and there
   is no over-reaction.
5. **In the §4.3 instance the correlation is exaggerated in the pooled population
   and removed within each group.**
   - Pooled across the two groups, the education-welfare correlation is
     `corr² = 361/5511 ≈ 0.066` under the true laws. Under the stereotypes it is `1`
     for `d = 1` and `≈ 0.217` for `d = 2`: this is the p.27 claim.
   - Within `G`, the `d = 2` stereotype recalls only welfare types, so its
     education-welfare covariance is `0`, against a true value of `-6/125`. The same
     happens within `-G`.
   - For a 2×2 law the covariance equals the determinant `p₀₀p₁₁ - p₀₁p₁₀`, which is
     Paper B's `assoc`. So in this instance BCGS's exaggeration of association is
     carried by *group membership*, and representativeness-driven truncation
     *removes* within-group association.

## Bearing on Paper B

* **One mechanism.** In BCGS, representativeness *drives* selective recall
  (Definition 1 selects, Definition 2 truncates, "Selective recall is driven by
  representativeness", p.12). The manuscript's "representativeness-distortion or
  selective recall" reads as two alternatives. In this Drive version, updating on new
  information is Bayesian (fn 33, p.31), and no sampling distortion appears anywhere.
* **The association contrast is sharper than the audit suggests, but in a different
  place.** BCGS §4.3 does generate an exaggerated cross-attribute correlation, so
  Paper B cannot claim the *concept* of a distorted believed association as its own.
  In the formal instance, however (point 5), BCGS's exaggeration is between-group:
  the pooled law mixes group stereotypes. Within a group, truncation can remove the
  association. Paper B's `assoc` is defined on one evaluator's joint belief about the
  two attributes, and Paper B's excess association is produced by coherent Jeffrey
  updating on two correlated cues. The honest contrast is therefore one of mechanism
  and of locus:
  - BCGS: recall truncation, a group-level association;
  - Paper B: reading-order kinematics, a within-belief association.
* BIR use BCGS as the foundation of their heuristic type (BIR fn 11, p.20; Appendix
  C). BIR's eq. (30) is the smooth form, which is the published QJE form ("overweighting
  its representative types"). In this Drive version the baseline is hard truncation,
  and the smooth form appears only in footnote 17.

## Not formalized

* Corollary 1 (the power family); Proposition 2(ii), which is symmetric to (i).
* Proposition 3 in general. It is checked on an instance in sympy.
* Proposition 4(ii) (a limit in `n`).
* Lemma 1(ii) in general.
* The likelihood/availability extension (Appendix C, eqs. 8-9).
* The continuous and normal cases (Appendix D, Propositions 6-7).
* The experiments' statistics. Only the designs are checked; the pictures of the ice
  cream cones are not transcribed in the text, so the reported means (3.167 vs 3.125,
  p.10) are not recomputed.

## Audit findings (2026-09-29)

These are from `verify_discrimination_econ.md` §4. The claims are in
`PAPER_B_MANUSCRIPT.tex` line 282 and `notes/paper_review_log.md`. Proposed wording is
text only.

1. **One mechanism, not two** (B2, VERIFIED-WITH-CAVEAT). "Representativeness-distortion
   or selective recall \citep{BCGS2016}" suggests two alternative accounts. BCGS have
   one: recall that is selective *because* it is driven by representativeness (p.12,
   p.14).
   *Proposed:* "instead of stereotypes as representativeness-driven selective recall
   \citep{BCGS2016}".
2. **"Sampling" is not BCGS's** (B3). "Not by a distortion of memory or sampling": the
   memory half is right (p.12, "stored in memory the full conditional distribution
   ... recalling only a limited and selected set of types"). No sampling distortion
   appears in BCGS; their Section 5 updating is Bayesian (fn 33, p.31).
   *Proposed:* "not by a distortion of recall".
3. **§4.3 cross-attribute correlation** (B3, adversarial point). BCGS state that
   stereotypes produce "an exaggeration of the correlation between education and being
   on welfare" (p.27). So a distorted believed cross-attribute association is not
   conceptually foreign to BCGS. The formal instance above qualifies this: the
   exaggeration is of the pooled, group-carried association, and within a group it
   can vanish.
   *Proposed:* "... the sense of stereotype here is a believed cross-attribute
   association within one evaluator's belief, produced by coherent updating on
   impoverished input. BCGS also generate exaggerated associations (their §4.3), but
   through representativeness-driven recall of group types, so that the exaggeration
   runs through group membership."
4. **Version** (B5). The Drive copy is the May 2015 working paper, not the QJE article.
   The QJE bibliographic details (131(4), 1753-1794, doi 10.1093/qje/qjw029) were
   checked externally by the audit. The published abstract's "overweighting" (smooth
   distortion) differs from the working paper's baseline hard truncation.
   "Representativeness-distortion" fits the published version. Pinpoint pages from
   this copy must not be cited as QJE pages.
