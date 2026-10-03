# Tversky, A. & Kahneman, D. (1992), "Advances in Prospect Theory: Cumulative Representation of Uncertainty"

*Journal of Risk and Uncertainty* 5, 297-323. DOI 10.1007/BF00122574.

**Source.** The journal PDF (27 pp.), downloaded 2026-10-03 from
`cemi.ehess.fr/docannexe/file/2780/tversjy_kahneman_advances.pdf` (the server's
certificate chain is incomplete, so the file was fetched without TLS
verification; its title page and running heads confirm it is the article,
pp. 297-323). Read in full on 2026-10-03. Page references are journal pages.
Not in the Drive folder.

## Claims formalized

* **The objects and the rule.** "Prospect theory distinguishes two phases in
  the choice process: framing and valuation ... The valuation process
  discussed in subsequent sections is applied to framed prospects" (p.299). A
  prospect maps states to outcomes; "we arrange the outcomes of each prospect in
  increasing order" (p.300), and the decision weight of a gain is "the
  difference between the capacities of the events 'the outcome is at least as
  good as x_i' and 'the outcome is strictly better than x_i'" (p.301). For a
  risky prospect, `π_i⁺ = w⁺(p_i + … + p_n) - w⁺(p_{i+1} + … + p_n)` (p.301).
* "For both positive and negative prospects, the decision weights add to 1.
  For mixed prospects, however, the sum can be either smaller or greater than
  1, because the decision weights for gains and for losses are defined by
  separate capacities" (p.301). "If each W is additive ... then π_i is simply
  the probability of A_i" (p.301). The die example (p.301).
* The weighting function (6), `w(p) = p^γ / (p^γ + (1-p)^γ)^{1/γ}` (p.309),
  and the median estimates: value exponent 0.88, λ = 2.25, γ = 0.61, δ = 0.69
  (pp.311-312), with "γ < δ" (p.312). The parameters were estimated
  "separately for each subject" (p.311).
* Table 6, the test of loss aversion (p.312).

## Result

`lean/TverskyKahneman.lean` is a symlink to `lean/Literature/TverskyKahneman.lean`;
standalone on Mathlib, no `sorry`, standard axioms only.
`sympy/check_tversky_kahneman1992.py` repeats the checks symbolically or
exactly, and evaluates the weighting function at the median estimates:
`w⁺(.5) ≈ 0.421` and `w⁻(.5) ≈ 0.454`, both below .5 as p.312 reports for every
subject. All checks pass.

## What formalizing revealed

* **Table 6's printed definition has the opposite sign.** The note defines
  `θ = (x - b)/(c - a)`; with the tabulated `a, b, c, x` that gives the
  negatives of all eight tabulated values (problem 1: `-61/25` against 2.44).
  The tabulated θ is `(x - b)/(a - c)`, the ratio of slopes the text describes.
  A typesetting slip; nothing in the paper depends on the sign.
* **No updating anywhere.** Read in full, the paper contains no rule for
  revising probabilities in the light of evidence: probabilities (risk) or
  capacities (uncertainty) enter as given, and the only ordering in the
  representation is the ranking of outcomes by value.

## Bearing on Paper B

Proposed for citation (plan, pending) in a sentence that sets CPT's rank
dependence against the paper's sequence dependence: in CPT the decision weight
of an outcome depends on its rank among the outcomes of a given prospect; in
Paper B belief depends on the sequence in which two cues are read, through the
believed link between the traits. The weighting function is a property of the
evaluator (estimated subject by subject), where under Jeffrey conditioning with
full adoption no weight of the evaluator's enters.

## Not formalized

The axiomatic analysis (Theorems 1 and 2); the experimental tables other than
Table 6; Figures 1-4.
