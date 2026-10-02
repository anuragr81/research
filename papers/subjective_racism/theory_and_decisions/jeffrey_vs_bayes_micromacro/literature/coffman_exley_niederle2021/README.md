# Coffman, K. B., Exley, C. L. & Niederle, M. (2021), "The Role of Beliefs in Driving Gender Discrimination"

*Management Science* 67(6), 3551-3569. DOI 10.1287/mnsc.2020.3660.

**Source.** The Harvard Business School working paper of 28 February 2020
(35 pp.), the copy the author linked
(`hbs.edu/ris/Publication Files/CoffmanExleyNiederle_8ccfd8f3-...pdf`). Read in
full on 2026-10-02, pp. 1-30; the appendix tables are not in this file. Page
references are to its printed pages. The published version has not been read;
its pages differ.

## Claims formalized

The paper is experimental. Employers choose between a female-even-month and a
male-odd-month worker; the same workers are labelled by gender in one
treatment and by birth month in the other, after both treatments see the same
performance information. Discrimination that the birth-month label also
produces is attributed to beliefs, the remainder under the gender label to
tastes.

* **Belief measures.** Employers "predict the difference in average scores
  between these groups of workers on a 10-question easy quiz and a 10-question
  hard quiz", any integer from -10 to 10, not incentivised (p.8).
  "Differential improvement" is the believed hard-quiz gap minus the believed
  easy-quiz gap (fn 18, p.17).
* **Coding.** A decision is coded 1, 1/2 or 0 as the female-even-month worker is
  hired, chance decides, or the male-odd-month worker is hired (fn 17, p.16);
  absent discrimination the rate is 50% (p.16).
* **Results.** Female workers are hired in 43% of equal-score decisions in the
  Gender treatment and even-month workers in 37% in the Birth Month treatment,
  both below 50% (p.16); Table 1, column 1 puts the difference at 0.061.
  Posterior believed gaps "get the direction of the difference right on
  average, but exaggerate it" (p.14, numbers in fn 13). In-group employers
  believe in smaller gaps (p.23) and hire in-group workers more often (p.22).
* **Classification.** Taste-based (Becker) against statistical (Phelps, Arrow)
  discrimination, the latter widened to "belief-based" discrimination that
  includes inaccurate beliefs (p.2); the design separates beliefs, accurate or
  not, from tastes (pp.3, 15). Belief formation is set aside: "We focus instead
  on decision-making given a certain set of beliefs" (fn 5, p.4).

## Result

`lean/CoffmanExleyNiederle.lean` is a symlink to
`lean/Literature/CoffmanExleyNiederle.lean`; standalone on Mathlib, no `sorry`,
standard axioms only. `sympy/check_coffman_exley_niederle2021.py` checks the
same claims in exact arithmetic. Every reported number checked agrees with the
paper's own statements, and the figures agree across places to rounding:
Fig. 1's easy-quiz gaps (3.96 - 3.27, 5.50 - 4.50) match fn 13's actual easy
gaps (0.692, 1.0), and the two hiring rates match Table 1's coefficient.

## Bearing on Paper B

Cited by plan entry 7.2 (pending) in the conclusion's third implication, as an
existing design that asks a difference question, the believed gap between
groups, and reads a decision rate, who is hired. With a 0/1 score the believed
gap is the conditional difference `P(B=1|A=1) - P(B=1|A=0)`
(`gap_is_conditional_difference`), which Paper B's Proposition PRO places in
the protected class (second order), while the share of decisions changed is
first order (Proposition SHR). The match is in the form of the question only:
group membership is observed here and nothing is read in sequence, so the paper
is not evidence for or against Paper B's mechanism.

## Not formalized

The regressions (Tables 1-3), the Kolmogorov-Smirnov and t-tests, and the
appendix tables.
