# Zhao, J. & Osherson, D. (2010), "Updating beliefs in light of uncertain evidence: Descriptive assessment of Jeffrey's rule"

*Thinking & Reasoning* 16(4), 288-307. DOI 10.1080/13546783.2010.521695.

**Source.** `zhao_osherson2010.pdf` is the publisher's PDF: a cover sheet
followed by journal pp. 288-307. Read in full on 2026-09-30. Tables 1-3 were
checked against the text layer, which keeps their columns intact. In the text
layer, "§" appears as "x" and overbars (negation) are lost. The negations were
restored from context: each pair of questions on p.293 is "blue" and "purple",
or "giraffe" and "hippo".

## Claims formalized

The paper is empirical. Its formal content is the account of Jeffrey's rule
that it tests (pp.288-291):

* **Eq. (1) simple updating.** `Pr₂(A) = Pr₁(A | B)` when `B` becomes certain.
* **Eq. (2).** The law of total probability for `Pr₂(G)` over `{B, B̄}`.
* **Eq. (3) invariance for G, B.** `Pr₂(G|B) = Pr₁(G|B)` and
  `Pr₂(G|B̄) = Pr₁(G|B̄)`. ZO introduce it with "invariance is said to hold
  (Jeffrey, 2004, §3.2)". Their fn 1 reads: "Many authors, including Oaksford
  and Chater (2007) and Over and Hadjichristidis (2009) use the term 'rigidity'
  instead of 'invariance'."
* **Eq. (4) generalised updating ("Jeffrey's rule").** They obtain it by
  substituting (3) into (2). They state that it is a genuine distribution, that
  it sets `Pr₂(B)` to its new value, and that it reduces to (1) when
  `Pr₂(B) = 1` (p.290).
* **Eq. (5) Pearl's criterion** (p.291). Invariance holds iff `G` is
  conditionally independent of the experience `e` given `B`. ZO: "invariance and
  the conditional independence expressed by (5) are equivalent".
* **The converse.** `Pr(B|G)` and `Pr(B|Ḡ)` are "not expected to be invariant
  across the flashlight experience" (p.293). The reason is that "the colour of
  the card is not conditionally independent of the light given the giraffe"
  (p.305). ZO measure the violation of (3) and the movement of the converse by
  eqs. (6)-(8), `|Pr₂ − Pr₁| / Pr₁`.

Their reported numbers that the project uses or relies on:

* **Counts.** 22 of 40 participants changed `Pr(G|B)` and 18 of 40 changed
  `Pr(G|B̄)` (p.298, p.305).
* **Averages.** Mean violation 33.0% against converse movement 73.0% (p.298),
  and 0.18 against 0.45 after removing outliers (p.297). Exp. 1 shifts
  "averaging around 33%", Exp. 2 "less than 2%" (p.305).
* **The impact** on `Pr(G|B)` is "mild (although non-negligible)" (p.304).

## Result

`lean/ZhaoOsherson.lean` is a symlink to `lean/Literature/ZhaoOsherson.lean`.
It is standalone on Mathlib and checks with `lake env lean`, with no `sorry`.
Its 40 theorems use only the axioms `[propext, Classical.choice, Quot.sound]`;
7 of them use no axioms.

| Lean theorem | Content |
|---|---|
| `total_probability` | eq. (2) |
| `jeffrey_isDist`, `jeffrey_prB`, `jeffrey_prNB` | (4) is a distribution with `Pr₂(B) = q` (p.290) |
| `jeffrey_invariance` | **Jeffrey's rule on `{B, B̄}` satisfies invariance (3)**: `Pr(G\|B)` and `Pr(G\|B̄)` are unchanged |
| `jeffrey_rule` | eq. (4) |
| `invariance_imp_jeffrey` | "substituting (3) into (2) yields (4)", in its strongest form: any distribution with `Pr₂(B) = q ∈ (0,1)` that satisfies (3) *is* the Jeffrey update, cell by cell |
| `simple_updating` | `q = 1` gives rule (1) |
| `jeffrey_converse_odds`, `jeffrey_condBG`, `condBG_eq` | the odds of `B` given `G` are multiplied by exactly the factor `q(1-p) / (p(1-q))` that multiplies the odds of `B` |
| `converse_invariant_iff`, `converse_moves` | **the converse is not invariant**: with positive likelihoods, `Pr₂(B\|G) = Pr₁(B\|G)` iff `Pr₂(B) = Pr₁(B)` |
| `converse_at_independence` | even when `G` is independent of `B`, `Pr₂(B\|G) = q`, so it moves with `Pr(B)` |
| `jeffrey_movement` | the same, in ZO's measures (6)-(8): invariance violation 0 and converse movement > 0 |
| `post_condGB`, `pearl_invariance_iff` | if `Pr₂ = Pr₁(· \| e)`, then `Pr₂(G\|B) = Pr₁(G\|B,e)`, so invariance ⟺ (5) (p.291) |
| `pearl_jeffrey` | under (5) for `B` and `B̄`, conditioning on `e` *is* the Jeffrey update with `q = Pr₁(B\|e)` |
| `deck_exact`, `deck_objective` | ZO's deck (Table 1: 8/4/10/28 of 50) gives exactly 9/25, 6/25, 2/3, 5/19, 4/9, 1/8. Rounded, these are Table 2's "Objective" row: 0.36, 0.24, 0.67, 0.26, 0.44, 0.13 |
| `deck_jeffrey_instance` | **explicit instance**: update `Pr(blue)` from 0.24 to 0.75 (Jeffrey's candle value, p.289). `Pr(G\|B) = 2/3` and `Pr(G\|B̄) = 5/19` stay put, while `Pr(B\|G)` moves from 4/9 to 38/43 |
| `rain_example` | `.6 × .8 + .1 × .2 = .5` (p.290) |
| `exp1_changed_GB`, `exp1_changed_GNB`, `exp1_changed_total` | 22 = 11 Blue + 11 Purple (a majority of 40); 18 = 8 + 10; together 40 of 80 (p.304) |
| `exp1_changed_converse` | the converse `Pr(B\|G)` changed for 15 + 17 = 32 of 40 |
| `exp1_means_33_73`, `exp1_mean_27_73` | 33.0 = mean(23.5, 42.5) and 73.0 = mean(82.3, 63.7); 27.73 = mean of the four Exp. 1 violation means (27.725) |
| `exp1_outliers`, `exp1_conditioning_check` | N = 38 and 37 in Fig. 1, and t(34); the conditioning control group is within 0.03 |
| `exp2_lottery`, `exp2_ultimatum_counts` | 1.42% and 1.33% < 2%; 33 + 38 = 71 of 100 |
| `exp2_ultimatum_mean_slip`, `exp2_ultimatum_weight` | **printed 18.45% (p.304) does not reproduce**: mean(18.71, 17.38) = 18.045. Getting 18.45 needs a 107 : 26 (≈ 4.1 : 1) weighting |

`sympy/check_zo.py` (33/33 PASS) re-derives (1)-(5) symbolically. It checks
that invariance together with `Pr₂(B) = q` has a unique solution, and redoes
Pearl's criterion in a three-variable model where the light depends only on
colour. It also checks the deck instance, the counts and averages, and the four
binomial tests (exact two-sided p = .041, .0026, .033 and .0003, which match
"< .05", "< .01", "< .05" and "< .01").

## What formalizing revealed

1. **The normative contrast ZO test is exact, not approximate.** Jeffrey's
   rule on `{B, B̄}` leaves `Pr(G|B)` and `Pr(G|B̄)` fixed. It moves the
   converse `Pr(B|G)` by exactly the factor by which it moves the odds of `B`,
   and it does so whenever `Pr(B)` moves, even when `G` and `B` are
   independent. ZO's manipulation checks show that `Pr(B)` did move (median
   .30 → .80 for Blue, .38 → .05 for Purple, pp.294, 296). So normatively the
   converse *had* to move, and `Pr(G|B)` had to stay. The measured contrast (33%
   against 73%) is therefore a contrast between a quantity that should be zero
   and one that should not. It is not a contrast between two quantities that
   are both expected to move.
2. **ZO's Pearl argument is a theorem in the three-variable model**
   (`pearl_jeffrey`). If the light carries information only about colour, then
   conditioning on the light experience is Jeffrey's rule with
   `q = Pr₁(B | light)`.
3. **One printed average does not reproduce.** On p.304, "18.45% versus
   27.73%" (the mean invariance "violation", ultimatum against Experiment 1).
   The 27.73% is exactly the equal-weight mean of Experiment 1's four reported
   means. By the same method the ultimatum figure is 18.045%, not 18.45%. A
   different weighting of the two questions (≈ 4.1 : 1) or unreported
   exclusions would be needed to get 18.45. This is probably a typographical
   slip (18.05 → 18.45). No conclusion of ZO's and no claim of the project
   depends on it.

## Bearing on Paper B

The paper is not cited in the manuscript or the write-ups (review log Entry 15,
"Zhao's experimental concerns not referred to"). It remains in
`measurement_susceptibility_survey.md` as research history, so the corrections
below apply to that survey only.

The formal point relevant to Paper B is the converse theorem. A Jeffrey step on
one partition is "rigid" only for conditionals *on* that partition. Every
conditional in the other direction moves with the partition's odds. This is the
same asymmetry that Paper B's attribute locality turns on.

## Audit findings (2026-09-30)

This section summarises `notes/citation_audit.md` L10 (a), (b) and (e),
`notes/citation_audit/verify_record_papers.md` §3 and
`notes/citation_audit/verify_jeffrey.md` J11-J15, and checks them against the
paper and the formalization.

* **L10(a) / VRP 3.2 (CONTRADICTED), survey :67.** The survey says
  "'invariance' (their term; they note Jeffrey's is 'rigidity')". This is
  confirmed as wrong. p.289: "invariance is said to hold (Jeffrey, 2004, §3.2)".
  fn 1 attributes "rigidity" to Oaksford and Chater (2007) and Over and
  Hadjichristidis (2009). Qualification (J15): Jeffrey himself uses
  "rigidity" in *Subjective Probability* ch. 2, for conditioning on a
  certainty. So the correction must not say that "rigidity" is foreign to
  Jeffrey. It should report what ZO say.
* **L10(b) / VRP 3.6 (MISQUOTED), survey :101-102.** The survey's version,
  "the ineffable character of sensory impressions (Jeffrey 1983, x11.1)",
  rewrites the parenthetical inside the quotation marks and keeps the
  pdftotext artifact "x" for "§". p.306 reads "the ineffable character of
  sensory impressions (as stressed by Jeffrey, 1983, §11.1)". Confirmed. The
  location is right against *The Logic of Decision* (J11), though Jeffrey never
  uses the word "ineffable".
* **VRP 3.7 (VWC), survey :102-104.** The ineffability remark is offered "As an
  alternative to vivacity". It is one of three tentative explanations ("Of
  course, more data are needed"). The survey presents it as ZO's explanation.
* **L10(e) / VRP 3.5 (VWC), survey :86-88.** The survey says "stability in
  `Pr(G|B)`". Confirmed as overstated.
  * A majority, 22 of 40 (11 + 11, `exp1_changed_GB`), changed `Pr(G|B)`.
  * The shifts averaged 33.0% of the initial value.
  * ZO call the impact "mild (although non-negligible)".
  * What is supported is the *relative* claim: `Pr(G|B)` moved less than the
    converse. The figures are 33.0% vs 73.0% over all 40, and 0.18 vs 0.45 over
    the 35 left after outliers. By count, 22 vs 32 of 40 changed.
  * The normative baseline for that contrast is exact: 0 against a positive
    amount (`jeffrey_movement`).
* **VRP 3.1 (VWC), survey :64-66.** "Twice, before and after" holds for the 40
  experimental participants only. The 30 control participants answered once,
  after the light (p.293).
* **VRP 3.3, 3.4, 3.8, 3.10 (VERIFIED)** are confirmed. Eq. (3) is on p.289,
  the tests are within-subject, 33% vs "less than 2%" is on p.305
  (`exp2_lottery`), and the framing is descriptive (p.291).
* **New, about the paper.** The "18.45%" on p.304 does not reproduce (see
  above). The project does not use it.

### Proposed corrected wording (not applied)

* **Survey :64-68.** Replace the first three sentences of "What they elicit"
  with:
  > Conditional probabilities, directly and by name. In Experiment 1 the 40
  > experimental participants estimate `Pr(G)`, `Pr(B)`, `Pr(G|B)`, `Pr(G|~B)`,
  > `Pr(B|G)`, `Pr(B|~G)` twice, before and after a dim-flashlight glimpse of a
  > card's colour; 30 control participants answer once, after the glimpse. The
  > test of "invariance" (their term, which they attribute to Jeffrey 2004,
  > §3.2; their footnote 1 notes that Oaksford and Chater and Over and
  > Hadjichristidis call it "rigidity") is whether
  > `Pr_2(G|B) = Pr_1(G|B)` and `Pr_2(G|~B) = Pr_1(G|~B)`.
* **Survey :86-88.** Replace "(finding rough, selective conformity -- stability
  in the conditionally-independent direction `Pr(G|B)`, larger movement in the
  non-invariant converse `Pr(B|G)`)" with:
  > (finding rough, selective conformity: `Pr(G|B)`, which invariance requires
  > to stay fixed, moved less than its converse `Pr(B|G)`, which normatively must
  > move -- by 33% against 73% of the initial estimate on average -- yet 22 of
  > the 40 participants changed `Pr(G|B)`, an impact the authors call "mild
  > (although non-negligible)")
* **Survey :100-104.** Replace from "Their discussion reaches for" to "tangible
  lottery draw." with:
  > Among three tentative explanations of why invariance was violated more
  > under the flashlight than in the lottery scenario, their discussion offers,
  > "as an alternative to vivacity", that the flashlight's greater impact "might
  > be related to the ineffable character of sensory impressions (as stressed by
  > Jeffrey, 1983, §11.1)", the lottery event being "more tangible"; they add
  > that "more data are needed".

## Not formalized

* The t-tests, Wilcoxon tests and regressions. The binomial tests are
  recomputed in `check_zo.py` only.
* Finer partitions. ZO fn 2 keep to the binary case.
* The Amazon Turk replication (fn 5), which is reported without data.
