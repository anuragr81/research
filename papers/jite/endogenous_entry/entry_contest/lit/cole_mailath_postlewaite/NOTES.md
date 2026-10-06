# Notes — Cole, Mailath & Postlewaite

Run `python3 lit/cole_mailath_postlewaite/verify_cmp.py` from the
`entry_contest` root. Result on 2026-10-06 is **14 checks, 0 failures**, with
`ColeMailathPostlewaite.lean` at 41 theorems, all audited, 15 axiom-free, 26
on `propext`/`Quot.sound` only, none on anything else. The `lean` on `PATH` is
v4.34.1; the file was also compiled directly with the v4.33.1 toolchain
(`~/.elan/toolchains/leanprover--lean4---v4.33.1/bin/lean`), exit code 0.

**Scope.** Pass 1 (below) checked the *use* we make of these papers. Pass 2
formalises in Lean the parts of the papers that our documents attribute and
that are mathematical, namely the CMP92 §IV.A two-period example and the rank
allocation of the prize. The status sentence (CMP-B) and the welfare sentence
(CMP-C) are prose in the sources and are verified by quotation, not by Lean.
Nothing here reproves a result of either paper. The permitted conclusion is
that our reading is arithmetically consistent with the sources, with the
discrepancies listed under Findings.

## Source identities (both read in full from Drive)

| Drive file | Drive id | Title page | Identity |
|---|---|---|---|
| `cole_status_value.pdf` | `16WUA3v3ofhgiCMQ7QMSjYU7p5tNrTd7K` | "Social Norms, Savings Behavior, and Growth", Cole, Mailath and Postlewaite, *Journal of Political Economy* 100(6), Centennial Issue, Dec. 1992, pp. 1092–1125, JSTOR stable 2138828 | `ColeMailathPostlewaite1992`, published version, 35 PDF pages, PDF page `p` is journal page `1090 + p` |
| `colemaliath_incorporating_concern.pdf` | `1E8Q-6SfQTqG742yjCVBhqXD3xYkCizRg` | "CARESS Working Paper #95-14 Incorporating Concern for Relative Wealth into Economic Models", "Forthcoming, Quarterly Review, Federal Reserve Bank of Minneapolis August 1, 1995" | `ColeMailathPostlewaite1995`, **working paper**, 24 PDF pages, PDF page `p` is printed page `p − 2` |

The §IV.A example is in the 1992 JPE paper, pp. 1100–1103. Equations were read
from rendered page images, because the Drive text layer garbles them.

Rule 5 applies to CMP95. The working-paper title has "Concern" singular. The
*Quarterly Review* title is not on either PDF, so TODO S1 stays open. Every
CMP95 page number in this directory is a working-paper page. `PROOFS.tex`
l.364–367 quotes CMP95 without a page, which is safe; a page added later must
say "working paper, p. 15" or be re-checked against *QR* 19(3).

## What `ColeMailathPostlewaite.lean` states, theorem by theorem

Notation in the file. `λ` is the first-period consumption fraction, so the
savings rate is `S − λ` with `S` the scale of 1. `U λ` stands for
`u(AKλ) + βu(A²K(1 − λ))`, the objective of CMP92 eq. (2). `bj` stands for
`βj`. `ls` stands for `λ(0)`. `Uc` stands for the constrained maximum in the
p. 1103 problem and `bh` for `β·½`. Scaled integers carry every quantity in
the abstract theorems. The `inst_*` theorems use an exact rational pair type
`Q` whose equality and order are cross-multiplied integer statements with
non-zero denominators, decided by `decide`.

| Lean theorem | Claim | Page, locator | Verbatim quote |
|---|---|---|---|
| `twoPoint_mean` | CMP-E1 | CMP92 p.1102, §IV.A | "Note that the average level of capital is the same as before but that the distribution is now unequal." |
| `twoPoint_unequal` | CMP-E1 | CMP92 p.1102, §IV.A | "man i ∈ [0, ½) has initial capital level K − ε and i ∈ [½, 1] has initial capital K + ε, where ε > 0." |
| `twoPoint_ranks` | CMP-E1 | CMP92 p.1102, §IV.A | "The sons of those agents with the lower capital endowment will be matching in equilibrium with the lower half of the distribution of the women." (The theorem states the fathers' initial ranks; the sons inherit the order by CMP-I.) |
| `total_append`, `total_rep_pair` | CMP-E1 | CMP92 p.1102 | Helpers for `twoPoint_mean`, same quote. |
| `matching_raises_savings` | CMP-E2 | CMP92 p.1102, §IV.A | "First, matching considerations cause agents to increase their savings levels relative to their nonmatching or involuntary matching levels." |
| `lambda_decreasing_in_j` | CMP-E2 | CMP92 p.1102, display after eq. (2) | "Since λ(j) < λ(0), the denominator is positive and so ∂λ/∂j < 0." |
| `lower_V_lower_lambda` | CMP-E2 | CMP92 p.1103 | "Notice that the lower V(½) is, the lower V(j) must be and, hence, the lower λ(j) must be also." |
| `slack_of_feasible` | CMP-E3 | CMP92 p.1103, the constrained problem | "This last restriction requires that the savings rate for this man be sufficiently high that his son's capital level is at least as large as that of the son of any man in the lower half of the distribution." |
| `compact_saves_more` | CMP-E3 | CMP92 p.1103 | "The average savings rate in any section of the top half of the distribution of second-period capital must be lower than the comparable fraction from the one-point capital level economy." |
| `compact_saves_more_from_primitives` | CMP-E3 | CMP92 p.1103 | "if we compare two men with the same initial income level in two different economies in which the equilibrium assignment rule for mates is based on wealth, then the agent from the economy with the more compact income distribution will tend to have a higher savings rate, all other things being equal." |
| `compact_saves_strictly_more` | CMP-E3 | CMP92 p.1103 | "must be lower than the comparable fraction from the one-point capital level economy." |
| `comparison_needs_slack` (control) | CMP-E3 | CMP92 p.1103 | "As ε gets larger and the restriction that the son with mate j = ½ be at least as wealthy as any son with mate j < ½ becomes less severe, the savings rates for men in the top half of the distribution fall." |
| `comparison_needs_equal_income` (control) | CMP-E3 | CMP92 p.1103 | "this subset of men is wealthier (whereas those in the bottom half of the distribution are poorer), and this will tend to increase or decrease (decrease or increase) the extent of their sons' competition for mates depending on whether γ is greater or less than one, respectively." |
| `inst_lambda0_formula` | CMP-E4 | CMP92 p.1101, after eq. (2) | "Denote the optimal value of λ by λ(0) = [1 + (βA^{1−γ})^{1/γ}]^{−1}." |
| `inst_lambda0_foc` | CMP-E4 | CMP92 p.1101, eq. (2) | "V(0) ≡ max_λ u(AKλ) + βu(A²K(1 − λ))." |
| `inst_lower_half` | CMP-E4 | CMP92 p.1102–1103 | "has welfare level V(0) given by (2) with K − ε replacing K" and "As before, we must have V(j) = V(0) for j < ½. This in turn determines his agent's consumption rate and hence his son's capital level, k(j)." and "k⁻(½) = lim_{i↑½} k(i)" |
| `inst_restriction_binds` | CMP-E4 | CMP92 p.1103, constraint | "subject to λ ≤ 1 − k⁻(½)/((K + ε)A)." |
| `inst_welfare_levels` | CMP-E4 | CMP92 p.1103 | "His equilibrium welfare level, V(½), is the value of the following maximization problem" with objective "u(A(K + ε)λ) + βu(A²(K + ε)(1 − λ)) + β½" |
| `inst_one_point_match` | CMP-E4 | CMP92 p.1101–1102 | "V(j) ≡ u(AKλ(j)) + β[u(A²K(1 − λ(j))) + j] = V(0)" and "λ(j)^{1−γ} + βA^{1−γ}[1 − λ(j)]^{1−γ} = [V(0) − βj](1 − γ)(AK)^{γ−1}." |
| `inst_two_point_match` | CMP-E4 | CMP92 p.1103 | "must be such that V(j) = V(½), where his initial consumption rate, λ(j), is adjusted so that this equality holds." |
| `inst_compact_saves_more` | CMP-E4 | CMP92 p.1103 | "the agent from the economy with the more compact income distribution will tend to have a higher savings rate" |
| `inst_dlambda_dj_negative` | CMP-E4 | CMP92 p.1102 | "∂λ(j)/∂j = −β(AK)^{γ−1} / (λ(j)^{−γ} − βA^{1−γ}[1 − λ(j)]^{−γ})" and "Since λ(j) < λ(0), the denominator is positive" |
| `inst_not_mean_preserving` | CMP-E4 | CMP92 p.1103 | "choosing initial distributions in the two cases such that the wealthier agents in the two-point distribution are just as well off as the agents in the one-point distribution." |
| `compared_dispersive` | CMP-F | CMP92 p.1103; dispersive order as in `lit/hopkins_kornienko` | "The average savings rate in any section of the top half of the distribution of second-period capital must be lower than the comparable fraction from the one-point capital level economy." |
| `compared_nonpositive` | CMP-F | CMP92 p.1103 | same quote |
| `compared_zero_top` | CMP-F | CMP92 p.1103 | "if we compare two men with the same initial income level" |
| `meanPreserving_crosses` | CMP-F | CMP92 p.1102 | "Note that the average level of capital is the same as before but that the distribution is now unequal." |
| `rank_mono` | CMP-G1 | CMP92 p.1101, §IV.A | "A man's status, then, is determined precisely by his capital relative to other men's capital, higher capital yielding higher status." |
| `rank_ordinal` | CMP-G1 | CMP92 p.1101, §IV.A | "A man's match in period 2 depends, then, only on his relative position in the capital distribution of period 2." |
| `rank_append`, `rank_rep_below`, `rank_rep_not_below` | CMP-G1 | CMP92 p.1101 | "matching the wealthiest man with the woman of highest endowment, and so on." |
| `cmp95_eq28` | CMP-G2 | CMP95 p.6, eq. (2.8); p.4 | "(2.8) m(y(j)) = g(1 + g)α²j = j." and "In other words, m is the distribution function of female output." |
| `cmp95_eq25_instance` | CMP-G2 | CMP95 p.6, eq. (2.5) | "(2.5) g(1 + g) = 1/α²." |
| `cmp95_slope_falls_with_alpha` | CMP-G2 | CMP95 p.7, §2.1 | "if the productivity multiplier in society A was greater than that in society B, α_A > α_B, then this would imply that society A's matching function was flatter, g_A < g_B" |
| `cmp95_effort_falls_with_alpha` | CMP-G2 | CMP95 pp.7–8, §2.1 | "and females with identical ability levels would choose to work less in society A than in B. This is because output levels would be more disperse in society A than in B; hence the competition over matches would be more intense in B." |
| `rank_none_below`, `rank_bottom` | CMP-H | CMP92 p.1101; n.11 p.1102 | "His savings behavior cannot be distorted (from the level that is optimal when matching considerations are ignored) since any deviation in savings cannot reduce the quality of his match." and "The man at the bottom of the matching hierarchy who matches with the least endowed woman is the only exception; his savings level is undistorted." |
| `property1_no_switch` | CMP-I | CMP92 p.1107 (statement), p.1122 (proof) | "PROPERTY 1. In an equilibrium, if k₀(i) > k₀(i′), then, for all t, the optimal level of capital in period t for agents i and i′ satisfies k_{t+1}(i) > k_{t+1}(i′)." and "This contradicts our assumption that u(·) is a monotonically increasing, strictly concave function, and hence switching is inconsistent with optimizing behavior." |
| `property1_tie_not_excluded` (control) | CMP-I | CMP92 p.1122 | "Let i and i′ be two family lines in which a switch occurs" |

### The `γ = 2` instance (CMP-E4), every number exact

`u(c) = −1/c`, `A = 3`, `β = 3/4`, so `βA^{1−γ} = 1/4` and `λ(0) = 2/3`. The
two-point economy has `K − ε = 1` and `K + ε = 5/3`, so `K = 4/3` and
`ε = 1/3`. The lower-half man whose son reaches rank `½` consumes `λ = 1/3`
and bequeaths `k⁻(½) = 2`. The top-half restriction is `λ ≤ 3/5`, which binds
because `3/5 < 2/3`. The comparison economy is the one-point economy at the
same initial capital `5/3`. Its welfare level is `V(0) = −9/20`, and the
two-point top half has `V(½) = −1/12`. At the match `j = 5/9` the one-point man
consumes `λ = 1/4` and saves 3/4 of first-period output, while the two-point
man consumes `λ = 1/2` and saves 1/2. The paper's p. 1102 display is checked at
both points, with `V(½)` in place of `V(0)` for the two-point man, which is the
same derivation applied to the p. 1103 problem (our step, stated here because
the paper displays the equation for the one-point case only).

## Analytic steps carried as hypotheses

- `StrictUpBelow U ls`, that `U` is strictly increasing on consumption
  fractions at or below `λ(0)`. The paper uses this through the strict
  concavity of CRRA `u`. SymPy check CMP-5 confirms the `γ = 2` case exactly,
  through the identity `g(λ) − 9/4 = (3λ − 2)²/(4λ(1 − λ))`.
- The selection `λ(j) ≤ λ(0)`, which the paper states on p. 1102 as
  "and λ(j) < λ(0)".
- `hmax`, that `Uc` is the maximum of `U` over the restricted set, and in the
  instance that the maximum sits at the binding `λ = 3/5`.
- `hfeas`, that the one-point man at rank `½` already satisfies the two-point
  restriction, `k⁻(½) ≤ k₁(½)`. **The paper does not state this step.** See
  Finding 3.
- The continuity step `λ(j) → 1/3` as `j ↑ ½` in the instance, which is how the
  paper defines `k⁻(½)`.
- In CMP-I, strict supermodularity of `(k, k′) ↦ u(Ak − k′)`, which is the
  content of "strictly concave" in the p. 1122 proof.

## Controls (rule 3)

- `comparison_needs_slack`. All hypotheses of `compact_saves_more` hold except
  the slack `U ls ≤ Uc + bh`, and the compact economy then saves strictly less.
  So the slack hypothesis is load-bearing rather than decorative.
- `comparison_needs_equal_income`. The two compared men get different utility
  functions, which is what different initial capital does, every other
  hypothesis holds including the slack, and the comparison reverses. So the
  paper's "same initial income level" is load-bearing. CMP-7 finds the same
  reversal in the CRRA model, in 243 of 2318 grid cases.
- `property1_tie_not_excluded`. Both exchange inequalities of the p. 1122 proof
  hold, with strict supermodularity, for a richer and a poorer family whose
  bequests are equal. So the displayed argument yields `k_{t+1}(i) ≥
  k_{t+1}(i′)` and not the strict inequality Property 1 states.
- SymPy and numeric controls. CMP-5 confirms that `λ = 1/4` fails `V(j) = V(0)`
  at `j = ½`. CMP-6 swaps the two capitals and the inequality fails in all 788
  cases. CMP-7's mean-preserving framing reverses in 243 cases, so CMP-7 would
  detect a reversal under the equal-income framing if one existed.

## Claims verified by quotation only

- **CMP-B**, CMP92 abstract, p. 1092. "We interpret an agent's status as a
  ranking device that determines how well he or she fares in the nonmarket
  sector." `PROOFS.tex` l.362–364 quotes this verbatim. The body restates the
  sentence twice with different endings, p. 1093 "with respect to the
  allocation of nonmarket goods" and p. 1096 "how well he fares with respect to
  nonmarket decisions". The abstract wording is the one we quote.
- **CMP-C**, CMP95 §4, p. 15. "In particular, we should note that when the
  desirable goods or decisions are allocated as prizes rather than sold, the
  standard welfare theorems regarding the Pareto optimality of the outcomes no
  longer apply." `PROOFS.tex` l.364–367 quotes the fragment from "are
  allocated" verbatim. The unquoted lead-in "when desirable goods" drops the
  source's "or decisions", which is outside the quotation marks and harmless.
  This sentence is a remark in Concluding Comments, with no theorem, proof or
  stated conditions, so no Lean statement is possible without inventing a
  model the paper does not give.
- **CMP-A, the "not sold" half**, CMP95 p. 15. "women in our models don't
  really pay for mates. A woman who generates the highest wealth in the first
  period does match with the wealthiest man, but she also continues to consume
  the wealth she accumulated. To make the land example analogous to our
  models, we should have the land simply given away, with the best given to the
  wealthiest, and so on." The rank half of CMP-A is in Lean (CMP-G1, CMP-G2).
- **CMP-H, the CMP95 half**, p. 7. "It can also be seen from this example that
  it is competition from below that distorts individuals' effort decisions."
  and "However, truncating from the bottom would create a new
  lowest-productivity female who cannot be distorted."
- **CMP-K**, the remaining `LITERATURE.tex` paraphrases, each checked against
  the page and listed under Findings 6 to 9 where they differ.

## Attributed claims not formalised, and why

- CMP-B and CMP-C, as above, because they are prose.
- CMP-D (multiple equilibria), context only and not used by `PROOFS.tex`.
- CMP92 Proposition 1 (existence by Mas-Colell's theorem, Appendix A) and
  Proposition 2 (aristocratic equilibrium). Both are measure-theoretic
  existence arguments over a continuum, `LITERATURE.tex` uses them as context,
  and core Lean without Mathlib has no measure theory.
- CMP95 §3 (signalling, `d(j)/y(j)` increasing in `j`). The closed form (3.10)
  involves `e^{γj}`, which has no exact rational instance, and `PROOFS.tex`
  does not use it.

## Findings

1. **Direction confirmed, three qualifiers missing.** CMP92 §IV.A does have
   the more compact distribution producing the higher savings rate. The Lean
   theorem `compact_saves_more` proves it from the paper's hypotheses, and the
   exact instance gives 3/4 against 1/2. The paper's sentence on p. 1103 carries
   three qualifiers that `PROOFS.tex` l.1620–1621 and `LITERATURE.tex`
   l.862–863 drop. The compared men have "the same initial income level"; the
   comparison covers "any section of the top half"; and the agent "will tend to
   have a higher savings rate, all other things being equal". `LITERATURE.tex`
   keeps "tends to" and drops the other two.
2. **The comparison is not a mean-preserving contraction.** The paper first
   builds the two-point economy around mean `K` (p. 1102), then compares it
   with a one-point economy at the top-half capital `K + ε` (p. 1103), whose mean
   is higher. In the instance the two-point mean is 4/3 and the comparison
   economy sits at 5/3 (`inst_not_mean_preserving`). Under the mean-preserving
   framing, one-point at `K` against the top half at `K + ε`, the compact economy
   saves *less* in 243 of 2318 grid cases, all at `γ = 3` or `γ = 5` (CMP-7).
   The paper's own p. 1103 sentence on `γ` anticipates this. The falsifier for
   reading §IV.A as "a mean-preserving spread lowers savings" is therefore
   realised, and the received result as stated in `PROOFS.tex` holds only in
   the equal-income form.
3. **An unstated step.** "Must be lower" on p. 1103 needs the two-point
   restriction at rank `½` to be no tighter than what the one-point man at
   rank `½` already saves, `k⁻(½) ≤ k₁(½)`. The paper does not say so. On the
   grid of CMP-6 (`γ` from 0.3 to 5) the step holds in all 788 cases. Our
   inference for why, not checked in Lean, is that with CRRA the bequest needed
   to reach a given rank rises with initial capital for every `γ > 0`. The
   Lean theorem therefore carries the step as hypothesis `hfeas`.
4. **"Just as well off" means equal initial capital.** Read as equal welfare
   at equal capital, the two `V` targets would coincide and the two savings
   rates would be equal, which contradicts "must be lower". The next sentence,
   "two men with the same initial income level", fixes the equal-capital
   reading. In the instance the two-point top-half man is strictly better off,
   `V(½) = −1/12` against `V(0) = −9/20`.
5. **The below-pivot mapping in `PROOFS.tex` is not supported by the example's
   structure.** `PROOFS.tex` l.1175–1176 and l.1624–1625 place CMP's finding on
   the branch of "mass competitions, in which the marginal entrant sits low in
   the distribution". In the comparison the paper draws, the quantile
   displacement is `−2ε` below rank `½` and `0` from rank `½` up
   (`compared_nonpositive`, `compared_zero_top`), and the men whose savings
   fall are the top half, whose own capital the spread leaves unchanged. Their
   savings fall because the poorer half below them relaxes the p. 1103
   restriction, a channel through rivals' wealth. `PROOFS.tex` l.1155–1156 says
   `Δ` "is a function of the entrant count alone and is invariant to the wealth
   distribution", so that channel is absent from our model. Under the
   mean-preserving framing the top half sits *above* the pivot
   (`meanPreserving_crosses`), and there CMP's savings mostly fall (2075 of
   2318 cases), the opposite of the sign `PROOFS.tex` assigns to the
   above-pivot branch. The correspondence that does hold is the sign of the
   aggregate statement, more dispersion and less competition. The position of
   the affected agents and the channel do not match. The falsifier for this
   finding would be a CMP comparison in which the agents whose competition
   falls are the ones the spread moves down, and neither framing in §IV.A
   has that. The Lean file proves the displacement facts only; it does not
   formalise our sign rule.
6. **`LITERATURE.tex` l.864 drops a condition from §V.D.** The source, p. 1117,
   reads "As seen in the wealth-is-status savings model with γ < 1, when the
   capital distribution becomes sufficiently dispersed, the increased incentive
   to save disappears." `LITERATURE.tex` omits "with γ < 1".
7. **`LITERATURE.tex` l.858–859 drops conditions from Proposition 2.** The
   source, p. 1111, reads "PROPOSITION 2. Suppose k₀(0) = 0. Fix i′ ∈ (0, 1) and
   suppose β ∈ (β(i′), 1). An aristocratic equilibrium exists if γ > 1 and" an
   inequality holds "for all i ≤ i′". `LITERATURE.tex` keeps "spread out at the
   lower tail" (p. 1111, "if capital is sufficiently 'spread out' at the lower
   tail and k₀(0) = 0") and omits `γ > 1`, `k₀(0) = 0` and `β` near one.
8. **`LITERATURE.tex` l.856–857 misplaces the exception in Property 2.** The
   source, p. 1107, says "For all family lines and all t ≥ 0, λ_t ≤ λ*", then
   "In fact, the inequality is strict for all but a set of families of measure
   zero", then that the family matched with the zero-endowment woman has
   "λ_t = λ* for all t". The bottom family is an exception to the strict
   inequality, not to the weak one, whereas `LITERATURE.tex` writes "everyone
   weakly over-saves ... except the bottom agent".
9. **Two CMP95 locators in `LITERATURE.tex`.** The "competition from below"
   sentence and the truncation argument are in the body of §2.1, p. 7, which
   invokes footnote 8; footnote 8 itself (pp. 5–6) is about the lowest-ability
   female being undistorted, whereas `LITERATURE.tex` l.879 attributes the
   whole passage to "Footnote 8". The tight-distribution sentence ("if the
   distribution of females' output is tight, m′ is large") is in §2, p. 5, not
   §2.3 as l.877 cites. §2.3 is about tax policy.
10. **Property 1's displayed proof rules out a strict reversal only.** This is
    an observation about the source, not a discrepancy with our documents.
    `property1_no_switch` proves `k_{t+1}(i′) ≤ k_{t+1}(i)` from the two
    p. 1122 inequalities, and `property1_tie_not_excluded` shows those
    inequalities do not exclude equal bequests. The strict ">" in the statement
    needs something further, such as Property 3 (no atoms). `LITERATURE.tex`'s
    "no family's relative rank ever changes" is safe under either version.
11. **Closer textual support for an exogenous rank-allocated `V`.** CMP95 §2.2,
    p. 8, reads "Since men make no decisions in this model, they play no role
    other than to serve as prizes in the wealth tournament the females are
    engaged in. Any other exogenously given set of prizes that are to be
    awarded to females based on their relative rank in the final wealth
    distribution would serve the same purpose." That passage supports
    `PROOFS.tex` l.356–371 (an exogenous, fixed prize allocated by rank) more
    directly than the §4 remark does, and it is a statement about the model
    rather than a concluding remark. Suggestion for the author only; neither
    `.tex` file was edited.
12. **Broader locators for the received result.** CMP92 states the same
    direction outside §IV.A. §IV.C, p. 1106, reads "This will imply, all else
    being equal, that a more equal distribution of capital leads to a smaller
    share of current output devoted to current consumption, since a more equal
    distribution will be reflected in a larger m′_{t+1}." §V.A, p. 1114, reads
    "for example, other things being equal, tighter distributions of agents'
    characteristics induce greater deviations from that behavior than would
    have arisen had the ranking not mattered." Both run through the slope of the
    matching function at the agent's own wealth, which is the density channel
    of Hopkins and Kornienko rather than the rivals'-restriction channel of
    §IV.A.

---

# Pass 1

## The citation instruction is sound

The survey's next-step 2 — cite CMP92/95 where `V` is introduced, "since they
justify a rank-allocated non-market prize" — holds up. Both halves are in the
sources:

- **A prize allocated by rank rather than sold.** CMP95 §4: *"To make the land
  example analogous to our models, we should have the land simply given away,
  with the best given to the wealthiest, and so on."*
- **Status as the ranking device.** CMP92 abstract: *"We interpret an agent's
  status as a ranking device that determines how well he or she fares in the
  nonmarket sector."*

Their examples are close to the paper's motivating cases: *"Country club
memberships, charity board invitations, university trusteeships, invitations to
chic parties, and assigned seats in churches and synagogues."*

And the generalisation gives the licence directly (CMP95 §4): *"Whenever an
increase in an individual's position in the wealth distribution by itself
increases the likelihood of obtaining desirable outcomes, optimal individual
behavior will exhibit some of the qualitative features exhibited in the models
analyzed above."*

---

## CMP-C confirmed verbatim — with one qualification

The handover cites "Cole–Mailath–Postlewaite 1995 §4 (welfare theorems do not
apply to prizes allocated by rank)". The source says exactly that:

> "when the desirable goods or decisions are allocated as prizes rather than
> sold, the standard welfare theorems regarding the Pareto optimality of the
> outcomes no longer apply."

**Qualification: §4 is *Concluding Comments*.** This is a discussion section,
not a formal result — there is no theorem, no proof, no stated conditions. It
is an authoritative conceptual claim by three authors who have just built the
models, and it should be cited that way: as motivation for why a welfare
analysis is needed, not as a result that does any work. A referee who follows
the citation expecting a proposition will not find one.

This matters because the handover's welfare plan lists CMP95 §4 alongside
Levin–Smith Prop 3 and Becker–Murphy–Werning §7 as though they were the same
kind of object. They are not: LS Prop 3 is a proposition with a proof; CMP95 §4
is a paragraph. Both are usable, for different purposes.

---

## The finding: CMP's conceptual premise generates LS's welfare function

This is what makes the directory worth having.

CMP95's point is conceptual — prizes given by rank are not sold, so the welfare
theorems lapse. Levin–Smith's is algebraic — with `V_n ≡ V` the marginal
entrant's social gain is zero while her private gain is positive. **They are
the same point, and the bridge is one line.**

Take CMP's premise literally: one prize of value `V`, delivered to the
top-ranked competitor, delivered iff at least one competitor enters (CMP-1: the
delivered prize mass is `V` regardless of the count, so `∂/∂n = 0`). With
independent entry at probability `q` among `N`, each entrant paying `c`:

> `S = V·Pr(at least one entrant) − (expected total entry cost)`
> `  = [1 − (1−q)^N]V − qNc`

which is **Levin–Smith equation (8) exactly** (CMP-2).

So the deferred welfare section has a conceptual authority (CMP95 §4) and a
formal apparatus (LS §I.A) that agree, and deriving one from the other takes a
sentence. CMP-3 states the resulting wedge arithmetically: social marginal
value of an entrant beyond the first is `0`, her private gain is `Δ(m) ≥ κ > 0`.
That is the same wedge as LS-7's business stealing.

---

## Why we may import the justification without the machinery

`V` enters our model **only as a multiplicative scale factor**. P1 gives
`Δ(m) = V·E[φ(M_m)]`, so `∂Δ/∂V = E[φ]` is free of `V`, `∂²Δ/∂V² = 0`, and `Δ`
is exactly homogeneous of degree one in `V` (CMP-4).

Nothing in P1, P3, P5, P7, P8 or P9 depends on *where* `V` comes from — only on
its being fixed and rank-allocated. That is the licence to cite CMP for the
foundation without inheriting their matching model, their CRRA specification,
or their multiple-equilibria machinery. Worth stating in one sentence at the
point of citation, because it pre-empts "why are you citing a marriage-matching
model?"

---

## Results

| ID | Result |
|---|---|
| CMP-1 | CONSISTENT. Delivered prize mass is `V` independent of the count; `∂/∂n = 0`. |
| CMP-2 | CONSISTENT, and the useful one. The rank-allocation premise reproduces LS eq. (8) exactly. |
| CMP-3 | CONSISTENT. Social marginal value `0`, private gain `Δ > 0`; wedge equals the private gain. |
| CMP-4 | CONSISTENT. `Δ` homogeneous of degree one in `V`; `∂²Δ/∂V² = 0`. |
| CMP-5 | CONSISTENT. The `γ = 2` instance of §IV.A, exact (pass 2). |
| CMP-6 | CONSISTENT on the grid, 788 cases. The unstated step holds numerically (pass 2, Finding 3). |
| CMP-7 | The §IV.A claim holds in all 2318 equal-income cases and reverses in 243 mean-preserving cases (pass 2, Finding 2). |
| CMP-Lean | 41 theorems compile, audited, no axiom beyond `propext`/`Quot.sound` (pass 2). |

## Not attempted in either pass

1. **CMP92 §III's** equilibrium existence and characterisation, §III.D's
   aristocratic equilibrium, and §IV.B to §IV.D (the infinite-horizon
   wealth-is-status model, Propositions 1 and 2, Properties 2 to 6). Property 1
   is formalised as its exchange argument only (CMP-I). §IV.A, the two-period
   example, is formalised in pass 2.
2. **CMP95 §3** (incomplete information and signalling). CMP95 §2 is
   formalised in pass 2 through the closed form of §2.1 (CMP-G2).
3. The Appendices of both papers, beyond the Property 1 proof.

**CMP-D** (multiple equilibria explaining growth differences) is context only
and is unchecked.
