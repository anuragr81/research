# Benjamin, D., Bodoh-Creed, A. & Rabin, M. (2019), "Base-Rate Neglect: Foundations and Implications"

Working paper, July 19, 2019, 62 pp. The title page reads "Dan Benjamin and
Aaron Bodoh-Creed and Matthew Rabin" and "[Please See Corresponding Author's
Website for Latest Version]". It is unpublished. Ortoleva's survey cites it as
"Benjamin DJ, Bodoh-Creed A, Rabin M. 2019. Base-rate neglect: foundations and
implications. Work. Pap., Univ. Calif., Berkeley".

**Source.** The copy used is the PDF of the 2019-07-19 working paper
(`benjamin2019.pdf`, with a text extraction beside it; Drive:
`baserateneglect-2019-07.pdf`). The printed page numbers coincide with the PDF
page numbers, and the references below use them. The whole paper was read on
2026-09-30, including Appendices A and B. Every formula used below was checked
on the rendered pages.

**Notation.** The paper's `α` is the prior-weight exponent. It is **not** Paper
B's `α = P(A=0)`. Below, "BRN `α`" always means the paper's exponent.

## Claims formalized

The model:

* **One-shot rule** (p.2; eq. (2) on p.10 with subjective likelihoods):
  `p_α(θ|s) = p(s|θ) p(θ)^α / ∑_θ' p(s|θ') p(θ')^α`, with `α ∈ [0,1)`. Here
  `α = 1` is Bayes ("Tommy") and `α < 1` is base-rate neglect ("Saki"). The
  specification is Grether's regression (fn. 1, p.2; eq. (3), p.12, where
  `β₂` corresponds to `α`). Benjamin's (2018) meta-analysis gives `α̂ = 0.61`,
  or `0.43` in incentivized studies, and `β̂₂ = 0.88` in sequential designs
  (p.12).
* **Dynamic assumption** (p.3, and p.19: "the key additional assumption—the
  major modeling gambit of the paper—is that a person who sequentially
  processes information will treat updated posteriors as the priors for further
  updating"). The authors "know of no direct tests of this assumption" (p.19),
  and they name it "the most promising target for experimental testing" (p.46).
* **Closed form.** Eq. (4) (p.20) gives two signals, the display below it gives
  `t` signals, and eq. (5) (p.21) gives conditionally independent signals:
  `p_α(θ|s₁..s_t)/p_α(θ̃|s₁..s_t) = (p(θ)/p(θ̃))^{α^t} ∏_τ (p(s_τ|θ)/p(s_τ|θ̃))^{α^{t-τ}}`.
* **Eq. (6)** (p.21), in logs: `L = ∑_τ α^{t-τ} l_τ + α^t l₀`. The paper reads
  three things off it:
  * a long run of uninformative signals drives beliefs to uniform, which is
    "a long-run form of the moderation effect" (p.21);
  * "Saki's beliefs exhibit a recency bias—she draws stronger inferences from
    signals observed recently relative to signals observed in the more distant
    past" (p.21);
  * "the influence of a signal on Saki's current belief is exponentially
    declining in the number of intervening signals. This generates a recency
    effect whereby early signals eventually have no influence on her beliefs"
    (p.3).
* **Eq. (7)** (p.22), non-convergence: if `|l_τ| ≤ L̄`, the log odds are at most
  `L̄/(1-α)`, so "Saki's beliefs are always bounded away from certainty". Eq. (8)
  (p.22) gives the mean log odds, `E[l]/(1-α)`.
* **Proposition 1** (extreme moderation, p.14; proof p.52, eq. (17)). A signal
  favouring `θ` lowers the odds on `θ` iff its likelihood ratio is below
  `(p(θ)/p(θ'))^{1-α}`.
* **Moderation on average** (p.14). This is Augenblick & Rabin's (2017) result
  that expected uncertainty reduction is lower for Saki. It is cited, not
  proved, here.

Section 3 closes with two "completions":

* **Signals** (end of Sec. 2.4, p.19): Saki "codes any random variable with a
  distribution that depends on θ as a signal".
* **Prospective beliefs** (p.23): "to complete our theory of base-rate neglect,
  we assume that Saki believes she will be a Bayesian when she observes future
  information", and "pre-emptive Bayesianism" is floated.

Section 9 has two extensions:

* **Fortified signals** (9.1, pp.43-44). A repeated `s₁` is either not a signal
  at all, or it restores `s₁` to full weight.
* **"Peggy"** (9.2, pp.44-45; App. B, pp.60-62). She always gives the `t = 0`
  prior full weight and neglects only past signals. The paper says: "Almost all
  of our findings continue to hold in this alternative model" (p.21).

## Result

`lean/BenjaminBodohCreedRabin.lean` is a symlink to
`lean/Literature/BenjaminBodohCreedRabin.lean`. It is standalone on Mathlib. It
checks with `lake env lean` and contains no `sorry`. Its 41 theorems and lemmas
all use only the axioms `[propext, Classical.choice, Quot.sound]`. The
hypotheses throughout are a strictly positive prior and strictly positive
likelihoods on a finite `Θ`.

**The paper's model (Section 1 of the file)**

| Lean theorem | Content |
|---|---|
| `brn`, `brnIter` | the one-shot rule (p.2); the dynamic rule, in which the posterior becomes the prior (p.19) |
| `brnIter_closedForm` | eq. (5), proportional form: after `n` signals the posterior is `∝ p₀^{α^n} ∏_{k<n} ℓ_k^{α^{n-1-k}}` |
| `ratio_brnIter` | eq. (5) in the paper's ratio form |
| `logOdds_brn`, `logOdds_brnIter` | eq. (17) and eq. (6): the weight on signal `k` is `α^{n-1-k}`, and the prior's weight is `α^n` |
| `logOdds_uninformative` | uninformative signals leave `α^n l₀` (p.21) |
| `logOdds_bounded` | eq. (7), finite horizon: the signal part of the log odds is at most `L̄/(1-α)` in absolute value, for every `n` |
| `logOdds_brn2` | eq. (4) in logs: `α² l₀ + α l₁ + l₂` |
| `twoSignal_logOdds_order` | reversing two signals shifts the log odds by `(1-α)(l₂ - l₁)` |
| `twoSignal_order_iff` | for `α ≠ 1`, the two orders agree on a pair iff the likelihood ratios agree |
| `recency_two_signals` | recency: for `α < 1`, the more favourable signal moves belief more when it comes last |
| `brn_order_invariant_iff` | for `α ≠ 1`, the whole posterior is order-invariant iff the two likelihood functions are proportional |
| `extreme_moderation_iff` | Prop. 1's core: `z O^α < O ⟺ z < O^{1-α}` |

**The Paper B question (Section 2 of the file, not the paper's claim).** Take
the 2×2 joint, with a cue on A of likelihood `a_i` and a cue on B of likelihood
`b_j`. BRN is applied with the four cells as hypotheses.

| Lean theorem | Content |
|---|---|
| `assoc_brn_product` | one BRN step with a product-form likelihood multiplies the log odds ratio by `α` |
| `assoc_brnAB`, `assoc_brnBA`, `assoc_brn_orders_eq` | **(i)** after two steps the log odds ratio is `α² · assoc(P)` in either order, so it is identical across orders at every prior |
| `assoc_bayes`, `assoc_brnAB_vs_bayes` | `P^B ∝ P a_i b_j` keeps `assoc(P)`, so BRN gives `α² · assoc(P^B)` |
| `margOddsA_brnAB_indep`, `margOddsA_brnBA_indep` | at independence `P = u ⊗ v`: the A-marginal odds are `(a₀/a₁)^α (u₀/u₁)^{α²}` when A is read first and `(a₀/a₁)(u₀/u₁)^{α²}` when A is read last |
| `margOddsA_order_ratio` | the ratio of the two is `(a₀/a₁)^{α-1}`: the **earlier** cue is down-weighted |
| `margOddsA_orders_ne` | **(ii)** at independence, for `α ≠ 1` and `a₀ ≠ a₁`, the A-marginal differs between orders |
| `example_marginal_gap` | **(ii), explicit instance**: `α = 1/2`, uniform prior, `a = b = (49/50, 1/50)` gives `P(A=0) = 7/8` (A first) and `49/50` (A last). `P^B` is also `49/50` |

`sympy/check_brn.py` gives **55/55 PASS** (exit 0). It checks the following.

Part I, the paper:

* Eq. (6) symbolically for `t = 1..5`, and eq. (4).
* The order shift `(1-α)(l₂-l₁)`, eq. (7)'s geometric sum and eq. (8).
* Prop. 1 on a grid, Prop. 2 and Prop. 3 on exact instances, and Prop. 7's
  threshold on a grid.
* The worked numbers:
  * Kahneman-Tversky's 5.4 and 1.2, and the Cab problem's 41% (p.8);
  * Eddy's 32%, 86% and >99%, and the belief movements 0.27, 0.045, 0.85 and
    0.05 (pp.13-14);
  * Heidi/Tarso's 1/32, 15/31, 1/16, 31/46 = .674, and fn. 10's .270 (p.17);
  * eq. (9)'s 7/12 and 5/12 (p.25), Table 2 (p.27) and eq. (14) (p.29).
* Table 1 (p.11), re-derived from its own Median column.
* The five-employee example (p.39), by enumeration.

Part II, Paper B:

* Facts (i) and (ii) symbolically.
* The exact witness.
* The same facts in Paper B's own parametrisation, `prior(α, β, c)` with
  matched likelihoods `q_i/P(A=i)`. At `c = 0` the A-marginal differs between
  orders. At `c ≠ 0` the odds ratio is equal across orders and equals
  `OR(P)^{α²}`.
* The framing check in item 5 of "What formalizing revealed" below.

## What formalizing revealed

1. **The closed form is exactly eq. (5)/(6).** No hidden conditions are needed
   beyond positivity. `α^{n-1-k}` in 0-indexed form is the paper's
   `α^{t-τ}`.
2. **Order dependence is a two-line identity.** Swapping two signals shifts the
   log odds by `(1-α)(l₂ - l₁)`. So BRN is order-invariant exactly when the two
   likelihood functions are proportional (`brn_order_invariant_iff`). "Recency"
   in the paper's sense is the sign of this shift.
3. **Paper B (i): the association is order-blind but shrunk.** On the 2×2 joint,
   a likelihood that is a product of an A-part and a B-part multiplies the log
   odds ratio by `α`. Two cues give `α²`, in either order and at every prior.
   The Bayes benchmark keeps the prior's odds ratio. So under BRN the believed
   association equals `α² · assoc(P^B)`. It is identical across reading
   sequences, which is the same pattern as Paper B's Lemma SEP. But it is not
   equal to the benchmark's, and the gap `(α²-1) assoc(P^B)` is first order in
   `c`.
4. **Paper B (ii): a position channel with Bayes-factor inputs.** At independence
   the A-marginal odds are `(a₀/a₁)^α (u₀/u₁)^{α²}` when A is read first and
   `(a₀/a₁)(u₀/u₁)^{α²}` when A is read last. They differ whenever `α < 1` and
   the A-cue is informative. The cue read first enters with exponent `α` and
   the cue read last with exponent `1`: this is recency. The last-read marginal
   is not pinned to the Bayes value either. Its odds carry `(u₀/u₁)^{α²}` in
   place of `u₀/u₁`, so with matched likelihoods they are
   `(q₀/q₁)(u₀/u₁)^{α²-1}`, not `q₀/q₁`, unless the prior marginal is uniform.
5. **The position channel depends on the framing of the hypotheses.** This is
   the paper's own Section 2.3 point (pp.15-18). The results above apply BRN to
   the four cells of the joint. Suppose instead that BRN is applied to each
   attribute's partition, with the other attribute's conditionals carried over
   (a Jeffrey-type step whose marginal is the BRN posterior). Then there is
   **no** order effect at `c = 0`, and the odds ratio is preserved, not shrunk
   (sympy check (10)). The position channel at independence arises because the
   B-cue's update also neglects the A-information already in the joint prior.
6. **The channel also depends on the dynamic assumption.** If both cues are
   processed in one update (the paper's "pooling"/"clumping" alternatives,
   pp.20-21), BRN has no order effect at all.

## Bearing on Paper B

BRN is a published non-Bayesian rule whose inputs are likelihoods (Bayes
factors), not delivered credences. On Paper B's 2×2 joint with attribute-local
cues, applied to the joint cells and with the posterior serving as the next
prior, it does three things:

* It yields an order effect on the marginals **at independence**. This is a
  position channel, and it shows that the channel is not an artefact of
  Jeffrey-type (credence) inputs.
* It leaves the believed association **identical across orders**, as Paper B's
  Lemma SEP does.
* It **shrinks that association by `α²` relative to the Bayes benchmark `P^B`**,
  at every `c`. In Paper B's `ω` family, by contrast, the odds ratio does not
  move.

**Direction.** BRN down-weights the **earlier** evidence. The paper's own words
are "recency bias" (p.21) and "early signals eventually have no influence"
(p.3). The later cue enters at full weight. BRN therefore does **not** support
the prelude's phrase "the observer weights the later cue less"
(`notes/interior_omega.tex:64-65`). It supports the existence of a position
channel under likelihood inputs, but in the opposite direction to the damped
second cue of Paper B's `ω < 1` model.

BRN is also a counterexample to the prelude's next sentence, "Which channels
operate is fixed by how fully the later cue is adopted", if that sentence is
read as a general claim. Under BRN the later cue is adopted in full and the
position channel still operates. The sentence is correct only as a statement
about Paper B's model.

**What to cite, and how:**

* Cite it as a working paper (July 19, 2019). There is no bib entry yet in
  `bibliography.bib`.
* Quote the paper's own words (p.21 or p.3). "More recent messages are given
  more weight" is Ortoleva's paraphrase in his survey, not a sentence of this
  paper (`notes/citation_audit.md` M8(d) attributes it to "p. 557", which is
  the survey's page).
* If the position channel is illustrated with BRN, say that BRN is applied to
  the joint cells with posterior-as-prior updating. The order effect at `c = 0`
  disappears under per-attribute framing and under pooled updating (points 5
  and 6 above).
* The authors themselves note the order-of-presentation prediction in fn. 7
  (p.13): "our model might be interpreted as predicting that ... if the
  description comes first, then it would be treated as the subjects' priors
  and the description—rather than the base rates—would be downweighted". They
  decline to treat it as a clean test. They report that Krosnick, Li & Lehman
  (1990) and Chun & Kruglanski (2006) found base rates underused more when
  presented first, which is the recency direction. Borgida & Brekke (1981)
  found no relation.

**Suggested prelude wording (text only, not applied).** Replace "a
\textbf{position} channel, in which the observer weights the later cue less
whatever the attributes are \citep{HogarthEinhorn1992,Asch1946}. Which channels
operate is fixed by how fully the later cue is adopted." with:

> a \textbf{position} channel, in which the weight a cue receives depends on
> where in the sequence it arrives, whatever the attributes are. The position
> channel is not peculiar to cues delivered as credences: under base-rate
> neglect, where each posterior becomes the prior for the next cue and priors
> are under-weighted, the earlier cue is discounted and the marginals register
> the reading sequence even when the attributes are believed unrelated
> \citep{BenjaminBodohCreedRabin2019}. In the model studied here, which
> channels operate is fixed by how fully the later cue is adopted.

Proposed bib entry:

```
@unpublished{BenjaminBodohCreedRabin2019,
  author = {Benjamin, Daniel J. and Bodoh-Creed, Aaron and Rabin, Matthew},
  title  = {Base-Rate Neglect: Foundations and Implications},
  note   = {Working paper, July 19, 2019},
  year   = {2019}
}
```

(The PDF says "Dan Benjamin". The middle initial "J." is taken from Ortoleva's
reference list.)

## Audit findings (2026-09-30)

These are errors and slips in the paper itself, found while checking it. None
affects the model or Section 3.

* **Five-employee example, p.39 (Section 8.2; set up on p.20). Numerical
  error.** The first step is right: `p(a|H) = 11/16 = 0.6875` and
  `p(a|¬H) = 5/16`. The second step is not.
  * The paper plugs in `0.875` and `0.125` as the likelihoods of the second
    "agree". These are the posteriors `P(H|s₁ = s₂ = a)` and its complement.
    The correct likelihoods are `p(s₂=a|H, s₁=a) = 7/11` and
    `p(s₂=a|¬H, s₁=a) = 1/5`.
  * The extreme Saki's belief after two "agree" is therefore `35/46 ≈ 0.761`,
    not `0.875`.
  * Tommy's is `7/8 = 0.875`, not the printed `0.939`. The `0.939` is Bayes run
    with `0.875/0.125` as likelihoods on the `0.6875` prior, which counts `s₁`
    twice.
  * The same display writes `p(E|…)` for `p(H|…)`.
  * The qualitative point, certainty after the third "agree", holds.
* **Table 2, p.27.** The third row is labelled `(h,h)`. It should be `(t,h)`.
  The entries are right.
* **Table 1, p.11.** The Bayes value for prior 0.10 with 8 heads is 0.5586, and
  it is printed 0.55. Every other derived entry can be attained from the
  printed (two-decimal) medians. The priors printed 0.33 and 0.67 are `1/3` and
  `2/3`.
* **Footnote 10, p.17.** The value is 0.2707, printed as "0.270".
* **p.14.** Tommy's move after a negative test is 0.0442, printed as "0.045".
* **Proposition 2, p.16.** The labels are swapped relative to the text. The
  text defines `Θ₁ = {A,B,C}` and `Θ₂ = {A, B∪C}`, but the proposition and its
  proof (pp.52-53) use `Θ₂` for the fine partition and `Θ₁` for the coarse one.
  The inequality (fine > coarse) is correct.
* **Section 9.2, p.45.** Peggy's displays have two slips. They write `p(θ|s₁)`
  where the likelihood `p(s₁|θ)` is meant (as Appendix B's formula on p.60
  confirms). The two-signal posterior is also labelled `p_α(θ|s₁)`.
* **Eq. (14), p.29.** The denominator's `α^{t-i}` should read `α^{τ-i}`.
* **Citation note.** The quotation "more recent messages are given more
  weight", which has been attached to this paper, is not in it (see Bearing on
  Paper B).

## Not formalized

* Propositions 2-10, including hypothesis dependence, prediction momentum
  (Props. 4-5), learning traps, persuasion (Props. 6-7), reputation and
  ergodicity (Props. 8-9) and NBLLN (Prop. 10). Props. 2, 3 and 7 are checked
  numerically in sympy only.
* The infinite-past form of eq. (7), eq. (8) as an expectation, and the
  non-martingale remark (p.22).
* The prospective-belief completion (p.23), Section 9's fortified signals and
  Peggy, and Appendix B.
* The Paper B framing variant (point 5) and Paper B's own parametrisation.
  These are checked in sympy only.
