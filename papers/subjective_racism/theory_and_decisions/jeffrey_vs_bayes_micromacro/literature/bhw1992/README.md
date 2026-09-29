# Bikhchandani, S., Hirshleifer, D. & Welch, I. (1992), "A Theory of Fads, Fashion, Custom, and Cultural Change as Informational Cascades"

*Journal of Political Economy* 100(5), 992-1026. (Drive copy: JSTOR scan, stable
URL 2138632. All 36 PDF pages read, including the Appendix and Table 2.)

## Claims formalized

The specific model of Section IIA (pp.996-999): `V ∈ {0,1}` with prior 1/2, cost
`C = 1/2`, binary signals with `Pr(H | V=1) = Pr(L | V=0) = p > 1/2` (Table 1), each
individual seeing all predecessors' actions and not their signals. Tie-break (p.996):
an indifferent individual adopts or rejects with probability 1/2.

* **The counting characterisation** (pp.996-997; restated in the proof of Result 3,
  pp.1005-1006; footnote 23, p.1009): with `d = #adopt - #reject`, the individual
  follows his signal at `d = 0`, adopts on `H` and flips a coin on `L` at `d = 1`
  (mirror at `d = -1`), and ignores his signal once `|d| ≥ 2`.
* **DEFINITION** (p.1000): "An informational cascade occurs if an individual's
  action does not depend on his private information signal." Consequence (p.1000):
  "If an individual i is in a cascade, then his action conveys no information", so
  the cascade "once started will last forever".
* **Eq. (1)** (p.997), **eqs. (2)-(3)** (p.998): the probabilities of an UP cascade,
  no cascade and a DOWN cascade after `n = 2m` individuals, unconditional and given
  `V = 1`. The p.997-998 claim that no cascade after 10 individuals has probability
  below 0.1 percent even for `p = 1/2 + ε`. The limit plotted in Fig. 1.
* The algebra of **Result 4**'s proof (pp.1020-1023): (A6), (A10) = .488 and
  (A11) = .0935 at `p = .9`.
* Numerically: **Result 2** (Tables A1-A5) and **Table 2** (p.1008).

## Result

`lean/BHW.lean` (symlink to `lean/Literature/BHW.lean`; checked with
`lake env lean Literature/BHW.lean`, no `sorry`, headline theorems use only
`[propext, Classical.choice, Quot.sound]`):

* `lik_ratio`: for every action history, the public likelihood ratio
  `Pr(h | V=1) / Pr(h | V=0)` depends on `h` only through `d`. It equals `1`,
  `p/(1-p)` and `p(1+p)/((1-p)(2-p))` at `d = 0, 1, ≥ 2`, with reciprocals for
  negative `d`.
* `counting_rule_is_bayes`: for every history of positive probability and each
  signal, the counting rule gives the Bayes-optimal adoption probability with
  the paper's tie-break. That is 1, 0 or 1/2 as the posterior is above, below or
  equal to `C = 1/2`.
* `cascade_ignores_signal`: the DEFINITION holds once `|d| ≥ 2`.
* `cascade_uninformative` and `cascade_uninformative_down`: the cascade action
  multiplies both likelihoods by 1, the opposite action has probability 0, and
  the next posterior is unchanged.
* `cascade_forever`: after any number of further individuals, `d` stays `≥ 2` and
  the likelihoods do not move.
* `informative_outside_cascade`: for `|d| ≤ 1` the adopt probability differs
  across `V`.
* `eq3`: eqs. (2)-(3), and their mirror image given `V = 0`, by induction over
  pairs on the five-regime chain `dist`. The chain's transition probabilities are
  the counting rule's `actProb`.
* `eq1`: eq. (1).
* `no_cascade_after_ten`: the bound holds for every `p ∈ [0,1]`, since `p - p² ≤ 1/4`
  gives `(1/4)^5 = 1/1024`.
* `tendsto_correct_cascade`: `Pr(UP | V=1) → p(p+1)/(2(1-p+p²))`.
* `A6_posterior_up`: `Pr(V=1 | UP started at pair k+1) = p(p+1)/(2(p²-p+1)) > 1/2`,
  from the chain's increments.
* `A10_closed_form`, `A10_value`, `A11_simplify`, `A11_value`.

`sympy/check_bhw.py` (83 checks, all pass, exit 0):

* **Brute-force exact Bayesian agents.** Each agent computes the posterior from the
  exact likelihood of the observed history under the equilibrium itself. The
  resulting rule coincides with the counting rule on every history up to 10
  agents, for `p ∈ {3/5, 3/4, 9/10}`. This is independent of the Lean
  proof and also covers the history-to-chain lift that the Lean file does not
  formalize.
* Eqs. (1)-(3) are reproduced exactly, both from the enumeration and symbolically
  in `p` for `n ≤ 10`.
* Footnote 11, the 0.1-percent claim, and Fig. 1 monotonicity all check out.
* (A6), (A10) and (A11) check out, including .488, .0935 and "87 percent higher".
* Result 2's conclusion is reproduced: the ex ante profit is .0425 without the
  release and .03124 with it, so the release hurts individual 2.
* **Table 2** and **Tables A3/A5**: see below.

## What formalizing revealed

**BHW's cascade is a consequence of a coarse action, and coarseness loses
information before the cascade too.** `lik_ratio` shows the public likelihood
ratio at `d = 2` is `p(1+p)/((1-p)(2-p))`, strictly less than the ratio of two
`H` signals, `(p/(1-p))²`. The second adopter at `d = 1` pools `H` with
"`L` and the coin said adopt". So a binary action is not a sufficient statistic
even outside a cascade. The cascade (`|d| ≥ 2`) is the extreme case, in which the
action reveals nothing: `cascade_uninformative` multiplies both likelihoods by
exactly 1.

**"Conveys no information" is a theorem, not the definition.** BHW define a cascade
by signal-independence of the action (`cascade_ignores_signal`). Uninformativeness
(`cascade_uninformative`) follows from it together with the i.i.d. signals. The
two are separate statements in the Lean file.

**The sequence effect is a population-level filter.** A single Bayesian who saw all
the signals would have an order-invariant posterior. Order matters in BHW only
because each agent sees coarse actions, not signals. For example, the signal
sequences `H,H,L,L` and `L,L,H,H` give opposite cascades from the same multiset of
signals (checked in `sympy/check_bhw.py`).

**Arithmetic slips in the paper (none affect its conclusions):**

* Table 2 (p.1008), "no public information" columns. These are not the exact
  `n → ∞` values of eq. (3). For example, at `p = .55` the table gives .5645 where
  eq. (3) gives .5664, and at `p = .85` it gives .9065 where eq. (3) gives .9011.
  The largest gap is .0054. The columns look simulated. The p.1007 text value
  ".81" at `p = .75` agrees with eq. (3) (.8077).
  * The table also does not use the adopt-when-indifferent tie-break that footnote
    22 (p.1006) announces for Section III.A.3. That rule would give
    `p/(1-p+p²)`, which is .731 at `p = .55`.
* Table A3, row (R, x2): the posterior .3839 should be .3889 (= 7/18).
* Table A5, rows (L, R, ·): the unconditional probabilities .14176, .2847 and .0339
  should be .141, .2876 and .0342. The published column sums to .99766.
  * The expected profit with the release is .03124 exactly, not .03114. Either way
    it is below .0425, so Result 2 stands.
* Result 4 proof (p.1023). The final display multiplies the second term by
  `Pr(UP before 101)` where `Pr(DOWN before 101)` is meant. This is harmless by
  (A5). The bound is `≥ .0935` (exact .093516), although Result 4 says "greater
  than".

## Bearing on Paper B

`PAPER_B_MANUSCRIPT.tex` lines 254-267 cite BHW for four things:

* herd behaviour and cascades "in which sequence matters";
* "In \citet{BHW1992}, on the other hand, a cascade is an action that conveys no
  private signal";
* the contrast with Banerjee: "the sequence persists because a coarse public action
  fails to be a sufficient statistic";
* "the sequence effect in \citep{BHW1992, Banerjee1992} lives on the analogue of
  the marginals".

1. **"a cascade is an action that conveys no private signal".** The claim is
   supported, but it merges definition and consequence. BHW define a cascade as
   an action that does not depend on the private signal (p.1000). That it conveys
   no information is what `cascade_uninformative` proves.
2. **"on the other hand".** This is not supported. The formalization shows the
   mechanism is the same one the manuscript gives Banerjee: a coarse public action
   that is not a sufficient statistic (`lik_ratio`), with the cascade as the limit
   in which it conveys nothing. The descriptor "coarse" in fact fits BHW better
   than Banerjee. BHW's action is binary. Banerjee's action space is the same
   continuum as his signal space (see `literature/banerjee1992/README.md`). BHW say
   it themselves on p.1002 and in fn 17: with a continuum of actions behaviour
   converges, and Banerjee's incorrect cascades "derive from a degenerate payoff
   function".
3. **"lives on the analogue of the marginals".** This has no referent in BHW. The
   state is the single binary `V`. Every object in the model (`lik`, `post`,
   `dist`) is a function of that one variable, so there is no joint or
   association structure for the sequence effect to avoid. At most, the claim is
   vacuously true.
4. **Proposed corrected wording** (text only, not applied):

   > \citet{BHW1992} and \citet{Banerjee1992} study informational cascades and herd
   > behaviour, in which the order of moves matters: agents observe predecessors'
   > actions but not their signals. In both, the equilibrium action is not a
   > sufficient statistic for the agent's private information (a binary action in
   > \citet{BHW1992}; in \citet{Banerjee1992}, a decision rule that maps a wide range
   > of signals to the same option), and a cascade---an action that does not depend
   > on the private signal \citep[p.~1000]{BHW1992}---therefore conveys no
   > information, so later signals are no longer aggregated.

   For line 266-267, either delete the sentence or mark it as the authors' analogy:

   > In both models the unknown state is a single variable, so their order effects
   > are, by analogy only, effects on a marginal belief; there is no cross-attribute
   > association for them to act on.

## Audit findings (2026-09-29)

Against `verify_updating_theory.md` §3 (H1-H5):

* **H1 (VERIFIED):** agreed.
* **H2 (VERIFIED-WITH-CAVEAT):** agreed, and sharpened. `cascade_ignores_signal`
  and `cascade_uninformative` separate the definition from its consequence.
  `lik_ratio` shows that the binary action already fails to be a sufficient
  statistic before the cascade (at `d = 2` the ratio is `p(1+p)/((1-p)(2-p))`,
  not `(p/(1-p))²`). This is exactly the mechanism the manuscript assigns to
  Banerjee "on the other hand".
  * One qualification to the audit's framing: the audit says both papers share a
    coarse-action mechanism. More precisely, the action is coarse in BHW, while in
    Banerjee it is the decision rule that is non-invertible, because of the
    discontinuous payoff. The shared mechanism is the failure of a sufficient
    statistic, not coarseness as such.
* **H3 (VERIFIED):** agreed. `lik p v (a :: h) = lik p v h * actProb p v (diff h) a`
  is the feed-forward externality.
* **H4 (UNSUPPORTED):** agreed. The formal model has a single scalar state.
* **H5:** not re-checked (bibliographic).
* **New, not in the audit:** the slips in Table 2, Tables A3/A5 and the Result 4
  display listed above. They affect none of BHW's conclusions and none of the
  manuscript's uses.

## Not formalized

* Section IIB, the general model: the inference sets `J_i` and Proposition 1
  (cascades start almost surely under MLRP, a strong-law argument).
* Result 1 (fashion leaders).
* Result 2 and Result 3, which are only checked numerically.
* Propositions 2 and 3.
* Section V's fads model beyond the displayed algebra of Result 4.
* Table 2's public-information columns. Their release process is not specified
  precisely enough to reproduce.
* The lift from the five-regime chain `dist` to sums of `lik` over action
  histories. The sympy enumeration checks it instead.
