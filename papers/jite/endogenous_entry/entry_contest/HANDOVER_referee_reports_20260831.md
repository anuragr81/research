# Handover: BEJEAP referee reports (DGBEJEAP.2026.0180) — full record and rebuild directions

Status: manuscript rejected 31 Aug 2026, decision final (Editor-in-Chief
Till Requate). Both referees recommended reject. The education-policy paper
is retired. This document records **every** referee comment, marks which are
model-level and which were manuscript-level, and sets out directions for a
rebuilt model in a general labour-economics / theoretical framing.

Standing decisions already taken: no uniform distribution (general
distributions throughout; distribution-specific assumptions only where an
empirical or illustrative claim demands them, flagged as such); output for
now is a proofs document only, no prose paper.

---

## Part 1 — Reviewer 1, in full

Reviewer 1 summarised the model accurately (fixed-slot contest, one
high-income participant vs Q ≥ 1 low-income, entry cost nu buys access to
the social-capital component of the score, winner takes the high-income
position) and acknowledged the revision was clearer than the previous
version: the Spence-signaling framing was gone, and the scope restriction to
fixed-slot positional competitions was explicit. Both were called useful
changes. The judgement was nonetheless that the model remains too stylized
for the education-policy and inequality-dynamics conclusions, and that the
general-Q extension introduced a serious technical problem.

### R1 Main comment 1 — wrong deviations in the general-Q analysis (MODEL, fatal)

The Q low-income participants are treated as a single representative player.
That is not Nash logic for a finite-player game. In a Nash equilibrium one
low-income participant deviates while the other Q−1 hold their strategies
fixed. So in the exclusive-participation profile, a deviating low-income
participant who invests does **not** move the game to full participation —
she invests while the others still do not. Symmetrically, in the
full-participation profile, a deviating withdrawer still faces Q investing
opponents. The paper compares payoffs across aggregate regimes instead of
checking individual unilateral deviations.

Consequence, as stated by the referee: the general-Q equilibrium
classification, the participation thresholds, the comparative statics in Q,
and the mobility-ceiling result may not follow from the stated finite-player
game.

Remedy offered by the referee (two options): redo the analysis using standard
unilateral deviations, allowing profiles with **any number** of investing
low-income participants; or explicitly reformulate as a two-player game
between the high-income participant and a collective low-income group.

### R1 Main comment 2 — reach into education policy remains limited (SCOPE)

The scope restriction helped but does not remove the limitation. The model is
a zero-sum status contest: social capital affects only the probability of
winning a fixed position; it does not enter a production function, raise
aggregate output, benefit losing investors, or generate intergenerational
dynamics. Suitable for narrow positional competitions; a strong assumption
for a paper drawing conclusions about education policy and inequality.

The referee frames this as a scope problem: title, abstract, introduction and
policy section draw broad conclusions about mobility, education policy and
class barriers, but the model supports claims only about fixed-prize,
fixed-slot competitions where social capital affects rankings but not
productivity. Without further structure it cannot support claims about
education as human capital, aggregate output, intergenerational mobility, or
general labour-market inequality. These claims should be narrowed.

### R1 Main comment 3 — robustness beyond uniform distributions (MODEL)

The added robustness paragraph "states an intention rather than a theorem."
The closed forms for p_1, nu*, and the ceiling mu/[2(1−mu)] rely on the
uniform specification; the claimed extension of the binary equilibrium
structure and comparative statics to symmetric distributions is not proven.
The concern is sharper given comment 1, because the correct
unilateral-deviation probabilities involve objects such as
∫ F_X(x) G(x)^{Q−1} dF_X(x), whose behaviour may depend nontrivially on the
distributions of r and s. Either provide precise assumptions and a formal
result, or present the uniform model as a tractable example rather than a
generic theorem.

### R1 Main comment 4 — contribution to the literature (POSITIONING)

Engagement with related work improved, and the threshold-participation
mechanism is now distinguished from models of continuous investment
intensity. But the substantive contribution appears modest: the core
observation is that a threshold cost in a fixed-prize status contest can
generate participation and non-participation regimes. The paper should
explain why this is **not already captured** by existing models of contests
with entry costs, positional education, or status competition.

The mobility ceiling is also less general than claimed: it is an asymptotic
expression from the exclusive-participation probability formula, and it is
not clear that it reflects a robust economic principle rather than a property
of the uniform specification. The paper would be stronger presenting the
ceiling as a model-specific implication and centring the contribution on the
interaction between threshold costs and positional competition, rather than
as a broad structural limit on mobility policy.

### R1 Additional comments

1. The correction of the misleading "signaling" usage is a useful
   improvement. (Credit — no action.)
2. "Stable Nash equilibrium" is used repeatedly but stability is never
   defined. If pure-strategy Nash is meant, say so; if a refinement or
   dynamic stability notion is meant, define it.
3. The claim that exactly two stable Nash equilibria arise should be
   qualified — the model section also discusses full withdrawal as a Nash
   equilibrium for sufficiently high participation costs. Either restrict the
   claim to the relevant parameter region or present three regimes: full
   participation, exclusive participation, full withdrawal.
4. The mobility-ceiling claim should be stated more carefully. If the ceiling
   refers to the bound **within** the exclusive-participation regime, cost
   reductions do not affect it as long as the economy stays in that regime —
   but a sufficiently large cost reduction moves the economy to full
   participation, where the relevant bound changes. Abstract and policy
   discussion should use the term consistently.
5. The discussion of inequality should distinguish **prize effects** from
   **affordability effects**. Holding y_1 and nu fixed, an increase in y_2
   raises the prize u(y_2) − u(y_1) and can *strengthen* low-income
   incentives to invest. The force that prices out low-income participants is
   the budget/utility-cost burden of paying nu out of low income. These
   channels must not be conflated.
6. Phi(Q) notation is inconsistent: defined in the text as a probability
   surplus scaled by the utility gap and the discount factor, but Proposition
   2 and Table 3 treat it as an unscaled probability surplus. Either redefine
   as a pure probability object or carry the utility and discount factors
   throughout.
7. Some comparative statics w.r.t. mu and 1−mu appear misstated. Under the
   paper's own expression for A, increasing mu **lowers** A while increasing
   the social-capital weight 1−mu **raises** A. The text sometimes says the
   participation threshold is decreasing in the social-capital weight, the
   opposite of what the formula implies.
8. Figures improved, but the paper remains verbose. Move derivations to the
   appendix; focus the main text on the economic mechanism, the equilibrium
   conditions, and interpretation.

---

## Part 2 — Reviewer 2, in full

Reviewer 2 gave a detailed and accurate model summary (score
mu·r_i + (1−mu)·s_i; s_i = 0 for non-investors; fixed access cost paid in
period 1; incumbent richer, so concavity makes investment cheaper in utility
terms; symmetric strategies across the Q poor participants; EP / FP / FW
equilibria; nu* falling in Q and in mu; the ceiling result). General
assessment: **the question is interesting, but the analysis lacks clarity and
the arguments are often difficult to follow.**

### R2 Comment 1 — asymmetric equilibria (MODEL, fatal and constructive)

The analysis focuses on symmetric equilibria in which all poor households
either invest or do not. But there are also **asymmetric equilibria in which
only some poor households invest**.

The referee's illustration: take mu = 0, so only social-capital investors can
win. Incentives to invest fall as more others invest, since the win
probability declines. So there must be Nash equilibria where only part of the
poor households invest.

Conjectured comparative statics for general mu > 0 (these are effectively a
research programme for the rebuild — each is a claim to prove or refute):
- more participants invest as the prize y_2 − y_1 rises;
- more invest as the investment cost nu falls;
- more invest as social capital becomes more important (mu falls);
- effect of Q on the number investing depends on mu: at mu = 0 the gains are
  divided among investors only, so the equilibrium number of investors is
  **independent of Q**; at mu > 0, untalented-but-lucky non-investors can
  win, so an increase in Q shrinks the pie distributed among investors and
  **fewer** invest in equilibrium.

Consequence for the equilibrium check: for an exclusive-participation
equilibrium to exist it must be verified that no poor household wants to
deviate and become **the only** poor investor — not merely that poor
households do not want to jointly deviate to full participation (as equation
8 does). There would then be several intermediate equilibria between
exclusive and full participation, each with its own participation threshold,
making policy-parameter effects **smoother** rather than sharply regime-switching.

Open question the referee raises: applying the same asymmetric logic, it may
also be possible to obtain an EP equilibrium in which one poor household
invests while the incumbent does not — even under concave utility where the
incumbent has the cost advantage.

### R2 Comment 2 — the effect of inequality on status expenditure (MODEL, fatal)

The abstract claims wider income gaps simultaneously strengthen the
incumbent's investment incentive and price out challengers. The referee does
not see where this is proven or what it means.

- An increase in y_2 − y_1 raises the contest prize, which should raise
  investment incentives for **poor participants too**, not only the incumbent.
- If y_2 − y_1 widens because y_1 **falls**, poor households may stop
  investing because they cannot afford to — so total investment may **fall**.
- Or is the argument that a higher y_2 makes a previously non-investing
  incumbent start to invest? This needs clarifying.

Deeper objection: talking about the effect of inequality on status
expenditure is **misleading when y_2 − y_1 is literally the contest prize**.
The related literature cited in the introduction analyses how ex-ante
inequality in **endowments** affects incentives to invest in status, taking
the status reward as given. In that formulation, the referee expects the
standard result — larger inequality **reduces** total status expenditure — to
hold in this setting too.

### R2 Comment 3 — policy implications without a policy objective (MODEL/WELFARE)

Policy is discussed without first specifying a clear objective. In the model
the contest is zero-sum and the prize does not depend on the winner's talent
or social capital, so **social-capital expenditure is costly but socially
inefficient**.

- One natural objective: minimise total social-capital expenditure.
  Policies achieving zero expenditure: set mu = 1 (social capital no longer
  rewarded), or raise nu high enough that nobody invests. Neither is
  addressed, and the paper appears to treat broader investment in social
  capital as a positive thing — which needs motivating.
- Another natural objective: select the highest-raw-talent winner. Again
  mu = 1 or sufficiently high nu is optimal. Explain why these are not
  considered optimal or attainable.
- If it is reasonable that mu = 1 or prohibitive nu are unattainable in the
  real world, other policies remain interesting — but the analysis needs a
  clear benchmark (total spending on social capital; or waste of raw talent
  when the winner is not the most talented). Such analysis would be more
  interesting under asymmetric equilibria (Comment 1).

### R2 Comment 4 — mobility ceilings and limits to growth (MODEL)

The emphasis on the ceiling mu/[2(1−mu)] is misplaced, for three reasons:

1. **It is an artifact of the assumptions.** At mu = ½ the incumbent drawing
   both r and s from U[0,1] has average score ½ and wins with probability ½
   against a poor participant who (selected from infinitely many) has maximal
   talent 1 and hence score ½. This is arithmetic from the uniform supports,
   not a major insight.
2. **The limit is inconsistent with equilibrium.** Increasing Q to infinity
   when mu > 0 is inconsistent with the existence of an equilibrium in which
   the incumbent invests in social status, so analysing large Q is
   meaningless. The same holds in the full-participation equilibrium.
3. **The comparative-static setup is artificial.** Letting the number of
   participants grow while the prize and investment costs stay fixed is very
   specific and not obviously matched to real-world examples. Talking about
   "limits to growth" in a zero-sum contest with a fixed prize is a stretch.

### R2 Comment 5 — modelling assumptions (MODEL/DESIGN)

The model is overly complex in places and could be more general in others:

- A **static one-shot model** would be simpler than the dynamic setting on
  p.8. Consider discussing symmetric participants first and introducing
  asymmetries later. Rather than giving the incumbent higher first-period
  income, simply assume the incumbent has **lower investment costs**, without
  concavity of utility doing the work.
- **Financial constraints are not crucial** — the main insights come from
  investment incentives. Consider a setting without financial constraints
  first, then financial constraints as an extension.
- Consider a **reduced-form contest success function** with win probabilities
  not micro-founded.
- It is unclear why **mu < ½** is needed. Would the main results not survive
  mu > ½?

### R2 Comment 6 — exposition (MANUSCRIPT, but with model content embedded)

Main results should be stated as precise propositions followed by proofs
(currently the only proposition concerns an equilibrium that does not exist
under standard assumptions). Propositions should state, for exogenous
parameters in a given range, how a change in a particular exogenous variable
affects a particular endogenous variable. Currently there is loose verbal
discussion, sometimes changing several variables at once, without checking
for which parameter values an equilibrium exists or when it switches. Verbal
discussion (including of the literature) is often imprecise, and the link
between mathematics and verbal analysis is hard to follow.

Specific errors listed (each verified as a genuine defect):
- p.5: "a participant abstains and saves instead" — but **saving is not
  allowed in the model**.
- p.6: mathematical parameters (p_1 …) used before their meaning is explained.
- p.9 middle: mathematical derivations need better explanation.
- p.10, Proposition 1: "the marginal utility cost of nu-bar is higher" —
  what are marginal utility costs?
- p.11 top half: the passage on the probability surplus narrowing and the IP
  window tightening, and the mu → 0 statement, is unclear and **contains
  mistakes**.
- p.14 middle: "a uniform upward shift in both incomes that leaves the ratio
  constant has no effect on the participation threshold" is **not correct
  given equation 20** (nu* scales with y_1).
- p.14 last paragraph: appears to contain a mistake.
- p.15 middle: "the corresponding minimum income ratio" — how is it defined,
  and where does it appear in the table?
- p.16 middle: why does the high-income participant "free ride on raw
  talent"? Why does the poor participant "over-invest"?
- p.17 middle: "access expansion without competition redesign reinforces
  elite-only equilibria" — **why is this bad if the incumbent is replaced
  with higher probability?**
- p.17 bottom: "it raises competitive intensity" — meaning unclear.
- p.18 middle: "as if it also removes budget constraints" — meaning unclear.
- p.18 bottom: "reference-dependent equilibrium" — undefined.
- p.19 top: the Sutton Trust corroboration — **doesn't the model predict a
  higher probability that poor participants win when Q increases?** (i.e. the
  cited evidence may contradict rather than support the model.)
- p.19 bottom: "move the budget constraint from binding to non-binding" —
  what matters are investment incentives, not only whether a household is
  financially constrained.
- p.18: the conclusion should be much shorter.

---

## Part 3 — Consolidated model-level findings

Stripping out everything that was about the manuscript, six findings bear on
the model itself. Ranked by severity.

1. **Wrong equilibrium concept (R1.1, R2.1).** Aggregate-regime comparison
   instead of unilateral deviation. Invalidates the equilibrium
   classification, nu*, comparative statics in Q, Phi(Q), and the ceiling.
   The true equilibrium object is an interior number k* of investing
   challengers, not a binary all-in/all-out.
2. **Prize/affordability conflation (R1.5, R2.2).** Because y_2 − y_1 is
   simultaneously "inequality" and the contest prize, "inequality raises
   status expenditure" is near-tautological, and the prize effect operates on
   poor participants too. The pricing-out force is the utility cost of nu out
   of low y_1 — a distinct channel that must be separated. Against a fixed
   reward and endowment inequality, the standard result (inequality reduces
   expenditure) may well hold.
3. **No welfare benchmark (R2.3).** Zero-sum contest ⇒ social-capital
   spending is waste ⇒ mu = 1 or prohibitive nu are first-best. Any policy
   claim needs an objective and an argument for why the corner solutions are
   unattainable.
4. **Ceiling is an artifact (R1.4, R1.add.4, R2.4).** Uniform-support
   arithmetic; Q → ∞ inconsistent with the incumbent's participation
   constraint; fixed prize with unbounded entry is not a meaningful limit;
   and the term is used inconsistently across regimes.
5. **Contribution not located (R1.4).** Must establish why threshold entry in
   a fixed-prize contest is not already covered by contests-with-entry-costs,
   positional-education, or status-competition literatures.
6. **Distributional generality (R1.3).** Results asserted beyond uniform
   without proof. Now moot by the standing decision to work distribution-free,
   which converts this from a defect into the default.

Also to carry forward as definitional hygiene: define "stable Nash
equilibrium" or drop the word (R1.add.2); admit full withdrawal as a third
regime (R1.add.3); fix or retire Phi(Q)'s scaled/unscaled inconsistency
(R1.add.6); and re-derive every comparative static sign, since at least one
was demonstrably backwards (R1.add.7).

---

## Part 4 — Directions for the rebuilt model

Framing target: general labour economics / pure theory, possibly JITE. No
education-policy claims. Proofs document only for now.

### Primitives (proposed)

- Prize V > 0, **exogenous and fixed**, decoupled from the wealth
  distribution. This is the fix for finding 2.
- N = Q + 1 players: an incumbent with wealth w_0 and Q challengers with
  wealth w < w_0. Static, one-shot (per R2.5).
- Entry choice e_i ∈ {0,1} at nominal cost c; draws r_i, s_i iid from
  **general** distributions; score mu·r_i + (1−mu)·s_i·e_i; highest score wins.
- Two induced score distributions: G for non-investors (mu·r), F for
  investors (mu·r + (1−mu)·s). All results written in F and G only.
- Affordability channel: the same nominal c costs u(w) − u(w−c) in utility,
  decreasing in w by concavity. Inequality now enters through **endowments
  alone**. Alternative per R2.5: give the incumbent a lower cost directly and
  drop concavity — worth doing as a robustness variant to show which results
  need which assumption.

### Claims to attempt, in dependency order

1. F first-order stochastically dominates G, strictly if s has positive mass
   on (0, ∞). Replaces the whole p_1 < 1/(Q+1) < p_1' apparatus,
   distribution-free.
2. Win probability for an investor facing m other investors and n
   non-investors is ∫ F^m G^n dF; for a non-investor, ∫ F^m G^n dG. This is
   the unilateral-deviation object the old model never computed (R1.1, R1.3).
3. Both are strictly decreasing in m: entry is a negative externality on
   entrants; strategic substitutes.
4. Let Delta(k) be a challenger's utility gain from investing when k others
   invest. If Delta is decreasing in k, the entry game has threshold
   structure and a pure-strategy equilibrium in the **number** of investors
   exists. This is the linchpin — everything after depends on it.
5. Characterisation: k* pinned by Delta(k*−1) ≥ u(w) − u(w−c) > Delta(k*).
   Interior k* for intermediate c; boundary cases recover full entry and no
   entry (this is also R1.add.3's three regimes, now derived rather than
   asserted). Check whether a symmetric mixed-strategy equilibrium coexists.
6. Comparative statics on k*: nondecreasing in V, nonincreasing in c,
   nondecreasing in w. These are R2.1's conjectures. Expect the first two by
   monotone comparative statics; the third should need concavity explicitly.
7. mu = 0 case: non-investors score zero, investors split the prize 1/k,
   k* **independent of Q**. Cheap to prove and a good test of whether the
   machinery in 4–5 is right. Directly from R2.1.
8. mu > 0: k* nonincreasing in Q. Expect sign-ambiguity in general, needing a
   hazard-rate or single-crossing condition relating F and G. **If a
   condition is needed, the condition is the result.**
9. **Collapse threshold, replacing the ceiling.** For fixed V, c, w there
   exists finite Q-bar such that Q > Q-bar implies k* = 0 — and possibly a
   further threshold beyond which even the incumbent abstains. This is the
   honest version of what the ceiling was gesturing at, and it answers R2.4:
   no asymptotics, no ceiling, all content at finite Q. Any asymptotic
   statement, if ever wanted, must scale V, c, or the number of positions
   with Q.
10. Incumbent's role: with w_0 > w she has a lower utility cost, so an
    equilibrium with the incumbent investing and few challengers investing
    should exist for a range of c. Then test R2.1's open question — whether
    the reverse configuration (incumbent abstains, one challenger invests)
    survives under concavity now that deviations are unilateral.
11. **The inequality result, done properly.** Hold V and c fixed; apply a
    mean-preserving spread to the wealth distribution; determine what happens
    to aggregate expenditure k*·c. This is the paper's real empirical claim
    and now a theorem to prove or refute rather than an artifact. Note
    honestly: it is entirely possible the threshold does **not** reverse the
    standard result (R2.2 expects it will not), in which case there is no
    paper and that is the finding.
12. **Welfare section (R2.3).** State the objective explicitly — minimise
    wasteful expenditure k*·c, or minimise talent misallocation
    P(winner is not the highest-r player). Show mu = 1 and prohibitive c as
    first-best corners, then argue what makes them unattainable, then compare
    interior policies. For a pure-theory framing it is legitimate to
    characterise the equilibrium set and treat welfare as one section rather
    than the paper's burden.

### Open design decisions (needed before proofs start)

- Does the prize enter as money added to wealth — so concavity makes the rich
  value it less, a countervailing force worth having — or as a separable
  utility term (cleaner, weaker)?
- What minimal regularity on F and G is assumed? Continuity, common support,
  atomlessness — decide deliberately rather than by default.
- Incumbent asymmetry via wealth-and-concavity, or via a directly lower cost
  (R2.5)? Possibly both, as main model and robustness variant.
- Keep or drop the mu < ½ assumption (R2.5 questions whether it is needed).

### Step zero, before any of the above

Literature check on contests with endogenous entry / entry costs (R1.4). The
plausibly novel element is the **interaction**: income heterogeneity plus
concave utility makes an identical nominal entry cost asymmetric in utility
terms, so *who* enters is determined by the wealth distribution rather than
by ability. Whether that combination is genuinely unoccupied determines
whether the rebuilt model has a reason to exist. Do this before investing in
proofs 4–11.

### Suggested order of work

Claims 1–3 first: foundational, cheap, and if any fails the structure is
wrong — better to know in an hour than a month. Then claim 7 as the cheapest
correctness test. Then claim 4, which is where the real difficulty is. Then
5–6, 8–10. Claim 11 last, and expect it to be the one that decides whether
there is a paper.
