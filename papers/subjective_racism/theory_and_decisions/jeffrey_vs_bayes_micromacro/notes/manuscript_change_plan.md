# Manuscript change plan (v2): two-horn motivation + question/answer, compacted

Before -> after report for `PAPER_B_MANUSCRIPT.tex`. **No changes have been made
to the manuscript.** Every AFTER block is paste-ready LaTeX. Line numbers refer
to the current manuscript (1231 lines).

This plan supersedes `notes/two_horn_motivation_body.tex` and
`notes/question_and_answer.tex` (two-horn -> Change 4; question/answer ->
Changes 1, 2, 3, 5, 6). Those two files are drafting history and should not be
edited further; anything destined for the manuscript belongs in a Change here. New citation key: `Hawthorne2004` (entry in Change 7).

## What the edits achieve

1. **Changes 1-3 put the right question at the front.** The literature answered
   when two Jeffrey updates commute. The paper answers which measurable
   summaries of the belief show the difference when they do not. The intro
   currently frames the gap as one about aggregation, which the paper itself
   proves is inert. These changes replace that framing.
2. **Change 4 justifies the modelling premise in one place.** A cue can be read
   as a level or as a Bayes factor. An evaluator can report a level. A Bayes
   factor needs a counterfactual likelihood an impression does not supply. So
   impressions deliver levels, Jeffrey updating is how a level is coherently
   adopted, and the benchmark is the same cue under the other reading. The
   order effect is the difference between the two readings. Change 4 also
   states the second premise, full adoption, and names the adoption weight
   $\omega$, which Proposition ADJ recovers from the marginals.
3. **Changes 5-6 state the answer together with its instrument.** Marginals and
   the share of decisions changed show the sequence effect at first order. The
   association and the surplus-weighted loss do not.
   The test is a comparison of the two reading groups with each other. It needs
   neither the benchmark nor the sequence mix. Proposition ORD is that
   statement.

Changes 14--15 retitle Sections 4 and 5 and re-open them on the statistic
rather than the level of aggregation (cap of thirteen lifted 2026-09-20). The
Table 1 redesign remains parked.

---

## Change 1 -- Abstract (line 61): middle sentences

**Purpose.** The current sentence "an arbitrary statistic is an order of
magnitude closer to the benchmark" is false as stated. Only protected
statistics are. Replace it with the actual finding.

**BEFORE** (sentences 3-7 of the abstract):

> The paper argues that since the two cue-sequences displace a belief only
> within a two-dimensional plane, an arbitrary statistic is an order of
> magnitude closer to the benchmark. Thus while measures of the believed
> association and the average value lost through decisions differing from
> benchmark stay close to the benchmark, the share of the population affected
> by the reading sequence differs. The paper discusses two major implications
> of this finding for audits. First, an aggregate conforming to the benchmark
> is no evidence that reading sequence does not affect evaluator beliefs.
> Second, whether the effect of order is detectable or not depends on the kind
> of question being asked.

**AFTER**:

```latex
The two reading sequences displace a belief only within a two-dimensional
plane, so whether a statistic registers the order effect depends on the
statistic alone. The classification is decided before any averaging over
evaluators, and it can be tested by comparing evaluators who read the cues in
opposite sequences, without reference to the benchmark or to the mix of
sequences. The believed association between the attributes hides the effect.
The marginal probabilities and the share of decisions changed by reading
sequence show it. Two implications follow for audits. A population whose
believed association matches the benchmark may still hold sequence-dependent
beliefs. And whether the effect is detectable depends on which statistic is
read, not on how many evaluators are averaged.
The results take each impression to be adopted in full rather than in part;
the weight placed on the later impression is recovered from the two marginals
read in both sequences rather than assumed, from which marginal ignores the
prior association and from the size of the sequence effect at independence.
```

---

## Change 2 -- Intro paragraph 1 (line 74): full replacement

**Purpose.** Replace the aggregation framing of the gap with the correct
contrast. The commutation literature answers a binary question, equality of the
two beliefs, which means agreement in every statistic at once. Observation is
per-statistic at finite precision, so the operative question is by how much the
two sequences disagree in each statistic. In the 2x2 model the marginals and
the association are not summaries of the belief but its coordinates; the point
is not that observers see less than the joint, it is that the difference is
spread unevenly across the coordinates. State that question, then the answer in
substance.

*On the novelty claim:* the weak form, "has received little attention", is used
deliberately in place of "the literature has not asked that question". The
strong form is a negative claim about an entire literature. Nothing among the
sources read directly -- Diaconis-Zabell, Field, Garber, Wagner (2002, 2003),
Hawthorne, Doring, Pettigrew-Weisberg, Asch, Hogarth-Einhorn, Domotor, Heckman,
Bohren-Imas-Rosenberg -- asks how far two reading sequences disagree *in each
statistic*, but that is not a proof of absence, and a referee is entitled to
name a counterexample.

**BEFORE** (the whole paragraph):

> That the sequence in which evidence arrives can move the final judgment is
> among the oldest findings in the study of impression formation
> \citep{Asch1946} yet there seems no general account of how such order
> dependence carries over in aggregate statistics. The current paper builds
> upon the belief-updating literature \citep{Asch1946,HogarthEinhorn1992} to
> explore how the sequence in which two cues arrive matters for average beliefs
> about correlated attributes in the population. Using Jeffrey conditioning as
> a coherent updating mechanism for impression-formation, the paper shows that
> while the share of population for whom the arrival sequence of evidence
> matters -- does deviate from the sequence-invariant benchmark -- the believed
> association between attributes stays close to the benchmark. The paper then
> pins down what really makes an arbitrary smooth statistic remain first-order
> and what makes it remain closer to the order-independent Bayesian updating
> mechanism. While the paper uses a two-cue setting to explain the underlying
> mechanism behind order effects, the central mechanism of how an
> attribute-local updating leaves the interactions among attributes untouched
> holds for any number of attributes and cues.

**AFTER**:

```latex
That the sequence in which evidence arrives can move the final judgment is
among the oldest findings in the study of impression formation
\citep{Asch1946,HogarthEinhorn1992}. The formal counterpart is also settled.
Jeffrey conditioning on successive uncertain impressions depends on their
sequence, \citet{DiaconisZabell1982} characterised exactly when the two
sequences lead to the same belief, and \citet{Hawthorne2004} named its full-adoption
premise the Amnestic Update-Factor Thesis, crediting the commutation criterion
to them. The criterion decides an
all-or-nothing question. Two sequences commute when the updated beliefs agree
on every statistic at once. When the two beliefs differ, the difference has a size in each
statistic, and the sizes need not be alike. In the two-attribute setting of
this paper a belief has three coordinates, the two marginal probabilities and
the cross-attribute association, and an observation reads one of them, or a
decision derived from them, at some finite precision. The question that matters
for observation is therefore not whether the two sequences agree but by how
much they disagree in each statistic. That question has received little
attention. The answer of this paper is that the marginal probabilities and the share of
decisions changed by sequence carry the difference at first order in the prior
covariance, while the believed association and the surplus-weighted loss carry
it only at second order. The paper
characterises exactly which smooth statistics fall on each side. The two-cue
setting serves to exhibit the mechanism; the underlying fact that
attribute-local updating leaves the interactions among attributes untouched
holds for any number of attributes and cues.
```

---

## Change 3 -- Intro paragraph 2 (lines 76-79): final two sentences only

**Purpose.** The paragraph's closing question should ask in which statistics
the sequence stays visible, not whether it matters "in the aggregate".

**BEFORE** (last two sentences):

> Whether arrival sequence matters or not in the aggregate is the question that
> the current paper attempts to answer. What it finds is that the answer
> depends on a geometric property of a given statistic rather than the means of
> aggregation.

**AFTER**:

```latex
Whether the arrival sequence remains visible, and in which statistics, is the
question the current paper answers. The answer turns on a geometric property of
the statistic being read. Aggregation neither creates nor removes the effect, and the
population average inherits whatever the two sequences already fixed.
```

---

## Change 4 -- Intro paragraph 3 (line 81): full replacement (compacted two-horn)

**Purpose.** State the modelling assumption (impressions arrive as revised
credences), then defend it. A cue admits two readings, level and Bayes factor;
the Bayes-factor reading needs a counterfactual likelihood an impression does
not supply; strict conditionalisation fails separately because no proposition
is certified. Jeffrey updating is the coherent way to adopt a level. The
documented order effects motivate the paper since Bayes-factor updating
predicts none. Author's own draft with two repairs (the two-readings setup
sentence restoring the referent of "first/second", and the broken "While..."
sentence split into its two independent reasons). The Field/Wagner footnote is
retained; the benchmark-as-other-reading sentence is left to Section 2.2, which
already states it.

**BEFORE** (the whole paragraph, including its footnote):

> Our conclusions for arrival sequence do not apply when evidence is hard or
> updating is Bayesian. The sequence-relevance arises when the evidence is soft
> and when it can be assumed that impressions arrive as revised credences, not
> as likelihoods. The panelist in the example comes to feel the candidate
> competent, without that feeling arising from any known likelihood
> ratio---indicating how many more times the credential is more probable under
> competence than in its absence. For input of that kind, strict
> conditionalisation is simply not available, i.e., there is no certifiable
> proposition to condition on. As a mechanism that holds conditionals fixed,
> Jeffrey updating is the unique coherent revision for credence delivered on
> cue's partition and is equivalent to making the minimal change of the prior
> consistent with the delivered marginal \citep{DiaconisZabell1982}
> \footnote{See \citet{Field1978} for how Jeffrey updates are asymmetric in the
> delivered-credence parametrisation and \citet{Wagner2002} for how they
> commute when their inputs are held fixed in Bayes-factor form.}.

**AFTER**:

```latex
Our conclusions for arrival sequence do not apply when evidence is hard or
updating is Bayesian. The sequence-relevance arises when the evidence is soft
and when it can be assumed that impressions arrive as revised credences, not as
likelihoods. The same cue admits both readings. Under the first, the panelist
in the example comes to feel the candidate competent -- the cue delivers a
level (a credence on the attribute's partition into competent and
not-competent categories). Under the second, the credential carries a Bayes
factor, a likelihood ratio stating how much more probable the credential is
under competence than in its absence, and this ratio multiplies whatever belief
it meets. A Bayes factor, however, requires the probability of the same
credential for a candidate who is not competent, and an impression does not
supply that counterfactual. Strict conditionalisation is unavailable for a
separate reason. A soft cue certifies no proposition to condition on. As a
mechanism that holds conditionals fixed, Jeffrey updating is the unique
coherent revision for credence delivered on the cue's partition and is
equivalent to making the minimal change of the prior consistent with the
delivered marginal \citep{DiaconisZabell1982}\footnote{See \citet{Field1978}
for how Jeffrey updates are asymmetric in the delivered-credence
parametrisation and \citet{Wagner2002} for how they commute when their inputs
are held fixed in Bayes-factor form.}. The paper is motivated by the
observation that order effects are a documented regularity in impression
formation \citep{Asch1946,HogarthEinhorn1992}, while Bayes-factor updating
predicts none.
Updating on levels rests on a second premise, which the paper states rather
than assumes: the latest impression sets its attribute's marginal outright,
with no weight left on what an earlier cue implied about that attribute.
\citet{DiaconisZabell1982} hold that premise. \citet{Hawthorne2004} objects
that full adoption is implausible; the alternative to a weight of one on the
later cue is a weight below one. That weight has two endpoints, the later impression
overwriting the earlier one and the earlier impression never being moved, and
the belief-adjustment model of \citet{HogarthEinhorn1992} lies between them
with a memory that, in their words, ``is limited to the location of one's
current anchor and not how this was reached''. The results of
Sections~\ref{sec:individual} and~\ref{sec:aggregation} hold at the overwrite
endpoint. Proposition~\ref{prop:ADJ} shows that the weight is recovered rather than
assumed. What is observed is each marginal in both reading sequences, across
values of the prior covariance $c$; what is recovered is the sequence effect
on any statistic, without the benchmark or the sequence mix
(Proposition~\ref{prop:ORD}), and the weight: the marginal that ignores the
prior association is the last-read one at $\omega=1$ and the first-read one
at $\omega=0$, and the sequence effect on the $A$-marginal at $c=0$ equals
$(1-\omega)(\alpha-q_0)$.
```

---

**Second premise, now stated here.** The paragraph's first part argues that
impressions arrive as *levels* rather than as Bayes factors. Sequential
updating on levels rests on a second premise, full adoption (Hawthorne's
Amnestic Update-Factor Thesis, p.~96), which licenses using the same `q` and
`r` on both reading orders. The appended paragraph states it, attributes it
(Diaconis--Zabell hold it; Hawthorne objects, pp.~98--99), names the weight
$\omega$ with its two endpoints, and points to Proposition ADJ (Change 6) for
the identification. The $\omega=0$ endpoint is the paper's own construction:
no source states "first impressions stick" as full protection of the first
cue (Hogarth--Einhorn's primacy comes from decaying weights over long series).
The Hogarth--Einhorn quotation is from their General Discussion, checked
against the text (Drive: `hogarth_einhorn_1992.pdf`). Change 11 carries the
rival-mechanism discussion.

---

## Change 5 -- Intro "two steps" paragraph (line 86): full replacement

**Purpose.** Add the third element of the summary. The classification can be
measured by comparing the two reading groups directly, with no benchmark and no
knowledge of the mix. Also fix the ungrammatical final sentence. The existing
footnote is retained verbatim (`\footnote{...}` below stands for it).

**BEFORE** (the whole paragraph; footnote abbreviated):

> We elaborate the finding in two steps. First, we establish that while
> evaluator diverges from the benchmark at first order in $c$
> (Proposition~\ref{prop:DIV}), the average believed association between
> attributes (Proposition~\ref{prop:DEC}) and the surplus-weighted loss of the
> population's decisions (Theorem~\ref{thm:LOS}) are second order. Second, we
> characterise exactly which statistics can register the phenomenon at its own
> order. For every interior mixture of reading sequences and across an open set
> of priors, a smooth statistic of the average belief is second order if and
> only if its differential at independence is proportional to the differential
> of the believed cross-attribute association (Proposition~\ref{prop:PRO}). The
> condition is necessary as well as sufficient, so the protected statistics
> (i.e.\ those that become second order) are the local dependence measures and
> surplus-weighted loss (for the threshold decision). The clear implication of
> the finding is that what an audit can answer depends on which statistic is
> aggregated rather than on how many evaluators are averaged over
> \footnote{...}. One consequence, is that the instruments most naturally
> trusted for detecting stereotype---belief audits asking how attributes are
> thought to go together---may not even be detectable.

**AFTER**:

```latex
We elaborate the finding in two steps. First, an evaluator's belief diverges
from the benchmark at first order in $c$ (Proposition~\ref{prop:DIV}), yet the
average believed association between attributes (Proposition~\ref{prop:DEC})
and the surplus-weighted loss of the population's decisions
(Theorem~\ref{thm:LOS}) stay within second order. Second, we characterise which
statistics register the phenomenon at its own order. Across an open set of
priors and every interior mixture of reading sequences, a smooth statistic of
the average belief is second order if and only if its differential at
independence is proportional to the differential of the believed
cross-attribute association (Proposition~\ref{prop:PRO}). The condition is
necessary as well as sufficient, so the protected statistics are exactly the
local dependence measures, together with the surplus-weighted loss for the
threshold decision. The classification rests neither on the benchmark nor on
aggregation. A statistic is protected exactly when the sequence effect on it,
$F(\PJ_{AB})-F(\PJ_{BA})$, is second order (Proposition~\ref{prop:ORD}). That
quantity names neither $\PB$ nor the sequence mix, so any observer who records
which cue each evaluator met first can compute it. What an audit can answer
therefore depends on which statistic is read, not on how many evaluators are
averaged over\footnote{...}. One consequence is that belief audits asking how
attributes are thought to go together are blind to the sequence effect at
first order, while audits asking about one attribute's probability register
it.
```

*Cut in counterfactual pass 3:* the closing sentence on mechanism
identification ("the marginal that stays fixed ... sits on the attribute read
last under overwriting, and on the attribute read first under a rule that
protects the earlier impression") now lives in Change 4's appended paragraph,
one paragraph earlier, with the reference to Proposition ADJ.

---

## Change 6 -- Section 5: new definition + Proposition ORD (insert after line 440)

**Purpose.** State how the sequence effect, already defined on the joint
belief, behaves when read through a statistic. No new definition is introduced;
one lead-in sentence gives the formula and notes that it involves neither the
benchmark nor the mixing weight. The manuscript already displays the
between-sequence formula in Proposition DIV(ii), so the proposition is a
corollary. Insert directly after
`\end{definition}` of Definition (protection) (line 440), before the paragraph
"A particularly surprising finding is that ..." (line 442).

**INSERT**:

```latex
The distortion compares the population average with the benchmark. The same
classification can be reached without either ingredient, by reading the
sequence effect of Definition~\ref{def:seqeffect} through the statistic
itself. For a smooth statistic $F$ the sequence effect read through it is
$F(\PJ_{AB})-F(\PJ_{BA})$, and it involves neither the benchmark $\PB$ nor
the mixing weight $\lambda$.

\renewcommand{\theproposition}{ORD}%
\begin{proposition}[between-sequence contrast]\label{prop:ORD}
For any weight $v$,
\[
\langle v,\;\PJ_{AB}-\PJ_{BA}\rangle
   \;=\; c\,\bigl(\kappa\,\langle v,R_1\rangle-\kappa'\,\langle v,R_2\rangle\bigr)
   \;+\;\bigO(c^{2}),
\]
with $\kappa$, $\kappa'$, $R_1$, $R_2$ as in Proposition~\ref{prop:DIV}. The
sequence effect on the $A$-marginal is $c\,\kappa'+\bigO(c^{2})$ and on the
$B$-marginal $-c\,\kappa+\bigO(c^{2})$; each is $\Theta(c)$ generically. The
sequence effect on the believed association is $\bigO(c^{2})$ exactly. Across
an open set of priors, a smooth statistic has a second-order sequence effect if
and only if it is protected (Definition~\ref{def:protection}); both conditions
hold exactly when the differential of the statistic at independence annihilates
$\mathrm{span}\{R_1,R_2\}$ (Proposition~\ref{prop:PRO} below). Nothing in the
display involves $\PB$ or $\lambda$. The classification is decided by the two
reading sequences, and every mixture $\Pbar_\lambda$ inherits it.
\end{proposition}

\begin{proof}
By Proposition~\ref{prop:DIV}(ii), $\PJ_{AB}-\PJ_{BA}=c(\kappa R_1-\kappa'R_2)+\bigO(c^{2})$;
applying $\langle v,\cdot\rangle$ gives the display. With $R_1=q\otimes(1,-1)$
and $R_2=(1,-1)\otimes r$,
\[
  \langle\mathbf 1_{A=1},R_1\rangle=0,\quad
  \langle\mathbf 1_{A=1},R_2\rangle=-1,\qquad
  \langle\mathbf 1_{B=1},R_1\rangle=-1,\quad
  \langle\mathbf 1_{B=1},R_2\rangle=0,
\]
giving $c\,\kappa'$ and $-c\,\kappa$. For the association,
$\langle\nabla\assoc(q\otimes r),R_1\rangle=\langle\nabla\assoc(q\otimes r),R_2\rangle=0$
(proof of Proposition~\ref{prop:PRO}), so its sequence effect is
$\bigO(c^{2})$. For the equivalence, with
$M_\lambda=\lambda\kappa R_1+(1-\lambda)\kappa'R_2$ from
Proposition~\ref{prop:DIV}(i) and $\nabla F:=\nabla F(q\otimes r)$,
\[
  \langle\nabla F,\Delta_{\mathrm{seq}}\rangle
    =\kappa\,\langle\nabla F,R_1\rangle-\kappa'\,\langle\nabla F,R_2\rangle,\qquad
  \langle\nabla F,M_\lambda\rangle
    =\lambda\kappa\,\langle\nabla F,R_1\rangle+(1-\lambda)\kappa'\,\langle\nabla F,R_2\rangle;
\]
across an open set of priors $\kappa$ and $\kappa'$ vary independently, so
either vanishes identically exactly when
$\langle\nabla F,R_1\rangle=\langle\nabla F,R_2\rangle=0$.
\end{proof}
```

**Then edit the following paragraph (line 442), first sentence only.**

BEFORE:

> A particularly surprising finding is that for a smooth statistic of the
> belief, the answer is settled before any averaging over the aggregate takes
> place.

AFTER:

```latex
A particularly surprising finding is that for a smooth statistic of the belief,
the answer is settled before any averaging over the aggregate takes place
(Proposition~\ref{prop:ORD}).
```

**Verification status.** The display, both marginal coefficients, the exact
association annihilation, the lambda-freeness and the generic non-vanishing are
machine-checked in `lean/JeffreyOrder/PropORD.lean` (builds, no sorry) and
`sympy/verify_ORD.py` (22/22; registered in `run_all.py`, full suite 13/13).
The proof is 29\% prose by character count.
The manuscript's conventions match the Lean definitions exactly:
$\kappa=(\alpha-q_0)r_0(1-r_0)/Z$ and $\kappa'=(\beta-r_0)q_0(1-q_0)/Z$ (lines
290, 297), and $\kappa'=-K$ for the $K$ of Proposition DRF. The equivalence-with-protection sentence is checked in step (8) of
`verify_ORD.py`: vanishing of the between-sequence coefficient over four
independent priors leaves exactly $\mathrm{span}\{J,\nabla\assoc\}$, and
$\det\,\partial(\kappa,\kappa')/\partial(\alpha,\beta)\neq0$, so $\kappa,\kappa'$
vary independently (added 2026-09-20).


**Second INSERT (adoption weight), directly after the ORD proof above.** The
weight is written $\omega$ throughout the manuscript (decided 2026-09-20),
because $\delta_\sigma$ is already the score coefficient (manuscript line
357). The Lean and sympy files keep `δ`/`delta` for the same quantity.

```latex
The two sequences so far adopt each delivered credence in full. Suppose instead
that the second cue is adopted only in part: after the first Jeffrey step, the
response to the second cue is a Jeffrey step on its partition to the target
$(1-\omega)\,m+\omega\,r_1$, where $m$ is the marginal the first step left and
$0\le\omega\le1$ is the adoption weight. The rule is the averaging form of the
belief-adjustment model, $S_k=(1-w_k)S_{k-1}+w_k s(x_k)$
\citep[Eq.~4]{HogarthEinhorn1992}, applied to a marginal and embedded in the
joint law by a Jeffrey step. Write $P^{\omega}_{AB}$ and $P^{\omega}_{BA}$ for
the two sequences under this rule; $\omega=1$ recovers $\PJ_{AB}$ and
$\PJ_{BA}$, and $\omega=0$ ignores the second cue. Let $P^{A}$ and $P^{B}$
denote the belief after the $A$-cue alone and after the $B$-cue alone.

\renewcommand{\theproposition}{ADJ}%
\begin{proposition}[adoption weight]\label{prop:ADJ}
For every $c$ and every $\omega$, in sequence $AB$,
\begin{align*}
P^{\omega}_{AB}(B{=}1)-r_1 &=(1-\omega)\bigl[P^{A}(B{=}1)-r_1\bigr],\\
P^{\omega}_{AB}(A{=}1)-q_1 &=\omega\,\bigl[\PJ_{AB}(A{=}1)-q_1\bigr],\\
P^{\omega}_{AB}(A{=}1)-P^{\omega}_{BA}(A{=}1)
  &=\omega\,\bigl[\PJ_{AB}(A{=}1)-q_1\bigr]+(1-\omega)\bigl[q_1-P^{B}(A{=}1)\bigr].
\end{align*}
Hence the last-read marginal equals its delivered credence identically in $c$
exactly when $\omega=1$, the first-read marginal does so exactly when
$\omega=0$, and for $0<\omega<1$ neither does. The first-order coefficient in
$c$ of the first-read marginal is $\omega$ times the benchmark's coefficient
for the same marginal. At $c=0$ the third line equals $(1-\omega)(\alpha-q_0)$:
a partially adopting evaluator shows a sequence effect at independence, where
the fully adopting evaluator (Proposition~\ref{prop:IMM}) and the benchmark
show none.
\end{proposition}

\begin{proof}
With $Q$ the belief after the first step and $t=(t_0,t_1)$,
$t_1=(1-\omega)\,Q(B{=}1)+\omega r_1$, the target of the second, column $j$
is multiplied by $t_j/Q(B{=}j)$:
\[
  P(B{=}1)=t_1,\qquad
  P(A{=}1)=\sum_j Q(A{=}1,B{=}j)\,\frac{t_j}{Q(B{=}j)}
  =(1-\omega)\,Q(A{=}1)+\omega\sum_j Q(A{=}1,B{=}j)\,\frac{r_j}{Q(B{=}j)}.
\]
In sequence $AB$, $Q=P^{A}$, $Q(A{=}1)=q_1$, and the last sum is
$\PJ_{AB}(A{=}1)$:
\[
  P^{\omega}_{AB}(B{=}1)-r_1=(1-\omega)\bigl[P^{A}(B{=}1)-r_1\bigr],\qquad
  P^{\omega}_{AB}(A{=}1)-q_1=\omega\bigl[\PJ_{AB}(A{=}1)-q_1\bigr].
\]
In sequence $BA$, by symmetry,
$P^{\omega}_{BA}(A{=}1)=(1-\omega)\,P^{B}(A{=}1)+\omega q_1$; subtracting
gives the third line. Neither bracket vanishes identically:
\[
  P^{A}(B{=}1)-r_1\Big|_{c=0}=r_0-\beta,\qquad
  \PJ_{AB}(A{=}1)-q_1=-c\,\frac{q_0(1-q_0)(r_0-\beta)}{\alpha\beta(1-\alpha)(1-\beta)}+\bigO(c^2),
\]
the second by Proposition~\ref{prop:DRF}, whose proof also gives
$\PJ_{AB}(A{=}1)-\PB(A{=}1)=\bigO(c^2)$, hence the coefficient claim. At
$c=0$ a step leaves the other marginal unchanged:
\[
  \PJ_{AB}(A{=}1)=q_1,\qquad P^{B}(A{=}1)=1-\alpha,\qquad
  P^{\omega}_{AB}(A{=}1)-P^{\omega}_{BA}(A{=}1)=(1-\omega)(\alpha-q_0).
\]
\end{proof}
```

**Verification status (ADJ).** Every line is machine-checked in
`lean/JeffreyOrder/Anchoring.lean`: line 1 is `dampedB_deviation`, line 2 is
`routeDamped_mA1_deviation`, line 3 is `orderEffect_damped_mA1`, the $c=0$
value is `orderEffect_damped_at_indep`, the endpoints are `dampedB_at_one`,
`dampedB_at_zero`, `routeDamped_at_zero_pins_A`; all nine Anchoring theorems
are in `check_axioms.lean` with standard axioms only. The coefficient claim
and the "neither at interior $\omega$" claim are in
`sympy/check_zero_slope_identification.py` (20/20; suite 13/13), cases (3)
and (6); the "only if" half of "exactly when" uses `dampedB_at_zero` with
`prior_mB1` (value $r_0-\beta$) and `propDRF_route_AB` (the DRF coefficient).
Case (6d) there, that no single $\omega$ reproduces both benchmark slopes,
is verified but not claimed in the manuscript. The proof is 30\% prose by
character count (characters inside `$..$` and `\[..\]` counted as
expressions). The Hogarth--Einhorn attributions were checked against their text
(Drive: `hogarth_einhorn_1992.pdf`): Eq.~(4) is theirs; "memory is limited to
the location of one's current anchor and not how this was reached" is their
General Discussion; primacy in their model comes from weights decaying over a
long series, so the $\omega=0$ endpoint is ours, not theirs; "attention
decrement" is Anderson (1981), reported by them as a rival.

---

## Change 7 -- bibliography.bib

Required: `Hawthorne2004` (Change 2), `Heckman1998` (Change 9), `Doring1999`
and `Garber1980` (Change 12), and `Bohren2019` (Change 13) -- the last three
carry their bibtex in their own sections. Zhao-Osherson is not cited anywhere in the change set (author's
decision; review log Entry 15).

```bibtex
@article{Heckman1998,
  author  = {Heckman, James J.},
  title   = {Detecting Discrimination},
  journal = {Journal of Economic Perspectives},
  year    = {1998},
  volume  = {12},
  number  = {2},
  pages   = {101--116}
}

@article{Hawthorne2004,
  author  = {Hawthorne, James},
  title   = {Three Models of Sequential Belief Updating on Uncertain Evidence},
  journal = {Journal of Philosophical Logic},
  year    = {2004},
  volume  = {33},
  number  = {1},
  pages   = {89--123}
}

```

---

## Change 8 (optional) -- AI declaration (line 1212)

The declaration names Proposition PRO as proposed and cross-verified with
Claude. Proposition ORD has the same provenance. If the declaration is meant to
be exhaustive, extend "Proposition~\ref{prop:PRO} (uniqueness of the protected
statistic)" to "Propositions~\ref{prop:PRO} (uniqueness of the protected
statistic), \ref{prop:ORD} (between-sequence contrast)
and~\ref{prop:ADJ} (adoption weight)".

---

## Change 9 -- Related literature (Section 3): the audit-identification precedent

**Purpose.** The manuscript's opening now claims an identification gap, and its
headline implication is about what audits can detect. The economics of
discrimination has its own audit-identification critique, and the manuscript
cites that lineage (Phelps, Arrow, Coate-Loury, Becker) without it. One
insertion connects the two and marks the complementarity: Heckman attacks the
decision-rate audit on distributional grounds; this paper attacks the
belief-association audit on geometric grounds and shows the decision side is
where the sequence effect stays visible.

**Location.** End of the statistical-discrimination paragraph in Section 3
(the paragraph citing Phelps1972, Arrow1973, CoateLoury1993, BCGS2016), which
currently ends:

> ...protection intrinsic to the arithmetic of aggregation, rather than imposed
> by an enforced constraint on behaviour---goes back at least as far as
> \citet{Becker1962}.

**INSERT after that sentence:**

```latex
The audit perspective has a precedent in the economics of discrimination,
which already accepts that an audit's informativeness is an identification
question settled by the instrument's structure rather than its sample size:
\citet{Heckman1998} showed that audit-pair estimates of
discrimination rest on assumptions about unobserved productivity that no sample
size repairs. The current paper brings that discipline to the belief-measurement
side of the audit, where the failure is geometric rather than distributional and
no auxiliary assumption rescues the association statistic, and shows that the
sequence effect remains detectable in the marginal probabilities and in the
share of decisions changed, the decision side being the target of Heckman's
caution.
```

**Optional companion sentence** (fits either here or beside the protected-class
discussion in Section 5), making the identification language of the new
opening precise:

```latex
The blindness of the protected statistics is not a small-sample limitation: the
first-order signal is zero in the population itself, so it is an identification
failure rather than an estimation difficulty, and no quantity of data overcomes
it.
```

---

## Change 10 -- Related literature (line 242): Asch as evidence, not decoration

**Purpose.** The manuscript's claim against the belief-adjustment tradition is
currently that it reads "a single evaluative anchor" and so needs "a full
posterior table rather than a single rating". Asch's own data shows the framing
is slightly off and the underlying point is stronger than stated. In
Experiment VI he did **not** collect a single rating: his Table 7 reports, for
each of eighteen traits, the percentage of subjects judging it to fit, separately
for the orders `intelligent->envious` and `envious->intelligent`. Eighteen
marginals under two reading orders, in 1946. He still could not identify the
mechanism, because they are all *marginals* -- the check-list asks whether a trait
fits, never whether two traits go together, so the association cannot be formed
from his data. The requirement is therefore not "many ratings rather than one" but
**the joint rather than the margins**, and marginals are precisely the statistics
this paper shows are guaranteed to register the effect.

His numbers also supply evidence that the unevenness is real and was visible at
the outset: across the two orders, `restrained` moves 64 to 9 and `good-looking`
74 to 35, while `serious` moves 97 to 100, `persistent` 82 to 87 and `reliable`
84 to 91. Some statistics swing enormously, others barely move, in one table, with
no account given of why.

**BEFORE** (final two sentences of the paragraph at line 242):

> While Bayes-factor updating predicts no sequence effect anywhere
> \citep{Wagner2002}, the belief-adjustment accounts of order effects
> \citep{HogarthEinhorn1992} focus on a single evaluative anchor. What the paper
> highlights is that such experimental analyses must also factor in the
> cross-attribute associaton -- thus requiring the elicitation of a full posterior
> table rather than a single rating produced by step-by-step or end-of-sequnce
> response modes.

**AFTER**:

```latex
While Bayes-factor updating predicts no sequence effect anywhere
\citep{Wagner2002}, the experimental tradition has measured the phenomenon
entirely in marginals. \citet{HogarthEinhorn1992} track a single evaluative
anchor. \citet{Asch1946} is more generous and more instructive: his Experiment~VI
reports, for each of eighteen traits, the proportion of subjects judging it to fit
the person described, separately for the two reading orders. That the sequence effect
differs in size across statistics is already visible there, although the
differences Asch reports are all among marginals, which the paper treats
alike. Between the orders, \emph{restrained}
moves from 64 to 9 per cent and \emph{good-looking} from 74 to 35, while
\emph{serious} moves from 97 to 100, \emph{persistent} from 82 to 87 and
\emph{reliable} from 84 to 91. Some statistics swing enormously and others
scarcely move, with no account offered of the difference. Yet eighteen marginals
separate amnestic updating from partial adjustment no better than one.
Separating them reads a marginal across values of the prior covariance
(Proposition~\ref{prop:ADJ}), and the check-list
asks whether a trait fits and never whether two traits go together, so the
believed association cannot be formed from such data at all. What the paper
adds is therefore not that more ratings are needed but that a second object
must be elicited. Seeing the sequence effect requires only a marginal read in
both orders, which Asch has; locating the mechanism behind it requires the
association as well, which he does not.
```

**Note.** This supersedes the de-duplication entry recording that the
Hogarth-Einhorn discussion was reduced to one clause; the passage now carries
Asch as evidence rather than as a bare citation. `Asch1946` and
`HogarthEinhorn1992` are already in the bibliography, so Change 7 is unaffected.

**Verification status.** Experiment VI, Table 7 and Table 8 read directly from the
scan; the percentages above are transcribed from Table 7. Asch's own explanation
of the order effect is a "direction" set by the first term, with his footnote 5
naming *centrality* -- not softness -- as what abolishes or reverses primacy. No
claim is made here that his data supports the amnestic mechanism; the claim is
only that the unevenness is present and unexplained, and that his instrument
cannot identify its source.

---

## Change 11 -- Scope section: rival mechanisms, and what separates the mechanisms

**Purpose.** Order effects have a competing rational explanation, and the
conflict is stark: at its $\omega=0$ endpoint the adjustment family protects
the FIRST impression, amnestic updating protects the LAST. The literature holds
only the second view (Diaconis--Zabell) and an objection to it (Hawthorne); the
first is our endpoint, not a cited position. The section also
discharges Hawthorne's plausibility objection, which the paper **inherits rather
than escapes**: full adoption of the latest impression does disregard what an
earlier cue implied about that attribute. The reply is not that the objection
misses, but that the adoption weight is a parameter the data locate. Only his
*illustration* -- one basis cued twice -- fails to reach a model with one cue per
attribute. The scope section
currently discusses which assumptions break the results but not which rival
mechanisms could mimic the phenomenon. This insertion states the conflict, gives
the observable that separates the mechanisms, and records the Asch caution.

**Location.** Section 6 (`sec:scope`), immediately after the paragraph ending:

> ...A population whose members differ in the prior covariance $c$ itself is a
> different aggregation problem, outside the stated results.

**INSERT:**

```latex
A separate scope question concerns rival mechanisms rather than broken
assumptions. Order effects have an explanation that competes with amnestic updating. Under
partial adjustment, later cues move belief only part of the way to their
targets \citep{HogarthEinhorn1992}, and at the limit of no adjustment the first
impression is never moved by later evidence. Such accounts and the amnestic model point in
opposite directions: the protected impression is the first one there, and the
last one here. An observable separates them. Under partial adoption of the second cue with
weight $\omega$ (Proposition~\ref{prop:ADJ}), the marginal that ignores the
prior association is the last-read one at $\omega=1$, the first-read one at
$\omega=0$, and neither in between. A reading on which neither marginal ignores
the association is consistent with an interior weight and with the benchmark
alike; the two are separated by the sequence comparison, under which the benchmark
shows no sequence effect at any $c$ and an interior weight shows one already
at $c=0$, equal to $(1-\omega)(\alpha-q_0)$. Since every rule here is a separable reweighting, all
of them carry the prior's odds ratio (Lemma~\ref{lem:SEP}), so the odds ratio
separates none of them and the two marginals are the only statistics that do. Three cautions bound the claim. The
classification covers this one-parameter adjustment family, not every
conceivable mechanism. \citet{Asch1946} is evidence that order moves marginals, one cue per trait
across eighteen traits, and not evidence for either endpoint: his own account is
that early terms set a direction for the reading of later ones, so that what a
later cue delivers depends on its position, a mechanism outside the family
here, in which each cue delivers the same credence in either position. And full adoption is a substantive commitment rather than a
consequence of the level reading. \citet{Hawthorne2004} objects that it ``seems
implausible that the most recent experience or non-propositional state should
completely dictate belief strengths for basis sentences, with no regard for the
import of previous experiences or states''. The objection applies here: the
credential's implication for trustworthiness is erased once the letter fixes
that attribute. What does not apply is the illustration he uses to press it, in which one
basis is cued repeatedly so that successive impressions overwrite each other; attribute
locality (Assumption~\ref{as:local}) gives each cue a basis of its own. The
adoption weight is his objection stated as a parameter; the reply offered here
is not that the objection misses but that the weight is recovered from the two
marginals rather than assumed.
```

**Verification status.** Every claim in the paragraph is Proposition ADJ
(Change 6), whose verification note lists the Lean theorems and sympy cases;
the odds-ratio sentence is the manuscript's own remark after Lemma SEP (lines
625--627), extended to the damped routes because they are Jeffrey steps.
Garber removed from the illustration clause: his repeated-glances example
targets Field's portable factors and never discusses order. Citations
already in the bibliography; no Change 7 impact. A bounded-memory citation
(Wilson 2014, Econometrica) is available if a referee asks for one, but the
family is stated through Hogarth-Einhorn's own equation, which is already cited.

---

## Change 12 -- Related literature: open with the Jeffrey commutativity literature

**Purpose.** Section 3 currently opens on empirical psychology and observational
learning, and never discusses the paper's closest neighbours -- the literature
on whether, and when, probability kinematics commutes. That material sits in the
introduction instead, and Change 2 adds more of it there. A reader looking under
"Related literature" for where the paper stands in the Jeffrey debate does not
find it. This inserts a first paragraph covering that literature, so the section
moves from nearest to most distant, and the introduction can state the question
without also having to survey.

**Location.** Section 3 (`sec:literature`), as a new opening paragraph
immediately after `\label{sec:literature}` and before "While sequence-dependence
has been of interest to empirical psychology...".

**INSERT:**

```latex
The closest literature concerns the commutativity of probability kinematics
itself. \citet{DiaconisZabell1982} give the condition under which two successive
Jeffrey revisions lead to the same belief, and \citet{Hawthorne2004} names its
full-adoption premise, which the present paper adopts, the Amnestic
Update-Factor Thesis, crediting that criterion to them. His
survey also fixes the vocabulary. Extensions of Jeffrey updating to sequences
differ in what an experience is taken to deliver: new probabilities for its own
basis directly, a multiplicative factor applied to the belief it meets, or a
ratio between basis sentences that the prior does not constrain. The model used
here sits at the first, where the impression delivers a credence and the most
recent one fixes its attribute outright; the benchmark $\PB$ of
Section~\ref{sec:jeffrey} has the form of the extended update formula built on
the second. The comparison drawn throughout is therefore between the two ends of
his taxonomy, and the identification questions this paper raises are the ones
that the first end brings with it and the second end does not, since revisions
at the second end commute. (What Section~\ref{sec:jeffrey} calls a Bayes
factor is his normed-likelihood factor; on a two-element basis the two induce the
same revision.) Two responses to the resulting order-dependence have been made. One treats it as a
defect of the framework: \citet{Doring1999} argues that the order effect can be
pronounced enough to call for an adjustment that Jeffrey's rule cannot supply.
The other re-describes the input so that the effect disappears.
\citet{Field1978} reparametrises the update so that its input is a portable
factor rather than a delivered credence, and \citet{Wagner2002} shows that when
identical learning is represented by identical Bayes factors, sequential
revisions commute; \citet{Garber1980} objects that a portable factor compounds
implausibly under repetition. The present paper takes neither route. It accepts
the order-dependence that the delivered-credence reading entails, and asks a
question that has received little attention, namely how far the two sequences
disagree in each statistic of the resulting belief. That the disagreement is uneven
across statistics---first order in the marginals and the share of decisions
changed, second order in the believed association and the surplus-weighted
loss---is what makes the debate answerable by
measurement rather than by introspection about how completely an evaluator
dismisses an earlier impression.

The insensitivity of the association is moreover not peculiar to the extension
adopted here. Each of the three extensions revises a basis by multiplying its
cells by a factor that depends on that basis alone; they differ in what fixes
the factor, not in the form of the revision. Lemma~\ref{lem:SEP} therefore
covers all of them, and under each the believed association is rescaled but
never shifted. What separates the extensions is whether there is any sequence
effect for that statistic to conceal. An observer reading the believed
association alone can accordingly no more tell which extension a population
uses than detect the sequence effect itself, while the marginals and the
share of decisions changed do both.
```

**Then adjust the following sentence** so the section reads as a descent from
nearest to most distant neighbours.

BEFORE:

> While sequence-dependence has been of interest to empirical psychology at
> least since \citet{Asch1946}, the problem seems to have received interest in
> economics primarily through the observational learning literature.

AFTER:

```latex
Further afield, sequence-dependence has been of interest to empirical psychology
at least since \citet{Asch1946}, and in economics has been taken up chiefly
through the observational learning literature.
```

**Bibliography.** `Doring1999` and `Garber1980` are **not** currently in
`bibliography.bib` and must be added (Change 7 already adds `Hawthorne2004`;
`DiaconisZabell1982`, `Field1978` and `Wagner2002` are present):

```bibtex
@article{Doring1999,
  author  = {D\"oring, Frank},
  title   = {Why {B}ayesian Psychology Is Incomplete},
  journal = {Philosophy of Science},
  year    = {1999},
  volume  = {66},
  number  = {Supplement},
  pages   = {S379--S389}
}

@article{Garber1980,
  author  = {Garber, Daniel},
  title   = {Field and {J}effrey Conditionalization},
  journal = {Philosophy of Science},
  year    = {1980},
  volume  = {47},
  number  = {1},
  pages   = {142--145}
}
```

**Note on overlap with Change 2.** Both mention Diaconis-Zabell and Hawthorne.
This is deliberate and not duplication: the introduction states the *question*
against what is settled, while this paragraph places the paper among the
*responses* to order-dependence. If the overlap still reads as repetitive when
both are applied, the fix is to shorten the introduction's version, since the
survey belongs here.

**Note on the closing paragraph.** The claim that Lemma~SEP covers all three
extensions is exact, not a gesture. In the Lean development a step is a pair
(attribute, `factor : Bool -> R`) -- a multiplicative factor depending on that
attribute's value alone, with **no constraint on where the factor comes from** --
and `isSeparable_applySteps` proves that any finite list of such steps composes
to a separable reweighting, for arbitrary `N`. A delivered-credence ratio
(amnestic), a normed likelihood, and a factor derived from a likelihood ratio are
all instances. `sep_rescales_association` then gives the rescaling. This answers
the obvious objection to leaning on Hawthorne's taxonomy -- that his
order-independent extensions already dispose of the problem, making this paper
redundant. They dispose of the order effect, not of the identification issue: the
association is uninformative under every extension, and what the order-free ones
remove is the thing it would otherwise be concealing.

**Note on the taxonomy sentences.** These place the paper's own model and its
benchmark within Hawthorne's classification of extensions (his Sections 5-7):
the model is the absolute-credence (Amnestic) end, the benchmark the factor end.
The identification of `P^B` with his extended update formula is by inspection of
the two formulas -- `P^B(i,j) = P(i,j) * l^A_i * l^B_j` with
`l^A_i = q_i / P(A=i)`, which is his `NL[Q,e,E_i] = Q_e[E_i]/Q[E_i]`. The
parenthetical terminological note is needed because the manuscript's "Bayes
factor" is his normed-likelihood factor, while his likelihood-ratio factor is the
ratio of two of those; the distinction does not affect any result here, since on
a two-element basis both give posterior `q` on a single cue (Proposition IMM).

**Verification status.** All five papers read in full and formalized in
`literature/`; the characterisations above (D\"oring's "cannot be understood as
an assimilation", Wagner's "identical Bayes factors", Garber's compounding
counterexample, Hawthorne's attribution of the criterion to Diaconis-Zabell in
his note 12) are quoted or paraphrased from the primary texts.

---

## Change 13 -- Related literature: the identification of discrimination

**Purpose.** The paper's headline implication is that an audit's instrument
decides what it can detect. Economics has a current formulation of that problem
which the manuscript does not engage: \citet{Bohren2019} give a taxonomy of
sources of discrimination, a parameter-to-behaviour map, and an identification
strategy built on it. Their framework presupposes Bayesian updating, and under
credence-input updating the presupposition fails in a specific way. This is the
compressed manuscript form of `notes/the_discrimination_problem.tex`.

**Location.** Section 3, at the end of the statistical-discrimination paragraph
-- immediately after the Heckman sentences inserted by Change 9, so the two
audit-identification points sit together.

**INSERT:**

```latex
A current formulation of the same problem is given by \citet{Bohren2019}, who
distinguish discrimination arising from correct beliefs, from biased beliefs,
and from preferences, and identify the source from how discrimination evolves
along a history of evaluations. Their map from partiality to behaviour is
Bayesian: a belief gap between groups is transmitted to evaluations, attenuated
by the precision of the signal, and vanishes only as judgment becomes perfectly
objective. Under the reading adopted here that map does not hold. A Jeffrey step
sets the marginal of the attribute it addresses to the delivered credence, so an
evaluator who takes the impression of quality last evaluates two workers alike
whatever her prior beliefs about their groups, and the belief gap is silenced
without any gain in objectivity. Under partial adoption with weight $\omega$ on
the impression, a fraction $1-\omega$ of the belief gap survives
(Proposition~\ref{prop:ADJ}). What credence-input updating does is therefore not to add a
further source of discrimination to their three, but to relocate where
partiality must sit in order to act: lodged in the prior it is silenced, lodged
in the impression it passes through untouched, and their framework has no
parameter for the latter.
```

**Bibliography.** `Bohren2019` must be added:

```bibtex
@article{Bohren2019,
  author  = {Bohren, J. Aislinn and Imas, Alex and Rosenberg, Michael},
  title   = {The Dynamics of Discrimination: Theory and Evidence},
  journal = {American Economic Review},
  year    = {2019},
  volume  = {109},
  number  = {10},
  pages   = {3395--3436}
}
```

**Also adjust**, in the same section, the claim that economics took up
sequence-dependence "chiefly through the observational learning literature"
(Change 12's wording): \citet{Bohren2019} is an economics treatment in which
sequence is central, so the clause should read "chiefly through the
observational learning literature, though it also arises in the dynamics of
discrimination".

**Verification status.** The silencing result is machine-checked:
`literature/bohren_imas_rosenberg2019/sympy/check_pinning_kills_partiality.py`
(7/7; the gap is zero for arbitrary group priors, arbitrary and even unequal
covariances, and arbitrary delivered credences), and the proportional
attenuation under partial adoption is `dampedB_deviation` in
`lean/JeffreyOrder/Anchoring.lean`. Deliberately **omitted** from the manuscript
paragraph: the sign-inversion result (verified at only twelve configurations),
and any suggestion that this bears on their discrimination reversal, which is
driven by evaluator heterogeneity and beliefs about other evaluators' beliefs
(their Proposition 3) and is untouched by anything here.

---

## De-duplication notes (what was cut in compaction)

- The question/answer draft's resolution passage ($c^2 \ll \varepsilon \ll c$)
  is dropped. The manuscript's terminology paragraph (line 83) already carries
  it and stays unchanged.
- The two-horn draft's second paragraph is folded into the single Change 4
  paragraph.
- The Hogarth-Einhorn instrument discussion is reduced to one clause (Change
  2). The Zhao papers are not referred to at all (review log Entry 15); the
  reads remain on record in `literature/measurement_susceptibility_survey.md`.

## Change 14 -- Section 4: title and first two paragraphs (lines 290-295)

**Purpose.** Sections 4 and 5 are organised on the individual-versus-aggregate
axis, which the paper's own results (ORD, PRO) show to be inert: the
classification is decided before any averaging. The section titles and
openers should be organised on the statistic instead. This change retitles
Section 4 and rewrites its first two paragraphs; the third paragraph (why
these statistics; FGT remark) is unchanged. No new proof: "both are first
order" is DIV and SCR; the closing sentence is ORD and PRO.

**Title BEFORE:** `\section{Sequence effects at the individual level}`

**Title AFTER:** `\section{Which statistics register the sequence}`

**BEFORE** (paragraphs 1-2):

> \section{Sequence effects at the individual level}
> \label{sec:individual}
> Since Bayesian conditioning is sequence-independent by definition, we measure sequence effects with respect the Bayesian benchmark using two \textbf{primitive} statistics -- gap and score. As discussed in Section  \ref{sec:jeffrey}, the \emph{gap} is a distance from a Bayes-factor benchmark $\PB$ for the evaluator (see Definition \ref{def:indgap}). The second primitive, \emph{score-gap} $s(P)=\langle\vv,P\rangle$ defined against the sequence-free reference $\PB$ alongside with a threshold $\tau$. As we show in Section \ref{sec:individual_primitives}, both \emph{gap} and \emph{score-gap} are first-order to the evaluator.
> The belief-statistics and decision-statistics follow from the two primitive statistics. The two \textbf{belief-statistics} that we are interested in are the \emph{marginal probability} and \emph{believed cross-attribute association}. The \textbf{decision-statistics} that we discuss for the individual evaluator and the population aggregate are \emph{surplus-weighted loss} $L(c)$ and a decision-\emph{flip}. The decision-flip is counted in the population to determine the \emph{share} of those evaluators whose decision is changed by the sequence. The loss is then simply an aggregation of $|u|$ over this share. How these quantities are carried over in aggregate are detailed in Section~\ref{sec:aggregation}.

**AFTER**:

```latex
This section asks, of each statistic of a single evaluator's belief, whether
the two reading sequences move it at first order in the prior covariance $c$
or only at second order. Two primitive statistics carry the comparison with
the sequence-free benchmark $\PB$ of Section~\ref{sec:jeffrey}: the
\emph{gap}, the distance of the evaluator's belief from $\PB$
(Definition~\ref{def:indgap}), and the \emph{score-gap}
$s(P)=\langle\vv,P\rangle$, the same distance read through the decision
weights and set against a threshold $\tau$.
Section~\ref{sec:individual_primitives} shows that both are first order.

The belief statistics and the decision statistics are read from the same
belief. The belief statistics are the \emph{marginal probability} of each
attribute and the \emph{believed cross-attribute association}; the decision
statistics are the surplus-weighted loss $L(c)$ and the decision \emph{flip},
the indicator that the sequence changes the evaluator's decision. Whether a
statistic registers the sequence is settled here, for one evaluator;
Section~\ref{sec:aggregation} shows that averaging over evaluators who met the
cues in either sequence changes neither the order of any statistic nor the
classification.
```

*Removed:* "How these quantities are carried over in aggregate are detailed in
Section 5", which promised that the aggregate adds something.

---

## Change 15 -- Section 5: title and first paragraph (lines 465-469)

**Purpose.** Same as Change 14, for the aggregate section. "Largely the nature
of the statistic itself" hedged an exact result. No new proof: the plane and
the differential condition are PRO; the sequence-side statement is ORD.

**Title BEFORE:** `\section{Aggregated Effects of Cue-Sequence}`

**Title AFTER:** `\section{Averaging over sequences does not change the classification}`

**BEFORE** (paragraph 1):

> \section{Aggregated Effects of Cue-Sequence}
> \label{sec:aggregation}
> We now describe how the effect of arrival order on statistics discussed in the previous section fares in the population aggregate. Throughout this section we consider a population mixing the two encounter sequences: a fraction $\lambda\in[0,1]$ of evaluators meet the $A$-first (the credential cue in the example) and the
> rest $B$-first (the letter), so the mean belief under Jeffrey conditioning is

**AFTER**:

```latex
The previous section classified statistics for one evaluator. This section
shows that the classification survives averaging over a population in which a
fraction $\lambda\in[0,1]$ of evaluators meet the $A$-cue first (the
credential in the example) and the rest the $B$-cue first (the letter), so
that the mean belief under Jeffrey conditioning is
$\Pbar_\lambda=\lambda\,\PJ_{AB}+(1-\lambda)\,\PJ_{BA}$. What makes a
statistic first order or second order is the statistic itself, not the mixture
$\lambda$: the mean belief lies in the plane through $\PB$ spanned by the
leading directions of the two sequences, and a statistic's order is fixed by
its differential on that plane (Propositions~\ref{prop:ORD}
and~\ref{prop:PRO}).
```

The paragraph that follows ("Two definitions are worth pinning down ...") and
the protection definition are unchanged; Change 6 inserts ORD after that
definition.

---
