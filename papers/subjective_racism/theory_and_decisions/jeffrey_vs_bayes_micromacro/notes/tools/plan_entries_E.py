"""Section E: the entries of the archived change plan
(notes/manuscript_change_plan_asof_2026-09-30.md), re-issued against the
author's draft of 2026-09-30 with the citation audit's corrections applied.

Each entry is (id, title, parts, why, evidence).  A part is either
  (start, end, after)                      replace the manuscript text from
                                           start to end (inclusive) by after
  ("insert_para", anchor, anchor, after)   new paragraph(s) after the paragraph
                                           containing anchor
Paragraph breaks inside an AFTER text are written as \\par.
"""

E2 = []

E2.append(("E.1", "Related literature, the statistical-discrimination paragraph: a group gap from the reading sequence alone (was 1.5, compacted)", [(
    "insert_sentence", "and yet remain invisible in the protected dependence statistics.", "and yet remain invisible in the protected dependence statistics.",
    r"""The sequence need not be assigned by chance, however. A candidate who comes
through a referral is met first through the letter and one who applies unsolicited
through the credential, so if referral is more common in one group, two groups
presenting the same evidence to evaluators with the same priors and preferences
receive mean beliefs that differ by the sequence effect times the difference in the
share of evaluators who read the credential first. Such a gap is neither statistical
discrimination in the sense of \citet{Phelps1972} and \citet{Arrow1973}, which rests
on different prior beliefs about the groups, nor any of the sources
\citet{Bohren2019} distinguish, correct beliefs, biased beliefs and preferences,
since none of these differs between the groups, and it registers in the marginal
probabilities and the share of decisions changed but in the believed association
only at second order.""")],
    "Moved and compacted at the author's request (2026-10-05): the introduction is long, and the "
    "point belongs with the statistical-discrimination lineage. It goes right after the sentence "
    "saying that in the baseline the differences are keyed to encounter sequence rather than to any "
    "group-marker, which it turns on: the sequence can itself track a group. Dropped from the 1.5 "
    "text: the opening on Asch and on Hogarth and Einhorn's review (the psychological framing the "
    "author has moved away from); the clause on which group the gap favours, which cited "
    "Proposition ADJ, no longer in the paper; and \"an audit of the believed association does not "
    "register it\", which overclaimed, since the group difference in mean believed association is "
    "second order, not zero (Proposition DEC). Audit P4 (\"prior beliefs about the groups\") and D13 "
    "(the three sources named, not treated as exhaustive) are kept.",
    "The identity mean(lambda) - mean(lambda') = (lambda - lambda')(PJ_AB - PJ_BA) is exact, "
    "sympy/verify_ORD.py step 9; PropORD.lean (marginals first order); Ladder.lean "
    "(ladder_assoc_coeff_at_one: the association second order at full adoption); Phelps 1972, "
    "Arrow 1973 and Bohren-Imas-Rosenberg 2019 records."))

E2.append(("E.2", "Setup 2.1, what an impression is (was 2.A)", [(
    "yields an impression (a target marginal) $q=(q_{0},q_{1})$ on $A$'s partition and a cue on $B$ yields an impression $r=(r_{0},r_{1})$ on $B$'s partition---with no cue bearing on a joint event.",
    "yields an impression (a target marginal) $q=(q_{0},q_{1})$ on $A$'s partition and a cue on $B$ yields an impression $r=(r_{0},r_{1})$ on $B$'s partition---with no cue bearing on a joint event.",
    r"""yields an impression (a target marginal) $q=(q_{0},q_{1})$ on $A$'s partition,
with $q_i$ the credence that $A=i$ and $q_0+q_1=1$, and a cue on $B$ yields an
impression $r=(r_{0},r_{1})$ on $B$'s partition, with $r_j$ the credence that $B=j$
and $r_0+r_1=1$, and no cue bears on a joint event. An impression is thus a
probability distribution on one attribute, on the same scale as the prior marginal
it replaces, $(\alpha,1-\alpha)$ for $q$ and $(\beta,1-\beta)$ for $r$.""")],
    "Plan 2.A as written; the manuscript never says that $q_0$ belongs to $A=0$, that "
    "$q_0+q_1=1$, or that an impression is on the prior marginal's scale. The dash construction "
    "is replaced by a clause.",
    "Notation only; every sympy script and Lean file uses $q_1=1-q_0$, $r_1=1-r_0$."))

E2.append(("E.3", "Setup 2.2, the worked example, and why a soft cue differs from a hard one (was 2.B, extended)", [(
    "insert_para", r"The \emph{gap} is the difference between either sequence-posterior and the benchmark,", r"The \emph{gap} is the difference between either sequence-posterior and the benchmark,",
    r"""\paragraph{A worked example.} Let $\alpha=\beta=\tfrac12$ and $c=\tfrac1{20}$,
so that the prior table is
$P=\bigl(\begin{smallmatrix}.30&.20\\.20&.30\end{smallmatrix}\bigr)$, each trait
judged as likely absent as present and the two believed to go together. The
credential delivers $q_0=\tfrac15$ and the letter $r_0=\tfrac7{10}$. After the
credential alone the table is
$\bigl(\begin{smallmatrix}.12&.08\\.32&.48\end{smallmatrix}\bigr)$. The
$A$-marginal is $(.20,.80)$, as delivered, and the $B$-marginal has moved from
$(.50,.50)$ to $(.44,.56)$ although no cue about $B$ has been read. Reading the
letter next resets the $B$-marginal to $(.70,.30)$ and moves the $A$-marginal
from $.20$ to $\PJ_{AB}(A{=}0)=.234$. In the reverse sequence the $A$-marginal
ends at $.20$, as delivered, and the $B$-marginal at $\PJ_{BA}(B{=}0)=.643$ in
place of $.70$. The benchmark gives $\PB(A{=}0)=.227$ and $\PB(B{=}0)=.647$. The
sequence effect is therefore $.034$ on the $A$-marginal and $.057$ on the
$B$-marginal. On the believed association it is $.0002$, since
$\assoc(\PJ_{AB})=.0273$ and $\assoc(\PJ_{BA})=.0271$, smaller than either
marginal effect by a factor above $150$. The odds ratio equals $9/4$ for the
prior, for both sequences and for the benchmark. Against the benchmark, sequence
$AB$ departs by $.053$ on the marginal read last, by $.007$ on the marginal read
first, and by $.002$ on the association.
\par
\paragraph{Why a soft cue differs from a hard one.} Suppose instead that the letter
were a hard cue, a proposition $F$ the panel becomes certain of, with a likelihood
fixed before any belief is formed, say $P(F\mid B{=}0)/P(F\mid B{=}1)=7/3$, and the
credential likewise a proposition $E$ with ratio $1/4$. Conditioning on $E$ and then
on $F$ is conditioning on $E\cap F$, so the two sequences give the same belief, which
is the benchmark $\PB$, with $A$-marginal $.227$ in either sequence. The soft letter
delivers the credence $r_0=\tfrac7{10}$ instead. The factor that credence implies is
the ratio of the odds it delivers to the odds it meets, $7/3$ when the letter is
read first against the prior marginal $\tfrac12$, but $98/33$ when it is read after
the credential, against the marginal $.44$ that the credential has already moved.
The panel knows the credence the letter left, not what that credence would have
been at another prior, and so cannot extract a portable factor from it. That
dependence of the implied factor on the belief it meets is the whole of the
sequence effect, and it vanishes at $c=0$, where the credential leaves the
$B$-marginal where it was and the two implied factors coincide.""")],
    "Plan 2.B, with the colon removed, plus the passage the author asked for on 2026-09-30, "
    "saying exactly what is special about a soft cue. A hard cue is a proposition with a "
    "belief-independent likelihood, so conditioning is defined and commutes; a soft cue fixes "
    "a credence, and the factor that credence implies is matched to the marginal in force when "
    "it arrives (7/3 read first, 98/33 read second). On one cue the two readings agree "
    "(Proposition IMM). This replaces the counterfactual-likelihood argument the audit found "
    "contradicted by Jeffrey's definition of a Bayes factor (J2). With $c=1/20$ against a bound "
    "of $1/4$ the example illustrates the ordering of magnitudes, not the asymptotic rates.",
    "Every number in both paragraphs is asserted in sympy/verify_example.py (18/18) and "
    "sympy/verify_soft_vs_hard.py (15/15, added 2026-09-30, registered in run_all.py; its symbolic checks give the exact condition, c = 0 or each cue delivering its prior marginal); the "
    "mechanism is Cripps.lean composite_AB_eq_bayes and DiaconisZabell.lean thm21."))

E2.append(("E.4", "Setup 2.3, Assumption 2 (was 2.C)", [(
    "The cues are bundled, correlated, and soft, each delivering a credence on an attribute's partition, not a decisive cue.",
    "The cues are bundled, correlated, and soft, each delivering a credence on an attribute's partition, not a decisive cue.",
    r"""The cues are bundled, correlated, and soft, each delivering a credence on an
attribute's partition, not a decisive cue, with $0<q_0<1$ and $0<r_0<1$.""")],
    "Plan 2.C, with the colon replaced by a clause.",
    "Notation only."))

E2.append(("E.5", "Related literature, new opening paragraphs on the commutativity literature (was 3.A)", [(
    r"While sequence-dependence has been of interest to empirical psychology at least since \citet{Asch1946}, the problem seems to have received interest in economics primarily through the observational learning literature.",
    r"While sequence-dependence has been of interest to empirical psychology at least since \citet{Asch1946}, the problem seems to have received interest in economics primarily through the observational learning literature.",
    r"""The closest literature concerns the commutativity of probability kinematics
itself. \citet{DiaconisZabell1982} give the condition under which two successive
Jeffrey revisions lead to the same belief, and \citet{Hawthorne2004} names the
full-adoption premise, which the present paper adopts, the Amnestic Update-Factor
Thesis, crediting that criterion to them. His survey also fixes the vocabulary.
Extensions of Jeffrey updating to sequences differ in what an experience is taken
to deliver, new probabilities for its own basis directly, a multiplicative factor
applied to the belief it meets, or a ratio between basis sentences that the prior
does not constrain. The model used here takes the impression to deliver a credence,
with the most recent one fixing its attribute outright, and the benchmark $\PB$ of
Section~\ref{sec:jeffrey} has the form of his extended update formula built on
multiplicative factors. The comparison drawn throughout is therefore between his
amnestic model and his factor models, and the identification questions this paper
raises are the ones the amnestic model brings with it and the factor models do not,
since factor revisions on distinct attributes commute. What
Section~\ref{sec:jeffrey} calls a Bayes factor is, up to normalisation, his
normed-likelihood factor, and as a ratio of two such factors it is his
likelihood-ratio factor, which he too calls a Bayes factor; on a two-element basis
all induce the same revision.
\par
The premise is disputed on two different grounds. \citet{Doring1999} objects on
normative grounds, that a sequence dependence of this size is unjustified in an
account of rational belief change. \citet[p.~115]{Hawthorne2004} objects on
psychological grounds and declines to settle them, asking ``Which extension of
Basic Jeffrey Updating is the more plausible model of human agents? I'm a logician,
not a psychologist.'' He judges amnestic updating less plausible than the
factor-based extensions, ``because it seems unlikely that we dismiss previous
experiences so completely'', and turns to the normative properties of the competing
models, where his interest lies. The ground on which the paper adopts the premise
is information-theoretic, since Jeffrey's rule is the revision that moves the
prior least in $I$-divergence while taking on the delivered credence
\citep{DiaconisZabell1982}. An objection of Hawthorne's form is a reason to measure
the degree of adoption, not a reason to discard the premise. With two cues that
degree has exactly two ends, the impression read last prevailing and the
impression read first prevailing, and Proposition~\ref{prop:ADJ} recovers where
between those ends an evaluator sits, from the rating of one trait read before and
after the second cue and against what that cue delivers.
\par
Two responses to the resulting sequence dependence have been made. One treats it
as a defect of the framework. \citet{Doring1999} argues that the sequence effect can
be pronounced enough to call for an adjustment that successive Jeffrey steps cannot
supply, though a single Jeffrey step from the original prior can. The other
re-describes the input so that the effect disappears. \citet{Field1978}
reparametrises the update so that its input is a portable factor rather than a
delivered credence, and \citet{Wagner2002} shows that when identical learning is
represented by identical Bayes factors, sequential revisions commute;
\citet{Garber1980} objects that a portable factor compounds implausibly under
repetition. The present paper takes neither route. It accepts the sequence
dependence that the delivered-credence reading entails, and asks how far the two
sequences disagree in each statistic of the resulting belief. That the
disagreement is uneven across statistics, first order in the marginals and the
share of decisions changed and second order in the believed association and the
surplus-weighted loss, is what makes the debate answerable by measurement rather
than by introspection about how completely an evaluator dismisses an earlier
impression.
\par
The insensitivity of the association is moreover not peculiar to the extension
adopted here. Each of the three extensions revises a basis by multiplying its cells
by a factor that depends on that basis alone; they differ in what fixes the factor,
not in the form of the revision. Lemma~\ref{lem:SEP} therefore covers all of them,
and under each the believed association is rescaled but never shifted. What
separates the extensions is whether there is any sequence effect for that
statistic to conceal. An observer reading the believed association alone can
accordingly no more tell which extension a population uses than detect the
sequence effect itself, while the marginals and the share of decisions changed do
both.
\par
Further afield, sequence dependence has been of interest to empirical psychology
at least since \citet{Asch1946}, and in economics has been taken up chiefly through
the observational learning literature, though it also arises in the dynamics of
discrimination \citep{Bohren2019}.""")],
    "Plan 3.A with the audit applied. D1/P3, Doring's objection is normative and Hawthorne's "
    "psychological, so the two are separated and \"disputed on psychological grounds\" goes. "
    "P2, Doring's own remedy is a single Jeffrey update from the original prior. M21, \"the two "
    "ends of his taxonomy\" becomes \"his amnestic model and his factor models\" (NL is the "
    "middle model), and the Bayes-factor identification names the normalisation and his own "
    "use of \"Bayes factor\" for LR factors (his note 20). [Corrected 2026-10-02: note 20 reports "
    "that Jeffrey, following Good, calls LR factors Bayes factors; Hawthorne does not, so the applied "
    "clause \"which he too calls a Bayes factor\" is false. The author is removing the sentence.] "
    "\"Order\" becomes \"sequence\" "
    "throughout (the manuscript's convention). Two colons and one dash removed. The Hawthorne "
    "quotations are verbatim from pp. 115-116; the second drops \"not because it is un-Bayesian, "
    "but\" before \"because\", which leaves his stated reason unchanged. Needs Doring1999 and "
    "Garber1980 in bibliography.bib (E.14).",
    "Doring.lean (fig2, gap_tends_to_one); Hawthorne.lean (amnestic_thesis, factorUpdate_comm, "
    "NL_jeffrey, med_NL_denominator); Field.lean (field_eq7_comm); Wagner2002.lean (thm31); "
    "Garber.lean (garber_table); DiaconisZabell.lean (thm51_KL_eq_iff); LemmaSEP.lean."))

E2.append(("E.6", "Related literature, Asch as evidence (was 3.B)", [(
    r"While Bayes-factor updating predicts no sequence effect anywhere \citep{Wagner2002}, the belief-adjustment accounts of order effects \citep{HogarthEinhorn1992} focus on a single evaluative anchor.",
    "end-of-sequence response modes.",
    r"""While Bayes-factor updating predicts no sequence effect anywhere
\citep{Wagner2002}, the experimental tradition has measured the phenomenon entirely
in marginals. \citet{HogarthEinhorn1992} track a single evaluative anchor.
\citet{Asch1946} is more generous and more instructive. His Experiment~VI reports,
for each of eighteen traits, the proportion of subjects choosing it over its
opposite for the person described, separately for the two reading sequences. That
the sequence effect differs in size across statistics is already visible there,
although the differences Asch reports are all among marginals, which the paper
treats alike. Between the sequences, \emph{restrained} moves from 64 to 9 per cent
and \emph{good-looking} from 74 to 35, while \emph{serious} moves from 97 to 100,
\emph{persistent} from 82 to 87 and \emph{reliable} from 84 to 91. Some statistics
swing enormously and others scarcely move, and Asch attributes the difference to
the content of the terms rather than to their position. Yet eighteen marginals
separate full adoption from partial adoption no better than one. Separating them
needs the rating of one trait before and after the second cue and against what
that cue delivers (Proposition~\ref{prop:ADJ}), and the check-list asks whether a
trait fits and never how strongly two traits are believed to go together
beforehand, so the prior association cannot be formed from such data at all. What
the paper adds is therefore not that more ratings are needed but that a second
object must be elicited. Seeing the sequence effect requires only a marginal read
in both sequences, which Asch has; locating the mechanism behind it requires the
prior association as well, which he does not.""")],
    "Plan 3.B with the audit applied. A11, the check-list is a forced choice between "
    "opposite pairs. A15, Asch does give an account of the uneven effects, by content. P13, "
    "what is never elicited is the prior association, not the joint. The ADJ clause now states "
    "what the recovery formula uses, three ratings of one marginal, not a marginal across "
    "values of $c$. \"amnestic updating\" becomes \"full adoption\". Colon removed. All eight "
    "percentages are correct and in the right columns.",
    "Asch.lean (quoted_all, d_neg_exactly, stimulus_disjoint_checklist); Anchoring.lean "
    "(dampedB_deviation, the identification)."))

E2.append(("E.7", "Related literature, Bohren-Imas-Rosenberg, after the identification paragraph (was 3.D)", [(
    "insert_para", "the identifying variable is the reading sequence, which pooled data discard.", "the identifying variable is the reading sequence, which pooled data discard.",
    r"""The map from partiality to behaviour in \citet{Bohren2019} is Bayesian. A
belief gap between groups is transmitted to evaluations, attenuated by the
precision of the signal and along the history, and it vanishes as judgement becomes
perfectly objective. Under the reading adopted here that map does not hold. A
Jeffrey step sets the marginal of the attribute it addresses to the delivered
credence, so an evaluator who takes the impression of quality last evaluates two
workers alike whatever their prior beliefs about the groups, and the belief gap is
silenced without any gain in objectivity. Under partial adoption with weight
$\omega$ on the impression, the rule of Proposition~\ref{prop:LAD} leaves a fraction
$1-\omega$ of the belief gap. What credence-input updating does is therefore not to
add a further source of discrimination to their three, but to relocate where
partiality must sit in order to act. Lodged in the prior it is silenced, lodged in
the impression it passes through untouched, and their framework has no parameter
for the latter.""")],
    "Plan 3.D with the audit applied. P17b, the belief gap is also attenuated along the "
    "history (their Proposition 2), so \"vanishes only as\" becomes \"vanishes as\". Colon "
    "removed. Goes directly after C.15's paragraph, which carries the Heckman precedent that "
    "plan 3.C proposed (3.C is superseded by C.15). Presupposes E.10 and the Bohren2019 entry. "
    "Rebased on da5e3cff: the author deleted the amalgamation paragraph that held the old "
    "insertion point, so the paragraph now goes directly after the Heckman paragraph, as "
    "intended. That paragraph already names Bohren et al. for identification from the history "
    "of evaluations, so this paragraph's first sentence repeats it and should be merged or "
    "shortened when applied (rule 5). Revised 2026-10-05: that repetition removed (the paragraph "
    "now opens with the Bayesian map), the three sources not named again since entry 3.8 names them, "
    "the citation of Proposition ADJ (no longer in the paper) replaced by the rule Proposition LAD now "
    "defines, whose damped step leaves the fraction $1-\\omega$ (Anchoring.lean dampedB_deviation), and "
    "\"her\" replaced by \"their\".",
    "check_pinning_kills_partiality.py (7/7, D = 0 exactly for arbitrary group priors and "
    "covariances); Anchoring.lean dampedB_deviation; BohrenImasRosenberg.lean (prop1_decreasing, "
    "prop2_decreasing, endo_gap)."))

E2.append(("E.8", "Section 4, title and opening (was 4.A)", [
    (r"\section{Sequence effects at the individual level}", r"\section{Sequence effects at the individual level}",
     r"""\section{Which statistics register the sequence}"""),
    ("Since Bayesian conditioning is sequence-independent by definition, we measure sequence effects",
     r"How these quantities are carried over in aggregate are detailed in Section~\ref{sec:aggregation}.",
     r"""This section asks, of each statistic of a single evaluator's belief, whether the
two reading sequences move it at first order in the prior covariance $c$ or only at
second order. Two primitive statistics carry the comparison with the sequence-free
benchmark $\PB$ of Section~\ref{sec:jeffrey}, the \emph{gap}, the distance of the
evaluator's belief from $\PB$ (Definition~\ref{def:indgap}), and the
\emph{score-gap} $s(P)=\langle\vv,P\rangle$, the same distance read through the
decision weights and set against a threshold $\tau$.
Section~\ref{sec:individual_primitives} shows that both are first order.
\par
The belief statistics and the decision statistics are read from the same belief.
The belief statistics are the \emph{marginal probability} of each attribute and the
\emph{believed cross-attribute association}; the decision statistics are the
surplus-weighted loss $L(c)$ and the decision \emph{flip}, the indicator that the
sequence changes the evaluator's decision. Whether a statistic registers the
sequence is settled here, for one evaluator; Section~\ref{sec:aggregation} shows
that averaging over evaluators who met the cues in either sequence leaves the
classification unchanged across an open set of priors and every interior
mixture.""")],
    "Plan 4.A, colon removed. The section is organised on the statistic rather than on the "
    "individual-versus-aggregate axis, which ORD and PRO show to be inert. The third paragraph "
    "(why these statistics; FGT, corrected by C.13) is unchanged.",
    "DIV and SCR (both primitives first order); ORD and PRO (the classification is settled "
    "before averaging)."))

E2.append(("E.9", "Section 5, title and opening (was 5.A)", [
    (r"\section{Aggregated Effects of Cue-Sequence}", r"\section{Aggregated Effects of Cue-Sequence}",
     r"""\section{Averaging over sequences does not change the classification}"""),
    ("We now describe how the effect of arrival order on statistics discussed in the previous section fares in the population aggregate.",
     "rather than the mechanism of aggregation.",
     r"""The previous section classified statistics for one evaluator. This section shows
that the classification survives averaging over a population in which a fraction
$\lambda\in[0,1]$ of evaluators meet the $A$-cue first (the credential in the
example) and the rest the $B$-cue first (the letter), so that the mean belief under
Jeffrey conditioning is $\Pbar_\lambda=\lambda\,\PJ_{AB}+(1-\lambda)\,\PJ_{BA}$.
Across an open set of priors, what makes a statistic first order or second order is
the statistic itself and not the mixture $\lambda$, since the mean belief lies in
the plane through $\PB$ spanned by the leading directions of the two sequences,
and a statistic's order is fixed by its differential on that plane
(Propositions~\ref{prop:ORD} and~\ref{prop:PRO}).""")],
    "Plan 5.A, colon removed. \"Largely the nature of the statistic itself\" hedged an exact "
    "result. Presupposes E.10.",
    "PropPRO.lean (annihilator_eq_span, propPRO_uniqueness); PropORD.lean."))

E2.append(("E.10", "Section 5, Proposition ORD after the definition of protection (was 5.B, ADJ moved to E.15)", [
    ("insert_para", r"\emph{unprotected} if that distortion is first order, $\bigO(c)$.", r"\emph{unprotected} if that distortion is first order, $\bigO(c)$.",
     r"""The distortion compares the population average with the benchmark. The same
classification can be reached without either ingredient, by reading the sequence
effect of Definition~\ref{def:seqeffect} through the statistic itself. For a smooth
statistic $F$ the sequence effect read through it is $F(\PJ_{AB})-F(\PJ_{BA})$, and
it involves neither the benchmark $\PB$ nor the mixing weight $\lambda$.
\par
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
sequence effect on the believed association is $\bigO(c^{2})$ exactly. Across an
open set of priors, a smooth statistic has a second-order sequence effect if and
only if it is protected (Definition~\ref{def:protection}); both conditions hold
exactly when the differential of the statistic at independence annihilates
$\mathrm{span}\{R_1,R_2\}$ (Proposition~\ref{prop:PRO} below). Nothing in the
display involves $\PB$ or $\lambda$. The classification is decided by the two
reading sequences, and every mixture $\Pbar_\lambda$ inherits it.
\end{proposition}
\par
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
\begin{aligned}
  \langle\nabla F,\Delta_{\mathrm{seq}}\rangle
    &=\kappa\,\langle\nabla F,R_1\rangle-\kappa'\,\langle\nabla F,R_2\rangle,\\
  \langle\nabla F,M_\lambda\rangle
    &=\lambda\kappa\,\langle\nabla F,R_1\rangle+(1-\lambda)\kappa'\,\langle\nabla F,R_2\rangle;
\end{aligned}
\]
across an open set of priors $\kappa$ and $\kappa'$ vary independently, so
either vanishes identically exactly when
$\langle\nabla F,R_1\rangle=\langle\nabla F,R_2\rangle=0$.
\end{proof}
"""),
    ("A particularly surprising finding is that for a smooth statistic of the belief, the answer is settled before any averaging over the aggregate takes place.",
     "A particularly surprising finding is that for a smooth statistic of the belief, the answer is settled before any averaging over the aggregate takes place.",
     r"""A particularly surprising finding is that for a smooth statistic of the belief,
the answer is settled before any averaging over the aggregate takes place
(Proposition~\ref{prop:ORD}).""")],
    "Plan 5.B, mathematics unchanged, prose corrected. Only Proposition ORD stays in Section 5, "
    "where it belongs with the classification at full adoption. The adoption-weight material, "
    "Proposition ADJ with its worked example and Proposition LAD, moves to the robustness "
    "subsection of Section 6 (E.15, 2026-10-01), so that Sections 4 and 5 state the results "
    "under the two premises and Section 6 relaxes them. Colons before displays are kept; colons "
    "in prose are replaced. ORD must go in before E.1, E.5, E.6, E.7, E.9, E.11 and E.15, which "
    "cite it.",
    "PropORD.lean (lemmaORD_gap, propORD_Amarg, propORD_Bmarg, propORD_const); "
    "verify_ORD.py (49/49)."))

E2.append(("E.11", "Scope, rival mechanisms (was 6.A)", [(
    "insert_para", "is outside the scope of stated results.", "is outside the scope of stated results.",
    r"""A separate scope question concerns rival mechanisms rather than broken
assumptions. Sequence effects have an explanation that competes with full adoption.
In the belief-adjustment model of \citet{HogarthEinhorn1992}, when the first cue is
adopted in full and later cues only in part the first impression is protected, and
at zero adoption of later cues it is never moved, whereas when every cue is adopted
only in part from a prior anchor the same model predicts recency. That first
account and full adoption point in opposite directions, since the protected
impression is the first one there and the last one here. An observable separates
them. Under partial adoption of the second cue with weight $\omega$
(Proposition~\ref{prop:ADJ}), the marginal that ignores the prior association is
the last-read one at $\omega=1$, the first-read one at $\omega=0$, and neither in
between. A reading on which neither marginal ignores the association is consistent
with an interior weight and with the benchmark alike; the two are separated by the
sequence comparison, under which the benchmark shows no sequence effect at any $c$
and an interior weight shows one already at $c=0$ (Proposition~\ref{prop:ADJ}).
Since every rule here is a separable reweighting, all of them carry the prior's
odds ratio (Lemma~\ref{lem:SEP}), so the odds ratio separates none of them and the
two marginals are the only statistics that do. Three cautions bound the claim. The
classification covers this one-parameter family, not every conceivable mechanism.
\citet{Asch1946} is evidence that sequence moves marginals, six stimulus terms read
in two sequences moving the proportions on eighteen response traits, and not
evidence for either endpoint, since his own account is that early terms set a
direction for the reading of later ones, so that what a later cue delivers depends
on its position, a mechanism outside the family here, in which each cue delivers
the same credence in either position. And full adoption is a substantive
commitment rather than a consequence of the level reading. \citet{Hawthorne2004}
objects that it ``seems implausible that the most recent experience or
non-propositional state should completely dictate belief strengths for basis
sentences, with no regard for the import of previous experiences or states''. The
objection applies here, and his own two-basis example shows the overwriting
explicitly, so that the credential's implication for trustworthiness is erased
once the letter fixes that attribute. The adoption weight is his objection stated
as a parameter, and the reply offered here is not that the objection misses but
that the weight is recovered from three ratings of one marginal rather than
assumed.""")],
    "Plan 6.A with the audit applied. P10, the belief-adjustment model protects the first "
    "impression only when the first cue is the anchor and later cues are partly adopted; damping "
    "every cue gives recency, so the rival is stated as that case. P14, Experiment VI has six "
    "stimulus terms and eighteen response traits. P9, Hawthorne's objection follows his "
    "two-basis medical example, so the claim that his illustration misses a one-cue-per-attribute "
    "model is dropped. The recovery uses three ratings of one marginal (plan's own correction). "
    "Colon removed. Goes after the scope paragraph, inside the Scope subsection that E.15 "
    "opens, and before E.15.",
    "HogarthEinhorn.lean (eq8_estimation_first_dominates, appB_recency, twoAttr_oneSided_orderEffect); "
    "Anchoring.lean; Asch.lean (seriesA_length, checkListI_length); Hawthorne.lean "
    "(med_overwrite; quotation pp. 98-99 verbatim)."))

E2.append(("E.15h", "Section 6, the Scope subsection heading and first sentence", [
    ("We now revisit how Assumptions~\\ref{as:localc}--\\ref{as:surplus} in Section~\\ref{sec:assumptions} define scope for any conclusions made from the model.",
     "We now revisit how Assumptions~\\ref{as:localc}--\\ref{as:surplus} in Section~\\ref{sec:assumptions} define scope for any conclusions made from the model.",
     r"""\subsection{Scope}\label{sec:scope-assumptions}
\par
We now revisit how Assumptions~\ref{as:localc}--\ref{as:surplus} in
Section~\ref{sec:assumptions} define the scope of the conclusions drawn from the
model.""")],
    "Section 6 splits into two subsections (author's title \"Scope and robustness\" kept; the "
    "author may drop \"and robustness\"). This adds the Scope heading and repairs \"define scope for "
    "any conclusions made from the model\". The second subsection is E.15.",
    "None needed."))

E2.append(("E.15r", "Introduction, roadmap, the Section 6 sentence", [
    ("Section~\\ref{sec:scope} states the scope of results and limitations that would break the results.",
     "Section~\\ref{sec:scope} states the scope of results and limitations that would break the results.",
     r"""Section~\ref{sec:scope} states the scope of the results and the limitations that
would break them, shows that the second-order results require full adoption of the
later cue, and gives a test of that condition from the ratings.""")],
    "The roadmap's Section 6 sentence names what that section now does: the scope of the "
    "results, that the second-order results require full adoption of the later cue, and the "
    "test of that condition (E.15).",
    "The weight from the ratings: the Section 6 sentence of entry C.24 (Anchoring.lean, "
    "dampedB_mB1); Proposition ADJ itself is now in PAPER_B_ADDENDUM.tex."))

E2.append(("E.15", "Section 6, the subsection on partial adoption and factor inputs (Propositions ADJ, LAD and FAC)", [
    ("insert_para", "is outside the scope of stated results.", "is outside the scope of stated results.",
     r"""\subsection{Partial adoption and factor inputs}\label{sec:robust}
\par
The results of Sections~\ref{sec:individual} and~\ref{sec:aggregation} rest on two
premises about a cue. It delivers a credence on its own attribute rather than a
factor, and the credence is adopted in full. Full adoption is the setting in which
an evaluator adopts a cue alike whichever position it arrives in, so that the
sequence can act only through the believed link between the attributes. A weight on
the later cue that depends on its position is a property of the evaluator rather than
of the evidence, and it is the subject of the belief-adjustment literature
\citep{HogarthEinhorn1992}. Whether a population adopts in full can be read from its
ratings, since Proposition~\ref{prop:ADJ} below recovers the adoption weight from
three marginals of one reading group, and Propositions~\ref{prop:LAD}
and~\ref{prop:FAC} say what to expect when it does not, under either reading of the
cues. What partial adoption leaves in place is the odds ratio, which is the same
in both sequences at every weight, and a second-order class of statistics that no
longer contains the association. Table~\ref{tab:robust} collects the answers.
\par
\paragraph{The two channels.} Sequence dependence has two channels in this
setting, and the results of Sections~\ref{sec:individual} and~\ref{sec:aggregation}
concern one of them. One is the association channel. A cue on one attribute moves
the belief about the other through the prior association, so the second cue meets
a belief the first has already changed. The association channel's effect is first
order in $c$ and vanishes at independence, where each cue sets its own marginal and
touches nothing else (Proposition~\ref{prop:IMM}). The other is the position
channel. If the cue read second is adopted only in part, then which cue is weakened
depends on the sequence, and the two sequences differ even at $c=0$, by
$(1-\omega)(\alpha-q_0)$ on the competence marginal and $-(1-\omega)(\beta-r_0)$ on
the trustworthiness marginal. The position channel is not a feature of updating on
delivered credences. A Bayes-factor update that gives the second cue's factor the
weight $\omega$ is sequence-dependent in the same way, since commutation requires
the same factor in either position \citep{Wagner2002}. Under full adoption the
position channel is absent and the association channel is the only one, which is
the setting of the results. Under partial adoption the position channel is present
at $c=0$ and moves every marginal at order zero, while the believed association
still does not differ between sequences at $c=0$ and differs at first order
(Proposition~\ref{prop:LAD}). The classification by order in $c$ shifts by one order,
each statistic keeping its place relative to the others, and the odds ratio is the
same in both sequences at every weight. The mean belief of a population mixing the two sequences
acquires a cross-product association of
$-\lambda(1-\lambda)(1-\omega)^2(\alpha-q_0)(\beta-r_0)$ that no member holds, and the
separation between the share of decisions changed and the loss is lost. What the
paper says about the position channel is confined to Propositions~\ref{prop:ADJ}
and~\ref{prop:LAD}, the first recovering its strength from the ratings and the
second stating what it does to the order in $c$ at which each statistic registers
the sequence. The
position channel is a temporal bias of the observer, present whether or not the
attributes are believed related, while the association channel runs through what
one cue implies about the other and exists only when they are.
Table~\ref{tab:settings} sets the two settings side by side."""),
    ("insert_cont", "is outside the scope of stated results.", "is outside the scope of stated results.",
     r"""\paragraph{Partial adoption.} The two sequences of Sections~\ref{sec:individual} and~\ref{sec:aggregation} adopt
each delivered credence in full. Suppose instead
that the second cue is adopted only in part, so that after the first Jeffrey step
the response to the second cue is a Jeffrey step on its partition to the target
$(1-\omega)\,m+\omega\,r_1$, where $m$ is the marginal the first step left and
$0\le\omega\le1$ is the adoption weight of Section~\ref{sec:setup}. The rule is the
averaging form of the belief-adjustment model,
$S_k=(1-w_k)S_{k-1}+w_k s(x_k)$ \citep[Eq.~4]{HogarthEinhorn1992}, applied to the
second cue only, which is their end-of-sequence form for two items with a constant
weight, and embedded in the joint law by a Jeffrey step. Write $P^{\omega}_{AB}$
and $P^{\omega}_{BA}$ for the two sequences under this rule; $\omega=1$ recovers
$\PJ_{AB}$ and $\PJ_{BA}$, and $\omega=0$ ignores the second cue. Let $P^{A}$ and
$P^{B}$ denote the belief after the $A$-cue alone and after the $B$-cue alone.
\par
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
for the same marginal. At $c=0$ the third line equals $(1-\omega)(\alpha-q_0)$,
so a partially adopting evaluator shows a sequence effect at independence, where
the fully adopting evaluator (Proposition~\ref{prop:IMM}) and the benchmark
show none. Whenever $P^{A}(B{=}1)\neq r_1$, the first line gives the weight from
three marginals of one reading group, for every $c$,
\[
  \omega=\frac{P^{A}(B{=}1)-P^{\omega}_{AB}(B{=}1)}{P^{A}(B{=}1)-r_1}.
\]
\end{proposition}
\par
\begin{proof}
With $Q$ the belief after the first step and $t=(t_0,t_1)$,
$t_1=(1-\omega)\,Q(B{=}1)+\omega r_1$, the target of the second, column $j$
is multiplied by $t_j/Q(B{=}j)$, so that
\[
\begin{aligned}
  P(B{=}1)&=t_1,\\
  P(A{=}1)&=\sum_j Q(A{=}1,B{=}j)\,\frac{t_j}{Q(B{=}j)}\\
  &=(1-\omega)\,Q(A{=}1)+\omega\sum_j Q(A{=}1,B{=}j)\,\frac{r_j}{Q(B{=}j)}.
\end{aligned}
\]
In sequence $AB$, $Q=P^{A}$, $Q(A{=}1)=q_1$, and the last sum is
$\PJ_{AB}(A{=}1)$, giving
\[
  P^{\omega}_{AB}(B{=}1)-r_1=(1-\omega)\bigl[P^{A}(B{=}1)-r_1\bigr],\qquad
  P^{\omega}_{AB}(A{=}1)-q_1=\omega\bigl[\PJ_{AB}(A{=}1)-q_1\bigr].
\]
In sequence $BA$, by symmetry,
$P^{\omega}_{BA}(A{=}1)=(1-\omega)\,P^{B}(A{=}1)+\omega q_1$; subtracting
gives the third line. Neither bracket vanishes identically, since
\[
  P^{A}(B{=}1)-r_1\Big|_{c=0}=r_0-\beta,\qquad
  \PJ_{AB}(A{=}1)-q_1=-c\,\frac{q_0(1-q_0)(r_0-\beta)}{\alpha\beta(1-\alpha)(1-\beta)}+\bigO(c^2),
\]
the second by Proposition~\ref{prop:DRF}, whose proof also gives
$\PJ_{AB}(A{=}1)-\PB(A{=}1)=\bigO(c^2)$, hence the coefficient claim. At
$c=0$ a step leaves the other marginal unchanged, so
\[
  \PJ_{AB}(A{=}1)=q_1,\qquad P^{B}(A{=}1)=1-\alpha,\qquad
  P^{\omega}_{AB}(A{=}1)-P^{\omega}_{BA}(A{=}1)=(1-\omega)(\alpha-q_0).
\]
For $P^{A}(B{=}1)\neq r_1$ the first line gives
\[
  1-\omega=\frac{P^{\omega}_{AB}(B{=}1)-r_1}{P^{A}(B{=}1)-r_1},\qquad
  \omega=\frac{P^{A}(B{=}1)-P^{\omega}_{AB}(B{=}1)}{P^{A}(B{=}1)-r_1}.
\]
\end{proof}
\par
In the example of Section~\ref{sec:jeffrey}, an evaluator who reads the credential
first rates trustworthiness at $.56$ before the letter, the letter alone delivers
$.30$, and a final rating of $.43$ gives $\omega=(.56-.43)/(.56-.30)=\tfrac12$."""),
    ("insert_cont", "is outside the scope of stated results.", "is outside the scope of stated results.",
     r"""Proposition~\ref{prop:ADJ} reads the weight from the ratings. The next result says
what the weight does to the order in $c$ at which each statistic first differs between
the two sequences. A statistic is \emph{protected
for every pair of cues} if its differential at every independent belief is a
multiple of the differential of the association, the condition of
Proposition~\ref{prop:PRO} imposed at every point of the independence surface
rather than at $q\otimes r$ alone; the association and the correlation coefficient
are examples.
\par
\renewcommand{\theproposition}{LAD}%
\begin{proposition}[partial adoption]\label{prop:LAD}
Let $0\le\omega\le1$, $t_0=(1-\omega)\beta+\omega r_0$, $s_0=(1-\omega)\alpha+\omega q_0$,
$t=(t_0,1-t_0)$, $s=(s_0,1-s_0)$ and $Z=\alpha\beta(1-\alpha)(1-\beta)$.
\begin{enumerate}
\item[(i)] At $c=0$ the two sequences end at the independent beliefs $q\otimes t$
and $s\otimes r$, and
\[
  P^{\omega}_{AB}-P^{\omega}_{BA}\big|_{c=0}
  =(1-\omega)(\beta-r_0)\,R_1+(1-\omega)(q_0-\alpha)\,R_2 .
\]
Every marginal therefore differs between the sequences at order zero in $c$, the
$A$-marginal by $(1-\omega)(\alpha-q_0)$, while a statistic protected for every pair
of cues differs between them by $\bigO(c)$.
\item[(ii)] The believed association differs between the sequences by
\[
  \assoc(P^{\omega}_{AB})-\assoc(P^{\omega}_{BA})=c\,\frac{(1-\omega)H}{Z}+\bigO(c^{2}),
\]
\[
  H=q_0q_1\bigl[\beta(1-\beta)+\omega(\beta-r_0)^2\bigr]
   -r_0r_1\bigl[\alpha(1-\alpha)+\omega(\alpha-q_0)^2\bigr],
\]
which is first order whenever $\omega<1$ and $H\neq0$, and second order at $\omega=1$.
\item[(iii)] A statistic protected for every pair of cues differs between the
sequences by $\bigO(c^{2})$ for every $\omega$ if and only if its differential at
every independent belief is a multiple of the differential of the log odds ratio,
as for $\assoc(P)/\bigl(P(A{=}0)P(A{=}1)P(B{=}0)P(B{=}1)\bigr)$. The odds ratio
itself is the same in both sequences for every $c$ and every $\omega$.
\end{enumerate}
\end{proposition}
\par
\begin{proof}
Every Jeffrey step, damped or not, multiplies the rows or the columns of the table
by constants, so after both steps $P^{\omega}_{\sigma}(i,j)=a_i b_j P(i,j)$ for some
factors $a_0,a_1,b_0,b_1$. Such a rescaling multiplies the association by
$a_0a_1b_0b_1$ and leaves the odds ratio unchanged, and $\assoc(P)=c$, so
$\assoc(P^{\omega}_{\sigma})=c\,k_\sigma$ with $k_\sigma=a_0a_1b_0b_1$, and the odds
ratio claim of (iii) follows. (i) At $c=0$ a step on one attribute leaves the other
marginal unchanged (Proposition~\ref{prop:IMM}), so sequence $AB$ reaches $q\otimes\beta$
and then $q\otimes t$, and sequence $BA$ reaches $s\otimes r$; subtracting gives the
display. A statistic protected for every pair of cues has a differential that
annihilates both tangent directions of the independence surface at each of its
points (Lemma~\ref{lem:ASC}), so it is constant on the surface and takes the same
value at $q\otimes t$ and $s\otimes r$. (ii) At $c=0$ the factors give
$k_{AB}=q_0q_1t_0t_1/Z$ and $k_{BA}=s_0s_1r_0r_1/Z$, and $k_{AB}-k_{BA}=(1-\omega)H/Z$.
(iii) Near the independence surface a statistic protected for every pair of cues
is $F=F_0+\assoc\cdot h$ with $h$ smooth, and writing $h=g/m$ with
$m=P(A{=}0)P(A{=}1)P(B{=}0)P(B{=}1)$, the first-order coefficient of
$F(P^{\omega}_{AB})-F(P^{\omega}_{BA})$ is
$k_{AB}\,h(q\otimes t)-k_{BA}\,h(s\otimes r)=\bigl[g(q\otimes t)-g(s\otimes r)\bigr]/Z$,
since $m(q\otimes t)=q_0q_1t_0t_1$ and $m(s\otimes r)=s_0s_1r_0r_1$. It vanishes for
every pair of cues and every $\omega$ exactly when $g$ is constant on the
independence surface, that is when the differential of $F$ there is a multiple of
the differential of $\assoc/m$, which at an independent belief equals the
differential of the log odds ratio.
\end{proof}
\par
Partial adoption therefore moves the sequence effect on each belief statistic other
than the odds ratio one order earlier in $c$, the marginals staying ahead of the
association and the association ahead of the odds ratio, which never moves. Decisions
do not keep their places. At $c=0$ the
benchmark is $q\otimes r$ while sequence $AB$ ends at $q\otimes t$, so under partial
adoption an evaluator's score departs from the benchmark's at order zero, and the
share of decisions the sequence changes and the surplus-weighted loss are then both
of order zero. The separation of Theorem~\ref{thm:LOS} is a property of full
adoption."""),
    ("insert_cont", "is outside the scope of stated results.", "is outside the scope of stated results.",
     r"""\paragraph{Factor inputs.} Under the benchmark reading each cue supplies a factor
on its own attribute, $a=(a_0,a_1)$ for the credential and $b=(b_0,b_1)$ for the
letter, and the belief after both cues is the prior with cell $(i,j)$ multiplied by
$a_ib_j$ and renormalised. This is the reading of the factor-based variants of
\citet{Hawthorne2004}, and with both factors applied in full the two sequences give
the same belief for every $c$, since the same cells are multiplied by the same
numbers whichever cue comes first \citep{Wagner2002}. Partial adoption has a
counterpart under this reading. Let the factor read second be adopted only in part,
so that sequence $AB$ applies $a$ and then $b^{\omega}=(b_0^{\omega},b_1^{\omega})$
and sequence $BA$ applies $b$ and then $a^{\omega}$, with $\omega=1$ the benchmark
and $\omega=0$ the second cue ignored.
\par
\renewcommand{\theproposition}{FAC}%
\begin{proposition}[factor inputs]\label{prop:FAC}
Let $a$ and $b$ be positive and $0\le\omega\le1$. Write $K_\sigma$ for the product
of the four factors that sequence $\sigma$ applies and $S_\sigma$ for its
normalising sum.
\begin{enumerate}
\item[(i)] At $\omega=1$ the two sequences give the same belief for every $c$.
\item[(ii)] At $c=0$ each sequence ends at an independent belief. The $A$-marginals
are $q_1$ in sequence $AB$ and
$(1-\alpha)a_1^{\omega}/\bigl(\alpha a_0^{\omega}+(1-\alpha)a_1^{\omega}\bigr)$ in
sequence $BA$, and they agree if and only if $a_0=a_1$ or $\omega=1$. Every
marginal therefore differs between the sequences at order zero whenever the cue is
informative and $\omega<1$, while a statistic protected for every pair of cues
differs by $\bigO(c)$.
\item[(iii)] In sequence $\sigma$ the believed association equals
$c\,K_\sigma/S_\sigma^{2}$ exactly, so it is zero at $c=0$ and first order for
$\omega<1$ whenever $K_{AB}/S_{AB}^{2}$ and $K_{BA}/S_{BA}^{2}$ differ at $c=0$. A
statistic whose differential at every independent belief is a multiple of that of
the log odds ratio differs between the sequences by $\bigO(c^{2})$ for every
$\omega$, and the odds ratio is the prior's in both sequences for every $c$ and
$\omega$.
\end{enumerate}
\end{proposition}
\par
\begin{proof}
Applying a factor on $A$ and then one on $B$ multiplies cell $(i,j)$ by $a_ib_j$
whichever is applied first, and renormalisation divides every cell by the same sum,
which gives (i). Both steps rescale rows or columns, so the odds ratio is the
prior's, and the association of the rescaled table is $K_\sigma\assoc(P)=cK_\sigma$,
divided by $S_\sigma^{2}$ after renormalisation. At $c=0$ the prior is
$\alpha\otimes\beta$ and the rescaled table is
$(\alpha_0a_0,\alpha_1a_1)\otimes(\beta_0b_0,\beta_1b_1)$ up to the normalising
sum, an independent belief whose $A$-marginal is
$(1-\alpha)a_1/(\alpha a_0+(1-\alpha)a_1)$ for the factor applied on $A$. With
$a_i=q_i/\alpha_i$ this is $q_1$, and with $a^{\omega}$ in its place the two values
agree exactly when $a_1a_0^{\omega}=a_0a_1^{\omega}$, that is when
$(a_1/a_0)^{1-\omega}=1$. The claims about protected statistics follow as in
Proposition~\ref{prop:LAD}, since both sequences end at independent beliefs. For
the last claim, at $c=0$ the product of the four marginals is
$ZK_\sigma/S_\sigma^{2}$, so the first-order factor of $\assoc/m$ is $1/Z$ in both
sequences and the argument of Proposition~\ref{prop:LAD}(iii) applies.
\end{proof}"""),
    ("insert_cont", "is outside the scope of stated results.", "is outside the scope of stated results.",
     r"""\begin{table}[htbp]
\centering
\small
\begin{tabular}{@{}p{3.4cm}cccc@{}}
\toprule
& \multicolumn{2}{c}{\textbf{Credence delivered}} & \multicolumn{2}{c}{\textbf{Factor delivered}} \\
\textbf{Sequence effect on} & $\omega=1$ & $\omega<1$ & $\omega=1$ & $\omega<1$ \\
\midrule
Marginals & $\Theta(c)$ & $\Theta(1)$ & $0$ & $\Theta(1)$ \\
Believed association & $\bigO(c^{2})$ & $\Theta(c)$ & $0$ & $\Theta(c)$ \\
Statistics agreeing with the log odds ratio to first order & $\bigO(c^{2})$ & $\bigO(c^{2})$ & $0$ & $\bigO(c^{2})$ \\
Odds ratio & $0$ & $0$ & $0$ & $0$ \\
Share of decisions changed & $\Theta(c)$ & $\Theta(1)$ & $0$ & $\Theta(1)$ \\
Surplus-weighted loss & $\bigO(c^{2})$ & $\Theta(1)$ & $0$ & $\Theta(1)$ \\
\bottomrule
\end{tabular}
\caption{The between-sequence effect of each statistic, by what a cue delivers and
how fully the later cue is adopted. Entries are orders in the prior covariance $c$,
generic in the prior and the cues, and $0$ means no sequence effect at any $c$. The
decision rows of the partial-adoption columns read Theorem~\ref{thm:LOS} with a
score gap of order zero and are not separately verified. The columns are, in turn,
Sections~\ref{sec:individual} and~\ref{sec:aggregation}, Proposition~\ref{prop:LAD},
the benchmark of Section~\ref{sec:setup}, and Proposition~\ref{prop:FAC}.}
\label{tab:robust}
\end{table}"""),
    ("insert_cont", "is outside the scope of stated results.", "is outside the scope of stated results.",
     r"""\paragraph{Opposite predictions about one procedure.} Whether a procedure removes
the sequence effect depends on which channel it removes. Consider rubric scoring,
in which the rating on each attribute is fixed by the document on that attribute
and a set scale, so that what each document delivers no longer depends on what was
read first. Under full adoption the rubric removes the position channel and leaves
the association channel untouched. The recorded scores carry the delivered
credences in either sequence, the belief about the attribute read second agrees
with its score, and the belief about the attribute read first sits at $cK$ from its
score (Proposition~\ref{prop:DRF}), so the sequence effect lives in that belief and
in the decision made from it, vanishes when the attributes are believed unrelated
(Proposition~\ref{prop:IMM}), and is invisible in the scores and in the association
between them. Under partial adoption the rubric removes the position channel from
the record only. The belief about the attribute read second sits at
$(1-\omega)[P^{A}(B{=}1)-r_1]$ from its score (Proposition~\ref{prop:ADJ}), which at
independence equals $(1-\omega)(r_0-\beta)$, so the two reading sequences differ
even when the attributes are believed unrelated. The two settings therefore predict
opposite things about the same procedure, and the difference is observable. In the
rubric setting $r_1$ is on the record, so Proposition~\ref{prop:ADJ} needs only the
belief about trustworthiness after the credential alone and after both documents
to say which prediction holds.
""")
,    ("insert_cont", "is outside the scope of stated results.", "is outside the scope of stated results.",
     r"""\begin{table}[htbp]
\centering
\small
\begin{tabular}{@{}p{2.6cm}p{5.4cm}p{5.4cm}@{}}
\toprule
& \textbf{Full adoption, $\omega=1$} (association channel only)
& \textbf{Partial or no adoption, $\omega<1$} (position channel operates) \\
\midrule
Where the sequence dependence arises &
In the rule. The inputs $q,r$ are the same in either position; the sequence matters
because two rigid steps on correlated partitions do not commute
\citep{DiaconisZabell1982}. &
In the inputs. What the second cue delivers depends on its position; the rule adds
nothing new at $c=0$. \\
\addlinespace[3pt]
Source in the model &
Association only. Zero at $c=0$ (Proposition~\ref{prop:IMM}). &
Position, present at $c=0$, plus association when $c\neq0$. \\
\addlinespace[3pt]
Status of the inputs &
Independent of the sequence. The credential delivers $q$ whether or not the letter
was read. &
Dependent on the sequence. The later cue is adopted only in part, so what it
delivers depends on being second. \\
\addlinespace[3pt]
Size of the effect &
Small, first order in $c$, spread unevenly across statistics. &
Blunt. $(1-\omega)(\alpha-q_0)$ on a marginal whatever $c$ is
(Proposition~\ref{prop:ADJ}). \\
\addlinespace[3pt]
Marginals &
Last-read exact, first-read drifts at first order (Proposition~\ref{prop:DRF}). &
Both differ at zeroth order between sequences. \\
\addlinespace[3pt]
Believed association &
Second order, between sequences and pooled (Propositions~\ref{prop:DEC}
and~\ref{prop:PRO}). &
Zero between sequences at $c=0$, first order in $c$. Pooled, nonzero at $c=0$,
$-\lambda(1-\lambda)(1-\omega)^2(\alpha-q_0)(\beta-r_0)$. \\
\addlinespace[3pt]
Odds ratio &
Identical across sequences, every $c$ (Lemma~\ref{lem:SEP}). &
Identical across sequences, every $c$ (Lemma~\ref{lem:SEP}). The one statistic that
does not move. \\
\addlinespace[3pt]
Classification by order in $c$ &
Is the contribution. Which statistics are protected, and why. &
Shifts by one order, each statistic keeping its place. Marginals at order zero, protected
statistics at first order, the odds-ratio shadow at second, the odds ratio never
(Proposition~\ref{prop:LAD}). \\
\addlinespace[3pt]
Share and loss of decisions &
Share first order, loss second (Proposition~\ref{prop:SHR},
Theorem~\ref{thm:LOS}). &
Both of order zero; the separation is lost. \\
\addlinespace[3pt]
Audit implication &
False negative. An association audit passes a sequence-dependent population. &
False positive as well. A pooled association audit returns a stereotype no member
holds, from mixing alone. \\
\addlinespace[3pt]
Identification &
The reading, from which marginal ignores the prior association
(Propositions~\ref{prop:ORD} and~\ref{prop:DRF}). &
The weight, from three ratings (Proposition~\ref{prop:ADJ}). At its strongest here,
since the position channel is what Proposition~\ref{prop:ADJ} reads. \\
\bottomrule
\end{tabular}
\caption{The two settings. Under full adoption only the association channel
operates; under partial or no adoption the position channel operates as well, and
the association channel with it when the attributes are believed related.}
\label{tab:settings}
\end{table}""")],
    "Placement (2026-10-01). The author's Section 6 is already titled \"Scope and robustness\", so the "
    "section is split into two subsections. Scope keeps the assumptions paragraph and the rival "
    "mechanisms of E.11. The second subsection states that the second-order results require full "
    "adoption of the later cue, gives the test of that condition (Proposition ADJ, three marginals "
    "of one reading group) and says what partial adoption and a factor reading change "
    "(Propositions LAD and FAC). It is not a robustness result and is not called one (author, "
    "2026-10-01): under partial adoption the association differs between sequences at first "
    "order and the pooled association acquires a term of order zero, so the paper's second-order "
    "conclusions do not survive; what survives is the odds ratio, exact at every weight, and the "
    "form of the characterisation, with the second-order class moving to the statistics that "
    "agree with the log odds ratio to first order. The author may want to drop \"and robustness\" "
    "from the section title. The Scope heading, with a repair of \"define scope for any "
    "conclusions made from the model\" (E.15h). The roadmap clause is E.15r. The parts here are "
    "the new subsection, one insertion after E.11 shown as consecutive paragraphs so that the "
    "plan can break pages between them. What it gathers: the two-channel paragraph of E.12 "
    "unchanged; the adoption-weight lead-in, Proposition ADJ, its example and Proposition LAD "
    "from E.10, with \"so far\" replaced by the section references; the rubric prediction and the "
    "settings table of E.12 unchanged. What is new: the framing paragraph, Proposition FAC and "
    "Table tab:robust. Proposition FAC is the factor-input counterpart of LAD. Its weighted factor "
    "$a^\\omega$ is a construction of this paper, log-linear damping of the factor, not one of "
    "Hawthorne's variants; his variants are the $\\omega=1$ column, where updates on distinct bases "
    "commute. The Lean record proves FAC for an arbitrary damped factor $a'$ and states the "
    "marginal agreement as $a_1a_0'=a_0a_1'$; the power form is LadderFactorPow.lean "
    "(`factor_pow_gap_iff`: the marginals agree at c = 0 iff omega = 1 or q0 = alpha), with its "
    "closed forms in sympy/verify_ladder.py section F. Table tab:robust puts the four settings side by side; its decision rows for the "
    "partial-adoption columns are Theorem LOS read with an order-zero score gap and are the only "
    "entries not checked by sympy, as the caption says. Two tables now sit in the subsection; the "
    "author may drop tab:settings if tab:robust carries enough. Order of application: after E.10 "
    "(ORD) and E.11; before C.4, C.5 and C.9, which cite LAD or sec:robust; E.13 after it.",
    "Ladder.lean: `rescale_rescale`, `rescale_rescale_eq`, `assoc_normalize`, `oddsRatio_normalize`, "
    "`oddsRatio_factorRoute`, `assoc_factorRoute`, `factorRoute_at_zero`, `factorRoute_mA1_zero`, "
    "`factor_mA1_gap_iff`, `mprod_factorRoute_zero`, `oddsShadow_factor_coeff` (FAC); `ladder_gap`, "
    "`ladder_assoc_coeff`, `ladder_oddsShadow_seqEffect` (LAD); Anchoring.lean (ADJ). "
    "sympy/verify_ladder.py (53/53, section F for the factor routes, including that the "
    "full-adoption factor table is the manuscript's benchmark); sympy/verify_interior_omega.py "
    "rows 5 and 6. Hawthorne.lean (`factorUpdate_comm`, `extUpdate_basisCommuting`); Wagner2002.lean "
    "(thm31); HogarthEinhorn.lean (`oneSided_eq_eq8`)."))

E2.append(("E.13", "Back matter, AI declaration (was B.B)", [(
    r"Proposition~\ref{prop:PRO} (uniqueness of the protected statistic), which was then independently re-derived",
    r"Proposition~\ref{prop:PRO} (uniqueness of the protected statistic), which was then independently re-derived",
    r"""Propositions~\ref{prop:PRO} (uniqueness of the protected statistic),
\ref{prop:ORD} (between-sequence contrast), \ref{prop:ADJ} (adoption weight)
and~\ref{prop:LAD} (partial adoption), which were then independently re-derived""")],
    "Plan B.B. Propositions ORD, ADJ and LAD have the same provenance as PRO, so the "
    "declaration names them; FAC is dropped with 6.4.",
    "None needed."))

E2.append(("E.14", "Bibliography entries the Section E texts need (was B.A)", [],
    "Needed by E.1, E.5, E.7 (Bohren2019, Doring1999, Garber1980) and C.15 (Heckman1998, "
    "Bohren2019). Hawthorne2004 is already in bibliography.bib. All six proposed entries are in "
    "notes/manuscript_corrections_extra.bib and move to bibliography.bib on approval. Zhao-"
    "Osherson is cited only in the introduction (author's draft) and is already in the "
    "bibliography. Added 2026-10-02: CoffmanExleyNiederle2021, needed by C.19. Added "
    "2026-10-03: TverskyKahneman1992, needed by C.20.",
    "Bibliographic details verified against the Drive copies where printed (Doring: Phil. "
    "Sci. 66 (Proceedings) S379-S389; Garber: 47(1) 142-145; Heckman: JEP 12(2) 101-116); "
    "Bohren et al.'s AER pages are from the reference lists of later papers, the Drive copy "
    "being the working paper."))
