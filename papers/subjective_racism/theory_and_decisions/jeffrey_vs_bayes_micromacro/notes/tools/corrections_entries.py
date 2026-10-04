#!/usr/bin/env python3
"""Correction entries for Paper B (source of notes/manuscript_corrections.tex).
Edit the entries here, then run make_corrections_tex.py.
BEFORE text is cut from the committed manuscript between a start and an end
phrase, so every BEFORE block matches the manuscript exactly (whitespace
normalised). Fails if any phrase is missing or ambiguous."""
import re, sys, textwrap

import os
REPO = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", ".."))
MS = open(f"{REPO}/PAPER_B_MANUSCRIPT.tex").read()
NORM = re.sub(r"\s+", " ", MS)

# Entries already applied to the manuscript, by former id, with the commit that applied them.
# Their BEFORE anchors no longer exist in the manuscript, so the anchor checks skip them and
# the plan lists them above its table instead of in it.
APPLIED = {k: "db3a2f41" for k in ("C.9", "E.2", "C.9w", "C.10", "E.3", "E.5", "C.11b",
                                   "E.8", "E.9", "E.10", "E.15h", "E.11", "E.15")}
APPLIED["C.16"] = "6775f822"
APPLIED["C.18"] = "6cdf15f3"
APPLIED["C.17"] = "6cdf15f3"
APPLIED["C.21"] = "deddd600"
APPLIED["C.19"] = "f125368b"
APPLIED["C.20"] = "f125368b"
APPLIED["C.22"] = "46b68558"
APPLIED["C.23"] = "5088448e"
APPLIED["C.24"] = "adaa99f6"
APPLIED["C.25"] = "8e45609a"
APPLIED["C.26"] = "dd5b1311"
# Parts applied on their own while the rest of the entry stays pending.
APPLIED_PARTS = {("C.11", 3): "6775f822", ("C.11", 4): "6775f822", ("C.11", 5): "6775f822"}


_HIST = {}


def _norm_at(commit):
    """The manuscript as it stood just before `commit`, whitespace-normalised."""
    if commit not in _HIST:
        import subprocess
        raw = subprocess.run(["git", "-C", REPO, "show", f"{commit}^:./PAPER_B_MANUSCRIPT.tex"],
                             capture_output=True, text=True, check=True).stdout
        _HIST[commit] = re.sub(r"\s+", " ", raw)
    return _HIST[commit]


def cut(start, end, eid=None, k=None):
    """The BEFORE text, from the current manuscript, or for an applied entry or part from
    the manuscript just before the commit that applied it."""
    commit = APPLIED.get(eid) or APPLIED_PARTS.get((eid, k))
    src = _norm_at(commit) if commit else NORM
    i = src.find(start)
    if i < 0 or src.find(start, i + 1) >= 0:
        sys.exit(f"start phrase missing or ambiguous: {start!r}")
    j = src.find(end, i)
    if j < 0:
        sys.exit(f"end phrase missing: {end!r}")
    return src[i:j + len(end)]


def norm_part(part):
    """(kind, start, end, after); a 3-tuple is a replace."""
    return ("replace",) + tuple(part) if len(part) == 3 else tuple(part)


def quote(s):
    return "\n".join("> " + l for l in textwrap.wrap(s, 78, break_long_words=False, break_on_hyphens=False))


def block(s):
    return "```latex\n" + textwrap.fill(" ".join(s.split()), 80, break_long_words=False, break_on_hyphens=False) + "\n```"


E = []  # (id, title, [(start, end, after)], purpose, verification)

E.append(("C.1", "Abstract, sentences 1, 2-4 and 6 (rebased on the author's draft c2ae782f)", [
    ("The effect of the order in which evidence arrives on an individual judgment", "in both experimental and theoretical studies.",
     r"""The effect of the sequence in which evidence arrives on an individual judgment has
sparked much interest in both experimental and theoretical studies."""),
    ("Considering a case where evaluators make decisions using two correlated soft cues", "whereas the believed association departs only at second order.",
     r"""Considering a case where evaluators make decisions using two correlated soft cues,
the current paper asks which statistics of their beliefs and decisions register a
resulting effect of arrival sequence with respect to a sequence-free benchmark.
With a view to implications for empirical measurement, the paper shows that a
smooth statistic of the belief is an order of magnitude closer to the benchmark
than the marginal probabilities when it is insensitive to small shifts in those
marginals. The average marginal probabilities and the share of evaluators whose
decision the sequence changes depart from the sequence-free benchmark at first
order, whereas the believed association departs only at second order."""),
    ("Second, whether the effect of order is detectable", "rather than on the number of evaluators considered.",
     r"""Second, at a given precision of measurement, whether the sequence effect is
detectable depends on the kind of question being asked.""")],
    "The author applied C.1 in their own wording at c2ae782f; this entry keeps only what "
    "still needs correcting. Sentence 1 and the last sentence use \"order\" for reading "
    "sequence, which the paper reserves for order in $c$ (writing discipline 6). Sentence 2: "
    "\"the current papers asks\", \"with respective to\", and \"which of their beliefs and decision "
    "statistics\" (a belief is not a statistic). Sentence 3: \"With a view of\" is \"With a view to\"; "
    "\"an arbitrary statistic\" is false as stated, since only statistics insensitive to the "
    "marginals are closer (Proposition PRO), so \"smooth ... of the belief\", the comparator \"than "
    "the marginal probabilities\" and \"those marginals\" are restored from the approved C.1. "
    "Sentence 4: two consecutive sentences open \"The paper shows\", and \"shows how\" states a "
    "result as a method; \"collective\" becomes \"average\" (Proposition DRF reads the mean "
    "belief). Leaving the loss out of sentence 4 is accurate, since the loss is also second "
    "order (Theorem LOS). Last sentence: \"rather than on the number of evaluators\" is the "
    "identification overclaim (todo, 2026-10-01). Only the odds ratio is blind at every sample "
    "size (Lemma SEP); the association is second order and visible once precision is finer "
    "than $c^2$ (MS precision passage, $c^2\\ll\\varepsilon\\ll c$), and precision improves with "
    "the number of evaluators. Fixing the precision makes the claim true.",
    "PRO (propPRO_protection, propPRO_uniqueness); DEC and LOS (second order); DRF and SHR "
    "(first order); LemmaSEP.lean (the odds ratio). All in the Lean and sympy suites."))

E.append(("C.2", "Introduction, paragraph 1 (rebased on the author's draft c2ae782f)", [
    ("That arrival sequence of cues does effect judgment is rarely under doubt", "how later cues are read at all.",
     r"""That the arrival sequence of cues affects judgment is rarely in doubt, since if
individual judgments did not depend on the arrival sequence, then each cue must move
belief by a factor fixed before it is received, leading to the unlikely situation
where experience does not change how later cues are read at all."""),
    ("The sequence-dependence of Jeffrey conditioning for soft evidence in particular", "has received little attention.",
     r"""The sequence dependence of Jeffrey conditioning for soft evidence in particular has
attracted much debate \citep{DiaconisZabell1982,Hawthorne2004}, but the question of
how that dependence registers in the statistics an observer reads has received
little attention."""),
    ("It finds that the share of population affected", "and the total loss due to sequence effects do not.",
     r"""It finds that the share of the population whose decision the sequence changes
departs from a sequence-invariant benchmark at first order, while the believed
association between attributes and the surplus-weighted loss depart from it only at
second order."""),
    ("Given that a difference from the benchmark below the observer's precision", "the paper discusses the implications for identification of sequence effects.",
     r"""% Delete the sentence (see Why)."""),
    ("Despite its simplicity the two-cue setting", "remains valid for any number of cues and attributes.",
     r"""Despite its simplicity, the two-cue setting demonstrates the central mechanism,
that attribute-local updating leaves the interactions among attributes untouched,
and the mechanism remains valid for any number of cues and attributes
(Lemma~\ref{lem:SEP}).""")],
    "The author applied C.2 in their own wording at c2ae782f; this entry keeps what still "
    "needs correcting. Modus tollens sentence: \"does effect\" is \"affects\", \"under doubt\" is "
    "\"in doubt\", the double hyphen is a dash doing a sentence's work, and \"and lead to\" breaks "
    "the parallel with \"move\". Caution kept from the earlier entry: \"must\" is stronger than "
    "the literature. Fixed factors imply commutation (Wagner 2003 Theorem 3.2, Hawthorne "
    "factorUpdate_comm, Cripps order_invariance); the converse is exact in this paper's two-cue "
    "model, where the sequence effect vanishes iff q0 = alpha and r0 = beta, but is not a "
    "theorem for arbitrary updating rules. Debate sentence: the hyphen in the noun "
    "\"sequence dependence\" (the author removed the stray \"but\" at da5e3cff). ZhaoOsherson2010 is no longer cited anywhere in the "
    "manuscript, which is the author's choice; its bibliography entry is now unused. "
    "Sentence \"It finds that\": the loss is not free of the sequence effect, it carries it at "
    "second order (LOS), and \"-- but\" is a dash doing a sentence's work. "
    "Sentence \"Given that\": cut (rule 4 of the writing discipline); the precision point is "
    "made in the C.4 paragraph, and \"identification of sequence effects\" is the identification "
    "overclaim C.1 removes from the abstract. Last sentence: \"demonstrates the central mechanism "
    "of how ... remains valid\" has two verbs for one subject; Lemma SEP is the general-N result "
    "it refers to.",
    "verify_jeffrey.md J1; verify_kinematics.md; verify_hawthorne_weisberg.md; Theorem LOS, "
    "Proposition SHR; LemmaSEP.lean (general N). Modus tollens: Cripps.lean order_invariance, "
    "Hawthorne.lean factorUpdate_comm, Wagner2003.lean thm32; sympy/verify_soft_vs_hard.py "
    "(exact condition for the sequence effect)."))

E.append(("C.3", "Introduction, paragraph 2 and its footnote (rebased on the author's draft c2ae782f)", [(
    "The discussions on how Jeffrey\\citep{Jeffrey1983} conditioning", "cannot tell whether impressions replace prior belief or adjust it.",
    r"""The discussions on how Jeffrey conditioning \citep{Jeffrey1983} depends on arrival
sequence have focused more often on whether the commutation criterion is exactly
satisfied or not but not enough on how measurable its failure is in observed data.
The belief-adjustment literature often models an impression as a single score
\citep{HogarthEinhorn1992}, and the impression-formation literature considers traits
one attribute at a time \citep{Asch1946}. The characterisation of when two sequences
lead to the same belief does consider a joint belief over attributes
\citep{DiaconisZabell1982}, but elsewhere sequence dependence is more often treated
as a defect to repair\footnote{\citet{Hawthorne2004} offers factor-based alternatives
in which a cue supplies a normed-likelihood or likelihood-ratio factor instead of a
new probability. Under these, updates on distinct attributes commute, and in his
Basis-Commuting Version all updates do.} rather than as something to measure. While
changing the input from a credence that replaces the prior to a factor that
multiplies it does settle whether beliefs depend on sequence, it hardly addresses
the empirical issue that an observer cannot tell whether impressions replace prior
belief or adjust it.""")],
    "The author applied C.3 in their own wording at c2ae782f; this entry keeps what still "
    "needs correcting. (i) \"models an impression as a single score \\citep{Hawthorne2004}\": the "
    "single-score model is Hogarth-Einhorn's belief-adjustment model, and Hawthorne is not part "
    "of that literature. (ii) Asch is impression formation, not belief adjustment, and reports "
    "eighteen traits one by one (audit A28). (iii) \"\\citet{DiaconisZabell1982}\" is glued to "
    "\"attributes\" and should be \\citep. (iv) \"the order dependence is more often treated as a "
    "defect\" (the doubled \"more\" was fixed by the author at da5e3cff): \"order\" for reading sequence, and the clause still groups "
    "Diaconis-Zabell with the defect view, though they hold that non-commutativity \"is not a "
    "real problem\" (M24); \"elsewhere\" detaches it from them. (v) The footnote still says "
    "Hawthorne \"repairs\" the defect and that the effect disappears; he offers alternatives, "
    "and freedom from sequence holds across distinct bases, fully only in his Basis-Commuting "
    "Version (M19). (vi) \"Jeffrey\\citep\" lacks a space and the citation sits inside \"Jeffrey "
    "conditioning\". (vii) The double hyphen before \"does settle\" is a dash doing a comma's work.",
    "Asch1946 record (Asch.lean, Table 7); HogarthEinhorn.lean; DiaconisZabell.lean and README; "
    "Hawthorne.lean (`extUpdate_basisCommuting`, `factorUpdate_comm`), Sections 6-8."))

E.append(("C.4", "Introduction, the two-channel paragraph (rebased on the author's draft da5e3cff)", [(
    "Whether impressions replace prior belief or adjust is a question which", "the formational mechanisms biases and prejudice.",
    r"""Whether impressions replace prior belief or adjust it is a question which
\citet[pp.~115--116]{Hawthorne2004}, writing as a logician rather than a psychologist,
leaves open. To set the scope of its conclusions, the paper distinguishes two channels of
arrival-sequence dependence that differ in kind. The \textbf{association} channel is
a property of what the evaluator believes. A cue on one attribute moves the belief
about the other because the evaluator believes the attributes go together, a belief
measured by the prior covariance $c$, so the channel exists only when $c\neq0$. The
\textbf{position} channel is a property of how the evaluator revises. In full, it is
any way in which the share of a cue's delivered credence that the evaluator takes on
depends on where in the sequence the cue arrives, whether or not the attributes are
believed related. The belief-adjustment model of \citet{HogarthEinhorn1992} gives it a
general form, a weight on each cue, and its varieties differ in which cues are
discounted and by how much. The paper represents only one variety, a constant discount
on the later cue, by an adoption weight $\omega$, and Section~\ref{sec:scope} sets out
the others, which lie outside its results. When each impression is adopted
in full, $\omega=1$, as in the successive updating of \citet{DiaconisZabell1982}, the
position channel is absent and only the association channel remains. In the two-attribute
setting with full adoption, the paper thus considers three coordinates of a belief,
the two marginal probabilities and the cross-attribute association, and explains how a
difference from the sequence-free benchmark below the observer's precision is
invisible in the statistic read by the observer. The question is therefore not simply
whether the two sequences agree but by how much they disagree in an arbitrary
statistic. The paper finds that the marginal probabilities and the share of decisions
they change carry the difference at first order in the prior covariance, while the
believed association and the statistics that move with it carry it only at second
order. This carries clear implications for what an audit can or cannot measure
about sequence dependence, an observation that may elucidate the mechanisms by which
biases and prejudice form."""),
    ("More specifically, if the strength with which the two traits are believed to go together",
     "were represented with a prior covariance $c$,",
     r"""More specifically, with $c$ the prior covariance,""")],
    "The author applied C.4 in their own wording at c2ae782f; this entry keeps what still "
    "needs correcting and adds one sentence. Rebased on da5e3cff, where the author appended a "
    "clause to the last sentence: the entry keeps the clause and repairs it, the double hyphen "
    "becoming a comma (rule 9) and \"the formational mechanisms biases and prejudice\" becoming "
    "\"the mechanisms by which biases and prejudice form\". The clause is the author's and is "
    "hedged by \"may\"; the results say where sequence dependence registers, not how prejudice "
    "forms. Corrections: \"adjust\" to \"adjust it\"; \"leave open\" "
    "to \"leaves open\" (Hawthorne is one author; he gives a tentative view and says his interest "
    "is normative, pp. 115-116); \"an arrival-sequence dependence\" loses its article; the position "
    "channel was defined as \"the observer weights the later cue less\" and cited to Hogarth-Einhorn "
    "and Asch, but Hogarth-Einhorn's step-by-step adjustment predicts recency (Appendix B) and Asch "
    "denies that \"sheer temporal position\" matters (p. 272), so the definition is made neutral "
    "and Asch is dropped (audit P10-P12); the two dashes become commas; \"decisions from them\" "
    "becomes \"the share of decisions they change\", since the loss is a decision statistic and is "
    "second order (LOS); \"and statistics\" becomes \"and the statistics that move with it\". "
    "Scope defence (author, 2026-10-01): the position channel is a property of the evaluator, so the "
    "paper sets it aside and studies the measurement problem that remains under the association "
    "channel. The author placed the one-sentence version of this in the panel paragraph (C.5) and "
    "the full version with its test in Section 6 (E.15), and asked for less in the introduction; "
    "this paragraph therefore keeps the author's own two sentences on the two settings and adds "
    "nothing. "
    "Amended 2026-10-04 at the author's request, after a discussion of what the two channels are: "
    "the channel sentences now say what each is, the association channel a property of what the "
    "evaluator believes and the position channel a property of how the evaluator revises, define "
    "the position channel in full (any dependence of the share of a delivered credence taken on "
    "on where the cue arrives, whether or not the attributes are believed related), and name the "
    "adoption weight as the one variety the paper represents, with the others set out in Section 6 "
    "(entry 6.9, which this needs). Asch is not brought back to place his mechanism, a delivered "
    "credence that changes with what was read before, outside both channels: the author's call "
    "(2026-10-04), since the paper draws on philosophy of science and economics, normative and "
    "positive, and uses a source for a formal model or a normative argument, not for an account "
    "of how impressions form; the premise his mechanism would break is already Definition 2's "
    "\"the same two impressions\". The channel sentences grow from 118 to 195 words, the length of the content the author "
    "asked for. Part 2 shortens the terminology paragraph's definition of $c$, which the channel "
    "sentence now gives first; it is shorter than the clause it replaces (7 words against 22) by "
    "design, since what it removes is a repetition.",
    "HogarthEinhorn.lean (`appB_recency`, `eq8_estimation_first_dominates`); Hawthorne 2004 pp. "
    "115-116 (grounds_E_literature); Ladder.lean (`ladder_gap`, `ladder_assoc_coeff`, "
    "`ladder_oddsShadow_seqEffect`, `oddsRatio_rescale`); sympy/verify_ladder.py (99/99). "
    "Amendment: PropIMM.lean (`propIMM_indep`); Anchoring.lean (`dampedB_at_one`); Hogarth and "
    "Einhorn 1992, Eqs. 3-4 (T-p.7)."))

E.append(("C.5", "Introduction, hiring-panel paragraph, the two sentences before the last (rebased on the author's draft da5e3cff)", [(
    "As the paper shows, this means that the believed association differs between the two sequences only at second order.",
    "(and the believed association differs at first order - see Proposition LAD).",
    r"""The believed association then differs between the two sequences only at second
order (Proposition~\ref{prop:ORD}). A letter adopted only in part moves that belief
from wherever the credential left it, and the believed association then differs at
first order (Proposition~\ref{prop:LAD}).""")],
    "The author applied the panel paragraph's new ending at d48dd872 in the pre-correction "
    "wording; this entry keeps the corrections that remain. \"As the paper shows, this means that\" "
    "has no noun for \"this\" (rule 3); the sentence now follows on from the previous one, which "
    "ends \"the believed link between the traits\". The hyphen in \"at first order - see Proposition "
    "LAD\" is a dash doing a sentence's work (rule 9), and \"Proposition LAD\" needs the reference. "
    "\"On the other hand\" goes, the contrast being carried by \"adopted in full\" against \"adopted "
    "only in part\". The author applied the corrected last sentence at da5e3cff "
    "(\"The paper therefore restricts attention to situations in which unrelated traits exhibit "
    "no sequence dependence through the position channel, and studies the measurement issues "
    "under the association channel.\"), which supersedes the plan's version of it (rule 5), so "
    "the entry now ends before it. That sentence is the one place in the introduction that "
    "states the scope defence (author, 2026-10-01). The odds-ratio sentence is in a comment at the author's "
    "choice (less in the introduction); its claim, exact invariance at every weight, is Lemma SEP "
    "and stays in Section 6. Economics behind the three sentences: under full adoption the letter "
    "fixes its own marginal whatever the credential implied, so the sequence acts only through the "
    "believed link and the association differs at second order (IMM, ORD); under partial adoption "
    "the sequence decides which document is discounted, both ratings differ at order zero even for "
    "unrelated traits, and the association, $c$ times rescaling factors that now differ at order "
    "zero, differs at first order with coefficient $(1-\\omega)H/Z$ (LAD). Propositions ORD and LAD "
    "must be applied (5.2, 6.3) before the references resolve.",
    "PropIMM.lean; PropORD.lean; Ladder.lean (`ladder_gap`, `ladder_gap_mA1`, `ladder_assoc_coeff`, "
    "`ladder_assoc_coeff_eq_zero_iff`); LemmaSEP.lean (general N, any attribute-local rescaling); "
    "sympy/verify_ladder.py (99/99); sympy/verify_interior_omega.py rows 3 and 4 (39/39)."))

E.append(("C.6", "Introduction, premise paragraph, from \"Read as a Bayes factor\"", [(
    "Read as a Bayes factor, the credential carries a likelihood ratio", "consistent with the delivered marginal \\citep{DiaconisZabell1982}",
    r"""Read as a Bayes factor, the credential carries the ratio of new to old odds on
competence, which multiplies whatever belief it meets. The two readings agree on a
single cue (Proposition~\ref{prop:IMM}) and differ in what stays fixed when the cue
meets another prior. The panelist's own impression is naturally a credence, whereas
a factor suits evidence reported by someone else, whose report mixes the evidence
with that person's prior \citep[ch.~3]{Jeffrey2004}. Jeffrey updating holds fixed
the conditionals given the cue's partition, and its posterior is the unique belief
with the delivered marginal closest to the prior in Kullback--Leibler divergence
\citep[Theorem~5.1]{DiaconisZabell1982}""")],
    "(i) \"A Bayes factor requires the probability of the same credential for a candidate who "
    "is not competent\" is contradicted by Jeffrey's own definition, new odds over old odds, "
    "which needs no likelihood (2002 draft of Jeffrey 2004, ch. 3; audit J2). Jeffrey draws "
    "the line between the readings by provenance instead (own experience gives credences, "
    "others' reports should be converted to factors), which supports the paper's modelling "
    "choice. (ii) \"the unique coherent revision\" is not in Diaconis-Zabell; what they prove is "
    "the unique Kullback-Leibler (and Hellinger) minimiser (M22). \"equivalent to making the "
    "minimal change\" without naming the distance is also loose (not unique in variation "
    "distance). The footnote after the citation is unchanged. Needs the Jeffrey2004 bib entry "
    "(C.12); the Drive copy is the 2002 draft, so cite the chapter, not a page.",
    "verify_jeffrey.md J2; DiaconisZabell.lean (`thm51_KL_eq_iff`, `jcond_iff_jeffrey`); "
    "PropIMM.lean (`propIMM_single_cue`)."))

E.append(("C.7", "Introduction, terminology paragraph, Domotor sentence and two typos", [
    ("The sequence-dependence of marginals follows easily", "\\citep{Domotor1980}.",
     r"""The sequence dependence of the marginals follows directly, since a delivered
credence is taken against the marginal in force when the cue arrives, which the
first cue moves when $c\neq0$ (Proposition~\ref{prop:DIV})."""),
    ("To avoid a clash in terminology we use", "to denote the sequence in which evidence is read.",
     r"""To avoid a clash in terminology we use ``order'' to describe proximity with a
sequence-free Bayesian benchmark and ``sequence'' to denote the sequence in which
evidence is read.""")],
    "Domotor never mentions likelihoods or marginals; he supports only bare non-commutativity, "
    "and treats it as a defect of Jeffrey machines (p. 395), the opposite lean (audit M1). The "
    "mechanism is the paper's own (Proposition DIV). If a Domotor citation is wanted, it fits "
    "the literature paragraph as a source of the commutativity demand that Jeffrey (1983, "
    "pp. 182-183) rejects. Typos: \"bechmark\", ``sequence`` closed with backquotes.",
    "Domotor.lean (`field_eq_jeffrey_embed`, `embed_depends_on_state`); Cripps.lean "
    "(`composite_AB_eq_bayes`: the B-marginal moves by c(q0-alpha)/(alpha(1-alpha)))."))

E.append(("C.8", "Introduction, key-finding paragraph, last two sentences", [(
    "The implication for evaluation is that and audit's answer", "may not even be detectable.",
    r"""The implication for evaluation is that an audit's answer depends on which
statistic is aggregated rather than on how many evaluators are averaged over. One
consequence is that belief audits asking how attributes are thought to go together,
the instruments most naturally trusted for detecting stereotype, cannot detect the
sequence effect at first order.""")],
    "Typos (\"and audit's\", \"One consequence, is\"), and \"the instruments ... may not even be "
    "detectable\" says the instruments are undetectable; it is the sequence effect that belief "
    "audits cannot detect at first order.",
    "Propositions DEC, PRO."))

E.append(("C.9", "Setup 2.1, the prior paragraph", [
    ("The three numbers that fix $P$ are described as $\\Delta^{3}$ -- specified using the marginals",
     "the prior covariance $c$ between the attributes:",
     r"""The prior is a point of $\Delta^{3}$ fixed by three numbers, the marginals
$P(A{=}0)=\alpha$ and $P(B{=}0)=\beta$ and the prior covariance $c$ between the
attributes,"""),
    ("Simply put, the marginals $\\alpha=P(A{=}0)$ and $\\beta=P(B{=}0)$ denotes",
     "denotes how strongly the panelist believes the",
     r"""Simply put, the marginals $\alpha=P(A{=}0)$ and $\beta=P(B{=}0)$ denote the shares
of applicants the panelist believes to lack competence and to lack trustworthiness,
and the prior covariance $c=\assoc(P)$ denotes how strongly the panelist believes
the""")],

    "Grammar (\"described as $\\Delta^3$ --\", \"denotes\" for two subjects). The "
    "Frechet-Hoeffding bounds in the new text are correct (checked cell by cell). The "
    "adoption-weight sentence that ends this paragraph is a separate entry, since a "
    "Section~2 entry on what an impression is falls between them (C.9w).",
    "Bounds: nonnegativity of the four cells of P."))

E.append(("C.9w", "Setup 2.1, the adoption-weight sentence", [
    ("To address the issues in amnestic updating debate, we also consider an adoption weight",
     "the credential's implication is never moved.",
     r"""To compare full adoption with the belief-adjustment model of
\citet{HogarthEinhorn1992}, in which a later impression is adopted only in part, we
also consider an adoption weight $\omega\in[0,1]$ (Section~\ref{sec:robust}), which
denotes how far the letter
displaces what the credential had already implied about trustworthiness. At
$\omega=1$ the letter sets the rating outright, and at $\omega=0$ the credential's
implication is never moved.""")],
    "Purpose of omega (author's decision, 2026-10-01): the weight nests Hogarth-Einhorn's "
    "averaging rule (their Eq. 4, applied to the second cue; E.15), so full adoption is one "
    "corner of a descriptive model rather than an assumption taken from the normative "
    "literature, and the pointer sends the reader to the subsection where the weight is used. "
    "The Hawthorne debate is taken up in the introduction (C.4). \"Amnestic\" is used only "
    "where Hawthorne is quoted (writing_discipline.md 6), so it goes. Omega is used nowhere "
    "else before Section 6, so this entry needs E.15.",
    "Hawthorne pp. 98-99; HogarthEinhorn.lean (`oneSided_eq_eq8`); Anchoring.lean."))

E.append(("C.10", "Setup 2.2, the invariance condition and the justification of the rule", [
    ("holding the conditionals fixed---the invariance condition", "\\citep[ch.~11]{Jeffrey1983}.",
     r"""holding the conditionals given the cue's partition fixed, the invariance
condition \citep[ch.~3]{Jeffrey2004}, which \citet[ch.~11]{Jeffrey1983} expresses by
saying that the change originates in the partition."""),
    ("The axiomatic grounding is twofold.", "in the $I$-divergence sense \\citep{DiaconisZabell1982}.",
     r"""Two results of \citet{DiaconisZabell1982} support the rule. The invariance
condition makes the cue's partition sufficient for the prior and posterior, and
minimal whenever the delivered credence differs from the prior marginal (their
Theorem~2.2). The rule is also the unique minimiser of the $I$-divergence from the
prior among all beliefs with the delivered marginal (their Theorem~5.1).""")],
    "(i) Chapter 11 of Jeffrey (1983) never uses \"invariance\"; it says the change "
    "\"originates in\" the partition (pp. 168, 174). \"Invariance\" is the 2004 book's term "
    "(audit J4). (ii) \"The partition satisfying the invariance condition is the minimal "
    "sufficient statistic for revising the prior to any candidate posterior\" misstates D-Z "
    "Theorem 2.2: sufficiency is for one pair {P, P*}, every refinement of such a partition "
    "also qualifies, and the minimal one is the likelihood-ratio partition (M23; Jeffrey p. 174 "
    "also allows \"a certain latitude in the choice\" of partition, J3). (iii) D-Z give no "
    "axioms; they call this \"mechanical updating\". The summation index is $x$ because "
    "$\\omega$ is now the adoption weight.",
    "DiaconisZabell.lean (`thm22_lr_minimal`, `jcond_of_refines`, `lr_jeffrey_iff`, "
    "`thm51_KL_eq_iff`); verify_jeffrey.md J3-J4."))

E.append(("C.11", "Related literature, the paragraph from BHW/Banerjee to Becker and Good-Mittal", [
    ("In \\citet{Banerjee1992}, the sequence persists because", "a cascade is an action that conveys no private signal.",
     r"""In both, the equilibrium action is not a sufficient statistic for private
information, being binary in \citet{BHW1992} and a non-invertible choice rule in
\citet{Banerjee1992}, so once a cascade or herd forms, later actions convey no
private signal."""),
    ("One similarity with the literature, however,", "which remain so in the current paper as well.",
     r"""One analogy remains. Those models have a single state, so their sequence effect
falls on a belief about one variable, the counterpart of a marginal, and here too
the marginals carry the sequence effect."""),
    ("A related normative literature evaluates group belief formation", "outside the protected class of Proposition~\\ref{prop:PRO}.",
     r"""% Deleted (see Why): the three Dietrich sentences go; the paragraph keeps its first two sentences."""),
    ("\\textbf{Several alternatives to Bayesian updating", "Divisibility axioms jointly force.}",
     r"""Decision theory has axiomatised several alternatives to Bayesian updating, each
for a single piece of news. \citet{Epstein2006} makes the updating rule, and not only
the prior, subjective, and \citet{Ortoleva2012} axiomatises departures triggered by
unexpected news. \citet{Cripps2021} treats the sequence of signals, and his
Symmetry and Divisibility axioms imply that two conditionally independent signals
give the same posterior in either sequence. Read with each cue as its likelihood matched to the prior
(Proposition~\ref{prop:IMM}), the composite of Proposition~\ref{prop:DIV} is a
sequence of Bayes updates satisfying all four of his axioms, and it depends on the
sequence because the second likelihood is matched to the intermediate belief, so
the two sequences process different experiments."""),
    ("While the literature restores sequence-invariance for sequential Jeffrey updating", "\\citep{PettigrewWeisberg2025},",
     r"""While \citet{PettigrewWeisberg2025} restore sequence-invariance by pooling the
prior with each new input multiplicatively before the Jeffrey step,""")],
    "One entry per sentence group, in manuscript order. BHW/Banerjee: \"coarse\" belongs to BHW's "
    "binary action, not Banerjee's continuum; the two share one mechanism, so \"on the other hand\" "
    "goes; the marginals point is the paper's analogy (M2, M3). Dietrich: Def. 1 is preference "
    "aggregation, and with a common prior linear pooling passes the criterion wherever it applies "
    "(M4, M5). Decision 2026-10-02: the paper designs and evaluates no pooling rule, its "
    "aggregation being an auditor's average, so the Dietrich sentences are deleted rather than "
    "rewritten; the one point they were guarding, that the first-order divergence is not an "
    "artefact of linear pooling, is now a sentence in Section 5 (entry 5.3). "
    "Epstein/Ortoleva/Cripps (bold sentence): the framing is unsupported for Epstein "
    "and Ortoleva; the Cripps claim is false, since under Proposition IMM's reading the "
    "composite satisfies all four axioms, and the footnote goes with it (M6-M9). "
    "Pettigrew-Weisberg pool the prior with each input, not successive inputs (M25). The rest of "
    "this paragraph is C.11b, since the Asch entry (E.6) falls in between.",
    "BHW.lean, Banerjee.lean, Dietrich.lean, Epstein.lean, Ortoleva.lean (via the 2024 survey), "
    "Cripps.lean (`order_invariance`, `composite_AB_eq_bayes`), PettigrewWeisberg.lean."))

E.append(("C.11b", "Related literature, Phelps-Arrow to the amalgamation paragraph, plus the identification paragraph", [
    ("The paper borrows the evaluator-with-binary-attributes frame", "\\citep{Phelps1972, Arrow1973}.",
     r"""The paper borrows the evaluator-with-binary-attribute frame from the
statistical-discrimination lineage \citep{Arrow1973,CoateLoury1993}."""),
    ("instead of stereotypes as representativeness-distortion or selective recall", "not by a distortion of memory or sampling.",
     r"""instead of stereotypes as the selective recall of representative types
\citep{BCGS2016}, whose exaggerated associations arise across groups, the
stereotype in the current paper is a believed cross-attribute association held by
one evaluator and produced by coherent updating on impoverished input, not by a
distortion of memory."""),
    ("The lineage distinction the concluding section trades on", "goes back at least as far as \\citet{Becker1962}.",
     r"""% Delete the sentence (see Why)."""),
    ("It is worth contrasting the phenomenon explored in the current paper against the amalgamation paradox", "contributes only a second-order loss in aggregate (Theorem~\\ref{thm:LOS}).",
     r"""It is worth contrasting the phenomenon explored here with the amalgamation paradox
\citep{GoodMittal1987}, in which a measure of association on a combined table lies
outside the range of the same measure on its subtables, because treatment is
allocated unevenly across the subpopulations, even when they are equally large.
That paradox arises from combining subpopulations. The decision-space result here
shows a comparable loss of visibility within a single population, where a
first-order share of individuals who are individually affected contributes only a
second-order loss in aggregate (Theorem~\ref{thm:LOS}).

The audit perspective has a precedent in the economics of discrimination.
\citet{Heckman1998} showed that audit-pair estimates of discrimination rest on an
assumption about unobserved productivity that ``nothing guarantees'' (p.~109), so
that such an audit ``can find discrimination when in fact none exists; it can also
disguise discrimination when it is present'' (p.~102), and \citet{Bohren2019}
identify the source of discrimination by conditioning on the history of
evaluations, since in static data, as they note, different sources generate the
same patterns of observable behaviour. The obstruction here is of the same kind
and arises on the belief side of an audit. The first-order signal in a protected
statistic is zero in the population itself, so its silence is a failure of
identification and not of estimation, which no sample size repairs, and the
identifying variable is the reading sequence, which pooled data discard.""")],
    "Continues C.11 after the Asch entry (E.6). Phelps has no binary attribute, so he is dropped "
    "from this sentence and can be re-cited in E.1 for statistical discrimination in general "
    "(M10). BCGS name one mechanism, not two, and \"sampling\" is not theirs (M12). Becker: the "
    "concluding section never draws the distinction, and Becker's own mechanism is averaging plus "
    "a budget constraint, the opposite side of it (\"Our statement goes beyond arithmetic\", p. 7; "
    "M13); the AFTER deletes the sentence, and an alternative that keeps Becker is in "
    "literature/becker1962/README.md. Good-Mittal: their paradox covers amplification and effects "
    "created from none, and its cause is uneven allocation, not weighting by population shares "
    "(M14). The last part also appends the identification paragraph (formerly C.15).",
    "Phelps.lean, Arrow.lean, CoateLoury.lean, BCGS.lean, Becker.lean, GoodMittal.lean "
    "(`piR_amalg_general`, `equalSize_reversal`). The appended paragraph carries the identification point of notes/positioning_economics.tex and plan 3.C/3.D into Section 3, with audit P17a (no sample-size claim attributed to Heckman; that clause is the paper's own consequence of Proposition PRO), H2 (\"nothing guarantees\" is p. 109), H3 (\"can find discrimination\" is p. 102) and B37 (BIR credit \"different sources\" to Fang-Moro, so it is paraphrased) applied. Needs Heckman1998 and Bohren2019 in bibliography.bib (E.14)."))

E.append(("C.16", "Section 5, the pooling sentence after the definition of the mean belief (new, applied)", [
    ("insert_sentence", "Jeffrey conditioning is $\\Pbar_\\lambda=\\lambda\\,\\PJ_{AB}+(1-\\lambda)\\,\\PJ_{BA}$.",
     "Jeffrey conditioning is $\\Pbar_\\lambda=\\lambda\\,\\PJ_{AB}+(1-\\lambda)\\,\\PJ_{BA}$.",
     r"""The population mean is a linear pool of the two posteriors, and the choice of
pooling rule is immaterial at first order, since the two posteriors coincide at
independence and a geometric pool of them differs from the linear one only at
second order.""")],
    "Replaces the Dietrich passage of Section 3 (3.2 part 3, deleted) with the one point that "
    "matters for the results, placed where the mean belief is defined. The two posteriors "
    "coincide at $c=0$ (Proposition IMM), so a linear and a geometric pool of them agree at "
    "orders $c^0$ and $c^1$ and differ at $c^2$; their associations likewise agree to first "
    "order. Hence the first-order divergence of unprotected statistics is a property of the "
    "updating, not of averaging linearly. No citation, since the claim is elementary and "
    "verified; Dietrich (2021) is the source of the commutation criterion if the author wants "
    "it cited.",
    "sympy/verify_pooling.py (10/10, at lambda = 1/2 and 2/5 on the generic prior); "
    "PropIMM.lean for the coincidence at c = 0."))

E.append(("C.17", "Concluding remarks, closing paragraph on the debate of Section 3 (new)", [
    ("insert_para", "instruments of different asymptotic order rather than disagreeing about the same quantity.",
     "instruments of different asymptotic order rather than disagreeing about the same quantity.",
     r"""Section~\ref{sec:literature} framed the debate over Jeffrey conditioning as whether its
sequence dependence is a problem. The results do not answer that objection so much as
locate it, in the levels rather than in the believed link between the traits, and in
the number of decisions changed rather than in their cost. The normative question is
left where \citet{Doring1999} and \citet{Hawthorne2004} leave it. What the paper
settles is where the dependence sits and what it costs, and both are properties of
full adoption, since under partial adoption the believed link moves at first order as
well (Proposition~\ref{prop:LAD}).""")],
    "Section 3 now opens by saying the debate is whether the sequence dependence is a problem "
    "and that the paper addresses that question; the conclusion never returns to it, all three "
    "of its implications being about audits. This paragraph closes that loop (author, "
    "2026-10-02, the second sentence approved as written). It claims only location, not "
    "justification: the two sequences disagree at first order in the marginals and the "
    "decisions made from them, at second order in the believed association (Propositions DRF, "
    "SHR, DEC, ORD) and never in the odds ratio (Lemma SEP); the surplus-weighted loss is second "
    "order while the share of decisions changed is first order (Theorem LOS, Proposition SHR). "
    "Doring's objection is normative and Hawthorne's psychological, and neither is answered by "
    "what an auditor can or cannot see, so the paragraph says the normative question is left "
    "where they leave it. The last sentence restates the scope defence of the introduction, "
    "which a closing paragraph may do (rule 5): under partial adoption the association differs "
    "at first order (Proposition LAD). Goes last in the conclusion.",
    "PropDRF, PropSHR, PropDEC, PropORD.lean; LemmaSEP.lean; Decision.lean (LOS); Ladder.lean "
    "(`ladder_assoc_coeff`); sympy/verify_tables.py (56/56); sympy/verify_ladder.py (99/99); "
    "Doring.lean; Hawthorne.lean (amnestic_thesis; pp. 115-116)."))

E.append(("C.20", "Related literature: prospect theory's rank dependence set against sequence dependence (new)", [
    ("insert_sentence", "prior with each new input multiplicatively before the Jeffrey step,",
     "characterises what remains of it in aggregate.",
     r"""Cumulative prospect theory assigns each outcome a decision weight that depends on
its rank among the outcomes of a given prospect \citep{TverskyKahneman1992}, whereas in
the current paper belief depends on the sequence in which two cues about one candidate
are read, through the believed link between the traits, and not at all when the traits
are believed unrelated.""")],
    "Author's request (2026-10-03): remove a descriptive-models objection a referee may raise, "
    "that cumulative prospect theory already accounts for sequence. Tversky and Kahneman (1992), "
    "read in full, take the prospect as given to the valuation (p. 299) and contain no rule for "
    "revising its probabilities; the one ordering in the representation is the ranking of outcomes "
    "by value, on which each decision weight depends (pp. 300-301). The paper's sequence is that in "
    "which two cues about one candidate are read, and under full adoption it moves belief only "
    "through the believed link between the traits (Propositions IMM and ORD). The sentence names "
    "both sides in parallel (a decision weight depends on rank; belief depends on the sequence) and "
    "uses \"sequence\" only for reading, as rule 6 requires. A dynamic application of the theory "
    "(Barberis 2012, read and formalised) was dropped at the author's call: a casino's bets on "
    "known odds are not the panel's setting. Placed after the Pettigrew-Weisberg sentence, before "
    "the Hogarth-Einhorn sentences, so that the two descriptive models sit together; the author "
    "chose to keep the paragraph unsplit. Needs the bibliography entry in B.2.",
    "literature/tversky_kahneman1992 (TverskyKahneman.lean, 16 theorems; sympy check); "
    "PropIMM.lean (propIMM_indep, propIMM_no_sequence_effect); PropORD.lean (propORD_Amarg, "
    "propORD_Bmarg); sympy/verify_IMM.py, sympy/verify_ORD.py (49/49)."))

E.append(("C.21", "Section 6: the proofs of Propositions ADJ and LAD moved to Appendix A (applied)", [
    (r"\begin{proof} With $Q$ the belief after the first step", r"\end{proof}",
     r"""The proof is provided in Appendix~\ref{app:proofs}."""),
    (r"\begin{proof} Every Jeffrey step, damped or not, multiplies the rows", r"\end{proof}",
     r"""The proof is provided in Appendix~\ref{app:proofs}."""),
    ("insert_para", r"the annihilator of a single direction is three-dimensional rather than two",
     r"constants and $\nabla\assoc$.",
     r"""\subsection{Proof of Proposition~\ref{prop:ADJ}}

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
\qed

\subsection{Proof of Proposition~\ref{prop:LAD}}

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
\qed""")],
    "Author's request (2026-10-03), applied directly at the author's instruction and recorded "
    "here afterwards: to keep Section 6 short, the two proofs move word for word to Appendix A, "
    "which already holds the proofs of DIV, SEP, LOS and PRO, and each proposition ends with the "
    "pointer the others use. Nothing else in the paper referred into either proof, and the "
    "introduction's roadmap sentence already sends readers to the appendix for deferred proofs. "
    "Section 6 falls from about 2,450 to 2,120 words; the manuscript stays at 26 pages.",
    "A line diff of the manuscript before and after: the only lines removed are the two "
    "\\begin{proof} and \\end{proof} pairs, and the only lines added are the two pointers, the "
    "two subsection headings and two \\qed. Anchoring.lean (`dampedB_deviation`, "
    "`routeDamped_mA1_deviation`) and Ladder.lean (`ladder_gap`, `ladder_assoc_coeff`, "
    "`ladder_oddsShadow_seqEffect`) record the proofs' content."))

E.append(("C.22", "Section 6 merged into one Scope section (applied)", [
    (r"\section{Scope and robustness}", r"\section{Scope and robustness}", r"""\section{Scope}"""),
    (r"\subsection{Scope}\label{sec:scope-assumptions}", r"\subsection{Scope}\label{sec:scope-assumptions}",
     r"""% Deleted (see Why): the subsection heading goes."""),
    ("A separate scope question concerns rival mechanisms", r"Table~\ref{tab:robust} collects the answers.",
     r"""% Deleted (see Why): the rival-mechanisms paragraph, the second subsection heading and its opening paragraph go."""),
    ("Partial adoption therefore moves the sequence effect on each belief statistic other", "one order earlier in $c$,",
     r"""Partial adoption therefore moves the sequence effect on each belief statistic other
than the odds ratio one order earlier in $c$ (Table~\ref{tab:robust}),"""),
    ("insert_para", "Rubric scoring, which fixes what each document delivers",
     "two sequences differ even when the attributes are believed unrelated.",
     r"""Three cautions bound these results. The
classification covers this one-parameter family, not every conceivable mechanism.
\citet{Asch1946} is evidence that sequence moves marginals, six stimulus terms read
in two sequences moving the proportions on eighteen response traits, and not
evidence for either endpoint of the family, $\omega=0$ or $\omega=1$, since his own account is that early terms set a
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
assumed."""),
    (r"(Section~\ref{sec:robust})", r"(Section~\ref{sec:robust})", r"""(Section~\ref{sec:scope})""")],
    "Author's request (2026-10-03), applied directly at the author's instruction and recorded "
    "here afterwards. Sections 6.1 and 6.2 repeated each other: the adoption weight read from "
    "three ratings (6.1's last sentence, 6.2's opening, Proposition ADJ), the odds ratio unchanged "
    "at every weight (6.1, 6.2's opening, the two-channels paragraph, LAD(iii), the table), which "
    "marginal matches its delivered credence at the two endpoints (6.1 and ADJ), and Hogarth and "
    "Einhorn's partial adoption (6.1, 6.2's opening, the partial-adoption paragraph). The section "
    "becomes one, titled Scope, dropping \"and robustness\" as entry 6.1 had left open. Kept "
    "unchanged: the assumptions paragraph, the two-channels paragraph, the partial-adoption "
    "paragraph with ADJ, LAD and the table, and the rubric sentence. Cut: 6.2's opening paragraph, "
    "which said nothing the two-channels paragraph and ADJ do not, and the comparison with Hogarth "
    "and Einhorn that opened the rival-mechanisms paragraph, which ADJ states (which marginal keeps "
    "its credence at each endpoint, and the sequence effect at independence that the benchmark "
    "lacks). One remark of that paragraph goes without being restated, that the belief-adjustment "
    "model predicts recency when every cue, not only the later one, is adopted in part from an "
    "anchor; it concerns a rule outside the one-parameter family and can return as a clause in the "
    "partial-adoption paragraph if the author wants it. The Asch and Hawthorne cautions close the "
    "section word for word, with \"the claim\" (which pointed to the cut sentences) read as \"these "
    "results\" and the endpoints glossed as $\\omega=0$ or $\\omega=1$, as Proposition ADJ states "
    "them. The table's only pointer was in the cut paragraph and moves to the sentence after LAD; "
    "Section 2's reference to the removed subsection now points to the section. Section 6 falls "
    "from 2,124 to 1,719 words (text outside the table, commands and comments excluded); the "
    "manuscript is 26 pages.",
    "No new claim. The manuscript builds with no undefined references; every statement kept rests "
    "on the records already cited for 6.3 and 6.4 (Anchoring.lean, Ladder.lean, verify_ladder.py "
    "99/99, verify_interior_omega.py 39/39, Asch.lean, Hawthorne.lean)."))

E.append(("C.23", "The two channels named by what each needs (applied)", [
    ("Instead of being the property of the observer (position channel),", "(association channel).",
     r"""Sequence dependence is not assumed here as a weight on a cue's position (position
channel) but derived from a believed link between the traits (association channel)."""),
    ("Since sequence dependence of cues is indeed observed in practice,", "for unrelated attributes as well.",
     r"""Under full adoption, sequence dependence sits in neither the evidence nor the
evaluator alone, since it needs both cues that deliver credences rather than Bayes
factors and an evaluator who believes the traits related."""),
    ("A cue on one attribute moves the belief about the other", "a belief the first has already changed.",
     r"""A cue on one attribute moves
the evaluator's belief about the other through the association in the evaluator's
prior, so the second cue meets a belief the first has already changed."""),
    ("The position channel is a temporal bias of the observer", "exists only when they are.",
     r"""The
position channel is a weight the evaluator gives a cue for its position, present
whether or not the attributes are believed related, while the association channel
needs no such weight and exists only when they are.""")],
    "Author's request (2026-10-03), applied directly at the author's instruction and recorded "
    "here afterwards. The author asked whether the Section 6 sentence on the association channel "
    "made beliefs external to the observer, and then why the position channel needs sequence "
    "dependence at unrelated traits while the association channel can have none. The text set "
    "\"the property of the observer\" (position channel) against \"a property of believed "
    "association\" (association channel), but both belong to the evaluator; what differs is that "
    "the position channel puts sequence dependence in by a weight on a cue's position, while the "
    "association channel derives it from cues weighed alike in either position. Part 1 states that "
    "contrast in Section 3 at the same length (26 words against 24). Part 2 is the author's choice "
    "of sentence, stating what full adoption needs: remove the believed association and the two "
    "sequences give one belief (Proposition IMM), and remove the delivered credences, reading "
    "each cue as a Bayes factor the same in either position, and they give one belief at every "
    "association (Wagner 2002). It replaces the sentence the author found confusing, that the only "
    "way for sequence dependence to exist under partial adoption is for it to be there for "
    "unrelated attributes as well, whose premise that sequence dependence is observed in practice "
    "goes with it (34 words against 33). \"Needs\" claims only necessity: the dependence can still "
    "vanish at $c\\neq0$, as when a cue delivers the prior marginal. Part 3 names the evaluator as "
    "the holder of the association, which \"the prior association\" left open to a reading as a "
    "correlation among applicants (31 words against 27). Part 4 replaces \"temporal bias of the "
    "observer\", which writing_discipline.md 6 rules out twice (\"bias\" collides with biased "
    "beliefs, and the observer is the auditor, not the evaluator), and \"what one cue implies about "
    "the other\", which left the implication without an owner (37 words against 38).",
    "PropIMM.lean (propIMM_indep); Wagner 2002, Theorem 3.1, and Wagner2002.lean (thm31); "
    "PropDIV.lean (jeffreyA_prior_mB0); PropORD.lean (propORD_Amarg); Anchoring.lean "
    "(orderEffect_damped_at_indep)."))

E.append(("C.24", "Section 6: Proposition ADJ dropped, its first identity kept as a sentence, the proposition kept in an addendum (applied)", [
    ("Let $P^{A}$ and $P^{B}$ denote the belief after the $A$-cue alone", r"The proof is provided in Appendix~\ref{app:proofs}.",
     r"""Let $P^{A}$
denote the belief after the $A$-cue alone. The rule sets the rating of the attribute
read second to $P^{\omega}_{AB}(B{=}1)=(1-\omega)P^{A}(B{=}1)+\omega r_1$, so whenever
$P^{A}(B{=}1)\neq r_1$ three ratings from one reading group give $\omega$, and full
adoption is the case in which the final rating equals what the second cue delivers
alone."""),
    (r"Proposition~\ref{prop:ADJ} reads the weight from the ratings.", r"Proposition~\ref{prop:ADJ} reads the weight from the ratings.",
     r"""% Deleted (see Why): the bridge sentence goes with the proposition."""),
    ("What the paper says about the position channel is confined to", "registers the sequence.",
     r"""What the
paper says about the position channel is confined to reading its strength from the
ratings and to Proposition~\ref{prop:LAD}, which states what it does to the order in
$c$ at which each statistic registers the sequence."""),
    (r"from its score (Proposition~\ref{prop:ADJ}) and the", "believed unrelated.",
     r"""from its score and the two sequences differ even when
the attributes are believed unrelated (Proposition~\ref{prop:LAD})."""),
    (r"\subsection{Proof of Proposition~\ref{prop:ADJ}}", r"\qed",
     r"""% Deleted (see Why): the proof moves to PAPER_B_ADDENDUM.tex with the proposition.""")],
    "Author's question and decision (2026-10-03): what use is the adoption weight in the main "
    "conclusions, and is ADJ needed. None of the results of Sections 4 and 5 uses the weight; they "
    "are all at full adoption, and the weight enters only as the scope condition, through LAD and "
    "through the reply to Hawthorne (Section 3, \"a reason to measure the degree of adoption\"; "
    "the cautions, \"recovered from three ratings of one marginal rather than assumed\"). That reply "
    "needs only that the weight can be read from the ratings, which ADJ's first identity gives, and "
    "that identity is the partial-adoption rule read backwards: the proof's first line is "
    "$P(B{=}1)=t_1$, the damped step attaining its target. ADJ's gap at $c=0$ is LAD(i) again. Its "
    "second identity, for the marginal read first, and the coefficient claim drawn from it are "
    "used nowhere else in the paper. Part 1 states the first identity as one sentence where the "
    "rule is introduced, with the case of full adoption, and keeps the worked example; $P^{B}$, "
    "used only by ADJ, goes. Part 3 keeps the two-channels sentence's length (35 words against "
    "36). Part 4 sends the rubric sentence's claim about unrelated attributes to LAD(i). The "
    "proposition, its proof and the example are kept word for word in PAPER_B_ADDENDUM.tex, which "
    "the numbered copy does not replicate (the copy's script now expects twelve mnemonics). "
    "Section 6 falls from 1,722 to 1,591 words (text outside the table, commands and comments "
    "excluded, so the displays dropped are not counted), and the manuscript from 26 pages to 25.",
    "Anchoring.lean (dampedB_mB1, dampedB_deviation, orderEffect_damped_at_indep); "
    "sympy/verify_example.py. The manuscript builds with no undefined references and no "
    "remaining mention of ADJ or $P^{B}$."))

E.append(("C.25", "Setup 2.2: the results are stated at full adoption (applied)", [
    ("insert_sentence", "The two-cue Jeffrey posteriors are the two sequences", r"\PJ_{BA}=\PJ_{A}\!\circ\PJ_{B}\,P . \]",
     r"""Resetting the marginal to the delivered credence is adoption in full, $\omega=1$, and
every result of Sections~\ref{sec:individual} and~\ref{sec:aggregation} is stated at
that weight; Section~\ref{sec:scope} says what changes at $\omega<1$, down to
$\omega=0$, where the second cue is ignored.""")],
    "Author's question (2026-10-04): are the results about $\\omega=0$ or $\\omega=1$, or both. "
    "They are all at $\\omega=1$, but the manuscript did not say so where a reader needs it. "
    "Setup 2.1 introduces $\\omega$ with both endpoints; Section 2.2 then defines the Jeffrey "
    "step as resetting the marginal to the delivered credence, which is the $\\omega=1$ step, "
    "without naming the weight; Sections 4 and 5 never mention adoption; and the first statement "
    "that they sit at $\\omega=1$ was in Section 6. The sentence goes right after the two "
    "sequences are defined, where a reader who has just met $\\omega$ first sees the step, and "
    "names $\\omega=0$ as the other end of what Section 6 covers (Proposition LAD holds for "
    "$0\\le\\omega\\le1$). Applied at the author's instruction.",
    "Anchoring.lean (dampedB_at_one, dampedB_at_zero, routeDamped_at_zero_pins_A)."))

E.append(("C.26", "Section 6 repaired after the author's cuts, and the Hawthorne paragraph given its substance (applied)", [
    ("the evaluator's belief about the other in the evaluator's prior, so that", "the evaluator's belief about the other in the evaluator's prior, so that",
     r"""the evaluator's belief about the other through the association in the evaluator's prior, so that"""),
    ("The position channel is not a feature of updating on delivered credences.", "The position channel is not a feature of updating on delivered credences.",
     r"""% Deleted (see Why): the claim lost its support when the Wagner sentence was cut."""),
    ("is that through position channel, present under partial adoption,", "is that through position channel, present under partial adoption,",
     r"""is that through the position channel, present under partial adoption,"""),
    ("shifts by one order -- with each statistic keeping its place", "shifts by one order -- with each statistic keeping its place",
     r"""shifts by one order (Table~\ref{tab:robust}), with each statistic keeping its place"""),
    ("and the odds being the same in both sequences at every weight.", "and the odds being the same in both sequences at every weight.",
     r"""and the odds ratio being the same in both sequences at every weight."""),
    ("What the next result says what the weight does", "What the next result says what the weight does",
     r"""The next result says what the weight does"""),
    ("And full adoption is a substantive commitment", "rather than assumed.",
     r"""Reading a cue as a credence does not oblige the evaluator to adopt it in full, as
the partial-adoption rule above shows, so full adoption is an assumption of this
paper, and \citet{Hawthorne2004} objects that it ``seems implausible that the most
recent experience or non-propositional state should completely dictate belief
strengths for basis sentences, with no regard for the import of previous
experiences or states''. The objection applies here, since the letter erases the
credential's implication for trustworthiness. If it holds, unrelated traits lose
their immunity (Proposition~\ref{prop:IMM}), and Theorem~\ref{thm:LOS}, which
needs a first-order score-gap, no longer applies, since the gap is then of order
zero (Table~\ref{tab:robust}). The ranking survives the objection, so that a
question about the believed prevalence of an attribute still registers the sequence
one order in $c$ before a question about the believed association.""")],
    "The author's cuts of 7e8d546e (\"Cuts.\") shortened Section 6 and left breakages, listed "
    "for the author as BEFORE/AFTER pairs; the author chose all but one (the backward pointer "
    "\"As explained with Proposition LAD\" stays). Part 1 restores \"through the association\", "
    "whose loss made the sentence say the belief lies in the prior. Part 2 cuts a claim whose "
    "support, the Wagner sentence, the cuts removed. Parts 3, 5 and 6 fix an article, \"odds\" "
    "for \"odds ratio\" and a garbled bridge sentence. Part 4 replaces a dash with a comma and "
    "restores the only pointer to the partial-adoption table. Part 7 rewrites the closing "
    "paragraph. The author judged its reply to Hawthorne no response at all (\"yes $\\omega=1$ "
    "is problematic but we don't really care\") and asked what \"a substantive commitment rather "
    "than a consequence of the level reading\" meant; that phrase was this plan's (entry 6.2), "
    "and Hawthorne neither separates a credence input from full adoption nor proposes a partial "
    "weight. The new paragraph says the assumption is the paper's, since the partial-adoption "
    "rule reads cues as credences too; quotes the objection (pp. 98-99); concedes that it "
    "applies; and states what it costs if it holds (the immunity of Proposition IMM, and Theorem "
    "LOS, whose hypothesis is a first-order score-gap) and what it leaves (marginals register "
    "the sequence one order before the believed association at every weight, LAD against ORD and "
    "DEC). The three ratings are not repeated, since the partial-adoption paragraph states them. "
    "The paragraph is about 25 words longer than the author's, the cost of the substance.",
    "PropDIV.lean (jeffreyA_prior_mB0); Anchoring.lean (orderEffect_damped_at_indep); "
    "Ladder.lean (ladder_gap, assoc_routeDamped, ladder_assoc_coeff, ladder_assoc_coeff_at_one); "
    "PropORD.lean (propORD_Amarg); Decision.lean (volume_flipSet); Hawthorne 2004, pp. 98-99."))

E.append(("C.27", "Section 6, the four cases of $c$ and $\\omega$ (with a table) and the position channel's varieties", [
    ("As discussed in Section \\ref{sec:intro}, the two channels", "at every weight.",
     r"""The two channels of Section~\ref{sec:intro} differ in kind, and each has its
parameter. The association channel belongs to what the evaluator believes, and the
prior covariance $c$ represents it completely, since a belief about two binary
attributes has three degrees of freedom, the two marginals and $c$. The position
channel belongs to how the evaluator revises, and the adoption weight $\omega$
represents one variety of it, a weight on the cue read second, set out with the
others below. The two parameters give four cases (Table~\ref{tab:cases}). At $c=0$
the evaluator believes the attributes unrelated, and at $c\neq0$ believes them
related in either direction. At $\omega=1$ each cue is adopted in full whenever it is
read, at $0<\omega<1$ the cue read second moves the belief only part of the way, and
at $\omega=0$ it is ignored. The results of Sections~\ref{sec:individual}
and~\ref{sec:aggregation} are those of the case $c\neq0$ and $\omega=1$, where the
association channel is the only one. At $c=0$ and $\omega=1$ no statistic registers
the sequence, so every effect those sections find needs a believed link, and every
effect the position channel adds carries the factor $1-\omega$
(Proposition~\ref{prop:LAD}). In all four cases the odds ratio is the same in both
sequences, and wherever the sequence registers, the marginals register it at a lower
order in $c$ than the believed association. What the results keep when $\omega<1$ is
taken up at the end of this section."""),
    ("insert_sentence", "The rule is the averaging form of the belief-adjustment model,", "and embedded in the joint law by a Jeffrey step.",
     r"""The rule adopts the first cue in full and gives the second the weight $\omega$,
the same whichever cue is read second, whatever it delivers and whatever marginal it
meets. The position channel admits other weights. The first cue may also be
discounted, its step moving the marginal only a fraction $\omega_1$ of the way from
the prior's marginal to the delivered credence; in the belief-adjustment model, where
both cues bear on one judgment, equal weights on the two cues make the cue read last
weigh more \citep[Appendix~B]{HogarthEinhorn1992}. The weight may depend on what the
cue delivers, as under their contrast assumption, in which a cue delivering $r_1<m$
gets a weight proportional to $m$ and one delivering $r_1>m$ a weight proportional to
$1-m$. And the weight may differ between attributes, as it would for a panel that
adopts a letter read second with one weight and a credential read second with
another. Proposition~\ref{prop:LAD} below and the second column of
Table~\ref{tab:cases} are proved for the rule and are not claimed for these
variants."""),
    ("insert_para", "The ranking survives the objection,", "before a question about the believed association.",
     r"""\begin{table}[htbp]
\centering
\small
\begin{tabular}{@{}p{3.0cm}p{5.6cm}p{5.6cm}@{}}
\toprule
 & \textbf{Full adoption, $\omega=1$} & \textbf{Partial or no adoption, $\omega<1$} \\
\midrule
\textbf{Attributes believed unrelated, $c=0$}
  & Neither channel. No statistic registers the sequence (Proposition~\ref{prop:IMM}).
  & Position channel only. $P(A{=}1)$ differs between the sequences by $(1-\omega)(\alpha-q_0)$ and $P(B{=}1)$ by $-(1-\omega)(\beta-r_0)$; the believed association is zero in both. \\
\addlinespace
\textbf{Attributes believed related, $c\neq0$}
  & Association channel only. The marginals and the share of decisions changed differ at first order in $c$, the believed association and the surplus-weighted loss at second order (Sections~\ref{sec:individual} and~\ref{sec:aggregation}).
  & Both channels. The marginals differ at order zero and the believed association at first order, by $c(1-\omega)H/Z$ (Proposition~\ref{prop:LAD}). \\
\bottomrule
\end{tabular}
\caption{What registers the sequence in each of the four cases. In every case the odds
ratio is the same in both sequences. The second column is for the rule of this section,
a weight $\omega$ on the cue read second, and is not claimed for the other varieties of
the position channel.}
\label{tab:cases}
\end{table}"""),
    (r"\textbf{Partial adoption, $\omega<1$}", r"\textbf{Partial adoption, $\omega<1$}",
     r"""\textbf{Partial or no adoption, $\omega<1$}""")],
    "Author's request (2026-10-04), after a discussion of what the two channels are: set out how "
    "they differ, how $c$ and $\\omega$ represent them, what the position channel comprises, how "
    "the four cases $c=0$, $c\\neq0$, $\\omega=1$ and $\\omega<1$ are to be read, and how sensitive "
    "the results are to each, with a table. The introduction (entry 1.3) states the difference in "
    "kind; this entry carries the rest in Section 6. Part 1 replaces the two-channels paragraph, "
    "which made part of this case and which the author's cuts and edits had left partly garbled: "
    "it says that $c$ represents its channel completely (a belief about two binary attributes has "
    "three degrees of freedom, as Section 4 says) and $\\omega$ only one variety of its, reads each "
    "of the four cases, and states what is sensitive to each parameter (every effect of Sections 4 "
    "and 5 needs $c\\neq0$; every position effect carries $1-\\omega$; the odds ratio and the "
    "marginals' lead over the association hold in all four). What each result keeps when "
    "$\\omega<1$ stays in the Hawthorne paragraph at the end of the section, which already states "
    "it, so it is pointed to and not repeated. The position channel's numbers at $c=0$ move into "
    "the table. Part 2 is the earlier 6.9 text without its odds-ratio sentences, which the author "
    "found made no point on their own; the odds-ratio claim now stands in Part 1 and the table, "
    "where it says what does not depend on either parameter. Part 3 is the table of the four "
    "cases; it sits next to the existing table, which gives each statistic's order under full and "
    "under partial adoption, and does not repeat it. Part 4 relabels that table's second column, "
    "since $\\omega=0$ is no adoption, not partial adoption (writing_discipline.md 6). Hogarth "
    "and Einhorn's equal-weight result is labelled as theirs for a single judgment.",
    "PropIMM.lean (`propIMM_indep`); Ladder.lean (`ladder_gap`, `assoc_routeDamped`, "
    "`ladder_assoc_coeff`, `ladder_assoc_coeff_at_one`, `jeffreyA_eq_rescale`, `oddsRatio_rescale`); "
    "PropORD.lean (`propORD_Amarg`); Decision.lean (`volume_flipSet`); HogarthEinhorn.lean "
    "(`oneSided_eq_eq8`, `twoSided_recency`, `contrastWeight`)."))

E.append(("C.19", "Concluding remarks, third implication: an existing design that asks a difference question and reads decisions", [
    ("What the paper finds is that a population could pass every",
     "lives entirely in outcomes.",
     r"""\citet{CoffmanExleyNiederle2021}, for instance, ask employers how much better
they believe one group of workers performs than another, and read discrimination
from the hiring decisions themselves. A believed difference between groups is second
order under the sequence dependence studied here, while the share of decisions the
reading sequence changes is first order (Propositions~\ref{prop:PRO}
and~\ref{prop:SHR}). Such a belief audit therefore cannot rule out a role for the
reading sequence, since a population of coherent evaluators would pass it while its
hiring decisions record the sequence.""")],
    "Author's request (2026-10-02): name an existing instrument that sets a difference question "
    "against a decision rate. Coffman, Exley and Niederle (read in full, the February 2020 working "
    "paper the author linked) elicit the believed gap in average scores between two groups of "
    "workers (p. 8) and measure discrimination by how often the worker from one group is hired when "
    "the two have identical scores (pp. 15-17); the believed gap predicts those decisions (Table 1, "
    "column 2). A difference between groups has the form of the conditional difference "
    "P(B=1|A=1) - P(B=1|A=0), which Proposition PRO places in the protected class (second order), "
    "while the share of decisions changed is first order (Proposition SHR). The match is in the "
    "form of the question only: in their design group membership is observed and nothing is read "
    "in sequence, and they set belief formation aside (fn. 5, p. 4), so the sentence says what a "
    "question of that form records under the sequence dependence studied here, not what their data "
    "show. The entry replaces the sentence 'a population could pass every belief-level audit "
    "sincerely while the footprint of sequence-dependence lives entirely in outcomes' rather than "
    "adding to it, because that sentence contradicts the paper's own classification: a belief "
    "audit that asks a level question (a marginal, or a conditional probability such as "
    "P(B=1|A=1)) records the sequence at first order (Propositions DRF and PRO), so only audits "
    "asking how much likelier one trait makes another are passed. Bohren, Haggag, Imas and Pope "
    "(2025), who ask the level question for each group, are left for the author's review. Page "
    "numbers are the working paper's and are to be checked against Management Science 67(6), "
    "3551-3569, before applying. Needs the bibliography entry in B.2. Amended at the author's "
    "request (2026-10-02): the last sentence says what the audit cannot rule out rather than "
    "that a population passes it, so the claim rests on one admissible population, coherent "
    "evaluators updating by Jeffrey's rule, and not on how real evaluators update; it has the "
    "form of Canay, Mogstad and Mountjoy's Theorem 4.1 (an outcome test may find no bias in a "
    "biased judge). It is a full-adoption claim: under partial adoption the conditional "
    "difference differs between the sequences at first order, with coefficient "
    "$(1-\\omega)(r_0-\\beta)(t_0-r_1)/Z$, $t_0$ the damped $B$-marginal, which vanishes at full "
    "adoption, when the letter delivers the prior marginal, and at one further weight fixed by "
    "the prior and the cue, so a partly adopting population would in general not pass the audit.",
    "sympy/verify_PRO.py (59/59: the conditional difference is protected, the conditional "
    "probability and a cell probability are not); sympy/verify_ladder.py row (B), 99/99 (the "
    "conditional difference is first order under partial adoption); PropPRO.lean; "
    "Decision.lean (Proposition SHR); "
    "literature/coffman_exley_niederle2021 (Lean + sympy)."))

E.append(("C.18", "Section 6.2 compacted: Proposition FAC and the settings table go, the rubric paragraph becomes one sentence", [
    ("Whether a population adopts in full can be read from its",
     "under either reading of the cues.",
     r"""Whether a population adopts in full can be read from its ratings, since
Proposition~\ref{prop:ADJ} below recovers the adoption weight from three marginals
of one reading group, and Proposition~\ref{prop:LAD} says what to expect when it
does not."""),
    ("The position channel is not a feature of updating on delivered credences.",
     "since commutation requires the same factor in either position \\citep{Wagner2002}.",
     r"""The position channel is not a feature of updating on delivered credences. A
Bayes-factor update that gives the second cue's factor the weight $\omega$ is
sequence-dependent in the same way, the marginals differing at order zero and the
believed association at first order while the odds ratio is unchanged, since
commutation requires the same factor in either position \citep{Wagner2002}."""),
    ("Table~\\ref{tab:settings} sets the two settings side by side.",
     "Table~\\ref{tab:settings} sets the two settings side by side.",
     r"""% Deleted (see Why): the settings table goes."""),
    ("\\paragraph{Factor inputs.} Under the benchmark reading each cue supplies a factor",
     "and the argument of Proposition~\\ref{prop:LAD}(iii) applies. \\end{proof}",
     r"""% Deleted (see Why): Proposition FAC and its proof go; the two-channel paragraph carries the point."""),
    ("\\begin{table}[htbp] \\centering \\small \\begin{tabular}{@{}p{3.4cm}cccc@{}}",
     "\\label{tab:robust} \\end{table}",
     r"""\begin{table}[htbp]
\centering
\small
\begin{tabular}{@{}p{5.2cm}cc@{}}
\toprule
\textbf{Sequence effect on} & \textbf{Full adoption, $\omega=1$} & \textbf{Partial adoption, $\omega<1$} \\
\midrule
Marginals & $\Theta(c)$ & $\Theta(1)$ \\
Believed association & $\bigO(c^{2})$ & $\Theta(c)$ \\
Statistics agreeing with the log odds ratio to first order & $\bigO(c^{2})$ & $\bigO(c^{2})$ \\
Odds ratio & $0$ & $0$ \\
Share of decisions changed & $\Theta(c)$ & $\Theta(1)$ \\
Surplus-weighted loss & $\bigO(c^{2})$ & $\Theta(1)$ \\
\bottomrule
\end{tabular}
\caption{The between-sequence effect of each statistic under full and partial
adoption. Entries are orders in the prior covariance $c$, generic in the prior and
the cues, and $0$ means no sequence effect at any $c$. The first column is
Sections~\ref{sec:individual} and~\ref{sec:aggregation}, the second
Proposition~\ref{prop:LAD}; the decision rows of the second read
Theorem~\ref{thm:LOS} with a score gap of order zero and are not separately
verified.}
\label{tab:robust}
\end{table}"""),
    ("\\paragraph{Opposite predictions about one procedure.} Whether a procedure removes",
     "to say which prediction holds.",
     r"""Rubric scoring, which fixes what each document delivers, tells the two settings
apart, since under full adoption it leaves the sequence effect in the belief about
the attribute read first and invisible in the scores (Proposition~\ref{prop:DRF}),
while under partial adoption the belief about the attribute read second sits
$(1-\omega)[P^{A}(B{=}1)-r_1]$ from its score (Proposition~\ref{prop:ADJ}) and the
two sequences differ even when the attributes are believed unrelated."""),
    ("\\begin{table}[htbp] \\centering \\small \\begin{tabular}{@{}p{2.6cm}p{5.4cm}p{5.4cm}@{}}",
     "\\label{tab:settings} \\end{table}",
     r"""% Deleted (see Why): the ten-row settings table goes.""")],
    "Author's decision (2026-10-02): Section 6.2 had become the longest subsection in the paper "
    "and completeness was costing readability. Proposition FAC filled the fourth cell of the "
    "two-by-two, factors read as inputs and the later one adopted in part, and no result cites "
    "it; the two-channel paragraph already stated its content with Wagner (2002), so FAC and its "
    "proof go (part 4) and that sentence gains the verified orders (part 2). The framing sentence "
    "cites LAD alone (part 1). The four-settings table loses its two factor columns, which the "
    "amended sentence now carries, and its caption is shortened (part 5). The rubric paragraph, "
    "an empirical implication rather than a result, becomes one sentence (part 6). The ten-row "
    "settings table overlapped the four-settings table and the two-channel prose, so it goes "
    "(part 7), together with the sentence that pointed to it (part 3). Nothing verified is "
    "lost: the factor-input theorems stay in Ladder.lean and section F of verify_ladder.py as "
    "the record behind part 2. Entry B.1, the AI declaration, no longer names FAC.",
    "Ladder.lean (`rescale_rescale`, `oddsRatio_factorRoute`, `assoc_factorRoute`, "
    "`factorRoute_at_zero`, `factor_mA1_gap_iff`, `mprod_factorRoute_zero`); "
    "sympy/verify_ladder.py section F (99/99); LadderFactorPow.lean; Wagner2002.lean (thm31); for the rubric "
    "sentence, Aggregate.lean (propDRF_route_AB) and Anchoring.lean (dampedB_deviation)."))

E.append(("C.13", "Section 4, the Foster-Greer-Thorbecke sentence", [(
    "This is the standard incidence-versus-intensity pairing of the measurement literature",
    "\\citep{FosterGreerThorbecke1984}.",
    r"""The two decision statistics pair incidence with intensity, as do the first two
poverty measures of \citet{FosterGreerThorbecke1984}, the headcount ratio, the share
below a threshold, and the average shortfall below it normalised by the threshold,
to which those above it contribute zero.""")],
    "P_0 is the headcount *ratio*, a share, and P_1 = (1/n) sum g_i / z is normalised by the "
    "threshold and averaged over the whole population; \"average of their distances\" is the "
    "income-gap measure, which is not in the family (M15). FGT do not use "
    "\"incidence/intensity\"; the AFTER does not attribute it to them.",
    "FGT.lean (`P0_eq_H`, `P1_eq_popMean_normGap`, `I_not_decomposable`)."))

E.append(("C.14", "Appendix, proof of Theorem LOS, the two Tao citations", [
    ("Tonelli's theorem \\citep[Corollary~1.7.23]{tao2011measure} gives", "\\citep[Corollary~1.7.23]{tao2011measure} gives",
     r"""Tonelli's theorem \citep[Theorem~1.7.15]{tao2011measure}, applicable since the
integrand is nonnegative, gives"""),
    ("As $\\mu$ is finite, continuity from above", "\\citep[\\S1.4]{tao2011measure} yields",
     r"""As $\mu$ is finite and $B_c$ decreases as $c\downarrow0$, continuity from above
\citep[Exercise~1.4.23(iii)]{tao2011measure}, applied along any sequence
$c_n\downarrow0$, yields""")],
    "Corollary 1.7.23 is Fubini-Tonelli; for the nonnegative integrand Tonelli (Theorem 1.7.15, "
    "or 1.7.18 for complete measures) is the exact reference. Continuity from above is Exercise "
    "1.4.23(iii), stated for sequences; the proof takes c to 0 over a continuum (M16). Numbering "
    "is from the author's preprint of Tao; confirm against the printed book.",
    "Tao.lean (`thm_1_7_15`, `ex_1_4_23_iii`, `measure_band_tendsto_zero`, `los_step4`)."))

out = []
out.append("""## C -- Corrections to the author's draft (audit-driven)

These entries correct the manuscript as committed at c2ae782f (2026-10-01). The
author applied C.1 to C.4 in their own wording at that commit; those four entries
are rebased on the applied text and keep only what still needs correcting. Each BEFORE block is cut from that file by script, so it matches
the manuscript text exactly (whitespace normalised). Each AFTER block is
paste-ready LaTeX written under `notes/writing_discipline.md` (no colons, both
sides of each contrast named, rewrites within 80-120% of the draft's length;
exceptions are deletions, citation
fixes on short phrases, and C.6, which replaces a refuted argument). Each entry names
the audit item in `notes/citation_audit.md` and the Lean or sympy record that
justifies it. **None is applied.** The author approves each entry, and approved
entries are then applied and committed.

Entries C.1-C.14 are in manuscript order; C.15, the identification paragraph from
notes/positioning_economics.tex, is appended inside C.11.9. C.12 lists the bibliography entries
the AFTER texts need. Section D lists what these corrections change in the
existing plan entries 0.A-6.B.
""")
from plan_entries_E import E2
for eid, title, pairs, purpose, verif in E + E2:
    out.append(f"### {eid} -- {title}\n")
    if eid in APPLIED:
        out.append(f"**Applied to the manuscript at {APPLIED[eid]}.** BEFORE is from the manuscript just before that commit.\n")
    out.append(f"**Why.** {purpose}\n")
    for k, part in enumerate(pairs, 1):
        kind, s, e, after = norm_part(part)
        before = cut(s, e, eid, k)
        tag = f" ({k} of {len(pairs)})" if len(pairs) > 1 else ""
        if kind == "insert_cont":
            out.append(f"**BEFORE{tag}:** continues the insertion of the previous part.\n")
        else:
            if (eid, k) in APPLIED_PARTS:
                out.append(f"*Part {k} applied to the manuscript at {APPLIED_PARTS[(eid, k)]}.*\n")
            out.append(f"**BEFORE{tag}:**\n\n{quote(before)}\n")
        out.append(f"**AFTER{tag}:**\n\n{block(after)}\n")
    out.append(f"**Verification.** {verif}\n")
    if eid == "C.11":
        out.append("""### C.12 -- Bibliography entries the AFTER texts need

`Hawthorne2004` and `ZhaoOsherson2010` were added to `bibliography.bib` at
e7b997e3 (mechanical, already committed). Two more are needed only if C.4's
optional sentence and C.6/C.10 are approved:

```bibtex
@book{Jeffrey2004,
  author    = {Jeffrey, Richard C.},
  title     = {Subjective Probability: The Real Thing},
  publisher = {Cambridge University Press},
  address   = {Cambridge},
  year      = {2004}
}

@unpublished{BenjaminBodohCreedRabin2019,
  author = {Benjamin, Daniel J. and Bodoh-Creed, Aaron and Rabin, Matthew},
  title  = {Base-Rate Neglect: Foundations and Implications},
  note   = {Working paper, July 19, 2019},
  year   = {2019}
}
```

The Drive copy of Jeffrey (2004) is the November 2002 draft, so C.6 and C.10 cite
the chapter, not a page or section number.
""")
out.append("""## D -- What the corrections change in existing plan entries

The entries 0.A-6.B were drafted before the audit. Status against the author's
draft and the audit, to be settled entry by entry before any is applied:

- **0.A (abstract):** superseded by the author's rewrite and C.1.
- **1.A, 1.B, 1.D, 1.E:** superseded by the author's rewrite of the introduction;
  what survives is corrected in C.2-C.8. The ORD sentence of 1.E ("a statistic is
  protected exactly when the sequence effect on it is second order") is still
  unapplied and presupposes 5.B.
- **1.A2 (two-channel prelude):** applied by the author; corrected by C.4.
- **1.C (why sequence matters beyond one judgement):** "rests on a difference in
  beliefs about the groups" should read "prior beliefs" (audit P4); "none of the
  sources that Bohren2019 distinguish" should not imply they have no
  process-driven discrimination (their impartial type, audit D13).
- **2.A, 2.C:** still applicable as written.
- **2.B (worked example):** still applicable; every number is in verify_example.py.
- **3.A (Jeffrey-commutativity literature):** "disputed on psychological grounds"
  holds for Hawthorne, not Doring, whose objection is normative (D1, P3); Doring's
  remedy is a single Jeffrey update from the original prior (P2); "the two ends of
  his taxonomy" should be "his amnestic model and his factor models" (M21); the
  Bayes-factor/NL identification needs the denominator (M21); D-Z are on the
  paper's side (M24).
- **3.B (Asch):** "the joint is never elicited" should be "no prior association
  is elicited" (P13); the Table 7 numbers are all correct.
- **3.C (Heckman):** "no sample size repairs" is the paper's inference, not
  Heckman's (P17a); "nothing guarantees" is p. 109.
- **3.D (Bohren-Imas-Rosenberg):** "vanishes only as judgment becomes perfectly
  objective" omits attenuation along histories and tau_q -> 0 (P17b); their
  pages are working-paper pages.
- **5.B (Propositions ORD and ADJ):** mathematics verified; the prose attributing
  primacy to Hogarth-Einhorn's decaying weights and calling omega = 0 "ours, not
  theirs" is wrong (P10, P11). The one-sided construction is their Eq. 8; what is
  new is its two-attribute Jeffrey embedding. Epstein (2006, eq. 12) gives an
  axiomatised instance of the same averaging form, with omega = 1 - alpha
  lambda/(1+alpha) (optional citation).
- **6.A (rival mechanisms):** the Hogarth-Einhorn direction (P10), "one cue per
  trait across eighteen traits" (P14), and the claim that Hawthorne's
  illustration is single-basis (P9) all need correcting.
- **6.B (two channels, rubric):** carries the interior-omega numbers C.5 points
  to; its position-channel wording needs the C.4 change.
- **B.A (bibliography):** Hawthorne2004 is now in `bibliography.bib`; Heckman1998,
  Doring1999, Garber1980 and Bohren2019 are still to add when 3.A-3.D land.
""")
MD = "\n".join(out)


DISCIPLINE_EXEMPT_LENGTH = {
    "C.2.1": "adds the modus tollens sentence after the author's sentence 1",
                            "C.1.3": "removes the identification overclaim",
                            "C.4.1": "adds the partial-adoption sentence (Proposition LAD)",
                            "C.4.2": "removes a definition of c that the channel sentence now gives first",
                            "C.27.4": "relabels a column to the agreed term, since omega=0 is no adoption",
                            "C.5.1": "adds what survives partial adoption (Proposition LAD)",
                            "C.9w.1": "names the belief-adjustment model the weight nests",
                            "E.13.1": "names the two new propositions",
                            "E.15h.1": "adds the Scope subsection heading",
                            "E.15r.1": "adds the full-adoption clause to the roadmap",
                            "C.18.2": "adds the verified orders to the Wagner sentence",
                            "C.18.5": "drops two columns of the table",
                            "C.18.6": "a paragraph becomes one sentence",
                            "C.19.1": "replaces an overclaiming sentence and names an existing instrument",
                            "C.2.5": "repairs a sentence with two verbs",
                            "C.10.1": "citation fix", "C.11b.3": "deletion",
                            "C.14.1": "citation fix", "C.14.2": "citation fix",
                            "C.11b.4": "appends the identification paragraph",
                            "E.2.1": "adds the definition of an impression",
                            "E.4.1": "adds the bounds on the credences",
                            "E.5.1": "inserts four paragraphs before the existing sentence",
                            "E.6.1": "carries Asch's data",
                            "E.8.1": "title", "E.8.2": "rewrite of two paragraphs, same jobs",
                            "E.9.1": "title", "E.9.2": "rewrite, same job",
                            "E.10.2": "adds a reference", "E.13.1": "adds two references"}


def discipline_check(entries=None):
    """Mechanical rules of notes/writing_discipline.md applied to every AFTER text:
    no colons in prose (9; a colon that introduces a display is allowed), no
    dashes doing a sentence's work (9), "sequence" for reading order and "order"
    only for order in c (6), primacy/recency/anchor only where Hogarth-Einhorn is
    cited and amnestic/overwrite only where Hawthorne is cited (6), rewrites
    within 80-120% of the draft (9) for replacements unless exempted. Exits on a
    breach."""
    if entries is None:
        from plan_entries_E import E2
        entries = E + E2
    def prose(t):
        t = re.sub(r"\\\[.*?\\\]", " ", t, flags=re.S)
        t = re.sub(r"\\begin\{(align\*?|tabular|table)\}.*?\\end\{\1\}", " ", t, flags=re.S)
        t = re.sub(r"\\cite[pt]?(\[[^\]]*\])*\{[^}]*\}", "", t)
        t = re.sub(r"\$[^$]*\$", "", t)
        t = re.sub(r"\\(ref|label|paragraph|caption)\{[^}]*\}", "", t)
        t = re.sub(r"\\(par|renewcommand\{[^}]*\}\{[^}]*\}%?|begin\{[^}]*\}(\[[^\]]*\])?|end\{[^}]*\}|centering|small|toprule|midrule|bottomrule|addlinespace(\[[^\]]*\])?)", " ", t)
        return " ".join(t.split())
    bad = []
    for eid, _, parts, _, _ in entries:
        if eid in APPLIED:
            continue
        for k, part in enumerate(parts, 1):
            kind, st, en, after = norm_part(part)
            tag = f"{eid}.{k}"
            if after.lstrip().startswith("%"):
                continue
            if (eid, k) in APPLIED_PARTS:
                cut(st, en, eid, k)
                continue
            a = prose(after)
            if ":" in a:
                bad.append(f"{tag}: colon in prose")
            if "---" in after or " -- " in after:
                bad.append(f"{tag}: dash doing a sentence's work")
            for m in re.finditer(r"\b(?:in (?:either|any|the same|reverse|both) order|order in which|order of (?:arrival|reading)|treats order|arrival order|reading order|order effect|order dependence|order-free|order-dependen\w*)\b", a):
                bad.append(f"{tag}: 'order' used for reading sequence ({m.group(0)!r})")
            he = "HogarthEinhorn1992" in after
            hw = "Hawthorne2004" in after
            for w in ("primacy", "recency", "anchor"):
                if w in a.lower() and not he:
                    bad.append(f"{tag}: term {w!r} without a Hogarth-Einhorn citation")
            for w in ("amnestic", "overwrit"):
                if w in a.lower() and not hw:
                    bad.append(f"{tag}: term {w!r} without a Hawthorne citation")
            for w in ("base rate", "base-rate"):
                if w in a.lower():
                    bad.append(f"{tag}: term {w!r} (use 'marginal')")
            if kind == "replace":
                bw, aw = len(cut(st, en).split()), len(after.split())
                if not 0.8 <= aw / bw <= 1.2 and tag not in DISCIPLINE_EXEMPT_LENGTH:
                    bad.append(f"{tag}: length {bw}->{aw} words outside 80-120%")
            else:
                cut(st, en)   # the anchor must exist, once
    if bad:
        sys.exit("writing_discipline breaches:\n  " + "\n  ".join(bad))

def write_md(path):
    open(path, "w").write(MD)



if __name__ == "__main__":
    discipline_check()
    write_md(sys.argv[1])
    print("entries:", len(E), "pairs:", sum(len(p) for _, _, p, _, _ in E))
