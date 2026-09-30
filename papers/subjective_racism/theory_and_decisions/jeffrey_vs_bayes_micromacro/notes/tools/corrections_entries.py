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


def cut(start, end):
    i = NORM.find(start)
    if i < 0 or NORM.find(start, i + 1) >= 0:
        sys.exit(f"start phrase missing or ambiguous: {start!r}")
    j = NORM.find(end, i)
    if j < 0:
        sys.exit(f"end phrase missing: {end!r}")
    return NORM[i:j + len(end)]


def quote(s):
    return "\n".join("> " + l for l in textwrap.wrap(s, 78, break_long_words=False, break_on_hyphens=False))


def block(s):
    return "```latex\n" + textwrap.fill(" ".join(s.split()), 80, break_long_words=False, break_on_hyphens=False) + "\n```"


E = []  # (id, title, [(start, end, after)], purpose, verification)

E.append(("C.1", "Abstract, sentences 2-4", [(
    "Considering a case where evaluators", "the share of the population affected by the sequence differing from the benchmark.",
    r"""Considering evaluators who read two correlated soft cues and comparing their
beliefs and decisions with a sequence-free Bayesian benchmark, the paper asks which
statistics of those beliefs and decisions register the resulting order effect. The
two reading sequences displace a belief only within a two-dimensional plane, so
whether a statistic registers the effect is decided by the statistic alone. The
believed association between the attributes and the average value lost through
decisions that differ from the benchmark stay within second order of the
benchmark, while the marginal probabilities and the share of the population whose
decision the sequence changes depart from it at first order.""")],
    "\"An arbitrary statistic is an order of magnitude closer to the benchmark\" is false as "
    "stated; only the protected statistics are (Proposition PRO; plan 0.A made the same point). "
    "Also \"the current papers asks\", and \"This results in measures ... staying ... while the "
    "share ... differing\" has no main verb. \"Considering the empirical limitations in addition "
    "to theoretical underpinnings\" is cut because no empirical limitation is named.",
    "DEC and LOS (second order), DRF and SHR (first order), PRO (the plane and the "
    "classification). All in the Lean and sympy suites."))

E.append(("C.2", "Introduction, paragraph 1, sentences 2 and 4", [
    ("The sequence-dependence of Jeffrey conditioning for soft evidence", "remains unclear \\citep{ZhaoOsherson2010}.",
     r"""The sequence dependence of Jeffrey conditioning on soft evidence has attracted
debate \citep{DiaconisZabell1982,Hawthorne2004,ZhaoOsherson2010}, but how that
dependence registers in the statistics an observer reads has received little
attention."""),
    ("It finds that the share of population affected", "and the total loss due to sequence effects do not.",
     r"""It finds that the share of the population whose decision the sequence changes
departs from a sequence-invariant benchmark at first order, while the believed
association between attributes and the surplus-weighted loss depart from it only at
second order.""")],
    "Zhao-Osherson do not say the mechanism \"remains unclear\"; they say doubts about order "
    "dependence \"disappear on closer inspection of the evidential weight of probability "
    "judgements\" (p. 291). Cited for the debate, as the author agreed. Jeffrey himself says "
    "non-commutativity \"is as it should be\" (1983, pp. 182-183; audit J1) and Diaconis-Zabell "
    "that it \"is not a real problem\" (Remark 2, p. 827; M24); Hawthorne calls the order effect "
    "\"very troubling\" (p. 99). Sentence 4: the loss is not free of the sequence effect, it "
    "carries it at second order (LOS), and \"-- but\" is a dash doing a sentence's work.",
    "verify_jeffrey.md J1; verify_kinematics.md; verify_hawthorne_weisberg.md; ZO p. 291 read "
    "2026-09-30; Theorem LOS, Proposition SHR."))

E.append(("C.3", "Introduction, paragraph 2 (the literature paragraph) and its footnote", [(
    "The discussion on Jeffrey\\citep{Jeffrey1983}", "cannot tell whether impressions replace prior belief or adjust it.",
    r"""Discussion of how Jeffrey conditioning \citep{Jeffrey1983} depends on arrival
sequence has more often asked whether the commutation criterion is exactly
satisfied than how measurable a failure of it is in observed data. The
belief-adjustment literature models an impression as a single score
\citep{HogarthEinhorn1992}, and the founding multi-trait study reports its results
trait by trait \citep{Asch1946}, so both read marginals only. The characterisation
of when two sequences lead to the same belief does consider a joint belief over
attributes \citep{DiaconisZabell1982}, but where order dependence has been treated
as a problem it has been treated as a defect to repair\footnote{\citet{Hawthorne2004}
offers factor-based alternatives in which a cue supplies a normed-likelihood or
likelihood-ratio factor instead of a new probability. Under these, updates on
distinct attributes commute, and in his Basis-Commuting Version all updates do.}
rather than as something to measure. Replacing a credence that resets the prior
marginal by a factor that multiplies it settles whether beliefs depend on
sequence, but it leaves open the empirical question of whether an impression
replaces a prior belief or adjusts it.""")],
    "(i) Asch did not model an impression as a single score; Experiment VI reports eighteen "
    "traits under two orders (audit A28). His published tables give per-trait percentages, "
    "i.e. marginals. (ii) Diaconis-Zabell are grouped with the defect view, but they hold that "
    "non-commutativity \"is not a real problem\" (M24). (iii) The footnote says Hawthorne "
    "\"repairs\" the defect and that the effect disappears; he offers alternatives, and order-"
    "freedom holds across distinct bases, fully only in his Basis-Commuting Version (M19). "
    "(iv) \\citet inside a sentence where \\citep is meant; \"have focused\" agreement.",
    "Asch1946 record (Asch.lean, Table 7); DiaconisZabell.lean and README; Hawthorne.lean "
    "(`extUpdate_basisCommuting`, `factorUpdate_comm`), Sections 6-8."))

E.append(("C.4", "Introduction, the two-channel paragraphs (appear twice)", [(
    "More specifically, arrival-sequence dependence can enter through two channels, an \\textbf{association} channel, in which one cue changes what the other implies and which exists only when the attributes are believed related, and a \\textbf{position} channel, in which the observer weights the later cue less whatever the attributes are \\citep{HogarthEinhorn1992,Asch1946}. Unlike",
    "the believed association and related statistics carry it only at second order.",
    r"""More specifically, arrival-sequence dependence can enter through two channels.
In the \textbf{association} channel one cue changes what the other implies, and the
channel exists only when the attributes are believed related. In the
\textbf{position} channel the weight a cue receives depends on where in the sequence
it arrives, whatever the attributes are. Anchoring on the first cue can favour the
earlier impression and step-by-step partial adjustment favours the later one
\citep{HogarthEinhorn1992}. In the model studied here, which channels operate is
fixed by how fully the later cue is adopted. When it is adopted only in part, the
position channel operates and every marginal registers the sequence even when the
attributes are believed unrelated. When each impression is adopted in full, as in
the successive updating of \citet{DiaconisZabell1982}, the position channel is
absent and the association channel is the only one, so the sequence registers
unevenly across statistics. That full-adoption setting is the one the current paper
explores. A belief in the two-attribute setting has three coordinates, the two
marginal probabilities and the cross-attribute association, and a departure from
the sequence-free benchmark smaller than the observer's precision is invisible in
the statistic read. The question the paper addresses is therefore not only whether
the two sequences agree but by how much they disagree in a given statistic. Its
answer is that the marginal probabilities and the decisions made from them carry
the difference at first order in the prior covariance, while the believed
association and the statistics that move with it carry it only at second order.""")],
    "(i) The paragraph appears twice; the first copy is an earlier version and lacks its final "
    "period. The AFTER replaces both copies. (ii) \"the observer weights the later cue less\" is "
    "cited to Hogarth-Einhorn and Asch, but Hogarth-Einhorn's step-by-step partial adjustment "
    "predicts recency (Appendix B) and their primacy comes from anchoring on the first item "
    "(End-of-Sequence, Eq. 8); Asch denies that \"sheer temporal position\" matters (p. 272). "
    "Asch is dropped (audit P10-P12). (iii) \"Which channels operate is fixed by how fully the "
    "later cue is adopted\" is true of this model but not in general (under base-rate neglect "
    "the later cue is adopted in full and a position channel still operates), so it is scoped "
    "to \"the model studied here\". (iv) Full adoption is Diaconis-Zabell's setting, not their "
    "premise. Optional, if a published instance of a position channel with likelihood inputs "
    "is wanted: add after the Hogarth-Einhorn sentence \"Under base-rate neglect, where each "
    "posterior becomes the prior for the next cue, the earlier cue is discounted "
    "\\citep{BenjaminBodohCreedRabin2019}.\" (needs the bib entry in C.12).",
    "HogarthEinhorn.lean (`appB_recency`, `eq8_estimation_first_dominates`, "
    "`oneSided_primacy_iff`); BenjaminBodohCreedRabin.lean; verify_interior_omega.py rows 1-2; "
    "Proposition ADJ endpoints (Anchoring.lean)."))

E.append(("C.5", "Introduction, hiring-panel paragraph, last sentence", [(
    "In demonstrating how the sequence-dependence of certain statistics can be invisible",
    "(the amnestic updating concern in the literature).",
    r"""In the panel example the odds ratio between the two traits is the same in both
reading sequences however far a later impression erases the earlier one, since each
update rescales rows or columns of the belief (Lemma~\ref{lem:SEP}), whereas the
cross-product association is protected in that way only under full adoption
(Section~\ref{sec:scope}).""")],
    "False as written for the cross-product association, which is the paper's `assoc`. Under "
    "partial adoption its sequence effect is first order in c, with coefficient (1-omega) times "
    "a nonzero term, and the pooled association is nonzero already at c = 0. Only the odds "
    "ratio is identical across sequences for every omega. The commented-out line below the "
    "paragraph states the odds-ratio version. The AFTER points to Section 6, where plan 6.B "
    "carries the interior-omega numbers; apply 6.B with it.",
    "sympy/verify_interior_omega.py checks (2), (3), (4); Lemma SEP (LemmaSEP.lean, general N)."))

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

E.append(("C.9", "Setup 2.1, the prior paragraph and the sequence/weight sentences", [
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
the"""),
    ("To address the issues in amnestic updating debate, we also consider an adoption weight",
     "the credential's implication is never moved.",
     r"""To address the debate over whether a later impression should overwrite what an
earlier one implied \citep{Hawthorne2004}, we also consider an adoption weight
$\omega\in[0,1]$, which denotes how far the letter displaces what the credential
had already implied about trustworthiness. At $\omega=1$ the letter sets the rating
outright, and at $\omega=0$ the credential's implication is never moved.""")],
    "Grammar (\"described as $\\Delta^3$ --\", \"denotes\" for two subjects). \"Amnestic\" is "
    "used only where Hawthorne is quoted (writing_discipline.md 6). **Dependency:** omega is "
    "defined here but used nowhere else in the manuscript until plan 5.B (Proposition ADJ) is "
    "applied; either apply 5.B or move this sentence into 5.B. The Frechet-Hoeffding bounds "
    "in the new text are correct (checked cell by cell).",
    "Bounds: nonnegativity of the four cells of P; Hawthorne pp. 98-99."))

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
     r"""A related normative literature evaluates group belief formation by whether
aggregation commutes with updating, a criterion that geometric pooling meets and
linear pooling generally does not \citep[Theorem~2]{Dietrich2021}. The criterion
does not discriminate here. Evaluators who share a prior and differ only in reading
sequence do not hold conditionalisations of that prior on a common event, and the
linear and geometric averages of their posteriors differ only at $\bigO(c^{2})$. The
first-order divergence of every statistic outside the protected class of
Proposition~\ref{prop:PRO} is therefore a property of Jeffrey updating, not of
linear pooling."""),
    ("\\textbf{Several alternatives to Bayesian updating", "Divisibility axioms jointly force.}",
     r"""Decision theory has axiomatised several alternatives to Bayesian updating, each
for a single piece of news. \citet{Epstein2006} makes the updating rule, and not only
the prior, subjective, and \citet{Ortoleva2012} axiomatises departures triggered by
unexpected news. \citet{Cripps2021} treats order, and his Symmetry and Divisibility
axioms imply that two conditionally independent signals give the same posterior in
either order. Read with each cue as its likelihood matched to the prior
(Proposition~\ref{prop:IMM}), the composite of Proposition~\ref{prop:DIV} is a
sequence of Bayes updates satisfying all four of his axioms, and it depends on the
sequence because the second likelihood is matched to the intermediate belief, so
the two sequences process different experiments."""),
    ("While the literature restores sequence-invariance for sequential Jeffrey updating", "\\citep{PettigrewWeisberg2025},",
     r"""While \citet{PettigrewWeisberg2025} restore sequence-invariance by pooling the
prior with each new input multiplicatively before the Jeffrey step,"""),
    ("The paper borrows the evaluator-with-binary-attributes frame", "\\citep{Phelps1972, Arrow1973}.",
     r"""The paper borrows its evaluator of an uncertain binary attribute from the
statistical-discrimination lineage \citep{Arrow1973,CoateLoury1993}, in which
\citet{Phelps1972} treats the attribute as continuous."""),
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
second-order loss in aggregate (Theorem~\ref{thm:LOS}).""")],
    "One entry per sentence group, in manuscript order. BHW/Banerjee: \"coarse\" belongs to BHW's "
    "binary action, not Banerjee's continuum; the two share one mechanism, so \"on the other hand\" "
    "goes; the marginals point is the paper's analogy (M2, M3). Dietrich: Def. 1 is preference "
    "aggregation, and with a common prior linear pooling passes the criterion wherever it applies "
    "(M4, M5). Epstein/Ortoleva/Cripps (bold sentence): the framing is unsupported for Epstein "
    "and Ortoleva; the Cripps claim is false, since under Proposition IMM's reading the "
    "composite satisfies all four axioms, and the footnote goes with it (M6-M9). "
    "Pettigrew-Weisberg pool the prior with each input, not successive inputs (M25). Phelps has "
    "no binary attribute (M10). BCGS name one mechanism, not two, and \"sampling\" is not theirs "
    "(M12). Becker: the concluding section never draws the distinction, and Becker's own "
    "mechanism is averaging plus a budget constraint, the opposite side of it (\"Our statement "
    "goes beyond arithmetic\", p. 7; M13); the AFTER deletes the sentence, and an alternative "
    "that keeps Becker is in literature/becker1962/README.md. Good-Mittal: their paradox covers "
    "amplification and effects created from none, and its cause is uneven allocation, not "
    "weighting by population shares (M14).",
    "BHW.lean, Banerjee.lean, Dietrich.lean, Epstein.lean, Ortoleva.lean (via the 2024 survey), "
    "Cripps.lean (`order_invariance`, `composite_AB_eq_bayes`), PettigrewWeisberg.lean, "
    "Phelps.lean, Arrow.lean, CoateLoury.lean, BCGS.lean, Becker.lean, GoodMittal.lean "
    "(`piR_amalg_general`, `equalSize_reversal`)."))

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
out.append("""## C -- Corrections to the author's draft of 2026-09-30 (audit-driven)

These entries correct the manuscript as committed at e7b997e3 (the author's
revised draft). Each BEFORE block is cut from that file by script, so it matches
the manuscript text exactly (whitespace normalised). Each AFTER block is
paste-ready LaTeX written under `notes/writing_discipline.md` (no colons, both
sides of each contrast named, rewrites within 80-120% of the draft's length;
exceptions are deletions, the removal of the duplicated paragraph in C.4, citation
fixes on short phrases, and C.6, which replaces a refuted argument). Each entry names
the audit item in `notes/citation_audit.md` and the Lean or sympy record that
justifies it. **None is applied.** The author approves each entry, and approved
entries are then applied and committed.

Entries C.1-C.14 are in manuscript order. C.12 lists the bibliography entries
the AFTER texts need. Section D lists what these corrections change in the
existing plan entries 0.A-6.B.
""")
for eid, title, pairs, purpose, verif in E:
    out.append(f"### {eid} -- {title}\n")
    out.append(f"**Why.** {purpose}\n")
    for k, (s, e, after) in enumerate(pairs, 1):
        before = cut(s, e)
        tag = f" ({k} of {len(pairs)})" if len(pairs) > 1 else ""
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


def write_md(path):
    open(path, "w").write(MD)



if __name__ == "__main__":
    write_md(sys.argv[1])
    print("entries:", len(E), "pairs:", sum(len(p) for _, _, p, _, _ in E))
