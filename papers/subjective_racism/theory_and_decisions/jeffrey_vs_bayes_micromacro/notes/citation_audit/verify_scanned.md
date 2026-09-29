# Verification of project claims against scanned papers: Döring 1999, Domotor 1980, Good & Mittal 1987

Repo: `/home/anuragr/development/git/research/papers/subjective_racism/theory_and_decisions/jeffrey_vs_bayes_micromacro` (read-only; nothing edited).
Paths below are relative to the repo root. The grep covered PAPER_B_MANUSCRIPT.tex, bibliography.bib, notes/*.md, notes/*.tex, literature/**/README.md, literature/measurement_susceptibility_survey.md, and lean/**/*.lean. There were no hits in lean/, literature/README.md or measurement_susceptibility_survey.md.

## File identification (read this first)

| File | Pages | What it actually is |
|---|---|---|
| doring.pdf | 11 | Döring, "Why Bayesian Psychology Is Incomplete", Phil. Sci. 66 (Proceedings), S379–S389. Complete. |
| domotor1980.pdf | 27 | Domotor, "Probability Kinematics and Representation of Belief Change", Phil. Sci. 47 (1980), 384–403. PDF p.4 and p.5 are the same page (journal p.387, scanned twice). PDF pp.22–27 are blank. Journal pp.384–403 are complete. |
| goodmittal1987.pdf | 13 | **NOT Good & Mittal.** This is I. J. Good (1960), "Weight of Evidence, Corroboration, Explanatory Power, Information and the Utility of Experiments", JRSS B 22(2), pp.319–331. The file is mislabelled. |
| goodmittal1987_1.pdf | 18 | Good & Mittal, "The Amalgamation and Geometry of Two-by-Two Contingency Tables", Ann. Statist., pp.694–711, complete with the appendix and references. This is the real paper. |

---

## 1. Döring (1999)

(a) I read all 11 pages (S379–S389).

Notes on the paper:
- S379: The abstract and introduction frame the paper as "an exercise in Bayesian **rational** psychology". Its contention is that Jeffrey conditionalization "cannot be a complete account of **rational** belief change".
- S380: Field's "remedy... is ineffective". The order effect "is serious enough to call for an occasional adjustment of beliefs that is not conditionalization on the evidence."
- S382–S383: The worked example uses a prior over the 2x2 {A,¬A}×{B,¬B}: p(AB)=p(¬AB)=.05 and p(A¬B)=p(¬A¬B)=.45. Sequence 1 raises A∨B to .99 (partition {A∨B, ¬A¬B}) and then ¬A∨B to .99 (partition {¬A∨B, A¬B}). Sequence 2 applies them in reverse. The cues are **disjunctive two-cell partitions, each singling out one joint cell. They are not attribute-local.** Figure 1 shows 2x2 tables. P(A|¬B) ends at 1/6 in one order and 5/6 in the other. S383: "imagine a third updating step in which the probability of B is lowered close to 0. This would force the *un*conditional probabilities for A and ¬A near 0 and 1, with the roles of A and ¬A reversed". "This seems wholly unjustified if there is nothing essential about the order". The plane-crash illustration follows: A/¬A = left/right half, B/¬B = front/rear.
- S384: Skyrms's embedding is cited as "(cf. Jeffrey 1988)". The proposed remedy is to "revert to the original probabilities and try to assimilate the later evidence all at once".
- S385: "Jeffrey conditionalizing in one step on the new assignments yields a posterior distribution in which both pieces of evidence receive equal weight" (Figure 2: 49/49/1/1). The remedy is itself a single application of Jeffrey's rule to the original prior. Dempster's rule is an alternative.
- S386: "Jeffrey conditionalization offers no solution. Consequently, Jeffrey conditionalization alone cannot be all there is to rational belief change." §4 covers Field's proposal.
- S387–S388: Döring criticises Field, arguing that informativeness is relative to beliefs. Figure 3 shows a dampened order effect. S388: "I conclude that Field has neither shown that Jeffrey conditionalization is inapplicable nor that his own scheme is a viable alternative." §5 argues that incremental updating is the limitation.
- S389: The paper calls for non-incremental schemes. The references include Jeffrey (1988), "Conditioning, Kinematics, and Exchangeability", in Skyrms & Harper (eds.), *Causation, Chance, and Credence*, vol. 1, 221–255.

(b) Claims

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| D1 | Abstract quote: "Jeffrey conditionalization is sensitive to the order in which the evidence arrives. This order effect can be so pronounced as to call for a belief adjustment that cannot be understood as an assimilation of incoming evidence by Jeffrey's rule." (S379) | notes/papers_dialectic.tex:34–41 | VERIFIED | S379 abstract, sentences 2–3, word for word. |
| D2 | "Field has neither shown that Jeffrey conditionalization is inapplicable nor that his own scheme is a viable alternative" [S388] | notes/papers_dialectic.tex:84–85 | VERIFIED | S388: "I conclude that Field has neither shown that Jeffrey conditionalization is inapplicable nor that his own scheme is a viable alternative." The excerpt is exact. |
| D3 | "even the attacker rejects the escape" (Field's reparametrisation) | notes/papers_dialectic.tex:83–84 | VERIFIED | §4, S386–S388. Döring's reason differs from the project's: he argues informativeness is belief-relative (S387), whereas the project says an impression gives no counterfactual. The note does not attribute the project's reason to Döring. |
| D4 | Döring "argues that the order effect can be pronounced enough to call for an adjustment that Jeffrey's rule cannot supply" | notes/manuscript_change_plan.md:686–687 (planned 3.A text; not yet in the manuscript) | VERIFIED-WITH-CAVEAT | The paper says an adjustment that "cannot be understood as an assimilation of incoming evidence by Jeffrey's rule" (S379) and one that "is not conditionalization on the evidence" (S380). But his own remedy is carried out by Jeffrey's rule: revert to the original prior, then "Jeffrey conditionalizing in one step" on the merged partition (S384–S385). What Jeffrey's rule cannot supply is the *incremental* look-back (§5, S388–S389). "cannot supply" overstates this; "cannot supply incrementally", or the original "cannot be understood as an assimilation of incoming evidence by", is accurate. |
| D5 | Plan's verification status: D\"oring's "cannot be understood as an assimilation" is quoted from the primary text | notes/manuscript_change_plan.md:791–794 | VERIFIED | S379. |
| D6 | "All five papers read in full and formalized in `literature/`" (Döring is among the 3.A papers) | notes/manuscript_change_plan.md:791 | UNSUPPORTED | There is no `literature/doring*` directory. The literature/ folder has field1978, garber1980, hawthorne2004, wagner2002 and others, but no Döring. notes/papers_dialectic.tex:202 is accurate by contrast: "Read in full from the scanned original". Related: notes/verification_coverage.md:61–62 says literature claims about Döring are "sourced by direct reading; see `literature/`", but there is nothing on Döring in literature/. |
| D7 | "His own worked example displays the order effect in the cells of a 2x2 joint over two attributes" | notes/papers_dialectic.tex:155–156 | VERIFIED-WITH-CAVEAT | True for Figures 1 and 3 (S383, S388), which are 2x2 tables over A and B. Caveat: the cues are not attribute-marginal (see D9). |
| D8 | "cells are exactly what an observer of a population does not get to read, and the step from his tables to observable statistics is the step this paper supplies" | notes/papers_dialectic.tex:156–158 | VERIFIED-WITH-CAVEAT (bordering CONTRADICTED) | Döring himself moves off the cells. He reads the effect as P(A given ¬B) = 1/6 vs 5/6. He also argues explicitly that a third update lowering B "would force the *un*conditional probabilities for A and ¬A near 0 and 1, with the roles of A and ¬A reversed in the two sequences" (S383), which is a marginal-level effect. The Figure 1 marginals of A already differ (48 vs 52%). What he does not supply is any *population/observer* statistic, since his example is a single agent (investigators). The claim should be narrowed to that. |
| D9 | "Döring's disjunctive-cue counterexample" vs Hawthorne's attribute-local cues; "the locality question Döring's example raises" | literature/hawthorne2004/README.md:97–101 | VERIFIED | S382: the cues raise P(A∨B) to .99 (partition {A∨B, ¬A¬B}) and then P(¬A∨B) to .99 (partition {¬A∨B, A¬B}). Each cue is a two-cell partition that isolates one joint cell and mixes both attributes, so it is not attribute-local. Döring does not discuss locality himself; "raises" is the project's reading, and a fair one. |
| D10 | Jeffrey (1988) "appears in Döring's own reference list" | notes/papers_dialectic.tex:219–221 | VERIFIED | S389 references list "———. (1988), 'Conditioning, Kinematics, and Exchangeability', in Brian Skyrms and William L Harper (eds.), Causation, Chance, and Credence, vol. 1. Dordrecht: Kluwer, 221–255". Döring cites it at S384 for Skyrms's embedding of Jeffrey updating in classical conditionalization, not for a commutativity result. The note claims only that it appears in the list, so this is fine. |
| D11 | Bibliographic details: Phil. Sci. 66 (Proceedings), S379–S389 (bibitem). The plan bibtex has volume 66, number {Supplement}, pages S379--S389, title "Why {B}ayesian Psychology Is Incomplete" | notes/papers_dialectic.tex:200–203; notes/manuscript_change_plan.md:1418–1426 | VERIFIED | S379 footer: "Philosophy of Science, 66 (Proceedings) pp. S379–S389. 0031-8248/99/66supp-0029$0.00. Copyright 1999". Status note: `Doring1999` is **not** in bibliography.bib, and the manuscript currently does not cite it, so nothing is broken yet. The entry must be added when change 3.A is applied. |
| D12 | The case against sequential Jeffrey updating is a psychological claim ("The attack says the model is unrealistic. The attack is about psychology."), with Döring's title as its "sharpest statement" | notes/papers_dialectic.tex:32–34, 54–56 | CONTRADICTED (as applied to Döring) | Döring's claim is normative. S379: "an exercise in Bayesian **rational** psychology... cannot be a complete account of **rational** belief change". S383: the order dependence "seems wholly unjustified". S386: "Jeffrey conditionalization alone cannot be all there is to rational belief change". He never claims people do not update this way. The realism framing fits Hawthorne (p.115, not checked here) but not Döring. |
| D13 | "Döring's attack" (Döring as the attack in the standoff); ranking "Doring (attack)" | notes/positioning_economics.tex:26; notes/paper_review_log.md:49 | VERIFIED | Döring attacks the completeness of Jeffrey conditionalization (S379–S380, S386). The same normative vs. psychological caveat as D12 applies. |
| D14 | Döring does not ask how far two reading sequences disagree in each statistic (novelty claim) | notes/manuscript_change_plan.md:138–141 | VERIFIED | The paper contains no statistic-by-statistic comparison and no population. |
| D15 | Hawthorne note 15 cites Döring "for its implausibility" | notes/papers_dialectic.tex:102–103 | NOT-CHECKABLE | This is a claim about Hawthorne (2004), which is not among my papers. |

(c) Most important problems for Döring
1. **D12**: the project presents Döring's attack as a *psychological realism* claim. Döring's argument is explicitly about *rational* (normative) belief change. The "psychology" in his title is "Bayesian rational psychology" (S379). A referee who knows the paper would object to this.
2. **D8**: "cells are what an observer does not read; the step to observable statistics is the step this paper supplies" understates Döring. At S383 he extends the effect to conditional probabilities and to unconditional marginals (P(A) near 0 vs near 1 after a third step). The claim is safe only if narrowed to *population* statistics.
3. **D4**: "an adjustment that Jeffrey's rule cannot supply" is inaccurate. His remedy (S384–S385) *is* a one-step Jeffrey update applied to the original prior. The shortfall is incremental assimilation.
4. **D6**: "formalized in literature/" is false for Döring. No literature folder exists for him, and verification_coverage.md points to literature/ for him as well.
5. D7/D9: the worked example is 2x2, but its cues are disjunctive and not attribute-local. This matters wherever Döring is offered as an instance of the manuscript's attribute-local setting. The Hawthorne README handles this correctly.

---

## 2. Domotor (1980)

(a) I read all 27 PDF pages: journal pp.384–403, plus one duplicated page (387) and six blank trailing pages.

Notes on the paper:
- p.384: The abstract says Bayesian, Jeffrey and Field conditionals are compared, and it is shown why the last two cannot be reduced to the first. The footer reads "Philosophy of Science, 47 (1980) pp. 384–403."
- p.385: Machines (P, E, *) with the laws triviality and composition.
- p.386: "The key problem here concerns the lack of composition in Jeffrey's input space... if the domain is extended as a remedy, composition will not be commutative". "Field (1978), dissatisfied with the lack of commutativity in a Jeffrey machine, suggests a different input space".
- pp.387–389: Dominance, orthogonality, convex and propositional mixing, and partitions.
- pp.392–393: The Jeffrey formula (5) P'(H)=Σ p(A)P_A(H), and the Jeffrey machine on strings of partition-probability pairs.
- p.394: Minimal revision (a)–(d): expectation, max relative entropy, dP'/dP = dp/dP|_D, and transition probability.
- p.395: "Along with Field (1978) we may argue that Jeffrey machines are inadequate because, among other things, the commutativity of evidence application: [P_(U,p)]_(V,q) = [P_(V,q)]_(U,p) fails in general."
- p.396: Properties of the generalized conditional (zeros cannot be revised; orthogonality; irrelevance), and the Field machine.
- p.397: The Field conditional is commutative. The embedding h_P: F_X → E_X, (U,α) ↦ (U,p) with **p(A) = e^{α_A} P(A) : α_X**, depends on the current state P. "Field's F_X in fact covers only what is probabilistically independent in Jeffrey's E_X... whatever advantage may have been gained in commutativity is lost in probabilistic independence."
- pp.398–400: The impossibility of reducing generalized conditionals to Bayesian ones. p.399: "in a Jeffrey machine we have a noncommutative transition from P to P_(A,a) and then to [P_(A,a)]_(B,b)".
- pp.400–403: Maximum relative entropy, Field derived two ways, Martin-Löf, and the references.
- The paper never uses the words "likelihood" or "marginal", and never mentions reading a delivered credence against the marginal in force.

(b) Claims

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| M1 | Bare content: sequence-dependence under Jeffrey updating, cited to Domotor | PAPER_B_MANUSCRIPT.tex:120 | VERIFIED | p.395: commutativity of evidence application "fails in general". p.399: "in a Jeffrey machine we have a noncommutative transition". p.386: "lack of commutativity in a Jeffrey machine". |
| M2 | Mechanism: "since the likelihood a delivered credence implies must be read against the marginal in force when the cue arrives \citep{Domotor1980}" | PAPER_B_MANUSCRIPT.tex:120 | UNSUPPORTED | Domotor gives no such explanation and never speaks of likelihoods or marginals. The closest passage is p.397, the embedding of Field inputs into Jeffrey inputs, p(A) = e^{α_A}P(A):α_X. This shows that the posterior corresponding to a fixed factor α depends on the current state P, which is the converse of the claimed reading, and Domotor does not present it as the source of non-commutativity. He attributes non-commutativity to the structure of the Jeffrey input space (lack of composition; strings of pairs, pp.386, 393). This is really the Field/Wagner Bayes-factor point; cite Field 1978 or Wagner 2002 for it, or rephrase so Domotor is cited only for non-commutativity. |
| M3 | "follows easily" / framing | PAPER_B_MANUSCRIPT.tex:120 | VERIFIED-WITH-CAVEAT | Domotor treats non-commutativity as a reason Jeffrey machines are "inadequate" ("Along with Field (1978)...", p.395), whereas the manuscript treats sequence effects as the coherent response. This is not a misquote, but the cited source leans the other way. |
| M4 | Domotor does not ask how far two reading sequences disagree in each statistic | notes/manuscript_change_plan.md:138–141 | VERIFIED | There is no such analysis in pp.384–403. |
| M5 | Bib: Domotor, Zoltan; 1980; "Probability Kinematics and Representation of Belief Change"; Philosophy of Science; vol 47; pages 384--403 | bibliography.bib:86–93 | VERIFIED | p.384 title and footer "Philosophy of Science, 47 (1980) pp. 384–403". The issue number is absent from both the bib entry and the scan (acceptable). The name is printed "ZOLTAN DOMOTOR" without an accent, so the bib spelling matches. |
| M6 | Listed as "retained" / "read directly" | notes/paper_review_log.md:671, 830; notes/manuscript_change_plan.md:140, 1394 | VERIFIED | These are status notes only. The bib key exists. |

(c) Most important problem for Domotor
- **M2**: the manuscript's only substantive use of Domotor attaches a mechanism ("likelihood... read against the marginal in force when the cue arrives") that the paper does not state. Domotor supports only the bare non-commutativity of Jeffrey updating (pp.386, 395, 399), and he frames it as a defect.

---

## 3. Good & Mittal (1987)

(a) I read all 18 pages of goodmittal1987_1.pdf (pp.694–711) and all 13 pages of goodmittal1987.pdf. The second file turned out to be Good (1960), JRSS B, pp.319–331, which is unrelated to the amalgamation claim.

Notes on the real paper:
- p.694 abstract: "If a pair of two-by-two contingency tables are amalgamated by addition it can happen that a measure of association for the amalgamated table lies outside the interval between the association measures of the individual tables. We call this the amalgamation paradox and we show how it can be avoided by suitable designs of the sampling experiments." The paper was received July 1985 and revised June 1986. The page shows no volume or issue number.
- p.695, **Definition 1.1**: "the amalgamation (or aggregation) paradox... occurs if max_i α(a_i) < α(A) or α(A) < min_i α(a_i)." The Yule (1903) form is: "α(a_i)=0 (or 1) for all i, but α(A)≠0", meaning association **appears** in the aggregate when there is none in any subpopulation. The "stronger form" (sign reversal) comes via Cohen–Nagel and Simpson (1951). Blyth (1972) called it Simpson's paradox. G&M reject "reversal paradox" because "in Yule's formulation the subpopulations each had zero association in which case there is no reversal of sign. Hence we prefer the name 'amalgamation paradox.'"
- pp.695–696: "a drug can be judged to be beneficial... for both men and women considered separately, but can seem to be harmful for the population at large... **This can happen even though N_i ∝ p_i.** We claim that such a situation can arise only if not enough care is used in the design of the experiment."
- p.696: Def. 1.2 (homogeneity), Def. 2.1 (row-uniform: (a_i+b_i)/(c_i+d_i)=λ for all i), and column-uniform (2.2).
- pp.698–700: The measures π_R, π_C, Yule's y, κ, W_R, W_C, Q_R, Q_C.
- pp.701–703: Under row-uniform designs α(A) = Σ(N_i/N)α(a_i) for π_R and y (Thm 4.1), and the paradox cannot occur. Thm 4.2 covers Q, W. For κ, Thm 4.3 needs *both* row- and column-uniformity (counterexample on p.702).
- pp.703–707: Homogeneity theorems 5.1–5.6. Appendix (pp.707–710): approximately row-fair designs. p.711: references.

(b) Claims

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| G1 | The amalgamation paradox is where "a real effect present in every subpopulation is erased or reversed" | PAPER_B_MANUSCRIPT.tex:285–286 | VERIFIED-WITH-CAVEAT | This is narrower than Definition 1.1 (p.695), under which the aggregate lies *outside the interval* of the subpopulation measures in either direction. That covers amplification above the max, and it covers the Yule case where **no** effect exists in any subpopulation and one *appears* in the aggregate. G&M chose the name "amalgamation" precisely because of that null-to-nonnull case (p.695). "Erased" (aggregate 0 while all subpopulations are >0) and "reversed" are both special cases of α(A) < min α(a_i), so the description is not wrong, but it omits the case G&M treat as primary. |
| G2 | "...by a confound in how the subpopulations are weighted together" / "combining subpopulations under confounded weights" | PAPER_B_MANUSCRIPT.tex:285–286 | VERIFIED-WITH-CAVEAT | G&M never use "confound". They locate the cause in the sampling design, namely non-uniform row (or column) ratios across subpopulations (Defs 2.1–2.2; Thms 4.1–4.3). They explicitly say the paradox "can happen even though N_i ∝ p_i" (p.696), so weighting subpopulations by their population shares does not prevent it. When the design is row-uniform, the aggregate *is* the N_i/N-weighted average (4.1). The problem is imbalance of the treatment margin across subpopulations, which makes the two rows weighted differently (Lemma 4.1 convex weights), not the weights on the subpopulations themselves. "Confounded weights" is defensible only as a loose Simpson-style gloss. Better wording: "by an imbalance of the treatment (row) margin across subpopulations". |
| G3 | Bib: Good, I. J. and Mittal, Y.; title; Annals of Statistics; 1987; vol 15; no 2; pp 694--711; doi 10.1214/aos/1176350369 | bibliography.bib:208–217 | VERIFIED (authors, title, journal, pages); NOT-CHECKABLE (volume, number, year, DOI) | Scan p.694 shows the title and authors exactly (I. J. Good, Y. Mittal, Virginia Polytechnic Institute and State University), first page 694, and last page (references) 711. The running head shows no volume, issue, year or DOI. The received/revised dates (1985/1986) are consistent with 1987. |
| G4 | The local file `goodmittal1987.pdf` is the Good–Mittal paper | papers dir naming (not a repo claim) | CONTRADICTED | It is I. J. Good (1960), "Weight of Evidence, Corroboration, Explanatory Power, Information and the Utility of Experiments", JRSS B 22(2), 319–331. Anyone verifying G1–G3 from that file would be checking the wrong paper. |

(c) Most important problems for Good–Mittal
1. **G2**: "a confound in how the subpopulations are weighted together" misplaces the mechanism. G&M stress the paradox arises even with population-proportional weights (p.696). It comes from row- or column-margin non-uniformity across subpopulations.
2. **G1**: "a real effect present in every subpopulation is erased or reversed" drops the Yule case (no effect in any subpopulation, effect in the aggregate) that G&M name the paradox after, and it drops amplification beyond the max.
3. **G4**: goodmittal1987.pdf is a different Good paper. Rename it or remove it from the source folder.

---

## Summary of counts

| Verdict | Count | Items |
|---|---|---|
| VERIFIED | 14 | D1, D2, D3, D5, D9, D10, D11, D13, D14, M1, M4, M5, M6, G3 (bibliographic core) |
| VERIFIED-WITH-CAVEAT | 6 | D4, D7, D8, M3, G1, G2 |
| UNSUPPORTED | 2 | D6, M2 |
| CONTRADICTED | 2 | D12, G4 (file identity) |
| NOT-CHECKABLE | 2 | D15, G3 (volume/issue/year/DOI) |
| MISQUOTED / WRONG-LOCATION | 0 | |
