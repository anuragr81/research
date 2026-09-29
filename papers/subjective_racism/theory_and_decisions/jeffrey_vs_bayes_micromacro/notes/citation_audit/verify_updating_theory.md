# Verification: updating-theory / herding / aggregation sources (Paper B, Section 3)

Repo (read-only): `/home/anuragr/development/git/research/papers/subjective_racism/theory_and_decisions/jeffrey_vs_bayes_micromacro`
Papers dir: `.../scratchpad/papers`

## Where the project cites these papers (Pass 2)

`grep -n -i "Cripps|Dietrich|Bikhchandani|BHW|Banerjee|Ortoleva|Epstein|Hayashi|Thoma|cascade|herd|divisib|geometric pooling|linear pooling"` over PAPER_B_MANUSCRIPT.tex, bibliography.bib, notes/*.md, notes/*.tex and literature/**/README.md:

- **Manuscript:** all hits are in Section 3 "Related literature", PAPER_B_MANUSCRIPT.tex:255–277.
- **bibliography.bib:** Epstein2006 (43–53), Ortoleva2012 (54–63), Cripps2021 (95–100), Dietrich2021 (102–110), Banerjee1992 (112–121), BHW1992 (123–132).
- **notes/*.md, notes/*.tex, literature/**/README.md:** no claims about any of these papers. The only hits are "pooling" in the unrelated Pettigrew–Weisberg README and in notes/question_and_answer*.tex, plus "divisible by c^2" in lean/, which is unrelated.
- **Hayashi and Thoma:** not cited anywhere in the repo (no hit in .tex/.md/.bib). No project text relies on them.

Verdict key: VERIFIED, VERIFIED-WITH-CAVEAT, MISQUOTED, WRONG-LOCATION, UNSUPPORTED, CONTRADICTED, NOT-CHECKABLE.
Page numbers are the journal's or the manuscript's printed page numbers, not PDF page indices.

---

## 1. Cripps, M.W., "Divisible Updating" (cripps.pdf = CrippsBayessubmission2comb)

**(a)** I read all 39 PDF pages: main text pp.1–26, references pp.26–27, and the appendix proofs pp.27–39 (Lemma 1, Props 1–6, Lemma 2, Lemma 3). The version is dated "Originally 2019. This version November 4, 2021", UCL (p.1 footnote).

Facts that matter for the claims:
- **The four axioms (pp.7–9):**
  - Axiom 1 **Uninformativeness**
  - Axiom 2 **Symmetry**: permuting signal *labels* within one experiment permutes the updated-belief profile
  - Axiom 3 **Divisibility**: parts (a) consequentialism and (b) the two-step "s=1 vs s≠1, then residual experiment" decomposition
  - Axiom 4 **Non-Dogmatic**

  Axioms 5–8 (Continuity, Respects Certainty, Unbiased, No Learning without Evidence) are additional and are used only for Corollary 1, Result 1 and Props 4–5.
- **Main theorem (Prop. 1, p.11):** "The updating U satisfies the Axioms 1–4, if and only if, it is divisible", i.e. u(µ,p_s) = F⁻¹(F(µ)∘p_s / F(µ)ᵀp_s) for a bijection F ("shadow prior", Bayes, map back).
- **Order-independence is an informal remark, not a theorem (p.9):** "The divisibility axiom asserts that this change has no effect on the final profile of updated beliefs. Furthermore, symmetry implies that the order in which the signals are revealed can be changed without an effect on the ultimate beliefs. Hence, these axioms imply that reversing the order in which two signals arrive has no effect on the ultimate beliefs." See also p.2: "Hence, it ensures that order effects do not matter."
- **The objects are experiments:** an updating rule maps (µ, E_n), with E_n a finite experiment of state-dependent, full-support signal probabilities, to a profile of posteriors (p.7). The word "Jeffrey" never appears. Credence-type (soft) inputs are not in the domain.

**(b)**

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| C1 | Cripps has "four axioms" | tex:277 | VERIFIED | Abstract p.1: "characterized by four axioms"; Axioms 1–4 on pp.7–9; Prop. 1 p.11. |
| C2 | "Cripps2021 shows that symmetry and divisibility jointly force sequence-independence" | tex:276 | VERIFIED-WITH-CAVEAT | p.9 says so almost verbatim ("symmetry implies that the order in which the signals are revealed can be changed … these axioms imply that reversing the order in which two signals arrive has no effect"). **Caveats:** (i) it is a one-sentence remark after Axiom 3, not a proposition, so "shows" overstates it; (ii) the "order" is the order in which the outcome of **one experiment** is revealed through nested coarsenings (s=k vs s>k), plus pairs of experiments E^A/E^B where E^B is a coarsening of E^A (p.9). It is not the order of two different cues on different partitions; (iii) "Symmetry" is invariance to relabelling signals, not to reordering evidence. |
| C3 | "so the correlated two-cue composite of Prop. DIV fails divisibility" | tex:277 | UNSUPPORTED | Nothing in Cripps, and nothing in the repo (grep across .tex/.md/lean), shows this. Logically, p.9 only gives: Symmetry and Divisibility together imply order-invariance. By contraposition, an order-dependent rule fails **Divisibility or Symmetry**, *if it is in Cripps's domain at all*. The Jeffrey composite takes (prior, delivered marginal credences q, r), not an experiment E. The only bridge in the manuscript is the matched likelihood ℓ = q/P(A) (Prop. IMM, tex:309). That likelihood depends on the prior, so the Jeffrey map does not define a function U(µ,E). The footnote on the same line concedes this mismatch. |
| C4 | "…and, of the four axioms, only divisibility" | tex:277 | UNSUPPORTED | No derivation exists anywhere in the project that the Jeffrey composite satisfies Uninformativeness, Symmetry and Non-Dogmatic. These axioms are stated for experiments with signal probabilities (pp.7–9) and are undefined for credence inputs without a translation that the paper never gives. Non-Dogmatic (unique binary experiment reaching every µ from µ°) is especially doubtful for an attribute-marginal Jeffrey step: a single cue on A can move only the A-marginal and keeps the conditionals P(B\|A) fixed, so most µ are unreachable. Cripps never discusses Jeffrey updating. |
| C5 | Footnote: "Cripps's axioms govern rules on fixed-likelihood experiments" | tex:277 fn | VERIFIED | p.7: an experiment is "a finite set of signals … and state-dependent full-support probability distributions for the signals"; U maps (µ,E_n). |
| C6 | Footnote: "the sequence-invariance that his Symmetry and Divisibility axioms jointly force" | tex:277 fn | VERIFIED-WITH-CAVEAT | Same as C2 (p.9 remark). The footnote also says "the statement here is behavioural", which admits the composite is not an object Cripps's axioms apply to. That contradicts the main-text assertion that it "fails divisibility — and only divisibility" (C3/C4). |
| C7 | bib: Cripps, M.W. 2021, "Divisible Updating", unpublished, Manuscript, UCL | bib:95–100 | VERIFIED | Title and author match p.1. UCL affiliation is on p.1. "This version November 4, 2021" supports year 2021. The file name "…submission2…" suggests a journal submission, so check whether a published version now exists. |

**(c) Most important problems.**
1. **"Fails divisibility — and, of the four axioms, only divisibility" (C3/C4) has no support in the source or the repo.** It asserts axiom-by-axiom facts about an object that is not in Cripps's domain, and the attached footnote concedes as much. The honest statement is: the composite is order-dependent, and Cripps observes (p.9, informally) that Symmetry plus Divisibility imply order-invariance for rules on experiments. Therefore any extension of the Jeffrey composite to experiments would have to violate Divisibility or Symmetry. Drop "only divisibility" unless a proof is added.
2. "Shows" should be softened to "observes" or "notes" (p.9 remark). The kind of order in Cripps (nested revelation within one experiment) should be stated.

---

## 2. Dietrich, F. (2021), "Fully Bayesian Aggregation", JET 194:105255 (dietrich.pdf)

**(a)** I read all 28 PDF pages: HAL cover page plus the "extended version of January 2021", printed pp.1–27, including Appendix A (proof of Thm 2 on opinion pooling), Appendix B (proof of Thms 1/1+) and the references. The Drive copy is the HAL preprint (hal-03194928), not the typeset JET article. Definition and theorem numbering could differ in the published version.

Facts that matter:
- **Definition 1 (p.7)** defines a *linear-geometric EU preference aggregation rule*: utilities are pooled linearly (weights α_i) and probabilities geometrically, "∏_i [p_i]^{β_i} up to a multiplicative constant". It is not a definition of geometric (belief) pooling as such.
- **Pure geometric opinion pooling:** defined in Appendix A (p.16), "The rule is geometric if there exist weights … of sum one such that … G(p) is given on states by ∏_i[p_i]^{w_i}". Characterised by **Theorem 2 (p.11)**: "A belief aggregation rule … is dynamically rational, unanimity-preserving, and continuous if and only if it is geometric."
- **The criterion is Dynamic Rationality (p.8):** "When information is learnt by everyone, the new group preferences equal the old ones conditional on the information", i.e. commutation with **conditionalisation on a common event E**. External Bayesianity (p.15) is commutation with a common **likelihood function**: "Like our axiom, External Bayesianity requires aggregation to commute with revision". Neither covers Jeffrey or soft-evidence revision.
- **Linear pooling fails:** the p.5 duel example ("This is not the case under the linear-linear rule"); fn 7 says linear rules with variable weights "still violate Bayes' rule, except if beliefs are combined dictatorially".
- The theorem excludes |S| = 2 (p.6). Paper B's 2×2 table has 4 states, so this exclusion is harmless.

**(b)**

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| D1 | A normative literature evaluates group belief formation by whether aggregation commutes with updating, a design criterion for the pooling rule | tex:270–272 | VERIFIED | p.15: "External Bayesianity requires aggregation to commute with revision"; p.8 Dynamic Rationality; pp.3–4 frame it as a rationality requirement on the group. |
| D2 | "geometric pooling \citep[Def.~1]{Dietrich2021}" | tex:273 | WRONG-LOCATION | Def. 1 (p.7) defines *linear-geometric preference aggregation*. Geometric belief pooling appears only as its second bullet. Pure geometric pooling is defined in App. A (p.16) and characterised in **Thm 2 (p.11, §4 "Linear or geometric pooling of beliefs?")**. Cite Thm 2 or §4 instead. Also check numbering against the JET version. |
| D3 | Geometric pooling "satisfies the criterion" | tex:273 | VERIFIED-WITH-CAVEAT | Thm 2 (p.11) and App. A Part 1 (p.17) prove that geometric pooling is dynamically rational. p.15: "geometric opinion pooling rules satisfy External Bayesianity". **Caveat:** this holds for updating on a common event or likelihood. The manuscript's evaluators update by Jeffrey on delivered credences, which Dietrich's criterion does not cover. Geometric pooling is not generally known to commute with Jeffrey conditioning. The sentence reads as though Dietrich's criterion applies to the paper's updating operation. |
| D4 | Linear pooling "generically fails the criterion" (as a general fact) | tex:274 | VERIFIED | p.5 example; fn 7 p.6 (fails except dictatorial belief weights); Thm 2 (only geometric rules qualify, given unanimity and continuity). "Generically" is the manuscript's word, but the claim is consistent with the paper. |
| D5 | "the population mean studied in the current paper … is the outcome of linear pooling and generically fails the criterion" (the application) | tex:273–274 | UNSUPPORTED | Dietrich's criterion compares "pool then conditionalise on E" with "conditionalise each member on the same E, then pool". In Paper B the members share a **common prior** (so pooling priors gives P for any rule) and receive the same cues, but apply different Jeffrey **sequences** (AB vs BA). The post-update profile does not "arise from another by conditionalising on an event". No group-level update is defined for the "aggregate then update" side. The criterion therefore cannot be evaluated here as Dietrich defines it. That the average of P_AB and P_BA is a linear pool is true, but "fails the criterion" is the authors' own extension and is not shown in the manuscript. |
| D6 | bib: Dietrich 2021, JET 194, 105255, doi 10.1016/j.jet.2021.105255 | bib:102–110 | VERIFIED | HAL cover: "Journal of Economic Theory, 2021, 194, ⟨10.1016/j.jet.2021.105255⟩". |

**(c) Most important problems.**
1. **Def. 1 is the wrong pointer (D2).** It defines linear-geometric *preference* aggregation. Use Thm 2 / §4 / App. A for geometric belief pooling.
2. **Scope mismatch (D3/D5).** Dietrich's commutation criterion concerns common Bayesian conditioning (event) or External Bayesianity (likelihood). Paper B's non-commutativity comes from Jeffrey updating in different sequences across members who share a prior. Saying the population mean "fails the criterion" is not a result of the source and, as stated, is not well-defined under Dietrich's axiom.
3. **Unused relevant source.** Hayashi (2024, JME 115:103050, "Belief aggregation, updating and dynamic collective choice") is in Drive and bears directly on this paragraph, but is not cited.

---

## 3. Bikhchandani, Hirshleifer & Welch (1992), JPE 100(5):992–1026 (bhw1992.pdf)

**(a)** I read all 36 PDF pages: JSTOR cover plus journal pp.992–1026, including the Appendix (proofs of Prop 1, Results 2 and 4) and the references.

**(b)**

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| H1 | BHW consider herd behaviour and informational cascades "in which sequence matters" | tex:255–256 | VERIFIED | p.992 abstract: cascade "when it is optimal for an individual, having observed the actions of those ahead of him, to follow the behavior of the preceding individual"; p.996 "Each individual observes the decisions of all those ahead of him. The ordering of individuals is exogenous"; p.1003 "it is crucial to pay careful attention to the early leaders". |
| H2 | "In BHW1992 … a cascade is an action that conveys no private signal" | tex:258–259 | VERIFIED-WITH-CAVEAT | p.1000: "DEFINITION. An informational cascade occurs if an individual's action does not depend on his private information signal." and "If an individual i is in a cascade, then his action conveys no information". Also p.999: "actions convey no information about private signals". **Caveats:** (i) BHW define the cascade as a *situation* in which the action does not depend on the signal, and "conveys no information" is the consequence; the paraphrase conflates the two, harmlessly. (ii) "On the other hand" sets BHW against Banerjee as if the mechanism differed. In BHW the cascade also arises because a coarse (binary) action pools signals. BHW fn 17 (p.1002): "with a continuum of actions, behavior generically converges to the correct action"; the same p.1002 passage says Banerjee's incorrect cascades "derive from a degenerate payoff function". The two papers share one mechanism. |
| H3 | Sequencing is across agents; coupling is a feed-forward informational externality (agent n's action enters n+1's information set) | tex:260–265 | VERIFIED | p.996 "observes the decisions of all those ahead of him"; p.1000 inference J_{n+1}(A_n, a_{n+1}). |
| H4 | "the sequence effect in [BHW, Banerjee] lives on the analogue of the marginals" | tex:266–267 | UNSUPPORTED | BHW's state is a single scalar value V (binary or finite, pp.996, 999). There is no multi-attribute state, and hence no marginal-versus-association distinction to map onto. The claim is an interpretive analogy by the authors with no textual basis, and it is trivially true at best (a belief about one variable is "a marginal"). Recast it as the authors' own analogy or remove it. |
| H5 | bib: JPE 100(5):992–1026, doi 10.1086/261849 | bib:123–132 | VERIFIED-WITH-CAVEAT | Title, authors, journal, vol/no/pages and year match the JSTOR cover and p.992. The DOI is not printed on the PDF (JSTOR stable 2138632), so it is unverified here. |

**(c) Most important problems.**
1. **H4 has no basis in BHW.** It is an analogy.
2. **The "on the other hand" contrast between Banerjee and BHW is misleading (H2).** Both rest on a coarse action failing to reveal the signal. BHW's cascade is the extreme case in which the action reveals nothing.

---

## 4. Banerjee, A.V. (1992), "A Simple Model of Herd Behavior", QJE 107(3):797–817 (banerjee1992.pdf)

**(a)** I read all 22 PDF pages: JSTOR cover plus journal pp.797–817.

**(b)**

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| N1 | Banerjee considers herd behaviour in which sequence matters | tex:255–256 | VERIFIED | p.797 abstract: "a sequential decision model in which each decision maker looks at the decisions made by previous decision makers"; p.800: "The signals … that the first few decision makers have will determine where the first crowd forms". Minor nuance: p.815 (§V.B) says the equilibrium rules "do not make use of any information about the order of choice", only the distribution of choices. The arrival sequence still matters. |
| N2 | "the sequence persists because a coarse public action fails to be a sufficient statistic for private information" | tex:257–258 | VERIFIED-WITH-CAVEAT | p.809: "The key reason why we get a different result is that in our model the choices made by agents are not always sufficient statistics for the information they have … this lack of invertibility is what causes the sufficient property to fail"; also p.809: "machines, for example, come in only a small number of sizes". **Caveats:** Banerjee offers the sufficient-statistic failure as the reason for **herding and the herd externality/inefficiency** (compared with the normal learning model that converges). He does not present it as the reason "the sequence persists". "Coarse" paraphrases "lack of invertibility". |
| N3 | Coupling via predecessors' actions (informational externality) | tex:260–265 | VERIFIED | p.799: "herd externality"; p.802: each decision maker observes "the choice made by the previous person" and not their signal. |
| N4 | Sequence effect lives on the analogue of the marginals | tex:266–267 | UNSUPPORTED | The state is a single unknown i* ∈ [0,1] (p.802). There is no joint/marginal structure. Same issue as H4. |
| N5 | bib: QJE 107(3):797–817, doi 10.2307/2118364 | bib:112–121 | VERIFIED | JSTOR cover: "Vol. 107, No. 3, (Aug., 1992), pp. 797-817", stable URL …/2118364. |

**(c) Most important problems.** N2 attributes a "sequence persists" mechanism where Banerjee states a herding/inefficiency mechanism. It is close enough, but should be reworded, e.g. "herding arises because choices are not sufficient statistics for private information (p.809)". N4 is the same unsupported analogy as H4.

---

## 5. Ortoleva, P. (2012), AER 102(6):2410–36, and Epstein, L.G. (2006), RES 73(2):413–36

**(a)**
- **Ortoleva 2012 is NOT in Drive.** No file matches ortoleva/epstein except ortoleva2024.pdf. The Drive has only Ortoleva (2024), "Alternatives to Bayesian Updating", Annu. Rev. Econ. 16:545–70. I read all 26 pages of that review.
- **Epstein 2006 is NOT in Drive.**

What the 2024 review says:
- **On Ortoleva (2012):**
  - §4 "Questioning the Prior" (p.558): "suppose that our individual receives some unexpected news, information that was unlikely given their adopted prior … They may reconsider whether they were using the right prior".
  - §4.1 (p.558): "The intuition above is formalized in the Hypothesis Testing (HT) model of Ortoleva (2012)"; fn 17: "Ortoleva (2012) studies preferences as primitives".
  - p.559: when π(A) ≤ ε "the prior is questioned".
  - p.560: "The following theorem is proved by Ortoleva (2012). Theorem 2. π and {π_A} satisfy Consequentialism and Dynamic Coherence iff they admit a minimal HT model representation".
- **On Epstein (2006):**
  - §3.4 "Biases Due to Temptation" (p.558): "Epstein (2006) introduces this idea and studies a three-period model in which … agents also know that they will be tempted to deviate from Bayes' rule … [à la] Gul & Pesendorfer … Epstein (2006) provides a very neat representation theorem and shows how the model accommodates under- and overreaction, base-rate neglect, sample bias, and representativeness."
  - p.548 cites Epstein (2006) for the point that with *subjective* priors, departures from Bayes need not be mistakes.
- **Scope note:** fn 2 (p.546) explicitly puts **Jeffrey's rule** outside the review's scope ("more general types of information … include classical approaches such as Jeffrey's rule"). The review covers only event-type information.

**(b)**

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| O1 | "\citet{Ortoleva2012} axiomatises departures triggered by unexpected news" | tex:276 | VERIFIED-WITH-CAVEAT | Supported by the secondary source only: Ortoleva 2024 pp.558–560 (HT model; Theorem 2 "proved by Ortoleva (2012)"; departure when π(A) ≤ ε). The paper's title also says "Non-Bayesian Reactions to Unexpected News". The primary is not in Drive. |
| E1 | "\citet{Epstein2006} makes the updating rule subjective" | tex:276 | NOT-CHECKABLE | The primary is not in Drive. The only secondary (Ortoleva 2024 p.558) describes Epstein (2006) as a **temptation / self-control** model (Gul–Pesendorfer menus; agents "tempted to deviate from Bayes' rule"). It does not describe it as making "the updating rule subjective". Obtain the RES paper and check the abstract/representation (in which the tempting posterior is a subjective component) before keeping this wording. |
| OE1 | Epstein and Ortoleva are "alternatives to Bayesian updating … used … to explain whether sequential conditioning is sequence-independent or not" | tex:276 | UNSUPPORTED | The 2024 review presents both as single-step updating on events: temptation-driven under/overreaction and base-rate neglect (Epstein), and prior-questioning after unlikely events (Ortoleva). Neither is presented as a theory of order or sequence effects. The review also explicitly excludes Jeffrey-type soft evidence (fn 2, p.546). The framing fits Cripps (p.2 "order effects"), not these two. |
| O2 | bib Ortoleva2012: AER 102(6):2410–2436, doi 10.1257/aer.102.6.2410 | bib:54–63 | VERIFIED-WITH-CAVEAT | Journal, vol/no/pages and title match the 2024 review's reference list ("Am. Econ. Rev. 102(6):2410–36") and Cripps's references (p.26). The DOI is not verifiable from Drive. |
| E2 | bib Epstein2006: RES 73(2):413–436, doi 10.1111/j.1467-937X.2006.00381.x | bib:43–53 | VERIFIED-WITH-CAVEAT | Matches the review's reference list ("Rev. Econ. Stud. 73(2):413–36"). The DOI is not verifiable from Drive. |

**(c) Most important problems.**
1. **Neither primary is in Drive.** Every check here is second-hand, via a review by Ortoleva himself.
2. **"Makes the updating rule subjective" does not match the available secondary description.** That description is a temptation model.
3. **The sentence frames both papers as addressing sequence-(in)dependence.** The available evidence does not support that framing.

---

## 6. Hayashi (hayashi.pdf) and Thoma (thoma_mistakes.pdf)

- **Hayashi, T. (2024):** "Belief aggregation, updating and dynamic collective choice", J. Math. Econ. 115:103050 (10 pp.). Not cited. No project text relies on it. It is topically relevant to the Dietrich paragraph (tex:270–274) and could support or qualify it.
- **Thoma, J. (2026):** "Some Mistakes Are Irreducibly Diachronic", Theory and Decision (18 pp.). Not cited. No project text relies on it.

(I confirmed these by grep and by reading the title and abstract only. A full read was unnecessary because no claims depend on them.)

---

## Summary counts (25 claims checked)

| Verdict | Count | Items |
|---|---|---|
| VERIFIED | 9 | C1, C5, C7, D1, D4, D6, H1, H3, N1, N3, N5 (H1/N1 and H3/N3 are each one manuscript sentence, counted once) |
| VERIFIED-WITH-CAVEAT | 9 | C2, C6, D3, H2, H5, N2, O1, O2, E2 |
| WRONG-LOCATION | 1 | D2 |
| UNSUPPORTED | 5 | C3, C4, D5, H4/N4 (one sentence), OE1 |
| NOT-CHECKABLE | 1 | E1 |
| MISQUOTED / CONTRADICTED | 0 | — |
