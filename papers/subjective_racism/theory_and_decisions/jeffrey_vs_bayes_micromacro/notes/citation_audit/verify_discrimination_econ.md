# Verification: discrimination-economics and poverty-measurement sources

Repo: `/home/anuragr/development/git/research/papers/subjective_racism/theory_and_decisions/jeffrey_vs_bayes_micromacro` (read-only; nothing edited).
Abbreviations: MS = `PAPER_B_MANUSCRIPT.tex`, BIB = `bibliography.bib`, PLAN = `notes/manuscript_change_plan_asof_2026-09-30.md`, LOG = `notes/paper_review_log.md`, BIR = Bohren, Imas & Rosenberg (2019), `papers/bohren3.pdf`.

How I collected the claims: I grepped every file named in the task. Nothing about these papers turned up in `notes/*.tex` apart from uncited uses of the word "stereotype", and nothing in `literature/**/README.md`. No file mentions `phelps_slides.pdf` or any slides, so the project does not rely on them.

---

## 1. Phelps (1972), "The Statistical Theory of Racism and Sexism", AER

(a) I read all 7 PDF pages visually (the file is a scan). The article is on pages 1–3 (journal pp. 659–661). Pages 4–7 are blank.

What the model is (p. 659–660): the qualification q_i is continuous, and the test score y_i = q_i + u_i has normal error. The only binary variable is the race dummy c_i (eq. 3a). Case 1 is a mean shift, Case 2 gives the groups different variance of qualification (eq. 6), and the "Further Case" (p. 661) gives the test different reliability for each group (eq. 7).

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| P1 | The paper "borrows the evaluator-with-binary-attributes frame" from Phelps | MS:279 | **UNSUPPORTED** (for Phelps) | Phelps's frame is Gaussian signal extraction with a continuous attribute: "The employer is able to measure the performance of each applicant in some kind of test, y_i, which … measure[s] the applicant's promise or degree of qualification, q_i, plus an error term" and "μ is normally distributed with mean zero" (p. 659). The only binary variable is "c_i = 1 if the applicant is black and zero otherwise" (p. 660). There is no pair of binary attributes and no joint law on {0,1}². |
| P2 | Phelps belongs to the "statistical-discrimination lineage" | MS:279 | VERIFIED | p. 659: "introduces what is called the statistical theory of racial (and sexual) discrimination". |
| P3 | Statistical discrimination à la Phelps "rests on a difference in beliefs about the groups" | PLAN:310–311 (item 1.C) | VERIFIED-WITH-CAVEAT | p. 659: the employer "will discriminate against blacks or women if he believes them to be less qualified … on the average". **Caveat 1:** Case 2 and the Further Case rest on differences in variance, or in how reliable the test is by group (eqs. 6, 7), not on a difference in mean belief. CL 1993 (p. 1222) summarises Phelps exactly this way: "Phelps assumed available measures of productivity to be noisier for minority workers". **Caveat 2 (internal tension):** the 1.C paragraph itself says the two groups "then receive different mean beliefs", so the sequence gap is also a difference in beliefs about the groups. The contrast only works as "a difference in prior beliefs / group-level statistics", so the wording should say that. |
| P4 | BIR's correct-belief lineage includes Phelps | LOG:318 | VERIFIED (as a report of BIR) | BIR p. 13: "Theories of belief-based discrimination have typically focused on rational, or statistical, discrimination, where evaluators hold correct beliefs … (i) … imperfect information (Phelps 1972)". Note that Phelps himself allows the prior to come from "prevailing sociological beliefs" (p. 659), so "correct beliefs" is BIR's label, not Phelps's own claim. |
| P5 | BIB: AER 62(4):659–661, 1972 | BIB:145–153 | VERIFIED | The scan's pages are numbered 659–661 ("THE AMERICAN ECONOMIC REVIEW"). The issue (Sept. 1972 = no. 4) is confirmed by the CL 1993 reference list ("September 1972, 62, 659-61") and by BCGS ("62 (4): 659 – 661"). |

(c) Main problem: Phelps does **not** supply a binary-attributes frame. His model is a continuous normal signal-extraction model. Citing him for the "evaluator-with-binary-attributes frame" is wrong for Phelps; it is only defensible for Arrow's §4.

---

## 2. Arrow (1973), "The Theory of Discrimination"

(a) I read all 37 PDF pages visually (scan). **The Drive copy is not the published chapter.** It is *Princeton Industrial Relations Section Working Paper No. 30A*, "Presented at Conference on 'Discrimination in Labor Markets' October 7-8, 1971", with its own pagination (1–31, then references and an appendix).

Structure: §1 covers taste-based discrimination by employers and by co-workers (pp. 1–12). §2 covers nonconvexities and segregation (pp. 13–20). §3 covers costs of adjustment and personnel investment (pp. 21–24). Only §4, "Imperfect Information" (pp. 25–31), is statistical discrimination.

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| A1 | The binary-attributes frame is borrowed from Arrow | MS:279 | VERIFIED-WITH-CAVEAT | Arrow §4, p. 26: there are two groups (W and B), and "Only some workers, however, are qualified to hold skilled jobs … The employer cannot know of any given worker whether or not he is qualified; however, he does believe that the probability that a random W worker is qualified is p_W and that a random B worker is qualified is p_B." So the evaluator faces binary group × binary qualification. **Caveat:** the group is observed and only qualification is uncertain, so this is not a joint belief over two uncertain binary attributes as in the manuscript's {0,1}² law. |
| A2 | Arrow belongs to the "statistical-discrimination lineage" | MS:279 | VERIFIED-WITH-CAVEAT | This holds for §4 (p. 25: "There is an alternative interpretation of employer discrimination. It can be thought of as reflecting, not tastes, but perception of reality"). But about 24 of the 31 text pages are taste-based: "Discrimination means that some economic agent has some negative valuation for B … for which he is willing to pay" (p. 4). |
| A3 | Arrow-style statistical discrimination "rests on a difference in beliefs about the groups" | PLAN:310–311 | VERIFIED | p. 25: "if employers have preconceived ideas that B workers have lower productivity than W workers, they may be expected to be willing to hire them only at lower wages". p. 27: "Once we shift the explanation of discriminatory behavior from … tastes to beliefs". The same wording caveat as P3 applies to the plan's own "different mean beliefs". |
| A4 | BIR's correct-belief lineage includes Arrow | LOG:318 | VERIFIED-WITH-CAVEAT | BIR p. 13–14 lists "(ii) group differences are 'self-fulfilling' and discrimination is an equilibrium effect (Arrow 1973)" under "correct beliefs". Arrow himself also allows beliefs that are *not* correct: he suggests that, by cognitive dissonance, employers "accept subjective probabilities which will supply an appropriate justification for his conduct" (p. 28). "Correct beliefs" is BIR's classification, not a description of Arrow's paper. |
| A5 | BIB: incollection in Ashenfelter & Rees (eds.), *Discrimination in Labor Markets*, Princeton UP, 1973, pp. 3–33 | BIB:155–164 | VERIFIED-WITH-CAVEAT | The book pages cannot be checked against the Drive copy (a working paper with different pagination). They are confirmed by two independent reference lists I read: CL 1993 ("Princeton, NJ: Princeton University Press, 1973, pp. 3-33") and BCGS ("Princeton University Press: 3 – 33"). Any page-specific citation of Arrow must not use the working-paper pagination; none exists at present. |

(c) Main problems: (i) Arrow 1973 is mostly a *taste-based* paper, with statistical discrimination only in §4. (ii) Arrow's §4 also contains the self-confirming-equilibrium idea (pp. 28–31: p_W and p_B "differ in reality … even though the intrinsic abilities … are identical"). This is the idea MS:280 attributes to CL as the contrast case, so the manuscript's "frame from Arrow / fixed point is CL" split is cleaner than the sources are. (iii) The Drive file is the 1971 working paper, not the cited chapter.

---

## 3. Coate & Loury (1993), "Will Affirmative-Action Policies Eliminate Negative Stereotypes?", AER

(a) I read all 21 journal pages (pp. 1220–1240) in full from the text extraction, plus the JSTOR cover.

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| C1 | In CL "the stereotype is a self-confirming steady state of best responses" | MS:280–281 | VERIFIED | p. 1224: "Equilibrium is then a pair of employer beliefs which are self-confirming. A discriminatory equilibrium is one in which workers from one group (B's, say) are believed less likely to be qualified." p. 1225 fn 9: "An equilibrium is a strategy pair (I, A) such that each strategy is a best response to the other." p. 1221: "negative stereotypes constitute a 'self-fulfilling prophecy.'" |
| C2 | Contrast: the current paper is "kinematic rather than an equilibrium fixed point", "unlike CL" | MS:279–281 | VERIFIED-WITH-CAVEAT | The contrast holds, but it is less clean than it reads. (i) CL also study dynamics: "the obvious adjustment process: π_{t+1} = G(β(s*(π_t)))" (p. 1226), and local stability (p. 1226, p. 1232). (ii) CL's employer is a Bayesian with a binary latent attribute (qualified or not), a binary group and a noisy signal θ ("using Bayes' Rule", p. 1224). That is closer to an "evaluator-with-binary-attributes frame" than Phelps is. (iii) CL *define* stereotypes as beliefs about "the correlation between group identity and productivity" (p. 1221). That is a believed cross-attribute association, close to the manuscript's own definition (MS:282, 552). The difference lies in mechanism and correctness, not in the concept. |
| C3 | BIR's correct-belief lineage includes Coate–Loury | LOG:318 | VERIFIED | BIR p. 14: "Discrimination arises when workers from different groups coordinate on equilibria with different levels of skill acquisition (Coate and Loury 1993…)". CL p. 1221: beliefs "in the equilibria of our model, must be correct"; p. 1227: "They correctly perceive group identity to be correlated with worker productivity". |
| C4 | BIB: AER 83(5):1220–1240, 1993 | BIB:166–174 | VERIFIED | JSTOR cover: "The American Economic Review, Vol. 83, No. 5 (Dec., 1993), pp. 1220-1240". |

(c) No outright error. Two framing risks:
- CL's stereotype concept (a believed group–productivity correlation) is close to the manuscript's own concept. The "unlike" should rest on the mechanism: an equilibrium of best responses versus coherent updating.
- CL's affirmative action is "a government-mandated constraint on employers" (p. 1221). That is plausibly the "enforced constraint on behaviour" that MS:283 contrasts with aggregation, but the manuscript never draws that link explicitly.

---

## 4. Bordalo, Coffman, Gennaioli & Shleifer (2016), "Stereotypes", QJE

(a) I read all 73 pages (text extraction), including Appendices A–E. **Version:** the title page reads "First draft, November 2013. This version, May 2015." (PDF created 28 May 2015, modified 1 Jun 2015). It is not dated "June 6", and it is not the published QJE version.

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| B1 | BCGS treat stereotypes as "representativeness-distortion" | MS:282 | VERIFIED | Abstract: "a decision maker assessing a group recalls only that group's most representative or distinctive types". Definition 1 (p. 12): R(t,G)=Pr(G\|T=t)/Pr(−G\|T=t). p. 16: "representativeness is the key assumption that generates stereotypes' most central feature". |
| B2 | "… or selective recall \citep{BCGS2016}" | MS:282 | VERIFIED-WITH-CAVEAT | The term is BCGS's own: "Selective recall is driven by representativeness" (p. 12), and stereotypes are "characterized by limited and selective recall" (p. 14). But it is **one** mechanism (representativeness-based recall), not two alternatives. The "or" wrongly suggests BCGS offer two different accounts. |
| B3 | The manuscript's stereotype comes "not by a distortion of memory or sampling" (contrast with BCGS) | MS:282 | VERIFIED-WITH-CAVEAT | The memory contrast is right: "The DM has stored in memory the full conditional distribution … but he assesses this distribution by recalling only a limited and selected set of types" (p. 12). "Sampling" is not a BCGS mechanism; in §5 updating is Bayesian (fn 33, p. 31). Adversarial point: BCGS §4.3 (p. 27) also produce a distorted *cross-attribute association*: "an exaggeration of the correlation between education and being on welfare". The manuscript's association-based sense of stereotype is therefore not conceptually unlike BCGS; only the source of the distortion differs. |
| B4 | LOG: BIR cite Bordalo et al. (2016b) for biased stereotypes: "such stereotyping corresponds to distortions in the subjective belief about average ability, µ̂_g" | LOG:323–325, 332–334 | VERIFIED | BIR p. 14 verbatim: "Evaluators may form biased stereotypes of ability as a result of using the representative heuristic … (Bordalo et al. 2016b) … In our setting, such stereotyping corresponds to distortions in the subjective belief about average ability, µ̂g." |
| B5 | BIB: QJE 131(4):1753–1794, 2016, doi 10.1093/qje/qjw029 | BIB:176–185 | VERIFIED-WITH-CAVEAT | This cannot be checked against the Drive copy (a 2015 working paper with no journal data). It is confirmed externally: OUP (academic.oup.com/qje/article-abstract/131/4/1753/2468882) and RePEc list vol. 131, issue 4, Nov. 2016, pp. 1753–1794, doi 10.1093/qje/qjw029. Note that the published abstract speaks of "overweighting its representative types" (smooth distortion), whereas the WP's baseline is hard truncation. The manuscript's "representativeness-distortion" fits the published version. |

(c) The attribution is essentially right. "Representativeness-distortion or selective recall" should read "representativeness-driven selective recall". The claimed difference in *concept* is weak, because BCGS §4.3 also generates exaggerated cross-attribute correlation.

---

## 5. Becker (1962), "Irrational Behavior and Economic Theory", JPE — NOT in the Drive folder

(a) Not in Drive. To support the claim rather than rely on memory, I read a JSTOR copy fetched from the web (cooperative-individualism.org; 14 pp. including the JSTOR cover; saved at `scratchpad/becker1962_web.pdf`). I read the introduction, §II passages, §III and §IV in full.

| # | Claim (short) | Where | Verdict | Evidence (external copy) |
|---|---|---|---|---|
| K1 | The distinction "protection intrinsic to the arithmetic of aggregation, rather than imposed by an enforced constraint on behaviour — goes back at least as far as Becker 1962" | MS:282–283 | VERIFIED-WITH-CAVEAT (source outside Drive) | Aggregation half: p. 13: "A group of irrational units would, however, respond more smoothly and rationally than a single unit would"; p. 8: "households may be irrational and yet markets quite rational". **But** Becker's mechanism *is* a constraint: "the word 'survive' simply refers to a resource constraint on behavior" (p. 10), and "irrational units would often be 'forced' by a change in opportunities to respond rationally" (p. 12). A reader who knows Becker can read "rather than imposed by … constraint on behaviour" as the opposite of Becker's point. It is defensible only if "enforced constraint" means a *regulatory* constraint (CL's affirmative action), not a budget constraint. The sentence should say so. |
| K2 | "The lineage distinction the concluding section trades on" | MS:282–283 vs MS:900–967 | **UNSUPPORTED** (internal cross-reference) | The Concluding remarks (MS:900–967) never mention a constraint on behaviour, enforcement, Becker, or a contrast between aggregation and constraint. They discuss protected statistics (separability, stake-weighting). The forward reference points to an argument the conclusion does not make. |
| K3 | BIB: JPE 70(1):1–13, 1962 | BIB:187–195 | VERIFIED (external) | JSTOR cover of the fetched copy: "Journal of Political Economy, Feb., 1962, Vol. 70, No. 1 (Feb., 1962), pp. 1-13". |
| K4 | PLAN says the manuscript "cites that lineage (Phelps, Arrow, Coate-Loury, Becker)" | PLAN:877 | VERIFIED-WITH-CAVEAT | The manuscript's Becker is **Becker 1962 (irrational behaviour)**, not the discrimination Becker. Phelps (p. 659, 661: "G. S. Becker, *The Economics of Discrimination*, Chicago 1959"), Arrow (p. 3: "Becker [1959]") and CL (p. 1222: "Becker (1957)") all cite *The Economics of Discrimination*. Listing Becker in the "discrimination lineage" conflates the two works. |

(c) The Becker sentence is the riskiest in the paragraph. Becker's rational-market result is driven by the budget ("opportunity set") constraint plus averaging over many units, so "rather than … a constraint on behaviour" nearly inverts him. The sentence also points to a distinction the conclusion never draws.

---

## 6. Foster, Greer & Thorbecke (1984), "A Class of Decomposable Poverty Measures", Econometrica

(a) I read all 9 PDF pages: the JSTOR cover, journal pp. 761–766, and 2 pages of JSTOR linked citations. I read equation pages 762–763 visually, because the text extraction drops the formulas.

Key text (p. 763, eq. 3): "For each α ≥ 0, let P_α be defined by P_α(y; z) = (1/n) Σ_{i=1}^{q} (g_i/z)^α. The measure P_0 is simply the headcount ratio H, while P_1 is H·I, a renormalization of the income-gap measure. The measure P is obtained by setting α = 2." Here H = q/n and I = Σ_{i≤q} g_i/(qz) (p. 762).

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| F1 | Headcount and mean shortfall belong to "a single parametrised family of indices \citep{FGT1984}" | MS:297 | VERIFIED | Eq. (3), p. 763; "We generalize the new poverty measure to a parametric family of measures" (p. 761). |
| F2 | "a headcount (a count of those past a threshold)" is the α = 0 member | MS:297 | VERIFIED-WITH-CAVEAT | P_0 is the headcount **ratio** H = q/n (p. 762–763), a share, not a count. (The manuscript's own statistic is a *share* of flipped decisions, which matches FGT better than the gloss does.) |
| F3 | "a mean shortfall (average of their distances from it)" is the α = 1 member | MS:297 | **MISQUOTED** | "Average of *their* distances" = (1/q)Σ g_i, the mean gap among the poor, which equals z·I. That is the income-gap measure, and it is **not** a member of the P_α family. FGT's P_1 = H·I = (1/n)Σ_{i≤q} g_i/z: it is **normalised by z** and averaged over **all n** (the non-poor count as zero), i.e. "a renormalization of the income-gap measure". Suggested wording: "the population-average normalised shortfall (the non-poor contributing zero)". The manuscript's own L(c) = E[\|u\|·1{flip}] is in fact a whole-population average of this P_1 kind, so only the gloss is wrong, not the analogy. |
| F4 | These are the "first two members" | MS:297 | VERIFIED-WITH-CAVEAT | α ranges over all reals ≥ 0, so 0 and 1 are the first two *integer* members. FGT's own headline measure is α = 2 (P, eq. 1), which the manuscript's pairing does not use. |
| F5 | "the standard incidence-versus-intensity pairing of the measurement literature" (attributed via the FGT citation) | MS:297 | VERIFIED-WITH-CAVEAT | FGT do not use "incidence/intensity" language. They present P_0 as the headcount ratio, P_1 as the renormalised income-gap measure, and α as "poverty aversion" (p. 763). The pairing is standard in later literature but is not stated in FGT. |
| F6 | BIB: Econometrica 52(3):761–766, 1984 | BIB:219–226 | VERIFIED | JSTOR cover: "Econometrica, Vol. 52, No. 3. (May, 1984), pp. 761-766". |

(c) The Section 4 gloss of FGT is technically wrong on P_1. It describes the average gap among the poor, not normalised by z, which is FGT's I (times z) and not in the family. It also calls P_0 a count rather than a ratio. Both are easy fixes, and the underlying analogy to the manuscript's share and L(c) is sound.

---

## Tally

Out of 29 claims: 12 VERIFIED; 14 VERIFIED-WITH-CAVEAT; 2 UNSUPPORTED; 1 MISQUOTED; 0 CONTRADICTED; 0 WRONG-LOCATION; 0 NOT-CHECKABLE. Becker was checked against an external copy; judged on the Drive folder alone, K1, K3 and K4 would be NOT-CHECKABLE.

## Most important problems, ranked

1. **MS:297, FGT (MISQUOTED):** "mean shortfall (average of their distances from it)" is not P_1. P_1 = (1/n)Σ g_i/z is normalised by z and averaged over the whole population. Also, P_0 is the headcount ratio, not "a count".
2. **MS:279, Phelps (UNSUPPORTED):** Phelps has no binary-attributes frame (continuous q, normal test score, only a race dummy). Arrow §4, and arguably CL, fit; Phelps does not.
3. **MS:282–283, Becker (tension plus a dangling cross-reference):** Becker's "rational markets" result depends on the budget constraint ("resource constraint on behavior", units "forced" to respond rationally). Saying the protection is "rather than imposed by … constraint on behaviour" risks inverting him. The "concluding section" never draws the distinction it is said to trade on.
4. **MS:280–282, CL/BCGS contrasts overstated at the level of concept:** CL define stereotypes as believed group–productivity correlations, and BCGS §4.3 produce exaggerated cross-attribute correlation. Both are close to the manuscript's own definition. The honest contrast is in mechanism: best-response equilibrium, or representativeness-driven selective recall, versus coherent Jeffrey updating. "Representativeness-distortion **or** selective recall" should read as one mechanism.
5. **Arrow 1973 characterisation:** the paper is mostly taste-based (§§1–3). Its §4 already contains the self-confirming equilibrium the manuscript attributes to CL as the contrast. The Drive copy is the 1971 Princeton IR working paper, not the chapter (pp. 3–33 confirmed only via the CL and BCGS reference lists).
6. **PLAN 1.C wording:** "statistical discrimination … rests on a difference in beliefs about the groups" collides with the same paragraph's "the two groups … receive different mean beliefs". It should say "prior beliefs / group statistics". Phelps's own cases also include differences in variance and in test reliability.
7. **PLAN:877:** "lineage (Phelps, Arrow, Coate-Loury, Becker)" conflates Becker 1962 with Becker's *Economics of Discrimination* (1957), which is the work the discrimination papers cite.
8. **Versions:** the BCGS Drive copy is the May 2015 WP (not "June 6"). The published QJE details in BIB are correct (checked externally).
