# Verifying the project's claims about Hogarth & Einhorn (1992) and Asch (1946)

Repo: `research/papers/subjective_racism/theory_and_decisions/jeffrey_vs_bayes_micromacro` (read-only; nothing was edited).
Files searched: `PAPER_B_MANUSCRIPT.tex`, `bibliography.bib`, `notes/*.md`, `notes/*.tex`, `literature/**/README.md`, `literature/measurement_susceptibility_survey.md`, `lean/JeffreyOrder/Anchoring.lean`, `sympy/*.py`.
Abbreviations: CP = `notes/manuscript_change_plan.md`, RL = `notes/paper_review_log.md`, SV = `literature/measurement_susceptibility_survey.md`, AL = `lean/JeffreyOrder/Anchoring.lean`, ZS = `sympy/check_zero_slope_identification.py`, IO = `notes/interior_omega.tex`, PD = `notes/papers_dialectic.tex`, DP = `notes/the_discrimination_problem.tex`, QA = `notes/question_and_answer.tex`, MS = `PAPER_B_MANUSCRIPT.tex`, VWC = VERIFIED-WITH-CAVEAT.

---

## 0. The source files themselves

**Hogarth–Einhorn: the "authoritative PDF" is not the journal article.** `hogarth_einhorn1992.pdf` has 42 pages. Its metadata says `Creator: TeX, Producer: MiKTeX pdfTeX-1.40.24`, created 2026-07-11. It is a compiled plain-LaTeX transcription, apparently of `hogarth_einhorn1992.tex`, and not a scan of *Cognitive Psychology* 24:1–55. Consequences:
* Journal page numbers (1–55) **cannot be checked**. Below, pages are the transcription's own (T-p.N).
* The PDF has unresolved cross-references ("Table ??", "Fig. ??"). Appendix A is typeset as "Table 5".
* The transcriber notes: "The complexity / length / response-mode marks above were transcribed from a rotated scan and a few should be checked against the original." The equations are "reconstructed from the (OCR-mangled) source".
* The PDF and the .tex **agree** on every equation (1)–(8), (B.1)–(C.7), on the quoted sentences and on Table 1. I found no substantive disagreement. One small internal inconsistency: Appendix A row 23 has "Feldstein & Bernstein (1978)", while the reference list has "Feldman & Bernstein (1978)".
* The project describes this honestly in SV:10 ("full plain-LaTeX transcription") and DP:322. CP:332/450/1264 say "checked against the text (Drive: `hogarth_einhorn_1992.pdf`)" without saying it is a transcription. The Drive file name there also differs from the local `hogarth_einhorn1992.pdf`.

**Asch: an original scan.** 33 pages, journal pp. 258–290. The table numbers below were read from the rendered pages (PDF pp. 13–16 = journal pp. 270–273).

---

## 1. Hogarth & Einhorn (1992)

**(a) I read all 42 pages of the transcription PDF**, covering abstract, review, model, predictions, Experiments 1–5, Discussion, General Discussion, Appendices A–C and references. I checked Table 1 and Appendix A p.31 on the rendered page. I also checked the .tex equations against the PDF.

Key locations (transcription pages):
* Eq.(1) and the definitions, including "w_k the adjustment weight for the kth piece of evidence (0 ≤ w_k ≤ 1)": T-p.6.
* Eqs.(2)–(4): T-p.7.
* Eq.(5): T-p.9.
* Eqs.(6a)–(7b), plus "values of α and β would decline in a long series of evidence items": T-p.10–11.
* EoS "force toward primacy", Eq.(8): T-p.12.
* Table 2: T-p.12.
* Long series, "we predict decrements in α and β that will eventually induce primacy": T-p.13.
* "76 data points for the 60 studies": T-p.5.
* Attention decrement (Anderson 1981): T-p.6.
* Contrast assumption "critical to the belief-adjustment model": T-p.28.
* Memory quote, in General Discussion › Limitations and Extensions: T-p.29.
* Fig. 7, the α–β space with the "Insensitive" (α=β=0), "Advocate" (α=0,β=1) and "Skeptic" corners: T-p.29–30.
* Appendix A row 14, "Asch (1946). Trait adjectives — S, Sh, EoS, Primacy": T-p.31.
* Appendix B, "when R = S_{k−1} … recency always obtains": T-p.34–35.
* Appendix C: T-p.35–36.

**(b) Claims**

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| H1 | Eq.(1) `S_k = S_{k-1} + w_k[s(x_k) − R]`; S_k is the "degree of belief in some hypothesis, impression or attitude after evaluating k pieces of evidence", S_k ∈ [0,1] | SV:28-31 | VERIFIED | T-p.6: "S_k = S_{k−1} + w_k [ s(x_k) − R ], (1) … S_k degree of belief in some hypothesis, impression or attitude after evaluating k pieces of evidence (0 ≤ S_k ≤ 1)." |
| H2 | Averaging form `S_k=(1−w_k)S_{k−1}+w_k s(x_k)` is their Eq. 4 | AL:14-15; CP:217, 1177-1178, 1264 | VERIFIED | T-p.7: "which, after rearranging terms, leads to the averaging form S_k = (1 − w_k)S_{k−1} + w_k s(x_k). (4)". Note: Eq.(4) is the estimation-mode (R=S_{k−1}) special case only. |
| H3 | "Adjustment weight" definition checked | CP:217-218 | VERIFIED | T-p.6: "w_k the adjustment weight for the kth piece of evidence (0 ≤ w_k ≤ 1)." |
| H4 | Quote "memory is limited to the location of one's current anchor and not how this was reached", from the General Discussion | AL:17-18; CP:423-424, 449, 1264-1266 | VERIFIED | T-p.29 (General Discussion › Limitations and Extensions): "…because it is assumed that memory is limited to the location of one's current anchor and not how this was reached…". Context: said about "double-counting" of redundant testimony. |
| H4b | "Their model shares the amnestic feature of the paper's rule" | AL:16-17 (also implied at CP:421-424) | VWC | HE's sentence is about path-independence: only the current anchor is remembered. The paper's "amnestic" means Hawthorne's full adoption (ω=1). HE's rule is path-independent at every w, so the quote does not support full adoption. The gloss mixes these two properties up. |
| H5 | Abstract: "under what conditions do primacy, recency, or no order effects occur ... interaction of information-processing strategies and task characteristics" | CP:329-331 | VERIFIED | T-p.1: "However, under what conditions do primacy, recency, or no order effects occur? This paper presents a theory … as arising from the interaction of information-processing strategies and task characteristics." The ellipsis is faithful. |
| H6 | HE "find primacy, recency or no order effect, depending on the characteristics of the task" | CP:299-300 (proposed manuscript text) | VWC | Primacy appears only in their **literature classification** (Table 1, T-p.4) and their model **predictions** (Table 2). Their five experiments found only no effect (Exps 1, 2, and 5-verbal) or recency (Exps 3, 4, and 5-numerical); none found primacy. Their predictions also depend on encoding mode (R=0 vs R=S_{k−1}) and on mixed vs consistent evidence, not only on task characteristics. |
| H7 | "Primacy in their model comes from weights decaying over a long series" | AL:18-20; CP:447-448, 787-788, 1266 | **CONTRADICTED** (as an exclusive account) | HE have a **second and principal** source of primacy: the EoS process. T-p.12: "the structure of the EoS strategy contains within it a force toward primacy … the first piece of evidence … serves as the anchor … Eq.(8) … the effective weight accorded to s(x1) must be greater than the weight attached to any of the other pieces". Table 2 predicts **Primacy** for short simple EoS series. This is the cell they match to "19 of 27 studies" (T-p.12). Decaying α, β covers only long series (T-p.11, T-p.13). |
| H8 | The ω=0 / δ=0 endpoint ("first impression never moved") is "ours, not theirs"; "no source states 'first impressions stick' as full protection of the first cue"; "the first is our endpoint, not a cited position" | AL:20-21; CP:446-448, 1267, 1277-1278 | **CONTRADICTED** (partly) | HE define w_k on the closed interval "(0 ≤ w_k ≤ 1)" (T-p.6). Fig. 7 (T-p.29) names the corner α=β=0: "attitudes of extreme insensitivity … to both negative and positive evidence". It also names the "advocate who ignores negative evidence" (α=0). Appendix C (T-p.11) has "except if α or β is zero (i.e., either negative or positive evidence is completely ignored)". With the EoS anchor on the first item (Eq. 8), w=0 gives S_k = s(x1), which is literally "first impression never moved". Asch p.273 also reports subjects who excluded a late trait entirely: "I excluded it … In my first impression it was left out completely." The **asymmetric** construction is new: first cue adopted in full, only the second damped. The endpoint itself is not. |
| H9 | "Attention decrement" is Anderson (1981), reported by HE as a rival | AL:21-22; CP:1267-1268; ZS:6 | VERIFIED | T-p.6: "Anderson claimed that primacy can be explained by a process of 'attention decrement' … The attention-decrement explanation, however, raises two important issues…" |
| H10 | "HE document order effects (primacy and recency) abundantly across 76 data points" | SV:39-40 | VWC | T-p.5: "There are thus 76 data points for the 60 studies detailed in Appendix A". Table 1 totals are 20+30+4+16+5+1 = 76 (checked on the render), so 5 of the 76 are "No effect". These are other authors' studies, classified by HE, not HE's measurements. Some use multi-item instruments; Asch 1946 is row 14. |
| H11 | Response modes Step-by-Step vs End-of-Sequence; response mode governs *when* the scalar is reported | SV:34-35 | VERIFIED | T-p.3–4: "the Step-by-Step procedure … and the End-of-Sequence procedure … abbreviated as SbS and EoS". |
| H12 | S_k is one-dimensional; "There is no joint distribution over two attributes anywhere in the paper" | SV:28-34 | VWC | True of the model and of their experiments (0–100 likelihood ratings). But the Limitations section (T-p.29) explicitly raises "how people deal with dependencies among different pieces of information … we have implicitly assumed that the outcomes of the coding process already include whatever conditioning the subject has done". That is a dependence/conditioning issue, even though no joint over attributes is modelled. |
| H13 | "The family is their eq.(1) with R = S_{k−1}" | RL:800-801; DP:322-325 | VWC | Algebraically correct: it is their **Eq.(3)** (T-p.7). But HE's w_k is not a free constant. It is state-dependent, w_k=αS_{k−1} or β(1−S_{k−1}) (Eqs. 6a/6b, T-p.10), and HE call this contrast assumption "critical to the belief-adjustment model" (T-p.28). HE attribute the constant-weight R=S_{k−1} model to Anderson & Hovland (1957) (T-p.11). HE also apply w to **every** item, including the first, whereas the project damps only the second cue. |
| H14 | `S_k=S_{k−1}+w(s−S_{k−1})` is "Hogarth-Einhorn's own adjustment equation … so the rival is their model, not a strawman" | RL:721-723; ZS:15-16; AL:4-6, 14-15; DP:96; CP:1177-1179, 1339 | VWC | Same caveats as H13. With constant weight, one-sided damping and no contrast assumption, the family is at most a stripped-down relative of HE's model. It is not "their model". |
| H15 | "Partial adjustment in the manner of Hogarth and Einhorn protects the *first* impression"; in 6.A, partial-adjustment accounts "point in opposite directions [to amnestic]: the protected impression is the first one there" | PD:164-166; CP:1300-1303 (proposed manuscript text, cites HE at 1300); RL:802-803 | **CONTRADICTED** | In HE, partial adjustment protects the **last** impression under SbS. Appendix B (T-p.34–35), R=S_{k−1}: "D = w_a w_b[s(x_b) − s(x_a)] … Because s(x_b) > s(x_a), D > 0 and recency always obtains". T-p.11: "when R = S_{k−1}, the SbS process always predicts recency (for α, β ≠ 0)". For R=0 with mixed evidence it is also recency (Appendix C). In HE, primacy comes only from EoS anchoring or long-series decay, not from partial adjustment as such. |
| H16 | "Position channel, in which the observer weights the later cue less whatever the attributes are", cited to HE and Asch | IO:62-65 (propagated to CP:203-205 and writing_discipline.md:84) | **CONTRADICTED** (for both citations) | HE: SbS partial adjustment gives the later cue **more** effective weight (recency; see H15). Asch explicitly rejects position as such (p.272): "It is not the sheer temporal position of the item which is important as much as the functional relation of its content to the content of the items following it". So in Asch the effect depends on *what the traits are*. HE (T-p.5–6) describe Asch's mechanism as "change of meaning", not down-weighting. |
| H17 | "The belief-adjustment model of HE lies between [the overwrite and never-moved endpoints]" | CP:421-424 | VWC | w_k ∈ [0,1], so this is true in a loose sense. But HE's weight is state-dependent, applies to every cue, and includes both endpoints (see H8). |
| H18 | HE "focus on a single evaluative anchor"; they elicit a single scalar or level | MS:277; CP:836; QA:64 (HE half); SV:15, 28, 158 | VERIFIED | Model: S_k is a scalar anchor. Experiments 1–5: a 0–100 rating after the stem and after each item (SbS) or at the end (EoS) (T-p.15, 23). Minor: "evaluative" may be confused with HE's *evaluation mode* (R=0), but the anchor exists in both modes. |
| H19 | "HE frame it as a descriptive taxonomy of order effects" | SV:115 | VWC | T-p.1: "This paper presents a descriptive theory of belief updating". It is a theory, and the task taxonomy is only one part of it. |
| H20 | Order effects are "a documented regularity" / "among the oldest findings in the study of impression formation" (cited with Asch) | MS:73; notes/two_horn_motivation_body.tex:35; CP:170, 412 | VERIFIED | HE review 60 studies (T-p.4–5). HE themselves mention earlier order-effect work (Lund 1925, T-p.34 note), so the "oldest" part rests on Asch. |
| H21 | Bibliography: Cognitive Psychology 24, 1–55, 1992, title | bibliography.bib:75-84; bibitems in QA-doc:49-51, two_horn_motivation.tex:45-47, DP:320-322 | VERIFIED | Transcription header: "Cognitive Psychology 24, 1–55 (1992)"; the title matches. |
| H21b | bib `number = {1}`, `doi = {10.1016/0010-0285(92)90002-J}` | bibliography.bib:81, 83 | NOT-CHECKABLE | Neither appears in the transcription. The journal original is not in the folder. |
| H22 | Interior ω is "the primacy regime" (with the rule that "primacy"/"recency" appear only where HE are cited) | IO:323-324, 448; writing_discipline.md:79-80 | UNSUPPORTED (as HE vocabulary) | In HE, interior partial adjustment under SbS is the **recency** regime (Appendix B). Even in the project's one-sided scheme, for a single attribute the weights on first and second items are (1−ω, ω), which gives primacy only for ω < ½. |
| H23 | HE statements "checked against the text (Drive: `hogarth_einhorn_1992.pdf`)" | CP:332, 450, 1264 | VWC | The checked file is a LaTeX transcription with reconstructed equations. See §0. |

---

## 2. Asch (1946)

**(a) I read all 33 pages (journal pp. 258–290).** For the load-bearing pages (270–273: Experiment VI, Tables 7 and 8, footnotes 4 and 5, Experiment VII) I checked the rendered scan, not only the text extraction.

Notes by page:
* 258–260: Propositions I, Ia and II.
* 260–261: procedure (over 1,000 subjects).
* 262: Check List I (Table 1), 18 forced-choice pairs "mostly opposites"; the subject selects "the one that was most in accordance with the view he had formed".
* 262–265: Experiment I, warm/cold, N=90/76. Table 2. Traits unaffected by warm/cold are (8) reliability, (9) importance, (11) attractiveness, (12) persistence, (13) seriousness, (14) restraint, (17) strength and (18) honesty. Asch explains this "by virtue of their content" (p.264). Table 3 has the rankings. p.264–265: "Generally the individual responses exhibit much stronger trends in a consistently positive or negative direction."
* 265: Experiment II (warm/cold omitted, N=56). Asch subdivides subjects by whether they checked "warm" or "cold" (23 = 41% warm) and tabulates the other traits within each subgroup.
* 266–267: Experiment III (polite/blunt).
* 267–268: Experiment IV (the meaning of warm/cold shifts).
* 268–270: Experiment V (calm, strong; Table 6).
* 270: conclusions, "The change of a central trait may completely alter the impression". Experiment VI lists A and B, N=34 and 24. "some of the qualities (e.g., impulsiveness, criticalness) are interpreted in a positive way under Condition A, while they take on, under Condition B, a negative color".
* 271: Table 7. Within-subject group (N=24): 14 changed and 10 did not, some of whom "waited until the entire series was read". "the first terms set up in most subjects a direction".
* 272: "a broad, uncrystallized…", "a factor of primacy", "not the sheer temporal position…". Footnotes 4 and 5. Table 8. Experiment VII lists.
* 273: Experiment VII results. "evasive" did not fit for 11/27 (A) and 11/30 (B). Some subjects "completely excluded it", while for others "the final term completely undid their impression". Experiment VIII begins.
* 274–275: Experiment VIII (Table 9; "Did you experience difficulty…", with 32/52 answering yes).
* 275–277: Experiments IX and IXa (Tables 10 and 11).
* 278–283: Experiment X (resemblance of sets; Tables 12 and 13) and the comparisons of aggressive, critical, stubborn, witty and gay.
* 283–290: Discussion; Hartshorne & May.

**My transcription of Table 7 (rendered p.271), "Choice of fitting qualities (percentages)".** Each cell is the percentage choosing the listed (positive) member of the Check List I pair, for example restrained vs talkative.

| # | Trait (vs opposite) | Exp VI: Intelligent→Envious (N=34) | Exp VI: Envious→Intelligent (N=24) | Exp VII: Intelligent→Evasive (N=46) | Exp VII: Evasive→Intelligent (N=53) |
|---|---|---|---|---|---|
| 1 | generous (ungenerous) | 24 | 10 | 42 | 23 |
| 2 | wise (shrewd) | 18 | 17 | 35 | 19 |
| 3 | happy (unhappy) | 32 | 5 | 51 | 49 |
| 4 | good-natured (irritable) | 18 | 0 | 54 | 37 |
| 5 | humorous (humorless) | 52 | 21 | 53 | 29 |
| 6 | sociable (unsociable) | 56 | 27 | 50 | 48 |
| 7 | popular (unpopular) | 35 | 14 | 44 | 39 |
| 8 | reliable (unreliable) | 84 | 91 | 96 | 94 |
| 9 | important (insignificant) | 85 | 90 | 77 | 89 |
| 10 | humane (ruthless) | 36 | 21 | 49 | 46 |
| 11 | good-looking (unattractive) | 74 | 35 | 59 | 53 |
| 12 | persistent (unstable) | 82 | 87 | 94 | 100 |
| 13 | serious (frivolous) | 97 | 100 | 44 | 100 |
| 14 | restrained (talkative) | 64 | 9 | 91 | 91 |
| 15 | altruistic (self-centered) | 6 | 5 | 32 | 25 |
| 16 | imaginative (hard-headed) | 26 | 14 | 37 | 16 |
| 17 | strong (weak) | 94 | 73 | 74 | 96 |
| 18 | honest (dishonest) | 80 | 79 | 66 | 81 |

**Table 8 (rendered p.272), "Ranking of 'Envious': Experiment VI"**

| Rank | I→E: N | I→E: % | E→I: N | E→I: % |
|---|---|---|---|---|
| 1 | 5 | 15 | 7 | 29 |
| 2 | 4 | 11 | 4 | 17 |
| 3 | 5 | 15 | 5 | 21 |
| 4 | 3 | 9 | 2 | 8 |
| 5 | 4 | 11 | 2 | 8 |
| 6 | 13 | 39 | 4 | 17 |
| Total | 34 | 100 | 24 | 100 |

**(b) Claims**

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| A1 | Experiment VI / Table 7 reports 18 traits (generous … honest, in the listed order) under `intelligent->envious` and `envious->intelligent`; "eighteen marginals under two reading orders" | RL:522-528; CP:805-808, 837-840 | VERIFIED | Table 7, p.271. The list and its order match exactly. |
| A2 | restrained 64 vs 9 | RL:531; CP:816, 842-843 | VERIFIED | 64 / 9 (91% chose "talkative" under E→I). |
| A3 | good-looking 74 vs 35 | RL:530; CP:816-817, 843 | VERIFIED | 74 / 35 |
| A4 | serious 97 vs 100 | RL:533; CP:817, 844 | VERIFIED | 97 / 100 |
| A5 | persistent 82 vs 87 | RL:533; CP:817, 844 | VERIFIED | 82 / 87 |
| A6 | reliable 84 vs 91 | RL:532; CP:817-818, 845 | VERIFIED | 84 / 91 |
| A7 | humorous 52 vs 21 | RL:531 | VERIFIED | 52 / 21 |
| A8 | good-natured 18 vs 0 | RL:531 | VERIFIED | 18 / 0 |
| A9 | important 85 vs 90 | RL:532 | VERIFIED | 85 / 90 |
| A10 | Column assignment: first number is intelligent→envious | same lines as A2–A9 | VERIFIED | Col 1 is "Intelligent→Envious (N=34)" and col 2 is "Envious→Intelligent (N=24)". All eight pairs are in the right order. Unremarked by the project: all four "barely move" traits move **toward** envious-first, the opposite direction to the large swings. |
| A11 | Table 7 gives "the percentage/proportion of subjects judging [each trait] to fit"; the check-list asks "does generous fit?" | RL:526, 538; CP:806, 809-810, 838-839, 849-850 | VWC | The check-list is a **forced choice between a pair of mostly-opposite terms** (p.262: "From each pair … select the one that was most in accordance with the view he had formed"). The subject is never asked whether a single trait fits. The figure is the percentage choosing the positive member of the pair (p.263). |
| A12 | "The check-list … never [asks] whether two traits go together" | RL:538-539; CP:810, 850 | VWC | True of the check-list. But Asch does ask fit and co-occurrence questions elsewhere: p.273 (Exp VII) "Were there any characteristics that did not fit with the others?"; p.274 (Exp VIII) "Did you experience difficulty in forming an impression…", with answers about traits being "contradictory"; pp.278–280 (Exp X) which qualities in one set "resemble" those in another. |
| A13 | "The joint is never elicited, so the association cannot be formed from his data"; "the association cannot be formed from his data" | RL:539-540; CP:810-811 | **CONTRADICTED** | Each subject fills in all 18 pairs, so the data **are** a joint response across traits. Asch uses this: in Experiment II (p.265) he splits subjects by the "warm"/"cold" item and reports the other traits' distributions within each subgroup, which is a conditional (association) analysis. On p.264–265 he notes that individual protocols "exhibit much stronger trends in a consistently positive or negative direction" than the group marginals. He *publishes* only marginals for Exp VI, but the association can be formed from his data. What the data lack is a subject's **credence** about co-occurrence (and any prior). |
| A14 | "The check-list asks whether a trait fits and never whether two traits go together, so the **prior** association cannot be formed from such data at all" | CP:849-851 (proposed manuscript text) | VWC | Asch collected nothing before the list was read, so this is correct for a *prior* association. The first half has the A11/A12 caveats. |
| A15 | Uneven swings shown "with no account given/offered of why"; "an unexplained regularity" | CP:818-819, 846-847; RL:534-535 | VWC | For Exp VI, Asch gives no trait-by-trait account. But he does give a content-based account of uneven effects in Exp I (p.264: "it is not surprising that some will, by virtue of their content, remain unchanged"; footnote 2). Restrained and good-looking were among the traits unaffected by warm/cold in Exp I, yet they swing strongly in Exp VI. |
| A16 | "'impulsive' and 'critical' are the ones he reports as taking positive or negative colour from context" | RL:552-553 | VERIFIED | p.270: "some of the qualities (e.g., impulsiveness, criticalness) are interpreted in a positive way under Condition A, while they take on, under Condition B, a negative color". |
| A17 | "'intelligent' and 'envious' are near-decisive"; "his trait words differ in decisiveness, and Experiment VI turns on exactly that" (a softness contrast) | RL:550-552 | UNSUPPORTED | Asch (p.270) calls them "qualities of high merit (intelligent—industrious)" and "a dubious quality (envious)". He makes no claim about decisiveness. The project withdrew this itself at RL:589-590. |
| A18 | Quote "A broad, uncrystallized but directed impression is born. The next characteristic comes not as a separate item, but is related to the established direction... later characteristics are fitted --- if conditions permit --- to the given direction." at "pp.272-273" | RL:574-577 | WRONG-LOCATION (minor) | The wording is verbatim, apart from a capitalised "A" (the original continues a sentence begun on p.271: "When the subject hears the first term, a broad, …") and a dropped footnote marker 4. Location: the sentence starts on **p.271** and the quoted text is all on **p.272**. Nothing is on p.273. |
| A19 | "It is not the sheer temporal position of the item which is important as much as the functional relation of its content to the content of the items following it." | RL:577-579 | VERIFIED | p.272, verbatim. The bold emphasis in RL is the project's and is not marked as added. |
| A20 | Footnote 5: primacy "should be abolished --- or reversed --- if it does not stand in a fitting relation to the succeeding qualities, or if a certain quality stands out as central despite its position", citing warm-cold from Experiment I, high-ranked from a middle position | RL:581-585; CP:863-865 | VERIFIED | p.272, fn 5, verbatim; it continues "The latter was clearly the case for the quality 'warm-cold' in Experiment I (see Table 1) which, though occupying a middle position, ranked comparatively high." Asch's own "(see Table 1)" should read Table 3 (the rankings, p.265). |
| A21 | Footnote 5 names "*centrality* -- not softness -- as what abolishes or reverses primacy" | CP:864-865; RL:580 | VWC | Footnote 5 gives **two** conditions. The first is the lack of "a fitting relation to the succeeding qualities"; centrality is the second. |
| A22 | "The change of a central trait may completely alter the impression" | RL:587 | VERIFIED | p.270, conclusion 1, verbatim. |
| A23 | Table 8: "envious" is 6th under `intelligent->envious` and 1st under the reverse; "Order alters a trait's centrality" | RL:591-594 | VWC | These are **modal** ranks only. I→E: rank 6 has 13/34 = 39%, but 15% gave rank 1. E→I: rank 1 has 7/24 = 29%, ranks 2–3 have 38%, and 17% still gave rank 6 (see the table above). The claim reads as if the ranks were deterministic. Asch presents Table 8 only as "further evidence with regard to this point" (p.272). |
| A24 | "Asch reports that early terms dominate" | CP:298-299 (proposed manuscript text) | VWC | p.271: "the first terms set up in most subjects a direction which then exerts a continuous effect on the latter terms"; p.272: "a factor of primacy". HE's Appendix A classifies Asch as EoS primacy. Caveats: 10 of the 24 within-subject subjects reported **no change** (p.271). In Exp VII, for some subjects "the final term completely undid their impression and forced a new view" (p.273). Footnote 5 says primacy can be abolished or reversed. Asch also denies that position as such is what matters (p.272). |
| A25 | "On single-attribute sequential data of the Asch type, an overwriting rule predicts recency whereas Asch found primacy" | RL:702-705 | VWC | "Asch found primacy" is supported (p.271–272, Exp VII p.272). But Asch's data are not "single-attribute": they are 18 check-list traits plus sketches and rankings, as RL:522 itself says. |
| A26 | Asch's "own account is that early terms set a direction for the reading of later ones, so that what a later cue delivers depends on its position" | CP:1315-1319 (proposed manuscript text) | VWC | The "direction" account is correct (p.271–272). But Asch says it is "not the sheer temporal position … as much as the functional relation of its content". What a later term delivers depends on its **relation to the established direction**, not on its position as such. |
| A27 | "Asch is evidence that order moves marginals, **one cue per trait** across eighteen traits" | CP:1314-1315 (proposed manuscript text) | **CONTRADICTED** | Experiment VI has **six** stimulus terms (intelligent, industrious, impulsive, critical, stubborn, envious; p.270). The 18 traits are **response** items on Check List I, and none of them is a stimulus term. There is no cue-to-trait correspondence, and the paper's attribute-locality structure (each cue sets its own attribute's marginal) does not map onto Asch's design. |
| A28 | "Those studies [HE, Asch] elicit a single evaluative level" | QA:63-65 | **CONTRADICTED** (for Asch) | Asch collects an 18-pair check-list (Tables 2, 7, 9, 10), a second 12-pair check-list (Table 11), free sketches, importance rankings (Tables 3, 4, 5, 8), synonyms (Table 6) and resemblance judgments (Tables 12, 13). RL:522 already calls the "single impression" description of Asch "wrong", but QA:64 still says it. |
| A29 | Asch's account is a "direction" account | literature/weisberg2009/README.md:47; CP:333, 863-864 | VERIFIED | p.271: "the first terms set up in most subjects a *direction*". |
| A30 | "HE collapse to one scalar, Asch collects many of one kind" | RL:543-544 | VWC | Asch collects several kinds (see A28), for example the Table 8 rankings that RL:591 itself uses. |
| A31 | Bibliography: J. Abnormal and Social Psychology 41, 258–290, 1946, title | bibliography.bib:65-73; bibitems QA-doc:40-41, two_horn_motivation.tex:39-40 | VERIFIED | Scan: running heads and pages 258–290; the title matches; the footnote dates the start of the study to 1943. |
| A31b | bib `number = {3}` | bibliography.bib:71 | NOT-CHECKABLE | The scan has no issue number. |
| A32 | "Sequence-dependence has been of interest to empirical psychology at least since Asch1946" | MS:254 | VERIFIED | Experiments VI–VIII (pp.270–275) are explicitly about order. |

---

## 3. Verdict counts

| Verdict | HE | Asch | Total |
|---|---|---|---|
| VERIFIED | 10 | 17 | 27 |
| VERIFIED-WITH-CAVEAT | 9 | 10 | 19 |
| CONTRADICTED | 4 | 3 | 7 |
| UNSUPPORTED | 1 | 1 | 2 |
| WRONG-LOCATION | 0 | 1 | 1 |
| NOT-CHECKABLE | 1 | 1 | 2 |
| MISQUOTED / WRONG-NUMBER | 0 | 0 | 0 |

Every Table 7 number the project quotes is correct and in the correct column. Every verbatim quotation is accurate apart from A18's page range and capital letter. There are no misquotes and no wrong numbers.

---

## 4. The most important problems

1. **The direction of HE's partial adjustment is reversed (H15, H16, H22).** Several passages say partial adjustment "in the manner of Hogarth and Einhorn" protects the first impression, or weights the later cue less: PD:164-166, the proposed 6.A text at CP:1300-1303, and IO:62-65. HE prove the opposite: under SbS processing with R = S_{k−1}, their model "always predicts recency" (Appendix B; T-p.11). In HE, primacy comes only from EoS anchoring on the first item, or from α, β declining over long series. The project's one-sided scheme (first cue adopted in full, only the second damped) is what produces "first impression protected". That scheme is the project's own and should not carry HE's name. Citing Asch for down-weighting the later cue is also wrong: Asch explicitly rejects "sheer temporal position" (p.272).
2. **"Primacy in their model comes from decaying weights over a long series" leaves out HE's main primacy mechanism (H7).** HE's main mechanism is the EoS "force toward primacy" (Eq. 8, Table 2 row 1), which they match to 19 of 27 short simple EoS studies. This sentence appears in AL:18-20 and CP:447-448, 787-788 and 1266.
3. **"The ω = 0 endpoint is ours, not theirs" is overstated (H8).** HE's weight lies on [0, 1] inclusive. Their Fig. 7 names the α = β = 0 "insensitive" corner and the "advocate" who ignores negative evidence. Appendix C discusses evidence "completely ignored". Asch (p.273) reports subjects who "completely excluded" the late trait. Only the asymmetric construction is new.
4. **"Their model" is not HE's model (H13, H14).** The damped family uses a constant weight, damps only the second cue, and drops the contrast assumption (Eqs. 6a/6b) that HE call "critical". HE themselves attribute the constant-weight R = S_{k−1} model to Anderson & Hovland (1957). "The rival is their model, not a strawman" (RL:723) overstates this.
5. **The account of Asch's design has errors (A27, A13, A28, A11).**
   * The proposed 6.A text says Asch shows order moving marginals "one cue per trait across eighteen traits". That is false: there are 6 cues and 18 response items, and they do not overlap.
   * RL:539 and CP:810 say association "cannot be formed from his data". Each subject's check-list is a full 18-item joint response, and Asch himself conditions on the warm/cold item in Experiment II (p.265). What is missing is a subject's credence about co-occurrence, and any prior.
   * QA:64 still says Asch elicits "a single evaluative level", which the project's own review log (RL:522) has already called wrong.
   * The check-list is a forced choice between opposites, not "does X fit?".
6. **Smaller points.**
   * "Asch reports that early terms dominate" (A24) needs Asch's own qualifications: 10 of 24 subjects reported no change, some subjects' impressions were undone by the final term, footnote 5 says primacy can be abolished or reversed, and the effect is "not the sheer temporal position".
   * Table 8's "6th vs 1st" (A23) are modal ranks, with 39% and 29% of subjects.
   * The p.272 quote is labelled "pp.272-273" (A18).
   * HE were verified against a LaTeX transcription with reconstructed equations, not the journal article. Journal page numbers, the issue number and the DOI cannot be checked.
