# Verification of claims about the probability-kinematics papers

Repo: `/home/anuragr/development/git/research/papers/subjective_racism/theory_and_decisions/jeffrey_vs_bayes_micromacro` (read-only; nothing edited).
Abbreviations for locations: `MS` = PAPER_B_MANUSCRIPT.tex, `bib` = bibliography.bib, `DZ-R`, `F-R`, `G-R`, `W2-R`, `W3-R`, `PW-R` = literature/<paper>/README.md, `dial` = notes/papers_dialectic.tex, `2h` = notes/two_horn_motivation_body.tex, `plan` = notes/manuscript_change_plan.md, `QA` = notes/question_and_answer.tex, `QAd` = notes/question_and_answer_doc.tex, `IO` = notes/interior_omega.tex, `rev` = notes/paper_review_log.md.
Independent recomputation script: `scratchpad/indep.py` (my own implementation, not the repo's). I also ran all six repo scripts under `literature/*/sympy/` (`python3 -B`, nothing written to the repo).
Sources with no claims about these six papers: `literature/measurement_susceptibility_survey.md`, every `lean/**/*.lean` (the only "field" hits are the `field_simp` tactic), `lean/README.md`.

---

## 1. Diaconis & Zabell (1982), JASA 77(380) 822–830

(a) I read all 10 PDF pages: a JSTOR cover plus journal pp. 822–830. Every page was rendered, because the two-column text layer is badly interleaved. Journal page numbers are used below.

(b)

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| DZ1 | "Jeffrey updating is the unique coherent revision for credence delivered on cue's partition" (D–Z) | MS:117; dial:64–65; 2h:24; plan:381, 404–405 | **UNSUPPORTED** | D–Z never claim this. They list four legitimate routes (complete reassessment, retrospective conditioning, exchangeability, Jeffrey) and say the rule "is valid whenever" (J) holds (pp. 822–823). "Coherence" in D–Z §4.1 means that P* extends to a measure, which is a different thing. Their only uniqueness results are metric ones (Thm 5.1, Thm 6.1). Uniqueness-by-coherence is a Dutch-book result (Armendt 1980, Skyrms), which D–Z only list in the bibliography. |
| DZ2 | Jeffrey is "equivalent to making the minimal change of the prior consistent with the delivered marginal" | MS:117; dial:65; 2h:24; plan:382, 406; DZ-R:5 | **VERIFIED-WITH-CAVEAT** | Thm 5.1, p. 828: for the Hellinger distance (5.5) and KL (5.6), "equality holds if and only if Q(A) = Σ P(A\|Eᵢ)P*(Eᵢ)". Remark (a): Jeffrey minimises the variation distance "it does not do so uniquely". So "equivalent" holds only for strictly convex f (Thm 6.1, p. 829), not for every notion of minimal change. |
| DZ3 | Rule is "the minimal revision of the prior consistent with the delivered marginal, in the I-divergence sense"; "moves the prior least in I-divergence" | MS:193–195; 2h:25; plan:670–671, 778 | **VERIFIED** | Thm 5.1 (5.6), p. 828: I(Q,P) ≥ Σ P*(Eᵢ) log(P*(Eᵢ)/P(Eᵢ)), with equality iff Q is Jeffrey. I(Q,P)=Σ Q log(Q/P) (5.3), which is posterior-relative-to-prior, so the direction matches. (MS:191 calls this "axiomatic grounding". D–Z call §5 *mechanical updating* and do not axiomatise.) |
| DZ4 | "The partition satisfying the invariance condition is the minimal sufficient statistic for revising the prior to any candidate posterior on that partition" | MS:190–193 | **MISQUOTED** | D–Z p. 824: finding a partition with P(A\|Eᵢ)=P*(A\|Eᵢ) "is simply the problem of finding a *sufficient* partition for the two-element family {P,P*}". "A coarsest sufficient partition is said to be minimal sufficient." Thm 2.2 identifies it as the likelihood-ratio level-set partition {E_x: P*(ω)/P(ω)=x}. Many partitions satisfy (J): every refinement does, including the atom partition. So "the" partition is not unique and is not in general minimal, and sufficiency is relative to one pair {P,P*}, not "any candidate posterior". In the 2×2 binary case the cue partition is minimal only when q ≠ P(A), so the statement fails when the posterior equals the prior marginal. |
| DZ5 | D–Z "characterise how two sequences may lead to the same belief" / "characterised exactly when the two sequences lead to the same belief" / "give the condition" / the no-disturbance condition "known since D–Z" | MS:78; plan:172–173, 650–651; QA:19–21 | **VERIFIED** | Thm 3.1 (p. 825): (3.3) ⇒ P_𝓔𝓕 = P_𝓕𝓔. Thm 3.2 (p. 825): P_𝓔𝓕 = P_𝓕𝓔 iff 𝓔,𝓕 are Jeffrey independent, i.e. P_𝓔(F_j)=P(F_j) and P_𝓕(Eᵢ)=P(Eᵢ). The joint belief over both partitions is used throughout §§3–4. |
| DZ6 | D–Z's (and Hawthorne's) "focus remains on treating order dependence as a defect to repair -- the amnestic objection…" | MS:78–79 | **CONTRADICTED** (for D–Z) | p. 827, Remark 1: "There is no reason to require P_𝓔𝓕 = P_𝓕𝓔 for successive updating to be useful and valid." Remark 2: "Thus noncommutativity is not a real problem for successive Jeffrey updating." D–Z nowhere raise an amnestic objection. That objection is Hawthorne's. |
| DZ7 | Statement of Thm 3.2; J-independence strictly weaker than P-independence (Ex 3.3) | DZ-R:12–18 | **VERIFIED** | Thm 3.2 as quoted in DZ5. Thm 3.3: P-independence ⇔ J-independence for *all* p,q. Ex 3.3 ("J independence ≠> P independence", p. 826): I recomputed R = [[1,1,1],[1,0,2],[1,2,0]] from their table, and pR = 1 holds for every p=(p,(1−p)/2,(1−p)/2). |
| DZ8 | Ex 3.4 numbers: prior 1/8, 1/4, 3/8, 1/4; p₁=1/2, q₁=7/15; E-then-F gives margin(E)=1/2 exactly; F-then-E gives margin(F)=371/851 ≠ 7/15 | DZ-R:19–22, 29–32; `check_theorem32.py`:12–19 | **VERIFIED** | Table and targets p. 826: E row 1/8, 1/4; Ē row 3/8, 1/4; p₁=p₂=1/2; q₁=7/15, q₂=8/15. D–Z: "P_𝓔𝓕(E)=1/2=P_𝓔𝓕(Ē), but P_𝓕𝓔(F) ≠ q₁". My own code gives P_EF(E)=1/2, P_EF(F)=7/15, P_FE(E)=1/2, P_FE(F)=371/851. D–Z do not print 371/851. |
| DZ9 | c=0 is "exactly the Jeffrey-independence condition" for the 2×2 two-cue prior; witness −1032/369935 | DZ-R:33–37 | **VERIFIED-WITH-CAVEAT** | Supported by the unnumbered Remark on p. 826: if one partition has two elements, J-independence for some (p,q) ⇔ P-independence. This excludes trivial targets (q = prior marginal). The README does not cite this Remark. I independently reproduced the witness −1032/369935. It is a Paper-B computation, not a number from D–Z. |
| DZ10 | "Theorem 3.2 itself is … proved via Csiszár's I-projection result" | DZ-R:39–41 | **CONTRADICTED** | Thm 3.2 is proved by direct algebra, eqs (3.5)–(3.6), pp. 825–826. Csiszár (1975, Thm 3.2) is the omitted proof of **Thm 3.1**: "Theorem 3.1 is an immediate consequence of Csiszár (1975, Theorem 3.2) and its proof is omitted" (p. 825). |
| DZ11 | Ex 3.4 is a precedent for "pinning": the last-updated marginal is held exactly and the earlier one may drift | DZ-R:20–22, 47–50 | **VERIFIED** | p. 825, §3.1: "Clearly, the order of updating matters, since the second opinion dominates." Ex 3.4 as in DZ8. |
| DZ12 | §3.1: successive steps are Jeffrey steps to targets; the second stage dominates | DZ-R:60–62 | **VERIFIED** | p. 825, quoted in DZ11. |
| DZ13 | §4.2 attributes the successive route to Jeffrey (1957, Ch. 4) and asks as an open question "when is successive updating reasonable?" | DZ-R:62–64 | **VERIFIED-WITH-CAVEAT** | p. 827: "Richard Jeffrey (1957, Ch. 4) has advocated another route from (4.1) …: successive Jeffrey updating on 𝓔 and 𝓕. This raises two issues: … 2. When is successive updating reasonable?" It is posed as an issue, and D–Z then offer "One approach to this is via checking the Jeffrey condition at each stage", so it is not left wholly open. |
| DZ14 | Remark 2: non-commutativity "is not a real problem", because the two sequences cannot both be acceptable incorporations of the same targets | DZ-R:64–66 | **VERIFIED** | p. 827: "Condition (4.3) implies that P_𝓔𝓕 and P_𝓕𝓔 cannot both incorporate (4.1) and both be judged acceptable updates … without P_𝓔𝓕 = P_𝓕𝓔. Thus noncommutativity is not a real problem for successive Jeffrey updating." |
| DZ15 | §3.2 quote "The J condition is an internal or psychological condition that must be checked or accepted at each stage. Mathematics has nothing to offer here." Also "leave the acceptability of each step to psychology". | DZ-R:67–70; plan:354–355, 416–418 | **VERIFIED** | Verbatim, p. 825, §3.2, immediately after (3.2). |
| DZ16 | §4 simultaneous adoption with existence by Strassen; §5.2 I-projection by IPFP; Ex 5.1 "I projections preserve the association factor", Mosteller (1968) | DZ-R:71–75 | **VERIFIED-WITH-CAVEAT** | Thm 4.1, p. 827: "Theorem 11 of Strassen (1965)". §5.2, p. 828: IPFP converges to the I-projection (Csiszár 1975, Thm 3.2). p. 829, verbatim: "I projections preserve the association factor of a 2 × 2 table (see, e.g., Mosteller 1968, p. 3)". **But Example 5.1 is in §5.3 "Comparing Different Metrics" (pp. 828–829), not §5.2.** Recomputed: Pᴵ=(1/9, 2/9, 2/9, 4/9) has odds ratio 1 and ‖Pᴵ−P⁰‖=7/36; Pⱽ=(1/12, 1/4, 1/4, 5/12) with distance 1/6. Both match D–Z. |
| DZ17 | Full adoption is "D–Z's setting"; "two rigid steps on correlated partitions do not commute (D–Z)" | IO:69, 111–113, 353–355, 435; plan:354, 443 | **VERIFIED** | §3.1 (p. 825) sets each step to the target. Thm 3.2 plus the Remark on p. 826 give non-commutation for 2-cell partitions unless P-independent. |
| DZ18 | "The literature holds only the second view (Diaconis–Zabell)", i.e. amnestic updating protects the last impression | plan:1276–1277 | **VERIFIED-WITH-CAVEAT** | D–Z describe successive updating (§3.1) but do not "hold" it as a view. They also present simultaneous adoption and the I-projection (§§4–5), which protect neither cue, and they deny that commutativity is required (Remarks 1–2). DZ-R:77–79 itself says "setting, not premise". |
| DZ19 | Hawthorne's note cites D–Z "for how ubiquitous the effect is" | dial:101–102 | **NOT-CHECKABLE** | This is a claim about Hawthorne's note 15. Hawthorne is not in my set. D–Z themselves only say "in general the order matters" (p. 825) and do not speak of ubiquity. |
| DZ20 | README: D–Z is cited in the manuscript "footnote at line ~81" | DZ-R:5–6 | **WRONG-LOCATION** | Stale pointer. D–Z is now cited in body text at MS:117 and MS:193–195, not in a footnote. |
| DZ21 | Bibliographic entry: JASA 77(380), 822–830, 1982 | bib:11–20; .bbl:75–81; QAd:42–43; dial:196–199 | **VERIFIED-WITH-CAVEAT** | JSTOR cover: "Vol. 77, No. 380 (Dec., 1982), pp. 822-830". The DOI 10.1080/01621459.1982.10477893 does not appear on the copy and cannot be checked from it. |

(c) Most important problems:
1. **MS:117**, repeated in dial, 2h and plan, attributes to D–Z a claim they do not make: that Jeffrey updating is "the unique coherent revision". A referee who knows the paper will spot this. D–Z support only the metric-minimality clause, uniquely for KL and Hellinger and not for variation distance.
2. **MS:190–193** misstates Thm 2.2. D–Z say a (J)-partition is *sufficient*, and the minimal sufficient partition is the likelihood-ratio partition. "The partition satisfying the invariance condition" is not unique, and "any candidate posterior" is wrong. Calling this "axiomatic grounding" also mislabels what D–Z call mechanical updating.
3. **MS:78–79** implies D–Z treat order dependence "as a defect to repair". Their Remarks 1–2 (p. 827) say the opposite.
4. DZ-R:39–41 credits Thm 3.2's proof to Csiszár. That proof is direct algebra; Csiszár is for Thm 3.1.

---

## 2. Field (1978), Phil. Sci. 45(3) 361–367

(a) I read all 8 PDF pages: a JSTOR cover plus journal pp. 361–367. I checked every equation on the rendered pages.

(b)

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| F1 | Footnote: "Field1978 for how Jeffrey updates are asymmetric in the delivered-credence parametrisation" | MS:117 fn; 2h:26–27; plan:384–385, 407–409 | **VERIFIED** | p. 365: expressing two successive changes "in terms of the Jeffrey parameters q and q′ … we get a very complicated law; moreover, it is an *asymmetric* law, in that the simultaneous interchange of E with E′ and q with q′ very much affects the result." |
| F2 | Eq. (4) α = ½ log((q/p)/((1−q)/(1−p))) inverts eq. (5) q = pe^α/(pe^α+(1−p)e^−α) | F-R:7–8; `check_commutativity.py`:5–14 | **VERIFIED** | p. 364, (4) and (5), transcribed correctly. The inversion holds. The repo script prints `-alpha + log(exp(2*alpha))/2` rather than 0, because α is not declared real. It is still zero for real α. |
| F3a | Eq. (7): the tilt update applied E-then-E′ equals E′-then-E, closed form ∝ e^{±α±α′} | F-R:9–12; dial:71–73; plan:689–690 | **VERIFIED** | p. 366, eq. (7) has exactly the four e^{±α±α′} weights, "both simple and symmetric". My own implementation gives an identically zero difference in all four cells. |
| F3b | "All verified in `sympy/check_commutativity.py` … the tilt update commutes exactly (both directions match the closed form identically)" | F-R:18–20 | **CONTRADICTED** (by the repo's own script) | Bug: `tilt_step1(F, al)` ignores its argument `F` and re-tilts the global prior symbols. So `route_EpE` is just the E-tilt of the prior. When run, the script prints **non-zero** "diff" expressions for all four cells. At F=(.1,.2,.3,.4), α=1/3, α′=1/2 they are ≈ (0.096, −0.121, 0.148, −0.124). Only the E-then-E′ route is compared to the closed form; E′-then-E never is. |
| F4 | Field's unformalized assertion that the same updates in raw credences q,q′ do not commute; witness 651/28120 | F-R:13–14, 20 | **VERIFIED** | p. 365, quoted in F1. My own code reproduces the credence-input gap of 651/28120 at the stated point. |
| F5 | "Field's α is a log-odds shift (symmetric around 0, combines additively in the exponent)" | F-R:33–34 | **VERIFIED-WITH-CAVEAT** | By (4), α is **half** the log-odds shift. Field, p. 364: "(The (1/2) log is of course just in there for reasons of scale…)". The additive combination is in (7) and (6′). |
| F6 | "his e^{2α} is a squared likelihood ratio" | F-R:35–36 | **CONTRADICTED** | From (4), e^{2α} = (q/p)/((1−q)/(1−p)). That is the Bayes factor / likelihood ratio itself: Wagner 2002 (1.1); P-W p. 9 "β just is the Bayes factor"; Paper B's ℓ₁/ℓ₀. Sympy: e^{2α} − BF = 0. So e^{α} is the *square root* of the likelihood ratio, and e^{2α} is not its square. |
| F7 | Field "reparametrised the update so that the input is a portable factor rather than a delivered credence, and sequential revisions then commute" | dial:71–73; plan:689–690 | **VERIFIED** | pp. 363–366. α is to be "a function of sensory stimulation alone" (p. 364), and (7) is symmetric. |
| F8 | The sequence-free value "is precisely the Bayes-factor rule of Field1978 and Wagner2002"; "the commutative horn: Field's α …" | QA:96–98; IO:371 | **VERIFIED-WITH-CAVEAT** | This holds when each α is computed from the cue against the **prior** marginal, so that the second α is not recomputed against the intermediate belief. Field never uses the words "Bayes factor". |
| F9 | Field's exponential tilt is "the same object as Paper B's PB applied sequentially"; "PB genuinely is Field's 1978 procedure" | F-R:27–31; PW-R:49–51 | **VERIFIED-WITH-CAVEAT** | Same condition as F8. With α_A, α_B fixed from (q, P(A)) and (r, P(B)), sequential (6)/(7) gives P·e^{±α}e^{±α′} normalised, which equals PB. |
| F10 | Bibliographic: Phil. Sci. 45(3), 361–367 | bib:33–41; .bbl:104–108; QAd:44; dial:204–206 | **VERIFIED** | JSTOR cover: "Vol. 45, No. 3 (Sep., 1978), pp. 361-367". |

(c) Most important problems:
1. F-R:35–36 is wrong: e^{2α} *is* the likelihood ratio (Bayes factor), not its square. If this goes into the manuscript it inverts the scaling between Field and PB.
2. `literature/field1978/sympy/check_commutativity.py` is buggy (`tilt_step1` ignores its input). Its commutativity output is non-zero, yet the README reports "verified … both directions match the closed form identically". The mathematical claim is true (I checked it independently), but the repo's verification does not show it.
3. Field has only two footnotes. The "footnote 9" gloss that PW-R attributes to Field comes from Pettigrew–Weisberg (see PW7).

---

## 3. Garber (1980), Phil. Sci. 47(1) 142–145

(a) I read all 5 PDF pages: a JSTOR cover plus journal pp. 142–145. All pages were rendered.

(b)

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| G1 | α = .2209 for p₀=.3 → q₀=.4 | G-R:19–21; `check_repeated_glances.py`:3–5 | **VERIFIED** | p. 143: "has an α value of .2209 (to four places)". My recomputation gives α=0.2209164 (e^{2α}=14/9). |
| G2 | Sequence .3, .4, .5091, .6173, .7150, .7961, .8586, .9043, .9363, .9581 | G-R:20–21, 28–29 | **VERIFIED** | Table, p. 144, identical. I recomputed all ten values (odds × 14/9 per look) and they agree to 4 dp. Note: Garber's own prose on p. 143 says the second value "will be .5019". That is a typo in the paper, since the table and the computation give .5091. The README does not mention it. |
| G3 | The "slightly richer" experience (.3 → .5) exceeds .95 after five repetitions | G-R:22–23, 29–30 | **VERIFIED** | p. 144: "only *five* repetitions … above .95". Recomputed: .3, .5, .7, .8448, .9270, **.9674**. After four repetitions the value is .9270 < .95, so five is the first to exceed .95. |
| G4 | Garber "concludes Field's repair is 'neither correct nor necessary'" | G-R:44 | **VERIFIED-WITH-CAVEAT** | The phrase is verbatim, but it is the opening thesis on p. 142 ("I shall argue that Field's proposed revision of Jeffrey's formula is neither correct nor necessary"), not the conclusion. |
| G5 | Deeper task quoted as "far more interesting and far more difficult" | G-R:46–47 | **MISQUOTED** (minor) | p. 145: "the far more interesting (and far more difficult) task of finding an alternative way of characterizing rational belief change". The parentheses were dropped. |
| G6 | The dilemma: if P₁(E) is independent of P₀(E), (1)/(2) apply and no reparametrisation is needed; if not, conditionalisation may be inappropriate | G-R:39–44 | **VERIFIED** | p. 145, near-verbatim paraphrase. |
| G7 | Garber "never discusses order" | plan:1336–1337 | **VERIFIED** | The paper contains no discussion of commutativity or order of updates, only repetition of the same experience on the same E. |
| G8 | "Garber1980 objects that a portable factor compounds implausibly under repetition"; "compounds absurdly" | plan:692–693; dial:82–83 | **VERIFIED** | p. 144: "after *nine* repetitions of the *same* rather uninformative experience, S will become *virtually certain*"; "practical certainty in E is much too easily obtained". "Absurdly" is a fair gloss. |
| G9 | "this particular Drive scan has a clean text layer throughout -- no image rendering was needed" | G-R:5–8 | **VERIFIED-WITH-CAVEAT** | The numbers are clean, but the text layer garbles the equations: `garber.txt`:99 reads "(3) q = (pea)/(pex + (1 -p)e- )", :106 reads "(4) a =- log ((q/p)/((l - q)/( - p)))", and subscripts are lost ("P (E) = .3"). So the handover's OCR warning was partly right. |
| G10 | "This is the counterexample Hawthorne answers" | G-R:51 | **NOT-CHECKABLE** | Hawthorne is not in my set. (Wagner 2002 Remark 5.1, p. 10, does explicitly answer Garber: "all Bayes factors beyond the first are equal to one".) |
| G11 | Bib entry: Phil. Sci. 47(1), 142–145, "Field and Jeffrey Conditionalization" | plan:1428–1435 | **VERIFIED** | JSTOR cover: "Vol. 47, No. 1 (Mar., 1980), pp. 142-145". The printed title is prefixed "DISCUSSION:". |

(c) Most important problems: none substantive. The numbers are exact. The plan's framing (Garber targets portable factors under repetition, and never discusses order) is correct. Minor: the dropped parentheses in the quote, and "concludes" should be "argues/announces".

---

## 4. Wagner (2002), Phil. Sci. 69(2) 266–278 (preprint copy)

(a) I read all 14 pages of the preprint, which has its own pagination 1–14 and no journal page numbers. Every page was rendered. Primes in (3.2)/(3.3) were checked on rendered p. 4.

(b)

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| W1 | Abstract quotes "has caused much unjustified concern" and "when identical learning is properly represented, namely, by identical Bayes factors rather than identical posterior probabilities, then sequential probability-kinematical revisions behave just as they should" | dial:74–78 | **VERIFIED** | Abstract, p. 1, verbatim. The second sentence is repeated in §1. |
| W2 | Thm 3.1: (3.2) β_{r′,q′}(E_{i1}:E_{i2}) = β_{q,p}(E_{i1}:E_{i2}), (3.3) β_{q′,p}(F_{j1}:F_{j2}) = β_{r,q}(F_{j1}:F_{j2}) ⇒ r′=r; pdftotext drops the primes | W2-R:17–19, 26–37 | **VERIFIED** | Rendered p. 4 shows both primes in (3.2) and (3.3) as stated. `wagner2002.txt`:147 reads "βr ,q (Ei1 : Ei2 ) = βq,p", which confirms that the primes were dropped. |
| W3 | Thm 4.1 necessity under (4.3)–(4.4), "which full support trivially satisfies"; "Wagner's own contribution beyond Field/Diaconis–Zabell" | W2-R:20–22 | **VERIFIED-WITH-CAVEAT** | p. 7: (4.3) ∀i₁∀i₂∃j p(E_{i1}F_j)p(E_{i2}F_j)>0, and (4.4) likewise; "If r′=r, then … (3.2) and (3.3) hold." Remark 4.1 (p. 8) says these hold when E,F are *qualitatively independent* and p is strictly coherent. Full support alone is not enough, for example when F=E. Remark 4.3: D–Z already proved necessity (of J-independence) in the matched case (2.7), which Thm 4.1 generalises. |
| W4 | Jeffrey (1988) commutativity is derived as a corollary in Wagner 2002 Remark 3.3 | dial:73–74, 218–221 | **VERIFIED** | p. 5, Remark 3.3: probability-factor identities (3.13)–(3.14) imply r′=r; "Jeffrey's result is a corollary of Theorem 3.1". |
| W5 | Footnote: Jeffrey updates "commute when their inputs are held fixed in Bayes-factor form" | MS:117 fn; 2h:28–29; plan:385–386, 409–410 | **VERIFIED** | Thm 3.1, p. 4; §5 Principle II modified, p. 9. |
| W6 | "Combining the two cues' Bayes-factor content is sequence-invariant (Wagner2002)"; PB is "a sequence-free reference (Wagner2002)"; "commutes by construction" | MS:216–217; MS:171; 2h:30–31; `sympy/jeffrey_core.py`:86 | **VERIFIED** | (3.7)/(3.8), p. 5: r(A) = Σ B_i b_j p(AE_iF_j)/Σ B_i b_j p(E_iF_j). PB(i,j) ∝ P(i,j)ℓᴬᵢℓᴮⱼ is exactly this form, with B_i = β_{q,p}, b_j = β_{q′,p}. |
| W7 | "Bayes-factor updating predicts no sequence effect anywhere (Wagner2002)"; "predicts none" | MS:277; MS:117 (uncited); 2h:36; plan:823, 834 | **VERIFIED-WITH-CAVEAT** | This holds for Thm 3.1 when identical Bayes factors are used in either position. Caveats: Remark 5.3 and note 11 (pp. 10–13) show that for countable partitions the Bayes-factor-matched r may fail to exist (5.3)–(5.4). Remark 5.2: if F=E, identical Bayes factors across positions cannot hold except trivially. "Anywhere" is fine for the finite 2×2 setting. |
| W8 | "Wagner's commutation needs the same factor in either position" | IO:313, 481; `sympy/verify_interior_omega.py`:120 | **VERIFIED** | Thm 3.1 gives sufficiency. Thm 4.1 gives necessity under (4.3)–(4.4). |
| W9 | Thm 4.1 justifies calling PB "*the*" sequence-free benchmark; "Bayes-factor consistency … is the *only* route. Any two-cue schema that happens to commute, on a full-support prior, must already be Bayes-factor-consistent" | W2-R:68–72 | **UNSUPPORTED** (the "the benchmark" inference) | Necessity is Thm 4.1, subject to the W3 caveat. But Thm 4.1 does not single out factors computed against the prior, i.e. PB. The README's own check finds "a genuine one-parameter family of commuting schemas" (W2-R:55–56), each with a different common r. Thm 4.1 says any commuting schema is BF-consistent across positions, not that PB is the unique benchmark. |
| W10 | Lean covers "matching-target routes, i.e. Field's simpler special case" | W2-R:62–64 | **CONTRADICTED** | p. 4: Field proved Thm 3.1 for schema (3.1), where targets need not match, "in the special case where E and F are finite". The matched-target schema (2.7) is the Diaconis–Zabell case (Remark 3.4, p. 6). |
| W11 | README: manuscript cites Wagner "at line ~180" | W2-R:73–74 | **WRONG-LOCATION** (minor) | Stale pointer. Now MS:171, 217, 277 and the footnote at 117. |
| W12 | "generalizes Field (1978) to countable partitions" | W2-R:6–8 | **VERIFIED** | p. 4: "Field's result holds for all countable families E and F, but requires a different proof." |
| W13 | Bib: Phil. Sci. 69(2), 266–278, DOI 10.1086/341051 | bib:22–31; .bbl:175–180; dial:222–225; QAd:54 | **VERIFIED-WITH-CAVEAT** | The preprint has no front matter. Volume and pages match Wagner 2003's reference list ("Philosophy of Science 69, 266–278") and P-W's ("69(2):266–78"). The DOI and issue number cannot be checked from the copy. |
| W14 | "Drive copy is the preprint, hence quotations are located by section" | dial:223–224 | **VERIFIED** | The copy is an unpaginated preprint with only page numbers 1–14. |

(c) Most important problems:
1. W2-R:68–72 over-reads Thm 4.1: necessity does not make PB "the" benchmark, and the README's own sympy result shows a one-parameter family of commuting schemas.
2. W2-R:62–64 confuses Field's case (finite partitions, general schema) with Diaconis–Zabell's matched-target case.
3. MS:277's "no sequence effect anywhere" is fine for the paper's finite setting. Wagner's own Remarks 5.2 and 5.3 give the edge cases.

---

## 5. Wagner (2003), Erkenntnis 59(3) 349–364

(a) I read all 17 PDF pages: a JSTOR cover plus journal pp. 349–364. All pages were rendered.

(b)

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| V1 | "This is the 'considered experiences' fix the handover flags as directly load-bearing in the Lange/Cassell exchange" | W3-R:3–4 | **WRONG-LOCATION** | Wagner 2003 never mentions Lange, Cassell or "considered experience". Neither author is in the references (pp. 363–364), and `grep` of the text finds no hit. The phrase "*considered* experience (in light of ambient memory and prior probabilistic commitment)" is in **Wagner 2002, note 9** (p. 13), in a discussion of Lange (2000). |
| V2 | Thm 2.1: schema p→q→R, p→Q→r; (2.2) β_{r,Q}(A:B)=β_{q,p}(A:B), (2.3) β_{R,q}(A:B)=β_{Q,p}(A:B) for atoms ⇒ r=R; three-line proof | W3-R:8–13 | **VERIFIED-WITH-CAVEAT** | p. 351: (2.2) β^r_Q(A:B)=β^q_p(A:B); (2.3) β^R_q(A:B)=β^Q_p(A:B), ∀A,B∈A*. The proof multiplies Q-odds by the same factor. The paper says "**purely** atomic σ-algebra" (countable atoms that partition Ω), which is narrower than the README's "arbitrary atomic". Strict coherence is assumed throughout (p. 350). |
| V3 | Thm 2.1 "generalizes Wagner (2002)'s two-partition commutativity result" | W3-R:8–9 | **VERIFIED-WITH-CAVEAT** | In the paper, the 2002 result reappears as **Thm 3.2** (p. 354) for arbitrary σ-algebras and is derived from Thm 2.1 via Thm 3.1. Remark 3.2 (p. 355) calls Thm 2.1 "the fundamental result". |
| V4 | Remark 2.2, formula (2.7): r(A) ∝ π_{q,p}(A)π_{Q,p}(A)p(A) | W3-R:14–16 | **VERIFIED** | p. 351: r(A)=R(A)= [q(A)Q(A)/p(A)] / Σ_{A∈A*} q(A)Q(A)/p(A). This is algebraically identical to the README's form. The repo script also matches. |
| V5 | §5 contrasts BF with "two naive alternative indices" against criteria "(commutativity; reproducibility on any partition; not overly restricting the prior)" | W3-R:17–20 | **MISQUOTED** | p. 360: **three** indices, δ/d, the normalised difference D, and π ("D and π clearly stand or fall together"). Criterion II is that learning prompting a probability-kinematical revision "should prompt a probability-kinematical revision **on the same partition**", not "on any partition". |
| V6 | π-index "reaches the point of absurdity" at two atoms: unless Q=p, no r exists | W3-R:20–23, 35–38; `check_theorem21.py`:43–49 | **VERIFIED** | p. 361: "this phenomenon reaches the point of absurdity when A* has just two members. Then, unless Q = p, there is no r satisfying (5.1) for ν = π". Algebra in note 5 (p. 363). Recomputed: Q₁ = p₁ is the unique solution. |
| V7 | The d-index failure of criterion II ("no valid kinematical revision on a finer partition") is "a structural/qualitative point in the paper (no canonical refinement exists), not a single identity to check" | W3-R:40–42 | **CONTRADICTED** | Criterion II is about the *same* partition. Wagner gives a concrete numeric counterexample in **note 4** (p. 363): p=(.4,.1,.1,.4), q=(.64,.16,.04,.16), p′=(.2,.2,.3,.3), q′=(.44,.26,.24,.06). I checked it: q comes from p by PK on {E,Ē} (q(H\|E)=.8=p(H\|E), q(H\|Ē)=.2=p(H\|Ē)); q′−p′=q−p on all atoms; but q′(H\|E)=.6286 ≠ p′(H\|E)=.5. |
| V8 | "Sections 3-4 (applying the Uniformity Rule to the old-evidence problem …) … read but not formalized" | W3-R:44–46 | **WRONG-LOCATION** | Only §4 (pp. 356–359) is about old evidence and new explanation. §3, "Observation-based revision" (pp. 352–355), contains Thm 3.1 and **Thm 3.2**, the Field/Wagner two-partition commutativity theorem for arbitrary σ-algebras. That is the result most relevant to Paper B, and the README dismisses it. |
| V9 | The two alternatives "each fail one of three … criteria, and the ratio index fails catastrophically (vacuously, at just two atoms)" | W3-R:55–58 | **VERIFIED-WITH-CAVEAT** | p. 361: "the index d fails to satisfy II, and its satisfying I is vitiated by its failure to satisfy III. The index π satisfies both I and II, but this is vitiated by its failure to satisfy III." So d fails II and III, not just one. |
| V10 | Citation: Erkenntnis 59(3), 349–364 | W3-R:3 | **VERIFIED** | JSTOR cover: "Vol. 59, No. 3 (Nov., 2003), pp. 349-364". The printed page footer says "Erkenntnis 59: 349–364, 2003". This paper is not in bibliography.bib. |

(c) Most important problems:
1. W3-R:3–4 hangs the "considered experiences / Lange–Cassell" role on the wrong paper. That material is in Wagner 2002 note 9, and 2003 is silent on it.
2. W3-R:40–46 wrongly states that criterion II cannot be checked (note 4 gives a numeric counterexample, which I verified). It also wrongly files §3 under "old evidence", when §3 holds Thm 3.2, the directly relevant commutativity theorem.
3. W3-R:17–20 paraphrases the criteria and the number of indices wrongly.

---

## 6. Pettigrew & Weisberg (2025), "Jeffrey Pooling", Philosophers' Imprint 25(8)

(a) I read all 16 PDF pages, paginated "- 1 -" to "- 16 -" (July 2025). All pages were rendered.

(b)

| # | Claim (short) | Where | Verdict | Evidence |
|---|---|---|---|---|
| PW1 | Thm 1 is attributed to Field: upco followed by Jeffrey conditioning commutes for any regular P | PW-R:12–14 | **VERIFIED** | p. 3: "**Theorem 1** (Field). *Upco ensures that Jeffrey pooling commutes for any regular P, and any Q and R.*" "We attribute this result to Field (1978)". |
| PW2 | Thm 2: among monotonic, continuous, uniformity-preserving, symmetric rules, only upco commutes for arbitrary Q,R | PW-R:15–17 | **VERIFIED-WITH-CAVEAT** | p. 6, Theorem 2 as stated, "for any regular P, and any Q and R". The README omits "regular P". The appendix version (Thm 8, p. 15) adds **extensionality** and restricts to finite partitions, and p. 6 calls extensionality "a tacit fifth assumption". |
| PW3 | Eq. (1) upco P′(E)=P(E)Q(E)/[P(E)Q(E)+P(Ē)Q(Ē)] | PW-R:13; `check_upco_vs_PB.py`:6–8 | **VERIFIED** | p. 3, eq. (1). Recomputed upco(.4,.8)=8/11≈.727, which matches "≈ 0.73". I also recomputed the worked example on p. 4: 6/11, 2/11, 1/11, 2/11 → 18/29, 4/29, 3/29, 4/29 in both orders. |
| PW4 | Eq. (2) Field updating on (E,β): P′(E)=βP(E)/(βP(E)+P(Ē)), β ≥ 0 odds scale; equals upco with Q(E)=β/(β+1) | PW-R:18–21 | **VERIFIED** | p. 7, eqs (2) and (3): "the same as Equation (1), where Q's probabilities are Q(E)=β/(β+1) and Q(Ē)=1/(β+1)". |
| PW5 | Theorems 3/4 are attributed to Wagner 2002 | PW-R:22–23 | **VERIFIED** | pp. 8–9: "Theorem 3 (Wagner)", "Theorem 4 (Wagner)". p. 10: "consult Wagner (2002, Theorem 3.1)". |
| PW6 | Q(E)=β/(β+1)=q(p−1)/(2pq−p−q), equal to q only at p=½; upco(p,q)−q vanishes only at p=½ | PW-R:29–33, 43–45 | **VERIFIED** | Algebra: β=q(1−p)/(p(1−q)), so Q=q(1−p)/(p+q−2pq). Also upco(p,q)−q = q(2p−1)(1−q)/D. Edge cases q∈{0,1} aside, both claims are correct. |
| PW7 | "Field's own gloss (footnote 9 in the paper): β/(β+1) is what you'd defer to 'if you have no prior opinion'" | PW-R:45–47 | **WRONG-LOCATION** (and misattributed) | Field (1978) has only two footnotes and never writes β/(β+1). P-W footnote 9 (p. 6) says only "Field actually uses a log scaled version of β, which he labels α … We've removed these scaling features". The "no prior opinion" gloss is P-W's own main text on p. 7: "when P(E)=P(Ē), Equation (3) delivers P′(E)=β/(β+1). So if you have no prior opinion about E, you will defer to your sensory system's proposal". |
| PW8 | "the literature restores sequence-invariance for sequential Jeffrey updating by changing how successive inputs are pooled (PettigrewWeisberg2025)" | MS:277; rev:120–121 | **VERIFIED-WITH-CAVEAT** | P-W restore commutativity by choosing the pooling rule (upco) that combines the **prior** P(E) with each source's opinion Q(E), before the Jeffrey step (pp. 3–6). Successive inputs are not pooled with each other. The phrase "how successive inputs are pooled" is loose but not wrong in spirit. P-W also stress that they are not claiming upco is always best (p. 6). |
| PW9 | "PB genuinely is Field's 1978 procedure … and Field's procedure is, by PW's own algebra, the same construction as upco -- but only once translated through β" | PW-R:49–58 | **VERIFIED-WITH-CAVEAT** | P-W p. 7: "Field updating is the same thing as Jeffrey pooling with upco". The PB ≡ Field identity needs each β computed against the prior (see F8/F9). |
| PW10 | Bib: Philosophers' Imprint 25(8), pp. 1–16, 2025, doi 10.3998/phimp.3806 | bib:134–143; .bbl:151–157 | **VERIFIED** | p. 1 masthead: "volume 25, no. 8, july 2025", "doi.org/10.3998/phimp.3806". Page footers run from "- 1 -" to "- 16 -". |
| PW11 | P-W listed as "(preprint / forthcoming)" | QAd:52–53 | **CONTRADICTED** (stale) | Published: Philosophers' Imprint 25(8), July 2025. |

(c) Most important problems:
1. PW-R:45–47 attributes the β/(β+1) "no prior opinion" gloss to "Field's own … footnote 9". It is P-W's main-text gloss (p. 7), P-W's footnote 9 says something else, and Field never writes β/(β+1). The task brief repeats this README error.
2. MS:277 compresses P-W's mechanism. They pool the prior with each source's opinion using upco, rather than pooling successive inputs with one another.

---

## Cross-paper summary

Verdict counts (78 claims):

| Verdict | DZ | Field | Garber | W2002 | W2003 | P-W | Total |
|---|---|---|---|---|---|---|---|
| VERIFIED | 9 | 6 | 7 | 8 | 3 | 6 | **39** |
| VERIFIED-WITH-CAVEAT | 6 | 3 | 2 | 3 | 3 | 3 | **20** |
| MISQUOTED | 1 | 0 | 1 | 0 | 1 | 0 | **3** |
| WRONG-LOCATION | 1 | 0 | 0 | 1 | 2 | 1 | **5** |
| WRONG-NUMBER | 0 | 0 | 0 | 0 | 0 | 0 | **0** |
| UNSUPPORTED | 1 | 0 | 0 | 1 | 0 | 0 | **2** |
| CONTRADICTED | 2 | 2 | 0 | 1 | 1 | 1 | **7** |
| NOT-CHECKABLE | 1 | 0 | 1 | 0 | 0 | 0 | **2** |
| total | 21 | 11 | 11 | 14 | 10 | 11 | **78** |

All numerical claims check out: Garber's α and both sequences, D–Z Ex 3.4 (371/851), both Paper-B witnesses (−1032/369935 and 651/28120), P-W's Q(E) formula and worked example, and Wagner 2003's π-absurdity. There are no WRONG-NUMBER findings. The problems are attributional and interpretive, and they concentrate in two places: the manuscript's D–Z sentences (MS:78, 117, 190–193), and the literature READMEs (Field e^{2α}, the Field script bug, the Wagner 2003 mis-filings, and P-W footnote 9).
