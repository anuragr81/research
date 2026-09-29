# Verification of the research record against six source papers

Repo: `/home/anuragr/development/git/research/papers/subjective_racism/theory_and_decisions/jeffrey_vs_bayes_micromacro` (read-only; nothing edited).
Papers: `scratchpad/papers/*.pdf` (+ `.txt`). Rendered pages were checked where tables, formulas or notation mattered (AR pp. 6, 9-11, 20-21; SY pp. 11-14 of the PDF).
Scope of claims: every hit of `Augenblick|Rabin|Shmaya|Yariv|Zhao|Osherson|Crupi|Wilson|Cassell|Lange` in the files you listed, plus the Lean and SymPy sources. I confirmed that PAPER_B_MANUSCRIPT.tex, bibliography.bib, notes/positioning_economics.tex and notes/question_and_answer*.tex contain **0** hits, so all the claims are in notes/paper_review_log.md, notes/manuscript_change_plan.md, notes/interior_omega.tex, literature/*/README.md, literature/measurement_susceptibility_survey.md, and the Lean and SymPy files.

I also ran:
- `check_prop1.py`: all checks pass.
- `check_theorems.py`: all checks pass.
- `#print axioms` on `prop1_of_orthogonal`, `resolving`, `excess_step`, `anythingGoes` and `no_reversal_of_restricted`, via `lake env lean` on a scratch file. Each one gives `[propext, Classical.choice, Quot.sound]`.

Verdict key: VERIFIED, VERIFIED-WITH-CAVEAT (VWC), MISQUOTED, WRONG-LOCATION, WRONG-NUMBER, UNSUPPORTED, CONTRADICTED, NOT-CHECKABLE (NC).

Abbreviations used in the "where" column:
- `ARR` = literature/augenblick_rabin2021/README.md
- `ARL` = lean/Literature/AugenblickRabin.lean
- `ARpy` = literature/augenblick_rabin2021/sympy/check_prop1.py
- `SYR` = literature/shmaya_yariv2016/README.md
- `SYL` = lean/Literature/ShmayaYariv.lean
- `SYpy` = literature/shmaya_yariv2016/sympy/check_theorems.py
- `LOG` = notes/paper_review_log.md
- `PLAN` = notes/manuscript_change_plan.md
- `SURV` = literature/measurement_susceptibility_survey.md
- `IO` = notes/interior_omega.tex
- `W03R` = literature/wagner2003/README.md
- `WBR` = literature/weisberg2009/README.md

---

## 1. Augenblick & Rabin, "Belief Movement, Uncertainty Reduction, & Rational Updating" (Drive copy: working paper dated November 2020)

(a) I read all 54 PDF pages in the text extraction: the main text pp. 1-41 in full and the Appendix proofs pp. 41-54 (the proofs of Props 7-9 were read at a skim). All page numbers below are the paper's printed page numbers.

**Notation warning that applies throughout.** The paper writes beliefs as **π** (π_t; belief stream π(H_t)), not θ. The project writes θ everywhere and calls its definitions "verbatim".

| # | claim (short) | where | verdict | evidence |
|---|---|---|---|---|
| 1 | The Drive copy is the Nov 2020 working paper | ARR:3-4 | VERIFIED | Title page: "November 2020". |
| 2 | Cited as QJE 136(2), 933-985 | ARR:3, ARL:4 | NC | The copy is the working paper, so the journal citation can't be checked here. All Prop/Cor numbers the project uses are the working paper's, and they may differ from the QJE numbering. |
| 3 | Sec. 2 definitions: u_t=(1-θ_t)θ_t; m_{t1,t2}=Σ(θ_{τ+1}-θ_τ)²; r_{t1,t2}=Σ(u_τ-u_{τ+1})=u_{t1}-u_{t2} | ARR:8-13 | VWC | p.9 matches exactly, but in π: "m_{t1,t2}(π) ≡ Σ_{τ=t1}^{t2-1}(π_{τ+1}-π_τ)²", "u_t(π) ≡ (1-π_t)π_t", "r_{t1,t2}(π) ≡ Σ(u_τ(π)-u_{τ+1}(π)) = u_{t1}(π)-u_{t2}(π)". The paper defines these for t2 > t1. |
| 4 | Lean docstring says "Definitions (their Section 2, verbatim)" and uses θ | ARL:9-13 | MISQUOTED | The maths is right but it is not verbatim: the paper's symbol is π throughout (p.9). |
| 5 | Prop 1: for any DGP and any t1,t2, EM=ER | ARR:15, ARL:15 | VERIFIED | p.10: "Proposition 1 (Implication of Martingale Property for Beliefs) For any DGP and for any periods t1 and t2 … EM_{t1,t2} = ER_{t1,t2}." |
| 6 | The one-period rewriting EM-ER = E[(2θ_t-1)(θ_t-θ_{t+1})] is displayed in the proof | ARL:17-19, ARR:29-30, LOG:77-79 | VERIFIED | Displayed on p.10, straight after Prop 1: "The proof is straightforward and instructive (and, as with all proofs, is in the Appendix). First, it is possible to rewrite the one-period difference as: 𝔼M_{t,t+1} − 𝔼R_{t,t+1} = 𝔼[(2π_t − 1)·(π_t − π_{t+1})]". It is also line 3 of the formal Appendix proof on p.42, and appears in prose on p.3. |
| 7 | Corollary 1: for a resolving DGP, EM = u_0 | ARR:17, ARL:127-129 | VERIFIED | p.12: "For any resolving DGP, the expected movement over the entire stream must equal the expected uncertainty reduction: EM = u_0 = π_0(1-π_0)". Resolving is defined on p.9 as π_T(H_T) ∈ {0,1}. |
| 8 | Prop 4: for any v and ε there is a DGP with Pr(V=v) ≥ 1-ε | ARR:19-21 | VWC | p.20: "For all δ∈(0,1) and expectations stream v, there exists some DGP **and set of state values** such that Pr(V=v) ≥ δ." Writing δ as 1-ε is harmless. But the README drops "and set of state values", and that freedom is exactly the scale point the result turns on. |
| 9 | Quote: "for any observed stream of expected value predictions, there is some DGP in which that exact stream occurs with arbitrarily high probability" | ARR:76-78 | VERIFIED | Verbatim, p.4 (Introduction). It is the intro's paraphrase, not the text of Prop 4. |
| 10 | Prop 4's content is "measure-theoretic rather than algebraic", which is why it was not formalized | ARR:96-97 | UNSUPPORTED | The proof (pp.44-45) is an explicit finite construction: two states, two signals, and closed-form transition probability (v_t−v^L)/(v_{t+1}−v^L), pushed to 1 by moving v^L (or v^H). Nothing measure-theoretic is needed. |
| 11 | Table 1: stream [1/2,1/4,1/10] with P=5/16, m=17/200, r=4/25, m-r=-3/40; stream [1/2,1/4,1/2] with P=3/16, m=1/8, r=0; EM=ER=1/10 | ARR:41-45, ARpy:47-51 | VERIFIED | I recomputed these by hand and with the script. Rendered p.11, panel (1): row [l,l] "5/16 [1/2 1/4 1/10] … 17/200 4/25 −3/40"; row [l,h] "3/16 [1/2 1/4 1/2] … 1/8 0 1/8"; the Average row has m = r = 1/10. The per-period columns (1/16, 1/16, 9/400, 39/400; r_{1,2} = −1/16 for [l,h]) are also consistent. |
| 12 | Table 1 "reproduced row by row" | ARR:41 | VWC | The script asserts against the published values only P, m and r, and only for 2 of the 4 rows of panel (1). It does not assert the streams, the four per-period columns, m-r, or panels (2)-(3). |
| 13 | Quote: "we are not claiming that our instrument is universally more powerful to detect all deviations from rationality" | ARR:68-69 | VERIFIED | Verbatim, p.6 (Introduction; the sentence begins with a capital "We"). Section 4 repeats the disclaimer in fn 33, p.29: "this does not imply that our test is universally better at detecting all deviations". |
| 14 | "AR's driving assumption, held throughout, is that 'the normatively-correct beliefs are unobserved'" | ARR:85-87 | VWC | The quote is verbatim (p.5), but the paper says "held throughout **the rest of** the paper" and explicitly departs from it in Sec. 3.3 (p.27: "in sharp contrast to the rest of the paper"). |
| 15 | "they call their approach agnostic" | ARR:87 | VERIFIED | p.2: "this 'agnostic' investigation". |
| 16 | "AR need a belief *stream* over time per agent" | ARR:89-90 | VWC | The main test (Sec. 2.4, pp.15-16) pools one-period movements "for any time period and for any DGP". Each observation needs only two consecutive beliefs, not a whole stream per agent. |
| 17 | Section 4 framing: E[f(θ_0..θ_t)(θ_t-θ_{t+1})]=0 for any instrument f; their test is f = 2θ_t-1 | ARR:57-60, ARL:27-29, LOG:32-36 | VERIFIED | Section 4, p.30: "recall from Section 2.2 that 𝔼[f(π_0,π_1,..π_t)·(π_t−π_{t+1})] = 0 for any instrument f and our test employs the instrument (2π_t−1)". The same framing is on p.6 and p.11 ("Different tests of the martingale property arise from different instruments"). |
| 18 | They explicitly ask "why use this particular instrument?" | ARR:67-68 | VERIFIED | p.6: "But, why use this particular instrument?" |
| 19 | Review log: zero occurrences of order effect / ordering / permut / exchangeab / commut | LOG:21-22 | VERIFIED | I grepped the full text: 0 for each stem. "order" appears only twice, in unrelated senses: "three orders of magnitude" (fn 11) and "In order to" (p.23). |
| 20 | The four targeted biases are base-rate neglect, confirmation bias, underreaction and overreaction, "all magnitude biases" | LOG:24-26 | VWC | The four biases are correct (pp.5, 24). "Magnitude" is the project's gloss: AR read their statistic as a *directional* tendency to "revert to the point of highest uncertainty" (p.11). |
| 21 | They never compare two orderings of the same evidence | LOG:26 | VERIFIED | No such analysis appears anywhere in the paper. |
| 22 | Their one-dimensional belief stream "could not express permutation non-commutativity" | LOG:26-27 | UNSUPPORTED | The DGP may depend arbitrarily on history (p.9: "Our setup puts no restrictions on the DGP"). Nothing in the framework prevents comparing streams under permuted signals. The paper simply doesn't do it. |
| 23 | "formalizing it is what exposed that excess_step is a pure algebraic identity … a genuine finding about *their* paper"; README section "What formalizing revealed" | LOG:86-91, ARR:50-57, ARL:21-24 | CONTRADICTED | AR state the pointwise identity themselves. p.3: "the one-period excess movement statistic M_{t,t+1}−R_{t,t+1} can be simplified as (2π_t−1)(π_t−π_{t+1})". fn 21, p.18: "can be written as (2π_t−1)(π_t−π_{t+1})". p.11: "EM−ER is equivalent to the use of the instrument (2π_t−1)". The README concedes "This matches the paper's own Section 4 framing"; the log does not. |
| 24 | AR is "not a mechanism … No alternative rule is proposed" | LOG:106-111 | CONTRADICTED | Sec. 3.1, p.24: "We consider a very simple, portable framework for non-Bayesian updating … LR[π_{t+1}] = LR[π_t]^α·LR[s_{t+1}]^β". This is an alternative updating rule. It is offered as a model of biases, not a normative rival, so the spirit of "not a competitor to Jeffrey" survives, but the sentence as written is false. |
| 25 | Their agent is Bayesian by assumption, and the martingale comes from conditioning with known likelihoods | LOG:107-109 | VERIFIED | p.9: π_t(H_t) = π_0P(H_t\|x=1)/(…). p.11: "The only requirement on Bayesian beliefs is that they are bounded … and satisfy the martingale property". |
| 26 | Flag: the martingale property may not be automatic for a Jeffrey updater, which would limit the test's scope (recorded as UNVERIFIED) | LOG:128-137 | NC | AR never mention Jeffrey. The closest point is p.22: beliefs that are Bayesian for "a completely different DGP" are not rejected. The log is right to keep this unasserted. |
| 27 | Proposition PRO "completes the classification Augenblick–Rabin decline to attempt" | LOG:36-39, 151-153 | UNSUPPORTED | AR disclaim universality and optimality (p.6; fn 6: "We do not claim nor suspect that our choice is optimal for all deviations or DGPs"; fn 33). They never pose, or decline, a classification of instruments. |
| 28 | Prop 4 means expectations streams carry no testable content, while binary-belief streams do | LOG:40-41, ARR:75-80 | VWC | Prop 4's title says "Without Additional Assumptions". Prop 5 and Cors 7-8 (pp.21-22) give expectations streams testable content once variance or bounds are known. For binary beliefs, a single stream can't be rejected for too little movement (Cor 4, p.15). |
| 29 | Not-formalized list: "Prop 2 and Cors 2-5, Prop 3, Props 4-5 and Cors 6-7, Props 6-7 (the four biases)" | ARR:94-98 | VWC | The list leaves out Corollary 8, Proposition 8 (biases, EM vs EM^B) and Proposition 9 (affine transformations), and it uses working-paper numbering. |
| 30 | `orthogonal_of_martingale`: the martingale property makes the difference orthogonal to "every instrument measurable at t" | ARL:118-125 | VWC | The hypothesis quantifies only over f(θ_τ), not f(θ_0..θ_τ). That is weaker than the paper's p.11 statement, but it is exactly what the Appendix proof uses (p.42: "E[π_{t+1}\|π_t]=π_t"). |

---

## 2. Shmaya & Yariv, "Experiments on Decisions Under Uncertainty: A Theoretical Framework" (Drive copy: working paper, "Current Version: November 9, 2008")

(a) I read all 45 PDF pages: main text pp. 1-29 and the Appendix pp. 32-45 (the interval-order lemmas of the Theorem 3 proof were read at a skim).

**Notation warning.** The paper's triple is **(α, τ, ζ = {ζ_n})**, not (α, ν, ς), and the length variable takes values in bold **N** = {0,1,…,N}, not ℕ.

| # | claim (short) | where | verdict | evidence |
|---|---|---|---|---|
| 1 | The Drive copy is the Nov 2008 working paper | SYR:3-4, SYL:4 | VERIFIED | Title page: "Current Version: November 9, 2008". |
| 2 | Cited as AER 106(7), 1775-1801 (2016) | SYR:3, SYL:4 | NC | The copy is the 2008 working paper, so Theorem and Definition numbers are the working paper's and are unconfirmed for the AER version. |
| 3 | σ : S^{≤N}→A; σ(s) is the report of the most probable alternative | SYR:10-11 | VERIFIED | p.10: "Experimental observations are summarized by a mapping σ : S^{≤N} → A. For every signal sequence s, σ(s) is the subject's report of the most probable alternative given s." |
| 4 | Def 1: a triple "(α, ν, ς)" valued in "A, ℕ, S^N" | SYR:12-14, SYL:12-14 | MISQUOTED | p.11: "a triplet (α, τ, ζ = {ζ_n}_{1≤n≤N}) of random variables over some probability space (Ω, 𝒜, ℙ) with values in A, **N**, S^N", where **N** = {0,…,N} (p.10). The symbols differ, and ℕ is wrong. (Lean's Fin (N+1) actually matches **N**.) |
| 5 | Def 2: restricted means the length is independent of (α, ς) | SYR:15, SYL:15-16 | VERIFIED | p.12: "such that τ is independent of the pair (α, ζ)." |
| 6 | Def 3: positive probability of every conditioning event, and σ(s) = argmax_a P(α=a \| …) | SYR:17-19, SYL:17-20 | VERIFIED | p.12, conditions 1-2 and eq. (1). The paper adds "when we use the arg max notation, we implicitly assert the uniqueness of the maximizer", which supports Lean's strict-max reading. |
| 7 | Theorem 2 is the unrestricted "anything goes" result | SYR:21-22, SYL:22-23 | VERIFIED | p.18: "Theorem 2. [Unrestricted Conjectured Experiments] For every σ : S^{≤N} → A the experimental observations σ admit an explanation by an unrestricted conjectured experiment." Abstract: "(“anything goes”)". |
| 8 | Theorem 1 is the restricted result: iff σ(s^x)=a for all x ⇒ σ(s)=a | SYR:24-26, SYL:25-27 | VERIFIED | p.13: "…can be explained by a restricted conjectured experiment if and only if … Let s be an instance. If, for some a* ∈ A one has σ(s^s) = a* for every s ∈ S then σ(s) = a*." |
| 9 | It is a Sure-Thing-Principle condition | SYR:26, SYL:27 | VERIFIED | p.13: "reminiscent of the 'Sure Thing Principle' and the notion of dynamic consistency". |
| 10 | Quote: "her assessment of each realized alternative under s is a convex combination of the corresponding assessments over all continuations s^x; in particular, the most likely alternative must be a." | SYL:198-200 | VWC | p.13 reads "…over all continuations **s^s. In** particular, the most likely alternative must be **a***." The quote renames s^s to s^x, joins two sentences with a semicolon, and drops the star from a*. The substance is intact. |
| 11 | Quote: "all our results and proofs depend only on the rooted tree structure of the set of instances", used to dismiss "the measure-theoretic generality of Definition 1" | SYR:102-105 | VWC | Verbatim on p.11 ("Technically, all our results…"). But the remark is about the *instance tree* ("In particular, the results remain true if the number of available signals is infinite"), not about the general probability space in Def 1, which is what the README uses it for. |
| 12 | Theorem 3 is "the characterization of Bayesian-violating reversals", listed separately from "Sections 5-6 (the intermediate independence case …)" | SYR:101-102 | VWC | Theorem 3 (p.23) *is* the Section 5 result. It applies to partially restricted conjectures (Def 4: τ ⊥ ζ given α), assumes binary A, and states that σ is explainable iff "revealed higher" is anti-symmetric. Theorem 4 (unordered dimensions, no Dutch book) is not mentioned at all. |
| 13 | "Claims formalized: Their Definitions 1-3" | SYR:8 | CONTRADICTED | ShmayaYariv.lean has no definition of Definition 2 (restricted): it appears only in docstring prose, and neither Theorem 1 lemma refers to it. See the fidelity section. |
| 14 | `no_reversal_of_restricted` is "that step as the necessity direction" | SYR:50, SYL:211-215 | UNSUPPORTED | It is a lemma about arbitrary real functions f and g that *assumes* the convex-combination identity, and it concludes only `f aParent = f a` (equal scores), not σ(s)=a. There is no link to conjectured experiments or to `Explains`. The paper's necessity step goes through Lemma 1(4) (relative interior of the convex hull, p.15). |
| 15 | `anythingGoes` uses uniform weights, with α reading σ off the observed prefix | SYR:42-43, SYL:161-175 | VERIFIED | This matches the proof of Lemma 3 and Theorem 2 (pp.19-20: arbitrary full-support μ on **N**×S^N, with α's conditional law p_{x\|n}) once one takes point-mass p_s and uniform μ. |
| 16 | "Under an unrestricted conjecture ν may be a function of the history, and then the conditioning events of distinct instances do not overlap" | SYR:69-73 | UNSUPPORTED | The events {τ=n, ζ\|n=s} are disjoint for distinct instances in *every* conjectured experiment, restricted or not. In the paper's own construction τ is *not* a function of history (μ has full support, p.20). What does the work is that α's conditional law may depend on τ, which Remark 1 (p.13) removes under the restriction. |
| 17 | "Testability is bought entirely by the nesting that the restriction reinstates" | SYR:74-77 | VWC | Consistent with Remark 1, the Theorem 1 proof, and Corollary 1 (adapted conjectures have the same power, pp.16-17). It is an interpretation, not a statement in the paper. |
| 18 | Correction (i): "Theorem 2 is about action rules … not about belief sequences" | SYR:88-89 | VWC | True of Theorem 2 as stated. But Lemma 3 (p.19) explicitly says "even if for each instance the full belief were elicited … an analogous 'anything goes' result would still hold". |
| 19 | Correction (ii): the underdetermination comes from the conjecture about the experimental design | SYR:89-91 | VERIFIED | Abstract; p.18: "anomalous updating behavior is indistinguishable from particular framing of the experimental design itself." |
| 20 | SymPy check (1) uses "the construction used in Lean" | SYpy:7-11, 57-62 | VWC | The script's sample space is the 7 instances, with singleton events. Lean's is Ω = (Fin N→S)×Fin(N+1). The two are equivalent in outcome but not the same construction. |
| 21 | `alpha_depends_on_nu`: α "cannot be independent" of ν | SYL:177-184, SYR:44-46 | VWC | The formal content only exhibits two outcomes with the same ς and different α. It does not prove that independence fails (that would need positive weight and a definition of independence, and there is none). |

---

## 3. Zhao & Osherson (2010), "Updating beliefs in light of uncertain evidence: Descriptive assessment of Jeffrey's rule", *Thinking & Reasoning* 16(4), 288-307

(a) I read all 21 PDF pages: the publisher cover sheet plus journal pp. 288-307.

| # | claim (short) | where | verdict | evidence |
|---|---|---|---|---|
| 1 | Exp. 1 elicits Pr(G), Pr(B), Pr(G\|B), Pr(G\|~B), Pr(B\|G), Pr(B\|~G) "twice, before and after a dim-flashlight glimpse" | SURV:64-66 | VWC | p.293 lists the six questions. Only the experimental condition (N=40) answered twice; the control group (N=30) answered once, after the light (p.293). |
| 2 | "invariance" is their term, and "they note Jeffrey's is 'rigidity'" | SURV:67 | CONTRADICTED | p.289: "invariance is said to hold (Jeffrey, 2004, §3.2)". fn 1: "Many authors, including Oaksford and Chater (2007) and Over and Hadjichristidis (2009) use the term 'rigidity' instead of 'invariance'." So invariance is Jeffrey's term, and rigidity belongs to others. |
| 3 | The test is Pr2(G\|B)=Pr1(G\|B) and Pr2(G\|~B)=Pr1(G\|~B) | SURV:67-68 | VERIFIED | Eq. (3), p.289. |
| 4 | The conditional is measured directly, per subject, within subject | SURV:16-17, 68-69, 79-83 | VERIFIED | Paired Pr1-vs-Pr2 tests per participant, pp.294-297. |
| 5 | Finding: rough, selective conformity, with stability in Pr(G\|B) and larger movement in the converse Pr(B\|G) | SURV:86-88 | VWC | The abstract says "rough conformity" and p.305 says "selective". The averages are 0.18 vs 0.45 (p.297) and 33.0% vs 73.0% (p.298). But 22 of 40 participants changed Pr(G\|B) (p.305), and the paper calls the impact "mild (although non-negligible)", so "stability" overstates it. |
| 6 | Quote: "the ineffable character of sensory impressions (Jeffrey 1983, x11.1)" | SURV:101-102 | MISQUOTED | p.306 reads "the ineffable character of sensory impressions (as stressed by Jeffrey, 1983, §11.1)". The survey rewrites the parenthetical inside the quotation marks and keeps the pdftotext artifact "x" for "§". |
| 7 | They use it to explain why invariance was violated more under the vivid flashlight than under the tangible lottery | SURV:102-104 | VWC | p.306 offers it "As an alternative to vivacity", as one of three tentative explanations ("Of course, more data are needed"). "Tangible" refers to "the event of matching the first four lottery numbers". The survey folds the rival vividness hypothesis into the ineffability one. |
| 8 | Flashlight vs lottery: violation is larger in Exp. 1 | SURV:102-103 | VERIFIED | p.305: shifts "averaging around 33%" in Exp. 1 vs "less than 2%" in Exp. 2. |
| 9 | ZO is not cited in the bibliography or the write-ups | SURV:106-107, PLAN:1394-1395, 1514-1515, LOG:657-666 | VERIFIED | grep finds 0 hits in bibliography.bib, question_and_answer*.tex, positioning_economics.tex and PAPER_B_MANUSCRIPT.tex. |
| 10 | ZO frame their work as a descriptive test of rigidity, not identification | SURV:115-116 | VERIFIED | pp.291-292 ("directly address the descriptive adequacy of Jeffrey's rule"). |

---

## 4. Zhao, Crupi, Tentori, Fitelson & Osherson (2012), "Updating: Learning versus supposing", *Cognition* 124, 373-378

(a) I read all 6 pages (pp. 373-378).

| # | claim (short) | where | verdict | evidence |
|---|---|---|---|---|
| 1 | Cognition 124, 373-378 | SURV:123-124 | VERIFIED | Header on p.373. |
| 2 | "A single directly-elicited probability judgment per subject" | SURV:126 | WRONG-NUMBER | Each participant gave **five** estimates: five decks in Exps 1-2, five trials in Exp. 3 (pp.374, 376: "This procedure was performed five times per participant"). |
| 3 | The design is between-subjects and yoked | SURV:127-128 | VERIFIED | p.374: "yoking each suppose participant to the immediately preceding participant". |
| 4 | Their Eq. (1): Pr2(A)=Pr1(A\|B) | SURV:128-129 | VERIFIED | p.373: "(1) UPDATING FOR LEARNED EVENTS". |
| 5 | Exp. 3: learn 0.64, suppose 0.53, control 0.51; suppose is indistinguishable from control | SURV:132-134 | VERIFIED | Table 5 (p.377): 0.64 (0.09) and 0.53 (0.10). Text: "the average raw estimate of Pr(A) was 0.51 … close to the 0.53 … [t(19)=0.51, p=.61] but reliably different from the 0.64 … [t(19)=4.09, p<.001]". |
| 6 | Footnote 1 frames the violation as a failure of invariance of Pr(A\|B) for the learned event | SURV:134-135 | VERIFIED | fn 1, p.373: "Violation of (1) can be conceived as failure to respect the invariance of conditional probability for the learned event B." |
| 7 | Quote: the learning/supposing debate "might be of limited relevance to the typical transition from one probability distribution to another" | SURV:144-146 | VWC | The quote is verbatim (p.377), but its subject is "the debate about (1)", meaning the normative status of Bayesian updating (Bacchus et al.; Arntzenius), not the learning/supposing debate. |
| 8 | The conclusion points to Jeffrey's rule and to ZO (2010) | SURV:147-148 | VERIFIED | p.377. |
| 9 | Swing states: learn participants took a win in one state to raise the chance of a win in another; suppose participants did not | SURV:149-152 | VERIFIED | p.377: "learn participants interpreted a win [loss] of one swing state to increase the chance of a win [loss] of another. In contrast, suppose participants' estimates of Pr(A\|B) were almost identical to the control group's Pr(A)." The paper covers losses too. |
| 10 | "…but this too is read off direct per-subject estimates, not an aggregate audit" | SURV:152-153 | CONTRADICTED | The swing-state conclusion is a *between-group* inference: the learn-group mean against a separate control group's mean Pr(A) (N=20 each), plus a two-way ANOVA on consistent vs inconsistent pairs. That is an aggregate comparison against a no-evidence benchmark. |
| 11 | Overall: the literature is "always per subject, never as an aggregate association compared to a sequence-free benchmark" | SURV:157-161 | VWC | Survives only through its qualifiers: Zhao 2012 compares groups against a control benchmark, but the statistic is a level and the benchmark is not "sequence-free". |
| 12 | It sits in the hard-learning regime, with B raised to certainty | SURV:142-143 | VERIFIED | In the learn condition B is revealed or announced (pp.374, 376). |

---

## 5. Wilson, A., "Bounded Memory and Biases in Information Processing" (Drive copy: Princeton working paper dated **April 29, 2003**, not the 2014 Econometrica version)

(a) I read all 55 PDF pages: main text pp. 1-31 in full and the Appendix pp. 31-55 (the proofs of Thm 1, Thm 4 and Lemmas 1-8 read; the long Claims 0-4 algebra at a skim).

| # | claim (short) | where | verdict | evidence |
|---|---|---|---|---|
| 1 | Cited as Wilson (2014), Econometrica | PLAN:1338-1339, LOG:122, 828 | NC | The Drive copy is the 2003 draft. Every checkable Wilson claim in the record rests on that draft; the 2014 version was not available. |
| 2 | Wilson is not a competitor to Jeffrey updating but a competing explanation for order effects | LOG:122-124 | VERIFIED | Theorem 4 (pp.18-19) is order dependence from optimal bounded memory: "First impressions matter in the short run", "Last impressions matter in the long run". There is no rule-level comparison with Jeffrey. |
| 3 | Bounded memory generates order effects *rationally* | LOG:697-699 | VERIFIED | Theorem 1 (p.10: optimal ⇒ incentive compatible, i.e. modified multi-self consistent) combined with Theorem 4. |
| 4 | Wilson's agent holds a single belief, so her model makes no cross-attribute prediction | LOG:699-701, 200-202 | VWC | There is one binary state S ∈ {L,H} (p.7). The model is silent on association by construction; the paper never says so. |
| 5 | The damped family is "a two-attribute stand-in for her behavioural signature" ("the second cue moves you less than fully") | LOG:769-774 | VWC | The one-attribute point is correct. But Wilson's signature also includes *over*-adjustment in interior states (beliefs move "as if he had received two h-signals", proof of Thm 6, pp.24-25; overconfidence) and long-run recency (Thm 4(ii)). A δ ∈ [0,1] damping family cannot show either, so it captures only the "ignore information at the extreme states" / short-run primacy part. |
| 6 | Wilson is removed and unused in the write-ups | LOG:671, 678-682 | VERIFIED | 0 hits in positioning_economics.tex, the bib and the manuscript. |

Note (not a verdict): Theorem 4(i) makes Wilson's model predict short-run **primacy** on single-attribute data. That is a bounded-memory account of Asch-type primacy, and it is relevant to the LOG:702-705 caution about Asch.

---

## 6. Cassell (what it is), and the Lange claims checked through it

**What it is.** Lisa Cassell, "Commutativity, Normativity, and Holism: Lange Revisited", *Canadian Journal of Philosophy* 50(2): 159-173, doi:10.1017/can.2019.17. It is "© The Author(s) 2019" (published online 2019); the paper's own "Cite this article" line says "Cassell, L. 2020". It argues that Lange's (2000) defence of Jeffrey conditionalization fails: either the Jeffrey framework is defective because it does not commute its inputs, or it is defective because it commutes the wrong kind of inputs (experiences mapped to Bayes factors, which no norm on evidence can govern). Sections: ECJC, the normativity problem, and the holism problem (Garber, Weisberg, Wagner's "considered experiences").

(a) I read all 15 pages (pp. 159-173).

| # | claim (short) | where | verdict | evidence |
|---|---|---|---|---|
| 1 | "Cassell (2019)" | LOG:828 | VWC | Online and copyright year 2019; the volume year is 2020, per the paper's own citation line. |
| 2 | Wagner 2003 is "the 'considered experiences' fix the handover flags as directly load-bearing in the Lange/Cassell exchange" | W03R:3-4 | CONTRADICTED | Cassell p.171: "**Wagner (2002)** offers a suggestion that overcomes these worries: it is considered experiences … that should be mapped to Bayes factors." The phrase is in Wagner 2002, fn 9 ("namely, considered experience (in light of ambient memory …)"). wagner2003.txt has 0 hits for "considered" or "Lange". Cassell cites Wagner 2003 only in fn 2 (Uniformity Principle). Also, "exchange" is unsupported: Cassell answers Lange (2000) nineteen years later, and there is no Lange reply in the set. |
| 3 | "Lange's point is that reversing two experiences does not reverse their input values"; WBR: "reversing input values does not correspond to reversing experiences" | IO:329-330, WBR:20-22 | VWC | Consistent with Cassell's reconstruction (abstract; p.160: "reversing the order of the evidence … does not reverse the order of the experiences"). IO states the contrapositive, which is logically equivalent. Checked against Cassell only: Lange (2000) is not in the paper set. |
| 4 | Non-commutativity on input distributions is "the case Lange declines" | IO:382, WBR:49 | VWC | Per Cassell, Lange grants that JC is non-commutative over weighted evidence partitions and declines to count that as a defect. "Declines" is ambiguous but consistent with this. |
| 5 | "Lange's point declined" / "Lange's point conceded by one parameter" | IO:435, 438 | NC | These describe the project's own models, not a paper. |
| 6 | "Lange was never admitted" | LOG:669 | NC | Internal bookkeeping. (It is consistent with 0 hits in the bib and manuscript.) |

---

## Formalization fidelity

### lean/Literature/AugenblickRabin.lean

It builds, with axioms `[propext, Classical.choice, Quot.sound]` and no `sorry` (checked).

- **Definitions** (`unc`, `movementStep`, `reductionStep`, `movement`, `reduction`): these match the p.9 definitions exactly, up to the renaming π→θ. `Ico t₁ t₂` gives Σ_{τ=t1}^{t2-1}. They are defined for all t₁, t₂ (an empty sum when t₁ ≥ t₂); the paper assumes t₂ > t₁. This is harmless. The docstring's word "verbatim" is not accurate because of the notation.
- **`excess_step`**: correct, proved by `ring`. It is exactly the paper's p.3 prose, fn 21 and the p.10 display (in expectation). It is not a new finding (see item 1.23).
- **`prop1_of_orthogonal`**: faithful *conditional on* the orthogonality hypothesis. It does not prove Prop 1 "for any DGP" from Bayes' rule: no DGP, no signal likelihoods, and no posterior formula are formalized. The weights are an arbitrary real vector (no nonnegativity, no normalization). That is a genuine generalization of an identity of weighted sums, and it is correctly disclosed.
- **`orthogonal_of_martingale`**: trivial, since it just instantiates the hypothesis at f = 2x−1. The hypothesis quantifies over functions of θ_τ only, not over f(θ_0..θ_τ). That is weaker than the p.11 statement but matches the Appendix proof (E[π_{t+1}|π_t] = π_t). The martingale property itself is assumed, never derived.
- **`resolving` (Corollary 1)**: it concludes E[m_{0,T}] = Σ_i w_i·u(θ_i 0). With the paper's common prior π_0 and weights summing to 1, this is u_0 = π_0(1−π_0), so it is a faithful generalization. It still rests on the orthogonality hypothesis.
- **What is not formalized**: Props 2-9, Cors 2-8, and any connection between Bayesian conditioning and the martingale property.

### lean/Literature/ShmayaYariv.lean

It builds, with axioms `[propext, Classical.choice, Quot.sound]` and no `sorry` (checked).

- **Definition 1**: only partially encoded. The paper allows an arbitrary probability space (Ω, 𝒜, ℙ). Lean fixes Ω = (Fin N→S)×Fin(N+1), with ς and ν as projections and α a *function* of (ς, ν). That rules out extra randomness in α: fine for the existence theorem, but not general. The "probability" is an arbitrary real weight function w, with no nonnegativity and no normalization. `Fin (N+1)` correctly matches bold **N** = {0..N}; the README/docstring's "ℕ" does not.
- **Definition 2 (restricted)**: **not encoded at all.** There is no predicate for independence of ν from (α, ς). The README's "Claims formalized: Definitions 1-3" is therefore false (item 2.13).
- **Definition 3 (`Explains`)**: faithful. It has positive weight on every event {ν=n, first n signals = x|n}, and strict maximality of the joint weight. That strict maximality matches the paper's "implicitly assert the uniqueness of the maximizer". Quantifying over full realizations x rather than instances is equivalent, because the event depends only on the prefix. Caveat: with signed weights `Explains` is weaker than Def 3. It is used only existentially, with w ≡ 1, so no harm results.
- **`anythingGoes` (Theorem 2)**: faithful. It is the paper's Lemma 3 / Thm 2 construction specialised to point-mass p_s and uniform μ. `Obs.prefix_inv` correctly encodes σ as a function on S^{≤N}.
- **`argmax_of_convex_combination` / `no_reversal_of_restricted` (Theorem 1 necessity)**: these are **not** Theorem 1 necessity. They are generic lemmas about real functions. The key step, that a restricted experiment makes the parent's assessment a convex combination of its children's, is taken as hypothesis `hf`, not derived. The conclusion is equality of scores, not σ(s) = a*; the latter would also need `Explains.argmax` uniqueness, and nothing connects to it. The paper also needs strictly positive weights (relative interior, Lemma 1(4)); Lean's p ≥ 0 is weaker as a hypothesis, which is fine for this direction.
- **`alpha_depends_on_nu`**: a two-element witness only. It does not prove failure of independence.
- **Theorem 1 sufficiency, Theorem 3 and Theorem 4**: not formalized (correctly disclosed).

---

## (c) Most important problems

1. **A "finding" credited to the formalization is stated in the AR paper itself.** LOG:86-91 calls the algebraic identity `excess_step` "a genuine finding about *their* paper" that formalizing "exposed". AR state the identity pointwise on p.3 and in fn 21, and state its instrument reading on pp.6, 11 and 30 (item 1.23). The README heading "What formalizing revealed" has the same problem.
2. **"No alternative rule is proposed" (LOG Entry 3) is false.** AR's Sec. 3.1 specifies a non-Bayesian updating rule, LR[π_{t+1}] = LR[π_t]^α·LR[s]^β (item 1.24).
3. **ZO terminology is reversed.** ZO attribute "invariance" to Jeffrey (2004) and "rigidity" to Oaksford–Chater and Over–Hadjichristidis. The survey says the opposite (item 3.2).
4. **The "considered experiences" fix is attributed to the wrong paper.** It belongs to Wagner 2002 (fn 9), and Cassell cites it as Wagner (2002). The wagner2003 README credits Wagner 2003 and calls it load-bearing in a "Lange/Cassell exchange"; Wagner 2003 never mentions considered experience or Lange (item 6.2).
5. **The Shmaya–Yariv Lean overclaims.** Definition 2 is not formalized, and the "necessity direction" is a generic convex-combination inequality that assumes the key step (items 2.13, 2.14). The README's mechanism story ("ν may be a function of the history, then events do not overlap") misdescribes the paper's construction (item 2.16).
6. **Zhao 2012 is mischaracterised as "per-subject, not aggregate".** The swing-state conclusion is a between-group comparison against a control benchmark (item 4.10). The survey's "single judgment per subject" is also wrong: there were five per participant (item 4.2).
7. **Notation and version drift.** AR's variable is π, and the Lean docstring calls θ "verbatim". SY's triple is (α, τ, ζ) with values in **N** = {0..N}, not (α, ν, ς) with ℕ. All three economics papers are cited by journal (QJE 2021, AER 2016, Econometrica 2014), while the Drive copies are working papers from 2020, 2008 and 2003, so every theorem or proposition number in the record is a working-paper number.
8. **Overreach about AR in the positioning claims.** "Completes a classification AR decline to attempt" and "a one-dimensional stream could not express permutation non-commutativity" have no textual support (items 1.22, 1.27). The Prop 4 paraphrase drops "and set of state values", and "no testable content" drops "without additional assumptions" (items 1.8, 1.28).
9. **ZO's ineffability quote is altered inside the quotation marks**, and a tentative, one-of-three hypothesis is presented as their explanation (items 3.6, 3.7).
10. **The Wilson "stand-in" ignores half of her signature.** It leaves out interior over-adjustment and long-run recency, and the source copy is the 2003 draft (items 5.1, 5.5).

## Verdict counts (85 claims: AR 30, SY 21, ZO 10, Zhao 2012 12, Wilson 6, Cassell/Lange 6)

| verdict | count |
|---|---|
| VERIFIED | 38 |
| VERIFIED-WITH-CAVEAT | 26 |
| MISQUOTED | 3 |
| WRONG-LOCATION | 0 |
| WRONG-NUMBER | 1 |
| UNSUPPORTED | 5 |
| CONTRADICTED | 6 |
| NOT-CHECKABLE | 6 |
