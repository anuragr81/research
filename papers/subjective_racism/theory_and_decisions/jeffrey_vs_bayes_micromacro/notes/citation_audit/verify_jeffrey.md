# Verification of claims about Jeffrey's two books

Repo: `/home/anuragr/development/git/research/papers/subjective_racism/theory_and_decisions/jeffrey_vs_bayes_micromacro`. I edited nothing in the repo except the files listed under "Files created" at the end.

Sources:
- **LoD** = R. C. Jeffrey, *The Logic of Decision*, 2nd ed., University of Chicago Press. This is a 242-page scan with no text layer. PDF page = printed page + 11 for the body.
- **SP** = R. C. Jeffrey, *Subjective Probability: The Real Thing*, draft dated 4 Nov 2002, 119 pp. PDF page = printed page + 1.

Page numbers below are always the books' printed pages.

**Pages read**
- **LoD.** I rendered every page listed here at 110 dpi and read it visually:
  - Front and back cover, half-title, and Contents pp. vii-ix.
  - Preface pp. xi-xiv.
  - All of **Chapter 11, "Probability Kinematics", pp. 164-183**, every page, including §11.11 Notes and References.
  - p. 184, the opening of ch. 12.
  - Index pp. 229-231.
  - The scan has no copyright page.
- **SP.** I read the whole of **Chapter 3, "Probability Dynamics; Collaboration", pp. 55-65**, in the text layer. I also read pp. 56-65 visually wherever numbers or formulas mattered: pp. 56-63 and 64-65 rendered.
- **SP, other passages.** I also read the Contents and Preface pp. 1-4. I searched the whole text layer for `rigid`, `invarian`, `soft`, `Bayes factor`, `Field`, `Wagner` and `commut`. That search took me to ch. 2, pp. 40, 42 and 49 ("rigidity condition"), and I read the context there.
- **Cross-checks.** I checked the other ends of the attributions in the text layers of Zhao & Osherson (2010), pp. 289-290 and 306; Diaconis & Zabell (1982), p. 827; and Hawthorne (2004), where the quoted passage is at text-layer l. 363.

**Warning on SP.** This is the November 2002 draft, not the Cambridge University Press 2004 book. Section numbers, page numbers and the arithmetic of the worked examples may differ in the published edition. Two drafting slips are visible even here:
- p. 60 says "the formula in sec. 2.2" where it means §3.2.
- p. 59 fn 4 gives the title "Probability Kinematics and Exchangeability" for *Phil. Sci.* 69 (2002) 266-278. On p. 62 fn 9 the same reference is titled "Probability Kinematics and Commutativity", which is the correct title.
- There is also an arithmetic error in Example 5; see J-X1.

Verdict key: VERIFIED, VERIFIED-WITH-CAVEAT (VWC), MISQUOTED, WRONG-LOCATION, UNSUPPORTED, CONTRADICTED, NOT-CHECKABLE (NC).

Abbreviations used in the "where" column:
- `MS` = PAPER_B_MANUSCRIPT.tex
- `bib` = bibliography.bib
- `dial` = notes/papers_dialectic.tex
- `2h` = notes/two_horn_motivation_body.tex
- `CA` = notes/citation_audit.md
- `VRP` = notes/citation_audit/verify_record_papers.md
- `SURV` = literature/measurement_susceptibility_survey.md
- `HR` = literature/hawthorne2004/README.md
- `DZR` = literature/diaconis_zabell1982/README.md
- `LOG` = notes/paper_review_log.md

---

## 1. Manuscript and bibliography

| # | Claim | Where | Verdict | Evidence |
|---|---|---|---|---|
| J1 | "Jeffrey conditioning \citep{Jeffrey1983} on successive uncertain impressions depends on their sequence" | MS:77 | **VERIFIED** | LoD p. 172 (§11.6): "Nor need it always be the case that the result of two applications of (11-2) is independent of the order: the belief function obtained from *prob* by first changing the probability of *B* to *p* and then changing the probability of *C* to *q* need not be the same as the belief function that would have been obtained if first the probability of *C* had been changed to *q*, and then the probability of *B* had been changed to *p*. But in the case of conditionalization, order is irrelevant". Also pp. 182-183 (§11.11): "It is straightforward to verify that the present kinematical scheme is not generally commutative: if we apply (11-7) twice, ... the resulting probability measure may be one thing or another depending on the order of the two applications." |
| J2 | Both cues are "soft evidence in the sense of \citet{Jeffrey1983} so that neither is decisive but each leaves an impression" | MS:111 | **VWC** | The concept is Jeffrey's, but the term is not. LoD never says "soft evidence": I read ch. 11 in full, and the index has no entry for "soft" or "evidence". His term is "uncertain evidence": p. xii, "Chapter 11 deals with uncertain evidence, where an observation leads the agent to change his degrees of belief in one or more propositions to new values that fall short of 1"; p. 167, "the way in which uncertain evidence can be assimilated". "Impression" is his word too. p. 165, Example 1: "The agent inspects a piece of cloth by candlelight, and gets the impression that it is green, although he concedes that it might be blue or even (but very improbably) violet." SP p. 64 has "softcore, probabilistic data sentences", which is the nearest Jeffrey term. Suggested fix: "uncertain evidence in the sense of \citet[ch.~11]{Jeffrey1983}", or keep "soft" and cite the concept. |
| J3 | Jeffrey's rule resets the cue's marginal to the delivered credence and holds the conditionals fixed, "the invariance condition \citep[ch.~11]{Jeffrey1983}" | MS:184-186 | **VWC** | The content is right, but the name is not in the cited chapter. The condition is (11-1), p. 169, "(a) PROB(A/B) = prob(A/B) (b) PROB(A/B̄) = prob(A/B̄)", generalized in (11-8), p. 174: "PROB(A/A_i) = prob(A/A_i) for each i". LoD calls this the change "originat[ing] in B" (p. 168: "(c) the change from prob to PROB *originated* in B"; p. 174: "which defines what we shall mean by saying that the change from prob to PROB *originated* in the set (11-6)"; index: "Origination, 173-75"). "Invariance" does not occur in ch. 11 or the index. It is SP's term: §3.1, p. 56, "Invariant conditional probabilities: (1) For all H, new(H\|D) = old(H\|D)"; §3.2, p. 58, "If the invariance condition holds for each answer, D_i ...". Fix: either "---the condition that the change originates in the cue's partition \citep[§§11.3, 11.7]{Jeffrey1983}" or add SP and cite "\citep[§3.2]{Jeffrey2004}" for the word "invariance". |
| J4 | Displayed rule `(P^J_A P)(i,j) = q_i P(i,j)/P(A=i)` | MS:187-188 | **VERIFIED** | This is (11-7), p. 173, "PROB A = prob(A/A_1) PROB A_1 + ... + prob(A/A_m) PROB A_m", applied to the atom (i,j) of the cell A=i: prob((i,j)/A=i) q_i = q_i P(i,j)/P(A=i). SP p. 58 gives the same formula, "new(H) = Σ old(H\|D_i) new(D_i)". |
| J5 | "*The* partition satisfying the invariance condition is the minimal sufficient statistic ... for revising the prior to any candidate posterior" (cited to D-Z) | MS:191-193 | **CONTRADICTED** (by Jeffrey; supports DZ4 in verify_kinematics.md) | The manuscript cites D-Z, not Jeffrey, for this. But Jeffrey says outright that the originating partition is not unique. p. 174: "Note that since distinct sets of propositions can have the same set of atoms, there is a certain latitude in the choice of a set (11-6) in which the change from prob to PROB is viewed as originating." Example 7 (p. 174) re-expresses the same change as originating in {G∨B, G}. So "the partition" in the definite article is wrong on Jeffrey's own account as well. |
| J6 | bib entry `Jeffrey1983`: Jeffrey, R. C., *The Logic of Decision*, 2nd ed., University of Chicago Press, Chicago, 1983; "First edition 1965" | bib:1-9 | **VERIFIED** | The cover reads "Second Edition". The back cover reads "Since publication of the first edition in 1965, *The Logic of Decision* has become a classic ... Richard Jeffrey added much new material for the second edition in 1983", with the imprint "The University of Chicago Press", ISBN 0-226-39582-0. Note: the scan has no copyright page and is a later printing. p. xiv: "I have taken the opportunity afforded by a new printing to correct errata ... and to clarify the discussion on p. 20". Ch. 11 pagination should be unaffected. |

## 2. Working notes: the dialectic and the two-horn material

| # | Claim | Where | Verdict | Evidence |
|---|---|---|---|---|
| J7 | "The escape [Bayes-factor reparametrization] re-describes the input, and even Jeffrey took it", on the strength of Jeffrey (1988) | dial:70-73 | **VWC** (true of the late Jeffrey, and in a restricted setting; the 1983 Jeffrey explicitly refused it) | LoD pp. 182-183: after proving non-commutativity, Jeffrey says "That is as it should be, for just after the A-application, one's degrees of belief in the A_i *are* a_i, no matter what they were before". He then turns on Domotor and Field: "The notion that the kinematical scheme *ought* to be generally commutative (Domotor, *Philosophy of Science* 47 [1980]: 395) stems from a conflation of two attitudes toward The Given ... another, that moves Hartry Field to reparametrize it ... With Field, I see the α_i and β_j as artifacts, but unlike him, and like Garber, I suspect them of being epistemological geegaws that do no work. I prefer to make do with the a_i and b_j, which represent what Field and Domotor see as the probabilistic output of the observer-as-black-box, when the inputs are the α_i and β_j." SP does adopt factors, in §3.3-§3.4.2, pp. 59-63. But it does so for **someone else's** report: "We now move outside the native ground of probability kinematics into a region where your new probabilities for the D_i are to be influenced by someone else's probabilistic observation report" (p. 59). Updating on one's own observation keeps the probabilities as data. p. 59: "the results new′(D_i) of that interaction are *data* for the kinematical formula". So "even Jeffrey took it" needs dating (1988/2002, not 1983) and scoping (alien reports, not one's own impressions). |
| J8 | "The order effect is not a defect bolted onto the rule. It is what coherence costs when inputs arrive as levels." | dial:66-68 | **VERIFIED as Jeffrey's own position** (the dial does not cite him for it) | LoD p. 183: "That is as it should be", and the demand for commutativity "stems from a conflation". SP §3.4.1, p. 62: "Can order matter? Certainly. ... the second assignment simply replaces the first." The dial could cite LoD pp. 182-183 directly. |
| J9 | "\citet{Jeffrey1988} proved a commutativity result of the same kind for probability factors"; "Field (1978) / Wagner (2002) / Jeffrey (1988): Bayes-factor updating ... commutes" | dial:73; LOG:115 | **NC** for 1988; **corroborated** by SP | Jeffrey 1988 is not among the sources. SP §3.4.2, p. 62: "In updating by Bayes or probability factors f for diagnoses as in 3.3, order cannot matter." Fn 10: "Proofs are straightforward. See pp. 52-64 of Richard Jeffrey, Petrus Hispanus Lectures 2000". Example 7, p. 63: factor updates on two partitions "must commute, for in either order they are equivalent to a single mapping ... with partition {D′_i ∧ D″_j} and factors f′_i f″_j". |
| J10 | "a Bayes factor requires the probability of the same credential for a candidate who is not competent, and an impression ... does not supply that counterfactual" | MS:115-117; dial:79-81; 2h:12 | **CONTRADICTED by SP** (Jeffrey is not cited for it) | SP p. 61 (4) defines the Bayes factor from the old and new probabilities alone, with no likelihood: "β(D_i : D_1) = [new(D_i)/new(D_1)] / [old(D_i)/old(D_1)] = π(D_i)/π(D_1) ... it is what remains of your new odds when the old odds have been factored out". p. 61: "probability factors are a little easier to compute than Bayes factors, starting from old and new probabilities". Bayes factor = likelihood ratio holds only for conditioning on a certainty, "Assuming rigidity relative to D" (ch. 2, p. 42). The manuscript's own §3.3 formula `ℓ^B_j ∝ r_j/P(B=j)` (MS:213-215) is Jeffrey's probability factor, computed with no counterfactual. See Framing point 2. |

## 3. Zhao-Osherson attributions and the survey's report of them

| # | Claim | Where | Verdict | Evidence |
|---|---|---|---|---|
| J11 | ZO: "the ineffable character of sensory impressions (as stressed by Jeffrey, 1983, §11.1)" (ZO p. 306), and ZO p. 289: "Jeffrey (1983, §11.1) notes that the passage of experience need not raise the probability of any event to one ... says Jeffrey (pp. 165-166), 'the best we can do is to describe, not the quality of the visual experience itself, but rather its effects on the observer'" | ZO, via SURV:101-102, VRP:114 | **VWC** | The substance is in §11.1. p. 165: "there need be no such proposition *E* in his preference ranking; nor need any such proposition be expressible in the English language. Thus, the description 'The cloth looked green or possibly blue or conceivably violet,' would be too vague to convey the precise quality of the experience." pp. 165-166: "It seems that the best we can do is to describe, not the quality of the visual experience itself, but rather its effects on the observer". ZO's quotation is exact. Caveat: Jeffrey never uses the word "ineffable". The strongest statement is in §11.2, p. 166: "there is no reason to suppose that the language he speaks provides the means for him to describe that experience in the relevant respects ... the pattern of stimulation need not be describable in the language he speaks". So "§11.1" is right but "§§11.1-11.2" would be complete. |
| J12 | SURV's version of J11: "the ineffable character of sensory impressions (Jeffrey 1983, x11.1)" | SURV:101-102 | **MISQUOTED** (of ZO; content OK against Jeffrey) | This confirms VRP item 3.6: the parenthetical inside the quotation marks is rewritten, and "x" is a pdftotext artifact for "§". The location is correct against LoD (see J11). |
| J13 | ZO attribute "invariance" to Jeffrey (2004, §3.2) | ZO p. 289; CA:700; VRP:110 | **VWC** | In the draft the term is introduced in §3.1, p. 56: "Invariant conditional probabilities: (1) For all H, new(H\|D) = old(H\|D) ... certainty and invariance together imply conditioning". p. 57, still in §3.1: "There are special circumstances in which the invariance condition dependably holds". It is carried over to partitions in §3.2, p. 57-58: "As long as invariance holds, updating is valid by a generalization of conditioning"; "If invariance holds relative to each answer (D, ¬D)"; "If the invariance condition holds for each answer, D_i". So §3.2 is where invariance *for a partition* (ZO's (3)) is stated. Section numbers are the draft's, and I could not check the published 2004 numbering. |
| J14 | "The test of 'invariance' (their term; they note Jeffrey's is 'rigidity')" | SURV:67-68 | **CONTRADICTED** | This confirms VRP 3.2 against ZO. Against Jeffrey: in the kinematics chapters Jeffrey's word is "invariance" (SP ch. 3) or "originates" (LoD ch. 11). "Rigidity" does not occur in LoD ch. 11 or its index. |
| J15 | "'rigidity' is Oaksford-Chater's and Over-Hadjichristidis's (ZO fn 1)", i.e. not Jeffrey's term | CA:700-701; VRP:110, 208 | **VWC** | This is correct as a report of ZO fn 1. But Jeffrey himself uses "rigidity" in SP ch. 2, for conditioning on a certainty. p. 40: "presumably the rigidity condition was satisfied so that new(C\|H) ≈ 1"; p. 42: "Assuming rigidity relative to D, the odds factor ..."; p. 49: "if the rigidity condition is satisfied, new(H) = old(H\|C)". So both terms are Jeffrey's in the same book: "rigidity" in ch. 2 and "invariance" in ch. 3. The audit should not imply that "rigidity" is foreign to Jeffrey. |

## 4. Other attributions to Jeffrey in literature/ READMEs

| # | Claim | Where | Verdict | Evidence |
|---|---|---|---|---|
| J16 | "Amnestic Updating is just Standard Sequential Updating -- Jeffrey's original approach" (Hawthorne, citing Jeffrey 1965) | HR:15-16 | **VERIFIED** (consistent with Jeffrey's own text) | LoD p. 183: "just after the A-application, one's degrees of belief in the A_i *are* a_i, no matter what they were before, and after the B-application, the degrees of belief in the B_j *are* b_j, no matter what they were before". SP p. 62: "the second assignment simply replaces the first". Hawthorne cites the 1965 first edition. The preface, p. xii, says the 2nd edition is "as in the first edition" apart from ch. 9, §1.7, §§7.5-7.6, "minor corrections" and "new Notes and References", so the ch. 11 text is the 1965 approach. |
| J17 | D-Z §4.2 "attributes the successive route to Jeffrey (1957, Ch. 4)" | DZR:62-63 | **VWC** | This is accurate as a report of D-Z. D-Z p. 827: "Richard Jeffrey (1957, Ch. 4) has advocated another route from (4.1) to a final probability assignment: successive Jeffrey updating". It is NC against the 1957 dissertation itself. Jeffrey's own account, LoD p. 181: "This chapter stems from chapter 3 ('Elementary Dynamics of Belief') of my Ph.D. dissertation ... (Princeton University, 1957)". Either D-Z's "Ch. 4" is a slip or the successive route sits in a later chapter. Cite it as "D-Z attribute ... to Jeffrey (1957)" without adopting the chapter number. |
| J18 | Order-dependence and independence: D-Z (and the project's Prop. IMM at c=0) have successive updating commute when the partitions are independent | DZR; JeffreyOrder/PropIMM (`propIMM_indep`) | **VERIFIED** (Jeffrey states it) | LoD p. 183: "(This cannot happen if the A_i and B_j are independent relative to the initial probability function, in the sense that for all i and j, prob A_iB_j is simply the product of prob A_i with prob B_j.) For much more about these matters, see section 3 of Diaconis and Zabell's article." SP p. 62: order is immaterial "When there are two partitions, and updating on the second leaves probabilities of all elements of the first unchanged—as happens, e.g., when the two partitions are independent relative to your old", with fn 8 "see Diaconis and Zabell (1982), esp. 825-6". Formalized in general form as `kin_comm_of_indep`. |
| J19 | Jeffrey on Field and Garber: "See Field1978 for how Jeffrey updates are asymmetric in the delivered-credence parametrisation" | MS:117 fn; 2h:26-28 | **VERIFIED** (Jeffrey agrees with the reading) | LoD p. 181: "For a different sort of challenge to the kinematical scheme of sections 11.3-11.6, see Hartry Field, 'A Note on Jeffrey Conditionalization' ... and the reply by Daniel Garber". The discussion on p. 183 (see J7) confirms that Field's α_i, β_j are the reparametrized inputs. |

## 5. Items from the draft itself (not project claims)

| # | Item | Verdict | Evidence |
|---|---|---|---|
| J-X1 | SP Example 5 (p. 60): "your diagnostic probability factors (1) would be π(D_i) = 4/3, 2/3, 2 ... your new(H) would be 1/2 as against old(H) = 3/8" | arithmetic slip in the draft | With old(D_i) = 1/4, 1/2, 1/4 and π′(D_i) = 1, 1/2, 3/2, formula (1) gives Σ π′ old = 7/8, so π = 8/7, 4/7, 12/7 and new(H) = 3/7. The printed factors give new(D_i) summing to 7/6. Formula (2) on the same page is also misprinted: it has π′ where the example uses π. This is checked in `example5_printed` and `example5_normalized`. Do not quote these numbers. |
| J-X2 | SP §3.5 (pp. 64-65): kinematics is conditioning on an expanded space, the softcore proposition ℰ = [new(D_1)=d_1 ∧ ... ∧ new(D_n)=d_n] (after Skyrms 1980) | stated; formalized | (1) old(D_i\|new(D_i)=d_i) = d_i and (2) old(H\|D_i∧ℰ) = old(H\|D_i) give (3) old(H\|ℰ) = Σ d_i old(H\|D_i). "The price of that trip would be a tricky expansion of the probability assignment ... to subjective propositions of the second order ... and third ... and beyond." Formalized as `skyrms_three` and `expansion`. |

## Counts

19 project claims in total:

- VERIFIED: 7 (J1, J4, J6, J8, J16, J18, J19)
- VERIFIED-WITH-CAVEAT: 7 (J2, J3, J7, J11, J13, J15, J17)
- MISQUOTED: 1 (J12)
- CONTRADICTED: 3 (J5, J10, J14)
- NOT-CHECKABLE: 1 (J9, for Jeffrey 1988 itself)
- Plus 2 draft-internal items (J-X1, J-X2).

## Framing points

1. **Jeffrey is on the paper's side about the order effect, in so many words.** The 1983 book says non-commutativity "is as it should be" and calls the demand for commutativity a "conflation". It dismisses Field's reparametrized inputs as "epistemological geegaws that do no work. I prefer to make do with the a_i and b_j" (pp. 182-183). The paper can cite LoD pp. 182-183 for its premise that impressions arrive as levels and that the order effect is the price of that reading. That is a stronger citation than D-Z Remark 2. The same passage names Domotor (1980, p. 395) as the source of the demand for commutativity, which matters wherever the paper cites Domotor.
2. **The phenomenological horn needs rewording.** Jeffrey's Bayes factor (SP p. 61) is the ratio of new odds to old odds. It is computed from the prior and the delivered credence, and needs no likelihood under the counterfactual. So "a Bayes factor requires the probability of the same credential for a candidate who is not competent" is false of the Bayes factor that Field, Wagner and Jeffrey (SP) use. It is true only of the likelihood-ratio reading, which Jeffrey reserves for conditioning on a certainty "assuming rigidity" (p. 42).
   - The real difference between the two readings is **what is held fixed when the input meets a different prior**. Is it the level or the ratio?
   - In a single step the two readings are the same update. This is formalized as `fac_eq_kin` and `kin_eq_fac`.
   - SP draws the line exactly there, by provenance. One's own experience yields levels that are "data" (p. 59). Another person's report is to be taken as factors, because their new probabilities are "a confusion of what the other person has gathered from the observation itself ... with that person's prior judgmental state" (p. 59).
   - That supports the paper's choice for a panelist's own impression. But the argument should be "the impression is the panelist's own, so it arrives as a level (Jeffrey 2004 §3.2-3.3)", not "a Bayes factor needs a counterfactual likelihood".
3. **"Invariance" should be cited to SP, not LoD ch. 11.** Alternatively, use LoD's own word, "originates". SP is not in `bibliography.bib`. If it is added, cite the published edition (Cambridge UP, 2004) and re-check its section numbers against this draft.
4. **"Even Jeffrey took it" (dial step 2) should be dated.** The 1983 Jeffrey rejected the escape. The 1988/2002 Jeffrey adopted factors, and in SP only for pooling other people's reports.

## Files created

- `notes/citation_audit/verify_jeffrey.md` (this file)
- `literature/jeffrey1983/README.md`, `literature/jeffrey2004/README.md`
- `lean/Literature/Jeffrey.lean`: 18 theorems, no `sorry`. It checks with `lake env lean Literature/Jeffrey.lean`, with no errors or warnings. Every theorem I printed uses only `[propext, Classical.choice, Quot.sound]`. It is standalone and is not added to `lean/Literature.lean`.
- Symlinks `literature/jeffrey1983/lean/Jeffrey.lean` and `literature/jeffrey2004/lean/Jeffrey.lean`, both pointing to `../../../lean/Literature/Jeffrey.lean`.
