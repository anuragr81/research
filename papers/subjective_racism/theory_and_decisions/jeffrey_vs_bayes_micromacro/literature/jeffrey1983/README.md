# Jeffrey (1983), *The Logic of Decision*, 2nd ed.

University of Chicago Press, Chicago. The first edition was 1965. Bib key: `Jeffrey1983`.

The copy read is a 242-page scan with no text layer and no copyright page. It is a later printing: the preface, p. xiv, mentions "a new printing to correct errata". The body's PDF page is the printed page + 11.

**Pages read, visually, on 2026-09-30:**
- The covers.
- Contents, pp. vii-ix.
- Preface, pp. xi-xiv.
- **All of chapter 11, "Probability Kinematics", pp. 164-183.**
- p. 184.
- Index, pp. 229-231.

## What the text says on the points the project relies on

- **Uncertain evidence, not "soft evidence".** p. xii: "Chapter 11 deals with uncertain evidence, where an observation leads the agent to change his degrees of belief in one or more propositions to new values that fall short of 1."
  - The word "impression" is Jeffrey's. p. 165, Example 1: the agent "gets the impression that [the cloth] is green, although he concedes that it might be blue or even (but very improbably) violet" (.30/.30/.40 → .70/.25/.05).
  - "Soft evidence", "invariance" and "rigidity" occur nowhere in ch. 11 or the index.
- **Ineffability (§11.1-§11.2).**
  - p. 165: "there need be no such proposition *E* in his preference ranking; nor need any such proposition be expressible in the English language."
  - pp. 165-166: "It seems that the best we can do is to describe, not the quality of the visual experience itself, but rather its effects on the observer".
  - p. 166, §11.2: "there is no reason to suppose that the language he speaks provides the means for him to describe that experience in the relevant respects".
- **The rule and its condition (§11.3, §11.6-§11.7).**
  - (11-1), p. 169: PROB(A/B) = prob(A/B), PROB(A/B̄) = prob(A/B̄), "for every proposition A". Jeffrey says the change "*originated* in B" (p. 168).
  - (11-2), p. 169: PROB A = prob(A/B) PROB B + prob(A/B̄) PROB B̄.
  - General case: (11-7) PROB A = Σ prob(A/A_i) PROB A_i over the atoms, and (11-8) PROB(A/A_i) = prob(A/A_i) (pp. 173-174). The two are "equivalent if both prob and PROB satisfy the probability axioms and none of the m numbers prob A_i is either 0 or 1".
- **Latitude in the originating partition.** p. 174: "since distinct sets of propositions can have the same set of atoms, there is a certain latitude in the choice of a set (11-6) in which the change from prob to PROB is viewed as originating". Example 7 illustrates this.
- **Relevance (§11.4).** (11-5), p. 170: PROB A − prob A = (PROB B − prob B) rel(A/B), with rel(A/B) = prob(A/B) − prob(A/B̄).
  - The mudrunner: .52 − .31 = .21 is seven tenths of .3.
  - The sign of rel agrees with Carnap's prob AB − prob A prob B (p. 171).
- **Conditionalization as a limiting case; reversibility (§11.5).** pp. 171-172: kinematics approaches conditioning as PROB B → 1. Unlike conditioning it is reversible: "mistakes can be erased".
- **Order dependence (§11.6).** pp. 172-173: "Nor need it always be the case that the result of two applications of (11-2) is independent of the order ... But in the case of conditionalization, order is irrelevant: prob_BC is always the same assignment as prob_CB".
- **Minimum change (§11.11).** p. 182: "As Persi Diaconis and Sandy Zabell show in section 5 of 'Updating Subjective Probability' ... the present kinematical scheme yields the closest new belief function on several common understandings of closeness, among which is the Jaynes-like relative entropy". Warning: "different senses of 'close' can yield nontrivially different generalizations".
- **Commutativity, and Jeffrey's verdict on it (§11.11, pp. 182-183).** This passage is central for Paper B.
  - "It is straightforward to verify that the present kinematical scheme is not generally commutative ... the resulting probability measure may be one thing or another depending on the order of the two applications. That is as it should be, for just after the A-application, one's degrees of belief in the A_i *are* a_i, no matter what they were before".
  - "(This cannot happen if the A_i and B_j are independent relative to the initial probability function ...)"
  - "The notion that the kinematical scheme *ought* to be generally commutative (Domotor, *Philosophy of Science* 47 [1980]: 395) stems from a conflation of two attitudes toward The Given ... another, that moves Hartry Field to reparametrize it".
  - "With Field, I see the α_i and β_j as artifacts, but unlike him, and like Garber, I suspect them of being epistemological geegaws that do no work. I prefer to make do with the a_i and b_j".
- **Provenance.** p. 181: "This chapter stems from chapter 3 ('Elementary Dynamics of Belief') of my Ph.D. dissertation ... (Princeton University, 1957)".
  - p. 181 also cites Teller (1973) and Armendt (1980) as "attempts to establish the credentials of the kinematical scheme".
  - It names Levi (1967) as "an early and persistent critic".

## Claims formalized

These are in `lean/Jeffrey.lean` (symlink to `lean/Literature/Jeffrey.lean`, shared with `literature/jeffrey2004/`):
- `relevance_identity`: (11-5).
- `mudrunner`: Examples 3-4.
- `cellMass_kin` and `total_kin`: (11-7) sets the originating marginal.
- `cellMass_kin_of_indep` and `kin_comm_of_indep`: the p. 183 parenthesis. Updating on independent partitions commutes, for any finite space and any two finite partitions. `JeffreyOrder/PropIMM` had only the 2x2, c = 0 case.

Rigidity (11-8), reversibility (§11.5) and "the later setting wins" are already in `lean/Literature/Doring.lean`: `jeffrey_rigid`, `jeffrey_reversible` and `jeffrey_same_partition`.

## Bearing on Paper B

- **Order dependence (MS:77).** The citation for it is verified: p. 172 and pp. 182-183.
- **The paper's premise that impressions arrive as levels is Jeffrey's own 1983 position.** He defends the resulting order dependence as correct ("That is as it should be"). He diagnoses the demand for commutativity as a conflation, and he names Domotor as its source. He rejects Field's factor inputs outright.
- **This is a better anchor than D-Z Remark 2** for "the order effect is not a defect". It also bears on how Domotor (1980) is cited.
- **The dialectic's "even Jeffrey took [the escape]" is false of this book.** It is true only of the later Jeffrey (1988, and the 2002 draft of *Subjective Probability*).

## Audit findings

See `notes/citation_audit/verify_jeffrey.md`.

1. **MS:111.** "soft evidence in the sense of Jeffrey1983": the concept is Jeffrey's, but the phrase is his "uncertain evidence" (VWC).
2. **MS:184-186.** "the invariance condition \citep[ch.~11]{Jeffrey1983}": the content is right, but ch. 11 never says "invariance". Its term is "originates in" (pp. 168, 174). "Invariance" is the 2004 book's term (VWC).
3. **MS:191.** "*The* partition satisfying the invariance condition": Jeffrey p. 174 says there is "a certain latitude" in the choice of that partition (CONTRADICTED; this supports DZ4 in verify_kinematics.md).
4. **Bib entry.** Verified against the cover and back cover.
5. **ZO's "(as stressed by Jeffrey, 1983, §11.1)".** The substance is on pp. 165-166 and is completed on p. 166 (§11.2). Jeffrey does not use the word "ineffable" (VWC).
