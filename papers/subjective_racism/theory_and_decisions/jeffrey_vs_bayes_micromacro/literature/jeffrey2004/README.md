# Jeffrey (2004), *Subjective Probability: The Real Thing*

Cambridge University Press, 2004. It is not in `bibliography.bib`.

The copy read is the **draft dated 4 November 2002**, 119 pp. (PDF page = printed page + 1). **Section numbers, page numbers and worked-example arithmetic may differ in the published book.** Re-check them before citing.

**Pages read on 2026-09-30:**
- **All of chapter 3, "Probability Dynamics; Collaboration", pp. 55-65.** I read it in the text layer, and read pp. 56-65 visually as well.
- Contents and Preface, pp. 1-4.
- Ch. 2, pp. 40, 42 and 49, read for "rigidity".
- A search of the whole text layer for rigid, invarian, soft, Bayes factor, Field, Wagner and commut.

## What the text says on the points the project relies on

- **Invariance (§3.1-§3.2).**
  - §3.1, p. 56: "Invariant conditional probabilities: (1) For all H, new(H\|D) = old(H\|D) ... certainty and invariance together imply conditioning."
  - p. 57 gives the equivalent forms: invariant odds (2) and invariant probability factors (3), π(A) = new(A)/old(A).
  - §3.2 ("Generalized Conditioning", fn 3: "Also known as *probability kinematics* and *Jeffrey conditioning*"), pp. 57-58: "As long as invariance holds, updating is valid by a generalization of conditioning"; "If the invariance condition holds for each answer, D_i, ... new(H) = Σ old(H\|D_i) new(D_i). This is equivalent to invariance with respect to every answer".
  - This is what Zhao-Osherson cite as "(Jeffrey, 2004, §3.2)".
- **Rigidity.** Jeffrey also uses "rigidity", in ch. 2 and for conditioning on a certainty.
  - p. 40: "presumably the rigidity condition was satisfied so that new(C\|H) ≈ 1".
  - p. 42: "Assuming rigidity relative to D, the odds factor ... Bayes Factor = Likelihood Ratio".
  - p. 49: "if the rigidity condition is satisfied, new(H) = old(H\|C)".
- **Own observations: the new probabilities are data.** p. 59, Example 4: "Certainly her new′(D_i)'s will have arisen through an interaction of features of her prior mental state with her new experiences at the microscope. But the results new′(D_i) of that interaction are *data* for the kinematical formula".
  - Example 4's figure, 41/60, is correct.
- **Other people's reports: use factors (§3.3, pp. 59-61).** "We now move outside the native ground of probability kinematics into a region where your new probabilities for the D_i are to be influenced by someone else's probabilistic observation report."
  - Such probabilities "are necessarily a confusion of what the other person has gathered from the observation itself ... with that person's prior judgmental state".
  - Hence "It is not from the new′(D_i) themselves but from the updates old′(D_i) ↦ new′(D_i) that you must extract the information".
  - Formulas (1)-(3), p. 60, combine the other person's probability factors π′ with your own priors.
  - (4), p. 61, defines the **Bayes factor as new odds over old odds**: "it is what remains of your new odds when the old odds have been factored out".
  - "Bayes factors have wide credibility as probabilistic observation reports, with prior probabilities 'factored out'". Fn 7 cites Schwartz, Wolfe and Pauker (1981).
- **Commutativity (§3.4, pp. 61-63).** "If you update twice, should order be irrelevant? ... The answer depends on particulars of (1) the partitions ...; (2) the mode of updating (by probabilities? Bayes factors?); and (3) your starting point, P."
  - §3.4.1, probabilities: "Can order matter? Certainly. ... the second assignment simply replaces the first. When is order immaterial? When there are two partitions, and updating on the second leaves probabilities of all elements of the first unchanged—as happens, e.g., when the two partitions are independent relative to your old." Fn 8 cites D-Z 1982, 825-6.
  - §3.4.2, factors: "In updating by Bayes or probability factors ... order cannot matter". Fn 10 says proofs are in Jeffrey's Petrus Hispanus Lectures 2000, pp. 52-64. Fn 9 cites Wagner (2002).
  - Example 7: factor updates on two partitions "must commute, for in either order they are equivalent to a single mapping ... with partition {D′_i ∧ D″_j} and factors f′_i f″_j".
- **Softcore empiricism (§3.5, pp. 63-65).** Kinematics can be recast as conditioning on an expanded space, after Skyrms (1980).
  - The softcore proposition is ℰ = [new(D_1)=d_1 ∧ ... ∧ new(D_n)=d_n].
  - (1) old(D_i\|new(D_i)=d_i) = d_i and (2) old(H\|D_i∧ℰ) = old(H\|D_i) give (3) old(H\|ℰ) = Σ d_i old(H\|D_i).
  - "The price of that trip would be a tricky expansion of the probability assignment ... to subjective propositions of the second order ... and third ... and beyond."

## Claims formalized

These are in `lean/Jeffrey.lean` (symlink to `lean/Literature/Jeffrey.lean`):
- `kin_comm_of_indep`: §3.4.1, order is immaterial for independent partitions. This is the general finite form.
- `fac_fac_eq_product` and `fac_comm`: §3.4.2, factor updates in sequence are a single update on the product partition with product factors, so they commute.
- `fac_smul`: p. 61, factors matter only up to a constant, so anchored Bayes factors and probability factors give the same update.
- `fac_eq_kin` and `kin_eq_fac`: p. 60 (2) and p. 57 (3). One factor update and one Jeffrey update are the same map under the two parametrizations.
- `skyrms_three` and `expansion`: §3.5, (1)-(3), and the existence of an expanded prior whose conditional on ℰ is the Jeffrey update.
- `example4`, `example5_printed` and `example5_normalized`: the worked numbers.

## Draft errata found

- **Example 5, p. 60.** The printed factors `π(D_i) = 4/3, 2/3, 2` do not satisfy formula (1).
  - With old(D_i) = 1/4, 1/2, 1/4 and π′ = 1, 1/2, 3/2, the correct values are 8/7, 4/7, 12/7, and new(H) = 3/7, not 1/2.
  - Formula (2) prints π′ where π is meant.
  - p. 60 refers to "sec. 2.2" for §3.2.
  - Fn 4, p. 59, mistitles Wagner (2002).

## Bearing on Paper B

- **Jeffrey's own line between levels and factors is provenance.**
  - One's own experience gives levels, and these are "data".
  - A reporter's probabilities are to be converted to factors, because they are contaminated by the reporter's prior.
  - This supports the paper's choice to model a panelist's *own* impression as a delivered level.
- **It also undercuts the manuscript's phenomenological argument** that "a Bayes factor requires the probability of the same credential for a candidate who is not competent" (MS:115-117).
  - Jeffrey's Bayes factor is computed from old and new odds and needs no counterfactual likelihood (p. 61).
  - The two readings coincide for a single step. The difference is what is held fixed when an input meets another prior.
  - The argument should rest on that, and on provenance, not on the likelihood reading. The likelihood reading holds only for conditioning on a certainty under rigidity (p. 42).
- **For MS:184-186, this book, not LoD ch. 11, is the source of the word "invariance".**
- **"Even Jeffrey took [the factor escape]" (papers_dialectic.tex step 2)** is supported here, but only for pooling others' reports. The 1983 book rejected it; see `literature/jeffrey1983/README.md`.

## Audit findings

See `notes/citation_audit/verify_jeffrey.md`.
- **J13, ZO's "(Jeffrey, 2004, §3.2)":** VWC. The term is introduced in §3.1 and applied to partitions in §3.2. This is the draft's numbering.
- **J15:** "rigidity" is also Jeffrey's own term (ch. 2), so notes/citation_audit.md:700-701 and verify_record_papers.md should not present it as foreign to him.
- **J9, Jeffrey (1988) on factor commutativity:** not checkable, but corroborated by §3.4.2.
- **J10:** the counterfactual-likelihood argument is contradicted by the definition on p. 61.
