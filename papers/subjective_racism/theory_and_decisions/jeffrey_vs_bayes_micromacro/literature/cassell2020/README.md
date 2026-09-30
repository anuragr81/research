# Cassell, L. (2020), "Commutativity, Normativity, and Holism: Lange Revisited"

*Canadian Journal of Philosophy* 50(2), 159-173, doi:10.1017/can.2019.17.
Published online 2019 ("© The Author(s) 2019"); the paper's own citation line
reads "Cassell, L. 2020", the volume year.

**Source.** The Drive copy (`lisa_cassell.pdf`, here `cassell.pdf`) is the
published article, 15 pages, journal pp. 159-173. Page references are journal
pages (PDF page + 158). Read in full on 2026-09-30; Figures 1 and 2 checked on
the rendered pages 162-163. Lange (2000) is **not** in the paper set: every
Lange claim here is Lange as Cassell reports him.

## Claims formalized

Cassell's reconstruction of Lange and her reply, to the extent they are
mathematical.

* **Jeffrey Conditionalization** (p.160): on a partition `{Bᵢ}`,
  `p'(A) = ∑ᵢ p(A|Bᵢ) p'(Bᵢ)`. It is "non-commutative over weighted evidence
  partitions" (p.161): the raven example, `.9` then `.7` ends at `.7`, the
  reverse order at `.9` (pp.160-161).
* **Lange's argument** (pp.161-163): "reversing the order of the evidence …
  does not reverse the order of the experiences" (p.159, abstract; p.161).
  Figure 1 (p.162): `p(e) = .99`, `q(e) = .8`, `r(e) = .75` against
  `q'(e) = .75`, `r'(e) = .8`, with `p(e) ≠ q'(e)`, `p(e) ≠ q(e)`,
  `q(e) = r'(e)`, `q'(e) = r(e)`, so `ξ₁ ≠ ξ₄` and `ξ₂ ≠ ξ₃`. Lange's quote
  (p.161): `0.99 → 0.8` and `0.75 → 0.8` are "different experiences".
  Figure 2 (p.163): `p(R) = .1`, `q(R) = .3`, `r(R) = .7` against
  `q'(R) = .7`, `r'(R) = .3`; the first move is a confirming whiff, the reversed
  one a "disconfirming" experience ("maybe a whiff of lemon").
* **The Bayes-factor reading** (p.165): "the impact … that some experience
  induces … is often identified with a Bayes factor",
  `BF_{p,p_ξ} = BF_{q,q_ξ*}` iff the new-to-old odds ratios agree; "An Account
  of the Impact of an Experience": identical experiences have identical Bayes
  factors. Fn 5: "two updates will commute just in case they yield the same
  Bayes factors in the original case and its permutation" (Field 1978 for one
  direction, Wagner 2002 for both).
* **ECJC** (p.167): clause 1 is JC; clause 2 says phenomenally identical
  experiences yield the same Bayes factors for any priors (fn 7: on the same
  partition). Joan (pp.167-168) violates clause 2.
* **Certain evidence** (p.168): "Bayes factors are undefined whenever one of the
  evidence propositions receives a value of one", so under Bayes-factor inputs
  classical conditioning is not a special case of Jeffrey updating.
* **Objective likelihoods** (p.169): probabilities `.8` and `.2` of the
  experience given `R` and `¬R` give "the Bayes factor … `.8/.2 = 4`".
* **Garber** (p.170): the same factor repeated drives the credence to near
  certainty; **Wagner (2002)'s considered experiences** (p.171) give "Bayes
  factors [that] decrease over time".

## Result

`lean/Cassell.lean` is a symlink to `lean/Literature/Cassell.lean`. It is
standalone on Mathlib, checks with `lake env lean`, and contains no `sorry`. Its
33 theorems use only `[propext, Classical.choice, Quot.sound]`.

| Lean theorem | Content |
|---|---|
| `jc`, `jc_cell`, `jc_rigid` | JC on a finite space and partition: realizes its input, rigid within cells |
| `jc_twice`, `jc_noncomm` | two updates on one partition end at the later input, so the orders differ whenever the inputs do (pp.160-161) |
| `raven` | `.9 → .7` has factor `7/27`, `.7 → .9` has `27/7` |
| `bf`, `oddsUpdate`, `bf_oddsUpdate`, `oddsUpdate_bf` | Bayes factors (p.165) and Field's rule, mutually inverse |
| `bf_eq_iff_prior`, `bf_eq_iff_post`, `different_priors_different_bf` | **Lange's inference**: the same posterior from different priors takes different factors |
| `figure1`, `lange_quote` | **Figure 1**: Lange's conditions hold; `ξ₁..ξ₄` have factors `4/99, 3/4, 1/33, 4/3`; `.99 → .8` disconfirms, `.75 → .8` confirms |
| `figure2` | **Figure 2**: `27/7, 49/9` against `21, 9/49`; `R ↦ .3` confirms first and disconfirms second |
| `reversed_inputs_reversed_bf_iff` | reversing the input values reverses the factors iff `x = y = p` (nothing moves) |
| `reversed_bf_reversed_inputs_iff` | reversing the factors reverses the input values iff `a = b = 1` (nothing moves) |
| `jellybean_weisberg` | Weisberg's jellybean numbers (Weisberg pp.3-4, not Cassell): `(36, 9/4)` against `(81, 4/9)` |
| `oddsUpdate_comm`, `bfUpdate`, `bfUpdate_comm` | **factors commute**, on one partition and on any two partitions (fn 5, Field's direction) |
| `same_partition_converse_fails` | fn 5's "only if" fails on a single partition |
| `ecjc`, `ecjc_eq_prod`, `ecjc_perm` | ECJC: any reordering of the same experiences ends at the same credence |
| `jcKeyed`, `jc_experience_comm_iff` | JC with inputs keyed to experiences commutes two experiences iff their inputs coincide |
| `joan` | Joan's two updates on `{R, C, O}`: factors `β(R:C) = 3/14` against `14/3` |
| `jc_input_one`, `oddsUpdate_lt_one`, `no_factor_reaches_certainty` | JC with input `1` is conditioning, but no positive factor reaches certainty (p.168) |
| `objective_factor` | likelihoods `.8/.2` give factor `4` for every prior (p.169) |
| `garber_repeated`, `oddsUpdate_mono`, `considered_bounded` | Garber's repetition drives the credence to `1`; a bounded product of factors keeps it below `oddsUpdate M p < 1` (pp.170-171) |

`sympy/check_cassell.py` (all PASS, exit 0) recomputes the raven, Figure 1 and
Figure 2 factors and Lange's conditions, solves both reversal equations
symbolically (only `x = y = p`, only `a = b = 1`), checks JC's later-input
property and non-commutation on a random exact-rational 3-cell example, factor
commutation on a random 3×4 cell model, the same-partition converse, all 24
orders of four ECJC experiences, the certainty equation, the `.8/.2` factor,
Garber's repetition and a bounded product, and Joan's factors.

## What formalizing revealed

1. **The jellybean example is not Cassell's.** `.1 → .8 → .9` against
   `.1 → .9 → .8` is Weisberg's (pp.3-4, where he reports Lange). Cassell's own
   examples are the raven (pp.160-161), Figure 1 (`.99/.8/.75`, p.162) and
   Figure 2 (`.1/.3/.7`, p.163). All are instances of one theorem.
2. **Lange's point holds in both directions, and neither is the contrapositive
   of the other.** Cassell's form, "reversing the evidence does not reverse the
   experiences", is `reversed_inputs_reversed_bf_iff`; the form in
   `notes/interior_omega.tex:329-330`, "reversing two experiences does not
   reverse their input values", is `reversed_bf_reversed_inputs_iff`. Under the
   Bayes-factor reading both are true except when no update moves anything.
   (The audit called the second the contrapositive of the first; it is a
   separate statement, also true.)
3. **Fn 5's biconditional fails on a single partition,** which is where all of
   Lange's and Cassell's examples live. Factors reversed ⇒ commutation holds for
   any partitions (`bfUpdate_comm`), but commutation does not force reversed
   factors when both updates are on the same partition
   (`same_partition_converse_fails`). Wagner (2002) needs (4.3)-(4.4), which a
   partition fails with itself (Remark 4.1; the example opening his Section 4;
   `Wagner2002.remark41_FeqE`, `sec4_FeqE`).
4. **Joan needs a common partition.** Fn 7 restricts ECJC's clause 2 to the same
   partition. The child moves `C` to `.7` and the adult moves `R` to `.7`; on
   the two-cell partitions `{C, ¬C}` and `{R, ¬R}` clause 2 does not apply. The
   violation is clean on `{R, C, O}` (`joan`).
5. **The "undefined" factor** (p.168) has a precise form: for `0 < p < 1` no
   positive real factor takes `E` to certainty (`no_factor_reaches_certainty`),
   while JC with input `1` is conditioning (`jc_input_one`). (In Lean `1/0 = 0`,
   so `bf 1 p` is `0`, not undefined; the theorem is stated to avoid that
   convention.)

## What is philosophical and not formalized

* Which elements *ought* to commute (Lange's Assumption, first conjunct,
  pp.163, 166): experiences individuated by phenomenal character, or by their
  impact (pp.163-167, fn 6).
* The identity conditions for experiences and the holism they presuppose
  (pp.163-164).
* The **normativity problem** (pp.167-170): that no internalist or externalist
  norm can govern a Bayes factor "qua magnitude"; that the clarity of an
  experience has no objective probability (p.169); that a reliability norm
  yields a probability, not a magnitude (p.170).
* The **holism problem** (pp.170-171): whether any rule maps considered
  experiences to factors non-arbitrarily.
* The Carnap/Field history (pp.171-173) and the conclusion that "the Jeffrey
  framework is defective … either … in virtue of not commuting its inputs, or …
  in virtue of commuting the wrong kinds of ones" (p.173).

The formal results are the premises these arguments use: probability inputs do
not commute (`jc_twice`), factor inputs do (`bfUpdate_comm`, `ecjc_perm`), and
factor inputs exclude certainty (`no_factor_reaches_certainty`). The dilemma's
normative force is not a mathematical claim.

## Bearing on Paper B

The project uses Lange only second-hand, through Weisberg and Cassell (L11).
Paper B's `ω = 1` model fixes the same input values in either order, so in
Cassell's terms it sits on the horn that "does not commute its inputs"; the
factor its inputs imply varies with what the first cue did
(`notes/interior_omega.tex:378-383`). Under the Bayes-factor reading its
non-commutativity is therefore non-commutativity of *experiences*, which is the
audit's D9 point. Cassell gives no support to the claim that Lange "declines"
non-commutativity on input distributions beyond granting it and denying that it
is a defect (p.159).

## Audit findings (2026-09-30)

This section answers `notes/citation_audit.md` item L11 and
`notes/citation_audit/verify_record_papers.md` §6.

* **L11 (identity).** Confirmed: Cassell, *Can. J. Phil.* 50(2) 159-173,
  online 2019, volume year 2020.
* **§6.1 "Cassell (2019)"** (LOG:828). VWC stands; prefer "Cassell (2020)" with
  "online 2019".
* **§6.2** (the wagner2003 README's "considered experiences" attribution). Already
  corrected in `../wagner2003/README.md`; Cassell cites the idea as Wagner (2002)
  (p.171).
* **§6.3 (VWC)**, IO:329-330 and WBR:20-22. Both statements are theorems (finding
  2). WBR's jellybean is correctly attributed to Weisberg.
* **§6.4 (VWC)**, "the case Lange declines" (IO:382, WBR:49). Per Cassell
  (p.159), Lange grants that JC is non-commutative over weighted evidence
  partitions and denies that this is a defect. "Declines" should say that.
* **§6.5, 6.6 (NC).** Unchanged.

### Proposed wording (text only, not applied)

* **IO:329-330**, replace "Lange's point is that reversing two experiences does
  not reverse their input values;" with:
  > Lange's point, as Weisberg (pp.~3--4) and Cassell (2020, pp.~159--163)
  > report it, is that reversing the input values of two updates does not
  > reverse the experiences behind them (and, reading experiences as Bayes
  > factors, reversing the experiences does not reverse the input values either,
  > except when neither update moves anything);
* **IO:382** (and WBR:49), replace "the case Lange declines" with:
  > the case Lange grants but does not count as a defect (as reported by
  > Cassell 2020, p.~159)
* **LOG:828**, replace "Cassell (2019)" with "Cassell (2020; online 2019)".

## Not formalized

* Lange (2000) itself (not in the paper set).
* Wagner's (2002) converse theorem, which is formalized in `../wagner2002/`;
  here only its failure on a single partition is shown.
* Weisberg's holism example (pp.170-171 of Cassell), formalized from the source
  in `../weisberg2009/`.
* Everything listed under "What is philosophical and not formalized".
