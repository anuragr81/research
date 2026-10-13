# Bonnay, D. and Cozic, M. (2018), "Weighted averaging, Jeffrey conditioning and invariance"

*Theory and Decision*, doi:10.1007/s11238-017-9639-3. The copy used is Drive:
`Weighted_averaging_Jeffrey_conditioning.pdf` (19 pages, the journal's online
version, with no printed page numbers). Page references below are to the PDF's
pages. Read in full on 2026-10-13, from the abstract through §6, the notes, the
acknowledgements and the references.

**Status in the manuscript:** not cited as of 2026-10-13. It is a candidate
citation for the partial-adoption rule, which the manuscript currently credits to
\citet{HogarthEinhorn1992} alone (MS §2.1, §6, Proposition LAD).

## The paper's structure (what is where)

- **§1 (pp. 2-3).** A social update has two stages. In *trade-off*, the agent
  sets her new probability for the event the other agent reveals. In
  *adjustment*, she revises everything else. Individual-update theories (Jeffrey
  conditioning) do only adjustment, and social-update theories (pooling) do only
  trade-off (p. 2). Jehle and Fitelson (2009) and Steele (2012) raised the
  question of how to complete a trade-off rule. Steele "considers supplementing
  weighted averaging with Jeffrey conditioning, but ends up rejecting both, on
  account of failure of commutativity" (pp. 2-3).
- **§2 (pp. 3-9). Jeffrey conditioning and invariance.** Belief states are
  pairs (finite algebra, probability) (p. 4). Definition 1 (p. 4) defines an
  individual updating rule. It is continuous in `p`, defined when `0 < p(A) < 1`,
  and the update gives `A` the new probability `r`. Jeffrey's rule is
  `J_U(p)(B) = p(B|A) r + p(B|¬A)(1-r)` (p. 4). Definitions 2-3 (pp. 4-5) define
  embedding and invariance under embedding (IE). Theorem 1 (p. 6): *Jeffrey
  conditioning is the only individual updating rule satisfying (IE).* The proof
  of (⇐) (pp. 6-7) works for rational values and appeals to continuity for the
  rest. The partitional generalization, rigidity (R) and within-cell ratio
  preservation (R′) are on p. 8, as is the comparison with Teller (1973) and van
  Fraassen (1990). Minimal change: IE makes the *process* informationally frugal
  ("blindness to content and context"), and the authors read their result as a
  convergence between process-minimality and output-minimality (p. 9).
- **§3 (pp. 9-11). Social updating.** Definition 4 (p. 10) defines social
  updating rules, which are restricted (`S′ = S`) or unrestricted (`S′ ⊂ S`).
  Definition 5 (p. 10) is weighted averaging, `F^W_λ(p,p′)(A) = (1-λ)p(A) + λp′(A)`.
  Definition 6 (p. 10) is **extended weighted averaging**:
  `EF^W_λ(p,p′)(A) = Σ_i p(A|A_i) F^W_λ(p,p′)(A_i)`, with `A_i` the atoms of
  `S′`. The Joe/John example (pp. 10-11) and the defence of order dependence
  (p. 11) follow, and then the "Nature" reading (p. 11).
- **§4 (pp. 12-15).** Eventwise independence, marginalization, invariance under
  isomorphism, zero unanimity, neutrality (Definitions 7-12). Definition 13
  (p. 13) is social IE. Theorem 2 (p. 13): for restricted rules on states with at
  least three atoms, `F = F^W_λ` iff (N) iff (MP)+(ZU) iff (EI)+(ZU) iff (EI)+(II)
  iff (MP)+(II) iff (IE). Theorem 3 (p. 14): an unrestricted rule is `EF^W_λ` for
  some `λ` iff it satisfies (IE). Then on p. 15: "Whereas Jeffrey conditioning is
  rigid, weighted averaging, as is well known, does not preserve independence."
- **§5 (pp. 15-18).** Geometric averaging, invariance under ratio-embedding,
  Theorems 4-5 (p. 17), and an open question (p. 18).
- **§6 (pp. 18-19).** Conclusion. The rules "form a coherent, unified, package of
  updating rules" (p. 18).

## Claims, as the paper makes them (quoted)

- Extended weighted averaging, read as completing Jeffrey conditioning (p. 11):
  "`p′(A)` could also be viewed as the probability suggested by 'Nature'. From
  this point of view, the 'new' probability `(1-λ) × p(A) + λ × p′(A)` which
  serves as input for Jeffrey conditioning ... is a sort of trade-off between the
  agent's priors `p(A)` and Nature's probabilities `p′(A)`. Under this
  interpretation, extended weighted averaging is not a way of completing weighted
  averaging, but rather a way of completing Jeffrey conditioning: Jeffrey
  conditioning (alone) takes as exogeneous the new probabilities to which it is
  applied."
- Order dependence (pp. 10-11): critics "contend that the end result should be
  the same, no matter whether I first meet Joe or John. This relies on the faulty
  assumption that doxastic weights should themselves be insensitive to the order
  of encounters." If I have already met Joe and found him reliable, "it does not
  seem right to still grant John's a doxastic weight of 0.8: intuitively, the
  figure should be lower." Then: "failure of commutativity per se is by no means a
  knock-down argument against weighted averaging". The same holds for Jeffrey
  conditioning, citing Bradley (2005): "the same experiences have different
  effects on my probabilities depending on the order in which they occur." In
  both cases "the objection wrongly takes for granted that the impact of evidence
  should itself be insensitive to order."
- The commutativity used in the axioms "does not concern the order of
  experiences, but merely the way experience is represented" (p. 11).
- "If one likes Jeffrey conditioning as an individual updating rule, one should
  also like weighted averaging (at least in similar doxastic conditions), and the
  other way around" (p. 11).

## Audit findings

1. **Edge cases in Theorem 1 (⇐) (pp. 6-7).** The rational argument writes
   `p(B)/p(A) = m/n` and builds `S″` with `m` atoms `X_i` and `n - m` atoms `Y_j`.
   When `m = 0` (a null atom) the block of `X`s is empty. When `m = n` the block of
   `Y`s is empty. In either case the map onto `X` or onto `Y` sends a nonempty
   atom to `∅`, so it is not injective and not an embedding. The same happens on
   `S → S′` when `B = A`. Continuity (Definition 1) closes all three cases, but the
   paper does not say so. The Lean proof covers `0 < m < k` and records the gap.
2. **Theorem 2 rests mostly on cited results.** The paper proves only
   (vii)⇒(vi) and (i)⇒(vii). The other equivalences are McConway (1981, Thms
   3.2-3.3) and Wagner (1982, Thms 1, 2, 4), as the proof says (p. 14). Theorem 3's
   proof is "a simple combination of the proofs for Theorems 1 and 2" (p. 14).
3. **Unproved remark.** "It can be shown that Jeffrey conditioning does not
   satisfy the individual version of invariance under ratio-embedding" (p. 18)
   is stated without proof. The project does not use it.
4. **One constant weight.** `EF^W_λ` has a single `λ` (Definition 6). Pages
   10-11 argue that weights should change with the order of encounters, but the
   axiomatized rule does not model how they change.

## Lean

`lean/BonnayCozic.lean` is a symlink to `lean/Literature/BonnayCozic.lean`, which
imports `Literature/Jeffrey.lean` for `kin`, `cellMass` and `Indep`. It builds
with no `sorry`. Every theorem uses only `[propext, Classical.choice, Quot.sound]`
(`lean/check_axioms.lean`).

| Lean | Paper |
|---|---|
| `mass_kin` | Jeffrey's rule on events, p. 4; partitional form, p. 8 |
| `kin_ratio`, `kin_rigid` | (R′) and (R), p. 8 |
| `push`, `cellMass_push`, `kin_push` | Definitions 2-3; **Theorem 1 (⇒)**, p. 6 |
| `Rule`, `Dom`, `Target`, `Success`, `IE` | Definition 1; Definitions 2-3 for finite algebras with atoms `Fin n` |
| `push_single`, `push_equiv`, `sum_lt`, `bool_sum` | lemmas for the next row |
| `IE_forces_jeffrey` | **Theorem 1 (⇐)**, pp. 6-7, for `p(b)/p(A) = m/k` with `0 < m < k`. The proof is the paper's: coarse-grain to `X, Y, Z`, refine to `k` equiprobable atoms, use the swaps as self-embeddings, then apply Success |
| `wa`, `mass_wa`, `wa_push` | Definition 5; **Theorem 2 (i)⇒(vii)**, p. 14 |
| `ewa`, `cellMass_ewa`, `mass_ewa` | Definition 6, p. 10, and "when `A ∈ S ∩ S′`, `EF^W_λ` reduces to `F^W_λ`" |
| `ewa_one`, `ewa_zero`, `ewa_id` | `λ = 1` is Jeffrey conditioning; `λ = 0` keeps the prior; with every atom revealed, EWA is WA |
| `ewa_push` | **Theorem 3 (i)⇒(ii)**, p. 14 |
| `joe_john`, `wa_two_step_gap`, `joe_john_gap` | The Joe/John example, pp. 10-11: `.515` and `.65`. In general, meeting `a` first minus meeting `b` first is `λ_a λ_b (y - x)` |
| `wa_assoc_product` | p. 15: the `λ`-mixture of two product tables has association `λ(1-λ)(a-a′)(b-b′)` |
| `ewa_comm_of_indep` | **Not the paper's.** EWA on two partitions that are independent under the prior commutes, for any weights attached to the two cues |

Not formalized: Theorem 1 (⇐) at irrational ratios and at null atoms (continuity);
Theorem 2 (i)-(vi) (McConway, Wagner); Theorem 3 (ii)⇒(i); §5.

## Sympy

`sympy/check_theorems.py` asserts 27 checks and exits 0 iff all pass. It checks
Jeffrey's formula, (R) and (R′), and IE for Jeffrey and WA on a refined 12-atom
state. It replays the paper's construction for Theorem 1 (⇐) at an actual ratio
`m/k`, and checks EWA's reductions and IE, the Joe/John numbers, the two-step gap
and the mixture's association. For the discussion below it also checks four
things on the paper's `2×2` table:
- With weights that belong to the cue, the two sequences agree at `c = 0`.
- With the paper's position weights (first cue in full, second `ω`), the
  `A`-marginal gap at `c = 0` is `(1-ω)(α - q₀)`, which is LAD(i).
- An EWA step leaves the odds ratio unchanged.
- At `c = 1/20` the sequences differ even with cue weights.

## Bearing on Paper B

1. **The paper's partial-adoption step is extended weighted averaging.** LAD
   answers the cue read second "by a Jeffrey step to `(1-ω)m+ωx`". That is
   Definition 6 with `λ = ω`, the cue's partition as the revealed algebra `S′`,
   `m` the marginal in force and `x` the delivered credence. `ewa_one` and
   `ewa_zero` are the paper's full adoption and no adoption.
   `JeffreyOrder/Anchoring.lean` (`dampedB`) is the same step on the `2×2`
   table. Theorem 3 axiomatizes it: it is the unique unrestricted rule invariant
   under embedding.
2. **The "Nature" reading (p. 11) fits a soft cue.** The paper calls an impression
   a delivered credence. Bonnay and Cozic's reading treats `p′(A)` as "the
   probability suggested by 'Nature'". On that reading, EWA completes Jeffrey
   conditioning by trading the delivered credence off against the prior instead of
   taking it as exogenous. This is the paper's partial adoption, and it does not
   need the testimonial reading. The reference letter does fit the testimonial
   reading as well.
3. **The weight belongs to the cue or to its place in the sequence.**
   `ewa_comm_of_indep` shows that when each cue carries its own weight, a product
   prior (`c = 0`) gives no sequence effect, whatever the weights. LAD(i)'s
   order-zero gap at `c = 0` therefore comes from the paper's choice to weight by
   position (first cue in full, second `ω`), not from averaging. Bonnay and
   Cozic's argument for order-dependent weights (pp. 10-11) is about two
   witnesses on **one** event: the second gets less weight because the first has
   already improved "my grounds for my belief in A". With cues on different
   attributes, the first cue improves the grounds for the second attribute only
   through a believed link, so their rationale gives a reason to down-weight the
   second cue only when `c ≠ 0`. That is an inference from their argument, not
   their claim.
4. **LAD(iii) has a one-line reason.** Every EWA step is a Jeffrey step, which
   rescales rows (or columns) of the `2×2` table and so leaves the odds ratio
   unchanged, whatever the weight.
5. **Normative positioning.** Bonnay and Cozic hold that failure of commutativity
   is not "a knock-down objection" to either weighted averaging or Jeffrey
   conditioning (p. 11), against Steele (2012). The concluding remarks set Döring
   (1999) and Hawthorne (2004) against sequence dependence. Bonnay and Cozic are
   a published voice on the other side.
6. **Two senses of "invariance".** §2.2 of the manuscript calls rigidity "the
   invariance condition" (Jeffrey 2004). Bonnay and Cozic's invariance under
   embedding is a different axiom about the process, and the paper itself
   relates the two (p. 8). Citing them next to §2.2 would need the two named
   apart.
7. **Hogarth-Einhorn link.** The two-step gap `λ_a λ_b (y - x)` is
   Hogarth-Einhorn's Appendix B recency `w_a w_b [s(x_b) - s(x_a)]`
   (`Literature/HogarthEinhorn.lean`, `appB_orderEffect`) in Bonnay and Cozic's
   notation.

## What the manuscript may and may not attribute to Bonnay and Cozic

**May attribute:**
- Extended weighted averaging, the rule `Σ_i p(·|A_i)[(1-λ)p(A_i) + λ x_i]`
  (Definition 6), and its axiomatization by invariance under embedding
  (Theorem 3).
- Jeffrey conditioning as the unique rule invariant under embedding (Theorem 1),
  and weighted averaging's characterization (Theorem 2, with McConway and Wagner).
- The "Nature" reading of the trade-off input (p. 11).
- The defence of order-dependent weights and of non-commutativity (pp. 10-11),
  and the report that Steele (2012) rejects EWA for its non-commutativity.

**May not attribute:**
- a weight that depends on the cue's place in the sequence (their rule has one
  `λ`; their argument for order-dependent weights is informal and about a single
  event);
- any claim about two attributes, association or the `2×2` table;
- any claim about measurement or aggregate statistics;
- that EWA commutes across independent partitions (`ewa_comm_of_indep` is the
  project's).
