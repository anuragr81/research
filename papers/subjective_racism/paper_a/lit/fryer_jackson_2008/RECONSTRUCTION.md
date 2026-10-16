# Fryer and Jackson (2008), reconstruction

Roland Fryer and Matthew O. Jackson, "A Categorical Model of Cognition and
Biased Decision Making", *The B.E. Journal of Theoretical Economics* 8(1)
(Contributions), 2008, Article 6.

## Source read

- The bepress PDF of the published article, 44 pages, of which PDF page 1 is
  the cover, PDF pages 3 to 38 are journal pp. 1 to 36 of the text and
  appendix, and PDF pages 38 to 44 are the references. Cached at
  `~/.cache/paper_a/sources/fryer_jackson_2008.pdf`, 308,753 bytes, sha256
  `eac29b93…89af2`. Text layer extracted to
  `~/.cache/paper_a/sources/fryer_jackson_2008.txt`, sha256 `cdf5ab0d…81d93`.
- Read in full on 16 October 2026 from the text layer, every page, so the
  evidence tag is [F]. Page images at 110 dpi were read for PDF pages 20, 21,
  22, 34 and 38, where the extraction garbles equations.

## The model, from its own primitives

1. **Objects and attributes** (PDF pp. 12–13). A finite set `O` of objects,
   each a past experience, with attributes `θ : O → {0,1}^m`. All attributes
   of an object are observed. Section 2 stresses that the employer "observed
   all attributes" and stores them coarsely (PDF p. 11).
2. **Categories and prototypes** (PDF pp. 13–14). A finite set of categories
   `C = {C_1, …, C_n}`, a categorization `f : O → C`, and the prototype of a
   category, its mean attribute vector, eqs. (1) and (2).
3. **Prediction** (PDF p. 14). Facing object `o`, the decision maker calls up
   `f(o)` and evaluates action `a` at the prototype,
   `EU(a, o) = U(a, θ̄(f(o)))`, eq. (3).
4. **Variation** (PDF pp. 15–16). With `d` the city-block metric,
   `Var(O) = Σ_{o∈O} d(θ(o), θ̄(O))`, eq. (4), and the total within-category
   variation of `f` is eq. (5). An optimal categorization minimises eq. (5).
5. **Why variance** (PDF p. 16). If attribute `k` matters with probability
   `p_k` and a correct prediction is worth `V_k`, the expected-utility
   categorization maximises `Σ_o Σ_k p_k V_k (1 − d(θ_k(o), θ̄_k(f(o))))`. Coding
   attributes so that the `p_k V_k` are equal, this is minimising eq. (5).
6. **Results** (PDF pp. 17–25). Lemma 1, objects of one type share a category.
   Lemma 2, a k-fold replication leaves the optimum unchanged. Example 4,
   enlarging a group can move its members further from their prototype.
   Proposition 1, which of two pairs of homogeneous groups to merge. Theorem 1,
   which two heterogeneous groups to merge. Proposition 2, where to put new
   identical objects in the equidistant world. Corollary 1, a strict minority
   with `r_E (2 − r_I) > 1` and enough categories is segregated from the
   majority and more coarsely sorted.
7. **Evidence and remarks** (PDF pp. 26–33). Own-race bias in face
   recognition, identity as a choice of attributes, endogenous categories,
   tests of the model, role models and policy.

## Derivation chain of what Paper A1 uses

- Lemma 4 (PDF p. 34) follows from Lemma 3 and the additivity of the
  city-block metric across attributes. In one attribute it is
  `Var = 2 n⁺ n⁻ / n` (`lemma4_one`).
- Proposition 1 follows from Lemma 4. Two homogeneous groups differing on `h`
  attributes have pooled variation `h · 2 n_i n_j/(n_i + n_j)` (`pairVar`,
  `prop1`).
- Example 1's Table 1 follows from Lemma 4 (`example1_table`,
  `example1_race_optimal`). The prototype `(0, 1/2)` of the black category puts
  the black worker of high human capital `1/2` below his type
  (`example1_prototype_error`).
- The decision maker's expected per-object accuracy loss in a category, from
  item 5, is the category's variation divided by its size, which in one
  binary attribute is `2 p (1 − p)` (`meanDev_eq`).

## What could not be reconstructed

- Example 4's footnote numbers (FJ-D3) and the last step of the proof of
  Corollary 1 (FJ-D4). Neither is relied on.
- Theorem 1 and Corollary 1 in general are not formalised. Paper A1 relies only
  on the one-attribute structure.
- The model has no equilibrium. The decision maker optimises memory against a
  fixed population of past experiences, and the population does not respond,
  except in the informal remarks on human capital (PDF p. 8) and identity
  (PDF pp. 28–30).
