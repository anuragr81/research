# Domotor, Z. (1980), "Probability Kinematics and Representation of Belief Change"

*Philosophy of Science* 47, 384-403. Read in full on 2026-09-29, visually,
from the scan: journal pp.384-403. PDF pp.4-5 are the same page (p.387, scanned
twice) and PDF pp.22-27 are blank.

## Claims formalized

On a finite sample space (the paper allows any measurable space):

* **Machines** (p.385): `(P, E, *)` with (a) triviality `P₁ = P` and (b)
  composition `(P_E)_{E'} = P_{E∧E'}`.
* **Bayesian machine** (p.389-390, (4)) on probabilities plus the improper
  state `0`. The monotonicity laws (1) `A ⊂ B ⇒ P_A ≪ P_B` (p.387) and (2)
  `A ∩ B = ∅ ⇒ P_A ⊥ P_B` (p.388). The mixing law (3)
  `[P +_a Q]_A = P_A +_{a_A} Q_A`, `a_A = a·P(A) : [P +_a Q](A)` (p.388).
  The p.399 impossible state: conditioning on incompatible propositions gives
  `0`.
* **Jeffrey's formula** (0)/(5) (p.386, p.392) and the **Jeffrey machine** on
  finitary strings of partition-probability pairs, the free monoid under
  concatenation (p.393). Observations (i) "zeros cannot be revised" and (iii)
  irrelevance (p.396). The Markovian iteration law of p.399. Non-commutativity,
  `[P_{(U,p)}]_{(V,q)} = [P_{(V,q)}]_{(U,p)}` "fails in general" (p.395), and
  "a noncommutative transition from `P` to `P_{(A,a)}` and then to
  `[P_{(A,a)}]_{(B,b)}`" (p.399).
* **Field's conditional** `P'(H) = (1/α_X) Σ_A e^{α_A} P(A∩H)` and the **Field
  machine** (p.396-397). Composition
  `[P_{(U,α)}]_{(V,β)} = [P_{(V,β)}]_{(U,α)} = P_{(U∧V, α∧β)}`. Clause (i)
  (mutual dominance) and clause (ii) (convexity; see below).
* **The embedding** `h_P : F_X → E_X`, `h_P(U,α) = (U,p)`,
  `p(A) = e^{α_A} P(A) : α_X` (p.397), which "sends the unit `(U,0)` to
  `(U, P|_U)`".

## Result

`lean/Domotor.lean` (symlink to `lean/Literature/Domotor.lean`; compiles with
`lake env lean Literature/Domotor.lean`; no `sorry`; axioms
`[propext, Classical.choice, Quot.sound]` for every theorem and for both machine
definitions):

* Machines: `Machine`, `bayesMachine` (triviality and composition proved as
  structure fields), `jeffreyMachine`.
* Bayesian: `cond_cond`, `cond_univ`, `cond_zero`, `cond_empty`, `cond_state`,
  `cond_disjoint`, `cond_dominated_of_subset` (1), `cond_orth_of_disjoint` (2),
  `cond_mix` (3).
* Jeffrey: `cellMass_jeffrey`, `jeffrey_dominated` (i), `jeffrey_irrelevant`
  (iii), `jeffrey_unit`, `jeffrey_markov`, `jeffrey_not_comm` (explicit 2x2
  instance: `(A, 4/5)` then `(B, 1/5)` from `(2/5, 1/10, 1/10, 2/5)` gives
  `(16/85, 2/5, 1/85, 2/5)`; reversed, `(2/5, 2/5, 1/85, 16/85)`),
  `jeffrey_of_cellwise`, `jeffrey_seq_on_meet`.
* Field: `fieldZ_pos`, `field_comp`, `field_comm`, `field_zero_iff` (i),
  `field_not_convex` (clause (ii) refuted), `field_mix` (corrected (ii)).
* Embedding: `field_eq_jeffrey_embed`, `embed_unit`, `embed_depends_on_state`,
  `embed_surjective`.

`sympy/check_claims.py` (16 checks, exact/symbolic, exit status 0): law (3),
Bayesian composition, the Markov law, the non-commutativity instance, the
iterate-on-the-meet identity (on 8 worlds, so each meet cell has two atoms),
Field composition and commutativity, the clause (ii) counterexample and the
corrected coefficient, the four embedding facts, and observation (iii). The paper
has no numeric examples of its own.

## What formalizing revealed

* **Clause (ii) of p.397 is false as stated.** Domotor writes
  `(P +_a Q)_{(U,α)} = P_{(U,α)} +_a Q_{(U,α)}`: "conditionalization commutes
  with mixing in a trivial fashion. All one modifies is the mixands and not the
  mixing coefficient." Counterexample (`field_not_convex`): two points,
  `α = (log 2, 0)`, `P, Q` the point masses, `a = 1/2`. The left side gives the
  first point `2/3`, the right side `1/2`. The correct law (`field_mix`)
  changes the coefficient to `a·Z_P/(a·Z_P + (1-a)·Z_Q)`, exactly as the
  Bayesian law (3) changes `a` to `a_A`. So on this point Field's conditional
  behaves like Bayes's, not "trivially".
* **p.395-396 needs a reading.** "The foregoing iterated application of inputs
  ... does not reduce to one joint input (composition) on the meet `U ∧ V`,
  save the special cases of refinement and probabilistic independence."
  `jeffrey_seq_on_meet` proves that two sequential Jeffrey steps *are* always
  one Jeffrey step on the meet, with the iterate's own distribution over the
  meet as input. That is also the p.393 description of the iterate as
  `Σ p̄₁(A₁∩…∩Aₙ)·P_{A₁∩…∩Aₙ}`. The claim is true only if it means that the
  joint input is not a function of `(p, q)` alone: it depends on `P`.
* **p.397, "Field's `F_X` in fact covers only what is probabilistically
  independent in Jeffrey's `E_X`", needs a reading too.** For a single input,
  `h_P` reaches every strictly positive Jeffrey input on a partition whose
  cells have positive mass (`embed_surjective`, `α_A = log(p(A)/P(A))`). The
  restriction concerns compositions: a fixed `α` delivers different `p` at
  different states (`embed_depends_on_state`: `(2/3, 1/3)` at `(1/2, 1/2)`,
  `(2/5, 3/5)` at `(1/4, 3/4)`).
* **Jeffrey on `h_P(U,α)` is Field on `(U,α)`** (`field_eq_jeffrey_embed`).
  Field's conditional is Jeffrey's rule with a state-dependent input. Domotor
  states the embedding but not this identity.
* Minor slips, not formalized: p.387 calls the belief space of a die "a
  6-simplex ... in the five-dimensional real Euclidean space" (it is a
  5-simplex). p.401 says `p` is found by *maximizing*
  `H_P(U,p) = Σ p(A) log(p(A)/P(A))`, which is the Kullback-Leibler divergence
  and is *minimized* under constraint (7). The stated solution
  `p(A) = (1/α_X) P(A) e^{-λα_A}` is the minimizer. `α_X` is used both as the
  normalizing constant (p.397) and as an expectation (7).

## Bearing on Paper B

Domotor supplies the bare fact Paper B cites him for: the Jeffrey machine is
non-commutative (pp.386, 395, 399). He frames this, "along with Field (1978)",
as a reason Jeffrey machines are "inadequate" (p.395). He does not explain
*why* it is non-commutative. He locates the problem in the structure of the
input space: no composition on partition-probability pairs, only strings
(pp.386, 393).

The formal content closest to Paper B's mechanism is the embedding. Jeffrey on
`h_P(U,α)` equals Field on `(U,α)`, and `h_P` depends on the current state.
Read the other way, a fixed delivered credence `p` corresponds to a Bayes
factor that depends on the state it meets. That is Paper B's `P^J` vs `P^B`
contrast. But Domotor does not draw this reading, and he uses the state
dependence to argue that Field's space is *smaller*, not to explain
non-commutativity.

`jeffrey_seq_on_meet` is useful to Paper B independently. Any two-step Jeffrey
route on the `A`- and `B`-partitions of a 2x2 table is a single Jeffrey update
on the four cells. What differs between routes is only the input on the meet.

## Audit findings (2026-09-29)

From `verify_scanned.md` §2:

* **Domotor never discusses likelihoods or marginals** (M2, UNSUPPORTED). The
  manuscript's mechanism sentence ("since the likelihood a delivered credence
  implies must be read against the marginal in force when the cue arrives
  \citep{Domotor1980}", `PAPER_B_MANUSCRIPT.tex:120`) attaches a mechanism the
  paper does not state. The closest passage is the p.397 embedding, which shows
  the converse direction (a fixed `α` gives a state-dependent `p`) and is used
  for a different point. Cite Domotor for bare non-commutativity only (pp.386,
  395, 399), and Field (1978) or Wagner (2002) for the Bayes-factor mechanism.
  This reading confirms it: the words "likelihood" and "marginal" do not occur
  in pp.384-403.
* **He treats non-commutativity as a defect** (M3): "Along with Field (1978) we
  may argue that Jeffrey machines are inadequate because, among other things,
  the commutativity ... fails in general" (p.395). The manuscript treats
  sequence effects as the coherent response. Not a misquote, but the source
  leans the other way.
* Bare non-commutativity (M1) is verified at pp.386, 395, 399.
  `jeffrey_not_comm` gives an explicit instance with attribute partitions, which
  Domotor himself does not give ("the reader is asked to write this out in
  detail", p.399).
* No statistic-by-statistic comparison of reading orders (M4).
* Bibliographic entry verified (M5): *Philosophy of Science* 47 (1980),
  384-403. The name is printed without an accent.

## Not formalized

The Boolean machine on filters and its conditional `⊳` (p.391-392). The
coarse-graining conditions (iii)(a)-(d) of p.394, whose equivalence Domotor
"claim[s] (but will not prove in detail here)". The particle example (p.395,
a measure on `ℝ`). Observation (ii) of p.396 (orthogonality preserved). The
reduction sketch of §3 (p.398-400: product spaces, Hahn-Banach, the Miller
principle), which has no theorem statement. The internalized conditionals
(p.400). §4 on maximum entropy and Martin-Löf's limit theorem (p.400-403),
which is calculus and asymptotics.
