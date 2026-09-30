# Ortoleva, P. (2012), "Modeling the Change of Paradigm: Non-Bayesian Reactions to Unexpected News"

*American Economic Review* 102(6), 2410-2436.

**Source. The primary was not available.** This record formalizes the
restatement in Ortoleva, P. (2024), "Alternatives to Bayesian Updating",
*Annual Review of Economics* 16:545-570 (`ortoleva2024.pdf`, 26 pp., CC-BY). The
relevant parts are Section 2.2.2 (pp. 549-550: Axioms 1-2 and Theorem 1) and
Section 4 (pp. 558-560: the Hypothesis Testing model, Axiom 3 and Theorem 2).
Theorem 2 is "proved by Ortoleva (2012)" (p. 560). The survey restates the 2012
model in terms of beliefs, although "Ortoleva (2012) studies preferences as
primitives" (fn. 17, p. 558). **Nothing here has been checked against the AER
paper.** Where this record says "Ortoleva (2012)", it means the 2012 model as
its author restated it in 2024. Formulas were checked on the rendered pages
549, 558, 559 and 560. Read on 2026-09-30.

## Claims formalized

Setting (p. 549): a finite state space `Ω`, acts `f : Ω → X`, and
subjective-expected-utility preferences `≽` (prior `π`) and `≽_A` (posterior
`π_A`). A posterior is Bayesian when `π_A = π^BU_A`.

* **Axiom 1 (Consequentialism, p. 549).** If `f(ω) = g(ω)` for all `ω ∈ A`, then
  `f ∼_A g`.
* **Axiom 2 (Dynamic Consistency, p. 549).** If `π(A) > 0`, then for all `f, g`,
  `fAg ≽ g ⇔ f ≽_A g`.
* **Theorem 1 (p. 550; the survey attributes the proof to Ghirardato 2002).**
  `≽` and `≽_A` satisfy Consequentialism and Dynamic Consistency iff
  `π_A = π^BU_A`.
* **The HT model (pp. 558-559).** There is a prior over priors `ρ ∈ Δ(Δ(Ω))`, a
  prior `π` with `{π} = argmax ρ`, and a threshold `ε ∈ [0, 1)`. After an event
  `A`: if `π(A) > ε`, then `π_A = π^BU_A`. Otherwise `π_A = π̄^BU_A`, where
  `{π̄} = argmax ρ^BU_A` and `ρ^BU_A(π̄) = π̄(A)ρ(π̄)/∫π'(A)ρ(dπ')`. The
  maximizer is unique (fn. 18).
* **Axiom 3 (Dynamic Coherence, p. 560).** If `π_{A_i}(A_{i+1}) = 1` for
  `i = 1, …, n-1` and `π_{A_n}(A_1) = 1`, then `π_{A_1} = π_{A_n}`.
* **Theorem 2 (p. 560).** `π` and `{π_A}` satisfy Consequentialism and Dynamic
  Coherence iff they admit a minimal HT representation `(π, ρ, ε)`. Moreover,
  `ε = 0` iff `{π_A}` also satisfies Dynamic Consistency. A *minimal* HT model
  is one "such that no strictly smaller ε represents the same behavior".
* **p. 560 (4.2.2).** "The model's predictions coincide with Bayes' rule for all
  events with a likelihood above ε."

## Result

`lean/Ortoleva.lean` is a symlink to `lean/Literature/Ortoleva.lean`. The file
is standalone on Mathlib, checks with `lake env lean`, and contains no `sorry`.
All 49 of its theorems use only the axioms `[propext, Classical.choice,
Quot.sound]` (checked in a scratch copy).

Modelling choices, which the Lean header also records:

* Acts are identified with their utility profiles `Ω → ℝ`. Theorem 1's "only
  if" direction uses point acts `c·1_ω`. Any utility with a nondegenerate
  interval of values supplies these after an affine rescaling.
* `ρ` has finite support. It is an indexed family `P : ι → Δ(Ω)` with weights
  `ρ : ι → ℝ`. The argmax is carried as a field `sel` with the survey's
  maximality and uniqueness properties.
* The field `full` requires, for every nonempty `A`, some prior in the support
  with `π'(A) > 0`. The survey's formula for `ρ^BU_A` needs this (its
  denominator must be positive), but the survey does not state it.
* Axiom 3 is stated for cycles `A 0, …, A n` (`Fin (n+1)`).

| Lean theorem | Content |
|---|---|
| `thm1_if` | Bayes satisfies Consequentialism and Dynamic Consistency when `π(A) > 0` |
| `thm1_only_if` | a posterior belief satisfying Consequentialism and the forward half of Dynamic Consistency (`fAg ≽ g ⇒ f ≽_A g`) is `π^BU_A` |
| `thm1_iff` | **Theorem 1**, both directions |
| `eu_splice_sub` | the identity `EU_π(fAg) − EU_π(g) = π(A)[EU_{π_A}(f) − EU_{π_A}(g)]` |
| `HT.post` | the HT rule, exactly the display on p. 559 |
| `HT.ht_consequentialism`, `HT.ht_prob_self` | **Theorem 2, "if": Consequentialism**, in preference form and in belief form (`π_A(A) = 1`) |
| `HT.ht_dynamicCoherence` | **Theorem 2, "if": Dynamic Coherence**, for every HT model (minimality is not needed) |
| `HT.ht_bayes_of_gt` | HT equals Bayes whenever `π(A) > ε` (p. 560) |
| `HT.ht_eps_zero_bayes`, `HT.ht_eps_zero_dynCons` | **"Moreover", ε = 0 ⇒ DC**: with `ε = 0`, HT is Bayes on every positive-probability event and satisfies Dynamic Consistency |
| `HT.ht_dynCons_same_as_eps_zero` | **"Moreover", DC ⇒ ε = 0, in substance**: if an HT model satisfies DC, resetting `ε` to 0 (same `π`, `ρ`) changes no posterior |
| `HT.ht_minimal_dynCons_eps_zero` | hence a model that is minimal in `ε` and satisfies DC has `ε = 0` |
| `bayes_cycle`, `cycle_const`, `edge_support`, `edge_prob_le` | the cycle lemmas behind Dynamic Coherence |
| `exM`, `ex_no_tie` | an explicit HT model: `π = (24/25, 3/100, 1/100)` with `ρ = 9/10`, `π' = (1/5, 1/5, 3/5)` with `ρ = 1/10`, `ε = 1/20`. The scores never tie, so it is a valid HT model |
| `ex_prob_A`, `ex_unexpected`, `ex_sel_A` | news `A = {b, c}` has `π(A) = 1/25 ≤ ε`. `ρ^BU_A` then selects `π'` (scores `9/250 < 2/25`) |
| `ex_bayes_A`, `ex_ht_A`, `ex_departs` | **HT departs from Bayes on unlikely news**: Bayes gives `(0, 3/4, 1/4)`, HT gives `(0, 1/4, 3/4)` |
| `ex_likely` | on likely news `{a, b}` (`π = 99/100`) the same model is Bayesian |
| `ex_not_dynCons` | the model violates Dynamic Consistency. With `f = 1_b` and `g = 1_c`, `fAg ≻ g` (`3/100` vs `1/100`) but `g ≻_A f` (`3/4` vs `1/4`) |

`sympy/check_ht.py` (15/15 PASS) checks the following:

* The Theorem 1 identity, symbolically on 4 states.
* That the relative-likelihood equations have the Bayes posterior as their
  unique solution.
* Every number in the worked example, and that `{b, c}` is the only event on
  which the example departs from Bayes.
* A seeded sweep of 300 random exact-rational HT models (3-4 states, 3-5
  candidate priors, `ε ∈ {0, 1/20, …, 19/20}`):
  * Consequentialism holds on every event.
  * Dynamic Coherence holds on all 13284 cycles of length 2 or 3 whose premise
    holds.
  * `ε = 0` gives Bayes on every positive-probability event.
  * Dynamic Consistency fails (in 131 models) exactly when some event with
    `0 < π(A) ≤ ε` has a selected prior whose conditional differs from `π`'s.
    In each of those models `HT(ε) ≠ HT(0)`.
  * Point acts alone detect only 123 of the 131 failures. The acts used in the
    proof of Theorem 1 detect all of them.

## What formalizing revealed

1. **The "if" half of Theorem 2 holds for every HT model**, minimal or not. The
   proof of Dynamic Coherence has two cases, and a cycle cannot mix them. If
   `π(A_i) > ε` and `π_{A_i}(A_{i+1}) = 1`, then `π(A_{i+1}) ≥ π(A_i) > ε`, so
   the whole cycle is Bayesian with prior `π`. Otherwise, the top score
   `max_π' π'(A_i)ρ(π')` weakly rises along every edge, so it is constant on the
   cycle. Uniqueness of the maximizer (fn. 18) then forces the same `π̄` at
   every `A_i`. **Uniqueness is essential.** Without it, a tie-breaking rule
   could select different priors around a cycle.
2. **The "Moreover" needs a notion of minimality, and the survey's is
   informal.** "No strictly smaller ε represents the same behavior" is
   formalized here with `(π, ρ)` held fixed. With that reading, both
   directions are proved. The substantive fact is
   `ht_dynCons_same_as_eps_zero`: under DC, every threshold between the
   minimal one and `ε` gives the same posteriors.
3. **Well-definedness needs an unstated support condition.** `ρ^BU_A` is
   undefined when every prior in `supp ρ` gives `A` probability zero. The field
   `full` excludes this. The survey says the HT rule "is well defined after
   zero probability events" (p. 559). That is true only under such a
   condition.
4. **HT is a one-step rule.** It maps `(π, ρ, ε)` and a single event `A` to
   `π_A`, always from the original prior. Dynamic Coherence compares
   posteriors after *different single events*, not after successive updates.
   Its "sequence of events" (p. 560) is a cycle of alternatives. The survey does
   not define sequential HT updating. Nothing in Theorem 2 speaks to whether
   updating on `A` then `B` equals updating on `B` then `A`.

## Bearing on Paper B

The manuscript cites Ortoleva (2012) once (MS:276), as an alternative to Bayes
"used … to explain whether sequential conditioning is sequence-independent or
not". The HT model is a model of a *change of prior* triggered by news the
current prior finds unlikely (`π(A) ≤ ε`). Its axioms and theorem concern
single-event updating, and it has no order effects to explain. The model is
also event-based, while Paper B's cues are soft (Jeffrey) inputs. The survey's
fn. 2 (p. 546) puts "vague, ambiguous, or imprecise information", including
"Jeffrey's rule", outside its scope. So the citation fits as an example of a
non-Bayesian updating rule with an axiomatic foundation. It does not fit as a
theory of sequence (in)dependence.

## Audit findings (2026-09-30)

This section responds to `notes/citation_audit.md` items M7 and M8, and to
the "M6-M8 re-read" entry.

* **M7 (CAVEAT): "axiomatises departures triggered by unexpected news".
  Confirmed as accurate, second-hand.**
  * The HT rule departs from Bayes only when `π(A) ≤ ε`, which is the survey's
    "unexpected news" (p. 558).
  * Theorem 2 is the axiomatization.
  * The formalization confirms the parts that can be checked from the survey:
    the "if" direction, the ε = 0 ⇔ DC clause, and an explicit departure on
    unlikely news.
  * The "only if" direction (the representation theorem proper) is Ortoleva's
    2012 construction. It is not reproduced in the survey and is not
    formalized.

  The caveat stands. The primary has not been read, so the citation rests on
  the author's own later restatement.
* **M8 (UNSUPPORTED): the framing "to explain whether sequential conditioning
  is sequence-independent or not".** Confirmed for Ortoleva. See point 4 above.
  Dynamic Coherence is the only axiom that mentions "a sequence of events".
  It is about the consistency of posteriors after alternative single events, not
  about the order of updates.
* **Bibliographic entry.** `Ortoleva2012` (bibliography.bib:54) matches the
  survey's reference list for author, title, journal, volume, issue and pages.
  The DOI `10.1257/aer.102.6.2410` is not printed in the survey, so it remains
  unverified (audit M18).

### Proposed corrected wording (text only, not applied)

Replace MS:276 "Several alternatives to Bayesian updating have been used in
the decision theory literature to explain whether sequential conditioning is
sequence-independent or not. For example, \citet{Epstein2006} makes the
updating rule subjective, and \citet{Ortoleva2012} axiomatises departures
triggered by unexpected news." with:

> Decision theory offers axiomatic alternatives to Bayesian updating on events:
> \citet{Epstein2006} makes the updating rule itself subjective, and
> \citet{Ortoleva2012} axiomatises a change of prior triggered by news that the
> current prior deems unlikely. Both concern the response to a single event,
> not the order in which several inputs arrive. Order enters with
> \citet{Cripps2021}, …

(For Epstein, see `literature/epstein2006/` and audit M6, which was verified
against the primary on 2026-09-30.) If the citation must rest on the source
actually read, add "(see the restatement in \citealp{Ortoleva2024})" and a
`bibliography.bib` entry for Ortoleva (2024), *Annu. Rev. Econ.* 16:545-570,
doi:10.1146/annurev-economics-100223-050352.

## Not formalized

* **The "only if" direction of Theorem 2**, that Consequentialism and Dynamic
  Coherence imply a minimal HT representation. It needs Ortoleva's (2012)
  construction of `ρ` and `ε` from the family `{π_A}`, which the survey does not
  give. This is out of reach without the primary.
* `ρ` with infinite support, and the claim in fn. 18 that "ρ can always be
  constructed to allow a unique maximizer".
* The survey's equivalence with Inertial Updating (Dominiak et al. 2023,
  pp. 560 and 564) and with Myerson's CPS (Section 7, p. 564).
* The preference-level statements, which are covered only through their
  expected-utility (belief) forms. The survey says "the mapping is routine"
  (fn. 17).
