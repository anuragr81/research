# Cripps, M.W. (2021), "Divisible Updating"

Working paper, Department of Economics, UCL. The version on Drive (`cripps.pdf`,
39 pp.) is dated "Originally 2019. This version November 4, 2021". It includes
the appendix proofs of Lemma 1, Props 1-6 and Lemmas 2-3.

## Claims formalized

The setup (Section 3, pp. 6-9):

* A finite state space `Θ`.
* A full-support belief `μ ∈ Δ°(Θ)`.
* An experiment `E_n`: full-support signal distributions `pᶿ` on `n` signals.
* An updating process `U = (U_n)`, which maps `(μ, E_n)` to a profile of `n`
  posteriors, one for each signal (p. 7).

The four axioms:

* **Axiom 1 (Uninformativeness), p. 7.**
* **Axiom 2 (Symmetry), p. 7.** Relabelling signals permutes the profile.
* **Axiom 3 (Divisibility), p. 8.** It has two parts: (a) consequentialism;
  (b) the two-step process "`s = 1` vs `s ≠ 1`, then the residual experiment".
* **Axiom 4 (Non-Dogmatic), p. 9.**

**Definition 1, p. 11 (divisible updating).**
`u(μ,p_s) = F⁻¹(F(μ)∘p_s / F(μ)ᵀp_s)` for a bijection `F`: map to a shadow
prior, apply Bayes, map back.

**Proposition 1, p. 11.** `U` satisfies Axioms 1-4 iff it is divisible.

**The p. 9 remark.** "symmetry implies that the order in which the signals are
revealed can be changed … these axioms imply that reversing the order in which
two signals arrive has no effect on the ultimate beliefs."

## Result

`lean/Cripps.lean` is a symlink to `lean/Literature/Cripps.lean`. It builds
under the repo's Lean project with no `sorry`. Every headline theorem uses only
`[propext, Classical.choice, Quot.sound]`.

The paper's content:

* `bayes_bayes`: two Bayes updates equal one update on the product likelihood.
* `divRule_seq`, `divRule_comm`: every divisible rule satisfies
  `u(u(μ,x),y) = u(μ,x∘y)`, so independent signals commute. This is the last
  display of the proof of Prop. 1, p. 29.
* `prop1_if`: **the "if" direction of Proposition 1.** A divisible `U`
  satisfies Axioms 1-4. `bayes_isDivisible` is the special case of Bayes
  (`F = id`).
* `lemma1_i`: Lemma 1(i), for three signals.
* `seq_eq_product`: **Lemma 1(iii) in sequential form.** Symmetry and
  Divisibility alone give `u(u(μ,x),y) = u(μ,x∘y)` for all `x, y ∈ (0,1)^Θ`.
  Axiom 1, Axiom 4 and the bijection `F` are not needed.
* `order_invariance`: **the p. 9 remark as a theorem.** Under Axioms 2 + 3,
  `u(u(μ,x),y) = u(u(μ,y),x)`.

The project's question, labelled as such in the file. States are
`Θ = A × B` (Paper B: `Fin 2 × Fin 2`).

* `jeffreyA_eq_bayes_matched`, `jeffreyB_eq_bayes_matched`: a Jeffrey step is a
  Bayes step on the matched likelihood `q_a / μ(A=a)`, which depends on the
  current belief. This is Paper B's Prop. IMM stated in Cripps's language.
* `composite_AB_eq_bayes`: the composite `J_B ∘ J_A` is **one Bayes update**.
  Its `A`-likelihood is matched to the prior. Its `B`-likelihood is matched to
  the *intermediate* belief `J_A μ`.
* `rigidRule φ`: the rigid-credence reading. A cue fixes the delivered credence
  `φ(p_s)` whatever the prior. For **every** `φ`:
  * `rigid_symmetry`: Axiom 2 holds;
  * `rigid_not_uninformative`: Axiom 1 fails;
  * `rigid_not_nonDogmatic`: Axiom 4 fails, because `μ(B|A)` is frozen;
  * `rigid_divisible_imp_evidence_blind` and `rigid_divisible_imp_const`:
    Axiom 3, given Axiom 2, forces `φ` to be constant on `(0,1)^Θ`, so the
    rule would ignore its evidence.

`sympy/check_cripps.py` runs 16 of 16 checks, exact, and exits 0.

* Bayes and geometric probability weighting (Sec. 4.1, symbolic `a`) satisfy
  `u(u(μ,x),y) = u(μ,x∘y)` and are order-invariant.
* Sec. 4.1's closed form equals `F⁻¹(Bayes(F^a μ))`.
* Cripps's non-divisible example (Epstein-Noor-Sandroni, eq. (1)) is
  order-dependent.
* On Paper B's model:
  * `J_A(P;q) = Bayes(P, q/P_A)`.
  * The B-marginal after `J_A` moves by exactly `c(q₀−α)/(α(1−α))`, so the
    re-matched B-likelihood differs from the prior-matched one iff
    `c(q₀−α) ≠ 0`.
  * With both likelihoods held fixed, Bayes commutes and returns `P^B`.
  * The rigid translation fails Axioms 1, 3(b) and 4, and satisfies Axiom 2.

## What formalizing revealed

1. **The p. 9 remark is a theorem, not only a remark.** It follows from Axioms 2
   and 3 through Lemma 1(i) and (iii) (`seq_eq_product`, `order_invariance`).
   The two signals are arbitrary likelihood vectors on `Θ`. So the result
   covers two conditionally independent binary experiments on *different*
   partitions (an `A`-measurable `x` and a `B`-measurable `y`), not only nested
   revelation within one experiment.
2. **What is being made order-invariant is a rule applied to fixed
   experiments.** Order-invariance says nothing about a procedure that changes
   the experiment it feeds in depending on the order. That is exactly what the
   Jeffrey composite does under Paper B's own matched-likelihood bridge.
3. **The easy direction of Prop. 1 needs `|Θ| ≥ 2`** (`Nontrivial Θ`) for
   Axiom 4. With one state, `(p₁, 1−p₁)` is not a full-support experiment. The
   paper does not state this, but it is harmless.

## Bearing on Paper B

The Jeffrey composite is a map from (prior, delivered credences) to a posterior.
It is not a Cripps rule, because Cripps's rules take experiments with
prior-independent likelihoods (p. 7). Any statement about which axioms "it"
satisfies needs a translation from cues to experiments first. The two canonical
translations give opposite pictures, and **neither matches the manuscript**:

| Translation | Ax. 1 | Ax. 2 | Ax. 3 | Ax. 4 | Source of the order effect |
|---|---|---|---|---|---|
| Matched likelihood `ℓ = q/P(A)` (Paper B's Prop. IMM) | ✓ | ✓ | ✓ | ✓ | the second cue's likelihood is re-matched to the intermediate belief, so the two orders feed *different experiments* into the same Bayes rule |
| Rigid credence `q = φ(evidence)` | ✗ (any `φ`) | ✓ | ✗ unless `φ` is constant | ✗ (any `φ`) | a Jeffrey step discards the prior's `A`-marginal |

## Audit findings (2026-09-29)

These findings respond to `verify_updating_theory.md` §1, which covers the
manuscript at `PAPER_B_MANUSCRIPT.tex` lines 276-277 and its footnote.

* **C1 ("four axioms"): VERIFIED.**
* **C2 ("Cripps2021 shows that symmetry and divisibility jointly force
  sequence-independence"): VERIFIED, and stronger than the audit allowed.**
  The audit rated this VERIFIED-WITH-CAVEAT, on the ground that p. 9 is only an
  informal remark about nested revelation within one experiment. The
  formalization proves the claim from Axioms 2 + 3 (`order_invariance`), and
  for arbitrary pairs of conditionally independent binary experiments,
  including ones on different partitions. So the audit's caveat (ii) is too
  narrow. Caveat (iii) stands: Symmetry is relabelling-invariance. It is used
  in the proof to identify `U₂²(μ,p,1−p)` with `u(μ,1−p)`, and it is not
  itself a statement about evidence order. What remains true is that the
  invariance concerns *fixed-likelihood experiments*.
* **C3 ("so the correlated two-cue composite of Prop. DIV fails
  divisibility"): FALSE under the manuscript's own bridge, and otherwise
  ill-posed.** The audit's contrapositive was "any extension to experiments
  must violate Divisibility or Symmetry". That holds only if the extension
  feeds the *same* two experiments in both orders. The matched-likelihood
  translation does not: it satisfies Divisibility and Symmetry (it is Bayes)
  and is still order-dependent, because it re-matches the second likelihood.
  The claim holds only under the rigid-credence translation.
* **C4 ("…and, of the four axioms, only divisibility"): FALSE under both
  canonical translations.** The matched reading satisfies all four axioms. The
  rigid reading fails Uninformativeness and Non-Dogmatic for *every*
  evidence-to-credence map (`rigid_not_uninformative`,
  `rigid_not_nonDogmatic`), and fails Divisibility unless it ignores its input.
  No translation was found under which the composite fails Divisibility alone.
* **C5, C6 (footnote): VERIFIED.** The footnote's admission ("the statement
  here is behavioural") is correct, and it is inconsistent with the main-text
  axiom-by-axiom claim.
* **C7 (bib): VERIFIED.** The file name ("…submission2…") suggests a journal
  submission, so check whether a published version exists.

### Proposed corrected wording (text only, not applied)

Replace "\citet{Cripps2021} shows that symmetry and divisibility jointly force
sequence-independence, so the correlated two-cue composite of
Proposition~\ref{prop:DIV} fails divisibility---and, of the four axioms, only
divisibility." and its footnote with:

> \citet{Cripps2021} characterises updating rules on fixed-likelihood experiments
> by four axioms. His Symmetry and Divisibility axioms together imply that two
> conditionally independent signals yield the same posterior in either order.
> The composite of Proposition~\ref{prop:DIV} lies outside his domain, because a
> Jeffrey step takes a delivered credence rather than an experiment. If each cue
> is read as its matched likelihood $q_i/P(A{=}i)$
> (Proposition~\ref{prop:IMM}), every step is a Bayes update, which satisfies
> all four axioms. The sequence effect then arises because the second cue's
> likelihood is re-matched to the intermediate belief, so the two orders
> process different experiments. In Cripps's terms, the non-commutativity
> therefore lies in the map from cues to likelihoods, not in the updating rule.

If a footnote is wanted:

> Reading a cue instead as fixing the credence regardless of the prior makes the
> rule violate Uninformativeness and Non-Dogmatic, and violate Divisibility
> unless it ignores its input. See `literature/cripps2021/`.

## Not formalized

* The "only if" direction of Proposition 1, which needs the Aczél-Hosszú
  solution of the multidimensional translation equation.
* Lemma 1(ii) (homogeneity) as a standalone statement.
* Corollary 1 (continuity).
* Result 1 (consistency).
* Props 2-3 (positive learning, over- and under-reaction).
* Props 4-5 (unbiasedness, Bayes characterisation).
* Prop. 6 and Lemma 3 (sequential sampling).
