# Augenblick, N. & Rabin, M. (2021), "Belief Movement, Uncertainty Reduction, and Rational Updating"

*Quarterly Journal of Economics* 136(2), 933-985. (Drive copy is the November 2020
working paper.)

## Claims formalized

Their Section 2 definitions, for a belief stream `θ = [θ_0, θ_1, ...]` of beliefs
in state 1:

* uncertainty `u_t(θ) = (1 - θ_t) θ_t`;
* **movement** `m_{t1,t2}(θ) = Σ_{τ=t1}^{t2-1} (θ_{τ+1} - θ_τ)²`;
* **uncertainty reduction** `r_{t1,t2}(θ) = Σ_{τ=t1}^{t2-1} (u_τ - u_{τ+1}) = u_{t1} - u_{t2}`.

**Proposition 1.** For any DGP and any `t1 < t2`, `EM_{t1,t2} = ER_{t1,t2}`.

**Corollary 1.** For a resolving DGP (terminal belief in `{0,1}`), `EM = u_0`.

**Proposition 4** (negative result, not formalized -- see below): for any stream of
*expectations* `v` and any `ε`, there is a DGP making `Pr(V = v) ≥ 1 - ε`, so an
expectations stream carries no testable content without a scale.

## Result

`lean/AugenblickRabin.lean` (symlink to `lean/Literature/AugenblickRabin.lean`;
builds under the repo's Lean project, no `sorry`, axioms `[propext,
Classical.choice, Quot.sound]` only):

* `excess_step` -- the paper's displayed one-period rewriting
  `m_{t,t+1} - r_{t,t+1} = (2θ_t - 1)(θ_t - θ_{t+1})`, proved by `ring`.
* `reduction_telescope` -- `r_{t1,t2} = u_{t1} - u_{t2}`.
* `movement_sub_reduction` -- the summed form.
* `prop1_of_orthogonal` -- **Proposition 1**, over a finite weighted index of
  histories.
* `orthogonal_of_martingale` -- the specialization `f = 2θ_t - 1` of the
  martingale property.
* `resolving` -- **Corollary 1**.

`sympy/check_prop1.py` (4/4 checks, exact rationals):

* the paper's **Table 1** reproduced row by row for the symmetric noisy-signal DGP
  (γ = 3/4, θ_0 = 1/2, two periods): stream `[1/2, 1/4, 1/10]` with `P = 5/16`,
  `m = 17/200`, `r = 4/25`, `m - r = -3/40`; and `[1/2, 1/4, 1/2]` with `P = 3/16`,
  `m = 1/8`, `r = 0` -- matching the published values exactly;
* Proposition 1 on that DGP (`EM = ER = 1/10`);
* the one-period identity symbolically (residual 0);
* Proposition 1 symbolically for a *general* two-period binary DGP with free prior
  and free per-state likelihoods, so the result is not an artefact of γ = 3/4.

## What formalizing revealed

**Proposition 1 is an algebraic identity plus one orthogonality condition.**
`excess_step` holds pointwise for *any* real sequence, with no probability in it.
So the entire probabilistic content of Proposition 1 is that the martingale
difference is orthogonal to the single instrument `2θ_t - 1`. `prop1_of_orthogonal`
therefore takes exactly that as its hypothesis, which is strictly weaker than the
full martingale property -- and the Lean proof needs nothing more. This matches the
paper's own Section 4 framing, where the martingale property is written
`E[f(θ_0,…,θ_t)(θ_t - θ_{t+1})] = 0` for an arbitrary instrument `f` and their test
is identified as the choice `f = 2θ_t - 1`. The formalization also shows the
weights need not be a probability measure: no nonnegativity, no normalization.

## Bearing on Paper B

**This is an instrument-choice paper, which is Paper B's genre.** AR pick two
summary statistics of a belief stream, prove an identity relating them, and turn
the discrepancy into a test -- explicitly asking "why use this particular
instrument?" and disclaiming universality ("we are not claiming that our instrument
is universally more powerful to detect all deviations from rationality"). Paper B
asks the same kind of question for sequence-dependence and answers it *completely*:
Proposition PRO characterizes the whole space of smooth statistics, necessary and
sufficient. That completeness is the axis on which Paper B is stronger; AR select a
good instrument, Paper B classifies all of them.

**Their Proposition 4 is the closest published precedent for Paper B's protected
class.** An expectations stream has *no* testable content ("for any observed stream
of expected value predictions, there is some DGP in which that exact stream occurs
with arbitrarily high probability"), while a binary-belief stream does. That is the
same shape of finding as Paper B's: one summary of a belief retains testable content
about the updating process and another does not. The reasons differ -- AR's is the
absence of a scale, Paper B's is an order-in-`c` gap from the route-plane geometry --
so it is a precedent for the *kind* of result, not a structural cousin.

**Correction to an earlier positioning claim.** Benchmark-freeness is *not* a
distinguishing feature of Paper B's Proposition ORD. AR's driving assumption, held
throughout, is that "the normatively-correct beliefs are unobserved"; they call
their approach agnostic. The property is shared, and evidently valued in this
literature. What does distinguish ORD is that it needs a single measurement per
evaluator with reading order varying across evaluators, whereas AR need a belief
*stream* over time per agent.

## Not formalized

Proposition 2 and Corollaries 2-5 (variance bound and the finite-sample statistical
cutoffs), Proposition 3 (many states), Propositions 4-5 and Corollaries 6-7
(expectations; Proposition 4 is an existence-of-DGP statement whose content is
measure-theoretic rather than algebraic), and Propositions 6-7 (the four
psychological biases).
