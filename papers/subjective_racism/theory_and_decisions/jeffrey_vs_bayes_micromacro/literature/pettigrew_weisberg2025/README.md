# Pettigrew, R. and Weisberg, J. (2025), "Jeffrey Pooling"

*Philosophers' Imprint* 25(8), July 2025, pp. 1-16, doi 10.3998/phimp.3806. It is published;
it is not a preprint or "forthcoming". The manuscript cites it once, at MS:277.

## What the paper proves

- **Equation (1)** (p. 3), upco: $P'(E)=P(E)Q(E)/[P(E)Q(E)+P(\bar E)Q(\bar E)]$. *Jeffrey
  pooling* pools the prior $P(E)$ with a source's opinion $Q(E)$ (Step 1), then Jeffrey
  conditionalizes on the result (Step 2).
- **Theorem 1** (Field; p. 3): "Upco ensures that Jeffrey pooling commutes for any **regular**
  P, and any Q and R." Regular means that $EF$, $E\bar F$, $\bar EF$ and $\bar E\bar F$ all
  have positive probability. Theorem 5 in the appendix is the countable-partition version,
  "whenever defined".
- **Theorem 2** (p. 6): "Among the monotonic, continuous, uniformity preserving, and
  symmetric pooling rules, only upco ensures that Jeffrey pooling commutes for any
  **regular** P, and any Q and R." P-W call extensionality "a tacit fifth assumption"
  (p. 6). The appendix version, Theorem 8 (p. 15), adds extensionality and is stated for
  finite partitions. The proof goes through Lemma 7 and Theorem 6, which is Wagner's
  necessity theorem.
- **Equations (2)-(3)** (p. 7). Field updating on $(E,\beta)$ is
  $P'(E)=\beta P(E)/(\beta P(E)+P(\bar E))$, and it equals upco with $Q(E)=\beta/(\beta+1)$.
  P-W's gloss, in their main text on p. 7: "when $P(E)=P(\bar E)$, Equation (3) delivers
  $P'(E)=\beta/(\beta+1)$. So if you have no prior opinion about E, you will defer to your
  sensory system's proposal." This gloss is **P-W's own**. P-W's footnote 9 says only that
  Field uses a log-scaled $\beta$, which he labels $\alpha$. Field never writes
  $\beta/(\beta+1)$.
- p. 9: "$\beta$ just is the Bayes factor". Theorems 3 and 4 are Wagner (2002)'s, restated
  for regular P.
- P-W restore commutativity by choosing the pooling rule that combines the **prior** with
  each source's opinion. Successive inputs are never pooled with each other. They also stress
  that upco is not claimed to be always best (p. 6).

## Lean

`lean/PettigrewWeisberg.lean` is a symlink to `lean/Literature/PettigrewWeisberg.lean`. The
file builds with no `sorry`. Every theorem, 54 in all, uses only
`[propext, Classical.choice, Quot.sound]`.

| Lean | Paper |
|---|---|
| `upco`, `linPool`, `opening_example` | Eq. (1); linear pooling; .4 and .8 give .6 linearly and $8/11\approx .73$ by upco |
| `field`, `eq3`, `no_prior_opinion`, `beta_is_bf` | Eq. (2); Eq. (3); P-W's p. 7 gloss; "β just is the Bayes factor" (p. 9) |
| `upcoV`, `upcoV_binary`, `jpool` | Definitions 2-3 (upco on a partition; Jeffrey pooling), finite partitions |
| `thm1` | **Theorem 1**, for regular P and any Q, R, on finite partitions (the paper states the binary case) |
| `thm5`, `route` | Theorem 5, "whenever defined"; both orders give $P(\omega)Q(E_i)R(F_j)$ renormalized |
| `example_P1`, `example_P2`, `example_other_order` | the p. 4 worked example, exactly: $8/11$; $(6,2,1,2)/11$; $7/11$; $21/29$; $(18,4,3,4)/29$; the other order via $(9,2,6,8)/25$ to the same $P''$ |
| `wagner_grid` | Theorem 6, the Wagner necessity step, $E$-half, as used in the proof |
| `lemma7` | **Lemma 7**, uniform distributions are neutral |
| `thm8_regular` | **Theorem 2 / Theorem 8, regular part** |
| `upco_regPres`, `upco_UP`, `upco_mono`, `upco_symm`, `upco_cont`, `upco_commutes` | upco satisfies every hypothesis of `thm8_regular`, so the theorem is not vacuous |

**Theorem 2 exactly as formalized.** A pooling operator on $n$-cell partitions has the type
`PoolOp n`. Because the type sees only the two vectors of cell probabilities, extensionality
(Def. 10) is built in. `thm8_regular` assumes that the operator is uniformity preserving,
monotonic, symmetric and continuous (Defs. 6-9), and that it makes Jeffrey pooling commute.
It concludes that the operator agrees with upco on regular inputs. The main-text Theorem 2
is the case $n=2$. The formalization differs from the paper in three ways.
1. It adds a hypothesis, `RegularityPreserving`: pooling regular distributions gives a
   regular distribution. P-W's proof divides by pooled values in (6) and (8) and uses the
   pooled result as a probability function, so it relies on this assumption without
   stating it. Upco satisfies it (`upco_regPres`).
2. The axioms are required only on regular inputs, and commutativity only on two $n$-cell
   partitions forming a grid, with regular P, Q, R. These are weaker hypotheses than the
   paper's.
3. **Not formalized:** the last paragraph of Theorem 8's proof, which extends agreement from
   regular inputs to all inputs by continuity.

## The project's question, not P-W's (labelled separately in the Lean file)

| Lean | Content |
|---|---|
| `upco_eq_self_iff` | $\mathrm{upco}(p,q)=q$ iff $p=\tfrac12$ |
| `naive_opinion` | the opinion upco needs to return a delivered credence $q$ is $\beta/(\beta+1)=q(1-p)/(p+q-2pq)$, which equals $q$ only at $p=\tfrac12$ |
| `PB_is_upco_pooling` | $\PB$ **is** Jeffrey pooling with upco, in either order, when each cue's pooled opinion is its likelihood matched against the **prior** marginal (`matchedOpinion`), not its delivered credence |
| `PB_vs_example` | on P-W's p. 4 numbers read as two delivered credences, pooling the credences themselves gives $18/29$ at $EF$, and $\PB$ gives $27/40$ |

So "$\PB$ = upco" is true only with the translation spelled out. Each cue's pooled opinion is
$\beta/(\beta+1)$, with $\beta$ the cue's Bayes factor against the prior. Read literally as
"upco of the prior and the delivered credence", it is false, except at a uniform prior.

## SymPy

`sympy/check_upco_vs_PB.py` runs 33 exact checks, prints PASS/FAIL for each, and exits 0 iff
all pass (33/33). It covers:

- the opening example;
- the full p. 4 example in both orders;
- that linear pooling does not commute on the same numbers;
- Theorem 1 symbolically for a regular 2x2 prior and any Q(E), R(F);
- Eqs. (2)-(3), the p. 7 gloss, and "β is the Bayes factor";
- the $\mathrm{upco}(p,q)\neq q$ algebra;
- that $\PB$ is upco-pooling with matched opinions, in both orders, and $27/40$ vs $18/29$;
- spot checks that upco satisfies the Theorem 2 hypotheses.

## Corrections to the earlier version of this README

- It attributed the "no prior opinion" gloss on $\beta/(\beta+1)$ to "Field's own gloss
  (footnote 9 in the paper)". The gloss is P-W's own main text (p. 7). P-W's footnote 9 only
  says that Field's $\alpha$ is a log-scaled $\beta$. Field has two footnotes and never writes
  $\beta/(\beta+1)$.
- It stated Theorem 2 without "for any **regular** P". The appendix version (Theorem 8) also
  needs extensionality.

## What the manuscript may and may not attribute

**MS:277 now reads:** "the literature restores sequence-invariance for sequential Jeffrey
updating by changing how successive inputs are pooled \citep{PettigrewWeisberg2025}".
P-W never pool successive inputs with each other. They pool the prior with each input.

**Suggested wording:** "Pettigrew and Weisberg restore sequence-invariance by pooling the
prior with each new input multiplicatively (upco) before the Jeffrey step
\citep{PettigrewWeisberg2025}."

**May attribute to P-W:**
- Upco-then-Jeffrey commutes for regular priors (Theorem 1, which they attribute to Field).
- Among monotonic, continuous, uniformity-preserving and symmetric pooling rules (with the
  tacit fifth assumption, extensionality), only upco does so for every regular prior
  (Theorem 2).
- Field updating on $(E,\beta)$ is Jeffrey pooling with upco, with $Q(E)=\beta/(\beta+1)$.

**Must not attribute to P-W:**
- Pooling of successive inputs with each other.
- That $\PB$ equals upco of the prior with the delivered credence. That is false unless the
  prior marginal is $\tfrac12$. If the manuscript wants to connect $\PB$ to upco, it must say
  that each cue's pooled opinion is its likelihood matched against the prior
  (`PB_is_upco_pooling`).
- The "no prior opinion" reading as Field's.
- Theorem 2 without "regular P" and without its list of axioms.
