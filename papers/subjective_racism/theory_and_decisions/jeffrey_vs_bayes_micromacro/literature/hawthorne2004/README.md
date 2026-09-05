# Hawthorne, J. (2004), "Three Models of Sequential Belief Updating on Uncertain Evidence"

*Journal of Philosophical Logic* 33(1), 89-123.

The most consequential paper for positioning Paper B: it names Paper B's own updating
model and its commutation condition, and supplies two worked examples richer than
Paper B's 2x2 (a hidden hypothesis $C$ plus two observable bases $E,F$, not just two
observable attributes) that ground exactly the same phenomenon the manuscript studies.

## Claims formalized

- **The Amnestic Update-Factor Thesis**: for any state $e$ affecting basis $\{E_i\}$,
  $Q_{\alpha de}[E_i]=Q_{\alpha e}[E_i]$ for any other state $d$ and sequence $\alpha$ --
  i.e. the most recent basis-affecting state completely overwrites all earlier influence
  on that basis. "Amnestic Updating is just Standard Sequential Updating -- Jeffrey's
  original approach" (this is the formal name for the manuscript's undefined "pinning").
- **The medical diagnosis example** (Section 5): hidden hypothesis $C$ (lung cancer),
  two conditionally-independent-given-$C$ observable bases $E$ (chest x-ray) and $F$
  (sputum cytology), prior $P(C)=.5$, likelihoods $P(E{=}1\mid C)=P(F{=}1\mid C)=.95$,
  $P(E{=}1\mid\lnot C)=P(F{=}1\mid\lnot C)=.05$. Soft cues deliver credences
  $Q_f(F{=}1)=.10$, $Q_e(E{=}1)=.90$. Claimed: $Q_f[C]=.14\to Q_{fe}[C]=.68$ one order,
  $Q_e[C]=.86\to Q_{ef}[C]=.32$ the other -- a $.18$ swing either direction from the
  symmetric $.5$ prior, despite the two test reports being "symmetric" and intuitively
  expected to cancel.
- **The commutation criterion**: two states $e,f$ commute for amnestic updating iff
  neither's basis-marginal is moved by the other -- Hawthorne's own footnote attributes
  this exactly to Diaconis-Zabell (1982) Theorem 3.2 (already verified in
  `literature/diaconis_zabell1982/`).
- **The Likelihood-Ratio ("Field") Updating example**, same section 7: same prior, but
  cues deliver *likelihood-ratio factors* instead of credences -- $\mathrm{LR}[f,F,\lnot
  F]=.50$, $\mathrm{LR}[e,E,\lnot E]=2$ -- giving $Q_f[C]=.35\to Q_{fe}[C]=.50$
  (returns exactly to the prior, the two factors constructed to cancel), and claimed to
  be order-independent regardless of the specific factor values.

## Result

Amnestic and Likelihood-Ratio examples verified exactly in `sympy/check_examples.py`;
the Update Reordering Theorem in `sympy/check_reordering_theorem.py`. Examples on the
full 3-variable joint
$P(C,E,F)=P(C)P(E\mid C)P(F\mid C)$ (8 atoms, not Paper B's 2x2):

- Amnestic sequence: $Q_f[C]=7/50=.14$, $Q_{fe}[C]=24689/36256\approx.681$ (rounds to
  Hawthorne's $.68$); $Q_e[C]=43/50=.86$, $Q_{ef}[C]=11567/36256\approx.319$ (rounds to
  $.32$) -- both orders reproduced exactly.
- Cross-basis influence confirmed directly: the $F$-update alone moves $P(E{=}1)$ from
  $\tfrac12$ to $22/125$, and the $E$-update alone moves $P(F{=}1)$ from $\tfrac12$ to
  $103/125$ -- exhibiting why the commutation criterion fails here.
- Likelihood-ratio sequence: $Q_f[C]=7/20=.35$ exactly, $Q_{fe}[C]=Q_{ef}[C]=1/2$ exactly
  -- order-independence confirmed both ways, not just asserted.

- **The Update Reordering Theorem** (Section 9): a necessary-and-sufficient condition
  for order-independence of an arbitrary finite update sequence, strictly weaker than
  Extended Rigidity. Two states $d$ (basis $\{D_i\}$) and $\gamma$ (basis $\{E_j\}$,
  distinct) commute iff, for each $Q_{\alpha d}$-*compatibility class* (the set of
  $E_j$'s compatible with a given $D_i$), there is *some* ratio $r$ -- possibly
  different for each class -- relating $\gamma$'s normed-likelihood factors before and
  after $d$ is incorporated. Extended Rigidity is the special case where all classes
  share the *same* $r$; the theorem's point is that this is not required when
  compatibility classes don't overlap.

## Formalizing the Reordering Theorem's non-overlapping-classes content

Paper B's own two-cue case has two 2-element bases, where the compatibility-class
structure collapses to the single-class case already covered by Diaconis-Zabell's
Theorem 3.2. To test the theorem's actual generalization -- genuinely disjoint
compatibility classes, each free to carry its own $r$ -- I built a case with a coarser
basis $D\in\{1,2\}$ and a finer basis $E\in\{1,2,3,4\}$ with **block-diagonal support**:
$(D{=}1)$ co-occurs only with $E\in\{1,2\}$, $(D{=}2)$ only with $E\in\{3,4\}$, so
$\{E_1,E_2\}$ and $\{E_3,E_4\}$ are two disjoint compatibility classes.

Fixing the $D$-update's target at $(s_1,s_2)=(\tfrac34,\tfrac14)$ and solving
symbolically for which $E$-update targets $(t_1,t_2,t_3,t_4)$ make the two orders
($D$-then-$E$ vs $E$-then-$D$) agree exactly gives: $t_1+t_2=s_1$ and $t_3+t_4=s_2$,
with $t_1$ and $t_3$ each **completely free**. Two concrete solutions with markedly
different within-block splits ($t_1/w_1=1$ vs $t_1/w_1=5$ at the corresponding prior
mass $w_1$) both commute exactly, confirming order-independence holds without any
single global ratio relating all four $E_j$'s to the prior -- exactly the theorem's
claim that its condition is weaker than Extended Rigidity, verified as a genuine
solution family rather than merely asserted.

Not formalized: the fully general theorem statement for arbitrary "suitable sequences"
of any length on any number of bases (a general combinatorial/measure-theoretic
statement); the Extended Rigidity / Basis-Overwrite and Basis-Commuting variants
(Section 8), which are further refinements of the Likelihood-Ratio model not directly
instantiated by Paper B's setup.

No Lean formalization: both examples are single numeric instances (not general
identities over a free prior/parameter space), so a machine-checked proof would only
re-verify specific rational arithmetic already confirmed exactly by SymPy.

## Bearing on Paper B

This is the primary source for the manuscript's own vocabulary. Hawthorne's medical
example is structurally identical to Paper B's setup but with an explicit *causal*
generative story (a hidden hypothesis driving two conditionally independent
observables) rather than a directly-specified joint over two attributes -- worth
citing as a second worked example alongside (or instead of) Döring's, since unlike
Döring's disjunctive-cue counterexample, Hawthorne's cues *are* attribute-local (each
soft cue targets only its own basis $E$ or $F$), yet the order effect is still large
(a $.18$ swing on the hypothesis $C$) -- this is the amnestic/pinning phenomenon in its
purest form, uncomplicated by the locality question Döring's example raises. The
Likelihood-Ratio companion example is the cleanest available illustration that
*exactly the same likelihoods and reports*, interpreted as Bayes-factor content rather
than delivered credence, exactly cancel and commute -- concretely dramatizing the
manuscript's own $\PJ$-vs-$\PB$ contrast with a $.18$-vs-$0$ side-by-side comparison,
in Hawthorne's own numbers rather than Paper B's abstract $(\alpha,\beta,c,q,r)$.

The Reordering Theorem's non-overlapping-compatibility-classes result is a genuine
generalization beyond anything the manuscript's own two-attribute Lean development
covers: Paper B's model has exactly two 2-element bases, where "commutes" collapses to
a single Diaconis-Zabell-style condition. Hawthorne's theorem shows that with a *third*
correlated variable structuring the two cues' interaction (more than two attributes,
or a hierarchy of hypotheses as in the medical example), commutation can survive
without any single global consistency ratio -- only block-by-block consistency. If
Paper B's discussion of scope (Section on relaxing assumptions) ever extends to $N>2$
attributes, this is the relevant reference for what replaces the single prior-covariance
condition $c=0$.
