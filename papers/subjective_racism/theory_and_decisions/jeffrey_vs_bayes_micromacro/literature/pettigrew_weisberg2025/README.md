# Pettigrew, R. and Weisberg, J. (2025), "Jeffrey Pooling"

*Philosophers' Imprint* 25(8), 1-16.

The cleanest, most recent statement of the state-of-the-art commuting rule. This pass
specifically re-checks the memo's claim "$\PB$ is Field's update, hence upco, hence the
rule Pettigrew-Weisberg endorse" -- flagged as likely wrong in the very first alignment
assessment, before any primary source had been read.

## Claims formalized

- **Theorem 1** (attributed to Field): "upco" pooling,
  $P'(E)=P(E)Q(E)/[P(E)Q(E)+P(\neg E)Q(\neg E)]$, followed by Jeffrey conditionalizing on
  the pooled value, ensures Jeffrey pooling commutes for any regular $P$.
- **Theorem 2**: among monotonic, continuous, uniformity-preserving, symmetric pooling
  rules, only upco ensures commutativity for arbitrary $Q,R$ (an axiomatic uniqueness
  result -- not attempted here, see below).
- **The Field/upco identification** (their eq. 2 vs eq. 1/3): "Field updating on
  $(E,\beta)$" -- $P'(E)=\beta P(E)/(\beta P(E)+P(\neg E))$, where $\beta\ge0$ is the
  *odds-scale* strength of a sensory experience -- is algebraically the same formula as
  upco, with $Q(E):=\beta/(\beta+1)$.
- **Theorem 3/4** (attributed to Wagner 2002): restated without new content beyond what
  `literature/wagner2002/` already verifies directly from the primary source.

## The precise resolution of the $\PB$-vs-upco question

The key subtlety, verified in `sympy/check_upco_vs_PB.py`:

1. **Plugging Paper B's delivered credence $q$ directly into upco's two-argument
   formula as $Q(E)$ does *not* reproduce $\PB$.** $\mathrm{upco}(p,q) - q$ is a nonzero
   rational function of $(p,q)$, vanishing only at $p=\tfrac12$. This is exactly what
   the original assessment found, and it stands: the memo's "$\PB=\mathrm{upco}$" is
   **false** if read as "compute upco of the prior and the delivered credence."
2. **Field's own $\beta$-parametrized construction is a different object.** $\beta$ is
   not the delivered credence -- it is $\beta:=\ell_1/\ell_0$, the *ratio* of Paper B's
   own per-outcome Bayes factors $\ell_i=q_i/P(A{=}i)$. Plugging this $\beta$, *derived
   jointly from $q$ and the current prior $p$*, into Field's formula (2) reproduces $q$
   exactly -- a tautology confirming Proposition IMM (Paper B's own single-cue
   full-deference result), not a substantive new claim.
3. **PW's own identity holds exactly**: Field's eq. (2) with this $\beta$ literally
   equals $\mathrm{upco}(p,\,Q(E))$ for $Q(E):=\beta/(\beta+1)$ -- confirmed
   algebraically, residual 0.
4. **But $Q(E):=\beta/(\beta+1)$ is *not* $q$.** Simplified: $Q(E)=q(p-1)/(2pq-p-q)$,
   equal to $q$ only at $p=\tfrac12$. It is a different, derived number representing
   "what a neutral ($p=\tfrac12$) agent would conclude from the same evidence" -- Field's
   own gloss (footnote 9 in the paper): $\beta/(\beta+1)$ is what you'd defer to "if you
   have no prior opinion."

So: $\PB$ genuinely **is** Field's 1978 procedure (independently verified cell-by-cell
in the memo's `checks/c5_benchmark_is_field.py`, and via the eq.(7) commutativity check
in `literature/field1978/`), and Field's procedure **is**, by PW's own algebra, the
same construction as upco -- but only once translated through $\beta$. Calling $\PB$
"upco" without that translation is misleading: a reader who computes
$\mathrm{upco}(\text{prior}, \text{delivered credence})$ literally, as the memo's phrasing
invites, gets the wrong number except at a uniform prior. The manuscript itself does not
make this claim (it correctly describes $\PB$ via the Bayes-factor formula
$\PB(i,j)\propto P(i,j)\ell^A_i\ell^B_j$, not via upco); the imprecision is specific to
the alignment memo's gloss.

Not attempted: Theorem 2's axiomatic uniqueness (a characterization over the space of
*all* monotonic/continuous/uniformity-preserving/symmetric pooling rules) -- this is a
general impossibility-style theorem, not a single identity to check computationally.

## Bearing on Paper B

Confirms and sharpens the earlier finding (problem #1 in the standalone problems
memo): the manuscript's own wording ("Bayes-factor benchmark," "sequence-invariant
\citep{Wagner2002}") is accurate and should be kept exactly as is; any future addition
identifying $\PB$ with "upco" for a reader unfamiliar with the $\beta$-translation
should either state the translation explicitly or avoid the word "upco" and just say
"Field/Wagner's Bayes-factor combination."
