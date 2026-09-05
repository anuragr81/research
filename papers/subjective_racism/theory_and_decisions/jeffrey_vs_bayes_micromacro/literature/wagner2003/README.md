# Wagner, C.G. (2003), "Commuting Probability Revisions: The Uniformity Rule" (In Memoriam Richard Jeffrey)

*Erkenntnis* 59(3), 349-364. This is the "considered experiences" fix the handover
flags as directly load-bearing in the Lange/Cassell exchange.

## Claims formalized

- **Theorem 2.1**: generalizes Wagner (2002)'s two-partition commutativity result to
  an **arbitrary atomic $\sigma$-algebra**. Given a general revision schema
  $p\to q\to R$ and $p\to Q\to r$, if the Bayes-factor identities (2.2)
  $\beta_{r,Q}(A:B)=\beta_{q,p}(A:B)$ and (2.3) $\beta_{R,q}(A:B)=\beta_{Q,p}(A:B)$ hold
  for all atoms $A,B$, then $r=R$. Proof is three lines: both routes multiply $q$'s (or
  $Q$'s) odds by the same factor.
- **Remark 2.2**'s explicit formula (2.7): $r(A) \propto \pi_{q,p}(A)\,\pi_{Q,p}(A)\,p(A)$,
  reconstructing $r$ directly as a product of the two independent reweightings applied
  to $p$ at once, bypassing the two-step schema.
- **Section 5**: the Uniformity Rule (Bayes factors) is contrasted with two naive
  alternative indices of "identical learning" -- the difference $q(A)-p(A)$ and the
  probability factor $\pi(A)=q(A)/p(A)$ -- against three criteria (commutativity;
  reproducibility on any partition; not overly restricting the prior). Headline result:
  for a **2-atom** partition, the $\pi$-index construction is not merely restrictive but
  vacuous -- unless $Q=p$ exactly, no probability $r$ satisfies the $\pi$-index identity
  at all.

## Result

All verified in `sympy/check_theorem21.py`, deliberately on a **4-atom algebra with no
$2\times2$ product structure** (unlike Paper B's own two-binary-partition setup), to
test the theorem's actual generality rather than re-verify what the Lean development
already covers:

- Theorem 2.1 holds identically for arbitrary symbolic per-atom reweighting factors,
  and numerically.
- Formula (2.7) reproduces the schema-built $r$ exactly.
- The $\pi$-index absurdity: solving the exact (non-proportional) identity
  $r(A_1) = Q(A_1)q(A_1)/p(A_1)$, $r(A_2)=(1-Q(A_1))q(A_2)/p(A_2)$ for the unique
  $Q(A_1)$ making $r$ sum to 1 gives $Q(A_1)=p(A_1)$ exactly -- confirming that any
  $Q\ne p$ admits no valid $r$ at all under the $\pi$-index.

Not attempted: the difference-index ($d$) failure of criterion II (no valid
kinematical revision on a finer partition) -- this is a structural/qualitative point
in the paper (no canonical refinement exists), not a single identity to check.

Sections 3-4 (applying the Uniformity Rule to the old-evidence problem in confirmation
theory) are a different application area -- not about the same two-cue sequence-effect
question Paper B addresses -- and were read but not formalized.

No Lean formalization: Theorem 2.1's proof is three lines of odds-multiplication
algebra valid for arbitrary atomic algebras; a machine-checked instance would only
reprove what `LemmaSEP.lean`'s general-$N$ separability argument already covers.

## Bearing on Paper B

This is the cleanest statement available of *why* Bayes-factor consistency is the
right notion of "identical learning" and not an arbitrary convention: the two most
natural alternatives (raw difference, raw ratio) each fail one of three
minimal-plausibility criteria, and the ratio index fails catastrophically (vacuously,
at just two atoms) rather than merely imperfectly. If the manuscript's motivation
section is ever expanded to argue for the delivered-credence/Bayes-factor framing more
carefully (the "two-horn" direction discussed earlier), Section 5 here is the strongest
available citation for "why not just use $q(A)-p(A)$ or $q(A)/p(A)$ instead."
