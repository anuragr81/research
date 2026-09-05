# Wagner, C.G. (2002), "Probability Kinematics and Commutativity"

*Philosophy of Science* 69(2), 266-278 (preprint pagination used below).

This is the paper the manuscript cites (`\citep{Wagner2002}`) for the claim that the
Bayes-factor benchmark $\PB$ is a sequence-free reference. It generalizes Field (1978)
to countable partitions and, unlike Field, proves the Bayes-factor identities are
**necessary**, not just sufficient, for commutativity.

## Claims formalized

Schema (3.1): $p\xrightarrow{E}q\xrightarrow{F}r$ (route 1) and
$p\xrightarrow{F}q'\xrightarrow{E}r'$ (route 2), where the second-step targets need
**not** match the first route's marginals -- the fully general two-cue schema (Paper
B's setup is the special case with matching returns).

- **Theorem 3.1** (sufficiency, generalizing Field): if the Bayes-factor identities
  (3.2) $\beta_{r',q'}(E_{i_1}:E_{i_2})=\beta_{q,p}(E_{i_1}:E_{i_2})$ and (3.3)
  $\beta_{q',p}(F_{j_1}:F_{j_2})=\beta_{r,q}(F_{j_1}:F_{j_2})$ hold, then $r=r'$.
- **Theorem 4.1** (necessity, Wagner's own contribution beyond Field/Diaconis-Zabell):
  under mild non-degeneracy ((4.3)-(4.4), which full support trivially satisfies),
  $r=r'$ implies both (3.2) and (3.3).

## A transcription trap worth recording

The Google Drive copy of this paper OCRs primes unreliably: `pdftotext` renders
$\beta_{r',q'}$ as `βr ,q ` (both primes silently dropped as bare spaces), which reads
exactly like the *unprimed* $\beta_{r',q}$ -- a cross-route quantity comparing route 2's
final measure to route 1's *intermediate* one. That misreading was tested first and
produces a **clean, reproducible counterexample** (a genuine algebraic identity, not a
numerical accident) to the misread "theorem." Only rendering the actual page image
(`Theorem 3.1`, p.4) resolved it: condition (3.2) is $\beta_{r',q'}$, both primed --
route 2's *own* Bayes factor for its second step, compared to route 1's own Bayes
factor for its first step. This is the conceptually sensible reading ("the same
evidence carries the same Bayes factor whichever position it's read in"), and matches
(3.3)'s already-unambiguous same-chain structure ($q'$ vs $p$, chain 2's first step;
$r$ vs $q$, chain 1's second step).

A second, independent bug surfaced during the same debugging pass: $q'(E_i)$ (the
*derived*, generally-shifted $E$-marginal of $q'$ after the $F$-update) is not the same
number as $g$ (the $F$-update's own target $q'(F_1)=g$) -- conflating the two silently
produces the same kind of spurious "counterexample." Both bugs are recorded here
because they are exactly the errors a second attempt at this formalization would
repeat.

## Result

Both theorems verified in `sympy/check_theorems.py`, on Paper B's own $2\times2$,
$(\alpha,\beta,c)$-parametrized prior:

- Theorem 3.1: solving the two Bayes-factor equations for the route-2 targets
  $(f_1,h_1)$ given route-1's $(e_1,g_1)$ reproduces $r=r'$ exactly, symbolically, in
  all four cells.
- Theorem 4.1: fixing a full-support numeric prior and $e_1$, solving $r=r'$ for
  $(f_1,h_1)$ leaves one free parameter $g_1$ (a genuine one-parameter family of
  commuting schemas); both (3.2) and (3.3) residuals vanish identically along the
  entire family, not just at isolated points.

No Lean formalization: both theorems are proved for arbitrary countable partitions via
an algebraic argument (solving rigidity identities pairwise); what's checked here is
their restriction to Paper B's own two-cue, $2\times2$ instance, which the existing
Lean development (`JeffreyOrder/PropIMM.lean`, `LemmaSEP.lean`) already covers at the
level that matters for the paper (matching-target routes, i.e. Field's simpler
special case).

## Bearing on Paper B

This is the strongest available justification for calling $\PB$ "the" sequence-free
benchmark rather than "a" sequence-free benchmark: Theorem 4.1 shows that under mild
non-degeneracy, Bayes-factor consistency is not merely one sufficient route to
commutativity among others -- it is the *only* route. Any two-cue schema that happens
to commute, on a full-support prior, must already be Bayes-factor-consistent. This
strengthens the manuscript's citation of Wagner (2002) at line ~180 beyond Field's
weaker sufficiency-only result, and is worth stating explicitly if the related-work
discussion is expanded.
