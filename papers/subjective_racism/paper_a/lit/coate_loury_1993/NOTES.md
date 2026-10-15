# Coate and Loury (1993), notes

## What was verified

- Thirteen quotations are found on the pages they cite, in the cached text
  layer, by exact containment after normalising whitespace, quote marks and
  end-of-line hyphenation (`lit/litcheck.py`, LC-2).
- Eqs. (1) and (2), the EE locus of Proposition 1's proof, the
  no-investment equilibrium, and the dynamics of the uniform example
  (eq. 9) are Lean theorems in the paper's notation (LC-4).
- Our reading is consistent with the source on these points. The source
  itself is not reproved.

## What could not be verified

- Proposition 4 and condition (5). Paper A relies on neither.
- Whether the convergence claimed after eq. (9) holds for the paper's
  exact start is covered by the stronger Lean statement (CL-D2).

## The parallel with Paper A, and where it ends

1. **Same.** Both models put a group's standing, or the belief that tracks
   it, on a one-dimensional map whose fixed points are self-confirming, with
   several fixed points of different stability (CL-Q1 to CL-Q4, Paper A M5
   and M6). Coate and Loury's equilibrium beliefs are correct by
   definition, which is the property Paper A's re-learned prior has at a
   fixed point.
2. **Same.** A small intervention leaves the economy in the basin it
   started in, and a large one on the worker side removes the low
   equilibria (CL-Q10 to CL-Q12, Paper A M6, M9 and M11).
3. **Different channel.** Their beliefs act through employers' assignment
   standards on workers' investment, within one generation, followed by an
   adjustment process. Paper A's act through refused contact on the next
   generation's standing, with convergence and mobility forces that their
   model does not have.
4. **Different floor.** Zero is always an equilibrium for them (CL-Q5). In
   Paper A standing 0 maps to `μ > 0` and is never a fixed point (M4).
5. **Not their patronizing equilibrium.** Patronizing needs the
   equal-assignment constraint (CL-Q7, CL-Q9). Paper A has no such
   constraint. The predecessor's identification of the closure trap with
   "the patronising equilibrium" is not supported by the source. The
   closure trap is the analogue of their discriminatory equilibrium of
   Definition 1.
6. **No closed form.** Their Proposition 1 is a sufficient condition for
   several equilibria in a general model. The paper has no closed-form
   threshold of the kind in Paper A's M5, M9 and M10, so it does not
   anticipate them. Novelty still waits on the selection–mutation
   literature.

## Controls

- LC-2. A fabricated quotation, "every solution of (3) is locally stable
  under the obvious adjustment process", the opposite of CL-Q4, is not found.
  A true quotation is not found on a neighbouring page.
- Lean. `control_start_at_colorblind` shows the start matters, and
  `control_half_needed` shows `π_ℓ > 1/2` cannot be dropped.
