# Notes — Genicot and Ray (2017), pass 1 (read in full)

Run `python3 lit/genicot_ray_2017/verify_gr.py` from the bundle root. It checks
the pinned sha256 of the PDF, the first page, that every block carries its page
number, each quotation verbatim on its page, two controls, and measures
`lean/Lit/GenicotRay2017.lean` (build root, build, no `sorry`, every cited name
declared, every declared theorem audited, allowed axioms only, controls
present).

## What was verified

Our reading of the aspirations formation function and its properties, of
Observation 1 and Lemma 1, of the two steps of Proposition 5, of the scale
reduction of the constant elasticity model, and of the comparison behind
`g(r) > g` is consistent with the source. Observation 1's printed proof inverts
its own ratio (GR-D1); the statement holds and is proved directly.

## What could not be verified

Propositions 6–8 in full, Lemmas 2–6, Observations 2 and 3, the numerical
examples, and the Supplemental Material.

## Discrepancies and readings

GR-D1 and GR-D2 in `CLAIMS.md`; neither affects a result.

## Findings for the firmworkers ledger

1. **A reference built from the distribution is not new, and neither is a
   group-based one.** GR's aspirations depend on own income and the
   distribution (GR-4), including the mean (GR-5; Bogliacino and Ortoleva use a
   common mean, GR-6). Windows may be drawn by caste, religion or other inherited
   identity (GR-13), and group-based aspirations are named as an extension
   (GR-14). With the author's pointer to consumption economics (Kaus on South
   Africa, not yet read), the reference `R_h` set by the income norm of the own
   status group cannot be claimed as new on its own.
2. **What GR leave open is cross-group interaction.** Without interactions,
   group aspirations are "several co-existing copies" of their model; with
   them, "fresh and interesting considerations would arise" (GR-15), and they
   name group aspirations and conflict as a further extension (GR-23). In the
   firmworkers model the groups interact through the swap pool, weighted by the
   closure `σ` (M25–M29). The candidate point of novelty for the sub-section is
   therefore not `R_h` itself but the interaction: a group-specific reference
   (the preference channel) together with closure in a cross-group lottery for
   positions (the feasibility channel), with group membership inherited
   (decision of 11 Oct 2026). That is a narrower claim, and it still needs
   checking against Kaus and against the consumption literature.
3. **Shape of the reference effect.** GR's milestone utility adds a concave
   payoff above the threshold, so the reference creates a convex kink (GR-10,
   GR-18), not a convex region below it. The sub-section's assumption "convex
   below `R_h`" is a different shape; GR show that local nonconcavity at the
   reference is what matters for their results, which supports stating the
   sub-section's assumption as nonconcavity of total utility around `R_h`
   (compare Kőszegi and Rabin, KR-9).
4. **Equality is unstable here too** (Proposition 5), as in Robson (Lemma 2)
   and Ray and Robson; the mechanism is again local convexification at a point
   where many incomes sit.

## Controls

| Check | Control |
|---|---|
| Quotations | GR-C1, GR-15 with "trivial" replaced by "difficult", absent; GR-C2, GR-14 looked up on p.493, absent |
| Quotation suite | mutation-tested 12 Oct 2026: an altered word (GR-13), a wrong page (GR-14 on p.493), an undeclared Lean name and a wrong sha256 pin each fail |
| Lean | three `control_*` theorems: purely personal aspirations `Ψ = y` give a constant ratio; with no milestone jump `z = a` can satisfy both conditions; with no aspirational term the two first-order conditions coincide |

## Relevance to the firmworkers ledger

Row L20. No model row rests on Genicot and Ray yet. It moves the novelty of the
reference-dependent sub-section from the reference itself to the cross-group
interaction.
