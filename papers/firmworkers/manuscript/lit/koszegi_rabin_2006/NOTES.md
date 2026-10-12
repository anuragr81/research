# Notes — Kőszegi and Rabin (2006), pass 1 (read in full)

Run `python3 lit/koszegi_rabin_2006/verify_kr.py` from the bundle root. It
checks the pinned sha256 of the PDF, the first page, that every block carries
its page number, each quotation verbatim on its page, two controls, and
measures `lean/Lit/KoszegiRabin2006.lean` (build root, build, no `sorry`, every
cited name declared, every declared theorem audited, allowed axioms only,
controls present).

## What was verified

Our reading of the gain–loss assumptions, of Propositions 1.2, 2 and 3 in one
dimension, and of the shopping thresholds and inequalities (6) and (7) is
consistent with the source. One step the paper takes in a phrase (KR-D1) is
proved: A0's continuity plus A2 give loss aversion at every stake.

## What could not be verified

Proposition 1.3, multi-dimensional versions, the existence of PE and PPE (via
Kőszegi 2005, not read), the driving model and Proposition 4.

## Discrepancies and readings

KR-D1 and KR-D2 in `CLAIMS.md`; neither affects a result.

## Findings for the firmworkers ledger

1. **Endogenous reference, but the wrong anchor for `R_h`.** KR's reference is
   the person's own recent expectations (KR-2, KR-10), and they claim priority
   for that rule (KR-5). The sub-section's `R_h` is the income norm of the
   worker's status group, which is neither own expectations nor the status quo
   (KR-11). That is a clean point of difference, but the novelty check must say
   it: like KR, `R_h` leaves no free reference parameter (KR-6).
2. **Displacement is exactly where KR's caution bites.** "Convex below the
   reference" is prospect theory's diminishing sensitivity in losses (A3, KR-7).
   KR note that for large losses, concave consumption utility counteracts it
   (KR-9). A displaced worker's fall to the bottom rung `x_0` (M32) is a large
   loss for anyone above it. So convexity of the gain–loss part below `R_h` does
   not by itself make overall utility convex there; the sub-section must state
   its assumption on total utility, not on the gain–loss part alone.
3. **Reference dependence acts only under uncertainty.** With a deterministic
   choice set, PPE maximises consumption utility (KR-3, KR-13, and the sign
   result `expected_gain_loss_nonpos`). The displaced worker's choice is between
   a sure downgrade and a lottery, which is the stochastic case where KR's
   framework says reference dependence matters (KR-14).
4. **Labour-market losses are relative to expected pay** (KR-4, KR-12), and a
   worker's target follows own typical income (KR-17). That is Markowitz's
   customary-wealth idea in expectations form; `R_h` replaces own typical income
   by the group's.

## Controls

| Check | Control |
|---|---|
| Quotations | KR-C1, KR-2 with "rational expectations" replaced by "status quo", absent; KR-C2, KR-9 looked up on p.1140, absent |
| Quotation suite | mutation-tested 12 Oct 2026: an altered word (KR-16), a wrong page (KR-9 on p.1140), an undeclared Lean name and a wrong sha256 pin each fail |
| Lean | two `control_*` theorems: with `λ = 1` the symmetric sum is zero, so A2 fails; with `λ = 1` both effects vanish |

## Relevance to the firmworkers ledger

Row L19. No model row rests on Kőszegi and Rabin yet. It is the contrast case
for `R_h` and the source of the large-loss caveat for the sub-section.
