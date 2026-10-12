# Notes — Kahneman and Tversky (1979), pass 1 (read in full through a local OCR)

Run `python3 lit/kahneman_tversky_1979/verify_kt.py` from the bundle root. It
checks the pinned sha256 of the scan and of the local OCR text, the first page,
that there is one block per printed page with its number, each quotation
verbatim on its page, two controls, and measures
`lean/Lit/KahnemanTversky1979.lean` (build root, build, no `sorry`, every cited
name declared, every declared theorem audited, allowed axioms only, controls
present).

## What was verified

Our reading of the value function's shape and loss aversion, of subcertainty
and overweighting as derived from the choice problems, of equation (2), of the
expected-utility argument on probabilistic insurance, and of the proposition
on unadapted losses (p.287) is consistent with the source. The proposition's
premises are derived in Lean from concavity for gains and convexity for losses
(KT-D2).

## What could not be verified

The editing phase, subproportionality, the dominance argument, the
preference-reversal remarks and the Appendix axiomatisation. The OCR drops a few
lines elsewhere in the paper (KT-D1); the reading of those lines rests on the
surrounding text, not on a check.

## Findings for the firmworkers ledger

1. **The sub-section's shape is prospect theory's.** Utility convex below the
   reference and concave above it (KT-2, KT-12) is the value function, with the
   reference able to sit at an aspiration level rather than the status quo
   (KT-9, KT-18). Assuming "convex below `R_h`" is therefore not new; it must be
   cited to KT, and to Markowitz, whom KT credit (KT-10).
2. **The shape inhibits gambling; small-probability weights drive it.** KT
   attribute both gambling and insurance to overweighting of small probabilities
   and say the S-shaped value function "tends to inhibit both" (KT-17). In the
   firmworkers lottery the probabilities are not small (a uniform draw among the
   other entrants), so prospect theory's route to gambling is the convexity for
   losses (reflection), not overweighting.
3. **Large losses may make losses concave** (KT-13), as Kőszegi and Rabin also
   note (KR-9). A displaced worker's fall to `x_0` is such a loss. The
   sub-section must say which regime it assumes.
4. **Unadapted losses and gambling.** A person who has not adapted to a loss
   accepts gambles otherwise unacceptable (KT-19; `unadapted_loss_accepts_gamble`).
   That is the most direct link from prospect theory to the displaced worker of
   M32: if the reference stays at the former income (or at the group norm `R_h`)
   after displacement, the downgrade is coded as a loss and the lottery becomes
   more attractive. Whether the reference is the former income (own status quo
   ante) or the group norm is a modelling choice the author has made for `R_h`;
   KT support the mechanism, not the choice of reference.

## Controls

| Check | Control |
|---|---|
| Quotations | KT-C1, KT-14 with gains and losses swapped, absent; KT-C2, KT-19 looked up on p.286, absent |
| Quotation suite | mutation-tested 12 Oct 2026: an altered word (KT-13), a wrong page (KT-19 on p.286), an undeclared Lean name and a wrong OCR sha256 pin each fail |
| Lean | four `control_*` theorems: a linear value function gives `v(x) = −v(−x)`; equations (1) and (2) differ under subcertainty; a linear `u` gives no strict preference for probabilistic insurance; without subcertainty the unadapted-loss conclusion fails |

## Relevance to the firmworkers ledger

Row L21. No model row rests on Kahneman and Tversky yet. It is the source of
the sub-section's shape assumption and of the unadapted-loss mechanism.
