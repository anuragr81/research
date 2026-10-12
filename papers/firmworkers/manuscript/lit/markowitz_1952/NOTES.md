# Notes — Markowitz (1952), pass 1 (read in full through local and Drive OCR)

Run `python3 lit/markowitz_1952/verify_mk.py` from the bundle root. It checks
the pinned sha256 of the scan and of the local OCR text, one block per page,
the first page, the even-page running heads, each quotation verbatim on its
page, two controls, and measures `lean/Lit/Markowitz1952.lean` (build root,
build, no `sorry`, every cited name declared, every declared theorem audited,
allowed axioms only, controls present).

## What was verified

Our reading of the customary-wealth hypothesis, of symmetric-bet aversion, and
of the fun-of-participation argument is consistent with the source. One
inequality on p.154 is printed inverted (MK-D2); p.155 has it right.

## What could not be verified

The point-by-point critique of Friedman and Savage, the skewness argument and
the closing remark on multiple prizes. The OCR's `c`/`e` confusion means some
passages were read from Drive's OCR and page images rather than checked.

## Discrepancies and readings

MK-D1 (terminology) and MK-D2 (p.154) in `CLAIMS.md`.

## Findings for the firmworkers ledger

1. **The reference is own customary wealth.** Markowitz puts the middle
   inflection at each person's customary wealth, equal to present wealth except
   after windfalls (MK-13, MK-14), and lets the inflection distances scale with
   wealth (MK-10, MK-16). This is the own-position reference that Kőszegi and
   Rabin later tie to expectations. The sub-section's `R_h`, the income norm of
   the worker's status group, differs from it in being shared within a group.
2. **The shape around the reference is the reverse of prospect theory's.** In
   modern terms Markowitz is risk averse for small losses and risk seeking for
   small gains, convex again only for large losses (MK-7 to MK-9, MK-15); KT
   are concave for gains and convex for losses and note the contrast (KT-15).
   For a displaced worker's large loss both put convexity far below the
   reference, but they disagree near it, and KT add that large losses may
   instead be concave (KT-13). The sub-section's "convex below `R_h`" is the
   KT shape near the reference, not Markowitz's.
3. **Windfalls and adaptation.** A windfall moves present wealth off customary
   wealth, and how customary wealth catches up is left open (MK-20). Displacement
   is a negative windfall in this sense; whether the reference stays at the
   former position is exactly the adaptation question Markowitz leaves open and
   Kahneman and Tversky take up (KT-19).

## Controls

| Check | Control |
|---|---|
| Quotations | MK-C1, MK-11 with "avoid" replaced by "prefer", absent; MK-C2, MK-10 looked up on p.155, absent |
| Quotation suite | mutation-tested 12 Oct 2026: an altered word (MK-21), a wrong page (MK-10 on p.155), an undeclared Lean name and a wrong OCR sha256 pin each fail |
| Lean | three `control_*` theorems: the inequality printed on p.154 would make symmetric bets attractive; with convex utility a larger stake is better; without adaptation a windfall does not register as a gain |

## Relevance to the firmworkers ledger

Row L23. No model row rests on Markowitz yet. It is the own-wealth reference
against which the group reference `R_h` is placed.
