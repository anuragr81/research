# Notes — Friedman and Savage (1948), pass 1 (read in full through Drive's OCR)

Run `python3 lit/friedman_savage_1948/verify_fs.py` from the bundle root. It
checks the pinned sha256 of the scan and of the local OCR text, that there is
one block per PDF page, the first page, the running heads, each quotation
verbatim on its page, two controls, and measures
`lean/Lit/FriedmanSavage1948.lean` (build root, build, no `sorry`, every cited
name declared, every declared theorem audited, allowed axioms only, controls
present).

## What was verified

Our reading of the chord condition, the income equivalent, the conceptual
experiment, the bounds on gambles, and the single-prize argument is consistent
with the source; the class interpretation is quoted on its pages (FS-14 to
FS-24). Robson's quotation of FS (ROB-10) is confirmed against the source
(FS-16).

## What could not be verified

The risk attitudes by segment in §6 and their footnotes, the argument that few
units sit in the convex segment, and the empirical material of §2.

## Discrepancies and readings

FS-D1 (terminology), FS-D2 (p.292, from the page image) and FS-D3 in
`CLAIMS.md`. Drive's OCR orders each two-column page out of reading sequence,
which is why the page checks use a local OCR.

## Findings for the firmworkers ledger

1. **FS's classes are not the firmworkers groups.** FS read the two concave
   segments as socioeconomic classes and the convex segment as the transition
   (FS-14, FS-23). Gambling is for a jump out of one's class (FS-18), and within
   a class people are risk averse (FS-17). Their classes are income bands with
   boundaries common in a community (FS-19, FS-24): status follows income.
2. **This does not match "convex below `R_h`".** In FS the convex region lies
   between classes, above the lower class's incomes; a worker gambles when near
   the top of his class. The sub-section's assumption, convexity below the own
   group's income norm, has workers below their group's norm gambling, which is
   the prospect-theory reading (KT-2, KT-12), not FS's. The firmworkers
   manuscript's appendix calls its utility "S-shaped Friedman-Savage"; the
   ledger should say which of the two it means, since they place the convex
   region differently.
3. **Fixed class boundaries.** In FS the boundaries are incomes common within a
   community; with inherited `h` in the firmworkers model, groups are not income
   bands, so FS's mechanism (a jump in income moving a unit into another class)
   does not operate: income cannot change `h` (decision of 11 Oct 2026).
4. **Occupational choice as a gamble** (FS-3) and the premium for moderate risk
   (FS-6) are the closest FS come to the labour market; they do not model
   displacement.

## Controls

| Check | Control |
|---|---|
| Quotations | FS-C1, FS-18 with "know" replaced by "do not know", absent; FS-C2, FS-7 looked up on p.287, absent |
| Quotation suite | mutation-tested 12 Oct 2026: an altered word (FS-20), a wrong page (FS-7 on p.287), an undeclared Lean name and a wrong OCR sha256 pin each fail |
| Lean | three `control_*` theorems: linear utility gives no strict preference for insurance; a mixture of a refused and an accepted gamble can be accepted; with concave utility two prizes can beat one |

## Relevance to the firmworkers ledger

Row L22. No model row rests on Friedman and Savage yet. It is the source the
manuscript's appendix names, and it separates the class reading of the S-shape
from the reference-point reading that the sub-section uses.
