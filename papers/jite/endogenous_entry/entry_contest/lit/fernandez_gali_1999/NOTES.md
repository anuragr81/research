# Notes — Fernández and Galí (NBER WP 5930, 1997), pass 1 (read in full from page images)

Run `python3 lit/fernandez_gali_1999/verify_fernandez_gali.py` from the repository root. The
OCR text it checks against is made by `ocr_fernandez_gali.py` and pinned by sha256. The PDF
stores its scans flipped and its page transformation turns them the right way, so the OCR reads
pages rendered by `pdftoppm`, not the images extracted by `pdfimages` (which read mirrored).

## Verdict for C1 to C14

No claim is overturned. The paper is the closest found on wealth in a rank-order allocation,
and it differs from ours in the channel, the choice and the population.

- **C1, C5** (the sign rule for a spread of wealth): unaffected. The wealth distribution is
  fixed and no spread is signed (FG-D3). Wealth works through a borrowing constraint with
  linear utility, not through the utility cost of a fee (FG-D1). The paper joins C5's list as a
  rank-order allocation with wealth that signs no spread.
- **C3, C4, C6** (the count and who enters): unaffected. There is no binary choice and no count;
  everyone spends a continuous amount (FG-D2).
- **C9, C12** (the sign of M28): unaffected, with a new neighbour. Lemma 4.19 is a limit in the
  same direction: as the signal weighs ability more relative to spending, aggregate spending
  goes to zero (FG-D4). It is not a sign at each weight, the expenditure is continuous, and the
  population is a continuum. C12 can cite it beside Fryer and Loury (2005) and Coate and Loury.
- **C10, C13** (the entrants' share): unaffected. Who spends nothing is placed in the lowest
  school (FG-10, FG-11), so there is no group that does not pay and still competes, and no share
  is analysed.
- **Research question.** The paper is the natural contrast for the referees' channel: here
  wealth matters only because it limits spending. In ours it matters through the utility cost of
  a fixed fee even with no constraint on spending.

## Findings from the formalisation

1. The finite counterpart of Theorem 2.2 holds by swaps, without the Levy-Paroush lemma
   (`assortative_max`); supermodularity cannot be dropped (`assortative_needs_supermod`).
2. Strict complementarity is needed for the sorting step (`sorting_needs_strict`).
3. No misprint was found in the parts formalised.
