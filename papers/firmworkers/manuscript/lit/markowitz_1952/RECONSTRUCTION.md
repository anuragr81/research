# Reconstruction of Markowitz, 1952

**Source read.**

| Short | Full | Copy | Evidence |
|---|---|---|---|
| MK | "The Utility of Wealth", *Journal of Political Economy* 60(2), April 1952, pp.151–158, in the Cowles Foundation Paper 57 reprint, which keeps the JPE pagination | scan without a usable text layer, 8 PDF pages (pp.151–158), Drive `Markowitz- The Utility of Wealth.pdf`, id `1asg1X32udA65WOuxGDz29cif1N39v0mB`, uploaded 12 Oct 2026; cached `~/.cache/firmworkers/markowitz_1952.pdf`, sha256 `ec2f16ff3965b5b99cdafddaeee3761eb0145a59574d341652e6cdf962eaa3f1` | [F], published (reprint) |

Drive's OCR skips p.151, so the text is a local OCR with one block per page:
pages rendered with `pdftoppm -r 250 -png`, then `lit/ocr_pages.py columns`
with rapidocr-onnxruntime 1.2.3 (cached
`~/.cache/firmworkers/markowitz_1952.ocr.txt`, sha256
`a78ce6666ff253e68ca8500f05b88080317496dc8e07b22e77a166ee598bdfd9`; the tool
reproduces it page for page). Block `i` is printed page `150 + i`. The scan
prints `e` close to `c`, and the OCR confuses them; quotations were chosen
from passages the OCR reads cleanly, and pp.154–155 were read from page images
at 100 dpi. Drive's OCR of pp.152–158 was also read. Read in full on
12 Oct 2026.

**Terminology.** Like Friedman and Savage, Markowitz says "convex" for what is
now concave and "concave" for convex (MK-D1). His curve, from far below to far
above customary wealth, is in modern terms convex, concave, convex, concave:
risk seeking for large losses, risk averse for small losses, risk seeking for
small gains, risk averse for large gains.

**Why this paper.** The first reference-point utility of wealth: the middle
inflection sits at each person's "customary wealth". Kahneman and Tversky
credit it (KT-10) and contrast its shape with theirs (KT-15). It is needed to
place the sub-section's group reference `R_h` (decision of 12 Oct 2026)
against an own-wealth reference.

## Derivation chain

1. **Friedman–Savage restated (§1.1).** Their curve explains insurance and
   lotteries; units between the tangency points C and D take fair bets (p.152).
2. **Critique (§1.2).** Middle-income units would prefer large symmetric bets;
   the nearly rich would underwrite insurance at a loss; units below C or above
   D would never gamble; all contradict observation (pp.152–153).
3. **Hypothesis (§2.1, pp.153–155).** From hypothetical choices: above present
   wealth, risk seeking for small gains then risk averse; below, risk averse for
   small losses then risk seeking for large ones; three inflection points, the
   middle one at customary wealth, which equals present wealth except after
   windfalls; inflection distances nondecreasing in wealth; bounded utility;
   symmetric bets avoided, so the curve falls faster to the left.
4. **Consistency (§2.2).** Insurance and lotteries together, the same pattern at
   all wealth levels with "large" and "small" rescaled.
5. **Applications (§2.3).** Playing conservatively when losing and liberally
   when winning yields right skew; after windfalls the unit leaves the middle
   inflection; asymmetric bets avoided.
6. **Fun of participation (§3.1).** A concave utility plus a fixed utility of
   playing implies smaller stakes are always better, which contradicts
   observation.

## What was reconstructed

Symmetric-bet aversion as the curve falling faster to the left, the inverted
inequality printed on p.154 as a control, the fun-of-participation argument
(the value of a fair symmetric bet falls with the stake under concavity), and
the rescaling and windfall statements of the customary-wealth hypothesis. See
`CLAIMS.md`.

## What was not reconstructed

The critique of the Friedman–Savage curve point by point, the skewness and
gaming-strategy arguments, and the closing remark on multiple-prize lotteries.
