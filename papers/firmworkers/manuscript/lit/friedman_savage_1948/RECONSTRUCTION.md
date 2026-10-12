# Reconstruction of Friedman and Savage, 1948

**Source read.**

| Short | Full | Copy | Evidence |
|---|---|---|---|
| FS | "The Utility Analysis of Choices Involving Risk", *Journal of Political Economy* 56(4), August 1948, pp.279–304 | JSTOR scan without a text layer, 27 PDF pages (a JSTOR cover sheet, then pp.279–304), Drive `friedmansavutilriskjpe1948.pdf`, id `1qihqx_Pm2fGy_6DTbqs7dbGlVULICndO`, uploaded 12 Oct 2026; cached `~/.cache/firmworkers/friedman_savage_1948.pdf`, sha256 `21fe1a556fa20286be51bf6c11992e70c0793224feee241625e423cf1571113c` | [F], published |

Two texts were made from the scan. Drive's OCR (cached
`~/.cache/firmworkers/friedman_savage_1948.drive_ocr.txt`, sha256
`f4517f6214980830e03355983c7a5f00dfc18651e2418d06bcc4897a88f437d2`) is complete
and was used for reading, but it orders each two-column page so that a page's
first paragraph precedes its running head, so it cannot anchor pages. The
quotations are checked against a local OCR with one block per PDF page: pages
rendered with `pdftoppm -r 250 -png`, then `lit/ocr_pages.py columns` with
rapidocr-onnxruntime 1.2.3 (cached `~/.cache/firmworkers/friedman_savage_1948.ocr.txt`,
sha256 `a12ccf28f8979557a3008b6b9b85aafd03ac940aae11fb67ee994fa05977c3a0`;
the tool reproduces it page for page). Block `i ≥ 2` is printed page
`277 + i`; even pages open with their number, odd pages with the running head.
The worked example on p.292 was read from the page image at 110 dpi. Read in
full on 12 Oct 2026.

**Terminology.** FS say "convex from above" for what is now called concave
and "concave from above" for convex (FS-D1). Their utility curve is, in modern
terms, concave, then convex, then concave.

**Why this paper.** The source of the S-shaped utility in the firmworkers
manuscript's appendix ("an S-shaped Friedman-Savage utility") and of the
reading of its segments as socioeconomic classes, which the reference-dependent
sub-section (decision of 12 Oct 2026) must place itself against.

## Primitives (§3, pp.287–293)

- A consumer unit behaves as if it maximised expected utility of money income,
  with a utility function unique up to origin and unit (von Neumann and
  Morgenstern).
- Riskless choice requires only that utility rise with income; choice under
  risk reveals its shape. A unit pays for certainty where utility lies above its
  chord and pays for risk where it lies below (p.291).
- A conceptual experiment fixes the function: `U(500) = 0`, `U(1000) = 1`,
  indifference at chance 2/5 gives `U(600) = 3/5`; indifference at chance 4/5
  of $500 against $10,000 versus $1,000 gives `U(10,000) = 5` (p.292).

## Derivation chain

1. **Observed behaviour (§2).** Insurance and lotteries are bought by low-income
   units, often by the same units; moderate risks command a premium; lotteries
   have several prizes.
2. **Restrictions (§4).** Insurance and lottery purchase by low-income units
   rule out a utility that is everywhere concave or everywhere convex; the
   simplest fit is concave then convex (p.294). A single prize would then be
   optimal for a lottery seller (p.297); several prizes require a terminal
   concave segment, so the curve is concave–convex–concave (Figure 3).
3. **Interpretation (§5, pp.298–299).** "As if" methodology; the concave
   segments as socioeconomic levels and the convex segment as the transition,
   with class boundaries possibly common within a community.
4. **Further implications (§6).** Premia for moderate risk depend on how units
   are distributed across segments; few units in the convex segment (class
   border, and instability of status there); investment patterns by income;
   stability of relative income status.
5. **Conclusion (§7).** The hypothesis restated, with its refutable
   implications.

## What was reconstructed

The chord condition for insurance and gambling under strict concavity and
convexity, the certainty-equivalent ordering, the conceptual experiment, the
bound on what anyone pays for a gamble, refusal of a compound of refused
gambles, and the single-prize argument from convexity over the prize range
(with a concave control where two prizes win). See `CLAIMS.md`.

## What was not reconstructed

The segment-by-segment risk attitudes of §6 and their footnotes, the
population-distribution argument, and the empirical sections.
