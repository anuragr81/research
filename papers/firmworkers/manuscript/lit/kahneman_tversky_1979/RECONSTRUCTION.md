# Reconstruction of Kahneman and Tversky, 1979

**Source read.**

| Short | Full | Copy | Evidence |
|---|---|---|---|
| KT | "Prospect Theory: An Analysis of Decision under Risk", *Econometrica* 47(2), March 1979, pp.263–291 | ProQuest scan, 29 PDF pages (one per printed page), text layer holding only the ProQuest notice, Drive `ProspectTheory.pdf`, id `1v3lD-pWNpNOnoR0yTwI6KghCOa1ZOD8P`, uploaded 12 Oct 2026; cached `~/.cache/firmworkers/kahneman_tversky_1979.pdf`, sha256 `ac09b4c02f081123fbe4bb0d14cbebfe065cbf57e787b773543fd5c9a72eb4e1` | [F], published |

The scan has no usable text layer and Drive does not OCR it (it returns the
ProQuest notice only), so the text is a local OCR: pages rendered with
`pdftoppm -r 200 -png`, then `lit/ocr_pages.py single` with
rapidocr-onnxruntime 1.2.3 (onnxruntime 1.31.0) in a separate virtual
environment. Output cached `~/.cache/firmworkers/kahneman_tversky_1979.ocr.txt`,
sha256 `9ddeaa09d54d310bb9fed0ebf576259509ebafa3230d3d5f520c6c50c478e971`; the
tool reproduces it page for page. One form feed per PDF page, so block `i` is
printed page `262 + i`; every running head carries its page number. The OCR
drops an occasional line and garbles displays; the quotations avoid both.
Read in full on 12 Oct 2026.

**Why this paper.** The reference-dependent sub-section (decision of
12 Oct 2026) assumes utility convex below a reference income. That is prospect
theory's value function in the loss domain, so this paper is its source, and
it also shows that a person who has not adapted to a loss accepts gambles,
which bears on displaced workers (M32).

## Primitives

- Prospects `(x₁, p₁; ...; xₙ, pₙ)`; expected utility rests on expectation,
  asset integration and risk aversion (§2, pp.263–264).
- Prospect theory (§3): an editing phase (coding against a reference point,
  combination, segregation, cancellation, simplification, dominance) and an
  evaluation phase with value `v` on gains and losses and decision weights `π`.
- Equation (1) for regular prospects, `V = π(p)v(x) + π(q)v(y)`; equation (2)
  for strictly positive or negative ones, `V = v(y) + π(p)[v(x) − v(y)]`.

## Derivation chain

1. **Critique (§2).** Certainty effect (Problems 1–8), reflection effect
   (Table I: risk aversion for gains, risk seeking for losses), probabilistic
   insurance rejected although concave expected utility prefers it (Problem 9),
   isolation effect (Problems 10–12).
2. **Value function (pp.277–279).** Defined on deviations from the reference
   point; concave above it, often convex below; losses loom larger, derived from
   aversion to symmetric bets; large losses that force a change in life style can
   make it concave for losses.
3. **Weighting function (pp.280–283).** Overweighting of small probabilities,
   subadditivity, subcertainty (from Allais), subproportionality.
4. **Risk attitudes (pp.284–286).** Allais and substitution violations from
   subcertainty and subproportionality; regular over probabilistic insurance
   from subadditivity; risk seeking for gains and risk aversion for losses only
   at small probabilities.
5. **Shifts of reference (pp.286–288).** Reference points set by expectation or
   aspiration; a negative translation (unadapted losses) increases risk
   seeking, proved from the value function's shape, loss aversion and
   subcertainty; final-asset framing makes the value function concave
   everywhere.

## What was reconstructed

Subadditivity of a concave value function for gains and superadditivity of a
convex one for losses (from `v(0) = 0`), loss aversion from aversion to
symmetric bets, subcertainty from the Allais preferences, overweighting from
Problem 14, the rewriting of equation (2), the expected-utility case for
probabilistic insurance, and the proposition on unadapted losses on p.287 with
its premises derived from the value function's shape. See `CLAIMS.md`.

## What was not reconstructed

The editing phase, subproportionality and its log-convexity characterisation,
the dominance argument on p.283, the preference-reversal remarks, and the
axiomatisation in the Appendix.
