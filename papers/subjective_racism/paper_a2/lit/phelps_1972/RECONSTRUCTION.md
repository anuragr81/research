# Phelps (1972), reconstruction

Edmund S. Phelps, "The Statistical Theory of Racism and Sexism", *American
Economic Review* 62(4), September 1972, pp. 659–661.

## Source read

- A scan of the published communication, 7 PDF pages, of which pages 1 to 3
  are pp. 659 to 661 and pages 4 to 7 are blank. Downloaded on 15 October
  2026 from the author's Drive folder `temp`, file `phelps1972.pdf` (Drive id
  `1PUjuPqJJfUXk88SOlc5lvdG2FF_9SwEY`, 496,182 bytes).
- Cached at `~/.cache/paper_a/phelps_1972.pdf`, sha256 `ab6f4870…77fd`. The
  PDF has no text layer. Page images were rendered at 150 dpi and read in
  full on 15 October 2026, so the evidence tag is [P]. The images are pinned
  in `NOTES.md`.
- For the quotation check, the pages were also put through OCR with
  `lit/ocr_pages.py` (RapidOCR, 200 dpi) into
  `~/.cache/paper_a/phelps_1972.txt`, sha256 `fd143d1e…10a6`.

## The model, from its own primitives

1. **Setting** (p. 659). An employer who cannot cheaply observe an applicant's
   qualification uses the applicant's group as "a proxy for relevant data not
   sampled". The prior belief about the group may come from experience or
   from "prevailing sociological beliefs", and in the latter case the
   discrimination is "self-perpetuating".
2. **Test score** (p. 659, eq. 1). `y_i = q_i + μ_i`, with `μ` normal with
   mean zero.
3. **No group information** (p. 660, eq. 2). The least-squares predictor is
   `q′ = a_1 y′ + u′` in deviations from means, with
   `0 < a_1 = var q′/(var q′ + var μ) < 1`.
4. **Group information** (p. 660, eqs. 3, 3a, 4). `q_i = α + x_i + η_i`,
   `x_i = (−β + ε_i) c_i`, `β > 0`, `c_i = 1` for a black applicant. With
   `λ_i = η_i + c_i ε_i` and `z_i = −β c_i`, `q_i = α + z_i + λ_i` and
   `y_i = α + z_i + λ_i + μ_i`.
5. **Prediction** (p. 660, eqs. 5, 5′). `q′ = [var λ/(var λ + var μ)] y′ +
   [var μ/(var λ + var μ)] z′ + u`. The weights on the test and on the group
   are inversely related to the variances of their disturbances.
6. **Case 1** (p. 660). If `var ε = 0`, the coefficients do not depend on
   `c`, and the prediction line for blacks is parallel to and below that for
   whites (Figure 1).
7. **Case 2** (pp. 660–661, eq. 6). `var λ = var η + c² var ε`, so the test
   coefficient is larger for blacks, and above some score a black applicant is
   predicted to exceed a white applicant with the same or a lower score.
8. **A further case** (p. 661, eq. 7). With `μ_i = ξ_i + c_i ρ_i`, white
   scores are more reliable, and the white prediction line can be the steeper.
9. **A final word** (p. 661). Phelps does not claim that most discrimination
   is statistical.

## Derivation chain of what Paper A uses

- Eq. (5′) in levels is `pred = α + z + a (y − α − z)` with
  `a = var λ/(var λ + var μ)`, so `0 < a < 1` (`a_mem`) and the two weights
  sum to 1 (`weights_sum`).
- Case 1 is `pred(black) − pred(white) = −β (1 − a)` at every score
  (`case1_parallel`).
- Case 2 follows from `a` rising in `var λ` (`a_strictMono`), which makes
  the difference of the predictions rise in `y` and cross zero
  (`case2_crossing`).

## What could not be reconstructed

- The further case of eq. (7) is described in words only, and Paper A does
  not rely on it.
- The model is static. It has no updating of the group's parameters across
  applicants or periods, so nothing in it can be checked against a claim
  that counter-examples erode the stereotype.
