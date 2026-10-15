# Arrow (1973), reconstruction

Cited as Kenneth J. Arrow, "The Theory of Discrimination", in O. Ashenfelter
and A. Rees (eds.), *Discrimination in Labor Markets*, Princeton University
Press, 1973, pp. 3–33.

## Source read

- **Not the cited version.** The Drive file `Arrow_1973_Discrimination and
  Prejudice.pdf` (Drive id `16A4PTSWmYyzZ3ncLofKSr52_SeUi2Q8-`, 2,473,233
  bytes, folder `temp`) is Princeton Industrial Relations Section Working
  Paper No. 30A, "The Theory of Discrimination", presented at the conference
  "Discrimination in Labor Markets", 7–8 October 1971. The 1973 chapter is
  the published version of that conference paper. It has not been read, so
  every claim here is about the working paper (AR-D1).
- 37 PDF pages, a scan without a text layer. Cached at
  `~/.cache/paper_a/arrow_1973.pdf`, sha256 `5fb8d80d…0473`.
- Read in full from page images on 15 October 2026 [P], all 37 pages at
  100 dpi and Section 4 (PDF pages 27 to 33) again at 130 dpi. The images
  are pinned in `NOTES.md`.
- OCR by `lit/ocr_pages.py` in single-column mode, cached at
  `~/.cache/paper_a/arrow_1973.txt`, sha256 `83c6fc44…889f`. The OCR drops
  some lines (AR-D2).
- Pages are cited as PDF pages, since the working paper numbers its pages
  `-2-` to `-31-` with an unnumbered figure page and restarts at the
  references and the appendix. PDF page `n` is the paper's page `n − 1` for
  `n` from 3 to 16.

## The paper, from its own primitives

1. **Scope** (PDF pp. 2–5). Discrimination as it appears on the market. Two
   groups, B and W, perfect substitutes in production. Other markets, such
   as housing, show "a greater degree of simple exclusion and less of price
   differentials".
2. **Taste discrimination by employers** (pp. 6–11). Employers maximise
   `U(π, B, W)`, with discrimination coefficients `d_B > 0`, `d_W ≤ 0`, so
   `w_W − w_B = d_B − d_W > 0` (eq. 4). With utility depending on the ratio
   only, employers neither gain nor lose, and the effect is a transfer from
   B to W workers. Differing tastes give partial segregation, and in the long
   run only the least discriminatory firms survive.
3. **Discrimination by complementary workers** (pp. 11–13). Foremen with
   tastes over the W/B ratio of floor workers, eq. (10).
4. **Nonconvexity** (pp. 14–22). Indifference maps that depend only on
   ratios cannot be convex (Appendix). Discrimination by perfect-substitute
   W workers gives segregation and no wage differential (eqs. 11, 12). In the
   long run, employer discrimination gives perfect segregation and equal
   wages.
5. **Costs of adjustment** (pp. 23–26). A personnel investment at each hire
   makes firms slow to switch, so "history matters a good deal" and wage
   differences coexist with segregation.
6. **Imperfect information** (pp. 27–33). Employers believe a W worker is
   qualified with probability `p_W` and a B worker with `p_B`. With a required
   return `r` on the personnel investment, `r = (MP_S − w_W) p_W` and
   `r = (MP_S − w_B) p_B` (eq. 13), so `w_W = q w_B + (1 − q) MP_S` with
   `q = p_B/p_W` (eq. 14), and `p_B < p_W` gives `w_W > w_B`. Beliefs may be
   explained by cognitive dissonance, or by qualification being an
   unobservable investment whose supply `p = S(v)` rises with the gain
   `v = w − w_U` (eq. 15). A symmetric equilibrium exists. Under Marshallian
   dynamics `dp_W/dt = k[S(v_W) − p_W]` it is stable iff
   `E (MP_S − w_S)/(w_S − w_U) < 1`, and its possible instability "strongly
   suggests" asymmetric, discriminatory equilibria. The paper closes by
   calling for a study of how employers' knowledge modifies their initial
   estimates.

## Derivation chain of what Paper A uses

- Eq. (14) from the two equations (13) (`wage_eq14`), and the wage gap from
  the belief gap (`wage_differential`).
- The stability condition is stated, with its proof referred to Arrow
  (1971), Technical Note F. Our derivation of the difference mode
  `p_W − p_B` reproduces its left side: with `w_i = MP_S − r/p_i` the wage gap
  is `r (p_W − p_B)/(p_W p_B)` (`wage_gap_of_belief_gap`), and the gap's
  growth coefficient `S′ r/p²` equals `E (MP_S − w_S)/(w_S − w_U)`
  (`diff_mode_coeff`). The sum mode, which moves `MP_S`, is not derived.

## What could not be reconstructed

- Technical Notes B, E and F of Arrow (1971) are not in the paper, so
  eq. (10), the adjustment-cost result and the full stability condition are
  taken as stated.
- Lines the OCR dropped were read from the images. No quotation is taken
  from them.
