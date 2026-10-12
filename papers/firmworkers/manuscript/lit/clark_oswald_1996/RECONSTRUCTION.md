# Reconstruction of Clark and Oswald, 1996

**Source read.**

| Short | Full | Copy | Evidence |
|---|---|---|---|
| CO | "Satisfaction and Comparison Income", authors' manuscript "Revised: August 1995", an unnumbered title page and pp.1–27 (pp.24–27 are tables, p.27 unnumbered); published as *Journal of Public Economics* 61(3), 1996, pp.359–381 | PDF with text layer (WordPerfect via Acrobat PDFWriter), 28 PDF pages, Drive `jpub.pdf`, id `1XPscmDLuUz7j14i8g_Qm8s12TVgDZB4k`, uploaded 12 Oct 2026; cached `~/.cache/firmworkers/clark_oswald_1996.pdf`, sha256 `ff666a87c0b6a799a42af02fcd628259713d81b55443ed3343ab81582dec3814` | [F], authors' revision; the JPubE pages are not read |

Text extracted with `pdftotext -layout`; form-feed block `p` is page `p` for
`p = 1, ..., 27`, block 0 being the title page with the abstract; pages 1–26
end with their number. Read in full on 12 Oct 2026, with footnotes, appendix
and tables. Cited as the JPubE article with `version_read`.

**Why this paper.** The empirical counterpart for the reference income `R_h`
of the reference-dependent sub-section (decision of 12 Oct 2026, and the
author's choice of the income norm of the own group): CO's comparison income is
the predicted pay of a typical worker with the same characteristics, the
nearest measured object to a group income norm. Needed for the measurement
map.

## Primitives (§2)

- Utility from work `u(y, h, i, j)` (standard) against `u(y, y*, h, i, j)` with
  `y*` a comparison income and `u` declining in `y*` (relative deprivation).
- `y*` measured two ways: predicted from a Mincer earnings equation estimated on
  the whole sample ("the income of 'typical' employees of given
  characteristics"), and cell means by gender and weekly hours from the 1991 New
  Earnings Survey.
- Reported overall job satisfaction (1–7) from wave 1 of the British Household
  Panel Survey, 1991, treated as proxy utility data; ordered probit.

## Derivation chain

1. **Raw data (§3, Table 1).** Satisfaction is not rising in absolute pay, but
   rises across quintiles of `y/y*` for men.
2. **Table 2.** Income alone enters negatively; with `y*`, income is positive
   (0.11) and `y*` negative (−0.20); equal-and-opposite coefficients not
   rejected (χ² 2.8); with the NES `y**`, −0.26.
3. **Table 3.** The `y*` effect survives a broad specification, for overall
   satisfaction and for satisfaction with pay.
4. **Magnitude (p.13).** ±1 s.d. of `y*` (5.7 to 7.3) times −0.2 is about −0.3,
   larger than the gender gap.
5. **Education (§4, Table 4).** Holding income fixed, more educated workers
   report less satisfaction, perhaps through higher aspirations.
6. **Conclusion (§5).** Comparisons matter; implications for growth, taxation
   and wage setting.

## What was reconstructed

The relative-income form when the coefficients are equal and opposite, the
hourly-income reading, the effect of a common proportional rise in `y` and `y*`
(footnote 16) with the Table 2 coefficients, the magnitude on p.13, and the
satisfaction distribution on p.8. See `CLAIMS.md`.

## What was not reconstructed

The ordered-probit estimates themselves (data not available here), the
earnings equation of the Appendix, and the education results beyond their
reading.
