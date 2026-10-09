# Claims about Shaked (1982)

**Source.** *J. Appl. Prob.* 19 (1982), 310-320, read in full [F] from page
images (see `RECONSTRUCTION.md`). Cited in `MANUSCRIPT.tex` (row L21), in
`LITERATURE.tex`, and in `refs.bib` as `Shaked1982`. Quotations were
transcribed by me from the images; mathematical symbols are rendered in plain
text. The results are checked in `lean/mathlib/ShakedDispersive.lean` (see
`LEAN`).

## Quotations

| ID | Page | Quotation | What we rely on | Verified by |
|---|---|---|---|---|
| SH-1 | 310 | "This relation has been studied by Saunders and Moran (1978)." | The order predates Lewis and Thompson | page image |
| SH-2 | 310 | "Lewis and Thompson (1981) have introduced the concept of ordering in dispersion (o.d.)" | The name is Lewis and Thompson's | page image |
| SH-3 | 311 | "In the first pair of theorems it is shown that order in dispersion is equivalent to the condition that some functions, determined by the pair of the underlying distributions, change sign at most once." | Theorem 2.1, the characterisation behind M13 | page image; Lean `thm21_quantile`, `thm21_cdf` |
| SH-4 | 312 | "we shall assume throughout that the supports of the underlying distributions are intervals (finite or infinite) and that these distributions have no atoms" | The scope of Theorems 2.1 to 2.3 | page image |
| SH-5 | 320 | "which contain Theorem 2.3 and Remark 2.3 of the present paper and other related results" | Theorem 2.3 is also in Bickel and Lehmann (1979) and Oja (1981) | page image |

## Results in Lean (`lean/mathlib/ShakedDispersive.lean`, namespace `Shaked1982`)

| Result | Lean | What is proved |
|---|---|---|
| Encoding of "S⁻ ≤ 1, signs −, + in case of equality" | `signChangeUpOnce_iff` | the definition used equals at most one sign change, and the first change upward |
| Theorem 2.1, quantile form | `thm21_quantile` | the spacing order holds iff every shift of `z − y` changes sign at most once, from − to + |
| Theorem 2.1, distribution-function form | `thm21_cdf`, `thm21_cdf_support`, `cdf_sign_eq_quantile_sign` | both directions, for monotone `F`, `G` with `F` strictly increasing on its support and quantile right inverses |
| Theorem 2.3 | `thm23`, `thm23_deriv_iff`, `thm23_full_support` | the order holds iff `φ = G⁻¹ ∘ F` has increments at least those of the identity, on interval supports; with a derivative, iff `φ' ≥ 1`. The full-support case is Lewis and Thompson's transport result (`LewisThompson.transport_disp`) |
| Scale illustration (p.315) | `scale_disp` | X precedes aX for a ≥ 1 (Lewis and Thompson's `LewisThompson.scale_pair`) |
| Example 3.3 (p.319) | `example33`, `example33_cdf` | exponential against `x/(1+x)`: ordered, through `φ' = e^x ≥ 1`, and the crossing form holds |
| Controls | `control_not_monotone`, `control_down_crossing` | dropping "at most once" admits a −, +, − pattern; dropping "from − to +" admits a single downward crossing; neither pair is ordered |

## What is not claimed

Theorem 2.2, Lemma 2.1 and Theorems 2.5 and 2.6 are read but not used or
formalised. "Y = φ(X) in law" is formalised through the increments and the
derivative of φ only.
