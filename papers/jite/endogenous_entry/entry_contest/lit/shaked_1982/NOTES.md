# Notes — Shaked (1982), pass 1 (read in full from page images)

Run `python3 lit/shaked_1982/verify_shaked.py` from the repository root. No
OCR is installed and the PDF has no text layer, so the suite pins the cached
PDF by sha256 and checks the quotation rows; the transcription was made by
me from the page images on 9 Oct 2026. The Lean file is
`lean/mathlib/ShakedDispersive.lean` (see `LEAN`), which the suite measures
as the Lewis-Thompson suite measures its file.

## What the manuscript takes from it

**The characterisation behind M13.** A pivot-spread is a change whose new
quantiles cross the old ones once. Shaked's Theorem 2.1 says the dispersive
order is exactly single crossing, from below, of every shift of one
distribution function against the other: "order in dispersion is equivalent
to the condition that some functions, determined by the pair of the
underlying distributions, change sign at most once" (p.311). In quantile
form this is the statement that `G⁻¹ − F⁻¹ − c` changes sign at most once,
from − to +, for every `c`, which is the monotonicity of `G⁻¹ − F⁻¹`
(Lewis and Thompson's spacing form, `lit/lewis_thompson_1981/`).

## Provenance corrections for our documents

1. **The order is older than Lewis and Thompson.** Shaked credits the
   relation to Saunders and Moran (1978) and the name "ordering in
   dispersion" to Lewis and Thompson (1981). Our documents said the order
   "is older than that citation" with Lewis and Thompson as the earlier
   source; the earliest is Saunders and Moran (1978), unread.
2. **Theorem 2.3 is not Shaked's alone.** His note added in proof says
   Bickel and Lehmann (1979) and Oja (1981) contain it. Lewis and Thompson's
   transport result (`LewisThompson.transport_disp`) is the same statement.
3. Hopkins and Kornienko (2009) cite Shaked (1982) for the order, which is
   accurate as a citation of the characterisation; the definition itself is
   Saunders and Moran's.

## Under the one-citation rule

For the manuscript's claim that a pivot-spread is a dispersive shift with a
crossing point, Shaked's Theorem 2.1 is the citation (row L21). Lewis and
Thompson (L11) stay for the definition by spacing, Hopkins and Kornienko
(L10) for its use in inequality comparisons.
