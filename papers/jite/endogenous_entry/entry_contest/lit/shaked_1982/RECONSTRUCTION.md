# Reconstruction of Shaked (1982)

**Source read.** Moshe Shaked, "Dispersive ordering of distributions",
*Journal of Applied Probability* 19 (1982), 310-320. Read in full [F] from
`dispersive_ordering_shaked.pdf`, uploaded by the author to Drive on 9 Oct
2026: 11 page images with no text layer, read page by page as images. The
PDF is cached at `~/.cache/entry_contest/shaked_1982.pdf` (sha256
`07d1c43f7b4e7ac857c40d801c273e7cae560a99a57f2ba5841537cccf518fd6`), which
`verify_shaked.py` pins. Quotations were transcribed from the images.

**Why this paper.** Our documents cite it, with Hopkins and Kornienko (2009),
for the dispersive order, and the manuscript listed it as unread.

## Provenance the paper itself gives

- The relation was "studied by Saunders and Moran (1978)" (p.310), for the
  gamma family and in the context of matching optimal receptors.
- "Lewis and Thompson (1981) have introduced the concept of ordering in
  dispersion (o.d.)" (p.310).
- A note added in proof (p.320) says Bickel and Lehmann (1979) and Oja (1981)
  "contain Theorem 2.3 and Remark 2.3 of the present paper".

So the order is Saunders and Moran's relation, named by Lewis and Thompson,
and characterised here.

## Objects

`F`, `G` distribution functions, strictly increasing and continuous on
interval supports, without atoms (p.312). `F ≤disp G` iff
`F⁻¹(β) − F⁻¹(α) ≤ G⁻¹(β) − G⁻¹(α)` for `0 < α < β < 1` (1.1).
`F_c(x) = F(x − c)`. `S⁻(a)` is the number of sign changes of `a` (1.4).

## Results

1. **Theorem 2.1 (p.312).** `F ≤disp G` iff `S⁻(F_c − G) ≤ 1` for every
   `c ∈ ℝ`, with sign sequence `−, +` in case of equality. Proof: one
   direction by contradiction from (2.1); the converse by the choice
   `c_α = G⁻¹(α) − F⁻¹(α)`.
2. **Theorem 2.2 (p.313).** On supports `[0, ∞)`: `F ≤disp G` iff
   `F ≥ G` on `(0, ∞)` (2.5) and `S⁻(F_c − G) ≤ 1` for `c > 0` (2.6).
3. **Theorem 2.3 (p.314).** `F ≤disp G` iff `Y = φ(X)` in law with
   `φ = G⁻¹ ∘ F` and `φ' ≥ 1`. Remark 2.3: equivalently
   `f(F⁻¹(u)) / g(G⁻¹(u)) ≥ 1`.
4. **Illustration (p.315).** `Y = aX`, `a > 1`, gives `X ≤disp Y`.
5. **Lemma 2.1, Theorems 2.5, 2.6 (pp.315-316).** Density sufficient
   conditions via total positivity of order infinity of the indicator kernel
   (Karlin 1968).
6. **Examples (pp.317-320).** Gamma family ordered by shape (3.1); log-gamma
   reversed (3.3); ratios of gammas (3.5), (3.7); exponential against
   `x/(1+x)` (3.8, via Theorem 2.3 with `φ(x) = e^x − 1`); Weibull and
   lognormal families not ordered (Examples 3.4, 3.5).

## What could not be reconstructed

Nothing material; the examples' sign-change counts were read but are not
used.
