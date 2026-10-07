# Reconstruction of Lewis and Thompson, "Dispersive distributions, and the connection between dispersivity and strong unimodality"

**Source read.** Toby Lewis and J. W. Thompson, *Journal of Applied
Probability* 18, 76-90, 1981, read in full from `dispersive_distribution.pdf`
(Drive id `14ekjfuJuUZt7VGJ-LDxv4R0P4mY3qkLc`, uploaded 7 Oct 2026). The scan
has 17 pages. Printed pages 76 to 90 are PDF pages 1 to 15, and PDF pages 16
and 17 are blank. The article's reference list is not in the scan. Every page
was read from the rendered images, since the PDF has no text layer. Locators
are printed page numbers.

**Why this paper.** The upload was expected to be Shaked (1982), which
`PROOFS.tex` cites for the dispersive order. It is a different paper. The
author decided on 7 Oct 2026 to treat it as an earlier source for the same
order. Shaked (1982) remains unread.

## 1. Objects

| Paper | Meaning | Locator | Our nearest object |
|---|---|---|---|
| `G`, `H` | two distribution functions | p.76 | the wealth distribution before and after a spread |
| `y_α`, `z_α` | corresponding quantiles, `G(y_α) = H(z_α) = α` | p.76, (1.1) | `Λ_α^{-1}(r)`, `Λ_β^{-1}(r)` in the quantile coupling of `PROOFS.tex` |
| `G <disp H` | "ordering in dispersion", `H(z_α + c) < G(y_α + c)` for every `α` and `c > 0` | p.77, (1.2) | the dispersive order, `F ≤_d G` in Hopkins and Kornienko |
| `x_α`, `x̄_α`, `x_α^(θ)` | lower and upper α-quantiles, and points between them, for distributions with flats | p.78, (1.3) to (1.5) | none |
| `G ≤disp H` | the weak form, `G(y_α + c) ≥ H(z_α + c)` for `c > 0` | p.78, (1.6) | the weak dispersive order |
| dispersive `F` | `F * G ≤disp F * H` whenever `G ≤disp H` | p.83 | none |
| strongly unimodal | a log-concave density (Ibragimov 1956) | p.88-89 | none |

## 2. Derivation chain

1. **The definition** (p.76-78). Any two quantiles of `H` are at least as far
   apart as the corresponding quantiles of `G`. For continuous, strictly
   increasing distribution functions this is (1.2), and for general ones the
   paper redefines quantiles by (1.3) and uses (1.6).
2. **Elementary properties** (p.77). The relation is invariant under a shift
   of location and under a common change of scale, it is transitive, and "if X
   is any random variable whose distribution function is strictly increasing
   on R, then the distributions of X and kX form an o.d. pair for k ≠ 1".
3. **Lemmas 1 to 5** (p.79-80). The weak order is symmetric in `c` (Lemma 1),
   and the choice among quantiles inside a flat does not matter (Lemma 5).
4. **Densities** (p.80-81). Theorem 1. Ordering implies `g(y_α) ≥ h(z_α)` where
   the derivatives exist. Theorem 2. For absolutely continuous `G, H` with
   densities positive above a point, `g(y_α) ≥ h(z_α)` for every `α` implies
   `G ≤disp H`, through `y_{α2} − y_{α1} = ∫_{α1}^{α2} dα / g(y_α)`.
5. **Weak limits** (p.81-82, Lemma 6, Lemma 7, Theorem 3). Ordering is
   preserved under weak convergence.
6. **Smoothing** (p.83, Theorem 4). Convolution with a vanishing normal gives
   smooth densities converging to the original density.
7. **Dispersive distributions** (p.83-89). Theorem 5 lists closure
   properties. Theorem 6 shows the exponential is dispersive. Theorem 7 shows a
   distribution with a positive density whose log has a second derivative is
   dispersive exactly when that log is concave. Theorem 8 extends this to
   every non-degenerate distribution, so dispersive distributions are the
   strongly unimodal ones.
8. **Examples** (p.89-90). Section 6.1 lists dispersive and non-dispersive
   families. Section 6.2 shows no mixture of exponentials is dispersive, by
   Schwarz's inequality. Section 6.3 orders normals by variance and Paretos by
   index, and shows two lognormals are ordered only when their `σ` agree.

## 3. What each result is derived from

| Result | Derived from | Role |
|---|---|---|
| (1.2), (1.6) | the quantile functions | the definition |
| Theorem 1 | Lemma 1 and differentiability at the quantiles | a necessary condition in densities |
| Theorem 2 | the inverse-function formula for quantiles | a sufficient condition in densities |
| Theorems 7 and 8 | Theorem 2, a two-point distribution `G`, Lemma 8, Ibragimov | the characterisation of dispersive distributions |
| §6.3 | Theorems 1 and 2 with closed-form quantiles | examples of the order |

## 4. What could not be reconstructed

1. Saunders and Moran (1978), which the paper credits with the gamma result
   that motivated the order (p.76). Not read.
2. The step "Elementary manipulation shows that d²/dx² ln f(x) ≤ 0" in the
   proof of Theorem 7 (p.86), from the three-point inequality.
3. Ibragimov's theorem, used for Theorem 8 (p.88).
4. The reference list, absent from the scan.
