# Notes — Lewis & Thompson (1981), pass 1 (full read and Lean)

Run from the repository root `python3 lit/lewis_thompson_1981/verify_lt.py`.
The suite measures `lean/mathlib/LewisThompson.lean`, which uses Mathlib for
real analysis and follows the Mathlib audit rule.

**Scope.** Our reading of the source is consistent with it, and the parts
listed in `CLAIMS.md` with a Lean name are proved. Theorems 7 and 8, the
weak-limit results of Section 3 and the convolution results of Sections 4 and
5 are not formalised. They concern dispersive *distributions*, which our
documents do not use.

## 1. What the paper settles for us

1. **The order predates Shaked (1982).** Lewis and Thompson define "ordering
   in dispersion" by quantile spacing (p.77) in a paper received in 1977 and
   published in 1981. The quantile-spacing form, the CDF form (1.6) and
   Hopkins and Kornienko's Definition 1 (`z − y` non-decreasing) are the same
   order for continuous, strictly increasing distribution functions
   (`cdf_iff_spacing`, `spacing_iff_diff`). The paper itself credits the
   phenomenon to Saunders and Moran (1978), which was not read.
2. **The bridge to pivot-spreads holds over the reals.** The transport map
   `T = z ∘ G` has a non-decreasing displacement exactly when the order holds
   (`transport_disp`), and with a crossing point it is a pivot-spread
   (`transport_pivot`). `lit/hopkins_kornienko/Dispersive.lean` proved the
   crossing step over the integers in core Lean.

## 2. Discrepancies against the source

1. **"X and kX form an o.d. pair for k ≠ 1" is false at `k = −1`** (p.77). For
   `k > 0` the claim holds, `kX` being more dispersed when `k > 1`
   (`scale_pair`). Take the quantile function `q(p) = log(p/(1−p)) + p²`. It is
   strictly increasing and maps `(0, 1)` onto `ℝ` (`qAsym_strictMonoOn`,
   `qAsym_onto`), so its distribution function is strictly increasing on `ℝ`,
   as the claim requires. The quantile function of `−X` is `−q(1 − p)`
   (`neg_quantile`). The difference of the two spacings over `[a, b]` is
   `2(b − a)(1 − a − b)` (`spacing_gap`), positive on `[1/10, 2/10]` and
   negative on `[8/10, 9/10]`, so neither distribution is more dispersed
   (`neg_not_ordered`). The statement needs `k > 0`, or symmetry about a point
   when `k < 0`.
2. **Theorem 1 is stated with one-sided derivatives of the distribution
   functions.** `thm1` proves it in quantile form, `y' ≤ z'`, which is the
   same inequality where the densities are positive. No discrepancy.

## 3. Terminology to keep apart

The paper's "dispersive" is a property of one distribution (Theorem 8, a
log-concave density). Our documents say "dispersive" of a spread whose
displacement changes sign once, which is the order of LT-1 plus a crossing
point. The two meet only in name. Wherever our documents cite this paper they
should say "ordering in dispersion" for the order and avoid "dispersive
distribution".
