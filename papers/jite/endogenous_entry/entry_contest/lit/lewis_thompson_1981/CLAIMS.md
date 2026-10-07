# Claims about Lewis & Thompson (1981)

**Paper.** Toby Lewis and J. W. Thompson, "Dispersive distributions, and the
connection between dispersivity and strong unimodality", *Journal of Applied
Probability* 18, 76-90, 1981. Read in full from `dispersive_distribution.pdf`
(Drive id `14ekjfuJuUZt7VGJ-LDxv4R0P4mY3qkLc`). Evidence level **[F]**. The
scan lacks the reference list. Locators are printed page numbers.

Our documents cited Shaked (1982) for the dispersive order and did not cite
this paper. The author decided on 7 Oct 2026 to use it as an earlier source
for the order. The claims below are what our documents now attribute to it.

| ID | Claim | Source locator | Lean |
|---|---|---|---|
| LT-1 | The order is defined by quantile spacing. "We call the relationship defined by (1.1), (1.2) an ordering in dispersion of G and H, since it implies that any pair of quantiles of H, z_α and z_β say, are at least as widely separated as the corresponding quantiles of G, y_α and y_β." | p.77 | `OrdCdf`, `OrdSpacing`, `cdf_iff_spacing` |
| LT-2 | The order is Hopkins and Kornienko's dispersive order, `z − y` non-decreasing, and the transport map `T = z ∘ G` has a non-decreasing displacement exactly when it holds. With a crossing point `T` is a pivot-spread. | our reading of p.77 against Hopkins and Kornienko's Definition 1 | `OrdDiff`, `spacing_iff_diff`, `transport_disp`, `transport_pivot` |
| LT-3 | "The o.d. relationship is clearly invariant under shifts of location and also under a common change of scale." | p.77 | `affine_invariant` |
| LT-4 | "if X is any random variable whose distribution function is strictly increasing on R, then the distributions of X and kX form an o.d. pair for k ≠ 1." True for `k > 0`. False at `k = −1`. | p.77 | `scale_pair`, `qAsym_strictMonoOn`, `qAsym_onto`, `neg_quantile`, `spacing_gap`, `neg_not_ordered` |
| LT-5 | Theorem 1, ordering gives `g(y_α) ≥ h(z_α)` where the derivatives exist, and Theorem 2, the converse "If g(y_α) ≥ h(z_α) for every α, then G ≤disp H", in quantile-derivative form. | p.80-81 | `thm1`, `thm2`, `thm2_density` |
| LT-6 | "Normal distributions are ordered in dispersion according to their variances" and "Pareto distributions are ordered in dispersion according to their indices, the distribution with the smaller index being more dispersed". Lognormals are ordered only when their `σ` agree. | p.89-90, §6.3 | `scale_family`, `pareto`, `lognormal_ratio` |
| LT-7 | "No mixture of exponential distributions can be dispersive", through `ff'' ≤ (f')²` failing by Schwarz's inequality. Formalised for two components. | p.89, §6.2 | `mixture_logconvex` |
| LT-8 | Theorems 7 and 8. A distribution is dispersive, meaning it preserves the order under convolution, exactly when it has a log-concave density. | p.86-89 | not formalised |
