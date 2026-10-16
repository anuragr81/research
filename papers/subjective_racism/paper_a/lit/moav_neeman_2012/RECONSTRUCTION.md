# Moav and Neeman (2012), reconstruction

Omer Moav and Zvika Neeman, "Saving Rates and Poverty: The Role of
Conspicuous Consumption and Human Capital", *Economic Journal* 122(563),
September 2012, pp. 933–956, doi 10.1111/j.1468-0297.2012.02516.x.

## Source read

- The published article, 24 PDF pages (journal pp. 933–956), a Wiley download
  dated 26 November 2023 in its margin. Cached at
  `~/.cache/paper_a/sources/moav_neeman_2012.pdf`, sha256
  `fb6e40bd940465af8607d1bf6d8964a834b2a4ba79d418fc34546621a5e364c0`, and its
  text layer at `~/.cache/paper_a/sources/moav_neeman_2012.txt`, sha256
  `b915559b7975cd3b5b8e2cf298c129e82ad586f238f5362ab25b5e7b892e142a`.
- Every page was read from the text layer on 16 October 2026 [F]. PDF pages
  9, 12, 13, 14, 16, 17, 18, 19 and 20 were also rendered at 110 dpi and read as
  images, because the text layer garbles the symbols of the model, eqs. (1) to
  (11), the mapping `φ` and the proofs (MN-D6).

## The model, from its own primitives

1. **Generations** (p. 941). Overlapping generations, one parent and one
   child. Spending `e` on schooling gives human capital `h = θ + γ e`, with
   `θ > 0`, `γ > 1` (eq. 1).
2. **Income** (p. 941). `y = h + π`, with `π ∈ [π̲(h), π̄(h)]` unobserved, of
   mean zero and from a continuous distribution.
3. **Choice** (p. 941). The old split income into consumption `c`,
   conspicuous consumption `x` and bequest `b`, `c + b + x ≤ y` (eq. 2), with
   utility `B (c^{1−β} b^β)^{1−λ} S^λ` (eq. 3), where `S = E(y | h, x)` is the
   social belief about income given the observed `h` and `x` (MN-Q5).
   Consumption and bequest are unobserved.
4. **Split** (p. 943). `b = β (y − x)` and `c = (1 − β)(y − x)` (eqs. 4, 5).
5. **Equilibrium** (p. 943). Spending `x(h, y)` is optimal given beliefs
   `ỹ(h, x)`, and beliefs are consistent, `ỹ(h, x) = E[y : x(h, y) = x]`.
6. **First order condition** (p. 944). Reduced utility is
   `(y − x)^{1−λ} ỹ(h, x)^λ` (eq. 6), and
   `(λ/(1 − λ)) (y − x)/ỹ = 1/(dỹ/dx)` (eq. 7).
7. **Separating solution** (p. 944). With `ỹ = y` and the initial condition
   `ỹ(h, 0) = y̲(h) = h + π̲(h)` (MN-Q6), eq. (8) holds implicitly and
   `x(h, y) = λ y [1 − (y̲(h)/y)^{1/(1−λ)}]` (eq. 9). Proposition 1: this is
   the unique fully separating equilibrium.
8. **Properties** (pp. 945–946). `x = 0` at the lowest income, rising and
   concave in `y`, with slope `λ/(1 − λ)` at the lowest income and `λ` at
   high income. At given income, a smaller `h` gives a larger `x`.
9. **Proposition 2** (p. 946). If `y̲(h)` is non-decreasing and convex, the
   share `x(h, h)/h` of the average person falls in `h` (MN-D1).
10. **Proposition 3** (pp. 947–948). Under eqs. (10) and (11), the expected
    share `E[x(h, y)/y]` falls in `h`.
11. **Remark** (p. 948). Without observable `h`, `x(y) = λ y` (MN-Q10).
12. **Dynamics** (pp. 948–950). Along the average path,
    `h_{t+1} = φ(h_t) = θ + γ β {(1 − λ) h_t + λ y̲(h_t)^{1/(1−λ)}/h_t^{λ/(1−λ)}}`,
    linear where `y̲ = 0` and convex beyond, with two crossings of the 45
    degree line when `π̲` is small enough. Dynasties below the threshold stay
    poor.
13. **Refinement** (pp. 952–954). By Proposition A1, an equilibrium that meets a
    variant of the intuitive criterion is fully separating.

## Derivation chain of what Paper A uses

- Eq. (9) satisfies eq. (7) along `ỹ = y` (`eq9_solves_eq7`) and the initial
  condition (`x_at_floor`).
- The share of the average person is `λ (1 − (y̲(h)/h)^{1/(1−λ)})`
  (`share_avg`), non-increasing when `y̲(h)/h` is non-decreasing (`prop2`).
- A non-decreasing, convex `y̲` does not by itself make `y̲(h)/h`
  non-decreasing (`control_prop2_constant_floor`, MN-D1).
- With `h` unobserved, the share is the constant `λ` (`remark_constant_share`,
  `control_unobserved_constant`), and the mapping is linear in case (i)
  (`phi_case_i`).

## What could not be reconstructed

- The uniqueness argument of Proposition 1 and the refinement of
  Proposition A1 are not formalised.
- Proposition 3's expectation over `π` and the two crossings of `φ` are not
  formalised. The derivative printed in the proof of Proposition 3 was checked
  by hand against the image and agrees with the integrand.
- The model has no group and no matching, so nothing in it can be checked
  against A1's engagement rule.
