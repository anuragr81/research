# Reconstruction of Genicot and Ray, 2017

**Source read.**

| Short | Full | Copy | Evidence |
|---|---|---|---|
| GR | "Aspirations and Inequality", *Econometrica* 85(2), March 2017, pp.489–519 | Econometric Society PDF with text layer, 32 pages (a cover sheet, then pp.489–519), Drive `GenicotRayAsp.pdf`, id `1a58FN3l3SlgLr4TnA61gmIMmAIC0XsNp`, uploaded 12 Oct 2026; cached `~/.cache/firmworkers/genicot_ray_2017.pdf`, sha256 `cbf493e019f1bb91a385d6e32a72e3c2f8d1c0d6bfdeb501c207feae4a9a9bcb` | [F], published; the Supplemental Material is not read |

Text extracted with `pdftotext -layout`; form-feed block `i ≥ 1` is printed page
`488 + i`, and every block's running head carries its page number. The proof
of Observation 1 (p.512) was read from the page image at 110 dpi. Read in full
on 12 Oct 2026, main text and Appendix.

**Why this paper.** Ray and Robson (2012, RR-7) credit it with reference points
built from the income distribution. It is the closest precedent for the
reference income `R_h` of the reference-dependent sub-section (decision of
12 Oct 2026), and it names group-based aspirations by caste or religion as an
extension.

## Primitives (§3, pp.492–495)

- Single-parent dynasties; income `y` split between consumption `c` and the
  child's wealth `z`; payoff `u(c) + w0(z) + w1(max{z − a, 0})`, with
  "milestone utility" `w1` on the excess over the aspiration `a`.
- Aspirations `a = Ψ(y, F)`, own income and the society-wide distribution (1):
  regularity, scale-invariance `Ψ(λy, F^λ) = λΨ(y, F)`, range-boundedness, and
  (where stated) social monotonicity under first-order dominance.
- §3.3: windows may be local in income, or formed by occupation, caste,
  religion or geography; group-based aspirations with or without cross-identity
  interactions.

## Derivation chain

1. **Proposition 2 (p.497).** For given wealth, a unique aspiration threshold
   `a∗` separates satisfied from frustrated aspirations; investment rises with
   aspirations while satisfied and is insensitive once frustrated.
2. **Constant elasticity growth model (§4.2).** `f(k) = ρk`, common elasticity;
   the problem (10) reduces to the growth factor problem (11) in `g = z/y` and
   the aspirations ratio `r = a/y`. Proposition 3: a unique `r∗ > 1`; growth
   factor `g` (frustrated) above `r∗`, `g(r) > g` below.
3. **Observation 1 (p.499).** Scale-invariance and social monotonicity make
   `Ψ(y, F)/y` strictly decreasing in `y`. Proposition 4: an inverted-U growth
   incidence curve.
4. **Proposition 5 (p.501).** With range-bounded aspirations no steady state is
   perfectly equal: at a single point `a = y∗`, and `z = a` would need two
   incompatible first-order inequalities when `w1′(0) > 0`.
5. **Proposition 6 (p.502).** A stationary state is bimodal (Lemma 1 on the
   gap `y − Ψ(y, F)`).
6. **Propositions 7 and 8 (pp.504–509).** With growth, either convergence to
   equality or persistent divergence into two clusters; without social
   monotonicity, also infinite crossings.
7. **Conclusion (§6).** Multidimensional aspirations; group-based aspirations
   linked to frustration and violence.

## What was reconstructed

Observation 1 (abstractly, from scale-invariance and strict monotonicity under
scaling), Lemma 1, the average-of-own-and-mean aspirations of Examples 1 and 3
(its ratio, range and scale properties), the two steps of Proposition 5, the
scale reduction from (10) to (11), and the comparison step behind `g(r) > g` in
Proposition 3. See `CLAIMS.md`.

## What was not reconstructed

Propositions 1, 2, 4 and 6–8 beyond the lemmas above, Examples 2 and 4–7,
Observations 2 and 3, Lemmas 2–6, and the Supplemental Material.
