# Schoemaker (1982), reconstruction

Paul J. H. Schoemaker, "The Expected Utility Model: Its Variants, Purposes,
Evidence and Limitations", *Journal of Economic Literature* 20(2), June 1982,
pp. 529-563. Read in full on 6 Oct 2026 from Drive folder
`1r7J3xJ2rotpmlRaBANCNhviy2d8a1bx_`, file
`1982-Schoemaker_ExpectedUtilityModel-VariantsPurposesLimits_JofEconomicLiterature.pdf`.
A survey, so its formal content is the standard theory it summarises.

## Objects, in our notation

- A lottery is a finite list of outcomes with probabilities summing to one.
- The von Neumann-Morgenstern model ranks lotteries by the expected value of a
  utility `u` built from choices among lotteries (Table 1 variant 3, p.538).
- A utility `v` built under certainty measures strength of preference, and is
  a different object from `u` (pp.533-535).

## Derivation chain

1. p.531. The five axioms give a utility whose expectation ranks lotteries as
   the person does, unique up to `aU + b` with `a > 0`.
2. p.532. In this model utility represents preference rather than causing it.
3. p.533. The scale is an interval scale, so ratios of utility differences are
   invariant, but as a theory of preference it is ordinal and does not measure
   strength of preference under certainty.
4. p.535. Reading a concave `u` as diminishing pleasure from money under
   certainty confuses `u` with `v`.
5. pp.538-541. Four purposes (descriptive, predictive, postdictive,
   prescriptive) need different evidence.

## What could not be reconstructed formally

- The representation theorem itself and the "only if" half of step 1 (every
  representing utility is affine in another). The Lean proves the "if" half.
- Steps 2 to 5 are claims about interpretation and evidence, verified by
  quotation.
