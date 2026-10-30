# Notes: Choulli, Taksar and Zhou (2004)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated page of the
  pinned PDF. Controls: a fabricated quotation and a true quotation on the
  wrong page are both refused.
- The Lean results in `CLAIMS.md` are declared in
  `theory/lean/ChoulliTaksarZhou.lean`, which builds with no `sorry` and
  audits within propext, Classical.choice and Quot.sound.
- CTZ-D1 was checked against the rendered page 9, not only the extracted
  text.

## Not verified

- The verification theorem (Theorem 2.2), the case analysis of Section 3
  (Propositions 3.3 to 3.17) and the policy construction of Theorem 4.1.
  Read, not formalised.
- The typeset journal version. Reference [4], the SIAM paper of 2003.

## Discrepancies

- CTZ-D1 and CTZ-D2, two typographical slips in the constants of the middle
  case of Theorem 3.4. Neither changes the policy.

## Controls

- `ChoulliTaksarZhou.control_printed_exponent_fails`.
- `ChoulliTaksarZhou.control_printed_ktilde_fails`.

## Consequences for the theory paper

- C5. The paper does not pre-empt K2. Its exposure bounds are constant and
  its liability is deterministic (CTZ-D3, CTZ-D4), so where a bound binds
  the reserve variance is a constant, and there is no asymmetry parameter
  for the variance to be free of (CTZ-D5). K2's new content is the
  state-dependent two-branch cap in the buffer coordinate, the unhedged
  liability shock inside the limits, and their freedom from the parameter
  that moves the thresholds.
- The qualitative message that a binding constraint, not the objective,
  sets the volatility at the ends of the state space is here, for constant
  bounds. K2's claim should be worded to say so.
- The hypothesis of M11 that the exposure binds the cap near both ends has
  a precedent here. The minimum exposure binds near ruin (CTZ-Q11) and,
  with barrier dividends as in [4], the maximum exposure binds before the
  dividend barrier (CTZ-Q15). The method of Proposition 3.3, an ODE for the
  feedback $a(x)=-\mu V'/(\sigma^2V'')$ showing that once the feedback
  crosses a bound it does not return, is a template for proving the
  hypothesis in the paper's model, where the cap moves with the state.
- C5 still waits on [4], which this paper reports but which was not read.
