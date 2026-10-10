# Notes: Barberis, Huang and Santos (1999)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated page of the
  pinned working-paper PDF. Control: a fabricated quotation and a true
  quotation on the wrong page are both refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/BarberisHuangSantos.lean`, which builds with no `sorry` and
  audits within propext, Classical.choice and Quot.sound. Our reading of
  eqs. (4), (5) and (15) is consistent with the source.

## Not verified

- Nothing about the journal version. It has its own record (BHS-D1).
- The numerical solution of Section 3.

## Discrepancies

- None in the mathematics read. BHS-D2 to BHS-D5 are text-layer defects.

## Controls

- `BarberisHuangSantos.control_dispersion_depends_on_state`. Once the
  price-dividend ratio depends on the state, as in Section 3, the dispersion
  of log returns picks up the movement in $f$, so the identity of
  `dispersion_free_of_f` needs a constant $f$.
- `BarberisHuangSantos.control_concavity_needs_lam_ge_one`. Concavity of $v$
  needs $\lambda\ge1$.

## Consequences for this bundle

- The parallel to C2. In Section 2 of this paper the level of loss aversion
  is silent in return volatility, because the price-dividend ratio is
  constant. In this bundle the level of $\lambda_S$ is silent in the
  saturation limits of the $z$-volatility, because the regulatory cap binds.
  The mechanisms differ.
- The paper's loss-aversion term is already kinked-linear (BHS-Q4). A claim
  that a kinked-linear penalty departs from S-shaped models must not suggest
  the kinked-linear form is new (H5).
