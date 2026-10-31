# Notes: Pagel (2016)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated page of the
  pinned PDF, after the suite's normalisation. Controls: a fabricated
  quotation, a true quotation on the wrong page and a page before the first
  PDF page are all refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/Pagel.lean`, which builds with no `sorry` and audits within
  propext, Classical.choice and Quot.sound. Our reading of eq. (7), of
  Propositions 2 and 3 in difference form, and of eq. (10) is consistent
  with the source.

## Not verified

- The power cancellation between the first-order condition of Appendix B.1
  and eq. (7). It is done by hand, and `Pagel.foc_iff_rho` starts after it.
- Proposition 1's existence claim beyond the guess and its verification
  (PG-Q9). Convergence of $Q$, existence of the fixed point $\psi$, the
  second-order condition and transversality are not checked by the paper or
  by us.
- The sign $\Omega>0$ for $\theta>1$ (Appendix B.3). It is used as a
  hypothesis ($\gamma Q<\psi$) in `Pagel.rho_strictAnti_in_F`, not proved.
- The moments of Tables 2 and 3, the Online Appendix, and the simulations
  "available on request".
- The typeset journal version (PG-D1).

## Discrepancies

- PG-D3. "the risk-free rate is constant" (p. 14) against eq. (11).
- PG-D4. "I chose F(εt) = 1" (p. 42) where the formula needs $F=0$.
- PG-D5. "Pt = Ct − Wt" (p. 36) for $P_t=W_t-C_t$.
- PG-D7. The sluggish loading described through the consumption-wealth
  ratio (p. 26) and defined through the risk-free rate (p. 43).

## Controls

- `Pagel.control_rho_flat_at_psi_eq_gammaQ`. Without $\psi>\gamma Q$ the
  ratio need not move with the shock.
- `Pagel.control_weight_flat_at_lam_one`. Without $\lambda>1$ the weight
  does not move with the shock.
- `Pagel.control_gross_dispersion_depends_on_state`. The state-free
  dispersion of `Pagel.log_dispersion_free_of_state` is a property of the
  log return, and fails for the gross return.

## Consequences for this bundle

- The question checked against this paper is whether a prospect-theory-like
  utility whose reference point follows a derived model of information
  processing already has its volatility structure worked out. The paper does
  not settle it. Its reference point is endogenous but moves by full
  rational updating. In the basic model that gives a state-dependent level
  of risk and no persistence (PG-Q7). The conditional dispersion of the log
  return is constant (PG-D8). Persistence enters only through an assumed
  AR(1) rule, either in the consumption process (Section 4.1) or in beliefs
  (PG-Q11), solved numerically (PG-Q12).
- The sluggish belief rule of Appendix C occupies the slot that a derived
  updating rule, such as one from efficient coding, would fill. The paper
  gives no model of why updating is sluggish.
- With BHS. In Barberis, Huang and Santos (2001), constant loss aversion
  leaves the price-dividend ratio constant and return volatility equal to
  dividend volatility (`BarberisHuangSantos.dispersion_free_of_f`). Here the
  ratio moves with the shock, but log-return dispersion is still free of the
  state (`Pagel.log_dispersion_free_of_state`). The parallel ends at the
  object. The BHS identity holds because the ratio is constant, and this one
  holds because the ratio is i.i.d. and predetermined.
- No claim of novelty rests on this record alone. The other papers that
  could hold the result (Frydman and Jin, Woodford, Khaw, Li and Woodford,
  Andries and Haddad, McQueen and Vorkink) are unread.
