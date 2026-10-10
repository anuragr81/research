# Notes: Engle and Siriwardane (2018)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated page of the
  pinned PDF, after Unicode normalisation (the "ﬁ" ligature becomes "fi").
  Control: a fabricated quotation and a true quotation on the wrong page are
  both refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/EngleSiriwardane.lean`, which builds with no `sorry` and
  audits within propext, Classical.choice and Quot.sound. Our reading of
  eqs. (8) and (16) and of the share arithmetic is consistent with the
  source.

## Not verified

- The estimates themselves (data not supplied).
- The Online Appendix (not supplied).

## Discrepancies

- ES-D1. "100−17 ≈ 80%" rounds 83% down.
- ES-D2. The C.2.4 recursion display differs from eq. (15).

## Controls

- `EngleSiriwardane.control_symmetric_without_gamma`. The news asymmetry
  needs $\gamma\ne0$.
- `EngleSiriwardane.control_market_share_is_not_eighty`. The computed market
  share is not the printed 80%.

## Consequences for this bundle

- The paper separates a structural source of an observed volatility
  asymmetry (mechanical leverage, about 3% or 14%) from market exposure. This
  is the empirical counterpart of separating cap geometry from preference in
  C2. The parallel ends at the object, since their asymmetry is a news
  response and not a ratio of state-dependent variances, and their residual
  goes to risk premia.
- The project's earlier note in `00_reader/TODO.md` item B2 reports the same
  figures (0.97, 0.86, 0.17, about 3%, 14% and 80%). They are now checked
  against the published text.
