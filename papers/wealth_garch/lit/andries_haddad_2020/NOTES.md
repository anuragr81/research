# Notes: Andries and Haddad (2020)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim, after the suite's
  normalisation, in the OCR text of its stated page under the page map of
  AH-D1. Controls: a fabricated quotation, a true quotation on the wrong
  page and a page before the first PDF page are all refused, and the OCR
  text is stamped with the pinned PDF's sha256.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/AndriesHaddad.lean`, which builds with no `sorry` and
  audits within propext, Classical.choice and Quot.sound. For finite
  lotteries, our reading of eq. (2) is consistent with the source, and the
  example of AH-D4 shows the inequality of Proposition 1 with the gap
  $\theta/(4+2\theta)$.

## Not verified

- Every proof. The appendices are online and were not supplied (AH-D3).
  Propositions 1 and 2 in general, Lemma 2, Propositions 4 to 9 and the
  fixed-cost derivations rest on the paper's word.
- Proposition 3's "increasing in T" with the certainty-equivalent rate
  varying (AH-D5).
- The calibrated numbers (1.1 years, 118 basis points, $X_l^*=0.77$, the
  elasticities $-0.63$ and $0.79$, the correlation thresholds 37.5 and
  22.5 percent).
- Eq. (2) for continuous distributions. The Lean `solvesCE` is the
  finite-lottery form.

## Discrepancies

- AH-D1. Printed page 1923 appears twice in the supplied PDF, and printed
  page 1939 is absent.
- AH-D2. The OCR text layer drops or swaps the letter f and reads some
  symbols as letters.

## Controls

- `AndriesHaddad.control_no_aversion_at_theta_zero`. Without
  disappointment aversion the single-date and two-step values coincide, so
  information aversion needs $\theta>0$.
- `AndriesHaddad.control_ce2_degenerate`. At $q=1$ the two-point value is
  $L$, so strict betweenness needs $q<1$.

## Consequences for this bundle

- The paper is a preference-based account of infrequent updating. Each
  intermediate signal is costly under eq. (1) and eq. (2), so the agent
  observes rarely, more rarely when volatility is high (AH-Q10) and after
  bad news (AH-Q15). It occupies the slot Pagel (2016) leaves open in her
  Appendix C, where sluggish updating is assumed. It is one of two
  candidate accounts; the other is a capacity constraint on perception
  (Woodford 2012, Frydman and Jin), which this bundle is exploring.
- The two accounts change different objects. Here the update rule is
  Bayesian and the filtration is coarse by choice. Under efficient coding
  the agent attends continuously and the perceived signal is distorted.
  They also pull against each other, since an information-averse agent
  would pay to avoid the frequent signals an adaptive encoder uses
  (AH-Q6). That is a reason to keep them in separate papers.
- On price volatility the paper says nothing. The price is an exogenous
  geometric Brownian motion and $\sigma$ is an input. The channel it
  establishes runs from volatility to attention, the reverse of the
  direction this bundle asks about.
- With BHS and Pagel. The kink is at the certainty equivalent, as in
  Routledge and Zin (2010), which the paper cites for equilibrium prices.
  The paper itself does not study prices, so it neither confirms nor
  contradicts `BarberisHuangSantos.dispersion_free_of_f` or
  `Pagel.log_dispersion_free_of_state`.
- No claim of novelty rests on this record. The remaining papers that
  could hold the result (Frydman and Jin, Woodford, Khaw, Li and Woodford,
  McQueen and Vorkink) are unread.
