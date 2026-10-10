# Notes: Barberis, Huang and Santos (2001)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated page of the
  pinned PDF. Control: a fabricated quotation and a true quotation on the
  wrong page are both refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/BarberisHuangSantos.lean`, shared with the working-paper
  record, which builds with no `sorry` and audits within propext,
  Classical.choice and Quot.sound.

## Differences from the working paper (NBER WP 7220, 1999)

Read in both versions, page by page.

- Economy II, in which dividends and consumption follow separate processes
  ($\sigma_D=12\%$, $\omega=0.15$, Prop. 2). The working paper only discusses
  this as an extension (WP pp. 32 to 33).
- The constant-loss-aversion model moves from Section 2 of the working paper,
  set in an Economy I of consumption equal to dividends (log-return
  volatility 3.79%), to Section VI, set in Economy II (12%, Table XIII, the
  same at every $b_0$).
- Calibration. $\gamma=1.0$ (WP 0.9). Baseline $k=3$, chosen to keep average
  loss aversion near 2.25 (WP 50). $\eta=0.9$ (WP 1). $\bar R$ pinned by a
  median $z$ of one (WP mean). 50,000 draws (WP 10,000).
- Results the working paper does not have. A low correlation of returns with
  consumption (Economy II). Sensitivity tables for $k$, $\lambda$, $\eta$, the
  evaluation period and the reference level (Tables VII to XI). A note on
  aggregation (IV.D). A measure of average loss aversion (Appendix). A
  sufficiency proof for the Euler equations (Appendix).
- Unchanged. The kinked-linear $v$ at $z=1$, the cushioning of losses by
  prior gains, the penalty $\lambda+k(z-1)$ after prior losses, and the
  conclusion that constant loss aversion leaves return volatility at that of
  the cash flows.

## Consequences for this bundle

- For C2 the journal version states the parallel more strongly than the
  working paper, since Table XIII shows the volatility unchanged across four
  values of $b_0$ (QJE-Q2). Which version L2 cites is the author's decision.
