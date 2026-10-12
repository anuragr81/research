# Results: where banks pay dividends

Descriptive. The solved model pays dividends only at the barrier
y* = 1.571518 (solve resultM_lambda_1.0000_maxiter3000.mat), so it predicts
no dividend below it. No choice here can turn 56% into 0%, so no
pre-registration was needed. Run 12 October 2026.

Pinned: fdic_distress_panel_2015_2024.csv.gz sha256 3cb78c0aa33691037f275eaf7bea5a75d02958161e0f8f7f2318fae4b0cc879b

Reproduce: python3 dividend_location.py fdic_distress_panel_2015_2024.csv.gz

```
panel sha256 3cb78c0aa33691037f275eaf7bea5a75d02958161e0f8f7f2318fae4b0cc879b
dividend paid in the quarter, by x = ASSET/LIAB, banks with assets >= $100m; y* = 1.571518
  x in [1.0000, 1.0300):     653 bank-quarters, paid  49.5%
  x in [1.0300, 1.1000):   43417 bank-quarters, paid  60.1%
  x in [1.1000, 1.2000):  112264 bank-quarters, paid  56.5%
  x in [1.2000, 1.3900):    7733 bank-quarters, paid  32.0%
  x in [1.3900, 1.5715):     411 bank-quarters, paid  17.5%
  x in [1.5715, 2.0000):     294 bank-quarters, paid  35.0%
below y*: 164478 bank-quarters, paid 56.2%; the model predicts 0%
```
