# Replication of Peura and Keppo (2006), stage 4: the model's untested predictions

Run 12 October 2026 under PREREG_stage4.md (commit a8998384). The RBCT2
supplement was fetched by fetch_stage4.py after that commit.

Pinned: fdic_dec_2004_2024.csv.gz sha256 0f3af028bb3471f2693e34869663c050909adc6cdeae89f294f1d2b3d4f1801d
Pinned: fedfunds.csv sha256 74680b2d09bb28f90fb30c1c09e93ac2d443de1b7345ba5a9112b3eca9be0a25
Pinned: ../../06_empirical_starter/fdic_distress_panel_2015_2024.csv.gz sha256 3cb78c0aa33691037f275eaf7bea5a75d02958161e0f8f7f2318fae4b0cc879b
Pinned: fdic_rbct2_2015_2024.csv.gz sha256 5b63c5903d671634ef1d92375a421ec3b5dbe2931306996a06005fa3292dacfc

Reproduce: python3 stage4.py fdic_dec_2004_2024.csv.gz fedfunds.csv ../../06_empirical_starter/fdic_distress_panel_2015_2024.csv.gz fdic_rbct2_2015_2024.csv.gz

```
fdic_dec_2004_2024.csv.gz sha256 0f3af028bb3471f2693e34869663c050909adc6cdeae89f294f1d2b3d4f1801d
fedfunds.csv sha256 74680b2d09bb28f90fb30c1c09e93ac2d443de1b7345ba5a9112b3eca9be0a25
fdic_distress_panel_2015_2024.csv.gz sha256 3cb78c0aa33691037f275eaf7bea5a75d02958161e0f8f7f2318fae4b0cc879b
fdic_rbct2_2015_2024.csv.gz sha256 5b63c5903d671634ef1d92375a421ec3b5dbe2931306996a06005fa3292dacfc

===== SAMPLE A (all national banks): 453 banks, 256 issuer, 197 non-issuer
T1 dividends below the barrier: 63/9816 = 0.6% (model 0%) -> holds   [issuer 63/4682 = 1.3%; non-issuer 0/5134 = 0.0%]
T2 capital far above the barrier: 14378/15243 = 94.3% (model 0%) -> fails   [issuer 7474/8224 = 90.9%; non-issuer 6904/7019 = 98.4%]
T3 issue from near u1: 14/201 = 7.0% (model 100%) -> fails
T4 issue lands near u2: 16/201 = 8.0% (model 100%) -> fails
median b_(q-1) - barrier over dividend quarters: +6.12 pp
median b_q - u2 over episodes: +5.25 pp

===== SAMPLE B (61 largest): 61 banks, 29 issuer, 32 non-issuer
T1 dividends below the barrier: 5/1583 = 0.3% (model 0%) -> holds   [issuer 5/612 = 0.8%; non-issuer 0/971 = 0.0%]
T2 capital far above the barrier: 2241/2418 = 92.7% (model 0%) -> fails   [issuer 1004/1139 = 88.1%; non-issuer 1237/1279 = 96.7%]
T3 issue from near u1: 2/35 = 5.7% (model 100%) -> fails
T4 issue lands near u2: 4/35 = 11.4% (model 100%) -> fails
median b_(q-1) - barrier over dividend quarters: +4.12 pp
median b_q - u2 over episodes: +3.82 pp

VERDICTS
  T1 dividends below the barrier: A holds, B holds
  T2 capital far above the barrier: A fails, B fails
  T3 issue from near u1: A fails, B fails
  T4 issue lands near u2: A fails, B fails
```

## Verdicts: the baseline's scorecard

| Test | Model | A (453 banks) | B (61 largest) |
|---|---|---|---|
| T1 dividends paid below the dividend barrier | 0% | 0.6%: holds | 0.3%: holds |
| T2 capital far above the dividend barrier | 0% | 94.3%: fails | 92.7%: fails |
| T3 issues start near u1 | 100% | 7.0%: fails | 5.7%: fails |
| T4 issues land near u2 | 100% | 8.0%: fails | 11.4%: fails |

## Reading

- With each bank's own barriers, Peura and Keppo's dividend barrier is low
  (stage 3: 3-5% above the 8% minimum) and banks sit far above it: in 93-94%
  of bank-quarters the buffer exceeds the barrier by more than a year of
  expected earnings plus two quarters of noise. The model pays any such
  excess out at once.
- T1 holds because of T2: almost every dividend is paid from capital far
  above the barrier (median 4-6 points above). T1 does not show that
  dividends follow the barrier; it shows that the barrier is not where
  dividends start. E3, with our model's single barrier, failed for the
  opposite reason.
- Recapitalisations neither start at the issue barrier nor end at the
  dividend barrier: 93-94% start more than σ above u1, and they land, on
  median, 3.8-5.3
  points above u2. Banks recapitalise from positions the model calls safe.
- The baseline fails three of four of its own untested predictions, on the
  banks most like Peura and Keppo's and on all national banks alike. With
  stages 2 and 3 (capital levels underpredicted; buffers track volatility
  and the barrier in the cross-section), this is the baseline the enhanced
  model is to be scored against.
