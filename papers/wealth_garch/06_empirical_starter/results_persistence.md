# Results: persistence test of M12

Run 12 October 2026 under PREREG_persistence.md (commit 2a56b918).

Pinned: fdic_distress_panel_2015_2024.csv.gz sha256 3cb78c0aa33691037f275eaf7bea5a75d02958161e0f8f7f2318fae4b0cc879b

Reproduce: python3 persistence_test.py fdic_distress_panel_2015_2024.csv.gz

```
panel sha256 3cb78c0aa33691037f275eaf7bea5a75d02958161e0f8f7f2318fae4b0cc879b
pairs passing the filters: 130488

=== requirement 0.085 x RWAJT (primary)
pairs starting in deficit: 462 of 130488; with 0 < b <= 1: 108
deficit pairs: mean g -0.0047, retention b 0.3131
surplus pairs: payout rate delta_p 0.0211 per quarter
T1 deficit level: 108 pairs, 32 banks
    phi- estimated +0.8809   phi- predicted +1.0102
    difference -0.1293, 95% interval [-0.3952, -0.0262]
    verdict: fails
T2 deficit falls in g: 108 pairs, 32 banks
    phi- low g +0.9275   phi- high g +0.7666
    difference +0.1609, 95% interval [-0.1521, +0.8592]
    verdict: not measured
T3 surplus level: 129787 pairs, 5144 banks
    phi+ estimated +0.9959   phi+ predicted +0.9789
    difference +0.0171, 95% interval [+0.0152, +0.0188]
    verdict: fails

=== requirement 0.06 x RWAJT (secondary)
pairs starting in deficit: 189 of 130488; with 0 < b <= 1: 24
deficit pairs: mean g -0.0100, retention b 1.0000
surplus pairs: payout rate delta_p 0.0135 per quarter
T1 deficit level: 24 pairs, 8 banks
    phi- estimated +1.0200   phi- predicted +1.0000
    difference +0.0200, 95% interval [-0.0792, +0.3431]
    verdict: secondary, no verdict
T2 deficit falls in g: 24 pairs, 8 banks
    phi- low g +0.9697   phi- high g +1.1268
    difference -0.1571, 95% interval [+nan, +nan]
    verdict: secondary, no verdict
T3 surplus level: 130060 pairs, 5151 banks
    phi+ estimated +0.9981   phi+ predicted +0.9865
    difference +0.0115, 95% interval [+0.0105, +0.0125]
    verdict: secondary, no verdict
```

## Choices the pre-registration did not fix

- In the secondary run, some bootstrap draws leave one half of the T2 split
  empty, so that interval is undefined (printed as nan). The secondary run
  carries no verdict.
- The diagnostic lines (deficit-pair counts, mean g, retention b, payout
  rate) were added to the output after the first run. They change no test
  and no verdict; the bootstrap draws are identical.

## Reading, beyond the verdict rule

- T1 fails. Deficits revert (estimated 0.88 a quarter) where the model
  predicts none (1.01). The prediction exceeds 1 because the mean growth of
  required capital in deficit is negative (-0.47% a quarter): banks in
  deficit shrink risk-weighted assets. The model takes required capital as
  exogenous, so it has no such channel.
- Only 108 of the 462 deficit pairs have a defined retention ratio in
  (0, 1]. The rest follow losses. The T1 sample is the profitable part of
  the deficit population.
- T3 fails. Surplus persistence is 0.9959 a quarter against 0.9789 from the
  payout rate alone, half-lives of about 170 and 33 quarters. Surpluses
  last far longer than payouts alone imply.
- T2 is not measured: the interval for the fall of persistence in g is too
  wide on 108 pairs.
