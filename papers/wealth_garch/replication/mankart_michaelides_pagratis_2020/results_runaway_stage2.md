# Mankart, Michaelides and Pagratis stage 2: the unbounded equity region

Computed 17 October 2026 as an illustration,
run under PREREG_stage2_7.md,
and under PREREG_stage2_6.md for the grid candidate it summarises.

## Findings

- Grid candidate e_top 1.0 (select_e1_out.txt): large banks have
  6,256,639 non-finite bank-quarters out of 20,000,000 and 133,861 above
  the top of the grid; small banks have none of either, and their largest
  equity is 0.570.
- Break-even, below. If equity is held in liquid assets, one more unit
  of e earns (1 - tau) r_S plus the marginal funding saving (1 - tau) 2
  phi_E e, against the shareholders' required return 1/beta - 1. The two
  are equal at e of 0.177 (large banks, expansion) and 0.206 (recession),
  and at 0.884 and 1.113 for small banks. Their Table 5 implies e of
  0.102 (large) and 0.075 (small).
- The calculation ignores deposit growth, risk, the capital constraints
  and the value of new loans; it shows where retaining equity starts to
  beat paying it out, not where simulated banks go.

Pinned: mmp_config.py sha256 06cc133c320e1bb8c5156be77776c6e320ee960372e7ead0f1160a4a2af60c29
Pinned: mmp_model.py sha256 aa51c30d5bc933ff2491cfec74db19884a5c60a312c6d66fc91611d049112752
Pinned: breakeven.py sha256 b390da01c8642154d2d7129d87fe7be1b1e34708718788e08d0885839b1a6acd

Reproduce: python3 breakeven.py

```
large expansion: phi_E 0.0705, beta 0.9753, r_S 0.0049, 1/beta - 1 0.0253, break-even e 0.177
large recession: phi_E 0.0705, beta 0.9753, r_S 0.0008, 1/beta - 1 0.0253, break-even e 0.206
small expansion: phi_E 0.0072, beta 0.9859, r_S 0.0041, 1/beta - 1 0.0143, break-even e 0.884
small recession: phi_E 0.0072, beta 0.9859, r_S 0.0008, 1/beta - 1 0.0143, break-even e 1.113
large: their Table 5 equity/assets 0.065 over deposits/assets 0.638 gives e 0.102
small: their Table 5 equity/assets 0.067 over deposits/assets 0.891 gives e 0.075
```
