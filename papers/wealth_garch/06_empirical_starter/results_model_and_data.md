# Results: the solved policy against the FDIC panel

Model side from the solve at lambda_S = 1; data side from the FDIC panel
2015-2024. Run 12 October 2026, reproducing HANDOVER_20261010.md section 3.

Pinned: ../03_empirical/results/resultM_lambda_1.0000_maxiter3000.mat sha256 e31605eda52ac9eeb26b77b428d66c190ba945ea38e959bdbb3ed58ffbea3dc1
Pinned: fdic_panel_2015_2024_ext.csv.gz sha256 124be6644c0261197c9b476658b4619a1ff1b1669af7b56591e671118bb9a1d7

Reproduce: python3 rederive_model_and_data.py ../03_empirical/results/resultM_lambda_1.0000_maxiter3000.mat fdic_panel_2015_2024_ext.csv.gz

```
=== MODEL SIDE (from the solve file) ===
  pi_cap unset (0x0)            : True
  trigger s                     : 1.030000   E/A 2.91%
  target  S                     : 1.390747   E/A 28.10%
  gap S-s                       : 0.360747
  pi* == pi_bar on              : 300/301 grid points   <- corner solution

=== DATA SIDE (from the panel) ===
  bank-quarters                 : 213,771
  x=A/L  p25/p50/p75            : 1.0997 / 1.1182 / 1.1447
  E/A    p25/p50/p75            : 9.06% / 10.57% / 12.64%
  frac at/below model trigger   : 0.81%
  frac at/above model target    : 2.30%
  realised sd(dx) quarterly     : 0.00734  (6,030 banks >=8q)
  realised annualised           : 0.01468  at x=1.11991

=== CORNER SOLUTION TEST (regulatory basis) ===
  measured pi = RWA/ASSET  p50  : 0.6857
  model pi_bar at x=1.1199     : 2.3794
  measured capital/RWA     p50  : 15.04%   = 3.34x a1=0.045
                           p25  : 12.52%   = 2.78x a1

=== SIGMA LEVEL ===
  model sigma at pi*=pi_bar     : 0.21249   = 14.5x measured
  model sigma at OBSERVED pi    : 0.06081   = 4.1x measured
```
