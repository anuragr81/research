# Mankart, Michaelides and Pagratis stage 3: where the faster solver's solutions differ

Run 18-19 October 2026 (about 5.7 hours) as the systemd unit mmp-visits,
with the code of commit 4250d755,
run under PREREG_stage3_2.md.
Stage 3 was deferred before this result (PREREG_stage3_3.md), so no
decision on stage 3 depends on it.

## Findings

- The three solves reproduce results_validation_fast.md exactly: 50, 190
  and 194 sweeps.
- Of the 50,430 cells, 6,263 (12.42%) are visited by the simulated banks
  of either perturbed solution, as corners of an interpolation in any of
  the 2,000 quarters.
- Over visited cells the warm and cold solutions differ by at most
  2.088e-05 of the value range; over cells not visited, by up to 9.429e-02
  (the difference F4 of results_validation_fast.md found). No visit, of
  160,000,000 corner-visits, lands on a cell that differs by more than
  1e-4.
- Under the rule of PREREG_stage3_2.md the faster solver is adopted for
  stage 3: the solutions disagree only where no simulated bank goes,
  which fits the agreement of the moments in F3 and F4.
- solved_fast_small_41.npz is the small-bank solution that
  comparison/results_mmp.md uses.

## Limits

- One parameter perturbation and one size class (small banks).
- Visited means visited in this simulation, with its seed; a different
  parameter vector visits different cells.

Pinned: mmp_model.py sha256 aa51c30d5bc933ff2491cfec74db19884a5c60a312c6d66fc91611d049112752
Pinned: mmp_config.py sha256 06cc133c320e1bb8c5156be77776c6e320ee960372e7ead0f1160a4a2af60c29
Pinned: mmp_a1.py sha256 4a6f85c1696af0a712920b6a31a5bc91221a73546e168f1cfc00ad675a6e2bf6
Pinned: mmp_fast.py sha256 26d33708a8b36cb98d50ecc939ec7239d22d16b0c72b421304cb2ac33c607a16
Pinned: stage2.py sha256 b6be175625a9ce996d838975d872a1f4d01840725f6fa8c02028e299fab4aac6
Pinned: validate_fast.py sha256 04b2d1a89a04759fecc950e95ec00e0ada5bfebec1b736d2e79a76c41b75ded4
Pinned: check_visits.py sha256 737e7f039c573c48b657e4e89b89fd189b11d8aadfe853ad5f15d8ac28b2fb05
Pinned: solved_fast_small_41.npz sha256 63393d88102e995e880005d8324ffe4da16ad30cac0297a8a146a5d49d5d4bf1
Pinned: solved_fast_small_41_p_warm.npz sha256 22b0d9bf48bb6e9218c12ae7873a6c89156c1a70b2fed6f283396cd1014696ae
Pinned: solved_fast_small_41_p_cold.npz sha256 41ccc06a6bc41567bba3d00b0cf73bccdc5008fc2dfcbeadebe187794a2937d2

Reproduce: python3 check_visits.py

```
solved_fast_small_41.npz: converged after 50 sweeps (8 full), last change 7.1e-02
solved_fast_small_41_p_warm.npz: converged after 190 sweeps (22 full), last change 3.6e-02
solved_fast_small_41_p_cold.npz: converged after 194 sweeps (23 full), last change 0.0e+00
cells 50430, visited 6263 (12.42%)
largest difference over visited cells 2.088e-05, over cells not visited 9.429e-02 of the value range
visits to cells with a difference above 1e-4: 0 of 160000000 corner-visits (0.0000%)
visited cells with a difference above 1e-4: 0
rule: adopted
```
