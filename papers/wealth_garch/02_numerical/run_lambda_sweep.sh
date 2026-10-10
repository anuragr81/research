#!/usr/bin/env bash
# run_lambda_sweep.sh
#
# Generates resultM_lambda_{lambda_S}.mat for the six lambda_S values already
# reported in PROOFS_v2.tex Remark 3 (rem:compstat), by calling
# headless_lambda_single_M.m once per value.
#
# Confirmed dependency set for headless_lambda_single_M.m (verified by
# running it end-to-end in a sandbox on 9 Aug 2026 -- this is the complete
# closure, not a partial trace):
#   check_params_valid.m        compute_M_operator.m
#   compute_pi_bar.m            compute_y_star_analytical.m
#   compute_y_star_from_slope.m drift_and_sigma2.m
#   enforce_slope_floor.m       safe_max.m
#   solve_bank_qvi_lambda_M.m   thomas_solve.m
# All ten are included in this bundle -- the benchmark's upstream repo
# (github.com/yuqiongwang/bank_capital_structure) ships only 7 of them
# (check_params_valid, compute_pi_bar, compute_y_star_analytical,
# compute_y_star_from_slope, drift_and_sigma2, enforce_slope_floor,
# thomas_solve); compute_M_operator, safe_max, solve_bank_qvi_lambda_M and
# this script itself are the project's own additions on top of it.
#
# Run from the repo root (the same directory as solve_bank_qvi.m and the
# other benchmark files). Each solve is slow at full resolution
# (N=301, pi_grid_size=151, max_iter=1500) -- tens of minutes each is
# plausible; that is why this was not run in the sandbox. Six solves,
# so budget the better part of a session.
#
# Usage:  bash run_lambda_sweep.sh

set -euo pipefail

REQUIRED="check_params_valid.m compute_M_operator.m compute_pi_bar.m \
compute_y_star_analytical.m compute_y_star_from_slope.m drift_and_sigma2.m \
enforce_slope_floor.m safe_max.m solve_bank_qvi_lambda_M.m thomas_solve.m \
headless_lambda_single_M.m"

for f in $REQUIRED; do
    if [ ! -f "$f" ]; then
        echo "MISSING: $f -- run this script from the repo root (the same directory as solve_bank_qvi.m)." >&2
        exit 1
    fi
done

LAMBDAS="1.00 1.10 1.25 1.50 1.75 2.00"

for L in $LAMBDAS; do
    OUT="resultM_lambda_${L}00.mat"
    if [ -f "$OUT" ] || [ -f "resultM_lambda_${L}.mat" ]; then
        echo "== lambda_S=$L : output already present, skipping =="
        continue
    fi
    echo "== lambda_S=$L : solving (this is the slow step) =="
    octave --no-gui --eval "lambda_S_arg=${L}; run('headless_lambda_single_M.m')"
done

echo
echo "Done. Expect one resultM_lambda_*.mat per lambda_S value:"
ls -la resultM_lambda_*.mat 2>/dev/null || echo "  (none found -- check the log above for errors)"
