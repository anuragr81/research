#!/usr/bin/env bash
# run_convergence_recert.sh
#
# Re-solves each lambda_S at max_iter=3000 (double the original 1500) via
# headless_lambda_converge_check_M.m, and lets that script's own comparison
# report whether the max_iter=1500 results (all six of which came back
# converged=0 -- see 03_empirical/results/resultM_lambda_*.csv) had
# actually settled. Does NOT touch the original resultM_lambda_*.mat/.csv
# files -- writes resultM_lambda_<L>_maxiter3000.mat/.csv alongside them.
#
# Run from 02_numerical/, with the six resultM_lambda_*.mat files from
# 03_empirical/results/ copied in first (needed as the baseline to diff
# against):
#     cp ../03_empirical/results/resultM_lambda_*.mat .
#     bash run_convergence_recert.sh
#
# By default re-certifies all six lambda_S. Pass a subset to check only
# some of them, e.g. for a cheap spot-check of the extremes:
#     bash run_convergence_recert.sh 1.00 2.00
#
# WARNING on cost: this is the same solve as run_lambda_sweep.sh, just at
# double max_iter, so budget similarly -- roughly the time the original
# sweep took, again, for however many lambda_S you re-run.

set -euo pipefail

REQUIRED="check_params_valid.m compute_M_operator.m compute_pi_bar.m \
compute_y_star_analytical.m compute_y_star_from_slope.m drift_and_sigma2.m \
enforce_slope_floor.m safe_max.m solve_bank_qvi_lambda_M.m thomas_solve.m \
headless_lambda_converge_check_M.m"

for f in $REQUIRED; do
    if [ ! -f "$f" ]; then
        echo "MISSING: $f -- run this script from 02_numerical/ (or wherever these files live)." >&2
        exit 1
    fi
done

if [ "$#" -gt 0 ]; then
    LAMBDAS="$*"
else
    LAMBDAS="1.00 1.10 1.25 1.50 1.75 2.00"
fi

SUMMARY="convergence_recert_summary.txt"
: > "$SUMMARY"

for L in $LAMBDAS; do
    BASELINE="resultM_lambda_${L}00.mat"
    if [ ! -f "$BASELINE" ]; then
        echo "SKIP lambda_S=$L : $BASELINE not found (copy it from 03_empirical/results/ first)"
        continue
    fi
    echo "== lambda_S=$L : re-solving at max_iter=3000 (slow -- same cost as the original solve) =="
    octave --no-gui --eval "lambda_S_arg=${L}; max_iter_arg=3000; run('headless_lambda_converge_check_M.m')" \
        2>&1 | tee -a "$SUMMARY"
    echo
done

echo
echo "Done. Full output, including every PASS/-> verdict line, is in $SUMMARY"
echo "--- verdict lines only ---"
grep -E "STABLE|had NOT settled|standalone run" "$SUMMARY" || echo "  (none found -- check $SUMMARY for errors)"
