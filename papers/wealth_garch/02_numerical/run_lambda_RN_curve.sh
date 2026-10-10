#!/usr/bin/env bash
# run_lambda_RN_curve.sh
#
# Produces the lambda_V(lambda_S) curve that PROOFS_v2.tex Section "What is
# not established" flags as missing: whether an ESTIMATED lambda_V varies
# with lambda_S, and in which direction. As shipped, lambda_RN_panelconv.m
# hardcodes a single input, resultM_lambda_1.0000.mat, so it only ever
# computes the lambda_S=1 point. This script does NOT modify that file --
# per the project's one-canonical-routine rule, it stays exactly as
# verified. Instead, for each lambda_S it makes a throwaway copy with only
# the load-line's filename changed (confirmed to be the only line that
# differs -- see the diff this script prints for lambda_S=1.00, which
# should be empty), runs that copy, and archives the output.
#
# PREREQUISITE: run_lambda_sweep.sh (or an equivalent manual run of
# headless_lambda_single_M.m) must already have produced
# resultM_lambda_{1.0000,1.1000,1.2500,1.5000,1.7500,2.0000}.mat in the
# current directory.
#
# Each call runs a Monte Carlo simulation (600,000 steps in the script as
# written) so this is slow per point, independent of the solve time already
# spent generating the .mat files.
#
# Usage:  bash run_lambda_RN_curve.sh
# Run from the repo root, alongside lambda_RN_panelconv.m and the
# resultM_lambda_*.mat files that run_lambda_sweep.sh produced there.

set -euo pipefail

SRC="lambda_RN_panelconv.m"
if [ ! -f "$SRC" ]; then
    echo "MISSING: $SRC -- run this script from the repo root (same directory as lambda_RN_panelconv.m)." >&2
    exit 1
fi

LAMBDAS="1.00 1.10 1.25 1.50 1.75 2.00"
OUTDIR="lambda_RN_curve_$(date +%Y%m%d)"
mkdir -p "$OUTDIR"
CSV="$OUTDIR/lambda_V_curve.csv"
echo "lambda_S,convention,ratio,lambda_RN,split_x" > "$CSV"

echo "Sanity check -- diff for lambda_S=1.00 should be EMPTY (same file):"
sed "s/resultM_lambda_1\.0000\.mat/resultM_lambda_1.0000.mat/" "$SRC" > "$OUTDIR/_check.m"
diff "$SRC" "$OUTDIR/_check.m" && echo "  (empty, as expected)"
rm -f "$OUTDIR/_check.m"
echo

for L in $LAMBDAS; do
    MATFILE="resultM_lambda_${L}00.mat"
    if [ ! -f "$MATFILE" ]; then
        echo "SKIP lambda_S=$L : $MATFILE not found (run run_lambda_sweep.sh first)"
        continue
    fi
    RUNFILE="$OUTDIR/lambda_RN_at_${L}.m"
    sed "s/resultM_lambda_1\.0000\.mat/${MATFILE}/" "$SRC" > "$RUNFILE"

    # Confirm the substitution touched exactly the load line and nothing else.
    # Expect exactly 2 changed lines (one removed, one added) -- or 0 for
    # lambda_S=1.00, where the substitution is a no-op because $SRC already
    # loads resultM_lambda_1.0000.mat. Anything else means the sed hit
    # something it should not have, so refuse to run it.
    NDIFF=$(diff "$SRC" "$RUNFILE" | grep -c '^[<>]' || true)
    if [ "$NDIFF" -ne 2 ] && [ "$NDIFF" -ne 0 ]; then
        echo "ABORT lambda_S=$L : substitution changed $NDIFF lines, expected 2 (or 0 for the no-op case). Not running an unverified script." >&2
        exit 1
    fi
    if [ "$NDIFF" -eq 0 ] && [ "$MATFILE" != "resultM_lambda_1.0000.mat" ]; then
        echo "ABORT lambda_S=$L : no-op substitution but target is $MATFILE, not resultM_lambda_1.0000.mat." >&2
        exit 1
    fi

    echo "== lambda_S=$L : running lambda_RN_panelconv.m against $MATFILE (slow -- Monte Carlo) =="
    LOG="$OUTDIR/lambda_RN_at_${L}.log"
    octave --no-gui "$RUNFILE" > "$LOG" 2>&1 || { echo "FAILED, see $LOG"; continue; }

    grep -E "^\((a|b)\)" "$LOG" | awk -v ls="$L" '{
        n=NF; printf "%s,%s,%s,%s,%s\n", ls, $1, $(n-2), $(n-1), $n
    }' >> "$CSV"
done

echo
echo "Done. Curve written to $CSV"
echo "Full logs, one per lambda_S, are in $OUTDIR/"
cat "$CSV"
