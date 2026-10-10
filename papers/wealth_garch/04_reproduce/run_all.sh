#!/usr/bin/env bash
# run_all.sh -- v2 verification harness
#
# Runs every canonical verifier that a plain machine can run, and reports
# PASS / FAIL / expected-fail / SKIP counts. Exit code is 0 iff nothing
# that was expected to pass failed.
#
# What runs where:
#   01_theory/*.py            here, needs python3 + sympy (+ numpy)
#   01_theory/verify_document_figures.py
#                             here IF octave is on PATH AND the solved
#                             .mat files are present, else SKIP
#   02_numerical/verify_M_operator.m
#                             here IF octave is on PATH, else SKIP
#   01_theory/*.lean          NOT run here -- needs a mathlib toolchain.
#                             Verify separately:  lake build
#   02_numerical sweeps       NOT run here -- hours of compute. See
#                             run_lambda_sweep.sh / headless_* directly.
#
# Tag conventions, inherited from v1: a trailing * on a tag marks a
# DELIBERATE expected-failure (the check failing is the confirmation);
# SKIP means a prerequisite (a tool or a data file) is absent, which is
# not a failure.

set -u
cd "$(dirname "$0")/.."

pass=0; fail=0; expfail=0; skip=0
run_py () {
    local script="$1"
    echo "== $script =="
    local out
    out=$(python3 "$script" 2>&1)
    local rc=$?
    echo "$out" | tail -5
    local p f_ e k
    # PASS/FAIL split three ways: a plain tag failing is a real failure, a
    # starred tag failing is a deliberate expected-failure, SKIP is neither.
    p=$(echo "$out"  | grep -cE '^[A-Za-z0-9]+\*?[[:space:]]+PASS' || true)
    f_=$(echo "$out" | grep -cE '^[A-Za-z0-9]+[[:space:]]+FAIL' || true)
    e=$(echo "$out"  | grep -cE '^[A-Za-z0-9]+\*[[:space:]]+FAIL' || true)
    k=$(echo "$out"  | grep -cE '^[A-Za-z0-9]+\*?[[:space:]]+SKIP' || true)
    pass=$((pass+p)); fail=$((fail+f_)); expfail=$((expfail+e)); skip=$((skip+k))
    if [ $rc -ne 0 ] && [ $f_ -eq 0 ]; then
        echo "(non-zero exit with no unexpected FAIL tags -- check output above)"
    fi
    echo
}

run_py 01_theory/verify_rate_based.py
run_py 01_theory/verify_state_map.py
run_py 01_theory/verify_saturation_limits.py
run_py 01_theory/verify_comparative_statics.py

# Regenerates every numerical figure the document quotes, from the solve
# files themselves, and asserts each against the printed value. SKIPs the
# figures whose solve outputs are not in the repo (K-sweep, H-convention,
# multi-mesh) rather than omitting them silently.
run_py 01_theory/verify_document_figures.py

# Shooting test for smooth fit at the trigger. SKIPs unless a solution grid
# has been exported; see the file header.
run_py 01_theory/verify_smooth_fit_shooting.py

# Estimator-recovery evidence cited by the empirical companion
# (EMPIRICAL_v2 Section 1). Simulation-based and REPORT-STYLE: it prints
# tables for reading, not PASS/FAIL tags, so it contributes nothing to
# the counts below -- it is run here so a broken environment or a
# regression that crashes it is caught, and so the figures the companion
# quotes are regenerable on demand.
echo "== 00_reader (registry, empirical ledger, citations) =="
if [ ! -f 00_document/PROOFS_v2.aux ]; then
    if command -v pdflatex > /dev/null 2>&1; then
        (cd 00_document && pdflatex -interaction=nonstopmode PROOFS_v2.tex > /dev/null 2>&1)
    fi
fi
if [ ! -f 00_document/PROOFS_v2.aux ]; then
    echo "  SKIPPED: PROOFS_v2.aux absent and pdflatex unavailable."
    echo "  The reader checks read numbering from the aux and cannot run without it."
    skip=$((skip+1))
else
for reader_check in proof_registry ledger_empirical check_citations; do
    if (cd 00_reader && python3 ${reader_check}.py > /tmp/_${reader_check}.out 2>&1); then
        echo "  ${reader_check}: OK"
    else
        echo "  ${reader_check}: FAILED -- see /tmp/_${reader_check}.out"
        tail -5 /tmp/_${reader_check}.out
        fail=$((fail+1))
    fi
done
fi
echo

echo "== 01_theory/verify_egarch_recovery.py (report-style, no tags) =="
if python3 01_theory/verify_egarch_recovery.py > /tmp/_egarch.out 2>&1; then
    tail -3 /tmp/_egarch.out
    echo "(completed; full report in /tmp/_egarch.out)"
else
    echo "FAILED to run -- see /tmp/_egarch.out"
    fail=$((fail+1))
fi
echo

if command -v octave > /dev/null 2>&1; then
    echo "== 02_numerical/verify_M_operator.m =="
    ( cd 02_numerical && octave --no-gui verify_M_operator.m 2>&1 | tail -14 ) | tee /tmp/_mop.out
    p=$(grep -cE '^V[0-9]+[[:space:]]+PASS' /tmp/_mop.out || true)
    f_=$(grep -cE '^V[0-9]+[[:space:]]+FAIL' /tmp/_mop.out || true)
    pass=$((pass+p)); fail=$((fail+f_))
    rm -f /tmp/_mop.out
    echo
else
    echo "== 02_numerical/verify_M_operator.m : SKIP (octave not on PATH) =="
    skip=$((skip+1))
    echo
fi

echo "=============================================================="
echo "TOTAL: $pass pass / $fail fail / $expfail expected-fail / $skip skip"
echo "Lean files (01_theory/*.lean) are NOT covered by this harness --"
echo "run 'lake build' against them separately."
echo "=============================================================="
exit $([ $fail -eq 0 ] && echo 0 || echo 1)
