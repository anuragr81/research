#!/usr/bin/env bash
# Runs the suite of every literature record of the theory paper (theory/lit/).
# Each record checks its quotations against the pinned source PDF in
# ~/.cache/wealth_garch/ and its Lean file against theory/lean/.
set -u -o pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
FAILED=0
RAN=0

for d in "$HERE"/*/; do
  name="$(basename "$d")"
  [ "$name" = "__pycache__" ] && continue
  suite=$(find "$d" -maxdepth 1 -name 'verify_*.py' | head -1)
  if [ -z "$suite" ]; then
    echo "[FAIL] $name has no verify_*.py"
    FAILED=1
    continue
  fi
  echo "======================================================================"
  echo " $name"
  echo "======================================================================"
  if python3 "$suite"; then
    RAN=$((RAN + 1))
  else
    echo "[FAIL] $name suite reported failures"
    FAILED=1
  fi
  echo
done

if [ "$RAN" -eq 0 ]; then
  echo "[FAIL] no paper suites ran"
  FAILED=1
fi
echo "LITERATURE SUMMARY: $RAN record suites passed, failures: $FAILED"
exit "$FAILED"
