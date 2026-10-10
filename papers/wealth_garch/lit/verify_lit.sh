#!/usr/bin/env bash
set -u -o pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
FAILED=0
RAN=0

for d in "$HERE"/*/; do
  name="$(basename "$d")"
  [ "$name" = "pdfs" ] && continue
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

echo "papers verified: $RAN"
if [ "$FAILED" -eq 0 ]; then
  echo "ALL LITERATURE SUITES COMPLETED"
else
  echo "SOME LITERATURE SUITES REPORTED FAILURES"
fi
exit "$FAILED"
