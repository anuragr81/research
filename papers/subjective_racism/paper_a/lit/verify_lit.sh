#!/usr/bin/env bash
# Runs every paper suite under lit/. Fails if a suite fails or if a paper
# directory has no verify_*.py.
set -u -o pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
FAILED=0; N=0
for d in "$HERE"/*/; do
  [ -d "$d" ] || continue
  N=$((N+1))
  s=$(ls "$d"verify_*.py 2>/dev/null | head -1)
  if [ -z "$s" ]; then
    echo "[FAIL] $(basename "$d") has no verify_*.py"; FAILED=1; continue
  fi
  python3 "$s" || FAILED=1
done
echo "paper records: $N"
[ "$FAILED" -eq 0 ] && echo "ALL PAPER SUITES PASSED" || echo "SOME PAPER SUITES FAILED"
exit "$FAILED"
