#!/usr/bin/env bash
# RUN_C3.sh -- smooth fit vs kink at x_L, by mesh refinement. Solves FRESH;
# reads no existing .mat or .csv.
#
#     bash RUN_C3.sh
#
# Writes c3_rows.csv (one line per completed mesh) and c3_output.txt.
# Send back c3_output.txt.
#
# EACH MESH RUNS IN ITS OWN OCTAVE INVOCATION and appends its row before the
# next starts, so a timeout at a large N keeps everything already computed.
# The previous version lost all but the first row this way -- it ran the
# whole sequence inside one invocation.
#
# TIMING, measured on your machine: N=301 at tol=1e-8 completed; N=601 did
# NOT finish inside 10 hours. Default here is therefore tol=1e-7 and a
# sequence that stops at 1201. TWO MESHES ARE ENOUGH for a first rate
# estimate; three make it convincing. Set PER_MESH_TIMEOUT to bound each
# solve independently.

set -u
LAMBDA=1.0
N_SEQUENCE=(301 601 1201)
TOL=1e-7
MAXITER=6000
PER_MESH_TIMEOUT=43200          # 12h per mesh; raise if 1201 needs it

OUT="c3_output.txt"
CSV="c3_rows.csv"
: > "$OUT"
exec > >(tee "$OUT") 2>&1

echo "=============================================================================="
echo "C3 -- SMOOTH FIT vs KINK AT x_L   (fresh solves, one octave run per mesh)"
echo "date (UTC): $(date -u '+%Y-%m-%d %H:%M:%S')"
echo "lambda_S=$LAMBDA  N=${N_SEQUENCE[*]}  tol=$TOL  maxiter=$MAXITER"
echo "=============================================================================="
echo
echo "Measuring: jump(h) = forward slope just above x_L, minus (1+kappa)."
echo "  jump -> 0     as h shrinks  =>  V' continuous: SMOOTH FIT HOLDS"
echo "  jump -> J > 0 as h shrinks  =>  genuine kink of size J"
echo
echo "A single mesh says little -- the forward difference straddles a boundary"
echo "that sits at a sub-cell position, so jump is noisy per-mesh. The trend"
echo "across meshes is the result."
echo

command -v octave >/dev/null 2>&1 || { echo "FATAL: octave not on PATH."; exit 1; }
[ -d 02_numerical ] || { echo "FATAL: run this from the repo root."; exit 1; }

echo "lambda_S,N,h,x_L,v_left,v_right,jump,overshoot,final_err,converged,seconds" > "$CSV"

cd 02_numerical
for N in "${N_SEQUENCE[@]}"; do
  echo "------------------------------------------------------------------------------"
  echo "N = $N   (started $(date -u '+%H:%M:%S') UTC)"
  echo "------------------------------------------------------------------------------"
  timeout "$PER_MESH_TIMEOUT" octave --no-gui --eval \
    "lambda_S_arg=${LAMBDA}; N_arg=${N}; tol_arg=${TOL}; maxiter_arg=${MAXITER}; run('C3_smooth_fit_refinement.m')"
  rc=$?
  if [ $rc -ne 0 ]; then
    echo
    echo "N=$N did not complete (exit $rc; 124 = hit PER_MESH_TIMEOUT)."
    echo "Rows already in $CSV remain valid. Continuing to next N is pointless"
    echo "if this one timed out -- larger N will only be slower. Stopping."
    break
  fi
done
cd ..

echo
echo "=============================================================================="
echo "RATE ANALYSIS"
echo "=============================================================================="
python3 - "$CSV" <<'PY'
import sys, csv
rows=[]
with open(sys.argv[1]) as f:
    for r in csv.DictReader(f):
        rows.append({k: float(v) if k not in ('N','converged') else int(float(v))
                     for k, v in r.items()})
if len(rows) < 2:
    print(f"  Only {len(rows)} mesh completed -- need at least 2 for a rate.")
    if rows:
        r = rows[0]
        print(f"  N={r['N']}  h={r['h']:.6f}  jump={r['jump']:.6f}  x_L={r['x_L']:.6f}")
    print("  Re-run with a larger PER_MESH_TIMEOUT, or a smaller N_SEQUENCE.")
    sys.exit(0)
print(f"  {'N':>6} {'h':>10} {'x_L':>10} {'jump':>12} {'jump ratio':>12} {'h ratio':>9}")
prev=None
for r in rows:
    if prev is None:
        print(f"  {r['N']:>6} {r['h']:>10.6f} {r['x_L']:>10.6f} {r['jump']:>12.6f} {'--':>12} {'--':>9}")
    else:
        jr = prev['jump']/r['jump'] if r['jump'] != 0 else float('inf')
        hr = prev['h']/r['h']
        print(f"  {r['N']:>6} {r['h']:>10.6f} {r['x_L']:>10.6f} {r['jump']:>12.6f} {jr:>12.2f} {hr:>9.2f}")
    prev=r
print()
first, last = rows[0], rows[-1]
shrink = first['jump']/last['jump'] if last['jump'] else float('inf')
htot   = first['h']/last['h']
print(f"  Over the whole sequence: h shrank {htot:.1f}x, jump shrank {shrink:.2f}x.")
print()
if shrink > 0.7*htot:
    print("  READING: jump is shrinking at roughly the rate of h.")
    print("  => consistent with SMOOTH FIT; the apparent kink is discretization.")
elif shrink < 1.5:
    print("  READING: jump is NOT shrinking materially as h falls.")
    print("  => consistent with a GENUINE KINK. SOC/BDR/VER's smooth-fit")
    print("     assumption needs revisiting, not just relabeling.")
else:
    print("  READING: jump shrinks, but slower than h. Inconclusive from these")
    print("  meshes -- could be a kink plus discretization, or a sub-linear rate.")
    print("  A third (finer) mesh would separate them.")
print()
worst = max(r['final_err'] for r in rows)
minjump = min(abs(r['jump']) for r in rows)
if worst > minjump/10:
    print(f"  WARNING: worst final_err ({worst:.2e}) is not well below the smallest")
    print(f"  jump ({minjump:.6f}). Tighten TOL before trusting the trend.")
if any(r['converged'] == 0 for r in rows):
    print("  WARNING: at least one mesh did not converge -- its row is unreliable.")
PY
echo
echo "Rows: c3_rows.csv    Report: c3_output.txt"
