#!/usr/bin/env bash
# DIAGNOSE_OCTAVE.sh -- run ONLY if verify_document_figures.py still reports
# "octave and .mat files BOTH present but unreadable".
#
# Runs the smallest possible read against one .mat file, one step at a time,
# and shows the raw Octave error instead of swallowing it.
#
#     bash DIAGNOSE_OCTAVE.sh
#
# Send back everything it prints.

set -u
MATDIR="${MATDATA_DIR:-03_empirical/results}"
F="$MATDIR/resultM_lambda_1.0000_maxiter3000.mat"

echo "octave: $(octave --version 2>&1 | head -1)"
echo "file  : $F"
echo "exists: $([ -f "$F" ] && echo yes || echo NO)"
echo "size  : $(wc -c < "$F" 2>/dev/null || echo n/a) bytes"
echo "head  : $(head -c 120 "$F" 2>/dev/null | tr '\n' ' ')"
echo

echo "--- step 1: can octave load the file at all? ---"
octave --no-gui --eval "S = load('$F'); disp(fieldnames(S));" 2>&1
echo

echo "--- step 2: are sol_new / recap_edge_new / y_post_new present? ---"
octave --no-gui --eval "
S = load('$F');
printf('has sol_new       : %d\n', isfield(S,'sol_new'));
printf('has recap_edge_new: %d\n', isfield(S,'recap_edge_new'));
printf('has y_post_new    : %d\n', isfield(S,'y_post_new'));
printf('has params        : %d\n', isfield(S,'params'));
if isfield(S,'sol_new'); disp(fieldnames(S.sol_new)); end
" 2>&1
echo

echo "--- step 3: the exact printf the verifier uses ---"
octave --no-gui --eval "
S = load('$F');
sol = S.sol_new; p = S.params;
printf('SCALARS %.10f %.10f %.10f %d %d %.10f %.10f %.10f %.10f %.10f %.10f %.10f %.10f\n', ...
  sol.y_star, S.recap_edge_new, S.y_post_new, sol.converged, sol.iterations, ...
  p.rho, p.mu_L, p.kappa, p.K, p.R_ref, p.lambda_S, p.a1, p.a3);
printf('ROW %.12g %.12g %.12g %.12g %.12g %.12g %.12g\n', ...
  sol.y(1), sol.v(1), sol.v_prime(1), sol.v_second(1), ...
  sol.pi_star(1), sol.pi_max(1), sol.Lam(1));
printf('DONE\n');
" 2>&1
echo
echo "--- end ---"
