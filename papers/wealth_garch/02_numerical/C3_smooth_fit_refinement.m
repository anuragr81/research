%% C3 -- SMOOTH FIT vs KINK AT x_L. ONE MESH PER INVOCATION.
%%
%% Solves fresh at a single N and appends one row to c3_rows.csv, so a
%% timeout at a large N never loses the rows already computed. RUN_C3.sh
%% drives the sequence and computes rates from whatever rows exist.
%%
%% WHAT IS MEASURED, and why this differs from the first version.
%%
%% The trigger x_L is the LAST grid point where v == Mv (the value is still
%% on the affine intervention piece). This matches recap_edge as defined
%% everywhere else in this repo (headless_M_refine.m, headless_K_single.m,
%% headless_lambda_single_M.m) and the recap_edge_new stored in the .mat
%% files.
%%
%% The first version of this script also tracked a "plateau point" -- the
%% last point where the CENTRAL-difference v_prime still equalled 1+kappa --
%% and reported the gap between the two as though it were a live question of
%% convention. It is not. At the last active point the value is exactly on
%% the affine piece and the BACKWARD difference is exactly 1+kappa; only the
%% central difference departs, because its stencil reaches forward across the
%% boundary. That one-cell gap was a stencil artifact, not an ambiguity, and
%% measuring it answered nothing.
%%
%% The real question is the RIGHT-hand derivative. Define
%%
%%     jump(h) := [v(i+1) - v(i)]/h  -  (1 + kappa)      at i = index of x_L
%%
%% i.e. how far the forward slope immediately above the trigger sits above
%% the intervention slope. Then:
%%
%%   jump(h) -> 0        => V' is continuous at x_L: SMOOTH FIT HOLDS.
%%   jump(h) -> J > 0    => genuine convex kink of size J; the smooth-fit
%%                          assumption in SOC/BDR/VER needs revisiting.
%%
%% CAUTION on reading a single mesh: the forward difference straddles the
%% true boundary, which falls at a sub-cell position varying with the
%% parameters, so jump(h) at ONE mesh is noisy and non-monotone across
%% lambda_S. Only its behaviour as h -> 0 is informative.
%%
%% Also recorded: v_left (must be exactly 1+kappa -- a sanity check on the
%% trigger detection, not a result), and the sub-cell overshoot
%% (v(i+1)-Mv(i+1))/h, which indicates how far past the true boundary the
%% first inactive node sits.
%%
%% Usage:
%%   octave --no-gui --eval "lambda_S_arg=1.0; N_arg=301; run('C3_smooth_fit_refinement.m')"

if ~exist('lambda_S_arg','var'); error('set lambda_S_arg, e.g. 1.0'); end
if ~exist('N_arg','var'); error('set N_arg, e.g. 301'); end
if ~exist('tol_arg','var'); tol_arg = 1e-7; end
if ~exist('maxiter_arg','var'); maxiter_arg = 6000; end
if ~exist('csv_arg','var'); csv_arg = '../c3_rows.csv'; end

params.r=0.025; params.mu=0.04; params.mu_L=0.03; params.rho=0.12;
params.sigma=0.08; params.sigma_L=0.03; params.c=0.20; params.gamma=0.02;
params.a1=0.045; params.a2=0.05; params.a3=0.30;
params.kappa=0.01; params.kappa_p=0.02; params.pi_cap=[];
params.R_ref=1.15; params.K=0.02; params.lambda_S=lambda_S_arg;
check_params_valid(params);

settings.y_max=2.5; settings.N=N_arg; settings.dt=0.05;
settings.max_iter=maxiter_arg; settings.tol=tol_arg; settings.pi_grid_size=151;

t0 = tic;
sol = solve_bank_qvi_lambda_M(params, settings, 'QVI');
elapsed = toc(t0);

y=sol.y; v=sol.v; dy=y(2)-y(1); N=length(y); Mv=sol.Mv;

active = abs(v - Mv) < 1e-9;
if ~any(active)
    error('C3: no active (intervention) region found at N=%d -- cannot locate x_L', N_arg);
end
i = find(active, 1, 'last');
if i < 2 || i >= N
    error('C3: trigger at a domain endpoint (i=%d of %d) -- mesh or domain wrong', i, N);
end

x_L     = y(i);
v_left  = (v(i)   - v(i-1)) / dy;
v_right = (v(i+1) - v(i))   / dy;
jump    = v_right - (1 + params.kappa);
oversh  = (v(i+1) - Mv(i+1)) / dy;

printf('\n');
printf('C3 row: lambda_S=%.4f  N=%d  h=%.6f\n', lambda_S_arg, N_arg, dy);
printf('  x_L (last active)      = %.6f\n', x_L);
printf('  V left  (backward)     = %.6f   [must equal 1+kappa = %.4f]\n', v_left, 1+params.kappa);
printf('  V right (forward)      = %.6f\n', v_right);
printf('  jump = right-(1+kappa) = %.6f   <-- the quantity that decides it\n', jump);
printf('  sub-cell overshoot     = %.6f\n', oversh);
printf('  final_err              = %.3e   converged=%d\n', sol.final_err, sol.converged);
printf('  solve time             = %.1f s\n', elapsed);

if abs(v_left - (1+params.kappa)) > 1e-6
    printf('  WARNING: left derivative is not 1+kappa. Trigger detection is wrong;\n');
    printf('  do not use this row.\n');
end

%% Save the solution arrays. The shooting test
%% (01_theory/verify_smooth_fit_shooting.py) needs y, v and Mv; without them
%% a 5-6 hour solve cannot be re-examined without re-running it.
outmat = sprintf('../c3_solution_lambda%.4f_N%d.mat', lambda_S_arg, N_arg);
sol_c3 = struct('y', y, 'v', v, 'Mv', Mv, 'v_prime', sol.v_prime, ...
                'v_second', sol.v_second, 'pi_star', sol.pi_star, ...
                'pi_max', sol.pi_max, 'Lam', sol.Lam, 'y_star', sol.y_star, ...
                'final_err', sol.final_err, 'converged', sol.converged);
params_c3 = params; settings_c3 = settings;
save('-text', outmat, 'sol_c3', 'params_c3', 'settings_c3');
printf('  solution saved to %s\n', outmat);

fid = fopen(csv_arg, 'a');
if fid > 0
    fprintf(fid, '%.4f,%d,%.8f,%.8f,%.8f,%.8f,%.8f,%.8f,%.3e,%d,%.1f\n', ...
            lambda_S_arg, N_arg, dy, x_L, v_left, v_right, jump, oversh, ...
            sol.final_err, sol.converged, elapsed);
    fclose(fid);
    printf('  appended to %s\n', csv_arg);
else
    printf('  WARNING: could not append to %s\n', csv_arg);
end
