%% Convergence check for the M (additive-cost) operator: resolve ONE lambda_S at a higher iteration cap and
%% diff against the existing max_iter=1500 result, if present.
%% Usage: octave --no-gui --eval "lambda_S_arg=1.0; run('headless_lambda_converge_check_M.m')"
%% Optional: set max_iter_arg before calling (default 3000).
%% Does NOT overwrite the original result_lambda_<val>.mat/.csv -- writes
%% to result_lambda_<val>_maxiter<N>.mat/.csv instead, so both exist
%% side by side for inspection.
if ~exist('lambda_S_arg', 'var')
    error('set lambda_S_arg before calling, e.g. lambda_S_arg=1.0');
end
if ~exist('max_iter_arg', 'var')
    max_iter_arg = 3000;
end

params.r = 0.025; params.mu = 0.04; params.mu_L = 0.03; params.rho = 0.12;
params.sigma = 0.08; params.sigma_L = 0.03; params.c = 0.20; params.gamma = 0.02;
params.a1 = 0.045; params.a2 = 0.05; params.a3 = 0.30;
params.kappa = 0.01; params.kappa_p = 0.02; params.pi_cap = [];
params.R_ref = 1.15;
params.K = 0.02;
params.lambda_S = lambda_S_arg;
check_params_valid(params);

settings.y_max = 2.5; settings.N = 301; settings.dt = 0.05;
settings.max_iter = max_iter_arg; settings.tol = 1e-6; settings.pi_grid_size = 151;

sol_new = solve_bank_qvi_lambda_M(params, settings, 'QVI');

y = sol_new.y; v_new = sol_new.v; dy = y(2)-y(1);
target = 1 + params.kappa;
vp = sol_new.v_prime;
y_post_new = NaN;
for i = 2:length(y)-1
    if vp(i) > target && vp(i+1) <= target
        y_post_new = interp1(vp(i:i+1), y(i:i+1), target);
        break;
    end
end
active = abs(v_new - sol_new.Hv) < 1e-9;
recap_edge_new = NaN;
if any(active); recap_edge_new = max(y(active)); end

fname_mat = sprintf('resultM_lambda_%.4f_maxiter%d.mat', lambda_S_arg, max_iter_arg);
fname_csv = sprintf('resultM_lambda_%.4f_maxiter%d.csv', lambda_S_arg, max_iter_arg);
save(fname_mat, 'sol_new', 'params', 'settings', 'y_post_new', 'recap_edge_new');
fid = fopen(fname_csv, 'w');
fprintf(fid, 'lambda_S,max_iter,y_star,y_post,v1,recap_edge,iterations,final_err,converged\n');
fprintf(fid, '%.6f,%d,%.6f,%.6f,%.6f,%.6f,%d,%.6e,%d\n', ...
        lambda_S_arg, max_iter_arg, sol_new.y_star, y_post_new, v_new(1), ...
        recap_edge_new, sol_new.iterations, sol_new.final_err, sol_new.converged);
fclose(fid);

printf("NEW RUN[M]  lambda_S=%.4f  max_iter=%d  y_star=%.6f  v1=%.6f  recap_edge=%.6f  iters=%d  final_err=%.3e  converged=%d\n", ...
       lambda_S_arg, max_iter_arg, sol_new.y_star, v_new(1), recap_edge_new, ...
       sol_new.iterations, sol_new.final_err, sol_new.converged);

old_fname = sprintf('resultM_lambda_%.4f.mat', lambda_S_arg);
if exist(old_fname, 'file')
    S_old = load(old_fname);
    sol_old = S_old.sol;
    y_star_old = sol_old.y_star; v1_old = sol_old.v(1);
    recap_edge_old = S_old.recap_edge;
    if isfield(S_old, 'y_post'); y_post_old = S_old.y_post; else; y_post_old = NaN; end

    d_ystar = sol_new.y_star - y_star_old;
    d_v1 = v_new(1) - v1_old;
    d_recap = recap_edge_new - recap_edge_old;
    d_ypost = y_post_new - y_post_old;

    printf("\nCOMPARISON vs existing %s (max_iter=%d, final_err=%.3e, converged=%d):\n", ...
           old_fname, sol_old.iterations, sol_old.final_err, sol_old.converged);
    printf("  delta y_star     = %+.6e\n", d_ystar);
    printf("  delta v(1)       = %+.6e\n", d_v1);
    printf("  delta recap_edge = %+.6e\n", d_recap);
    printf("  delta y_post     = %+.6e\n", d_ypost);

    thresh = 1e-4;
    if abs(d_ystar) < thresh && abs(d_v1) < thresh && abs(d_recap) < thresh
        printf("  -> ALL deltas below %.0e: the max_iter=1500 result is STABLE, additional iterations did not move it.\n", thresh);
    else
        printf("  -> AT LEAST ONE delta exceeds %.0e: the max_iter=1500 result had NOT settled. Treat those numbers as provisional.\n", thresh);
    end
else
    printf("\nNo existing %s found to compare against -- this is a standalone run.\n", old_fname);
end
