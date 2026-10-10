%% Run ONE lambda_S value, write result to its own .mat file.
%% Usage: octave --no-gui --eval "lambda_S_arg=1.5; run('headless_lambda_single.m')"
if ~exist('lambda_S_arg', 'var')
    error('set lambda_S_arg before calling, e.g. lambda_S_arg=1.5');
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
settings.max_iter = 1500; settings.tol = 1e-6; settings.pi_grid_size = 151;

sol = solve_bank_qvi_lambda_M(params, settings, 'QVI');

y = sol.y; v = sol.v; dy = y(2)-y(1);
target = 1 + params.kappa;
vp = sol.v_prime;
y_post = NaN;
for i = 2:length(y)-1
    if vp(i) > target && vp(i+1) <= target
        y_post = interp1(vp(i:i+1), y(i:i+1), target);
        break;
    end
end
active = abs(v - sol.Hv) < 1e-9;
recap_edge = NaN;
if any(active); recap_edge = max(y(active)); end

band = 5*dy;

if isnan(recap_edge); recap_edge = y(4); end
if isnan(sol.y_star); y_star_use = y(end); else; y_star_use = sol.y_star; end

R_lo = params.R_ref - band;
R_hi = params.R_ref + band;

near_recap = (y > recap_edge) & (y <= recap_edge + band) & (y < y_star_use);
near_ystar = (y >= y_star_use - band) & (y < y_star_use) & (y > recap_edge);
near_Rref  = (y >= R_lo) & (y <= R_hi) & (y > recap_edge) & (y < y_star_use);
interior   = (y > recap_edge + band) & (y < y_star_use - band) & ...
             ~((y > R_lo) & (y < R_hi));

vpp = sol.v_second;
tol_zero = 1e-8;

[near_recap_max, nr_idx] = safe_max(vpp, near_recap, y);
[near_ystar_max, ny_idx] = safe_max(vpp, near_ystar, y);
[near_Rref_max, nR_idx]  = safe_max(vpp, near_Rref, y);
[interior_max, ii_idx]   = safe_max(vpp, interior, y);

interior_positive_count = sum(vpp(interior) > tol_zero);
interior_total_count = sum(interior);
near_recap_positive_count = sum(vpp(near_recap) > tol_zero);
near_recap_total_count = sum(near_recap);

fname_mat = sprintf('resultM_lambda_%.4f.mat', lambda_S_arg);
save(fname_mat, 'sol', 'params', 'settings', 'y_post', 'recap_edge', ...
     'near_recap_max', 'near_ystar_max', 'near_Rref_max', ...
     'interior_max', 'ii_idx', 'interior_positive_count', ...
     'interior_total_count', 'near_recap_positive_count', ...
     'near_recap_total_count', 'band');

fname_csv = sprintf('resultM_lambda_%.4f.csv', lambda_S_arg);
fid = fopen(fname_csv, 'w');
fprintf(fid, ['lambda_S,y_star,y_post,v1,recap_edge,', ...
              'near_recap_max_vpp,near_ystar_max_vpp,near_Rref_max_vpp,', ...
              'interior_max_vpp,interior_max_vpp_at_y,', ...
              'interior_positive_count,interior_total_count,', ...
              'near_recap_positive_count,near_recap_total_count,', ...
              'iterations,final_err,converged\n']);
fprintf(fid, '%.6f,%.6f,%.6f,%.6f,%.6f,%.6e,%.6e,%.6e,%.6e,%.6f,%d,%d,%d,%d,%d,%.6e,%d\n', ...
        lambda_S_arg, sol.y_star, y_post, v(1), recap_edge, ...
        near_recap_max, near_ystar_max, near_Rref_max, ...
        interior_max, y(ii_idx), ...
        interior_positive_count, interior_total_count, ...
        near_recap_positive_count, near_recap_total_count, ...
        sol.iterations, sol.final_err, sol.converged);
fclose(fid);

printf(["DONE lambda_S=%.4f  y_star=%.6f  v1=%.6f  recap_edge=%.6f\n" ...
        "     near_recap_max_vpp=%.4e  near_ystar_max_vpp=%.4e  near_Rref_max_vpp=%.4e\n" ...
        "     INTERIOR: max_vpp=%.4e at y=%.4f  positive_pts=%d/%d\n" ...
        "     near_recap: positive_pts=%d/%d\n" ...
        "     iters=%d  final_err=%.3e  converged=%d\n"], ...
       lambda_S_arg, sol.y_star, v(1), recap_edge, ...
       near_recap_max, near_ystar_max, near_Rref_max, ...
       interior_max, y(ii_idx), interior_positive_count, interior_total_count, ...
       near_recap_positive_count, near_recap_total_count, ...
       sol.iterations, sol.final_err, sol.converged);
