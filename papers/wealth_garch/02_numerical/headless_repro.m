%% headless reproduction of a1_main_vi_solver.m, no plotting
params.r       = 0.01;
params.mu      = 0.04;
params.mu_L    = 0.03;
params.rho     = 0.12;
params.sigma   = 0.08;
params.sigma_L = 0.03;
params.c       = 0.20;
params.gamma   = 0.01;
params.a1 = 0.045;
params.a2 = 0.05;
params.a3 = 0.30;
params.kappa   = 0.01;
params.kappa_p = 0.02;
params.pi_cap  = [];
check_params_valid(params);

settings.y_max        = 2.5;
settings.N            = 301;
settings.dt           = 0.05;
settings.max_iter     = 1500;
settings.tol          = 1e-6;
settings.pi_grid_size = 151;

bounds = compute_value_bounds(params);
sol_qvi = solve_bank_qvi(params, settings, 'QVI');
sol_hjb = solve_bank_qvi(params, settings, 'HJB');
metrics = compute_metrics(sol_qvi, sol_hjb);

printf("y_star (QVI, dividend barrier)     = %.6f   [paper reports 1.674]\n", sol_qvi.y_star);
printf("y_star (HJB, no-issuance benchmark) = %.6f\n", sol_hjb.y_star);
printf("v(1) QVI = %.6f   v(1) HJB = %.6f\n", metrics.v_at_1_qvi, metrics.v_at_1_hjb);
printf("v(1.2) QVI = %.6f   v(1.2) HJB = %.6f\n", metrics.v_at_12_qvi, metrics.v_at_12_hjb);
printf("Delta_v(1.2)/v(1.2) = %.6f   [paper reports 0.5229]\n", (metrics.v_at_12_qvi-metrics.v_at_12_hjb)/metrics.v_at_12_qvi);

% locate y_post_star: where v' crosses 1/(1-kappa_p) after the jump, i.e.
% the post-issuance target the paper reports as 1.228
y = sol_qvi.y; v = sol_qvi.v;
vp = gradient(v, y);
target = 1/(1-params.kappa_p);
idx = find(vp(1:end-1) > target & vp(2:end) <= target, 1);
if ~isempty(idx)
    y_post = interp1(vp(idx:idx+1), y(idx:idx+1), target);
    printf("y_post_star (v'=1/(1-kappa_p)=%.4f) = %.6f   [paper reports 1.228]\n", target, y_post);
else
    printf("y_post_star: no crossing found in range\n");
end
