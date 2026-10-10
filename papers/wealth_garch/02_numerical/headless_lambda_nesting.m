%% Two-sided correctness check for solve_bank_qvi_lambda.m, tiny grid.
%% (1) NESTING: at lambda_S=1 the patched solver must reproduce
%%     solve_bank_qvi.m EXACTLY (Lambda==0 identically, same iterates).
%% (2) WIRED-IN: at lambda_S=1.5 the value function must be strictly
%%     lower somewhere (the penalty must actually bite) -- nesting alone
%%     cannot distinguish "correct" from "Lambda never applied".
params.r=0.01; params.mu=0.04; params.mu_L=0.03; params.rho=0.12;
params.sigma=0.08; params.sigma_L=0.03; params.c=0.20; params.gamma=0.01;
params.a1=0.045; params.a2=0.05; params.a3=0.30;
params.kappa=0.01; params.kappa_p=0.02; params.pi_cap=[];

settings.y_max=2.5; settings.N=51; settings.dt=0.05;
settings.max_iter=300; settings.tol=1e-6; settings.pi_grid_size=21;

sol_orig = solve_bank_qvi(params, settings, 'QVI');

params.lambda_S = 1.0; params.R_ref = 1.15;
sol_lam1 = solve_bank_qvi_lambda(params, settings, 'QVI');

d1 = max(abs(sol_orig.v - sol_lam1.v));
printf("NESTING  max|v_orig - v_lambda1| = %.3e   (must be 0)\n", d1);
assert(d1 == 0, "nesting FAILED: lambda_S=1 does not reproduce original");

params.lambda_S = 1.5;
sol_lam15 = solve_bank_qvi_lambda(params, settings, 'QVI');

d2 = max(abs(sol_lam15.v - sol_orig.v));
below = sol_lam15.v(sol_lam1.y < params.R_ref + 0.3) - ...
        sol_orig.v(sol_lam1.y < params.R_ref + 0.3);
printf("WIRED-IN max|v_lambda1.5 - v_orig| = %.3e   (must be > 0)\n", d2);
printf("         min(v_lam15 - v_orig) near/below R = %.3e (penalty lowers v)\n", min(below));
assert(d2 > 1e-6, "wired-in FAILED: lambda_S=1.5 identical to original");
assert(min(below) < 0, "wired-in FAILED: penalty did not lower v below R");

printf("y_star: orig=%.4f  lam1=%.4f  lam1.5=%.4f\n", ...
       sol_orig.y_star, sol_lam1.y_star, sol_lam15.y_star);
printf("v(1):   orig=%.4f  lam1=%.4f  lam1.5=%.4f\n", ...
       sol_orig.v(1), sol_lam1.v(1), sol_lam15.v(1));
printf("BOTH CHECKS PASSED\n");
