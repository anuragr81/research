% Rename the converged solves' variables to the names lambda_RN_panelconv.m
% expects. headless_lambda_converge_check_M.m saves sol_new/y_post_new/
% recap_edge_new; the sweep saves sol/y_post/recap_edge. Pure rename, no
% recomputation -- and lambda_RN_panelconv.m is left untouched.
Ls = {'1.0000','1.1000','1.2500','1.5000','1.7500','2.0000'};
for k = 1:numel(Ls)
    L = Ls{k};
    S = load(['resultM_lambda_' L '_maxiter3000.mat']);
    sol = S.sol_new;
    y_post = S.y_post_new;
    recap_edge = S.recap_edge_new;
    params = S.params;
    settings = S.settings;
    save('-v7', ['resultM_lambda_' L '.mat'], 'sol','y_post','recap_edge','params','settings');
    printf('converted %s : y_star=%.6f  (from sol_new)\n', L, sol.y_star);
end
