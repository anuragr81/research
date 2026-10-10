%% K-SENSITIVITY: one value of the fixed issuance cost K, at fixed lambda_S.
%%
%% WHY: Remark 21 attributes the convex region above the recapitalisation
%% trigger to the FIXED cost K, not to Lambda. That attribution makes a
%% sharp, falsifiable prediction: as K -> 0 the trigger and target collapse
%% together (no fixed cost, so infinitesimal jumps are worthwhile) and the
%% convex region between them must vanish. If the region SURVIVES at K=0
%% the attribution is WRONG and something else is producing it.
%%
%% Default lambda_S = 1.0, where Lambda == 0 identically -- so K is the
%% only thing varying and the test is clean.
%%
%% NOTE on K=0: the sup is over xi>0 strictly, and on a grid the smallest
%% jump is dy, so MV stays well defined. But K=0 is the discrete stand-in
%% for the singular-control limit, and the trigger/target separation there
%% is expected to be O(dy) -- i.e. grid-limited, not structural. Read the
%% K=0 row as "as close to no fixed cost as this grid allows", not as an
%% exact singular-control solution.
%%
%% Usage: octave --no-gui --eval "K_arg=0.02; run('headless_K_single.m')"
if ~exist('K_arg','var'); error('set K_arg, e.g. K_arg=0.02'); end
if ~exist('lambda_S_arg','var'); lambda_S_arg = 1.0; end

params.r=0.025; params.mu=0.04; params.mu_L=0.03; params.rho=0.12;
params.sigma=0.08; params.sigma_L=0.03; params.c=0.20; params.gamma=0.02;
params.a1=0.045; params.a2=0.05; params.a3=0.30;
params.kappa=0.01; params.kappa_p=0.02; params.pi_cap=[];
params.R_ref=1.15;
params.K = K_arg; params.lambda_S = lambda_S_arg;
check_params_valid(params);

settings.y_max=2.5; settings.N=301; settings.dt=0.05;
settings.max_iter=3000; settings.tol=1e-6; settings.pi_grid_size=151;

sol = solve_bank_qvi_lambda_M(params, settings, 'QVI');

y=sol.y; v=sol.v; vp=sol.v_prime; vpp=sol.v_second; dy=y(2)-y(1); N=length(y);
if isfield(sol,'Mv'); Ob=sol.Mv; else; Ob=sol.Hv; end
t = 1 + params.kappa;

active = abs(v-Ob) < 1e-9;
if any(active); recap_edge=max(y(active)); else; recap_edge=y(1); end
i_edge = find(abs(y-recap_edge) < dy/2, 1);
if isempty(i_edge) || i_edge < 2; i_edge = 2; end

if ~isnan(sol.y_star); ystar=sol.y_star; else; ystar=y(end); end
scan = (i_edge : min(N-1, i_edge + round(0.6/dy)))';
scan = scan(y(scan) < ystar - 5*dy);

[peak_vp, ploc] = max(vp(scan));
peak_idx = scan(ploc);
max_excess = peak_vp - t;

above = scan(vp(scan) > t + 1e-12);
if isempty(above); trig_to_target = 0; else
  trig_to_target = y(max(above)) - y(min(above)) + dy; end

posv = scan(vpp(scan) > 1e-8);
if isempty(posv); convex_width=0; n_convex=0; else
  convex_width = y(max(posv)) - y(min(posv)) + dy; n_convex = length(posv); end

left_slope  = (v(i_edge)   - v(i_edge-1)) / dy;
right_slope = (v(i_edge+1) - v(i_edge))   / dy;

save(sprintf('resultK_%.5f.mat', K_arg), 'sol','params','settings', ...
     'recap_edge','max_excess','convex_width','n_convex','trig_to_target', ...
     'left_slope','right_slope');

fid = fopen(sprintf('resultK_%.5f.csv', K_arg), 'w');
fprintf(fid, ['K,lambda_S,recap_edge,y_star,trig_to_target,convex_width,', ...
              'n_convex,max_excess,left_slope,right_slope,iterations,', ...
              'final_err,converged\n']);
fprintf(fid, '%.5f,%.4f,%.6f,%.6f,%.6f,%.6f,%d,%.6e,%.8f,%.8f,%d,%.4e,%d\n', ...
        K_arg, lambda_S_arg, recap_edge, sol.y_star, trig_to_target, ...
        convex_width, n_convex, max_excess, left_slope, right_slope, ...
        sol.iterations, sol.final_err, sol.converged);
fclose(fid);

printf(["DONE K=%.5f lambda_S=%.2f | recap_edge=%.4f y_star=%.4f\n" ...
        "     trig_to_target=%.4f  CONVEX_WIDTH=%.4f (%d pts)  max_excess=%.4e\n" ...
        "     one-sided at trigger: left=%.8f right=%.8f (1+kappa=%.4f)\n" ...
        "     iters=%d final_err=%.3e converged=%d\n"], ...
       K_arg, lambda_S_arg, recap_edge, sol.y_star, trig_to_target, ...
       convex_width, n_convex, max_excess, left_slope, right_slope, t, ...
       sol.iterations, sol.final_err, sol.converged);
