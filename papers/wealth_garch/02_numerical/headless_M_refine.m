%% GRID-REFINEMENT TEST for the positive-v'' region above recap_edge
%% under the M operator. This is the falsifiable discriminator:
%%
%%   artifact of the max(continuation, Mv) kink -> the positive region is
%%     pinned to a fixed number of GRID POINTS, so its y-extent scales
%%     with dy (halving N-1 spacing halves the extent)
%%   genuine convex region of V -> the positive region has a fixed
%%     Y-WIDTH, so the grid-point count grows as dy shrinks
%%
%% Usage: octave --no-gui --eval "lambda_S_arg=1.0; run('headless_M_refine.m')"
%% Optional: N_list_arg (default [301 601 1201]).
if ~exist('lambda_S_arg','var'); error('set lambda_S_arg, e.g. 1.0'); end
if ~exist('N_list_arg','var'); N_list_arg = [301 601 1201]; end

params.r=0.025; params.mu=0.04; params.mu_L=0.03; params.rho=0.12;
params.sigma=0.08; params.sigma_L=0.03; params.c=0.20; params.gamma=0.02;
params.a1=0.045; params.a2=0.05; params.a3=0.30;
params.kappa=0.01; params.kappa_p=0.02; params.pi_cap=[];
params.R_ref=1.15; params.K=0.02; params.lambda_S=lambda_S_arg;
check_params_valid(params);

printf("%6s %10s %8s %10s %12s %12s %10s %9s\n", ...
       "N", "dy", "n_pos", "extent", "extent/dy", "recap_edge", "first_step", "conv");

rows = [];
for k = 1:length(N_list_arg)
    settings.y_max=2.5; settings.N=N_list_arg(k); settings.dt=0.05;
    settings.max_iter=4000; settings.tol=1e-6; settings.pi_grid_size=151;

    sol = solve_bank_qvi_lambda_M(params, settings, 'QVI');
    y=sol.y; v=sol.v; vpp=sol.v_second; dy=y(2)-y(1); N=length(y);
    if isfield(sol,'Mv'); Ob=sol.Mv; else; Ob=sol.Hv; end

    active = abs(v-Ob) < 1e-9;
    if any(active); recap_edge=max(y(active)); else; recap_edge=y(1); end
    i_edge = find(abs(y-recap_edge) < dy/2, 1);
    if isempty(i_edge); i_edge=2; end

    if ~isnan(sol.y_star); ystar=sol.y_star; else; ystar=y(end); end
    scan = ((i_edge+1):min(N-1, i_edge+round(0.5/dy)))';
    scan = scan(y(scan) < ystar - 10*dy);

    pos = scan(vpp(scan) > 1e-8);
    if isempty(pos)
        n_pos=0; extent=0; ratio=0; first_step=NaN;
    else
        n_pos=length(pos); extent=y(max(pos))-y(min(pos));
        ratio=extent/dy; first_step=min(pos)-i_edge;
    end

    printf("%6d %10.6f %8d %10.6f %12.2f %12.4f %10d %9d\n", ...
           N, dy, n_pos, extent, ratio, recap_edge, first_step, sol.converged);
    rows(end+1,:) = [N, dy, n_pos, extent, first_step];
end

printf("\nINTERPRETATION\n");
if size(rows,1) >= 2
    for k = 2:size(rows,1)
        dyr = rows(k-1,2)/rows(k,2);
        exr = NaN;
        if rows(k,4) > 0 && rows(k-1,4) > 0
            exr = rows(k-1,4)/rows(k,4);
        end
        cntr = NaN;
        if rows(k,3) > 0 && rows(k-1,3) > 0
            cntr = rows(k,3)/rows(k-1,3);
        end
        printf("  N %d->%d: dy ratio %.2f | y-extent ratio %.2f | point-count ratio %.2f\n", ...
               rows(k-1,1), rows(k,1), dyr, exr, cntr);
    end
    printf("  y-extent ratio ~= dy ratio AND point-count ratio ~= 1 -> ARTIFACT (grid-pinned)\n");
    printf("  y-extent ratio ~= 1   AND point-count ratio ~= dy ratio -> REAL convex region\n");
end
