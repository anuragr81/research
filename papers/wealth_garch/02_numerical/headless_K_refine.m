%% Mesh refinement at FIXED K -- answers BOTH open questions from the K sweep
%% in the same set of solves.
%%
%% Q1: is the narrowing of the convex region at LARGE K real, or is it just
%%     that at K=0.05 the region is only 3 grid cells wide and the mesh
%%     cannot resolve it?
%%       real       -> convex_width roughly CONSTANT, n_convex scales with 1/dy
%%       mesh-bound -> convex_width shrinks in proportion to dy, n_convex flat
%%
%% Q2: is the kink at the trigger genuine (V' really jumps, C^0 only), or an
%%     artefact of the discrete max(v_continuation, Mv) update that would
%%     disappear in the limit?
%%       genuine  -> right_slope - (1+kappa) converges to a NONZERO limit
%%       artefact -> right_slope - (1+kappa) -> 0 as dy -> 0
%%     The left slope is the control: it must read exactly 1+kappa at every
%%     mesh, because the Mv branch is affine with that slope by construction.
%%     If it ever does not, the measurement is broken and Q2 is unanswerable.
%%
%% CONVENTION (matched to headless_M_refine.m on 6 Aug 2026): the scan for
%% the convex region starts one cell ABOVE the detected trigger, excluding
%% the trigger point itself. That point sits on the kink and its v'' is
%% contaminated by the branch switch; including it inflated the count by one
%% at every mesh and distorted the Q1 ratios. Reported cvx_width is n*dy
%% over the scanned points -- note headless_M_refine.m reports (n-1)*dy for
%% the same region, so compare n*dy to n*dy, not width to width.
%%
%% Usage: octave --no-gui --eval "K_arg=0.05; run('headless_K_refine.m')"
%% Optional: N_list_arg (default [301 601]); adding 1201 strengthens both
%% tests but costs ~4x the N=601 run.
if ~exist('K_arg','var'); error('set K_arg, e.g. K_arg=0.05'); end
if ~exist('N_list_arg','var'); N_list_arg = [301 601]; end
if ~exist('lambda_S_arg','var'); lambda_S_arg = 1.0; end

params.r=0.025; params.mu=0.04; params.mu_L=0.03; params.rho=0.12;
params.sigma=0.08; params.sigma_L=0.03; params.c=0.20; params.gamma=0.02;
params.a1=0.045; params.a2=0.05; params.a3=0.30;
params.kappa=0.01; params.kappa_p=0.02; params.pi_cap=[];
params.R_ref=1.15; params.K=K_arg; params.lambda_S=lambda_S_arg;
check_params_valid(params);
t = 1 + params.kappa;

printf("K=%.5f  lambda_S=%.2f  (1+kappa=%.4f)\n\n", K_arg, lambda_S_arg, t);
printf("%6s %9s %10s %9s %11s %13s %11s %12s %6s\n", ...
  "N","dy","cvx_width","n_cvx","recap_edge","right_slope","right-(1+k)","v(ie+1)-v(ie)","conv");

rows = [];
for k = 1:length(N_list_arg)
    settings.y_max=2.5; settings.N=N_list_arg(k); settings.dt=0.05;
    settings.max_iter=4000; settings.tol=1e-6; settings.pi_grid_size=151;
    sol = solve_bank_qvi_lambda_M(params, settings, 'QVI');

    y=sol.y; v=sol.v; vpp=sol.v_second; dy=y(2)-y(1); N=length(y);
    if isfield(sol,'Mv'); Ob=sol.Mv; else; Ob=sol.Hv; end
    active = abs(v-Ob) < 1e-9;
    if any(active); edge=max(y(active)); else; edge=y(1); end
    ie = find(abs(y-edge) < dy/2, 1);
    if isempty(ie) || ie < 2; ie = 2; end

    if ~isnan(sol.y_star); ys=sol.y_star; else; ys=y(end); end
    scan = ((ie+1):min(N-1, ie+round(0.6/dy)))';
    scan = scan(y(scan) < ys - 5*dy);
    posv = scan(vpp(scan) > 1e-8);
    if isempty(posv); wid=0; npos=0; else
        wid = y(max(posv)) - y(min(posv)) + dy; npos = length(posv); end

    ls = (v(ie)   - v(ie-1)) / dy;
    rs = (v(ie+1) - v(ie))   / dy;

    n_recap_cells = ie - 1;
    if n_recap_cells < 3
        printf(["  WARNING N=%d: recapitalisation region is only %d cell(s) wide.\n" ...
                "    The trigger has collapsed onto the lower boundary, so the\n" ...
                "    one-sided slopes and the convex-region measurement below are\n" ...
                "    unreliable at this mesh. Expect the left-slope control to fail.\n"], ...
               N, n_recap_cells);
    end

    vgap = v(ie+1) - v(ie);
    printf("%6d %9.6f %10.6f %9d %11.5f %13.8f %11.3e %12.6e %6d\n", ...
      N, dy, wid, npos, edge, rs, rs-t, vgap, sol.converged);
    rows(end+1,:) = [N, dy, wid, npos, ls, rs-t, edge, vgap];
end

printf("\nCONTROL: left_slope must be %.8f at every mesh.\n", t);
if all(abs(rows(:,5) - t) < 1e-8)
    printf("  OK -- exact at every mesh, measurement is sound.\n");
else
    printf("  FAILED -- Mv branch not applied as intended; Q2 is unanswerable until fixed.\n");
end

if size(rows,1) >= 2
    printf("\nQ1 (convex region at this K):\n");
    for k=2:size(rows,1)
        dyr = rows(k-1,2)/rows(k,2);
        wr  = NaN; if rows(k,3)>0; wr = rows(k-1,3)/rows(k,3); end
        cr  = NaN; if rows(k-1,4)>0; cr = rows(k,4)/rows(k-1,4); end
        printf("  N %d->%d: dy ratio %.2f | width ratio %.2f | count ratio %.2f\n", ...
               rows(k-1,1), rows(k,1), dyr, wr, cr);
    end
    printf("  width ratio ~1 and count ratio ~dy ratio -> REAL (region survives refinement)\n");
    printf("  width ratio ~dy ratio and count ratio ~1 -> MESH-BOUND (was never resolved)\n");

    printf("\nQ2 (kink at the trigger):\n");
    for k=1:size(rows,1)
        printf("  N=%4d: right-(1+kappa) = %.6e\n", rows(k,1), rows(k,6));
    end
    if size(rows,1)>=2
        r = rows(end,6)/rows(1,6);
        printf("  ratio finest/coarsest = %.3f\n", r);
        printf("  ratio ~1 (excess holds up)      -> GENUINE jump, V' is C^0 at the trigger\n");
        printf("  ratio ~1/dy-ratio (excess falls) -> ARTEFACT, smooth fit recovered in the limit\n");
        printf("\n  DIAGNOSTIC for a ratio near the dy ratio (excess GROWING):\n");
        printf("    v(ie+1)-v(ie) by mesh:");
        for k=1:size(rows,1); printf(" %.4e", rows(k,8)); end
        printf("\n");
        printf("    if that tends to a CONSTANT, V itself is jumping -- not physical here,\n");
        printf("      so suspect the trigger index rather than the solution.\n");
        printf("    recap_edge by mesh:");
        for k=1:size(rows,1); printf(" %.5f", rows(k,7)); end
        printf("\n");
        printf("    if recap_edge MOVES between meshes, the detected trigger sits a\n");
        printf("      variable distance below the true boundary and the forward\n");
        printf("      difference straddles it -- the meshes are then not comparable\n");
        printf("      and Q2 is unanswerable from these numbers alone.\n");
    end
end
