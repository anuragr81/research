% verify_M_operator.m
%
% Checks compute_M_operator.m's O(N) suffix-maximum implementation of
%
%     MV(x) = sup_{xi>0} { V(x+xi) - K - (1+kappa)*xi }
%
% against a direct O(N^2) supremum over the same grid points. The brute-force
% form is transparently the definition, so any disagreement is a defect in the
% fast path.
%
% Both sides take the supremum over GRID POINTS only, so this isolates
% implementation correctness from discretisation error -- the two are different
% questions and only the first is being asked here.
%
% Failure modes this discriminates (each verified to move the residual from
% ~1e-16 to between 1e-3 and 1e+100, i.e. far above TOL):
%   - suffix started at i rather than i+1, admitting the null jump xi=0 and
%     making the obstacle V >= MV trivially tight;
%   - the fixed cost K dropped from the objective;
%   - the sign of the proportional cost kappa reversed;
%   - the y(N) sentinel omitted, where no upward jump exists.
%
% Profiles include non-concave ones deliberately: a suffix maximum is most
% likely to misbehave exactly where V is not concave, which is the regime
% Remark 4 of PROOFS_v2.tex reports.
%
% Usage:  octave --no-gui verify_M_operator.m
% Exit:   0 if every tag passes.

more off;
TOL = 1e-12;
ledger = {};

function Mv = M_bruteforce(y, v, params)
    % Definition, evaluated directly: for each i, maximise over every grid
    % point strictly above it. No algebraic rearrangement.
    N = length(y);
    Mv = -1e100 * ones(N, 1);
    for i = 1:N-1
        best = -1e100;
        for j = i+1:N
            cand = v(j) - params.K - (1 + params.kappa) * (y(j) - y(i));
            if cand > best
                best = cand;
            end
        end
        Mv(i) = best;
    end
    Mv(N) = -1e100;
end

function ledger = record(ledger, tag, ok, statement)
    ledger{end+1} = {tag, ok, statement};
end

params.K = 0.02;
params.kappa = 0.01;

N = 301;
y = linspace(1.0, 2.5, N)';

% ---------------------------------------------------------------------
% Profiles. Names are descriptive of shape, not of provenance.
% ---------------------------------------------------------------------
profiles = {};
profiles{1} = {'concave', -(y - 1.8).^2 + 2.0};
profiles{2} = {'affine', 0.9 * y + 0.1};
profiles{3} = {'convex region above a kink', ...
               0.8 * y + 0.35 * ((y > 1.3) .* (y - 1.3).^2 .* exp(-6 * (y - 1.3)))};
profiles{4} = {'non-concave, two regions', ...
               -(y - 1.5).^2 + 0.6 * max(y - 2.0, 0).^2};
profiles{5} = {'piecewise-flat (ties in the suffix)', round(y * 4) / 4};
profiles{6} = {'monotone decreasing', 3.0 - 1.4 * y};

worst = 0;
for p = 1:numel(profiles)
    name = profiles{p}{1};
    v    = profiles{p}{2};
    fast = compute_M_operator(y, v, params);
    slow = M_bruteforce(y, v, params);
    d = max(abs(fast(1:N-1) - slow(1:N-1)));
    worst = max(worst, d);
    ledger = record(ledger, sprintf('V%d', p), d < TOL, ...
        sprintf('%s: max|fast-slow| = %.3e over %d interior points', ...
                name, d, N-1));
end

% ---------------------------------------------------------------------
% V7: small grids, where an off-by-one in the backward pass would surface.
% ---------------------------------------------------------------------
ok7 = true; d7 = 0;
for Nsmall = [2 3 5 11]
    ys = linspace(1.0, 2.0, Nsmall)';
    vs = -(ys - 1.6).^2 + 1.5;
    fs = compute_M_operator(ys, vs, params);
    ss = M_bruteforce(ys, vs, params);
    d = max(abs(fs(1:Nsmall-1) - ss(1:Nsmall-1)));
    d7 = max(d7, d);
    ok7 = ok7 && (d < TOL);
end
ledger = record(ledger, 'V7', ok7, ...
    sprintf('small grids N in {2,3,5,11}: worst max|fast-slow| = %.3e', d7));

% ---------------------------------------------------------------------
% V8: degenerate cost parameters. K=0 and kappa=0 are the limits the
% K-sweep of Remark 5 relies on, so the operator must stay exact there.
% ---------------------------------------------------------------------
ok8 = true; d8 = 0;
combos = [0.0 0.0; 0.0 0.01; 0.5 0.0; 0.02 0.25];
for k = 1:size(combos, 1)
    q.K = combos(k, 1);
    q.kappa = combos(k, 2);
    v = profiles{4}{2};
    fq = compute_M_operator(y, v, q);
    sq = M_bruteforce(y, v, q);
    d = max(abs(fq(1:N-1) - sq(1:N-1)));
    d8 = max(d8, d);
    ok8 = ok8 && (d < TOL);
end
ledger = record(ledger, 'V8', ok8, ...
    sprintf('degenerate costs (K,kappa) in {(0,0),(0,.01),(.5,0),(.02,.25)}: worst = %.3e', d8));

% ---------------------------------------------------------------------
% V9: the supremum is over xi>0 STRICTLY. Direct property test rather than
% a comparison: if the null jump were admitted, MV(i) would be at least
% v(i)-K, so on a profile where that dominates every upward move the two
% readings separate.
% ---------------------------------------------------------------------
vdec = 3.0 - 1.4 * y;                 % decreasing: no upward jump is ever profitable
Mdec = compute_M_operator(y, vdec, params);
null_jump_value = vdec(1:N-1) - params.K;
strict = all(Mdec(1:N-1) < null_jump_value - 1e-14);
ledger = record(ledger, 'V9', strict, ...
    sprintf(['strict xi>0: on a decreasing profile every MV(i) lies below the ' ...
             'null-jump value v(i)-K (margin >= %.3e), so xi=0 is excluded'], ...
            min(null_jump_value - Mdec(1:N-1))));

% ---------------------------------------------------------------------
% V10: the sentinel at the top of the grid.
% ---------------------------------------------------------------------
vc = profiles{1}{2};
sent = compute_M_operator(y, vc, params);
ledger = record(ledger, 'V10', sent(N) <= -1e99, ...
    sprintf('no upward jump exists from y(N): MV(N) = %.3e is the sentinel', sent(N)));

% ---------------------------------------------------------------------
line = repmat('=', 1, 78);
printf('%s\n', line);
printf('compute_M_operator.m: O(N) suffix maximum vs O(N^2) direct supremum\n');
printf('%s\n', line);
nfail = 0;
for i = 1:numel(ledger)
    tag = ledger{i}{1};
    ok  = ledger{i}{2};
    st  = ledger{i}{3};
    if ok
        status = 'PASS';
    else
        status = 'FAIL';
        nfail = nfail + 1;
    end
    printf('%-6s%-8s%s\n', tag, status, st);
end
printf('%s\n', line);
printf('worst residual across the six profiles: %.3e (tolerance %.1e)\n', worst, TOL);
if nfail == 0
    printf('The fast path agrees with the definition to machine precision.\n');
else
    printf('%d tag(s) FAILED.\n', nfail);
end
exit(nfail > 0);
