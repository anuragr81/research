function Mv = compute_M_operator(y, v, params)
    % Our additive-cost impulse operator, PROOFS.tex eq. (impulse):
    %     MV(x) = sup_{xi>0} { V(x+xi) - K - (1+kappa)*xi }
    % vs the benchmark's proportional-cost convention in compute_H_operator.m
    %     Y_new = (1-kappa)Y_old + (1-kappa')xi + kappa
    % Requires params.K (fixed issuance cost) and params.kappa (proportional).
    %
    % Discretisation: on a uniform grid y with y(j) = y(i) + xi, the objective
    %     v(j) - K - (1+kappa)*(y(j) - y(i))
    %   = [ v(j) - (1+kappa)*y(j) ] + (1+kappa)*y(i) - K
    % so the j-dependent part is base(j) = v(j) - (1+kappa)*y(j) and the sup
    % over xi>0 is a strict suffix maximum of base, computed in one backward
    % pass (same O(N) structure as compute_H_operator.m).
    %
    % xi>0 STRICTLY: the suffix starts at i+1, never i, so the null jump is
    % excluded -- matching sup_{xi>0} and preserving the meaning of the
    % obstacle V >= MV (a zero jump would make it trivially tight everywhere).
    if ~isfield(params, 'K')
        error('compute_M_operator: params.K (fixed issuance cost) is required');
    end
    K = params.K;
    kappa = params.kappa;
    N = length(y);

    base = v - (1 + kappa) * y;

    suffix = -1e100 * ones(N, 1);
    suffix(N) = base(N);
    for j = N-1:-1:1
        suffix(j) = max(base(j), suffix(j+1));
    end

    Mv = -1e100 * ones(N, 1);
    for i = 1:N-1
        Mv(i) = suffix(i+1) + (1 + kappa) * y(i) - K;
    end
    Mv(N) = -1e100;
end
