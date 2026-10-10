function [m, idx] = safe_max(vpp, mask, y)
    if any(mask)
        [m, local_idx] = max(vpp(mask));
        idxs = find(mask);
        idx = idxs(local_idx);
    else
        m = NaN;
        idx = 1;
    end
end
