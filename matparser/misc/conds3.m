function out = f_cmpCnd(str, cnd)

    out = 0;
    for i=1:length(cnd)
        if strcmp(str, cnd{i})
            out = i;
            return;
        end
    end
    munlock f_cmpCnd;
    clear cnd str functions;
end