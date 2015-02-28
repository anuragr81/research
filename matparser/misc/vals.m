function out = f_getVals(data, conds, level, type, xtype, steps)
    % this takes the 12 optimal conditions and calculates the corresponding pnl for each level
    % for exits, it needs to use the opposite side alphas.
    try
        out=[];
        fnames = fieldnames(data);
        fnames2 = fieldnames(data.go);
        if strcmp(xtype, 'exit')
            if strcmp(type, 'buy')
                type = 'sell';
            elseif strcmp(type, 'sell')
                type = 'buy';
            end
        end
        if ~isempty(conds.conds_2{1})
            for i=1:length(conds.conds_1)
                if i==1
                    cdx= f_cmpCnd(conds.conds_1{i}, fnames);
                    if cdx~=0
                        alpha_t = data.(fnames{cdx}).(type).levels(1:end-steps)==level;
                    else
                        cdx= f_cmpCnd(conds.conds_1{i}, fnames2);
                        if cdx~=0
                            alpha_t = data.go.(fnames2{cdx})(1:end-steps);
                        else
                            % error msg here
                            
                        end
                    end
                    
                    cdx= f_cmpCnd(conds.conds_2{i}, fnames);
                    if cdx~=0
                        alpha_c = data.(fnames{cdx}).(type).levels(1:end-steps)==level;
                    else
                        cdx= f_cmpCnd(conds.conds_2{i}, fnames2);
                        if cdx~=0
                            alpha_c = data.go.(fnames2{cdx})(1:end-steps);
                        else
                            % error msg here
                        end
                    end
                    
                    if ~isempty(alpha_t) && ~isempty(alpha_c)
                        if strcmp(conds.rels{i}, '&')
                            out = alpha_t & alpha_c;
                        elseif strcmp(conds.rels{i}, '|')
                            out = alpha_t | alpha_c;
                        end
                    end
                else
                    if ~isempty(out)
                        cdx= f_cmpCnd(conds.conds_1{i}, fnames);
                        if cdx~=0
                            alpha_t = data.(fnames{cdx}).(type).levels(1:end-steps)==level;
                        else
                            cdx= f_cmpCnd(conds.conds_1{i}, fnames2);
                            if cdx~=0
                                alpha_t = data.go.(fnames2{cdx})(1:end-steps);
                            else
                                % error msg here

                            end
                        end
                        if ~isempty(alpha_t)
                            if strcmp(conds.rels{i}, '&')
                                out = out & alpha_t;
                            elseif strcmp(conds.rels{i}, '|')
                                out = out | alpha_t;
                            end
                        end
                    end     
                end
            end
        else
            cdx= f_cmpCnd(conds.conds_1{1}, fnames);
            if cdx~=0
                out = data.(fnames{cdx}).(type).levels(1:end-steps)==level;
            else
                % error msg here
            end
        end              
   catch exception
        if size(exception.stack,1)>1
            msg = strcat(exception.identifier,' ', exception.message, ...
              ' ',exception.stack(1,1).name, ' ', ...
              num2str(exception.stack(1,1).line), ' ',exception.stack(2,1).name, ' ', ...
              num2str(exception.stack(2,1).line));
        else
            msg = strcat(exception.identifier,' ', exception.message, ...
              ' ',exception.stack(1,1).name, ' ', ...
              num2str(exception.stack(1,1).line));
        end
        error(msg);
    end   
    munlock f_getVals;
    clear data conds alpha_c alpha_t level functions;
end

