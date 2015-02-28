function [out, day2] = f_getConds(data, alphas, settings, day2, hour, steps)
    % this function gets the entry exit conditions, for exits getVals need to take the opposite alphas
    try
        type = {'buy', 'sell'};out=[];
        fnames = fieldnames(alphas);
        for i=1:length(fnames)
            if size(data.(fnames{i}).alpha,1)>steps
                for kk=1:length(type)
                    if isfield(settings.(fnames{i}), type{kk})
                        if ~isempty(day2)
                            if isfield(day2, fnames{i})
                                if isfield(day2.(fnames{i}), ['hour_' sprintf('%d', hour)])
                                    if isfield(day2.(fnames{i}).(['hour_' sprintf('%d', hour)]), type{kk})
                                        day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).day = day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).day+1;
                                    else
                                        day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).day = 1;
                                    end
                                else
                                    day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).day = 1;
                                end
                            else
                                day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).day = 1;
                            end
                        else
                            day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).day = 1;
                        end

                        idx = find(settings.(fnames{i}).(type{kk}).iEOD==1);
                        if day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).day==1
                            if idx(1)>steps
                                day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).idx=(1:idx(1))';
                            else
                                day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).idx = 0;
                            end
                        else
                            if idx(day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).day)-idx(day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).day-1)>steps
                                day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).idx=(idx(day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).day-1)+1:idx(day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).day))';
                            else
                                day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).idx = 0;
                            end
                        end
                        if size(day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).idx,1)>1     
                            out.(fnames{i}).(type{kk}).xsnumber = settings.(fnames{i}).(type{kk}).xsnumber(day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).idx,1);
                            [~, idc, ~] = intersect(data.(fnames{i}).symbolSequenceNumber, out.(fnames{i}).(type{kk}).xsnumber, 'rows');    
                            assert((any(size(idc,1)-size(day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).idx,1))~=0)==0, '%s %s hour %d entry length doesnt match, please check!! ',fnames{i}, type{kk}, hour);
                            fnames_E = f_parseCond(settings.(fnames{i}).(type{kk}).entry.fentry_conds);     
                            for ii=1:length(fnames_E)
                                ndx = strfind(fnames_E{ii}, '_');
                                level = sscanf(fnames_E{ii}(ndx+1:end),'%d');
                                out.(fnames{i}).(type{kk}).entry.(fnames_E{ii}).condo = settings.(fnames{i}).(type{kk}).entry.(fnames_E{ii}).conds;
                                out.(fnames{i}).(type{kk}).entry.(fnames_E{ii}).conds = f_parseCond(settings.(fnames{i}).(type{kk}).entry.(fnames_E{ii}).conds);
                                out.(fnames{i}).(type{kk}).entry.(fnames_E{ii}).scond = f_getScond(out.(fnames{i}).(type{kk}).entry.(fnames_E{ii}).conds, 'inames', fnames{i}, 'type', type{kk}, 'hour', hour, 'levels',level);
                                out.(fnames{i}).(type{kk}).entry.(fnames_E{ii}).pnl = settings.(fnames{i}).(type{kk}).entry.(fnames_E{ii}).pnl;
                                out.(fnames{i}).(type{kk}).entry.(fnames_E{ii}).vals = settings.(fnames{i}).(type{kk}).entry.(fnames_E{ii}).vals(day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).idx,1);

                                for iR=1:length(out.(fnames{i}).(type{kk}).entry.(fnames_E{ii}).scond)
                                    out.(fnames{i}).(type{kk}).entry.(fnames_E{ii}).xvals{iR} = f_getVals(alphas.(fnames{i}), out.(fnames{i}).(type{kk}).entry.(fnames_E{ii}).scond{iR}, level, type{kk}, 'entry', steps);
                                    out.(fnames{i}).(type{kk}).entry.(fnames_E{ii}).xvals_o{iR} = f_getVals(alphas.(fnames{i}), out.(fnames{i}).(type{kk}).entry.(fnames_E{ii}).scond{iR}, level, type{kk}, 'entry',0);
                                end       
                            end
                            out.(fnames{i}).(type{kk}).entry.vals = settings.(fnames{i}).(type{kk}).entry.vals(day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).idx,1);
                            assert((any(size(idc,1)-size(out.(fnames{i}).(type{kk}).entry.(fnames_E{1}).xvals{1},1))~=0)==0, '%s %s hour %d entry length doesnt match, please check!! ',fnames{i}, type{kk}, hour);
                            out.(fnames{i}).(type{kk}).entry.pnl = settings.(fnames{i}).(type{kk}).entry.pnl;
                            out.(fnames{i}).(type{kk}).entry.dpnl = settings.(fnames{i}).(type{kk}).entry.dpnl;
                            out.(fnames{i}).(type{kk}).entry.fentry_conds = settings.(fnames{i}).(type{kk}).entry.fentry_conds;

                            out.(fnames{i}).(type{kk}).exit.vals = settings.(fnames{i}).(type{kk}).exit.vals(day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).idx,1);
                            out.(fnames{i}).(type{kk}).exit.pnl = settings.(fnames{i}).(type{kk}).exit.pnl;
                            out.(fnames{i}).(type{kk}).exit.dpnl = settings.(fnames{i}).(type{kk}).exit.dpnl;
                            out.(fnames{i}).(type{kk}).exit.fexit_conds = settings.(fnames{i}).(type{kk}).exit.fexit_conds;

                            fnames_X = f_parseCond(settings.(fnames{i}).(type{kk}).exit.fexit_conds); 
                            for ii=1:length(fnames_X)
                                ndx = strfind(fnames_X{ii}, '_');
                                level = sscanf(fnames_X{ii}(ndx+1:end),'%d');
                                out.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).pnl = settings.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).pnl;
                                out.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).vals = settings.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).vals(day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).idx,1);

                                fnames_ex = fieldnames(settings.(fnames{i}).(type{kk}).exit.(fnames_X{ii}));
                                if ismember('sexit_conds', fnames_ex)
                                    fnames_ex = f_parseCond(settings.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).sexit_conds);
                                end
                                for iK=1:length(fnames_ex)
                                    if ismember(fnames_ex{iK}, {'pnl', 'dpnl', 'conds'})
                                        out.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}) = settings.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK});
                                    elseif strcmp(fnames_ex{iK}, 'vals')
                                        out.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}) = settings.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK})(day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).idx,1);
                                    else
                                        fnames_ee = fieldnames(settings.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}));
                                        if ismember('fexit_conds', fnames_ee)
                                            fnames_ee = f_parseCond(settings.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).fexit_conds);
                                        end
                                        for iG=1:length(fnames_ee)
                                            if ismember(fnames_ee{iG}, {'pnl', 'dpnl', 'conds'})
                                                out.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG})= settings.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG});
                                            elseif strcmp(fnames_ee{iG},'vals')
                                                out.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG})= settings.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG})(day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).idx,1);
                                            else
                                                out.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG}).condo = settings.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG}).conds;
                                                out.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG}).pnl = settings.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG}).pnl;
                                                out.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG}).vals = settings.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG}).vals(day2.(fnames{i}).(['hour_' sprintf('%d', hour)]).(type{kk}).idx,1);
                                                out.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG}).conds = f_parseCond(settings.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG}).conds);
                                                out.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG}).scond = f_getScond(out.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG}).conds, 'inames', fnames{i}, 'type', type{kk}, 'hour', hour, 'levels',level);
                                                ndx = strfind(fnames_ee{iG}, '_');
                                                level_e = sscanf(fnames_ee{iG}(ndx+1:end), '%d');
                                                for iM=1:length(out.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG}).scond)
                                                    out.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG}).xvals{iM} = f_getVals(alphas.(fnames{i}), out.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG}).scond{iM}, level_e, type{kk}, 'exit', steps);
                                                    out.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG}).xvals_o{iM} = f_getVals(alphas.(fnames{i}), out.(fnames{i}).(type{kk}).exit.(fnames_X{ii}).(fnames_ex{iK}).(fnames_ee{iG}).scond{iM}, level_e, type{kk}, 'exit', 0);
                                                end
                                            end
                                        end
                                    end
                                end
                            end
                        end
                    end
                end
            end
            if isfield(out, fnames{i})
                if isfield(out.(fnames{i}), 'buy') && isfield(out.(fnames{i}), 'sell')
                else
                    out = rmfield(out, fnames{i});
                    settings = rmfield(settings, fnames{i});
                    alphas = rmfield(alphas, fnames{i});
                end
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
    munlock f_getConds;
    clear data settings fnames type fnames_E idx idc mex functions;
end

