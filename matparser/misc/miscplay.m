
                for ii=cfg.estep:cfg.xstep
                     xdata{ii} = f_runTData(xdata{ii}, settings, 'sdate', tdates{i}, 'edate', tdates{i}, 'type', cfg.type, 'res', 1, 'hour', ii, 'steps', 50, 'levels', 10, 'debug', 0, 'tType', 1);
                     if ~isempty(xdata{ii})
                        alpha{ii} = f_runTestAlpha(xdata{ii}, settings, 'sdate', tdates{i}, 'edate', tdates{i},'hour', ii, 'nTrials', 50, 'levels', 10);
                        if ~isempty(alpha{ii})
                            [conds{ii}, day2] = f_getConds(xdata{ii}, alpha{ii}, settings.data.instruments{ii}, day2, ii, 50);
                            if ~isempty(conds{ii}) && ~isempty(day2)
                                res.(['d_' tdates{i}]).vals{ii} = f_getTradePnL(xdata{ii}, conds{ii}, settings.data.instruments{ii}, settings.strategy, day2, ii, tdates{i}, 50); 
                            end
                            
                            [condsA{ii}, day2A] = f_getCondsA(alpha{ii}, settings.data.instruments{ii}, day2A, ii);
                            if ~isempty(condsA{ii}) && ~isempty(day2A)
                                contracts = f_getTcontracts(xdata{ii}, condsA{ii}, settings.data.instruments{ii}, day2A, ii); 
                                if ~isempty(fieldnames(contracts))
                                    [Events, subset] = f_getEvents(contracts); 
                                    Events2 = [Events.contract Events.index Events.bidRate Events.askRate Events.bidSize Events.askSize Events.trade Events.iEOD Events.time Events.snumber Events.bentry Events.bexit Events.sentry Events.sexit];
                                    Events = Events2; 
                                    clear Events2


-------------
function data = f_runTData(data, settings, varargin)

    cfg = inputParser;
    cfg.addOptional('sdate', @isstring);
    cfg.addOptional('edate', @isstring);
    cfg.addOptional('type', @isnumeric);
    cfg.addOptional('tType', @isnumeric);
    cfg.addOptional('hour', @isnumeric);
    cfg.addOptional('res', @isnumeric);
    cfg.addOptional('steps', @isnumeric);
    cfg.addOptional('levels', @isnumeric);
    cfg.addOptional('debug', @isnumeric);
    cfg.parse(varargin{:}); 
    cfg = cfg.Results;
       
   try
        if ~isempty(data)
            fnames = fieldnames(data);
            fnames2 = fieldnames(settings.data.instruments{cfg.hour});
            for i=1:length(fnames)
                if ismember(fnames{i}, fnames2)
                    if size(data.(fnames{i}).alpha,1)>cfg.steps
                        idx = (ismember(data.(fnames{i}).alphaR, settings.data.instruments{cfg.hour}.(fnames{i}).alphaR))';
                        idx_sel = find(idx==1);
                        % need to drop the linearly dependent alphas
                        if size(idx_sel,2)~=size(data.(fnames{i}).alpha,2)
                            idx_drop = ~ismember(1:size(data.(fnames{i}).alpha,2), idx_sel);
                            idx_dropped = find(idx_drop==1);
                            if ~isempty(idx_dropped)
%                                 for iD=1:length(idx_dropped)
%                                     fprintf(' the alpha %s is linearly dependent for %s, and hour %d, please check!! \n', data.(fnames{i}).alphaR{idx_dropped(iD)}, fnames{i}, cfg.hour);
%                                 end
                                % clean the linearly dependent alphas
                                data.(fnames{i}).alpha(:,idx_dropped)=[];
                                data.(fnames{i}).alphaR(idx_dropped)=[]; % length are different, keep the original one for unit test
                            end
                            if ~isequal(data.(fnames{i}).alphaR, settings.data.instruments{cfg.hour}.(fnames{i}).alphaR)
                                [~,idx_m] = ismember(settings.data.instruments{cfg.hour}.(fnames{i}).alphaR, data.(fnames{i}).alphaR);
                                data.(fnames{i}).alphaR = data.(fnames{i}).alphaR(idx_m);
                                data.(fnames{i}).alpha=data.(fnames{i}).alpha(:,idx_m);
                                assert((isequal(data.(fnames{i}).alphaR,settings.data.instruments{cfg.hour}.(fnames{i}).alphaR)~=1==1)==0, 'settings and data alphaR sequence different %s hour %d, please check!! ',fnames{i}, cfg.hour);
                            end
                        end                     
                    else
                        fprintf('%s has less data with steps %d for dates %s and %s, and hour %d, please check!! \n', fnames{i},cfg.steps, cfg.sdate, cfg.edate, cfg.hour);
                    end
                else
                    data = rmfield(data, fnames{i});
                    fprintf('%s not exist in settings for steps %d for dates %s and %s, and hour %d, please check!! \n', fnames{i},cfg.steps, cfg.sdate, cfg.edate, cfg.hour);
                end
            end 
        else
            fprintf('%s is empty for dates %s and %s, and hour %d, please check!! \n', settings.data.main_ptf, cfg.sdate, cfg.edate, cfg.hour);
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
    munlock f_runTData;
    clear cfg fnames fnames2 idx idx_sel idx_drop idx_dropped idx_m mex functions;  
end






-------------------

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

-------------------------------------------
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
-------------------------------
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
-------------------------------------