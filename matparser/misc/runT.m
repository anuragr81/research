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


