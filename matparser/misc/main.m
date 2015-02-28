
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
