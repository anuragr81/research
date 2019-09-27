constraint define 1 [qnonfresh]lpdensefoods=[qdensefoods]lpnonfresh
constraint define 2 [qnonfresh]lpcomplements=[qcomplements]lpnonfresh
constraint define 3 [qnonfresh]lpfruitsveg=[qfruitsveg]lpnonfresh
constraint define 4 [qnonfresh]lpprotein=[qprotein]lpnonfresh
constraint define 5 [qnonfresh]lphousehold=[qhousehold]lpnonfresh
constraint define 6 [qnonfresh]lptransport=[qtransport]lpnonfresh
constraint define 7 [qdensefoods]lpcomplements=[qcomplements]lpdensefoods
constraint define 8 [qdensefoods]lpfruitsveg=[qfruitsveg]lpdensefoods
constraint define 9 [qdensefoods]lpprotein=[qprotein]lpdensefoods
constraint define 10 [qdensefoods]lphousehold=[qhousehold]lpdensefoods
constraint define 12 [qdensefoods]lptransport=[qtransport]lpdensefoods
constraint define 13 [qcomplements]lpfruitsveg=[qfruitsveg]lpcomplements
constraint define 14 [qcomplements]lpprotein=[qprotein]lpcomplements
constraint define 15 [qcomplements]lphousehold=[qhousehold]lpcomplements
constraint define 16 [qcomplements]lptransport=[qtransport]lpcomplements
constraint define 17 [qfruitsveg]lpprotein=[qprotein]lpfruitsveg
constraint define 18 [qfruitsveg]lphousehold=[qhousehold]lpfruitsveg
constraint define 19 [qfruitsveg]lptransport=[qtransport]lpfruitsveg
constraint define 20 [qprotein]lphousehold=[qhousehold]lpprotein
constraint define 21 [qprotein]lptransport=[qtransport]lpprotein
constraint define 22 [qhousehold]lptransport=[qtransport]lphousehold
constraint define 23 [qnonfresh]lpnonfresh  +[qnonfresh]lpdensefoods +[qnonfresh]lpcomplements +[qnonfresh]lpfruitsveg +[qnonfresh]lpprotein  +[qnonfresh]lphousehold +[qnonfresh]lptransport = 0
constraint define 24 [qVnonfresh]lpnonfresh  +[qVnonfresh]lpdensefoods +[qVnonfresh]lpcomplements +[qVnonfresh]lpfruitsveg +[qVnonfresh]lpprotein  +[qVnonfresh]lphousehold +[qVnonfresh]lptransport = 0
constraint define 25 [qdensefoods]lpnonfresh  +[qdensefoods]lpdensefoods +[qdensefoods]lpcomplements +[qdensefoods]lpfruitsveg +[qdensefoods]lpprotein  +[qdensefoods]lphousehold +[qdensefoods]lptransport = 0
constraint define 26 [qVdensefoods]lpnonfresh  +[qVdensefoods]lpdensefoods +[qVdensefoods]lpcomplements +[qVdensefoods]lpfruitsveg +[qVdensefoods]lpprotein  +[qVdensefoods]lphousehold +[qVdensefoods]lptransport = 0
constraint define 27 [qcomplements]lpnonfresh  +[qcomplements]lpdensefoods +[qcomplements]lpcomplements +[qcomplements]lpfruitsveg +[qcomplements]lpprotein  +[qcomplements]lphousehold +[qcomplements]lptransport = 0
constraint define 28 [qVcomplements]lpnonfresh  +[qVcomplements]lpdensefoods +[qVcomplements]lpcomplements +[qVcomplements]lpfruitsveg +[qVcomplements]lpprotein  +[qVcomplements]lphousehold +[qVcomplements]lptransport = 0
constraint define 29 [qfruitsveg]lpnonfresh  +[qfruitsveg]lpdensefoods +[qfruitsveg]lpcomplements +[qfruitsveg]lpfruitsveg +[qfruitsveg]lpprotein  +[qfruitsveg]lphousehold +[qfruitsveg]lptransport = 0
constraint define 30 [qVfruitsveg]lpnonfresh  +[qVfruitsveg]lpdensefoods +[qVfruitsveg]lpcomplements +[qVfruitsveg]lpfruitsveg +[qVfruitsveg]lpprotein  +[qVfruitsveg]lphousehold +[qVfruitsveg]lptransport = 0
constraint define 31 [qprotein]lpnonfresh  +[qprotein]lpdensefoods +[qprotein]lpcomplements +[qprotein]lpfruitsveg +[qprotein]lpprotein  +[qprotein]lphousehold +[qprotein]lptransport = 0
constraint define 32 [qVprotein]lpnonfresh  +[qVprotein]lpdensefoods +[qVprotein]lpcomplements +[qVprotein]lpfruitsveg +[qVprotein]lpprotein  +[qVprotein]lphousehold +[qVprotein]lptransport = 0
constraint define 33 [qhousehold]lpnonfresh  +[qhousehold]lpdensefoods +[qhousehold]lpcomplements +[qhousehold]lpfruitsveg +[qhousehold]lpprotein  +[qhousehold]lphousehold +[qhousehold]lptransport = 0
constraint define 34 [qtransport]lpnonfresh  +[qtransport]lpdensefoods +[qtransport]lpcomplements +[qtransport]lpfruitsveg +[qtransport]lpprotein  +[qtransport]lphousehold +[qtransport]lptransport = 0
constraint define 35 [qnonfresh]educ_rank+[qVnonfresh]educ_rank+[qdensefoods]educ_rank+[qVdensefoods]educ_rank+[qcomplements]educ_rank+[qVcomplements]educ_rank+[qfruitsveg]educ_rank+[qVfruitsveg]educ_rank+[qprotein]educ_rank+[qVprotein]educ_rank+[qhousehold]educ_rank+[qtransport]educ_rank+[qnonfresh]age+[qVnonfresh]age+[qdensefoods]age+[qVdensefoods]age+[qcomplements]age+[qVcomplements]age+[qfruitsveg]age+[qVfruitsveg]age+[qprotein]age+[qVprotein]age+[qhousehold]age+[qtransport]age+[qnonfresh]occupation_rank+[qVnonfresh]occupation_rank+[qdensefoods]occupation_rank+[qVdensefoods]occupation_rank+[qcomplements]occupation_rank+[qVcomplements]occupation_rank+[qfruitsveg]occupation_rank+[qVfruitsveg]occupation_rank+[qprotein]occupation_rank+[qVprotein]occupation_rank+[qhousehold]occupation_rank+[qtransport]occupation_rank+[qnonfresh]consu+[qVnonfresh]consu+[qdensefoods]consu+[qVdensefoods]consu+[qcomplements]consu+[qVcomplements]consu+[qfruitsveg]consu+[qVfruitsveg]consu+[qprotein]consu+[qVprotein]consu+[qhousehold]consu+[qtransport]consu =0
constraint define 36 [qnonfresh]ln_tot_exp +[qVnonfresh]ln_tot_exp +[qdensefoods]ln_tot_exp +[qVdensefoods]ln_tot_exp +[qcomplements]ln_tot_exp +[qVcomplements]ln_tot_exp +[qfruitsveg]ln_tot_exp +[qVfruitsveg]ln_tot_exp +[qprotein]ln_tot_exp +[qVprotein]ln_tot_exp +[qhousehold]ln_tot_exp +[qtransport]ln_tot_exp  =0
global demand1 "(qnonfresh: w_nonfresh  ln_tot_exp educ_rank age occupation_rank consu lpnonfresh lpdensefoods lpcomplements lpfruitsveg lpprotein lphousehold lptransport)"
global demand2 "(qVnonfresh: lnV_nonfresh  ln_tot_exp educ_rank age occupation_rank consu lpnonfresh lpdensefoods lpcomplements lpfruitsveg lpprotein lphousehold lptransport)"
global demand3 "(qdensefoods: w_densefoods  ln_tot_exp educ_rank age occupation_rank consu lpnonfresh lpdensefoods lpcomplements lpfruitsveg lpprotein lphousehold lptransport)"
global demand4 "(qVdensefoods: lnV_densefoods  ln_tot_exp educ_rank age occupation_rank consu lpnonfresh lpdensefoods lpcomplements lpfruitsveg lpprotein lphousehold lptransport)"
global demand5 "(qcomplements: w_complements  ln_tot_exp educ_rank age occupation_rank consu lpnonfresh lpdensefoods lpcomplements lpfruitsveg lpprotein lphousehold lptransport)"
global demand6 "(qVcomplements: lnV_complements  ln_tot_exp educ_rank age occupation_rank consu lpnonfresh lpdensefoods lpcomplements lpfruitsveg lpprotein lphousehold lptransport)"
global demand7 "(qfruitsveg: w_fruitsveg  ln_tot_exp educ_rank age occupation_rank consu lpnonfresh lpdensefoods lpcomplements lpfruitsveg lpprotein lphousehold lptransport)"
global demand8 "(qVfruitsveg: lnV_fruitsveg  ln_tot_exp educ_rank age occupation_rank consu lpnonfresh lpdensefoods lpcomplements lpfruitsveg lpprotein lphousehold lptransport)"
global demand9 "(qprotein: w_protein  ln_tot_exp educ_rank age occupation_rank consu lpnonfresh lpdensefoods lpcomplements lpfruitsveg lpprotein lphousehold lptransport)"
global demand10 "(qVprotein: lnV_protein  ln_tot_exp educ_rank age occupation_rank consu lpnonfresh lpdensefoods lpcomplements lpfruitsveg lpprotein lphousehold lptransport)"
global demand11 "(qhousehold: w_household  ln_tot_exp educ_rank age occupation_rank consu lpnonfresh lpdensefoods lpcomplements lpfruitsveg lpprotein lphousehold lptransport)"
global demand12 "(qtransport: w_transport  ln_tot_exp educ_rank age occupation_rank consu lpnonfresh lpdensefoods lpcomplements lpfruitsveg lpprotein lphousehold lptransport)"
sureg $demand1 $demand2 $demand3 $demand4 $demand5 $demand6 $demand7 $demand8 $demand9 $demand10 $demand11 $demand12, const(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36) isure
