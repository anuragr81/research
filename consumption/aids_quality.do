constraint define 2 [qnonfresh]lpdensefoods=[qdensefoods]lpnonfresh
constraint define 4 [qnonfresh]lpcomplements=[qcomplements]lpnonfresh
constraint define 6 [qnonfresh]lpfruitsveg=[qfruitsveg]lpnonfresh
constraint define 8 [qnonfresh]lpprotein=[qprotein]lpnonfresh
constraint define 10 [qnonfresh]lpenergy=[qenergy]lpnonfresh
constraint define 11 [qnonfresh]lphousehold=[qhousehold]lpnonfresh
constraint define 12 [qnonfresh]lptransport=[qtransport]lpnonfresh
constraint define 25 [qdensefoods]lpcomplements=[qcomplements]lpdensefoods
constraint define 27 [qdensefoods]lpfruitsveg=[qfruitsveg]lpdensefoods
constraint define 29 [qdensefoods]lpprotein=[qprotein]lpdensefoods
constraint define 31 [qdensefoods]lpenergy=[qenergy]lpdensefoods
constraint define 32 [qdensefoods]lphousehold=[qhousehold]lpdensefoods
constraint define 33 [qdensefoods]lptransport=[qtransport]lpdensefoods
constraint define 44 [qcomplements]lpfruitsveg=[qfruitsveg]lpcomplements
constraint define 46 [qcomplements]lpprotein=[qprotein]lpcomplements
constraint define 48 [qcomplements]lpenergy=[qenergy]lpcomplements
constraint define 49 [qcomplements]lphousehold=[qhousehold]lpcomplements
constraint define 50 [qcomplements]lptransport=[qtransport]lpcomplements
constraint define 59 [qfruitsveg]lpprotein=[qprotein]lpfruitsveg
constraint define 61 [qfruitsveg]lpenergy=[qenergy]lpfruitsveg
constraint define 62 [qfruitsveg]lphousehold=[qhousehold]lpfruitsveg
constraint define 63 [qfruitsveg]lptransport=[qtransport]lpfruitsveg
constraint define 70 [qprotein]lpenergy=[qenergy]lpprotein
constraint define 71 [qprotein]lphousehold=[qhousehold]lpprotein
constraint define 72 [qprotein]lptransport=[qtransport]lpprotein
constraint define 76 [qenergy]lphousehold=[qhousehold]lpenergy
constraint define 77 [qenergy]lptransport=[qtransport]lpenergy
constraint define 78 [qhousehold]lptransport=[qtransport]lphousehold
constraint define 79 [qnonfresh]lpnonfresh  +[qnonfresh]lpdensefoods +[qnonfresh]lpcomplements +[qnonfresh]lpfruitsveg +[qnonfresh]lpprotein +[qnonfresh]lpenergy +[qnonfresh]lphousehold +[qnonfresh]lptransport = 0
constraint define 80 [qVnonfresh]lpnonfresh  +[qVnonfresh]lpdensefoods +[qVnonfresh]lpcomplements +[qVnonfresh]lpfruitsveg +[qVnonfresh]lpprotein +[qVnonfresh]lpenergy +[qVnonfresh]lphousehold +[qVnonfresh]lptransport = 0
constraint define 81 [qdensefoods]lpnonfresh  +[qdensefoods]lpdensefoods +[qdensefoods]lpcomplements +[qdensefoods]lpfruitsveg +[qdensefoods]lpprotein +[qdensefoods]lpenergy +[qdensefoods]lphousehold +[qdensefoods]lptransport = 0
constraint define 82 [qVdensefoods]lpnonfresh  +[qVdensefoods]lpdensefoods +[qVdensefoods]lpcomplements +[qVdensefoods]lpfruitsveg  +[qVdensefoods]lpprotein +[qVdensefoods]lpenergy +[qVdensefoods]lphousehold +[qVdensefoods]lptransport = 0
constraint define 83 [qcomplements]lpnonfresh  +[qcomplements]lpdensefoods +[qcomplements]lpcomplements +[qcomplements]lpfruitsveg  +[qcomplements]lpprotein +[qcomplements]lpenergy +[qcomplements]lphousehold +[qcomplements]lptransport = 0
constraint define 84 [qVcomplements]lpnonfresh  +[qVcomplements]lpdensefoods +[qVcomplements]lpcomplements +[qVcomplements]lpfruitsveg  +[qVcomplements]lpprotein +[qVcomplements]lpenergy +[qVcomplements]lphousehold +[qVcomplements]lptransport = 0
constraint define 85 [qfruitsveg]lpnonfresh  +[qfruitsveg]lpdensefoods +[qfruitsveg]lpcomplements +[qfruitsveg]lpfruitsveg  +[qfruitsveg]lpprotein +[qfruitsveg]lpenergy +[qfruitsveg]lphousehold +[qfruitsveg]lptransport = 0
constraint define 86 [qVfruitsveg]lpnonfresh  +[qVfruitsveg]lpdensefoods +[qVfruitsveg]lpcomplements +[qVfruitsveg]lpfruitsveg  +[qVfruitsveg]lpprotein +[qVfruitsveg]lpenergy +[qVfruitsveg]lphousehold +[qVfruitsveg]lptransport = 0
constraint define 87 [qprotein]lpnonfresh  +[qprotein]lpdensefoods +[qprotein]lpcomplements +[qprotein]lpfruitsveg  +[qprotein]lpprotein +[qprotein]lpenergy +[qprotein]lphousehold +[qprotein]lptransport = 0
constraint define 88 [qVprotein]lpnonfresh  +[qVprotein]lpdensefoods +[qVprotein]lpcomplements +[qVprotein]lpfruitsveg  +[qVprotein]lpprotein +[qVprotein]lpenergy +[qVprotein]lphousehold +[qVprotein]lptransport = 0
constraint define 89 [qenergy]lpnonfresh  +[qenergy]lpdensefoods +[qenergy]lpcomplements +[qenergy]lpfruitsveg  +[qenergy]lpprotein +[qenergy]lpenergy +[qenergy]lphousehold +[qenergy]lptransport = 0
constraint define 90 [qhousehold]lpnonfresh  +[qhousehold]lpdensefoods +[qhousehold]lpcomplements +[qhousehold]lpfruitsveg  +[qhousehold]lpprotein +[qhousehold]lpenergy +[qhousehold]lphousehold +[qhousehold]lptransport = 0
constraint define 91 [qtransport]lpnonfresh  +[qtransport]lpdensefoods +[qtransport]lpcomplements +[qtransport]lpfruitsveg  +[qtransport]lpprotein +[qtransport]lpenergy +[qtransport]lphousehold +[qtransport]lptransport = 0
constraint define 92 [qnonfresh]educ_rank+[qVnonfresh]educ_rank+[qdensefoods]educ_rank+[qVdensefoods]educ_rank+[qcomplements]educ_rank+[qVcomplements]educ_rank+[qfruitsveg]educ_rank+[qVfruitsveg]educ_rank+[qprotein]educ_rank+[qVprotein]educ_rank+[qenergy]educ_rank+[qhousehold]educ_rank+[qtransport]educ_rank+[qnonfresh]age+[qVnonfresh]age+[qdensefoods]age+[qVdensefoods]age+[qcomplements]age+[qVcomplements]age+[qfruitsveg]age+[qVfruitsveg]age+[qprotein]age+[qVprotein]age+[qenergy]age+[qhousehold]age+[qtransport]age+[qnonfresh]occupation_rank+[qVnonfresh]occupation_rank+[qdensefoods]occupation_rank+[qVdensefoods]occupation_rank+[qcomplements]occupation_rank+[qVcomplements]occupation_rank+[qfruitsveg]occupation_rank+[qVfruitsveg]occupation_rank+[qprotein]occupation_rank+[qVprotein]occupation_rank+[qenergy]occupation_rank+[qhousehold]occupation_rank+[qtransport]occupation_rank+[qnonfresh]consu+[qVnonfresh]consu+[qdensefoods]consu+[qVdensefoods]consu+[qcomplements]consu+[qVcomplements]consu+[qfruitsveg]consu+[qVfruitsveg]consu+[qprotein]consu+[qVprotein]consu+[qenergy]consu+[qhousehold]consu+[qtransport]consu+[qnonfresh]norm_trans_asset_score+[qVnonfresh]norm_trans_asset_score+[qdensefoods]norm_trans_asset_score+[qVdensefoods]norm_trans_asset_score+[qcomplements]norm_trans_asset_score+[qVcomplements]norm_trans_asset_score+[qfruitsveg]norm_trans_asset_score+[qVfruitsveg]norm_trans_asset_score+[qprotein]norm_trans_asset_score+[qVprotein]norm_trans_asset_score+[qenergy]norm_trans_asset_score+[qhousehold]norm_trans_asset_score+[qtransport]norm_trans_asset_score + [qnonfresh]norm_hh_asset_score+[qVnonfresh]norm_hh_asset_score+[qdensefoods]norm_hh_asset_score+[qVdensefoods]norm_hh_asset_score+[qcomplements]norm_hh_asset_score+[qVcomplements]norm_hh_asset_score+[qfruitsveg]norm_hh_asset_score+[qVfruitsveg]norm_hh_asset_score+[qprotein]norm_hh_asset_score+[qVprotein]norm_hh_asset_score+[qenergy]norm_hh_asset_score+[qhousehold]norm_hh_asset_score+[qtransport]norm_hh_asset_score + [qnonfresh]norm_elec_asset_score+[qVnonfresh]norm_elec_asset_score+[qdensefoods]norm_elec_asset_score+[qVdensefoods]norm_elec_asset_score+[qcomplements]norm_elec_asset_score+[qVcomplements]norm_elec_asset_score+[qfruitsveg]norm_elec_asset_score+[qVfruitsveg]norm_elec_asset_score+[qprotein]norm_elec_asset_score+[qVprotein]norm_elec_asset_score+[qenergy]norm_elec_asset_score+[qhousehold]norm_elec_asset_score+[qtransport]norm_elec_asset_score + [qnonfresh]expensiveregion+[qVnonfresh]expensiveregion+[qdensefoods]expensiveregion+[qVdensefoods]expensiveregion+[qcomplements]expensiveregion+[qVcomplements]expensiveregion+[qfruitsveg]expensiveregion+[qVfruitsveg]expensiveregion+[qprotein]expensiveregion+[qVprotein]expensiveregion+[qenergy]expensiveregion+[qhousehold]expensiveregion+[qtransport]expensiveregion = 0
constraint define 93 [qnonfresh]ln_tot_exp +[qVnonfresh]ln_tot_exp +[qdensefoods]ln_tot_exp +[qVdensefoods]ln_tot_exp +[qcomplements]ln_tot_exp +[qVcomplements]ln_tot_exp +[qfruitsveg]ln_tot_exp +[qVfruitsveg]ln_tot_exp +[qprotein]ln_tot_exp +[qVprotein]ln_tot_exp +[qenergy]ln_tot_exp +[qhousehold]ln_tot_exp +[qtransport]ln_tot_exp  =0


global demand1 "(qnonfresh: w_nonfresh  ln_tot_exp educ_rank age occupation_rank consu norm_trans_asset_score norm_hh_asset_score norm_elec_asset_score expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand2 "(qVnonfresh: lnV_nonfresh  ln_tot_exp educ_rank age occupation_rank consu norm_trans_asset_score norm_hh_asset_score norm_elec_asset_score expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand3 "(qdensefoods: w_densefoods  ln_tot_exp educ_rank age occupation_rank consu norm_trans_asset_score norm_hh_asset_score norm_elec_asset_score expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand4 "(qVdensefoods: lnV_densefoods  ln_tot_exp educ_rank age occupation_rank consu norm_trans_asset_score norm_hh_asset_score norm_elec_asset_score expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand5 "(qcomplements: w_complements  ln_tot_exp educ_rank age occupation_rank consu norm_trans_asset_score norm_hh_asset_score norm_elec_asset_score expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand6 "(qVcomplements: lnV_complements  ln_tot_exp educ_rank age occupation_rank consu norm_trans_asset_score norm_hh_asset_score norm_elec_asset_score expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand7 "(qfruitsveg: w_fruitsveg  ln_tot_exp educ_rank age occupation_rank consu norm_trans_asset_score norm_hh_asset_score norm_elec_asset_score expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand8 "(qVfruitsveg: lnV_fruitsveg  ln_tot_exp educ_rank age occupation_rank consu norm_trans_asset_score norm_hh_asset_score norm_elec_asset_score expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand9 "(qprotein: w_protein  ln_tot_exp educ_rank age occupation_rank consu norm_trans_asset_score norm_hh_asset_score norm_elec_asset_score expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand10 "(qVprotein: lnV_protein  ln_tot_exp educ_rank age occupation_rank consu norm_trans_asset_score norm_hh_asset_score norm_elec_asset_score expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand11 "(qenergy: w_energy  ln_tot_exp educ_rank age occupation_rank consu norm_trans_asset_score norm_hh_asset_score norm_elec_asset_score expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand12 "(qhousehold: w_household  ln_tot_exp educ_rank age occupation_rank consu norm_trans_asset_score norm_hh_asset_score norm_elec_asset_score expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand13 "(qtransport: w_transport  ln_tot_exp educ_rank age occupation_rank consu norm_trans_asset_score norm_hh_asset_score norm_elec_asset_score expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"

sureg $demand1 $demand2 $demand3 $demand4 $demand5 $demand6 $demand7 $demand8 $demand9 $demand10 $demand11 $demand12 $demand13, const(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93) isure
