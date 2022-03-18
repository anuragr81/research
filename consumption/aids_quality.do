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
* constraint define 91 [qtransport]lpnonfresh  +[qtransport]lpdensefoods +[qtransport]lpcomplements +[qtransport]lpfruitsveg  +[qtransport]lpprotein +[qtransport]lpenergy +[qtransport]lphousehold +[qtransport]lptransport = 0
* constraint define 92 [qnonfresh]hh_education_rank+[qVnonfresh]hh_education_rank+[qdensefoods]hh_education_rank+[qVdensefoods]hh_education_rank+[qcomplements]hh_education_rank+[qVcomplements]hh_education_rank+[qfruitsveg]hh_education_rank+[qVfruitsveg]hh_education_rank+[qprotein]hh_education_rank+[qVprotein]hh_education_rank+[qenergy]hh_education_rank+[qhousehold]hh_education_rank+[qtransport]hh_education_rank+[qnonfresh]hh_age+[qVnonfresh]hh_age+[qdensefoods]hh_age+[qVdensefoods]hh_age+[qcomplements]hh_age+[qVcomplements]hh_age+[qfruitsveg]hh_age+[qVfruitsveg]hh_age+[qprotein]hh_age+[qVprotein]hh_age+[qenergy]hh_age+[qhousehold]hh_age+[qtransport]hh_age+[qnonfresh]hh_occupation_rank+[qVnonfresh]hh_occupation_rank+[qdensefoods]hh_occupation_rank+[qVdensefoods]hh_occupation_rank+[qcomplements]hh_occupation_rank+[qVcomplements]hh_occupation_rank+[qfruitsveg]hh_occupation_rank+[qVfruitsveg]hh_occupation_rank+[qprotein]hh_occupation_rank+[qVprotein]hh_occupation_rank+[qenergy]hh_occupation_rank+[qhousehold]hh_occupation_rank+[qtransport]hh_occupation_rank+[qnonfresh]hsize+[qVnonfresh]hsize+[qdensefoods]hsize+[qVdensefoods]hsize+[qcomplements]hsize+[qVcomplements]hsize+[qfruitsveg]hsize+[qVfruitsveg]hsize+[qprotein]hsize+[qVprotein]hsize+[qenergy]hsize+[qhousehold]hsize+[qtransport]hsize+[qnonfresh]transport_assets_mtm+[qVnonfresh]transport_assets_mtm+[qdensefoods]transport_assets_mtm+[qVdensefoods]transport_assets_mtm+[qcomplements]transport_assets_mtm+[qVcomplements]transport_assets_mtm+[qfruitsveg]transport_assets_mtm+[qVfruitsveg]transport_assets_mtm+[qprotein]transport_assets_mtm+[qVprotein]transport_assets_mtm+[qenergy]transport_assets_mtm+[qhousehold]transport_assets_mtm+[qtransport]transport_assets_mtm + [qnonfresh]household_assets_mtm+[qVnonfresh]household_assets_mtm+[qdensefoods]household_assets_mtm+[qVdensefoods]household_assets_mtm+[qcomplements]household_assets_mtm+[qVcomplements]household_assets_mtm+[qfruitsveg]household_assets_mtm+[qVfruitsveg]household_assets_mtm+[qprotein]household_assets_mtm+[qVprotein]household_assets_mtm+[qenergy]household_assets_mtm+[qhousehold]household_assets_mtm+[qtransport]household_assets_mtm + [qnonfresh]electric_assets_mtm+[qVnonfresh]electric_assets_mtm+[qdensefoods]electric_assets_mtm+[qVdensefoods]electric_assets_mtm+[qcomplements]electric_assets_mtm+[qVcomplements]electric_assets_mtm+[qfruitsveg]electric_assets_mtm+[qVfruitsveg]electric_assets_mtm+[qprotein]electric_assets_mtm+[qVprotein]electric_assets_mtm+[qenergy]electric_assets_mtm+[qhousehold]electric_assets_mtm+[qtransport]electric_assets_mtm + [qnonfresh]expensiveregion+[qVnonfresh]expensiveregion+[qdensefoods]expensiveregion+[qVdensefoods]expensiveregion+[qcomplements]expensiveregion+[qVcomplements]expensiveregion+[qfruitsveg]expensiveregion+[qVfruitsveg]expensiveregion+[qprotein]expensiveregion+[qVprotein]expensiveregion+[qenergy]expensiveregion+[qhousehold]expensiveregion+[qtransport]expensiveregion = 0
constraint define 93 [qnonfresh]ln_tot_exp +[qVnonfresh]ln_tot_exp +[qdensefoods]ln_tot_exp +[qVdensefoods]ln_tot_exp +[qcomplements]ln_tot_exp +[qVcomplements]ln_tot_exp +[qfruitsveg]ln_tot_exp +[qVfruitsveg]ln_tot_exp +[qprotein]ln_tot_exp +[qVprotein]ln_tot_exp +[qenergy]ln_tot_exp +[qhousehold]ln_tot_exp +[qtransport]ln_tot_exp  =0


global demand1 "(qnonfresh: w_nonfresh  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand2 "(qVnonfresh: lnV_nonfresh  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand3 "(qdensefoods: w_densefoods  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand4 "(qVdensefoods: lnV_densefoods  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand5 "(qcomplements: w_complements  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand6 "(qVcomplements: lnV_complements  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand7 "(qfruitsveg: w_fruitsveg  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand8 "(qVfruitsveg: lnV_fruitsveg  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand9 "(qprotein: w_protein  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand10 "(qVprotein: lnV_protein  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand11 "(qenergy: w_energy  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
global demand12 "(qhousehold: w_household  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"
* global demand13 "(qtransport: w_transport  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy lphousehold lptransport)"


sureg $demand1 $demand2 $demand3 $demand4 $demand5 $demand6 $demand7 $demand8 $demand9 $demand10 $demand11 $demand12, const(2 4 6 7 8 10 11 12 25 27 29 31 32 33 44 46 48 49 50 59 61 62 63 70 71 72 76 77 78 79 80) isure corr

* sureg $demand1 $demand2 $demand3 $demand4 $demand5 $demand6 $demand7 $demand8 $demand9 $demand10 $demand11 $demand12 $demand13, const(2 4 6 7 8 10 11 12 25 27 29 31 32 33 44 46 48 49 50 59 61 62 63 70 71 72 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93) isure corr
