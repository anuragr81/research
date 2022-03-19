constraint define 2 [qnonfresh]lpdensefoods=[qdensefoods]lpnonfresh
constraint define 4 [qnonfresh]lpcomplements=[qcomplements]lpnonfresh
constraint define 6 [qnonfresh]lpfruitsveg=[qfruitsveg]lpnonfresh
constraint define 8 [qnonfresh]lpprotein=[qprotein]lpnonfresh
constraint define 10 [qnonfresh]lpenergy=[qenergy]lpnonfresh


constraint define 25 [qdensefoods]lpcomplements=[qcomplements]lpdensefoods
constraint define 27 [qdensefoods]lpfruitsveg=[qfruitsveg]lpdensefoods
constraint define 29 [qdensefoods]lpprotein=[qprotein]lpdensefoods
constraint define 31 [qdensefoods]lpenergy=[qenergy]lpdensefoods


constraint define 44 [qcomplements]lpfruitsveg=[qfruitsveg]lpcomplements
constraint define 46 [qcomplements]lpprotein=[qprotein]lpcomplements
constraint define 48 [qcomplements]lpenergy=[qenergy]lpcomplements


constraint define 59 [qfruitsveg]lpprotein=[qprotein]lpfruitsveg
constraint define 61 [qfruitsveg]lpenergy=[qenergy]lpfruitsveg


constraint define 70 [qprotein]lpenergy=[qenergy]lpprotein

constraint define 79 [qnonfresh]lpnonfresh  +[qnonfresh]lpdensefoods +[qnonfresh]lpcomplements +[qnonfresh]lpfruitsveg +[qnonfresh]lpprotein +[qnonfresh]lpenergy = 0
 constraint define 80 [qVnonfresh]lpnonfresh  +[qVnonfresh]lpdensefoods +[qVnonfresh]lpcomplements +[qVnonfresh]lpfruitsveg +[qVnonfresh]lpprotein +[qVnonfresh]lpenergy = 0
 constraint define 81 [qdensefoods]lpnonfresh  +[qdensefoods]lpdensefoods +[qdensefoods]lpcomplements +[qdensefoods]lpfruitsveg +[qdensefoods]lpprotein +[qdensefoods]lpenergy  0
 constraint define 82 [qVdensefoods]lpnonfresh  +[qVdensefoods]lpdensefoods +[qVdensefoods]lpcomplements +[qVdensefoods]lpfruitsveg  +[qVdensefoods]lpprotein +[qVdensefoods]lpenergy  0
 constraint define 83 [qcomplements]lpnonfresh  +[qcomplements]lpdensefoods +[qcomplements]lpcomplements +[qcomplements]lpfruitsveg  +[qcomplements]lpprotein +[qcomplements]lpenergy = 0
 constraint define 84 [qVcomplements]lpnonfresh  +[qVcomplements]lpdensefoods +[qVcomplements]lpcomplements +[qVcomplements]lpfruitsveg  +[qVcomplements]lpprotein +[qVcomplements]lpenergy= 0
 constraint define 85 [qfruitsveg]lpnonfresh  +[qfruitsveg]lpdensefoods +[qfruitsveg]lpcomplements +[qfruitsveg]lpfruitsveg  +[qfruitsveg]lpprotein +[qfruitsveg]lpenergy = 0
 constraint define 86 [qVfruitsveg]lpnonfresh  +[qVfruitsveg]lpdensefoods +[qVfruitsveg]lpcomplements +[qVfruitsveg]lpfruitsveg  +[qVfruitsveg]lpprotein +[qVfruitsveg]lpenergy = 0
 constraint define 87 [qprotein]lpnonfresh  +[qprotein]lpdensefoods +[qprotein]lpcomplements +[qprotein]lpfruitsveg  +[qprotein]lpprotein +[qprotein]lpenergy = 0
 constraint define 88 [qVprotein]lpnonfresh  +[qVprotein]lpdensefoods +[qVprotein]lpcomplements +[qVprotein]lpfruitsveg  +[qVprotein]lpprotein +[qVprotein]lpenergy = 0
 constraint define 89 [qenergy]lpnonfresh  +[qenergy]lpdensefoods +[qenergy]lpcomplements +[qenergy]lpfruitsveg  +[qenergy]lpprotein +[qenergy]lpenergy = 0

* Adding up won't be applied (because of singularity problems) as it is indirectly implied. Only homogeneity and symmetry are applied and mentioned.


global demand1 "(qnonfresh: w_nonfresh  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy )"
global demand2 "(qVnonfresh: lnV_nonfresh  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy )"
global demand3 "(qdensefoods: w_densefoods  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy )"
global demand4 "(qVdensefoods: lnV_densefoods  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy )"
global demand5 "(qcomplements: w_complements  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy )"
global demand6 "(qVcomplements: lnV_complements  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy )"
global demand7 "(qfruitsveg: w_fruitsveg  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy )"
global demand8 "(qVfruitsveg: lnV_fruitsveg  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy )"
global demand9 "(qprotein: w_protein  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy )"
global demand10 "(qVprotein: lnV_protein  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy )"
global demand11 "(qenergy: w_energy  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy )"
global demand12 "(qhousehold: w_household  ln_tot_exp hh_education_rank hh_age hh_occupation_rank hsize transport_assets_mtm household_assets_mtm electric_assets_mtm expensiveregion lpnonfresh lpdensefoods  lpcomplements  lpfruitsveg  lpprotein lpenergy )"


sureg $demand1 $demand2 $demand3 $demand4 $demand5 $demand6 $demand7 $demand8 $demand9 $demand10 $demand11 $demand12, const(2 4 6 8 10 25 27 29 31 44 46 48 59 61 70 79 80 81) isure corr

