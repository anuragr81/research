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


sureg $demand1 $demand2 $demand3 $demand4 $demand5 $demand6 $demand7 $demand8 $demand9 $demand10 $demand11 $demand12, const(2 4 6 8 10 25 27 29 31 44 46 48 59 61 ) isure corr

