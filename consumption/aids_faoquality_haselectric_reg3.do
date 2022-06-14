constraint define 2 [qfat]lpmeatsproteins=[qmeatsproteins]lpfat
constraint define 4 [qfat]lpcereals=[qcereals]lpfat

constraint define 10 [qfat]lpstarches=[qstarches]lpfat
constraint define 12 [qfat]lpcomplements=[qcomplements]lpfat
constraint define 14 [qfat]lptubers=[qtubers]lpfat
constraint define 16 [qfat]lpfruits=[qfruits]lpfat
constraint define 18 [qfat]lpfish=[qfish]lpfat
constraint define 39 [qmeatsproteins]lpcereals=[qcereals]lpmeatsproteins

constraint define 45 [qmeatsproteins]lpstarches=[qstarches]lpmeatsproteins
constraint define 47 [qmeatsproteins]lpcomplements=[qcomplements]lpmeatsproteins
constraint define 49 [qmeatsproteins]lptubers=[qtubers]lpmeatsproteins
constraint define 51 [qmeatsproteins]lpfruits=[qfruits]lpmeatsproteins
constraint define 53 [qmeatsproteins]lpfish=[qfish]lpmeatsproteins

constraint define 76 [qcereals]lpstarches=[qstarches]lpcereals
constraint define 78 [qcereals]lpcomplements=[qcomplements]lpcereals
constraint define 80 [qcereals]lptubers=[qtubers]lpcereals
constraint define 82 [qcereals]lpfruits=[qfruits]lpcereals
constraint define 84 [qcereals]lpfish=[qfish]lpcereals
constraint define 147 [qstarches]lpcomplements=[qcomplements]lpstarches
constraint define 149 [qstarches]lptubers=[qtubers]lpstarches
constraint define 151 [qstarches]lpfruits=[qfruits]lpstarches
constraint define 153 [qstarches]lpfish=[qfish]lpstarches
constraint define 164 [qcomplements]lptubers=[qtubers]lpcomplements
constraint define 166 [qcomplements]lpfruits=[qfruits]lpcomplements
constraint define 168 [qcomplements]lpfish=[qfish]lpcomplements
constraint define 177 [qtubers]lpfruits=[qfruits]lptubers
constraint define 179 [qtubers]lpfish=[qfish]lptubers
constraint define 186 [qfruits]lpfish=[qfish]lpfruits

constraint define 190 [qfat]lpmilk=[qmilk]lpfat
constraint define 191 [qmeatsproteins]lpmilk=[qmilk]lpmeatsproteins
constraint define 192 [qcereals]lpmilk=[qmilk]lpcereals
constraint define 193 [qveg]lpmilk=[qmilk]lpveg
constraint define 194 [qmilk]lpstarches=[qstarches]lpmilk
constraint define 195 [qmilk]lpcomplements=[qcomplements]lpmilk
constraint define 196 [qmilk]lptubers=[qtubers]lpmilk
constraint define 197 [qmilk]lpfruits=[qfruits]lpmilk
constraint define 198 [qmilk]lpfish=[qfish]lpmilk


*constraint define 200 [qfat]lpfat +[qfat]lpmeatsproteins  +[qfat]lpcereals +[qfat]lpveg  +[qfat]lpmilk + [qfat]lpstarches + [qfat]lpcomplements +[qfat]lptubers +[qfat]lpfruits +[qfat]lpfish  = 0
*constraint define 201 [qmeatsproteins]lpfat +[qmeatsproteins]lpmeatsproteins  +[qmeatsproteins]lpcereals +[qmeatsproteins]lpveg  +[qmeatsproteins]lpmilk + [qmeatsproteins]lpstarches + [qmeatsproteins]lpcomplements +[qmeatsproteins]lptubers +[qmeatsproteins]lpfruits +[qmeatsproteins]lpfish  = 0
*constraint define 202 [qcereals]lpfat +[qcereals]lpmeatsproteins  +[qcereals]lpcereals +[qcereals]lpveg  +[qcereals]lpmilk + [qcereals]lpstarches + [qcereals]lpcomplements +[qcereals]lptubers +[qcereals]lpfruits +[qcereals]lpfish  = 0
*constraint define 203 [qveg]lpfat +[qveg]lpmeatsproteins  +[qveg]lpcereals +[qveg]lpveg  +[qveg]lpmilk + [qveg]lpstarches + [qveg]lpcomplements +[qveg]lptubers +[qveg]lpfruits +[qveg]lpfish  = 0
*constraint define 204 [qstarches]lpfat +[qstarches]lpmeatsproteins  +[qstarches]lpcereals +[qstarches]lpveg  +[qstarches]lpmilk + [qstarches]lpstarches + [qstarches]lpcomplements +[qstarches]lptubers +[qstarches]lpfruits +[qstarches]lpfish  = 0
*constraint define 205 [qcomplements]lpfat +[qcomplements]lpmeatsproteins  +[qcomplements]lpcereals +[qcomplements]lpveg  +[qcomplements]lpmilk + [qcomplements]lpstarches + [qcomplements]lpcomplements +[qcomplements]lptubers +[qcomplements]lpfruits +[qcomplements]lpfish  = 0
*constraint define 206 [qtubers]lpfat +[qtubers]lpmeatsproteins  +[qtubers]lpcereals +[qtubers]lpveg  +[qtubers]lpmilk + [qtubers]lpstarches + [qtubers]lpcomplements +[qtubers]lptubers +[qtubers]lpfruits +[qtubers]lpfish  = 0
*constraint define 207 [qfruits]lpfat +[qfruits]lpmeatsproteins  +[qfruits]lpcereals +[qfruits]lpveg  +[qfruits]lpmilk + [qfruits]lpstarches + [qfruits]lpcomplements +[qfruits]lptubers +[qfruits]lpfruits +[qfruits]lpfish  = 0
*constraint define 208 [qfish]lpfat +[qfish]lpmeatsproteins  +[qfish]lpcereals +[qfish]lpveg  +[qfish]lpmilk + [qfish]lpstarches + [qfish]lpcomplements +[qfish]lptubers +[qfish]lpfruits +[qfish]lpfish  = 0


* Adding up won't be applied (because of singularity problems) as it is indirectly implied. Only homogeneity and symmetry are applied and mentioned.
global demand1 "(qfat: w_fat  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand2 "(qVfat: lnV_fat  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand3 "(qmeatsproteins: w_meatsproteins  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand4 "(qVmeatsproteins: lnV_meatsproteins  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand5 "(qcereals: w_cereals  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand6 "(qVcereals: lnV_cereals  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand7 "(qveg: w_veg  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand8 "(qVveg: lnV_veg  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand11 "(qmilk: w_milk  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand11 "(qstarches: w_starches  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand12 "(qVstarches: lnV_starches  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand13 "(qcomplements: w_complements  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand14 "(qVcomplements: lnV_complements  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand15 "(qtubers: w_tubers  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand16 "(qVtubers: lnV_tubers  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand17 "(qfruits: w_fruits  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand18 "(qVfruits: lnV_fruits  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand19 "(qfish: w_fish  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand20 "(qVfish: lnV_fish  ln_tot_exp hsize hh_age i.expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
*global enlist "ln_tot_exp"
*global exlist "hh_education_rank"

reg3 $demand1 $demand2 $demand3 $demand4 $demand5 $demand6 $demand7 $demand8 $demand11 $demand12 $demand13 $demand14 $demand15 $demand16 $demand17 $demand18 $demand19 $demand20, const(2 4 10 12 14 16 18 39  45 47 49 51 53 76 78 80 82 84 147 149 151 153 164 166 168 177 179 186 190 191 192 193 194 195 196 197 198) ireg3

* sureg $demand1 $demand2 $demand3 $demand4 $demand5 $demand6 $demand7 $demand8 $demand11 $demand12 $demand13 $demand14 $demand15 $demand16 $demand17 $demand18 $demand19 $demand20, const(2 4 10 12 14 16 18 39  45 47 49 51 53 76 78 80 82 84 147 149 151 153 164 166 168 177 179 186 190 191 192 193 194 195 196 197 198)  isure corr


* 200 201 202 203 204 205 206 207 208
*  endog($enlist) exog($exlist)
