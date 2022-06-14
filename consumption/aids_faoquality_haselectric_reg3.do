 use "C:\local_files\research\consumption\lsms\data\faodf2014.dta", clear


constraint define 2 [qfat]lpmeatsproteins=[qmeatsproteins]lpfat
constraint define 4 [qfat]lpcereals=[qcereals]lpfat
constraint define 6 [qfat]lpveg=[qveg]lpfat
constraint define 8 [qfat]lpmilk=[qmilk]lpfat
constraint define 10 [qfat]lpstarches=[qstarches]lpfat
constraint define 12 [qfat]lpcomplements=[qcomplements]lpfat
constraint define 14 [qfat]lptubers=[qtubers]lpfat
constraint define 16 [qfat]lpfruits=[qfruits]lpfat
constraint define 18 [qfat]lpfish=[qfish]lpfat
constraint define 39 [qmeatsproteins]lpcereals=[qcereals]lpmeatsproteins
constraint define 41 [qmeatsproteins]lpveg=[qveg]lpmeatsproteins
constraint define 43 [qmeatsproteins]lpmilk=[qmilk]lpmeatsproteins
constraint define 45 [qmeatsproteins]lpstarches=[qstarches]lpmeatsproteins
constraint define 47 [qmeatsproteins]lpcomplements=[qcomplements]lpmeatsproteins
constraint define 49 [qmeatsproteins]lptubers=[qtubers]lpmeatsproteins
constraint define 51 [qmeatsproteins]lpfruits=[qfruits]lpmeatsproteins
constraint define 53 [qmeatsproteins]lpfish=[qfish]lpmeatsproteins
constraint define 72 [qcereals]lpveg=[qveg]lpcereals
constraint define 74 [qcereals]lpmilk=[qmilk]lpcereals
constraint define 76 [qcereals]lpstarches=[qstarches]lpcereals
constraint define 78 [qcereals]lpcomplements=[qcomplements]lpcereals
constraint define 80 [qcereals]lptubers=[qtubers]lpcereals
constraint define 82 [qcereals]lpfruits=[qfruits]lpcereals
constraint define 84 [qcereals]lpfish=[qfish]lpcereals
constraint define 101 [qveg]lpmilk=[qmilk]lpveg
constraint define 103 [qveg]lpstarches=[qstarches]lpveg
constraint define 105 [qveg]lpcomplements=[qcomplements]lpveg
constraint define 107 [qveg]lptubers=[qtubers]lpveg
constraint define 109 [qveg]lpfruits=[qfruits]lpveg
constraint define 111 [qveg]lpfish=[qfish]lpveg
constraint define 126 [qmilk]lpstarches=[qstarches]lpmilk
constraint define 128 [qmilk]lpcomplements=[qcomplements]lpmilk
constraint define 130 [qmilk]lptubers=[qtubers]lpmilk
constraint define 132 [qmilk]lpfruits=[qfruits]lpmilk
constraint define 134 [qmilk]lpfish=[qfish]lpmilk
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

constraint define 191 [qfat]lpfat +[qfat]lpmeatsproteins  +[qfat]lpcereals  +[qfat]lpveg  +[qfat]lpmilk  +[qfat]lpstarches  +[qfat]lpcomplements  +[qfat]lptubers  +[qfat]lpfruits  +[qfat]lpfish  = 0
constraint define 192 [qVfat]lpfat  +[qVfat]lpmeatsproteins  +[qVfat]lpcereals  +[qVfat]lpveg  +[qVfat]lpmilk  +[qVfat]lpstarches  +[qVfat]lpcomplements  +[qVfat]lptubers  +[qVfat]lpfruits  +[qVfat]lpfish  = 0
constraint define 193 [qmeatsproteins]lpfat  +[qmeatsproteins]lpmeatsproteins  +[qmeatsproteins]lpcereals  +[qmeatsproteins]lpveg  +[qmeatsproteins]lpmilk  +[qmeatsproteins]lpstarches  +[qmeatsproteins]lpcomplements  +[qmeatsproteins]lptubers  +[qmeatsproteins]lpfruits  +[qmeatsproteins]lpfish  = 0
constraint define 194 [qVmeatsproteins]lpfat  +[qVmeatsproteins]lpmeatsproteins  +[qVmeatsproteins]lpcereals  +[qVmeatsproteins]lpveg  +[qVmeatsproteins]lpmilk  +[qVmeatsproteins]lpstarches  +[qVmeatsproteins]lpcomplements  +[qVmeatsproteins]lptubers  +[qVmeatsproteins]lpfruits  +[qVmeatsproteins]lpfish  = 0
constraint define 195 [qcereals]lpfat  +[qcereals]lpmeatsproteins  +[qcereals]lpcereals  +[qcereals]lpveg  +[qcereals]lpmilk  +[qcereals]lpstarches  +[qcereals]lpcomplements  +[qcereals]lptubers  +[qcereals]lpfruits  +[qcereals]lpfish  = 0
constraint define 196 [qVcereals]lpfat  +[qVcereals]lpmeatsproteins  +[qVcereals]lpcereals  +[qVcereals]lpveg  +[qVcereals]lpmilk  +[qVcereals]lpstarches  +[qVcereals]lpcomplements  +[qVcereals]lptubers  +[qVcereals]lpfruits  +[qVcereals]lpfish  = 0
constraint define 197 [qveg]lpfat  +[qveg]lpmeatsproteins  +[qveg]lpcereals  +[qveg]lpveg  +[qveg]lpmilk  +[qveg]lpstarches  +[qveg]lpcomplements  +[qveg]lptubers  +[qveg]lpfruits  +[qveg]lpfish  = 0
constraint define 198 [qVveg]lpfat  +[qVveg]lpmeatsproteins  +[qVveg]lpcereals  +[qVveg]lpveg  +[qVveg]lpmilk  +[qVveg]lpstarches  +[qVveg]lpcomplements  +[qVveg]lptubers  +[qVveg]lpfruits  +[qVveg]lpfish  = 0
constraint define 199 [qmilk]lpfat  +[qmilk]lpmeatsproteins  +[qmilk]lpcereals  +[qmilk]lpveg  +[qmilk]lpmilk  +[qmilk]lpstarches  +[qmilk]lpcomplements  +[qmilk]lptubers  +[qmilk]lpfruits  +[qmilk]lpfish  = 0
constraint define 200 [qVmilk]lpfat  +[qVmilk]lpmeatsproteins  +[qVmilk]lpcereals  +[qVmilk]lpveg  +[qVmilk]lpmilk  +[qVmilk]lpstarches  +[qVmilk]lpcomplements  +[qVmilk]lptubers  +[qVmilk]lpfruits  +[qVmilk]lpfish  = 0
constraint define 201 [qstarches]lpfat  +[qstarches]lpmeatsproteins  +[qstarches]lpcereals  +[qstarches]lpveg  +[qstarches]lpmilk  +[qstarches]lpstarches  +[qstarches]lpcomplements  +[qstarches]lptubers  +[qstarches]lpfruits  +[qstarches]lpfish  = 0
constraint define 202 [qVstarches]lpfat  +[qVstarches]lpmeatsproteins  +[qVstarches]lpcereals  +[qVstarches]lpveg  +[qVstarches]lpmilk  +[qVstarches]lpstarches  +[qVstarches]lpcomplements  +[qVstarches]lptubers  +[qVstarches]lpfruits  +[qVstarches]lpfish  = 0
constraint define 203 [qcomplements]lpfat  +[qcomplements]lpmeatsproteins  +[qcomplements]lpcereals  +[qcomplements]lpveg  +[qcomplements]lpmilk  +[qcomplements]lpstarches  +[qcomplements]lpcomplements  +[qcomplements]lptubers  +[qcomplements]lpfruits  +[qcomplements]lpfish  = 0
constraint define 204 [qVcomplements]lpfat  +[qVcomplements]lpmeatsproteins  +[qVcomplements]lpcereals  +[qVcomplements]lpveg  +[qVcomplements]lpmilk  +[qVcomplements]lpstarches  +[qVcomplements]lpcomplements  +[qVcomplements]lptubers  +[qVcomplements]lpfruits  +[qVcomplements]lpfish  = 0
constraint define 205 [qtubers]lpfat  +[qtubers]lpmeatsproteins  +[qtubers]lpcereals  +[qtubers]lpveg  +[qtubers]lpmilk  +[qtubers]lpstarches  +[qtubers]lpcomplements  +[qtubers]lptubers  +[qtubers]lpfruits  +[qtubers]lpfish  = 0
constraint define 206 [qVtubers]lpfat  +[qVtubers]lpmeatsproteins  +[qVtubers]lpcereals  +[qVtubers]lpveg  +[qVtubers]lpmilk  +[qVtubers]lpstarches  +[qVtubers]lpcomplements  +[qVtubers]lptubers  +[qVtubers]lpfruits  +[qVtubers]lpfish  = 0
constraint define 207 [qfruits]lpfat  +[qfruits]lpmeatsproteins  +[qfruits]lpcereals  +[qfruits]lpveg  +[qfruits]lpmilk  +[qfruits]lpstarches  +[qfruits]lpcomplements  +[qfruits]lptubers  +[qfruits]lpfruits  +[qfruits]lpfish  = 0
constraint define 208 [qVfruits]lpfat  +[qVfruits]lpmeatsproteins  +[qVfruits]lpcereals  +[qVfruits]lpveg  +[qVfruits]lpmilk  +[qVfruits]lpstarches  +[qVfruits]lpcomplements  +[qVfruits]lptubers  +[qVfruits]lpfruits  +[qVfruits]lpfish  = 0
constraint define 209 [qfish]lpfat  +[qfish]lpmeatsproteins  +[qfish]lpcereals  +[qfish]lpveg  +[qfish]lpmilk  +[qfish]lpstarches  +[qfish]lpcomplements  +[qfish]lptubers  +[qfish]lpfruits  +[qfish]lpfish  = 0
constraint define 210 [qVfish]lpfat  +[qVfish]lpmeatsproteins  +[qVfish]lpcereals  +[qVfish]lpveg  +[qVfish]lpmilk  +[qVfish]lpstarches  +[qVfish]lpcomplements  +[qVfish]lptubers  +[qVfish]lpfruits  +[qVfish]lpfish  = 0

* Only homogeneity and symmetry are applied and mentioned.

* constraint define 212 [qfat]ln_tot_exp +[qVfat]ln_tot_exp +[qmeatsproteins]ln_tot_exp +[qVmeatsproteins]ln_tot_exp +[qcereals]ln_tot_exp +[qVcereals]ln_tot_exp +[qveg]ln_tot_exp +[qVveg]ln_tot_exp +[qmilk]ln_tot_exp +[qVmilk]ln_tot_exp +[qstarches]ln_tot_exp +[qVstarches]ln_tot_exp +[qcomplements]ln_tot_exp +[qVcomplements]ln_tot_exp +[qtubers]ln_tot_exp +[qVtubers]ln_tot_exp +[qfruits]ln_tot_exp +[qVfruits]ln_tot_exp +[qfish]ln_tot_exp +[qVfish]ln_tot_exp  =0

global demand1 "(qfat: w_fat  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand2 "(qVfat: lnV_fat  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand3 "(qmeatsproteins: w_meatsproteins  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand4 "(qVmeatsproteins: lnV_meatsproteins  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand5 "(qcereals: w_cereals  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand6 "(qVcereals: lnV_cereals  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand7 "(qveg: w_veg  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand8 "(qVveg: lnV_veg  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand9 "(qmilk: w_milk  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand10 "(qVmilk: lnV_milk  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand11 "(qstarches: w_starches  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand12 "(qVstarches: lnV_starches  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand13 "(qcomplements: w_complements  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand14 "(qVcomplements: lnV_complements  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand15 "(qtubers: w_tubers  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand16 "(qVtubers: lnV_tubers  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand17 "(qfruits: w_fruits  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand18 "(qVfruits: lnV_fruits  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand19 "(qfish: w_fish  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand20 "(qVfish: lnV_fish  ln_tot_exp hsize hh_age expensiveregion has_electric lpfat lpmeatsproteins  lpcereals lpveg lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"


*global enlist "ln_tot_exp has_electric"
*global exlist "hh_education_rank"

* WORKED
* reg3 $demand1 $demand2 $demand3 $demand4 $demand5 $demand6 $demand7 $demand8 $demand9 $demand10 $demand11 $demand12 $demand13 $demand14 $demand15 $demand16 $demand17 $demand18 $demand19 $demand20, const(2 4 6 8 10 12 14 16 18 39 41 43 45 47 49 51 53 72 74 76 78 80 82 84 101 103 105 107 109 111 126 128 130 132 134 147 149 151 153 164 166 168 177 179 186 212) ireg3

reg3 $demand1 $demand2 $demand3 $demand4 $demand5 $demand6 $demand7 $demand8 $demand9 $demand10 $demand11 $demand12 $demand13 $demand14 $demand15 $demand16 $demand17 $demand18 $demand19 $demand20, const(2 4 6 8 10 12 14 16 18 39 41 43 45 47 49 51 53 72 74 76 78 80 82 84 101 103 105 107 109 111 126 128 130 132 134 147 149 151 153 164 166 168 177 179 186 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210) ireg3



* endog($enlist) exog($exlist)


