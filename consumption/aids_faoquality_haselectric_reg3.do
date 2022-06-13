constraint define 2 [qfat]lpmeatsproteins=[qmeatsproteins]lpfat
constraint define 4 [qfat]lpcereals=[qcereals]lpfat
constraint define 8 [qfat]lpmilk=[qmilk]lpfat
constraint define 10 [qfat]lpstarches=[qstarches]lpfat
constraint define 12 [qfat]lpcomplements=[qcomplements]lpfat
constraint define 14 [qfat]lptubers=[qtubers]lpfat
constraint define 16 [qfat]lpfruits=[qfruits]lpfat
constraint define 18 [qfat]lpfish=[qfish]lpfat
constraint define 39 [qmeatsproteins]lpcereals=[qcereals]lpmeatsproteins
constraint define 43 [qmeatsproteins]lpmilk=[qmilk]lpmeatsproteins
constraint define 45 [qmeatsproteins]lpstarches=[qstarches]lpmeatsproteins
constraint define 47 [qmeatsproteins]lpcomplements=[qcomplements]lpmeatsproteins
constraint define 49 [qmeatsproteins]lptubers=[qtubers]lpmeatsproteins
constraint define 51 [qmeatsproteins]lpfruits=[qfruits]lpmeatsproteins
constraint define 53 [qmeatsproteins]lpfish=[qfish]lpmeatsproteins
constraint define 74 [qcereals]lpmilk=[qmilk]lpcereals
constraint define 76 [qcereals]lpstarches=[qstarches]lpcereals
constraint define 78 [qcereals]lpcomplements=[qcomplements]lpcereals
constraint define 80 [qcereals]lptubers=[qtubers]lpcereals
constraint define 82 [qcereals]lpfruits=[qfruits]lpcereals
constraint define 84 [qcereals]lpfish=[qfish]lpcereals
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
* Adding up won't be applied (because of singularity problems) as it is indirectly implied. Only homogeneity and symmetry are applied and mentioned.
global demand1 "(qfat: w_fat  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand2 "(qVfat: lnV_fat  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand3 "(qmeatsproteins: w_meatsproteins  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand4 "(qVmeatsproteins: lnV_meatsproteins  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand5 "(qcereals: w_cereals  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand6 "(qVcereals: lnV_cereals  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand7 "(qveg: w_veg  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand8 "(qVveg: lnV_veg  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand9 "(qmilk: w_milk  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand10 "(qVmilk: lnV_milk  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand11 "(qstarches: w_starches  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand12 "(qVstarches: lnV_starches  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand13 "(qcomplements: w_complements  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand14 "(qVcomplements: lnV_complements  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand15 "(qtubers: w_tubers  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand16 "(qVtubers: lnV_tubers  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand17 "(qfruits: w_fruits  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand18 "(qVfruits: lnV_fruits  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand19 "(qfish: w_fish  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global demand20 "(qVfish: lnV_fish  ln_tot_exp hsize hh_age expensiveregion lpfat lpmeatsproteins lpcereals lpmilk lpstarches lpcomplements lptubers lpfruits lpfish )"
global enlist "ln_tot_exp has_electric"
global exlist "hh_education_rank"
reg3 $demand1 $demand2 $demand3 $demand4 $demand5 $demand6 $demand7 $demand8 $demand9 $demand10 $demand11 $demand12 $demand13 $demand14 $demand15 $demand16 $demand17 $demand18 $demand19 $demand20, const(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212) ireg3 endog($enlist) exog($exlist)
