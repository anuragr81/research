************************************** r  **************************************

************************************* w_educ - r ****************************************

eststo clear

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3


eststo tnz_weduc_r , title (HH): quietly tobit w_educ logx r2012 log_mean_cost_ne i.agri i.educpriv   i.father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, ll(-7)


use "C:\local_files\research\consumption\lsms\data\tn_i_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3


eststo tnz_weduc_r_i , title (INDIV): quietly tobit w_educ_i logx r2012 log_mean_cost_ne i.agri i.educpriv  i.father_educ_rank secondary_schools i.current_educ_rank i.is_female  i.has_english numchild, ll(-7) 

************************************* w_educ URBAN-RURAL ****************************************


use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3

eststo tnz_weduc_urbanrural, title (HH): quietly tobit w_educ lnA0 log_mean_cost_ne i.agri i.educpriv   i.father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage rural_wards i.has_english numchild, ll(-7)

use "C:\local_files\research\consumption\lsms\data\tn_i_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3

eststo tnz_weduc_urbanrural_i, title (INDIV): quietly tobit w_educ_i lnA0 log_mean_cost_ne i.agri i.educpriv   i.father_educ_rank secondary_schools i.current_educ_rank i.is_female  rural_wards i.has_english numchild, ll(-7)

esttab tnz_weduc_r tnz_weduc_r_i tnz_weduc_urbanrural tnz_weduc_urbanrural_i using "c:/temp/tnz_r.tex", replace f booktabs no p nogaps compress mtitle mgroups("assetdensity" "urban-rural", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))



************************************* r_educ, r_agri  ****************************************
eststo clear

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3

eststo tnz_weduc_reduc, title (Educ HH): quietly tobit w_educ logx r_educ2012 log_mean_cost_ne i.agri i.educpriv i.father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, ll(-7)

use "C:\local_files\research\consumption\lsms\data\tn_i_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3
eststo tnz_weduc_reduc_i, title (Educ INDIV): quietly tobit w_educ_i logx r_educ2012 log_mean_cost_ne i.agri i.educpriv i.father_educ_rank secondary_schools i.current_educ_rank i.is_female   i.has_english numchild, ll(-7)

*********************************


use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3

eststo tnz_weduc_roccup, title (Agri HH): quietly tobit w_educ logx r_agri2012 log_mean_cost_ne i.educpriv  i.father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, ll(-7)


use "C:\local_files\research\consumption\lsms\data\tn_i_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3
eststo tnz_weduc_roccup_i, title (Agri INDIV): quietly tobit w_educ_i logx r_agri2012 log_mean_cost_ne i.educpriv  i.father_educ_rank secondary_schools i.current_educ_rank i.is_female   i.has_english numchild, ll(-7)


esttab tnz_weduc_reduc tnz_weduc_reduc_i tnz_weduc_roccup tnz_weduc_roccup_i using "c:/temp/tnz_reducoccup.tex", replace f booktabs no p nogaps compress mtitle mgroups("Educ" "Occup", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))


*********************** above and below median ****************************************************************

eststo clear

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3

drop if lnA0 < 12.75996   

eststo tnz_weduc_abovemed, title (Above Median HH): quietly tobit w_educ logx r2012 log_mean_cost_ne i.agri i.educpriv   i.father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, ll(-7)

use "C:\local_files\research\consumption\lsms\data\tn_i_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3
drop if lnA0 <  11.0021     

eststo tnz_weduc_abovemed_i, title (Above Median INDIV): quietly tobit w_educ_i logx r2012 log_mean_cost_ne i.agri i.educpriv   i.father_educ_rank secondary_schools i.current_educ_rank i.is_female   i.has_english numchild, ll(-7)

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3

drop if lnA0 >= 12.75996   

eststo tnz_weduc_belowmed, title (Below Median HH): quietly tobit w_educ logx r2012 log_mean_cost_ne i.agri i.educpriv   i.father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, ll(-7)

use "C:\local_files\research\consumption\lsms\data\tn_i_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3
drop if lnA0 >=  11.0021     

eststo tnz_weduc_belowmed_i, title (Below Median INDIV): quietly tobit w_educ_i logx r2012 log_mean_cost_ne i.agri i.educpriv   i.father_educ_rank secondary_schools i.current_educ_rank i.is_female   i.has_english numchild, ll(-7)

esttab tnz_weduc_abovemed tnz_weduc_abovemed_i tnz_weduc_belowmed tnz_weduc_belowmed_i using "c:/temp/tnz_abovebelowmedian.tex", replace f booktabs no p nogaps compress mtitle mgroups("Above Median" "Below Median", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))