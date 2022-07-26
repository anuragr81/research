************************************** r  **************************************

************************************* w_educ - r ****************************************

eststo clear

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0

generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)

eststo tnz_weduc_r , title (2014): quietly reg w_educ logx r2012 log_mean_cost_ne i.agri i.educpriv   father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust


use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
drop if numchild == 0
gen log_educ= log(toteducexpense+1e-7)

eststo ngr_weduc_r , title (2015): quietly reg  w_educ logx r2012  log_mean_cost_ne i.agri i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.religion numchild


************************************* q_educ - r ****************************************

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)

eststo tnz_logeduc_r, title (2014): quietly reg log_educ logx r2012 log_mean_cost_ne i.agri i.educpriv    father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust

use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
drop if numchild == 0

gen log_educ= log(toteducexpense+1e-7)

eststo ngr_logeduc_r, title (2015): quietly reg log_educ logx r2012  log_mean_cost_ne i.agri i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.religion numchild, robust


esttab tnz_weduc_r ngr_weduc_r tnz_logeduc_r ngr_logeduc_r using "c:/temp/ngrtnz_r.tex", replace f booktabs nomtitles mgroups("\$w_{educ}\$" "\$log(x_{educ})\$", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))



************************************** URBAN-RURAL  **************************************
************************************* w_educ URBAN-RURAL ****************************************

eststo clear

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)

eststo tnz_weduc_urbanrural, title (2014): quietly reg w_educ lnA0 log_mean_cost_ne i.agri i.educpriv   father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage rural_wards i.has_english numchild, robust

use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
drop if numchild == 0
gen log_educ= log(toteducexpense+1e-7)


eststo ngr_weduc_urbanrural, title (2015): quietly reg  w_educ lnA0  log_mean_cost_ne i.agri i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage rural_wards i.religion numchild


************************************* q_educ URBAN-RURAL ****************************************

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)

eststo tnz_logeduc_urbanrural, title (2014): quietly reg log_educ lnA0 log_mean_cost_ne i.agri i.educpriv father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage rural_wards i.has_english numchild, robust



use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
drop if numchild == 0
gen log_educ= log(toteducexpense+1e-7)

eststo ngr_logeduc_urbanrural, title (2015): quietly reg log_educ lnA0  log_mean_cost_ne i.agri i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage rural_wards i.religion numchild, robust


esttab tnz_weduc_urbanrural ngr_weduc_urbanrural tnz_logeduc_urbanrural ngr_logeduc_urbanrural using "c:/temp/ngrtnz_urbanrural.tex", replace f booktabs nomtitles mgroups("\$w_{educ}\$" "\$log(x_{educ})\$", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))





************************************* r_educ  ****************************************
eststo clear

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3

eststo tnz_weduc_reduc, title (Educ 2014): quietly reg w_educ logx r_educ2012 log_mean_cost_ne i.agri i.educpriv father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust

use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
drop if numchild == 0

eststo ngr_weduc_reduc, title (Educ 2015): quietly reg  w_educ logx r_educ2012  log_mean_cost_ne i.agri i.educpriv father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.religion numchild

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)

eststo tnz_logeduc_reduc, title (Educ 2014): quietly reg log_educ logx r_educ2012  log_mean_cost_ne i.agri i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust

use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
drop if numchild == 0
gen log_educ= log(toteducexpense+1e-7)

eststo ngr_logeduc_reduc, title (Educ 2015): quietly reg log_educ logx r_educ2012  log_mean_cost_ne i.agri i.educpriv father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.religion numchild, robust

esttab tnz_weduc_reduc ngr_weduc_reduc tnz_logeduc_reduc ngr_logeduc_reduc using "c:/temp/ngrtnz_reduc.tex", replace f booktabs nomtitles mgroups("\$w_{educ}\$" "\$log(x_{educ})\$", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))



************************************* r_agri  ****************************************

eststo clear

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3

eststo tnz_weduc_roccup, title (Agri 2014): quietly reg w_educ logx r_agri2012 log_mean_cost_ne i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust


use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
drop if numchild == 0
eststo ngr_weduc_roccup, title (Agri 2015): quietly reg  w_educ logx r_agri2012 log_mean_cost_ne i.educpriv father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.religion numchild


use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)

eststo tnz_logeduc_roccup, title (Agri 2014): quietly reg log_educ logx r_agri2012 log_mean_cost_ne i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust


use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
drop if numchild == 0
gen log_educ= log(toteducexpense+1e-7)

eststo ngr_logeduc_roccup, title (Agri 2015): quietly reg log_educ logx r_agri2012 log_mean_cost_ne i.educpriv father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.religion numchild

esttab tnz_weduc_roccup ngr_weduc_roccup tnz_logeduc_roccup ngr_logeduc_roccup using "c:/temp/ngrtnz_roccup.tex", replace f booktabs nomtitles mgroups("\$w_{educ}\$" "\$log(x_{educ})\$", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))


*********************** above median ****************************************************************

eststo clear

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3

drop if lnA0 < 12.75996   

eststo tnz_weduc_abovemed: quietly reg w_educ logx r2012 log_mean_cost_ne i.agri i.educpriv   father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust

use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
drop if numchild == 0
drop if lnA0 <  11.0021     

eststo ngr_weduc_abovemed: quietly reg  w_educ logx r2012  log_mean_cost_ne i.agri i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.religion numchild

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3
drop if lnA0 < 12.75996   
gen log_educ= log(toteducexpense+1e-7)


eststo tnz_logeduc_abovemed: quietly reg log_educ logx r2012 log_mean_cost_ne i.agri i.educpriv    father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust

use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
drop if numchild == 0
drop if lnA0 <  11.0021     
gen log_educ= log(toteducexpense+1e-7)

eststo ngr_logeduc_abovemed: quietly reg log_educ logx r2012  log_mean_cost_ne i.agri i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.religion numchild, robust


***********************below median ****************************************************************

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3

drop if lnA0 >= 12.75996   

eststo tnz_weduc_belowmed: quietly reg w_educ logx r2012 log_mean_cost_ne i.agri i.educpriv   father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust

use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
drop if numchild == 0

drop if lnA0 >=  11.0021     

eststo ngr_weduc_belowmed: quietly reg  w_educ logx r2012  log_mean_cost_ne i.agri i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.religion numchild


use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3
drop if lnA0 >= 12.75996   
gen log_educ= log(toteducexpense+1e-7)


eststo tnz_logeduc_belowmed: quietly reg log_educ logx r2012 log_mean_cost_ne i.agri i.educpriv    father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust

use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
drop if numchild == 0
drop if lnA0 >=  11.0021     
gen log_educ= log(toteducexpense+1e-7)

eststo ngr_logeduc_belowmed: quietly reg log_educ logx r2012  log_mean_cost_ne i.agri i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.religion numchild, robust


esttab tnz_weduc_abovemed ngr_weduc_abovemed tnz_logeduc_abovemed ngr_logeduc_abovemed tnz_weduc_belowmed ngr_weduc_belowmed tnz_logeduc_belowmed ngr_logeduc_belowmed using "c:/temp/ngrtnz_abovebelowmedian.tex", replace f booktabs nomtitles mgroups("Above Median \$w_{educ}\$" "Above Median \$log(x_{educ})\$" "Below Median \$w_{educ}\$" "Below Median \$log(x_{educ})\$", pattern(1 0 1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
