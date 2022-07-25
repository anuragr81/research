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

esttab tnz_weduc_r ngr_weduc_r tnz_logeduc_r ngr_logeduc_r using "c:/temp/t.tex", replace f booktabs nomtitles mgroups("budget-share" "education-expenditure", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) 


************************************* w_educ URBAN-RURAL ****************************************


use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
drop if numchild == 0
gen log_educ= log(toteducexpense+1e-7)


eststo, title (2015): quietly reg  w_educ lnA0  log_mean_cost_ne i.agri i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage rural_wards i.religion numchild




************************************* q_educ URBAN-RURAL ****************************************

use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
drop if numchild == 0

gen log_educ= log(toteducexpense+1e-7)

eststo, title (2015): quietly reg log_educ lnA0  log_mean_cost_ne i.agri i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage rural_wards i.religion numchild, robust






************************************* r_educ r_agri  ****************************************


eststo, title (Educ 2015): quietly reg  w_educ logx r_educ2012  log_mean_cost_ne i.agri i.educpriv father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.religion numchild
eststo, title (Agri 2015): quietly reg  w_educ logx r_agri2012 log_mean_cost_ne i.educpriv father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.religion numchild

eststo, title (Educ 2015): quietly reg log_educ logx r_educ2012  log_mean_cost_ne i.agri i.educpriv father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.religion numchild, robust
eststo, title (Agri 2015): quietly reg log_educ logx r_agri2012 log_mean_cost_ne i.educpriv father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.religion numchild


