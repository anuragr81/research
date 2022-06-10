************************************* w_educ wealth concentration ****************************************


*use "C:\local_files\research\consumption\lsms\data\tn_df2012.dta", clear
*drop if numchild == 0

eststo clear

*generate has_english =  litlang == 2 | litlang == 3
*gen log_educ= log(toteducexpense+1e-7)

*eststo, title (2012): quietly reg w_educ logx r2012 log_mean_cost_ne i.agri i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust
*eststo, title (Educ 2012): quietly reg w_educ logx r_educ2012 log_mean_cost_ne i.agri i.educpriv father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust
*eststo, title  (Agri 2012): quietly reg w_educ logx r_agri2012 log_mean_cost_ne   i.educpriv father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0

generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)

eststo, title (2014): quietly reg w_educ logx r2012 log_mean_cost_ne i.agri i.educpriv   father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust
eststo, title (Educ 2014): quietly reg w_educ logx r_educ2012 log_mean_cost_ne i.agri i.educpriv father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust
eststo, title (Agri 2014): quietly reg w_educ logx r_agri2012 log_mean_cost_ne i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust


esttab using c:/temp/resnu1.tex, mtitle no p numbers nogaps compress title() 

************************************* q_educ ****************************************

eststo clear

*use "C:\local_files\research\consumption\lsms\data\tn_df2012.dta", clear
*drop if numchild == 0

*generate has_english =  litlang == 2 | litlang == 3
*gen log_educ= log(toteducexpense+1e-7)
*eststo, title (2012): quietly reg log_educ logx r2012 log_mean_cost_ne i.agri i.educpriv    father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust
*eststo, title (Educ 2012): quietly reg log_educ logx r_educ2012 log_mean_cost_ne i.agri i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust
*eststo, title (Agri 2012): quietly reg log_educ logx r_agri2012  log_mean_cost_ne i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust


use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)

eststo, title (2014): quietly reg log_educ logx r2012 log_mean_cost_ne i.agri i.educpriv    father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust
eststo, title (Educ 2014): quietly reg log_educ logx r_educ2012  log_mean_cost_ne i.agri i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust
eststo, title (Agri 2014): quietly reg log_educ logx r_agri2012 log_mean_cost_ne i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage  i.has_english numchild, robust

*occupation-rank statistic
*estat firststi.is_primaryage i.is_secondaryage i.is_tertiaryage, all forcenonrobust

esttab using c:/temp/resnu2.tex, mtitle no p numbers nogaps compress title() 


***********************************************************************************************
************************************* w_educ URBAN-RURAL ****************************************

*use "C:\local_files\research\consumption\lsms\data\tn_df2012.dta", clear
*drop if numchild == 0
eststo clear


*generate has_english =  litlang == 2 | litlang == 3
*gen log_educ= log(toteducexpense+1e-7)


*eststo, title (2012): quietly reg w_educ lnA0 log_mean_cost_ne i.agri i.educpriv  father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage rural_wards i.has_english numchild, robust

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)

eststo, title (2014): quietly reg w_educ lnA0 log_mean_cost_ne i.agri i.educpriv   father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage rural_wards i.has_english numchild, robust

esttab using c:/temp/resnu3.tex, mtitle no p numbers nogaps compress title() 

************************************* q_educ URBAN-RURAL ****************************************

eststo clear

*use "C:\local_files\research\consumption\lsms\data\tn_df2012.dta", clear
*drop if numchild == 0
*generate has_english =  litlang == 2 | litlang == 3
*gen log_educ= log(toteducexpense+1e-7)
*eststo, title (2012): quietly reg log_educ lnA0 log_mean_cost_ne i.agri i.educpriv father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage rural_wards i.has_english numchild, robust

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
drop if numchild == 0
generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)

eststo, title (2014): quietly reg log_educ lnA0 log_mean_cost_ne i.agri i.educpriv father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage rural_wards i.has_english numchild, robust

*occupation-rank statistic
*estat firststi.is_primaryage i.is_secondaryage i.is_tertiaryage, all forcenonrobust

esttab using c:/temp/resnu4.tex, mtitle no p numbers nogaps compress title() 


************************************* q_educ quantile  ****************************************

*eststo clear

*use "C:\local_files\research\consumption\lsms\data\tn_df2012.dta", clear

*generate has_english =  litlang == 2 | litlang == 3
*gen log_educ= log(toteducexpense+1e-7)

*eststo, title (2012): quietly sqreg log_educ logx r2012 log_mean_cost_ne i.agri i.educpriv    father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage rural_wards i.has_english numchild, q(.30 .60 .90) reps(200)

*use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear

*generate has_english =  litlang == 2 | litlang == 3
*gen log_educ= log(toteducexpense+1e-7)

*eststo, title (2014): quietly sqreg log_educ logx r2012 log_mean_cost_ne i.agri i.educpriv    father_educ_rank secondary_schools i.is_primaryage i.is_secondaryage i.is_tertiaryage rural_wards i.has_english numchild, q(.30 .60 .90) reps(200)

*esttab using c:/temp/resnu3.tex, mtitle no p numbers nogaps compress title() 

