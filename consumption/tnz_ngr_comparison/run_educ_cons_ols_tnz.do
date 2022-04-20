************************************* w_educ ****************************************

use "C:\local_files\research\consumption\lsms\data\tn_df2012.dta", clear
eststo clear


generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)

*eststo, title (w_ne) : quietly reg w_ne logx r2012 log_mean_cost_ne i.agri i.educpriv  hh_education_rank age rural_wards i.has_english numchild, robust
*eststo, title (Educ w_ne): quietly reg w_ne logx r_educ2012 log_mean_cost_ne i.agri i.educpriv hh_education_rank age rural_wards i.has_english numchild, robust
*eststo, title  (Agri w_ne): quietly reg w_ne logx r_agri2012  log_mean_cost_ne  i.educpriv hh_education_rank age rural_wards i.has_english numchild, robust

eststo, title (2012): quietly reg w_educ logx r2012 log_mean_cost_ne i.agri i.educpriv  hh_education_rank age rural_wards i.has_english numchild, robust
eststo, title (Educ 2012): quietly reg w_educ logx r_educ2012 log_mean_cost_ne i.agri i.educpriv hh_education_rank age rural_wards i.has_english numchild, robust
eststo, title  (Agri 2012): quietly reg w_educ logx r_agri2012 log_mean_cost_ne   i.educpriv hh_education_rank age rural_wards i.has_english numchild, robust

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear

generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)

*eststo, title (2014 w_ne): quietly  reg w_ne logx r2012 log_mean_cost_ne i.agri i.educpriv  hh_education_rank age rural_wards i.has_english numchild, robust
*eststo, title (Educ 2014 w_ne): quietly reg w_ne logx r_educ2012  log_mean_cost_ne i.agri i.educpriv hh_education_rank age rural_wards i.has_english numchild, robust
*eststo, title (Agri 2014 w_ne): quietly reg w_ne  logx r_agri2012 log_mean_cost_ne i.educpriv hh_education_rank age rural_wards i.has_english numchild, robust

eststo, title (2014): quietly reg w_educ logx r2012 log_mean_cost_ne i.agri i.educpriv   hh_education_rank age rural_wards i.has_english numchild, robust
eststo, title (Educ 2014): quietly reg w_educ logx r_educ2012 log_mean_cost_ne i.agri i.educpriv hh_education_rank age rural_wards i.has_english numchild, robust
eststo, title (Agri 2014): quietly reg w_educ logx r_agri2012 log_mean_cost_ne i.educpriv  hh_education_rank age rural_wards i.has_english numchild, robust


esttab using c:/temp/resnu1.tex, mtitle no p numbers nogaps compress title() 

************************************* q_educ ****************************************

eststo clear

use "C:\local_files\research\consumption\lsms\data\tn_df2012.dta", clear

generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)
eststo, title (2012): quietly reg log_educ logx r2012 log_mean_cost_ne i.agri i.educpriv    hh_education_rank age rural_wards i.has_english numchild, robust
eststo, title (Educ 2012): quietly reg log_educ logx r_educ2012 log_mean_cost_ne i.agri i.educpriv  hh_education_rank age rural_wards i.has_english numchild, robust
eststo, title (Agri 2012): quietly reg log_educ logx r_agri2012  log_mean_cost_ne i.educpriv  hh_education_rank age rural_wards i.has_english numchild, robust


use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear

generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)

eststo, title (2014): quietly reg log_educ logx r2012 log_mean_cost_ne i.agri i.educpriv    hh_education_rank age rural_wards i.has_english numchild, robust
eststo, title (Educ 2014): quietly reg log_educ logx r_educ2012  log_mean_cost_ne i.agri i.educpriv  hh_education_rank age rural_wards i.has_english numchild, robust
eststo, title (Agri 2014): quietly reg log_educ logx r_agri2012 log_mean_cost_ne i.educpriv  hh_education_rank age rural_wards i.has_english numchild, robust

*occupation-rank statistic
*estat firststage, all forcenonrobust

esttab using c:/temp/resnu2.tex, mtitle no p numbers nogaps compress title() 

