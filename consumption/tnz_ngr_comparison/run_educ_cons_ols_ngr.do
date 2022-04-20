************************************* w_educ ****************************************

use "C:\local_files\research\consumption\lsms\data\ngr_df2012.dta", clear
eststo clear

gen log_educ= log(toteducexpense+1e-7)


*eststo, title (2012): quietly reg  w_ne logx r2012 log_mean_cost_ne i.agri i.educpriv  hh_education_rank meanchildage rural_wards i.religion numchild
*eststo, title (Educ 2012): quietly reg  w_ne logx r_educ2012 log_mean_cost_ne i.agri i.educpriv hh_education_rank meanchildage rural_wards i.religion numchild
*eststo, title (Agri 2012): quietly reg  w_ne logx r_agri2012  log_mean_cost_ne  i.educpriv hh_education_rank meanchildage rural_wards i.religion numchild

eststo, title (2012): quietly reg  w_educ  logx r2012  log_mean_cost_ne i.agri i.educpriv  hh_education_rank meanchildage rural_wards i.religion numchild
eststo, title (Educ 2012): quietly reg  w_educ  logx r_educ2012  log_mean_cost_ne i.agri i.educpriv hh_education_rank meanchildage rural_wards i.religion numchild
eststo, title (Agri 2012): quietly reg  w_educ logx r_agri2012 log_mean_cost_ne  i.educpriv hh_education_rank meanchildage rural_wards i.religion numchild

use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
gen log_educ= log(toteducexpense+1e-7)


*eststo, title (2015): quietly reg  w_ne logx r2012 log_mean_cost_ne i.agri i.educpriv  hh_education_rank meanchildage rural_wards i.religion numchild
*eststo, title (2015): quietly reg  w_ne logx r_educ2012  log_mean_cost_ne i.agri i.educpriv hh_education_rank meanchildage rural_wards i.religion numchild
*eststo, title (2015): quietly reg  w_ne logx r_agri2012 log_mean_cost_ne  i.educpriv hh_education_rank meanchildage rural_wards i.religion numchild

eststo, title (2015): quietly reg  w_educ logx r2012  log_mean_cost_ne i.agri i.educpriv  hh_education_rank meanchildage rural_wards i.religion numchild
eststo, title (2015): quietly reg  w_educ logx r_educ2012  log_mean_cost_ne i.agri i.educpriv hh_education_rank meanchildage rural_wards i.religion numchild
eststo, title (2015): quietly reg  w_educ logx r_agri2012 log_mean_cost_ne i.educpriv hh_education_rank meanchildage rural_wards i.religion numchild

esttab using c:/temp/resnu1.tex, mtitle no p numbers nogaps compress title() 


************************************* q_educ ****************************************
eststo clear

use "C:\local_files\research\consumption\lsms\data\ngr_df2012.dta", clear

gen log_educ= log(toteducexpense+1e-7)


eststo, title (2012): quietly reg log_educ logx r2012 log_mean_cost_ne i.agri i.educpriv  hh_education_rank meanchildage rural_wards i.religion numchild, robust
eststo, title (Educ 2012): quietly reg log_educ logx r_educ2012 log_mean_cost_ne i.agri i.educpriv hh_education_rank meanchildage rural_wards i.religion numchild, robust
eststo, title (Agri 2012): quietly reg log_educ logx r_agri2012 log_mean_cost_ne i.educpriv hh_education_rank meanchildage rural_wards i.religion numchild


use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear


gen log_educ= log(toteducexpense+1e-7)

eststo, title (2015): quietly reg log_educ logx r2012  log_mean_cost_ne i.agri i.educpriv  hh_education_rank meanchildage rural_wards i.religion numchild, robust
eststo, title (Educ 2015): quietly reg log_educ logx r_educ2012  log_mean_cost_ne i.agri i.educpriv hh_education_rank meanchildage rural_wards i.religion numchild, robust
eststo, title (Agri 2015): quietly reg log_educ logx r_agri2012 log_mean_cost_ne i.educpriv hh_education_rank meanchildage rural_wards i.religion numchild

esttab using c:/temp/resnu2.tex, mtitle no p numbers nogaps compress title() 


************************************* q_educ qreg ****************************************
eststo clear

*use "C:\local_files\research\consumption\lsms\data\ngr_df2012.dta", clear

*gen log_educ= log(toteducexpense+1e-7)

*eststo, title (2012): quietly sqreg log_educ logx r2012 log_mean_cost_ne i.agri i.educpriv  hh_education_rank meanchildage rural_wards i.religion numchild, q(.30 .60 .90) reps(200)

*use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear

*gen log_educ= log(toteducexpense+1e-7)

*eststo, title (2015): quietly sqreg log_educ logx r2012  log_mean_cost_ne i.agri i.educpriv  hh_education_rank meanchildage rural_wards i.religion numchild, q(.30 .60 .90) reps(200)

*esttab using c:/temp/resnu3.tex, mtitle no p numbers nogaps compress title() 

