use "C:\local_files\research\consumption\lsms\data\ngr_df2012.dta", clear
eststo clear

gen log_educ= log(toteducexpense+1e-7)


eststo, title (2012 w_ne): quietly reg  w_ne logx r2012 log_mean_cost_ne i.agri  hh_education_rank age rural_wards i.religion hsize
eststo, title (Educ 2012 w_ne): quietly reg  w_ne logx r_educ2012 log_mean_cost_ne i.agri hh_education_rank age rural_wards i.religion hsize
eststo, title (Agri 2012 w_ne): quietly reg  w_ne logx r_agri2012 log_mean_cost_ne  hh_education_rank age rural_wards i.religion hsize

eststo, title (2012 w_educ): quietly reg  w_educ logx r2012 log_mean_cost_ne i.agri  hh_education_rank age rural_wards i.religion hsize
eststo, title (Educ 2012 w_educ): quietly reg  w_educ logx r_educ2012 log_mean_cost_ne i.agri hh_education_rank age rural_wards i.religion hsize
eststo, title (Agri 2012 w_educ): quietly reg  w_educ logx r_agri2012 log_mean_cost_ne  hh_education_rank age rural_wards i.religion hsize

esttab using c:/temp/resnu1.tex, mtitle no p numbers nogaps compress title() 

eststo clear

use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
gen log_educ= log(toteducexpense+1e-7)


eststo, title (2015 w_ne): quietly reg  w_ne logx r2012 log_mean_cost_ne i.agri  hh_education_rank age rural_wards i.religion hsize
eststo, title (2015 w_ne): quietly reg  w_ne logx r_educ2012 log_mean_cost_ne i.agri hh_education_rank age rural_wards i.religion hsize
eststo, title (2015 w_ne): quietly reg  w_ne logx r_agri2012 log_mean_cost_ne  hh_education_rank age rural_wards i.religion hsize

eststo, title (2015 w_educ): quietly reg  w_educ logx r2012 log_mean_cost_ne i.agri  hh_education_rank age rural_wards i.religion hsize
eststo, title (2015 w_educ): quietly reg  w_educ logx r_educ2012 log_mean_cost_ne i.agri hh_education_rank age rural_wards i.religion hsize
eststo, title (2015 w_educ): quietly reg  w_educ logx r_agri2012 log_mean_cost_ne  hh_education_rank age rural_wards i.religion hsize

esttab using c:/temp/resnu2.tex, mtitle no p numbers nogaps compress title() 
