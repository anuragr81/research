use "C:\local_files\research\consumption\lsms\data\ngr_df2012.dta", clear

*gen log_educ= log(toteducexpense+1e-7)

global logqne2012 (w_ne lnX r log_mean_cost_ne max_occupation_rank max_education_rank age rural_wards hsize)
*global logA2012 (w_A lnX r log_mean_cost_ne max_occupation_rank max_education_rank age rural_wards hsize)
global logeduc2012 (w_educ lnX r log_mean_cost_ne max_occupation_rank max_education_rank age rural_wards hsize)
eststo clear
eststo, title (2012): quietly sureg $logqne2012 $logeduc2012


use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear

global logqne2015 (w_ne lnX r log_mean_cost_ne max_occupation_rank max_education_rank age rural_wards hsize)
*global logA2015 (w_A lnX r log_mean_cost_ne max_occupation_rank max_education_rank age rural_wards hsize)
global logeduc2015 (w_educ lnX r log_mean_cost_ne max_occupation_rank max_education_rank age rural_wards hsize)
eststo, title (2015): quietly sureg $logqne2015 $logeduc2015

esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title() 


