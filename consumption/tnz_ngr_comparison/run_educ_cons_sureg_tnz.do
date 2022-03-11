use "C:\local_files\research\consumption\lsms\data\tn_df2012.dta", clear

*gen log_educ= log(toteducexpense+1e-7)

generate has_english =  litlang == 2 | litlang == 3

global logqne2012 (w_ne lnX r log_mean_cost_ne max_occupation_rank max_education_rank father_educ_rank mother_educ_rank age rural_wards i.has_english hsize)
*global logA2012 (w_A lnX r log_mean_cost_ne max_occupation_rank max_education_rank age rural_wards i.has_english hsize)
global logeduc2012 (w_educ lnX r log_mean_cost_ne max_occupation_rank max_education_rank father_educ_rank mother_educ_rank age rural_wards i.has_english hsize)
eststo clear
eststo, title (2012): quietly sureg $logqne2012 $logeduc2012


use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
generate has_english =  litlang == 2 | litlang == 3

global logqne2014 (w_ne lnX r  log_mean_cost_ne max_occupation_rank max_education_rank father_educ_rank mother_educ_rank age rural_wards i.has_english hsize)
*global logA2014 (w_A lnX r log_mean_cost_ne max_occupation_rank max_education_rank age rural_wards i.has_english hsize)
global logeduc2014 (w_educ lnX r log_mean_cost_ne max_occupation_rank max_education_rank  father_educ_rank mother_educ_rank age rural_wards i.has_english hsize)
eststo, title (2014): quietly sureg $logqne2014 $logeduc2014

esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title() 
