use "C:\local_files\research\consumption\lsms\data\tn_df2012.dta", clear
eststo clear


generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)

global logqne2012 (w_ne lnX r log_mean_cost_ne i.agri max_education_rank father_educ_rank age rural_wards i.has_english hsize)
global logeduc2012 (w_educ lnX r log_mean_cost_ne i.agri max_education_rank father_educ_rank age rural_wards i.has_english hsize)
eststo, title (2012): quietly sureg $logqne2012 $logeduc2012

global edlogqne2012 (w_ne lnX r_educ log_mean_cost_ne i.agri father_educ_rank age rural_wards i.has_english hsize)
global edlogeduc2012 (w_educ lnX r_educ log_mean_cost_ne i.agri father_educ_rank age rural_wards i.has_english hsize)
eststo, title (Educ 2012): quietly sureg $edlogqne2012 $edlogeduc2012

global aglogqne2012 (w_ne lnX r_agri  log_mean_cost_ne max_education_rank father_educ_rank age rural_wards i.has_english hsize)
global aglogeduc2012 (w_educ lnX r_agri log_mean_cost_ne max_education_rank  father_educ_rank age rural_wards i.has_english hsize)
eststo, title  (Agri 2012): quietly sureg $aglogqne2012 $aglogeduc2012


use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear


generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)

global logqne2014 (w_ne lnX r  log_mean_cost_ne i.agri max_education_rank father_educ_rank age rural_wards i.has_english hsize)
global logeduc2014 (w_educ lnX r log_mean_cost_ne i.agri max_education_rank  father_educ_rank age rural_wards i.has_english hsize)
eststo, title (2014): quietly sureg $logqne2014 $logeduc2014

global edlogqne2014 (w_ne lnX r_educ  log_mean_cost_ne i.agri father_educ_rank age rural_wards i.has_english hsize)
global edlogeduc2014 (w_educ lnX r_educ log_mean_cost_ne i.agri father_educ_rank age rural_wards i.has_english hsize)
eststo, title (Educ 2014): quietly sureg $edlogqne2014 $edlogeduc2014

global aglogqne2014 (w_ne lnX r_agri  log_mean_cost_ne max_education_rank father_educ_rank age rural_wards i.has_english hsize)
global aglogeduc2014 (w_educ lnX r_agri log_mean_cost_ne max_education_rank  father_educ_rank age rural_wards i.has_english hsize)
eststo, title (Agri 2014): quietly sureg $aglogqne2014 $aglogeduc2014



esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title() 

************************************* q_educ ****************************************

eststo clear

use "C:\local_files\research\consumption\lsms\data\tn_df2012.dta", clear

generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)
eststo, title (2012): quietly ivregress 2sls log_educ (lnX = hh_occupation) r log_mean_cost_ne i.agri max_education_rank  father_educ_rank age rural_wards i.has_english hsize, robust
eststo, title (Educ 2012): quietly ivregress 2sls log_educ (lnX = hh_occupation) r_educ log_mean_cost_ne i.agri father_educ_rank age rural_wards i.has_english hsize, robust
eststo, title (Agri 2012): quietly ivregress 2sls log_educ (lnX = hh_occupation) r_agri log_mean_cost_ne max_education_rank  father_educ_rank age rural_wards i.has_english hsize, robust


use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear

generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)

eststo, title (2014): quietly ivregress 2sls log_educ (lnX = hh_occupation) r log_mean_cost_ne i.agri max_education_rank  father_educ_rank age rural_wards i.has_english hsize, robust
eststo, title (Educ 2014): quietly ivregress 2sls log_educ (lnX = hh_occupation) r_educ log_mean_cost_ne i.agri father_educ_rank age rural_wards i.has_english hsize, robust
eststo, title (Agri 2014): quietly ivregress 2sls log_educ (lnX = hh_occupation) r_agri log_mean_cost_ne max_education_rank  father_educ_rank age rural_wards i.has_english hsize, robust

*occupation-rank statistic
*estat firststage, all forcenonrobust

esttab using c:/temp/resnu2.tex, mtitle no p numbers nogaps compress title() 

