use "C:\local_files\research\consumption\lsms\data\tn_df2012.dta", clear
eststo clear


generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)

global logqne2012 (w_ne logx r2012 log_mean_cost_ne i.agri  hh_education_rank age rural_wards i.has_english hsize)
global logeduc2012 (w_educ logx r2012 log_mean_cost_ne i.agri  hh_education_rank age rural_wards i.has_english hsize)
eststo, title (2012): quietly sureg $logqne2012 $logeduc2012

global edlogqne2012 (w_ne logx r_educ2012 log_mean_cost_ne i.agri hh_education_rank age rural_wards i.has_english hsize)
global edlogeduc2012 (w_educ logx r_educ2012 log_mean_cost_ne i.agri hh_education_rank age rural_wards i.has_english hsize)
eststo, title (Educ 2012): quietly sureg $edlogqne2012 $edlogeduc2012

global aglogqne2012 (w_ne logx r_agri2012  log_mean_cost_ne  hh_education_rank age rural_wards i.has_english hsize)
global aglogeduc2012 (w_educ logx r_agri2012 log_mean_cost_ne   hh_education_rank age rural_wards i.has_english hsize)
eststo, title  (Agri 2012): quietly sureg $aglogqne2012 $aglogeduc2012


use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear


generate has_english =  litlang == 2 | litlang == 3
gen log_educ= log(toteducexpense+1e-7)

global logqne2014 (w_ne logx r2012  log_mean_cost_ne i.agri  hh_education_rank age rural_wards i.has_english hsize)
global logeduc2014 (w_educ logx r2012 log_mean_cost_ne i.agri   hh_education_rank age rural_wards i.has_english hsize)
eststo, title (2014): quietly sureg $logqne2014 $logeduc2014

global edlogqne2014 (w_ne logx r_educ2012  log_mean_cost_ne i.agri hh_education_rank age rural_wards i.has_english hsize)
global edlogeduc2014 (w_educ logx r_educ2012 log_mean_cost_ne i.agri hh_education_rank age rural_wards i.has_english hsize)
eststo, title (Educ 2014): quietly sureg $edlogqne2014 $edlogeduc2014

global aglogqne2014 (w_ne logx r_agri2012  log_mean_cost_ne  hh_education_rank age rural_wards i.has_english hsize)
global aglogeduc2014 (w_educ logx r_agri2012 log_mean_cost_ne   hh_education_rank age rural_wards i.has_english hsize)
eststo, title (Agri 2014): quietly sureg $aglogqne2014 $aglogeduc2014



esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title() 


*************************************** logit for education ******************************
eststo clear

use "C:\local_files\research\consumption\lsms\data\tn_df2012.dta", clear
generate has_english =  litlang == 2 | litlang == 3
gen edchange = hh_education_rank > father_educ_rank

eststo, title (2012): quietly logit edchange father_educ_rank age i.has_english rural_wards i.agri r2012
eststo, title (Educ 2012): quietly logit edchange father_educ_rank age i.has_english rural_wards i.agri r_educ2012
eststo, title (Agri 2012): quietly logit edchange father_educ_rank age i.has_english rural_wards  r_agri2012

use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
generate has_english =  litlang == 2 | litlang == 3
gen edchange = hh_education_rank > father_educ_rank
eststo, title (2014): quietly logit edchange father_educ_rank age i.has_english rural_wards i.agri r2012
eststo, title (Educ 2014): quietly logit edchange father_educ_rank age i.has_english rural_wards i.agri r_educ2012
eststo, title (Agri 2014): quietly logit edchange father_educ_rank age i.has_english rural_wards r_agri2012


esttab using c:/temp/resnu3.tex, mtitle no p numbers nogaps compress title() 

