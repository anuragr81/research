use "C:\local_files\research\consumption\lsms\data\ngr_df2012.dta", clear
eststo clear

gen log_educ= log(toteducexpense+1e-7)


global logqne2012 (w_ne logx r2012 log_mean_cost_ne i.agri  hh_education_rank age rural_wards i.religion hsize)
global logeduc2012 (w_educ logx r2012 log_mean_cost_ne i.agri  hh_education_rank age rural_wards i.religion hsize)
eststo, title (2012): quietly sureg $logqne2012 $logeduc2012

global edlogqne2012 (w_ne logx r_educ2012 log_mean_cost_ne i.agri hh_education_rank age rural_wards i.religion hsize)
global edlogeduc2012 (w_educ logx r_educ2012 log_mean_cost_ne i.agri hh_education_rank age rural_wards i.religion hsize)
eststo, title (Educ 2012): quietly sureg $edlogqne2012 $edlogeduc2012

global aglogqne2012 (w_ne logx r_agri2012 log_mean_cost_ne  hh_education_rank age rural_wards i.religion hsize)
global aglogeduc2012 (w_educ logx r_agri2012 log_mean_cost_ne  hh_education_rank age rural_wards i.religion hsize)
eststo, title (Agri 2012): quietly sureg $aglogqne2012 $aglogeduc2012

use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
gen log_educ= log(toteducexpense+1e-7)


global logqne2015 (w_ne logx r2012 log_mean_cost_ne i.agri  hh_education_rank age rural_wards i.religion hsize)
global logeduc2015 (w_educ logx r2012 log_mean_cost_ne i.agri  hh_education_rank age rural_wards i.religion hsize)
eststo, title (2015): quietly sureg $logqne2015 $logeduc2015

global edlogqne2015 (w_ne logx r_educ2012 log_mean_cost_ne i.agri hh_education_rank age rural_wards i.religion hsize)
global edlogeduc2015 (w_educ logx r_educ2012 log_mean_cost_ne i.agri hh_education_rank age rural_wards i.religion hsize)
eststo, title (Educ 2015): quietly sureg $edlogqne2015 $edlogeduc2015


global aglogqne2015 (w_ne logx r_agri2012 log_mean_cost_ne  hh_education_rank age rural_wards i.religion hsize)
global aglogeduc2015 (w_educ logx r_agri2012 log_mean_cost_ne  hh_education_rank age rural_wards i.religion hsize)
eststo, title (Agri 2015): quietly sureg $aglogqne2015 $aglogeduc2015

esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title() 



*************************************** logit for education ******************************
eststo clear

use "C:\local_files\research\consumption\lsms\data\ngr_df2012.dta", clear
gen edchange = hh_education_rank > father_educ_rank
eststo, title (2012): quietly logit edchange father_educ_rank age i.religion rural_wards i.agri r2012
eststo, title (Educ 2012): quietly logit edchange father_educ_rank age i.religion rural_wards i.agri r_educ2012
eststo, title (Agri 2012): quietly logit edchange father_educ_rank age i.religion rural_wards r_agri2012

use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear

gen edchange = hh_education_rank > father_educ_rank
eststo, title (2015): quietly logit edchange father_educ_rank age i.religion rural_wards i.agri r2012
eststo, title (Educ 2015): quietly logit edchange father_educ_rank age i.religion rural_wards i.agri r_educ2012
eststo, title (Agri 2015): quietly logit edchange father_educ_rank age i.religion rural_wards r_agri2012

esttab using c:/temp/resnu3.tex, mtitle no p numbers nogaps compress title() 

