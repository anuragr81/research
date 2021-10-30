
*use "C:\temp\ng_df2012_2015.dta", clear
use "C:\temp\tn_df2012_2014.dta", clear

eststo clear

xtset hhid year

eststo, title (2012-2015): quietly xtreg log_q_ne logx log_mean_A0 log_mean_cost_ne  hh_age,  fe vce(robust)

esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title(FE estimation for excess \$ w \$ as dependent variable \label{tabExcessFE})


*xtreg nu i.max_education_rank i.max_occupation_rank r Ar,  fe vce(robust)
*predict resid, residuals
*hist resid