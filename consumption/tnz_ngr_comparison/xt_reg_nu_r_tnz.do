
use "C:\temp\tn_df2012_2014.dta", clear

eststo clear

xtset hhid year

eststo, title (2012-2014): quietly xtreg `depvar' logx log_mean_A0  log_mean_cost_ne_food log_mean_cost_ne_nonfood age hsize,  fe vce(robust)

esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title(Tanzania: FE estimation for excess \$ w \$ as dependent variable \label{tabExcessFETNZ})


*xtreg nu i.max_education_rank i.max_occupation_rank r Ar,  fe vce(robust)
*predict resid, residuals
*hist resid
