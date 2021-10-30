
args depvar

use "C:\temp\ng_df2012_2015.dta", clear

eststo clear

xtset hhid year

eststo, title (2012-2015): quietly xtreg `depvar' logx log_mean_A0 log_mean_cost_ne_food log_mean_cost_ne_nonfood age hsize,  fe vce(robust)

esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title(Nigeria: FE estimation for excess \$ w \$ as dependent variable \label{tabExcessFENGR})

*predict resid, residuals
*hist resid
