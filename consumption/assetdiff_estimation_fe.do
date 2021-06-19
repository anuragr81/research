
*use "C:\temp\df2010_2012.dta", clear
use "C:\temp\df2012_2014.dta", clear

** try with fe estimator from the output of get_band_metrics(estimation_df_budget_quantile(ll,e) , b= 1.2)
eststo clear

xtset hhid year
*eststo, title(LR): quietly xtreg w_nu local_outofplace log_needs_price logx ,  fe vce(robust)
*eststo, title (NR): quietly xtreg w_nu nat_outofplace_rank log_needs_price logx ,  fe vce(robust)
*eststo, title (LR+NR): quietly xtreg w_nu nat_outofplace_rank local_outofplace log_needs_price logx,  fe vce(robust)

eststo, title (A_band): quietly xtreg w_nu i.A_band log_needs_price logx ,  fe vce(robust)

esttab using c:/temp/resnu.tex, mtitle no p nostar numbers nogaps compress title(FE estimation for excess budget weight \$ w\_{\nu}\$ as dependent variable \label{tabExcessFE})

*xtreg logct lnA0 ,  fe vce(robust)