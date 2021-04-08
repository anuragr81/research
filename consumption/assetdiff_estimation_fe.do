
* use "C:\temp\df2010_2012.dta", clear
* use "C:\temp\df2012_2014.dta", clear

** try with fe estimator from the output of get_band_metrics(estimation_df_budget_quantile(ll,e) , b= 1.2)
eststo clear

xtset hhid year
eststo, title(RP): quietly xtreg w_nu local_outofplace log_needs_price logx ,  fe vce(robust)
eststo, title (BW): quietly xtreg w_nu richness_rank log_needs_price logx ,  fe vce(robust)
eststo, title (BW+RP): quietly xtreg w_nu richness_rank local_outofplace log_needs_price logx,  fe vce(robust)

esttab using c:/temp/resnu.tex, mtitle no p numbers star nogaps compress title(FE estimation for excess budget weight \$ w\_{\nu}\$ as dependent variable \label{tabExcessFE})

