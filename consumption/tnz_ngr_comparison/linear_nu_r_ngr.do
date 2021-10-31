
* eststo, title (2010): quietly reg nu r Ar i.max_occupation_rank i.max_education_rank
* eststo, title (2010): quietly nl (nu  = {b0=.1}*(r^{b1=.5}))


* nl (cost_asset_costs = {b0=1e+6}/(1+exp(-{b1=1e-6}*(A0-{b2=8e+6}) ) ))
* tested with b0 <- 1e+6 ; b1 <- 1.76e-6 ; b2=5e+5; plot(dd$A0,b0/(1+exp(-(b1)*(dd$A0-b2) ) ))


{b0=100}/(1+exp (-m(A-n)) )

args depvar
eststo clear


use "C:\local_files\research\consumption\lsms\data\ngr_df2010.dta", clear
eststo, title (2010): quietly reg `depvar' logx log_mean_A0 log_mean_cost_ne  i.max_occupation_rank i.max_education_rank age i.is_urban hsize


use "C:\local_files\research\consumption\lsms\data\ngr_df2012.dta", clear
eststo, title (2012): quietly reg `depvar' logx log_mean_A0 log_mean_cost_ne  i.max_occupation_rank i.max_education_rank age i.is_urban hsize

use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
eststo, title (2015): quietly reg `depvar' logx log_mean_A0 log_mean_cost_ne  i.max_occupation_rank i.max_education_rank age i.is_urban hsize


esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title(Nigeria: Linear estimation for \$ `depvar' \$ as dependent variable \label{tabExcessLinear}) 

 

