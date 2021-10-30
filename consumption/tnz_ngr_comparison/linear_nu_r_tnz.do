
args depvar
eststo clear


use "C:\local_files\research\consumption\lsms\data\tn_df2010.dta", clear

* generate has_english =  litlang == 2 | litlang == 3
eststo, title (2010): quietly reg `depvar' logx log_mean_A0 log_mean_cost_ne  i.max_occupation_rank i.max_education_rank hh_age i.is_rural hsize


use "C:\local_files\research\consumption\lsms\data\tn_df2012.dta", clear
eststo, title (2012): quietly reg `depvar' logx log_mean_A0 log_mean_cost_ne  i.max_occupation_rank i.max_education_rank hh_age i.is_rural hsize


use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
eststo, title (2014): quietly reg `depvar' logx log_mean_A0 log_mean_cost_ne  i.max_occupation_rank i.max_education_rank hh_age i.is_rural hsize


esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title(Tanzania: Linear estimation for \$ `depvar' \$ as dependent variable \label{tabExcessLinearTNZ})

 
