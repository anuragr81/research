
eststo clear


use "C:\local_files\research\consumption\lsms\data\ngr_df2010.dta", clear

* generate has_english =  litlang == 2 | litlang == 3
generate log_mean_A0 = log(mean_A0)
generate log_mean_cost_ne = log(mean_cost_ne)


* eststo, title (2010): quietly reg nu r Ar i.max_occupation_rank i.max_education_rank
* eststo, title (2010): quietly nl (nu  = {b0=.1}*(r^{b1=.5}))

eststo, title (2010): quietly reg w_ne logx log_mean_A0 log_mean_cost_ne  i.max_occupation_rank i.max_education_rank age


* use "C:\temp\df2012.dta", clear

use "C:\local_files\research\consumption\lsms\data\ngr_df2012.dta", clear

* generate has_english =  litlang == 2 | litlang == 3
generate log_mean_A0 = log(mean_A0)
generate log_mean_cost_ne = log(mean_cost_ne)

* eststo, title (2012): quietly reg nu r Ar i.max_occupation_rank i.max_education_rank
* eststo, title (2012): quietly nl (nu  = {b0=.1}*(r^{b1=.5}))
eststo, title (2012): quietly reg w_ne logx log_mean_A0 log_mean_cost_ne  i.max_occupation_rank i.max_education_rank age

* use "C:\temp\df2014.dta", clear
use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear
* generate has_english =  litlang == 2 | litlang == 3
generate log_mean_A0 = log(mean_A0)
generate log_mean_cost_ne = log(mean_cost_ne)

*eststo, title (2014): quietly reg nu r Ar i.max_occupation_rank i.max_education_rank
*eststo, title (2014): quietly nl (nu  = {b0=.1}*(r^{b1=.5}))

eststo, title (2015): quietly reg w_ne logx log_mean_A0 log_mean_cost_ne  i.max_occupation_rank i.max_education_rank age


*esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title(Nonlinear estimation for excess budget weight \$ \nu \$ as dependent variable \label{tabExcessNonlinear})

esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title(Linear estimation for excess weight \$ w_{ne} \$ as dependent variable \label{tabExcessLinear})

 