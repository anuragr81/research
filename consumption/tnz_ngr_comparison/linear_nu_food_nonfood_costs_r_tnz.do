
args depvar file1 file2 file3 
eststo clear

display "file1=`file1'"
display "file2=`file2'"
display "file3=`file3'"

* use "C:\local_files\research\consumption\lsms\data\tn_df2010.dta", clear
* use `file1', clear
* eststo, title (2010): quietly reg `depvar' logx log_mean_A0 log_mean_cost_ne_food log_mean_cost_ne_nonfood  i.max_occupation_rank i.max_education_rank age i.isrural hsize


*use "C:\local_files\research\consumption\lsms\data\tn_df2012.dta", clear
use `file2', clear

gen log_q_ne_x = log_q_ne - log(hsize)

eststo, title (2012): quietly reg `depvar' logx log_mean_A0 log_mean_cost_ne_food log_mean_cost_ne_nonfood  max_occupation_rank max_education_rank age i.isrural hsize


*use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
use `file3', clear

gen log_q_ne_x = log_q_ne - log(hsize)

eststo, title (2014): quietly reg `depvar' logx log_mean_A0 log_mean_cost_ne_food log_mean_cost_ne_nonfood  max_occupation_rank max_education_rank age hsize


esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title(Tanzania: Linear estimation for \$ `depvar' \$ as dependent variable \label{tabExcessLinearTNZ})

 
