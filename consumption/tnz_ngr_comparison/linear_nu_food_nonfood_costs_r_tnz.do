
args depvar foodpricevar nonfoodpricevar file1 file2 file3 
eststo clear

display "file1=`file1'"
display "file2=`file2'"
display "file3=`file3'"

* use "C:\local_files\research\consumption\lsms\data\tn_df2010.dta", clear
* use `file1', clear
* eststo, title (2010): quietly reg `depvar' logx log_mean_A0 `foodpricevar' `nonfoodpricevar'  i.max_occupation_rank i.max_education_rank age rural_wards hsize


*use "C:\local_files\research\consumption\lsms\data\tn_df2012.dta", clear
use `file2', clear

gen log_q_ne_x = log_q_ne - log(hsize)

eststo, title (2012): quietly reg `depvar' logx log_mean_A0 `foodpricevar' `nonfoodpricevar'  max_occupation_rank max_education_rank age rural_wards hsize


*use "C:\local_files\research\consumption\lsms\data\tn_df2014.dta", clear
use `file3', clear

gen log_q_ne_x = log_q_ne - log(hsize)

eststo, title (2014): quietly reg `depvar' logx log_mean_A0 `foodpricevar' `nonfoodpricevar'  max_occupation_rank max_education_rank age rural_wards hsize


esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title(Tanzania: \$ `depvar' \$ \label{tab`depvar'TNZ})

 
