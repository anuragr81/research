
eststo clear

use "C:\local_files\research\consumption\lsms\data\ngr_df2010.dta", clear
eststo, title (2010): quietly sqreg log_q_ne logx log_mean_A0 log_mean_cost_ne  i.max_occupation_rank i.max_education_rank age, q(.30 .60 .90) reps(100)

use "C:\local_files\research\consumption\lsms\data\ngr_df2012.dta", clear
eststo, title (2012): quietly sqreg log_q_ne logx log_mean_A0 log_mean_cost_ne  i.max_occupation_rank i.max_education_rank age, q(.30 .60 .90) reps(100)

use "C:\local_files\research\consumption\lsms\data\ngr_df2015.dta", clear

eststo, title (2015): quietly sqreg log_q_ne logx log_mean_A0 log_mean_cost_ne  i.max_occupation_rank i.max_education_rank age, q(.30 .60 .90) reps(100)


esttab using c:/temp/resnu.tex, mtitle no p numbers star nogaps compress
* copy table after running estout works best (and running own script to tabulate works best) - make sure that formatting of target cells is text
estout, cells(b(star fmt(3)) se(par fmt(2)))
