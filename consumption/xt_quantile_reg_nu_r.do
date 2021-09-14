use "C:\temp\df2010.dta", clear

eststo clear

eststo, title (2010): quietly sqreg nu r Ar i.max_occupation_rank i.max_education_rank, q(.30 .60 .90) reps(100)

use "C:\temp\df2012.dta", clear
eststo, title (2012): quietly sqreg nu r Ar i.max_occupation_rank i.max_education_rank, q(.30 .60 .90) reps(100)

use "C:\temp\df2014.dta", clear
eststo, title (2014): quietly sqreg nu r Ar i.max_occupation_rank i.max_education_rank, q(.30 .60 .90) reps(100)


esttab using c:/temp/resnu.tex, mtitle no p numbers star nogaps compress
* copy table after running estout works best (and running own script to tabulate works best) - make sure that formatting of target cells is text
estout, cells(b(star fmt(3)) se(par fmt(2)))
