
use "C:\temp\df2012_2014.dta", clear

eststo clear

xtset hhid year

eststo, title (2012-2014): quietly xtreg nu r Ar i.max_occupation_rank i.max_education_rank,  fe vce(robust)

esttab using c:/temp/resnu.tex, mtitle no p nostar numbers nogaps compress title(FE estimation for excess \$ \nu\$ as dependent variable \label{tabExcessFE})


*xtreg nu i.max_education_rank i.max_occupation_rank r Ar,  fe vce(robust)
*predict resid, residuals
*hist resid