
use "C:\temp\df2010_2012.dta", clear

eststo clear

xtset hhid year

eststo, title (2010-2012): quietly xtreg nu r i.max_occupation_rank i.max_education_rank,  fe vce(robust)

esttab using c:/temp/resnu.tex, mtitle no p nostar numbers nogaps compress title(FE estimation for excess budget weight \$ \nu\$ as dependent variable \label{tabExcessFE})

