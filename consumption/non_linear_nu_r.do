
use "C:\temp\df2012.dta", clear

eststo clear

* eststo, title (r_t): quietly nl (nu  = {b0=.1}*(r^{b1=.5}))

eststo, title (r_t): quietly reg nu r i.max_occupation_rank i.max_education_rank

* esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title(Nonlinear estimation for excess budget weight \$ \nu \$ as dependent variable \label{tabExcessNonlinear})

esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title(Linear estimation for excess budget weight \$ \nu \$ as dependent variable \label{tabExcessLinear})

 