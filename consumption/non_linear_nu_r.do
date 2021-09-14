
*use "C:\temp\df2010.dta", clear

eststo clear



use "C:\temp\df2010.dta", clear
eststo, title (2010): quietly reg nu r Ar i.max_occupation_rank i.max_education_rank
* eststo, title (2010): quietly nl (nu  = {b0=.1}*(r^{b1=.5}))

use "C:\temp\df2012.dta", clear
eststo, title (2012): quietly reg nu r Ar i.max_occupation_rank i.max_education_rank
*eststo, title (2012): quietly nl (nu  = {b0=.1}*(r^{b1=.5}))

use "C:\temp\df2014.dta", clear
eststo, title (2014): quietly reg nu r Ar i.max_occupation_rank i.max_education_rank
*eststo, title (2014): quietly nl (nu  = {b0=.1}*(r^{b1=.5}))


*esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title(Nonlinear estimation for excess budget weight \$ \nu \$ as dependent variable \label{tabExcessNonlinear})

esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title(Linear estimation for excess \$ \nu \$ as dependent variable \label{tabExcessLinear})

 