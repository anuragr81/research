
use "C:\temp\df2012.dta", clear

eststo clear

eststo, title (A_band): quietly nl (nu  = {b0=.1}*(r^{b1=.5}))

esttab using c:/temp/resnu.tex, mtitle no p numbers nogaps compress title(FE estimation for excess budget weight \$ \nu \$ as dependent variable \label{tabExcessNonlinear})

