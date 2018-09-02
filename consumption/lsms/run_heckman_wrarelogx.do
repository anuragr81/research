rm c:\temp\heckman_wrarelogx.tex
use "C:\temp\allg.dta", clear

eststo clear

*use "C:\temp\gf.dta", clear
eststo, title(food rare): quietly heckman w_rare ln_tot_exp consu occupation_rank highest_educ i.has_english years_community roomsnum i.housing_type i.band_electric i.band_agricultural i.band_transport i.band_equipment   i.expensiveregion if tag=="food", select(has_sparse = nonrarefood_region occupation_rank highest_educ) twostep

*use "C:\temp\ge.dta", clear
eststo, title(energy rare): quietly heckman w_rare ln_tot_exp consu occupation_rank highest_educ i.has_english years_community roomsnum i.housing_type i.band_electric i.band_agricultural i.band_transport i.band_equipment   i.expensiveregion if tag=="energy" , select(has_sparse = nonrareenergy_region occupation_rank highest_educ ) twostep

*use "C:\temp\gp.dta", clear
eststo, title(pers prod rare): quietly heckman w_rare ln_tot_exp consu occupation_rank highest_educ i.has_english years_community roomsnum i.housing_type i.band_electric i.band_agricultural i.band_transport i.band_equipment   i.expensiveregion if tag=="personal_products", select(has_sparse = nonrarepersprd_region occupation_rank highest_educ) twostep 

*use "C:\temp\gt.dta", clear
eststo, title(trans rare): quietly heckman w_rare ln_tot_exp consu occupation_rank highest_educ i.has_english years_community roomsnum i.housing_type i.band_electric i.band_agricultural i.band_transport i.band_equipment   i.expensiveregion if tag=="transport", select(has_sparse = nonraretransport_region occupation_rank highest_educ ) twostep 

esttab using c:\temp\heckman_wrarelogx.tex, r2 ar2 mtitle no p numbers star nogaps compress title(OLS results for rare categories with budget-share as dependent variable \label{tabheckwlogx})
