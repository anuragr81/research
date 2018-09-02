rm c:\temp\heckman_wsparselogx.tex
use "C:\temp\allg.dta", clear

eststo clear

*use "C:\temp\gf.dta", clear
eststo, title(food sparse): quietly heckman w_sparse ln_tot_exp consu occupation_rank highest_educ i.has_english years_community roomsnum i.housing_type i.band_electric i.band_agricultural i.band_transport i.band_equipment   i.expensiveregion if tag=="food", select(has_sparse = nonsparsefood_region occupation_rank highest_educ) twostep

*use "C:\temp\ge.dta", clear
eststo, title(energy sparse): quietly heckman w_sparse ln_tot_exp consu occupation_rank highest_educ i.has_english years_community roomsnum i.housing_type i.band_electric i.band_agricultural i.band_transport i.band_equipment   i.expensiveregion if tag=="energy" , select(has_sparse = nonsparseenergy_region occupation_rank highest_educ ) twostep

*use "C:\temp\gp.dta", clear
eststo, title(pers prod sparse): quietly heckman w_sparse ln_tot_exp consu occupation_rank highest_educ i.has_english years_community roomsnum i.housing_type i.band_electric i.band_agricultural i.band_transport i.band_equipment   i.expensiveregion if tag=="personal_products", select(has_sparse = nonsparsepersprd_region occupation_rank highest_educ) twostep 

*use "C:\temp\gt.dta", clear
eststo, title(trans sparse): quietly heckman w_sparse ln_tot_exp consu occupation_rank highest_educ i.has_english years_community roomsnum i.housing_type i.band_electric i.band_agricultural i.band_transport i.band_equipment   i.expensiveregion if tag=="transport", select(has_sparse = nonsparsetransport_region occupation_rank highest_educ ) twostep 

esttab using c:\temp\heckman_wsparselogx.tex, r2 ar2 mtitle no p numbers star nogaps compress title(Heckman results for sparse categories with budget-share as dependent variable \label{tabheckwlogx})
