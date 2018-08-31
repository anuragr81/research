rm c:\temp\allw.tex

use "C:\temp\allg.dta", clear
eststo clear

eststo, title(food): quietly reg w ln_tot_exp consu occupation_rank i.has_english years_community roomsnum i.housing_type ln_electric ln_agricultural ln_transport ln_equipment ln_housing  i.expensiveregion if tag == "food", robust 

eststo, title(energy): quietly reg w ln_tot_exp consu occupation_rank i.has_english years_community roomsnum i.housing_type ln_electric ln_agricultural ln_transport ln_equipment ln_housing  i.expensiveregion if tag == "energy", robust

eststo, title(transport): quietly reg w ln_tot_exp consu occupation_rank i.has_english years_community roomsnum i.housing_type ln_electric ln_agricultural ln_transport ln_equipment ln_housing  i.expensiveregion if tag == "transport", robust

eststo, title(personal_products): quietly reg w ln_tot_exp consu occupation_rank i.has_english years_community roomsnum i.housing_type ln_electric ln_agricultural ln_transport ln_equipment ln_housing  i.expensiveregion if tag == "personal_products", robust

eststo, title(social_functions): quietly reg w ln_tot_exp consu occupation_rank i.has_english years_community roomsnum i.housing_type  ln_electric ln_agricultural ln_transport ln_equipment ln_housing  i.expensiveregion if tag == "social_functions", robust

eststo, title(housing): quietly reg w ln_tot_exp consu occupation_rank i.has_english years_community roomsnum i.housing_type  ln_electric ln_agricultural ln_transport ln_equipment ln_housing  i.expensiveregion if tag == "housing", robust

eststo, title(education): quietly reg w ln_tot_exp consu occupation_rank i.has_english years_community roomsnum i.housing_type  ln_electric ln_agricultural ln_transport ln_equipment ln_housing  i.expensiveregion if tag == "education", robust

esttab using c:/temp/allw.tex, r2 ar2 mtitle no p numbers star nogaps compress title(OLS results for separable categories with \$w\$ as dependent variable \label{tabwlogx})

