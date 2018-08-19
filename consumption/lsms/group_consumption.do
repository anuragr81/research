* use data from function: get_allgrp_food_data_frame

use "C:\local_files\research\consumption\datamapper\allGrpsDS.dta", clear
eststo clear
eststo, title(starch): quietly regress lnunitvalue_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "starch"
eststo, title(vegstarch): quietly regress lnunitvalue_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "vegstarch"
eststo, title(fruits): quietly regress lnunitvalue_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "fruits"
eststo, title(meat): quietly regress lnunitvalue_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "meat"
eststo, title(alcohol): quietly regress lnunitvalue_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "alcohol"
eststo, title(oil): quietly regress lnunitvalue_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "oil"
eststo, title(beverages): quietly regress lnunitvalue_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "beverages"
eststo, title(milk): quietly regress lnunitvalue_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "milk"
eststo, title(fat): quietly regress lnunitvalue_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "fat"
eststo, title(vegetables): quietly regress lnunitvalue_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "vegetables"
eststo, title(sugars): quietly regress lnunitvalue_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "sugars"
eststo, title(condiments): quietly regress lnunitvalue_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "condiments"
esttab using c:/temp/unitvalue_results.tex, r2 ar2 mtitle no p numbers star nogaps compress title(Unit Value Regression for food groups\label{tabunitvalue})


eststo clear
eststo, title(starch): quietly regress lnmerge_quantity_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "starch"
eststo, title(vegstarch): quietly regress lnmerge_quantity_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "vegstarch"
eststo, title(fruits): quietly regress lnmerge_quantity_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "fruits"
eststo, title(meat): quietly regress lnmerge_quantity_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "meat"
eststo, title(alcohol): quietly regress lnmerge_quantity_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "alcohol"
eststo, title(oil): quietly regress lnmerge_quantity_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "oil"
eststo, title(beverages): quietly regress lnmerge_quantity_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "beverages"
eststo, title(milk): quietly regress lnmerge_quantity_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "milk"
eststo, title(fat): quietly regress lnmerge_quantity_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "fat"
eststo, title(vegetables): quietly regress lnmerge_quantity_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "vegetables"
eststo, title(sugars): quietly regress lnmerge_quantity_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "sugars"
eststo, title(condiments): quietly regress lnmerge_quantity_a lnx_a occupation_rank_a hsize_a age_a roomsnum_a if group == "condiments"
esttab using c:/temp/mergedq_results.tex, r2 ar2 mtitle no p numbers star nogaps compress title(Quantity consumed Regression for food groups\label{tabmergedq})

* For fitting in the landscape view, use {landscape} (after inserting usepackage lscape in preamble) and encapsulate within \resizebox{\columnwidth}{!} { \begin{tabular} ... \end{tabular} }


use c:\temp\ds.dta,clear

eststo clear
eststo, title(energy): quietly reg highratio ln_tot_categ_exp highest_educ popdensity i.housingstatus roomsnum i.is_resident electronics_asset_score if tag == "energy", robust

esttab using c:/temp/group_expenditure_results.tex, r2 ar2 mtitle no p numbers star nogaps compress title(Status measure regression for separable categories\label{tabgroups})

* For mlogit results

use c:\temp\ds.dta,clear

eststo clear
eststo, title(transport): quietly mlogit highest_asset lntot consu i.isurbanp popdensity highest_educ roomsnum i.has_english i.expensiveregion if tag == "transport", base(0) 

esttab using c:/temp/group_expenditure_mlogit_results.tex, r2 ar2 mtitle no p numbers star nogaps compress title(Status measure regression for separable categories\label{tabgroups})

*

use c:\temp\rescateg.dta,clear

eststo clear
* has_high and expensive region are mutually exclusive for food, energy
eststo, title(food): quietly reg w ln_tot_exp highest_educ consu age i.expensiveregion popdensity i.housingstatus roomsnum i.is_resident if tag == "food", robust 

eststo, title(energy): quietly reg w ln_tot_exp highest_educ i.expensiveregion popdensity years_community i.is_resident  if tag == "energy", robust

eststo, title(transport): quietly reg w ln_tot_exp consu age popdensity i.housingstatus i.is_resident i.has_high if tag == "transport", robust

eststo, title(personal_products): quietly reg w ln_tot_exp highest_educ age popdensity i.housingstatus roomsnum ln_asset_score  if tag == "personal_products", robust

eststo, title(social_functions): quietly reg w ln_tot_exp consu popdensity i.housingstatus roomsnum i.is_resident if tag == "social_functions", robust

eststo, title(housing): quietly reg w ln_tot_exp consu age i.expensiveregion i.housingstatus years_community roomsnum ln_asset_score  if tag == "housing", robust


* not considering tothouserent toteducexpense
* ln_tot_exp highest_educ consu age i.expensiveregion popdensity i.housingstatus years_community roomsnum i.is_resident i.has_high ln_asset_score 

esttab using c:/temp/group_expenditure_results_1ststage.tex, r2 ar2 mtitle no p numbers star nogaps compress title(First-stage regression for separable categories with $w$ as dependent variable \label{tabgroupsfirststage})



**** Second stage ****

eststo clear

* ln_tot_categ_exp highest_educ consu age i.expensiveregion popdensity i.housingstatus years_community roomsnum i.is_resident

eststo, title(food): quietly reg highratio ln_tot_categ_exp highest_educ consu popdensity i.housingstatus if tag == "food", robust 

eststo, title(energy): quietly reg highratio ln_tot_categ_exp highest_educ  age i.expensiveregion i.housingstatus roomsnum i.is_resident if tag == "energy", robust

eststo, title(transport): quietly reg highratio ln_tot_categ_exp highest_educ i.expensiveregion popdensity i.housingstatus years_community if tag == "transport", robust

** assets 
* ln_tot_exp highest_educ consu age i.expensiveregion popdensity i.housingstatus years_community roomsnum i.is_resident

eststo, title(personal_products): quietly reg pe ln_tot_exp highest_educ consu age i.housingstatus roomsnum if tag == "personal_products", robust

eststo, title(social_functions): quietly reg pe ln_tot_exp popdensity i.housingstatus  if tag == "social_functions", robust

eststo, title(housing): quietly reg pe ln_tot_exp consu age popdensity i.housingstatus roomsnum  if tag == "housing", robust

esttab using c:/temp/group_expenditure_results_2ndstage.tex, r2 ar2 mtitle no p numbers star nogaps compress title(Second-stage regression for separable categories with $\lambda/\zeta$ as dependent variable \label{tabgroupssecondstage})




* housingstatus - 1 owned 2,3 employer provided 4 rented 5 free 6 nomad 


************* simple w reg *******************

use "C:\temp\allg.dta", clear
eststo clear

eststo, title(food): quietly reg w ln_tot_exp ln_tot_asset_cost if tag == "food", robust 

eststo, title(energy): quietly reg w ln_tot_exp ln_tot_asset_cost if tag == "energy", robust

eststo, title(transport): quietly reg w ln_tot_exp ln_tot_asset_cost if tag == "transport", robust

eststo, title(personal_products): quietly reg w ln_tot_exp ln_tot_asset_cost if tag == "personal_products", robust

eststo, title(social_functions): quietly reg w ln_tot_exp  ln_tot_asset_cost if tag == "social_functions", robust

eststo, title(housing): quietly reg w ln_tot_exp  ln_tot_asset_cost if tag == "housing", robust

esttab using c:/temp/allw.tex, r2 ar2 mtitle no p numbers star nogaps compress title(OLS results for separable categories with \$w\$ as dependent variable \label{tabwlogx})

