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
