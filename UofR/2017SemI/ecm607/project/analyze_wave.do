

cd C:\Users\anuragr\Documents\UofR\2017SemI\ecm607\project
clear
use US_data6W_2
drop if wave !=3
* drop if big5o etc. are missing (1116 entries out of 15533)
drop if big5o_dv<0 | big5c_dv<0| big5e_dv<0| big5a_dv<0| big5n_dv<0

* generation of dummies etc. for analysis
decode jbterm1, generate (job_term)
decode jbsoc00_cc, generate (job_category)
decode racel_dv, generate (racel_dvd)

gen ln_paygu_dv = log(paygu_dv)
gen exper = age_cr - scend
gen exper_sq = exper * exper


decode jbstat, generate (job_status)

generate chosen_job_category= 1  if( job_category == "Administrative occupations: finance"||  job_category =="Administrative occupations: records"||  job_category =="Administrative occupations: communications"||  job_category =="Administrative occupations: general"||  job_category =="Secretarial and related occupations"||  job_category =="Elementary administration occupations" )
replace chosen_job_category= 2  if( job_category == "Managers in farming, horticulture, forestry and services"||  job_category =="Agricultural trades"||  job_category =="Elementary agricultural occupations" )
replace chosen_job_category= 3  if( job_category == "Artistic and literary occupations"||  job_category =="Design associate professionals" )
replace chosen_job_category= 4  if( job_category == "Corporate managers and senior officials"||  job_category =="Functional managers"||  job_category =="Financial institution and office managers"||  job_category =="Managers in distribution, storage and retailing"||  job_category =="Business and statistical professionals"||  job_category =="Business and finance associate professionals"||  job_category =="Managers and proprietors in other service industries" )
replace chosen_job_category= 5  if( job_category == "Engineering professionals"||  job_category =="Information and communication technology professionals" )
replace chosen_job_category= 6  if( job_category == "Architects, town planners, surveyors"||  job_category =="Draughtspersons and building inspectors"||  job_category =="Conservation associate professionals"||  job_category =="Construction trades"||  job_category =="Building trades"||  job_category =="Construction operatives"||  job_category =="Elementary construction occupations" )
replace chosen_job_category= 7  if( job_category == "Quality and customer care managers"||  job_category =="Leisure and travel service occupations"||  job_category =="Hairdressers and related occupations"||  job_category =="Housekeeping occupations"||  job_category =="Personal services occupations nec"||  job_category =="Customer service occupations"||  job_category =="Mobile machine drivers and operatives"||  job_category =="Elementary personal services occupations"||  job_category =="Elementary cleaning occupations"||  job_category =="Managers and proprietors in hospitality and leisure services"||  job_category =="Sports and fitness occupations" )
replace chosen_job_category= 8  if( job_category == "Health and social services managers"||  job_category =="Health professionals"||  job_category =="Health associate professionals"||  job_category =="Therapists"||  job_category =="Social welfare associate professionals"||  job_category =="Healthcare and related personal services"||  job_category =="Childcare and related personal services"||  job_category =="Animal care services" )
replace chosen_job_category= 9  if( job_category == "Legal professionals"||  job_category =="Legal associate professionals" )
replace chosen_job_category= 10  if( job_category == "Media associate professionals" )
replace chosen_job_category= 11  if( job_category == "Production managers"||  job_category =="Metal forming, welding and related trades"||  job_category =="Metal machining, fitting and instrument making trades"||  job_category =="Vehicle trades"||  job_category =="Electrical trades"||  job_category =="Textiles and garments trades"||  job_category =="Printing trades"||  job_category =="Food preparation trades"||  job_category =="Skilled trades nec"||  job_category =="Process operatives"||  job_category =="Plant and machine operatives"||  job_category =="Assemblers and routine operatives"||  job_category =="Elementary process plant occupations"||  job_category =="Elementary goods storage occupations" )
replace chosen_job_category= 12  if( job_category == "Public service professionals"||  job_category =="Librarians and related professionals"||  job_category =="Public service and other associate professionals"||  job_category =="Administrative occupations: government and related organisat"||  job_category =="Teaching professionals" )
replace chosen_job_category= 13  if( job_category == "It service delivery occupations"||  job_category =="Sales and related associate professionals"||  job_category =="Sales assistants and retail cashiers"||  job_category =="Sales related occupations"||  job_category =="Elementary sales occupations" )
replace chosen_job_category= 14  if( job_category == "Science professionals"||  job_category =="Research professionals"||  job_category =="Science and engineering technicians" )
replace chosen_job_category= 15  if( job_category == "Protective service officers"||  job_category =="Protective service occupations"||  job_category =="Elementary security occupations" )
replace chosen_job_category= 16  if( job_category == "Transport associate professionals"||  job_category =="Transport drivers and operatives" )



generate race_rank=1 if racel_dvd=="white: british/english/scottish/welsh/northern irish"
replace race_rank=2 if racel_dvd=="white: any other white background"
replace race_rank=3 if racel_dvd=="asian/asian british: indian"
replace race_rank=4 if racel_dvd=="white: irish"
replace race_rank=5 if racel_dvd=="black/african/caribbean/black british: african"
replace race_rank=6 if racel_dvd=="black/african/caribbean/black british: caribbean"
replace race_rank=7 if racel_dvd=="asian/asian british: pakistani"
replace race_rank=8 if racel_dvd=="missing"
replace race_rank=9 if racel_dvd=="asian/asian british: any other asian background"
replace race_rank=10 if racel_dvd=="mixed: white and black caribbean"
replace race_rank=11 if racel_dvd=="asian/asian british: bangladeshi"
replace race_rank=12 if racel_dvd=="mixed: white and asian"
replace race_rank=13 if racel_dvd=="mixed: any other mixed background"
replace race_rank=14 if racel_dvd=="asian/asian british: chinese"
replace race_rank=15 if racel_dvd=="other ethnic group: any other ethnic group"
replace race_rank=16 if racel_dvd=="mixed: white and black african"
replace race_rank=17 if racel_dvd=="other ethnic group: arab"
replace race_rank=18 if racel_dvd=="black/african/caribbean/black british: any other black backg"
replace race_rank=19 if racel_dvd=="white: gypsy or irish traveller"

* remove missing/invalid entries
drop if hiqual_dv <=-1 |  racel_dv<=-1 | gor_dv <=0  

* discard if neither a permanent job nor unemployed (students, retirees and disabled)
drop if job_term != "a permanent job" & jbstat != 3
drop if pasoc10_cc <0 | sclfsat1<0
* perform unemployment related regressions
gen unemployed = jbstat==3
gen employed = !unemployed
count if unemployed == 1

* probit for employment
eststo clear
eststo : quietly probit employed i.educ_level i.big5o_dv i.big5c_dv i.big5e_dv i.big5a_dv i.big5n_dv i.sclfsat1 scend race_rank
esttab using results_unemp_probit.tex, r2 ar2 pr2 nomtitles no p numbers star nogaps compress title(Heckman Regression\label{tabheckman})

* generate educ_level from hiqual_dv
generate educ_level= 0 if hiqual_dv== 9
replace educ_level= 1 if hiqual_dv== 5
replace educ_level= 2 if hiqual_dv== 4
replace educ_level= 3 if hiqual_dv== 3
replace educ_level= 4 if hiqual_dv== 1
replace educ_level= 5 if hiqual_dv== 2

* replace ln_paygu_dv =0 if jbstat==3
* run tobit, heckman
tobit ln_paygu_dv i.educ_level exper exper_sq i.gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex, ll(0)
eststo clear
eststo : quietly regress ln_paygu_dv i.educ_level exper exper_sq i.gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex, robust
eststo : quietly heckman ln_paygu_dv i.educ_level exper exper_sq i.gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex, select(employed = i.educ_level exper exper_sq i.gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex i.sclfsat1) twostep
esttab using results_heckman.csv, r2 ar2 pr2 nomtitles no p numbers star nogaps compress title(Heckman Regression\label{tabheckman})
* discard unemployment related entries 
drop if job_term != "a permanent job" 
drop if jbsoc00_cc<=0
* remove non specified job types for further analysis
drop if chosen_job_category < 0



generate gor_rank= 1 if gor_dv== 10
replace gor_rank= 2 if gor_dv== 3
replace gor_rank= 3 if gor_dv== 9
replace gor_rank= 4 if gor_dv== 4
replace gor_rank= 5 if gor_dv== 5
replace gor_rank= 6 if gor_dv== 12
replace gor_rank= 7 if gor_dv== 1
replace gor_rank= 8 if gor_dv== 2
replace gor_rank= 9 if gor_dv== 11
replace gor_rank= 10 if gor_dv== 6
replace gor_rank= 11 if gor_dv== 8
replace gor_rank= 12 if gor_dv== 7


// regressions

* the mprobit on personalities alone

* mprobit with factor variables does not converge
** mprobit chosen_job_category scend i.gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv i.racel_dv i.sex_cr i.hiqual_dv
mprobit chosen_job_category scend gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv race_rank sex_cr educ_level
* eststo: quietly margins, dydx(educ_level)

* esttab using margins_educ_level.tex, p numbers star nogaps compress title(Margins for Education \label{margins_educ})


* Part II
* considering only 1,4,5,6,7,8,11,12,13,14,15,16
eststo clear
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==1,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==4,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==5,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==6,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==7,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==8,robust
esttab using results_plainreg1.tex, r2 nomtitles no p numbers star nogaps compress title(Wage Regression\label{tabplainreg1})

eststo clear
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==11,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==12,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==13,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==14,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==15,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==16,robust

esttab using results_plainreg2.tex, r2 nomtitles no p numbers star nogaps compress title(Wage Regression\label{tabplainreg2})



eststo clear
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==1,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==4,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==5,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==6,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==7,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==8,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==11,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==12,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==13,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==14,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==15,robust
eststo: quietly regress ln_paygu_dv exper exper_sq gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv sex_cr ib3.hiqual_dv if chosen_job_category ==16,robust

esttab using results_plainreg.csv, r2 nomtitles no p numbers star nogaps compress title(Wage Regression\label{tabplainreg2})

