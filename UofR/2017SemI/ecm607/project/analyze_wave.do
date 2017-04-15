

cd C:\Users\anuragr\Documents\UofR\2017SemI\ecm607\project
clear
use US_data6W
drop if wave !=3
* drop if big5o are invalid

* generation of dummies etc. for analysis
decode jbterm1, generate (job_status)
decode jbsoc00_cc, generate (job_type)
decode racel_dv, generate (racel_dvd)

gen ln_paygu_dv = log(paygu_dv)


// regressions
/*
regress paygu_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.hiqual_dv if hiqual_dv >-1 & job_status != "a permanent job"

regress paygu_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.hiqual_dv if hiqual_dv >-1 & age_cr>20 & age_cr<35 & job_status != "a permanent job"

regress paygu_dv i.gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.jbsoc00_cc i.hiqual_dv if hiqual_dv >-1 & job_status == "a permanent job" & gor_dv >0 & jbsoc00_cc>0


regress ln_paygu_dv i.gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.jbsoc00_cc i.hiqual_dv if hiqual_dv >-1 & job_status == "a permanent job" & gor_dv >0 & jbsoc00_cc>0

regress ln_paygu_dv i.gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr b711.jbsoc00_cc i.hiqual_dv i.jbnssec_dv if hiqual_dv >-1 & job_status == "a permanent job" & gor_dv >0 & jbsoc00_cc>0 & jbnssec_dv>0

regress ln_paygu_dv i.livesp_dv nchild_dv maju paju i.pasoc10_cc scend i.gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv i.sex_cr i.jbsoc00_cc i.hiqual_dv i.jbnssec_dv if hiqual_dv >-1 & job_status == "a permanent job" & gor_dv >0 & jbsoc00_cc>0 & jbnssec_dv>0
// production doesn't have maju, paju as significant
// mixed-race people seemingly are rewarded in production sector jobs - but that factor becomes less significant when father's occupation is added (indicating that people of mixed race usually have fathers in middle paying jobs)
*/
// production

generate byte chosen_job_type=1 if( job_type == "Production managers"||  job_type =="Metal forming, welding and related trades"||  job_type =="Metal machining, fitting and instrument making trades"||  job_type =="Vehicle trades"||  job_type =="Electrical trades"||  job_type =="Textiles and garments trades"||  job_type =="Printing trades"||  job_type =="Food preparation trades"||  job_type =="Skilled trades nec"||  job_type =="Process operatives"||  job_type =="Plant and machine operatives"||  job_type =="Assemblers and routine operatives"||  job_type =="Elementary process plant occupations"||  job_type =="Elementary goods storage occupations" )  
replace chosen_job_type=0 if chosen_job_type!=1
quietly regress ln_paygu_dv i.livesp_dv scend i.gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv i.sex_cr i.hiqual_dv i.jbnssec_dv if hiqual_dv >-1 &  racel_dv>-1 & job_status == "a permanent job" & gor_dv >0 & jbsoc00_cc>0 & jbnssec_dv>0 & chosen_job_type==1, robust


drop chosen_job_type
generate byte chosen_job_type=1 if( job_type == "Health and social services managers"||  job_type =="Health professionals"||  job_type =="Health associate professionals"||  job_type =="Therapists"||  job_type =="Social welfare associate professionals"||  job_type =="Healthcare and related personal services"||  job_type =="Childcare and related personal services"||  job_type =="Animal care services" )  
replace chosen_job_type=0 if chosen_job_type!=1
// quietly regress ln_paygu_dv i.livesp_dv scend i.gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv i.sex_cr i.hiqual_dv i.jbnssec_dv if hiqual_dv >-1 &  racel_dv>-1 & job_status == "a permanent job" & gor_dv >0 & jbsoc00_cc>0 & jbnssec_dv>0 & chosen_job_type==1, robust
// we need multinomial/ordered for jbnssec_dv against (personality, age, educ, scend)

drop chosen_job_type
generate byte chosen_job_type=1 if( job_type == "Artistic and literary occupations"||  job_type =="Design associate professionals" )
replace chosen_job_type=0 if chosen_job_type!=1

regress ln_paygu_dv i.livesp_dv scend i.gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv i.sex_cr i.hiqual_dv i.jbnssec_dv if hiqual_dv >-1 &  racel_dv>-1 & job_status == "a permanent job" & gor_dv >0 & jbsoc00_cc>0 & jbnssec_dv>0 & chosen_job_type==1, robust

drop chosen_job_type
generate byte chosen_job_type=1 if( job_type == "Public service professionals"||  job_type =="Librarians and related professionals"||  job_type =="Public service and other associate professionals"||  job_type =="Administrative occupations: government and related organisat" ) 
replace chosen_job_type=0 if chosen_job_type!=1
regress ln_paygu_dv i.livesp_dv scend i.gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv i.sex_cr i.hiqual_dv i.jbnssec_dv if hiqual_dv >-1 &  racel_dv>-1 & job_status == "a permanent job" & gor_dv >0 & jbsoc00_cc>0 & jbnssec_dv>0 & chosen_job_type==1, robust

drop chosen_job_type
generate byte chosen_job_type=1 if( job_type == "Financial institution and office managers"||  job_type =="Administrative occupations: finance" ) 
replace chosen_job_type=0 if chosen_job_type!=1
regress ln_paygu_dv i.livesp_dv scend i.gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv i.sex_cr i.hiqual_dv i.jbnssec_dv if hiqual_dv >-1 &  racel_dv>-1 & job_status == "a permanent job" & gor_dv >0 & jbsoc00_cc>0 & jbnssec_dv>0 & chosen_job_type==1, robust

drop chosen_job_type
generate byte chosen_job_type=1 if( job_type == "Engineering professionals"||  job_type =="Information and communication technology professionals" ) 
replace chosen_job_type=0 if chosen_job_type!=1
regress ln_paygu_dv i.livesp_dv scend i.gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv i.sex_cr i.hiqual_dv i.jbnssec_dv if hiqual_dv >-1 &  racel_dv>-1 & job_status == "a permanent job" & gor_dv >0 & jbsoc00_cc>0 & jbnssec_dv>0 & chosen_job_type==1, robust

drop chosen_job_type
generate byte chosen_job_type=1 if( job_type == "Science professionals"||  job_type =="Research professionals"||  job_type =="Science and engineering technicians" ) 
replace chosen_job_type=0 if chosen_job_type!=1
regress ln_paygu_dv i.livesp_dv scend i.gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv i.sex_cr i.hiqual_dv i.jbnssec_dv if hiqual_dv >-1 &  racel_dv>-1 & job_status == "a permanent job" & gor_dv >0 & jbsoc00_cc>0 & jbnssec_dv>0 & chosen_job_type==1, robust

drop chosen_job_type
generate byte chosen_job_type=1 if( job_type == "Sales and related associate professionals"||  job_type =="Sales assistants and retail cashiers"||  job_type =="Sales related occupations"||  job_type =="Elementary sales occupations" )
replace chosen_job_type=0 if chosen_job_type!=1
regress ln_paygu_dv i.livesp_dv scend i.gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv age_cr i.racel_dv i.sex_cr i.hiqual_dv i.jbnssec_dv if hiqual_dv >-1 &  racel_dv>-1 & job_status == "a permanent job" & gor_dv >0 & jbsoc00_cc>0 & jbnssec_dv>0 & chosen_job_type==1, robust



generate chosen_job_type = 0
replace chosen_job_type= 1  if( job_type == "Administrative occupations: finance"||  job_type =="Administrative occupations: records"||  job_type =="Administrative occupations: communications"||  job_type =="Administrative occupations: general"||  job_type =="Secretarial and related occupations"||  job_type =="Elementary administration occupations" )
replace chosen_job_type= 2  if( job_type == "Managers in farming, horticulture, forestry and services"||  job_type =="Agricultural trades"||  job_type =="Elementary agricultural occupations" )
replace chosen_job_type= 3  if( job_type == "Artistic and literary occupations"||  job_type =="Design associate professionals" )
replace chosen_job_type= 4  if( job_type == "Corporate managers and senior officials"||  job_type =="Functional managers"||  job_type =="Financial institution and office managers"||  job_type =="Managers in distribution, storage and retailing"||  job_type =="Business and statistical professionals"||  job_type =="Business and finance associate professionals"||  job_type =="Managers and proprietors in other service industries" )
replace chosen_job_type= 5  if( job_type == "Engineering professionals"||  job_type =="Information and communication technology professionals" )
replace chosen_job_type= 6  if( job_type == "Architects, town planners, surveyors"||  job_type =="Draughtspersons and building inspectors"||  job_type =="Conservation associate professionals"||  job_type =="Construction trades"||  job_type =="Building trades"||  job_type =="Construction operatives"||  job_type =="Elementary construction occupations" )
replace chosen_job_type= 7  if( job_type == "Quality and customer care managers"||  job_type =="Leisure and travel service occupations"||  job_type =="Hairdressers and related occupations"||  job_type =="Housekeeping occupations"||  job_type =="Personal services occupations nec"||  job_type =="Customer service occupations"||  job_type =="Mobile machine drivers and operatives"||  job_type =="Elementary personal services occupations"||  job_type =="Elementary cleaning occupations"||  job_type =="Managers and proprietors in hospitality and leisure services"||  job_type =="Sports and fitness occupations" )
replace chosen_job_type= 8  if( job_type == "Health and social services managers"||  job_type =="Health professionals"||  job_type =="Health associate professionals"||  job_type =="Therapists"||  job_type =="Social welfare associate professionals"||  job_type =="Healthcare and related personal services"||  job_type =="Childcare and related personal services"||  job_type =="Animal care services" )
replace chosen_job_type= 9  if( job_type == "Legal professionals"||  job_type =="Legal associate professionals" )
replace chosen_job_type= 10  if( job_type == "Media associate professionals" )
replace chosen_job_type= 11  if( job_type == "Production managers"||  job_type =="Metal forming, welding and related trades"||  job_type =="Metal machining, fitting and instrument making trades"||  job_type =="Vehicle trades"||  job_type =="Electrical trades"||  job_type =="Textiles and garments trades"||  job_type =="Printing trades"||  job_type =="Food preparation trades"||  job_type =="Skilled trades nec"||  job_type =="Process operatives"||  job_type =="Plant and machine operatives"||  job_type =="Assemblers and routine operatives"||  job_type =="Elementary process plant occupations"||  job_type =="Elementary goods storage occupations" )
replace chosen_job_type= 12  if( job_type == "Public service professionals"||  job_type =="Librarians and related professionals"||  job_type =="Public service and other associate professionals"||  job_type =="Administrative occupations: government and related organisat"||  job_type =="Teaching professionals" )
replace chosen_job_type= 13  if( job_type == "It service delivery occupations"||  job_type =="Sales and related associate professionals"||  job_type =="Sales assistants and retail cashiers"||  job_type =="Sales related occupations"||  job_type =="Elementary sales occupations" )
replace chosen_job_type= 14  if( job_type == "Science professionals"||  job_type =="Research professionals"||  job_type =="Science and engineering technicians" )
replace chosen_job_type= 15  if( job_type == "Protective service officers"||  job_type =="Protective service occupations"||  job_type =="Elementary security occupations" )
replace chosen_job_type= 16  if( job_type == "Transport associate professionals"||  job_type =="Transport drivers and operatives" )

drop if chosen_job_type == 0
drop if hiqual_dv <=-1 |  racel_dv<=-1 | job_status != "a permanent job" | gor_dv <=0 | jbsoc00_cc<=0 

* the mprobit on personalities alone
* mprobit chosen_job_type big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv if hiqual_dv >-1 &  racel_dv>-1 & job_status == "a permanent job" & gor_dv >0 & jbsoc00_cc>0 & jbnssec_dv>0

* mprobit chosen_job_type livesp_dv scend gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv racel_dv sex_cr hiqual_dv
* margins, dydx(hiqual_dv)

* mprobit with factor variables does not converge
** mprobit chosen_job_type scend i.gor_dv big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv i.racel_dv i.sex_cr i.hiqual_dv
mprobit chosen_job_type scend gor_rank big5o_dv big5c_dv big5e_dv big5a_dv big5n_dv race_rank sex_cr educ_level


generate gor_rank = 0
replace gor_rank= 1 if gor_dv== 10
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

generate educ_level = -1

replace educ_level= 0 if hiqual_dv== 9
replace educ_level= 1 if hiqual_dv== 5
replace educ_level= 2 if hiqual_dv== 4
replace educ_level= 3 if hiqual_dv== 3
replace educ_level= 4 if hiqual_dv== 1
replace educ_level= 5 if hiqual_dv== 2


generate race_rank= 0
replace race_rank=1 if racel_dvd=="white: british/english/scottish/welsh/northern irish"
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
