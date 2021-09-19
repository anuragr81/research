

if (isClass("NigeriaNormaliser")){
  print ("Warning !!! previous definition of NigeriaNormaliser would be overwritten.")
}

## all exported functions are declared here
setClass("NigeriaNormaliser", representation(diary_columns_mapping="function", diary_info_columns_2010="function",
                                             get_lsms_weekrecall_info_columns="function",get_lsms_weekrecall_fields_mapping="function",
                                             get_lsms_monthrecall_info_columns="function",get_lsms_monthrecall_fields_mapping="function",
                                             get_lsms_sixmonthrecall_info_columns="function",get_lsms_sixmonthrecall_fields_mapping="function",
                                             get_lsms_yearrecall_info_columns="function",get_lsms_yearrecall_fields_mapping="function",
                                             item_codes_2010="function", ohs_info_columns_lsms="function",ohs_mapping_lsms="function",
                                             ohs_educ_info_columns_lsms="function",ohs_educ_columns_mapping_lsms="function",
                                             ohs_income_info_columns_lsms="function",ohs_income_columns_mapping_lsms="function",
                                             market_data_columns_mapping="function",market_data_info="function",ohs_geodata_columns_mapping_lsms="function",
                                             unit_codes_2010="function", get_diary_assets_fields_mapping_lsms="function",items_codes="function"
))


ngr_normaliser<-function() {
  
  unit_codes_2010  <- function(){
    s = data.frame(unitcode=NULL, factor=NULL, longunitname=NULL, unit=NULL)
    
    s = rbind(s,data.frame(unitcode='1', factor=0.087, longunitname='tomatoes cup ', unit='kg'))
    s = rbind(s,data.frame(unitcode='2', factor=0.175, longunitname='milk cup ', unit='kg'))
    s = rbind(s,data.frame(unitcode='3', factor=0.23, longunitname='cigarette cup ', unit='kg'))
    s = rbind(s,data.frame(unitcode='4', factor=0.35, longunitname='derica cup small ', unit='kg'))
    s = rbind(s,data.frame(unitcode='5', factor=0.7, longunitname='derica cup big  ', unit='kg'))
    s = rbind(s,data.frame(unitcode='6', factor=1.5, longunitname='congo/mudu ', unit='kg'))
    s = rbind(s,data.frame(unitcode='7', factor=3, longunitname='tiya ', unit='kg'))
    s = rbind(s,data.frame(unitcode='11', factor=20, longunitname='sack/bag: small ', unit='kg'))
    s = rbind(s,data.frame(unitcode='12', factor=50, longunitname='sack/bag: medium ', unit='kg'))
    s = rbind(s,data.frame(unitcode='13', factor=100, longunitname='sack/bag: big/large ', unit='kg'))
    s = rbind(s,data.frame(unitcode='14', factor=120, longunitname='sack/bag: extra large ', unit='kg'))
    s = rbind(s,data.frame(unitcode='21', factor=15, longunitname='basket: small ', unit='kg'))
    s = rbind(s,data.frame(unitcode='22', factor=30, longunitname='basket: medium ', unit='kg'))
    s = rbind(s,data.frame(unitcode='23', factor=50, longunitname='basket: big ', unit='kg'))
    s = rbind(s,data.frame(unitcode='24', factor=75, longunitname='basket: extra large ', unit='kg'))
    s = rbind(s,data.frame(unitcode='31', factor=10, longunitname='basin: small ', unit='kg'))
    s = rbind(s,data.frame(unitcode='32', factor=25, longunitname='basin: medium ', unit='kg'))
    s = rbind(s,data.frame(unitcode='33', factor=40, longunitname='basin: big/large ', unit='kg'))
    s = rbind(s,data.frame(unitcode='34', factor=75, longunitname='basin: extra large ', unit='kg'))
    s = rbind(s,data.frame(unitcode='41', factor=5, longunitname='bunch of plantain/ffb: small ', unit='kg'))
    s = rbind(s,data.frame(unitcode='42', factor=8, longunitname='bunch of plantain/ffb: medium ', unit='kg'))
    s = rbind(s,data.frame(unitcode='43', factor=15, longunitname='bunch of plantain/ffb: big ', unit='kg'))
    s = rbind(s,data.frame(unitcode='51', factor=3, longunitname='tuber of yam: small ', unit='kg'))
    s = rbind(s,data.frame(unitcode='52', factor=5, longunitname='tuber of yam: medium ', unit='kg'))
    s = rbind(s,data.frame(unitcode='53', factor=8, longunitname='tuber of yam: big/large ', unit='kg'))
    s = rbind(s,data.frame(unitcode='61', factor=1, longunitname='bundle of millet g/corn sugarcane vegetable etc:small ', unit='NA'))
    s = rbind(s,data.frame(unitcode='62', factor=NA, longunitname='bundle of millet g/corn sugarcane vegetable etc:medium ', unit='NA'))
    s = rbind(s,data.frame(unitcode='63', factor=40, longunitname='bundle of millet g/corn sugarcane vegetable etc:big ', unit='NA'))
    s = rbind(s,data.frame(unitcode='71', factor=60, longunitname='wheel barrow: small ', unit='kg'))
    s = rbind(s,data.frame(unitcode='72', factor=85, longunitname='wheel barrow: medium ', unit='kg'))
    s = rbind(s,data.frame(unitcode='73', factor=110, longunitname='wheel barrow: big/large ', unit='kg'))
    s = rbind(s,data.frame(unitcode='74', factor=150, longunitname='wheel barrow: extra large ', unit='kg'))
    s = rbind(s,data.frame(unitcode='81', factor=1500, longunitname='pick-up van: small ', unit='kg'))
    s = rbind(s,data.frame(unitcode='82', factor=2000, longunitname='pick-up van: medium ', unit='kg'))
    s = rbind(s,data.frame(unitcode='83', factor=2500, longunitname='pick-up van: big ', unit='kg'))
    s = rbind(s,data.frame(unitcode='91', factor=10, longunitname='jerry can keg rubber of palm oil: small', unit='litres'))
    s = rbind(s,data.frame(unitcode='92', factor=20, longunitname='jerry can keg rubber of palm oil: medium ', unit='litres'))
    s = rbind(s,data.frame(unitcode='93', factor=25, longunitname='jerry can keg rubber of palm oil: big ', unit='litres'))
    s = rbind(s,data.frame(unitcode='94', factor=50, longunitname='jerry can keg rubber of palm oil: large ', unit='litres'))
    s = rbind(s,data.frame(unitcode='95', factor=200, longunitname='jerry can keg rubber of palm oil: drum ', unit='litres'))
    s = rbind(s,data.frame(unitcode='101', factor=1, longunitname='kilograms', unit=' kg'))
    s = rbind(s,data.frame(unitcode='102', factor=0.001, longunitname='grams', unit=' kg'))
    s = rbind(s,data.frame(unitcode='103', factor=1, longunitname='litres', unit='litres'))
    s = rbind(s,data.frame(unitcode='104', factor=0.001, longunitname='millilitre', unit='litres'))
    s = rbind(s,data.frame(unitcode='105', factor=NA, longunitname='pieces', unit='NA'))
    s = rbind(s,data.frame(unitcode='106', factor=NA, longunitname='other ', unit='NA'))
    return(s)
  }
  
  market_data_columns_mapping <- function(year){
    if (year == 2010){
      s = data.frame(iesname=NULL,name=NULL)
      
      s= rbind(s,data.frame(iesname="state",name="region"))
      s= rbind(s,data.frame(iesname="lga",name="district"))
      s= rbind(s,data.frame(iesname="sector",name="is_urban"))
      s= rbind(s,data.frame(iesname="ea",name="ea"))
      s= rbind(s,data.frame(iesname="item_cd",name="item"))
      s= rbind(s,data.frame(iesname="sc2q2",name="is_available"))
      s= rbind(s,data.frame(iesname="sc2q3a",name="price"))
      s= rbind(s,data.frame(iesname="sc2q3b",name="lwp"))
      s= rbind(s,data.frame(iesname="sc2q3c",name="lwp_unit"))
      
      return(s)
    }
    if (year == 2012){
      s = data.frame(iesname=NULL,name=NULL)
      
      s= rbind(s,data.frame(iesname="state",name="region"))
      s= rbind(s,data.frame(iesname="lga",name="district"))
      s= rbind(s,data.frame(iesname="sector",name="is_urban"))
      s= rbind(s,data.frame(iesname="ea",name="ea"))
      s= rbind(s,data.frame(iesname="item_cd",name="item"))
      s= rbind(s,data.frame(iesname="c2q1",name="is_available"))
      s= rbind(s,data.frame(iesname="c2q3",name="price"))
      s= rbind(s,data.frame(iesname="c2q2",name="lwp_unit"))
      
      return(s)
    }
    stop (paste("Cannot find market data columns for year:",year))
  }
  market_data_info <- function(year){
    return(c("hhid","region","district", "zone","item","ea","is_urban","price","lwp_unit","lwp"))
    
  }
  
  diary_info_columns_2010<-function(){
    return(c("hhid","item","lwp_unit", "lwp","tlwp_unit", "gift_unit","gift","own_unit","own", "tlwp", "cost"))
  }
  
  diary_columns_mapping<-function(year){
    if (year == 2010){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="hhid",name="hhid"))
      s= rbind(s,data.frame(iesname="s7bq1",name="is_consumed"))
      s= rbind(s,data.frame(iesname="item_cd",name="item"))
      s= rbind(s,data.frame(iesname="s7bq4",name="cost")) # this is populated only if there is consumption form items purchased in last 7 days
      s= rbind(s,data.frame(iesname="s7bq2b",name="lwp_unit"))
      s= rbind(s,data.frame(iesname="s7bq2a",name="lwp"))
      s= rbind(s,data.frame(iesname="s7bq3b",name="tlwp_unit"))
      s= rbind(s,data.frame(iesname="s7bq3a",name="tlwp"))
      
      s= rbind(s,data.frame(iesname="s7bq5b",name="own_unit"))
      s= rbind(s,data.frame(iesname="s7bq5a",name="own"))
      s= rbind(s,data.frame(iesname="s7bq6b",name="gift_unit"))
      s= rbind(s,data.frame(iesname="s7bq6a",name="gift"))
      
      return(s)
    }
    stop(paste("Year:",year,"not supported"))
  }
  
  get_lsms_weekrecall_info_columns <-function(year){
    if (year == 2010){
      return(c("hhid","region","district", "zone","item","ea","cost","is_urban"))
    }
    
    stop(paste("Year:",year,"not supported"))
  }
  
  get_lsms_monthrecall_info_columns <-function(year){
    return(get_lsms_weekrecall_info_columns(year))
  }
  
  get_lsms_sixmonthrecall_info_columns<-function(year){
    return(get_lsms_weekrecall_info_columns(year))  
  }
  
  get_lsms_sixmonthrecall_fields_mapping <- function(year){
    if (year == 2010){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="hhid",name="hhid"))
      s= rbind(s,data.frame(iesname="state",name="region"))
      s= rbind(s,data.frame(iesname="lga",name="district"))
      s= rbind(s,data.frame(iesname="sector",name="is_urban"))
      s= rbind(s,data.frame(iesname="ea",name="ea"))
      s= rbind(s,data.frame(iesname="item_cd",name="item"))
      s= rbind(s,data.frame(iesname="s8q6",name="cost"))
      return(s)
    }
    
    
    stop(paste("Year:",year,"not supported"))
  }
  
  get_lsms_yearrecall_info_columns <- function(year){
    return(get_lsms_weekrecall_info_columns(year)) 
  }
  
  get_lsms_yearrecall_fields_mapping <- function(year){
    if (year == 2010){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="hhid",name="hhid"))
      s= rbind(s,data.frame(iesname="state",name="region"))
      s= rbind(s,data.frame(iesname="lga",name="district"))
      s= rbind(s,data.frame(iesname="sector",name="is_urban"))
      s= rbind(s,data.frame(iesname="ea",name="ea"))
      s= rbind(s,data.frame(iesname="item_cd",name="item"))
      s= rbind(s,data.frame(iesname="s8q8",name="cost"))
      return(s)
    }
    
    stop(paste("Year:",year,"not supported"))
  }
  
  ohs_info_columns_lsms <- function(year){
    if (year == 2010){
      return(c("hhid","region","district","ea","personid","is_urban", "zone","YOB",'is_hh_member', 'marital_status', 'marriage_year', 
               'spouse_personid', 'religion', 'is_father_hh', 'father_personid', 'is_father_alive', 'father_educ', 'father_occup',
               'is_mother_hh', 'mother_personid', 'is_mother_alive', 'mother_educ', 'mother_occup'))
    }
    if (year == 2012 || year == 2015){
      return(c("hhid","region","district","ea","personid","is_urban", "zone","YOB",'is_hh_member', 'marital_status', 
               'marriage_year_1','marriage_year_2', 'marriage_year_3', 'marriage_year_4',  
               'spouse_personid', 'religion', 'is_father_hh', 'father_personid', 'is_father_alive', 'father_educ', 'father_occup',
               'is_mother_hh', 'mother_personid', 'is_mother_alive', 'mother_educ', 'mother_occup'))
    }
    
    stop(paste("Year:",year,"not supported"))
  }
  
  ohs_mapping_lsms <- function(year) {
    if (year == 2010){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="hhid",name="hhid"))
      s= rbind(s,data.frame(iesname="indiv",name="personid"))
      s= rbind(s,data.frame(iesname="state",name="region"))
      s= rbind(s,data.frame(iesname="lga",name="district"))
      s= rbind(s,data.frame(iesname="sector",name="is_urban"))
      s= rbind(s,data.frame(iesname="ea",name="ea"))
      s= rbind(s,data.frame(iesname="s1q5_year",name="YOB"))
      s= rbind(s,data.frame(iesname="s1q7",name="is_hh_member"))
      s= rbind(s,data.frame(iesname="s1q8",name="marital_status"))
      s= rbind(s,data.frame(iesname="s1q9",name="marriage_year"))
      s= rbind(s,data.frame(iesname="s1q11",name="spouse_personid"))
      s= rbind(s,data.frame(iesname="s1q12",name="religion"))
      s= rbind(s,data.frame(iesname="s1q13",name="is_father_hh"))
      s= rbind(s,data.frame(iesname="s1q14",name="father_personid"))
      s= rbind(s,data.frame(iesname="s1q15",name="is_father_alive"))
      s= rbind(s,data.frame(iesname="s1q16",name="father_educ"))
      s= rbind(s,data.frame(iesname="s1q17",name="father_occup"))
      s= rbind(s,data.frame(iesname="s1q18",name="is_mother_hh"))
      s= rbind(s,data.frame(iesname="s1q19",name="mother_personid"))
      s= rbind(s,data.frame(iesname="s1q20",name="is_mother_alive"))
      s= rbind(s,data.frame(iesname="s1q21",name="mother_educ"))
      s= rbind(s,data.frame(iesname="s1q22",name="mother_occup"))
      return(s)
    } 
    if (year == 2012 || year == 2015){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="hhid",name="hhid"))
      s= rbind(s,data.frame(iesname="indiv",name="personid"))
      s= rbind(s,data.frame(iesname="state",name="region"))
      s= rbind(s,data.frame(iesname="lga",name="district"))
      s= rbind(s,data.frame(iesname="sector",name="is_urban"))
      s= rbind(s,data.frame(iesname="ea",name="ea"))
      s= rbind(s,data.frame(iesname="s1q7_year",name="YOB"))
      s= rbind(s,data.frame(iesname="s1q8",name="marital_status"))
      
      s= rbind(s,data.frame(iesname="s1q11a",name="marriage_year_1"))
      s= rbind(s,data.frame(iesname="s1q11b",name="marriage_year_2"))
      s= rbind(s,data.frame(iesname="s1q11c",name="marriage_year_3"))
      s= rbind(s,data.frame(iesname="s1q11d",name="marriage_year_4"))
      
      s= rbind(s,data.frame(iesname="s1q13",name="spouse_personid"))
      
      
      s= rbind(s,data.frame(iesname="s1q18",name="religion"))
      s= rbind(s,data.frame(iesname="s1q19",name="is_father_hh"))
      s= rbind(s,data.frame(iesname="s1q20",name="father_personid"))
      s= rbind(s,data.frame(iesname="s1q21",name="is_father_alive"))
      s= rbind(s,data.frame(iesname="s1q22",name="father_educ"))
      s= rbind(s,data.frame(iesname="s1q23",name="father_occup"))
      s= rbind(s,data.frame(iesname="s1q24",name="is_mother_hh"))
      s= rbind(s,data.frame(iesname="s1q25",name="mother_personid"))
      s= rbind(s,data.frame(iesname="s1q26",name="is_mother_alive"))
      s= rbind(s,data.frame(iesname="s1q27",name="mother_educ"))
      s= rbind(s,data.frame(iesname="s1q28",name="mother_occup"))
      return(s)
    }
    
    stop(paste("Year:",year,"not supported"))
  }
  
  ohs_educ_info_columns_lsms <- function(year){
    if (year == 2010){
      return(c("hhid","personid", "highest_educ","is_schooled","school_start_age",
               "qualification","is_inschool","reason_not_inschool","school_body","school_conveyance","school_distance",
               "educ_costa","educ_costb","educ_costc","educ_costd","educ_coste","educ_costf","educ_costg","educ_costh","educ_cost"))
    }
    
    stop(paste("Year:",year,"not supported"))
  }
  
  items_codes <- function(year){
    r <- NULL
    r <- rbind(r, data.frame(shortname='furniture_large', longname='Furniture (3/4 piece sofa set)', itemcode='301'))
    r <- rbind(r, data.frame(shortname='furniture_small', longname='Furniture (chairs)', itemcode='302'))
    r <- rbind(r, data.frame(shortname='furntiure_medium', longname='Furniture (table)', itemcode='303'))
    r <- rbind(r, data.frame(shortname='mattress', longname='Mattress ', itemcode='304'))
    r <- rbind(r, data.frame(shortname='bed', longname='Bed', itemcode='305'))
    r <- rbind(r, data.frame(shortname='mat', longname='Mat', itemcode='306'))
    r <- rbind(r, data.frame(shortname='sewing_machine', longname='Sewing machine', itemcode='307'))
    r <- rbind(r, data.frame(shortname='gas_cooker', longname='Gas cooker', itemcode='308'))
    r <- rbind(r, data.frame(shortname='stove_electric', longname='Stove (electric)', itemcode='309'))
    r <- rbind(r, data.frame(shortname='stove_gas_table', longname='Stove gas (table)', itemcode='310'))
    r <- rbind(r, data.frame(shortname='stove_kerosene', longname='Stove (kerosene)', itemcode='311'))
    r <- rbind(r, data.frame(shortname='fridge', longname='Fridge', itemcode='312'))
    r <- rbind(r, data.frame(shortname='freezer', longname='Freezer', itemcode='313'))
    r <- rbind(r, data.frame(shortname='air_conditioner', longname='Air conditioner', itemcode='314'))
    r <- rbind(r, data.frame(shortname='washing_machine', longname='Washing Machine', itemcode='315'))
    r <- rbind(r, data.frame(shortname='electric_clothes_dryer', longname='Electric Clothes Dryer', itemcode='316'))
    r <- rbind(r, data.frame(shortname='bicycle', longname='Bicycle', itemcode='317'))
    r <- rbind(r, data.frame(shortname='motorbike', longname='Motorbike', itemcode='318'))
    r <- rbind(r, data.frame(shortname='cars', longname='Cars and other vehicles', itemcode='319'))
    r <- rbind(r, data.frame(shortname='generator', longname='Generator', itemcode='320'))
    r <- rbind(r, data.frame(shortname='fan', longname='Fan', itemcode='321'))
    r <- rbind(r, data.frame(shortname='radio', longname='Radio', itemcode='322'))
    r <- rbind(r, data.frame(shortname='cassette_recorder', longname='Cassette recorder', itemcode='323'))
    r <- rbind(r, data.frame(shortname='sound_system', longname='Hi-Fi (Sound System)', itemcode='324'))
    r <- rbind(r, data.frame(shortname='microwave_oven', longname='Microwave', itemcode='325'))
    r <- rbind(r, data.frame(shortname='iron', longname='Iron', itemcode='326'))
    r <- rbind(r, data.frame(shortname='tv_set', longname='TV Set', itemcode='327'))
    r <- rbind(r, data.frame(shortname='computer', longname='Computer ', itemcode='328'))
    r <- rbind(r, data.frame(shortname='dvd_player', longname='DVD Player', itemcode='329'))
    r <- rbind(r, data.frame(shortname='satellite_dish', longname='Satellite Dish', itemcode='330'))
    r <- rbind(r, data.frame(shortname='musical_instrument', longname='Musical Instrument', itemcode='331'))
    r <- rbind(r, data.frame(shortname='mobile_phone', longname='Mobile Phone', itemcode='332'))
    r <- rbind(r, data.frame(shortname='inverter', longname='Inverter', itemcode='333'))
    r <- rbind(r, data.frame(shortname='other_asset', longname='Other', itemcode='334'))
    return(r)
  }
  ohs_educ_columns_mapping_lsms <- function(year){
    if (year == 2010){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="hhid",name="hhid"))
      s= rbind(s,data.frame(iesname="indiv",name="personid"))
      s= rbind(s,data.frame(iesname="s2q4",name="is_schooled"))
      s= rbind(s,data.frame(iesname="s2q5",name="reason_not_schooled"))
      s= rbind(s,data.frame(iesname="s2q6",name="school_start_age"))
      s= rbind(s,data.frame(iesname="s2q7",name="highest_educ"))
      s= rbind(s,data.frame(iesname="s2q8",name="qualification"))
      s= rbind(s,data.frame(iesname="s2q9",name="is_inschool"))
      s= rbind(s,data.frame(iesname="s2q10",name="reason_not_inschool"))
      s= rbind(s,data.frame(iesname="s2q11",name="school_body"))
      s= rbind(s,data.frame(iesname="s2q12",name="school_conveyance"))
      s= rbind(s,data.frame(iesname="s2q13",name="school_distance"))
      s= rbind(s,data.frame(iesname="s2q18a",name="educ_costa"))
      s= rbind(s,data.frame(iesname="s2q18b",name="educ_costb"))
      s= rbind(s,data.frame(iesname="s2q18c",name="educ_costc"))
      s= rbind(s,data.frame(iesname="s2q18d",name="educ_costd"))
      s= rbind(s,data.frame(iesname="s2q18e",name="educ_coste"))
      s= rbind(s,data.frame(iesname="s2q18f",name="educ_costf"))
      s= rbind(s,data.frame(iesname="s2q18g",name="educ_costg"))
      s= rbind(s,data.frame(iesname="s2q18h",name="educ_costh"))
      s= rbind(s,data.frame(iesname="s2q18i",name="educ_cost"))
      return(s)
    }
    if (year == 2012){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="hhid",name="hhid"))
      s= rbind(s,data.frame(iesname="indiv",name="personid"))
      s= rbind(s,data.frame(iesname="s2q5",name="is_schooled"))
      
      s= rbind(s,data.frame(iesname="s2q6",name="reason_not_schooled"))
      s= rbind(s,data.frame(iesname="s2q7",name="school_start_age"))
      s= rbind(s,data.frame(iesname="s2q8",name="highest_educ"))
      s= rbind(s,data.frame(iesname="s2q9",name="qualification"))
      s= rbind(s,data.frame(iesname="s2q10",name="is_inschool"))
      s= rbind(s,data.frame(iesname="s2q11",name="reason_not_inschool"))
      s= rbind(s,data.frame(iesname="s2q12",name="school_body"))
      s= rbind(s,data.frame(iesname="s2q13",name="school_conveyance"))
      s= rbind(s,data.frame(iesname="s2q14",name="school_distance"))
      
      s= rbind(s,data.frame(iesname="s2q19a",name="educ_costa"))
      s= rbind(s,data.frame(iesname="s2q19b",name="educ_costb"))
      s= rbind(s,data.frame(iesname="s2q19c",name="educ_costc"))
      s= rbind(s,data.frame(iesname="s2q19d",name="educ_costd"))
      s= rbind(s,data.frame(iesname="s2q19e",name="educ_coste"))
      s= rbind(s,data.frame(iesname="s2q19f",name="educ_costf"))
      s= rbind(s,data.frame(iesname="s2q19g",name="educ_costg"))
      s= rbind(s,data.frame(iesname="s2q19h",name="educ_costh"))
      s= rbind(s,data.frame(iesname="s2q19i",name="educ_cost"))
      return(s)
    }
    stop(paste("Year:",year,"not supported"))
  }
  
  ohs_income_info_columns_lsms <- function(year){
    if (year == 2010){
      return(c('hhid', 'personid', 'is_gt_5y', 'worked_ext_pastweek', 
               'worked_hh_pastweek1', 'worked_hh_pastweek2', 'worked_pastweek', 'occupation_primary', 'occupation_sector_primary', 
               'employer_type_primary', 'hours_worked_week_primary', 'is_paid_primary', 'reason_unpaid_primary', 'last_payment_primary', 
               'last_payment_primary_units', 'has_secondary', 'occupation_secondary', 'occupation_sector_secondary', 
               'employer_type_secondary', 'months_secondary_work', 'weeks_secondary_work', 'hoursperweek_secondary_work', 
               'is_paid_secondary', 'reason_unpaid_secondary', 'last_payment_secondary', 'last_payment_secondary_units'))
    } 
    if (year == 2012 || year == 2015){
      return(c('hhid', 'personid', 'is_gt_5y', 'worked_ext_pastweek', 
               'worked_hh_pastweek1', 'worked_hh_pastweek2', 'worked_pastweek', 'occupation_primary', 'occupation_sector_primary', 
               'employer_type_primary', 'hours_worked_week_primary', 'is_paid_primary', 'reason_unpaid_primary', 'last_payment_primary', 
               'last_payment_primary_units', 'has_secondary', 'occupation_secondary', 'occupation_sector_secondary', 
               'employer_type_secondary', 'months_secondary_work', 'weeks_secondary_work' )
               )
    }
    stop(paste("Year:",year,"not supported"))
    
  }
  
  timeunitcodes <- function (){
    #40 hours per week average - but hours per week in the field when unit is week
    # most pay is reported for month, then day and then week followed by fortnight, year, quarter, half-year, hours(checked with  ddply(subset(ohs,!is.na(last_payment_primary)),.(last_payment_primary_units),summarise,n=length(hhid)))
    r = rbind(r,data.frame(timeunitcode=1, timeunitname="hour"))
    r = rbind(r,data.frame(timeunitcode=2, timeunitname="day"))
    r = rbind(r,data.frame(timeunitcode=3, timeunitname="week"))
    r = rbind(r,data.frame(timeunitcode=4, timeunitname="fortnight"))
    r = rbind(r,data.frame(timeunitcode=5, timeunitname="month"))
    r = rbind(r,data.frame(timeunitcode=6	, timeunitname="quarter"))
    r = rbind(r,data.frame(timeunitcode=7, timeunitname="half year"))
    r = rbind(r,data.frame(timeunitcode=8, timeunitname="year"))
    return(r)
  }
  
  get_diary_assets_fields_mapping_lsms<- function(year){
    if (year == 2012 || year == 2010) {
        s = data.frame(iesname=NULL,name=NULL)
        s= rbind(s,data.frame(iesname="hhid",name="hhid"))
        s= rbind(s,data.frame(iesname="item_cd",name="itemcode"))
        s= rbind(s,data.frame(iesname="s5q1",name="number"))
        s= rbind(s,data.frame(iesname="s5q3",name="age"))
        s= rbind(s,data.frame(iesname="s5q4",name="mtm"))
        return(s)
    }
    stop(paste("Year:",year,"not supported"))
  }
  
  ohs_income_columns_mapping_lsms <- function(year){
    
    if (year == 2010){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="hhid",name="hhid"))
      s= rbind(s,data.frame(iesname="state",name="region"))
      s= rbind(s,data.frame(iesname="lga",name="district"))
      s= rbind(s,data.frame(iesname="sector",name="is_urban"))
      s= rbind(s,data.frame(iesname="ea",name="ea"))
      s= rbind(s,data.frame(iesname="indiv",name="personid"))
      s= rbind(s,data.frame(iesname="s3q1",name="is_gt_5y"))
      s= rbind(s,data.frame(iesname="s3q4",name="worked_ext_pastweek"))
      s= rbind(s,data.frame(iesname="s3q5",name="worked_hh_pastweek1"))
      s= rbind(s,data.frame(iesname="s3q6",name="worked_hh_pastweek2"))
      s= rbind(s,data.frame(iesname="s3q7",name="worked_pastweek"))
      s= rbind(s,data.frame(iesname="s3q13",name="occupation_primary"))
      s= rbind(s,data.frame(iesname="s3q14",name="occupation_sector_primary"))
      s= rbind(s,data.frame(iesname="s3q15",name="employer_type_primary"))
      s= rbind(s,data.frame(iesname="s3q18",name="hours_worked_week_primary"))
      s= rbind(s,data.frame(iesname="s3q19",name="is_paid_primary"))
      s= rbind(s,data.frame(iesname="s3q20",name="reason_unpaid_primary"))
      s= rbind(s,data.frame(iesname="s3q21a",name="last_payment_primary"))
      s= rbind(s,data.frame(iesname="s3q21b",name="last_payment_primary_units"))
      s= rbind(s,data.frame(iesname="s3q24",name="has_secondary"))
      s= rbind(s,data.frame(iesname="s3q25",name="occupation_secondary"))
      s= rbind(s,data.frame(iesname="s3q26",name="occupation_sector_secondary"))
      s= rbind(s,data.frame(iesname="s3q27",name="employer_type_secondary"))
      s= rbind(s,data.frame(iesname="s3q28",name="months_secondary_work"))
      s= rbind(s,data.frame(iesname="s3q29",name="weeks_secondary_work"))
      s= rbind(s,data.frame(iesname="s3q30",name="hoursperweek_secondary_work"))
      s= rbind(s,data.frame(iesname="s3q31",name="is_paid_secondary"))
      s= rbind(s,data.frame(iesname="s3q32",name="reason_unpaid_secondary"))
      s= rbind(s,data.frame(iesname="s3q33a",name="last_payment_secondary"))
      s= rbind(s,data.frame(iesname="s3q33b",name="last_payment_secondary_units"))
      
      return(s)
    }
    
    if (year == 2012){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="hhid",name="hhid"))
      s= rbind(s,data.frame(iesname="state",name="region"))
      s= rbind(s,data.frame(iesname="lga",name="district"))
      s= rbind(s,data.frame(iesname="sector",name="is_urban"))
      s= rbind(s,data.frame(iesname="ea",name="ea"))
      s= rbind(s,data.frame(iesname="indiv",name="personid"))
      s= rbind(s,data.frame(iesname="s3aq1",name="is_gt_5y"))
      s= rbind(s,data.frame(iesname="s3aq4",name="worked_ext_pastweek"))
      s= rbind(s,data.frame(iesname="s3aq5",name="worked_hh_pastweek1"))
      s= rbind(s,data.frame(iesname="s3aq6",name="worked_hh_pastweek2"))
      s= rbind(s,data.frame(iesname="s3aq7",name="worked_pastweek"))
      s= rbind(s,data.frame(iesname="s3aq13a",name="occupation_primary"))
      s= rbind(s,data.frame(iesname="s3aq13b",name="occupation_primary_code"))
      s= rbind(s,data.frame(iesname="s3aq14",name="occupation_sector_primary"))
      s= rbind(s,data.frame(iesname="s3aq14b",name="occupation_sector_primary_code"))
      s= rbind(s,data.frame(iesname="s3aq15",name="employer_type_primary"))
      s= rbind(s,data.frame(iesname="s3aq18",name="hours_worked_week_primary"))
      s= rbind(s,data.frame(iesname="s3aq19",name="is_paid_primary"))
      s= rbind(s,data.frame(iesname="s3aq20",name="reason_unpaid_primary"))
      s= rbind(s,data.frame(iesname="s3aq21a",name="last_payment_primary"))
      s= rbind(s,data.frame(iesname="s3aq21b",name="last_payment_primary_units"))
      s= rbind(s,data.frame(iesname="s3aq25",name="has_secondary"))
      s= rbind(s,data.frame(iesname="s3aq26a",name="occupation_secondary"))
      s= rbind(s,data.frame(iesname="s3aq26b",name="occupation_secondary_code"))
      s= rbind(s,data.frame(iesname="s3aq27",name="occupation_sector_secondary"))
      s= rbind(s,data.frame(iesname="s3aq28",name="employer_type_secondary"))
      s= rbind(s,data.frame(iesname="s3aq29",name="months_secondary_work"))
      s= rbind(s,data.frame(iesname="s3aq30",name="weeks_secondary_work"))
      return(s)
    }
    
    
    stop(paste("Year:",year,"not supported"))
  }
  
  
  
  #generated using:
  #write(paste("r=rbind(r,data.frame(longname='",r$longname,"', shortname='",r$shortname,"', code=",as.integer(r$itemcode),"))",sep=""),'c:/temp/t.txt')
  item_codes_2010 <- function(){
    r=data.frame(item=NULL,code=NULL,stringsAsFactors = FALSE)
    r=rbind(r,data.frame(longname='Guineacorn/sorghum', shortname='corn', code=10010))
    r=rbind(r,data.frame(longname='Millet', shortname='millet', code=10011))
    r=rbind(r,data.frame(longname='Maize', shortname='maize_grain', code=10012))
    r=rbind(r,data.frame(longname='Rice local', shortname='rice_local', code=10013))
    r=rbind(r,data.frame(longname='Rice imported', shortname='rice_imported', code=10014))
    r=rbind(r,data.frame(longname='Bread', shortname='bread', code=10015))
    r=rbind(r,data.frame(longname='Maize flour', shortname='maize_flour', code=10016))
    r=rbind(r,data.frame(longname='Yam flour', shortname='yam_flour', code=10017))
    r=rbind(r,data.frame(longname='Cassava flour', shortname='cassava_flour', code=10018))
    r=rbind(r,data.frame(longname='Wheat flour', shortname='wheat_flour', code=10019))
    r=rbind(r,data.frame(longname='Other grains?and?flour', shortname='other_grains', code=10020))
    r=rbind(r,data.frame(longname='Cassava roots', shortname='cassava_fresh', code=10030))
    r=rbind(r,data.frame(longname='Yam roots', shortname='yam_fresh', code=10031))
    r=rbind(r,data.frame(longname='Gari white', shortname='gari_white', code=10032))
    r=rbind(r,data.frame(longname='Gari yellow', shortname='gari_yellow', code=10033))
    r=rbind(r,data.frame(longname='Cocoyam', shortname='cocoyam', code=10034))
    r=rbind(r,data.frame(longname='Plantains', shortname='plantains', code=10035))
    r=rbind(r,data.frame(longname='Sweet-potatoes', shortname='sweet_potato', code=10036))
    r=rbind(r,data.frame(longname='Potatoes', shortname='potato', code=10037))
    r=rbind(r,data.frame(longname='Other-roots and tuber', shortname='roots_other', code=10038))
    r=rbind(r,data.frame(longname='Soya-beans', shortname='soyabeans', code=10040))
    r=rbind(r,data.frame(longname='Brown-beans', shortname='brown_beans', code=10041))
    r=rbind(r,data.frame(longname='White-beans', shortname='white_beans', code=10042))
    r=rbind(r,data.frame(longname='Groundnuts', shortname='peanuts', code=10043))
    r=rbind(r,data.frame(longname='Other-nuts/seeds/pulses', shortname='nuts_others', code=10044))
    r=rbind(r,data.frame(longname='Palm-oil', shortname='palm_oil', code=10050))
    r=rbind(r,data.frame(longname='Butter/Margarine', shortname='butter_margarine', code=10051))
    r=rbind(r,data.frame(longname='Groundnuts-Oil', shortname='peanut_oil', code=10052))
    r=rbind(r,data.frame(longname='Other oil and Fat', shortname='oil_others', code=10053))
    r=rbind(r,data.frame(longname='Bananas', shortname='bananas', code=10060))
    r=rbind(r,data.frame(longname='Orange/tangerine', shortname='citrus', code=10061))
    r=rbind(r,data.frame(longname='Mangoes', shortname='mangoes', code=10062))
    r=rbind(r,data.frame(longname='Avocado-pear', shortname='avocado_pear', code=10063))
    r=rbind(r,data.frame(longname='Pineapples', shortname='pineapples', code=10064))
    r=rbind(r,data.frame(longname='Fruit canned', shortname='fruit_canned', code=10065))
    r=rbind(r,data.frame(longname='Other fruits', shortname='other_fruits', code=10066))
    r=rbind(r,data.frame(longname='Tomatoes', shortname='tomatoes', code=10070))
    r=rbind(r,data.frame(longname='Tomato puree (canned)', shortname='tomatoes_puree', code=10071))
    r=rbind(r,data.frame(longname='Onions', shortname='onions', code=10072))
    r=rbind(r,data.frame(longname='Garden eggs/eggplant', shortname='eggplant', code=10073))
    r=rbind(r,data.frame(longname='Okra fresh', shortname='okra_fresh', code=10074))
    r=rbind(r,data.frame(longname='Okra dried', shortname='okra_dried', code=10075))
    r=rbind(r,data.frame(longname='Pepper', shortname='pepper', code=10076))
    r=rbind(r,data.frame(longname='Leaves(Cocoyam Spinach etc.)', shortname='fresh_leaves', code=10077))
    r=rbind(r,data.frame(longname='Other vegetables (fresh or ... )', shortname='vegetables_others', code=10078))
    r=rbind(r,data.frame(longname='Chicken', shortname='chicken', code=10080))
    r=rbind(r,data.frame(longname='Duck', shortname='duck', code=10081))
    r=rbind(r,data.frame(longname='Other domestic poultry', shortname='poultry_others', code=10082))
    r=rbind(r,data.frame(longname='Agricultural eggs', shortname='eggs', code=10083))
    r=rbind(r,data.frame(longname='Local eggs', shortname='eggs', code=10084))
    r=rbind(r,data.frame(longname='Othereggs(not chicken)', shortname='eggs_others', code=10085))
    r=rbind(r,data.frame(longname='Beef', shortname='beef', code=10090))
    r=rbind(r,data.frame(longname='Mutton', shortname='mutton', code=10091))
    r=rbind(r,data.frame(longname='Pork', shortname='pork', code=10092))
    r=rbind(r,data.frame(longname='Goat', shortname='goat', code=10093))
    r=rbind(r,data.frame(longname='Wild game meat', shortname='game', code=10094))
    r=rbind(r,data.frame(longname='Canned beef/corned beef', shortname='beef_corned', code=10095))
    r=rbind(r,data.frame(longname='Other meat(excl.poultry)', shortname='meat_others', code=10096))
    r=rbind(r,data.frame(longname='Fish fresh', shortname='fresh_fish', code=10100))
    r=rbind(r,data.frame(longname='Fish frozen', shortname='frozen_fish', code=10101))
    r=rbind(r,data.frame(longname='Fish smoked', shortname='smoked_fish', code=10102))
    r=rbind(r,data.frame(longname='Fish dried', shortname='dried_fish', code=10103))
    r=rbind(r,data.frame(longname='Snails', shortname='snails', code=10104))
    r=rbind(r,data.frame(longname='Seafood(lobster crab prawns)', shortname='seafoods', code=10105))
    r=rbind(r,data.frame(longname='Canned fish/seafood', shortname='canned_seafood', code=10106))
    r=rbind(r,data.frame(longname='Other fish orseafood', shortname='fish_others', code=10107))
    r=rbind(r,data.frame(longname='Fresh milk', shortname='fresh_milk', code=10110))
    r=rbind(r,data.frame(longname='Milk powder', shortname='milk_powder', code=10111))
    r=rbind(r,data.frame(longname='Baby milkpowder', shortname='babymilk', code=10112))
    r=rbind(r,data.frame(longname='Milk tinned(unsweetened)', shortname='canned_milk', code=10113))
    r=rbind(r,data.frame(longname='Other milk products', shortname='milk_others', code=10114))
    r=rbind(r,data.frame(longname='Coffee', shortname='coffee', code=10120))
    r=rbind(r,data.frame(longname='Chocolate drinks (including Milo)', shortname='choco_drinks', code=10121))
    r=rbind(r,data.frame(longname='Tea', shortname='tea', code=10122))
    r=rbind(r,data.frame(longname='Sugar', shortname='sugar', code=10130))
    r=rbind(r,data.frame(longname='Jams', shortname='jams', code=10131))
    r=rbind(r,data.frame(longname='Honey', shortname='honey', code=10132))
    r=rbind(r,data.frame(longname='Other sweets and confectionary', shortname='sweets_others', code=10133))
    r=rbind(r,data.frame(longname='Condiments (salt spices pepper)', shortname='condiments', code=10140))
    r=rbind(r,data.frame(longname='Bottled water', shortname='water_bottled', code=10150))
    r=rbind(r,data.frame(longname='Sachet water', shortname='water_sachet', code=10151))
    r=rbind(r,data.frame(longname='Malt drinks', shortname='malt_drinks', code=10152))
    r=rbind(r,data.frame(longname='Soft drinks(Coca, Cola spirit etc)', shortname='soft_drinks', code=10153))
    r=rbind(r,data.frame(longname='Fruit juice canned/Pack', shortname='peanut_pack', code=10154))
    r=rbind(r,data.frame(longname='Other non-alcoholic drinks', shortname='soft_drinks_others', code=10155))
    r=rbind(r,data.frame(longname='Beer(local and imported)', shortname='local_beer', code=10160))
    r=rbind(r,data.frame(longname='Palm wine', shortname='palm_wine', code=10161))
    r=rbind(r,data.frame(longname='Pito', shortname='pito', code=10162))
    r=rbind(r,data.frame(longname='Gin', shortname='gin', code=10163))
    r=rbind(r,data.frame(longname='Other alcoholic beverages', shortname='alcoholic_others', code=10164))
    r=rbind(r,data.frame(longname='Cigarettes or tobacco', shortname='cigarettes', code=101))
    r=rbind(r,data.frame(longname='Matches', shortname='matches', code=102))
    r=rbind(r,data.frame(longname='Newspaper and magazines?', shortname='newspapers', code=103))
    r=rbind(r,data.frame(longname='Public transport (bus rail boat etc excludding educational expenses)', shortname='public_transport', code=104))
    r=rbind(r,data.frame(longname='Kerosene', shortname='kerosene', code=301))
    r=rbind(r,data.frame(longname='Palm Kernel Oil', shortname='palm_kernel_oil', code=302))
    r=rbind(r,data.frame(longname='Gas (for lighting/cooking)', shortname='gas', code=303))
    r=rbind(r,data.frame(longname='Other liquid cooking fuel', shortname='liquid_fuel', code=304))
    r=rbind(r,data.frame(longname='Electricity including electricity vouchers', shortname='electricity', code=305))
    r=rbind(r,data.frame(longname='Candle', shortname='candle', code=306))
    r=rbind(r,data.frame(longname='Firewood', shortname='firewood', code=307))
    r=rbind(r,data.frame(longname='Charcoal', shortname='charcoal', code=308))
    r=rbind(r,data.frame(longname='Petrol', shortname='petrol', code=309))
    r=rbind(r,data.frame(longname='Diesel', shortname='diesel', code=310))
    r=rbind(r,data.frame(longname='Light bulbs/globes', shortname='electric_bulb', code=311))
    r=rbind(r,data.frame(longname='Water', shortname='water', code=312))
    r=rbind(r,data.frame(longname='Soap and Washing powder', shortname='soap', code=313))
    r=rbind(r,data.frame(longname='Toilet paper', shortname='toilet_paper', code=314))
    r=rbind(r,data.frame(longname='Personal care goods (razor blades cosmetics)', shortname='cosmetics', code=315))
    r=rbind(r,data.frame(longname='Vitamin suplements', shortname='vitamin', code=316))
    r=rbind(r,data.frame(longname='Insecticides disinfectant and cleaners', shortname='insecticides', code=317))
    r=rbind(r,data.frame(longname='Postal (inlc. Stamps courier)', shortname='fresh_postal', code=318))
    r=rbind(r,data.frame(longname='Recharge cards', shortname='phone_recharge', code=319))
    r=rbind(r,data.frame(longname='Landline charges', shortname='landline_charge', code=320))
    r=rbind(r,data.frame(longname='Internet Services', shortname='internet', code=321))
    r=rbind(r,data.frame(longname='Recreational (Cinemas video/DVD rental', shortname='cinema_dvd', code=322))
    r=rbind(r,data.frame(longname='Motor vehicle service repair or parts', shortname='motor_repair', code=323))
    r=rbind(r,data.frame(longname='Bicycle service repair or parts', shortname='bicycle_repair', code=324))
    r=rbind(r,data.frame(longname='Wages paid to staff/maid/lawnsboy', shortname='wages', code=325))
    r=rbind(r,data.frame(longname='Mortgage regular payment to purchase', shortname='mortgage', code=326))
    r=rbind(r,data.frame(longname='Repairs & maintenance to dwelling', shortname='maintenance_house', code=327))
    r=rbind(r,data.frame(longname='Repairs to household and personal items', shortname='maintenance_household', code=328))
    r=rbind(r,data.frame(longname='House Rent', shortname='house_rent', code=329))
    r=rbind(r,data.frame(longname='Infant clothing', shortname='clothes_infant', code=401))
    r=rbind(r,data.frame(longname='Baby nappies/diapers', shortname='clothes_baby', code=402))
    r=rbind(r,data.frame(longname='Boys Tailored clothes', shortname='clothes_boys_tailored', code=403))
    r=rbind(r,data.frame(longname='Boys dress (ready made)', shortname='clothes_boys_made', code=404))
    r=rbind(r,data.frame(longname='Girls Tailored clothes', shortname='clothes_girls_tailored', code=405))
    r=rbind(r,data.frame(longname='Girls dress (ready made)', shortname='clothes_girls_made', code=406))
    r=rbind(r,data.frame(longname='Men Tailored clothes', shortname='clothes_men_tailored', code=407))
    r=rbind(r,data.frame(longname='Men dress (ready made)', shortname='clothes_men_made', code=408))
    r=rbind(r,data.frame(longname='Women Tailored clothes', shortname='women_clothes_tailored', code=409))
    r=rbind(r,data.frame(longname='Women dress (ready made)', shortname='women_clothes_made', code=410))
    r=rbind(r,data.frame(longname='Ankara George materials', shortname='ankara_george_clothes', code=411))
    r=rbind(r,data.frame(longname='Other clothing materials', shortname='clothes_others', code=412))
    r=rbind(r,data.frame(longname='Boys shoes', shortname='shoes_boys', code=413))
    r=rbind(r,data.frame(longname='Mens shoes', shortname='shoes_men', code=414))
    r=rbind(r,data.frame(longname='Girls shoes', shortname='shoes_girls', code=415))
    r=rbind(r,data.frame(longname='Ladys shoes', shortname='shoes_women', code=416))
    r=rbind(r,data.frame(longname='Tailoring charges', shortname='tailoring_charge', code=417))
    r=rbind(r,data.frame(longname='laundry and dry cleaning', shortname='laundry_charge', code=418))
    r=rbind(r,data.frame(longname='Bowls glassware plates silverware etc.', shortname='bowls', code=419))
    r=rbind(r,data.frame(longname='Cooking utensils (cookpots stirring spoons and wisks etc.)', shortname='cooking_utensils', code=420))
    r=rbind(r,data.frame(longname='Cleaning utensils (brooms brushes etc.)', shortname='cleaning_utensils', code=421))
    r=rbind(r,data.frame(longname='Torch / flashlight', shortname='torch', code=422))
    r=rbind(r,data.frame(longname='Umbrella and raincoat', shortname='rainwear', code=423))
    r=rbind(r,data.frame(longname='Paraffin lamp (hurricane or pressure)', shortname='lamp', code=424))
    r=rbind(r,data.frame(longname='Stationery items (not for school)', shortname='stationery_notschool', code=425))
    r=rbind(r,data.frame(longname='Books (not for school)', shortname='books_notschool', code=426))
    r=rbind(r,data.frame(longname='House decorations', shortname='house_decorations', code=427))
    r=rbind(r,data.frame(longname='Nights lodging in rest house or hotel', shortname='lodging_charges', code=428))
    r=rbind(r,data.frame(longname='Health expenditures (excluding insurance)', shortname='health_expenditure', code=430))
    r=rbind(r,data.frame(longname='Donations to church mosque other religious group', shortname='donations', code=429))
    r=rbind(r,data.frame(longname='Carpet rugs drapes curtains', shortname='carpets_rugs', code=501))
    r=rbind(r,data.frame(longname='Linen towels sheets blankets', shortname='linen', code=502))
    r=rbind(r,data.frame(longname='Mat sleeping or for drying maize flour', shortname='mat', code=503))
    r=rbind(r,data.frame(longname='Mosquito net', shortname='mosquito_net', code=504))
    r=rbind(r,data.frame(longname='Mattress', shortname='mattress', code=505))
    r=rbind(r,data.frame(longname='Sports & hobby equipment musical', shortname='sports_hobby', code=506))
    r=rbind(r,data.frame(longname='Film film processing camera', shortname='films', code=507))
    r=rbind(r,data.frame(longname='Builiding items (cement bricks timber iron)', shortname='building_material', code=508))
    r=rbind(r,data.frame(longname='Council rates', shortname='council_rates', code=509))
    r=rbind(r,data.frame(longname='Health insurance', shortname='health_insurance', code=510))
    r=rbind(r,data.frame(longname='Auto insurance', shortname='auto_insurance', code=511))
    r=rbind(r,data.frame(longname='Home insurance', shortname='home_insurance', code=512))
    r=rbind(r,data.frame(longname='Life insurance', shortname='life_insurance', code=513))
    r=rbind(r,data.frame(longname='Fines or legal fees', shortname='fines', code=514))
    r=rbind(r,data.frame(longname='Dowry costs', shortname='dowry', code=515))
    r=rbind(r,data.frame(longname='Marriage ceremony costs', shortname='marriage_ceremony_costs', code=516))
    r=rbind(r,data.frame(longname='Funeral costs', shortname='funeral_costs', code=517))
    r=rbind(r,data.frame(longname='Woodpoles bamboo', shortname='woodpoles_bamboo', code=518))
    r=rbind(r,data.frame(longname='Grass for thatching roof or other use', shortname='grass', code=519))
    
    
    return(r)
  }
  
  get_lsms_weekrecall_fields_mapping <- function(year){
    if (year == 2010){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="hhid",name="hhid"))
      s= rbind(s,data.frame(iesname="state",name="region"))
      s= rbind(s,data.frame(iesname="lga",name="district"))
      s= rbind(s,data.frame(iesname="sector",name="is_urban"))
      s= rbind(s,data.frame(iesname="ea",name="ea"))
      s= rbind(s,data.frame(iesname="item_cd",name="item"))
      s= rbind(s,data.frame(iesname="s8q2",name="cost"))
      return(s)
    }
    
    
    stop(paste("Year:",year,"not supported"))
  }
  
  get_lsms_monthrecall_fields_mapping <- function(year){
    if (year == 2010){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="hhid",name="hhid"))
      s= rbind(s,data.frame(iesname="state",name="region"))
      s= rbind(s,data.frame(iesname="lga",name="district"))
      s= rbind(s,data.frame(iesname="sector",name="is_urban"))
      s= rbind(s,data.frame(iesname="ea",name="ea"))
      s= rbind(s,data.frame(iesname="item_cd",name="item"))
      s= rbind(s,data.frame(iesname="s8q4",name="cost"))
      return(s)
    }
    
    
    stop(paste("Year:",year,"not supported"))
  }
  
  
  ohs_geodata_columns_mapping_lsms <- function(year){
    if (year == 2010 || year == 2012 || year == 2015){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="hhid",name="hhid"))
      s= rbind(s,data.frame(iesname="LAT_DD_MOD",name="S"))
      s= rbind(s,data.frame(iesname="LON_DD_MOD",name="E"))
      return(s)
    }
    
    stop(paste("Year:",year,"not supported"))
  }
  
  return(new("NigeriaNormaliser",diary_columns_mapping=diary_columns_mapping, 
             diary_info_columns_2010=diary_info_columns_2010,get_lsms_weekrecall_info_columns=get_lsms_weekrecall_info_columns,
             get_lsms_weekrecall_fields_mapping=get_lsms_weekrecall_fields_mapping,item_codes_2010=item_codes_2010,
             get_lsms_monthrecall_info_columns=get_lsms_monthrecall_info_columns,get_lsms_monthrecall_fields_mapping=get_lsms_monthrecall_fields_mapping,
             get_lsms_sixmonthrecall_info_columns=get_lsms_sixmonthrecall_info_columns,get_lsms_sixmonthrecall_fields_mapping=get_lsms_sixmonthrecall_fields_mapping,
             get_lsms_yearrecall_info_columns=get_lsms_yearrecall_info_columns,get_lsms_yearrecall_fields_mapping=get_lsms_yearrecall_fields_mapping,
             ohs_info_columns_lsms=ohs_info_columns_lsms, ohs_mapping_lsms=ohs_mapping_lsms,ohs_educ_info_columns_lsms=ohs_educ_info_columns_lsms,
             ohs_educ_columns_mapping_lsms=ohs_educ_columns_mapping_lsms,ohs_income_info_columns_lsms=ohs_income_info_columns_lsms,
             ohs_income_columns_mapping_lsms=ohs_income_columns_mapping_lsms,market_data_columns_mapping=market_data_columns_mapping,
             market_data_info=market_data_info, unit_codes_2010=unit_codes_2010,ohs_geodata_columns_mapping_lsms=ohs_geodata_columns_mapping_lsms,
             get_diary_assets_fields_mapping_lsms=get_diary_assets_fields_mapping_lsms,items_codes=items_codes) )
  
}