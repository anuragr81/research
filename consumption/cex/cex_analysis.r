
# duplicates in diary hhdata are not uncommon
library(foreign)
require(plyr)
require(AER)
source('../panelfunc.R')
source('../regressions.R')

setwd('c:/local_files/research/consumption/cex/')

#########################

read_tnz <- function(filename,convert_factors) {
  if (!is.logical(convert_factors) || !is.atomic(convert_factors)){
    stop("convert_factors must be ")
  }
  dat1 = read.dta(filename,convert.factors = convert_factors);
  dat2 = as.data.frame(dat1);
  dat3 = dat2[as.numeric(dat2$y2_hhid)>0,] # only take data with hhid>0
  return(dat3);
}

#########################


process_expd<-function(hh)
{
  hhk=ddply(hh,.(alloc),summarize,n=length(newid))
  n_0_allocs = hhk[hhk$alloc==0,]$n
  print(paste("Allocated/Topcoded percentage:",1-n_0_allocs/length(hh$newid)));
  return(hhk)
}

combine_subfiles<-function (filenames,unsharedkey){
  res=NULL
  # perform an rbind over the vector filenames
  
  for (filename in filenames){
    df<-read.dta(filename)
    if (!is.data.frame(df)){
      stop(paste("Must be a data frame object:",filename))
    }
    if (is.null(res)){
      columns <-colnames(df)
      if (!is.null(unsharedkey)){
        unsharedkeys <- df[,unsharedkey]
      }
    } else {
      if (length(colnames(df))!=length(columns)){
        stop("Columns of all subfiles must have same size")
      } else {
        
        if (any(colnames(df)!=columns)){
          print(paste("columns:",toString(columns)))
          print(paste("columns:",toString(colnames(df))))
          stop("Columns of all subfiles must match")
        }
      }
      if (!is.null(unsharedkey)){
        unsharedkeys<-intersect(unsharedkeys,df[,unsharedkey])
      }
    }
    print(paste("For file:",filename ," read data-frame of size :",dim(df)[1],"x",dim(df)[2]))
    res = rbind(res,df)
  }
  if (!is.null(unsharedkey) && length(filenames)>1){
    if (length(unsharedkeys)>0){
      stop(paste("column:",unsharedkey," must not be shared across files"));
    }
  }
  
  print(paste("Total size from files:",dim(res)[1],"x",dim(res)[2]))
  return(res)
}

check_ds<-function(df){
  if (!is.data.frame(df)){
    stop("df must be a dataframe")
  }
  for (c in colnames(df)) {
    
    if (any(as.character(ds[,c])=="")){
      stop(paste("column ",c," in df has empty "))
    }
  }
  return(TRUE)
}

get_lsms_secm_info_columns<-function(year){
  if (year == 2010){
    return(c("hhid","is_consumed","cost","price"))
  }
  stop(paste("Not secm info columns for year: ",year))
}

get_lsms_secm_fields_mapping<-function(year){
  if (year == 2010){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y2_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="hh_m01_2",name="is_consumed"))
    s= rbind(s,data.frame(iesname="hh_m02",name="cost"))
    s= rbind(s,data.frame(iesname="hh_m03",name="price"))
    return(s)
  }
  stop(paste("Not secm info columns for year: ",year))
}

get_lsms_secl_info_columns_2010<-function(){
  return(c("hhid","item","is_consumed","cost"))
}

get_lsms_secl_fields_mapping_2010<-function(){
  s = data.frame(iesname=NULL,name=NULL)
  s= rbind(s,data.frame(iesname="y2_hhid",name="hhid"))
  s= rbind(s,data.frame(iesname="hh_l01_2",name="is_consumed"))
  s= rbind(s,data.frame(iesname="itemcode",name="item"))
  s= rbind(s,data.frame(iesname="hh_l02",name="cost"))
  return(s)
}

multiplyLsmsQuantities <-function(dat,quantity_field_name,item_field_name,factor,items_list){
  
  if (is.factor(dat[,item_field_name])){
    stop(item_field_name," must be converted to integers")
  }
  if (!is.atomic(factor) || !is.numeric(factor) || factor<1){
    stop("factor must be a numeric greater than or equal to 1")
  }
  
  if (!is.vector(items_list)){
    stop("multiplyLsmsQuantities: itemslist must be a vector")
  }
  # if not a character or has length>0
  
  if (!(length(quantity_field_name) ==1  && is.character(quantity_field_name))){
    stop(paste("Invalid quantity field name",quantity_field_name))
  }
  
  if (!(length(item_field_name) ==1  && is.character(item_field_name))){
    stop(paste("Invalid item field name",item_field_name))
  }
  
  if (!is.data.frame(dat)){
    stop("data must be a data.frame")
  }
#  if (is.element("mulfactor__",colnames(dat))){
#    stop("input data already has a column named mulfactor__")
#  }
  
  mulfactor_1 =  (factor-1)*as.integer(is.element(as.integer(dat[,item_field_name]),as.integer(items_list)))
  dat[,quantity_field_name]<-mulfactor_1*dat[,quantity_field_name]+dat[,quantity_field_name]
  return(dat)
}

load_diary_file <-function(dataset,year){
  if (dataset == "lsms"){
    if (year == 2010){
      # combine sections ( k , l, m )
      kdat <- read_tnz("../lsms/TZNPS2HH3DTA/HH_SEC_K1.dta",TRUE)
      k = get_translated_frame(dat=kdat,
                               names=diary_info_columns_lsms_2010(),
                               m=hh_mapping_lsms_2010())
      
      k <- k[as.numeric(k$cost)>0 & !is.na(k$cost),]
      ###
      ldat <- read_tnz("../lsms/TZNPS2HH2DTA/HH_SEC_L.dta",FALSE)
      l = get_translated_frame(dat=ldat,
                               names=get_lsms_secl_info_columns_2010(),
                               m=get_lsms_secl_fields_mapping_2010())
      
      ###
      mdat <-read_tnz( '../lsms/TZNPS2HH2DTA/HH_SEC_M.dta',TRUE)
      m = get_translated_frame(dat=mdat,
                               names=get_lsms_secm_info_columns(year),
                               m=get_lsms_secm_fields_mapping(year))
      
      yearly_recall_items <- c("301", "302", "303", "304", "305", "306", "307", "308", "309", 
                               "310", "311", "312", "313", "314", "315", "316", "317", "318", "319")
      
      # nothing to be multiplied for yearly-recall (since we're looking at annual consumption)
      
      # m, l are month or year basis
      monthly_recall_items <- c("201", "202", "203", "204", "205", "206", "207", "208", "209",
        "210", "211", "212", "213", "214", "215", "216", "217", "218", "219",
        "220", "221", "222", "223", "224")  
      
      l <- multiplyLsmsQuantities(dat = l , 
                                  quantity_field_name="cost", 
                                  item_field_name="item", 
                                  factor=12,
                                  items_list = monthly_recall_items)
      
      return(k)
    }
    stop(paste("Year:",year, " not supported"))
  }
  if (dataset=="us_cex"){
    # consider gifts in the expd file
    if (year == 2004){
      return (combine_subfiles(filenames=c("2004/diary04/diary04/expd041.dta",
                                           "2004/diary04/diary04/expd042.dta",
                                           "2004/diary04/diary04/expd043.dta",
                                           "2004/diary04/diary04/expd044.dta"),unsharedkey="newid"))
      
    }
    if (year == 2009){
      return (combine_subfiles(filenames=c("2009/diary09/diary09/expd091.dta",
                                           "2009/diary09/diary09/expd092.dta",
                                           "2009/diary09/diary09/expd093.dta",
                                           "2009/diary09/diary09/expd094.dta"),unsharedkey="newid"))
      
    }
    if (year == 2014){
      return (combine_subfiles(filenames=c("2014/diary14/expd141.dta",
                                           "2014/diary14/expd142.dta",
                                           "2014/diary14/expd143.dta",
                                           "2014/diary14/expd144.dta"),unsharedkey="newid"))
      
    }
    stop("paste- year :", year," not supported")
  }
  if (dataset =="sa_ies"){
    
  }
  
  stop(paste("Unknown dataset:",dataset))
}

get_ohs_secc_columns_lsms_2010<-function(){
  return(c("hhid","personid","is_ge5y","litlang","is_literate","highest_educ","schoolowner",
           "schoolconveyance","has_missedschool","educexpense","has_adulteduc","adulteducmonths"))
}

get_ohs_secc_fields_mapping_lsms_2010<-function(){
  s = data.frame(iesname=NULL,name=NULL)
  s= rbind(s,data.frame(iesname="y2_hhid",name="hhid"))
  s= rbind(s,data.frame(iesname="indidy2",name="personid"))
  s= rbind(s,data.frame(iesname="hh_c01",name="is_ge5y"))
  s= rbind(s,data.frame(iesname="hh_c02",name="litlang"))
  s= rbind(s,data.frame(iesname="hh_c01",name="is_literate"))
  s= rbind(s,data.frame(iesname="hh_c07",name="highest_educ"))
  s= rbind(s,data.frame(iesname="hh_c12",name="schoolowner"))
  s= rbind(s,data.frame(iesname="hh_c14",name="schoolconveyance"))
  s= rbind(s,data.frame(iesname="hh_c17",name="has_missedschool"))
  s= rbind(s,data.frame(iesname="hh_c28_8",name="educexpense"))
  s= rbind(s,data.frame(iesname="hh_c29",name="has_adulteduc"))
  s= rbind(s,data.frame(iesname="hh_c30",name="adulteducmonths"))
  return(s)
}

load_ohs_file <-function(dataset,year){
  
  if (dataset == "lsms"){
    if (year == 2010){
      bdat<-read_tnz('../lsms/TZNPS2HH1DTA/HH_SEC_B.dta',TRUE)
      b <- get_translated_frame(dat=bdat,
                                names=ohs_info_columns_lsms_2010(),
                                m=ohs_mapping_lsms_2010())
      
      cdat<-read_tnz('../lsms/TZNPS2HH1DTA/HH_SEC_C.dta',TRUE)
      c <- get_translated_frame(dat=cdat,
                                names=get_ohs_secc_columns_lsms_2010(),
                                m=get_ohs_secc_fields_mapping_lsms_2010())
      
      return(b)
    }
    stop(paste("Year:",year," not supported"))
  }
  if (dataset=="us_cex"){
    # consider gifts in the expd file
    if (year == 2004){
      return (combine_subfiles(filenames=c("2004/diary04/diary04/fmld041.dta",
                                           "2004/diary04/diary04/fmld042.dta",
                                           "2004/diary04/diary04/fmld043.dta",
                                           "2004/diary04/diary04/fmld044.dta"),unsharedkey="newid"))
    }
    if (year ==2009){
      return (combine_subfiles(filenames=c("2009/diary09/diary09/fmld091.dta",
                                           "2009/diary09/diary09/fmld092.dta",
                                           "2009/diary09/diary09/fmld093.dta",
                                           "2009/diary09/diary09/fmld094.dta"),unsharedkey="newid"))
      
    }
    
    if (year ==2014){
      return (combine_subfiles(filenames=c("2014/diary14/fmld141.dta",
                                           "2014/diary14/fmld142.dta",
                                           "2014/diary14/fmld143.dta",
                                           "2014/diary14/fmld144.dta"),unsharedkey="newid"))
      
    }
    stop(paste("year: ",year," not supported"))
    
  }
  if (dataset =="sa_ies"){
    
  }
  
  stop(paste("Unknown dataset:",dataset))
}

diary_info_columns_us_cex_2004<-function(){
  return(c("hhid","cost","ucc","alloc"));
}

diary_info_columns_lsms_2010<-function(){
  return(c("hhid","item","lwp_unit", "lwp", "cost", "own_unit", "own", "gift_unit", "gift"))
}

ohs_info_columns_us_cex_2004<-function(){
  return(c("hhid","age","gender","educ","race","hsize","income","horref1","urban_rural","popsize","highest_education"))
}

ohs_info_columns_lsms_2010<-function(){
  # hhid, age, gender, educ, race, hsize, areatype, 
  # income file: income
  return(c("hhid", "gender", "personid","YOB", "head_of_household", "inhouse_consumer",
           "inhouse_days_in_month", "inhouse_resident", "outhouse_days_in_year", 
           "occupation", "fathers_educ", "mothers_educ", "married", "spouse_resident",
           "outhouse_spouses", "source_migration_name", "source_migration_code", 
           "reason_migration", "birthdistrict_name", "birthdistrict_code"))
}

get_ohs_info_columns<-function(dataset,year){
  
  if (dataset == "us_cex"){
    if (year ==2004 || year ==2009|| year == 2014){
      return(ohs_info_columns_us_cex_2004());
    }
    
    stop(paste("Year : ",year," not found for us_cex"))
  }
  stop(paste("Could not find ohs info columns for dataset:",dataset))
}

get_diary_info_columns<-function(dataset,year){
  
  if(dataset== "us_cex"){
    if (year == 2004 || year == 2009|| year == 2014){
      return(diary_info_columns_us_cex_2004())
    }
    stop(paste("Could not find diary info columns for year:",year))
  }
  stop(paste("Unknown dataset:",dataset))
}

get_ignored_hhids<-function(dataset,hh,ohs,income){
  if (dataset == "us_cex"){
    non_topcoded_hhids=as.integer(unique(hh[hh$alloc>=2,]$newid));
    n_total_hhids <- length(unique(hh$newid))
    print(paste("Percentage of topcoded households ignored:",100*length(non_topcoded_hhids)/n_total_hhids));
    nullpopsize_hhids<-unique(as.integer(ohs[as.character(ohs$popsize)== "",]$newid))
    combined_ignored_hhids <-union(nullpopsize_hhids,non_topcoded_hhids)
    print(paste("Percentage of households with non-null popsize and not-topcoded ignored:",
                100*length(combined_ignored_hhids)/n_total_hhids));
    return(combined_ignored_hhids);
  } 
  if (dataset == "lsms"){
    return(NULL)
  }
  stop(paste("No ignored hhids rationale for dataset:",dataset));
}

load_ohs_mapping<-function(dataset,year){
  
  if (dataset == "us_cex") {
    if (year ==2004 || year == 2009 || year == 2014){
      return(ohs_mapping_us_cex_2004());
    }
    stop(paste("Year not found:",year))
  }
  stop(paste('No dataset for:',dataset));
  
}

hh_mapping_lsms_2010 <-function(){
  s = data.frame(iesname=NULL,name=NULL)
  s= rbind(s,data.frame(iesname="y2_hhid",name="hhid"))
  s= rbind(s,data.frame(iesname="itemcode",name="item"))
  s= rbind(s,data.frame(iesname="hh_k03_1",name="lwp_unit"))
  s= rbind(s,data.frame(iesname="hh_k03_2",name="lwp"))
  s= rbind(s,data.frame(iesname="hh_k04",name="cost"))
  s= rbind(s,data.frame(iesname="hh_k05_1",name="own_unit"))
  s= rbind(s,data.frame(iesname="hh_k05_2",name="own"))
  s= rbind(s,data.frame(iesname="hh_k06_1",name="gift_unit"))
  s= rbind(s,data.frame(iesname="hh_k06_2",name="gift"))
  return(s)
}

hh_us_cex_mapping_2004<-function(){
  s = data.frame(iesname=NULL,name=NULL)
  s= rbind(s,data.frame(iesname="newid",name="hhid"))
  s= rbind(s,data.frame(iesname="cost",name="cost"))
  s= rbind(s,data.frame(iesname="alloc",name="alloc"))
  s= rbind(s,data.frame(iesname="ucc",name="ucc"))
  return(s)
  
}
ohs_mapping_lsms_2010<-function(){
  s = data.frame(iesname=NULL,name=NULL)
  s= rbind(s,data.frame(iesname="y2_hhid",name="hhid"))
  s= rbind(s,data.frame(iesname="indidy2",name="personid"))
  s= rbind(s,data.frame(iesname="hh_b02",name="gender"))
  s= rbind(s,data.frame(iesname="hh_b03_1",name="YOB"))
  s= rbind(s,data.frame(iesname="hh_b05",name="head_of_household"))
  s= rbind(s,data.frame(iesname="hh_b07",name="inhouse_consumer"))
  s= rbind(s,data.frame(iesname="hh_b08",name="inhouse_days_in_month"))
  s= rbind(s,data.frame(iesname="hh_b09_1",name="inhouse_resident"))
  s= rbind(s,data.frame(iesname="hh_b10",name="outhouse_days_in_year"))
  s= rbind(s,data.frame(iesname="hh_b11",name="occupation"))
  s= rbind(s,data.frame(iesname="hh_b14",name="fathers_educ"))
  s= rbind(s,data.frame(iesname="hh_b17",name="mothers_educ"))
  s= rbind(s,data.frame(iesname="hh_b19",name="married"))
  s= rbind(s,data.frame(iesname="hh_b21",name="spouse_resident"))
  s= rbind(s,data.frame(iesname="hh_b24",name="outhouse_spouses"))
  s= rbind(s,data.frame(iesname="hh_b26_2",name="source_migration_name"))
  s= rbind(s,data.frame(iesname="hh_b26_3",name="source_migration_code"))
  s= rbind(s,data.frame(iesname="hh_b27",name="reason_migration"))
  s= rbind(s,data.frame(iesname="hh_b28_2",name="birthdistrict_name"))
  s= rbind(s,data.frame(iesname="hh_b28_3",name="birthdistrict_code"))
  return(s)
}

ohs_mapping_us_cex_2004<-function(){
  s = data.frame(iesname=NULL,name=NULL)
  s= rbind(s,data.frame(iesname="newid",name="hhid"))
  s= rbind(s,data.frame(iesname="age_ref",name="age"))
  s= rbind(s,data.frame(iesname="sex_ref",name="gender"))
  s= rbind(s,data.frame(iesname="educ_ref",name="highest_education"))
  s= rbind(s,data.frame(iesname="ref_race",name="race"))
  s= rbind(s,data.frame(iesname="horref1",name="horref1"))
  s= rbind(s,data.frame(iesname="fam_size",name="hsize"))
  s= rbind(s,data.frame(iesname="fincaftm",name="income"))
  s= rbind(s,data.frame(iesname="popsize",name="popsize"))
  s= rbind(s,data.frame(iesname="bls_urbn",name="urban_rural"))
  return(s)
}

load_diary_fields_mapping<-function(dataset,year){
  if (dataset=="us_cex"){
    
    if (year == 2004 || year == 2009|| year == 2014){
      return(hh_us_cex_mapping_2004());
    }
    stop(paste('No hh data found for',year));
  }
  stop(paste("Not supported dataset:",dataset))
}


load_income_file<-function (dataset,year){
  
  if (dataset== "us_cex"){
    return(NULL)
    #stop(paste("No us_cex data for:",year))
  }
  if (dataset == "lsms"){
    return(NULL)
  }
  
  stop(paste("Unknown dataset: ",dataset))
}

visible_categories_us_cex_2004<-function(){
  
  # personal care, clothing and apparel (including footwear),jewelry, cars
  return(c('miscpersonalcare', 'haircareproducts', 'nonelectrichairequipment', 'wigshairpieces',
           'oralhygieneproducts', 'shavingneeds', 'cosmetics', 'miscpersonalcare',
           'electricalpersonalcareequipment', 'femalepersonalcareservices', 
           'malepersonalcareservices', 'personalcareappliancesrentalrepair', 'menssuits', 
           'menssportjackets', 'mensformaljackets', 'mensunderwear', 'menshosiery',
           'menssleepwear', 'mensaccessories', 'menssweater', 'mensactivesportswear', 
           'mensshirts', 'menspants', 'mensshorts_exathletic', 'mensuniforms', 
           'boyscoatsjackets', 'boyssweaters', 'boysshirts', 'boysunderwear',
           'boyssleepwear', 'boyshosiery', 'boysaccessories', 
           'boyssuitssportcoats', 'boyspants', 'boysshortsexcathletic', 'boysuniformsactivesportswear',
           'womenscoats', 'womensdresses', 'womenssportcoats', 
           'womenssweaters', 'womensshirts', 'womensskirts', 'womenspants',
           'womensshorts_exathletic', 'womensactivesportswear', 'womenssleepwear', 
           'womensundergarments', 'womenshosiery', 'womenssuits', 'womensaccessories',
           'womensuniforms', 'girlscoatsjackets', 'girlsdressessuits', 'girlssportcoats',
           'girlsskirtspants', 'girlsshortsexathletic', 'girlsactivesportswear', 
           'girlsundergarments', 'girlshosiery', 'girlsaccessories', 'girlsuniforms',
           'mensfootwear', 'boysfootwear', 'girlsfootwear', 'womensfootwear', 'infantscoats',
           'infantsdresses', 'infantsundergarments', 'infantssleepingwear', 'infantsaccessories', 
           'sewingmaterial_clothes', 'watches', 'jewelry', 'shoerepair', 'apparelcleaning_coinoperated',
           'clothes_repair', 'clothing_rental', 'watchjewelryrepair', 'apparell_notcoinoperated', 
           'newcars', 'newtrucks', 'newmotorcycles', 'carlease', 'trucklease', 'usedcars', 
           'usedtrucks', 'usedmotorcycles', 'usedaircraft'))
}


get_ucc_mapping_2004<-function(){
  mfile<-read.csv('2004/ucc_mapping.csv')
  return(mfile)
}

get_visible_categories_cex_2004<-function(hh,visible_categories){
  vis<-hh[is.element(hh$uccname,visible_categories),] # get only visible categories
  vis<-ddply(vis,.(hhid),summarize,visible_consumption=sum(cost))
  return(vis)
}

merge_hh_ohs_income_data_lsms_2010<-function(hh,ohs,income){
  print(colnames(hh))
  return(NULL)
}

merge_hh_ohs_income_data_us_cex_2004<-function(hh,ohs,income){
  
  # hh's ucc should be merged first
  print("Reading ucc mapping file")
  ucc_mapping<-get_ucc_mapping_2004()
  print (paste("Merging with ucc_mapping (columns: ",toString(colnames(ucc_mapping)),")"))
  hh<-merge(hh,ucc_mapping)
  print ("Obtaining visible categories")
  vis<-get_visible_categories_cex_2004(hh=hh,visible_categories = visible_categories_us_cex_2004())
  print ("====Food categories to be modified =====")
  #food<-get_food_categories_cex(hh=hh,food_categories = food_categories_cex());
  print ("Running ddply for total expenditures")
  hh_total_exp <-ddply(hh,.(hhid),summarize,total_expenditure=sum(cost))
  print("Merging visual expenditures")
  hh11<-merge(vis,hh_total_exp)
  print("Merging personal ohs file")
  ds<-merge(hh11,ohs)
  # post-processing
  ds$race <-as.integer(ds$race)
  # convert translate horref into hispanic field and remove horref1 from dataframe since it has NULLs
  
  ds$hispanic<-as.character(ds$horref1)!=""# & as.integer(ds$race)!=1 & as.integer(ds$race)!=2 # neither black nor white 
  ds$horref1<-NULL
  return(ds);
}

#@desc - merges translated data into one big data frame. There are
#        two phases of normalization. The first is merely a translation
#        into well-known names (age,gender etc.). The second phase is 
#        translation into dependent variables (each of which can correspond to
#        one function). The merging function here encapsulates the extraction from
#        all tables and merging into a combined frame.
merge_hh_ohs_income_data<-function(dataset,year,hh,ohs,income){
  
  if (dataset == "us_cex"){
    if (year == 2004 || year == 2009|| year == 2014){
      ds <-merge_hh_ohs_income_data_us_cex_2004(hh=hh,ohs=ohs,income=income)
      ds$year <- rep(year,dim(ds)[1])
      return(ds)
    }
    
    stop(paste("Method to merge data for year:",year," not found"))
  }
  if (dataset == "lsms"){
    ds <-merge_hh_ohs_income_data_lsms_2010(hh=hh,ohs=ohs,income=income)
    return(ds)
  }
  stop(paste("Method to merge data for dataset:",dataset," not found"))
}

cex_combined_years_ds<-function(years)
{
  if (!is.vector(years)){
    stop("years must be a vector")
  }
  resds <-NULL
  for (year in years){
    ds=cex_combined_data_set(year)
    resds = rbind(resds,ds)
  }
  return(resds)
}

combined_data_set<-function(dataset,year,isTranslated){
  
  #1-black
  #2-coloured
  #3-asian
  #4-white
  
  ############ PHASE 0 ########################
  
  hhdat <- load_diary_file(dataset,year) # must provide total and visible expenditure (must be already translated)
  
  ohsdat <-load_ohs_file(dataset,year) # (using fmld) must provide age (age_ref), gender(sex_ref), 
  # highest_educ(educ_ref), ishead(no_earnr,earncomp - all reference person data),
  # race(ref_race),family size (fam_size),
  # area_type (popsize,bls_urbn)
  incomedat <-load_income_file(dataset,year) # must provide total income (fincaftm)
  
  if (is.null(hhdat)){
    stop("Could not load diary hhdata")
  }
  
  print("Ensuring duplicates do NOT exist in the diary hhdata")
  hhdat = unique(hhdat)
  
  ############ PHASE 1 - Translation ########################
  # info_columns must contain all hhdata-fields referred to in merging/aggregation phase (one per file)
  # translated frame makes the data available in a universal dictionary (age, gender etc.)
  if (missing(isTranslated)) {
    hh = get_translated_frame(dat=hhdat,
                              names=get_diary_info_columns(dataset,year),
                              m=load_diary_fields_mapping(dataset,year))
    print("Translated hh data")
    # optional data files
    if (!is.null(ohsdat)){
      ohs = get_translated_frame(dat=ohsdat,
                                 names=get_ohs_info_columns(dataset,year),
                                 m=load_ohs_mapping(dataset,year))
      
      print("Translated ohs data")
    }
    
    if (!is.null(incomedat)){
      income = get_translated_frame(dat=incomedat,
                                    names=get_income_info_columns(dataset,year),
                                    m=load_income_mapping(dataset,year))
      print("Translated income data")
    }
  } else {
    hh<-hhdat
    ohs <-ohsdat
    income<-incomedat
  }
  print("Loaded translated frame(s)")
  ############ PHASE 2 - Aggregation and Merge ########################
  # merge criteria is defined for every dependent variable
  
  ignored_hhids <- get_ignored_hhids(dataset,hhdat,ohsdat,incomedat);
  
  if (!is.null(ignored_hhids)){  
    if (!is.null(hhdat)){
      hh<-hh[!is.element(hh$hhid,ignored_hhids),]
    }
    if (!is.null(ohsdat)){
      ohs<-ohs[!is.element(ohs$hhid,ignored_hhids),]
    }
    if (!is.null(incomedat)){
      income<-income[!is.element(income$hhid,ignored_hhids),]
    }
  }
  dstruct<-merge_hh_ohs_income_data(dataset=dataset,hh=hh,ohs=ohs,income=income,year=year);
  return(dstruct);
}

cpi_adjust<-function(ds,mfile,refyear){
  m=read.csv(mfile)
  m<-data.frame(year=as.integer(m$Year),cpi=m$Jun)
  refcpi = m[m$year==as.integer(refyear),]$cpi
  if (length(refcpi)>1){
    stop(paste("More than one values for year:",year))
  }
  print(paste("refcpi=",refcpi))
  m$cpi=m$cpi/refcpi
  mds<-merge(ds,m)
  mds$total_expenditure<-mds$total_expenditure*mds$cpi
  mds$visible_consumption<-mds$visible_consumption*mds$cpi
  return(mds)
}