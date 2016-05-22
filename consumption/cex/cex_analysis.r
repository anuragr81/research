library(foreign)
require(plyr)
require(AER)

setwd('c:/local_files/research/consumption/cex/')
ucc_name<-function(ucc_code){
  
}
process_expd<-function(hh)
{
  hhk=ddply(hh,.(alloc),summarize,n=length(newid))
  n_0_allocs = hhk[hhk$alloc==0,]$n
  print(paste("Allocated/Topcoded percentage:",1-n_0_allocs/length(hh$newid)));
  return(hhk)
}

load_diary_file <-function(dataset,year){
  if (dataset=="us_cex"){
    # consider gifts in the expd file
  }
  if (dataset =="sa_ies"){
    
  }
  
  stop(paste("Unknown dataset:",dataset))
}
cex_combined_data_set<-function(year){
  
  #1-black
  #2-coloured
  #3-asian
  #4-white
  
  ############ PHASE 0 ########################
  
  hhdat <- load_diary_file("us_cex",year)
  
  
  ohsdat <-load_ohs_file("us_cex",year)
  incomedat <-load_income_file("us_cex",year)
  
  if (is.null(hhdat)){
    stop("Could not load diary hhdata")
  }
  
  # duplicates in diary hhdata are not uncommon
  print("Ensuring duplicates do NOT exist in the diary hhdata")
  hhdat = unique(hhdat)
  
  ############ PHASE 1 - Translation ########################
  # info_columns must contain all hhdata-fields referred to in merging/aggregation phase (one per file)
  # translated frame makes the data available in a universal dictionary (age, gender etc.)
  
  hh = get_translated_frame(dat=hhdat,
                            names=get_diary_info_columns(year),
                            m=load_diary_fields_mapping(year))
  print("Translated hh data")
  # optional data files
  if (!is.null(ohsdat)){
    ohs = get_translated_frame(dat=ohsdat,
                               names=get_ohs_info_columns(year),
                               m=load_ohs_mapping(year))
    
    print("Translated ohs data")
  }
  
  if (!is.null(incomedat)){
    income = get_translated_frame(dat=incomedat,
                                  names=get_income_info_columns(year),
                                  m=load_income_mapping(year))
    print("Translated income data")
  }
  
  print("Loaded translated frame(s)")
  ############ PHASE 2 - Aggregation and Merge ########################
  # merge criteria is defined for every dependent variable
  
  dstruct<-merge_hh_ohs_income_data(hh=hh,ohs=ohs,income=income,year=year);
  return(dstruct);
}
