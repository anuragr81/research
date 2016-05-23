# duplicates in diary hhdata are not uncommon
library(foreign)
require(plyr)
require(AER)

setwd('c:/local_files/research/consumption/cex/')

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
    print(paste("size read:",dim(df)))
    res = rbind(res,df)
  }
  if (!is.null(unsharedkey) && length(filenames)>1){
    if (length(unsharedkeys)>0){
      stop(paste("column:",unsharedkey," must not be shared across files"));
    }
  }
  
  print(paste("size returned:",dim(res)))
  return(res)
}

load_diary_file <-function(dataset,year){
  if (dataset=="us_cex"){
    # consider gifts in the expd file
    if (year == 2004){
      return (combine_subfiles(filenames=c("2004/diary04/diary04/expd041.dta",
                                           "2004/diary04/diary04/expd042.dta",
                                           "2004/diary04/diary04/expd043.dta",
                                           "2004/diary04/diary04/expd044.dta"),unsharedkey="newid"))
    }
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
  
  hhdat <- load_diary_file("us_cex",year) # must provide total and visible expenditure
  
  ohsdat <-load_ohs_file("us_cex",year) # must provide age, gender, highest_educ, ishead, race,family size, area_type
  incomedat <-load_income_file("us_cex",year) # must provide total income
  
  if (is.null(hhdat)){
    stop("Could not load diary hhdata")
  }
  
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
