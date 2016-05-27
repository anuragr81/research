
# duplicates in diary hhdata are not uncommon
library(foreign)
require(plyr)
require(AER)
source('../panelfunc.R')
source('../regressions.R')

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

load_ohs_file <-function(dataset,year){
  if (dataset=="us_cex"){
    # consider gifts in the expd file
    if (year == 2004){
      return (combine_subfiles(filenames=c("2004/diary04/diary04/fmld041.dta",
                                           "2004/diary04/diary04/fmld042.dta",
                                           "2004/diary04/diary04/fmld043.dta",
                                           "2004/diary04/diary04/fmld044.dta"),unsharedkey="newid"))
    }
  }
  if (dataset =="sa_ies"){
    
  }
  
  stop(paste("Unknown dataset:",dataset))
}


diary_info_columns_us_cex_2004<-function(){
  return(c("hhid","cost","ucc","qredate","alloc"));
}

ohs_info_columns_us_cex_2004<-function(){
  return(c("hhid","age","gender","educ","race","hsize","income","urban_rural","popsize","highest_education"))
}

get_ohs_info_columns<-function(dataset,year){
  
  if (dataset == "us_cex"){
    if (year ==2004){
      return(ohs_info_columns_us_cex_2004());
    }
    stop(paste("Year not found for us_cex:",year))
  }
  stop(paste("Could not find ohs info columns for dataset:",dataset))
}


get_diary_info_columns<-function(dataset,year){
  
  if(dataset== "us_cex"){
    if (year == 2004 ){
      return(diary_info_columns_us_cex_2004())
    }
    stop(paste("Could not find diary info columns for year:",year))
  }
  stop(paste("Unknown dataset:",dataset))
}

get_ignored_hhids<-function(hh,ohs,income){
  non_topcoded_hhids=unique(hh[hh$alloc>=2,]$newid);
  print(paste("Percentage of topcoded households ignored:",100*length(non_topcoded_hhids)/dim(hh)[1]));
  return(non_topcoded_hhids);
}

load_ohs_mapping<-function(dataset,year){
  
  if (dataset == "us_cex") {
    if (year ==2004){
      return(ohs_mapping_us_cex_2004());
    }
    stop(paste("Year not found:",year))
  }
  stop(paste('No dataset for:',dataset));
  
}

hh_us_cex_mapping_2004<-function(){
  s = data.frame(iesname=NULL,name=NULL)
  s= rbind(s,data.frame(iesname="newid",name="hhid"))
  s= rbind(s,data.frame(iesname="cost",name="cost"))
  s= rbind(s,data.frame(iesname="qredate",name="qredate"))
  s= rbind(s,data.frame(iesname="alloc",name="alloc"))
  s= rbind(s,data.frame(iesname="ucc",name="ucc"))
  return(s)
  
}

ohs_mapping_us_cex_2004<-function(){
  s = data.frame(iesname=NULL,name=NULL)
  s= rbind(s,data.frame(iesname="newid",name="hhid"))
  s= rbind(s,data.frame(iesname="age_ref",name="age"))
  s= rbind(s,data.frame(iesname="sex_ref",name="gender"))
  s= rbind(s,data.frame(iesname="educ_ref",name="highest_education"))
  s= rbind(s,data.frame(iesname="ref_race",name="race"))
  s= rbind(s,data.frame(iesname="fam_size",name="hsize"))
  s= rbind(s,data.frame(iesname="fincaftm",name="income"))
  s= rbind(s,data.frame(iesname="popsize",name="popsize"))
  s= rbind(s,data.frame(iesname="bls_urbn",name="urban_rural"))
  return(s)
}

load_diary_fields_mapping<-function(dataset,year){
  if (dataset=="us_cex"){
    
    if (year == 2004){
      return(hh_us_cex_mapping_2004());
    }
    stop(paste('No entry found for',year));
  }
}


load_income_file<-function (dataset,year){
  
  if (dataset== "us_cex"){
    if (year == 2004){
      return(NULL)
    }
    stop(paste("No us_cex data for:",year))
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
merge_hh_ohs_income_data_us_cex_2004<-function(hh,ohs,income){
  
  # hh's ucc should be merged first
  ucc_mapping<-get_ucc_mapping_2004()
  print (paste("The columns of ucc_mapping: ",colnames(ucc_mapping)));
  hh<-merge(hh,ucc_mapping)

  vis<-get_visible_categories_cex_2004(hh=hh,visible_categories = visible_categories_us_cex_2004())
  hh_total_exp <-ddply(hh,.(hhid),summarize,total_expediture=sum(cost))
  hh11<-merge(vis,hh_total_exp)
  ds<-merge(hh11,ohs)
  # post-processing
  ds$race <-as.integer(ds$race)
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
  if (year == 2004){
    return(merge_hh_ohs_income_data_us_cex_2004(hh=hh,ohs=ohs,income=income))
  }
    
    stop(paste("Method to merge data for year:",year," not found"))
  }
  stop(paste("Method to merge data for dataset:",dataset," not found"))
}


cex_combined_data_set<-function(year){
  
  #1-black
  #2-coloured
  #3-asian
  #4-white
  
  ############ PHASE 0 ########################
  
  hhdat <- load_diary_file("us_cex",year) # must provide total and visible expenditure
  
  ohsdat <-load_ohs_file("us_cex",year) # (using fmld) must provide age (age_ref), gender(sex_ref), 
  # highest_educ(educ_ref), ishead(no_earnr,earncomp - all reference person data),
  # race(ref_race),family size (fam_size),
  # area_type (popsize,bls_urbn)
  incomedat <-load_income_file("us_cex",year) # must provide total income (fincaftm)
  
  if (is.null(hhdat)){
    stop("Could not load diary hhdata")
  }
  
  print("Ensuring duplicates do NOT exist in the diary hhdata")
  hhdat = unique(hhdat)
  
  ############ PHASE 1 - Translation ########################
  # info_columns must contain all hhdata-fields referred to in merging/aggregation phase (one per file)
  # translated frame makes the data available in a universal dictionary (age, gender etc.)
  
  hh = get_translated_frame(dat=hhdat,
                            names=get_diary_info_columns("us_cex",year),
                            m=load_diary_fields_mapping("us_cex",year))
  print("Translated hh data")
  # optional data files
  if (!is.null(ohsdat)){
    ohs = get_translated_frame(dat=ohsdat,
                               names=get_ohs_info_columns("us_cex",year),
                               m=load_ohs_mapping("us_cex",year))
    
    print("Translated ohs data")
  }
  
  if (!is.null(incomedat)){
    income = get_translated_frame(dat=incomedat,
                                  names=get_income_info_columns("us_cex",year),
                                  m=load_income_mapping("us_cex",year))
    print("Translated income data")
  }
  
  print("Loaded translated frame(s)")
  ############ PHASE 2 - Aggregation and Merge ########################
  # merge criteria is defined for every dependent variable
  
  ignored_hhids <- get_ignored_hhids(hhdat,ohsdat,incomedat);
  
  print("Pending ignoring of hhids")
  
  dstruct<-merge_hh_ohs_income_data(dataset="us_cex",hh=hh,ohs=ohs,income=income,year=year);
  return(dstruct);
}
