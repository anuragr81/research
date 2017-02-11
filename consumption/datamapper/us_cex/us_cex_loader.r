
library(foreign)
require(plyr)
#require(AER)

if (isClass("USCEXLoader")){
  print ("Warning !!! previous definition of USCEXLoader would be overwritten.")
}

## all exported functions are declared here
setClass("USCEXLoader", representation(load_cex_diary="function",load_cex_ohs="function",combined_data_set="function"))

# the parameter passes any external classes that this class may need 

uscex<-function(fu,ufl) {
  load_cex_diary<-function(year,dirprefix){
    # consider gifts in the expd file
    if (year == 2004){
      return (combine_subfiles(filenames=c(paste(dirprefix,"2004/diary04/diary04/expd041.dta",sep=""),
                                           paste(dirprefix,"2004/diary04/diary04/expd042.dta",sep=""),
                                           paste(dirprefix,"2004/diary04/diary04/expd043.dta",sep=""),
                                           paste(dirprefix,"2004/diary04/diary04/expd044.dta",sep="")),unsharedkey="newid"))
      
    }
    if (year == 2009){
      return (combine_subfiles(filenames=c(paste(dirprefix,"2009/diary(09/diary09/expd091.dta",sep=""),
                                           paste(dirprefix,"2009/diary09/diary09/expd092.dta",sep=""),
                                           paste(dirprefix,"2009/diary09/diary09/expd093.dta",sep=""),
                                           paste(dirprefix,"2009/diary09/diary09/expd094.dta",sep="")),unsharedkey="newid"))
      
    }
    if (year == 2014){
      return (combine_subfiles(filenames=c(paste(dirprefix,"2014/diary14/expd141.dta",sep=""),
                                           paste(dirprefix,"2014/diary14/expd142.dta",sep=""),
                                           paste(dirprefix,"2014/diary14/expd143.dta",sep=""),
                                           paste(dirprefix,"2014/diary14/expd144.dta",sep="")),unsharedkey="newid"))
      
    }
    stop("paste- year :", year," not supported")
    
  }
  
  combine_subfiles<-function (filenames,unsharedkey){
    res=NULL
    # perform an rbind over the vector filenames
    
    for (filename in filenames){
      print(paste("Reading:",filename))
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
      res = rbind(res,df,stringsAsFactors=FALSE)
    }
    if (!is.null(unsharedkey) && length(filenames)>1){
      if (length(unsharedkeys)>0){
        stop(paste("column:",unsharedkey," must not be shared across files"));
      }
    }
    
    print(paste("Total size from files:",dim(res)[1],"x",dim(res)[2]))
    return(res)
  }
  
  
  load_cex_ohs <-function(year,dirprefix) {
    # consider gifts in the expd file
    if (year == 2004){
      return (combine_subfiles(filenames=c(paste(dirprefix,"2004/diary04/diary04/fmld041.dta",sep=""),
                                           paste(dirprefix,"2004/diary04/diary04/fmld042.dta",sep=""),
                                           paste(dirprefix,"2004/diary04/diary04/fmld043.dta",sep=""),
                                           paste(dirprefix,"2004/diary04/diary04/fmld044.dta",sep="")),unsharedkey="newid"))
    }
    if (year ==2009){
      return (combine_subfiles(filenames=c(paste(dirprefix,"2009/diary09/diary09/fmld091.dta",sep=""),
                                           paste(dirprefix,"2009/diary09/diary09/fmld092.dta",sep=""),
                                           paste(dirprefix,"2009/diary09/diary09/fmld093.dta",sep=""),
                                           paste(dirprefix,"2009/diary09/diary09/fmld094.dta",sep="")),unsharedkey="newid"))
      
    }
    
    if (year ==2014){
      return (combine_subfiles(filenames=c(paste(dirprefix,"2014/diary14/fmld141.dta",sep=""),
                                           paste(dirprefix,"2014/diary14/fmld142.dta",sep=""),
                                           paste(dirprefix,"2014/diary14/fmld143.dta",sep=""),
                                           paste(dirprefix,"2014/diary14/fmld144.dta",sep="")),unsharedkey="newid"))
      
    }
    stop(paste("year: ",year," not supported"))
    
  }
  load_diary_file <-function(year,dirprefix){
    
    if (year == 2004){
      return (combine_subfiles(filenames=c(paste(dirprefix,"2004/diary04/diary04/expd041.dta",sep=""),
                                           paste(dirprefix,"2004/diary04/diary04/expd042.dta",sep=""),
                                           paste(dirprefix,"2004/diary04/diary04/expd043.dta",sep=""),
                                           paste(dirprefix,"2004/diary04/diary04/expd044.dta",sep="")),unsharedkey="newid"))
      
    }
    if (year == 2009){
      return (combine_subfiles(filenames=c(paste(dirprefix,"2009/diary09/diary09/expd091.dta",sep=""),
                                           paste(dirprefix,"2009/diary09/diary09/expd092.dta",sep=""),
                                           paste(dirprefix,"2009/diary09/diary09/expd093.dta",sep=""),
                                           paste(dirprefix,"2009/diary09/diary09/expd094.dta",sep="")),unsharedkey="newid"))
      
    }
    if (year == 2014){
      return (combine_subfiles(filenames=c(paste(dirprefix,"2014/diary14/expd141.dta",sep=""),
                                           paste(dirprefix,"2014/diary14/expd142.dta",sep=""),
                                           paste(dirprefix,"2014/diary14/expd143.dta",sep=""),
                                           paste(dirprefix,"2014/diary14/expd144.dta",sep="")),unsharedkey="newid"))
      
    }
    stop("paste- year :", year," not supported")
    
  }
  
  load_ohs_file <-function(year,dirprefix){
    # consider gifts in the expd file
    if (year == 2004){
      return (combine_subfiles(filenames=c(paste(dirprefix,"2004/diary04/diary04/fmld041.dta",sep=""),
                                           paste(dirprefix,"2004/diary04/diary04/fmld042.dta",sep=""),
                                           paste(dirprefix,"2004/diary04/diary04/fmld043.dta",sep=""),
                                           paste(dirprefix,"2004/diary04/diary04/fmld044.dta",sep="")),unsharedkey="newid"))
    }
    if (year ==2009){
      return (combine_subfiles(filenames=c(paste(dirprefix,"2009/diary09/diary09/fmld091.dta",sep=""),
                                           paste(dirprefix,"2009/diary09/diary09/fmld092.dta",sep=""),
                                           paste(dirprefix,"2009/diary09/diary09/fmld093.dta",sep=""),
                                           paste(dirprefix,"2009/diary09/diary09/fmld094.dta",sep="")),unsharedkey="newid"))
      
    }
    
    if (year ==2014){
      return (combine_subfiles(filenames=c(paste(dirprefix,"2014/diary14/fmld141.dta",sep=""),
                                           paste(dirprefix,"2014/diary14/fmld142.dta",sep=""),
                                           paste(dirprefix,"2014/diary14/fmld143.dta",sep=""),
                                           paste(dirprefix,"2014/diary14/fmld144.dta",sep="")),unsharedkey="newid"))
      
    }
    stop(paste("year: ",year," not supported"))
    
  }
  
  
  get_ignored_hhids<-function(hh,ohs){
    non_topcoded_hhids=as.integer(unique(hh[hh$alloc>=2,]$newid));
    n_total_hhids <- length(unique(hh$newid))
    print(paste("Percentage of topcoded households ignored:",100*length(non_topcoded_hhids)/n_total_hhids));
    nullpopsize_hhids<-unique(as.integer(ohs[as.character(ohs$popsize)== "",]$newid))
    combined_ignored_hhids <-union(nullpopsize_hhids,non_topcoded_hhids)
    print(paste("Percentage of households with non-null popsize and not-topcoded ignored:",
                100*length(combined_ignored_hhids)/n_total_hhids));
    return(combined_ignored_hhids);
  }
  
  
  combined_data_set<-function(year,dirprefix,selected_category,isDebug, set_depvar){
    
    
    ucc_mapping=ufl()@get_ucc_file_mapping(dirprefix)
    
    ############ PHASE 0 ########################
    if (missing(set_depvar)){
      set_depvar = TRUE 
    }
    if (missing(selected_category)){
      print("setting selected_category to the default value")
      selected_category= visible_categories(year=year)
    }
    
    #*  (( Loading diary file
    hhdat <- load_diary_file(year,dirprefix) # must provide total and visible expenditure (must be already translated)
    
    #* Loading the person/family data fie
    ohsdat <-load_ohs_file(year,dirprefix) # (using fmld) must provide age (age_ref), gender(sex_ref), 
    # highest_educ(educ_ref), ishead(no_earnr,earncomp - all reference person data),
    # race(ref_race),family size (fam_size),
    # area_type (popsize,bls_urbn)
    #* Loading income file
    
    if (is.null(hhdat)){
      stop("Could not load diary hhdata")
    }
    
    print("Ensuring duplicates do NOT exist in the diary hhdata")
    hhdat = unique(hhdat)
    
    ############ PHASE 1 - Translation ########################
    # info_columns must contain all hhdata-fields referred to in merging/aggregation phase (one per file)
    # translated frame makes the data available in a universal dictionary (age, gender etc.)
    translateEnabled<-TRUE
    
    if (translateEnabled) {
      hh = fu()@get_translated_frame(dat=hhdat,
                                     names=get_diary_info_columns(year),
                                     m=load_diary_fields_mapping(year))
      print("Translated hh data")
      # optional data files
      if (!is.null(ohsdat)){
        ohs = fu()@get_translated_frame(dat=ohsdat,
                                        names=get_ohs_info_columns(year),
                                        m=load_ohs_mapping(year))
        
        print("Translated ohs data")
      }
      
    } else {
      hh<-hhdat
      ohs <-ohsdat
    }
    print("Loaded translated frame(s)")
    ############ PHASE 2 - Aggregation and Merge ########################
    # merge criteria is defined for every dependent variable
    
    ignored_hhids <- get_ignored_hhids(hhdat,ohsdat);
    if (!missing(isDebug) && isDebug==TRUE){
      print(paste("Ids to be ignored(",length(ignored_hhids),"):{",toString(ignored_hhids),"}"))
    }
    if (!is.null(ignored_hhids)){
      if (!is.null(hhdat)){
        n1<-length(unique(hh$hhid))
        hh<-hh[!is.element(as.character(hh$hhid),as.character(ignored_hhids)),]
        
        n2<-length(unique(hh$hhid))
        print(paste("ignored",n1-n2,"/",n1," hhids"))
      }
      if (!is.null(ohsdat)){
        
        n1<-length(unique(ohs$hhid))
        ohs<-ohs[!is.element(as.character(ohs$hhid),as.character(ignored_hhids)),]
        
        n2<-length(unique(ohs$hhid))
        print(paste("ignored",n1-n2,"/",n1," hhids"))
      }
    }
    dstruct<-merge_hh_ohs_income_data(ucc_mapping=ucc_mapping,hh=hh,ohs=ohs,year=year,selected_category=selected_category,set_depvar=set_depvar);
    return(dstruct);
    #* ))
  }
  
  merge_hh_ohs_income_data<-function(ucc_mapping=ucc_mapping,year,hh,ohs,selected_category,set_depvar){
    
    if (year == 2004 || year == 2009|| year == 2014){
      ds <-merge_hh_ohs_income_data_us_cex_2004(ucc_mapping=ucc_mapping,hh=hh,ohs=ohs,selected_category=selected_category,set_depvar=set_depvar)
      ds$year <- rep(year,dim(ds)[1])
      return(ds)
    }
    
    stop(paste("Method to merge data for year:",year," not found"))
  }
  
  ### TODO: move into normalizer
  
  ohs_info_columns_us_cex_2004<-function(){
    return(c("hhid","age","gender","educ","race","hsize","income","horref1","urban_rural","popsize","highest_education"))
  }
  
  get_ohs_info_columns<-function(year){
    if (year ==2004 || year ==2009|| year == 2014){
      return(ohs_info_columns_us_cex_2004());
    }
    stop(paste("Year : ",year," not found for us_cex"))
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
  
  load_ohs_mapping<-function(year){
    if (year ==2004 || year == 2009 || year == 2014){
      return(ohs_mapping_us_cex_2004());
    }
    stop(paste("Year not found:",year))
    
  }
  
  diary_info_columns_us_cex_2004<-function(){
    return(c("hhid","cost","ucc","alloc"));
  }
  
  
  get_diary_info_columns<-function(year){
    
    if (year == 2004 || year == 2009|| year == 2014){
      return(diary_info_columns_us_cex_2004())
    }
    stop(paste("Could not find diary info columns for year:",year))
    
  }
  
  hh_us_cex_mapping_2004<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="newid",name="hhid"))
    s= rbind(s,data.frame(iesname="cost",name="cost"))
    s= rbind(s,data.frame(iesname="alloc",name="alloc"))
    s= rbind(s,data.frame(iesname="ucc",name="ucc"))
    return(s)
    
  }
  
  
  load_diary_fields_mapping<-function(year){
    if (year == 2004 || year == 2009|| year == 2014){
      return(hh_us_cex_mapping_2004());
    }
    stop(paste('No hh data found for',year));
  }
  
  
  
  visible_categories<-function(year){
    
    if (year == 2004 || year == 2009|| year == 2014){
      return(visible_categories_us_cex_2004())
    }
    stop(paste("visible categories list for year:",year," not found"))
    
    
  }
  
  
  merge_hh_ohs_income_data_us_cex_2004<-function(ucc_mapping,hh,ohs,selected_category,set_depvar){
    
    # hh's ucc should be merged first
    print("Reading ucc mapping file")
    
    print (paste("Merging with ucc_mapping (columns: ",toString(colnames(ucc_mapping)),")"))
    hh<-merge(hh,ucc_mapping)
    print ("Obtaining visible categories")
    vis<-fu()@filter_categories_data(hh=hh,selected_category = selected_category,item_field = "uccname",set_depvar)
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
  
  
  return(new("USCEXLoader",load_cex_diary=load_cex_diary,load_cex_ohs=load_cex_ohs,combined_data_set=combined_data_set))
}