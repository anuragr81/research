
library(foreign)
require(plyr)
#require(AER)

if (isClass("USCEXLoader")){
  print ("Warning !!! previous definition of USCEXLoader would be overwritten.")
}

## all exported functions are declared here
setClass("USCEXLoader", representation(load_cex_diary="function",load_ohs_file="function",
                                       combined_data_set="function",group_expenditure="function",
                                       load_cex_fmli="function"))

# the parameter passes any external classes that this class may need 

uscex<-function(fu,un) {
  load_cex_diary<-function(year,dirprefix,un){
    # consider gifts in the expd file
    if (year == 2004){
      dat<- (combine_subfiles(filenames=c(paste(dirprefix,"/2004/diary04/diary04/expd041.dta",sep=""),
                                           paste(dirprefix,"/2004/diary04/diary04/expd042.dta",sep=""),
                                           paste(dirprefix,"/2004/diary04/diary04/expd043.dta",sep=""),
                                           paste(dirprefix,"/2004/diary04/diary04/expd044.dta",sep="")),unsharedkey="newid"))
      
    } else if (year == 2009){
      dat<- (combine_subfiles(filenames=c(paste(dirprefix,"/2009/diary09/diary09/expd091.dta",sep=""),
                                           paste(dirprefix,"/2009/diary09/diary09/expd092.dta",sep=""),
                                           paste(dirprefix,"/2009/diary09/diary09/expd093.dta",sep=""),
                                           paste(dirprefix,"/2009/diary09/diary09/expd094.dta",sep="")),unsharedkey="newid"))
      mappingItems <- un()@ucc_codes_2009()
      dat$ucc      <- as.integer(as.character(dat$ucc))
      
    } else if (year == 2014){
      dat<- (combine_subfiles(filenames=c(paste(dirprefix,"/2014/diary14/expd141.dta",sep=""),
                                           paste(dirprefix,"/2014/diary14/expd142.dta",sep=""),
                                           paste(dirprefix,"/2014/diary14/expd143.dta",sep=""),
                                           paste(dirprefix,"/2014/diary14/expd144.dta",sep="")),unsharedkey="newid"))
      
    } else {
      stop("paste- year :", year," not supported")
    }
    dat$hhid     <- as.integer(substr(as.character(dat$newid),1,nchar(as.character(dat$newid))-1))
    dat$week     <- as.integer(dat$newid)-dat$hhid*10
    
    datMapped   <- merge(dat,mappingItems,all.x=TRUE)
    uccNotFound <- unique(subset(datMapped,is.na(shortname))$ucc)
    if (length(uccNotFound)>0){
      stop("The following ucc codes were not recognised: ",toString(uccNotFound ) ," (Check normaliser)")
    }
    
    return(datMapped)
  }
  
  
  load_cex_fmli<-function(year,dirprefix,un){
    # consider gifts in the expd file
    if (year == 2004){

    } else if (year == 2009){
      dat<- (combine_subfiles(filenames=c(paste(dirprefix,"/2009/intrvw09/fmli091x.dta",sep=""),
                                          paste(dirprefix,"/2009/intrvw09/fmli092.dta",sep=""),
                                          paste(dirprefix,"/2009/intrvw09/fmli093.dta",sep=""),
                                          paste(dirprefix,"/2009/intrvw09/fmli094.dta",sep=""),
                                          paste(dirprefix,"/2009/intrvw09/fmli101.dta",sep="")),unsharedkey="newid"))
      
    } else {
      stop(paste("No files for year:",year, " in ", dirprefix))
    }
    
    return(dat)
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
      fmld             <- combine_subfiles(filenames=c(paste(dirprefix,"2009/diary09/diary09/fmld091.dta",sep=""),
                                           paste(dirprefix,"2009/diary09/diary09/fmld092.dta",sep=""),
                                           paste(dirprefix,"2009/diary09/diary09/fmld093.dta",sep=""),
                                           paste(dirprefix,"2009/diary09/diary09/fmld094.dta",sep="")),unsharedkey="newid")
      
      
      memd             <- combine_subfiles(filenames=c(paste(dirprefix,"2009/diary09/diary09/memd091.dta",sep=""),
                                   paste(dirprefix,"2009/diary09/diary09/memd092.dta",sep=""),
                                   paste(dirprefix,"2009/diary09/diary09/memd093.dta",sep=""),
                                   paste(dirprefix,"2009/diary09/diary09/memd094.dta",sep="")),unsharedkey="newid")
      
      print("Merging member data (memd) with family data(fmld)")
      mf               <- merge(fmld,memd[,c("newid","wagex")],all.left=TRUE)
      print("Filtering out non-unique entries in the data")
      mf                     <- unique(mf)
      
      return(mf)
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
    non_topcoded_hhids=as.integer(unique(hh[hh$alloc>=2,]$hhid));
    n_total_hhids <- length(unique(hh$hhid))
    print(paste("Percentage of topcoded households ignored:",100*length(non_topcoded_hhids)/n_total_hhids));
    nullpopsize_hhids<-unique(as.integer(ohs[as.character(ohs$popsize)== "",]$hhid))
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
                                     names=un()@get_diary_info_columns(year),
                                     m=un()@load_diary_fields_mapping(year))
      print("Translated hh data")
      # optional data files
      if (!is.null(ohsdat)){
        ohs = fu()@get_translated_frame(dat=ohsdat,
                                        names=un()@ohs_info_columns_us_cex_2004(),
                                        m=un()@load_ohs_mapping(year))
        
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
  
  
  get_us_cex_total_expenditures <- function (hh) {
    hh                    <- merge(hh,ddply(hh,.(hhid),summarise,week=min(week))) # choose only first available week's data
    
    print("Calculating total expenditures")
    totexp<-ddply(hh,.(hhid),summarize,total_expenditure=sum(cost))
    print("Education expense included in diary")
    print(paste("Number of households with total expenditure data = ",length(unique(totexp$hhid))))
    return(totexp)
  }
  
  max_age_educ_wage_sex<-function(age,educ,wagex,sex){
    if (any(is.na(age))){
      stop("age must be exist for all members")
    }
    if (length(age[age==max(age)])>1){
      # look at educ
      notNAEduc <- educ[!is.na(educ)]
      if (length(notNAEduc)<1 || length(notNAEduc[notNAEduc==max(notNAEduc)])>1)
      {
        # look at wagex
        notNAWage<- wagex[!is.na(wagex)]
        if(length(notNAWage)<1 || length(notNAWage[notNAWage==max(notNAWage)])>1){
          #look at sex
          if (any(is.na(sex))){
            stop("sex must be recorded for the member")
          } 
          if (length(sex[sex==max(sex)])>1){
            print(paste("age=",toString(age)))
            print(paste("educ=",toString(educ)))
            print(paste("wagex=",toString(wagex)))
            print(paste("sex=",toString(sex)))
            stop("No criteria for identifying head of the family")
          }
          else {
            return("sex")
          }
        } else {
          return("wagex")
        }
      } else {
        return("educ")
      }
    } else {
      return("age")
    }
    stop("Invalid state of the head identifiation function")
  }
  
  get_us_cex_household_head_info <- function (ohs) {
    
    #infer household status from educ, age, WAGEX, sex
    #get criteria for max selection in a new field
    #combine the data filtered 
    ohs$week               <- as.integer(ohs$newid)-as.integer(ohs$hhid)*10
    
    print("Inferring criteria for selection of heads of the households (used for personal characteristics of the family)")
    selectionCriteria      <- ddply(ohs,.(hhid,week),summarise,sf=max_age_educ_wage_sex(age,highest_educ,wagex,gender))
    selectionCriteria$week <- NULL
    selectionCriteria      <- unique(selectionCriteria )
    
    ohs                    <- merge(ohs,ddply(ohs,.(hhid),summarise,week=min(week))) # choose only one week's data
    
    print("Selecting household members with the head-criteria")
    agemax                 <- ddply(subset(merge(ohs[,c("hhid","age")],selectionCriteria), sf=="age") ,.(hhid),summarise,age=fu()@max_non_na(age))
    educmax                <- ddply(subset(merge(ohs[,c("hhid","highest_educ")],selectionCriteria), sf=="educ") ,.(hhid),summarise,highest_educ=fu()@max_non_na(highest_educ))
    wagexmax               <- ddply(subset(merge(ohs[,c("hhid","wagex")],selectionCriteria), sf=="wagex") ,.(hhid),summarise,wagex=fu()@max_non_na(wagex))
    sexmax                 <- ddply(subset(merge(ohs[,c("hhid","gender")],selectionCriteria), sf=="sex") ,.(hhid),summarise,gender=fu()@max_non_na(gender))
    
    print("Merging data from all criteria")
    heads                    <- NULL
    heads                    <- rbind(heads,merge(agemax,ohs))
    heads                    <- rbind(heads,merge(educmax,ohs))
    heads                    <- rbind(heads,merge(wagexmax,ohs))
    heads                    <- rbind(heads,merge(sexmax,ohs))
    
    if (length(setdiff(heads$hhid,ohs$hhid))>0 || length(setdiff(ohs$hhid,heads$hhid))>0 ){
      stop("Not all consumer units (hhid) were accounted for in the household-head identification")
    }
    
    print(paste("Number of houesehold heads = ",length(unique(heads$newid))))
    print(paste("Columns of heads df:",toString(colnames(heads))))

    heads<-data.frame(newid = heads$newid,
                      hhid = heads$hhid,
                      highest_educ=heads$highest_educ,
                      age=heads$age,
                      ismetro=heads$smsastat,#
                      isurban=heads$bls_urbn,#
                      race=heads$race,
                      occupation = heads$occulis1,
                      stringsAsFactors=FALSE);

    return(heads)
  }
  
  get_hsize<-function (ohs)
  {
    print ("Selecting first available week's data for hsize")
    ohs$week                <- ohs$newid-ohs$hhid*10
    ohs                     <- merge(ohs,ddply(ohs,.(hhid),summarise,week=min(week))) # choose only one week's data
#    mf$consumption_factor <-as.integer(mf$age_ref<=5)*.2+as.integer(mf$age_ref>5 & mf$age_ref<=10)*.3+as.integer(mf$age_ref>10 & mf$age_ref<=15)*.4+as.integer(mf$age_ref>15 & mf$age_ref<=45)+as.integer(mf$age_ref>45 & mf$age_ref<=65)*.7+as.integer(mf$age_ref>65)*.6
#    hsize                 <- ddply(mf[,c("hhid","age_ref","educ_ref","consumption_factor")],.(hhid),summarise,hsize=length(age_ref),consu=sum(consumption_factor))
    # use fam_type and child_num to infer consu
    hsize                  <- unique(ohs[,c("hhid","hsize")])
    return(hsize)
  }
  
  
  group_expenditure <- function(year,dirprefix,categoryName,fu,un){
    
    
    if (year ==2009) {
      
      hh               <- load_cex_diary(dirprefix=dirprefix,year=year, un=un) # must provide total and visible expenditure (must be already translated)
      extremeDataHhids <- unique ( dplyr::filter( merge(hh,ddply(hh,.(shortname),summarise,v=fu()@fv(cost)),all.x=TRUE) , cost > v)$hhid )
      print (paste("Households with extreme data (many times the median) - purged from the diary file:",length(extremeDataHhids)))
      hh              <- dplyr::filter(hh,!is.element(hhid,extremeDataHhids))
      
      groups           <- subset( un()@cex_groups(year), category == categoryName )

      # check if group columns are known
      if (!setequal(colnames(groups),c("shortname","group","category"))){
        stop("groups must strictly have shortname, group, category columns")
      } 
      
      
      #* Loading the person/family data fie
      ohsdat           <-load_ohs_file(dirprefix=dirprefix,year=year) # (using fmld) must provide age (age_ref), gender(sex_ref), 
      
      if (is.null(ohsdat) || !is.element("descrip",colnames(ohsdat))){
        stop(paste("No OHS data found in directory",dirprefix,"for year",year))
      }
      
      ohs = fu()@get_translated_frame(dat=ohsdat,
                                        names=un()@ohs_info_columns_us_cex_2004(),
                                        m=un()@ohs_mapping_us_cex_2004())
        
      print("Translated ohs data")
        
      ohs$highest_educ        <- as.integer(ohs$highest_educ)
      
      if (!(is.integer(ohs$highest_educ))){
        stop("factors must be converted an integer")
      }
      
      print("Ensuring duplicates do NOT exist in the diary hhdata")
      hh = unique(hh)
      

      ignored_hhids <- as.character(get_ignored_hhids(hh,ohs));
      if (!is.null(ignored_hhids)){
        if (!is.null(hh)){
          n1<-length(unique(hh$hhid))
          hh<-hh[!is.element(as.character(hh$hhid),ignored_hhids),]
          
          n2<-length(unique(hh$hhid))
          print(paste("ignored",n1-n2,"/",n1," hhids"))
        }
        if (!is.null(ohs)){
          
          n1<-length(unique(ohs$hhid))
          ohs<-ohs[!is.element(as.character(ohs$hhid),ignored_hhids),]
          
          n2<-length(unique(ohs$hhid))
          print(paste("ignored",n1-n2,"/",n1," hhids"))
        }
      }
      
      totexp          <- get_us_cex_total_expenditures(hh=hh)
      print("Calculated total expenditures")
      heads           <- get_us_cex_household_head_info(ohs=ohs)
      print("Calculated head info")
      hhid_personid   <- get_hsize(ohs)
      
      
      if (setequal(groups$group,c("high","low"))){
        
        ##Note: cost would not be avalable from the diary if the type of commodity in the group is an asset.
        # if we can get the asset costs populated, then the data-frame can be rbind-ed to vis data-frame.
        #
        
        vis                                  <- ddply(merge(hh,groups) ,.(hhid,group),summarise,group_cost = sum(cost)) 
        noGroupCostHhids                     <- setdiff(unique(hh$hhid),unique(vis$hhid))
        
        noGroupCostHigh                      <- data.frame(hhid=noGroupCostHhids,group="high",group_cost=rep(0,length(noGroupCostHhids)))
        noGroupCostLow                       <- data.frame(hhid=noGroupCostHhids,group="low" ,group_cost=rep(0,length(noGroupCostHhids)))
        
        vis                                  <- rbind(vis,noGroupCostHigh)
        vis                                  <- rbind(vis,noGroupCostLow)
        
        
        vis                                  <- subset(vis,!is.na(group_cost))
        vis                                  <- merge(rename(subset(vis,group=="low"),replace = c("group_cost"="low_cost")),rename(subset(vis,group=="high")[,c("hhid","group_cost")],replace = c("group_cost"="high_cost")),all=TRUE)
        vis$has_high                         <- !is.na(vis$high_cost) & vis$high_cost>0
        
        if (dim(vis[is.na(vis$high_cost),])[1]>0){
        vis[is.na(vis$high_cost),]$high_cost <- 0
        }
        if(dim(vis[is.na(vis$low_cost),])[1]>0){
        vis[is.na(vis$low_cost),]$low_cost   <- 0
        }
        vis$highratio                        <- with(vis,high_cost/(low_cost+high_cost)) + 1e-16
        vis$ln_tot_categ_exp                 <- log(with(vis,high_cost+low_cost+1e-16))
        vis$tot_categ_exp                    <- with(vis,high_cost+low_cost)
        
        vis$group                            <- NULL
        vis$low_cost                         <- NULL
        vis$high_cost                        <- NULL
        
        
      } else if (setequal(groups$group,c("asset","expenditure"))){
        
        
        assets            <- read_assets_file(year = year, dirprefix = dirprefix,fu = fu, ln = ln)
        if (dim(subset(assets,is.na(shortname) ))[1]) {
          stop(paste("Missing itemcode->shortname mapping for year",year))
        }
        expenditureData   <- merge(hh,groups)
        print(paste("Items relevant for expenditure:", toString(unique(as.character(expenditureData$shortname)))))
        
        print("Running ddply on groups and hhid")
        vis               <- ddply(expenditureData ,.(hhid,group),summarise,group_cost = sum(cost))
        no_vis_hhid       <- setdiff(unique(hh$hhid),unique(vis$hhid))
        print("Assigning zero cost to hhids with missing expenditure")
        no_vis            <- data.frame(hhid=no_vis_hhid,group="expenditure",group_cost=rep(0,length(no_vis_hhid)))
        vis               <- rbind(vis,no_vis)
        print("Filtering NA expenditure out")
        
        vis               <- rename(subset(vis,group=="expenditure"),c("group_cost"="tot_categ_exp"))
        vis               <- subset(vis, !is.na(tot_categ_exp))
        relevantAssets    <- as.character(subset(groups, group== "asset")$shortname)
        
        ady               <- get_asset_score(diaryData = hh,assetsData = assets,assetsList = relevantAssets , 
                                             ln=ln, year = year);
        vis$group         <- NULL
        vis               <- merge(vis,ady,by=c("hhid"))
        
        vis$pe  <- with(vis,tot_categ_exp/log(asset_score+1e-7))
        
        
      } else if (setequal(groups$group,c("assetsonly"))) {
        
        print("Only assets to be used as dependent variable")
        assets            <- read_assets_file(year = year, dirprefix = dirprefix,fu = fu, ln = ln)
        if (dim(subset(assets,is.na(shortname) ))[1]) {
          stop(paste("Missing itemcode->shortname mapping for year",year))
        }
        
        vis               <- ddply(merge(hh,groups) ,.(hhid,group),summarise,group_cost = sum(cost))
        no_vis_hhid       <- setdiff(unique(hh$hhid),unique(vis$hhid))
        no_vis            <- data.frame(hhid=no_vis_hhid,group="assetsonly",group_cost=rep(0,length(no_vis_hhid)))
        vis               <- rbind(vis,no_vis)
        
        relevantAssets    <- as.character(subset(groups, group== "assetsonly")$shortname)
        
        print(paste("relevantAssets=",toString(relevantAssets)))
        
        ady               <- get_asset_score(diaryData = hh,assetsData = assets,assetsList = relevantAssets , 
                                             ln=ln, year = year);
        vis$group         <- NULL
        vis               <- merge(vis,ady,by=c("hhid"),all.y=TRUE)
        if (dim (vis[is.na(vis$asset_score),])[1]>0){
          vis[is.na(vis$asset_score),]$asset_score<-0
        }
        if (dim (vis[is.na(vis$group_cost),])[1]>0){
          vis[is.na(vis$group_cost),]$group_cost<-0
        }
        
      } else if (setequal(groups$group,c("expenditureonly") )){
        
        print("Only expenditure to be used as the dependent variable");
        relevantItems    <- as.character(subset(groups, group== "expenditureonly")$shortname)
        print(paste("Expenditures being added up for items:",toString(relevantItems)))
        
        vis              <- ddply(subset( merge(hh,groups), is.element(shortname, relevantItems) ),.(hhid),summarize,group_cost=sum(cost))
        no_vis_hhid      <- setdiff(unique(hh$hhid),unique(vis$hhid))
        no_vis           <- data.frame(hhid=no_vis_hhid,group_cost=rep(0,length(no_vis_hhid)))
        vis              <- rbind(vis,no_vis)
        
      } else {
        stop( paste ( "Unknown row elements in groups frame for year", year))
      }
      
      ds                  <- merge(totexp,vis);
      
      print(paste("Merging hsize",dim(ds)[1]))
      
      ds                  <- merge(ds,hhid_personid);
      print(paste("Number of households after merging resultant with hsize data= ",length(unique(ds$hhid))))
      
      ds                  <- merge(ds,heads)
      print(paste("Number of households after merging resultant with household head data = ",length(unique(ds$hhid))))
      
      if (is.element("tot_categ_exp",colnames(ds))){
        ds$w                <- with(ds,tot_categ_exp/total_expenditure)
      }
      if (is.element("asset_score",colnames(ds))) {
        ds$ln_asset_score   <- log(ds$asset_score+1e-7)
      }
      
      ds$ln_tot_exp       <- with(ds,log(total_expenditure+1e-16))
      ds$personid         <- NULL
      return(ds)
    }
    stop(paste("merge not available for year:",year))
  }
  
  
  
  return(new("USCEXLoader",load_cex_diary=load_cex_diary,
             combined_data_set=combined_data_set,load_ohs_file=load_ohs_file,
             group_expenditure=group_expenditure,load_cex_fmli=load_cex_fmli))
}
