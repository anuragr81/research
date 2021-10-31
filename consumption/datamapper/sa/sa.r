

library(foreign)
require(plyr)
require(AER)
#source('./translation/frameutils.R')
#source('./sa/sanormalize.r')

if (isClass("SA")){
  print ("Warning !!! previous definition of SA would be overwritten.")
} 
## all exported functions are declared here
setClass("SA", representation(combined_data_set = "function"))

sa<-function(){
  
  load_sa_data_file_list <- function(dirprefix){
    s = data.frame(year=NULL,filename=NULL,type=NULL,ohs_filename=NULL,ohs_type=NULL)
    s = rbind(s,data.frame(year=1990,
                           filename=paste(dirprefix,"1990/SPSS/IES 1990 hhroster_v1.1.sav",sep=""),
                           type="sav",
                           ohs_filename='',
                           ohs_type="",
                           income_filename="",
                           income_file_type=""))
    s = rbind(s,data.frame(year=1995,
                           filename=paste(dirprefix,"1995/STATA/IES 1995.dta",sep=""),
                           type="dta",
                           ohs_filename=paste(dirprefix,'1995/ohs/ohs_1995_v1.2_stata10/OHS 1995 Person_nw_v1.2.dta',sep=""),
                           ohs_type="dta",
                           income_filename="",
                           income_file_type=""))
    s = rbind(s,data.frame(year=2005,
                           filename=paste(dirprefix,"2005/Stata10/ies 2005-2006 house_info_v2.1.dta",sep=""),
                           type="dta",
                           ohs_filename=paste(dirprefix,'2005/Stata10/ies 2005-2006 person_info_v2.1.dta',sep=""),
                           ohs_type="dta",
                           income_filename=paste(dirprefix,"2005/Stata10/ies 2005-2006 person_income_v2.1.dta",sep=""),
                           income_file_type="dta"))
    s = rbind(s,data.frame(year=2010,
                           filename=paste(dirprefix,'2010/stata10/ies_2010_2011_house_info_v1.dta',sep=""),
                           type="dta",
                           ohs_filename=paste(dirprefix,'2010/stata10/ies_2010_2011_person_info_v1.dta',sep=""),
                           ohs_type="dta",
                           income_filename=paste(dirprefix,"2010/stata10/ies_2010_2011_person_income_v1.dta",sep=""),
                           income_file_type="dta"))
    return (s)
  }
  
  
  
  # choose ohs entries with max age amongst hhs with more than on head
  choose_max_ohs_age_head_2010<-function(hh,ohs){
    ohs11=ohs[ohs$relationship_to_head==1,]; 
    if (dim(ohs11)[1]==0){
      stop("No household head data found in ohs file")
    }
    ohs11<-data.frame(hhid=ohs11$hhid,age=ohs11$age,persno=ohs11$persno)
    
    x=ddply(ohs11,.(hhid),summarize,age=max(age)) # select max age
    return(merge(x,ohs11));
  }
  
  
  #@desc - merges translated data into one big data frame. There are
  #        two phases of normalization. The first is merely a translation
  #        into well-known names (age,gender etc.). The second phase is 
  #        translation into dependent variables (each of which can correspond to
  #        one function). The merging function here encapsulates the extraction from
  #        all tables and merging into a combined frame.
  merge_sa_hh_ohs_income_data<-function(fu,year,hh,ohs,income){
    
    if (year == 1995){
      return(merge_sa_hh_ohs_data_1995(fu=fu,hh=hh,ohs=ohs))
    }
    if (year == 2010){
      return(merge_sa_hh_ohs_income_data_2010(fu=fu,hh=hh,ohs=ohs,income=income))
    }
    if (year == 2005){
      return(merge_sa_hh_ohs_income_data_2005(fu=fu,hh=hh,ohs=ohs,income=income))
    }
    
    stop(paste("Method to merge data for year:",year," not found"))
  }
  
  # extract data from hh,ohs,income and merge into one combined frame
  merge_sa_hh_ohs_income_data_2010<-function(fu,hh,ohs,income){
    
    hh<-sum_visible_categories(hh=hh,visible_categories = sa_visible_categories_2010())
    
    hh11=data.frame(hhid = hh$hhid,
                    gender = hh$gender_household_head,
                    total_expenditure = hh$total_expenditure,
                    race_household_head = hh$race_household_head,
                    visible_consumption = hh$visible_consumption,
                    area_type=hh$area_type,
                    n_members = hh$n_members);
    
    age_table <- choose_max_ohs_age_head_2010(hh,ohs)
    
    income_table <- data.frame(hhid=income$hhid,persno=income$persno,
                               total_income_of_household_head=income$total_income)
    
    age_income_table <- merge(age_table,income_table)
    
    ohs11=data.frame(hhid=ohs$hhid,
                     persno=ohs$persno,
                     age=ohs$age,
                     education=ohs$highest_education)
    
    ohs11<-merge(ohs11,age_income_table)
    # select ohs data for members with age and gender of the household head
    x=merge(hh11,ohs11)# merges on common columns in h11,ohs11
    return(x);
  }
  
  
  # extract data from hh,ohs,income and merge into one combined frame
  merge_sa_hh_ohs_income_data_2005<-function(fu,hh,ohs,income){
    
    isDebug<-FALSE
    
    hh<-sum_visible_categories(hh=hh,visible_categories = sa_visible_categories_2005())
    
    if (isDebug){
      print(paste("columns of hh:",toString(colnames(hh))));
      print(paste("size of hh:",toString(dim(hh))));
    }
    
    # reducing the data to work on
    hh11=data.frame(hhid = hh$hhid,
                    gender = hh$gender_household_head,
                    total_expenditure = hh$total_expenditure,
                    race_household_head = hh$race_household_head,
                    visible_consumption = hh$visible_consumption,
                    area_type=hh$area_type,
                    n_members = hh$n_members);
    
    ohs11=data.frame(hhid=ohs$hhid,
                     persno=ohs$persno,
                     age=ohs$agegrp,
                     education=ohs$highest_education)
    
    print("agegrp has been converted to age")
    
    income_table <- data.frame(hhid=income$hhid,persno=income$persno,
                               total_income_of_household_head=income$total_income);
    
    ohs11 <- merge(ohs11,income_table)
    
    ohs11=ohs11[ohs11$persno==1,] # heads have persno = 1
    
    # select ohs data for members with age and gender of the household head
    x=merge(hh11,ohs11)# merges on common columns in h11,ohs11
    return(x);
    
  }
  
  # merge data from hh,ohs files into one common big frame
  merge_sa_hh_ohs_data_1995<-function(hh,ohs){
    
    #infer should come from processing of hh,ohs,income..etc. (should not be here)
    hh$n_members = infer_n_members_1995(fu=fu,hh=hh);
    
    # compute visible consumption
    hh<-sum_visible_categories(hh=hh,visible_categories=sa_visible_categories_1995())
    
    rejection_threshold<-0.01
    # persno doesn't work for some 860 households
    # a merge on (hhid,age,gender) doesn't work for 50/29,750 data because same age,gender persons
    # exist in some households
    # regardless of params one needs to make sure the merged array does not have duplicates
    
    hh11=data.frame(hhid=hh$hhid,
                    age=hh$age_household_head,
                    gender=hh$gender_household_head,
                    total_income_of_household_head=hh$total_income_of_household_head,
                    total_expenditure=hh$total_expenditure,
                    race_household_head=hh$race_household_head,
                    visible_consumption = hh$visible_consumption,
                    n_members=hh$n_members);
    
    ohs11=data.frame(hhid=ohs$hhid,
                     age=ohs$age,
                     gender=ohs$gender,
                     education=ohs$highest_education,
                     area_type=ohs$area_type)
    
    # select ohs data for members with age and gender of the household head
    x=merge(hh11,ohs11)# merges on common columns in h11,ohs11
    y=ddply(x,.(hhid),summarize,n_mem=length(hhid))
    n_nonuniq=length(y[y$n_mem>1,]$hhid)
    n_tot= length(unique(hh$hhid));
    # if this happens for more than 1% (or threshold) of the data, then reject analysis
    if (n_nonuniq/n_tot>rejection_threshold){
      stop ("Could not match ohs data with hh data ")
    }else{
      print(paste("Percentage of hhids with non-uniq (gender,age) for household-heads:",100*n_nonuniq/n_tot))
      valid_hhids<-y[y$n_mem<=1,]$hhid
      # return data for household with heads 
      # having unique (gender,age)
      return(x[is.element(x$hhid,valid_hhids),]);
    }
  }
  
  run_sa_regression_multiple_years<-function(years,dirprefix,san){
    years <-c (2005,2010)
    
    if (!is.vector(years) || !is.numeric(years)){
      stop("years must of a vector numbers")
    }
    
    resds = NULL
    
    for (year in years){
      ds=combined_data_set(san=san,year=year,dirprefix = dirprefix)
      resds = rbind(resds,ds)
    }
    
    ds$inc <-ds$total_income_of_household_head
    ds$incpsv <- as.integer(ds$inc>0)
    ds$lninc <-log(ds$inc)
    ds$cbinc <- ds$inc*ds$inc*ds$inc
    ds$secd <- as.integer(ds$education>=8 && ds$education <=12)
    ds$degree <-as.integer(ds$education==13)
    ds$year_2005 <- 0 || 1 
    # no secondary education is the dropped dummy
    res= ivreg(lnvis~black_dummy+coloured_dummy+ lnpinc+area_type+age+agesq+n_members+year_dummy | . - lnpinc + inc + incpsv + lninc + cbinc + secd + degree )
    return(res)
    # run regression on the combined data set
  }
  
  run_regression<-function(ds,year,type){
    if (year==1995 || year =="2005_2010"){
      # ln(visible_consumption) ~ black_dummy + coloured_dummy + ln(pInc) 
      #     + area_type + age + age*age + n_members + year_dummy
      if (class(ds$race_household_head)!="integer"){
        stop("race variable must be of integer type")
      }
      
      # Do NOT consider hhds with zero income heads
      n_all_income<-length(ds$hhid);
      ds <- ds[ds$total_income_of_household_head>0,]
      n_cur<-length(ds$hhid)
      print (paste("Ignoring ",round(100*(1- n_cur/n_all_income),3),
                   " % households(hhids having heads with zero income) at sample size=",n_cur))
      ds<-ds[ds$visible_consumption>0,]
      n_vis<-length(ds$hhid)
      print (paste("Ignoring altogether",round(100*(1- n_vis/n_all_income),3),
                   " % households(hhids with zero visible income) at sample size=",n_vis))
      
      ds$lnvis<-log(ds$visible_consumption)
      ds$black_dummy <-as.integer(ds$race_household_head==1)
      ds$coloured_dummy<-as.integer(ds$race_household_head==2)
      print("Regression using only 1995 data")
      
      if (type=="no_controls"){
        res = lm(lnvis~black_dummy+coloured_dummy,data=ds)
        # the dummies themselves black_dummy + coloured_dummy show negative coefficients for lnvsi as dependent variable
        return(res)
      }
      if (type=="income_controls"){
        ds$inc <-ds$total_income_of_household_head # income control
        #ds$incpsv <- as.integer(ds$inc>0) # income control - ignored because we only consider positive income households
        ds$lninc <-log(ds$inc)# income control
        ds$cbinc <- ds$inc*ds$inc*ds$inc # income control
        # only lninc is significant
        res=lm(lnvis~black_dummy+coloured_dummy+ lninc ,data=ds)
        #TODO: compare with ivreg (and perform the Hausman test)
        return(res)
      }
      if (type == "incpinc_controls"){
        ds$inc <-ds$total_income_of_household_head # income control
        ds$lninc <-log(ds$inc)# income control
        ds$lnpinc <- log(ds$total_expenditure)
        
        res=lm(lnvis~black_dummy+coloured_dummy+
                 lninc+lnpinc,data=ds)
        return(res)
      }
      if (type == "iv1") {
        ds$agesq <- ds$age*ds$age
        #ds$year_dummy <-year-1995
        
        ds$inc <-ds$total_income_of_household_head
        ds$incpsv <- as.integer(ds$inc>0) # income control
        ds$lninc <-log(ds$inc)# income control
        ds$cbinc <- ds$inc*ds$inc*ds$inc # income control
        ds$lsecd <-as.integer(ds$education<8) 
        ds$secd <- as.integer(ds$education>=8 && ds$education <=12)
        ds$degree <-as.integer(ds$education==13)
        
        ds$lnvis <-log(ds$visible_consumption)
        
        ds$lnpinc <- log(ds$total_expenditure)
        
        res= ivreg(lnvis~black_dummy+coloured_dummy+ lninc+ lnpinc |
                     . - lnpinc + cbinc + lsecd + secd + degree ,data=ds)
        return(res)
        
      }
      
      if (type == "iv2") {
        ds$agesq <- ds$age*ds$age
        #ds$year_dummy <-year-1995
        
        ds$inc <-ds$total_income_of_household_head
        ds$incpsv <- as.integer(ds$inc>0) # income control
        ds$lninc <-log(ds$inc)# income control
        ds$cbinc <- ds$inc*ds$inc*ds$inc # income control
        ds$lsecd <-as.integer(ds$education<8) 
        ds$secd <- as.integer(ds$education>=8 && ds$education <=12)
        ds$degree <-as.integer(ds$education==13)
        
        ds$lnvis <-log(ds$visible_consumption)
        
        ds$lnpinc <- log(ds$total_expenditure)
        
        res= ivreg(lnvis~black_dummy+coloured_dummy+ lnpinc  +lsecd |
                     . - lnpinc + cbinc+lninc +incpsv,data=ds)
        return(res)
        
      }
      
      if (type == "ivt1"){
        ds$agesq <- ds$age*ds$age
        #ds$year_dummy <-year-1995
        
        ds$inc <-ds$total_income_of_household_head
        ds$incpsv <- as.integer(ds$inc>0) # income control
        ds$lninc <-log(ds$inc)# income control
        ds$cbinc <- ds$inc*ds$inc*ds$inc # income control
        ds$lsecd <-as.integer(ds$education<8) 
        ds$secd <- as.integer(ds$education>=8 && ds$education <=12)
        ds$degree <-as.integer(ds$education==13)
        ds$year2005 <- as.integer(ds$year==2005)
        ds$lnvis <-log(ds$visible_consumption)
        
        ds$lnpinc <- log(ds$total_expenditure)
        #res=lm(lnvis~black_dummy+coloured_dummy+ lninc+ lnpinc + year2005,data=ds)
        
        res= ivreg(lnvis~black_dummy+coloured_dummy+ lninc+ lnpinc + year2005 |
                     . - lnpinc + cbinc + lsecd + secd + degree ,data=ds)
        return(res)
        
      }
      if (type == "ivt2"){
        ds$agesq <- ds$age*ds$age
        ds$year2005 <- as.integer(ds$year==2005)
        
        ds$inc <-ds$total_income_of_household_head
        ds$incpsv <- as.integer(ds$inc>0) # income control
        ds$lninc <-log(ds$inc)# income control
        ds$cbinc <- ds$inc*ds$inc*ds$inc # income control
        ds$lsecd <-as.integer(ds$education<8) 
        ds$secd <- as.integer(ds$education>=8 && ds$education <=12)
        ds$degree <-as.integer(ds$education==13)
        
        ds$lnvis <-log(ds$visible_consumption)
        
        ds$lnpinc <- log(ds$total_expenditure)
        
        res= ivreg(lnvis~black_dummy+coloured_dummy+ lnpinc  +lsecd+year2005 |
                     . - lnpinc + cbinc+lninc +incpsv,data=ds)
        return(res)
        
      }
      if (type == "ivd1") {
        ds$agesq <- ds$age*ds$age
        #ds$year_dummy <-year-1995
        
        ds$inc <-ds$total_income_of_household_head
        ds$incpsv <- as.integer(ds$inc>0) # income control
        ds$lninc <-log(ds$inc)# income control
        ds$cbinc <- ds$inc*ds$inc*ds$inc # income control
        ds$lsecd <-as.integer(ds$education<8) 
        ds$secd <- as.integer(ds$education>=8 && ds$education <=12)
        ds$degree <-as.integer(ds$education==13)
        
        ds$lnvis <-log(ds$visible_consumption)
        
        ds$lnpinc <- log(ds$total_expenditure)
        
        res= ivreg(lnvis~black_dummy+coloured_dummy+ lninc+ lnpinc + age+ n_members + area_type|
                     . - lnpinc + cbinc + lsecd + secd + degree ,data=ds)
        return(res)
        
      }
      if (type == "ivd2") {
        ds$agesq <- ds$age*ds$age
        #ds$year_dummy <-year-1995
        
        ds$inc <-ds$total_income_of_household_head
        ds$incpsv <- as.integer(ds$inc>0) # income control
        ds$lninc <-log(ds$inc)# income control
        ds$cbinc <- ds$inc*ds$inc*ds$inc # income control
        ds$lsecd <-as.integer(ds$education<8) 
        ds$secd <- as.integer(ds$education>=8 && ds$education <=12)
        ds$degree <-as.integer(ds$education==13)
        
        ds$lnvis <-log(ds$visible_consumption)
        
        ds$lnpinc <- log(ds$total_expenditure)
        
        res= ivreg(lnvis~black_dummy+coloured_dummy+ lnpinc  +lsecd  + age + n_members + area_type |
                     . - lnpinc + cbinc+lninc +incpsv,data=ds)
        return(res)
        
      }
    }
    
    stop(paste("Year",year,"not supported"));
  }
  
  descriptive_statistics<-function(ds){
    
    # dummy would count every member with valid value as 1 
    ds$less_than_12 = as.integer(ds$education<12);
    r=ddply(ds,.(race_household_head),summarize,
            mean_income=mean(total_income_of_household_head),
            n=length(total_income_of_household_head),
            mean_tot_expenditure= mean(total_expenditure),
            mean_age_head=mean(age),
            mean_visible_consumption=mean(visible_consumption),
            n_members=mean(n_members),
            n_less_than_12=mean(less_than_12));
    r$mean_visible_consumption_fraction<-r$mean_visible_consumption/r$mean_tot_expenditure
    
    r$percentage = 100*r$n/sum(r$n);
    return(r);
    
  }
  
  
  #TODO: wrap up into an object
  load_sa_diary_fields_mapping<-function(san,year){
    
    if( year == 1995 ){
      return (san()@hh_mapping_1995());
    }
    if (year == 2010){
      return(san()@hh_mapping_2010());
    }
    
    if (year == 2005){
      return(san()@hh_mapping_2005());
    }
    stop(paste('No entry found for',year));
    
  }
  load_sa_ohs_mapping<-function(san,year){
    
    if( year == 1995 ){
      return (san()@sa_ohs_mapping_1995());
    }
    if (year == 2010){
      return(san()@sa_ohs_mapping_2010());
    }
    if (year ==2005){
      return(san()@sa_ohs_mapping_2005());
    }
    stop(paste('No entry found for',year));
    
  }
  
  load_sa_income_mapping<-function(san,year){
    
    if( year == 1995 ){
      stop("No income file for 1995")
    }
    if (year == 2010){
      return(san()@sa_income_mapping_2010());
    }
    
    if (year == 2005){
      return(san()@sa_income_mapping_2005());
    }
    stop(paste('No entry found for',year));
    
  }
  # Notice that these are defined in terms of mapping fields
  sa_visible_categories_1995<-function(){
    # personal care, clothing and apparel (including footwear),jewelry, cars
    return (c("motor_cars_new","bakkies_new","caravantrailers_new",
              "motor_cars_used","bakkies_used","caravantrailers_new","hire_of_clothing",
              "jewelry","handbags","total_boys_footwear",
              "total_mens_footwear","total_girls_footwear","total_infants_footwear",
              "total_womens_footwear","total_infants_clothing",
              "total_boys_clothing","total_mens_clothing","total_girls_clothing",
              "total_womens_clothing","total_personal_care"))
  }
  
  sa_visible_categories_2010<-function(){
    return(c("num_cars"))
  }
  
  sa_visible_categories_2005<-function(){
    return(c("num_cars"))
  }
  
  
  # Info Columns are names in a commonly-understood nomenclature
  # all merging/aggregation rules are specified in terms of these
  # translated names
  get_sa_diary_info_columns<-function(san,year){
    if (year == 1995){
      return(san()@diary_sa_info_columns_1995(sa_visible_categories_1995()));
    }
    if (year == 2010){
      return(san()@diary_sa_info_columns_2010(sa_visible_categories_2010()))
    }
    if (year == 2005){
      
      return(san()@diary_sa_info_columns_2005(sa_visible_categories_2005()))
    }
    stop(paste("Could not find diary info columns for year:",year))
  }
  
  get_sa_ohs_info_columns<-function(san,year){
    if (year == 1995){
      return(san()@ohs_info_columns_1995());
    }
    if (year == 2010){
      return(san()@ohs_info_columns_2010())
    }
    if (year ==2005){
      return(san()@ohs_info_columns_2005());
    }
    
    stop(paste("Could not find ohs info columns for year:",year))
  }
  
  get_sa_income_info_columns<-function(san,year){
    if (year == 1995){
      return(san()@income_info_columns_1995());
    }
    if (year == 2010){
      return(san()@sa_income_info_columns_2010())
    }
    
    if (year == 2005){
      return(san()@sa_income_info_columns_2005())
    }
    stop(paste("Could not find income info columns for year:",year))
  }
  # utility to infer number of members from 1995 sa diary 
  infer_n_members_1995<-function(fu,hh)
  {
    print("infer_n_members_1995 - Running ddply on diary ")
    n_members = ddply(hh,.(hhid),summarize,n_members=fu()@count_higher_than(a=0,
                                                                       m1=age_household_head,
                                                                       m2=age_member2,
                                                                       m3=age_member3,
                                                                       m4=age_member4,
                                                                       m5=age_member5,
                                                                       m6=age_member6,
                                                                       m7=age_member7,
                                                                       m8=age_member8,
                                                                       m9=age_member9,
                                                                       m10=age_member10))
    return(n_members$n_members)
  }
  #@desc - 
  sum_visible_categories<-function(hh,visible_categories){
    isDebug=FALSE
    
    if (isDebug==TRUE){
      print(paste("visible:",colnames(hh)))
    }
    hh$visible_consumption <- 0
    for ( col in visible_categories){
      print(paste("Adding ",col," to visible_consumption"))
      hh$visible_consumption <- hh$visible_consumption+hh[,col]
    }
    return(hh)
  }
  #@desc prepares the normalized data-set with all
  #      information for regression analysis
  combined_data_set<-function(year,dirprefix,san,fu){
    
    #1-black
    #2-coloured
    print("Translated hh data")
    #3-asian
    #4-white
    
    ############ PHASE 0 ########################
    
    hhdat <- load_sa_diary_file(year,dirprefix)
    ohsdat <-load_sa_ohs_file(year,dirprefix)
    incomedat <-load_sa_income_file(year,dirprefix)
    
    if (is.null(hhdat)){
      stop("Could not load diary hhdata")
    }
    
    # duplicates in diary hhdata are not uncommon
    print("Ensuring duplicates do NOT exist in the diary hhdata")
    hhdat = unique(hhdat)
    
    ############ PHASE 1 - Translation ########################
    # info_columns must contain all hhdata-fields referred to in merging/aggregation phase (one per file)
    # translated frame makes the data available in a universal dictionary (age, gender etc.)
    
    hh = fu()@get_translated_frame(dat=hhdat,
                              names=get_sa_diary_info_columns(san=san,year=year),
                              m=load_sa_diary_fields_mapping(san=san,year=year))
    # optional data files
    if (!is.null(ohsdat)){
      ohs = fu()@get_translated_frame(dat=ohsdat,
                                 names=get_sa_ohs_info_columns(san=san,year=year),
                                 m=load_sa_ohs_mapping(san=san,year=year))
      
      print("Translated ohs data")
    }
    
    if (!is.null(incomedat)){
      income = fu()@get_translated_frame(dat=incomedat,
                                    names=get_sa_income_info_columns(san=san,year=year),
                                    m=load_sa_income_mapping(san=san,year=year))
      print("Translated income data")
    }
    
    print("Loaded translated frame(s)")
    ############ PHASE 2 - Aggregation and Merge ########################
    # merge criteria is defined for every dependent variable
    
    dstruct<-merge_sa_hh_ohs_income_data(fu=fu,hh=hh,ohs=ohs,income=income,year=year)
    return(dstruct);
  }
  
  # Usage: fu()@get_translated_frame(dat,c("hhid","age_member2","age_member3","total_income_of_household_head","age_household_head","race_household_head"))
  
  # returns NULL when the filenames are ''
  load_sa_diary_file<-function (year,dirprefix){
    s = load_sa_data_file_list(dirprefix);
    ds= s[s$year==year,]
    if( length(ds$year)==0 ){
      stop("No entry found")
    }
    
    if (ds$type=="sav"){
      ff<-ds$filename
      print(paste("Loading File:",ff))
      return(read.spss(as.character(ff)));
    }
    
    if (ds$type =="dta"){
      ff<-ds$filename
      print(paste("Loading File:",ff))
      return(read.dta(as.character(ff),convert.factors=FALSE));
    }
    
    if (ds$type=='' && ds$filename==''){
      return(NULL)
    }
    
    stop("No valid entry found");
  }
  
  load_sa_ohs_file<-function (year,dirprefix){
    s = load_sa_data_file_list(dirprefix);
    ds= s[s$year==year,];

    if( length(ds$year)==0 ){
      stop("No entry found")
    }
    
    if (ds$ohs_type=="sav"){
      ff<-ds$ohs_filename
      print(paste("Loading file:",ff));
      return(read.spss(as.character(ff)));
    }
    
    if (ds$ohs_type =="dta"){
      ff<-ds$ohs_filename
      print(paste("Loading file:",ff))
      return(read.dta(file=as.character(ff),convert.factors=FALSE));    
    }
    
    if (ds$ohs_type=='' && ds$ohs_filename==''){
      return(NULL)
    }
    
    return(ds)
  }
  
  
  load_sa_income_file<-function (year,dirprefix){
    s = load_sa_data_file_list(dirprefix);
    ds= s[s$year==year,];
    print(length(ds$year));
    
    if( length(ds$year)==0 ){
      stop("No entry found")
    }
    
    if (ds$income_file_type=="sav"){
      ff<-ds$income_filename
      print(paste("Loading file:",ff));
      return(read.spss(as.character(ff)));
    }
    
    if (ds$income_file_type =="dta"){
      ff<-ds$income_filename
      print(paste("Loading file:",ff))
      return(read.dta(file=as.character(ff),convert.factors=FALSE));    
    }
    
    if (ds$income_file_type=='' && ds$income_filename==''){
      return(NULL)
    }
    stop("No valid entry found")
  }
  
  
  return(new("SA",combined_data_set = combined_data_set))
}