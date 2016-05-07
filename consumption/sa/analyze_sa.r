library(foreign)
require(plyr)
source('datanormalizer.R')

setwd('c:/local_files/research/consumption/sa/')

load_data_file_list <- function(){
  s = data.frame(year=NULL,filename=NULL,type=NULL,ohs_filename=NULL,ohs_type=NULL)
  s = rbind(s,data.frame(year=1990,
                         filename="1990/SPSS/IES 1990 hhroster_v1.1.sav",
                         type="sav",
                         ohs_filename='',
                         ohs_type="",
                         income_filename="",
                         income_file_type=""))
  s = rbind(s,data.frame(year=1995,
                         filename="1995/STATA/IES 1995.dta",
                         type="dta",
                         ohs_filename='1995/ohs/ohs_1995_v1.2_stata10/OHS 1995 Person_nw_v1.2.dta',
                         ohs_type="dta",
                         income_filename="",
                         income_file_type=""))
  s = rbind(s,data.frame(year=2010,
                         filename='2010/stata10/ies_2010_2011_house_info_v1.dta',
                         type="dta",
                         ohs_filename='2010/stata10/ies_2010_2011_person_info_v1.dta',
                         ohs_type="dta",
                         income_filename="2010/stata10/ies_2010_2011_person_income_v1.dta",
                         income_file_type="dta"))
  return (s)
}

count_higher_than<-function(a,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10) 
{ 
  return (sum(as.integer(c(m1>a,
                           m2>a,
                           m3>a,
                           m4>a,
                           m5>a,
                           m6>a,
                           m7>a,
                           m8>a,
                           m9>a,
                           m10>a)
  )));
}

merge_hh_ohs_income_data<-function(year,hh,ohs){
  
  if (year == 1995){
    return(merge_hh_ohs_data_1995(hh=hh,ohs=ohs))
  }
  if (year == 2010){
    return(merge_hh_ohs_income_data_2010(hh=hh,ohs=ohs))
  }
  
  stop(paste("Method to merge data for year:",year," not found"))
}

choose_max_age_head<-function(hh,ohs){
  ohs11=ohs[ohs$Q15RELATIONSHIP==1,]; 
  if (dim(ohs11)[1]==0){
    stop("No household head data found in ohs file")
  }
  ohs11<-data.frame(UQNO=ohs11$UQNO,AGE=ohs11$Q14AGE,PERSNO=ohs11$PERSONNO)
  x=ddply(ohs11,.(UQNO),summarize,AGE=max(AGE)) # select max age
  return(merge(x,ohs11));
}

merge_hh_ohs_income_data_2010<-function(hh,ohs,income){
  
  hh11=data.frame(UQNO = hh$UQNO,
                  gender = hh$gender_household_head,
                  total_expenditure = hh$total_expenditure,
                  race_household_head = hh$race_household_head,
                  visible_consumption = hh$visible_consumption,
                  n_members = hh$n_members);
  
  age_table <- choose_max_age_head(hh,ohs)
  
  income_table <- data.frame(UQNO=income$UQNO,PERSNO=income$Personno,
                             total_income_of_household_head=income$Value);
  age_income_table <- merge(age_table,income_table)
  
  ohs11=data.frame(hhid=ohs$UQNO,
                   persno=ohs$PERSONNO,
                   AGE=ohs$Q14AGE,
                   education=ohs$Q21HIGHESTLEVEL)
  
  # select ohs data for members with age and gender of the household head
  x=merge(hh11,ohs11)# merges on common columns in h11,ohs11
  return(x);
}

merge_hh_ohs_data_1995<-function(hh,ohs){
  rejection_threshold<-0.01
  # persno doesn't work for some 860 households
  # a merge on (hhid,age,gender) doesn't work for 50/29,750 data because same age,gender persons
  # exist in some households
  # regardless of params one needs to make sure the merged array does not have duplicates
  
  # TODO: All 
  hh11=data.frame(hhid=hh$hhid,
                  age=hh$age_household_head,
                  gender=hh$gender_household_head,
                  total_income_of_household_head=hh$total_income_of_household_head,
                  total_expenditure=hh$total_expenditure,
                  race_household_head=hh$race_household_head,
                  visible_consumption = hh$visible_consumption,
                  n_members=hh$n_members);
  
  ohs11=data.frame(hhid=ohs$hhid,
                   age=ohs$AGE,
                   gender=ohs$GENDER,
                   education=ohs$Q216,
                   area_type=ohs$TYPE)
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

run_regression<-function(ds,year){
  if (year==1995){
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
    ds$lnpinc <- log(ds$total_expenditure)
    ds$agesq <- ds$age*ds$age
    ds$year_dummy <-year-1995
    res=lm(lnvis~black_dummy+coloured_dummy+lnpinc+
             area_type+age+agesq+n_members + year_dummy,data=ds)
    return(res)
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
load_diary_fields_mapping<-function(year){
  
  if( year == 1995 ){
    return (mapping_1995());
  }
  if (year ==2010){
    return(mapping_2010());
  }
  stop(paste('No entry found for',year));
  
}


#TODO: wrap up into an object
load_visible_categories<-function(year){
  if (year == 1995){
    return(visible_categories_1995());
  }
  stop(paste('No entry found for',year));
  
}

#TODO: wrap up into an object
get_info_columns<-function(year){
  if (year == 1995){
    return(info_columns_1995());
  }
  if (year == 2010){
    return(info_columns_2010())
  }
  stop(paste("Could not find info columns for year:",year))
}

infer_n_members<-function(hh,year)
{
  if (year == 1995){
    n_members = ddply(hh,.(hhid),summarize,n_members=count_higher_than(a=0,
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
  stop(paste("infer_n_members not available for year",year))
}

combined_data_set<-function(year){
  
  #1-black
  #2-coloured
  #3-asian
  #4-white
  
  dat <- load_diary_file(year);
  # the following mapping must have the fields required in merge_hh_ohs_data/filter_hh_data
  
  diary_mapping <- load_diary_fields_mapping(year);
  
  visible_categories<-load_visible_categories(year);
  #Note: Found duplicates with x=ddply(hh,.(hhid),summarize,n=length(hhid));x[x$n>1,]
  #      ensuring there are no duplicates with unique call
  dat = unique(dat);
  # these vary according to year as well TODO: add another layer of normalization
  
  info_columns <- get_info_columns(year)
  info_columns <- c(info_columns,visible_categories)
  
  hh = get_sub_frame(dat=dat,names=info_columns,m=diary_mapping);
  
  print("Loaded subframe")
  print("Running ddply on diary")
  
  
  hh$n_members = infer_n_members(hh=hh,year=year);
  
  # Summing up visible categories into column: visible_consumption
  hh$visible_consumption <- 0
  for ( col in visible_categories){
    print(paste("Adding ",col," to visible_consumption"))
    hh$visible_consumption <- hh$visible_consumption+hh[,col]
  }
  
  # OHS file is not necessary for 2005 and later
  opers = load_ohs_file(year)
  ohs_filtering = FALSE
  
  # TODO: the dependent variable is education_household_head (instead of the  highest level
  # of education in the household). This requires that the merge matches and gender in the
  # in the OHS data)
  
  if (ohs_filtering){
    print("Running ddply on OHS")
    opers_n=ddply(opers,.(hhid),n_mempers=length(PERSNO)) # doesn't apply to all OHS values
    print("Merging OHS and Diary data")
    nh=merge(hh,opers_n);
    # the ones in the diary would be included but those not in the diary would be excluded
    result = (nh[nh$n_members-nh$n_mempers!=0,])# differences between member numbers (to be discarded/corrected)
  }
  
  dstruct<-merge_hh_ohs_income_data(hh=hh,ohs=opers);
  return(dstruct);
  
}

# Usage: get_sub_frame(dat,c("hhid","age_member2","age_member3","total_income_of_household_head","age_household_head","race_household_head"))
get_sub_frame<-function(dat,names,m){
  if (!is.vector(names)){
    stop("names must be a vector")
  }
  
  #m= mapping();
  
  mapped<-m[is.element(m$name,names),]
  array_names <- as.character(mapped$iesname)
  res=data.frame(dat[,array_names])
  # print(mapped)
  if (length(mapped$name)!=length(colnames(res))){
    stop('Error in mapping of columns')
  }
  colnames(res)<-mapped$name
  return(res)
}

load_diary_file<-function (year){
  s = load_data_file_list();
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
    return(read.dta(as.character(ff)));    
  }
  
  return(ds)
}

load_ohs_file<-function (year){
  s = load_data_file_list();
  ds= s[s$year==year,];
  print(length(ds$year));
  
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


load_income_file<-function (year){
  s = load_data_file_list();
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
}