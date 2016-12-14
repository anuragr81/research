
# duplicates in diary hhdata are not uncommon
library(foreign)
require(plyr)
require(AER)
source('../panelfunc.R')
source('../regressions.R')

setwd('c:/local_files/research/consumption/cex/');

#
# Tasks
# 1. Incorporate high-price regions (dummy) and high-population-density (dummy) and re-run simple2 as well as 2sls again
# 2. prepare maps of scarcity for other countries - inspecting whether scarcity maps overlaps with items sorted on visibility 
# 3. load India data and perform zahra's analysis without visibility survey
#

lsms_default_vars_init<-function(){
  return(c("total_expenditure","age","hsize","housingstatus","occupation_rank","isrural","region",
           "english","roomsnum","years_community","is_resident"))
}

lsms_default_ln_vars_init<-function(){
  return(c("lnpinc","age","hsize","housingstatus","occupation_rank","isrural","highest_educ","region",
           "english","roomsnum","years_community","is_resident"))
}


lsms_latest_ln_vars_init<-function(){
  return(c("lnpinc","age","hsize","housingstatus","occupation_rank","isrural","highest_educ","expensiveregion","popdensity",
           "english","roomsnum","years_community","is_resident"))
}


get_expensiveregion_codes<-function(){
  return (c(7,12,19,53)) 
}

get_instruments_for_item<-function(item){
  if (!is.character(item)){
    stop("item must be a character type")
  }
  instrument_table=list();
  instrument_table[['carpetsrugs']]=c("occupation","years_community","roomsnum","is_resident","region","isrural")
  instrument_table[['dseducexpense']]=c("years_community","roomsnum","tothouserent","region")
  instrument_table[['dselectricity']]=c("ln_highest_educ","cubic_highest_educ","ln_age","occupation","years_community","roomsnum","tothouserent","toteducexpense","accessiblemarket","litlang");
  instrument_table[['dspersonalitemsrepair']]=c("ln_highest_educ","cubic_highest_educ","ln_age","occupation","years_community","roomsnum","tothouserent","toteducexpense");
  instrument_table[['sportshobbyequipment']]=c("ln_highest_educ","occupation","years_community","roomsnum","toteducexpense")
  instrument_table[['dshouserent']]=c("ln_highest_educ","cubic_highest_educ","ln_age","occupation","years_community","toteducexpense","accessiblemarket","litlang");
  instrument_table[['funeralcosts']]=c("ln_highest_educ","hsize","cubic_highest_educ","occupation","toteducexpense","tothouserent");
  instrument_table[['marriagecosts']]=c("ln_highest_educ","cubic_highest_educ","occupation","toteducexpense","tothouserent");
  instrument_table[['dspersonalprods']]=c("ln_highest_educ","cubic_highest_educ","occupation","toteducexpense","tothouserent");
  instrument_table[['dsskincream']] =c("ln_highest_educ","cubic_highest_educ","occupation","toteducexpense","tothouserent");
  instruments_list<-instrument_table[[item]]
  if (is.null(instruments_list)){
    stop(paste("No instrument found for item:",toString(item)))
  }
  return (instruments_list)
  
}

get_latest_instruments_for_item<-function(item){
  if (!is.character(item)){
    stop("item must be a character type")
  }
  instrument_table=list();
  instrument_table[['carpetsrugs']]=c("occupation","years_community","roomsnum","is_resident","region","isrural")
  instrument_table[['dseducexpense']]=c("years_community","roomsnum","tothouserent")
  instrument_table[['dselectricity']]=c("ln_highest_educ","cubic_highest_educ","occupation","years_community","roomsnum","tothouserent","toteducexpense","accessiblemarket","litlang");
  instrument_table[['dspersonalitemsrepair']]=c("ln_highest_educ","cubic_highest_educ","occupation","years_community","roomsnum","tothouserent","toteducexpense");
  instrument_table[['sportshobbyequipment']]=c("years_community","roomsnum","is_resident")
  instrument_table[['dshouserent']]=c("ln_highest_educ","cubic_highest_educ","occupation","years_community","toteducexpense","accessiblemarket","litlang");
  instrument_table[['funeralcosts']]=c("ln_highest_educ","hsize","tothouserent");
  instrument_table[['marriagecosts']]=c("ln_highest_educ","cubic_highest_educ","occupation","toteducexpense","tothouserent");
  instrument_table[['dspersonalprods']]=c("ln_highest_educ","cubic_highest_educ","occupation","toteducexpense","tothouserent");
  instrument_table[['dsskincream']] =c("occupation","toteducexpense","tothouserent");
  instruments_list<-instrument_table[[item]]
  if (is.null(instruments_list)){
    stop(paste("No instrument found for item:",toString(item)))
  }
  return (instruments_list)
  
}

get_presentation_name<-function(item){
  
  presnames=list();
  presnames[['carpetsrugs']]='carpetsrugs';
  presnames[['dseducexpense']]='education' 
  presnames[['dselectricity']]='electricity'
  presnames[['dshouserent']]='houserent'
  presnames[['dspersonalitemsrepair']]= 'personalitemsrepair'
  presnames[['dspersonalprods']]='personalproducts'
  presnames[['dsskincream']]= 'cosmetics'
  presnames[['funeralcosts']]='funeral'
  presnames[['marriagecosts']]='marriage'
  presnames[['sportshobbyequipment']]='hobbyequipment'
  itemname=presnames[[item]]
  if (is.null(itemname)){
    stop(paste("presentation name not found for:",toString(item)))
  }
  return(itemname)
}

runTest<-function(outfile,regtype,parametersFunc,instrumentsFunc){
  
  items<-c('carpetsrugs', 'dseducexpense', 'dselectricity',
           'dshouserent', 'dspersonalitemsrepair', 'dspersonalprods', 
           'dsskincream', 'funeralcosts', 'marriagecosts', 'sportshobbyequipment')
  library(stargazer)
  resList = list()
  columnLabels = array()
  count=1
  for (item in items){
    resList[[count]]<-item_analysis(itemname = item,
                                    regtype = regtype,
                                    commodity_type = item,
                                    parameters_func = parametersFunc,
                                    instruments_func = instrumentsFunc)
    count=count+1
    #resdf<-as.data.frame(summary(res)$coefficients)
    #results<-paste(results,"\n",item,":",toString(rownames(resdf)))
  }
  write(do.call(stargazer,resList),outfile)
  #write(stargazer(res,column.labels = c("testerchamp1","testerchamp2"),label="visibilitycheck"),"c:/temp/1.tex")
  #write(results,outfile)
}

get_item_diary_code <- function (item) {
  if (!is.character(item)){
    stop(paste(item,"must be a string"))
  }
  item_codes = list()
  item_codes[["dselectricity"]] = c(202)
  item_codes[["carpetsrugs"]]=c(301)
  item_codes[["dspersonalitemsrepair"]]=c(224)
  item_codes[["sportshobbyequipment"]]=c(306)
  item_codes[["funeralcosts"]]=c(314)
  item_codes[['marriagecosts']]=c(313)
  item_codes[['dspersonalprods']]=c(214)
  item_codes[['dsskincream']]=c(213)
  
  
  code <- item_codes[[item]]
  if (is.null(code)){
    stop(paste("Could not find itemcode for",item))
  }
  return(code)
}

get_ds_for_item<-function(itemname){
  if (is.element(itemname, c("dseducexpense","dshouserent"))){
    # vis is not needed for these categories
    ds<-combined_data_set(dataset = "lsms",year = 2010,selected_category = NULL ,isTranslated = TRUE, set_depvar=FALSE)
    
  } else {
    diaryCode = get_item_diary_code(itemname)
    ds<-combined_data_set(dataset = "lsms",year = 2010,selected_category = diaryCode ,isTranslated = TRUE)
  }
}

item_analysis<-function(itemname,regtype,commodity_type,ds,parameters_func,instruments_func){
  
  if (missing(parameters_func)){
    parameters_func<-lsms_default_ln_vars_init
  }
  
  if (missing(instruments_func)){
    instruments_func<-get_instruments_for_item;
  }
  
  if (missing(ds)){
    ds<-get_ds_for_item(itemname)
  }
  
  #return(ds)
  source('../regressions.R')
  
  if (regtype=="engel") {
    varsInfo = list()
    if (is.element(itemname, c("dseducexpense","dshouserent"))){
      varsInfo[["depvar"]]=paste("ln",itemname,sep="");
    }
    else {  
      varsInfo[["depvar"]]="visible_consumption"
    }
    res<-run_regression_lsms(ds,regtype,commodity_type,varsInfo)
    return(res)
  }
  
  if (regtype == "2sls"){
    varsInfo = list()
    if (is.element(itemname, c("dseducexpense","dshouserent"))){
      varsInfo[["depvar"]]=paste("ln",itemname,sep="");
    }else{
      varsInfo [["depvar"]]="lnvis"
    }
    varsInfo[["instrument_list"]]=instruments_func(itemname);
    varsInfo [["vars_list"]]=parameters_func();
    varsInfo[["endogenous_vars"]] = "lnpinc"
    
    
    if (is.null(varsInfo[["instrument_list"]])){
      stop("Cannot have null instrument_list")
    }
    
    res<-run_regression_lsms(ds,"2sls",commodity_type,varsInfo)
    return(res)
  }
  
  if (regtype == "simple2"){
    
    varsInfo = list()
    if (is.element(itemname, c("dseducexpense","dshouserent"))){
      varsInfo [["depvar"]]=paste("ln",commodity_type,sep="");
    }else {
      varsInfo [["depvar"]]="lnvis"
    }
    
    varsInfo [["vars_list"]]=parameters_func();
    res<-run_regression_lsms(ds,"simple2",commodity_type,varsInfo)
    return(res)
  }
  
  stop(paste("analysis of type",regtype, "not supported"))
  
}


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

load_diary_file <-function(dataset,year){
  #* ((
  if (dataset == "lsms"){
    if (year == 2010){
      # combine sections ( k , l, m )
      kdat <- read_tnz("../lsms/TZNPS2HH3DTA/HH_SEC_K1.dta",FALSE)
      #*    Reading weekly Diary data in Section K data and retrieving item as well as the quantity as well as cost 
      
      k <- get_translated_frame(dat=kdat,
                                names=diary_info_columns_lsms_2010(),
                                m=hh_mapping_lsms_2010())
      k$hhid <-as.character(k$hhid)
      #*    Ignored items where there is no associated cost
      k <- k[as.numeric(k$cost)>0 & !is.na(k$cost),]
      k$item<-k$item+10000 # adding 10,000 only to avoid overlaps with sections (l,m)
      factor <- 52
      
      #*    Multiplied weekly diary data by 52 (to look at annual data)
      # quantities are normalized to annual values
      k$cost <- k$cost*factor
      k$lwp <- k$lwp *factor
      k$own <-k$own*factor
      k$gift <-k$gift*factor
      
      #*    gift quantities are ignored (total quantity ignored is to be presented)
      #*    weekly recall items are also multiplied by 52
      
      ldat <- read_tnz("../lsms/TZNPS2HH2DTA/HH_SEC_L.dta",FALSE)
      l <- get_translated_frame(dat=ldat,
                                names=get_lsms_secl_info_columns_2010(),
                                m=get_lsms_secl_fields_mapping_2010())
      l$hhid <-as.character(l$hhid)
      l <- l[!is.na(l$cost) & l$cost>0 & !is.na(l$hhid),]
      weekly_recall_items <-c(101,102,103)
      
      # l is weekly and  monthly data
      
      l <- multiplyLsmsQuantities(dat = l , 
                                  quantity_field_name="cost", 
                                  item_field_name="item", 
                                  factor=52,
                                  items_list = weekly_recall_items)
      
      monthly_recall_items <- c("201", "202", "203", "204", "205", "206", "207", "208", "209",
                                "210", "211", "212", "213", "214", "215", "216", "217", "218", "219",
                                "220", "221", "222", "223", "224")  
      #*    Monthly recall items are multiplied by 12
      l <- multiplyLsmsQuantities(dat = l , 
                                  quantity_field_name="cost", 
                                  item_field_name="item", 
                                  factor=12,
                                  items_list = monthly_recall_items)
      
      # m is yearly data
      mdat <-read_tnz( '../lsms/TZNPS2HH2DTA/HH_SEC_M.dta',FALSE)
      m <- get_translated_frame(dat=mdat,
                                names=get_lsms_secm_info_columns(2010),
                                m=get_lsms_secm_fields_mapping(2010))
      m$hhid <-as.character(m$hhid)
      m<- m[!is.na(m$hhid) & !is.na(m$cost) & m$cost>0,]
      # nothing to be multiplied for yearly-recall (since we're looking at annual consumption)
      
      #yearly_recall_items <- c("301", "302", "303", "304", "305", "306", "307", "308", "309", 
      #                         "310", "311", "312", "313", "314", "315", "316", "317", "318", "319")
      
      # Either outer-join or an rbind must be used
      #*    zero-cost items are ignored for all these 
      ml <-merge(m,l,all=TRUE)
      #return(ml)
      mlk <-merge(ml,k,all=TRUE)
      return(mlk)
      #*    merging all the 4 categories results in the expenditure file
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
  #* ))
}


load_ohs_file <-function(dataset,year){
  
  #* (( 
  if (dataset == "lsms"){
    if (year == 2010){
      
      #* Read section c_cb file
      cbdat<-read.dta('../lsms/TZNPS2COMDTA/COMSEC_CB.dta',convert.factors = FALSE)
      
      cb <- get_translated_frame(dat=cbdat,
                                 names=ohs_seccb_columns_lsms_2010(),
                                 m=ohs_seccb_mapping_lsms_2010())
      #* chose facilitycode l and collected accessibility 1 and 2(<10) (in the centre or less than 10 km away)
      l<-(cb[is.element(tolower(as.character(cb$facilitycode)),c("l")),])
      #* extract those with 1
      l$accessiblemarket<-as.integer(l$accessibility==1)
      #* extract those with 2 (and assign them the same status as 1's)
      l$accessiblemarket<-l$accessiblemarket+as.integer(l$accessibility==2 & l$distance<10)
      l=l[!is.na(l$accessiblemarket),]
      #* chose accessible market value using (if both in the centre and closer then ambiguous)
      l=ddply(l,.(region,district,ward),summarize,accessiblemarket=max(accessiblemarket))
      #l = data.frame(region=l$region,district=l$district,ward=l$ward,ea=l$ea,accessiblemarket=l$accessiblemarket)
      ##
      #* Also considered urban/rural based on population density 
      u <-read.csv('../lsms/district_code.csv')
      u = data.frame(region=u$region,district=u$district,isurbanp=u$is_urban);
      
      adat<-read_tnz('../lsms/TZNPS2HH1DTA/HH_SEC_A.dta',FALSE)
      
      a <- get_translated_frame(dat=adat,
                                names=ohs_seca_columns_lsms_2010(),
                                m=ohs_seca_mapping_lsms_2010())
      a<-merge(a,u)
      a<-merge(a,l)
      a$expensiveregion<-as.integer(is.element(a$region,get_expensiveregion_codes()))
      popDensity <- get_region_popdensity_map()
      a<-merge(a,popDensity)
      
      #*    Read section B
      bdat<-read_tnz('../lsms/TZNPS2HH1DTA/HH_SEC_B.dta',FALSE)
      b <- get_translated_frame(dat=bdat,
                                names=ohs_info_columns_lsms_2010(),
                                m=ohs_mapping_lsms_2010())
      
      
      b$hhid<-as.character(b$hhid)
      #* inferring occupation rank with occupation_mapping
      b<-merge(b,occupation_mapping())
      
      cdat<-read_tnz('../lsms/TZNPS2HH1DTA/HH_SEC_C.dta',FALSE)
      #*    Read section C
      c <- get_translated_frame(dat=cdat,
                                names=get_ohs_secc_columns_lsms_2010(),
                                m=get_ohs_secc_fields_mapping_lsms_2010())
      c$hhid<-as.character(c$hhid)
      ab <- merge(a,b)
      ohs<-merge(ab,c)
      ohs$age <-2010-ohs$YOB
      #*    calculated age by subtracting YOB from 2010 (survey year)
      #*    read section J for housing data (rent, number of primary/secondary rooms)
      
      jdat <- read.dta('../lsms/TZNPS2HH1DTA/HH_SEC_J1.dta',convert.factors=FALSE)
      j <- get_translated_frame(dat=jdat,
                                names=get_lsms_secj_info_columns_2010(),
                                m=get_lsms_secj_fields_mapping_2010())
      j$hhid <-as.character(j$hhid)
      j$roomsnum_secondary[is.na(j$roomsnum_secondary)]<-0
      j$houserent[is.na(j$houserent)]<-0
      j$roomsnum<-j$roomsnum_primary+j$roomsnum_secondary
      ohsj<-merge(ohs,j,all=TRUE)
      return(ohsj)
      
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
  #* ))
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
    #* ((
    #* read section E
    idat1 <-read_tnz('../lsms/./TZNPS2HH1DTA/HH_SEC_E1.dta',FALSE)
    idat2 <-read_tnz('../lsms/./TZNPS2HH1DTA/HH_SEC_E2.dta',FALSE)
    i1 <- get_translated_frame(dat=idat1,
                               names=get_lsms_sece1_columns_2010(),
                               m=get_lsms_sece_fields_mapping_2010())
    #TODO: add the conversion into get_translated_frame functionality
    i1$hhid<-as.character(i1$hhid)
    i2 <- get_translated_frame(dat=idat1,
                               names=get_lsms_sece2_columns_2010(),
                               m=get_lsms_sece_fields_mapping_2010())
    i2$hhid<as.character(i2$hhid)
    #TODO: add the conversion into get_translated_frame functionality
    
    ti <- infer_lsms_sece_total_income(i1,i2);
    #* inferred section e data
    #* ))
    # idat2 has only got self-employment details
    
    return(ti)
  }
  
  stop(paste("Unknown dataset: ",dataset))
}

get_ucc_mapping_2004<-function(){
  mfile<-read.csv('2004/ucc_mapping.csv')
  return(mfile)
}

filter_categories_data<-function(hh,selected_category,item_field,set_depvar){
  
  vis<-hh[is.element(hh[,item_field],selected_category),]
  if (dim(vis)[1] <=0){
    if (set_depvar){
      stop(paste("No entries found in category: ",toString(selected_category)))
    }
  }
  vis<-ddply(vis,.(hhid),summarize,visible_consumption=sum(cost))
  no_vis_hhid<-setdiff(unique(hh$hhid),unique(vis$hhid))
  
  # set all values to zero for the hhids where the data isn't found in the
  # selected_category
  
  no_vis<-data.frame(hhid=no_vis_hhid,visible_consumption=rep(0,length(no_vis_hhid)))
  vis <- rbind(vis,no_vis)
  return(vis)
}


merge_hh_ohs_income_data_lsms_2010<-function(hh,ohs,income,selected_category,set_depvar){
  if (!is.integer(ohs$household_status)|| !(is.integer(ohs$highest_educ))){
    stop("factors must be converted an integer")
  }
  #* merge_hh_ohs_income_data_lsms_2010 ((
  print ("Calculating visible expenditures")
  print(paste("Total number of households to search for visible consumption=",length(unique(hh$hhid))))
  vis<-filter_categories_data(hh=hh,selected_category = selected_category, item_field = "item", set_depvar=set_depvar)
  print(paste("Number of households with visible expenditure = ",length(unique(vis$hhid))))
  print("Calculating total expenditures")
  totexp<-ddply(hh,.(hhid),summarize,total_expenditure=sum(cost))
  print(paste("Number of households with total expenditure data minus housing = ",length(unique(totexp$hhid))))
  
  # obtain map (hhid->housing) with ddply
  tothouserent<-ddply(ohs,.(hhid),summarize,tothouserent=sum(houserent))
  # obtain map (hhid->educexpen) with ddply 
  print(paste("Number of households with houserent data = ",length(unique(tothouserent$hhid))))
  
  print ("Appending educexpense and houserent to total expenditure");
  #* setting houses with education exenses= NA as zero
  ohs$educexpense[is.na(ohs$educexpense)]<-0
  toteducexpense<-ddply(ohs,.(hhid),summarize,toteducexpense=sum(educexpense))
  print(paste("Number of households with educexpense data = ",length(unique(toteducexpense$hhid))))
  
  totexp<-merge(totexp,toteducexpense)
  print(paste("Number of households after merging total_expenditure and total_educexpense = ",length(unique(totexp$hhid))))
  
  totexp<-merge(totexp,tothouserent)
  print(paste("Number of households after merging total_expenditure with houserent = ",length(unique(totexp$hhid))))
  
  totexp$total_expenditure=totexp$total_expenditure+totexp$tothouserent+totexp$toteducexpense
  #totexp$tothouserent<-NULL
  #totexp$toteducexpense<-NULL
  
  #* calculating educational expense and total houserent
  print(paste("Number of households with total expenditure data = ",length(unique(totexp$hhid))))
  #* finding personids of the house-heads and their education-level, age, years in community, 
  #* language, occupation and 
  #* other household characteristics
  
  heads<-ohs[as.integer(ohs$household_status)==1,]
  print(paste("Number of houesehold heads = ",length(unique(heads$hhid))))
  
  heads<-data.frame(hhid=heads$hhid,
                    highest_educ=heads$highest_educ,
                    age=heads$age,
                    region=heads$region,
                    expensiveregion=heads$expensiveregion,
                    popdensity =heads$popdensity,
                    district=heads$district,
                    ward=heads$ward,
                    ea=heads$ea,
                    personid=heads$personid,
                    litlang=heads$litlang,
                    isrural=heads$isrural,
                    isurbanp=heads$isurbanp,
                    accessiblemarket=heads$accessiblemarket,
                    schoolowner=heads$schoolowner,
                    occupation = heads$occupation,
                    occupation_rank = heads$occupation_rank,
                    years_community=heads$years_community,
                    housingstatus=heads$housingstatus,
                    roomsnum=heads$roomsnum,
                    stringsAsFactors=FALSE);
  print(
    paste("Total number of households with head_highest_education=NA : ",
          dim(heads[is.na(heads$highest_educ),])[1]
    )
  );
  #heads$highest_educ[is.na(heads$highest_educ)]<-1
  print("Setting members with years_community=99 as their age");
  heads$is_resident<-as.integer(as.integer(heads$years_community)==99)
  heads$years_community<-heads$years_community+heads$is_resident*(heads$age-99);
  
  print ("Calculating hsize")
  hhid_personid<-data.frame(hhid=ohs$hhid,personid=ohs$personid,stringsAsFactors=FALSE);
  hhid_personid<- ddply(hhid_personid,.(hhid),summarize,hsize=length(personid));
  print(paste("Number of households with hsize data = ",length(unique(hhid_personid$hhid))))
  
  print("Merging visual expenditure")
  ds <-merge(totexp,vis);
  print(paste("Number of households after merging total expenditure with visible expenditure= ",length(unique(ds$hhid))))
  print(paste("Merging hsize",dim(ds)[1]))
  
  ds <-merge(ds,hhid_personid);
  print(paste("Number of households after merging resultant with hsize data= ",length(unique(ds$hhid))))
  
  ds<-merge(ds,heads)
  print(paste("Number of households after merging resultant with household head data = ",length(unique(ds$hhid))))
  
  #print(paste("Merging income",dim(ds)[1]))
  #ds<-merge(ds,income)
  print(paste("personid range:",toString(unique(ds$personid))))
  ds$personid<-NULL
  return(ds)
  #* ))
}


merge_hh_ohs_income_data_us_cex_2004<-function(hh,ohs,income,selected_category,set_depvar){
  
  # hh's ucc should be merged first
  print("Reading ucc mapping file")
  ucc_mapping<-get_ucc_mapping_2004()
  print (paste("Merging with ucc_mapping (columns: ",toString(colnames(ucc_mapping)),")"))
  hh<-merge(hh,ucc_mapping)
  print ("Obtaining visible categories")
  vis<-filter_categories_data(hh=hh,selected_category = selected_category,item_field = "uccname",set_depvar)
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
merge_hh_ohs_income_data<-function(dataset,year,hh,ohs,income,selected_category,set_depvar){
  #* merge_hh_ohs_income_data((
  if (dataset == "us_cex"){
    if (year == 2004 || year == 2009|| year == 2014){
      ds <-merge_hh_ohs_income_data_us_cex_2004(hh=hh,ohs=ohs,income=income,selected_category=selected_category,set_depvar=set_depvar)
      ds$year <- rep(year,dim(ds)[1])
      return(ds)
    }
    
    stop(paste("Method to merge data for year:",year," not found"))
  }
  if (dataset == "lsms"){
    ds <-merge_hh_ohs_income_data_lsms_2010(hh=hh,ohs=ohs,income=income,selected_category=selected_category,set_depvar=set_depvar)
    return(ds)
  }
  stop(paste("Method to merge data for dataset:",dataset," not found"))
  #*))
}


cpi_adjust<-function(ds,mfile,refyear){
  m=read.csv(mfile)
  m<-data.frame(year=as.integer(m$Year),cpi=m$Jun,stringsAsFactors=FALSE)
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
