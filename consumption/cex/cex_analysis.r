
# duplicates in diary hhdata are not uncommon
library(foreign)
require(plyr)
require(AER)
source('../panelfunc.R')
source('../regressions.R')

setwd('c:/local_files/research/consumption/cex/');


# Tasks
# 1. Incorporate high-price regions (dummy) and high-population-density (dummy) and re-run simple2 as well as 2sls again


lsms_vars_init<-function(){
  return(c("total_expenditure","age","hsize","housingstatus","occupation_rank","isrural","region",
           "english","roomsnum","years_community","is_resident"))
}

lsms_ln_vars_init<-function(){
  return(c("lnpinc","age","hsize","housingstatus","occupation_rank","isrural","highest_educ","region",
           "english","roomsnum","years_community","is_resident"))
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
  
  instruments_list<-instrument_table[[item]]
  if (is.null(instruments_list)){
    stop(paste("No instrument found for item:",toString(item)))
  }
  return (instruments_list)
  
}


runTest<-function(outfile){
  items<-c('carpetsrugs', 'dseducexpense', 'dselectricity', 'dsfood',
           'dshouserent', 'dspersonalitemsrepair', 'dspersonalprods', 
           'dsskincream', 'funeralcosts', 'marriagecosts', 'sportshobbyequipment')
  results<-""
  for (item in items){
    res<-item_analysis(item,"simple2")
    resdf<-as.data.frame(summary(res)$coefficients)
    results<-paste(results,"\n",item,":",toString(rownames(resdf)))
  }
  write(results,outfile)
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
  
  code <- item_codes[[item]]
  if (is.null(code)){
    stop(paste("Could not find itemcode for",item))
  }
  return(code)
}
item_analysis<-function(itemname,regtype,commodity_type,ds){
  
  if (missing(ds)){
    
    if (is.element(itemname, c("dseducexpense","dshouserent"))){
      # vis is not needed for these categories
      ds<-combined_data_set(dataset = "lsms",year = 2010,selected_category = NULL ,isTranslated = TRUE, set_depvar=FALSE)
      
    } else {
      diaryCode = get_item_diary_code(itemname)
      ds<-combined_data_set(dataset = "lsms",year = 2010,selected_category = diaryCode ,isTranslated = TRUE)
    }
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
    varsInfo[["instrument_list"]]=get_instruments_for_item(itemname);
    varsInfo [["vars_list"]]=lsms_ln_vars_init();
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
    
    varsInfo [["vars_list"]]=lsms_ln_vars_init();
    varsInfo[["endogenous_vars"]] = "lnpinc"
    res<-run_regression_lsms(ds,"simple2",commodity_type,varsInfo)
    return(res)
  }
  
  stop(paste("analysis of type",regtype, "not supported"))
  
}

#########################

read_tnz <- function(filename,convert_factors) {
  if (!is.logical(convert_factors) || !is.atomic(convert_factors)){
    stop("convert_factords must be ")
  }
  dat1 = read.dta(filename,convert.factors = convert_factors);
  dat2 = as.data.frame(dat1,stringsAsFactors=FALSE);
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

get_lsms_secm_info_columns<-function(year){
  if (year == 2010){
    return(c("hhid","item","is_consumed","cost"))
  }
  stop(paste("Not secm info columns for year: ",year))
}

get_lsms_secm_fields_mapping<-function(year){
  if (year == 2010){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y2_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="hh_m01_2",name="is_consumed"))
    s= rbind(s,data.frame(iesname="itemcode",name="item"))
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
get_lsms_secj_info_columns_2010<-function(){
  return(c("hhid","housingstatus","houserent","roomsnum_primary","roomsnum_secondary"))
}

get_lsms_secj_fields_mapping_2010<-function(){
  s = data.frame(iesname=NULL,name=NULL)
  s= rbind(s,data.frame(iesname="y2_hhid",name="hhid"))
  s= rbind(s,data.frame(iesname="hh_j01",name="housingstatus")) # 1- owner occupied, 2- EMPLOYER PROVIDED - SUBSIDIZED, 3-EMPLOYER PROVIDED - FREE, 4- RENTED, 5- FREE, 6-NOMADS 
  s= rbind(s,data.frame(iesname="hh_j03",name="houserent"))
  s= rbind(s,data.frame(iesname="hh_j04_1",name="roomsnum_primary"))
  s= rbind(s,data.frame(iesname="hh_j04_2",name="roomsnum_secondary"))
  return(s)
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

ohs_seccb_columns_lsms_2010<-function(){
  return(c("facilitycode","accessibility","distance","region","district","ward"))
}


ohs_seccb_mapping_lsms_2010<-function(){
  s = data.frame(iesname=NULL,name=NULL)
  s= rbind(s,data.frame(iesname="id_01",name="region"))
  s= rbind(s,data.frame(iesname="id_02",name="district"))
  s= rbind(s,data.frame(iesname="id_03",name="ward"))
  s= rbind(s,data.frame(iesname="id_04",name="ea"))
  s= rbind(s,data.frame(iesname="cboa",name="facilitycode"))
  s= rbind(s,data.frame(iesname="cm_b01",name="accessibility"))
  s= rbind(s,data.frame(iesname="cm_b03",name="distance"))
  return(s)
}

ohs_seccj_columns_lsms_2010<-function(){
  return(c("item","lwp","lwp_unit","price","region","district","ward","ea"))
}

ohs_seccj_mapping_lsms_2010<-function(){
  s = data.frame(iesname=NULL,name=NULL)
  s= rbind(s,data.frame(iesname="id_01",name="region"))
  s= rbind(s,data.frame(iesname="id_02",name="district"))
  s= rbind(s,data.frame(iesname="id_03",name="ward"))
  s= rbind(s,data.frame(iesname="id_04",name="ea"))
  s= rbind(s,data.frame(iesname="itemid",name="item"))
  s= rbind(s,data.frame(iesname="cm_j01a",name="lwp_unit"))
  s= rbind(s,data.frame(iesname="cm_j01b",name="lwp"))
  s= rbind(s,data.frame(iesname="cm_j01c",name="price"))
  return(s)
}


ohs_seca_columns_lsms_2010<-function(){
  return(c("hhid","region","district","ward","ea","isrural"))
}

ohs_seca_mapping_lsms_2010<-function(){
  s = data.frame(iesname=NULL,name=NULL)
  s= rbind(s,data.frame(iesname="y2_hhid",name="hhid"))
  s= rbind(s,data.frame(iesname="region",name="region"))
  s= rbind(s,data.frame(iesname="district",name="district"))
  s= rbind(s,data.frame(iesname="ward",name="ward"))
  s= rbind(s,data.frame(iesname="ea",name="ea"))
  s= rbind(s,data.frame(iesname="y2_rural",name="isrural"))
  return(s)
}

area_code<-function(df,field_array)
{
  areacode=NULL
  for (field in field_array){
    if (class(df[,field])!="integer" && class(df[,field])!="numeric"){
      stop("field(",field,") is not integer")
    }
    a<-formatC(format="d",flag="0",x=df[,field],width=ceiling(log10(max(df[,field]))))
    areacode<-paste(areacode,a,sep="")
    
  }
  return(as.integer(areacode))
  #return(areacode)
}

analyse_cj<-function(sl){
  cjdat<-read.dta('../lsms/TZNPS2COMDTA/COMSEC_CJ.dta',convert.factors = FALSE) 
  
  cj <- get_translated_frame(dat=cjdat, names=ohs_seccj_columns_lsms_2010(), m=ohs_seccj_mapping_lsms_2010())
  cj$factor<-as.integer(cj$lwp_unit==1)+as.integer(cj$lwp_unit==2)/1000.0+as.integer(cj$lwp_unit==3)+as.integer(cj$lwp_unit==4)/1000.0+as.integer(cj$lwp_unit==5)
  cj$lwp <-cj$lwp*cj$factor
  cj$price <cj$price/cj$lwp
  
  if (missing(sl)){
    sl<-sort(unique(cj$item));
  }
  
  print (paste("sl=",sl))
  for (i in sl) {
    print(i);
    cjt<-cj[cj$item==i,]; 
    cjt<-cjt[!is.na(cjt$price) & cjt$price>0,];
    if(dim(cjt)[1]>0)
    {
      plot(cjt$r,cjt$price,xlab="region",ylab=paste("price for item=",i));
      View(cjt); 
      print(paste("Enter threshold for item=",i)); 
      m<-as.numeric(readline());
      if (m <= 0 || is.na(m)){
        stop ("Done")
      }
      print (paste("Using",m,"as threshold")); 
      x<-cjt[cjt$price<=max(cjt$price) & cjt$price > m,]; 
      print(paste(unique(x$item),unique(x$region),sep=","));
    } # end if
  } # end for
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

diary_info_columns_us_cex_2004<-function(){
  return(c("hhid","cost","ucc","alloc"));
}

diary_info_columns_lsms_2010<-function(){
  return(c("hhid","item","lwp_unit", "lwp", "cost", "own_unit", "own", "gift_unit", "gift"))
}

ohs_info_columns_us_cex_2004<-function(){
  return(c("hhid","age","gender","educ","race","hsize","income","horref1","urban_rural","popsize","highest_education"))
}

search_temp<-function (res,a){
  stepsize<-.01
  from <-a
  to <- a + stepsize
  for ( i in seq(100)){
    result <- res[res$mpay<=to & res$mpay>from,]
    nrows<-dim(result)[1]
    if (nrows>0){
      print(result)
      return(c(from,to))
    } else {
      from <-to
      to <- from+stepsize
    }
  }
}
occupation_mapping<-function(){
  
  occupation<-"AGRICULTURE/LIVESTOCK.......1
FISHING.............2
MINING..............3
TOURISM.............4
EMPLOYED:
  GOVERMENT...........5
PARASTATAL..........6
PRIVATE SECTOR......7
NGO/RELIGIOUS.......8
EMPLOYED(NOT
         AGRICULTURE):
  WITH EMPLOYEES......9
WITHOUT EMPLOYEES..10
UNPAID FAMILY
WORK...............11
PAID FAMILY WORK...12
JOB SEEKERS........13
STUDENT............14
DISABLED...........15
NO JOB.............16
TOO YOUNG .........17"
  
  res<-"
     occupation        mpay      sdpay    n
1           1  1187965.72  7055046.0 1141 (4)
4           5  2906734.21  4783352.2  148 (8)
6           7  1614594.39  5137868.2  385 (6)
8           9 11107091.89 32746264.8   37 (10)
9          10  4872580.65 13506098.7  309 (9)
10         11  1820317.04  6415290.0   90 (7)
11         12   456890.91  1220102.0   66 (3)
12         13   450215.87   899453.6   21 (2)
13         14    64989.34   214007.3   96 (1)
15         16  1217024.62  2625432.6   65 (5)
  "
  occupations<-"STUDENT,14
  JOB_SEEKERS,13
  PAID_FAMILY_WORK,12
  AGRICULTURE_LIVESTOCK,1
  UNEMPLOYED,16
  PRIVATE_SECTOR,7
  GOVERMENT,5
  NON_AGR_WO_EMPLOYEES,10
  NON_AGR_W_EMPLOYEES,9"
  #(0m(12,13,14,17),1m(1,7,8,11,16),...2m(5,15),..,4m(6,10),...,9m(2),..,11m(9),)
  # based on the following, rank (or class) does not have a strong predictive power
  #12,13,14,17 <-poor/unqualified
  #1,7,8,11,16 <- middle
  #5,15,6,10<- upper
  # Following mappings to try:
  
  occupations<-c(14,13,12,1,16,7,5,10,9)
  # the following doesn't work well for the mean pay
  # rank doesn't have a predictive power for total expenditure
  r=NULL;
  r=rbind(r,data.frame(occupation=14,occupation_rank=0))
  r=rbind(r,data.frame(occupation=13,occupation_rank=0))
  r=rbind(r,data.frame(occupation=12,occupation_rank=0))
  r=rbind(r,data.frame(occupation=16,occupation_rank=0))
  r=rbind(r,data.frame(occupation=11,occupation_rank=0))
  r=rbind(r,data.frame(occupation=1,occupation_rank=0))
  r=rbind(r,data.frame(occupation=17,occupation_rank=1))
  r=rbind(r,data.frame(occupation=2,occupation_rank=1))
  r=rbind(r,data.frame(occupation=3,occupation_rank=1))
  r=rbind(r,data.frame(occupation=4,occupation_rank=1))
  r=rbind(r,data.frame(occupation=7,occupation_rank=2))
  r=rbind(r,data.frame(occupation=9,occupation_rank=2))
  r=rbind(r,data.frame(occupation=10,occupation_rank=2))
  r=rbind(r,data.frame(occupation=15,occupation_rank=2))
  
  r=rbind(r,data.frame(occupation=8,occupation_rank=3))
  r=rbind(r,data.frame(occupation=5,occupation_rank=3))
  r=rbind(r,data.frame(occupation=6,occupation_rank=3))
  
  return(r)
  
  #2(fishing)~9
  #3(mining)~5
  #6(parastatal)~10
  #8(religious)~16
  #15(disabled)~5
  #17(too young)~14
  # based on median, we have 1,14 as lowest (<0.1) (student or farmer - poor)
  #                          11,12,13,14,16 <.2 (family work or student or jobseeker - poor) 
  
  #                          .11 < (11) < .12 (unpaid family work also with farmer - poor)
  #                          .15 < (16) < .16 (no job - poor)
  #                          .17 < (13) < .18 (job seeker - poor)
  #                          .28 < (2,4) < .29 ( tourism, fishing - worker)  
  #                                          (3) ( mining - worker) forced
  #                          .4< (7,9) < .5 (private company - worker)
  #                          .41 < (7) <.42 
  #                          .47 < (9) < .48 ( company - worker )
  #                          .59 <(10) < .6  ( business worker)
  #                          .71 < (15) < .72 (ommitted or worker) 
  #                          1.0 < (8) < 1.01 (religious - business worker)
  #                           1.59 < (5) < 1.6 (govt - business worker)
  #                           2.37 <(6) < 2.38 (parastatal - business worker)
}
ohs_info_columns_lsms_2010<-function(){
  # hhid, age, gender, educ, race, hsize, areatype, 
  # income file: income
  return(c("hhid", "gender", "personid","YOB", "household_status", "inhouse_consumer",
           "inhouse_days_in_month", "inhouse_resident", "outhouse_days_in_year", 
           "occupation", "fathers_educ", "mothers_educ", "married", "spouse_resident","years_community",
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
  #* get_ignored_hhids ((
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
    #* ignored 5 households with really high expenditure on marriage (more than reported annual income)
    ignoredhhids_adhoc<- c("0701006104006701","0702006012004001","0701021174002601","0702001125000103")
    #* ignored households with zero income (ensuring that not more than 2.5% number of households are ignored)
    ignoredhhids_zero_income <- unique(income[as.integer(income$yearly_pay)==0,]$hhid)
    ignored_threshold<-.025
    if( length(ignoredhhids_zero_income)/length(unique(income$hhid))>ignored_threshold){
      stop (paste("More than",ignored_threshold*100, "% hhids with zero income"))
    }
    print(paste("Ignored ",length(ignoredhhids_zero_income),"/",length(unique(income$hhid)),"(=",
                length(ignoredhhids_zero_income)/length(unique(income$hhid)),") households with zero income" ))
    ignored<-union(ignoredhhids_zero_income,ignoredhhids_adhoc)
    return(ignored)
  }
  
  stop(paste("No ignored hhids rationale for dataset:",dataset));
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
  s= rbind(s,data.frame(iesname="hh_b05",name="household_status"))
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
  
  s= rbind(s,data.frame(iesname="hh_b25",name="years_community"))
  
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


get_lsms_sece1_columns_2010<-function(){
  return(c("hhid", "personid", "is_ge5", "mainoccup", "is_wageworker", "employertype", "num_colleagues", 
           "lastpayment_unit", "lastpayment", "workweekhour","workyearmonths", "workyearmonthweeks",
           "workyearweekhours","has_lastpayment_other", "lastpayment_other_unit", 
           "lastpayment_other", "has_secjob", "employertype_secjob", "num_colleagues_secjob", "has_secjobwages",
           "workweekhour_secjob","workyearmonths_secjob", "workyearmonthweeks_secjob","workyearweekhours_secjob",
           "lastpayment_secjobwage_unit", "lastpayment_secjobwage", "has_secjobwages_other", "lastpayment_secjobwage_other_unit",
           "lastpayment_secjobwage_other", "has_selfemployment_week", "has_selfemployment_year", "selfemploymenttype",
           "selfemploymentstockvalue", "selfemploymentincome_unit", "selfemploymentincome","selfemploymentyearmonths",
           "selfemploymentyearmonthincome"))
}

get_lsms_sece2_columns_2010<-function(){
  return(c("hhid", "personid","selfemploymenttype",
           "selfemploymentstockvalue", "selfemploymentincome_unit", "selfemploymentincome","has_selfemployment_year",
           "selfemploymentyearmonths",
           "selfemploymentyearmonthincome"))
}

get_lsms_sece_fields_mapping_2010<-function(){
  s = data.frame(iesname=NULL,name=NULL)
  s= rbind(s,data.frame(iesname="y2_hhid",name="hhid"))
  s= rbind(s,data.frame(iesname="indidy2",name="personid"))
  s= rbind(s,data.frame(iesname="hh_e01",name="is_ge5"))
  s= rbind(s,data.frame(iesname="hh_e06",name="mainoccup"))
  s= rbind(s,data.frame(iesname="hh_e13",name="is_wageworker"))
  s= rbind(s,data.frame(iesname="hh_e15",name="employertype"))
  s= rbind(s,data.frame(iesname="hh_e18",name="num_colleagues"))
  s= rbind(s,data.frame(iesname="hh_e22_2",name="lastpayment_unit"))
  s= rbind(s,data.frame(iesname="hh_e22_1",name="lastpayment"))
  s= rbind(s,data.frame(iesname="hh_e23",name="has_lastpayment_other"))
  s= rbind(s,data.frame(iesname="hh_e24_2",name="lastpayment_other_unit"))
  s= rbind(s,data.frame(iesname="hh_e24_1",name="lastpayment_other"))
  s= rbind(s,data.frame(iesname="hh_e25",name="workweekhours"))
  s= rbind(s,data.frame(iesname="hh_e26",name="workyearmonths"))
  s= rbind(s,data.frame(iesname="hh_e27",name="workyearmonthweeks"))
  s= rbind(s,data.frame(iesname="hh_e28",name="workyearweekhours"))
  s= rbind(s,data.frame(iesname="hh_e29",name="has_secjob"))
  s= rbind(s,data.frame(iesname="hh_e30",name="employertype_secjob"))
  s= rbind(s,data.frame(iesname="hh_e33",name="num_colleagues_secjob"))
  s= rbind(s,data.frame(iesname="hh_e35",name="has_secjobwages"))
  s= rbind(s,data.frame(iesname="hh_e37_2",name="lastpayment_secjobwage_unit"))
  s= rbind(s,data.frame(iesname="hh_e37_1",name="lastpayment_secjobwage"))
  s= rbind(s,data.frame(iesname="hh_e38",name="has_secjobwages_other"))
  s= rbind(s,data.frame(iesname="hh_e39_2",name="lastpayment_secjobwage_other_unit"))
  s= rbind(s,data.frame(iesname="hh_e39_1",name="lastpayment_secjobwage_other"))
  
  s= rbind(s,data.frame(iesname="hh_e40",name="workweekhours_secjob"))
  s= rbind(s,data.frame(iesname="hh_e41",name="workyearmonths_secjob"))
  s= rbind(s,data.frame(iesname="hh_e42",name="workyearmonthweeks_secjob"))
  s= rbind(s,data.frame(iesname="hh_e43",name="workyearweekhours_secjob"))
  
  s= rbind(s,data.frame(iesname="hh_e51",name="has_selfemployment_week"))
  s= rbind(s,data.frame(iesname="hh_e52",name="has_selfemployment_year"))
  s= rbind(s,data.frame(iesname="hh_e53_2",name="selfemploymenttype"))
  s= rbind(s,data.frame(iesname="hh_e61",name="selfemploymentstockvalue"))
  s= rbind(s,data.frame(iesname="hh_e65_1",name="selfemploymentincome_unit"))
  s= rbind(s,data.frame(iesname="hh_e65_2",name="selfemploymentincome"))
  
  s= rbind(s,data.frame(iesname="hh_e70",name="selfemploymentyearmonths"))
  s= rbind(s,data.frame(iesname="hh_e71",name="selfemploymentyearmonthincome"))
  return(s)
}

computeYearValues<-function(dat,
                            unit_field,
                            quantity_field,
                            workyearweekhours_field,
                            workyearmonthweeks_field,
                            workyearmonths_field,
                            output_field)
{
  #* computeYearValues ((
  ufr <- range(dat[!is.na(dat[,unit_field]),][,unit_field])
  if (ufr[1]<1 || ufr[2]>8){
    stop("unit_field range not supported")
  }
  if (length(grep(output_field,colnames(dat),fixed=TRUE))>0){
    stop("column yearly_pay already present in data-frame")
  }
  
  #* pay frequency can be given in hours, days, weeks, months, fortnights, months, quarter, half year or year
  # last_payment_unit HOUR(1) DAY(2)  WEEK(3) FORTNIGHT(4) MONTH(5) QUATOR(6) HALF YEAR(7) YEAR(8)
  #* if (pay is per hours) then we use number of hours worked per week and multiply it with number of 
  #* weeks worked per month and further multiply the product with number of months worked per year
  h<-dat[!is.na(dat[,unit_field]),]
  h<-h[h[,unit_field]==1 ,]
  total_hour_workers<- dim(h)[1]
  if (total_hour_workers>0){
    print(paste("Total number of hour-wage-workers:",total_hour_workers))
    h<-h[!is.na(h[,workyearweekhours_field]),]
    h<-h[!is.na(h[,workyearmonthweeks_field]),] 
    h<-h[!is.na(h[,workyearmonths_field]),]
    total_hour_workers_considered<- dim(h)[1]
    print(paste("Number of hour-wage-workers ignored because of incomplete data:",total_hour_workers-total_hour_workers_considered))
    factor_hour = h[,workyearweekhours_field] * h[,workyearmonthweeks_field]* h[,workyearmonths_field] 
    h[,output_field] <-factor_hour*h[,quantity_field]
  } else {
    h<-NULL
  }
  #if (pay is per day )
  # assuming a 10 hour work day
  #* if pay is per day, then we assume a 10 hour working day and obtain the effective number of days per 
  #* week (based on the number of hours worked per week) and then multiply the number of days per week with 
  #* the number of weeks worked per month in th year - multiplying this product further with the number
  #* of months worked in an year
  d<-dat[!is.na(dat[,unit_field]),]
  d<-d[d[,unit_field]==2 ,]
  total_day_workers<- dim(d)[1]
  if (total_day_workers>0){
    print(paste("Total number of day-wage-workers:",total_day_workers))
    d<-d[!is.na(d[,workyearweekhours_field]),]
    d<-d[!is.na(d[,workyearmonthweeks_field]),]
    d<-d[!is.na(d[,workyearmonths_field]),]
    
    total_day_workers_considered<- dim(d)[1]
    print(paste("Number of day-wage-workers ignored because of incomplete data:",total_day_workers-total_day_workers_considered))
    factor_day = (d[,workyearweekhours_field]/10)*d[,workyearmonthweeks_field]*d[,workyearmonths_field]
    d[,output_field] <-factor_day*d[,quantity_field]
  } else {
    d<-NULL
  }
  #if (pay is per week )
  w<-dat[!is.na(dat[,unit_field]),]
  w<-w[w[,unit_field]==3,]
  total_week_workers<- dim(w)[1]
  if(total_week_workers>0){
    #* if pay is per week, the number then we multiply weeks worked per month into number of 
    #* months worked per year
    print(paste("Total number of week-wage-workers:",total_week_workers))
    w<-w[!is.na(w[,workyearmonths_field]) & !is.na(w[,workyearmonthweeks_field]),]
    total_week_workers_considered<- dim(w)[1]
    print(paste("Number of week-wage-workers ignored because of incomplete data:",total_week_workers-total_week_workers_considered))
    factor_week = w[,workyearmonthweeks_field] * w[,workyearmonths_field]
    w[,output_field] <-factor_week*w[,quantity_field]
  } else {
    w<-NULL
  }
  #if (pay is per fortnight )
  f<-dat[!is.na(dat[,unit_field]),]
  f<-f[f[,unit_field]==4,]
  total_fortnight_workers<- dim(f)[1]
  if (total_fortnight_workers>0){
    #* if pay is in fortnights, then use 2* the number of months worked in an year to calculate
    #* the total pay over the year
    print(paste("Total number of fortnight-wage-workers:",total_fortnight_workers))
    f<-f[!is.na(f[,workyearmonths_field]),]
    total_fortnight_workers_considered<- dim(f)[1]
    print(paste("Number of fortnight-wage-workers ignored because of incomplete data:",total_fortnight_workers-total_fortnight_workers_considered))
    factor_fortnight = f[,workyearmonths_field]*2
    f[,output_field] <-factor_fortnight*f[,quantity_field]
  } else {
    f<-NULL
  }
  #if (pay is per month )
  m<-dat[!is.na(dat[,unit_field]),]
  m<-m[m[,unit_field]==5 ,]
  total_month_workers<- dim(m)[1]
  if(total_month_workers>0){
    #* if the pay is per month, then the multiplication factor is just the number of months worked
    #* per year
    print(paste("Total number of month-wage-workers:",total_month_workers))
    m<-m[!is.na(m[,workyearmonths_field]),]
    total_month_workers_considered<- dim(m)[1]
    print(paste("Number of month-wage-workers ignored because of incomplete data:",total_month_workers-total_month_workers_considered))
    factor_month = m[,workyearmonths_field]
    m[,output_field] <-factor_month*m[,quantity_field]
  }else{
    m<-NULL
  }
  #if (pay i quartor)
  #* if the pay is per quarter, then we infer the effective number of quarters from the number of 
  #* months worked per year (number_of_months/3) and multiply with the number of 
  #* months worked per year
  q<-dat[!is.na(dat[,unit_field]),]
  q<-q[q[,unit_field]==5,]
  total_quarter_workers<- dim(q)[1]
  if(total_quarter_workers>0){
    print(paste("Total number of quarter-wage-workers:",total_quarter_workers))
    q<-q[!is.na(m[,workyearmonths_field]),]
    total_quarter_workers_considered<- dim(q)[1]
    print(paste("Number of quarter-wage-workers ignored because of incomplete data:",total_quarter_workers-total_quarter_workers_considered))
    factor_quarter = q[,workyearmonths_field]/3
    q[,output_field] <-factor_quarter*q[,quantity_field]
  }else{
    q<-NULL
  }
  # if pay is year 
  # factor = 1
  y<-dat[!is.na(dat[,unit_field]),]
  y<-y[y[,unit_field]==6,]
  total_year_workers<-dim(y)[1]
  if(total_year_workers>0){
    print(paste("Total number of yearly-wage-workers:",total_year_workers))
    qy <-y[,quantity_field]
    y[,output_field] <-qy
  } else {
    y<-NULL
  }
  
  hd<-rbind(h,d,stringsAsFactors=FALSE)
  hdw<-rbind(hd,w,stringsAsFactors=FALSE)
  hdwf<-rbind(hdw,f,stringsAsFactors=FALSE)
  hdwfm<-rbind(hdwf,m,stringsAsFactors=FALSE)
  hdwfmq<-rbind(hdwfm,q,stringsAsFactors=FALSE)
  hdwfmqy<-rbind(hdwfmq,y,stringsAsFactors=FALSE)
  return(hdwfmqy)
  #* ))
}

computeLsmsSelfemployedValues<-function(dat,has_selfemployment_year_field,selfemploymentyearmonths_field,selfemploymentyearmonthincome_field)
{
  #* computeLsmsSelfemployedValues ((
  i11<-dat[!is.na(dat[,has_selfemployment_year_field]),]
  i1_selfemployed<-i11[as.integer(i11[,has_selfemployment_year_field])==1,]
  total_self_employed<-dim(i1_selfemployed)[1]
  print(paste("Number of self-employed-workers:",total_self_employed))
  i1_selfemployed<-i1_selfemployed[!is.na(i1_selfemployed[,selfemploymentyearmonths_field]),]
  i1_selfemployed<-i1_selfemployed[!is.na(i1_selfemployed[,selfemploymentyearmonthincome_field]),]
  total_self_employed_considered<-dim(i1_selfemployed)[1]
  print(paste("Number of self-employed-workers ignored because of incomplete data:",total_self_employed-total_self_employed_considered));
  x <-i1_selfemployed
  x$yearly_pay<-x[,selfemploymentyearmonths_field]*x[,selfemploymentyearmonthincome_field]
  #* used months in an year for computing total income from self-employment in an year
  #* ))
  return(x)
}

infer_lsms_sece_total_income<-function(i1,i2){
  #* ((
  #* Rejecting less than 5 year old members from income data
  ydata<-NULL
  i1 <- i1[!is.na(i1$is_ge5),]
  i1 <- i1[as.integer(i1$is_ge5)==1,]
  i1_w<-i1[!is.na(i1$is_wageworker),]
  i1_w <- i1_w[as.integer(i1_w$is_wageworker)==1,]
  #* Looking only at wage workers
  #* sum up wages into column yearly pay
  i1_w_y <- computeYearValues(dat=i1_w,
                              unit_field="lastpayment_unit",
                              quantity_field="lastpayment",
                              workyearweekhours_field="workyearweekhours",
                              workyearmonthweeks_field="workyearmonthweeks",
                              workyearmonths_field="workyearmonths",
                              output_field="yearly_pay");
  ydata<-rbind(ydata,
               data.frame(hhid=i1_w_y$hhid,personid=i1_w_y$personid,
                          yearly_pay=i1_w_y$yearly_pay,
                          employertype=i1_w_y$employertype,stringsAsFactors=FALSE),
               stringsAsFactors=FALSE)
  #other forms of payment
  #* sum up values in other forms of payment as well
  i1_w_other<- i1_w[!is.na(i1_w$has_lastpayment_other),]
  i1_w_other <- i1_w_other[as.integer(i1_w_other$has_lastpayment_other)==1 ,]
  
  i1_w_other_y <- computeYearValues(dat=i1_w_other,
                                    unit_field="lastpayment_other_unit",
                                    quantity_field="lastpayment_other",
                                    workyearweekhours_field="workyearweekhours",
                                    workyearmonthweeks_field="workyearmonthweeks",
                                    workyearmonths_field="workyearmonths",
                                    output_field="yearly_pay");
  
  ydata<-rbind(ydata,
               data.frame(hhid=i1_w_other_y$hhid,
                          personid=i1_w_other_y$personid,
                          yearly_pay=i1_w_other_y$yearly_pay,
                          employertype=i1_w_other_y$employertype,
                          stringsAsFactors=FALSE
               ),
               stringsAsFactors=FALSE)
  #secondary job wages
  
  #* sum up values in from secondary of payment (for wage-workers)
  i1_secjob<-i1[!is.na(i1$has_secjobwages),]
  i1_secjob<-i1_secjob[!is.na(i1_secjob$has_secjob),]
  i1_secjob <- i1_secjob[as.integer(i1_secjob$has_secjobwages)==1,]
  i1_secjob <-i1_secjob[as.integer(i1_secjob$has_secjob)==1,]
  
  i1_secjob_y <- computeYearValues(dat=i1_secjob,
                                   unit_field="lastpayment_secjobwage_unit",
                                   quantity_field="lastpayment_secjobwage",
                                   workyearweekhours_field="workyearweekhours_secjob",
                                   workyearmonthweeks_field="workyearmonthweeks_secjob",
                                   workyearmonths_field="workyearmonths_secjob",
                                   output_field="yearly_pay");
  
  # secondary job must have employertype invalidated (set to -1 in the current convention)
  
  #* Only primary job is used to identify the employer type of the individual 
  print (paste("Setting employertype as -1 (for ",dim(i1_secjob_y)[1],") wage-workers with secondary jobs"))
  ydata<-rbind(ydata,
               data.frame(hhid=i1_secjob_y$hhid,
                          personid=i1_secjob_y$personid,
                          yearly_pay=i1_secjob_y$yearly_pay,
                          employertype=rep(-1,dim(i1_secjob_y)[1]),
                          stringsAsFactors=FALSE
               ),
               stringsAsFactors=FALSE)
  #* collecting other wages from secondary job
  i1_secjob_other<-i1[!is.na(i1$has_secjobwages_other),]
  i1_secjob_other<-i1_secjob_other[!is.na(i1_secjob_other$has_secjob),]
  i1_secjob_other <-i1_secjob_other[as.integer(i1_secjob_other$has_secjob)==1,]
  i1_secjob_other <- i1_secjob_other[as.integer(i1_secjob_other$has_secjobwages_other)==1,]
  
  i1_secjob_other_y <- computeYearValues(dat=i1_secjob_other,
                                         unit_field="lastpayment_secjobwage_other_unit",
                                         quantity_field="lastpayment_secjobwage_other",
                                         workyearweekhours_field="workyearweekhours_secjob",
                                         workyearmonthweeks_field="workyearmonthweeks_secjob",
                                         workyearmonths_field="workyearmonths_secjob",
                                         output_field="yearly_pay");
  print(paste("Setting employertype=-1 for ",dim(i1_secjob_other_y)[1]," wage workers with other payments in their secondary jobs")) 
  ydata<-rbind(ydata,data.frame(hhid=i1_secjob_other_y$hhid,
                                personid=i1_secjob_other_y$personid,
                                yearly_pay=i1_secjob_other_y$yearly_pay,
                                employertype=rep(-1,dim(i1_secjob_other_y)[1]),
                                stringsAsFactors=FALSE
  ),
  stringsAsFactors=FALSE
  )
  #rbind for the yearly-pay data-frame
  selfemployment_offset<-1000
  if (max(ydata$employertype)>=selfemployment_offset){
    stop(paste("max(employertype)=",max(ydata$employertype)," in income data(ydata) is less than the selected offset (",selfemployment_offset,")"))
  }
  print (paste("Adding ",selfemployment_offset," to selfemployment_type code and setting those values as employertype"))
  #* collecting self-employment income
  i1_selfemployed_y<-computeLsmsSelfemployedValues(dat=i1,
                                                   has_selfemployment_year_field="has_selfemployment_year",
                                                   selfemploymentyearmonths_field="selfemploymentyearmonths",
                                                   selfemploymentyearmonthincome_field="selfemploymentyearmonthincome");
  a1=data.frame(hhid=i1_selfemployed_y$hhid,
                personid=i1_selfemployed_y$personid,
                yearly_pay=i1_selfemployed_y$yearly_pay,
                employertype=selfemployment_offset+i1_selfemployed_y$selfemploymenttype,
                stringsAsFactors=FALSE
  )
  
  #* calling computeLsmsSelfemployedValues
  
  #i1_selfemployed_y2<-computeLsmsSelfemployedValues(dat=i2,
  #                                                  has_selfemployment_year_field="has_selfemployment_year",
  #                                                  selfemploymentyearmonths_field="selfemploymentyearmonths",
  #                                                  selfemploymentyearmonthincome_field="selfemploymentyearmonthincome");
  #a2=data.frame(hhid=i1_selfemployed_y2$hhid,
  #              personid=i1_selfemployed_y2$personid,
  #              yearly_pay=i1_selfemployed_y2$yearly_pay,
  #              employertype=i1_selfemployed_y2$selfemploymenttype,
  #              stringsAsFactors=FALSE)
  
  #print("Running outer-join (all-merge) for data from files 1 and 2");
  #a=merge(a1,a2,all=TRUE)
  a=a1;
  
  ydata<-rbind(ydata,a,stringsAsFactors=FALSE)
  
  print ("PENDING CONTROL VARS: employment_type, self_owned_business_type")
  
  #* summing up yearly-income from all sources
  
  print ("Running ddply to sum up yearly-pay from all sources")
  ydata <-ddply(ydata,.(hhid,personid),total_income=sum(yearly_pay))
  return(ydata)
  
  #* ))
  
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

visible_categories_us_cex_2004<-function(){
  #return(c('miscpersonalcare', 'haircareproducts', 'nonelectrichairequipment', 'wigshairpieces',
  #         'oralhygieneproducts', 'shavingneeds', 'cosmetics', 'miscpersonalcare'))
  # personal care, clothing and apparel (including footwear),jewelry, cars
  #return(c("homerent"));
  #return(c('apples', 'bananas', 'oranges', 'freshfruits_other', 'fruites_citrus_non_orange', 'fruits_frozen'));
  return(c("jewelry"))
  stop("should NOT get here")
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

#PP.........1 ADULT......2
#PRIMARY  SECONDARY
#D1........11 F1........21
#D2........12 F2........22
#D3........13 F3........23
#D4........14 F4........24
#D5........15 'O' +COURSE .25
#D6........16 F5........31
#D7........17 F6........32
#D8........18 'A'+COURSE .33
#OSC.......19 DIPLOMA...34
#MS+COURSE.20
#UNIVERSITY
#U1........41 U2........42
#U3........43 U4........44
#U5&+......45

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

food_categories_lsms_2010<-function(){
  return(c("10101", "10102", "10103", "10104", "10105", "10106", "10107", "10108", "10109", "10110", 
           "10111", "10112", "10201", "10202", "10203", "10204", "10205", "10206", "10207", "10301", 
           "10302", "10303", "10401", "10501", "10502", "10503", "10504", "10601", "10602", "10603", 
           "10701", "10702", "10703", "10704", "10801", "10802", "10803", "10804", "10805", "10806", 
           "10807", "10808", "10809", "10810", "10901", "10902", "10903", "11001", "11002", "11003", 
           "11004", "11101", "11102", "11103", "11104", "11105", "11106", "11107", "11108"))
}

visible_categories<-function(dataset,year){
  if (dataset == "us_cex"){
    if (year == 2004 || year == 2009|| year == 2014){
      return(visible_categories_us_cex_2004())
    }
    stop(paste("visible categories list for year:",year," not found"))
  }
  if (dataset == "lsms"){
    if (year == 2010){
      return(visible_categories_lsms_2010())
    }
    stop(paste("visible categories list for year:",year," not found"))
    
  }
  stop(paste("visible categories list for dataset:",dataset," not found"))
}

visible_categories_lsms_2010<-function(){
  # is it worthwhile to select commodities only if they're 
  # used only by a large section? - probably not - while this is a way 
  # to check if we have congestion or not, a set of overall visibile basket
  # doesn't necessarily consist widely used commodities (consumer can get 
  # visibility from a multitude of commodities)
  #return(c("213"))# skin creams
  #return(c("214"))# shampoo etc
  #return(c("202","213","214","224","301","306","313","314"));
  #return(c("10101","10102")) # rice
  #return(c("213")) # glycerine, vaseline
  #return(c("218")) # bar soap (body and/or clothes) 
  #return(c("224")) # radio/watch personal items repairs
  #return(c("211")) # toothbrush
  #return(c("10801","10802","10803","10804","10805","10806","10807","10808","10809","10810")) # meat
  #return(c("10701","10702","10703","10704"))
  #return(c("101")) # cigarettes/tobacco
  #return(c("11106","11107","11108")) # alcohol
  #return(c("218")) # donations
  #return(c("202"))# electricity
  #return(c("301"))# carpets, rugs
  #return(c("306"))# sports, hobby
  #return(c("313")) # bride price
  #return(c("314")) # funeral costs
  return(food_categories_lsms_2010());
  #return(c("313","314"))
  #return(c("218"))
  #return(c("306"))
  # 213 - glycerine, vaseline skin creams
  # 217 - Phone internet
  # 219 - Motor vehicle service, repair, or parts
  # 214 - Other personal products (shampoo, razor blades, cosmetics, hair products, etc.)
  # 218 - bar soap
  # 202 - electricity
  # 301 - Carpets, rugs
  # 306 - sports/hobby equipment
  # 313 - Bride price
  # 314 - Funeral costs
  # 224 - repairs to personal items
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

cex_combined_years_ds<-function(years)
{
  if (!is.vector(years)){
    stop("years must be a vector")
  }
  resds <-NULL
  for (year in years){
    ds=combined_data_set("us_cex",year,FALSE)
    resds = rbind(resds,ds,stringsAsFactors=FALSE)
  }
  return(resds)
}

combined_data_set<-function(dataset,year,selected_category,isTranslated,isDebug, set_depvar){
  
  
  ############ PHASE 0 ########################
  if (missing(set_depvar)){
    set_depvar = TRUE 
  }
  if (missing(selected_category)){
    print("setting selected_category to the default value")
    selected_category= visible_categories(dataset=dataset,year=year)
  }
  
  #*  (( Loading diary file
  hhdat <- load_diary_file(dataset,year) # must provide total and visible expenditure (must be already translated)
  
  #* Loading the person/family data fie
  ohsdat <-load_ohs_file(dataset,year) # (using fmld) must provide age (age_ref), gender(sex_ref), 
  # highest_educ(educ_ref), ishead(no_earnr,earncomp - all reference person data),
  # race(ref_race),family size (fam_size),
  # area_type (popsize,bls_urbn)
  #* Loading income file
  incomedat <-load_income_file(dataset,year) # must provide total income (fincaftm)
  
  if (is.null(hhdat)){
    stop("Could not load diary hhdata")
  }
  
  print("Ensuring duplicates do NOT exist in the diary hhdata")
  hhdat = unique(hhdat)
  
  ############ PHASE 1 - Translation ########################
  # info_columns must contain all hhdata-fields referred to in merging/aggregation phase (one per file)
  # translated frame makes the data available in a universal dictionary (age, gender etc.)
  if (missing(isTranslated) || isTranslated==FALSE) {
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
    if (!is.null(incomedat)){
      n1<-length(unique(income$hhid))
      income<-income[!is.element(as.character(income$hhid),as.character(ignored_hhids)),]
      n2<-length(unique(income$hhid))
      print(paste("ignored",n1-n2,"/",n1," hhids"))
    }
  }
  dstruct<-merge_hh_ohs_income_data(dataset=dataset,hh=hh,ohs=ohs,income=income,year=year,selected_category=selected_category,set_depvar=set_depvar);
  return(dstruct);
  #* ))
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
