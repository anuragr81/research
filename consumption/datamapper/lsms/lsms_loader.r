library(foreign)
library(haven)
require(plyr)
require(tidyr)
require(jsonlite)

if (isClass("LSMSLoader")){
  print ("Warning !!! previous definition of LSMSLoader would be overwritten.")
}

## all exported functions are declared here
setClass("LSMSLoader", representation(combined_data_set="function",load_diary_file="function",
                                      analyse_cj="function",load_ohs_file="function",
                                      match_recorded_prices="function",get_inferred_prices="function",load_market_prices="function",
                                      aggregate_local_prices="function",add_localmedian_price_columns="function",
                                      food_expenditure_data="function",read_assets_file="function",
                                      group_expenditure="function",group_collect="function",get_asset_score="function",
                                      item_usage="function", item_ownership="function",
                                      split_households="function",
                                      asset_differences="function",get_regressed_market_prices="function"))


lsms_loader<-function(fu,ln,lgc) {
  
  read_tnz <- function(filename,convert_factors,hhidColName) {
    
    if (!is.logical(convert_factors) || !is.atomic(convert_factors)){
      stop("convert_factors must be ")
    }
    print(paste("Reading file:",filename))
    dat1 = read.dta(filename,convert.factors = convert_factors);
    dat2 = as.data.frame(dat1,stringsAsFactors=FALSE);
    if (missing(hhidColName)){
      hhidColName<-"y2_hhid";
      dat3 = dat2[as.numeric(dat2[,hhidColName])>0,] # only take data with hhid>0
    } else {
      dat3 = dat2[!is.na(dat2[,hhidColName]),] # only take data with hhid>0
      dat3[,hhidColName]<-as.character(dat3[,hhidColName])
    }
    
    return(dat3);
  }
  
  get_expensiveregion_codes<-function(){
    return (c(7,12,19,53)) 
  }
  
  load_diary_file <-function(dirprefix,year,fu,ln){
    if (year == 2012){
      jdat <- read_tnz(filename = paste(dirprefix,"./lsms/tnz2012/TZA_2012_LSMS_v01_M_STATA_English_labels/HH_SEC_J1.dta",sep=""),
                       convert_factors = FALSE,hhidColName = "y3_hhid")
      k <- fu()@get_translated_frame(dat=jdat,
                                     names=ln()@diary_info_columns_lsms_2012(),
                                     m=ln()@hh_mapping_lsms_2012())
      k <- k[as.numeric(k$cost)>0 & !is.na(k$cost),]
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
      kdat <- read_tnz(filename = paste(dirprefix,"./lsms/tnz2012/TZA_2012_LSMS_v01_M_STATA_English_labels/HH_SEC_K.dta",sep=""),
                       convert_factors = FALSE,hhidColName = "y3_hhid")
      
      l <- fu()@get_translated_frame(dat=kdat,
                                     names=ln()@get_lsms_seck_info_columns_2012(),
                                     m=ln()@get_lsms_seck_fields_mapping_2012());
      
      l$hhid <-as.character(l$hhid)
      l <- l[!is.na(l$cost) & l$cost>0 & !is.na(l$hhid),]
      weekly_recall_items <-c(101,102,103)
      
      # l is weekly and  monthly data
      
      l <- ln()@multiplyLsmsQuantities(dat = l , 
                                       quantity_field_name="cost", 
                                       item_field_name="item", 
                                       factor=52,
                                       items_list = weekly_recall_items)
      
      
      monthly_recall_items_non_repair <- c("201", "202", "203", "204", "205", "206", "207", "208", "209",
                                           "210", "211", "212", "213", "214", "215", "216", "217", "218", "221", "222")
      
      
      l <- ln()@multiplyLsmsQuantities(dat = l , 
                                       quantity_field_name="cost", 
                                       item_field_name="item", 
                                       factor=12,
                                       items_list = monthly_recall_items_non_repair)
      
      monthly_recall_items_repair     <- c("219", "220", "223")
      l <- ln()@multiplyLsmsQuantities(dat = l , 
                                       quantity_field_name="cost", 
                                       item_field_name="item", 
                                       factor=6,
                                       items_list = monthly_recall_items_repair)
      
      
      # m is yearly data
      ldat <-read_tnz( filename = paste(dirprefix,'./lsms/tnz2012/TZA_2012_LSMS_v01_M_STATA_English_labels/HH_SEC_L.dta',sep=""),
                       convert_factors = FALSE,
                       hhidColName = "y3_hhid")
      m <- fu()@get_translated_frame(dat=ldat,
                                     names=ln()@get_lsms_secm_info_columns(2012),
                                     m=ln()@get_lsms_secm_fields_mapping(2012))
      m$hhid <-as.character(m$hhid)
      m<- m[!is.na(m$hhid) & !is.na(m$cost) & m$cost>0,]
      # nothing to be multiplied for yearly-recall (since we're looking at annual consumption)
      #*    zero-cost items are ignored for all these 
      ml <-merge(m,l,all=TRUE)
      #*    merging all the 4 categories results in the expenditure file
      
      mlk <-merge(ml,k,all=TRUE)
      mlk <-merge(mlk,rename(ln()@items_codes_2012()[,c("shortname","code")],c("code"="item")),by=c("item"),all.x=TRUE)
      if (dim(subset(mlk,is.na(shortname)))[1] > 0) { stop ("itemcodes are not known for some entries in the diary file")}
      
      extremeDataHhids <- unique ( dplyr::filter( merge(mlk,ddply(mlk,.(shortname),summarise,v=fu()@fv(cost)),all.x=TRUE) , cost > v)$hhid )
      print (paste("Households with extreme data (many times the median) - purged from the diary file:",length(extremeDataHhids)))
      mlk              <- dplyr::filter(mlk,!is.element(hhid,extremeDataHhids))
      return(mlk)
    }
    if (year == 2010){
      # combine sections ( k , l, m )
      kdat <- read_tnz(paste(dirprefix,"./lsms/TZNPS2HH3DTA/HH_SEC_K1.dta",sep=""),FALSE)
      #*    Reading weekly Diary data in Section K data and retrieving item as well as the quantity as well as cost 
      
      k <- fu()@get_translated_frame(dat=kdat,
                                     names=ln()@diary_info_columns_lsms_2010(),
                                     m=ln()@hh_mapping_lsms_2010())
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
      
      ldat <- read_tnz(paste(dirprefix,"./lsms/TZNPS2HH2DTA/HH_SEC_L.dta",sep=""),FALSE)
      l <- fu()@get_translated_frame(dat=ldat,
                                     names=ln()@get_lsms_secl_info_columns_2010(),
                                     m=ln()@get_lsms_secl_fields_mapping_2010())
      l$hhid <-as.character(l$hhid)
      l <- l[!is.na(l$cost) & l$cost>0 & !is.na(l$hhid),]
      weekly_recall_items <-c(101,102,103)
      
      # l is weekly and  monthly data
      
      l <- ln()@multiplyLsmsQuantities(dat = l , 
                                       quantity_field_name="cost", 
                                       item_field_name="item", 
                                       factor=52,
                                       items_list = weekly_recall_items)
      #*    Monthly recall items are multiplied by 12
      
      monthly_recall_items_non_repair <- c("201", "202", "203", "204", "205", "206", "207", "208", "209",
                                           "210", "211", "212", "213", "214", "215", "216", "217", "218", "221", "222")  
      
      l <- ln()@multiplyLsmsQuantities(dat = l , 
                                       quantity_field_name="cost", 
                                       item_field_name="item", 
                                       factor=12,
                                       items_list = monthly_recall_items_non_repair)
      
      monthly_recall_items_repair     <- c("219", "220", "223", "224")
      
      l <- ln()@multiplyLsmsQuantities(dat = l , 
                                       quantity_field_name="cost", 
                                       item_field_name="item", 
                                       factor=6,
                                       items_list = monthly_recall_items_repair)
      
      
      # m is yearly data
      mdat <-read_tnz( paste(dirprefix,'./lsms/TZNPS2HH2DTA/HH_SEC_M.dta',sep=""),FALSE)
      m <- fu()@get_translated_frame(dat=mdat,
                                     names=ln()@get_lsms_secm_info_columns(2010),
                                     m=ln()@get_lsms_secm_fields_mapping(2010))
      m$hhid <-as.character(m$hhid)
      m<- m[!is.na(m$hhid) & !is.na(m$cost) & m$cost>0,]
      # nothing to be multiplied for yearly-recall (since we're looking at annual consumption)
      
      #yearly_recall_items <- c("301", "302", "303", "304", "305", "306", "307", "308", "309", 
      #                         "310", "311", "312", "313", "314", "315", "316", "317", "318", "319")
      
      # Either outer-join or an rbind must be used
      #*    zero-cost items are ignored for all these 
      ml <-merge(m,l,all=TRUE)
      
      mlk <-merge(ml,k,all=TRUE)
      ## mapping name codes
      mlk <-merge(mlk,rename(ln()@items_codes_2010()[,c("shortname","code")],c("code"="item")),by=c("item"),all.x=TRUE)
      if (dim(subset(mlk,is.na(shortname)))[1] > 0) { stop ("itemcodes are not known for some entries in the diary file")}
      
      # filtering out extreme values
      extremeDataHhids <- unique ( dplyr::filter( merge(mlk,ddply(mlk,.(shortname),summarise,v=fu()@fv(cost) ),all.x=TRUE) , cost > v)$hhid ) 
      print (paste("Households with extreme data (with many times the median) - purged from the diary file:",length(extremeDataHhids)))
      mlk              <- dplyr::filter(mlk,!is.element(hhid,extremeDataHhids))
      
      return(mlk)
      #*    merging all the 4 categories results in the expenditure file
    }
    if (year == 2008) {
      # combine sections ( k , l, m )
      kdat <- read_tnz(paste(dirprefix,'./lsms/tnz2008/TZNPS1HHDTA_E/SEC_K1.dta',sep=""),FALSE,hhidColName="hhid")
      
      #*    Reading weekly Diary data in Section K data and retrieving item as well as the quantity as well as cost 
      
      k <- fu()@get_translated_frame(dat=kdat,
                                     names=ln()@diary_info_columns_lsms_2008(),
                                     m=ln()@hh_mapping_lsms_2008())
      k$hhid <-as.character(k$hhid)
      
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
      
      ldat <- read_tnz(paste(dirprefix,"/lsms/tnz2008/TZNPS1HHDTA_E/SEC_L.dta",sep=""),FALSE,hhidColName = "hhid")
      l <- fu()@get_translated_frame(dat=ldat,
                                     names=ln()@get_lsms_secl_info_columns_2008(),
                                     m=ln()@get_lsms_secl_fields_mapping_2008())
      l$hhid <-as.character(l$hhid)
      l <- l[!is.na(l$cost) & l$cost>0 & !is.na(l$hhid),]
      weekly_recall_items <-c(101,102,103)
      
      # l is weekly and  monthly data
      
      l <- ln()@multiplyLsmsQuantities(dat = l , 
                                       quantity_field_name="cost", 
                                       item_field_name="item", 
                                       factor=52,
                                       items_list = weekly_recall_items)
      #*    Monthly recall items are multiplied by 12
      
      monthly_recall_items_non_repair <- c("201", "202", "203", "204", "205", "206", "207", "208", "209",
                                           "210", "211", "212", "213", "214", "215", "216", "217", "218", "221", "222")  
      
      l <- ln()@multiplyLsmsQuantities(dat = l , 
                                       quantity_field_name="cost", 
                                       item_field_name="item", 
                                       factor=12,
                                       items_list = monthly_recall_items_non_repair)
      
      monthly_recall_items_repair     <- c("219", "220", "223", "224")
      
      l <- ln()@multiplyLsmsQuantities(dat = l , 
                                       quantity_field_name="cost", 
                                       item_field_name="item", 
                                       factor=6,
                                       items_list = monthly_recall_items_repair)
      
      
      # m is yearly data
      mdat <-read_tnz( paste(dirprefix,'./lsms/tnz2008/TZNPS1HHDTA_E/SEC_M.dta',sep=""),FALSE,hhidColName = "hhid")
      m <- fu()@get_translated_frame(dat=mdat,
                                     names=ln()@get_lsms_secm_info_columns(2008),
                                     m=ln()@get_lsms_secm_fields_mapping(2008))
      m$hhid <-as.character(m$hhid)
      m<- m[!is.na(m$hhid) & !is.na(m$cost) & m$cost>0,]
      
      
      #########
      ml <-merge(m,l,all=TRUE)
      
      mlk <-merge(ml,k,all=TRUE)
      #print("RETURNING PREMATURELY")
      #return(mlk)
      
      ## mapping name codes
      mlk <-merge(mlk,plyr::rename(ln()@items_codes_2010()[,c("shortname","code")],c("code"="item")),by=c("item"),all.x=TRUE)
      if (dim(subset(mlk,is.na(shortname)))[1] > 0) { stop ("itemcodes are not known for some entries in the diary file")}
      
      # filtering out extreme values
      extremeDataHhids <- unique ( dplyr::filter( merge(mlk,ddply(mlk,.(shortname),summarise,v=fu()@fv(cost) ),all.x=TRUE) , cost > v)$hhid ) 
      print (paste("Households with extreme data (with many times the median) - purged from the diary file:",length(extremeDataHhids)))
      mlk              <- dplyr::filter(mlk,!is.element(hhid,extremeDataHhids))
      
      return(mlk)
      
      
    }
    stop(paste("Year:",year, " not supported"))
  }
  
  convert_cj_item <- function(x){ 
    
    if (regexpr('^L',x)[[1]]==1)
      
    { return(as.integer(substring(x,2))) } 
    
    else { return (10000+as.integer(x)) }
    
  }
  
  
  match_recorded_prices <-function(year,dirprefix,fu,ln,marketPricesOnly){
    # loading ohs data
    ohs<-load_ohs_file(year=year, dirprefix = dirprefix,fu = fu,ln = ln)
    print ("Loaded OHS file")
    # loading diary data
    dat2010<-load_diary_file(dirprefix = '.',year = year, fu=fu, ln=ln )
    
    dat2010$factor<-as.integer(dat2010$lwp_unit==1)+as.integer(dat2010$lwp_unit==2)/1000.0+as.integer(dat2010$lwp_unit==3)+as.integer(dat2010$lwp_unit==4)/1000.0+as.integer(dat2010$lwp_unit==5) 
    dat2010$quantity<-dat2010$factor*dat2010$lwp
    dat2010$price<-dat2010$cost/dat2010$quantity
    print ("Loaded Diary file")
    ## preparing market data
    cjdat<-read.dta(paste(dirprefix,'/./lsms/TZNPS2COMDTA/COMSEC_CJ.dta',sep=''),convert.factors = FALSE) 
    
    cj <- fu()@get_translated_frame(dat=cjdat, names=ln()@ohs_seccj_columns_lsms_2010(), m=ln()@ohs_seccj_mapping_lsms_2010()) # recorded market prices
    
    cj$factor<-as.integer(cj$lwp_unit==1)+as.integer(cj$lwp_unit==2)/1000.0+as.integer(cj$lwp_unit==3)+as.integer(cj$lwp_unit==4)/1000.0+as.integer(cj$lwp_unit==5) # command label list (from labutil package) tells us the numeric values of factors
    
    cj<-subset(cj,!is.na(price) & !is.na(lwp) & lwp!=0 &price!=0)
    
    cj$recorded_quantity<-cj$lwp*cj$factor
    
    cj$recorded_price <-cj$price/cj$recorded_quantity
    
    k<-cj[,c("region","district","ward","ea","item","recorded_quantity","recorded_price")]
    k$item<-as.numeric(lapply(X = k$item,FUN = convert_cj_item))
    
    k<-k[!is.na(k$recorded_price),]
    
    if (marketPricesOnly){
      return(k)
    }
    print ("Loaded market prices file")
    
    hhidsRegion<-unique(ohs[,c("hhid","region","district","ward","ea")]) # unique ignores person id
    
    hhidsRegion<-subset(hhidsRegion,!is.na(hhid) & !is.na(region))# too many NAs in hhidsRegion
    
    householdLocation<-merge(hhidsRegion[c("hhid","region","district","ward","ea")], dat2010[,c("hhid","item","quantity","price")],by=c("hhid"),all=TRUE)
    
    print ("Merging market prices with ohs household ids")
    hhidsRegionRecPrices<-merge(k[,c("item","region","district","ward","ea","recorded_price","recorded_quantity")],householdLocation,by = c("item","region","district","ward","ea"),all.y = TRUE)
    
    hhidsRegionRecPrices$pricediff<-abs(1-hhidsRegionRecPrices$price/hhidsRegionRecPrices$recorded_price)
    
    print ("Matching market prices with inferred prices")
    minPriceDiffs<-ddply(hhidsRegionRecPrices,.(item,region,district,ward,ea,hhid),summarise,pricediff=min(pricediff))
    
    hhidsRegionRecClosestPrices<-merge(minPriceDiffs,hhidsRegionRecPrices,by=c("hhid","item","region","district","ward","ea","pricediff"))
    print ("Returning")
    return(hhidsRegionRecClosestPrices)
  }
  
  
  
  get_inferred_prices <-function(year,dirprefix,fu,ln,shortNamesFile,datConsum){
    if (missing(datConsum)){
      
      # loading ohs data
      ohs<-load_ohs_file(year=year, dirprefix = dirprefix,fu = fu,ln = ln)
      print ("Loaded OHS file")
      # loading diary data
      datConsum<-load_diary_file(dirprefix = dirprefix,year = year, fu=fu, ln=ln )
    }
    
    datConsum$factor<-as.integer(datConsum$lwp_unit==1)+as.integer(datConsum$lwp_unit==2)/1000.0+as.integer(datConsum$lwp_unit==3)+as.integer(datConsum$lwp_unit==4)/1000.0
    
    # convert "piece" to units of volume (l/ml) or weight (g/kg)
    pieceMeasureMapping                      <-ln()@get_piece_measures(year=year)
    datConsum                                <-merge(datConsum,pieceMeasureMapping,all.x=TRUE)
    
    datConsum[ is.na(datConsum$piece_unit) & datConsum$lwp_unit!=5 & !is.na(datConsum$lwp_unit) ,]$piece_unit<-0
    
    
    datConsum$converted_unit                 <- datConsum$piece_unit + as.integer(datConsum$lwp_unit==2 | datConsum$lwp_unit==1) + 3*as.integer(datConsum$lwp_unit==3 | datConsum$lwp_unit==4)
    
    pieceData                                <-subset(datConsum,lwp_unit==5 & is.na(piecefactor))
    if (dim(pieceData)[1]>0){
      stop(paste("The items: ",toString(unique(pieceData$item))," do not have piece conversions available."))
    }
    
    datConsum[is.na(datConsum$piecefactor),]$piecefactor <- 0. # deactivate piecefactor for lwp_unit ==5 i.e. when unit is not in pieces
    
    if (length(setdiff(unique(datConsum$converted_unit),c(1,3,NA)))>0){
      print(unique(datConsum$converted_unit))
      stop("converted_unit must be either kg(1) or liter(3)")
    }
    
    datConsum$factor     <-datConsum$factor+ as.integer(datConsum$lwp_unit==5)*datConsum$piecefactor # datConsum$factor is zero when lwp_unit is not 5
    
    datConsum$quantity   <-datConsum$factor*datConsum$lwp # quantity is now in liters or kilograms
    
    # quantity thus computed has been unified into kilograms or liters (pieces are converted to respective kg/l as well)
    # quantity is now made uniform across a whole group using another field group_quantity which expresses the quantity in the 
    # group's standard unit
    
    # For example, 130 mls of coffee needs 15 grams of coffee or tea or miscpowder. So, to express quantity in multiplying with unif_factor 
    # for g->mls for coffee as 130/15 (available in a map :coffee-> 130/15 )
    
    datConsum$group_quantity <- datConsum$quantity
    
    groupMap          <- rename(ln()@get_group_qconv_factor(year=year),c("lwp_unit"="converted_unit"));
    
    
    ## ddply(subset(datConsum,item==11104),.(lwp_unit),summarize,length(hhid)) # we can also complain that appropriate conversions don't exist 
    ## (we can ignore improper conversions automatically)
    
    
    datConsum <- merge(datConsum,groupMap,by = c("item","converted_unit")  , all.x=TRUE);
    
    datConsum[ is.na(datConsum$group_unit),]$group_unit <- datConsum[ is.na(datConsum$group_unit),]$converted_unit
    
    datConsum[is.na(datConsum$unif_factor),]$unif_factor           <-1
    
    datConsum$group_quantity  <- with(datConsum,group_quantity*unif_factor)
    
    badUnithhids    <-ln()@ignored_bad_units(year=year,datConsum=datConsum)
    
    
    datConsum       <-subset(datConsum,!is.element(hhid,badUnithhids ) )
    
    # make sure there are no multiple quantities after grouping
    unitsForItems    <- ddply(datConsum,.(item),summarise,n=length(unique(group_unit)))
    
    if (  dim(subset(unitsForItems, n>1))[1]>0){
      stop(paste("Number of units >1 for items:",subset(unitsForItems, n>1)$item))
    }
    
    
    datConsum$price  <-datConsum$cost/datConsum$quantity
    datConsum        <-subset(datConsum,item>10000)
    datConsum        <-merge(datConsum,ddply(datConsum,.(hhid),summarise,x=sum(cost)))
    if (year == 2010){
      ii               <-merge(ln()@items_codes_2010(),read.csv(shortNamesFile)[c("calories","shortname","group")],by=c("shortname"))
    }
    else
    {
      stop(paste("get_inferred_prices - year",year,"not supported"))
    }
    
    ii               <-rename(ii,c("code"="item","item"="longname"))
    
    print (paste("Assuming the groups in the file:",shortNamesFile ,"are in sync with the groups in the conversion function"))
    
    
    datConsum$group                                      <-NULL # group would be overwritten by the merge
    datConsum                                            <-merge(datConsum,ii,all.x=TRUE,by=c("item"))
    
    datConsum$merge_quantity                             <- datConsum$group_quantity
    datConsum$merge_unit                                 <- datConsum$group_unit
    
    # merge_unit
    datConsum[!is.na(datConsum$calories),]$merge_quantity       <- datConsum[!is.na(datConsum$calories),]$calories  * datConsum[!is.na(datConsum$calories),]$group_quantity 
    datConsum[!is.na(datConsum$calories),]$merge_unit           <- 10
    print ("merge_unit = 10 => calories used as merge_unit")
    
    datConsum$group_quantity              <- NULL
    datConsum$group_unit                  <- NULL
    
    return(datConsum)
    #    print ("Loaded Diary file")
    #    
    #    hhidsRegion<-unique(ohs[,c("hhid","region","district","ward","ea")]) # unique ignores person id
    
    #    hhidsRegion<-subset(hhidsRegion,!is.na(hhid) & !is.na(region))# too many NAs in hhidsRegion
    
    #    householdLocation<-merge(hhidsRegion[c("hhid","region","district","ward","ea")], datConsum[,c("hhid","item","quantity","price")],by=c("hhid"),all=TRUE)
    #    print ("Returning prices with household location")
    #    return(householdLocation)
    
  }
  
  load_market_prices <-function(year,dirprefix,fu,ln,use_pieces,aggregation_code){
    
    if (missing(use_pieces)){
      use_pieces <- TRUE
    } else {
      use_pieces <- FALSE
    }
    if (year ==2010) {
      a<-read.dta(paste(dirprefix,'/lsms/TZNPS2COMDTA/COMSEC_CJ.dta',sep=""),convert.factors = FALSE)
      a<-(a[!is.na(a),])
      cj <- fu()@get_translated_frame(dat=a, names=ln()@ohs_seccj_columns_lsms_2010(), m=ln()@ohs_seccj_mapping_lsms_2010())
      item_names<- ln()@items_codes_2010()
    } else if (year == 2012){
      a <- read.dta(paste(dirprefix,'/lsms/tnz2012/TZA_2012_LSMS_v01_M_STATA_English_labels/COM_SEC_CF.dta',sep=""),convert.factors = FALSE)
      a<-(a[!is.na(a),])
      cj <- fu()@get_translated_frame(dat=a, names=ln()@ohs_seccj_columns_lsms_2012(), m=ln()@ohs_seccj_mapping_lsms_2012())
      item_names<- ln()@items_codes_2012()
    } else if (year == 2008){
      a <- read.dta(paste(dirprefix,'/lsms/tnz2008/TZNPS1CMDTA_E/SEC_J2.dta',sep=""),convert.factors = FALSE)
      a<-(a[!is.na(a),])
      cj <- fu()@get_translated_frame(dat=a, names=ln()@ohs_seccj_columns_lsms_2008(), m=ln()@ohs_seccj_mapping_lsms_2008())
      item_names<- ln()@items_codes_2008()
    } else if (year == 2014) {
      a <- read.dta(paste(dirprefix,'/lsms/tnz2014/TZA_2014_NPS-R4_v03_M_v01_A_EXT_STATA11/com_sec_cf.dta',sep=""),convert.factors = FALSE)
      a<-(a[!(is.na(a$cm_f062) & is.na(a$cm_f065)) ,])
      cj <- fu()@get_translated_frame(dat=a, names=ln()@ohs_seccf_columns_lsms_2014(), m=ln()@ohs_seccf_mapping_lsms_2014())
      cj$region <- as.integer(sapply(cj$clusterid,function(x) { strsplit(x,"-")[[1]][1] }))
      cj$district <- as.integer(sapply(cj$clusterid,function(x) { strsplit(x,"-")[[1]][2] }))
      cj$ward <- as.integer(sapply(cj$clusterid,function(x) { strsplit(x,"-")[[1]][3] }))
      cj$village <- as.integer(sapply(cj$clusterid,function(x) { strsplit(x,"-")[[1]][4] }))
      cj$ea <- as.integer(sapply(cj$clusterid,function(x) { strsplit(x,"-")[[1]][5] }))
      
      k1<-subset(cj,!is.na(price1) & is.na(price2))
      k2<-subset(cj,is.na(price1) & !is.na(price2))
      k3<-subset(cj,!is.na(price1) & !is.na(price2))
      res <- NULL
      cols <- c("region","district","ward","village","ea","item","lwp","lwp_unit","price")
      res <- rbind(res,plyr::rename(k1,c("lwp_unit1"="lwp_unit","lwp1"="lwp","price1"="price"))[,cols])[,cols]
      res <- rbind(res,plyr::rename(k2,c("lwp_unit2"="lwp_unit","lwp2"="lwp","price2"="price"))[,cols])[,cols]
      res <- rbind(res,plyr::rename(k3,c("lwp_unit1"="lwp_unit","lwp1"="lwp","price1"="price"))[,cols])[,cols]
      cj  <- unique(res)
      item_names<- ln()@items_market_price_codes_2014()
    }
    
    else {
      stop(paste("year:",year,"not supported"))
    }
    
    if (use_pieces==FALSE) {
      cj_new <- subset(cj,lwp_unit!=5)
      print(paste("Ignoring pieces from the market prices (", sprintf("%.2f",100*(1-dim(cj_new)[1]/dim(cj)[1])),"% entries)"))
      cj     <- cj_new
    }
    cj<-(cj[!is.na(cj$price),])
    cj <- subset(cj,price>0)
    cj$factor<-as.integer(cj$lwp_unit==1)+as.integer(cj$lwp_unit==2)/1000.0+as.integer(cj$lwp_unit==3)+as.integer(cj$lwp_unit==4)/1000.0+as.integer(cj$lwp_unit==5)
    
    cj$lwp <-cj$lwp*cj$factor 
    
    cj$price      <-cj$price/cj$lwp
    
    cj$has_number <- !grepl("[^0-9]+",cj$item) & !is.na(cj$item)
    cj$code       <- mapply( function(has_number,item) { if(has_number) { as.integer(item)+10000 } else { item }}, 
                             cj$has_number, 
                             as.character(cj$item))
    #cj$code       <- sapply(cj$item,function(x) {if (is.na(as.integer(x))) x else as.integer(x)+10000 } )
    
    
    Lcodes<-unique(as.character(cj$code)[grep("^L",as.character(cj$code))])
    
    cj$code <- sapply(cj$code,function(x) { if (!is.na(x) && is.element(x,Lcodes)) as.integer(substr(x,2,nchar(x))) else x })
    
    cj <- subset(cj,price!=Inf)
    
    cj$has_number <- NULL
    
    if (missing(aggregation_code)) {
      prices <- cj
    } else {
      if (aggregation_code=="region"){
        prices<-ddply(cj,.(code,region),summarise,mean_price=mean(price[!is.na(price)]),qprices = quantile(names=FALSE, x=price[!is.na(price)],probs=0.5) , pricessd = sd(price[!is.na(price)]) ) 
      } else if (aggregation_code=="district"){
        prices<-ddply(cj,.(code,region,district),summarise,mean_price=mean(price[!is.na(price)]),qprices = quantile(names=FALSE, x=price[!is.na(price)],probs=0.5), pricessd = sd(price[!is.na(price)]))
      } else if (aggregation_code=="all") {
        prices<-ddply(cj,.(code),summarise,mean_price=mean(price[!is.na(price)]),qprices = quantile(names=FALSE, x=price[!is.na(price)],probs=0.5), pricessd = sd(price[!is.na(price)]))
      } else {
        stop(paste("Unrecognised value for",aggregation_code))
      }
      
    }
    prices_merged<-merge(prices,item_names,all.x=TRUE,by=c("code"))
    
    missing_shortnames <- unique(subset(prices_merged,is.na(shortname))$code)
    if (length(missing_shortnames)>0){
      print(paste("Could not find itemnames for:",toString(missing_shortnames)))
    }
    prices_merged <- merge(prices_merged, ddply(prices_merged, .(shortname),summarise,median_price = median(price)) , by = c("shortname"))
    
    n<-10
    prices_merged_removed   <-(subset(prices_merged, abs(log(price/median_price,n))>=1))
    prices_merged_remaining <-(subset(prices_merged, abs(log(price/median_price,n))<1))
    print(paste("removing",dim(prices_merged_removed)[1],"/",dim(prices_merged)[1],"(",100*dim(prices_merged_removed)[1]/dim(prices_merged)[1]
                ,"%) price entries for being ",n,"times away from the national median"))
    
    return(prices_merged_remaining)
  }
  
  aggregate_local_prices<-function (cp){
    
    cp<-subset(cp,!is.na(price) & item >10000 & !is.na(region))
    
    prices<-merge(ddply(cp,.(item,region),summarise,localmedianprice=mean(quantile(price,c(.4,.6)))),ddply(cp,.(item),summarise,natmedianprice=mean(quantile(price,c(.4,.6)))))
    
    cpTemp<-merge(prices,cp)
    
    print (paste("Number of households ignored due to distant expenditure/price reporting:",length(unique(subset(cpTemp,abs(1-localmedianprice/natmedianprice)>=1.)$hhid))))
    
    cpTemp<-(subset(cpTemp,abs(1-localmedianprice/natmedianprice)<1.))
    
    prices<-merge(ddply(cpTemp,.(item,region),summarise,localmedianprice=mean(quantile(price,c(.4,.6)))),ddply(cpTemp,.(item),summarise,natmedianprice=mean(quantile(price,c(.4,.6)))))
    
    cp<-merge(prices,cpTemp)
    
    return(cp)
  }
  
  add_localmedian_price_columns<-function(cp)
  {
    p<-cp;
    
    for (i in unique(p$item)){
      
      print(paste('Appending item',i))
      
      p<- merge(p,rename(x = unique(subset(p,item==i)[c("region","localmedianprice")]), c("localmedianprice"=paste("localmedianprice_",i,sep=""))),by=c("region"),all=TRUE)
      
    }
    
    return(p)
  }
  
  read_assets_file<-function(year,dirprefix,fu,ln){
    
    if (year == 2008){
      
      secnFileName <- paste(dirprefix,'/lsms/tnz2008/TZNPS1HHDTA_S/SEC_N.dta',sep="")
      print(paste("read_assets_file - opening file:",secnFileName))
      secndat<-read.dta(secnFileName,convert.factors = FALSE)
      
      assetsData <- fu()@get_translated_frame(dat=secndat,
                                              names=ln()@get_diary_secn_columns_lsms_2008(),
                                              m=ln()@get_diary_secn_fields_mapping_lsms_2008())
      assetsData <-merge(assetsData, rename(ln()@items_codes_2010(),c("code"="itemcode","item"="longname")), all.x=TRUE)
      if (dim(subset(assetsData,is.na(shortname)))[1] >0 ) { stop ("assets codes are not known") ; }
      return(assetsData)
    }
    
    if (year == 2010){
      secnFileName <- paste(dirprefix,'./lsms/TZNPS2HH2DTA/HH_SEC_N.dta',sep="")
      print(paste("read_assets_file - opening file:",secnFileName))
      secndat<-read.dta(secnFileName,convert.factors = FALSE)
      
      assetsData <- fu()@get_translated_frame(dat=secndat,
                                              names=ln()@get_diary_secn_columns_lsms_2010(),
                                              m=ln()@get_diary_secn_fields_mapping_lsms_2010())
      assetsData <-merge(assetsData, rename(ln()@items_codes_2010(),c("code"="itemcode","item"="longname")), all.x=TRUE)
      if (dim(subset(assetsData,is.na(shortname)))[1] >0 ) { stop ("assets codes are not known") ; }
      return(assetsData)
    } 
    if (year == 2012 ) {
      
      #secn of 2010 maps to secm of 2012
      secnFileName <- paste(dirprefix,'./lsms/tnz2012/TZA_2012_LSMS_v01_M_STATA_English_labels/HH_SEC_M.dta',sep="")
      print(paste("read_assets_file - opening file:",secnFileName))
      secndat<-read.dta(secnFileName,convert.factors = FALSE)
      
      assetsData <- fu()@get_translated_frame(dat=secndat,
                                              names=ln()@get_diary_secn_columns_lsms_2012(),
                                              m=ln()@get_diary_secn_fields_mapping_lsms_2012())
      assetsData <-merge(assetsData, rename(ln()@items_codes_2012(),c("code"="itemcode","item"="longname")), all.x=TRUE)
      
      ignored_hhids_adoc <- c("0743-001") # high mtm of house
      assetsData <- subset(assetsData,!is.element(hhid,ignored_hhids_adoc))
      print (paste("Ignored hhids:",toString(ignored_hhids_adoc)))
      if (dim(subset(assetsData,is.na(shortname)))[1] >0 ) { stop ("assets codes are not known") ; }
      return(assetsData)
    }
    stop("read_assets_file - year", year, "not supported")
    
  }
  
  get_asset_score<-function(year, diaryData, assetsData,assetsList,ln){
    unavailable = TRUE
    if (year == 2010 || year == 2012) {
      relevantAssets<-ln()@assets_order_2010_2012(shortnames = assetsList)
      unavailable = FALSE
    } 
    if (unavailable == TRUE) {
      stop( paste ( "No assets data available for year:" , year ) ) 
    }
    #getting masks from diary data
    expenditure    <- subset(relevantAssets,has_expenditure==TRUE)[c("shortname","mask")]
    dy             <- merge(diaryData,expenditure,all.x=TRUE, by=c("shortname"))
    dy             <- subset(subset(dy,!is.na(mask)), cost>0 )
    print(paste("Households(",length(unique(dy$hhid)),") with expenditure data on item(s) - ", toString(expenditure)))
    
    #getting masks from assets data
    nonExpenditure   <- subset(relevantAssets,has_expenditure==FALSE)[c("shortname","mask")]
    ay               <- merge(assetsData,nonExpenditure,all.x=TRUE, by=c("shortname"))
    ay               <- subset(subset(ay,!is.na(mask)), number>0 )
    print(paste("Households(",length(unique(ay$hhid)),")  with no expenditure data on item(s) - ", toString(nonExpenditure)))
    
    #merging ay and dy (cannot overlap since has_expenditure is either TRUE or FALSE)
    ady              <-rbind(rename(dy[,c("hhid","shortname","mask","item") ], c("item"="itemcode")),ay[,c("hhid","shortname","mask","itemcode")])
    
    adys             <-ddply(ady,.(hhid),summarise,asset_score=sum(mask))
    
    noAssetHhids     <-setdiff(unique(diaryData$hhid),unique(adys$hhid))
    noAssetScore     <-data.frame(hhid=noAssetHhids,asset_score=rep(0,length(noAssetHhids)))
    #adys$asset_score <-adys$asset_score/max(adys$asset_score)
    adys             <- rbind(adys,noAssetScore)
    return(adys)
  }
  
  load_ohs_file <-function(year,dirprefix,fu,ln){
    if (year ==2012){
      cbFileName <- paste(dirprefix,'./lsms/tnz2012/TZA_2012_LSMS_v01_M_STATA_English_labels/COM_SEC_CB.dta',sep="")
      cbdat<-read.dta(cbFileName,convert.factors = FALSE)
      
      cb <- fu()@get_translated_frame(dat=cbdat,
                                      names=ln()@ohs_seccb_columns_lsms(2012),
                                      m=ln()@ohs_seccb_mapping_lsms(2012))
      print(paste("Reading file:",cbFileName))
      #* chose facilitycode l and collected accessibility 1 and 2(<10) (in the centre or less than 10 km away)
      l<-(cb[is.element(tolower(as.character(cb$facilitycode)),c("l")),])
      #* extract those with 1
      l$accessiblemarket<-as.integer(l$accessibility==1)
      #* extract those with 2 (and assign them the same status as 1's)
      l$accessiblemarket<-l$accessiblemarket+as.integer(l$accessibility==2 & l$distance<10)
      l=l[!is.na(l$accessiblemarket),]
      #* chose accessible market value using (if both in the centre and closer then ambiguous)
      l_i=ddply(l,.(region,district,ward),summarize,accessiblemarket=max(accessiblemarket))
      l = merge(l,l_i)
      fu()@removeall_cols_except(l,c("region","district","ward","accessiblemarket","travelcost"))
      #* Also considered urban/rural based on population density 
      u <-read.csv(paste(dirprefix,'./lsms/district_code.csv',sep=""))
      u = data.frame(region=u$region,district=u$district,isurbanp=u$is_urban);
      
      
      adat<-read_tnz(filename = paste(dirprefix,'./lsms/tnz2012/TZA_2012_LSMS_v01_M_STATA_English_labels/HH_SEC_A.dta',sep=""),
                     convert_factors = FALSE,
                     hhidColName = "y3_hhid")
      
      a <- fu()@get_translated_frame(dat=adat,
                                     names=ln()@ohs_seca_columns_lsms(2012),
                                     m=ln()@ohs_seca_mapping_lsms_2012())
      a<-merge(a,u)
      a<-merge(a,l)
      a$expensiveregion<-as.integer(is.element(a$region,get_expensiveregion_codes()))
      popDensity <- read.csv(paste(dirprefix,"./lsms/tnzpopdensity.csv",sep=""))
      a<-merge(a,popDensity)
      
      #*    Read section B
      bdat<-read_tnz(filename = paste(dirprefix,'./lsms/tnz2012/TZA_2012_LSMS_v01_M_STATA_English_labels/HH_SEC_B.dta',sep=""),
                     convert_factors = FALSE,
                     hhidColName = "y3_hhid")
      
      b <- fu()@get_translated_frame(dat=bdat,
                                     names=ln()@ohs_info_columns_lsms_2012(),
                                     m=ln()@ohs_mapping_lsms_2012())
      
      
      b$hhid<-as.character(b$hhid)
      #* inferring occupation rank with occupation_mapping
      b<-merge(b,occupation_mapping())
      
      
      cdat<-read_tnz(filename = paste(dirprefix,'./lsms/tnz2012/TZA_2012_LSMS_v01_M_STATA_English_labels/HH_SEC_C.dta',sep=""),
                     convert_factors = FALSE,
                     hhidColName = "y3_hhid")
      
      #*    Read section C
      c <- fu()@get_translated_frame(dat=cdat,
                                     names=ln()@get_ohs_secc_columns_lsms_2012(),
                                     m=ln()@get_ohs_secc_fields_mapping_lsms_2012())
      
      c$education_rank <-as.integer(c$highest_educ>=33)*4 + as.integer(c$highest_educ<33 & c$highest_educ>=25) *3 + as.integer(c$highest_educ>=19 & c$highest_educ<25)*2 + as.integer(c$highest_educ>=1 & c$highest_educ<19)
      
      c$hhid<-as.character(c$hhid)
      ab <- merge(a,b)
      ohs<-merge(ab,c)
      ohs$age <-2012-ohs$YOB
      
      #*    calculated age by subtracting YOB from 2012 (survey year)
      #*    read section J for housing data (rent, number of primary/secondary rooms)
      
      jdat <- read.dta(paste(dirprefix,'./lsms/tnz2012/TZA_2012_LSMS_v01_M_STATA_English_labels/HH_SEC_I.dta',sep=""),convert.factors=FALSE)
      
      j <- fu()@get_translated_frame(dat=jdat,
                                     names=ln()@get_lsms_secj_info_columns_2012(),
                                     m=ln()@get_lsms_secj_fields_mapping_2012())
      j$hhid <-as.character(j$hhid)
      j$roomsnum_secondary[is.na(j$roomsnum_secondary)]<-0
      j$houserent[is.na(j$houserent)]<-0
      print(head(j))
      j$roomsnum<-j$roomsnum_primary+j$roomsnum_secondary
      ohsj<-merge(ohs,j,all=TRUE)
      return(ohsj)
      
    }
    
    if (year == 2010){
      
      #* Read section c_cb file
      cbFileName = paste(dirprefix,'/lsms/TZNPS2COMDTA/COMSEC_CB.dta',sep="")
      print(paste("Reading file ",cbFileName))
      cbdat<-read.dta(cbFileName,convert.factors = FALSE)
      
      
      cb <- fu()@get_translated_frame(dat=cbdat,
                                      names=ln()@ohs_seccb_columns_lsms(2010),
                                      m=ln()@ohs_seccb_mapping_lsms(2010))
      #* chose facilitycode l and collected accessibility 1 and 2(<10) (in the centre or less than 10 km away)
      l<-(cb[is.element(tolower(as.character(cb$facilitycode)),c("l")),])
      #* extract those with 1
      l$accessiblemarket<-as.integer(l$accessibility==1)
      #* extract those with 2 (and assign them the same status as 1's)
      l$accessiblemarket<-l$accessiblemarket+as.integer(l$accessibility==2 & l$distance<10)
      l=l[!is.na(l$accessiblemarket),]
      #* chose accessible market value using (if both in the centre and closer then ambiguous)
      l_i=ddply(l,.(region,district,ward),summarize,accessiblemarket=max(accessiblemarket))
      l = merge(l,l_i)
      fu()@removeall_cols_except(l,c("region","district","ward","accessiblemarket","travelcost"))
      #l[,setdiff(names(l),)]<-NULL
      
      #l = data.frame(region=l$region,district=l$district,ward=l$ward,ea=l$ea,accessiblemarket=l$accessiblemarket)
      ##
      #* Also considered urban/rural based on population density 
      u <-read.csv(paste(dirprefix,'./lsms/district_code.csv',sep=""))
      u = data.frame(region=u$region,district=u$district,isurbanp=u$is_urban);
      
      adat<-read_tnz(paste(dirprefix,'./lsms/TZNPS2HH1DTA/HH_SEC_A.dta',sep=""),FALSE)
      
      a <- fu()@get_translated_frame(dat=adat,
                                     names=ln()@ohs_seca_columns_lsms(2010),
                                     m=ln()@ohs_seca_mapping_lsms_2010())
      a<-merge(a,u)
      a<-merge(a,l)
      a$expensiveregion<-as.integer(is.element(a$region,get_expensiveregion_codes()))
      popDensity <- read.csv(paste(dirprefix,"./lsms/tnzpopdensity.csv",sep=""))
      a<-merge(a,popDensity)
      
      #*    Read section B
      bdat<-read_tnz(paste(dirprefix,'./lsms/TZNPS2HH1DTA/HH_SEC_B.dta',sep=""),FALSE)
      b <- fu()@get_translated_frame(dat=bdat,
                                     names=ln()@ohs_info_columns_lsms_2010(),
                                     m=ln()@ohs_mapping_lsms_2010())
      
      
      b$hhid<-as.character(b$hhid)
      #* inferring occupation rank with occupation_mapping
      b<-merge(b,occupation_mapping())
      
      cdat<-read_tnz(paste(dirprefix,'./lsms/TZNPS2HH1DTA/HH_SEC_C.dta',sep=""),FALSE)
      #*    Read section C
      c <- fu()@get_translated_frame(dat=cdat,
                                     names=ln()@get_ohs_secc_columns_lsms_2010(),
                                     m=ln()@get_ohs_secc_fields_mapping_lsms_2010())
      c$education_rank <-as.integer(c$highest_educ>=33)*4 + as.integer(c$highest_educ<33 & c$highest_educ>=25) *3 + as.integer(c$highest_educ>=19 & c$highest_educ<25)*2 + as.integer(c$highest_educ>=1 & c$highest_educ<19)
      
      c$hhid<-as.character(c$hhid)
      ab <- merge(a,b)
      ohs<-merge(ab,c)
      ohs$age <-2010-ohs$YOB
      #*    calculated age by subtracting YOB from 2010 (survey year)
      #*    read section J for housing data (rent, number of primary/secondary rooms)
      
      jdat <- read.dta(paste(dirprefix,'./lsms/TZNPS2HH1DTA/HH_SEC_J1.dta',sep=""),convert.factors=FALSE)
      j <- fu()@get_translated_frame(dat=jdat,
                                     names=ln()@get_lsms_secj_info_columns_2010(),
                                     m=ln()@get_lsms_secj_fields_mapping_2010())
      j$hhid <-as.character(j$hhid)
      j$roomsnum_secondary[is.na(j$roomsnum_secondary)]<-0
      j$houserent[is.na(j$houserent)]<-0
      j$roomsnum<-j$roomsnum_primary+j$roomsnum_secondary
      ohsj<-merge(ohs,j,all=TRUE)
      return(ohsj)
      
    }
    ##########################2008#########################
    
    if (year == 2008){
      
      #* Read section c_cb file
      cbFileName = paste(dirprefix,'./lsms/tnz2008/TZNPS1CMDTA_E/SEC_B.dta',sep="")
      cbdat<-read.dta(cbFileName,convert.factors = FALSE)
      print(paste("Reading file ",cbFileName))
      
      cb <- fu()@get_translated_frame(dat=cbdat,
                                      names=ln()@ohs_seccb_columns_lsms(2008),
                                      m=ln()@ohs_seccb_mapping_lsms(2008))
      
      
      #* chose facilitycode l and collected accessibility 1 and 2(<10) (in the centre or less than 10 km away)
      l<-(cb[is.element(tolower(as.character(cb$facilitycode)),c(12)),])
      #* extract those with 1
      l$accessiblemarket<-as.integer(l$accessibility==1)
      #* extract those with 2 (and assign them the same status as 1's)
      l$accessiblemarket<-l$accessiblemarket+as.integer(l$accessibility==2 & l$distance<10)
      l=l[!is.na(l$accessiblemarket),]
      #* chose accessible market value using (if both in the centre and closer then ambiguous)
      l_i=ddply(l,.(region,district,ward),summarize,accessiblemarket=max(accessiblemarket))
      l = merge(l,l_i)
      fu()@removeall_cols_except(l,c("region","district","ward","accessiblemarket","travelcost"))
      #l[,setdiff(names(l),)]<-NULL
      
      #l = data.frame(region=l$region,district=l$district,ward=l$ward,ea=l$ea,accessiblemarket=l$accessiblemarket)
      ##
      #* Also considered urban/rural based on population density 
      u <-read.csv(paste(dirprefix,'./lsms/district_code.csv',sep=""))
      u = data.frame(region=u$region,district=u$district,isurbanp=u$is_urban);
      
      adat<-read_tnz(paste(dirprefix,'./lsms/tnz2008/TZNPS1HHDTA_E/SEC_A_T.dta',sep=""),FALSE,hhidColName = "hhid")
      
      a <- fu()@get_translated_frame(dat=adat,
                                     names=ln()@ohs_seca_columns_lsms(2008),
                                     m=ln()@ohs_seca_mapping_lsms_2008())
      a<-merge(a,u)
      a<-merge(a,l)
      a$expensiveregion<-as.integer(is.element(a$region,get_expensiveregion_codes()))
      popDensity <- read.csv(paste(dirprefix,"./lsms/tnzpopdensity.csv",sep=""))
      a<-merge(a,popDensity)
      
      #*    Read section B
      bcdat<-read_tnz(paste(dirprefix,'/lsms/tnz2008/TZNPS1HHDTA_E/SEC_B_C_D_E1_F_G1_U.dta',sep=""),FALSE,hhidColName = "hhid")
      b <- fu()@get_translated_frame(dat=bcdat,
                                     names=ln()@ohs_info_columns_lsms_2008(),
                                     m=ln()@ohs_mapping_lsms_2008())
      
      
      b$hhid<-as.character(b$hhid)
      #* inferring occupation rank with occupation_mapping
      b<-merge(b,occupation_mapping())
      
      #*    Read section C
      c <- fu()@get_translated_frame(dat=bcdat,
                                     names=ln()@get_ohs_secc_columns_lsms_2008(),
                                     m=ln()@get_ohs_secc_fields_mapping_lsms_2008())
      
      c$education_rank <-as.integer(c$highest_educ>=33)*4 + as.integer(c$highest_educ<33 & c$highest_educ>=25) *3 + as.integer(c$highest_educ>=19 & c$highest_educ<25)*2 + as.integer(c$highest_educ>=1 & c$highest_educ<19)
      
      c$hhid<-as.character(c$hhid)
      ab <- merge(a,b)
      ohs<-merge(ab,c)
      ohs$age <-2008-ohs$YOB
      
      
      #*    calculated age by subtracting YOB from 2010 (survey year)
      #*    read section J for housing data (rent, number of primary/secondary rooms)
      
      #lsms/tnz2008/TZNPS1HHDTA_E/SEC_H1_J_K2_O2_P1_Q1_S1.dta
      jdat <- read.dta(paste(dirprefix,'./lsms/tnz2008/TZNPS1HHDTA_E/SEC_H1_J_K2_O2_P1_Q1_S1.dta',sep=""),convert.factors=FALSE)
      j <- fu()@get_translated_frame(dat=jdat,
                                     names=ln()@get_lsms_secj_info_columns_2008(),
                                     m=ln()@get_lsms_secj_fields_mapping_2008())
      j$hhid <-as.character(j$hhid)
      j$roomsnum_secondary[is.na(j$roomsnum_secondary)]<-0
      j$houserent[is.na(j$houserent)]<-0
      j$roomsnum<-j$roomsnum_primary+j$roomsnum_secondary
      ohsj<-merge(ohs,j,all=TRUE)
      ohsj$litlang <- rep(NA,dim(ohsj)[1])
      ohsj$y2_hhid <- ohsj$hhid
      return(ohsj)
      
    }
    
    #######################################################
    stop(paste("Year:",year," not supported"))
    
  }
  
  visible_categories<-function(year){
    #return(c("213","214","219","224","301","313","314"));
    #return(c("10101","10102"))#rice
    #return(c("218")) #bar soap 
    #return(c("224")) # radio/watch personal items repairs
    #return(c("211")) # toothbrush
    #return(c("10801","10802","10803","10804","10805","10806","10807","10808","10809","10810")) # meat
    #return(c("10701","10702","10703","10704"))
    #return(c("101")) # cigarettes/tobacco
    #return(c("11106","11107","11108")) # alcohol
    #return(c("218")) # donations
    #return(c("202"))# electricity
    #return(food_categories_lsms_2010());
    # return(c("313","314"))
    #return(c("313"))
    # 219 - Motor vehicle service, repair, or parts
    # 214 - Other personal products (shampoo, razor blades, cosmetics, hair products, etc.)
    # 301 - Carpets, rugs
    # 313 - Bride price
    # 314 - Funeral costs
    # 224 - repairs to personal items
    return (-1)
  }
  
  
  get_ignored_hhids<-function(hh,ohs,income){
    #* ignored 5 households with really high expenditure on marriage (more than reported annual income)
    ignoredhhids_adhoc<- c("0701006104006701","0702006012004001","0701021174002601","0702001125000103")
    #* ignored households with zero income (ensuring that not more than 2.5% number of households are ignored)
    if (is.null(income)) {
      ignoredhhids_zero_income<-NULL
    }
    else {
      ignoredhhids_zero_income <- unique(income[as.integer(income$yearly_pay)==0,]$hhid)
      ignored_threshold<-.025
      if( length(ignoredhhids_zero_income)/length(unique(income$hhid))>ignored_threshold){
        stop (paste("More than",ignored_threshold*100, "% hhids with zero income"))
      }
      print(paste("Ignored ",length(ignoredhhids_zero_income),"/",length(unique(income$hhid)),"(=",
                  length(ignoredhhids_zero_income)/length(unique(income$hhid)),") households with zero income" ))
    }
    
    ignored<-union(ignoredhhids_zero_income,ignoredhhids_adhoc)
    return(ignored)
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
  
  
  analyse_cj<-function(dirprefix,sl){
    cjdat<-read.dta(paste(dirprefix,'/./lsms/TZNPS2COMDTA/COMSEC_CJ.dta',sep=''),convert.factors = FALSE) 
    
    cj <- fu()@get_translated_frame(dat=cjdat, names=ln@ohs_seccj_columns_lsms_2010(), m=ln@ohs_seccj_mapping_lsms_2010())
    cj$factor<-as.integer(cj$lwp_unit==1)+as.integer(cj$lwp_unit==2)/1000.0+as.integer(cj$lwp_unit==3)+as.integer(cj$lwp_unit==4)/1000.0+as.integer(cj$lwp_unit==5)
    cj$lwp <-cj$lwp*cj$factor
    #cj$price <-cj$price/cj$lwp
    
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
  
  
  load_income_file<-function (year,dirprefix,fu,ln){
    if (year == 2010){
      #* read section E
      idat1 <-read_tnz(paste(dirprefix,'./lsms/./TZNPS2HH1DTA/HH_SEC_E1.dta',sep=""),FALSE)
      idat2 <-read_tnz(paste(dirprefix,'./lsms/./TZNPS2HH1DTA/HH_SEC_E2.dta',sep=""),FALSE)
      i1 <- fu()@get_translated_frame(dat=idat1,
                                      names=ln()@get_lsms_sece1_columns_2010(),
                                      m=ln()@get_lsms_sece_fields_mapping_2010())
      #TODO: add the conversion into get_translated_frame functionality
      i1$hhid<-as.character(i1$hhid)
      i2 <- fu()@get_translated_frame(dat=idat1,
                                      names=ln()@get_lsms_sece2_columns_2010(),
                                      m=ln()@get_lsms_sece_fields_mapping_2010())
      i2$hhid<as.character(i2$hhid)
      #TODO: add the conversion into get_translated_frame functionality
      
      ti <- ln()@infer_lsms_sece_total_income(i1,i2);
      #* inferred section e data
      #* ))
      # idat2 has only got self-employment details
      
      return(ti)
    } 
    if (year == 2012){
      return(NULL)
    }
    stop(paste("Year ",year," not supported"))
  }
  
  get_total_expenditures <- function (hh,ohs) {
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
    
    totexp$total_expenditure=totexp$total_expenditure+totexp$toteducexpense+totexp$tothouserent
    
    #* calculating educational expense and total houserent
    print(paste("Number of households with total expenditure data = ",length(unique(totexp$hhid))))
    #* finding personids of the house-heads and their education-level, age, years in community, 
    #* language, occupation and 
    #* other household characteristics
    return(totexp)
  }
  
  get_household_head_info <- function (ohs) {
    
    heads<-ohs[as.integer(ohs$household_status)==1,]
    print(paste("Number of houesehold heads = ",length(unique(heads$hhid))))
    if (!is.element("y2_hhid",colnames(heads)) ){
      heads$y2_hhid<-rep(NA,dim(heads)[1])
    }
    
    heads<-data.frame(hhid=heads$hhid,
                      y2_hhid = heads$y2_hhid,
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
                      travelcost=heads$travelcost,
                      schoolowner=heads$schoolowner,
                      occupation = heads$occupation,
                      occupation_rank = heads$occupation_rank,
                      years_community=heads$years_community,
                      housingstatus=heads$housingstatus,
                      roomsnum=heads$roomsnum,
                      roofmaterial=heads$roofmaterial,
                      floormaterial=heads$floormaterial,
                      wallsmaterial=heads$wallsmaterial,
                      toilet=heads$toilet,
                      cookingfuel=heads$cookingfuel,
                      dryseasonwatersource=heads$dryseasonwatersource,
                      
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
    
    return(heads)
  }
  
  get_hsize<-function (ohs)
  {
    
    print ("Calculating hsize")
    hhid_personid_consu<-data.frame(hhid=ohs$hhid,personid=ohs$personid,age=ohs$age,stringsAsFactors=FALSE);
    szNonIgnoredAgeWithNAs<- dim(hhid_personid_consu)[1]
    szIgnoredAgeWithNAs<-dim(hhid_personid_consu[is.na(hhid_personid_consu$age),])[1]
    hhid_personid_consu<-hhid_personid_consu[!is.na(hhid_personid_consu$age),]
    print (paste("Ignored ", szIgnoredAgeWithNAs ," (out of ",szNonIgnoredAgeWithNAs ,") ohs entries with no age (cursz=",
                 dim(hhid_personid_consu)[1],")"))
    
    #calculating the consumption_factor
    
    hhid_personid_consu$consumption_factor<-as.integer(hhid_personid_consu$age<=5)*.2+as.integer(hhid_personid_consu$age>5 & hhid_personid_consu$age<=10)*.3+as.integer(hhid_personid_consu$age>10 & hhid_personid_consu$age<=15)*.4+as.integer(hhid_personid_consu$age>15 & hhid_personid_consu$age<=45)+as.integer(hhid_personid_consu$age>45 & hhid_personid_consu$age<=65)*.7+as.integer(hhid_personid_consu$age>65)*.6
    
    hhid_personid<- ddply(hhid_personid_consu,.(hhid),summarize,hsize=length(personid), consu=sum(consumption_factor));
    print(paste("Number of households with hsize data = ",length(unique(hhid_personid$hhid))))
    
    return(hhid_personid)
  }
  
  merge_hh_ohs_income_data<-function(hh,ohs,income,year,selected_category,fu,set_depvar){
    if (year == 2010 || year == 2012) {
      if (!is.integer(ohs$household_status)|| !(is.integer(ohs$highest_educ))){
        stop("factors must be converted an integer")
      }
      print ("Calculating visible expenditures")
      print(paste("Total number of households to search for visible consumption=",length(unique(hh$hhid))))
      vis             <- fu()@filter_categories_data(hh=hh,selected_category = selected_category, item_field = "item", set_depvar=set_depvar)
      print(paste("Number of households with visible expenditure = ",length(unique(vis$hhid))))
      
      totexp          <- get_total_expenditures(hh=hh,ohs=ohs)
      
      heads           <- get_household_head_info(ohs=ohs)
      
      hhid_personid   <- get_hsize(ohs)
      
      print("Merging visual expenditure")
      ds              <- merge(totexp,vis);
      
      print(paste("Number of households after merging total expenditure with visible expenditure= ",length(unique(ds$hhid))))
      print(paste("Merging hsize",dim(ds)[1]))
      
      ds              <- merge(ds,hhid_personid);
      print(paste("Number of households after merging resultant with hsize data= ",length(unique(ds$hhid))))
      
      ds              <- merge(ds,heads)
      print(paste("Number of households after merging resultant with household head data = ",length(unique(ds$hhid))))
      
      #print(paste("Merging income",dim(ds)[1]))
      #ds<-merge(ds,income)
      print(paste("personid range:",toString(unique(ds$personid))))
      ds$personid     <- NULL
      return(ds)
    }
    stop(paste("merge not available for year:",year))
  }
  
  
  
  
  ####
  
  
  item_usage<-function(itemName,dat,ohs)
  {
    hhidsRegion<-unique(ohs[,c("hhid","region","district","ward","ea")]) # unique ignores person id 
    hhidsRegion<-subset(hhidsRegion,!is.na(hhid) & !is.na(region))# too many NAs in hhidsRegion
    
    
    k<-(subset(dat,shortname==itemName)[,c("item","hhid","cost","shortname")])
    
    kk<-merge(k[,c("hhid","item","shortname","cost")],hhidsRegion[c("hhid","region","district","ward","ea")],by=c("hhid"),all.x=TRUE)
    
    return(kk)
    
  }
  
  item_ownership<-function(itemName,assets,ohs)
  {
    hhidsRegion<-unique(ohs[,c("hhid","region","district","ward","ea")]) # unique ignores person id 
    hhidsRegion<-subset(hhidsRegion,!is.na(hhid) & !is.na(region))# too many NAs in hhidsRegion
    
    k<-subset(subset(assets,shortname==itemName)[,c("itemcode","hhid","number","shortname")], number>0)
    
    kk<-merge(k[,c("hhid","itemcode","shortname","number")],hhidsRegion[c("hhid","region","district","ward","ea")],by=c("hhid"),all.x=TRUE)
    
    return(subset(kk, !is.na(region)))
    
  }
  
  
  
  get_regressed_market_prices <-function (lgc,ld,marketpricesdata,ohsdata,diarydata) {
    
    
    
    region_district_consumed_items <- unique(merge(unique(ohsdata[,c("region","district","hhid")]),
                                                   unique(diarydata[,c("hhid","shortname","item")],by=c("hhid")))[,c("region","district","shortname","item")])
    
    print("Obtaining district level prices")
    market_prices_district <- ddply(unique(marketpricesdata[,c("region","district","code","shortname","lwp","price","factor")]),
                                    .(region,district,shortname),summarise,nprices=length(price[!is.na(price)]),
                                    median_price=median(price[!is.na(price)]), 
                                    reg_price_district=lgc()@get_regressed_market_price(lwp=lwp,price=price) )[,c("region","district","shortname","median_price","nprices","reg_price_district")]
    print("Marking the missing prices from districts") 
    matched_district_prices <- merge(market_prices_district,region_district_consumed_items,by=c("region","district","shortname"),
                                     all.y=TRUE)
    print("Obtaining regional,national prices")
    market_prices_regional <- ddply(marketpricesdata,.(region,shortname),summarise,reg_price_region=lgc()@get_regressed_market_price(lwp=lwp,price=price))[,c("region","shortname","reg_price_region")]
    market_prices_national <- ddply(marketpricesdata,.(shortname),summarise,reg_price_nat=lgc()@get_regressed_market_price(lwp=lwp,price=price))[,c("shortname","reg_price_nat")]
    print("Merging regional, national prices")
    price_reg_dstt  <-merge(market_prices_regional,matched_district_prices,by=c("region","shortname"),all.y=TRUE)
    price_nat_reg_dstt <-merge(market_prices_national,price_reg_dstt,by=c("shortname"),all.y=TRUE)
    
    price_nat_reg_dstt <- subset(price_nat_reg_dstt,item>10000);
    
    natprices <- subset(price_nat_reg_dstt,!is.na(reg_price_nat))
    print(paste("Ignored price entries:",(dim(price_nat_reg_dstt)[1]-dim(natprices)[1]),"/",dim(price_nat_reg_dstt)[1],"(", 
                100*(dim(price_nat_reg_dstt)[1]-dim(natprices)[1])/dim(price_nat_reg_dstt)[1],"%)"))
    prices = ddply(natprices,.(shortname,region,district),summarise,price=lgc()@select_market_price(nat=reg_price_nat,
                                                                                                    reg=reg_price_region,
                                                                                                    dstt=reg_price_district))
    #k<-merge(c2010,np2010,by=c("region","district","shortname"),all.x=TRUE)
    return(prices)
  }
  
  
  
  add_market_price_to_fooddiary <- function (lgc,ld,marketpricesdata,ohsdata,ddata){
    
    prices     <-     get_regressed_market_prices (lgc,ld,marketpricesdata,ohsdata,ddata)
    #k<-merge(c2010,np2010,by=c("region","district","shortname"),all.x=TRUE)
    #market_prices_national <- ddply(marketpricesdata,.(shortname),summarise,reg_price_nat=get_regressed_market_price(lwp=lwp,price=price))[,c("shortname","reg_price_nat")]
    diarywithregiondistrict <- merge(ddata,unique(ohsdata[,c("hhid","region","district")]),all.x=TRUE)
    k<-merge(diarywithregiondistrict,prices,all.x=TRUE)
    noregionprice <- subset(k,is.na(region) & is.na(price))
    ignored_items <- as.character(unique(noregionprice$shortname))
    print(paste("Ignoring consumption on:", toString(ignored_items)))
    diary                   <- subset(k,!is.element(shortname,ignored_items))
    print(paste("Total number of entries ignored:",(dim(k)[1]-dim(diary)[1]),"/",dim(k)[1],"(",
                100*(dim(k)[1]-dim(diary)[1])/dim(k)[1],"%)"))
    print("PENDING: Addition of prices for wheat, bunscakes, beer, othervegstarch and winespirits (marked as beer)")
    
    return(diary)
  }
  
  add_market_price_to_misc_diary <-function (curyear,dirprefix,fu,ln, groups,lgc,ld,marketpricesdata,ohsdata,ddata) {
    
    # adding electricity and others
    
    
    interpolation_years <- c(2008,2010,2012,2014)
    years_to_use <- setdiff(interpolation_years,c(curyear))
    
    region_district_consumed_items <- unique(merge(unique(ohsdata[,c("region","district","hhid")]),
                                                   unique(ddata[,c("hhid","shortname","item")],by=c("hhid")))[,c("region","district","shortname","item")])
    
    alldat <- get_regressed_market_prices(lgc = lgc, ld = ld, marketpricesdata = marketpricesdata, ohsdata = ohsdata, diarydata = ddata)
    if (dim(alldat)[1]>0){
      stop(paste("market prices retrieval FAILURE for year: ",curyear))
    }
    
    print(paste("Interpolation: Using market prices for year:",curyear))
    for (yr in years_to_use) {
      print(paste("Interpolation: Getting market prices for year:",yr))
      mdata        <- load_market_prices(year = yr, dirprefix = dirprefix,fu = fu, ln = ln, use_pieces = FALSE)
      mdat         <- get_regressed_market_prices(lgc = lgc, marketpricesdata = mdata, ohsdata = ohsdata, diarydata = ddata)
      mdat$year    <- yr
      print("Ignoring the use of village column")
      mdat$village <- NULL
      alldat       <- rbind(alldat,mdat)
      yeardata     <- ddply(alldat, .(shortname,year), summarise, n = length(price))
      yearmarkers  <- expand.grid(shortname=unique(as.character(alldat$shortname)),year=interpolation_years)
      k<-merge(yearmarkers,yeardata,all.x=TRUE)

    }

    #append electricity, transport and household indices (which are national averages)

    alldat$id <- paste(alldat$shortname,alldat$region,alldat$district,sep=",")
    allyearsdf <-merge(unique(alldat[,c("shortname","region","district","id")]),expand.grid(id=unique(alldat$id), year=interpolation_years),by=c("id"))
    paddedalldat <- merge(allyearsdf,alldat,all.x=TRUE)
    paddedalldat <- merge(paddedalldat,groups,all.x=TRUE)
    
    
    curprices <- ddply(paddedalldat[,c("region","district","shortname","category","year","price")],
                       .(region,district,category,shortname),summarise, 
                       price=lgc()@fill_missing_yearvals(category,year,price,curyear))
    curprices$ year <- curyear
    
    return(merge(region_district_consumed_items,curprices,all.x=TRUE))
    
    # the price data should have the following columns
    #shortname, region, district, hhid, item, cost, is_consumed, 
    #lwp_unit, lwp, own_unit, own, gift_unit, gift, price
    
    # FIND MISSING 
    
  }
  
  group_collect <- function(year,dirprefix,categoryName,fu,ln,lgc,ld, ohs, hh,basis, use_market_prices) {
    
    
    if (year == 2010 || year == 2012) {
      if (year == 2010){
        itemcodes <- ln()@items_codes_2010()
      } else {
        itemcodes <- ln()@items_codes_2012()
      }
      if (basis=="price"){
        print("Price based groups")
        groups      <- subset( ln()@lsms_groups_pricebased_2010_2012(), category == categoryName )
      } else if (basis == "sparseness"){
        print("sparseness based groups")
        groups      <- subset( ln()@lsms_groups_sparsenessbased_2010_2012(), category == categoryName )
      } else if (basis == "quality"){
        print("quality based groups")
        groups      <- subset( ln()@lsms_groups_qualitybased_2010_2012(), category == categoryName )
      } else {
        stop("Invalid basis")
      }
      
      # check if group columns are known
      if (!setequal(colnames(groups),c("shortname","group","category"))){
        stop("groups must strictly have shortname, group, category columns")
      }
      
    } else {
      stop(paste("Year",year,"not supported for group_collect"))
    } 
    
    
    print(paste("Collecting from groups:",toString(unique(groups$group))))
    if (setequal(groups$group,c("quality"))) {
      # get prices for the year
      print ("Loaded prices noted by the respondents (instead of the market recorded prices)")
      
      if ( use_market_prices == FALSE) {
        print ("Using unit-value as prices")
        mp<-match_recorded_prices(year = year, dirprefix = dirprefix,fu = fu, ln=ln,marketPricesOnly = TRUE)
        mpp<-ddply(mp,.(item),summarise,mean_price=mean(recorded_price))
        itemcodesmap <- plyr::rename(itemcodes[,c("shortname","code")],c("code"="item"))
        mpp <- merge(mpp,itemcodesmap)
        hhp <- merge(hh,mpp,by=c("shortname"))
        hhpg <- merge(hhp,groups,by=c("shortname"))
        print("Obtained prices for quality/expenditureonly group")
        minprices <- ddply(hhpg[,c("shortname","mean_price","category")],.(category),summarise,min_price=min(mean_price))
        hhpg$factor<-as.integer(hhpg$lwp_unit==1)+as.integer(hhpg$lwp_unit==2)/1000.0+as.integer(hhpg$lwp_unit==3)+as.integer(hhpg$lwp_unit==4)/1000.0+as.integer(hhpg$lwp_unit==5)
        hhpg$quantity <-with (hhpg,lwp*factor)
        
        hhpg <- merge(minprices,hhpg)
        # get price ratio for every group
        hhpg$price_ratio <- with (hhpg,mean_price/min_price) 
      } else {
        print ("Using survey's market prices")
        mktprices <- load_market_prices(year = year, dirprefix = dirprefix,fu = fu , ln = ln, use_pieces = FALSE)
        fooddiarydata      <- subset(hh,item>10000)
        hhp <- add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices,ohsdata=ohs,ddata=fooddiarydata)
        #handle non-food and missing items here
        #notice that we don't worry about items which we don't have diary data for
        relevant_names <- intersect(unique(hh$shortname),groups$shortname)
        miscdiarydata  <- subset(hh,is.element(shortname,setdiff(relevant_names,unique(hhp$shortname))))
        
        hhpm <- add_market_price_to_misc_diary (curyear = year, dirprefix =dirprefix, fu=fu, ln=ln, groups = groups, lgc=lgc,
                                                ld = ld, marketpricesdata=mktprices,ohsdata=ohs,ddata=miscdiarydata)
        print("group_collect: RETURNING PREMATURELY")
        return(hhpm)
        hhpg <- merge(hhp,groups,by=c("shortname"))
        minprices <- ddply(hhpg[,c("shortname","price","category","region","district")],.(category,region,district),summarise,min_price=min(price))
        
        hhpg <- merge(minprices,hhpg)
        # get price ratio for every group
        hhpg$price_ratio <- with (hhpg,price/min_price) 
        hhpg <- plyr::rename(hhpg,c("lwp"="quantity"))
      }
      
      # calculate sum of quantity consumed
      totq <- ddply(unique(hhpg[,c("hhid","shortname","category","quantity","price_ratio","min_price","cost")]),.(category,hhid),summarise,totq=sum(quantity), 
                    qsum = sum (price_ratio*quantity), min_price=unique(min_price),tot_categ_exp = sum(cost))[,c("hhid","category","qsum","totq","min_price","tot_categ_exp")]
      # calculate the quality ratio -  sum (price/min(price)*quantity) / sum (quantity)
      totq$quality <- with(totq,qsum/totq)
      
      print(head(totq))
      return(totq[,c("hhid","category","quality","min_price","tot_categ_exp")])
      
    } else if (setequal(groups$group,c("high","low")) || setequal(groups$group,c("low")) || setequal(groups$group,c("high"))) {
      print("Using high-low groups")
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
      if (dim(vis[is.na(vis$low_cost),])[1]>0){
        vis[is.na(vis$low_cost),]$low_cost   <- 0
      }
      
      vis$low_cost                         <- vis$low_cost  + 1e-16 # to avoid divide by zero error in the ratio
      vis$high_cost                        <- vis$high_cost + 1e-16
      
      vis$highratio                        <- with(vis,high_cost/(low_cost+high_cost))
      vis$quality                          <- vis$highratio
      vis$ln_tot_categ_exp                 <- log(with(vis,high_cost+low_cost))
      vis$tot_categ_exp                    <- with(vis,high_cost+low_cost)
      vis$high_expenditure                 <- vis$high_cost
      vis$group                            <- NULL
      vis$low_cost                         <- NULL
      vis$high_cost                        <- NULL
      if (length(unique(groups$category))>1){
        stop("Cannot handle more than one category")
      } else {
        vis$category <- unique(groups$category)
      }
      
    } else if (setequal(groups$group,c("asset","expenditure"))){
      print("Using asset-expenditure groups")
      
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
      print("Using assets-only groups")
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
      print("Using expenditure-only groups")
      print("Only expenditure to be used as the dependent variable");
      relevantItems    <- as.character(subset(groups, group== "expenditureonly")$shortname)
      print(paste("Expenditures being added up for items:",toString(relevantItems)))
      
      vis              <- ddply(subset( merge(hh,groups), is.element(shortname, relevantItems) ),.(hhid),summarize,group_cost=sum(cost))
      no_vis_hhid      <- setdiff(unique(hh$hhid),unique(vis$hhid))
      no_vis           <- data.frame(hhid=no_vis_hhid,group_cost=rep(0,length(no_vis_hhid)))
      vis              <- rbind(vis,no_vis)
      vis$quality      <- log(vis$group_cost+1e-7)
      if (length(unique(groups$category))>1){
        stop("Cannot handle more than one category")
      } else {
        vis$category <- unique(groups$category)
      }
    } else {
      stop( paste ( "Unknown row elements in groups frame",toString(unique(groups$group))))
    }
    print("Collected group expenditures")
    return(vis)
  }
  
  
  ####
  group_expenditure <- function(year,dirprefix,fu,ln,lgc,ld,basis,categoryNames,returnBeforeGrouping,minConsumerNumber,
                                assets_type,use_market_prices){
    if (missing(returnBeforeGrouping)){
      returnBeforeGrouping <- FALSE
    }
    
    
    hh            <- load_diary_file(dirprefix=dirprefix,year=year, fu=fu,ln=ln) # must provide total and visible expenditure (must be already translated)
    
    
    
    #* Loading the person/family data file
    ohs           <-load_ohs_file(dirprefix=dirprefix,year=year,fu=fu,ln=ln) # (using fmld) must provide age (age_ref), gender(sex_ref), 
    
    if (!is.integer(ohs$household_status)|| !(is.integer(ohs$highest_educ))){
      stop("factors must be converted an integer")
    }
    
    print("Ensuring duplicates do NOT exist in the diary hhdata")
    hh = unique(hh)
    
    ignored_hhids <- get_ignored_hhids(hh,ohs,NULL);
    
    if (!is.null(ignored_hhids)){  
      if (!is.null(hh)){
        n1<-length(unique(hh$hhid))
        hh<-hh[!is.element(as.character(hh$hhid),as.character(ignored_hhids)),]
        
        n2<-length(unique(hh$hhid))
        print(paste("ignored",n1-n2,"/",n1," hhids"))
      }
      if (!is.null(ohs)){
        
        n1<-length(unique(ohs$hhid))
        ohs<-ohs[!is.element(as.character(ohs$hhid),as.character(ignored_hhids)),]
        
        n2<-length(unique(ohs$hhid))
        print(paste("ignored",n1-n2,"/",n1," hhids"))
      }
    }
    
    totexp          <- get_total_expenditures(hh=hh,ohs=ohs)
    
    heads           <- get_household_head_info(ohs=ohs)
    
    hhid_personid   <- get_hsize(ohs)
    
    if (missing(minConsumerNumber)){
      minConsumerNumber <- 10
      
    }
    
    if (returnBeforeGrouping){
      vis <- hh
      mcn <- ddply(vis, .(shortname),summarise,nc=length(unique(hhid)))
      negligibleShortNames <- unique(as.character(subset(mcn,nc<=minConsumerNumber)$shortname))
      print(paste("Removing", toString(negligibleShortNames), "from the analysis on grounds of low(",minConsumerNumber, ") number of consumers "))
      vis                 <- subset(vis,!is.element(shortname,negligibleShortNames))
      
    } else {
      # if there are multiple categorynames then - use (h %>% spread(category, quality))
      
      if (basis=="quality"){
        pdat <- NULL 
        qualitydat <- NULL
        minpricedat <- NULL
        categexpdat <- NULL
        
        for (categ in categoryNames){
          to_be_added <- group_collect(year=year,dirprefix=dirprefix,fu=fu,ln=ln,lgc=lgc,ld=ld,categoryName=categ
                                       ,hh=hh,basis=basis, ohs=ohs,use_market_prices=use_market_prices)
          print(paste("To be added: ", toString(colnames(to_be_added))))
          qualitydat     <-   rbind(qualitydat,to_be_added[,c("hhid","category","quality")])
          minpricedat   <-   rbind(minpricedat,to_be_added[,c("hhid","category","min_price")])
          categexpdat    <- rbind(categexpdat,to_be_added[,c("hhid","category","tot_categ_exp")])
        }
        
        vis <- qualitydat %>% spread(category, quality)
        colnames (vis) <- as.character(sapply(colnames(vis), function(x) { if(is.element(x,c("hhid"))) {x} else {paste(x,"_quality",sep="")} }))
        
        pdat <- minpricedat %>% spread(category, min_price)  
        colnames (pdat) <- as.character(sapply(colnames(pdat), function(x) { if(is.element(x,c("hhid"))) {x} else {paste(x,"_min_price",sep="")}}))
        
        cedat <- categexpdat  %>% spread(category, tot_categ_exp)  
        colnames (cedat) <- as.character(sapply(colnames(cedat), function(x) { if(is.element(x,c("hhid"))) {x} else {paste(x,"_tot_categ_exp",sep="")}}))
        
        vis <- merge(pdat,vis,by=c("hhid"))
        vis <- merge(cedat,vis,by=c("hhid"))
        
      } else if (basis == "price" || basis == "sparseness") {
        vis <-   group_collect(year=year,dirprefix=dirprefix,fu=fu,ln=ln,lgc=lgc,ld=ld,categoryName=categoryNames,
                               hh=hh,basis=basis, ohs=ohs, use_market_prices=use_market_prices)
      } else {
        stop (paste("category names not handled for basis:",basis))
      }
    }
    
    ds                  <- merge(totexp,vis);
    print(paste("Merging hsize",dim(ds)[1]))
    
    ds                  <- merge(ds,hhid_personid);
    print(paste("Number of households after merging resultant with hsize data= ",length(unique(ds$hhid))))
    
    ds                  <- merge(ds,heads)
    print(paste("Number of households after merging resultant with household head data = ",length(unique(ds$hhid))))
    
    print(paste("personid range:",toString(unique(ds$personid))))
    
    if (is.element("tot_categ_exp",colnames(ds))){
      ds$w                <- with(ds,tot_categ_exp/total_expenditure)
    }
    if (is.element("asset_score",colnames(ds))) {
      ds$ln_asset_score   <- log(ds$asset_score+1e-7)
    }
    
    ds$ln_tot_exp       <- with(ds,log(total_expenditure+1e-16))
    ds$personid         <- NULL
    
    if (year == 2012)
    {
      if (missing(assets_type)){
        assets_type <- "allassets"
      }
      if (assets_type == "groupwise"){
        asset_order_func <- ln()@asset_types_2010_2012
      } else if (assets_type == "allassets"){
        asset_order_func <- ln()@inheritable_assets_2010_2012
      }
      a<-read_assets_file(year = year, dirprefix = dirprefix,fu = fu, ln=ln)
      a<-subset(subset(a,!is.na(cost)),number>0)
      missingTypes <- setdiff(unique(as.character(a$shortname)), as.character(asset_order_func()$shortname))
      if ( length ( missingTypes )>0 ){
        stop(paste("Asset types not known for:",toString(missingTypes)))
      }
      a <-merge(a,asset_order_func())
      
      ac<-ddply(a,.(hhid),summarise,tot_asset_cost=sum(cost))
      at<-ddply(a,.(hhid,assettype),summarise,type_cost=sum(cost))
      at<-spread(data=at, key = assettype,value = type_cost)
      ac<-merge(ac,at,by=c("hhid"),all=TRUE)
      ac[is.na(ac)]<-0
      if (abs(sum(rowSums(subset(ac,select=setdiff(colnames(ac),c("hhid","tot_asset_cost"))))-ac$tot_asset_cost))>1e-7){
        stop("Asset-types costs-splitting error!")
      }
      assetCols <- setdiff(colnames(ac),c("hhid","tot_asset_cost"))
      for (logCol in assetCols){
        ac[paste("ln_",logCol,sep="")] = log(subset(ac,select=logCol)+1e-7)
      }
      ds<-merge(ds,ac,by=c("hhid"),all.x=TRUE)
      
      ds$ln_tot_asset_cost<-log(ds$tot_asset_cost+1e-7)
      
      missingAssets<-subset(ds,is.na(tot_asset_cost))
      print(paste("The number of families with no assets-recorded",length(unique(missingAssets$hhid)),"/",length(unique(ds$hhid)),"(setting asset_cost to 0)"))
      
      if(dim(missingAssets)[1]>0) {
        
        for (logCol in assetCols){
          val=1e-7
          ds[is.na(ds$tot_asset_cost),][logCol]<-val
          ds[is.na(ds$tot_asset_cost),][paste("ln_",logCol,sep="")]<-log(val)
          print(paste("zeroed",logCol,"(sz=",dim(ds[is.na(ds$tot_asset_cost),])[1],")"))
        }
        ds[is.na(ds$tot_asset_cost),]$ln_tot_asset_cost<-log(val)
        ds[is.na(ds$tot_asset_cost),]$tot_asset_cost<-val # changing at last to invalidate
      }
      print("zeroed out missing assets asset costs")
      # adding band dummies
      for (logCol in assetCols){
        bandColName <- paste("band_",logCol,sep="")
        logColName  <- paste("ln_",logCol,sep="")
        logData     <- ds[,c(logColName)]   
        b1_start    <- quantile(logData,0)
        b1_end      <- quantile(logData,0.25)
        b2_start    <- quantile(logData,0.25)
        b2_end    <- quantile(logData,0.5)
        b3_start    <- quantile(logData,0.5)
        b3_end    <- quantile(logData,0.75)
        b4_start    <- quantile(logData,.75)
        b4_end    <- quantile(logData,1.0)
        print(paste("band start,end=",b1_start,",",b1_end))
        print(paste("logColName=",logColName))
        ds[bandColName] <- NA
        print(paste("bandColName=",bandColName))
        ds[ds[,c(logColName)] >= b1_start & ds [,c(logColName)] <= b1_end ,][,c(bandColName)] <- 1
        print("Assigned 1")
        ds[ds[,c(logColName)] >= b2_start & ds [,c(logColName)] <= b2_end ,][,c(bandColName)] <- 2
        print("Assigned 2")
        ds[ds[,c(logColName)] >= b3_start & ds [,c(logColName)] <= b3_end ,][,c(bandColName)] <- 3
        print("Assigned 3")
        ds[ds[,c(logColName)] >= b4_start & ds [,c(logColName)] <= b4_end ,][,c(bandColName)] <- 4
        print("Assigned 4")
        
      }
    }  
    if (is.element("household",colnames(ds)) && is.element("toteducexpense",colnames(ds)) && is.element("tothouserent",colnames(ds))) {
      print ("Adding education expense and house rent into household expense")
      ds$household <- with(ds,log(toteducexpense+tothouserent+exp(household)))
    }
    ds <- add_high_low_exp_ratios(ds)
    return(ds)
    
  }
  
  
  
deprecated_check_diary_nullity<-function(criteria, year,dirprefix,fu,ln){
    
    
    if (year == 2010){
      itemsfunc <- ln()@items_codes_2010
      bounds <- c(400,500)
    } else if (year == 2012){
      itemsfunc <- ln()@items_codes_2012
      bounds<- c(400,500)
    } else {
      stop(paste("Year",year,"not supported"))
    }
    if (missing(criteria)){
      criteria <- c("region","district")
    }
    g2<-group_expenditure(year = year, dirprefix = dirprefix,fu = fu, ln=lsms_normalizer, returnBeforeGrouping = TRUE)
    
    allrds<-unique(subset(g2,select=criteria))
    
    shortNamesNonAsset <- subset(itemsfunc(), !( as.integer(as.character(code))>=bounds[1] & as.integer(as.character(code))<bounds[2]) )$shortname
    
    allrdsShortNames <- merge(allrds,data.frame("shortname"=shortNamesNonAsset))
    mergeCriteriaArray <- c(criteria,"shortname")
    print(paste("Merge criteria - ",toString(mergeCriteriaArray)))
    print("Obtaining costs for criteria")
    regExpenses <- ddply(g2, mergeCriteriaArray, summarise,mean=mean(cost),n=length(cost))
    print("Merging expenditure with criteria")
    #return(allrdsShortNames)
    f<-merge(allrdsShortNames,regExpenses,by=mergeCriteriaArray,all.x=TRUE)
    print("Merged")
    return(f)
  }
  
  
  food_expenditure_data<-function(dirprefix,year,fu,ln,foodDiary,shortNamesFile){
    if (is.element(year,c(2010,2012))){
      if (missing(foodDiary)){
        foodDiary    <-get_inferred_prices(year = year,dirprefix = dirprefix , fu = fu , ln = ln, shortNamesFile=shortNamesFile);
      }
      
      ohs          <-load_ohs_file(dirprefix=dirprefix,year=year,fu=fu,ln=ln)
      
      heads<-ohs[as.integer(ohs$household_status)==1,]
      print(paste("Number of houesehold heads = ",length(unique(heads$hhid))))
      if (!is.element("y2_hhid",colnames(heads)) ){
        heads$y2_hhid<-rep(NA,dim(heads)[1])
      }
      
      heads<-data.frame(hhid=heads$hhid,
                        y2_hhid = heads$y2_hhid,
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
                        occupation = heads$occupation,
                        occupation_rank = heads$occupation_rank,
                        years_community=heads$years_community,
                        housingstatus=heads$housingstatus,
                        roomsnum=heads$roomsnum,
                        roofmaterial=heads$roofmaterial,
                        floormaterial=heads$floormaterial,
                        wallsmaterial=heads$wallsmaterial,
                        toilet=heads$toilet,
                        cookingfuel=heads$cookingfuel,
                        dryseasonwatersource=heads$dryseasonwatersource,
                        stringsAsFactors=FALSE);
      
      print ("Calculating hsize")
      hhid_personid_consu<-data.frame(hhid=ohs$hhid,personid=ohs$personid,age=ohs$age,stringsAsFactors=FALSE);
      szNonIgnoredAgeWithNAs<- dim(hhid_personid_consu)[1]
      szIgnoredAgeWithNAs<-dim(hhid_personid_consu[is.na(hhid_personid_consu$age),])[1]
      hhid_personid_consu<-hhid_personid_consu[!is.na(hhid_personid_consu$age),]
      print (paste("Ignored ", szIgnoredAgeWithNAs ," (out of ",szNonIgnoredAgeWithNAs ,") ohs entries with no age (cursz=",
                   dim(hhid_personid_consu)[1],")"))
      
      #calculating the consumption_factor
      
      hhid_personid_consu$consumption_factor<-as.integer(hhid_personid_consu$age<=5)*.2+as.integer(hhid_personid_consu$age>5 & hhid_personid_consu$age<=10)*.3+as.integer(hhid_personid_consu$age>10 & hhid_personid_consu$age<=15)*.4+as.integer(hhid_personid_consu$age>15 & hhid_personid_consu$age<=45)+as.integer(hhid_personid_consu$age>45 & hhid_personid_consu$age<=65)*.7+as.integer(hhid_personid_consu$age>65)*.6
      
      hhid_personid<- ddply(hhid_personid_consu,.(hhid),summarize,hsize=length(personid), consu=sum(consumption_factor));
      print(paste("Number of households with hsize data = ",length(unique(hhid_personid$hhid))))
      ds <-merge(foodDiary,hhid_personid,by=c("hhid"));
      print(paste("Number of households after merging resultant with hsize data= ",length(unique(ds$hhid))))
      ds<-merge(ds,heads)
      print(paste("Number of households after merging resultant with household head data = ",length(unique(ds$hhid))))
      
      return(ds)
    }
    
    stop(paste("food_expenditure_data : year",year, " not supported"))
    
  }
  
  add_high_low_exp_ratios<- function(ds) {
    if (all(is.element(c("high_expenditure","total_expenditure","highratio","tot_categ_exp"), colnames(ds)))) {
      print("Adding high-low expenditures to total expenditure")
      ds$w_sparse<-with(ds,high_expenditure/total_expenditure)
      ds$w_nonsparse<-with(ds,(tot_categ_exp*(1-highratio)/total_expenditure))
    }
    return(ds)
  }
  
  split_households <- function(fromYear,toYear,dirprefix,fu,ln){
    
    if (toYear == 2008){
      stop("Can't provide households for before the earliest available year ")
    }
    
    if (fromYear == 2008 && toYear == 2010){
      
      a2008 <- read_assets_file(year = fromYear, dirprefix =dirprefix,fu = fu , ln=ln)
      a2010 <- read_assets_file(year = toYear, dirprefix =dirprefix,fu = fu , ln=ln)
      a2008<-subset(a2008,number>0)
      a2010<-subset(a2010,number>0)
      ## 2008 hhids are 2 characters shorter than 2010, the expanded digits are to record split 
      #  households
      a2010$hhid2008<-substr(as.character(a2010$hhid),1,nchar(as.character(a2010$hhid))-2) 
      #c2010 <- (ddply(a2010[,c("hhid2008","itemcode")],.(hhid2008),summarise,a2010=toString(itemcode[order(itemcode)])))
      #c2008 <- (ddply(a2008[,c("hid2008","itemcode")],.(hhid2008),summarise,a2008=toString(itemcode[order(itemcode)]))) 
      print ("Finding splits in asset data")
      ca <- merge(unique(a2010[,c("hhid2008","hhid")]),data.frame(hhid2008=unique(a2008[,c("hhid2008")])))
      
      
      dat <- ddply(ca,.(hhid2008),summarise,n=length(unique(hhid))) 
      print(paste("Found",length( unique (subset(dat,n>1)$hhid2008)), "splits"))
      
      ##
      hh2008<-load_diary_file(year = fromYear, dirprefix =dirprefix,fu = fu , ln=ln)
      
      hh2008$hhid2008<-hh2008$hhid
      
      hh2010<-load_diary_file(year = toYear, dirprefix =dirprefix,fu = fu , ln=ln)
      
      hh2010$hhid2008<-substr(as.character(hh2010$hhid),1,nchar(as.character(hh2010$hhid))-2) 
      print ("Finding splits in diary data")
      ch <- merge(unique( hh2010[,c("hhid2008","hhid")] ) ,data.frame(hhid2008=unique(hh2008[,c("hhid2008")])))
      
      dath <- ddply(ch,.(hhid2008),summarise,n=length(unique(hhid))) 
      print(paste("Found ",length( unique (subset(dath,n>1)$hhid2008)), "splits"))
      ##
      ohs2008<-load_ohs_file(year = fromYear, dirprefix =dirprefix,fu = fu , ln=ln)
      
      ohs2008$hhid2008 <- ohs2008$hhid
      
      ohs2010<-load_ohs_file(year = toYear, dirprefix =dirprefix,fu = fu , ln=ln)
      
      ohs2010$hhid2008<-substr(as.character(ohs2010$hhid),1,nchar(as.character(ohs2010$hhid))-2)
      print ("Finding splits in ohs data")
      
      co <- merge(unique(hh2010[,c("hhid2008","hhid")]),unique(data.frame(hhid2008=hh2008[,c("hhid2008")])))
      
      dato <- ddply(co,.(hhid2008),summarise,n=length(unique(hhid)))
      print(paste("Found ",length( unique (subset(dato,n>1)$hhid2008)), "splits"))
      doubleHhids<- ( unique ( union( union( (subset(dato,n>1))$hhid2008, subset(dat,n>1)$hhid2008) , subset(dath,n>1)$hhid2008) ) )
      return(doubleHhids)
    }
    
    if (fromYear == 2010 && toYear == 2012){
      ohs2012 <- load_ohs_file(year = toYear, dirprefix = dirprefix,fu = fu , ln = ln)
      ohs2012 <- subset(ohs2012,!is.na(hhid2010) & hhid2010!='')
      
      # 2012 hhids have a different hhid but more than two hhids can be associated with one hhid2010
      print ("Finding splits in ohs data")
      dat <- ddply(ohs2012[,c("hhid","hhid2010")],.(hhid2010),summarise,n=length(unique(hhid)))
      return(unique(subset(dat,n>1)$hhid2010))
      
    }
    
    stop(paste("No data for splits between",fromYear,"and", toYear))
  }
  
  
  
  asset_differences <- function(fromYear,toYear,assetsBaseYear, splitHouseholdHhids, dirprefix, fu, ln){
    
    a2012 <- read_assets_file(year = assetsBaseYear, dirprefix = dirprefix,fu = fu , ln=ln) 
    
    assetsValues2012 <- subset(a2012, ( !is.na(mtm) & cost > 0 )  |  ( !is.na(cost) & cost >0 ) )
    val2012 <- ddply(assetsValues2012 [,c('itemcode','cost','mtm')],.(itemcode),summarise,mean_cost = mean(cost), mean_mtm = mean(mtm), sd_cost = sd(cost), sd_mtm = sd(mtm))
    ##
    
    if (fromYear == 2010 && toYear == 2012){
      a2010 <- read_assets_file(year = fromYear, dirprefix =dirprefix,fu = fu , ln=ln)
      a2010<-subset(a2010,number>0)
      a2010<- plyr::rename(a2010,c("hhid"="hhid2010"))
      
      a2012 <- read_assets_file(year = toYear, dirprefix =dirprefix,fu = fu , ln=ln)
      a2012<-subset(a2012,number>0)
      
      ohs2012 <- load_ohs_file(year = toYear, dirprefix = dirprefix,fu = fu , ln = ln)
      
      notSplitHHids_noy2hhid <-  unique(subset( ohs2012, (is.na(hhid2010) | as.character(hhid2010)=="" )  )$hhid)
      # those that are not split could have a hhid2010 or not - the households
      # that in not-split cagtegory but don't have a hhid2010 would be ignored for asset differences
      
      print(paste("Ignoring - ", length(unique(notSplitHHids_noy2hhid)),"/", length(unique(ohs2012$hhid)) , "households with no corresponding hhid2010"))
      
      
      ohs2012 <- subset(ohs2012,!is.na(hhid2010) & hhid2010!='')
      a2012   <- merge(ohs2012[,c("hhid","hhid2010")],a2012)
      notSplitHHids2012_wy2hhid <-  subset( a2012, !is.na(hhid2010) & as.character(hhid2010)!="" & !is.element(hhid2010,splitHouseholdHhids) )
      
      # double check hhid2010 -> hhid mapping in a2012
      countHhids2012 <- ddply(notSplitHHids2012_wy2hhid, .(hhid2010), summarise , n=length(unique(hhid)))
      if (dim(subset(countHhids2012, n>1))[1]>0){
        #return()
        stop("Invalid list of splitHouseholdHhids provided")
      }
      
      notSplitHHids2010 <-  subset( a2010,!is.element(hhid2010,splitHouseholdHhids) )
      notSplitHHids2010 <- merge(ohs2012[,c("hhid2010","hhid")],notSplitHHids2010,by=c("hhid2010"))
      
      
      cc2010 <- (ddply(unique(notSplitHHids2010[,c("hhid","itemcode")]),.(hhid),summarise,a2010=jsonlite::toJSON(itemcode[order(itemcode)]))) 
      cc2012 <- (ddply(notSplitHHids2012_wy2hhid[,c("hhid","itemcode")],.(hhid),summarise,a2012=jsonlite::toJSON(itemcode[order(itemcode)])))
      
      
      compare2010_2012 <- merge(cc2010,cc2012)
      assetsDiffNonSplit <- ddply(compare2010_2012,.(hhid),summarise, newAssets = fu()@diff_lists (a2012,a2010), 
                                  soldAssets = fu()@diff_lists (a2010,a2012 )) 
      
      # those that have been split - always have hhid2010 (that's how we know of them splitting)
      
      
      # take split households and find the one with the largest asset
      preSplit <- subset(a2010,is.element(hhid2010,splitHouseholdHhids) & !is.na(hhid2010) & as.character(hhid2010)!="" )
      postSplit <- subset(a2012,is.element(hhid2010,splitHouseholdHhids) & !is.na(hhid2010) & as.character(hhid2010)!="" )
      
      #postSplit.is_main should be worked for the household with more assets - the other household(s) - should be treated as new  
      #value of total assets is based on the mean price of assets
      
      assetValues <- merge(val2012[,c("itemcode","mean_cost")], postSplit[,c("hhid","itemcode")], all.x=TRUE)
      if (dim(subset(assetValues, is.na(mean_cost)))[1]>0){
        stop(paste("Cannot find cost(s)/price(s) for", unique(subset(assetValues, is.na(mean_cost))$itemcode)))
      }
      print("Calculating total asset values")
      postSplitTotalAssetValues <- ddply(assetValues,.(hhid),summarise,total_asset_value=sum(mean_cost))
      postSplitTotalAssetValues <- merge(unique(postSplit[,c("hhid","hhid2010")]),postSplitTotalAssetValues)
      
      maxHhids <- (ddply(postSplitTotalAssetValues,.(hhid2010),summarise,hhid=fu()@get_max_col(total_asset_value,hhid)))
      maxHhids$has_max <- 1
      allHhids <- (merge(maxHhids[,c("hhid","has_max")],postSplitTotalAssetValues[,c("hhid","hhid2010")],all.y=TRUE,by=c("hhid")))
      
      if (dim(allHhids[is.na(allHhids$has_max),])[1]>0){
        allHhids[is.na(allHhids$has_max),]$has_max <- 0
      }
      
      maxAssetsHHsPreSplit <- merge(subset(allHhids,has_max==1),preSplit[,c("hhid2010","itemcode")] )[,c("hhid","itemcode")]
      print("Gathering assets owned")
      cc2010MaxAssets <- (ddply( maxAssetsHHsPreSplit[,c("hhid","itemcode")],.(hhid),summarise,a2010=jsonlite::toJSON(itemcode[order(itemcode)])))
      
      maxAssetsHHsPostSplit <- merge ( subset(allHhids,has_max==1), postSplit[,c("hhid","itemcode")], by=c("hhid"))[,c("hhid","itemcode")]
      cc2012MaxAssets <- (ddply( maxAssetsHHsPostSplit[,c("hhid","itemcode")],.(hhid),summarise,a2012=jsonlite::toJSON(itemcode[order(itemcode)])))
      compare2010_2012_maxassets <- merge(cc2010MaxAssets,cc2012MaxAssets)
      assetsDiff_maxassets <- ddply(compare2010_2012_maxassets,.(hhid),summarise, newAssets = fu()@diff_lists (a2012,a2010), 
                                    soldAssets = fu()@diff_lists (a2010,a2012 )) 
      
      nonMaxHHsPostSplit = merge ( subset(allHhids,has_max==0), postSplit[,c("hhid","itemcode")], by=c("hhid"))[,c("hhid","itemcode")]
      cc2012NonMaxAssets <- (ddply( nonMaxHHsPostSplit[,c("hhid","itemcode")],.(hhid),summarise,a2012=jsonlite::toJSON(itemcode[order(itemcode)])))
      cc2012NonMaxAssets$a2010 <- "[]"
      if (length(intersect(cc2012NonMaxAssets$hhid,assetsDiff_maxassets$hhid))){
        stop("Max and not max cannot overlap")
      }
      assetsDiff_nonmaxassets <- ddply(cc2012NonMaxAssets,.(hhid),summarise, newAssets = fu()@diff_lists (a2012,a2010), 
                                       soldAssets = fu()@diff_lists (a2010,a2012 )) 
      print(paste("Num nonsplit:",dim(assetsDiffNonSplit)[1]))
      print(paste("Num split (max assets):",dim(assetsDiff_maxassets)[1]))
      print(paste("Num split (not max assets):",dim(assetsDiff_nonmaxassets)[1]))
      print(colnames(assetsDiffNonSplit))
      print(colnames(assetsDiff_maxassets))
      print(colnames(assetsDiff_nonmaxassets))
      splitHHAssetsDiff <- rbind(assetsDiff_nonmaxassets,assetsDiff_maxassets)
      if (length(intersect(splitHHAssetsDiff$hhid,assetsDiffNonSplit$hhid))>0){
        stop("Split and non-split assets must not overlap")
      }
      allAssets <- rbind(splitHHAssetsDiff,assetsDiffNonSplit)
      
      
      return(allAssets)
    }
    if (fromYear == 2008 && toYear == 2010){
      a2008 <- read_assets_file(year = fromYear, dirprefix =dirprefix,fu = fu , ln=ln)
      a2008<-subset(a2008,number>0)
      
      
      a2010 <- read_assets_file(year = toYear, dirprefix =dirprefix,fu = fu , ln=ln)
      a2010<-subset(a2010,number>0)
      
      a2010$hhid2008<-substr(as.character(a2010$hhid),1,nchar(as.character(a2010$hhid))-2)
      # take not-split households
      notSplit2008 <- subset(a2008,!is.element(hhid2008,splitHouseholdHhids))
      notSplit2010 <- subset(a2010,!is.element(hhid2008,splitHouseholdHhids))
      
      # double check hhid2008 -> hhid mapping in a2010
      countHhids2010 <- ddply(notSplit2010, .(hhid2008), summarise , n=length(unique(hhid)))
      if (dim(subset(countHhids2010, n>1))[1]>0){
        stop("Invalid list of splitHouseholdHhids provided")
      }
      
      cc2008 <- (ddply(notSplit2008[,c("hhid2008","itemcode")],.(hhid2008),summarise,a2008=jsonlite::toJSON(itemcode[order(itemcode)])))
      # add unique hhid2008->hhid mapping
      cc2008 <- unique(merge(cc2008,a2010[,c("hhid","hhid2008")])[,c("hhid","a2008")])
      cc2010 <- (ddply(unique(notSplit2010[,c("hhid","itemcode")]),.(hhid),summarise,a2010=jsonlite::toJSON(itemcode[order(itemcode)]))) 
      
      compare2008_2010 <- merge(cc2008,cc2010)
      assetsDiffNonOverlapping <- ddply(compare2008_2010,.(hhid),summarise, newAssets = fu()@diff_lists (a2010,a2008), 
                                        soldAssets = fu()@diff_lists (a2008,a2010 )) 
      
      # take split households and find the one with the largest asset
      preSplit <- subset(a2008,is.element(hhid2008,splitHouseholdHhids))
      postSplit <- subset(a2010,is.element(hhid2008,splitHouseholdHhids))
      
      #postSplit.is_main should be worked for the household with more assets - the other household(s) - should be treated as new  
      #value of total assets is based on the mean price of assets
      
      assetValues <- merge(val2012[,c("itemcode","mean_cost")], postSplit[,c("hhid","itemcode")], all.x=TRUE)
      if (dim(subset(assetValues, is.na(mean_cost)))[1]>0){
        stop(paste("Cannot find cost(s)/price(s) for", unique(subset(assetValues, is.na(mean_cost))$itemcode)))
      }
      print("Calculating total asset values")
      postSplitTotalAssetValues <- ddply(assetValues,.(hhid),summarise,total_asset_value=sum(mean_cost))
      postSplitTotalAssetValues <- merge(unique(postSplit[,c("hhid","hhid2008")]),postSplitTotalAssetValues)
      
      maxHhids <- (ddply(postSplitTotalAssetValues,.(hhid2008),summarise,hhid=fu()@get_max_col(total_asset_value,hhid)))
      maxHhids$has_max <- 1
      allHhids <- (merge(maxHhids[,c("hhid","has_max")],postSplitTotalAssetValues[,c("hhid","hhid2008")],all.y=TRUE,by=c("hhid")))
      if (dim(allHhids[is.na(allHhids$has_max),])[1]>0){
        allHhids[is.na(allHhids$has_max),]$has_max <- 0
      }
      #cc2008 <- (ddply(preSplit[,c("hhid2008","itemcode")],.(hhid2008),summarise,a2008=jsonlite::toJSON(itemcode[order(itemcode)])))
      #those with max assets)
      maxAssetsHHsPreSplit <- merge(subset(allHhids,has_max==1),preSplit[,c("hhid2008","itemcode")] )[,c("hhid","itemcode")]
      print("Gathering assets owned")
      cc2008MaxAssets <- (ddply( maxAssetsHHsPreSplit[,c("hhid","itemcode")],.(hhid),summarise,a2008=jsonlite::toJSON(itemcode[order(itemcode)])))
      
      maxAssetsHHsPostSplit <- merge ( subset(allHhids,has_max==1), postSplit[,c("hhid","itemcode")], by=c("hhid"))[,c("hhid","itemcode")]
      cc2010MaxAssets <- (ddply( maxAssetsHHsPostSplit[,c("hhid","itemcode")],.(hhid),summarise,a2010=jsonlite::toJSON(itemcode[order(itemcode)])))
      compare2008_2010_maxassets <- merge(cc2008MaxAssets,cc2010MaxAssets)
      assetsDiff_maxassets <- ddply(compare2008_2010_maxassets,.(hhid),summarise, newAssets = fu()@diff_lists (a2010,a2008), 
                                    soldAssets = fu()@diff_lists (a2008,a2010 )) 
      
      nonMaxHHsPostSplit = merge ( subset(allHhids,has_max==0), postSplit[,c("hhid","itemcode")], by=c("hhid"))[,c("hhid","itemcode")]
      cc2010NonMaxAssets <- (ddply( nonMaxHHsPostSplit[,c("hhid","itemcode")],.(hhid),summarise,a2010=jsonlite::toJSON(itemcode[order(itemcode)])))
      cc2010NonMaxAssets$a2008 <- "[]"
      if (length(intersect(cc2010NonMaxAssets$hhid,assetsDiff_maxassets$hhid))){
        stop("Max and not max cannot overlap")
      }
      assetsDiff_nonmaxassets <- ddply(cc2010NonMaxAssets,.(hhid),summarise, newAssets = fu()@diff_lists (a2010,a2008), 
                                       soldAssets = fu()@diff_lists (a2008,a2010 )) 
      print(paste("Num nonsplit:",dim(assetsDiffNonOverlapping)[1]))
      print(paste("Num split (max assets):",dim(assetsDiff_maxassets)[1]))
      print(paste("Num split (not max assets):",dim(assetsDiff_nonmaxassets)[1]))
      print(colnames(assetsDiffNonOverlapping))
      print(colnames(assetsDiff_maxassets))
      print(colnames(assetsDiff_nonmaxassets))
      splitHHAssetsDiff <- rbind(assetsDiff_nonmaxassets,assetsDiff_maxassets)
      if (length(intersect(splitHHAssetsDiff$hhid,assetsDiffNonOverlapping$hhid))>0){
        stop("Split and non-split assets must not overlap")
      }
      allAssets <- rbind(splitHHAssetsDiff,assetsDiffNonOverlapping)
      return(allAssets)
    }
    stop(paste("Data not available for",fromYear,"->",toYear)) 
  }
  
  combined_data_set<-function(year,dirprefix,selected_category,isDebug, set_depvar, fu, ln){
    
    ############ PHASE 0 ########################
    if (missing(set_depvar)){
      set_depvar = TRUE 
    }
    if (missing(selected_category)){
      print("setting selected_category to the default value")
      selected_category= visible_categories(year=year)
    } else {
      print(paste("using selected_category:",toString(selected_category)));
    }
    
    #*  (( Loading diary file
    hhdat <- load_diary_file(dirprefix=dirprefix,year=year, fu=fu,ln=ln) # must provide total and visible expenditure (must be already translated)
    
    #* Loading the person/family data fie
    ohsdat <-load_ohs_file(dirprefix=dirprefix,year=year,fu=fu,ln=ln) # (using fmld) must provide age (age_ref), gender(sex_ref), 
    # highest_educ(educ_ref), ishead(no_earnr,earncomp - all reference person data),
    # race(ref_race),family size (fam_size),
    # area_type (popsize,bls_urbn)
    #* Loading income file
    incomedat <-load_income_file(dirprefix=dirprefix,year=year,fu=fu,ln=ln) # must provide total income (fincaftm)
    
    if (is.null(hhdat)){
      stop("Could not load diary hhdata")
    }
    
    print("Ensuring duplicates do NOT exist in the diary hhdata")
    hhdat = unique(hhdat)
    
    ############ PHASE 1 - Translation ########################
    # info_columns must contain all hhdata-fields referred to in merging/aggregation phase (one per file)
    # translated frame makes the data available in a universal dictionary (age, gender etc.)
    hh<-hhdat
    ohs <-ohsdat
    income<-incomedat
    
    print("Loaded translated frame(s)")
    ############ PHASE 2 - Aggregation and Merge ########################
    # merge criteria is defined for every dependent variable
    
    ignored_hhids <- get_ignored_hhids(hhdat,ohsdat,incomedat);
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
    dstruct<-merge_hh_ohs_income_data(hh=hh,ohs=ohs,income=income,year=year,fu=fu,selected_category=selected_category,set_depvar=set_depvar);
    return(dstruct);
    #* ))
  }
  
  
  return(new("LSMSLoader",combined_data_set=combined_data_set,load_diary_file=load_diary_file, 
             analyse_cj=analyse_cj,load_ohs_file=load_ohs_file,match_recorded_prices=match_recorded_prices, load_market_prices=load_market_prices,
             get_inferred_prices=get_inferred_prices,aggregate_local_prices=aggregate_local_prices,
             add_localmedian_price_columns=add_localmedian_price_columns,food_expenditure_data=food_expenditure_data,
             read_assets_file=read_assets_file, group_expenditure=group_expenditure,group_collect=group_collect,
             get_asset_score=get_asset_score, item_usage = item_usage, item_ownership=item_ownership,split_households=split_households,asset_differences=asset_differences,
             get_regressed_market_prices=get_regressed_market_prices))
  
}