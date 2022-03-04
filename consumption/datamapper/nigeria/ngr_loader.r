library(foreign)
library(haven)
require(plyr)
require(tidyr)
require(jsonlite)

if (isClass("NigeriaLoader")){
  print ("Warning !!! previous definition of NigeriaLoader would be overwritten.")
}

## all exported functions are declared here
setClass("NigeriaLoader", representation(load_diary_file="function",
                                         load_ohs_file="function", 
                                         load_market_prices = "function", read_assets_file="function"
))

ngr_loader<-function(fu,ngrn,lgc) {
  
  load_market_prices <- function(dirprefix,year,fu,ngrn){
    if (year == 2010){
      fname <- paste(dirprefix,"./lsms/nigeria/2010/NGA_2010_GHSP-W1_v03_M_STATA/Post\ Planting\ Wave\ 1/Community/sectc2_plantingw1.dta",sep="")
      sec2dat <- read.dta(fname, convert.factors = FALSE)
      mdat <- fu()@get_translated_frame(dat=sec2dat,
                                        names=ngrn()@market_data_info(),
                                        m=ngrn()@market_data_columns_mapping(2010))
      mdat <- subset(mdat,!is.na(price) & !is.na(lwp_unit)& !is.na(lwp))
      #merging with itemcodes
      mdat <- merge (plyr::rename(mdat %>% mutate ( item = 10000 + item), c("item"="code")) , ngrn()@item_codes_2010(), by = c("code"))
      if (dim(subset(mdat,is.na(shortname)))[1]>0){
        stop(paste("Failed to interprets codes for the items in the market file",toString(unique(subset(mdat,is.na(shortname))$item))))
      }
      mdatu <- merge(plyr::rename(mdat,c("lwp_unit"="unitcode")),ngrn()@unit_codes_2010(), by = c("unitcode"),all.x=TRUE)
      if (dim(subset(mdatu,is.na(unit)))[1]>0){
        stop(paste("Failed to interprets units in the market file",toString(unique(subset(mdatu,is.na(unit))$item))))
      }
      mdatu <- mdatu %>% mutate(quantity = factor*lwp)
      mdatu <- subset(mdatu,!is.na(factor)) %>% mutate(unit_price = price/quantity) ;
      ignored <- dplyr::filter( merge(mdatu,ddply(mdatu,.(shortname),summarise,v=fu()@fv(unit_price)),all.x=TRUE) , unit_price >= v)
      print(paste("Number of entries ignored due to extreme values:", dim(ignored)[1],"(",round(dim(ignored)[1]*100/dim(mdatu)[1],2),"%)"))
      mdatuf <- dplyr::filter( merge(mdatu,ddply(mdatu,.(shortname),summarise,v=fu()@fv(unit_price)),all.x=TRUE) , unit_price < v)
      return(mdatuf)
    }
    
    if (year == 2012){
      
      return(mdatuf)
    }
    stop(paste("Cannot load market prices for year:",year))
  }
  
  read_assets_file<-function(year,dirprefix,fu,ngrn){
    
    
    if (year == 2010){
      
      secnFileName1 <- paste(dirprefix,'./lsms/nigeria/2010/NGA_2010_GHSP-W1_v03_M_STATA//Post Planting Wave 1/Household/sect5_plantingw1.dta',sep="")
      print(paste("read_assets_file - opening file:",secnFileName1))
      secnFileName2 <- paste(dirprefix,'./lsms/nigeria/2010/NGA_2010_GHSP-W1_v03_M_STATA//Post Planting Wave 1/Household/sect5b_plantingw1.dta',sep="")
      print(paste("read_assets_file - opening file:",secnFileName2))
      
      secndat1<-read.dta(secnFileName1,convert.factors = FALSE)
      secndat2<-read.dta(secnFileName2,convert.factors = FALSE)
      
      assetsData1 <- fu()@get_translated_frame(dat=secndat1,
                                               names=c("hhid","itemcode","number"),
                                               m=ngrn()@get_diary_assets_fields_mapping_lsms(year))
      
      assetsData2 <- fu()@get_translated_frame(dat=secndat2,
                                               names=c("hhid","itemcode","age","mtm"),
                                               m=ngrn()@get_diary_assets_fields_mapping_lsms(year))
      
      assetsData <- merge(assetsData1,assetsData2)
      print(paste("Ignoring",nrow(subset(assetsData,is.na(number))),"entries because of no reported number"))
      assetsData <- subset(assetsData,!is.na(number))
      assetsData <-merge(assetsData, ngrn()@items_codes(year), all.x=TRUE)
      ignored_hhids_adoc <- c("") # high mtm of house
      assetsData <- subset(assetsData,!is.element(hhid,ignored_hhids_adoc))
      print (paste("Ignored hhids:",toString(ignored_hhids_adoc)))
      if (dim(subset(assetsData,is.na(shortname)))[1] >0 ) { stop ("assets codes are not known") ; }
      return(assetsData)
    } 
    if (year == 2012 ) {
      
      secnFileName1 <- paste(dirprefix,'./lsms/nigeria/2012/NGA_2012_GHSP-W2_v02_M_STATA/Post Planting Wave 2/Household/sect5a_plantingw2.dta',sep="")
      print(paste("read_assets_file - opening file:",secnFileName1))
      secnFileName2 <- paste(dirprefix,'./lsms/nigeria/2012/NGA_2012_GHSP-W2_v02_M_STATA/Post Planting Wave 2/Household/sect5b_plantingw2.dta',sep="")
      print(paste("read_assets_file - opening file:",secnFileName2))
      
      secndat1<-read.dta(secnFileName1,convert.factors = FALSE)
      secndat2<-read.dta(secnFileName2,convert.factors = FALSE)
      
      assetsData1 <- fu()@get_translated_frame(dat=secndat1,
                                               names=c("hhid","itemcode","number"),
                                               m=ngrn()@get_diary_assets_fields_mapping_lsms(year))
      
      assetsData2 <- fu()@get_translated_frame(dat=secndat2,
                                               names=c("hhid","itemcode","age","mtm"),
                                               m=ngrn()@get_diary_assets_fields_mapping_lsms(year))
      
      assetsData <- merge(assetsData1,assetsData2)
      print(paste("Ignoring",nrow(subset(assetsData,is.na(number))),"entries because of no reported number"))
      assetsData <- subset(assetsData,!is.na(number))
      assetsData <-merge(assetsData, ngrn()@items_codes(year), all.x=TRUE)
      
      ignored_hhids_adoc <- c("") # high mtm of house
      assetsData <- subset(assetsData,!is.element(hhid,ignored_hhids_adoc))
      print (paste("Ignored hhids:",toString(ignored_hhids_adoc)))
      if (dim(subset(assetsData,is.na(shortname)))[1] >0 ) { stop ("assets codes are not known") ; }
      return(assetsData)
    }
    
    if (year == 2015 ) {
      secnFileName <- paste(dirprefix,'./lsms/nigeria/2015/NGA_2015_GHSP-W3_v02_M_Stata/sect5_plantingw3.dta',sep="")
      print(paste("read_assets_file - opening file:",secnFileName))
      
      secndat<-read.dta(secnFileName,convert.factors = FALSE)
      
      
      assetsData <- fu()@get_translated_frame(dat=secndat,
                                              names=c("hhid","itemcode","number","age","mtm"),
                                              m=ngrn()@get_diary_assets_fields_mapping_lsms(year))
      
      print(paste("Ignoring",nrow(subset(assetsData,is.na(number))),"entries because of no reported number"))
      assetsData <- subset(assetsData,!is.na(number))
      assetsData <-merge(assetsData, ngrn()@items_codes(year), all.x=TRUE)
      
      ignored_hhids_adoc <- c("") # high mtm of house
      assetsData <- subset(assetsData,!is.element(hhid,ignored_hhids_adoc))
      print (paste("Ignored hhids:",toString(ignored_hhids_adoc)))
      if (dim(subset(assetsData,is.na(shortname)))[1] >0 ) { stop ("assets codes are not known") ; }
      return(assetsData)
    }
    
    stop("read_assets_file - year", year, "not supported")
    
  }
  
  load_diary_file <-function(dirprefix,year,fu,ngrn,load_cost){
    ##
    if (year == 2010){
      fname <- paste(dirprefix,"./lsms/nigeria/2010/NGA_2010_GHSP-W1_v03_M_STATA/Post\ Planting\ Wave\ 1/Household/sect7b_plantingw1.dta",sep="")
      print(paste("Opening:",fname))
      sec7dat <- read.dta(fname, convert.factors = FALSE)
      k <- fu()@get_translated_frame(dat=sec7dat,
                                     names=ngrn()@diary_info_columns_2010(),
                                     m=ngrn()@diary_columns_mapping(2010))
      #,
      #        convert_factors = FALSE,hhidColName = "y4_hhid")
      # we have 7-day recalls (81) 1-month recalls (82) six month recalls(83) , 12-month recalls (84)
      
      if (load_cost){
        k <- subset(subset(k,!is.na(cost), cost>0))
      } else {
        k <- subset(subset(k,!is.na(lwp) | !is.na(tlwp)),lwp>0 | tlwp>0)
        k$cost <- NA
      }
      
      
      k$item<-as.integer(as.character(k$item))+10000 # adding 10,000 only to avoid overlaps with sections (l,m)
      factor <- 52
      
      #*    Multiplied weekly diary data by 52 (to look at annual data)
      # quantities are normalized to annual values
      k$cost <- k$cost*factor
      k$lwp <- k$lwp *factor
      k$tlwp <- k$tlwp *factor
      k$own <-k$own*factor
      k$gift <-k$gift*factor
      
      k <- merge(plyr::rename(k,c("item"="code")),ngrn()@item_codes_2010()[,c("shortname","code")],by=c('code'),all.x=TRUE)
      if (dim(subset(k,is.na(shortname)))[1]>0){
        stop("Could not find diary items",toString(unique(subset(k,is.na(shortname))$code)))
      }
      
      
      #*    gift quantities are ignored (total quantity ignored is to be presented)
      #*    weekly recall items are also multiplied by 52
      lfname <- paste(dirprefix,"./lsms/nigeria/2010/NGA_2010_GHSP-W1_v03_M_STATA/Post\ Planting\ Wave\ 1/Household/sect81_plantingw1.dta",sep="")
      sec81dat <- read_dta(lfname)
      l <- fu()@get_translated_frame(dat=sec81dat,
                                     names=ngrn()@get_lsms_weekrecall_info_columns(year),
                                     m=ngrn()@get_lsms_weekrecall_fields_mapping(year));
      
      l$hhid <-as.character(l$hhid)
      l <- l[!is.na(l$cost) & l$cost>0 & !is.na(l$hhid),]
      l$item <- as.character(l$item)
      l$cost <- l$cost*52 # 52 weeks
      l <- merge(plyr::rename(l,c("item"="code")),ngrn()@item_codes_2010()[,c("shortname","code")],by=c('code'),all.x=TRUE)
      
      if (dim(subset(l,is.na(shortname)))[1]>0){
        stop("Could not find weekly recall items",toString(subset(l,is.na(shortname))$code))
      }
      
      ## monthly recall
      
      lmfname <- paste(dirprefix,"./lsms/nigeria/2010/NGA_2010_GHSP-W1_v03_M_STATA/Post\ Planting\ Wave\ 1/Household/sect82_plantingw1.dta",sep="")
      sec82dat <- read_dta(lmfname)
      lmdat <- fu()@get_translated_frame(dat=sec82dat,
                                         names=ngrn()@get_lsms_monthrecall_info_columns(year),
                                         m=ngrn()@get_lsms_monthrecall_fields_mapping(year));
      
      lmdat$hhid <-as.character(lmdat$hhid)
      lmdat <- lmdat[!is.na(lmdat$cost) & lmdat$cost>0 & !is.na(lmdat$hhid),]
      lmdat$item <- as.character(lmdat$item)
      lmdat <- merge(plyr::rename(lmdat,c("item"="code")),ngrn()@item_codes_2010()[,c("shortname","code")],by=c('code'),all.x=TRUE)
      if (dim(subset(lmdat,is.na(shortname)))[1]>0){
        stop("Could not find monthly recall items",toString(subset(lmdat,is.na(shortname))$code))
      }
      
      repair_items <- c("maintenance_house","maintenance_household")
      lmdat_nonrepair      <- subset(lmdat,!is.element(shortname,repair_items)) 
      lmdat_repair         <- subset(lmdat,is.element(shortname,repair_items))
      lmdat_repair$cost    <- lmdat_repair$cost*12
      lmdat_nonrepair$cost <- lmdat_nonrepair$cost*6
      lmdat <- rbind(lmdat_nonrepair,lmdat_repair)
      
      # 6 months recall
      l6mfname <- paste(dirprefix,"./lsms/nigeria/2010/NGA_2010_GHSP-W1_v03_M_STATA/Post\ Planting\ Wave\ 1/Household/sect83_plantingw1.dta",sep="")
      sec83dat <- read_dta(l6mfname)
      l6mdat <- fu()@get_translated_frame(dat=sec83dat,
                                          names=ngrn()@get_lsms_sixmonthrecall_info_columns(year),
                                          m=ngrn()@get_lsms_sixmonthrecall_fields_mapping(year));
      
      l6mdat$hhid <- as.character(l6mdat$hhid)
      l6mdat      <- l6mdat[!is.na(l6mdat$cost) & l6mdat$cost>0 & !is.na(l6mdat$hhid),]
      l6mdat$item <- as.character(l6mdat$item)
      l6mdat      <- merge(plyr::rename(l6mdat,c("item"="code")),ngrn()@item_codes_2010()[,c("shortname","code")],by=c('code'),all.x=TRUE)
      if (dim(subset(l6mdat,is.na(shortname)))[1]>0){
        stop("Could not find six-monthly recall items",toString(subset(l6mdat,is.na(shortname))$code))
      }
      l6mdat$cost <- l6mdat$cost*2
      
      #12 months recall
      l1yfname <- paste(dirprefix,"./lsms/nigeria/2010/NGA_2010_GHSP-W1_v03_M_STATA/Post\ Planting\ Wave\ 1/Household/sect84_plantingw1.dta",sep="")
      sec84dat <- read_dta(l1yfname)
      l1ydat <- fu()@get_translated_frame(dat=sec84dat,
                                          names=ngrn()@get_lsms_yearrecall_info_columns(year),
                                          m=ngrn()@get_lsms_yearrecall_fields_mapping(year));
      l1ydat$hhid <- as.character(l1ydat$hhid)
      l1ydat      <- l1ydat[!is.na(l1ydat$cost) & l1ydat$cost>0 & !is.na(l1ydat$hhid),]
      l1ydat$item <- as.character(l1ydat$item)
      l1ydat      <- merge(plyr::rename(l1ydat,c("item"="code")),ngrn()@item_codes_2010()[,c("shortname","code")],by=c('code'),all.x=TRUE)
      if (dim(subset(l1ydat,is.na(shortname)))[1]>0){
        stop("Could not find yearly recall items",toString(subset(l1ydat,is.na(shortname))$code))
      }
      
      #*    merging all the 4 categories results in the expenditure file
      
      y6m <-merge(l1ydat,l6mdat,all=TRUE)
      y6m1m <- merge(y6m,lmdat,all=TRUE)
      
      diary <-merge(y6m1m,k,all=TRUE)
      
      # filtering out extreme values
      if (load_cost){
        extremeDataHhids <- unique ( dplyr::filter( merge(diary,ddply(diary,.(shortname),summarise,v=fu()@fv(cost)),all.x=TRUE) , cost > v)$hhid )
      } else {
        extremeDataHhids <- unique ( dplyr::filter( merge(y6m1m,ddply(y6m1m,.(shortname),summarise,v=fu()@fv(cost) ),all.x=TRUE) , cost > v)$hhid )
      }
      
      print (paste("Households with extreme data (many times the median) - purged from the diary file:",length(extremeDataHhids)))
      diary              <- dplyr::filter(diary,!is.element(hhid,extremeDataHhids))
      return(diary)  
    } # end 2010
    
    if (year == 2012){
      
      fname <- paste(dirprefix,"./lsms/nigeria/2012/NGA_2012_GHSP-W2_v02_M_STATA/Post\ Planting\ Wave\ 2/Household/sect7b_plantingw2.dta",sep="")
      print(paste("Opening:",fname))
      sec7dat <- read.dta(fname, convert.factors = FALSE)
      k <- fu()@get_translated_frame(dat=sec7dat,
                                     names=ngrn()@diary_info_columns_2010(),
                                     m=ngrn()@diary_columns_mapping(2010))
      #,
      #        convert_factors = FALSE,hhidColName = "y4_hhid")
      # we have 7-day recalls (81) 1-month recalls (82) six month recalls(83) , 12-month recalls (84)
      
      if (load_cost){
        k <- subset(subset(k,!is.na(cost), cost>0))
      } else {
        k <- subset(subset(k,!is.na(lwp) | !is.na(tlwp)),lwp>0 | tlwp>0)
        k$cost <- NA
      }
      
      
      k$item<-as.integer(as.character(k$item))+10000 # adding 10,000 only to avoid overlaps with sections (l,m)
      factor <- 52
      
      
      #*    Multiplied weekly diary data by 52 (to look at annual data)
      # quantities are normalized to annual values
      k$cost <- k$cost*factor
      k$lwp <- k$lwp *factor
      k$tlwp <- k$tlwp *factor
      k$own <-k$own*factor
      k$gift <-k$gift*factor
      
      k <- merge(plyr::rename(k,c("item"="code")),ngrn()@item_codes_2012()[,c("shortname","code")],by=c('code'),all.x=TRUE)
      if (dim(subset(k,is.na(shortname)))[1]>0){
        stop("Could not find diary items",toString(unique(subset(k,is.na(shortname))$code)))
      }
      
      
      #*    gift quantities are ignored (total quantity ignored is to be presented)
      #*    weekly recall items are also multiplied by 52
      lfname <- paste(dirprefix,"./lsms/nigeria/2012/NGA_2012_GHSP-W2_v02_M_STATA/Post\ Planting\ Wave\ 2/Household/sect8a_plantingw2.dta",sep="")
      sec81dat <- read_dta(lfname)
      l <- fu()@get_translated_frame(dat=sec81dat,
                                     names=ngrn()@get_lsms_weekrecall_info_columns(2010),
                                     m=ngrn()@get_lsms_weekrecall_fields_mapping(2010));
      
      l$hhid <-as.character(l$hhid)
      l <- l[!is.na(l$cost) & l$cost>0 & !is.na(l$hhid),]
      l$item <- as.character(l$item)
      l$cost <- l$cost*52 # 52 weeks
      l <- merge(plyr::rename(l,c("item"="code")),ngrn()@item_codes_2012()[,c("shortname","code")],by=c('code'),all.x=TRUE)
      
      if (dim(subset(l,is.na(shortname)))[1]>0){
        stop("Could not find weekly recall items",toString(subset(l,is.na(shortname))$code))
      }
      
      ## monthly recall
      
      lmfname <- paste(dirprefix,"./lsms/nigeria/2012/NGA_2012_GHSP-W2_v02_M_STATA/Post\ Planting\ Wave\ 2/Household/sect8b_plantingw2.dta",sep="")
      sec82dat <- read_dta(lmfname)
      lmdat <- fu()@get_translated_frame(dat=sec82dat,
                                         names=ngrn()@get_lsms_monthrecall_info_columns(2010),
                                         m=ngrn()@get_lsms_monthrecall_fields_mapping(2010));
      
      lmdat$hhid <-as.character(lmdat$hhid)
      lmdat <- lmdat[!is.na(lmdat$cost) & lmdat$cost>0 & !is.na(lmdat$hhid),]
      lmdat$item <- as.character(lmdat$item)
      lmdat <- merge(plyr::rename(lmdat,c("item"="code")),ngrn()@item_codes_2012()[,c("shortname","code")],by=c('code'),all.x=TRUE)
      if (dim(subset(lmdat,is.na(shortname)))[1]>0){
        stop("Could not find monthly recall items",toString(subset(lmdat,is.na(shortname))$code))
      }
      
      repair_items <- c("maintenance_house","maintenance_household")
      lmdat_nonrepair      <- subset(lmdat,!is.element(shortname,repair_items)) 
      lmdat_repair         <- subset(lmdat,is.element(shortname,repair_items))
      lmdat_repair$cost    <- lmdat_repair$cost*12
      lmdat_nonrepair$cost <- lmdat_nonrepair$cost*6
      lmdat <- rbind(lmdat_nonrepair,lmdat_repair)
      
      # 6 months recall
      l6mfname <- paste(dirprefix,"./lsms/nigeria/2012/NGA_2012_GHSP-W2_v02_M_STATA/Post\ Planting\ Wave\ 2/Household/sect8c_plantingw2.dta",sep="")
      sec83dat <- read_dta(l6mfname)
      l6mdat <- fu()@get_translated_frame(dat=sec83dat,
                                          names=ngrn()@get_lsms_sixmonthrecall_info_columns(2010),
                                          m=ngrn()@get_lsms_sixmonthrecall_fields_mapping(2010));
      
      l6mdat$hhid <- as.character(l6mdat$hhid)
      l6mdat      <- l6mdat[!is.na(l6mdat$cost) & l6mdat$cost>0 & !is.na(l6mdat$hhid),]
      l6mdat$item <- as.character(l6mdat$item)
      l6mdat      <- merge(plyr::rename(l6mdat,c("item"="code")),ngrn()@item_codes_2012()[,c("shortname","code")],by=c('code'),all.x=TRUE)
      if (dim(subset(l6mdat,is.na(shortname)))[1]>0){
        stop("Could not find six-monthly recall items",toString(subset(l6mdat,is.na(shortname))$code))
      }
      l6mdat$cost <- l6mdat$cost*2
      
      #12 months recall
      l1yfname <- paste(dirprefix,"./lsms/nigeria/2012/NGA_2012_GHSP-W2_v02_M_STATA/Post\ Planting\ Wave\ 2/Household/sect8d_plantingw2.dta",sep="")
      sec84dat <- read_dta(l1yfname)
      l1ydat <- fu()@get_translated_frame(dat=sec84dat,
                                          names=ngrn()@get_lsms_yearrecall_info_columns(2010),
                                          m=ngrn()@get_lsms_yearrecall_fields_mapping(2010));
      l1ydat$hhid <- as.character(l1ydat$hhid)
      l1ydat      <- l1ydat[!is.na(l1ydat$cost) & l1ydat$cost>0 & !is.na(l1ydat$hhid),]
      l1ydat$item <- as.character(l1ydat$item)
      l1ydat      <- merge(plyr::rename(l1ydat,c("item"="code")),ngrn()@item_codes_2012()[,c("shortname","code")],by=c('code'),all.x=TRUE)
      if (dim(subset(l1ydat,is.na(shortname)))[1]>0){
        stop("Could not find yearly recall items",toString(subset(l1ydat,is.na(shortname))$code))
      }
      
      #*    merging all the 4 categories results in the expenditure file
      
      y6m <-merge(l1ydat,l6mdat,all=TRUE)
      y6m1m <- merge(y6m,lmdat,all=TRUE)
      
      diary <-merge(y6m1m,k,all=TRUE)
      
      # filtering out extreme values
      if (load_cost){
        extremeDataHhids <- unique ( dplyr::filter( merge(diary,ddply(diary,.(shortname),summarise,v=fu()@fv(cost)),all.x=TRUE) , cost > v)$hhid )
      } else {
        extremeDataHhids <- unique ( dplyr::filter( merge(y6m1m,ddply(y6m1m,.(shortname),summarise,v=fu()@fv(cost) ),all.x=TRUE) , cost > v)$hhid )
      }
      
      print (paste("Households with extreme data (many times the median) - purged from the diary file:",length(extremeDataHhids)))
      diary              <- dplyr::filter(diary,!is.element(hhid,extremeDataHhids))
      
      return(diary)  
    } # end 2012
    
    if (year == 2015){
      
      fname <- paste(dirprefix,"./lsms/nigeria/2015/NGA_2015_GHSP-W3_v02_M_Stata/sect7b_plantingw3.dta",sep="")
      print(paste("Opening:",fname))
      sec7dat <- read.dta(fname, convert.factors = FALSE)
      k <- fu()@get_translated_frame(dat=sec7dat,
                                     names=ngrn()@diary_info_columns_2010(),
                                     m=ngrn()@diary_columns_mapping(2010))
      #,
      #        convert_factors = FALSE,hhidColName = "y4_hhid")
      # we have 7-day recalls (81) 1-month recalls (82) six month recalls(83) , 12-month recalls (84)
      
      if (load_cost){
        k <- subset(subset(k,!is.na(cost), cost>0))
      } else {
        k <- subset(subset(k,!is.na(lwp) | !is.na(tlwp)),lwp>0 | tlwp>0)
        k$cost <- NA
      }
      
      
      k$item<-as.integer(as.character(k$item))+10000 # adding 10,000 only to avoid overlaps with sections (l,m)
      factor <- 52
      
      
      #*    Multiplied weekly diary data by 52 (to look at annual data)
      # quantities are normalized to annual values
      k$cost <- k$cost*factor
      k$lwp <- k$lwp *factor
      k$tlwp <- k$tlwp *factor
      k$own <-k$own*factor
      k$gift <-k$gift*factor
      
      k <- merge(plyr::rename(k,c("item"="code")),ngrn()@item_codes_2015()[,c("shortname","code")],by=c('code'),all.x=TRUE)
      if (dim(subset(k,is.na(shortname)))[1]>0){
        stop("Could not find diary items",toString(unique(subset(k,is.na(shortname))$code)))
      }
      
      #*    gift quantities are ignored (total quantity ignored is to be presented)
      #*    weekly recall items are also multiplied by 52
      lfname <- paste(dirprefix,"./lsms/nigeria/2015/NGA_2015_GHSP-W3_v02_M_Stata/sect8a_plantingw3.dta",sep="")
      sec81dat <- read_dta(lfname)
      l <- fu()@get_translated_frame(dat=sec81dat,
                                     names=ngrn()@get_lsms_weekrecall_info_columns(2010),
                                     m=ngrn()@get_lsms_weekrecall_fields_mapping(2010));
      
      l$hhid <-as.character(l$hhid)
      l <- l[!is.na(l$cost) & l$cost>0 & !is.na(l$hhid),]
      l$item <- as.character(l$item)
      l$cost <- l$cost*52 # 52 weeks
      l <- merge(plyr::rename(l,c("item"="code")),ngrn()@item_codes_2015()[,c("shortname","code")],by=c('code'),all.x=TRUE)
      
      if (dim(subset(l,is.na(shortname)))[1]>0){
        stop("Could not find weekly recall items",toString(subset(l,is.na(shortname))$code))
      }
      
      ## monthly recall
      
      lmfname <- paste(dirprefix,"./lsms/nigeria/2015/NGA_2015_GHSP-W3_v02_M_Stata/sect8b_plantingw3.dta",sep="")
      sec82dat <- read_dta(lmfname)
      lmdat <- fu()@get_translated_frame(dat=sec82dat,
                                         names=ngrn()@get_lsms_monthrecall_info_columns(2010),
                                         m=ngrn()@get_lsms_monthrecall_fields_mapping(2010));
      
      lmdat$hhid <-as.character(lmdat$hhid)
      lmdat <- lmdat[!is.na(lmdat$cost) & lmdat$cost>0 & !is.na(lmdat$hhid),]
      lmdat$item <- as.character(lmdat$item)
      lmdat <- merge(plyr::rename(lmdat,c("item"="code")),ngrn()@item_codes_2015()[,c("shortname","code")],by=c('code'),all.x=TRUE)
      if (dim(subset(lmdat,is.na(shortname)))[1]>0){
        stop("Could not find monthly recall items",toString(unique(subset(lmdat,is.na(shortname))$code)))
      }
      
      repair_items <- c("maintenance_house","maintenance_household")
      lmdat_nonrepair      <- subset(lmdat,!is.element(shortname,repair_items)) 
      lmdat_repair         <- subset(lmdat,is.element(shortname,repair_items))
      lmdat_repair$cost    <- lmdat_repair$cost*12
      lmdat_nonrepair$cost <- lmdat_nonrepair$cost*6
      lmdat <- rbind(lmdat_nonrepair,lmdat_repair)
      
      # 6 months recall
      l6mfname <- paste(dirprefix,"./lsms/nigeria/2015/NGA_2015_GHSP-W3_v02_M_Stata/sect8c_plantingw3.dta",sep="")
      sec83dat <- read_dta(l6mfname)
      l6mdat <- fu()@get_translated_frame(dat=sec83dat,
                                          names=ngrn()@get_lsms_sixmonthrecall_info_columns(2010),
                                          m=ngrn()@get_lsms_sixmonthrecall_fields_mapping(2010));
      
      l6mdat$hhid <- as.character(l6mdat$hhid)
      l6mdat      <- l6mdat[!is.na(l6mdat$cost) & l6mdat$cost>0 & !is.na(l6mdat$hhid),]
      l6mdat$item <- as.character(l6mdat$item)
      l6mdat      <- merge(plyr::rename(l6mdat,c("item"="code")),ngrn()@item_codes_2015()[,c("shortname","code")],by=c('code'),all.x=TRUE)
      if (dim(subset(l6mdat,is.na(shortname)))[1]>0){
        stop("Could not find six-monthly recall items",toString( unique(subset(l6mdat,is.na(shortname))$code ) ))
      }
      l6mdat$cost <- l6mdat$cost*2
      
      #No 12 months recall for 2015

      #*    merging all the 3 categories results in the expenditure file
      
      y6m1m <- merge(l6mdat,lmdat,all=TRUE)
      
      diary <-merge(y6m1m,k,all=TRUE)
      
      # filtering out extreme values
      if (load_cost){
        extremeDataHhids <- unique ( dplyr::filter( merge(diary,ddply(diary,.(shortname),summarise,v=fu()@fv(cost)),all.x=TRUE) , cost > v)$hhid )
      } else {
        extremeDataHhids <- unique ( dplyr::filter( merge(y6m1m,ddply(y6m1m,.(shortname),summarise,v=fu()@fv(cost) ),all.x=TRUE) , cost > v)$hhid )
      }
      
      print (paste("Households with extreme data (many times the median) - purged from the diary file:",length(extremeDataHhids)))
      diary              <- dplyr::filter(diary,!is.element(hhid,extremeDataHhids))
      
      return(diary)  
      
    }
    
    stop (paste("Cannot process data for year:",year))
    
  }
  
  add_father_educ <- function(ohs,education_rank_mapping){
    ohs$father_educ_temp <- ohs$father_educ
    ohs[is.na(ohs$father_educ_temp),]$father_educ_temp <- 0
    #ignored_educ_types <- c(4,19,20,98) # Invalid entries in the data
    ignored_educ_types <- c(98)
    ohs <- subset(ohs,!is.element(father_educ_temp,ignored_educ_types))
    if (length(setdiff(unique(ohs$father_educ_temp),education_rank_mapping$highest_educ))>0 ){
      stop("Missing education codes")
    }
    ohs <- merge(ohs,plyr::rename(education_rank_mapping,c("highest_educ"="father_educ_temp","education_rank"="father_educ_rank"))[,c("father_educ_temp","father_educ_rank")],by=c("father_educ_temp"))
    ohs$father_educ_temp <- NULL
    return (ohs) 
  }
  
  add_mother_educ <- function(ohs,education_rank_mapping){
    ohs$mother_educ_temp <- ohs$mother_educ
    ohs[is.na(ohs$mother_educ_temp),]$mother_educ_temp <- 0
    ignored_educ_types <- c(4,19,20) # Invalid entries in the data
    ohs <- subset(ohs,!is.element(mother_educ_temp,ignored_educ_types))
    if (length(setdiff(unique(ohs$mother_educ_temp),education_rank_mapping$highest_educ))>0 ){
      stop("Missing education codes")
    }
    ohs <- merge(ohs,plyr::rename(education_rank_mapping,c("highest_educ"="mother_educ_temp","education_rank"="mother_educ_rank"))[,c("mother_educ_temp","mother_educ_rank")],by=c("mother_educ_temp"))
    ohs$mother_educ_temp <- NULL
    return (ohs) 
  }
  
  load_ohs_file <-function(year,dirprefix,fu,ngrn){
    #
    if (year ==2010){
      sec1fname  <-paste(dirprefix,'./lsms/nigeria/2010/NGA_2010_GHSP-W1_v03_M_STATA/Post\ Planting\ Wave\ 1/Household/sect1_plantingw1.dta',sep="")
      sec1dat    <- read.dta(sec1fname,convert.factors = FALSE)
      sec1dat    <- fu()@get_translated_frame(dat=sec1dat,
                                              names=ngrn()@ohs_info_columns_lsms(year),
                                              m=ngrn()@ohs_mapping_lsms(year))
      sec2fname    <- paste(dirprefix,'./lsms/nigeria/2010/NGA_2010_GHSP-W1_v03_M_STATA/Post\ Planting\ Wave\ 1/Household/sect2_plantingw1.dta',sep="")
      sec2dat    <- read.dta(sec2fname,convert.factors = FALSE)
      sec2dat    <- fu()@get_translated_frame(dat=sec2dat,
                                              names=ngrn()@ohs_educ_info_columns_lsms(year),
                                              m=ngrn()@ohs_educ_columns_mapping_lsms(year))
      
      sec3fname    <- paste(dirprefix,'./lsms/nigeria/2010/NGA_2010_GHSP-W1_v03_M_STATA/Post\ Planting\ Wave\ 1/Household/sect3_plantingw1.dta',sep="")
      sec3dat    <- read.dta(sec3fname,convert.factors = FALSE)
      sec3dat    <- fu()@get_translated_frame(dat=sec3dat,
                                              names=ngrn()@ohs_income_info_columns_lsms(year),
                                              m=ngrn()@ohs_income_columns_mapping_lsms(year))
      
      secGeofname    <- paste(dirprefix,'./lsms/nigeria/2010/NGA_2010_GHSP-W1_v03_M_STATA/Geodata/NGA_HouseholdGeovariables_Y1.dta',sep="")
      secGeodat    <- read.dta(secGeofname,convert.factors = FALSE)
      
      secGeodat    <- fu()@get_translated_frame(dat=secGeodat,
                                                names=c("hhid","S","E"),
                                                m=ngrn()@ohs_geodata_columns_mapping_lsms(year))
      
      print(paste("Merging OHS data from files for year:",year))
      ohs <- merge(sec1dat,sec2dat,by=c("hhid","personid"))
      ohs <- merge(ohs,sec3dat,by=c("hhid","personid"), all.x=TRUE)
      ohs <- merge(ohs,secGeodat,by=c("hhid"),all.x=T)

      #food quality      
      secFqname    <- paste(dirprefix,'./lsms/nigeria/2010/NGA_2010_GHSP-W1_v03_M_STATA/Post Planting Wave 1/Household/sect9_plantingw1.dta',sep="")
      secFqdat    <- read.dta(secFqname,convert.factors = FALSE)
      
      secFqdat    <- fu()@get_translated_frame(dat=secFqdat,
                                                names=c("hhid","same_diet","less_quality_1","less_quality_2","outoffood","outoffood_reason1","outoffood_reason2","outoffood_reason3"),
                                                m=ngrn()@ohs_food_quality_columns_mapping_lsms(year))
      secFqdat$outoffood <- as.integer(as.integer(secFqdat$outoffood)==1) + as.integer(as.integer(secFqdat$outoffood)==2)*0
      

      ohs <- merge(ohs,secFqdat,by=c("hhid"),all.x=T)
      education_rank_mapping <- read.csv(paste0(dirprefix,"./lsms/nigeria/education_codes.csv"),stringsAsFactors = F)
    
      ohs$highest_educ <- as.integer(as.character(ohs$highest_educ))
      ohs$age          <- 2010 - as.integer(as.character(ohs$YOB))
      
      ohs$highest_educ_temp <- ohs$highest_educ
      ohs[is.na(ohs$highest_educ_temp),]$highest_educ_temp <- 0
      #ohs$education_rank <- as.integer(ohs$highest_educ_temp<=0)*0 + as.integer(ohs$highest_educ_temp>0 & ohs$highest_educ_temp<=11)*1 + as.integer(ohs$highest_educ_temp>11 & ohs$highest_educ_temp<=23)*2 +as.integer(ohs$highest_educ_temp>23)*3
      if (length(setdiff(unique(ohs$highest_educ_temp),education_rank_mapping$highest_educ))>0 ){
        stop("Missing education codes")
      }
      ohs <- merge(ohs,plyr::rename(education_rank_mapping,c("highest_educ"="highest_educ_temp"))[,c("highest_educ_temp","education_rank")],by=c("highest_educ_temp"))
      ohs$highest_educ_temp <- NULL
    
      #household_status must be determined by 1. rank based on occupation_rank 2. occupation_primary 3. highest_educ 4. qualification 5. age (pay is not available for the most)
      #ohsi <- subset(ohs,is.na(last_payment_primary)) # income units need to be standardised
      ohsf <- add_father_educ(ohs,education_rank_mapping)
      ohsfm <- add_mother_educ(ohsf,education_rank_mapping)
      return(ohsfm)
    }
    
    
    if (year ==2012){
      
      
      sec1fname  <-paste(dirprefix,'./lsms/nigeria/2012/NGA_2012_GHSP-W2_v02_M_STATA/Post\ Planting\ Wave\ 2/Household/sect1_plantingw2.dta',sep="")
      sec1dat    <- read.dta(sec1fname,convert.factors = FALSE)
      sec1dat    <- fu()@get_translated_frame(dat=sec1dat,
                                              names=ngrn()@ohs_info_columns_lsms(year),
                                              m=ngrn()@ohs_mapping_lsms(year))
      sec2fname    <- paste(dirprefix,'./lsms/nigeria/2012/NGA_2012_GHSP-W2_v02_M_STATA/Post\ Planting\ Wave\ 2/Household/sect2_plantingw2.dta',sep="")
      sec2dat    <- read.dta(sec2fname,convert.factors = FALSE)
      sec2dat    <- fu()@get_translated_frame(dat=sec2dat,
                                              names=ngrn()@ohs_educ_info_columns_lsms(2010),
                                              m=ngrn()@ohs_educ_columns_mapping_lsms(year))
      
      sec3fname1    <- paste(dirprefix,'./lsms/nigeria/2012/NGA_2012_GHSP-W2_v02_M_STATA/Post\ Planting\ Wave\ 2/Household/sect3a_plantingw2.dta',sep="")
      sec3dat1    <- read.dta(sec3fname1,convert.factors = FALSE)
      sec3dat1    <- fu()@get_translated_frame(dat=sec3dat1,
                                               names=ngrn()@ohs_income_info_columns_lsms(year),
                                               m=ngrn()@ohs_income_columns_mapping_lsms(year))
      
      
      secGeofname    <- paste(dirprefix,'./lsms/nigeria/2012/NGA_2012_GHSP-W2_v02_M_STATA/Geodata Wave 2/NGA_HouseholdGeovars_Y2.dta',sep="")
      secGeodat    <- read.dta(secGeofname,convert.factors = FALSE)
      
      secGeodat    <- fu()@get_translated_frame(dat=secGeodat,
                                                names=c("hhid","S","E"),
                                                m=ngrn()@ohs_geodata_columns_mapping_lsms(year))

      secFqname    <- paste(dirprefix,'./lsms/nigeria/2012/NGA_2012_GHSP-W2_v02_M_STATA/Post Planting Wave 2/Household/sect9_plantingw2.dta',sep="")
      secFqdat    <- read.dta(secFqname,convert.factors = FALSE)
      
      secFqdat    <- fu()@get_translated_frame(dat=secFqdat,
                                               names=c("hhid","same_diet","less_quality_1","less_quality_2","outoffood"),
                                               m=ngrn()@ohs_food_quality_columns_mapping_lsms(year))
      secFqdat$outoffood <- as.integer(as.integer(secFqdat$outoffood)==1) + as.integer(as.integer(secFqdat$outoffood)==2)*0
      print(paste("Merging OHS data from files for year:",year))
      ohs <- merge(sec1dat,sec2dat,by=c("hhid","personid"))
      ohs <- merge(ohs,sec3dat1,by=c("hhid","personid"), all.x=TRUE)
      ohs <- merge(ohs,secGeodat,by=c("hhid"),all.x=T)
      ohs <- merge(ohs,secFqdat,by=c("hhid"),all.x=T)
      
      #../lsms/nigeria/2012/NGA_2012_GHSP-W2_v02_M_STATA/Geodata Wave 2/NGA_HouseholdGeovars_Y2.dta
      
      ohs$highest_educ <- as.integer(as.character(ohs$highest_educ))
      ohs$age          <- 2012 - as.integer(as.character(ohs$YOB))
      
      #household_status must be determined by 1. rank based on occupation_rank 2. occupation_primary 3. highest_educ 4. qualification 5. age (pay is not available for the most)
      #ohsi <- subset(ohs,is.na(last_payment_primary)) # income units need to be standardised
      
      
      education_rank_mapping <- read.csv(paste0(dirprefix,"./lsms/nigeria/education_codes.csv"),stringsAsFactors = F)
      ohs$highest_educ_temp <- ohs$highest_educ
      ohs[is.na(ohs$highest_educ_temp),]$highest_educ_temp <- 0
      #ohs$education_rank <- as.integer(ohs$highest_educ_temp<=0)*0 + as.integer(ohs$highest_educ_temp>0 & ohs$highest_educ_temp<=11)*1 + as.integer(ohs$highest_educ_temp>11 & ohs$highest_educ_temp<=23)*2 +as.integer(ohs$highest_educ_temp>23)*3
      ignored_educ_types <- c(4,19,20) # Invalid entries in the data
      ohs <- subset(ohs,!is.element(highest_educ_temp,ignored_educ_types))
      if (length(setdiff(unique(ohs$highest_educ_temp),education_rank_mapping$highest_educ))>0 ){
        
        stop("Missing education codes")
      }
      ohs <- merge(ohs,plyr::rename(education_rank_mapping,c("highest_educ"="highest_educ_temp"))[,c("highest_educ_temp","education_rank")],by=c("highest_educ_temp"))
      ohs$highest_educ_temp <- NULL
      
      ohsf <- add_father_educ(ohs,education_rank_mapping)
      ohsfm <- add_mother_educ(ohsf,education_rank_mapping)
      return(ohsfm)
    }
    
    if ( year == 2015) {
      
      sec1fname  <-paste(dirprefix,'./lsms/nigeria/2015/NGA_2015_GHSP-W3_v02_M_Stata/sect1_plantingw3.dta',sep="")
      print(paste("Opening file:",sec1fname))
      sec1dat    <- read_dta(sec1fname)
      sec1dat    <- fu()@get_translated_frame(dat=sec1dat,
                                              names=setdiff(ngrn()@ohs_info_columns_lsms(year),c("state")),
                                              m=subset(ngrn()@ohs_mapping_lsms(year),!is.element(iesname,c("state"))))
      sec2fname    <- paste(dirprefix,'./lsms/nigeria/2015/NGA_2015_GHSP-W3_v02_M_Stata/sect2_harvestw3.dta',sep="")
      print(paste("Opening file:",sec2fname))
      sec2dat    <- read_dta(sec2fname)
      sec2dat    <- fu()@get_translated_frame(dat=sec2dat,
                                              names=ngrn()@ohs_educ_info_columns_lsms(2010),
                                              m=ngrn()@ohs_educ_columns_mapping_lsms(year))
      
      sec3fname1    <- paste(dirprefix,'./lsms/nigeria/2015/NGA_2015_GHSP-W3_v02_M_Stata/sect3_plantingw3.dta',sep="")
      print(paste("Opening file:",sec3fname1))
      sec3dat1    <- read_dta(sec3fname1)
      sec3dat1    <- fu()@get_translated_frame(dat=sec3dat1,
                                               names=ngrn()@ohs_income_info_columns_lsms(2015),
                                               m=ngrn()@ohs_income_columns_mapping_lsms(year))
      
      secGeofname    <- paste(dirprefix,'./lsms/nigeria/2015/NGA_2015_GHSP-W3_v02_M_Stata/NGA_HouseholdGeovars_Y3.dta',sep="")
      secGeodat    <- read.dta(secGeofname,convert.factors = FALSE)
      
      secGeodat    <- fu()@get_translated_frame(dat=secGeodat,
                                                names=c("hhid","S","E"),
                                                m=ngrn()@ohs_geodata_columns_mapping_lsms(year))
      
      secFqname    <- paste(dirprefix,'./lsms/nigeria/2015/NGA_2015_GHSP-W3_v02_M_Stata/sect9b_plantingw3.dta',sep="")
      secFqdat    <- read.dta(secFqname,convert.factors = FALSE)
      
      secFqdat    <- fu()@get_translated_frame(dat=secFqdat,
                                               names=c("hhid","same_diet","less_quality","outoffood"),
                                               m=ngrn()@ohs_food_quality_columns_mapping_lsms(year))
      secFqdat$outoffood <- as.integer(as.integer(secFqdat$outoffood)==1) + as.integer(as.integer(secFqdat$outoffood)==2)*0
      
            
      print(paste("Merging OHS data from files for year:",year))
      ohs <- merge(sec1dat,sec2dat,by=c("hhid","personid"))
      # merge is only with hhid and personid
      ohs <- merge(ohs,sec3dat1[,c("hhid","personid",setdiff(colnames(sec3dat1),colnames(ohs)))],by=c("hhid","personid"),all.x=TRUE)
      ohs <- merge(ohs,secGeodat,by=c("hhid"),all.x=T)
      ohs <- merge(ohs,secFqdat,by=c("hhid"),all.x=T)
      
      ohs$highest_educ <- as.integer(as.character(ohs$highest_educ))
      ohs$age          <- 2015 - as.integer(as.character(ohs$YOB))
      
      #household_status must be determined by 1. rank based on occupation_rank 2. occupation_primary 3. highest_educ 4. qualification 5. age (pay is not available for the most)
      #ohsi <- subset(ohs,is.na(last_payment_primary)) # income units need to be standardised
      ohs$highest_educ_temp <- ohs$highest_educ
      
      education_rank_mapping <- read.csv(paste0(dirprefix,"./lsms/nigeria/education_codes.csv"),stringsAsFactors = F)
      ohs[is.na(ohs$highest_educ_temp),]$highest_educ_temp <- 0
      #ohs$education_rank <- as.integer(ohs$highest_educ_temp<=0)*0 + as.integer(ohs$highest_educ_temp>0 & ohs$highest_educ_temp<=11)*1 + as.integer(ohs$highest_educ_temp>11 & ohs$highest_educ_temp<=23)*2 +as.integer(ohs$highest_educ_temp>23)*3
      if (length(setdiff(unique(ohs$highest_educ_temp),education_rank_mapping$highest_educ))>0 ){
        stop("Missing education codes")
      }
      ohs <- merge(ohs,plyr::rename(education_rank_mapping,c("highest_educ"="highest_educ_temp"))[,c("highest_educ_temp","education_rank")],by=c("highest_educ_temp"))
      ohs$highest_educ_temp <- NULL
      
      ohsf <- add_father_educ(ohs,education_rank_mapping)
      ohsfm <- add_mother_educ(ohsf,education_rank_mapping)
      return(ohsfm)
    }
    stop(paste("Year:",year,"not supported"))
  }
  
  
  
  
  return(new("NigeriaLoader",load_diary_file=load_diary_file, 
             load_ohs_file=load_ohs_file,load_market_prices=load_market_prices,read_assets_file=read_assets_file) )
  
}
