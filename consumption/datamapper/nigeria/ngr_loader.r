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
                                         load_market_prices = "function"
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
    stop(paste("Cannot load market prices for year:",year))
  }
  load_diary_file <-function(dirprefix,year,fu,ngrn,load_cost){
    ##
    if (year == 2010){
      fname <- paste(dirprefix,"./lsms/nigeria/2010/NGA_2010_GHSP-W1_v03_M_STATA/Post\ Planting\ Wave\ 1/Household/sect7b_plantingw1.dta",sep="")
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
    
    stop (paste("Cannot process data for year:",year))
    
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
      
      print("Mergign OHS data from files")
      ohs <- merge(sec1dat,sec2dat,by=c("hhid","personid"))
      ohs <- merge(ohs,sec3dat,by=c("hhid","personid"), all.x=TRUE)
      
      ohs$highest_educ <- as.integer(as.character(ohs$highest_educ))
      ohs$age          <- 2010 - as.integer(as.character(ohs$YOB))
      print("Testing Data Integrity after merge")
      if (dim(subset(ddply(ohs[,c("hhid","personid","educ_cost")],.(hhid,personid),summarise,n=length(educ_cost)),n>1))[1]>0){
        stop("Multiple entries for a person found")
      }
      
      #household_status must be determined by 1. rank based on occupation_rank 2. occupation_primary 3. highest_educ 4. qualification 5. age (pay is not available for the most)
      #ohsi <- subset(ohs,is.na(last_payment_primary)) # income units need to be standardised
      
      return(ohs)
    }
    stop(paste("Year:",year,"not supported"))
  }
  
  return(new("NigeriaLoader",load_diary_file=load_diary_file, 
             load_ohs_file=load_ohs_file,load_market_prices=load_market_prices) )
  
}