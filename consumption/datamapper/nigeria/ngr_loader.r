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
                                         load_ohs_file="function"
))

ngr_loader<-function(fu,ngrn,lgc) {
  
  load_diary_file <-function(dirprefix,year,fu,ngrn,load_cost){
    ##
    if (year == 2010){
      fname <- paste(dirprefix,"./lsms/nigeria/2010/NGA_2010_GHSP-W1_v03_M_STATA/Post\ Planting\ Wave\ 1/Household/sect7b_plantingw1.dta",sep="")
      sec7dat <- read_dta(fname)
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
      l <- merge(plyr::rename(l,c("item"="code")),ngrn()@item_codes_2010()[,c("shortname","code")],by=c('code'),all.x=TRUE)
      if (dim(subset(l,is.na(shortname)))[1]>0){
        stop("Could not find weekly recall items",toString(subset(l,is.na(shortname))$code))
      }
      
      }
    return(l)
  }
  load_ohs_file <-function(year,dirprefix,fu,ln){
    #
    if (year ==2010){
      
    }
    return(1)
  }
  return(new("NigeriaLoader",load_diary_file=load_diary_file, 
             load_ohs_file=load_ohs_file) )
  
}