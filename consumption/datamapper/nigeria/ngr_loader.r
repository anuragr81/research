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
  
  load_diary_file <-function(dirprefix,year,fu,ln){
    ##
    if (year == 2010){
      fname <- paste(dirprefix,"./lsms/nigeria/2010/NGA_2010_GHSP-W1_v03_M_STATA/Post\ Planting\ Wave\ 1/Household/sect7b_plantingw1.dta",sep="")
      sec7dat <- read_dta(fname)
      k <- fu()@get_translated_frame(dat=sec7dat,
                                     names=ngrn()@diary_info_columns_2010(),
                                     m=ngrn()@diary_columns_mapping(2010))
      #,
      #        convert_factors = FALSE,hhidColName = "y4_hhid")
      }
    return(dat)
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